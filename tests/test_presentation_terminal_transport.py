"""Pure conformance tests for the additive presentation-terminal host port."""

from __future__ import annotations

from dataclasses import FrozenInstanceError

import pytest

from presentation_terminal import (
    AdmissionStatus,
    EgressBatch,
    EgressWatermarks,
    FakeTerminalHost,
    GeometryRecord,
    HostPortLimits,
    IngressRecord,
    ResizeRecord,
    TerminalHost,
)


def _limits(
    *,
    high_bytes: int = 16,
    low_bytes: int = 4,
    high_batches: int = 4,
    low_batches: int = 1,
    retained_bytes: int = 8,
    ingress_bytes: int = 8,
    ingress_events: int = 4,
    control_bytes: int = 2,
    control_events: int = 1,
    geometry_events: int = 2,
) -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=high_bytes,
            low_bytes=low_bytes,
            high_batches=high_batches,
            low_batches=low_batches,
        ),
        retained_publication_bytes=retained_bytes,
        ingress_bytes=ingress_bytes,
        ingress_events=ingress_events,
        ingress_control_bytes=control_bytes,
        ingress_control_events=control_events,
        geometry_events=geometry_events,
    )


def test_records_are_immutable_copies_and_limits_are_caller_validated():
    source = bytearray(b"cell")
    batch = EgressBatch(attachment_epoch=1, publication_sequence=0, payload=source)
    source[:] = b"xxxx"

    assert batch.payload == b"cell"
    with pytest.raises(FrozenInstanceError):
        batch.payload = b"other"
    with pytest.raises(ValueError, match="low_bytes"):
        EgressWatermarks(8, 8, 2, 0)
    with pytest.raises(ValueError, match="low_batches"):
        EgressWatermarks(8, 0, 2, 2)
    with pytest.raises(ValueError, match="retained_publication_bytes"):
        _limits(high_bytes=4, low_bytes=0, retained_bytes=5)
    with pytest.raises(ValueError, match="ingress_control_bytes"):
        _limits(ingress_bytes=4, control_bytes=5)
    with pytest.raises(ValueError, match="combined byte storage"):
        HostPortLimits(
            egress=EgressWatermarks((1 << 64) - 1, 0, 2, 0),
            retained_publication_bytes=1,
            ingress_bytes=1,
            ingress_events=2,
            ingress_control_bytes=1,
            ingress_control_events=1,
            geometry_events=1,
        )


def test_fake_requires_explicit_exclusive_attachment():
    host = FakeTerminalHost()

    assert isinstance(host, TerminalHost)
    assert not host.enhanced_attached
    assert host.active_attachment_epoch is None
    assert host.can_start_guest_batch

    lease = host.attach(_limits())
    assert host.enhanced_attached
    assert lease.attachment_epoch == host.active_attachment_epoch
    with pytest.raises(RuntimeError, match="already active"):
        host.attach(_limits())


def test_two_publications_poll_in_order_and_release_exact_capacity():
    host = FakeTerminalHost()
    lease = host.attach(_limits())

    assert host.publish_egress(lease, b"") is AdmissionStatus.ACCEPTED
    first_source = bytearray(b"one")
    assert host.publish_egress(lease, first_source) is AdmissionStatus.ACCEPTED
    assert host.publish_egress(lease, b"two") is AdmissionStatus.ACCEPTED
    first_source[:] = b"bad"

    first = lease.poll_egress()
    second = lease.poll_egress()
    assert first.status is second.status is AdmissionStatus.ACCEPTED
    assert first.delivery is not None
    assert second.delivery is not None
    assert first.delivery.batch == EgressBatch(lease.attachment_epoch, 0, b"one")
    assert second.delivery.batch == EgressBatch(lease.attachment_epoch, 1, b"two")
    assert host.accepted_egress_bytes == 6
    assert host.accepted_egress_batches == 2

    assert first.delivery.release() is AdmissionStatus.ACCEPTED
    assert first.delivery.release() is AdmissionStatus.ACCEPTED
    assert host.accepted_egress_bytes == 3
    assert host.accepted_egress_batches == 1
    assert second.delivery.release() is AdmissionStatus.ACCEPTED
    assert host.accepted_egress_bytes == 0
    assert host.accepted_egress_batches == 0
    assert lease.poll_egress().delivery is None


def test_full_queue_retains_one_exact_publication_until_runner_services_it():
    host = FakeTerminalHost()
    lease = host.attach(
        _limits(
            high_bytes=4,
            low_bytes=0,
            high_batches=2,
            low_batches=0,
            retained_bytes=4,
        )
    )

    assert host.publish_egress(lease, b"ABC") is AdmissionStatus.ACCEPTED
    retained_source = bytearray(b"XY")
    assert (
        host.publish_egress(lease, retained_source)
        is AdmissionStatus.BACKPRESSURED
    )
    retained_source[:] = b"zz"
    assert host.retained_publication == EgressBatch(
        lease.attachment_epoch,
        1,
        b"XY",
    )
    assert host.runner_backpressured
    assert not host.can_start_guest_batch
    assert (
        lease.machine_egress_quiescent()
        is AdmissionStatus.BACKPRESSURED
    )

    # A forbidden extra runner publication neither replaces nor duplicates
    # the one adapter-owned retained record.
    assert host.publish_egress(lease, b"QQ") is AdmissionStatus.BACKPRESSURED
    assert host.retained_publication.payload == b"XY"

    first = lease.poll_egress().delivery
    assert first is not None and first.batch.payload == b"ABC"
    assert host.service_retained(lease) is AdmissionStatus.BACKPRESSURED
    assert first.release() is AdmissionStatus.ACCEPTED
    assert (
        lease.machine_egress_quiescent()
        is AdmissionStatus.BACKPRESSURED
    )

    assert host.service_retained(lease) is AdmissionStatus.ACCEPTED
    assert host.retained_publication is None
    assert host.can_start_guest_batch
    second = lease.poll_egress().delivery
    assert second is not None
    assert (second.batch.publication_sequence, second.batch.payload) == (1, b"XY")
    assert lease.poll_egress().delivery is None
    assert second.release() is AdmissionStatus.ACCEPTED
    assert lease.machine_egress_quiescent() is AdmissionStatus.ACCEPTED


def test_reset_invalidates_old_lease_delivery_and_stale_close_cannot_detach_new():
    host = FakeTerminalHost()
    old = host.attach(_limits())
    assert host.publish_egress(old, b"old") is AdmissionStatus.ACCEPTED
    old_delivery = old.poll_egress().delivery
    assert old_delivery is not None
    assert old.submit_ingress(b"key") is AdmissionStatus.ACCEPTED
    assert old.submit_geometry(80, 24) is AdmissionStatus.ACCEPTED

    reset_epoch = host.reset()
    assert reset_epoch > old.attachment_epoch
    assert old_delivery.release() is AdmissionStatus.STALE
    assert old.poll_egress().status is AdmissionStatus.STALE
    assert old.submit_ingress(b"late") is AdmissionStatus.STALE
    assert old.submit_geometry(100, 30) is AdmissionStatus.STALE
    assert host.publish_egress(old, b"late") is AdmissionStatus.STALE
    assert host.service_retained(old) is AdmissionStatus.STALE
    assert old.close() is AdmissionStatus.STALE

    current = host.attach(_limits())
    assert current.attachment_epoch > reset_epoch
    assert old.close() is AdmissionStatus.STALE
    assert host.active_attachment_epoch == current.attachment_epoch

    assert current.close() is AdmissionStatus.ACCEPTED
    detached_epoch = host.epoch
    assert current.close() is AdmissionStatus.ACCEPTED
    assert host.epoch == detached_epoch
    assert not host.enhanced_attached


def test_ingress_reserves_control_capacity_and_geometry_waits_for_boundary():
    host = FakeTerminalHost()
    lease = host.attach(
        _limits(
            ingress_bytes=6,
            ingress_events=3,
            control_bytes=2,
            control_events=1,
            geometry_events=1,
        )
    )

    assert lease.submit_ingress(b"ABCD") is AdmissionStatus.ACCEPTED
    assert lease.submit_ingress(b"X") is AdmissionStatus.BACKPRESSURED
    assert lease.submit_ingress(b"OK", control=True) is AdmissionStatus.ACCEPTED
    assert lease.submit_ingress(b"!", control=True) is AdmissionStatus.BACKPRESSURED
    assert lease.submit_geometry(120, 40) is AdmissionStatus.ACCEPTED
    assert lease.submit_geometry(121, 41) is AdmissionStatus.BACKPRESSURED

    assert host.pending_ingress_bytes == 6
    assert host.pending_ingress_events == 2
    assert host.pending_geometry_events == 1
    assert [record.schedule_sequence for record in host.pending_ingress] == [0, 1]
    assert host.pending_geometry == (
        GeometryRecord(lease.attachment_epoch, 2, 120, 40),
    )

    first = host.take_scheduled_event(lease)
    second = host.take_scheduled_event(lease)
    third = host.take_scheduled_event(lease)
    assert first.event == IngressRecord(lease.attachment_epoch, 0, b"ABCD", False)
    assert second.event == IngressRecord(lease.attachment_epoch, 1, b"OK", True)
    assert third.event == GeometryRecord(lease.attachment_epoch, 2, 120, 40)
    assert host.take_scheduled_event(lease).event is None
    assert host.pending_ingress_bytes == 0
    assert host.pending_ingress_events == 0
    assert host.pending_geometry_events == 0

    assert lease.submit_geometry(90, 30) is AdmissionStatus.ACCEPTED
    host.reset()
    assert host.pending_ingress == ()
    assert host.pending_geometry == ()
    assert host.take_scheduled_event(lease).status is AdmissionStatus.STALE


def test_resize_admission_reserves_ingress_and_geometry_atomically():
    host = FakeTerminalHost()
    lease = host.attach(
        _limits(
            ingress_bytes=4,
            ingress_events=2,
            control_bytes=1,
            control_events=1,
            geometry_events=1,
        )
    )

    assert lease.submit_ingress(b"ABC") is AdmissionStatus.ACCEPTED
    assert lease.resize_admission_ready(1) is AdmissionStatus.BACKPRESSURED
    assert (
        lease.submit_resize(b"R", cols=100, rows=40)
        is AdmissionStatus.BACKPRESSURED
    )
    assert (host.pending_ingress_bytes, host.pending_ingress_events) == (3, 1)
    assert host.pending_geometry_events == 0
    first = host.take_scheduled_event(lease)
    assert first.event == IngressRecord(lease.attachment_epoch, 0, b"ABC", False)

    assert lease.submit_geometry(90, 30) is AdmissionStatus.ACCEPTED
    assert lease.resize_admission_ready(1) is AdmissionStatus.BACKPRESSURED
    assert (
        lease.submit_resize(b"R", cols=100, rows=40)
        is AdmissionStatus.BACKPRESSURED
    )
    assert (host.pending_ingress_bytes, host.pending_ingress_events) == (0, 0)
    assert host.pending_geometry_events == 1
    second = host.take_scheduled_event(lease)
    assert second.event == GeometryRecord(lease.attachment_epoch, 1, 90, 30)
    assert lease.resize_admission_ready(1) is AdmissionStatus.ACCEPTED

    source = bytearray(b"R")
    assert (
        lease.submit_resize(source, cols=100, rows=40)
        is AdmissionStatus.ACCEPTED
    )
    source[:] = b"X"
    expected = ResizeRecord(lease.attachment_epoch, 2, b"R", 100, 40)
    assert host.pending_ingress == (expected,)
    assert host.pending_geometry == (expected,)
    assert (host.pending_ingress_bytes, host.pending_ingress_events) == (1, 1)
    assert host.pending_geometry_events == 1
    assert host.take_scheduled_event(lease).event == expected
    assert (host.pending_ingress_bytes, host.pending_ingress_events) == (0, 0)
    assert host.pending_geometry_events == 0

    assert lease.close() is AdmissionStatus.ACCEPTED
    assert lease.resize_admission_ready(1) is AdmissionStatus.STALE
    assert (
        lease.submit_resize(b"late", cols=80, rows=24)
        is AdmissionStatus.STALE
    )
