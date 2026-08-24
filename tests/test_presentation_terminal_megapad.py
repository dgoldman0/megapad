"""Focused runtime tests for the opt-in MegaPad terminal host port."""

from __future__ import annotations

from presentation_terminal import (
    AdmissionStatus,
    EgressWatermarks,
    HostPortLimits,
    TerminalHost,
)
from system import MegapadSystem


def _limits(
    *,
    high_bytes: int = 16,
    low_bytes: int = 4,
    high_batches: int = 4,
    low_batches: int = 1,
    retained_bytes: int = 8,
) -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=high_bytes,
            low_bytes=low_bytes,
            high_batches=high_batches,
            low_batches=low_batches,
        ),
        retained_publication_bytes=retained_bytes,
        ingress_bytes=8,
        ingress_events=4,
        ingress_control_bytes=2,
        ingress_control_events=1,
        geometry_events=2,
    )


def _write_native_uart(system: MegapadSystem, payload: bytes) -> bytes:
    for value in payload:
        system.cpu._cs.uart_write8(0x00, value)
    return system._drain_native_uart_output()


def test_runtime_attachment_suspends_and_restores_legacy_uart_consumers():
    system = MegapadSystem(ram_size=64 * 1024)
    legacy_batches: list[bytes] = []
    system.uart.on_tx = None
    system.uart.on_tx_batch = legacy_batches.append

    assert isinstance(system.presentation_terminal_host, TerminalHost)
    assert not system.presentation_terminal_host.enhanced_attached
    assert _write_native_uart(system, b"A") == b"A"
    assert legacy_batches == [b"A"]
    assert bytes(system.uart.tx_buffer) == b"A"

    lease = system.attach_presentation_terminal(_limits())
    assert _write_native_uart(system, b"B") == b"B"
    assert legacy_batches == [b"A"]
    assert bytes(system.uart.tx_buffer) == b"A"

    delivery = lease.poll_egress().delivery
    assert delivery is not None
    assert delivery.batch.payload == b"B"
    assert delivery.release() is AdmissionStatus.ACCEPTED
    assert lease.close() is AdmissionStatus.ACCEPTED

    assert _write_native_uart(system, b"C") == b"C"
    assert legacy_batches == [b"A", b"C"]
    assert bytes(system.uart.tx_buffer) == b"AC"


def test_runtime_retains_one_exact_batch_and_stops_before_more_guest_work():
    system = MegapadSystem(ram_size=64 * 1024)
    lease = system.attach_presentation_terminal(
        _limits(
            high_bytes=4,
            low_bytes=0,
            high_batches=2,
            low_batches=0,
            retained_bytes=4,
        )
    )

    assert _write_native_uart(system, b"ABC") == b"ABC"
    assert _write_native_uart(system, b"XY") == b"XY"
    retained = system.presentation_terminal_host.retained_publication
    assert retained is not None
    assert retained.payload == b"XY"

    pc_before = system.cpu.pc
    blocked = system.run_batch_stats(1)
    assert blocked.instructions_executed == 0
    assert blocked.system_cycles_advanced == 0
    assert blocked.system_stop_reason == "host_backpressure"
    assert system.cpu.pc == pc_before

    first = lease.poll_egress().delivery
    assert first is not None and first.batch.payload == b"ABC"
    still_blocked = system.run_batch_stats(1)
    assert still_blocked.system_stop_reason == "host_backpressure"
    assert first.release() is AdmissionStatus.ACCEPTED

    resumed = system.run_batch_stats(1)
    assert resumed.system_stop_reason != "host_backpressure"
    second = lease.poll_egress().delivery
    assert second is not None
    assert (second.batch.publication_sequence, second.batch.payload) == (1, b"XY")
    assert second.release() is AdmissionStatus.ACCEPTED


def test_runtime_applies_ingress_and_geometry_only_at_runner_boundary():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=80,
        terminal_rows=24,
    )
    system.schedule_uart_input(b"L")
    lease = system.attach_presentation_terminal(_limits())

    # The legacy keyboard/DSR and display-resize facades are paused without
    # replacing their callbacks; the lease is the only active host route.
    system.uart.inject_input(b"ignored")
    system.uart_geom.host_set_size(90, 30)
    assert system.uart.rx_pending == 1
    assert (system.uart_geom.cols, system.uart_geom.rows) == (80, 24)

    assert lease.submit_ingress(b"Q") is AdmissionStatus.ACCEPTED
    assert lease.submit_geometry(100, 40) is AdmissionStatus.ACCEPTED
    assert system.uart.rx_pending == 1
    assert (system.uart_geom.cols, system.uart_geom.rows) == (80, 24)

    system.cpu.halted = True
    result = system.run_batch_stats(1)
    assert result.external_events_applied == 2
    assert system.uart.rx_pending == 2
    assert (system.uart_geom.cols, system.uart_geom.rows) == (100, 40)
    assert system.presentation_terminal_host.pending_ingress_events == 0
    assert system.presentation_terminal_host.pending_geometry_events == 0

    # Older ANSI input remains at the front.  Detach then removes only the
    # unconsumed byte supplied by this attachment.
    assert system.cpu._cs.uart_read8(0x01) == ord("L")
    assert lease.close() is AdmissionStatus.ACCEPTED
    assert system.uart.rx_pending == 0
    assert lease.submit_ingress(b"stale") is AdmissionStatus.STALE

    system.uart.inject_input(b"Z")
    system.uart_geom.host_set_size(90, 30)
    assert system.uart.rx_pending == 1
    assert (system.uart_geom.cols, system.uart_geom.rows) == (90, 30)


def test_subsequent_boot_retires_the_active_epoch_before_execution_resumes():
    system = MegapadSystem(ram_size=64 * 1024)
    lease = system.attach_presentation_terminal(_limits())
    system.boot()
    assert system.presentation_terminal_host.enhanced_attached

    assert _write_native_uart(system, b"view") == b"view"
    old_delivery = lease.poll_egress().delivery
    assert old_delivery is not None
    assert lease.submit_ingress(b"old") is AdmissionStatus.ACCEPTED
    system.cpu.halted = True
    system.run_batch_stats(1)
    assert system.uart.rx_pending == 3

    system.boot()
    assert not system.presentation_terminal_host.enhanced_attached
    assert system.uart.rx_pending == 0
    assert old_delivery.release() is AdmissionStatus.STALE
    assert lease.poll_egress().status is AdmissionStatus.STALE
    assert lease.submit_ingress(b"late") is AdmissionStatus.STALE

    current = system.attach_presentation_terminal(_limits())
    assert current.attachment_epoch > lease.attachment_epoch
    assert lease.close() is AdmissionStatus.STALE
    assert (
        system.presentation_terminal_host.active_attachment_epoch
        == current.attachment_epoch
    )


def test_oversized_invalid_publication_latches_without_leaking_or_throwing():
    system = MegapadSystem(ram_size=64 * 1024)
    legacy_batches: list[bytes] = []
    system.uart.on_tx = None
    system.uart.on_tx_batch = legacy_batches.append
    lease = system.attach_presentation_terminal(_limits(retained_bytes=8))
    assert lease.submit_ingress(b"Q") is AdmissionStatus.ACCEPTED

    # A misconfigured or nonconforming guest can exceed the caller's declared
    # one-publication slot.  The scheduler boundary must stay nonthrowing and
    # must never route those binary bytes into the suspended ANSI consumer.
    assert _write_native_uart(system, b"123456789") == b"123456789"
    assert legacy_batches == []
    assert lease.poll_egress().delivery is None
    assert "9 bytes" in system.presentation_terminal_host.failure_reason

    stopped = system.run_batch_stats(1)
    assert stopped.instructions_executed == 0
    assert stopped.system_cycles_advanced == 0
    assert stopped.system_stop_reason == "terminal_failure"
    assert system.uart.rx_pending == 0
    assert system.presentation_terminal_host.pending_ingress_events == 1

    assert lease.close() is AdmissionStatus.ACCEPTED
    assert _write_native_uart(system, b"A") == b"A"
    assert legacy_batches == [b"A"]
