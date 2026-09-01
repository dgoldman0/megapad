"""Focused host/core integration tests for the opt-in terminal driver."""

from __future__ import annotations

import hashlib
import struct

import pytest

from rich_terminal import (
    AdmissionStatus,
    EgressWatermarks,
    FakeTerminalHost,
    HostPortLimits,
)
from rich_terminal.apt1 import (
    FrameEncoder,
    IncrementalFrameDecoder,
    MessageType,
    Offer,
    OpenRequest,
    encode_open,
    encode_probe,
    parse_negotiation,
)
from rich_terminal.driver import (
    DriverLimits,
    DriverStatus,
    RichTerminalDriver,
)
from rich_terminal.retained_model import (
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_wire import (
    OwnerDrop,
    OwnerOpen,
    ResourceBegin,
    ResourceChunk,
    RetStatus,
    RetainedMessageType,
    decode_ret_caps,
    decode_ret_formats,
    decode_ret_result,
    encode_owner_drop,
    encode_owner_open,
    encode_resource_begin,
    encode_resource_chunk,
    encode_ret_query,
)
from rich_terminal.server import (
    RichTerminalCore,
    TerminalConfig,
    TerminalState,
)
from system import MegapadSystem


READY = struct.Struct("<IIIIIIQ")
BEGIN = struct.Struct("<QQIIII")
SPAN = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
COMMIT = struct.Struct("<Q")
TX_RESULT = struct.Struct("<QHHQ")
CREDIT = struct.Struct("<Q")
TEXT_PREFIX = struct.Struct("<HHQ")
POINTER = struct.Struct("<iiHHHHhhQ")
RESIZE = struct.Struct("<IIQ")
FOCUS = struct.Struct("<B7sQ")


def _terminal_config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=256,
        max_transaction_bytes=512,
        terminal_receive_credit=1_024,
        max_cells=4,
        max_feed_bytes=4_608,
        max_cols=4,
        max_rows=2,
        cols=2,
        rows=2,
    )


def _retained_policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE,
        max_owner_records=4,
        max_live_owners=2,
        max_regions=8,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=4,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=512,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=512,
    )


def _image_policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE | RetainedFeature.RGBA_IMAGE,
        max_owner_records=2,
        max_live_owners=1,
        max_regions=2,
        max_resources=2,
        max_objects=2,
        max_series=0,
        max_operations_per_transaction=4,
        max_resource_chunk_bytes=16,
        max_retained_transaction_bytes=512,
        total_resource_bytes=16,
        image_format=1,
        max_image_width=2,
        max_image_height=2,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=512,
    )


def _host_limits(
    *,
    control_events: int = 8,
    ingress_events: int = 16,
) -> HostPortLimits:
    return HostPortLimits(
        egress=EgressWatermarks(
            high_bytes=8_192,
            low_bytes=1_024,
            high_batches=16,
            low_batches=2,
        ),
        retained_publication_bytes=4_608,
        ingress_bytes=8_192,
        ingress_events=ingress_events,
        ingress_control_bytes=4_096,
        ingress_control_events=control_events,
        geometry_events=2,
    )


def _write_native_uart(system: MegapadSystem, payload: bytes) -> None:
    for value in payload:
        system.cpu._cs.uart_write8(0x00, value)
    assert system._drain_native_uart_output() == payload


def _drain_uart_rx(system: MegapadSystem) -> bytes:
    result = bytearray()
    while system.uart.has_rx_data:
        result.append(system.cpu._cs.uart_read8(0x01))
    return bytes(result)


def _snapshot(
    encoder: FrameEncoder,
    *,
    transaction_id: int = 1,
    cols: int = 2,
    rows: int = 2,
) -> bytes:
    cells = (
        (ord("A"), 7, 0, 1),
        (ord("B"), 2, 0, 8),
        (ord("C"), 4, 0, 0),
        (ord(" "), 7, 1, 0x20),
    )
    assert cols * rows == len(cells)
    spans = tuple(
        SPAN.pack(row, 0, cols)
        + b"".join(
            CELL.pack(*cell)
            for cell in cells[row * cols : (row + 1) * cols]
        )
        for row in range(rows)
    )
    return b"".join(
        (
            encoder.encode(
                MessageType.SNAPSHOT_BEGIN,
                BEGIN.pack(transaction_id, 0, cols, rows, rows, len(cells)),
            ),
            *(encoder.encode(MessageType.CELL_SPAN, span) for span in spans),
            encoder.encode(
                MessageType.CURSOR,
                CURSOR.pack(rows - 1, cols - 1, 1),
            ),
            encoder.encode(
                MessageType.SNAPSHOT_COMMIT,
                COMMIT.pack(transaction_id),
            ),
        )
    )


def _open_bytes(
    offer: Offer,
    *,
    client_credit: int = 256,
) -> tuple[bytes, FrameEncoder]:
    request = OpenRequest(
        nonce=offer.nonce,
        session_id=offer.session_id,
        client_max_payload=256,
        client_receive_credit=client_credit,
    )
    encoder = FrameEncoder(offer.session_id, max_payload=offer.max_payload)
    ready = encoder.encode(
        MessageType.CLIENT_READY,
        READY.pack(0, 256, 0, client_credit, 64, 0, 0x3F),
    )
    return encode_open(request) + ready + _snapshot(encoder), encoder


def test_driver_keeps_ansi_default_then_runs_a_real_cell_snapshot():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    legacy_batches: list[bytes] = []
    ansi_batches: list[bytes] = []
    views = []
    system.uart.on_tx = None
    system.uart.on_tx_batch = legacy_batches.append
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 3),
        ansi_sink=ansi_batches.append,
        view_sink=views.append,
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )

    _write_native_uart(system, b"ANSI")
    ansi_result = driver.service()
    assert ansi_result.status is DriverStatus.PROGRESS
    assert ansi_result.ansi_bytes == 4
    assert ansi_batches == [b"ANSI"]
    assert legacy_batches == []

    nonce = 0xFEDCBA9876543210
    _write_native_uart(system, encode_probe(nonce))
    offered = driver.service()
    assert offered.outbound_records == 1
    assert system.uart.rx_pending == 0

    system.cpu.halted = True
    boundary = system.run_batch_stats(1)
    assert boundary.external_events_applied == 2  # initial geometry + offer
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)

    open_and_snapshot, client_encoder = _open_bytes(offer, client_credit=512)
    _write_native_uart(system, open_and_snapshot)
    presented = driver.service()
    assert presented.status is DriverStatus.PROGRESS
    assert presented.views == 1
    assert len(views) == 1 and views[0].revision == 1
    assert presented.outbound_records == 3

    system.run_batch_stats(1)
    replies = _drain_uart_rx(system)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    frames = decoder.feed(replies)
    assert [frame.message_type for frame in frames] == [
        MessageType.SERVER_READY,
        MessageType.TX_RESULT,
        MessageType.CREDIT,
    ]

    assert driver.send_key(ord("x"), modifiers=1) is DriverStatus.PROGRESS
    assert driver.send_legacy_input(b"not-framed") is DriverStatus.INVALID
    sent = driver.service()
    assert sent.outbound_records == 1
    system.run_batch_stats(1)
    key = decoder.feed(_drain_uart_rx(system))
    assert len(key) == 1 and key[0].message_type == MessageType.KEY

    assert driver.max_text_bytes == 64
    assert driver.send_text(b"hi", paste=True) is DriverStatus.PROGRESS
    assert (
        driver.send_pointer(1, 0, buttons=1, modifiers=2, kind=2)
        is DriverStatus.PROGRESS
    )
    assert driver.send_focus(True) is DriverStatus.PROGRESS

    # The bounded retention queue is full.  Rejection happens before the
    # core advances sequence/credit or changes its pointer-button history.
    assert (
        driver.send_pointer(1, 0, buttons=0, kind=3)
        is DriverStatus.BACKPRESSURED
    )
    assert driver.service().outbound_records == 3
    assert driver.send_pointer(1, 0, buttons=0, kind=3) is DriverStatus.PROGRESS
    assert driver.service().outbound_records == 1
    system.run_batch_stats(1)

    normalized = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in normalized] == [
        MessageType.TEXT,
        MessageType.POINTER,
        MessageType.FOCUS,
        MessageType.POINTER,
    ]
    assert TEXT_PREFIX.unpack(normalized[0].payload[:12]) == (1, 0, 1)
    assert normalized[0].payload[12:] == b"hi"
    assert POINTER.unpack(normalized[1].payload) == (
        1,
        0,
        1,
        1,
        2,
        2,
        0,
        0,
        1,
    )
    assert FOCUS.unpack(normalized[2].payload) == (1, bytes(7), 1)
    assert POINTER.unpack(normalized[3].payload) == (
        1,
        0,
        0,
        1,
        0,
        3,
        0,
        0,
        1,
    )

    # Latest-wins resize can return to the already-selected geometry without
    # forcing a redundant replacement snapshot.
    assert driver.request_resize(4, 1) is DriverStatus.PROGRESS
    assert driver.pending_resize == (4, 1)
    assert driver.request_resize(2, 2) is DriverStatus.PROGRESS
    assert driver.pending_resize is None
    assert driver.core.geometry_generation == 0

    # A resize request cannot cross an unpolled transaction begin.  Service
    # first completes that transaction, then materializes one composite wire
    # and MMIO geometry record only after machine egress is observed empty.
    _write_native_uart(
        system,
        client_encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 2, 2, 0, 0),
        ),
    )
    assert driver.request_resize(4, 1) is DriverStatus.PROGRESS
    assert driver.pending_resize == (4, 1)
    assert driver.service().machine_batches == 1
    assert driver.pending_resize == (4, 1)
    assert (system.uart_geom.cols, system.uart_geom.rows) == (2, 2)

    _write_native_uart(
        system,
        client_encoder.encode(MessageType.CURSOR, CURSOR.pack(1, 1, 1))
        + client_encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2)),
    )
    completed = driver.service()
    assert completed.views == 1
    assert driver.pending_resize == (4, 1)
    materialized = driver.service()
    assert materialized.outbound_records == 1
    assert driver.pending_resize is None
    assert (system.uart_geom.cols, system.uart_geom.rows) == (2, 2)

    resized_boundary = system.run_batch_stats(1)
    assert resized_boundary.external_events_applied == 3
    resized_frames = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in resized_frames] == [
        MessageType.TX_RESULT,
        MessageType.CREDIT,
        MessageType.RESIZE,
    ]
    assert RESIZE.unpack(resized_frames[2].payload) == (4, 1, 1)
    assert (system.uart_geom.cols, system.uart_geom.rows) == (4, 1)

    _write_native_uart(
        system,
        _snapshot(client_encoder, transaction_id=3, cols=4, rows=1),
    )
    replaced = driver.service()
    assert replaced.views == 1
    assert (views[-1].cols, views[-1].rows, views[-1].revision) == (4, 1, 1)
    system.run_batch_stats(1)
    replacement_results = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in replacement_results] == [
        MessageType.TX_RESULT,
        MessageType.CREDIT,
    ]

    assert driver.close().value == "accepted"
    _write_native_uart(system, b"legacy")
    assert legacy_batches == [b"legacy"]


def test_driver_admits_retained_discovery_pair_then_covering_credit_in_order():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 3),
        retained_policy=_retained_policy(),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    _write_native_uart(system, encode_probe(1))
    assert driver.service().outbound_records == 1
    system.cpu.halted = True
    system.run_batch_stats(1)
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)

    open_and_snapshot, encoder = _open_bytes(offer, client_credit=512)
    _write_native_uart(system, open_and_snapshot)
    opened = driver.service()
    assert opened.views == 1
    assert opened.outbound_records == 3
    system.run_batch_stats(1)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    initial_frames = decoder.feed(_drain_uart_rx(system))
    assert initial_frames[-1].message_type == MessageType.CREDIT

    _write_native_uart(
        system,
        encoder.encode(
            RetainedMessageType.RET_QUERY,
            encode_ret_query(),
        ),
    )
    discovered = driver.service()
    assert discovered.status is DriverStatus.PROGRESS
    assert discovered.outbound_records == 3
    assert driver.pending_outbound_events == 0
    assert driver.core.retained_enabled

    boundary = system.run_batch_stats(1)
    assert boundary.external_events_applied == 3
    frames = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in frames] == [
        RetainedMessageType.RET_CAPS,
        RetainedMessageType.RET_FORMATS,
        MessageType.CREDIT,
    ]
    assert decode_ret_caps(frames[0].payload).max_regions == 8
    assert decode_ret_formats(frames[1].payload).bounds_format == 2
    assert CREDIT.unpack(frames[2].payload) == (1_024 + 312 + 48,)

    assert driver.request_resize(2, 2) is DriverStatus.PROGRESS
    assert driver.pending_resize is None
    assert driver.request_resize(4, 1) is DriverStatus.PROGRESS
    assert driver.pending_resize == (4, 1)
    materialized = driver.service()
    assert materialized.outbound_records == 1
    assert driver.pending_resize is None
    assert driver.core.selected_geometry == (4, 1)
    assert driver.core.geometry_generation == 1
    assert driver.core.state is TerminalState.RESYNCING
    assert (system.uart_geom.cols, system.uart_geom.rows) == (2, 2)

    assert system.run_batch_stats(1).external_events_applied == 1
    resize = decoder.feed(_drain_uart_rx(system))[0]
    assert resize.message_type == MessageType.RESIZE
    assert RESIZE.unpack(resize.payload) == (4, 1, 1)
    assert (system.uart_geom.cols, system.uart_geom.rows) == (4, 1)
    driver.close()


def test_driver_rejects_retained_discovery_capacity_before_attachment():
    system = MegapadSystem(ram_size=64 * 1024)
    host_limits = HostPortLimits(
        egress=EgressWatermarks(8_192, 1_024, 16, 2),
        retained_publication_bytes=4_608,
        ingress_bytes=4_196,
        ingress_events=9,
        ingress_control_bytes=4_096,
        ingress_control_events=8,
        geometry_events=1,
    )

    with pytest.raises(ValueError, match="discovery reply"):
        RichTerminalDriver.attach(
            system,
            host_limits,
            _terminal_config(),
            DriverLimits(4_096, 3),
            retained_policy=_retained_policy(),
        )
    assert not system.rich_terminal_host.enhanced_attached


def test_driver_settles_owner_lifecycle_markers_after_ordered_admission():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 3),
        retained_policy=_retained_policy(),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    _write_native_uart(system, encode_probe(1))
    driver.service()
    system.cpu.halted = True
    system.run_batch_stats(1)
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)
    opened_bytes, encoder = _open_bytes(offer, client_credit=512)
    _write_native_uart(system, opened_bytes)
    driver.service()
    system.run_batch_stats(1)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    decoder.feed(_drain_uart_rx(system))

    _write_native_uart(
        system,
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query()),
    )
    driver.service()
    system.run_batch_stats(1)
    decoder.feed(_drain_uart_rx(system))

    owner = OwnerOpen(7, 1, OwnerQuotas(4, 0, 0, 0, 0, 0, 0))
    _write_native_uart(
        system,
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner)),
    )
    owner_result = driver.service()
    assert owner_result.outbound_records == 2
    assert driver.core.outstanding_lifecycle_result is None
    assert driver.core.owner_state is not None
    assert driver.core.owner_state.reservations.regions == 4
    system.run_batch_stats(1)
    owner_frames = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in owner_frames] == [
        RetainedMessageType.RET_RESULT,
        MessageType.CREDIT,
    ]
    assert decode_ret_result(owner_frames[0].payload).status is RetStatus.OK

    _write_native_uart(
        system,
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(2, 1, 7, 1)),
        ),
    )
    drop_result = driver.service()
    assert drop_result.outbound_records == 1
    assert driver.core.outstanding_result_transaction_id is None
    assert driver.core.model_revision == 2
    assert driver.core.owner_state is not None
    assert not driver.core.owner_state.records[7].live
    system.run_batch_stats(1)
    drop_frame = decoder.feed(_drain_uart_rx(system))[0]
    assert TX_RESULT.unpack(drop_frame.payload) == (2, 0, 0, 2)
    driver.close()


def test_driver_settles_resource_chunk_only_when_covering_credit_is_admitted():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 16),
        retained_policy=_image_policy(),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    _write_native_uart(system, encode_probe(1))
    driver.service()
    system.cpu.halted = True
    system.run_batch_stats(1)
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)
    opened_bytes, encoder = _open_bytes(offer, client_credit=512)
    _write_native_uart(system, opened_bytes)
    driver.service()
    system.run_batch_stats(1)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    decoder.feed(_drain_uart_rx(system))

    _write_native_uart(
        system,
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query()),
    )
    driver.service()
    system.run_batch_stats(1)
    decoder.feed(_drain_uart_rx(system))
    owner = OwnerOpen(7, 1, OwnerQuotas(1, 1, 1, 0, 4, 0, 0))
    _write_native_uart(
        system,
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner)),
    )
    driver.service()
    system.run_batch_stats(1)
    decoder.feed(_drain_uart_rx(system))

    pixels = bytes((1, 2, 3, 255))
    _write_native_uart(
        system,
        encoder.encode(
            RetainedMessageType.RESOURCE_BEGIN,
            encode_resource_begin(
                ResourceBegin(
                    7,
                    1,
                    1,
                    1,
                    1,
                    1,
                    0,
                    len(pixels),
                    hashlib.sha3_256(pixels).digest(),
                )
            ),
        ),
    )
    begin_service = driver.service()
    assert begin_service.outbound_records == 2
    assert driver.core.outstanding_lifecycle_result is None
    system.run_batch_stats(1)
    begin_frames = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in begin_frames] == [
        RetainedMessageType.RET_RESULT,
        MessageType.CREDIT,
    ]
    assert decode_ret_result(begin_frames[0].payload).status is RetStatus.OK

    # Fill the host's bounded control ingress with unrelated covering credits.
    # The driver has no retained output at this point, so the next chunk can be
    # processed while its own CREDIT remains unable to cross host admission.
    for _ in range(16):
        _write_native_uart(system, encoder.encode(0x8003))
        assert driver.service().outbound_records == 1

    _write_native_uart(
        system,
        encoder.encode(
            RetainedMessageType.RESOURCE_CHUNK,
            encode_resource_chunk(ResourceChunk(7, 1, 1, 0, pixels)),
        ),
    )
    blocked = driver.service()
    assert blocked.status is DriverStatus.BACKPRESSURED
    assert blocked.outbound_records == 0
    assert driver.pending_outbound_events == 1
    assert driver.core.outstanding_lifecycle_result is not None
    assert driver.core.resource_upload is not None
    assert driver.core.resource_upload.accepted_bytes == len(pixels)

    system.run_batch_stats(1)
    unrelated = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in unrelated] == [MessageType.CREDIT] * 16
    admitted = driver.service()
    assert admitted.status is DriverStatus.PROGRESS
    assert admitted.outbound_records == 1
    assert driver.pending_outbound_events == 0
    assert driver.core.outstanding_lifecycle_result is None
    system.run_batch_stats(1)
    chunk_credit = decoder.feed(_drain_uart_rx(system))
    assert [frame.message_type for frame in chunk_credit] == [MessageType.CREDIT]
    driver.close()


def test_driver_routes_bounded_preswitch_input_through_the_lease():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 3),
    )

    assert driver.max_legacy_input_bytes == 4_096
    assert driver.send_legacy_input(b"boot\r") is DriverStatus.PROGRESS
    assert system.uart.rx_pending == 0
    assert driver.service().outbound_records == 1

    system.cpu.halted = True
    boundary = system.run_batch_stats(1)
    assert boundary.external_events_applied == 2  # geometry, then raw input
    assert _drain_uart_rx(system) == b"boot\r"

    assert driver.request_resize(4, 1) is DriverStatus.PROGRESS
    assert driver.core.selected_geometry == (4, 1)
    assert system.run_batch_stats(1).external_events_applied == 1
    assert (system.uart_geom.cols, system.uart_geom.rows) == (4, 1)

    _write_native_uart(system, encode_probe(7))
    assert driver.service().outbound_records == 1
    assert driver.request_resize(2, 2) is DriverStatus.BACKPRESSURED
    assert system.run_batch_stats(1).external_events_applied == 1
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)
    assert (offer.cols, offer.rows) == (4, 1)
    assert driver.send_legacy_input(b"") is DriverStatus.INVALID
    assert driver.send_legacy_input("text") is DriverStatus.INVALID
    driver.close()


def test_driver_retries_lease_retirement_after_close_raises(monkeypatch):
    host = FakeTerminalHost()
    limits = _host_limits()
    lease = host.attach(limits)
    driver = RichTerminalDriver(
        lease,
        RichTerminalCore(
            _terminal_config(),
            attachment_epoch=lease.attachment_epoch,
        ),
        limits,
        DriverLimits(4_096, 8),
    )
    original_close = host._lease_close
    calls = 0

    def flaky_close(token, epoch):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise RuntimeError("injected retirement failure")
        return original_close(token, epoch)

    monkeypatch.setattr(host, "_lease_close", flaky_close)
    with pytest.raises(RuntimeError, match="injected retirement failure"):
        driver.close()

    assert not driver.closed
    assert host.active_attachment_epoch == lease.attachment_epoch
    assert driver.close() is AdmissionStatus.ACCEPTED
    assert driver.closed
    assert host.active_attachment_epoch is None
    assert calls == 2


def test_driver_rejects_incoherent_capacity_before_acquiring_the_lease():
    system = MegapadSystem(ram_size=64 * 1024)
    too_small = HostPortLimits(
        egress=EgressWatermarks(4_096, 1_024, 8, 2),
        retained_publication_bytes=4_096,
        ingress_bytes=8_192,
        ingress_events=16,
        ingress_control_bytes=4_096,
        ingress_control_events=8,
        geometry_events=1,
    )
    with pytest.raises(ValueError, match="maximum transaction"):
        RichTerminalDriver.attach(
            system,
            too_small,
            _terminal_config(),
            DriverLimits(4_096, 8),
        )
    assert not system.rich_terminal_host.enhanced_attached


def test_driver_retains_ordered_control_replies_across_host_backpressure():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = RichTerminalDriver.attach(
        system,
        _host_limits(control_events=1, ingress_events=2),
        _terminal_config(),
        DriverLimits(4_096, 8),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    _write_native_uart(system, encode_probe(1))
    assert driver.service().outbound_records == 1
    system.cpu.halted = True
    system.run_batch_stats(1)
    offer = parse_negotiation(_drain_uart_rx(system))
    assert isinstance(offer, Offer)

    # Occupy the one ordinary slot before OPEN. SERVER_READY then fills the
    # reserved control slot, leaving the following TX_RESULT retained by the
    # driver until the exact host-admission boundary.
    assert driver.send_legacy_input(b"queued-before-open") is DriverStatus.PROGRESS
    open_and_snapshot, _encoder = _open_bytes(offer)
    _write_native_uart(system, open_and_snapshot)
    first = driver.service()
    assert first.status is DriverStatus.BACKPRESSURED
    assert first.outbound_records == 2
    assert driver.pending_outbound_events == 2
    assert driver.core.outstanding_result_transaction_id == 1

    system.run_batch_stats(1)
    second = driver.service()
    assert second.status is DriverStatus.PROGRESS
    assert second.outbound_records == 2
    assert driver.pending_outbound_events == 0
    assert driver.core.outstanding_result_transaction_id is None
    driver.close()


def test_resize_intent_waits_for_adapter_retained_machine_egress():
    host_limits = HostPortLimits(
        egress=EgressWatermarks(60, 0, 4, 0),
        retained_publication_bytes=60,
        ingress_bytes=4_096,
        ingress_events=9,
        ingress_control_bytes=2_048,
        ingress_control_events=8,
        geometry_events=1,
    )
    host = FakeTerminalHost()
    lease = host.attach(host_limits)
    core = RichTerminalCore(
        _terminal_config(),
        attachment_epoch=lease.attachment_epoch,
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    probe_result = core.feed_machine(encode_probe(1))
    offer = parse_negotiation(probe_result.outbound[0].payload)
    assert isinstance(offer, Offer)
    open_and_snapshot, encoder = _open_bytes(offer, client_credit=512)
    opened = core.feed_machine(open_and_snapshot)
    for outbound in opened.outbound:
        if outbound.result_transaction_id is not None:
            core.settle_result_delivery(outbound.result_transaction_id)
    driver = RichTerminalDriver(
        lease,
        core,
        host_limits,
        DriverLimits(4_096, 8),
    )

    credit = encoder.encode(MessageType.CREDIT, CREDIT.pack(1_024))
    begin = encoder.encode(
        MessageType.TX_BEGIN,
        BEGIN.pack(2, 1, 2, 2, 0, 0),
    )
    assert host.publish_egress(lease, credit) is AdmissionStatus.ACCEPTED
    assert (
        host.publish_egress(lease, begin[:20])
        is AdmissionStatus.BACKPRESSURED
    )
    assert driver.request_resize(4, 1) is DriverStatus.PROGRESS

    assert driver.service().machine_batches == 1
    deferred = driver.service()
    assert deferred.status is DriverStatus.IDLE
    assert driver.pending_resize == (4, 1)
    assert core.geometry_generation == 0

    assert host.service_retained(lease) is AdmissionStatus.ACCEPTED
    assert driver.service().machine_batches == 1
    assert driver.service().status is DriverStatus.IDLE
    assert driver.pending_resize == (4, 1)
    assert core.geometry_generation == 0

    assert host.publish_egress(lease, begin[20:]) is AdmissionStatus.ACCEPTED
    assert driver.service().machine_batches == 1
    assert driver.pending_resize == (4, 1)
    assert core.geometry_generation == 0

    cursor = encoder.encode(MessageType.CURSOR, CURSOR.pack(1, 1, 1))
    commit = encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2))
    assert host.publish_egress(lease, cursor) is AdmissionStatus.ACCEPTED
    assert driver.service().machine_batches == 1
    assert host.publish_egress(lease, commit) is AdmissionStatus.ACCEPTED
    assert driver.service().machine_batches == 1

    assert lease.submit_ingress(b"older-key") is AdmissionStatus.ACCEPTED
    capacity_deferred = driver.service()
    assert capacity_deferred.status is DriverStatus.IDLE
    assert driver.pending_resize == (4, 1)
    assert core.geometry_generation == 0
    assert core.state is TerminalState.ACTIVE
    drained = [host.take_scheduled_event(lease).event for _ in range(3)]
    assert all(event is not None for event in drained)
    assert host.take_scheduled_event(lease).event is None

    materialized = driver.service()
    assert materialized.status is DriverStatus.PROGRESS
    assert materialized.outbound_records == 1
    assert driver.pending_resize is None
    assert core.geometry_generation == 1
    assert core.state is TerminalState.RESYNCING
    assert host.pending_geometry_events == 1
