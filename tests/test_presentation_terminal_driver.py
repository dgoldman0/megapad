"""Focused host/core integration tests for the opt-in terminal driver."""

from __future__ import annotations

import struct

import pytest

from presentation_terminal import EgressWatermarks, HostPortLimits
from presentation_terminal.apt1 import (
    FrameEncoder,
    IncrementalFrameDecoder,
    MessageType,
    Offer,
    OpenRequest,
    encode_open,
    encode_probe,
    parse_negotiation,
)
from presentation_terminal.driver import (
    DriverLimits,
    DriverStatus,
    PresentationTerminalDriver,
)
from presentation_terminal.server import TerminalConfig
from system import MegapadSystem


READY = struct.Struct("<IIIIIIQ")
BEGIN = struct.Struct("<QQIIII")
SPAN = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
COMMIT = struct.Struct("<Q")


def _terminal_config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=256,
        max_transaction_bytes=512,
        terminal_receive_credit=1_024,
        max_cells=4,
        max_feed_bytes=4_608,
        cols=2,
        rows=2,
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


def _snapshot(encoder: FrameEncoder) -> bytes:
    cells = (
        (ord("A"), 7, 0, 1),
        (ord("B"), 2, 0, 8),
        (ord("C"), 4, 0, 0),
        (ord(" "), 7, 1, 0x20),
    )
    rows = (
        SPAN.pack(0, 0, 2) + b"".join(CELL.pack(*cell) for cell in cells[:2]),
        SPAN.pack(1, 0, 2) + b"".join(CELL.pack(*cell) for cell in cells[2:]),
    )
    return b"".join(
        (
            encoder.encode(MessageType.SNAPSHOT_BEGIN, BEGIN.pack(1, 0, 2, 2, 2, 4)),
            encoder.encode(MessageType.CELL_SPAN, rows[0]),
            encoder.encode(MessageType.CELL_SPAN, rows[1]),
            encoder.encode(MessageType.CURSOR, CURSOR.pack(1, 1, 1)),
            encoder.encode(MessageType.SNAPSHOT_COMMIT, COMMIT.pack(1)),
        )
    )


def _open_bytes(offer: Offer) -> tuple[bytes, FrameEncoder]:
    request = OpenRequest(
        nonce=offer.nonce,
        session_id=offer.session_id,
        client_max_payload=256,
        client_receive_credit=256,
    )
    encoder = FrameEncoder(offer.session_id, max_payload=offer.max_payload)
    ready = encoder.encode(
        MessageType.CLIENT_READY,
        READY.pack(1, 256, 0, 256, 64, 0, 0x3F),
    )
    return encode_open(request) + ready + _snapshot(encoder), encoder


def test_driver_keeps_ansi_default_then_runs_a_real_cell_snapshot():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    legacy_batches: list[bytes] = []
    ansi_batches: list[bytes] = []
    views = []
    system.uart.on_tx = None
    system.uart.on_tx_batch = legacy_batches.append
    driver = PresentationTerminalDriver.attach(
        system,
        _host_limits(),
        _terminal_config(),
        DriverLimits(4_096, 8),
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

    open_and_snapshot, _client_encoder = _open_bytes(offer)
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
    sent = driver.service()
    assert sent.outbound_records == 1
    system.run_batch_stats(1)
    key = decoder.feed(_drain_uart_rx(system))
    assert len(key) == 1 and key[0].message_type == MessageType.KEY

    assert driver.close().value == "accepted"
    _write_native_uart(system, b"legacy")
    assert legacy_batches == [b"legacy"]


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
        PresentationTerminalDriver.attach(
            system,
            too_small,
            _terminal_config(),
            DriverLimits(4_096, 8),
        )
    assert not system.presentation_terminal_host.enhanced_attached


def test_driver_retains_ordered_control_replies_across_host_backpressure():
    system = MegapadSystem(ram_size=64 * 1024, terminal_cols=2, terminal_rows=2)
    driver = PresentationTerminalDriver.attach(
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

    open_and_snapshot, _encoder = _open_bytes(offer)
    _write_native_uart(system, open_and_snapshot)
    first = driver.service()
    assert first.status is DriverStatus.BACKPRESSURED
    assert first.outbound_records == 2
    assert driver.pending_outbound_events == 1

    system.run_batch_stats(1)
    second = driver.service()
    assert second.status is DriverStatus.PROGRESS
    assert second.outbound_records == 1
    assert driver.pending_outbound_events == 0
    driver.close()
