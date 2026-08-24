"""Lightweight CELL-1 happy-path tests for the headless terminal core."""

from __future__ import annotations

import struct

import pytest

from presentation_terminal.apt1 import (
    FrameEncoder,
    IncrementalFrameDecoder,
    MessageType,
    Offer,
    OpenRequest,
    Probe,
    encode_open,
    encode_probe,
    parse_negotiation,
)
from presentation_terminal.server import (
    PresentationTerminalCore,
    TerminalConfig,
    TerminalSessionError,
    TerminalState,
)


READY = struct.Struct("<IIIIIIQ")
BEGIN = struct.Struct("<QQIIII")
SPAN = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
COMMIT = struct.Struct("<Q")
TX_RESULT = struct.Struct("<QHHQ")
CREDIT = struct.Struct("<Q")
KEY = struct.Struct("<IBBHQ")


def _config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=256,
        max_transaction_bytes=512,
        terminal_receive_credit=1_024,
        max_cells=4,
        max_feed_bytes=4_096,
        cols=2,
        rows=2,
    )


def _negotiate(*, client_credit: int = 256):
    core = PresentationTerminalCore(
        _config(),
        attachment_epoch=9,
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    probe = encode_probe(nonce)

    first = core.feed_machine(b"legacy" + probe[:11])
    assert first.ansi_bytes == b"legacy"
    assert first.outbound == ()
    second = core.feed_machine(probe[11:])
    assert second.ansi_bytes == b""
    assert len(second.outbound) == 1
    offer = parse_negotiation(second.outbound[0].payload)
    assert isinstance(offer, Offer)

    request = OpenRequest(
        nonce=nonce,
        session_id=offer.session_id,
        client_max_payload=256,
        client_receive_credit=client_credit,
    )
    encoder = FrameEncoder(offer.session_id, max_payload=offer.max_payload)
    client_ready = encoder.encode(
        MessageType.CLIENT_READY,
        READY.pack(1, 256, 0, client_credit, 64, 0, 0x3F),
    )
    return core, offer, request, encoder, client_ready


def _snapshot_frames(encoder: FrameEncoder) -> bytes:
    cells = (
        (ord("A"), 7, 0, 1),
        (ord("B"), 2, 0, 8),
        (ord("C"), 4, 0, 0),
        (ord(" "), 7, 1, 0x20),
    )
    row_zero = SPAN.pack(0, 0, 2) + b"".join(CELL.pack(*cell) for cell in cells[:2])
    row_one = SPAN.pack(1, 0, 2) + b"".join(CELL.pack(*cell) for cell in cells[2:])
    return b"".join(
        (
            encoder.encode(MessageType.SNAPSHOT_BEGIN, BEGIN.pack(1, 0, 2, 2, 2, 4)),
            encoder.encode(MessageType.CELL_SPAN, row_zero),
            encoder.encode(MessageType.CELL_SPAN, row_one),
            encoder.encode(MessageType.CURSOR, CURSOR.pack(1, 1, 1)),
            encoder.encode(MessageType.SNAPSHOT_COMMIT, COMMIT.pack(1)),
        )
    )


def test_ansi_remains_default_and_non_apt_escapes_pass_byte_exact():
    core = PresentationTerminalCore(
        _config(), attachment_epoch=1, session_id_factory=lambda: 2
    )
    assert core.state is TerminalState.ANSI
    assert core.feed_machine(b"plain\x1b[31mred").ansi_bytes == b"plain\x1b[31mred"

    held = core.feed_machine(b"tail\x1b")
    assert held.ansi_bytes == b"tail"
    assert core.feed_machine(b"X").ansi_bytes == b"\x1bX"
    assert core.state is TerminalState.ANSI
    assert core.view is None


def test_real_negotiation_snapshot_result_credit_view_and_key_event():
    core, offer, request, encoder, client_ready = _negotiate()
    machine_bytes = encode_open(request) + client_ready + _snapshot_frames(encoder)

    result = core.feed_machine(machine_bytes)

    assert result.ansi_bytes == b""
    assert core.state is TerminalState.ACTIVE
    assert len(result.views) == 1
    view = result.views[0]
    assert core.view is view
    assert view.attachment_epoch == 9
    assert view.revision == 1
    assert tuple(cell.codepoint for row in view.cells for cell in row) == (
        ord("A"), ord("B"), ord("C"), ord(" ")
    )

    # SERVER_READY, TX_RESULT, and cumulative CREDIT are all framed control
    # responses and share the server-to-client directional sequence.
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    frames = []
    for outbound in result.outbound:
        assert outbound.control
        frames.extend(decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames] == [
        MessageType.SERVER_READY,
        MessageType.TX_RESULT,
        MessageType.CREDIT,
    ]
    assert TX_RESULT.unpack(frames[1].payload) == (1, 0, 0, 1)
    assert CREDIT.unpack(frames[2].payload) == (1_024 + 312,)

    key = core.send_key(ord("x"), modifiers=1)
    assert key is not None and not key.control
    key_frame = decoder.feed(key.payload)[0]
    assert key_frame.message_type == MessageType.KEY
    assert KEY.unpack(key_frame.payload) == (ord("x"), 1, 0, 1, 1)


def test_client_receive_credit_backpressures_data_but_not_control_results():
    core, _offer, request, encoder, client_ready = _negotiate(client_credit=40)
    result = core.feed_machine(encode_open(request) + client_ready + _snapshot_frames(encoder))
    assert core.active
    assert len(result.outbound) == 3
    assert core.send_key(ord("x")) is None


def test_bad_binary_fails_closed_and_never_returns_to_ansi_locally():
    core, _offer, request, _encoder, client_ready = _negotiate()
    damaged = bytearray(client_ready)
    damaged[36] ^= 1

    with pytest.raises(TerminalSessionError, match="BAD_CRC32C"):
        core.feed_machine(encode_open(request) + damaged)
    assert core.state is TerminalState.FAILED
    with pytest.raises(TerminalSessionError, match="already failed"):
        core.feed_machine(b"ordinary ANSI must not escape")


def test_probe_cancellation_is_legal_only_before_open():
    core = PresentationTerminalCore(
        _config(), attachment_epoch=1, session_id_factory=lambda: 2
    )
    probe = encode_probe(3)
    core.feed_machine(probe)
    assert core.state is TerminalState.PROBING
    assert core.cancel_probe().ansi_bytes == b""
    assert core.state is TerminalState.ANSI

    core, _offer, request, _encoder, client_ready = _negotiate()
    core.feed_machine(encode_open(request) + client_ready)
    with pytest.raises(TerminalSessionError, match="after the OPEN"):
        core.cancel_probe()
