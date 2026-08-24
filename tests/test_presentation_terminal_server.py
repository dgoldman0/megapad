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
from presentation_terminal.retained_model import RetainedFeature, RetainedPolicy
from presentation_terminal.retained_wire import (
    RetainedMessageType,
    decode_ret_caps,
    decode_ret_formats,
    encode_ret_query,
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
ABORT = struct.Struct("<QH6x")
TX_RESULT = struct.Struct("<QHHQ")
CREDIT = struct.Struct("<Q")
KEY = struct.Struct("<IBBHQ")
TEXT_PREFIX = struct.Struct("<HHQ")
POINTER = struct.Struct("<iiHHHHhhQ")
RESIZE = struct.Struct("<IIQ")
FOCUS = struct.Struct("<B7sQ")
CLOSE = struct.Struct("<H6sQ")
CLOSE_ACK = struct.Struct("<H6s")
SOFT_RESET_REQUEST = struct.Struct("<I4xQ")
SOFT_RESET_ACK = struct.Struct("<IHH")


def _config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=256,
        max_transaction_bytes=512,
        terminal_receive_credit=1_024,
        max_cells=4,
        max_feed_bytes=4_096,
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
        max_label_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=256,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=512,
    )


def _negotiate(
    *,
    client_credit: int = 256,
    client_max_payload: int = 256,
    retained_policy: RetainedPolicy | None = None,
):
    core = PresentationTerminalCore(
        _config(),
        attachment_epoch=9,
        retained_policy=retained_policy,
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
        client_max_payload=client_max_payload,
        client_receive_credit=client_credit,
    )
    encoder = FrameEncoder(offer.session_id, max_payload=offer.max_payload)
    client_ready = encoder.encode(
        MessageType.CLIENT_READY,
        READY.pack(
            1,
            client_max_payload,
            0,
            client_credit,
            min(64, client_max_payload - 12),
            0,
            0x3F,
        ),
    )
    return core, offer, request, encoder, client_ready


def _snapshot_frames(
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


def _settle_results(
    core: PresentationTerminalCore,
    result,
) -> tuple[int, ...]:
    transaction_ids = tuple(
        outbound.result_transaction_id
        for outbound in result.outbound
        if outbound.result_transaction_id is not None
    )
    for transaction_id in transaction_ids:
        core.settle_result_delivery(transaction_id)
    return transaction_ids


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


def test_real_negotiation_snapshot_result_credit_view_and_normalized_input():
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
    assert core.outstanding_result_transaction_id == 1
    with pytest.raises(TerminalSessionError, match="result boundaries"):
        core.send_key(ord("x"))
    assert _settle_results(core, result) == (1,)
    assert core.outstanding_result_transaction_id is None

    key = core.send_key(ord("x"), modifiers=1)
    assert key is not None and not key.control
    key_frame = decoder.feed(key.payload)[0]
    assert key_frame.message_type == MessageType.KEY
    assert KEY.unpack(key_frame.payload) == (ord("x"), 1, 0, 1, 1)

    assert core.max_text_bytes == 64
    text = core.send_text("café".encode(), paste=True)
    pointer = core.send_pointer(1, 0, buttons=1, modifiers=2, kind=2)
    focus = core.send_focus(True)
    assert text is not None and pointer is not None and focus is not None
    input_frames = decoder.feed(text.payload + pointer.payload + focus.payload)
    assert [frame.message_type for frame in input_frames] == [
        MessageType.TEXT,
        MessageType.POINTER,
        MessageType.FOCUS,
    ]
    assert TEXT_PREFIX.unpack(input_frames[0].payload[:12]) == (1, 0, 1)
    assert input_frames[0].payload[12:] == "café".encode()
    assert POINTER.unpack(input_frames[1].payload) == (
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
    assert FOCUS.unpack(input_frames[2].payload) == (1, bytes(7), 1)

    with pytest.raises(ValueError, match="well-formed UTF-8"):
        core.send_text(b"\xff")
    with pytest.raises(ValueError, match="wheel deltas"):
        core.send_pointer(0, 0, wheel_y=1)
    with pytest.raises(TypeError, match="focused must be bool"):
        core.send_focus(1)


def test_retained_query_emits_exact_adjacent_replies_before_covering_credit():
    policy = _retained_policy()
    core, offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        retained_policy=policy,
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    outbound_decoder = IncrementalFrameDecoder(
        offer.session_id,
        max_payload=256,
    )
    for outbound in opened.outbound:
        outbound_decoder.feed(outbound.payload)
    assert _settle_results(core, opened) == (1,)
    assert not core.retained_enabled
    assert core.retained_policy is None

    discovered = core.feed_machine(
        encoder.encode(
            RetainedMessageType.RET_QUERY,
            encode_ret_query(),
        )
    )

    frames = []
    for outbound in discovered.outbound:
        frames.extend(outbound_decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames] == [
        RetainedMessageType.RET_CAPS,
        RetainedMessageType.RET_FORMATS,
        MessageType.CREDIT,
    ]
    assert [outbound.control for outbound in discovered.outbound] == [
        False,
        False,
        True,
    ]
    caps = decode_ret_caps(frames[0].payload)
    formats = decode_ret_formats(frames[1].payload)
    assert caps.features == policy.features
    assert caps.max_regions == policy.max_regions
    assert formats.coordinate_format == 1
    assert formats.total_sample_slots == 0
    assert CREDIT.unpack(frames[2].payload) == (1_024 + 312 + 48,)
    assert core.retained_enabled
    assert core.retained_policy is not None
    assert core.retained_policy.max_regions == policy.max_regions


@pytest.mark.parametrize(
    ("policy", "client_max_payload"),
    (
        (None, 256),
        (_retained_policy(), 48),
    ),
)
def test_retained_query_keeps_cell_only_fallback_without_complete_support(
    policy: RetainedPolicy | None,
    client_max_payload: int,
):
    core, offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        client_max_payload=client_max_payload,
        retained_policy=policy,
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    discovered = core.feed_machine(
        encoder.encode(
            RetainedMessageType.RET_QUERY,
            encode_ret_query(),
        )
    )

    decoder = IncrementalFrameDecoder(
        offer.session_id,
        max_payload=client_max_payload,
    )
    frames = []
    for outbound in opened.outbound + discovered.outbound:
        frames.extend(decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames[-1:]] == [MessageType.CREDIT]
    assert CREDIT.unpack(frames[-1].payload) == (1_024 + 312 + 48,)
    assert not core.retained_enabled
    assert core.retained_policy is None


def test_retained_query_waits_for_initial_snapshot_result_delivery():
    core, _offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        retained_policy=_retained_policy(),
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    assert core.outstanding_result_transaction_id == 1

    with pytest.raises(TerminalSessionError, match="settled initial snapshot"):
        core.feed_machine(
            encoder.encode(
                RetainedMessageType.RET_QUERY,
                encode_ret_query(),
            )
        )
    assert not core.retained_enabled


def test_retained_query_remains_valid_after_a_later_settled_cell_delta():
    core, _offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        retained_policy=_retained_policy(),
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    delta = core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 2, 2, 0, 0),
        )
        + encoder.encode(MessageType.CURSOR, CURSOR.pack(0, 0, 1))
        + encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2))
    )
    _settle_results(core, delta)
    assert core.presentation_revision == 2

    discovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    assert len(discovered.outbound) == 3
    assert core.retained_enabled


def test_soft_reset_clears_retained_and_requires_snapshot_first_rediscovery():
    core, offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        retained_policy=_retained_policy(),
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    first_discovery = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    assert core.retained_enabled

    outbound_decoder = IncrementalFrameDecoder(
        offer.session_id,
        max_payload=256,
    )
    for outbound in opened.outbound + first_discovery.outbound:
        outbound_decoder.feed(outbound.payload)
    reset = core.request_soft_reset()
    reset_frame = outbound_decoder.feed(reset.payload)[0]
    assert reset_frame.message_type == MessageType.SOFT_RESET_REQUEST

    encoder.set_presentation_epoch(1)
    core.feed_machine(
        encoder.encode(
            MessageType.SOFT_RESET_ACK,
            SOFT_RESET_ACK.pack(1, 0, 0),
        )
    )
    assert not core.retained_enabled
    assert core.retained_policy is None

    outbound_decoder.advance_presentation_epoch(1)
    replacement = core.feed_machine(_snapshot_frames(encoder, transaction_id=1))
    for outbound in replacement.outbound:
        outbound_decoder.feed(outbound.payload)
    _settle_results(core, replacement)
    rediscovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    rediscovery_frames = []
    for outbound in rediscovered.outbound:
        rediscovery_frames.extend(outbound_decoder.feed(outbound.payload))
    assert [frame.message_type for frame in rediscovery_frames] == [
        RetainedMessageType.RET_CAPS,
        RetainedMessageType.RET_FORMATS,
        MessageType.CREDIT,
    ]
    assert core.retained_enabled


def test_client_receive_credit_backpressures_data_but_not_control_results():
    core, _offer, request, encoder, client_ready = _negotiate(client_credit=40)
    result = core.feed_machine(encode_open(request) + client_ready + _snapshot_frames(encoder))
    assert core.active
    assert len(result.outbound) == 3
    assert _settle_results(core, result) == (1,)
    assert core.send_key(ord("x")) is None


def test_resize_requires_one_replacement_snapshot_at_the_new_geometry():
    core, offer, request, encoder, client_ready = _negotiate(client_credit=512)
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    assert _settle_results(core, opened) == (1,)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)

    resize = core.send_resize(4, 1)
    assert resize is not None
    assert core.state is TerminalState.RESYNCING
    assert core.geometry_generation == 1
    assert core.view is None
    resize_frame = decoder.feed(resize.payload)[0]
    assert resize_frame.message_type == MessageType.RESIZE
    assert RESIZE.unpack(resize_frame.payload) == (4, 1, 1)
    with pytest.raises(TerminalSessionError, match="ACTIVE"):
        core.send_key(ord("x"))

    duplicate = core.feed_machine(
        _snapshot_frames(encoder, transaction_id=1, cols=4, rows=1)
    )
    duplicate_result = next(
        outbound
        for outbound in duplicate.outbound
        if outbound.result_transaction_id is not None
    )
    duplicate_frame = decoder.feed(duplicate_result.payload)[0]
    assert TX_RESULT.unpack(duplicate_frame.payload) == (1, 2, 0, 0)
    assert _settle_results(core, duplicate) == (1,)

    replacement = core.feed_machine(
        _snapshot_frames(encoder, transaction_id=2, cols=4, rows=1)
    )
    assert core.state is TerminalState.ACTIVE
    assert len(replacement.views) == 1
    assert (replacement.views[0].cols, replacement.views[0].rows) == (4, 1)
    assert replacement.views[0].revision == 1


def test_resize_waits_for_rejected_transaction_wire_boundary():
    core, _offer, request, encoder, client_ready = _negotiate(client_credit=512)
    core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    core.settle_result_delivery(1)

    rejected_begin = encoder.encode(
        MessageType.TX_BEGIN,
        BEGIN.pack(2, 1, 3, 2, 0, 0),
    )
    core.feed_machine(rejected_begin)
    assert not core.resize_ready
    with pytest.raises(TerminalSessionError, match="transaction boundary"):
        core.send_resize(4, 1)

    rejected = core.feed_machine(
        encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2))
    )
    assert core.outstanding_result_transaction_id == 2
    assert not core.resize_ready
    assert _settle_results(core, rejected) == (2,)
    assert core.resize_ready


def test_rejected_begin_can_drain_through_abort_without_a_result_gate():
    core, _offer, request, encoder, client_ready = _negotiate(client_credit=512)
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)

    core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 3, 2, 0, 0),
        )
    )
    aborted = core.feed_machine(
        encoder.encode(MessageType.TX_ABORT, ABORT.pack(2, 7))
    )

    assert core.outstanding_result_transaction_id is None
    assert len(aborted.outbound) == 1
    assert aborted.outbound[0].result_transaction_id is None
    assert core.feed_machine(
        encoder.encode(MessageType.TX_ABORT, ABORT.pack(2, 7))
    ).outbound == ()

    completed = core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(3, 1, 2, 2, 0, 0),
        )
        + encoder.encode(MessageType.CURSOR, CURSOR.pack(0, 0, 0))
        + encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(3))
    )
    assert len(completed.views) == 1
    assert completed.views[0].revision == 2
    assert core.outstanding_result_transaction_id == 3


def test_soft_reset_cancels_crossed_commit_then_resets_clock_and_cell_model():
    core, offer, request, encoder, client_ready = _negotiate(client_credit=512)
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    outbound_decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    for outbound in opened.outbound:
        outbound_decoder.feed(outbound.payload)

    core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 2, 2, 0, 0),
        )
    )
    reset = core.request_soft_reset()
    reset_frame = outbound_decoder.feed(reset.payload)[0]
    assert reset_frame.message_type == MessageType.SOFT_RESET_REQUEST
    assert SOFT_RESET_REQUEST.unpack(reset_frame.payload) == (1, 1)
    assert core.state is TerminalState.RESYNCING
    assert core.view is None

    crossed = core.feed_machine(
        encoder.encode(MessageType.CURSOR, CURSOR.pack(0, 0, 0))
        + encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2))
    )
    crossed_frames = []
    for outbound in crossed.outbound:
        crossed_frames.extend(outbound_decoder.feed(outbound.payload))
    assert TX_RESULT.unpack(crossed_frames[0].payload) == (2, 1, 0, 1)
    assert crossed.views == ()
    assert core.presentation_revision == 1
    assert _settle_results(core, crossed) == (2,)

    encoder.set_presentation_epoch(1)
    acknowledged = core.feed_machine(
        encoder.encode(
            MessageType.SOFT_RESET_ACK,
            SOFT_RESET_ACK.pack(1, 0, 0),
        )
    )
    assert acknowledged.outbound == ()
    assert core.presentation_revision == 0
    assert core.state is TerminalState.RESYNCING

    outbound_decoder.advance_presentation_epoch(1)
    replacement = core.feed_machine(_snapshot_frames(encoder, transaction_id=1))
    assert len(replacement.views) == 1
    assert replacement.views[0].presentation_epoch == 1
    assert replacement.views[0].revision == 1
    assert core.presentation_revision == 1
    assert core.state is TerminalState.ACTIVE


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


def test_client_close_is_acknowledged_before_returning_to_ansi():
    core, offer, request, encoder, client_ready = _negotiate()
    close = encoder.encode(
        MessageType.CLOSE,
        CLOSE.pack(7, bytes(6), 0),
    )

    result = core.feed_machine(encode_open(request) + client_ready + close)

    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    frames = []
    for outbound in result.outbound:
        frames.extend(decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames] == [
        MessageType.SERVER_READY,
        MessageType.CLOSE_ACK,
    ]
    assert CLOSE_ACK.unpack(frames[1].payload) == (7, bytes(6))
    assert core.state is TerminalState.ANSI
    assert core.view is None
    assert core.feed_machine(b"legacy").ansi_bytes == b"legacy"


def test_ready_payload_floor_is_enforced_for_narrow_geometries():
    with pytest.raises(ValueError, match="READY"):
        TerminalConfig(
            max_payload=20,
            max_transaction_bytes=256,
            terminal_receive_credit=256,
            max_cells=1,
            max_feed_bytes=4_352,
            max_cols=1,
            max_rows=1,
            cols=1,
            rows=1,
        )
