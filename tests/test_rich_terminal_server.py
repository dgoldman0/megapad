"""Lightweight CELL-1 happy-path tests for the headless terminal core."""

from __future__ import annotations

import struct

import pytest

from rich_terminal.apt1 import (
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
from rich_terminal.retained_model import (
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_scene import (
    ExplicitSamples,
    GroupBody,
    ObjectBounds,
    RGBA,
    ReadoutBody,
    ReadoutFormat,
    RebuildRequirement,
    Sample,
    TimestampMode,
)
from rich_terminal.retained_wire import (
    CellMode,
    ObjectSetValue,
    ObjectSetVisibility,
    ObjectWireDefinition,
    OwnerDrop,
    OwnerOpen,
    PresentBegin,
    PresentCommit,
    PresentDisposition,
    PresentRetainedMode,
    RegionWireDefinition,
    RetainedItemReference,
    RetStatus,
    RetainedMessageType,
    SeriesWireDefinition,
    SeriesWireSamples,
    decode_ret_caps,
    decode_ret_formats,
    decode_ret_result,
    encode_object_definition,
    encode_object_drop,
    encode_object_replace,
    encode_object_set_value,
    encode_object_set_visibility,
    encode_owner_drop,
    encode_owner_open,
    encode_present_begin,
    encode_present_commit,
    encode_region_definition,
    encode_region_drop,
    encode_region_replace,
    encode_ret_query,
    encode_series_append,
    encode_series_definition,
    encode_series_drop,
    encode_series_replace,
)
from rich_terminal.server import (
    RichTerminalCore,
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
OWNER_OPEN_WIRE = struct.Struct("<QQIIIIQQQQ")
OWNER_DROP_WIRE = struct.Struct("<QQQQ")


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


def _soundlab_policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=(
            RetainedFeature.CORE
            | RetainedFeature.VECTOR
            | RetainedFeature.INSTRUMENT
            | RetainedFeature.SERIES
        ),
        max_owner_records=4,
        max_live_owners=2,
        max_regions=8,
        max_resources=0,
        max_objects=8,
        max_series=4,
        max_operations_per_transaction=8,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=512,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=8,
        max_label_bytes=64,
        max_samples_per_append=4,
        max_history_per_series=8,
        minimum_presentation_interval_us=0,
        total_sample_slots=16,
        total_utf8_bytes=128,
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
    core = RichTerminalCore(
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
    core: RichTerminalCore,
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


def _open_retained_core(*, retained_policy: RetainedPolicy | None = None):
    core, offer, request, encoder, client_ready = _negotiate(
        client_credit=512,
        retained_policy=(
            _retained_policy() if retained_policy is None else retained_policy
        ),
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    discovery = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    for outbound in opened.outbound + discovery.outbound:
        decoder.feed(outbound.payload)
    assert core.retained_enabled
    assert core.owner_state is not None
    return core, encoder, decoder


def _owner_open(
    owner_id: int = 7,
    generation: int = 1,
    *,
    regions: int = 4,
    objects: int = 0,
    series: int = 0,
    utf8_bytes: int = 0,
    sample_slots: int = 0,
) -> OwnerOpen:
    return OwnerOpen(
        owner_id,
        generation,
        OwnerQuotas(
            regions,
            0,
            objects,
            series,
            0,
            utf8_bytes,
            sample_slots,
        ),
    )


def _settle_lifecycle(core: RichTerminalCore, result) -> None:
    marker = next(
        outbound.lifecycle_result
        for outbound in result.outbound
        if outbound.lifecycle_result is not None
    )
    core.settle_lifecycle_result_delivery(marker)


def _present_frames(
    encoder: FrameEncoder,
    *,
    transaction_id: int,
    base_revision: int,
    retained_mode: PresentRetainedMode,
    disposition: PresentDisposition,
    operations: tuple[tuple[RetainedMessageType, bytes], ...] = (),
    declared_adjustment: int = 0,
) -> bytes:
    declared = (
        104
        + sum(40 + len(payload) for _message_type, payload in operations)
        + 56
        + declared_adjustment
    )
    begin = PresentBegin(
        transaction_id,
        base_revision,
        0,
        declared,
        2,
        2,
        0,
        0,
        len(operations),
        CellMode.NONE,
        retained_mode,
    )
    commit = PresentCommit(transaction_id, disposition)
    return b"".join(
        (
            encoder.encode(
                RetainedMessageType.PRESENT_BEGIN,
                encode_present_begin(begin),
            ),
            *(
                encoder.encode(message_type, payload)
                for message_type, payload in operations
            ),
            encoder.encode(
                RetainedMessageType.PRESENT_COMMIT,
                encode_present_commit(commit),
            ),
        )
    )


def test_ansi_remains_default_and_non_apt_escapes_pass_byte_exact():
    core = RichTerminalCore(
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
    assert core.model_revision == 2

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


def test_owner_open_reserves_atomically_and_reports_exact_lifecycle_statuses():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(owner),
        )
    )
    frames = []
    for outbound in opened.outbound:
        frames.extend(decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames] == [
        RetainedMessageType.RET_RESULT,
        MessageType.CREDIT,
    ]
    result = decode_ret_result(frames[0].payload)
    assert result.request_type is RetainedMessageType.OWNER_OPEN
    assert result.status is RetStatus.OK
    assert result.current_revision == 1
    assert CREDIT.unpack(frames[1].payload) == (1_024 + 312 + 48 + 104,)
    assert core.outstanding_lifecycle_result is opened.outbound[0].lifecycle_result
    state = core.owner_state
    assert state is not None
    assert state.records[owner.owner_id].identity.owner_generation == 1
    assert state.reservations.regions == 4
    _settle_lifecycle(core, opened)

    source = core.owner_state
    duplicate = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(owner),
        )
    )
    duplicate_result = decode_ret_result(
        decoder.feed(duplicate.outbound[0].payload)[0].payload
    )
    decoder.feed(duplicate.outbound[1].payload)
    assert duplicate_result.status is RetStatus.OK
    assert core.owner_state is source
    _settle_lifecycle(core, duplicate)

    stale = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(_owner_open(generation=2)),
        )
    )
    stale_result = decode_ret_result(decoder.feed(stale.outbound[0].payload)[0].payload)
    decoder.feed(stale.outbound[1].payload)
    assert stale_result.status is RetStatus.STALE_OWNER
    assert core.owner_state is source
    _settle_lifecycle(core, stale)

    exhausted = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(_owner_open(owner_id=8, regions=5)),
        )
    )
    exhausted_result = decode_ret_result(
        decoder.feed(exhausted.outbound[0].payload)[0].payload
    )
    decoder.feed(exhausted.outbound[1].payload)
    assert exhausted_result.status is RetStatus.NO_CAPACITY
    assert core.owner_state is source
    _settle_lifecycle(core, exhausted)


@pytest.mark.parametrize(
    ("owner_id", "owner_generation", "reserved"),
    ((0, 1, 0), (7, 0, 0), (7, 1, 1)),
)
def test_owner_open_well_framed_scalar_rejections_echo_ret_invalid(
    owner_id: int,
    owner_generation: int,
    reserved: int,
):
    core, encoder, decoder = _open_retained_core()
    source = core.owner_state
    payload = OWNER_OPEN_WIRE.pack(
        owner_id,
        owner_generation,
        4,
        0,
        0,
        0,
        0,
        0,
        0,
        reserved,
    )

    rejected = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, payload)
    )
    result_frame = decoder.feed(rejected.outbound[0].payload)[0]
    decoder.feed(rejected.outbound[1].payload)

    result = decode_ret_result(result_frame.payload)
    assert result.request_type is RetainedMessageType.OWNER_OPEN
    assert result.status is RetStatus.INVALID
    assert result.owner_id == owner_id
    assert result.owner_generation == owner_generation
    assert result.item_id == 0
    assert result.current_revision == 1
    assert result.accepted_bytes == 0
    assert core.owner_state is source
    _settle_lifecycle(core, rejected)


def test_owner_open_result_gate_blocks_a_second_lifecycle_request():
    core, encoder, _decoder = _open_retained_core()
    owner = _owner_open()
    core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(owner),
        )
    )

    with pytest.raises(TerminalSessionError, match="outstanding RET_RESULT"):
        core.feed_machine(
            encoder.encode(
                RetainedMessageType.OWNER_OPEN,
                encode_owner_open(_owner_open(owner_id=8)),
            )
        )


def test_owner_drop_uses_shared_revision_and_exact_tombstone_authority():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)

    dropped = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(2, 1, owner.owner_id, owner.owner_generation)),
        )
    )
    drop_frame = decoder.feed(dropped.outbound[0].payload)[0]
    assert TX_RESULT.unpack(drop_frame.payload) == (2, 0, 0, 2)
    assert dropped.views == (core.output_view,)
    state = core.owner_state
    assert state is not None
    assert not state.records[owner.owner_id].live
    assert state.reservations.live_owners == 0
    core.settle_result_delivery(2)

    tombstone = core.owner_state
    repeated = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(3, 2, owner.owner_id, owner.owner_generation)),
        )
    )
    assert TX_RESULT.unpack(decoder.feed(repeated.outbound[0].payload)[0].payload) == (
        3,
        0,
        0,
        3,
    )
    assert repeated.views == (core.output_view,)
    assert core.owner_state is tombstone
    core.settle_result_delivery(3)

    stale = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(4, 3, owner.owner_id, 2)),
        )
    )
    assert TX_RESULT.unpack(decoder.feed(stale.outbound[0].payload)[0].payload) == (
        4,
        2,
        0,
        3,
    )
    assert core.owner_state is tombstone
    core.settle_result_delivery(4)

    stale_base = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(5, 2, owner.owner_id, owner.owner_generation)),
        )
    )
    assert TX_RESULT.unpack(
        decoder.feed(stale_base.outbound[0].payload)[0].payload
    ) == (5, 3, 0, 3)
    assert core.owner_state is tombstone


@pytest.mark.parametrize(
    ("owner_id", "owner_generation"),
    ((0, 1), (7, 0)),
)
def test_owner_drop_invalid_owner_scalars_return_status_two_without_mutation(
    owner_id: int,
    owner_generation: int,
):
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    source = core.owner_state

    rejected = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            OWNER_DROP_WIRE.pack(2, 1, owner_id, owner_generation),
        )
    )

    assert TX_RESULT.unpack(decoder.feed(rejected.outbound[0].payload)[0].payload) == (
        2,
        2,
        0,
        1,
    )
    assert core.owner_state is source
    assert core.model_revision == 1
    core.settle_result_delivery(2)


@pytest.mark.parametrize(
    ("message_type", "payload", "detail"),
    (
        (
            RetainedMessageType.OWNER_OPEN,
            bytes(OWNER_OPEN_WIRE.size - 1),
            "OWNER_OPEN payload length",
        ),
        (
            RetainedMessageType.OWNER_DROP,
            bytes(OWNER_DROP_WIRE.size - 1),
            "OWNER_DROP payload length",
        ),
    ),
)
def test_owner_lifecycle_wrong_fixed_length_remains_structurally_fatal(
    message_type: RetainedMessageType,
    payload: bytes,
    detail: str,
):
    core, encoder, _decoder = _open_retained_core()

    with pytest.raises(TerminalSessionError, match=detail):
        core.feed_machine(encoder.encode(message_type, payload))


def test_soft_reset_settles_one_crossed_owner_open_then_destroys_epoch_authority():
    core, encoder, decoder = _open_retained_core()
    reset = core.request_soft_reset()
    assert decoder.feed(reset.payload)[0].message_type == MessageType.SOFT_RESET_REQUEST

    crossed = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(_owner_open()),
        )
    )
    result_frame = decoder.feed(crossed.outbound[0].payload)[0]
    decoder.feed(crossed.outbound[1].payload)
    assert decode_ret_result(result_frame.payload).status is RetStatus.OK
    assert core.owner_state is not None
    assert core.owner_state.reservations.live_owners == 1
    _settle_lifecycle(core, crossed)

    encoder.set_presentation_epoch(1)
    acknowledged = core.feed_machine(
        encoder.encode(
            MessageType.SOFT_RESET_ACK,
            SOFT_RESET_ACK.pack(1, 0, 0),
        )
    )
    assert acknowledged.outbound == ()
    assert core.owner_state is None
    assert not core.retained_enabled


def test_soft_reset_cancels_crossed_exact_owner_drop_with_status_one():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    decoder.feed(core.request_soft_reset().payload)

    crossed = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(2, 1, owner.owner_id, owner.owner_generation)),
        )
    )
    assert TX_RESULT.unpack(decoder.feed(crossed.outbound[0].payload)[0].payload) == (
        2,
        1,
        0,
        1,
    )
    state = core.owner_state
    assert state is not None
    assert state.records[owner.owner_id].live
    core.settle_result_delivery(2)


def test_synchronized_close_retires_settled_owner_authority():
    core, encoder, _decoder = _open_retained_core()
    opened = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_OPEN,
            encode_owner_open(_owner_open()),
        )
    )
    _settle_lifecycle(core, opened)
    assert core.owner_state is not None

    core.feed_machine(
        encoder.encode(
            MessageType.CLOSE,
            CLOSE.pack(0, bytes(6), 1),
        )
    )
    assert core.state is TerminalState.ANSI
    assert core.owner_state is None


def test_present_region_hidden_replace_commits_then_reveals_atomically():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)

    started = core.feed_machine(
        _present_frames(
            encoder,
            transaction_id=2,
            base_revision=1,
            retained_mode=PresentRetainedMode.REPLACE_START,
            disposition=PresentDisposition.COMMIT,
            operations=((RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),),
        )
    )
    frames = []
    for outbound in started.outbound:
        frames.extend(decoder.feed(outbound.payload))
    assert TX_RESULT.unpack(frames[0].payload) == (2, 0, 0, 2)
    assert frames[1].message_type == MessageType.CREDIT
    assert len(started.views) == 1
    hidden = core.retained_state
    assert hidden is not None
    assert hidden.revision == 2
    assert hidden.hidden is not None
    assert hidden.active.owners == {}
    assert hidden.hidden.owners[7].regions[1].geometry_generation == 0
    assert not hidden.retained_visible
    assert core.owner_state is not None
    assert core.owner_state.records[7].high_water.region == 1
    core.settle_result_delivery(2)
    with pytest.raises(TerminalSessionError, match="hidden-target reveal"):
        core.send_key(ord("x"))

    revealed = core.feed_machine(
        _present_frames(
            encoder,
            transaction_id=3,
            base_revision=2,
            retained_mode=PresentRetainedMode.REPLACE_CONTINUE,
            disposition=PresentDisposition.COMMIT_AND_REVEAL,
        )
    )
    reveal_frames = []
    for outbound in revealed.outbound:
        reveal_frames.extend(decoder.feed(outbound.payload))
    assert TX_RESULT.unpack(reveal_frames[0].payload) == (3, 0, 0, 3)
    assert len(revealed.views) == 1
    state = core.retained_state
    assert state is not None
    assert state.revision == 3
    assert state.hidden is None
    assert state.active.owners[7].regions[1].visible
    assert state.retained_visible
    assert revealed.views[0].retained is state
    core.settle_result_delivery(3)


def test_present_declared_byte_mismatch_rejects_without_scene_or_id_publication():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    scene_source = core.retained_state
    owner_source = core.owner_state
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)

    rejected = core.feed_machine(
        _present_frames(
            encoder,
            transaction_id=2,
            base_revision=1,
            retained_mode=PresentRetainedMode.REPLACE_START,
            disposition=PresentDisposition.COMMIT,
            operations=((RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),),
            declared_adjustment=1,
        )
    )

    assert TX_RESULT.unpack(decoder.feed(rejected.outbound[0].payload)[0].payload) == (
        2,
        2,
        0,
        1,
    )
    decoder.feed(rejected.outbound[1].payload)
    assert rejected.views == ()
    assert core.retained_state is scene_source
    assert core.owner_state is owner_source
    core.settle_result_delivery(2)


def test_present_abort_discards_only_transaction_staging_and_returns_credit():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    scene_source = core.retained_state
    owner_source = core.owner_state
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)
    begin = PresentBegin(
        2,
        1,
        0,
        248,
        2,
        2,
        0,
        0,
        1,
        CellMode.NONE,
        PresentRetainedMode.REPLACE_START,
    )
    core.feed_machine(
        encoder.encode(
            RetainedMessageType.PRESENT_BEGIN,
            encode_present_begin(begin),
        )
        + encoder.encode(
            RetainedMessageType.REGION_DEFINE,
            encode_region_definition(region),
        )
    )

    aborted = core.feed_machine(
        encoder.encode(MessageType.TX_ABORT, ABORT.pack(2, 7))
    )

    assert len(aborted.outbound) == 1
    assert (
        decoder.feed(aborted.outbound[0].payload)[0].message_type
        == MessageType.CREDIT
    )
    assert core.outstanding_result_transaction_id is None
    assert core.model_revision == 1
    assert core.retained_state is scene_source
    assert core.owner_state is owner_source


def test_present_transaction_rejects_an_intervening_control_frame():
    core, encoder, _decoder = _open_retained_core()
    begin = PresentBegin(
        2,
        1,
        0,
        160,
        2,
        2,
        0,
        0,
        0,
        CellMode.NONE,
        PresentRetainedMode.REPLACE_START,
    )
    core.feed_machine(
        encoder.encode(
            RetainedMessageType.PRESENT_BEGIN,
            encode_present_begin(begin),
        )
    )

    with pytest.raises(TerminalSessionError, match="intervened inside a PRESENT"):
        core.feed_machine(encoder.encode(MessageType.CREDIT, CREDIT.pack(4_096)))

    assert core.model_revision == 1


def test_owner_drop_retires_committed_scene_and_authority_as_one_revision():
    core, encoder, decoder = _open_retained_core()
    owner = _owner_open()
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)
    started = core.feed_machine(
        _present_frames(
            encoder,
            transaction_id=2,
            base_revision=1,
            retained_mode=PresentRetainedMode.REPLACE_START,
            disposition=PresentDisposition.COMMIT,
            operations=((RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),),
        )
    )
    for outbound in started.outbound:
        decoder.feed(outbound.payload)
    core.settle_result_delivery(2)
    owner_source = core.owner_state
    scene_source = core.retained_state

    stale = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(3, 2, 7, 2)),
        )
    )
    assert TX_RESULT.unpack(decoder.feed(stale.outbound[0].payload)[0].payload) == (
        3,
        2,
        0,
        2,
    )
    assert core.owner_state is owner_source
    assert core.retained_state is scene_source
    assert core.model_revision == 2
    assert stale.views == ()
    core.settle_result_delivery(3)

    dropped = core.feed_machine(
        encoder.encode(
            RetainedMessageType.OWNER_DROP,
            encode_owner_drop(OwnerDrop(4, 2, 7, 1)),
        )
    )
    assert TX_RESULT.unpack(decoder.feed(dropped.outbound[0].payload)[0].payload) == (
        4,
        0,
        0,
        3,
    )
    assert core.model_revision == 3
    assert core.owner_state is not owner_source
    assert core.owner_state is not None
    assert not core.owner_state.records[7].live
    assert core.retained_state is not scene_source
    assert core.retained_state is not None
    assert core.retained_state.hidden is not None
    assert 7 not in core.retained_state.hidden.owners
    assert scene_source is not None
    assert scene_source.hidden is not None
    assert scene_source.hidden.owners[7].owner.owner_generation == 1
    assert core.output_view is not None
    assert core.output_view.retained is core.retained_state
    assert dropped.views == (core.output_view,)


def test_retained_resize_requires_present_cell_replace_before_publication():
    core, encoder, decoder = _open_retained_core()
    old_output = core.output_view

    assert core.resize_ready
    resize = core.send_resize(4, 1)
    assert resize is not None
    assert RESIZE.unpack(decoder.feed(resize.payload)[0].payload) == (4, 1, 1)
    assert core.state is TerminalState.RESYNCING
    assert core.selected_geometry == (4, 1)
    assert core.geometry_generation == 1
    assert core.view is None
    assert core.output_view is old_output
    assert core.retained_state is not None
    assert core.retained_state.requirement is RebuildRequirement.REPLACE
    assert not core.retained_state.retained_visible

    cells = b"".join(
        CELL.pack(codepoint, 7, 0, 0)
        for codepoint in (ord("W"), ord("I"), ord("D"), ord("E"))
    )
    begin = PresentBegin(
        2,
        1,
        1,
        300,
        4,
        1,
        1,
        4,
        0,
        CellMode.REPLACE,
        PresentRetainedMode.NONE,
    )
    replaced = core.feed_machine(
        encoder.encode(
            RetainedMessageType.PRESENT_BEGIN,
            encode_present_begin(begin),
        )
        + encoder.encode(MessageType.CELL_SPAN, SPAN.pack(0, 0, 4) + cells)
        + encoder.encode(MessageType.CURSOR, CURSOR.pack(0, 3, 1))
        + encoder.encode(
            RetainedMessageType.PRESENT_COMMIT,
            encode_present_commit(PresentCommit(2, PresentDisposition.COMMIT)),
        )
    )

    assert core.state is TerminalState.ACTIVE
    assert core.model_revision == 2
    assert replaced.views == (core.output_view,)
    assert core.output_view is not old_output
    assert core.output_view is not None
    assert core.output_view.geometry.generation == 1
    assert core.output_view.cell is core.view
    assert core.output_view.retained is core.retained_state


def test_legacy_snapshot_begin_is_forbidden_after_retained_discovery():
    core, encoder, _decoder = _open_retained_core()
    view = core.view
    owner_state = core.owner_state

    with pytest.raises(TerminalSessionError, match="forbidden after retained"):
        core.feed_machine(_snapshot_frames(encoder, transaction_id=2))

    assert core.model_revision == 1
    assert core.view is view
    assert core.owner_state is owner_state
    assert core.outstanding_result_transaction_id is None


def test_legacy_cell_delta_remains_valid_after_retained_discovery():
    core, encoder, decoder = _open_retained_core()

    committed = core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 2, 2, 0, 0),
        )
        + encoder.encode(MessageType.CURSOR, CURSOR.pack(1, 1, 1))
        + encoder.encode(MessageType.TX_COMMIT, COMMIT.pack(2))
    )

    assert TX_RESULT.unpack(decoder.feed(committed.outbound[0].payload)[0].payload) == (
        2,
        0,
        0,
        2,
    )
    assert len(committed.views) == 1
    assert committed.views[0].revision == 2
    assert committed.views[0].cell is core.view
    assert committed.views[0].retained is core.retained_state
    core.settle_result_delivery(2)


def test_soundlab_retained_vocabulary_dispatches_through_atomic_composite_views():
    core, encoder, decoder = _open_retained_core(
        retained_policy=_soundlab_policy()
    )
    owner = _owner_open(
        objects=4,
        series=2,
        utf8_bytes=64,
        sample_slots=8,
    )
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)
    series = SeriesWireDefinition(7, 1, 1, 4, TimestampMode.EXPLICIT, 0)
    bounds = ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF)
    group = ObjectWireDefinition(
        owner_id=7,
        owner_generation=1,
        object_id=1,
        region_id=1,
        parent_object_id=0,
        bounds=bounds,
        z_order=0,
        visible=True,
        body=GroupBody(),
    )
    readout = ObjectWireDefinition(
        owner_id=7,
        owner_generation=1,
        object_id=2,
        region_id=1,
        parent_object_id=1,
        bounds=bounds,
        z_order=1,
        visible=True,
        body=ReadoutBody(
            RGBA(255, 255, 255, 255),
            RGBA(0, 0, 0, 255),
            ReadoutFormat.FIXED,
            1,
            -10,
            10,
            " dB",
        ),
    )

    def commit(
        transaction_id: int,
        mode: PresentRetainedMode,
        operations: tuple[tuple[RetainedMessageType, bytes], ...],
        *,
        disposition: PresentDisposition = PresentDisposition.COMMIT,
    ):
        result = core.feed_machine(
            _present_frames(
                encoder,
                transaction_id=transaction_id,
                base_revision=transaction_id - 1,
                retained_mode=mode,
                disposition=disposition,
                operations=operations,
            )
        )
        frames = []
        for outbound in result.outbound:
            frames.extend(decoder.feed(outbound.payload))
        assert TX_RESULT.unpack(frames[0].payload) == (
            transaction_id,
            0,
            0,
            transaction_id,
        )
        assert result.views == (core.output_view,)
        core.settle_result_delivery(transaction_id)
        return result

    commit(
        2,
        PresentRetainedMode.REPLACE_START,
        (
            (RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),
            (RetainedMessageType.SERIES_DEFINE, encode_series_definition(series)),
        ),
    )
    revealed = commit(
        3,
        PresentRetainedMode.REPLACE_CONTINUE,
        (
            (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(group)),
            (RetainedMessageType.OBJECT_DEFINE, encode_object_definition(readout)),
        ),
        disposition=PresentDisposition.COMMIT_AND_REVEAL,
    )
    revealed_scene = revealed.views[0].retained.active

    replacement_region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, -4, 0x2)
    commit(
        4,
        PresentRetainedMode.DELTA,
        (
            (
                RetainedMessageType.REGION_REPLACE,
                encode_region_replace(replacement_region),
            ),
        ),
    )

    replacement_readout = ObjectWireDefinition(
        owner_id=7,
        owner_generation=1,
        object_id=2,
        region_id=1,
        parent_object_id=1,
        bounds=bounds,
        z_order=2,
        visible=True,
        body=ReadoutBody(
            RGBA(32, 220, 96, 255),
            RGBA(0, 0, 0, 255),
            ReadoutFormat.FIXED,
            1,
            20,
            10,
            " dB",
        ),
    )
    commit(
        5,
        PresentRetainedMode.DELTA,
        (
            (
                RetainedMessageType.OBJECT_REPLACE,
                encode_object_replace(replacement_readout),
            ),
            (
                RetainedMessageType.OBJECT_SET_VALUE,
                encode_object_set_value(ObjectSetValue(7, 1, 2, -33)),
            ),
            (
                RetainedMessageType.OBJECT_SET_VISIBILITY,
                encode_object_set_visibility(ObjectSetVisibility(7, 1, 2, False)),
            ),
        ),
    )

    commit(
        6,
        PresentRetainedMode.DELTA,
        (
            (
                RetainedMessageType.SERIES_APPEND,
                encode_series_append(
                    SeriesWireSamples(
                        7,
                        1,
                        1,
                        ExplicitSamples((Sample(10, 1), Sample(20, 2))),
                    )
                ),
            ),
            (
                RetainedMessageType.SERIES_REPLACE,
                encode_series_replace(
                    SeriesWireSamples(
                        7,
                        1,
                        1,
                        ExplicitSamples((Sample(100, -1), Sample(200, -2))),
                    )
                ),
            ),
        ),
    )

    state = core.retained_state
    assert state is not None
    current = state.active.owners[7]
    assert current.regions[1].z_order == -4
    assert not current.regions[1].visible
    assert current.objects[2].body.value == -33
    assert not current.objects[2].visible
    assert current.series[1].samples == (Sample(100, -1), Sample(200, -2))
    assert revealed_scene.owners[7].regions[1].z_order == 0
    assert revealed_scene.owners[7].objects[2].body.value == -10
    assert not revealed_scene.owners[7].series[1].samples

    commit(
        7,
        PresentRetainedMode.DELTA,
        (
            (
                RetainedMessageType.OBJECT_DROP,
                encode_object_drop(RetainedItemReference(7, 1, 1)),
            ),
            (
                RetainedMessageType.OBJECT_DROP,
                encode_object_drop(RetainedItemReference(7, 1, 2)),
            ),
            (
                RetainedMessageType.SERIES_DROP,
                encode_series_drop(RetainedItemReference(7, 1, 1)),
            ),
            (
                RetainedMessageType.REGION_DROP,
                encode_region_drop(RetainedItemReference(7, 1, 1)),
            ),
        ),
    )

    state = core.retained_state
    assert state is not None
    emptied = state.active.owners[7]
    assert not emptied.regions and not emptied.objects and not emptied.series
    assert core.owner_state is not None
    high_water = core.owner_state.records[7].high_water
    assert (high_water.region, high_water.object, high_water.series) == (1, 2, 1)


def test_unadvertised_image_rejection_stays_sticky_while_present_drains():
    core, encoder, decoder = _open_retained_core(
        retained_policy=_soundlab_policy()
    )
    owner = _owner_open(objects=1)
    opened = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(owner))
    )
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)
    _settle_lifecycle(core, opened)
    group = ObjectWireDefinition(
        7,
        1,
        1,
        1,
        0,
        ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF),
        0,
        True,
        GroupBody(),
    )
    image_payload = bytearray(encode_object_definition(group))
    struct.pack_into("<H", image_payload, 24, 3)
    source = core.retained_state
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)

    rejected = core.feed_machine(
        _present_frames(
            encoder,
            transaction_id=2,
            base_revision=1,
            retained_mode=PresentRetainedMode.REPLACE_START,
            disposition=PresentDisposition.COMMIT,
            operations=(
                (RetainedMessageType.OBJECT_DEFINE, bytes(image_payload)),
                (
                    RetainedMessageType.REGION_DEFINE,
                    encode_region_definition(region),
                ),
            ),
        )
    )

    assert TX_RESULT.unpack(decoder.feed(rejected.outbound[0].payload)[0].payload) == (
        2,
        2,
        0,
        1,
    )
    assert rejected.views == ()
    assert core.model_revision == 1
    assert core.retained_state is source
    assert core.owner_state is not None
    assert core.owner_state.records[7].high_water.region == 0


def test_resource_lifecycle_frame_cannot_intervene_inside_present():
    core, encoder, _decoder = _open_retained_core()

    with pytest.raises(TerminalSessionError):
        core.feed_machine(
            _present_frames(
                encoder,
                transaction_id=2,
                base_revision=1,
                retained_mode=PresentRetainedMode.REPLACE_START,
                disposition=PresentDisposition.COMMIT,
                operations=((RetainedMessageType.RESOURCE_BEGIN, bytes(24)),),
            )
        )

    assert core.model_revision == 1


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
    assert core.model_revision == 1
    assert _settle_results(core, crossed) == (2,)

    encoder.set_presentation_epoch(1)
    acknowledged = core.feed_machine(
        encoder.encode(
            MessageType.SOFT_RESET_ACK,
            SOFT_RESET_ACK.pack(1, 0, 0),
        )
    )
    assert acknowledged.outbound == ()
    assert core.model_revision == 0
    assert core.state is TerminalState.RESYNCING

    outbound_decoder.advance_presentation_epoch(1)
    replacement = core.feed_machine(_snapshot_frames(encoder, transaction_id=1))
    assert len(replacement.views) == 1
    assert replacement.views[0].presentation_epoch == 1
    assert replacement.views[0].revision == 1
    assert core.model_revision == 1
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
    core = RichTerminalCore(
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


def test_client_close_discards_an_open_transaction_without_a_result():
    core, offer, request, encoder, client_ready = _negotiate(client_credit=512)
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    _settle_results(core, opened)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=256)
    for outbound in opened.outbound:
        decoder.feed(outbound.payload)

    closed = core.feed_machine(
        encoder.encode(
            MessageType.TX_BEGIN,
            BEGIN.pack(2, 1, 2, 2, 0, 0),
        )
        + encoder.encode(
            MessageType.CLOSE,
            CLOSE.pack(7, bytes(6), 1),
        )
    )

    frames = []
    for outbound in closed.outbound:
        frames.extend(decoder.feed(outbound.payload))
    assert [frame.message_type for frame in frames] == [MessageType.CLOSE_ACK]
    assert CLOSE_ACK.unpack(frames[0].payload) == (7, bytes(6))
    assert core.state is TerminalState.ANSI
    assert core.outstanding_result_transaction_id is None


def test_client_close_refuses_an_already_emitted_transaction_result():
    core, _offer, request, encoder, client_ready = _negotiate(client_credit=512)
    core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )

    with pytest.raises(TerminalSessionError, match="unsettled emitted result"):
        core.feed_machine(
            encoder.encode(
                MessageType.CLOSE,
                CLOSE.pack(7, bytes(6), 1),
            )
        )


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
