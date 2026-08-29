"""Focused semantic-control PRESENT-ingress tests."""

from __future__ import annotations

from dataclasses import replace
import struct

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
from rich_terminal.retained_model import OwnerQuotas, RetainedFeature, RetainedPolicy
from rich_terminal.retained_scene import (
    ControlKind,
    ControlState,
    ObjectBounds,
)
from rich_terminal.retained_wire import (
    CellMode,
    ControlWireDefinition,
    OwnerOpen,
    PresentBegin,
    PresentCommit,
    PresentDisposition,
    PresentRetainedMode,
    RegionWireDefinition,
    RetainedItemReference,
    RetainedMessageType,
    encode_control_definition,
    encode_control_drop,
    encode_control_replace,
    encode_owner_open,
    encode_present_begin,
    encode_present_commit,
    encode_region_definition,
    encode_ret_query,
)
from rich_terminal.server import RichTerminalCore, TerminalConfig


_READY = struct.Struct("<IIIIIIQ")
_BEGIN = struct.Struct("<QQIIII")
_SPAN = struct.Struct("<III")
_CELL = struct.Struct("<IBBH")
_CURSOR = struct.Struct("<IIB7x")
_COMMIT = struct.Struct("<Q")
_TX_RESULT = struct.Struct("<QHHQ")


def _config() -> TerminalConfig:
    return TerminalConfig(
        max_payload=512,
        max_transaction_bytes=4_096,
        terminal_receive_credit=8_192,
        max_cells=4,
        max_feed_bytes=16_384,
        max_cols=4,
        max_rows=2,
        cols=2,
        rows=2,
    )


def _policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE | RetainedFeature.CONTROLS,
        max_owner_records=4,
        max_live_owners=2,
        max_regions=8,
        max_resources=0,
        max_objects=8,
        max_series=0,
        max_operations_per_transaction=12,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=4_096,
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
        total_utf8_bytes=256,
        client_to_terminal_max_payload=512,
        terminal_to_client_max_payload=512,
        base_max_transaction_bytes=4_096,
    )


def _snapshot_frames(encoder: FrameEncoder) -> bytes:
    cells = tuple(_CELL.pack(ord(" "), 7, 0, 0) for _ in range(4))
    spans = (
        _SPAN.pack(0, 0, 2) + b"".join(cells[:2]),
        _SPAN.pack(1, 0, 2) + b"".join(cells[2:]),
    )
    return b"".join(
        (
            encoder.encode(
                MessageType.SNAPSHOT_BEGIN,
                _BEGIN.pack(1, 0, 2, 2, 2, 4),
            ),
            *(encoder.encode(MessageType.CELL_SPAN, span) for span in spans),
            encoder.encode(MessageType.CURSOR, _CURSOR.pack(0, 0, 0)),
            encoder.encode(MessageType.SNAPSHOT_COMMIT, _COMMIT.pack(1)),
        )
    )


def _consume(decoder: IncrementalFrameDecoder, result):
    frames = []
    for outbound in result.outbound:
        frames.extend(decoder.feed(outbound.payload))
    return tuple(frames)


def _open_core():
    core = RichTerminalCore(
        _config(),
        attachment_epoch=9,
        retained_policy=_policy(),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    offered = core.feed_machine(encode_probe(nonce))
    offer = parse_negotiation(offered.outbound[0].payload)
    assert isinstance(offer, Offer)
    request = OpenRequest(nonce, offer.session_id, 512, 8_192)
    encoder = FrameEncoder(offer.session_id, max_payload=512)
    client_ready = encoder.encode(
        MessageType.CLIENT_READY,
        _READY.pack(0, 512, 0, 8_192, 256, 0, 0x3F),
    )
    opened = core.feed_machine(
        encode_open(request) + client_ready + _snapshot_frames(encoder)
    )
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=512)
    _consume(decoder, opened)
    core.settle_result_delivery(1)

    discovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    _consume(decoder, discovered)
    assert core.retained_enabled
    return core, encoder, decoder


def _open_owner(core: RichTerminalCore, encoder: FrameEncoder, decoder) -> None:
    request = OwnerOpen(
        7,
        1,
        OwnerQuotas(
            regions=2,
            resources=0,
            objects=8,
            series=0,
            resource_bytes=0,
            utf8_bytes=128,
            sample_slots=0,
        ),
    )
    result = core.feed_machine(
        encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(request))
    )
    _consume(decoder, result)
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
    operations=(),
) -> bytes:
    operations = tuple(operations)
    declared_bytes = 104 + sum(40 + len(payload) for _, payload in operations) + 56
    begin = PresentBegin(
        transaction_id=transaction_id,
        base_revision=base_revision,
        geometry_generation=0,
        declared_transaction_bytes=declared_bytes,
        cols=2,
        rows=2,
        cell_span_count=0,
        cell_count=0,
        retained_operation_count=len(operations),
        cell_mode=CellMode.NONE,
        retained_mode=retained_mode,
    )
    return b"".join(
        (
            encoder.encode(
                RetainedMessageType.PRESENT_BEGIN,
                encode_present_begin(begin),
            ),
            *(encoder.encode(message_type, payload) for message_type, payload in operations),
            encoder.encode(
                RetainedMessageType.PRESENT_COMMIT,
                encode_present_commit(PresentCommit(transaction_id, disposition)),
            ),
        )
    )


def _commit(
    core: RichTerminalCore,
    encoder: FrameEncoder,
    decoder: IncrementalFrameDecoder,
    *,
    transaction_id: int,
    mode: PresentRetainedMode,
    disposition: PresentDisposition = PresentDisposition.COMMIT,
    operations=(),
) -> None:
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
    frames = _consume(decoder, result)
    assert _TX_RESULT.unpack(frames[0].payload) == (
        transaction_id,
        0,
        0,
        transaction_id,
    )
    core.settle_result_delivery(transaction_id)


def _control(
    control_id: int,
    kind: ControlKind,
    state: ControlState,
    *,
    parent: int = 0,
    order: int = 0,
    label: str = "",
    shortcut: str = "",
) -> ControlWireDefinition:
    return ControlWireDefinition(
        owner_id=7,
        owner_generation=1,
        control_id=control_id,
        kind=kind,
        state=state,
        z_order=20 if kind is ControlKind.MENU_BAR else 0,
        region_id=1,
        parent_control_id=parent,
        order=order,
        bounds=(
            ObjectBounds(0, 0, 0xFFFFFFFF, 0x18000000)
            if kind is ControlKind.MENU_BAR
            else None
        ),
        label=label,
        shortcut=shortcut,
    )


def _visible_control_core():
    core, encoder, decoder = _open_core()
    _open_owner(core, encoder, decoder)
    visible_enabled = ControlState.VISIBLE | ControlState.ENABLED
    definitions = (
        _control(1, ControlKind.MENU_BAR, visible_enabled),
        _control(
            2,
            ControlKind.MENU,
            visible_enabled | ControlState.OPEN,
            parent=1,
            label="File",
        ),
        _control(
            3,
            ControlKind.MENU_ITEM,
            visible_enabled,
            parent=2,
            label="Open",
            shortcut="Ctrl+O",
        ),
        _control(
            4,
            ControlKind.MENU_SEPARATOR,
            ControlState.VISIBLE,
            parent=2,
            order=1,
        ),
    )
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0x3)
    _commit(
        core,
        encoder,
        decoder,
        transaction_id=2,
        mode=PresentRetainedMode.REPLACE_START,
        operations=(
            (RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),
            *(
                (RetainedMessageType.CONTROL_DEFINE, encode_control_definition(item))
                for item in definitions
            ),
        ),
    )
    _commit(
        core,
        encoder,
        decoder,
        transaction_id=3,
        mode=PresentRetainedMode.REPLACE_CONTINUE,
        disposition=PresentDisposition.COMMIT_AND_REVEAL,
    )
    return core, encoder, decoder


def test_present_ingress_defines_updates_state_and_drops_semantic_controls() -> None:
    core, encoder, decoder = _visible_control_core()
    state = core.retained_state
    assert state is not None
    controls = state.active.owners[7].controls
    assert set(controls) == {1, 2, 3, 4}
    assert controls[3].label == "Open"
    assert controls[3].shortcut == "Ctrl+O"

    replacement = _control(
        3,
        ControlKind.MENU_ITEM,
        ControlState.VISIBLE | ControlState.ENABLED | ControlState.CHECKED,
        parent=2,
        label="Open",
        shortcut="Ctrl+O",
    )
    _commit(
        core,
        encoder,
        decoder,
        transaction_id=4,
        mode=PresentRetainedMode.DELTA,
        operations=(
            (
                RetainedMessageType.CONTROL_REPLACE,
                encode_control_replace(replacement),
            ),
            (
                RetainedMessageType.CONTROL_DROP,
                encode_control_drop(RetainedItemReference(7, 1, 4)),
            ),
        ),
    )

    state = core.retained_state
    assert state is not None
    controls = state.active.owners[7].controls
    assert controls[3].label == "Open"
    assert controls[3].shortcut == "Ctrl+O"
    assert controls[3].state & ControlState.CHECKED
    assert 4 not in controls
    assert core.owner_state is not None
    assert core.owner_state.records[7].high_water.control == 4


def test_present_ingress_restarts_live_hidden_replacement_before_reveal() -> None:
    core, encoder, decoder = _visible_control_core()
    initial = core.retained_state
    assert initial is not None
    active = initial.active
    visible_enabled = ControlState.VISIBLE | ControlState.ENABLED

    older_region = RegionWireDefinition(7, 1, 2, 0, 0, 2, 2, 0, 0x3)
    older_bar = replace(
        _control(5, ControlKind.MENU_BAR, visible_enabled),
        region_id=2,
    )
    _commit(
        core,
        encoder,
        decoder,
        transaction_id=4,
        mode=PresentRetainedMode.REPLACE_START,
        operations=(
            (RetainedMessageType.REGION_DEFINE, encode_region_definition(older_region)),
            (RetainedMessageType.CONTROL_DEFINE, encode_control_definition(older_bar)),
        ),
    )

    state = core.retained_state
    assert state is not None and state.hidden is not None
    older_hidden = state.hidden
    assert state.active is active
    assert set(older_hidden.owners[7].controls) == {5}

    newest_region = RegionWireDefinition(7, 1, 3, 0, 0, 2, 2, 0, 0x3)
    newest_bar = replace(
        _control(6, ControlKind.MENU_BAR, visible_enabled),
        region_id=3,
    )
    _commit(
        core,
        encoder,
        decoder,
        transaction_id=5,
        mode=PresentRetainedMode.REPLACE_START,
        operations=(
            (RetainedMessageType.REGION_DEFINE, encode_region_definition(newest_region)),
            (RetainedMessageType.CONTROL_DEFINE, encode_control_definition(newest_bar)),
        ),
    )

    state = core.retained_state
    assert state is not None and state.hidden is not None
    newest_hidden = state.hidden
    assert state.active is active
    assert newest_hidden is not older_hidden
    assert set(newest_hidden.owners[7].controls) == {6}
    assert core.owner_state is not None
    assert core.owner_state.records[7].high_water.region == 3
    assert core.owner_state.records[7].high_water.control == 6

    _commit(
        core,
        encoder,
        decoder,
        transaction_id=6,
        mode=PresentRetainedMode.REPLACE_CONTINUE,
        disposition=PresentDisposition.COMMIT_AND_REVEAL,
    )

    state = core.retained_state
    assert state is not None
    assert state.active is newest_hidden
    assert state.hidden is None
    assert set(state.active.owners[7].controls) == {6}
