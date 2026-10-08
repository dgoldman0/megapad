"""Negotiated grid value roles preserve existing whole-cell input authority."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_server as wire_helpers
from tests import test_rich_terminal_taskbar_server as taskbar_helpers
from rich_terminal.apt1 import (
    FrameEncoder, IncrementalFrameDecoder, MessageType, Offer, OpenRequest,
    encode_open, encode_probe, parse_negotiation,
)
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEventKind, ControlWireDefinition, PresentDisposition, PresentRetainedMode,
    RetainedMessageType, decode_control_event, decode_ret_caps, encode_control_definition,
    encode_ret_query,
)
from rich_terminal.semantic_content import (
    SemanticContentFlag, SemanticTextContent, SemanticTextItem, SemanticTextRole,
    SemanticTextState,
)
from rich_terminal.server import RichTerminalCore, TerminalSessionError


_present = taskbar_helpers._present


def _content(*, typed=True, unavailable=False, revision=7):
    roles = ((SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR)
             if typed else (SemanticTextRole.CONTENT,) * 3)
    items = (
        SemanticTextItem(11, 0, 0, 1, 1, roles[0],
                         SemanticTextState.UNAVAILABLE if unavailable else SemanticTextState(0), "7"),
        SemanticTextItem(12, 0, 1, 1, 1, roles[1], SemanticTextState(0), "42"),
        SemanticTextItem(13, 1, 0, 1, 1, roles[2], SemanticTextState(0), "#ERR"),
        SemanticTextItem(14, 1, 1, 1, 1, SemanticTextRole.COLUMN_HEADER, SemanticTextState(0), "A"),
    )
    return SemanticTextContent(revision, 2, 2, 0, 0, 2, 2, SemanticContentFlag(0),
                               0, 0, 0, 0, items)


def _definition(content=None):
    return ControlWireDefinition(
        7, 1, 1, ControlKind.TEXT_GRID, ControlState.VISIBLE | ControlState.ENABLED,
        0, 1, 0, 0, ObjectBounds(0, 0, 2, 2), "", "", content or _content(),
    )


def _open_core(*, grid_cells=True):
    features = (RetainedFeature.CORE | RetainedFeature.CONTROLS
                | RetainedFeature.CONTROL_COLLECTIONS)
    if grid_cells:
        features |= RetainedFeature.GRID_CELLS
    core = RichTerminalCore(
        wire_helpers._config(), attachment_epoch=9,
        retained_policy=replace(wire_helpers._policy(control_collections=True), features=features),
        session_id_factory=lambda: 0x0123456789ABCDEF,
    )
    nonce = 0xFEDCBA9876543210
    offer = parse_negotiation(core.feed_machine(encode_probe(nonce)).outbound[0].payload)
    assert isinstance(offer, Offer)
    encoder = FrameEncoder(offer.session_id, max_payload=512)
    decoder = IncrementalFrameDecoder(offer.session_id, max_payload=512)
    ready = encoder.encode(MessageType.CLIENT_READY,
                           wire_helpers._READY.pack(0, 512, 0, 8192, 256, 0, 0x3F))
    wire_helpers._consume(decoder, core.feed_machine(
        encode_open(OpenRequest(nonce, offer.session_id, 512, 8192))
        + ready + wire_helpers._snapshot_frames(encoder)))
    core.settle_result_delivery(1)
    discovered = core.feed_machine(encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query()))
    caps = next(frame for frame in wire_helpers._consume(decoder, discovered)
                if frame.message_type == RetainedMessageType.RET_CAPS)
    assert decode_ret_caps(caps.payload).features == features
    wire_helpers._open_owner(core, encoder, decoder)
    return core, encoder, decoder


def _visible_core(content=None):
    core, encoder, decoder = _open_core()
    _present(core, encoder, decoder, 2,
             taskbar_helpers._initial_operations((_definition(content),)),
             mode=PresentRetainedMode.REPLACE_START)
    _present(core, encoder, decoder, 3, mode=PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


@pytest.mark.parametrize("item_key", [11, 12, 13])
def test_number_formula_and_error_accept_existing_whole_cell_place(item_key):
    core, _, decoder = _visible_core()
    before, output = core.retained_state, core.output_view
    outbound = core.send_control_event(
        7, 1, 1, model_revision=3, event_kind=ControlEventKind.PLACE,
        content_revision=7, item_key=item_key, scalar_offset=0,
    )
    frame, = decoder.feed(outbound.payload)
    assert frame.message_type == MessageType.CONTROL_EVENT
    assert len(frame.payload) == 64
    assert frame.payload[40:] == struct.pack("<QQII", 7, item_key, 0, 0)
    event = decode_control_event(frame.payload)
    assert event.event_kind is ControlEventKind.PLACE
    assert core.retained_state is before and core.output_view is output
    assert before.active.owners[7].controls[1].content.primary_key == 0


@pytest.mark.parametrize("content,changes", [
    (_content(), {"item_key": 14}),
    (_content(unavailable=True), {"item_key": 11}),
    (_content(), {"scalar_offset": 1}),
    (_content(), {"event_kind": ControlEventKind.EXTEND}),
    (_content(), {"content_revision": 6}),
    (_content(), {"content_revision": 8}),
    (_content(), {"model_revision": 2}),
    (_content(), {"owner_generation": 2}),
])
def test_typed_grid_positions_reject_headers_unavailable_cells_and_stale_or_partial_positions(content, changes):
    core, _, _ = _visible_core(content)
    before = core.retained_state
    arguments = dict(owner_id=7, owner_generation=1, control_id=1, model_revision=3,
                     event_kind=ControlEventKind.PLACE, content_revision=7,
                     item_key=11, scalar_offset=0)
    with pytest.raises(TerminalSessionError):
        core.send_control_event(**(arguments | changes))
    assert core.retained_state is before


def test_typed_grid_scroll_keeps_existing_detents_and_leaves_viewport_guest_owned():
    core, _, decoder = _visible_core()
    before = core.retained_state
    outbound = core.send_control_event(
        7, 1, 1, model_revision=3, event_kind=ControlEventKind.SCROLL, wheel_x=-1, wheel_y=2,
    )
    frame, = decoder.feed(outbound.payload)
    assert len(frame.payload) == 48
    event = decode_control_event(frame.payload)
    assert (event.event_kind, event.wheel_x, event.wheel_y, event.content_revision) == (
        ControlEventKind.SCROLL, -1, 2, 0,
    )
    assert core.retained_state is before


def test_old_collection_profile_rejects_typed_roles_atomically_and_plain_content_retry_works():
    core, encoder, decoder = _open_core(grid_cells=False)
    before, fallback = core.retained_state, core.view
    _present(core, encoder, decoder, 2,
             taskbar_helpers._initial_operations((_definition(),)),
             mode=PresentRetainedMode.REPLACE_START, expected_status=2)
    assert core.retained_state is before and core.view.cells == fallback.cells
    assert core.model_revision == 1

    plain = _definition(_content(typed=False))
    _present(core, encoder, decoder, 3, taskbar_helpers._initial_operations((plain,)),
             mode=PresentRetainedMode.REPLACE_START)
    _present(core, encoder, decoder, 4, mode=PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    committed = core.retained_state
    assert committed.active.owners[7].controls[1].content == plain.content
    _present(core, encoder, decoder, 5, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(_definition())),
    ), expected_status=2)
    assert core.retained_state is committed and core.model_revision == 3


@pytest.mark.parametrize("role", [SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR])
def test_typed_grid_roles_cannot_enter_text_area_even_when_full_row_geometry_fits(role):
    core, encoder, decoder = _open_core()
    content = SemanticTextContent(7, 1, 2, 0, 0, 1, 2, SemanticContentFlag(0), 0, 0, 0, 0,
                                  (SemanticTextItem(11, 0, 0, 1, 2, role, SemanticTextState(0), "7"),))
    raw = bytearray(encode_control_definition(_definition(content)))
    struct.pack_into("<H", raw, 24, int(ControlKind.TEXT_AREA))
    region_operation = taskbar_helpers._initial_operations(())[0]
    before = core.retained_state
    _present(core, encoder, decoder, 2, (
        region_operation, (RetainedMessageType.CONTROL_DEFINE, raw),
    ), mode=PresentRetainedMode.REPLACE_START, expected_status=2)
    assert core.retained_state is before and core.model_revision == 1
