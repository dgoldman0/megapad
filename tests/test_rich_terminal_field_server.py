"""FDC1 ingress and revision-bound field intents through the binary server."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_server as wire_helpers
from tests import test_rich_terminal_taskbar_server as taskbar_helpers
from rich_terminal.apt1 import (
    FrameEncoder, IncrementalFrameDecoder, MessageType, Offer, OpenRequest,
    encode_open, encode_probe, parse_negotiation,
)
from rich_terminal.retained_model import OwnerQuotas, RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEventKind, ControlWireDefinition, OwnerOpen, PresentDisposition,
    PresentRetainedMode, RetainedMessageType, decode_control_event, decode_ret_caps,
    encode_control_definition, encode_owner_open, encode_ret_query,
)
from rich_terminal.semantic_fields import FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect
from rich_terminal.server import RichTerminalCore, TerminalSessionError


VISIBLE_ENABLED = ControlState.VISIBLE | ControlState.ENABLED
_present = taskbar_helpers._present


def _content(kind=FieldKind.INTEGER, *, readonly=False, revision=7):
    numeric = dict(value=441, minimum=40, maximum=2000, step=10)
    if kind is FieldKind.CHOICE:
        numeric = dict(value=0, choices=(FieldChoice(-7, "Sine"), FieldChoice(0, "Pulse")))
    elif kind is FieldKind.TEXT:
        numeric = dict(text="e\u0301 茶")
    return FieldContent(
        content_revision=revision, kind=kind,
        flags=FieldFlag.READ_ONLY if readonly else FieldFlag(0),
        label_bounds=FieldRect(0, 0, 1, 1), value_bounds=FieldRect(1, 0, 1, 1),
        **numeric,
    )


def _definition(content=None, *, state=VISIBLE_ENABLED, label="Hz"):
    return ControlWireDefinition(
        7, 1, 1, ControlKind.FIELD, state, 0, 1, 0, 0,
        ObjectBounds(0, 1, 2, 1), label, "", content or _content(),
    )


def _open_core(*, fields=True, objects=8, utf8_bytes=128):
    features = RetainedFeature.CORE | RetainedFeature.CONTROLS
    if fields:
        features |= RetainedFeature.FIELDS
    core = RichTerminalCore(
        wire_helpers._config(), attachment_epoch=9,
        retained_policy=replace(wire_helpers._policy(), features=features),
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
    assert not features & RetainedFeature.CONTROL_COLLECTIONS
    request = OwnerOpen(7, 1, OwnerQuotas(2, 0, objects, 0, 0, utf8_bytes, 0))
    opened = core.feed_machine(encoder.encode(RetainedMessageType.OWNER_OPEN, encode_owner_open(request)))
    wire_helpers._consume(decoder, opened)
    marker = next(outbound.lifecycle_result for outbound in opened.outbound
                  if outbound.lifecycle_result is not None)
    core.settle_lifecycle_result_delivery(marker)
    return core, encoder, decoder


def _visible_core(content=None, *, state=VISIBLE_ENABLED):
    core, encoder, decoder = _open_core()
    _present(core, encoder, decoder, 2,
             taskbar_helpers._initial_operations((_definition(content, state=state),)),
             mode=PresentRetainedMode.REPLACE_START)
    _present(core, encoder, decoder, 3, mode=PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


@pytest.mark.parametrize("kind", list(FieldKind))
def test_writable_fields_activate_guest_owned_editor_without_mutating_value(kind):
    core, _, decoder = _visible_core(_content(kind))
    before = core.retained_state
    outbound = core.send_control_event(7, 1, 1, model_revision=3)
    frames = decoder.feed(outbound.payload)
    assert len(frames) == 1 and frames[0].message_type == MessageType.CONTROL_EVENT
    event = decode_control_event(frames[0].payload)
    assert (event.event_kind, event.control_id, event.model_revision) == (
        ControlEventKind.ACTIVATE, 1, 3,
    )
    assert len(frames[0].payload) == 40 and event.content_revision == 0
    assert core.retained_state is before
    assert before.active.owners[7].controls[1].content == _content(kind)


@pytest.mark.parametrize("kind,adjustment", [
    (FieldKind.INTEGER, 1), (FieldKind.INTEGER, -(1 << 63)),
    (FieldKind.CHOICE, -1), (FieldKind.CHOICE, (1 << 63) - 1),
])
def test_adjust_has_exact_revision_and_signed_tail_without_applying_value(kind, adjustment):
    core, _, decoder = _visible_core(_content(kind))
    before, output = core.retained_state, core.output_view
    outbound = core.send_control_event(
        7, 1, 1, model_revision=3, event_kind=ControlEventKind.ADJUST,
        content_revision=7, adjustment=adjustment, modifiers=5,
    )
    frames = decoder.feed(outbound.payload)
    assert len(frames) == 1 and frames[0].message_type == MessageType.CONTROL_EVENT
    payload = frames[0].payload
    assert len(payload) == 56
    assert payload[40:] == struct.pack("<Qq", 7, adjustment)
    event = decode_control_event(payload)
    assert (event.event_kind, event.content_revision, event.adjustment, event.modifiers) == (
        ControlEventKind.ADJUST, 7, adjustment, 5,
    )
    assert core.retained_state is before and core.output_view is output
    assert before.active.owners[7].controls[1].content.value == _content(kind).value


@pytest.mark.parametrize("kind,readonly,state,action", [
    (FieldKind.INTEGER, True, VISIBLE_ENABLED, "activate"),
    (FieldKind.TEXT, True, VISIBLE_ENABLED, "activate"),
    (FieldKind.CHOICE, True, VISIBLE_ENABLED, "adjust"),
    (FieldKind.INTEGER, True, VISIBLE_ENABLED, "adjust"),
    (FieldKind.TEXT, False, VISIBLE_ENABLED, "adjust"),
    (FieldKind.INTEGER, False, ControlState.VISIBLE, "adjust"),
    (FieldKind.INTEGER, False, ControlState(0), "adjust"),
    (FieldKind.INTEGER, False, ControlState.VISIBLE, "activate"),
    (FieldKind.INTEGER, False, ControlState(0), "activate"),
])
def test_field_authority_rejects_readonly_unavailable_and_text_adjustment(kind, readonly, state, action):
    core, _, _ = _visible_core(_content(kind, readonly=readonly), state=state)
    before = core.retained_state
    tail = {} if action == "activate" else dict(
        event_kind=ControlEventKind.ADJUST, content_revision=7, adjustment=1,
    )
    with pytest.raises(TerminalSessionError):
        core.send_control_event(7, 1, 1, model_revision=3, **tail)
    assert core.retained_state is before


@pytest.mark.parametrize("changes", [
    {"owner_generation": 2}, {"model_revision": 2}, {"model_revision": 4},
    {"content_revision": 6}, {"content_revision": 8}, {"control_id": 2},
])
def test_adjust_rejects_stale_model_content_owner_and_missing_target(changes):
    core, _, _ = _visible_core()
    before = core.retained_state
    arguments = dict(owner_id=7, owner_generation=1, control_id=1, model_revision=3,
                     event_kind=ControlEventKind.ADJUST, content_revision=7, adjustment=1)
    with pytest.raises(TerminalSessionError):
        core.send_control_event(**(arguments | changes))
    assert core.retained_state is before


@pytest.mark.parametrize("fault", ["reserved", "trailing", "geometry"])
def test_malformed_field_delta_preserves_complete_scene_and_valid_retry(fault):
    core, encoder, decoder = _visible_core()
    before, output = core.retained_state, core.output_view
    raw = bytearray(encode_control_definition(_definition()))
    offset = 80 + len("Hz".encode())
    assert raw[offset:offset + 4] == b"FDC1"
    if fault == "reserved":
        struct.pack_into("<I", raw, offset + 20, 1)
    elif fault == "trailing":
        raw += b"\0"
    else:
        # Value slot moves onto the label while the enclosing rectangle stays fixed.
        struct.pack_into("<i", raw, offset + 40, 0)
    staged = _definition(replace(_content(), content_revision=8, value=442))
    _present(core, encoder, decoder, 4, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(staged)),
        (RetainedMessageType.CONTROL_REPLACE, raw),
    ), expected_status=2)
    assert core.retained_state is before and core.output_view is output
    _present(core, encoder, decoder, 5, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(staged)),
    ))
    assert core.retained_state.active.owners[7].controls[1].content.value == 442
    assert core.model_revision == 4


@pytest.mark.parametrize("objects,utf8_bytes", [(2, 128), (8, 10)])
def test_field_choices_reserve_each_object_and_every_utf8_label(objects, utf8_bytes):
    core, encoder, decoder = _open_core(objects=objects, utf8_bytes=utf8_bytes)
    before = core.retained_state
    # One root plus two choices uses three slots; Hz + Sine + Pulse uses11bytes.
    _present(core, encoder, decoder, 2,
             taskbar_helpers._initial_operations((_definition(_content(FieldKind.CHOICE)),)),
             mode=PresentRetainedMode.REPLACE_START, expected_status=2)
    assert core.retained_state is before and core.model_revision == 1


def test_old_controls_profile_rejects_field_and_retains_complete_cell_fallback():
    core, encoder, decoder = _open_core(fields=False)
    before, fallback = core.retained_state, core.view
    _present(core, encoder, decoder, 2,
             taskbar_helpers._initial_operations((_definition(),)),
             mode=PresentRetainedMode.REPLACE_START, expected_status=2)
    assert core.retained_state is before and core.view.cells == fallback.cells
    assert core.model_revision == 1 and core.active
