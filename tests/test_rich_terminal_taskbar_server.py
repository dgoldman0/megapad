"""Taskbar publication and activation through the production binary server."""

from dataclasses import replace

import pytest

from tests import test_rich_terminal_semantic_server as wire_helpers
from rich_terminal.apt1 import (
    FrameEncoder, IncrementalFrameDecoder, MessageType, Offer, OpenRequest,
    encode_open, encode_probe, parse_negotiation,
)
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEventKind, ControlWireDefinition, PresentDisposition,
    PresentRetainedMode, RegionWireDefinition, RetainedItemReference,
    RetainedMessageType, decode_control_event, decode_ret_caps,
    encode_control_definition, encode_control_drop, encode_region_definition,
    encode_ret_query,
)
from rich_terminal.server import RichTerminalCore, TerminalSessionError


VISIBLE_ENABLED = ControlState.VISIBLE | ControlState.ENABLED


def _open_core(*, taskbars=True):
    features = RetainedFeature.CORE | RetainedFeature.CONTROLS
    if taskbars:
        features |= RetainedFeature.TASKBARS
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
    ready = encoder.encode(
        MessageType.CLIENT_READY,
        wire_helpers._READY.pack(0, 512, 0, 8192, 256, 0, 0x3F),
    )
    opened = core.feed_machine(
        encode_open(OpenRequest(nonce, offer.session_id, 512, 8192))
        + ready + wire_helpers._snapshot_frames(encoder)
    )
    wire_helpers._consume(decoder, opened)
    core.settle_result_delivery(1)
    discovered = core.feed_machine(
        encoder.encode(RetainedMessageType.RET_QUERY, encode_ret_query())
    )
    caps = next(frame for frame in wire_helpers._consume(decoder, discovered)
                if frame.message_type == RetainedMessageType.RET_CAPS)
    assert decode_ret_caps(caps.payload).features == features
    wire_helpers._open_owner(core, encoder, decoder)
    return core, encoder, decoder


def _definitions(*, task_state=VISIBLE_ENABLED, parent_state=VISIBLE_ENABLED,
                 launcher_state=VISIBLE_ENABLED):
    return (
        ControlWireDefinition(7, 1, 1, ControlKind.TASKBAR, parent_state, 0,
                              1, 0, 0, ObjectBounds(0, 1, 2, 1), "", ""),
        ControlWireDefinition(7, 1, 2, ControlKind.TASK, task_state, 0,
                              1, 1, 0, ObjectBounds(0, 0, 1, 1), "Pad", "1"),
        ControlWireDefinition(7, 1, 3, ControlKind.LAUNCHER, launcher_state, 0,
                              1, 1, 1, ObjectBounds(1, 0, 1, 1), "Open", ""),
    )


def _present(core, encoder, decoder, transaction_id, operations=(), *,
             mode=PresentRetainedMode.DELTA,
             disposition=PresentDisposition.COMMIT, expected_status=0):
    before = core.model_revision
    result = core.feed_machine(wire_helpers._present_frames(
        encoder, transaction_id=transaction_id, base_revision=before,
        retained_mode=mode, disposition=disposition, operations=operations,
    ))
    completion = next(frame for frame in wire_helpers._consume(decoder, result)
                      if frame.message_type == MessageType.TX_RESULT)
    assert wire_helpers._TX_RESULT.unpack(completion.payload) == (
        transaction_id, expected_status, 0, before + (expected_status == 0),
    )
    core.settle_result_delivery(transaction_id)


def _initial_operations(definitions):
    region = RegionWireDefinition(7, 1, 1, 0, 0, 2, 2, 0, 0, 2, 2, 0, 3)
    return (
        (RetainedMessageType.REGION_DEFINE, encode_region_definition(region)),
        *((RetainedMessageType.CONTROL_DEFINE, encode_control_definition(item))
          for item in definitions),
    )


def _visible_core(**states):
    core, encoder, decoder = _open_core()
    _present(core, encoder, decoder, 2, _initial_operations(_definitions(**states)),
             mode=PresentRetainedMode.REPLACE_START)
    _present(core, encoder, decoder, 3,
             mode=PresentRetainedMode.REPLACE_CONTINUE,
             disposition=PresentDisposition.COMMIT_AND_REVEAL)
    return core, encoder, decoder


@pytest.mark.parametrize("task_state", [VISIBLE_ENABLED,
                                         VISIBLE_ENABLED | ControlState.MINIMIZED,
                                         VISIBLE_ENABLED | ControlState.SELECTED])
def test_task_activation_is_revision_bound_intent_and_preserves_guest_state(task_state):
    core, _, decoder = _visible_core(task_state=task_state)
    before = core.retained_state
    output = core.output_view
    outbound = core.send_control_event(7, 1, 2, model_revision=3, modifiers=5)
    assert outbound is not None
    frames = decoder.feed(outbound.payload)
    assert len(frames) == 1 and frames[0].message_type == MessageType.CONTROL_EVENT
    event = decode_control_event(frames[0].payload)
    assert (event.owner_id, event.owner_generation, event.control_id,
            event.event_kind, event.modifiers, event.model_revision) == (
        7, 1, 2, ControlEventKind.ACTIVATE, 5, 3,
    )
    assert len(frames[0].payload) == 40
    assert core.retained_state is before
    assert core.output_view is output
    assert before.active.owners[7].controls[2].state == task_state


def test_launcher_uses_existing_activate_without_task_selection_side_effects():
    core, _, decoder = _visible_core(task_state=VISIBLE_ENABLED | ControlState.SELECTED)
    before = core.retained_state
    outbound = core.send_control_event(7, 1, 3, model_revision=3)
    event = decode_control_event(decoder.feed(outbound.payload)[0].payload)
    assert event.control_id == 3 and event.event_kind is ControlEventKind.ACTIVATE
    assert core.retained_state is before
    assert before.active.owners[7].controls[2].state & ControlState.SELECTED


@pytest.mark.parametrize("states,target", [
    ({"task_state": ControlState.VISIBLE}, 2),
    ({"task_state": ControlState(0)}, 2),
    ({"parent_state": ControlState.VISIBLE}, 2),
    ({"parent_state": ControlState(0)}, 2),
    ({"launcher_state": ControlState.VISIBLE}, 3),
    ({"launcher_state": ControlState(0)}, 3),
])
def test_disabled_or_hidden_child_or_parent_rejects_activation(states, target):
    core, _, _ = _visible_core(**states)
    before = core.retained_state
    with pytest.raises(TerminalSessionError, match="not interactable"):
        core.send_control_event(7, 1, target, model_revision=3)
    assert core.retained_state is before


@pytest.mark.parametrize("owner,generation,control,revision", [
    (7, 2, 2, 3), (8, 1, 2, 3), (7, 1, 2, 2), (7, 1, 2, 4), (7, 1, 1, 3),
])
def test_taskbar_activation_rejects_stale_identity_revision_and_root(
        owner, generation, control, revision):
    core, _, _ = _visible_core()
    before = core.retained_state
    with pytest.raises(TerminalSessionError):
        core.send_control_event(owner, generation, control, model_revision=revision)
    assert core.retained_state is before


def test_taskbar_parent_drop_requires_children_and_failed_delta_is_atomic():
    core, encoder, decoder = _visible_core()
    before = core.retained_state
    drop = lambda item: (RetainedMessageType.CONTROL_DROP,
                         encode_control_drop(RetainedItemReference(7, 1, item)))
    _present(core, encoder, decoder, 4, (drop(1),), expected_status=2)
    assert core.retained_state is before
    assert core.model_revision == 3
    _present(core, encoder, decoder, 5, (drop(2), drop(3), drop(1)))
    assert not core.retained_state.active.owners[7].controls
    with pytest.raises(TerminalSessionError, match="not interactable"):
        core.send_control_event(7, 1, 2, model_revision=4)


def test_old_control_policy_rejects_taskbar_without_disturbing_cell_fallback():
    core, encoder, decoder = _open_core(taskbars=False)
    before, fallback = core.retained_state, core.view
    _present(core, encoder, decoder, 2, _initial_operations(_definitions()),
             mode=PresentRetainedMode.REPLACE_START, expected_status=2)
    assert core.retained_state is before
    assert core.view.cells == fallback.cells
    assert core.model_revision == 1 and core.active
