"""Rendered taskbar targets cross JSON/display authority before activation."""

import json

import pytest

pygame = pytest.importorskip("pygame")

from tests import test_rich_terminal_taskbar_server as taskbar_helpers
from tests import test_rich_terminal_semantic_viewer_input as viewer_helpers
from emulator.shared_session import SharedMachine
from rich_terminal import DriverStatus
from rich_terminal.pygame_view import composite_draw_plane_result
from rich_terminal.retained_scene import ControlKind, ControlState
from rich_terminal.retained_view import project_composite_draw_plane
from rich_terminal.retained_wire import (
    ControlEventKind, RetainedMessageType, decode_control_event,
    encode_control_definition,
)
from rich_terminal.server import TerminalSessionError
from session_viewer import _GuestKeyboardForwarder, _PointerRouter, _RetainedDisplayState
from shared.session import OutputSnapshotRows, TerminalDisplayOffer
from shared_session import SessionServer, display_offer_from_wire, display_offer_to_wire


GENERATION = 4
CONNECTION = 9


class _CoreSession:
    """Keep real shared RPC and terminal validation, without starting a CPU."""

    retained_display_required = True
    rich_terminal_failure = None
    last_acknowledged_display_offer = None

    def __init__(self, core, decoder):
        self.core = core
        self.decoder = decoder
        self.backpressured = False
        self.events = []

    def send_control_event(self, owner_id, owner_generation, control_id, **fields):
        if self.backpressured:
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self.core.send_control_event(
                owner_id, owner_generation, control_id,
                model_revision=self.last_acknowledged_display_offer[1].model_revision,
                **fields,
            )
        except TerminalSessionError:
            return DriverStatus.INVALID
        if outbound is None:
            return DriverStatus.BACKPRESSURED
        self.events.extend(decode_control_event(frame.payload)
                           for frame in self.decoder.feed(outbound.payload))
        return DriverStatus.PROGRESS


class _JsonClient:
    def __init__(self, server):
        self.server = server
        self.requests = []

    def request(self, method, **params):
        # Exercise exact JSON-shaped RPC requests as an external viewer sends.
        request = json.loads(json.dumps(params))
        self.requests.append((method, request))
        return self.server.dispatch(method, request, connection_id=CONNECTION)


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _offer_and_hits(core, font, offer_id=1):
    scope, plane = project_composite_draw_plane(core.output_view)
    original = TerminalDisplayOffer(
        offer_id, scope, OutputSnapshotRows().snapshot(core.output_view.cell), plane,
    )
    offer = display_offer_from_wire(json.loads(json.dumps(display_offer_to_wire(original))))
    assert offer == original
    result = composite_draw_plane_result(
        pygame, pygame.Surface((80, 40)), offer.retained, font, 40, 20,
    )
    return offer, result


def _environment(font, **states):
    core, encoder, decoder = taskbar_helpers._visible_core(**states)
    session = _CoreSession(core, decoder)
    machine = SharedMachine(session)
    machine._reset_generation = GENERATION
    server = SessionServer(machine, "/tmp/unused-taskbar-input.sock")
    server._display_holder = CONNECTION
    client = _JsonClient(server)
    keyboard = _GuestKeyboardForwarder(
        viewer_helpers._Pygame(), client, generation=GENERATION, display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=40, cell_height=20)
    offer, result = _offer_and_hits(core, font)
    return core, encoder, decoder, session, server, client, keyboard, state, pointer, offer, result


def _ack(session, server, keyboard, state, offer, result):
    if state.pending_offer != offer:
        state.stage(offer, keyboard.generation)
    state.stage_frame_hit_map(offer, result.hit_entries)
    assert state.finish_presentation({
        "status": "presented", "presented": True,
        "revision": offer.scope.model_revision,
    }) == offer.scope.model_revision
    proof = (offer.offer_id, offer.scope)
    server._display_delivered = server._display_ack = proof
    session.last_acknowledged_display_offer = proof
    keyboard.acknowledge_display_offer(*proof)


@pytest.mark.parametrize("control_id,kind,point", [
    (2, ControlKind.TASK, (20, 30)),
    (3, ControlKind.LAUNCHER, (60, 30)),
])
def test_rendered_minimized_task_and_launcher_activate_only_after_sink_ack(
        font, control_id, kind, point):
    (core, _, _, session, server, client, keyboard, state, pointer,
     offer, result) = _environment(
        font, task_state=taskbar_helpers.VISIBLE_ENABLED | ControlState.MINIMIZED,
    )
    assert {(target.identity.control_id, target.kind) for target in result.hit_targets} == {
        (2, ControlKind.TASK), (3, ControlKind.LAUNCHER),
    }
    state.stage(offer, GENERATION)
    state.stage_frame_hit_map(offer, result.hit_entries)
    assert not pointer.button_down(1, point, (80, 40))
    assert not pointer.button_up(1, point, (80, 40))
    assert client.requests == session.events == []

    _ack(session, server, keyboard, state, offer, result)
    before = core.retained_state
    assert pointer.button_down(1, point, (80, 40))
    assert pointer.button_up(1, point, (80, 40), modifiers=5)
    assert len(session.events) == 1
    event = session.events[0]
    assert (event.control_id, event.event_kind, event.modifiers, event.model_revision) == (
        control_id, ControlEventKind.ACTIVATE, 5, 3,
    )
    method, params = client.requests[0]
    assert method == "send_control_event"
    assert set(params) == {
        "owner_id", "owner_generation", "control_id", "modifiers", "generation",
        "display_offer_id", "display_scope",
    }
    assert params["display_offer_id"] == offer.offer_id
    assert core.retained_state is before
    assert before.active.owners[7].controls[2].state & ControlState.MINIMIZED


def test_task_activation_backpressure_cannot_replay_on_replacement_display(font):
    (core, encoder, decoder, session, server, client, keyboard, state, pointer,
     offer, result) = _environment(font)
    _ack(session, server, keyboard, state, offer, result)
    session.backpressured = True
    assert pointer.button_down(1, (20, 30), (80, 40))
    assert pointer.button_up(1, (20, 30), (80, 40))
    assert keyboard.pending_events == 1
    assert len(client.requests) == 1 and session.events == []

    replacement = taskbar_helpers._definitions(task_state=(
        taskbar_helpers.VISIBLE_ENABLED | ControlState.MINIMIZED))[1]
    taskbar_helpers._present(core, encoder, decoder, 4, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(replacement)),
    ))
    new_offer, new_result = _offer_and_hits(core, font, offer_id=2)
    keyboard.begin_display_offer()
    _ack(session, server, keyboard, state, new_offer, new_result)
    session.backpressured = False
    keyboard.flush_pending()
    assert keyboard.pending_events == 0
    assert len(client.requests) == 1 and session.events == []

    assert pointer.button_down(1, (20, 30), (80, 40))
    assert pointer.button_up(1, (20, 30), (80, 40))
    assert len(session.events) == 1 and session.events[0].model_revision == 4


def test_press_on_previous_taskbar_offer_cannot_activate_after_new_sink_ack(font):
    (core, encoder, decoder, session, server, client, keyboard, state, pointer,
     offer, result) = _environment(font)
    _ack(session, server, keyboard, state, offer, result)
    assert pointer.button_down(1, (20, 30), (80, 40))
    replacement = taskbar_helpers._definitions(task_state=(
        taskbar_helpers.VISIBLE_ENABLED | ControlState.MINIMIZED))[1]
    taskbar_helpers._present(core, encoder, decoder, 4, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(replacement)),
    ))
    new_offer, new_result = _offer_and_hits(core, font, offer_id=2)
    keyboard.begin_display_offer()
    _ack(session, server, keyboard, state, new_offer, new_result)
    assert not pointer.button_up(1, (20, 30), (80, 40))
    assert client.requests == session.events == []


@pytest.mark.parametrize("states", [
    {"task_state": ControlState.VISIBLE},
    {"task_state": ControlState(0)},
    {"parent_state": ControlState.VISIBLE},
])
def test_unavailable_task_surface_has_no_activation_or_raw_pointer_fallthrough(font, states):
    (_, _, _, session, server, client, keyboard, state, pointer,
     offer, result) = _environment(font, **states)
    assert all(target.identity.control_id != 2 for target in result.hit_targets)
    _ack(session, server, keyboard, state, offer, result)
    assert not pointer.button_down(1, (20, 30), (80, 40))
    assert not pointer.button_up(1, (20, 30), (80, 40))
    assert client.requests == session.events == []
