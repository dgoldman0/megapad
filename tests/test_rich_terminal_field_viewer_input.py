"""Displayed FIELD geometry and revision proof govern semantic pointer input."""

from dataclasses import replace
from types import SimpleNamespace

import pytest

pygame = pytest.importorskip("pygame")

from tests import test_rich_terminal_field_server as field_helpers
from tests import test_rich_terminal_taskbar_viewer_input as taskbar_helpers
from tests import test_session_viewer as keyboard_helpers
from emulator.shared_session import SharedMachine
from rich_terminal import DriverStatus
from rich_terminal.pygame_view import FieldHitTarget
from rich_terminal.retained_scene import ControlState
from rich_terminal.retained_wire import ControlEventKind, RetainedMessageType, encode_control_definition
from rich_terminal.semantic_fields import FieldKind
from session_viewer import _GuestKeyboardForwarder, _PointerRouter, _RetainedDisplayState
from shared_session import SessionServer


class _FieldSession(taskbar_helpers._CoreSession):
    def __init__(self, core, decoder):
        super().__init__(core, decoder)
        self.keys = []

    def send_key(self, key):
        self.keys.append(key)
        return DriverStatus.PROGRESS


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _environment(font, content=None, *, state=field_helpers.VISIBLE_ENABLED):
    core, encoder, decoder = field_helpers._visible_core(content, state=state)
    session = _FieldSession(core, decoder)
    machine = SharedMachine(session)
    machine._reset_generation = taskbar_helpers.GENERATION
    server = SessionServer(machine, "/tmp/unused-field-input.sock")
    server._display_holder = taskbar_helpers.CONNECTION
    client = taskbar_helpers._JsonClient(server)
    keyboard = _GuestKeyboardForwarder(
        keyboard_helpers._FakePygame(), client,
        generation=taskbar_helpers.GENERATION, display_required=True,
    )
    display = _RetainedDisplayState()
    pointer = _PointerRouter(display, keyboard, cell_width=40, cell_height=20)
    offer, result = taskbar_helpers._offer_and_hits(core, font)
    return core, encoder, decoder, session, server, client, keyboard, display, pointer, offer, result


@pytest.mark.parametrize("kind,steps", [(FieldKind.INTEGER, 2), (FieldKind.CHOICE, -3)])
def test_acknowledged_value_slot_wheel_reaches_binary_adjust_without_value_mutation(font, kind, steps):
    (core, _, _, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font, field_helpers._content(kind))
    assert len(result.hit_targets) == 1
    target = result.hit_targets[0]
    assert isinstance(target, FieldHitTarget) and target.adjustable
    assert target.content_revision == 7
    assert (target.rect.left, target.rect.top, target.rect.right, target.rect.bottom) == (40, 20, 80, 40)
    display.stage(offer, keyboard.generation)
    display.stage_frame_hit_map(offer, result.hit_entries)
    assert not pointer.wheel(0, steps, (60, 30), (80, 40))
    assert client.requests == []

    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    before, output = core.retained_state, core.output_view
    assert pointer.wheel(0, steps, (60, 30), (80, 40), modifiers=5)
    assert len(session.events) == 1
    event = session.events[0]
    assert (event.control_id, event.event_kind, event.content_revision,
            event.adjustment, event.modifiers, event.model_revision) == (
        1, ControlEventKind.ADJUST, 7, steps, 5, 3,
    )
    method, params = client.requests[0]
    assert method == "send_text_event"
    assert set(params) == {
        "owner_id", "owner_generation", "control_id", "modifiers", "generation",
        "display_offer_id", "display_scope", "event_kind", "content_revision", "adjustment",
    }
    assert keyboard.pending_events == 0
    assert core.retained_state is before and core.output_view is output


@pytest.mark.parametrize("kind", list(FieldKind))
def test_value_click_uses_existing_activate_for_each_writable_kind(font, kind):
    (core, _, _, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font, field_helpers._content(kind))
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    assert isinstance(result.hit_targets[0], FieldHitTarget)
    assert result.hit_targets[0].adjustable == (kind is not FieldKind.TEXT)
    before = core.retained_state
    assert pointer.button_down(1, (60, 30), (80, 40))
    assert pointer.button_up(1, (60, 30), (80, 40))
    assert len(session.events) == 1
    event = session.events[0]
    assert event.event_kind is ControlEventKind.ACTIVATE and event.content_revision == 0
    assert client.requests[0][0] == "send_control_event"
    assert core.retained_state is before


def test_label_area_and_horizontal_wheel_emit_no_field_or_raw_pointer_input(font):
    (_, _, _, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font)
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    assert not pointer.wheel(0, 1, (20, 30), (80, 40))
    assert not pointer.button_down(1, (20, 30), (80, 40))
    assert not pointer.button_up(1, (20, 30), (80, 40))
    assert not pointer.wheel(3, 0, (60, 30), (80, 40))
    assert not pointer.wheel(0, 0, (60, 30), (80, 40))
    assert client.requests == session.events == []


@pytest.mark.parametrize("kind,readonly,state", [
    (FieldKind.INTEGER, True, field_helpers.VISIBLE_ENABLED),
    (FieldKind.CHOICE, True, field_helpers.VISIBLE_ENABLED),
    (FieldKind.TEXT, True, field_helpers.VISIBLE_ENABLED),
    (FieldKind.INTEGER, False, ControlState.VISIBLE),
])
def test_readonly_and_disabled_fields_have_no_value_target_or_input_fallthrough(font, kind, readonly, state):
    (_, _, _, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font, field_helpers._content(kind, readonly=readonly), state=state)
    assert result.hit_targets == ()
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    assert not pointer.wheel(0, 1, (60, 30), (80, 40))
    assert not pointer.button_down(1, (60, 30), (80, 40))
    assert not pointer.button_up(1, (60, 30), (80, 40))
    assert client.requests == session.events == []


def test_writable_text_field_accepts_activation_but_ignores_adjustment_wheel(font):
    (_, _, _, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font, field_helpers._content(FieldKind.TEXT))
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    assert not pointer.wheel(0, -2, (60, 30), (80, 40))
    assert client.requests == session.events == []


def test_backpressured_adjustment_is_never_queued_or_rebound_to_next_content(font):
    (core, encoder, decoder, session, server, client, keyboard, display, pointer,
     offer, result) = _environment(font)
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    session.backpressured = True
    assert not pointer.wheel(0, 2, (60, 30), (80, 40))
    assert keyboard.pending_events == 0
    assert len(client.requests) == 1 and session.events == []

    replacement = field_helpers._definition(replace(field_helpers._content(), content_revision=8, value=442))
    field_helpers._present(core, encoder, decoder, 4, (
        (RetainedMessageType.CONTROL_REPLACE, encode_control_definition(replacement)),
    ))
    new_offer, new_result = taskbar_helpers._offer_and_hits(core, font, offer_id=2)
    keyboard.begin_display_offer()
    taskbar_helpers._ack(session, server, keyboard, display, new_offer, new_result)
    session.backpressured = False
    keyboard.flush_pending()
    assert len(client.requests) == 1 and session.events == []
    assert pointer.wheel(0, -1, (60, 30), (80, 40))
    assert len(session.events) == 1
    assert (session.events[0].content_revision, session.events[0].adjustment,
            session.events[0].model_revision) == (8, -1, 4)
    assert core.retained_state.active.owners[7].controls[1].content.value == 442


def test_selected_field_preserves_guest_keyboard_navigation_and_editor_keys(font):
    (_, _, _, session, server, client, keyboard, display, _,
     offer, result) = _environment(font, state=field_helpers.VISIBLE_ENABLED | ControlState.SELECTED)
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    keys = keyboard.pygame
    for code in (keys.K_LEFT, keys.K_RIGHT, keys.K_UP, keys.K_DOWN, keys.K_RETURN, keys.K_F2):
        assert keyboard.key_down(SimpleNamespace(key=code, mod=0, unicode=""))
    assert session.keys == ["left", "right", "up", "down", "enter", "f2"]
    assert all(method == "send_key" for method, _ in client.requests)
    assert session.events == []
