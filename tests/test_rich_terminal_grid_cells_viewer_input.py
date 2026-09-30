"""Typed grid cells retain their stable keys through render, ACK and input."""

import pytest

pygame = pytest.importorskip("pygame")

from tests import test_rich_terminal_grid_cells_server as grid_helpers
from tests import test_rich_terminal_taskbar_viewer_input as taskbar_helpers
from tests import test_rich_terminal_semantic_viewer_input as viewer_helpers
from emulator.shared_session import SharedMachine
from rich_terminal.pygame_view import TextHitTarget, TextPosition
from rich_terminal.retained_scene import ControlKind
from rich_terminal.retained_wire import ControlEventKind
from session_viewer import _GuestKeyboardForwarder, _PointerRouter, _RetainedDisplayState
from shared_session import SessionServer


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _environment(font, *, unavailable=False):
    core, _, decoder = grid_helpers._visible_core(grid_helpers._content(unavailable=unavailable))
    session = taskbar_helpers._CoreSession(core, decoder)
    machine = SharedMachine(session)
    machine._reset_generation = taskbar_helpers.GENERATION
    server = SessionServer(machine, "/tmp/unused-grid-cells-input.sock")
    server._display_holder = taskbar_helpers.CONNECTION
    client = taskbar_helpers._JsonClient(server)
    keyboard = _GuestKeyboardForwarder(
        viewer_helpers._Pygame(), client,
        generation=taskbar_helpers.GENERATION, display_required=True,
    )
    display = _RetainedDisplayState()
    pointer = _PointerRouter(display, keyboard, cell_width=40, cell_height=20)
    offer, result = taskbar_helpers._offer_and_hits(core, font)
    target, = (entry for entry in result.hit_entries if isinstance(entry, TextHitTarget))
    assert target.kind is ControlKind.TEXT_GRID
    return core, session, server, client, keyboard, display, pointer, offer, result, target


@pytest.mark.parametrize("point,item_key", [((20, 10), 11), ((60, 10), 12), ((20, 30), 13)])
def test_number_formula_error_pixels_name_exact_whole_cell_after_ack(font, point, item_key):
    (core, session, server, client, keyboard, display, pointer,
     offer, result, target) = _environment(font)
    assert target.position_at(*point) == TextPosition(item_key, 0)
    display.stage(offer, keyboard.generation)
    display.stage_frame_hit_map(offer, result.hit_entries)
    assert not pointer.button_down(1, point, (80, 40))
    assert client.requests == []

    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    before = core.retained_state
    assert pointer.button_down(1, point, (80, 40))
    pointer.button_up(1, point, (80, 40))
    assert len(session.events) == 1
    event = session.events[0]
    assert (event.event_kind, event.item_key, event.scalar_offset,
            event.content_revision, event.model_revision) == (
        ControlEventKind.PLACE, item_key, 0, 7, 3,
    )
    method, params = client.requests[0]
    assert method == "send_text_event"
    assert params["item_key"] == item_key and params["scalar_offset"] == 0
    assert core.retained_state is before
    assert before.active.owners[7].controls[1].content.primary_key == 0


def test_header_and_unavailable_number_never_become_selectable_or_raw_targets(font):
    (_, session, server, client, keyboard, display, pointer,
     offer, result, target) = _environment(font, unavailable=True)
    assert target.position_at(20, 10) is None
    assert target.position_at(60, 30) is None
    assert target.position_at(60, 10) == TextPosition(12, 0)
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    for point in ((20, 10), (60, 30)):
        assert not pointer.button_down(1, point, (80, 40))
        assert not pointer.button_up(1, point, (80, 40))
    assert client.requests == session.events == []


def test_typed_grid_wheel_preserves_scroll_event_and_positive_down_convention(font):
    (core, session, server, client, keyboard, display, pointer,
     offer, result, _) = _environment(font)
    taskbar_helpers._ack(session, server, keyboard, display, offer, result)
    before = core.retained_state
    assert pointer.wheel(1, 2, (60, 10), (80, 40))
    event, = session.events
    assert (event.event_kind, event.wheel_x, event.wheel_y, event.content_revision) == (
        ControlEventKind.SCROLL, 1, -2, 0,
    )
    assert client.requests[0][0] == "send_text_event"
    assert core.retained_state is before
