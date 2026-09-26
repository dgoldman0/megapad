"""Focused sink-ACK authority tests for semantic viewer activation."""

from __future__ import annotations

from contextlib import nullcontext
from types import SimpleNamespace

import pytest

import session_viewer
from rich_terminal.pygame_view import (
    CompositeDrawResult,
    ControlHitTarget,
    ControlIdentity,
    ControlSurface,
    PixelRect,
    RegionOcclusion,
    ResidualPoint,
    TextHitTarget,
)
from rich_terminal.retained_scene import ControlKind
from rich_terminal.retained_view import DisplayScope, RetainedDrawPlane
from session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from session_viewer import (
    _GuestKeyboardForwarder,
    _RetainedDisplayState,
    _PointerRouter,
    _accept_status_update,
    _pygame_apt_modifiers,
    capture_final_terminal_raster,
    compose_terminal_frame_result,
    draw_flip_and_present,
)
from shared_session import display_scope_to_wire


class _KeyState:
    def __init__(self, modifiers=0):
        self.modifiers = modifiers

    def get_mods(self):
        return self.modifiers


class _Pygame:
    # Deliberately use pygame-shaped, non-APT masks so raw forwarding fails.
    KMOD_SHIFT = 0x0003
    KMOD_CTRL = 0x00C0
    KMOD_ALT = 0x0300
    KMOD_GUI = 0x0C00
    KMOD_NUM = 0x1000
    KMOD_CAPS = 0x2000

    def __init__(self, modifiers=0):
        self.key = _KeyState(modifiers)


class _RecordingClient:
    def __init__(self, responses=()):
        self.requests = []
        self.responses = list(responses)

    def request(self, method, **params):
        self.requests.append((method, params))
        if self.responses:
            return {"status": self.responses.pop(0)}
        return {"status": "progress"}


def _offer(offer_id=1, *, revision=None):
    model_revision = offer_id if revision is None else revision
    scope = DisplayScope(1, 2, 0, model_revision, 0, model_revision, model_revision)
    cell = TerminalSnapshot(
        10,
        8,
        tuple(
            tuple(
                TerminalCell(" ", (7, 7, 7), (0, 0, 0), 0)
                for _ in range(10)
            )
            for _ in range(8)
        ),
        0,
        0,
        False,
        False,
    )
    return TerminalDisplayOffer(
        offer_id,
        scope,
        cell,
        RetainedDrawPlane(True, True, ()),
    )


def _target(control_id=3, *, rect=(10, 10, 60, 32), kind=ControlKind.MENU_ITEM):
    return ControlHitTarget(
        ControlIdentity(7, 2, control_id),
        kind,
        PixelRect(*rect),
    )


def _occlusion(*, region_id=9, rect=(10, 10, 30, 32)):
    return RegionOcclusion(8, 3, region_id, PixelRect(*rect))


def _promote(state, keyboard, offer, targets, *, response_status="presented"):
    state.stage(offer, keyboard.generation)
    state.stage_frame_hit_map(offer, targets)
    revision = state.finish_presentation(
        {
            "status": response_status,
            "presented": True,
            "revision": offer.scope.model_revision,
        }
    )
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
    return revision


def test_only_independently_activatable_controls_can_enter_the_hit_map():
    assert _target(kind=ControlKind.TAB).kind is ControlKind.TAB
    with pytest.raises(ValueError, match="MENU, MENU_ITEM, and TAB"):
        _target(kind=ControlKind.TEXT_AREA)
    with pytest.raises(ValueError, match="MENU, MENU_ITEM, and TAB"):
        _target(kind=ControlKind.TEXT_GRID)


def test_pending_hit_map_is_not_authority_until_accepted_sink_present():
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=4,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    offer = _offer(1)
    target = _target()

    state.stage(offer, 4)
    state.stage_frame_hit_map(offer, (target,))
    # Even a prematurely installed input proof cannot expose candidate hits.
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
    assert state.hit_targets == ()
    assert state.hit_map_token is None
    assert not pointer.button_down(1, (20, 20), (100, 80))
    assert not pointer.button_up(1, (20, 20), (100, 80), modifiers=0)
    assert client.requests == []

    assert state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 1}
    ) == 1
    assert state.hit_targets == (target,)
    assert state.hit_map_token == (offer.offer_id, offer.scope)
    assert pointer.move((20, 20), (100, 80)) == target


def test_unified_barrier_map_promotes_only_on_ack_and_clears_on_stale_offer():
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=4,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    first = _offer(1)
    lower = _target(rect=(10, 10, 60, 32))
    barrier = _occlusion(rect=(10, 10, 30, 32))
    entries = (lower, barrier)

    state.stage(first, 4)
    state.stage_frame_hit_map(first, entries)
    keyboard.acknowledge_display_offer(first.offer_id, first.scope)
    assert state.hit_entries == ()
    assert pointer.move((20, 20), (100, 80)) is None

    assert state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 1}
    ) == 1
    assert state.hit_entries == entries
    assert state.hit_targets == (lower,)
    # The upper region's barrier shows that region's own content, so the
    # point is a raw cell there and never the lower control.
    assert pointer.move((20, 20), (100, 80)) == ResidualPoint(2, 2)
    assert pointer.hovered is None
    assert pointer.move((40, 20), (100, 80)) == lower

    second = _offer(2)
    state.stage(second, 4)
    assert state.hit_entries == ()
    assert state.hit_targets == ()
    assert pointer.move((40, 20), (100, 80)) is None
    state.stage_frame_hit_map(second, entries)
    assert state.finish_presentation(
        {"status": "stale_display", "presented": False}
    ) is None
    assert state.hit_entries == ()
    assert state.hit_map_token is None


def test_accepted_present_requires_a_rendered_map_for_the_exact_offer():
    state = _RetainedDisplayState()
    offer = _offer(1)
    state.stage(offer, 4)

    with pytest.raises(RuntimeError, match="not rendered"):
        state.finish_presentation(
            {"status": "presented", "presented": True, "revision": 1}
        )
    assert state.hit_map_token is None
    assert state.hit_targets == ()


def test_reference_sink_flip_precedes_present_and_hit_map_promotion():
    events = []
    state = _RetainedDisplayState()
    offer = _offer(1)
    target = _target()
    state.stage(offer, 4)

    class Display:
        @staticmethod
        def flip():
            events.append("flip")

    class Client:
        @staticmethod
        def request(method, **params):
            events.append(method)
            return {"status": "presented", "presented": True, "revision": 1}

    def draw():
        events.append("render")
        state.stage_frame_hit_map(offer, (target,))

    response = draw_flip_and_present(
        SimpleNamespace(display=Display()),
        Client(),
        draw,
        offer=offer,
        generation=4,
    )
    events.append("promote")
    state.finish_presentation(response)

    assert events == ["render", "flip", "present", "promote"]
    assert state.hit_targets == (target,)


def test_duplicate_promotes_but_new_stale_and_fallback_transitions_clear_hits():
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        _RecordingClient(),
        generation=4,
        display_required=True,
    )
    state = _RetainedDisplayState()
    first = _offer(1)
    target = _target()

    assert _promote(
        state,
        keyboard,
        first,
        (target,),
        response_status="duplicate",
    ) == first.scope.model_revision
    assert state.hit_targets == (target,)

    second = _offer(2)
    state.stage(second, 4)
    assert state.hit_targets == ()
    assert state.hit_map_token is None
    state.stage_frame_hit_map(second, (target,))
    assert state.finish_presentation(
        {"status": "stale_display", "presented": False}
    ) is None
    assert state.hit_targets == ()

    third = _offer(3)
    _promote(state, keyboard, third, (target,))
    state.reset()  # CELL fallback and display-lease reset use this exact path.
    assert state.hit_targets == ()
    assert state.hit_map_token is None


def test_generation_transition_clears_promoted_hits_and_pointer_state():
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        _RecordingClient(),
        generation=4,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    _promote(state, keyboard, _offer(1), (_target(),))
    assert pointer.move((20, 20), (100, 80)) is not None
    assert pointer.button_down(1, (20, 20), (100, 80))

    revision, refresh = _accept_status_update(
        {"generation": 5, "rich_terminal": {"display_required": True}},
        keyboard=keyboard,
        display_state=state,
        revision=1,
    )

    assert (revision, refresh) == (-1, True)
    assert state.hit_targets == ()
    assert pointer.hovered is None
    assert pointer.pressed is None


def test_tab_press_release_reuses_exact_proof_and_backpressure_path():
    pygame = _Pygame()
    client = _RecordingClient(("backpressured", "progress"))
    keyboard = _GuestKeyboardForwarder(
        pygame,
        client,
        generation=9,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    offer = _offer(4, revision=12)
    target = _target(kind=ControlKind.TAB)
    _promote(state, keyboard, offer, (target,))
    modifiers = _pygame_apt_modifiers(
        pygame,
        SimpleNamespace(mod=pygame.KMOD_SHIFT | pygame.KMOD_ALT),
    )

    assert pointer.button_down(1, (20, 20), (100, 80))
    assert pointer.button_up(1, (20, 20), (100, 80), modifiers=modifiers)
    assert keyboard.pending_events == 1
    expected = (
        "send_control_event",
        {
            "owner_id": 7,
            "owner_generation": 2,
            "control_id": 3,
            "modifiers": 0x05,
            "generation": 9,
            "display_offer_id": offer.offer_id,
            "display_scope": display_scope_to_wire(offer.scope),
        },
    )
    assert client.requests == [expected]

    keyboard.flush_pending()
    assert keyboard.pending_events == 0
    assert client.requests == [expected, expected]


def test_mismatched_release_status_area_and_popup_padding_are_noops():
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=2,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    first = _target(3, rect=(10, 10, 50, 30))
    second = _target(4, rect=(55, 10, 95, 30), kind=ControlKind.MENU)
    # An open popup is a renderer-laid-out surface: its padding, separators,
    # and disabled rows swallow the pointer instead of reaching CELL.
    popup = ControlSurface(7, 2, 5, PixelRect(0, 0, 100, 60))
    _promote(state, keyboard, _offer(1), (popup, first, second))

    assert pointer.button_down(1, (20, 20), (100, 80))
    assert not pointer.button_up(1, (60, 20), (100, 80), modifiers=0)
    assert not pointer.button_down(1, (5, 40), (100, 80))
    assert not pointer.button_up(1, (5, 40), (100, 80), modifiers=0)
    # Y=90 is the caller-owned status strip, below the 80-pixel terminal.
    assert not pointer.button_down(1, (20, 90), (100, 80))
    assert not pointer.button_up(1, (20, 90), (100, 80), modifiers=0)
    assert not pointer.button_down(1, (20, 50), (100, 80))
    assert not pointer.button_up(1, (20, 50), (100, 80), modifiers=0)
    assert client.requests == []


def test_offer_and_focus_transitions_clear_renderer_local_hover_and_press():
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        _RecordingClient(),
        generation=3,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    _promote(state, keyboard, _offer(1), (_target(),))
    pointer.move((20, 20), (100, 80))
    pointer.button_down(1, (20, 20), (100, 80))
    assert pointer.hovered is not None and pointer.pressed is not None

    state.stage(_offer(2), 3)
    keyboard.begin_display_offer()
    assert pointer.hovered is None
    assert pointer.pressed is None

    state.reset()
    pointer.cancel()  # Main invokes this for both focus-lost and focus-gained.
    assert pointer.hovered is None
    assert pointer.pressed is None


def test_pygame_modifiers_are_normalized_to_only_apt_bits_zero_through_five():
    pygame = _Pygame()
    raw = (
        pygame.KMOD_SHIFT
        | pygame.KMOD_CTRL
        | pygame.KMOD_ALT
        | pygame.KMOD_GUI
        | pygame.KMOD_CAPS
        | pygame.KMOD_NUM
        | 0x800000
    )

    assert _pygame_apt_modifiers(pygame, SimpleNamespace(mod=raw)) == 0x3F
    assert _pygame_apt_modifiers(pygame, SimpleNamespace(mod=0)) == 0
    pygame.key.modifiers = pygame.KMOD_CTRL | pygame.KMOD_NUM
    assert _pygame_apt_modifiers(pygame, SimpleNamespace()) == 0x22


def test_companion_composition_returns_hits_from_the_exact_paint_pass(monkeypatch):
    events = []
    surface = object()
    plane = RetainedDrawPlane(True, True, ())
    target = _target()
    barrier = _occlusion()
    hit_entries = (barrier, target)
    control_font = object()

    class Terminal:
        cols = 2
        rows = 2
        cx = 0
        cy = 0
        cursor_visible = False
        _lock = nullcontext()

        @staticmethod
        def render(*args, **kwargs):
            events.append(("cell", kwargs["show_cursor"]))
            return surface

    def composite(*args, **kwargs):
        events.append(("semantic", args[1], args[2], kwargs))
        return CompositeDrawResult(surface, hit_entries)

    monkeypatch.setattr(session_viewer, "composite_draw_plane_result", composite)
    result = compose_terminal_frame_result(
        SimpleNamespace(draw=SimpleNamespace()),
        Terminal(),
        object(),
        6,
        10,
        retained_plane=plane,
        show_cursor=False,
        control_font=control_font,
        hovered=target.identity,
    )

    assert result.surface is surface
    assert result.hit_entries == hit_entries
    assert result.hit_targets == (target,)
    assert events[0] == ("cell", False)
    assert events[1][0:3] == ("semantic", surface, plane)
    assert events[1][3]["control_font"] is control_font
    assert events[1][3]["hovered"] == target.identity


def test_explicit_final_raster_capture_freezes_post_composition_surface(monkeypatch):
    events = []

    class Surface:
        @staticmethod
        def get_size():
            return 2, 1

    surface = Surface()
    plane = RetainedDrawPlane(True, True, ())

    class Terminal:
        cols = 2
        rows = 1
        cx = 0
        cy = 0
        cursor_visible = True
        _lock = nullcontext()

        @staticmethod
        def render(*args, **kwargs):
            events.append("cell")
            return surface

    def composite(*args, **kwargs):
        events.append("rich")
        return CompositeDrawResult(surface, ())

    def cursor(*args, **kwargs):
        events.append("cursor")

    class Image:
        @staticmethod
        def tobytes(captured, pixel_format):
            assert captured is surface
            assert pixel_format == "RGB"
            events.append("capture")
            return b"\x01\x02\x03\x04\x05\x06"

    pygame = SimpleNamespace(draw=SimpleNamespace(), image=Image())
    monkeypatch.setattr(session_viewer, "composite_draw_plane_result", composite)
    monkeypatch.setattr(session_viewer, "_paint_terminal_cursor", cursor)

    result = compose_terminal_frame_result(
        pygame,
        Terminal(),
        object(),
        6,
        10,
        retained_plane=plane,
        show_cursor=True,
    )
    raster = capture_final_terminal_raster(pygame, result.surface)

    assert events == ["cell", "rich", "cursor", "capture"]
    assert raster.pixel_format == "RGB888"
    assert raster.pixels == b"\x01\x02\x03\x04\x05\x06"


def _router(client, *, generation=5):
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=generation,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _PointerRouter(state, keyboard, cell_width=10, cell_height=10)
    return keyboard, state, pointer


def _text_target(*, content_revision=6, rect=(0, 0, 40, 20)):
    return TextHitTarget(
        ControlIdentity(7, 2, 30),
        ControlKind.TEXT_AREA,
        PixelRect(*rect),
        anchor_left=rect[0],
        anchor_top=rect[1],
        anchor_width=rect[2] - rect[0],
        anchor_height=rect[3] - rect[1],
        content_revision=content_revision,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=2,
        viewport_columns=4,
        rows=((0, 1, 3), (1, 2, 4)),
    )


def _pointer_request(
    offer, *, x, y, buttons, kind, generation=5, wheel_y=0, modifiers=0
):
    return (
        "send_pointer",
        {
            "x": x,
            "y": y,
            "buttons": buttons,
            "modifiers": modifiers,
            "kind": kind,
            "wheel_x": 0,
            "wheel_y": wheel_y,
            "generation": generation,
            "display_offer_id": offer.offer_id,
            "display_scope": display_scope_to_wire(offer.scope),
        },
    )


def test_residual_press_drag_and_release_reach_the_guest_as_raw_cells():
    client = _RecordingClient()
    keyboard, state, pointer = _router(client)
    offer = _offer(3)
    _promote(state, keyboard, offer, (_occlusion(rect=(0, 0, 100, 80)),))

    assert pointer.button_down(1, (15, 25), (100, 80))
    pointer.move((18, 28), (100, 80))  # same cell: nothing new to report
    pointer.move((35, 25), (100, 80))
    # A right press joins the gesture; its release leaves the left held.
    assert pointer.button_down(3, (35, 25), (100, 80))
    assert pointer.button_up(3, (35, 25), (100, 80))
    assert pointer.button_up(1, (45, 25), (100, 80))
    assert pointer.raw_buttons == 0

    assert client.requests == [
        _pointer_request(offer, x=1, y=2, buttons=1, kind=2),
        _pointer_request(offer, x=3, y=2, buttons=1, kind=1),
        _pointer_request(offer, x=3, y=2, buttons=5, kind=2),
        _pointer_request(offer, x=3, y=2, buttons=1, kind=3),
        _pointer_request(offer, x=4, y=2, buttons=0, kind=3),
    ]


def test_a_release_the_display_cannot_carry_is_owed_until_it_can():
    client = _RecordingClient()
    keyboard, state, pointer = _router(client)
    first = _offer(3)
    barrier = _occlusion(rect=(0, 0, 100, 80))
    _promote(state, keyboard, first, (barrier,))
    assert pointer.button_down(1, (15, 25), (100, 80))

    # A new frame is on its way: moves are dropped, the release is owed.
    second = _offer(4)
    state.stage(second, keyboard.generation)
    keyboard.begin_display_offer()
    pointer.move((55, 25), (100, 80))
    assert not pointer.button_up(1, (65, 45), (100, 80))
    assert pointer.release_owed
    pointer.flush()
    assert len(client.requests) == 1

    state.stage_frame_hit_map(second, (barrier,))
    state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 4}
    )
    keyboard.acknowledge_display_offer(second.offer_id, second.scope)
    pointer.flush()
    assert not pointer.release_owed
    assert client.requests[-1] == _pointer_request(
        second, x=6, y=4, buttons=0, kind=3
    )
    # With nothing owed a new gesture may start.
    assert pointer.button_down(1, (5, 5), (100, 80))


def test_focus_loss_ends_a_raw_gesture_with_an_owed_release():
    client = _RecordingClient()
    keyboard, state, pointer = _router(client)
    offer = _offer(3)
    _promote(state, keyboard, offer, (_occlusion(rect=(0, 0, 100, 80)),))
    assert pointer.button_down(2, (15, 25), (100, 80))

    pointer.cancel()

    assert client.requests[-1] == _pointer_request(
        offer, x=1, y=2, buttons=0, kind=3
    )
    assert pointer.raw_buttons == 0 and not pointer.release_owed


def test_text_press_places_and_a_drag_extends_through_positions():
    client = _RecordingClient()
    keyboard, state, pointer = _router(client)
    offer = _offer(3)
    _promote(
        state,
        keyboard,
        offer,
        (_occlusion(rect=(0, 0, 100, 80)), _text_target()),
    )

    def text_request(kind, key, offset, modifiers=0):
        return (
            "send_text_event",
            {
                "owner_id": 7,
                "owner_generation": 2,
                "control_id": 30,
                "event_kind": kind,
                "modifiers": modifiers,
                "content_revision": 6,
                "item_key": key,
                "scalar_offset": offset,
                "generation": 5,
                "display_offer_id": offer.offer_id,
                "display_scope": display_scope_to_wire(offer.scope),
            },
        )

    assert pointer.button_down(1, (25, 5), (100, 80))
    pointer.move((25, 8), (100, 80))  # same position: no repeat
    pointer.move((35, 15), (100, 80))
    # Dragging out of the root clamps to its nearest edge.
    pointer.move((90, 70), (100, 80))
    assert not pointer.button_up(1, (90, 70), (100, 80))
    assert pointer.button_down(1, (5, 5), (100, 80), modifiers=1)
    assert not pointer.button_up(1, (5, 5), (100, 80), modifiers=1)

    # PLACE at row 0 column 2; one EXTEND per new position (the clamped
    # out-of-root point repeats the last one); Shift-press extends directly.
    assert client.requests == [
        text_request(2, 1, 2),
        text_request(3, 2, 3),
        text_request(3, 1, 0, modifiers=1),
    ]


def test_wheel_scrolls_text_roots_and_reaches_residual_cells_as_raw_steps():
    client = _RecordingClient()
    keyboard, state, pointer = _router(client)
    offer = _offer(3)
    surface = ControlSurface(7, 2, 40, PixelRect(50, 0, 100, 20))
    _promote(
        state,
        keyboard,
        offer,
        (_occlusion(rect=(0, 0, 100, 80)), _text_target(), surface),
    )

    # Host wheel steps are positive upward; APT detents are positive down.
    assert pointer.wheel(0, 1, (5, 5), (100, 80))
    assert pointer.wheel(0, -2, (75, 45), (100, 80))
    assert not pointer.wheel(0, 1, (75, 5), (100, 80))  # tab strip or menu bar
    assert not pointer.wheel(0, 0, (75, 45), (100, 80))

    assert client.requests == [
        (
            "send_text_event",
            {
                "owner_id": 7,
                "owner_generation": 2,
                "control_id": 30,
                "event_kind": 4,
                "modifiers": 0,
                "wheel_x": 0,
                "wheel_y": -1,
                "generation": 5,
                "display_offer_id": offer.offer_id,
                "display_scope": display_scope_to_wire(offer.scope),
            },
        ),
        _pointer_request(offer, x=7, y=4, buttons=0, kind=4, wheel_y=2),
    ]
