"""Focused physical-ACK authority tests for semantic viewer activation."""

from __future__ import annotations

from contextlib import nullcontext
from types import SimpleNamespace

import pytest

import session_viewer
from rich_terminal.pygame_view import (
    CompositeDrawResult,
    ControlHitTarget,
    ControlIdentity,
    PixelRect,
)
from rich_terminal.retained_scene import ControlKind
from rich_terminal.retained_view import DisplayScope, RetainedDrawPlane
from session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from session_viewer import (
    _GuestKeyboardForwarder,
    _RetainedDisplayState,
    _SemanticPointerInteractor,
    _accept_status_update,
    _pygame_apt_modifiers,
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


def test_pending_hit_map_is_not_authority_until_accepted_physical_present():
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=4,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _SemanticPointerInteractor(state, keyboard)
    offer = _offer(1)
    target = _target()

    state.stage(offer, 4)
    state.stage_frame_hit_map(offer, (target,))
    # Even a prematurely installed input proof cannot expose candidate hits.
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
    assert state.hit_targets == ()
    assert state.hit_map_token is None
    assert not pointer.left_down((20, 20), (100, 80))
    assert not pointer.left_up((20, 20), (100, 80), modifiers=0)
    assert client.requests == []

    assert state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 1}
    ) == 1
    assert state.hit_targets == (target,)
    assert state.hit_map_token == (offer.offer_id, offer.scope)
    assert pointer.move((20, 20), (100, 80)) == target


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


def test_render_flip_present_then_promotion_order_is_explicit():
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
    pointer = _SemanticPointerInteractor(state, keyboard)
    _promote(state, keyboard, _offer(1), (_target(),))
    assert pointer.move((20, 20), (100, 80)) is not None
    assert pointer.left_down((20, 20), (100, 80))

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
    pointer = _SemanticPointerInteractor(state, keyboard)
    offer = _offer(4, revision=12)
    target = _target(kind=ControlKind.TAB)
    _promote(state, keyboard, offer, (target,))
    modifiers = _pygame_apt_modifiers(
        pygame,
        SimpleNamespace(mod=pygame.KMOD_SHIFT | pygame.KMOD_ALT),
    )

    assert pointer.left_down((20, 20), (100, 80))
    assert pointer.left_up((20, 20), (100, 80), modifiers=modifiers)
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


def test_mismatched_release_status_area_and_unmapped_rows_are_noops():
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        client,
        generation=2,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _SemanticPointerInteractor(state, keyboard)
    first = _target(3, rect=(10, 10, 50, 30))
    second = _target(4, rect=(55, 10, 95, 30), kind=ControlKind.MENU)
    _promote(state, keyboard, _offer(1), (first, second))

    assert pointer.left_down((20, 20), (100, 80))
    assert not pointer.left_up((60, 20), (100, 80), modifiers=0)
    assert not pointer.left_down((5, 40), (100, 80))
    assert not pointer.left_up((5, 40), (100, 80), modifiers=0)
    # Y=90 is the caller-owned status strip, below the 80-pixel terminal.
    assert not pointer.left_down((20, 90), (100, 80))
    assert not pointer.left_up((20, 90), (100, 80), modifiers=0)
    # Disabled items and separators have no ControlHitTarget, so their painted
    # rows are indistinguishable from any other deliberate gap here.
    assert not pointer.left_down((20, 50), (100, 80))
    assert not pointer.left_up((20, 50), (100, 80), modifiers=0)
    assert client.requests == []


def test_offer_and_focus_transitions_clear_renderer_local_hover_and_press():
    keyboard = _GuestKeyboardForwarder(
        _Pygame(),
        _RecordingClient(),
        generation=3,
        display_required=True,
    )
    state = _RetainedDisplayState()
    pointer = _SemanticPointerInteractor(state, keyboard)
    _promote(state, keyboard, _offer(1), (_target(),))
    pointer.move((20, 20), (100, 80))
    pointer.left_down((20, 20), (100, 80))
    assert pointer.hovered is not None and pointer.pressed is not None

    state.stage(_offer(2), 3)
    keyboard.begin_display_offer()
    assert pointer.hovered is None
    assert pointer.pressed is None

    state.reset()
    pointer.clear()  # Main invokes this for both focus-lost and focus-gained.
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
        return CompositeDrawResult(surface, (target,))

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
    assert result.hit_targets == (target,)
    assert events[0] == ("cell", False)
    assert events[1][0:3] == ("semantic", surface, plane)
    assert events[1][3]["control_font"] is control_font
    assert events[1][3]["hovered"] == target.identity
