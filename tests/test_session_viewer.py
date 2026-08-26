"""Focused keyboard-forwarding contracts for the shared pygame viewer."""

from __future__ import annotations

import sys
from contextlib import nullcontext
from types import SimpleNamespace

import pytest

import session_viewer
from display import VirtualTerminal
from rich_terminal.retained_view import DisplayScope, RetainedRootLabelPlane
from session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from session_viewer import (
    DISPLAY_CLAIM_RETRY_SECONDS,
    KEY_REPEAT_DELAY_MS,
    KEY_REPEAT_INTERVAL_MS,
    _GuestKeyboardForwarder,
    _RetainedDisplayState,
    _accept_screen_update,
    _accept_status_update,
    _accepted_presentation_revision,
    _configure_keyboard,
    _pygame_guest_key,
    _retry_display_claim,
    compose_terminal_frame,
    draw_flip_and_present,
)
from shared_session import (
    display_offer_to_wire,
    display_scope_to_wire,
    snapshot_to_wire,
)


class _FakeKeyModule:
    def __init__(self):
        self.calls = []

    def start_text_input(self):
        self.calls.append(("start_text_input",))

    def set_repeat(self, delay, interval):
        self.calls.append(("set_repeat", delay, interval))

    def get_mods(self):
        return 0


class _FakePygame:
    KMOD_SHIFT = 0x01
    KMOD_ALT = 0x02
    KMOD_CTRL = 0x04
    KMOD_MODE = 0x08

    K_a = 100
    K_z = 125
    K_0 = 200
    K_9 = 209
    K_SPACE = 300

    K_RETURN = 400
    K_ESCAPE = 401
    K_TAB = 402
    K_BACKSPACE = 403
    K_DELETE = 404
    K_UP = 405
    K_DOWN = 406
    K_LEFT = 407
    K_RIGHT = 408
    K_HOME = 409
    K_END = 410
    K_PAGEUP = 411
    K_PAGEDOWN = 412
    K_INSERT = 413
    K_F1 = 414
    K_F2 = 415
    K_F3 = 416
    K_F4 = 417
    K_F5 = 418
    K_F6 = 419
    K_F7 = 420
    K_F8 = 421
    K_F9 = 422
    K_F10 = 423
    K_F11 = 424
    K_F12 = 425

    def __init__(self):
        self.key = _FakeKeyModule()


class _RecordingClient:
    def __init__(self, responses=None):
        self.requests = []
        self.responses = list(responses or [])

    def request(self, method, **params):
        self.requests.append((method, params))
        if self.responses:
            response = self.responses.pop(0)
            if isinstance(response, dict):
                return response
            return {"status": response}
        return {"status": "progress"}


def _key_event(key, *, mod=0, unicode=""):
    return SimpleNamespace(key=key, mod=mod, unicode=unicode)


def _display_offer(offer_id=1, *, char="X"):
    scope = DisplayScope(1, 2, 0, offer_id, 0, offer_id, offer_id)
    snapshot = TerminalSnapshot(
        cols=1,
        rows=1,
        cells=((TerminalCell(char, (1, 2, 3), (4, 5, 6), 0),),),
        cursor_col=0,
        cursor_row=0,
        cursor_visible=True,
        alternate_screen=False,
    )
    plane = RetainedRootLabelPlane(
        retained_initialized=True,
        retained_visible=bool(offer_id & 1),
        regions=(),
    )
    return TerminalDisplayOffer(offer_id, scope, snapshot, plane)


def test_keyboard_configuration_enables_established_repeat_rate():
    pygame = _FakePygame()

    _configure_keyboard(pygame)

    assert pygame.key.calls == [
        ("start_text_input",),
        ("set_repeat", KEY_REPEAT_DELAY_MS, KEY_REPEAT_INTERVAL_MS),
    ]
    assert (KEY_REPEAT_DELAY_MS, KEY_REPEAT_INTERVAL_MS) == (400, 35)


def test_alt_digit_is_forwarded_once_without_textinput_leak():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    event = _key_event(pygame.K_0 + 5, mod=pygame.KMOD_ALT, unicode="5")

    assert keyboard.key_down(event)
    assert keyboard.text_input(SimpleNamespace(text="5"))
    keyboard.key_up(event)

    assert client.requests == [
        ("send_key", {"key": "alt+5", "generation": 0})
    ]


def test_modified_chord_does_not_suppress_unrelated_composed_text():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    event = _key_event(pygame.K_a + 5, mod=pygame.KMOD_ALT, unicode="f")

    assert keyboard.key_down(event)
    assert keyboard.text_input(SimpleNamespace(text="é"))
    assert keyboard.text_input(SimpleNamespace(text="f"))

    assert client.requests == [
        ("send_key", {"key": "alt+f", "generation": 0}),
        ("send_text", {"text": "é", "generation": 0}),
    ]


def test_named_keydown_repeats_and_modified_navigation_is_preserved():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    backspace = _key_event(pygame.K_BACKSPACE)
    alt_left = _key_event(pygame.K_LEFT, mod=pygame.KMOD_ALT)

    assert keyboard.key_down(backspace)
    assert keyboard.key_down(backspace, repeated=True)
    assert _pygame_guest_key(pygame, alt_left) == "alt+left"
    assert keyboard.key_down(alt_left)

    assert client.requests == [
        ("send_key", {"key": "backspace", "generation": 0}),
        ("send_key", {"key": "backspace", "generation": 0}),
        ("send_key", {"key": "alt+left", "generation": 0}),
    ]


def test_activation_keys_and_shortcuts_do_not_repeat():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    enter = _key_event(pygame.K_RETURN)
    alt_digit = _key_event(pygame.K_0 + 5, mod=pygame.KMOD_ALT, unicode="5")

    assert keyboard.key_down(enter)
    assert keyboard.key_down(enter, repeated=True)
    assert keyboard.key_down(alt_digit)
    assert keyboard.key_down(alt_digit, repeated=True)

    assert client.requests == [
        ("send_key", {"key": "enter", "generation": 0}),
        ("send_key", {"key": "alt+5", "generation": 0}),
    ]


def test_altgr_and_composed_text_remain_textinput_driven():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    altgr = _key_event(
        pygame.K_0 + 5,
        mod=pygame.KMOD_CTRL | pygame.KMOD_ALT | pygame.KMOD_MODE,
        unicode="€",
    )

    assert not keyboard.key_down(altgr)
    assert keyboard.text_input(SimpleNamespace(text="€"))

    assert client.requests == [
        ("send_text", {"text": "€", "generation": 0})
    ]


def test_focus_reset_releases_modified_text_suppression():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    event = _key_event(pygame.K_a + 5, mod=pygame.KMOD_ALT, unicode="f")

    assert keyboard.key_down(event)
    keyboard.reset()
    assert keyboard.text_input(SimpleNamespace(text="x"))

    assert client.requests == [
        ("send_key", {"key": "alt+f", "generation": 0}),
        ("send_text", {"text": "x", "generation": 0}),
    ]


def test_backpressured_input_is_retained_and_retried_in_order():
    pygame = _FakePygame()
    client = _RecordingClient(
        responses=("backpressured", "progress", "progress")
    )
    keyboard = _GuestKeyboardForwarder(
        pygame,
        client,
        max_pending_events=2,
    )

    assert keyboard.key_down(_key_event(pygame.K_RETURN))
    assert keyboard.pending_events == 1
    assert keyboard.text_input(SimpleNamespace(text="x"))
    assert keyboard.pending_events == 2
    assert client.requests == [
        ("send_key", {"key": "enter", "generation": 0})
    ]

    keyboard.flush_pending()

    assert keyboard.pending_events == 0
    assert client.requests == [
        ("send_key", {"key": "enter", "generation": 0}),
        ("send_key", {"key": "enter", "generation": 0}),
        ("send_text", {"text": "x", "generation": 0}),
    ]


def test_failed_or_full_input_retention_is_visible():
    pygame = _FakePygame()
    failed = _RecordingClient(responses=("failed",))
    keyboard = _GuestKeyboardForwarder(pygame, failed)
    assert keyboard.key_down(_key_event(pygame.K_RETURN))
    assert keyboard.pending_events == 0
    assert "failed" in keyboard.last_error

    blocked = _RecordingClient(responses=("backpressured",))
    keyboard = _GuestKeyboardForwarder(
        pygame,
        blocked,
        max_pending_events=1,
    )
    assert keyboard.key_down(_key_event(pygame.K_RETURN))
    assert keyboard.text_input(SimpleNamespace(text="x"))
    assert keyboard.pending_events == 1
    assert "retention full" in keyboard.last_error

    # Even if the older retained event drains in this same frame, the dropped
    # input remains visible until a reset/generation change acknowledges it.
    keyboard.flush_pending()
    assert keyboard.pending_events == 0
    assert "retention full" in keyboard.last_error


def test_reset_generation_rejects_and_discards_retained_old_input():
    pygame = _FakePygame()
    client = _RecordingClient(responses=("backpressured", "stale_generation"))
    keyboard = _GuestKeyboardForwarder(pygame, client, generation=3)

    assert keyboard.key_down(_key_event(pygame.K_RETURN))
    assert keyboard.pending_events == 1
    keyboard.flush_pending()

    assert keyboard.pending_events == 0
    assert "stale_generation" in keyboard.last_error
    keyboard.set_generation(4)
    assert keyboard.generation == 4
    assert keyboard.last_error is None


def test_retained_display_state_promotes_only_an_accepted_offer():
    state = _RetainedDisplayState()
    first = _display_offer(1)

    state.stage(first, 7)

    assert state.since_offer == 0
    assert state.pending_offer is first
    assert state.pending_generation == 7
    assert state.frame_plane is first.retained
    assert state.retained_plane is None

    assert state.finish_presentation(
        {"status": "stale_display", "presented": False}
    ) is None
    assert state.since_offer == 0
    assert state.pending_offer is None
    assert state.retained_plane is None

    state.stage(first, 7)
    assert state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 11}
    ) == 11
    assert state.since_offer == first.offer_id
    assert state.pending_offer is None
    assert state.retained_plane is first.retained
    assert state.frame_plane is first.retained

    second = _display_offer(2, char="Y")
    state.stage(second, 9)
    assert state.since_offer == first.offer_id
    assert state.retained_plane is first.retained
    assert state.frame_plane is second.retained
    assert state.finish_presentation(
        {"status": "stale_generation", "presented": False}
    ) is None
    assert state.since_offer == first.offer_id
    assert state.pending_offer is None
    assert state.retained_plane is None

    state.stage(second, 9)
    assert state.finish_presentation(
        {"status": "duplicate", "presented": True, "revision": 12}
    ) == 12
    assert state.since_offer == second.offer_id
    assert state.retained_plane is second.retained

    state.reset()
    assert state.since_offer == second.offer_id
    assert state.retained_plane is None
    with pytest.raises(RuntimeError, match="did not advance"):
        state.stage(_display_offer(second.offer_id), 10)
    with pytest.raises(RuntimeError, match="did not advance"):
        state.stage(_display_offer(first.offer_id), 10)


def test_retained_display_reset_clears_fallback_and_input_context():
    client = _RecordingClient(responses=("backpressured",))
    keyboard = _GuestKeyboardForwarder(
        _FakePygame(),
        client,
        generation=3,
        display_required=True,
    )
    state = _RetainedDisplayState()
    offer = _display_offer(1)
    state.stage(offer, 3)
    state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 4}
    )
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
    assert keyboard.key_down(_key_event(_FakePygame.K_RETURN))
    assert keyboard.pending_events == 1

    state.reset()
    keyboard.clear_display_context(waiting=True)

    assert state.since_offer == offer.offer_id
    assert state.retained_plane is None
    assert keyboard.display_ack is None
    assert keyboard.pending_events == 0
    requests_before_waiting_input = list(client.requests)
    assert keyboard.text_input(SimpleNamespace(text="stale"))
    assert client.requests == requests_before_waiting_input
    assert "waiting" in keyboard.last_error


def test_screen_cell_fallback_clears_plane_but_offer_cell_wins_when_present():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(
        pygame,
        client,
        generation=3,
        display_required=True,
    )
    terminal = VirtualTerminal(cols=1, rows=1)
    state = _RetainedDisplayState()
    acknowledged = _display_offer(1, char="A")
    state.stage(acknowledged, 3)
    state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 4}
    )
    keyboard.acknowledge_display_offer(
        acknowledged.offer_id, acknowledged.scope
    )

    fallback = _display_offer(4, char="F").cell
    revision, resized = _accept_screen_update(
        {
            "changed": True,
            "generation": 3,
            "revision": 5,
            "snapshot": snapshot_to_wire(fallback),
        },
        display_holder=True,
        terminal=terminal,
        keyboard=keyboard,
        display_state=state,
        revision=4,
    )

    assert (revision, resized) == (5, False)
    assert terminal.grid[0][0][0] == "F"
    assert state.since_offer == acknowledged.offer_id
    assert state.retained_plane is None
    assert keyboard.display_ack is None

    baseline = _display_offer(5, char="B").cell
    offered = _display_offer(2, char="O")
    revision, resized = _accept_screen_update(
        {
            "changed": True,
            "generation": 3,
            "revision": 6,
            "snapshot": snapshot_to_wire(baseline),
            "display_offer": display_offer_to_wire(offered),
        },
        display_holder=True,
        terminal=terminal,
        keyboard=keyboard,
        display_state=state,
        revision=revision,
    )

    assert (revision, resized) == (6, False)
    assert terminal.grid[0][0][0] == "O"
    assert state.since_offer == acknowledged.offer_id
    assert state.pending_offer == offered
    assert state.frame_plane == offered.retained
    assert keyboard.display_ack is None


def test_screen_response_shape_is_current_and_fail_closed():
    terminal = VirtualTerminal(cols=1, rows=1)
    keyboard = _GuestKeyboardForwarder(
        _FakePygame(), _RecordingClient(), generation=1
    )
    state = _RetainedDisplayState()

    with pytest.raises(RuntimeError, match="invalid response shape"):
        _accept_screen_update(
            {
                "changed": False,
                "generation": 1,
                "revision": 0,
                "extra": None,
            },
            display_holder=True,
            terminal=terminal,
            keyboard=keyboard,
            display_state=state,
            revision=0,
        )
    with pytest.raises(RuntimeError, match="does not match"):
        _accept_screen_update(
            {"changed": True, "generation": 1, "revision": 0},
            display_holder=True,
            terminal=terminal,
            keyboard=keyboard,
            display_state=state,
            revision=0,
        )
    with pytest.raises(TypeError, match="not bool"):
        _accept_screen_update(
            {"changed": False, "generation": 1, "revision": True},
            display_holder=True,
            terminal=terminal,
            keyboard=keyboard,
            display_state=state,
            revision=0,
        )
    with pytest.raises(RuntimeError, match="refresh returned no CELL"):
        _accept_screen_update(
            {"changed": False, "generation": 1, "revision": 0},
            display_holder=True,
            terminal=terminal,
            keyboard=keyboard,
            display_state=state,
            revision=-1,
        )


def test_status_invalidation_forces_refresh_before_another_flip():
    keyboard = _GuestKeyboardForwarder(
        _FakePygame(),
        _RecordingClient(),
        generation=1,
        display_required=True,
    )
    state = _RetainedDisplayState()
    offer = _display_offer(1)
    state.stage(offer, 1)
    state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 3}
    )
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)
    state.stage(_display_offer(2), 1)
    keyboard.begin_display_offer()

    revision, refresh = _accept_status_update(
        {"generation": 2, "rich_terminal": {"display_required": True}},
        keyboard=keyboard,
        display_state=state,
        revision=4,
    )

    assert (revision, refresh) == (-1, True)
    assert keyboard.generation == 2
    assert keyboard.display_ack is None
    assert state.since_offer == offer.offer_id
    assert state.pending_offer is None
    assert state.retained_plane is None


def test_offer_transition_discards_queued_old_display_proof():
    pygame = _FakePygame()
    client = _RecordingClient(responses=("backpressured", "progress"))
    keyboard = _GuestKeyboardForwarder(
        pygame,
        client,
        generation=5,
        display_required=True,
    )
    old_offer = _display_offer(1)
    new_offer = _display_offer(2)
    keyboard.acknowledge_display_offer(old_offer.offer_id, old_offer.scope)

    assert keyboard.key_down(_key_event(pygame.K_RETURN))
    assert keyboard.pending_events == 1
    keyboard.begin_display_offer()
    assert keyboard.pending_events == 0
    assert keyboard.display_ack is None

    requests_before_ack = list(client.requests)
    assert keyboard.text_input(SimpleNamespace(text="blocked"))
    assert client.requests == requests_before_ack

    keyboard.acknowledge_display_offer(new_offer.offer_id, new_offer.scope)
    assert keyboard.text_input(SimpleNamespace(text="current"))
    assert client.requests[-1] == (
        "send_text",
        {
            "text": "current",
            "generation": 5,
            "display_offer_id": new_offer.offer_id,
            "display_scope": display_scope_to_wire(new_offer.scope),
        },
    )


def test_stale_display_is_nonfatal_but_invalidates_input_proof():
    pygame = _FakePygame()
    client = _RecordingClient(responses=("stale_display",))
    keyboard = _GuestKeyboardForwarder(
        pygame,
        client,
        generation=8,
        display_required=True,
    )
    offer = _display_offer(1)
    keyboard.acknowledge_display_offer(offer.offer_id, offer.scope)

    assert keyboard.text_input(SimpleNamespace(text="x"))

    assert keyboard.display_ack is None
    assert keyboard.pending_events == 0
    assert "stale_display" in keyboard.last_error
    request_count = len(client.requests)
    assert keyboard.text_input(SimpleNamespace(text="y"))
    assert len(client.requests) == request_count


def test_periodic_display_reclaim_transitions_busy_observer_to_holder():
    assert DISPLAY_CLAIM_RETRY_SECONDS == 0.25
    client = _RecordingClient(
        responses=(
            {"status": "display_busy", "claimed": False},
            {"status": "claimed", "claimed": True},
        )
    )
    keyboard = _GuestKeyboardForwarder(
        _FakePygame(),
        client,
        generation=4,
        input_enabled=False,
        display_required=True,
    )
    state = _RetainedDisplayState()
    acknowledged = _display_offer(1)
    state.stage(acknowledged, 4)
    state.finish_presentation(
        {"status": "presented", "presented": True, "revision": 5}
    )
    pending = _display_offer(2)
    state.stage(pending, 4)

    holder, revision, refresh = _retry_display_claim(
        client,
        keyboard=keyboard,
        display_state=state,
        revision=5,
    )
    assert (holder, revision, refresh) == (False, 5, False)
    assert not keyboard.input_enabled

    keyboard.acknowledge_display_offer(pending.offer_id, pending.scope)
    assert keyboard._enqueue_input("send_text", {"text": "old"})
    holder, revision, refresh = _retry_display_claim(
        client,
        keyboard=keyboard,
        display_state=state,
        revision=revision,
    )

    assert (holder, revision, refresh) == (True, -1, True)
    assert keyboard.input_enabled
    assert keyboard.display_ack is None
    assert keyboard.pending_events == 0
    assert state.since_offer == acknowledged.offer_id
    assert state.pending_offer is None
    assert state.retained_plane is None
    assert client.requests == [
        ("claim_display", {}),
        ("claim_display", {}),
    ]


def test_periodic_display_reclaim_rejects_malformed_claim():
    client = _RecordingClient(
        responses=({"status": "claimed", "claimed": True, "extra": 3},)
    )
    keyboard = _GuestKeyboardForwarder(_FakePygame(), client)
    with pytest.raises(RuntimeError, match="invalid response shape"):
        _retry_display_claim(
            client,
            keyboard=keyboard,
            display_state=_RetainedDisplayState(),
            revision=0,
        )
    assert not keyboard.input_enabled


def test_terminal_composition_orders_cell_label_then_cursor(monkeypatch):
    events = []
    surface = object()
    plane = _display_offer().retained

    class Terminal:
        cols = 2
        rows = 2
        cx = 1
        cy = 1
        cursor_visible = True
        _lock = nullcontext()

        def render(
            self,
            pygame_module,
            font,
            cell_width,
            cell_height,
            *,
            show_cursor,
            _cache,
        ):
            events.append(("cell", show_cursor))
            return surface

    class Draw:
        @staticmethod
        def rect(target, color, rectangle):
            events.append(("cursor", target, color, rectangle))

    pygame = SimpleNamespace(draw=Draw())

    def composite(*args):
        assert args[1] is surface
        assert args[2] is plane
        events.append(("label",))

    monkeypatch.setattr(session_viewer, "composite_root_labels", composite)

    assert compose_terminal_frame(
        pygame,
        Terminal(),
        object(),
        6,
        10,
        retained_plane=plane,
        show_cursor=True,
    ) is surface
    assert events == [
        ("cell", False),
        ("label",),
        ("cursor", surface, (255, 255, 255), (6, 18, 6, 2)),
    ]


def test_physical_present_occurs_only_after_draw_and_flip():
    events = []
    offer = _display_offer(3)

    class Display:
        @staticmethod
        def flip():
            events.append("flip")

    class Client:
        @staticmethod
        def request(method, **params):
            events.append((method, params))
            return {"status": "presented", "presented": True, "revision": 9}

    result = draw_flip_and_present(
        SimpleNamespace(display=Display()),
        Client(),
        lambda: events.append("draw"),
        offer=offer,
        generation=7,
    )

    assert result == {"status": "presented", "presented": True, "revision": 9}
    assert events == [
        "draw",
        "flip",
        (
            "present",
            {
                "generation": 7,
                "display_offer_id": offer.offer_id,
                "display_scope": display_scope_to_wire(offer.scope),
            },
        ),
    ]


def test_quit_draw_and_flip_failures_never_present():
    offer = _display_offer()
    client = _RecordingClient()
    flips = []

    class Display:
        @staticmethod
        def flip():
            flips.append("flip")

    pygame = SimpleNamespace(display=Display())
    draws = []
    assert draw_flip_and_present(
        pygame,
        client,
        lambda: draws.append("draw"),
        offer=offer,
        generation=1,
        active=False,
    ) is None
    assert draws == []
    assert flips == []
    assert client.requests == []

    def failed_draw():
        raise RuntimeError("draw failed")

    with pytest.raises(RuntimeError, match="draw failed"):
        draw_flip_and_present(
            pygame, client, failed_draw, offer=offer, generation=1
        )
    assert flips == []
    assert client.requests == []

    class FailedDisplay:
        @staticmethod
        def flip():
            raise RuntimeError("flip failed")

    with pytest.raises(RuntimeError, match="flip failed"):
        draw_flip_and_present(
            SimpleNamespace(display=FailedDisplay()),
            client,
            lambda: draws.append("drawn before failed flip"),
            offer=offer,
            generation=1,
        )
    assert client.requests == []


def test_status_only_flip_never_presents_and_present_shape_is_strict():
    client = _RecordingClient()
    flips = []
    pygame = SimpleNamespace(
        display=SimpleNamespace(flip=lambda: flips.append("flip"))
    )

    assert draw_flip_and_present(
        pygame,
        client,
        lambda: None,
        offer=None,
        generation=0,
    ) is None
    assert flips == ["flip"]
    assert client.requests == []

    assert _accepted_presentation_revision(
        {"status": "duplicate", "presented": True, "revision": 4}
    ) == 4
    assert _accepted_presentation_revision(
        {"status": "stale_generation", "presented": False}
    ) is None
    with pytest.raises(RuntimeError, match="invalid state"):
        _accepted_presentation_revision(
            {"status": "presented", "presented": False, "revision": 4}
        )
    with pytest.raises(RuntimeError, match="invalid shape"):
        _accepted_presentation_revision(
            {
                "status": "presented",
                "presented": True,
                "revision": 4,
                "extra": None,
            }
        )
    with pytest.raises(RuntimeError, match="invalid shape"):
        _accepted_presentation_revision(
            {"status": "stale_display", "presented": False, "extra": None}
        )
    with pytest.raises(TypeError, match="not bool"):
        _accepted_presentation_revision(
            {"status": "presented", "presented": True, "revision": True}
        )


def test_initial_shape_failure_closes_client_without_pygame_cleanup(
    monkeypatch,
):
    clients = []
    pygame_quits = []

    class Client:
        def __init__(self, *args, **kwargs):
            self.closed = False
            clients.append(self)

        def connect(self):
            pass

        def request(self, method, **params):
            assert method == "claim_display"
            return {"status": "claimed", "claimed": 1}

        def close(self):
            self.closed = True

    fake_pygame = SimpleNamespace(quit=lambda: pygame_quits.append(True))
    monkeypatch.setattr(session_viewer, "SessionClient", Client)
    monkeypatch.setitem(sys.modules, "pygame", fake_pygame)
    monkeypatch.setattr(sys, "argv", ["session_viewer.py"])

    assert session_viewer.main() == 2
    assert len(clients) == 1
    assert clients[0].closed
    assert pygame_quits == []


def test_pygame_initialization_failure_closes_client_and_pygame(monkeypatch):
    clients = []
    calls = []
    snapshot = snapshot_to_wire(_display_offer().cell)

    class Client:
        def __init__(self, *args, **kwargs):
            self.closed = False
            clients.append(self)

        def connect(self):
            pass

        def request(self, method, **params):
            if method == "claim_display":
                return {"status": "claimed", "claimed": True}
            if method == "status":
                return {
                    "generation": 1,
                    "rich_terminal": {"display_required": False},
                }
            if method == "screen":
                return {
                    "changed": True,
                    "generation": 1,
                    "revision": 0,
                    "snapshot": snapshot,
                }
            raise AssertionError(method)

        def close(self):
            self.closed = True

    class Display:
        @staticmethod
        def init():
            calls.append("display.init")

    class Font:
        @staticmethod
        def init():
            calls.append("font.init")
            raise RuntimeError("font failed")

    fake_pygame = SimpleNamespace(
        display=Display(),
        font=Font(),
        quit=lambda: calls.append("pygame.quit"),
    )
    monkeypatch.setattr(session_viewer, "SessionClient", Client)
    monkeypatch.setitem(sys.modules, "pygame", fake_pygame)
    monkeypatch.setattr(sys, "argv", ["session_viewer.py"])

    assert session_viewer.main() == 2
    assert len(clients) == 1
    assert clients[0].closed
    assert calls == ["display.init", "font.init", "pygame.quit"]
