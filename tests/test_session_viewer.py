"""Focused keyboard-forwarding contracts for the shared pygame viewer."""

from __future__ import annotations

from types import SimpleNamespace

from session_viewer import (
    KEY_REPEAT_DELAY_MS,
    KEY_REPEAT_INTERVAL_MS,
    _GuestKeyboardForwarder,
    _configure_keyboard,
    _pygame_guest_key,
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
    def __init__(self):
        self.requests = []

    def request(self, method, **params):
        self.requests.append((method, params))
        return {}


def _key_event(key, *, mod=0, unicode=""):
    return SimpleNamespace(key=key, mod=mod, unicode=unicode)


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

    assert client.requests == [("send_key", {"key": "alt+5"})]


def test_modified_chord_does_not_suppress_unrelated_composed_text():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    event = _key_event(pygame.K_a + 5, mod=pygame.KMOD_ALT, unicode="f")

    assert keyboard.key_down(event)
    assert keyboard.text_input(SimpleNamespace(text="é"))
    assert keyboard.text_input(SimpleNamespace(text="f"))

    assert client.requests == [
        ("send_key", {"key": "alt+f"}),
        ("send_text", {"text": "é"}),
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
        ("send_key", {"key": "backspace"}),
        ("send_key", {"key": "backspace"}),
        ("send_key", {"key": "alt+left"}),
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
        ("send_key", {"key": "enter"}),
        ("send_key", {"key": "alt+5"}),
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

    assert client.requests == [("send_text", {"text": "€"})]


def test_focus_reset_releases_modified_text_suppression():
    pygame = _FakePygame()
    client = _RecordingClient()
    keyboard = _GuestKeyboardForwarder(pygame, client)
    event = _key_event(pygame.K_a + 5, mod=pygame.KMOD_ALT, unicode="f")

    assert keyboard.key_down(event)
    keyboard.reset()
    assert keyboard.text_input(SimpleNamespace(text="x"))

    assert client.requests == [
        ("send_key", {"key": "alt+f"}),
        ("send_text", {"text": "x"}),
    ]
