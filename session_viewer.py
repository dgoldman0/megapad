#!/usr/bin/env python3
"""Pygame viewer/input client for a shared MegaPad session."""

from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path

from display import VirtualTerminal
from shared_session import DEFAULT_SOCKET, SessionClient, snapshot_from_wire


ROOT = Path(__file__).resolve().parent
KEY_REPEAT_DELAY_MS = 400
KEY_REPEAT_INTERVAL_MS = 35


class _GuestKeyboardForwarder:
    """Forward pygame input once while keeping TEXTINPUT for composed text."""

    def __init__(self, pygame, client):
        self.pygame = pygame
        self.client = client
        self.suppressed_text_keys: dict[int, set[str]] = {}

    def key_down(self, event, *, repeated: bool = False) -> bool:
        key_name = _pygame_guest_key(self.pygame, event)
        if key_name is None:
            self.suppressed_text_keys.pop(event.key, None)
            return False
        if repeated and not _pygame_repeatable_guest_key(self.pygame, event):
            return True
        character = _pygame_modified_character(self.pygame, event)
        if character is not None:
            translated = getattr(event, "unicode", "")
            self.suppressed_text_keys[event.key] = {
                text for text in (character, translated) if text
            }
        self.client.request("send_key", key=key_name)
        return True

    def key_up(self, event) -> None:
        self.suppressed_text_keys.pop(event.key, None)

    def text_input(self, event) -> bool:
        if not event.text:
            return False
        if any(
            event.text in texts for texts in self.suppressed_text_keys.values()
        ):
            return True
        self.client.request("send_text", text=event.text)
        return True

    def reset(self) -> None:
        self.suppressed_text_keys.clear()


def apply_snapshot(terminal: VirtualTerminal, wire: dict):
    snapshot = snapshot_from_wire(wire)
    if terminal.cols != snapshot.cols or terminal.rows != snapshot.rows:
        terminal.resize(snapshot.cols, snapshot.rows)
    with terminal._lock:
        terminal.grid = [
            [(cell.char, cell.fg, cell.bg, cell.attrs) for cell in row]
            for row in snapshot.cells
        ]
        terminal.cx = snapshot.cursor_col
        terminal.cy = snapshot.cursor_row
        terminal.cursor_visible = snapshot.cursor_visible
        terminal._in_alt_screen = snapshot.alternate_screen
        terminal._dirty = True


def main() -> int:
    parser = argparse.ArgumentParser(description="Watch a shared MegaPad session")
    parser.add_argument("--socket", default=DEFAULT_SOCKET)
    parser.add_argument("--font", type=Path)
    parser.add_argument("--font-size", type=int, default=18)
    parser.add_argument("--fps", type=int, default=30)
    parser.add_argument("--title", default="MegaPad-64 Shared Session")
    parser.add_argument("--exit-after", type=float, help=argparse.SUPPRESS)
    args = parser.parse_args()

    try:
        import pygame
    except ImportError:
        print("session viewer requires pygame", file=sys.stderr)
        return 2

    client = SessionClient(args.socket, timeout=2.0)
    try:
        client.connect()
        first = client.request("screen", since=-1)
    except OSError as exc:
        print(f"cannot connect to shared session: {exc}", file=sys.stderr)
        return 2

    # The machine-owner process may hold the optional audio mixer.  This
    # viewer only needs video, font, and input, so do not claim an audio
    # device merely as a side effect of pygame.init().
    pygame.display.init()
    pygame.font.init()
    _configure_keyboard(pygame)
    font = (
        pygame.font.Font(str(args.font), args.font_size)
        if args.font else pygame.font.SysFont("monospace", args.font_size)
    )
    status_font = pygame.font.SysFont("sans", max(12, args.font_size - 4))
    cell_w = max(1, font.size("M")[0])
    cell_h = font.get_linesize()
    status_h = max(24, status_font.get_linesize() + 8)
    terminal = VirtualTerminal(cols=80, rows=30)
    revision = -1
    if first.get("changed"):
        apply_snapshot(terminal, first["snapshot"])
        revision = first["revision"]

    def make_window():
        return pygame.display.set_mode(
            (terminal.cols * cell_w, terminal.rows * cell_h + status_h)
        )

    screen = make_window()
    pygame.display.set_caption(args.title)
    clock = pygame.time.Clock()
    status = client.request("status", detailed=False)
    last_poll = 0.0
    last_status = 0.0
    connected = True
    glyph_cache = {}
    running = True
    guest_keyboard = _GuestKeyboardForwarder(pygame, client)
    keys_down: set[int] = set()
    viewer_started = time.monotonic()

    try:
        while running:
            if args.exit_after and time.monotonic() - viewer_started >= args.exit_after:
                break
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.TEXTINPUT:
                    guest_keyboard.text_input(event)
                elif event.type == pygame.KEYDOWN:
                    mods = _pygame_event_mods(pygame, event)
                    ctrl = bool(mods & pygame.KMOD_CTRL)
                    repeated = event.key in keys_down
                    keys_down.add(event.key)
                    if ctrl and event.key == pygame.K_q and not repeated:
                        running = False
                    elif ctrl and event.key == pygame.K_F5 and not repeated:
                        status = client.request("status", detailed=False)
                        method = "resume" if status["paused"] else "pause"
                        status = client.request(method)
                    elif ctrl and event.key == pygame.K_F10 and not repeated:
                        status = client.request("pause")
                        client.request("step", count=1)
                    elif ctrl and event.key == pygame.K_r and not repeated:
                        status = client.request("reset", paused=False)
                    elif not (
                        ctrl
                        and event.key
                        in (pygame.K_q, pygame.K_F5, pygame.K_F10, pygame.K_r)
                    ):
                        guest_keyboard.key_down(event, repeated=repeated)
                elif event.type == pygame.KEYUP:
                    keys_down.discard(event.key)
                    guest_keyboard.key_up(event)
                elif event.type == getattr(pygame, "WINDOWFOCUSLOST", -1):
                    keys_down.clear()
                    guest_keyboard.reset()

            now = time.monotonic()
            if now - last_poll >= 1.0 / max(1, args.fps):
                update = client.request("screen", since=revision)
                if update.get("changed"):
                    old_size = (terminal.cols, terminal.rows)
                    apply_snapshot(terminal, update["snapshot"])
                    revision = update["revision"]
                    if old_size != (terminal.cols, terminal.rows):
                        screen = make_window()
                last_poll = now
            if now - last_status >= 0.25:
                status = client.request("status", detailed=False)
                last_status = now

            screen.fill((0, 0, 0))
            cursor_blink = int(now * 2) % 2 == 0
            terminal_surface = terminal.render(
                pygame,
                font,
                cell_w,
                cell_h,
                show_cursor=cursor_blink,
                _cache=glyph_cache,
            )
            screen.blit(terminal_surface, (0, 0))
            y = terminal.rows * cell_h
            pygame.draw.rect(screen, (28, 30, 34), (0, y, screen.get_width(), status_h))
            state_color = (100, 220, 140) if status["state"] in ("running", "idle") else (245, 190, 80)
            status_text = (
                f"{status['state'].upper()}  steps {status['steps']:,}  "
                f"rev {status['revision']}  clients {status.get('clients', 0)}"
            )
            label = status_font.render(status_text, True, state_color)
            screen.blit(label, (8, y + (status_h - label.get_height()) // 2))
            pygame.display.flip()
            clock.tick(max(1, args.fps))
    except (OSError, ConnectionError, RuntimeError) as exc:
        connected = False
        print(f"shared viewer disconnected: {exc}", file=sys.stderr)
    finally:
        client.close()
        pygame.key.stop_text_input()
        pygame.quit()
    return 0 if connected else 2


def _pygame_key_name(pygame, key: int) -> str | None:
    mapping = {
        pygame.K_RETURN: "enter",
        pygame.K_ESCAPE: "escape",
        pygame.K_TAB: "tab",
        pygame.K_BACKSPACE: "backspace",
        pygame.K_DELETE: "delete",
        pygame.K_UP: "up",
        pygame.K_DOWN: "down",
        pygame.K_LEFT: "left",
        pygame.K_RIGHT: "right",
        pygame.K_HOME: "home",
        pygame.K_END: "end",
        pygame.K_PAGEUP: "pageup",
        pygame.K_PAGEDOWN: "pagedown",
        pygame.K_INSERT: "insert",
        pygame.K_F1: "f1",
        pygame.K_F2: "f2",
        pygame.K_F3: "f3",
        pygame.K_F4: "f4",
        pygame.K_F5: "f5",
        pygame.K_F6: "f6",
        pygame.K_F7: "f7",
        pygame.K_F8: "f8",
        pygame.K_F9: "f9",
        pygame.K_F10: "f10",
        pygame.K_F11: "f11",
        pygame.K_F12: "f12",
    }
    return mapping.get(key)


def _configure_keyboard(pygame) -> None:
    pygame.key.start_text_input()
    pygame.key.set_repeat(KEY_REPEAT_DELAY_MS, KEY_REPEAT_INTERVAL_MS)


def _pygame_event_mods(pygame, event) -> int:
    mods = getattr(event, "mod", None)
    return pygame.key.get_mods() if mods is None else mods


def _pygame_character_name(pygame, event) -> str | None:
    if pygame.K_a <= event.key <= pygame.K_z:
        return chr(ord("a") + event.key - pygame.K_a)
    if pygame.K_0 <= event.key <= pygame.K_9:
        return chr(ord("0") + event.key - pygame.K_0)
    if event.key == pygame.K_SPACE:
        return "space"
    text = getattr(event, "unicode", "")
    if len(text) == 1 and text.isascii() and text.isprintable() and text != "+":
        return text
    return None


def _pygame_modifier_names(pygame, event) -> list[str]:
    mods = _pygame_event_mods(pygame, event)
    if mods & getattr(pygame, "KMOD_MODE", 0):
        return []
    names = []
    if mods & pygame.KMOD_CTRL:
        names.append("ctrl")
    if mods & pygame.KMOD_ALT:
        names.append("alt")
    if mods & pygame.KMOD_SHIFT:
        names.append("shift")
    return names


def _pygame_modified_character(pygame, event) -> str | None:
    modifiers = _pygame_modifier_names(pygame, event)
    if "ctrl" not in modifiers and "alt" not in modifiers:
        return None
    return _pygame_character_name(pygame, event)


def _pygame_guest_key(pygame, event) -> str | None:
    modifiers = _pygame_modifier_names(pygame, event)
    named = _pygame_key_name(pygame, event.key)
    if named is not None:
        if modifiers and named in {
            "up",
            "down",
            "left",
            "right",
            "home",
            "end",
            "insert",
            "delete",
            "pageup",
            "pagedown",
            "f5",
            "f6",
            "f7",
            "f8",
            "f9",
            "f10",
            "f11",
            "f12",
        }:
            return "+".join((*modifiers, named))
        return named

    character = _pygame_modified_character(pygame, event)
    if character is None:
        return None
    return "+".join((*modifiers, character))


def _pygame_repeatable_guest_key(pygame, event) -> bool:
    """Limit host key repeat to editing and navigation operations."""

    return _pygame_key_name(pygame, event.key) in {
        "backspace",
        "delete",
        "up",
        "down",
        "left",
        "right",
        "home",
        "end",
        "pageup",
        "pagedown",
    }


if __name__ == "__main__":
    raise SystemExit(main())
