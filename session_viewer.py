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

    pygame.init()
    pygame.key.start_text_input()
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
    status = client.request("status")
    last_poll = 0.0
    last_status = 0.0
    connected = True
    glyph_cache = {}
    running = True
    viewer_started = time.monotonic()

    try:
        while running:
            if args.exit_after and time.monotonic() - viewer_started >= args.exit_after:
                break
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.TEXTINPUT:
                    if event.text:
                        client.request("send_text", text=event.text)
                elif event.type == pygame.KEYDOWN:
                    mods = pygame.key.get_mods()
                    ctrl = bool(mods & pygame.KMOD_CTRL)
                    if ctrl and event.key == pygame.K_q:
                        running = False
                    elif event.key == pygame.K_F5:
                        status = client.request("status")
                        method = "resume" if status["paused"] else "pause"
                        status = client.request(method)
                    elif event.key == pygame.K_F10:
                        status = client.request("pause")
                        client.request("step", count=1)
                    elif ctrl and event.key == pygame.K_r:
                        status = client.request("reset", paused=False)
                    elif ctrl and pygame.K_a <= event.key <= pygame.K_z:
                        client.request("send_key", key=f"ctrl+{chr(event.key)}")
                    else:
                        key_name = _pygame_key_name(pygame, event.key)
                        if key_name:
                            client.request("send_key", key=key_name)

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
                status = client.request("status")
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
    }
    return mapping.get(key)


if __name__ == "__main__":
    raise SystemExit(main())
