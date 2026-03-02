"""
Megapad-64 GUI Display
========================
Full-featured emulator GUI built on pygame.

Features:
  - Menu bar: File (Load/Save snapshot, Quit), Machine (Reset, Pause/Resume),
    View (Terminal / Graphics / Debug)
  - Tab bar for Terminal (VT100) and Graphics (framebuffer) views
  - Debug panel: registers, flags, disassembly, memory view
  - Status bar: cycles, speed (MIPS), core state, FPS
  - Keyboard pass-through to UART
  - Snapshot save / load (JSON + binary memory)
  - Window resizing

Usage (programmatic):
    from display import FramebufferDisplay
    disp = FramebufferDisplay(sys_emu)
    disp.start()       # launches background thread
    ...                 # run emulator normally
    disp.stop()         # clean shutdown

Usage (CLI):
    python cli.py --bios bios.asm --storage sample.img --display
"""

from __future__ import annotations

import json
import os
import struct
import threading
import time
import zlib
from collections import deque
from pathlib import Path
from typing import TYPE_CHECKING, Optional

from diskutil import MP64FS, FTYPE_NAMES, FTYPE_FREE, FTYPE_DIR

if TYPE_CHECKING:
    from system import MegapadSystem

# Memory map constants (must match system.py)
HBW_BASE = 0xFFD0_0000
EXT_MEM_BASE = 0x0010_0000
VRAM_BASE = 0xFF00_0000

# Virtual terminal defaults
TERM_COLS = 80
TERM_ROWS = 30
CURSOR_BLINK_MS = 530    # cursor blink interval

# Base (unscaled) chrome sizes — multiplied by ui_scale in FramebufferDisplay
_BASE_MENU_HEIGHT = 24
_BASE_TAB_HEIGHT = 28
_BASE_STATUS_HEIGHT = 22

# ── Color Palette ─────────────────────────────────────────────────────

class Theme:
    """GUI color theme."""
    BG          = (24, 24, 32)
    MENU_BG     = (36, 36, 48)
    MENU_FG     = (200, 200, 210)
    MENU_HI_BG  = (60, 90, 160)
    MENU_HI_FG  = (255, 255, 255)
    MENU_SEP    = (55, 55, 70)
    TAB_BG      = (30, 30, 38)
    TAB_ACTIVE  = (50, 50, 65)
    TAB_FG      = (110, 110, 130)
    TAB_ACTIVE_FG = (220, 220, 240)
    TAB_ACCENT  = (80, 140, 255)
    STATUS_BG   = (36, 36, 48)
    STATUS_FG   = (140, 140, 160)
    STATUS_OK   = (80, 200, 120)
    STATUS_WARN = (220, 180, 60)
    STATUS_ERR  = (220, 80, 80)
    DROPDOWN_BG = (42, 42, 56)
    DROPDOWN_FG = (200, 200, 210)
    DROPDOWN_HI = (60, 90, 160)
    DROPDOWN_BORDER = (70, 70, 90)
    DEBUG_BG    = (28, 28, 36)
    DEBUG_LABEL = (100, 140, 200)
    DEBUG_VALUE = (200, 200, 220)
    DEBUG_ADDR  = (120, 180, 120)
    DEBUG_HEX   = (180, 180, 200)
    DEBUG_ASCII = (140, 140, 160)
    DEBUG_PC    = (255, 220, 80)
    DIALOG_BG   = (40, 40, 52)
    DIALOG_FG   = (200, 200, 220)
    DIALOG_BTN  = (60, 100, 180)
    DIALOG_BTN_FG = (255, 255, 255)


# ── Virtual Terminal ──────────────────────────────────────────────────


class VirtualTerminal:
    """VT100-ish text terminal backed by a character grid.

    Supports a subset of ANSI/VT100:
      - printable ASCII
      - \\r, \\n, \\t, \\b (BS), \\x7f (DEL)
      - ESC [ <n> ; <m> H  (cursor position)
      - ESC [ <n> J         (erase in display)
      - ESC [ <n> K         (erase in line)
      - ESC [ <n> m         (SGR — bold, color)
      - ESC [ ? 25 h/l      (show/hide cursor)
    """

    # Default CGA-ish 16-color palette (R, G, B)
    COLORS = [
        (0, 0, 0),          # 0 black
        (170, 0, 0),        # 1 red
        (0, 170, 0),        # 2 green
        (170, 85, 0),       # 3 yellow/brown
        (0, 0, 170),        # 4 blue
        (170, 0, 170),      # 5 magenta
        (0, 170, 170),      # 6 cyan
        (170, 170, 170),    # 7 white (light gray)
        (85, 85, 85),       # 8 bright black (dark gray)
        (255, 85, 85),      # 9 bright red
        (85, 255, 85),      # 10 bright green
        (255, 255, 85),     # 11 bright yellow
        (85, 85, 255),      # 12 bright blue
        (255, 85, 255),     # 13 bright magenta
        (85, 255, 255),     # 14 bright cyan
        (255, 255, 255),    # 15 bright white
    ]

    def __init__(self, cols: int = TERM_COLS, rows: int = TERM_ROWS):
        self.cols = cols
        self.rows = rows
        self.cx = 0        # cursor column
        self.cy = 0        # cursor row
        self.fg = 7        # foreground color index
        self.bg = 0        # background color index
        self.bold = False
        self.cursor_visible = True

        # Character grid: list of rows, each row = list of (char, fg, bg)
        self.grid: list[list[tuple[str, int, int]]] = []
        self._clear_grid()

        # Scrollback buffer (lines that scrolled off the top)
        self.scrollback: deque[list[tuple[str, int, int]]] = deque(maxlen=1000)

        # ESC sequence parser state
        self._esc_state = 0   # 0=normal, 1=got ESC, 2=got CSI
        self._esc_buf = ""

        # Lock for thread safety
        self._lock = threading.Lock()
        self._dirty = True

    def _clear_grid(self):
        self.grid = [
            [(' ', self.fg, self.bg) for _ in range(self.cols)]
            for _ in range(self.rows)
        ]

    def write(self, data: bytes | int):
        """Feed raw bytes (or a single int) into the terminal."""
        with self._lock:
            if isinstance(data, int):
                self._process_byte(data)
            else:
                for b in data:
                    self._process_byte(b)
            self._dirty = True

    def _process_byte(self, b: int):
        ch = chr(b) if b < 128 else ''

        # ESC sequence parsing
        if self._esc_state == 1:
            if ch == '[':
                self._esc_state = 2
                self._esc_buf = ""
            else:
                self._esc_state = 0  # unsupported ESC sequence
            return
        if self._esc_state == 2:
            if ch.isdigit() or ch in ';?':
                self._esc_buf += ch
                return
            # Got final character
            self._handle_csi(self._esc_buf, ch)
            self._esc_state = 0
            return

        # Normal character processing
        if b == 0x1B:   # ESC
            self._esc_state = 1
            return
        if b == 0x0D:   # CR
            self.cx = 0
            return
        if b == 0x0A:   # LF
            self._line_feed()
            return
        if b == 0x09:   # TAB
            self.cx = min((self.cx // 8 + 1) * 8, self.cols - 1)
            return
        if b == 0x08 or b == 0x7F:  # BS or DEL
            if self.cx > 0:
                self.cx -= 1
                self.grid[self.cy][self.cx] = (' ', self.fg, self.bg)
            return
        if b == 0x07:   # BEL — ignore
            return
        if b < 0x20:    # other control chars — ignore
            return

        # Printable character
        fg = min(self.fg + (8 if self.bold and self.fg < 8 else 0), 15)
        self.grid[self.cy][self.cx] = (ch, fg, self.bg)
        self.cx += 1
        if self.cx >= self.cols:
            self.cx = 0
            self._line_feed()

    def _line_feed(self):
        self.cy += 1
        if self.cy >= self.rows:
            self.cy = self.rows - 1
            self._scroll_up()

    def _scroll_up(self):
        """Scroll the display up one line."""
        old_top = self.grid[0]
        self.scrollback.append(old_top)
        del self.grid[0]
        self.grid.append(
            [(' ', self.fg, self.bg) for _ in range(self.cols)]
        )

    def _handle_csi(self, params: str, cmd: str):
        """Handle CSI (ESC [) sequences."""
        parts = params.split(';') if params else ['']

        def num(idx=0, default=1):
            try:
                return int(parts[idx]) if parts[idx] else default
            except (IndexError, ValueError):
                return default

        if cmd == 'H' or cmd == 'f':
            # Cursor position: ESC[row;colH
            self.cy = max(0, min(num(0, 1) - 1, self.rows - 1))
            self.cx = max(0, min(num(1, 1) - 1, self.cols - 1))
        elif cmd == 'A':
            self.cy = max(0, self.cy - num())
        elif cmd == 'B':
            self.cy = min(self.rows - 1, self.cy + num())
        elif cmd == 'C':
            self.cx = min(self.cols - 1, self.cx + num())
        elif cmd == 'D':
            self.cx = max(0, self.cx - num())
        elif cmd == 'J':
            n = num(0, 0)
            if n == 2:
                self._clear_grid()
                self.cx = self.cy = 0
            elif n == 0:
                # Erase from cursor to end
                for x in range(self.cx, self.cols):
                    self.grid[self.cy][x] = (' ', self.fg, self.bg)
                for y in range(self.cy + 1, self.rows):
                    for x in range(self.cols):
                        self.grid[y][x] = (' ', self.fg, self.bg)
        elif cmd == 'K':
            n = num(0, 0)
            if n == 0:
                for x in range(self.cx, self.cols):
                    self.grid[self.cy][x] = (' ', self.fg, self.bg)
            elif n == 1:
                for x in range(0, self.cx + 1):
                    self.grid[self.cy][x] = (' ', self.fg, self.bg)
            elif n == 2:
                for x in range(self.cols):
                    self.grid[self.cy][x] = (' ', self.fg, self.bg)
        elif cmd == 'm':
            # SGR — set graphic rendition
            codes = [num(i, 0) for i in range(len(parts))]
            for code in codes:
                if code == 0:
                    self.fg, self.bg, self.bold = 7, 0, False
                elif code == 1:
                    self.bold = True
                elif code == 22:
                    self.bold = False
                elif 30 <= code <= 37:
                    self.fg = code - 30
                elif 40 <= code <= 47:
                    self.bg = code - 40
                elif 90 <= code <= 97:
                    self.fg = code - 90 + 8
                elif 100 <= code <= 107:
                    self.bg = code - 100 + 8
                elif code == 39:
                    self.fg = 7
                elif code == 49:
                    self.bg = 0
        elif cmd == 'h':
            if params == '?25':
                self.cursor_visible = True
        elif cmd == 'l':
            if params == '?25':
                self.cursor_visible = False

    def render(self, pygame_module, font, cell_w: int, cell_h: int,
               show_cursor: bool = True,
               _cache: dict | None = None) -> 'pygame.Surface':
        """Render the terminal grid to a pygame surface."""
        with self._lock:
            surf_w = self.cols * cell_w
            surf_h = self.rows * cell_h
            surface = pygame_module.Surface((surf_w, surf_h))
            surface.fill(self.COLORS[0])

            cache = _cache if _cache is not None else {}

            for y in range(self.rows):
                for x in range(self.cols):
                    ch, fg, bg = self.grid[y][x]
                    px = x * cell_w
                    py = y * cell_h
                    if bg != 0:
                        pygame_module.draw.rect(
                            surface, self.COLORS[bg],
                            (px, py, cell_w, cell_h))
                    if ch != ' ':
                        key = (ch, fg)
                        glyph = cache.get(key)
                        if glyph is None:
                            glyph = font.render(
                                ch, False, self.COLORS[fg])
                            cache[key] = glyph
                        surface.blit(glyph, (px, py))

            # Draw cursor
            if show_cursor and self.cursor_visible:
                cx_px = self.cx * cell_w
                cy_px = self.cy * cell_h
                pygame_module.draw.rect(
                    surface, self.COLORS[7],
                    (cx_px, cy_px + cell_h - 2, cell_w, 2))

            self._dirty = False
        return surface


# ── Menu System ───────────────────────────────────────────────────────


class MenuItem:
    """A single menu item (label + optional shortcut + callback)."""
    def __init__(self, label: str, callback: callable = None,
                 shortcut: str = "", separator: bool = False,
                 enabled: bool = True, checked: bool = False):
        self.label = label
        self.callback = callback
        self.shortcut = shortcut
        self.separator = separator
        self.enabled = enabled
        self.checked = checked


class Menu:
    """A single dropdown menu (e.g. "File")."""
    def __init__(self, title: str, items: list[MenuItem]):
        self.title = title
        self.items = items


class MenuBar:
    """Top-level menu bar with dropdown menus."""
    def __init__(self, menus: list[Menu]):
        self.menus = menus
        self.active_menu: int = -1  # index of open dropdown, -1 = none
        self._rects: list = []  # (x, w) for each top-level label

    def layout(self, pygame_module, font, win_w: int):
        """Compute positions for each menu title."""
        self._rects = []
        x = 8
        for m in self.menus:
            w = font.size(f" {m.title} ")[0]
            self._rects.append((x, w))
            x += w + 2

    def draw(self, pygame_module, screen, font, win_w: int, menu_h: int = 24):
        """Draw the menu bar and any open dropdown."""
        self._menu_h = menu_h
        # Background
        pygame_module.draw.rect(screen, Theme.MENU_BG,
                                (0, 0, win_w, menu_h))

        # Menu titles
        for i, (m, (x, w)) in enumerate(zip(self.menus, self._rects)):
            active = (i == self.active_menu)
            bg = Theme.MENU_HI_BG if active else Theme.MENU_BG
            fg = Theme.MENU_HI_FG if active else Theme.MENU_FG
            pygame_module.draw.rect(screen, bg, (x, 0, w, menu_h))
            text = font.render(f" {m.title} ", True, fg)
            ty = (menu_h - text.get_height()) // 2
            screen.blit(text, (x, ty))

        # Draw dropdown if active
        if self.active_menu >= 0:
            self._draw_dropdown(pygame_module, screen, font)

    def _draw_dropdown(self, pygame_module, screen, font):
        menu = self.menus[self.active_menu]
        x, _w = self._rects[self.active_menu]
        items = menu.items
        menu_h = getattr(self, '_menu_h', 24)

        # Calculate dropdown dimensions
        item_h = font.get_linesize() + 8
        dd_w = max(240, font.size("Save Snapshot...    Ctrl+S")[0] + 40)
        dd_h = 0
        for item in items:
            dd_h += 1 if item.separator else item_h

        # Draw dropdown background + shadow
        dd_y = menu_h
        # Shadow
        pygame_module.draw.rect(screen, (15, 15, 20),
                                (x + 3, dd_y + 3, dd_w, dd_h))
        pygame_module.draw.rect(screen, Theme.DROPDOWN_BG,
                                (x, dd_y, dd_w, dd_h))
        pygame_module.draw.rect(screen, Theme.DROPDOWN_BORDER,
                                (x, dd_y, dd_w, dd_h), 1)

        # Draw items
        iy = dd_y
        for _idx, item in enumerate(items):
            if item.separator:
                pygame_module.draw.line(screen, Theme.MENU_SEP,
                                        (x + 8, iy), (x + dd_w - 8, iy))
                iy += 1
                continue

            # Highlight on hover
            mouse_pos = pygame_module.mouse.get_pos()
            rect = pygame_module.Rect(x + 1, iy, dd_w - 2, item_h)
            hover = rect.collidepoint(mouse_pos)
            if hover and item.enabled:
                pygame_module.draw.rect(screen, Theme.DROPDOWN_HI, rect)

            fg = Theme.DROPDOWN_FG if item.enabled else Theme.MENU_SEP
            # Check mark
            prefix = "\u2713 " if item.checked else "  "
            text = font.render(prefix + item.label, True, fg)
            screen.blit(text, (x + 8, iy + 4))

            # Shortcut
            if item.shortcut:
                sc = font.render(item.shortcut, True, Theme.MENU_SEP)
                screen.blit(sc, (x + dd_w - sc.get_width() - 10, iy + 4))

            iy += item_h

    def handle_click(self, mx: int, my: int, font=None) -> Optional[callable]:
        """Handle a mouse click. Returns a callback if a menu item was clicked."""
        menu_h = getattr(self, '_menu_h', 24)
        # Click on menu title?
        if my < menu_h:
            for i, (x, w) in enumerate(self._rects):
                if x <= mx < x + w:
                    if self.active_menu == i:
                        self.active_menu = -1  # toggle off
                    else:
                        self.active_menu = i
                    return None
            self.active_menu = -1
            return None

        # Click on dropdown item?
        if self.active_menu >= 0:
            menu = self.menus[self.active_menu]
            x, _w = self._rects[self.active_menu]
            dd_w = max(240, font.size("Save Snapshot...    Ctrl+S")[0] + 40) if font else 240
            item_h = (font.get_linesize() + 8) if font else 24

            iy = menu_h
            for item in menu.items:
                if item.separator:
                    iy += 1
                    continue
                if x <= mx < x + dd_w and iy <= my < iy + item_h:
                    self.active_menu = -1
                    if item.enabled and item.callback:
                        return item.callback
                    return None
                iy += item_h

            self.active_menu = -1
        return None

    def is_open(self) -> bool:
        return self.active_menu >= 0


# ── File Dialogs (native OS via tkinter) ──────────────────────────────

def _tk_dialog(func, **kwargs) -> Optional[str]:
    """Run a tkinter file dialog on a hidden root window.

    Works safely alongside pygame — the Tk root is created, used once,
    then destroyed.  Returns the selected path or None.
    """
    import tkinter as tk
    root = tk.Tk()
    root.withdraw()            # hide the Tk root window
    root.attributes("-topmost", True)   # appear above pygame
    root.update()              #  ensure it's mapped before the dialog
    result = func(**kwargs)
    root.destroy()
    return result if result else None


class SimpleDialog:
    """File / directory pickers using the native OS dialog (tkinter)."""

    @staticmethod
    def file_save(pygame_module, screen, font,
                  default_name: str = "snapshot.mp64") -> Optional[str]:
        """Native Save-File dialog.  Returns path or None."""
        from tkinter import filedialog
        return _tk_dialog(
            filedialog.asksaveasfilename,
            title="Save Snapshot",
            initialfile=default_name,
            defaultextension=".mp64",
            filetypes=[("Megapad-64 Snapshot", "*.mp64"), ("All Files", "*.*")],
        )

    @staticmethod
    def file_open(pygame_module, screen, font,
                  default_name: str = "") -> Optional[str]:
        """Native Open-File dialog.  Returns path or None."""
        from tkinter import filedialog
        return _tk_dialog(
            filedialog.askopenfilename,
            title="Load Snapshot",
            filetypes=[("Megapad-64 Snapshot", "*.mp64"), ("All Files", "*.*")],
        )

    @staticmethod
    def disk_swap(pygame_module, screen, font,
                  current_path: str = "") -> Optional[str]:
        """Native Open-File dialog for picking a disk image."""
        from tkinter import filedialog
        import os
        init_dir = os.path.dirname(current_path) if current_path else "."
        return _tk_dialog(
            filedialog.askopenfilename,
            title="Swap Disk Image",
            initialdir=init_dir,
            filetypes=[("Disk Images", "*.img"), ("All Files", "*.*")],
        )

    @staticmethod
    def pick_directory(pygame_module, screen, font,
                       default: str = "extracted",
                       title: str = "Choose Destination Folder") -> Optional[str]:
        """Native directory picker."""
        from tkinter import filedialog
        return _tk_dialog(
            filedialog.askdirectory,
            title=title,
            initialdir=default,
            mustexist=False,
        )

    # -- Disk file browser (list / extract) --------------------------------

    @staticmethod
    def disk_browser(pygame_module, screen, font,
                     fs: MP64FS) -> Optional[list[str]]:
        """Show a scrollable list of files on the disk image.

        Returns a list of filenames the user chose to extract, or None.
        """
        import pygame

        clock = pygame.time.Clock()
        win_w, win_h = screen.get_size()

        entries = fs.list_files()
        items: list[dict] = []
        for e in entries:
            if e.ftype == FTYPE_FREE:
                continue
            items.append({
                "name": e.name,
                "type": FTYPE_NAMES.get(e.ftype, f"?{e.ftype}"),
                "size": e.used_bytes,
                "sectors": e.total_sectors,
                "selected": False,
                "is_dir": e.ftype == FTYPE_DIR,
            })

        scroll = 0
        row_h = font.get_linesize() + 6
        dw = min(600, win_w - 40)
        max_visible = min(len(items), (win_h - 200) // row_h)
        max_visible = max(4, max_visible)
        dh = 80 + max_visible * row_h + 50
        dx = (win_w - dw) // 2
        dy = max(10, (win_h - dh) // 2)
        header_h = 40

        sel_all = False

        while True:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return None
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        return None
                    elif event.key == pygame.K_RETURN:
                        chosen = [it["name"] for it in items if it["selected"]]
                        return chosen if chosen else None
                    elif event.key == pygame.K_a and (
                            pygame.key.get_mods() & pygame.KMOD_CTRL):
                        sel_all = not sel_all
                        for it in items:
                            if not it["is_dir"]:
                                it["selected"] = sel_all
                if event.type == pygame.MOUSEBUTTONDOWN:
                    mx, my = event.pos
                    # Scroll
                    if event.button == 4:  # wheel up
                        scroll = max(0, scroll - 1)
                        continue
                    elif event.button == 5:  # wheel down
                        scroll = min(max(0, len(items) - max_visible),
                                     scroll + 1)
                        continue

                    # Cancel / Extract buttons
                    cancel_rect = pygame.Rect(dx + dw - 200, dy + dh - 42,
                                              90, 28)
                    extract_rect = pygame.Rect(dx + dw - 100, dy + dh - 42,
                                               90, 28)
                    if cancel_rect.collidepoint(mx, my):
                        return None
                    if extract_rect.collidepoint(mx, my):
                        chosen = [it["name"] for it in items
                                  if it["selected"]]
                        return chosen if chosen else None

                    # Row click → toggle selection
                    list_y = dy + header_h + row_h  # after column header
                    for vi in range(max_visible):
                        idx = scroll + vi
                        if idx >= len(items):
                            break
                        ry = list_y + vi * row_h
                        if dx <= mx < dx + dw and ry <= my < ry + row_h:
                            if not items[idx]["is_dir"]:
                                items[idx]["selected"] = not items[idx]["selected"]

            # ── Draw ──────────────────────────────────
            overlay = pygame.Surface((win_w, win_h), pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 140))
            screen.blit(overlay, (0, 0))

            pygame.draw.rect(screen, Theme.DIALOG_BG,
                             (dx, dy, dw, dh), border_radius=8)
            pygame.draw.rect(screen, Theme.DROPDOWN_BORDER,
                             (dx, dy, dw, dh), 1, border_radius=8)

            # Title
            n_sel = sum(1 for it in items if it["selected"])
            title = f"Disk Contents  ({len(items)} files"
            if n_sel:
                title += f", {n_sel} selected"
            title += ")"
            t = font.render(title, True, Theme.DIALOG_FG)
            screen.blit(t, (dx + 14, dy + 10))

            # Column headers
            col_y = dy + header_h
            hdr = font.render(
                f"{'':>3} {'Name':<24} {'Type':<8} {'Size':>8}  {'Sec':>5}",
                True, Theme.DEBUG_LABEL)
            screen.blit(hdr, (dx + 10, col_y))
            pygame.draw.line(screen, Theme.MENU_SEP,
                             (dx + 8, col_y + row_h - 2),
                             (dx + dw - 8, col_y + row_h - 2))

            # Rows
            list_y = col_y + row_h
            for vi in range(max_visible):
                idx = scroll + vi
                if idx >= len(items):
                    break
                it = items[idx]
                ry = list_y + vi * row_h
                # Highlight selected
                if it["selected"]:
                    pygame.draw.rect(screen, (40, 70, 120),
                                     (dx + 4, ry, dw - 8, row_h),
                                     border_radius=3)
                # Hover
                mouse_pos = pygame.mouse.get_pos()
                row_rect = pygame.Rect(dx + 4, ry, dw - 8, row_h)
                if row_rect.collidepoint(mouse_pos):
                    pygame.draw.rect(screen, (50, 50, 70),
                                     row_rect, 1, border_radius=3)

                chk = "\u2713" if it["selected"] else " "
                is_dir = "[dir]" if it["is_dir"] else ""
                line = (f" {chk}  {it['name']:<24} "
                        f"{it['type']:<8} {it['size']:>8}  "
                        f"{it['sectors']:>5}  {is_dir}")
                fg = Theme.DEBUG_ADDR if it["is_dir"] else Theme.DIALOG_FG
                rt = font.render(line, True, fg)
                screen.blit(rt, (dx + 10, ry + 2))

            # Scroll indicator
            if len(items) > max_visible:
                bar_h = max(10, int(dh * max_visible / len(items)))
                bar_y = dy + header_h + int(
                    (dh - header_h - 50) * scroll /
                    max(1, len(items) - max_visible))
                pygame.draw.rect(screen, Theme.MENU_SEP,
                                 (dx + dw - 10, bar_y, 6, bar_h),
                                 border_radius=3)

            # Buttons
            cancel_rect = pygame.Rect(dx + dw - 200, dy + dh - 42, 90, 28)
            extract_rect = pygame.Rect(dx + dw - 100, dy + dh - 42, 90, 28)

            pygame.draw.rect(screen, Theme.MENU_SEP, cancel_rect,
                             border_radius=4)
            ct = font.render("Cancel", True, Theme.DIALOG_FG)
            screen.blit(ct, (cancel_rect.x + (90 - ct.get_width()) // 2,
                             cancel_rect.y + 5))

            btn_col = Theme.DIALOG_BTN if n_sel else Theme.MENU_SEP
            pygame.draw.rect(screen, btn_col, extract_rect,
                             border_radius=4)
            et = font.render("Extract", True, Theme.DIALOG_BTN_FG)
            screen.blit(et, (extract_rect.x + (90 - et.get_width()) // 2,
                             extract_rect.y + 5))

            # Hint
            hint = font.render(
                "Click rows to select, Ctrl+A toggle all, "
                "Enter=extract, Esc=cancel",
                True, Theme.STATUS_FG)
            screen.blit(hint, (dx + 14, dy + dh - 18))

            pygame.display.flip()
            clock.tick(30)


# ── Status Bar ────────────────────────────────────────────────────────


class StatusBar:
    """Bottom status bar showing system metrics."""

    def __init__(self):
        self.cycles = 0
        self.mips = 0.0
        self.fps = 0.0
        self.core_states: list[str] = []
        self.message = ""
        self.message_time = 0.0
        self.paused = False

    def set_message(self, msg: str, duration: float = 3.0):
        self.message = msg
        self.message_time = time.time() + duration

    def draw(self, pygame_module, screen, font, win_w: int, win_h: int, 
             status_h: int = 22):
        y = win_h - status_h
        pygame_module.draw.rect(screen, Theme.STATUS_BG,
                                (0, y, win_w, status_h))
        pygame_module.draw.line(screen, Theme.MENU_SEP,
                                (0, y), (win_w, y))

        x_off = 8

        ty = y + (status_h - font.get_linesize()) // 2

        # Paused indicator
        if self.paused:
            pt = font.render(" PAUSED ", True, Theme.DIALOG_BTN_FG, Theme.STATUS_WARN)
            screen.blit(pt, (x_off, ty))
            x_off += pt.get_width() + 8

        # Core state indicators
        for i, state in enumerate(self.core_states[:4]):
            color = Theme.STATUS_OK if state == "run" else (
                Theme.STATUS_WARN if state == "idle" else Theme.STATUS_ERR)
            ct = font.render(f"C{i}:{state}", True, color)
            screen.blit(ct, (x_off, ty))
            x_off += ct.get_width() + 8

        # Separator
        sep_t = font.render("|", True, Theme.MENU_SEP)
        screen.blit(sep_t, (x_off, ty))
        x_off += sep_t.get_width() + 6

        # Cycles
        cyc_t = font.render(f"Cycles: {self.cycles:,}", True, Theme.STATUS_FG)
        screen.blit(cyc_t, (x_off, ty))
        x_off += cyc_t.get_width() + 8

        # MIPS
        if self.mips > 0:
            mips_t = font.render(f"{self.mips:.1f} MIPS", True,
                                 Theme.STATUS_OK if self.mips > 1 else Theme.STATUS_FG)
            screen.blit(mips_t, (x_off, ty))

        # Right side: message + FPS
        right_parts = []
        if time.time() < self.message_time:
            right_parts.append(self.message)
        right_parts.append(f"FPS: {self.fps:.0f}")
        right_text = "  |  ".join(right_parts)
        rt = font.render(right_text, True, Theme.STATUS_FG)
        screen.blit(rt, (win_w - rt.get_width() - 8, ty))


# ── Debug Panel ───────────────────────────────────────────────────────


class DebugPanel:
    """Register / disassembly / memory view panel."""

    def __init__(self, ui_scale: int = 1):
        self.visible = False
        self.width = int(380 * max(1, ui_scale * 0.75))

    def draw(self, pygame_module, screen, font, mono_font,
             sys_emu: "MegapadSystem", x: int, y: int, w: int, h: int):
        """Draw the debug panel."""
        if not self.visible:
            return

        pygame_module.draw.rect(screen, Theme.DEBUG_BG, (x, y, w, h))
        pygame_module.draw.line(screen, Theme.MENU_SEP, (x, y), (x, y + h))

        cpu = sys_emu.cpu
        cy = y + 4
        line_h = mono_font.get_linesize()
        pad = x + 8

        # Section: Registers
        lbl = font.render("REGISTERS", True, Theme.DEBUG_LABEL)
        screen.blit(lbl, (pad, cy))
        cy += line_h + 2

        for i in range(16):
            if cy + line_h > y + h - 4:
                break
            val = cpu.regs[i]
            tag = ""
            if i == cpu.psel:
                tag = " <PC"
            if i == cpu.xsel:
                tag += " <X"
            if i == cpu.spsel:
                tag += " <SP"
            color = Theme.DEBUG_PC if i == cpu.psel else Theme.DEBUG_VALUE
            rt = mono_font.render(f"R{i:<2d}= {val:#018x}{tag}", True, color)
            screen.blit(rt, (pad, cy))
            cy += line_h

        cy += 4
        if cy + line_h * 4 > y + h:
            return

        # Section: Flags
        flags = f"Z={cpu.flag_z} C={cpu.flag_c} N={cpu.flag_n} V={cpu.flag_v}"
        flags2 = f"P={cpu.flag_p} G={cpu.flag_g} I={cpu.flag_i} S={cpu.flag_s}"
        ft = mono_font.render(flags, True, Theme.DEBUG_VALUE)
        screen.blit(ft, (pad, cy))
        cy += line_h
        ft2 = mono_font.render(flags2, True, Theme.DEBUG_VALUE)
        screen.blit(ft2, (pad, cy))
        cy += line_h

        # D, Q, Priv
        misc = f"D={cpu.d_reg:#04x}  Q={cpu.q_out}  Priv={cpu.priv_level}"
        mt = mono_font.render(misc, True, Theme.DEBUG_VALUE)
        screen.blit(mt, (pad, cy))
        cy += line_h + 6

        if cy + line_h * 8 > y + h:
            return

        # Section: Disassembly around PC
        lbl2 = font.render("DISASSEMBLY", True, Theme.DEBUG_LABEL)
        screen.blit(lbl2, (pad, cy))
        cy += line_h + 2

        try:
            from cli import disasm_one
            pc_val = cpu.pc
            addr = max(0, pc_val - 10)
            for _ in range(14):
                if cy + line_h > y + h - 4 or addr >= cpu.mem_size:
                    break
                try:
                    text_str, size = disasm_one(cpu.mem, addr, cpu.mem_size)
                except Exception:
                    text_str, size = "???", 1

                is_pc = (addr == pc_val)
                prefix = "\u25b6" if is_pc else " "
                color = Theme.DEBUG_PC if is_pc else Theme.DEBUG_HEX
                line = f"{prefix}{addr:06x}: {text_str}"
                dt = mono_font.render(line, True, color)
                screen.blit(dt, (pad, cy))
                cy += line_h
                addr += size
        except ImportError:
            pass

        cy += 6
        if cy + line_h * 4 > y + h:
            return

        # Section: Memory around SP
        lbl3 = font.render("STACK (top 8)", True, Theme.DEBUG_LABEL)
        screen.blit(lbl3, (pad, cy))
        cy += line_h + 2

        sp_val = cpu.sp if hasattr(cpu, 'sp') else 0
        for i in range(min(8, (y + h - cy) // line_h)):
            a = (sp_val + i * 8) % cpu.mem_size
            if a + 8 <= cpu.mem_size:
                val = int.from_bytes(cpu.mem[a:a+8], 'little')
            else:
                val = 0
            prefix = "\u25b6" if i == 0 else " "
            line = f"{prefix}{a:06x}: {val:#018x}"
            st = mono_font.render(line, True, Theme.DEBUG_ADDR)
            screen.blit(st, (pad, cy))
            cy += line_h


# ── Snapshot (save/load) ──────────────────────────────────────────────


def save_snapshot(sys_emu: "MegapadSystem", path: str):
    """Save a complete system snapshot to disk.

    Format: "MP64SNAP" magic + lengths + JSON metadata + zlib-compressed memory.
    File extension: .mp64
    """
    cpu = sys_emu.cpu

    # CPU state
    state = {
        "version": 1,
        "ram_size": sys_emu.ram_size,
        "hbw_size": sys_emu.hbw_size,
        "ext_mem_size": sys_emu.ext_mem_size,
        "num_cores": sys_emu.num_full_cores,
        "num_clusters": sys_emu.num_clusters,
        "cores": [],
        "timer": {
            "counter": sys_emu.timer.counter,
            "compare": sys_emu.timer.compare,
            "control": sys_emu.timer.control,
            "prescaler": sys_emu.timer.prescaler,
        },
        "uart": {
            "control": sys_emu.uart.control,
            "rx_buffer": list(sys_emu.uart.rx_buffer),
        },
        "storage": {
            "image_path": sys_emu.storage.image_path,
        },
        "fb": {
            "enable": sys_emu.fb.enable,
            "width": sys_emu.fb.width,
            "height": sys_emu.fb.height,
            "mode": sys_emu.fb.mode,
            "fb_base": sys_emu.fb.fb_base,
            "stride": sys_emu.fb.stride,
        },
    }

    # Per-core state
    for i, core in enumerate(sys_emu.cores[:sys_emu.num_full_cores]):
        cs = {
            "core_id": core.core_id,
            "regs": list(core.regs),
            "psel": core.psel,
            "xsel": core.xsel,
            "spsel": core.spsel,
            "flag_z": core.flag_z, "flag_c": core.flag_c,
            "flag_n": core.flag_n, "flag_v": core.flag_v,
            "flag_p": core.flag_p, "flag_g": core.flag_g,
            "flag_i": core.flag_i, "flag_s": core.flag_s,
            "d_reg": core.d_reg, "q_out": core.q_out, "t_reg": core.t_reg,
            "ivt_base": core.ivt_base, "ivec_id": core.ivec_id,
            "priv_level": core.priv_level,
            "mpu_base": core.mpu_base, "mpu_limit": core.mpu_limit,
            "halted": core.halted, "idle": core.idle,
            "cycle_count": core.cycle_count,
            "sb": core.sb, "sr": core.sr, "sc": core.sc, "sw": core.sw,
            "tmode": core.tmode, "tctrl": core.tctrl,
            "tsrc0": core.tsrc0, "tsrc1": core.tsrc1, "tdst": core.tdst,
            "acc": list(core.acc),
            "tstride_r": core.tstride_r,
            "ttile_h": core.ttile_h, "ttile_w": core.ttile_w,
        }
        state["cores"].append(cs)

    # Write: JSON header + compressed memory blobs
    meta_bytes = json.dumps(state, indent=2).encode('utf-8')
    ram_compressed = zlib.compress(bytes(sys_emu._shared_mem), level=6)
    hbw_compressed = zlib.compress(bytes(sys_emu._hbw_mem), level=6) if sys_emu.hbw_size > 0 else b''
    ext_compressed = zlib.compress(bytes(sys_emu._ext_mem), level=6) if sys_emu.ext_mem_size > 0 else b''

    with open(path, 'wb') as f:
        f.write(b'MP64SNAP')
        f.write(struct.pack('<IIII',
                            len(meta_bytes),
                            len(ram_compressed),
                            len(hbw_compressed),
                            len(ext_compressed)))
        f.write(meta_bytes)
        f.write(ram_compressed)
        f.write(hbw_compressed)
        f.write(ext_compressed)


def load_snapshot(sys_emu: "MegapadSystem", path: str) -> bool:
    """Load a system snapshot from disk. Returns True on success."""
    try:
        with open(path, 'rb') as f:
            magic = f.read(8)
            if magic != b'MP64SNAP':
                print(f"[snapshot] Invalid file: bad magic")
                return False

            meta_len, ram_len, hbw_len, ext_len = struct.unpack('<IIII', f.read(16))
            meta_bytes = f.read(meta_len)
            ram_compressed = f.read(ram_len)
            hbw_compressed = f.read(hbw_len) if hbw_len > 0 else b''
            ext_compressed = f.read(ext_len) if ext_len > 0 else b''

        state = json.loads(meta_bytes.decode('utf-8'))

        # Restore RAM
        ram_data = zlib.decompress(ram_compressed)
        if len(ram_data) <= len(sys_emu._shared_mem):
            sys_emu._shared_mem[:len(ram_data)] = ram_data

        # Restore HBW
        if hbw_compressed and sys_emu.hbw_size > 0:
            hbw_data = zlib.decompress(hbw_compressed)
            if len(hbw_data) <= len(sys_emu._hbw_mem):
                sys_emu._hbw_mem[:len(hbw_data)] = hbw_data

        # Restore ext mem
        if ext_compressed and sys_emu.ext_mem_size > 0:
            ext_data = zlib.decompress(ext_compressed)
            if len(ext_data) <= len(sys_emu._ext_mem):
                sys_emu._ext_mem[:len(ext_data)] = ext_data

        # Restore per-core state
        for cs_data in state.get("cores", []):
            core_id = cs_data["core_id"]
            if core_id < len(sys_emu.cores):
                core = sys_emu.cores[core_id]
                for i, v in enumerate(cs_data["regs"]):
                    core.regs[i] = v
                core.psel = cs_data["psel"]
                core.xsel = cs_data["xsel"]
                core.spsel = cs_data["spsel"]
                core.flag_z = cs_data["flag_z"]
                core.flag_c = cs_data["flag_c"]
                core.flag_n = cs_data["flag_n"]
                core.flag_v = cs_data["flag_v"]
                core.flag_p = cs_data["flag_p"]
                core.flag_g = cs_data["flag_g"]
                core.flag_i = cs_data["flag_i"]
                core.flag_s = cs_data["flag_s"]
                core.d_reg = cs_data["d_reg"]
                core.q_out = cs_data["q_out"]
                core.t_reg = cs_data["t_reg"]
                core.ivt_base = cs_data["ivt_base"]
                core.ivec_id = cs_data["ivec_id"]
                core.priv_level = cs_data["priv_level"]
                core.mpu_base = cs_data["mpu_base"]
                core.mpu_limit = cs_data["mpu_limit"]
                core.halted = cs_data["halted"]
                core.idle = cs_data["idle"]
                core.cycle_count = cs_data["cycle_count"]
                core.sb = cs_data["sb"]
                core.sr = cs_data["sr"]
                core.sc = cs_data["sc"]
                core.sw = cs_data["sw"]
                core.tmode = cs_data["tmode"]
                core.tctrl = cs_data["tctrl"]
                core.tsrc0 = cs_data["tsrc0"]
                core.tsrc1 = cs_data["tsrc1"]
                core.tdst = cs_data["tdst"]
                for j, v in enumerate(cs_data.get("acc", [])):
                    core.acc[j] = v
                core.tstride_r = cs_data.get("tstride_r", 0)
                core.ttile_h = cs_data.get("ttile_h", 8)
                core.ttile_w = cs_data.get("ttile_w", 8)

        # Restore timer
        timer_data = state.get("timer", {})
        sys_emu.timer.counter = timer_data.get("counter", 0)
        sys_emu.timer.compare = timer_data.get("compare", 0)
        sys_emu.timer.control = timer_data.get("control", 0)
        sys_emu.timer.prescaler = timer_data.get("prescaler", 0)

        # Restore FB config
        fb_data = state.get("fb", {})
        sys_emu.fb.enable = fb_data.get("enable", 0)
        sys_emu.fb.width = fb_data.get("width", 320)
        sys_emu.fb.height = fb_data.get("height", 240)
        sys_emu.fb.mode = fb_data.get("mode", 0)
        sys_emu.fb.fb_base = fb_data.get("fb_base", 0)
        sys_emu.fb.stride = fb_data.get("stride", 320)

        sys_emu._booted = True
        return True

    except Exception as e:
        print(f"[snapshot] Error loading {path}: {e}")
        return False


# ── Main Display ──────────────────────────────────────────────────────


class FramebufferDisplay:
    """Full GUI display for the Megapad-64 emulator.

    Features:
      - Menu bar (File, Machine, View)
      - Tab bar (Terminal, Graphics)
      - Optional debug panel (registers, disassembly, stack)
      - Status bar (cycles, MIPS, FPS, core states)
      - Keyboard input -> UART
      - Snapshot save/load
    """

    TAB_TERMINAL = 0
    TAB_GRAPHICS = 1

    def __init__(self, sys_emu: "MegapadSystem", scale: int = 2,
                 title: str = "Megapad-64",
                 on_close: Optional[callable] = None):
        self.sys = sys_emu
        self.scale = max(1, scale)
        self.title = title
        self.on_close = on_close  # called when window is closed
        self._thread: threading.Thread | None = None
        self._stop_event = threading.Event()
        self._started = threading.Event()
        self.fps = 30
        self.active_tab = self.TAB_TERMINAL
        self.term = VirtualTerminal(TERM_COLS, TERM_ROWS)
        self.status = StatusBar()
        self.debug = DebugPanel(ui_scale=self.scale)
        self._paused = False
        self._last_cycles = 0
        self._last_time = 0.0
        self._pending_action: Optional[str] = None
        # Compute scaled chrome sizes
        self._ui_scale = max(1.0, self.scale * 0.6)
        self._menu_h = int(_BASE_MENU_HEIGHT * self._ui_scale)
        self._tab_h = int(_BASE_TAB_HEIGHT * self._ui_scale)
        self._status_h = int(_BASE_STATUS_HEIGHT * self._ui_scale)

    # -- public API -------------------------------------------------------

    def start(self):
        """Start the display thread.  Returns once the window is open."""
        self.sys.uart._tx_listeners.append(self.term.write)

        self._stop_event.clear()
        self._started.clear()
        self._thread = threading.Thread(target=self._run, daemon=True,
                                        name="mp64-display")
        self._thread.start()
        self._started.wait(timeout=5.0)

    def stop(self):
        """Signal the display thread to shut down and wait for it."""
        self._stop_event.set()
        if self._thread is not None:
            self._thread.join(timeout=3.0)
            self._thread = None
        try:
            self.sys.uart._tx_listeners.remove(self.term.write)
        except ValueError:
            pass

    @property
    def running(self) -> bool:
        return self._thread is not None and self._thread.is_alive()

    @property
    def paused(self) -> bool:
        return self._paused

    # -- Menu callbacks ---------------------------------------------------

    def _cb_save_snapshot(self):
        self._pending_action = "save_snapshot"

    def _cb_load_snapshot(self):
        self._pending_action = "load_snapshot"

    def _cb_quit(self):
        self._stop_event.set()

    def _cb_reset(self):
        self.sys.boot()
        self.status.set_message("System reset")

    def _cb_pause(self):
        self._paused = not self._paused
        self.status.set_message("Paused" if self._paused else "Resumed")

    def _cb_toggle_debug(self):
        self.debug.visible = not self.debug.visible

    def _cb_tab_terminal(self):
        self.active_tab = self.TAB_TERMINAL

    def _cb_tab_graphics(self):
        self.active_tab = self.TAB_GRAPHICS

    # -- Disk callbacks ---------------------------------------------------

    def _cb_disk_swap(self):
        self._pending_action = "disk_swap"

    def _cb_disk_browse(self):
        self._pending_action = "disk_browse"

    def _cb_disk_extract_all(self):
        self._pending_action = "disk_extract_all"

    def _cb_disk_info(self):
        self._pending_action = "disk_info"

    def _cb_disk_save(self):
        self.sys.storage.save_image()
        self.status.set_message("Disk image saved")

    # -- internals --------------------------------------------------------

    def _build_menus(self) -> MenuBar:
        return MenuBar([
            Menu("File", [
                MenuItem("Save Snapshot...", self._cb_save_snapshot, "Ctrl+S"),
                MenuItem("Load Snapshot...", self._cb_load_snapshot, "Ctrl+O"),
                MenuItem("", separator=True),
                MenuItem("Quit", self._cb_quit, "Ctrl+Q"),
            ]),
            Menu("Machine", [
                MenuItem("Reset", self._cb_reset, "Ctrl+R"),
                MenuItem("Pause / Resume", self._cb_pause, "F5"),
                MenuItem("", separator=True),
                MenuItem("Step (1 instruction)", lambda: None, "F10"),
            ]),
            Menu("View", [
                MenuItem("Terminal", self._cb_tab_terminal, "F1"),
                MenuItem("Graphics", self._cb_tab_graphics, "F2"),
                MenuItem("", separator=True),
                MenuItem("Debug Panel", self._cb_toggle_debug, "F3"),
            ]),
            Menu("Disk", [
                MenuItem("Swap Image...", self._cb_disk_swap, "Ctrl+D"),
                MenuItem("Browse / Extract...", self._cb_disk_browse, "Ctrl+E"),
                MenuItem("Extract All...", self._cb_disk_extract_all),
                MenuItem("", separator=True),
                MenuItem("Disk Info", self._cb_disk_info),
                MenuItem("Save Image", self._cb_disk_save),
            ]),
        ])

    def _run(self):
        """Main display loop (runs in background thread)."""
        import pygame

        fb = self.sys.fb
        pygame.init()
        pygame.key.set_repeat(400, 35)  # 400ms delay, then repeat every 35ms
        pygame.display.set_caption(self.title)

        # Fonts — scale UI text with the display scale
        term_font_size = max(12, 14 * self.scale)
        term_font = pygame.font.SysFont("monospace", term_font_size)
        test = term_font.render("M", False, (255, 255, 255))
        cell_w = test.get_width()
        cell_h = term_font.get_linesize()

        ui_font_size = max(12, int(13 * self._ui_scale))
        ui_font = pygame.font.SysFont("sans", ui_font_size)
        mono_font_size = max(11, int(12 * self._ui_scale))
        mono_font = pygame.font.SysFont("monospace", mono_font_size)

        menu_h = self._menu_h
        tab_h = self._tab_h
        status_h = self._status_h

        # Window dimensions
        term_pixel_w = self.term.cols * cell_w
        term_pixel_h = self.term.rows * cell_h
        chrome_h = menu_h + tab_h + status_h
        content_w = term_pixel_w
        win_w = content_w + (self.debug.width if self.debug.visible else 0)
        win_h = term_pixel_h + chrome_h

        screen = pygame.display.set_mode((win_w, win_h), pygame.RESIZABLE)
        clock = pygame.time.Clock()

        # Build menus
        menubar = self._build_menus()
        menubar.layout(pygame, ui_font, win_w)

        # Framebuffer surface
        fb_surface = pygame.Surface((320, 240))
        last_w, last_h, last_mode = 0, 0, -1

        # Glyph cache
        glyph_cache: dict = {}
        term_surf_cached = None
        last_cursor_blink = True

        # Cursor blink
        cursor_blink = True
        last_blink = pygame.time.get_ticks()

        # MIPS tracking
        self._last_cycles = self.sys.cpu.cycle_count
        self._last_time = time.time()

        self._started.set()

        try:
            while not self._stop_event.is_set():
                # ── Handle pending cross-thread actions ───────
                if self._pending_action == "save_snapshot":
                    self._pending_action = None
                    path = SimpleDialog.file_save(pygame, screen, ui_font,
                                                   "snapshot.mp64")
                    if path:
                        try:
                            save_snapshot(self.sys, path)
                            self.status.set_message(f"Saved: {path}")
                        except Exception as e:
                            self.status.set_message(f"Save failed: {e}")

                elif self._pending_action == "load_snapshot":
                    self._pending_action = None
                    path = SimpleDialog.file_open(pygame, screen, ui_font,
                                                    "snapshot.mp64")
                    if path and os.path.exists(path):
                        ok = load_snapshot(self.sys, path)
                        if ok:
                            self.status.set_message(f"Loaded: {path}")
                        else:
                            self.status.set_message(f"Load failed: {path}")
                    elif path:
                        self.status.set_message(f"File not found: {path}")

                elif self._pending_action == "disk_swap":
                    self._pending_action = None
                    cur = self.sys.storage.image_path or ""
                    new_path = SimpleDialog.disk_swap(
                        pygame, screen, ui_font, cur)
                    if new_path:
                        try:
                            old = self.sys.storage.image_path or "(none)"
                            self.sys.storage.swap_image(new_path)
                            self.sys.sysinfo.has_storage = True
                            self.status.set_message(
                                f"Swapped: {old} \u2192 {new_path}")
                        except Exception as e:
                            self.status.set_message(f"Swap failed: {e}")

                elif self._pending_action == "disk_browse":
                    self._pending_action = None
                    if not self.sys.storage._image_data:
                        self.status.set_message("No disk image attached")
                    else:
                        fs = MP64FS(bytearray(self.sys.storage._image_data))
                        chosen = SimpleDialog.disk_browser(
                            pygame, screen, ui_font, fs)
                        if chosen:
                            dest_str = SimpleDialog.pick_directory(
                                pygame, screen, ui_font,
                                default="extracted",
                                title="Extract Selected Files To")
                            if dest_str is None:
                                continue
                            dest = Path(dest_str)
                            dest.mkdir(parents=True, exist_ok=True)
                            count = 0
                            for name in chosen:
                                try:
                                    data = fs.read_file(name)
                                    data = data.rstrip(b"\x00")
                                    (dest / name).write_bytes(data)
                                    count += 1
                                except Exception:
                                    pass
                            self.status.set_message(
                                f"Extracted {count} file(s) \u2192 {dest}/")

                elif self._pending_action == "disk_extract_all":
                    self._pending_action = None
                    if not self.sys.storage._image_data:
                        self.status.set_message("No disk image attached")
                    else:
                        fs = MP64FS(bytearray(self.sys.storage._image_data))
                        dest_str = SimpleDialog.pick_directory(
                            pygame, screen, ui_font,
                            default="extracted",
                            title="Extract All Files To")
                        if dest_str is None:
                            continue
                        dest = Path(dest_str)
                        dest.mkdir(parents=True, exist_ok=True)
                        count = 0
                        for e in fs.list_files():
                            if e.ftype in (FTYPE_FREE, FTYPE_DIR):
                                continue
                            try:
                                data = fs.read_file(e.name, parent=e.parent)
                                data = data.rstrip(b"\x00")
                                (dest / e.name).write_bytes(data)
                                count += 1
                            except Exception:
                                pass
                        self.status.set_message(
                            f"Extracted all ({count} files) \u2192 {dest}/")

                elif self._pending_action == "disk_info":
                    self._pending_action = None
                    s = self.sys.storage
                    if not s._image_data:
                        self.status.set_message("No disk image attached")
                    else:
                        fs = MP64FS(bytearray(s._image_data))
                        meta = fs.info()
                        if meta.get("formatted"):
                            self.status.set_message(
                                f"Disk: {s.image_path}  "
                                f"{meta['files']} files  "
                                f"{meta['free_sectors']} free sectors")
                        else:
                            self.status.set_message(
                                f"Disk: {s.image_path}  "
                                f"({s.total_sectors} sectors, unformatted)")

                # ── Handle pygame events ──────────────────────
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        self._stop_event.set()
                        return
                    elif event.type == pygame.VIDEORESIZE:
                        win_w, win_h = event.w, event.h
                        screen = pygame.display.set_mode((win_w, win_h),
                                                          pygame.RESIZABLE)
                        menubar.layout(pygame, ui_font, win_w)
                    elif event.type == pygame.KEYDOWN:
                        mods = pygame.key.get_mods()
                        ctrl = mods & pygame.KMOD_CTRL

                        if ctrl and event.key == pygame.K_q:
                            self._stop_event.set()
                            return
                        elif ctrl and event.key == pygame.K_s:
                            self._cb_save_snapshot()
                        elif ctrl and event.key == pygame.K_o:
                            self._cb_load_snapshot()
                        elif ctrl and event.key == pygame.K_d:
                            self._cb_disk_swap()
                        elif ctrl and event.key == pygame.K_e:
                            self._cb_disk_browse()
                        elif ctrl and event.key == pygame.K_r:
                            self._cb_reset()
                        elif event.key == pygame.K_F1:
                            self.active_tab = self.TAB_TERMINAL
                            menubar.active_menu = -1
                        elif event.key == pygame.K_F2:
                            self.active_tab = self.TAB_GRAPHICS
                            menubar.active_menu = -1
                        elif event.key == pygame.K_F3:
                            self._cb_toggle_debug()
                            debug_w = self.debug.width if self.debug.visible else 0
                            new_w = content_w + debug_w
                            win_w = new_w
                            screen = pygame.display.set_mode(
                                (win_w, win_h), pygame.RESIZABLE)
                            menubar.layout(pygame, ui_font, win_w)
                        elif event.key == pygame.K_F5:
                            self._cb_pause()
                        elif event.key == pygame.K_F10:
                            try:
                                self.sys.step()
                                self.status.set_message("Stepped 1 instruction")
                            except Exception:
                                pass
                        elif not menubar.is_open():
                            # Forward keys to UART
                            if event.unicode and ord(event.unicode) < 128:
                                ch = event.unicode.encode('ascii', 'ignore')
                                if ch:
                                    self.sys.uart.inject_input(ch)
                            elif event.key == pygame.K_RETURN:
                                self.sys.uart.inject_input(b'\r')
                            elif event.key == pygame.K_BACKSPACE:
                                self.sys.uart.inject_input(b'\x08')
                            elif event.key == pygame.K_UP:
                                self.sys.uart.inject_input(b'\x1b[A')
                            elif event.key == pygame.K_DOWN:
                                self.sys.uart.inject_input(b'\x1b[B')
                            elif event.key == pygame.K_RIGHT:
                                self.sys.uart.inject_input(b'\x1b[C')
                            elif event.key == pygame.K_LEFT:
                                self.sys.uart.inject_input(b'\x1b[D')
                            elif event.key == pygame.K_TAB:
                                self.sys.uart.inject_input(b'\x09')
                            elif event.key == pygame.K_DELETE:
                                self.sys.uart.inject_input(b'\x7f')
                            elif event.key == pygame.K_HOME:
                                self.sys.uart.inject_input(b'\x1b[H')
                            elif event.key == pygame.K_END:
                                self.sys.uart.inject_input(b'\x1b[F')
                            elif event.key == pygame.K_PAGEUP:
                                self.sys.uart.inject_input(b'\x1b[5~')
                            elif event.key == pygame.K_PAGEDOWN:
                                self.sys.uart.inject_input(b'\x1b[6~')

                    elif event.type == pygame.MOUSEBUTTONDOWN:
                        mx, my = event.pos
                        if my < menu_h or menubar.is_open():
                            cb = menubar.handle_click(mx, my, font=ui_font)
                            if cb:
                                cb()
                        elif menu_h <= my < menu_h + tab_h:
                            debug_w = self.debug.width if self.debug.visible else 0
                            tab_w = (win_w - debug_w) // 2
                            if mx < tab_w:
                                self.active_tab = self.TAB_TERMINAL
                            elif mx < tab_w * 2:
                                self.active_tab = self.TAB_GRAPHICS

                # ── Update status metrics ─────────────────────
                now = time.time()
                if now - self._last_time >= 1.0:
                    current_cycles = self.sys.cpu.cycle_count
                    delta = current_cycles - self._last_cycles
                    elapsed = now - self._last_time
                    self.status.mips = (delta / elapsed) / 1e6 if elapsed > 0 else 0
                    self._last_cycles = current_cycles
                    self._last_time = now

                self.status.cycles = self.sys.cpu.cycle_count
                self.status.fps = clock.get_fps()
                self.status.paused = self._paused

                # Core states
                states = []
                for core in self.sys.cores[:4]:
                    if core.halted:
                        states.append("halt")
                    elif core.idle:
                        states.append("idle")
                    else:
                        states.append("run")
                self.status.core_states = states

                # Cursor blink
                tick_now = pygame.time.get_ticks()
                if tick_now - last_blink >= CURSOR_BLINK_MS:
                    cursor_blink = not cursor_blink
                    last_blink = tick_now

                # ── Render ────────────────────────────────────
                screen.fill(Theme.BG)

                debug_w = self.debug.width if self.debug.visible else 0
                content_area_w = win_w - debug_w
                tab_y = menu_h
                content_y = menu_h + tab_h
                content_h = win_h - chrome_h

                # ── Tab bar ───────────────────────────────────
                self._draw_tabs(pygame, screen, ui_font,
                                content_area_w, tab_h, tab_y)

                # ── Content area ──────────────────────────────
                content_rect = pygame.Rect(0, content_y,
                                           content_area_w, content_h)

                if self.active_tab == self.TAB_TERMINAL:
                    need_render = (self.term._dirty
                                   or cursor_blink != last_cursor_blink
                                   or term_surf_cached is None)
                    if need_render:
                        term_surf_cached = self.term.render(
                            pygame, term_font, cell_w, cell_h,
                            show_cursor=cursor_blink,
                            _cache=glyph_cache)
                        last_cursor_blink = cursor_blink
                    term_surf = term_surf_cached
                    tx = (content_rect.width - term_surf.get_width()) // 2
                    ty = (content_rect.height - term_surf.get_height()) // 2
                    screen.blit(term_surf,
                                (content_rect.x + max(0, tx),
                                 content_rect.y + max(0, ty)))

                elif self.active_tab == self.TAB_GRAPHICS:
                    if not (fb.enable & 1):
                        label = ui_font.render(
                            "Framebuffer disabled  \u2014  use GFX-INIT",
                            True, (100, 100, 120))
                        screen.blit(label,
                                    (content_rect.x + 10,
                                     content_rect.y + 10))
                    else:
                        w, h, mode = fb.width, fb.height, fb.mode
                        if w >= 1 and h >= 1:
                            if (w != last_w or h != last_h
                                    or mode != last_mode):
                                last_w, last_h, last_mode = w, h, mode
                                fb_surface = pygame.Surface((w, h))

                            self._render_fb(fb, fb_surface, w, h, mode)

                            sw = content_rect.width
                            sh = content_rect.height
                            aspect = w / h
                            if sw / sh > aspect:
                                sh2 = sh
                                sw2 = int(sh2 * aspect)
                            else:
                                sw2 = sw
                                sh2 = int(sw2 / aspect)
                            scaled = pygame.transform.scale(
                                fb_surface, (sw2, sh2))
                            ox = (content_rect.width - sw2) // 2
                            oy = (content_rect.height - sh2) // 2
                            screen.blit(scaled,
                                        (content_rect.x + ox,
                                         content_rect.y + oy))

                            fb.vsync_count += 1
                            fb.vblank = True

                # ── Debug panel ───────────────────────────────
                if self.debug.visible:
                    self.debug.draw(pygame, screen, ui_font, mono_font,
                                    self.sys,
                                    content_area_w, content_y,
                                    debug_w, content_h)

                # ── Menu bar (drawn last, on top) ─────────────
                menubar.draw(pygame, screen, ui_font, win_w, menu_h)

                # ── Status bar ────────────────────────────────
                self.status.draw(pygame, screen, ui_font, win_w, win_h,
                                 status_h)

                pygame.display.flip()
                clock.tick(self.fps)

        except Exception as e:
            import traceback
            print(f"\n[display] error: {e}")
            traceback.print_exc()
        finally:
            pygame.quit()
            # Fire the on_close callback (e.g. to exit the CLI process)
            if self.on_close:
                self.on_close()

    def _draw_tabs(self, pygame, screen, font, area_w: int,
                    tab_h: int, y: int):
        """Draw the tab bar."""
        tab_w = area_w // 2
        tabs = [
            ("Terminal [F1]", self.TAB_TERMINAL),
            ("Graphics [F2]", self.TAB_GRAPHICS),
        ]
        for i, (label, tab_id) in enumerate(tabs):
            x = i * tab_w
            active = (self.active_tab == tab_id)
            bg = Theme.TAB_ACTIVE if active else Theme.TAB_BG
            fg = Theme.TAB_ACTIVE_FG if active else Theme.TAB_FG
            pygame.draw.rect(screen, bg, (x, y, tab_w, tab_h))
            if active:
                pygame.draw.rect(screen, Theme.TAB_ACCENT,
                                 (x, y + tab_h - 2, tab_w, 2))
            text = font.render(label, True, fg)
            tx = x + (tab_w - text.get_width()) // 2
            ty_off = (tab_h - text.get_height()) // 2
            screen.blit(text, (tx, y + ty_off))
        pygame.draw.line(screen, (60, 60, 75),
                         (tab_w, y), (tab_w, y + tab_h - 1))

    def _resolve_fb_mem(self, base_addr: int):
        """Return (buffer, offset) for framebuffer base address."""
        # Check VRAM first (dedicated graphics memory)
        if self.sys.vram_size > 0:
            vram_base = self.sys.vram_base
            vram_end = self.sys.vram_end
            if vram_base <= base_addr < vram_end:
                return self.sys._vram_mem, base_addr - vram_base
        if self.sys.ext_mem_size > 0:
            ext_base = self.sys.ext_mem_base
            ext_end = self.sys.ext_mem_end
            if ext_base <= base_addr < ext_end:
                return self.sys._ext_mem, base_addr - ext_base
        if base_addr >= HBW_BASE:
            off = base_addr - HBW_BASE
            if off < len(self.sys._hbw_mem):
                return self.sys._hbw_mem, off
        return None, 0

    def _render_fb(self, fb, surface, w: int, h: int, mode: int):
        """Read pixel data and paint onto a pygame Surface."""
        import pygame
        import numpy as np

        # Fast path: C++ pixel conversion (no GIL, no per-pixel Python)
        if hasattr(fb, '_cs') and hasattr(fb._cs, 'render_fb_rgb'):
            result = fb._cs.render_fb_rgb()
            if result is not None:
                pixels_rgb = np.asarray(result)
                if pixels_rgb.shape == (w, h, 3):
                    pygame.surfarray.blit_array(surface, pixels_rgb)
                    return
            # Fall through to Python path if C++ returned None
            # (e.g. fb_base points to unmapped memory)

        base_addr = fb.fb_base
        stride = fb.stride
        mem, mem_off = self._resolve_fb_mem(base_addr)
        if mem is None:
            return

        if mode == 0:
            # 8-bit indexed color
            pal = fb.palette
            pal_lut = np.zeros((256, 3), dtype=np.uint8)
            for i in range(256):
                rgb = pal[i]
                pal_lut[i, 0] = (rgb >> 16) & 0xFF
                pal_lut[i, 1] = (rgb >> 8) & 0xFF
                pal_lut[i, 2] = rgb & 0xFF
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y_row in range(h):
                row_off = mem_off + y_row * stride
                end = row_off + w
                if end > len(mem):
                    break
                row = np.frombuffer(mem, dtype=np.uint8,
                                    count=w, offset=row_off)
                pixels_rgb[:, y_row, :] = pal_lut[row]
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 1:
            # RGB565
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y_row in range(h):
                row_off = mem_off + y_row * stride
                end = row_off + w * 2
                if end > len(mem):
                    break
                raw = np.frombuffer(mem, dtype=np.uint16,
                                    count=w, offset=row_off)
                pixels_rgb[:, y_row, 0] = ((raw >> 11) & 0x1F).astype(np.uint8) << 3
                pixels_rgb[:, y_row, 1] = ((raw >> 5) & 0x3F).astype(np.uint8) << 2
                pixels_rgb[:, y_row, 2] = (raw & 0x1F).astype(np.uint8) << 3
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 3:
            # RGBA8888
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y_row in range(h):
                row_off = mem_off + y_row * stride
                end = row_off + w * 4
                if end > len(mem):
                    break
                raw = np.frombuffer(mem, dtype=np.uint8,
                                    count=w * 4, offset=row_off).reshape(w, 4)
                pixels_rgb[:, y_row, 0] = raw[:, 0]
                pixels_rgb[:, y_row, 1] = raw[:, 1]
                pixels_rgb[:, y_row, 2] = raw[:, 2]
            pygame.surfarray.blit_array(surface, pixels_rgb)

        else:
            surface.fill((64, 64, 64))


# ── Headless Display (testing) ──────────────────────────────────────


class HeadlessDisplay:
    """No-op display for testing — records framebuffer snapshots."""

    def __init__(self, sys_emu: "MegapadSystem"):
        self.sys = sys_emu
        self.snapshots: list[bytes] = []

    def start(self):
        pass

    def stop(self):
        pass

    def _resolve_fb_mem(self, base_addr: int):
        # Check VRAM first (dedicated graphics memory)
        if self.sys.vram_size > 0:
            vram_base = self.sys.vram_base
            vram_end = self.sys.vram_end
            if vram_base <= base_addr < vram_end:
                return self.sys._vram_mem, base_addr - vram_base
        if self.sys.ext_mem_size > 0:
            ext_base = self.sys.ext_mem_base
            ext_end = self.sys.ext_mem_end
            if ext_base <= base_addr < ext_end:
                return self.sys._ext_mem, base_addr - ext_base
        if base_addr >= HBW_BASE:
            off = base_addr - HBW_BASE
            if off < len(self.sys._hbw_mem):
                return self.sys._hbw_mem, off
        return None, 0

    def snapshot(self) -> bytes | None:
        fb = self.sys.fb
        if not (fb.enable & 1):
            return None
        base_addr = fb.fb_base
        stride = fb.stride
        h = fb.height
        mem, mem_off = self._resolve_fb_mem(base_addr)
        if mem is None:
            return None
        end = mem_off + stride * h
        if end > len(mem):
            return None
        data = bytes(mem[mem_off:end])
        self.snapshots.append(data)
        return data

    @property
    def running(self) -> bool:
        return False
