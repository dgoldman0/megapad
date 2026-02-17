"""
Megapad-64 Framebuffer Display
================================
Reads the FramebufferDevice configuration and pixel data from the
emulator — either external memory or HBW — and renders them in a
pygame window.  Runs in a background thread so it doesn't block the
UART console loop.

The window has two tabs:
  [Terminal]  — virtual VT100-style console mirroring UART output
  [Graphics]  — live view of the hardware framebuffer

Press F1/F2 or click the tab buttons to switch.

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

import threading
import time
import struct
from collections import deque
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from system import MegapadSystem

# Memory map constants (must match system.py)
HBW_BASE = 0xFFD0_0000
EXT_MEM_BASE = 0x0010_0000

# Virtual terminal defaults
TERM_COLS = 80
TERM_ROWS = 30
TAB_HEIGHT = 24          # pixels for tab bar
CURSOR_BLINK_MS = 530    # cursor blink interval


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
               show_cursor: bool = True) -> 'pygame.Surface':
        """Render the terminal grid to a pygame surface."""
        with self._lock:
            surf_w = self.cols * cell_w
            surf_h = self.rows * cell_h
            surface = pygame_module.Surface((surf_w, surf_h))
            surface.fill(self.COLORS[0])

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
                        glyph = font.render(ch, False, self.COLORS[fg])
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


class FramebufferDisplay:
    """Background-threaded pygame display for the Megapad-64 framebuffer.

    Shows two tabs: Terminal (VT100 console) and Graphics (hardware FB).
    """

    TAB_TERMINAL = 0
    TAB_GRAPHICS = 1

    def __init__(self, sys_emu: "MegapadSystem", scale: int = 2,
                 title: str = "Megapad-64"):
        self.sys = sys_emu
        self.scale = max(1, scale)
        self.title = title
        self._thread: threading.Thread | None = None
        self._stop_event = threading.Event()
        self._started = threading.Event()
        self.fps = 30
        self.active_tab = self.TAB_TERMINAL
        self.term = VirtualTerminal(TERM_COLS, TERM_ROWS)

        # Hook UART TX so every byte also goes to the virtual terminal
        self._orig_uart_tx = None

    # -- public API -------------------------------------------------------

    def start(self):
        """Start the display thread.  Returns once the window is open."""
        # Hook UART TX to also feed the virtual terminal
        self._orig_uart_tx = self.sys.uart.on_tx
        orig = self._orig_uart_tx

        def _tee_tx(b):
            self.term.write(b)
            if orig is not None:
                orig(b)

        self.sys.uart.on_tx = _tee_tx

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

    @property
    def running(self) -> bool:
        return self._thread is not None and self._thread.is_alive()

    # -- internals --------------------------------------------------------

    def _run(self):
        """Main display loop (runs in background thread)."""
        import pygame

        fb = self.sys.fb
        pygame.init()
        pygame.display.set_caption(self.title)

        # Monospace font for virtual terminal
        term_font = pygame.font.SysFont("monospace", 14)
        # Measure cell size from font
        test = term_font.render("M", False, (255, 255, 255))
        cell_w = test.get_width()
        cell_h = term_font.get_linesize()

        # Compute window dimensions from terminal size
        term_pixel_w = self.term.cols * cell_w
        term_pixel_h = self.term.rows * cell_h
        content_w = max(term_pixel_w, 320 * self.scale)
        content_h = max(term_pixel_h, 240 * self.scale)
        win_w = content_w
        win_h = content_h + TAB_HEIGHT

        screen = pygame.display.set_mode((win_w, win_h), pygame.RESIZABLE)
        clock = pygame.time.Clock()

        # Tab bar font
        tab_font = pygame.font.SysFont("sans", 14, bold=True)

        # Framebuffer surface (will be resized when FB dimensions change)
        fb_surface = pygame.Surface((320, 240))
        last_w, last_h, last_mode = 0, 0, -1

        # Cursor blink state
        cursor_blink = True
        last_blink = pygame.time.get_ticks()

        self._started.set()

        try:
            while not self._stop_event.is_set():
                # Handle pygame events
                for event in pygame.event.get():
                    if event.type == pygame.QUIT:
                        self._stop_event.set()
                        return
                    elif event.type == pygame.KEYDOWN:
                        if event.key == pygame.K_ESCAPE:
                            self._stop_event.set()
                            return
                        elif event.key == pygame.K_F1:
                            self.active_tab = self.TAB_TERMINAL
                        elif event.key == pygame.K_F2:
                            self.active_tab = self.TAB_GRAPHICS
                        else:
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
                    elif event.type == pygame.MOUSEBUTTONDOWN:
                        # Check tab bar clicks
                        mx, my = event.pos
                        if my < TAB_HEIGHT:
                            if mx < win_w // 2:
                                self.active_tab = self.TAB_TERMINAL
                            else:
                                self.active_tab = self.TAB_GRAPHICS

                # Cursor blink
                now = pygame.time.get_ticks()
                if now - last_blink >= CURSOR_BLINK_MS:
                    cursor_blink = not cursor_blink
                    last_blink = now

                screen.fill((32, 32, 40))

                # ── Draw tab bar ──────────────────────────────────
                self._draw_tabs(pygame, screen, tab_font, win_w)

                # ── Draw content area ─────────────────────────────
                content_rect = pygame.Rect(0, TAB_HEIGHT, win_w,
                                           win_h - TAB_HEIGHT)

                if self.active_tab == self.TAB_TERMINAL:
                    # Render virtual terminal
                    term_surf = self.term.render(
                        pygame, term_font, cell_w, cell_h,
                        show_cursor=cursor_blink)
                    # Center it in the content area
                    tx = (content_rect.width - term_surf.get_width()) // 2
                    ty = (content_rect.height - term_surf.get_height()) // 2
                    screen.blit(term_surf,
                                (content_rect.x + max(0, tx),
                                 content_rect.y + max(0, ty)))

                elif self.active_tab == self.TAB_GRAPHICS:
                    if not (fb.enable & 1):
                        label = tab_font.render(
                            "Framebuffer disabled  —  use GFX-INIT",
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

                            # Scale to fit content area
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

                            # Tick vsync
                            fb.vsync_count += 1
                            fb.vblank = True

                pygame.display.flip()
                clock.tick(self.fps)

        except Exception as e:
            print(f"\n[display] error: {e}")
        finally:
            pygame.quit()

    def _draw_tabs(self, pygame, screen, font, win_w: int):
        """Draw the tab bar at the top of the window."""
        tab_w = win_w // 2
        tabs = [
            ("Terminal [F1]", self.TAB_TERMINAL),
            ("Graphics [F2]", self.TAB_GRAPHICS),
        ]
        for i, (label, tab_id) in enumerate(tabs):
            x = i * tab_w
            active = (self.active_tab == tab_id)
            bg = (50, 50, 65) if active else (30, 30, 38)
            fg = (220, 220, 240) if active else (110, 110, 130)
            pygame.draw.rect(screen, bg, (x, 0, tab_w, TAB_HEIGHT))
            if active:
                # Active tab indicator line
                pygame.draw.rect(screen, (80, 140, 255),
                                 (x, TAB_HEIGHT - 2, tab_w, 2))
            text = font.render(label, True, fg)
            tx = x + (tab_w - text.get_width()) // 2
            ty = (TAB_HEIGHT - text.get_height()) // 2
            screen.blit(text, (tx, ty))
        # Separator line
        pygame.draw.line(screen, (60, 60, 75),
                         (tab_w, 0), (tab_w, TAB_HEIGHT - 1))

    def _resolve_fb_mem(self, base_addr: int):
        """Return (buffer, offset) for the given framebuffer base address.

        Checks external memory first, then HBW.  Returns (None, 0) if
        the address doesn't fall in either region.
        """
        # External memory?
        if self.sys.ext_mem_size > 0:
            ext_base = self.sys.ext_mem_base
            ext_end = self.sys.ext_mem_end
            if ext_base <= base_addr < ext_end:
                return self.sys._ext_mem, base_addr - ext_base
        # HBW?
        if base_addr >= HBW_BASE:
            off = base_addr - HBW_BASE
            if off < len(self.sys._hbw_mem):
                return self.sys._hbw_mem, off
        return None, 0

    def _render_fb(self, fb, surface, w: int, h: int, mode: int):
        """Read pixel data from ext mem or HBW and paint onto a pygame Surface."""
        import pygame
        import numpy as np

        base_addr = fb.fb_base
        stride = fb.stride
        mem, mem_off = self._resolve_fb_mem(base_addr)
        if mem is None:
            return

        if mode == 0:
            # 8-bit indexed color — fast path via numpy
            pal = fb.palette  # list of 0x00RRGGBB ints
            # Build palette lookup table (256 × 3 uint8)
            pal_lut = np.zeros((256, 3), dtype=np.uint8)
            for i in range(256):
                rgb = pal[i]
                pal_lut[i, 0] = (rgb >> 16) & 0xFF
                pal_lut[i, 1] = (rgb >> 8) & 0xFF
                pal_lut[i, 2] = rgb & 0xFF
            # Read raw index bytes for entire frame
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y in range(h):
                row_off = mem_off + y * stride
                end = row_off + w
                if end > len(mem):
                    break
                row = np.frombuffer(mem, dtype=np.uint8,
                                    count=w, offset=row_off)
                pixels_rgb[:, y, :] = pal_lut[row]
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 1:
            # RGB565 — fast path via numpy
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y in range(h):
                row_off = mem_off + y * stride
                end = row_off + w * 2
                if end > len(mem):
                    break
                raw = np.frombuffer(mem, dtype=np.uint16,
                                    count=w, offset=row_off)
                pixels_rgb[:, y, 0] = ((raw >> 11) & 0x1F).astype(np.uint8) << 3
                pixels_rgb[:, y, 1] = ((raw >> 5) & 0x3F).astype(np.uint8) << 2
                pixels_rgb[:, y, 2] = (raw & 0x1F).astype(np.uint8) << 3
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 3:
            # RGBA8888 — fast path via numpy
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y in range(h):
                row_off = mem_off + y * stride
                end = row_off + w * 4
                if end > len(mem):
                    break
                raw = np.frombuffer(mem, dtype=np.uint8,
                                    count=w * 4, offset=row_off).reshape(w, 4)
                pixels_rgb[:, y, 0] = raw[:, 0]
                pixels_rgb[:, y, 1] = raw[:, 1]
                pixels_rgb[:, y, 2] = raw[:, 2]
            pygame.surfarray.blit_array(surface, pixels_rgb)

        else:
            # Unsupported mode — fill gray
            surface.fill((64, 64, 64))


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
        """Return (buffer, offset) — same logic as FramebufferDisplay."""
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
        """Capture current FB pixel data as raw bytes."""
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
