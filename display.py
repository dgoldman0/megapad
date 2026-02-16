"""
Megapad-64 Framebuffer Display
================================
Reads the FramebufferDevice configuration and HBW pixel data from the
emulator and renders them in a pygame window.  Runs in a background
thread so it doesn't block the UART console loop.

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
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from system import MegapadSystem

# HBW constants (must match system.py)
HBW_BASE = 0xFFD0_0000


class FramebufferDisplay:
    """Background-threaded pygame display for the Megapad-64 framebuffer."""

    def __init__(self, sys_emu: "MegapadSystem", scale: int = 2,
                 title: str = "Megapad-64"):
        self.sys = sys_emu
        self.scale = max(1, scale)
        self.title = title
        self._thread: threading.Thread | None = None
        self._stop_event = threading.Event()
        self._started = threading.Event()
        self.fps = 30

    # -- public API -------------------------------------------------------

    def start(self):
        """Start the display thread.  Returns once the window is open."""
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

        # Start with a small default window; resize when FB changes
        win_w = 320 * self.scale
        win_h = 240 * self.scale
        screen = pygame.display.set_mode((win_w, win_h))
        clock = pygame.time.Clock()

        # Surface for the actual framebuffer pixels (unscaled)
        fb_surface = pygame.Surface((320, 240))
        last_w, last_h, last_mode = 0, 0, -1

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
                        # Forward printable keys to UART
                        if event.unicode and ord(event.unicode) < 128:
                            ch = event.unicode.encode('ascii', 'ignore')
                            if ch:
                                self.sys.uart.inject_input(ch)
                        elif event.key == pygame.K_RETURN:
                            self.sys.uart.inject_input(b'\r')
                        elif event.key == pygame.K_BACKSPACE:
                            self.sys.uart.inject_input(b'\x08')

                # Check if FB is enabled
                if not (fb.enable & 1):
                    # Not enabled — show a dark screen with a label
                    screen.fill((24, 24, 32))
                    font = pygame.font.SysFont("monospace", 14)
                    label = font.render("Framebuffer disabled", True,
                                        (100, 100, 120))
                    screen.blit(label, (10, 10))
                    pygame.display.flip()
                    clock.tick(10)
                    continue

                w, h, mode = fb.width, fb.height, fb.mode
                if w < 1 or h < 1:
                    clock.tick(10)
                    continue

                # Resize window / surface if dimensions changed
                if w != last_w or h != last_h or mode != last_mode:
                    last_w, last_h, last_mode = w, h, mode
                    win_w = w * self.scale
                    win_h = h * self.scale
                    screen = pygame.display.set_mode((win_w, win_h))
                    fb_surface = pygame.Surface((w, h))

                # Read pixel data from HBW memory
                self._render_fb(fb, fb_surface, w, h, mode)

                # Scale and blit
                if self.scale != 1:
                    scaled = pygame.transform.scale(fb_surface, (win_w, win_h))
                    screen.blit(scaled, (0, 0))
                else:
                    screen.blit(fb_surface, (0, 0))

                pygame.display.flip()

                # Tick vsync counter
                fb.vsync_count += 1
                fb.vblank = True

                clock.tick(self.fps)

        except Exception as e:
            print(f"\n[display] error: {e}")
        finally:
            pygame.quit()

    def _render_fb(self, fb, surface, w: int, h: int, mode: int):
        """Read HBW pixel data and paint onto a pygame Surface."""
        import pygame
        import numpy as np

        base_addr = fb.fb_base
        stride = fb.stride
        hbw_mem = self.sys._hbw_mem

        # Compute offset into HBW memory
        hbw_off = base_addr - HBW_BASE
        if hbw_off < 0:
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
                row_off = hbw_off + y * stride
                end = row_off + w
                if end > len(hbw_mem):
                    break
                row = np.frombuffer(hbw_mem, dtype=np.uint8,
                                    count=w, offset=row_off)
                pixels_rgb[:, y, :] = pal_lut[row]
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 1:
            # RGB565 — fast path via numpy
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y in range(h):
                row_off = hbw_off + y * stride
                end = row_off + w * 2
                if end > len(hbw_mem):
                    break
                raw = np.frombuffer(hbw_mem, dtype=np.uint16,
                                    count=w, offset=row_off)
                pixels_rgb[:, y, 0] = ((raw >> 11) & 0x1F).astype(np.uint8) << 3
                pixels_rgb[:, y, 1] = ((raw >> 5) & 0x3F).astype(np.uint8) << 2
                pixels_rgb[:, y, 2] = (raw & 0x1F).astype(np.uint8) << 3
            pygame.surfarray.blit_array(surface, pixels_rgb)

        elif mode == 3:
            # RGBA8888 — fast path via numpy
            pixels_rgb = np.zeros((w, h, 3), dtype=np.uint8)
            for y in range(h):
                row_off = hbw_off + y * stride
                end = row_off + w * 4
                if end > len(hbw_mem):
                    break
                raw = np.frombuffer(hbw_mem, dtype=np.uint8,
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

    def snapshot(self) -> bytes | None:
        """Capture current FB pixel data as raw bytes."""
        fb = self.sys.fb
        if not (fb.enable & 1):
            return None
        base_addr = fb.fb_base
        stride = fb.stride
        h = fb.height
        hbw_off = base_addr - HBW_BASE
        if hbw_off < 0:
            return None
        end = hbw_off + stride * h
        if end > len(self.sys._hbw_mem):
            return None
        data = bytes(self.sys._hbw_mem[hbw_off:end])
        self.snapshots.append(data)
        return data

    @property
    def running(self) -> bool:
        return False
