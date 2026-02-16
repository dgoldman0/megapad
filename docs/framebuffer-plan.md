# Framebuffer & Module System — Plan of Action

**Branch:** `feature/framebuffer`
**Started:** February 2026
**Goal:** Add a memory-mapped framebuffer peripheral, a KDOS module
system (`REQUIRE`), and a graphics library — all headless by default,
with optional SDL2 display in the emulator.

---

## Design Principles

1. **Headless first** — the framebuffer is just another MMIO device.
   Software writes pixels to HBW RAM; a scanout device reads them.
   With no display attached, the device still works (testable,
   benchmarkable).
2. **Tile-native** — pixel formats align with tile engine element
   widths.  8bpp indexed = 64 pixels/tile op.  FP16 grayscale =
   32 pixels/tile op.  No format conversion needed between tile
   output and framebuffer input.
3. **No GPU** — the CPU + tile engine *are* the graphics processor.
   The framebuffer device is a dumb scanout, not a command processor.
4. **Modular** — graphics is a KDOS module loaded from disk via
   `REQUIRE`, not compiled into the kernel.  Systems that don't
   need graphics don't pay for it.
5. **RTL-consistent** — every MMIO register in the emulator has a
   corresponding RTL implementation.  Register map is defined once
   and shared.

---

## MMIO Register Map

**Base offset:** `+0x0A00` (next available slot; KEM ends at `+0x0940`)

| Register     | Offset  | R/W | Width | Description                          |
|-------------|---------|-----|-------|--------------------------------------|
| FB_BASE     | +0x00   | RW  | 64b   | Pixel data start address (in HBW)    |
| FB_WIDTH    | +0x08   | RW  | 32b   | Active width in pixels               |
| FB_HEIGHT   | +0x10   | RW  | 32b   | Active height in pixels              |
| FB_STRIDE   | +0x18   | RW  | 32b   | Bytes per scanline (≥ width × bpp/8) |
| FB_MODE     | +0x20   | RW  | 8b    | Pixel format (see below)             |
| FB_ENABLE   | +0x28   | RW  | 8b    | bit 0: scanout enable, bit 1: vsync IRQ |
| FB_VSYNC    | +0x30   | RW  | 32b   | Frame counter (read); write 1 to ack IRQ |
| FB_PAL_IDX  | +0x38   | W   | 8b    | Palette index for next PAL_DATA write |
| FB_PAL_DATA | +0x40   | W   | 32b   | 24-bit RGB palette entry (0x00RRGGBB) |
| FB_STATUS   | +0x48   | R   | 8b    | bit 0: enabled, bit 1: in-vblank     |

**Size:** 80 bytes (0x50).  Fits comfortably in the MMIO aperture.

### Pixel Modes

| Mode | BPP | Format              | Tile lanes | Notes                        |
|------|-----|---------------------|-----------|------------------------------|
| 0    | 8   | Indexed (palette)   | 64        | Default.  Best tile match.   |
| 1    | 16  | RGB565              | 32        | Direct color, no palette.    |
| 2    | 16  | FP16 grayscale      | 32        | Tile FP16 mode native.       |
| 3    | 32  | RGBA8888            | 16        | True color + alpha.          |

### Resolution Targets

| Resolution  | Mode 0 (8bpp) | Mode 3 (32bpp) | Use case               |
|-------------|---------------|----------------|------------------------|
| 160×120     | 18.75 KiB     | 75 KiB         | Tiny / embedded        |
| 320×240     | 75 KiB        | 300 KiB        | Primary target         |
| 640×480     | 300 KiB       | 1.2 MiB        | Max (8bpp only in HBW) |

---

## Implementation Plan

### Commit 1: Plan of action (this document)

### Commit 2: FramebufferDevice in emulator

- `devices.py`: `FramebufferDevice` class (~150 lines)
  - MMIO register read/write for all 10 registers
  - 256-entry palette table (default: grayscale ramp)
  - `tick()` method increments vsync counter when enabled
  - No display output — purely register-level
- `system.py`: Wire `FramebufferDevice` at MMIO offset `0x0A00`
  - Add `fb` attribute to `MegapadSystem`
  - Register in MMIO dispatch
- `devices.py`: Add `FB_BASE = 0x0A00` constant
- Tests: `TestFramebuffer` in `test_system.py`
  - Register read/write for all CSRs
  - Palette write sequence (idx then data)
  - Enable/disable transitions
  - Vsync counter increment via tick()
  - Mode validation
  - FB_STATUS reflects enable state
  - Pixel data read-back via HBW RAM

### Commit 3: BIOS framebuffer words

- `bios.asm`: ~15 new dictionary entries
  - Address registers: `FB-BASE!`, `FB-WIDTH!`, `FB-HEIGHT!`,
    `FB-STRIDE!`
  - Mode/control: `FB-MODE!`, `FB-MODE@`, `FB-ENABLE`, `FB-DISABLE`
  - Vsync: `FB-VSYNC@`, `FB-VSYNC-ACK`
  - Palette: `FB-PAL!` ( r g b index -- )
  - Status: `FB-STATUS@`
  - Convenience: `FB-SETUP` ( width height mode -- ) sets base/width/
    height/stride/mode in one call, defaults base to HBW Bank 3
- Tests: BIOS-level framebuffer word tests in `test_system.py`

### Commit 4: KDOS module system (`REQUIRE`)

- `kdos.f` §20: Module system (~50 lines)
  - `MOD-LOADED` — 64-bit bitmap (one bit per FS directory slot)
  - `REQUIRE` ( "filename" -- ) — parse name, check bitmap, skip if
    loaded, otherwise `LOAD` and set bit
  - `PROVIDED` ( "filename" -- ) — mark a module as already loaded
    without loading it (for built-in subsystems)
  - `MODULES` — list all loaded modules with their FS slot numbers
  - `MODULE?` ( "filename" -- flag ) — check if module is loaded
- `diskutil.py`: Ensure Forth module files use `FTYPE_FORTH` (3)
- Tests: `TestModuleSystem` in `test_system.py`
  - Load once, REQUIRE twice (idempotent — no double-define errors)
  - PROVIDED marks without loading
  - MODULE? query
  - Not-found error handling
  - MODULES listing

### Commit 5: KDOS graphics module (graphics.f)

- New file: `graphics.f` (~300 lines Forth), injected into disk image
  - Loaded via `REQUIRE graphics.f`
  - Core framebuffer ops:
    - `GFX-INIT` ( width height mode -- ) — FB-SETUP + default palette
      + enable + clear
    - `GFX-CLEAR` ( color -- ) — fill entire framebuffer via tile engine
      (TFILL on 64-byte-aligned rows)
    - `GFX-SYNC` ( -- ) — wait for vsync (poll FB-VSYNC@)
  - Pixel ops:
    - `GFX-PIXEL!` ( color x y -- ) — set single pixel
    - `GFX-PIXEL@` ( x y -- color ) — read single pixel
  - Primitives (tile-accelerated where possible):
    - `GFX-HLINE` ( color x y len -- ) — horizontal line via TFILL
    - `GFX-VLINE` ( color x y len -- ) — vertical line (byte loop)
    - `GFX-RECT` ( color x y w h -- ) — filled rectangle (TFILL rows)
    - `GFX-BOX` ( color x y w h -- ) — rectangle outline
  - Blitting:
    - `GFX-BLIT` ( src x y w h -- ) — copy tile data to framebuffer
      region (TLOAD2D/TSTORE2D for strided access)
    - `GFX-SCROLL-UP` ( n -- ) — scroll screen up by n rows (CMOVE)
  - Text rendering:
    - Built-in 8×8 bitmap font (96 printable ASCII characters, 768 B)
    - `GFX-CHAR` ( char x y color -- ) — render one glyph
    - `GFX-TYPE` ( addr len x y color -- ) — render string
    - `GFX-CR` ( -- ) — advance cursor, scroll if needed
  - Palette helpers:
    - `GFX-PAL-DEFAULT` ( -- ) — load default 16-color CGA palette
    - `GFX-PAL-GRAY` ( -- ) — load 256-level grayscale ramp
    - `GFX-PAL-SET` ( r g b idx -- ) — set one palette entry
  - Demo:
    - `GFX-DEMO` ( -- ) — draws rectangles, text, gradient; useful
      for visual smoke testing
- `diskutil.py`: Add `graphics.f` to `build_sample_image()`
- Tests: `TestGraphicsModule` in `test_system.py`
  - GFX-INIT sets registers and clears screen
  - GFX-PIXEL! / GFX-PIXEL@ round-trip
  - GFX-RECT fills correct region in HBW RAM
  - GFX-HLINE via tile engine produces correct bytes
  - GFX-BLIT transfers data correctly
  - GFX-CHAR renders glyph data to framebuffer
  - Tile engine integration (verifying TMODE, TSRC0, TDST usage)

### Commit 6: Emulator display output (optional SDL2/pygame)

- `cli.py`: `--display` flag (~120 lines)
  - When set, open a pygame window sized to FB_WIDTH × FB_HEIGHT × scale
  - Display loop (30 FPS default): read HBW RAM at `fb.base`, apply
    palette (mode 0) or direct decode (modes 1–3), blit to surface
  - Handle window close → clean shutdown
  - `--display-scale N` — integer pixel scaling (default 2×)
  - `--display-fps N` — target frame rate (default 30)
- Graceful fallback: if pygame not installed, `--display` prints a
  warning and continues headless
- `pyproject.toml`: Add `pygame-ce` as optional dependency
  (`[project.optional-dependencies] display = ["pygame-ce"]`)
- Manual smoke test: `GFX-DEMO` visible in window

### Commit 7: RTL framebuffer module

- `fpga/rtl/mp64_framebuffer.v` (~250 lines)
  - MMIO register bank (matches emulator exactly)
  - Palette LUT (256×24-bit, distributed RAM)
  - VGA timing generator (640×480 @ 60 Hz, active area configurable)
  - Line buffer (double-buffered, 1 BRAM18 each)
  - HBW read port: fetches one scanline per hblank
  - Pixel clock: 25.175 MHz for 640×480 VGA (from PLL)
- `fpga/rtl/mp64_soc.v`: Wire framebuffer into SoC
  - MMIO decode at offset 0x0A00
  - Connect HBW read port (shared with tile, arbitrated)
- `fpga/sim/tb_framebuffer.v`: Testbench (~200 lines)
  - Register read/write for all CSRs
  - Palette programming
  - Scanline fetch sequence verification
  - Vsync timing / IRQ assertion

### Commit 8: Documentation & roadmap update

- `docs/framebuffer.md` — full peripheral specification
- `docs/architecture.md` — add framebuffer to memory map table and
  block diagram
- `docs/kdos-reference.md` — §20 module system, graphics module words
- `docs/bios-forth.md` — framebuffer BIOS word reference
- `EMULATOR.md` — `--display` flag, pygame dependency note
- `ROADMAP-v1.0.md` — add framebuffer items to Layer 4, update status
- `fpga/docs/fpga.md` — update future peripherals section

---

## Dependency Graph

```
Commit 2 (device) ←── Commit 3 (BIOS words) ←── Commit 5 (graphics.f)
                                                      ↑
Commit 4 (REQUIRE) ──────────────────────────────────┘
Commit 2 (device) ←── Commit 6 (SDL display)
Commit 2 (device) ←── Commit 7 (RTL)
All commits ←──────── Commit 8 (docs)
```

Commits 2 and 4 are independent of each other and could be done in
either order.  Commit 5 depends on both 3 and 4.  Commits 6 and 7
depend only on 2.  Documentation is last.

---

## What This Does NOT Include

- **GPU command processor** — no hardware rasterizer, no draw commands.
  The CPU + tile engine handles all rendering.
- **3D graphics** — out of scope.  The tile engine can do matrix ops
  for 3D transforms, but there's no rasterization pipeline.
- **System console on framebuffer** — the UART serial console remains
  the primary interface.  GFX-CHAR/GFX-TYPE are for applications,
  not the OS console.
- **Window manager / compositor** — that's application-level code,
  built on top of the graphics module if desired.
- **Mouse / touchscreen / gamepad** — separate peripherals, future
  work.
- **Hardware sprites / layers / scrolling** — the framebuffer is a
  flat pixel buffer.  Sprite-like effects can be done in software
  via GFX-BLIT and the tile engine.
- **Video / DMA scanout chaining** — single-buffer for now.  Double-
  buffering is possible (two FB_BASE addresses, swap on vsync) but
  is a software pattern, not a hardware feature.

---

## File Impact Summary

| File | Change | Est. Lines |
|------|--------|-----------|
| `devices.py` (2,348 → ~2,500) | Add `FramebufferDevice` | +150 |
| `system.py` (921 → ~940) | Wire FB device | +15 |
| `bios.asm` (11,596 → ~11,800) | 15 framebuffer BIOS words | +200 |
| `kdos.f` (9,085 → ~9,135) | §20 Module system | +50 |
| `graphics.f` (new) | Graphics module for disk | +300 |
| `diskutil.py` (1,144 → ~1,155) | Inject graphics.f | +10 |
| `cli.py` (1,005 → ~1,125) | `--display` option | +120 |
| `pyproject.toml` | Optional pygame dependency | +3 |
| `test_system.py` (16,403 → ~16,750) | 3 new test classes | +350 |
| `fpga/rtl/mp64_framebuffer.v` (new) | RTL scanout module | +250 |
| `fpga/rtl/mp64_soc.v` | Wire FB into SoC | +30 |
| `fpga/sim/tb_framebuffer.v` (new) | RTL testbench | +200 |
| Docs (5 files) | Spec, arch, reference, emulator, roadmap | +250 |
| **Total** | | **~1,930** |

---

## Test Plan

| Test Class | Scope | Est. Tests |
|------------|-------|-----------|
| `TestFramebuffer` | MMIO register R/W, palette, vsync, status, pixel readback | ~15 |
| `TestModuleSystem` | REQUIRE idempotency, PROVIDED, MODULE?, listing | ~8 |
| `TestGraphicsModule` | GFX-INIT, pixel ops, rect, hline, blit, char, tile integration | ~15 |
| `tb_framebuffer.v` | RTL register bank, palette, scanline fetch, vsync timing | ~12 |
| **Total new tests** | | **~50** |
