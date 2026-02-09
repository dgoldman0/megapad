# Megapad-64

### A Tile-Oriented Fantasy Computer with a Forth BIOS and Operating System

Megapad-64 is a complete computer system built from scratch — CPU, BIOS,
operating system, filesystem, SIMD tile engine, and interactive dashboard
— all running inside a Python emulator and verified by 619+ tests.

The core idea: put a large, fast scratchpad memory directly on the
processor die and give the CPU a dedicated engine that runs SIMD
operations across that memory one **64-byte tile** at a time.  Instead of
shuttling data through a cache hierarchy, software works on tiles
directly.  A single MEX instruction can add, multiply, dot-product, or
reduce an entire tile's worth of data in one shot.

Everything from the instruction encoding to the filesystem runs in the
emulator.  You can boot the system, type Forth at the REPL, create
buffers, chain compute kernels into pipelines, ingest network data, browse
on-disk documentation, and explore a 7-screen TUI dashboard — all
interactively.

---

## Current Status: v1.0

| Component | Stats |
|-----------|-------|
| **BIOS** | 197 Forth dictionary words, 8,287 lines ASM, 20.1 KB binary |
| **KDOS** | 225 colon definitions + 119 variables/constants, 2,778 lines Forth |
| **Emulator** | Full CPU + tile engine, 1,358 lines Python |
| **Tests** | 678+ passing (CPU, BIOS, KDOS, FS, devices, assembler, diskutil) |
| **Filesystem** | MP64FS — 1 MiB images, 64 files, 6 file types |
| **Tooling** | CLI/debugger, two-pass assembler (with listing output), disk utility |

All core subsystems are **functionally complete**: BIOS Forth, KDOS kernel
dashboard, tile engine, filesystem, scheduler, pipelines, networking, disk
I/O, auto-boot from disk, interactive TUI, built-in documentation browser.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Megapad-64 CPU                        │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ Scalar Core   │  │ Tile Engine  │  │ CSR Registers │  │
│  │ 16×64-bit GPR │  │ 64-byte SIMD │  │ FLAGS, TMODE  │  │
│  │ 8-bit D acc   │  │ 256-bit ACC  │  │ TCTRL, IVT    │  │
│  │ 8 flags       │  │ 8–64 lanes   │  │ cursor regs   │  │
│  └──────┬───────┘  └──────┬───────┘  └───────────────┘  │
│         │                  │                              │
│         └──────┬───────────┘                              │
│                │                                          │
│     ┌──────────▼──────────┐                               │
│     │ Unified Address Bus │                               │
│     └──────────┬──────────┘                               │
└────────────────┼────────────────────────────────────────┘
                 │
    ┌────────────┼────────────┐
    │            │            │
┌───▼───┐  ┌────▼────┐  ┌────▼──────┐
│  RAM  │  │  MMIO   │  │ Storage   │
│1 MiB  │  │ Devices │  │Controller │
└───────┘  └────┬────┘  └───────────┘
                │
      ┌────┬────┼────┬──────┐
      │    │    │    │      │
    UART Timer SysInfo NIC  │
                           Disk
```

**Scalar CPU** — 16 general-purpose 64-bit registers, variable-length
instructions (1–10 bytes), 16 instruction families, heritage from the RCA
CDP1802 with modern 64-bit extensions.  Any GPR can serve as PC, data
pointer, or stack pointer via runtime selectors.

**Tile Engine** — A SIMD execution unit controlled through CSR registers
and triggered by MEX instructions.  Each operation processes a 64-byte
tile (8 to 64 lanes depending on element width) with element-wise ALU,
multiply, dot product, or reduction functions.  A 256-bit accumulator
supports multi-tile accumulation without overflow.

**Megapad Memory** — The full design calls for 8–64 MiB of on-chip
multi-ported SRAM organized into 16 banks, delivering GPU-class bandwidth
from a single core.  The emulator models this as flat memory with
simplified timing — behavior is correct, only cycle-level timing differs
from the hardware target.

**Devices** — UART (serial I/O), Timer (cycle-accurate with interrupts),
Storage Controller (sector-based disk I/O), NIC (Ethernet frames with
DMA), SystemInfo (CPUID, memory size).  All are memory-mapped at
`0xFFFF_FF00+`.

---

## Software Stack

```
┌─────────────────────────────────┐
│          User Programs          │  ← Forth words at the REPL
├─────────────────────────────────┤
│     KDOS v1.0 (2,778 lines)    │  ← Buffers, kernels, pipelines,
│  Buffers · Kernels · Pipelines  │    scheduler, filesystem, TUI,
│  Scheduler · Filesystem · TUI   │    data ports, documentation
├─────────────────────────────────┤
│     BIOS v1.0 (8,287 lines)    │  ← Subroutine-threaded Forth,
│  197 words · EVALUATE · FSLOAD  │    compiler, I/O, tile CSR words
├─────────────────────────────────┤
│         Hardware / Emulator     │  ← megapad64.py + devices.py
└─────────────────────────────────┘
```

**BIOS** — A subroutine-threaded Forth interpreter/compiler in assembly.
197 dictionary words covering arithmetic, logic, stack manipulation,
memory access, control flow (IF/ELSE, BEGIN/UNTIL/WHILE, DO/LOOP),
strings, compilation, I/O, disk, timer, tile engine, and NIC.  Includes
`FSLOAD` for booting KDOS directly from a disk image.  Hardened with
stack underflow detection, EVALUATE depth limiting, dictionary-full
guards, and FSLOAD error recovery with file/line context.

**KDOS** — The Kernel Dashboard OS, written entirely in Forth.  14 sections
covering: utility words, described buffers with tile-aligned storage,
tile-accelerated buffer operations (B.SUM, B.ADD, etc.), a kernel registry
with 18 built-in compute kernels, a pipeline engine, raw and named file
I/O, the MP64FS filesystem, a documentation browser, dictionary search
tools, a cooperative scheduler with timer-assisted preemption, an 8-screen
interactive TUI (with auto-refresh), data ports for NIC ingestion,
benchmarking, a full dashboard, a categorized help system with
per-word `DESCRIBE`, and auto-boot.

---

## Quick Start

### Prerequisites

Python 3.8+ (3.10 recommended).  **No external dependencies** — the
entire project is pure Python standard library.

```bash
git clone <repository-url>
cd megapad-64
```

### Boot the System

```bash
# Without disk (KDOS injected via UART)
python cli.py --bios bios.asm --forth kdos.f

# With a disk image (full filesystem access)
python diskutil.py sample              # build sample.img first
python cli.py --bios bios.asm --forth kdos.f --storage sample.img
```

You'll see the KDOS banner and land at the Forth REPL:

```
------------------------------------------------------------
  KDOS v1.0 — Kernel Dashboard OS
------------------------------------------------------------
Type HELP for command reference.
Type SCREENS for interactive TUI.
Type TOPICS or LESSONS for documentation.
```

### Try It Out

```forth
HELP                              \ Full command reference
DASHBOARD                         \ System overview

256 0 1 BUFFER: demo              \ Create a 256-byte buffer
42 demo B.FILL                    \ Fill every byte with 42
demo B.SUM .                      \ Sum → 10752
demo kstats                       \ Prints sum, min, max

256 0 1 BUFFER: a                 \ Two more buffers
256 0 1 BUFFER: b
10 a B.FILL  20 b B.FILL
a b demo kadd                     \ Tile-accelerated add: demo = a + b
demo B.PREVIEW                    \ Hex dump first 64 bytes

SCREENS                           \ Launch 8-screen TUI dashboard
```

### Run the Tests

```bash
python -m pytest test_system.py test_megapad64.py -v --timeout=30
```

All 678+ tests should pass, covering the CPU, BIOS, KDOS, filesystem,
assembler, disk utility, devices, and networking.

---

## Project Files

| File | Lines | Purpose |
|------|-------|---------|
| `megapad64.py` | 1,358 | CPU + tile engine emulator |
| `system.py` | 300 | System integration (CPU + devices + memory map) |
| `bios.asm` | 8,287 | Forth BIOS in assembly (197 words, hardened) |
| `bios.rom` | 20,605 B | Pre-assembled BIOS binary |
| `kdos.f` | 2,778 | KDOS operating system in Forth (344 definitions) |
| `cli.py` | 992 | CLI, boot modes, interactive debug monitor |
| `asm.py` | 748 | Two-pass assembler with SKIP and listing output |
| `devices.py` | 718 | MMIO devices: UART, Timer, Storage, SystemInfo, NIC |
| `datasources.py` | 697 | Simulated network data sources |
| `diskutil.py` | 941 | MP64FS filesystem utility and disk image builder |
| `test_megapad64.py` | 712 | CPU instruction set unit tests |
| `test_system.py` | 5,258 | Full integration test suite |

---

## Documentation

The `docs/` directory contains comprehensive reference material:

| Document | Contents |
|----------|----------|
| [docs/getting-started.md](docs/getting-started.md) | Quick-start guide — booting, REPL, first buffer, first kernel, first pipeline |
| [docs/bios-forth.md](docs/bios-forth.md) | Complete BIOS Forth word reference (all 197 entries by category) |
| [docs/kdos-reference.md](docs/kdos-reference.md) | Complete KDOS word reference (all 300+ definitions by section) |
| [docs/isa-reference.md](docs/isa-reference.md) | CPU instruction set — all 16 families, encodings, condition codes, CSRs |
| [docs/architecture.md](docs/architecture.md) | System architecture — memory map, MMIO registers, boot sequence, interrupts |
| [docs/filesystem.md](docs/filesystem.md) | MP64FS specification — on-disk format, directory entries, file types |
| [docs/tile-engine.md](docs/tile-engine.md) | Tile engine programming guide — CSRs, MEX encoding, KDOS integration |
| [docs/tools.md](docs/tools.md) | CLI & debug monitor, assembler, disk utility, test suite |

> **Note:** Some details (e.g., the full multi-bank megapad architecture)
> reflect the simplified emulator model rather than the complete hardware
> design.

---

## Why Forth?

Forth might seem like an unusual choice for a modern system, but it's
remarkably well-suited to this architecture:

**Compactness** — The entire BIOS interpreter, compiler, and 197 built-in
words fit in under 20 KB of machine code.  Forth is one of the most
space-efficient programming environments ever created.

**Interactivity** — Type a word, it executes immediately.  Development is
fast and exploratory — perfect for building kernels and pipelines on the
fly.

**Extensibility** — New words are defined in terms of existing ones.
There's no distinction between "system" words and "user" words.  The
dictionary is open and everything is inspectable.

**Control** — Forth gives complete control over memory, I/O, and execution
flow.  You can write high-level abstractions or drop to raw machine
operations as needed.

The combination of Forth (for orchestration) and the tile engine (for
data-parallel computation) creates a programming model that's both
powerful and surprisingly elegant.

---

## Emulator vs. Hardware Design

The Python emulator is a **functional simulation** — it implements the
full instruction set and produces correct results, but models the megapad
and tile engine as flat memory with simplified cycle counting.

The full hardware design envisions:

- **8–64 MiB on-chip SRAM** organized into 16 banks with multi-port access
- **4 read ports + 2 write ports** for sustained tile throughput
- **Bank-local operations** for rapid intra-bank calculation
- **4-stage in-order pipeline** (IF, ID, EX, WB) with single-cycle
  bubble on taken branches
- **Fully static design** — retains state down to DC for ultra-low power

Software written for the emulator will run identically on any future
hardware implementation.  The simplification affects only timing accuracy,
not behavior.

---

## License

CC0 (for all components not superseded by existing licenses)

---

## Acknowledgments

This project draws inspiration from:

- The **Forth** programming language and its interactive, stack-based model
- Modern **GPU architectures** and their data-parallel execution models
- Classic microcomputers like the **Commodore 64**, where hardware and
  software formed a unified, exploratory environment
- The **RCA CDP1802** for its register-designator concepts and compact
  instruction encoding
- The **RISC-V** ISA for its clean, orthogonal design principles

Megapad-64 is a synthesis of these ideas, reimagined for a hypothetical
architecture that never was — but perhaps could be.
