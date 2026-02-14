# Megapad-64

### A Tile-Oriented Fantasy Computer with a Forth BIOS and Operating System

Megapad-64 is a complete computer system built from scratch — CPU, BIOS,
operating system, filesystem, SIMD tile engine, and interactive dashboard
— all running inside a Python emulator and verified by 750+ tests.

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

## Current Status: v1.0 (Multicore)

| Component | Stats |
|-----------|-------|
| **BIOS** | 265 Forth dictionary words, 10,070 lines ASM, ~22 KB binary |
| **KDOS** | v1.1 — 433 colon definitions + 219 variables/constants, 5,328 lines Forth |
| **Emulator** | Quad-core SoC with mailbox IPI & spinlocks, 2,541 lines Python |
| **C++ Accelerator** | Optional pybind11 CPU core — 63× speedup over PyPy (23 s full suite) |
| **Tests** | 754 passing (CPU, BIOS, KDOS, FS, devices, assembler, multicore, tile engine, crypto) |
| **Filesystem** | MP64FS — 1 MiB images, 64 files, 7 file types |
| **Tooling** | CLI/debugger, two-pass assembler (with listing output), disk utility |
| **FPGA RTL** | 18 Verilog modules + 13 testbenches, Genesys 2 (Kintex-7) target |

All core subsystems are **functionally complete**: BIOS Forth, KDOS kernel
dashboard, tile engine, filesystem, scheduler, pipelines, networking, disk
I/O, auto-boot from disk, interactive TUI, built-in documentation browser,
and **quad-core multicore dispatch** with IPI, spinlocks, and barriers.

---

## Architecture Overview

```
┌───────────────────────────────────────────────────────────────┐
│                      Megapad-64 SoC                           │
│                                                               │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐             │
│  │ Core 0  │ │ Core 1  │ │ Core 2  │ │ Core 3  │             │
│  │ 16×GPR  │ │ 16×GPR  │ │ 16×GPR  │ │ 16×GPR  │             │
│  │ 4K I$   │ │ 4K I$   │ │ 4K I$   │ │ 4K I$   │             │
│  │ Tile    │ │ Tile    │ │ Tile    │ │ Tile    │             │
│  │ Engine  │ │ Engine  │ │ Engine  │ │ Engine  │             │
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘             │
│       │           │           │           │                   │
│  ┌────▼───────────▼───────────▼───────────▼────┐              │
│  │    Round-Robin Bus Arbiter (per-core QoS)   │              │
│  └────────────────────┬────────────────────────┘              │
│                       │                                       │
│       ┌───────────────┼───────────────┐                       │
│       │               │               │                       │
│  ┌────▼────┐  ┌───────▼───────┐  ┌────▼──────┐               │
│  │  SRAM   │  │  MMIO Devices │  │  Mailbox  │               │
│  │ 1–4 MiB │  │  UART  Timer  │  │  IPI +    │               │
│  │         │  │  Disk  NIC    │  │  Spinlock │               │
│  │         │  │  CRC Engine   │  │           │               │
│  └─────────┘  └───────────────┘  └───────────┘               │
└───────────────────────────────────────────────────────────────┘
```

**Quad-Core SoC** — Four identical cores sharing a unified address space
via a round-robin bus arbiter with per-core QoS weights.  Cores
communicate through a hardware mailbox (IPI) and 8 spinlocks.  Secondary
cores boot into a worker loop; the primary core dispatches work via
`WAKE-CORE` / `IPI-SEND`.

**Scalar CPU** — Each core has 16 general-purpose 64-bit registers,
variable-length instructions (1–10 bytes), 16 instruction families,
heritage from the RCA CDP1802 with modern 64-bit extensions.  Any GPR can
serve as PC, data pointer, or stack pointer via runtime selectors.
A 2-stage pipeline (IF + DEX) with a per-core 4 KiB direct-mapped
instruction cache delivers ~2× throughput over the original FSM design
with no speculation or out-of-order execution.

**Tile Engine** — Per-core SIMD execution unit controlled through CSR
registers and triggered by MEX instructions.  Each operation processes a
64-byte tile (8 to 64 lanes depending on element width) with element-wise
ALU, multiply, dot product, or reduction functions.  Extended operations
include FMA, widening multiply, saturating arithmetic, rounding shifts,
tile views (SHUFFLE/PACK/UNPACK/RROT), strided 2D loads/stores, and
FP16/bfloat16 support.  A 256-bit accumulator enables multi-tile
accumulation without overflow.

**Megapad Memory** — 1 MiB on-chip SRAM in the base design (4 MiB in the
expanded model), modeled as flat byte-addressable memory in the emulator.
The FPGA target uses block RAM; a future multi-bank organization could
deliver higher bandwidth for tile-heavy workloads.

**Devices** — UART (serial I/O), Timer (cycle-accurate with interrupts),
Storage Controller (sector-based disk I/O), NIC (Ethernet frames with
DMA), CRC Engine (CRC32/CRC32C/CRC64), SystemInfo (CPUID, memory size),
Mailbox (inter-core IPI), Spinlocks (8 hardware mutexes).  All are
memory-mapped at `0xFFFF_FF00+`.

---

## Software Stack

```
┌─────────────────────────────────┐
│          User Programs          │  ← Forth words at the REPL
├─────────────────────────────────┤
│    KDOS v1.1 (5,328 lines)     │  ← Buffers, kernels, pipelines,
│  Buffers · Kernels · Pipelines  │    scheduler, filesystem, TUI,
│  Scheduler · Filesystem · TUI   │    data ports, multicore dispatch
├─────────────────────────────────┤
│    BIOS v1.0 (10,070 lines)    │  ← Subroutine-threaded Forth,
│  265 words · EVALUATE · FSLOAD  │    compiler, I/O, tile, multicore
├─────────────────────────────────┤
│         Hardware / Emulator     │  ← megapad64.py + devices.py
└─────────────────────────────────┘
```

**BIOS** — A subroutine-threaded Forth interpreter/compiler in assembly.
265 dictionary words covering arithmetic, logic, stack manipulation,
memory access, control flow (IF/ELSE, BEGIN/UNTIL/WHILE, DO/LOOP),
strings, compilation, I/O, disk, timer, tile engine, NIC, **multicore**
(COREID, NCORES, IPI-SEND, SPIN@/SPIN!, WAKE-CORE, CORE-STATUS),
**performance counters**, **CRC engine**, **memory BIST**, **tile self-test**,
**strided/2D addressing**, and **FP16/BF16 modes**.
Includes `FSLOAD` for booting KDOS directly from a disk image.  Hardened
with stack underflow detection, EVALUATE depth limiting, dictionary-full
guards, and FSLOAD error recovery with file/line context.

**KDOS** — The Kernel Dashboard OS v1.1, written entirely in Forth.  16
sections covering: utility words, described buffers with tile-aligned
storage, tile-accelerated buffer operations (B.SUM, B.ADD, etc.), a kernel
registry with 18 built-in compute kernels, a pipeline engine, raw and
named file I/O, the MP64FS filesystem, a documentation browser, dictionary
search tools, a cooperative scheduler with timer-assisted preemption, a
9-screen interactive TUI (with auto-refresh), data ports for NIC ingestion,
benchmarking, a full dashboard, a categorized help system with per-word
`DESCRIBE`, versioned pipeline bundles, **multicore dispatch** (CORE-RUN,
CORE-WAIT, BARRIER, P.RUN-PAR), and auto-boot.

---

## Quick Start

### Prerequisites

Python 3.8+ (3.12 recommended).  The emulator and all tools run with
**no external dependencies** — pure Python standard library.

For the optional **C++ accelerator** (63× speedup), you need CPython 3.12
and pybind11 (`pip install pybind11`).  Build with `make accel`.

```bash
git clone <repository-url>
cd megapad-64
```

### Boot the System

```bash
# Build the sample disk image (includes KDOS + docs + tutorials)
python diskutil.py sample

# Boot from disk (recommended) — BIOS auto-loads KDOS from disk
python cli.py --bios bios.asm --storage sample.img

# Without disk (development mode — KDOS injected via UART, no FS access)
python cli.py --bios bios.asm --forth kdos.f

# ~5× faster under PyPy (see 'make setup-pypy')
.pypy/bin/pypy3 cli.py --bios bios.asm --storage sample.img
```

You'll see the KDOS banner and land at the Forth REPL:

```
------------------------------------------------------------
  KDOS v1.1 — Kernel Dashboard OS
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

SCREENS                           \ Launch 9-screen TUI dashboard
```

### Run the Tests

```bash
# C++ accelerator (recommended — 63× faster than PyPy)
python -m venv .venv && .venv/bin/pip install pybind11 pytest pytest-xdist
make accel                         # build C++ extension
make test-accel                    # ~23 s, all 754 tests

# PyPy + xdist (no C++ compiler needed)
make setup-pypy                    # one-time
make test                          # ~24 min

# CPython fallback (no setup required)
python -m pytest test_system.py test_megapad64.py -v --timeout=30  # ~40 min
```

All 754 tests should pass, covering the CPU, BIOS, KDOS, filesystem,
assembler, disk utility, devices, multicore, networking, crypto, and
extended tile engine.

| Runner | Time | Speedup |
|--------|------|---------|
| CPython (pure Python) | ~40 min | 1× |
| PyPy + xdist -n 8 | ~24 min | 1.7× |
| **CPython + C++ accel -n 8** | **~23 s** | **104×** |

---

## Project Files

| File | Lines | Purpose |
|------|-------|---------|
| `megapad64.py` | 2,541 | CPU + tile engine emulator (incl. extended ops, FP16/BF16) |
| `accel/mp64_accel.cpp` | 1,929 | C++ CPU core (pybind11) — 63× speedup |
| `accel_wrapper.py` | 830 | Drop-in Python wrapper for the C++ CPU core |
| `system.py` | 598 | Quad-core SoC integration + `run_batch()` C++ fast path |
| `bios.asm` | 10,070 | Forth BIOS in assembly (265 words, multicore, hardened) |
| `bios.rom` | ~22 KB | Pre-assembled BIOS binary |
| `kdos.f` | 5,328 | KDOS v1.1 operating system in Forth (433 definitions) |
| `cli.py` | 995 | CLI, boot modes, interactive debug monitor |
| `asm.py` | 788 | Two-pass assembler with SKIP and listing output |
| `devices.py` | 1,418 | MMIO devices: UART, Timer, Storage, NIC, Mailbox, Spinlock, CRC, AES, SHA3 |
| `data_sources.py` | 697 | Simulated network data sources |
| `diskutil.py` | 1,039 | MP64FS filesystem utility and disk image builder |
| `test_megapad64.py` | 2,193 | 23 CPU + tile engine tests |
| `test_system.py` | 9,673 | 754 integration tests (25 classes, incl. multicore, tile, crypto, FS) |
| `Makefile` | 177 | Build, test, & accel targets (PyPy + xdist + C++ accel) |
| `setup_accel.py` | 35 | pybind11 build configuration |
| `bench_accel.py` | 139 | C++ vs Python speed comparison script |
| `conftest.py` | 193 | Test fixtures, snapshot caching, live status reporting |
| `fpga/rtl/` | 11,284 | 18 Verilog modules (CPU, tile, FP16 ALU, SoC, peripherals) |
| `fpga/sim/` | 7,293 | 13 Verilog testbenches (137 hardware tests) |

---

## Documentation

The `docs/` directory contains comprehensive reference material:

| Document | Contents |
|----------|----------|
| [docs/getting-started.md](docs/getting-started.md) | Quick-start guide — booting, REPL, first buffer, first kernel, first pipeline |
| [docs/bios-forth.md](docs/bios-forth.md) | Complete BIOS Forth word reference (all 265 entries by category) |
| [docs/kdos-reference.md](docs/kdos-reference.md) | Complete KDOS v1.1 word reference (all 400+ definitions by section, incl. multicore) |
| [docs/isa-reference.md](docs/isa-reference.md) | CPU instruction set — all 16 families, encodings, condition codes, CSRs |
| [docs/architecture.md](docs/architecture.md) | System architecture — memory map, MMIO registers, boot sequence, interrupts |
| [docs/filesystem.md](docs/filesystem.md) | MP64FS specification — on-disk format, directory entries, file types |
| [docs/tile-engine.md](docs/tile-engine.md) | Tile engine programming guide — CSRs, MEX encoding, extended ops, FP16/BF16 |
| [docs/extended-tpu-spec.md](docs/extended-tpu-spec.md) | Extended TPU specification — crypto, DMA, BIST, perf counters, FP16 |
| [docs/tools.md](docs/tools.md) | CLI & debug monitor, assembler, disk utility, test suite, C++ accelerator |

> **Note:** Some details (e.g., the full multi-bank megapad architecture)
> reflect the simplified emulator model rather than the complete hardware
> design.

---

## Why Forth?

Forth might seem like an unusual choice for a modern system, but it's
remarkably well-suited to this architecture:

**Compactness** — The entire BIOS interpreter, compiler, and 265 built-in
words fit in roughly 22 KB of machine code.  Forth is one of the most
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

## Emulator vs. FPGA RTL

The Python emulator is a **functional simulation** — it implements the
full instruction set and produces correct results, but models memory as
flat RAM with simplified cycle counting.

The FPGA RTL (targeting Nexys A7-200T) implements the full
quad-core SoC including the extended tile engine.  Key differences from
the emulator are cycle-level timing and block-RAM-backed memory:

- **1 MiB SRAM** base (4 MiB expanded), backed by FPGA block RAM
- **2-stage pipeline** (IF + DEX) with per-core 4 KiB direct-mapped
  instruction cache; no speculation or branch prediction
- **Quad-core** with round-robin bus arbiter and per-core QoS weights
- **Fully static design** — retains state down to DC for ultra-low power

No post-synthesis resource numbers yet — the extended tile engine
(FP16 ALU, LOAD2D/STORE2D, CRC, BIST, perf counters) added significant
logic beyond the original base-ISA estimates.

Software written for the emulator runs identically on the RTL.
The simplification affects only timing accuracy, not behavior.

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
