# Megapad-64

### A Tile-Oriented Computer Architecture & Operating System

Megapad-64 is a complete computer system built around a simple premise: put a large, fast scratchpad memory directly on the processor die, and give the CPU a dedicated engine that can run SIMD operations across that memory one 64-byte tile at a time. Instead of shuttling data through a traditional cache hierarchy, the "megapad" — between 8 and 64 megabytes of on-chip SRAM depending on the build configuration — sits right next to the execution unit, delivering GPU-class throughput from a single core.

This project explores that idea end-to-end: a Python-based emulator for the chip, an assembler and instruction set, a Forth BIOS, and a complete operating system called KDOS (Kernel Dashboard OS). Everything from the instruction encoding to the file system runs in the emulator and is verified by a comprehensive test suite.

---

## What Makes Megapad-64 Different?

Conventional processors keep a small set of registers close to the ALU and rely on caches to bridge the gap to main memory. The Megapad-64 replaces most of that hierarchy with a single, large on-chip SRAM — the "megapad." Software sees this as a flat array of 64-byte tiles organized into 16 logical banks, and the chip's tile engine can read from two tiles and write to a third every clock cycle through dedicated multi-port access.

Because the megapad is on-die SRAM rather than external DRAM, there are no cache misses and no unpredictable latencies. A single MEX (Memory Execute) instruction tells the tile engine to run an arithmetic, logical, or reduction operation across every lane of a tile simultaneously — much like a GPU shader, but orchestrated by a conventional scalar CPU sitting right alongside it.

The scalar side is a 16-register, 64-bit RISC core with a 4-stage in-order pipeline. It handles control flow, I/O, and orchestration while the tile engine does the heavy lifting on data. The two halves share the same address space: the CPU can read and write individual bytes in the megapad, and the tile engine can be pointed at any 64-byte-aligned address. This means there's no awkward "upload to GPU" step — data is just *there*, ready for either side to work on.

Think of it as a hybrid between a classic microcomputer and a modern GPU shader core, designed so that one person sitting at a Forth prompt can interactively build, test, and compose data-parallel programs without any of the ceremony that GPU programming normally demands.

---

## Architecture Highlights

**Megapad SRAM**: The defining feature. The chip carries 8 to 64 MiB of on-chip SRAM (selected at fabrication time) organized into 16 logical banks of 64-byte tiles. Four read ports and two write ports let the tile engine sustain one tile operation per clock cycle — at 1 GHz that's 64 GB/s of internal throughput, comparable to a small GPU, without any caches.

**Tile Engine**: A dedicated SIMD execution unit controlled by CSR registers and triggered by a single MEX instruction. Each MEX operation reads up to two source tiles, performs a lane-parallel arithmetic, logic, multiply, or reduction operation, and writes the result to a destination tile. Element widths of 8, 16, 32, or 64 bits are selected per-operation via the TMODE register, so the same 64-byte tile can be treated as 64 bytes, 32 halfwords, 16 words, or 8 doublewords depending on what the program needs.

**Scalar CPU**: A 16-register RISC core with 64-bit native word size and a 4-stage in-order pipeline (IF, ID, EX, WB). Branches are PC-relative with a single-cycle bubble on taken branches. The design inherits register-designator concepts from the RCA 1802 — any GPR can serve as the program counter, data pointer, or stack pointer, selected at runtime — giving the instruction set an unusual flexibility.

**Fully Static Design**: The core retains state down to DC. An IDLE instruction puts the chip into deep sleep; an HALT stops the clock entirely. This makes the architecture well-suited to embedded and battery-powered applications where the megapad can hold working data while the CPU sleeps.

**Emulator Note**: The Python emulator in this repository is a *functional* simulation — it implements the full instruction set and produces correct results, but it models the megapad, banks, and pipeline as a flat memory array with simplified cycle counting. Software written for the emulator will run identically on any future hardware implementation; the simplification only affects timing accuracy, not behavior.

**Forth-Based System Software**: The BIOS is written in assembly but presents a Forth interpreter. Forth is a stack-based language that's remarkably compact, interactive, and well-suited to systems programming. The entire BIOS fits in about 5,500 lines of assembly and provides 157 Forth words.

**KDOS Operating System**: Built on top of the BIOS, KDOS is a complete interactive environment written in Forth. It provides buffers (typed memory regions), kernels (computational building blocks), pipelines (kernel composition), tasks (cooperative multitasking), and a full-screen dashboard interface. KDOS also includes MP64FS, an on-disk file system for persistent storage, and a built-in documentation browser.

**Devices & I/O**: The system includes a UART for serial I/O, a network interface card with DMA support, a storage controller for disk I/O, a timer, and system information registers. All devices are memory-mapped and accessible from both Forth and assembly.

---

## What Can You Do With It?

Megapad-64 is a complete, working computer. You can:

- **Write interactive programs** in Forth at the BIOS prompt
- **Create buffers** to hold data and run kernels across them
- **Compose pipelines** that chain multiple processing steps together
- **Ingest data** from simulated network sources (sensor streams, audio, images, text)
- **Store and retrieve files** using the MP64FS file system
- **Build custom kernels** in assembly or high-level Forth definitions
- **Explore the system** using the DASHBOARD, HELP, and built-in documentation browser
- **Run 327 comprehensive tests** that verify every aspect of the system

The project includes real-world examples: audio processing, image filtering, text analysis, embedding similarity, sensor data pipelines, and more. These aren't toy demos—they're complete, working programs that demonstrate the architecture's strengths.

---

## Project Structure

The repository contains a complete implementation:

- **`megapad64.py`** — The Megapad-64 CPU and tile engine emulator (1,305 lines)
- **`bios.asm`** — Forth BIOS in Megapad-64 assembly (5,483 lines, 157 words)
- **`kdos.f`** — Kernel Dashboard OS in Forth (2,393 lines)
- **`asm.py`** — Assembler for Megapad-64 machine code (649 lines)
- **`cli.py`** — Interactive REPL with storage and debugging commands (990 lines)
- **`devices.py`** — Device emulators: UART, NIC, storage, timer, system info (718 lines)
- **`data_sources.py`** — Simulated data sources: sensors, audio, images, text (697 lines)
- **`system.py`** — System integration layer connecting CPU to devices (300 lines)
- **`diskutil.py`** — MP64FS file system and disk image builder (914 lines)
- **`test_megapad64.py`** — CPU-level unit tests (711 lines)
- **`test_system.py`** — Full integration test suite (3,956 lines, 327 tests)
- **`KDOS.md`** — Complete KDOS technical documentation (1,184 lines)
- **`EMULATOR.md`** — Emulator and instruction set documentation (461 lines)

---

## Getting Started

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd megapad-64

# Create a virtual environment and install (no external dependencies needed)
python3 -m venv .venv
source .venv/bin/activate
```

### Running the System

Start an interactive session:

```bash
python cli.py --bios bios.asm --forth kdos.f
```

You'll see the KDOS boot sequence, then a Forth prompt. Try these commands:

```forth
HELP                          \ Show all available commands
DASHBOARD                     \ System overview
0 1 128 BUFFER mybuf          \ Create a 128-byte buffer
42 mybuf B.FILL               \ Fill with value 42
mybuf B.SUM .                 \ Sum all bytes → prints 5376
```

### Running Tests

The full test suite validates the entire system:

```bash
python test_system.py
```

All 327 tests should pass, covering the CPU emulator, BIOS words, KDOS features, file system, networking, and end-to-end application scenarios.

### Building a Disk Image

Create a disk image with pre-loaded documentation and tutorials:

```bash
python -c "from diskutil import build_image; build_image('disk.img')"
```

Then boot with storage attached:

```bash
python cli.py --bios bios.asm --forth kdos.f --storage disk.img
```

Inside KDOS, you can now use:

```forth
TOPICS                        \ List documentation topics
LESSONS                       \ List tutorials
DOC getting-started           \ Read a documentation file
TUTORIAL hello-world          \ Follow an interactive tutorial
```

---

## Why Forth?

Forth might seem like an unusual choice for a modern system, but it's remarkably well-suited to this architecture:

**Compactness**: The entire BIOS interpreter, compiler, and 157 built-in words fit in about 13KB of assembled code. Forth is one of the most space-efficient programming environments ever created.

**Interactivity**: Forth is designed for live interaction. You type a word, it executes immediately. This makes development fast and exploratory—perfect for a system where you're building kernels and pipelines on the fly.

**Extensibility**: New words are defined in terms of existing ones. There's no distinction between "system" words and "user" words—everything is in one dictionary, and you can inspect, modify, or extend anything.

**Control**: Forth gives you complete control over memory, I/O, and execution flow. You can write high-level abstractions or drop down to raw machine operations as needed.

The combination of Forth (for orchestration and control) and the tile engine (for data-parallel computation) creates a unique programming model that's both powerful and surprisingly elegant.

---

## Development Status

**Current version: KDOS v0.9d**

The system is stable and comprehensive, suitable for experimentation, education, and research. Recent additions include:


- **v0.9d**: Dictionary search, stack safety utilities, diagnostics
- **v0.9c**: Documentation browser with 10 topics and 5 tutorials
- **v0.9b**: MP64FS file system with create, read, write, delete operations
- **v0.9a**: BIOS v0.5 with 157 core Forth words
- **v0.8**: Advanced kernels and real-world data sources

The test suite has grown from 233 tests (v0.8) to 327 tests (v0.9d), with comprehensive coverage of all subsystems.

---

## Philosophy

Megapad-64 is an exploration of alternative computing architectures. Modern computers are incredibly powerful, but their architecture hasn't fundamentally changed in decades. This project asks: what if we rethought the basics?

What if memory and computation were tightly integrated? What if data-parallel operations were a first-class primitive, not an afterthought? What if the programming environment was built for live interaction and incremental exploration rather than batch compilation?

The result is a system that feels different from conventional computers. It's not trying to be a faster x86 or a better ARM—it's exploring a different point in the design space.

Megapad-64 is also a demonstration that you can build a complete, working computer system from scratch without massive complexity. The entire emulator, BIOS, OS, devices, and test suite fit in about 20,000 lines of readable code. This is a system you can understand completely, modify freely, and learn from deeply.

---

## Future Directions

Possible areas for expansion:

- **Floating-point tile operations**: Add FP32/FP64 SIMD kernels
- **Multithreading**: Run multiple CPU threads with shared tile memory
- **JIT compilation**: Compile Forth words to native Megapad-64 machine code
- **Graphics output**: Add a framebuffer device and display driver
- **Network protocols**: Implement TCP/IP stack over the NIC
- **Self-hosting development**: Write an assembler and compiler in Forth

The architecture is flexible enough to support these extensions while maintaining its core identity.

---

## License

CC0 (for all components not supersceded by existing licenses)

---

## Acknowledgments

This project draws inspiration from:

- The Forth programming language and its interactive, stack-based computing model
- Modern GPU architectures and their data-parallel execution models
- Classic microcomputers like the Commodore 64, where hardware and software formed a unified, exploratory environment
- The RISC-V instruction set for its clean, orthogonal design principles

Megapad-64 is a synthesis of these ideas, reimagined for a hypothetical architecture that never was—but perhaps could be.
