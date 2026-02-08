# Megapad-64

### A Tile-Oriented Computer Architecture & Operating System

Megapad-64 is a complete computer system built around a radical idea: what if your CPU had a massive grid of on-chip memory, and computation happened by running kernels across that memory in fixed-size chunks called tiles?

This project explores that question with a fully functional emulator, a custom assembly language, a Forth-based BIOS, and a complete operating system called KDOS (Kernel Dashboard OS).

---

## What Makes Megapad-64 Different?

Most computers are built around a traditional von Neumann architecture: a CPU fetches instructions, operates on a few registers, and moves data to and from main memory. The Megapad-64 takes a different approach inspired by modern GPUs, vector processors, and data-parallel computing.

**The core idea:** Instead of moving data to the CPU, we bring computation to the data. The Megapad-64's "tile engine" is an on-chip grid of 4,096 tiles, each holding 64 bytes. Kernels—small computational routines—stream across these tiles in parallel, processing data in place.

Think of it as a hybrid between a traditional CPU and a GPU shader core, but designed for general-purpose computing. You get the interactivity and flexibility of a classic microcomputer with the throughput of data-parallel processing.

---

## Architecture Highlights

**Tile Engine**: The heart of the system is a 256KB on-chip tile memory organized as 4,096 tiles of 64 bytes each. Kernels run across these tiles with SIMD operations supporting 1-byte, 2-byte, 4-byte, and 8-byte element widths. Every tile can hold a vector of 64 bytes, 32 words, 16 doublewords, or 8 quadwords.

**General-Purpose CPU**: A 16-register RISC core with 64-bit registers handles control flow, I/O, and orchestration. The CPU and tile engine work together: the CPU prepares data, launches kernels, and processes results.

**Forth-Based System Software**: The BIOS is written in assembly but presents a Forth interpreter. Forth is a stack-based language that's remarkably compact, interactive, and well-suited to systems programming. The entire BIOS fits in about 5,500 lines of assembly and provides 156 Forth words.

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

- **`cpu.py`** — The Megapad-64 CPU emulator (1,600 lines)
- **`bios.asm`** — Forth BIOS in Megapad-64 assembly (5,483 lines, 156 words)
- **`kdos.f`** — Kernel Dashboard OS in Forth (2,393 lines)
- **`test_system.py`** — Comprehensive integration tests (3,956 lines, 327 tests)
- **`asm.py`** — Assembler for Megapad-64 machine code
- **`cli.py`** — Interactive REPL for running the system
- **`diskutil.py`** — MP64FS disk image creation and content injection
- **`KDOS.md`** — Complete technical documentation (1,176 lines)

Additional components include device emulators (UART, NIC, storage, timer), data sources (sensors, audio, images, text), and Python-side utilities for building disk images with pre-loaded content.

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

Create a bootable disk image with pre-loaded documentation and tutorials:

```bash
python diskutil.py build_image disk.img 256
```

Then boot with storage:

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

**Compactness**: The entire BIOS interpreter, compiler, and 156 built-in words fit in about 30KB of assembled code. Forth is one of the most space-efficient programming environments ever created.

**Interactivity**: Forth is designed for live interaction. You type a word, it executes immediately. This makes development fast and exploratory—perfect for a system where you're building kernels and pipelines on the fly.

**Extensibility**: New words are defined in terms of existing ones. There's no distinction between "system" words and "user" words—everything is in one dictionary, and you can inspect, modify, or extend anything.

**Control**: Forth gives you complete control over memory, I/O, and execution flow. You can write high-level abstractions or drop down to raw machine operations as needed.

The combination of Forth (for orchestration and control) and the tile engine (for data-parallel computation) creates a unique programming model that's both powerful and surprisingly elegant.

---

## Development Status

**Current version: KDOS v0.9d**

The system is feature-complete and production-ready for experimentation, education, and research. Recent additions include:

- **v0.9d**: Dictionary search, stack safety utilities, diagnostics
- **v0.9c**: Documentation browser with 10 topics and 5 tutorials
- **v0.9b**: MP64FS file system with create, read, write, delete operations
- **v0.9a**: BIOS v0.5 with 156 core Forth words
- **v0.8**: Advanced kernels and real-world data sources

The test suite has grown from 233 tests (v0.8) to 327 tests (v0.9d), with comprehensive coverage of all subsystems.

---

## Philosophy

Megapad-64 is an exploration of alternative computing architectures. Modern computers are incredibly powerful, but their architecture hasn't fundamentally changed in decades. This project asks: what if we rethought the basics?

What if memory and computation were tightly integrated? What if data-parallel operations were a first-class primitive, not an afterthought? What if the programming environment was built for live interaction and incremental exploration rather than batch compilation?

The result is a system that feels different from conventional computers. It's not trying to be a faster x86 or a better ARM—it's exploring a different point in the design space.

Megapad-64 is also a demonstration that you can build a complete, working computer system from scratch without massive complexity. The entire emulator, BIOS, OS, and test suite fit in about 15,000 lines of readable code. This is a system you can understand completely, modify freely, and learn from deeply.

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

[Specify your license here]

---

## Acknowledgments

This project draws inspiration from:

- The Forth programming language and its interactive, stack-based computing model
- Modern GPU architectures and their data-parallel execution models
- Classic microcomputers like the Commodore 64, where hardware and software formed a unified, exploratory environment
- The RISC-V instruction set for its clean, orthogonal design principles

Megapad-64 is a synthesis of these ideas, reimagined for a hypothetical architecture that never was—but perhaps could be.
