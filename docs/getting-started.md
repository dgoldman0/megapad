# Getting Started with Megapad-64

Welcome to the Megapad-64 — a fantasy computer built around a 64-bit
CPU, a Forth BIOS, and a tile-engine SIMD accelerator.  This guide will
take you from zero to running your first pipeline in about ten minutes.

---

## Prerequisites

You need **Python 3.8+** (3.12 recommended).  The emulator and all tools
run with no external dependencies — pure Python standard library.

For the optional **C++ accelerator** (63× speedup for tests):

```bash
python -m venv .venv && .venv/bin/pip install pybind11 pytest pytest-xdist
make accel
```

```bash
git clone <repository-url>
cd megapad-64
```

Without the C++ extension, everything still works — just slower for
automated test runs.

---

## Your First Boot

### With a Disk Image (recommended)

The normal way to run KDOS is from a disk image.  The BIOS automatically
finds and loads the first Forth file on the disk:

```bash
python diskutil.py sample              # build the sample disk image
python cli.py --bios bios.asm --storage sample.img
```

What happens:

1. The assembler translates `bios.asm` into machine code (~20 KB)
2. The emulator loads the binary at address 0 and starts the CPU
3. The BIOS boots, detects the disk, reads the MP64FS directory
4. It finds `kdos.f` (the first Forth-type file) and loads it via FSLOAD
5. KDOS loads all 16 sections and prints its banner:

```
------------------------------------------------------------
  KDOS v1.1 — Kernel Dashboard OS
------------------------------------------------------------
Type HELP for command reference.
Type SCREENS for interactive TUI.
Type TOPICS or LESSONS for documentation.
```

You're now at the Forth REPL with the full KDOS environment, including
filesystem access (`DIR`, `CAT`, `DOC`, `TUTORIAL`, etc.).

### Without a Disk Image (development mode)

For development, you can inject Forth source directly through the UART
without a disk:

```bash
python cli.py --bios bios.asm --forth kdos.f
```

The CLI sends `kdos.f` line-by-line through the serial port.  This is
slower than disk boot and doesn't give you filesystem access (no `DIR`,
`CAT`, `DOC`, etc.), but it's useful for testing KDOS changes without
rebuilding the disk image.

> **Don't mix the two.**  If you have a disk image with KDOS, you don't
> need `--forth kdos.f` — the BIOS loads it from disk automatically.
> Using `--forth` with `--storage` would load KDOS twice.

### Building the Sample Disk Image

The project ships with a disk builder that creates a fully-populated
image:

```bash
python diskutil.py sample
```

This creates `sample.img` containing KDOS itself, ten documentation
topics, five tutorials, demo data, and a demo pipeline bundle (18 files total).

---

## The REPL — Your Interactive Workspace

Once KDOS is running, you're in a standard Forth REPL.  Type expressions,
press Enter, and the system evaluates them immediately.

### Basic Arithmetic

```forth
2 3 + .            \ Prints: 5
10 3 * .           \ Prints: 30
100 7 MOD .        \ Prints: 2
```

Forth uses **postfix notation** (Reverse Polish).  Numbers go on the
stack; words consume and produce stack values.

### Stack Operations

```forth
1 2 3 .S           \ Shows stack: <3> 1 2 3
SWAP .S            \ <3> 1 3 2
DROP .S            \ <2> 1 3
DUP .S             \ <3> 1 3 3
```

### Defining New Words

```forth
: SQUARE  DUP * ;
5 SQUARE .         \ Prints: 25

: CUBE  DUP DUP * * ;
3 CUBE .           \ Prints: 27
```

### Getting Help

```forth
HELP               \ Full command reference — every KDOS word, grouped by category
DESCRIBE BUF       \ Detailed help for a specific word (stack effect, description)
WORDS              \ List all ~650+ defined words
WORDS-LIKE BUF     \ Find words containing "BUF"
APROPOS TILE       \ Find words related to tiles
```

---

## Creating Your First Buffer

Buffers are KDOS's fundamental data container — a described region of
tile-aligned memory with a known element width and length.

```forth
256 0 1 BUFFER: demo
```

This creates a buffer named `demo` with:
- **256** elements
- **0** type (raw)
- **1** byte per element

### Inspecting It

```forth
demo B.INFO
```

This prints the buffer's type, element width, element count, data address,
total byte size, and tile count.

### Filling and Viewing

```forth
42 demo B.FILL       \ Fill every byte with 42
demo B.PREVIEW       \ Show first 16 bytes as hex

demo B.SUM .         \ Sum all elements → 256 × 42 = 10752
demo B.MIN .         \ Minimum element → 42
demo B.MAX .         \ Maximum element → 42
```

### Zeroing

```forth
demo B.ZERO          \ Clear to all zeros (uses tile TZERO internally)
demo B.SUM .         \ Prints: 0
```

---

## Running Your First Kernel

Kernels are named, registered operations that process buffers.  KDOS ships
with 18 built-in kernels.

### List Available Kernels

```forth
KERNELS
```

This shows every registered kernel with its input/output count, footprint,
and whether it uses the tile engine.

### Sum a Buffer

```forth
256 0 1 BUFFER: my-data
99 my-data B.FILL        \ Fill with 99

my-data ksum             \ Run the 'ksum' kernel — sums all elements
.                        \ Prints: 25344  (= 256 × 99)
```

### Add Two Buffers Together

```forth
256 0 1 BUFFER: a-buf
256 0 1 BUFFER: b-buf
256 0 1 BUFFER: c-buf

10 a-buf B.FILL          \ a = [10, 10, 10, ...]
20 b-buf B.FILL          \ b = [20, 20, 20, ...]

a-buf b-buf c-buf kadd   \ c = a + b (tile-accelerated!)
c-buf B.PREVIEW          \ Shows: 1E 1E 1E ...  (30 = 0x1E)
c-buf B.SUM .            \ Prints: 7680  (= 256 × 30)
```

### Compute Statistics

```forth
my-data kstats           \ Prints sum, min, and max in one call
```

---

## Building Your First Pipeline

Pipelines chain multiple kernel steps into a reusable sequence.

```forth
\ Create a 3-step pipeline
PIPELINE: my-pipe

' my-init    my-pipe P.ADD
' my-compute my-pipe P.ADD
' my-report  my-pipe P.ADD

my-pipe P.RUN            \ Execute all three steps in order
my-pipe P.BENCH          \ Run and print elapsed time
```

### Try the Built-In Demo Pipeline

KDOS ships with `pipe-add-stats`, a demonstration pipeline:

```forth
pipe-add-stats P.INFO    \ Show pipeline structure
pipe-add-stats P.RUN     \ Run it — fills two buffers, adds them, reports stats
```

---

## Exploring the Dashboard

### Quick Status

```forth
STATUS                   \ One-line system overview
```

### Full Dashboard

```forth
DASHBOARD                \ Multi-section view:
                         \   Memory usage, buffer list, kernel registry,
                         \   pipeline list, task list, port list,
                         \   file list, disk info
```

### Interactive TUI

```forth
SCREENS                  \ Launch the 9-screen interactive text UI:
                         \   [1] Home    — system overview
                         \   [2] Buffers — buffer details
                         \   [3] Kernels — kernel registry
                         \   [4] Pipes   — pipeline list
                         \   [5] Tasks   — scheduler view
                         \   [6] Help    — full command reference
                         \   [7] Docs    — browse documentation
                         \   [8] Storage — file browser & disk info
                         \   [9] Cores   — multicore status & dispatch
```

Navigate with number keys and `q` to quit.

---

## Working with Files (Disk Required)

If you booted with `--storage`, you have a full MP64FS filesystem:

```forth
DIR                      \ List all files with size and type
CAT readme               \ Print a file to the terminal
DOC getting-started      \ Page through a documentation topic
TUTORIAL hello-world     \ Walk through a tutorial step by step
TOPICS                   \ List all doc-type files
LESSONS                  \ List all tutorial-type files
```

### Creating and Deleting Files

```forth
4 3 MKFILE mydata        \ Create "mydata" — 4 sectors, type 3 (Forth)
RMFILE mydata            \ Delete it
```

### Saving a Buffer to Disk

```forth
256 0 1 BUFFER: results
42 results B.FILL
4 5 MKFILE results.dat   \ Create a data file (type 5)
results SAVE-BUFFER results.dat
```

---

## The Debug Monitor

At any time while in the console, press **Ctrl+]** to escape to the
debug monitor.  The monitor gives you low-level access to the machine:

```
MP64> regs               Show all CPU registers
MP64> dump 0x0 64        Hex dump memory
MP64> disasm 0x0 16      Disassemble instructions
MP64> step 5             Step 5 instructions
MP64> bp 0x100           Set breakpoint at address 0x100
MP64> console            Return to the Forth REPL
```

Type `help` at the `MP64>` prompt for the full command list.

---

## Running the Test Suite

The project has a comprehensive test suite (1,258+ tests):

```bash
# Full test suite (background — runs via conftest guard)
make test                                # launches in background
make test-status                         # check progress

# Run a specific test class
make test-one K=TestKDOS                 # just KDOS tests

# CPU unit tests only
make test-one K=TestCPU
```

### Fast Tests with PyPy + xdist

The CPU emulator is pure Python and benefits enormously from PyPy's JIT
compiler.  Combined with pytest-xdist for parallel execution, the full
suite runs **~10× faster** (4 min vs 40 min under CPython):

```bash
make setup-pypy   # one-time: downloads PyPy 3.10, installs pytest + xdist
make test          # PyPy + 8 parallel workers (~4 min)
make test-seq      # PyPy sequential (~8 min, good for debugging)
make test-quick    # BIOS + CPU only (~6 sec)
```

PyPy also speeds up interactive use of the emulator:

```bash
.pypy/bin/pypy3 cli.py --bios bios.asm --storage sample.img
```

---

## What to Read Next

| Topic | Document |
|-------|----------|
| All 265 BIOS Forth words | [docs/bios-forth.md](bios-forth.md) |
| All 430+ KDOS definitions | [docs/kdos-reference.md](kdos-reference.md) |
| CPU instruction set | [docs/isa-reference.md](isa-reference.md) |
| System architecture & memory map | [docs/architecture.md](architecture.md) |
| MP64FS filesystem format | [docs/filesystem.md](filesystem.md) |
| Tile engine programming | [docs/tile-engine.md](tile-engine.md) |
| CLI, assembler, & tools | [docs/tools.md](tools.md) |
