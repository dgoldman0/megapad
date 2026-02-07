# Kernel Dashboard OS (KDOS)

### A Megapad-Centric General-Purpose Computer

**Current Status: KDOS v0.2 — Tile Engine Integration Complete**

---

## Quick Start

### Running KDOS in the Emulator

```bash
# Interactive session (boot BIOS + load KDOS)
python cli.py --bios bios.asm --load kdos.f

# Full test suite (109 tests)
python test_system.py

# Build BIOS binary only
python asm.py bios.asm
```

### Try It Out
```forth
HELP                          \ Full command reference
DASHBOARD                     \ System overview
0 1 128 BUFFER mybuf          \ Create 128-byte buffer
42 mybuf B.FILL               \ Fill with byte value 42
mybuf B.SUM .                 \ Sum via tile engine → prints 5376
mybuf B.MIN .                 \ Min byte → prints 42
0 1 64 BUFFER a               \ Create three 64-byte buffers
0 1 64 BUFFER b
0 1 64 BUFFER c
10 a B.FILL                   \ Fill a with 10
20 b B.FILL                   \ Fill b with 20
a b c B.ADD                   \ Element-wise add a+b → c
c B.DATA C@ .                 \ First byte of c → prints 30
```

---

## Implementation Status

### ✅ Completed (v0.2)

**BIOS v0.4** (115 words, 4069 lines):
- Complete Forth system with colon compiler, conditionals, loops
- All 22 tile engine words: TSRC0!/TSRC1!/TDST!/TMODE!/TCTRL!, TADD/TSUB/TAND/TOR/TXOR, TMUL/TDOT, TSUM/TMIN/TMAX, TTRANS/TZERO, TI/TVIEW/TFILL/CYCLES
- **NEW in v0.4**: ACC@/ACC1@/ACC2@/ACC3@ (read accumulator), TPOPCNT/TL1/TEMIN/TEMAX/TABS, EXECUTE, ' (tick)
- Comment words: `\` (line comment), `(` (paren comment)
- Network device support: NET-STATUS, NET-RX, NET-TX, NET-MAC@

**KDOS v0.2** (520 lines Forth):
- **Buffer subsystem**: Typed tile-aligned buffers with descriptors (up to 16 registered)
- **Tile-aware operations**: B.SUM, B.MIN, B.MAX, B.ADD, B.SUB, B.SCALE (all using MEX)
- **Kernel registry**: Metadata for compute kernels (up to 16 registered)
- **7 sample kernels**: kzero, kfill, kadd, ksum, kstats, kscale, kthresh
- **Dashboard UI**: HELP (command reference), DASHBOARD (system overview), STATUS (one-liner)
- **Benchmarking**: BENCH ( xt -- cycles ) for performance measurement

**Tests**: 109 passing
- 42 KDOS tests (buffer ops, tile operations, kernels, dashboard)
- 42 BIOS tests (all Forth words, compilation, tile engine)
- 25 system tests (UART, Timer, Storage, NIC, DeviceBus, MMIO)

### 🚧 Roadmap to v1.0

**Phase 1: Kernel Pipeline Engine** (not started)
- PIPELINE descriptor: sequence of kernels with buffer routing
- Pipeline compiler: validate buffer types, allocate intermediates
- Pipeline scheduler: dispatch with dependency tracking
- Example: image → blur → threshold → edge-detect → display

**Phase 2: Storage & Persistence** (not started)
- FILE abstraction backed by storage device sectors
- LOAD/SAVE for buffers and kernels
- Persistent kernel library on disk
- Boot from storage instead of UART injection

**Phase 3: Interactive Screens** (not started)
- Screen system: 7 screens (Dashboard, Buffers, Kernels, Pipelines, Perf, Console, Inspector)
- Screen navigation: TAB/Shift-TAB, arrow keys
- Buffer inspector: hex/tile view with cursor
- Performance visualizer: cycle counts, tile utilization

**Phase 4: Advanced Kernels** (not started)
- Matrix operations: GEMM, GEMV via tile engine
- Image processing: convolution, resize, filters
- Signal processing: FFT, correlation
- String/text: search, parse, format

**Phase 5: Scheduler & Preemption** (not started)
- Timer-based preemption for background kernels
- Priority scheduling with fairness
- Kernel cancellation and cleanup

**Phase 6: User Experience** (not started)
- REPL improvements: history, tab completion, multi-line editing
- Error messages with context and suggestions
- Online help with examples
- Tutorials and documentation browser

---

## 1. System Identity

**Kernel Dashboard OS (KDOS)** is a full computer environment where
*applications are kernels*, *data is buffers*, and *the Megapad tile engine
is the primary execution surface*.

The system treats computation as a first-class, inspectable, schedulable
object rather than an opaque process.

The OS boots from ROM, presents a complete interactive environment, and can
be extended dynamically without breaking continuity between firmware and OS.

---

## 2. Architectural Principles

1. **Megapad-first**

   * The tile engine is the primary compute surface.
   * 64-byte tiles with 1/2/4/8-byte element widths; SIMD ALU, multiply,
     reduction, and system ops — all driven through MEX CSRs.
   * Residency (hot / pinned / evictable) is explicit and visible.
   * Performance is explained in terms of tile packing and data movement,
     not hidden caches.

2. **Kernels as Apps**

   * An "app" is a dispatchable kernel or kernel pipeline.
   * Kernels are installable, inspectable, versioned objects.
   * Running an app = dispatching a kernel over buffers.

3. **Buffers as the Universal Medium**

   * All data lives in typed, tile-aligned buffers.
   * Files, streams, datasets, views, UI previews are all buffer-backed.
   * No hidden mutable global state.

4. **Single Interactive Core**

   * One Forth system exists from power-on onward.
   * BIOS Forth is extended, not replaced, by the OS.
   * All UI actions ultimately resolve to Forth words.

---

## 3. Hardware Foundation

KDOS runs on the Megapad-64 system emulator.  The relevant hardware
primitives available today:

### 3.1 CPU Core

* 16 × 64-bit general-purpose registers (R0–R15)
* Full 16-family ISA: ALU, MEM, BRANCH, MULDIV, CSR, MEX (tile), etc.
* Flags: Z, C, N, V, P, G, I, S
* Subroutine calls via CALL.L / RET.L (return-stack based)
* Trap/IVT mechanism for fault handling

### 3.2 Tile Engine (MEX)

The "Megapad" — a 64-byte SIMD tile processor controlled by CSRs:

| CSR | Description |
|---|---|
| TSRC0, TSRC1 | Source tile addresses in RAM |
| TDST | Destination tile address in RAM |
| TMODE | Element width (8/16/32/64-bit), signed/unsigned |
| TCTRL | Tile control (ACC_ZERO, ACC_ACC for accumulation) |
| SB, SR, SC, SW | Cursor position: base, row, column, stride |
| ACC0–ACC3 | 256-bit accumulator for reductions/dot products |

**Tile ALU ops** (TALU): ADD, SUB, AND, OR, XOR, MIN, MAX, ABS — element-wise on src0 × src1 → dst

**Tile multiply** (TMUL): MUL (element-wise), DOT (dot product → ACC)

**Tile reductions** (TRED): SUM, MIN, MAX, POPCNT, L1 — results in ACC

**Tile system** (TSYS): TRANS (8×8 transpose), ZERO, LOADC (cursor load), MOVBANK

Each tile is 64 bytes.  With 8-bit elements: 64 lanes.  With 64-bit
elements: 8 lanes.  All operations are SIMD-parallel across lanes.

**KDOS Integration**: All reduction ops now accessible via ACC@. Multi-tile
accumulation supported via TCTRL (ACC_ACC bit).

### 3.3 Peripherals

| Device | Use |
|---|---|
| **UART** | Serial console — primary I/O for the Forth REPL |
| **Timer** | 32-bit free-running counter with compare-match and IRQ |
| **Storage** | Sector-based block device (512-byte sectors, DMA) |
| **NIC** | Ethernet device with DMA, TX/RX queues, 1500-byte MTU |
| **SysInfo** | Board ID, RAM size, feature flags |

### 3.4 Memory

Flat address space.  Default 256 KiB RAM, configurable up to 64 MiB via
`--ram` flag.  MMIO devices at `0xFFFF_FF00_0000_0000`.

---

## 4. BIOS Forth: The Permanent Nucleus

The BIOS Forth (currently v0.4, 115 words, 4069 lines) is the **permanent,
extensible nucleus** — not replaced, but extended by KDOS.

### 4.1 Current State (v0.4)

The BIOS provides:

* Subroutine-threaded Forth interpreter with outer interpreter loop
* 115 built-in words: stack ops, arithmetic, logic, comparison, memory,
  I/O, hex/decimal modes, FILL, DUMP, WORDS, BYE
* **Colon compiler**: `:` `;` for defining new words
* **Conditionals**: IF/THEN/ELSE
* **Loops**: DO/LOOP, BEGIN/UNTIL
* **Variables & Constants**: VARIABLE, CONSTANT, ALLOT
* **Tile engine**: Full MEX support with accumulator readback
* Dictionary as linked-list with case-insensitive lookup
* Number parser supporting `-`, `0x` prefix, `BASE` variable
* `HERE`, `,`, `C,`, `ALLOT` — basic dictionary extension
* **Comment words**: `\` (line comment), `(` (paren comment)
* **Execution tokens**: `'` (tick), `EXECUTE`
* **Network support**: NET-STATUS, NET-RX, NET-TX, NET-MAC@

All required BIOS extensions for KDOS are **complete** as of v0.4.

---

## 5. KDOS Core Objects (v0.2 Implementation)

### 5.1 Buffer (IMPLEMENTED)

A buffer is a typed, tile-aligned data region with a 32-byte descriptor.

**Implementation (kdos.f §2)**:
```forth
buffer-descriptor:
  +0   type        ( 0=raw  1=records  2=tiles  3=bitset )
  +8   elem_width  ( bytes per element: 1, 2, 4, or 8 )
  +16  length      ( number of elements )
  +24  data_addr   ( pointer to tile-aligned data )
```

**Usage**:
```forth
0 1 128 BUFFER mybuf   \ type=raw, width=1, length=128
mybuf B.TYPE .         \ → 0
mybuf B.LEN .          \ → 128
mybuf B.TILES .        \ → 2 (128 bytes / 64 = 2 tiles)
42 mybuf B.FILL        \ Fill with byte value 42
mybuf B.SUM .          \ Sum via tile engine → 5376
```

**Implemented operations**:
- `B.TYPE`, `B.WIDTH`, `B.LEN`, `B.DATA` — field accessors
- `B.BYTES`, `B.TILES` — derived queries
- `B.FILL`, `B.ZERO` — basic fill operations
- `B.SUM`, `B.MIN`, `B.MAX` — tile-engine reductions via ACC@
- `B.ADD`, `B.SUB` — element-wise SIMD ops on two buffers
- `B.SCALE` — multiply each element by scalar
- `B.INFO` — print descriptor details
- `B.PREVIEW` — hex dump first tile
- `BUFFERS` — list all registered buffers (up to 16)

### 5.2 Kernel (IMPLEMENTED)

A kernel is compute object with metadata for the dashboard.

**Implementation (kdos.f §4)**:
```forth
kernel-descriptor:
  +0   n_inputs    ( number of input buffers )
  +8   n_outputs   ( number of output buffers )
  +16  footprint   ( estimated tile working set )
  +24  flags       ( 0=normal, 1=tile-accelerated )
```

**Usage**:
```forth
: my-kernel ( buf -- )  B.ZERO ;    \ Define kernel word
1 1 0 0 KERNEL my-kernel-desc       \ Register metadata
```

**Implemented sample kernels**:
- `kzero` — zero a buffer
- `kfill` — fill buffer with byte value
- `kadd` — element-wise add two buffers
- `ksum` — sum all bytes, leave result on stack
- `kstats` — compute (sum min max) triple
- `kscale` — multiply buffer by scalar
- `kthresh` — binary threshold: <n→0, >=n→255

**Dashboard**:
- `K.IN`, `K.OUT`, `K.FOOT`, `K.FLAGS` — field accessors
- `K.INFO` — print kernel descriptor
- `KERNELS` — list all registered kernels (up to 16)

### 5.3 Pipeline (NOT YET IMPLEMENTED)

**Planned for v0.3**:

A pipeline is a DAG of kernels with buffer routing.

```forth
pipeline-node:
  +0   kernel_addr  ( pointer to kernel descriptor )
  +8   input_bufs   ( array of buffer addresses )
  +16  output_bufs  ( array of buffer addresses )
  +24  next_nodes   ( array of downstream node addresses )
```

Pipelines enable:
- Automatic dependency tracking
- Incremental recomputation (only re-run changed subgraphs)
- Scheduling optimizations
- Visual DAG display in dashboard

---

## 6. Tile Engine Integration (v0.2)

KDOS v0.2 makes full use of the MEX tile engine for all buffer operations.

### 6.1 Accumulator Readback

**Critical addition**: ACC@ word reads the 256-bit accumulator back to the Forth stack.

Before v0.4, reductions like TSUM/TMIN/TMAX wrote results to ACC0-ACC3 CSRs but there was no way to read them from Forth. Now:

```forth
mybuf B.DATA TSRC0!   \ Point tile engine at buffer
2 TCTRL!              \ ACC_ZERO: clear accumulator
TSUM                  \ Reduce 64 bytes → ACC
ACC@                  \ Read ACC0 to stack → result
```

### 6.2 Multi-Tile Operations

B.SUM demonstrates multi-tile accumulation:

```forth
: B.SUM  ( desc -- n )
    0 TMODE!              \ 8-bit unsigned
    2 TCTRL!              \ Clear ACC before first tile
    DUP B.DATA SWAP B.TILES
    0 DO
        DUP TSRC0!        \ Point at tile
        TSUM              \ Reduce → ACC
        1 TCTRL!          \ Enable accumulation for next tile
        64 +              \ Advance to next tile
    LOOP
    DROP ACC@ ;           \ Read final result
```

### 6.3 Element-Wise Operations

B.ADD uses TADD for SIMD element-wise addition:

```forth
: B.ADD  ( src1 src2 dst -- )
    0 TMODE!
    \ ... setup addresses ...
    ntiles 0 DO
        src1-addr TSRC0!
        src2-addr TSRC1!
        dst-addr  TDST!
        TADD              \ 64-byte SIMD add in one instruction
        \ ... advance pointers ...
    LOOP ;
```

All 64 lanes operate in parallel. For 8-bit data, that's 64 additions per TADD.

### 6.4 Performance

Measured with BENCH:
```forth
0 1 1024 BUFFER bigbuf
42 bigbuf B.FILL
' bigbuf DROP BENCH .     \ Baseline overhead
' bigbuf B.SUM DROP BENCH .  \ Actual tile-engine work
```

Tile operations are ~10-100× faster than byte-by-byte loops for large buffers.

---

## 7. Dashboard & User Interface (v0.2)

### 7.1 HELP System

```forth
HELP
```

Prints full command reference with categories:
- Buffer words: BUFFER, B.INFO, B.SUM, B.ADD, etc.
- Kernel words: KERNEL, K.INFO, KERNELS
- Sample kernels: kzero, kfill, kadd, ksum, kstats, kscale, kthresh
- Bench & tools: BENCH, .BENCH, DASHBOARD, STATUS, HELP

### 7.2 DASHBOARD

```forth
DASHBOARD
```

Shows:
- Memory: HERE address
- Buffers: count + list with descriptors
- Kernels: count + list with metadata

Output example:
```
------------------------------------------------------------
  KDOS v0.2 — Kernel Dashboard OS
------------------------------------------------------------
  Memory:
    HERE  = 22108
  
 --- Buffers (3 ) ---
0  :  [buf  t=0   w=1   n=128   tiles=2   @21504  ]
1  :  [buf  t=0   w=1   n=64    tiles=1   @21696  ]
2  :  [buf  t=0   w=1   n=256   tiles=4   @21824  ]

 --- Kernels (7 ) ---
0  :  [kern  in=1   out=1   foot=0   fl=0  ]
1  :  [kern  in=1   out=1   foot=0   fl=0  ]
2  :  [kern  in=2   out=1   foot=3   fl=1  ]
...
------------------------------------------------------------
```

### 7.3 STATUS

```forth
STATUS
```

One-line summary:
```
KDOS | bufs=3  kerns=7  HERE=22108
```

### 7.4 Benchmarking

```forth
: my-operation ... ;
' my-operation BENCH .     \ Prints cycle count
' my-operation .BENCH      \ Prints "cycles=NNN"
```

Uses the CYCLES word (reads timer MMIO) and EXECUTE for indirect call.

---

## 8. Future Work

### Phase 1: Pipeline Engine (v0.3 target)

**Goal**: DAG-based kernel composition with automatic scheduling.

**Additions needed**:

### 5.4 Scheduler

The scheduler is a Forth vocabulary (`SCHED`) that:
* Allocates tile-aligned RAM regions for buffers
* Dispatches kernels by setting MEX CSRs and executing tile ops
* Batches compatible kernels to minimize pack/unpack overhead
* Tracks residency: which buffers are hot (recently used), pinned
  (user-requested), or cold (evictable)
* Exposes all decisions as inspectable Forth variables

---

## 6. Screen Flow

### Screen 1 — Boot

```
┌──────────────────────────────────────────────────────────────┐
│ MEGAPAD-64 SYSTEM                                            │
├──────────────────────────────────────────────────────────────┤
│ Firmware: BIOS Forth v0.4                                    │
│ RAM: 67108864 bytes (64 MiB)                                 │
│ Tile engine: 64-byte tiles, 8/16/32/64-bit elements          │
│ Storage: present                                             │
│ Timer: enabled                                               │
│                                                              │
│ Choose environment:                                          │
│  [1] Kernel Dashboard OS                                     │
│  [2] Forth Console                                           │
│  [3] Recovery                                                │
│                                                              │
│ Default: 1                                                   │
└──────────────────────────────────────────────────────────────┘
```

### Screen 2 — Home / System Overview

```
┌──────────────────────────────────────────────────────────────┐
│ KERNEL DASHBOARD                                             │
├──────────────────────────────────────────────────────────────┤
│ Installed Kernels                                            │
│  • tile_fill          • transpose_tile                       │
│  • filter_mask_apply  • bitset_intersect_popcnt              │
│  • groupby_reduce     • matmul_tiled                         │
│                                                              │
│ Active Sessions                                              │
│  (none)                                                      │
│                                                              │
│ Tile Memory                                                  │
│  pinned: 96 tiles (6 KiB)                                    │
│  hot:    288 tiles (18 KiB)                                  │
│  free:   15936 tiles (996 KiB)                               │
│                                                              │
│ [Connect Data]  [Build Kernel]  [System Monitor]             │
└──────────────────────────────────────────────────────────────┘
```

### Screen 3 — Connect Data

```
┌──────────────────────────────────────────────────────────────┐
│ CONNECT DATA                                                 │
├──────────────────────────────────────────────────────────────┤
│ Source:                                                      │
│  (•) Storage sector range                                    │
│  ( ) UART stream                                             │
│                                                              │
│ Start sector: 0     Count: 128                               │
│ Format: records (u16 fields)                                 │
│                                                              │
│ Ingest Plan                                                  │
│  tiles needed: 1024                                          │
│  batch size: 128 tiles                                       │
│  rolling window: 8 batches                                   │
│                                                              │
│ [Create Buffer]                                              │
└──────────────────────────────────────────────────────────────┘
```

### Screen 4 — Kernel Dashboard (Apps View)

```
┌──────────────────────────────────────────────────────────────┐
│ KERNEL DASHBOARD                                             │
├──────────────────────────────────────────────────────────────┤
│ Kernels                                                      │
│  [▶] column_sum         footprint: 32 tiles                  │
│  [▶] filter_mask_apply  footprint: 64 tiles                  │
│                                                              │
│ Buffers                                                      │
│  data#01   hot    1024 tiles (64 KiB)                        │
│  mask#02   hot      16 tiles  (1 KiB)                        │
│                                                              │
│ Pipelines                                                    │
│  (none)                                                      │
│                                                              │
│ Tile Heatmap                                                 │
│  [████████░░░░░░░░░░░░░░░░░░░░░░░░]  6%                     │
└──────────────────────────────────────────────────────────────┘
```

### Screen 5 — Buffer Inspector

```
┌──────────────────────────────────────────────────────────────┐
│ BUFFER: data#01                                              │
├──────────────────────────────────────────────────────────────┤
│ Type: records    Elem: u16    Shape: 32768 × 1               │
│ Tiles: 1024      Residency: hot                              │
│                                                              │
│ Lenses                                                       │
│  [Preview] [Histogram] [Top-K] [Nulls] [Tile View]          │
│                                                              │
│ Preview (first tile, u16 elements)                           │
│  0000: 0012 0048 001F 00A3 0017 0055 002B 0090              │
│  0010: 004C 0011 0033 0067 0019 00FF 0042 0028              │
│  0020: 0051 0039 007A 0003 00B1 0064 001E 0044              │
│  0030: 000A 005F 0072 0086 0029 0014 003D 006B              │
│                                                              │
│ Reductions (via TRED)                                        │
│  SUM: 1284903   MIN: 3   MAX: 65521                          │
└──────────────────────────────────────────────────────────────┘
```

### Screen 6 — Kernel Builder

```
┌──────────────────────────────────────────────────────────────┐
│ KERNEL BUILDER                                               │
├──────────────────────────────────────────────────────────────┤
│ Name: column_sum                                             │
│                                                              │
│ Definition (Forth):                                          │
│  : column_sum ( buf -- result )                              │
│    buf>tiles DO                                              │
│      I TSRC0! TSUM                                           │
│    LOOP                                                      │
│    ACC@ ;                                                    │
│                                                              │
│ Footprint: 1 tile (source) + accumulator                     │
│ Deterministic: yes                                           │
│                                                              │
│ [Install Kernel]  [Test]  [Show Tile Plan]                   │
└──────────────────────────────────────────────────────────────┘
```

### Screen 7 — Run Kernel (Pinned as App)

```
┌──────────────────────────────────────────────────────────────┐
│ RUN: column_sum                                              │
├──────────────────────────────────────────────────────────────┤
│ Input:  data#01  (1024 tiles)                                │
│ Output: result#03                                            │
│                                                              │
│ Runtime                                                      │
│  total: 1024 tile ops + 1024 reductions                      │
│  cycles: 48291                                               │
│                                                              │
│ Result                                                       │
│  SUM = 1284903                                               │
│                                                              │
│ [Pin as App]  [Branch Pipeline]  [Compare Runs]              │
└──────────────────────────────────────────────────────────────┘
```

---

## 7. Relationship Between BIOS and OS

* BIOS Forth **never exits** — it is the bottom of the stack
* OS is a set of vocabularies layered on top
* UI screens are Forth words that emit UART escape sequences
* Kernel installation = defining Forth words that drive the tile engine
* Buffer creation = allocating tile-aligned RAM and writing a descriptor
* Scheduling = a Forth word that walks the pipeline DAG

If the OS faults:
* Trap handler drops to the BIOS Forth prompt
* All buffers, kernels, and pipeline state remain in RAM
* User can inspect with `DUMP`, `WORDS`, `@`, and tile ops
* Recovery = dropping vocabularies, not rebooting

---

## 8. Implementation Roadmap

### Phase 1 — BIOS v0.4: Compilation & Control Flow

Extend the BIOS to support colon definitions, IF/ELSE/THEN, DO/LOOP,
BEGIN/UNTIL/WHILE/REPEAT, VARIABLE, CONSTANT, string literals.

This is the prerequisite for writing any OS code in Forth.

### Phase 2 — Buffer & Kernel Subsystem

Implement buffer descriptors, tile-aligned allocation, kernel descriptors.
All as Forth words in a `BUFFERS` and `KERNELS` vocabulary.

### Phase 3 — Storage Module Loader

Implement `LOAD-MODULE` to read Forth source from storage sectors and
interpret it.  This enables loading OS components from disk.

### Phase 4 — Scheduler

Implement the tile-region allocator and kernel dispatcher.  Track
residency, batch compatible operations, expose state.

### Phase 5 — Dashboard UI

Build the screen-based UI as Forth words emitting UART sequences.
Navigation, buffer inspection, kernel management, pipeline view.

### Phase 6 — Pipeline Engine

Implement the DAG data structure, incremental recomputation, and the
"pin as app" mechanism.

---

## 9. Resulting Computer Identity

This machine is:

* not a PC
* not a workstation
* not an accelerator host

It is a **Kernel Computer**:

* where computation is installed as named, inspectable objects
* data is explored live through typed, tile-aligned buffers
* performance is visible in tile ops, cycle counts, and residency state
* the tile engine is the unquestioned center of gravity
* and the Forth console is always one fault away
