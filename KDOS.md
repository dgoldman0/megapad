# Kernel Dashboard OS (KDOS)

### A Megapad-Centric General-Purpose Computer

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
| TCTRL | Tile control / cursor control |
| SB, SR, SC, SW | Cursor position: base, row, column, stride |
| ACC0–ACC3 | Accumulator registers for reductions |

**Tile ALU ops** (TALU): ADD, SUB, AND, OR, XOR, element-wise on src0 × src1 → dst

**Tile multiply** (TMUL): element-wise multiply, dot product

**Tile reductions** (TRED): SUM, MIN, MAX, results in accumulator or R0

**Tile system** (TSYS): FILL (broadcast), TRANSPOSE, ZERO, LOADC (cursor load),
TI (tile info → R0)

Each tile is 64 bytes.  With 8-bit elements: 64 lanes.  With 64-bit
elements: 8 lanes.  All operations are SIMD-parallel across lanes.

### 3.3 Peripherals

| Device | Use |
|---|---|
| **UART** | Serial console — primary I/O for the Forth REPL |
| **Timer** | 32-bit free-running counter with compare-match and IRQ |
| **Storage** | Sector-based block device (512-byte sectors, DMA) |
| **SysInfo** | Board ID, RAM size, feature flags |

### 3.4 Memory

Flat address space.  Default 1 MiB RAM, configurable up to 64 MiB via
`--ram` flag.  MMIO devices at `0xFFFF_FF00_0000_0000`.

---

## 4. BIOS Forth: The Permanent Nucleus

The BIOS Forth (currently v0.3, 62 words, 5650 bytes) must be extended into
a **permanent, extensible nucleus** — not replaced.

### 4.1 Current State (v0.3)

The BIOS already provides:

* Subroutine-threaded Forth interpreter with outer interpreter loop
* 62 built-in words: stack ops, arithmetic, logic, comparison, memory,
  I/O, hex/decimal modes, FILL, DUMP, WORDS, BYE
* Dictionary as linked-list with case-insensitive lookup
* Number parser supporting `-`, `0x` prefix, `BASE` variable
* `HERE`, `,`, `C,`, `ALLOT` — basic dictionary extension
* 22 tile engine words: TVIEW, TFILL, TSRC0!, TSRC1!, TDST!, TMODE!,
  TCTRL!, TADD, TSUB, TAND, TOR, TXOR, TMUL, TDOT, TSUM, TMIN, TMAX,
  TTRANS, TZERO, TI, CYCLES

### 4.2 Required Extensions for KDOS

The following must be added to the BIOS Forth (v0.4+) to support the OS:

#### Compilation & Control Flow

| Word | Stack effect | Description |
|---|---|---|
| `:` | ( "name" -- ) | Begin colon definition |
| `;` | ( -- ) | End colon definition |
| `IF` | ( flag -- ) | Conditional branch |
| `ELSE` | ( -- ) | Alternative branch |
| `THEN` | ( -- ) | End conditional |
| `DO` | ( limit index -- ) | Begin counted loop |
| `LOOP` | ( -- ) | End counted loop |
| `I` | ( -- n ) | Loop index |
| `BEGIN` | ( -- ) | Begin indefinite loop |
| `UNTIL` | ( flag -- ) | Loop until true |
| `WHILE` | ( flag -- ) | Loop while true |
| `REPEAT` | ( -- ) | End BEGIN..WHILE loop |
| `VARIABLE` | ( "name" -- ) | Create variable |
| `CONSTANT` | ( n "name" -- ) | Create constant |
| `CREATE` | ( "name" -- ) | Create dictionary entry |
| `DOES>` | ( -- ) | Define runtime behavior |

#### String & I/O

| Word | Stack effect | Description |
|---|---|---|
| `."` | ( -- ) | Print inline string |
| `S"` | ( -- addr len ) | String literal |
| `TYPE` | ( addr len -- ) | Print counted string |
| `ACCEPT` | ( addr max -- n ) | Read line into buffer |
| `SPACE` | ( -- ) | Emit a space |
| `SPACES` | ( n -- ) | Emit n spaces |

#### Vocabulary / Namespace Support

| Word | Stack effect | Description |
|---|---|---|
| `VOCABULARY` | ( "name" -- ) | Create new vocabulary |
| `DEFINITIONS` | ( -- ) | Set compilation vocabulary |
| `ALSO` | ( -- ) | Add vocabulary to search order |
| `ONLY` | ( -- ) | Reset search order to root |

This lets the OS install words into `KERNELS`, `BUFFERS`, `SCHED`, `UI`
vocabularies without polluting the root namespace.

#### Module Loader

| Word | Stack effect | Description |
|---|---|---|
| `LOAD-MODULE` | ( addr len -- ) | Load Forth source from storage |
| `INCLUDE` | ( "filename" -- ) | Include from storage by name |

These use the storage controller to read sectors, interpret the contents as
Forth source, and install the words into the current vocabulary.

#### Trap Recovery

On fault, the system drops to the Forth prompt with:
* Panic record (trap type, faulting PC, registers)
* Last kernel name
* Buffer IDs involved
* OS state remains inspectable from the console

---

## 5. Core OS Objects

### 5.1 Buffer

A buffer is a first-class Forth object — a dictionary entry pointing to
a descriptor in RAM.

```
buffer-descriptor:
  +0x00  type       ( u8: 0=raw, 1=records, 2=tiles, 3=bitset, 4=tensor )
  +0x01  flags      ( u8: bit0=pinned, bit1=hot, bit2=dirty )
  +0x02  elem_width ( u8: 1/2/4/8 bytes — matches TMODE )
  +0x03  reserved
  +0x04  shape[0]   ( u32: rows or length )
  +0x08  shape[1]   ( u32: columns or 0 )
  +0x0C  tile_count ( u32: number of 64-byte tiles )
  +0x10  data_addr  ( u64: pointer to tile-aligned data )
  +0x18  prov_addr  ( u64: pointer to provenance record )
  +0x20  size = 32 bytes
```

Buffers are tile-aligned: their data region starts on a 64-byte boundary
so the tile engine can operate directly on them without packing.

**Lenses** are Forth words that operate on buffer descriptors:
`PREVIEW`, `HISTOGRAM`, `TOPK`, `NULLS`, `TILE-VIEW`.

### 5.2 Kernel

A kernel is a schedulable compute object — a dictionary entry pointing to
a kernel descriptor.

```
kernel-descriptor:
  +0x00  name_addr   ( u64: pointer to name string )
  +0x08  code_addr   ( u64: pointer to kernel body — Forth xt or native )
  +0x10  n_inputs    ( u8 )
  +0x11  n_outputs   ( u8 )
  +0x12  flags       ( u8: bit0=deterministic, bit1=native )
  +0x13  reserved
  +0x14  footprint   ( u32: estimated tiles needed )
  +0x18  param_addr  ( u64: pointer to parameter schema )
  +0x20  size = 32 bytes
```

A kernel body is either:
* A Forth word (colon definition using tile primitives)
* A native code address (hand-written assembly for hot paths)

### 5.3 Pipeline

A pipeline is a DAG of kernels and buffers.

```
pipeline-node:
  +0x00  kernel_addr  ( u64: pointer to kernel descriptor )
  +0x08  input_bufs   ( u64: pointer to array of buffer addrs )
  +0x10  output_bufs  ( u64: pointer to array of buffer addrs )
  +0x18  next_nodes   ( u64: pointer to array of downstream node addrs )
  +0x20  size = 32 bytes
```

Pipelines are incrementally recomputed: when an input buffer changes, only
the dependent downstream nodes re-execute.  A pinned pipeline is an "app."

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
