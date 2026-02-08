# Kernel Dashboard OS (KDOS)

### A Megapad-Centric General-Purpose Computer

**Current Status: KDOS v0.8 — Advanced Kernels & Real-World Data Sources**

---

## Quick Start

### Running KDOS in the Emulator

```bash
# Interactive session (boot BIOS + load KDOS via UART)
python cli.py --bios bios.asm --forth kdos.f

# Full test suite (292 tests)
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

\ Pipelines
pipe-fill-sum P.RUN           \ Run built-in pipeline: fill+sum
pipe-add-stats P.RUN          \ Run built-in pipeline: add+stats
3 PIPELINE my-pipe            \ Create a 3-step pipeline
: step1 42 mybuf B.FILL ;
' step1 my-pipe P.ADD         \ Append step
my-pipe P.RUN                 \ Execute all steps
my-pipe P.BENCH               \ Time each step

\ Storage & persistence (requires --storage)
DISK-INFO                     \ Show disk status
mybuf 0 B.SAVE                \ Save buffer to sector 0
mybuf B.ZERO                  \ Clear buffer in RAM
mybuf 0 B.LOAD                \ Load buffer back from disk

\ File abstraction
0 16 FILE myfile              \ File at sector 0, up to 16 sectors
mybuf B.DATA 128 myfile FWRITE  \ Write 128 bytes to file
myfile FREWIND                \ Reset cursor to 0
HERE 128 myfile FREAD .       \ Read back, print bytes read
FILES                         \ List registered files

\ Scheduler & tasks
: my-work 99 . ;              \ Define task body
' my-work 10 TASK my-task     \ Create task, priority 10
TASKS                         \ List all tasks
SCHEDULE                      \ Run all READY tasks
my-task T.INFO                \ Show task status (DONE)
' my-work BG                  \ One-shot: spawn + schedule

\ Interactive TUI
SCREENS                       \ Enter full-screen TUI
                              \ Press 1-6 to switch screens
                              \ Press q to quit, r to refresh

\ Data ports (NIC-based ingestion)
\ Python side: inject frames via data_sources.py
\   from data_sources import CounterSource, inject
\   inject(system, CounterSource(src_id=1, length=64), count=5)
\ Forth side: bind source to buffer, poll for data
0 1 64 BUFFER sensor           \ Create receive buffer
sensor 1 PORT!                 \ Bind source 1 to buffer
POLL .                         \ Receive & route one frame
sensor B.SUM .                 \ Process received data
PORTS                          \ List all port bindings
```

---

## Implementation Status

### ✅ Completed (v0.9b)

**BIOS v0.5** (155 words, ~5200 lines):
- Complete Forth system with colon compiler, conditionals, loops
- **v0.5 additions**: EXIT, >R/R>/R@, J, UNLOOP, +LOOP, AGAIN, S",
  CREATE, IMMEDIATE, STATE, [, ], LITERAL, 0>, <>, 0<>, ?DUP,
  MIN, MAX, CELLS, CELL+, +!, 2*, CMOVE, -ROT, BL, TRUE, FALSE, WORD
- All 22 tile engine words + ACC@/ACC1@/ACC2@/ACC3@, TPOPCNT/TL1/TEMIN/TEMAX/TABS
- Comment words: `\` (line comment), `(` (paren comment)
- Network device support: NET-STATUS, NET-RECV, NET-SEND, NET-MAC@
- Storage device support: DISK@, DISK-SEC!, DISK-DMA!, DISK-N!, DISK-READ, DISK-WRITE
- Timer & interrupt support: TIMER!, TIMER-CTRL!, TIMER-ACK, EI!, DI!, ISR!
- **Non-blocking input**: KEY? (non-blocking key check for interactive TUI)

**KDOS v0.9b** (~2080 lines Forth):
- **Utility words**: CELLS, CELL+, MIN, MAX, ABS, +!, CMOVE, and more
- **Buffer subsystem**: Typed tile-aligned buffers with descriptors (up to 16 registered)
- **Tile-aware operations**: B.SUM, B.MIN, B.MAX, B.ADD, B.SUB, B.SCALE (all using MEX)
- **Kernel registry**: Metadata for compute kernels (up to 16 registered)
- **7 sample kernels**: kzero, kfill, kadd, ksum, kstats, kscale, kthresh
- **11 advanced kernels**: kclamp, kavg, khistogram, kdelta, knorm, kpeak, krms-buf, kcorrelate, kconvolve3, kinvert, kcount
- **Pipeline engine**: Ordered kernel pipelines with per-step timing (up to 8 registered)
- **3 demo pipelines**: fill-sum, add-stats, threshold (with demo buffers)
- **Storage & persistence**: Buffer save/load to disk, file abstraction layer
- **File abstraction**: Sector-backed files with cursor I/O (up to 8 registered)
- **Scheduler & tasks**: Cooperative multitasking with task registry (up to 8 tasks)
- **Task lifecycle**: TASK, SPAWN, KILL, RESTART, BG, YIELD, SCHEDULE
- **Timer preemption**: PREEMPT-ON/PREEMPT-OFF for timer-based preemption
- **Interactive screens**: Full-screen ANSI TUI with 6 screens and keyboard navigation
- **ANSI terminal**: ESC, CSI, AT-XY, PAGE, SGR colors, BOLD, DIM, REVERSE
- **Screen system**: SCREENS entry point, RENDER-SCREEN, HANDLE-KEY event loop
- **Data ports**: NIC-based external data ingestion with frame protocol
- **Port binding**: PORT!, UNPORT, PORT@, 256-slot source→buffer mapping
- **Frame routing**: POLL, INGEST, ROUTE-FRAME, NET-RX?, RECV-FRAME
- **Frame protocol**: 6-byte header (src_id, dtype, seq, len) + payload
- **Python data sources**: data_sources.py — SineSource, CounterSource, RandomSource, ReplaySource, CSVSource
- **Real-world data sources**: TemperatureSource, StockSource, SeismicSource, ImageSource, AudioSource, TextSource, EmbeddingSource, MultiChannelSource
- **Dashboard UI**: HELP, DASHBOARD, STATUS, TASKS, PIPES, FILES, PORTS, DISK-INFO
- **Benchmarking**: BENCH ( xt -- cycles ), P.BENCH for per-step timing
- **String utilities**: .ZSTR, SAMESTR?, NAMEBUF, PARSE-NAME (from input stream via WORD)
- **Comparison operators**: >=, <= (defined atop BIOS < and >)
- **MP64FS file system**: Named on-disk files with bitmap allocation and directory
- **FS operations**: FORMAT, FS-LOAD, FS-SYNC, DIR, CATALOG, MKFILE, RMFILE, OPEN, FFLUSH
- **Bitmap allocator**: BIT-FREE?, BIT-SET, BIT-CLR, FIND-FREE (contiguous sector search)
- **Refactored file I/O**: FWRITE/FREAD with cursor advancement and used_bytes tracking
- **Python diskutil.py**: MP64FS image formatter, file injector/reader/lister/deleter

**Tests**: 292 passing
- 195+ KDOS tests (buffers, tile ops, kernels, advanced kernels, pipelines, storage, files, MP64FS, scheduler, screens, data ports, real-world data sources, end-to-end pipelines, dashboard)
- 73 BIOS tests (all Forth words, compilation, tile engine, disk I/O, timer, KEY?, WORD)
- 12 diskutil tests (pure Python MP64FS image manipulation)
- 24 system tests (UART, Timer, Storage, NIC, DeviceBus, MMIO)

### � Roadmap to v1.0

**Phase 1: Kernel Pipeline Engine** (✅ complete — v0.3)
- PIPELINE descriptor: ordered sequence of execution tokens
- Pipeline registry: up to 8 named pipelines
- P.RUN (execute), P.BENCH (per-step timing), P.INFO, P.CLEAR
- 3 demo pipelines: fill-sum, add-stats, threshold
- Demo buffers: demo-a, demo-b, demo-c

**Phase 2: Storage & Persistence** (✅ complete — v0.4)
- 6 BIOS disk words: DISK@, DISK-SEC!, DISK-DMA!, DISK-N!, DISK-READ, DISK-WRITE
- Buffer persistence: B.SAVE / B.LOAD (DMA to/from disk sectors)
- B.SECTORS, DISK?, DISK-INFO — disk queries
- FILE abstraction: sector-backed files with cursor, up to 8 registered
- FWRITE / FREAD / FSEEK / FREWIND / FSIZE — cursor-based I/O
- F.INFO, FILES — file introspection

**Phase 3: Scheduler & Preemption** (✅ complete — v0.5)
- 6 BIOS timer/interrupt words: TIMER!, TIMER-CTRL!, TIMER-ACK, EI!, DI!, ISR!
- IVT slot 7 (IVEC_TIMER) wired with IRQ delivery in system.py
- Task descriptor: 6-cell (48 bytes) — status, priority, xt, dsp, rsp, name
- Task registry: up to 8 tasks, cooperative scheduling via SCHEDULE
- TASK, SPAWN, KILL, RESTART, BG — task lifecycle management
- YIELD for cooperative release, FIND-READY for round-robin scan
- Timer-based preemption: PREEMPT-ON / PREEMPT-OFF
- Introspection: T.INFO, TASKS, TASK-COUNT-READY

**Phase 4: Interactive Screens** (✅ complete — v0.6)
- 1 BIOS word added: KEY? (non-blocking key check)
- ANSI terminal primitives: ESC, CSI, AT-XY, PAGE, SGR, colors, BOLD, DIM, REVERSE
- .N (number output without trailing space) for formatted screen output
- 6 screens: Home, Buffers, Kernels, Pipelines, Tasks, Help
- Screen framework: SCREEN-HEADER, SCREEN-FOOTER, RENDER-SCREEN, HANDLE-KEY
- SCREENS entry point with event loop (1-6 switch, q quit, r refresh)
- Each screen uses color, bold/dim for visual hierarchy

**Phase 5: Data Ports** (✅ complete — v0.7)
- NIC-based external data ingestion via frame protocol
- 6-byte header: src_id, dtype, seq (LE16), payload_len (LE16)
- Port table: 256 source-to-buffer bindings with PORT!/UNPORT/PORT@
- Frame routing: POLL, INGEST, ROUTE-FRAME — receive NIC frames and copy payload into bound buffers
- NET-RX? convenience, RECV-FRAME, frame header accessors
- Introspection: PORTS, .FRAME, PORT-STATS
- Python data_sources.py: SineSource, CounterSource, RandomSource, ReplaySource, CSVSource
- Injection: inject() for emulator, send_udp() for real hardware bridge
- End-to-end: external data → NIC frame → buffer → tile-engine kernel processing
- +! and CMOVE utility words added to §1

**Phase 6: Advanced Kernels & Real-World Data** (✅ complete — v0.8)
- 11 advanced kernels: kclamp, kavg (moving average), khistogram (256-bin), kdelta (delta encode), knorm (normalize 0-255), kpeak (local maxima), krms-buf (RMS), kcorrelate (tile-engine dot product), kconvolve3 (3-tap FIR), kinvert (bitwise), kcount (match count)
- Histogram analysis: HIST@ (query bin), .HIST (display non-zero bins)
- Real-world Python data sources: TemperatureSource (weather API / synthetic diurnal), StockSource (Brownian motion), SeismicSource (noise + event spikes), ImageSource (gradient/checkerboard/circle/noise patterns), AudioSource (tone/chord/chirp/square), TextSource (lorem/pangram/digits/DNA, file/URL), EmbeddingSource (OpenAI API / synthetic hash embeddings)
- MultiChannelSource: round-robin multiplexing of multiple sources onto single NIC
- End-to-end real-world pipelines tested: temperature→normalize, stock→delta, seismic→peak-detect, image→threshold, audio→smoothing, text→histogram, embedding→correlate, multi-channel→ingest, signal processing chain (normalize→smooth→threshold→count)

**Phase 7: User Experience** — sub-stages:

**v0.9a — BIOS v0.5: Core Forth Completeness** (✅ complete)

The BIOS v0.4 is missing critical standard Forth words (`EXIT`, `>R`/`R>`/
`R@`, `J`, `UNLOOP`, `S"`, `CREATE`, `[`/`]`, `LITERAL`, `IMMEDIATE`,
`STATE`), and KDOS §1 needlessly shadows 8 native BIOS words with slower
high-level Forth versions. This stage upgrades the BIOS foundation before
building higher-level features.

*New BIOS words (~29 additions, 125→~154 words):*

| Category | Words |
|---|---|
| **Return stack** | `>R` `R>` `R@` (IMMEDIATE — compile inline) |
| **Loop** | `J` (outer index), `UNLOOP` (IMMEDIATE), `+LOOP` (IMMEDIATE) |
| **Control** | `EXIT` (IMMEDIATE — compile ret.l), `AGAIN` (IMMEDIATE) |
| **Metaprogramming** | `STATE` `[` `]` `LITERAL` `IMMEDIATE` `CREATE` `S"` |
| **Comparison** | `0>` `<>` `0<>` `?DUP` |
| **Arithmetic** | `MIN` `MAX` `CELLS` `CELL+` `+!` `2*` |
| **Memory** | `CMOVE` `-ROT` |
| **Constants** | `BL` `TRUE` `FALSE` |

*KDOS §1 cleanup:*
- Remove 8 shadow words (ABS, NEGATE, /, MOD, SPACES, 2DROP, NIP, TUCK)
- Remove words now in BIOS (CELLS, CELL+, MIN, MAX, +!, CMOVE, ?DUP, <>, 0<>, -ROT, 2*)
- Fix SAMESTR? (now that EXIT, 0> exist natively)
- Fix PARSE-NAME (now that BL exists natively)

**v0.9b — MP64FS: On-Disk File System** (✅ complete)

Replace the manual sector-range FILE abstraction with a proper named file
system on disk, providing the storage foundation for docs, tutorials, and
persistent user data.

*Disk layout (MP64FS, 1 MiB = 2048 × 512-byte sectors):*
```
Sector 0:      Superblock (magic "MP64", version, geometry)
Sector 1:      Allocation bitmap (256 bytes = 2048 bits)
Sectors 2-5:   Directory (64 entries × 32 bytes each)
Sectors 6+:    Data area (2042 sectors ≈ 1 MB usable)
```

*Directory entry (32 bytes):*
```
+0   name[16]       null-terminated (max 15 chars)
+16  start_sec[2]   starting sector (u16 LE)
+18  sec_count[2]   allocated sectors (u16 LE)
+20  used_bytes[4]  bytes written (u32 LE)
+24  type[1]        0=free 1=raw 2=text 3=forth 4=doc 5=data
+25  flags[1]       bit0=readonly, bit1=system
+26  reserved[6]
```

New Python tool — diskutil.py:
- `format_image()`: create formatted 1 MiB image
- `inject_file()`: allocate sectors, write named file into image
- `read_file()`: read file from image by name
- `list_files()`: list directory
- `delete_file()`: free sectors and remove entry

KDOS rewrite — new Forth words:
- RAM caches: FS-SUPER, FS-BMAP, FS-DIR
- FS-LOAD / FS-SYNC: disk ↔ RAM
- FORMAT, DIR / CATALOG, MKFILE, RMFILE, OPEN
- FWRITE / FREAD with proper cursor advancement
- FFLUSH: persist descriptor metadata

**v0.9c — Documentation & Tutorial Browser** (planned)

Built-in documentation and interactive tutorials stored as MP64FS files.

Documentation format (lightweight markup):
```
#TITLE Buffer Operations
#SECTION Creating Buffers
  0 1 256 BUFFER mydata
#EXAMPLE
  42 mydata B.FILL
  mydata B.PREVIEW
#END
```

New Forth words:
- `DOC` ( "topic" -- ): page through documentation file
- `DESCRIBE` ( "word" -- ): detailed help for one word + example
- `TUTORIAL` ( "name" -- ): interactive step-by-step lesson
- `TOPICS` / `LESSONS`: list available docs and tutorials
- SCR-DOCS: new screen 7 (documentation browser)

Python-side doc builder (diskutil.py extensions):
- `build_docs()`: generate .doc files from structured source
- `build_tutorials()`: generate .tut files
- `build_image()`: format + inject all docs + tutorials

Pre-built content:
- ~10 documentation files (buffers, kernels, pipelines, storage,
  scheduler, screens, data-ports, tile-engine, getting-started, reference)
- ~5 tutorials (hello-world, first-kernel, build-pipeline,
  data-ingest, custom-kernel)

**v0.9d — REPL Improvements & Error Handling** (planned)

Command history:
- HISTORY ring buffer (last 16 commands)
- `HIST`: print history, `!!` re-execute last, `!n` re-execute nth

Error handling:
- Replace bare "?" with descriptive messages
- "Unknown word. Did you mean: ..." (fuzzy dictionary match)
- Stack underflow detection with context

New Forth words:
- `HIST`, `!!` — command history
- `WORDS-LIKE` ( "pattern" -- ): list words containing substring
- `APROPOS` ( "topic" -- ): search word names and help text
- Custom INTERPRET loop with richer error reporting

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

The BIOS Forth (currently v0.4, 128 words, ~4400 lines) is the **permanent,
extensible nucleus** — not replaced, but extended by KDOS.

### 4.1 Current State (v0.4)

The BIOS provides:

* Subroutine-threaded Forth interpreter with outer interpreter loop
* 128 built-in words: stack ops, arithmetic, logic, comparison, memory,
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
* **Storage support**: DISK@, DISK-SEC!, DISK-DMA!, DISK-N!, DISK-READ, DISK-WRITE
* **Timer & interrupt**: TIMER!, TIMER-CTRL!, TIMER-ACK, EI!, DI!, ISR!
* **Non-blocking input**: KEY? (poll UART RX without blocking)

All required BIOS extensions for KDOS are **complete** as of v0.7.

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

### 5.3 Pipeline (IMPLEMENTED)

A pipeline is an ordered sequence of no-argument execution tokens.

**Implementation (kdos.f §6)**:
```forth
pipeline-descriptor:
  +0   capacity    ( max steps )
  +8   count       ( current steps in use )
  +16  steps[]     ( array of XTs, capacity cells )
```

**Usage**:
```forth
3 PIPELINE my-pipe          \ Create 3-step pipeline
: step1 ( -- ) 42 buf B.FILL ;   \ Define step words
: step2 ( -- ) buf B.SUM . ;
' step1 my-pipe P.ADD       \ Append steps
' step2 my-pipe P.ADD
my-pipe P.RUN               \ Execute all steps in order
my-pipe P.BENCH             \ Time each step
```

**Implemented operations**:
- `P.CAP`, `P.COUNT`, `P.DATA` — field accessors
- `P.GET`, `P.SET` — step access by index
- `P.ADD` — append step (auto-increment count)
- `P.CLEAR` — reset pipeline to 0 steps
- `P.RUN` — execute all steps in order
- `P.BENCH` — execute and print cycle count per step
- `P.INFO` — print pipeline descriptor
- `PIPES` — list all registered pipelines (up to 8)

**Built-in demo pipelines** (with demo-a, demo-b, demo-c buffers):
- `pipe-fill-sum` — fill demo-a with 42, sum via tile engine (→ 2688)
- `pipe-add-stats` — fill a=10, b=20, add a+b→c, print stats (→ sum=1920)
- `pipe-thresh` — fill ramp 0..63, threshold at 32, print stats (→ sum=8160)

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

## 7. Storage & Persistence (v0.4)

KDOS v0.4 adds full storage integration — buffers can be persisted to disk,
and a file abstraction provides cursor-based I/O over contiguous sector ranges.

### 7.1 BIOS Disk Words

Six new BIOS words expose the storage device (at MMIO offset 0x0200):

| Word | Stack | Description |
|---|---|---|
| `DISK@` | ( -- status ) | Read device status (bit 7 = present) |
| `DISK-SEC!` | ( n -- ) | Set starting sector number |
| `DISK-DMA!` | ( addr -- ) | Set DMA address in RAM |
| `DISK-N!` | ( n -- ) | Set sector count |
| `DISK-READ` | ( -- ) | Issue read command: disk → RAM via DMA |
| `DISK-WRITE` | ( -- ) | Issue write command: RAM → disk via DMA |

The storage device uses 512-byte sectors and DMA transfers.

### 7.2 Buffer Persistence

```forth
\ Save buffer to disk
mybuf 0 B.SAVE               \ Write mybuf data starting at sector 0

\ Load buffer from disk
mybuf 0 B.LOAD               \ Read mybuf data from sector 0

\ Query
mybuf B.SECTORS .             \ Print sectors needed (ceil(bytes/512))
DISK?                         \ True if storage device present
DISK-INFO                     \ Print "Storage: present" or "not attached"
```

`B.SAVE` and `B.LOAD` compute the number of sectors from the buffer descriptor,
set up DMA address / sector / count, and issue a single DMA transfer.

### 7.3 File Abstraction

A file is a contiguous region of sectors with a cursor for sequential I/O.

**File descriptor** (4 cells = 32 bytes):
```forth
  +0   start_sector   first sector on disk
  +8   max_sectors    allocated size in sectors
  +16  used_bytes     bytes actually written
  +24  cursor         current read/write position (byte offset)
```

**Usage**:
```forth
0 16 FILE myfile              \ File at sector 0, up to 16 sectors
mybuf B.DATA 128 myfile FWRITE  \ Write 128 bytes from buffer
myfile FREWIND                \ Reset cursor to 0
HERE 128 myfile FREAD .       \ Read 128 bytes, print actual count
myfile F.INFO                 \ Print descriptor
FILES                         \ List all registered files
```

**Implemented operations**:
- `FILE ( start max "name" -- )` — create and register a file descriptor
- `FWRITE ( addr len fdesc -- )` — write bytes at cursor position
- `FREAD ( addr len fdesc -- actual )` — read bytes, return actual count
- `FSEEK ( pos fdesc -- )` — set cursor to absolute position
- `FREWIND ( fdesc -- )` — reset cursor to 0
- `FSIZE ( fdesc -- n )` — return used bytes
- `F.START`, `F.MAX`, `F.USED`, `F.CURSOR` — field accessors
- `F.INFO ( fdesc -- )` — print file descriptor
- `FILES ( -- )` — list all registered files (up to 8)

The scratch buffer `FSCRATCH` (512 bytes) is used for unaligned writes
where the offset within a sector is non-zero.

---

## 8. Dashboard & User Interface (v0.2)

### 8.1 HELP System

```forth
HELP
```

Prints full command reference with categories:
- Buffer words: BUFFER, B.INFO, B.SUM, B.ADD, etc.
- Kernel words: KERNEL, K.INFO, KERNELS
- Sample kernels: kzero, kfill, kadd, ksum, kstats, kscale, kthresh
- Pipeline words: PIPELINE, P.ADD, P.RUN, P.BENCH, P.INFO, PIPES
- Storage words: DISK?, DISK-INFO, B.SAVE, B.LOAD, B.SECTORS
- File words: FILE, FWRITE, FREAD, FSEEK, FREWIND, FSIZE, F.INFO, FILES
- Bench & tools: BENCH, .BENCH, DASHBOARD, STATUS, HELP

### 8.2 DASHBOARD

```forth
DASHBOARD
```

Shows:
- Memory: HERE address
- Buffers: count + list with descriptors
- Kernels: count + list with metadata
- Pipelines: count + list with capacities
- Storage: disk status
- Files: count + list

Output example:
```
------------------------------------------------------------
  KDOS v0.4 — Kernel Dashboard OS
------------------------------------------------------------
  Memory:
    HERE  = 24576
  
 --- Buffers (3 ) ---
0  :  [buf  t=0   w=1   n=64   tiles=1   @21504  ]
1  :  [buf  t=0   w=1   n=64   tiles=1   @21632  ]
2  :  [buf  t=0   w=1   n=64   tiles=1   @21760  ]

 --- Kernels (7 ) ---
0  :  [kern  in=1   out=1   foot=0   fl=0  ]
...

 --- Pipelines (3 ) ---
0  :  [pipe cap=2  steps=2  ]
1  :  [pipe cap=3  steps=3  ]
2  :  [pipe cap=3  steps=3  ]

Storage: not attached
 --- Files (0 ) ---
------------------------------------------------------------
```

### 8.3 STATUS

```forth
STATUS
```

One-line summary:
```
KDOS | bufs=3  kerns=7  pipes=3  files=0  disk=no  HERE=24576
```

### 8.4 Benchmarking

```forth
: my-operation ... ;
' my-operation BENCH .     \ Prints cycle count
' my-operation .BENCH      \ Prints "cycles=NNN"
```

Uses the CYCLES word (reads timer MMIO) and EXECUTE for indirect call.

---

## 9. Future Work

### Phase 1: Pipeline Engine (v0.3 target)

**Goal**: DAG-based kernel composition with automatic scheduling.

**Additions needed**:

### 5.4 Scheduler (IMPLEMENTED)

The scheduler provides cooperative multitasking with a task registry
and round-robin dispatching.

**Task descriptor** (6 cells = 48 bytes, kdos.f §8):
```forth
task-descriptor:
  +0   status      ( 0=FREE  1=READY  2=RUNNING  3=BLOCKED  4=DONE )
  +8   priority    ( 0-255, lower = higher priority )
  +16  xt          ( execution token of task body )
  +24  dsp_save    ( saved data stack pointer — reserved )
  +32  rsp_save    ( saved return stack pointer — reserved )
  +40  name_addr   ( address of name string )
```

**Task registry**: `TASK-TABLE` — up to 8 tasks. `TASK-COUNT` tracks count.

**Task lifecycle words**:
| Word | Stack | Description |
|---|---|---|
| `TASK` | `( xt priority "name" -- )` | Create named task |
| `SPAWN` | `( xt -- )` | Anonymous task, priority 128 |
| `KILL` | `( tdesc -- )` | Mark task DONE |
| `RESTART` | `( tdesc -- )` | Mark task READY again |
| `BG` | `( xt -- )` | SPAWN + SCHEDULE |
| `YIELD` | `( -- )` | Cooperatively mark current task DONE |

**Scheduling words**:
| Word | Stack | Description |
|---|---|---|
| `SCHEDULE` | `( -- )` | Run all READY tasks until none remain |
| `FIND-READY` | `( -- tdesc\|0 )` | Find first READY task |
| `RUN-TASK` | `( tdesc -- )` | Execute task, mark DONE on return |

**Timer preemption**:
| Word | Stack | Description |
|---|---|---|
| `PREEMPT-ON` | `( -- )` | Enable timer-based preemption flag |
| `PREEMPT-OFF` | `( -- )` | Disable timer preemption |

**BIOS timer/interrupt words** (bios.asm):
| Word | Stack | Description |
|---|---|---|
| `TIMER!` | `( n -- )` | Set timer compare register |
| `TIMER-CTRL!` | `( n -- )` | Set timer control byte |
| `TIMER-ACK` | `( -- )` | Acknowledge timer interrupt |
| `EI!` | `( -- )` | Enable interrupts |
| `DI!` | `( -- )` | Disable interrupts |
| `ISR!` | `( xt slot -- )` | Install ISR at IVT slot |

---

## 6. Screen Flow (v0.7 Implementation)

KDOS v0.7 provides a full-screen ANSI TUI accessed via the `SCREENS` word.
Six screens are navigable via number keys; `q` quits, `r` refreshes.

All screens share a common header (tab bar) and footer (key hints):

```
 KDOS v0.7  [1]Home [2]Bufs [3]Kern [4]Pipe [5]Task [6]Help
────────────────────────────────────────────────────────────────
  (screen content)

 [1-6] Switch screen  [r] Refresh  [q] Quit
```

The active tab is shown in reverse video.  Bold and dim SGR attributes
provide visual hierarchy within each screen.

### Screen 1 — Home (System Overview)

```
  System Overview

   Memory  : HERE = 45056
   Buffers : 3
   Kernels : 7
   Pipes   : 3
   Tasks   : 0
   Files   : 0
   Storage : present
   Ports   : 0 bound  rx=0  drop=0
   Network : idle
   Files   : 0
   Storage : present          (green if present, red if not)

   Scheduler: cooperative     (or "preempt ON" in green)
   Tasks rdy: 0
```

### Screen 2 — Buffers

```
  Buffers (6)

   0  raw w=1  n=64   tiles=1   @21504
   1  raw w=1  n=64   tiles=1   @21632
   2  raw w=1  n=64   tiles=1   @21760
   3  raw w=1  n=128  tiles=2   @21888
   4  raw w=1  n=256  tiles=4   @22080
   5  raw w=1  n=64   tiles=1   @22400
```

Shows type abbreviation (raw/rec/til/bit), element width, element count,
tile count, and data address for each registered buffer.

### Screen 3 — Kernels

```
  Kernels (7)

   0  1 in 1 out 0 foot [cpu]
   1  1 in 1 out 0 foot [cpu]
   2  2 in 1 out 3 foot [tile]    ← green
   3  1 in 0 out 1 foot [tile]    ← green
   4  1 in 0 out 3 foot [tile]    ← green
   5  1 in 1 out 1 foot [cpu]
   6  1 in 1 out 1 foot [cpu]
```

Tile-accelerated kernels show `[tile]` in green; CPU-only kernels show
`[cpu]` in dim text.

### Screen 4 — Pipelines

```
  Pipelines (3)

   0  cap=2  steps=2
   1  cap=3  steps=3
   2  cap=3  steps=3
```

Shows pipeline capacity and current step count.

### Screen 5 — Tasks

```
  Tasks (3)

   0  READY pri=10 xt=12345    ← green
   1  RUN   pri=5  xt=12400    ← yellow
   2  DONE  pri=20 xt=12500    ← dim
```

Task status is color-coded: READY=green, RUN=yellow, BLOCK=red,
DONE/FREE=dim.

### Screen 6 — Help (Quick Reference)

```
  Quick Reference

  Buffers:
   0 1 N BUFFER name    Create buffer
   buf B.SUM/MIN/MAX    Tile reductions
   a b c B.ADD/SUB      Element-wise ops
   n buf B.SCALE/FILL   Modify buffer
  Kernels:
   1 1 2 0 KERNEL name  Register kernel
   buf kzero/kfill/kadd Sample kernels
  Pipelines:
   3 PIPELINE name      Create pipeline
   ' w pipe P.ADD/RUN   Build & execute
  Tasks:
   ' w 0 TASK name      Create task
   SCHEDULE / BG         Run tasks
  Storage:
   buf sec B.SAVE/LOAD  Persist buffers
   0 16 FILE name       Create file
  Data Ports:
   buf id PORT!          Bind NIC source
   POLL / n INGEST       Receive frames
   PORTS                 List bindings
  Tools:
   DASHBOARD / STATUS    System views
   ' w BENCH / .BENCH   Benchmark
```

Section headers are bold, content is normal text.

### Implementation Notes

The TUI is built entirely from ANSI escape sequences emitted via
the BIOS `EMIT` word — no special terminal hardware is required.

Key BIOS support: `KEY?` (non-blocking input check, added in v0.6)
polls the UART STATUS register (bit 1 = RX_AVAIL) without blocking.

The event loop in `SCREENS` uses `KEY?` to poll for input:
```forth
BEGIN
    KEY? IF KEY HANDLE-KEY THEN
    SCREEN-RUN @
0= UNTIL
```

---

## 7. Data Ports — NIC-Based External Data Ingestion (v0.7)

KDOS v0.7 introduces **Data Ports** — a system for ingesting external data
through the NIC (Network Interface Controller) and routing it directly into
buffers for kernel processing.

### Design Philosophy

The same Forth code works identically whether data arrives from:
- **Test harness**: `inject_frame()` in Python tests
- **Python data sources**: `data_sources.py` programmatic injection
- **Real UDP network**: NIC's built-in UDP passthrough mode
- **Real hardware**: Swap the NIC emulator for a real Ethernet controller

This means kernels developed and tested in the emulator are **immediately
portable** to real hardware — no code changes needed.

### Frame Protocol

All data enters as NIC frames with a 6-byte header:

```
Offset  Size  Field
  +0    u8    SRC_ID       Source identifier (0-255)
  +1    u8    DTYPE        Data type (0=raw, 1=u8, 2=u16, 3=u64, 4=text, 5=cmd)
  +2    u16   SEQ          Sequence number (LE)
  +4    u16   PAYLOAD_LEN  Payload byte count (LE)
  +6    ...   PAYLOAD      Data bytes (up to 1494)
```

### Port Binding

Each source ID (0-255) can be bound to a buffer descriptor:

| Word | Stack | Description |
|---|---|---|
| `PORT!` | `( buf id -- )` | Bind source ID to buffer |
| `UNPORT` | `( id -- )` | Unbind source |
| `PORT@` | `( id -- buf\|0 )` | Query binding (0 = unbound) |

### Frame Receive & Route

| Word | Stack | Description |
|---|---|---|
| `NET-RX?` | `( -- flag )` | Is a NIC frame waiting? |
| `RECV-FRAME` | `( -- len )` | Receive frame into FRAME-BUF |
| `POLL` | `( -- id\|-1 )` | Receive + route one frame |
| `INGEST` | `( n -- received )` | Receive + route up to n frames |

### Frame Header Accessors

After `RECV-FRAME`, the header fields are available:

| Word | Stack | Description |
|---|---|---|
| `FRAME-SRC` | `( -- id )` | Source identifier |
| `FRAME-TYPE` | `( -- type )` | Data type |
| `FRAME-SEQ` | `( -- seq )` | Sequence number |
| `FRAME-LEN` | `( -- len )` | Payload byte count |
| `FRAME-DATA` | `( -- addr )` | Payload start address |

### Introspection

| Word | Stack | Description |
|---|---|---|
| `PORTS` | `( -- )` | List all bound ports |
| `.FRAME` | `( -- )` | Print last received frame header |
| `PORT-STATS` | `( -- )` | Print rx/drop counters |

### Python Data Sources (`data_sources.py`)

The companion Python module provides data source classes that generate
frames matching the protocol:

| Class | Description |
|---|---|
| `SineSource` | Sine wave as U8 vector (amplitude, frequency, offset) |
| `CounterSource` | Incrementing byte counter (easy to verify) |
| `RandomSource` | Pseudo-random bytes (noise injection) |
| `ReplaySource` | Replay a fixed byte sequence in chunks |
| `CSVSource` | Read CSV column as U8 vector frames |

**Injection helpers**:
```python
from data_sources import CounterSource, SineSource, inject, inject_raw

# Inject 10 frames of incrementing bytes
src = CounterSource(src_id=1, length=64)
inject(system, src, count=10)

# Inject a single hand-crafted frame
inject_raw(system, src_id=5, payload=bytes([1,2,3,4]))

# Bridge to real hardware via UDP
from data_sources import send_udp
send_udp(SineSource(src_id=1), host='192.168.1.100', port=9000, count=100)
```

### End-to-End Example

```forth
\ 1. Create a receive buffer
0 1 64 BUFFER sensor

\ 2. Bind NIC source 1 to the buffer
sensor 1 PORT!

\ 3. Python side injects a CounterSource frame (bytes 0-63)
\    (via inject_frame or UDP)

\ 4. Receive and route
POLL .          \ prints 1 (source ID)

\ 5. Process with tile engine
sensor B.SUM .  \ prints 2016 (sum of 0+1+...+63)
sensor B.MIN .  \ prints 0
sensor B.MAX .  \ prints 63

\ 6. Run through a pipeline
3 PIPELINE data-pipe
: step-ingest  sensor 1 PORT!  POLL DROP ;
: step-process sensor B.SUM . ;
: step-report  ." done" CR ;
' step-ingest data-pipe P.ADD
' step-process data-pipe P.ADD
' step-report data-pipe P.ADD
data-pipe P.RUN
```

---

## 10. Relationship Between BIOS and OS

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

## 11. Implementation Roadmap

See the **Roadmap to v1.0** section in Implementation Status above for
detailed phase descriptions and status.

Summary:
- **Phase 1**: Pipeline Engine (✅ v0.3)
- **Phase 2**: Storage & Persistence (✅ v0.4)
- **Phase 3**: Scheduler & Preemption (✅ v0.5)
- **Phase 4**: Interactive Screens (✅ v0.6)
- **Phase 5**: Data Ports (✅ v0.7)
- **Phase 6**: Advanced Kernels & Real-World Data (✅ v0.8)
- **Phase 7**: User Experience (planned)

---

## 12. Resulting Computer Identity

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
