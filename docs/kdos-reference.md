# KDOS Word Reference

**KDOS v1.0** — the Kernel Dashboard Operating System — is a Forth-based
OS that runs on top of the Megapad-64 BIOS.  It provides buffers, compute
kernels, pipelines, a cooperative scheduler, a named filesystem, networking,
and an interactive 7-screen TUI dashboard.

This reference documents every word defined in KDOS, organized by the 14
sections of `kdos.f`.  There are **217 colon definitions** and **86
variables/constants/creates** — roughly 303 named entities in total across
2,519 lines of Forth.

> **Notation.**  `( before -- after )` is the Forth stack comment.
> Words from the BIOS are used freely (see `docs/bios-forth.md` for those).
> *desc* means a descriptor address (buffer, kernel, pipe, task, or file).

---

## Table of Contents

1. [§1 Utility Words](#1-utility-words)
2. [§2 Buffer Subsystem](#2-buffer-subsystem)
3. [§3 Tile-Aware Buffer Operations](#3-tile-aware-buffer-operations)
4. [§4 Kernel Registry](#4-kernel-registry)
5. [§5 Sample Kernels](#5-sample-kernels)
6. [§6 Pipeline Engine](#6-pipeline-engine)
7. [§7 Storage & Persistence](#7-storage--persistence)
8. [§7.5 File Abstraction](#75-file-abstraction)
9. [§7.6 MP64FS Filesystem](#76-mp64fs-filesystem)
10. [§7.7 Documentation Browser](#77-documentation-browser)
11. [§7.8 Dictionary Search](#78-dictionary-search)
12. [§8 Scheduler & Tasks](#8-scheduler--tasks)
13. [§9 Interactive Screens (TUI)](#9-interactive-screens-tui)
14. [§10 Data Ports](#10-data-ports)
15. [§11–§12 Benchmarking & Dashboard](#1112-benchmarking--dashboard)
16. [§13 Help System](#13-help-system)
17. [§14 Startup](#14-startup)

---

## §1 Utility Words

Small general-purpose helpers used throughout KDOS.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `.R` | `( n width -- )` | Print number *n* right-justified in a field of *width* characters.  Currently a simplified implementation that drops the width and calls `.`. |
| `SAMESTR?` | `( addr1 addr2 maxlen -- flag )` | Compare two zero-padded byte strings up to *maxlen* bytes.  Returns `-1` if identical, `0` if they differ.  Uses the BIOS `COMPARE` word internally. |
| `PARSE-NAME` | `( "name" -- )` | Parse the next whitespace-delimited word from the input stream and copy it into `NAMEBUF` (a 16-byte scratch buffer), null-terminated.  Sets `PN-LEN` to the parsed length. |
| `NEEDS` | `( n -- )` | Stack safety guard — aborts with an error message if the data stack currently has fewer than *n* items.  Useful at the start of words that need a specific number of arguments. |
| `ASSERT` | `( flag -- )` | Abort with "Assertion failed" if the flag is false (zero).  Useful in tests and sanity checks. |
| `.DEPTH` | `( -- )` | Print the current stack depth in brackets, e.g., `[3 deep]`.  Handy for debugging stack issues. |

**Variables:** `NAMEBUF` (16-byte name scratch buffer), `PN-LEN` (parsed name length).

**Example:**
```forth
3 NEEDS          \ aborts if fewer than 3 items on stack
PARSE-NAME cat   \ copies "cat" into NAMEBUF, PN-LEN = 3
```

---

## §2 Buffer Subsystem

Buffers are the core data container in KDOS.  A buffer is a contiguous,
**tile-aligned** (64-byte aligned) block of memory with a 4-cell (32-byte)
descriptor.  Up to **16 buffers** can be registered in the system.

### Buffer Descriptor Layout

```
Offset   Field         Meaning
───────  ────────────  ─────────────────────────────────────
+0       type          0=raw, 1=records, 2=tiles, 3=bitset
+8       elem_width    Bytes per element (1, 2, 4, or 8)
+16      length        Number of elements
+24      data_addr     Pointer to tile-aligned data region
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `BUFFER` | `( type width length "name" -- )` | **Create a new buffer.**  Allocates a descriptor and a tile-aligned data region.  Registers it in `BUF-TABLE`.  Defines a CONSTANT named *"name"* that pushes the descriptor address.  This is the primary way to create buffers. |
| `B.TYPE` | `( desc -- type )` | Read the buffer type field. |
| `B.WIDTH` | `( desc -- width )` | Read the element width in bytes. |
| `B.LEN` | `( desc -- len )` | Read the element count. |
| `B.DATA` | `( desc -- addr )` | Read the data pointer. |
| `B.BYTES` | `( desc -- n )` | Total data size in bytes (length × width). |
| `B.TILES` | `( desc -- n )` | Number of 64-byte tiles needed to cover the data (ceiling division). |
| `B.FILL` | `( byte desc -- )` | Fill the entire buffer with a byte value. |
| `B.ZERO` | `( desc -- )` | Zero the entire buffer. |
| `B.INFO` | `( desc -- )` | Print a one-line summary: type, width, length, tiles, address. |
| `B.PREVIEW` | `( desc -- )` | Hex-dump the first tile (64 bytes) as 4 rows of 16 bytes.  Useful for quick data inspection. |
| `BUFFERS` | `( -- )` | List all registered buffers with their info. |

**Variables:** `BUF-COUNT`, `BUF-TABLE` (16-slot registry), `BDESC` (internal temp).

**Example — creating and using a buffer:**
```forth
0 1 256 BUFFER my-signal       \ raw, 1 byte/elem, 256 elements
42 my-signal B.FILL             \ fill every byte with 42
my-signal B.INFO                \ prints descriptor summary
my-signal B.PREVIEW             \ hex-dump first 64 bytes
BUFFERS                         \ list all registered buffers
```

---

## §3 Tile-Aware Buffer Operations

These words use the **MEX tile engine** (hardware SIMD) to perform fast
bulk operations on buffers.  They iterate over the buffer one 64-byte tile
at a time, using tile registers `TSRC0!`, `TSRC1!`, `TDST!`, and tile
instructions like `TSUM`, `TMIN`, `TMAX`, `TADD`, `TSUB`.

The default tile mode is `0` (8-bit unsigned, 64 lanes per tile).

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `B.SUM` | `( desc -- n )` | Sum all bytes in the buffer using tile-accelerated reduction.  Iterates over tiles, accumulating with `TSUM`.  Returns the total. |
| `B.MIN` | `( desc -- n )` | Find the minimum byte value across the entire buffer.  Uses per-tile `TMIN`, then takes the minimum across tiles. |
| `B.MAX` | `( desc -- n )` | Find the maximum byte value.  Mirror of `B.MIN`. |
| `B.ADD` | `( src1 src2 dst -- )` | Element-wise addition of two buffers into a destination: `dst[i] = src1[i] + src2[i]`.  All three buffers must have the same tile count.  Uses `TADD` per tile — very fast. |
| `B.SUB` | `( src1 src2 dst -- )` | Element-wise subtraction: `dst[i] = src1[i] − src2[i]`.  Uses `TSUB` per tile. |
| `B.SCALE` | `( n desc -- )` | Multiply every byte in the buffer by *n* in-place.  This is a byte-by-byte loop (not tile-accelerated), clamping results to 0–255. |

**Example — tile-accelerated statistics:**
```forth
my-signal B.SUM .    \ print the sum of all bytes
my-signal B.MIN .    \ print the minimum byte
my-signal B.MAX .    \ print the maximum byte
```

---

## §4 Kernel Registry

A "kernel" in KDOS is a **compute function** (an ordinary Forth colon
word) paired with a **metadata descriptor** that records its input/output
requirements and hardware acceleration status.  Up to **32 kernels** can
be registered.

### Kernel Descriptor Layout

```
Offset   Field         Meaning
───────  ────────────  ─────────────────────────────────────
+0       n_inputs      Number of input buffers expected
+8       n_outputs     Number of output buffers produced
+16      footprint     Estimated tile working set
+24      flags         0 = CPU only, 1 = tile-accelerated
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `KERNEL` | `( n_in n_out footprint flags "name" -- )` | Register a new kernel.  Creates a descriptor and a CONSTANT.  The actual kernel body is a separate colon definition — this just records metadata. |
| `K.IN` | `( desc -- n )` | Number of input buffers. |
| `K.OUT` | `( desc -- n )` | Number of output buffers. |
| `K.FOOT` | `( desc -- n )` | Tile footprint. |
| `K.FLAGS` | `( desc -- flags )` | Flags (0=CPU, 1=tile). |
| `K.INFO` | `( desc -- )` | Print kernel descriptor details. |
| `KERNELS` | `( -- )` | List all registered kernels. |

**Variables:** `KERN-COUNT`, `KERN-TABLE` (32-slot registry), `KDESC` (internal temp).

---

## §5 Sample Kernels

KDOS ships with **18 ready-to-use compute kernels** covering common
signal-processing and data-analysis tasks.  Each kernel is a callable
Forth word, plus a descriptor constant (named `<kernel>-desc`).

### Zero & Fill

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kzero` | `( desc -- )` | Zero an entire buffer. | No |
| `kfill` | `( byte desc -- )` | Fill buffer with a byte value. | No |

### Arithmetic

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kadd` | `( src1 src2 dst -- )` | Element-wise add two buffers → dst.  `dst[i] = src1[i] + src2[i]`. | **Yes** |
| `kscale` | `( n desc -- )` | Multiply every byte by *n* in-place. | No |
| `kinvert` | `( desc -- )` | Bitwise invert: every byte → `255 − val`. | No |

### Statistics & Measurement

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `ksum` | `( desc -- n )` | Sum all bytes, return on stack. | **Yes** |
| `kstats` | `( desc -- sum min max )` | Compute sum, minimum, and maximum in one pass. | **Yes** |
| `kcount` | `( val desc -- count )` | Count bytes matching a specific value. | No |
| `krms-buf` | `( desc -- rms )` | Compute the integer RMS (root mean square) using Newton's method for the square root.  8 iterations. | No |
| `kcorrelate` | `( a b -- dot )` | Dot product of two buffers via tile engine `TDOT`. | **Yes** |

### Signal Processing

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `kthresh` | `( n desc -- )` | Threshold: bytes < n → 0, bytes ≥ n → 255.  Binary binarization. | No |
| `kclamp` | `( lo hi desc -- )` | Clamp all bytes to the range [lo, hi]. | No |
| `kavg` | `( window desc -- )` | Moving average with the given window size (simplified single-pass). | No |
| `kdelta` | `( src dst -- )` | Delta encoding: `out[i] = in[i] − in[i−1]` (first element = 0). | No |
| `knorm` | `( desc -- )` | Normalize buffer to full 0–255 range using tile min/max. | **Yes** |
| `kpeak` | `( thresh src dst -- )` | Peak detector: write 255 at local maxima ≥ threshold, 0 elsewhere. | No |
| `kconvolve3` | `( c0 c1 c2 desc -- )` | 3-tap FIR filter `[c0, c1, c2]` applied in-place, with edge replication. | No |

### Histogram

| Kernel | Stack Effect | Description | Tile? |
|--------|-------------|-------------|-------|
| `khistogram` | `( desc -- )` | Build a 256-bin histogram of all byte values into `hist-bins`. | No |
| `HIST@` | `( v -- count )` | Query histogram bin for byte value *v*. | — |
| `.HIST` | `( -- )` | Print all non-zero histogram bins. | — |

**Scratch buffers:** `mavg-scratch` (256 bytes), `hist-bins` (256×8-byte bins), `conv-scratch` (256 bytes).

**Example — basic signal analysis:**
```forth
0 1 256 BUFFER sensor-data      \ create a 256-byte buffer
\ ... fill with data ...
sensor-data kstats              \ leaves sum min max on stack
." Sum=" . ."  Min=" . ."  Max=" . CR
128 sensor-data kthresh          \ binarize: < 128 → 0, ≥ 128 → 255
sensor-data khistogram           \ build histogram
.HIST                            \ show non-zero bins
```

---

## §6 Pipeline Engine

A pipeline is an **ordered sequence of execution tokens** (XTs) that run
in series.  Think of it as a batch macro: chain several kernel calls
together, then run or benchmark the whole sequence with one word.
Up to **8 pipelines** can be registered.

### Pipeline Descriptor Layout

```
Offset   Field      Meaning
───────  ─────────  ─────────────────────────────────────
+0       capacity   Maximum number of steps
+8       count      Current number of steps
+16      steps[]    Array of execution tokens
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PIPELINE` | `( capacity "name" -- )` | Create a new pipeline with room for *capacity* steps.  Defines a CONSTANT. |
| `P.CAP` | `( pipe -- n )` | Read capacity. |
| `P.COUNT` | `( pipe -- n )` | Read current step count. |
| `P.DATA` | `( pipe -- addr )` | Address of the step array. |
| `P.GET` | `( pipe n -- xt )` | Get the execution token of step *n*. |
| `P.SET` | `( xt pipe n -- )` | Set step *n* to *xt*. |
| `P.ADD` | `( xt pipe -- )` | Append a step, incrementing the count. |
| `P.CLEAR` | `( pipe -- )` | Reset to zero steps. |
| `P.RUN` | `( pipe -- )` | Execute all steps in order. |
| `BENCH` | `( xt -- cycles )` | Time a single word's execution using the cycle counter.  Returns elapsed cycles. |
| `.BENCH` | `( xt -- )` | Time a word and print `cycles=N`. |
| `P.BENCH` | `( pipe -- )` | Execute and individually time each pipeline step. |
| `P.INFO` | `( pipe -- )` | Print pipeline descriptor details. |
| `PIPES` | `( -- )` | List all registered pipelines. |

### Demo Pipelines

KDOS ships with three pre-built demo pipelines:

| Pipeline | Steps | What It Does |
|----------|-------|-------------|
| `pipe-fill-sum` | 2 | Fill `demo-a` with 42, then sum and print. |
| `pipe-add-stats` | 3 | Fill `demo-a`=10 and `demo-b`=20, add them into `demo-c`, print stats. |
| `pipe-thresh` | 3 | Fill `demo-a` with a ramp 0..63, threshold at 32, print stats. |

**Example — building a custom pipeline:**
```forth
8 PIPELINE my-pipe

' my-init  my-pipe P.ADD    \ step 0: initialize
' my-proc  my-pipe P.ADD    \ step 1: process
' my-report my-pipe P.ADD   \ step 2: report results

my-pipe P.RUN               \ run all three steps
my-pipe P.BENCH             \ run and time each step
```

---

## §7 Storage & Persistence

Low-level disk access built on the BIOS disk words.  Provides
buffer-to-disk save/load using sector-based I/O.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DISK?` | `( -- flag )` | True if a storage device is attached (checks bit 7 of `DISK@`). |
| `B.SECTORS` | `( desc -- n )` | Number of 512-byte sectors needed to store this buffer's data. |
| `B.SAVE` | `( desc sector -- )` | Save buffer data to disk starting at the given sector. |
| `B.LOAD` | `( desc sector -- )` | Load buffer data from disk starting at the given sector. |
| `DISK-INFO` | `( -- )` | Print whether storage is present or not. |

**Constant:** `SECTOR` = 512 (bytes per sector).

---

## §7.5 File Abstraction

A **legacy file layer** built on raw sector access — before the named
filesystem (§7.6) was added.  Files here are identified by their starting
sector, not by name.  Up to **8 files** can be open.

### File Descriptor Layout

```
Offset   Field          Meaning
───────  ─────────────  ─────────────────────────────────────
+0       start_sector   First sector on disk
+8       max_sectors    Allocated capacity in sectors
+16      used_bytes     How many bytes have been written
+24      cursor         Current read/write byte offset
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FILE` | `( start_sector max_sectors "name" -- )` | Create a file descriptor backed by disk sectors.  Defines a CONSTANT. |
| `FSEEK` | `( pos fdesc -- )` | Set the cursor to byte position *pos*. |
| `FREWIND` | `( fdesc -- )` | Reset cursor to 0 (start of file). |
| `FSIZE` | `( fdesc -- n )` | Return the used byte count. |
| `FWRITE` | `( addr len fdesc -- )` | Write *len* bytes from *addr* at the current cursor.  Advances cursor.  Bounds-checked against capacity. |
| `FREAD` | `( addr len fdesc -- actual )` | Read up to *len* bytes at cursor into *addr*.  Returns actual bytes read.  Clamps to available data. |
| `F.INFO` | `( fdesc -- )` | Print file descriptor summary. |
| `FILES` | `( -- )` | List all registered legacy file descriptors. |

---

## §7.6 MP64FS Filesystem

The **MP64FS** is a simple on-disk named filesystem that fits on a 1 MiB
(2048-sector) disk image.  It supports up to 64 files with 15-character
names.  See `docs/filesystem.md` for the full on-disk format specification.

### Key Concepts

- **Superblock** (sector 0) — magic number `"MP64"`, version, geometry
- **Bitmap** (sector 1) — 2048-bit allocation map, one bit per sector
- **Directory** (sectors 2–5) — 64 entries × 32 bytes each
- **Data area** (sectors 6–2047) — ~1 MB of usable file storage

### File Type Codes

| Code | Name | Typical Use |
|------|------|-------------|
| 0 | free | Empty directory slot |
| 1 | raw | Binary data |
| 2 | text | Plain text |
| 3 | forth | Forth source code |
| 4 | doc | Documentation topic |
| 5 | data | Structured data |
| 6 | tutorial | Step-by-step lesson |

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `FS-LOAD` | `( -- )` | Load the superblock, bitmap, and directory from disk into RAM.  Checks the `"MP64"` magic.  Sets `FS-OK`. |
| `FS-SYNC` | `( -- )` | Write the in-RAM bitmap and directory back to disk.  Call after any changes. |
| `FS-ENSURE` | `( -- )` | Auto-load the filesystem if not yet loaded. |
| `FORMAT` | `( -- )` | **Initialize a fresh filesystem** on the attached disk.  Writes superblock, clears bitmap (marks metadata sectors 0–5 as allocated), clears directory. |
| `DIR` | `( -- )` | List all files showing name, size, and type.  Also shows a free-space summary. |
| `CATALOG` | `( -- )` | Detailed directory listing with sector start, sector count, byte size, and type. |
| `FIND-BY-NAME` | `( -- slot \| -1 )` | Search the directory for a file matching `NAMEBUF`.  Caller must call `PARSE-NAME` first.  Returns the slot index or −1. |
| `MKFILE` | `( nsectors type "name" -- )` | Create a new file: allocate contiguous sectors, create directory entry, sync.  Checks for duplicate names. |
| `RMFILE` | `( "name" -- )` | Delete a file: free its bitmap sectors, clear the directory entry, sync. |
| `RENAME` | `( "oldname" "newname" -- )` | Rename a file.  Verifies the old name exists and the new name doesn't. |
| `CAT` | `( "name" -- )` | Print a file's contents to the terminal (reads sectors into memory, emits bytes). |
| `FS-FREE` | `( -- )` | Report disk free space: free sectors, bytes, and file count. |
| `SAVE-BUFFER` | `( buf "name" -- )` | Save a KDOS buffer's data to a named file on disk (file must already exist).  Updates `used_bytes` in the directory. |
| `OPEN` | `( "name" -- fdesc \| 0 )` | Open a file by name, returning a file descriptor for `FREAD`/`FWRITE` access.  Returns 0 if not found. |
| `LOAD` | `( "filename" -- )` | Open a Forth source file from disk, read it into memory, and EVALUATE each line.  This is how KDOS extensions and scripts are loaded. |
| `DIRENT` | `( n -- addr )` | Address of directory entry *n* in the RAM cache (for low-level access). |

**Example — filesystem operations:**
```forth
DIR                          \ list all files
CAT getting-started          \ print a file's contents
4 MKFILE my-notes            \ create a 4-sector file of type "doc"
my-buffer SAVE-BUFFER my-data   \ save buffer to existing file
LOAD my-script.f             \ evaluate a Forth source file
FS-FREE                      \ check remaining space
```

---

## §7.7 Documentation Browser

A built-in paging reader for documentation and tutorial files stored on
disk.  Files with type=4 (doc) and type=6 (tutorial) are browsable.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TOPICS` | `( -- )` | List all documentation files on disk (type=doc). |
| `LESSONS` | `( -- )` | List all tutorial files on disk (type=tutorial). |
| `DOC` | `( "name" -- )` | Open and page through a documentation file, pausing every 20 lines with a "--- more ---" prompt. |
| `TUTORIAL` | `( "name" -- )` | Open and walk through a tutorial file (same pagination as DOC). |
| `DESCRIBE` | `( "word" -- )` | Search for a documentation file matching the given word name.  If found, displays it.  If not, suggests using `TOPICS`. |
| `SHOW-FILE` | `( fdesc -- )` | Low-level: page through an open file descriptor with pagination. |
| `OPEN-BY-SLOT` | `( slot -- fdesc \| 0 )` | Open a file by its directory slot index (for internal use). |

**Example:**
```forth
TOPICS              \ see what docs are available
DOC buffers         \ read the "buffers" documentation
LESSONS             \ see what tutorials are available
TUTORIAL hello-world  \ walk through the hello-world tutorial
DESCRIBE ksum       \ look up documentation for a word
```

---

## §7.8 Dictionary Search

Tools for exploring the Forth dictionary — finding words by pattern and
inspecting recent definitions.

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `WORDS-LIKE` | `( "pattern" -- )` | Search the entire dictionary for words whose names contain *pattern* (case-insensitive substring match).  Prints all matches with a count. |
| `APROPOS` | `( "pattern" -- )` | Alias for `WORDS-LIKE`. |
| `.RECENT` | `( n -- )` | Show the last *n* words added to the dictionary, starting from `LATEST`. |
| `ICONTAINS?` | `( pa pl sa sl -- flag )` | Low-level: case-insensitive substring search.  True if the pattern (addr *pa*, len *pl*) appears anywhere in the string (addr *sa*, len *sl*). |
| `ENTRY>NAME` | `( entry -- addr len )` | Extract the name from a dictionary entry (skip 8-byte link + 1-byte flags/len). |
| `ENTRY>LINK` | `( entry -- next )` | Follow the link field to the previous dictionary entry. |

**Example:**
```forth
WORDS-LIKE buf      \ find all words containing "buf"
WORDS-LIKE pipe     \ find all pipeline-related words
APROPOS task        \ find all task-related words
10 .RECENT          \ show the 10 most recently defined words
```

---

## §8 Scheduler & Tasks

KDOS includes a **cooperative multitasking scheduler** with optional
timer-assisted preemption.  Up to **8 tasks** can be registered, each
with a **256-byte private data stack**.

### Task States

| State | Value | Meaning |
|-------|-------|---------|
| `T.FREE` | 0 | Slot is available (no task). |
| `T.READY` | 1 | Task is runnable, waiting for CPU time. |
| `T.RUNNING` | 2 | Task is currently executing. |
| `T.BLOCKED` | 3 | Task is waiting for an external event. |
| `T.DONE` | 4 | Task has finished; can be cleaned up or restarted. |

### Task Descriptor Layout

```
Offset   Field       Meaning
───────  ──────────  ─────────────────────────────────────
+0       status      T.FREE .. T.DONE
+8       priority    0 = highest, 255 = lowest
+16      xt          Execution token (the task body)
+24      dsp_save    Saved data stack pointer
+32      rsp_save    Saved return stack pointer
+40      name_addr   Pointer to name string (or 0)
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `TASK` | `( xt priority "name" -- )` | Create a named task.  Allocates a 256-byte private stack area, initializes the descriptor as READY, and registers it in `TASK-TABLE`.  Defines a CONSTANT. |
| `TASKS` | `( -- )` | List all tasks showing state, priority, xt, and name. |
| `SCHEDULE` | `( -- )` | Run the scheduler: repeatedly find READY tasks and execute them round-robin until no READY tasks remain. |
| `SPAWN` | `( xt -- )` | Create an anonymous READY task with default priority 128. |
| `BG` | `( xt -- )` | Spawn a task and immediately run the scheduler ("background" a task). |
| `KILL` | `( tdesc -- )` | Force a task to DONE state (cancel it). |
| `RESTART` | `( tdesc -- )` | Reset a DONE task back to READY so it can run again. |
| `YIELD` | `( -- )` | Cooperative yield: mark the current task DONE (give up CPU). |
| `YIELD?` | `( -- )` | Conditional yield: check the preemption flag and yield if it's set. Insert this in long-running loops for timer-based preemption support. |
| `FIND-READY` | `( -- tdesc \| 0 )` | Find the first READY task in the table (0 if none). |
| `RUN-TASK` | `( tdesc -- )` | Low-level: set task to RUNNING, execute its XT, mark DONE on return. |
| `TASK-COUNT-READY` | `( -- n )` | Count tasks currently in READY state. |
| `PREEMPT-ON` | `( -- )` | Enable timer-based preemption.  Configures the hardware timer with `TIME-SLICE` cycles (default 50,000) and enables auto-reload.  Yield points (`YIELD?`) will check the preemption flag. |
| `PREEMPT-OFF` | `( -- )` | Disable timer preemption. |

**Variables:** `TASK-COUNT`, `TASK-TABLE`, `CURRENT-TASK`, `SCHED-RUNNING`, `PREEMPT-FLAG`, `TIME-SLICE` (default 50000), `PREEMPT-ENABLED`, `TASK-STACKS` (2048 bytes).

**Example — running background tasks:**
```forth
: blink  ( -- )  ." Blink! " CR ;
: count  ( -- )  10 0 DO I . LOOP CR ;

' blink 100 TASK my-blink    \ priority 100
' count 50 TASK my-count     \ priority 50 (higher)

SCHEDULE              \ run both tasks
\ Output: numbers print first (higher priority),
\         then "Blink!" prints

\ Or spawn and run in one shot:
' blink BG            \ runs immediately
```

### How Preemption Works

KDOS uses a "soft preemption" model.  The hardware timer fires periodically
and sets `PREEMPT-FLAG`.  Long-running tasks should call `YIELD?` at
regular intervals (e.g., inside loops).  When `YIELD?` sees the flag set,
it yields back to the scheduler, which picks the next READY task.  This
avoids the complexity of full preemptive context switching while still
preventing runaway tasks.

---

## §9 Interactive Screens (TUI)

The SCREENS system is a full-screen terminal UI built on **ANSI escape
sequences**.  It provides a tabbed dashboard with 7 screens showing system
status in real time.

### Starting the TUI

```forth
SCREENS     \ enters the interactive dashboard
```

### Navigation Keys

| Key | Action |
|-----|--------|
| `1` – `7` | Switch to screen 1–7 |
| `r` | Refresh the current screen |
| `q` | Quit back to the Forth REPL |

### The 7 Screens

| # | Name | What It Shows |
|---|------|---------------|
| 1 | **Home** | System overview — `HERE` (memory usage), buffer/kernel/pipeline/task/file counts, storage status, network status, scheduler mode, ready task count. |
| 2 | **Buffers** | All registered buffers with type (raw/rec/til/bit), element width, length, tile count, and data address. |
| 3 | **Kernels** | All registered kernels with input/output counts, footprint, and a color-coded `[tile]` or `[cpu]` tag. |
| 4 | **Pipes** | All registered pipelines with capacity and current step count. |
| 5 | **Tasks** | All tasks with **color-coded** state (dim=FREE, green=READY, yellow=RUNNING, red=BLOCKED, dim=DONE), priority, and XT. |
| 6 | **Help** | Quick-reference card listing key commands for all subsystems. |
| 7 | **Docs** | Documentation browser — lists available topics and tutorials from the filesystem, plus doc commands. |

### ANSI Terminal Helpers

These are available for your own use outside of SCREENS:

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PAGE` / `CLS` | `( -- )` | Clear screen and home cursor. |
| `AT-XY` | `( col row -- )` | Move cursor to column, row (1-based). |
| `BOLD` | `( -- )` | Enable bold text. |
| `DIM` | `( -- )` | Enable dim text. |
| `REVERSE` | `( -- )` | Enable reverse video. |
| `RESET-COLOR` | `( -- )` | Reset all text attributes. |
| `FG` | `( n -- )` | Set foreground color (0=black, 1=red, 2=green, 3=yellow, 4=blue, 5=magenta, 6=cyan, 7=white). |
| `BG-COLOR` | `( n -- )` | Set background color. |
| `HBAR` | `( -- )` | Draw a dim 60-character horizontal rule. |
| `SGR` | `( n -- )` | Emit a raw ANSI SGR (Select Graphic Rendition) code. |

---

## §10 Data Ports

The data port system provides **NIC-based external data ingestion**.
External sources send frames over the network; KDOS routes each frame's
payload into a bound buffer based on the source ID.

### Frame Protocol

Every incoming frame has a 6-byte header:

```
Offset   Size   Field          Description
───────  ─────  ─────────────  ─────────────────────────
+0       1      SRC_ID         Source identifier (0–255)
+1       1      DTYPE          Data type (0=raw..5=cmd)
+2       2      SEQ            Sequence number (LE)
+4       2      PAYLOAD_LEN    Payload byte count (LE)
+6       ...    PAYLOAD        Actual data
```

### Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `PORT!` | `( buf id -- )` | Bind a buffer descriptor to source ID *id*.  Incoming frames from that source will be routed to this buffer. |
| `PORT@` | `( id -- buf \| 0 )` | Get the buffer bound to a source ID (0 if unbound). |
| `UNPORT` | `( id -- )` | Unbind a source ID. |
| `POLL` | `( -- id \| -1 )` | Receive and route one frame.  Returns the source ID, or −1 if no frame was available. |
| `INGEST` | `( n -- received )` | Receive and route up to *n* frames.  Returns the actual count received. |
| `RECV-FRAME` | `( -- len )` | Low-level: receive one raw frame into the internal frame buffer. |
| `ROUTE-FRAME` | `( -- id \| -1 )` | Low-level: receive a frame and route its payload to the bound buffer. |
| `.FRAME` | `( -- )` | Print the last received frame's header (source, type, seq, length). |
| `PORTS` | `( -- )` | List all bound ports with stats. |
| `PORT-STATS` | `( -- )` | One-line summary: port count, received frames, dropped frames. |
| `FRAME-SRC` | `( -- id )` | Source ID of the last received frame. |
| `FRAME-TYPE` | `( -- type )` | Data type of the last received frame. |
| `FRAME-SEQ` | `( -- seq )` | Sequence number of the last received frame. |
| `FRAME-LEN` | `( -- len )` | Payload length of the last received frame. |
| `FRAME-DATA` | `( -- addr )` | Address of the payload in the frame buffer. |

**Example — ingesting sensor data from the network:**
```forth
0 1 256 BUFFER sensor    \ create a 256-byte buffer for sensor data
sensor 1 PORT!           \ bind buffer to source ID 1

\ Later, receive data:
10 INGEST .              \ receive up to 10 frames, print count
PORT-STATS               \ show port/rx/drop counts
sensor B.PREVIEW         \ inspect the received data
```

---

## §11–§12 Benchmarking & Dashboard

### Benchmarking

The `BENCH` and `.BENCH` words are defined in §6 (Pipeline Engine) but
are general-purpose:

```forth
' ksum .BENCH    \ times ksum and prints cycle count
```

### Dashboard

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `DASHBOARD` | `( -- )` | Print a comprehensive text-mode system overview: memory, disk, buffers, kernels, pipelines, tasks, files, ports.  Like Screen 1 but in the REPL. |
| `STATUS` | `( -- )` | Quick one-line status showing all subsystem counts (buffers, kernels, pipes, tasks, files, ports). |
| `.MEM` | `( -- )` | Print current memory usage (value of HERE). |
| `HRULE` | `( -- )` | Print 60 dashes. |
| `THIN-RULE` | `( -- )` | Print 40 dots. |

---

## §13 Help System

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `HELP` | `( -- )` | Print a comprehensive online reference for all KDOS subsystems — buffers, kernels, pipelines, storage, filesystem, scheduler, data ports, screens, and more.  This is the "man page" built into the running system. |

```forth
HELP    \ print the full reference
```

The HELP text covers: Buffer words, Kernel words, all 18 sample kernels,
Pipeline words, Storage words, MP64FS filesystem, File I/O, Scheduler
words, Data port words, Screens & tools, Documentation, Dictionary search,
Stack & diagnostics.

---

## §14 Startup

The startup section runs automatically when KDOS loads.  It:

1. Prints the banner: **"KDOS v1.0 — Kernel Dashboard OS"**
2. Prints usage hints: `HELP`, `SCREENS`, `TOPICS`/`LESSONS`
3. If a disk is attached (`DISK?`), automatically loads the filesystem
   (`FS-LOAD`) so DIR, CAT, LOAD, etc. work immediately

No user-callable words are defined here — it's purely the boot sequence.

---

## Quick Reference Card

### Most-Used Words by Task

**Working with buffers:**
```forth
0 1 256 BUFFER name      \ create
42 name B.FILL           \ fill
name B.INFO              \ inspect
name B.SUM .             \ measure
BUFFERS                  \ list all
```

**Running kernels:**
```forth
name kstats              \ sum min max
name khistogram .HIST    \ histogram
name knorm               \ normalize to 0–255
```

**Building pipelines:**
```forth
8 PIPELINE p
' step1 p P.ADD
' step2 p P.ADD
p P.RUN                  \ run
p P.BENCH                \ benchmark
```

**Managing files:**
```forth
DIR                      \ list files
CAT filename             \ print file
LOAD script.f            \ evaluate Forth source
buf SAVE-BUFFER fname    \ save buffer to file
```

**Multitasking:**
```forth
' work BG                \ spawn + run
TASKS                    \ list tasks
SCHEDULE                 \ run all READY tasks
```

**Dashboard:**
```forth
SCREENS                  \ full TUI
DASHBOARD                \ text overview
STATUS                   \ one-liner
HELP                     \ reference
```
