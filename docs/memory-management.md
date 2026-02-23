# Memory Management on Megapad-64

This document describes how memory works on the Megapad-64 — the
physical regions, the allocators built on top of them, the multicore
model, and the design philosophy tying it all together.  It is the
single reference for anyone asking "how should I manage memory on
this system?"

---

## 1. Physical Memory Map

The Megapad-64 has no MMU.  Every address a program uses is a physical
address.  The hardware exposes four distinct memory regions, each on a
different bus with different characteristics:

```
 Address              Region          Size    Backing      Latency
 ─────────────────────────────────────────────────────────────────
 0x0000_0000          Bank 0          1 MiB   On-chip BRAM  1 cycle
   ├─ BIOS code + dict (~20 KB)
   ├─ KDOS dictionary  (~50 KB)
   ├─ HERE ↓ (grows up)
   │    ... free heap space ...
   ├─ data stack SP ↑ (grows down)
   └─ return stack
 0x000F_FFFF

 0x0010_0000          External RAM   ≤16 MiB  HyperRAM/SDRAM  6+ cycles
   ├─ Userland dictionary (1 MiB zone)
   └─ XMEM allocator region
 0x00FF_FFFF+ (configurable)

 0xFF00_0000          VRAM            4 MiB   Dedicated FB  varies
 0xFF3F_FFFF

 0xFFD0_0000          Banks 1–3      3 MiB   On-chip BRAM  1 cycle*
 0xFFFF_FFFF          (HBW math RAM)          (* tile has priority)

 0xFFFF_FF00_         MMIO           ~3 KB    Registers     1 cycle
 0000_0000+
```

### Why three heaps?

Because these are three different physical buses.  Address `0x1000` in
Bank 0 and address `0x101000` in XMEM are not the same memory accessed
through different names — they are different wires, different chips,
different latencies.  A programmer who doesn't know which bus they're
on will write code that "works" but runs 6× slower than necessary.

On a machine with an MMU, the OS hides this behind a flat virtual
address space.  That's useful when you have hundreds of processes that
shouldn't see each other, but it's actively harmful when you're one
programmer trying to write fast code.  The Megapad-64 keeps the map
visible because **the map is the performance model**.

---

## 2. The Five Allocation Strategies

The system provides five ways to obtain memory, each designed for a
specific lifetime and access pattern.

### 2.1 Dictionary — `ALLOT`, `CREATE`, `:`

The Forth dictionary grows upward from `HERE`.  Every `:` definition,
`CREATE`, `VARIABLE`, or `ALLOT` advances `HERE`.

- **Lifetime:** permanent until `MARKER` or `FORGET`.
- **Reclaim:** `MARKER` saves HERE/LATEST; executing the marker word
  rewinds the dictionary to that point.  `FORGET name` rewinds to
  `name` and unlinks everything after it.
- **Region:** Bank 0 (system dictionary) or External RAM (userland
  dictionary, see §6).
- **Cost:** O(1) per allocation, O(1) reclaim via marker.

```forth
VARIABLE MY-DATA          \ 8 bytes at HERE, permanent
CREATE SCRATCH 256 ALLOT  \ 256 bytes at HERE, permanent
MARKER CLEAN              \ snapshot
: FOO  ... ;              \ compiles past HERE
CLEAN                     \ rewind — FOO is gone
```

**When to use:** kernel definitions, long-lived data structures,
anything that should survive until the next `MARKER`.

**When not to use:** temporary buffers, per-request scratch, anything
you want to free independently.

### 2.2 Heap — `ALLOCATE`, `FREE`, `RESIZE`

A first-fit free-list allocator in Bank 0.  Each block carries a
16-byte header (next pointer + size).  `FREE` inserts addresses in
sorted order and coalesces adjacent blocks.

- **Lifetime:** until explicitly `FREE`d.
- **Region:** Bank 0, above `HERE`, below the data stack.
- **Cost:** O(n) where n = number of free blocks.
- **Constraint:** core-0 only (`?CORE0` guard).

```forth
1024 ALLOCATE ABORT" OOM"   ( addr )
\ ... use addr ...
FREE                        \ return to free list
```

**Diagnostic words:**

| Word | Stack | Purpose |
|------|-------|---------|
| `.HEAP` | `( -- )` | Print base, free bytes, fragments, largest block, safety check |
| `HEAP-FREE-BYTES` | `( -- u )` | Total free bytes across all fragments |
| `HEAP-FRAG` | `( -- n )` | Number of free-list nodes (fragmentation = n−1) |
| `HEAP-LARGEST` | `( -- u )` | Size of the largest contiguous free block |
| `HEAP-CHECK` | `( -- flag )` | True if heap hasn't collided with the data stack |

**When to use:** objects with independent lifetimes, growable buffers
(via `RESIZE`), anything that doesn't fit the arena or dictionary model.

**When not to use:** tight loops that allocate/free rapidly (fragmentation
risk without an MMU to hide it), secondary cores (uses shared scratch
variables).

### 2.3 Bump Allocators — `XMEM-ALLOT`, `HBW-ALLOT`

Simple pointer-advance allocators for the two large memory regions.

#### XMEM (External RAM)

```forth
XMEM-ALLOT   ( u -- addr )   \ bump-allocate u bytes
XMEM-RESET   ( -- )          \ reset pointer to floor (bulk free)
XMEM-FREE    ( -- u )        \ bytes remaining
.XMEM        ( -- )          \ print status
XMEM?        ( -- flag )     \ hardware present?
```

XMEM also has a **free-list** for individual block reclaim.  When an
arena backed by XMEM is destroyed, its backing block is returned to
the XMEM free-list via `XMEM-FREE-BLOCK`.  Subsequent `XMEM-ALLOT`
calls check the free-list (first-fit) before falling back to the bump
pointer.  `XMEM-RESET` clears the free-list along with the pointer.

**Floor protection:** `XMEM-FLOOR` marks the boundary between
kernel-reserved XMEM allocations (file buffers loaded at boot) and
the general-purpose region.  `XMEM-RESET` will not reclaim below
the floor.

#### HBW (Math RAM)

```forth
HBW-ALLOT    ( u -- addr )   \ bump-allocate u bytes
HBW-RESET    ( -- )          \ reset pointer (bulk free)
HBW-TALIGN   ( -- )          \ align to 64-byte tile boundary
HBW-FREE     ( -- u )        \ bytes remaining
.HBW         ( -- )          \ print status
```

HBW is a pure bump allocator with no individual free.  It has no
free-list.  In practice, HBW arenas are short-lived tile/SIMD
scratch buffers, and HBW is large (3 MiB), so abandoned slivers
are tolerable.

**When to use XMEM:** large datasets, file contents, network buffers,
map data — anything big that doesn't need 1-cycle latency.

**When to use HBW:** tile engine working buffers, SIMD scratch, any
data the tile engine or DMA will access.  HBW banks have 512-bit
tile-width ports with tile priority; Bank 0 accesses compete with the
CPU.

### 2.4 Arenas — `ARENA-NEW`, `ARENA-ALLOT`, `ARENA-DESTROY`

Region-based scoped allocation.  An arena pre-allocates a backing block
from any of the three regions, then provides O(1) bump allocation
within it and O(1) bulk deallocation.  No per-object headers, no
free-list, no fragmentation within the arena.

Full specification: [arenas.md](arenas.md)

```
┌─────────────────────────────────────────────┐
│  Arena backing region                        │
│  ┌──────┬──────┬──────┬────────────────┐    │
│  │ obj1 │ obj2 │ obj3 │    free        │    │
│  └──────┴──────┴──────┴────────────────┘    │
│  ^base                ^ptr             ^end │
└─────────────────────────────────────────────┘
```

#### Core API

```forth
\ Create
4096 A-HEAP ARENA-NEW CONSTANT scratch    \ permanent descriptor
CREATE my-desc 32 ALLOT                   \ or: user-placed descriptor
my-desc 4096 A-XMEM ARENA-NEW-AT         \ no dictionary leak

\ Use
scratch 256 ARENA-ALLOT   ( addr )        \ O(1) bump
scratch 128 ARENA-ALLOT   ( addr2 )       \ O(1) bump

\ Inspect
scratch ARENA-USED  ( bytes-consumed )
scratch ARENA-FREE  ( bytes-remaining )

\ Rewind
scratch ARENA-RESET                       \ all allocs logically freed, O(1)

\ Destroy
scratch ARENA-DESTROY                     \ backing freed, descriptor zeroed
```

#### Sources

| Constant | Value | Backing region |
|----------|-------|----------------|
| `A-HEAP` | 0 | Bank 0 heap (`ALLOCATE`/`FREE`) |
| `A-XMEM` | 1 | External RAM (XMEM bump + free-list) |
| `A-HBW` | 2 | HBW math RAM (bump only) |

#### Snapshots

```forth
scratch ARENA-SNAP       ( snap )         \ save pointer
  \ ... tentative work ...
  ok? IF DROP ELSE scratch SWAP ARENA-ROLLBACK THEN
```

#### Scoped Arena Stack

```forth
scratch ARENA-PUSH                        \ push onto 4-deep stack
  256 AALLOT                              \ allocate from current arena
ARENA-POP
```

This supports **allocation-polymorphic code** — library words that call
`AALLOT` without knowing which arena (or which region) is current.

#### Arena-Scoped Buffers

```forth
65536 A-XMEM ARENA-NEW CONSTANT map-arena
2 8 4096 map-arena ARENA-BUFFER tile-data  \ buffer lives in the arena
map-arena ARENA-DESTROY                     \ buffer auto-unregistered
```

**When to use:** any pattern where multiple allocations share a
lifetime — parse buffers, tile map scratch, per-task working memory,
transactional computation, undo stacks.

**When not to use:** objects with independent lifetimes (use the heap),
data that must persist across arena resets, objects that need `RESIZE`.

### 2.5 Userland Dictionary — `ENTER-USERLAND`

When external RAM is present, KDOS partitions the first 1 MiB of XMEM
as a userland dictionary zone.  `ENTER-USERLAND` redirects `HERE` into
this zone; all subsequent `:` definitions, `CREATE`, `VARIABLE`, etc.
compile there instead of Bank 0.

```forth
ENTER-USERLAND      \ HERE → XMEM userland zone
: MY-WORD  ... ;    \ compiled in XMEM
LEAVE-USERLAND      \ HERE → Bank 0 system dictionary
```

This protects the kernel dictionary from overflow when loading large
user modules (like `tools.f`).  System words in Bank 0 remain
accessible — the dictionary search walks from LATEST regardless of
where HERE points.

---

## 3. Memory Lifetime Hierarchy

```
Lifetime         Strategy           Reclaim             Region
────────────────────────────────────────────────────────────────
Permanent        Dictionary         MARKER / FORGET     Bank 0 / XMEM
Long-lived       Heap ALLOCATE      FREE                Bank 0
Scoped           Arena              ARENA-DESTROY       Any
Transactional    Arena + snapshot    ARENA-ROLLBACK      Any
Ephemeral        Bump (raw)         XMEM-RESET /        XMEM / HBW
                                    HBW-RESET
```

The discipline is: **choose the shortest lifetime that fits**.

- If the data outlives the current operation → dictionary or heap.
- If it dies with the current operation → arena.
- If it dies with the current transaction → arena + snapshot.
- If it dies with the entire session → bump reset.

Arenas cover the vast majority of real workloads.  The heap exists for
the cases where objects have truly independent lifetimes.  The
dictionary is for code and long-lived data.

---

## 4. Fragmentation: Why It Matters Here

On a system with an MMU, external fragmentation is invisible — the OS
maps scattered physical pages behind contiguous virtual addresses.
**The Megapad-64 has no MMU.**  If the heap fragments into 100 small
blocks with no large contiguous region, a big allocation fails even
though the total free bytes are sufficient.  There is no virtual
memory to paper over it.

The defenses:

1. **Arenas eliminate fragmentation within their scope.**  Bump
   allocation can't fragment.  When the arena is destroyed, 100% of
   its backing is returned as a single contiguous block.

2. **The heap coalesces on FREE.**  Adjacent free blocks are merged
   immediately (forward + backward coalescing).  This keeps
   fragmentation bounded for well-behaved alloc/free patterns.

3. **HEAP-FRAG tells you the truth.**  A perfectly defragmented heap
   has `HEAP-FRAG` = 1 (one large free block).  Higher values mean
   the free list has been split.  Monitor this during development.

4. **XMEM and HBW use bump allocators.**  No fragmentation is
   possible within the bump region.  The XMEM free-list (for
   destroyed arenas) can fragment over many create/destroy cycles,
   but `XMEM-RESET` clears it completely.

**The system pushes you toward arenas precisely because arenas cannot
fragment.**  This is the correct default on a machine with no virtual
memory.

---

## 5. Multicore Memory Model

The Megapad-64 has 4 full cores and up to 12 micro-cores (3 clusters
of 4).  There is no hardware cache coherency protocol.  All cores
share the same physical address space but there is no shared-state
locking on any allocator.

### 5.1 The Concurrency Contract

The allocators use shared scratch `VARIABLE`s (`A-PREV`, `A-CURR`,
`A-SIZE`, `AR-SZ`, `AR-SRC`, `AR-BLK`, `FL-PREV`, `FL-CURR`).  Two
cores calling `ALLOCATE` concurrently would corrupt these variables
and silently destroy the free list.

The rule: **all allocator words that touch shared state are core-0
only.**  They enforce this with `?CORE0`, which aborts with a clear
message if `COREID` ≠ 0.

| Core-0 only | Why |
|---|---|
| `ALLOCATE`, `FREE`, `RESIZE` | Shared heap free-list + scratch |
| `ARENA-NEW`, `ARENA-NEW-AT` | Uses AR-SZ, AR-SRC, AR-BLK |
| `ARENA-DESTROY` | Calls `FREE` or `XMEM-FREE-BLOCK` |

| Safe on any core | Why |
|---|---|
| `ARENA-ALLOT`, `ARENA-ALLOT?` | Pure stack + one arena-local pointer |
| `ARENA-FREE`, `ARENA-USED` | Read-only |
| `ARENA-SNAP`, `ARENA-ROLLBACK` | Single pointer write to own arena |
| `AALLOT` | Delegates to `ARENA-ALLOT` via current-arena |
| `@`, `!`, `C@`, `C!`, `MOVE`, `FILL` | Direct memory access |

### 5.2 The Pattern: Core 0 as Memory Manager

Core 0 is the system's software MMU.  It performs the three MMU
functions — allocate, map, and protect — explicitly at task setup
time rather than implicitly on every memory access:

```forth
\ ── Core 0: set up ──
4096 A-XMEM ARENA-NEW CONSTANT c1-arena   \ allocate
4096 A-XMEM ARENA-NEW CONSTANT c2-arena
4096 A-XMEM ARENA-NEW CONSTANT c3-arena

\ ── Core 0: dispatch ──
' task1 1 CORE-RUN                          \ map (hand arena to core)
' task2 2 CORE-RUN
' task3 3 CORE-RUN
BARRIER                                     \ wait for all cores

\ ── Core 0: tear down ──
c1-arena ARENA-DESTROY                      \ reclaim
c2-arena ARENA-DESTROY
c3-arena ARENA-DESTROY
```

```forth
\ ── Secondary core: task body ──
: task1  ( -- )
    c1-arena ARENA-PUSH
    256 AALLOT                              \ bump from own arena — no locks
    ( ... compute ... )
    ARENA-POP ;
```

Each core writes only to its own bump pointer.  No locks, no atomic
CAS, no cache flushes, no false sharing.  The only synchronization
points are arena creation (before dispatch) and arena destruction
(after barrier).

This is equivalent to an MMU that maps a private virtual address space
per process — but the "mapping" happens once at dispatch time in
software, costs zero cycles per memory access, and the programmer sees
exactly which bus the memory is on.

### 5.3 Micro-Core Clusters

Micro-core clusters share a 1 KiB scratchpad (`SPAD`) and a hardware
barrier.  For cluster workloads, core 0 can pre-allocate an HBW arena
and pass it to the cluster lead, which distributes sub-regions to
micro-cores:

```forth
\ Core 0: allocate HBW scratch for cluster 0
4096 A-HBW ARENA-NEW CONSTANT cl0-arena

\ Cluster lead (core 4): split among 4 micro-cores
: cluster-task  ( -- )
    cl0-arena ARENA-PUSH
    1024 AALLOT   ( my-scratch )
    ( ... each micro-core gets its own slice ... )
    ARENA-POP ;
```

---

## 6. The Userland Boundary

KDOS splits XMEM into two zones:

```
XMEM region:
  ┌──────────────────────────────┐  EXT-MEM-BASE
  │  Kernel file buffers (XBUF)  │  ← protected by XMEM-FLOOR
  ├──────────────────────────────┤
  │  Userland dictionary         │  ← HERE when ULAND=1
  │  (1 MiB zone)               │
  ├──────────────────────────────┤  XMEM-FLOOR
  │  XMEM general allocator     │  ← XMEM-ALLOT / arenas
  │  (remaining XMEM)           │
  └──────────────────────────────┘  XMEM-LIMIT
```

`XMEM-FLOOR` protects kernel allocations from `XMEM-RESET`.  The
userland zone is reserved at `ENTER-USERLAND` time and is not
reclaimable by the XMEM allocator.  `LEAVE-USERLAND` switches `HERE`
back to Bank 0 without affecting XMEM allocations.

The `XBUF` word allocates data buffers preferring XMEM (to conserve
Bank 0 dictionary space), then advances the floor:

```forth
16384 XBUF my-data   \ lives in XMEM, protected from XMEM-RESET
```

---

## 7. Quick Reference

### Creating Memory

| Need | Word | Region | Lifetime |
|------|------|--------|----------|
| Permanent definition | `: FOO ... ;` | Bank 0 / userland | Until `MARKER`/`FORGET` |
| Permanent variable | `VARIABLE X` | Bank 0 / userland | Until `MARKER`/`FORGET` |
| Permanent buffer | `CREATE BUF 256 ALLOT` | Bank 0 / userland | Until `MARKER`/`FORGET` |
| Heap object | `256 ALLOCATE` | Bank 0 | Until `FREE` |
| Scoped scratch | `4096 A-HEAP ARENA-NEW` | Any | Until `ARENA-DESTROY` |
| Temp arena (no dict leak) | `desc 4096 A-XMEM ARENA-NEW-AT` | Any | Until `ARENA-DESTROY` |
| Raw bump (XMEM) | `4096 XMEM-ALLOT` | XMEM | Until `XMEM-RESET` |
| Raw bump (HBW) | `4096 HBW-ALLOT` | HBW | Until `HBW-RESET` |
| Tile scratch (XMEM) | `16384 XBUF name` | XMEM | Permanent (floor-protected) |

### Freeing Memory

| Strategy | Word | Cost | Scope |
|----------|------|------|-------|
| Dictionary rewind | `CLEAN` (marker word) | O(1) | Everything after marker |
| Individual heap free | `addr FREE` | O(n) | One block |
| Arena destroy | `arena ARENA-DESTROY` | O(1) | Entire arena |
| Arena reset | `arena ARENA-RESET` | O(1) | All arena allocs (keeps backing) |
| Arena rollback | `arena snap ARENA-ROLLBACK` | O(1) | Allocs after snapshot |
| Bulk XMEM reset | `XMEM-RESET` | O(1) | All XMEM above floor |
| Bulk HBW reset | `HBW-RESET` | O(1) | All HBW |

### Inspecting Memory

| Word | Output |
|------|--------|
| `.MEM` | Full system memory summary (dictionary, heap, HBW, XMEM, buffers, stack) |
| `.HEAP` | Heap base, free bytes, fragment count, largest block, safety flag |
| `.HBW` | HBW base, size, used, free |
| `.XMEM` | XMEM base, size, used, free |
| `.ARENA` | Per-arena: base, size, used, free, source name |
| `.USERLAND` | Userland mode, base, used, free |
| `HEAP-CHECK` | True if heap hasn't collided with data stack |
| `HEAP-FRAG` | Number of free-list fragments |

---

## 8. Design Principles

### 8.1 Transparency Over Abstraction

Every address is physical.  Every allocator tells you which bus your
memory is on.  Every cost is visible.  There is no TLB hiding a
6-cycle XMEM access behind the same syntax as a 1-cycle Bank 0 access.

This is deliberate.  On a power-user Forth machine, the programmer is
trusted to make the right choice — but only if they have the information.
The three-region model isn't a limitation to be hidden; it's the
performance model to be exploited.

### 8.2 Arenas as the Default

The heap exists for objects with independent lifetimes.  But most
allocations don't have independent lifetimes — they're scratch for an
operation, buffers for a parse, working memory for a task.  Arenas
capture this directly:

- O(1) allocation (bump a pointer, no header overhead per object)
- O(1) deallocation (reset the pointer or free one backing block)
- Zero fragmentation within the arena
- Trivial accounting (`ARENA-USED` = one subtraction)

On a machine with no MMU, where fragmentation is a real and
unrecoverable failure mode, defaulting to arenas is not a preference —
it's a survival strategy.

### 8.3 Core 0 as Software MMU

An MMU performs allocation, mapping, and protection implicitly on every
memory access.  Core 0 performs the same three functions explicitly at
task dispatch time:

| MMU function | Core 0 equivalent | When |
|---|---|---|
| Page allocation | `ARENA-NEW` | Setup time |
| Address mapping | `WAKE-CORE` (pass arena base) | Dispatch time |
| Protection | Arena bounds + `?CORE0` | Enforced at call boundary |

The cost of a hardware MMU is per-access (TLB lookup, page walk on
miss).  The cost of the core-0 model is per-task (one setup, one
teardown).  On a 2-stage pipeline at 100 MHz, that's the difference
between a 50% IPC hit on every load/store and zero overhead during
computation.

### 8.4 Predictable Timing

Every allocation strategy on the system has a known worst-case cost:

| Strategy | Worst case |
|----------|-----------|
| Dictionary `ALLOT` | O(1) — advance HERE |
| Arena `ARENA-ALLOT` | O(1) — advance ptr, bounds check |
| HBW/XMEM bump | O(1) — advance pointer, bounds check |
| XMEM bump with free-list | O(n) where n = freed arena blocks |
| Heap `ALLOCATE` | O(n) where n = free-list length |
| Heap `FREE` | O(n) — sorted insert + coalesce |

For real-time workloads (tile engine, NIC frame assembly), only the
O(1) paths are acceptable.  The system is designed so that the fast
paths are the natural ones and the O(n) paths are reserved for setup
and teardown.

### 8.5 No Garbage Collection

Arenas are a deliberate alternative to GC.  The programmer scopes
lifetimes explicitly.  There are no type tags, no tracing, no stop-
the-world pauses, no write barriers, no finalisers.  The programmer
knows what's in the arena because they put it there.

This is a Forth value: the programmer sees everything, controls
everything, and accepts responsibility for everything.  Memory
management is not something that happens to you — it's something
you do.

---

## 9. Common Patterns

### 9.1 File Processing

```forth
: PROCESS-FILE  ( -- )
    8192 A-HEAP ARENA-NEW ABORT" OOM"
    DUP 4096 ARENA-ALLOT           ( arena buf )
    S" data.txt" READ-FILE
    ( ... parse buf ... )
    ARENA-DESTROY ;                \ everything freed
```

### 9.2 Tile Map with Undo

```forth
65536 A-XMEM ARENA-NEW CONSTANT map-arena

: TRY-BRUSH  ( x y tile -- )
    map-arena ARENA-SNAP
    ( ... paint into arena-backed tile buffer ... )
    ESCAPE? IF
        map-arena SWAP ARENA-ROLLBACK     \ undo
    ELSE
        DROP                               \ commit
    THEN ;
```

### 9.3 Per-Core Parallel Workload

```forth
: SETUP-CORES  ( -- )
    4096 A-XMEM ARENA-NEW CONSTANT c1-work
    4096 A-XMEM ARENA-NEW CONSTANT c2-work
    4096 A-XMEM ARENA-NEW CONSTANT c3-work ;

: TEARDOWN-CORES  ( -- )
    c1-work ARENA-DESTROY
    c2-work ARENA-DESTROY
    c3-work ARENA-DESTROY ;

: PARALLEL-COMPUTE  ( -- )
    SETUP-CORES
    ' worker1 1 CORE-RUN
    ' worker2 2 CORE-RUN
    ' worker3 3 CORE-RUN
    BARRIER
    TEARDOWN-CORES ;
```

### 9.4 Region-Agnostic Library Code

```forth
: BUILD-INDEX  ( n -- addr )
    8 * AALLOT                     \ uses whatever arena is current
    ( ... populate ... ) ;

\ Caller chooses the region:
my-xmem-arena ARENA-PUSH
  1024 BUILD-INDEX                 \ index in XMEM
ARENA-POP
```

### 9.5 Temporary Arena in a Loop (No Dictionary Leak)

```forth
CREATE LOOP-DESC 32 ALLOT          \ one 32-byte descriptor, reused

: ITER  ( -- )
    LOOP-DESC 4096 A-HEAP ARENA-NEW-AT ABORT" OOM"
    LOOP-DESC 256 ARENA-ALLOT      ( scratch )
    ( ... work ... )
    LOOP-DESC ARENA-DESTROY ;

\ Safe to call ITER 10,000 times — no dictionary growth
```

### 9.6 Choosing the Right Strategy

```
Need temporary scratch for one operation?
  └─ Arena  (ARENA-NEW / ARENA-DESTROY)

Need multiple objects with independent lifetimes?
  └─ Heap  (ALLOCATE / FREE)

Need permanent data or code?
  └─ Dictionary  (ALLOT / CREATE / :)

Need large storage with moderate latency?
  └─ XMEM arena  (A-XMEM)

Need tile-engine-accessible working buffers?
  └─ HBW arena  (A-HBW)

Running on a secondary core?
  └─ Arena only  (ARENA-ALLOT / AALLOT)
```

---

## 10. Diagnostic Workflow

When debugging memory issues, use this sequence:

```forth
.MEM                   \ overview: dict, heap, HBW, XMEM, buffers, stack
.HEAP                  \ heap detail: fragmentation, largest block, safety
HEAP-CHECK .           \ 1 = safe, 0 = heap/stack collision
HEAP-FRAG .            \ 1 = perfect, higher = fragmented
my-arena .ARENA        \ per-arena utilisation
.USERLAND              \ userland dict usage
```

If `HEAP-CHECK` returns false, the heap has grown into the data stack.
This is unrecoverable — reboot, allocate less, or use arenas in XMEM/
HBW to move pressure off Bank 0.

If `HEAP-FRAG` is growing over time, you have a fragmentation problem.
Switch the affected workload to arenas — arenas cannot fragment.

---

## 11. Glossary

| Term | Meaning |
|------|---------|
| **Bank 0** | 1 MiB on-chip BRAM at address 0.  Holds BIOS, dictionary, heap, stacks. |
| **XMEM** | External RAM (HyperRAM/SDRAM).  Large, higher latency.  Starts at 0x0010_0000. |
| **HBW** | High-Bandwidth math RAM.  3 MiB on-chip BRAM at 0xFFD0_0000.  512-bit tile ports. |
| **Arena** | Pre-allocated region with O(1) bump allocation and O(1) bulk free. |
| **Bump allocator** | Advances a pointer; no individual free.  Cannot fragment. |
| **Free-list** | Linked list of available blocks.  First-fit search.  Can fragment. |
| **Coalescing** | Merging adjacent free blocks to reduce fragmentation. |
| **XMEM floor** | Pointer below which `XMEM-RESET` will not reclaim.  Protects kernel data. |
| **Userland zone** | 1 MiB dictionary area in XMEM for user-loaded code. |
| **Descriptor** | 32-byte arena metadata: base, size, ptr, source. |
| **?CORE0** | Runtime guard that aborts if `COREID` ≠ 0.  Protects shared-state allocators. |
