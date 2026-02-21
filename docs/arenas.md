# Arena Allocator — Design Document

**Status:** Planned  
**Depends on:** Item 47 (memory management hardening) ✅  
**Section:** KDOS §1.1b (after §1.1a Dictionary Snapshots)

---

## 1. Motivation

KDOS has three allocation strategies, each with a clear scope:

| Strategy | Scope | Reclaim |
|----------|-------|---------|
| Dictionary (`ALLOT`, `:`) | Permanent definitions | `MARKER` / `FORGET` |
| Heap (`ALLOCATE` / `FREE`) | Individual objects | Per-object `FREE` |
| Bump (`HBW-ALLOT`, `XMEM-ALLOT`) | Region-wide | `HBW-RESET`, `XMEM-RESET` |

The gap is **scoped scratch memory** — temporary allocations whose
lifetime is tied to an operation, not to an individual object.  Examples:

- Parsing a file: allocate parse buffers, process, discard everything.
- Tile map editing: allocate undo snapshots, drop old ones in bulk.
- Network packet assembly: assemble a frame in scratch, send, discard.
- Task-local computation: each task gets scratch, auto-freed on exit.
- REPL experimentation: try something, undo all side-effects.

Today these patterns require careful manual `FREE` of every allocation,
or they leak.  An arena makes the lifetime explicit and the cleanup
atomic.

---

## 2. Core Concept

An arena is a pre-allocated region where allocations are O(1) (bump a
pointer forward) and deallocation is O(1) (reset the pointer or free
the entire region).  No per-object headers.  No free list.  No
fragmentation within the arena.

```
┌─────────────────────────────────────────────┐
│  Arena backing region (one ALLOCATE block)   │
│                                             │
│  ┌──────┬──────┬──────┬────────────────┐    │
│  │ obj1 │ obj2 │ obj3 │    free        │    │
│  └──────┴──────┴──────┴────────────────┘    │
│  ^base                ^ptr             ^end │
└─────────────────────────────────────────────┘
```

- `ARENA-ALLOT` advances `ptr`.  No header overhead per object.
- `ARENA-RESET` sets `ptr = base`.  All objects gone, O(1).
- `ARENA-DESTROY` calls `FREE` on the backing block, O(1).

---

## 3. Data Structure

Arena descriptor: 4 cells (32 bytes), allocated in the dictionary.

```
+0   base     Start of the arena's data region
+8   size     Total capacity in bytes
+16  ptr      Current bump pointer (next allocation address)
+24  source   Backing store: 0 = Bank 0 heap, 1 = XMEM, 2 = HBW
```

The descriptor itself lives in the dictionary (via `HERE`/`,`), so it
persists until `MARKER`/`FORGET` reclaims it.  The backing data region
lives in the selected memory store.

---

## 4. Word Definitions

### 4.1 Core API (MVP)

```forth
ARENA-NEW     ( size source -- arena ior )
```
Allocate a backing region of `size` bytes from the specified memory
source (0=heap, 1=XMEM, 2=HBW).  Build a descriptor in the dictionary.
Returns descriptor address and 0 on success, or 0 and -1 on failure.

```forth
ARENA-ALLOT   ( arena u -- addr )
```
Bump-allocate `u` bytes (8-byte aligned) from the arena.  Aborts if
the arena is full.  Returns the start address of the allocated region.

```forth
ARENA-ALLOT?  ( arena u -- addr ior )
```
Like `ARENA-ALLOT` but returns ior instead of aborting on overflow.
Allows callers to handle full-arena gracefully.

```forth
ARENA-RESET   ( arena -- )
```
Reset the bump pointer to the arena's base address.  All prior
allocations within the arena are logically freed.  O(1).  The backing
region is retained for reuse.

```forth
ARENA-DESTROY ( arena -- )
```
Free the arena's backing region (via `FREE`, `HBW-RESET`, or
`XMEM-RESET` depending on source) and zero the descriptor.  After
this call, the descriptor is inert (all fields zero).

```forth
ARENA-FREE    ( arena -- u )
```
Bytes remaining before the arena is full.

```forth
ARENA-USED    ( arena -- u )
```
Bytes consumed so far.

```forth
.ARENA        ( arena -- )
```
Print arena status: base, size, used, free, source name.

### 4.2 Convenience constants

```forth
0 CONSTANT HEAP   \ source = Bank 0 heap
1 CONSTANT XMEM   \ source = external RAM (already used, but overloaded here)
2 CONSTANT HBW    \ source = HBW math RAM
```

These already exist implicitly; the constants make `ARENA-NEW` calls
self-documenting:

```forth
4096 HEAP ARENA-NEW CONSTANT my-scratch
65536 XMEM ARENA-NEW CONSTANT file-arena
1024 HBW ARENA-NEW CONSTANT tile-scratch
```

### 4.3 Snapshots (Phase 2)

```forth
ARENA-SNAP      ( arena -- snap )
```
Save the current bump pointer as a snapshot token.  The token is just
the `ptr` value — no allocation, no overhead.

```forth
ARENA-ROLLBACK  ( arena snap -- )
```
Restore the bump pointer to a previous snapshot.  Everything allocated
after the snapshot is logically freed.  O(1).

```forth
ARENA-SNAP-DROP ( snap -- )
```
Discard a snapshot token (no-op — included for API symmetry and to
make intent explicit in user code).

**Use case — transactional scratch:**
```forth
my-arena ARENA-SNAP     ( snap )
  ( ... tentative work: allot parse nodes, build tree ... )
  parse-ok? IF
    DROP                 \ commit: drop the snap, keep allocations
  ELSE
    my-arena SWAP ARENA-ROLLBACK   \ abort: rewind
  THEN
```

**Use case — tile map undo:**
```forth
map-arena ARENA-SNAP    ( snap )
  ( ... apply brush stroke into arena-backed tile buffer ... )
  ESCAPE? IF
    map-arena SWAP ARENA-ROLLBACK   \ undo stroke
  ELSE
    DROP                             \ keep stroke
  THEN
```

### 4.4 Scoped Arena Stack (Phase 3)

```forth
ARENA-PUSH   ( arena -- )
```
Push `arena` onto a 4-deep "current arena" stack.

```forth
ARENA-POP    ( -- )
```
Pop the current arena stack.

```forth
AALLOT       ( u -- addr )
```
Allocate from the current (top-of-stack) arena.  Aborts if no arena
has been pushed.

This supports writing allocation-polymorphic words — code that doesn't
know or care which arena (or even which memory region) it's allocating
from:

```forth
: BUILD-INDEX  ( n -- addr )
    8 * AALLOT          \ allocate from whatever arena is current
    ( ... fill index ... ) ;

my-heap-arena ARENA-PUSH
  1024 BUILD-INDEX      \ index built in heap arena
ARENA-POP

my-xmem-arena ARENA-PUSH
  1024 BUILD-INDEX      \ same code, index built in XMEM
ARENA-POP
```

### 4.5 Arena-Scoped Buffers (Phase 4)

```forth
ARENA-BUFFER  ( type width length arena "name" -- )
```
Like `BUFFER`, but both the descriptor and data region are allocated
from the given arena.  The buffer is registered in the normal linked
list (via `(BUF-REG)`) but tagged as arena-scoped.

On `ARENA-DESTROY`, all arena-scoped buffers are automatically
unregistered from the buffer list.  `BUFFERS` shows `[arena]` next to
them.

```forth
\ Create a tile buffer in XMEM scratch
65536 XMEM ARENA-NEW CONSTANT map-arena
2 8 4096 map-arena ARENA-BUFFER tile-data

\ Use tile-data normally — B.SUM, B.FILL, tile ops all work
tile-data B.SUM .

\ Discard everything — tile-data + descriptor + all scratch gone
map-arena ARENA-DESTROY
```

This transforms buffers from "permanent until reboot" into
"scoped to an operation."

---

## 5. Memory Sources — Region-Aware Design

The unique property of Megapad-64 is three memory regions with
different performance characteristics.  Arenas unify access to all
three behind a single API:

| Source | Backing | Best for | Perf |
|--------|---------|----------|------|
| `HEAP` (0) | `ALLOCATE` / `FREE` | General scratch, parse trees | CPU-fast |
| `XMEM` (1) | `XMEM-ALLOT` region | Large files, datasets, maps | Large, moderate latency |
| `HBW` (2) | `HBW-ALLOT` region | Tile engine buffers, SIMD ops | Tile-width bandwidth |

`ARENA-NEW` dispatches to the appropriate allocator based on `source`.
`ARENA-DESTROY` dispatches to the appropriate deallocator.  User code
above the arena is region-agnostic.

### XMEM and HBW backing: implementation note

The heap (`ALLOCATE`/`FREE`) supports individual block freeing.
HBW and XMEM are bump allocators with only bulk-reset semantics
(`HBW-RESET`, `XMEM-RESET`).  This creates a subtlety:

- **HEAP-backed arenas:** `ARENA-DESTROY` calls `FREE` on the backing
  block.  Works perfectly — the heap reclaims exactly that region.

- **XMEM-backed arenas:** `ARENA-DESTROY` cannot free just the arena's
  region because `XMEM-ALLOT` is a bump allocator.  However, arenas
  are typically the *only* dynamic XMEM consumers (kernel modules use
  `XMEM-FLOOR` to protect their allocations).  Strategy: track active
  XMEM arena count; when the last one is destroyed, call `XMEM-RESET`.
  Otherwise, the backing memory is abandoned until bulk reset.

- **HBW-backed arenas:** Same situation as XMEM.  In practice, HBW
  arenas are short-lived (tile operation scratch) and HBW is large
  (3 MiB), so abandoned slivers are tolerable.

**Phase 1 implementation:** heap-backed only.  XMEM/HBW variants in
Phase 2, with explicit documentation of the reclaim limitations.

---

## 6. Per-Core Arenas

Each core (or micro-core cluster) can own a private arena.  This
eliminates heap contention in multi-core workloads:

```forth
\ In task setup (runs on assigned core):
4096 HEAP ARENA-NEW CONSTANT my-arena

\ In task body:
my-arena 256 ARENA-ALLOT   ( scratch-addr )
( ... compute ... )

\ On task exit:
my-arena ARENA-DESTROY
```

Today, if two cores call `ALLOCATE` concurrently, they race on the
shared free list (no locking — silent corruption risk).  Per-core
arenas sidestep this entirely: each core only touches its own arena
pointer.  Inter-core results pass through the mailbox; scratch stays
local.

A future scheduler enhancement could auto-create a per-task arena
and destroy it on task exit, making task-local scratch fully automatic.

---

## 7. Interaction with Existing Subsystems

| Subsystem | Interaction |
|-----------|-------------|
| **Heap** (`ALLOCATE`/`FREE`) | Heap-backed arenas are normal heap blocks.  `HEAP-CHECK`, `.HEAP` see them.  `FREE` reclaims them cleanly thanks to coalescing (item 47a). |
| **MARKER/FORGET** | Orthogonal.  Arenas live in the heap; MARKER saves/restores dictionary (HERE/LATEST).  If an arena descriptor is in the dictionary, `FORGET` past it leaves the backing block allocated (leak).  Recommendation: `ARENA-DESTROY` before `MARKER`/`FORGET`. |
| **Buffers** | Phase 4 `ARENA-BUFFER` integrates with the buffer linked list.  Non-arena buffers are unaffected. |
| **Tasks/scheduler** | Per-task arenas (§6) compose with the existing task lifecycle.  No scheduler changes needed for Phase 1. |
| **HBW/XMEM** | Phase 2.  Arena becomes a structured front-end to the existing bump allocators. |
| **RESIZE** | Not applicable — arenas don't support per-object resize.  Use the heap for objects that may grow. |
| **.MEM** | `.ARENA` provides per-arena detail.  `.MEM` could list active arenas in a future enhancement. |

---

## 8. Implementation Phases

### Phase 1: Heap-backed arenas (MVP)
- `ARENA-NEW` (heap source only), `ARENA-ALLOT`, `ARENA-ALLOT?`,
  `ARENA-RESET`, `ARENA-DESTROY`, `ARENA-FREE`, `ARENA-USED`, `.ARENA`
- ~30 lines of Forth
- ~8 tests: create/allot/reset/destroy, overflow abort, overflow ior,
  free/used accounting, reset-and-reuse, destroy-then-allot-fails

### Phase 2: Multi-source + snapshots
- Add XMEM and HBW backing to `ARENA-NEW` / `ARENA-DESTROY`
- `ARENA-SNAP`, `ARENA-ROLLBACK`, `ARENA-SNAP-DROP`
- ~20 additional lines
- ~6 tests: XMEM arena, HBW arena, snap/rollback, nested snaps

### Phase 3: Scoped arena stack
- `ARENA-PUSH`, `ARENA-POP`, `AALLOT`, `CURRENT-ARENA` variable,
  4-deep arena stack
- ~15 additional lines
- ~4 tests: push/pop/allot, nesting, underflow error

### Phase 4: Arena-scoped buffers
- `ARENA-BUFFER` word
- `ARENA-DESTROY` auto-unregisters arena-scoped buffers
- `BUFFERS` shows `[arena]` tag
- ~20 additional lines
- ~4 tests: arena buffer create, use, auto-unregister, BUFFERS output

**Total:** ~85 lines of Forth, ~22 tests across 4 phases.

---

## 9. Examples

### File processing

```forth
\ Read a config file into scratch, parse it, discard
: LOAD-CONFIG  ( -- )
    8192 HEAP ARENA-NEW ABORT" arena fail"
    DUP                              ( arena arena )
    DUP 4096 ARENA-ALLOT             ( arena arena buf )
    S" config.f" READ-FILE           ( arena arena )
    ( ... parse lines from buf ... )
    ARENA-DESTROY ;                  ( all scratch gone )
```

### Tile map undo

```forth
65536 XMEM ARENA-NEW CONSTANT undo-arena

: TRY-BRUSH  ( x y tile -- )
    undo-arena ARENA-SNAP            ( x y tile snap )
    ( ... paint tile into arena-backed map buffer ... )
    ESCAPE? IF
        undo-arena SWAP ARENA-ROLLBACK   \ revert
    ELSE
        DROP                              \ commit
    THEN ;
```

### Task-local scratch

```forth
: MY-TASK  ( -- )
    2048 HEAP ARENA-NEW DROP         ( arena )
    DUP ARENA-PUSH
    ( ... use AALLOT freely — all scratch is arena-local ... )
    ARENA-POP
    ARENA-DESTROY ;                  ( task cleanup, zero leaks )
```

### Region-agnostic library code

```forth
\ This word doesn't know or care where memory comes from
: BUILD-TABLE  ( n -- addr )
    8 * AALLOT
    ( ... populate ... ) ;

\ Caller decides the region
my-xmem-arena ARENA-PUSH
  1024 BUILD-TABLE                   ( table in XMEM )
ARENA-POP
```

---

## 10. Non-Goals

- **Garbage collection.**  Arenas are a deliberate alternative.  The
  programmer scopes lifetimes explicitly.  No tracing, no pauses, no
  type tags.

- **Per-object free within an arena.**  This defeats the purpose.  If
  individual objects have independent lifetimes, use the heap.

- **Finalizers / destructors.**  Adding cleanup callbacks to
  `ARENA-DESTROY` would add complexity with minimal payoff.  The
  programmer knows what's in the arena.

- **Thread safety / locking.**  Arenas are intended to be task-local
  or caller-owned.  No concurrent access, no locks needed.

- **Auto-growing arenas.**  Allocating a linked chain of chunks when
  the arena fills would complicate the implementation and break the
  O(1) guarantee.  Right-size the arena upfront;  `.ARENA` shows
  utilization for tuning.
