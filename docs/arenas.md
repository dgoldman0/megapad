# Arena Allocator — Design Document

**Status:** Implemented in KDOS §1.1b with Buffer integration in §2.1

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
or they leak. An arena makes the lifetime explicit and gives O(1) logical
cleanup by pointer reset. Physical reclamation on destruction depends on the
backing allocator. Creation, descriptor publication, and destruction are
ordered Forth operations, not a transactional commit.

---

## 2. Core Concept

An arena is a pre-allocated region where allocations are O(1) (bump a
pointer forward) and logical deallocation is O(1) (reset the pointer).
Destroying an arena performs one backing-release dispatch, whose cost and
reclamation behavior depend on the selected allocator. No per-object
headers. No free list within the arena. No fragmentation within the arena.

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
- `ARENA-DESTROY` performs one backing-release dispatch; its cost and whether
  space is immediately reusable depend on the selected allocator.

---

## 3. Data Structure

Arena descriptor: 4 cells (32 bytes).

```
+0   base     Start of the arena's data region
+8   size     Total capacity in bytes
+16  ptr      Current bump pointer (next allocation address)
+24  source   Backing store: 0 = general ALLOCATE, 1 = XMEM, 2 = HBW
```

The descriptor can live in one of two places:

- **Dictionary** (`ARENA-NEW`): the descriptor is compiled at `HERE`
  via `,`.  It persists until `MARKER`/`FORGET` reclaims it.  Simple
  and appropriate for long-lived arenas.

- **User-provided address** (`ARENA-NEW-AT`): the caller supplies a
  32-byte buffer (e.g. `CREATE … 32 ALLOT`, a `VARIABLE` cluster, or
  a region in another arena).  No dictionary space is consumed.  Use
  this for temporary arenas created/destroyed in a loop to avoid a
  slow dictionary leak.

---

## 4. Word Definitions

### 4.1 Core API (MVP)

```forth
ARENA-NEW     ( size source -- arena ior )
```
Allocate a backing region of `size` bytes from the specified memory
source (0=heap, 1=XMEM, 2=HBW).  Build a descriptor in the dictionary.
Returns descriptor address and 0 on success, or 0 and -1 on failure.

The examples below use local checked helpers so the policy can be reused; they
are illustrative and are not built-in KDOS words. State-smart `ABORT"` can
also consume an `ior` directly in interpretation state when a one-off
top-level arena constant is appropriate.

```forth
: MUST-ARENA  ( size source -- arena )
    ARENA-NEW ABORT" arena fail" ;
: MUST-ARENA-AT  ( desc size source -- )
    ARENA-NEW-AT ABORT" arena fail" ;
```

**Note:** the 32-byte descriptor is permanently committed to the
dictionary.  For temporary arenas created/destroyed in a loop, use
`ARENA-NEW-AT` instead.

```forth
ARENA-NEW-AT  ( desc size source -- ior )
```
Like `ARENA-NEW` but writes the descriptor at `desc` (a user-provided,
writable, cell-aligned span of at least 32 bytes) instead of consuming
dictionary space. The current source relies on that caller contract rather
than checking it. Returns 0 on success, -1 on failure. Example:

```forth
CREATE MY-DESC 32 ALLOT
MY-DESC 4096 A-HEAP MUST-ARENA-AT
\ ... use MY-DESC as the arena (ARENA-ALLOT, ARENA-RESET, etc.) ...
MY-DESC ARENA-DESTROY
```

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
Free the arena's backing region (via `FREE` for heap, the XMEM
free-list for XMEM, or abandonment for HBW) and zero the descriptor.
After this call, the descriptor is inert (all fields zero).

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
0 CONSTANT A-HEAP    \ source = general ALLOCATE/FREE route
1 CONSTANT A-XMEM    \ source = external RAM
2 CONSTANT A-HBW     \ source = HBW math RAM
```

The `A-` prefix avoids collision with `XMEM-ALLOT`, `HBW-ALLOT`, etc.
The constants make `ARENA-NEW` calls self-documenting:

```forth
4096 A-HEAP MUST-ARENA CONSTANT my-scratch
65536 A-XMEM MUST-ARENA CONSTANT file-arena
1024 A-HBW MUST-ARENA CONSTANT tile-scratch
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

The implementation does not prove that the token came from
`ARENA-SNAP` or that it moves backward. It accepts any address in the
inclusive `[base,base+size]` interval, including an unaligned or future
address. Callers must preserve genuine tokens.

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

The four stack cells and `ARENA-SP` are global dictionary state. They are not
task-local or per-core, are not unwound automatically on an abort, and are not
synchronized. Use this convenience only under one coordinated owner; worker
code with private arenas should call `ARENA-ALLOT` directly.

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
from the given arena. The buffer is registered in the normal linked list via
`(BUF-REG)`. No arena tag is stored in the descriptor or link; destruction
recognizes an arena buffer solely because its descriptor address falls in the
arena's `[base, base+size)` interval.

On `ARENA-DESTROY`, all arena-scoped buffers are automatically
unregistered from the buffer list. `BUFFERS` uses the ordinary `B.INFO`
format and does not show an `[arena]` tag. Unregistration does not reclaim the
16-byte dictionary link nodes or undefine the constants created for those
buffers, so each old name becomes a dangling descriptor address after the
arena backing is released or abandoned.

`ARENA-RESET` is intentionally only a pointer rewind and does not run the
unregistration walk. Existing registered descriptors and constants therefore
remain visible while their Arena storage becomes eligible for overwrite by
later allocations. Likewise, dictionary `MARKER`, `FORGET`, or rollback does
not coordinate with `BUF-HEAD`/`BUF-COUNT`; reclaiming a published link or
constant can strand stale registry state.

`ARENA-BUFFER` uses the Arena allocator's eight-byte rounding for its data
request. It does not insert a 64-byte alignment step after the 32-byte
descriptor, despite the Buffer comments' tile-alignment intent. Callers must
not assume an Arena buffer is a valid tile-engine operand. Construction is
ordered, not transactional: descriptor/data allocation and stores precede
link/count publication, which precedes the final constant definition. A later
failure can therefore leave consumed Arena capacity, a partial descriptor, or
a registered buffer without the requested name. `AB-AR`, `AB-DESC`, and the
registry are shared global state rather than task-local publication state.

```forth
\ Create an arena buffer in XMEM scratch
65536 A-XMEM MUST-ARENA CONSTANT map-arena
2 8 4096 map-arena ARENA-BUFFER tile-data

\ Byte-oriented operations do not require tile alignment
7 tile-data B.FILL
tile-data B.INFO

\ Do not use tile ops unless this particular data address was proven aligned

\ Unregister and destroy backing; tile-data is now a stale constant
map-arena ARENA-DESTROY
```

This scopes registry visibility and backing lifetime to the operation. It does
not give the dictionary name/link allocations the same lifetime.

---

## 5. Memory Sources — Region-Aware Design

The unique property of Megapad-64 is three memory regions with
different performance characteristics.  Arenas unify access to all
three behind a single API:

| Source | Backing | Best for | Perf |
|--------|---------|----------|------|
| `A-HEAP` (0) | General `ALLOCATE` / `FREE` (XMEM when present, Bank 0 otherwise) | General reclaimable scratch | Platform-dependent |
| `A-XMEM` (1) | `XMEM-ALLOT` region | Large files, datasets, maps | Large, moderate latency |
| `A-HBW` (2) | `HBW-ALLOT` region | High-bandwidth scratch; tile operands require explicit alignment | Tile-width bandwidth |

`ARENA-NEW` dispatches to the appropriate allocator based on `source`.
`ARENA-DESTROY` dispatches to the appropriate deallocator.  User code
above the arena is region-agnostic.

### XMEM and HBW backing: implementation note

The heap (`ALLOCATE`/`FREE`) supports individual block freeing.
HBW is a bump allocator with only bulk-reset semantics (`HBW-RESET`).
XMEM now supports individual block reclaim via a free-list.

- **General-allocation arenas:** `ARENA-DESTROY` calls `FREE` on the backing
  block. With XMEM present this is a prefixed XMEM allocation; without XMEM it
  is a Bank 0 heap block. Either valid route reclaims the backing span.

- **XMEM-backed arenas:** `ARENA-DESTROY` returns the backing block
  to the XMEM free-list via `XMEM-FREE-BLOCK`.  Subsequent
  `XMEM-ALLOT` calls check the free-list (first-fit) before falling
  back to bump allocation. Requests and frees share 16-byte size
  normalization, so padding remains part of the live allocation and is
  recovered even when the requested arena size is not node-aligned. This
  means XMEM-backed arenas can be repeatedly created and destroyed without
  leaking backing memory. A dictionary descriptor created by `ARENA-NEW`
  remains; use caller-placed `ARENA-NEW-AT` when that growth is unwanted.
  `XMEM-RESET` clears the free-list along with the bump pointer.

- **HBW-backed arenas:** HBW remains a pure bump allocator. In practice, HBW
  arenas are short-lived high-bandwidth scratch and HBW is large (3 MiB), so
  abandoned slivers are tolerable. Arena allocation is only eight-byte
  aligned; a tile operand still needs an explicitly proven 64-byte address. A
  free-list could be added later if needed.

### Source contract edges

The intended size domain is a positive representable cell no greater than the
arena capacity. The current `ARENA-ALLOT` words use wrapping
`7 + -8 AND` followed by a signed `<` comparison. The seven highest cell
patterns round to zero and succeed; other sign-bit-set aligned requests can
pass and wrap `ptr` below `base`. HBW construction also inherits the raw HBW
allocator's high-cell wrap. These are open source defects, not supported
large-allocation behavior.

Snapshot validation has the same signed/wrapping limitation. Its nominal
`[base, base+size]` interval is meaningful only for an ordinary low-half,
nonwrapping descriptor; corrupt or high-cell descriptors can admit or reject
tokens according to signed comparisons instead. `ARENA-USED` and
`ARENA-FREE` likewise perform wrapping arithmetic rather than validating a
descriptor.

Construction is ordered but not transactional. Backing allocation completes
before the four descriptor cells are written. A dictionary fault in
`ARENA-NEW`, or a bad caller span in `ARENA-NEW-AT`, can leak the backing and
leave no complete descriptor from which to destroy it. `ARENA-NEW-AT` also
does not reject overwriting a live descriptor, which loses the old backing.
Callers must provide valid empty storage and sufficient dictionary capacity.

---

## 6. Per-Core Arenas

Each core (or micro-core cluster) can own a private arena.  This
eliminates heap contention in multi-core workloads:

```forth
\ In task setup (runs on assigned core):
4096 A-HEAP MUST-ARENA CONSTANT my-arena

\ In task body:
my-arena 256 ARENA-ALLOT   ( scratch-addr )
( ... compute ... )

\ On task exit:
my-arena ARENA-DESTROY
```

Today, if two cores call `ALLOCATE` concurrently, they race on shared
allocator state. Per-core arenas can sidestep that during the bump phase only
when each core receives a distinct descriptor and calls `ARENA-ALLOT`
directly. The scoped `ARENA-PUSH`/`AALLOT` selection stack is global and
cannot select different current arenas concurrently. Creation and destruction
remain coordinated core-0 lifecycle operations.

A future scheduler enhancement could auto-create a per-task arena
and destroy it on task exit, making task-local scratch fully automatic.

---

## 7. Interaction with Existing Subsystems

| Subsystem | Interaction |
|-----------|-------------|
| **General allocation** (`ALLOCATE`/`FREE`) | `A-HEAP` follows the current public route: prefixed XMEM when present, otherwise a Bank 0 heap block. `FREE` reclaims a valid block through the matching route. |
| **MARKER/FORGET** | Orthogonal. `MARKER` saves/restores dictionary HERE/LATEST. If an arena descriptor is in the dictionary, `FORGET` past it reclaims only that descriptor and leaks whichever general, raw-XMEM, or HBW backing it owns. Recommendation: `ARENA-DESTROY` before `MARKER`/`FORGET`. |
| **Buffers** | Phase 4 `ARENA-BUFFER` integrates with the linked registry. Destroy walks descriptor addresses to unlink Arena members; it leaves their constants and dictionary link nodes behind. Non-arena buffers are unaffected. |
| **Tasks/scheduler** | Direct allocation through an exclusively owned descriptor composes with task dispatch. The current-arena stack is global and needs serialization or redesign before it can be called task-local. |
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
- `BUFFERS` retains the ordinary untagged `B.INFO` format
- ~20 additional lines
- ~4 tests: arena buffer create, eight-byte data alignment, auto-unregister,
  dangling-name/link behavior

**Total:** ~85 lines of Forth, ~22 tests across 4 phases.

---

## 9. Examples

### File processing

```forth
\ Read a config file into scratch, parse it, discard
: LOAD-CONFIG  ( -- )
    8192 A-HEAP ARENA-NEW ABORT" arena fail"
    DUP                              ( arena arena )
    DUP 4096 ARENA-ALLOT             ( arena arena buf )
    S" config.f" READ-FILE           ( arena arena )
    ( ... parse lines from buf ... )
    ARENA-DESTROY ;                  ( backing gone; descriptor remains )
```

### Tile map undo

```forth
65536 A-XMEM MUST-ARENA CONSTANT undo-arena

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
    2048 A-HEAP ARENA-NEW ABORT" arena fail"  ( arena )
    DUP ARENA-PUSH
    ( ... use AALLOT freely — all scratch is arena-local ... )
    ARENA-POP
    ARENA-DESTROY ;                  ( backing cleanup; descriptor remains )
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

- **Thread safety / locking.** The source supplies no locks. Direct bump
  allocation is suitable only when a descriptor has one owner. The scoped
  current-arena stack is global, so it is not a task-local selection mechanism.

- **Auto-growing arenas.**  Allocating a linked chain of chunks when
  the arena fills would complicate the implementation and break the
  O(1) guarantee.  Right-size the arena upfront;  `.ARENA` shows
  utilization for tuning.
