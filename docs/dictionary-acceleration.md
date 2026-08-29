# Dictionary Acceleration Contract

Status: locked and implemented in the host emulator and BIOS/KDOS sources on
2026-08-29. RTL implementation is intentionally deferred to the hardware
team; its exact requirements are recorded below.

This contract keeps the Forth linked dictionary authoritative. It separates
two jobs which the original 256-entry `EXT.DICT` table could not perform at
once:

- a small per-core hardware cache keeps recently used positive bindings fast;
- a caller-backed BIOS hash index makes positive cache misses and negative
  lookups independent of dictionary depth.

Neither structure changes Forth name comparison, latest-binding shadowing,
`IMMEDIATE`, `MARKER`, or `FORGET` semantics. If either accelerator is absent
or cannot prove an answer, lookup follows the linked dictionary.

## Measured sizing decision

The `desktop-apt1` cold-source profile at Akashic
`9d72d0192c306331873bbba9b735aa96757accae` reached a final live dictionary of
30,598 entries. Its source-load interval made 392,386 cacheable lookups:
358,875 positive and 33,511 negative. The original 64-set by four-way cache
was already full; all 148,421 insert attempts overflowed, and cacheable misses
walked 971.663 million linked nodes.

The implemented target is **1,024 hardware entries: 256 sets by four ways**.
With replacement, that profile projected a 97.82% positive-hit rate and
653.725 million linked visits under eager definition seeding. Demand fill with
update-existing-only definition publication was slightly better at this size
and suppressed about 24,300 one-use definition allocations. Four ways was
already near the associativity plateau: two, four, and eight ways at 1,024
entries produced 661.59, 653.73, and 651.20 million visits respectively.

A cache alone would justify about 4,096 entries, but it would still leave the
dominant negative-lookup traversal in place and cost about 192 KiB per core.
The 1,024-entry cache costs about 48.1 KiB per core with the present 384-bit
physical entry representation and is the measured byte-efficiency elbow once
the BIOS index makes misses cheap. It is not intended to contain the complete
dictionary.

On the canonical 128 MiB configuration KDOS reserves a **65,536-slot** BIOS
index. At 16 bytes per slot this is 1 MiB and holds the measured 30,598 live
entries at a load factor below 0.47. The allocation policy takes at most
1/128 of currently free XMEM and rounds the resulting slot count down to a
power of two; that selects 65,536 slots for this arrangement and scales down
without turning the profile-derived size into an architectural maximum. The
BIOS interface remains caller-bounded and accepts any valid power-of-two slot
count.

## Per-core `EXT.DICT` cache

Names are counted strings of at most 31 bytes. Hashing is FNV-1a over the
already uppercased bytes, with 32-bit truncation after every multiply. The low
eight hash bits select one of 256 sets.

Each set has four ways and a two-bit next-replacement cursor. Operations obey
these deterministic rules:

1. `DFIND` compares all ways and returns the matching entry address. It never
   changes replacement state.
2. `DINS` first updates a matching way's entry address without moving the
   cursor. Otherwise it fills the lowest-numbered invalid way and sets the
   cursor to the following way. If all ways are valid, it replaces the way at
   the cursor and advances the cursor modulo four.
3. Every `DINS`, including a full-set replacement, succeeds with `Z=1,V=0`.
   Capacity overflow is no longer an outcome.
4. `DUPD` updates a matching way's entry address without allocating or moving
   the cursor. A miss returns `Z=0,V=0` and leaves the set unchanged.
5. `DDEL` invalidates a matching way and does not move the cursor.
6. `DCLR` and CPU reset invalidate every way and reset every cursor to zero.

The architectural encodings are three bytes because the current decoder
always consumes the register byte: `DFIND FA 00 DR`, `DINS FA 01 DR`,
`DDEL FA 02 0R`, `DCLR FA 03 00`, and `DUPD FA 04 DR`. The unused `DCLR`
register byte must be zero in canonical code.

Cache population is demand driven. A positive lookup which was not already in
the cache performs local `DINS` after the authoritative lookup returns.
Publishing a new definition executes `DUPD`, which updates an already resident
binding but does not allocate a line merely because the definition exists.
This prevents a cold source load from replacing useful lookup state with
thousands of definitions which may never be queried.

## Caller-backed BIOS index

The side index uses open addressing with linear probing. `DICT-INDEX!`
`( base slots -- status )` binds or disables the index: status 0 means an
authoritative installation or successful disable, 1 rejects invalid arguments
without changing the prior binding, and 2 reports an installed but saturated,
non-authoritative rebuild. `DICT-INDEX@`
`( -- base slots count flags )` exposes bounded diagnostics, with flag bits 0
through 3 meaning `BOUND`, `AUTHORITATIVE`, `BUILDING`, and `SATURATED`.
Storage is provided by the caller as a 16-byte-aligned base and a power-of-two
slot count.
The complete non-wrapping span must lie in advertised external memory. The
BIOS validates the span before publishing it and never owns, grows, or frees
the allocation. With no valid allocation, lookup remains correct through the
linked list.

Each 16-byte slot contains the entry pointer at `+0`, the 32-bit uppercase
FNV-1a hash at `+8`, the seven-bit name length at `+12`, and three zero reserved
bytes at `+13..+15`. An entry pointer of zero marks an empty slot. Metadata is
written before the entry pointer so the pointer is the publication field. Hash
equality is only a probe filter: an index hit must also compare the stored
length and the case-folded name bytes at the referenced dictionary header.

The index covers dictionary names from one through 127 bytes. Creation of a
name longer than 127 bytes is rejected before writing a header; otherwise the
length byte would alias bit 7, which belongs to `IMMEDIATE`. Names longer than
the hardware cache limit remain fully indexed.

The BIOS maintains an `enabled` state and a separate `authoritative` state.
Every probe is bounded by the caller's slot count:

- an exact hit is a safe positive result;
- an empty slot is a definitive negative result only while the index is
  authoritative;
- a full probe or any non-authoritative state falls back to the linked list;
- failure to insert a distinct binding clears `authoritative` before lookup
  can treat any empty slot as proof of absence.

Ordinary definition publication upserts the latest binding for its name.
Initial installation and rollback rebuild iteratively from `LATEST` toward the
oldest entry using insert-if-absent, so the first (newest) binding wins. A
normal upsert during that newest-to-oldest walk would incorrectly let an older
shadowed definition replace the new one.

KDOS allocates a capacity-derived index after the one-shot `XMEM-INIT` and
before sealing the userland dictionary/general-XMEM partition; that table is
1 MiB in the canonical 128 MiB arrangement. Its own initializer is also
one-shot. It uses the checked XMEM allocator, installs the bounded span,
advances `XMEM-FLOOR` after a successful installation, and rebuilds all BIOS
and KDOS definitions accumulated before installation. If external memory is
absent or the reservation cannot be made, installation is skipped and linked
lookup remains the fallback.

## Publication and rollback

Dictionary publication is single-writer work under the dictionary lock. The
new header and link must be complete before publishing `LATEST`; the side-index
upsert follows that publication. A private 64-bit seqlock epoch is even while
the dictionary, index, and cache bindings are stable and odd across every
publication, index installation/disable, and rollback. Readers snapshot an
even generation and revalidate it after `DFIND` or an index probe. An exact
index hit is safe in any stable bound state, including saturation; an empty
slot proves absence only in a stable authoritative generation. If publication
crosses a late local `DINS`, the reader deletes that possibly stale fill and
resolves through the linked head without filling again.

Rollback is one BIOS-owned operation, `DICT-ROLLBACK`
`( saved-here saved-latest -- )`, taking the saved `HERE` and `LATEST` as a
pair. It validates both targets and the saved head's ancestry before mutation,
marks the index non-authoritative, globally clears `EXT.DICT`, publishes both
dictionary pointers, clears the side index, and rebuilds newest-to-oldest. It
marks the index authoritative only if the complete rebuild succeeds. `MARKER`,
`FORGET`, and transactional compiler rollback must use this operation; a raw
store to `var_latest` is not a supported rollback path. This prevents a
forgotten entry from surviving as a hardware-cache or side-index hit.

The two-cell checkpoint rewinds one contiguous active dictionary zone. Every
removed header must lie in `[saved-HERE,current-HERE)`, and no retained header
may lie there. A rollback whose intervening definitions cross between Bank 0
and userland is rejected before mutation; reclaiming two independently moving
allocation cursors would require a wider transaction record.

## Required RTL follow-up

The RTL team must implement the same 256-set by four-way geometry and exact
replacement state machine. The cursor is two bits per set. Local `DINS`,
snooped updates, reset, `DDEL`, and `DCLR` must produce the same entry and
cursor state as the host models.

Demand fills are local to the requesting core and must not populate every
core. Definition publication is a distinct, totally ordered coherence event:
it updates matching lines in all cores but never allocates a missing line or
advances its cursor. Rollback/global clear must invalidate every core before a
forgotten entry can execute. The fabric therefore needs exposed coherence
ports, backpressure, and deterministic ordering for simultaneous publishers;
tying broadcast/snoop off is not conformant.

The existing snoop implementation also needs replacement: it currently does
not reliably update matching entries or handle a full set and can create
duplicates. Focused RTL qualification must cover local full-set replacement,
matching snoop updates, snoops against full sets, global clear, and multicore
ordering. `K00044`, `K00109`, `K00192`, `K00350`, and `K00431` are useful
same-set names; all map to set `0x5e` under the required hash.

The logical fields total 350 bits, not 370. A practical packed implementation
rounds each entry to 384 bits (48 bytes), for about 48 KiB of entry storage per
core plus 64 bytes of replacement cursors. No RTL files are changed by the
host/BIOS implementation task.

## Qualification boundary

The implementation slice requires focused, seconds-scale checks for:

- reference/native parity across same-set fill, replacement, update, deletion,
  clear, reset, flags, and checkpoint rollback;
- positive and negative side-index lookup, hash collision, saturation fallback,
  long names, latest-binding shadowing, and pre-install fallback;
- demand-only cache allocation and update-existing definition publication;
- `MARKER`, `FORGET`, and transactional rollback rejecting stale bindings.

The landed host-cache selector passes all 15 reference/native cases. The
focused BIOS selectors pass 21 cases covering index behavior, rollback,
definition publication, dictionary-chain geometry, and the secondary-core
boot path affected by BIOS growth.

Cold source-load, Desktop smoke, source/warm equivalence, sustained cadence,
and RTL qualification remain deferred under the repository's current vertical
and resource gates. The profiler result is sizing evidence, not implemented
speedup evidence; the combined design must be timed after it is built.
