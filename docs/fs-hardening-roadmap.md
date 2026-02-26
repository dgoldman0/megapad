# MP64FS Hardening Roadmap

Prompted by the "No space on disk" issue: with Roboto + 35 pipeline
files consuming 3874 of 4096 bitmap-trackable sectors, only 222
remained — not enough for a 938-sector BMP.  The XMEM extraction
workaround avoids this cleanly, but the incident exposed several
capacity and robustness limits worth addressing.

---

## Phase 0 — Understand the Constraints (no code)

| Parameter | Current | Limit Imposed |
|-----------|---------|---------------|
| Bitmap | 1 sector (512 bytes) | 4096 sectors max (2 MiB) |
| Image default | 2048 sectors (1 MiB) | Comfortable for small FS |
| Directory | 128 entries × 48 B | 128 files max, flat scan |
| Extents per file | 2 | Large files need contiguous runs or fail |
| Filename | 23 chars | Tight for paths with directories |
| `total_sectors` in superblock | u32 | No structural limit, but bitmap is the bottleneck |

The bitmap is the hard ceiling.  Everything else scales if it grows.

---

## Phase 1 — Multi-Sector Bitmap  ★ highest impact

**Problem:** `BMAP_SECTORS = 1` caps trackable sectors at 4096.  Any
image > 2 MiB hits this wall even though the superblock already stores
`total_sectors` as a u32.

**Plan:**
- Make `BMAP_SECTORS` dynamic: `ceil(total_sectors / (512 × 8))`.
- Update `DATA_START` to `BMAP_START + bmap_sectors + DIR_SECTORS + …`
  (read from superblock, not hardcoded).
- `diskutil.py`: `format()` computes bitmap size from `total_sectors`,
  writes it into the superblock, adjusts `dir_start` and `data_start`.
- `kdos.f`: `FS-LOAD` reads `bmap_sectors` from superblock, DMA-reads
  that many sectors into an appropriately-sized RAM cache.
- Default stays 1 MiB / 1-sector bitmap for backward compat; larger
  images (e.g. `--sectors 8192`) get 2 bitmap sectors automatically.

**Risk:** Low — clean arithmetic change.  Existing 1 MiB images are
unaffected (their superblock says `bmap_sectors = 1`).

**Estimated scope:** ~60 lines Python, ~30 lines Forth.

---

## Phase 2 — Allocation Pre-Check & Reporting

**Problem:** `MKFILE` in kdos.f prints "No space on disk" and drops the
request.  The user gets no guidance on *how much* space is needed vs
available, or whether compaction would help.

**Plan:**
- `MKFILE` error message: include requested vs available sector counts.
  `"  Need 938 sectors, only 222 free"`.
- Add `FS-LARGEST-FREE ( -- n )` that scans the bitmap and returns the
  largest contiguous run.  Print it alongside free count in `FS-FREE`.
- `diskutil.py` `inject_file()`: raise a descriptive `RuntimeError`
  with the same numbers instead of the generic message.
- Consider auto-suggesting `FS-COMPACT` when total free ≥ needed but
  largest contiguous < needed (fragmentation case).

**Estimated scope:** ~20 lines Forth, ~10 lines Python.

---

## Phase 3 — Best-Fit Two-Extent Allocator

**Problem:** The current two-extent fallback finds the *largest* free
run as primary, then looks for a second run.  This is greedy — it can
waste a big run on a small file's primary extent, leaving nothing for
the secondary.

**Plan:**
- Switch to best-fit: find the *smallest* run ≥ half the request for
  primary, then find any run ≥ remainder for secondary.
- Add a fast free-run census: scan bitmap once, collect (start, length)
  tuples, sort by length.  Use this for both single-extent and
  two-extent decisions.
- `diskutil.py` and `kdos.f` both get the improved allocator.

**Estimated scope:** ~40 lines Python, ~50 lines Forth.

---

## Phase 4 — Bitmap ↔ Directory Cross-Validation

**Problem:** No tool currently checks whether the bitmap and directory
agree.  A stale bitmap bit (from a crash mid-`FS-SYNC`) silently leaks
sectors.  A cleared bit on an allocated sector silently corrupts data.

**Plan:**
- `FS-CHECK` already walks files and verifies CRC.  Extend it:
  1. Rebuild a shadow bitmap from directory entries (both extents).
  2. Compare against on-disk bitmap bit-by-bit.
  3. Report leaked sectors (bitmap=1, no file claims it) and
     collisions (bitmap=0, but file claims it; or two files overlap).
- `diskutil.py` `check()` gets the same logic.
- Optional `FS-REPAIR` to fix the bitmap from directory truth.

**Estimated scope:** ~50 lines Python, ~40 lines Forth.

---

## Phase 5 — Journaling / Crash Safety  (longer term)

**Problem:** `FS-SYNC` writes bitmap then directory in two separate DMA
transactions.  A power loss between them leaves an inconsistent state.

**Plan (pick one):**
- **Option A — Write-ahead intent sector:** Reserve 1 sector for a
  write-intent record.  Before any mutation: write intent (op, slot,
  old bitmap bits).  After mutation: clear intent.  On boot, if intent
  sector is non-zero, replay or rollback.
- **Option B — Shadow directory:** Write directory to an alternate
  location, then flip a "commit" pointer in the superblock.  Atomic
  at the sector level (single-sector superblock write).
- Either option costs 1–2 extra sectors of metadata.

**Estimated scope:** ~100 lines Forth, ~60 lines Python, needs careful
testing.

---

## Phase 6 — Larger Directory Table (optional)

**Problem:** 128 entries is tight when subdirectories, docs, tutorials,
bundles, streams, and links all compete for slots.

**Plan:**
- Bump `MAX_FILES` to 256 (needs 24 directory sectors,
  `DIR_SECTORS = 24`).  Or make it superblock-driven.
- `parent` field stays u8, which supports 0–254 as indices (0xFF =
  root), so 255 is the max without changing the entry format.
- Alternative: keep 128 entries but add a "directory overflow sector"
  pointer in the superblock for a second directory block.

**Estimated scope:** Trivial constant change + test pass.

---

## Priority & Sequencing

| Phase | Priority | Dependency | Why |
|-------|----------|------------|-----|
| 1 | **P0** | None | Removes the hard ceiling that caused the original issue |
| 2 | **P0** | None | Low-effort UX fix, prevents user confusion |
| 4 | **P1** | None | Silent corruption is worse than crashes |
| 3 | **P1** | None | Reduces wasted space, fewer "no space" errors |
| 6 | **P2** | Phase 1 | Nice-to-have, only matters at scale |
| 5 | **P2** | Phase 4 | Significant complexity, save for v1.1+ |

Phases 1 and 2 can ship together as a single PR.  Phase 4 is a natural
follow-up.  Phase 5 is a design decision that warrants its own RFC.

---

## Non-Goals

- **Block-level indirection / extent trees:** Over-engineered for a
  retro-system FS.  Two extents + compaction is the right trade-off.
- **File-level fragmentation beyond 2 extents:** `FS-COMPACT` is the
  answer; adding more extents complicates read/write paths for
  marginal gain.
- **Permissions / ACLs:** Single-user system; `readonly` and `system`
  flags are sufficient.
