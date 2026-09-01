# MP64FS Filesystem Specification

**MP64FS** (Megapad-64 File System) is a sector-based filesystem designed
for the Megapad-64's storage controller.  One draft format marker (`1`)
uses the same derived-geometry rule from 15 through 65536 sectors (32 MiB).
The host utility still defaults to **1 MiB** (2048 × 512-byte sectors) and
the filesystem supports up to
**128 named files** with 23-character names, hierarchical subdirectories,
two-extent metadata, RTC timestamp fields, and a CRC32 integrity field.

This document covers:

- On-disk layout (superblock, bitmap, directory, data area)
- 48-byte directory entry format, file types, and flags
- Parent-byte subdirectory model
- Two-extent allocation and defragmentation
- Stream files (circular ring buffers)
- Symbolic links
- CRC32 data integrity (hardware-accelerated)
- The Python `diskutil.py` tool for managing disk images
- The KDOS Forth words for runtime filesystem access

## Implementation Status

This document specifies the on-disk format and also records intended MP64FS
behavior. It is not a blanket claim that every described maintenance workflow
already exists in `kdos.f`. The current source defines secondary-extent
metadata; its later loader concatenates both validated extents, `RMFILE` frees
both, and `FD-FILL` copies both into a descriptor. Its `MKFILE` allocates only
one contiguous primary run, while legacy `FREAD`/`FWRITE`, `CAT`,
`SAVE-BUFFER`, and `LOAD-BUFFER` do not generally traverse the secondary
extent. It does not
currently define `FAPPEND`, `FS-CHECK`, `FS-COMPACT`, `STREAM-OPEN`, or
`STREAM-WRITE`, and its ordinary file writes do not maintain `data_crc32` on
every update. Those portions below are design/host-tool behavior until
matching runtime words land and are qualified.

The hosted simulator's contiguous unchanged-source frontier currently ends at
`kdos.f` line 8339. It qualifies the initial MP64FS cache, derived geometry,
bitmap, first-fit search, packed directory helpers, and the unchanged
`FS-LOAD`, `FS-SYNC`, `FS-ENSURE`, and `FORMAT` lifecycle on pathless in-memory
media, followed by `.FTYPE`, `DIR`, and `CATALOG` over the cached directory and
bitmap, then exact-name lookup, `MKFILE`/`RMFILE`/`RENAME` metadata mutation,
bounded primary-extent `CAT` publication, cache-only total/largest-free and
global occupancy reporting, primary-extent `SAVE-BUFFER`/`LOAD-BUFFER`, the
fixed FD pool with cached `OPEN`, used-metadata `FFLUSH`, and final `FCLOSE`,
then the checked source compiler, nested two-extent filesystem `LOAD`,
application loader, ANSI byte helpers, whole-file encryption, parent-byte
subdirectory navigation/mutation, and the paged Documentation Browser through
ordinary descriptors and `FREAD`/`FCLOSE`, followed by raw linked-header
Dictionary Search, the task registry/synchronous run-to-completion executor,
Timer Preemption Setup, Multicore Dispatch's honest one-core
validation/fallback behavior, the §8.2–§8.7 queue, affinity, flag, message,
and named-lock state machines, the §8.8–§8.9 cluster-control and MPU failure
boundary, absent-network forward bridge, ANSI screen registry/control layer,
and §9.5–§9.6 widget-vector SDL and ordinary screen definitions.
The exact 5286–5408 fixture contains 123 lines and 4,020 bytes, with SHA-256
`a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028`.
The exact 5409–5436 fixture contains 28 LF lines and 838 bytes, with SHA-256
`e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23`
and Git blob `2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5`.
The exact 5437–5471 fixture contains 35 LF lines and 984 bytes, with SHA-256
`6ad3b135d3b2b69f651814349899f507d56dde4c876c8be9e0cd7aefd4a1d75c`
and Git blob `1884c81ba2b8aa48082d472250f13a2265fd1def`.
The exact 5472–5514 fixture contains 43 LF lines and 1,317 bytes, with SHA-256
`7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104`
and Git blob `8b4645f16c7ac2f21036282a896b7ede6bad16b0`. Its six definitions, in source
order, are `SB-SLOT`, `SB-DESC`, `SAVE-BUFFER`, `LB-SLOT`, `LB-DESC`, and
`LOAD-BUFFER`; loading zeroes the four variables and installs the two colon
bodies and strings without any filesystem, Buffer, media, diagnostic, flush,
or output effect.
The exact 5515–5610 fixture contains 96 LF lines and 3,397 bytes, with SHA-256
`16637705bd8d26e0e92b14605ba0e4e772ec2d5d5c9eb02bbd107714c8650c78`
and Git blob `e01ffa80d946b2cddd50e37bcefd9421a1b8dbb9`. Its 14-definition
source-order ledger is `FD-MAX`, `FD-SLOT-SZ`, `FD-POOL`, `FD-SLOT`,
`FD-ALLOC`, `(FCLOSE-NOFS)`, `FCLOSE`, `FD-FILL`, `OP-SLOT`, `(OPEN)`,
`OPEN`, `F.SLOT`, `FFLUSH`, and `(FCLOSE)`. Load zero-fills the 1,152-byte
pool, zeroes `OP-SLOT`, binds `FCLOSE` first to `(FCLOSE-NOFS)` and finally to
`(FCLOSE)`, and binds `OPEN` to `(OPEN)`. It performs no filesystem or media
I/O, synchronization, diagnostic update, or output.
The exact 6201–6296 navigation fixture contains 96 LF records and 3,082 bytes,
with SHA-256
`dc7f065cfac1fc3eb6efd1de7f4b0f472ff40e66fa14666e1087c18047e1d6c8`
and Git blob `b964ca87a1af44e54b22abd25116edd2a7e2a853`. Its ledger is the raw
64-byte `_PWD-STK` body followed by `PWD`, `CD`, `MKDIR`, and `RMDIR`; loading
publishes those words without a filesystem, cache, media, RTC, diagnostic,
lock, or output effect.

The exact 6297–6427 Documentation Browser fixture contains 131 LF records and
3,945 bytes, with SHA-256
`442e5e39598d71a589bf19d6345c5bb042d678ba9f51607a878ae5030fbdcee6`
and Git blob `242fc879957ba14f3a00b3284e8af921a4fa365c`. It publishes 13
definitions, including a raw 512-byte buffer and one zeroed counter, without
filesystem/media access, FD allocation, input, output, or synchronization.

`FS-LOAD` consumes the separately qualified native
`MP64FS-VALID?` word with its executable raw-device reads, scratch layout,
metadata predicate, and generation check. This boundary is not evidence of
file-backed close/reopen durability, later KDOS source, general multi-extent
content I/O, malformed mutation/content safety,
allocator improvement, compaction, repair, or stronger filesystem validation.

---

## Disk Geometry

| Property | Value |
|----------|-------|
| Supported size | 15–65536 sectors (up to 32 MiB); host default 2048 sectors |
| Sector size | 512 bytes |
| Bitmap sectors | `ceil(total_sectors / 4096)` (1 through 16) |
| Directory start | `1 + bitmap_sectors` |
| Data start | `directory_start + 12` |
| Max files | 128 |
| Max filename | 23 characters (NUL-terminated in 24 bytes) |
| Directory entry size | 48 bytes |
| Max extents per file | 2 (primary + one secondary) |

The superblock stores `total_sectors` as a **u32**, but marker 1 deliberately
caps it at 65536 sectors.  Its u16 extent starts can name every valid LBA from
0 through 65535 at that exact capacity, and the runtime caches at most sixteen
bitmap sectors.  The 1 MiB host default and 32 MiB Desktop image use the same
marker and formula.

---

## On-Disk Layout

```
Sector 0                          Superblock (format identifier + geometry)
Sector 1 .. bmap_sectors          Allocation bitmap (one bit per sector)
Next 12 sectors                   Directory (128 × 48 bytes = 6144 bytes)
Remaining sectors                 Data area
```

### Superblock (Sector 0)

The superblock identifies the disk as MP64FS and records the filesystem
geometry.  The first 4 bytes are the magic number — if they don't read
`"MP64"`, the disk is not formatted.

| Offset | Size | Field | Value | Description |
|--------|------|-------|-------|-------------|
| +0 | 4 | `magic` | `b"MP64"` | Format identifier.  Always the ASCII bytes `4D 50 36 34`. |
| +4 | 2 | `marker` | 1 (u16 LE) | The single accepted draft format marker. |
| +6 | 4 | `total_sectors` | u32 LE | Must exactly equal attached media capacity and be ≤65536. |
| +10 | 2 | `bmap_start` | 1 (u16 LE) | Starting sector of the allocation bitmap. |
| +12 | 2 | `bmap_sectors` | u16 LE | Exactly `ceil(total_sectors / 4096)`. |
| +14 | 2 | `dir_start` | u16 LE | Exactly `bmap_start + bmap_sectors`. |
| +16 | 2 | `dir_sectors` | 12 (u16 LE) | Number of directory sectors. |
| +18 | 2 | `data_start` | u16 LE | Exactly `dir_start + dir_sectors`. |
| +20 | 1 | `max_files` | 128 (u8) | Maximum directory entries. |
| +21 | 1 | `entry_size` | 48 (u8) | Bytes per directory entry. |
| +22 | 490 | *reserved* | zeroes | Canonical producer padding; executable BIOS validation ignores it. |

### Allocation Bitmap (Starting at Sector 1)

The bitmap tracks which sectors are allocated.  It uses **one bit per
sector** — bit N = 1 means sector N is in use.

- One 512-byte bitmap sector represents 4096 sectors; the 65536-sector
  geometry uses sixteen bitmap sectors
- On a freshly formatted disk, sectors 0 through `data_start - 1` are marked
  allocated; everything else is free
- Bitmap count, directory start, and data start are always derived and
  validated rather than selected by another format marker

### Directory (12 Sectors at `dir_start`)

The directory holds **128 fixed-size entries**, each **48 bytes**:

```
Offset   Size   Field        Description
───────  ─────  ───────────  ─────────────────────────────────────────
+0       24     name         NUL-terminated filename (max 23 chars)
+24      2      start_sec    Primary extent start sector (u16 LE)
+26      2      sec_count    Primary extent sector count (u16 LE)
+28      4      used_bytes   Actual content bytes (u32 LE)
+32      1      type         File type code (see below)
+33      1      flags        Bit flags (see below)
+34      1      parent       Parent directory index (0xFF = root)
+35      1      reserved     Zeroed
+36      4      mtime        Modification time — epoch seconds (u32 LE)
+40      4      data_crc32   CRC32 of file content (u32 LE)
+44      2      ext1_start   Secondary extent start sector (u16 LE, 0 = none)
+46      2      ext1_count   Secondary extent sector count (u16 LE)
```

128 entries × 48 bytes = 6,144 bytes = 12 sectors.

Canonical producers encode a **free** slot by zeroing all 48 bytes. Executable
BIOS validation and the low-level `FIND-FREE-SLOT` helper, however, use only
`name[0]`: zero makes the slot empty and the other 47 bytes are ignored.
Fully zero tails are therefore a producer convention, not a validator-enforced
invariant.

The BIOS predicate performs up to three raw whole-device checked reads in
order: superblock, active bitmap, and the 12-sector directory. It checks canonical geometry,
reserved allocation bits, occupied-entry parent/type rules, allocated extent
bounds, directory zero-extent rules, and used-byte capacity. It does not check
name termination or character policy, uniqueness, flags, reserved entry
bytes, timestamps, CRCs, parent cycles or root reachability, extent
disjointness, orphan allocations, bitmap tail bits, or file data. Those
producer rules and stronger integrity properties remain format requirements
or planned checks, not facts established by `MP64FS-VALID?`.

`DIR` and `CATALOG` pass every occupied entry name directly to BIOS `.ZSTR`.
That word consumes its address, publishes bytes one at a time until the first
NUL, and has no length bound; it also publishes control and escape bytes
without sanitizing them. A later memory fault preserves its already published
prefix. Because the validator does not require termination, an accepted
occupied entry can make those listings publish adjacent metadata or entries.
Hosted listing qualification therefore requires the canonical producer
invariant of a NUL within the 24-byte name field.

#### Key Fields

- **`parent`** — Index (0–127) of the parent directory entry, or `0xFF`
  for root-level files/directories.  See §Subdirectories below.

- **`mtime`** — Last-modified timestamp as seconds since Unix epoch,
  read from the RTC's epoch counter (`EPOCH@ 1000 /`).  Set on every
  source path that explicitly updates it. The layout comment at `kdos.f` line
  5026 instead says “seconds since boot”; that comment disagrees with the
  executable `TICKS@` definition and this format specification.

  The hosted epoch register is explicit and deterministic: it defaults to
  zero, changes only through host control or direct MMIO writes, and does not
  consult or advance with wall time. `TICKS@` uses signed division and returns
  a full cell; `MKFILE` stores only its low 32 bits. `RENAME` does not update
  this field. `MS@`, calendar, automatic, and realtime RTC behavior remain
  unqualified.

- **`data_crc32`** — CRC32 of the file's content bytes (not the full sector
  padding). The host tool populates it; automatic hardware recomputation on
  every KDOS write is intended but not currently implemented. See §Integrity
  below.

- **`ext1_start` / `ext1_count`** — Optional second extent.  If a file
  cannot fit in one contiguous run, a second run is allocated.  A file's
  total capacity is `(sec_count + ext1_count) × 512` bytes.  The content
  fills the primary extent first, then continues into the secondary
  extent.  See §Two-Extent Allocation below.

### File Type Codes

| Code | Constant | Meaning | When to Use |
|------|----------|---------|-------------|
| 0 | `FTYPE_FREE` | Free slot | Empty directory entry |
| 1 | `FTYPE_RAW` | Raw binary | Arbitrary binary data |
| 2 | `FTYPE_TEXT` | Plain text | Human-readable text files |
| 3 | `FTYPE_FORTH` | Forth source | `.f` files — can be loaded with `LOAD` |
| 4 | `FTYPE_DOC` | Documentation | Browsable with `DOC` and `DESCRIBE` |
| 5 | `FTYPE_DATA` | Structured data | Application data, saved buffers |
| 6 | `FTYPE_TUT` | Tutorial | Step-by-step lessons, browsable with `TUTORIAL` |
| 7 | `FTYPE_BUNDLE` | Pipeline bundle | Declarative config, loadable with `BUNDLE-LOAD` |
| 8 | `FTYPE_DIR` | Directory | Subdirectory (no data sectors) |
| 9 | `FTYPE_STREAM` | Stream | Circular ring buffer (see §Stream Files) |
| 10 | `FTYPE_LINK` | Symbolic link | Target path stored in data (see §Symbolic Links) |

### Flag Bits

| Bit | Meaning | Description |
|-----|---------|-------------|
| 0 | `readonly` | File should not be modified |
| 1 | `system` | System file (e.g., `kdos.f`) |
| 2 | `encrypted` | File data is GCM-encrypted by `FENCRYPT` (intended AES-256; executable mode is ambient) |
| 3 | `append` | Append-only — writes extend `used_bytes` within existing allocation |

---

## Subdirectories

MP64FS implements hierarchical directories using a **parent-byte model**.
No separate tree structure is needed — the flat 128-entry directory array
is the tree.

### How It Works

Each directory entry has a 1-byte `parent` field:

| `parent` value | Meaning |
|----------------|---------|
| `0xFF` | Entry is in the root directory |
| `0`–`127` | Entry is inside the directory at that index |

A **directory** is simply an entry with `type = FTYPE_DIR` (8).
Directories have `start_sec = 0`, `sec_count = 0`, and `used_bytes = 0`
— they occupy no data sectors.  Their only role is to serve as a `parent`
target for other entries.

### Path Resolution

Path interpretation is command-specific. Host `diskutil` paths and KDOS's
`_RESOLVE-PATH` helper for `LOAD`/`REQUIRE` use `/`-separated traversal;
leading `/` starts at root and a `..` directory component moves to the parent.
The public `CD` word below does **not** use that resolver: it recognizes only
the complete tokens `..` and `/`, and otherwise treats its token as one direct
child name. `.` has no special meaning in the current KDOS resolver or `CD`.

To resolve `/tools/crypto/aes-test.f`:

1. Start at root (`parent = 0xFF`).
2. Scan all entries for `name == "tools"` where `parent == 0xFF` and
   `type == FTYPE_DIR`.  Suppose this is entry index 4.
3. Scan for `name == "crypto"` where `parent == 4` and `type == FTYPE_DIR`.
   Suppose this is entry index 11.
4. Scan for `name == "aes-test.f"` where `parent == 11`.  Found.

Resolving `..` from entry index 11: read `entry[11].parent` → 4, so the
parent directory is entry 4.  From entry 4, `entry[4].parent` → 0xFF,
so its parent is root.

### Constraints

- The packed format can link up to 128 entries, but runtime `PWD` displays only
  the eight components nearest CWD and silently omits higher ancestors.
- Deleting a directory requires it to be empty (no entries with that
  index as their `parent`).
- The current directory's logical value is one byte (`0..127`, or `0xFF` for
  root), but runtime `CWD` stores it in a full 64-bit cell.

These are canonical tree constraints. BIOS validation proves only that a
non-root parent names an occupied directory entry; it accepts self-parenting,
cycles, directories unreachable from root, and exact duplicate siblings.
Runtime lookup is first-slot-wins. A parent cycle makes `PWD` loop instead of
reaching root.

### Example Directory Structure

```
/
├── kdos.f          (entry 0, parent=0xFF, type=FORTH)
├── tools/          (entry 1, parent=0xFF, type=DIR)
│   ├── crypto/     (entry 2, parent=1,    type=DIR)
│   │   └── aes.f   (entry 3, parent=2,    type=FORTH)
│   └── bench.f     (entry 4, parent=1,    type=FORTH)
├── docs/           (entry 5, parent=0xFF, type=DIR)
│   └── getting.doc (entry 6, parent=5,    type=DOC)
└── log.stream      (entry 7, parent=0xFF, type=STREAM)
```

---

## Two-Extent Allocation

Files can occupy up to **two extents** (contiguous runs of sectors).
This reduces allocation failures caused by bitmap fragmentation while
keeping the design simple — no block lists, no indirect sectors, no
extent trees.

BIOS validation requires every sector named by an extent to be allocated, but
does not reconstruct ownership. Overlapping extents and allocated orphan
sectors are accepted.

### Creating a File

The host-side `diskutil inject` path can create a two-extent layout. Its exact
publication order is:

1. Validate the formatted image, name, parent path, and duplicate-name rule.
2. Try one complete free run; if none exists, choose the largest primary run
   and then a second run for the remainder.
3. Mark both runs and publish the bitmap to the in-memory image.
4. Write the content across the primary and secondary runs.
5. Compute CRC32 with the host implementation.
6. Find the first directory slot whose `name[0]` is zero, construct the entry,
   and publish the directory to the image.

If two extents cannot satisfy the request, allocation fails. This host path is
not transactional: because the slot check follows bitmap and content writes,
a directory-full failure can leave those earlier image changes. Current KDOS
`MKFILE` does not implement the two-run fallback; it succeeds only when one
contiguous primary run satisfies the complete request and leaves the secondary
extent zero.

The admitted KDOS path requires a positive run, nonempty canonical component,
non-directory valid type, valid current parent, and validator-approved
geometry. It checks duplicate, free slot, and free run before mutation, then
marks cached bitmap bits, constructs an entry with `used_bytes = 0`, and calls
`FS-SYNC`. It does not clear the allocated data sectors. An empty name allocates
bits but leaves `name[0] = 0`, creating an invisible orphan; type 8 with its
positive run is validator-invalid. Because `FS-LOAD` retains `CWD`, creation
after rebinding can also publish a stale parent rejected by the next load.

### Appending to a File

The intended append contract extends `used_bytes` within the existing
allocation without re-allocating when the append flag (bit 3) is set. A future
`FAPPEND` word would compute the byte offset, select the extent, and write the
data; current `kdos.f` does not define that word.

If `used_bytes` reaches total capacity (`(sec_count + ext1_count) × 512`),
the append fails — the file must be recreated with a larger allocation.

### Data Layout Across Extents

Content bytes fill the primary extent first, sector by sector.  Once the
primary extent is full, content continues into the secondary extent.
The split point is `sec_count × 512` bytes.

```
Logical byte 0 ──────────────────► sec_count × 512 ──────────────► capacity
│         Primary extent          │       Secondary extent          │
│  start_sec .. start_sec+N-1    │  ext1_start .. ext1_start+M-1  │
```

### Deleting a File

When `RMFILE` (or `diskutil delete`) removes a file:

1. **Find the directory entry** by name in the current directory.
2. **Clear bitmap bits** for both extents (primary + secondary if present).
3. **Zero the directory entry** (all 48 bytes set to 0).
4. **Sync to disk.**

KDOS performs the cache mutations before `FS-SYNC` and never wipes payload.
`RMFILE` must not target a directory: its ordinary zero-count primary-extent
`DO` traverses the modulo-cell range. It also assumes extents are disjoint and
exclusively owned even though BIOS validation accepts overlaps; otherwise it
can clear allocation bits still referenced by another entry.

### Renaming a File

`RENAME` compares complete zero-padded 24-byte names, changes only that name
field, and then syncs. It does not update `mtime`; renaming to the same name is
reported as taken. An empty replacement makes the entry invisible without
releasing its sectors.

All three metadata mutations precede the nontransactional bitmap, directory,
flush sequence. A late failure leaves changed cache and can leave earlier media
effects; non-stale failure can leave `FS-OK` true, and simply repeating the
command can short-circuit against the changed cache rather than repair media.

---

## Stream Files

A **stream file** (`type = FTYPE_STREAM`, code 9) is a fixed-size
circular ring buffer — ideal for logs, sensor data, or event traces.
It has a pre-allocated sector range and overwrites oldest data when full.

### Behavior

- **`used_bytes`** stores the **write-head byte offset** within the
  allocated space (0 to `capacity - 1`, where `capacity = sec_count × 512`).
- Writes append at the write-head position and advance it.
- When the write-head reaches capacity, it wraps to 0, overwriting
  the oldest data.
- The `append` flag (bit 3) is always set on stream files.

### Reading a Stream

To read a stream's contents in chronological order:

1. If the buffer has never wrapped (total bytes written < capacity),
   read from byte 0 to `used_bytes - 1`.
2. If the buffer has wrapped, read from `used_bytes` to end of
   allocation (oldest data), then from byte 0 to `used_bytes - 1`
   (newest data).

The intended `data_crc32` field on a stream file covers the *current* buffer
contents at the time of the last CRC update. Since streams are append-heavy,
the design calls for recalculation only during the planned `FS-CHECK` pass.

### Forth Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `STREAM-OPEN` *(planned)* | `( "name" -- fd )` | Open a stream file; not currently defined in `kdos.f` |
| `STREAM-WRITE` *(planned)* | `( addr len fd -- )` | Circular write; not currently defined in `kdos.f` |

---

## Symbolic Links

A **symbolic link** (`type = FTYPE_LINK`, code 10) stores a target path
in its data sector(s).  The target is a NUL-terminated path string
(e.g., `tools/crypto/aes.f` or `/kdos.f`).

### Behavior

- A link occupies at least 1 data sector.  `used_bytes` is the length
  of the target path string (including the NUL terminator).
- When KDOS resolves a filename and encounters a link, it reads the
  target path and re-resolves it.  A maximum of **4 link hops** is
  enforced to prevent cycles.
- Links can point to files in other directories (relative or absolute
  paths).
- Deleting a link removes only the link entry — the target is unaffected.

---

## CRC32 Data Integrity

The format reserves `data_crc32` for a CRC32 checksum of content bytes from
byte 0 through `used_bytes - 1`. The hardware CRC DMA engine can compute that
checksum at 8 bytes/cycle. Current KDOS file mutation paths do not yet keep the
field current on every write; the automatic behavior below is the target
contract rather than admitted runtime behavior. `MP64FS-VALID?` does not read
file data or verify this field.

### When CRC Is Computed

- **On file creation** — if data is provided, CRC is computed and stored.
- **On write / SAVE-BUFFER** — CRC is recomputed over the full content.
- **On append** — CRC is recomputed over the full content (including the
  appended bytes).

### Verification

A planned `FS-CHECK` word would walk every non-free, non-directory entry, read
its data via DMA, compute CRC32, and compare against the stored `data_crc32`.
`kdos.f` does not currently define it. Intended diagnostics look like:

```
> FS-CHECK
 128 entries checked, 14 files verified.
 CRC OK.
```

If a mismatch is found:

```
> FS-CHECK
 Entry 7 "log.stream" CRC MISMATCH: stored=0x1A2B3C4D computed=0xDEADBEEF
 1 error(s).
```

---

## Defragmentation

Over time, file creation and deletion can leave the bitmap fragmented.
A planned `FS-COMPACT` operation would consolidate free space by moving file
data into contiguous regions and collapsing two-extent files into single
extents. `kdos.f` does not currently define it.

### Algorithm

1. **Sort live entries** by primary extent start sector.
2. **Pack forward** — read each file's data into a RAM buffer, allocate
   a single contiguous run at the lowest available sector, write data
   back, update the directory entry (`start_sec`, `sec_count`,
   `ext1_start = 0`, `ext1_count = 0`).
3. **Rebuild bitmap** from the updated directory entries.
4. **Sync** superblock + bitmap + directory to disk.

After compaction, all files are single-extent and all free space is one
contiguous region at the end of the data area.

```
> FS-COMPACT
 Compacting... moved 6 files, freed 3 extents.
 Free: 1800 contiguous sectors.
```

---

## The `diskutil.py` Tool

`diskutil.py` is a Python command-line tool and library for creating and
managing MP64FS disk images from the host system.

### Command-Line Usage

```bash
# Create a blank, formatted 32 MiB image
python diskutil.py format -o myimage.img --sectors 65536

# Inject a file into the image (root directory by default)
python diskutil.py inject myimage.img myfile.f --type forth

# Inject a file into a subdirectory
python diskutil.py inject myimage.img aes.f --type forth --path /tools/crypto

# Create a subdirectory
python diskutil.py mkdir myimage.img tools
python diskutil.py mkdir myimage.img crypto --path /tools

# List all files on the image
python diskutil.py ls myimage.img

# Read a file from the image
python diskutil.py cat myimage.img myfile.f

# Delete a file from the image
python diskutil.py rm myimage.img myfile.f

# Verify CRC integrity of all files
python diskutil.py check myimage.img

# Defragment the image
python diskutil.py compact myimage.img

# Build the standard sample image with all docs and tutorials
python diskutil.py sample
```

### File Type Flags for Injection

When injecting a file, use `--type` to set the file type:

| `--type` | Code | Description |
|----------|------|-------------|
| `raw` | 1 | Raw binary data |
| `text` | 2 | Plain text |
| `forth` | 3 | Forth source (loadable with `LOAD`) |
| `doc` | 4 | Documentation topic |
| `data` | 5 | Structured data |
| `tutorial` | 6 | Tutorial/lesson |
| `bundle` | 7 | Pipeline bundle (declarative config) |
| `stream` | 9 | Stream (circular ring buffer) |
| `link` | 10 | Symbolic link |

### Python API

The `MP64FS` class provides programmatic access:

```python
from diskutil import FTYPE_FORTH, MP64FS

# Create a new formatted image
fs = MP64FS()
fs.format()

# Create subdirectories
fs.mkdir("/tools")
fs.mkdir("/tools/crypto")

# Inject a file into root
fs.inject_file("hello.f", b': greet ." Hello!" CR ;\n', ftype=FTYPE_FORTH)

# Inject a file into a subdirectory
fs.inject_file("aes.f", data, ftype=FTYPE_FORTH, path="/tools/crypto")

# Create a symbolic link
fs.mklink("quick.f", target="/tools/crypto/aes.f")

# Create a stream file (pre-allocate 4 sectors)
fs.mkstream("log.stream", sectors=4)

# List all entries. Pass a directory slot as parent= to filter.
for entry in fs.list_files():
    print(entry.name, entry.used_bytes, 'bytes')

# Read a file
content = fs.read_file("hello.f")

# Delete a file
fs.delete_file("hello.f")

# Verify CRC integrity
errors = fs.check()

# Defragment
fs.compact()

# Get filesystem info
info = fs.info()
print(f"Free: {info['free_sectors']} sectors")

# Save to disk
fs.save("myimage.img")

# Load from disk
fs = MP64FS.load("myimage.img")
```

### The Sample Image

`build_sample_image()` creates a fully-populated disk image with:

- **KDOS core** (`kdos.f`) — packed executable Forth source with blank lines
  and full-line backslash comments omitted; the BIOS loads it into Bank 0 as
  the first Forth-type file on disk
- **Networking** (`networking.f`) — packed loadable module containing Ethernet
  through TLS, sockets, and the UDP-backed data-port transport
- **Boot script** (`autoexec.f`) — enters the XMEM userland dictionary, loads
  `networking.f` with KDOS `REQUIRE`, configures the network, and loads
  `tools.f`
- **User modules** — `tools.f` is loaded by standard autoexec; `graphics.f`
  is present for explicit loading
- **10 documentation topics** — getting-started, buffers, kernels,
  pipelines, data-ports, scheduler, screens, filesystem, tile-engine,
  reference
- **5 tutorials** — hello-world, first-kernel, build-pipeline,
  data-ingest, custom-kernel
- **demo-data** — 256-byte test data file
- **demo-bundle** — sample pipeline bundle (type 7)

This is the standard "ship it" disk image.  The BIOS loads the KDOS core;
KDOS then runs autoexec, which loads networking and tools in userland.
Packing changes no executable line, inline comment, or string content.  It
keeps `kdos.f` within `FSLOAD`'s bounded Bank 0 DMA window and reduces the
disk and XMEM source-buffer footprint of the larger `networking.f` module.
KDOS reads that module in bounded 255-sector batches and concatenates its
validated secondary extent before evaluation.

---

## KDOS Filesystem Words

These words are available at the KDOS Forth prompt for working with
MP64FS.  The filesystem is automatically loaded at boot if a disk is
present.

### Browsing & Inspection

| Word | Description |
|------|-------------|
| `DIR` | List files in current directory (name, size, type) + free space summary |
| `CATALOG` | List name, bytes, primary sector count, numeric type, and flags + free-space summary |
| `CAT filename` | Print file contents to terminal |
| `FS-LARGEST-FREE` | Return the largest cached contiguous free run; low-level and unguarded |
| `FS-FREE` | Report cached free sectors/bytes, largest run, and global occupied entries/max |

The qualified hosted `DIR` and `CATALOG` paths inspect occupied direct
children of `CWD` in the global cache. Their free-space summaries count clear
bitmap bits over the data-sector range rather than reconstructing ownership
from directory extents. `CATALOG` reports only the primary sector count, and
all numeric fields use signed `.` in the current `BASE`. `FS-ENSURE` trusts an
already-true `FS-OK`, so detached or replaced media can leave stale cache
output eligible. This listing qualification and the adjacent admitted lookup
and mutation slices are pathless; none establishes close/reopen durability.

The hosted `CAT` slice is likewise pathless and has no load-time filesystem or
output effect: loading only zeroes `CAT-SLOT` and installs the word and inline
strings. At execution it checks for an unavailable filesystem before parsing,
then a name miss, then zero `DE.USED`. Those branches respectively leave the
filename token for the outer evaluator, print `Not found`, or print
`(empty file)`; miss and empty perform no file-data read.

For a nonempty match, `CAT` generation-binds one read of the complete primary
extent into the unreserved address at `HERE`, without advancing `HERE`, then
emits exactly `DE.USED` bytes. LF becomes CRLF; every other byte, including CR,
NUL, and ESC, is emitted raw, and no newline is appended. Safe use requires a
stable generation, canonical matched non-directory file, one small primary
extent, no secondary extent, `DE.USED <= DE.COUNT * 512`, and a complete unused
mapped DMA span at `HERE`. The source neither checks those bounds nor the type.
It ignores a validator-approved secondary extent, so content beyond primary
capacity comes from stale unread bytes after the DMA span. A failed read emits
no file content but can leave a partial scratch prefix. `CAT-SLOT`, parser state,
diagnostics, and the `HERE` scratch are global and unlocked. Blank line 5437 is
the leading seam of the admitted free-space reporting slice.

Loading that slice only zeroes `LF-BEST` and `LF-RUN` and installs
`FS-LARGEST-FREE`, `FS-FREE`, and their inline strings; it performs no ensure,
cache scan, media/diagnostic access, or output. `FS-LARGEST-FREE` itself has no
`FS-OK` gate. It resets its global scratch and scans the cached data-sector
bitmap, retaining the largest run even when it reaches `FS-TOTAL`.

`FS-FREE` ensures and checks the filesystem first. Failure prints
`No filesystem` without scanning or changing `LF-*`. Success separately scans the
cache for total clear bits and the largest clear run, then counts all occupied
directory entries globally by nonzero `name[0]`. The count ignores `CWD`,
includes directories, and is nevertheless labeled `files`. The report uses
signed `.` in the current `BASE` for free sectors, `free * 512` bytes, largest
contiguous sectors, occupied entries, and the 128-entry maximum.

Safe reporting requires validator-approved positive geometry and complete
cache spans; direct helper use does not establish them. An already-true
`FS-OK` is not revalidated, so replaced or detached media can leave stale
results eligible without I/O. The scans and `LF-*` scratch are global,
unlocked, and not a coherent allocation snapshot. This remains reporting only;
the planned runtime allocator improvements, `FS-CHECK`/repair, and
`FS-COMPACT` sections below remain aspirational. Blank line 5472 is the leading
seam of the admitted Buffer-I/O fixture described below.

### Directory Navigation

| Word | Description |
|------|-------------|
| `CD name` | Change to exact `..`, root `/`, or one direct type-8 child; embedded `/` is not a separator |
| `PWD` | Print root or the retained suffix of at most eight components, with leading/trailing `/` |
| `MKDIR name` | Create a metadata-only subdirectory in the lowest logically free slot, then sync |
| `RMDIR name` | Clear one direct empty subdirectory and sync; nonempty rejection leaks its slot on the stack |

`CD`, `MKDIR`, and `RMDIR` ensure/check the filesystem before parsing. With no
filesystem they print `No filesystem` and leave the would-be name token for the
outer evaluator. Beyond shared parser scratch, ordinary successful `CD` changes
only volatile CWD and issues no storage command. The mutation words write the unchanged bitmap, complete
directory, then flush; they allocate/free no data sectors and do not update the
parent mtime.

Safe runtime use requires a stable validator-approved cache, a root or live
directory CWD, sibling-unique nonempty 1–23-byte NUL-terminated simple names,
an acyclic root-reaching parent chain, and synchronous non-reentrant calls.
`MKDIR` does not enforce that name domain: an empty token creates an invisible
metadata-bearing but logically free slot; longer tokens silently truncate to
23 bytes; and `..` or `/` entries are shadowed by CD's operators. Mutation
ignores MP64FS policy flags, changes cache before nontransactional sync, and
does not invalidate saved loader/REQUIRE CWD snapshots when a directory is
removed. CWD, NAMEBUF/PATHBUF/PN-LEN parser state, `_PWD-STK`, and cache state
are global and unlocked.

### Creating & Managing Files

| Word | Description |
|------|-------------|
| `n type MKFILE name` | Reserve one contiguous primary run and create an empty file in the current directory |
| `RMFILE name` | Clear both extents and the entry without wiping payload; files only, not directories |
| `RENAME old new` | Replace only the name; `mtime` is retained |
| `FAPPEND` *(planned)* | `( addr len fd -- )` Append data to a file with the `append` flag; not currently defined in `kdos.f` |

`FIND-BY-NAME` and these commands compare all 24 name bytes, not merely the
visible prefix. Validator-accepted post-NUL tails can prevent a match, and the
first exact duplicate shadows later entries. If the filesystem is unavailable,
the mutation words return before parsing their name tokens, so those tokens
remain for the outer evaluator. `RENAME` also leaves its proposed new token
when the old name is absent.

### Integrity & Maintenance

| Word | Description |
|------|-------------|
| `FS-CHECK` *(planned)* | Verify CRC32 of all files against stored checksums; not currently defined in `kdos.f` |
| `FS-COMPACT` *(planned)* | Defragment and collapse two-extent files; not currently defined in `kdos.f` |

### Stream Files

| Word | Description |
|------|-------------|
| `STREAM-OPEN name` *(planned)* | Open a stream file; not currently defined in `kdos.f` |
| `STREAM-WRITE` *(planned)* | `( addr len fd -- )` Circular write; not currently defined in `kdos.f` |

### Loading & Saving

| Word | Description |
|------|-------------|
| `LOAD filename` | Resolve an MP64FS Forth source path, concatenate validated primary/secondary extents, and evaluate its physical lines |
| `buf SAVE-BUFFER name` | Write an existing file's complete primary allocation from `B.DATA`, cache low-u32 `B.LEN` as `used_bytes`, then sync |
| `buf LOAD-BUFFER name` | Read an existing file's complete primary allocation, including padding, into `B.DATA` without changing the Buffer descriptor |

Both admitted words ensure and check the filesystem before storing the Buffer
descriptor or parsing the name. With no filesystem they drop the descriptor,
leave the name token for the outer evaluator, print `No filesystem`, and do not
change the `SB-*`/`LB-*` scratch. A miss comes after the descriptor and parsed
name are stored and the slot becomes `-1`, but before Buffer dereference or
I/O. The save miss additionally suggests creating the file with `MKFILE`.

On a match, the transfer length is the full primary allocation
(`DE.COUNT * 512`), not `DE.USED`. Neither word follows the optional secondary
extent. `SAVE-BUFFER` performs its generation-bound payload write first,
stores the low 32 bits of cell-sized `B.LEN` in the cached directory entry,
then invokes `FS-SYNC` (bitmap, directory, flush). It retains the entry's name,
extents, type, flags, parent, `mtime`, and CRC. The current word therefore does
not implement the automatic CRC-on-save target described later in this
document and does not timestamp the update. A payload failure can retain a
partial media prefix without changing cached `used_bytes`; a later sync or
flush failure can leave payload and metadata partly published and the cache
changed. This is deliberately documented nontransactionality, not durability.

`LOAD-BUFFER` reads the complete allocation into `B.DATA`, including padding
after `DE.USED`, and leaves `B.LEN`, every other Buffer field, and all file
metadata unchanged. A failed generation-bound read can retain a partial
Buffer prefix. Complete success reports cached `DE.USED`, whereas save reports
`B.LEN`; both use signed `.` in the ambient `BASE`.

`B.LEN` is an element count, while `B.BYTES` is byte capacity. Because the
unchanged save word stores and labels `B.LEN` as bytes but transfers whole
sectors, multi-byte Buffers expose a source-width discrepancy. Safe use with
ordinary Buffer constructors requires a byte-width Buffer whose
`B.LEN = B.BYTES = DE.COUNT * 512`, with the full `B.DATA` range mapped and
readable for save or writable for load; the saved length must represent the
intended unsigned 32-bit field, and save requires a writable selected volume.
It also requires a stable mounted generation,
a canonical matched non-directory file, one positive in-range primary extent,
and no secondary extent. None of those descriptor, capacity, type, or extent
conditions or the entry's read-only/system flags is checked here. Scratch
cells, parser state, cache, and diagnostics
are global and unlocked. Blank line 5515 leads into the admitted FD-pool slice.

### File Descriptor Pool and Cached Open

The pool has 16 fixed 72-byte slots and is fully zeroed at source load. A
returned fdesc points eight bytes into its slot:

| Slot offset | fdesc offset | Field |
|---:|---:|---|
| `+0` | — | in-use header (`0` free, `-1` allocated) |
| `+8` | `+0` | primary start sector |
| `+16` | `+8` | primary maximum sector count |
| `+24` | `+16` | used bytes |
| `+32` | `+24` | cursor |
| `+40` | `+32` | cached directory slot |
| `+48` | `+40` | secondary start sector |
| `+56` | `+48` | secondary sector count |
| `+64` | `+56` | reserved |

`FD-ALLOC` scans lowest slot first, marks the first free header, and returns
its fdesc; it returns zero at 16-slot exhaustion. It never clears payload.
`FD-FILL` snapshots the cached directory fields through secondary count and
sets cursor to zero, but does not touch reserved `+56`. That cell begins zero
and is retained across fill, close, and reuse, as are all payload cells when a
slot is merely released and allocated again. The named `(FCLOSE-NOFS)` helper
remains directly callable: zero is a no-op and nonzero clears only the header,
always bypassing persistence.

`OPEN` calls `FS-ENSURE` and checks `FS-OK` before parsing. Gate failure prints
`No filesystem`, returns zero, and leaves the name token and `OP-SLOT`
unchanged. A miss records `-1`, prints the parsed name, and returns zero before
allocation. Exhaustion retains the matched slot, prints `No free FD slots`,
and returns zero. Success selects the lowest free descriptor, snapshots cached
primary/secondary coordinates, used count, and directory slot, resets cursor,
and produces no output. With an already-true `FS-OK`, open performs no storage
or payload I/O; only an initial `FS-ENSURE` load can do metadata I/O.

This snapshot has no binding/generation identity and does not revalidate a
true `FS-OK`, type, flags, or directory status. It permits multiple opens of
the same entry and does not coordinate their independent cursor and used
counts. Directory mutation, cache reload, and storage rebinding can stale an
open descriptor, while later flush order among duplicates can overwrite a
newer used count. The copied secondary fields document descriptor layout only;
they do not qualify multi-extent `FREAD`, `FWRITE`, or other content I/O.

`FFLUSH` checks `FS-OK` before descriptor access. A false marker prints `FS not
loaded`, drops the descriptor, and does nothing else. With a true marker it
stores only low-u32 `F.USED` in the cached directory entry selected by
`F.SLOT`, then calls nontransactional `FS-SYNC`. It never writes file payload
or changes the name, extents, type, flags, parent, `mtime`, or CRC. It validates
neither fdesc/directory-slot identity nor used against capacity; `L!` truncates
the cell to low u32. The cache changes before bitmap/directory writes and flush,
so failure can retain the new cache value and a partial media prefix.

Final `FCLOSE` treats zero as a no-op. For nonzero input with true `FS-OK`, it
calls `FFLUSH` and releases only after a successful return; a flush failure
keeps the header allocated while cache/media effects may remain. With false
`FS-OK`, it silently discards persistence and releases. Release clears only
the in-use header, retaining descriptor/reserved cells and leaving file payload
untouched. No operation validates pool membership, alignment, allocation, or
directory identity. Lowest-first reuse creates an ABA hazard: a stale fdesc can
flush or close a new occupant. Pool/header state, `OP-SLOT`, parser/cache state,
and deferred targets are global and unlocked. The contiguous hosted frontier
continues through §9.1–§9.6 registry/control, widget-vector SDL, and ordinary
screen definitions at line 8339; the screen-label, dispatch, registration, and
event-loop tail begins at line 8340.

### Documentation Access

| Word | Description |
|------|-------------|
| `TOPICS` | Globally list every occupied type-4 cached name, ignoring CWD/parent |
| `LESSONS` | Globally list every occupied type-6 cached name, ignoring CWD/parent |
| `DOC name` | Use ordinary current-directory `OPEN` and page the selected payload |
| `TUTORIAL name` | Identical to `DOC`; no tutorial-type check is performed |
| `DESCRIBE word` | Globally select the lowest-slot, case-sensitive type-4 filename match |

These are compatibility descriptions, not stronger safety guarantees.
`DOC`/`TUTORIAL` do not validate type, encryption, CRC, or directory status;
`DESCRIBE` does not search the Forth dictionary or file contents. The browser
emits arbitrary payload control bytes, maps LF to CRLF, pauses after every
twentieth LF, and starts `SHOW-FILE` at the descriptor's incoming cursor.
Legacy `FREAD` ignores secondary extents, so browsing a valid split file can
publish adjacent sectors. Successful high-level display closes through
`FFLUSH`/`FS-SYNC` and therefore writes and flushes media. Open failure leaves
a zero on the data stack in `DOC`, `TUTORIAL`, and DESCRIBE's final open path;
read/input/sync failure can leak the allocated descriptor.

### Low-Level Access

| Word | Description |
|------|-------------|
| `FORMAT` | Initialize a fresh filesystem on the attached disk |
| `FS-LOAD` | Load superblock + bitmap + directory into RAM |
| `FS-SYNC` | Write RAM cache back to disk |
| `FS-ENSURE` | Auto-load FS if not yet loaded |
| `FD-ALLOC` | Allocate the lowest free fixed-pool slot, returning its retained fdesc or zero |
| `OPEN name` | Ensure and find a cached name, allocate an FD, and snapshot its directory fields; deferred to `(OPEN)` |
| `FFLUSH fdesc` | Cache low-u32 `F.USED` and run `FS-SYNC`; no payload write |
| `FCLOSE fdesc` | Flush used metadata before release when `FS-OK`; otherwise silently release; deferred to `(FCLOSE)` |
| `DIRENT n` | Address of directory entry *n* in the RAM cache (48 bytes each) |
| `FIND-BY-NAME` | Return the first occupied current-directory entry whose complete 24-byte name matches zero-padded `NAMEBUF`; it does not check `FS-OK` |

The admitted lifecycle is ordered and nontransactional. `FS-LOAD` clears
`FS-OK`, destructively rebinds raw storage, validates, then publishes
superblock geometry, bitmap, and directory; a late read failure can retain
earlier caches and binding while leaving `FS-OK = 0`, and it never resets
`CWD`. The validation and cache reads are not one coherent snapshot.

`FS-SYNC` writes bitmap then directory and flushes, never the superblock;
failure does not undo earlier writes. `FS-ENSURE` trusts a true `FS-OK` without
checking attachment identity. `FORMAT` writes superblock, active bitmap, and
directory before flush, and only flush success sets `FS-OK = -1` and root
`CWD`. Failed format retains constructed caches, geometry, binding, and any
completed metadata writes. Neither format nor sync erases data sectors, and
format does not clear the inactive bitmap-cache tail.

### File Encryption

| Word | Description |
|------|-------------|
| `FS-KEY!` | `( addr -- )` Set 256-bit encryption key for file operations |
| `ENCRYPTED?` | `( fdesc -- flag )` Check whether a file's encrypted flag is set |
| `FENCRYPT` | `( fdesc -- result... )` Encrypt in-place using the ambient AES-GCM key mode; returns 0 on success/no-op, -1 for capacity or first-allocation failure, and malformed `0 -1` on second-allocation failure; storage/sync failures throw |
| `FDECRYPT` | `( fdesc -- result... )` Authenticate and decrypt in-place; returns 0 on success/no-op, -1 for authentication or first-allocation failure, and malformed `0 -1` on second-allocation failure; storage/sync failures throw |

The current source does **not** encrypt each sector independently. It performs
one whole-file GCM transaction over `used_bytes` rounded up to 16 bytes, stores
one 16-byte tag immediately after that ciphertext, then rounds the combined
span to sectors for disk I/O. The IV is the little-endian directory-slot cell
followed by four zero bytes; it is not derived from each sector. A file must be
`OPEN`ed before either operation, must use one contiguous primary extent, and
must reserve room for the rounded ciphertext plus tag.

This interface is compatibility behavior, not production-safe nonce
management. Decrypt/change/re-encrypt of one slot, or later reuse of that slot,
repeats the IV under an unchanged key. File metadata and exact logical length
are not authenticated as AAD, no key-set marker exists, and the source relies
on the shared AES engine's ambient key mode rather than forcing AES-256. Bytes
between `used_bytes` and the 16-byte boundary come from existing disk slack,
not guaranteed zero padding. Payload and directory flag updates are separate
writes; failure is nontransactional, and post-allocation aborts leak unwiped
DMA buffers. `FENCRYPT` also trusts the returned output/tag without checking
AES status. The wrapper ignores the MP64FS readonly flag, although lower layers
still enforce device write protection. `FDECRYPT` returns 0 for a file that is
not encrypted even though its nearby source comment says that case returns -1;
an encrypted empty file also returns 0 without clearing its flag.
