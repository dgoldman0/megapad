# MP64FS Filesystem Specification

**MP64FS** (Megapad-64 File System) is a sector-based filesystem designed
for the Megapad-64's storage controller.  The default geometry fits on a
**1 MiB disk image** (2048 × 512-byte sectors) and supports up to
**128 named files** with 23-character names, hierarchical subdirectories,
two-extent allocation, RTC timestamps, and hardware-verified CRC32
integrity.

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

---

## Disk Geometry

| Property | Value |
|----------|-------|
| Total size | 1,048,576 bytes (1 MiB) |
| Sector size | 512 bytes |
| Total sectors | 2,048 |
| Metadata sectors | 14 (sectors 0–13) |
| Data sectors | 2,034 (sectors 14–2047) |
| Max files | 128 |
| Max filename | 23 characters (NUL-terminated in 24 bytes) |
| Directory entry size | 48 bytes |
| Max extents per file | 2 (primary + one secondary) |

The superblock stores `total_sectors` as a **u32**, so larger disk images
(e.g., 4 MiB, 16 MiB) work simply by increasing the image size and
bitmap sector count.  The 1 MiB default is the reference geometry.

---

## On-Disk Layout

```
Sector 0           Superblock (format identifier + geometry)
Sector 1           Allocation Bitmap (one bit per sector)
Sectors 2–13       Directory (128 entries × 48 bytes = 6144 bytes)
Sectors 14–2047    Data Area (~1 MB usable storage)
```

### Superblock (Sector 0)

The superblock identifies the disk as MP64FS and records the filesystem
geometry.  The first 4 bytes are the magic number — if they don't read
`"MP64"`, the disk is not formatted.

| Offset | Size | Field | Value | Description |
|--------|------|-------|-------|-------------|
| +0 | 4 | `magic` | `b"MP64"` | Format identifier.  Always the ASCII bytes `4D 50 36 34`. |
| +4 | 2 | `version` | 1 (u16 LE) | Filesystem version number. |
| +6 | 4 | `total_sectors` | 2048 (u32 LE) | Total sectors on disk. |
| +10 | 2 | `bmap_start` | 1 (u16 LE) | Starting sector of the allocation bitmap. |
| +12 | 2 | `bmap_sectors` | 1 (u16 LE) | Number of bitmap sectors. |
| +14 | 2 | `dir_start` | 2 (u16 LE) | Starting sector of the directory. |
| +16 | 2 | `dir_sectors` | 12 (u16 LE) | Number of directory sectors. |
| +18 | 2 | `data_start` | 14 (u16 LE) | First data sector. |
| +20 | 1 | `max_files` | 128 (u8) | Maximum directory entries. |
| +21 | 1 | `entry_size` | 48 (u8) | Bytes per directory entry. |
| +22 | 490 | *reserved* | zeroes | Padding to fill the 512-byte sector. |

### Allocation Bitmap (Sector 1)

The bitmap tracks which sectors are allocated.  It uses **one bit per
sector** — bit N = 1 means sector N is in use.

- 2048 sectors ÷ 8 bits/byte = **256 bytes** of bitmap data
- The remaining 256 bytes of the sector are unused (zeroed)
- On a freshly formatted disk, sectors 0–13 (metadata) are marked as
  allocated; everything else is free
- For larger disk images, additional bitmap sectors are used
  (e.g., a 4 MiB / 8192-sector disk needs 2 bitmap sectors)

### Directory (Sectors 2–13)

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

An entry is **free** (empty) if `type == 0` and the name is all zeros.

#### Key Fields

- **`parent`** — Index (0–127) of the parent directory entry, or `0xFF`
  for root-level files/directories.  See §Subdirectories below.

- **`mtime`** — Last-modified timestamp as seconds since Unix epoch,
  read from the RTC's epoch counter (`EPOCH@ 1000 /`).  Set on every
  write or rename.

- **`data_crc32`** — CRC32 of the file's content bytes (not the full
  sector padding).  Computed by the hardware CRC DMA engine on every
  write.  See §Integrity below.

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
| 2 | `encrypted` | File data is AES-256-GCM encrypted (set by `FENCRYPT`) |
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

Paths use `/` as the separator.  Leading `/` means absolute (from root);
otherwise the path is relative to the current directory.  `.` refers to
the current directory and `..` refers to the parent.

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

- Maximum directory depth is limited only by the 128-entry array.
- Deleting a directory requires it to be empty (no entries with that
  index as their `parent`).
- The current directory is tracked at runtime as a single byte (the
  current directory's index, or `0xFF` for root).

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

### Creating a File

When `MKFILE` (or `diskutil inject`) creates a file:

1. **Find a free directory slot** — scan entries for `type == 0`.
2. **Check for duplicate names** — abort if a file with the same name
   exists in the same parent directory.
3. **Try single-extent allocation** — scan the bitmap for a contiguous
   run of N free sectors.  If found, set `start_sec` and `sec_count`.
4. **Fall back to two-extent allocation** — if no single run of N
   sectors is available, find the largest free run (becomes the primary
   extent), then find a second run for the remainder (becomes the
   secondary extent at `ext1_start` / `ext1_count`).
5. **Mark sectors allocated** — set the corresponding bits in the bitmap.
6. **Write the directory entry** — name, extents, type, parent, mtime.
7. **Compute CRC32** — if data is provided, feed it through the hardware
   CRC DMA engine and store the result in `data_crc32`.
8. **Sync to disk** — write bitmap + directory sectors back to the image.

If two extents still cannot satisfy the request, allocation fails.

### Appending to a File

When the `append` flag (bit 3) is set, writes extend `used_bytes` within
the existing allocation without re-allocating.  The word `FAPPEND`
computes the byte offset, determines which extent the write falls in,
and writes the data.

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

The `data_crc32` field on a stream file covers the *current* buffer
contents at the time of the last CRC update.  Since streams are
append-heavy, CRC is only recalculated on explicit `FS-CHECK`.

### Forth Words

| Word | Stack Effect | Description |
|------|-------------|-------------|
| `STREAM-OPEN` | `( "name" -- fd )` | Open a stream file, return descriptor |
| `STREAM-WRITE` | `( addr len fd -- )` | Write bytes to stream (wraps on overflow) |

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

Every file's `data_crc32` field stores a CRC32 checksum of its content
bytes (from byte 0 to `used_bytes - 1`).  This checksum is computed
by the **hardware CRC DMA engine** (8 bytes/cycle), making it fast
enough to run on every write without noticeable overhead.

### When CRC Is Computed

- **On file creation** — if data is provided, CRC is computed and stored.
- **On write / SAVE-BUFFER** — CRC is recomputed over the full content.
- **On append** — CRC is recomputed over the full content (including the
  appended bytes).

### Verification

The `FS-CHECK` word walks every non-free, non-directory entry, reads its
data via DMA, computes CRC32, and compares against the stored
`data_crc32`.  Mismatches are reported with the entry index and name.

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
`FS-COMPACT` consolidates free space by moving file data into contiguous
regions and collapsing two-extent files into single extents.

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
# Create a blank, formatted 1 MiB image
python diskutil.py create myimage.img

# Inject a file into the image (root directory by default)
python diskutil.py inject myimage.img myfile.f --type forth

# Inject a file into a subdirectory
python diskutil.py inject myimage.img aes.f --type forth --path /tools/crypto

# Create a subdirectory
python diskutil.py mkdir myimage.img /tools/crypto

# List all files on the image
python diskutil.py list myimage.img

# List files in a subdirectory
python diskutil.py list myimage.img /tools

# Read a file from the image
python diskutil.py read myimage.img myfile.f

# Delete a file from the image
python diskutil.py delete myimage.img myfile.f

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
from diskutil import MP64FS

# Create a new formatted image
fs = MP64FS()
fs.format()

# Create subdirectories
fs.mkdir("/tools")
fs.mkdir("/tools/crypto")

# Inject a file into root
fs.inject("hello.f", b': greet ." Hello!" CR ;\n', file_type=3)

# Inject a file into a subdirectory
fs.inject("aes.f", data, file_type=3, path="/tools/crypto")

# Create a symbolic link
fs.mklink("quick.f", target="/tools/crypto/aes.f")

# Create a stream file (pre-allocate 4 sectors)
fs.mkstream("log.stream", sectors=4)

# List files (optionally filtered by directory)
for entry in fs.list_files(path="/tools"):
    print(entry['name'], entry['used_bytes'], 'bytes')

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

- **KDOS** (`kdos.f`) — the full operating system as a Forth source file
  (auto-booted by the BIOS as the first Forth-type file on disk)
- **10 documentation topics** — getting-started, buffers, kernels,
  pipelines, data-ports, scheduler, screens, filesystem, tile-engine,
  reference
- **5 tutorials** — hello-world, first-kernel, build-pipeline,
  data-ingest, custom-kernel
- **demo-data** — 256-byte test data file
- **demo-bundle** — sample pipeline bundle (type 7)

This is the standard "ship it" disk image that boots KDOS automatically.

---

## KDOS Filesystem Words

These words are available at the KDOS Forth prompt for working with
MP64FS.  The filesystem is automatically loaded at boot if a disk is
present.

### Browsing & Inspection

| Word | Description |
|------|-------------|
| `DIR` | List files in current directory (name, size, type) + free space summary |
| `CATALOG` | Detailed listing (extents, bytes, type, parent, mtime, CRC) |
| `CAT filename` | Print file contents to terminal |
| `FS-FREE` | Report free space (sectors, bytes, file count) |

### Directory Navigation

| Word | Description |
|------|-------------|
| `CD path` | Change current directory (`CD /tools/crypto`, `CD ..`, `CD /`) |
| `PWD` | Print working directory path |
| `MKDIR name` | Create a subdirectory in the current directory |
| `RMDIR name` | Remove an empty subdirectory |

### Creating & Managing Files

| Word | Description |
|------|-------------|
| `n type MKFILE name` | Create a new file with *n* sectors and *type* in current directory |
| `RMFILE name` | Delete a file from current directory |
| `RENAME old new` | Rename a file |
| `FAPPEND` | `( addr len fd -- )` Append data to a file with the `append` flag |

### Integrity & Maintenance

| Word | Description |
|------|-------------|
| `FS-CHECK` | Verify CRC32 of all files against stored checksums (hw-accelerated) |
| `FS-COMPACT` | Defragment: pack files, collapse two-extent files to single extent |

### Stream Files

| Word | Description |
|------|-------------|
| `STREAM-OPEN name` | Open a stream file, return descriptor |
| `STREAM-WRITE` | `( addr len fd -- )` Write bytes to stream (circular, wraps on overflow) |

### Loading & Saving

| Word | Description |
|------|-------------|
| `LOAD filename` | Open a Forth source file and EVALUATE each line |
| `buf SAVE-BUFFER name` | Save a buffer's data to an existing named file |

### Documentation Access

| Word | Description |
|------|-------------|
| `TOPICS` | List all doc-type files |
| `LESSONS` | List all tutorial-type files |
| `DOC name` | Page through a documentation file |
| `TUTORIAL name` | Walk through a tutorial file |
| `DESCRIBE word` | Search docs for info about a word |

### Low-Level Access

| Word | Description |
|------|-------------|
| `FORMAT` | Initialize a fresh filesystem on the attached disk |
| `FS-LOAD` | Load superblock + bitmap + directory into RAM |
| `FS-SYNC` | Write RAM cache back to disk |
| `FS-ENSURE` | Auto-load FS if not yet loaded |
| `OPEN name` | Open a file, return a file descriptor for FREAD/FWRITE |
| `DIRENT n` | Address of directory entry *n* in the RAM cache (48 bytes each) |
| `FIND-BY-NAME` | Search directory for a name within the current directory |

### File Encryption

| Word | Description |
|------|-------------|
| `FS-KEY!` | `( addr -- )` Set 256-bit encryption key for file operations |
| `ENCRYPTED?` | `( fdesc -- flag )` Check whether a file's encrypted flag is set |
| `FENCRYPT` | `( fdesc -- )` Encrypt file in-place using AES-256-GCM, set encrypted flag |
| `FDECRYPT` | `( fdesc -- )` Decrypt file in-place, verify auth tag, clear encrypted flag |

Encryption operates at the sector level: each sector is encrypted as
a separate AES-256-GCM block with a unique IV derived from sector
index.  The authentication tag is stored alongside the ciphertext.
A file must be `OPEN`ed before encrypting/decrypting.
