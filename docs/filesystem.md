# MP64FS Filesystem Specification

**MP64FS** (Megapad-64 File System) is a simple, flat filesystem designed
for the Megapad-64's storage controller.  It fits on a **1 MiB disk
image** (2048 × 512-byte sectors) and supports up to **64 named files**
with 15-character names.

This document covers:
- The on-disk layout (superblock, bitmap, directory, data area)
- Directory entry format and file types
- How allocation works
- The Python `diskutil.py` tool for managing disk images
- The KDOS Forth words for accessing the filesystem at runtime

---

## Disk Geometry

| Property | Value |
|----------|-------|
| Total size | 1,048,576 bytes (1 MiB) |
| Sector size | 512 bytes |
| Total sectors | 2,048 |
| Metadata sectors | 6 (sectors 0–5) |
| Data sectors | 2,042 (sectors 6–2047) |
| Max files | 64 |
| Max filename | 15 characters (null-terminated in 16 bytes) |

---

## On-Disk Layout

```
Sector 0          Superblock (format identifier + geometry)
Sector 1          Allocation Bitmap (one bit per sector)
Sectors 2–5       Directory (64 entries × 32 bytes = 2048 bytes)
Sectors 6–2047    Data Area (~1 MB usable storage)
```

### Superblock (Sector 0)

The superblock identifies the disk as MP64FS and records the filesystem
geometry.  The first 4 bytes are the magic number — if they don't read
`"MP64"`, the disk is not formatted.

| Offset | Size | Field | Value | Description |
|--------|------|-------|-------|-------------|
| +0 | 4 | `magic` | `b"MP64"` | Format identifier.  Always the ASCII bytes `4D 50 36 34`. |
| +4 | 2 | `version` | 1 (u16 LE) | Filesystem version number. |
| +6 | 2 | `total_sectors` | 2048 (u16 LE) | Total sectors on disk. |
| +8 | 2 | `bmap_start` | 1 (u16 LE) | Starting sector of the allocation bitmap. |
| +10 | 2 | `bmap_sectors` | 1 (u16 LE) | Number of sectors for the bitmap (always 1). |
| +12 | 2 | `dir_start` | 2 (u16 LE) | Starting sector of the directory. |
| +14 | 2 | `dir_sectors` | 4 (u16 LE) | Number of directory sectors. |
| +16 | 2 | `data_start` | 6 (u16 LE) | First data sector. |
| +18 | 494 | *reserved* | zeroes | Padding to fill the 512-byte sector. |

### Allocation Bitmap (Sector 1)

The bitmap tracks which sectors are allocated.  It uses **one bit per
sector** — bit N = 1 means sector N is in use.

- 2048 sectors ÷ 8 bits/byte = **256 bytes** of bitmap data
- The remaining 256 bytes of the sector are unused (zeroed)
- On a freshly formatted disk, sectors 0–5 (metadata) are marked as
  allocated; everything else is free

**Allocation is contiguous.**  When a file is created, it receives a run
of consecutive sectors.  There is no fragmentation handling — if you need
N sectors, they must be consecutive in the bitmap.

### Directory (Sectors 2–5)

The directory holds 64 fixed-size entries, each 32 bytes:

```
Offset   Size   Field        Description
───────  ─────  ───────────  ─────────────────────────────
+0       16     name         Null-terminated filename (max 15 chars)
+16      2      start_sec    Starting sector (u16 LE)
+18      2      sec_count    Number of allocated sectors (u16 LE)
+20      4      used_bytes   Actual bytes written (u32 LE)
+24      1      type         File type code (see below)
+25      1      flags        Bit flags (see below)
+26      6      reserved     Zeroed
```

64 entries × 32 bytes = 2048 bytes = 4 sectors.

An entry is **free** (empty) if `type == 0` and the name is all zeros.

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

### Flag Bits

| Bit | Meaning | Description |
|-----|---------|-------------|
| 0 | `readonly` | File should not be modified |
| 1 | `system` | System file (e.g., `kdos.f`) |
| 2 | `encrypted` | File data is AES-256-GCM encrypted (set by `FENCRYPT`) |

---

## How Allocation Works

### Creating a File

When `MKFILE` (or `diskutil inject`) creates a file:

1. **Find a free directory slot** — scan entries for `type == 0`
2. **Check for duplicate names** — abort if a file with the same name exists
3. **Find contiguous free sectors** — scan the bitmap for a run of N
   consecutive free bits
4. **Mark sectors allocated** — set the corresponding bits in the bitmap
5. **Write the directory entry** — name, start sector, sector count, type
6. **Sync to disk** — write bitmap + directory sectors back to the image

### Deleting a File

When `RMFILE` (or `diskutil delete`) removes a file:

1. **Find the directory entry** by name
2. **Clear the bitmap bits** for all sectors in the file's range
3. **Zero the directory entry** (name, start, count, type all set to 0)
4. **Sync to disk**

### Limitation: No Fragmentation Support

Files are always stored in contiguous sectors.  If the free space is
fragmented into small gaps, a large file creation may fail even though
enough total free space exists.  In practice, with a 1 MiB disk and
typical file sizes, this is rarely a problem.

---

## The `diskutil.py` Tool

`diskutil.py` is a Python command-line tool and library for creating and
managing MP64FS disk images from the host system.

### Command-Line Usage

```bash
# Create a blank, formatted 1 MiB image
python diskutil.py create myimage.img

# Inject a file into the image
python diskutil.py inject myimage.img myfile.f --type forth

# List all files on the image
python diskutil.py list myimage.img

# Read a file from the image
python diskutil.py read myimage.img myfile.f

# Delete a file from the image
python diskutil.py delete myimage.img myfile.f

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

### Python API

The `MP64FS` class provides programmatic access:

```python
from diskutil import MP64FS

# Create a new formatted image
fs = MP64FS()
fs.format()

# Inject a file
fs.inject("hello.f", b': greet ." Hello!" CR ;\n', file_type=3)

# List files
for entry in fs.list_files():
    print(entry['name'], entry['used_bytes'], 'bytes')

# Read a file
content = fs.read_file("hello.f")

# Delete a file
fs.delete_file("hello.f")

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
| `DIR` | List all files (name, size, type) + free space summary |
| `CATALOG` | Detailed listing (sector start, sector count, bytes, type) |
| `CAT filename` | Print file contents to terminal |
| `FS-FREE` | Report free space (sectors, bytes, file count) |

### Creating & Managing Files

| Word | Description |
|------|-------------|
| `n type MKFILE name` | Create a new file with *n* sectors and *type* |
| `RMFILE name` | Delete a file |
| `RENAME old new` | Rename a file |

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
| `DIRENT n` | Address of directory entry *n* in the RAM cache |
| `FIND-BY-NAME` | Search directory for a name (uses NAMEBUF) |

### File Encryption (§7.6.1)

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
