"""
diskutil.py — MP64FS disk image utility for Megapad-64.

Provides Python-side tools for creating, formatting, and populating
MP64FS disk images.  These images can then be attached to the Megapad-64
emulator via --storage and accessed from KDOS.

Disk layout (1 MiB = 2048 × 512-byte sectors):
    Sector 0       Superblock
    Sector 1       Allocation bitmap (2048 bits = 256 bytes)
    Sectors 2-5    Directory (64 entries × 32 bytes)
    Sectors 6+     Data area

Superblock (sector 0, 512 bytes):
    +0   magic[4]       b"MP64"
    +4   version[2]     u16 LE  (1)
    +6   total_sec[2]   u16 LE  (2048)
    +8   bmap_start[2]  u16 LE  (1)
    +10  bmap_secs[2]   u16 LE  (1)
    +12  dir_start[2]   u16 LE  (2)
    +14  dir_secs[2]    u16 LE  (4)
    +16  data_start[2]  u16 LE  (6)
    +18  reserved[494]  zeroed

Directory entry (32 bytes):
    +0   name[16]       null-terminated (max 15 chars)
    +16  start_sec[2]   u16 LE
    +18  sec_count[2]   u16 LE
    +20  used_bytes[4]  u32 LE
    +24  type[1]        0=free 1=raw 2=text 3=forth 4=doc 5=data
    +25  flags[1]       bit0=readonly bit1=system
    +26  reserved[6]    zeroed

Allocation bitmap:
    Bit N = 1 means sector N is allocated.
    Sectors 0-5 (superblock + bitmap + directory) are always allocated.
"""

from __future__ import annotations

import os
import struct
import sys
import time
import zlib
from pathlib import Path
from dataclasses import dataclass, field

# ── Constants ──────────────────────────────────────────────────────────

SECTOR_SIZE = 512
DEFAULT_TOTAL_SECTORS = 2048        # 1 MiB
MAGIC = b"MP64"
FS_VERSION = 1

BMAP_START = 1
BMAP_SECTORS = 1
DIR_START = 2
DIR_SECTORS = 12
DATA_START = 14

DIR_ENTRY_SIZE = 48
MAX_NAME_LEN = 23
MAX_FILES = 128

PARENT_ROOT = 0xFF

# File types
FTYPE_FREE   = 0
FTYPE_RAW    = 1
FTYPE_TEXT   = 2
FTYPE_FORTH  = 3
FTYPE_DOC    = 4
FTYPE_DATA   = 5
FTYPE_TUT    = 6
FTYPE_BUNDLE = 7
FTYPE_DIR    = 8
FTYPE_STREAM = 9
FTYPE_LINK   = 10

FTYPE_NAMES = {
    FTYPE_FREE: "free", FTYPE_RAW: "raw", FTYPE_TEXT: "text",
    FTYPE_FORTH: "forth", FTYPE_DOC: "doc", FTYPE_DATA: "data",
    FTYPE_TUT: "tutorial", FTYPE_BUNDLE: "bundle",
    FTYPE_DIR: "dir", FTYPE_STREAM: "stream", FTYPE_LINK: "link",
}

# Flag bits
FLAG_READONLY  = 0x01
FLAG_SYSTEM    = 0x02
FLAG_ENCRYPTED = 0x04
FLAG_APPEND    = 0x08


# ── Data classes ───────────────────────────────────────────────────────

@dataclass
class DirEntry:
    """One 48-byte directory entry."""
    name: str
    start_sector: int
    sector_count: int
    used_bytes: int
    ftype: int
    flags: int
    parent: int = PARENT_ROOT
    mtime: int = 0
    data_crc32: int = 0
    ext1_start: int = 0
    ext1_count: int = 0

    @property
    def type_name(self) -> str:
        return FTYPE_NAMES.get(self.ftype, f"?{self.ftype}")

    @property
    def readonly(self) -> bool:
        return bool(self.flags & FLAG_READONLY)

    @property
    def system(self) -> bool:
        return bool(self.flags & FLAG_SYSTEM)

    @property
    def encrypted(self) -> bool:
        return bool(self.flags & FLAG_ENCRYPTED)

    @property
    def append_only(self) -> bool:
        return bool(self.flags & FLAG_APPEND)

    @property
    def total_sectors(self) -> int:
        """Primary + secondary extent sectors."""
        return self.sector_count + self.ext1_count

    @property
    def capacity(self) -> int:
        """Total byte capacity across both extents."""
        return self.total_sectors * SECTOR_SIZE


# ── Low-level helpers ──────────────────────────────────────────────────

def _sectors_needed(nbytes: int) -> int:
    """Number of 512-byte sectors needed to hold *nbytes*."""
    return (nbytes + SECTOR_SIZE - 1) // SECTOR_SIZE


def _read_entry(data: bytes, offset: int) -> DirEntry | None:
    """Parse a 48-byte directory entry.  Returns None if slot is free."""
    raw = data[offset : offset + DIR_ENTRY_SIZE]
    if raw[0] == 0:
        return None
    name = raw[0:24].split(b"\x00", 1)[0].decode("ascii", errors="replace")
    start      = struct.unpack_from("<H", raw, 24)[0]
    count      = struct.unpack_from("<H", raw, 26)[0]
    used       = struct.unpack_from("<I", raw, 28)[0]
    ftype      = raw[32]
    flags      = raw[33]
    parent     = raw[34]
    mtime      = struct.unpack_from("<I", raw, 36)[0]
    data_crc   = struct.unpack_from("<I", raw, 40)[0]
    ext1_start = struct.unpack_from("<H", raw, 44)[0]
    ext1_count = struct.unpack_from("<H", raw, 46)[0]
    return DirEntry(name, start, count, used, ftype, flags,
                    parent, mtime, data_crc, ext1_start, ext1_count)


def _write_entry(buf: bytearray, offset: int, entry: DirEntry):
    """Serialise a DirEntry into 48 bytes at *offset* in *buf*."""
    # Zero the slot first
    buf[offset : offset + DIR_ENTRY_SIZE] = b"\x00" * DIR_ENTRY_SIZE
    name_bytes = entry.name.encode("ascii")[:MAX_NAME_LEN]
    buf[offset : offset + len(name_bytes)] = name_bytes
    struct.pack_into("<H", buf, offset + 24, entry.start_sector)
    struct.pack_into("<H", buf, offset + 26, entry.sector_count)
    struct.pack_into("<I", buf, offset + 28, entry.used_bytes)
    buf[offset + 32] = entry.ftype
    buf[offset + 33] = entry.flags
    buf[offset + 34] = entry.parent
    struct.pack_into("<I", buf, offset + 36, entry.mtime)
    struct.pack_into("<I", buf, offset + 40, entry.data_crc32)
    struct.pack_into("<H", buf, offset + 44, entry.ext1_start)
    struct.pack_into("<H", buf, offset + 46, entry.ext1_count)


def _clear_entry(buf: bytearray, offset: int):
    """Zero a 48-byte directory slot."""
    buf[offset : offset + DIR_ENTRY_SIZE] = b"\x00" * DIR_ENTRY_SIZE


def _compute_crc32(data: bytes) -> int:
    """CRC32 of *data* as unsigned 32-bit value."""
    return zlib.crc32(data) & 0xFFFFFFFF


def _epoch_seconds() -> int:
    """Current Unix epoch in seconds (u32)."""
    return int(time.time()) & 0xFFFFFFFF


# ── Bitmap helpers ─────────────────────────────────────────────────────

def _bitmap_get(bmap: bytearray, sector: int) -> bool:
    """Return True if *sector* is marked allocated."""
    byte_idx, bit_idx = divmod(sector, 8)
    if byte_idx >= len(bmap):
        return False
    return bool(bmap[byte_idx] & (1 << bit_idx))


def _bitmap_set(bmap: bytearray, sector: int):
    """Mark *sector* as allocated."""
    byte_idx, bit_idx = divmod(sector, 8)
    if byte_idx < len(bmap):
        bmap[byte_idx] |= (1 << bit_idx)


def _bitmap_clear(bmap: bytearray, sector: int):
    """Mark *sector* as free."""
    byte_idx, bit_idx = divmod(sector, 8)
    if byte_idx < len(bmap):
        bmap[byte_idx] &= ~(1 << bit_idx) & 0xFF


def _bitmap_find_free(bmap: bytearray, count: int,
                      total: int, start: int = DATA_START) -> int | None:
    """Find a contiguous run of *count* free sectors starting at >= *start*.
    Returns the first sector number, or None if not found."""
    run_start = start
    run_len = 0
    for s in range(start, total):
        if _bitmap_get(bmap, s):
            run_start = s + 1
            run_len = 0
        else:
            run_len += 1
            if run_len == count:
                return run_start
    return None


# ── Image-level operations ─────────────────────────────────────────────

class MP64FS:
    """In-memory representation of an MP64FS disk image."""

    def __init__(self, data: bytearray | None = None,
                 total_sectors: int = DEFAULT_TOTAL_SECTORS):
        if data is not None:
            self.img = bytearray(data)
        else:
            self.img = bytearray(total_sectors * SECTOR_SIZE)
        self.total = len(self.img) // SECTOR_SIZE

    # ── sector I/O ─────────────────────────────────────────────────

    def _sector(self, n: int) -> memoryview:
        off = n * SECTOR_SIZE
        return memoryview(self.img)[off : off + SECTOR_SIZE]

    # ── superblock ─────────────────────────────────────────────────

    def _write_superblock(self):
        sb = bytearray(SECTOR_SIZE)
        sb[0:4] = MAGIC
        struct.pack_into("<H", sb, 4, FS_VERSION)
        struct.pack_into("<I", sb, 6, self.total)       # u32 total_sectors
        struct.pack_into("<H", sb, 10, BMAP_START)
        struct.pack_into("<H", sb, 12, BMAP_SECTORS)
        struct.pack_into("<H", sb, 14, DIR_START)
        struct.pack_into("<H", sb, 16, DIR_SECTORS)
        struct.pack_into("<H", sb, 18, DATA_START)
        sb[20] = MAX_FILES                               # u8 max_files
        sb[21] = DIR_ENTRY_SIZE                          # u8 entry_size
        self.img[0:SECTOR_SIZE] = sb

    def _verify_magic(self) -> bool:
        return self.img[0:4] == MAGIC

    # ── bitmap ─────────────────────────────────────────────────────

    @property
    def _bmap_offset(self) -> int:
        return BMAP_START * SECTOR_SIZE

    @property
    def _bmap(self) -> bytearray:
        off = self._bmap_offset
        return bytearray(self.img[off : off + SECTOR_SIZE])

    def _flush_bmap(self, bmap: bytearray):
        off = self._bmap_offset
        self.img[off : off + SECTOR_SIZE] = bmap[:SECTOR_SIZE]

    # ── directory ──────────────────────────────────────────────────

    @property
    def _dir_offset(self) -> int:
        return DIR_START * SECTOR_SIZE

    @property
    def _dir_data(self) -> bytearray:
        off = self._dir_offset
        size = DIR_SECTORS * SECTOR_SIZE
        return bytearray(self.img[off : off + size])

    def _flush_dir(self, dirdata: bytearray):
        off = self._dir_offset
        size = DIR_SECTORS * SECTOR_SIZE
        self.img[off : off + size] = dirdata[:size]

    # ── public API ─────────────────────────────────────────────────

    def format(self):
        """Initialise a fresh MP64FS on this image."""
        self.img[:] = b"\x00" * len(self.img)
        self._write_superblock()

        # Mark metadata sectors (0-13) as allocated in bitmap
        bmap = self._bmap
        for s in range(DATA_START):
            _bitmap_set(bmap, s)
        self._flush_bmap(bmap)

    def list_files(self, parent: int = None) -> list[DirEntry]:
        """Return all non-free directory entries.

        If *parent* is given, filter to entries with that parent index.
        """
        if not self._verify_magic():
            return []
        dirdata = self._dir_data
        entries: list[DirEntry] = []
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is not None:
                if parent is not None and e.parent != parent:
                    continue
                entries.append(e)
        return entries

    def find_file(self, name: str, parent: int = PARENT_ROOT
                  ) -> tuple[int, DirEntry] | None:
        """Find file by name within a parent directory.

        Returns (slot_index, entry) or None.
        """
        dirdata = self._dir_data
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is not None and e.name == name and e.parent == parent:
                return (i, e)
        return None

    def _find_first_free_slot(self, dirdata: bytearray) -> int | None:
        """Return the index of the first free directory slot, or None."""
        for i in range(MAX_FILES):
            if dirdata[i * DIR_ENTRY_SIZE] == 0:
                return i
        return None

    def _alloc_sectors(self, bmap: bytearray, nsectors: int
                       ) -> tuple[int, int, int, int]:
        """Allocate up to two extents totalling *nsectors*.

        Returns (start, count, ext1_start, ext1_count).
        Raises RuntimeError if allocation fails.
        """
        # Try single extent first
        start = _bitmap_find_free(bmap, nsectors, self.total)
        if start is not None:
            return (start, nsectors, 0, 0)

        # Fall back to two-extent allocation — find largest free run
        best_start, best_len = 0, 0
        run_start, run_len = DATA_START, 0
        for s in range(DATA_START, self.total):
            if _bitmap_get(bmap, s):
                if run_len > best_len:
                    best_start, best_len = run_start, run_len
                run_start = s + 1
                run_len = 0
            else:
                if run_len == 0:
                    run_start = s
                run_len += 1
        if run_len > best_len:
            best_start, best_len = run_start, run_len

        if best_len == 0:
            raise RuntimeError(f"No space for {nsectors} sectors")

        primary = min(best_len, nsectors)
        remainder = nsectors - primary

        if remainder == 0:
            return (best_start, primary, 0, 0)

        # Mark primary extent temporarily to find second run
        for s in range(best_start, best_start + primary):
            _bitmap_set(bmap, s)
        ext1 = _bitmap_find_free(bmap, remainder, self.total)
        # Unmark (caller will mark properly)
        for s in range(best_start, best_start + primary):
            _bitmap_clear(bmap, s)

        if ext1 is None:
            raise RuntimeError(
                f"No space for {nsectors} sectors (need {remainder} more)")

        return (best_start, primary, ext1, remainder)

    def _write_file_data(self, data: bytes | bytearray,
                         start: int, count: int,
                         ext1_start: int, ext1_count: int):
        """Write file content across up to two extents."""
        primary_bytes = count * SECTOR_SIZE
        # Primary extent
        chunk1 = data[:primary_bytes]
        padded1 = bytearray(chunk1) + b"\x00" * (primary_bytes - len(chunk1))
        off1 = start * SECTOR_SIZE
        self.img[off1 : off1 + primary_bytes] = padded1

        # Secondary extent (if any)
        if ext1_count > 0 and len(data) > primary_bytes:
            ext_bytes = ext1_count * SECTOR_SIZE
            chunk2 = data[primary_bytes:]
            padded2 = bytearray(chunk2) + b"\x00" * (ext_bytes - len(chunk2))
            off2 = ext1_start * SECTOR_SIZE
            self.img[off2 : off2 + ext_bytes] = padded2

    def _read_file_data(self, entry: DirEntry) -> bytes:
        """Read file content across up to two extents."""
        used = entry.used_bytes
        if used == 0:
            return b""
        primary_cap = entry.sector_count * SECTOR_SIZE
        off1 = entry.start_sector * SECTOR_SIZE

        if used <= primary_cap:
            return bytes(self.img[off1 : off1 + used])

        # Spans into secondary extent
        part1 = bytes(self.img[off1 : off1 + primary_cap])
        remain = used - primary_cap
        off2 = entry.ext1_start * SECTOR_SIZE
        part2 = bytes(self.img[off2 : off2 + remain])
        return part1 + part2

    def resolve_path(self, path: str) -> int:
        """Resolve a directory path string to a parent index.

        Returns the directory entry index, or PARENT_ROOT for '/'.
        Raises FileNotFoundError if any component is missing.
        """
        if not path or path == "/":
            return PARENT_ROOT

        parts = [p for p in path.strip("/").split("/") if p]
        current = PARENT_ROOT
        for part in parts:
            result = self.find_file(part, parent=current)
            if result is None or result[1].ftype != FTYPE_DIR:
                raise FileNotFoundError(
                    f"Directory not found: {part!r} in path {path!r}")
            current = result[0]
        return current

    def mkdir(self, path: str) -> DirEntry:
        """Create a directory.  *path* may include parent components.

        E.g., mkdir("/tools/crypto") creates "crypto" inside "tools".
        "tools" must already exist.
        """
        if not self._verify_magic():
            raise RuntimeError("Image not formatted (bad magic)")

        parts = [p for p in path.strip("/").split("/") if p]
        if not parts:
            raise ValueError("Empty directory name")

        dir_name = parts[-1]
        parent_path = "/".join(parts[:-1])
        parent = self.resolve_path(parent_path) if parent_path else PARENT_ROOT

        if len(dir_name) > MAX_NAME_LEN:
            raise ValueError(f"Name too long: {dir_name!r}")
        if self.find_file(dir_name, parent=parent) is not None:
            raise FileExistsError(f"Already exists: {dir_name!r}")

        dirdata = self._dir_data
        slot = self._find_first_free_slot(dirdata)
        if slot is None:
            raise RuntimeError("Directory full (128 entries)")

        entry = DirEntry(
            name=dir_name, start_sector=0, sector_count=0,
            used_bytes=0, ftype=FTYPE_DIR, flags=0,
            parent=parent, mtime=_epoch_seconds(),
        )
        _write_entry(dirdata, slot * DIR_ENTRY_SIZE, entry)
        self._flush_dir(dirdata)
        return entry

    def inject_file(self, name: str, data: bytes | bytearray,
                    ftype: int = FTYPE_RAW, flags: int = 0,
                    path: str = "/") -> DirEntry:
        """Write a named file into the image.  Allocates sectors.

        *path* specifies the parent directory (default: root).
        """
        if not self._verify_magic():
            raise RuntimeError("Image not formatted (bad magic)")
        if len(name) > MAX_NAME_LEN:
            raise ValueError(f"Name too long: {name!r} (max {MAX_NAME_LEN})")

        parent = self.resolve_path(path)

        if self.find_file(name, parent=parent) is not None:
            raise FileExistsError(f"File already exists: {name!r}")

        nsectors = _sectors_needed(len(data)) if data else 1
        bmap = self._bmap
        start, count, ext1_start, ext1_count = self._alloc_sectors(
            bmap, nsectors)

        # Mark sectors allocated
        for s in range(start, start + count):
            _bitmap_set(bmap, s)
        for s in range(ext1_start, ext1_start + ext1_count):
            _bitmap_set(bmap, s)
        self._flush_bmap(bmap)

        # Write data across extents
        self._write_file_data(data, start, count, ext1_start, ext1_count)

        # CRC32 of content
        crc = _compute_crc32(data) if data else 0

        # Create directory entry
        dirdata = self._dir_data
        slot = self._find_first_free_slot(dirdata)
        if slot is None:
            raise RuntimeError("Directory full (128 entries)")

        entry = DirEntry(
            name=name, start_sector=start, sector_count=count,
            used_bytes=len(data), ftype=ftype, flags=flags,
            parent=parent, mtime=_epoch_seconds(),
            data_crc32=crc, ext1_start=ext1_start, ext1_count=ext1_count,
        )
        _write_entry(dirdata, slot * DIR_ENTRY_SIZE, entry)
        self._flush_dir(dirdata)
        return entry

    def read_file(self, name: str, parent: int = PARENT_ROOT) -> bytes:
        """Read file contents by name within *parent* directory."""
        result = self.find_file(name, parent=parent)
        if result is None:
            raise FileNotFoundError(f"File not found: {name!r}")
        _, entry = result
        return self._read_file_data(entry)

    def delete_file(self, name: str, parent: int = PARENT_ROOT):
        """Delete a file: free its sectors and directory entry."""
        result = self.find_file(name, parent=parent)
        if result is None:
            raise FileNotFoundError(f"File not found: {name!r}")
        slot, entry = result

        # For directories, check that they're empty
        if entry.ftype == FTYPE_DIR:
            children = self.list_files(parent=slot)
            if children:
                raise RuntimeError(
                    f"Directory not empty: {name!r} ({len(children)} entries)")

        # Free sectors in bitmap (both extents)
        bmap = self._bmap
        for s in range(entry.start_sector,
                       entry.start_sector + entry.sector_count):
            _bitmap_clear(bmap, s)
        if entry.ext1_count > 0:
            for s in range(entry.ext1_start,
                           entry.ext1_start + entry.ext1_count):
                _bitmap_clear(bmap, s)
        self._flush_bmap(bmap)

        # Clear directory slot
        dirdata = self._dir_data
        _clear_entry(dirdata, slot * DIR_ENTRY_SIZE)
        self._flush_dir(dirdata)

    def mkstream(self, name: str, sectors: int = 4,
                 parent: int = PARENT_ROOT) -> DirEntry:
        """Create a stream file (circular ring buffer)."""
        return self.inject_file(
            name, b"\x00" * (sectors * SECTOR_SIZE),
            ftype=FTYPE_STREAM,
            flags=FLAG_APPEND,
            path="/" if parent == PARENT_ROOT else self._parent_to_path(parent),
        )

    def mklink(self, name: str, target: str,
               parent: int = PARENT_ROOT) -> DirEntry:
        """Create a symbolic link pointing to *target*."""
        target_bytes = target.encode("ascii") + b"\x00"
        return self.inject_file(
            name, target_bytes, ftype=FTYPE_LINK, flags=0,
            path="/" if parent == PARENT_ROOT else self._parent_to_path(parent),
        )

    def check(self) -> list[str]:
        """Verify CRC32 of all files.  Returns list of error messages."""
        errors = []
        dirdata = self._dir_data
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is None or e.ftype in (FTYPE_FREE, FTYPE_DIR):
                continue
            data = self._read_file_data(e)
            computed = _compute_crc32(data)
            if computed != e.data_crc32:
                errors.append(
                    f"Entry {i} {e.name!r} CRC MISMATCH: "
                    f"stored=0x{e.data_crc32:08X} computed=0x{computed:08X}")
        return errors

    def compact(self) -> int:
        """Defragment: pack all files into contiguous single extents.
        Returns the number of files moved."""
        if not self._verify_magic():
            raise RuntimeError("Image not formatted")

        dirdata = self._dir_data

        # Collect live non-directory entries with their slot indices
        live: list[tuple[int, DirEntry, bytes]] = []
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is None or e.ftype in (FTYPE_FREE, FTYPE_DIR):
                continue
            data = self._read_file_data(e)
            live.append((i, e, data))

        # Sort by primary start sector
        live.sort(key=lambda t: t[1].start_sector)

        # Rebuild bitmap — clear all data sectors
        bmap = self._bmap
        for s in range(DATA_START, self.total):
            _bitmap_clear(bmap, s)

        # Re-pack each file into a single contiguous run
        moved = 0
        next_sector = DATA_START
        for slot_idx, entry, data in live:
            nsectors = _sectors_needed(len(data)) if data else 1
            start = next_sector
            if start != entry.start_sector or entry.ext1_count > 0:
                moved += 1
            # Mark allocated
            for s in range(start, start + nsectors):
                _bitmap_set(bmap, s)
            # Write data (single extent)
            padded = bytearray(data) + b"\x00" * (
                nsectors * SECTOR_SIZE - len(data))
            off = start * SECTOR_SIZE
            self.img[off : off + nsectors * SECTOR_SIZE] = padded
            # Update entry
            entry.start_sector = start
            entry.sector_count = nsectors
            entry.ext1_start = 0
            entry.ext1_count = 0
            entry.data_crc32 = _compute_crc32(data)
            _write_entry(dirdata, slot_idx * DIR_ENTRY_SIZE, entry)
            next_sector = start + nsectors

        self._flush_bmap(bmap)
        self._flush_dir(dirdata)
        return moved

    def _parent_to_path(self, parent_idx: int) -> str:
        """Convert a parent index to a path string (for internal use)."""
        if parent_idx == PARENT_ROOT:
            return "/"
        parts = []
        idx = parent_idx
        dirdata = self._dir_data
        while idx != PARENT_ROOT:
            e = _read_entry(dirdata, idx * DIR_ENTRY_SIZE)
            if e is None:
                break
            parts.append(e.name)
            idx = e.parent
        return "/" + "/".join(reversed(parts))

    def info(self) -> dict:
        """Return superblock metadata."""
        if not self._verify_magic():
            return {"formatted": False}
        return {
            "formatted": True,
            "version": struct.unpack_from("<H", self.img, 4)[0],
            "total_sectors": struct.unpack_from("<I", self.img, 6)[0],
            "data_start": struct.unpack_from("<H", self.img, 18)[0],
            "max_files": self.img[20],
            "entry_size": self.img[21],
            "files": len(self.list_files()),
            "free_sectors": self._count_free(),
        }

    def _count_free(self) -> int:
        bmap = self._bmap
        free = 0
        for s in range(DATA_START, self.total):
            if not _bitmap_get(bmap, s):
                free += 1
        return free

    # ── serialisation ──────────────────────────────────────────────

    def save(self, path: str | Path):
        """Write the image to a file."""
        Path(path).write_bytes(self.img)

    @classmethod
    def load(cls, path: str | Path) -> "MP64FS":
        """Load an image from file."""
        data = bytearray(Path(path).read_bytes())
        return cls(data)


# ── Convenience functions ──────────────────────────────────────────────

def format_image(path: str | Path, total_sectors: int = DEFAULT_TOTAL_SECTORS):
    """Create and format a new MP64FS disk image."""
    fs = MP64FS(total_sectors=total_sectors)
    fs.format()
    fs.save(path)
    return fs


def inject_file(path: str | Path, name: str, data: bytes | bytearray,
                ftype: int = FTYPE_RAW, flags: int = 0,
                fs_path: str = "/") -> DirEntry:
    """Open an existing image and inject a named file."""
    fs = MP64FS.load(path)
    entry = fs.inject_file(name, data, ftype, flags, path=fs_path)
    fs.save(path)
    return entry


def read_file(path: str | Path, name: str, parent: int = PARENT_ROOT) -> bytes:
    """Open an existing image and read a named file."""
    return MP64FS.load(path).read_file(name, parent=parent)


def list_files(path: str | Path, parent: int | None = None) -> list[DirEntry]:
    """Open an existing image and list all files."""
    return MP64FS.load(path).list_files(parent=parent)


def delete_file(path: str | Path, name: str, parent: int = PARENT_ROOT):
    """Open an existing image and delete a named file."""
    fs = MP64FS.load(path)
    fs.delete_file(name, parent=parent)
    fs.save(path)


# ── Documentation content ──────────────────────────────────────────────

DOCS = {
    "getting-started": """\
GETTING STARTED WITH KDOS
=========================

KDOS is the Kernel Dashboard OS for Megapad-64.
It boots automatically from disk -- the BIOS finds
the first Forth-type file and loads it via FSLOAD.
No configuration needed.

FIRST STEPS
  Type HELP for a command reference.
  Type DESCRIBE <word> for detailed word help.
  Type STATUS for a quick system overview.
  Type DASHBOARD for a full system report.
  Type SCREENS for the 9-screen interactive TUI.

CREATING A BUFFER
  0 1 256 BUFFER mydata
  42 mydata B.FILL
  mydata B.PREVIEW

RUNNING A KERNEL
  mydata kzero           Zero the buffer
  99 mydata kfill        Fill with 99
  mydata ksum .          Sum all bytes

STORAGE
  If a disk is attached, KDOS loads MP64FS
  automatically. Use DIR to list files,
  MKFILE to create, and OPEN to access them.

ERROR HANDLING
  The BIOS detects stack underflow and prints
  a warning. Undefined words show the word name
  and context. During FSLOAD, errors include the
  file line number for easy debugging.
""",

    "buffers": """\
BUFFERS
=======

Buffers are typed, tile-aligned data regions.
Each buffer has a type, element width, and length.

CREATING BUFFERS
  0 1 256 BUFFER mydata    raw, 1-byte, 256 elts
  1 2 128 BUFFER rec       record, 2-byte, 128
  2 4  64 BUFFER tiles     tile, 4-byte, 64

BUFFER OPERATIONS
  byte buf B.FILL          Fill with byte value
  buf B.ZERO               Zero entire buffer
  buf B.PREVIEW            Hex dump first tile
  buf B.INFO               Show descriptor

TILE-ENGINE REDUCTIONS (SIMD)
  buf B.SUM                Sum all elements
  buf B.MIN                Minimum element
  buf B.MAX                Maximum element

ELEMENT-WISE OPS
  a b c B.ADD              c = a + b
  a b c B.SUB              c = a - b
  n buf B.SCALE            Multiply each by n

DISK PERSISTENCE
  buf sec B.SAVE           Save buffer to sector
  buf sec B.LOAD           Load buffer from sector

Press 1 in SCREENS to see all buffers listed
with type, width, length, and data address.
""",

    "kernels": """\
KERNELS
=======

Kernels are registered compute operations with
metadata describing inputs, outputs, and footprint.

REGISTRATION
  1 1 2 0 KERNEL myk    1 in, 1 out, 2 foot, CPU
  desc K.INFO            Show kernel descriptor
  KERNELS                List all registered

SAMPLE KERNELS
  buf kzero              Zero a buffer
  byte buf kfill         Fill with byte
  a b c kadd             Add two buffers -> c
  buf ksum               Sum -> stack
  buf kstats             Sum, min, max -> stack
  n buf kscale           Scale by n
  n buf kthresh          Threshold filter

ADVANCED KERNELS
  lo hi buf kclamp       Clamp to [lo,hi]
  w buf kavg             Moving average
  buf khistogram         256-bin histogram
  buf knorm              Normalize to 0-255
  th src dst kpeak       Peak detection
  buf krms-buf           RMS value
  a b kcorrelate         Dot product
  k src dst kconvolve    Convolution
""",

    "pipelines": """\
PIPELINES
=========

Pipelines chain multiple kernel steps into a
single executable sequence.

CREATING A PIPELINE
  3 PIPELINE mypipe      3-step capacity

BUILDING
  ' kzero mypipe P.ADD   Append step
  ' kfill mypipe P.ADD   Append another
  mypipe P.INFO          Show pipeline info

RUNNING
  mypipe P.RUN           Execute all steps
  mypipe P.BENCH         Time each step

MANAGEMENT
  mypipe P.CLEAR         Reset pipeline
  PIPES                  List all pipelines

Press 3 in SCREENS to see all pipelines listed
with their step count and execution status.
""",

    "storage": """\
STORAGE & MP64FS
================

KDOS supports persistent storage via the MP64FS
file system on a 1 MiB virtual disk.

DISK BASICS
  DISK?                  Check if disk is attached
  DISK-INFO              Print storage status

MP64FS FILE SYSTEM
  FORMAT                 Format disk with MP64FS
  FS-LOAD                Load FS from disk
  FS-SYNC                Write changes back
  DIR                    List all files
  CATALOG                Detailed listing

FILE OPERATIONS
  8 2 MKFILE readme      Create: 8 secs, type text
  RMFILE readme           Delete file
  OPEN readme             Open -> file descriptor
  RENAME old new          Rename a file

FILE I/O
  addr len fd FWRITE     Write bytes
  addr len fd FREAD      Read bytes -> actual
  pos fd FSEEK           Set cursor position
  fd FREWIND             Reset cursor to 0
  fd FFLUSH              Flush metadata to disk

STORAGE SCREEN
  Press 7 in SCREENS to open the Storage screen.
  It shows a DIR listing and disk usage info.

DISK LAYOUT
  Sector 0       Superblock (magic MP64)
  Sector 1       Allocation bitmap
  Sectors 2-13   Directory (128 entries, 48 B each)
  Sectors 14+    Data area (~1 MB usable)

SUBDIRECTORIES
  MKDIR subdir             Create a subdirectory
  CD subdir                Change into it
  CD ..                    Go up one level
  PWD                      Print current path
""",

    "scheduler": """\
SCHEDULER & TASKS
=================

KDOS provides cooperative multitasking with
optional timer-assisted preemption.

CREATING TASKS
  ' myword 0 TASK mytask   Create (xt, priority)
  xt SPAWN                  Spawn anonymous task
  xt BG                     Spawn + schedule

RUNNING
  SCHEDULE                  Run all ready tasks
  YIELD                     Cooperative yield point

MANAGEMENT
  tdesc KILL                Cancel a task
  tdesc RESTART             Reset done -> ready
  TASKS                     List all tasks

PREEMPTION
  PREEMPT-ON                Enable timer preemption
  PREEMPT-OFF               Disable preemption

The scheduler round-robins among READY tasks,
respecting priority levels.

Press 4 in SCREENS to see the Task screen where
you can view, resume, or kill tasks interactively.
""",

    "screens": """\
INTERACTIVE TUI (SCREENS)
=========================

Type SCREENS to enter the interactive terminal UI.

SCREEN LIST
  [0] Home     System overview & status
  [1] Bufs     Buffer listing with details
  [2] Kern     Kernel registry & metadata
  [3] Pipe     Pipeline listing & steps
  [4] Task     Task listing (resume/kill)
  [5] Help     Full command reference card
  [6] Docs     Documentation browser
  [7] Stor     File browser & disk info
  [8] Core     Multicore status & dispatch

CONTROLS
  0-9, a-f       Switch to screen
  [/]            Next/prev subscreen
  r              Refresh current screen
  A              Toggle auto-refresh
  q              Quit back to REPL

AUTO-REFRESH
  Screens auto-refresh periodically so you see
  live system state without pressing 'r'.

Each screen shows formatted columns, color-coded
headers, and a footer with available key bindings.
""",

    "data-ports": """\
DATA PORTS
==========

Data ports bind NIC network sources to buffers
for external data ingestion.

BINDING
  buf id PORT!             Bind source id to buffer
  id UNPORT                Unbind source

RECEIVING DATA
  POLL                     Receive & route one frame
  n INGEST                 Receive n frames
  NET-RX?                  Is a frame waiting?

MONITORING
  PORTS                    List port bindings
  .FRAME                   Show last frame header

FRAME PROTOCOL (6-byte header + payload)
  +0  u8  SRC_ID           Source identifier
  +1  u8  DTYPE            Data type
  +2  u16 SEQ              Sequence number
  +4  u16 LEN              Payload length

See also: the NIC device words in the BIOS
  NET-STATUS NET-SEND NET-RECV NET-MAC@
""",

    "tile-engine": """\
TILE ENGINE
===========

The Megapad-64 tile engine is a SIMD compute
surface with 64-byte tiles.

TILE PROPERTIES
  64 bytes per tile
  1/2/4/8 byte element widths
  Hardware ALU, multiply, reduction
  256-bit accumulator for multi-tile ops

TILE OPERATIONS (via buffers)
  buf B.SUM     Sum reduction (tile engine)
  buf B.MIN     Min reduction
  buf B.MAX     Max reduction
  a b c B.ADD   Element-wise addition
  a b c B.SUB   Element-wise subtraction
  n buf B.SCALE Scalar multiply

BIOS TILE WORDS
  TADD TSUB TAND TOR TXOR TMIN TMAX TABS
  TMUL TDOT
  TSUM TEMIN TEMAX TPOPCNT TL1
  TTRANS TZERO TLOADC TMOVBANK

The tile engine operates on tile-aligned data
and provides hardware-accelerated SIMD operations
for buffer processing.

MEX CSR INTERFACE
  Operations via memory-mapped control registers
  Source selection: register, immediate, memory
  See docs/tile-engine.md for full MEX encoding
""",

    "reference": """\
KDOS QUICK REFERENCE
====================

STACK NOTATION
  ( before -- after )    Stack effect
  n, u, addr, flag      Common types

ARITHMETIC
  + - * / MOD            Basic math
  ABS NEGATE MIN MAX     Utilities
  2* CELLS CELL+         Scaling

LOGIC
  AND OR XOR INVERT      Bitwise ops
  = <> < > 0= 0> >= <=  Comparisons

MEMORY
  @ ! C@ C! W@ W! L@ L!  Fetch/store
  +! CMOVE FILL           Modify

CONTROL
  IF ELSE THEN            Conditional
  DO LOOP +LOOP I J       Counted loop
  BEGIN UNTIL WHILE REPEAT Indefinite loop
  EXIT                     Return from word

DEFINING
  : name ... ;            Define word
  VARIABLE name           Create variable
  CONSTANT name           Create constant
  CREATE name             Create entry

HELP & DISCOVERY
  HELP                    Full command listing
  HELP <word>             Help for a specific word
  DESCRIBE <word>         Detailed word help
  WORDS-LIKE <text>       Find matching words
  APROPOS <text>          Search by topic
  TOPICS                  List documentation
  LESSONS                 List tutorials

ERROR HANDLING
  Stack underflow is detected and reported.
  Undefined words show name + context.
  FSLOAD errors include file line numbers.
""",
}

TUTORIALS = {
    "hello-world": """\
TUTORIAL: HELLO WORLD
=====================

Welcome to your first KDOS tutorial!

STEP 1: Print a message
  Type: ." Hello, Megapad!" CR

STEP 2: Define a word
  Type: : greet ." Hello from KDOS!" CR ;
  Then: greet

STEP 3: Use the stack
  Type: 10 20 + .
  Result: 30

STEP 4: Create a variable
  Type: VARIABLE myvar
  Type: 42 myvar !
  Type: myvar @ .
  Result: 42

STEP 5: Create a buffer
  Type: 0 1 64 BUFFER mybuf
  Type: 65 mybuf B.FILL
  Type: mybuf B.PREVIEW

STEP 6: Get help
  Type: DESCRIBE DUP
  Shows the stack effect and description.
  Type: HELP
  Shows the full command reference.

Congratulations! You've learned the basics.
Type TOPICS to explore more documentation.
""",

    "first-kernel": """\
TUTORIAL: YOUR FIRST KERNEL
============================

Kernels are registered compute operations.

STEP 1: Create input and output buffers
  0 1 64 BUFFER inbuf
  0 1 64 BUFFER outbuf

STEP 2: Fill input with test data
  42 inbuf B.FILL
  inbuf B.PREVIEW

STEP 3: Register a kernel
  1 1 2 0 KERNEL mykern

STEP 4: Use built-in kernels
  inbuf kzero              Zero the buffer
  99 inbuf kfill           Fill with 99
  inbuf ksum .             Sum all bytes

STEP 5: Check system state
  KERNELS                  List all kernels
  BUFFERS                  List all buffers

Kernels track metadata about their inputs,
outputs, and resource footprint.

TIP: Use DESCRIBE ksum to see help for any
kernel word. Press 2 in SCREENS to browse
all registered kernels.
""",

    "build-pipeline": """\
TUTORIAL: BUILD A PIPELINE
==========================

Pipelines chain kernel steps together.

STEP 1: Create buffers
  0 1 64 BUFFER src
  0 1 64 BUFFER dst

STEP 2: Fill source data
  42 src B.FILL

STEP 3: Create a pipeline
  3 PIPELINE mypipe

STEP 4: Add steps
  ' kzero mypipe P.ADD
  ' kfill mypipe P.ADD

STEP 5: Run the pipeline
  mypipe P.RUN
  mypipe P.INFO

STEP 6: Benchmark it
  mypipe P.BENCH

Pipelines make complex workflows repeatable
and benchmarkable.

TIP: Press 3 in SCREENS to see your pipeline.
Use DESCRIBE P.RUN for detailed word help.
""",

    "data-ingest": """\
TUTORIAL: DATA INGESTION
========================

Data ports connect NIC sources to buffers.

STEP 1: Create a receive buffer
  0 1 256 BUFFER rxbuf

STEP 2: Bind a port
  rxbuf 1 PORT!

STEP 3: Check for data
  NET-RX?
  If frames are available:
  POLL
  Or receive multiple:
  10 INGEST

STEP 4: Inspect received data
  rxbuf B.PREVIEW
  PORTS

Data flows from the network interface through
the port binding system into your buffers.

TIP: Use DESCRIBE PORT! for detailed help.
The Home screen (1 in SCREENS) shows port count.
""",

    "custom-kernel": """\
TUTORIAL: CUSTOM KERNEL
=======================

Create your own compute kernel.

STEP 1: Define the operation
  : my-double ( buf -- )
    DUP B.DATA SWAP B.LEN
    OVER + SWAP DO
      I C@ 2* I C!
    LOOP ;

STEP 2: Create test data
  0 1 64 BUFFER testbuf
  3 testbuf B.FILL
  testbuf B.PREVIEW

STEP 3: Run your kernel
  testbuf my-double
  testbuf B.PREVIEW

STEP 4: Register as a kernel
  1 1 1 0 KERNEL my-double-k

STEP 5: Benchmark it
  ' my-double .BENCH

Custom kernels let you extend KDOS with
your own tile-engine operations.

TIP: Use DESCRIBE KERNEL to learn about the
KERNEL registration word's stack effect.
""",
}


# ── Doc / Tutorial builder ─────────────────────────────────────────────

def build_docs(fs: MP64FS):
    """Inject all documentation files into *fs*."""
    for name, content in DOCS.items():
        fs.inject_file(name, content.encode("ascii"), ftype=FTYPE_DOC)


def build_tutorials(fs: MP64FS):
    """Inject all tutorial files into *fs*."""
    for name, content in TUTORIALS.items():
        fs.inject_file(name, content.encode("ascii"), ftype=FTYPE_TUT)


def build_image(path: str | Path | None = None,
                total_sectors: int = DEFAULT_TOTAL_SECTORS) -> MP64FS:
    """Create a fully-populated image with docs and tutorials."""
    fs = MP64FS(total_sectors=total_sectors)
    fs.format()
    build_docs(fs)
    build_tutorials(fs)
    if path is not None:
        fs.save(path)
    return fs


def build_sample_image(path: str | Path | None = None,
                       kdos_path: str | Path = "kdos.f",
                       total_sectors: int = DEFAULT_TOTAL_SECTORS) -> MP64FS:
    """Create a complete sample image with KDOS, docs, tutorials & demo bundle.

    This is the "ship it" image: booting with this disk + BIOS will
    auto-load KDOS -- the BIOS finds the first Forth-type file and
    loads it via FSLOAD.
    """
    fs = build_image(total_sectors=total_sectors)

    # Inject KDOS source -- the BIOS auto-boots this via FSLOAD kdos.f
    kdos_src = Path(kdos_path).read_bytes()
    fs.inject_file("kdos.f", kdos_src, ftype=FTYPE_FORTH, flags=0x02)  # system

    # Inject sample data for tutorials
    demo = bytes(range(256))
    fs.inject_file("demo-data", demo, ftype=FTYPE_DATA)

    # Inject demo pipeline bundle (type 7)
    demo_bundle = (
        "\\ PBDL v1 -- Demo Pipeline Bundle\n"
        "\\ Demonstrates the declarative pipeline bundle format.\n"
        "\\ Load with:  BUNDLE-LOAD demo-bundle\n"
        "\\ Inspect:    BUNDLE-INFO demo-bundle\n"
        "\n"
        "1 BDL-BEGIN\n"
        "\n"
        "\\ --- Buffer Schemas ---\n"
        "0 1 128 BDL-BUF bdl-sensor\n"
        "0 1 128 BDL-BUF bdl-result\n"
        "0 8 256 BDL-BUF bdl-hist\n"
        "\n"
        "\\ --- Kernel Registrations ---\n"
        "1 1 2 1 BDL-KERN bdl-knorm\n"
        "1 1 4 0 BDL-KERN bdl-khist\n"
        "\n"
        "\\ --- Pipeline ---\n"
        "3 BDL-PIPE bdl-main\n"
        "\n"
        "\\ --- Schedule ---\n"
        "0 50000 3 BDL-SCHED\n"
        "\n"
        "\\ --- Policy ---\n"
        "0 0 3 BDL-POLICY\n"
        "\n"
        "\\ --- Dashboard ---\n"
        "1 511 BDL-SCREEN\n"
        "\n"
        "BDL-END\n"
    ).encode("ascii")
    fs.inject_file("demo-bundle", demo_bundle, ftype=FTYPE_BUNDLE)

    # Inject graphics module (Forth, loadable via LOAD graphics.f)
    gfx_path = Path(__file__).parent / "graphics.f"
    if gfx_path.exists():
        gfx_src = gfx_path.read_bytes()
        fs.inject_file("graphics.f", gfx_src, ftype=FTYPE_FORTH)

    # Inject tools module (Forth, loadable via LOAD tools.f)
    tools_path = Path(__file__).parent / "tools.f"
    if tools_path.exists():
        tools_src = tools_path.read_bytes()
        fs.inject_file("tools.f", tools_src, ftype=FTYPE_FORTH)

    # Inject autoexec boot script (runs automatically at startup)
    autoexec_path = Path(__file__).parent / "autoexec.f"
    if autoexec_path.exists():
        autoexec_src = autoexec_path.read_bytes()
        fs.inject_file("autoexec.f", autoexec_src, ftype=FTYPE_FORTH)

    if path is not None:
        fs.save(path)
    return fs


# ── CLI ────────────────────────────────────────────────────────────────

def main():
    import argparse

    parser = argparse.ArgumentParser(
        prog="diskutil",
        description="Megapad-64 MP64FS disk image utility",
    )
    sub = parser.add_subparsers(dest="cmd")

    # sample — build a ready-to-boot image with KDOS + docs + tutorials
    p_sample = sub.add_parser("sample", help="Build sample image with KDOS")
    p_sample.add_argument("-o", "--output", default="sample.img",
                          help="Output path (default: sample.img)")
    p_sample.add_argument("--kdos", default="kdos.f",
                          help="KDOS source file (default: kdos.f)")

    # format — create a blank formatted image
    p_fmt = sub.add_parser("format", help="Create a blank formatted image")
    p_fmt.add_argument("-o", "--output", default="disk.img",
                       help="Output path (default: disk.img)")
    p_fmt.add_argument("--sectors", type=int, default=DEFAULT_TOTAL_SECTORS,
                       help=f"Total sectors (default: {DEFAULT_TOTAL_SECTORS})")

    # ls — list files in an image
    p_ls = sub.add_parser("ls", help="List files in an image")
    p_ls.add_argument("image", help="Disk image path")

    # inject — add a file to an image
    p_inj = sub.add_parser("inject", help="Inject a file into an image")
    p_inj.add_argument("image", help="Disk image path")
    p_inj.add_argument("file", help="File to inject")
    p_inj.add_argument("-n", "--name", default=None,
                       help="Name in image (default: basename of file)")
    p_inj.add_argument("-t", "--type", default="raw",
                       choices=list(FTYPE_NAMES.values()),
                       help="File type (default: raw)")
    p_inj.add_argument("-p", "--path", default="/",
                       help="Directory path (default: /)")

    # cat — read a file from an image
    p_cat = sub.add_parser("cat", help="Read a file from an image")
    p_cat.add_argument("image", help="Disk image path")
    p_cat.add_argument("name", help="File name to read")

    # rm — delete a file from an image
    p_rm = sub.add_parser("rm", help="Delete a file from an image")
    p_rm.add_argument("image", help="Disk image path")
    p_rm.add_argument("name", help="File name to delete")
    p_rm.add_argument("-p", "--path", default="/",
                       help="Directory path (default: /)")

    # mkdir — create a subdirectory
    p_mkdir = sub.add_parser("mkdir", help="Create a subdirectory")
    p_mkdir.add_argument("image", help="Disk image path")
    p_mkdir.add_argument("name", help="Directory name")
    p_mkdir.add_argument("-p", "--path", default="/",
                          help="Parent path (default: /)")

    # check — verify CRC integrity of all files
    p_chk = sub.add_parser("check", help="Verify CRC integrity")
    p_chk.add_argument("image", help="Disk image path")

    # compact — defragment the disk
    p_compact = sub.add_parser("compact", help="Defragment disk")
    p_compact.add_argument("image", help="Disk image path")

    # info — show superblock details
    p_info = sub.add_parser("info", help="Show superblock info")
    p_info.add_argument("image", help="Disk image path")

    args = parser.parse_args()

    if args.cmd is None:
        parser.print_help()
        return

    if args.cmd == "sample":
        build_sample_image(args.output, kdos_path=args.kdos)
        fs = MP64FS.load(args.output)
        files = fs.list_files()
        total_bytes = sum(e.used_bytes for e in files)
        print(f"Created {args.output} ({os.path.getsize(args.output)} bytes, "
              f"{len(files)} files, {total_bytes} bytes used)")

    elif args.cmd == "format":
        format_image(args.output, total_sectors=args.sectors)
        print(f"Formatted {args.output} ({args.sectors} sectors)")

    elif args.cmd == "ls":
        entries = list_files(args.image)
        if not entries:
            print("(empty)")
            return
        print(f"{'Name':<24} {'Type':<8} {'Size':>8}  {'Sectors':>7}  {'Parent':>6}  Flags")
        print("-" * 72)
        for e in entries:
            tname = FTYPE_NAMES.get(e.ftype, f"?{e.ftype}")
            par = "root" if e.parent == PARENT_ROOT else str(e.parent)
            print(f"{e.name:<24} {tname:<8} {e.used_bytes:>8}  {e.sector_count:>7}  "
                  f"{par:>6}  0x{e.flags:02x}")

    elif args.cmd == "inject":
        name = args.name or os.path.basename(args.file)
        ftype_rev = {v: k for k, v in FTYPE_NAMES.items()}
        ftype = ftype_rev[args.type]
        with open(args.file, "rb") as f:
            data = f.read()
        fs_path = getattr(args, "path", "/")
        inject_file(args.image, name, data, ftype=ftype, fs_path=fs_path)
        print(f"Injected '{name}' ({len(data)} bytes, type={args.type})")

    elif args.cmd == "cat":
        data = read_file(args.image, args.name)
        sys.stdout.buffer.write(data)

    elif args.cmd == "rm":
        fs = MP64FS.load(args.image)
        parent = fs.resolve_path(args.path)
        fs.delete_file(args.name, parent=parent)
        fs.save(args.image)
        print(f"Deleted '{args.name}'")

    elif args.cmd == "mkdir":
        fs = MP64FS.load(args.image)
        parent = fs.resolve_path(args.path)
        fs.mkdir(args.name, parent=parent)
        fs.save(args.image)
        print(f"Created directory '{args.name}'")

    elif args.cmd == "check":
        fs = MP64FS.load(args.image)
        errors = fs.check()
        if errors:
            for err in errors:
                print(f"  ERROR: {err}")
            print(f"{len(errors)} error(s) found")
        else:
            print("All files OK")

    elif args.cmd == "compact":
        fs = MP64FS.load(args.image)
        moved = fs.compact()
        fs.save(args.image)
        print(f"Compacted: {moved} file(s) moved")

    elif args.cmd == "info":
        fs = MP64FS.load(args.image)
        info = fs.info()
        for k, v in info.items():
            print(f"  {k}: {v}")


if __name__ == "__main__":
    main()
