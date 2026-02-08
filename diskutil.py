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

import struct
from pathlib import Path
from dataclasses import dataclass

# ── Constants ──────────────────────────────────────────────────────────

SECTOR_SIZE = 512
DEFAULT_TOTAL_SECTORS = 2048        # 1 MiB
MAGIC = b"MP64"
FS_VERSION = 1

BMAP_START = 1
BMAP_SECTORS = 1
DIR_START = 2
DIR_SECTORS = 4
DATA_START = 6

DIR_ENTRY_SIZE = 32
MAX_NAME_LEN = 15
ENTRIES_PER_SECTOR = SECTOR_SIZE // DIR_ENTRY_SIZE   # 16
MAX_FILES = DIR_SECTORS * ENTRIES_PER_SECTOR          # 64

# File types
FTYPE_FREE  = 0
FTYPE_RAW   = 1
FTYPE_TEXT  = 2
FTYPE_FORTH = 3
FTYPE_DOC   = 4
FTYPE_DATA  = 5

FTYPE_NAMES = {
    FTYPE_FREE: "free", FTYPE_RAW: "raw", FTYPE_TEXT: "text",
    FTYPE_FORTH: "forth", FTYPE_DOC: "doc", FTYPE_DATA: "data",
}


# ── Data classes ───────────────────────────────────────────────────────

@dataclass
class DirEntry:
    """One directory entry."""
    name: str
    start_sector: int
    sector_count: int
    used_bytes: int
    ftype: int
    flags: int

    @property
    def type_name(self) -> str:
        return FTYPE_NAMES.get(self.ftype, f"?{self.ftype}")

    @property
    def readonly(self) -> bool:
        return bool(self.flags & 1)

    @property
    def system(self) -> bool:
        return bool(self.flags & 2)


# ── Low-level helpers ──────────────────────────────────────────────────

def _sectors_needed(nbytes: int) -> int:
    """Number of 512-byte sectors needed to hold *nbytes*."""
    return (nbytes + SECTOR_SIZE - 1) // SECTOR_SIZE


def _read_entry(data: bytes, offset: int) -> DirEntry | None:
    """Parse a 32-byte directory entry.  Returns None if slot is free."""
    raw = data[offset : offset + DIR_ENTRY_SIZE]
    if raw[0] == 0:
        return None
    name = raw[0:16].split(b"\x00", 1)[0].decode("ascii", errors="replace")
    start = struct.unpack_from("<H", raw, 16)[0]
    count = struct.unpack_from("<H", raw, 18)[0]
    used  = struct.unpack_from("<I", raw, 20)[0]
    ftype = raw[24]
    flags = raw[25]
    return DirEntry(name, start, count, used, ftype, flags)


def _write_entry(buf: bytearray, offset: int, entry: DirEntry):
    """Serialise a DirEntry into 32 bytes at *offset* in *buf*."""
    # Zero the slot first
    buf[offset : offset + DIR_ENTRY_SIZE] = b"\x00" * DIR_ENTRY_SIZE
    name_bytes = entry.name.encode("ascii")[:MAX_NAME_LEN]
    buf[offset : offset + len(name_bytes)] = name_bytes
    struct.pack_into("<H", buf, offset + 16, entry.start_sector)
    struct.pack_into("<H", buf, offset + 18, entry.sector_count)
    struct.pack_into("<I", buf, offset + 20, entry.used_bytes)
    buf[offset + 24] = entry.ftype
    buf[offset + 25] = entry.flags


def _clear_entry(buf: bytearray, offset: int):
    """Zero a 32-byte directory slot."""
    buf[offset : offset + DIR_ENTRY_SIZE] = b"\x00" * DIR_ENTRY_SIZE


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
        struct.pack_into("<H", sb, 6, self.total)
        struct.pack_into("<H", sb, 8, BMAP_START)
        struct.pack_into("<H", sb, 10, BMAP_SECTORS)
        struct.pack_into("<H", sb, 12, DIR_START)
        struct.pack_into("<H", sb, 14, DIR_SECTORS)
        struct.pack_into("<H", sb, 16, DATA_START)
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

        # Mark metadata sectors (0-5) as allocated in bitmap
        bmap = self._bmap
        for s in range(DATA_START):
            _bitmap_set(bmap, s)
        self._flush_bmap(bmap)

    def list_files(self) -> list[DirEntry]:
        """Return all non-free directory entries."""
        if not self._verify_magic():
            return []
        dirdata = self._dir_data
        entries: list[DirEntry] = []
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is not None:
                entries.append(e)
        return entries

    def find_file(self, name: str) -> tuple[int, DirEntry] | None:
        """Find file by name.  Returns (slot_index, entry) or None."""
        dirdata = self._dir_data
        for i in range(MAX_FILES):
            e = _read_entry(dirdata, i * DIR_ENTRY_SIZE)
            if e is not None and e.name == name:
                return (i, e)
        return None

    def inject_file(self, name: str, data: bytes | bytearray,
                    ftype: int = FTYPE_RAW,
                    flags: int = 0) -> DirEntry:
        """Write a named file into the image.  Allocates sectors."""
        if not self._verify_magic():
            raise RuntimeError("Image not formatted (bad magic)")
        if len(name) > MAX_NAME_LEN:
            raise ValueError(f"Name too long: {name!r} (max {MAX_NAME_LEN})")
        if self.find_file(name) is not None:
            raise FileExistsError(f"File already exists: {name!r}")

        nsectors = _sectors_needed(len(data)) if data else 1
        bmap = self._bmap
        start = _bitmap_find_free(bmap, nsectors, self.total)
        if start is None:
            raise RuntimeError(f"No space for {nsectors} contiguous sectors")

        # Allocate
        for s in range(start, start + nsectors):
            _bitmap_set(bmap, s)
        self._flush_bmap(bmap)

        # Write data
        off = start * SECTOR_SIZE
        padded = bytearray(data) + b"\x00" * (nsectors * SECTOR_SIZE - len(data))
        self.img[off : off + nsectors * SECTOR_SIZE] = padded

        # Create directory entry in first free slot
        dirdata = self._dir_data
        slot = None
        for i in range(MAX_FILES):
            if dirdata[i * DIR_ENTRY_SIZE] == 0:
                slot = i
                break
        if slot is None:
            raise RuntimeError("Directory full (64 files)")

        entry = DirEntry(name, start, nsectors, len(data), ftype, flags)
        _write_entry(dirdata, slot * DIR_ENTRY_SIZE, entry)
        self._flush_dir(dirdata)
        return entry

    def read_file(self, name: str) -> bytes:
        """Read file contents by name."""
        result = self.find_file(name)
        if result is None:
            raise FileNotFoundError(f"File not found: {name!r}")
        _, entry = result
        off = entry.start_sector * SECTOR_SIZE
        return bytes(self.img[off : off + entry.used_bytes])

    def delete_file(self, name: str):
        """Delete a file: free its sectors and directory entry."""
        result = self.find_file(name)
        if result is None:
            raise FileNotFoundError(f"File not found: {name!r}")
        slot, entry = result

        # Free sectors in bitmap
        bmap = self._bmap
        for s in range(entry.start_sector,
                       entry.start_sector + entry.sector_count):
            _bitmap_clear(bmap, s)
        self._flush_bmap(bmap)

        # Clear directory slot
        dirdata = self._dir_data
        _clear_entry(dirdata, slot * DIR_ENTRY_SIZE)
        self._flush_dir(dirdata)

    def info(self) -> dict:
        """Return superblock metadata."""
        if not self._verify_magic():
            return {"formatted": False}
        return {
            "formatted": True,
            "version": struct.unpack_from("<H", self.img, 4)[0],
            "total_sectors": struct.unpack_from("<H", self.img, 6)[0],
            "data_start": struct.unpack_from("<H", self.img, 16)[0],
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
                ftype: int = FTYPE_RAW, flags: int = 0) -> DirEntry:
    """Open an existing image and inject a named file."""
    fs = MP64FS.load(path)
    entry = fs.inject_file(name, data, ftype, flags)
    fs.save(path)
    return entry


def read_file(path: str | Path, name: str) -> bytes:
    """Open an existing image and read a named file."""
    return MP64FS.load(path).read_file(name)


def list_files(path: str | Path) -> list[DirEntry]:
    """Open an existing image and list all files."""
    return MP64FS.load(path).list_files()


def delete_file(path: str | Path, name: str):
    """Open an existing image and delete a named file."""
    fs = MP64FS.load(path)
    fs.delete_file(name)
    fs.save(path)
