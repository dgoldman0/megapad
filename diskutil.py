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

FTYPE_TUT   = 6
FTYPE_BUNDLE = 7

FTYPE_NAMES = {
    FTYPE_FREE: "free", FTYPE_RAW: "raw", FTYPE_TEXT: "text",
    FTYPE_FORTH: "forth", FTYPE_DOC: "doc", FTYPE_DATA: "data",
    FTYPE_TUT: "tutorial", FTYPE_BUNDLE: "bundle",
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
  Sector 0     Superblock (magic MP64)
  Sector 1     Allocation bitmap
  Sectors 2-5  Directory (64 entries)
  Sectors 6+   Data area (~1 MB usable)
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

    # cat — read a file from an image
    p_cat = sub.add_parser("cat", help="Read a file from an image")
    p_cat.add_argument("image", help="Disk image path")
    p_cat.add_argument("name", help="File name to read")

    # rm — delete a file from an image
    p_rm = sub.add_parser("rm", help="Delete a file from an image")
    p_rm.add_argument("image", help="Disk image path")
    p_rm.add_argument("name", help="File name to delete")

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
        print(f"{'Name':<16} {'Type':<8} {'Size':>8}  {'Sectors':>7}  Flags")
        print("-" * 56)
        for e in entries:
            tname = FTYPE_NAMES.get(e.ftype, f"?{e.ftype}")
            print(f"{e.name:<16} {tname:<8} {e.used_bytes:>8}  {e.sector_count:>7}  "
                  f"0x{e.flags:02x}")

    elif args.cmd == "inject":
        name = args.name or os.path.basename(args.file)
        ftype_rev = {v: k for k, v in FTYPE_NAMES.items()}
        ftype = ftype_rev[args.type]
        with open(args.file, "rb") as f:
            data = f.read()
        inject_file(args.image, name, data, ftype=ftype)
        print(f"Injected '{name}' ({len(data)} bytes, type={args.type})")

    elif args.cmd == "cat":
        data = read_file(args.image, args.name)
        sys.stdout.buffer.write(data)

    elif args.cmd == "rm":
        delete_file(args.image, args.name)
        print(f"Deleted '{args.name}'")


if __name__ == "__main__":
    main()
