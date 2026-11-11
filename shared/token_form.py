"""The token form of a Forth source file: its records and its file layout.

A token file is what the interpreter did with a source file, written down:
which words it found, which numbers, and the text each parsing word took.
BIOS ``TOKEN-REPLAY`` and its hosted counterpart run the records through the
interpreter's own two steps, so loading a token file builds the dictionary
its source builds.  ``bios.asm`` (``w_token_replay``) defines the records and
``kdos.f`` the header; this module writes and reads both for the tools that
build images and for tests.

Records
-------
A first byte below ``0xF0`` starts a two-byte slot number, high byte first:
the word in that slot of the file's table.  Otherwise the first byte is an
opcode::

    F0 n name   IMPORT   find NAME now, put it in the next slot, use it
    F1 b        NUM1     a number, one signed byte
    F2 h        NUM2     two bytes, signed, little-endian
    F3 w        NUM4     four bytes, signed
    F4 q        NUM8     eight bytes
    F5 n text   TEXT     the input for the next word
    F6          LINE     the next source line begins
    F7 h        LINES    H source lines on
    F8          SKIPPED  a conditional skip ends here
    F9          DEFINED  the word just defined takes the next slot
    FF          END

File
----
``MFTOKEN1``, then little-endian fields::

     +8  u32  bytes before the records   +12  u32  cells in the table
    +16  u64  bytes of records           +24  u64  bytes of the source
    +32  u32  CRC-32 of the source       +36  u32  CRC-32 of the records
    +40  u32  lines in the source        +44  u16  bytes of the module name
    +46  u16  flags                      +48  u64  the stamp
    +56  u64  reserved, zero             +64  the module name, zero-padded

then the records.  The table is allocated when the file is replayed.

The stamp is KDOS's ``LOAD-STAMP`` as it stood where the file's recording
began (``shared/boot_identity.py``); KDOS uses the file only where the
stamp stands the same.  Flag 1 says that the file's source ships beside it,
under the file's name without its last letter: KDOS then also checks the
file against that source and loads the source when either check fails.  A
file with no records only names its module.  It stands on a volume for a
module that is loaded before the volume is mounted.
"""

from __future__ import annotations

import struct
import zlib
from dataclasses import dataclass
from typing import Hashable, Iterator

MAGIC = b"MFTOKEN1"
HEADER_BYTES = 64
_HEADER = struct.Struct("<8sIIQQIIIHHQQ")
assert _HEADER.size == HEADER_BYTES

OP_IMPORT = 0xF0
OP_NUM1 = 0xF1
OP_NUM2 = 0xF2
OP_NUM4 = 0xF3
OP_NUM8 = 0xF4
OP_TEXT = 0xF5
OP_LINE = 0xF6
OP_LINES = 0xF7
OP_SKIPPED = 0xF8
OP_DEFINED = 0xF9
OP_END = 0xFF

# A slot number's first byte is below 0xF0.
MAX_SLOTS = 0xF000
MAX_NAME_BYTES = 255
MAX_TEXT_BYTES = 255
MODULE_NAME_MAX_BYTES = 0xFFFF
FLAG_SOURCE_BESIDE = 1
_KNOWN_FLAGS = FLAG_SOURCE_BESIDE


class TokenFormError(ValueError):
    """A source file's record cannot be written in the token form."""


class TokenWriter:
    """Write one file's records in the order the interpreter produced them."""

    def __init__(self) -> None:
        self._records = bytearray()
        self._slots: dict[Hashable, int] = {}
        self._line = 0
        self._written_line = 0
        self._finished = False

    @property
    def slots(self) -> int:
        return len(self._slots)

    def line(self, number: int) -> None:
        """The records that follow come from source line NUMBER (from 1)."""

        if number < self._line:
            raise TokenFormError("source lines must not go backward")
        self._line = number

    def _mark_line(self) -> None:
        gap = self._line - self._written_line
        while gap > 0:
            if gap == 1:
                self._records.append(OP_LINE)
                gap = 0
            else:
                step = min(gap, 0xFFFF)
                self._records.append(OP_LINES)
                self._records += step.to_bytes(2, "little")
                gap -= step
        self._written_line = self._line

    def word(self, key: Hashable, name: bytes, text: bytes | None = None) -> None:
        """A word the interpreter found.

        KEY tells one binding of a name from another: the first record with
        a key imports NAME, and later ones use its slot.  TEXT is the input
        the word took when it ran, if any.
        """

        self._mark_line()
        if text:
            if len(text) > MAX_TEXT_BYTES:
                raise TokenFormError("a word's input text exceeds 255 bytes")
            self._records.append(OP_TEXT)
            self._records.append(len(text))
            self._records += text
        slot = self._slots.get(key)
        if slot is None:
            if not 0 < len(name) <= MAX_NAME_BYTES:
                raise TokenFormError("a word's name must be 1 to 255 bytes")
            if len(self._slots) >= MAX_SLOTS:
                raise TokenFormError("the file uses more than 61,440 names")
            self._slots[key] = len(self._slots)
            self._records.append(OP_IMPORT)
            self._records.append(len(name))
            self._records += name
            return
        self._records += slot.to_bytes(2, "big")

    def number(self, value: int) -> None:
        """A number the interpreter converted; VALUE is the cell."""

        self._mark_line()
        value &= (1 << 64) - 1
        signed = value - (1 << 64) if value >> 63 else value
        for opcode, size in ((OP_NUM1, 1), (OP_NUM2, 2), (OP_NUM4, 4), (OP_NUM8, 8)):
            if -(1 << (8 * size - 1)) <= signed < (1 << (8 * size - 1)):
                self._records.append(opcode)
                self._records += signed.to_bytes(size, "little", signed=True)
                return
        raise AssertionError("a cell fits in eight bytes")

    def skipped(self) -> None:
        """The skip that the last word began ends here."""

        self._records.append(OP_SKIPPED)

    def defined(self, key: Hashable) -> None:
        """The last record completed the definition of the word with KEY:
        it takes the next slot, with no search for its name."""

        if key in self._slots:
            raise TokenFormError("the word already has a slot")
        if len(self._slots) >= MAX_SLOTS:
            raise TokenFormError("the file uses more than 61,440 names")
        self._slots[key] = len(self._slots)
        self._records.append(OP_DEFINED)

    def has_slot(self, key: Hashable) -> bool:
        return key in self._slots

    def finish(self) -> bytes:
        if not self._finished:
            self._records.append(OP_END)
            self._finished = True
        return bytes(self._records)


def pack_token_file(
    records: bytes,
    slots: int,
    *,
    source: bytes,
    lines: int,
    module_name: bytes = b"",
    stamp: int = 0,
    source_beside: bool = False,
) -> bytes:
    """Lay RECORDS out as a token file with a table of SLOTS cells.

    STAMP is the load's stamp where the recording of SOURCE began; a file
    left at zero is never current.  SOURCE_BESIDE says that the source ships
    beside the file.
    """

    if not 0 <= slots <= MAX_SLOTS:
        raise TokenFormError("slot count out of range")
    if len(module_name) > MODULE_NAME_MAX_BYTES:
        raise TokenFormError("module name too long")
    if not 0 <= stamp < 1 << 64:
        raise TokenFormError("stamp out of range")
    name_bytes = (len(module_name) + 7) & ~7
    header_bytes = HEADER_BYTES + name_bytes
    header = _HEADER.pack(
        MAGIC,
        header_bytes,
        slots,
        len(records),
        len(source),
        zlib.crc32(source) & 0xFFFFFFFF,
        zlib.crc32(records) & 0xFFFFFFFF,
        lines,
        len(module_name),
        FLAG_SOURCE_BESIDE if source_beside else 0,
        stamp,
        0,
    )
    return b"".join((
        header,
        module_name.ljust(name_bytes, b"\0"),
        records,
    ))


def pack_module_name_file(module_name: bytes) -> bytes:
    """A token file with no records, which only names its module."""

    if not module_name:
        raise TokenFormError("a file that only names a module needs the name")
    return pack_token_file(b"", 0, source=b"", lines=0, module_name=module_name)


@dataclass(frozen=True, slots=True)
class TokenFile:
    """A token file's parts."""

    header_bytes: int
    slots: int
    records: bytes
    source_bytes: int
    source_crc32: int
    records_crc32: int
    lines: int
    module_name: bytes
    stamp: int = 0
    source_beside: bool = False

    @property
    def text_identity(self) -> int:
        """The identity of the source in the stamp of a load."""

        return ((self.source_crc32 << 32) ^ self.source_bytes) & ((1 << 64) - 1)


def is_token_file(data: bytes) -> bool:
    return len(data) >= HEADER_BYTES and data[:8] == MAGIC


def unpack_token_file(data: bytes) -> TokenFile:
    """Validate DATA's layout and return its parts."""

    if not is_token_file(data):
        raise TokenFormError("not a token file")
    (
        _magic, header_bytes, slots, record_bytes, source_bytes, source_crc,
        records_crc, lines, name_bytes, flags, stamp, reserved,
    ) = _HEADER.unpack_from(data)
    if header_bytes < HEADER_BYTES or header_bytes & 7:
        raise TokenFormError("bad header size")
    if HEADER_BYTES + name_bytes > header_bytes:
        raise TokenFormError("module name outside the header")
    if header_bytes + record_bytes != len(data):
        raise TokenFormError("sizes do not account for the file")
    if flags & ~_KNOWN_FLAGS or reserved:
        raise TokenFormError("reserved fields are not zero")
    records = bytes(data[header_bytes:])
    if zlib.crc32(records) & 0xFFFFFFFF != records_crc:
        raise TokenFormError("record CRC mismatch")
    return TokenFile(
        header_bytes=header_bytes,
        slots=slots,
        records=records,
        source_bytes=source_bytes,
        source_crc32=source_crc,
        records_crc32=records_crc,
        lines=lines,
        module_name=bytes(data[HEADER_BYTES : HEADER_BYTES + name_bytes]),
        stamp=stamp,
        source_beside=bool(flags & FLAG_SOURCE_BESIDE),
    )


def with_stamp(data: bytes, stamp: int, *, source_beside: bool | None = None) -> bytes:
    """DATA, a token file, with STAMP in its header and, when given, its
    source-beside flag set or cleared."""

    unpack_token_file(data)
    out = bytearray(data)
    struct.pack_into("<Q", out, 48, stamp & ((1 << 64) - 1))
    if source_beside is not None:
        flags = struct.unpack_from("<H", out, 46)[0] & ~FLAG_SOURCE_BESIDE
        struct.pack_into(
            "<H", out, 46, flags | (FLAG_SOURCE_BESIDE if source_beside else 0)
        )
    return bytes(out)


def iter_records(records: bytes) -> Iterator[tuple]:
    """Decode RECORDS for inspection: ("word", slot), ("import", name),
    ("number", value), ("text", bytes), ("line", count), ("skipped",),
    ("defined",)."""

    index = 0
    size = len(records)
    while True:
        if index >= size:
            raise TokenFormError("records end without END")
        opcode = records[index]
        if opcode < 0xF0:
            yield ("word", (opcode << 8) | records[index + 1])
            index += 2
        elif opcode == OP_IMPORT:
            count = records[index + 1]
            yield ("import", bytes(records[index + 2 : index + 2 + count]))
            index += 2 + count
        elif opcode in (OP_NUM1, OP_NUM2, OP_NUM4, OP_NUM8):
            count = {OP_NUM1: 1, OP_NUM2: 2, OP_NUM4: 4, OP_NUM8: 8}[opcode]
            yield ("number", int.from_bytes(
                records[index + 1 : index + 1 + count], "little", signed=True))
            index += 1 + count
        elif opcode == OP_TEXT:
            count = records[index + 1]
            yield ("text", bytes(records[index + 2 : index + 2 + count]))
            index += 2 + count
        elif opcode == OP_LINE:
            yield ("line", 1)
            index += 1
        elif opcode == OP_LINES:
            yield ("line", int.from_bytes(records[index + 1 : index + 3], "little"))
            index += 3
        elif opcode == OP_SKIPPED:
            yield ("skipped",)
            index += 1
        elif opcode == OP_DEFINED:
            yield ("defined",)
            index += 1
        elif opcode == OP_END:
            return
        else:
            raise TokenFormError(f"unknown opcode {opcode:#x}")


__all__ = [
    "FLAG_SOURCE_BESIDE",
    "HEADER_BYTES",
    "MAGIC",
    "MAX_SLOTS",
    "TokenFile",
    "TokenFormError",
    "TokenWriter",
    "is_token_file",
    "iter_records",
    "pack_module_name_file",
    "pack_token_file",
    "unpack_token_file",
    "with_stamp",
]
