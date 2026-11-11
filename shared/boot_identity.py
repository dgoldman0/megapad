"""The identity of what booted, and the stamp of a load that starts from it.

BIOS ``BOOT-ID`` gives one cell that differs when the BIOS's code differs or
when the first file it loaded since reset, KDOS for a boot from disk, differs.
KDOS starts the stamp of a load from that cell and takes in each file as its
load begins; a module's token file holds the stamp that stood where its
recording began and is used only where the stamp stands the same
(``docs/token-modules-proposal-2026-11-11.md``).

``bios.asm`` (``boot_identity_fold``) and ``kdos.f`` (``_STAMP-TAKE``) define
the arithmetic.  This module computes the same cells for the hosted
simulator, which has no BIOS image of its own and stands for the one
``bios.asm`` assembles to, and for the tools that build images.
"""

from __future__ import annotations

import functools
import struct
import zlib
from pathlib import Path

_MASK = (1 << 64) - 1
_K = 0x9E3779B97F4A7C15
_BIOS_SOURCE = Path(__file__).resolve().parents[1] / "bios.asm"


def take(stamp: int, value: int) -> int:
    """One step: STAMP with VALUE taken in."""

    stamp = ((stamp ^ (value & _MASK)) * _K) & _MASK
    return stamp ^ (stamp >> 32)


def fold_bytes(stamp: int, data: bytes) -> int:
    """STAMP with DATA taken in: each eight bytes as one little-endian
    cell, then each byte left over."""

    whole = len(data) & ~7
    for (cell,) in struct.iter_unpack("<Q", bytes(data[:whole])):
        stamp = take(stamp, cell)
    for byte in data[whole:]:
        stamp = take(stamp, byte)
    return stamp


@functools.lru_cache(maxsize=4)
def _bios_code(path: Path, _size: int, _mtime_ns: int) -> bytes:
    import asm

    labels: dict[str, int] = {}
    image = asm.assemble(path.read_text(), labels_out=labels)
    return bytes(image[: labels["var_state"]])


def bios_code(path: Path | None = None) -> bytes:
    """The bytes of the BIOS below its first variable: its code and its
    dictionary, which never change while it runs."""

    source = _BIOS_SOURCE if path is None else Path(path)
    status = source.stat()
    return _bios_code(source, status.st_size, status.st_mtime_ns)


def boot_identity(boot_file: bytes | None = None, *, bios: bytes | None = None) -> int:
    """What ``BOOT-ID`` gives on a machine whose BIOS code is BIOS (by
    default the code ``bios.asm`` assembles to) after it loaded BOOT_FILE
    first, or before it loaded any file."""

    stamp = fold_bytes(0, bios_code() if bios is None else bios)
    if boot_file is not None:
        stamp = fold_bytes(stamp, boot_file)
    return stamp or 1


def text_identity(text: bytes) -> int:
    """A source text's identity in the stamp: its CRC-32 and its length."""

    return (((zlib.crc32(text) & 0xFFFFFFFF) << 32) ^ len(text)) & _MASK


def stamp_take(stamp: int, identity: int) -> int:
    """KDOS ``_STAMP-TAKE``: STAMP with one identity taken in, never zero."""

    return take(stamp, identity) or 1


def stamp_start(boot: int) -> int:
    """The stamp of a load before any file: started from BOOT-ID."""

    return stamp_take(0, boot)


__all__ = [
    "bios_code",
    "boot_identity",
    "fold_bytes",
    "stamp_start",
    "stamp_take",
    "take",
    "text_identity",
]
