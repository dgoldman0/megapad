"""The identity of what booted.

BIOS ``BOOT-ID`` gives one cell that differs when the BIOS's code differs or
when the first file it loaded since reset, KDOS for a boot from disk, differs.
A module's token file holds the cell of the system it was built for, and
KDOS uses the file only where the BIOS gives the same one
(``docs/token-modules-proposal-2026-11-11.md``).

``bios.asm`` (``boot_identity_fold``) defines the arithmetic.  This module
computes the same cell for the hosted simulator, which has no BIOS image of
its own and stands for the one ``bios.asm`` assembles to, and for the tools
that build images.
"""

from __future__ import annotations

import functools
import struct
from pathlib import Path

_MASK = (1 << 64) - 1
_K = 0x9E3779B97F4A7C15
_BIOS_SOURCE = Path(__file__).resolve().parents[1] / "bios.asm"


def take(cell: int, value: int) -> int:
    """One step of the hash: CELL with VALUE taken in."""

    cell = ((cell ^ (value & _MASK)) * _K) & _MASK
    return cell ^ (cell >> 32)


def fold_bytes(cell: int, data: bytes) -> int:
    """CELL with DATA taken in: each eight bytes as one little-endian
    cell, then each byte left over."""

    whole = len(data) & ~7
    for (word,) in struct.iter_unpack("<Q", bytes(data[:whole])):
        cell = take(cell, word)
    for byte in data[whole:]:
        cell = take(cell, byte)
    return cell


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

    cell = fold_bytes(0, bios_code() if bios is None else bios)
    if boot_file is not None:
        cell = fold_bytes(cell, boot_file)
    return cell or 1


__all__ = ["bios_code", "boot_identity", "fold_bytes", "take"]
