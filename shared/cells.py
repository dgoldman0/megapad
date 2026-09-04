"""Backend-neutral MegaPad cell representation.

Guest cells are bit patterns, so normalization is explicit at the boundary
instead of relying on Python's unbounded signed integers.
"""

from __future__ import annotations


CELL_BITS = 64
CELL_BYTES = CELL_BITS // 8
MASK64 = (1 << CELL_BITS) - 1
SIGN64 = 1 << (CELL_BITS - 1)

FALSE = 0
TRUE = MASK64


def u64(value: int) -> int:
    """Return *value* reduced to its unsigned 64-bit cell pattern."""

    return value & MASK64


def s64(value: int) -> int:
    """Interpret *value*'s low 64 bits as a two's-complement integer."""

    value = u64(value)
    return value - (1 << CELL_BITS) if value & SIGN64 else value


def forth_flag(value: bool) -> int:
    """Return MegaForth's full-width canonical flag for *value*."""

    return TRUE if value else FALSE


__all__ = [
    "CELL_BITS",
    "CELL_BYTES",
    "MASK64",
    "SIGN64",
    "FALSE",
    "TRUE",
    "u64",
    "s64",
    "forth_flag",
]
