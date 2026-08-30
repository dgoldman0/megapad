"""Minimal production tile value operations used by hosted diagnostics.

These functions intentionally model values only: one 64-byte tile of unsigned
8-bit lanes, with no registers, scratchpad, instruction encoding, latency, or
bus behavior.  Later semantic tile adapters can reuse this kernel.
"""

from __future__ import annotations

from shared.cells import u64


TILE_BYTES = 64


def _tile_bytes(value: bytes, *, label: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{label} must be bytes")
    if len(value) != TILE_BYTES:
        raise ValueError(f"{label} must contain exactly {TILE_BYTES} lanes")
    return value


def tile_add_u8(left: bytes, right: bytes) -> bytes:
    """Return wrapping unsigned 8-bit lane addition."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return bytes((a + b) & 0xFF for a, b in zip(left, right))


def tile_multiply_u8(left: bytes, right: bytes) -> bytes:
    """Return wrapping unsigned 8-bit lane multiplication."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return bytes((a * b) & 0xFF for a, b in zip(left, right))


def tile_dot_u8(left: bytes, right: bytes) -> int:
    """Return the wrapped cell sum of unsigned lane products."""

    left = _tile_bytes(left, label="left tile")
    right = _tile_bytes(right, label="right tile")
    return u64(sum(a * b for a, b in zip(left, right)))


def tile_sum_u8(tile: bytes) -> int:
    """Return the wrapped cell sum of unsigned lanes."""

    tile = _tile_bytes(tile, label="tile")
    return u64(sum(tile))


__all__ = [
    "TILE_BYTES",
    "tile_add_u8",
    "tile_dot_u8",
    "tile_multiply_u8",
    "tile_sum_u8",
]
