"""Backend-neutral Keccak-f[1600] value operations.

The architectural emulator and hosted simulator expose different execution
models around Keccak, but portable state bytes use the same 25 little-endian
64-bit lanes.  This module owns only the pure 24-round permutation.  It has no
MMIO, ownership, padding, absorb/squeeze policy, timing, or host-secret API.
"""

from __future__ import annotations

from collections.abc import Sequence

from shared.cells import MASK64


KECCAK_LANES = 25

_ROUND_CONSTANTS = (
    0x0000000000000001,
    0x0000000000008082,
    0x800000000000808A,
    0x8000000080008000,
    0x000000000000808B,
    0x0000000080000001,
    0x8000000080008081,
    0x8000000000008009,
    0x000000000000008A,
    0x0000000000000088,
    0x0000000080008009,
    0x000000008000000A,
    0x000000008000808B,
    0x800000000000008B,
    0x8000000000008089,
    0x8000000000008003,
    0x8000000000008002,
    0x8000000000000080,
    0x000000000000800A,
    0x800000008000000A,
    0x8000000080008081,
    0x8000000000008080,
    0x0000000080000001,
    0x8000000080008008,
)

_ROTATIONS = (
    0,
    1,
    62,
    28,
    27,
    36,
    44,
    6,
    55,
    20,
    3,
    10,
    43,
    25,
    39,
    41,
    45,
    15,
    21,
    8,
    18,
    2,
    61,
    56,
    14,
)


def _rotate_left(value: int, shift: int) -> int:
    if shift == 0:
        return value
    return ((value << shift) | (value >> (64 - shift))) & MASK64


def keccak_f1600(lanes: Sequence[int]) -> tuple[int, ...]:
    """Return the 24-round permutation of exactly 25 uint64 lanes.

    Lane index is ``x + 5*y``.  Serializing each returned lane little-endian
    therefore gives the exact 200-byte caller image used by MegaPad.
    """

    if not isinstance(lanes, Sequence):
        raise TypeError("Keccak state must be a sequence of uint64 lanes")
    if len(lanes) != KECCAK_LANES:
        raise ValueError("Keccak state must contain exactly 25 lanes")

    state: list[int] = []
    for lane in lanes:
        if isinstance(lane, bool) or not isinstance(lane, int):
            raise TypeError("Keccak lanes must be uint64 integers")
        if not 0 <= lane <= MASK64:
            raise ValueError("Keccak lanes must be uint64 integers")
        state.append(lane)

    for round_constant in _ROUND_CONSTANTS:
        columns = [
            state[x]
            ^ state[x + 5]
            ^ state[x + 10]
            ^ state[x + 15]
            ^ state[x + 20]
            for x in range(5)
        ]
        deltas = [
            columns[(x + 4) % 5]
            ^ _rotate_left(columns[(x + 1) % 5], 1)
            for x in range(5)
        ]
        for index in range(KECCAK_LANES):
            state[index] ^= deltas[index % 5]

        rotated = [0] * KECCAK_LANES
        for x in range(5):
            for y in range(5):
                source = x + 5 * y
                destination = y + 5 * ((2 * x + 3 * y) % 5)
                rotated[destination] = _rotate_left(
                    state[source],
                    _ROTATIONS[source],
                )

        for y in range(5):
            row = y * 5
            for x in range(5):
                state[row + x] = (
                    rotated[row + x]
                    ^ (
                        (~rotated[row + ((x + 1) % 5)] & MASK64)
                        & rotated[row + ((x + 2) % 5)]
                    )
                )

        state[0] ^= round_constant

    return tuple(state)


__all__ = ["KECCAK_LANES", "keccak_f1600"]
