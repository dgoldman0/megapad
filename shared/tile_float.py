"""Floating-point tile lane values shared by the emulator and hosted simulator.

``docs/floating-point.md`` §4–§5 define these results.  The functions take
and return raw lane integers.  TMODE, TCTRL, the accumulator, guest memory,
fault ordering, and timing remain backend state.
"""

from __future__ import annotations

from typing import Sequence

from shared import ieee_fp as fp
from shared.ieee_fp import Format, accumulation_format


TILE_BYTES = 64

# TALU function codes.
ADD, SUB, AND, OR, XOR, MIN, MAX, ABS = range(8)


def unpack_lanes(fmt: Format, tile: bytes) -> list[int]:
    width = fmt.width // 8
    return [int.from_bytes(tile[offset:offset + width], "little")
            for offset in range(0, TILE_BYTES, width)]


def pack_lanes(fmt: Format, lanes: Sequence[int]) -> bytearray:
    width = fmt.width // 8
    output = bytearray(len(lanes) * width)
    for index, value in enumerate(lanes):
        output[index * width:(index + 1) * width] = (
            value & fmt.mask
        ).to_bytes(width, "little")
    return output


def elementwise(fmt: Format, funct: int, a: Sequence[int],
                b: Sequence[int]) -> list[int]:
    """TALU in a float format."""

    if funct == ADD:
        return [fp.lane_add(fmt, x, y) for x, y in zip(a, b)]
    if funct == SUB:
        return [fp.lane_sub(fmt, x, y) for x, y in zip(a, b)]
    if funct == AND:
        return [x & y for x, y in zip(a, b)]
    if funct == OR:
        return [x | y for x, y in zip(a, b)]
    if funct == XOR:
        return [x ^ y for x, y in zip(a, b)]
    if funct == MIN:
        return [fp.minimum(fmt, x, y) for x, y in zip(a, b)]
    if funct == MAX:
        return [fp.maximum(fmt, x, y) for x, y in zip(a, b)]
    if funct == ABS:
        magnitude = fmt.mask ^ fmt.sign_bit
        return [x & magnitude for x in a]
    raise ValueError(f"unknown TALU function {funct}")


def multiply(fmt: Format, a: Sequence[int], b: Sequence[int]) -> list[int]:
    return [fp.lane_mul(fmt, x, y) for x, y in zip(a, b)]


def fused_multiply_add(fmt: Format, a: Sequence[int], b: Sequence[int],
                       c: Sequence[int]) -> list[int]:
    """TMAC and TFMA: ``RN(a * b + c)`` per lane."""

    return [fp.lane_fma(fmt, x, y, z) for x, y, z in zip(a, b, c)]


def widening_multiply(fmt: Format, a: Sequence[int],
                      b: Sequence[int]) -> list[int]:
    """WMUL: each product rounded once to the accumulation format."""

    wide = accumulation_format(fmt)
    return [fp.lane_product(wide, fmt, x, y) for x, y in zip(a, b)]


def _products(fmt: Format, a: Sequence[int], b: Sequence[int]) -> list[int]:
    wide = accumulation_format(fmt)
    return [fp.lane_product(wide, fmt, x, y) for x, y in zip(a, b)]


def dot(fmt: Format, a: Sequence[int], b: Sequence[int]) -> int:
    return fp.reduce_tree(accumulation_format(fmt), _products(fmt, a, b))


def dot_chunks(fmt: Format, a: Sequence[int], b: Sequence[int]) -> list[int]:
    """DOTACC: the four quarter subtrees, in chunk order."""

    wide = accumulation_format(fmt)
    leaves = _products(fmt, a, b)
    quarter = len(leaves) // 4
    return [fp.reduce_tree(wide, leaves[k * quarter:(k + 1) * quarter])
            for k in range(4)]


def sum_lanes(fmt: Format, a: Sequence[int]) -> int:
    wide = accumulation_format(fmt)
    return fp.reduce_tree(wide, [fp.lane_convert(wide, fmt, x) for x in a])


def sum_squares(fmt: Format, a: Sequence[int]) -> int:
    return fp.reduce_tree(accumulation_format(fmt), _products(fmt, a, a))


def l1_norm(fmt: Format, a: Sequence[int]) -> int:
    magnitude = fmt.mask ^ fmt.sign_bit
    return sum_lanes(fmt, [x & magnitude for x in a])


def extreme(fmt: Format, a: Sequence[int], largest: bool) -> int:
    """TRED MIN/MAX: the NaN-skipping extreme, in the accumulation format."""

    return extreme_index(fmt, a, largest)[1]


def extreme_index(fmt: Format, a: Sequence[int], largest: bool
                  ) -> tuple[int, int]:
    """TRED MINIDX/MAXIDX: ``(lane, value in the accumulation format)``."""

    index, value = fp.extreme_index(fmt, a, largest)
    return index, fp.lane_convert(accumulation_format(fmt), fmt, value)


def accumulate_sum(wide: Format, old: int, result: int) -> int:
    """ACC_ACC for DOT, DOTACC, SUM, SUMSQ, and L1: one more rounding."""

    return fp.lane_add(wide, old, result)


def accumulate_extreme(wide: Format, old: int, result: int,
                       largest: bool) -> int:
    """ACC_ACC for TRED MIN/MAX: running NaN-skipping extreme."""

    return fp.skip_nan_extreme(wide, old, result, largest)


def index_replaces(wide: Format, candidate: int, old: int,
                   largest: bool) -> bool:
    """ACC_ACC for MINIDX/MAXIDX: does the tile result replace ACC0/ACC1?"""

    if fp.is_nan(wide, candidate):
        return False
    if fp.is_nan(wide, old):
        return True
    candidate_key = fp.order_key(wide, candidate)
    old_key = fp.order_key(wide, old)
    return candidate_key > old_key if largest else candidate_key < old_key


def is_zero(fmt: Format, bits: int) -> bool:
    return (bits & fmt.mask & ~fmt.sign_bit) == 0


def convert_lanes(dst: Format, src: Format, a: Sequence[int]) -> list[int]:
    """Convert each lane with one RNE rounding (exact when widening)."""

    return [fp.lane_convert(dst, src, x) for x in a]
