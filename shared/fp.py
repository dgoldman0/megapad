"""Backend-neutral floating-point value conversions for MegaPad tile lanes.

This module owns only raw-value conversion and classification.  Tile registers,
guest memory, instruction dispatch, accumulator state, and operation ordering
remain properties of the emulator or simulator adapter that calls it.
"""

from __future__ import annotations

import math
import struct


FP16_FORMAT = 4
BF16_FORMAT = 5


def fp16_to_float(raw: int) -> float:
    """Decode one IEEE 754 binary16 bit pattern into a Python float."""

    sign = (raw >> 15) & 1
    exponent = (raw >> 10) & 0x1F
    fraction = raw & 0x3FF
    if exponent == 0:
        if fraction == 0:
            return -0.0 if sign else 0.0
        value = (2.0**-14) * (fraction / 1024.0)
        return -value if sign else value
    if exponent == 0x1F:
        if fraction == 0:
            return float("-inf") if sign else float("inf")
        return float("nan")
    value = (2.0 ** (exponent - 15)) * (1.0 + fraction / 1024.0)
    return -value if sign else value


def pack_fp32_bits(value: float) -> int:
    """Round a Python float to IEEE binary32 and return its raw bits."""

    try:
        return struct.unpack("<I", struct.pack("<f", value))[0]
    except OverflowError:
        return 0xFF80_0000 if value < 0.0 else 0x7F80_0000


def float_to_fp16(value: float) -> int:
    """Encode like the legacy oracle, including its subnormal-carry defect."""

    if math.isnan(value):
        return 0x7E00
    if math.isinf(value):
        return 0xFC00 if value < 0 else 0x7C00
    if value == 0.0:
        return 0x8000 if math.copysign(1.0, value) < 0 else 0x0000

    bits = pack_fp32_bits(value)
    sign = (bits >> 31) & 1
    exponent32 = (bits >> 23) & 0xFF
    fraction32 = bits & 0x7F_FFFF
    exponent16 = exponent32 - 127 + 15
    if exponent16 >= 0x1F:
        return (sign << 15) | 0x7C00
    if exponent16 <= 0:
        if exponent16 < -10:
            return sign << 15
        fraction32 |= 0x80_0000
        shift = 1 - exponent16
        round_bit = (fraction32 >> (12 + shift)) & 1
        sticky = (fraction32 & ((1 << (12 + shift)) - 1)) != 0
        result = fraction32 >> (13 + shift)
        if round_bit and (sticky or (result & 1)):
            result += 1
        return (sign << 15) | (result & 0x3FF)

    round_bit = (fraction32 >> 12) & 1
    sticky = (fraction32 & 0xFFF) != 0
    fraction16 = fraction32 >> 13
    if round_bit and (sticky or (fraction16 & 1)):
        fraction16 += 1
        if fraction16 >= 0x400:
            fraction16 = 0
            exponent16 += 1
            if exponent16 >= 0x1F:
                return (sign << 15) | 0x7C00
    return (sign << 15) | (exponent16 << 10) | (fraction16 & 0x3FF)


def bf16_to_float(raw: int) -> float:
    """Decode one bfloat16 bit pattern into a Python float."""

    bits32 = (raw & 0xFFFF) << 16
    return struct.unpack("<f", struct.pack("<I", bits32))[0]


def float_to_bf16(value: float) -> int:
    """Encode a Python float as bfloat16 using round-to-nearest-even."""

    bits = pack_fp32_bits(value)
    if (bits & 0x7F80_0000) == 0x7F80_0000 and bits & 0x007F_FFFF:
        return ((bits >> 16) | 0x0040) & 0xFFFF
    round_bit = (bits >> 15) & 1
    sticky = (bits & 0x7FFF) != 0
    result = bits >> 16
    if round_bit and (sticky or (result & 1)):
        result += 1
    return result & 0xFFFF


def fp32_to_bits(value: float) -> int:
    """Encode a Python float as one IEEE binary32 bit pattern."""

    return pack_fp32_bits(value)


def bits_to_fp32(raw: int) -> float:
    """Decode the low 32 bits of an IEEE binary32 bit pattern."""

    return struct.unpack("<f", struct.pack("<I", raw & 0xFFFF_FFFF))[0]


def decode_tile_float(raw: int, format_code: int) -> float:
    """Decode one FP16 or BF16 tile lane selected by its TMODE EW code."""

    if format_code == FP16_FORMAT:
        return fp16_to_float(raw & 0xFFFF)
    if format_code == BF16_FORMAT:
        return bf16_to_float(raw & 0xFFFF)
    raise ValueError(f"unsupported tile floating-point format {format_code}")


def encode_tile_float(value: float, format_code: int) -> int:
    """Encode one FP16 or BF16 tile lane selected by its TMODE EW code."""

    if format_code == FP16_FORMAT:
        return float_to_fp16(value)
    if format_code == BF16_FORMAT:
        return float_to_bf16(value)
    raise ValueError(f"unsupported tile floating-point format {format_code}")


def tile_float_is_nan(raw: int, format_code: int) -> bool:
    """Return whether one raw FP16 or BF16 tile lane is a NaN."""

    if format_code == FP16_FORMAT:
        return ((raw >> 10) & 0x1F) == 0x1F and (raw & 0x3FF) != 0
    if format_code == BF16_FORMAT:
        return ((raw >> 7) & 0xFF) == 0xFF and (raw & 0x7F) != 0
    raise ValueError(f"unsupported tile floating-point format {format_code}")


__all__ = [
    "BF16_FORMAT",
    "FP16_FORMAT",
    "bf16_to_float",
    "bits_to_fp32",
    "decode_tile_float",
    "encode_tile_float",
    "float_to_bf16",
    "float_to_fp16",
    "fp16_to_float",
    "fp32_to_bits",
    "pack_fp32_bits",
    "tile_float_is_nan",
]
