"""Backend-neutral MegaPad CRC value semantics.

The architectural emulator and hosted simulator have different ownership and
execution models, but the six public CRC modes use the same bit recurrence,
width masking, byte order, and final XOR.  This module contains only those
pure value operations; transaction ownership remains backend-local.
"""

from __future__ import annotations

from types import MappingProxyType
from typing import Final, Mapping

from shared.cells import MASK64, u64
from shared.crypto_caps import CRYPTO_CAP_CRC_REFLECT_RAW


# mode -> (polynomial, width, reflected)
CRC_MODE_PARAMETERS: Mapping[int, tuple[int, int, bool]] = MappingProxyType(
    {
        0: (0x04C11DB7, 32, False),
        1: (0x1EDC6F41, 32, False),
        2: (0x42F0E1EBA9EA3693, 64, False),
        4: (0xEDB88320, 32, True),
        5: (0x82F63B78, 32, True),
        6: (0xC96C5795D7870F42, 64, True),
    }
)
CRC_MODE_IDS: Final = frozenset(CRC_MODE_PARAMETERS)
CRC_REFLECTED_MODE_IDS: Final = frozenset((4, 5, 6))


def _mode_parameters(mode: int) -> tuple[int, int, bool]:
    try:
        return CRC_MODE_PARAMETERS[mode]
    except (KeyError, TypeError) as exc:
        raise ValueError(f"unsupported CRC mode {mode!r}") from exc


def crc_width_mask(mode: int) -> int:
    """Return the accumulator mask selected by one complete mode."""

    _polynomial, width, _reflected = _mode_parameters(mode)
    return MASK64 if width == 64 else 0xFFFF_FFFF


def crc_update_byte(
    accumulator: int,
    byte: int,
    polynomial: int,
    width: int,
    reflected: bool,
) -> int:
    """Process one byte through an explicit complete CRC recurrence."""

    if width not in (32, 64):
        raise ValueError("CRC width must be 32 or 64 bits")
    mask = MASK64 if width == 64 else 0xFFFF_FFFF
    accumulator &= mask
    byte &= 0xFF
    if reflected:
        accumulator ^= byte
        for _ in range(8):
            if accumulator & 1:
                accumulator = (accumulator >> 1) ^ polynomial
            else:
                accumulator >>= 1
    else:
        accumulator ^= byte << (width - 8)
        top = 1 << (width - 1)
        for _ in range(8):
            if accumulator & top:
                accumulator = ((accumulator << 1) & mask) ^ polynomial
            else:
                accumulator = (accumulator << 1) & mask
    return accumulator & mask


def crc_reset_value(mode: int) -> int:
    """Return the selected mode's all-ones initial accumulator."""

    return crc_width_mask(mode)


def crc_seed_value(mode: int, seed: int) -> int:
    """Width-mask an arbitrary guest seed for the selected mode."""

    return u64(seed) & crc_width_mask(mode)


def crc_feed_byte(mode: int, accumulator: int, byte: int) -> int:
    """Feed exactly the low byte into the selected mode."""

    polynomial, width, reflected = _mode_parameters(mode)
    return crc_update_byte(
        accumulator,
        byte,
        polynomial,
        width,
        reflected,
    )


def crc_feed_cell(mode: int, accumulator: int, cell: int) -> int:
    """Feed one cell as eight bytes in least-significant-byte-first order."""

    value = u64(cell)
    result = accumulator
    for index in range(8):
        result = crc_feed_byte(mode, result, value >> (index * 8))
    return result


def crc_raw_value(mode: int, accumulator: int) -> int:
    """Return the mode-width raw accumulator."""

    return u64(accumulator) & crc_width_mask(mode)


def crc_final_value(mode: int, accumulator: int) -> int:
    """Apply the selected mode's all-ones final XOR."""

    mask = crc_width_mask(mode)
    return (u64(accumulator) & mask) ^ mask


__all__ = [
    "CRYPTO_CAP_CRC_REFLECT_RAW",
    "CRC_MODE_IDS",
    "CRC_MODE_PARAMETERS",
    "CRC_REFLECTED_MODE_IDS",
    "crc_feed_byte",
    "crc_feed_cell",
    "crc_final_value",
    "crc_raw_value",
    "crc_reset_value",
    "crc_seed_value",
    "crc_update_byte",
    "crc_width_mask",
]
