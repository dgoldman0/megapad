"""Tile element formats selected by ``TMODE.EW``.

``docs/tile-engine.md`` and ``docs/floating-point.md`` §2 define the codes.
Both Python backends decode the tile format through :func:`decode`, so lane
geometry, float-ness, and the accumulation format come from one table rather
than from per-site width arithmetic.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared import ieee_fp


TILE_BYTES = 64

# TMODE is eight bits wide: writes keep EW [3:0], signed [4], saturate [5],
# and rounding [6]; bit 7 and every higher bit read as zero.
TMODE_EW_MASK = 0x0F
TMODE_WRITE_MASK = 0x7F
TMODE_SIGNED = 0x10
TMODE_SATURATE = 0x20
TMODE_ROUNDING = 0x40

# TCTRL keeps its defined bits, ACC_ACC [0] and ACC_ZERO [1].
TCTRL_ACC_ACC = 0x01
TCTRL_ACC_ZERO = 0x02
TCTRL_WRITE_MASK = TCTRL_ACC_ACC | TCTRL_ACC_ZERO

EW_U8 = 0
EW_U16 = 1
EW_U32 = 2
EW_U64 = 3
EW_FP16 = 4
EW_BF16 = 5
EW_FP32 = 6
EW_FP64 = 7


@dataclass(frozen=True)
class TileFormat:
    """One defined ``TMODE.EW`` format."""

    ew: int
    name: str
    lane_bytes: int
    float_format: ieee_fp.Format | None = None

    @property
    def lanes(self) -> int:
        return TILE_BYTES // self.lane_bytes

    @property
    def lane_bits(self) -> int:
        return self.lane_bytes * 8

    @property
    def lane_mask(self) -> int:
        return (1 << self.lane_bits) - 1

    @property
    def is_float(self) -> bool:
        return self.float_format is not None

    @property
    def accumulation(self) -> ieee_fp.Format | None:
        """The float accumulation format ``A``; None for integer formats."""

        if self.float_format is None:
            return None
        return ieee_fp.accumulation_format(self.float_format)

    @property
    def canonical_nan(self) -> int | None:
        if self.float_format is None:
            return None
        return self.float_format.canonical_nan


FORMATS = (
    TileFormat(EW_U8, "u8", 1),
    TileFormat(EW_U16, "u16", 2),
    TileFormat(EW_U32, "u32", 4),
    TileFormat(EW_U64, "u64", 8),
    TileFormat(EW_FP16, "fp16", 2, ieee_fp.FP16),
    TileFormat(EW_BF16, "bf16", 2, ieee_fp.BF16),
    TileFormat(EW_FP32, "fp32", 4, ieee_fp.FP32),
    TileFormat(EW_FP64, "fp64", 8, ieee_fp.FP64),
)

# Codes 8-15 are reserved.
_BY_CODE: tuple[TileFormat | None, ...] = FORMATS + (None,) * (
    TMODE_EW_MASK + 1 - len(FORMATS)
)


def element_width(tmode: int) -> int:
    """Return the 4-bit ``EW`` code of ``tmode``."""

    return tmode & TMODE_EW_MASK


def decode(tmode: int) -> TileFormat | None:
    """Return the format ``tmode`` selects, or None for a reserved code."""

    return _BY_CODE[tmode & TMODE_EW_MASK]


__all__ = [
    "EW_BF16",
    "EW_FP16",
    "EW_FP32",
    "EW_FP64",
    "EW_U16",
    "EW_U32",
    "EW_U64",
    "EW_U8",
    "FORMATS",
    "TCTRL_ACC_ACC",
    "TCTRL_ACC_ZERO",
    "TCTRL_WRITE_MASK",
    "TILE_BYTES",
    "TMODE_EW_MASK",
    "TMODE_ROUNDING",
    "TMODE_SATURATE",
    "TMODE_SIGNED",
    "TMODE_WRITE_MASK",
    "TileFormat",
    "decode",
    "element_width",
]
