#!/usr/bin/env python3
"""Generate the floating-point TAMAC RTL oracle vectors.

The final accumulator images are produced by executing the canonical TAMAC
instruction through the pure-Python emulator.  Twelve deterministic cases
cover FP16, BF16, FP32, and FP64 in tile, broadcast, and in-place source
forms.  In FP16/BF16, boundary lanes 0, 15, 16, and 31 carry the
exact-rounding and IEEE exceptional cases so both 16-lane RTL groups are
exercised.  In FP32 (lanes 0, 7, 8, 15) and FP64 (lanes 0, 3, 4, 7) they sit
on the FMA-unit beat and result-register seams.  Every boundary value is
written out by hand and checked against the emulator.

The output is deliberately a simple whitespace-separated format.  A Verilog
testbench can read one line with:

    %s %d %d %d %d %d %d %h %h %h %h %h

The fields are:

    name ew signed source_form repeats cycles total_cycles scalar
    source_a source_b initial_tacc final_tacc

EW is the architectural TMODE encoding (4=FP16, 5=BF16, 6=FP32, 7=FP64).
signed is always
zero for floating-point TAMAC.  source_form is the TAMAC SS encoding
(0=tile, 1=broadcast, 3=in-place).  cycles is the engine-local cycle count for
one TAMAC and total_cycles includes all repeats.  scalar is the complete
64-bit broadcast GPR, including deliberately poisoned upper bits.
source_a/source_b are the effective 512-bit lane operands:

    source_form 0: source_a=TSRC0, source_b=TSRC1
    source_form 1: source_a=TSRC0, source_b=replicated low scalar element
    source_form 3: source_a=TDST,  source_b=TSRC0

Within every hex token, byte offset zero occupies bits [7:0] (the rightmost
two hex digits).  This makes lane zero the least-significant lane of a Verilog
reg.  TACC images are always the full 2048-bit architectural bank.  FP16/BF16
TAMAC uses the low 128 bytes as 32 binary32 lanes, FP32 the low 128 bytes as
16 binary64 lanes, and FP64 the low 64 bytes as 8 binary64 lanes; every other
byte must remain zero.

Regenerate from the repository root with:

    python3 rtl/sim/gen_tamac_fp_vectors.py > rtl/sim/tamac_fp_vectors.vec
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from asm import assemble  # noqa: E402
from megapad64 import (  # noqa: E402
    EW_BF16,
    EW_FP16,
    EW_FP32,
    EW_FP64,
    TACC_CANONICAL_NAN,
    TACC_IMAGE_BYTES,
    Megapad64,
)
from shared import ieee_fp  # noqa: E402


PROGRAM_ADDR = 0x000
SOURCE_A_ADDR = 0x400
SOURCE_B_ADDR = 0x500
INPLACE_A_ADDR = 0x600
BROADCAST_REG = 7

FORM_TILE = 0
FORM_BROADCAST = 1
FORM_INPLACE = 3

FP_LANES = 32
BROADCAST_POISON = 0xA5A5_5A5A_DEAD_0000
CANONICAL_NAN64 = ieee_fp.FP64.canonical_nan

# (source bytes, accumulator bytes) per floating TACC format.
FORMAT_BYTES = {EW_FP16: (2, 4), EW_BF16: (2, 4), EW_FP32: (4, 8),
                EW_FP64: (8, 8)}


def _source_bytes(ew: int) -> int:
    return FORMAT_BYTES[ew][0]


def _accumulator_bytes(ew: int) -> int:
    return FORMAT_BYTES[ew][1]


def _lane_count(ew: int) -> int:
    return 64 // _source_bytes(ew)


def _active_bytes(ew: int) -> int:
    return _lane_count(ew) * _accumulator_bytes(ew)


def d32(value: float) -> int:
    return ieee_fp.from_double(ieee_fp.FP32, value)


def d64(value: float) -> int:
    return ieee_fp.from_double(ieee_fp.FP64, value)


@dataclass(frozen=True)
class FPCase:
    name: str
    ew: int
    source_form: int
    source_a_values: tuple[int, ...]
    source_b_values: tuple[int, ...]
    initial_values: tuple[int, ...]
    repeats: int
    cycles: int
    expected_boundary_values: tuple[tuple[int, int], ...]


def _lanes(default: int, overrides: dict[int, int],
           count: int = FP_LANES) -> tuple[int, ...]:
    values = [default] * count
    for lane, value in overrides.items():
        if not 0 <= lane < count:
            raise ValueError(f"lane {lane} is outside the FP TAMAC image")
        values[lane] = value
    return tuple(values)


CASES = (
    FPCase(
        name="fp16_tile_exact_special_repeat",
        ew=EW_FP16,
        source_form=FORM_TILE,
        source_a_values=_lanes(
            0x3C00,
            {
                0: 0x3C01,   # Product retains bits beyond rounded FP16.
                15: 0x7E55,  # Source NaN canonicalizes.
                16: 0x0000,  # Zero times infinity is invalid.
                31: 0xFC00,  # -infinity opposes the accumulator.
            },
        ),
        source_b_values=_lanes(
            0x4000,
            {
                0: 0x3C01,
                15: 0x3C00,
                16: 0x7C00,
                31: 0x3C00,
            },
        ),
        initial_values=_lanes(
            0x0000_0000,
            {
                31: 0x7F80_0000,
            },
        ),
        repeats=2,
        cycles=7,
        expected_boundary_values=(
            (0, 0x4000_4008),
            (15, TACC_CANONICAL_NAN),
            (16, TACC_CANONICAL_NAN),
            (31, TACC_CANONICAL_NAN),
        ),
    ),
    FPCase(
        name="fp16_broadcast_subnormal_zero_poison",
        ew=EW_FP16,
        source_form=FORM_BROADCAST,
        source_a_values=_lanes(
            0x4000,
            {
                0: 0x0001,   # Smallest FP16 subnormal.
                15: 0x8000,  # Negative zero product.
                16: 0xBC00,  # Exact cancellation against +1.0.
                31: 0x8001,  # Negative smallest FP16 subnormal.
            },
        ),
        source_b_values=(0x3C00,),
        initial_values=_lanes(
            0xBF80_0000,
            {
                0: 0x0000_0000,
                15: 0x8000_0000,
                16: 0x3F80_0000,
                31: 0x0000_0000,
            },
        ),
        repeats=1,
        cycles=6,
        expected_boundary_values=(
            (0, 0x3380_0000),
            (15, 0x8000_0000),
            (16, 0x0000_0000),
            (31, 0xB380_0000),
        ),
    ),
    FPCase(
        name="fp16_inplace_exception_boundaries",
        ew=EW_FP16,
        source_form=FORM_INPLACE,
        source_a_values=_lanes(
            0x4200,
            {
                0: 0x0000,   # Zero times infinity is invalid.
                15: 0x7E01,  # Source NaN canonicalizes.
                16: 0xFC00,  # -infinity opposes the accumulator.
                31: 0x8000,  # Two negative zero terms retain -zero.
            },
        ),
        source_b_values=_lanes(
            0x3800,
            {
                0: 0x7C00,
                15: 0x3C00,
                16: 0x3C00,
                31: 0x3C00,
            },
        ),
        initial_values=_lanes(
            0x0000_0000,
            {
                16: 0x7F80_0000,
                31: 0x8000_0000,
            },
        ),
        repeats=1,
        cycles=7,
        expected_boundary_values=(
            (0, TACC_CANONICAL_NAN),
            (15, TACC_CANONICAL_NAN),
            (16, TACC_CANONICAL_NAN),
            (31, 0x8000_0000),
        ),
    ),
    FPCase(
        name="bf16_tile_fused_rounding_repeat",
        ew=EW_BF16,
        source_form=FORM_TILE,
        source_a_values=_lanes(
            0x3F80,
            {
                0: 0x0001,   # Exact product is half a binary32 subnormal ULP.
                15: 0x3980,  # Half-ULP tie with an even accumulator.
                16: 0x3980,  # Half-ULP tie with an odd accumulator.
                31: 0x7F7F,  # Largest finite BF16 overflows when doubled.
            },
        ),
        source_b_values=_lanes(
            0x4000,
            {
                0: 0x3700,
                15: 0x3980,
                16: 0x3980,
                31: 0x4000,
            },
        ),
        initial_values=_lanes(
            0x0000_0000,
            {
                0: 0x0000_0001,
                15: 0x3F80_0000,
                16: 0x3F80_0001,
            },
        ),
        repeats=2,
        cycles=7,
        expected_boundary_values=(
            (0, 0x0000_0002),
            (15, 0x3F80_0000),
            (16, 0x3F80_0002),
            (31, 0x7F80_0000),
        ),
    ),
    FPCase(
        name="bf16_broadcast_subnormal_special_poison",
        ew=EW_BF16,
        source_form=FORM_BROADCAST,
        source_a_values=_lanes(
            0x4000,
            {
                0: 0x0001,   # Smallest BF16 subnormal widens exactly.
                15: 0x8000,  # Negative zero product.
                16: 0x7FC1,  # Source NaN canonicalizes.
                31: 0xFF80,  # -infinity opposes the accumulator.
            },
        ),
        source_b_values=(0x3F80,),
        initial_values=_lanes(
            0xBF80_0000,
            {
                0: 0x0000_0000,
                15: 0x8000_0000,
                16: 0x0000_0000,
                31: 0x7F80_0000,
            },
        ),
        repeats=1,
        cycles=6,
        expected_boundary_values=(
            (0, 0x0001_0000),
            (15, 0x8000_0000),
            (16, TACC_CANONICAL_NAN),
            (31, TACC_CANONICAL_NAN),
        ),
    ),
    FPCase(
        name="bf16_inplace_invalid_subnormal_overflow",
        ew=EW_BF16,
        source_form=FORM_INPLACE,
        source_a_values=_lanes(
            0x4040,
            {
                0: 0x0000,   # Zero times infinity is invalid.
                15: 0x8000,  # Two negative zero terms retain -zero.
                16: 0x0001,  # Smallest BF16 subnormal.
                31: 0x7F7F,  # Largest finite BF16 overflows when doubled.
            },
        ),
        source_b_values=_lanes(
            0x3F00,
            {
                0: 0x7F80,
                15: 0x3F80,
                16: 0x3F80,
                31: 0x4000,
            },
        ),
        initial_values=_lanes(
            0x0000_0000,
            {
                15: 0x8000_0000,
            },
        ),
        repeats=1,
        cycles=7,
        expected_boundary_values=(
            (0, TACC_CANONICAL_NAN),
            (15, 0x8000_0000),
            (16, 0x0001_0000),
            (31, 0x7F80_0000),
        ),
    ),
    FPCase(
        name="fp32_tile_exact_special_repeat",
        ew=EW_FP32,
        source_form=FORM_TILE,
        source_a_values=_lanes(
            d32(1.5),
            {
                0: 0x3F80_0001,  # (1 + 2**-23)**2 is exact in binary64.
                7: 0x7FC0_0055,  # Source NaN canonicalizes.
                8: 0x0000_0000,  # Zero times infinity is invalid.
                15: 0xFF80_0000,  # -infinity opposes the accumulator.
            },
            16,
        ),
        source_b_values=_lanes(
            d32(2.0),
            {0: 0x3F80_0001, 7: d32(1.0), 8: 0x7F80_0000, 15: d32(1.0)},
            16,
        ),
        initial_values=_lanes(0, {15: ieee_fp.FP64.infinity}, 16),
        repeats=2,
        cycles=11,
        expected_boundary_values=(
            (0, d64(2.0 + 2.0 ** -21 + 2.0 ** -45)),
            (7, CANONICAL_NAN64),
            (8, CANONICAL_NAN64),
            (15, CANONICAL_NAN64),
        ),
    ),
    FPCase(
        name="fp32_broadcast_subnormal_zero_poison",
        ew=EW_FP32,
        source_form=FORM_BROADCAST,
        source_a_values=_lanes(
            d32(3.0),
            {
                0: 0x0000_0001,  # Smallest binary32 subnormal is normal here.
                7: 0x8000_0000,  # Negative zero product.
                8: d32(-1.0),    # Exact cancellation against +1.0.
                15: d32(2.0),    # Far below a large accumulator, but exact.
            },
            16,
        ),
        source_b_values=(d32(1.0),),
        initial_values=_lanes(
            d64(-1.0),
            {0: 0, 7: ieee_fp.FP64.sign_bit, 8: d64(1.0), 15: d64(2.0 ** 60)},
            16,
        ),
        repeats=1,
        cycles=10,
        expected_boundary_values=(
            (0, d64(2.0 ** -149)),
            (7, ieee_fp.FP64.sign_bit),
            (8, 0),
            (15, d64(2.0 ** 60 + 2.0)),
        ),
    ),
    FPCase(
        name="fp32_inplace_invalid_wide_range",
        ew=EW_FP32,
        source_form=FORM_INPLACE,
        source_a_values=_lanes(
            d32(0.25),
            {
                0: 0x0000_0000,   # Zero times infinity is invalid.
                7: 0x7F7F_FFFF,   # max**2 does not overflow binary64.
                8: d32(1.5),      # Cancels the accumulator exactly.
                15: 0x8000_0000,  # Two negative zero terms retain -zero.
            },
            16,
        ),
        source_b_values=_lanes(
            d32(4.0),
            {0: 0x7F80_0000, 7: 0x7F7F_FFFF, 8: d32(1.5), 15: d32(1.0)},
            16,
        ),
        initial_values=_lanes(
            0, {8: d64(-2.25), 15: ieee_fp.FP64.sign_bit}, 16),
        repeats=1,
        cycles=11,
        expected_boundary_values=(
            (0, CANONICAL_NAN64),
            (7, d64(ieee_fp.to_double(ieee_fp.FP32, 0x7F7F_FFFF) ** 2)),
            (8, 0),
            (15, ieee_fp.FP64.sign_bit),
        ),
    ),
    FPCase(
        name="fp64_tile_fused_overflow_repeat",
        ew=EW_FP64,
        source_form=FORM_TILE,
        source_a_values=_lanes(
            d64(1.25),
            {
                0: d64(1.0 + 2.0 ** -52),  # Fused: the product error survives.
                3: 0x7FF0_0000_0000_0123,  # Signalling NaN canonicalizes.
                4: ieee_fp.FP64.max_finite,  # 2 max - max stays finite, then
                7: 0,                        # 0 x infinity is invalid.
            },
            8,
        ),
        source_b_values=_lanes(
            d64(-2.0),
            {0: d64(1.0 + 2.0 ** -52), 3: d64(1.0), 4: d64(2.0),
             7: ieee_fp.FP64.infinity},
            8,
        ),
        initial_values=_lanes(
            0,
            {0: d64(-(1.0 + 2.0 ** -51)),
             4: ieee_fp.FP64.max_finite | ieee_fp.FP64.sign_bit},
            8,
        ),
        repeats=2,
        cycles=7,
        expected_boundary_values=(
            # 2**-104 after the first TAMAC, then 1 + 2**-51 (+2**-103 lost).
            (0, d64(1.0 + 2.0 ** -51)),
            (3, CANONICAL_NAN64),
            # max after the first TAMAC, then 3 max overflows.
            (4, ieee_fp.FP64.infinity),
            (7, CANONICAL_NAN64),
        ),
    ),
    FPCase(
        name="fp64_broadcast_subnormal_zero",
        ew=EW_FP64,
        source_form=FORM_BROADCAST,
        source_a_values=_lanes(
            d64(3.0),
            {
                0: 1,                          # Smallest binary64 subnormal.
                3: ieee_fp.FP64.sign_bit,      # Negative zero product.
                4: d64(-1.0),                  # Exact cancellation.
                7: d64(0.5),                   # Swallows a subnormal addend.
            },
            8,
        ),
        source_b_values=(d64(1.0),),
        initial_values=_lanes(
            d64(-1.0),
            {0: 0, 3: ieee_fp.FP64.sign_bit, 4: d64(1.0), 7: 1},
            8,
        ),
        repeats=1,
        cycles=6,
        expected_boundary_values=(
            (0, 1),
            (3, ieee_fp.FP64.sign_bit),
            (4, 0),
            (7, d64(0.5)),
        ),
    ),
    FPCase(
        name="fp64_inplace_ties_and_invalid",
        ew=EW_FP64,
        source_form=FORM_INPLACE,
        source_a_values=_lanes(
            d64(0.5),
            {
                0: d64(1.0),                 # 1 + 2**-53 ties to even.
                3: d64(2.0 ** -53),          # The product breaks the tie.
                4: ieee_fp.FP64.sign_bit,    # Two negative zero terms.
                7: ieee_fp.FP64.infinity,    # inf - inf is invalid.
            },
            8,
        ),
        source_b_values=_lanes(
            d64(8.0),
            {0: d64(2.0 ** -53), 3: d64(1.0 + 2.0 ** -52), 4: d64(1.0),
             7: d64(1.0)},
            8,
        ),
        initial_values=_lanes(
            d64(1.0),
            {4: ieee_fp.FP64.sign_bit,
             7: ieee_fp.FP64.infinity | ieee_fp.FP64.sign_bit},
            8,
        ),
        repeats=1,
        cycles=7,
        expected_boundary_values=(
            (0, d64(1.0)),
            (3, d64(1.0 + 2.0 ** -52)),
            (4, ieee_fp.FP64.sign_bit),
            (7, CANONICAL_NAN64),
        ),
    ),
)


def _tile(ew: int, values: tuple[int, ...]) -> bytes:
    if not values:
        raise ValueError("an FP source pattern must contain at least one value")
    width = _source_bytes(ew)
    return b"".join(
        (values[lane % len(values)] & ((1 << (8 * width)) - 1)).to_bytes(
            width,
            "little",
        )
        for lane in range(_lane_count(ew))
    )


def _accumulator_image(ew: int, values: tuple[int, ...]) -> bytes:
    if len(values) != _lane_count(ew):
        raise ValueError(
            f"an FP accumulator image needs {_lane_count(ew)} lanes")
    width = _accumulator_bytes(ew)
    active = b"".join(
        (value & ((1 << (8 * width)) - 1)).to_bytes(width, "little")
        for value in values
    )
    return active + bytes(TACC_IMAGE_BYTES - len(active))


def _scalar(case: FPCase) -> int:
    """The broadcast GPR, with every bit above the lane width poisoned."""
    if case.source_form != FORM_BROADCAST:
        return 0
    bits = 8 * _source_bytes(case.ew)
    lane_mask = (1 << bits) - 1
    return (BROADCAST_POISON & ~lane_mask & ((1 << 64) - 1)) | (
        case.source_b_values[0] & lane_mask)


def _instruction(source_form: int) -> str:
    if source_form == FORM_TILE:
        return "t.amac"
    if source_form == FORM_BROADCAST:
        return f"t.amac r{BROADCAST_REG}"
    if source_form == FORM_INPLACE:
        return "t.amac inplace"
    raise ValueError(f"unsupported source form {source_form}")


def _run_emulator(
    case: FPCase,
    source_a: bytes,
    source_b: bytes,
    initial_tacc: bytes,
) -> bytes:
    cpu = Megapad64(mem_size=4096)
    cpu.tmode = case.ew
    cpu.tacc[:] = initial_tacc
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = case.ew
    cpu.tacc_format_signed = 0
    cpu.tacc_busy = False
    cpu.tacc_force_pending = False

    if case.source_form == FORM_TILE:
        cpu.tsrc0 = SOURCE_A_ADDR
        cpu.tsrc1 = SOURCE_B_ADDR
        cpu.mem[SOURCE_A_ADDR:SOURCE_A_ADDR + 64] = source_a
        cpu.mem[SOURCE_B_ADDR:SOURCE_B_ADDR + 64] = source_b
    elif case.source_form == FORM_BROADCAST:
        cpu.tsrc0 = SOURCE_A_ADDR
        cpu.mem[SOURCE_A_ADDR:SOURCE_A_ADDR + 64] = source_a
        cpu.regs[BROADCAST_REG] = _scalar(case)
    elif case.source_form == FORM_INPLACE:
        cpu.tdst = INPLACE_A_ADDR
        cpu.tsrc0 = SOURCE_B_ADDR
        cpu.mem[INPLACE_A_ADDR:INPLACE_A_ADDR + 64] = source_a
        cpu.mem[SOURCE_B_ADDR:SOURCE_B_ADDR + 64] = source_b
    else:
        raise ValueError(f"unsupported source form {case.source_form}")

    encoded = bytes(assemble(_instruction(case.source_form)))
    cpu.load_bytes(PROGRAM_ADDR, encoded * case.repeats)
    cpu.pc = PROGRAM_ADDR
    observed_cycles = tuple(cpu.step() for _ in range(case.repeats))
    expected_cycles = (case.cycles,) * case.repeats
    if observed_cycles != expected_cycles:
        raise RuntimeError(
            f"{case.name}: emulator cycles {observed_cycles}, "
            f"expected {expected_cycles}"
        )
    return bytes(cpu.tacc)


def _verify_case_images(
    case: FPCase,
    initial_tacc: bytes,
    final_tacc: bytes,
) -> None:
    if len(initial_tacc) != TACC_IMAGE_BYTES:
        raise RuntimeError(f"{case.name}: initial TACC is not 2048 bits")
    if len(final_tacc) != TACC_IMAGE_BYTES:
        raise RuntimeError(f"{case.name}: final TACC is not 2048 bits")
    active = _active_bytes(case.ew)
    width = _accumulator_bytes(case.ew)
    required_inactive = bytes(TACC_IMAGE_BYTES - active)
    if initial_tacc[active:] != required_inactive:
        raise RuntimeError(f"{case.name}: initial inactive TACC bytes are nonzero")
    if final_tacc[active:] != required_inactive:
        raise RuntimeError(f"{case.name}: final inactive TACC bytes are nonzero")

    for lane, expected in case.expected_boundary_values:
        offset = lane * width
        observed = int.from_bytes(
            final_tacc[offset:offset + width],
            "little",
        )
        if observed != expected:
            raise RuntimeError(
                f"{case.name}: lane {lane} is {observed:#010x}, "
                f"expected {expected:#010x}"
            )


def _hex_token(data: bytes) -> str:
    """Encode address/lane byte zero into the least-significant hex byte."""
    return data[::-1].hex()


def _render_case(case: FPCase) -> str:
    if case.ew not in FORMAT_BYTES:
        raise ValueError(f"{case.name}: non-floating TMODE {case.ew}")
    lanes = _lane_count(case.ew)
    if case.source_form == FORM_BROADCAST:
        if len(case.source_b_values) != 1:
            raise ValueError(f"{case.name}: broadcast needs one scalar value")
    elif len(case.source_b_values) != lanes:
        raise ValueError(f"{case.name}: tile source B needs {lanes} lanes")
    if len(case.source_a_values) != lanes:
        raise ValueError(f"{case.name}: tile source A needs {lanes} lanes")

    source_a = _tile(case.ew, case.source_a_values)
    source_b = _tile(case.ew, case.source_b_values)
    initial_tacc = _accumulator_image(case.ew, case.initial_values)
    final_tacc = _run_emulator(case, source_a, source_b, initial_tacc)
    _verify_case_images(case, initial_tacc, final_tacc)
    fields = (
        case.name,
        str(case.ew),
        "0",
        str(case.source_form),
        str(case.repeats),
        str(case.cycles),
        str(case.repeats * case.cycles),
        f"{_scalar(case):016x}",
        _hex_token(source_a),
        _hex_token(source_b),
        _hex_token(initial_tacc),
        _hex_token(final_tacc),
    )
    return " ".join(fields)


def main() -> None:
    print("# Generated by rtl/sim/gen_tamac_fp_vectors.py; do not edit.")
    print(
        "# name ew signed source_form repeats cycles total_cycles scalar "
        "source_a[511:0] source_b[511:0] "
        "initial_tacc[2047:0] final_tacc[2047:0]"
    )
    print("# Byte offset zero is the least-significant byte of every hex token.")
    for case in CASES:
        print(_render_case(case))


if __name__ == "__main__":
    main()
