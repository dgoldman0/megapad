#!/usr/bin/env python3
"""Generate the floating-point TAMAC RTL oracle vectors.

The final accumulator images are produced by executing the canonical TAMAC
instruction through the Phase-1 pure-Python emulator.  Six deterministic cases
cover FP16 and BF16 in tile, broadcast, and in-place source forms.  Boundary
lanes 0, 15, 16, and 31 carry the exact-rounding and IEEE exceptional cases so
both 16-lane RTL groups are exercised.

The output is deliberately a simple whitespace-separated format.  A Verilog
testbench can read one line with:

    %s %d %d %d %d %d %d %h %h %h %h %h

The fields are:

    name ew signed source_form repeats cycles total_cycles scalar
    source_a source_b initial_tacc final_tacc

EW is the architectural TMODE encoding (4=FP16, 5=BF16).  signed is always
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
reg.  TACC images are always the full 2048-bit architectural bank.  FP TAMAC
uses the low 128 bytes as 32 binary32 lanes and requires the high 128 bytes to
remain zero.

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
    TACC_CANONICAL_NAN,
    TACC_IMAGE_BYTES,
    Megapad64,
)


PROGRAM_ADDR = 0x000
SOURCE_A_ADDR = 0x400
SOURCE_B_ADDR = 0x500
INPLACE_A_ADDR = 0x600
BROADCAST_REG = 7

FORM_TILE = 0
FORM_BROADCAST = 1
FORM_INPLACE = 3

FP_LANES = 32
FP_SOURCE_BYTES = 2
FP_ACCUMULATOR_BYTES = 4
FP_ACTIVE_TACC_BYTES = FP_LANES * FP_ACCUMULATOR_BYTES
BROADCAST_POISON = 0xA5A5_5A5A_DEAD_0000


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


def _lanes(default: int, overrides: dict[int, int]) -> tuple[int, ...]:
    values = [default] * FP_LANES
    for lane, value in overrides.items():
        if not 0 <= lane < FP_LANES:
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
)


def _tile(values: tuple[int, ...]) -> bytes:
    if not values:
        raise ValueError("an FP source pattern must contain at least one value")
    return b"".join(
        (values[lane % len(values)] & 0xFFFF).to_bytes(
            FP_SOURCE_BYTES,
            "little",
        )
        for lane in range(FP_LANES)
    )


def _accumulator_image(values: tuple[int, ...]) -> bytes:
    if len(values) != FP_LANES:
        raise ValueError(f"an FP accumulator image needs {FP_LANES} lanes")
    active = b"".join(
        (value & 0xFFFF_FFFF).to_bytes(FP_ACCUMULATOR_BYTES, "little")
        for value in values
    )
    return active + bytes(TACC_IMAGE_BYTES - len(active))


def _scalar(case: FPCase) -> int:
    if case.source_form != FORM_BROADCAST:
        return 0
    return BROADCAST_POISON | (case.source_b_values[0] & 0xFFFF)


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
    inactive_bytes = TACC_IMAGE_BYTES - FP_ACTIVE_TACC_BYTES
    required_inactive = bytes(inactive_bytes)
    if initial_tacc[FP_ACTIVE_TACC_BYTES:] != required_inactive:
        raise RuntimeError(f"{case.name}: initial inactive TACC bytes are nonzero")
    if final_tacc[FP_ACTIVE_TACC_BYTES:] != required_inactive:
        raise RuntimeError(f"{case.name}: final inactive TACC bytes are nonzero")

    for lane, expected in case.expected_boundary_values:
        offset = lane * FP_ACCUMULATOR_BYTES
        observed = int.from_bytes(
            final_tacc[offset:offset + FP_ACCUMULATOR_BYTES],
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
    if case.ew not in (EW_FP16, EW_BF16):
        raise ValueError(f"{case.name}: non-floating TMODE {case.ew}")
    if case.source_form == FORM_BROADCAST:
        if len(case.source_b_values) != 1:
            raise ValueError(f"{case.name}: broadcast needs one scalar value")
    elif len(case.source_b_values) != FP_LANES:
        raise ValueError(f"{case.name}: tile source B needs {FP_LANES} lanes")
    if len(case.source_a_values) != FP_LANES:
        raise ValueError(f"{case.name}: tile source A needs {FP_LANES} lanes")

    source_a = _tile(case.source_a_values)
    source_b = _tile(case.source_b_values)
    initial_tacc = _accumulator_image(case.initial_values)
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
