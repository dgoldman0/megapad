#!/usr/bin/env python3
"""Generate the integer TAMAC RTL oracle vectors.

The final accumulator images are produced by executing the canonical TAMAC
instruction through the Phase-1 pure-Python emulator.  The input patterns are
the six adversarial integer cases used by the native/emulator differential
oracle in tests/test_native_mex_oracle.py.

The output is deliberately a simple whitespace-separated format.  A Verilog
testbench can read one line with:

    %s %d %d %d %d %d %d %h %h %h %h %h

The fields are:

    name ew signed source_form repeats cycles total_cycles scalar
    source_a source_b initial_tacc final_tacc

EW is the architectural TMODE encoding (0=U8, 1=U16, 2=U32).
source_form is the TAMAC SS encoding (0=tile, 1=broadcast, 3=in-place).
cycles is the engine-local cycle count for one TAMAC and total_cycles includes
all repeats.  scalar is the complete 64-bit broadcast GPR, including poisoned
upper bits.  source_a/source_b are the effective 512-bit lane operands:

    source_form 0: source_a=TSRC0, source_b=TSRC1
    source_form 1: source_a=TSRC0, source_b=replicated low scalar element
    source_form 3: source_a=TDST,  source_b=TSRC0

Within every hex token, byte offset zero occupies bits [7:0] (the rightmost
two hex digits).  This makes lane zero the least-significant lane of a Verilog
reg.  TACC images are always the full 2048-bit architectural bank, including
the required zeroed inactive half for U32.

Regenerate from the repository root with:

    python3 rtl/sim/gen_tamac_integer_vectors.py \
        > rtl/sim/tamac_integer_vectors.vec
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
    EW_U16,
    EW_U32,
    EW_U8,
    MASK64,
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


@dataclass(frozen=True)
class IntegerCase:
    name: str
    ew: int
    signed: int
    source_form: int
    source_a_values: tuple[int, ...]
    source_b_values: tuple[int, ...]
    initial_values: tuple[int, ...]
    repeats: int
    cycles: int


CASES = (
    IntegerCase(
        name="u8_unsigned_tile_wrap_repeat",
        ew=EW_U8,
        signed=0,
        source_form=FORM_TILE,
        source_a_values=(0xFF, 0x80, 1, 0),
        source_b_values=(0xFF, 2, 0xFF, 0),
        initial_values=(0xFFFF_FFF0, 0x8000_0000, 0xFFFF_FFFF, 0),
        repeats=2,
        cycles=7,
    ),
    IntegerCase(
        name="u8_signed_broadcast_extremes",
        ew=EW_U8,
        signed=1,
        source_form=FORM_BROADCAST,
        source_a_values=(0x80, 0x7F, 0xFF, 1),
        source_b_values=(0x80,),
        initial_values=(0x8000_0000, 0x7FFF_FFFF, 1, 0xFFFF_FFFF),
        repeats=1,
        cycles=6,
    ),
    IntegerCase(
        name="u16_unsigned_broadcast_wrap_repeat",
        ew=EW_U16,
        signed=0,
        source_form=FORM_BROADCAST,
        source_a_values=(0xFFFF, 0x8000, 1, 0),
        source_b_values=(0xFFFF,),
        initial_values=(MASK64 - 0xF, 0, 1, MASK64),
        repeats=2,
        cycles=4,
    ),
    IntegerCase(
        name="u16_signed_tile_extremes",
        ew=EW_U16,
        signed=1,
        source_form=FORM_TILE,
        source_a_values=(0x8000, 0x7FFF, 0xFFFF, 1),
        source_b_values=(0xFFFF, 2, 3, 0x8000),
        initial_values=(MASK64, 0x7FFF_FFFF_FFFF_FFFF, 1, 0),
        repeats=1,
        cycles=5,
    ),
    IntegerCase(
        name="u32_unsigned_tile_wrap_repeat",
        ew=EW_U32,
        signed=0,
        source_form=FORM_TILE,
        source_a_values=(0xFFFF_FFFF, 0x8000_0000, 1, 0),
        source_b_values=(0xFFFF_FFFF, 2, 0xFFFF_FFFF, 0),
        initial_values=(MASK64, 0x8000_0000_0000_0000, 1, 0),
        repeats=2,
        cycles=4,
    ),
    IntegerCase(
        name="u32_signed_inplace_extremes",
        ew=EW_U32,
        signed=1,
        source_form=FORM_INPLACE,
        source_a_values=(0x8000_0000, 0x7FFF_FFFF, 0xFFFF_FFFF, 1),
        source_b_values=(0xFFFF_FFFF, 2, 3, 0x8000_0000),
        initial_values=(MASK64, 0x7FFF_FFFF_FFFF_FFFF, 1, 0),
        repeats=1,
        cycles=4,
    ),
)


def _element_bytes(ew: int) -> int:
    return 1 << ew


def _tile(ew: int, values: tuple[int, ...]) -> bytes:
    element_bytes = _element_bytes(ew)
    lane_count = 64 // element_bytes
    mask = (1 << (element_bytes * 8)) - 1
    return b"".join(
        (values[lane % len(values)] & mask).to_bytes(element_bytes, "little")
        for lane in range(lane_count)
    )


def _accumulator_image(ew: int, values: tuple[int, ...]) -> bytes:
    accumulator_bytes = 4 if ew == EW_U8 else 8
    lane_count = 64 // _element_bytes(ew)
    mask = (1 << (accumulator_bytes * 8)) - 1
    active = b"".join(
        (values[lane % len(values)] & mask).to_bytes(
            accumulator_bytes,
            "little",
        )
        for lane in range(lane_count)
    )
    return active + bytes(TACC_IMAGE_BYTES - len(active))


def _scalar(case: IntegerCase) -> int:
    if case.source_form != FORM_BROADCAST:
        return 0
    source_bits = 8 << case.ew
    source_mask = (1 << source_bits) - 1
    return 0xA5A5_5A5A_0000_0000 | (case.source_b_values[0] & source_mask)


def _instruction(source_form: int) -> str:
    if source_form == FORM_TILE:
        return "t.amac"
    if source_form == FORM_BROADCAST:
        return f"t.amac r{BROADCAST_REG}"
    if source_form == FORM_INPLACE:
        return "t.amac inplace"
    raise ValueError(f"unsupported source form {source_form}")


def _run_emulator(
    case: IntegerCase,
    source_a: bytes,
    source_b: bytes,
    initial_tacc: bytes,
) -> bytes:
    cpu = Megapad64(mem_size=4096)
    cpu.tmode = case.ew | (case.signed << 4)
    cpu.tacc[:] = initial_tacc
    cpu.tacc_owner = cpu.core_id
    cpu.tacc_valid = True
    cpu.tacc_dirty = True
    cpu.tacc_format_ew = case.ew
    cpu.tacc_format_signed = case.signed
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


def _hex_token(data: bytes) -> str:
    """Encode address/lane byte zero into the least-significant hex byte."""
    return data[::-1].hex()


def _render_case(case: IntegerCase) -> str:
    source_a = _tile(case.ew, case.source_a_values)
    source_b = _tile(case.ew, case.source_b_values)
    initial_tacc = _accumulator_image(case.ew, case.initial_values)
    final_tacc = _run_emulator(case, source_a, source_b, initial_tacc)
    fields = (
        case.name,
        str(case.ew),
        str(case.signed),
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
    print("# Generated by rtl/sim/gen_tamac_integer_vectors.py; do not edit.")
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
