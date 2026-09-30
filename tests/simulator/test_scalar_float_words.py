"""The hosted scalar floating-point words against the executable machine.

Each hosted word must leave the stack and FPCSR the Python emulator leaves
after executing the ``FC`` operation byte the BIOS word runs
(docs/floating-point.md §11).
"""

from __future__ import annotations

import random
import struct

import pytest

from emulator.megapad64 import CSR_FPCSR
from emulator.megapad64 import Megapad64 as PythonMegapad64
from shared import ieee_fp, scalar_fp
from shared.cells import MASK64
from simulator.runtime import MegaForthRuntime
from simulator.scalar_float import IllegalScalarFloatError


RUP = 3
NX = 1 << 4


def _machine(op: int, rd: int, rs: int, rt: int, fpcsr: int
             ) -> tuple[int, int]:
    cpu = PythonMegapad64(mem_size=0x1000)
    code = bytes([0xFC, op, 0x16])
    if scalar_fp.instruction_length(op) == 4:
        code += bytes([7])
    cpu.load_bytes(0, code)
    cpu.csr_write(CSR_FPCSR, fpcsr)
    cpu.regs[1], cpu.regs[6], cpu.regs[7] = rd, rs, rt
    cpu.pc = 0
    cpu.step()
    return cpu.regs[1], cpu.csr_read(CSR_FPCSR)


def _hosted(name: str, arguments: tuple[int, ...], fpcsr: int
            ) -> tuple[tuple[int, ...], int]:
    runtime = MegaForthRuntime()
    runtime.scalar_float.write_fpcsr(fpcsr)
    for value in arguments:
        runtime.main_context.data.push(value)
    runtime.execute(name, step_budget=10_000)
    return runtime.main_context.data.snapshot(), runtime.scalar_float.fpcsr


def _operand(rng: random.Random, op: int) -> int:
    fmt = ieee_fp.FP64 if op >> 6 else ieee_fp.FP32
    pick = rng.random()
    if pick < 0.2:
        return rng.choice((0, fmt.sign_bit, fmt.infinity, fmt.canonical_nan,
                           fmt.infinity | 1, 1, fmt.max_finite))
    if pick < 0.4:
        return rng.getrandbits(64)
    return ieee_fp.from_double(fmt, rng.choice((-1, 1)) * rng.choice((
        0.5, 1.5, 2.5, 3.0, 1e-3, 1e10, 2.0 ** 40, 65520.0, 1e300)))


@pytest.mark.parametrize(
    ("name", "shape", "op"),
    [word for word in scalar_fp.BIOS_WORDS if word[2] is not None],
    ids=[name for name, _, op in scalar_fp.BIOS_WORDS if op is not None],
)
def test_word_matches_the_machine(name: str, shape: str, op: int) -> None:
    rng = random.Random(name)
    for _ in range(12):
        a, b, c = (_operand(rng, op) for _ in range(3))
        fpcsr = rng.randrange(5)
        if shape == "unary":
            expected = _machine(op, a, a, 0, fpcsr)
            arguments = (a,)
        elif shape == "fma":
            expected = _machine(op, c, a, b, fpcsr)
            arguments = (a, b, c)
        else:
            expected = _machine(op, a, b, 0, fpcsr)
            arguments = (a, b)
        stack, flags = _hosted(name, arguments, fpcsr)
        assert (stack, flags) == ((expected[0],), expected[1]), (
            name, [hex(v) for v in arguments], fpcsr)


def test_fpcsr_words_mask_and_round_trip() -> None:
    assert _hosted("FPCSR!", (MASK64,), 0) == ((), 0x1F7)
    assert _hosted("FPCSR@", (), RUP | NX) == ((RUP | NX,), RUP | NX)


def test_dynamic_words_fail_closed_on_a_reserved_mode() -> None:
    runtime = MegaForthRuntime()
    runtime.scalar_float.write_fpcsr(5)
    one = struct.unpack("<Q", struct.pack("<d", 1.0))[0]
    runtime.main_context.data.push(one)
    runtime.main_context.data.push(one)
    with pytest.raises(IllegalScalarFloatError):
        runtime.execute("F64+", step_budget=10_000)
    assert runtime.scalar_float.fpcsr == 5
    # Fixed-mode words ignore FPCSR.RM.
    runtime = MegaForthRuntime()
    runtime.scalar_float.write_fpcsr(7)
    runtime.main_context.data.push(one)
    runtime.execute("F64TRUNC", step_budget=10_000)
    assert runtime.main_context.data.snapshot() == (one,)


def test_flags_accumulate_across_words() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b"1 S>F64 0 S>F64 F64/ DROP 1 S>F64 3 S>F64 F64/ DROP FPCSR@")
    # DZ from 1/0 and NX from 1/3.
    assert runtime.main_context.data.snapshot() == ((1 << 7) | NX,)
