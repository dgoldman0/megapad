"""Scalar floating-point engine (``FC``) and ``FPCSR`` on the Python emulator.

docs/floating-point.md §8–§10 define the engine.  Hand values below are
derived from host binary64 arithmetic or by hand, not from the oracle; the
randomized sweep then checks every operation's plumbing against the oracle
functions it names.
"""

from __future__ import annotations

import math
import random
import struct

import pytest

from asm import assemble
from megapad64 import (
    CSR_FPCSR,
    IVEC_ILLEGAL_OP,
    Megapad64,
    Megapad64Micro,
    TrapError,
)
from shared import ieee_fp, scalar_fp
from shared.ieee_fp import BF16, FP16, FP32, FP64
from system import MicroCluster


MASK64 = (1 << 64) - 1
NX, UF, OF, DZ, NV = (1 << bit for bit in range(4, 9))
RNE, RTZ, RDN, RUP, RMM = range(5)


def d64(value: float) -> int:
    return struct.unpack("<Q", struct.pack("<d", value))[0]


def d32(value: float) -> int:
    return struct.unpack("<I", struct.pack("<f", value))[0]


def up64(value: float) -> int:
    return d64(math.nextafter(value, math.inf))


def run(source: str, *, fpcsr: int = 0, cpu: Megapad64 | None = None,
        **registers: int) -> Megapad64:
    """Assemble ``source`` at 0, set registers, and step every instruction."""

    cpu = cpu or Megapad64(mem_size=0x1000)
    code = assemble(source)
    cpu.load_bytes(0, code)
    cpu.csr_write(CSR_FPCSR, fpcsr)
    for name, value in registers.items():
        cpu.regs[int(name[1:])] = value & MASK64
    cpu.pc = 0
    while cpu.pc < len(code):
        cpu.step()
    return cpu


def result(source: str, **kwargs) -> tuple[int, int]:
    """Run one instruction whose destination is R1; return (R1, FPCSR)."""

    cpu = run(source, **kwargs)
    return cpu.regs[1], cpu.csr_read(CSR_FPCSR)


# ---------------------------------------------------------------------------
# Hand values
# ---------------------------------------------------------------------------

@pytest.mark.parametrize(("mode", "expected"), (
    (RNE, d64(1 / 3)),
    (RTZ, d64(1 / 3)),
    (RDN, d64(1 / 3)),
    (RUP, up64(1 / 3)),
    (RMM, d64(1 / 3)),
))
def test_divide_rounds_in_the_dynamic_mode(mode: int, expected: int) -> None:
    assert result("fdiv.d r1, r6", fpcsr=mode, r1=d64(1.0), r6=d64(3.0)) == (
        expected, mode | NX)


def test_square_root_is_correctly_rounded_and_keeps_negative_zero() -> None:
    assert result("fsqrt.d r1, r6", r6=d64(2.0)) == (d64(math.sqrt(2.0)), NX)
    assert result("fsqrt.s r1, r6", r6=d32(4.0)) == (d32(2.0), 0)
    assert result("fsqrt.d r1, r6", r6=d64(-0.0)) == (d64(-0.0), 0)
    assert result("fsqrt.d r1, r6", r6=d64(-1.0)) == (FP64.canonical_nan, NV)


def test_single_add_ties_to_even_or_rounds_up() -> None:
    tiny = d32(2.0 ** -24)
    assert result("fadd.s r1, r6", r1=d32(1.0), r6=tiny) == (d32(1.0), NX)
    assert result("fadd.s r1, r6", fpcsr=RUP, r1=d32(1.0), r6=tiny) == (
        d32(1.0 + 2.0 ** -23), RUP | NX)


def test_exact_cancellation_sign_follows_the_rounding_direction() -> None:
    assert result("fsub.d r1, r6", r1=d64(1.5), r6=d64(1.5)) == (0, 0)
    assert result("fsub.d r1, r6", fpcsr=RDN, r1=d64(1.5), r6=d64(1.5)) == (
        d64(-0.0), RDN)


def test_overflow_and_divide_by_zero_flags() -> None:
    big = FP64.max_finite
    assert result("fmul.d r1, r6", r1=big, r6=d64(2.0)) == (
        FP64.infinity, OF | NX)
    assert result("fmul.d r1, r6", fpcsr=RTZ, r1=big, r6=d64(2.0)) == (
        big, RTZ | OF | NX)
    assert result("fdiv.d r1, r6", r1=d64(-1.0), r6=0) == (
        FP64.infinity | FP64.sign_bit, DZ)
    assert result("fdiv.d r1, r6", r1=0, r6=0) == (FP64.canonical_nan, NV)


def test_underflow_needs_tiny_and_inexact() -> None:
    # 2**-1074 * 0.5 ties to zero: tiny and inexact.
    assert result("fmul.d r1, r6", r1=1, r6=d64(0.5)) == (0, UF | NX)
    # 2**-1073 * 0.5 is the exact subnormal 2**-1074: no flag.
    assert result("fmul.d r1, r6", r1=2, r6=d64(0.5)) == (1, 0)


def test_fused_multiply_add_and_subtract_round_once() -> None:
    x = d64(1.0 + 2.0 ** -27)
    # x*x = 1 + 2**-26 + 2**-54; adding -(1 + 2**-26) leaves 2**-54 exactly.
    assert result("fma.d r1, r6, r7", r1=d64(-(1.0 + 2.0 ** -26)),
                  r6=x, r7=x) == (d64(2.0 ** -54), 0)
    # FMS: Rd - Rs*Rt.
    assert result("fms.d r1, r6, r7", r1=d64(1.0 + 2.0 ** -26),
                  r6=x, r7=x) == (d64(-(2.0 ** -54)), 0)
    assert result("fms.s r1, r6, r7", r1=d32(10.0), r6=d32(2.0),
                  r7=d32(3.0)) == (d32(4.0), 0)


def test_propagating_minimum_and_maximum() -> None:
    neg, pos = d64(-0.0), d64(0.0)
    assert result("fmin.d r1, r6", r1=pos, r6=neg) == (neg, 0)
    assert result("fmax.d r1, r6", r1=neg, r6=pos) == (pos, 0)
    quiet = FP64.canonical_nan | FP64.sign_bit | 5
    assert result("fmin.d r1, r6", r1=d64(1.0), r6=quiet) == (
        FP64.canonical_nan, 0)
    signalling = FP64.infinity | 1
    assert result("fmax.d r1, r6", r1=d64(1.0), r6=signalling) == (
        FP64.canonical_nan, NV)


@pytest.mark.parametrize(("bits", "bit"), (
    (FP64.infinity | FP64.sign_bit, 0),
    (d64(-1.0), 1),
    (1 | FP64.sign_bit, 2),
    (FP64.sign_bit, 3),
    (0, 4),
    (1, 5),
    (d64(1.0), 6),
    (FP64.infinity, 7),
    (FP64.infinity | 1, 8),
    (FP64.canonical_nan, 9),
))
def test_class_mask_is_one_hot(bits: int, bit: int) -> None:
    assert result("fclass.d r1, r6", r6=bits) == (1 << bit, 0)


@pytest.mark.parametrize(("mnemonic", "value", "expected"), (
    ("fcvt.l.d.rne", 2.5, 2),
    ("fcvt.l.d.rmm", 2.5, 3),
    ("fcvt.l.d.rtz", 2.5, 2),
    ("fcvt.l.d.rup", 2.5, 3),
    ("fcvt.l.d.rdn", -2.5, -3),
    ("fcvt.l.d.rtz", -2.5, -2),
))
def test_float_to_integer_modes(mnemonic: str, value: float,
                                expected: int) -> None:
    assert result(f"{mnemonic} r1, r6", r6=d64(value)) == (
        expected & MASK64, NX)


def test_float_to_integer_saturates_and_flags_invalid() -> None:
    assert result("fcvt.l.d.rtz r1, r6", r6=d64(1e19)) == ((1 << 63) - 1, NV)
    assert result("fcvt.l.d.rtz r1, r6", r6=d64(-1e19)) == (1 << 63, NV)
    assert result("fcvt.l.d.rtz r1, r6", r6=FP64.canonical_nan) == (0, NV)
    assert result("fcvt.lu.d.rtz r1, r6", r6=d64(-1.0)) == (0, NV)
    # -0.25 rounds to zero, which is in range: inexact only.
    assert result("fcvt.lu.d.rtz r1, r6", r6=d64(-0.25)) == (0, NX)
    assert result("fcvt.lu.s.rtz r1, r6", r6=d32(2.0 ** 63)) == (1 << 63, 0)


def test_integer_to_float_conversions() -> None:
    assert result("fcvt.d.l r1, r6", r6=-1) == (d64(-1.0), 0)
    assert result("fcvt.d.lu r1, r6", r6=MASK64) == (d64(2.0 ** 64), NX)
    assert result("fcvt.s.l r1, r6", r6=(1 << 24) + 1) == (d32(2.0 ** 24), NX)
    assert result("fcvt.s.l r1, r6", fpcsr=RUP, r6=(1 << 24) + 1) == (
        d32(2.0 ** 24 + 2.0), RUP | NX)


def test_format_conversions() -> None:
    assert result("fcvt.s.d r1, r6", r6=d64(0.1)) == (d32(0.1), NX)
    assert result("fcvt.d.s r1, r6", r6=d32(0.1)) == (
        d64(struct.unpack("<f", struct.pack("<f", 0.1))[0]), 0)
    assert result("fcvt.h.s r1, r6", r6=d32(65520.0)) == (0x7C00, OF | NX)
    # Rounded toward zero, 65520 is the finite 65504: inexact, no overflow.
    assert result("fcvt.h.s r1, r6", fpcsr=RTZ, r6=d32(65520.0)) == (
        0x7BFF, RTZ | NX)
    assert result("fcvt.s.h r1, r6", r6=0xFFFF_3C00) == (d32(1.0), 0)
    assert result("fcvt.d.b r1, r6", r6=0x3F80) == (d64(1.0), 0)
    assert result("fcvt.b.d r1, r6", r6=d64(1.0)) == (0x3F80, 0)
    assert result("fcvt.d.h r1, r6", r6=0x7C01) == (FP64.canonical_nan, NV)


def test_round_to_integral_raises_no_inexact() -> None:
    assert result("frnd.d.rne r1, r6", r6=d64(2.5)) == (d64(2.0), 0)
    assert result("frnd.d.rup r1, r6", r6=d64(2.25)) == (d64(3.0), 0)
    assert result("frnd.d.rne r1, r6", r6=d64(-0.4)) == (d64(-0.0), 0)
    assert result("frnd.s.rdn r1, r6", r6=d32(-0.5)) == (d32(-1.0), 0)
    assert result("frnd.d r1, r6", fpcsr=RTZ, r6=d64(-2.75)) == (
        d64(-2.0), RTZ)
    assert result("frnd.d.rne r1, r6", r6=FP64.infinity | 1) == (
        FP64.canonical_nan, NV)


# ---------------------------------------------------------------------------
# Register, FLAGS, and FPCSR rules
# ---------------------------------------------------------------------------

def test_single_inputs_ignore_and_results_clear_the_high_word() -> None:
    garbage = 0xDEAD_BEEF << 32
    assert result("fadd.s r1, r6", r1=garbage | d32(1.0),
                  r6=garbage | d32(2.0)) == (d32(3.0), 0)
    assert result("feq.s r1, r6", r1=garbage | d32(1.0), r6=d32(1.0)) == (
        MASK64, 0)
    assert result("fclass.s r1, r6", r6=garbage | d32(-1.0)) == (1 << 1, 0)


def test_compare_writes_full_masks_and_flags_invalid_by_kind() -> None:
    one, two = d64(1.0), d64(2.0)
    quiet, signalling = FP64.canonical_nan, FP64.infinity | 1
    assert result("flt.d r1, r6", r1=one, r6=two) == (MASK64, 0)
    assert result("fle.d r1, r6", r1=two, r6=two) == (MASK64, 0)
    assert result("feq.d r1, r6", r1=d64(-0.0), r6=0) == (MASK64, 0)
    assert result("feq.d r1, r6", r1=one, r6=quiet) == (0, 0)
    assert result("feq.d r1, r6", r1=one, r6=signalling) == (0, NV)
    assert result("flt.d r1, r6", r1=one, r6=quiet) == (0, NV)
    assert result("fle.d r1, r6", r1=quiet, r6=one) == (0, NV)


@pytest.mark.parametrize(("left", "right", "z", "g", "n", "v", "fpcsr"), (
    (1.0, 2.0, 0, 0, 1, 0, 0),
    (2.0, 1.0, 0, 1, 0, 0, 0),
    (-0.0, 0.0, 1, 0, 0, 0, 0),
    (1.0, math.nan, 0, 0, 0, 1, 0),
))
def test_fcmp_sets_z_g_n_v_and_clears_c_p(left, right, z, g, n, v,
                                          fpcsr) -> None:
    cpu = Megapad64(mem_size=0x1000)
    cpu.flags_unpack(0xFF)  # every flag set, including S and I
    cpu = run("fcmp.d r1, r6", cpu=cpu, r1=d64(left), r6=d64(right))
    assert (cpu.flag_z, cpu.flag_g, cpu.flag_n, cpu.flag_v) == (z, g, n, v)
    assert (cpu.flag_c, cpu.flag_p, cpu.flag_s, cpu.flag_i) == (0, 0, 1, 1)
    assert cpu.regs[1] == d64(left)
    assert cpu.csr_read(CSR_FPCSR) == fpcsr


def test_fcmp_is_quiet_except_for_signalling_nan() -> None:
    cpu = run("fcmp.s r1, r6", r1=d32(1.0), r6=FP32.infinity | 1)
    assert cpu.flag_v == 1
    assert cpu.csr_read(CSR_FPCSR) == NV


def test_other_operations_leave_flags_unchanged() -> None:
    cpu = Megapad64(mem_size=0x1000)
    cpu.flags_unpack(0xA5)
    cpu = run("fadd.d r1, r6\nfeq.d r1, r6\nfcvt.l.d r1, r6", cpu=cpu,
              r1=d64(1.0), r6=d64(2.0))
    assert cpu.flags_pack() == 0xA5


def test_fpcsr_masks_writes_and_accumulates_sticky_flags() -> None:
    cpu = Megapad64(mem_size=0x1000)
    cpu.csr_write(CSR_FPCSR, MASK64)
    assert cpu.csr_read(CSR_FPCSR) == 0x1F7
    cpu = run("fdiv.d r1, r6\nfadd.d r7, r8", r1=d64(1.0), r6=0,
              r7=d64(1.0), r8=d64(2.0 ** -60))
    # DZ from the divide stays set after the inexact add.
    assert cpu.csr_read(CSR_FPCSR) == DZ | NX
    cpu._reset_state()
    assert cpu.csr_read(CSR_FPCSR) == 0


def test_csr_instructions_reach_fpcsr() -> None:
    cpu = run("csrw 0x0D, r6\nfadd.d r7, r8\ncsrr r1, 0x0D",
              r6=RUP, r7=d64(1.0), r8=d64(2.0 ** -60))
    assert cpu.regs[7] == up64(1.0)
    assert cpu.regs[1] == RUP | NX


@pytest.mark.parametrize("source", (
    "fadd.d r1, r6", "fsqrt.s r1, r6", "fma.d r1, r6, r7", "fcvt.d.l r1, r6",
    "fcvt.d.s r1, r6", "fcvt.h.d r1, r6", "frnd.d r1, r6",
    "fcvt.l.s.dyn r1, r6",
))
@pytest.mark.parametrize("reserved", (5, 6, 7))
def test_dynamic_mode_traps_on_reserved_rm(source: str, reserved: int) -> None:
    with pytest.raises(TrapError) as trap:
        run(source, fpcsr=reserved)
    assert trap.value.ivec_id == IVEC_ILLEGAL_OP


@pytest.mark.parametrize("source", (
    "fmin.d r1, r6", "fcmp.d r1, r6", "fclass.s r1, r6", "fcvt.d.h r1, r6",
    "fcvt.s.b r1, r6", "frnd.d.rne r1, r6", "fcvt.lu.d.rtz r1, r6",
))
def test_operations_without_dynamic_rounding_ignore_reserved_rm(
        source: str) -> None:
    run(source, fpcsr=7)


# ---------------------------------------------------------------------------
# Encoding, traps, and timing
# ---------------------------------------------------------------------------

def _trap_pc(code: bytes) -> int:
    cpu = Megapad64(mem_size=0x1000)
    cpu.load_bytes(0, code)
    cpu.pc = 0
    with pytest.raises(TrapError) as trap:
        cpu.step()
    assert trap.value.ivec_id == IVEC_ILLEGAL_OP
    return cpu.pc


@pytest.mark.parametrize("op", (
    [0x80, 0xC0, 0x87]
    + list(range(0x09, 0x10))
    + list(range(0x15, 0x20))
    + [0x3F, 0x25, 0x26, 0x2D, 0x2E, 0x35, 0x76]
))
def test_reserved_encodings_trap_at_the_end_of_the_instruction(op: int
                                                                ) -> None:
    length = scalar_fp.instruction_length(op)
    assert _trap_pc(bytes([0xFC, op, 0x12, 0x03][:length])) == length


def test_nonzero_t_high_bits_trap() -> None:
    assert _trap_pc(bytes([0xFC, 0x47, 0x12, 0x23])) == 4


@pytest.mark.parametrize("prefix", (0xF7, 0xFD, 0xFE, 0xFF))
def test_unassigned_prefixes_trap_instead_of_latching(prefix: int) -> None:
    assert _trap_pc(bytes([prefix, 0x11])) == 1


def test_rex_extends_rd_and_rs_and_rt_is_five_bits() -> None:
    code = assemble("fma.d r17, r18, r19")
    assert code == bytes([0xF3, 0xFC, 0x47, 0x12, 0x13])
    cpu = run("fma.d r17, r18, r19", r17=d64(1.0), r18=d64(2.0),
              r19=d64(3.0))
    assert cpu.regs[17] == d64(7.0)


@pytest.mark.parametrize(("source", "cycles"), (
    ("fadd.s r1, r6", 4),
    ("fma.d r1, r6, r7", 4),
    ("fcvt.l.d.rtz r1, r6", 4),
    ("frnd.s.rne r1, r6", 4),
    ("fmin.d r1, r6", 2),
    ("fcmp.s r1, r6", 2),
    ("fclass.d r1, r6", 2),
    ("fdiv.s r1, r6", 16),
    ("fsqrt.s r1, r6", 16),
    ("fdiv.d r1, r6", 31),
    ("fsqrt.d r1, r6", 31),
    ("fadd.d r17, r6", 5),
))
def test_cycle_costs(source: str, cycles: int) -> None:
    cpu = Megapad64(mem_size=0x1000)
    cpu.load_bytes(0, assemble(source))
    cpu.regs[6] = d64(4.0)
    cpu.pc = 0
    assert cpu.step() == cycles


@pytest.mark.parametrize("source", (
    "fma.d r1, r6, r7", "fadd.d r17, r18", "fsqrt.d r1, r6",
))
def test_skip_steps_over_the_complete_instruction(source: str) -> None:
    body = assemble(source)
    code = assemble("skip") + body + assemble("inc r5")
    cpu = Megapad64(mem_size=0x1000)
    cpu.load_bytes(0, code)
    cpu.pc = 0
    cpu.step()
    assert cpu.pc == 2 + len(body)


def test_standalone_microcore_traps_after_the_whole_instruction() -> None:
    core = Megapad64Micro(mem_size=0x1000, core_id=4)
    core.load_bytes(0, assemble("fma.d r1, r6, r7"))
    core.pc = 0
    with pytest.raises(TrapError) as trap:
        core.step()
    assert trap.value.ivec_id == IVEC_ILLEGAL_OP
    assert core.pc == 4


def test_cluster_microcores_share_the_unit_and_keep_private_fpcsr() -> None:
    memory = bytearray(4096)
    cluster = MicroCluster(cluster_id=0, id_base=4, shared_mem=memory,
                           mem_size=len(memory))
    cluster.set_enabled(True)
    first, second = cluster.cores[:2]
    memory[0:3] = assemble("fdiv.d r1, r6")
    first.csr_write(CSR_FPCSR, RUP)
    first.regs[1], first.regs[6] = d64(1.0), d64(3.0)
    first.pc = 0
    assert first.step() == 1 + 30 + 3
    assert first.regs[1] == up64(1 / 3)
    assert first.csr_read(CSR_FPCSR) == RUP | NX
    assert second.csr_read(CSR_FPCSR) == 0
    second.regs[1], second.regs[6] = d64(1.0), d64(3.0)
    second.pc = 0
    second.step()
    assert second.regs[1] == d64(1 / 3)
    assert second.csr_read(CSR_FPCSR) == NX


# ---------------------------------------------------------------------------
# Randomized plumbing check against the named oracle functions
# ---------------------------------------------------------------------------

def _operand(rng: random.Random, fmt) -> int:
    pick = rng.random()
    if pick < 0.25:
        return rng.choice((
            0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
            fmt.canonical_nan, fmt.infinity | 1, 1, fmt.max_finite,
            1 << fmt.fraction_bits, (1 << fmt.fraction_bits) - 1,
        ))
    if pick < 0.6:
        return rng.getrandbits(fmt.width)
    return ieee_fp.from_double(fmt, rng.choice((-1, 1)) * rng.choice((
        0.5, 1.5, 2.5, 3.0, 1e-3, 1e10, 2.0 ** 40, 7.25, 1e300, 1e-300)))


def _expected(op: int, d: int, s: int, t: int, rm: int) -> tuple[int, int]:
    fmt = FP64 if op >> 6 else FP32
    code = op & 0x3F
    d, s, t = d & fmt.mask, s & fmt.mask, t & fmt.mask
    table = {
        0x00: lambda: ieee_fp.add(fmt, d, s, rm),
        0x01: lambda: ieee_fp.sub(fmt, d, s, rm),
        0x02: lambda: ieee_fp.mul(fmt, d, s, rm),
        0x03: lambda: ieee_fp.div(fmt, d, s, rm),
        0x04: lambda: ieee_fp.sqrt(fmt, s, rm),
        0x07: lambda: ieee_fp.fma(fmt, s, t, d, rm),
        0x08: lambda: ieee_fp.fma(fmt, s ^ fmt.sign_bit, t, d, rm),
        0x38: lambda: ieee_fp.from_int(fmt, s - ((s >> 63) << 64)
                                       if fmt is FP64 else s, rm),
        0x3B: lambda: ieee_fp.convert(FP16, fmt, s, rm),
        0x3D: lambda: ieee_fp.convert(BF16, fmt, s, rm),
    }
    bits, flags = table[code]()
    return bits, flags << 4


@pytest.mark.parametrize("op", (
    0x00, 0x01, 0x02, 0x03, 0x04, 0x07, 0x08, 0x3B, 0x3D,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x47, 0x48, 0x7B, 0x7D,
))
def test_random_operands_match_the_oracle(op: int) -> None:
    rng = random.Random(0xFC00 + op)
    fmt = FP64 if op >> 6 else FP32
    for _ in range(40):
        d, s, t = (_operand(rng, fmt) for _ in range(3))
        rm = rng.randrange(5)
        code = bytes([0xFC, op, 0x16]) + (
            bytes([7]) if scalar_fp.instruction_length(op) == 4 else b"")
        cpu = Megapad64(mem_size=0x1000)
        cpu.load_bytes(0, code)
        cpu.csr_write(CSR_FPCSR, rm)
        cpu.regs[1], cpu.regs[6], cpu.regs[7] = d, s, t
        cpu.pc = 0
        cpu.step()
        assert (cpu.regs[1], cpu.csr_read(CSR_FPCSR)) == (
            _expected(op, d, s, t, rm)[0], rm | _expected(op, d, s, t, rm)[1])


# ---------------------------------------------------------------------------
# Native accelerator: FPCSR is native state and EXT.FP falls back exactly
# ---------------------------------------------------------------------------

from accel_wrapper import Megapad64 as NativeMegapad64  # noqa: E402

_PROGRAM = """
csrw 0x0D, r5
fdiv.d r1, r6
fma.d r1, r6, r7
fcmp.d r1, r6
fcvt.l.d.rne r8, r1
fcvt.s.d r9, r1
fsqrt.s r9, r9
fadd.d r17, r18
csrr r4, 0x0D
fclass.d r10, r6
inc r11
"""


def _state(cpu) -> tuple:
    return (
        tuple(cpu.regs[i] for i in range(32)), cpu.pc, cpu.flags_pack(),
        cpu.csr_read(CSR_FPCSR), cpu.cycle_count,
    )


def _prepared(factory, rm: int):
    cpu = factory(mem_size=0x1000)
    cpu.load_bytes(0, assemble(_PROGRAM))
    cpu.regs[1], cpu.regs[5] = d64(1.0), rm
    cpu.regs[6], cpu.regs[7] = d64(3.0), d64(-0.25)
    cpu.regs[17], cpu.regs[18] = d64(2.0 ** 60), d64(1.0)
    cpu.pc = 0
    return cpu


@pytest.mark.parametrize("rm", range(5))
def test_native_steps_match_the_python_emulator(rm: int) -> None:
    python = _prepared(Megapad64, rm)
    native = _prepared(NativeMegapad64, rm)
    for _ in range(11):
        assert python.step() == native.step()
        assert _state(python) == _state(native)


def test_native_batches_fall_back_for_each_fp_instruction() -> None:
    python = _prepared(Megapad64, RUP)
    native = _prepared(NativeMegapad64, RUP)
    for _ in range(11):
        python.step()
    # Each batch ends after the one instruction the oracle executes.
    steps = 0
    while steps < 11:
        steps += native.run_steps_stats(11 - steps).steps_executed
    assert steps == 11
    assert _state(python) == _state(native)


@pytest.mark.parametrize("code", (
    bytes([0xF7, 0x11]), bytes([0xFD, 0x11]), bytes([0xFE]), bytes([0xFF]),
    bytes([0xFC, 0x87, 0x12, 0x03]), bytes([0xFC, 0x09, 0x12]),
))
def test_native_traps_match_the_python_emulator(code: bytes) -> None:
    results = []
    for factory in (Megapad64, NativeMegapad64):
        cpu = factory(mem_size=0x1000)
        cpu.load_bytes(0x100, code)
        cpu.ivt_base = 0x800
        cpu.load_bytes(0x800 + 8 * IVEC_ILLEGAL_OP, (0x400).to_bytes(8, "little"))
        cpu.regs[15] = 0xF00
        cpu.pc = 0x100
        try:
            cpu.step()
        except TrapError as trap:
            cpu._trap(trap.ivec_id)
        results.append((cpu.pc, cpu.ivec_id, cpu.mem_read64(cpu.regs[15])))
    assert results[0] == results[1]
    assert results[0][0] == 0x400


@pytest.mark.parametrize("source", (
    "fma.d r1, r6, r7", "fadd.d r17, r18", "fsqrt.d r1, r6",
))
def test_native_skip_steps_over_the_complete_instruction(source: str) -> None:
    body = assemble(source)
    cpu = NativeMegapad64(mem_size=0x1000)
    cpu.load_bytes(0, assemble("skip") + body + assemble("inc r5"))
    cpu.pc = 0
    cpu.step()
    assert cpu.pc == 2 + len(body)


def test_native_fpcsr_csr_and_reset() -> None:
    cpu = NativeMegapad64(mem_size=0x1000)
    cpu.csr_write(CSR_FPCSR, MASK64)
    assert cpu.csr_read(CSR_FPCSR) == 0x1F7
    cpu.load_bytes(0, assemble("csrr r1, 0x0D\ncsrw 0x0D, r6"))
    cpu.regs[6] = RDN | NV
    cpu.pc = 0
    cpu.run_steps(2)
    assert cpu.regs[1] == 0x1F7
    assert cpu.fpcsr == RDN | NV
    cpu._reset_state()
    assert cpu.csr_read(CSR_FPCSR) == 0


def test_native_system_microcores_keep_private_fpcsr() -> None:
    from system import MegapadSystem

    system = MegapadSystem(ram_size=4096, num_cores=1, num_clusters=1,
                           hbw_size=0, ext_mem_size=0, vram_size=0)
    system.sysinfo.write8(0x18, 0x01)  # enable the cluster
    first, second = system.clusters[0].cores[:2]
    system.load_binary(0x100, assemble(
        "fdiv.d r1, r6\ncsrr r4, 0x0D\nhalt"))
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, mode in ((first, RUP), (second, RNE)):
        cpu.pc = 0x100
        cpu.regs[1], cpu.regs[6] = d64(1.0), d64(3.0)
        cpu.csr_write(CSR_FPCSR, mode)
        cpu.halted = False
    for _ in range(50):
        if first.halted and second.halted:
            break
        system.run_batch_stats(4)
    assert first.halted and second.halted
    assert (first.regs[1], first.regs[4]) == (up64(1 / 3), RUP | NX)
    assert (second.regs[1], second.regs[4]) == (d64(1 / 3), NX)
