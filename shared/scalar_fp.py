"""Scalar floating-point engine (``FC``) semantics.

docs/floating-point.md §8 and §9 define the engine; this module is its one
executable definition.  The Python emulator executes decoded ``FC``
instructions through :func:`execute`, and the hosted simulator's BIOS-shaped
words call it with the operation byte each word stands for.  Every value
comes from the exact reference in :mod:`shared.ieee_fp`.

An operation byte is ``op[7:6]`` = format (0 binary32, 1 binary64) and
``op[5:0]`` = operation.  Register values are 64-bit cells: binary32 inputs
use bits ``[31:0]`` and binary32 results clear bits ``[63:32]``.
"""

from __future__ import annotations

from dataclasses import dataclass

from shared import ieee_fp
from shared.ieee_fp import BF16, FP16, FP32, FP64

MASK64 = (1 << 64) - 1

# FPCSR (CSR 0x0D) layout, §9.
FPCSR = 0x0D
FPCSR_RM_MASK = 0x7
FPCSR_FLAGS_SHIFT = 4
FPCSR_WRITE_MASK = 0x1F7

# Operations, §8.3.
FADD, FSUB, FMUL, FDIV, FSQRT, FMIN, FMAX, FMA, FMS = range(9)
FCMP, FEQ, FLT, FLE, FCLASS = range(0x10, 0x15)
FRND = 0x20
FCVT_L = 0x28
FCVT_LU = 0x30
FCVT_F_L = 0x38
FCVT_F_LU = 0x39
FCVT_F_F = 0x3A
FCVT_H_F = 0x3B
FCVT_F_H = 0x3C
FCVT_B_F = 0x3D
FCVT_F_B = 0x3E

FORMAT_S = 0
FORMAT_D = 1
DYNAMIC = 7

_FORMATS = (FP32, FP64)

# Operations that round with FPCSR.RM.  Rounding-mode forms (FRND and the
# float-to-integer conversions) use FPCSR.RM only when their mode field is 7.
# The rule depends on op[5:0] alone, so FCVT.D.S consults FPCSR.RM although
# its result is always exact.
_DYNAMIC_OPERATIONS = frozenset((
    FADD, FSUB, FMUL, FDIV, FSQRT, FMA, FMS,
    FCVT_F_L, FCVT_F_LU, FCVT_F_F, FCVT_H_F, FCVT_B_F,
))
_ROUNDING_FORM_BASES = (FRND, FCVT_L, FCVT_LU)
_DEFINED = frozenset(
    tuple(range(9)) + tuple(range(0x10, 0x15)) + tuple(range(0x20, 0x3F))
)

# §10 extra cycles.  FDIV and FSQRT run a data-independent digit recurrence
# that retires two result bits per cycle.
DIVIDE_EXTRA_CYCLES = {FORMAT_S: 15, FORMAT_D: 30}
_SHORT_OPERATIONS = frozenset((FMIN, FMAX, FCMP, FEQ, FLT, FLE, FCLASS))


class IllegalOperation(ValueError):
    """The encoding is reserved; the machine raises ``IVEC_ILLEGAL_OP``."""


@dataclass(frozen=True)
class Outcome:
    """One executed operation.

    ``value`` is the 64-bit register result, or ``None`` for FCMP.
    ``flags`` holds the FPCSR flag bits to OR in (already shifted).
    ``relation`` is FCMP's ``ieee_fp`` comparison result, else ``None``.
    """

    value: int | None
    flags: int
    relation: int | None = None


def instruction_length(op: int) -> int:
    """Bytes after the ``FC`` byte's position, counting it: 3 or 4."""

    return 4 if op & 0x3F in (FMA, FMS) else 3


def extra_cycles(op: int) -> int:
    """§10 extra cycles for a legal operation byte."""

    code = op & 0x3F
    if code in (FDIV, FSQRT):
        return DIVIDE_EXTRA_CYCLES[op >> 6]
    if code in _SHORT_OPERATIONS:
        return 1
    return 3


def uses_dynamic_rounding(op: int) -> bool:
    code = op & 0x3F
    if code in _DYNAMIC_OPERATIONS:
        return True
    return code >= FRND and code < FCVT_F_L and code & 7 == DYNAMIC


def validate(op: int, t_byte: int = 0, fpcsr: int = 0) -> None:
    """Raise :class:`IllegalOperation` for any reserved encoding or mode."""

    if op >> 6 > FORMAT_D:
        raise IllegalOperation(f"reserved FC format {op >> 6}")
    code = op & 0x3F
    if code not in _DEFINED:
        raise IllegalOperation(f"reserved FC operation {code:#04x}")
    if code in (FMA, FMS) and t_byte >> 5:
        raise IllegalOperation("FC T byte bits [7:5] must be zero")
    if FRND <= code < FCVT_F_L and code & 7 in (5, 6):
        raise IllegalOperation(f"reserved FC rounding mode {code & 7}")
    if uses_dynamic_rounding(op) and fpcsr & FPCSR_RM_MASK > ieee_fp.RMM:
        raise IllegalOperation(
            f"reserved FPCSR.RM {fpcsr & FPCSR_RM_MASK} for a dynamic mode")


def _flags(oracle_flags: int) -> int:
    return oracle_flags << FPCSR_FLAGS_SHIFT


def _signalling(fmt, *values: int) -> bool:
    return any(ieee_fp.is_signalling_nan(fmt, value) for value in values)


def _nan(fmt, *values: int) -> bool:
    return any(ieee_fp.is_nan(fmt, value) for value in values)


def execute(op: int, rd: int, rs: int, rt: int = 0, fpcsr: int = 0
            ) -> Outcome:
    """Execute one legal ``FC`` operation on register values.

    ``rd``, ``rs``, and ``rt`` are the 64-bit register contents.  Call
    :func:`validate` first; this function assumes a legal encoding.
    """

    fmt = _FORMATS[op >> 6]
    code = op & 0x3F
    dynamic = fpcsr & FPCSR_RM_MASK
    d = rd & fmt.mask
    s = rs & fmt.mask

    def rounded(bits_flags: tuple[int, int]) -> Outcome:
        bits, oracle_flags = bits_flags
        return Outcome(bits, _flags(oracle_flags))

    if code == FADD:
        return rounded(ieee_fp.add(fmt, d, s, dynamic))
    if code == FSUB:
        return rounded(ieee_fp.sub(fmt, d, s, dynamic))
    if code == FMUL:
        return rounded(ieee_fp.mul(fmt, d, s, dynamic))
    if code == FDIV:
        return rounded(ieee_fp.div(fmt, d, s, dynamic))
    if code == FSQRT:
        return rounded(ieee_fp.sqrt(fmt, s, dynamic))
    if code in (FMIN, FMAX):
        pick = ieee_fp.minimum if code == FMIN else ieee_fp.maximum
        return Outcome(pick(fmt, d, s),
                       _flags(ieee_fp.NV if _signalling(fmt, d, s) else 0))
    if code in (FMA, FMS):
        t = rt & fmt.mask
        if code == FMS:
            s ^= fmt.sign_bit  # Rd - Rs*Rt = (-Rs)*Rt + Rd, one rounding
        return rounded(ieee_fp.fma(fmt, s, t, d, dynamic))
    if code in (FCMP, FEQ, FLT, FLE):
        relation = ieee_fp.compare(fmt, d, s)
        loud = code in (FLT, FLE)
        invalid = _nan(fmt, d, s) if loud else _signalling(fmt, d, s)
        flags = _flags(ieee_fp.NV if invalid else 0)
        if code == FCMP:
            return Outcome(None, flags, relation)
        holds = (
            relation == ieee_fp.EQUAL if code == FEQ
            else relation == ieee_fp.LESS if code == FLT
            else relation in (ieee_fp.LESS, ieee_fp.EQUAL)
        )
        return Outcome(MASK64 if holds else 0, flags)
    if code == FCLASS:
        return Outcome(ieee_fp.classify(fmt, s), 0)
    if FRND <= code < FCVT_F_L:
        rm = code & 7
        if rm == DYNAMIC:
            rm = dynamic
        if code < FCVT_L:
            return rounded(ieee_fp.round_integral(fmt, s, rm))
        return rounded(ieee_fp.to_int(fmt, s, 64, code < FCVT_LU, rm))
    if code in (FCVT_F_L, FCVT_F_LU):
        value = rs & MASK64
        if code == FCVT_F_L and value >> 63:
            value -= 1 << 64
        return rounded(ieee_fp.from_int(fmt, value, dynamic))
    if code == FCVT_F_F:
        other = FP64 if fmt is FP32 else FP32
        return rounded(ieee_fp.convert(fmt, other, rs & other.mask, dynamic))
    if code == FCVT_H_F:
        return rounded(ieee_fp.convert(FP16, fmt, s, dynamic))
    if code == FCVT_F_H:
        return rounded(ieee_fp.convert(fmt, FP16, rs & FP16.mask))
    if code == FCVT_B_F:
        return rounded(ieee_fp.convert(BF16, fmt, s, dynamic))
    if code == FCVT_F_B:
        return rounded(ieee_fp.convert(fmt, BF16, rs & BF16.mask))
    raise IllegalOperation(f"reserved FC operation {code:#04x}")
