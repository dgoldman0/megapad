#!/usr/bin/env python3
"""Generate RTL oracle vectors for the scalar FPU (mp64_fpu.v).

Every expected value comes from shared/scalar_fp.py, the executable
definition of the FC engine (docs/floating-point.md §8–§10), which takes its
arithmetic from the exact reference in shared/ieee_fp.py.  The vectors cover
every legal operation in both formats, every static and dynamic rounding mode,
special values, subnormal and overflow boundaries, cancellation, and integer
and half-format inputs.

Each row is:

    name op rd rs rt rm expected flags cmp write latency

``flags`` is {NV, DZ, OF, UF, NX}; ``cmp`` is {V, N, G, Z} and is checked
only for FCMP, whose ``write`` is 0; ``latency`` is the §10 extra cycles.

Regenerate from the repository root with:

    python3 rtl/sim/gen_fpu_vectors.py > rtl/sim/fpu_vectors.vec
"""

from __future__ import annotations

from pathlib import Path
import random
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from shared import ieee_fp, scalar_fp  # noqa: E402
from shared.ieee_fp import BF16, FP16, FP32, FP64  # noqa: E402


MASK64 = (1 << 64) - 1
RELATION_BITS = {
    ieee_fp.EQUAL: 0b0001,
    ieee_fp.GREATER: 0b0010,
    ieee_fp.LESS: 0b0100,
    ieee_fp.UNORDERED: 0b1000,
}


def _codes() -> list[int]:
    return (list(range(0x09)) + list(range(0x10, 0x15))
            + list(range(0x20, 0x3F)))


def _special(fmt) -> list[int]:
    return [
        0, fmt.sign_bit, fmt.infinity, fmt.infinity | fmt.sign_bit,
        fmt.canonical_nan, fmt.canonical_nan | fmt.sign_bit | 3,
        fmt.infinity | 1, 1, 1 | fmt.sign_bit, fmt.max_finite,
        fmt.max_finite | fmt.sign_bit, 1 << fmt.fraction_bits,
        (1 << fmt.fraction_bits) - 1,
    ]


def _float(rng: random.Random, fmt) -> int:
    pick = rng.random()
    if pick < 0.2:
        return rng.choice(_special(fmt))
    if pick < 0.45:
        return rng.getrandbits(fmt.width)
    if pick < 0.6:
        # Near the subnormal boundary or the top of the range.
        field = rng.choice((0, 1, 2, fmt.exponent_field_max - 1,
                            fmt.exponent_field_max - 2))
        return ((rng.getrandbits(1) << (fmt.width - 1))
                | (field << fmt.fraction_bits)
                | rng.getrandbits(fmt.fraction_bits))
    value = rng.choice((0.5, 1.5, 2.5, 3.0, 0.1, 1e-3, 7.25, 65520.0,
                        2.0 ** 24 + 1, 2.0 ** 53 + 1, 2.0 ** 63, 2.0 ** 64,
                        1e10, 1e30, 1e-30, 123456.789))
    return ieee_fp.from_double(fmt, rng.choice((-1, 1)) * value)


def _integer(rng: random.Random) -> int:
    pick = rng.random()
    if pick < 0.3:
        return rng.choice((0, 1, MASK64, 1 << 63, (1 << 63) - 1,
                           (1 << 24) + 1, (1 << 53) + 1, 3, 1 << 62))
    if pick < 0.6:
        return rng.getrandbits(rng.randrange(1, 65))
    return rng.getrandbits(64)


def _half(rng: random.Random, fmt) -> int:
    pick = rng.random()
    if pick < 0.3:
        return rng.choice(_special(fmt))
    return rng.getrandbits(16)


def _operands(rng: random.Random, op: int) -> tuple[int, int, int]:
    fmt = FP64 if op >> 6 else FP32
    code = op & 0x3F
    high = rng.getrandbits(32) << 32 if fmt is FP32 else 0
    d = _float(rng, fmt) | high
    s = _float(rng, fmt) | (rng.getrandbits(32) << 32 if fmt is FP32 else 0)
    t = _float(rng, fmt)
    if code in (scalar_fp.FCVT_F_L, scalar_fp.FCVT_F_LU):
        s = _integer(rng)
    elif code == scalar_fp.FCVT_F_F:
        other = FP32 if fmt is FP64 else FP64
        s = _float(rng, other)
    elif code == scalar_fp.FCVT_F_H:
        s = _half(rng, FP16) | (rng.getrandbits(48) << 16)
    elif code == scalar_fp.FCVT_F_B:
        s = _half(rng, BF16) | (rng.getrandbits(48) << 16)
    elif code in (scalar_fp.FADD, scalar_fp.FSUB) and rng.random() < 0.3:
        # Cancellation within a few ULPs.
        mag = (d & ~fmt.sign_bit & fmt.mask) + rng.randint(-2, 2)
        if 0 <= mag < fmt.infinity:
            sign = (d & fmt.sign_bit) ^ (fmt.sign_bit if (
                op & 0x3F) == scalar_fp.FADD else 0)
            s = sign | mag
    elif code in (scalar_fp.FMA, scalar_fp.FMS) and rng.random() < 0.3:
        product = ieee_fp.mul(fmt, s, t)[0]
        if not ieee_fp.is_nan(fmt, product):
            d = product ^ (fmt.sign_bit if code == scalar_fp.FMA else 0)
    return d & MASK64, s & MASK64, t & MASK64


def _row(name: str, op: int, d: int, s: int, t: int, rm: int) -> str:
    outcome = scalar_fp.execute(op, d, s, t, rm)
    flags = outcome.flags >> scalar_fp.FPCSR_FLAGS_SHIFT
    if outcome.relation is None:
        value, cmp, write = outcome.value, 0, 1
    else:
        value, cmp, write = 0, RELATION_BITS[outcome.relation], 0
    return (f"{name} {op:02x} {d:016x} {s:016x} {t:016x} {rm:x} "
            f"{value:016x} {flags:02x} {cmp:x} {write} "
            f"{scalar_fp.extra_cycles(op)}")


def _rows() -> list[str]:
    rng = random.Random(0xFC_0007)
    rows = []
    for fmt_bits in (0x00, 0x40):
        for code in _codes():
            op = fmt_bits | code
            if 0x20 <= code < 0x38 and code & 7 in (5, 6):
                continue
            count = 60 if code in (0x03, 0x04) else 40
            for index in range(count):
                d, s, t = _operands(rng, op)
                rm = index % 5
                rows.append(_row(f"op{op:02x}_{index:02d}", op, d, s, t, rm))
    return rows


def main() -> None:
    rows = _rows()
    print("# Generated by rtl/sim/gen_fpu_vectors.py; do not edit.")
    print("# name op rd rs rt rm expected flags cmp write latency")
    print(f"# count {len(rows)}")
    for row in rows:
        print(row)


if __name__ == "__main__":
    main()
