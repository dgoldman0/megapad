#!/usr/bin/env python3
"""Generate RTL oracle vectors for the multi-format FMA unit.

The expected results come directly from the shared exact IEEE reference
(shared/ieee_fp.py).  Each row drives both lanes of ``mp64_fma_unit``:

* ``d`` rows: binary64 operands and results in lane 0;
* ``s`` rows: two binary32 FMAs (split mode);
* ``w`` rows: binary32 operands with binary64 results, which is WMUL when the
  addends are -0 and a mixed-precision FMA otherwise.

Lane 1 always takes binary32 operands and a binary32 addend, so ``d`` rows
also check its binary64 rounding.

Regenerate from the repository root with:

    python3 rtl/sim/gen_fma_vectors.py > rtl/sim/fma_vectors.vec
"""

from __future__ import annotations

from pathlib import Path
import random
import sys


REPO_ROOT = Path(__file__).resolve().parents[2]
if str(REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(REPO_ROOT))

from shared import ieee_fp  # noqa: E402
from shared.ieee_fp import FP32, FP64  # noqa: E402


NEG_ZERO64 = FP64.sign_bit
NEG_ZERO32 = FP32.sign_bit


def d64(value: float) -> int:
    return ieee_fp.from_double(FP64, value)


def d32(value: float) -> int:
    return ieee_fp.from_double(FP32, value)


def lane0(mode: str, a: int, b: int, c: int) -> int:
    if mode == "d":
        return ieee_fp.fma(FP64, a, b, c)[0]
    if mode == "s":
        return ieee_fp.fma(FP32, a, b, c)[0]
    return ieee_fp.mixed_fma(FP64, FP32, a, b, c)[0]


def lane1(mode: str, a: int, b: int, c: int) -> int:
    if mode == "s":
        return ieee_fp.fma(FP32, a, b, c)[0]
    wide_c = ieee_fp.convert(FP64, FP32, c)[0]
    return ieee_fp.mixed_fma(FP64, FP32, a, b, wide_c)[0]


def _named_binary64() -> list[tuple[str, int, int, int]]:
    tiny = 1
    max64 = FP64.max_finite
    min_normal = 1 << 52
    one = d64(1.0)
    return [
        ("one_times_one_plus_zero", one, one, 0),
        ("exact_cancel_is_positive_zero", d64(1.5), d64(2.0), d64(-3.0)),
        ("negative_zero_plus_positive_zero", NEG_ZERO64, one, 0),
        ("negative_zero_plus_negative_zero", NEG_ZERO64, one, NEG_ZERO64),
        ("negative_product_zero", d64(-1.0), 0, NEG_ZERO64),
        ("zero_times_inf_plus_one", 0, FP64.infinity, one),
        ("zero_times_inf_plus_nan", 0, FP64.infinity, FP64.canonical_nan),
        ("inf_minus_inf", FP64.infinity, one,
         FP64.infinity | FP64.sign_bit),
        ("inf_plus_inf", FP64.infinity, one, FP64.infinity),
        ("finite_plus_negative_inf", one, one,
         FP64.infinity | FP64.sign_bit),
        ("signalling_nan_operand", FP64.infinity | 1, one, one),
        ("negative_nan_addend", one, one, FP64.canonical_nan | FP64.sign_bit),
        ("fused_product_does_not_overflow", max64, d64(2.0),
         max64 | FP64.sign_bit),
        ("product_overflows", max64, max64, 0),
        ("negative_overflow", max64 | FP64.sign_bit, max64, 0),
        ("half_min_subnormal_ties_to_zero", tiny, d64(0.5), 0),
        ("three_quarter_subnormal_rounds_up", tiny, d64(0.75), 0),
        ("one_and_half_subnormal_ties_even", tiny, d64(1.5), 0),
        ("negative_subnormal_underflow", tiny | FP64.sign_bit, d64(0.25), 0),
        ("largest_subnormal_carries_to_normal", min_normal - 1, one, tiny),
        ("tie_to_even_down", one, one, d64(2.0 ** -53)),
        ("tie_broken_by_product", d64(2.0 ** -53), d64(1.0 + 2.0 ** -52),
         one),
        ("jammed_subtraction", d64(-(2.0 ** -54)),
         d64(1.0 + 2.0 ** -52), one),
        ("far_small_product", d64(2.0 ** -600), d64(2.0 ** -600),
         d64(2.0 ** 1000)),
        ("far_small_addend", d64(2.0 ** 500), d64(2.0 ** 500),
         d64(-(2.0 ** -1000))),
        ("subnormal_by_cancellation", d64(1.0 + 2.0 ** -52),
         d64(2.0 ** -1022), d64(-(2.0 ** -1022))),
        ("subnormal_operands", tiny, min_normal - 1, min_normal),
    ]


def _named_binary32() -> list[tuple[str, int, int, int]]:
    one = d32(1.0)
    return [
        ("one_times_one_plus_zero", one, one, 0),
        ("exact_cancel_is_positive_zero", d32(1.5), d32(2.0), d32(-3.0)),
        ("negative_zeros", NEG_ZERO32, one, NEG_ZERO32),
        ("zero_times_inf", 0, FP32.infinity, one),
        ("inf_minus_inf", FP32.infinity, one, FP32.infinity | FP32.sign_bit),
        ("fused_product_does_not_overflow", FP32.max_finite, d32(2.0),
         FP32.max_finite | FP32.sign_bit),
        ("product_overflows", FP32.max_finite, FP32.max_finite, 0),
        ("half_min_subnormal_ties_to_zero", 1, d32(0.5), 0),
        ("one_and_half_subnormal_ties_even", 1, d32(1.5), 0),
        ("largest_subnormal_carries_to_normal", (1 << 23) - 1, one, 1),
        ("tie_to_even_down", one, one, d32(2.0 ** -24)),
        ("jammed_subtraction", d32(-(2.0 ** -25)),
         d32(1.0 + 2.0 ** -23), one),
        ("rounding_error_of_product", d32(1.0 + 2.0 ** -23),
         d32(1.0 + 2.0 ** -23), d32(-(1.0 + 2.0 ** -22))),
        ("signalling_nan", FP32.infinity | 1, one, one),
    ]


def _random_bits(rng: random.Random, fmt: ieee_fp.Format,
                 anchor: int) -> int:
    choice = rng.random()
    if choice < 0.08:
        return rng.choice((
            0, fmt.sign_bit, fmt.infinity, fmt.canonical_nan, 1,
            fmt.max_finite, (1 << fmt.fraction_bits) - 1,
            1 << fmt.fraction_bits, fmt.infinity | fmt.sign_bit,
        ))
    if choice < 0.3:
        return rng.getrandbits(fmt.width)
    field = min(fmt.exponent_field_max - 1,
                max(0, anchor + rng.randint(-4, 4)))
    return ((rng.getrandbits(1) << (fmt.width - 1))
            | (field << fmt.fraction_bits)
            | rng.getrandbits(fmt.fraction_bits))


def _near_cancelling_addend(rng: random.Random, fmt: ieee_fp.Format,
                            product_bits: int) -> int:
    """An addend within a few ULPs of -product, to stress cancellation."""
    if ieee_fp.is_nan(fmt, product_bits):
        return 0
    negated = product_bits ^ fmt.sign_bit
    delta = rng.randint(-3, 3)
    magnitude = (negated & ~fmt.sign_bit) + delta
    if magnitude < 0 or magnitude >= fmt.infinity:
        return negated
    return (negated & fmt.sign_bit) | magnitude


def _random_triple(rng: random.Random, fmt: ieee_fp.Format,
                   product_fmt: ieee_fp.Format) -> tuple[int, int, int]:
    anchor = rng.randrange(1, fmt.exponent_field_max)
    a = _random_bits(rng, fmt, anchor)
    b = _random_bits(rng, fmt, rng.randrange(1, fmt.exponent_field_max))
    if rng.random() < 0.35:
        product_bits = ieee_fp.product(product_fmt, fmt, a, b)[0]
        c = _near_cancelling_addend(rng, product_fmt, product_bits)
    else:
        c = _random_bits(rng, product_fmt,
                         rng.randrange(1, product_fmt.exponent_field_max))
    return a, b, c


def _rows() -> list[tuple[str, str, int, int, int, int, int, int]]:
    rng = random.Random(0x464D_4155)
    rows = []
    named32 = _named_binary32()
    for index, (name, a, b, c) in enumerate(_named_binary64()):
        a1, b1, c1 = named32[index % len(named32)][1:]
        rows.append((f"d_{name}", "d", a, b, c, a1, b1, c1))
    for index, (name, a, b, c) in enumerate(named32):
        a1, b1, c1 = named32[(index + 5) % len(named32)][1:]
        rows.append((f"s_{name}", "s", a, b, c, a1, b1, c1))
        rows.append((f"w_{name}", "w", a, b, NEG_ZERO64, a1, b1,
                     NEG_ZERO32))
    for index in range(400):
        a, b, c = _random_triple(rng, FP64, FP64)
        a1, b1, c1 = _random_triple(rng, FP32, FP32)
        rows.append((f"d_random_{index:03d}", "d", a, b, c, a1, b1, c1))
    for index in range(400):
        a, b, c = _random_triple(rng, FP32, FP32)
        a1, b1, c1 = _random_triple(rng, FP32, FP32)
        rows.append((f"s_random_{index:03d}", "s", a, b, c, a1, b1, c1))
    for index in range(200):
        a, b, c = _random_triple(rng, FP32, FP64)
        a1, b1, c1 = _random_triple(rng, FP32, FP32)
        if index % 2 == 0:
            c, c1 = NEG_ZERO64, NEG_ZERO32
        rows.append((f"w_random_{index:03d}", "w", a, b, c, a1, b1, c1))
    return rows


def main() -> None:
    print("# Generated by rtl/sim/gen_fma_vectors.py; do not edit.")
    print("# name mode(d/s/w) a0 b0 c0 a1 b1 c1 expected_r0 expected_r1")
    for name, mode, a0, b0, c0, a1, b1, c1 in _rows():
        r0 = lane0(mode, a0, b0, c0)
        r1 = lane1(mode, a1, b1, c1)
        print(
            f"{name} {mode} {a0:016x} {b0:016x} {c0:016x} "
            f"{a1:08x} {b1:08x} {c1:08x} {r0:016x} {r1:016x}"
        )


if __name__ == "__main__":
    main()
