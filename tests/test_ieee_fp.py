"""Checks for the shared exact floating-point reference (shared/ieee_fp.py).

The exact layer is checked against an independent rounding implementation
built on ``fractions.Fraction`` and, where the host provides one, against
numpy's IEEE binary32/binary64 arithmetic.  The fast lane layer is checked
against the exact layer.  Named cases pin the rules in
``docs/floating-point.md`` and the defects Phase 2 removes.
"""

from __future__ import annotations

import math
import random
import struct
from fractions import Fraction

import numpy as np
import pytest

from shared import ieee_fp as fp
from shared.ieee_fp import BF16, FP16, FP32, FP64, FORMATS, NARROW_FORMATS


# ---------------------------------------------------------------------------
# Independent reference (Fraction based)
# ---------------------------------------------------------------------------

def ref_decode(fmt, bits):
    """Return ('nan',) / ('inf', sign) / ('num', sign, Fraction magnitude)."""

    bits &= fmt.mask
    sign = bits >> (fmt.width - 1)
    exponent = (bits >> fmt.fraction_bits) & fmt.exponent_field_max
    fraction = bits & fmt.fraction_mask
    if exponent == fmt.exponent_field_max:
        return ("nan",) if fraction else ("inf", sign)
    if exponent == 0:
        magnitude = Fraction(fraction) * Fraction(2) ** (
            fmt.emin - fmt.fraction_bits
        )
    else:
        magnitude = Fraction((1 << fmt.fraction_bits) + fraction) * Fraction(
            2
        ) ** (exponent - fmt.bias - fmt.fraction_bits)
    return ("num", sign, magnitude)


def _floor_log2(value: Fraction) -> int:
    estimate = value.numerator.bit_length() - value.denominator.bit_length()
    while Fraction(2) ** estimate > value:
        estimate -= 1
    while Fraction(2) ** (estimate + 1) <= value:
        estimate += 1
    return estimate


def ref_round(fmt, sign, magnitude: Fraction, rm):
    """Round ``(-1)**sign * magnitude`` to ``fmt`` bits under ``rm``."""

    sign_bits = fmt.sign_bit if sign else 0
    if magnitude == 0:
        return sign_bits
    precision = fmt.precision
    exponent = max(_floor_log2(magnitude), fmt.emin)
    quantum = Fraction(2) ** (exponent - (precision - 1))
    scaled = magnitude / quantum
    low = scaled.numerator // scaled.denominator
    rest = scaled - low
    if rm == fp.RNE:
        up = rest > Fraction(1, 2) or (rest == Fraction(1, 2) and low & 1)
    elif rm == fp.RTZ:
        up = False
    elif rm == fp.RDN:
        up = bool(sign) and rest > 0
    elif rm == fp.RUP:
        up = not sign and rest > 0
    else:
        up = rest >= Fraction(1, 2)
    rounded = (low + 1 if up else low) * quantum
    max_finite = (Fraction(2) - Fraction(2) ** (1 - precision)) * Fraction(
        2
    ) ** fmt.bias
    if rounded > max_finite:
        to_infinity = (
            rm in (fp.RNE, fp.RMM)
            or (rm == fp.RDN and sign)
            or (rm == fp.RUP and not sign)
        )
        return sign_bits | (fmt.infinity if to_infinity else fmt.max_finite)
    if rounded == 0:
        return sign_bits
    lead = _floor_log2(rounded)
    if lead < fmt.emin:
        return sign_bits | int(rounded / Fraction(2) ** (
            fmt.emin - fmt.fraction_bits
        ))
    significand = int(rounded / Fraction(2) ** (lead - fmt.fraction_bits))
    return (
        sign_bits
        | ((lead + fmt.bias) << fmt.fraction_bits)
        | (significand - (1 << fmt.fraction_bits))
    )


def ref_signed(value):
    if value[0] != "num":
        raise AssertionError("not a number")
    return -value[2] if value[1] else value[2]


def ref_zero_sign(sign_a, sign_b, rm):
    return (sign_a | sign_b) if rm == fp.RDN else (sign_a & sign_b)


def ref_add(fmt, a, b, rm):
    x, y = ref_decode(fmt, a), ref_decode(fmt, b)
    if x[0] == "nan" or y[0] == "nan":
        return fmt.canonical_nan
    if x[0] == "inf" or y[0] == "inf":
        if x[0] == "inf" and y[0] == "inf" and x[1] != y[1]:
            return fmt.canonical_nan
        inf = x if x[0] == "inf" else y
        return (fmt.sign_bit if inf[1] else 0) | fmt.infinity
    total = ref_signed(x) + ref_signed(y)
    if total == 0:
        if x[2] == 0 and y[2] == 0:
            sign = ref_zero_sign(x[1], y[1], rm)
        else:
            sign = 1 if rm == fp.RDN else 0
        return fmt.sign_bit if sign else 0
    return ref_round(fmt, 1 if total < 0 else 0, abs(total), rm)


def ref_mul(dst, src, a, b, rm):
    x, y = ref_decode(src, a), ref_decode(src, b)
    if x[0] == "nan" or y[0] == "nan":
        return dst.canonical_nan
    sign = x[1] ^ y[1]
    zero_x = x[0] == "num" and x[2] == 0
    zero_y = y[0] == "num" and y[2] == 0
    if (x[0] == "inf" and zero_y) or (y[0] == "inf" and zero_x):
        return dst.canonical_nan
    if x[0] == "inf" or y[0] == "inf":
        return (dst.sign_bit if sign else 0) | dst.infinity
    return ref_round(dst, sign, x[2] * y[2], rm)


def ref_fma(dst, src, a, b, c, rm):
    x, y, z = ref_decode(src, a), ref_decode(src, b), ref_decode(dst, c)
    if "nan" in (x[0], y[0], z[0]):
        return dst.canonical_nan
    sign = x[1] ^ y[1]
    zero_x = x[0] == "num" and x[2] == 0
    zero_y = y[0] == "num" and y[2] == 0
    if (x[0] == "inf" and zero_y) or (y[0] == "inf" and zero_x):
        return dst.canonical_nan
    if x[0] == "inf" or y[0] == "inf":
        if z[0] == "inf" and z[1] != sign:
            return dst.canonical_nan
        return (dst.sign_bit if sign else 0) | dst.infinity
    if z[0] == "inf":
        return (dst.sign_bit if z[1] else 0) | dst.infinity
    product_value = x[2] * y[2]
    total = (-product_value if sign else product_value) + ref_signed(z)
    if total == 0:
        if product_value == 0 and z[2] == 0:
            zero_sign = ref_zero_sign(sign, z[1], rm)
        else:
            zero_sign = 1 if rm == fp.RDN else 0
        return dst.sign_bit if zero_sign else 0
    return ref_round(dst, 1 if total < 0 else 0, abs(total), rm)


def ref_div(fmt, a, b, rm):
    x, y = ref_decode(fmt, a), ref_decode(fmt, b)
    if x[0] == "nan" or y[0] == "nan":
        return fmt.canonical_nan
    sign = x[1] ^ y[1]
    sign_bits = fmt.sign_bit if sign else 0
    zero_x = x[0] == "num" and x[2] == 0
    zero_y = y[0] == "num" and y[2] == 0
    if (x[0] == "inf" and y[0] == "inf") or (zero_x and zero_y):
        return fmt.canonical_nan
    if x[0] == "inf" or zero_y:
        return sign_bits | fmt.infinity
    if y[0] == "inf" or zero_x:
        return sign_bits
    return ref_round(fmt, sign, x[2] / y[2], rm)


# ---------------------------------------------------------------------------
# Operand generators
# ---------------------------------------------------------------------------

def random_bits(rng, fmt):
    return rng.getrandbits(fmt.width)


def near_bits(rng, fmt, anchor):
    """Return a value with an exponent close to ``anchor``'s."""

    exponent = (anchor >> fmt.fraction_bits) & fmt.exponent_field_max
    exponent = max(0, min(fmt.exponent_field_max - 1,
                          exponent + rng.randint(-fmt.precision - 2,
                                                 fmt.precision + 2)))
    fraction = rng.getrandbits(fmt.fraction_bits)
    if rng.random() < 0.3:
        fraction &= ~((1 << rng.randint(0, fmt.fraction_bits)) - 1)
    return (rng.getrandbits(1) << (fmt.width - 1)) | (
        exponent << fmt.fraction_bits
    ) | fraction


def operand_pairs(fmt, count, seed):
    rng = random.Random(seed)
    for _ in range(count):
        a = random_bits(rng, fmt)
        b = random_bits(rng, fmt) if rng.random() < 0.4 else near_bits(rng, fmt, a)
        yield a, b


SPECIALS = {
    fmt: [
        0, fmt.sign_bit, 1, fmt.sign_bit | 1,
        fmt.infinity, fmt.sign_bit | fmt.infinity,
        fmt.canonical_nan, fmt.infinity | 1,
        fmt.max_finite, fmt.sign_bit | fmt.max_finite,
        1 << fmt.fraction_bits, (1 << fmt.fraction_bits) - 1,
        fmt.bias << fmt.fraction_bits,
    ]
    for fmt in FORMATS
}


def special_pairs(fmt):
    for a in SPECIALS[fmt]:
        for b in SPECIALS[fmt]:
            yield a, b


def bits_equal(fmt, got, expected):
    if fp.is_nan(fmt, expected):
        return got == fmt.canonical_nan
    return got == expected


# ---------------------------------------------------------------------------
# Exact layer against the independent reference
# ---------------------------------------------------------------------------

EXACT_COUNTS = {FP16: 1500, BF16: 1500, FP32: 800, FP64: 400}


@pytest.mark.parametrize("fmt", FORMATS, ids=lambda f: f.name)
@pytest.mark.parametrize("rm", fp.ROUNDING_MODES)
def test_exact_add_mul_div_match_fraction_reference(fmt, rm):
    count = EXACT_COUNTS[fmt] if rm == fp.RNE else EXACT_COUNTS[fmt] // 3
    pairs = list(special_pairs(fmt)) + list(
        operand_pairs(fmt, count, seed=0x5EED + fmt.ew * 16 + rm)
    )
    for a, b in pairs:
        assert fp.add(fmt, a, b, rm)[0] == ref_add(fmt, a, b, rm), (a, b)
        assert fp.mul(fmt, a, b, rm)[0] == ref_mul(fmt, fmt, a, b, rm), (a, b)
        assert fp.div(fmt, a, b, rm)[0] == ref_div(fmt, a, b, rm), (a, b)


@pytest.mark.parametrize("fmt", FORMATS, ids=lambda f: f.name)
@pytest.mark.parametrize("rm", fp.ROUNDING_MODES)
def test_exact_fma_matches_fraction_reference(fmt, rm):
    rng = random.Random(0xF4A + fmt.ew * 8 + rm)
    count = EXACT_COUNTS[fmt] if rm == fp.RNE else EXACT_COUNTS[fmt] // 3
    cases = []
    for a, b in operand_pairs(fmt, count, seed=0xFA + fmt.ew + rm):
        product_value = ref_mul(FP64, fmt, a, b, fp.RNE)
        c = near_bits(rng, fmt, a) if rng.random() < 0.5 else random_bits(
            rng, fmt
        )
        if rng.random() < 0.2 and fmt is not FP64:
            # Aim the addend at cancellation with the product.
            c = fp.from_double(fmt, -fp.to_double(FP64, product_value))
        cases.append((a, b, c))
    specials = SPECIALS[fmt]
    cases += [(a, b, c) for a in specials[:8] for b in specials[:8]
              for c in specials[:8]]
    for a, b, c in cases:
        assert fp.fma(fmt, a, b, c, rm)[0] == ref_fma(
            fmt, fmt, a, b, c, rm
        ), (a, b, c)


@pytest.mark.parametrize("src,dst", [(FP16, FP32), (BF16, FP32),
                                     (FP32, FP64)],
                         ids=lambda f: f.name)
def test_mixed_fma_and_product_match_fraction_reference(src, dst):
    rng = random.Random(0xAC + src.ew)
    for a, b in operand_pairs(src, 1500, seed=0xAC0 + src.ew):
        c = random_bits(rng, dst) if rng.random() < 0.3 else fp.from_double(
            dst, rng.uniform(-4, 4) * fp.to_double(FP64, ref_mul(
                FP64, src, a, b, fp.RNE)))
        assert fp.mixed_fma(dst, src, a, b, c)[0] == ref_fma(
            dst, src, a, b, c, fp.RNE
        ), (a, b, c)
        assert fp.product(dst, src, a, b)[0] == ref_mul(
            dst, src, a, b, fp.RNE
        ), (a, b)


@pytest.mark.parametrize("fmt", FORMATS, ids=lambda f: f.name)
@pytest.mark.parametrize("rm", fp.ROUNDING_MODES)
def test_exact_sqrt_is_correctly_rounded(fmt, rm):
    rng = random.Random(0x5A + fmt.ew * 8 + rm)
    values = SPECIALS[fmt] + [random_bits(rng, fmt) for _ in range(600)]
    for a in values:
        got = fp.sqrt(fmt, a, rm)[0]
        x = ref_decode(fmt, a)
        if x[0] == "nan" or (x[0] == "num" and x[1] and x[2] != 0) or (
            x[0] == "inf" and x[1]
        ):
            assert got == fmt.canonical_nan, a
            continue
        if x[0] == "inf" or x[2] == 0:
            assert got == a, a
            continue
        target = x[2]
        root = ref_decode(fmt, got)[2]
        below = ref_decode(fmt, got - 1)[2] if got > 0 else Fraction(0)
        above_decoded = ref_decode(fmt, got + 1)
        above = above_decoded[2] if above_decoded[0] == "num" else None
        if rm in (fp.RTZ, fp.RDN):
            assert root * root <= target, a
            assert above is None or above * above > target, a
        elif rm == fp.RUP:
            assert root * root >= target, a
            assert below * below < target, a
        else:
            low_mid = (below + root) / 2
            assert low_mid * low_mid <= target, a
            if above is not None:
                high_mid = (root + above) / 2
                assert target <= high_mid * high_mid, a
                if target == high_mid * high_mid and rm == fp.RNE:
                    assert not (got & 1), a


@pytest.mark.parametrize("fmt,dtype", [(FP32, np.float32), (FP64, np.float64),
                                       (FP16, np.float16)],
                         ids=["fp32", "fp64", "fp16"])
def test_exact_layer_matches_numpy_rne(fmt, dtype):
    """numpy's IEEE arithmetic is a second, host-hardware reference."""

    int_type = {16: np.uint16, 32: np.uint32, 64: np.uint64}[fmt.width]
    with np.errstate(all="ignore"):
        for a, b in operand_pairs(fmt, 3000, seed=0x9E0 + fmt.ew):
            x = np.array([a], dtype=int_type).view(dtype)[0]
            y = np.array([b], dtype=int_type).view(dtype)[0]
            for op, host in ((fp.add, np.add), (fp.mul, np.multiply),
                             (fp.div, np.divide)):
                expected = int(np.array([host(x, y)], dtype=dtype).view(
                    int_type)[0])
                assert bits_equal(fmt, op(fmt, a, b)[0], expected), (op, a, b)
            expected = int(np.array([np.sqrt(x)], dtype=dtype).view(
                int_type)[0])
            assert bits_equal(fmt, fp.sqrt(fmt, a)[0], expected), a


# ---------------------------------------------------------------------------
# Lane layer against the exact layer
# ---------------------------------------------------------------------------

LANE_COUNTS = {FP16: 20000, BF16: 20000, FP32: 15000, FP64: 8000}


@pytest.mark.parametrize("fmt", FORMATS, ids=lambda f: f.name)
def test_lane_arithmetic_matches_exact_layer(fmt):
    rng = random.Random(0x1A + fmt.ew)
    pairs = list(special_pairs(fmt)) + list(
        operand_pairs(fmt, LANE_COUNTS[fmt], seed=0x1A0 + fmt.ew)
    )
    for a, b in pairs:
        assert fp.lane_add(fmt, a, b) == fp.add(fmt, a, b)[0], (a, b)
        assert fp.lane_sub(fmt, a, b) == fp.sub(fmt, a, b)[0], (a, b)
        assert fp.lane_mul(fmt, a, b) == fp.mul(fmt, a, b)[0], (a, b)
        c = near_bits(rng, fmt, a) if rng.random() < 0.6 else random_bits(
            rng, fmt)
        assert fp.lane_fma(fmt, a, b, c) == fp.fma(fmt, a, b, c)[0], (a, b, c)


@pytest.mark.parametrize("src,dst", [(FP16, FP32), (BF16, FP32),
                                     (FP32, FP64), (FP64, FP64)],
                         ids=lambda f: f.name)
def test_lane_mixed_operations_match_exact_layer(src, dst):
    rng = random.Random(0x2B + src.ew)
    for a, b in list(special_pairs(src)) + list(
        operand_pairs(src, 15000, seed=0x2B0 + src.ew)
    ):
        exact_product = fp.product(dst, src, a, b)[0]
        assert fp.lane_product(dst, src, a, b) == exact_product, (a, b)
        if rng.random() < 0.5 and not fp.is_nan(dst, exact_product):
            c = exact_product ^ dst.sign_bit
            c = fp.lane_add(dst, c, near_bits(rng, dst, c) & ~(
                (1 << (dst.fraction_bits // 2)) - 1))
        else:
            c = random_bits(rng, dst)
        assert fp.lane_mixed_fma(dst, src, a, b, c) == fp.mixed_fma(
            dst, src, a, b, c
        )[0], (a, b, c)


@pytest.mark.parametrize("dst", FORMATS, ids=lambda f: f.name)
@pytest.mark.parametrize("src", FORMATS, ids=lambda f: f.name)
def test_lane_convert_matches_exact_layer(src, dst):
    rng = random.Random(0x3C + src.ew * 8 + dst.ew)
    if src.width == 16:
        values = range(1 << 16)
    else:
        values = SPECIALS[src] + [random_bits(rng, src) for _ in range(20000)]
    for a in values:
        assert fp.lane_convert(dst, src, a) == fp.convert(dst, src, a)[0], a


def test_from_double_matches_exact_rounding_at_every_fp16_midpoint():
    """struct's binary16 packing must round exactly like the exact layer."""

    finite = [bits for bits in range(0x7C00)]
    for low, high in zip(finite, finite[1:]):
        mid = (fp.to_double(FP16, low) + fp.to_double(FP16, high)) / 2
        for value in (mid, math.nextafter(mid, 0.0),
                      math.nextafter(mid, math.inf)):
            for signed in (value, -value):
                exact = fp.encode(FP16, fp.decode(FP64, fp.double_bits(
                    signed)))[0]
                assert fp.from_double(FP16, signed) == exact, signed
    top = 65504.0 + 16.0
    assert fp.from_double(FP16, top) == 0x7C00
    assert fp.from_double(FP16, math.nextafter(top, 0.0)) == 0x7BFF


@pytest.mark.parametrize("fmt", FORMATS, ids=lambda f: f.name)
def test_from_double_matches_exact_rounding_for_random_doubles(fmt):
    rng = random.Random(0x4D + fmt.ew)
    for _ in range(40000):
        bits = rng.getrandbits(64)
        if rng.random() < 0.8:
            exponent = max(0, min(0x7FF, 1023 + rng.randint(
                -fmt.bias - fmt.precision - 4, fmt.bias + 2)))
            bits = (bits & ~(0x7FF << 52)) | (exponent << 52)
        value = struct.unpack("<d", struct.pack("<Q", bits))[0]
        exact = fp.encode(fmt, fp.decode(FP64, bits))[0]
        assert fp.from_double(fmt, value) == exact, hex(bits)


@pytest.mark.parametrize("fmt", [FP32, FP64], ids=lambda f: f.name)
def test_reduce_tree_matches_exact_tree(fmt):
    rng = random.Random(0x5E + fmt.ew)
    for width in (2, 4, 8, 16, 32):
        for _ in range(400):
            anchor = random_bits(rng, fmt)
            leaves = [near_bits(rng, fmt, anchor) if rng.random() < 0.8
                      else random_bits(rng, fmt) for _ in range(width)]
            assert fp.reduce_tree(fmt, leaves) == fp.exact_reduce_tree(
                fmt, leaves
            ), leaves


def test_reduce_tree_uses_pairwise_lane_order():
    one = fp.from_double(FP32, 1.0)
    big = fp.from_double(FP32, 2.0 ** 24)
    neg_big = big ^ FP32.sign_bit
    # ((2^24 + 1) + (-2^24 + 1)) rounds 2^24 + 1 to 2^24 first: result 0 + 1.
    assert fp.reduce_tree(FP32, [big, one, neg_big, one]) == one
    # Pairing the ones together is exact: (2^24 - 2^24) + (1 + 1) = 2.
    assert fp.reduce_tree(FP32, [big, neg_big, one, one]) == fp.from_double(
        FP32, 2.0)


# ---------------------------------------------------------------------------
# Named rules and defects
# ---------------------------------------------------------------------------

def test_fp16_subnormal_carry_rounds_to_minimum_normal():
    assert fp.mul(FP16, 0x0017, 0x5190)[0] == 0x0400
    assert fp.lane_mul(FP16, 0x0017, 0x5190) == 0x0400
    assert fp.lane_convert(FP16, FP32,
                           fp.product(FP32, FP16, 0x0017, 0x5190)[0]) == 0x0400


def test_bf16_product_rounds_at_binary32_range_limits():
    bf16_max = 0x7F7F
    assert fp.product(FP32, BF16, bf16_max, bf16_max)[0] == 0x7F80_0000
    assert fp.lane_product(FP32, BF16, bf16_max, bf16_max) == 0x7F80_0000
    tiny = 0x0001  # smallest BF16 subnormal, 2**-133
    assert fp.product(FP32, BF16, tiny, tiny)[0] == 0


def test_canonical_nan_and_invalid_operations():
    inf = FP32.infinity
    assert fp.mul(FP32, 0, inf) == (FP32.canonical_nan, fp.NV)
    assert fp.add(FP32, inf, inf | FP32.sign_bit) == (FP32.canonical_nan,
                                                     fp.NV)
    assert fp.fma(FP32, 0, inf, FP32.canonical_nan) == (FP32.canonical_nan,
                                                        fp.NV)
    assert fp.add(FP32, 0xFFC1_2345, 0)[0] == FP32.canonical_nan
    assert fp.add(FP32, 0x7F80_0001, 0) == (FP32.canonical_nan, fp.NV)
    assert fp.div(FP32, 0, 0) == (FP32.canonical_nan, fp.NV)
    assert fp.div(FP32, fp.from_double(FP32, 1.0), 0) == (inf, fp.DZ)
    assert fp.sqrt(FP32, fp.from_double(FP32, -1.0)) == (FP32.canonical_nan,
                                                         fp.NV)
    assert fp.sqrt(FP32, FP32.sign_bit) == (FP32.sign_bit, 0)
    for fmt in FORMATS:
        assert fp.lane_mul(fmt, 0, fmt.infinity) == fmt.canonical_nan
        assert fp.lane_fma(fmt, 0, fmt.infinity, 0) == fmt.canonical_nan


def test_signed_zero_rules():
    zero, neg_zero = 0, FP32.sign_bit
    one = fp.from_double(FP32, 1.0)
    assert fp.add(FP32, zero, neg_zero)[0] == zero
    assert fp.add(FP32, zero, neg_zero, fp.RDN)[0] == neg_zero
    assert fp.add(FP32, neg_zero, neg_zero)[0] == neg_zero
    assert fp.sub(FP32, one, one)[0] == zero
    assert fp.sub(FP32, one, one, fp.RDN)[0] == neg_zero
    assert fp.lane_fma(FP32, neg_zero, one, neg_zero) == neg_zero
    assert fp.lane_fma(FP32, neg_zero, one, zero) == zero


def test_underflow_uses_tininess_after_rounding():
    # (2**25 - 1) * 2**-151 lies just below 2**-126 and rounds up to it.
    bits, flags = fp.encode(FP32, (fp.FINITE, 0, (1 << 25) - 1, -151))
    assert bits == 0x0080_0000
    assert flags == fp.NX
    bits, flags = fp.encode(FP32, (fp.FINITE, 0, 3, -150))
    assert bits == 0x0000_0002
    assert flags == fp.NX | fp.UF


def test_overflow_by_rounding_direction():
    big = fp.from_double(FP32, 3.0e38)
    for rm, expected in ((fp.RNE, FP32.infinity), (fp.RTZ, FP32.max_finite),
                         (fp.RDN, FP32.max_finite), (fp.RUP, FP32.infinity),
                         (fp.RMM, FP32.infinity)):
        assert fp.add(FP32, big, big, rm) == (expected, fp.OF | fp.NX)
    neg = big | FP32.sign_bit
    assert fp.add(FP32, neg, neg, fp.RDN)[0] == FP32.sign_bit | FP32.infinity
    assert fp.add(FP32, neg, neg, fp.RUP)[0] == FP32.sign_bit | FP32.max_finite


def test_integer_conversions_saturate_and_map_nan_to_zero():
    d = lambda value: fp.from_double(FP64, value)  # noqa: E731
    assert fp.to_int(FP64, d(2.5), 64, True, fp.RTZ) == (2, fp.NX)
    assert fp.to_int(FP64, d(2.5), 64, True, fp.RNE) == (2, fp.NX)
    assert fp.to_int(FP64, d(3.5), 64, True, fp.RNE) == (4, fp.NX)
    assert fp.to_int(FP64, d(-2.5), 64, True, fp.RMM) == (
        (-3) & (2**64 - 1), fp.NX)
    assert fp.to_int(FP64, d(1e30), 64, True) == ((1 << 63) - 1, fp.NV)
    assert fp.to_int(FP64, d(-1e30), 64, True) == (1 << 63, fp.NV)
    assert fp.to_int(FP64, d(-0.5), 64, False, fp.RTZ) == (0, fp.NX)
    assert fp.to_int(FP64, d(-1.0), 64, False) == (0, fp.NV)
    assert fp.to_int(FP64, FP64.canonical_nan, 64, True) == (0, fp.NV)
    assert fp.from_int(FP32, (1 << 24) + 1) == (0x4B80_0000, fp.NX)


def test_round_integral():
    d = lambda value: fp.from_double(FP32, value)  # noqa: E731
    assert fp.round_integral(FP32, d(2.5), fp.RNE) == (d(2.0), 0)
    assert fp.round_integral(FP32, d(2.5), fp.RMM) == (d(3.0), 0)
    assert fp.round_integral(FP32, d(-2.5), fp.RDN) == (d(-3.0), 0)
    assert fp.round_integral(FP32, d(-0.5), fp.RUP) == (FP32.sign_bit, 0)
    assert fp.round_integral(FP32, d(0.4), fp.RDN) == (0, 0)
    assert fp.round_integral(FP32, d(1e20), fp.RTZ) == (d(1e20), 0)


def test_classify_and_compare():
    masks = [fp.classify(FP32, bits) for bits in (
        0xFF80_0000, 0xBF80_0000, 0x8000_0001, 0x8000_0000,
        0x0000_0000, 0x0000_0001, 0x3F80_0000, 0x7F80_0000,
        0x7F80_0001, 0x7FC0_0000)]
    assert masks == [1 << i for i in range(10)]
    assert fp.compare(FP32, 0, FP32.sign_bit) == fp.EQUAL
    assert fp.compare(FP32, FP32.infinity, FP32.infinity) == fp.EQUAL
    assert fp.compare(FP32, FP32.infinity | FP32.sign_bit,
                      FP32.infinity) == fp.LESS
    assert fp.compare(FP32, FP32.canonical_nan, 0) == fp.UNORDERED


def test_minimum_maximum_and_nan_skipping_extremes():
    zero, neg_zero = 0, FP16.sign_bit
    assert fp.minimum(FP16, zero, neg_zero) == neg_zero
    assert fp.maximum(FP16, neg_zero, zero) == zero
    assert fp.minimum(FP16, 0x3C00, 0x7C01) == FP16.canonical_nan
    one, two = 0x3C00, 0x4000
    lanes = [0x7E00, two, one, 0xFE00, one, neg_zero]
    assert fp.extreme_index(FP16, lanes, largest=False) == (5, neg_zero)
    assert fp.extreme_index(FP16, lanes[:5], largest=False) == (2, one)
    assert fp.extreme_index(FP16, lanes, largest=True) == (1, two)
    assert fp.extreme_index(FP16, [0x7E00, 0xFE01], largest=True) == (
        0, FP16.canonical_nan)
    assert fp.skip_nan_extreme(FP16, 0x7E00, one, largest=False) == one


def test_narrow_formats_constant_matches_precision_bound():
    for fmt in NARROW_FORMATS:
        assert 53 >= 2 * fmt.precision + 2
