"""The tile FMA oracle remains fused without Python's optional math.fma."""

from __future__ import annotations

import pytest

from shared import ieee_fp as fp


F64_ONE = 0x3FF0000000000000
F64_TWO = 0x4000000000000000
F64_HALF = 0x3FE0000000000000
F64_NEG_ONE = 0xBFF0000000000000
F64_NEG_ZERO = 0x8000000000000000
F64_INF = 0x7FF0000000000000
F64_NAN = 0x7FF8000000000000


# These expected bits are pinned independently of the arithmetic under test.
# Both cancellation residuals follow (1 + e)(1 - e) - 1 = -e**2.
CASES = (
    pytest.param(fp.FP64, 0x3FF0000000000001, 0x3FEFFFFFFFFFFFFE,
                 F64_NEG_ONE, 0xB970000000000000, id="fused-residual-2^-104"),
    pytest.param(fp.FP64, F64_TWO, F64_ONE, 0xC000000000000000,
                 0, id="exact-cancellation"),
    pytest.param(fp.FP64, F64_NEG_ZERO, F64_ONE, F64_NEG_ZERO,
                 F64_NEG_ZERO, id="negative-zero"),
    pytest.param(fp.FP64, F64_NEG_ZERO, F64_ONE, 0,
                 0, id="opposed-zeros"),
    pytest.param(fp.FP64, 1, F64_TWO, 0, 2, id="exact-subnormal"),
    pytest.param(fp.FP64, 1, F64_HALF, 0, 0, id="underflow-tie-to-zero"),
    pytest.param(fp.FP64, 3, F64_HALF, 0, 2, id="subnormal-tie-to-even"),
    pytest.param(fp.FP64, 1, F64_HALF, 1, 2, id="subnormal-single-round"),
    pytest.param(fp.FP64, F64_INF, F64_ONE, 0, F64_INF, id="infinity"),
    pytest.param(fp.FP64, 0, F64_INF, F64_ONE, F64_NAN, id="zero-times-inf"),
    pytest.param(fp.FP64, F64_INF, F64_ONE, 0xFFF0000000000000,
                 F64_NAN, id="opposed-infinities"),
    pytest.param(fp.FP64, 0x7FF8000000012345, F64_ONE, 0,
                 F64_NAN, id="quiet-nan-canonical"),
    pytest.param(fp.FP64, 0x7FF0000000000001, F64_ONE, 0,
                 F64_NAN, id="signalling-nan-canonical"),
    pytest.param(fp.FP64, 0x7FEFFFFFFFFFFFFF, F64_TWO, 0xFFEFFFFFFFFFFFFF,
                 0x7FEFFFFFFFFFFFFF, id="finite-after-product-overflow"),
    pytest.param(fp.FP64, 0x7FEFFFFFFFFFFFFF, F64_TWO, 0,
                 F64_INF, id="positive-overflow"),
    pytest.param(fp.FP64, 0xFFEFFFFFFFFFFFFF, F64_TWO, 0,
                 0xFFF0000000000000, id="negative-overflow"),
    pytest.param(fp.FP32, 0x3F800001, 0x3F7FFFFE, F64_NEG_ONE,
                 0xBD10000000000000, id="fp32-to-fp64-residual-2^-46"),
    pytest.param(fp.FP32, 1, 0x3F800000, 0,
                 0x36A0000000000000, id="fp32-subnormal-widens-exactly"),
    pytest.param(fp.FP32, 0x80000000, 0x3F800000, F64_NEG_ZERO,
                 F64_NEG_ZERO, id="fp32-negative-zero"),
)


@pytest.mark.parametrize("host_available", (False, True), ids=("exact", "host"))
@pytest.mark.parametrize("src,a,b,c,expected", CASES)
def test_fp64_tile_fma_with_and_without_host_support(
    monkeypatch, host_available: bool, src, a: int, b: int, c: int, expected: int,
) -> None:
    exact_fma = fp.fma
    calls = []

    if host_available:
        def host_fma(x, y, z):
            # A deterministic stand-in lets Python 3.12 exercise the available
            # branch too. Expected results above do not come from this stub.
            bits = (fp.double_bits(x), fp.double_bits(y), fp.double_bits(z))
            calls.append(bits)
            result, flags = exact_fma(fp.FP64, *bits)
            if flags & fp.NV:
                raise ValueError("invalid fused operation")
            if flags & fp.OF:
                raise OverflowError("fused overflow")
            return fp.to_double(fp.FP64, result)

        def forbidden_fallback(*_args, **_kwargs):
            raise AssertionError("available math.fma must be selected")

        monkeypatch.setattr(fp.math, "fma", host_fma, raising=False)
        monkeypatch.setattr(fp, "fma", forbidden_fallback)
    else:
        def tracked_exact(fmt, x, y, z, rm=fp.RNE):
            assert fmt is fp.FP64
            assert rm == fp.RNE
            calls.append((x, y, z))
            return exact_fma(fmt, x, y, z, rm)

        monkeypatch.delattr(fp.math, "fma", raising=False)
        monkeypatch.setattr(fp, "fma", tracked_exact)

    actual = (
        fp.lane_fma(fp.FP64, a, b, c)
        if src is fp.FP64 else
        fp.lane_mixed_fma(fp.FP64, src, a, b, c)
    )

    assert actual == expected
    if fp.is_nan(src, a) or fp.is_nan(src, b) or fp.is_nan(fp.FP64, c):
        assert calls == []  # canonicalize before either executor sees a NaN
    else:
        assert calls == [(
            fp.convert(fp.FP64, src, a)[0],
            fp.convert(fp.FP64, src, b)[0],
            c,
        )]


def test_missing_host_fma_keeps_the_fused_cancellation_residual(monkeypatch) -> None:
    monkeypatch.delattr(fp.math, "fma", raising=False)
    a, b, c = 0x3FF0000000000001, 0x3FEFFFFFFFFFFFFE, F64_NEG_ONE

    fused = fp.lane_fma(fp.FP64, a, b, c)
    separately_rounded = fp.add(fp.FP64, fp.mul(fp.FP64, a, b)[0], c)[0]

    assert fused == 0xB970000000000000
    assert separately_rounded == 0


def test_missing_host_fma_keeps_subnormal_single_rounding(monkeypatch) -> None:
    monkeypatch.delattr(fp.math, "fma", raising=False)

    assert fp.lane_fma(fp.FP64, 1, F64_HALF, 1) == 2
    assert fp.add(fp.FP64, fp.mul(fp.FP64, 1, F64_HALF)[0], 1)[0] == 1
