"""Assertive bit-level contracts for the emulator's floating-point formats."""

from __future__ import annotations

import math

import pytest

from megapad64 import (
    _bf16_to_float,
    _bits_to_fp32,
    _float_to_bf16,
    _float_to_fp16,
    _fp16_to_float,
    _fp32_to_bits,
)


FP32_OVERFLOW_MIDPOINT = float.fromhex("0x1.ffffffp+127")


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(0.0, 0x0000, id="positive-zero"),
        pytest.param(-0.0, 0x8000, id="negative-zero"),
        pytest.param(float("inf"), 0x7C00, id="positive-infinity"),
        pytest.param(float("-inf"), 0xFC00, id="negative-infinity"),
        pytest.param(65504.0, 0x7BFF, id="maximum-finite"),
        pytest.param(65520.0, 0x7C00, id="overflow-midpoint"),
        pytest.param(1e300, 0x7C00, id="binary32-overflow-positive"),
        pytest.param(-1e300, 0xFC00, id="binary32-overflow-negative"),
    ],
)
def test_float_to_fp16_special_values_and_overflow(
    value: float,
    expected: int,
) -> None:
    assert _float_to_fp16(value) == expected


def test_float_to_fp16_canonicalizes_nan() -> None:
    assert _float_to_fp16(float("nan")) == 0x7E00


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(1.0 + 2.0**-11, 0x3C00, id="normal-tie-even-down"),
        pytest.param(1.0 + 3.0 * 2.0**-11, 0x3C02, id="normal-tie-even-up"),
        pytest.param(2.0**-25, 0x0000, id="subnormal-tie-even-down"),
        pytest.param(3.0 * 2.0**-25, 0x0002, id="subnormal-tie-even-up"),
    ],
)
def test_float_to_fp16_rounds_to_nearest_even(
    value: float,
    expected: int,
) -> None:
    assert _float_to_fp16(value) == expected


def test_fp16_decode_preserves_subnormal_and_signed_zero() -> None:
    assert _fp16_to_float(0x0001) == 2.0**-24
    assert math.copysign(1.0, _fp16_to_float(0x8000)) == -1.0
    assert math.isinf(_fp16_to_float(0x7C00))
    assert math.isnan(_fp16_to_float(0x7E00))


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(0.0, 0x0000, id="positive-zero"),
        pytest.param(-0.0, 0x8000, id="negative-zero"),
        pytest.param(float("inf"), 0x7F80, id="positive-infinity"),
        pytest.param(float("-inf"), 0xFF80, id="negative-infinity"),
        pytest.param(1e300, 0x7F80, id="binary32-overflow-positive"),
        pytest.param(-1e300, 0xFF80, id="binary32-overflow-negative"),
    ],
)
def test_float_to_bf16_special_values_and_overflow(
    value: float,
    expected: int,
) -> None:
    assert _float_to_bf16(value) == expected


@pytest.mark.parametrize(
    ("fp32_bits", "expected"),
    [
        pytest.param(0x7FFF_FFFF, 0x7FFF, id="positive-maximal-payload"),
        pytest.param(0xFFFF_FFFF, 0xFFFF, id="negative-maximal-payload"),
        pytest.param(0x7F80_0001, 0x7FC0, id="positive-low-payload"),
        pytest.param(0xFF80_0001, 0xFFC0, id="negative-low-payload"),
        pytest.param(0x7FCD_0000, 0x7FCD, id="preserved-high-payload"),
    ],
)
def test_float_to_bf16_preserves_a_valid_quiet_nan(
    fp32_bits: int,
    expected: int,
) -> None:
    encoded = _float_to_bf16(_bits_to_fp32(fp32_bits))

    assert encoded == expected
    assert math.isnan(_bf16_to_float(encoded))


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(1.0 + 2.0**-8, 0x3F80, id="normal-tie-even-down"),
        pytest.param(
            1.0 + 3.0 * 2.0**-8,
            0x3F82,
            id="normal-tie-even-up",
        ),
        pytest.param(2.0**-134, 0x0000, id="subnormal-tie-even-down"),
        pytest.param(
            3.0 * 2.0**-134,
            0x0002,
            id="subnormal-tie-even-up",
        ),
        pytest.param(
            _bits_to_fp32(0x7F7F_7FFF),
            0x7F7F,
            id="maximum-finite-round-down",
        ),
        pytest.param(
            _bits_to_fp32(0x7F7F_8000),
            0x7F80,
            id="finite-overflow-midpoint",
        ),
        pytest.param(
            _bits_to_fp32(0xFF7F_7FFF),
            0xFF7F,
            id="negative-maximum-finite-round-down",
        ),
        pytest.param(
            _bits_to_fp32(0xFF7F_8000),
            0xFF80,
            id="negative-finite-overflow-midpoint",
        ),
    ],
)
def test_float_to_bf16_rounds_binary32_to_nearest_even(
    value: float,
    expected: int,
) -> None:
    assert _float_to_bf16(value) == expected


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(
            float.fromhex("0x1.0100004p0"),
            0x3F80,
            id="first-double-round-boundary",
        ),
        pytest.param(
            float.fromhex("0x1.02ffffcp0"),
            0x3F82,
            id="second-double-round-boundary",
        ),
    ],
)
def test_float_to_bf16_preserves_binary32_staging(
    value: float,
    expected: int,
) -> None:
    assert _float_to_bf16(value) == expected


def test_bf16_decode_preserves_subnormal_nan_and_signed_zero() -> None:
    assert _bf16_to_float(0x0001) == 2.0**-133
    assert math.copysign(1.0, _bf16_to_float(0x8000)) == -1.0
    assert math.isinf(_bf16_to_float(0x7F80))
    assert math.isnan(_bf16_to_float(0x7FC1))


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(0.0, 0x0000_0000, id="positive-zero"),
        pytest.param(-0.0, 0x8000_0000, id="negative-zero"),
        pytest.param(float("inf"), 0x7F80_0000, id="positive-infinity"),
        pytest.param(float("-inf"), 0xFF80_0000, id="negative-infinity"),
        pytest.param(
            float.fromhex("0x1.fffffep+127"),
            0x7F7F_FFFF,
            id="maximum-finite",
        ),
        pytest.param(
            math.nextafter(FP32_OVERFLOW_MIDPOINT, 0.0),
            0x7F7F_FFFF,
            id="just-below-positive-overflow",
        ),
        pytest.param(
            -math.nextafter(FP32_OVERFLOW_MIDPOINT, 0.0),
            0xFF7F_FFFF,
            id="just-below-negative-overflow",
        ),
        pytest.param(
            FP32_OVERFLOW_MIDPOINT,
            0x7F80_0000,
            id="positive-overflow-midpoint",
        ),
        pytest.param(
            -FP32_OVERFLOW_MIDPOINT,
            0xFF80_0000,
            id="negative-overflow-midpoint",
        ),
        pytest.param(1e300, 0x7F80_0000, id="large-positive-overflow"),
        pytest.param(-1e300, 0xFF80_0000, id="large-negative-overflow"),
    ],
)
def test_fp32_conversion_handles_boundaries_and_overflow(
    value: float,
    expected: int,
) -> None:
    assert _fp32_to_bits(value) == expected


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        pytest.param(1.0 + 2.0**-24, 0x3F80_0000, id="tie-even-down"),
        pytest.param(
            1.0 + 3.0 * 2.0**-24,
            0x3F80_0002,
            id="tie-even-up",
        ),
    ],
)
def test_fp32_conversion_rounds_to_nearest_even(
    value: float,
    expected: int,
) -> None:
    assert _fp32_to_bits(value) == expected


def test_fp32_decode_preserves_nan_and_signed_zero() -> None:
    assert math.copysign(1.0, _bits_to_fp32(0x8000_0000)) == -1.0
    assert math.isinf(_bits_to_fp32(0x7F80_0000))
    assert math.isnan(_bits_to_fp32(0x7FC0_0001))
