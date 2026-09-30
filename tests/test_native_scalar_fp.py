"""Exact native scalar values against the independent Python IEEE oracle."""
from __future__ import annotations

import importlib
import random

import pytest

from shared import ieee_fp, scalar_fp


@pytest.fixture(params=("_megaforth_native", "_mp64_accel"))
def kernel(request):
    return importlib.import_module(request.param)


def _expected(op, rd, rs, rt, csr):
    result = scalar_fp.execute(op, rd, rs, rt, csr)
    return result.value, result.flags, result.relation


def _edges(fmt):
    magnitudes = (
        0, 1, 2, fmt.fraction_mask, 1 << fmt.fraction_bits,
        (1 << fmt.fraction_bits) + 1, fmt.max_finite - 1, fmt.max_finite,
        fmt.infinity, fmt.infinity | 1, fmt.canonical_nan,
        fmt.canonical_nan | 1, fmt.bias << fmt.fraction_bits,
        (fmt.bias << fmt.fraction_bits) + 1,
    )
    return tuple(value | sign for sign in (0, fmt.sign_bit) for value in magnitudes)


def test_native_encoding_validation_matches_oracle(kernel):
    for op in range(256):
        for tail in (0, 31, 32, 128, 255):
            for rm in range(8):
                csr = rm | scalar_fp.FPCSR_WRITE_MASK & ~7
                try:
                    scalar_fp.validate(op, tail, csr)
                except scalar_fp.IllegalOperation:
                    with pytest.raises(ValueError):
                        kernel.scalar_fp_validate(op, tail, csr)
                else:
                    kernel.scalar_fp_validate(op, tail, csr)


@pytest.mark.parametrize("fmt", (ieee_fp.FP32, ieee_fp.FP64), ids=lambda f: f.name)
def test_native_all_operations_modes_edges_and_seeded_values(kernel, fmt):
    rng = random.Random(0xFC0000 + fmt.width)
    edges = _edges(fmt)
    for code in range(64):
        op = (64 if fmt is ieee_fp.FP64 else 0) | code
        for rm in range(8):
            try:
                scalar_fp.validate(op, 0, rm)
            except scalar_fp.IllegalOperation:
                continue
            inputs = [
                (left, right, edges[(index * 7 + 3) % len(edges)])
                for index, left in enumerate(edges)
                for right in edges
            ]
            inputs.extend(tuple(rng.getrandbits(64) for _ in range(3))
                          for _ in range(96))
            for rd, rs, rt in inputs:
                expected = _expected(op, rd, rs, rt, rm)
                actual = kernel.scalar_fp_execute(op, rd, rs, rt, rm)
                assert actual == expected, (op, rm, hex(rd), hex(rs), hex(rt))


@pytest.mark.parametrize("source,code", (
    (ieee_fp.FP16, scalar_fp.FCVT_F_H),
    (ieee_fp.BF16, scalar_fp.FCVT_F_B),
), ids=("fp16", "bf16"))
def test_native_all_narrow_patterns_widen_exactly(kernel, source, code):
    for format_bits in (0, 64):
        op = format_bits | code
        for bits in range(1 << 16):
            # Fixed exact widening ignores reserved dynamic rounding modes.
            assert kernel.scalar_fp_execute(op, 0, bits, 0, 7) == (
                _expected(op, 0, bits, 0, 7)
            ), (source.name, op, hex(bits))


def test_native_fma_alignment_extremes_and_cancellation(kernel):
    fmt = ieee_fp.FP64
    values = (1, fmt.fraction_mask, 1 << 52, fmt.max_finite)
    for rm in range(5):
        for a in values:
            for b in values:
                for c in values:
                    for sign in (0, fmt.sign_bit):
                        for code in (scalar_fp.FMA, scalar_fp.FMS):
                            op = 64 | code
                            assert kernel.scalar_fp_execute(op, c | sign, a, b, rm) == (
                                _expected(op, c | sign, a, b, rm)
                            ), (code, rm, hex(a), hex(b), hex(c | sign))


def test_native_hosted_service_bypasses_python_value_oracle(monkeypatch):
    from simulator.runtime import MegaForthRuntime

    runtime = MegaForthRuntime(execution_backend="native")
    def forbidden(*args, **kwargs):
        raise AssertionError("native service called the Python FP value oracle")
    monkeypatch.setattr(scalar_fp, "execute", forbidden)
    runtime.main_context.data.push(0x3FF0000000000000)
    runtime.main_context.data.push(0x4000000000000000)
    runtime.execute("F64+")
    assert runtime.main_context.data.snapshot() == (0x4008000000000000,)
    assert runtime.scalar_float.fpcsr == 0


def test_stale_native_extension_is_rejected_or_auto_falls_back(monkeypatch):
    import sys
    from types import SimpleNamespace
    from simulator.native_execution import NativeExecutor

    monkeypatch.setitem(sys.modules, "_megaforth_native", SimpleNamespace())
    with pytest.raises(RuntimeError, match="matching.*build"):
        NativeExecutor.create(None, required=True, admit_core=True)
    assert NativeExecutor.create(None, required=False, admit_core=True) is None
