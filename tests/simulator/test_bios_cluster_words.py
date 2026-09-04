"""Focused one-core pseudo-BIOS micro-cluster boundary coverage."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError
from simulator.memory import UnmappedAddressError
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition


CLUSTER_SPAD_ADDRESS = 0xFFFF_FE00_0000_0000


def test_cluster_enable_mask_exposes_capability_truth_without_fake_state() -> None:
    runtime = MegaForthRuntime()
    other = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)

    runtime.execute("CLUSTER-EN@", context=context)
    assert context.data.snapshot() == (0xCAFE, 0)

    runtime.execute("CLUSTER-EN!", context=context)
    assert context.data.snapshot() == (0xCAFE,)

    other_context = other.new_context()
    other.execute("CLUSTER-EN@", context=other_context)
    assert other_context.data.snapshot() == (0,)


@pytest.mark.parametrize("mask", (1, 7, MASK64))
def test_cluster_enable_rejects_nonzero_masks_without_consuming_them(
    mask: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(mask)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match="cannot enable micro-core"):
        runtime.execute("CLUSTER-EN!", context=context)

    assert context.data.snapshot() == (0xCAFE, mask)
    assert context.returns.snapshot() == (0xBEEF,)
    check = runtime.new_context()
    runtime.execute("CLUSTER-EN@", context=check)
    assert check.data.snapshot() == (0,)


@pytest.mark.parametrize("word", ("BARRIER-ARRIVE", "BARRIER-STATUS"))
def test_cluster_barrier_words_fail_without_spinning_or_changing_stacks(
    word: str,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match="micro-core cluster"):
        runtime.execute(word, context=context)

    assert context.data.snapshot() == (0xCAFE,)
    assert context.returns.snapshot() == (0xBEEF,)


def test_spad_returns_the_native_sentinel_without_fabricating_storage() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.returns.push(0xBEEF)

    runtime.execute("SPAD", context=context)

    assert context.data.snapshot() == (0xCAFE, CLUSTER_SPAD_ADDRESS)
    assert context.returns.snapshot() == (0xBEEF,)
    with pytest.raises(UnmappedAddressError):
        runtime.execute("C@", context=context)
    assert context.data.snapshot() == (0xCAFE, CLUSTER_SPAD_ADDRESS)
    assert context.returns.snapshot() == (0xBEEF,)


@pytest.mark.parametrize(
    ("core_id", "expected"),
    ((0, 0), (1, MASK64), (MASK64, MASK64)),
)
def test_micro_question_preserves_the_bios_unsigned_threshold(
    core_id: int,
    expected: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(core_id)
    context.returns.push(0xBEEF)

    runtime.execute("MICRO?", context=context)

    assert context.data.snapshot() == (0xCAFE, expected)
    assert context.returns.snapshot() == (0xBEEF,)


@pytest.mark.parametrize(
    "word",
    ("CL-PRIV!", "CL-MPU-BASE!", "CL-MPU-LIMIT!"),
)
def test_cluster_mpu_stores_fail_without_consuming_the_operand(
    word: str,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(0x1234)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match=f"{word} is unavailable"):
        runtime.execute(word, context=context)

    assert context.data.snapshot() == (0xCAFE, 0x1234)
    assert context.returns.snapshot() == (0xBEEF,)


@pytest.mark.parametrize(
    "word",
    ("CL-PRIV@", "CL-MPU-BASE@", "CL-MPU-LIMIT@"),
)
def test_cluster_mpu_fetches_fail_without_pushing_a_fake_value(word: str) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match=f"{word} is unavailable"):
        runtime.execute(word, context=context)

    assert context.data.snapshot() == (0xCAFE,)
    assert context.returns.snapshot() == (0xBEEF,)


def test_cluster_words_are_live_primitives_without_guest_shadow_state() -> None:
    runtime = MegaForthRuntime()
    for name in (
        "CLUSTER-EN!",
        "CLUSTER-EN@",
        "BARRIER-ARRIVE",
        "BARRIER-STATUS",
        "SPAD",
        "MICRO?",
        "CL-PRIV!",
        "CL-PRIV@",
        "CL-MPU-BASE!",
        "CL-MPU-LIMIT!",
        "CL-MPU-BASE@",
        "CL-MPU-LIMIT@",
    ):
        word = runtime.find(name)
        assert word is not None
        assert isinstance(word.implementation, PrimitiveDefinition)

    n_full = runtime.find("N-FULL")
    micro = runtime.find("MICRO?")
    assert n_full is not None
    assert micro is not None
    assert runtime.memory.read64(micro.header_address) == n_full.header_address
