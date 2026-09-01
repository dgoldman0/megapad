"""Unchanged-source acceptance for KDOS micro-cluster support and MPU."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError, ForthAbort
from simulator.memory import UnmappedAddressError
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    MegaForthRuntime,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_run_queues import _load_run_queues
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-micro-clusters-7462-7569.f"
)

FIRST_LINE = 7462
FIXTURE_LAST_LINE = 7569
LAST_LINE = 7568
FIXTURE_BYTES = 3_755
FIXTURE_SHA256 = (
    "ab779a33407b88f24c44bd058ce9c389e90531c80506ef0b16a6aab43c6062cc"
)
FIXTURE_GIT_BLOB = "a89cdb704ab86fcb210d1d806e4a4858466309a2"
SLICE_BYTES = 3_693
SLICE_SHA256 = (
    "7f349876f58c132cf72f116c0fa764a97ff0963679abb78d961e4f9a08770932"
)
SLICE_GIT_BLOB = "3c13145b43c2eadc14841326f2fef22d34d01b6a"
HOSTED_DICTIONARY_GROWTH = 398
HOSTED_WORD_FIXED_BYTES = 17
CLUSTER_SPAD_ADDRESS = 0xFFFF_FE00_0000_0000
SPAD_TO_BANK0_OFFSET = (-CLUSTER_SPAD_ADDRESS) & MASK64
FIRST_SIGNED_CELL = 1 << 63

SOURCE_LEDGER = (
    ("CONSTANT", b"NUM-CLUSTERS"),
    (":", b"CLUSTER-ENABLE"),
    (":", b"CLUSTER-DISABLE"),
    (":", b"CLUSTERS-ON"),
    (":", b"CLUSTERS-OFF"),
    (":", b"CLUSTER-STATE"),
    (":", b"HW-BARRIER-WAIT"),
    (":", b"SPAD-C@"),
    (":", b"SPAD-C!"),
    (":", b"CL-MPU-SETUP"),
    (":", b"CL-ENTER-USER"),
    (":", b"CL-EXIT-USER"),
    (":", b"CL-MPU-OFF"),
    (":", b".CL-MPU"),
)
DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)


def _verified_slice() -> bytes:
    fixture = FIXTURE.read_bytes()
    assert len(fixture) == FIXTURE_BYTES
    assert fixture.count(b"\n") == FIXTURE_LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(fixture).hexdigest() == FIXTURE_SHA256
    assert _git_blob_id(fixture) == FIXTURE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert fixture == b"".join(lines[FIRST_LINE - 1 : FIXTURE_LAST_LINE])
    boundary = (
        b"\\ -- Forward declarations for \xc2\xa710 words needed by \xc2\xa79 TUI --\n"
    )
    assert lines[LAST_LINE - 1] == b"\n"
    assert lines[LAST_LINE] == boundary
    assert fixture.endswith(boundary)
    # Keep the terminal blank seam self-contained while authenticating the
    # following section marker; only the exact 7462–7568 prefix is evaluated.
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_micro_clusters(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_micro_clusters() -> MegaForthRuntime:
    return _evaluate_micro_clusters(_load_run_queues())


def test_cluster_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_run_queues()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    assert _execute(runtime, "SPIN@", 3) == (0,)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_micro_clusters(runtime)

    assert len(SOURCE_LEDGER) == 14
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name)
        for _definer, name in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((definer, name), word) in enumerate(
        zip(SOURCE_LEDGER, published)
    ):
        assert word.name == name
        assert runtime.memory.read64(word.header_address) == prior_header
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following == word.body_address
        expected_type = (
            ConstantDefinition if definer == "CONSTANT" else ColonDefinition
        )
        assert isinstance(word.implementation, expected_type)
        prior_header = word.header_address

    assert _constant(runtime, "NUM-CLUSTERS") == 3
    assert _execute(runtime, "CLUSTER-EN@") == (0,)
    assert runtime.timer.counter > counter_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.spinlocks.owners == locks_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"


def test_cluster_mask_wrappers_preserve_the_zero_capability_boundary() -> None:
    runtime = _load_micro_clusters()

    for cluster_id in range(3):
        assert _execute(runtime, "CLUSTER-DISABLE", cluster_id) == ()
    assert _execute(runtime, "CLUSTERS-OFF") == ()
    assert _execute(runtime, "CLUSTER-EN@") == (0,)

    for cluster_id in range(3):
        context = runtime.new_context()
        context.data.push(0xCAFE)
        context.data.push(cluster_id)
        with pytest.raises(ExecutionError, match="cannot enable micro-core"):
            runtime.execute("CLUSTER-ENABLE", context=context, step_budget=500)
        assert context.data.snapshot() == (0xCAFE, 1 << cluster_id)
        assert context.returns.snapshot() == ()
        assert _execute(runtime, "CLUSTER-EN@") == (0,)

    context = runtime.new_context()
    context.data.push(0xCAFE)
    with pytest.raises(ExecutionError, match="cannot enable micro-core"):
        runtime.execute("CLUSTERS-ON", context=context, step_budget=500)
    assert context.data.snapshot() == (0xCAFE, 7)
    assert context.returns.snapshot() == ()

    for cluster_id in (MASK64, 3):
        context = runtime.new_context()
        context.data.push(0xCAFE)
        context.data.push(cluster_id)
        with pytest.raises(ForthAbort, match='Forth ABORT"'):
            runtime.execute("CLUSTER-ENABLE", context=context, step_budget=500)
        assert context.data.snapshot() == ()
        assert context.returns.snapshot() == ()
        assert runtime.drain_uart_output() == b"Invalid cluster ID"
        assert _execute(runtime, "CLUSTER-EN@") == (0,)


def test_cluster_state_reports_three_source_declared_absent_clusters() -> None:
    runtime = _load_micro_clusters()

    assert _execute(runtime, "CLUSTER-STATE") == ()

    assert runtime.drain_uart_output() == (
        b" Clusters: 0  (mask)\r\n"
        b"   Cluster 0   disabled\r\n"
        b"   Cluster 1   disabled\r\n"
        b"   Cluster 2   disabled\r\n"
    )


def test_barrier_and_scratchpad_wrappers_fail_at_the_honest_boundary() -> None:
    runtime = _load_micro_clusters()
    barrier_context = runtime.new_context()
    barrier_context.data.push(0xCAFE)

    with pytest.raises(ExecutionError, match="BARRIER-ARRIVE is unavailable"):
        runtime.execute(
            "HW-BARRIER-WAIT",
            context=barrier_context,
            step_budget=100,
        )
    assert barrier_context.data.snapshot() == (0xCAFE,)
    assert barrier_context.returns.snapshot() == ()

    fetch_context = runtime.new_context()
    fetch_context.data.push(0xCAFE)
    fetch_context.data.push(0x23)
    with pytest.raises(UnmappedAddressError):
        runtime.execute("SPAD-C@", context=fetch_context, step_budget=100)
    assert fetch_context.data.snapshot() == (
        0xCAFE,
        CLUSTER_SPAD_ADDRESS + 0x23,
    )
    assert fetch_context.returns.snapshot() == ()

    store_context = runtime.new_context()
    store_context.data.push(0xCAFE)
    store_context.data.push(0xA5)
    store_context.data.push(0x23)
    with pytest.raises(UnmappedAddressError):
        runtime.execute("SPAD-C!", context=store_context, step_budget=100)
    assert store_context.data.snapshot() == (0xCAFE,)
    assert store_context.returns.snapshot() == ()

    # The source does not bound offsets. Cell wrapping can therefore leave the
    # sentinel aperture instead of failing closed.
    runtime.memory.write8(0, 0xA5)
    assert _execute(runtime, "SPAD-C@", SPAD_TO_BANK0_OFFSET) == (0xA5,)
    assert _execute(runtime, "SPAD-C!", 0x5A, SPAD_TO_BANK0_OFFSET) == ()
    assert runtime.memory.read8(0) == 0x5A


def test_cluster_mpu_wrappers_expose_exact_partial_source_ordering() -> None:
    runtime = _load_micro_clusters()
    cases = (
        ("CL-MPU-SETUP", (0xCAFE, 0x1000, 0x2000)),
        ("CL-ENTER-USER", (0xCAFE, 1)),
        ("CL-EXIT-USER", (0xCAFE, 0)),
        ("CL-MPU-OFF", (0xCAFE, 0)),
    )
    for word, expected in cases:
        context = runtime.new_context()
        inputs = expected if word == "CL-MPU-SETUP" else expected[:1]
        for value in inputs:
            context.data.push(value)
        with pytest.raises(ExecutionError, match="is unavailable"):
            runtime.execute(word, context=context, step_budget=100)
        assert context.data.snapshot() == expected
        assert context.returns.snapshot() == ()

    display_context = runtime.new_context()
    display_context.data.push(0xCAFE)
    with pytest.raises(ExecutionError, match="CL-PRIV@ is unavailable"):
        runtime.execute(".CL-MPU", context=display_context, step_budget=100)
    assert display_context.data.snapshot() == (0xCAFE,)
    assert display_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b" Cluster MPU:\r\n   priv = "


def test_bios_and_kdos_core_classifiers_retain_their_signedness_difference() -> None:
    runtime = _load_micro_clusters()

    assert _execute(runtime, "MICRO?", 0) == (0,)
    assert _execute(runtime, "MICRO-CORE?", 0) == (0,)
    assert _execute(runtime, "MICRO?", 1) == (MASK64,)
    assert _execute(runtime, "MICRO-CORE?", 1) == (MASK64,)
    assert _execute(runtime, "MICRO?", FIRST_SIGNED_CELL) == (MASK64,)
    assert _execute(runtime, "MICRO-CORE?", FIRST_SIGNED_CELL) == (0,)
    assert _execute(runtime, "FULL-CORE?", FIRST_SIGNED_CELL) == (MASK64,)
