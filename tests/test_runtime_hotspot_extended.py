"""Opt-in hotspot cases keep independent oracles and bounded host work."""

from __future__ import annotations

import ast
from pathlib import Path
from types import SimpleNamespace

import pytest

import bench_runtime_hotspots as benchmark
from simulator.runtime import BlockedExecution, ExecutionResult, YieldedExecution


def _capture_fixture(monkeypatch, workload, iterations=2):
    import simulator.runtime as runtime_module

    original = runtime_module.MegaForthRuntime
    runtimes = []

    def construct(*args, **kwargs):
        runtime = original(*args, **kwargs)
        runtimes.append(runtime)
        return runtime

    monkeypatch.setattr(runtime_module, "MegaForthRuntime", construct)
    fixture = benchmark.prepare_semantic(workload, "simulator-python", iterations)
    return fixture, runtimes[0]


def test_extended_cases_are_opt_in_and_existing_defaults_are_unchanged():
    defaults = benchmark.build_parser().parse_args([])
    assert defaults.workload == ["fp64", "tile-fp64", "sha3", "audio", "source-load"]
    assert (defaults.iterations, defaults.trials, defaults.warmup, defaults.timeout) == (64, 3, 1, 30)
    explicit = benchmark.build_parser().parse_args([
        "--workload", *benchmark.EXTENDED_WORKLOAD_LIMITS,
    ])
    assert explicit.workload == list(benchmark.EXTENDED_WORKLOAD_LIMITS)


@pytest.mark.parametrize("workload", benchmark.EXTENDED_WORKLOAD_LIMITS)
@pytest.mark.parametrize("invalid", [0, -1, True, 1.0, None, "above-limit"])
def test_case_specific_iteration_bounds_precede_runtime_construction(monkeypatch, workload, invalid):
    import simulator.runtime as runtime_module

    def unexpected_construction(**kwargs):
        pytest.fail("invalid work bound constructed a runtime")

    monkeypatch.setattr(runtime_module, "MegaForthRuntime", unexpected_construction)
    if invalid == "above-limit":
        invalid = benchmark.EXTENDED_WORKLOAD_LIMITS[workload] + 1
    with pytest.raises(ValueError, match="iterations must be between"):
        benchmark.prepare_semantic(workload, "simulator-python", invalid)


def test_aes_known_answer_is_complete_and_matches_existing_fixture_source():
    path = Path(__file__).parent / "simulator" / "test_kdos_aes.py"
    literals = {}
    for statement in ast.parse(path.read_text()).body:
        if isinstance(statement, ast.Assign) and isinstance(statement.targets[0], ast.Name):
            name = statement.targets[0].id
            if name in ("TWO_BLOCK_CIPHERTEXT", "TWO_BLOCK_TAG"):
                literals[name] = bytes.fromhex(ast.literal_eval(statement.value.args[0]))
    assert len(benchmark.AES_CIPHERTEXT) == 32
    assert len(benchmark.AES_TAG) == 16
    assert benchmark.AES_CIPHERTEXT == literals["TWO_BLOCK_CIPHERTEXT"]
    assert benchmark.AES_TAG == literals["TWO_BLOCK_TAG"]


@pytest.mark.parametrize("address, message", [
    (benchmark.CRYPTO_DESTINATION + 31, "AES ciphertext differs"),
    (benchmark.AES_TAG_ADDRESS + 15, "AES authentication tag differs"),
    (benchmark.AES_KEY_ADDRESS + 31, "AES source bytes changed"),
])
def test_aes_observer_rejects_corrupted_last_bytes(monkeypatch, address, message):
    fixture, runtime = _capture_fixture(monkeypatch, "aes-gcm32")
    try:
        result = fixture.action()
        runtime.memory.write8(address, runtime.memory.read8(address) ^ 1)
        with pytest.raises(benchmark.BenchmarkError, match=message):
            fixture.observe(result)
    finally:
        fixture.close()


@pytest.mark.parametrize("executor", ["simulator-python", "simulator-native"])
def test_page_layouts_do_equal_work_over_distinct_real_pages(executor):
    if executor == "simulator-native":
        pytest.importorskip("_megaforth_native")
    observations = []
    for workload, pages, stride in (("page-hot", 1, 8),
                                    ("page-scattered", 16, 4096),
                                    ("page-crossing", 17, 4096)):
        fixture = benchmark.prepare_semantic(workload, executor, 17)
        try:
            observation = fixture.observe(fixture.action())
            assert observation["stack"] == [2329]
            assert observation["cell_reads"] == 17
            description = fixture.description
            assert description["configured_data_pages"] == pages
            assert description["touched_data_pages"] == pages
            assert description["stride_bytes"] == stride
            assert description["buffer_start"] > benchmark.DESTINATION_ADDRESS + 4096
            assert description["buffer_start"] + description["buffer_bytes"] < 0x80000
            assert description["base_address"] % 4096 == (4092 if pages == 17 else 0)
            observations.append(observation)
        finally:
            fixture.close()
    assert len({row["semantic_steps"] for row in observations}) == 1


def test_page_observer_checks_guard_bytes_outside_read_cells(monkeypatch):
    fixture, runtime = _capture_fixture(monkeypatch, "page-crossing")
    try:
        result = fixture.action()
        runtime.memory.write8(fixture.description["buffer_start"], 0)
        with pytest.raises(benchmark.BenchmarkError, match="page source or guard bytes changed"):
            fixture.observe(result)
    finally:
        fixture.close()


def test_ntt_direct_oracle_has_known_device_order_and_dc_sum():
    # Fixed expected prefix from the independent NTT fixture, not the shared
    # radix-2 implementation or the service under measurement.
    source = (1, 2, 3) + (0,) * 253
    result = benchmark._ntt_direct_expected(source)
    assert result[:8] == (6, 1881, 3161, 837, 1602, 693, 2647, 835)
    assert len(result) == 256
    assert result[0] == sum(source) % 3329


def test_ntt_compute_and_transfer_report_same_value_and_distinct_transfer_work():
    guests = []
    for workload in ("ntt-compute", "ntt-transfer"):
        fixture = benchmark.prepare_semantic(workload, "simulator-python", 2)
        try:
            guests.append(fixture.observe(fixture.action()))
        finally:
            fixture.close()
    assert guests[0]["output_sha256"] == guests[1]["output_sha256"]
    assert [guest["guest_transfer_bytes"] for guest in guests] == [0, 4096]
    assert [guest["transforms"] for guest in guests] == [2, 2]


@pytest.mark.parametrize("corruption, message", [
    ("result", "NTT result differs from direct DFT"),
    ("destination", "NTT output or guard bytes differ"),
    ("source", "NTT source bytes changed"),
])
def test_ntt_observer_rejects_value_transfer_and_source_corruption(monkeypatch, corruption, message):
    fixture, runtime = _capture_fixture(monkeypatch, "ntt-transfer", iterations=1)
    try:
        result = fixture.action()
        if corruption == "result":
            runtime.ntt._result[-1] ^= 1
        else:
            address = (benchmark.CRYPTO_DESTINATION + 1024 if corruption == "destination"
                       else benchmark.CRYPTO_SOURCE + 1023)
            runtime.memory.write8(address, runtime.memory.read8(address) ^ 1)
        with pytest.raises(benchmark.BenchmarkError, match=message):
            fixture.observe(result)
    finally:
        fixture.close()


@pytest.mark.parametrize("executor", ["simulator-python", "simulator-native"])
def test_short_and_long_quantum_preserve_exact_final_work_and_return_state(executor):
    if executor == "simulator-native":
        pytest.importorskip("_megaforth_native")
    guests = []
    for workload in ("continuation-short", "continuation-long"):
        fixture = benchmark.prepare_semantic(workload, executor, 17)
        try:
            guests.append(fixture.observe(fixture.action()))
        finally:
            fixture.close()
    assert guests[0]["host_resumes"] > guests[1]["host_resumes"] == 0
    assert guests[0]["increments"] == guests[1]["increments"] == 17
    # Quantum choice may change native entries, but not completed guest work,
    # continuation cookies, slot metadata, or the retained return-stack bytes.
    assert {key: value for key, value in guests[0].items() if key != "host_resumes"} == {
        key: value for key, value in guests[1].items() if key != "host_resumes"
    }


@pytest.mark.parametrize("corruption", ["cookie", "retained-bytes", "steps"])
def test_continuation_observer_rejects_inexact_final_state(monkeypatch, corruption):
    fixture, runtime = _capture_fixture(monkeypatch, "continuation-short")
    try:
        result, resumes = fixture.action()
        if corruption == "cookie":
            runtime.main_context.returns._continuation_cookie += 1
        elif corruption == "retained-bytes":
            address = runtime.main_context.returns.empty_pointer - 8
            runtime.memory.write8(address, runtime.memory.read8(address) ^ 1)
        else:
            result = ExecutionResult(result.semantic_steps + 1)
        with pytest.raises(benchmark.BenchmarkError, match="continuation .+ differs"):
            fixture.observe((result, resumes))
    finally:
        fixture.close()


@pytest.mark.parametrize("mode, message", [
    ("stalled", "no progress"), ("too-many-resumes", "resume bound exceeded"),
    ("blocked", "unexpected block"),
])
def test_continuation_host_loop_is_bounded_even_if_runtime_contract_breaks(mode, message):
    steps = 1

    def resume(suspension):
        nonlocal steps
        if mode != "stalled":
            steps += 1
        return YieldedExecution(steps, suspension)

    initial = (BlockedExecution(1, object()) if mode == "blocked"
               else YieldedExecution(1, object()))
    fake = SimpleNamespace(run_until_blocked=lambda *args, **kwargs: initial,
                           resume_yielded=resume)
    with pytest.raises(benchmark.BenchmarkError, match=message):
        benchmark._run_continuations(fake, quantum=7, budget=21)


def test_watchdog_is_retained_for_extended_worker(monkeypatch):
    import subprocess

    monkeypatch.setattr(benchmark, "_repository", lambda: {"commit": "fixture"})
    monkeypatch.setattr(benchmark, "_source_manifest", lambda: {})
    calls = []

    def timeout(command, **kwargs):
        calls.append((command, kwargs))
        raise subprocess.TimeoutExpired(command, kwargs["timeout"])

    monkeypatch.setattr(benchmark.subprocess, "run", timeout)
    args = benchmark.build_parser().parse_args([
        "--workload", "ntt-transfer", "--executor", "simulator-python", "--timeout", "2",
    ])
    with pytest.raises(subprocess.TimeoutExpired):
        benchmark.run_suite(args)
    assert len(calls) == 1
    command, options = calls[0]
    assert "ntt-transfer" in command and "--worker" in command
    assert options["timeout"] == 2
