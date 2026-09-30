"""Bounded hotspot fixtures verify real results before reporting a timing."""

from __future__ import annotations

import os

import pytest

import bench_runtime_hotspots as benchmark


@pytest.mark.parametrize("executor", ["simulator-python", "simulator-native"])
@pytest.mark.parametrize("workload", benchmark.WORKLOADS)
def test_semantic_kernel_results_and_attribution_agree(workload, executor):
    if executor == "simulator-native":
        pytest.importorskip("_megaforth_native")
    result = benchmark.run_case(workload, executor, iterations=2, trials=1,
                                warmup=0, attribution=True)
    sample = result["samples"][0]
    attributed = result["attribution_run"]
    assert sample["guest"] == attributed["guest"]
    if workload == "source-load":
        # Parsing and compilation need not dispatch any semantic instructions.
        assert sample["guest"]["definitions"] == 3
        assert sample["guest"]["source_bytes"] == sample["description"]["source_bytes"] > 0
        assert sample["guest"]["stack"] == [1]
    else:
        assert sample["guest"]["semantic_steps"] > 0
    assert sample["wall_seconds"] > 0
    assert "attribution" not in sample
    assert "profile" not in sample["native_counter_delta"]
    assert attributed["attribution"]["top_functions_by_cumulative_time"]
    if executor == "simulator-native":
        assert "profile" in attributed["native_counter_delta"]


def test_architectural_fp_kernel_has_the_same_known_checksum_and_flags():
    pytest.importorskip("_mp64_accel")
    result = benchmark.run_case("fp64", "emulator-native", iterations=3,
                                trials=1, warmup=0, attribution=True)
    guest = result["samples"][0]["guest"]
    assert guest["fp_operations"] == 12
    assert guest["fpcsr"] == 16
    assert guest["instructions"] > 12
    assert guest["system_cycles"] > 0
    semantic = benchmark.run_case("fp64", "simulator-python", iterations=3,
                                  trials=1, warmup=0, attribution=False)
    assert guest["checksum"] == semantic["samples"][0]["guest"]["checksum"]
    assert guest == result["attribution_run"]["guest"]
    # Fallback coverage is evidence, not a pinned requirement: a later native
    # implementation must be able to reduce this to zero without changing tests.
    calls = result["attribution_run"]["attribution"]["python_fallback_and_sync_calls"]
    assert calls["_step_python_fallback_in_memory_scope"] >= 0


def test_kernel_validation_rejects_wrong_observations():
    fixture = benchmark.prepare_semantic("fp64", "simulator-python", 2)
    try:
        with pytest.raises(benchmark.BenchmarkError, match=r"result stack differs: actual=\(\), expected=\("):
            fixture.observe(type("Result", (), {"semantic_steps": 0})())
    finally:
        fixture.close()


def test_external_native_profiling_does_not_contaminate_timing_samples(monkeypatch):
    monkeypatch.setenv("MEGAFORTH_NATIVE_PROFILE", "1")
    result = benchmark.run_case("fp64", "simulator-python", iterations=1,
                                trials=1, warmup=0, attribution=False)
    assert "attribution" not in result["samples"][0]
    assert os.environ["MEGAFORTH_NATIVE_PROFILE"] == "1"


@pytest.mark.parametrize(("option", "value"), [
    ("--iterations", "0"), ("--iterations", "100001"),
    ("--trials", "11"), ("--warmup", "4"), ("--timeout", "121"),
])
def test_unbounded_or_empty_runs_are_rejected(option, value):
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, value])


def test_unsupported_architectural_service_case_is_explicit():
    with pytest.raises(ValueError, match="not provided"):
        benchmark.prepare("audio", "emulator-native", 1)
