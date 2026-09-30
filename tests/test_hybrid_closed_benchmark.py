"""The closed callback benchmark publishes only bounded, validated samples."""

from __future__ import annotations

import os
import subprocess

import pytest

import bench_hybrid_closed as benchmark


def _native(executor="python"):
    pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")


def test_integer_oracle_covers_signed_endpoints_and_exact_clamp_edges():
    inputs, outputs = benchmark.fixture_values(13)
    expected = (-17, -17, -17, -17, -16, -1, 0, 1, 23, 23, 23, 23, 23)
    assert inputs[0] == 1 << 63
    assert inputs[-1] == (1 << 63) - 1
    assert outputs == tuple(value & benchmark.MASK64 for value in expected)
    assert sum(outputs) & benchmark.MASK64 == 31


@pytest.mark.parametrize("executor", benchmark.EXECUTORS)
@pytest.mark.parametrize("iterations", (1, 3))
def test_fresh_control_and_hybrid_samples_have_exact_work_and_same_outputs(executor, iterations):
    _native(executor)
    result = benchmark.run_case(executor, iterations=iterations, trials=1, warmup=0)
    checked = result["validation"]
    for path in benchmark.PATHS:
        sample = result["paths"][path]["samples"][0]
        assert sample["guest"] == checked[path]["guest"]
        assert checked[path]["wall_seconds"] is None
        assert sample["wall_seconds"] > 0
        assert sample["process_cpu_seconds"] >= 0
        assert sample["preparation_seconds"] >= 0 and sample["validation_seconds"] >= 0
        assert sample["description"]["semantic_executor"] == executor
        assert sample["guest"]["return_depth"] == 0
    control = checked["semantic_control"]["guest"]
    hybrid = checked["hybrid_callbacks"]["guest"]
    assert control["semantic_steps"] == 30 * iterations + 5
    assert hybrid["semantic_steps"] == 7 * iterations + 1
    assert (hybrid["machine_instructions"], hybrid["machine_cycles"], hybrid["callback_requests"],
            hybrid["callback_semantic_steps"], hybrid["machine_segments"], hybrid["transitions"]) == (
                10 * iterations + 10, 13 * iterations + 10, iterations, 7 * iterations,
                iterations + 1, 1,
            )
    for key in ("final_cells", "buffer_sha256", "shared_span_sha256"):
        assert control[key] == hybrid[key]
    description = checked["hybrid_callbacks"]["description"]
    assert description["callback_executor"] == "python_reference"
    assert description["metadata_version"] == 3 and description["native_transport_version"] == 2
    assert description["selected_bounds"]["callback_requests"] == iterations
    assert len(description["sealed_machine_code_sha256"]) == 64
    assert {artifact["module"] for artifact in result["native_artifacts"]} == (
        {"_mp64_accel", "_megaforth_native"} if executor == "native" else {"_mp64_accel"}
    )


def test_validation_warmups_and_trials_have_distinct_memory_and_restore_profile_environment(monkeypatch):
    _native()
    retained = []
    prepare = benchmark.prepare

    def capture(*args):
        assert os.environ["MEGAFORTH_NATIVE_PROFILE"] == "0"
        prepared = prepare(*args)
        retained.append(prepared)
        return prepared

    monkeypatch.setenv("MEGAFORTH_NATIVE_PROFILE", "1")
    monkeypatch.setattr(benchmark, "prepare", capture)
    result = benchmark.run_case("python", iterations=2, trials=2, warmup=1)
    assert os.environ["MEGAFORTH_NATIVE_PROFILE"] == "1"
    assert len(retained) == 8  # Two validation, two warmup, four measured owners.
    assert len({id(fixture.memory) for fixture in retained}) == 8
    assert len({id(fixture.runtime) for fixture in retained}) == 8
    assert result["warmup_pairs"] == 1
    assert all(len(row["samples"]) == 2 for row in result["paths"].values())


@pytest.mark.parametrize("path", benchmark.PATHS)
def test_independent_validation_rejects_a_corrupted_guard_after_execution(path):
    _native()
    fixture = benchmark.prepare(path, "python", 2)
    try:
        result = fixture.action()
        fixture.memory.write8(fixture.description["buffer_address"] - 1, 0)
        with pytest.raises(benchmark.BenchmarkError, match="guard bytes differ"):
            fixture.observe(result)
    finally:
        fixture.close()


@pytest.mark.parametrize("option,value", (
    ("--iterations", "0"), ("--iterations", "1025"), ("--trials", "0"),
    ("--trials", "11"), ("--warmup", "4"), ("--timeout", "0"), ("--timeout", "121"),
))
def test_cli_rejects_unbounded_or_empty_cases(option, value):
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, value])


@pytest.mark.parametrize("value", (True, 0, 1025, 1.5))
def test_programmatic_fixture_rejects_nonexact_or_out_of_bound_iterations(value):
    with pytest.raises(ValueError):
        benchmark.fixture_values(value)


def test_parent_watchdog_bounds_entire_worker_and_disables_attribution(monkeypatch):
    args = benchmark.build_parser().parse_args(["--executor", "python", "--timeout", "1"])
    monkeypatch.setattr(benchmark, "_repository", lambda: {"commit": "test"})
    monkeypatch.setattr(benchmark, "_sources", lambda: {"harness": "hash"})
    calls = []

    def timeout(command, **kwargs):
        calls.append((command, kwargs))
        raise subprocess.TimeoutExpired(command, kwargs["timeout"])

    monkeypatch.setattr(benchmark.subprocess, "run", timeout)
    with pytest.raises(benchmark.BenchmarkError, match="wall watchdog"):
        benchmark.run_suite(args)
    assert len(calls) == 1
    command, options = calls[0]
    assert "--worker" in command
    assert options["timeout"] == 1
    assert options["env"]["MEGAFORTH_NATIVE_PROFILE"] == "0"


def test_cli_defaults_are_modest_paired_trials():
    args = benchmark.build_parser().parse_args([])
    assert args.executor == ["python", "native"]
    assert (args.iterations, args.trials, args.warmup, args.timeout) == (128, 3, 1, 30)
