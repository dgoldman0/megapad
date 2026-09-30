"""The timing benchmark qualifies outcomes before publishing measurements."""

from __future__ import annotations

from copy import deepcopy

import pytest

import bench_execution_timing as benchmark


@pytest.mark.parametrize("model", benchmark.MODELS)
@pytest.mark.parametrize("workload", ("compute", "memory"))
def test_small_fixed_total_cases_check_outputs_and_separate_model_accounting(workload, model):
    report = benchmark.run_case(workload, model, 2, 1, iterations=8, trials=1, warmup=0)
    assert report["description"]["per_core_iterations"] == (4, 4)
    assert report["guest"]["register_results"] == [(4, 4, 10, 0)] * 2
    assert report["guest"]["data_cells"] == ([4, 4] if workload == "memory" else [0, 0])
    sample, = report["samples"]
    assert sample["wall_seconds"] > 0 and sample["process_seconds"] > 0
    assert sample["execution"]["timing_model"] == model
    assert sample["execution"]["models_shared_clock_latency"] is (model == "strict_shared_clock")
    assert "wake_observations" not in sample["execution"]
    if model == "strict_shared_clock":
        replay = report["strict_replay"]
        assert replay["calls"] > sample["execution"]["calls"]
        assert benchmark._observable(replay) == benchmark._observable(sample["execution"])
    else:
        assert report["strict_replay"] is None


@pytest.mark.parametrize("model", benchmark.MODELS)
def test_wake_compares_same_completed_work_without_claiming_fast_clock_latency(model):
    report = benchmark.run_case("wake", model, 2, 1, iterations=256, trials=1, warmup=0)
    assert report["guest"]["register_results"] == [(256, 0, 0, 0), (1, 0, 0, 0)]
    assert report["guest"]["data_cells"] == [0, 1]
    assert report["guest"]["pending_ipi_mask"] == 1
    sample = report["samples"][0]
    assert "wake_observations" not in sample["execution"]
    if model == "instruction_batched":
        assert sample["execution"]["native_rounds"] >= 2
        assert report["strict_replay"] is None
        assert "no shared-clock latency" in report["latency_interpretation"]
    else:
        wake = report["strict_replay"]["wake_observations"]
        assert wake["maximum_observation_interval_cycles"] == 1
        assert set(wake["milestone_cycle_intervals"]) == {
            "ipi_asserted", "receiver_runnable", "receiver_marker_committed",
        }
        for first, last in wake["milestone_cycle_intervals"].values():
            assert 0 <= last - first <= 1
        for key in ("ipi_to_runnable_cycles", "ipi_to_marker_cycles"):
            interval = wake[key]
            assert 0 <= interval["minimum"] <= interval["maximum"]


def test_worker_width_preserves_strict_guest_state_clock_and_wake_observations():
    cases = [benchmark.run_case("wake", "strict_shared_clock", 2, workers,
                                iterations=8, trials=1, warmup=0)
             for workers in (1, 2, 4)]
    comparison, = benchmark.compare_workers(cases)
    assert comparison["workers"] == [1, 2, 4]
    assert comparison["one_worker_reference_available"]
    assert comparison["guest_and_clock_equivalent"]

    # Host timing does not enter guest equivalence. Corrupted architectural
    # evidence must fail the comparison instead of producing a speedup claim.
    changed = deepcopy(cases)
    changed[1]["median_wall_seconds"] *= 3
    assert benchmark.compare_workers(changed)[0]["guest_and_clock_equivalent"]
    changed[1]["samples"][0]["execution"]["system_cycles"] += 1
    with pytest.raises(benchmark.BenchmarkError, match="worker count changed"):
        benchmark.compare_workers(changed)
    assert not benchmark.compare_workers(cases[1:])[0]["guest_and_clock_equivalent"]


def test_cold_and_explicitly_warm_cache_fixtures_retain_identical_results():
    cases = [benchmark.run_case("memory", "strict_shared_clock", 2, 1,
                                iterations=8, trials=1, warmup=0, cache=cache)
             for cache in ("cold", "warm")]
    cold, warm = cases
    assert cold["guest"] == warm["guest"]
    assert cold["description"]["program_sha256"] == warm["description"]["program_sha256"]
    assert cold["samples"][0]["execution"]["system_cycles"] >= warm["samples"][0]["execution"]["system_cycles"]


def test_execution_budgets_and_result_checks_reject_incomplete_or_wrong_runs():
    with benchmark.prepare("compute", 2, 1, 8) as fixture:
        fixture.instruction_budget = 1
        with pytest.raises(benchmark.BenchmarkError, match="allowance"):
            benchmark.execute(fixture, "strict_shared_clock")
    with benchmark.prepare("memory", 2, 1, 8) as fixture:
        benchmark.execute(fixture, "instruction_batched")
        fixture.system.cores[0].regs[6] ^= 1
        with pytest.raises(benchmark.BenchmarkError, match="result mismatch"):
            fixture.guest()


def test_observers_cannot_turn_fast_execution_into_a_latency_report():
    with benchmark.prepare("wake", 2, 1, 8) as fixture:
        with pytest.raises(ValueError, match="cycle slices require"):
            benchmark.execute(fixture, "instruction_batched", cycle_slice=1)
        with pytest.raises(ValueError, match="one-cycle strict wake replay"):
            benchmark.execute(fixture, "instruction_batched", observe_wake=True)
        with pytest.raises(ValueError, match="one-cycle strict wake replay"):
            benchmark.execute(fixture, "strict_shared_clock", observe_wake=True)


@pytest.mark.parametrize(("option", "value"), [
    ("--iterations", "3"), ("--iterations", "4097"),
    ("--trials", "0"), ("--trials", "6"),
    ("--warmup", "3"), ("--timeout", "121"),
])
def test_cli_rejects_unbounded_or_empty_measurements(option, value):
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, value])


def test_single_core_peer_wake_is_explicitly_unsupported():
    assert not benchmark.supported("wake", 1)
    with pytest.raises(ValueError, match="unsupported"):
        with benchmark.prepare("wake", 1, 1, 8):
            pytest.fail("unsupported fixture was admitted")


@pytest.mark.parametrize("model", benchmark.MODELS)
def test_phase0_serialization_retains_timing_identity(model):
    import bench_phase0_concurrency as phase0

    with benchmark.prepare("compute", 1, 1, 4) as fixture:
        stats = (fixture.system.run_batch_stats(0) if model == "instruction_batched"
                 else fixture.system.run_cycle_batch(0))
        projected = phase0._system_run_stats_state(stats)
        assert projected["timing_model"] == model
        assert projected["models_shared_clock_latency"] is (model == "strict_shared_clock")
        assert projected["instructions_executed"] == projected["system_cycles_advanced"] == 0
