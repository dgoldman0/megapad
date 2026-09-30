"""The V5 crossing benchmark keeps real work, owners and timing scopes explicit."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys

import pytest

import bench_hybrid_services as benchmark


def _native(executor="python"):
    native = pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    return native


def test_known_binary64_answers_are_the_existing_hotspot_oracle():
    from bench_runtime_hotspots import FP_RESULTS, FP_XOR

    expected = (0x4010000000000000, 0x4024000000000000,
                0x3FD5555555555555, 0x3FF6A09E667F3BCD)
    assert benchmark.FP_RESULTS == FP_RESULTS == expected
    for count in (64, 256):
        oracle = benchmark.fixture_values(count)
        assert oracle == dict(result_bits=list(expected), checksum=(FP_XOR * count) & benchmark.MASK64,
                             fpcsr=16, fp_operations=4 * count)
        assert benchmark.expected_counts("service_callbacks", count)["callback_requests"] <= 1024
    assert sum(len(arguments) for _name, arguments in benchmark.OPERATIONS) == 8
    assert tuple(name for name, _arguments in benchmark.OPERATIONS) == ("F64+", "F64FMA", "F64/", "F64SQRT")


@pytest.mark.parametrize("path", benchmark.PATHS[1:])
@pytest.mark.parametrize("iterations", (1, 3))
def test_machine_fixture_runs_against_ordinary_mp64_with_real_call_ret_cycles(path, iterations):
    """Independent architectural stepping checks the benchmark's loop/counts.

    Only the external value service is replaced with known answers at its
    actual stub boundary. The ordinary CPU executes every CALL, RET, LDI,
    store, XOR, checksum and branch, without any hybrid runner state.
    """
    native = _native()
    from simulator.memory import MMIO_BASE, MMIO_LIMIT

    image, _source = benchmark.machine_image(path, iterations)
    code_base, external_base = 0x100, 0x100000
    output = external_base + 256
    stack_top = external_base + 640
    ram, external = bytearray(4096), bytearray(1024)
    ram[code_base:code_base + len(image.code)] = image.code
    external[stack_top - 8 - external_base:stack_top - external_base] = benchmark.MASK64.to_bytes(8, "little")
    initial = benchmark.GUARD + bytes(32) + benchmark.GUARD
    offset = output - external_base - len(benchmark.GUARD)
    external[offset:offset + len(initial)] = initial
    state = native.CPUState()
    state.attach_mem(ram, len(ram))
    state.attach_ext_mem(external, external_base, len(external))
    for index in range(32):
        state.set_reg(index, 0)
    state.psel, state.xsel, state.spsel, state.sw = 3, 2, 15, 1
    state.ext_modifier = -1  # Fresh CPUState otherwise retains EXT.IMM64's zero.
    state.flags_unpack(0)
    state.set_reg(3, code_base)
    state.set_reg(15, stack_top - 8)
    for index, value in enumerate((output, 32, iterations)):
        state.set_reg(4 + index, value)
    callbacks = {code_base + site.stub_offset: index for index, site in enumerate(getattr(image, "callbacks", ()))}
    instructions = cycles = requests = 0
    expected = benchmark.expected_counts(path, iterations)

    def unexpected(*_args):
        pytest.fail("bounded ordinary fixture reached a device")

    while state.get_reg(3) != benchmark.MASK64:
        assert instructions < expected["machine_instructions"], "fixture did not reach the root RET within its bound"
        cycles += native.step_one(state, mmio_read8=unexpected, mmio_write8=unexpected,
                                  on_output=unexpected, csr_read_override=None,
                                  mmio_start=MMIO_BASE, mmio_end=MMIO_LIMIT)
        instructions += 1
        pc = state.get_reg(3)
        if pc in callbacks:
            index = callbacks[pc]
            assert index == requests % 4
            arguments = (benchmark.OPERATIONS[index][1] if path == "service_callbacks" else
                         (benchmark.FP_RESULTS[index], 0))
            assert tuple(state.get_reg(4 + index) for index in range(len(arguments))) == arguments
            state.set_reg(4, benchmark.FP_RESULTS[index])
            requests += 1
    assert (instructions, cycles, requests) == (expected["machine_instructions"], expected["machine_cycles"], expected["callback_requests"])
    assert state.get_reg(4) == benchmark.fixture_values(iterations)["checksum"]
    assert state.get_reg(15) == stack_top
    known = b"".join(value.to_bytes(8, "little") for value in benchmark.FP_RESULTS)
    assert bytes(external[offset:offset + len(initial)]) == benchmark.GUARD + known + benchmark.GUARD


def test_v5_and_integer_control_metadata_have_exact_distinct_effects_and_bounds():
    from shared.hybrid_abi import RoutineImageV1, RoutineImageV2
    from shared.hybrid_services import RoutineImageV5, ServiceExportV5

    service, service_source = benchmark.machine_image("service_callbacks", 256)
    integer, integer_source = benchmark.machine_image("integer_callbacks", 256)
    machine, machine_source = benchmark.machine_image("machine_control", 256)
    assert type(service) is RoutineImageV5 and type(integer) is RoutineImageV2 and type(machine) is RoutineImageV1
    assert service.max_callback_requests == 1024
    assert service.input_cells == integer.input_cells == machine.input_cells == 3
    assert service.output_cells == integer.output_cells == machine.output_cells == 1
    assert service.buffers == integer.buffers == machine.buffers
    assert service.buffers[0].max_bytes == 32
    assert all(type(site.export) is ServiceExportV5 and site.export.effect == "scalar_fp_state"
               and site.export.max_semantic_steps == 1 for site in service.callbacks)
    assert tuple(site.export.name for site in service.callbacks) == tuple(name for name, _args in benchmark.OPERATIONS)
    assert all(site.export.name == "XOR" and site.export.effect == "integer_leaf" for site in integer.callbacks)
    assert len({site.stub_offset for site in service.callbacks}) == 4
    assert service_source.count(b"call.l") == integer_source.count(b"call.l") == 4
    assert b"call.l" not in machine_source
    assert b"OFFSET_" not in service_source + integer_source
    assert all(image.max_instructions < 1_000_000 for image in (service, integer, machine))


@pytest.mark.parametrize("executor", benchmark.EXECUTORS)
@pytest.mark.parametrize("iterations", (1, 3))
def test_real_same_owner_group_checks_exact_fp_and_discloses_different_control_work(executor, iterations):
    _native(executor)
    result = benchmark.run_case(executor, iterations=iterations, trials=1, warmup=0)
    validation = result["validation"]
    group = result["groups"][0]
    description = group["description"]
    assert description["semantic_executor"] == executor
    assert description["scalar_selection"]["scalar_value_executor"] == executor
    assert description["metadata_versions"]["service_callbacks"] == 5
    assert description["native_transport_versions"]["service_callbacks"] == 2
    assert description["callback_dispatcher"] == "python_reference"
    assert description["service_callback_value_executor"] == (
        "python_reference" if executor == "python" else "shared_native_kernel")
    assert description["suspension_supported"] is False
    assert group["order"] == ["semantic_fp", "service_callbacks", "machine_control", "integer_callbacks", "semantic_fp"]
    assert group["baseline_recheck"]["guest"] == group["paths"]["semantic_fp"]["guest"]
    assert group["baseline_recheck"]["wall_seconds"] > 0
    assert group["paths"]["service_callbacks"]["observed_owner_max_machine_depth"] == 1
    assert group["baseline_recheck"]["observed_owner_max_machine_depth"] == 1
    for path in benchmark.PATHS:
        row = group["paths"][path]
        checked = validation["paths"][path]
        assert row["guest"] == checked["guest"]
        assert row["wall_seconds"] > 0 and row["process_cpu_seconds"] >= 0
        assert checked["wall_seconds"] is checked["process_cpu_seconds"] is None
        guest = row["guest"]
        assert guest["result_bits"] == list(benchmark.FP_RESULTS)
        assert guest["checksum"] == benchmark.fixture_values(iterations)["checksum"]
        assert guest["exit_reason"] == "returned" and guest["return_depth"] == 0
        assert {key: guest[key] for key in benchmark.expected_counts(path, iterations)} == benchmark.expected_counts(path, iterations)
        assert guest["max_parked_depth"] == int(path in ("service_callbacks", "integer_callbacks"))
        assert guest["fpcsr"] == (16 if path in ("semantic_fp", "service_callbacks") else 0)
        assert "attribution" not in row
    assert group["paths"]["machine_control"]["guest"]["precomputed_values"] == 4 * iterations
    assert group["paths"]["integer_callbacks"]["guest"]["integer_leaf_operations"] == 4 * iterations
    assert group["paths"]["integer_callbacks"]["guest"]["fp_operations"] == 0
    assert {row["module"] for row in result["native_artifacts"]} == (
        {"_mp64_accel", "_megaforth_native"} if executor == "native" else {"_mp64_accel"})


def test_fresh_groups_share_one_scalar_owner_within_group_and_restore_profile_environment(monkeypatch):
    _native()
    fixtures, resets = [], []
    original_prepare = benchmark.prepare
    original_reset = benchmark.Prepared.reset

    def prepare(*args):
        assert os.environ["MEGAFORTH_NATIVE_PROFILE"] == "0"
        fixture = original_prepare(*args)
        fixtures.append(fixture)
        return fixture

    def reset(fixture, path):
        resets.append((id(fixture), id(fixture.hybrid.semantic.scalar_float), path))
        return original_reset(fixture, path)

    monkeypatch.setenv("MEGAFORTH_NATIVE_PROFILE", "1")
    monkeypatch.setattr(benchmark, "prepare", prepare)
    monkeypatch.setattr(benchmark.Prepared, "reset", reset)
    result = benchmark.run_case("python", iterations=1, trials=2, warmup=1)
    assert os.environ["MEGAFORTH_NATIVE_PROFILE"] == "1"
    assert len(fixtures) == 4  # Separate validation, warmup, two measured groups.
    assert len({id(fixture.memory) for fixture in fixtures}) == 4
    assert len({id(fixture.scalar_owner) for fixture in fixtures}) == 4
    for fixture in fixtures:
        observed = [(owner, path) for identity, owner, path in resets if identity == id(fixture)]
        assert len(observed) == 5
        assert {owner for owner, _path in observed} == {id(fixture.scalar_owner)}
        assert observed[0][1] == observed[-1][1] == "semantic_fp"
    assert result["groups"][1]["order"] == ["semantic_fp", "integer_callbacks", "machine_control", "service_callbacks", "semantic_fp"]


def test_attribution_has_separate_owner_and_no_latency_samples():
    _native()
    result = benchmark.run_case("python", iterations=1, trials=1, warmup=0, attribution=True)
    attributed = result["attribution_group"]
    for path in benchmark.PATHS:
        sample, profile = result["groups"][0]["paths"][path], attributed["paths"][path]
        assert sample["guest"] == profile["guest"]
        assert sample["wall_seconds"] > 0 and "attribution" not in sample
        assert profile["wall_seconds"] is profile["process_cpu_seconds"] is None
        assert profile["attribution"]["top_functions_by_cumulative_time"]
    with pytest.raises(benchmark.BenchmarkError, match="attribution"):
        benchmark.perform_group("python", 1, timed=True, attribution=True)


@pytest.mark.parametrize("corruption", ("guard", "result", "fpcsr"))
def test_independent_observer_rejects_real_post_execution_corruption(corruption):
    _native()
    fixture = benchmark.prepare("python", 1)
    try:
        before = fixture.reset("service_callbacks")
        result = fixture.action("service_callbacks")
        if corruption == "fpcsr":
            fixture.scalar_owner.write_fpcsr(0)
            message = "FPCSR"
        else:
            address = fixture.address - 1 if corruption == "guard" else fixture.address
            fixture.memory.write8(address, fixture.memory.read8(address) ^ 1)
            message = "result bits or guard"
        with pytest.raises(benchmark.BenchmarkError, match=message):
            fixture.observe("service_callbacks", result, before)
    finally:
        fixture.close()


@pytest.mark.parametrize("executor", benchmark.EXECUTORS)
def test_scalar_selection_checks_actual_original_kernel_without_mutating_binding(executor):
    _native(executor)
    from simulator.runtime import MegaForthRuntime

    runtime = MegaForthRuntime(execution_backend=executor)
    owner, original = runtime.scalar_float, runtime.scalar_float._native_execute
    try:
        assert benchmark.scalar_selection(runtime)["scalar_value_executor"] == executor
        assert runtime.scalar_float is owner and owner._native_execute is original
        owner._native_execute = lambda *_args: (0, 0, 0)
        with pytest.raises(benchmark.BenchmarkError, match="scalar|kernel"):
            benchmark.scalar_selection(runtime)
    finally:
        owner._native_execute = original
        runtime.memory.mmio.audio.release_host_sink()


@pytest.mark.parametrize("option,value", (
    ("--iterations", "0"), ("--iterations", "257"), ("--trials", "0"),
    ("--trials", "11"), ("--warmup", "4"), ("--timeout", "0"), ("--timeout", "121"),
))
def test_cli_rejects_empty_or_unbounded_cases(option, value):
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, value])


@pytest.mark.parametrize("value", (True, False, 0, 257, 1.5, "64"))
def test_programmatic_fixture_requires_exact_bounded_iterations(value):
    with pytest.raises(ValueError):
        benchmark.fixture_values(value)


def test_parent_watchdog_bounds_full_worker_and_disables_native_profiling(monkeypatch):
    args = benchmark.build_parser().parse_args(["--executor", "python", "--iterations", "64", "--timeout", "1"])
    monkeypatch.setattr(benchmark, "_repository", lambda: {"commit": "test"})
    monkeypatch.setattr(benchmark, "_sources", lambda: {"harness": "hash"})
    calls = []

    def timeout(command, **kwargs):
        calls.append((command, kwargs))
        raise subprocess.TimeoutExpired(command, kwargs["timeout"])

    monkeypatch.setattr(benchmark.subprocess, "run", timeout)
    with pytest.raises(benchmark.BenchmarkError, match="wall watchdog"):
        benchmark.run_suite(args)
    command, options = calls[0]
    assert len(calls) == 1 and "--worker" in command
    assert options["timeout"] == 1 and options["env"]["MEGAFORTH_NATIVE_PROFILE"] == "0"


def test_benchmark_import_is_backend_neutral_and_cannot_activate_service_capability():
    source = """
import importlib.abc
import sys
class Block(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(('simulator', 'emulator', 'hybrid', '_mp64_accel', '_megaforth_native')):
            raise AssertionError('unexpected backend import: ' + fullname)
sys.meta_path.insert(0, Block())
import bench_hybrid_services
assert bench_hybrid_services.fixture_values(64)['fp_operations'] == 256
assert bench_hybrid_services.build_parser().parse_args([]).iterations == [64, 256]
"""
    result = subprocess.run([sys.executable, "-c", source], cwd=Path(benchmark.__file__).parent,
                            capture_output=True, text=True, timeout=15, check=False)
    assert result.returncode == 0, result.stderr


def test_defaults_and_source_hashes_include_harness_dependencies():
    args = benchmark.build_parser().parse_args([])
    assert args.executor == ["python", "native"] and args.iterations == [64, 256]
    assert (args.trials, args.warmup, args.timeout, args.attribution) == (3, 1, 60, False)
    sources = benchmark._sources()
    for name in ("bench_hybrid_services.py", "bench_hybrid_closed.py", "bench_runtime_hotspots.py",
                 "simulator/scalar_float.py", "shared/scalar_fp.py", "hybrid/runtime.py"):
        assert sources[name] == benchmark._file_hash(benchmark.ROOT / name)
