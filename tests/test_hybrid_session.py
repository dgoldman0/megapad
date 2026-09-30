"""Hybrid boot declarations use the existing semantic session authority."""

from __future__ import annotations

import json
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace

import pytest

import megapad
from asm import assemble
from hybrid.manifest import HybridManifestError
from hybrid.runtime import HybridExecutionError, HybridRuntime
from hybrid.server import build_argument_parser, prepare_server
from hybrid.session import HybridSession, HybridSharedMachine
from shared.hybrid_abi import HYBRID_ABI, RoutineImageV1
from shared_session import SessionServer
from simulator.image_bootstrap import ImageBootstrapError, prepare_image_bootstrap
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.session import SEMANTIC_QUANTUM_ENVIRONMENT
from simulator.storage import HostedStorageService
from tests.simulator.test_image_bootstrap import _boot_image


ROOT = Path(__file__).resolve().parents[1]


def _manifest(tmp_path, *, extra_routines=()):
    (tmp_path / "increment.bin").write_bytes(bytes(assemble("inc r4\nret.l")))
    path = tmp_path / "routines.json"
    path.write_text(json.dumps({
        "abi": HYBRID_ABI,
        "version": 1,
        "dispatch_instruction_limit": 1000,
        "routines": [{
            "name": "H-INC",
            "image": "increment.bin",
            "entry_offset": 0,
            "input_cells": 1,
            "output_cells": 1,
            "buffers": [],
            "max_instructions": 100,
            "return_stack_cells": 16,
        }, *extra_routines],
    }), encoding="utf-8")
    return path


def _server_args(tmp_path, *, autoexec_body=None, extra_routines=(), executor="python"):
    image = tmp_path / "hybrid.img"
    image.write_bytes(_boot_image(autoexec_body=autoexec_body))
    manifest = _manifest(tmp_path, extra_routines=extra_routines)
    return build_argument_parser().parse_args([
        "--storage", str(image), "--hybrid-routines", str(manifest),
        "--socket", str(tmp_path / "hybrid.sock"),
        "--ram-kib", "768", "--ext-mem-mib", "1", "--vram-mib", "0",
        "--executor", executor, "--semantic-step-budget", "10000",
        "--semantic-quantum-steps", "4096", "--cols", "96", "--rows", "32",
        "--paused",
    ])


def _runtime(executor, *, dispatch_instruction_limit=1000):
    pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    memory = create_one_core_address_space(
        bank0_size=65536, external_size=4096, dense_backing=True,
    )
    hybrid = HybridRuntime.create(
        executor=executor, memory=memory,
        dispatch_instruction_limit=dispatch_instruction_limit,
    )
    hybrid.register_routine_v1(RoutineImageV1(
        name="H-INC", code=bytes(assemble("inc r4\nret.l")), entry_offset=0,
        input_cells=1, output_cells=1, buffers=(), max_instructions=100,
        return_stack_cells=16,
    ))
    return hybrid


def test_preconstructed_bootstrap_retains_declared_words_and_exact_runtime():
    memory = create_one_core_address_space()
    storage = HostedStorageService(_boot_image(autoexec_body=(
        b"PREINSTALLED AUTO-RUNS !\n"
        b'S" \' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    )))
    runtime = MegaForthRuntime(memory=memory, storage=storage, execution_backend="python")
    runtime.define_constant("PREINSTALLED", 73)

    prepared = prepare_image_bootstrap(memory=memory, storage=storage, runtime=runtime)

    assert prepared.runtime is runtime
    assert runtime.memory.read64(runtime.find("AUTO-RUNS").body_address) == 73
    assert runtime.memory.read64(runtime.find("SESSION-RUNS").body_address) == 0


@pytest.mark.parametrize("mismatch", ("memory", "storage", "executor", "type"))
def test_preconstructed_bootstrap_rejects_mismatch_before_reading_source(mismatch):
    memory = create_one_core_address_space()
    storage = HostedStorageService(_boot_image())
    runtime = MegaForthRuntime(memory=memory, storage=storage, execution_backend="python")
    options = dict(memory=memory, storage=storage, runtime=runtime)
    if mismatch == "memory":
        options["memory"] = create_one_core_address_space()
    elif mismatch == "storage":
        options["storage"] = HostedStorageService(_boot_image())
    elif mismatch == "executor":
        options["execution_backend"] = "python"
    else:
        options["runtime"] = object()
    with pytest.raises(TypeError if mismatch == "type" else ValueError):
        prepare_image_bootstrap(**options)
    assert runtime.find("AUTO-RUNS") is None


@pytest.mark.parametrize("executor", ("python", "native"))
def test_hybrid_server_registers_before_boot_and_preserves_shared_control(
    tmp_path, monkeypatch, executor,
):
    pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    now_ns = [7_000_000_000]
    epoch_ms = 1_788_890_400_000
    monkeypatch.setattr("hybrid.server.time.time_ns", lambda: epoch_ms * 1_000_000)
    monkeypatch.setattr("hybrid.server.time.monotonic_ns", lambda: now_ns[0])
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b"40 H-INC AUTO-RUNS !\n"
        b"TERMSIZE AUTO-ROWS ! AUTO-COLS !\n"
        b'S" : HYBRID-LIVE 64 H-INC EMIT KEY EMIT ; '
        b'\' HYBRID-LIVE IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    prepared = prepare_server(args)
    backend = prepared.machine.semantic_session.backend
    semantic = prepared.hybrid.semantic
    try:
        assert prepared.preparation.runtime is semantic
        assert prepared.machine.semantic_session.runtime is semantic
        assert prepared.server.machine is prepared.machine
        assert semantic.memory.dense_backing is not None
        assert not Path(args.socket).exists()
        for name, expected in (("AUTO-RUNS", 41), ("AUTO-COLS", 96), ("AUTO-ROWS", 32)):
            assert semantic.memory.read64(semantic.find(name).body_address) == expected
        assert prepared.machine.semantic_session.semantic_step_budget == (
            10000 - prepared.preparation.autoexec_semantic_steps
        )
        assert (semantic.rtc.uptime_ms, semantic.rtc.epoch_ms) == (0, epoch_ms)
        now_ns[0] += 50_000_000
        assert (semantic.rtc.uptime_ms, semantic.rtc.epoch_ms) == (50, epoch_ms + 50)
        prepared.machine.start()
        status = prepared.server.dispatch("status", {})
        assert status["backend"] == status["runtime"]["mode"] == "hybrid"
        assert status["semantic_execution"]["backend"] == executor
        assert status["semantic_execution"]["quantum_steps"] == 4096
        assert status["runtime"]["timing"]["model"] == "semantic"
        assert not status["runtime"]["timing"]["models_shared_clock_latency"]
        assert status["runtime"]["timing"]["timer_unit"] == "semantic_step"
        capabilities = status["runtime"]["capabilities"]
        assert capabilities["machine_code"] and capabilities["declared_machine_routines"]
        assert all(not capabilities[key] for key in (
            "arbitrary_machine_code", "machine_mmio", "semantic_callbacks",
            "native_bios_boot", "multicore", "native_snapshot", "reset",
            "cpu_diagnostics", "network_diagnostics", "host_profiling",
        ))
        assert status["steps"] == 0
        assert status["machine_execution"]["instructions"] == 2
        assert status["machine_execution"]["transitions"] == 1
        assert status["machine_execution"]["dispatch_instruction_limit"] == 1000
        assert status["hybrid"]["booted"]
        assert "hybrid" not in prepared.server.dispatch("status", {"detailed": False})
        stepped = prepared.server.dispatch("step", {"count": 1})
        assert stepped["stop_reason"] == "idle"
        assert "cycles" not in stepped
        assert stepped["status"]["machine_execution"]["instructions"] == 4
        assert prepared.server.dispatch("raw", {"since": 0})["text"] == "A"
        generation = status["generation"]
        assert prepared.server.dispatch("send_text", {
            "text": "!", "generation": generation - 1,
        }) == {"status": "stale_generation", "accepted_bytes": 0}
        assert prepared.server.dispatch("send_text", {
            "text": "K", "generation": generation,
        }) == {"status": "progress", "accepted_bytes": 1}
        completed = prepared.server.dispatch("step", {"count": 1})
        assert completed["stop_reason"] == "completed"
        assert prepared.server.dispatch("raw", {"since": 0})["text"] == "AK"
        assert completed["status"]["machine_execution"]["instructions"] == 4
    finally:
        prepared.server.stop()
    assert backend.closed and prepared.hybrid.closed
    semantic.evaluate(b"1 DROP")  # Closing released semantic session authority.


@pytest.mark.parametrize("executor", ("python", "native"))
def test_hybrid_machine_budget_survives_session_idle_and_resume(tmp_path, executor):
    hybrid = _runtime(executor, dispatch_instruction_limit=3)
    hybrid.semantic.evaluate(b": ROOT 0 H-INC DROP KEY DROP 0 H-INC DROP ;")
    session = HybridSession(hybrid, "ROOT", semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "budget.sock"))
    try:
        machine.start()
        assert server.dispatch("step", {"count": 1})["stop_reason"] == "idle"
        assert hybrid.machine_instructions == 2
        server.dispatch("send_text", {
            "text": "X", "generation": machine.status()["generation"],
        })
        with pytest.raises(HybridExecutionError):
            server.dispatch("step", {"count": 1})
        assert hybrid.machine_instructions == 3
        assert hybrid.semantic.main_context.data.snapshot() == (0,)
    finally:
        server.stop()
    assert hybrid.closed


def test_session_close_cancels_owned_suspension_before_closing_composition():
    hybrid = _runtime("python")
    hybrid.semantic.evaluate(b": ROOT KEY DROP ;")
    session = HybridSession(hybrid, "ROOT")
    backend = session.backend
    session.boot()
    assert session.run_boundary().stop_reason.value == "idle"
    assert backend.suspended
    session.close()
    session.close()
    assert backend.closed and hybrid.closed
    hybrid.semantic.evaluate(b"7 DROP")


def test_invalid_later_manifest_entry_prevents_all_runtime_publication(tmp_path, monkeypatch):
    args = _server_args(tmp_path, extra_routines=({"name": "INVALID"},))

    def create(**_options):
        pytest.fail("a complete manifest must validate before creating a runtime")

    monkeypatch.setattr("hybrid.server.HybridRuntime.create", create)
    with pytest.raises(HybridManifestError):
        prepare_server(args)
    assert not Path(args.socket).exists()


def test_invalid_quantum_prevents_manifest_and_boot_loading(tmp_path, monkeypatch):
    args = _server_args(tmp_path)
    args.semantic_quantum_steps = None
    monkeypatch.setenv(SEMANTIC_QUANTUM_ENVIRONMENT, "0")

    def load(_path):
        pytest.fail("a bad quantum must fail before loading the manifest")

    monkeypatch.setattr("hybrid.server.load_manifest_v1", load)
    with pytest.raises(ValueError, match=SEMANTIC_QUANTUM_ENVIRONMENT):
        prepare_server(args)


def test_failed_boot_closes_the_unpublished_composition(tmp_path, monkeypatch):
    pytest.importorskip("_mp64_accel")
    args = _server_args(tmp_path, autoexec_body=b"1 AUTO-RUNS +!\n")
    created = []
    create = HybridRuntime.create

    def capture(**options):
        runtime = create(**options)
        created.append(runtime)
        return runtime

    monkeypatch.setattr("hybrid.server.HybridRuntime.create", capture)
    with pytest.raises(ImageBootstrapError, match="did not bind"):
        prepare_server(args)
    assert len(created) == 1 and created[0].closed
    assert not Path(args.socket).exists()


@pytest.mark.parametrize("executor", ("python", "native", "auto"))
def test_hybrid_cli_semantic_executor_does_not_remove_native_machine_requirement(
    tmp_path, monkeypatch, executor,
):
    args = _server_args(tmp_path, executor=executor)
    monkeypatch.setitem(sys.modules, "_mp64_accel", None)
    with pytest.raises(RuntimeError):
        prepare_server(args)
    assert not Path(args.socket).exists()


def test_hybrid_help_and_launcher_route_need_no_native_import():
    source = """
import importlib.abc
import sys
class BlockImports(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in ('_mp64_accel', '_megaforth_native', 'emulator'):
            raise AssertionError('unexpected import: ' + fullname)
sys.meta_path.insert(0, BlockImports())
import megapad
raise SystemExit(megapad.main(['--mode', 'hybrid', '--help']))
"""
    result = subprocess.run(
        [sys.executable, "-c", source], cwd=ROOT, capture_output=True,
        text=True, timeout=20,
    )
    assert result.returncode == 0, result.stderr
    assert "--hybrid-routines" in result.stdout
    assert "--semantic-step-budget" in result.stdout
    assert "--executor {python,native,auto}" in result.stdout
    assert "--bios" not in result.stdout


def test_launcher_forwards_hybrid_arguments_unchanged(monkeypatch):
    calls = []

    def import_module(name):
        assert name == "hybrid.server"
        return SimpleNamespace(main=lambda arguments: calls.append(arguments) or 23)

    monkeypatch.setattr(megapad.importlib, "import_module", import_module)
    arguments = ["--storage", "a disk.img", "--mode=hybrid", "--hybrid-routines", "routines.json"]
    assert megapad.main(arguments) == 23
    assert calls == [["--storage", "a disk.img", "--hybrid-routines", "routines.json"]]
    assert "--mode=hybrid" in arguments
