"""Production defaults require native execution without changing the oracle API."""

from __future__ import annotations

import importlib
import json
import os
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace

import pytest

from shared.hybrid_abi import HYBRID_ABI
from shared.session_options import configured_production_executor
from tests.simulator.test_image_bootstrap import _boot_image


ROOT = Path(__file__).resolve().parents[1]


@pytest.fixture(autouse=True)
def isolated_executor_environment(monkeypatch):
    monkeypatch.delenv("MEGAFORTH_EXECUTOR", raising=False)
    monkeypatch.delenv("MEGAFORTH_QUANTUM_STEPS", raising=False)


@pytest.mark.parametrize("explicit,environment,expected", (
    (None, None, "native"), (None, "python", "python"),
    (None, "native", "native"), (None, "auto", "auto"),
    ("python", "native", "python"), ("native", "python", "native"),
    ("auto", "native", "auto"), ("python", "invalid", "python"),
))
def test_production_priority_does_not_mutate_environment(monkeypatch, explicit, environment, expected):
    if environment is not None:
        monkeypatch.setenv("MEGAFORTH_EXECUTOR", environment)
    before = dict(os.environ)
    assert configured_production_executor(explicit) == expected
    assert dict(os.environ) == before


@pytest.mark.parametrize("value", ("", "invalid", "NATIVE"))
def test_invalid_environment_is_not_treated_as_auto(monkeypatch, value):
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", value)
    with pytest.raises(ValueError, match="MEGAFORTH_EXECUTOR"):
        configured_production_executor(None)


@pytest.fixture(params=("simulator", "hybrid"))
def production_server(request, tmp_path):
    mode = request.param
    if mode == "hybrid":
        pytest.importorskip("_mp64_accel")
    module = importlib.import_module("simulator_server" if mode == "simulator" else "hybrid.server")
    image = tmp_path / "source.img"
    image.write_bytes(_boot_image())
    arguments = ["--storage", str(image), "--socket", str(tmp_path / "session.sock"),
                 "--ram-kib", "64", "--ext-mem-mib", "0", "--vram-mib", "0",
                 "--semantic-step-budget", "10000"]
    if mode == "hybrid":
        manifest = tmp_path / "routines.json"
        manifest.write_text(json.dumps({
            "abi": HYBRID_ABI, "version": 1,
            "dispatch_instruction_limit": 1000, "routines": [],
        }))
        arguments += ["--hybrid-routines", str(manifest)]
    return module, arguments


@pytest.mark.parametrize("extension", (None, SimpleNamespace(SEMANTIC_API_VERSION=0)),
                         ids=("missing", "stale"))
def test_omitted_executor_requires_matching_native_before_source_or_socket(
    production_server, monkeypatch, extension,
):
    module, arguments = production_server
    monkeypatch.setitem(sys.modules, "_megaforth_native", extension)

    def unexpected_source(*args, **kwargs):
        pytest.fail("required native selection must fail before source preparation")

    monkeypatch.setattr("simulator.image_bootstrap._evaluate_checked_source", unexpected_source)
    args = module.build_argument_parser().parse_args(arguments)
    with pytest.raises(RuntimeError, match="requires .*_megaforth_native"):
        module.prepare_server(args)
    assert not Path(args.socket).exists()


@pytest.mark.parametrize("explicit,environment", (
    ("python", "native"), (None, "python"), ("auto", "native"), (None, "auto"),
))
def test_only_selected_python_or_auto_can_prepare_without_semantic_extension(
    production_server, monkeypatch, explicit, environment,
):
    module, arguments = production_server
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", environment)
    monkeypatch.setitem(sys.modules, "_megaforth_native", None)
    if explicit is not None:
        arguments += ["--executor", explicit]
    args = module.build_argument_parser().parse_args(arguments)
    prepared = module.prepare_server(args)
    try:
        runtime = prepared.preparation.runtime
        assert runtime.execution_backend == "python"
        auto_runs = runtime.find("AUTO-RUNS")
        assert runtime.memory.read64(auto_runs.body_address) == 1
        assert os.environ["MEGAFORTH_EXECUTOR"] == environment
    finally:
        prepared.machine.stop()


def test_omitted_executor_selects_native_for_preparation_and_live_owner(production_server):
    pytest.importorskip("_megaforth_native")
    module, arguments = production_server
    prepared = module.prepare_server(module.build_argument_parser().parse_args(arguments))
    try:
        assert prepared.preparation.runtime.execution_backend == "native"
        assert prepared.machine.semantic_session.runtime is prepared.preparation.runtime
        assert prepared.machine.semantic_session.semantic_quantum_steps == 65536
    finally:
        prepared.machine.stop()


def test_embedded_semantic_runtime_retains_python_default(monkeypatch):
    from simulator.runtime import MegaForthRuntime

    monkeypatch.setitem(sys.modules, "_megaforth_native", None)
    runtime = MegaForthRuntime()
    runtime.evaluate(b"2 3 +")
    assert runtime.execution_backend == "python"
    assert runtime.main_context.data.snapshot() == (5,)


def test_embedded_hybrid_runtime_retains_python_default(monkeypatch):
    pytest.importorskip("_mp64_accel")
    from hybrid.runtime import HybridRuntime

    monkeypatch.setitem(sys.modules, "_megaforth_native", None)
    owner = HybridRuntime.create(geometry={"bank0_size": 65536, "external_size": 0})
    try:
        owner.evaluate(b"2 3 +")
        assert owner.executor == "python"
        assert owner.semantic.main_context.data.snapshot() == (5,)
    finally:
        owner.close()


@pytest.mark.parametrize("mode", (None, "simulator", "hybrid"))
def test_default_help_never_loads_native_extensions_even_with_bad_environment(mode):
    source = """
import importlib.abc
import sys
class RejectNative(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname in ('_mp64_accel', '_megaforth_native'):
            raise AssertionError('help imported ' + fullname)
sys.meta_path.insert(0, RejectNative())
from megapad import main
raise SystemExit(main(sys.argv[1:]))
"""
    arguments = ["--help"] if mode is None else ["--mode", mode, "--help"]
    result = subprocess.run(
        [sys.executable, "-c", source, *arguments], cwd=ROOT,
        env={**os.environ, "MEGAFORTH_EXECUTOR": "invalid"},
        capture_output=True, text=True, timeout=20,
    )
    assert result.returncode == 0, result.stderr
    expected = "default mode: emulator" if mode is None else "otherwise required native"
    assert expected in " ".join(result.stdout.split())
