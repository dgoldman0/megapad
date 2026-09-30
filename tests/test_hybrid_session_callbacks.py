"""V2 manifests and callback accounting through the production shared session."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from asm import assemble
from hybrid import load_manifest, load_manifest_v2
from hybrid.manifest import HybridManifestError
from hybrid.runtime import HybridRuntime
from hybrid.server import prepare_server
from hybrid.session import HybridSession, HybridSharedMachine
from shared.hybrid_abi import CallbackExportV2, CallbackSiteV2, HYBRID_ABI, RoutineImageV2
from shared_session import SessionServer
from simulator.platform import create_one_core_address_space
from tests.test_hybrid_session import _server_args


def _image():
    source = """
        mov r12, r3
    after_pc:
        addi r12, 0
    call:
        call.l r12
        ret.l
    stub:
        ret.l
    """
    labels = {}
    assemble(source, labels_out=labels)
    delta = labels["stub"] - labels["after_pc"]
    code = bytes(assemble(source.replace("addi r12, 0", f"addi r12, {delta}")))
    export = CallbackExportV2(export_id=7, name="MIN", input_cells=2, output_cells=1)
    return RoutineImageV2(
        name="H-MIN", code=code, entry_offset=0, input_cells=2, output_cells=1,
        buffers=(), max_instructions=100, return_stack_cells=16,
        callbacks=(CallbackSiteV2(call_offset=labels["call"],
                                  stub_offset=labels["stub"], export=export),),
    )


def _write_manifest(path, *, empty=False, callback_free=False):
    image = _image()
    code = bytes(assemble("inc r4\nret.l")) if callback_free else image.code
    (path.parent / "callback.bin").write_bytes(code)
    document = {
        "abi": HYBRID_ABI,
        "version": 2,
        "dispatch_instruction_limit": 1000,
        "dispatch_callback_limit": 7,
        "dispatch_callback_semantic_limit": 9,
        "exports": [{"export_id": 7, "name": "MIN", "input_cells": 2, "output_cells": 1}],
        "routines": [] if empty else [{
            "name": image.name,
            "image": "callback.bin",
            "entry_offset": 0,
            "input_cells": 1 if callback_free else 2,
            "output_cells": 1,
            "buffers": [],
            "max_instructions": 100,
            "return_stack_cells": 16,
            "callbacks": [] if callback_free else [{
                "call_offset": image.callbacks[0].call_offset,
                "stub_offset": image.callbacks[0].stub_offset,
                "export_id": 7,
            }],
        }],
    }
    path.write_text(json.dumps(document), encoding="utf-8")
    return document


def _native(executor):
    native = pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    return native


@pytest.mark.parametrize("executor", ("python", "native"))
def test_shared_dispatch_settles_exact_callback_clock_and_segment_counts(tmp_path, executor):
    _native(executor)
    memory = create_one_core_address_space(bank0_size=65536, external_size=4096,
                                           dense_backing=True)
    hybrid = HybridRuntime.create(executor=executor, memory=memory,
                                  dispatch_callback_limit=7,
                                  dispatch_callback_semantic_limit=9)
    word = hybrid.register_routine_v2(_image())
    hybrid.semantic.main_context.data.push(9)
    hybrid.semantic.main_context.data.push(4)
    session = HybridSession(hybrid, word.xt, semantic_step_budget=20,
                            semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "callback.sock"))
    semantic_cycles = hybrid.semantic.diagnostics.semantic_cycles
    timer = hybrid.semantic.timer.counter
    try:
        machine.start()
        before = server.dispatch("status", {})
        assert before["steps"] == 0
        assert before["machine_execution"]["callback_requests"] == 0
        completed = server.dispatch("step", {"count": 1})
        assert completed["stop_reason"] == "completed"
        assert completed["boundaries"] == 1
        assert completed["semantic_steps"] == completed["status"]["steps"] == 2
        assert completed["status"]["hybrid"]["semantic_steps"] == 2
        assert hybrid.semantic.diagnostics.semantic_cycles - semantic_cycles == 2
        assert hybrid.semantic.timer.counter - timer == 2
        assert hybrid.semantic.main_context.data.snapshot() == (4,)
        execution = completed["status"]["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"]) == (5, 8, 1, 2, 1, 1)
        assert execution["abi_version"] == 2
        assert execution["registered_abi_versions"] == [2]
        assert execution["callback_abi_available"]
        assert execution["callback_profile"] == "canonical_integer_leaf"
        assert execution["callback_exports"] == ["MIN"]
        assert execution["dispatch_callback_limit"] == 7
        assert execution["dispatch_callback_semantic_limit"] == 9
        capabilities = completed["status"]["runtime"]["capabilities"]
        assert capabilities["semantic_callbacks"]
        assert all(not capabilities[key] for key in (
            "arbitrary_semantic_callbacks", "nested_machine_callbacks",
            "callback_suspension", "machine_mmio", "multicore",
        ))
        compact = server.dispatch("status", {"detailed": False})
        assert compact["machine_execution"] == execution
        assert "hybrid" not in compact
    finally:
        server.stop()
    assert hybrid.closed


@pytest.mark.parametrize("executor", ("python", "native"))
def test_v2_server_loads_once_and_registers_before_boot_source(tmp_path, monkeypatch, executor):
    _native(executor)
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b"9 4 H-MIN AUTO-RUNS !\n"
        b'S" \' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines)
    import hybrid.manifest as loader

    reads = []
    read = loader._read_bounded

    def observe(path, limit, label):
        reads.append(label)
        return read(path, limit, label)

    monkeypatch.setattr(loader, "_read_bounded", observe)
    prepared = prepare_server(args)
    try:
        assert reads == ["manifest", "routine image"]
        semantic = prepared.hybrid.semantic
        assert semantic.memory.read64(semantic.find("AUTO-RUNS").body_address) == 4
        assert prepared.hybrid.dispatch_callback_limit == 7
        assert prepared.hybrid.dispatch_callback_semantic_limit == 9
        status = prepared.server.dispatch("status", {})
        execution = status["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"]) == (5, 8, 1, 2, 1, 1)
        assert status["runtime"]["capabilities"]["semantic_callbacks"]
        assert execution["callback_exports"] == ["MIN"]
        assert status["steps"] == 0  # Preparation is outside the live root session.
    finally:
        prepared.server.stop()
    assert prepared.hybrid.closed


def test_unused_v2_exports_do_not_advertise_semantic_callback_admission(tmp_path):
    _native("python")
    args = _server_args(tmp_path, autoexec_body=(
        b"40 H-MIN AUTO-RUNS !\n"
        b'S" \' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines, callback_free=True)
    prepared = prepare_server(args)
    try:
        status = prepared.server.dispatch("status", {})
        assert not status["runtime"]["capabilities"]["semantic_callbacks"]
        execution = status["machine_execution"]
        assert execution["abi_version"] == 2
        assert execution["callback_profile"] is None
        assert execution["callback_exports"] == []
        assert execution["callback_requests"] == execution["callback_semantic_steps"] == 0
        assert execution["instructions"] == 2
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("empty", (False, True))
def test_v2_manifest_requires_matching_callback_extension_before_boot(tmp_path, monkeypatch, empty):
    native = _native("python")
    args = _server_args(tmp_path)
    _write_manifest(args.hybrid_routines, empty=empty)
    created = []
    create = HybridRuntime.create

    def capture(**options):
        owner = create(**options)
        created.append(owner)
        return owner

    def unexpected(**_options):
        pytest.fail("stale callback extension reached boot source")

    monkeypatch.setattr(native, "HYBRID_CALLBACK_ABI_VERSION", -1)
    monkeypatch.setattr("hybrid.server.HybridRuntime.create", capture)
    monkeypatch.setattr("hybrid.server.prepare_image_bootstrap", unexpected)
    with pytest.raises(RuntimeError, match="matching _mp64_accel v2"):
        prepare_server(args)
    assert len(created) == 1 and created[0].closed
    assert not Path(args.socket).exists()


def test_invalid_v2_export_rejected_before_runtime_creation(tmp_path, monkeypatch):
    args = _server_args(tmp_path)
    document = _write_manifest(args.hybrid_routines)
    document["exports"][0]["name"] = "EXECUTE"
    args.hybrid_routines.write_text(json.dumps(document), encoding="utf-8")

    def unexpected(**_options):
        pytest.fail("invalid callback metadata reached runtime creation")

    monkeypatch.setattr("hybrid.server.HybridRuntime.create", unexpected)
    with pytest.raises(HybridManifestError, match="canonical integer leaf"):
        prepare_server(args)


def test_package_exports_strict_and_version_selecting_manifest_loaders(tmp_path):
    path = tmp_path / "routines.json"
    _write_manifest(path)
    selected = load_manifest(path)
    strict = load_manifest_v2(path)
    assert selected == strict
    assert selected.version == 2 and selected.routines[0].callbacks[0].export.name == "MIN"
