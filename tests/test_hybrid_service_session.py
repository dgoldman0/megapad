"""Private scalar services use the normal manifest and shared session path."""

import json
from pathlib import Path

import pytest

from asm import assemble
from hybrid.manifest import HybridManifestError, load_manifest
from hybrid.runtime import HybridRuntime
from hybrid.server import prepare_server
from hybrid.session import HybridSession, HybridSharedMachine
from shared.hybrid_abi import HYBRID_ABI
from shared.hybrid_nested import CallbackExportV4, CallbackSiteV4, RoutineImageV4
from shared.hybrid_services import RoutineManifestV5
from shared_session import SessionServer
from simulator.ir import Call, Return
from tests.test_hybrid_session import _server_args


ONE = 0x3FF0000000000000
INFINITY = 0x7FF0000000000000
DIVIDE_BY_ZERO = 0x80


def _manifest(path, *, empty=False):
    source = "mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\nret.l\nstub:\nret.l"
    labels = {}
    assemble(source, labels_out=labels)
    source = source.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
    (path.parent / "service.bin").write_bytes(bytes(assemble(source)))
    catalog = (("FPCSR!", "H-CSR!", 1, 0), ("F64/", "H-DIV", 2, 1), ("FPCSR@", "H-CSR@", 0, 1))
    document = {
        "abi": HYBRID_ABI, "version": 5, "dispatch_instruction_limit": 1000,
        "dispatch_callback_limit": 8, "dispatch_callback_semantic_limit": 64,
        "exports": [] if empty else [
            {"export_id": index, "name": name, "input_cells": inputs, "output_cells": outputs,
             "effect": "scalar_fp_state", "max_semantic_steps": 1}
            for index, (name, _machine, inputs, outputs) in enumerate(catalog)
        ],
        "routines": [] if empty else [
            {"name": machine, "image": "service.bin", "entry_offset": 0,
             "input_cells": inputs, "output_cells": outputs, "buffers": [],
             "max_instructions": 100, "max_callback_requests": 1, "return_stack_cells": 16,
             "callbacks": [{"call_offset": labels["call"], "stub_offset": labels["stub"],
                            "export_id": index}]}
            for index, (_name, machine, inputs, outputs) in enumerate(catalog)
        ],
    }
    path.write_text(json.dumps(document))
    return document


def _native(executor):
    pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")


def _assert_profile(status, executor, *, empty=False):
    work = status["machine_execution"]
    assert work["abi_version"] == work["manifest_abi_version"] == 5
    assert work["registered_abi_versions"] == ([] if empty else [5])
    assert work["native_transport_version"] == 2 and work["native_transport_versions"] == [2]
    assert work["service_callback_abi_available"] is True
    assert work["service_callback_profile"] == "private_scalar_fp_v1"
    assert work["service_value_executor"] == ("python_reference" if executor == "python" else "shared_native_kernel")
    assert work["service_parked_depth_limit"] == 1
    assert work["max_machine_depth"] == (0 if empty else 1)
    assert work["callback_profiles"] == ([] if empty else ["scalar_fp_state"])
    assert work["private_callback_executor"] == (None if empty else "python_reference")
    assert status["runtime"]["capabilities"]["private_scalar_fp_v1"] is True
    assert status["runtime"]["capabilities"]["nested_machine_callbacks"] is False
    assert status["runtime"]["capabilities"]["callback_suspension"] is False


@pytest.mark.parametrize("executor", ("python", "native"))
def test_strict_service_manifest_executes_shared_fpcsr_and_exact_bits_through_live_root(tmp_path, executor):
    _native(executor)
    body = (f'S" : SERVICE-LIVE 0 H-CSR! {ONE} 0 H-DIV H-CSR@ ; '
            '\' SERVICE-LIVE IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n').encode()
    args = _server_args(tmp_path, executor=executor, autoexec_body=body)
    _manifest(args.hybrid_routines)
    assert type(load_manifest(args.hybrid_routines)) is RoutineManifestV5
    original_image = args.storage.read_bytes()
    prepared = prepare_server(args)
    semantic = prepared.hybrid.semantic
    scalar = semantic.scalar_float
    try:
        assert prepared.hybrid.machine_instructions == 0
        prepared.machine.start()
        result = prepared.server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        assert semantic.main_context.data.snapshot() == (INFINITY, DIVIDE_BY_ZERO)
        assert semantic.scalar_float is scalar and scalar.fpcsr == DIVIDE_BY_ZERO
        _assert_profile(result["status"], executor)
        work = result["status"]["machine_execution"]
        assert (work["instructions"], work["cycles"], work["transitions"], work["segments"],
                work["callback_requests"], work["callback_semantic_steps"]) == (15, 24, 3, 6, 3, 3)
        assert semantic.main_context.returns.snapshot() == ()
        assert not Path(args.socket).exists()
    finally:
        prepared.server.stop()
    assert prepared.hybrid.closed and args.storage.read_bytes() == original_image


@pytest.mark.parametrize("executor", ("python", "native"))
def test_empty_service_manifest_still_requires_and_reports_exact_capability(tmp_path, executor):
    _native(executor)
    args = _server_args(tmp_path, executor=executor)
    _manifest(args.hybrid_routines, empty=True)
    prepared = prepare_server(args)
    try:
        status = prepared.server.dispatch("status", {})
        _assert_profile(status, executor, empty=True)
        assert status["machine_execution"]["instructions"] == 0
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("empty", (False, True))
def test_missing_service_capability_precedes_boot_or_session_claim(tmp_path, monkeypatch, empty):
    _native("python")
    args = _server_args(tmp_path)
    _manifest(args.hybrid_routines, empty=empty)
    monkeypatch.setattr(HybridRuntime, "service_callback_abi_available", property(lambda self: False))
    def forbidden(*args, **kwargs):
        raise AssertionError("unavailable scalar profile must not publish or boot")
    monkeypatch.setattr(HybridRuntime, "register_routine_v5", forbidden)
    monkeypatch.setattr("hybrid.server.prepare_image_bootstrap", forbidden)
    monkeypatch.setattr("hybrid.server.SessionServer", forbidden)
    with pytest.raises(RuntimeError):
        prepare_server(args)
    assert not Path(args.socket).exists()


def test_invalid_service_declaration_fails_before_runtime_creation(tmp_path, monkeypatch):
    args = _server_args(tmp_path)
    document = _manifest(args.hybrid_routines)
    document["exports"][1]["name"] = "ARBITRARY-SERVICE"
    args.hybrid_routines.write_text(json.dumps(document))
    def forbidden(**kwargs):
        raise AssertionError("invalid scalar manifest must fail before runtime creation")
    monkeypatch.setattr(HybridRuntime, "create", forbidden)
    with pytest.raises(HybridManifestError):
        prepare_server(args)


@pytest.mark.parametrize("executor", ("python", "native"))
def test_mixed_nested_and_service_words_keep_distinct_metadata_and_transport_status(tmp_path, executor):
    _native(executor)
    path = tmp_path / "services.json"
    _manifest(path)
    service = load_manifest(path).routines[2]
    owner = HybridRuntime.create(executor=executor, require_service_callbacks=True,
                                 require_nested_callbacks=True,
                                 geometry={"bank0_size": 65536, "external_size": 65536})
    server = None
    try:
        export = CallbackExportV4(export_id=10, name="ABS", input_cells=1, output_cells=1)
        site = service.callbacks[0]
        absolute = owner.register_routine_v4(RoutineImageV4(
            routine_id=0, name="H-ABS", code=service.code, entry_offset=0,
            input_cells=1, output_cells=1, buffers=(), max_instructions=100,
            max_callback_requests=1, return_stack_cells=16,
            callbacks=(CallbackSiteV4(call_offset=site.call_offset,
                                      stub_offset=site.stub_offset, export=export),),
        ))
        read_csr = owner.register_routine_v5(service)
        entry = owner.semantic.define_colon("MIXED", (Call(absolute.xt), Call(read_csr.xt), Return()))
        owner.semantic.main_context.data.push((1 << 64) - 7)
        session = HybridSession(owner, entry.xt, manifest_abi_version=5)
        machine = HybridSharedMachine(session)
        machine.paused = True
        server = SessionServer(machine, str(tmp_path / "mixed.sock"))
        machine.start()
        result = server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        assert owner.semantic.main_context.data.snapshot() == (7, 0)
        status = result["status"]
        work = status["machine_execution"]
        assert work["abi_version"] == work["manifest_abi_version"] == 5
        assert work["registered_abi_versions"] == [4, 5]
        assert work["native_transport_versions"] == [2, 3] and work["native_transport_version"] == 3
        assert work["callback_profiles"] == ["canonical_integer_leaf", "scalar_fp_state"]
        assert status["runtime"]["capabilities"]["nested_machine_callbacks"] is True
        assert status["runtime"]["capabilities"]["private_scalar_fp_v1"] is True
        assert (work["instructions"], work["cycles"], work["transitions"], work["segments"],
                work["callback_requests"], work["callback_semantic_steps"]) == (10, 16, 2, 4, 2, 2)
    finally:
        if server is not None:
            server.stop()
        else:
            owner.close()
