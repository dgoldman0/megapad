"""V4 application staging keeps startup private until the complete profile exists.

Successful V4 journeys require the complete native transport marker. The
explicit loader is injected into the staged server path until generic V4
dispatch is separately activated after the lower-layer qualification gates.
"""

import json
import os
from pathlib import Path

import pytest

from asm import assemble
from hybrid.manifest import HybridManifestError, load_manifest
from hybrid.nested_manifest import load_nested_manifest_v4
from hybrid.runtime import HybridRuntime
from hybrid.server import _install_nested_manifest, prepare_server
from hybrid.session import HybridSession, HybridSharedMachine
from shared.cells import MASK64
from shared.hybrid_abi import HYBRID_ABI, RoutineImageV1
from shared_session import SessionServer
from simulator.ir import Call
from simulator.platform import create_one_core_address_space
from tests.test_hybrid_session import _server_args


def _native(executor="python", *, nested=False):
    native = pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    if nested and getattr(native, "HYBRID_NESTED_ROUTINE_ABI_VERSION", None) != 3:
        pytest.skip("complete native nested transport v3 is not enabled")
    return native


def _runtime(executor="python", *, nested=False):
    _native(executor, nested=nested)
    owner = HybridRuntime.create(
        executor=executor, require_nested_callbacks=nested,
        memory=create_one_core_address_space(bank0_size=65536, external_size=4096,
                                             dense_backing=True),
    )
    if nested:
        assert owner.nested_callback_abi_available is True
    return owner


@pytest.fixture
def legacy():
    owner = _runtime()
    yield owner
    owner.close()


def _write_manifest(path, *, empty=False):
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
    (path.parent / "nested.bin").write_bytes(code)
    document = {
        "abi": HYBRID_ABI, "version": 4,
        "dispatch_instruction_limit": 1000,
        "dispatch_callback_limit": 8,
        "dispatch_callback_semantic_limit": 64,
        "policies": [] if empty else [{
            "policy_id": 0, "name": "POLICY-NESTED", "input_cells": 1, "output_cells": 1,
            "operations": [{"op": "call_machine", "routine_id": 1}, {"op": "return"}],
        }],
        "exports": [] if empty else [
            {"export_id": 0, "effect": "closed_integer_nested", "policy_id": 0,
             "max_semantic_steps": 4},
            {"export_id": 1, "effect": "integer_leaf", "name": "ABS",
             "input_cells": 1, "output_cells": 1, "max_semantic_steps": 1},
        ],
        "routines": [] if empty else [{
            "routine_id": index, "name": name, "image": "nested.bin", "entry_offset": 0,
            "input_cells": 1, "output_cells": 1, "buffers": [], "max_instructions": 100,
            "max_callback_requests": 1, "return_stack_cells": 16,
            "callbacks": [{"call_offset": labels["call"], "stub_offset": labels["stub"],
                           "export_id": index}],
        } for index, name in enumerate(("H-PARENT", "H-CHILD"))],
    }
    path.write_text(json.dumps(document), encoding="utf-8")
    return document


def _stage_explicit_loader(monkeypatch):
    monkeypatch.setattr("hybrid.server.load_manifest", load_nested_manifest_v4)


def _return_evidence(owner):
    stack = owner.semantic.main_context.returns
    return (stack.pointer, stack.snapshot(), dict(stack._continuations),
            stack._continuation_cookie, stack.pointer_capture_checkpoint(),
            owner.semantic.memory.read_bytes(stack.floor, stack.empty_pointer - stack.floor))


def _assert_nested_work(execution):
    assert (execution["instructions"], execution["cycles"], execution["transitions"],
            execution["segments"], execution["callback_requests"],
            execution["callback_semantic_steps"], execution["max_machine_depth"]) == (10, 16, 2, 4, 2, 4, 2)
    assert execution["abi_version"] == 4
    assert execution["native_transport_version"] == 3
    assert execution["callback_profiles"] == ["canonical_integer_leaf", "closed_integer_nested"]
    assert execution["callback_profile"] == "closed_integer_nested"
    assert execution["closed_callback_executor"] == "python_reference"
    assert execution["nested_callback_abi_available"] is True


@pytest.mark.parametrize("selected", [True, False, 0, 5, "4", 4.0])
def test_manifest_version_is_an_exact_bounded_diagnostic_before_session_claim(legacy, selected):
    word = legacy.semantic.define_primitive("EMPTY", lambda context: None)
    with pytest.raises((TypeError, ValueError), match="manifest ABI version"):
        HybridSession(legacy, word.xt, manifest_abi_version=selected)
    # Failed validation did not claim the semantic session owner.
    legacy.semantic.evaluate(b"1 DROP")


@pytest.mark.parametrize("selected,expected", [(None, 1), (1, 1), (2, 2), (3, 3)])
def test_empty_legacy_status_records_manifest_selection(legacy, selected, expected):
    word = legacy.semantic.define_primitive("EMPTY", lambda context: None)
    session = HybridSession(legacy, word.xt, manifest_abi_version=selected)
    try:
        status = HybridSharedMachine(session).status()
        execution = status["machine_execution"]
        assert execution["manifest_abi_version"] == selected
        assert execution["registered_abi_versions"] == []
        assert execution["abi_version"] == expected
        assert execution["native_transport_version"] == (1 if expected == 1 else 2)
        assert execution["max_machine_depth"] == 0
        assert not status["runtime"]["capabilities"]["nested_machine_callbacks"]
    finally:
        session.close()


def test_manifest_selection_does_not_override_actual_registered_versions(legacy):
    image = RoutineImageV1(name="H-INC", code=bytes(assemble("inc r4\nret.l")),
                           entry_offset=0, input_cells=1, output_cells=1, buffers=(),
                           max_instructions=10, return_stack_cells=8)
    word = legacy.register_routine_v1(image)
    session = HybridSession(legacy, word.xt, manifest_abi_version=3)
    try:
        execution = HybridSharedMachine(session).status()["machine_execution"]
        assert execution["manifest_abi_version"] == 3
        assert execution["registered_abi_versions"] == [1]
        assert execution["abi_version"] == execution["native_transport_version"] == 1
    finally:
        session.close()


def test_direct_v4_session_cannot_advertise_a_missing_profile(legacy, monkeypatch):
    monkeypatch.setattr(HybridRuntime, "nested_callback_abi_available", property(lambda self: False))
    word = legacy.semantic.define_primitive("EMPTY", lambda context: None)
    with pytest.raises(RuntimeError, match="full semantic profile v4"):
        HybridSession(legacy, word.xt, manifest_abi_version=4)
    legacy.semantic.evaluate(b"1 DROP")


@pytest.mark.parametrize("empty", [False, True])
@pytest.mark.parametrize("missing", ["marker", "spec", "runner"])
def test_staged_v4_capability_failure_precedes_publication_boot_or_session(
    tmp_path, monkeypatch, empty, missing,
):
    native = _native()
    args = _server_args(tmp_path)
    _write_manifest(args.hybrid_routines, empty=empty)
    _stage_explicit_loader(monkeypatch)
    monkeypatch.delattr(native, {
        "marker": "HYBRID_NESTED_ROUTINE_ABI_VERSION", "spec": "RoutineSpecV3",
        "runner": "RoutineRunnerV3",
    }[missing], raising=False)
    constructed = []
    original = HybridRuntime.create

    def create(**kwargs):
        assert kwargs["require_nested_callbacks"] is True
        owner = original(**kwargs)
        constructed.append(owner)
        return owner

    def forbidden(*args, **kwargs):
        raise AssertionError("missing capability cannot publish or prepare boot/session")

    monkeypatch.setattr(HybridRuntime, "create", create)
    monkeypatch.setattr("hybrid.server._install_nested_manifest", forbidden)
    monkeypatch.setattr("hybrid.server.prepare_image_bootstrap", forbidden)
    monkeypatch.setattr("hybrid.server.SessionServer", forbidden)
    with pytest.raises(RuntimeError):
        prepare_server(args)
    assert all(owner.closed for owner in constructed)
    assert not Path(args.socket).exists()


def test_generic_dispatch_remains_unchanged_during_application_staging(tmp_path, monkeypatch):
    args = _server_args(tmp_path)
    _write_manifest(args.hybrid_routines)

    def forbidden(**kwargs):
        raise AssertionError("generic V4 startup is not activated by staged application code")

    monkeypatch.setattr(HybridRuntime, "create", forbidden)
    with pytest.raises(HybridManifestError):
        load_manifest(args.hybrid_routines)
    with pytest.raises(HybridManifestError):
        prepare_server(args)


@pytest.mark.parametrize("executor", ["python", "native"])
def test_nested_shared_dispatch_counts_root_work_once_and_preserves_outer_returns(tmp_path, executor):
    hybrid = _runtime(executor, nested=True)
    path = tmp_path / "nested.json"
    _write_manifest(path)
    _install_nested_manifest(hybrid, load_nested_manifest_v4(path))
    context = hybrid.semantic.main_context
    context.data.push(0xCAFE)
    context.data.push(MASK64 - 6)
    evidence = _return_evidence(hybrid)
    session = HybridSession(hybrid, hybrid.semantic.find("H-PARENT").xt,
                            semantic_step_budget=20, semantic_quantum_steps=4096,
                            manifest_abi_version=4)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "nested.sock"))
    try:
        machine.start()
        result = server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        assert result["semantic_steps"] == result["status"]["steps"] == 5
        assert context.data.snapshot() == (0xCAFE, 7)
        assert _return_evidence(hybrid) == evidence
        _assert_nested_work(result["status"]["machine_execution"])
        assert result["status"]["machine_execution"]["manifest_abi_version"] == 4
        capabilities = result["status"]["runtime"]["capabilities"]
        assert capabilities["nested_machine_callbacks"] and capabilities["closed_integer_callbacks"]
        assert not capabilities["callback_suspension"] and not capabilities["arbitrary_semantic_callbacks"]
    finally:
        server.stop()
    assert hybrid.closed


@pytest.mark.parametrize("executor", ["python", "native"])
def test_staged_manifest_uses_mixed_dependency_order_before_boot_once(tmp_path, monkeypatch, executor):
    _native(executor, nested=True)
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b"-7 H-PARENT AUTO-RUNS !\n"
        b'S" \' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines)
    _stage_explicit_loader(monkeypatch)
    opened = []
    original_open = os.open

    def observe(path, *arguments, **kwargs):
        if Path(path) in (args_path, args_path.parent / "nested.bin"):
            opened.append(Path(path))
        return original_open(path, *arguments, **kwargs)

    args_path = args.hybrid_routines
    monkeypatch.setattr("os.open", observe)
    prepared = prepare_server(args)
    try:
        assert opened == [args_path, args_path.parent / "nested.bin", args_path.parent / "nested.bin"]
        semantic = prepared.hybrid.semantic
        child, policy, parent = (semantic.find(name) for name in ("H-CHILD", "POLICY-NESTED", "H-PARENT"))
        assert child.xt < policy.xt < parent.xt
        operation = policy.implementation.operations[0]
        assert type(operation) is Call and operation.xt == child.xt
        assert semantic.memory.read64(semantic.find("AUTO-RUNS").body_address) == 7
        _assert_nested_work(prepared.server.dispatch("status", {})["machine_execution"])
        assert prepared.preparation.runtime is semantic
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("executor", ["python", "native"])
def test_staged_manifest_nested_chain_runs_through_live_production_root(tmp_path, monkeypatch, executor):
    _native(executor, nested=True)
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b'S" : NESTED-LIVE -7 H-PARENT ; '
        b'\' NESTED-LIVE IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines)
    _stage_explicit_loader(monkeypatch)
    prepared = prepare_server(args)
    try:
        assert prepared.hybrid.machine_instructions == 0
        prepared.machine.start()
        result = prepared.server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        # Eight existing deferred-root dispatch ticks plus literal, machine
        # Call+primitive, four actual callback ticks, and the live Word Return.
        assert result["semantic_steps"] == result["status"]["steps"] == 16
        assert prepared.hybrid.semantic.main_context.data.snapshot() == (7,)
        _assert_nested_work(result["status"]["machine_execution"])
    finally:
        prepared.server.stop()


def test_empty_v4_selection_is_truthful_only_with_full_capability(tmp_path, monkeypatch):
    _native(nested=True)
    args = _server_args(tmp_path)
    _write_manifest(args.hybrid_routines, empty=True)
    _stage_explicit_loader(monkeypatch)
    prepared = prepare_server(args)
    try:
        execution = prepared.machine.status()["machine_execution"]
        assert execution["registered_abi_versions"] == []
        assert execution["manifest_abi_version"] == execution["abi_version"] == 4
        assert execution["native_transport_version"] == 3
        assert execution["max_machine_depth"] == 0
        assert execution["nested_callback_abi_available"] is True
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("kind", ["policy", "routine"])
def test_staged_namespace_collision_rejects_before_first_publication(tmp_path, monkeypatch, kind):
    _native(nested=True)
    args = _server_args(tmp_path)
    document = _write_manifest(args.hybrid_routines)
    document["policies" if kind == "policy" else "routines"][0]["name"] = "DUP"
    args.hybrid_routines.write_text(json.dumps(document))
    _stage_explicit_loader(monkeypatch)
    owners = []
    original = HybridRuntime.create

    def capture(**kwargs):
        owner = original(**kwargs)
        owners.append(owner)
        return owner

    monkeypatch.setattr(HybridRuntime, "create", capture)
    with pytest.raises(ValueError, match="already exists: DUP"):
        prepare_server(args)
    assert len(owners) == 1 and owners[0].closed
    assert owners[0].semantic.find("H-CHILD") is None
    assert owners[0].semantic.find("AUTO-RUNS") is None


def test_failed_later_native_publication_closes_unexposed_owner_without_boot(tmp_path, monkeypatch):
    _native(nested=True)
    args = _server_args(tmp_path)
    document = _write_manifest(args.hybrid_routines)
    # Independently valid byte geometry, but the parent's declared CALL points
    # to a MOV. Only native publication may reject its instruction encoding.
    document["routines"][0]["callbacks"][0]["call_offset"] = 0
    args.hybrid_routines.write_text(json.dumps(document))
    _stage_explicit_loader(monkeypatch)
    owners = []
    original = HybridRuntime.create

    def capture(**kwargs):
        owner = original(**kwargs)
        owners.append(owner)
        return owner

    monkeypatch.setattr(HybridRuntime, "create", capture)
    with pytest.raises((RuntimeError, ValueError)):
        prepare_server(args)
    assert len(owners) == 1 and owners[0].closed
    assert owners[0].semantic.find("H-CHILD") is not None
    assert owners[0].semantic.find("H-PARENT") is None
    assert owners[0].semantic.find("AUTO-RUNS") is None
    assert not Path(args.socket).exists()


def test_mixed_legacy_and_v4_registrations_report_actual_versions(tmp_path):
    hybrid = _runtime(nested=True)
    image = RoutineImageV1(name="H-INC", code=bytes(assemble("inc r4\nret.l")),
                           entry_offset=0, input_cells=1, output_cells=1, buffers=(),
                           max_instructions=10, return_stack_cells=8)
    legacy_word = hybrid.register_routine_v1(image)
    path = tmp_path / "nested.json"
    _write_manifest(path)
    _install_nested_manifest(hybrid, load_nested_manifest_v4(path))
    hybrid.semantic.main_context.data.push(8)
    hybrid.execute_xt(legacy_word.xt)
    assert hybrid.semantic.main_context.data.pop() == 9
    hybrid.semantic.main_context.data.push(MASK64 - 6)
    hybrid.execute_xt(hybrid.semantic.find("H-PARENT").xt)
    assert hybrid.semantic.main_context.data.pop() == 7
    empty = hybrid.semantic.define_primitive("EMPTY", lambda context: None)
    session = HybridSession(hybrid, empty.xt, manifest_abi_version=1)
    try:
        execution = HybridSharedMachine(session).status()["machine_execution"]
        assert execution["manifest_abi_version"] == 1
        assert execution["registered_abi_versions"] == [1, 4]
        assert execution["abi_version"] == 4 and execution["native_transport_version"] == 3
        assert execution["transitions"] == 3
        assert execution["max_machine_depth"] == 2
    finally:
        session.close()
