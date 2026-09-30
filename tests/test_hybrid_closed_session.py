"""Strict declarative V3 policies are installed before the production boot."""

from __future__ import annotations

import json

import pytest

from hybrid import load_manifest, load_manifest_v3
from hybrid.manifest import HybridManifestError
from hybrid.runtime import HybridRuntime
from hybrid.server import prepare_server
from hybrid.session import HybridSession, HybridSharedMachine
from shared.hybrid_abi import HYBRID_ABI, RoutineManifestV3
from shared_session import SessionServer
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.test_hybrid_closed_runtime import _closed_image
from tests.test_hybrid_session import _server_args


def _native(executor):
    native = pytest.importorskip("_mp64_accel")
    if executor == "native":
        pytest.importorskip("_megaforth_native")
    return native


def _write_manifest(path, *, helper=False, empty=False, policy_name="POLICY-CLAMP"):
    image = _closed_image(policy=policy_name, max_semantic_steps=9 if helper else 7)
    (path.parent / "closed.bin").write_bytes(image.code)
    body = [*(dict(op="call_core", name=name) for name in ("ROT", "MIN", "MAX")),
            {"op": "return"}]
    policies = [{"policy_id": 1, "name": policy_name, "input_cells": 3,
                 "output_cells": 1, "operations": body}]
    if helper:
        policies[0]["name"] += "-HELPER"
        # Dependents deliberately occur first in the file. Installation order
        # comes from the proof, never source-order name lookup.
        policies.insert(0, {"policy_id": 2, "name": policy_name, "input_cells": 3,
                            "output_cells": 1, "operations": [
                                {"op": "call_policy", "policy_id": 1}, {"op": "return"},
                            ]})
    document = {
        "abi": HYBRID_ABI, "version": 3,
        "dispatch_instruction_limit": 1000,
        "dispatch_callback_limit": 7,
        "dispatch_callback_semantic_limit": 20,
        "policies": [] if empty else policies,
        "exports": [] if empty else [{
            "export_id": 17, "effect": "closed_integer_colon",
            "policy_id": 2 if helper else 1, "max_semantic_steps": 9 if helper else 7,
        }],
        "routines": [] if empty else [{
            "name": image.name, "image": "closed.bin", "entry_offset": 0,
            "input_cells": 3, "output_cells": 1, "buffers": [],
            "max_instructions": 100, "return_stack_cells": 16,
            "callbacks": [{"call_offset": image.callbacks[0].call_offset,
                           "stub_offset": image.callbacks[0].stub_offset, "export_id": 17}],
        }],
    }
    path.write_text(json.dumps(document), encoding="utf-8")
    return document


@pytest.mark.parametrize("executor", ("python", "native"))
def test_closed_policy_shared_dispatch_reports_actual_reference_work(tmp_path, executor):
    _native(executor)
    hybrid = HybridRuntime.create(
        executor=executor,
        memory=create_one_core_address_space(bank0_size=65536, external_size=4096,
                                             dense_backing=True),
    )
    hybrid.semantic.evaluate(b": CLAMP ROT MIN MAX ;")
    word = hybrid.register_routine_v3(_closed_image())
    for value in (9, 2, 7):
        hybrid.semantic.main_context.data.push(value)
    session = HybridSession(hybrid, word.xt, semantic_step_budget=20,
                            semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "closed.sock"))
    try:
        machine.start()
        result = server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        assert result["semantic_steps"] == result["status"]["steps"] == 8
        assert hybrid.semantic.main_context.data.snapshot() == (7,)
        execution = result["status"]["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"]) == (5, 8, 1, 2, 1, 7)
        assert execution["abi_version"] == 3
        assert execution["registered_abi_versions"] == [3]
        assert execution["native_transport_version"] == 2
        assert execution["callback_profile"] == "closed_integer_colon"
        assert execution["callback_profiles"] == ["closed_integer_colon"]
        assert execution["closed_callback_executor"] == "python_reference"
        assert execution["callback_exports"] == ["CLAMP"]
        assert execution["closed_callback_abi_available"]
        capabilities = result["status"]["runtime"]["capabilities"]
        assert capabilities["closed_integer_callbacks"]
        assert not capabilities["nested_machine_callbacks"]
        assert not capabilities["callback_suspension"]
        assert not capabilities["arbitrary_semantic_callbacks"]
    finally:
        server.stop()
    assert hybrid.closed


@pytest.mark.parametrize("executor", ("python", "native"))
def test_v3_manifest_installs_helper_dependencies_before_boot_once(tmp_path, monkeypatch, executor):
    _native(executor)
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b"9 2 7 H-CLAMP AUTO-RUNS !\n"
        b'S" \' SESSION-MARK IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines, helper=True)
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
        assert semantic.memory.read64(semantic.find("AUTO-RUNS").body_address) == 7
        helper = semantic.find("POLICY-CLAMP-HELPER")
        policy = semantic.find("POLICY-CLAMP")
        assert helper.xt < policy.xt < semantic.find("H-CLAMP").xt
        execution = prepared.server.dispatch("status", {})["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"]) == (5, 8, 1, 2, 1, 9)
        assert execution["callback_exports"] == ["POLICY-CLAMP"]
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("executor", ("python", "native"))
def test_manifest_closed_policy_runs_in_live_production_root(tmp_path, executor):
    _native(executor)
    args = _server_args(tmp_path, executor=executor, autoexec_body=(
        b'S" : CLOSED-LIVE 9 2 7 H-CLAMP ; '
        b'\' CLOSED-LIVE IS _SIMULATOR-SESSION-ENTRY" EVALUATE\n'
    ))
    _write_manifest(args.hybrid_routines)
    prepared = prepare_server(args)
    try:
        assert prepared.hybrid.machine_instructions == 0
        prepared.machine.start()
        result = prepared.server.dispatch("step", {"count": 1})
        assert result["stop_reason"] == "completed"
        # Root -> deferred entry costs 2, @/EXECUTE cost 4, CLOSED-LIVE
        # costs 13 including its seven callback ticks, and two returns cost 2.
        assert result["semantic_steps"] == result["status"]["steps"] == 21
        assert prepared.hybrid.semantic.main_context.data.snapshot() == (7,)
        execution = result["status"]["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"]) == (5, 8, 1, 2, 1, 7)
    finally:
        prepared.server.stop()


@pytest.mark.parametrize("empty", (False, True))
@pytest.mark.parametrize("missing", ("native", "semantic"))
def test_v3_missing_capability_fails_before_policy_or_boot_publication(tmp_path, monkeypatch, empty, missing):
    native = _native("python")
    args = _server_args(tmp_path)
    _write_manifest(args.hybrid_routines, empty=empty)
    if missing == "native":
        monkeypatch.setattr(native, "HYBRID_CALLBACK_ABI_VERSION", -1)
    else:
        monkeypatch.setattr(MegaForthRuntime, "callback_export_abi_version", property(lambda self: 2))
    owners = []
    create = HybridRuntime.create

    def capture(**kwargs):
        owner = create(**kwargs)
        owners.append(owner)
        return owner

    monkeypatch.setattr(HybridRuntime, "create", capture)
    with pytest.raises(RuntimeError, match="semantic profile v3"):
        prepare_server(args)
    assert len(owners) == 1 and owners[0].closed
    assert owners[0].semantic.find("POLICY-CLAMP") is None
    assert owners[0].semantic.find("AUTO-RUNS") is None


def test_policy_namespace_collision_rejects_complete_table_before_first_definition(tmp_path, monkeypatch):
    _native("python")
    args = _server_args(tmp_path)
    document = _write_manifest(args.hybrid_routines)
    collision = dict(document["policies"][0], policy_id=2, name="DUP")
    document["policies"].append(collision)
    args.hybrid_routines.write_text(json.dumps(document), encoding="utf-8")
    owners = []
    create = HybridRuntime.create

    def capture(**kwargs):
        owner = create(**kwargs)
        owners.append(owner)
        return owner

    monkeypatch.setattr(HybridRuntime, "create", capture)
    with pytest.raises(ValueError, match="already exists: DUP"):
        prepare_server(args)
    assert len(owners) == 1 and owners[0].closed
    assert owners[0].semantic.find("POLICY-CLAMP") is None
    assert owners[0].semantic.find("AUTO-RUNS") is None


def test_invalid_closed_metadata_never_constructs_runtime(tmp_path, monkeypatch):
    args = _server_args(tmp_path)
    document = _write_manifest(args.hybrid_routines)
    document["policies"][0]["operations"][0] = {"op": "call_core", "name": "EXECUTE"}
    args.hybrid_routines.write_text(json.dumps(document), encoding="utf-8")

    def forbidden(**kwargs):
        raise AssertionError("invalid manifest must fail before runtime creation")

    monkeypatch.setattr(HybridRuntime, "create", forbidden)
    with pytest.raises(HybridManifestError):
        prepare_server(args)


def test_v3_package_loaders_preserve_explicit_version_and_policy_values(tmp_path):
    path = tmp_path / "closed.json"
    _write_manifest(path, helper=True)
    generic = load_manifest(path)
    strict = load_manifest_v3(path)
    assert type(generic) is type(strict) is RoutineManifestV3
    assert generic == strict
    assert tuple(policy.policy_id for policy in strict.policies) == (2, 1)
    assert strict.exports[0].name == "POLICY-CLAMP"
    assert strict.exports[0].version == strict.routines[0].version == 3
