"""V3 policy proofs and exact metadata precede every machine-image read."""

from __future__ import annotations

from dataclasses import FrozenInstanceError
import json
from pathlib import Path
import subprocess
import sys

import pytest

import hybrid.manifest as manifest_module
from hybrid.manifest import (
    HybridManifestError, load_manifest, load_manifest_v1, load_manifest_v2, load_manifest_v3,
)
from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI, MAX_CODE_BYTES, MAX_MANIFEST_BYTES,
    CallbackExportV3, CallbackSiteV3, RoutineImageV3, RoutineManifestV1,
    RoutineManifestV2, RoutineManifestV3,
)
from shared.hybrid_closed import (
    ClosedPolicyV3, PolicyLiteralV3, PolicyCoreCallV3, PolicyCallV3,
    PolicyBranchV3, PolicyBranchZeroV3, PolicyReturnV3, prove_policies,
)


def _policy(**changes):
    fields = dict(policy_id=0, name="CLAMP", input_cells=3, output_cells=1,
                  operations=[{"op": "call_core", "name": name} for name in ("ROT", "MIN", "MAX")]
                  + [{"op": "return"}])
    fields.update(changes)
    return fields


def _closed_export(**changes):
    fields = dict(export_id=0, effect="closed_integer_colon", policy_id=0, max_semantic_steps=7)
    fields.update(changes)
    return fields


def _leaf_export(**changes):
    fields = dict(export_id=1, effect="integer_leaf", name="MIN", input_cells=2,
                  output_cells=1, max_semantic_steps=1)
    fields.update(changes)
    return fields


def _site(**changes):
    fields = dict(call_offset=0, stub_offset=8, export_id=0)
    fields.update(changes)
    return fields


def _routine(**changes):
    fields = dict(name="H-CLAMP", image="routine.bin", entry_offset=0,
                  input_cells=3, output_cells=1, buffers=[], max_instructions=100,
                  return_stack_cells=16, callbacks=[_site()])
    fields.update(changes)
    return fields


def _document(**changes):
    fields = dict(abi=HYBRID_ABI, version=3, dispatch_instruction_limit=1000,
                  dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
                  policies=[_policy()], exports=[_closed_export()], routines=[_routine()])
    fields.update(changes)
    return fields


def _write(tmp_path, document=None, *, code=b"\x01" * 32):
    (tmp_path / "routine.bin").write_bytes(code)
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(_document() if document is None else document), encoding="utf-8")
    return path


def _record_opens(monkeypatch):
    opened = []
    original = manifest_module.os.open

    def record(path, *args, **kwargs):
        opened.append(Path(path))
        return original(path, *args, **kwargs)

    monkeypatch.setattr(manifest_module.os, "open", record)
    return opened


def _reject_before_images(tmp_path, monkeypatch, document, *, match=None):
    path = _write(tmp_path, document)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match=match):
        load_manifest_v3(path)
    assert opened == [path]


def test_closed_export_derives_one_immutable_descriptor_from_policy_id(tmp_path):
    path = _write(tmp_path, _document(exports=[_closed_export(), _leaf_export()], routines=[
        _routine(callbacks=[_site(), _site(call_offset=2, stub_offset=9, export_id=1)]),
        _routine(name="SECOND"),
    ]))
    value = load_manifest_v3(path)
    assert type(value) is RoutineManifestV3 and value.version == 3
    assert type(value.policies[0]) is ClosedPolicyV3
    assert tuple(type(op) for op in value.policies[0].operations) == (PolicyCoreCallV3,) * 3 + (PolicyReturnV3,)
    closed, leaf = value.exports
    assert type(closed) is type(leaf) is CallbackExportV3
    assert (closed.name, closed.input_cells, closed.output_cells, closed.max_semantic_steps,
            closed.effect, closed.version) == ("CLAMP", 3, 1, 7, "closed_integer_colon", 3)
    assert (leaf.name, leaf.max_semantic_steps, leaf.effect) == ("MIN", 1, "integer_leaf")
    assert all(type(routine) is RoutineImageV3 for routine in value.routines)
    for routine in value.routines:
        for site in routine.callbacks:
            assert type(site) is CallbackSiteV3
            assert site.export is value.exports[site.export.export_id]
    proof, = prove_policies(value.policies)
    assert (proof.required_input_cells, proof.net_data_cells, proof.max_return_cells,
            proof.min_semantic_steps, proof.max_semantic_steps) == (3, -2, 1, 7, 7)
    with pytest.raises(FrozenInstanceError):
        value.policies[0].name = "CHANGED"
    with pytest.raises(FrozenInstanceError):
        value.policies[0].operations[0].name = "DROP"
    (tmp_path / "routine.bin").write_bytes(b"changed")
    assert value.routines[0].code == b"\x01" * 32


def test_all_operation_forms_preserve_indexes_and_dependency_order(tmp_path):
    choose = _policy(policy_id=2, name="CHOOSE", input_cells=2, operations=[
        {"op": "branch_zero", "target": 4},
        {"op": "call_core", "name": "ABS"},
        {"op": "branch", "target": 5},
        {"op": "return"},
        {"op": "call_policy", "policy_id": 1},
        {"op": "return"},
    ])
    absolute = _policy(policy_id=1, name="INNER", input_cells=1,
                       operations=[{"op": "call_core", "name": "ABS"}, {"op": "return"}])
    constant = _policy(policy_id=3, name="CELL", input_cells=0,
                       operations=[{"op": "literal", "value": MASK64}, {"op": "return"}])
    path = _write(tmp_path, _document(policies=[choose, constant, absolute],
                                     exports=[_closed_export(policy_id=2, max_semantic_steps=6)]))
    value = load_manifest_v3(path)
    assert [policy.policy_id for policy in value.policies] == [2, 3, 1]
    operations = value.policies[0].operations
    assert tuple(type(op) for op in operations) == (
        PolicyBranchZeroV3, PolicyCoreCallV3, PolicyBranchV3,
        PolicyReturnV3, PolicyCallV3, PolicyReturnV3,
    )
    assert operations[0].target == 4 and operations[2].target == 5
    assert value.policies[1].operations == (PolicyLiteralV3(value=MASK64), PolicyReturnV3())
    proofs = prove_policies(value.policies)
    ids = [proof.policy_id for proof in proofs]
    assert ids.index(1) < ids.index(2)
    proof = next(proof for proof in proofs if proof.policy_id == 2)
    assert (proof.min_semantic_steps, proof.max_semantic_steps, proof.max_return_cells) == (5, 6, 2)


@pytest.mark.parametrize("name,inputs,outputs", [
    ("MIN", 2, 1), ("MAX", 2, 1), ("ABS", 1, 1), ("AND", 2, 1),
    ("OR", 2, 1), ("XOR", 2, 1), ("DUP", 1, 2), ("DROP", 1, 0),
    ("SWAP", 2, 2), ("OVER", 2, 3), ("ROT", 3, 3),
])
def test_each_canonical_dependency_has_its_declared_stack_effect(tmp_path, name, inputs, outputs):
    policy = _policy(input_cells=inputs, output_cells=outputs,
                     operations=[{"op": "call_core", "name": name}, {"op": "return"}])
    value = load_manifest_v3(_write(tmp_path, _document(
        policies=[policy], exports=[_closed_export(max_semantic_steps=3)],
    )))
    proof, = prove_policies(value.policies)
    assert (proof.required_input_cells, proof.net_data_cells,
            proof.min_semantic_steps, proof.max_semantic_steps) == (inputs, outputs - inputs, 3, 3)


def test_generic_dispatch_reads_v3_manifest_once(tmp_path, monkeypatch):
    path = _write(tmp_path)
    original = manifest_module._read_bounded
    reads = []

    def read(file, limit, label):
        reads.append((file, limit, label))
        payload = original(file, limit, label)
        if file == path:
            path.write_text("{}")
        return payload

    monkeypatch.setattr(manifest_module, "_read_bounded", read)
    assert type(load_manifest(path)) is RoutineManifestV3
    assert reads == [(path, MAX_MANIFEST_BYTES, "manifest"),
                     (tmp_path / "routine.bin", MAX_CODE_BYTES, "routine image")]


def test_earlier_loaders_and_schemas_stay_exact(tmp_path):
    path = _write(tmp_path)
    for loader in (load_manifest_v1, load_manifest_v2):
        with pytest.raises(HybridManifestError):
            loader(path)
    v2 = _document(version=2, exports=[dict(export_id=0, name="MIN", input_cells=2, output_cells=1)])
    del v2["policies"]
    path.write_text(json.dumps(v2))
    assert type(load_manifest(path)) is RoutineManifestV2
    with pytest.raises(HybridManifestError):
        load_manifest_v3(path)
    v2["exports"][0]["effect"] = "integer_leaf"
    path.write_text(json.dumps(v2))
    with pytest.raises(HybridManifestError, match="unknown fields"):
        load_manifest_v2(path)
    v1 = _document(version=1)
    for key in ("policies", "exports", "dispatch_callback_limit", "dispatch_callback_semantic_limit"):
        del v1[key]
    del v1["routines"][0]["callbacks"]
    path.write_text(json.dumps(v1))
    assert type(load_manifest(path)) is RoutineManifestV1
    with pytest.raises(HybridManifestError):
        load_manifest_v3(path)


@pytest.mark.parametrize("changes", [
    {"abi": "other"}, {"version": 2}, {"version": 4}, {"version": True},
    {"version": 3.0}, {"dispatch_instruction_limit": 0}, {"dispatch_callback_limit": 1025},
    {"dispatch_callback_semantic_limit": 65537}, {"dispatch_callback_limit": True},
    {"policies": {}}, {"policies": [_policy(policy_id=i, name=f"P{i}") for i in range(65)]},
    {"exports": {}}, {"exports": [_closed_export()] * 65}, {"routines": {}},
    {"routines": [_routine(name=f"R{i}") for i in range(65)]},
    {"source_prelude": ": CLAMP ROT MIN MAX ;"}, {"include": "policy.f"},
])
def test_top_level_metadata_rejected_before_images(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(**changes))


@pytest.mark.parametrize("changes", [
    {"policy_id": True}, {"policy_id": -1}, {"policy_id": 64}, {"name": "BAD NAME"},
    {"name": ""}, {"name": "é"}, {"name": "x" * 128}, {"input_cells": True},
    {"input_cells": 9}, {"output_cells": -1}, {"output_cells": 2},
    {"operations": {}}, {"operations": []}, {"source": "ROT MIN MAX"}, {"xt": 123},
])
def test_policy_fields_and_signature_proof_precede_images(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[_policy(**changes)]))


@pytest.mark.parametrize("operation", [
    {}, None, "RETURN", {"op": True}, {"op": "execute", "xt": 1},
    {"op": "call_core", "name": "EXECUTE"}, {"op": "call_core", "name": "F64+"},
    {"op": "call_core", "name": "min"}, {"op": "literal", "value": True},
    {"op": "literal", "value": -1}, {"op": "literal", "value": 1 << 64},
    {"op": "call_policy", "policy_id": True}, {"op": "call_policy", "policy_id": 63},
    {"op": "branch", "target": True}, {"op": "branch", "target": 0},
    {"op": "branch", "target": 2}, {"op": "branch_zero", "target": -1},
    {"op": "return", "value": 0}, {"op": "call_core", "name": "ABS", "xt": 1},
])
def test_operations_are_exact_and_references_are_closed_before_images(tmp_path, monkeypatch, operation):
    policy = _policy(input_cells=1, output_cells=1, operations=[operation, {"op": "return"}])
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[policy]))


@pytest.mark.parametrize("scope", ("manifest", "policy", "operation", "closed_export", "leaf_export", "routine", "callback"))
@pytest.mark.parametrize("mutation", ("duplicate", "missing", "unknown"))
def test_every_json_object_level_has_exact_fields(tmp_path, monkeypatch, scope, mutation):
    document = _document(exports=[_closed_export(), _leaf_export()])
    row, key = {
        "manifest": (document, "policies"),
        "policy": (document["policies"][0], "operations"),
        "operation": (document["policies"][0]["operations"][0], "op"),
        "closed_export": (document["exports"][0], "policy_id"),
        "leaf_export": (document["exports"][1], "max_semantic_steps"),
        "routine": (document["routines"][0], "callbacks"),
        "callback": (document["routines"][0]["callbacks"][0], "call_offset"),
    }[scope]
    if mutation == "missing":
        del row[key]
    elif mutation == "unknown":
        row["extra"] = 0
    payload = json.dumps(document)
    if mutation == "duplicate":
        fragment = json.dumps(key) + ": " + json.dumps(row[key])
        serialized_row = json.dumps(row)
        duplicate_row = serialized_row.replace(fragment, fragment + ", " + fragment, 1)
        payload = payload.replace(serialized_row, duplicate_row, 1)
    path = tmp_path / "manifest.json"
    path.write_text(payload)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="duplicate JSON field|missing fields|unknown fields"):
        load_manifest_v3(path)
    assert opened == [path]


@pytest.mark.parametrize("export", [
    _closed_export(export_id=True), _closed_export(export_id=64),
    _closed_export(policy_id=True), _closed_export(policy_id=1),
    _closed_export(max_semantic_steps=0), _closed_export(max_semantic_steps=4097),
    _closed_export(max_semantic_steps=True), _closed_export(max_semantic_steps=6),
    _closed_export(name="CLAMP"), _closed_export(input_cells=3),
    _closed_export(effect="host_function"), _leaf_export(name="DUP"),
    _leaf_export(max_semantic_steps=2), _leaf_export(input_cells=1),
    _leaf_export(policy_id=0), _leaf_export(effect=True),
])
def test_export_variants_do_not_accept_duplicated_or_unproved_metadata(tmp_path, monkeypatch, export):
    _reject_before_images(tmp_path, monkeypatch, _document(exports=[export]))


@pytest.mark.parametrize("kind", ("policy_id", "policy_name", "routine_name", "export_id", "cross_name"))
def test_duplicate_identity_and_case_folded_name_collisions_precede_images(tmp_path, monkeypatch, kind):
    document = _document()
    if kind == "policy_id":
        document["policies"].append(_policy(name="OTHER"))
    elif kind == "policy_name":
        document["policies"].append(_policy(policy_id=1, name="clamp"))
    elif kind == "routine_name":
        document["routines"].append(_routine(name="h-clamp"))
    elif kind == "export_id":
        document["exports"].append(_leaf_export(export_id=0))
    else:
        document["routines"].append(_routine(name="clamp"))
    _reject_before_images(tmp_path, monkeypatch, document)


def test_unknown_references_and_cycles_in_unused_policies_are_still_rejected(tmp_path, monkeypatch):
    # Neither extra policy is exported; its unreachable Call still participates
    # in the closed call graph and cannot hide mutual recursion after Return.
    policies = [_policy(),
                _policy(policy_id=1, name="P1", input_cells=0, output_cells=0,
                        operations=[{"op": "return"}, {"op": "call_policy", "policy_id": 2}]),
                _policy(policy_id=2, name="P2", input_cells=0, output_cells=0,
                        operations=[{"op": "call_policy", "policy_id": 1}, {"op": "return"}])]
    _reject_before_images(tmp_path, monkeypatch, _document(policies=policies))


@pytest.mark.parametrize("policy", [
    _policy(input_cells=1, operations=[{"op": "call_core", "name": "DROP"}, {"op": "return"}]),
    _policy(input_cells=8, output_cells=8, operations=[{"op": "call_core", "name": "DUP"},
                                                    {"op": "call_core", "name": "DROP"}, {"op": "return"}]),
    _policy(input_cells=0, output_cells=0, operations=[{"op": "call_core", "name": "DROP"}, {"op": "return"}]),
    _policy(input_cells=1, output_cells=1, operations=[{"op": "call_core", "name": "ABS"}]),
    _policy(input_cells=1, output_cells=1, operations=[{"op": "branch_zero", "target": 3},
        {"op": "literal", "value": 1}, {"op": "branch", "target": 4},
        {"op": "branch", "target": 4}, {"op": "return"}]),
])
def test_stack_join_temporary_growth_and_all_return_paths_are_proved_before_images(tmp_path, monkeypatch, policy):
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[policy]))


def test_every_branch_path_must_fit_export_allowance_even_if_literal_selects_shorter_path(tmp_path, monkeypatch):
    policy = _policy(input_cells=1, operations=[
        {"op": "literal", "value": 0}, {"op": "branch_zero", "target": 4},
        {"op": "call_core", "name": "ABS"}, {"op": "branch", "target": 5},
        {"op": "branch", "target": 5}, {"op": "return"},
    ])
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[policy],
                          exports=[_closed_export(max_semantic_steps=4)]))


def test_return_stack_depth_is_bounded_across_distinct_policy_calls(tmp_path, monkeypatch):
    policies = [_policy(policy_id=index, name=f"P{index}", input_cells=0, output_cells=0,
                        operations=([{"op": "call_policy", "policy_id": index + 1}] if index < 8 else [])
                        + [{"op": "return"}]) for index in range(9)]
    _reject_before_images(tmp_path, monkeypatch, _document(policies=policies,
                          exports=[_closed_export(max_semantic_steps=4096)]))


def test_shared_callees_are_summarized_but_repeated_work_still_has_a_bound(tmp_path, monkeypatch):
    policies = [_policy(policy_id=0, name="P0", input_cells=0, output_cells=0,
                        operations=[{"op": "return"}])]
    for index in range(1, 6):
        policies.append(_policy(policy_id=index, name=f"P{index}", input_cells=0, output_cells=0,
                                 operations=[{"op": "call_policy", "policy_id": index - 1}] * 6
                                 + [{"op": "return"}]))
    _reject_before_images(tmp_path, monkeypatch, _document(policies=policies,
                          exports=[_closed_export(policy_id=5, max_semantic_steps=4096)]))


def test_policy_table_total_operations_is_bounded_even_when_suffix_is_unreachable(tmp_path, monkeypatch):
    policies = [_policy(policy_id=index, name=f"P{index}", input_cells=0, output_cells=0,
                        operations=[{"op": "return"}] * count)
                for index, count in enumerate((2048, 2049))]
    _reject_before_images(tmp_path, monkeypatch, _document(policies=policies, exports=[]))


def test_full_4096_operation_table_and_eight_return_cells_remain_admitted(tmp_path):
    policies = [_policy(policy_id=index, name=f"P{index}", input_cells=0, output_cells=0,
                        operations=([{"op": "call_policy", "policy_id": index + 1}] if index < 7 else [])
                        + [{"op": "return"}] * (511 if index < 7 else 512))
                for index in range(8)]
    value = load_manifest_v3(_write(tmp_path, _document(policies=policies,
                                exports=[_closed_export(max_semantic_steps=15)])))
    assert sum(len(policy.operations) for policy in value.policies) == 4096
    proof = next(proof for proof in prove_policies(value.policies) if proof.policy_id == 0)
    assert (proof.max_return_cells, proof.max_semantic_steps) == (8, 15)


@pytest.mark.parametrize("changes", [
    {"image": "https://example.test/routine.bin"}, {"image": "/absolute.bin"},
    {"image": "C:\\routine.bin"}, {"entry_offset": True}, {"input_cells": 9},
    {"callbacks": [_site(export_id=63)]}, {"callbacks": [_site(stub_offset=1)]},
    {"callbacks": [_site(), _site(call_offset=7, stub_offset=9)]},
    {"buffers": [dict(address_argument=3, length_argument=1, element_bytes=1,
                     max_bytes=64, access="read")]},
])
def test_later_routine_metadata_fails_before_earlier_image_is_opened(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(routines=[_routine(), _routine(name="SECOND", **changes)]))


def test_empty_policy_tables_and_all_64_policy_ids_are_data_only(tmp_path):
    value = load_manifest_v3(_write(tmp_path, _document(policies=[], exports=[], routines=[])))
    assert value.policies == value.exports == value.routines == ()
    policies = [_policy(policy_id=index, name=f"P{index}", input_cells=0, output_cells=0,
                        operations=[{"op": "return"}]) for index in range(64)]
    value = load_manifest_v3(_write(tmp_path, _document(policies=policies, exports=[], routines=[])))
    assert len(value.policies) == 64


@pytest.mark.parametrize("code,site", [(b"", _site()), (b"\x01" * 32, _site(call_offset=31)),
                                      (b"\x01" * 32, _site(stub_offset=32))])
def test_actual_code_extent_is_checked_after_metadata_admission(tmp_path, code, site):
    path = _write(tmp_path, _document(routines=[_routine(callbacks=[site])]), code=code)
    with pytest.raises(HybridManifestError):
        load_manifest_v3(path)


def test_v3_loader_imports_no_runtime_native_backend_or_source_compiler(tmp_path):
    path = _write(tmp_path)
    script = r'''
import importlib.abc
import sys
forbidden = ("simulator", "emulator", "_mp64_accel", "_megaforth_native", "asm",
             "megapad64", "system", "accel_wrapper", "hybrid.runtime", "hybrid.server", "hybrid.session")
class Block(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if any(fullname == name or fullname.startswith(name + ".") for name in forbidden):
            raise AssertionError("loader imported execution dependency: " + fullname)
sys.meta_path.insert(0, Block())
from hybrid.manifest import load_manifest, load_manifest_v3
assert load_manifest(sys.argv[1]) == load_manifest_v3(sys.argv[1])
assert not any(any(name == block or name.startswith(block + ".") for block in forbidden) for name in sys.modules)
'''
    result = subprocess.run([sys.executable, "-c", script, str(path)],
                            cwd=Path(__file__).resolve().parents[1], capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
