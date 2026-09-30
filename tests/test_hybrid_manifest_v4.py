"""The explicit V4 loader proves all independent graph metadata before image I/O."""

from dataclasses import FrozenInstanceError
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

from hybrid.manifest import (
    HybridManifestError, load_manifest, load_manifest_v1, load_manifest_v2, load_manifest_v3,
)
from hybrid.nested_manifest import load_nested_manifest_v4
from hybrid.service_manifest import load_service_manifest_v5
from shared.hybrid_abi import HYBRID_ABI, MAX_CODE_BYTES, MAX_MANIFEST_BYTES
from shared.hybrid_nested import (
    CallbackExportV4, CallbackSiteV4, ClosedPolicyV4, PolicyMachineCallV4,
    RoutineImageV4, RoutineManifestV4,
)


def _policy(**changes):
    return dict(policy_id=0, name="POLICY", input_cells=1, output_cells=1,
                operations=[dict(op="call_machine", routine_id=1), dict(op="return")]) | changes


def _export(**changes):
    return dict(export_id=0, policy_id=0, effect="closed_integer_nested", max_semantic_steps=4) | changes


def _leaf(**changes):
    return dict(export_id=1, name="ABS", input_cells=1, output_cells=1,
                effect="integer_leaf", max_semantic_steps=1) | changes


def _site(**changes):
    return dict(call_offset=0, stub_offset=8, export_id=0) | changes


def _buffer(**changes):
    return dict(address_argument=0, length_argument=0, element_bytes=1,
                max_bytes=64, access="read") | changes


def _routine(**changes):
    return dict(routine_id=0, name="PARENT", image="routine.bin", entry_offset=0,
                input_cells=1, output_cells=1, buffers=[], max_instructions=100,
                max_callback_requests=1, return_stack_cells=16, callbacks=[_site()]) | changes


def _document(**changes):
    return dict(abi=HYBRID_ABI, version=4, dispatch_instruction_limit=1000,
                dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
                policies=[_policy()], exports=[_export(), _leaf()], routines=[
                    _routine(), _routine(routine_id=1, name="CHILD", callbacks=[_site(export_id=1)])]) | changes


def _write(tmp_path, document=None, *, code=b"\x01" * 64):
    (tmp_path / "routine.bin").write_bytes(code)
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(_document() if document is None else document), encoding="utf-8")
    return path


def _record_opens(monkeypatch):
    opened = []
    original = os.open

    def record(path, *args, **kwargs):
        opened.append(Path(path))
        return original(path, *args, **kwargs)

    monkeypatch.setattr(os, "open", record)
    return opened


def _reject_before_images(tmp_path, monkeypatch, document, match=None):
    path = _write(tmp_path, document)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match=match):
        load_nested_manifest_v4(path)
    assert opened == [path]


def test_v4_owns_images_and_resolves_exact_shared_descriptors_once(tmp_path, monkeypatch):
    directory = tmp_path / "nested"
    directory.mkdir()
    path = _write(directory)
    monkeypatch.chdir(tmp_path)
    value = load_nested_manifest_v4(path)
    assert type(value) is RoutineManifestV4 and value.version == 4
    assert type(value.policies[0]) is ClosedPolicyV4
    assert type(value.policies[0].operations[0]) is PolicyMachineCallV4
    assert value.policies[0].operations[0].routine_id == 1
    assert [(item.policy_id, item.effect) for item in value.exports] == [
        (0, "closed_integer_nested"), (None, "integer_leaf")]
    assert all(type(item) is CallbackExportV4 for item in value.exports)
    for index, routine in enumerate(value.routines):
        assert type(routine) is RoutineImageV4 and routine.routine_id == index
        assert routine.max_callback_requests == 1 and routine.code == b"\x01" * 64
        assert type(routine.callbacks[0]) is CallbackSiteV4
        assert routine.callbacks[0].export is value.exports[index]
    assert [(node.kind, node.node_id) for node in value.graph_proof().publication_order] == [
        ("routine", 1), ("policy", 0), ("routine", 0)]
    with pytest.raises(FrozenInstanceError):
        value.routines[0].routine_id = 2
    (directory / "routine.bin").write_bytes(b"changed")
    assert value.routines[0].code == b"\x01" * 64


def test_one_manifest_read_and_no_implicit_old_loader_capability(tmp_path, monkeypatch):
    path = _write(tmp_path)
    opened = _record_opens(monkeypatch)
    load_nested_manifest_v4(path)
    assert opened == [path, tmp_path / "routine.bin", tmp_path / "routine.bin"]
    for loader in (load_manifest, load_manifest_v1, load_manifest_v2, load_manifest_v3,
                   load_service_manifest_v5):
        opened.clear()
        with pytest.raises(HybridManifestError):
            loader(path)
        assert opened == [path]


@pytest.mark.parametrize("changes", [
    dict(version=1), dict(version=2), dict(version=3), dict(version=5), dict(version=True),
    dict(abi="other"), dict(dispatch_callback_limit=0), dict(dispatch_callback_limit=1025),
    dict(dispatch_callback_semantic_limit=65537), dict(dispatch_instruction_limit=True),
])
def test_explicit_version_and_dispatch_limits_precede_image_reads(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(**changes))


@pytest.mark.parametrize("routine_changes", [
    dict(routine_id=0), dict(routine_id=True), dict(routine_id=64),
    dict(name="parent"), dict(name="policy"), dict(max_callback_requests=-1),
    dict(max_callback_requests=True), dict(max_callback_requests=1025),
    dict(image="https://example.invalid/code"), dict(image="/absolute.bin"),
    dict(image="C:\\code.bin"), dict(image=""), dict(image="a\x00b"),
    dict(buffers=[_buffer(length_argument=1)]), dict(return_stack_cells=0),
    dict(callbacks=[_site(export_id=1), _site(call_offset=1, stub_offset=9, export_id=1)]),
    dict(callbacks=[_site(export_id=63)]),
])
def test_invalid_later_routine_precedes_every_image_read(tmp_path, monkeypatch, routine_changes):
    document = _document()
    document["routines"][1].update(routine_changes)
    _reject_before_images(tmp_path, monkeypatch, document)


@pytest.mark.parametrize("operations", [
    [dict(op="call_machine", routine_id=0), dict(op="return")],
    [dict(op="return"), dict(op="call_machine", routine_id=0)],
    [dict(op="call_machine", routine_id=63), dict(op="return")],
    [dict(op="call_policy", policy_id=63), dict(op="return")],
    [dict(op="branch", target=0)],
    [dict(op="literal", value=1)],
    [dict(op="call_machine", routine_id=True), dict(op="return")],
])
def test_combined_graph_and_unreachable_operations_are_checked_before_images(tmp_path, monkeypatch, operations):
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[_policy(operations=operations)]))


def test_unused_closed_export_effect_and_work_are_checked_before_images(tmp_path, monkeypatch):
    document = _document(exports=[_export(effect="closed_integer_colon"), _leaf()])
    document["routines"][0]["callbacks"] = []
    _reject_before_images(tmp_path, monkeypatch, document, match="cannot capture")


def test_conditional_policy_work_is_not_sum_of_both_paths(tmp_path):
    operations = [dict(op="branch_zero", target=3), dict(op="call_machine", routine_id=1),
                  dict(op="branch", target=4), dict(op="return"), dict(op="return")]
    document = _document(policies=[_policy(input_cells=2, operations=operations)],
                         exports=[_export(max_semantic_steps=6), _leaf()])
    value = load_nested_manifest_v4(_write(tmp_path, document))
    proof, = value.graph_proof().policy_proofs
    assert (proof.min_semantic_steps, proof.max_semantic_steps) == (2, 6)


def _chain_document(count):
    return _document(
        policies=[_policy(policy_id=i, name=f"P{i}", operations=[
            dict(op="call_machine", routine_id=i + 1), dict(op="return")]) for i in range(count - 1)],
        exports=[_export(export_id=i, policy_id=i, max_semantic_steps=4096) for i in range(count - 1)],
        routines=[_routine(routine_id=i, name=f"R{i}", callbacks=[_site(export_id=i)] if i < count - 1 else [])
                  for i in range(count)],
    )


def test_depth_eight_loads_and_nine_rejects_before_any_image(tmp_path, monkeypatch):
    accepted = load_nested_manifest_v4(_write(tmp_path, _chain_document(8)))
    assert max(proof.max_machine_depth for proof in accepted.graph_proof().routine_proofs) == 8
    _reject_before_images(tmp_path, monkeypatch, _chain_document(9), match="eight active")


def _edge_document(calls, parents):
    sites = [_site(call_offset=i * 3, stub_offset=i * 3 + 2) for i in range(16)]
    return _document(
        policies=[_policy(input_cells=0, output_cells=0, operations=[dict(op="return")]
                          + [dict(op="call_machine", routine_id=63)] * calls)],
        exports=[_export(max_semantic_steps=1)],
        routines=[_routine(routine_id=i, name=f"R{i}", callbacks=sites, max_callback_requests=0)
                  for i in range(parents)] + [_routine(routine_id=63, name="CHILD", callbacks=[])],
    )


@pytest.mark.parametrize("calls,parents,match", [(257, 1, "4096 child edges"), (256, 17, "65536 child edges")])
def test_child_edge_bounds_reject_before_images(tmp_path, monkeypatch, calls, parents, match):
    _reject_before_images(tmp_path, monkeypatch, _edge_document(calls, parents), match=match)


def _objects(document):
    document["routines"][0]["buffers"] = [_buffer()]
    return [document, document["policies"][0], document["policies"][0]["operations"][0],
            document["exports"][0], document["routines"][0],
            document["routines"][0]["callbacks"][0], document["routines"][0]["buffers"][0]]


@pytest.mark.parametrize("index", range(7))
@pytest.mark.parametrize("mutation", ["unknown", "missing", "duplicate"])
def test_exact_fields_and_duplicate_json_keys_at_every_level(tmp_path, monkeypatch, index, mutation):
    document = _document()
    objects = _objects(document)
    row = objects[index]
    first = next(iter(row))
    if mutation == "unknown":
        row["extra"] = 0
        _reject_before_images(tmp_path, monkeypatch, document)
    elif mutation == "missing":
        del row[first]
        _reject_before_images(tmp_path, monkeypatch, document)
    else:
        encoded = json.dumps(document)
        target = json.dumps(row)
        duplicate = "{" + json.dumps(first) + ":" + json.dumps(row[first]) + "," + target[1:]
        encoded = encoded.replace(target, duplicate, 1)
        path = tmp_path / "manifest.json"
        path.write_text(encoded)
        opened = _record_opens(monkeypatch)
        with pytest.raises(HybridManifestError, match="duplicate"):
            load_nested_manifest_v4(path)
        assert opened == [path]


@pytest.mark.parametrize("field,value", [
    ("input_cells", True), ("output_cells", 9), ("policy_id", 64),
    ("operations", []), ("operations", [dict(op="unknown")]),
])
def test_policy_metadata_is_strict_before_images(tmp_path, monkeypatch, field, value):
    _reject_before_images(tmp_path, monkeypatch, _document(policies=[_policy(**{field: value})]))


def test_zero_per_invocation_callbacks_remains_legal_with_declared_sites(tmp_path):
    document = _document()
    for routine in document["routines"]:
        routine["max_callback_requests"] = 0
    value = load_nested_manifest_v4(_write(tmp_path, document))
    assert all(proof.callback_work_bound == 0 for proof in value.graph_proof().routine_proofs)


@pytest.mark.parametrize("code", [b"", b"\x01", b"\x01" * (MAX_CODE_BYTES + 1)])
def test_image_size_and_actual_site_geometry_remain_separate_bounded_checks(tmp_path, code):
    path = _write(tmp_path, code=code)
    with pytest.raises(HybridManifestError):
        load_nested_manifest_v4(path)


def test_padding_counts_toward_existing_aggregate_limit(tmp_path):
    # Each immutable image is just under one MiB but reserves one full MiB
    # after sealed publication padding. No monkeypatch of inherited constants.
    document = _document(policies=[], exports=[], routines=[
        _routine(routine_id=i, name=f"R{i}", callbacks=[], max_callback_requests=0)
        for i in range(16)])
    path = _write(tmp_path, document, code=b"\x01" * (MAX_CODE_BYTES - 1))
    assert len(load_nested_manifest_v4(path).routines) == 16
    document["routines"].append(_routine(routine_id=16, name="R16", callbacks=[], max_callback_requests=0))
    path.write_text(json.dumps(document))
    with pytest.raises(HybridManifestError, match="16 MiB"):
        load_nested_manifest_v4(path)


def test_nonregular_image_is_rejected_without_blocking(tmp_path):
    path = _write(tmp_path)
    image = tmp_path / "routine.bin"
    image.unlink()
    os.mkfifo(image)
    with pytest.raises(HybridManifestError, match="regular local file"):
        load_nested_manifest_v4(path)


def test_manifest_byte_bound_applies_before_json_parsing(tmp_path):
    path = tmp_path / "manifest.json"
    path.write_bytes(b" " * (MAX_MANIFEST_BYTES + 1))
    with pytest.raises(HybridManifestError, match="exceeds"):
        load_nested_manifest_v4(path)


def test_cold_loader_does_not_import_execution_backends(tmp_path):
    path = _write(tmp_path)
    root = Path(__file__).resolve().parents[1]
    code = """
import builtins, sys
original = builtins.__import__
def guarded(name, *args, **kwargs):
    if name.split('.')[0] in {'simulator', 'emulator', '_mp64_accel', '_megaforth_native'}:
        raise AssertionError(name)
    return original(name, *args, **kwargs)
builtins.__import__ = guarded
from hybrid.nested_manifest import load_nested_manifest_v4
value = load_nested_manifest_v4(sys.argv[1])
assert value.version == 4
"""
    subprocess.run([sys.executable, "-c", code, str(path)], cwd=root, check=True)
