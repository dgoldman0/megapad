"""V2 manifests resolve finite callback metadata before opening any image."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

import hybrid.manifest as manifest_module
from hybrid.manifest import HybridManifestError, load_manifest, load_manifest_v1, load_manifest_v2
from shared.hybrid_abi import (
    HYBRID_ABI, MAX_CODE_BYTES, MAX_MANIFEST_BYTES, MAX_TOTAL_CODE_BYTES,
    CallbackExportV2, RoutineImageV1, RoutineManifestV1, RoutineManifestV2,
)


def _export(**changes):
    fields = dict(export_id=0, name="MIN", input_cells=2, output_cells=1)
    fields.update(changes)
    return fields


def _site(**changes):
    fields = dict(call_offset=0, stub_offset=8, export_id=0)
    fields.update(changes)
    return fields


def _routine(**changes):
    fields = dict(name="H-CALLBACK", image="routine.bin", entry_offset=0,
                  input_cells=2, output_cells=1, buffers=[], max_instructions=100,
                  return_stack_cells=16, callbacks=[_site()])
    fields.update(changes)
    return fields


def _document(**changes):
    fields = dict(abi=HYBRID_ABI, version=2, dispatch_instruction_limit=1000,
                  dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
                  exports=[_export()], routines=[_routine()])
    fields.update(changes)
    return fields


def _write(tmp_path, document=None, code=b"\x01" * 32):
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


def test_v2_loads_immutable_values_and_resolves_each_export_once(tmp_path, monkeypatch):
    directory = tmp_path / "metadata"
    directory.mkdir()
    path = _write(directory, _document(routines=[
        _routine(callbacks=[_site(), _site(call_offset=2, stub_offset=9)]),
        _routine(name="SECOND"),
    ]))
    monkeypatch.chdir(tmp_path)
    value = load_manifest_v2(path)
    assert type(value) is RoutineManifestV2
    assert (value.abi, value.version) == (HYBRID_ABI, 2)
    assert (value.dispatch_instruction_limit, value.dispatch_callback_limit,
            value.dispatch_callback_semantic_limit) == (1000, 32, 64)
    export, = value.exports
    assert (export.max_semantic_steps, export.effect) == (1, "integer_leaf")
    assert all(site.export is export for routine in value.routines for site in routine.callbacks)
    with pytest.raises(FrozenInstanceError):
        value.exports = ()
    with pytest.raises(FrozenInstanceError):
        value.routines[0].callbacks[0].stub_offset = 16
    (directory / "routine.bin").write_bytes(b"replacement")
    assert value.routines[0].code == b"\x01" * 32
    # Numerical metadata can describe NOP bytes. Only native publication proves
    # that these are admitted instruction boundaries and CALL.L/RET.L encodings.


def test_version_dispatch_reads_manifest_once_and_does_not_reopen_after_selection(tmp_path, monkeypatch):
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
    assert type(load_manifest(path)) is RoutineManifestV2
    assert [(limit, label) for file, limit, label in reads if file == path] == [
        (MAX_MANIFEST_BYTES, "manifest"),
    ]
    assert reads[1][1:] == (MAX_CODE_BYTES, "routine image")


def test_v1_loader_remains_strict_and_dispatch_preserves_v1_values(tmp_path):
    path = _write(tmp_path)
    with pytest.raises(HybridManifestError):
        load_manifest_v1(path)
    document = _document(version=1)
    for field in ("exports", "dispatch_callback_limit", "dispatch_callback_semantic_limit"):
        del document[field]
    del document["routines"][0]["callbacks"]
    path.write_text(json.dumps(document))
    assert type(load_manifest(path)) is RoutineManifestV1
    assert load_manifest(path) == load_manifest_v1(path)
    with pytest.raises(HybridManifestError):
        load_manifest_v2(path)
    # Version alone never silently opts a v1 document into callback handling.
    document["version"] = 2
    path.write_text(json.dumps(document))
    with pytest.raises(HybridManifestError, match="missing fields"):
        load_manifest(path)
    with pytest.raises(HybridManifestError, match="ABI version"):
        load_manifest_v1(path)


@pytest.mark.parametrize("changes,message", [
    ({"abi": "other"}, "ABI identity"),
    ({"version": 1}, "ABI version"), ({"version": 3}, "ABI version"),
    ({"version": True}, "exact integer"), ({"version": 2.0}, "exact integer"),
    ({"dispatch_instruction_limit": 0}, "dispatch instructions"),
    ({"dispatch_callback_limit": 0}, "dispatch callback requests"),
    ({"dispatch_callback_limit": 1025}, "dispatch callback requests"),
    ({"dispatch_callback_limit": True}, "exact integer"),
    ({"dispatch_callback_semantic_limit": 0}, "dispatch callback semantic steps"),
    ({"dispatch_callback_semantic_limit": 65537}, "dispatch callback semantic steps"),
    ({"dispatch_callback_semantic_limit": 1.5}, "exact integer"),
    ({"exports": {}}, "exports must be an array"),
    ({"exports": [_export()] * 65}, "at most 64"),
    ({"routines": {}}, "routines must be an array"),
    ({"routines": [_routine(name=f"R{i}") for i in range(65)]}, "at most 64"),
    ({"effect": "integer_leaf"}, "unknown fields"),
])
def test_top_level_metadata_fails_before_image_reads(tmp_path, monkeypatch, changes, message):
    path = _write(tmp_path, _document(**changes))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v2(path)
    assert opened == [path]


@pytest.mark.parametrize("changes,message", [
    ({"export_id": True}, "exact integer"), ({"export_id": -1}, "export ID"),
    ({"export_id": 64}, "export ID"), ({"name": "min"}, "canonical integer leaf"),
    ({"name": "EXECUTE"}, "canonical integer leaf"),
    ({"name": "F64+"}, "canonical integer leaf"),
    ({"input_cells": 1}, "arity"), ({"output_cells": True}, "exact integer"),
    ({"output_cells": 2}, "arity"),
    ({"effect": "integer_leaf"}, "unknown fields"),
    ({"max_semantic_steps": 1}, "unknown fields"),
])
def test_export_metadata_fails_before_images(tmp_path, monkeypatch, changes, message):
    path = _write(tmp_path, _document(exports=[_export(**changes)]))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v2(path)
    assert opened == [path]


@pytest.mark.parametrize("other", [_export(), _export(name="MAX")])
def test_duplicate_ids_are_rejected_even_when_descriptors_are_equal(tmp_path, monkeypatch, other):
    path = _write(tmp_path, _document(exports=[_export(), other]))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="duplicate callback export ID"):
        load_manifest_v2(path)
    assert opened == [path]


def test_distinct_ids_may_describe_the_same_leaf_and_empty_tables_grant_nothing(tmp_path):
    path = _write(tmp_path, _document(exports=[_export(export_id=i) for i in range(64)]))
    assert len(load_manifest_v2(path).exports) == 64
    path.write_text(json.dumps(_document(exports=[], routines=[_routine(callbacks=[])])))
    value = load_manifest_v2(path)
    assert value.exports == () and value.routines[0].callbacks == ()
    path.write_text(json.dumps(_document(exports=[], routines=[])))
    assert load_manifest_v2(path).routines == ()


@pytest.mark.parametrize("changes,message", [
    ({"callbacks": {}}, "callbacks must be an array"),
    ({"callbacks": [_site()] * 17}, "at most 16"),
    ({"callbacks": [_site(export_id=1)]}, "undeclared callback export ID"),
    ({"callbacks": [_site(export_id=True)]}, "exact integer"),
    ({"callbacks": [_site(call_offset=True)]}, "exact integer"),
    ({"callbacks": [_site(call_offset=MAX_CODE_BYTES - 1)]}, "call offset"),
    ({"callbacks": [_site(stub_offset=MAX_CODE_BYTES)]}, "stub offset"),
    ({"callbacks": [_site(stub_offset=1)]}, "disjoint"),
    ({"callbacks": [_site(), _site()]}, "overlap or repeat"),
    ({"callbacks": [_site(), _site(call_offset=7, stub_offset=10)]}, "overlap or repeat"),
    ({"callbacks": [_site(), _site(call_offset=2, stub_offset=1)]}, "overlap or repeat"),
    ({"callbacks": [_site(export={})]}, "unknown fields"),
    ({"name": "BAD NAME"}, "ASCII"),
    ({"name": "h-callback"}, "duplicate routine name"),
    ({"image": "https://example.test/image.bin"}, "relative local path"),
    ({"image": "C:\\image.bin"}, "relative local path"),
    ({"image": "bad\ud800.bin"}, "filesystem encodable"),
    ({"entry_offset": MAX_CODE_BYTES}, "entry offset"),
    ({"input_cells": 9}, "input cells"),
    ({"return_stack_cells": True}, "exact integer"),
    ({"buffers": [dict(address_argument=2, length_argument=1, element_bytes=8,
                       max_bytes=64, access="read")]}, "outside the input signature"),
])
def test_later_routine_metadata_is_checked_before_any_image_open(tmp_path, monkeypatch, changes, message):
    second = _routine(name="SECOND")
    second.update(changes)
    path = _write(tmp_path, _document(routines=[_routine(), second]))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v2(path)
    assert opened == [path]


@pytest.mark.parametrize("scope", ["manifest", "export", "routine", "callback"])
@pytest.mark.parametrize("mutation", ["duplicate", "missing", "unknown"])
def test_json_object_fields_are_exact_at_every_v2_level(tmp_path, monkeypatch, scope, mutation):
    document = _document()
    row, key = {
        "manifest": (document, "version"),
        "export": (document["exports"][0], "name"),
        "routine": (document["routines"][0], "name"),
        "callback": (document["routines"][0]["callbacks"][0], "call_offset"),
    }[scope]
    if mutation == "missing":
        del row[key]
    elif mutation == "unknown":
        row["extra"] = 0
    text = json.dumps(document)
    if mutation == "duplicate":
        fragment = json.dumps(key) + ": " + json.dumps(row[key])
        text = text.replace(fragment, fragment + ", " + fragment, 1)
    path = tmp_path / "manifest.json"
    path.write_text(text)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="duplicate JSON field|missing fields|unknown fields"):
        load_manifest_v2(path)
    assert opened == [path]


@pytest.mark.parametrize("payload", [
    b"{", b"\xff", b"[]", b"{}", b'{"version": NaN}', b'{"version": Infinity}',
    b'{"version": true}', b'{"version": 2.0}', b'{"version": 3}', b"[" * 2000,
])
def test_dispatch_rejects_invalid_json_versions_and_nonfinite_values(tmp_path, payload):
    path = tmp_path / "manifest.json"
    path.write_bytes(payload)
    with pytest.raises(HybridManifestError):
        load_manifest(path)


@pytest.mark.parametrize("site", [_site(call_offset=31), _site(stub_offset=32)])
def test_complete_callback_spans_must_fit_actual_image_after_bounded_read(tmp_path, site):
    path = _write(tmp_path, _document(routines=[_routine(callbacks=[site])]))
    with pytest.raises(HybridManifestError, match="inside the code image"):
        load_manifest_v2(path)


def test_callback_spans_can_touch_actual_image_edges(tmp_path):
    path = _write(tmp_path, _document(routines=[_routine(callbacks=[
        _site(call_offset=30, stub_offset=29),
    ])]))
    site, = load_manifest_v2(path).routines[0].callbacks
    assert (site.call_offset, site.stub_offset) == (30, 29)


@pytest.mark.parametrize("code", [b"", b"x" * (MAX_CODE_BYTES + 1)])
def test_v2_empty_or_oversized_images_fail(tmp_path, code):
    path = _write(tmp_path, code=code)
    with pytest.raises(HybridManifestError, match="code image bytes|exceeds"):
        load_manifest_v2(path)


def test_manifest_limit_and_nonregular_image_rejections_are_retained(tmp_path):
    path = tmp_path / "manifest.json"
    path.write_bytes(b" " * (MAX_MANIFEST_BYTES + 1))
    with pytest.raises(HybridManifestError, match="manifest exceeds"):
        load_manifest(path)
    path = _write(tmp_path, _document(routines=[_routine(image="directory")]))
    (tmp_path / "directory").mkdir()
    with pytest.raises(HybridManifestError, match="regular local file"):
        load_manifest_v2(path)
    if hasattr(os, "mkfifo") and hasattr(os, "O_NONBLOCK"):
        os.mkfifo(tmp_path / "fifo")
        path.write_text(json.dumps(_document(routines=[_routine(image="fifo")])))
        with pytest.raises(HybridManifestError, match="regular local file"):
            load_manifest_v2(path)


def test_padding_counts_toward_v2_aggregate_before_returning_values(tmp_path, monkeypatch):
    import shared.hybrid_abi as abi_module

    monkeypatch.setattr(manifest_module, "MAX_TOTAL_CODE_BYTES", 32)
    monkeypatch.setattr(abi_module, "MAX_TOTAL_CODE_BYTES", 32)
    path = _write(tmp_path, _document(routines=[
        _routine(name=f"R{i}", callbacks=[]) for i in range(3)
    ]), code=b"\x01")
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="total code image bytes"):
        load_manifest_v2(path)
    # Three bytes of source still need three separate 16-byte code allocations.
    assert opened == [path, tmp_path / "routine.bin", tmp_path / "routine.bin"]


def test_later_missing_image_returns_no_partial_manifest(tmp_path):
    path = _write(tmp_path, _document(routines=[_routine(), _routine(name="SECOND", image="missing")]))
    with pytest.raises(HybridManifestError, match="cannot read routine image"):
        load_manifest_v2(path)


def test_v2_value_rejects_mutable_tables_v1_images_and_export_conflicts(tmp_path):
    value = load_manifest_v2(_write(tmp_path))
    for field in ("routines", "exports"):
        with pytest.raises(TypeError, match="immutable tuple"):
            replace(value, **{field: list(getattr(value, field))})
    with pytest.raises(TypeError, match="RoutineImageV2"):
        replace(value, routines=(RoutineImageV1(
            name="V1", code=b"\x01", entry_offset=0, input_cells=0, output_cells=0,
            buffers=(), max_instructions=1, return_stack_cells=1,
        ),))
    with pytest.raises(ValueError, match="duplicate callback export ID"):
        replace(value, exports=value.exports * 2)
    with pytest.raises(ValueError, match="undeclared callback export ID"):
        replace(value, exports=())
    with pytest.raises(ValueError, match="conflicting descriptors"):
        replace(value, exports=(replace(value.exports[0], name="MAX"),))
    with pytest.raises(ValueError, match="duplicate routine name"):
        replace(value, routines=value.routines * 2)
    # Equal descriptor copies remain values, not invocation handles.
    assert replace(value, exports=(replace(value.exports[0]),)) == value


@pytest.mark.parametrize("field,value", [
    ("version", True), ("version", 1), ("version", 3),
    ("dispatch_instruction_limit", 0), ("dispatch_instruction_limit", True),
    ("dispatch_callback_limit", 0), ("dispatch_callback_limit", 1025),
    ("dispatch_callback_limit", True), ("dispatch_callback_semantic_limit", 65537),
    ("dispatch_callback_semantic_limit", True),
])
def test_v2_value_has_strict_finite_dispatch_limits(tmp_path, field, value):
    original = load_manifest_v2(_write(tmp_path))
    with pytest.raises((TypeError, ValueError)):
        replace(original, **{field: value})


def test_v2_value_limits_total_padded_code_and_exact_export_types(tmp_path):
    value = load_manifest_v2(_write(tmp_path))
    with pytest.raises(TypeError, match="CallbackExportV2"):
        replace(value, exports=(object(),))
    with pytest.raises(ValueError, match="at most 64 callback exports"):
        replace(value, exports=value.exports * 65)
    code = b"\x01" * (MAX_CODE_BYTES - 15)
    routines = tuple(replace(value.routines[0], name=f"R{i}", code=code)
                     for i in range(MAX_TOTAL_CODE_BYTES // MAX_CODE_BYTES + 1))
    with pytest.raises(ValueError, match="total code image bytes"):
        replace(value, routines=routines)


def test_manifest_loading_imports_no_engine_or_native_extension(tmp_path):
    path = _write(tmp_path)
    script = """
import sys
from hybrid.manifest import load_manifest
value = load_manifest(sys.argv[1])
assert value.version == 2
assert not any(name == 'simulator' or name.startswith('simulator.')
               or name == 'emulator' or name.startswith('emulator.')
               or name in ('_mp64_accel', '_megaforth_native') for name in sys.modules)
"""
    completed = subprocess.run([sys.executable, "-c", script, str(path)],
                               cwd=Path(__file__).resolve().parents[1],
                               capture_output=True, text=True, timeout=10)
    assert completed.returncode == 0, completed.stderr
