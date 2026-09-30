"""The explicit V5 loader validates all independent metadata before image I/O."""

from __future__ import annotations

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
from hybrid.service_manifest import load_service_manifest_v5
from shared.hybrid_abi import HYBRID_ABI, MAX_CODE_BYTES, MAX_MANIFEST_BYTES, MAX_TOTAL_CODE_BYTES
from shared.hybrid_services import (
    ServiceExportV5, CallbackSiteV5, RoutineImageV5, RoutineManifestV5,
)


def _export(**changes):
    return dict(export_id=0, name="F64+", input_cells=2, output_cells=1,
                effect="scalar_fp_state", max_semantic_steps=1) | changes


def _site(**changes):
    return dict(call_offset=0, stub_offset=8, export_id=0) | changes


def _buffer(**changes):
    return dict(address_argument=0, length_argument=1, element_bytes=8,
                max_bytes=64, access="read") | changes


def _routine(**changes):
    return dict(name="H-FP", image="routine.bin", entry_offset=0, input_cells=2,
                output_cells=1, buffers=[], max_instructions=100, max_callback_requests=1,
                return_stack_cells=16, callbacks=[_site()]) | changes


def _document(**changes):
    return dict(abi=HYBRID_ABI, version=5, dispatch_instruction_limit=1000,
                dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
                exports=[_export()], routines=[_routine()]) | changes


def _write(tmp_path, document=None, *, code=b"\x01" * 32):
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


def _reject_before_images(tmp_path, monkeypatch, document):
    path = _write(tmp_path, document)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError):
        load_service_manifest_v5(path)
    assert opened == [path]


def test_v5_resolves_shared_export_values_and_owns_immutable_relative_image_bytes(tmp_path, monkeypatch):
    directory = tmp_path / "nested"
    directory.mkdir()
    path = _write(directory, _document(routines=[
        _routine(callbacks=[_site(), _site(call_offset=2, stub_offset=9)], max_callback_requests=2),
        _routine(name="SECOND", max_callback_requests=0),
    ]))
    monkeypatch.chdir(tmp_path)
    value = load_service_manifest_v5(path)
    assert type(value) is RoutineManifestV5 and value.version == 5
    export, = value.exports
    assert type(export) is ServiceExportV5 and export.name == "F64+"
    assert [routine.max_callback_requests for routine in value.routines] == [2, 0]
    for routine in value.routines:
        assert type(routine) is RoutineImageV5
        assert routine.code == b"\x01" * 32
        for site in routine.callbacks:
            assert type(site) is CallbackSiteV5 and site.export is export
    with pytest.raises(FrozenInstanceError):
        value.routines[0].max_callback_requests = 100
    (directory / "routine.bin").write_bytes(b"changed")
    assert value.routines[0].code == b"\x01" * 32


def test_every_admitted_service_has_exact_manifest_arity(tmp_path):
    signatures = [("FPCSR@", 0, 1), ("FPCSR!", 1, 0)] + [
        (prefix + suffix, inputs, 1)
        for prefix in ("F32", "F64")
        for suffix, inputs in (("+", 2), ("-", 2), ("*", 2), ("/", 2), ("SQRT", 1), ("FMA", 3))
    ]
    exports = [_export(export_id=i, name=name, input_cells=inputs, output_cells=outputs)
               for i, (name, inputs, outputs) in enumerate(signatures)]
    sites = [_site(call_offset=i * 3, stub_offset=i * 3 + 2, export_id=i) for i in range(14)]
    value = load_service_manifest_v5(_write(tmp_path, _document(
        exports=exports, routines=[_routine(callbacks=sites, max_callback_requests=1024)],
    ), code=b"\x01" * 64))
    assert [(item.name, item.input_cells, item.output_cells) for item in value.exports] == signatures
    assert all(site.export is value.exports[i] for i, site in enumerate(value.routines[0].callbacks))


def test_generic_and_explicit_loaders_read_once_and_preserve_old_schemas(tmp_path, monkeypatch):
    path = _write(tmp_path)
    opened = _record_opens(monkeypatch)
    assert type(load_service_manifest_v5(path)) is RoutineManifestV5
    assert opened == [path, tmp_path / "routine.bin"]
    opened.clear()
    assert type(load_manifest(path)) is RoutineManifestV5
    assert opened == [path, tmp_path / "routine.bin"]
    for loader in (load_manifest_v1, load_manifest_v2, load_manifest_v3):
        opened.clear()
        with pytest.raises(HybridManifestError):
            loader(path)
        assert opened == [path]


@pytest.mark.parametrize("changes", (
    dict(abi="other"), dict(version=1), dict(version=2), dict(version=3), dict(version=4),
    dict(version=6), dict(version=True), dict(version=5.0),
    dict(dispatch_instruction_limit=0), dict(dispatch_instruction_limit=10_000_001),
    dict(dispatch_callback_limit=0), dict(dispatch_callback_limit=1025),
    dict(dispatch_callback_semantic_limit=0), dict(dispatch_callback_semantic_limit=65537),
    dict(dispatch_callback_limit=True), dict(exports={}), dict(routines={}),
    dict(exports=[_export(export_id=i % 64) for i in range(65)]),
    dict(routines=[_routine(name=f"R{i}") for i in range(65)]),
    dict(source_prelude="0 FPCSR!"), dict(policies=[]), dict(include="service.f"),
))
def test_manifest_bounds_and_unimplemented_profiles_reject_before_images(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(**changes))


@pytest.mark.parametrize("changes", (
    dict(export_id=True), dict(export_id=-1), dict(export_id=64),
    dict(name="MIN"), dict(name="F64MIN"), dict(name="f64+"), dict(name="FAULT-XT!"),
    dict(name="F64FMA"), dict(name="FPCSR@"), dict(input_cells=True),
    dict(output_cells=0), dict(max_semantic_steps=0), dict(max_semantic_steps=2),
    dict(effect="integer_leaf"), dict(effect="closed_integer_colon"),
    dict(effect="task_scalar_fp_state"), dict(max_input_bytes=1), dict(can_suspend=True),
    dict(fault_handler=0), dict(policy_id=0),
))
def test_later_unused_exports_are_validated_before_any_image(tmp_path, monkeypatch, changes):
    later = _export(export_id=1) | changes
    _reject_before_images(tmp_path, monkeypatch, _document(exports=[_export(), later]))


def test_duplicate_export_ids_reject_even_with_identical_descriptors(tmp_path, monkeypatch):
    _reject_before_images(tmp_path, monkeypatch, _document(exports=[_export(), _export()]))


@pytest.mark.parametrize("changes", (
    dict(name="h-fp"), dict(name="BAD NAME"), dict(name="é"), dict(name="X" * 128),
    dict(image="/absolute.bin"), dict(image="C:\\image.bin"),
    dict(image="https://example.test/image.bin"), dict(image=""), dict(image="bad\ud800.bin"),
    dict(entry_offset=MAX_CODE_BYTES), dict(input_cells=9), dict(output_cells=True),
    dict(max_instructions=0), dict(max_instructions=1_000_001),
    dict(max_callback_requests=-1), dict(max_callback_requests=1025),
    dict(max_callback_requests=True), dict(return_stack_cells=0), dict(return_stack_cells=8193),
    dict(buffers=[_buffer(address_argument=2)]), dict(buffers=[_buffer()] * 17),
    dict(callbacks=[_site(export_id=63)]), dict(callbacks=[_site(call_offset=True)]),
    dict(callbacks=[_site(call_offset=0, stub_offset=1)]),
    dict(callbacks=[_site(), _site(call_offset=7, stub_offset=10)]),
    dict(callbacks=[_site()] * 17), dict(callbacks={}),
))
def test_later_routine_metadata_rejects_before_first_image(tmp_path, monkeypatch, changes):
    _reject_before_images(tmp_path, monkeypatch, _document(routines=[
        _routine(), _routine(name="SECOND") | changes,
    ]))


@pytest.mark.parametrize("scope", ("manifest", "export", "routine", "buffer", "callback"))
@pytest.mark.parametrize("mutation", ("missing", "unknown", "duplicate"))
def test_every_json_object_level_has_exact_fields(tmp_path, monkeypatch, scope, mutation):
    document = _document(routines=[_routine(buffers=[_buffer()])])
    row, key = {
        "manifest": (document, "version"),
        "export": (document["exports"][0], "effect"),
        "routine": (document["routines"][0], "max_callback_requests"),
        "buffer": (document["routines"][0]["buffers"][0], "access"),
        "callback": (document["routines"][0]["callbacks"][0], "call_offset"),
    }[scope]
    if mutation == "missing":
        del row[key]
    elif mutation == "unknown":
        row["extra"] = 1
    payload = json.dumps(document)
    if mutation == "duplicate":
        fragment = json.dumps(key) + ": " + json.dumps(row[key])
        payload = payload.replace(fragment, fragment + ", " + fragment, 1)
    path = tmp_path / "manifest.json"
    path.write_text(payload)
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="missing fields|unknown fields|duplicate JSON field"):
        load_service_manifest_v5(path)
    assert opened == [path]


@pytest.mark.parametrize("payload", (b"{", b"\xff", b"[]", b"{}", b"[" * 2000,
                                      b'{"version": NaN}', b'{"version": Infinity}'))
def test_invalid_nonfinite_or_over_nested_json_fails_cleanly(tmp_path, payload):
    path = tmp_path / "manifest.json"
    path.write_bytes(payload)
    with pytest.raises(HybridManifestError):
        load_service_manifest_v5(path)


@pytest.mark.parametrize("code", (b"", b"x" * (MAX_CODE_BYTES + 1)))
def test_empty_and_oversized_images_are_bounded(tmp_path, code):
    with pytest.raises(HybridManifestError):
        load_service_manifest_v5(_write(tmp_path, code=code))


@pytest.mark.parametrize("changes", (
    dict(entry_offset=32), dict(callbacks=[_site(call_offset=31)]),
    dict(callbacks=[_site(stub_offset=32)]),
))
def test_actual_image_size_checks_follow_its_bounded_read(tmp_path, monkeypatch, changes):
    path = _write(tmp_path, _document(routines=[_routine(**changes)]))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError):
        load_service_manifest_v5(path)
    assert opened == [path, tmp_path / "routine.bin"]


def test_image_edge_sites_and_manifest_relative_parent_paths_remain_valid(tmp_path, monkeypatch):
    directory = tmp_path / "manifest-dir"
    directory.mkdir()
    code = b"\x01" * 32
    (tmp_path / "café.bin").write_bytes(code)
    path = _write(directory, _document(routines=[_routine(image="../café.bin", callbacks=[
        _site(call_offset=30, stub_offset=29),
    ])]))
    monkeypatch.chdir(directory)
    routine, = load_service_manifest_v5(path).routines
    assert routine.code == code and routine.callbacks[0].call_offset == 30


def test_regular_file_and_manifest_size_limits_are_enforced(tmp_path):
    path = tmp_path / "manifest.json"
    path.write_bytes(b" " * (MAX_MANIFEST_BYTES + 1))
    with pytest.raises(HybridManifestError, match="manifest exceeds"):
        load_service_manifest_v5(path)
    directory = tmp_path / "directory"
    directory.mkdir()
    path = _write(tmp_path, _document(routines=[_routine(image="directory")]))
    with pytest.raises(HybridManifestError, match="regular local file"):
        load_service_manifest_v5(path)
    if hasattr(os, "mkfifo") and hasattr(os, "O_NONBLOCK"):
        os.mkfifo(tmp_path / "fifo")
        path.write_text(json.dumps(_document(routines=[_routine(image="fifo")])))
        with pytest.raises(HybridManifestError, match="regular local file"):
            load_service_manifest_v5(path)


def test_aggregate_counts_per_image_padding_before_returning_any_manifest(tmp_path, monkeypatch):
    count = MAX_TOTAL_CODE_BYTES // MAX_CODE_BYTES
    assert count == 16
    code = b"\x01" * (MAX_CODE_BYTES - 1)
    rows = [_routine(name=f"R{i}", callbacks=[]) for i in range(count)]
    path = _write(tmp_path, _document(routines=rows), code=code)
    value = load_service_manifest_v5(path)
    assert len(value.routines) == count
    assert sum(len(routine.code) for routine in value.routines) == MAX_TOTAL_CODE_BYTES - count
    # Raw source bytes of this extra one-byte image would still fit, but its
    # distinct padded allocation cannot fit the already-full 16 MiB allowance.
    (tmp_path / "small.bin").write_bytes(b"\x01")
    rows.append(_routine(name="EXTRA", image="small.bin", callbacks=[]))
    path.write_text(json.dumps(_document(routines=rows)))
    opened = _record_opens(monkeypatch)
    with pytest.raises(HybridManifestError, match="total code image bytes"):
        load_service_manifest_v5(path)
    assert opened == [path] + [tmp_path / "routine.bin"] * count


def test_later_missing_image_returns_no_partial_manifest(tmp_path):
    path = _write(tmp_path, _document(routines=[_routine(), _routine(name="SECOND", image="missing")]))
    with pytest.raises(HybridManifestError, match="cannot read routine image"):
        load_service_manifest_v5(path)


def test_explicit_service_loader_is_cold_with_backends_and_runtime_blocked(tmp_path):
    path = _write(tmp_path)
    code = r'''
import importlib.abc
import sys
class Guard(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in ('emulator', 'simulator', '_mp64_accel', '_megaforth_native') or fullname == 'hybrid.runtime':
            raise AssertionError('forbidden backend import: ' + fullname)
sys.meta_path.insert(0, Guard())
from hybrid.service_manifest import load_service_manifest_v5
value = load_service_manifest_v5(sys.argv[1])
assert value.version == 5 and value.exports[0].name == 'F64+'
assert not hasattr(value, 'runtime') and not hasattr(value.exports[0], 'invoke')
'''
    result = subprocess.run([sys.executable, "-B", "-c", code, str(path)],
        cwd=Path(__file__).resolve().parents[1], capture_output=True, text=True, timeout=10)
    assert result.returncode == 0, result.stderr
