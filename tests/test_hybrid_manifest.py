"""Hybrid declaration admission is bounded and never imports an engine."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest

from hybrid.manifest import HybridManifestError, load_manifest_v1
import hybrid.manifest as manifest_module
from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI,
    MAX_CODE_BYTES,
    MAX_MANIFEST_BYTES,
    MAX_TOTAL_CODE_BYTES,
    BufferAccessV1,
    BufferRuleV1,
    BufferSpanV1,
    MachineExitKindV1,
    MachineRoutineResultV1,
    RoutineDeclarationV1,
    RoutineImageV1,
    RoutineManifestV1,
)


def _rule(**changes):
    fields = dict(address_argument=0, length_argument=1, element_bytes=8,
                  max_bytes=65536, access="read")
    fields.update(changes)
    return BufferRuleV1(**fields)


def _routine(**changes):
    fields = dict(name="HYB-CHECKSUM", image="checksum.bin", entry_offset=0,
                  input_cells=2, output_cells=1, buffers=[dict(
                      address_argument=0, length_argument=1, element_bytes=8,
                      max_bytes=65536, access="read",
                  )], max_instructions=65536, return_stack_cells=128)
    fields.update(changes)
    return fields


def _document(*routines, **changes):
    fields = dict(abi=HYBRID_ABI, version=1, dispatch_instruction_limit=1_000_000,
                  routines=list(routines or (_routine(),)))
    fields.update(changes)
    return fields


def _write(tmp_path, document=None, *, code=b"\x00\x01\x02\x03"):
    (tmp_path / "checksum.bin").write_bytes(code)
    path = tmp_path / "routines.json"
    path.write_text(json.dumps(_document() if document is None else document),
                    encoding="utf-8")
    return path


def _declaration(**changes):
    fields = dict(name="HYB-CHECKSUM", session_nonce=object(), registration_nonce=object(),
                  allocation_lease=object(), allocation_generation=1, control_lease=object(),
                  control_generation=1, body_base=0x1000, body_size=64, code_base=0x1010,
                  code=bytes(32), entry_offset=0, input_cells=2, output_cells=1,
                  buffers=(_rule(),), stack_base=0x2000, return_stack_cells=128,
                  max_instructions=65536, dispatch_instruction_limit=1_000_000)
    fields.update(changes)
    return RoutineDeclarationV1(**fields)


def test_manifest_loads_relative_images_into_immutable_unpublished_values(tmp_path, monkeypatch):
    directory = tmp_path / "manifest"
    directory.mkdir()
    path = _write(directory)
    monkeypatch.chdir(tmp_path)
    value = load_manifest_v1(path)
    assert type(value) is RoutineManifestV1
    assert value.abi == HYBRID_ABI and value.version == 1
    assert value.dispatch_instruction_limit == 1_000_000
    routine, = value.routines
    assert type(routine) is RoutineImageV1
    assert routine.name == "HYB-CHECKSUM"
    assert routine.code == b"\x00\x01\x02\x03"
    assert routine.buffers == (_rule(),)
    with pytest.raises(FrozenInstanceError):
        routine.code = b"changed"
    # The value owns immutable bytes and survives subsequent file changes.
    (directory / "checksum.bin").write_bytes(b"other image")
    assert routine.code == b"\x00\x01\x02\x03"


@pytest.mark.parametrize(("fields", "message"), [
    ({"abi": "other"}, "ABI identity"),
    ({"version": 2}, "ABI version"),
    ({"version": True}, "exact integer"),
    ({"dispatch_instruction_limit": 0}, "dispatch instructions"),
    ({"dispatch_instruction_limit": 10_000_001}, "dispatch instructions"),
    ({"dispatch_instruction_limit": 1.0}, "exact integer"),
    ({"routines": {}}, "routines must be an array"),
    ({"routines": [_routine(name=f"WORD{i}") for i in range(65)]}, "at most 64"),
    ({"callback": "host.call"}, "unknown fields"),
])
def test_manifest_rejects_unsupported_schema_before_loading_images(tmp_path, fields, message):
    path = _write(tmp_path, _document(**fields))
    (tmp_path / "checksum.bin").unlink()
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v1(path)


@pytest.mark.parametrize(("fields", "message"), [
    ({"name": ""}, "ASCII"), ({"name": "é"}, "ASCII"),
    ({"name": "X" * 128}, "ASCII"),
    ({"entry_offset": -1}, "entry offset"),
    ({"input_cells": 9}, "input cells"),
    ({"output_cells": True}, "exact integer"),
    ({"max_instructions": 0}, "per-call instructions"),
    ({"max_instructions": 1_000_001}, "per-call instructions"),
    ({"return_stack_cells": 0}, "return stack cells"),
    ({"return_stack_cells": 8193}, "return stack cells"),
    ({"buffers": [_routine()["buffers"][0]] * 17}, "at most 16"),
    ({"callback": "host.call"}, "unknown fields"),
    ({"image": "/absolute.bin"}, "relative local path"),
    ({"image": "https://example.test/code.bin"}, "relative local path"),
    ({"image": "C:\\code.bin"}, "relative local path"),
    ({"image": ""}, "relative local path"),
])
def test_routine_bounds_and_unknown_fields_fail_before_image_reads(tmp_path, fields, message):
    path = _write(tmp_path, _document(_routine(**fields)))
    (tmp_path / "checksum.bin").unlink()
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v1(path)


@pytest.mark.parametrize("name", [" ", "TWO WORDS", "TAB\tWORD", "LINE\nWORD", "NUL\x00", "\x1f", "\x7f"])
def test_routine_value_names_reject_whitespace_and_controls(name):
    with pytest.raises(ValueError, match="printable nonwhitespace ASCII"):
        _declaration(name=name)
    with pytest.raises(ValueError, match="printable nonwhitespace ASCII"):
        RoutineImageV1(name=name, code=b"\x00", entry_offset=0, input_cells=0,
                       output_cells=0, buffers=(), max_instructions=1, return_stack_cells=1)


@pytest.mark.parametrize(("fields", "message"), [
    ({"name": " "}, "printable nonwhitespace ASCII"),
    ({"name": "TWO WORDS"}, "printable nonwhitespace ASCII"),
    ({"name": "TAB\tWORD"}, "printable nonwhitespace ASCII"),
    ({"name": "LINE\nWORD"}, "printable nonwhitespace ASCII"),
    ({"name": "NUL\x00"}, "printable nonwhitespace ASCII"),
    ({"name": "\x1f"}, "printable nonwhitespace ASCII"),
    ({"name": "\x7f"}, "printable nonwhitespace ASCII"),
    ({"name": "SECOND", "image": "bad\ud800.bin"}, "filesystem encodable"),
])
def test_later_invalid_text_metadata_fails_before_any_image_open(
    tmp_path, monkeypatch, fields, message,
):
    path = _write(tmp_path, _document(_routine(), _routine(**fields)))
    real_open = manifest_module.os.open
    opened = []

    def record_open(file, *args, **kwargs):
        opened.append(Path(file))
        return real_open(file, *args, **kwargs)

    monkeypatch.setattr(manifest_module.os, "open", record_open)
    with pytest.raises(HybridManifestError, match=message):
        load_manifest_v1(path)
    assert opened == [path]


@pytest.mark.parametrize("name", ["!~", "X" * 127])
def test_printable_name_boundaries_and_encodable_unicode_image_paths_remain_valid(tmp_path, name):
    path = _write(tmp_path, _document(_routine(name=name, image="café.bin")))
    (tmp_path / "checksum.bin").rename(tmp_path / "café.bin")
    routine, = load_manifest_v1(path).routines
    assert routine.name == name
    assert routine.code == b"\x00\x01\x02\x03"
    assert _declaration(name=name).name == name


@pytest.mark.parametrize("scope", ["manifest", "routine", "buffer"])
def test_duplicate_json_keys_are_rejected_at_every_level(tmp_path, scope):
    text = json.dumps(_document())
    if scope == "manifest":
        text = text.replace('"version": 1', '"version": 1, "version": 1')
    elif scope == "routine":
        text = text.replace('"name": "HYB-CHECKSUM"',
                            '"name": "HYB-CHECKSUM", "name": "OTHER"')
    else:
        text = text.replace('"access": "read"', '"access": "read", "access": "write"')
    path = tmp_path / "routines.json"
    path.write_text(text)
    with pytest.raises(HybridManifestError, match="duplicate JSON field"):
        load_manifest_v1(path)


@pytest.mark.parametrize("scope", ["manifest", "routine", "buffer"])
def test_missing_fields_are_rejected(tmp_path, scope):
    value = _document()
    if scope == "manifest":
        del value["version"]
    elif scope == "routine":
        del value["routines"][0]["entry_offset"]
    else:
        del value["routines"][0]["buffers"][0]["max_bytes"]
    path = _write(tmp_path, value)
    with pytest.raises(HybridManifestError, match="missing fields"):
        load_manifest_v1(path)


@pytest.mark.parametrize("payload", [b"{", b"\xff", b"[]", b'{"version": NaN}',
                                      b'{"version": Infinity}', b"[" * 2000])
def test_invalid_nonfinite_or_excessively_nested_json_fails_cleanly(tmp_path, payload):
    path = tmp_path / "routines.json"
    path.write_bytes(payload)
    with pytest.raises(HybridManifestError):
        load_manifest_v1(path)


def test_case_insensitive_duplicate_names_fail_before_any_image_read(tmp_path):
    path = _write(tmp_path, _document(_routine(name="Word"), _routine(name="WORD")))
    (tmp_path / "checksum.bin").unlink()
    with pytest.raises(HybridManifestError, match="duplicate routine name"):
        load_manifest_v1(path)


@pytest.mark.parametrize("code", [b"", b"x" * (MAX_CODE_BYTES + 1)])
def test_empty_and_oversize_code_images_are_rejected(tmp_path, code):
    path = _write(tmp_path, code=code)
    with pytest.raises(HybridManifestError, match="code image bytes|exceeds"):
        load_manifest_v1(path)


def test_entry_must_lie_inside_actual_image(tmp_path):
    path = _write(tmp_path, _document(_routine(entry_offset=4)))
    with pytest.raises(HybridManifestError, match="entry offset"):
        load_manifest_v1(path)


def test_oversize_manifest_is_rejected_before_decoding(tmp_path):
    path = tmp_path / "routines.json"
    path.write_bytes(b" " * (MAX_MANIFEST_BYTES + 1))
    with pytest.raises(HybridManifestError, match="manifest exceeds"):
        load_manifest_v1(path)


def test_total_code_cap_is_enforced_before_returning_any_declarations(tmp_path):
    count = MAX_TOTAL_CODE_BYTES // MAX_CODE_BYTES + 1
    path = _write(tmp_path, _document(*(_routine(name=f"R{i}") for i in range(count))),
                  code=b"x" * MAX_CODE_BYTES)
    with pytest.raises(HybridManifestError, match="total code image bytes"):
        load_manifest_v1(path)


def test_later_invalid_image_returns_no_partial_manifest(tmp_path):
    path = _write(tmp_path, _document(_routine(), _routine(name="SECOND", image="missing.bin")))
    with pytest.raises(HybridManifestError, match="cannot read routine image"):
        load_manifest_v1(path)


def test_directory_and_fifo_images_are_rejected_without_waiting(tmp_path):
    path = _write(tmp_path, _document(_routine(image="directory")))
    (tmp_path / "directory").mkdir()
    with pytest.raises(HybridManifestError, match="regular local file"):
        load_manifest_v1(path)
    if hasattr(os, "mkfifo") and hasattr(os, "O_NONBLOCK"):
        os.mkfifo(tmp_path / "fifo")
        path.write_text(json.dumps(_document(_routine(image="fifo"))))
        with pytest.raises(HybridManifestError, match="regular local file"):
            load_manifest_v1(path)


def test_all_file_reads_are_bounded_even_if_file_size_check_is_stale(tmp_path, monkeypatch):
    path = _write(tmp_path)
    real_fdopen = manifest_module.os.fdopen
    counts = []

    class CheckedReader:
        def __init__(self, stream):
            self.stream = stream

        def __enter__(self):
            self.stream.__enter__()
            return self

        def __exit__(self, *args):
            return self.stream.__exit__(*args)

        def read(self, size):
            assert 0 < size <= MAX_CODE_BYTES + 1
            counts.append(size)
            return self.stream.read(size)

    monkeypatch.setattr(manifest_module.os, "fdopen",
                        lambda *args, **kwargs: CheckedReader(real_fdopen(*args, **kwargs)))
    load_manifest_v1(path)
    assert counts == [MAX_MANIFEST_BYTES + 1, MAX_CODE_BYTES + 1]

    # An inaccurate pre-read size cannot admit an image larger than its cap.
    original_fstat = manifest_module.os.fstat
    monkeypatch.setattr(manifest_module.os, "fstat", lambda fd: type("FileInfo", (), {
        "st_mode": original_fstat(fd).st_mode, "st_size": 0,
    })())
    (tmp_path / "checksum.bin").write_bytes(b"x" * (MAX_CODE_BYTES + 1))
    with pytest.raises(HybridManifestError, match="routine image exceeds"):
        load_manifest_v1(path)


@pytest.mark.parametrize("field", ["address_argument", "length_argument", "element_bytes", "max_bytes"])
def test_buffer_rule_rejects_booleans_for_every_integer_field(field):
    with pytest.raises(TypeError, match="exact integer"):
        _rule(**{field: True})


def test_buffer_rules_preserve_permissions_aliases_and_empty_uint64_pointer():
    span = _rule(access="read_write").resolve((0x100000, 3))
    assert span == BufferSpanV1(base=0x100000, size=24, access=BufferAccessV1.READ_WRITE)
    assert span.limit == 0x100018
    assert _rule(element_bytes=MASK64, max_bytes=0).resolve((MASK64, 0)).size == 0
    assert _rule(element_bytes=1, max_bytes=1).resolve((MASK64, 1)).limit == MASK64 + 1


@pytest.mark.parametrize(("rule", "arguments", "message"), [
    (_rule(), (0x100000, 8193), "maximum buffer bytes"),
    (_rule(element_bytes=MASK64, max_bytes=MASK64), (0, 2), "multiplication overflows"),
    (_rule(element_bytes=8), (MASK64, 1), "wraps"),
    (_rule(), (0,), "outside the input signature"),
    (_rule(), (False, 0), "exact integer"),
    (_rule(), (0, -1), "argument 1"),
])
def test_buffer_resolution_rejects_invalid_cells_lengths_and_spans(rule, arguments, message):
    with pytest.raises((TypeError, ValueError), match=message):
        rule.resolve(arguments)


@pytest.mark.parametrize(("changes", "message"), [
    ({"input_cells": 1}, "outside the input signature"),
    ({"code": bytearray(32)}, "immutable bytes"),
    ({"buffers": [_rule()]}, "immutable tuple"),
    ({"allocation_generation": True}, "exact integer"),
    ({"allocation_generation": 0}, "allocation generation"),
    ({"control_generation": 0}, "control generation"),
    ({"session_nonce": None}, "identify its owner"),
    ({"code_base": 0x1011}, "aligned to 16"),
    ({"code": bytes(17)}, "aligned to 16"),
    ({"body_size": 32}, "complete body allocation"),
    ({"stack_base": 0x1010}, "disjoint"),
    ({"stack_base": 0x2001}, "cell aligned"),
    ({"stack_base": MASK64 - 7}, "wraps"),
    ({"return_stack_cells": 8193}, "return stack cells"),
    ({"max_instructions": 1_000_001}, "per-call instructions"),
    ({"dispatch_instruction_limit": 10_000_001}, "dispatch instructions"),
])
def test_declaration_rejects_invalid_geometry_generations_and_bounds(changes, message):
    with pytest.raises((TypeError, ValueError), match=message):
        _declaration(**changes)


def test_declaration_records_full_body_and_retains_exact_opaque_identities():
    value = _declaration()
    assert value.entry_pc == 0x1010
    assert value.code_size == 32 and value.stack_size == 1024
    assert value.body_base == 0x1000 and value.body_size == 64
    updated = replace(value, output_cells=8)
    assert updated.allocation_lease is value.allocation_lease
    assert updated.registration_nonce is value.registration_nonce
    with pytest.raises(FrozenInstanceError):
        value.allocation_generation = 2


def test_results_are_immutable_separate_counters_with_no_failure_outputs():
    result = MachineRoutineResultV1(exit_kind="returned", instructions=3, cycles=7,
                                    entry_pc=0x1010, pc=0xFFFF, outputs=(MASK64,))
    assert result.exit_kind is MachineExitKindV1.RETURNED
    assert result.instructions == 3 and result.cycles == 7
    assert result.trap_id == -1 and result.detail == ""
    with pytest.raises(FrozenInstanceError):
        result.instructions = 4
    with pytest.raises(ValueError, match="cannot publish outputs"):
        replace(result, exit_kind="rejected_access")
    with pytest.raises(TypeError, match="exact integer"):
        replace(result, instructions=True)
    with pytest.raises(TypeError, match="immutable tuple"):
        replace(result, outputs=[1])
    failure = replace(result, exit_kind="rejected_access", outputs=(), access_address=0xFFFF,
                      access_width=8, access_operation="write", detail="outside borrowed spans")
    assert failure.instructions == 3 and failure.access_address == 0xFFFF


def test_declaration_and_manifest_imports_cannot_load_execution_backends():
    source = r'''
import importlib.abc
import sys
class RejectBackends(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {
            'emulator', 'simulator', '_mp64_accel', '_megaforth_native', 'pygame',
        }:
            raise AssertionError('forbidden backend import: ' + fullname)
sys.meta_path.insert(0, RejectBackends())
import shared.hybrid_abi
import hybrid
assert not hasattr(hybrid, 'HybridRuntime')
'''
    completed = subprocess.run([sys.executable, "-c", source],
                               cwd=Path(__file__).resolve().parents[1],
                               capture_output=True, text=True, timeout=10)
    assert completed.returncode == 0, completed.stderr
