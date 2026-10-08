"""One manifest format declares routines, their buffers and their callbacks."""

from __future__ import annotations

import json

import pytest

from asm import assemble
from hybrid.manifest import (
    ABI,
    VERSION,
    BufferRule,
    CallbackSite,
    HybridManifestError,
    RoutineDeclaration,
    load_manifest,
)


CODE = bytes(assemble("inc r4\ninc r4\ninc r4\nret.l"))


def write(tmp_path, routines, **document):
    (tmp_path / "inc.bin").write_bytes(CODE)
    path = tmp_path / "routines.json"
    path.write_text(json.dumps({"abi": ABI, "version": VERSION, "routines": routines, **document}))
    return path


def test_a_manifest_reads_images_relative_to_itself(tmp_path):
    path = write(tmp_path, [{
        "name": "H-FILL", "image": "inc.bin", "entry_offset": 0,
        "input_cells": 3, "output_cells": 1,
        "buffers": [{"address_argument": 0, "length_argument": 1, "element_bytes": 8,
                     "access": "write", "max_bytes": 4096}],
        "callbacks": [{"call_offset": 0, "stub_offset": 2, "target": "PICK-ONE",
                       "input_cells": 2, "output_cells": 1}],
    }])
    manifest = load_manifest(path)
    (routine,) = manifest.routines
    assert routine == RoutineDeclaration(
        "H-FILL", CODE, 0, 3, 1,
        (BufferRule(0, 1, 8, "write", 4096),),
        (CallbackSite(0, 2, "PICK-ONE", 2, 1),),
    )


def test_optional_fields_take_their_defaults(tmp_path):
    (routine,) = load_manifest(write(tmp_path, [{"name": "H-INC", "image": "inc.bin"}])).routines
    assert (routine.entry_offset, routine.input_cells, routine.output_cells) == (0, 0, 0)
    assert routine.buffers == () and routine.callbacks == ()


@pytest.mark.parametrize(("document", "message"), [
    ({"abi": "other"}, "abi"),
    ({"version": 2}, "version"),
    ({"limits": {}}, "unknown fields: limits"),
])
def test_the_document_identity_is_exact(tmp_path, document, message):
    with pytest.raises(HybridManifestError, match=message):
        load_manifest(write(tmp_path, [], **document))


@pytest.mark.parametrize(("routine", "message"), [
    ({"name": "H-INC"}, "missing: image"),
    ({"name": "H-INC", "image": "missing.bin"}, "cannot be read"),
    ({"name": "H INC", "image": "inc.bin"}, "word name"),
    ({"name": "H-INC", "image": "inc.bin", "return_stack_cells": 8}, "unknown fields"),
    ({"name": "H-INC", "image": "inc.bin", "input_cells": 9}, "input_cells"),
    ({"name": "H-INC", "image": "inc.bin", "entry_offset": 4}, "entry_offset"),
    ({"name": "H-INC", "image": "inc.bin", "input_cells": 1,
      "buffers": [{"address_argument": 0, "length_argument": 1}]}, "argument it does not take"),
    ({"name": "H-INC", "image": "inc.bin",
      "buffers": [{"address_argument": 0, "length_argument": 0, "access": "all"}]}, "access"),
    ({"name": "H-INC", "image": "inc.bin",
      "callbacks": [{"call_offset": 0, "stub_offset": 9, "target": "X"}]}, "outside the code"),
    ({"name": "H-INC", "image": "inc.bin",
      "callbacks": [{"call_offset": 0, "stub_offset": 1, "target": "X", "input_cells": 9}]},
     "callback input_cells"),
])
def test_routine_fields_are_checked(tmp_path, routine, message):
    with pytest.raises(HybridManifestError, match=message):
        load_manifest(write(tmp_path, [routine]))


def test_routine_names_are_distinct_ignoring_case(tmp_path):
    routines = [{"name": "H-INC", "image": "inc.bin"}, {"name": "h-inc", "image": "inc.bin"}]
    with pytest.raises(HybridManifestError, match="distinct"):
        load_manifest(write(tmp_path, routines))


def test_an_unreadable_manifest_is_reported(tmp_path):
    path = tmp_path / "routines.json"
    path.write_text("{not json")
    with pytest.raises(HybridManifestError, match="cannot read manifest"):
        load_manifest(path)
