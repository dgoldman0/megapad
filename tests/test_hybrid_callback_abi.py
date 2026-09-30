"""V2 callback metadata is immutable and finite, never executable authority."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
from pathlib import Path
import subprocess
import sys

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI,
    HYBRID_ABI_VERSION,
    HYBRID_CALLBACK_ABI_VERSION,
    MAX_CODE_BYTES,
    MAX_DISPATCH_CALLBACKS,
    MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS,
    CallbackExportV2,
    CallbackRequestV2,
    CallbackSiteV2,
    MachineExitKindV1,
    MachineExitKindV2,
    MachineRoutineResultV1,
    MachineSegmentResultV2,
    RoutineDeclarationV2,
    RoutineImageV1,
    RoutineImageV2,
    RoutineManifestV1,
)


def _export(**changes):
    fields = dict(export_id=0, name="MIN", input_cells=2, output_cells=1)
    fields.update(changes)
    return CallbackExportV2(**fields)


def _site(**changes):
    fields = dict(call_offset=0, stub_offset=8, export=_export())
    fields.update(changes)
    return CallbackSiteV2(**fields)


def _image(**changes):
    # Metadata validation deliberately does not pretend these NOP bytes are
    # native CALL/RET instructions; the future publisher must prove that.
    fields = dict(name="H-POLICY", code=b"\x01" * 32, entry_offset=0,
                  input_cells=2, output_cells=1, buffers=(), max_instructions=100,
                  return_stack_cells=16, callbacks=(_site(),))
    fields.update(changes)
    return RoutineImageV2(**fields)


def _declaration(**changes):
    fields = dict(name="H-POLICY", code=b"\x01" * 32, entry_offset=0,
                  input_cells=2, output_cells=1, buffers=(), max_instructions=100,
                  return_stack_cells=16, callbacks=(_site(),),
                  session_nonce=object(), registration_nonce=object(),
                  allocation_lease=object(), allocation_generation=1,
                  control_lease=object(), control_generation=1,
                  body_base=0x1000, body_size=64, code_base=0x1010,
                  stack_base=0x2000, dispatch_instruction_limit=1000)
    fields.update(changes)
    return RoutineDeclarationV2(**fields)


def _request(**changes):
    fields = dict(invocation_id=1, sequence=1, site=_site(), arguments=(7, MASK64))
    fields.update(changes)
    return CallbackRequestV2(**fields)


def _result(**changes):
    fields = dict(exit_kind="returned", invocation_id=1, instructions=3, cycles=7,
                  invocation_instructions=8, invocation_cycles=17,
                  entry_pc=0x1010, pc=MASK64, outputs=(7,))
    fields.update(changes)
    return MachineSegmentResultV2(**fields)


@pytest.mark.parametrize("name,inputs", [
    ("MIN", 2), ("MAX", 2), ("ABS", 1), ("AND", 2), ("OR", 2), ("XOR", 2),
])
def test_leaf_catalog_has_exact_arity_one_step_and_no_callable_authority(name, inputs):
    value = _export(export_id=63, name=name, input_cells=inputs)
    assert (value.name, value.input_cells, value.output_cells) == (name, inputs, 1)
    assert value.max_semantic_steps == 1 and value.effect == "integer_leaf"
    assert value.abi == HYBRID_ABI and value.version == 2
    assert not hasattr(value, "word") and not hasattr(value, "callback")
    assert not hasattr(value, "__dict__")
    with pytest.raises(FrozenInstanceError):
        value.export_id = 2
    with pytest.raises(ValueError, match="arity"):
        replace(value, input_cells=0)


@pytest.mark.parametrize("changes,error,message", [
    ({"name": "min"}, ValueError, "canonical integer leaf"),
    ({"name": "F64+"}, ValueError, "canonical integer leaf"),
    ({"name": "EXECUTE"}, ValueError, "canonical integer leaf"),
    ({"name": "MIN "}, ValueError, "ASCII"),
    ({"name": lambda: None}, TypeError, "string"),
    ({"export_id": True}, TypeError, "exact integer"),
    ({"export_id": -1}, ValueError, "export ID"),
    ({"export_id": 64}, ValueError, "export ID"),
    ({"input_cells": 2.0}, TypeError, "exact integer"),
    ({"output_cells": True}, TypeError, "exact integer"),
    ({"output_cells": 2}, ValueError, "arity"),
    ({"max_semantic_steps": True}, TypeError, "exact integer"),
    ({"max_semantic_steps": 2}, ValueError, "semantic steps"),
    ({"effect": "service"}, ValueError, "integer_leaf"),
    ({"effect": b"integer_leaf"}, TypeError, "string"),
])
def test_leaf_descriptors_reject_unadmitted_names_effects_arities_and_types(changes, error, message):
    with pytest.raises(error, match=message):
        _export(**changes)


@pytest.mark.parametrize("factory", [_export, _site, _image, _declaration, _request, _result])
def test_v2_values_reject_other_versions_and_foreign_abi_identity(factory):
    for version in (0, 1, 3):
        with pytest.raises(ValueError, match="ABI version"):
            factory(version=version)
    with pytest.raises(TypeError, match="exact integer"):
        factory(version=True)
    with pytest.raises(ValueError, match="ABI identity"):
        factory(abi="other")


def test_v1_identity_version_and_outcomes_are_not_promoted_by_v2():
    assert HYBRID_ABI_VERSION == 1 and HYBRID_CALLBACK_ABI_VERSION == 2
    with pytest.raises(ValueError, match="ABI version"):
        RoutineImageV1(name="V1", code=b"\x0e", entry_offset=0, input_cells=0,
                       output_cells=0, buffers=(), max_instructions=1,
                       return_stack_cells=1, version=2)
    with pytest.raises(TypeError, match="RoutineImageV1"):
        RoutineManifestV1(dispatch_instruction_limit=1000, routines=(_image(),))
    with pytest.raises(ValueError):
        MachineExitKindV1("callback_request")
    with pytest.raises(TypeError, match="string"):
        MachineRoutineResultV1(exit_kind=MachineExitKindV2.RETURNED, instructions=1,
                               cycles=2, entry_pc=0, pc=MASK64)


@pytest.mark.parametrize("changes,error,message", [
    ({"call_offset": True}, TypeError, "exact integer"),
    ({"stub_offset": 1.0}, TypeError, "exact integer"),
    ({"call_offset": MAX_CODE_BYTES - 1}, ValueError, "call offset"),
    ({"stub_offset": MAX_CODE_BYTES}, ValueError, "stub offset"),
    ({"stub_offset": 0}, ValueError, "disjoint"),
    ({"stub_offset": 1}, ValueError, "disjoint"),
    ({"export": {}}, TypeError, "CallbackExportV2"),
])
def test_sites_reserve_complete_call_and_stub_byte_spans(changes, error, message):
    with pytest.raises(error, match=message):
        _site(**changes)


def test_metadata_bounds_use_actual_image_length_and_allow_valid_edge_positions():
    site = _site(call_offset=30, stub_offset=29)
    value = _image(callbacks=(site,))
    assert value.callbacks == (site,)
    with pytest.raises(ValueError, match="inside the code image"):
        replace(value, code=value.code[:-1])
    with pytest.raises(ValueError, match="inside the code image"):
        _image(callbacks=(_site(stub_offset=32),))


@pytest.mark.parametrize("other", [
    _site(),
    _site(call_offset=1, stub_offset=9),
    _site(call_offset=7, stub_offset=9),
    _site(call_offset=2, stub_offset=8),
    _site(call_offset=2, stub_offset=0),
])
def test_duplicate_and_cross_site_byte_overlaps_are_rejected(other):
    with pytest.raises(ValueError, match="overlap or repeat"):
        _image(callbacks=(_site(), other))


def test_reusing_equal_export_metadata_is_allowed_but_id_conflicts_are_not():
    first = _site()
    second = _site(call_offset=2, stub_offset=9, export=replace(first.export))
    value = _image(callbacks=(first, second))
    assert value.callbacks[0].export == value.callbacks[1].export
    with pytest.raises(ValueError, match="conflicting descriptors"):
        _image(callbacks=(first, replace(second, export=_export(name="MAX"))))
    assert _image(callbacks=(first, replace(second, export=_export(export_id=1)))).callbacks


def test_callback_tables_are_immutable_exact_values_with_a_finite_site_limit():
    sites = tuple(_site(call_offset=3 * index, stub_offset=3 * index + 2)
                  for index in range(16))
    value = _image(code=b"\x01" * 64, callbacks=sites)
    assert len(value.callbacks) == 16
    assert _image(callbacks=()).callbacks == ()
    with pytest.raises(ValueError, match="at most 16"):
        replace(value, callbacks=sites + (_site(call_offset=48, stub_offset=50),))
    with pytest.raises(TypeError, match="immutable tuple"):
        replace(value, callbacks=list(sites))
    with pytest.raises(TypeError, match="CallbackSiteV2"):
        replace(value, callbacks=(object(),))
    with pytest.raises(FrozenInstanceError):
        value.callbacks = ()


@pytest.mark.parametrize("changes,message", [
    ({"code_base": 0x1011}, "aligned"),
    ({"body_size": 32}, "complete body allocation"),
    ({"stack_base": 0x1010}, "disjoint"),
    ({"allocation_generation": 0}, "allocation generation"),
    ({"callbacks": (_site(stub_offset=32),)}, "inside the code image"),
    ({"dispatch_callback_limit": 0}, "dispatch callback requests"),
    ({"dispatch_callback_limit": MAX_DISPATCH_CALLBACKS + 1}, "dispatch callback requests"),
    ({"dispatch_callback_semantic_limit": MAX_DISPATCH_CALLBACK_SEMANTIC_STEPS + 1},
     "dispatch callback semantic steps"),
])
def test_sealed_declaration_keeps_v1_geometry_and_adds_finite_callback_limits(changes, message):
    with pytest.raises(ValueError, match=message):
        _declaration(**changes)


def test_sealed_declaration_preserves_opaque_identity_without_establishing_liveness():
    value = _declaration(dispatch_callback_limit=1, dispatch_callback_semantic_limit=1)
    copied = replace(value)
    assert copied.session_nonce is value.session_nonce
    assert copied.allocation_lease is value.allocation_lease
    assert copied.control_lease is value.control_lease
    assert value.entry_pc == 0x1010 and value.stack_size == 128
    for field in ("dispatch_callback_limit", "dispatch_callback_semantic_limit"):
        with pytest.raises(TypeError, match="exact integer"):
            replace(value, **{field: True})


@pytest.mark.parametrize("changes,error,message", [
    ({"invocation_id": 0}, ValueError, "invocation ID"),
    ({"invocation_id": MASK64 + 1}, ValueError, "invocation ID"),
    ({"invocation_id": True}, TypeError, "exact integer"),
    ({"sequence": 0}, ValueError, "request sequence"),
    ({"sequence": MAX_DISPATCH_CALLBACKS + 1}, ValueError, "request sequence"),
    ({"arguments": (7,)}, ValueError, "argument count"),
    ({"arguments": (7, 8, 9)}, ValueError, "argument count"),
    ({"arguments": [7, 8]}, TypeError, "immutable tuple"),
    ({"arguments": (False, 8)}, TypeError, "exact integer"),
    ({"arguments": (-1, 8)}, ValueError, "argument cell"),
    ({"arguments": (MASK64 + 1, 8)}, ValueError, "argument cell"),
    ({"site": object()}, TypeError, "CallbackSiteV2"),
])
def test_request_metadata_validates_identity_numbers_and_declared_arguments(changes, error, message):
    with pytest.raises(error, match=message):
        _request(**changes)


def test_request_is_copyable_observation_with_no_resume_token():
    value = _request(invocation_id=MASK64, sequence=MAX_DISPATCH_CALLBACKS)
    assert replace(value) == value
    assert not hasattr(value, "token") and not hasattr(value, "word")
    assert value.arguments == (7, MASK64)
    with pytest.raises(FrozenInstanceError):
        value.sequence = 2


def test_callback_segment_carries_no_final_outputs_and_preserves_prefix_totals():
    request = _request(sequence=2)
    value = _result(exit_kind="callback_request", outputs=(), callback=request)
    assert value.exit_kind is MachineExitKindV2.CALLBACK_REQUEST
    assert (value.instructions, value.cycles) == (3, 7)
    assert (value.invocation_instructions, value.invocation_cycles) == (8, 17)
    assert value.callback is request
    with pytest.raises(FrozenInstanceError):
        value.invocation_instructions = 9


@pytest.mark.parametrize("changes,error,message", [
    ({"instructions": 9}, ValueError, "exceed invocation totals"),
    ({"cycles": 18}, ValueError, "exceed invocation totals"),
    ({"cycles": 2}, ValueError, "inconsistent"),
    ({"invocation_instructions": 3}, ValueError, "inconsistent"),
    ({"invocation_cycles": 8}, ValueError, "inconsistent"),
    ({"instructions": 0, "cycles": 0}, ValueError, "completed RET"),
    ({"invocation_instructions": True}, TypeError, "exact integer"),
    ({"invocation_cycles": True}, TypeError, "exact integer"),
    ({"callback": _request()}, ValueError, "only callback request exits"),
    ({"exit_kind": "callback_request", "outputs": ()}, TypeError, "CallbackRequestV2"),
    ({"exit_kind": "callback_request", "callback": _request()}, ValueError, "publish outputs"),
    ({"exit_kind": "callback_request", "outputs": (), "callback": _request(invocation_id=2)},
     ValueError, "different invocation"),
    ({"exit_kind": "callback_request", "outputs": (), "callback": _request(),
      "instructions": 0, "cycles": 0}, ValueError, "completed CALL"),
    ({"exit_kind": "callback_request", "outputs": (), "callback": _request(sequence=9)},
     ValueError, "sequence exceeds"),
])
def test_segment_variants_and_counter_relations_fail_closed(changes, error, message):
    with pytest.raises(error, match=message):
        _result(**changes)


@pytest.mark.parametrize("kind", [kind for kind in MachineExitKindV2
                                 if kind not in (MachineExitKindV2.RETURNED,
                                                 MachineExitKindV2.CALLBACK_REQUEST)])
def test_failure_variants_allow_zero_work_after_a_completed_prefix_and_no_outputs(kind):
    value = _result(exit_kind=kind, instructions=0, cycles=0, outputs=())
    assert value.invocation_instructions == 8 and value.callback is None
    with pytest.raises(ValueError, match="publish outputs"):
        replace(value, outputs=(1,))
    with pytest.raises(ValueError, match="only callback request exits"):
        replace(value, callback=_request())
    with pytest.raises(ValueError, match="inconsistent"):
        replace(value, cycles=1)


def test_zero_work_cancellation_has_zero_totals_and_v1_diagnostics_remain_available():
    value = _result(exit_kind="cancelled", instructions=0, cycles=0,
                    invocation_instructions=0, invocation_cycles=0, outputs=(),
                    pc=0x1010, instruction_pc=0x1010, detail="cancelled before entry")
    assert value.trap_id == -1
    failure = _result(exit_kind="rejected_access", outputs=(), access_address=MASK64,
                      access_width=8, access_operation="write", detail="outside borrow")
    assert failure.access_address == MASK64 and failure.access_width == 8


def test_callback_values_import_and_construct_without_execution_backends():
    source = r'''
import importlib.abc
import sys
class RejectBackends(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {
            'emulator', 'simulator', 'hybrid', '_mp64_accel', '_megaforth_native', 'pygame',
        }:
            raise AssertionError('forbidden backend import: ' + fullname)
sys.meta_path.insert(0, RejectBackends())
from shared.hybrid_abi import CallbackExportV2, CallbackSiteV2, CallbackRequestV2
export = CallbackExportV2(export_id=0, name='MIN', input_cells=2, output_cells=1)
site = CallbackSiteV2(call_offset=0, stub_offset=2, export=export)
request = CallbackRequestV2(invocation_id=1, sequence=1, site=site, arguments=(3, 4))
assert request.version == 2
'''
    completed = subprocess.run([sys.executable, "-c", source],
                               cwd=Path(__file__).resolve().parents[1],
                               capture_output=True, text=True, timeout=10)
    assert completed.returncode == 0, completed.stderr
