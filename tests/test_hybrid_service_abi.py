"""V5 scalar-service metadata is bounded observation, never callback authority."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
from pathlib import Path
import subprocess
import sys

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import (
    HYBRID_ABI, BufferRuleV1, CallbackExportV2, CallbackSiteV2,
    CallbackRequestV2, MachineExitKindV2, RoutineImageV2, RoutineManifestV2,
)
from shared.hybrid_services import (
    HYBRID_SERVICE_ABI_VERSION, SCALAR_FP_SIGNATURES_V5, ServiceExportV5, CallbackSiteV5,
    RoutineImageV5, RoutineDeclarationV5, RoutineManifestV5,
    CallbackRequestV5, MachineSegmentResultV5,
)


CATALOG = (("FPCSR@", 0, 1), ("FPCSR!", 1, 0)) + tuple(
    (prefix + suffix, inputs, 1)
    for prefix in ("F32", "F64")
    for suffix, inputs in (("+", 2), ("-", 2), ("*", 2), ("/", 2), ("SQRT", 1), ("FMA", 3))
)


def _export(**changes):
    return ServiceExportV5(**(dict(export_id=0, name="F64+", input_cells=2,
                                  output_cells=1, effect="scalar_fp_state",
                                  max_semantic_steps=1) | changes))


def _site(**changes):
    return CallbackSiteV5(**(dict(call_offset=0, stub_offset=8, export=_export()) | changes))


def _image_fields():
    return dict(name="H-FP", code=b"\x01" * 32, entry_offset=0,
                input_cells=2, output_cells=1, buffers=(), max_instructions=100,
                max_callback_requests=1, return_stack_cells=16, callbacks=(_site(),))


def _image(**changes):
    return RoutineImageV5(**(_image_fields() | changes))


def _declaration_fields():
    return _image_fields() | dict(
        session_nonce=object(), registration_nonce=object(),
        allocation_lease=object(), allocation_generation=1,
        control_lease=object(), control_generation=1,
        body_base=0x1000, body_size=64, code_base=0x1010,
        stack_base=0x2000, dispatch_instruction_limit=1000,
        dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
    )


def _declaration(**changes):
    return RoutineDeclarationV5(**(_declaration_fields() | changes))


def _manifest(**changes):
    return RoutineManifestV5(**(dict(dispatch_instruction_limit=1000,
        dispatch_callback_limit=32, dispatch_callback_semantic_limit=64,
        exports=(_export(),), routines=(_image(),)) | changes))


def _request(**changes):
    return CallbackRequestV5(**(dict(invocation_id=1, sequence=1, site=_site(),
                                     arguments=(0, MASK64)) | changes))


def _result(**changes):
    return MachineSegmentResultV5(**(dict(exit_kind="returned", instructions=1, cycles=2,
        invocation_id=1, invocation_instructions=5, invocation_cycles=12,
        entry_pc=0x1010, pc=MASK64, outputs=(MASK64,)) | changes))


@pytest.mark.parametrize("name,inputs,outputs", CATALOG)
def test_fixed_service_catalog_has_exact_bios_signature_and_one_tick(name, inputs, outputs):
    assert SCALAR_FP_SIGNATURES_V5 == CATALOG
    value = _export(name=name, input_cells=inputs, output_cells=outputs)
    assert (value.name, value.input_cells, value.output_cells) == (name, inputs, outputs)
    assert (value.effect, value.max_semantic_steps) == ("scalar_fp_state", 1)
    assert (value.max_input_bytes, value.max_output_bytes, value.can_suspend) == (0, 0, False)
    with pytest.raises(ValueError):
        replace(value, input_cells=inputs + 1)
    with pytest.raises(ValueError):
        replace(value, output_cells=outputs + 1)


@pytest.mark.parametrize("name", (
    "MIN", "F32MIN", "F64MAX", "F64FMS", "F32=", "F64CLASS", "F64>F32",
    "S>F64", "F64TRUNC", "FPCSR", "f64+", "EXECUTE", "FAULT-XT!", "AES-GCM",
))
def test_unlisted_services_and_aliases_do_not_enter_the_fixed_catalog(name):
    with pytest.raises(ValueError):
        _export(name=name)


@pytest.mark.parametrize("factory", (_export, _site, _image, _declaration, _manifest, _request, _result))
def test_v5_values_are_frozen_slotted_and_have_exact_version_discrimination(factory):
    value = factory()
    assert (value.abi, value.version) == (HYBRID_ABI, HYBRID_SERVICE_ABI_VERSION)
    assert HYBRID_SERVICE_ABI_VERSION == 5
    assert not hasattr(value, "__dict__")
    with pytest.raises(FrozenInstanceError):
        value.version = 4
    for version in (1, 2, 3, 4, 6):
        with pytest.raises(ValueError):
            factory(version=version)
    with pytest.raises(TypeError):
        factory(version=True)
    with pytest.raises(ValueError):
        factory(abi="megapad.hybrid.task-routine")


@pytest.mark.parametrize("changes", (
    dict(export_id=True), dict(export_id=-1), dict(export_id=64),
    dict(input_cells=True), dict(output_cells=1.0), dict(max_semantic_steps=0),
    dict(max_semantic_steps=2), dict(max_semantic_steps=True),
    dict(effect="integer_leaf"), dict(effect="closed_integer_colon"), dict(effect=None),
))
def test_export_metadata_cannot_coerce_or_expand_effects(changes):
    with pytest.raises((TypeError, ValueError)):
        _export(**changes)


@pytest.mark.parametrize("field,value", (("max_input_bytes", 1), ("max_output_bytes", 1),
                                        ("can_suspend", True), ("fault_handler", object())))
def test_export_constructor_does_not_accept_buffer_suspend_or_fault_authority(field, value):
    with pytest.raises(TypeError):
        _export(**{field: value})


@pytest.mark.parametrize("factory,fields", ((RoutineImageV5, _image_fields),
                                            (RoutineDeclarationV5, _declaration_fields)))
def test_per_invocation_callback_ceiling_is_required_and_independent(factory, fields):
    values = fields()
    del values["max_callback_requests"]
    with pytest.raises(TypeError):
        factory(**values)
    for limit in (0, 1, 1024):
        assert factory(**(fields() | {"max_callback_requests": limit})).max_callback_requests == limit
    for limit in (-1, 1025, True, 1.0):
        with pytest.raises((TypeError, ValueError)):
            factory(**(fields() | {"max_callback_requests": limit}))


@pytest.mark.parametrize("changes", (
    dict(dispatch_instruction_limit=0), dict(dispatch_instruction_limit=10_000_001),
    dict(dispatch_callback_limit=0), dict(dispatch_callback_limit=1025),
    dict(dispatch_callback_semantic_limit=0), dict(dispatch_callback_semantic_limit=65537),
    dict(dispatch_callback_limit=True),
))
def test_manifest_and_declaration_keep_finite_dispatch_ceilings(changes):
    for factory in (_manifest, _declaration):
        with pytest.raises((TypeError, ValueError)):
            factory(**changes)


def test_manifest_tables_are_exact_bounded_and_resolve_consistent_exports():
    for field in ("exports", "routines"):
        original = _manifest()
        with pytest.raises(TypeError):
            replace(original, **{field: list(getattr(original, field))})
    with pytest.raises(ValueError):
        _manifest(exports=tuple(_export(export_id=i % 64) for i in range(65)))
    with pytest.raises(ValueError):
        _manifest(routines=tuple(_image(name=f"R{i}") for i in range(65)))
    with pytest.raises(ValueError):
        _manifest(exports=(_export(), _export()))
    with pytest.raises(ValueError):
        _manifest(exports=())
    with pytest.raises(ValueError):
        _manifest(exports=(_export(name="F64-"),))
    with pytest.raises(ValueError):
        _manifest(routines=(_image(), _image(name="h-fp")))
    assert _manifest(exports=(replace(_export()),)) == _manifest()


@pytest.mark.parametrize("callbacks", (
    [_site()], (_site(),) * 17, (_site(), _site()),
    (_site(), _site(call_offset=7, stub_offset=10)), (_site(call_offset=31),),
    (_site(stub_offset=32),),
))
def test_callback_tables_reject_mutability_overlap_and_image_escape(callbacks):
    with pytest.raises((TypeError, ValueError)):
        _image(callbacks=callbacks)


def test_nested_export_site_image_and_request_values_are_revalidated():
    export = _export()
    object.__setattr__(export, "input_cells", True)
    with pytest.raises(TypeError):
        _site(export=export)
    site = _site()
    object.__setattr__(site, "stub_offset", -1)
    with pytest.raises(ValueError):
        _image(callbacks=(site,))
    with pytest.raises(ValueError):
        _request(site=site)
    image = _image()
    object.__setattr__(image, "max_callback_requests", 1025)
    with pytest.raises(ValueError):
        _manifest(routines=(image,))
    request = _request()
    object.__setattr__(request, "arguments", (True, 0))
    with pytest.raises(TypeError):
        _result(exit_kind="callback_request", outputs=(), callback=request)


@pytest.mark.parametrize("factory", (_image, _declaration))
@pytest.mark.parametrize("field", ("address_argument", "length_argument"))
def test_forged_buffer_fields_reject_before_host_comparison(factory, field):
    class ForbiddenComparison:
        def __lt__(self, other):
            raise AssertionError("forged metadata executed a comparison")

        __gt__ = __lt__

    rule = BufferRuleV1(address_argument=0, length_argument=1,
                        element_bytes=1, max_bytes=32, access="read")
    object.__setattr__(rule, field, ForbiddenComparison())
    with pytest.raises(TypeError):
        factory(buffers=(rule,))


def test_subclasses_and_older_metadata_do_not_become_service_values():
    class ExportSubclass(ServiceExportV5):
        pass

    with pytest.raises(TypeError):
        _site(export=ExportSubclass(export_id=0, name="F64+", input_cells=2, output_cells=1))
    leaf = CallbackExportV2(export_id=0, name="MIN", input_cells=2, output_cells=1)
    old_site = CallbackSiteV2(call_offset=0, stub_offset=8, export=leaf)
    with pytest.raises(TypeError):
        _site(export=leaf)
    with pytest.raises(TypeError):
        _image(callbacks=(old_site,))
    with pytest.raises(TypeError):
        CallbackSiteV2(call_offset=0, stub_offset=8, export=_export())
    with pytest.raises(TypeError):
        CallbackRequestV2(invocation_id=1, sequence=1, site=_site(), arguments=(0, 0))
    fields = _image_fields()
    fields.pop("max_callback_requests")
    old_image = RoutineImageV2(**(fields | {"callbacks": (old_site,)}))
    with pytest.raises(TypeError):
        _manifest(routines=(old_image,))
    with pytest.raises(TypeError):
        RoutineManifestV2(dispatch_instruction_limit=10, dispatch_callback_limit=1,
                          dispatch_callback_semantic_limit=1, exports=(_export(),), routines=())


@pytest.mark.parametrize("changes", (
    dict(body_base=True), dict(body_size=16), dict(code_base=0x1011),
    dict(code=b"\x01" * 31), dict(stack_base=0x1000), dict(stack_base=0x2001),
    dict(allocation_generation=0), dict(control_generation=True),
    dict(session_nonce=None), dict(control_lease=None),
))
def test_declaration_checks_body_control_geometry_and_lease_descriptions(changes):
    with pytest.raises((TypeError, ValueError)):
        _declaration(**changes)


def test_request_and_result_preserve_bits_and_separate_segment_from_invocation_work():
    request = _request(arguments=(0x7FF0000000000001, MASK64))
    result = _result(exit_kind="callback_request", outputs=(), callback=request,
                     instructions=3, cycles=7, invocation_instructions=8, invocation_cycles=19)
    assert result.exit_kind is MachineExitKindV2.CALLBACK_REQUEST
    assert result.callback is request and result.callback.arguments == (0x7FF0000000000001, MASK64)
    assert (result.instructions, result.cycles) == (3, 7)
    assert (result.invocation_instructions, result.invocation_cycles) == (8, 19)
    assert replace(result) == result
    assert not hasattr(request, "token") and not hasattr(request, "resume")
    assert not hasattr(result, "token") and not hasattr(result, "fault_handler")
    exhausted = _result(exit_kind="instruction_limit", instructions=0, cycles=0, outputs=())
    assert exhausted.invocation_instructions == 5 and exhausted.callback is None


@pytest.mark.parametrize("changes", (
    dict(invocation_id=0), dict(invocation_id=True), dict(sequence=0), dict(sequence=1025),
    dict(arguments=[0, 0]), dict(arguments=(0,)), dict(arguments=(True, 0)),
    dict(arguments=(0, MASK64 + 1)),
))
def test_requests_require_exact_cells_and_bounded_invocation_identity(changes):
    with pytest.raises((TypeError, ValueError)):
        _request(**changes)


@pytest.mark.parametrize("changes", (
    dict(instructions=6), dict(cycles=13), dict(instructions=0, cycles=0),
    dict(instructions=0, cycles=1), dict(cycles=0),
    dict(callback=_request()), dict(exit_kind="instruction_limit"),
    dict(exit_kind="callback_request", outputs=(), callback=None),
    dict(exit_kind="callback_request", outputs=(), callback=_request(invocation_id=2)),
    dict(exit_kind="callback_request", outputs=(), callback=_request(sequence=6)),
))
def test_results_reject_incoherent_accounting_or_callback_publication(changes):
    with pytest.raises((TypeError, ValueError)):
        _result(**changes)


def test_service_values_import_without_any_backend_or_composition_module():
    code = r'''
import importlib.abc
import sys
class Guard(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in ('emulator', 'simulator', 'hybrid', '_mp64_accel', '_megaforth_native'):
            raise AssertionError('backend import: ' + fullname)
sys.meta_path.insert(0, Guard())
from shared.hybrid_services import ServiceExportV5, RoutineManifestV5
value = ServiceExportV5(export_id=0, name='FPCSR@', input_cells=0, output_cells=1)
assert RoutineManifestV5(dispatch_instruction_limit=1, dispatch_callback_limit=1,
    dispatch_callback_semantic_limit=1, exports=(value,), routines=()).version == 5
'''
    result = subprocess.run([sys.executable, "-B", "-c", code],
        cwd=Path(__file__).resolve().parents[1], capture_output=True, text=True, timeout=10)
    assert result.returncode == 0, result.stderr
