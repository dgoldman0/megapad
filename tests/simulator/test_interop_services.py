"""Isolated V5 identity/provenance; this suite enables no callback dispatch."""

from __future__ import annotations

import copy
import copyreg
import pickle

import pytest

from shared import ieee_fp, scalar_fp
from shared.hybrid_services import ServiceExportV5
from simulator import core_words
from simulator.errors import (
    ForthAbort, IllegalInstructionFault, InstructionFault, ExecutionError, SimulatorError,
)
from simulator.interop_exports import CallbackExportError
from simulator.interop_services import ScalarServiceCatalog
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition
from simulator.scalar_float import HostedScalarFloatService, IllegalScalarFloatError


# Independent arities and raw-bit vectors; no expectation comes from the
# implementation catalog or from invoking the same hosted operation twice.
VECTORS = (
    ("FPCSR@", (), (0,), 0),
    ("FPCSR!", ((1 << 64) - 1,), (), 0x1F7),
    ("F32+", (0x3F800000, 0x40000000), (0x40400000,), 0),
    ("F32-", (0x40000000, 0x3F800000), (0x3F800000,), 0),
    ("F32*", (0x40000000, 0x40400000), (0x40C00000,), 0),
    ("F32/", (0x3F800000, 0), (0x7F800000,), 0x80),
    ("F32SQRT", (0x40800000,), (0x40000000,), 0),
    ("F32FMA", (0x3F800000, 0x40000000, 0x40800000), (0x40C00000,), 0),
    ("F64+", (0x3FF0000000000000, 0x4000000000000000), (0x4008000000000000,), 0),
    ("F64-", (0x4000000000000000, 0x3FF0000000000000), (0x3FF0000000000000,), 0),
    ("F64*", (0x4000000000000000, 0x4008000000000000), (0x4018000000000000,), 0),
    ("F64/", (0x3FF0000000000000, 0), (0x7FF0000000000000,), 0x80),
    ("F64SQRT", (0x4010000000000000,), (0x4000000000000000,), 0),
    ("F64FMA", (0x3FF0000000000000, 0x4000000000000000, 0x4010000000000000),
     (0x4018000000000000,), 0),
)


def _descriptor(name, export_id=0):
    _, inputs, outputs, _ = next(row for row in VECTORS if row[0] == name)
    return ServiceExportV5(export_id=export_id, name=name,
                           input_cells=len(inputs), output_cells=len(outputs))


def _catalog(*, backend="python", core_installed=True):
    runtime = MegaForthRuntime(execution_backend=backend, install_core_words=core_installed)
    catalog = ScalarServiceCatalog.capture(runtime, core_installed=core_installed)
    catalog.finalize_executor(runtime._native_execution)
    return runtime, catalog


def _push(runtime, arguments):
    for value in arguments:
        runtime.main_context.data.push(value)


@pytest.mark.parametrize("backend", ("python", "native"))
@pytest.mark.parametrize("name,arguments,outputs,fpcsr", VECTORS, ids=[row[0] for row in VECTORS])
def test_service_capture_exact_catalog_vectors(backend, name, arguments, outputs, fpcsr):
    if backend == "native":
        pytest.importorskip("_megaforth_native")
    runtime, catalog = _catalog(backend=backend)
    descriptor = _descriptor(name)
    capture = catalog.bind(descriptor)
    assert capture.descriptor == descriptor and capture.descriptor is not descriptor
    assert capture.word is runtime.dictionary.find(name)
    assert capture.callback is capture.word.implementation.callback
    assert runtime.scalar_float is catalog._service
    _push(runtime, arguments)
    scope = capture.validation_boundary()
    # Directly exercise the original closure. This is not a service-export
    # invocation: the isolated foundation neither owns nor charges a tick.
    with scope:
        assert capture.callback(runtime.main_context) is None
    assert runtime.main_context.data.snapshot() == outputs
    assert runtime.scalar_float.fpcsr == fpcsr
    assert scope.failure is None
    assert runtime.scalar_float._validation_scope is None
    assert runtime.scalar_float._validation_armed is None
    capture.verify()


@pytest.mark.parametrize("name,operation,arguments", (
    ("F32+", 0x00, (0x3F800000, 0x40000000)),
    ("F32SQRT", 0x04, (0x40800000,)),
    ("F32FMA", 0x07, (0x3F800000, 0x40000000, 0x40800000)),
    ("F64+", 0x40, (0x3FF0000000000000, 0x4000000000000000)),
    ("F64SQRT", 0x44, (0x4010000000000000,)),
    ("F64FMA", 0x47, (0x3FF0000000000000, 0x4000000000000000, 0x4010000000000000)),
))
@pytest.mark.parametrize("mode", (5, 6, 7))
def test_service_capture_invalid_rounding_consumes_inputs_without_service_effect(name, operation, arguments, mode):
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor(name))
    runtime.scalar_float.write_fpcsr(0x90 | mode)
    _push(runtime, (0xA5, *arguments))
    scope = capture.validation_boundary()
    with pytest.raises(IllegalScalarFloatError, match=f"reserved FPCSR.RM {mode}") as raised:
        with scope:
            capture.callback(runtime.main_context)
    assert runtime.main_context.data.snapshot() == (0xA5,)
    assert runtime.scalar_float.fpcsr == 0x90 | mode
    assert scope.failure.cause is raised.value
    assert (scope.failure.operation, scope.failure.fpcsr) == (operation, 0x90 | mode)
    assert runtime.scalar_float._validation_scope is None
    assert runtime.scalar_float._validation_armed is None


@pytest.mark.parametrize("shape,operation,arguments,expected", (
    ("unary", 0x44, (11,), (0x44, 11, 11, 0)),
    ("binary", 0x41, (11, 22), (0x41, 11, 22, 0)),
    ("fma", 0x47, (11, 22, 33), (0x47, 33, 11, 22)),
))
def test_service_validation_seam_preserves_original_closure_operand_order(shape, operation, arguments, expected):
    # A custom service is deliberately outside catalog admission; this spy
    # locks the original closure's pop order without replacing admitted code.
    events = []

    class Data:
        def __init__(self):
            self.values = list(arguments)

        def pop(self):
            value = self.values.pop()
            events.append(("pop", value))
            return value

        def push(self, value):
            events.append(("push", value))

    class Service:
        def operate(self, op, rd, rs, rt=0):
            events.append(("operate", op, rd, rs, rt))
            return 99

    class Context:
        data = Data()

    core_words._scalar_float_word(Service(), shape, operation)(Context())
    assert events == [("pop", value) for value in reversed(arguments)] + [
        ("operate", *expected), ("push", 99)]


def test_service_capture_finalize_once_and_missing_core():
    runtime = MegaForthRuntime(execution_backend="python")
    catalog = ScalarServiceCatalog.capture(runtime, core_installed=True)
    with pytest.raises(CallbackExportError, match="not been finalized"):
        catalog.bind(_descriptor("F64+"))
    catalog.finalize_executor(None)
    with pytest.raises(CallbackExportError, match="already finalized"):
        catalog.finalize_executor(None)
    _, empty = _catalog(core_installed=False)
    with pytest.raises(CallbackExportError, match="not captured"):
        empty.bind(_descriptor("FPCSR@"))
    with pytest.raises(TypeError, match="exact bool"):
        ScalarServiceCatalog.capture(runtime, core_installed=1)
    with pytest.raises(TypeError, match="admitted callback"):
        runtime.bind_callback_export(_descriptor("F64+"))


@pytest.mark.parametrize("value_name", ("descriptor", "word", "primitive", "outcome", "format"))
def test_service_capture_normal_metadata_copy_does_not_change_admission(value_name):
    runtime, catalog = _catalog()
    descriptor = _descriptor("F64+")
    capture = catalog.bind(descriptor)
    values = {
        "descriptor": descriptor,
        "word": capture.word,
        "primitive": capture.word.implementation,
        "outcome": scalar_fp.Outcome(0, 0),
        "format": ieee_fp.FP64,
    }
    value = values[value_name]
    duplicate = copy.copy(value)
    assert duplicate is not value
    # Exercise the standard lazy cache explicitly too: dataclass-specific
    # __getstate__ routes differ across supported Python versions.
    copyreg._slotnames(type(value))
    capture.verify()
    assert catalog.bind(copy.copy(descriptor)).word is capture.word
    assert catalog.bind(descriptor).word is capture.word


@pytest.mark.parametrize("cache", (("export_id",), ["wrong-slot"], [object()]))
def test_service_capture_noncanonical_derived_slot_cache_is_rejected(cache, monkeypatch):
    _, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    monkeypatch.setattr(ServiceExportV5, "__slotnames__", cache, raising=False)
    with pytest.raises(CallbackExportError, match="derived slot cache"):
        capture.verify()


@pytest.mark.parametrize("base", (IllegalInstructionFault, InstructionFault, ExecutionError, SimulatorError))
@pytest.mark.parametrize("route", ("__new__", "__init__"))
def test_service_capture_inherited_exception_constructor_override_never_runs(base, route, monkeypatch):
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    runtime.scalar_float.write_fpcsr(5)
    _push(runtime, (11, 22))
    calls = []

    def replacement(*args, **kwargs):
        calls.append((args, kwargs))
        raise AssertionError("host constructor must not run during admission")

    monkeypatch.setattr(base, route, replacement, raising=False)
    with pytest.raises(CallbackExportError, match="class route changed"):
        with capture.validation_boundary():
            capture.callback(runtime.main_context)
    assert calls == []
    assert runtime.main_context.data.snapshot() == (11, 22)
    assert runtime.scalar_float.fpcsr == 5


def test_service_capture_shadowing_does_not_retarget_original_word():
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    replacement = runtime.define_primitive("F64+", lambda _context: None)
    assert runtime.dictionary.find("F64+") is replacement
    assert capture.word is not replacement
    capture.verify()
    assert catalog.bind(_descriptor("F64+", 1)).word is capture.word


@pytest.mark.parametrize("change", ("remove", "reuse", "implementation", "callback"))
def test_service_capture_original_word_removal_reuse_and_replacement_decline(change, monkeypatch):
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    if change == "remove":
        monkeypatch.delitem(runtime.dictionary._by_xt, capture.word.xt)
    elif change == "reuse":
        monkeypatch.setitem(runtime.dictionary._by_xt, capture.word.xt, runtime.dictionary.find("F64-"))
    elif change == "implementation":
        previous = capture.word.implementation
        object.__setattr__(capture.word, "implementation", PrimitiveDefinition(lambda _context: None))
    else:
        previous = capture.word.implementation.callback
        object.__setattr__(capture.word.implementation, "callback", lambda _context: None)
    try:
        with pytest.raises(CallbackExportError, match="stale or changed"):
            capture.verify()
    finally:
        if change == "implementation":
            object.__setattr__(capture.word, "implementation", previous)
        elif change == "callback":
            object.__setattr__(capture.word.implementation, "callback", previous)


@pytest.mark.parametrize("change", ("service", "kernel", "validator", "oracle", "rounding_constant", "method"))
def test_service_capture_late_route_changes_decline_before_operand_pop(change, monkeypatch):
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    _push(runtime, (11, 22))
    if change == "service":
        monkeypatch.setattr(runtime, "scalar_float", HostedScalarFloatService())
    elif change == "kernel":
        monkeypatch.setattr(runtime.scalar_float, "_native_execute", lambda *_args: (0, 0, None))
    elif change == "validator":
        monkeypatch.setattr(scalar_fp, "validate", lambda *_args: None)
    elif change == "oracle":
        monkeypatch.setattr(ieee_fp, "add", lambda *_args: (0, 0))
    elif change == "rounding_constant":
        monkeypatch.setattr(ieee_fp, "RMM", 7)
    else:
        monkeypatch.setattr(HostedScalarFloatService, "operate", lambda *_args: 0)
    with pytest.raises(CallbackExportError):
        with capture.validation_boundary():
            capture.callback(runtime.main_context)
    assert runtime.main_context.data.snapshot() == (11, 22)


def test_service_capture_changed_function_code_and_closure_owner_decline():
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    closure = capture.callback
    service_cell = dict(zip(closure.__code__.co_freevars, closure.__closure__))["service"]
    original_service = service_cell.cell_contents
    try:
        service_cell.cell_contents = HostedScalarFloatService()
        with pytest.raises(CallbackExportError, match="closure contents"):
            capture.verify()
    finally:
        service_cell.cell_contents = original_service
    original_code = scalar_fp.uses_dynamic_rounding.__code__
    try:
        scalar_fp.uses_dynamic_rounding.__code__ = (lambda _op: False).__code__
        with pytest.raises(CallbackExportError, match="function implementation"):
            capture.verify()
    finally:
        scalar_fp.uses_dynamic_rounding.__code__ = original_code


def test_service_capture_namespace_key_scan_never_calls_host_equality():
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    calls = []

    class Key:
        armed = False

        def __hash__(self):
            return hash("scalar_float")

        def __eq__(self, other):
            if self.armed:
                calls.append(other)
                runtime.scalar_float = HostedScalarFloatService()
            return False

    key = Key()
    runtime.__dict__[key] = 1
    key.armed = True
    try:
        with pytest.raises(CallbackExportError, match="namespace"):
            capture.verify()
        assert calls == []
        assert runtime.scalar_float is catalog._service
    finally:
        del runtime.__dict__[key]


def test_service_capture_custom_service_and_python_kernel_cannot_be_finalized():
    class CustomService(HostedScalarFloatService):
        pass

    runtime = MegaForthRuntime(execution_backend="python")
    original = runtime.scalar_float
    runtime.scalar_float = CustomService()
    with pytest.raises(CallbackExportError, match="exact canonical"):
        ScalarServiceCatalog.capture(runtime, core_installed=True)
    runtime.scalar_float = original
    catalog = ScalarServiceCatalog.capture(runtime, core_installed=True)
    original._native_execute = lambda *_args: (0, 0, None)
    with pytest.raises(CallbackExportError, match="Python scalar"):
        catalog.finalize_executor(None)


def test_service_capture_scope_is_one_shot_and_nested_scope_cannot_clear_owner():
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("FPCSR@"))
    scope = capture.validation_boundary()
    with scope:
        armed = runtime.scalar_float._validation_scope
        with pytest.raises(CallbackExportError, match="already active"):
            with capture.validation_boundary():
                pytest.fail("nested scope entered")
        assert runtime.scalar_float._validation_scope is armed
        capture.callback(runtime.main_context)
    with pytest.raises(CallbackExportError, match="one-shot"):
        with scope:
            pytest.fail("scope replayed")
    assert runtime.scalar_float._validation_scope is None


@pytest.mark.parametrize("kind", (IllegalScalarFloatError, scalar_fp.IllegalOperation, RuntimeError))
def test_service_validation_seam_raw_kernel_error_keeps_original_identity(kind):
    service = HostedScalarFloatService()
    error = kind("host kernel escape")

    def kernel(*_args):
        assert service._validation_armed is None
        raise error

    service._native_execute = kernel
    boundary = service._begin_validation_boundary(0x40)
    with pytest.raises(kind) as raised:
        service.operate(0x40, 0, 0)
    assert raised.value is error
    assert service._finish_validation_boundary(boundary, error) is None
    assert service.fpcsr == 0


def test_service_validation_seam_reentrant_kernel_cannot_inherit_claimed_boundary():
    service = HostedScalarFloatService()
    failures = []

    def kernel(*_args):
        assert service._validation_armed is None
        service.write_fpcsr(5)
        try:
            service.operate(0x40, 0, 0)
        except IllegalScalarFloatError as error:
            failures.append(error)
            raise

    service._native_execute = kernel
    boundary = service._begin_validation_boundary(0x40)
    with pytest.raises(IllegalScalarFloatError) as raised:
        service.operate(0x40, 0, 0)
    assert raised.value is failures[0]
    assert service._finish_validation_boundary(boundary, raised.value) is None
    assert service.fpcsr == 5  # the completed host prefix is not rolled back


def test_service_validation_seam_replaced_validator_has_no_original_provenance(monkeypatch):
    service = HostedScalarFloatService()
    service.write_fpcsr(5)

    def replacement(*_args):
        raise scalar_fp.IllegalOperation("custom validator")

    monkeypatch.setattr(scalar_fp, "validate", replacement)
    boundary = service._begin_validation_boundary(0x40)
    with pytest.raises(IllegalScalarFloatError) as raised:
        service.operate(0x40, 0, 0)
    assert service._finish_validation_boundary(boundary, raised.value) is None


def test_service_validation_seam_only_first_operation_can_claim_boundary():
    service = HostedScalarFloatService()
    boundary = service._begin_validation_boundary(0x40)
    assert service.operate(0x40, 0, 0) == 0
    service.write_fpcsr(5)
    with pytest.raises(IllegalScalarFloatError) as raised:
        service.operate(0x40, 0, 0)
    assert service._finish_validation_boundary(boundary, raised.value) is None


def test_service_validation_seam_foreign_copied_replayed_and_wrong_error_do_not_match():
    service, other = HostedScalarFloatService(), HostedScalarFloatService()
    service.write_fpcsr(5)
    boundary = service._begin_validation_boundary(0x40)
    boundary_routes = tuple(vars(type(boundary)).items())
    with pytest.raises(RuntimeError, match="not issued"):
        other._finish_validation_boundary(boundary, None)
    for duplicate in (copy.copy(boundary), copy.deepcopy(boundary)):
        assert duplicate is not boundary and duplicate.operation == 0x40
        assert duplicate.failure is None
        with pytest.raises(RuntimeError, match="not issued"):
            service._finish_validation_boundary(duplicate, None)
    with pytest.raises(TypeError, match="cannot be serialized"):
        pickle.dumps(boundary)
    assert tuple(vars(type(boundary)).items()) == boundary_routes
    assert service._validation_scope is boundary
    with pytest.raises(IllegalScalarFloatError) as raised:
        service.operate(0x40, 0, 0)
    assert boundary.failure[0] is raised.value
    duplicate = copy.copy(boundary)
    assert duplicate.failure is None
    with pytest.raises(RuntimeError, match="not issued"):
        service._finish_validation_boundary(duplicate, raised.value)
    assert boundary.failure[0] is raised.value
    assert tuple(vars(type(boundary)).items()) == boundary_routes
    assert service._finish_validation_boundary(boundary, IllegalScalarFloatError(str(raised.value))) is None
    with pytest.raises(RuntimeError, match="consumed"):
        service._finish_validation_boundary(boundary, raised.value)
    assert boundary.failure is None
    assert service._validation_scope is None


def test_service_validation_seam_copied_service_cannot_consume_original_evidence():
    service = HostedScalarFloatService()
    service.write_fpcsr(5)
    boundary = service._begin_validation_boundary(0x40)
    duplicate = copy.copy(service)
    assert duplicate._validation_scope is boundary
    with pytest.raises(IllegalScalarFloatError) as raised:
        service.operate(0x40, 0, 0)
    with pytest.raises(RuntimeError, match="not issued"):
        duplicate._finish_validation_boundary(boundary, raised.value)
    assert service._validation_scope is boundary
    assert boundary.failure[0] is raised.value
    assert service._finish_validation_boundary(boundary, raised.value) == (raised.value, 0x40, 5)
    # Ordinary copying added at most the derived slot cache, not a reason to
    # disable every later canonical scalar catalog in the process.
    _, catalog = _catalog()
    catalog.bind(_descriptor("F64+")).verify()


def test_service_capture_raw_hook_error_is_not_a_validation_failure():
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    _push(runtime, (11, 22))
    error = IllegalScalarFloatError("host hook escape")
    scope = capture.validation_boundary()
    with pytest.raises(IllegalScalarFloatError) as raised:
        with scope:
            raise error
    assert raised.value is error
    assert scope.failure is None
    assert runtime.main_context.data.snapshot() == (11, 22)
    assert runtime.scalar_float._validation_scope is None


@pytest.mark.parametrize("notes_fail", (False, True))
def test_service_capture_cleanup_failure_preserves_host_error_and_fails_closed(notes_fail):
    runtime, catalog = _catalog()
    capture = catalog.bind(_descriptor("F64+"))
    error = IllegalScalarFloatError("original host error")
    if notes_fail:
        # BaseException.add_note rejects a non-list __notes__ value. No
        # exception-class monkeypatch or arbitrary cleanup hook is needed.
        error.__notes__ = ()
    with pytest.raises(IllegalScalarFloatError) as raised:
        with capture.validation_boundary():
            runtime.scalar_float._validation_scope = None
            raise error
    assert raised.value is error
    if notes_fail:
        assert error.__notes__ == ()
    else:
        assert any("cleanup failed" in note for note in error.__notes__)
    assert runtime.scalar_float._validation_armed is None
    with pytest.raises(CallbackExportError, match="failed closed"):
        capture.verify()


def test_service_validation_seam_unarmed_behavior_and_ordinary_fault_handler_are_unchanged():
    service = HostedScalarFloatService()
    service.write_fpcsr(5)
    with pytest.raises(IllegalScalarFloatError):
        service.operate(0x40, 0, 0)
    assert service._validation_scope is None and service._validation_armed is None

    runtime = MegaForthRuntime(execution_backend="python")
    runtime.scalar_float.write_fpcsr(5)
    _push(runtime, (0, 0))
    with pytest.raises(ForthAbort, match="reserved FPCSR.RM 5"):
        runtime.execute("F64+", step_budget=100)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.scalar_float.fpcsr == 5
    assert runtime.drain_uart_output() == b"\r\n*** ILLEGAL INSTRUCTION CORE=00\r\n"
    assert runtime.scalar_float._validation_scope is None
