"""V5 services share the existing export table and one accounting owner.

These semantic integration tests do not admit a native V5 machine profile.
Outer machine-Word exception settlement is a separate bridge gate.
"""

from __future__ import annotations

from copy import copy
from dataclasses import FrozenInstanceError
from pathlib import Path
import subprocess
import sys
import pytest

from shared.hybrid_abi import CallbackExportV2
from shared.hybrid_services import CallbackRequestV5, CallbackSiteV5, ServiceExportV5
from simulator.errors import ForthAbort, IllegalInstructionFault
from simulator.interop_exports import (
    CallbackExportBudgetExceeded, CallbackExportError, CallbackExportResult,
    ServiceCallbackFailureV5, ServiceCallbackReceiptV5, ServiceCallbackProfileV5,
    begin_service_callback_accounting, consume_service_callback_accounting,
    service_callback_profile,
)
from simulator.interop_services import ScalarServiceCatalog
from simulator.runtime import MegaForthRuntime
from simulator.scalar_float import IllegalScalarFloatError


SIGNATURES = {
    "FPCSR@": (0, 1), "FPCSR!": (1, 0),
    "F32+": (2, 1), "F32-": (2, 1), "F32*": (2, 1), "F32/": (2, 1),
    "F32SQRT": (1, 1), "F32FMA": (3, 1),
    "F64+": (2, 1), "F64-": (2, 1), "F64*": (2, 1), "F64/": (2, 1),
    "F64SQRT": (1, 1), "F64FMA": (3, 1),
}
ONE, TWO, THREE, FOUR, SIX = (
    0x3FF0000000000000, 0x4000000000000000, 0x4008000000000000,
    0x4010000000000000, 0x4018000000000000,
)


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    value = MegaForthRuntime(execution_backend=request.param)
    try:
        yield value
    finally:
        value.memory.mmio.audio.release_host_sink()


def descriptor(name="F64+", export_id=0):
    inputs, outputs = SIGNATURES[name]
    return ServiceExportV5(export_id=export_id, name=name, input_cells=inputs, output_cells=outputs)


def request_for(value, arguments, *, invocation_id=7, sequence=3):
    return CallbackRequestV5(invocation_id=invocation_id, sequence=sequence,
                             site=CallbackSiteV5(call_offset=16, stub_offset=24, export=value),
                             arguments=arguments)


def caller_state(runtime):
    context = runtime.main_context
    return (context.data.snapshot(), context.returns.snapshot(), context.data.pointer,
            context.returns.pointer, context.returns._continuation_cookie,
            context.returns._pointer_capture_generation)


def perform(runtime, value, arguments, *, semantic_step_limit=None):
    handle = runtime.bind_callback_export(value)
    request = request_for(value, arguments)
    checkpoint = begin_service_callback_accounting(runtime, handle, request)
    result, error = None, None
    try:
        result = runtime.invoke_callback_export(handle, arguments, semantic_step_limit=semantic_step_limit)
    except BaseException as escaped:
        error = escaped
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle, error)
    return result, error, receipt


@pytest.mark.parametrize("name,arguments,expected,fpcsr", (
    ("FPCSR@", (), (0,), 0), ("FPCSR!", ((1 << 64) - 1,), (), 0x1F7),
    ("F32+", (0x3F800000, 0x40000000), (0x40400000,), 0),
    ("F32-", (0x40400000, 0x3F800000), (0x40000000,), 0),
    ("F32*", (0x40000000, 0x40400000), (0x40C00000,), 0),
    ("F32/", (0x40C00000, 0x40400000), (0x40000000,), 0),
    ("F32SQRT", (0x40800000,), (0x40000000,), 0),
    ("F32FMA", (0x3F800000, 0x40000000, 0x40800000), (0x40C00000,), 0),
    ("F64+", (ONE, TWO), (THREE,), 0), ("F64-", (THREE, ONE), (TWO,), 0),
    ("F64*", (TWO, THREE), (SIX,), 0), ("F64/", (SIX, THREE), (TWO,), 0),
    ("F64SQRT", (FOUR,), (TWO,), 0), ("F64FMA", (ONE, TWO, FOUR), (SIX,), 0),
))
def test_all_services_use_issued_binding_original_owner_and_one_tick(runtime, name, arguments, expected, fpcsr):
    runtime.main_context.data.push(0xCAFE)
    runtime.main_context.returns.push(0xBEEF)
    before = caller_state(runtime)
    original_service = runtime.scalar_float
    ticks = runtime.diagnostics.semantic_cycles
    result, error, receipt = perform(runtime, descriptor(name), arguments)
    assert error is None and result.outputs == expected
    assert result.semantic_steps == receipt.semantic_steps == 1
    assert receipt.entered and receipt.completed
    assert receipt.consumed_input_cells == len(arguments) and receipt.failure is None
    assert runtime.diagnostics.semantic_cycles - ticks == 1
    assert runtime.scalar_float is original_service and original_service.fpcsr == fpcsr
    assert caller_state(runtime) == before
    assert runtime._callback_exports._active is None
    assert runtime._callback_exports._closed_accounting is None
    assert runtime._active_dispatches == [] and runtime.drain_uart_output() == b""
    assert original_service._validation_scope is original_service._validation_armed is None
    assert runtime.callback_export_abi_version == 4


@pytest.mark.parametrize("name,arguments,operation", (
    ("F32+", (0x3F800000, 0x40000000), 0x00),
    ("F32SQRT", (0x40800000,), 0x04),
    ("F32FMA", (0x3F800000, 0x40000000, 0x40800000), 0x07),
    ("F64+", (ONE, TWO), 0x40), ("F64SQRT", (FOUR,), 0x44),
    ("F64FMA", (ONE, TWO, FOUR), 0x47),
))
@pytest.mark.parametrize("mode", (5, 6, 7))
def test_only_exact_canonical_validation_issues_a_typed_failure(runtime, name, arguments, operation, mode):
    runtime.scalar_float.write_fpcsr(0x90 | mode)
    before = caller_state(runtime)
    result, error, receipt = perform(runtime, descriptor(name), arguments)
    assert result is None and type(error) is IllegalScalarFloatError
    assert receipt.semantic_steps == 1 and receipt.entered and not receipt.completed
    assert receipt.consumed_input_cells == len(arguments)
    failure = receipt.failure
    assert type(failure) is ServiceCallbackFailureV5 and failure.cause is error
    assert (failure.export_id, failure.name, failure.operation) == (0, name, operation)
    assert (failure.invocation_id, failure.sequence, failure.call_offset, failure.stub_offset) == (7, 3, 16, 24)
    assert (failure.fpcsr, failure.semantic_steps, failure.consumed_input_cells) == (0x90 | mode, 1, len(arguments))
    assert (failure.fault_kind, failure.throw_code) == ("illegal_scalar_float", -21)
    assert runtime.scalar_float.fpcsr == 0x90 | mode
    assert caller_state(runtime) == before and runtime.drain_uart_output() == b""


@pytest.mark.parametrize("kind", (IllegalScalarFloatError, IllegalInstructionFault,
                                 ForthAbort, RuntimeError, MemoryError, KeyboardInterrupt))
def test_raw_tick_escape_is_not_a_service_fault_and_preserves_effects(runtime, monkeypatch, kind):
    error = kind("original host escape")
    before = caller_state(runtime)

    def tick():
        runtime.scalar_float.write_fpcsr(0x80)
        raise error

    monkeypatch.setattr(runtime, "_account_semantic_step", tick)
    result, escaped, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert result is None and escaped is error
    assert receipt.semantic_steps == 1 and receipt.entered and not receipt.completed
    assert receipt.failure is None and receipt.consumed_input_cells == 0
    assert runtime.scalar_float.fpcsr == 0x80
    assert caller_state(runtime) == before and runtime.drain_uart_output() == b""


def test_completed_service_effect_survives_a_later_validation_failure(runtime):
    result, error, first = perform(runtime, descriptor("FPCSR!", 0), (0x85,))
    assert result.outputs == () and error is None and first.completed
    result, error, second = perform(runtime, descriptor("F64+", 1), (ONE, TWO))
    assert result is None and second.failure.cause is error
    assert runtime.scalar_float.fpcsr == 0x85


@pytest.mark.parametrize("invalid", (False, True))
def test_service_uses_enclosing_semantic_meter_without_touching_caller_stacks(runtime, invalid):
    runtime.main_context.data.push(0xCAFE)
    runtime.main_context.returns.push(0xBEEF)
    before = caller_state(runtime)
    runtime.scalar_float.write_fpcsr(5 if invalid else 0)
    captured = []
    runtime.define_primitive("SERVICE-CONTROL", lambda _context: captured.append(
        perform(runtime, descriptor(), (ONE, TWO))))
    ticks = runtime.diagnostics.semantic_cycles
    result = runtime.execute("SERVICE-CONTROL", step_budget=2)
    service_result, error, receipt = captured[0]
    assert result.semantic_steps == 2 and runtime.diagnostics.semantic_cycles - ticks == 2
    assert receipt.semantic_steps == 1 and receipt.entered
    if invalid:
        assert service_result is None and receipt.failure.cause is error
    else:
        assert service_result.outputs == (THREE,) and error is None and receipt.completed
    # The outer primitive allocates no return continuation or data cell.
    assert caller_state(runtime) == before


@pytest.mark.parametrize("metadata", ("export", "request", "site"))
def test_late_metadata_validator_replacement_never_runs(runtime, monkeypatch, metadata):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    request = request_for(value, (ONE, TWO))
    calls = []

    def forbidden(_value):
        calls.append(True)
        raise AssertionError("replaced metadata validator was executed")

    kind = {"export": ServiceExportV5, "request": CallbackRequestV5, "site": CallbackSiteV5}[metadata]
    monkeypatch.setattr(kind, "__post_init__", forbidden)
    with pytest.raises(CallbackExportError, match="class route"):
        if metadata == "export":
            runtime.bind_callback_export(value)
        else:
            begin_service_callback_accounting(runtime, handle, request)
    assert calls == [] and runtime._callback_exports._closed_accounting is None


def test_service_registration_reuses_one_table_and_rolls_back_only_new_handles(runtime):
    integer = CallbackExportV2(export_id=0, name="MIN", input_cells=2, output_cells=1)
    existing = runtime.bind_callback_export(integer)
    service = descriptor(export_id=1)
    error = RuntimeError("publication failed")
    with pytest.raises(RuntimeError) as raised:
        with runtime.callback_export_registration((integer, service, service)) as handles:
            assert handles[0] is existing and handles[1] is handles[2]
            assert len(runtime._callback_exports._exports) == 2
            raise error
    assert raised.value is error
    assert runtime.verify_callback_export(existing) == integer
    with pytest.raises(CallbackExportError, match="issued identity"):
        runtime.verify_callback_export(handles[1])
    replacement = runtime.bind_callback_export(service)
    assert replacement is not handles[1]
    with pytest.raises(CallbackExportError, match="conflicting"):
        runtime.bind_callback_export(descriptor(export_id=0))
    for export_id in range(2, 64):
        runtime.bind_callback_export(descriptor("FPCSR@", export_id))
    assert len(runtime._callback_exports._exports) == 64
    assert runtime.invoke_callback_export(existing, (2, 1)).outputs == (1,)


def test_early_capture_keeps_original_word_when_name_is_shadowed(runtime):
    original = runtime.dictionary.find("F64+")
    calls = []
    runtime.define_primitive("F64+", lambda _context: calls.append(True))
    assert runtime.dictionary.find("F64+") is not original
    result, error, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert result.outputs == (THREE,) and error is None and receipt.completed
    assert runtime._callback_exports._exports[0].service.word is original
    assert calls == []


def test_catalog_is_finalized_once_and_direct_service_invocation_requires_request(runtime):
    with pytest.raises(CallbackExportError, match="already finalized"):
        runtime._callback_exports.finalize_service_executor(runtime._native_execution)
    handle = runtime.bind_callback_export(descriptor())
    ticks = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError, match="prepared request"):
        runtime.invoke_callback_export(handle, (ONE, TWO))
    assert runtime.diagnostics.semantic_cycles == ticks
    assert runtime._callback_exports._closed_accounting is None


def test_profile_reports_only_verified_actual_scalar_value_executor(runtime):
    profile = service_callback_profile(runtime)
    assert (profile.version, profile.capability, profile.effect) == (5, "private_scalar_fp_v1", "scalar_fp_state")
    assert profile.value_executor == ("python_reference" if runtime.execution_backend == "python"
                                      else "shared_native_kernel")
    assert runtime.callback_export_abi_version == 4
    with pytest.raises(FrozenInstanceError):
        profile.value_executor = "unproved"
    runtime.scalar_float._native_execute = lambda *_args: (0, 0, None)
    assert service_callback_profile(runtime) is None


def test_old_embedding_and_no_core_keep_construction_without_service_admission():
    class OldEmbedding(MegaForthRuntime):
        pass

    for value in (OldEmbedding(execution_backend="python"),
                  MegaForthRuntime(execution_backend="python", install_core_words=False)):
        try:
            assert service_callback_profile(value) is None
            with pytest.raises(CallbackExportError, match="private scalar"):
                value.bind_callback_export(descriptor())
        finally:
            value.memory.mmio.audio.release_host_sink()


def test_unexpected_capture_failure_is_not_swallowed(monkeypatch):
    error = MemoryError("catalog allocation failed")

    def fail(_cls, _runtime, *, core_installed):
        raise error

    monkeypatch.setattr(ScalarServiceCatalog, "capture", classmethod(fail))
    with pytest.raises(MemoryError) as raised:
        MegaForthRuntime(execution_backend="python")
    assert raised.value is error


@pytest.mark.parametrize("mutation", ("implementation", "owner", "kernel"))
def test_late_scalar_route_changes_fail_before_any_tick(runtime, mutation):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    calls = []
    forbidden = lambda *_args: calls.append(True)
    if mutation == "implementation":
        object.__setattr__(runtime.dictionary.find("F64+").implementation, "callback", forbidden)
    elif mutation == "owner":
        from simulator.scalar_float import HostedScalarFloatService
        runtime.scalar_float = HostedScalarFloatService()
    else:
        runtime.scalar_float._native_execute = forbidden
    ticks = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError):
        begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    assert runtime.diagnostics.semantic_cycles == ticks and calls == []
    assert runtime._callback_exports._closed_accounting is None


def test_checkpoint_copy_foreign_and_replay_do_not_consume_issued_accounting(runtime):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    checkpoint = begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    with pytest.raises(CallbackExportError, match="not issued"):
        consume_service_callback_accounting(runtime, copy(checkpoint), handle)
    foreign = MegaForthRuntime(execution_backend="python")
    try:
        with pytest.raises(CallbackExportError, match="not issued"):
            consume_service_callback_accounting(foreign, checkpoint, handle)
    finally:
        foreign.memory.mmio.audio.release_host_sink()
    with pytest.raises(CallbackExportError):
        runtime.consume_closed_callback_accounting(checkpoint, handle)
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle)
    assert receipt.semantic_steps == 0 and not receipt.entered and not receipt.completed
    assert receipt.failure is None and receipt.consumed_input_cells == 0
    with pytest.raises(CallbackExportError, match="not issued"):
        consume_service_callback_accounting(runtime, checkpoint, handle)


def test_prepared_request_is_copied_and_exact_arguments_are_required(runtime):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    request = request_for(value, (ONE, TWO))
    checkpoint = begin_service_callback_accounting(runtime, handle, request)
    object.__setattr__(request, "sequence", 99)
    object.__setattr__(request.site, "call_offset", 32)
    result = runtime.invoke_callback_export(handle, (ONE, TWO))
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle)
    assert result.outputs == (THREE,) and receipt.completed
    checkpoint = begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    with pytest.raises(CallbackExportError, match="prepared request"):
        runtime.invoke_callback_export(handle, (TWO, ONE))
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle)
    assert receipt.semantic_steps == 0 and not receipt.completed


def test_service_request_disallows_equality_protocol_before_argument_validation(runtime):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    checkpoint = begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    calls = []

    class Arguments:
        def __eq__(self, _other):
            calls.append(True)
            return True

    with pytest.raises(TypeError, match="exact tuple"):
        runtime.invoke_callback_export(handle, Arguments())
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle)
    assert calls == [] and receipt.semantic_steps == 0


def test_zero_semantic_allowance_consumes_no_tick_or_service_inputs(runtime):
    ticks = runtime.diagnostics.semantic_cycles
    result, error, receipt = perform(runtime, descriptor(), (ONE, TWO), semantic_step_limit=0)
    assert result is None and type(error) is CallbackExportBudgetExceeded
    assert receipt.semantic_steps == 0 and receipt.consumed_input_cells == 0
    assert receipt.failure is None and receipt.entered and not receipt.completed
    assert runtime.diagnostics.semantic_cycles == ticks and runtime.scalar_float.fpcsr == 0


@pytest.mark.parametrize("operation", ("bind", "register", "begin", "closed", "invoke"))
def test_pending_service_excludes_other_publication_and_accounting(runtime, operation):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    integer = runtime.bind_callback_export(CallbackExportV2(export_id=1, name="MIN", input_cells=2, output_cells=1))
    request = request_for(value, (ONE, TWO))
    checkpoint = begin_service_callback_accounting(runtime, handle, request)
    with pytest.raises(CallbackExportError):
        if operation == "bind":
            runtime.bind_callback_export(descriptor("FPCSR@", 2))
        elif operation == "register":
            with runtime.callback_export_registration((descriptor("FPCSR@", 2),)):
                pytest.fail("pending callback allowed publication")
        elif operation == "begin":
            begin_service_callback_accounting(runtime, handle, request)
        elif operation == "closed":
            runtime.begin_closed_callback_accounting(integer)
        else:
            runtime.invoke_callback_export(integer, (1, 2))
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle)
    assert receipt.semantic_steps == 0 and not receipt.entered


@pytest.mark.parametrize("projection", ("record", "guard", "slot", "_service_accounting_type", "_service_dispatch_type"))
def test_accounting_corruption_cannot_refund_real_tick_or_issue_a_fault(runtime, monkeypatch, projection):
    engine = runtime._callback_exports
    error = IllegalScalarFloatError("unrelated raw hook failure")
    replacements = []

    def forbidden(*_args, **_kwargs):
        replacements.append(True)
        raise AssertionError("replacement service constructor was called")

    def tick():
        if projection == "record":
            engine._closed_accounting.semantic_steps = 0
        elif projection == "guard":
            engine._active.charged_ticks = 0
        elif projection == "slot":
            engine._closed_accounting = object()
        else:
            setattr(engine, projection, forbidden)
        raise error

    monkeypatch.setattr(runtime, "_account_semantic_step", tick)
    result, escaped, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert result is None and escaped is error
    assert receipt.semantic_steps == 1 and receipt.failure is None and not receipt.completed
    assert engine._registration_failure is not None and engine._closed_accounting is None
    assert replacements == []


@pytest.mark.parametrize("projection", ("_service_accounting_type", "_service_dispatch_type"))
def test_replaced_service_type_is_never_called_before_begin(runtime, projection):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    calls = []

    def forbidden(*_args, **_kwargs):
        calls.append(True)
        raise AssertionError("replacement service constructor was called")

    setattr(runtime._callback_exports, projection, forbidden)
    with pytest.raises(CallbackExportError, match="type projections"):
        begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    assert calls == [] and runtime._callback_exports._closed_accounting is None


def test_wrong_error_identity_cannot_claim_canonical_validation(runtime):
    runtime.scalar_float.write_fpcsr(5)
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    checkpoint = begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    with pytest.raises(IllegalScalarFloatError) as raised:
        runtime.invoke_callback_export(handle, (ONE, TWO))
    with pytest.raises(CallbackExportError, match="validation issuance"):
        consume_service_callback_accounting(runtime, checkpoint, handle, IllegalScalarFloatError(str(raised.value)))
    assert runtime._callback_exports._registration_failure is not None
    receipt = consume_service_callback_accounting(runtime, checkpoint, handle, raised.value)
    assert receipt.semantic_steps == 1 and receipt.failure is None


def test_failure_consumption_does_not_enter_a_late_stack_method(runtime, monkeypatch):
    from simulator.stacks import DataStack

    runtime.scalar_float.write_fpcsr(5)
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    checkpoint = begin_service_callback_accounting(runtime, handle, request_for(value, (ONE, TWO)))
    with pytest.raises(IllegalScalarFloatError) as raised:
        runtime.invoke_callback_export(handle, (ONE, TWO))
    calls = []

    def forbidden(_stack):
        calls.append(True)
        raise AssertionError("late stack snapshot was executed")

    monkeypatch.setattr(DataStack, "snapshot", forbidden)
    with pytest.raises(CallbackExportError, match="class route"):
        consume_service_callback_accounting(runtime, checkpoint, handle, raised.value)
    assert calls == []


@pytest.mark.parametrize("kind", (CallbackExportResult, ServiceCallbackReceiptV5, ServiceCallbackFailureV5))
@pytest.mark.parametrize("route", ("__new__", "__init__", "__setattr__", "field"))
@pytest.mark.parametrize("outcome", ("return", "raw", "validation"))
def test_changed_issued_value_routes_cannot_construct_or_refund_work(runtime, monkeypatch, kind, route, outcome, request):
    if route == "__new__" and request is not None:
        # CPython can retain a changed tp_new slot after adding and deleting a
        # class-local __new__, even when the class dictionary looks restored.
        # Exercise that real mutation in a fresh process so later legacy
        # dataclass constructors are not corrupted by the test's teardown.
        # Invoke this same assertion body directly; no nested pytest session
        # or shared test-monitor state is started by the child.
        program = """
import runpy
import sys
values = runpy.run_path(sys.argv[1])
runtime = values['MegaForthRuntime'](execution_backend=sys.argv[2])
patch = values['pytest'].MonkeyPatch()
try:
    values['test_changed_issued_value_routes_cannot_construct_or_refund_work'](
        runtime, patch, values[sys.argv[3]], '__new__', sys.argv[4], None)
finally:
    runtime.memory.mmio.audio.release_host_sink()
    patch.undo()
"""
        result = subprocess.run(
            [sys.executable, "-c", program, str(Path(__file__).resolve()),
             runtime.execution_backend, kind.__name__, outcome],
            cwd=Path(__file__).resolve().parents[2], capture_output=True, text=True, timeout=30,
        )
        assert result.returncode == 0, result.stdout + result.stderr
        return
    receipt_fields = tuple(vars(ServiceCallbackReceiptV5)[name] for name in (
        "semantic_steps", "entered", "completed", "consumed_input_cells", "failure"))
    result_fields = tuple(vars(CallbackExportResult)[name] for name in ("outputs", "semantic_steps"))
    calls = []
    original_error = IllegalScalarFloatError("raw hook escape")

    def forbidden(*_args, **_kwargs):
        calls.append(True)
        raise AssertionError("rejected value construction or getter was called")

    def tick():
        name = ("fpcsr" if kind is ServiceCallbackFailureV5 else "semantic_steps") if route == "field" else route
        replacement = (property(forbidden) if route == "field" else
                       staticmethod(forbidden) if route == "__new__" else forbidden)
        monkeypatch.setattr(kind, name, replacement)
        if outcome == "raw":
            raise original_error
        if outcome == "validation":
            runtime.scalar_float.write_fpcsr(5)

    monkeypatch.setattr(runtime, "_account_semantic_step", tick)
    result, error, receipt = perform(runtime, descriptor(), (ONE, TWO))
    steps, entered, completed, consumed, failure = tuple(
        field.__get__(receipt, ServiceCallbackReceiptV5) for field in receipt_fields)
    assert (steps, entered, completed, failure) == (1, True, outcome == "return", None)
    assert consumed == (0 if outcome == "raw" else 2)
    if outcome == "return":
        assert error is None
        assert tuple(field.__get__(result, CallbackExportResult) for field in result_fields) == ((THREE,), 1)
    elif outcome == "raw":
        assert result is None and error is original_error
    else:
        assert result is None and type(error) is IllegalScalarFloatError
    assert calls == []
    assert runtime._callback_exports._registration_failure is not None
    assert runtime._callback_exports._closed_accounting is None


@pytest.mark.parametrize("route", ("__init__", "value_executor"))
def test_profile_constructor_and_getter_mutations_make_profile_unavailable(runtime, monkeypatch, route):
    calls = []

    def forbidden(*_args, **_kwargs):
        calls.append(True)
        raise AssertionError("changed profile route was called")

    monkeypatch.setattr(ServiceCallbackProfileV5, route,
                        property(forbidden) if route == "value_executor" else forbidden)
    assert service_callback_profile(runtime) is None
    assert calls == []


def test_normal_issued_value_copies_do_not_disable_later_service_metadata(runtime):
    result, error, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert error is None
    assert copy(result) == result and copy(receipt) == receipt
    profile = service_callback_profile(runtime)
    assert copy(profile) == profile
    runtime.scalar_float.write_fpcsr(5)
    _, error, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert receipt.failure.cause is error
    assert copy(receipt.failure).cause is error
    runtime.scalar_float.write_fpcsr(0)
    result, error, receipt = perform(runtime, descriptor(), (ONE, TWO))
    assert error is None and result.outputs == (THREE,) and receipt.completed
    assert service_callback_profile(runtime) == profile
