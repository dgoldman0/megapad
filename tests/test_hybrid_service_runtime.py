"""V5 lower bridge uses one real owner and exact private service receipts."""

from dataclasses import replace

import pytest

pytest.importorskip("_mp64_accel")

from asm import assemble
from hybrid.runtime import HybridExecutionError, HybridRuntime
from shared.hybrid_services import CallbackSiteV5, RoutineImageV5, ServiceExportV5
from simulator.errors import ForthAbort, IllegalInstructionFault, StepBudgetExceeded
from simulator.interop_exports import CallbackExportBudgetExceeded, CallbackExportError
from simulator.scalar_float import IllegalScalarFloatError


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


@pytest.fixture(params=("python", "native"))
def owner(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    runtime = HybridRuntime.create(executor=request.param,
                                  geometry={"bank0_size": 65536, "external_size": 65536})
    yield runtime
    runtime.close()


def image(operation="F64+", *, name="SERVICE", cap=1, tail="", buffered=False):
    _, arguments, outputs, _ = next(row for row in VECTORS if row[0] == operation)
    export = ServiceExportV5(export_id=0, name=operation,
                            input_cells=len(arguments), output_cells=len(outputs))
    program = ("mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\n"
               + tail + "\nret.l\nstub:\nret.l")
    labels = {}
    assemble(program, labels_out=labels)
    code = bytes(assemble(program.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")))
    return RoutineImageV5(name=name, code=code, entry_offset=0,
                          input_cells=len(arguments), output_cells=len(outputs), buffers=(),
                          return_stack_cells=16, max_instructions=100, max_callback_requests=cap,
                          callbacks=(CallbackSiteV5(call_offset=labels["call"],
                                                   stub_offset=labels["stub"], export=export),))


def publish(owner, value=None, *, public=False):
    if public:
        word = owner.register_routine_v5(image() if value is None else value)
        return word, word
    word = owner._publish_service_routine(image() if value is None else value)
    def enter(context):
        owner._invoke_published_service(word, context)
    entry = owner.semantic.define_primitive("PRIVATE-SERVICE-TEST", enter)
    owner.semantic._register_primitive_host_escape(entry.implementation, enter)
    return word, entry


def push(owner, arguments):
    context = owner.semantic.main_context
    for cell in arguments:
        context.data.push(cell)


@pytest.mark.parametrize("operation,arguments,outputs,fpcsr", VECTORS,
                         ids=[row[0] for row in VECTORS])
@pytest.mark.parametrize("public", (False, True))
def test_catalog_runs_through_real_transport_with_independent_bits(owner, operation, arguments, outputs, fpcsr, public):
    word, entry = publish(owner, image(operation), public=public)
    context = owner.semantic.main_context
    push(owner, (0xCAFE, *arguments))
    context.returns.push(19)
    before = owner.semantic.timer.counter
    report = owner.execute(entry.xt)
    assert context.data.snapshot() == (0xCAFE, *outputs)
    assert context.returns.snapshot() == (19,)
    assert owner.semantic.scalar_float.fpcsr == fpcsr
    assert (report.machine_instructions, report.machine_cycles, report.transitions,
            report.machine_segments, report.callback_requests, report.callback_semantic_steps) == (5, 8, 1, 2, 1, 1)
    assert report.semantic_result.semantic_steps == owner.semantic.timer.counter - before == 2
    assert owner.max_machine_depth == 1
    assert owner.registered_routines[0].version == 5
    assert owner._nested_runner is None or owner._runner is owner._nested_runner.legacy_v2()
    assert owner.declaration_for(word).max_callback_requests == 1
    assert owner.semantic._callback_exports._closed_accounting is None


def test_public_service_admission_requires_its_qualified_owner(owner, monkeypatch):
    assert owner.service_callback_abi_available is True
    assert owner.service_callback_value_executor == (
        "python_reference" if owner.executor == "python" else "shared_native_kernel")
    monkeypatch.setattr(HybridRuntime, "service_callback_abi_available", property(lambda self: False))
    assert owner.service_callback_abi_available is False
    assert owner.service_callback_value_executor is None
    with pytest.raises(RuntimeError, match="qualified"):
        owner.register_routine_v5(image())
    with pytest.raises(RuntimeError, match="qualified"):
        HybridRuntime.create(require_service_callbacks=True)
    with pytest.raises(TypeError, match="exact boolean"):
        HybridRuntime.create(require_service_callbacks=1)
    word = owner._publish_service_routine(image())
    push(owner, (1, 2))
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(word.xt)
    assert caught.value.reason == "service_unavailable"
    assert owner.machine_instructions == owner.callback_semantic_steps == 0
    assert owner.semantic.main_context.data.snapshot() == (1, 2)


@pytest.mark.parametrize("operation,arguments", (
    ("F64+", (0x3FF0000000000000, 0x4000000000000000)),
    ("F64SQRT", (0x4010000000000000,)),
    ("F64FMA", (0x3FF0000000000000, 0x4000000000000000, 0x4010000000000000)),
))
@pytest.mark.parametrize("rounding", (5, 6, 7))
def test_invalid_rounding_is_exact_service_fault_without_guest_abort(owner, operation, arguments, rounding):
    word, entry = publish(owner, image(operation))
    context = owner.semantic.main_context
    push(owner, (0xCAFE, *arguments))
    context.returns.push(19)
    owner.semantic.scalar_float.write_fpcsr(0x90 | rounding)
    before_uart = owner.semantic.uart_output
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(entry.xt)
    error = caught.value
    assert error.reason == "service_fault"
    assert type(error.__cause__) is IllegalScalarFloatError
    failure = error.service_failure
    assert failure.cause is error.__cause__
    assert (failure.name, failure.consumed_input_cells, failure.fpcsr,
            failure.semantic_steps, failure.fault_kind, failure.throw_code) == (
                operation, len(arguments), 0x90 | rounding, 1, "illegal_scalar_float", -21)
    assert (failure.invocation_id, failure.sequence, failure.call_offset, failure.stub_offset) == (
        error.result.callback.invocation_id, 1,
        owner.declaration_for(word).callbacks[0].call_offset,
        owner.declaration_for(word).callbacks[0].stub_offset)
    assert context.data.snapshot() == (0xCAFE, *arguments)
    assert context.returns.snapshot() == (19,)
    assert owner.semantic.uart_output == before_uart
    assert owner.semantic.scalar_float.fpcsr == 0x90 | rounding
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert not owner._active_machine
    assert owner.semantic._callback_exports._closed_accounting is None
    # An admitted guest validation failure does not poison a reusable owner.
    owner.semantic.scalar_float.write_fpcsr(0)
    assert owner.execute(entry.xt).callback_semantic_steps == 1


@pytest.mark.parametrize("kind", ("forth_abort", "instruction", "same_class", "memory", "budget"))
@pytest.mark.parametrize("public", (False, True))
def test_host_errors_never_acquire_service_fault_authority(owner, monkeypatch, kind, public):
    _, entry = publish(owner, public=public)
    context = owner.semantic.main_context
    push(owner, (7, 9))
    context.returns.push(19)
    error = (ForthAbort("host error") if kind == "forth_abort" else
             IllegalInstructionFault("host error") if kind == "instruction" else
             IllegalScalarFloatError("host error") if kind == "same_class" else
             MemoryError("host error") if kind == "memory" else
             CallbackExportBudgetExceeded("callback_semantic_limit", 99, 99))
    account = owner.semantic._account_semantic_step
    def fail():
        account()
        if owner.semantic._callback_exports._active_context is not None:
            raise error
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", fail)
    with pytest.raises(type(error)) as caught:
        owner.execute(entry.xt)
    assert caught.value is error
    assert context.data.snapshot() == (7, 9)
    assert context.returns.snapshot() == (19,)
    assert owner.callback_semantic_steps == 1
    assert owner.semantic.scalar_float.fpcsr == 0
    if kind == "forth_abort":
        assert error.origin_context is None
    assert owner.semantic._callback_exports._closed_accounting is None


def test_successful_fpcsr_write_survives_later_machine_failure(owner):
    _, entry = publish(owner, image("FPCSR!", tail="ldi64 r12, 0xFFFFFFFFFFFFF000\nstr r12, r4"))
    push(owner, (0x85,))
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(entry.xt)
    assert caught.value.reason == "rejected_access"
    assert owner.semantic.scalar_float.fpcsr == 0x85
    assert owner.semantic.main_context.data.snapshot() == (0x85,)
    assert owner.callback_semantic_steps == 1


def test_zero_local_callback_cap_stops_after_real_call(owner):
    _, entry = publish(owner, image(cap=0))
    push(owner, (1, 2))
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(entry.xt)
    assert caught.value.reason == "callback_limit"
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 0, 0)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)


def test_last_instruction_call_does_not_dispatch_service(owner):
    _, entry = publish(owner, replace(image(), max_instructions=3))
    push(owner, (1, 2))
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(entry.xt)
    assert caught.value.reason == "instruction_limit"
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 0)


@pytest.mark.parametrize("field", ("instructions", "callback_semantic_steps", "limits", "owner_counter"))
def test_hook_cannot_refund_service_allowance_or_counts(owner, monkeypatch, field):
    _, entry = publish(owner)
    push(owner, (1, 2))
    account = owner.semantic._account_semantic_step
    def corrupt():
        account()
        if owner.semantic._callback_exports._active_context is None:
            return
        allowance = owner._allowances[owner._current_meter()]
        if field == "limits":
            allowance.limit += 100
        elif field == "owner_counter":
            owner._machine_instructions = 0
        else:
            setattr(allowance, field, -100)
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", corrupt)
    with pytest.raises(HybridExecutionError):
        owner.execute(entry.xt)
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)
    assert owner._registration_failure is not None


def test_shadowed_service_name_does_not_retarget_captured_word(owner):
    _, entry = publish(owner)
    owner.semantic.define_primitive("F64+", lambda context: (_ for _ in ()).throw(AssertionError("shadow ran")))
    push(owner, (0x3FF0000000000000, 0x4000000000000000))
    assert owner.execute(entry.xt).callback_semantic_steps == 1
    assert owner.semantic.main_context.data.snapshot() == (0x4008000000000000,)


def test_source_calls_share_root_callback_work_allowance(owner):
    publish(owner)
    push(owner, (1, 2, 3, 4))
    with pytest.raises(HybridExecutionError) as caught:
        owner.evaluate("PRIVATE-SERVICE-TEST PRIVATE-SERVICE-TEST",
                       dispatch_callback_semantic_limit=1)
    assert caught.value.reason == "callback_semantic_limit"
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps,
            owner.transitions) == (8, 2, 1, 2)
    assert owner.semantic.main_context.data.snapshot() == (1, 2, 7)


@pytest.mark.parametrize("point", ("begin", "resume"))
def test_native_work_is_settled_after_post_return_interruption(owner, point):
    import sys

    _, entry = publish(owner, image("F64/"))
    push(owner, (0x3FF0000000000000, 0))
    error = MemoryError("interrupted after real native result")
    fired = False
    def interrupt(frame, event, argument):
        nonlocal fired
        if (not fired and event == "line" and frame.f_code.co_name == "boundary"
                and frame.f_code.co_filename.endswith("hybrid/runtime.py")
                and "raw" in frame.f_locals):
            raw = frame.f_locals["raw"]
            wanted = "callback_request" if point == "begin" else "returned"
            if raw.exit_kind == wanted:
                fired = True
                raise error
        return interrupt
    previous = sys.gettrace()
    try:
        sys.settrace(interrupt)
        with pytest.raises(MemoryError) as caught:
            owner.execute(entry.xt)
    finally:
        sys.settrace(previous)
    assert fired and caught.value is error
    assert owner.semantic.main_context.data.snapshot() == (0x3FF0000000000000, 0)
    assert owner.machine_instructions == (3 if point == "begin" else 5)
    assert owner.callback_requests == owner.transitions == 1
    assert owner.callback_semantic_steps == (0 if point == "begin" else 1)
    assert owner.semantic.scalar_float.fpcsr == (0 if point == "begin" else 0x80)
    assert owner._active_machine is False


def test_foreign_numeric_result_cannot_enter_service_via_runner_wrapper(owner, monkeypatch):
    word, entry = publish(owner, image("FPCSR!"))
    other = HybridRuntime.create(executor=owner.executor,
                                 geometry={"bank0_size": 65536, "external_size": 65536})
    try:
        other_word = other._publish_service_routine(image("FPCSR!"))
        foreign = other._runner.begin_v2(other._registrations[id(other_word)].spec,
                                         (0x85,), (), 100)
        assert foreign.exit_kind == "callback_request"
        real = owner._runner
        calls = []
        class Forward:
            def __getattr__(self, name):
                return getattr(real, name)
            def begin_v2(self, *args, **kwargs):
                calls.append("begin")
                real.begin_v2(*args, **kwargs)
                return foreign
        monkeypatch.setattr(owner, "_runner", Forward())
        push(owner, (0,))
        with pytest.raises(RuntimeError, match="exact native facade"):
            owner.execute(entry.xt)
        assert calls == []
        assert owner.semantic.scalar_float.fpcsr == 0
        assert owner.machine_instructions == owner.callback_semantic_steps == 0
        assert owner.semantic.main_context.data.snapshot() == (0,)
    finally:
        other._runner.cancel_invocation()
        other.close()


def test_forwarded_service_failure_preserves_original_after_completed_tick(owner, monkeypatch):
    _, entry = publish(owner)
    push(owner, (1, 2))
    invoke = owner.semantic.invoke_callback_export
    error = ForthAbort("host wrapper after real scalar effect")
    def forward(*args, **kwargs):
        invoke(*args, **kwargs)
        raise error
    monkeypatch.setattr(owner.semantic, "invoke_callback_export", forward)
    with pytest.raises(ForthAbort) as caught:
        owner.execute(entry.xt)
    assert caught.value is error and error.origin_context is None
    assert owner.semantic.main_context.data.snapshot() == (1, 2)
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)


def test_meter_repair_cannot_become_successful_service_return(owner, monkeypatch):
    _, entry = publish(owner)
    push(owner, (1, 2))
    invoke = owner.semantic.invoke_callback_export
    def forward(*args, **kwargs):
        result = invoke(*args, **kwargs)
        owner._current_meter().steps = 0
        return result
    monkeypatch.setattr(owner.semantic, "invoke_callback_export", forward)
    with pytest.raises((RuntimeError, CallbackExportError, HybridExecutionError)):
        owner.execute(entry.xt)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner.semantic._callback_exports._registration_failure is not None


@pytest.mark.parametrize("raw_error", (False, True))
def test_replaced_receipt_constructor_cannot_forge_zero_work(owner, monkeypatch, raw_error):
    from simulator.interop_exports import ServiceCallbackReceiptV5

    _, entry = publish(owner)
    push(owner, (1, 2))
    error = ForthAbort("host error with replaced receipt constructor")
    calls = []
    account = owner.semantic._account_semantic_step
    def forged(self, *args, **kwargs):
        calls.append("constructor")
        object.__setattr__(self, "semantic_steps", 0)
    def corrupt():
        account()
        if owner.semantic._callback_exports._active_context is not None:
            monkeypatch.setattr(ServiceCallbackReceiptV5, "__init__", forged)
            if raw_error:
                raise error
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", corrupt)
    with pytest.raises(BaseException) as caught:
        owner.execute(entry.xt)
    if raw_error:
        assert caught.value is error and error.origin_context is None
    assert calls == []
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)
    assert owner._registration_failure is not None


def test_rejected_meter_hash_route_is_never_called_during_settlement(owner, monkeypatch):
    from simulator.runtime import _StepMeter

    _, entry = publish(owner)
    push(owner, (1, 2))
    calls = []
    account = owner.semantic._account_semantic_step
    def bad_hash(self):
        calls.append("hash")
        raise AssertionError("rejected meter route was called")
    def corrupt():
        account()
        if owner.semantic._callback_exports._active_context is not None:
            monkeypatch.setattr(_StepMeter, "__hash__", bad_hash)
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", corrupt)
    with pytest.raises(CallbackExportError):
        owner.execute(entry.xt)
    assert calls == []
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)


def test_allowance_namespace_replacement_is_repaired_without_refunding_work(owner, monkeypatch):
    from hybrid.runtime import _MachineAllowance

    _, entry = publish(owner)
    push(owner, (1, 2))
    account = owner.semantic._account_semantic_step
    original = owner._allowances.__dict__
    def corrupt():
        account()
        if owner.semantic._callback_exports._active_context is None:
            return
        current = owner._allowances[owner._current_meter()]
        copied = dict(original)
        copied["data"] = {key: _MachineAllowance(current.limit, current.callback_limit,
                                                current.callback_semantic_limit)
                          for key in original["data"]}
        owner._allowances.__dict__ = copied
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", corrupt)
    with pytest.raises(HybridExecutionError):
        owner.execute(entry.xt)
    assert owner._allowances.__dict__ is original
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner._registration_failure is not None


def test_changed_native_authority_stops_before_any_resume_instruction(owner, monkeypatch):
    _, entry = publish(owner)
    push(owner, (1, 2))
    account = owner.semantic._account_semantic_step
    def corrupt():
        account()
        if owner.semantic._callback_exports._active_context is not None:
            owner._service_native_authority = tuple(list(owner._service_native_authority))
    monkeypatch.setattr(owner.semantic, "_account_semantic_step", corrupt)
    with pytest.raises(HybridExecutionError) as caught:
        owner.execute(entry.xt)
    assert caught.value.reason == "invalid_callback"
    assert (owner.machine_instructions, owner.callback_requests, owner.callback_semantic_steps) == (3, 1, 1)
    assert owner.semantic.main_context.data.snapshot() == (1, 2)
    assert owner._registration_failure is not None
