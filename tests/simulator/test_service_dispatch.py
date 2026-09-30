"""Exercise the isolated V5 guard through the real private runtime frame.

Fixture-only bindings/accounting records do not register a V5 capability.
The eventual engine must issue/consume these identities itself.
"""

from __future__ import annotations

from types import SimpleNamespace
import sys

import pytest

from shared.hybrid_services import CallbackRequestV5, CallbackSiteV5, ServiceExportV5
from simulator.errors import ForthAbort, IllegalInstructionFault, StepBudgetExceeded
from simulator.interop_closed import capture_meter
from simulator.interop_exports import CallbackExportError, CallbackExportBudgetExceeded, CallbackExportHandle
from simulator.interop_services import ScalarServiceCatalog, ServiceDispatch, _ServiceAccounting
from simulator.memory import SparseAddressSpace
from simulator.runtime import ExecutionContext, MegaForthRuntime, _StepMeter
from simulator.scalar_float import IllegalScalarFloatError
from simulator.stacks import DataStack, ReturnStack


SIGNATURES = {"FPCSR@": (0, 1), "FPCSR!": (1, 0), "F64+": (2, 1),
              "F64/": (2, 1), "F64SQRT": (1, 1), "F64FMA": (3, 1)}
ONE, TWO, THREE, FOUR, SIX = (
    0x3FF0000000000000, 0x4000000000000000, 0x4008000000000000,
    0x4010000000000000, 0x4018000000000000,
)


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return MegaForthRuntime(execution_backend=request.param)


def _caller_state(runtime):
    context = runtime.main_context
    return (context.data.snapshot(), context.returns.snapshot(),
            context.data.pointer, context.returns.pointer,
            context.returns._continuation_cookie, context.returns._pointer_capture_generation)


class DispatchFixture:
    def __init__(self, runtime, name, arguments, *, on_tick=None, budget=1, semantic_limit=None,
                 initial_steps=0):
        self.runtime = runtime
        self.engine = runtime._callback_exports
        catalog = ScalarServiceCatalog.capture(runtime, core_installed=True)
        catalog.finalize_executor(runtime._native_execution)
        inputs, outputs = SIGNATURES[name]
        descriptor = ServiceExportV5(export_id=0, name=name, input_cells=inputs, output_cells=outputs)
        self.capture = catalog.bind(descriptor)
        handle = CallbackExportHandle(0, self.engine._owner)
        self.binding = SimpleNamespace(handle=handle, descriptor=descriptor, service=self.capture,
                                       closed=None, leaf=None)
        request = CallbackRequestV5(invocation_id=1, sequence=1,
                                   site=CallbackSiteV5(call_offset=0, stub_offset=8, export=descriptor),
                                   arguments=arguments)
        memory = SparseAddressSpace(bank0_size=128, page_size=128)
        memory.write8(0, 0)
        self.context = ExecutionContext(
            data=DataStack(arguments, memory=memory, floor=0, empty_pointer=64),
            returns=ReturnStack(memory=memory, floor=64, empty_pointer=128),
        )
        self.meter = _StepMeter(budget, on_tick or runtime._account_semantic_step)
        self.meter.steps = initial_steps
        namespace, starting = capture_meter(self.meter)
        self.record = _ServiceAccounting(token=object(), handle=handle, binding=self.binding,
                                         request=request, meter=self.meter, namespace=namespace,
                                         starting_steps=starting, entered=True)
        self.semantic_limit = semantic_limit
        self.active = None

    def run(self):
        previous_record, previous_active = self.engine._closed_accounting, self.engine._active
        self.engine._closed_accounting = self.record
        try:
            self.active = ServiceDispatch(self.engine, self.binding, self.context,
                                          self.meter, self.semantic_limit)
            self.engine._active = self.active
            prepare_unwind, cleanup_failed = self.active.prepare_unwind, self.active.cleanup_failed
            try:
                self.runtime._execute_guarded(self.capture.word, self.context, self.meter,
                                              closed_guard=self.active)
                self.active.require_state()
            except BaseException as error:
                try:
                    prepare_unwind(error)
                except BaseException:
                    cleanup_failed(error)
                raise
            return self.context.data.snapshot()
        finally:
            self.engine._active = previous_active
            self.engine._closed_accounting = previous_record


@pytest.mark.parametrize("name,arguments,outputs,fpcsr", (
    ("FPCSR@", (), (0,), 0),
    ("FPCSR!", ((1 << 64) - 1,), (), 0x1F7),
    ("F64+", (ONE, TWO), (THREE,), 0),
    ("F64SQRT", (FOUR,), (TWO,), 0),
    ("F64FMA", (ONE, TWO, FOUR), (SIX,), 0),
    ("F64/", (ONE, 0), (0x7FF0000000000000,), 0x80),
))
def test_service_dispatch_uses_one_actual_tick_and_private_frame(runtime, name, arguments, outputs, fpcsr):
    runtime.main_context.data.push(0xCAFE)
    runtime.main_context.returns.push(0xBEEF)
    before = _caller_state(runtime)
    ticks = runtime.diagnostics.semantic_cycles
    fixture = DispatchFixture(runtime, name, arguments)
    assert fixture.run() == outputs
    assert fixture.active.completed
    assert fixture.meter.steps == fixture.record.semantic_steps == 1
    assert runtime.diagnostics.semantic_cycles - ticks == 1
    assert fixture.record.consumed_input_cells == len(arguments)
    assert fixture.record.validation_failure is None and fixture.record.error is None
    assert fixture.record.scope is fixture.active._scope
    assert runtime.scalar_float.fpcsr == fpcsr
    assert _caller_state(runtime) == before
    assert runtime._active_dispatches == []
    assert runtime._callback_exports._active is None
    assert runtime.scalar_float._validation_scope is None
    assert runtime.scalar_float._validation_armed is None
    with pytest.raises(TypeError, match="admitted callback"):
        runtime.bind_callback_export(fixture.binding.descriptor)


@pytest.mark.parametrize("name,arguments,operation", (
    ("F64+", (ONE, TWO), 0x40), ("F64SQRT", (FOUR,), 0x44),
    ("F64FMA", (ONE, TWO, FOUR), 0x47),
))
@pytest.mark.parametrize("mode", (5, 6, 7))
def test_service_dispatch_validation_failure_follows_tick_and_all_operand_pops(runtime, name, arguments, operation, mode):
    runtime.main_context.data.push(0xCAFE)
    runtime.main_context.returns.push(0xBEEF)
    before = _caller_state(runtime)
    runtime.scalar_float.write_fpcsr(0x90 | mode)
    fixture = DispatchFixture(runtime, name, arguments)
    with pytest.raises(IllegalScalarFloatError) as raised:
        fixture.run()
    assert fixture.record.error is raised.value
    witness = fixture.record.validation_failure
    assert witness is fixture.record.scope.failure
    assert witness.cause is raised.value
    assert (witness.operation, witness.fpcsr) == (operation, 0x90 | mode)
    assert fixture.record.consumed_input_cells == len(arguments)
    assert fixture.context.data.snapshot() == ()
    assert fixture.context.returns.snapshot() == ()
    assert fixture.record.semantic_steps == fixture.meter.steps == 1
    assert runtime.scalar_float.fpcsr == 0x90 | mode
    assert runtime.drain_uart_output() == b""
    assert _caller_state(runtime) == before
    assert runtime._active_dispatches == []
    assert runtime.scalar_float._validation_scope is None


def test_service_dispatch_tick_can_change_live_fpcsr_before_validation(runtime):
    events = []

    def tick():
        events.append("tick")
        assert runtime.scalar_float._validation_armed is None
        runtime.scalar_float.write_fpcsr(5)

    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=tick)
    with pytest.raises(IllegalScalarFloatError) as raised:
        fixture.run()
    assert events == ["tick"]
    assert fixture.record.semantic_steps == 1
    assert fixture.record.validation_failure.cause is raised.value
    assert fixture.record.validation_failure.fpcsr == 5
    assert fixture.record.consumed_input_cells == 2


@pytest.mark.parametrize("kind", (IllegalScalarFloatError, IllegalInstructionFault,
                                 ForthAbort, MemoryError, KeyboardInterrupt, RuntimeError))
def test_service_dispatch_raw_tick_errors_keep_original_identity_and_no_fault_receipt(runtime, kind):
    error = kind("original host escape")
    before = _caller_state(runtime)

    def tick():
        assert runtime.scalar_float._validation_armed is None
        runtime.scalar_float.write_fpcsr(0x80)
        raise error

    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=tick)
    with pytest.raises(kind) as raised:
        fixture.run()
    assert raised.value is error
    assert fixture.record.error is error
    assert fixture.record.semantic_steps == fixture.meter.steps == 1
    assert fixture.record.scope is None and fixture.record.validation_failure is None
    assert fixture.record.consumed_input_cells == 0
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert runtime.scalar_float.fpcsr == 0x80
    assert runtime.drain_uart_output() == b""
    assert _caller_state(runtime) == before
    assert runtime._active_dispatches == []


@pytest.mark.parametrize("mutation", ("kernel", "private_data", "guard_method", "frame_list", "request_sequence",
                                     "_observe_escape", "cleanup_failed", "after_tick", "require_state",
                                     "engine_owner", "engine_budget"))
def test_service_dispatch_late_tick_mutations_stop_before_original_primitive(runtime, mutation):
    fixture = None
    calls = []

    def forbidden(*_args, **_kwargs):
        calls.append(True)
        raise AssertionError("late replacement must not run")

    def tick():
        if mutation == "kernel":
            runtime.scalar_float._native_execute = forbidden
        elif mutation == "private_data":
            fixture.context.data.pop()
        elif mutation == "guard_method":
            fixture.active.invoke_primitive = forbidden
        elif mutation == "request_sequence":
            object.__setattr__(fixture.record.request, "sequence", 2)
        elif mutation in ("_observe_escape", "cleanup_failed", "after_tick", "require_state"):
            setattr(fixture.active, mutation, forbidden)
        elif mutation == "engine_owner":
            fixture.engine._require_owner = forbidden
        elif mutation == "engine_budget":
            fixture.engine._budget_error = forbidden
        else:
            runtime._active_dispatches = []

    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=tick)
    before = _caller_state(runtime)
    with pytest.raises(CallbackExportError):
        fixture.run()
    assert fixture.record.semantic_steps == 1
    assert fixture.record.scope is None and fixture.record.validation_failure is None
    assert not fixture.active._primitive_entered
    assert calls == []
    assert _caller_state(runtime) == before
    assert runtime._active_dispatches == []
    assert runtime.drain_uart_output() == b""


@pytest.mark.parametrize("projection", ("record", "guard", "meter"))
@pytest.mark.parametrize("raises", (False, True))
def test_service_dispatch_tick_receipt_repairs_corrupted_projections_without_refund(runtime, projection, raises):
    fixture = None
    error = IllegalScalarFloatError("original tick error")

    def tick():
        if projection == "record":
            fixture.record.semantic_steps = 0
        elif projection == "guard":
            fixture.active.charged_ticks = 0
        else:
            fixture.meter.steps = 0
        if raises:
            raise error

    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=tick)
    with pytest.raises(IllegalScalarFloatError if raises else CallbackExportError) as raised:
        fixture.run()
    if raises:
        assert raised.value is error
    assert fixture.record.semantic_steps == fixture.active.charged_ticks == fixture.meter.steps == 1
    assert fixture.record.scope is None and fixture.record.validation_failure is None
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert runtime._callback_exports._registration_failure is not None


def test_service_dispatch_escape_after_meter_publication_cannot_refund_tick(runtime):
    hooks = []
    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=lambda: hooks.append(True))
    error = KeyboardInterrupt("after publication before local receipt")
    triggered = []
    previous_trace = sys.gettrace()

    def trace(frame, event, argument):
        if frame.f_code is ServiceDispatch.tick.__code__ and event == "line" and not triggered:
            values = frame.f_locals
            namespace = values.get("meter_namespace")
            if (type(namespace) is dict and namespace.get("steps") == 1
                    and values.get("charged") is False):
                triggered.append(True)
                raise error
        return trace

    try:
        sys.settrace(trace)
        with pytest.raises(KeyboardInterrupt) as raised:
            fixture.run()
    finally:
        sys.settrace(previous_trace)
    assert raised.value is error and triggered == [True]
    assert hooks == []
    assert fixture.record.semantic_steps == fixture.active.charged_ticks == fixture.meter.steps == 1
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert fixture.record.scope is None and fixture.record.validation_failure is None


@pytest.mark.parametrize("initial_steps,limit,kind", (
    (1, None, StepBudgetExceeded), (0, 0, CallbackExportBudgetExceeded),
))
def test_service_dispatch_exhaustion_has_zero_ticks_and_zero_consumed_inputs(runtime, initial_steps, limit, kind):
    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), initial_steps=initial_steps, semantic_limit=limit)
    with pytest.raises(kind):
        fixture.run()
    assert fixture.meter.steps == initial_steps
    assert fixture.record.semantic_steps == 0
    assert fixture.record.consumed_input_cells == 0
    assert fixture.record.validation_failure is None and fixture.record.scope is None
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert runtime.scalar_float.fpcsr == 0


@pytest.mark.parametrize("operation", ("evaluate", "execute", "nested_accounting"))
def test_service_dispatch_tick_cannot_enter_public_semantics_or_another_profile(runtime, operation):
    fixture = None

    def tick():
        if operation == "evaluate":
            runtime.evaluate(b"1 2 +")
        elif operation == "execute":
            runtime.execute("FPCSR@")
        else:
            runtime.begin_closed_callback_accounting(fixture.binding.handle)

    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO), on_tick=tick)
    before = _caller_state(runtime)
    with pytest.raises(CallbackExportError):
        fixture.run()
    assert fixture.record.semantic_steps == 1
    assert fixture.record.consumed_input_cells == 0
    assert fixture.record.scope is None and fixture.record.validation_failure is None
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert _caller_state(runtime) == before


@pytest.mark.parametrize("foreign", (False, True))
def test_service_dispatch_rejects_nonempty_or_foreign_private_return_control_before_tick(runtime, foreign):
    fixture = DispatchFixture(runtime, "F64+", (ONE, TWO))
    if foreign:
        fixture.context.returns._foreign_control = object()
    else:
        fixture.context.returns.push(123)
    with pytest.raises(CallbackExportError):
        fixture.run()
    assert fixture.record.semantic_steps == fixture.meter.steps == 0
    assert fixture.context.data.snapshot() == (ONE, TWO)
    assert runtime.scalar_float.fpcsr == 0
