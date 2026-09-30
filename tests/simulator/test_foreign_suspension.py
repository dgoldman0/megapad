"""Task callback IDL retains the original dispatcher, stacks and authority."""

from dataclasses import replace
from pathlib import Path

import pytest

from shared.cells import u64
from shared.foreign_abi import ForeignSpanV1
from simulator.errors import ExecutionError, ForthAbort, StepBudgetExceeded
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_runtime import ForeignTaskBudgetExceeded, ForeignTaskError
from simulator.ir import Idle, IdleUntil, Literal, Return as SemanticReturn
from simulator.memory import EXTERNAL_BASE
from simulator.rtc import HostedRTCService
from simulator.runtime import BlockedExecution, ExecutionResult, IdleWake, YieldedExecution
from simulator.stacks import StackUnderflow
from tests.simulator.foreign_reference import Callback, Reply, Return, Store
from tests.simulator.test_foreign_dispatch import (
    ObservedAdapter, adapter, assert_finished, capture, runtime, signature,
)


IDLE_FIXTURE = Path(__file__).with_name("fixtures") / "kdos-idle-2791-2805.f"


class SuspensionAdapter:
    """Observe real adapter boundaries; all guest control stays in Forth."""

    def __init__(self, inner, result):
        self.inner, self.result = inner, result
        self.validations = []
        self.cancellations = []
        self.validation_result = True
        self.validation_error = None
        self.replace_receipt = False
        self.cancel_error = None

    def begin(self, *args, **kwargs):
        return self.inner.begin(*args, **kwargs)

    def advance(self, *args, **kwargs):
        return self.inner.advance(*args, **kwargs)

    def reply(self, *args, **kwargs):
        return self.inner.reply(*args, **kwargs)

    def cancel_suffix(self, *args, **kwargs):
        return self.inner.cancel_suffix(*args, **kwargs)

    def cancel_all(self):
        context = self.result.main_context
        self.cancellations.append((context.suspended, context.returns.pointer,
                                   context.returns.snapshot(), self.inner.last_receipt()))
        outcome = self.inner.cancel_all()
        if self.cancel_error is not None:
            raise self.cancel_error
        return outcome

    def last_receipt(self):
        return self.inner.last_receipt()

    def validate_parked(self, root_token, operation_token, request_token=None):
        before = self.inner.last_receipt()
        valid = self.inner.validate_parked(root_token, operation_token, request_token)
        assert valid is True and self.inner.last_receipt() is before
        self.validations.append((root_token, operation_token, request_token, before,
                                 self.result.main_context.data.snapshot()))
        if self.validation_error is not None:
            raise self.validation_error
        if self.replace_receipt:
            self.inner._last = replace(before)
        return self.validation_result


def setup(*, source=b": PAUSE 7 IDLE 8 IDLE 9 ;", operations=None,
          outputs=3, missing_validation=False, prefix=False, exceptions=False,
          configure_clock=None, dynamic_names=()):
    result = runtime(exceptions=exceptions)
    result.evaluate(IDLE_FIXTURE.read_bytes(), source_name=str(IDLE_FIXTURE))
    if configure_clock is not None:
        configure_clock(result)
    if operations is None:
        result.evaluate(source)
        callback = result.find("PAUSE")
    else:
        callback = result.define_colon("PAUSE", operations)
    inner = adapter(result)
    observed = (ObservedAdapter(inner, result.main_context) if missing_validation
                else SuspensionAdapter(inner, result))
    export = capture(result, callback, outputs=outputs,
                     dynamic=tuple(result.find(name) for name in dynamic_names))
    script = ((Store(address=EXTERNAL_BASE, data=b"P"),) if prefix else ())
    script += (Callback(export=export, arguments=()),
               Return(outputs=tuple(Reply(index) for index in range(outputs))))
    grants = ((ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),) if prefix else ())
    operation = inner.register(signature(0, outputs), script, machine_grants=grants)
    word = result._foreign_tasks.define_operation("MACHINE", observed, operation)
    return result, inner, observed, word, callback, export


def wake(result, blocked, kind=IdleWake.INTERRUPT):
    receipt = result.deliver_idle_wake(blocked.suspension, kind)
    return result.resume(blocked.suspension, receipt)


def blocked_state(result):
    suspended = result._suspended_execution
    assert suspended is not None
    task = result._foreign_tasks._task_root
    assert suspended.task_root is task and task is not None
    return suspended, task, task.ledger.meter


def test_unchanged_kdos_idle_retains_original_stacks_meter_root_and_pending_request():
    result, inner, observed, word, _callback, _export = setup()
    context = result.main_context
    data, returns = context.data, context.returns
    data.push(99)
    returns.push(0xA5)
    first = result.run_until_blocked(word.xt, step_budget=40)
    assert type(first) is BlockedExecution and first.semantic_steps == 4
    suspended, task, meter = blocked_state(result)
    assert suspended.context is context and meter.budget == 40
    frame, = task.frames
    request, token, cookie = frame.request, frame.token, frame.cookie
    assert type(cookie) is ForeignContinuation and not cookie.retired
    raw_cookie = result.memory.read64(cookie.slot_address)
    native_receipt = inner.last_receipt()
    assert data.snapshot() == (99, 7) and context.suspended and not context.reusable
    assert native_receipt.root_instructions == native_receipt.root_callbacks == 1
    assert observed.validations and observed.validations[-1][1:3] == (token, request.request_token)

    second = wake(result, first)
    assert type(second) is BlockedExecution and second.semantic_steps == 8
    second_suspended, second_task, second_meter = blocked_state(result)
    assert second_task is task and second_meter is meter and second_suspended.context is context
    assert task.frames[0] is frame and frame.request is request and frame.token is token
    assert frame.cookie is cookie and not cookie.retired
    assert result.memory.read64(cookie.slot_address) == raw_cookie == cookie.raw_cookie
    assert inner.last_receipt() is native_receipt and inner.replies == ()
    assert data.snapshot() == (99, 7, 8)

    completed = wake(result, second, IdleWake.DMA)
    assert type(completed) is ExecutionResult and completed.semantic_steps == 11
    assert context.data is data and context.returns is returns
    assert data.snapshot() == (99, 7, 8, 9) and returns.snapshot() == (0xA5,)
    assert cookie.retired and result.memory.read64(cookie.slot_address) == raw_cookie
    assert inner.replies == ((1, 1, (7, 8, 9)),) and inner.active_invocations == ()
    report = result._foreign_tasks.last_dispatch
    assert (report.machine_instructions, report.callbacks, report.entries, report.semantic_steps) == (2, 1, 1, 10)
    assert report.root_id == task.ledger.root_id and report.completed
    assert context.reusable and result._suspended_execution is None


def test_copied_wrong_and_replayed_wakes_do_not_query_adapter_or_spend_work():
    result, inner, observed, word, _callback, _export = setup()
    first = result.run_until_blocked(word.xt)
    _suspended, task, meter = blocked_state(result)
    receipt, steps = inner.last_receipt(), meter.steps
    queries = len(observed.validations)
    with pytest.raises(ExecutionError):
        result.deliver_idle_wake(replace(first.suspension), IdleWake.INTERRUPT)
    with pytest.raises(ExecutionError):
        result.resume_yielded(first.suspension)
    issued = result.deliver_idle_wake(first.suspension, IdleWake.INTERRUPT)
    with pytest.raises(ExecutionError):
        result.resume(first.suspension, replace(issued))
    with pytest.raises(ExecutionError):
        result.deliver_idle_wake(first.suspension, IdleWake.DMA)
    assert inner.last_receipt() is receipt and meter.steps == steps
    assert len(observed.validations) == queries and result._foreign_tasks._task_root is task
    second = result.resume(first.suspension, issued)
    with pytest.raises(ExecutionError):
        result.resume(first.suspension, issued)
    with pytest.raises(ExecutionError):
        result.resume(second.suspension, issued)
    assert type(wake(result, second)) is ExecutionResult
    assert_finished(result, inner)


@pytest.mark.parametrize("limit", ["semantic", "public_meter"])
def test_wake_keeps_original_exhausted_semantic_allowance(limit):
    result, inner, _observed, word, _callback, _export = setup(
        operations=(Idle(), Literal(7), SemanticReturn()), outputs=1)
    if limit == "semantic":
        result._foreign_tasks.configure_limits(semantic_limit=1)
    blocked = result.run_until_blocked(word.xt, step_budget=2 if limit == "public_meter" else 30)
    assert type(blocked) is BlockedExecution and result.main_context.data.snapshot() == ()
    expected = ForeignTaskBudgetExceeded if limit == "semantic" else StepBudgetExceeded
    with pytest.raises(expected):
        wake(result, blocked)
    assert result.main_context.data.snapshot() == () and inner.replies == ()
    assert inner.last_receipt().root_instructions == 1
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert_finished(result, inner)


def test_empty_machine_chain_idle_keeps_root_spending_without_optional_validator():
    result = runtime()
    result.evaluate(IDLE_FIXTURE.read_bytes(), source_name=str(IDLE_FIXTURE))
    inner = adapter(result)
    observed = ObservedAdapter(inner, result.main_context)
    operation = inner.register(signature(), (Return(outputs=()),))
    result._foreign_tasks.define_operation("MACHINE", observed, operation)
    result.evaluate(b": OUTER MACHINE IDLE MACHINE ;")
    result._foreign_tasks.configure_limits(instruction_limit=1)
    first = result.run_until_blocked("OUTER", quantum_steps=1)
    retained_task = retained_meter = None
    for _ in range(12):
        if result._foreign_tasks._task_root is not None:
            _suspended, task, meter = blocked_state(result)
            if retained_task is None:
                retained_task, retained_meter = task, meter
            assert task is retained_task and meter is retained_meter
        if type(first) is BlockedExecution:
            break
        assert type(first) is YieldedExecution
        first = result.resume_yielded(first.suspension)
    else:
        pytest.fail("ordinary IDL was never reached")
    assert inner.active_invocations == () and inner.last_receipt().root_instructions == 1
    with pytest.raises(ForeignTaskBudgetExceeded):
        current = wake(result, first)
        for _ in range(12):
            assert type(current) is YieldedExecution
            _suspended, task, meter = blocked_state(result)
            assert task is retained_task and meter is retained_meter
            current = result.resume_yielded(current.suspension)
        pytest.fail("a wake renewed the exhausted original root")
    assert inner.admitted_inputs == ((1, ()),)
    assert result._foreign_tasks.last_dispatch.root_id == retained_task.ledger.root_id
    assert_finished(result, inner)


def test_empty_machine_chain_ordinary_deadline_uses_rtc_without_callback_capture():
    result = runtime()
    inner = adapter(result)
    observed = ObservedAdapter(inner, result.main_context)
    operation = inner.register(signature(), (Return(outputs=()),))
    result._foreign_tasks.define_operation("MACHINE", observed, operation)
    result.evaluate(b": OUTER MACHINE 5 IDLE-UNTIL 7 ;")
    first = result.run_until_blocked("OUTER")
    assert type(first) is BlockedExecution and result.idle_deadline_ms == 5
    _suspended, task, meter = blocked_state(result)
    assert task.frames == [] and task.tail is None and inner.active_invocations == ()
    receipt = inner.last_receipt()
    result.rtc.advance_uptime_ms(5)
    assert result.idle_wake_due
    assert type(wake(result, first)) is ExecutionResult
    assert result.main_context.data.snapshot() == (7,) and result.idle_deadline_ms is None
    assert inner.last_receipt() is receipt and task.ledger.meter is meter
    report = result._foreign_tasks.last_dispatch
    assert report.root_id == task.ledger.root_id and report.machine_instructions == 1
    assert report.semantic_steps == 0 and report.completed
    assert_finished(result, inner)


def test_real_local_catch_survives_idle_and_replies_with_the_guest_throw_code():
    result, inner, _observed, word, _callback, _export = setup(
        source=b": INNER IDLE -17 THROW ; : PAUSE ['] INNER CATCH ;",
        outputs=1, exceptions=True, dynamic_names=("INNER",))
    result.main_context.data.push(99)
    first = result.run_until_blocked(word.xt)
    _suspended, task, _meter = blocked_state(result)
    cookie = task.frames[-1].cookie
    assert not cookie.retired and result.memory.read64(cookie.slot_address) == cookie.raw_cookie
    assert result.memory.read64(result.find("_TASK-HANDLERS").body_address) != 0
    assert type(wake(result, first)) is ExecutionResult
    assert result.main_context.data.snapshot() == (99, u64(-17))
    assert inner.replies == ((1, 1, (u64(-17),)),) and cookie.retired
    assert_finished(result, inner)


def test_real_discarded_tail_can_idle_without_native_frame_or_optional_validator():
    result, inner, observed, _word, _callback, _export = setup(
        source=b": PAUSE HANDLER @ RP! R> HANDLER ! IDLE -42 THROW ;",
        outputs=0, exceptions=True, missing_validation=True)
    result.evaluate(b": INNER ['] MACHINE CATCH ; : OUTER 99 ['] INNER CATCH 7 ;")
    first = result.run_until_blocked("OUTER")
    assert type(first) is BlockedExecution and inner.active_invocations == ()
    _suspended, task, meter = blocked_state(result)
    assert task.frames == [] and task.tail is not None
    tail, receipt = task.tail, inner.last_receipt()
    assert receipt.root_instructions == receipt.root_callbacks == 1
    assert any(item[0] == "cancel_suffix" for item in observed.observations)
    assert type(wake(result, first)) is ExecutionResult
    assert result.main_context.data.snapshot() == (99, u64(-42), 7)
    assert inner.replies == () and inner.last_receipt() is receipt
    assert result._foreign_tasks.last_dispatch.root_id == task.ledger.root_id
    assert task.ledger.meter is meter and tail.capture is not None
    assert_finished(result, inner)


@pytest.mark.parametrize("deadline", [False, True])
def test_missing_validator_rejects_only_after_canonical_idle_tick_and_deadline_effect(deadline):
    reads = []

    def configure(result):
        def clock():
            reads.append(result.main_context.data.snapshot())
            return 0
        result.rtc.bind_monotonic_clock(clock)

    operations = ((Literal(5), IdleUntil(), Literal(9), SemanticReturn()) if deadline
                  else (Idle(), Literal(9), SemanticReturn()))
    result, inner, observed, word, _callback, _export = setup(
        operations=operations, outputs=1, missing_validation=True,
        configure_clock=configure if deadline else None)
    result.main_context.data.push(99)
    reads.clear()
    with pytest.raises(ForeignTaskError):
        result.run_until_blocked(word.xt)
    assert result.main_context.data.snapshot() == (99,)
    assert reads == ([(99,)] if deadline else [])
    assert result._foreign_tasks.last_dispatch.semantic_steps == (2 if deadline else 1)
    assert result._suspended_execution is None and inner.replies == ()
    assert any(item[0] == "cancel_all" for item in observed.observations)
    assert_finished(result, inner)


def test_six_method_adapter_still_runs_elapsed_deadline_without_detaching():
    result, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(0), IdleUntil(), Literal(9), SemanticReturn()),
        outputs=1, missing_validation=True)
    completed = result.run_until_blocked(word.xt)
    assert type(completed) is ExecutionResult
    assert result.main_context.data.snapshot() == (9,)
    assert inner.replies == ((1, 1, (9,)),)
    assert_finished(result, inner)


def test_deadline_underflow_spends_its_tick_but_never_samples_the_clock():
    reads = []

    def configure(result):
        def clock():
            reads.append(True)
            return 0
        result.rtc.bind_monotonic_clock(clock)

    result, inner, _observed, word, _callback, _export = setup(
        operations=(IdleUntil(), SemanticReturn()), outputs=0, configure_clock=configure)
    reads.clear()
    with pytest.raises(StackUnderflow):
        result.run_until_blocked(word.xt)
    assert reads == [] and result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result._suspended_execution is None and inner.replies == ()
    assert_finished(result, inner)


@pytest.mark.parametrize("invalid", [False, 1, None])
def test_optional_validator_requires_exact_true_before_handle_publication(invalid):
    result, inner, observed, word, _callback, _export = setup(
        operations=(Idle(), Literal(9), SemanticReturn()), outputs=1)
    observed.validation_result = invalid
    with pytest.raises(ForeignTaskError):
        result.run_until_blocked(word.xt)
    assert observed.validations and result._suspended_execution is None
    assert result.main_context.data.snapshot() == () and inner.replies == ()
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert_finished(result, inner)


@pytest.mark.parametrize("damage", ["callback_ir", "grants", "foreign_cookie", "adapter_route",
                                     "cursor_copy", "request_copy"])
def test_resume_revalidates_task_evidence_before_next_tick_and_cancels_tampering(monkeypatch, damage):
    result, inner, observed, word, callback, export = setup(
        operations=(Idle(), Literal(9), SemanticReturn()), outputs=1, prefix=True)
    first = result.run_until_blocked(word.xt)
    _suspended, task, meter = blocked_state(result)
    steps, receipt = meter.steps, inner.last_receipt()
    invoked = []
    if damage == "callback_ir":
        object.__setattr__(callback.implementation, "operations", (Literal(123), SemanticReturn()))
    elif damage == "grants":
        object.__setattr__(export, "task_grants", ())
    elif damage == "foreign_cookie":
        cookie = task.frames[-1].cookie
        result.memory.write64(cookie.slot_address, cookie.raw_cookie ^ 1)
    elif damage == "cursor_copy":
        _suspended.cursor = replace(_suspended.cursor)
    elif damage == "request_copy":
        task.frames[-1].request = replace(task.frames[-1].request)
    else:
        def replacement(self, *args):
            invoked.append(True)
            return True
        monkeypatch.setattr(SuspensionAdapter, "validate_parked", replacement)
    with pytest.raises(ExecutionError):
        wake(result, first)
    assert meter.steps == steps and inner.last_receipt() is receipt
    assert result.main_context.data.snapshot() == () and result.memory.read8(EXTERNAL_BASE) == ord("P")
    assert inner.replies == () and inner.active_invocations == () and invoked == []
    assert result._suspended_execution is None and not result.main_context.suspended
    with pytest.raises(ExecutionError):
        result.cancel_suspension(first.suspension)


def test_adapter_resume_validation_error_preserves_identity_and_completed_prefix():
    result, inner, observed, word, _callback, _export = setup(prefix=True)
    first = result.run_until_blocked(word.xt)
    error = KeyboardInterrupt("parked machine evidence unavailable")
    observed.validation_error = error
    receipt = inner.last_receipt()
    with pytest.raises(KeyboardInterrupt) as caught:
        wake(result, first)
    assert caught.value is error and inner.last_receipt() is receipt
    assert result.main_context.data.snapshot() == (7,)
    assert result.memory.read8(EXTERNAL_BASE) == ord("P") and inner.replies == ()
    assert inner.active_invocations == () and result._suspended_execution is None


def test_optional_validation_cannot_replace_equal_native_receipt_before_resume():
    result, inner, observed, word, _callback, _export = setup(
        operations=(Idle(), Literal(9), SemanticReturn()), outputs=1)
    first = result.run_until_blocked(word.xt)
    _suspended, _task, meter = blocked_state(result)
    receipt, steps = inner.last_receipt(), meter.steps
    observed.replace_receipt = True
    with pytest.raises(ForeignTaskError):
        wake(result, first)
    assert inner.last_receipt() is not receipt and inner.last_receipt() == receipt
    assert meter.steps == steps and result.main_context.data.snapshot() == ()
    assert inner.replies == () and inner.active_invocations == ()
    assert result._suspended_execution is None


def test_cancel_retires_machine_before_return_cleanup_and_invalidates_old_wake():
    result, inner, observed, word, _callback, _export = setup(prefix=True)
    context = result.main_context
    context.returns.push(0xA5)
    first = result.run_until_blocked(word.xt)
    _suspended, task, _meter = blocked_state(result)
    cookie = task.frames[-1].cookie
    issued = result.deliver_idle_wake(first.suspension, IdleWake.INTERRUPT)
    receipt = inner.last_receipt()
    result.cancel_suspension(first.suspension)
    assert observed.cancellations
    leased, _pointer, entries, observed_receipt = observed.cancellations[0]
    assert leased and any(entry is cookie for entry in entries) and observed_receipt is receipt
    assert cookie.retired and context.returns.snapshot() == (0xA5,)
    assert context.data.snapshot() == (7,) and context.reusable
    assert inner.active_invocations == () and inner.replies == ()
    assert result.memory.read8(EXTERNAL_BASE) == ord("P")
    with pytest.raises(ExecutionError):
        result.resume(first.suspension, issued)


def test_later_suspension_publication_failure_keeps_first_error_over_cleanup(monkeypatch):
    result, inner, observed, word, _callback, _export = setup()
    first = result.run_until_blocked(word.xt)
    original, secondary = KeyboardInterrupt("second handle allocation"), ForthAbort(b"cancel delivery")
    observed.cancel_error = secondary

    def fail_handle():
        raise original

    monkeypatch.setattr(result, "_allocate_suspension_handle", fail_handle)
    with pytest.raises(KeyboardInterrupt) as caught:
        wake(result, first)
    assert caught.value is original
    assert result.main_context.data.snapshot() == (7, 8)
    assert inner.active_invocations == () and inner.replies == ()
    assert result._suspended_execution is None and not result.main_context.suspended


@pytest.mark.parametrize("realtime", [False, True])
def test_idle_until_uses_original_manual_or_realtime_clock_and_pops_deadline(realtime):
    now, reads = [0], []

    def configure(result):
        result.rtc.set_uptime_ms(100)
        if realtime:
            def clock():
                reads.append(result.main_context.data.snapshot())
                return now[0]
            result.rtc.bind_monotonic_clock(clock)

    result, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(105), IdleUntil(), Literal(9), SemanticReturn()),
        outputs=1, configure_clock=configure)
    reads.clear()
    first = result.run_until_blocked(word.xt)
    assert type(first) is BlockedExecution and result.idle_deadline_ms == 105
    assert result.main_context.data.snapshot() == ()
    assert reads == ([()] if realtime else [])
    if realtime:
        now[0] = 4_000_000
    else:
        result.rtc.advance_uptime_ms(4)
    assert not result.idle_wake_due
    if realtime:
        now[0] = 5_000_000
    else:
        result.rtc.advance_uptime_ms(1)
    assert result.idle_wake_due
    assert type(wake(result, first)) is ExecutionResult
    assert result.main_context.data.snapshot() == (9,) and result.idle_deadline_ms is None
    assert_finished(result, inner)


@pytest.mark.parametrize("stage", ["entry", "resume"])
@pytest.mark.parametrize("damage", ["owner", "clock", "getter"])
def test_changed_rtc_route_is_rejected_without_calling_replacement(monkeypatch, damage, stage):
    result, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Literal(9), SemanticReturn()), outputs=1)
    first = result.run_until_blocked(word.xt) if stage == "resume" else None
    called = []
    if damage == "owner":
        result.rtc = HostedRTCService()
    elif damage == "clock":
        result.rtc._monotonic_ns = lambda: called.append(True) or 0
    else:
        monkeypatch.setattr(HostedRTCService, "uptime_ms", property(lambda self: called.append(True) or 0))
    with pytest.raises(ForeignTaskError):
        if first is None:
            result.run_until_blocked(word.xt)
        else:
            wake(result, first)
    assert called == [] and result._suspended_execution is None and inner.replies == ()
    assert inner.active_invocations == ()


@pytest.mark.parametrize("kind", [KeyboardInterrupt, ForthAbort])
def test_deadline_clock_failure_keeps_host_identity_after_pop_and_charged_tick(kind):
    armed, seen, error = [False], [], kind("deadline clock failed")

    def configure(result):
        def clock():
            if armed[0]:
                task = result._foreign_tasks._task_root
                seen.append((result.main_context.data.snapshot(), task.ledger.semantic_steps))
                raise error
            return 0
        result.rtc.bind_monotonic_clock(clock)

    result, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Literal(9), SemanticReturn()),
        outputs=1, prefix=True, configure_clock=configure)
    result.main_context.data.push(99)
    armed[0] = True
    with pytest.raises(kind) as caught:
        result.run_until_blocked(word.xt)
    assert caught.value is error and seen == [((99,), 2)]
    if kind is ForthAbort:
        assert error.origin_context is None
    assert result.main_context.data.snapshot() == (99,) and inner.replies == ()
    assert result.memory.read8(EXTERNAL_BASE) == ord("P")
    assert result._foreign_tasks.last_dispatch.semantic_steps == 2
    assert result._suspended_execution is None
    assert_finished(result, inner)


@pytest.mark.parametrize("damage", ["callback_ir", "grants"])
def test_clock_return_revalidates_code_and_grants_before_publishing_suspension(damage):
    armed, authority = [False], []

    def configure(result):
        def clock():
            if armed[0]:
                callback, export = authority
                if damage == "callback_ir":
                    object.__setattr__(callback.implementation, "operations", (Literal(123), SemanticReturn()))
                else:
                    object.__setattr__(export, "task_grants", ())
            return 0
        result.rtc.bind_monotonic_clock(clock)

    result, inner, _observed, word, callback, export = setup(
        operations=(Literal(5), IdleUntil(), Literal(9), SemanticReturn()),
        outputs=1, prefix=True, configure_clock=configure)
    authority.extend((callback, export))
    armed[0] = True
    with pytest.raises(ForeignTaskError):
        result.run_until_blocked(word.xt)
    assert result.main_context.data.snapshot() == () and result._suspended_execution is None
    assert result._foreign_tasks.last_dispatch.semantic_steps == 2
    assert result.memory.read8(EXTERNAL_BASE) == ord("P") and inner.replies == ()
    assert inner.active_invocations == ()


@pytest.mark.parametrize("name", ["KEY", "MS@", "IDLE-MS"])
def test_deadline_admission_does_not_expand_uart_or_general_clock_callbacks(name):
    result = runtime()
    with pytest.raises(ForeignTaskError):
        capture(result, name, inputs=int(name == "IDLE-MS"), outputs=int(name != "IDLE-MS"))
    assert result._foreign_tasks._task_root is None


@pytest.mark.parametrize("base", [Idle, IdleUntil])
def test_idle_subclasses_do_not_acquire_the_exact_ir_admission(base):
    class CustomizedIdle(base):
        pass

    result = runtime()
    callback = result.define_colon("CUSTOM-IDLE", (CustomizedIdle(), SemanticReturn()))
    with pytest.raises(ForeignTaskError):
        capture(result, callback)
    assert result._foreign_tasks._task_root is None


def test_source_evaluation_still_cannot_publish_callback_idle_suspension():
    result, inner, _observed, _word, _callback, _export = setup(
        operations=(Idle(), SemanticReturn()), outputs=0)
    with pytest.raises(ExecutionError):
        result.evaluate(b"MACHINE")
    assert result._suspended_execution is None and inner.replies == ()
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert_finished(result, inner)
