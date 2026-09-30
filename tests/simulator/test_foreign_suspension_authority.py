"""A parked task's engine witness owns resume rejection and cancellation."""

from dataclasses import replace

import pytest

from simulator.errors import ExecutionError
from simulator.foreign_runtime import ForeignTaskError
from simulator.ir import Idle, IdleUntil, Literal, Return
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import BlockedExecution, ExecutionContext, IdleWake
from tests.simulator.test_foreign_suspension import setup


@pytest.mark.parametrize("action", ("resume", "cancel"))
@pytest.mark.parametrize("damage", ("missing_root", "foreign_root", "context", "meter"))
def test_changed_blocked_projection_cannot_skip_original_native_retirement(action, damage):
    runtime, inner, observed, word, _callback, _export = setup(
        operations=(Idle(), Literal(9), Return()), outputs=1, prefix=True)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(0xA5)
    blocked = runtime.run_until_blocked(word.xt)
    assert type(blocked) is BlockedExecution
    original = runtime._suspended_execution
    root = runtime._foreign_tasks._task_root
    meter = original.meter
    cookie = root.frames[-1].cookie
    receipt = inner.last_receipt()
    steps = meter.steps
    wake = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    other_context = ExecutionContext()

    if damage == "missing_root":
        original.task_root = None
    elif damage == "foreign_root":
        original.task_root = object()
    elif damage == "context":
        original.context = other_context
    else:
        original.meter = object()

    # Selection uses the issued witness even when its exposed projections no
    # longer name a task. Explicit cancellation needs no guest resume proof.
    assert runtime.task_suspension_pending(blocked.suspension)
    if action == "resume":
        with pytest.raises(ExecutionError):
            runtime.resume(blocked.suspension, wake)
    else:
        runtime.cancel_suspension(blocked.suspension)

    assert observed.cancellations
    leased, _pointer, returns_at_cancel, receipt_at_cancel = observed.cancellations[0]
    assert leased and any(entry is cookie for entry in returns_at_cancel)
    assert receipt_at_cancel is receipt and inner.last_receipt() is receipt
    assert cookie.retired and inner.active_invocations == () and inner.replies == ()
    assert meter.steps == steps and runtime._foreign_tasks.last_dispatch.semantic_steps == 1
    assert runtime.memory.read8(EXTERNAL_BASE) == ord("P")
    assert context.data.snapshot() == (99,) and context.returns.snapshot() == (0xA5,)
    assert not context.suspended and context.reusable
    assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
    assert other_context.data.snapshot() == other_context.returns.snapshot() == ()
    assert other_context.reusable and not other_context.suspended
    with pytest.raises(ExecutionError):
        runtime.resume(blocked.suspension, wake)
    with pytest.raises(ExecutionError):
        runtime.cancel_suspension(blocked.suspension)


@pytest.mark.parametrize("clock_raises", (False, True))
@pytest.mark.parametrize("state_damage", ("refund", "shape", "policy"))
def test_deadline_clock_cannot_refund_the_tick_or_callback_scope_before_first_detach(clock_raises, state_damage):
    armed, observed_work = [False], []
    original_error = KeyboardInterrupt("clock changed spent callback work")

    def configure(runtime):
        def clock():
            if not armed[0]:
                return 0
            root = runtime._foreign_tasks._task_root
            ledger, meter = root.ledger, root.ledger.meter
            observed_work.append((ledger, meter, meter.steps, ledger.semantic_steps,
                                  ledger.semantic_limit, runtime.main_context.data.snapshot()))
            if state_damage == "policy":
                ledger._policy = replace(ledger._policy, semantic_limit=ledger.semantic_limit + 1)
            else:
                ledger._state = (replace(ledger._state, semantic_steps=0,
                    semantic_scopes=tuple(replace(scope, steps=0)
                                          for scope in ledger._state.semantic_scopes))
                    if state_damage == "refund" else object())
                meter.steps = 0
            if clock_raises:
                raise original_error
            return 0
        runtime.rtc.bind_monotonic_clock(clock)

    runtime, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Literal(9), Return()), outputs=1,
        configure_clock=configure, prefix=True)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(0xA5)
    armed[0] = True
    with pytest.raises(KeyboardInterrupt if clock_raises else ForeignTaskError) as caught:
        runtime.run_until_blocked(word.xt)
    if clock_raises:
        assert caught.value is original_error
    assert len(observed_work) == 1
    ledger, meter, original_steps, charged_steps, original_limit, data_at_clock = observed_work[0]
    assert data_at_clock == (99,) and charged_steps == 2
    assert meter.steps == original_steps and ledger.semantic_steps == charged_steps
    assert ledger.semantic_limit == original_limit
    assert runtime._foreign_tasks.last_dispatch.semantic_steps == charged_steps
    assert context.data.snapshot() == (99,) and context.returns.snapshot() == (0xA5,)
    assert runtime.memory.read8(EXTERNAL_BASE) == ord("P")
    assert inner.active_invocations == () and inner.replies == ()
    assert runtime._suspended_execution is None and not context.suspended


def test_first_detach_cannot_forge_an_empty_machine_chain_from_mutable_projections(monkeypatch):
    runtime, inner, observed, word, _callback, _export = setup(
        operations=(Idle(), Literal(9), Return()), outputs=1, prefix=True)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(0xA5)
    original_allocate = runtime._allocate_suspension_handle
    captured = []

    def allocate_then_hide_chain():
        handle = original_allocate()
        root = runtime._foreign_tasks._task_root
        cookie = root.frames[-1].cookie
        ledger = root.ledger
        captured.append((cookie, ledger, ledger.meter.steps, inner.last_receipt()))
        root.frames.clear()
        ledger._state = replace(ledger._state, active=())
        return handle

    monkeypatch.setattr(runtime, "_allocate_suspension_handle", allocate_then_hide_chain)
    with pytest.raises(ForeignTaskError):
        runtime.run_until_blocked(word.xt)
    assert len(captured) == 1 and observed.cancellations
    cookie, ledger, original_steps, receipt = captured[0]
    _leased, _pointer, returns_at_cancel, receipt_at_cancel = observed.cancellations[0]
    assert any(entry is cookie for entry in returns_at_cancel)
    assert receipt_at_cancel is receipt and inner.last_receipt() is receipt
    assert cookie.retired and inner.active_invocations == () and inner.replies == ()
    assert ledger.meter.steps == original_steps and ledger.semantic_steps == 1
    assert runtime._foreign_tasks.last_dispatch.semantic_steps == 1
    assert context.data.snapshot() == (99,) and context.returns.snapshot() == (0xA5,)
    assert runtime.memory.read8(EXTERNAL_BASE) == ord("P")
    assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
    assert not context.suspended and context.reusable


def test_clock_replaced_accounting_getter_is_not_called_during_raw_error_cleanup(monkeypatch):
    armed, called, original_work = [False], [], []
    failure = KeyboardInterrupt("clock changed accounting metadata")

    def configure(runtime):
        def changed_getter(_state):
            called.append(True)
            raise AssertionError("rejected accounting getter must not run during cleanup")

        def clock():
            if not armed[0]:
                return 0
            root = runtime._foreign_tasks._task_root
            original_work.append((root.ledger.meter, root.ledger.meter.steps,
                                  root.ledger.semantic_steps))
            monkeypatch.setattr(type(root.ledger._state), "semantic_steps", property(changed_getter))
            raise failure
        runtime.rtc.bind_monotonic_clock(clock)

    runtime, inner, observed, word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Return()), outputs=0,
        configure_clock=configure, prefix=True)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(0xA5)
    armed[0] = True
    with pytest.raises(KeyboardInterrupt) as caught:
        runtime.run_until_blocked(word.xt)
    assert caught.value is failure and called == []
    assert len(original_work) == 1
    meter, original_steps, charged = original_work[0]
    assert charged == runtime._foreign_tasks.last_dispatch.semantic_steps == 2
    assert meter.steps == original_steps
    assert observed.cancellations and inner.active_invocations == () and inner.replies == ()
    assert context.data.snapshot() == (99,) and context.returns.snapshot() == (0xA5,)
    assert runtime.memory.read8(EXTERNAL_BASE) == ord("P")
    assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
    assert not context.suspended and called == []
