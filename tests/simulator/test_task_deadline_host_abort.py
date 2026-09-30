"""Captured deadline-clock host ABORTs retain raw provenance through outer scopes."""

import pytest

from simulator.errors import ForthAbort
from simulator.ir import Idle, IdleUntil, Literal, Return
from simulator.rtc import HostedRTCService
from simulator.runtime import BlockedExecution, IdleWake
from tests.simulator.test_foreign_suspension import setup


@pytest.mark.parametrize("entry", ("run", "resume"))
@pytest.mark.parametrize("replace_rtc", (False, True))
def test_captured_deadline_abort_keeps_origin_stack_prefix_and_exact_outer_scope(entry, replace_rtc):
    armed, seen = [False], []
    error = ForthAbort("captured host deadline failure")

    def configure(runtime):
        def clock():
            if armed[0]:
                task = runtime._foreign_tasks._task_root
                seen.append((runtime.main_context.data.snapshot(), task.ledger.semantic_steps))
                if replace_rtc:
                    runtime.rtc = HostedRTCService()
                raise error
            return 0
        runtime.rtc.bind_monotonic_clock(clock)

    operations = ((Idle(),) if entry == "resume" else ()) + (
        Literal(5), IdleUntil(), Literal(9), Return())
    runtime, inner, _observed, word, _callback, _export = setup(
        operations=operations, outputs=1, configure_clock=configure)
    context = runtime.main_context
    context.data.push(99)
    context.returns.push(0xA5)
    blocked = runtime.run_until_blocked(word.xt) if entry == "resume" else None
    if blocked is not None:
        assert type(blocked) is BlockedExecution
    armed[0] = True

    with pytest.raises(ForthAbort) as caught:
        if entry == "run":
            runtime.run_until_blocked(word.xt)
        else:
            wake = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
            runtime.resume(blocked.suspension, wake)

    assert caught.value is error and error.origin_context is None
    assert seen == [((99,), 3 if entry == "resume" else 2)]
    assert context.data.snapshot() == (99,)
    assert context.returns.snapshot() == (0xA5,)
    assert context.reusable and not context.suspended
    assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
    assert runtime._foreign_tasks.last_dispatch.semantic_steps == seen[0][1]
    assert inner.active_invocations == () and inner.replies == ()
    assert runtime._private_host_abort._error is None


def test_source_evaluation_consumes_deadline_without_reading_or_parking():
    armed, samples = [False], []

    def configure(runtime):
        def clock():
            if armed[0]:
                samples.append(True)
                raise ForthAbort("source evaluation must not read a deadline clock")
            return 0
        runtime.rtc.bind_monotonic_clock(clock)

    runtime, inner, _observed, _word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Literal(9), Return()), outputs=1,
        configure_clock=configure)
    context = runtime.main_context
    context.data.push(99)
    armed[0] = True
    runtime.evaluate(b"MACHINE")
    assert samples == [] and context.data.snapshot() == (99, 9)
    assert context.returns.snapshot() == () and context.reusable
    assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
    assert inner.active_invocations == () and inner.replies == ((1, 1, (9,)),)
    assert runtime._foreign_tasks.last_dispatch.semantic_steps == 4
    assert runtime._private_host_abort._error is None


def test_deadline_abort_uses_original_traceback_descriptor_and_cannot_be_reused():
    trace_reads, armed = [], [False]

    class ClockAbort(ForthAbort):
        def __getattribute__(self, name):
            if name == "__traceback__":
                trace_reads.append(name)
                raise AssertionError("host-overridden traceback route must not execute")
            return super().__getattribute__(name)

    error = ClockAbort("reused deadline failure")

    def configure(runtime):
        def clock():
            if armed[0]:
                raise error
            return 0
        runtime.rtc.bind_monotonic_clock(clock)

    runtime, inner, _observed, word, _callback, _export = setup(
        operations=(Literal(5), IdleUntil(), Return()), outputs=0,
        configure_clock=configure)

    def ordinary(context):
        raise error

    reused = runtime.define_primitive("REUSE-HOST-ERROR", ordinary)
    runtime.main_context.data.push(99)
    armed[0] = True
    with pytest.raises(ClockAbort) as first:
        runtime.run_until_blocked(word.xt)
    assert first.value is error and error.origin_context is None
    assert runtime.main_context.data.snapshot() == (99,)
    assert runtime._private_host_abort._error is None and trace_reads == []
    assert inner.active_invocations == ()

    # Calling the helper elsewhere grants no authority for an ordinary source
    # primitive, even when it raises the very same already-caught object.
    runtime._private_host_abort.issue_task_deadline(object(), error)
    with pytest.raises(ClockAbort) as second:
        runtime.execute(reused.xt)
    assert second.value is error and error.origin_context is runtime.main_context
    assert runtime.main_context.data.snapshot() == runtime.main_context.returns.snapshot() == ()
    assert trace_reads == [] and runtime._private_host_abort._error is None
