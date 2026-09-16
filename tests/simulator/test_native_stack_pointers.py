"""Native pointer reads retain return-frontier capture and escape guards."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from simulator.errors import ExecutionError
from simulator.runtime import ExecutionResult, YieldedExecution
from simulator.stacks import DataStack, StackOverflow
from tests.simulator.test_native_execution import _compare, _runtimes


@pytest.mark.parametrize("operation", [b"SP@", b"RP@"])
def test_pointer_read_is_native_and_observes_the_pre_push_frontier(operation):
    runtimes = _runtimes(b": RUN " + operation + b" ;")
    context = runtimes[0].main_context
    expected = context.data.empty_pointer - 8 if operation == b"SP@" else context.returns.empty_pointer - 8
    result = _compare(runtimes, "RUN", inputs=(17,))
    assert result["error"] is None
    assert result["data"] == (17, expected)


@pytest.mark.parametrize("budget", range(1, 14))
def test_pointer_capture_fault_guard_matches_each_original_budget(budget):
    runtimes = _runtimes(b": RUN SP@ RP@ DROP RP@ DROP DROP ;")
    _compare(runtimes, "RUN", step_budget=budget, require_native=False)


@pytest.mark.parametrize("operation", [b"SP@", b"RP@"])
def test_pointer_push_overflow_keeps_rp_capture_before_the_fault(operation):
    runtimes = _runtimes(b": RUN " + operation + b" ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.data.empty_pointer
        context.data = DataStack((17,), memory=runtime.memory, floor=empty - 8, empty_pointer=empty)
    result = _compare(runtimes, "RUN", require_native=False)
    assert result["error"][0] is StackOverflow
    assert result["host_control_fault"] == ("StackOverflow" if operation == b"RP@" else None)
    assert result["reusable"] is (operation != b"RP@")


@pytest.mark.parametrize("initial_generation", [0, 1 << 100])
def test_native_capture_counts_are_settled_before_callbacks_without_truncation(initial_generation):
    runtimes = _runtimes()
    records = [[], []]
    for runtime, captured in zip(runtimes, records):
        def observe(context, *, captured=captured):
            captured.append(context.returns.pointer_capture_checkpoint())
        runtime.define_primitive("OBSERVE", observe)
        runtime.evaluate(b": INNER RP@ DROP ; : RUN RP@ DROP INNER OBSERVE ;")
        runtime.main_context.returns.restore_pointer_captures(initial_generation)
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert records == [[initial_generation + 2], [initial_generation + 2]]
    assert [r.main_context.returns.pointer_capture_checkpoint() for r in runtimes] == [initial_generation] * 2


def test_native_capture_reaches_host_failure_guard_before_unwinding():
    runtimes = _runtimes()
    for runtime in runtimes:
        def fail(context):
            raise ExecutionError("host callback failed")
        runtime.define_primitive("FAIL", fail)
        runtime.evaluate(b": RUN RP@ DROP FAIL ;")
    result = _compare(runtimes, "RUN")
    assert result["error"] == (ExecutionError, "host callback failed")
    assert result["host_control_fault"] == "ExecutionError"
    assert not result["reusable"]


@pytest.mark.parametrize("quantum", [2, 3, 7])
@pytest.mark.parametrize("cancel", [False, True])
def test_native_capture_survives_host_yield_and_resume_or_cancellation(quantum, cancel):
    observations = []
    for runtime in _runtimes(b": RUN RP@ DROP 20 0 DO I DROP LOOP ;"):
        result = runtime.run_until_blocked("RUN", quantum_steps=quantum, step_budget=200)
        context = runtime.main_context
        boundaries = []
        for _ in range(100):
            if not isinstance(result, YieldedExecution):
                break
            boundaries.append((result.semantic_steps, context.data.snapshot(),
                               context.returns.snapshot(),
                               context.returns.pointer_capture_checkpoint()))
            if cancel:
                runtime.cancel_suspension(result.suspension)
                break
            result = runtime.resume_yielded(result.suspension)
        if not cancel:
            assert isinstance(result, ExecutionResult)
        assert boundaries
        observations.append((boundaries, context.data.snapshot(), context.returns.snapshot(),
                             context.reusable, context.host_control_fault,
                             context.returns.pointer_capture_checkpoint()))
    assert observations[0] == observations[1]
    assert observations[0][3] is (not cancel)
    assert observations[0][4] == ("ExecutionError" if cancel else None)


def test_captures_across_native_allowances_still_reach_the_next_callback():
    runtimes = _runtimes()
    records = [[], []]
    for runtime, captured in zip(runtimes, records):
        def observe(context, *, captured=captured):
            captured.append(context.returns.pointer_capture_checkpoint())
        runtime.define_primitive("OBSERVE", observe)
        runtime.evaluate(b": RUN 2000 0 DO RP@ DROP LOOP OBSERVE ;")
    before = runtimes[1].native_execution_stats["entries"]
    result = _compare(runtimes, "RUN", step_budget=15000)
    assert result["error"] is None
    assert records == [[2000], [2000]]
    assert runtimes[1].native_execution_stats["entries"] - before >= 2


@pytest.mark.parametrize("operation", [b"SP@", b"RP@"])
def test_pointer_primitive_binding_survives_shadowing(operation):
    runtimes = _runtimes(b": ORIGINAL " + operation + b" ;")
    for runtime in runtimes:
        runtime.evaluate(b": " + operation + b" 99 ; : SHADOW " + operation + b" ;")
    # A host operand materializes the data page before the pointer push.
    assert _compare(runtimes, "ORIGINAL", inputs=(17,))["error"] is None
    for runtime in runtimes:
        runtime.main_context.data.clear()
    assert _compare(runtimes, "SHADOW")["data"] == (99,)
