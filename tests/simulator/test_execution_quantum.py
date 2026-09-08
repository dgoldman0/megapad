"""Generic host quanta preserve ordinary source and genuine IDL semantics."""

from __future__ import annotations

from dataclasses import replace

import pytest

from shared.cells import u64
from simulator.errors import ExecutionError, StepBudgetExceeded
from simulator.runtime import (
    BlockedExecution,
    ExecutionResult,
    IdleWake,
    MegaForthRuntime,
    YieldedExecution,
)
from tests.simulator.test_kdos_exceptions import _load_exceptions


def _finish_yields(runtime, result):
    observations = []
    for _ in range(2_000):
        if not isinstance(result, YieldedExecution):
            return result, observations
        observations.append(result.semantic_steps)
        result = runtime.resume_yielded(result.suspension)
    pytest.fail("bounded quantum fixture did not complete")


@pytest.mark.parametrize("quantum", [True, False, 1.5, "3"])
def test_quantum_rejects_non_integer_values_without_starting(quantum) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": RUN 42 ;")
    with pytest.raises(TypeError, match="quantum_steps"):
        runtime.run_until_blocked("RUN", quantum_steps=quantum)
    assert runtime.main_context.reusable
    assert runtime.main_context.data.snapshot() == ()


@pytest.mark.parametrize("quantum", [0, -1])
def test_quantum_rejects_nonpositive_values_without_starting(quantum) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": RUN 42 ;")
    with pytest.raises(ValueError, match="positive"):
        runtime.run_until_blocked("RUN", quantum_steps=quantum)
    assert runtime.main_context.reusable
    assert runtime.main_context.returns.snapshot() == ()


def test_ordinary_loop_yields_without_idle_and_preserves_bound_calls() -> None:
    source = (
        b": YIELD? ; : STEP 1+ ; "
        b": RUN 0 25 0 DO STEP YIELD? LOOP ; "
        b": STEP 100 + ;"
    )
    ordinary = MegaForthRuntime()
    ordinary.evaluate(source)
    expected = ordinary.run_until_blocked("RUN")
    assert isinstance(expected, ExecutionResult)

    runtime = MegaForthRuntime()
    runtime.evaluate(source)
    initial_time = (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms)
    first = runtime.run_until_blocked("RUN", quantum_steps=7)
    assert isinstance(first, YieldedExecution)
    completed, counts = _finish_yields(runtime, first)

    assert isinstance(completed, ExecutionResult)
    assert len(counts) > 2
    assert counts == sorted(set(counts))
    assert completed.semantic_steps == expected.semantic_steps
    assert runtime.main_context.data.snapshot() == (25,)
    assert runtime.main_context.data.snapshot() == ordinary.main_context.data.snapshot()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable
    assert (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms) == initial_time


def test_real_nested_catch_and_throw_keep_handler_and_pointer_captures() -> None:
    runtime = _load_exceptions()
    runtime.evaluate(
        b": INNER 11 22 -73 THROW 88 ; "
        b": WRAP ['] INNER CATCH 7 ; "
        b": ROOT 99 ['] WRAP CATCH 5 ;"
    )
    handler = runtime.find("_TASK-HANDLERS")
    assert handler is not None
    result = runtime.run_until_blocked("ROOT", quantum_steps=1)
    held_handler = False
    for _ in range(500):
        if not isinstance(result, YieldedExecution):
            break
        held_handler |= runtime.memory.read64(handler.body_address) != 0
        assert runtime.main_context.host_control_fault is None
        result = runtime.resume_yielded(result.suspension)
    assert isinstance(result, ExecutionResult)
    assert held_handler
    assert runtime.main_context.data.snapshot() == (99, u64(-73), 7, 0, 5)
    assert runtime.memory.read64(handler.body_address) == 0
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable


def test_quantum_never_resets_the_cumulative_step_budget() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FOREVER BEGIN 1 DROP AGAIN ;")
    result = runtime.run_until_blocked("FOREVER", quantum_steps=3, step_budget=19)
    counts = []
    with pytest.raises(StepBudgetExceeded):
        for _ in range(30):
            assert isinstance(result, YieldedExecution)
            counts.append(result.semantic_steps)
            result = runtime.resume_yielded(result.suspension)
    assert len(counts) > 2
    assert counts == sorted(set(counts))
    assert counts[-1] <= 19
    assert runtime.main_context.returns.snapshot() == ()
    assert not runtime.main_context.suspended


def test_idle_and_host_yield_require_distinct_resume_authority() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": IDLE [ 0 C, ] ; : ROOT 1 IDLE 2 3 + ;")
    first = runtime.run_until_blocked("ROOT", quantum_steps=1)
    assert isinstance(first, YieldedExecution)
    with pytest.raises(ExecutionError, match="IDL wake"):
        runtime.deliver_idle_wake(first.suspension, IdleWake.INTERRUPT)
    with pytest.raises(ExecutionError, match="resume_yielded"):
        runtime.resume(first.suspension, None)
    with pytest.raises(ExecutionError, match="stale"):
        runtime.resume_yielded(replace(first.suspension))

    blocked, counts = _finish_yields(runtime, first)
    assert isinstance(blocked, BlockedExecution)
    assert counts
    with pytest.raises(ExecutionError, match="runtime-issued wake"):
        runtime.resume_yielded(blocked.suspension)
    with pytest.raises(ExecutionError, match="stale"):
        runtime.resume_yielded(first.suspension)
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    after_idle = runtime.resume(blocked.suspension, receipt)
    completed, further_counts = _finish_yields(runtime, after_idle)
    assert isinstance(completed, ExecutionResult)
    assert further_counts
    assert runtime.main_context.data.snapshot() == (1, 5)
    assert runtime.main_context.reusable


@pytest.mark.parametrize("stack_name", ["data", "returns"])
def test_yielded_stack_mutation_is_rejected_and_cancel_restores_returns(
    stack_name,
) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": ROOT 1 2 + ;")
    first = runtime.run_until_blocked("ROOT", quantum_steps=1)
    assert isinstance(first, YieldedExecution)
    context = runtime.main_context
    stack = getattr(context, stack_name)
    stack.push(99)
    with pytest.raises(ExecutionError, match="stack changed"):
        runtime.resume_yielded(first.suspension)
    runtime.cancel_suspension(first.suspension)
    assert context.returns.snapshot() == ()
    assert context.reusable


def test_cancel_after_return_pointer_capture_fails_closed() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": ROOT RP@ 1 2 + ;")
    first = runtime.run_until_blocked("ROOT", quantum_steps=1)
    assert isinstance(first, YieldedExecution)
    assert len(runtime.main_context.data.snapshot()) == 1
    runtime.cancel_suspension(first.suspension)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.host_control_fault == "ExecutionError"
    assert not runtime.main_context.reusable


def test_nested_source_evaluation_finishes_before_outer_quantum() -> None:
    runtime = MegaForthRuntime()
    observations = []

    def evaluate_source(context):
        runtime.evaluate(b": BUILT 42 ; 5 6 +", context=context)
        observations.append((context.suspended, context.data.snapshot()))

    runtime.define_primitive("SOURCE-CALL", evaluate_source)
    runtime.evaluate(b": ROOT SOURCE-CALL 9 ;")
    first = runtime.run_until_blocked("ROOT", quantum_steps=1)
    assert isinstance(first, YieldedExecution)
    assert observations == [(False, (11,))]
    assert runtime.find("BUILT") is not None
    assert not runtime._active_input_states
    completed, _ = _finish_yields(runtime, first)
    assert isinstance(completed, ExecutionResult)
    assert runtime.main_context.data.snapshot() == (11, 9)
    assert runtime.main_context.reusable


def test_nested_guest_throw_keeps_ordinary_outer_catch_cleanup() -> None:
    runtime = _load_exceptions()

    def evaluate_throw(context):
        runtime.evaluate(b"11 -91 THROW 88", context=context)

    runtime.define_primitive("SOURCE-THROW", evaluate_throw)
    runtime.evaluate(b": BODY SOURCE-THROW 77 ; : ROOT ['] BODY CATCH 9 ;")
    first = runtime.run_until_blocked("ROOT", quantum_steps=2)
    completed, _ = _finish_yields(runtime, first)
    assert isinstance(completed, ExecutionResult)
    assert runtime.main_context.data.snapshot() == (u64(-91), 9)
    handler = runtime.find("_TASK-HANDLERS")
    assert handler is not None
    assert runtime.memory.read64(handler.body_address) == 0
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable
