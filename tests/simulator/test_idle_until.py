"""Hosted IDLE-UNTIL and IDLE-MS: sleep until input or an MS@ deadline."""

from __future__ import annotations

from shared.cells import MASK64
from simulator.runtime import (
    BlockedExecution,
    ExecutionResult,
    IdleWake,
    MegaForthRuntime,
)


def _blocked(runtime: MegaForthRuntime, source: bytes, word: str):
    runtime.evaluate(source)
    context = runtime.new_context()
    blocked = runtime.run_until_blocked(word, context=context)
    assert isinstance(blocked, BlockedExecution)
    return blocked, context


def _resume(runtime: MegaForthRuntime, blocked: BlockedExecution):
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    return runtime.resume(blocked.suspension, receipt)


def test_idle_ms_blocks_until_its_deadline() -> None:
    runtime = MegaForthRuntime()
    runtime.rtc.set_uptime_ms(100)
    blocked, context = _blocked(runtime, b": NAP 5 IDLE-MS 77 ;", "NAP")

    assert runtime.idle_deadline_ms == 105
    runtime.rtc.advance_uptime_ms(4)
    assert not runtime.idle_wake_due
    runtime.rtc.advance_uptime_ms(1)
    assert runtime.idle_wake_due

    assert isinstance(_resume(runtime, blocked), ExecutionResult)
    assert context.data.snapshot() == (77,)
    assert runtime.idle_deadline_ms is None


def test_input_is_due_before_the_deadline() -> None:
    runtime = MegaForthRuntime()
    blocked, context = _blocked(runtime, b": NAP 1000 IDLE-MS KEY ;", "NAP")

    assert not runtime.idle_wake_due
    runtime.inject_uart_input(b"k")
    assert runtime.idle_wake_due

    assert isinstance(_resume(runtime, blocked), ExecutionResult)
    assert context.data.snapshot() == (ord("k"),)


def test_a_passed_deadline_and_zero_return_at_once() -> None:
    runtime = MegaForthRuntime()
    runtime.rtc.set_uptime_ms(50)
    runtime.evaluate(b": NOW-NAP 0 IDLE-UNTIL MS@ IDLE-UNTIL 0 IDLE-MS 9 ;")
    context = runtime.new_context()

    result = runtime.run_until_blocked("NOW-NAP", context=context)

    assert isinstance(result, ExecutionResult)
    assert context.data.snapshot() == (9,)
    assert runtime.idle_deadline_ms is None


def test_idle_ms_saturates_its_deadline() -> None:
    runtime = MegaForthRuntime()
    runtime.rtc.set_uptime_ms(10)
    blocked, _context = _blocked(runtime, b": LONG -1 IDLE-MS ;", "LONG")
    assert runtime.idle_deadline_ms == MASK64


def test_source_evaluation_returns_at_once() -> None:
    # Only a root dispatch can suspend; elsewhere the wait may end early.
    runtime = MegaForthRuntime()
    runtime.evaluate(b"5 IDLE-MS 9")
    assert runtime.main_context.data.snapshot() == (9,)
    assert runtime.idle_deadline_ms is None
