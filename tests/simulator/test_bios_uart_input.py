"""Focused hosted BIOS UART input and resumable KEY coverage."""

from __future__ import annotations

import pytest

from shared.cells import TRUE
from simulator.errors import ExecutionError
from simulator.ir import Branch, BranchZero, Idle, Return, UartReadAttempt
from simulator.runtime import (
    BlockedExecution,
    ColonDefinition,
    ExecutionResult,
    IdleWake,
    MegaForthRuntime,
    PrimitiveDefinition,
)


def test_uart_input_is_runtime_owned_strict_and_fifo() -> None:
    runtime = MegaForthRuntime()
    other = MegaForthRuntime()

    assert runtime.uart_input == b""
    assert not runtime.uart_input_available
    assert other.uart_input == b""

    runtime.inject_uart_input(b"\x00\xff")
    snapshot = runtime.uart_input
    runtime.inject_uart_input(b"A")

    assert snapshot == b"\x00\xff"
    assert runtime.uart_input == b"\x00\xffA"
    assert runtime.uart_input_available
    assert other.uart_input == b""
    for invalid in (bytearray(b"B"), memoryview(b"B"), "B", 0):
        with pytest.raises(TypeError, match="UART input payload must be bytes"):
            runtime.inject_uart_input(invalid)  # type: ignore[arg-type]
    assert runtime.uart_input == b"\x00\xffA"

    context = runtime.new_context()
    runtime.execute("KEY?", context=context)
    runtime.execute("KEY?", context=context)
    assert context.data.snapshot() == (TRUE, TRUE)
    assert runtime.uart_input == b"\x00\xffA"
    context.data.clear()

    runtime.write_uart_bytes(b"already published")
    for expected, remaining in (
        (0x00, b"\xffA"),
        (0xFF, b"A"),
        (ord("A"), b""),
    ):
        runtime.execute("KEY", context=context)
        assert context.data.pop() == expected
        assert runtime.uart_input == remaining
        assert runtime.uart_output == b"already published"

    runtime.execute("KEY?", context=context)
    assert context.data.snapshot() == (0,)
    assert not runtime.uart_input_available
    assert runtime.uart_input == b""


def test_uart_input_tail_discard_is_exact_atomic_and_strict() -> None:
    runtime = MegaForthRuntime()

    assert runtime.uart_input_pending == 0
    runtime.discard_uart_input_tail(0)
    runtime.inject_uart_input(b"prefix-tail")
    assert runtime.uart_input_pending == len(b"prefix-tail")

    runtime.discard_uart_input_tail(len(b"-tail"))
    assert runtime.uart_input == b"prefix"
    assert runtime.uart_input_pending == len(b"prefix")

    for invalid in (True, False, 1.0, "1", None):
        with pytest.raises(
            TypeError,
            match="UART input discard count must be an integer",
        ):
            runtime.discard_uart_input_tail(invalid)  # type: ignore[arg-type]
        assert runtime.uart_input == b"prefix"

    for invalid in (-1, len(b"prefix") + 1):
        with pytest.raises(ValueError, match="UART input discard count"):
            runtime.discard_uart_input_tail(invalid)
        assert runtime.uart_input == b"prefix"

    runtime.discard_uart_input_tail(len(b"prefix"))
    assert runtime.uart_input == b""
    assert runtime.uart_input_pending == 0


def test_key_headers_and_ir_match_the_bios_polling_order_without_a_helper_word() -> None:
    runtime = MegaForthRuntime()
    emit = runtime.find("EMIT")
    key = runtime.find("KEY")
    key_query = runtime.find("KEY?")
    carriage_return = runtime.find("CR")

    assert emit is not None
    assert key is not None
    assert key_query is not None
    assert carriage_return is not None
    assert runtime.memory.read64(key.header_address) == emit.header_address
    assert runtime.memory.read64(key_query.header_address) == key.header_address
    assert (
        runtime.memory.read64(carriage_return.header_address)
        == key_query.header_address
    )
    assert isinstance(key.implementation, ColonDefinition)
    assert key.implementation.operations == (
        UartReadAttempt(),
        BranchZero(3),
        Branch(5),
        Idle(),
        Branch(0),
        Return(),
    )
    assert isinstance(key_query.implementation, PrimitiveDefinition)


def test_empty_key_retries_only_after_exact_wakes_and_releases_its_context() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": READ-PAIR 11 KEY 22 KEY 33 ;")
    runtime.write_uart_bytes(b"prompt")
    context = runtime.new_context()
    context.returns.push(0xA5)

    first = runtime.run_until_blocked("READ-PAIR", context=context)
    assert isinstance(first, BlockedExecution)
    assert context.data.snapshot() == (11,)
    assert context.returns.snapshot()[0] == 0xA5
    assert context.suspended
    assert not context.reusable
    assert runtime.uart_input == b""
    assert runtime.uart_output == b"prompt"

    blocked_data = context.data.snapshot()
    blocked_returns = context.returns.snapshot()
    runtime.inject_uart_input(b"A")
    assert context.suspended
    assert context.data.snapshot() == blocked_data
    assert context.returns.snapshot() == blocked_returns
    assert runtime.uart_input == b"A"
    with pytest.raises(ExecutionError, match="while dispatch .* is suspended"):
        runtime.run_until_blocked("KEY", context=runtime.new_context())

    first_wake = runtime.deliver_idle_wake(
        first.suspension,
        IdleWake.INTERRUPT,
    )
    second = runtime.resume(first.suspension, first_wake)
    assert isinstance(second, BlockedExecution)
    assert second.suspension != first.suspension
    assert context.data.snapshot() == (11, ord("A"), 22)
    assert runtime.uart_input == b""
    assert runtime.uart_output == b"prompt"
    with pytest.raises(ExecutionError, match="stale"):
        runtime.deliver_idle_wake(first.suspension, IdleWake.DMA)

    current = second
    for wake_kind in (IdleWake.DMA, IdleWake.INTERRUPT):
        before_data = context.data.snapshot()
        before_returns = context.returns.snapshot()
        wake = runtime.deliver_idle_wake(current.suspension, wake_kind)
        retried = runtime.resume(current.suspension, wake)
        assert isinstance(retried, BlockedExecution)
        assert retried.suspension != current.suspension
        assert context.data.snapshot() == before_data
        assert context.returns.snapshot() == before_returns
        assert context.suspended
        assert not context.reusable
        assert runtime.uart_input == b""
        assert runtime.uart_output == b"prompt"
        current = retried

    runtime.inject_uart_input(b"\xffQ")
    final_wake = runtime.deliver_idle_wake(
        current.suspension,
        IdleWake.INTERRUPT,
    )
    completed = runtime.resume(current.suspension, final_wake)

    assert isinstance(completed, ExecutionResult)
    assert context.data.snapshot() == (11, ord("A"), 22, 0xFF, 33)
    assert context.returns.snapshot() == (0xA5,)
    assert context.reusable
    assert not context.suspended
    assert runtime.uart_input == b"Q"
    assert runtime.uart_output == b"prompt"
    with pytest.raises(ExecutionError, match="stale"):
        runtime.resume(current.suspension, final_wake)

    runtime.execute("KEY?", context=context)
    assert context.data.pop() == TRUE
    assert runtime.uart_input == b"Q"
    runtime.execute("KEY", context=context)
    assert context.data.snapshot() == (11, ord("A"), 22, 0xFF, 33, ord("Q"))
    assert runtime.uart_input == b""
    assert runtime.uart_output == b"prompt"
