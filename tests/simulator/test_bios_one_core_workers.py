"""Focused one-core pseudo-BIOS worker-slot boundary coverage."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition


def _timer_state(runtime: MegaForthRuntime) -> tuple[int, int, int, int, bool]:
    timer = runtime.timer
    return (
        timer.counter,
        timer.compare,
        timer.control,
        timer.status,
        timer.irq_pending,
    )


def test_core_status_reports_only_the_idle_core_zero_worker_slot() -> None:
    runtime = MegaForthRuntime()
    other = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(0)
    context.returns.push(0xBEEF)

    runtime.execute("CORE-STATUS", context=context)

    assert context.data.snapshot() == (0xCAFE, 0)
    assert context.returns.snapshot() == (0xBEEF,)
    other_context = other.new_context()
    other_context.data.push(0)
    other.execute("CORE-STATUS", context=other_context)
    assert other_context.data.snapshot() == (0,)


@pytest.mark.parametrize("core_id", (1, MASK64))
def test_core_status_rejects_every_nonzero_uint64_without_consuming_it(
    core_id: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(core_id)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match="accepts only core ID 0"):
        runtime.execute("CORE-STATUS", context=context)

    assert context.data.snapshot() == (0xCAFE, core_id)
    assert context.returns.snapshot() == (0xBEEF,)


@pytest.mark.parametrize("live_xt", (True, False))
def test_wake_core_never_resolves_or_runs_an_xt_and_retains_both_operands(
    live_xt: bool,
) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b"VARIABLE WAKE-RAN : WAKE-BODY 1 WAKE-RAN +! ;")
    wake_ran = runtime.find("WAKE-RAN")
    body = runtime.find("WAKE-BODY")
    assert wake_ran is not None
    assert body is not None
    xt = body.xt if live_xt else 0xDEAD_BEEF
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(xt)
    context.data.push(0 if live_xt else MASK64)
    context.returns.push(0xBEEF)

    with pytest.raises(ExecutionError, match="no secondary core exists"):
        runtime.execute("WAKE-CORE", context=context)

    assert context.data.snapshot() == (
        0xCAFE,
        xt,
        0 if live_xt else MASK64,
    )
    assert context.returns.snapshot() == (0xBEEF,)
    assert runtime.memory.read64(wake_ran.body_address) == 0
    status_context = runtime.new_context()
    status_context.data.push(0)
    runtime.execute("CORE-STATUS", context=status_context)
    assert status_context.data.snapshot() == (0,)


def test_worker_words_have_native_order_and_no_hidden_side_effects() -> None:
    runtime = MegaForthRuntime()
    spin_release = runtime.find("SPIN!")
    wake = runtime.find("WAKE-CORE")
    status = runtime.find("CORE-STATUS")
    assert spin_release is not None
    assert wake is not None
    assert status is not None
    assert isinstance(wake.implementation, PrimitiveDefinition)
    assert isinstance(status.implementation, PrimitiveDefinition)
    assert runtime.memory.read64(wake.header_address) == spin_release.header_address
    assert runtime.memory.read64(status.header_address) == wake.header_address

    runtime.timer.write_control(0)
    before_timer = _timer_state(runtime)
    before_locks = runtime.spinlocks.owners
    before_uart = runtime.uart_output
    context = runtime.new_context()
    context.data.push(0x1234)
    context.data.push(0)

    with pytest.raises(ExecutionError, match="WAKE-CORE is unavailable"):
        runtime.execute("WAKE-CORE", context=context)

    assert context.data.snapshot() == (0x1234, 0)
    assert not context.suspended
    assert context.reusable
    assert _timer_state(runtime) == before_timer
    assert runtime.spinlocks.owners == before_locks
    assert runtime.uart_output == before_uart
