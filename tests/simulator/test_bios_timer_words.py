"""Focused deterministic coverage for the hosted pseudo-BIOS Timer words."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.diagnostics import HostedDiagnosticsService
from simulator.ir import Idle, Return
from simulator.runtime import (
    BlockedExecution,
    ExecutionResult,
    IdleWake,
    MegaForthRuntime,
    PrimitiveDefinition,
)
from simulator.timer import (
    HostedTimerService,
    TIMER_AUTO_RELOAD,
    TIMER_ENABLED,
    TIMER_IRQ_ENABLED,
    TIMER_MATCHED,
    UINT32_MASK,
)


def test_timer_reset_state_and_bios_words_are_runtime_local() -> None:
    runtime = MegaForthRuntime()
    timer = runtime.timer

    assert (
        timer.counter,
        timer.compare,
        timer.control,
        timer.status,
        timer.irq_pending,
    ) == (0, UINT32_MASK, TIMER_ENABLED, 0, False)
    ordered_names = (
        "CYCLES",
        "TIMER!",
        "TIMER-CTRL!",
        "TIMER-ACK",
        "PERF-STALLS",
    )
    ordered_words = []
    for name in ordered_names:
        word = runtime.find(name)
        assert word is not None
        assert isinstance(word.implementation, PrimitiveDefinition)
        ordered_words.append(word)
    for previous, current in zip(ordered_words, ordered_words[1:]):
        assert runtime.memory.read64(current.header_address) == (
            previous.header_address
        )
    with pytest.raises(AttributeError):
        timer.counter = 1  # type: ignore[misc]


@pytest.mark.parametrize(
    ("action", "error"),
    (
        (lambda: HostedTimerService(counter=True), TypeError),
        (lambda: HostedTimerService(counter=-1), ValueError),
        (lambda: HostedTimerService(counter=UINT32_MASK + 1), ValueError),
        (lambda: HostedTimerService(compare=False), TypeError),
        (lambda: HostedTimerService(control=0x100), ValueError),
        (lambda: HostedTimerService(status=0x100), ValueError),
        (lambda: HostedTimerService(irq_pending=1), TypeError),
        (lambda: HostedTimerService().write_compare(True), TypeError),
        (lambda: HostedTimerService().write_compare(MASK64 + 1), ValueError),
        (lambda: HostedTimerService().write_control(-1), ValueError),
        (
            lambda: MegaForthRuntime(timer=object()),  # type: ignore[arg-type]
            TypeError,
        ),
    ),
)
def test_timer_host_boundary_rejects_invalid_state(action, error) -> None:
    with pytest.raises(error):
        action()


def test_timer_words_truncate_cells_and_preserve_exact_stack_effects() -> None:
    seed = HostedTimerService(
        control=0,
        status=0x81,
        irq_pending=True,
    )
    runtime = MegaForthRuntime(timer=seed)
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(0x1234_5678_9ABC_DEF0)

    runtime.execute("TIMER!", context=context)

    assert context.data.snapshot() == (0xCAFE,)
    assert runtime.timer.compare == 0x9ABC_DEF0
    assert runtime.timer.control == 0
    assert runtime.timer.status == 0x81
    assert runtime.timer.irq_pending

    context.data.push(0xABCD_EF01_2345_6786)
    runtime.execute("TIMER-CTRL!", context=context)
    assert context.data.snapshot() == (0xCAFE,)
    assert runtime.timer.control == 0x86
    assert runtime.timer.status == 0x81
    assert runtime.timer.irq_pending

    before_counter = runtime.timer.counter
    runtime.execute("TIMER-ACK", context=context)
    assert context.data.snapshot() == (0xCAFE,)
    assert runtime.timer.counter == before_counter
    assert runtime.timer.compare == 0x9ABC_DEF0
    assert runtime.timer.control == 0x86
    assert runtime.timer.status == 0x80
    assert not runtime.timer.irq_pending


def test_disabled_timer_freezes_while_semantic_diagnostics_continue() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0)
    runtime.execute("TIMER-CTRL!", context=context)
    frozen = runtime.timer.counter
    semantic_before = runtime.diagnostics.semantic_cycles

    runtime.execute("TRUE", context=context)
    runtime.execute("CYCLES", context=context)

    assert context.data.snapshot() == (MASK64, frozen)
    assert runtime.timer.counter == frozen
    assert runtime.diagnostics.semantic_cycles == semantic_before + 2


def test_counter_wrap_match_and_autoreload_follow_enabled_ticks() -> None:
    equal_before_tick = HostedTimerService(
        counter=7,
        compare=7,
        control=TIMER_ENABLED,
    )
    assert equal_before_tick.status == 0
    equal_before_tick.advance()
    assert (equal_before_tick.counter, equal_before_tick.status) == (8, 0)

    wrapping = HostedTimerService(
        counter=UINT32_MASK - 1,
        compare=UINT32_MASK,
        control=TIMER_ENABLED,
    )
    wrapping.advance()
    assert (wrapping.counter, wrapping.status) == (
        UINT32_MASK,
        TIMER_MATCHED,
    )
    wrapping.advance()
    assert wrapping.counter == 0
    assert wrapping.status == TIMER_MATCHED

    zero_match = HostedTimerService(
        counter=UINT32_MASK,
        compare=0,
        control=TIMER_ENABLED | TIMER_AUTO_RELOAD,
    )
    zero_match.advance()
    assert (zero_match.counter, zero_match.status) == (0, TIMER_MATCHED)

    reloading = HostedTimerService(
        counter=4,
        compare=5,
        control=TIMER_ENABLED | TIMER_AUTO_RELOAD,
    )
    reloading.advance()
    assert (reloading.counter, reloading.status) == (0, TIMER_MATCHED)


def test_match_status_and_irq_latch_are_sticky_until_acknowledged() -> None:
    timer = HostedTimerService(
        compare=1,
        control=TIMER_ENABLED,
        status=0x80,
    )
    timer.advance()
    assert timer.status == 0x81
    assert not timer.irq_pending

    timer.write_control(TIMER_ENABLED | TIMER_IRQ_ENABLED)
    assert timer.status == 0x81
    assert not timer.irq_pending
    timer.write_compare(2)
    timer.advance()
    assert timer.status == 0x81
    assert timer.irq_pending

    timer.write_control(TIMER_ENABLED)
    assert timer.irq_pending
    timer.acknowledge()
    assert timer.status == 0x80
    assert not timer.irq_pending


def test_injected_timer_profile_is_cloned_per_runtime() -> None:
    profile = HostedTimerService(
        counter=9,
        compare=25,
        control=TIMER_ENABLED | TIMER_IRQ_ENABLED,
        status=TIMER_MATCHED,
        irq_pending=True,
    )
    first = MegaForthRuntime(timer=profile)
    second = MegaForthRuntime(timer=profile)

    assert first.timer is not profile
    assert second.timer is not profile
    assert first.timer is not second.timer
    first.execute("TRUE")
    first.timer.acknowledge()

    assert (first.timer.counter, first.timer.status, first.timer.irq_pending) == (
        10,
        0,
        False,
    )
    for untouched in (profile, second.timer):
        assert (untouched.counter, untouched.status, untouched.irq_pending) == (
            9,
            TIMER_MATCHED,
            True,
        )


def test_cycles_observes_its_own_pre_read_timer_tick() -> None:
    runtime = MegaForthRuntime(
        diagnostics=HostedDiagnosticsService(semantic_cycles=100),
        timer=HostedTimerService(counter=40),
    )
    context = runtime.new_context()

    result = runtime.execute("CYCLES", context=context)

    assert result.semantic_steps == 1
    assert context.data.snapshot() == (41,)
    assert runtime.timer.counter == 41
    assert runtime.diagnostics.semantic_cycles == 101


def test_idle_ticks_before_suspension_but_pending_irq_does_not_wake_it() -> None:
    runtime = MegaForthRuntime(
        timer=HostedTimerService(
            compare=1,
            control=(
                TIMER_ENABLED | TIMER_IRQ_ENABLED | TIMER_AUTO_RELOAD
            ),
        )
    )
    runtime.define_colon("TIMER-IDLE", (Idle(), Return()))
    context = runtime.new_context()

    blocked = runtime.run_until_blocked("TIMER-IDLE", context=context)

    assert isinstance(blocked, BlockedExecution)
    assert blocked.semantic_steps == 1
    assert context.suspended
    assert (runtime.timer.counter, runtime.timer.status) == (0, TIMER_MATCHED)
    assert runtime.timer.irq_pending

    detached_state = (
        runtime.timer.counter,
        runtime.timer.status,
        runtime.timer.irq_pending,
        runtime.diagnostics.semantic_cycles,
    )
    receipt = runtime.deliver_idle_wake(
        blocked.suspension,
        IdleWake.INTERRUPT,
    )
    assert (
        runtime.timer.counter,
        runtime.timer.status,
        runtime.timer.irq_pending,
        runtime.diagnostics.semantic_cycles,
    ) == detached_state

    completed = runtime.resume(blocked.suspension, receipt)
    assert isinstance(completed, ExecutionResult)
    assert completed.semantic_steps == 2
    assert not context.suspended
    assert (runtime.timer.counter, runtime.timer.status) == (0, TIMER_MATCHED)
    assert runtime.timer.irq_pending
