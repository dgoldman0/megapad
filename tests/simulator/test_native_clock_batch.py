"""Batched native work must preserve the existing one-step clock semantics."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.diagnostics import HostedDiagnosticsService
from simulator.timer import (
    HostedTimerService,
    TIMER_AUTO_RELOAD,
    TIMER_ENABLED,
    TIMER_IRQ_ENABLED,
    TIMER_MATCHED,
    UINT32_MASK,
)


_RELOAD = TIMER_ENABLED | TIMER_AUTO_RELOAD
_RELOAD_IRQ = _RELOAD | TIMER_IRQ_ENABLED
_WRAP = UINT32_MASK + 1


def _timer_state(timer: HostedTimerService) -> tuple[int, int, int, int, bool]:
    return (
        timer.counter,
        timer.compare,
        timer.control,
        timer.status,
        timer.irq_pending,
    )


def _diagnostic_state(service: HostedDiagnosticsService) -> tuple:
    return tuple(getattr(service, name) for name in service.__slots__)


@pytest.mark.parametrize("count", (0, 1, 2, 3, 5, 12, 31))
@pytest.mark.parametrize(
    ("counter", "compare", "control", "status", "irq_pending"),
    (
        (0, 3, TIMER_ENABLED, 0, False),
        (2, 3, _RELOAD, 0, False),
        (2, 3, _RELOAD_IRQ, 0x80, False),
        (3, 3, _RELOAD_IRQ, 0, False),
        (7, 3, _RELOAD_IRQ, 0, False),
        (UINT32_MASK - 1, 3, _RELOAD_IRQ, 0, False),
        (UINT32_MASK - 1, 0, _RELOAD_IRQ, 0x80, False),
        (UINT32_MASK - 1, UINT32_MASK, TIMER_ENABLED, 0, False),
        (0, 1, _RELOAD_IRQ, 0, False),
        (0, 0, _RELOAD_IRQ, 0, False),
        (2, 3, 0x80 | TIMER_AUTO_RELOAD | TIMER_IRQ_ENABLED, 0x81, True),
        (2, 3, TIMER_ENABLED, 0x80, True),
    ),
)
def test_timer_batch_matches_repeated_existing_ticks(
    counter, compare, control, status, irq_pending, count
) -> None:
    repeated = HostedTimerService(
        counter=counter,
        compare=compare,
        control=control,
        status=status,
        irq_pending=irq_pending,
    )
    batched = repeated.clone()

    for _ in range(count):
        repeated.advance()
    batched.advance_by(count)

    assert _timer_state(batched) == _timer_state(repeated)


@pytest.mark.parametrize(
    ("counter", "compare", "control", "count", "expected_counter", "matched"),
    (
        (7, 7, _RELOAD_IRQ, _WRAP - 1, 6, False),
        (7, 7, _RELOAD_IRQ, _WRAP, 0, True),
        (7, 7, _RELOAD_IRQ, _WRAP + 7 * (1 << 100) + 3, 3, True),
        (0, 0, _RELOAD_IRQ, _WRAP - 1, UINT32_MASK, False),
        (0, 0, _RELOAD_IRQ, _WRAP, 0, True),
        (0, 0, _RELOAD_IRQ, (1 << 100) + 7, 7, True),
        (UINT32_MASK - 1, 0, _RELOAD_IRQ, (1 << 100) + 7, 5, True),
        (0, 3, _RELOAD_IRQ, 3 * (1 << 100) + 2, 2, True),
        (0, 1, _RELOAD_IRQ, 1 << 100, 0, True),
        (UINT32_MASK - 1, 4, TIMER_ENABLED, (1 << 100) + 7, 5, True),
        (7, 7, TIMER_ENABLED, _WRAP, 7, True),
        (7, 7, TIMER_AUTO_RELOAD | TIMER_IRQ_ENABLED, 1 << 100, 7, False),
    ),
)
def test_timer_large_batches_cover_first_wrap_and_reload_periods(
    counter, compare, control, count, expected_counter, matched
) -> None:
    timer = HostedTimerService(
        counter=counter, compare=compare, control=control, status=0x80
    )

    timer.advance_by(count)

    assert _timer_state(timer) == (
        expected_counter,
        compare,
        control,
        0x80 | (TIMER_MATCHED if matched else 0),
        bool(matched and control & TIMER_IRQ_ENABLED),
    )


def test_timer_batch_respects_acknowledgement_and_control_changes() -> None:
    timer = HostedTimerService(compare=3, control=_RELOAD_IRQ, status=0x80)
    timer.advance_by(7)
    assert (timer.counter, timer.status, timer.irq_pending) == (1, 0x81, True)

    timer.acknowledge()
    timer.advance_by(1)
    assert (timer.counter, timer.status, timer.irq_pending) == (2, 0x80, False)

    timer.write_control(TIMER_ENABLED)
    timer.advance_by(1)
    assert (timer.counter, timer.status, timer.irq_pending) == (3, 0x81, False)

    timer.write_control(TIMER_ENABLED | TIMER_IRQ_ENABLED)
    timer.advance_by(0)
    assert not timer.irq_pending
    timer.write_compare(5)
    timer.advance_by(2)
    assert (timer.counter, timer.status, timer.irq_pending) == (5, 0x81, True)

    timer.write_control(0)
    timer.advance_by(1 << 100)
    assert (timer.counter, timer.status, timer.irq_pending) == (5, 0x81, True)


@pytest.mark.parametrize("count", (0, 1, 2, 7, 31))
@pytest.mark.parametrize("perf_enabled", (False, True))
def test_diagnostic_batch_matches_repeated_existing_work(count, perf_enabled) -> None:
    repeated = HostedDiagnosticsService(
        perf_cycles=MASK64 - 2, semantic_cycles=MASK64 - 1
    )
    repeated._perf_enabled = perf_enabled
    repeated._perf_tileops = 19
    repeated._perf_stalls = 23
    repeated._perf_extmem = 29
    repeated.disable_icache()
    batched = repeated.clone()

    for _ in range(count):
        repeated.account_work()
    batched.account_work_many(count)

    assert _diagnostic_state(batched) == _diagnostic_state(repeated)


@pytest.mark.parametrize("perf_enabled", (False, True))
def test_diagnostic_batch_handles_arbitrary_counts_and_reset(perf_enabled) -> None:
    service = HostedDiagnosticsService(
        perf_cycles=MASK64 - 2, semantic_cycles=MASK64 - 1
    )
    service._perf_enabled = perf_enabled

    service.account_work_many((1 << 100) + 7)

    assert service.semantic_cycles == 5
    assert service.perf_cycles == (4 if perf_enabled else MASK64 - 2)
    service.reset_performance()
    service.account_work_many(3)
    assert (service.semantic_cycles, service.perf_cycles) == (8, 3)


@pytest.mark.parametrize(
    ("count", "error"),
    ((True, TypeError), (False, TypeError), (None, TypeError),
     (1.5, TypeError), ("2", TypeError), (-1, ValueError)),
)
def test_invalid_batch_counts_leave_both_services_unchanged(count, error) -> None:
    # Validate counts even when the timer is disabled.
    timer = HostedTimerService(counter=17, control=0, status=0x81, irq_pending=True)
    diagnostics = HostedDiagnosticsService(perf_cycles=31, semantic_cycles=47)
    timer_before = _timer_state(timer)
    diagnostic_before = _diagnostic_state(diagnostics)

    with pytest.raises(error):
        timer.advance_by(count)
    with pytest.raises(error):
        diagnostics.account_work_many(count)

    assert _timer_state(timer) == timer_before
    assert _diagnostic_state(diagnostics) == diagnostic_before
