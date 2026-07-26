"""Phase 4 Element 5 coordinator GIL-scope oracles."""

from __future__ import annotations

import threading

import pytest

from asm import assemble
from devices import MMIO_BASE, SYSINFO_BASE, UART_BASE
from system import MegapadSystem


def _system(*, worker_count: int) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=4,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )


def _result_signature(stats) -> tuple:
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.external_events_applied,
        stats.system_stop_reason,
    )


def _native_mmio_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    system.load_binary(0, assemble("ld.b r4, r1"))
    system.boot(entry=0)
    system._scheduler_cursor = 2
    system.cores[0]._cs.uart_inject(b"ABCD")
    uart_rx_address = MMIO_BASE + UART_BASE + 0x01
    python_callbacks = []

    for cpu in system.cores:
        cpu.regs[1] = uart_rx_address

        def reject_python_mmio(address, *, core_id=cpu.core_id):
            python_callbacks.append((core_id, address))
            raise AssertionError("native UART read reached Python MMIO")

        cpu._mmio_read8 = reject_python_mmio

    stats = system.run_batch_stats(4)
    return (
        _result_signature(stats),
        system._scheduler_cursor,
        tuple(cpu.regs[4] for cpu in system.cores),
        tuple(cpu.pc for cpu in system.cores),
        system.uart.rx_pending,
        tuple(python_callbacks),
    )


def test_complete_native_mmio_pass_is_lane_width_independent() -> None:
    signatures = {
        worker_count: _native_mmio_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    result, cursor, values, pcs, rx_pending, callbacks = signatures[1]
    assert result[0] == 4
    assert result[2] == (1, 1, 1, 1)
    assert result[4] == (1, 1, 1, 1)
    assert cursor == 2
    assert values == (ord("C"), ord("D"), ord("A"), ord("B"))
    assert pcs == (2, 2, 2, 2)
    assert rx_pending == 0
    assert callbacks == ()


@pytest.mark.parametrize(
    (
        "boundary",
        "expected_reason",
        "expected_trap",
        "expected_cycles",
    ),
    (
        pytest.param("trap", 5, 6, 0, id="trap"),
        pytest.param("reset", 6, -1, 1, id="reset"),
    ),
)
@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_cold_terminal_continuations_retain_cyclic_python_scope(
    boundary: str,
    expected_reason: int,
    expected_trap: int,
    expected_cycles: int,
    worker_count: int,
) -> None:
    system = _system(worker_count=worker_count)
    system.load_binary(0, assemble(boundary))
    system.boot(entry=0)
    system._scheduler_cursor = 2
    original_settlement = (
        system._settle_native_core_continuation
    )
    caller_thread = threading.get_ident()
    settlements = []

    def observe_settlement(*args):
        settlements.append(
            (*args, threading.get_ident())
        )
        return original_settlement(*args)

    system._settle_native_core_continuation = (
        observe_settlement
    )

    stats = system.run_batch_stats(4)

    assert settlements == [
        (
            core_id,
            expected_reason,
            expected_trap,
            0,
            0,
            caller_thread,
        )
        for core_id in (2, 3, 0, 1)
    ]
    expected_reasons = [0] * 7
    expected_reasons[expected_reason] = 1
    assert stats.instructions_executed == 4
    assert stats.system_cycles_advanced == expected_cycles
    assert stats.per_core_instructions == (1, 1, 1, 1)
    assert stats.per_core_cycles == (
        expected_cycles,
        expected_cycles,
        expected_cycles,
        expected_cycles,
    )
    assert stats.per_core_dispatches == (1, 1, 1, 1)
    assert stats.per_core_stop_reasons == (
        tuple(expected_reasons),
        tuple(expected_reasons),
        tuple(expected_reasons),
        tuple(expected_reasons),
    )
    assert stats.native_continuations == 4


def _joined_ingress_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    system.load_binary(0, assemble("ld.b r4, r1"))
    system.boot(entry=0)
    system._scheduler_cursor = 3
    sysinfo_address = MMIO_BASE + SYSINFO_BASE + 0x10
    caller_thread = threading.get_ident()
    callback_trace = []
    injection_trace = []
    worker_failures = []
    join_timeouts = []
    workers = []

    for cpu in system.cores:
        core_id = cpu.core_id
        cpu.regs[1] = sysinfo_address
        original_read = cpu._mmio_read8

        def joined_read(
            address,
            *,
            observed_core=core_id,
            read=original_read,
        ):
            callback_trace.append(
                (observed_core, threading.get_ident())
            )

            def inject():
                try:
                    sequence = system.schedule_uart_input(
                        bytes((0x41 + observed_core,))
                    )
                    injection_trace.append(
                        (observed_core, sequence)
                    )
                except BaseException as exc:
                    worker_failures.append(exc)

            worker = threading.Thread(target=inject)
            workers.append(worker)
            worker.start()
            worker.join(timeout=2)
            if worker.is_alive():
                join_timeouts.append(observed_core)
            return read(address)

        cpu._mmio_read8 = joined_read

    stats = system.run_batch_stats(4)
    for worker in workers:
        worker.join(timeout=5)

    recording = system.export_external_ingress_recording()
    event_trace = tuple(
        (
            event["sequence"],
            event["cycle"],
            event["payload"],
            event["release_boundary"],
            event["release_phase"],
        )
        for event in recording["events"]
    )
    return (
        _result_signature(stats),
        system._scheduler_cursor,
        tuple(callback_trace),
        tuple(injection_trace),
        tuple(join_timeouts),
        tuple(str(exc) for exc in worker_failures),
        tuple(worker.is_alive() for worker in workers),
        event_trace,
        system.uart.rx_pending,
        caller_thread,
    )


def test_python_mmio_can_join_live_ingress_during_coalesced_pass() -> None:
    signatures = {
        worker_count: _joined_ingress_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2][:-1] == signatures[1][:-1]
    assert signatures[4][:-1] == signatures[1][:-1]
    (
        result,
        cursor,
        callback_trace,
        injection_trace,
        join_timeouts,
        worker_failures,
        workers_alive,
        event_trace,
        rx_pending,
        caller_thread,
    ) = signatures[1]
    assert result[0] == 4
    assert result[8] == 4
    assert cursor == 3
    assert callback_trace == (
        (3, caller_thread),
        (0, caller_thread),
        (1, caller_thread),
        (2, caller_thread),
    )
    assert injection_trace == (
        (3, 1),
        (0, 2),
        (1, 3),
        (2, 4),
    )
    assert join_timeouts == ()
    assert worker_failures == ()
    assert workers_alive == (False, False, False, False)
    assert tuple(
        (event[0], event[2], event[3], event[4])
        for event in event_trace
    ) == (
        (1, b"D", 1, "after_batch"),
        (2, b"A", 1, "after_batch"),
        (3, b"B", 1, "after_batch"),
        (4, b"C", 1, "after_batch"),
    )
    assert rx_pending == 4
