"""Public progress identifies the scheduler model that produced its cycles."""

from __future__ import annotations

from contextlib import contextmanager

import pytest

from asm import assemble
from emulator.session import MachineSession
from emulator.shared_session import SharedMachine
from emulator.system import MegapadSystem
from rich_terminal import AdmissionStatus, EgressWatermarks, HostPortLimits


@contextmanager
def _machine():
    system = MegapadSystem(
        ram_size=4096, num_cores=1, num_clusters=0, hbw_size=0,
        ext_mem_size=0, vram_size=0, worker_count=1, realtime_clock=False,
    )
    system.load_binary(0, assemble("ldi r4, 7\ninc r4\nhalt"))
    with MachineSession(system) as session:
        session.boot()
        yield system, session


def _run(system, model, *, instructions=100, cycles=1000):
    if model == "instruction_batched":
        return system.run_batch_stats(instructions)
    return system.run_cycle_batch(cycles, max_instructions=instructions)


def _assert_model(stats, model):
    assert stats.timing_model == model
    assert stats.models_shared_clock_latency is (model == "strict_shared_clock")


@pytest.mark.parametrize("model", ["instruction_batched", "strict_shared_clock"])
def test_real_execution_labels_the_actual_model_and_preserves_progress_counts(model):
    with _machine() as (system, _session):
        start = system._native_system.system_cycles
        stats = _run(system, model)
        _assert_model(stats, model)
        assert stats.native_scheduler
        assert system.cpu.halted and system.cpu.regs[4] == 8
        assert stats.instructions_executed == 3
        assert stats.per_core_instructions == (3,)
        assert stats.system_cycles_advanced == system._native_system.system_cycles - start
        assert stats.system_cycles_advanced > 0
        assert stats.stop_cycle == system._native_system.system_cycles
        assert stats.system_stop_reason == "all_halted"

        # Exhaustion still carries the selected model without inventing work.
        stopped = _run(system, model)
        _assert_model(stopped, model)
        assert stopped.instructions_executed == stopped.system_cycles_advanced == 0
        assert stopped.stop_cycle == stats.stop_cycle


@pytest.mark.parametrize(("model", "instructions", "cycles"), [
    ("instruction_batched", 0, 1000),
    ("strict_shared_clock", 0, 1000),
    ("strict_shared_clock", 100, 0),
])
def test_zero_budgets_retain_the_requested_model_without_mutating_execution(model, instructions, cycles):
    with _machine() as (system, _session):
        before = (system.cpu.pc, system.cpu.cycle_count,
                  system._native_system.system_cycles, system.timer.counter)
        stats = _run(system, model, instructions=instructions, cycles=cycles)
        _assert_model(stats, model)
        assert stats.instructions_executed == stats.system_cycles_advanced == 0
        assert before == (system.cpu.pc, system.cpu.cycle_count,
                          system._native_system.system_cycles, system.timer.counter)


@pytest.mark.parametrize("model", ["instruction_batched", "strict_shared_clock"])
def test_host_backpressure_retains_requested_model_before_any_guest_work(model):
    with _machine() as (system, _session):
        limits = HostPortLimits(
            egress=EgressWatermarks(high_bytes=4, low_bytes=0,
                                   high_batches=2, low_batches=0),
            retained_publication_bytes=4, ingress_bytes=8, ingress_events=4,
            ingress_control_bytes=2, ingress_control_events=1, geometry_events=2,
        )
        lease = system.attach_rich_terminal(limits)
        try:
            for payload in (b"ABC", b"XY"):
                for value in payload:
                    system.cpu._cs.uart_write8(0, value)
                assert system._drain_native_uart_output() == payload
            assert system.rich_terminal_host.retained_publication.payload == b"XY"
            before = (system.cpu.pc, system._native_system.system_cycles)
            stats = _run(system, model)
            _assert_model(stats, model)
            assert stats.system_stop_reason == "host_backpressure"
            assert stats.instructions_executed == stats.system_cycles_advanced == 0
            assert before == (system.cpu.pc, system._native_system.system_cycles)
        finally:
            assert lease.close() is AdmissionStatus.ACCEPTED


def test_shared_session_reports_instruction_batch_policy_even_for_single_steps():
    with _machine() as (_system, session):
        owner = SharedMachine(session)
        owner.paused = True
        before = owner.status(detailed=False)["runtime"]["timing"]
        assert before == {
            "model": "instruction_batched",
            "models_shared_clock_latency": False,
            "timer_unit": "mp64_system_cycle",
            "rtc_mode": "virtual",
        }
        result = owner.step(1)
        assert result["executed"] == 1
        assert result["status"]["runtime"]["timing"] == before
