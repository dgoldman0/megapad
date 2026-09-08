"""Explicit live RTC binding without semantic-step or interrupt clocking."""

from __future__ import annotations

from simulator.platform import create_one_core_address_space
from simulator.rich_terminal_host import SemanticBatchStop
from simulator.rtc import HostedRTCService, RTC_EPOCH, RTC_UPTIME
from simulator.runtime import MegaForthRuntime
from simulator.session import SimulatorMachineSession


def test_unbound_session_clock_stays_deterministic_across_host_quanta() -> None:
    memory = create_one_core_address_space(
        initial_epoch_ms=1_788_890_400_000,
        initial_uptime_ms=123,
    )
    runtime = MegaForthRuntime(memory=memory)
    runtime.evaluate(b": CLOCK-SPIN BEGIN MS@ DROP AGAIN ;")
    before = (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms)

    with SimulatorMachineSession(
        runtime, "CLOCK-SPIN", semantic_quantum_steps=128
    ) as session:
        session.boot()
        for _ in range(3):
            result = session.run_boundary()
            assert result.stop_reason is SemanticBatchStop.YIELDED
            assert result.semantic_steps > 0
            assert result.external_events_applied == 0
        assert (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms) == before


def test_bound_clock_retains_fractional_time_epoch_writes_and_latches() -> None:
    now_ns = [4_000_000_000]
    epoch = 1_788_890_400_000
    rtc = HostedRTCService(epoch, initial_uptime_ms=123)
    rtc.bind_monotonic_clock(lambda: now_ns[0])
    assert (rtc.uptime_ms, rtc.epoch_ms) == (123, epoch)

    rtc.read8(RTC_UPTIME)
    rtc.read8(RTC_EPOCH)
    latches = (rtc.uptime_latch, rtc.epoch_latch)
    now_ns[0] += 400_000
    assert rtc.uptime_ms == 123
    now_ns[0] += 600_000
    assert (rtc.uptime_ms, rtc.epoch_ms) == (124, epoch + 1)
    assert (rtc.uptime_latch, rtc.epoch_latch) == latches
    assert rtc.read8(RTC_EPOCH + 1) == (epoch >> 8) & 0xFF

    # Time elapsed while no guest ran still belongs to the live clock.  A
    # guest byte write replaces only its byte after that elapsed time settles.
    now_ns[0] += 125_000_000
    rtc.write8(RTC_EPOCH, 0x42)
    written_epoch = ((epoch + 126) & ~0xFF) | 0x42
    assert (rtc.uptime_ms, rtc.epoch_ms) == (249, written_epoch)
    now_ns[0] += 50_000_000
    assert (rtc.uptime_ms, rtc.epoch_ms) == (299, written_epoch + 50)
    assert (rtc.uptime_latch, rtc.epoch_latch) == latches
    rtc.read8(RTC_UPTIME)
    assert rtc.uptime_latch == 299
    assert rtc.epoch_latch == epoch
    rtc.read8(RTC_EPOCH)
    assert rtc.epoch_latch == written_epoch + 50


def test_source_clock_deadline_advances_only_with_elapsed_host_time() -> None:
    now_ns = [7_000_000_000]
    memory = create_one_core_address_space(initial_epoch_ms=1_788_890_400_000)
    memory.mmio.rtc.bind_monotonic_clock(lambda: now_ns[0])
    runtime = MegaForthRuntime(memory=memory)
    runtime.evaluate(
        b"VARIABLE CLOCK-READY "
        b": CLOCK-WAIT MS@ 50 + BEGIN DUP MS@ > WHILE REPEAT "
        b"DROP 1 CLOCK-READY ! ;"
    )
    ready = runtime.find("CLOCK-READY")
    assert ready is not None

    with SimulatorMachineSession(
        runtime, "CLOCK-WAIT", semantic_quantum_steps=128
    ) as session:
        session.boot()
        for _ in range(3):
            result = session.run_boundary()
            assert result.stop_reason is SemanticBatchStop.YIELDED
            assert result.semantic_steps > 0
            assert result.external_events_applied == 0
        assert runtime.memory.read64(ready.body_address) == 0
        now_ns[0] += 49_000_000
        assert session.run_boundary().stop_reason is SemanticBatchStop.YIELDED
        assert runtime.memory.read64(ready.body_address) == 0
        now_ns[0] += 1_000_000
        result = session.run_boundary()
        assert result.stop_reason is SemanticBatchStop.COMPLETED
        assert result.external_events_applied == 0
        assert runtime.memory.read64(ready.body_address) == 1
        assert runtime.rtc.uptime_ms == 50
        assert runtime.rtc.epoch_ms == 1_788_890_400_050
