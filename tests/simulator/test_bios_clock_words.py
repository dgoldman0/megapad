"""Focused acceptance for the hosted BIOS uptime and epoch clock surface."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.memory import MMIO_BASE, MMIOAccessError
from simulator.platform import create_one_core_address_space
from simulator.rtc import (
    RTC_EPOCH,
    RTC_EPOCH_LIMIT,
    RTC_EPOCH_STAT,
    RTC_UPTIME,
    RTC_UPTIME_LIMIT,
    RTC_UPTIME_SIZE,
    HostedRTCService,
    RTCAccessError,
)
from simulator.runtime import MegaForthRuntime


def test_epoch_low_byte_latches_all_later_bytes() -> None:
    rtc = HostedRTCService(0x0102_0304_0506_0708)

    assert rtc.read8(RTC_EPOCH + 7) == 0
    assert rtc.read8(RTC_EPOCH) == 0x08
    assert rtc.epoch_latch == 0x0102_0304_0506_0708
    rtc.set_epoch_ms(0xA1A2_A3A4_A5A6_A7A8)
    assert bytes(rtc.read8(RTC_EPOCH + index) for index in range(1, 8)) == (
        bytes.fromhex("07 06 05 04 03 02 01")
    )

    assert rtc.read8(RTC_EPOCH) == 0xA8
    assert bytes(rtc.read8(RTC_EPOCH + index) for index in range(1, 8)) == (
        bytes.fromhex("A7 A6 A5 A4 A3 A2 A1")
    )


def test_epoch_writes_and_host_advance_mutate_only_the_current_value() -> None:
    rtc = HostedRTCService(
        0x1122_3344_5566_7788,
        initial_uptime_ms=0x8877_6655_4433_2211,
    )
    assert rtc.read8(RTC_EPOCH) == 0x88
    latched = rtc.epoch_latch
    uptime_before = (rtc.uptime_ms, rtc.uptime_latch)

    # Bytes 0-6 only stage; the byte-7 write commits all eight at once.
    replacement = 0xA1A2_A3A4_A5A6_A7A8
    for index in range(7):
        rtc.write8(RTC_EPOCH + index, (replacement >> (8 * index)) & 0xFF)
    assert rtc.epoch_ms == 0x1122_3344_5566_7788
    assert rtc.epoch_stage == replacement & ((1 << 56) - 1)
    rtc.write8(RTC_EPOCH + 7, replacement >> 56)
    assert rtc.epoch_ms == replacement
    assert rtc.epoch_latch == latched
    rtc.set_epoch_ms(MASK64 - 1)
    rtc.advance_ms(3)
    assert rtc.epoch_ms == 1
    assert rtc.epoch_latch == latched
    assert (rtc.uptime_ms, rtc.uptime_latch) == uptime_before


def test_wall_clock_valid_bit_follows_commits_and_host_seeds() -> None:
    unset = HostedRTCService()
    assert not unset.epoch_valid
    assert unset.read8(RTC_EPOCH_STAT) == 0
    unset.advance_ms(5)
    for index in range(7):
        unset.write8(RTC_EPOCH + index, 0)
    assert not unset.epoch_valid
    unset.write8(RTC_EPOCH + 7, 0)
    assert unset.epoch_valid
    assert unset.read8(RTC_EPOCH_STAT) == 1

    # An explicit initial epoch, even zero, and a later host seed both set it.
    assert HostedRTCService(0).epoch_valid
    seeded = HostedRTCService()
    seeded.set_epoch_ms(1_760_000_000_000)
    assert seeded.epoch_valid
    assert seeded.read8(RTC_EPOCH_STAT) == 1


def test_wall_clock_valid_bit_is_read_only() -> None:
    rtc = HostedRTCService()
    rtc.preflight(RTC_EPOCH_STAT, 1, write=False)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH_STAT, 1, write=True)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH_STAT, 2, write=False)
    with pytest.raises(RTCAccessError):
        rtc.write8(RTC_EPOCH_STAT, 1)
    assert not rtc.epoch_valid


def test_platform_routes_the_wall_clock_valid_bit() -> None:
    unset = create_one_core_address_space()
    assert unset.read8(MMIO_BASE + RTC_EPOCH_STAT) == 0
    unset.write64(MMIO_BASE + RTC_EPOCH, 1_760_000_000_000)
    assert unset.read8(MMIO_BASE + RTC_EPOCH_STAT) == 1
    assert unset.read64(MMIO_BASE + RTC_EPOCH) == 1_760_000_000_000
    seeded = create_one_core_address_space(initial_epoch_ms=0)
    assert seeded.read8(MMIO_BASE + RTC_EPOCH_STAT) == 1


def test_interrupted_wide_epoch_write_commits_nothing() -> None:
    class FaultingRTC(HostedRTCService):
        def write8(self, offset: int, value: int) -> None:
            if offset == RTC_EPOCH + 4:
                raise RuntimeError("injected epoch byte fault")
            super().write8(offset, value)

    memory = create_one_core_address_space()
    assert memory.mmio is not None
    faulting = FaultingRTC(1000)
    memory.mmio.rtc = faulting
    with pytest.raises(MMIOAccessError):
        memory.write64(MMIO_BASE + RTC_EPOCH, 1_760_000_000_000)
    assert faulting.epoch_ms == 1000


def test_uptime_low_byte_latches_independently_from_epoch() -> None:
    rtc = HostedRTCService(
        0x1112_1314_1516_1718,
        initial_uptime_ms=0x0102_0304_0506_0708,
    )

    assert rtc.read8(RTC_UPTIME + 7) == 0
    assert rtc.read8(RTC_UPTIME) == 0x08
    assert rtc.uptime_latch == 0x0102_0304_0506_0708
    rtc.set_uptime_ms(0xA1A2_A3A4_A5A6_A7A8)
    assert bytes(rtc.read8(RTC_UPTIME + index) for index in range(1, 8)) == (
        bytes.fromhex("07 06 05 04 03 02 01")
    )

    assert rtc.read8(RTC_EPOCH) == 0x18
    assert rtc.epoch_latch == 0x1112_1314_1516_1718
    assert rtc.uptime_latch == 0x0102_0304_0506_0708
    assert rtc.read8(RTC_UPTIME) == 0xA8
    assert bytes(rtc.read8(RTC_UPTIME + index) for index in range(1, 8)) == (
        bytes.fromhex("A7 A6 A5 A4 A3 A2 A1")
    )


def test_uptime_advance_wraps_without_changing_epoch_or_latches() -> None:
    rtc = HostedRTCService(
        0x1122_3344_5566_7788,
        initial_uptime_ms=0x0102_0304_0506_0708,
    )
    rtc.read8(RTC_UPTIME)
    rtc.read8(RTC_EPOCH)
    latches_before = (rtc.uptime_latch, rtc.epoch_latch)
    epoch_before = rtc.epoch_ms

    rtc.set_uptime_ms(MASK64 - 1)
    rtc.advance_uptime_ms(3)

    assert rtc.uptime_ms == 1
    assert rtc.epoch_ms == epoch_before
    assert (rtc.uptime_latch, rtc.epoch_latch) == latches_before


@pytest.mark.parametrize(
    ("action", "error"),
    (
        (lambda: HostedRTCService(True), TypeError),
        (lambda: HostedRTCService(-1), ValueError),
        (lambda: HostedRTCService(MASK64 + 1), ValueError),
        (lambda: HostedRTCService(0, initial_uptime_ms=True), TypeError),
        (lambda: HostedRTCService(0, initial_uptime_ms=-1), ValueError),
        (lambda: HostedRTCService().set_epoch_ms(False), TypeError),
        (lambda: HostedRTCService().advance_ms(-1), ValueError),
        (lambda: HostedRTCService().advance_ms(True), TypeError),
        (lambda: HostedRTCService().set_uptime_ms(False), TypeError),
        (lambda: HostedRTCService().advance_uptime_ms(-1), ValueError),
        (lambda: HostedRTCService().advance_uptime_ms(True), TypeError),
    ),
)
def test_clock_host_controls_reject_invalid_values(action, error) -> None:
    with pytest.raises(error):
        action()


def test_clock_aperture_accepts_only_complete_supported_widths() -> None:
    rtc = HostedRTCService()

    assert RTC_UPTIME_LIMIT == RTC_UPTIME + RTC_UPTIME_SIZE
    rtc.preflight(RTC_UPTIME, 8, write=False)
    rtc.preflight(RTC_EPOCH, 8, write=False)
    rtc.preflight(RTC_EPOCH + 7, 1, write=True)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_UPTIME, 8, write=True)
    with pytest.raises(RTCAccessError):
        rtc.write8(RTC_UPTIME, 0)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_UPTIME_LIMIT - 1, 2, write=False)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH + 7, 2, write=False)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH, 3, write=False)
    with pytest.raises(RTCAccessError):
        rtc.read8(RTC_EPOCH_LIMIT)


def test_platform_routes_configured_uptime_as_a_read_only_latched_clock() -> None:
    uptime = 0x0102_0304_0506_0708
    epoch = 0x1112_1314_1516_1718
    memory = create_one_core_address_space(
        initial_epoch_ms=epoch,
        initial_uptime_ms=uptime,
    )

    assert memory.read64(MMIO_BASE + RTC_UPTIME) == uptime
    assert memory.mmio is not None
    assert memory.mmio.rtc.uptime_latch == uptime
    with pytest.raises(MMIOAccessError, match="rejected write preflight"):
        memory.write64(MMIO_BASE + RTC_UPTIME, 0)
    assert memory.mmio.rtc.uptime_ms == uptime
    assert memory.mmio.rtc.epoch_ms == epoch


def test_platform_routes_full_width_epoch_reads_through_the_latch() -> None:
    value = 0x0102_0304_0506_0708
    memory = create_one_core_address_space(initial_epoch_ms=value)

    assert memory.read64(MMIO_BASE + RTC_EPOCH) == value
    assert memory.mmio is not None
    assert memory.mmio.rtc.epoch_latch == value
    replacement = 0xA1A2_A3A4_A5A6_A7A8
    memory.write64(MMIO_BASE + RTC_EPOCH, replacement)
    assert memory.mmio.rtc.epoch_ms == replacement
    assert memory.mmio.rtc.epoch_latch == value
    assert memory.read64(MMIO_BASE + RTC_EPOCH) == replacement


def test_ms_fetch_reconstructs_latched_uptime_and_preserves_lower_cells() -> None:
    value = 0xFEDC_BA98_7654_3210
    memory = create_one_core_address_space(initial_uptime_ms=value)
    runtime = MegaForthRuntime(memory=memory)
    context = runtime.new_context()
    context.data.push(0xA55A)

    runtime.execute("MS@", context=context)

    assert context.data.snapshot() == (0xA55A, value)
    assert context.returns.snapshot() == ()
    assert runtime.rtc.uptime_latch == value


def test_ms_fetch_publishes_no_partial_cell_after_later_mmio_fault() -> None:
    class FaultingRTC(HostedRTCService):
        def read8(self, offset: int) -> int:
            if offset == RTC_UPTIME + 4:
                raise RuntimeError("injected uptime byte fault")
            return super().read8(offset)

    runtime = MegaForthRuntime()
    assert runtime.memory.mmio is not None
    faulting = FaultingRTC(initial_uptime_ms=0x0102_0304_0506_0708)
    runtime.memory.mmio.rtc = faulting
    context = runtime.new_context()
    context.data.push(0xA55A)

    with pytest.raises(MMIOAccessError, match="failed during read"):
        runtime.execute("MS@", context=context)

    assert faulting.uptime_latch == 0x0102_0304_0506_0708
    assert context.data.snapshot() == (0xA55A,)
    assert context.returns.snapshot() == ()


def test_epoch_fetch_reconstructs_u64_and_preserves_lower_stack_cells() -> None:
    value = 0xFEDC_BA98_7654_3210
    memory = create_one_core_address_space(initial_epoch_ms=value)
    runtime = MegaForthRuntime(memory=memory)
    context = runtime.new_context()
    context.data.push(0xA55A)

    runtime.execute("EPOCH@", context=context)

    assert context.data.snapshot() == (0xA55A, value)
    assert context.returns.snapshot() == ()
    assert runtime.rtc is memory.mmio.rtc
    assert runtime.rtc.epoch_latch == value


def test_epoch_fetch_publishes_no_partial_cell_after_later_mmio_fault() -> None:
    class FaultingRTC(HostedRTCService):
        def read8(self, offset: int) -> int:
            if offset == RTC_EPOCH + 4:
                raise RuntimeError("injected epoch byte fault")
            return super().read8(offset)

    runtime = MegaForthRuntime()
    assert runtime.memory.mmio is not None
    faulting = FaultingRTC(0x0102_0304_0506_0708)
    runtime.memory.mmio.rtc = faulting
    context = runtime.new_context()
    context.data.push(0xA55A)

    with pytest.raises(MMIOAccessError, match="failed during read"):
        runtime.execute("EPOCH@", context=context)

    assert faulting.epoch_latch == 0x0102_0304_0506_0708
    assert context.data.snapshot() == (0xA55A,)
    assert context.returns.snapshot() == ()


def test_runtimes_own_independent_explicit_clock_state() -> None:
    first = MegaForthRuntime(
        memory=create_one_core_address_space(
            initial_epoch_ms=1000,
            initial_uptime_ms=100,
        )
    )
    second = MegaForthRuntime(
        memory=create_one_core_address_space(
            initial_epoch_ms=2000,
            initial_uptime_ms=200,
        )
    )

    first.rtc.advance_ms(25)
    first.rtc.advance_uptime_ms(5)

    assert first.rtc.epoch_ms == 1025
    assert second.rtc.epoch_ms == 2000
    assert first.rtc.uptime_ms == 105
    assert second.rtc.uptime_ms == 200


def test_epoch_store_sets_the_wall_clock_and_its_valid_bit() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.execute("EPOCH-VALID?", context=context)
    assert context.data.snapshot() == (0,)
    context.data.pop()

    context.data.push(0xA55A)
    context.data.push(1_760_000_000_123)
    runtime.execute("EPOCH!", context=context)
    assert context.data.snapshot() == (0xA55A,)
    assert runtime.rtc.epoch_ms == 1_760_000_000_123
    assert runtime.rtc.epoch_valid

    runtime.execute("EPOCH-VALID?", context=context)
    runtime.execute("EPOCH@", context=context)
    assert context.data.snapshot() == (0xA55A, MASK64, 1_760_000_000_123)
    assert context.returns.snapshot() == ()


def test_seeded_runtime_reports_a_valid_wall_clock() -> None:
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(initial_epoch_ms=1_000),
    )
    context = runtime.new_context()
    runtime.execute("EPOCH-VALID?", context=context)
    assert context.data.snapshot() == (MASK64,)


def test_epoch_store_fault_before_the_commit_changes_nothing() -> None:
    class FaultingRTC(HostedRTCService):
        def write8(self, offset: int, value: int) -> None:
            if offset == RTC_EPOCH + 4:
                raise RuntimeError("injected epoch byte fault")
            super().write8(offset, value)

    runtime = MegaForthRuntime()
    assert runtime.memory.mmio is not None
    faulting = FaultingRTC()
    runtime.memory.mmio.rtc = faulting
    context = runtime.new_context()
    context.data.push(0xA55A)
    context.data.push(1_760_000_000_000)

    with pytest.raises(MMIOAccessError):
        runtime.execute("EPOCH!", context=context)

    assert context.data.snapshot() == (0xA55A, 1_760_000_000_000)
    assert faulting.epoch_ms == 0
    assert not faulting.epoch_valid
