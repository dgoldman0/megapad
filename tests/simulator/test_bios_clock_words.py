"""Focused acceptance for the hosted BIOS epoch clock surface."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.memory import MMIO_BASE, MMIOAccessError
from simulator.platform import create_one_core_address_space
from simulator.rtc import (
    RTC_EPOCH,
    RTC_EPOCH_LIMIT,
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
    rtc = HostedRTCService(0x1122_3344_5566_7788)
    assert rtc.read8(RTC_EPOCH) == 0x88
    latched = rtc.epoch_latch

    rtc.write8(RTC_EPOCH + 3, 0xAA)
    assert rtc.epoch_ms == 0x1122_3344_AA66_7788
    assert rtc.epoch_latch == latched
    rtc.set_epoch_ms(MASK64 - 1)
    rtc.advance_ms(3)
    assert rtc.epoch_ms == 1
    assert rtc.epoch_latch == latched


@pytest.mark.parametrize(
    ("action", "error"),
    (
        (lambda: HostedRTCService(True), TypeError),
        (lambda: HostedRTCService(-1), ValueError),
        (lambda: HostedRTCService(MASK64 + 1), ValueError),
        (lambda: HostedRTCService().set_epoch_ms(False), TypeError),
        (lambda: HostedRTCService().advance_ms(-1), ValueError),
        (lambda: HostedRTCService().advance_ms(True), TypeError),
    ),
)
def test_epoch_host_controls_reject_invalid_values(action, error) -> None:
    with pytest.raises(error):
        action()


def test_epoch_aperture_accepts_only_complete_supported_widths() -> None:
    rtc = HostedRTCService()

    rtc.preflight(RTC_EPOCH, 8, write=False)
    rtc.preflight(RTC_EPOCH + 7, 1, write=True)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH - 1, 1, write=False)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH + 7, 2, write=False)
    with pytest.raises(RTCAccessError):
        rtc.preflight(RTC_EPOCH, 3, write=False)
    with pytest.raises(RTCAccessError):
        rtc.read8(RTC_EPOCH_LIMIT)


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


def test_runtimes_own_independent_explicit_epoch_state() -> None:
    first = MegaForthRuntime(
        memory=create_one_core_address_space(initial_epoch_ms=1000)
    )
    second = MegaForthRuntime(
        memory=create_one_core_address_space(initial_epoch_ms=2000)
    )

    first.rtc.advance_ms(25)

    assert first.rtc.epoch_ms == 1025
    assert second.rtc.epoch_ms == 2000
