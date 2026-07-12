"""RTC timing-mode tests."""

from unittest.mock import patch

from accel_wrapper import Megapad64
from asm import assemble
from devices import CppRTCProxy, MMIO_BASE, RTC, RTC_BASE
from system import MegapadSystem


def _read_u64(device: RTC, offset: int) -> int:
    return sum(device.read8(offset + index) << (8 * index) for index in range(8))


def test_virtual_rtc_advances_from_emulated_cycles():
    rtc = RTC()
    epoch = rtc.epoch_ms

    rtc.tick(3 * RTC.MS_DIVISOR)

    assert _read_u64(rtc, 0x00) == 3
    assert _read_u64(rtc, 0x08) == epoch + 3
    assert rtc.clock_mode == "virtual"


def test_realtime_rtc_tracks_host_time_and_pauses_cleanly():
    clock = {"mono": 20.0, "wall": 1_000.0}
    with patch("devices._time.monotonic", side_effect=lambda: clock["mono"]), \
            patch("devices._time.time", side_effect=lambda: clock["wall"]):
        rtc = RTC(realtime=True)
        clock["mono"] = 20.250
        rtc.tick(1)

        assert _read_u64(rtc, 0x00) == 250
        assert _read_u64(rtc, 0x08) == 1_000_250
        assert rtc.clock_mode == "realtime"

        rtc.write8(0x18, 0)
        clock["mono"] = 25.0
        rtc.tick(10 * RTC.MS_DIVISOR)
        assert _read_u64(rtc, 0x00) == 250

        rtc.write8(0x18, 1)
        clock["mono"] = 25.100
        rtc.tick(1)
        assert _read_u64(rtc, 0x00) == 350


def _rtc_pair():
    reference = RTC()
    cpu = Megapad64(mem_size=64 * 1024)
    native = CppRTCProxy(cpu._cs)
    return reference, native


def _set_state(device):
    device.realtime = False
    device.uptime_ms = 0x0102030405060708
    device.epoch_ms = 0x1112131415161718
    device.sec = 58
    device.min = 59
    device.hour = 23
    device.day = 28
    device.mon = 2
    device.year = 2024
    device.dow = 3
    device.ctrl = 0x03
    device.status = 0
    device.alarm_sec = 59
    device.alarm_min = 59
    device.alarm_hour = 23
    device.irq_pending = False
    device._ms_prescaler = RTC.MS_DIVISOR - 1
    device._sec_prescaler = 999


def _observable_state(device):
    return tuple(getattr(device, name) for name in (
        "uptime_ms", "epoch_ms", "sec", "min", "hour", "day", "mon",
        "year", "dow", "ctrl", "status", "alarm_sec", "alarm_min",
        "alarm_hour", "irq_pending", "_ms_prescaler", "_sec_prescaler",
    ))


def test_cpp_rtc_matches_reference_tick_alarm_and_w1c():
    reference, native = _rtc_pair()
    for device in (reference, native):
        _set_state(device)
        device.tick(1)

    assert _observable_state(native) == _observable_state(reference)
    assert native.sec == 59
    assert native.status == 0x07
    assert native.irq_pending

    for device in (reference, native):
        device.write8(0x19, 0x07)
    assert _observable_state(native) == _observable_state(reference)
    assert not native.irq_pending


def test_cpp_rtc_matches_reference_latches_writes_and_pause():
    reference, native = _rtc_pair()
    for device in (reference, native):
        _set_state(device)
        device.ctrl = 1

    # Byte zero captures a stable eight-byte snapshot.
    assert reference.read8(0) == native.read8(0)
    reference.uptime_ms = native.uptime_ms = 0xFFEEDDCCBBAA9988
    assert [reference.read8(i) for i in range(1, 8)] == [
        native.read8(i) for i in range(1, 8)
    ]

    epoch = 0x8877665544332211
    for index in range(8):
        value = (epoch >> (8 * index)) & 0xFF
        reference.write8(0x08 + index, value)
        native.write8(0x08 + index, value)

    # The reference emulator preserves a partial millisecond while stopped.
    for device in (reference, native):
        device._ms_prescaler = RTC.MS_DIVISOR // 2
        device.write8(0x18, 0)
        device.tick(RTC.MS_DIVISOR)
        device.write8(0x18, 1)
        device.tick(RTC.MS_DIVISOR // 2)

    assert _observable_state(native) == _observable_state(reference)


def test_system_rtc_access_widths_stay_in_native_batch_loop():
    system = MegapadSystem(ram_size=64 * 1024)
    rtc_addr = MMIO_BASE + RTC_BASE
    code = assemble(f"""
        ldi64 r11, {rtc_addr}
    loop:
        ld.b r1, r11
        ld.h r2, r11
        ld.w r3, r11
        ld.d r4, r11
        lbr loop
    """)
    system.load_binary(0, code)
    system.boot()

    callback_reads = 0
    original_read = system.cpu._mmio_read8

    def counted_read(addr):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(addr)

    system.cpu._mmio_read8 = counted_read
    executed = system.run_batch(10_000)

    assert executed == 10_000
    assert callback_reads == 0


def test_secondary_core_falls_back_to_shared_core0_rtc():
    system = MegapadSystem(ram_size=64 * 1024, num_cores=2)
    expected = 0x0102030405060708
    system.rtc.ctrl = 0
    system.rtc.uptime_ms = expected

    assert system.cores[0]._cs.rtc_enabled()
    assert not system.cores[1]._cs.rtc_enabled()
    assert system.cores[1]._mmio_read8(MMIO_BASE + RTC_BASE) == (expected & 0xFF)
