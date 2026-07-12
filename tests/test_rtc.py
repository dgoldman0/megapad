"""RTC timing-mode tests."""

from unittest.mock import patch

from devices import RTC


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
