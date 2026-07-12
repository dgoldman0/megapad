#pragma once
// =========================================================================
//  mp64_rtc.h — C++ RTC device for the Megapad-64 accelerator
//
//  The Python RTC in devices.py remains the reference implementation.  This
//  device mirrors its observable emulator semantics so that MS@/EPOCH@ and
//  calendar polling stay inside the C++ batch loop.
// =========================================================================

#include <chrono>
#include <cstdint>
#include <ctime>

struct RTCDevice {
    static constexpr uint32_t RTC_BASE = 0x0B00;
    static constexpr uint32_t RTC_END = 0x0B20;
    static constexpr uint64_t CLOCK_HZ = 100000000ULL;
    static constexpr uint64_t MS_DIVISOR = CLOCK_HZ / 1000ULL;

    // Standalone CPUState instances have no system RTC until rtc_init().
    bool enabled = false;
    bool realtime = false;

    uint64_t uptime_ms;
    uint64_t epoch_ms;
    uint8_t sec;
    uint8_t min;
    uint8_t hour;
    uint8_t day;
    uint8_t mon;
    uint32_t year;
    uint8_t dow;
    uint8_t ctrl;
    uint8_t status;
    uint8_t alarm_sec;
    uint8_t alarm_min;
    uint8_t alarm_hour;
    bool irq_pending;

    uint64_t ms_prescaler;
    uint64_t sec_prescaler;
    uint64_t uptime_latch;
    uint64_t epoch_latch;

    std::chrono::steady_clock::time_point host_mono_anchor;
    uint64_t host_uptime_anchor;
    uint64_t host_epoch_anchor;

    void init(bool use_realtime, uint64_t initial_epoch_ms,
              uint8_t initial_sec, uint8_t initial_min,
              uint8_t initial_hour, uint8_t initial_day,
              uint8_t initial_mon, uint32_t initial_year,
              uint8_t initial_dow) {
        enabled = true;
        realtime = use_realtime;
        uptime_ms = 0;
        epoch_ms = initial_epoch_ms;
        sec = initial_sec;
        min = initial_min;
        hour = initial_hour;
        day = initial_day;
        mon = initial_mon;
        year = initial_year;
        dow = initial_dow;
        ctrl = 0x01;  // Match the Python emulator (the RTL reset differs).
        status = 0;
        alarm_sec = 0;
        alarm_min = 0;
        alarm_hour = 0;
        irq_pending = false;
        ms_prescaler = 0;
        sec_prescaler = 0;
        uptime_latch = 0;
        epoch_latch = 0;
        reanchor_host_clock();
    }

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= RTC_BASE && mmio_offset < RTC_END;
    }

    void reanchor_host_clock() {
        host_mono_anchor = std::chrono::steady_clock::now();
        host_uptime_anchor = uptime_ms;
        host_epoch_anchor = epoch_ms;
    }

    void sync_realtime() {
        if (!realtime || !(ctrl & 1))
            return;

        auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::steady_clock::now() - host_mono_anchor).count();
        uint64_t elapsed_ms = elapsed > 0 ? static_cast<uint64_t>(elapsed) : 0;
        uint64_t old_epoch = epoch_ms;
        uptime_ms = host_uptime_anchor + elapsed_ms;
        epoch_ms = host_epoch_anchor + elapsed_ms;
        if (epoch_ms == old_epoch)
            return;

        status |= 0x04;
        if (epoch_ms / 1000 == old_epoch / 1000)
            return;

        status |= 0x02;
        std::time_t whole_seconds = static_cast<std::time_t>(epoch_ms / 1000);
        std::tm local{};
#if defined(_WIN32)
        bool have_local = localtime_s(&local, &whole_seconds) == 0;
#else
        bool have_local = localtime_r(&whole_seconds, &local) != nullptr;
#endif
        if (have_local) {
            sec = static_cast<uint8_t>(local.tm_sec);
            min = static_cast<uint8_t>(local.tm_min);
            hour = static_cast<uint8_t>(local.tm_hour);
            day = static_cast<uint8_t>(local.tm_mday);
            mon = static_cast<uint8_t>(local.tm_mon + 1);
            year = static_cast<uint32_t>(local.tm_year + 1900);
            dow = static_cast<uint8_t>(local.tm_wday);
        }
        check_alarm();
    }

    uint8_t read8(uint32_t mmio_offset) {
        uint32_t off = mmio_offset - RTC_BASE;
        if (off == 0x00) {
            sync_realtime();
            uptime_latch = uptime_ms;
            return static_cast<uint8_t>(uptime_ms);
        }
        if (off >= 0x01 && off <= 0x07)
            return static_cast<uint8_t>(uptime_latch >> (8 * off));
        if (off == 0x08) {
            sync_realtime();
            epoch_latch = epoch_ms;
            return static_cast<uint8_t>(epoch_ms);
        }
        if (off >= 0x09 && off <= 0x0F)
            return static_cast<uint8_t>(epoch_latch >> (8 * (off - 0x08)));
        if (off == 0x10) {
            sync_realtime();
            return sec & 0x3F;
        }
        if (off == 0x11) return min & 0x3F;
        if (off == 0x12) return hour & 0x1F;
        if (off == 0x13) return day & 0x1F;
        if (off == 0x14) return mon & 0x0F;
        if (off == 0x15) return static_cast<uint8_t>(year);
        if (off == 0x16) return static_cast<uint8_t>(year >> 8);
        if (off == 0x17) return dow & 0x07;
        if (off == 0x18) return ctrl;
        if (off == 0x19) return status;
        if (off == 0x1A) return alarm_sec & 0x3F;
        if (off == 0x1B) return alarm_min & 0x3F;
        if (off == 0x1C) return alarm_hour & 0x1F;
        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        uint32_t off = mmio_offset - RTC_BASE;
        if (off >= 0x08 && off <= 0x0F) {
            sync_realtime();
            uint32_t shift = 8 * (off - 0x08);
            uint64_t mask = 0xFFULL << shift;
            epoch_ms = (epoch_ms & ~mask) | (static_cast<uint64_t>(value) << shift);
            if (realtime)
                reanchor_host_clock();
            return;
        }
        if (off == 0x10) sec = value & 0x3F;
        else if (off == 0x11) min = value & 0x3F;
        else if (off == 0x12) hour = value & 0x1F;
        else if (off == 0x13) day = value & 0x1F;
        else if (off == 0x14) mon = value & 0x0F;
        else if (off == 0x15) year = (year & 0xFF00U) | value;
        else if (off == 0x16) year = (year & 0x00FFU) | (static_cast<uint32_t>(value) << 8);
        else if (off == 0x17) dow = value & 0x07;
        else if (off == 0x18) {
            bool was_running = (ctrl & 1) != 0;
            if (was_running)
                sync_realtime();
            ctrl = value;
            if (realtime && ((ctrl & 1) != 0) != was_running)
                reanchor_host_clock();
        } else if (off == 0x19) {
            status &= static_cast<uint8_t>(~value);
            if (!(status & 1))
                irq_pending = false;
        } else if (off == 0x1A) alarm_sec = value & 0x3F;
        else if (off == 0x1B) alarm_min = value & 0x3F;
        else if (off == 0x1C) alarm_hour = value & 0x1F;
    }

    void tick(uint64_t cycles) {
        if (realtime) {
            sync_realtime();
            return;
        }
        if (!(ctrl & 1) || cycles == 0)
            return;

        uint64_t total_cycles = ms_prescaler + cycles;
        uint64_t elapsed_ms = total_cycles / MS_DIVISOR;
        ms_prescaler = total_cycles % MS_DIVISOR;
        if (elapsed_ms == 0)
            return;

        uptime_ms += elapsed_ms;
        epoch_ms += elapsed_ms;
        status |= 0x04;

        uint64_t total_ms = sec_prescaler + elapsed_ms;
        uint64_t elapsed_seconds = total_ms / 1000;
        sec_prescaler = total_ms % 1000;
        while (elapsed_seconds-- > 0)
            advance_one_second();
    }

private:
    static bool is_leap(uint32_t y) {
        return y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
    }

    static uint8_t days_in_month(uint8_t m, uint32_t y) {
        static constexpr uint8_t days[] = {
            0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
        };
        if (m == 2 && is_leap(y))
            return 29;
        return m >= 1 && m <= 12 ? days[m] : 31;
    }

    void check_alarm() {
        if (sec == alarm_sec && min == alarm_min && hour == alarm_hour) {
            status |= 0x01;
            if (ctrl & 2)
                irq_pending = true;
        }
    }

    void advance_one_second() {
        status |= 0x02;
        if (sec < 59) {
            ++sec;
        } else {
            sec = 0;
            if (min < 59) {
                ++min;
            } else {
                min = 0;
                if (hour < 23) {
                    ++hour;
                } else {
                    hour = 0;
                    dow = static_cast<uint8_t>((dow + 1) % 7);
                    uint8_t dim = days_in_month(mon, year);
                    if (day < dim) {
                        ++day;
                    } else {
                        day = 1;
                        if (mon < 12) {
                            ++mon;
                        } else {
                            mon = 1;
                            ++year;
                        }
                    }
                }
            }
        }
        check_alarm();
    }
};
