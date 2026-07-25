#pragma once
// =========================================================================
//  mp64_timer.h — C++ Timer device for the Megapad-64 accelerator
//
//  Handles all Timer MMIO (0x0100–0x010F) in C++ so that tight
//  busy-wait loops polling Timer.STATUS never cross the pybind11
//  boundary.
//
//  Register map (offsets from TIMER_BASE = 0x0100):
//    0x00–0x03  COUNT    (R)   32-bit free-running counter (LE)
//    0x04–0x07  COMPARE  (RW)  32-bit compare match (LE)
//    0x08       CONTROL  (RW)  bit 0: enable
//                               bit 1: compare-match IRQ enable
//                               bit 2: auto-reload on match
//    0x09       STATUS   (RW)  bit 0: compare-match flag (W1C)
// =========================================================================

#include <cstdint>
#include <optional>

struct TimerDevice {
    uint32_t counter = 0;
    uint32_t compare = 0;
    uint8_t  control = 0;
    uint8_t  status = 0;
    bool     irq_pending = false;
    bool     enabled = false;  // false = bypass, fall through to Python

    // MMIO address range (offsets from MMIO_START)
    static constexpr uint32_t TIMER_BASE = 0x0100;
    static constexpr uint32_t TIMER_END  = 0x0110;

    // -------------------------------------------------------------------
    //  Init / reset
    // -------------------------------------------------------------------

    void init() {
        counter = 0;
        compare = 0xFFFFFFFF;
        control = 0;
        status = 0;
        irq_pending = false;
        enabled = true;
    }

    // -------------------------------------------------------------------
    //  MMIO dispatch
    // -------------------------------------------------------------------

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= TIMER_BASE && mmio_offset < TIMER_END;
    }

    uint8_t read8(uint32_t mmio_offset) const {
        uint32_t off = mmio_offset - TIMER_BASE;

        // COUNT — 4 bytes LE (0x00..0x03)
        if (off <= 0x03)
            return (counter >> (8 * off)) & 0xFF;

        // COMPARE — 4 bytes LE (0x04..0x07)
        if (off >= 0x04 && off <= 0x07)
            return (compare >> (8 * (off - 0x04))) & 0xFF;

        // CONTROL — 1 byte (0x08)
        if (off == 0x08)
            return control;

        // STATUS — 1 byte (0x09)
        if (off == 0x09)
            return status;

        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        uint32_t off = mmio_offset - TIMER_BASE;

        // COMPARE — 4 bytes LE (0x04..0x07)
        if (off >= 0x04 && off <= 0x07) {
            uint32_t shift = 8 * (off - 0x04);
            uint32_t mask = 0xFFU << shift;
            compare = (compare & ~mask) | ((uint32_t)value << shift);
            return;
        }

        // CONTROL — 1 byte (0x08)
        if (off == 0x08) {
            control = value;
            return;
        }

        // STATUS — write-1-to-clear (0x09)
        if (off == 0x09) {
            status &= ~value;
            if (!(status & 1))
                irq_pending = false;
            return;
        }
    }

    // -------------------------------------------------------------------
    //  Tick — O(1) batch tick matching Python Timer.tick() exactly
    // -------------------------------------------------------------------

    uint64_t next_compare_match_delta() const {
        constexpr uint64_t COUNTER_MODULUS = uint64_t{1} << 32;
        const uint64_t delta =
            (static_cast<uint64_t>(compare) + COUNTER_MODULUS -
             static_cast<uint64_t>(counter)) %
            COUNTER_MODULUS;
        return delta == 0 ? COUNTER_MODULUS : delta;
    }

    std::optional<uint64_t> next_irq_assertion_delta() const {
        if (!enabled || !(control & 1) || !(control & 2) || irq_pending)
            return std::nullopt;
        return next_compare_match_delta();
    }

    void tick(uint64_t cycles) {
        if (!(control & 1) || cycles == 0)
            return;

        if (cycles == 1) {
            ++counter;
            if (counter == compare) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
                if (control & 4)
                    counter = 0;
            }
            return;
        }

        constexpr uint64_t COUNTER_MODULUS = uint64_t{1} << 32;
        const uint64_t elapsed = cycles;
        const uint64_t match_delta = next_compare_match_delta();

        if (elapsed < match_delta) {
            counter = static_cast<uint32_t>(
                static_cast<uint64_t>(counter) + elapsed);
            return;
        }

        status |= 1;
        if (control & 2)
            irq_pending = true;

        if (control & 4) {
            // The matching increment resets the counter to zero. Subsequent
            // matches are compare cycles apart, except compare==0 where a
            // complete 32-bit wrap is required to match zero again.
            const uint64_t period =
                compare == 0 ? COUNTER_MODULUS
                             : static_cast<uint64_t>(compare);
            counter = static_cast<uint32_t>(
                (elapsed - match_delta) % period);
        } else {
            counter = static_cast<uint32_t>(
                static_cast<uint64_t>(counter) + elapsed);
        }
    }
};
