#pragma once
// =========================================================================
//  mp64_timer.h — C++ Timer device for the Megapad-64 accelerator
//
//  Handles all Timer MMIO (0x0100–0x010F) in C++ so that tight
//  busy-wait loops polling Timer.STATUS never cross the pybind11
//  boundary.  This is the last high-traffic Python MMIO device.
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

struct TimerDevice {
    uint32_t counter;
    uint32_t compare;
    uint8_t  control;
    uint8_t  status;
    bool     irq_pending;
    bool     enabled;       // false = bypass, fall through to Python

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

    void tick(uint32_t cycles) {
        if (!(control & 1) || cycles == 0)
            return;

        if (cycles == 1) {
            // Fast path for single-tick
            counter = (counter + 1) & 0xFFFFFFFF;
            if (counter == compare) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
                if (control & 4)
                    counter = 0;
            }
            return;
        }

        // O(1) batch tick
        uint32_t old = counter;
        uint32_t cmp = compare;

        if (control & 4) {
            // Auto-reload mode: counter resets to 0 on match
            if (cmp == 0) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
                counter = 0;
                return;
            }
            uint32_t gap = (cmp - old) & 0xFFFFFFFF;
            if (cycles >= gap) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
                uint32_t remaining = cycles - gap;
                counter = (cmp > 0) ? (remaining % cmp) : 0;
            } else {
                counter = (old + cycles) & 0xFFFFFFFF;
            }
        } else {
            // No auto-reload: check if compare is crossed
            uint64_t new_val = (uint64_t)old + cycles;
            if (old < cmp && cmp <= new_val) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
            }
            // Handle 32-bit wrap-around case
            else if (new_val > 0xFFFFFFFF && cmp <= (new_val & 0xFFFFFFFF)) {
                status |= 1;
                if (control & 2)
                    irq_pending = true;
            }
            counter = (uint32_t)(new_val & 0xFFFFFFFF);
        }
    }
};
