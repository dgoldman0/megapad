#pragma once
// =========================================================================
//  mp64_uart_geom.h — C++ UART Geometry device for the Megapad-64 accel
//
//  Handles UART Geometry MMIO (0x0010–0x001F) in C++ so that firmware
//  polling terminal dimensions never crosses the pybind11 boundary.
//
//  Register map (offsets from MMIO_START):
//    0x10–0x11  COLS      (RW) 16-bit LE terminal column count
//    0x12–0x13  ROWS      (RW) 16-bit LE terminal row count
//    0x14       STATUS    (RW) bit 0: RESIZED (W1C — host sets, FW clears)
//                               bit 1: REQ_DENIED (W1C — host denied resize)
//    0x15       CTRL      (RW) bit 0: RESIZE_IE (enable resize notification)
//                               bit 1: REQ_RESIZE (FW sets to request resize;
//                                       host clears after accept/deny)
//    0x16–0x17  REQ_COLS  (RW) 16-bit LE requested columns (FW writes)
//    0x18–0x19  REQ_ROWS  (RW) 16-bit LE requested rows (FW writes)
//    0x1A–0x1F  reserved
//
//  Flow — host-initiated resize:
//    1. Host updates COLS/ROWS, sets STATUS.RESIZED=1.
//    2. If CTRL.RESIZE_IE, host can also fire an IRQ (TBD).
//    3. Firmware reads COLS/ROWS, clears STATUS by writing 1 to bit 0.
//
//  Flow — firmware-requested resize:
//    1. Firmware writes REQ_COLS/REQ_ROWS, sets CTRL.REQ_RESIZE=1.
//    2. Host reads REQ_COLS/REQ_ROWS, attempts resize.
//    3. On success: host updates COLS/ROWS to new values,
//       clears CTRL.REQ_RESIZE, sets STATUS.RESIZED=1.
//    4. On failure: host clears CTRL.REQ_RESIZE,
//       sets STATUS.REQ_DENIED=1 (COLS/ROWS unchanged).
// =========================================================================

#include <cstdint>

struct UartGeomDevice {
    uint16_t cols;        // current terminal columns
    uint16_t rows;        // current terminal rows
    uint8_t  status;      // bit 0: RESIZED, bit 1: REQ_DENIED
    uint8_t  ctrl;        // bit 0: RESIZE_IE, bit 1: REQ_RESIZE
    uint16_t req_cols;    // firmware-requested columns
    uint16_t req_rows;    // firmware-requested rows
    bool     enabled;     // false = bypass, fall through to Python

    // Status bits
    static constexpr uint8_t ST_RESIZED    = 0x01;
    static constexpr uint8_t ST_REQ_DENIED = 0x02;

    // Control bits
    static constexpr uint8_t CT_RESIZE_IE  = 0x01;
    static constexpr uint8_t CT_REQ_RESIZE = 0x02;

    // MMIO address range (offsets from MMIO_START)
    static constexpr uint32_t GEOM_BASE = 0x0010;
    static constexpr uint32_t GEOM_END  = 0x0020;

    // -------------------------------------------------------------------
    //  Init / reset
    // -------------------------------------------------------------------

    void init(uint16_t initial_cols = 80, uint16_t initial_rows = 30) {
        cols     = initial_cols;
        rows     = initial_rows;
        status   = 0;
        ctrl     = 0;
        req_cols = 0;
        req_rows = 0;
        enabled  = true;
    }

    // -------------------------------------------------------------------
    //  Host-side helpers (called from Python / display thread)
    // -------------------------------------------------------------------

    // Called when the host detects a terminal resize.
    void host_set_size(uint16_t new_cols, uint16_t new_rows) {
        cols = new_cols;
        rows = new_rows;
        status |= ST_RESIZED;
    }

    // Check if firmware has requested a resize.
    bool has_resize_request() const {
        return (ctrl & CT_REQ_RESIZE) != 0;
    }

    // Accept a firmware resize request: update actual dims, signal FW.
    void host_accept_resize(uint16_t accepted_cols, uint16_t accepted_rows) {
        cols = accepted_cols;
        rows = accepted_rows;
        ctrl &= ~CT_REQ_RESIZE;
        status |= ST_RESIZED;
    }

    // Deny a firmware resize request: leave dims unchanged, signal FW.
    void host_deny_resize() {
        ctrl &= ~CT_REQ_RESIZE;
        status |= ST_REQ_DENIED;
    }

    // -------------------------------------------------------------------
    //  MMIO dispatch
    // -------------------------------------------------------------------

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= GEOM_BASE && mmio_offset < GEOM_END;
    }

    uint8_t read8(uint32_t mmio_offset) const {
        uint32_t off = mmio_offset - GEOM_BASE;

        // COLS — 2 bytes LE (0x00..0x01 relative)
        if (off == 0x00) return cols & 0xFF;
        if (off == 0x01) return (cols >> 8) & 0xFF;

        // ROWS — 2 bytes LE (0x02..0x03 relative)
        if (off == 0x02) return rows & 0xFF;
        if (off == 0x03) return (rows >> 8) & 0xFF;

        // STATUS (0x04)
        if (off == 0x04) return status;

        // CTRL (0x05)
        if (off == 0x05) return ctrl;

        // REQ_COLS — 2 bytes LE (0x06..0x07)
        if (off == 0x06) return req_cols & 0xFF;
        if (off == 0x07) return (req_cols >> 8) & 0xFF;

        // REQ_ROWS — 2 bytes LE (0x08..0x09)
        if (off == 0x08) return req_rows & 0xFF;
        if (off == 0x09) return (req_rows >> 8) & 0xFF;

        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        uint32_t off = mmio_offset - GEOM_BASE;

        // COLS — 2 bytes LE (firmware-writable for resize request compat)
        if (off == 0x00) { cols = (cols & 0xFF00) | value; return; }
        if (off == 0x01) { cols = (cols & 0x00FF) | ((uint16_t)value << 8); return; }

        // ROWS — 2 bytes LE
        if (off == 0x02) { rows = (rows & 0xFF00) | value; return; }
        if (off == 0x03) { rows = (rows & 0x00FF) | ((uint16_t)value << 8); return; }

        // STATUS — write-1-to-clear
        if (off == 0x04) { status &= ~value; return; }

        // CTRL
        if (off == 0x05) { ctrl = value; return; }

        // REQ_COLS — 2 bytes LE
        if (off == 0x06) { req_cols = (req_cols & 0xFF00) | value; return; }
        if (off == 0x07) { req_cols = (req_cols & 0x00FF) | ((uint16_t)value << 8); return; }

        // REQ_ROWS — 2 bytes LE
        if (off == 0x08) { req_rows = (req_rows & 0xFF00) | value; return; }
        if (off == 0x09) { req_rows = (req_rows & 0x00FF) | ((uint16_t)value << 8); return; }
    }
};
