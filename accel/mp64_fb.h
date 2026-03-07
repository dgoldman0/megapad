#pragma once
// =========================================================================
//  mp64_fb.h — C++ Framebuffer device for the Megapad-64 accelerator
//
//  Handles all FB MMIO (0x0A00–0x0A50) in C++ so that the tight
//  GFX-SYNC polling loop and per-frame register reads never cross
//  the pybind11 boundary.
//
//  The display thread (Python/pygame) reads fb_base, width, height,
//  stride, mode, enable, vsync_count, vblank, and palette[] via
//  pybind11 properties exposed on CPUState.
// =========================================================================

#include <cstdint>
#include <cstring>
#include <array>

struct FramebufferDevice {
    // --- Configuration registers ---
    uint64_t fb_base;       // 64-bit pixel data start address
    uint32_t width;         // pixels per row
    uint32_t height;        // rows
    uint32_t stride;        // bytes per row
    uint8_t  mode;          // 0=8bpp indexed, 1=RGB565, 2=FP16, 3=RGBA8888
    uint8_t  enable;        // bit 0: scanout, bit 1: vsync IRQ

    // --- Vsync state ---
    uint32_t vsync_count;   // frame counter (wraps at 32 bits)
    bool     vblank;        // set by tick(), cleared by ack

    // --- Palette ---
    std::array<uint32_t, 256> palette;  // 24-bit 0x00RRGGBB entries
    uint8_t  pal_idx;       // current palette write index

    // --- Tick state ---
    uint32_t frame_cycles;       // accumulated cycles since last vsync
    uint32_t cycles_per_frame;   // threshold (~33333 for 30 FPS)

    // --- Byte-assembly buffers for multi-byte LE writes ---
    uint8_t  base_buf[8];
    uint8_t  width_buf[4];
    uint8_t  height_buf[4];
    uint8_t  stride_buf[4];
    uint8_t  vsync_buf[4];
    uint8_t  pal_data_buf[4];

    // Byte-push for fb_base (via port I/O bridge)
    uint8_t  base_push_ctr;
    uint64_t base_push_buf;

    // --- Active flag ---
    bool     enabled;       // false = bypass, fall through to Python

    // MMIO address range (offsets from MMIO_START)
    static constexpr uint32_t FB_BASE_OFF = 0x0A00;
    static constexpr uint32_t FB_END_OFF  = 0x0A50;

    // -------------------------------------------------------------------
    //  Init / reset
    // -------------------------------------------------------------------

    void init() {
        fb_base = 0;
        width = 320;
        height = 240;
        stride = 320;
        mode = 0;
        enable = 0;
        vsync_count = 0;
        vblank = false;
        pal_idx = 0;
        frame_cycles = 0;
        cycles_per_frame = 33333;
        enabled = true;

        std::memset(base_buf, 0, sizeof(base_buf));
        std::memset(width_buf, 0, sizeof(width_buf));
        std::memset(height_buf, 0, sizeof(height_buf));
        std::memset(stride_buf, 0, sizeof(stride_buf));
        std::memset(vsync_buf, 0, sizeof(vsync_buf));
        std::memset(pal_data_buf, 0, sizeof(pal_data_buf));
        base_push_ctr = 0;
        base_push_buf = 0;

        // Default palette: grayscale ramp
        for (int i = 0; i < 256; i++)
            palette[i] = (uint32_t)((i << 16) | (i << 8) | i);
    }

    // -------------------------------------------------------------------
    //  MMIO dispatch
    // -------------------------------------------------------------------

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= FB_BASE_OFF && mmio_offset < FB_END_OFF;
    }

    uint8_t read8(uint32_t mmio_offset) const {
        uint32_t off = mmio_offset - FB_BASE_OFF;

        // FB_BASE — 8 bytes LE (0x00..0x07)
        if (off < 0x08)
            return (fb_base >> (8 * off)) & 0xFF;

        // FB_WIDTH — 4 bytes LE (0x08..0x0B)
        if (off >= 0x08 && off < 0x0C)
            return (width >> (8 * (off - 0x08))) & 0xFF;

        // FB_HEIGHT — 4 bytes LE (0x10..0x13)
        if (off >= 0x10 && off < 0x14)
            return (height >> (8 * (off - 0x10))) & 0xFF;

        // FB_STRIDE — 4 bytes LE (0x18..0x1B)
        if (off >= 0x18 && off < 0x1C)
            return (stride >> (8 * (off - 0x18))) & 0xFF;

        // FB_MODE — 1 byte (0x20)
        if (off == 0x20)
            return mode;

        // FB_ENABLE — 1 byte (0x28)
        if (off == 0x28)
            return enable;

        // FB_VSYNC — 4 bytes LE (0x30..0x33)
        if (off >= 0x30 && off < 0x34)
            return (vsync_count >> (8 * (off - 0x30))) & 0xFF;

        // FB_STATUS — 1 byte (0x48)
        if (off == 0x48) {
            uint8_t status = 0;
            if (enable & 1) status |= 1;   // bit 0: enabled
            if (vblank)     status |= 2;   // bit 1: in vblank
            return status;
        }

        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        uint32_t off = mmio_offset - FB_BASE_OFF;

        // FB_BASE — 8-byte LE accumulator (0x00..0x07)
        if (off < 0x08) {
            base_buf[off] = value;
            if (off == 0x07) {
                uint64_t v = 0;
                for (int i = 0; i < 8; i++)
                    v |= (uint64_t)base_buf[i] << (8 * i);
                fb_base = v;
            }
            return;
        }
        // FB_WIDTH — 4-byte LE (0x08..0x0B)
        if (off >= 0x08 && off < 0x0C) {
            uint32_t idx = off - 0x08;
            width_buf[idx] = value;
            if (idx == 3) {
                uint32_t v = 0;
                for (int i = 0; i < 4; i++)
                    v |= (uint32_t)width_buf[i] << (8 * i);
                width = v;
            }
            return;
        }
        // FB_HEIGHT — 4-byte LE (0x10..0x13)
        if (off >= 0x10 && off < 0x14) {
            uint32_t idx = off - 0x10;
            height_buf[idx] = value;
            if (idx == 3) {
                uint32_t v = 0;
                for (int i = 0; i < 4; i++)
                    v |= (uint32_t)height_buf[i] << (8 * i);
                height = v;
            }
            return;
        }
        // FB_BASE_PUSH — byte-serial base address write (0x0C)
        if (off == 0x0C) {
            int shift = 8 * base_push_ctr;
            uint64_t mask = (uint64_t)0xFF << shift;
            base_push_buf = (base_push_buf & ~mask) | ((uint64_t)value << shift);
            base_push_ctr = (base_push_ctr + 1) & 7;
            if (base_push_ctr == 0) {   // wrapped → commit
                fb_base = base_push_buf;
                base_push_buf = 0;
            }
            return;
        }
        // FB_STRIDE — 4-byte LE (0x18..0x1B)
        if (off >= 0x18 && off < 0x1C) {
            uint32_t idx = off - 0x18;
            stride_buf[idx] = value;
            if (idx == 3) {
                uint32_t v = 0;
                for (int i = 0; i < 4; i++)
                    v |= (uint32_t)stride_buf[i] << (8 * i);
                stride = v;
            }
            return;
        }
        // FB_MODE — 1 byte (0x20)
        if (off == 0x20) {
            mode = value & 0x03;
            return;
        }
        // FB_ENABLE — 1 byte (0x28)
        if (off == 0x28) {
            enable = value & 0x03;
            return;
        }
        // FB_VSYNC — write to ack/clear IRQ (0x30..0x33)
        if (off >= 0x30 && off < 0x34) {
            uint32_t idx = off - 0x30;
            vsync_buf[idx] = value;
            if (idx == 3) {
                uint32_t ack = 0;
                for (int i = 0; i < 4; i++)
                    ack |= (uint32_t)vsync_buf[i] << (8 * i);
                if (ack & 1)
                    vblank = false;
            }
            return;
        }
        // FB_PAL_IDX — 1 byte (0x38)
        if (off == 0x38) {
            pal_idx = value;
            return;
        }
        // FB_PAL_DATA — 4-byte LE 0x00RRGGBB (0x40..0x43)
        if (off >= 0x40 && off < 0x44) {
            uint32_t idx = off - 0x40;
            pal_data_buf[idx] = value;
            if (idx == 3) {
                uint32_t rgb = 0;
                for (int i = 0; i < 4; i++)
                    rgb |= (uint32_t)pal_data_buf[i] << (8 * i);
                palette[pal_idx] = rgb & 0x00FFFFFF;
                pal_idx = (pal_idx + 1) & 0xFF;
            }
            return;
        }
    }

    // -------------------------------------------------------------------
    //  Tick — advance vsync counter based on accumulated cycles
    // -------------------------------------------------------------------

    void tick(uint32_t cycles) {
        if (!(enable & 1))
            return;
        frame_cycles += cycles;
        if (frame_cycles >= cycles_per_frame) {
            frame_cycles -= cycles_per_frame;
            vsync_count = (vsync_count + 1) & 0xFFFFFFFF;
            vblank = true;
        }
    }

    // -------------------------------------------------------------------
    //  Convenience: IRQ pending check
    // -------------------------------------------------------------------

    bool irq_pending() const {
        return (enable & 2) && vblank;
    }

    // -------------------------------------------------------------------
    //  BPP for current mode
    // -------------------------------------------------------------------

    int bpp() const {
        switch (mode) {
            case 0: return 1;
            case 1: return 2;
            case 2: return 2;
            case 3: return 4;
            default: return 1;
        }
    }
};
