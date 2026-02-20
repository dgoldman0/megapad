#pragma once
// =========================================================================
//  mp64_nic.h — C++ NIC device for the Megapad-64 accelerator
//
//  Handles all NIC MMIO (0x0400–0x0480) in C++ so that the ~15K–35K
//  register accesses per TLS handshake never cross the pybind11 boundary.
//
//  DMA reads/writes go directly through the CPUState memory pointers
//  (mem, hbw_mem, ext_mem) — no Python callback needed.
//
//  The TAP/UDP backend stays in Python; frames enter via inject_frame()
//  (called from the backend's RX thread through pybind11) and leave via
//  a Python tx_callback set at init time.
// =========================================================================

#include <cstdint>
#include <cstring>
#include <deque>
#include <mutex>
#include <vector>
#include <functional>
#include <algorithm>

static constexpr int NIC_MTU = 1500;

struct NICDevice {
    // --- State ---
    uint8_t  mac[6];
    uint64_t dma_addr;
    uint16_t frame_len;
    uint8_t  irq_ctrl;
    uint8_t  irq_status;
    bool     error;
    bool     link_up;
    uint16_t tx_count;
    uint16_t rx_count;
    bool     enabled;

    // Data port buffer (byte-at-a-time alternative to DMA)
    std::vector<uint8_t> data_buf;
    size_t data_pos;

    // RX queue — guarded by mutex (TAP thread pushes, CPU thread pops)
    std::deque<std::vector<uint8_t>> rx_queue;
    std::mutex rx_mutex;
    static constexpr size_t RX_QUEUE_MAX = 64;

    // TX queue (kept for compatibility / test inspection)
    std::deque<std::vector<uint8_t>> tx_queue;
    static constexpr size_t TX_QUEUE_MAX = 64;

    // TX callback — set from Python (calls backend.send)
    std::function<bool(const uint8_t*, size_t)> tx_callback;

    // Memory pointers — set by attach() from CPUState
    uint8_t* mem;
    uint64_t mem_size;
    uint8_t* hbw_mem;
    uint64_t hbw_base;
    uint64_t hbw_size;
    uint8_t* ext_mem;
    uint64_t ext_mem_base;
    uint64_t ext_mem_size;

    // MMIO address range (offsets from MMIO_START)
    static constexpr uint32_t NIC_BASE = 0x0400;
    static constexpr uint32_t NIC_END  = 0x0480;

    // -------------------------------------------------------------------
    //  Init / reset
    // -------------------------------------------------------------------

    void init(const uint8_t mac_addr[6]) {
        std::memcpy(mac, mac_addr, 6);
        dma_addr = 0;
        frame_len = 0;
        irq_ctrl = 0;
        irq_status = 0;
        error = false;
        link_up = true;
        tx_count = 0;
        rx_count = 0;
        enabled = true;

        data_buf.clear();
        data_pos = 0;

        {
            std::lock_guard<std::mutex> lock(rx_mutex);
            rx_queue.clear();
        }
        tx_queue.clear();

        mem = nullptr;
        mem_size = 0;
        hbw_mem = nullptr;
        hbw_base = 0;
        hbw_size = 0;
        ext_mem = nullptr;
        ext_mem_base = 0;
        ext_mem_size = 0;
    }

    void reset_state() {
        // CMD 0x04 RESET — clear queues and counters, keep MAC and mem ptrs
        {
            std::lock_guard<std::mutex> lock(rx_mutex);
            rx_queue.clear();
        }
        tx_queue.clear();
        data_buf.clear();
        data_pos = 0;
        frame_len = 0;
        irq_status = 0;
        error = false;
        tx_count = 0;
        rx_count = 0;
    }

    // -------------------------------------------------------------------
    //  Attach memory pointers (called from CPUState wiring)
    // -------------------------------------------------------------------

    void attach_mem_ptrs(uint8_t* m, uint64_t msz,
                         uint8_t* hbw, uint64_t hbase, uint64_t hsz,
                         uint8_t* ext, uint64_t ebase, uint64_t esz) {
        mem = m;   mem_size = msz;
        hbw_mem = hbw; hbw_base = hbase; hbw_size = hsz;
        ext_mem = ext; ext_mem_base = ebase; ext_mem_size = esz;
    }

    // -------------------------------------------------------------------
    //  MMIO dispatch
    // -------------------------------------------------------------------

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= NIC_BASE && mmio_offset < NIC_END;
    }

    uint8_t read8(uint32_t mmio_offset) {
        uint32_t off = mmio_offset - NIC_BASE;
        switch (off) {
            case 0x00:  // CMD (write-only)
                return 0;
            case 0x01: {  // STATUS
                uint8_t s = 0x80;  // present
                {
                    std::lock_guard<std::mutex> lock(rx_mutex);
                    if (!rx_queue.empty()) s |= 0x02;  // RX available
                }
                if (link_up) s |= 0x04;
                if (error)   s |= 0x08;
                return s;
            }
            case 0x02: case 0x03: case 0x04: case 0x05:
            case 0x06: case 0x07: case 0x08: case 0x09:
                // DMA_ADDR (8 bytes, little-endian)
                return (dma_addr >> (8 * (off - 0x02))) & 0xFF;
            case 0x0A:  // FRAME_LEN low
                return frame_len & 0xFF;
            case 0x0B:  // FRAME_LEN high
                return (frame_len >> 8) & 0xFF;
            case 0x0C:  // IRQ_CTRL
                return irq_ctrl;
            case 0x0D:  // IRQ_STATUS
                return irq_status;
            case 0x0E: case 0x0F: case 0x10: case 0x11:
            case 0x12: case 0x13: {  // MAC_ADDR (6 bytes)
                int idx = off - 0x0E;
                return (idx < 6) ? mac[idx] : 0;
            }
            case 0x14:  // TX_COUNT low
                return tx_count & 0xFF;
            case 0x15:  // TX_COUNT high
                return (tx_count >> 8) & 0xFF;
            case 0x16:  // RX_COUNT low
                return rx_count & 0xFF;
            case 0x17:  // RX_COUNT high
                return (rx_count >> 8) & 0xFF;
            default:
                if (off >= 0x20 && off <= 0x7F) {
                    // DATA port read
                    if (data_pos < data_buf.size()) {
                        return data_buf[data_pos++];
                    }
                    return 0;
                }
                return 0;
        }
    }

    void write8(uint32_t mmio_offset, uint8_t val) {
        uint32_t off = mmio_offset - NIC_BASE;
        switch (off) {
            case 0x00:  // CMD
                execute_cmd(val);
                break;
            case 0x02: case 0x03: case 0x04: case 0x05:
            case 0x06: case 0x07: case 0x08: case 0x09: {
                // DMA_ADDR
                int shift = 8 * (off - 0x02);
                uint64_t mask = (uint64_t)0xFF << shift;
                dma_addr = (dma_addr & ~mask) | ((uint64_t)val << shift);
                break;
            }
            case 0x0A:  // FRAME_LEN low
                frame_len = (frame_len & 0xFF00) | val;
                break;
            case 0x0B:  // FRAME_LEN high
                frame_len = (frame_len & 0x00FF) | ((uint16_t)val << 8);
                break;
            case 0x0C:  // IRQ_CTRL
                irq_ctrl = val;
                break;
            case 0x0D:  // IRQ_STATUS (write-1-to-clear)
                irq_status &= ~val;
                break;
            default:
                if (off >= 0x20 && off <= 0x7F) {
                    // DATA port write
                    data_buf.push_back(val);
                }
                break;
        }
    }

    // -------------------------------------------------------------------
    //  Commands
    // -------------------------------------------------------------------

    void execute_cmd(uint8_t cmd) {
        error = false;
        switch (cmd) {
            case 0x01:  // SEND
                do_send();
                break;
            case 0x02:  // RECV
                do_recv();
                break;
            case 0x03:  // STATUS (no-op)
                break;
            case 0x04:  // RESET
                reset_state();
                break;
        }
    }

    // -------------------------------------------------------------------
    //  DMA memory access (direct, no Python callback!)
    // -------------------------------------------------------------------

    uint8_t dma_read_byte(uint64_t addr) const {
        // Check ext_mem first (most likely for TLS buffers in userland)
        if (ext_mem && addr >= ext_mem_base && addr < ext_mem_base + ext_mem_size)
            return ext_mem[addr - ext_mem_base];
        // HBW memory
        if (hbw_mem && addr >= hbw_base && addr < hbw_base + hbw_size)
            return hbw_mem[addr - hbw_base];
        // Main RAM
        if (mem && addr < mem_size)
            return mem[addr];
        return 0;
    }

    void dma_write_byte(uint64_t addr, uint8_t val) {
        if (ext_mem && addr >= ext_mem_base && addr < ext_mem_base + ext_mem_size) {
            ext_mem[addr - ext_mem_base] = val;
            return;
        }
        if (hbw_mem && addr >= hbw_base && addr < hbw_base + hbw_size) {
            hbw_mem[addr - hbw_base] = val;
            return;
        }
        if (mem && addr < mem_size) {
            mem[addr] = val;
        }
    }

    // -------------------------------------------------------------------
    //  SEND — read frame from DMA, call tx_callback
    // -------------------------------------------------------------------

    void do_send() {
        if (frame_len == 0 || frame_len > NIC_MTU) {
            error = true;
            return;
        }

        // Try DMA first
        std::vector<uint8_t> frame;
        if (mem || ext_mem || hbw_mem) {
            frame.resize(frame_len);
            for (uint16_t i = 0; i < frame_len; i++)
                frame[i] = dma_read_byte(dma_addr + i);
        } else if (!data_buf.empty()) {
            // Fallback: data port buffer
            size_t n = std::min((size_t)frame_len, data_buf.size());
            frame.assign(data_buf.begin(), data_buf.begin() + n);
            data_buf.clear();
            data_pos = 0;
        } else {
            error = true;
            return;
        }

        // Record in TX queue
        if (tx_queue.size() >= TX_QUEUE_MAX) tx_queue.pop_front();
        tx_queue.push_back(frame);
        tx_count = (tx_count + 1) & 0xFFFF;

        // Deliver to backend via callback
        if (tx_callback) {
            if (!tx_callback(frame.data(), frame.size()))
                error = true;
        }

        // TX IRQ
        if (irq_ctrl & 2)
            irq_status |= 2;
    }

    // -------------------------------------------------------------------
    //  RECV — pop frame from RX queue, write to DMA + data port
    // -------------------------------------------------------------------

    void do_recv() {
        std::vector<uint8_t> frame;
        {
            std::lock_guard<std::mutex> lock(rx_mutex);
            if (rx_queue.empty()) {
                frame_len = 0;
                return;
            }
            frame = std::move(rx_queue.front());
            rx_queue.pop_front();
        }

        frame_len = (uint16_t)frame.size();

        // Write to DMA address
        for (size_t i = 0; i < frame.size(); i++)
            dma_write_byte(dma_addr + i, frame[i]);

        // Also populate data port buffer
        data_buf.assign(frame.begin(), frame.end());
        data_pos = 0;
    }

    // -------------------------------------------------------------------
    //  inject_frame — thread-safe push into RX queue
    //  Called from TAP/UDP backend RX thread via pybind11
    // -------------------------------------------------------------------

    void inject_frame(const uint8_t* data, size_t len) {
        if (len > NIC_MTU) len = NIC_MTU;
        std::lock_guard<std::mutex> lock(rx_mutex);
        if (rx_queue.size() < RX_QUEUE_MAX) {
            rx_queue.emplace_back(data, data + len);
            rx_count = (rx_count + 1) & 0xFFFF;
            if (irq_ctrl & 1)  // RX IRQ enable
                irq_status |= 1;
        }
    }

    // -------------------------------------------------------------------
    //  Query helpers (for Python-side inspection)
    // -------------------------------------------------------------------

    bool has_rx() {
        std::lock_guard<std::mutex> lock(rx_mutex);
        return !rx_queue.empty();
    }

    size_t rx_queue_size() {
        std::lock_guard<std::mutex> lock(rx_mutex);
        return rx_queue.size();
    }

    std::vector<uint8_t> drain_one_tx() {
        if (tx_queue.empty()) return {};
        auto f = std::move(tx_queue.front());
        tx_queue.pop_front();
        return f;
    }

    size_t tx_queue_size() const {
        return tx_queue.size();
    }
};


// =========================================================================
//  TRNG — True Random Number Generator (CSPRNG-backed)
// =========================================================================
//  Simple device that provides random bytes.  Uses a 64-byte pool
//  refilled from std::random_device (OS entropy source).
//
//  Register map (offsets from TRNG_BASE = 0x0800):
//    0x00        RAND8    (R) — one random byte
//    0x08..0x0F  RAND64   (R) — each read returns an independent random byte
//    0x10        STATUS   (R) — always 1 (entropy ready)
//    0x18..0x1F  SEED     (W) — XOR into pool to add entropy
// =========================================================================

#include <random>

struct TRNGDevice {
    uint8_t  pool[64];
    int      pool_pos;
    bool     enabled;

    std::random_device rd;   // OS entropy source

    static constexpr uint32_t TRNG_BASE = 0x0800;
    static constexpr uint32_t TRNG_END  = 0x0820;

    void init() {
        refill_pool();
        pool_pos = 0;
        enabled = true;
    }

    void refill_pool() {
        // Fill 64 bytes from OS entropy (4 bytes at a time)
        for (int i = 0; i < 64; i += 4) {
            uint32_t r = rd();
            pool[i]   = r & 0xFF;
            pool[i+1] = (r >> 8) & 0xFF;
            pool[i+2] = (r >> 16) & 0xFF;
            pool[i+3] = (r >> 24) & 0xFF;
        }
    }

    uint8_t next_byte() {
        if (pool_pos >= 64) {
            refill_pool();
            pool_pos = 0;
        }
        return pool[pool_pos++];
    }

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= TRNG_BASE && mmio_offset < TRNG_END;
    }

    uint8_t read8(uint32_t mmio_offset) {
        uint32_t off = mmio_offset - TRNG_BASE;
        if (off == 0x00)                       return next_byte();  // RAND8
        if (off >= 0x08 && off < 0x10)         return next_byte();  // RAND64
        if (off == 0x10)                       return 1;            // STATUS
        return 0;
    }

    void write8(uint32_t mmio_offset, uint8_t val) {
        uint32_t off = mmio_offset - TRNG_BASE;
        if (off >= 0x18 && off < 0x20) {
            int idx = off - 0x18;
            if (idx < 64) pool[idx] ^= val;   // SEED — mix into pool
        }
    }
};
