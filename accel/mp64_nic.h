#pragma once
// =========================================================================
//  mp64_nic.h — C++ NIC device for the Megapad-64 accelerator
//
//  Handles all NIC MMIO (0x0400–0x0480) in C++ so that the ~15K–35K
//  register accesses per TLS handshake never cross the pybind11 boundary.
//
//  Legacy DMA calls drain the device's byte FSM directly through CPUState
//  memory pointers. Strict cycle execution exposes the same held byte beat
//  to SystemState's physical NIC bus port instead.
//
//  The TAP/UDP backend stays in Python; frames enter via inject_frame()
//  (called from the backend's RX thread through pybind11) and leave via
//  a Python tx_callback set at init time.
// =========================================================================

#include <cstdint>
#include <atomic>
#include <array>
#include <bitset>
#include <cstring>
#include <deque>
#include <mutex>
#include <optional>
#include <vector>
#include <functional>
#include <algorithm>
#include <limits>

static constexpr int NIC_MAX_FRAME = 1514;
static constexpr size_t NIC_DATA_WINDOW_SIZE = 96;

enum class NICDMAOwner : uint8_t {
    NONE = 0,
    RX = 1,
    TX = 2,
};

struct NICDMABeat {
    uint64_t token = 0;
    NICDMAOwner owner = NICDMAOwner::NONE;
    uint64_t address = 0;
    bool write = false;
    uint8_t write_data = 0;
};

struct NICDevice {
    // --- State ---
    uint8_t  mac[6];
    uint64_t dma_addr;
    uint16_t frame_len;
    std::atomic<uint8_t>  irq_ctrl;
    std::atomic<uint8_t>  irq_status;
    std::atomic<bool>     error;      // sticky until CMD RESET
    bool     link_up;
    uint16_t tx_count;
    std::atomic<uint16_t> rx_count;
    bool     enabled;
    uint8_t  dma_push_ctr;   // byte-push counter for DMA_PUSH (0-7)

    // Resumable byte-DMA state. RX and TX may both be active, but they share
    // one physical port and therefore expose exactly one immutable beat.
    bool rx_dma_active;
    bool tx_dma_active;
    uint64_t rx_dma_base;
    uint64_t tx_dma_base;
    uint16_t tx_dma_len;
    size_t rx_dma_index;
    size_t tx_dma_index;
    std::vector<uint8_t> rx_dma_frame;
    std::vector<uint8_t> tx_dma_frame;
    std::optional<NICDMABeat> pending_dma_beat;
    uint64_t next_dma_token;

    // Address-indexed diagnostic window.  Frame transport remains DMA-only.
    std::array<uint8_t, NIC_DATA_WINDOW_SIZE> data_window;
    std::bitset<NIC_DATA_WINDOW_SIZE> data_window_valid;

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
        dma_push_ctr = 0;
        rx_dma_active = false;
        tx_dma_active = false;
        rx_dma_base = 0;
        tx_dma_base = 0;
        tx_dma_len = 0;
        rx_dma_index = 0;
        tx_dma_index = 0;
        rx_dma_frame.clear();
        tx_dma_frame.clear();
        pending_dma_beat.reset();
        // Reinitializing host configuration must not reuse an endpoint token
        // already observed by the persistent SystemState coordinator.
        if (next_dma_token == 0)
            next_dma_token = 1;

        data_window.fill(0);
        data_window_valid.reset();

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
        std::lock_guard<std::mutex> lock(rx_mutex);
        rx_queue.clear();
        tx_queue.clear();
        data_window.fill(0);
        data_window_valid.reset();
        frame_len = 0;
        irq_status.store(0, std::memory_order_relaxed);
        error.store(false, std::memory_order_relaxed);
        tx_count = 0;
        rx_count.store(0, std::memory_order_relaxed);
        dma_push_ctr = 0;
        rx_dma_active = false;
        tx_dma_active = false;
        rx_dma_base = 0;
        tx_dma_base = 0;
        tx_dma_len = 0;
        rx_dma_index = 0;
        tx_dma_index = 0;
        rx_dma_frame.clear();
        tx_dma_frame.clear();
        pending_dma_beat.reset();
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
                if (tx_dma_active) s |= 0x01;
                {
                    std::lock_guard<std::mutex> lock(rx_mutex);
                    if (rx_dma_active || !rx_queue.empty())
                        s |= 0x02;  // RX available or being delivered
                }
                if (link_up) s |= 0x04;
                if (error.load(std::memory_order_relaxed)) s |= 0x08;
                if (rx_dma_active) s |= 0x10;
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
                return irq_ctrl.load(std::memory_order_relaxed);
            case 0x0D:  // IRQ_STATUS
                return irq_status.load(std::memory_order_relaxed);
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
                return rx_count.load(std::memory_order_relaxed) & 0xFF;
            case 0x17:  // RX_COUNT high
                return (rx_count.load(std::memory_order_relaxed) >> 8) & 0xFF;
            default:
                if (off >= 0x20 && off <= 0x7F) {
                    // Address-indexed DATA window; reads have no cursor side effect.
                    size_t idx = off - 0x20;
                    return data_window_valid.test(idx) ? data_window[idx] : 0;
                }
                return 0;
        }
    }

    void write8(
            uint32_t mmio_offset,
            uint8_t val,
            bool strict_cycle_dma = false) {
        uint32_t off = mmio_offset - NIC_BASE;
        switch (off) {
            case 0x00:  // CMD
                execute_cmd(val, strict_cycle_dma);
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
                irq_ctrl.store(val, std::memory_order_relaxed);
                break;
            case 0x0D:  // IRQ_STATUS (write-1-to-clear)
                irq_status.fetch_and(static_cast<uint8_t>(~val),
                                     std::memory_order_relaxed);
                break;
            case 0x18: {  // DMA_PUSH — byte-serial address write
                int shift = 8 * dma_push_ctr;
                uint64_t mask = (uint64_t)0xFF << shift;
                dma_addr = (dma_addr & ~mask) | ((uint64_t)val << shift);
                dma_push_ctr = (dma_push_ctr + 1) & 7;
                break;
            }
            default:
                if (off >= 0x20 && off <= 0x7F) {
                    // Address-indexed DATA window.
                    size_t idx = off - 0x20;
                    data_window[idx] = val;
                    data_window_valid.set(idx);
                }
                break;
        }
    }

    // -------------------------------------------------------------------
    //  Commands
    // -------------------------------------------------------------------

    void execute_cmd(
            uint8_t cmd,
            bool strict_cycle_dma = false) {
        dma_push_ctr = 0;  // reset byte-push on any command
        switch (cmd) {
            case 0x01:  // SEND
                begin_send();
                if (!strict_cycle_dma)
                    drain_dma_immediate();
                break;
            case 0x02:  // RECV
                begin_recv();
                if (!strict_cycle_dma)
                    drain_dma_immediate();
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

    static bool mapped_region_contains(
            uint64_t base, uint64_t size, uint64_t addr) {
        return addr >= base && (addr - base) < size;
    }

    uint8_t dma_read_byte(uint64_t addr) const {
        // Check ext_mem first (most likely for TLS buffers in userland)
        if (ext_mem && mapped_region_contains(
                ext_mem_base, ext_mem_size, addr))
            return ext_mem[addr - ext_mem_base];
        // HBW memory
        if (hbw_mem && mapped_region_contains(hbw_base, hbw_size, addr))
            return hbw_mem[addr - hbw_base];
        // Main RAM
        if (mem && addr < mem_size)
            return mem[addr];
        return 0;
    }

    void dma_write_byte(uint64_t addr, uint8_t val) {
        if (ext_mem && mapped_region_contains(
                ext_mem_base, ext_mem_size, addr)) {
            ext_mem[addr - ext_mem_base] = val;
            return;
        }
        if (hbw_mem && mapped_region_contains(hbw_base, hbw_size, addr)) {
            hbw_mem[addr - hbw_base] = val;
            return;
        }
        if (mem && addr < mem_size) {
            mem[addr] = val;
        }
    }

    void latch_next_dma_beat() {
        if (pending_dma_beat.has_value())
            return;

        NICDMABeat beat;
        if (rx_dma_active) {
            beat.owner = NICDMAOwner::RX;
            beat.address = rx_dma_base + rx_dma_index;
            beat.write = true;
            beat.write_data =
                rx_dma_frame[rx_dma_index];
        } else if (tx_dma_active) {
            beat.owner = NICDMAOwner::TX;
            beat.address = tx_dma_base + tx_dma_index;
            beat.write = false;
        } else {
            return;
        }
        if (next_dma_token ==
            std::numeric_limits<uint64_t>::max()) {
            error.store(true, std::memory_order_relaxed);
            rx_dma_active = false;
            tx_dma_active = false;
            rx_dma_frame.clear();
            tx_dma_frame.clear();
            return;
        }
        beat.token = next_dma_token++;
        pending_dma_beat = beat;
    }

    void begin_send() {
        if (tx_dma_active) {
            error.store(true, std::memory_order_relaxed);
            return;
        }
        if (frame_len == 0 || frame_len > NIC_MAX_FRAME) {
            error.store(true, std::memory_order_relaxed);
            return;
        }
        if (!(mem || ext_mem || hbw_mem)) {
            error.store(true, std::memory_order_relaxed);
            return;
        }

        tx_dma_active = true;
        tx_dma_base = dma_addr;
        tx_dma_len = frame_len;
        tx_dma_index = 0;
        tx_dma_frame.assign(tx_dma_len, 0);
        latch_next_dma_beat();
    }

    void begin_recv() {
        if (rx_dma_active)
            return;
        {
            std::lock_guard<std::mutex> lock(rx_mutex);
            if (rx_queue.empty()) {
                frame_len = 0;
                return;
            }
            rx_dma_frame =
                std::move(rx_queue.front());
            rx_queue.pop_front();
        }

        rx_dma_active = true;
        rx_dma_base = dma_addr;
        rx_dma_index = 0;
        latch_next_dma_beat();
    }

    void finalize_send() {
        std::vector<uint8_t> frame =
            std::move(tx_dma_frame);
        tx_dma_frame.clear();
        tx_dma_active = false;
        tx_dma_base = 0;
        tx_dma_len = 0;
        tx_dma_index = 0;

        if (tx_queue.size() >= TX_QUEUE_MAX)
            tx_queue.pop_front();
        tx_queue.push_back(frame);
        tx_count = (tx_count + 1) & 0xFFFF;

        if (tx_callback) {
            if (!tx_callback(frame.data(), frame.size()))
                error.store(true, std::memory_order_relaxed);
        }
        irq_status.fetch_or(2, std::memory_order_relaxed);
    }

    void finalize_recv() {
        frame_len = static_cast<uint16_t>(
            rx_dma_frame.size());
        rx_dma_frame.clear();
        rx_dma_active = false;
        rx_dma_base = 0;
        rx_dma_index = 0;
    }

    bool complete_cycle_dma(
            uint64_t token,
            std::optional<uint8_t> read_value) {
        if (!pending_dma_beat.has_value() ||
            pending_dma_beat->token != token) {
            return false;
        }
        const NICDMABeat beat =
            *pending_dma_beat;
        pending_dma_beat.reset();

        if (beat.owner == NICDMAOwner::RX) {
            if (!rx_dma_active ||
                !beat.write ||
                rx_dma_index >= rx_dma_frame.size() ||
                beat.address !=
                    rx_dma_base + rx_dma_index ||
                beat.write_data !=
                    rx_dma_frame[rx_dma_index]) {
                return false;
            }
            rx_dma_index++;
            if (rx_dma_index == rx_dma_frame.size())
                finalize_recv();
        } else if (beat.owner == NICDMAOwner::TX) {
            if (!tx_dma_active ||
                beat.write ||
                tx_dma_index >= tx_dma_frame.size() ||
                beat.address !=
                    tx_dma_base + tx_dma_index) {
                return false;
            }
            tx_dma_frame[tx_dma_index] =
                read_value.value_or(0);
            tx_dma_index++;
            if (tx_dma_index == tx_dma_frame.size())
                finalize_send();
        } else {
            return false;
        }

        latch_next_dma_beat();
        return true;
    }

    void drain_dma_immediate() {
        while (pending_dma_beat.has_value()) {
            const NICDMABeat beat =
                *pending_dma_beat;
            bool completed = false;
            if (beat.write) {
                dma_write_byte(
                    beat.address,
                    beat.write_data);
                completed = complete_cycle_dma(
                    beat.token,
                    std::nullopt);
            } else {
                completed = complete_cycle_dma(
                    beat.token,
                    dma_read_byte(beat.address));
            }
            if (!completed) {
                error.store(true, std::memory_order_relaxed);
                pending_dma_beat.reset();
                rx_dma_active = false;
                tx_dma_active = false;
                break;
            }
        }
    }

    bool has_cycle_dma_work() const {
        return rx_dma_active ||
               tx_dma_active ||
               pending_dma_beat.has_value();
    }

    std::optional<NICDMABeat>
    cycle_dma_beat() const {
        return pending_dma_beat;
    }

    // -------------------------------------------------------------------
    //  inject_frame — thread-safe push into RX queue
    //  Called from TAP/UDP backend RX thread via pybind11
    // -------------------------------------------------------------------

    bool inject_frame(const uint8_t* data, size_t len) {
        // The backend thread and CPU MMIO thread serialize queue/reset
        // semantics here; associated status metadata is atomic for reads.
        std::lock_guard<std::mutex> lock(rx_mutex);
        if (len == 0 || len > NIC_MAX_FRAME) {
            error.store(true, std::memory_order_relaxed);
            return false;
        }
        if (rx_queue.size() < RX_QUEUE_MAX) {
            rx_queue.emplace_back(data, data + len);
            rx_count.fetch_add(1, std::memory_order_relaxed);
            irq_status.fetch_or(1, std::memory_order_relaxed);
            return true;
        }
        error.store(true, std::memory_order_relaxed);
        return false;
    }

    bool irq_pending() const {
        uint8_t ctrl = irq_ctrl.load(std::memory_order_relaxed);
        uint8_t pending = irq_status.load(std::memory_order_relaxed);
        return (ctrl & pending & 0x03) != 0;
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
