#pragma once
// =========================================================================
//  mp64_uart.h - Native UART for the Megapad-64 accelerator
//
//  Keeps UART status/RX/TX accesses inside the C++ execution loop. Host input
//  is injected in batches and pending output is drained by MegapadSystem after
//  each execution batch, avoiding a Python callback for every guest poll.
// =========================================================================

#include <cstddef>
#include <cstdint>
#include <deque>
#include <mutex>
#include <vector>

struct UARTDevice {
    uint8_t control = 0;
    uint8_t baud_lo = 0;
    uint8_t baud_hi = 0;
    uint8_t tx_ring_addr_bytes[8] = {0};
    uint64_t tx_ring_base = 0;
    uint8_t* mem = nullptr;
    uint64_t mem_size = 0;
    std::deque<uint8_t> rx;
    std::vector<uint8_t> tx_pending;
    bool enabled = false;
    mutable std::mutex mutex;

    static constexpr uint32_t UART_BASE = 0x0000;
    static constexpr uint32_t UART_END = 0x0010;
    static constexpr uint64_t TX_RING_CAPACITY = 4096;

    void init() {
        std::lock_guard<std::mutex> lock(mutex);
        control = 0;
        baud_lo = 0;
        baud_hi = 0;
        for (auto& b : tx_ring_addr_bytes)
            b = 0;
        tx_ring_base = 0;
        rx.clear();
        tx_pending.clear();
        enabled = true;
    }

    void attach_mem(uint8_t* memory, uint64_t size) {
        std::lock_guard<std::mutex> lock(mutex);
        mem = memory;
        mem_size = size;
    }

    bool handles(uint32_t mmio_offset) const {
        return enabled && mmio_offset >= UART_BASE && mmio_offset < UART_END;
    }

    bool has_rx_data() const {
        std::lock_guard<std::mutex> lock(mutex);
        return !rx.empty();
    }

    size_t rx_size() const {
        std::lock_guard<std::mutex> lock(mutex);
        return rx.size();
    }

    void inject(const uint8_t* data, size_t size) {
        std::lock_guard<std::mutex> lock(mutex);
        for (size_t i = 0; i < size; ++i)
            rx.push_back(data[i]);
    }

    uint8_t read8(uint32_t mmio_offset) {
        std::lock_guard<std::mutex> lock(mutex);
        const uint32_t off = mmio_offset - UART_BASE;
        switch (off) {
            case 0x00:
                return 0;
            case 0x01:
                if (rx.empty())
                    return 0;
                {
                    const uint8_t value = rx.front();
                    rx.pop_front();
                    return value;
                }
            case 0x02:
                // TX_READY | RX_AVAIL | TX_EMPTY
                return static_cast<uint8_t>(0x21 | (rx.empty() ? 0x00 : 0x02));
            case 0x03:
                return control;
            case 0x04:
                return baud_lo;
            case 0x05:
                return baud_hi;
            default:
                return 0;
        }
    }

    void write8(uint32_t mmio_offset, uint8_t value) {
        std::lock_guard<std::mutex> lock(mutex);
        const uint32_t off = mmio_offset - UART_BASE;
        switch (off) {
            case 0x00:
                tx_pending.push_back(value);
                return;
            case 0x03:
                control = value;
                return;
            case 0x04:
                baud_lo = value;
                return;
            case 0x05:
                baud_hi = value;
                return;
            case 0x06:
                drain_ring();
                return;
            default:
                break;
        }

        if (off >= 0x08 && off <= 0x0F) {
            tx_ring_addr_bytes[off - 0x08] = value;
            if (off == 0x0F) {
                tx_ring_base = 0;
                for (int i = 0; i < 8; ++i)
                    tx_ring_base |= static_cast<uint64_t>(tx_ring_addr_bytes[i]) << (8 * i);
            }
        }
    }

    std::vector<uint8_t> take_tx() {
        std::lock_guard<std::mutex> lock(mutex);
        std::vector<uint8_t> result;
        result.swap(tx_pending);
        return result;
    }

    uint64_t get_tx_ring_base() const {
        std::lock_guard<std::mutex> lock(mutex);
        return tx_ring_base;
    }

    void set_tx_ring_base(uint64_t value) {
        std::lock_guard<std::mutex> lock(mutex);
        tx_ring_base = value;
    }

private:
    uint64_t read_le64(uint64_t address) const {
        uint64_t value = 0;
        for (int i = 0; i < 8; ++i)
            value |= static_cast<uint64_t>(mem[address + i]) << (8 * i);
        return value;
    }

    void write_zero64(uint64_t address) {
        for (int i = 0; i < 8; ++i)
            mem[address + i] = 0;
    }

    void drain_ring() {
        if (!mem || tx_ring_base == 0 || tx_ring_base >= mem_size)
            return;
        if (mem_size - tx_ring_base < 8)
            return;

        const uint64_t head = read_le64(tx_ring_base);
        if (head == 0 || head > TX_RING_CAPACITY)
            return;
        if (head > mem_size - tx_ring_base - 8)
            return;

        const uint64_t start = tx_ring_base + 8;
        tx_pending.insert(tx_pending.end(), mem + start, mem + start + head);
        write_zero64(tx_ring_base);
    }
};
