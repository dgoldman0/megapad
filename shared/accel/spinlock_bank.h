#pragma once

// MegaPad's hardware spinlocks, one model for the emulator and the
// simulator.
//
// Sixteen locks, each held by at most one core (docs/architecture.md,
// "Portable MMIO crypto guard"). A core's acquire returns 0 when the lock
// was free or is already its own, and 1 while another core holds it.
// Reacquisition keeps no depth, so one release by the owner frees the lock,
// and a release by any other core changes nothing. A requester that names
// no core is refused: its acquire reads busy and its release changes
// nothing.
//
// The MMIO window at 0x600 is 64 bytes, four for each lock: reading +4n
// acquires lock n, writing +4n+1 releases it, and every other byte reads 0
// and ignores writes. An access moves 1, 2, 4 or 8 bytes, naturally aligned,
// inside the window. Lock 8 is the crypto guard that the checked BIOS
// acquires internally.
//
// The model has no Python dependency.

#include <array>
#include <cstdint>

#include "mmio_device.h"

namespace megapad::spinlock {

class SpinlockBank : public mmio::Device {
public:
    static constexpr int LOCKS = 16;
    static constexpr int FREE = -1;
    static constexpr int ACQUIRED = 0;
    static constexpr int BUSY = 1;
    static constexpr mmio::Offset OFFSET = 0x600;
    static constexpr mmio::Offset SIZE = 4 * LOCKS;
    static constexpr mmio::Offset LIMIT = OFFSET + SIZE;

    explicit SpinlockBank(int core_count) noexcept
        : cores_(core_count < 0 ? 0 : core_count) {
        reset();
    }

    int core_count() const noexcept { return cores_; }
    bool is_core(int requester) const noexcept {
        return requester >= 0 && requester < cores_;
    }
    static bool is_lock(int lock) noexcept { return lock >= 0 && lock < LOCKS; }

    // The owner of a lock, or FREE.
    int owner(int lock) const noexcept {
        return is_lock(lock) ? owners_[static_cast<std::size_t>(lock)] : FREE;
    }

    int acquire(int lock, int requester) noexcept {
        if (!is_lock(lock) || !is_core(requester))
            return BUSY;
        int& held = owners_[static_cast<std::size_t>(lock)];
        if (held == FREE) {
            held = requester;
            return ACQUIRED;
        }
        return held == requester ? ACQUIRED : BUSY;
    }

    // Whether the release freed the lock.
    bool release(int lock, int requester) noexcept {
        if (!is_lock(lock) || !is_core(requester))
            return false;
        int& held = owners_[static_cast<std::size_t>(lock)];
        if (held != requester)
            return false;
        held = FREE;
        return true;
    }

    void reset() noexcept { owners_.fill(FREE); }

    mmio::Verdict preflight(mmio::Offset offset, unsigned width, bool write,
                            mmio::Requester requester) override {
        static_cast<void>(write);
        static_cast<void>(requester);
        if (!mmio::scalar_width(width) || offset % width != 0 ||
            !mmio::inside(offset, width, OFFSET, LIMIT))
            return mmio::Verdict::refuse(
                "access is outside the spinlocks or not naturally aligned");
        return mmio::Verdict::admit();
    }

    std::uint8_t read8(mmio::Offset offset,
                       mmio::Requester requester) override {
        require_byte(offset);
        const mmio::Offset local = offset - OFFSET;
        if (local % 4 != 0)
            return 0;
        return static_cast<std::uint8_t>(
            acquire(static_cast<int>(local / 4), requester));
    }

    void write8(mmio::Offset offset, std::uint8_t value,
                mmio::Requester requester) override {
        static_cast<void>(value);
        require_byte(offset);
        const mmio::Offset local = offset - OFFSET;
        if (local % 4 == 1)
            release(static_cast<int>(local / 4), requester);
    }

private:
    static void require_byte(mmio::Offset offset) {
        if (offset < OFFSET || offset >= LIMIT)
            throw mmio::Refused("byte access is outside the spinlocks");
    }

    int cores_;
    std::array<int, LOCKS> owners_{};
};

}  // namespace megapad::spinlock
