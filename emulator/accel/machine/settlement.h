#pragma once

#include <array>
#include <cstdint>
#include <limits>
#include <mutex>
#include <stdexcept>

namespace mp64::machine {

class SystemClock {
public:
    static constexpr int EVENT_TIMER = 0;
    static constexpr int EVENT_FRAMEBUFFER = 1;
    static constexpr int EVENT_RTC = 2;
    static constexpr int EVENT_INTERRUPT = 3;
    static constexpr int EVENT_EXTERNAL = 4;
    static constexpr int EVENT_SOURCE_COUNT = 5;

    struct Snapshot {
        uint64_t cycles;
        bool has_deadline;
        uint64_t earliest_deadline;
        uint64_t source_mask;
        std::array<uint64_t, EVENT_SOURCE_COUNT> deadlines;
        uint64_t active_sources;
    };

    uint64_t cycles() const;

    template <
        typename Timer,
        typename Framebuffer,
        typename RTC,
        typename Crypto>
    void advance_by(
            uint64_t delta,
            Timer& timer,
            Framebuffer& framebuffer,
            RTC& rtc,
            Crypto& crypto) {
        std::lock_guard<std::mutex> guard(mutex_);
        advance_by_unlocked(
            delta, timer, framebuffer, rtc, crypto);
    }

    template <
        typename Timer,
        typename Framebuffer,
        typename RTC,
        typename Crypto>
    void advance_to(
            uint64_t target,
            Timer& timer,
            Framebuffer& framebuffer,
            RTC& rtc,
            Crypto& crypto) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (target < cycles_) {
            throw std::invalid_argument(
                "system cycle target cannot move backwards");
        }
        advance_by_unlocked(
            target - cycles_, timer, framebuffer, rtc, crypto);
    }

    void set_event_deadline(int source_id, uint64_t deadline);
    void clear_event_deadline(int source_id);
    Snapshot snapshot() const;

private:
    static void validate_source(int source_id);
    uint64_t earliest_deadline_unlocked() const;

    template <
        typename Timer,
        typename Framebuffer,
        typename RTC,
        typename Crypto>
    void advance_by_unlocked(
            uint64_t delta,
            Timer& timer,
            Framebuffer& framebuffer,
            RTC& rtc,
            Crypto& crypto) {
        if (delta == 0)
            return;
        if (cycles_ > std::numeric_limits<uint64_t>::max() - delta) {
            throw std::overflow_error("system cycle counter overflow");
        }
        const uint64_t target = cycles_ + delta;
        if (active_sources_ != 0 &&
            target > earliest_deadline_unlocked()) {
            throw std::invalid_argument(
                "system clock advance cannot cross the event horizon");
        }

        timer.tick(delta);
        framebuffer.tick(delta);
        rtc.tick(delta);
        crypto.tick(delta);
        cycles_ = target;
    }

    mutable std::mutex mutex_;
    uint64_t cycles_ = 0;
    std::array<uint64_t, EVENT_SOURCE_COUNT> deadlines_{};
    uint64_t active_sources_ = 0;
};

enum class UnboundedSettlementKind : uint8_t {
    SUCCESSFUL_ROUND,
    DMA_FRONTIER,
    FAILURE_PREFIX,
    BATCH_END,
};

class UnboundedSettlementRequest {
public:
    static UnboundedSettlementRequest successful_round(int64_t cycles);
    static UnboundedSettlementRequest dma_frontier(int64_t cycles);
    static UnboundedSettlementRequest failure_prefix(int64_t cycles);
    static UnboundedSettlementRequest batch_end();

    int64_t cycles() const noexcept { return cycles_; }
    UnboundedSettlementKind kind() const noexcept { return kind_; }
    bool advances_clock() const noexcept;
    bool drains_uart() const noexcept;
    bool delivers_interrupts() const noexcept;
    bool permits_native_no_event() const noexcept;

private:
    UnboundedSettlementRequest(
        int64_t cycles,
        UnboundedSettlementKind kind);

    int64_t cycles_;
    UnboundedSettlementKind kind_;
};

}  // namespace mp64::machine
