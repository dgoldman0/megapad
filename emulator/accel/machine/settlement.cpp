#include "settlement.h"

#include <algorithm>
#include <limits>
#include <stdexcept>

namespace mp64::machine {

uint64_t SystemClock::cycles() const {
    std::lock_guard<std::mutex> guard(mutex_);
    return cycles_;
}

void SystemClock::set_event_deadline(
        int source_id,
        uint64_t deadline) {
    std::lock_guard<std::mutex> guard(mutex_);
    validate_source(source_id);
    if (deadline < cycles_) {
        throw std::invalid_argument(
            "event deadline cannot precede current system time");
    }
    deadlines_[static_cast<std::size_t>(source_id)] = deadline;
    active_sources_ |= uint64_t{1} << source_id;
}

void SystemClock::clear_event_deadline(int source_id) {
    std::lock_guard<std::mutex> guard(mutex_);
    validate_source(source_id);
    active_sources_ &= ~(uint64_t{1} << source_id);
}

SystemClock::Snapshot SystemClock::snapshot() const {
    std::lock_guard<std::mutex> guard(mutex_);
    if (active_sources_ == 0) {
        return {
            cycles_, false, 0, 0, deadlines_, active_sources_,
        };
    }

    const uint64_t earliest = earliest_deadline_unlocked();
    uint64_t mask = 0;
    for (int source_id = 0;
         source_id < EVENT_SOURCE_COUNT;
         source_id++) {
        const uint64_t source_bit = uint64_t{1} << source_id;
        if (!(active_sources_ & source_bit))
            continue;
        const uint64_t deadline =
            deadlines_[static_cast<std::size_t>(source_id)];
        if (deadline == earliest)
            mask |= source_bit;
    }
    return {
        cycles_,
        true,
        earliest,
        mask,
        deadlines_,
        active_sources_,
    };
}

void SystemClock::validate_source(int source_id) {
    if (source_id < 0 || source_id >= EVENT_SOURCE_COUNT) {
        throw std::invalid_argument(
            "system event source must be between 0 and 4");
    }
}

uint64_t SystemClock::earliest_deadline_unlocked() const {
    uint64_t earliest = std::numeric_limits<uint64_t>::max();
    for (int source_id = 0;
         source_id < EVENT_SOURCE_COUNT;
         source_id++) {
        const uint64_t source_bit = uint64_t{1} << source_id;
        if (!(active_sources_ & source_bit))
            continue;
        earliest = std::min(
            earliest,
            deadlines_[static_cast<std::size_t>(source_id)]);
    }
    return earliest;
}

UnboundedSettlementRequest::UnboundedSettlementRequest(
        int64_t cycles,
        UnboundedSettlementKind kind)
    : cycles_(cycles), kind_(kind) {
    if (cycles < 0) {
        throw std::invalid_argument(
            "unbounded settlement cycles cannot be negative");
    }
}

UnboundedSettlementRequest
UnboundedSettlementRequest::successful_round(int64_t cycles) {
    return {cycles, UnboundedSettlementKind::SUCCESSFUL_ROUND};
}

UnboundedSettlementRequest
UnboundedSettlementRequest::dma_frontier(int64_t cycles) {
    return {cycles, UnboundedSettlementKind::DMA_FRONTIER};
}

UnboundedSettlementRequest
UnboundedSettlementRequest::failure_prefix(int64_t cycles) {
    return {cycles, UnboundedSettlementKind::FAILURE_PREFIX};
}

UnboundedSettlementRequest UnboundedSettlementRequest::batch_end() {
    return {0, UnboundedSettlementKind::BATCH_END};
}

bool UnboundedSettlementRequest::advances_clock() const noexcept {
    return kind_ != UnboundedSettlementKind::BATCH_END;
}

bool UnboundedSettlementRequest::drains_uart() const noexcept {
    return kind_ == UnboundedSettlementKind::FAILURE_PREFIX ||
           kind_ == UnboundedSettlementKind::BATCH_END;
}

bool UnboundedSettlementRequest::delivers_interrupts() const noexcept {
    return kind_ == UnboundedSettlementKind::SUCCESSFUL_ROUND ||
           kind_ == UnboundedSettlementKind::DMA_FRONTIER;
}

bool UnboundedSettlementRequest::permits_native_no_event() const noexcept {
    return kind_ == UnboundedSettlementKind::SUCCESSFUL_ROUND;
}

}  // namespace mp64::machine
