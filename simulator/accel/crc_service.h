#pragma once

// The hosted profile's CRC service: the BIOS checked owner record over the
// six-mode recurrence of shared/accel/crc.h. It is the one definition of the
// simulator's CRC words: the Python words call it through its binding and
// the native engine calls it directly. One identity at a time owns the
// engine, from CRC-MODE! until a final fetch releases it.
#include <cstdint>
#include <optional>
#include <utility>

#include "../../shared/accel/crc.h"

namespace megapad::crc_service {

using Cell = std::uint64_t;

inline constexpr Cell status_ok = 0;
inline constexpr Cell status_unsupported = 1;
inline constexpr Cell status_state = 2;
inline constexpr Cell status_range = 3;

// A checked BIOS caller: (COREID, TASK-ID).
struct Identity {
    Cell core = 0;
    Cell task = 0;
    bool operator==(const Identity& other) const noexcept {
        return core == other.core && task == other.task;
    }
};

class Service {
public:
    // REFLECT_RAW admits the reflected modes and CRC-RAW-FINAL@.
    explicit Service(bool reflect_raw) noexcept : reflect_raw_(reflect_raw) {}

    bool reflect_raw() const noexcept { return reflect_raw_; }
    std::uint8_t mode() const noexcept { return mode_; }
    Cell accumulator() const noexcept { return accumulator_; }
    const std::optional<Identity>& owner() const noexcept { return owner_; }

    // CRC-MODE!: validate and acquire, leaving the accumulator as it is.
    Cell select_mode(Identity who, Cell mode) noexcept {
        if (!crc::mode_valid(mode)) return status_range;
        if (crc::reflected(static_cast<std::uint8_t>(mode)) && !reflect_raw_)
            return status_unsupported;
        if (owner_) return status_state;
        mode_ = static_cast<std::uint8_t>(mode);
        owner_ = who;
        return status_ok;
    }

    Cell reset(Identity who) noexcept {
        if (!owns(who)) return status_state;
        accumulator_ = crc::width_mask(mode_);
        return status_ok;
    }

    Cell seed(Identity who, Cell seed) noexcept {
        if (!owns(who)) return status_state;
        accumulator_ = seed & crc::width_mask(mode_);
        return status_ok;
    }

    Cell feed_cell(Identity who, Cell cell) noexcept {
        if (!owns(who)) return status_state;
        accumulator_ = crc::feed_cell(mode_, accumulator_, cell);
        return status_ok;
    }

    // The low byte of BYTE.
    Cell feed_byte(Identity who, Cell byte) noexcept {
        if (!owns(who)) return status_state;
        accumulator_ = crc::feed_byte(mode_, accumulator_,
                                      static_cast<std::uint8_t>(byte));
        return status_ok;
    }

    // Memory-order bytes, as a host batch of CRC-FEED-BYTE.
    Cell feed_bytes(Identity who, const std::uint8_t* data,
                    std::size_t size) noexcept {
        if (!owns(who)) return status_state;
        Cell accumulator = accumulator_;
        for (std::size_t i = 0; i < size; ++i)
            accumulator = crc::feed_byte(mode_, accumulator, data[i]);
        accumulator_ = accumulator;
        return status_ok;
    }

    // CRC@: CRC-MODE! keeps the accumulator, so the fetch shows all of it,
    // as the BIOS reads the whole register, whatever the mode's width.
    std::pair<Cell, Cell> fetch(Identity who) const noexcept {
        if (!owns(who)) return {0, status_state};
        return {accumulator_, status_ok};
    }

    std::pair<Cell, Cell> raw_final(Identity who) noexcept {
        if (!reflect_raw_) {
            // The capability's status comes first. The BIOS still finalizes
            // an exact owner in a normal mode, to release its record.
            if (owns(who) && !crc::reflected(mode_)) {
                accumulator_ = finalized();
                owner_.reset();
            }
            return {0, status_unsupported};
        }
        if (!owns(who)) return {0, status_state};
        accumulator_ &= crc::width_mask(mode_);
        owner_.reset();
        return {accumulator_, status_ok};
    }

    Cell final(Identity who) noexcept {
        if (!owns(who)) return 0;
        accumulator_ = finalized();
        owner_.reset();
        return accumulator_;
    }

private:
    bool owns(Identity who) const noexcept { return owner_ && *owner_ == who; }
    Cell finalized() const noexcept {
        const Cell mask = crc::width_mask(mode_);
        return (accumulator_ & mask) ^ mask;
    }

    bool reflect_raw_;
    std::uint8_t mode_ = 0;
    Cell accumulator_ = 0xFFFFFFFFull;
    std::optional<Identity> owner_;
};

}  // namespace megapad::crc_service
