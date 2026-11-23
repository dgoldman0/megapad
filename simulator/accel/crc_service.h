#pragma once

// The hosted profile's CRC service: the BIOS checked owner record over the
// six-mode recurrence of shared/accel/crc.h. It is the one definition of the
// simulator's CRC words: the Python words call it through its binding and
// the native engine calls it directly. Each core has its own unit, which
// one identity at a time owns, from CRC-MODE! until a final fetch releases
// it.
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <utility>
#include <vector>

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
    // REFLECT_RAW admits the reflected modes and CRC-RAW-FINAL@. Each of
    // CORES cores has its own CRC unit and owner record, as the BIOS keeps a
    // topology-sized owner table, one record a core, over each core's unit.
    explicit Service(bool reflect_raw, int cores = 1)
        : reflect_raw_(reflect_raw),
          units_(static_cast<std::size_t>(cores < 1 ? 1 : cores)) {}

    bool reflect_raw() const noexcept { return reflect_raw_; }
    int core_count() const noexcept { return static_cast<int>(units_.size()); }
    std::uint8_t mode(int core = 0) const { return unit_at(core).mode; }
    Cell accumulator(int core = 0) const { return unit_at(core).accumulator; }
    const std::optional<Identity>& owner(int core = 0) const {
        return unit_at(core).owner;
    }

    // CRC-MODE!: validate and acquire the caller's core's unit, leaving its
    // accumulator as it is.
    Cell select_mode(Identity who, Cell mode) noexcept {
        if (!crc::mode_valid(mode)) return status_range;
        if (crc::reflected(static_cast<std::uint8_t>(mode)) && !reflect_raw_)
            return status_unsupported;
        Unit* unit = unit_of(who);
        if (unit == nullptr || unit->owner) return status_state;
        unit->mode = static_cast<std::uint8_t>(mode);
        unit->owner = who;
        return status_ok;
    }

    Cell reset(Identity who) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return status_state;
        unit->accumulator = crc::width_mask(unit->mode);
        return status_ok;
    }

    Cell seed(Identity who, Cell seed) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return status_state;
        unit->accumulator = seed & crc::width_mask(unit->mode);
        return status_ok;
    }

    Cell feed_cell(Identity who, Cell cell) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return status_state;
        unit->accumulator = crc::feed_cell(unit->mode, unit->accumulator, cell);
        return status_ok;
    }

    // The low byte of BYTE.
    Cell feed_byte(Identity who, Cell byte) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return status_state;
        unit->accumulator = crc::feed_byte(unit->mode, unit->accumulator,
                                           static_cast<std::uint8_t>(byte));
        return status_ok;
    }

    // Memory-order bytes, as a host batch of CRC-FEED-BYTE.
    Cell feed_bytes(Identity who, const std::uint8_t* data,
                    std::size_t size) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return status_state;
        Cell accumulator = unit->accumulator;
        for (std::size_t i = 0; i < size; ++i)
            accumulator = crc::feed_byte(unit->mode, accumulator, data[i]);
        unit->accumulator = accumulator;
        return status_ok;
    }

    // CRC@: CRC-MODE! keeps the accumulator, so the fetch shows all of it,
    // as the BIOS reads the whole register, whatever the mode's width.
    std::pair<Cell, Cell> fetch(Identity who) const noexcept {
        const Unit* unit = owned(who);
        if (unit == nullptr) return {0, status_state};
        return {unit->accumulator, status_ok};
    }

    std::pair<Cell, Cell> raw_final(Identity who) noexcept {
        Unit* unit = owned(who);
        if (!reflect_raw_) {
            // The capability's status comes first. The BIOS still finalizes
            // an exact owner in a normal mode, to release its record.
            if (unit != nullptr && !crc::reflected(unit->mode)) {
                unit->accumulator = finalized(*unit);
                unit->owner.reset();
            }
            return {0, status_unsupported};
        }
        if (unit == nullptr) return {0, status_state};
        unit->accumulator &= crc::width_mask(unit->mode);
        unit->owner.reset();
        return {unit->accumulator, status_ok};
    }

    Cell final(Identity who) noexcept {
        Unit* unit = owned(who);
        if (unit == nullptr) return 0;
        unit->accumulator = finalized(*unit);
        unit->owner.reset();
        return unit->accumulator;
    }

private:
    struct Unit {
        std::uint8_t mode = 0;
        Cell accumulator = 0xFFFFFFFFull;
        std::optional<Identity> owner;
    };

    // The caller's core's unit, or none for a core the topology lacks,
    // which the BIOS answers with STATE without touching the table.
    Unit* unit_of(Identity who) noexcept {
        return who.core < units_.size() ? &units_[static_cast<std::size_t>(who.core)]
                                        : nullptr;
    }
    const Unit* unit_of(Identity who) const noexcept {
        return who.core < units_.size() ? &units_[static_cast<std::size_t>(who.core)]
                                        : nullptr;
    }
    Unit* owned(Identity who) noexcept {
        Unit* unit = unit_of(who);
        return unit != nullptr && unit->owner && *unit->owner == who ? unit : nullptr;
    }
    const Unit* owned(Identity who) const noexcept {
        const Unit* unit = unit_of(who);
        return unit != nullptr && unit->owner && *unit->owner == who ? unit : nullptr;
    }
    const Unit& unit_at(int core) const {
        if (core < 0 || static_cast<std::size_t>(core) >= units_.size())
            throw std::out_of_range("no such core's CRC unit");
        return units_[static_cast<std::size_t>(core)];
    }
    static Cell finalized(const Unit& unit) noexcept {
        const Cell mask = crc::width_mask(unit.mode);
        return (unit.accumulator & mask) ^ mask;
    }

    bool reflect_raw_;
    std::vector<Unit> units_;
};

}  // namespace megapad::crc_service
