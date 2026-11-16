#include "storage_faults.h"

#include <string>

namespace megapad::storage {

namespace {

bool parse_stage(std::string_view name, Stage& stage) noexcept {
    static constexpr Stage stages[] = {
        Stage::accept, Stage::start, Stage::dma,
        Stage::media, Stage::write_complete, Stage::flush,
    };
    for (Stage candidate : stages) {
        if (stage_name(candidate) == name) {
            stage = candidate;
            return true;
        }
    }
    return false;
}

bool parse_action(std::string_view name, Action& action) noexcept {
    static constexpr Action actions[] = {
        Action::fail, Action::stall, Action::detach,
    };
    for (Action candidate : actions) {
        if (action_name(candidate) == name) {
            action = candidate;
            return true;
        }
    }
    return false;
}

// A point of an admitted transfer, numbered in the order a byte-serial
// controller reaches it. A READ sector is one MEDIA point and 512 DMA
// points; a WRITE sector is 512 DMA points and 512 MEDIA points, and
// WRITE_COMPLETE follows the last sector.
constexpr std::uint64_t read_sector_points = 1 + sector_size;
constexpr std::uint64_t write_sector_points = 2 * sector_size;
constexpr std::uint64_t controller_max_sectors = 255;

}  // namespace

std::string_view stage_name(Stage stage) noexcept {
    switch (stage) {
    case Stage::accept: return "accept";
    case Stage::start: return "start";
    case Stage::dma: return "dma";
    case Stage::media: return "media";
    case Stage::write_complete: return "write_complete";
    case Stage::flush: return "flush";
    }
    return "";
}

std::string_view action_name(Action action) noexcept {
    switch (action) {
    case Action::fail: return "fail";
    case Action::stall: return "stall";
    case Action::detach: return "detach";
    }
    return "";
}

Fault make_fault(std::string_view stage, std::int64_t result,
                 std::optional<std::int64_t> command,
                 std::optional<std::int64_t> sector_index,
                 std::optional<std::int64_t> byte_index,
                 std::string_view action) {
    Fault fault;
    if (!parse_stage(stage, fault.stage))
        throw std::invalid_argument(
            "unknown storage fault stage: " + std::string(stage));
    if (!parse_action(action, fault.action))
        throw std::invalid_argument(
            "unknown storage fault action: " + std::string(action));
    if (fault.action == Action::stall && fault.stage != Stage::accept
            && fault.stage != Stage::start)
        throw std::invalid_argument("storage stalls are only safe at accept/start");
    if (result < 0 || result > 0xFF)
        throw std::invalid_argument("storage fault result must fit in one byte");
    if (command && (*command < 0 || *command > 0xFF))
        throw std::invalid_argument("storage fault command must fit in one byte");
    if (sector_index && *sector_index < 0)
        throw std::invalid_argument("storage fault sector index cannot be negative");
    if (byte_index && *byte_index < 0)
        throw std::invalid_argument("storage fault byte index cannot be negative");
    fault.result = static_cast<std::uint8_t>(result);
    if (command)
        fault.command = static_cast<std::uint8_t>(*command);
    if (sector_index)
        fault.sector_index = static_cast<std::uint64_t>(*sector_index);
    if (byte_index)
        fault.byte_index = static_cast<std::uint64_t>(*byte_index);
    return fault;
}

std::uint8_t fault_result(const Fault& fault, bool effect_applied) noexcept {
    std::uint8_t result = fault.action == Action::detach
        ? result_media_removed : fault.result;
    if (effect_applied && result != result_ok)
        result |= result_partial;
    return result;
}

std::optional<Fault> FaultQueue::take(Stage stage, std::uint8_t command,
                                      std::optional<std::uint64_t> sector_index,
                                      std::optional<std::uint64_t> byte_index) {
    for (auto fault = faults_.begin(); fault != faults_.end(); ++fault) {
        if (fault->stage != stage)
            continue;
        if (fault->command && *fault->command != command)
            continue;
        if (fault->sector_index && fault->sector_index != sector_index)
            continue;
        if (fault->byte_index && fault->byte_index != byte_index)
            continue;
        Fault taken = *fault;
        faults_.erase(fault);
        return taken;
    }
    return std::nullopt;
}

std::optional<TransferFault> FaultQueue::take_in_transfer(
        std::uint8_t command, std::uint64_t count) {
    if (command != command_read && command != command_write)
        throw std::invalid_argument("a transfer is a READ or a WRITE");
    if (count == 0 || count > controller_max_sectors)
        throw std::invalid_argument(
            "a controller transfers one to 255 sectors at a time");
    const bool write = command == command_write;
    std::optional<std::size_t> chosen;
    std::uint64_t chosen_point = 0;
    TransferFault found;
    for (std::size_t index = 0; index < faults_.size(); ++index) {
        const Fault& fault = faults_[index];
        if (fault.command && *fault.command != command)
            continue;
        // The earliest point of this transfer where FAULT matches, if any.
        std::uint64_t point;
        std::uint64_t sector = fault.sector_index.value_or(0);
        std::optional<std::uint64_t> byte;
        if (fault.stage == Stage::media || fault.stage == Stage::dma) {
            if (sector >= count)
                continue;
            if (fault.stage == Stage::media && !write) {
                // A READ takes each sector from the medium whole, at a point
                // that has no byte index.
                if (fault.byte_index)
                    continue;
                point = sector * read_sector_points;
            } else {
                byte = fault.byte_index.value_or(0);
                if (*byte >= sector_size)
                    continue;
                if (!write)
                    point = sector * read_sector_points + 1 + *byte;
                else if (fault.stage == Stage::dma)
                    point = sector * write_sector_points + *byte;
                else
                    point = sector * write_sector_points + sector_size + *byte;
            }
        } else if (fault.stage == Stage::write_complete && write) {
            if (fault.sector_index && *fault.sector_index != count - 1)
                continue;
            if (fault.byte_index && *fault.byte_index != sector_size - 1)
                continue;
            sector = count - 1;
            byte = sector_size - 1;
            point = count * write_sector_points;
        } else {
            continue;
        }
        // At one point the first fault in planting order acts.
        if (chosen && point >= chosen_point)
            continue;
        chosen = index;
        chosen_point = point;
        found.stage = fault.stage;
        found.sector_index = sector;
        found.byte_index = byte;
    }
    if (!chosen)
        return std::nullopt;
    found.fault = faults_[*chosen];
    faults_.erase(faults_.begin() + static_cast<std::ptrdiff_t>(*chosen));
    const std::uint64_t sector = found.sector_index;
    if (found.stage == Stage::write_complete) {
        found.moved = count * sector_size;
        found.confirmed = count;
    } else if (found.stage == Stage::media && write) {
        found.moved = sector * sector_size + *found.byte_index;
        found.confirmed = sector;
    } else if (found.stage == Stage::dma && !write) {
        found.moved = sector * sector_size + *found.byte_index;
        found.confirmed = sector;
    } else {
        // A READ's MEDIA point or a WRITE's DMA point: the sectors before it
        // are complete and none of this one has moved.
        found.moved = sector * sector_size;
        found.confirmed = sector;
    }
    return found;
}

void RequestObserver::plan(const Fault& fault, std::uint8_t command,
                           std::uint64_t ordinal,
                           std::optional<std::uint64_t> after_writes) {
    if (command != command_read && command != command_write
            && command != command_flush)
        throw std::invalid_argument(
            "a fault is planned for a READ, WRITE or FLUSH request");
    if (ordinal == 0)
        throw std::invalid_argument("request ordinals count from one");
    planned_.push_back({fault, command, ordinal, after_writes});
}

std::size_t RequestObserver::admit(std::uint8_t command, std::uint64_t lba,
                                   std::uint64_t count, FaultQueue& queue) {
    const std::uint64_t writes_before = writes_;
    std::uint64_t ordinal;
    std::uint64_t ordinal_since_write;
    if (command == command_read) {
        ordinal = ++reads_;
        ordinal_since_write = ++reads_since_write_;
    } else if (command == command_write) {
        ordinal = ++writes_;
        ordinal_since_write = 1;
        reads_since_write_ = 0;
        flushes_since_write_ = 0;
    } else if (command == command_flush) {
        ordinal = ++flushes_;
        ordinal_since_write = ++flushes_since_write_;
        // A flush names no sectors, whatever the registers last held.
        lba = 0;
        count = 0;
    } else {
        throw std::invalid_argument(
            "a controller admits only READ, WRITE and FLUSH requests");
    }
    requests_.push_back({command, lba, count});
    std::size_t moved = 0;
    for (auto planned = planned_.begin(); planned != planned_.end();) {
        const bool due = planned->command == command && (
            planned->after_writes
                ? *planned->after_writes == writes_before
                    && planned->ordinal == ordinal_since_write
                : planned->ordinal == ordinal);
        if (!due) {
            ++planned;
            continue;
        }
        queue.push(planned->fault);
        planned = planned_.erase(planned);
        ++moved;
    }
    return moved;
}

}  // namespace megapad::storage
