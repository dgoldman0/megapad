#pragma once

// Test faults and the request observer of MegaPad's storage controllers,
// written once for both: the emulator's byte-serial controller takes a fault
// at each point it reaches, and the simulator's checked service, which moves
// a whole request at once, asks where in the request the first fault acts.
// docs/storage-faults.md describes the rules; this header has no Python
// dependency, guest memory or media of its own.
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace megapad::storage {

inline constexpr std::uint64_t sector_size = 512;
inline constexpr std::uint8_t command_read = 0x01;
inline constexpr std::uint8_t command_write = 0x02;
inline constexpr std::uint8_t command_flush = 0xFF;
inline constexpr std::uint8_t result_ok = 0x00;
inline constexpr std::uint8_t result_media_failure = 0x09;
inline constexpr std::uint8_t result_media_removed = 0x0B;
inline constexpr std::uint8_t result_partial = 0x80;

// Where in a request a fault acts. A controller reaches ACCEPT and START
// before it checks the request and admits it; then a READ reaches, for each
// sector in turn, MEDIA (the sector is read from the medium) and DMA once for
// each of its bytes (the byte is written to guest memory); a WRITE reaches,
// for each sector, DMA for each byte (read from guest memory) and then MEDIA
// for each byte (written to the medium), and WRITE_COMPLETE after the last
// sector; a FLUSH reaches FLUSH before it makes the medium durable.
enum class Stage : std::uint8_t {
    accept, start, dma, media, write_complete, flush,
};

// FAIL completes the request with the fault's result. STALL holds it, at
// ACCEPT or START only, until a test releases it. DETACH removes the medium
// and completes the request with MEDIA_REMOVED.
enum class Action : std::uint8_t { fail, stall, detach };

std::string_view stage_name(Stage stage) noexcept;
std::string_view action_name(Action action) noexcept;

// One one-shot fault. A field left empty matches every value.
struct Fault {
    Stage stage = Stage::media;
    std::uint8_t result = result_media_failure;
    std::optional<std::uint8_t> command;
    std::optional<std::uint64_t> sector_index;
    std::optional<std::uint64_t> byte_index;
    Action action = Action::fail;

    bool operator==(const Fault& other) const noexcept {
        return stage == other.stage && result == other.result
            && command == other.command && sector_index == other.sector_index
            && byte_index == other.byte_index && action == other.action;
    }
};

// A fault from its named stage and action, checked as the controllers have
// always checked it. Throws std::invalid_argument with the message a test
// sees. Integers arrive as they were given, so that one past a byte's range
// is refused rather than wrapped.
Fault make_fault(std::string_view stage, std::int64_t result,
                 std::optional<std::int64_t> command,
                 std::optional<std::int64_t> sector_index,
                 std::optional<std::int64_t> byte_index,
                 std::string_view action);

// The result a controller completes a request with when FAULT acts: the
// fault's own result, or MEDIA_REMOVED when it detaches the medium, marked
// PARTIAL when the request had already changed memory or the medium and the
// result is not OK. A stall has no result; its controller holds the request.
std::uint8_t fault_result(const Fault& fault, bool effect_applied) noexcept;

// Where a fault acted inside a transfer, and what the transfer had done:
// MOVED bytes written to guest memory (READ) or to the medium (WRITE), and
// CONFIRMED whole sectors completed.
struct TransferFault {
    Fault fault;
    Stage stage = Stage::media;
    std::uint64_t sector_index = 0;
    std::optional<std::uint64_t> byte_index;
    std::uint64_t moved = 0;
    std::uint64_t confirmed = 0;
};

// The faults planted in one controller, in planting order.
class FaultQueue {
public:
    void push(const Fault& fault) { faults_.push_back(fault); }
    bool empty() const noexcept { return faults_.empty(); }
    std::size_t size() const noexcept { return faults_.size(); }
    void clear() noexcept { faults_.clear(); }
    const std::vector<Fault>& faults() const noexcept { return faults_; }

    // The matching rule. At a point of stage STAGE in a request of COMMAND,
    // the fault that acts is the first in planting order whose stage is
    // STAGE and whose command, sector index and byte index, where set, equal
    // the point's. A point that has no sector or byte index matches only a
    // fault that leaves that field empty. The fault is removed.
    std::optional<Fault> take(Stage stage, std::uint8_t command,
                              std::optional<std::uint64_t> sector_index,
                              std::optional<std::uint64_t> byte_index);

    // For a controller that moves an admitted COUNT-sector READ or WRITE at
    // once: the fault that a byte-serial controller would take first after
    // admission, at the earliest point where one matches, with the point and
    // what the transfer had done there. The fault is removed.
    std::optional<TransferFault> take_in_transfer(std::uint8_t command,
                                                  std::uint64_t count);

private:
    std::vector<Fault> faults_;
};

struct Request {
    std::uint8_t command = 0;
    std::uint64_t lba = 0;
    std::uint64_t count = 0;
};

// What a controller admitted, in order, and faults planned for requests to
// come. A controller admits a request once it has accepted and checked it,
// just before the transfer or flush begins; faults planted then act within
// that request.
class RequestObserver {
public:
    // Plant FAULT when the ORDINAL-th request of COMMAND is admitted,
    // counting from one. With AFTER_WRITES, ORDINAL counts only the requests
    // of COMMAND admitted after exactly that many writes.
    void plan(const Fault& fault, std::uint8_t command, std::uint64_t ordinal,
              std::optional<std::uint64_t> after_writes);

    // Record an admitted READ, WRITE or FLUSH and move the faults planned
    // for it into QUEUE, in the order they were planned. Returns how many
    // moved. No other command reaches admission.
    std::size_t admit(std::uint8_t command, std::uint64_t lba,
                      std::uint64_t count, FaultQueue& queue);

    const std::vector<Request>& requests() const noexcept { return requests_; }
    std::uint64_t reads() const noexcept { return reads_; }
    std::uint64_t writes() const noexcept { return writes_; }
    std::uint64_t flushes() const noexcept { return flushes_; }
    // Planned faults whose request has not come.
    std::size_t pending() const noexcept { return planned_.size(); }

private:
    struct Planned {
        Fault fault;
        std::uint8_t command;
        std::uint64_t ordinal;
        std::optional<std::uint64_t> after_writes;
    };
    std::vector<Planned> planned_;
    std::vector<Request> requests_;
    std::uint64_t reads_ = 0;
    std::uint64_t writes_ = 0;
    std::uint64_t flushes_ = 0;
    // Reads and flushes admitted since the latest write.
    std::uint64_t reads_since_write_ = 0;
    std::uint64_t flushes_since_write_ = 0;
};

}  // namespace megapad::storage
