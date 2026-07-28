/*
 * mp64_accel.cpp — C++ accelerated core for Megapad-64 emulator
 *
 * Replaces the Python step() loop with a tight C++ implementation.
 * MMIO accesses call back to Python; tile-engine FP operations are now
 * handled natively in C++ (FP16/BF16 TALU, TMUL, TRED).
 *
 * Build: see setup_accel.py (pybind11 extension module)
 */

#include <algorithm>
#include <array>
#include <cassert>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <cmath>
#include <cstring>
#include <iterator>
#include <limits>
#include <memory>
#include <mutex>
#include <optional>
#include <shared_mutex>
#include <string>
#include <stdexcept>
#include <thread>
#include <vector>
#include <unistd.h>
#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/functional.h>
#include <pybind11/numpy.h>

#include "mp64_crypto.h"
#include "mp64_fb.h"
#include "mp64_nic.h"
#include "mp64_rtc.h"
#include "mp64_timer.h"
#include "mp64_uart_geom.h"
#include "mp64_uart.h"

namespace py = pybind11;

// ---------------------------------------------------------------------------
//  Constants — must match megapad64.py exactly
// ---------------------------------------------------------------------------

static constexpr uint64_t MASK64 = 0xFFFFFFFFFFFFFFFFULL;
static constexpr uint64_t SIGN64 = 1ULL << 63;

// Condition codes
enum CC {
    CC_AL=0, CC_EQ, CC_NE, CC_CS, CC_CC, CC_MI, CC_PL, CC_VS,
    CC_VC, CC_GT, CC_LE, CC_BQ, CC_BNQ, CC_SAT, CC_EF, CC_NV
};

// CSR addresses
enum CSR {
    CSR_FLAGS=0x00, CSR_PSEL=0x01, CSR_XSEL=0x02, CSR_SPSEL=0x03,
    CSR_IVT_BASE=0x04, CSR_D=0x05, CSR_DF=0x06, CSR_Q=0x07,
    CSR_T=0x08, CSR_IE=0x09, CSR_PRIV=0x0A,
    CSR_MPU_BASE=0x0B, CSR_MPU_LIMIT=0x0C,
    CSR_SB=0x10, CSR_SR=0x11, CSR_SC=0x12, CSR_SW=0x13,
    CSR_TMODE=0x14, CSR_TCTRL=0x15,
    CSR_TSRC0=0x16, CSR_TSRC1=0x17, CSR_TDST=0x18,
    CSR_ACC0=0x19, CSR_ACC1=0x1A, CSR_ACC2=0x1B, CSR_ACC3=0x1C,
    CSR_TACC_STATUS=0x1D, CSR_TACC_CTL=0x1E,
    CSR_COREID=0x20, CSR_NCORES=0x21, CSR_MBOX=0x22, CSR_IPIACK=0x23,
    CSR_IVEC_ID=0x24, CSR_TRAP_ADDR=0x25,
    CSR_MEGAPAD_SZ=0x30, CSR_CPUID=0x31,
    CSR_TSTRIDE_R=0x40, CSR_TSTRIDE_C=0x41,
    CSR_TTILE_H=0x42, CSR_TTILE_W=0x43,
    CSR_BIST_CMD=0x60, CSR_BIST_STATUS=0x61,
    CSR_BIST_FAIL_ADDR=0x62, CSR_BIST_FAIL_DATA=0x63,
    CSR_TILE_SELFTEST=0x64, CSR_TILE_ST_DETAIL=0x65,
    CSR_PERF_CYCLES=0x68, CSR_PERF_STALLS=0x69,
    CSR_PERF_TILEOPS=0x6A, CSR_PERF_EXTMEM=0x6B, CSR_PERF_CTRL=0x6C,
    CSR_ICACHE_CTRL=0x70, CSR_ICACHE_HITS=0x71, CSR_ICACHE_MISSES=0x72,
    // EXT.CRYPTO CSRs (Appendix B)
    CSR_CRC_ACC=0x80, CSR_CRC_MODE=0x81,
    CSR_SHA_MODE=0x82, CSR_SHA_MSGLEN=0x83, CSR_SHA_MSGLEN_HI=0x84,
    CSR_GF_PRIME_SEL=0x85,
};

// IVEC IDs
enum IVEC {
    IVEC_RESET=0, IVEC_NMI, IVEC_ILLEGAL_OP, IVEC_ALIGN_FAULT,
    IVEC_DIV_ZERO, IVEC_BUS_FAULT, IVEC_SW_TRAP, IVEC_TIMER, IVEC_IPI,
    IVEC_PRIV_FAULT = 15
};

// Tile EW codes
enum EW { EW_U8=0, EW_U16, EW_U32, EW_U64, EW_FP16, EW_BF16 };

static constexpr std::size_t TACC_IMAGE_BYTES = 256;
static constexpr uint8_t TACC_OWNER_NONE = 31;

// ---------------------------------------------------------------------------
//  Memory mappings
// ---------------------------------------------------------------------------

struct MemoryMappings {
    // Bank 0
    uint8_t* mem = nullptr;
    uint64_t mem_size = 0;
    uint64_t mem_capacity = 0;
    std::unique_ptr<py::buffer_info> mem_lease;
    py::object mem_exporter;

    // HBW math RAM (banks 1-3, contiguous)
    uint8_t* hbw_mem = nullptr;
    uint64_t hbw_base = 0;
    uint64_t hbw_size = 0;
    uint64_t hbw_capacity = 0;
    std::unique_ptr<py::buffer_info> hbw_lease;
    py::object hbw_exporter;

    // External memory (HyperRAM / SDRAM)
    uint8_t* ext_mem = nullptr;
    uint64_t ext_mem_base = 0;
    uint64_t ext_mem_size = 0;
    uint64_t ext_mem_capacity = 0;
    std::unique_ptr<py::buffer_info> ext_mem_lease;
    py::object ext_mem_exporter;

    // Dedicated VRAM (framebuffer pixel memory)
    uint8_t* vram_mem = nullptr;
    uint64_t vram_base = 0;
    uint64_t vram_size = 0;
    uint64_t vram_capacity = 0;
    std::unique_ptr<py::buffer_info> vram_lease;
    py::object vram_exporter;

    // Execution reads mappings under one shared admission. Attachment and
    // metadata replacement plus framebuffer rendering are exclusive users;
    // serializing rendering also avoids races with guest framebuffer writes.
    // The execution flag enforces one logical execution owner while that
    // owner may dispatch a physical worker cohort.
    std::shared_mutex mutex;
    std::atomic<bool> execution_active{false};
    std::atomic<bool> exclusive_active{false};
};

// ---------------------------------------------------------------------------
//  Interrupt routing metadata
// ---------------------------------------------------------------------------

class InterruptRouter {
public:
    void configure(int core_count) {
        std::lock_guard<std::mutex> guard(mutex_);
        core_count_ = core_count;
        pending_.assign(
            static_cast<std::size_t>(core_count) *
                static_cast<std::size_t>(core_count),
            0);
        ipi_lines_.assign(static_cast<std::size_t>(core_count), 0);
    }

    bool send_ipi(int requester_id, int target_id) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!valid_core_unlocked(requester_id) ||
            !valid_core_unlocked(target_id) ||
            requester_id == target_id)
            return false;
        pending_[pending_index_unlocked(target_id, requester_id)] = 1;
        ipi_lines_[static_cast<std::size_t>(target_id)] = 1;
        return true;
    }

    bool acknowledge_ipi(int target_id, int source_id) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!valid_core_unlocked(target_id) ||
            !valid_core_unlocked(source_id))
            return false;
        const std::size_t index =
            pending_index_unlocked(target_id, source_id);
        const bool was_pending = pending_[index] != 0;
        pending_[index] = 0;
        if (!has_pending_unlocked(target_id))
            ipi_lines_[static_cast<std::size_t>(target_id)] = 0;
        return was_pending;
    }

    uint64_t pending_mask(int target_id) const {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!valid_core_unlocked(target_id))
            return 0;
        uint64_t mask = 0;
        const int visible_sources = std::min(core_count_, 64);
        for (int source_id = 0; source_id < visible_sources; source_id++) {
            if (pending_[
                    pending_index_unlocked(target_id, source_id)] != 0)
                mask |= uint64_t{1} << source_id;
        }
        return mask;
    }

    bool ipi_line(int core_id) const {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!valid_core_unlocked(core_id))
            return false;
        return ipi_lines_[static_cast<std::size_t>(core_id)] != 0;
    }

    void set_ipi_line(int core_id, bool asserted) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!valid_core_unlocked(core_id))
            return;
        ipi_lines_[static_cast<std::size_t>(core_id)] =
            asserted ? 1 : 0;
    }

    std::vector<uint64_t> pending_snapshot() const {
        std::lock_guard<std::mutex> guard(mutex_);
        std::vector<uint64_t> masks(
            static_cast<std::size_t>(core_count_), 0);
        const int visible_sources = std::min(core_count_, 64);
        for (int target_id = 0; target_id < core_count_; target_id++) {
            for (int source_id = 0;
                 source_id < visible_sources;
                 source_id++) {
                if (pending_[
                        pending_index_unlocked(
                            target_id, source_id)] != 0)
                    masks[static_cast<std::size_t>(target_id)] |=
                        uint64_t{1} << source_id;
            }
        }
        return masks;
    }

private:
    bool valid_core_unlocked(int core_id) const {
        return core_id >= 0 && core_id < core_count_;
    }

    std::size_t pending_index_unlocked(
            int target_id, int source_id) const {
        return static_cast<std::size_t>(target_id) *
                   static_cast<std::size_t>(core_count_) +
               static_cast<std::size_t>(source_id);
    }

    bool has_pending_unlocked(int target_id) const {
        for (int source_id = 0; source_id < core_count_; source_id++) {
            if (pending_[
                    pending_index_unlocked(target_id, source_id)] != 0)
                return true;
        }
        return false;
    }

    mutable std::mutex mutex_;
    int core_count_ = 0;
    std::vector<uint8_t> pending_;
    std::vector<uint8_t> ipi_lines_;
};

// ---------------------------------------------------------------------------
//  System virtual time and deterministic event horizons
// ---------------------------------------------------------------------------

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

    uint64_t cycles() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return cycles_;
    }

    void advance_by(
            uint64_t delta,
            TimerDevice& timer,
            FramebufferDevice& framebuffer,
            RTCDevice& rtc) {
        std::lock_guard<std::mutex> guard(mutex_);
        advance_by_unlocked(delta, timer, framebuffer, rtc);
    }

    void advance_to(
            uint64_t target,
            TimerDevice& timer,
            FramebufferDevice& framebuffer,
            RTCDevice& rtc) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (target < cycles_)
            throw std::invalid_argument(
                "system cycle target cannot move backwards");
        advance_by_unlocked(
            target - cycles_, timer, framebuffer, rtc);
    }

    void set_event_deadline(int source_id, uint64_t deadline) {
        std::lock_guard<std::mutex> guard(mutex_);
        validate_source(source_id);
        if (deadline < cycles_)
            throw std::invalid_argument(
                "event deadline cannot precede current system time");
        deadlines_[static_cast<std::size_t>(source_id)] = deadline;
        active_sources_ |= uint64_t{1} << source_id;
    }

    void clear_event_deadline(int source_id) {
        std::lock_guard<std::mutex> guard(mutex_);
        validate_source(source_id);
        active_sources_ &= ~(uint64_t{1} << source_id);
    }

    Snapshot snapshot() const {
        std::lock_guard<std::mutex> guard(mutex_);
        if (active_sources_ == 0)
            return {
                cycles_, false, 0, 0, deadlines_, active_sources_
            };

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

private:
    static void validate_source(int source_id) {
        if (source_id < 0 || source_id >= EVENT_SOURCE_COUNT)
            throw std::invalid_argument(
                "system event source must be between 0 and 4");
    }

    uint64_t earliest_deadline_unlocked() const {
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

    void advance_by_unlocked(
            uint64_t delta,
            TimerDevice& timer,
            FramebufferDevice& framebuffer,
            RTCDevice& rtc) {
        if (delta == 0)
            return;
        if (cycles_ > std::numeric_limits<uint64_t>::max() - delta)
            throw std::overflow_error("system cycle counter overflow");
        const uint64_t target = cycles_ + delta;
        if (active_sources_ != 0 &&
            target > earliest_deadline_unlocked()) {
            throw std::invalid_argument(
                "system clock advance cannot cross the event horizon");
        }

        // Preserve the current DeviceBus tick order and call shape.  The
        // future scheduler will stop at event horizons before invoking this
        // operation; this ownership slice deliberately does not reinterpret
        // legacy Python instruction counts as system cycles.
        timer.tick(delta);
        framebuffer.tick(delta);
        rtc.tick(delta);
        cycles_ = target;
    }

    mutable std::mutex mutex_;
    uint64_t cycles_ = 0;
    std::array<uint64_t, EVENT_SOURCE_COUNT> deadlines_{};
    uint64_t active_sources_ = 0;
};

// ---------------------------------------------------------------------------
//  Timestamped external ingress
// ---------------------------------------------------------------------------

enum class ExternalEventKind : uint8_t {
    UART_RX = 0,
    NIC_RX = 1,
    UART_GEOMETRY = 2,
    UART_GEOMETRY_ACCEPT = 3,
    UART_GEOMETRY_DENY = 4,
    NIC_RX_REJECTED = 5,
    UART_GEOMETRY_ACCEPT_UNCONDITIONAL = 6,
    UART_GEOMETRY_DENY_UNCONDITIONAL = 7,
};

enum class ExternalEventReleasePhase : uint8_t {
    SCHEDULER = 0,
    BEFORE_BATCH = 1,
    AFTER_BATCH = 2,
};

struct ExternalEventRecord {
    uint64_t cycle = 0;
    uint64_t sequence = 0;
    ExternalEventKind kind = ExternalEventKind::UART_RX;
    std::vector<uint8_t> payload;
    uint64_t argument0 = 0;
    uint64_t argument1 = 0;
    // Zero means the event is visible directly on the strict scheduler
    // timeline. A positive value identifies the positive batch whose open
    // or close boundary makes host-timed ingress visible.
    uint64_t release_boundary = 0;
    ExternalEventReleasePhase release_phase =
        ExternalEventReleasePhase::SCHEDULER;
};

class ExternalEventInbox {
public:
    uint64_t enqueue(
            uint64_t current_cycle,
            uint64_t event_cycle,
            ExternalEventKind kind,
            std::vector<uint8_t> payload,
            uint64_t argument0,
            uint64_t argument1) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (replay_sealed_) {
            throw std::runtime_error(
                "live external ingress is disabled during replay");
        }
        if (event_cycle < current_cycle) {
            throw std::invalid_argument(
                "external event cycle cannot precede current system time");
        }
        const bool visible_before_next_batch =
            event_cycle == current_cycle;
        if (
            visible_before_next_batch &&
            completed_staging_boundaries_ ==
                std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "external event batch boundary counter overflow");
        }

        ExternalEventRecord record{
            event_cycle,
            allocate_sequence_unlocked(),
            kind,
            std::move(payload),
            argument0,
            argument1,
            visible_before_next_batch
                ? completed_staging_boundaries_ + 1
                : 0,
            visible_before_next_batch
                ? ExternalEventReleasePhase::BEFORE_BATCH
                : ExternalEventReleasePhase::SCHEDULER,
        };
        const uint64_t sequence = record.sequence;
        history_.push_back(record);
        const auto position = std::upper_bound(
            pending_.begin(),
            pending_.end(),
            record,
            [](const ExternalEventRecord& lhs,
               const ExternalEventRecord& rhs) {
                if (lhs.cycle != rhs.cycle)
                    return lhs.cycle < rhs.cycle;
                return lhs.sequence < rhs.sequence;
            });
        pending_.insert(position, std::move(record));
        return sequence;
    }

    uint64_t begin_staging(uint64_t cycle) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (staging_open_) {
            throw std::logic_error(
                "external event staging is already open");
        }
        if (
            completed_staging_boundaries_ ==
            std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "external event batch boundary counter overflow");
        }
        const uint64_t boundary =
            completed_staging_boundaries_ + 1;
        uint64_t released = 0;
        if (replay_sealed_) {
            released = release_replay_boundary_unlocked(
                replay_before_,
                boundary,
                cycle,
                "before",
                true);
        }
        staging_open_ = true;
        active_staging_boundary_ = boundary;
        return released;
    }

    std::optional<uint64_t> try_stage(
            ExternalEventKind kind,
            std::vector<uint8_t> payload,
            uint64_t argument0,
            uint64_t argument1) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (replay_sealed_) {
            throw std::runtime_error(
                "live external ingress is disabled during replay");
        }
        if (!staging_open_)
            return std::nullopt;
        const uint64_t sequence =
            allocate_sequence_unlocked();
        staged_.push_back(
            ExternalEventRecord{
                0,
                sequence,
                kind,
                std::move(payload),
                argument0,
                argument1,
                active_staging_boundary_,
                ExternalEventReleasePhase::AFTER_BATCH,
            });
        return std::optional<uint64_t>{sequence};
    }

    uint64_t close_staging(uint64_t cycle) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!staging_open_) {
            throw std::logic_error(
                "external event staging is not open");
        }
        const uint64_t release_boundary =
            active_staging_boundary_;
        if (
            release_boundary == 0 ||
            release_boundary !=
                completed_staging_boundaries_ + 1
        ) {
            throw std::logic_error(
                "external event batch boundary ownership diverged");
        }

        if (replay_sealed_) {
            const uint64_t released =
                release_replay_boundary_unlocked(
                    replay_after_,
                    release_boundary,
                    cycle,
                    "after",
                    true);
            staging_open_ = false;
            completed_staging_boundaries_ = release_boundary;
            active_staging_boundary_ = 0;
            return released;
        }

        // Closing under the inbox mutex is the handoff linearization point.
        // An arrival either joins staged_ before this transition or observes
        // the closed gate and serializes through the scheduler afterward.
        const uint64_t count =
            static_cast<uint64_t>(staged_.size());
        std::vector<ExternalEventRecord> published_history =
            history_;
        std::vector<ExternalEventRecord> published_pending =
            pending_;
        for (ExternalEventRecord record : staged_) {
            record.cycle = cycle;
            record.release_boundary = release_boundary;
            record.release_phase =
                ExternalEventReleasePhase::AFTER_BATCH;
            published_history.push_back(record);
            published_pending.push_back(std::move(record));
        }
        std::stable_sort(
            published_pending.begin(),
            published_pending.end(),
            cycle_sequence_less);

        staging_open_ = false;
        completed_staging_boundaries_ = release_boundary;
        active_staging_boundary_ = 0;
        history_.swap(published_history);
        pending_.swap(published_pending);
        staged_.clear();
        return count;
    }

    std::optional<uint64_t> next_cycle() const {
        std::lock_guard<std::mutex> guard(mutex_);
        if (pending_.empty())
            return std::nullopt;
        return pending_.front().cycle;
    }

    std::vector<ExternalEventRecord> take_due(uint64_t cycle) {
        std::lock_guard<std::mutex> guard(mutex_);
        auto end = pending_.begin();
        while (end != pending_.end() && end->cycle <= cycle)
            ++end;
        std::vector<ExternalEventRecord> due(
            std::make_move_iterator(pending_.begin()),
            std::make_move_iterator(end));
        pending_.erase(pending_.begin(), end);
        return due;
    }

    std::vector<ExternalEventRecord> pending_snapshot() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return pending_;
    }

    std::vector<ExternalEventRecord> history_snapshot() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return history_;
    }

    uint64_t next_sequence() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return next_sequence_;
    }

    uint64_t completed_staging_boundaries() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return completed_staging_boundaries_;
    }

    std::optional<uint64_t> next_before_cycle() const {
        std::lock_guard<std::mutex> guard(mutex_);
        if (replay_before_.empty())
            return std::nullopt;
        if (
            completed_staging_boundaries_ ==
            std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "external event batch boundary counter overflow");
        }
        const uint64_t next_boundary =
            completed_staging_boundaries_ + 1;
        if (
            replay_before_.front().release_boundary <
            next_boundary
        ) {
            throw std::logic_error(
                "external event replay missed a pre-batch boundary");
        }
        // The absolute ingress cycle remains a scheduler horizon even when
        // an intervening positive batch must complete before the recorded
        // BEFORE_BATCH boundary can release it.  Hiding a later-boundary
        // record here would let explicit clock progression cross its cycle
        // and mutate devices before replay divergence was detected.
        return replay_before_.front().cycle;
    }

    uint64_t release_before_next_batch(uint64_t cycle) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (!replay_sealed_)
            return 0;
        if (staging_open_)
            return 0;
        if (
            completed_staging_boundaries_ ==
            std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "external event batch boundary counter overflow");
        }
        return release_replay_boundary_unlocked(
            replay_before_,
            completed_staging_boundaries_ + 1,
            cycle,
            "before",
            false);
    }

    bool replay_sealed() const {
        std::lock_guard<std::mutex> guard(mutex_);
        return replay_sealed_;
    }

    void install_replay(
            uint64_t current_cycle,
            const std::vector<ExternalEventRecord>& records) {
        std::lock_guard<std::mutex> guard(mutex_);
        if (
            staging_open_ ||
            !staged_.empty() ||
            !pending_.empty() ||
            !history_.empty() ||
            !replay_before_.empty() ||
            !replay_after_.empty() ||
            active_staging_boundary_ != 0 ||
            completed_staging_boundaries_ != 0 ||
            next_sequence_ != 1 ||
            replay_sealed_
        ) {
            throw std::runtime_error(
                "external event replay requires a fresh journal and "
                "batch-boundary timeline");
        }

        uint64_t expected_sequence = 1;
        uint64_t last_release_boundary = 0;
        uint64_t last_release_cycle = 0;
        ExternalEventReleasePhase last_release_phase =
            ExternalEventReleasePhase::SCHEDULER;
        for (const ExternalEventRecord& record : records) {
            if (record.cycle < current_cycle) {
                throw std::invalid_argument(
                    "external event replay precedes current system time");
            }
            if (record.sequence != expected_sequence) {
                throw std::invalid_argument(
                    "external event replay sequences must be contiguous "
                    "from one");
            }
            switch (record.release_phase) {
                case ExternalEventReleasePhase::SCHEDULER:
                    if (record.release_boundary != 0) {
                        throw std::invalid_argument(
                            "scheduler external replay events cannot "
                            "name a batch boundary");
                    }
                    break;
                case ExternalEventReleasePhase::BEFORE_BATCH:
                case ExternalEventReleasePhase::AFTER_BATCH:
                    if (record.release_boundary == 0) {
                        throw std::invalid_argument(
                            "host-timed external replay events require "
                            "a positive batch boundary");
                    }
                    break;
                default:
                    throw std::invalid_argument(
                        "external event replay release phase is invalid");
            }
            if (
                record.release_phase !=
                ExternalEventReleasePhase::SCHEDULER
            ) {
                if (
                    record.release_boundary <
                    last_release_boundary
                ) {
                    throw std::invalid_argument(
                        "external event replay batch boundaries must "
                        "be monotonic");
                }
                if (
                    record.release_boundary ==
                        last_release_boundary &&
                    static_cast<uint8_t>(record.release_phase) <
                        static_cast<uint8_t>(last_release_phase)
                ) {
                    throw std::invalid_argument(
                        "external event replay batch phases must be "
                        "monotonic");
                }
                if (
                    record.release_boundary ==
                        last_release_boundary &&
                    record.release_phase ==
                        ExternalEventReleasePhase::AFTER_BATCH &&
                    record.release_phase ==
                        last_release_phase &&
                    record.cycle != last_release_cycle
                ) {
                    throw std::invalid_argument(
                        "one post-batch external replay phase must "
                        "have one cycle");
                }
                if (
                    last_release_boundary != 0 &&
                    record.cycle < last_release_cycle
                ) {
                    throw std::invalid_argument(
                        "external event replay batch cycles must be "
                        "monotonic");
                }
                last_release_boundary =
                    record.release_boundary;
                last_release_cycle = record.cycle;
                last_release_phase = record.release_phase;
            }
            if (
                expected_sequence ==
                std::numeric_limits<uint64_t>::max()
            ) {
                throw std::overflow_error(
                    "external event replay sequence counter overflow");
            }
            expected_sequence++;
        }

        // Build every owning copy before publishing any one, so allocation
        // failure also leaves the fresh journal untouched. Scheduler-visible
        // records sort by (cycle, sequence); staged live records remain
        // withheld until their recorded positive batch boundary closes.
        std::vector<ExternalEventRecord> replay_history =
            records;
        std::vector<ExternalEventRecord> replay_pending;
        std::vector<ExternalEventRecord> replay_before;
        std::vector<ExternalEventRecord> replay_after;
        replay_pending.reserve(records.size());
        replay_before.reserve(records.size());
        replay_after.reserve(records.size());
        for (const ExternalEventRecord& record : records) {
            switch (record.release_phase) {
                case ExternalEventReleasePhase::SCHEDULER:
                    replay_pending.push_back(record);
                    break;
                case ExternalEventReleasePhase::BEFORE_BATCH:
                    replay_before.push_back(record);
                    break;
                case ExternalEventReleasePhase::AFTER_BATCH:
                    replay_after.push_back(record);
                    break;
                default:
                    throw std::logic_error(
                        "validated external replay phase diverged");
            }
        }
        std::stable_sort(
            replay_pending.begin(),
            replay_pending.end(),
            cycle_sequence_less);
        const auto boundary_sequence_less =
            [](const ExternalEventRecord& lhs,
               const ExternalEventRecord& rhs) {
                if (
                    lhs.release_boundary !=
                    rhs.release_boundary
                ) {
                    return lhs.release_boundary <
                        rhs.release_boundary;
                }
                return lhs.sequence < rhs.sequence;
            };
        std::stable_sort(
            replay_before.begin(),
            replay_before.end(),
            boundary_sequence_less);
        std::stable_sort(
            replay_after.begin(),
            replay_after.end(),
            boundary_sequence_less);

        // Publish only after the complete replay has passed validation.
        // History retains ingress sequence order. Vector swap is non-throwing
        // for these standard-allocator vectors.
        history_.swap(replay_history);
        pending_.swap(replay_pending);
        replay_before_.swap(replay_before);
        replay_after_.swap(replay_after);
        next_sequence_ = expected_sequence;
        replay_sealed_ = true;
    }

private:
    static bool cycle_sequence_less(
            const ExternalEventRecord& lhs,
            const ExternalEventRecord& rhs) {
        if (lhs.cycle != rhs.cycle)
            return lhs.cycle < rhs.cycle;
        return lhs.sequence < rhs.sequence;
    }

    uint64_t release_replay_boundary_unlocked(
            std::vector<ExternalEventRecord>& deferred,
            uint64_t boundary,
            uint64_t cycle,
            const char* phase_name,
            bool require_complete) {
        auto end = deferred.begin();
        while (
            end != deferred.end() &&
            end->release_boundary <= boundary
        ) {
            if (end->release_boundary < boundary) {
                throw std::logic_error(
                    std::string("external event replay missed a ") +
                    phase_name + " batch boundary");
            }
            if (end->cycle > cycle)
                break;
            if (end->cycle != cycle) {
                throw std::runtime_error(
                    std::string("external event replay ") +
                    phase_name +
                    " batch boundary cycle diverged");
            }
            ++end;
        }
        if (
            require_complete &&
            end != deferred.end() &&
            end->release_boundary == boundary
        ) {
            throw std::runtime_error(
                std::string("external event replay ") +
                phase_name +
                " batch boundary cycle diverged");
        }

        const std::size_t released =
            static_cast<std::size_t>(
                end - deferred.begin());
        if (released == 0)
            return 0;

        // Build the published queue first so allocation failure leaves this
        // boundary retryable.
        std::vector<ExternalEventRecord> published_pending =
            pending_;
        published_pending.insert(
            published_pending.end(),
            deferred.begin(),
            end);
        std::stable_sort(
            published_pending.begin(),
            published_pending.end(),
            cycle_sequence_less);
        pending_.swap(published_pending);
        deferred.erase(deferred.begin(), end);
        return static_cast<uint64_t>(released);
    }

    uint64_t allocate_sequence_unlocked() {
        if (next_sequence_ ==
            std::numeric_limits<uint64_t>::max()) {
            throw std::overflow_error(
                "external event sequence counter overflow");
        }
        return next_sequence_++;
    }

    mutable std::mutex mutex_;
    std::vector<ExternalEventRecord> pending_;
    std::vector<ExternalEventRecord> history_;
    std::vector<ExternalEventRecord> staged_;
    std::vector<ExternalEventRecord> replay_before_;
    std::vector<ExternalEventRecord> replay_after_;
    bool staging_open_ = false;
    bool replay_sealed_ = false;
    uint64_t active_staging_boundary_ = 0;
    uint64_t completed_staging_boundaries_ = 0;
    uint64_t next_sequence_ = 1;
};

// ---------------------------------------------------------------------------
//  Explicit main-bus transactions and integrated-SoC arbitration
// ---------------------------------------------------------------------------

enum class BusOperation : uint8_t {
    READ = 0,
    WRITE = 1,
};

// Byte counts are used here rather than the RTL's encoded size values so a
// request describes the guest transaction directly.  The target adapter will
// translate 1/2/4/8 to BUS_BYTE/BUS_HALF/BUS_WORD/BUS_DWORD.
enum class BusWidth : uint8_t {
    BYTE = 1,
    HALF = 2,
    WORD = 4,
    DOUBLEWORD = 8,
};

enum class BusTarget : uint8_t {
    MEMORY = 0,
    MMIO = 1,
};

enum class BusFault : uint8_t {
    NONE = 0,
    MMIO_TIMEOUT = 1,
    MEMORY_TIMEOUT = 2,
    TARGET_FAULT = 3,
};

static bool main_bus_address_is_mmio(uint64_t address) {
    return static_cast<uint32_t>(address >> 32) == 0xFFFFFF00U;
}

struct BusOrderingMetadata {
    int main_port_id = 0;
    uint64_t issue_sequence = 0;
    bool port_io = false;
};

struct BusRequest {
    int requester_id = 0;
    uint64_t ready_cycle = 0;
    BusOperation operation = BusOperation::READ;
    uint64_t address = 0;
    BusWidth width = BusWidth::BYTE;
    uint64_t write_data = 0;
    BusOrderingMetadata ordering{};
};

struct BusGrant {
    BusRequest request{};
    uint64_t grant_sequence = 0;
    uint64_t grant_cycle = 0;
    BusTarget target = BusTarget::MEMORY;
    uint64_t timeout_cycle = 0;
};

struct BusResult {
    BusGrant grant{};
    uint64_t completion_cycle = 0;
    std::optional<uint64_t> read_value;
    BusFault fault = BusFault::NONE;
    bool target_effects_committed = false;
};

// A device endpoint describes only the next immutable byte beat that it is
// ready to place on its physical main-bus port.  Eligibility belongs to the
// endpoint; ordering among simultaneously eligible endpoints belongs solely
// to MainBusArbiter's equal round robin.
struct DmaBeat {
    uint64_t token = 0;
    std::optional<uint64_t> ready_cycle;
    BusOperation operation = BusOperation::READ;
    uint64_t address = 0;
    uint8_t write_data = 0;
};

struct DmaEndpointView {
    bool active = false;
    std::optional<DmaBeat> pending;
};

struct MainBusSnapshot {
    static constexpr uint64_t SCHEMA_VERSION = 1;

    uint64_t schema_version = SCHEMA_VERSION;
    int port_count = 0;
    int last_grant = 0;
    bool reset_port_zero_credit = true;
    uint64_t next_grant_sequence = 1;
    uint64_t earliest_arbitration_cycle = 0;
    bool served_last = false;
    std::optional<uint64_t> last_arbitration_cycle;
    std::optional<BusGrant> active_grant;
    std::vector<uint64_t> last_issue_sequences;
    std::vector<uint8_t> sticky_bus_errors;
};

class MainBusArbiter {
public:
    static constexpr uint64_t MMIO_TIMEOUT_CYCLES = 64;
    static constexpr uint64_t MEMORY_TIMEOUT_CYCLES = 256;
    static constexpr uint64_t TIMEOUT_SENTINEL =
        0xDEADDEADDEADDEADULL;

    MainBusArbiter() = default;

    void configure(int port_count) {
        if (port_count < 1 || port_count > 257)
            throw std::invalid_argument(
                "main bus port count must be between 1 and 257");
        port_count_ = port_count;
        last_issue_sequences_.assign(
            static_cast<std::size_t>(port_count), 0);
        sticky_bus_errors_.assign(
            static_cast<std::size_t>(port_count), 0);
        reset(0);
    }

    int port_count() const {
        return port_count_;
    }

    std::optional<uint64_t> active_timeout_cycle() const {
        if (!active_grant_.has_value())
            return std::nullopt;
        return active_grant_->timeout_cycle;
    }

    void validate_clock_target(uint64_t target_cycle) const {
        if (active_grant_.has_value() &&
            target_cycle > active_grant_->timeout_cycle) {
            throw std::invalid_argument(
                "system clock cannot cross the active main bus timeout");
        }
    }

    void reset(uint64_t system_cycle) {
        require_configured();
        last_grant_ = 0;
        // This is the literal mp64_bus reset state.  It gives port 0 the first
        // post-reset tie; after that, every eligible peer has equal weight.
        reset_port_zero_credit_ = true;
        next_grant_sequence_ = 1;
        earliest_arbitration_cycle_ = system_cycle;
        served_last_ = false;
        last_arbitration_cycle_.reset();
        active_grant_.reset();
        std::fill(
            last_issue_sequences_.begin(),
            last_issue_sequences_.end(),
            0);
        std::fill(
            sticky_bus_errors_.begin(),
            sticky_bus_errors_.end(),
            0);
    }

    std::optional<BusGrant> try_grant(
            const std::vector<BusRequest>& pending,
            uint64_t system_cycle) {
        require_configured();
        if (active_grant_.has_value())
            return std::nullopt;
        const auto slots = validate_pending(pending);
        if (system_cycle < earliest_arbitration_cycle_)
            return std::nullopt;
        if (last_arbitration_cycle_.has_value() &&
            *last_arbitration_cycle_ == system_cycle) {
            return std::nullopt;
        }

        // If the caller advanced past the first idle edge after completion,
        // the RTL has already cleared its one-edge held-valid safeguard.
        const bool held_valid_guard =
            served_last_ &&
            system_cycle == earliest_arbitration_cycle_;

        const int winner = select_port(slots, system_cycle);
        if (winner < 0) {
            // An empty IDLE edge clears served_last in mp64_bus.
            served_last_ = false;
            last_arbitration_cycle_ = system_cycle;
            return std::nullopt;
        }

        // The cycle after completion is suppressed when the same physical
        // master port still presents valid.  A different port can turn around
        // immediately even when its request was already waiting.
        if (held_valid_guard && winner == last_grant_) {
            if (system_cycle == std::numeric_limits<uint64_t>::max())
                throw std::overflow_error(
                    "main bus arbitration cycle overflow");
            served_last_ = false;
            earliest_arbitration_cycle_ = system_cycle + 1;
            last_arbitration_cycle_ = system_cycle;
            return std::nullopt;
        }

        if (next_grant_sequence_ ==
            std::numeric_limits<uint64_t>::max()) {
            throw std::overflow_error(
                "main bus grant sequence overflow");
        }

        const BusRequest& request =
            *slots[static_cast<std::size_t>(winner)];
        const BusTarget target = target_for_address(request.address);
        const uint64_t timeout_delta =
            target == BusTarget::MMIO
            ? MMIO_TIMEOUT_CYCLES
            : MEMORY_TIMEOUT_CYCLES;
        if (system_cycle >
            std::numeric_limits<uint64_t>::max() - timeout_delta) {
            throw std::overflow_error(
                "main bus timeout cycle overflow");
        }

        BusGrant grant{
            request,
            next_grant_sequence_,
            system_cycle,
            target,
            system_cycle + timeout_delta,
        };

        // Hard QoS is an eligibility/reservation concern, not a secondary
        // ordering bias.  The integrated RTL's write sideband is tied off, so
        // this primitive orders all ready peers by equal round-robin.  Consume
        // the sole reset credit after the first issued transaction.
        reset_port_zero_credit_ = false;

        next_grant_sequence_++;
        last_issue_sequences_[static_cast<std::size_t>(winner)] =
            request.ordering.issue_sequence;
        served_last_ = false;
        last_arbitration_cycle_ = system_cycle;
        active_grant_ = grant;
        return grant;
    }

    std::optional<uint64_t> next_arbitration_cycle(
            const std::vector<BusRequest>& pending,
            uint64_t system_cycle) const {
        require_configured();
        if (active_grant_.has_value())
            return std::nullopt;
        validate_pending(pending);
        if (pending.empty())
            return std::nullopt;

        uint64_t first_cycle = std::max(
            system_cycle, earliest_arbitration_cycle_);
        if (last_arbitration_cycle_.has_value() &&
            *last_arbitration_cycle_ == first_cycle) {
            if (first_cycle == std::numeric_limits<uint64_t>::max())
                return std::nullopt;
            first_cycle++;
        }

        std::optional<uint64_t> earliest;
        for (const BusRequest& request : pending) {
            const uint64_t candidate =
                std::max(first_cycle, request.ready_cycle);
            if (!earliest.has_value() || candidate < *earliest)
                earliest = candidate;
        }
        return earliest;
    }

    BusResult complete(
            uint64_t grant_sequence,
            uint64_t system_cycle,
            std::optional<uint64_t> read_value,
            BusFault fault,
            bool target_effects_committed) {
        require_configured();
        if (!active_grant_.has_value())
            throw std::runtime_error(
                "main bus has no active grant to complete");

        const BusGrant grant = *active_grant_;
        if (grant_sequence != grant.grant_sequence)
            throw std::invalid_argument(
                "main bus completion does not match the active grant");
        if (system_cycle <= grant.grant_cycle)
            throw std::invalid_argument(
                "main bus completion must follow its grant cycle");
        if (system_cycle > grant.timeout_cycle)
            throw std::invalid_argument(
                "main bus completion cannot follow its timeout cycle");
        if (system_cycle == std::numeric_limits<uint64_t>::max())
            throw std::overflow_error(
                "main bus turnaround cycle overflow");

        const bool is_timeout =
            fault == BusFault::MMIO_TIMEOUT ||
            fault == BusFault::MEMORY_TIMEOUT;
        if (!valid_fault(fault))
            throw std::invalid_argument("main bus fault is invalid");
        if (fault == BusFault::MMIO_TIMEOUT &&
            grant.target != BusTarget::MMIO) {
            throw std::invalid_argument(
                "MMIO timeout cannot complete a memory grant");
        }
        if (fault == BusFault::MEMORY_TIMEOUT &&
            grant.target != BusTarget::MEMORY) {
            throw std::invalid_argument(
                "memory timeout cannot complete an MMIO grant");
        }
        if (is_timeout && system_cycle != grant.timeout_cycle)
            throw std::invalid_argument(
                "main bus timeout must complete on its timeout cycle");
        if (is_timeout && target_effects_committed)
            throw std::invalid_argument(
                "a timed-out target cannot report committed effects");
        if (is_timeout && read_value.has_value())
            throw std::invalid_argument(
                "main bus timeout supplies its sentinel value");
        if (fault == BusFault::NONE &&
            grant.request.operation == BusOperation::READ &&
            !read_value.has_value()) {
            throw std::invalid_argument(
                "a successful bus read requires a result value");
        }

        if (is_timeout)
            read_value = TIMEOUT_SENTINEL;

        const std::size_t port = static_cast<std::size_t>(
            grant.request.ordering.main_port_id);
        if (is_timeout)
            sticky_bus_errors_[port] = 1;

        BusResult result{
            grant,
            system_cycle,
            read_value,
            fault,
            target_effects_committed,
        };

        last_grant_ = static_cast<int>(port);
        earliest_arbitration_cycle_ = system_cycle + 1;
        served_last_ = true;
        active_grant_.reset();
        return result;
    }

    MainBusSnapshot snapshot() const {
        require_configured();
        return {
            MainBusSnapshot::SCHEMA_VERSION,
            port_count_,
            last_grant_,
            reset_port_zero_credit_,
            next_grant_sequence_,
            earliest_arbitration_cycle_,
            served_last_,
            last_arbitration_cycle_,
            active_grant_,
            last_issue_sequences_,
            sticky_bus_errors_,
        };
    }

private:
    static bool valid_operation(BusOperation operation) {
        return operation == BusOperation::READ ||
               operation == BusOperation::WRITE;
    }

    static bool valid_width(BusWidth width) {
        return width == BusWidth::BYTE ||
               width == BusWidth::HALF ||
               width == BusWidth::WORD ||
               width == BusWidth::DOUBLEWORD;
    }

    static bool valid_fault(BusFault fault) {
        return fault == BusFault::NONE ||
               fault == BusFault::MMIO_TIMEOUT ||
               fault == BusFault::MEMORY_TIMEOUT ||
               fault == BusFault::TARGET_FAULT;
    }

    static BusTarget target_for_address(uint64_t address) {
        return main_bus_address_is_mmio(address)
            ? BusTarget::MMIO
            : BusTarget::MEMORY;
    }

    void require_configured() const {
        if (port_count_ < 1)
            throw std::logic_error("main bus arbiter is not configured");
    }

    std::vector<const BusRequest*> validate_pending(
            const std::vector<BusRequest>& pending) const {
        std::vector<const BusRequest*> slots(
            static_cast<std::size_t>(port_count_), nullptr);
        for (const BusRequest& request : pending) {
            const int port = request.ordering.main_port_id;
            if (port < 0 || port >= port_count_)
                throw std::out_of_range(
                    "main bus request port is out of range");
            if (request.ordering.issue_sequence == 0)
                throw std::invalid_argument(
                    "main bus issue sequence must be positive");
            if (!valid_operation(request.operation))
                throw std::invalid_argument(
                    "main bus operation is invalid");
            if (!valid_width(request.width))
                throw std::invalid_argument(
                    "main bus width must be 1, 2, 4, or 8 bytes");

            const std::size_t index = static_cast<std::size_t>(port);
            if (slots[index] != nullptr)
                throw std::invalid_argument(
                    "main bus pending snapshot contains duplicate ports");
            if (request.ordering.issue_sequence <=
                last_issue_sequences_[index]) {
                throw std::invalid_argument(
                    "main bus issue sequence must advance per port");
            }
            slots[index] = &request;
        }
        return slots;
    }

    int select_port(
            const std::vector<const BusRequest*>& slots,
            uint64_t system_cycle) const {
        const auto eligible = [&](int port) {
            const BusRequest* request =
                slots[static_cast<std::size_t>(port)];
            return request != nullptr &&
                   request->ready_cycle <= system_cycle;
        };

        if (reset_port_zero_credit_ && eligible(last_grant_))
            return last_grant_;

        for (int offset = 1; offset <= port_count_; offset++) {
            int candidate = last_grant_ + offset;
            if (candidate >= port_count_)
                candidate -= port_count_;
            if (eligible(candidate))
                return candidate;
        }
        return -1;
    }

    int port_count_ = 0;
    int last_grant_ = 0;
    bool reset_port_zero_credit_ = true;
    uint64_t next_grant_sequence_ = 1;
    uint64_t earliest_arbitration_cycle_ = 0;
    bool served_last_ = false;
    std::optional<uint64_t> last_arbitration_cycle_;
    std::optional<BusGrant> active_grant_;
    std::vector<uint64_t> last_issue_sequences_;
    std::vector<uint8_t> sticky_bus_errors_;
};

// ---------------------------------------------------------------------------
//  CPU State — flat execution state plus borrowed memory mappings
// ---------------------------------------------------------------------------

enum class CoreProfile : uint8_t {
    FULL = 0,
    MICRO = 1,
};

class ResumableBusAccess;

struct CPUState {
    CoreProfile profile = CoreProfile::FULL;
    uint64_t regs[32];      // GP registers (R0-R15 base, R16-R31 via REX)
    uint8_t  psel;          // PC register index
    uint8_t  xsel;          // X register index
    uint8_t  spsel;         // SP register index

    // Flags (1 bit each, stored as bytes for speed)
    uint8_t flag_z, flag_c, flag_n, flag_v;
    uint8_t flag_p, flag_g, flag_i, flag_s;

    // 1802 legacy
    uint8_t  d_reg;
    uint8_t  q_out;
    uint16_t t_reg;          // T packs {XSEL[4:0]<<8 | PSEL[4:0]} — needs 10+ bits

    // Tile CSRs
    uint64_t sb, sr, sc, sw;
    uint64_t tmode, tctrl;
    uint64_t tsrc0, tsrc1, tdst;
    uint64_t acc[4];

    // Physical tile-engine accumulator state. This is authoritative for a
    // full core and transient compatibility staging for a microcore, whose
    // physical engine state lives in its owning ClusterState.
    std::array<uint8_t, TACC_IMAGE_BYTES> tacc{};
    uint8_t tacc_owner = TACC_OWNER_NONE;
    bool tacc_valid = false;
    bool tacc_dirty = false;
    uint8_t tacc_format_ew = 0;
    uint8_t tacc_format_signed = 0;
    bool tacc_busy = false;
    bool tacc_force_pending = false;
    uint64_t tacc_epoch = 0;

    void reset_tacc(bool bump_epoch = true) noexcept {
        tacc.fill(0);
        tacc_owner = TACC_OWNER_NONE;
        tacc_valid = false;
        tacc_dirty = false;
        tacc_format_ew = 0;
        tacc_format_signed = 0;
        tacc_busy = false;
        tacc_force_pending = false;
        if (bump_epoch)
            tacc_epoch++;
    }

    // System CSRs
    uint64_t ivt_base;
    uint64_t ivec_id;
    uint64_t trap_addr;

    // External flags
    uint8_t  ef_flags;

    // I/O ports
    uint8_t  port_out[8];
    uint8_t  port_in[8];

    // Port I/O bridge remap table — port_map[1..7] = 12-bit MMIO offset
    // 0xFFFF = disabled (legacy port_out/port_in only)
    uint32_t port_map[8];

    // State
    bool halted;
    bool idle;
    uint64_t cycle_count;

    // Strided tile addressing
    uint64_t tstride_r, tstride_c;
    uint64_t ttile_h, ttile_w;

    // Performance counters
    uint8_t perf_enable;
    uint64_t perf_cycles, perf_stalls, perf_tileops, perf_extmem;

    // BIST
    uint64_t bist_status, bist_fail_addr, bist_fail_data;
    uint64_t tile_selftest, tile_st_detail;

    // Private full-core I-cache: 256 direct-mapped 16-byte lines.  Tags
    // retain every physical-address bit above the line/index fields.
    static constexpr std::size_t ICACHE_LINES = 256;
    static constexpr std::size_t ICACHE_LINE_BYTES = 16;
    uint8_t  icache_enabled;
    uint64_t icache_hits, icache_misses;
    std::array<uint8_t, ICACHE_LINES> icache_valid{};
    std::array<uint64_t, ICACHE_LINES> icache_tags{};
    std::array<
        std::array<uint8_t, ICACHE_LINE_BYTES>,
        ICACHE_LINES
    > icache_data{};
    bool ifetch_window_valid = false;
    uint64_t ifetch_window_addr = 0;
    uint64_t ifetch_window_data = 0;
    ResumableBusAccess* instruction_bus_access = nullptr;
    struct IcacheUndoLine {
        std::size_t index = 0;
        uint8_t valid = 0;
        uint64_t tag = 0;
        std::array<uint8_t, ICACHE_LINE_BYTES> data{};
    };
    std::array<IcacheUndoLine, 4> icache_undo_lines{};
    std::size_t icache_undo_count = 0;
    uint64_t icache_undo_hits = 0;
    uint64_t icache_undo_misses = 0;

    // Privilege level (0=supervisor, 1=user)
    uint8_t  priv_level;

    // MPU — user-mode memory window
    uint64_t mpu_base;   // inclusive lower bound
    uint64_t mpu_limit;  // exclusive upper bound

    // EXT prefix
    int ext_modifier;   // -1 = none

    // EXT.DICT hardware dictionary cache (64 sets × 4 ways)
    static constexpr int DICT_SETS = 64;
    static constexpr int DICT_WAYS = 4;
    static constexpr int DICT_MAX_NAME = 31;
    struct DictEntry {
        bool     valid;
        uint32_t hash;
        uint8_t  name_len;
        uint8_t  name[31];  // max 31 bytes (5-bit length)
        uint64_t xt;
    };
    DictEntry dict_table[64][4];  // zero-initialized by default

    void dict_clear_all() {
        std::memset(dict_table, 0, sizeof(dict_table));
    }

    // EXT.CRYPTO CRC state for accelerated full cores (Appendix B, §B.3)
    uint64_t crc_acc;   // 64-bit CRC accumulator
    uint8_t  crc_mode;  // exact non-reflected parameter tuple 0/1/2

    // EXT.CRYPTO SHA-2 per-core state (Appendix B, §B.4)
    uint8_t  sha_mode;       // 0=SHA-256, 1=SHA-384, 2=SHA-512
    uint64_t sha_msglen_lo;  // total message length in bits (low 64)
    uint64_t sha_msglen_hi;  // total message length in bits (high 64)

    // EXT.CRYPTO Field ALU per-core state (Appendix B, §B.5)
    uint8_t  gf_prime_sel;   // 0=Curve25519, 1=secp256k1, 2=P-256, 3=custom
    BigNum   gf_custom_p;    // 256-bit custom prime
    BigNum   gf_mont_pinv;   // -p^{-1} mod 2^{256}
    BigNum   gf_prev_lo;     // previous result (low 256)
    BigNum   gf_prev_hi;     // previous result (high 256, for MACR)

    // Core identity
    uint8_t  core_id;
    uint8_t  num_cores;
    // System-owned cores reject guest execution while their parent's native
    // scheduler batch is active. Standalone cores leave this pointer null.
    std::atomic<bool>* system_batch_active = nullptr;
    // A cycle-bounded call may leave one instruction suspended across public
    // calls. Direct per-core execution must not bypass that continuation.
    std::atomic<bool>* system_cycle_execution_pending = nullptr;

    // System-owned cores borrow the central IPI router.  Standalone cores
    // preserve the historical manually settable interrupt-line latch.
    InterruptRouter* interrupts = nullptr;
    std::atomic<bool> private_irq_ipi{false};

    // Standalone states own one private mapping.  System-owned states borrow
    // their parent's shared mapping and leave private_memory empty.
    std::unique_ptr<MemoryMappings> private_memory;
    MemoryMappings* memory = nullptr;

    // Standalone states retain private native SoC peripherals. System-owned
    // states borrow the one corresponding device retained by SystemState.
    std::unique_ptr<CryptoDevices> private_crypto;
    CryptoDevices* crypto = nullptr;
    std::unique_ptr<NICDevice> private_nic;
    NICDevice* nic = nullptr;
    std::unique_ptr<TRNGDevice> private_trng;
    TRNGDevice* trng = nullptr;
    std::unique_ptr<UARTDevice> private_uart;
    UARTDevice* uart = nullptr;

    // Standalone states own a private framebuffer.  System-owned states
    // borrow the one scanout controller retained by SystemState.
    std::unique_ptr<FramebufferDevice> private_fb;
    FramebufferDevice* fb = nullptr;

    // Standalone states own a private timer.  System-owned states borrow the
    // one architecturally singleton timer retained by SystemState.
    std::unique_ptr<TimerDevice> private_timer;
    TimerDevice* timer = nullptr;

    // Standalone states own a private RTC. System-owned states borrow the
    // one SoC clock retained by SystemState.
    std::unique_ptr<RTCDevice> private_rtc;
    RTCDevice* rtc = nullptr;

    // Standalone states own private terminal geometry.  System-owned states
    // borrow the one host/guest geometry block retained by SystemState.
    std::unique_ptr<UartGeomDevice> private_uart_geom;
    UartGeomDevice* uart_geom = nullptr;

    // Accelerator hooks — intercept CALL.L to known BIOS word addresses
    static constexpr int MAX_ACCEL_HOOKS = 8;
    struct AccelHookEntry {
        uint64_t addr;
        int      id;    // 1=RECT_FILL, 2=BLIT_GLYPH, 3=COPY, 4=STRING
        std::vector<uint8_t> code_identity;
    };
    AccelHookEntry accel_hooks[MAX_ACCEL_HOOKS];
    int accel_hook_count = 0;

    // Host-only private decode/admission plans live after the established
    // execution state so adding them does not displace hot architectural
    // fields. Entries are never architectural state: every hit is validated
    // against the complete encoding bytes visible through this core's
    // instruction-observation path before it can authorize private execution.
    static constexpr std::size_t
        PRIVATE_DECODE_CACHE_ENTRIES = 128;
    static constexpr std::size_t
        PRIVATE_DECODE_IDENTITY_BYTES = 16;
    static_assert(
        (
            PRIVATE_DECODE_CACHE_ENTRIES &
            (PRIVATE_DECODE_CACHE_ENTRIES - 1)
        ) == 0,
        "private decode cache must have power-of-two geometry");
    struct PrivateDecodeCacheEntry {
        bool valid = false;
        uint64_t address = 0;
        uint8_t identity_size = 0;
        std::array<
            uint8_t,
            PRIVATE_DECODE_IDENTITY_BYTES>
            identity{};
    };
    std::array<
        PrivateDecodeCacheEntry,
        PRIVATE_DECODE_CACHE_ENTRIES>
        private_decode_cache{};

    void clear_private_decode_cache() noexcept {
        for (PrivateDecodeCacheEntry& entry :
             private_decode_cache) {
            entry.valid = false;
        }
    }

    void register_accel_hook(
        uint64_t addr,
        int hook_id,
        uint64_t code_size);
};

// A suspended system-owned instruction may be decoded and executed more than
// once while completed shared accesses are replayed from an effect journal.
// Only core-private architectural state is rewound.  Borrowed owners,
// mappings, shared devices, interrupt lines, and host synchronization objects
// deliberately remain outside this checkpoint.
#define MP64_EXECUTION_CHECKPOINT_SCALARS(X) \
    X(uint8_t, psel) \
    X(uint8_t, xsel) \
    X(uint8_t, spsel) \
    X(uint8_t, flag_z) \
    X(uint8_t, flag_c) \
    X(uint8_t, flag_n) \
    X(uint8_t, flag_v) \
    X(uint8_t, flag_p) \
    X(uint8_t, flag_g) \
    X(uint8_t, flag_i) \
    X(uint8_t, flag_s) \
    X(uint8_t, d_reg) \
    X(uint8_t, q_out) \
    X(uint16_t, t_reg) \
    X(uint64_t, sb) \
    X(uint64_t, sr) \
    X(uint64_t, sc) \
    X(uint64_t, sw) \
    X(uint64_t, tmode) \
    X(uint64_t, tctrl) \
    X(uint64_t, tsrc0) \
    X(uint64_t, tsrc1) \
    X(uint64_t, tdst) \
    X(uint8_t, tacc_owner) \
    X(bool, tacc_valid) \
    X(bool, tacc_dirty) \
    X(uint8_t, tacc_format_ew) \
    X(uint8_t, tacc_format_signed) \
    X(bool, tacc_busy) \
    X(bool, tacc_force_pending) \
    X(uint64_t, tacc_epoch) \
    X(uint64_t, ivt_base) \
    X(uint64_t, ivec_id) \
    X(uint64_t, trap_addr) \
    X(uint8_t, ef_flags) \
    X(bool, halted) \
    X(bool, idle) \
    X(uint64_t, cycle_count) \
    X(uint64_t, tstride_r) \
    X(uint64_t, tstride_c) \
    X(uint64_t, ttile_h) \
    X(uint64_t, ttile_w) \
    X(uint8_t, perf_enable) \
    X(uint64_t, perf_cycles) \
    X(uint64_t, perf_stalls) \
    X(uint64_t, perf_tileops) \
    X(uint64_t, perf_extmem) \
    X(uint64_t, bist_status) \
    X(uint64_t, bist_fail_addr) \
    X(uint64_t, bist_fail_data) \
    X(uint64_t, tile_selftest) \
    X(uint64_t, tile_st_detail) \
    X(uint8_t, icache_enabled) \
    X(uint64_t, icache_hits) \
    X(uint64_t, icache_misses) \
    X(uint8_t, priv_level) \
    X(uint64_t, mpu_base) \
    X(uint64_t, mpu_limit) \
    X(int, ext_modifier) \
    X(uint64_t, crc_acc) \
    X(uint8_t, crc_mode) \
    X(uint8_t, sha_mode) \
    X(uint64_t, sha_msglen_lo) \
    X(uint64_t, sha_msglen_hi) \
    X(uint8_t, gf_prime_sel) \
    X(BigNum, gf_custom_p) \
    X(BigNum, gf_mont_pinv) \
    X(BigNum, gf_prev_lo) \
    X(BigNum, gf_prev_hi)

struct CPUExecutionCheckpoint {
    std::array<uint64_t, 32> regs{};
    std::array<uint64_t, 4> acc{};
    std::array<uint8_t, TACC_IMAGE_BYTES> tacc{};
    std::array<uint8_t, 8> port_out{};
    std::array<uint8_t, 8> port_in{};
    std::array<uint32_t, 8> port_map{};
    std::array<uint8_t, CPUState::ICACHE_LINES> icache_valid{};
    std::array<uint64_t, CPUState::ICACHE_LINES> icache_tags{};
    std::array<
        std::array<uint8_t, CPUState::ICACHE_LINE_BYTES>,
        CPUState::ICACHE_LINES
    > icache_data{};
    std::array<
        std::array<CPUState::DictEntry, CPUState::DICT_WAYS>,
        CPUState::DICT_SETS
    > dict_table{};

#define MP64_DECLARE_CHECKPOINT_FIELD(type, name) type name{};
    MP64_EXECUTION_CHECKPOINT_SCALARS(
        MP64_DECLARE_CHECKPOINT_FIELD)
#undef MP64_DECLARE_CHECKPOINT_FIELD

    explicit CPUExecutionCheckpoint(const CPUState& state) {
        std::copy(
            std::begin(state.regs),
            std::end(state.regs),
            regs.begin());
        std::copy(
            std::begin(state.acc),
            std::end(state.acc),
            acc.begin());
        tacc = state.tacc;
        std::copy(
            std::begin(state.port_out),
            std::end(state.port_out),
            port_out.begin());
        std::copy(
            std::begin(state.port_in),
            std::end(state.port_in),
            port_in.begin());
        std::copy(
            std::begin(state.port_map),
            std::end(state.port_map),
            port_map.begin());
        std::memcpy(
            dict_table.data(),
            state.dict_table,
            sizeof(state.dict_table));
        icache_valid = state.icache_valid;
        icache_tags = state.icache_tags;
        icache_data = state.icache_data;

#define MP64_CAPTURE_CHECKPOINT_FIELD(type, name) name = state.name;
        MP64_EXECUTION_CHECKPOINT_SCALARS(
            MP64_CAPTURE_CHECKPOINT_FIELD)
#undef MP64_CAPTURE_CHECKPOINT_FIELD
    }

    void restore(CPUState& state) const {
        std::copy(regs.begin(), regs.end(), std::begin(state.regs));
        std::copy(acc.begin(), acc.end(), std::begin(state.acc));
        state.tacc = tacc;
        std::copy(
            port_out.begin(),
            port_out.end(),
            std::begin(state.port_out));
        std::copy(
            port_in.begin(),
            port_in.end(),
            std::begin(state.port_in));
        std::copy(
            port_map.begin(),
            port_map.end(),
            std::begin(state.port_map));
        std::memcpy(
            state.dict_table,
            dict_table.data(),
            sizeof(state.dict_table));
        state.icache_valid = icache_valid;
        state.icache_tags = icache_tags;
        state.icache_data = icache_data;
        state.ifetch_window_valid = false;

#define MP64_RESTORE_CHECKPOINT_FIELD(type, name) state.name = name;
        MP64_EXECUTION_CHECKPOINT_SCALARS(
            MP64_RESTORE_CHECKPOINT_FIELD)
#undef MP64_RESTORE_CHECKPOINT_FIELD
    }
};

#undef MP64_EXECUTION_CHECKPOINT_SCALARS

struct BusReplayRecord {
    BusResult result{};
    std::optional<std::string> target_error_message;
};

enum class CycleOperationKind : uint8_t {
    GUEST_INSTRUCTION = 0,
    INTERRUPT_ENTRY = 1,
};

struct ResumableInstruction {
    explicit ResumableInstruction(
            const CPUState& state,
            uint64_t start_cycle_value,
            CycleOperationKind kind_value =
                CycleOperationKind::GUEST_INSTRUCTION,
            int interrupt_vector_value = -1)
        : checkpoint(state),
          start_cycle(start_cycle_value),
          kind(kind_value),
          interrupt_vector(interrupt_vector_value) {}

    CPUExecutionCheckpoint checkpoint;
    uint64_t start_cycle = 0;
    CycleOperationKind kind =
        CycleOperationKind::GUEST_INSTRUCTION;
    int interrupt_vector = -1;
    std::vector<BusReplayRecord> completed_accesses;
    std::optional<BusRequest> pending_request;
    std::optional<uint64_t> retire_cycle;
    bool tacc_python_fallback = false;
    bool tacc_busy_published = false;
    bool tacc_validation_trap_expected = false;
    uint64_t tacc_operation_epoch = 0;
    std::size_t replay_cursor = 0;
};

struct FullCoreCycleState {
    uint64_t ready_cycle = 0;
    uint64_t next_issue_sequence = 1;
    std::unique_ptr<ResumableInstruction> instruction;
};

struct DmaCycleState {
    int requester_id = 0;
    uint64_t next_issue_sequence = 1;
    uint64_t highest_observed_token = 0;
    bool timeline_active = false;
    std::optional<uint64_t> pending_token;
    std::optional<BusRequest> pending_request;
};

struct TaccImageTransferStage {
    static constexpr int NO_OWNER = -1;

    enum class Direction : uint8_t {
        NONE = 0,
        LOAD = 1,
        STORE = 2,
    };

    Direction direction = Direction::NONE;
    int owner_engine_id = NO_OWNER;
    int owner_core_id = NO_OWNER;
    uint64_t engine_epoch = 0;
    uint64_t caller_epoch = 0;
    uint64_t stage_epoch = 0;
    uint64_t base_address = 0;
    uint8_t format_ew = 0;
    bool format_signed = false;
    uint8_t beat_index = 0;
    std::array<uint8_t, TACC_IMAGE_BYTES> image{};
    int last_grant_engine_id = NO_OWNER;
    uint64_t grant_sequence = 0;

    bool active() const noexcept {
        return direction != Direction::NONE;
    }

    void clear_active(bool bump_epoch) noexcept {
        direction = Direction::NONE;
        owner_engine_id = NO_OWNER;
        owner_core_id = NO_OWNER;
        engine_epoch = 0;
        caller_epoch = 0;
        base_address = 0;
        format_ew = 0;
        format_signed = false;
        beat_index = 0;
        image.fill(0);
        if (
            bump_epoch &&
            stage_epoch !=
                std::numeric_limits<uint64_t>::max()
        ) {
            stage_epoch++;
        }
    }

    void cancel(bool reset_round_robin = false) noexcept {
        clear_active(true);
        if (reset_round_robin) {
            last_grant_engine_id = NO_OWNER;
            grant_sequence = 0;
        }
    }

    bool owned_by(
            int engine_id,
            int core_id,
            uint64_t token) const noexcept {
        return (
            active() &&
            owner_engine_id == engine_id &&
            owner_core_id == core_id &&
            stage_epoch == token
        );
    }
};

struct BusYieldSignal {};

class ResumableBusAccess {
public:
    virtual ~ResumableBusAccess() = default;

    virtual uint64_t access(
        BusOperation operation,
        uint64_t address,
        BusWidth width,
        uint64_t write_data,
        bool port_io
    ) = 0;
};

static std::unique_ptr<CPUState> make_cpu_state(
        CoreProfile profile = CoreProfile::FULL,
        MemoryMappings* shared_memory = nullptr,
        TimerDevice* shared_timer = nullptr,
        UartGeomDevice* shared_uart_geom = nullptr,
        FramebufferDevice* shared_fb = nullptr,
        RTCDevice* shared_rtc = nullptr,
        InterruptRouter* shared_interrupts = nullptr,
        CryptoDevices* shared_crypto = nullptr,
        NICDevice* shared_nic = nullptr,
        TRNGDevice* shared_trng = nullptr,
        UARTDevice* shared_uart = nullptr,
        std::atomic<bool>* shared_batch_active = nullptr,
        std::atomic<bool>* shared_cycle_execution_pending = nullptr) {
    auto state = std::make_unique<CPUState>();
    state->profile = profile;
    if (shared_memory != nullptr) {
        state->memory = shared_memory;
    } else {
        state->private_memory = std::make_unique<MemoryMappings>();
        state->memory = state->private_memory.get();
    }
    if (shared_timer != nullptr) {
        state->timer = shared_timer;
    } else {
        state->private_timer = std::make_unique<TimerDevice>();
        state->timer = state->private_timer.get();
    }
    if (shared_uart_geom != nullptr) {
        state->uart_geom = shared_uart_geom;
    } else {
        state->private_uart_geom = std::make_unique<UartGeomDevice>();
        state->uart_geom = state->private_uart_geom.get();
    }
    if (shared_fb != nullptr) {
        state->fb = shared_fb;
    } else {
        state->private_fb = std::make_unique<FramebufferDevice>();
        state->fb = state->private_fb.get();
    }
    if (shared_rtc != nullptr) {
        state->rtc = shared_rtc;
    } else {
        state->private_rtc = std::make_unique<RTCDevice>();
        state->rtc = state->private_rtc.get();
    }
    if (shared_crypto != nullptr) {
        state->crypto = shared_crypto;
    } else {
        state->private_crypto = std::make_unique<CryptoDevices>();
        state->crypto = state->private_crypto.get();
    }
    if (shared_nic != nullptr) {
        state->nic = shared_nic;
    } else {
        state->private_nic = std::make_unique<NICDevice>();
        state->nic = state->private_nic.get();
    }
    if (shared_trng != nullptr) {
        state->trng = shared_trng;
    } else {
        state->private_trng = std::make_unique<TRNGDevice>();
        state->trng = state->private_trng.get();
    }
    if (shared_uart != nullptr) {
        state->uart = shared_uart;
    } else {
        state->private_uart = std::make_unique<UARTDevice>();
        state->uart = state->private_uart.get();
    }
    state->interrupts = shared_interrupts;
    state->system_batch_active = shared_batch_active;
    state->system_cycle_execution_pending =
        shared_cycle_execution_pending;
    for (int index = 0; index < 8; index++)
        state->port_map[index] = 0xFFFF;
    state->dict_clear_all();
    state->icache_enabled =
        profile == CoreProfile::FULL ? 1 : 0;
    state->icache_valid.fill(0);
    state->icache_hits = 0;
    state->icache_misses = 0;
    state->crc_acc = 0xFFFFFFFF;
    state->crc_mode = 0;
    state->gf_prime_sel = 0;
    state->gf_custom_p = BigNum();
    state->gf_mont_pinv = BigNum();
    state->gf_prev_lo = BigNum();
    state->gf_prev_hi = BigNum();
    return state;
}

enum class ClusterResourceKind : uint8_t {
    NONE = 0,
    BUS = 1,
    MUL_DIV = 2,
    CRC = 3,
    TILE_ENGINE = 4,
};

static constexpr std::size_t CLUSTER_RESOURCE_KIND_COUNT = 5;

static const char* cluster_resource_name(
        ClusterResourceKind kind) {
    switch (kind) {
        case ClusterResourceKind::BUS:
            return "bus";
        case ClusterResourceKind::MUL_DIV:
            return "mul_div";
        case ClusterResourceKind::CRC:
            return "crc";
        case ClusterResourceKind::TILE_ENGINE:
            return "tile_engine";
        case ClusterResourceKind::NONE:
            return "none";
    }
    throw std::logic_error("invalid cluster resource kind");
}

struct ClusterState {
    int cluster_id = 0;
    int global_id_base = 0;
    int core_count = 0;
    std::array<int, CLUSTER_RESOURCE_KIND_COUNT> last_grants{};
    std::array<uint64_t, CLUSTER_RESOURCE_KIND_COUNT> grant_counts{};
    bool crc_locked = false;
    int crc_lock_owner = -1;
    bool sha_locked = false;
    int sha_lock_owner = -1;
    uint64_t grant_sequence = 0;
    uint64_t crc_acc = 0xFFFF'FFFFULL;
    int crc_mode = 0;
    std::array<uint8_t, 1024> scratchpad{};

    // Authoritative physical tile-engine state shared by the cluster.
    // Configuration, source/destination selectors, and addressing cursors
    // remain caller-private in each microcore CPUState and are sampled only
    // when that caller wins admission.
    std::array<uint64_t, 4> acc{};
    std::array<uint8_t, TACC_IMAGE_BYTES> tacc{};
    uint8_t tacc_owner = TACC_OWNER_NONE;
    bool tacc_valid = false;
    bool tacc_dirty = false;
    uint8_t tacc_format_ew = 0;
    uint8_t tacc_format_signed = 0;
    bool tacc_busy = false;
    bool tacc_force_pending = false;
    uint64_t tacc_epoch = 0;
    std::array<uint64_t, 4> tacc_caller_epochs{};

    // SHA transaction working state shares the engine ACC. The granted
    // caller's private TSRC0 remains outside this structure.
    int sha_mode = 0;
    uint64_t sha_msglen_lo = 0;
    uint64_t sha_msglen_hi = 0;

    ClusterState() = default;

    ClusterState(
            int cluster_id_value,
            int global_id_base_value,
            int core_count_value)
        : cluster_id(cluster_id_value),
          global_id_base(global_id_base_value),
          core_count(core_count_value) {
        if (core_count < 1 || core_count > 4) {
            throw std::invalid_argument(
                "native cluster must contain between one and four cores");
        }
        reset();
        tacc_caller_epochs.fill(0);
    }

    void reset_arbitration() {
        last_grants.fill(0);
        grant_counts.fill(0);
        crc_locked = false;
        crc_lock_owner = -1;
        sha_locked = false;
        sha_lock_owner = -1;
        grant_sequence = 0;
    }

    void reset_shared_engines() {
        crc_acc = 0xFFFF'FFFFULL;
        crc_mode = 0;
        acc.fill(0);
        tacc.fill(0);
        tacc_owner = TACC_OWNER_NONE;
        tacc_valid = false;
        tacc_dirty = false;
        tacc_format_ew = 0;
        tacc_format_signed = 0;
        tacc_busy = false;
        tacc_force_pending = false;
        tacc_epoch++;
        for (int index = 0; index < core_count; index++)
            tacc_caller_epochs[
                static_cast<std::size_t>(index)]++;
        sha_mode = 0;
        sha_msglen_lo = 0;
        sha_msglen_hi = 0;
    }

    void reset() {
        reset_arbitration();
        reset_shared_engines();
    }

    bool local_core_is_eligible(
            ClusterResourceKind kind,
            int local_core,
            bool sha_lock_protected) const {
        if (local_core < 0 || local_core >= core_count)
            return false;
        if (kind == ClusterResourceKind::CRC && crc_locked)
            return local_core == crc_lock_owner;
        if (
            kind == ClusterResourceKind::TILE_ENGINE &&
            sha_lock_protected &&
            sha_locked
        ) {
            return local_core == sha_lock_owner;
        }
        return true;
    }

    std::optional<int> choose(
            ClusterResourceKind kind,
            const std::vector<int>& candidates,
            const std::vector<bool>& sha_lock_protected) const {
        const std::size_t resource_index =
            static_cast<std::size_t>(kind);
        if (
            kind == ClusterResourceKind::NONE ||
            resource_index >= last_grants.size()
        ) {
            throw std::invalid_argument(
                "cluster arbitration requires a shared resource");
        }
        if (candidates.size() != sha_lock_protected.size()) {
            throw std::invalid_argument(
                "cluster arbitration candidate metadata is incomplete");
        }

        std::array<bool, 4> pending{};
        std::array<bool, 4> pending_sha_lock_protected{};
        for (std::size_t index = 0; index < candidates.size(); index++) {
            const int local_core = candidates[index];
            if (local_core < 0 || local_core >= core_count) {
                throw std::out_of_range(
                    "cluster request has an invalid local core");
            }
            const std::size_t local_index =
                static_cast<std::size_t>(local_core);
            if (
                pending[local_index] &&
                pending_sha_lock_protected[local_index] !=
                    sha_lock_protected[index]
            ) {
                throw std::invalid_argument(
                    "one core submitted conflicting cluster requests");
            }
            pending[local_index] = true;
            pending_sha_lock_protected[local_index] =
                sha_lock_protected[index];
        }

        const int last = last_grants[resource_index];
        for (int offset = 1; offset <= core_count; offset++) {
            int candidate = last + offset;
            if (candidate >= core_count)
                candidate -= core_count;
            if (
                pending[static_cast<std::size_t>(candidate)] &&
                local_core_is_eligible(
                    kind,
                    candidate,
                    pending_sha_lock_protected[
                        static_cast<std::size_t>(candidate)])
            ) {
                return candidate;
            }
        }
        return std::nullopt;
    }

    void commit(
            ClusterResourceKind kind,
            int local_core,
            int operation,
            bool sha_transaction) {
        const std::size_t resource_index =
            static_cast<std::size_t>(kind);
        if (
            kind == ClusterResourceKind::NONE ||
            resource_index >= last_grants.size() ||
            local_core < 0 ||
            local_core >= core_count
        ) {
            throw std::invalid_argument(
                "invalid native cluster grant");
        }
        if (
            grant_counts[resource_index] ==
                std::numeric_limits<uint64_t>::max() ||
            grant_sequence ==
                std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "native cluster grant accounting overflow");
        }
        last_grants[resource_index] = local_core;
        grant_counts[resource_index]++;
        grant_sequence++;

        if (kind == ClusterResourceKind::CRC) {
            if (
                operation == 0x0 ||
                operation == 0x4 ||
                operation == 0x5
            ) {
                crc_locked = true;
                crc_lock_owner = local_core;
            } else if (operation == 0x3) {
                crc_locked = false;
                crc_lock_owner = -1;
            }
        } else if (
            kind == ClusterResourceKind::TILE_ENGINE &&
            sha_transaction
        ) {
            if (operation == 0x0) {
                sha_locked = true;
                sha_lock_owner = local_core;
            } else if (operation == 0x6) {
                sha_locked = false;
                sha_lock_owner = -1;
            }
        }
    }
};

struct PersistentWorkerPoolSnapshot {
    int worker_count = 1;
    int auxiliary_worker_count = 0;
    int live_auxiliary_workers = 0;
    uint64_t launch_count = 0;
    bool inline_reference = true;
};

enum class PrivateCoreStopReason : uint8_t {
    INSTRUCTION_LIMIT = 0,
    ICACHE_BOUNDARY = 1,
    SHARED_INSTRUCTION = 2,
    INTERRUPT_BOUNDARY = 3,
    HALTED = 4,
    IDLE = 5,
    TRAP = 6,
    RESET = 7,
    INTERNAL_FAILURE = 8,
};

static constexpr std::size_t PRIVATE_CORE_STOP_REASON_COUNT = 9;
static constexpr std::size_t PRIVATE_WORKER_MAX_LANES = 4;
static constexpr std::size_t HOST_PROFILE_MAX_LANES =
    PRIVATE_WORKER_MAX_LANES;

static void host_saturating_add(
        uint64_t& destination,
        uint64_t increment) noexcept {
    if (
        increment >
        std::numeric_limits<uint64_t>::max() -
            destination
    ) {
        destination =
            std::numeric_limits<uint64_t>::max();
        return;
    }
    destination += increment;
}

static void host_saturating_increment(
        uint64_t& destination) noexcept {
    host_saturating_add(destination, 1);
}

static uint64_t host_elapsed_ns(
        std::chrono::steady_clock::time_point start)
        noexcept {
    const auto elapsed =
        std::chrono::duration_cast<
            std::chrono::nanoseconds>(
                std::chrono::steady_clock::now() -
                start)
            .count();
    if (elapsed <= 0)
        return 0;
    const auto unsigned_elapsed =
        static_cast<unsigned long long>(elapsed);
    return static_cast<uint64_t>(
        std::min<unsigned long long>(
            unsigned_elapsed,
            std::numeric_limits<uint64_t>::max()));
}

class HostProfileWallTimer {
public:
    HostProfileWallTimer(
            bool enabled,
            uint64_t* destination) noexcept
        : destination_(
              enabled ? destination : nullptr) {
        if (destination_ != nullptr)
            start_ = std::chrono::steady_clock::now();
    }

    HostProfileWallTimer(
        const HostProfileWallTimer&) = delete;
    HostProfileWallTimer& operator=(
        const HostProfileWallTimer&) = delete;

    ~HostProfileWallTimer() {
        if (destination_ != nullptr) {
            host_saturating_add(
                *destination_,
                host_elapsed_ns(start_));
        }
    }

private:
    uint64_t* destination_ = nullptr;
    std::chrono::steady_clock::time_point start_{};
};

struct WorkerWaveHostTiming {
    uint64_t prepare_ns = 0;
    uint64_t wait_ns = 0;
    uint64_t gather_ns = 0;
};

struct ConcurrencyProfileCounters {
    bool enabled = false;
    uint64_t generation = 0;

    uint64_t batches = 0;
    uint64_t prepare_batch_calls = 0;
    uint64_t scheduler_rounds = 0;
    uint64_t logical_subfrontiers = 0;
    uint64_t round_absorptions = 0;
    uint64_t worker_waves = 0;
    uint64_t worker_commands = 0;
    uint64_t frontier_routing_waves = 0;
    uint64_t frontier_routing_commands = 0;
    uint64_t frontier_preclassification_commands = 0;
    uint64_t frontier_preclassification_calls = 0;
    uint64_t worker_bypassed_commands = 0;
    uint64_t private_steps = 0;
    uint64_t private_classification_calls = 0;
    uint64_t private_decode_cache_lookups = 0;
    uint64_t private_decode_cache_hits = 0;
    uint64_t private_decode_cache_misses = 0;
    uint64_t micro_oracle_proof_reuses = 0;
    uint64_t frontier_decode_cache_lookups = 0;
    uint64_t frontier_decode_cache_hits = 0;
    uint64_t frontier_decode_cache_misses = 0;
    uint64_t zero_step_commands = 0;
    uint64_t checkpoint_captures = 0;
    uint64_t checkpoint_restores = 0;
    uint64_t coordinator_boundaries = 0;
    uint64_t settle_round_calls = 0;
    std::array<
        uint64_t,
        PRIVATE_CORE_STOP_REASON_COUNT>
        private_stop_reasons{};
    std::array<
        uint64_t,
        PRIVATE_CORE_STOP_REASON_COUNT>
        worker_bypass_stop_reasons{};
    std::array<
        uint64_t,
        PRIVATE_CORE_STOP_REASON_COUNT>
        coordinator_boundary_origins{};
    std::array<
        uint64_t,
        HOST_PROFILE_MAX_LANES>
        lane_commands{};
    std::array<
        uint64_t,
        HOST_PROFILE_MAX_LANES>
        lane_steps{};

    uint64_t batch_total_ns = 0;
    uint64_t prepare_batch_ns = 0;
    uint64_t scheduler_round_ns = 0;
    uint64_t logical_subfrontier_ns = 0;
    uint64_t round_absorption_ns = 0;
    uint64_t worker_wave_ns = 0;
    uint64_t worker_wave_prepare_ns = 0;
    uint64_t worker_wave_wait_ns = 0;
    uint64_t worker_wave_gather_ns = 0;
    uint64_t frontier_fast_path_ns = 0;
    uint64_t private_command_sum_ns = 0;
    uint64_t private_command_max_ns = 0;
    uint64_t private_scope_setup_ns = 0;
    uint64_t checkpoint_capture_ns = 0;
    uint64_t checkpoint_restore_ns = 0;
    uint64_t coordinator_boundary_ns = 0;
    uint64_t settle_round_ns = 0;
    std::array<
        uint64_t,
        PRIVATE_CORE_STOP_REASON_COUNT>
        coordinator_boundary_origin_ns{};
    std::array<
        uint64_t,
        HOST_PROFILE_MAX_LANES>
        lane_active_ns{};

    void start_session() noexcept {
        const uint64_t next_generation =
            generation ==
                std::numeric_limits<uint64_t>::max()
            ? generation
            : generation + 1;
        *this = ConcurrencyProfileCounters{};
        generation = next_generation;
        enabled = true;
    }
};

class SharedMemoryExecutionAdmission;

struct PrivateCoreHostTelemetry {
    uint64_t execution_ns = 0;
    uint64_t scope_setup_ns = 0;
    uint64_t checkpoint_capture_ns = 0;
    uint64_t checkpoint_restore_ns = 0;
    uint64_t classification_calls = 0;
    uint64_t decode_cache_lookups = 0;
    uint64_t decode_cache_hits = 0;
    uint64_t decode_cache_misses = 0;
    uint64_t micro_oracle_proof_reuses = 0;
    uint64_t checkpoint_captures = 0;
    uint64_t checkpoint_restores = 0;
};

struct PrivateCoreCommand {
    uint64_t command_sequence = 0;
    uint64_t wave_epoch = 0;
    std::size_t submission_index = 0;
    int lane_index = 0;
    int core_index = 0;
    int max_steps = 0;
    int pending_interrupt_vector = -1;
    // Strict cycle execution may submit only one preclassified, exactly
    // one-cycle private instruction. This internal ownership bit permits that
    // command to coexist with the strict scheduler's suspended timeline while
    // preserving the public/private-wave rejection contract.
    bool strict_cycle_one_instruction = false;
    // The unbounded scheduler may classify the first instruction under the
    // same frontier admission before posting the command. No core or mapping
    // mutation can intervene, so the worker can consume that proof once
    // instead of repeating the read-only classification.
    bool first_instruction_preclassified_private = false;
    CPUState* core = nullptr;
    std::shared_ptr<SharedMemoryExecutionAdmission> admission;
    // Non-owning per-command sidecar. It is non-null only during an opt-in
    // unbounded profile wave and outlives every mailbox copy of this command.
    PrivateCoreHostTelemetry* host_telemetry = nullptr;
};

struct PrivateCoreResult {
    uint64_t command_sequence = 0;
    uint64_t wave_epoch = 0;
    std::size_t submission_index = 0;
    int lane_index = 0;
    int core_index = 0;
    uint64_t thread_token = 0;
    uint64_t start_pc = 0;
    uint64_t end_pc = 0;
    int64_t steps_executed = 0;
    int64_t total_cycles = 0;
    PrivateCoreStopReason stop_reason =
        PrivateCoreStopReason::INSTRUCTION_LIMIT;
    int trap_id = -1;
    int interrupt_vector = -1;
    std::string internal_error;
};

struct PersistentWorkerLaneSnapshot {
    int lane_index = 0;
    bool auxiliary = false;
    uint64_t thread_token = 0;
    uint64_t completed_commands = 0;
    uint64_t completed_steps = 0;
};

struct PersistentWorkerPrivateSnapshot {
    uint64_t wave_epoch = 0;
    uint64_t next_command_sequence = 1;
    bool wave_active = false;
    std::vector<PersistentWorkerLaneSnapshot> lanes;
};

static PrivateCoreResult execute_private_core_command(
    const PrivateCoreCommand& command) noexcept;

static std::atomic<uint64_t> next_private_thread_token{1};

static uint64_t current_private_thread_token() {
    static thread_local const uint64_t token =
        next_private_thread_token.fetch_add(
            1, std::memory_order_relaxed);
    return token;
}

// Lane zero remains the coordinator/caller thread, preserving the exact
// one-lane reference path. Configurations with two or four lanes own N-1
// persistent helpers. Each helper has one typed mailbox; helpers can execute
// only callback-free full-core private commands and never select or commit a
// guest-visible shared effect.
class PersistentWorkerPool {
public:
    explicit PersistentWorkerPool(int worker_count)
        : worker_count_(validated_worker_count(worker_count)),
          auxiliary_worker_count_(worker_count_ - 1) {
        lane_thread_tokens_.assign(
            static_cast<std::size_t>(worker_count_),
            0);
        lane_completed_commands_.assign(
            static_cast<std::size_t>(worker_count_),
            0);
        lane_completed_steps_.assign(
            static_cast<std::size_t>(worker_count_),
            0);
        if (auxiliary_worker_count_ == 0)
            return;

        try {
            helper_slots_.resize(
                static_cast<std::size_t>(
                    auxiliary_worker_count_));
            workers_.reserve(
                static_cast<std::size_t>(auxiliary_worker_count_));
            for (
                int worker_index = 0;
                worker_index < auxiliary_worker_count_;
                worker_index++
            ) {
                workers_.emplace_back(
                    [this, worker_index]() {
                        worker_main(worker_index);
                    });
                launch_count_++;
            }

            std::unique_lock<std::mutex> lock(mutex_);
            state_changed_.wait(
                lock,
                [this]() {
                    return live_auxiliary_workers_ ==
                        auxiliary_worker_count_;
                });
        } catch (...) {
            shutdown_and_join();
            throw;
        }

        // Lane-zero diagnostics are allocated lazily on its first command so
        // constructing a helper-bearing SystemState does not claim that the
        // creating thread has executed guest work.
    }

    ~PersistentWorkerPool() noexcept {
        shutdown_and_join();
    }

    PersistentWorkerPool(const PersistentWorkerPool&) = delete;
    PersistentWorkerPool& operator=(
        const PersistentWorkerPool&) = delete;
    PersistentWorkerPool(PersistentWorkerPool&&) = delete;
    PersistentWorkerPool& operator=(
        PersistentWorkerPool&&) = delete;

    static void validate_worker_count(int worker_count) {
        if (
            worker_count != 1 &&
            worker_count != 2 &&
            worker_count != 4
        ) {
            throw std::invalid_argument(
                "worker_count must be exactly 1, 2, or 4");
        }
    }

    PersistentWorkerPoolSnapshot snapshot() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return PersistentWorkerPoolSnapshot{
            worker_count_,
            auxiliary_worker_count_,
            live_auxiliary_workers_,
            launch_count_,
            worker_count_ == 1,
        };
    }

    PersistentWorkerPrivateSnapshot private_snapshot() const {
        std::lock_guard<std::mutex> lock(mutex_);
        PersistentWorkerPrivateSnapshot result;
        result.wave_epoch = wave_epoch_;
        result.next_command_sequence = next_command_sequence_;
        result.wave_active = wave_active_;
        result.lanes.reserve(
            static_cast<std::size_t>(worker_count_));
        for (int lane_index = 0;
             lane_index < worker_count_;
             lane_index++) {
            result.lanes.push_back(
                PersistentWorkerLaneSnapshot{
                    lane_index,
                    lane_index != 0,
                    lane_thread_tokens_[
                        static_cast<std::size_t>(lane_index)],
                    lane_completed_commands_[
                        static_cast<std::size_t>(lane_index)],
                    lane_completed_steps_[
                        static_cast<std::size_t>(lane_index)],
                });
        }
        return result;
    }

    void validate_private_capacity(
            uint64_t maximum_additional_waves,
            uint64_t maximum_additional_commands,
            uint64_t maximum_additional_steps) const {
        std::lock_guard<std::mutex> lock(mutex_);
        if (
            maximum_additional_waves >
                std::numeric_limits<uint64_t>::max() -
                    wave_epoch_
        ) {
            throw std::overflow_error(
                "native private worker wave epoch overflow");
        }
        if (
            maximum_additional_commands >
                std::numeric_limits<uint64_t>::max() -
                    next_command_sequence_
        ) {
            throw std::overflow_error(
                "native private command sequence overflow");
        }
        for (
            std::size_t lane = 0;
            lane < lane_completed_commands_.size();
            lane++
        ) {
            if (
                maximum_additional_commands >
                    std::numeric_limits<uint64_t>::max() -
                        lane_completed_commands_[lane]
            ) {
                throw std::overflow_error(
                    "native private completed-command "
                    "accounting overflow");
            }
            if (
                maximum_additional_steps >
                    std::numeric_limits<uint64_t>::max() -
                        lane_completed_steps_[lane]
            ) {
                throw std::overflow_error(
                    "native private completed-step "
                    "accounting overflow");
            }
        }
    }

    std::vector<PrivateCoreResult> execute_wave(
            std::vector<PrivateCoreCommand> commands,
            WorkerWaveHostTiming* host_timing = nullptr) {
        if (commands.empty())
            return {};
        const bool host_profile_enabled =
            host_timing != nullptr;
        const auto prepare_started =
            host_profile_enabled
            ? std::chrono::steady_clock::now()
            : std::chrono::steady_clock::time_point{};
        if (commands.size() >
            static_cast<std::size_t>(worker_count_)) {
            throw std::invalid_argument(
                "a private wave cannot contain more commands than lanes");
        }

        std::vector<bool> seen_lanes(
            static_cast<std::size_t>(worker_count_),
            false);
        std::vector<CPUState*> seen_cores;
        seen_cores.reserve(commands.size());
        for (const PrivateCoreCommand& command : commands) {
            if (
                command.lane_index < 0 ||
                command.lane_index >= worker_count_
            ) {
                throw std::out_of_range(
                    "private command lane index is out of range");
            }
            if (seen_lanes[
                    static_cast<std::size_t>(
                        command.lane_index)]) {
                throw std::invalid_argument(
                    "a private wave cannot submit two commands "
                    "to one lane");
            }
            if (command.core == nullptr || !command.admission)
                throw std::invalid_argument(
                    "private command ownership is incomplete");
            if (command.max_steps < 0)
                throw std::invalid_argument(
                    "private command step budget cannot be negative");
            if (
                std::find(
                    seen_cores.begin(),
                    seen_cores.end(),
                    command.core) != seen_cores.end()
            ) {
                throw std::invalid_argument(
                    "a private wave cannot execute one core twice");
            }
            seen_lanes[
                static_cast<std::size_t>(
                    command.lane_index)] = true;
            seen_cores.push_back(command.core);
        }

        std::vector<PrivateCoreResult> results(
            commands.size());
        std::optional<PrivateCoreCommand> inline_command;
        std::unique_lock<std::mutex> lock(mutex_);
        if (stopping_)
            throw std::runtime_error(
                "native worker pool is stopping");
        if (wave_active_)
            throw std::runtime_error(
                "native worker pool already has an active wave");
        if (wave_epoch_ == std::numeric_limits<uint64_t>::max())
            throw std::overflow_error(
                "native worker wave epoch overflow");
        for (const PrivateCoreCommand& command : commands) {
            const std::size_t lane =
                static_cast<std::size_t>(
                    command.lane_index);
            if (
                lane_completed_commands_[lane] ==
                    std::numeric_limits<uint64_t>::max()
            ) {
                throw std::overflow_error(
                    "native private completed-command "
                    "accounting overflow");
            }
            const uint64_t maximum_steps =
                static_cast<uint64_t>(
                    command.max_steps);
            if (
                maximum_steps >
                    std::numeric_limits<uint64_t>::max() -
                        lane_completed_steps_[lane]
            ) {
                throw std::overflow_error(
                    "native private completed-step "
                    "accounting overflow");
            }
        }

        wave_epoch_++;
        for (std::size_t index = 0;
             index < commands.size();
             index++) {
            PrivateCoreCommand& command = commands[index];
            if (
                next_command_sequence_ ==
                std::numeric_limits<uint64_t>::max()
            ) {
                throw std::overflow_error(
                    "native private command sequence overflow");
            }
            command.command_sequence =
                next_command_sequence_++;
            command.wave_epoch = wave_epoch_;
            command.submission_index = index;
        }

        if (remaining_helper_commands_ != 0) {
            throw std::logic_error(
                "native helper completion accounting is not idle");
        }
        for (const PrivateCoreCommand& command : commands) {
            if (command.lane_index == 0) {
                inline_command = command;
                continue;
            }
            HelperSlot& slot = helper_slots_[
                static_cast<std::size_t>(
                    command.lane_index - 1)];
            if (slot.state != HelperSlotState::IDLE)
                throw std::logic_error(
                    "native helper mailbox is not idle");
            slot.epoch = wave_epoch_;
            slot.command = command;
            slot.result.reset();
            slot.state = HelperSlotState::POSTED;
            remaining_helper_commands_++;
        }
        wave_active_ = true;
        lock.unlock();
        work_ready_.notify_all();
        if (host_profile_enabled) {
            host_timing->prepare_ns =
                host_elapsed_ns(prepare_started);
        }

        std::optional<PrivateCoreResult> inline_result;
        if (inline_command.has_value()) {
            inline_result =
                execute_private_core_command(
                    *inline_command);
        }

        const auto wait_started =
            host_profile_enabled
            ? std::chrono::steady_clock::now()
            : std::chrono::steady_clock::time_point{};
        lock.lock();
        completion_ready_.wait(
            lock,
            [this, &commands]() {
                for (const PrivateCoreCommand& command :
                     commands) {
                    if (command.lane_index == 0)
                        continue;
                    const HelperSlot& slot = helper_slots_[
                        static_cast<std::size_t>(
                            command.lane_index - 1)];
                    if (
                        slot.state !=
                            HelperSlotState::COMPLETED ||
                        slot.epoch != wave_epoch_
                    ) {
                        return false;
                    }
                }
                return true;
            });
        if (host_profile_enabled) {
            host_timing->wait_ns =
                host_elapsed_ns(wait_started);
        }

        const auto gather_started =
            host_profile_enabled
            ? std::chrono::steady_clock::now()
            : std::chrono::steady_clock::time_point{};
        for (const PrivateCoreCommand& command : commands) {
            PrivateCoreResult result;
            if (command.lane_index == 0) {
                if (!inline_result.has_value())
                    throw std::logic_error(
                        "inline private command result is missing");
                result = std::move(*inline_result);
            } else {
                HelperSlot& slot = helper_slots_[
                    static_cast<std::size_t>(
                        command.lane_index - 1)];
                if (!slot.result.has_value())
                    throw std::logic_error(
                        "helper private command result is missing");
                result = std::move(*slot.result);
                slot.result.reset();
                slot.command.reset();
                slot.state = HelperSlotState::IDLE;
            }
            const std::size_t lane =
                static_cast<std::size_t>(
                    command.lane_index);
            lane_thread_tokens_[lane] =
                result.thread_token;
            lane_completed_commands_[lane]++;
            lane_completed_steps_[lane] +=
                static_cast<uint64_t>(
                    std::max<int64_t>(
                        result.steps_executed, 0));
            results[result.submission_index] =
                std::move(result);
        }
        wave_active_ = false;
        lock.unlock();
        if (host_profile_enabled) {
            host_timing->gather_ns =
                host_elapsed_ns(gather_started);
        }
        return results;
    }

private:
    enum class HelperSlotState : uint8_t {
        IDLE = 0,
        POSTED = 1,
        RUNNING = 2,
        COMPLETED = 3,
    };

    struct HelperSlot {
        HelperSlotState state = HelperSlotState::IDLE;
        uint64_t epoch = 0;
        std::optional<PrivateCoreCommand> command;
        std::optional<PrivateCoreResult> result;
    };

    static int validated_worker_count(int worker_count) {
        validate_worker_count(worker_count);
        return worker_count;
    }

    void worker_main(int worker_index) noexcept {
        std::unique_lock<std::mutex> lock(mutex_);
        const int lane_index = worker_index + 1;
        lane_thread_tokens_[
            static_cast<std::size_t>(lane_index)] =
            current_private_thread_token();
        live_auxiliary_workers_++;
        state_changed_.notify_all();

        HelperSlot& slot = helper_slots_[
            static_cast<std::size_t>(worker_index)];
        while (true) {
            work_ready_.wait(
                lock,
                [this, &slot]() {
                    return stopping_ ||
                        slot.state ==
                            HelperSlotState::POSTED;
                });
            if (
                stopping_ &&
                slot.state != HelperSlotState::POSTED
            ) {
                break;
            }

            PrivateCoreCommand command =
                *slot.command;
            slot.state = HelperSlotState::RUNNING;
            lock.unlock();
            PrivateCoreResult result =
                execute_private_core_command(command);
            lock.lock();

            slot.result = std::move(result);
            slot.state = HelperSlotState::COMPLETED;
            assert(remaining_helper_commands_ > 0);
            remaining_helper_commands_--;
            if (remaining_helper_commands_ == 0)
                completion_ready_.notify_one();
            // Return directly to the outer POSTED wait. The coordinator may
            // move COMPLETED -> IDLE -> POSTED before this helper next runs;
            // waiting specifically to observe the transient IDLE state would
            // lose that repost and deadlock the following wave.
        }

        live_auxiliary_workers_--;
        state_changed_.notify_all();
    }

    void shutdown_and_join() noexcept {
        {
            std::lock_guard<std::mutex> lock(mutex_);
            stopping_ = true;
        }
        work_ready_.notify_all();
        completion_ready_.notify_all();
        for (std::thread& worker : workers_) {
            if (worker.joinable())
                worker.join();
        }
        workers_.clear();
    }

    const int worker_count_;
    const int auxiliary_worker_count_;
    mutable std::mutex mutex_;
    std::condition_variable work_ready_;
    std::condition_variable completion_ready_;
    std::condition_variable state_changed_;
    std::vector<std::thread> workers_;
    std::vector<HelperSlot> helper_slots_;
    std::vector<uint64_t> lane_thread_tokens_;
    std::vector<uint64_t> lane_completed_commands_;
    std::vector<uint64_t> lane_completed_steps_;
    int live_auxiliary_workers_ = 0;
    uint64_t launch_count_ = 0;
    uint64_t wave_epoch_ = 0;
    uint64_t next_command_sequence_ = 1;
    int remaining_helper_commands_ = 0;
    bool wave_active_ = false;
    bool stopping_ = false;
};

// SystemState owns full-core lifetimes, exactly one mapping set, and every
// native SoC-singleton device reached directly by a full core. Python-bus
// devices and scheduling retain their compatibility paths for later
// transactional milestones. Shared resources are declared before cores so
// borrowed pointers die before their owners.
struct SystemState {
    static constexpr int NIC_DMA_REQUESTER_ID = -1;
    static constexpr int DISK_DMA_REQUESTER_ID = -2;
    static constexpr int MICRO_CORES_PER_CLUSTER = 4;

    explicit SystemState(
            int full_core_count,
            int all_core_count = 0,
            int main_bus_port_count = 0,
            int worker_count = 1) {
        PersistentWorkerPool::validate_worker_count(worker_count);
        if (full_core_count < 1 || full_core_count > 255)
            throw std::invalid_argument(
                "full_core_count must be between 1 and 255");
        if (all_core_count == 0)
            all_core_count = full_core_count;
        if (all_core_count < full_core_count || all_core_count > 255)
            throw std::invalid_argument(
                "all_core_count must include every full core and fit in 8 bits");

        const int micro_core_count = all_core_count - full_core_count;
        const int required_cluster_ports =
            (micro_core_count + MICRO_CORES_PER_CLUSTER - 1) /
            MICRO_CORES_PER_CLUSTER;
        const int minimum_main_bus_ports =
            full_core_count + required_cluster_ports + 2;
        if (main_bus_port_count == 0)
            main_bus_port_count = minimum_main_bus_ports;
        if (main_bus_port_count != minimum_main_bus_ports)
            throw std::invalid_argument(
                "main_bus_port_count must exactly match the system topology");

        main_bus.configure(main_bus_port_count);
        shared_interrupts.configure(all_core_count);
        shared_crypto.init();
        cluster_states.reserve(
            static_cast<std::size_t>(required_cluster_ports));
        for (
            int cluster_index = 0;
            cluster_index < required_cluster_ports;
            cluster_index++
        ) {
            const int cluster_core_base =
                cluster_index * MICRO_CORES_PER_CLUSTER;
            const int cluster_core_count = std::min(
                MICRO_CORES_PER_CLUSTER,
                micro_core_count - cluster_core_base);
            cluster_states.emplace_back(
                cluster_index,
                full_core_count + cluster_core_base,
                cluster_core_count);
        }
        cores.reserve(static_cast<std::size_t>(full_core_count));
        for (int index = 0; index < full_core_count; index++) {
            auto core = make_cpu_state(
                CoreProfile::FULL,
                &shared_memory,
                &shared_timer,
                &shared_uart_geom,
                &shared_fb,
                &shared_rtc,
                &shared_interrupts,
                &shared_crypto,
                &shared_nic,
                &shared_trng,
                &shared_uart,
                &native_batch_active,
                &cycle_execution_pending);
            core->core_id = static_cast<uint8_t>(index);
            core->num_cores = static_cast<uint8_t>(all_core_count);
            cores.push_back(std::move(core));
        }
        micro_cores.reserve(static_cast<std::size_t>(micro_core_count));
        for (int index = 0; index < micro_core_count; index++) {
            auto core = make_cpu_state(
                CoreProfile::MICRO,
                &shared_memory,
                &shared_timer,
                &shared_uart_geom,
                &shared_fb,
                &shared_rtc,
                &shared_interrupts,
                &shared_crypto,
                &shared_nic,
                &shared_trng,
                &shared_uart,
                &native_batch_active,
                &cycle_execution_pending);
            core->core_id = static_cast<uint8_t>(
                full_core_count + index);
            core->num_cores = static_cast<uint8_t>(all_core_count);
            micro_cores.push_back(std::move(core));
        }
        execution_cores.reserve(static_cast<std::size_t>(all_core_count));
        for (const auto& core : cores)
            execution_cores.push_back(core.get());
        for (const auto& core : micro_cores)
            execution_cores.push_back(core.get());
        full_core_cycle_states.resize(cores.size());
        dma_cycle_states[0].requester_id =
            NIC_DMA_REQUESTER_ID;
        dma_cycle_states[1].requester_id =
            DISK_DMA_REQUESTER_ID;
        advertised_core_count = all_core_count;
        configured_worker_count = worker_count;
        worker_pool =
            std::make_unique<PersistentWorkerPool>(worker_count);
    }

    ~SystemState() {
        // Helpers never touch Python and are joined before core, device,
        // mapping, buffer-exporter, or scheduler state begins to unwind.
        worker_pool.reset();
    }

    CPUState& core(int index) {
        if (index < 0 || index >= static_cast<int>(cores.size()))
            throw std::out_of_range("full-core index is out of range");
        mappings_sealed = true;
        return *cores[static_cast<std::size_t>(index)];
    }

    int full_core_count() const {
        return static_cast<int>(cores.size());
    }

    CPUState& micro_core(int index) {
        if (index < 0 ||
            index >= static_cast<int>(micro_cores.size())) {
            throw std::out_of_range(
                "micro-core index is out of range");
        }
        mappings_sealed = true;
        return *micro_cores[static_cast<std::size_t>(index)];
    }

    int micro_core_count() const {
        return static_cast<int>(micro_cores.size());
    }

    int all_core_count() const {
        return advertised_core_count;
    }

    int tacc_engine_for_core(int core_id) const {
        if (core_id < 0 || core_id >= advertised_core_count) {
            throw std::out_of_range(
                "TACC image-stage caller is outside the system topology");
        }
        if (core_id < full_core_count())
            return core_id;
        return (
            full_core_count() +
            (
                core_id - full_core_count()
            ) / MICRO_CORES_PER_CLUSTER
        );
    }

    void cancel_tacc_image_stage_for_core(int core_id) {
        (void)tacc_engine_for_core(core_id);
        if (tacc_image_stage.owner_core_id == core_id) {
            tacc_image_stage.cancel();
            refresh_cycle_execution_pending();
        }
    }

    void cancel_tacc_image_stage_for_cluster(
            int cluster_index) {
        if (
            cluster_index < 0 ||
            cluster_index >=
                static_cast<int>(cluster_states.size())
        ) {
            throw std::out_of_range(
                "cluster index is out of range");
        }
        ClusterState& cluster =
            cluster_states[
                static_cast<std::size_t>(
                    cluster_index)];
        const int owner =
            tacc_image_stage.owner_core_id;
        if (
            owner >= cluster.global_id_base &&
            owner <
                cluster.global_id_base +
                    cluster.core_count
        ) {
            tacc_image_stage.cancel();
            refresh_cycle_execution_pending();
        }
    }

    int worker_count() const {
        return configured_worker_count;
    }

    PersistentWorkerPoolSnapshot worker_pool_snapshot() const {
        if (!worker_pool)
            throw std::logic_error(
                "native worker pool is unavailable");
        return worker_pool->snapshot();
    }

    int main_bus_port_for_requester(int requester_id) const {
        const int port_count = main_bus.port_count();
        if (requester_id == NIC_DMA_REQUESTER_ID)
            return port_count - 2;
        if (requester_id == DISK_DMA_REQUESTER_ID)
            return port_count - 1;
        if (requester_id < 0 || requester_id >= advertised_core_count)
            throw std::out_of_range(
                "main bus requester is outside the advertised topology");
        if (requester_id < full_core_count())
            return requester_id;

        const int cluster_index =
            (requester_id - full_core_count()) /
            MICRO_CORES_PER_CLUSTER;
        const int cluster_port_count =
            port_count - full_core_count() - 2;
        if (cluster_index >= cluster_port_count)
            throw std::out_of_range(
                "micro-core requester has no main bus cluster port");
        return full_core_count() + cluster_index;
    }

    void validate_main_bus_request(const BusRequest& request) const {
        const int expected_port =
            main_bus_port_for_requester(request.requester_id);
        if (request.ordering.main_port_id != expected_port)
            throw std::invalid_argument(
                "main bus requester does not match its physical port");
        if (request.ordering.port_io &&
            (request.requester_id < 0 ||
             request.requester_id >= full_core_count())) {
            throw std::invalid_argument(
                "only a full core can issue main-bus port I/O");
        }
        if (request.requester_id < 0 &&
            request.width != BusWidth::BYTE) {
            throw std::invalid_argument(
                "main-bus DMA requesters are byte-wide");
        }
    }

    void reset_cycle_execution() {
        cycle_target_completion_cycle.reset();
        for (std::size_t index = 0;
             index < full_core_cycle_states.size();
             index++) {
            FullCoreCycleState& state =
                full_core_cycle_states[index];
            if (
                state.instruction &&
                state.instruction->tacc_busy_published
            ) {
                CPUState& core = *cores[index];
                if (core.tacc_force_pending) {
                    core.reset_tacc();
                } else {
                    core.tacc_busy = false;
                    core.tacc_epoch++;
                }
            }
            state.ready_cycle = shared_clock.cycles();
            state.next_issue_sequence = 1;
            state.instruction.reset();
        }
        for (DmaCycleState& state : dma_cycle_states) {
            state.next_issue_sequence = 1;
            state.highest_observed_token = 0;
            state.timeline_active = false;
            state.pending_token.reset();
            state.pending_request.reset();
        }
        cycle_execution_pending.store(
            false,
            std::memory_order_release);
        tacc_image_stage.cancel(true);
    }

    bool has_cycle_execution_pending() const {
        return cycle_execution_pending.load(
            std::memory_order_acquire);
    }

    void refresh_cycle_execution_pending() {
        const bool pending =
            tacc_image_stage.active() ||
            cycle_target_completion_cycle.has_value() ||
            std::any_of(
                full_core_cycle_states.begin(),
                full_core_cycle_states.end(),
                [](const FullCoreCycleState& state) {
                    return state.instruction != nullptr;
                }) ||
            std::any_of(
                dma_cycle_states.begin(),
                dma_cycle_states.end(),
                [](const DmaCycleState& state) {
                    return state.timeline_active ||
                           state.pending_request.has_value();
                });
        cycle_execution_pending.store(
            pending,
            std::memory_order_release);
    }

    MemoryMappings shared_memory;
    CryptoDevices shared_crypto{};
    NICDevice shared_nic{};
    TRNGDevice shared_trng{};
    UARTDevice shared_uart{};
    TimerDevice shared_timer{};
    UartGeomDevice shared_uart_geom{};
    FramebufferDevice shared_fb{};
    RTCDevice shared_rtc{};
    InterruptRouter shared_interrupts{};
    SystemClock shared_clock{};
    ExternalEventInbox external_events{};
    MainBusArbiter main_bus{};
    // Cluster-local arbitration state is declared before CPU ownership so
    // reduced CPU views are destroyed before the shared arbiters they use.
    std::vector<ClusterState> cluster_states;
    std::vector<std::unique_ptr<CPUState>> cores;
    std::vector<std::unique_ptr<CPUState>> micro_cores;
    // Stable non-owning topology order used by the unbounded native system
    // scheduler. The pointed-to CPUState objects are heap-owned by the two
    // vectors above, so vector growth cannot invalidate these addresses.
    std::vector<CPUState*> execution_cores;
    std::vector<FullCoreCycleState> full_core_cycle_states;
    std::array<DmaCycleState, 2> dma_cycle_states{};
    TaccImageTransferStage tacc_image_stage{};
    std::optional<uint64_t> cycle_target_completion_cycle;
    // Serializes native scheduling with clock/deadline mutation. Recursive
    // acquisition is required when a Python round-settlement callback advances
    // the clock on the same scheduler thread.
    mutable std::recursive_mutex scheduler_mutex;
    int scheduler_cursor = 0;
    uint64_t native_batch_runs = 0;
    uint64_t native_dispatches = 0;
    // Phase 4 host-only diagnostics. These counters are opt-in, never enter
    // architectural snapshots or scheduler decisions, and are mutated only
    // while the coordinator owns scheduler_mutex.
    ConcurrencyProfileCounters concurrency_profile{};
    bool concurrency_profile_batch_active = false;
    std::atomic<bool> native_batch_active{false};
    std::atomic<bool> cycle_execution_pending{false};
    int advertised_core_count = 0;
    bool mappings_sealed = false;
    int configured_worker_count = 1;
    // Declared last so ordinary reverse member destruction also stops helpers
    // before any state they will use in later Phase 3 elements.
    std::unique_ptr<PersistentWorkerPool> worker_pool;
};

class JournaledBusAccess final : public ResumableBusAccess {
public:
    JournaledBusAccess(SystemState& system, int core_index)
        : system_(system),
          core_index_(core_index) {}

    uint64_t access(
            BusOperation operation,
            uint64_t address,
            BusWidth width,
            uint64_t write_data,
            bool port_io) override {
        FullCoreCycleState& cycle_state =
            system_.full_core_cycle_states[
                static_cast<std::size_t>(core_index_)];
        if (!cycle_state.instruction)
            throw std::logic_error(
                "journaled bus access has no suspended instruction");
        ResumableInstruction& instruction =
            *cycle_state.instruction;

        if (instruction.replay_cursor <
            instruction.completed_accesses.size()) {
            BusReplayRecord& record =
                instruction.completed_accesses[
                    instruction.replay_cursor];
            const BusRequest& recorded =
                record.result.grant.request;
            validate_replay(
                recorded,
                operation,
                address,
                width,
                write_data,
                port_io);
            instruction.replay_cursor++;
            if (record.target_error_message.has_value()) {
                throw std::runtime_error(
                    "main bus target callback failed: " +
                    *record.target_error_message);
            }
            if (record.result.fault == BusFault::TARGET_FAULT)
                throw std::runtime_error("TRAP:BUS_FAULT");
            return record.result.read_value.value_or(0);
        }

        if (instruction.pending_request.has_value())
            throw std::logic_error(
                "a suspended instruction already has a pending request");
        if (cycle_state.next_issue_sequence ==
            std::numeric_limits<uint64_t>::max()) {
            throw std::overflow_error(
                "main bus issue sequence overflow");
        }

        uint64_t ready_cycle = instruction.start_cycle;
        if (!instruction.completed_accesses.empty()) {
            ready_cycle = instruction.completed_accesses.back()
                .result.completion_cycle;
        }

        const int requester_id =
            system_.cores[static_cast<std::size_t>(core_index_)]->core_id;
        instruction.pending_request = BusRequest{
            requester_id,
            ready_cycle,
            operation,
            address,
            width,
            write_data,
            BusOrderingMetadata{
                system_.main_bus_port_for_requester(requester_id),
                cycle_state.next_issue_sequence,
                port_io,
            },
        };
        cycle_state.next_issue_sequence++;
        throw BusYieldSignal{};
    }

private:
    void validate_replay(
            const BusRequest& recorded,
            BusOperation operation,
            uint64_t address,
            BusWidth width,
            uint64_t write_data,
            bool port_io) const {
        const int requester_id =
            system_.cores[static_cast<std::size_t>(core_index_)]->core_id;
        if (recorded.requester_id != requester_id ||
            recorded.operation != operation ||
            recorded.address != address ||
            recorded.width != width ||
            recorded.write_data != write_data ||
            recorded.ordering.port_io != port_io ||
            recorded.ordering.main_port_id !=
                system_.main_bus_port_for_requester(requester_id)) {
            throw std::runtime_error(
                "resumable instruction bus replay diverged");
        }
    }

    SystemState& system_;
    int core_index_;
};

// ---------------------------------------------------------------------------
//  Helpers
// ---------------------------------------------------------------------------

// Blocking on a different thread's framebuffer render is safe once the GIL is
// released.  Blocking on an exclusive lock already owned by this same thread
// is not, so guards publish every per-thread owner explicitly.  A stack, rather
// than only the innermost owner, also catches custom-buffer re-entry such as
// attach(A) -> attach(B) -> execute(A).
struct ThreadExclusiveMemoryOwner {
    MemoryMappings* memory;
    ThreadExclusiveMemoryOwner* previous;
};

static thread_local ThreadExclusiveMemoryOwner*
    thread_exclusive_memory_owners = nullptr;

// Mapping-wide admission and per-thread shared locking are distinct. One
// admission token owns the execution_active transition for an entire logical
// operation or private-worker wave. Every participating host thread then owns
// its own shared-mutex lease derived from that token. The coordinator retains
// the admission through collection, so no helper can release the global
// execution boundary.
class SharedMemoryExecutionAdmission {
public:
    SharedMemoryExecutionAdmission(
            MemoryMappings& memory,
            const char* busy_message)
        : memory_(memory) {
        bool expected = false;
        if (!memory_.execution_active.compare_exchange_strong(
                expected, true,
                std::memory_order_acq_rel, std::memory_order_acquire)) {
            throw std::runtime_error(busy_message);
        }
    }

    ~SharedMemoryExecutionAdmission() {
        memory_.execution_active.store(
            false, std::memory_order_release);
    }

    SharedMemoryExecutionAdmission(
        const SharedMemoryExecutionAdmission&) = delete;
    SharedMemoryExecutionAdmission& operator=(
        const SharedMemoryExecutionAdmission&) = delete;

    MemoryMappings& memory() const {
        return memory_;
    }

private:
    MemoryMappings& memory_;
};

// The root shared mapping ownership may be borrowed by nested same-thread
// Python scopes. Cross-thread participants share only the admission token and
// acquire their own SharedMemoryLease.
class SharedMemoryLease {
public:
    SharedMemoryLease(MemoryMappings& memory, const char* busy_message)
        : admission_(
              std::make_shared<SharedMemoryExecutionAdmission>(
                  memory, busy_message)),
          memory_lock_(memory.mutex) {}

    explicit SharedMemoryLease(
            std::shared_ptr<SharedMemoryExecutionAdmission>
                admission)
        : admission_(std::move(admission)),
          memory_lock_(checked_memory().mutex) {}

    SharedMemoryLease(const SharedMemoryLease&) = delete;
    SharedMemoryLease& operator=(const SharedMemoryLease&) = delete;

    const std::shared_ptr<SharedMemoryExecutionAdmission>&
    admission() const {
        return admission_;
    }

private:
    MemoryMappings& checked_memory() const {
        if (!admission_)
            throw std::invalid_argument(
                "shared memory execution admission is missing");
        return admission_->memory();
    }

    std::shared_ptr<SharedMemoryExecutionAdmission> admission_;
    std::shared_lock<std::shared_mutex> memory_lock_;
};

struct ThreadSharedMemoryOwner {
    MemoryMappings* memory;
    CPUState* permitted_cpu;
    std::shared_ptr<SharedMemoryLease> lease;
    ThreadSharedMemoryOwner* previous;
};

static thread_local ThreadSharedMemoryOwner*
    thread_shared_memory_owners = nullptr;

struct ThreadNativeExecutionOwner {
    MemoryMappings* memory;
    ThreadNativeExecutionOwner* previous;
};

static thread_local ThreadNativeExecutionOwner*
    thread_native_execution_owners = nullptr;

// A SystemState scheduler batch blocks every ordinary CPU execution entry
// point, including direct core bindings. Only the exact raw dispatch issued by
// that scheduler may pass the CPUExecutionGuard while the batch flag is set.
static thread_local std::atomic<bool>*
    thread_system_batch_execution_permission = nullptr;

class SystemBatchExecutionPermissionGuard {
public:
    explicit SystemBatchExecutionPermissionGuard(
            std::atomic<bool>& batch_active)
        : previous_(thread_system_batch_execution_permission) {
        thread_system_batch_execution_permission = &batch_active;
    }

    ~SystemBatchExecutionPermissionGuard() {
        thread_system_batch_execution_permission = previous_;
    }

    SystemBatchExecutionPermissionGuard(
        const SystemBatchExecutionPermissionGuard&) = delete;
    SystemBatchExecutionPermissionGuard& operator=(
        const SystemBatchExecutionPermissionGuard&) = delete;

private:
    std::atomic<bool>* previous_;
};

template <typename Owner>
static void unlink_thread_owner(Owner*& head, Owner& target) {
    Owner** link = &head;
    while (*link != nullptr) {
        if (*link == &target) {
            *link = target.previous;
            return;
        }
        link = &((*link)->previous);
    }
}

static bool thread_owns_exclusive_memory(const MemoryMappings& memory) {
    for (ThreadExclusiveMemoryOwner* owner =
             thread_exclusive_memory_owners;
         owner != nullptr;
         owner = owner->previous) {
        if (owner->memory == &memory)
            return true;
    }
    return false;
}

static ThreadSharedMemoryOwner* current_thread_shared_memory_owner(
        const MemoryMappings& memory) {
    for (ThreadSharedMemoryOwner* owner = thread_shared_memory_owners;
         owner != nullptr;
         owner = owner->previous) {
        if (owner->memory == &memory)
            return owner;
    }
    return nullptr;
}

static bool thread_owns_shared_memory(const MemoryMappings& memory) {
    return current_thread_shared_memory_owner(memory) != nullptr;
}

static bool thread_is_executing_memory(const MemoryMappings& memory) {
    for (ThreadNativeExecutionOwner* owner =
             thread_native_execution_owners;
         owner != nullptr;
         owner = owner->previous) {
        if (owner->memory == &memory)
            return true;
    }
    return false;
}

class ExclusiveMemoryUseGuard {
public:
    explicit ExclusiveMemoryUseGuard(
            MemoryMappings& memory,
            const char* busy_message =
                "memory attachments cannot be changed while CPUState memory is in use")
        : memory_(memory),
          lock_(memory.mutex, std::defer_lock),
          thread_owner_{&memory, nullptr} {
        // An execution callback reaches this check on the same thread that
        // owns the mapping mutex shared.  Reject before any mutex operation so the
        // path is defined by the C++ SharedMutex contract.
        if (memory_.execution_active.load(std::memory_order_acquire))
            throw std::runtime_error(busy_message);

        bool expected = false;
        if (!memory_.exclusive_active.compare_exchange_strong(
                expected, true,
                std::memory_order_acq_rel, std::memory_order_acquire)) {
            throw std::runtime_error(busy_message);
        }

        // Close the false->active race with CPUExecutionGuard.  If execution
        // publishes itself before we own the unique lock, this exclusive
        // operation backs off.  An execution that arrives after unique
        // ownership may safely wait because both Python execution bindings
        // release the GIL while acquiring their shared lock.
        if (memory_.execution_active.load(std::memory_order_acquire)) {
            memory_.exclusive_active.store(
                false, std::memory_order_release);
            throw std::runtime_error(busy_message);
        }

        bool locked = false;
        try {
            locked = lock_.try_lock();
        } catch (...) {
            memory_.exclusive_active.store(
                false, std::memory_order_release);
            throw;
        }
        if (!locked) {
            memory_.exclusive_active.store(
                false, std::memory_order_release);
            throw std::runtime_error(busy_message);
        }
        thread_owner_.previous = thread_exclusive_memory_owners;
        thread_exclusive_memory_owners = &thread_owner_;
        registered_thread_state_ = true;
    }

    ~ExclusiveMemoryUseGuard() {
        if (registered_thread_state_)
            unlink_thread_owner(
                thread_exclusive_memory_owners, thread_owner_);
        if (lock_.owns_lock())
            lock_.unlock();
        memory_.exclusive_active.store(false, std::memory_order_release);
    }

    ExclusiveMemoryUseGuard(const ExclusiveMemoryUseGuard&) = delete;
    ExclusiveMemoryUseGuard& operator=(const ExclusiveMemoryUseGuard&) = delete;

private:
    MemoryMappings& memory_;
    std::unique_lock<std::shared_mutex> lock_;
    ThreadExclusiveMemoryOwner thread_owner_;
    bool registered_thread_state_ = false;
};

using MemoryMutationGuard = ExclusiveMemoryUseGuard;

class CPUExecutionGuard {
public:
    explicit CPUExecutionGuard(CPUState& state)
        : state_(state),
          memory_(*state.memory),
          shared_owner_{&memory_, nullptr, nullptr, nullptr},
          native_owner_{&memory_, nullptr} {
        if (
            state_.system_cycle_execution_pending != nullptr &&
            state_.system_cycle_execution_pending->load(
                std::memory_order_acquire) &&
            thread_system_batch_execution_permission !=
                state_.system_batch_active
        ) {
            throw std::runtime_error(
                "suspended cycle execution must be resumed by its "
                "native system scheduler");
        }
        if (
            state_.system_batch_active != nullptr &&
            state_.system_batch_active->load(std::memory_order_acquire) &&
            thread_system_batch_execution_permission !=
                state_.system_batch_active
        ) {
            throw std::runtime_error(
                "native system batch is already active; "
                "CPUState is already executing");
        }

        // A custom buffer/exporter or other same-thread re-entry must never
        // block on an exclusive lock already owned by this thread.
        if (thread_owns_exclusive_memory(memory_))
            throw std::runtime_error(
                "CPUState cannot execute during same-thread exclusive memory use");

        if (thread_is_executing_memory(memory_))
            throw std::runtime_error("CPUState is already executing");

        // Megapad64 owns a logical-operation mapping scope across native
        // dispatch and any Python continuation.  The matching CPU may borrow
        // that scope, while sibling cores remain excluded by its global CAS.
        if (ThreadSharedMemoryOwner* owner =
                current_thread_shared_memory_owner(memory_)) {
            if (owner->permitted_cpu != &state_)
                throw std::runtime_error("CPUState is already executing");
            shared_owner_.lease = owner->lease;
            if (!shared_owner_.lease)
                throw std::runtime_error(
                    "CPUState shared memory ownership is invalid");
            // A logical operation grants exactly one native dispatch.  Clear
            // every matching permission in the nested owner stack before
            // entering native code so no outer scope can re-expose it after
            // an inner scope unwinds into Python continuation code.
            for (ThreadSharedMemoryOwner* candidate =
                     thread_shared_memory_owners;
                 candidate != nullptr;
                 candidate = candidate->previous) {
                if (candidate->memory == &memory_ &&
                    candidate->permitted_cpu == &state_) {
                    candidate->permitted_cpu = nullptr;
                }
            }
            register_shared_memory();
            register_native_execution();
            return;
        }

        // Both bindings release the GIL while acquiring this potentially
        // blocking lock.  They reacquire it only after this guard owns the
        // mapping, so render completion never waits behind a Python thread
        // that is itself waiting for memory.
        shared_owner_.lease = std::make_shared<SharedMemoryLease>(
            memory_, "CPUState is already executing");
        register_shared_memory();
        register_native_execution();
    }

    ~CPUExecutionGuard() {
        if (registered_native_state_)
            unlink_thread_owner(
                thread_native_execution_owners, native_owner_);
        if (registered_shared_state_)
            unlink_thread_owner(
                thread_shared_memory_owners, shared_owner_);
        shared_owner_.lease.reset();
    }

    CPUExecutionGuard(const CPUExecutionGuard&) = delete;
    CPUExecutionGuard& operator=(const CPUExecutionGuard&) = delete;

private:
    void register_shared_memory() {
        shared_owner_.previous = thread_shared_memory_owners;
        thread_shared_memory_owners = &shared_owner_;
        registered_shared_state_ = true;
    }

    void register_native_execution() {
        native_owner_.previous = thread_native_execution_owners;
        thread_native_execution_owners = &native_owner_;
        registered_native_state_ = true;
    }

    CPUState& state_;
    MemoryMappings& memory_;
    ThreadSharedMemoryOwner shared_owner_;
    ThreadNativeExecutionOwner native_owner_;
    bool registered_shared_state_ = false;
    bool registered_native_state_ = false;
};

static std::atomic<bool>& checked_system_batch_active(
        CPUState& state) {
    if (state.system_batch_active == nullptr)
        throw std::invalid_argument(
            "private execution requires a system-owned core");
    return *state.system_batch_active;
}

// A private command participates in one coordinator-owned mapping admission
// while retaining thread-local mutex and CPU execution ownership. Lane zero
// consumes the permission on the coordinator's root scope; auxiliary lanes
// create a per-thread shared lock from the same admission token.
class PrivateCoreExecutionScope {
public:
    PrivateCoreExecutionScope(
            CPUState& state,
            std::shared_ptr<SharedMemoryExecutionAdmission>
                admission)
        : permission_(checked_system_batch_active(state)),
          state_(state),
          admission_(std::move(admission)),
          thread_owner_{
              state.memory,
              &state,
              nullptr,
              nullptr,
          } {
        if (!admission_)
            throw std::invalid_argument(
                "private execution admission is missing");
        if (&admission_->memory() != state_.memory)
            throw std::invalid_argument(
                "private execution admission does not match core memory");

        ThreadSharedMemoryOwner* current =
            current_thread_shared_memory_owner(*state_.memory);
        if (current != nullptr) {
            if (
                current->permitted_cpu != &state_ ||
                !current->lease ||
                current->lease->admission() != admission_
            ) {
                throw std::runtime_error(
                    "private execution cannot borrow this "
                    "thread's memory scope");
            }
        } else {
            thread_owner_.lease =
                std::make_shared<SharedMemoryLease>(
                    admission_);
            thread_owner_.previous =
                thread_shared_memory_owners;
            thread_shared_memory_owners =
                &thread_owner_;
            registered_thread_owner_ = true;
        }

        try {
            execution_guard_ =
                std::make_unique<CPUExecutionGuard>(state_);
        } catch (...) {
            if (registered_thread_owner_) {
                unlink_thread_owner(
                    thread_shared_memory_owners,
                    thread_owner_);
                registered_thread_owner_ = false;
            }
            thread_owner_.lease.reset();
            throw;
        }
    }

    ~PrivateCoreExecutionScope() {
        execution_guard_.reset();
        if (registered_thread_owner_) {
            unlink_thread_owner(
                thread_shared_memory_owners,
                thread_owner_);
        }
        thread_owner_.lease.reset();
    }

    PrivateCoreExecutionScope(
        const PrivateCoreExecutionScope&) = delete;
    PrivateCoreExecutionScope& operator=(
        const PrivateCoreExecutionScope&) = delete;

private:
    SystemBatchExecutionPermissionGuard permission_;
    CPUState& state_;
    std::shared_ptr<SharedMemoryExecutionAdmission>
        admission_;
    ThreadSharedMemoryOwner thread_owner_;
    bool registered_thread_owner_ = false;
    std::unique_ptr<CPUExecutionGuard> execution_guard_;
};

// Direct DMA-capable device bindings participate in the same logical-owner
// mapping scope as instruction execution. A secondary-core Python MMIO
// fallback may re-enter a core-0 proxy on the same thread; that path borrows
// the outer scope instead of recursively locking std::shared_mutex.
class SharedMemoryUseGuard {
public:
    explicit SharedMemoryUseGuard(
            MemoryMappings& memory,
            CPUState* permitted_cpu = nullptr)
        : memory_(memory),
          thread_owner_{&memory, permitted_cpu, nullptr, nullptr} {
        if (thread_owns_exclusive_memory(memory_))
            throw std::runtime_error(
                "CPUState cannot use memory during same-thread exclusive memory use");

        if (ThreadSharedMemoryOwner* owner =
                current_thread_shared_memory_owner(memory_)) {
            thread_owner_.lease = owner->lease;
            if (!thread_owner_.lease)
                throw std::runtime_error(
                    "CPUState shared memory ownership is invalid");
            // Nested scopes can preserve an existing permission for the same
            // CPU, but can never introduce or transfer permission.
            if (permitted_cpu == nullptr ||
                owner->permitted_cpu != permitted_cpu ||
                thread_is_executing_memory(memory_)) {
                thread_owner_.permitted_cpu = nullptr;
            }
            register_thread_state();
            return;
        }

        thread_owner_.lease = std::make_shared<SharedMemoryLease>(
            memory_, "CPUState memory is already in use");
        register_thread_state();
    }

    ~SharedMemoryUseGuard() {
        if (registered_thread_state_)
            unlink_thread_owner(
                thread_shared_memory_owners, thread_owner_);
        thread_owner_.lease.reset();
    }

    SharedMemoryUseGuard(const SharedMemoryUseGuard&) = delete;
    SharedMemoryUseGuard& operator=(const SharedMemoryUseGuard&) = delete;

    std::shared_ptr<SharedMemoryExecutionAdmission>
    execution_admission() const {
        if (!thread_owner_.lease)
            throw std::logic_error(
                "shared memory execution admission is unavailable");
        return thread_owner_.lease->admission();
    }

private:
    void register_thread_state() {
        thread_owner_.previous = thread_shared_memory_owners;
        thread_shared_memory_owners = &thread_owner_;
        registered_thread_state_ = true;
    }

    MemoryMappings& memory_;
    ThreadSharedMemoryOwner thread_owner_;
    bool registered_thread_state_ = false;
};

static std::unique_ptr<SharedMemoryUseGuard>
acquire_shared_memory_use(
        CPUState& state,
        bool permit_native_execution = false) {
    // Proxy re-entry on the execution thread must not create a fresh
    // GIL-free window while the outer instruction is mutating CPU/device
    // state.  Borrowing and same-thread exclusive rejection cannot block.
    if (thread_owns_shared_memory(*state.memory) ||
        thread_owns_exclusive_memory(*state.memory)) {
        return std::make_unique<SharedMemoryUseGuard>(
            *state.memory,
            permit_native_execution ? &state : nullptr);
    }
    py::gil_scoped_release release;
    return std::make_unique<SharedMemoryUseGuard>(
        *state.memory,
        permit_native_execution ? &state : nullptr);
}

static void require_cycle_device_mutation_allowed(
        CPUState& state,
        const char* device_name) {
    const bool cycle_pending =
        state.system_cycle_execution_pending != nullptr &&
        state.system_cycle_execution_pending->load(
            std::memory_order_acquire);
    const bool batch_active =
        state.system_batch_active != nullptr &&
        state.system_batch_active->load(
            std::memory_order_acquire);
    const bool scheduler_continuation =
        batch_active &&
        thread_owns_shared_memory(*state.memory);
    if ((cycle_pending || batch_active) &&
        !scheduler_continuation) {
        throw std::runtime_error(
            std::string(device_name) +
            " cannot mutate while cycle execution is suspended");
    }
}

static std::unique_ptr<SharedMemoryUseGuard>
acquire_system_clock_advance_use(SystemState& system) {
    // A Python continuation or native callback already inside the logical
    // guest operation must not advance device time reentrantly.  Future
    // native scheduling can call SystemClock directly while it owns the
    // mapping-wide execution lease.
    if (thread_owns_shared_memory(system.shared_memory) ||
        thread_owns_exclusive_memory(system.shared_memory)) {
        throw std::runtime_error(
            "SystemState time cannot advance during guest execution");
    }
    py::gil_scoped_release release;
    return std::make_unique<SharedMemoryUseGuard>(
        system.shared_memory);
}

class PythonMemoryUseScope {
public:
    PythonMemoryUseScope(
            CPUState& state,
            bool permit_native_execution)
        : guard_(acquire_shared_memory_use(
              state, permit_native_execution)) {}

    void close() {
        guard_.reset();
    }

private:
    std::unique_ptr<SharedMemoryUseGuard> guard_;
};

struct PreparedBuffer {
    std::unique_ptr<py::buffer_info> lease;
    uint8_t* ptr;
    uint64_t capacity;
};

static PreparedBuffer prepare_writable_byte_buffer(
        py::buffer& buf, uint64_t logical_size, bool require_nonempty) {
    if (require_nonempty && logical_size == 0)
        throw py::value_error("main memory size must be greater than zero");

    auto lease = std::make_unique<py::buffer_info>(buf.request(true));
    if (lease->readonly)
        throw py::buffer_error("memory attachment requires a writable buffer");
    if (lease->ndim != 1 || lease->itemsize != 1 ||
        lease->shape.size() != 1 || lease->shape[0] < 0) {
        throw py::value_error(
            "memory attachment requires a one-dimensional byte buffer");
    }
    if (lease->strides.size() != 1 || lease->strides[0] != 1 ||
        !lease->view() || !PyBuffer_IsContiguous(lease->view(), 'C')) {
        throw py::value_error("memory attachment requires a C-contiguous buffer");
    }

    const uint64_t capacity = static_cast<uint64_t>(lease->shape[0]);
    if (logical_size > capacity)
        throw py::value_error("memory region size exceeds buffer capacity");
    if (capacity != 0 && lease->ptr == nullptr)
        throw py::value_error("memory attachment exposes a null data pointer");

    auto* ptr = static_cast<uint8_t*>(lease->ptr);
    return {std::move(lease), ptr, capacity};
}

static inline void validate_guest_region(uint64_t base, uint64_t size) {
    // The last byte may be exactly UINT64_MAX.  Only a non-empty region whose
    // inclusive last offset would wrap past it is invalid.
    if (size != 0 && (size - 1) > (MASK64 - base))
        throw py::value_error("guest memory region wraps past UINT64_MAX");
}

static inline void sync_nic_memory_ptrs(CPUState& s) {
    s.nic->attach_mem_ptrs(
        s.memory->mem, s.memory->mem_size,
        s.memory->hbw_mem, s.memory->hbw_base, s.memory->hbw_size,
        s.memory->ext_mem, s.memory->ext_mem_base, s.memory->ext_mem_size);
}

static inline void sync_main_memory_ptrs(CPUState& s) {
    s.uart->attach_mem(s.memory->mem, s.memory->mem_size);
    s.crypto->wots.mem = s.memory->mem;
    s.crypto->wots.mem_size = s.memory->mem_size;
    sync_nic_memory_ptrs(s);
}

static inline void sync_system_nic_memory_ptrs(SystemState& system) {
    system.shared_nic.attach_mem_ptrs(
        system.shared_memory.mem,
        system.shared_memory.mem_size,
        system.shared_memory.hbw_mem,
        system.shared_memory.hbw_base,
        system.shared_memory.hbw_size,
        system.shared_memory.ext_mem,
        system.shared_memory.ext_mem_base,
        system.shared_memory.ext_mem_size);
}

static inline void sync_system_main_memory_ptrs(SystemState& system) {
    system.shared_uart.attach_mem(
        system.shared_memory.mem,
        system.shared_memory.mem_size);
    system.shared_crypto.wots.mem = system.shared_memory.mem;
    system.shared_crypto.wots.mem_size = system.shared_memory.mem_size;
    sync_system_nic_memory_ptrs(system);
}

static inline void require_private_memory_mapping(const CPUState& s) {
    if (!s.private_memory)
        throw std::runtime_error(
            "system-owned CPUState mappings must be changed through SystemState");
}

static inline void require_unsealed_system_mappings(
        const SystemState& system) {
    if (system.mappings_sealed)
        throw std::runtime_error(
            "SystemState mappings are sealed after the first core borrow");
}

static inline uint64_t u64(uint64_t v) { return v; }  // native 64-bit
static inline int64_t  s64(uint64_t v) { return static_cast<int64_t>(v); }

static inline uint64_t sign_extend(uint64_t val, int bits) {
    uint64_t mask = (1ULL << bits) - 1;
    val &= mask;
    if (val & (1ULL << (bits - 1)))
        val |= ~mask;  // sign extend
    return val;
}

static inline uint8_t parity8(uint64_t val) {
    uint8_t b = val & 0xFF;
    b ^= b >> 4;
    b ^= b >> 2;
    b ^= b >> 1;
    return (b & 1) ^ 1;
}

// REX prefix helpers — extract register extension bits from ext_modifier.
// ext_modifier values 1-5 are REX prefixes; 0 is EXT.IMM64, 6 is SKIP, -1 is none.
static inline int rex_s(int m) { return (m >= 1 && m <= 5) ? (m & 1) : 0; }
static inline int rex_d(int m) { return (m >= 1 && m <= 5) ? ((m >> 1) & 1) : 0; }
static inline int rex_n(int m) { return (m >= 1 && m <= 5) ? ((m >> 2) & 1) : 0; }

// ---------------------------------------------------------------------------
//  Trap signaling
// ---------------------------------------------------------------------------

// We use a special return code to signal traps, halts, and MMIO needs
// to the Python layer.
enum StepResult {
    SR_OK = 0,
    SR_HALT = 1,
    SR_TRAP = 2,
    SR_IDLE = 3,
    SR_MMIO_READ = 4,
    SR_MMIO_WRITE = 5,
    SR_OUTPUT = 6,          // OUT port instruction
    SR_MEX_FALLBACK = 7,   // complex MEX op, fall back to Python
};

// ---------------------------------------------------------------------------
//  Memory access — region-aware (VRAM, XMEM, HBW, Bank 0)
// ---------------------------------------------------------------------------
//
// resolve_mem() maps a unified 64-bit address to a host pointer + region size,
// matching the RTL address decode in mp64_memory.v.  All scalar accessors
// route through it so that string instructions (BFILL, BCOPY) and ordinary
// load/stores work correctly across every memory aperture.
//

struct MemRegion {
    uint8_t* buf;
    uint64_t off;
    uint64_t size;
};

static inline bool region_contains(uint64_t base, uint64_t size, uint64_t addr) {
    // Subtraction after the lower-bound check avoids wrapping base + size.
    return addr >= base && (addr - base) < size;
}

static inline bool region_span_fits(uint64_t size, uint64_t off, uint64_t span) {
    return off < size && span <= (size - off);
}

static inline MemRegion resolve_mem(CPUState& s, uint64_t addr) {
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr))
        return {s.memory->vram_mem, addr - s.memory->vram_base, s.memory->vram_size};
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr))
        return {s.memory->ext_mem, addr - s.memory->ext_mem_base, s.memory->ext_mem_size};
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr))
        return {s.memory->hbw_mem, addr - s.memory->hbw_base, s.memory->hbw_size};
    if (!s.memory->mem || s.memory->mem_size == 0)
        throw std::runtime_error("main memory is not attached");
    return {s.memory->mem, addr % s.memory->mem_size, s.memory->mem_size};
}

static inline bool resolved_span_is_contiguous(
        CPUState& s, uint64_t addr, const MemRegion& first, uint64_t span) {
    if (!region_span_fits(first.size, first.off, span))
        return false;

    // Region membership can change in either direction inside a scalar
    // access (for example Bank0 -> HBW or HBW -> Bank0).  Prove every byte
    // maps to the same host buffer at the next contiguous offset before
    // taking the memcpy fast path.  Unsigned guest-address addition wraps
    // naturally, and an offset discontinuity then rejects the fast path.
    for (uint64_t i = 1; i < span; i++) {
        const MemRegion next = resolve_mem(s, addr + i);
        if (next.buf != first.buf || next.off != first.off + i)
            return false;
    }
    return true;
}

static inline uint8_t mem_read8(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    return r.buf[r.off];
}

static inline std::pair<std::size_t, uint64_t>
icache_key(uint64_t address) {
    const uint64_t line = address >> 4;
    return {
        static_cast<std::size_t>(
            line & (CPUState::ICACHE_LINES - 1)),
        line >> 8,
    };
}

static inline void icache_invalidate_span(
        CPUState& s,
        uint64_t address,
        uint64_t size) {
    if (s.profile != CoreProfile::FULL || size == 0)
        return;
    const uint64_t first =
        address & ~(CPUState::ICACHE_LINE_BYTES - 1);
    const uint64_t line_count =
        ((address & (CPUState::ICACHE_LINE_BYTES - 1)) +
         size + CPUState::ICACHE_LINE_BYTES - 1) /
        CPUState::ICACHE_LINE_BYTES;
    bool invalidated = false;
    for (uint64_t number = 0; number < line_count; number++) {
        const uint64_t line_address =
            first + number * CPUState::ICACHE_LINE_BYTES;
        const auto [index, tag] = icache_key(line_address);
        if (s.icache_valid[index] && s.icache_tags[index] == tag) {
            s.icache_valid[index] = 0;
            invalidated = true;
        }
    }
    if (invalidated)
        s.clear_private_decode_cache();
}

static inline void icache_invalidate_all(
        CPUState& s,
        bool reset_statistics) {
    s.clear_private_decode_cache();
    s.icache_valid.fill(0);
    s.ifetch_window_valid = false;
    if (reset_statistics) {
        s.icache_hits = 0;
        s.icache_misses = 0;
    }
}

static inline void icache_reset(CPUState& s) {
    s.icache_enabled = s.profile == CoreProfile::FULL ? 1 : 0;
    icache_invalidate_all(s, true);
}

static inline void mem_write8(CPUState& s, uint64_t addr, uint8_t val) {
    auto r = resolve_mem(s, addr);
    r.buf[r.off] = val;
    icache_invalidate_span(s, addr, 1);
}

static inline uint16_t mem_read16(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint16_t v;
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 2), 1))
        std::memcpy(&v, r.buf + r.off, 2);
    else {
        v = 0;
        for (int i = 0; i < 2; i++)
            v |= uint16_t(mem_read8(s, addr + static_cast<uint64_t>(i)))
                 << (8 * i);
    }
    return v;
}

static inline void mem_write16(CPUState& s, uint64_t addr, uint16_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 2), 1))
        std::memcpy(r.buf + r.off, &val, 2);
    else {
        for (int i = 0; i < 2; i++)
            mem_write8(s, addr + static_cast<uint64_t>(i),
                       static_cast<uint8_t>(val >> (8 * i)));
    }
    icache_invalidate_span(s, addr, 2);
}

static inline uint32_t mem_read32(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint32_t v;
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 4), 1))
        std::memcpy(&v, r.buf + r.off, 4);
    else {
        v = 0;
        for (int i = 0; i < 4; i++)
            v |= uint32_t(mem_read8(
                     s, addr + static_cast<uint64_t>(i))) << (8 * i);
    }
    return v;
}

static inline void mem_write32(CPUState& s, uint64_t addr, uint32_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 4), 1))
        std::memcpy(r.buf + r.off, &val, 4);
    else {
        for (int i = 0; i < 4; i++)
            mem_write8(s, addr + static_cast<uint64_t>(i),
                       static_cast<uint8_t>(val >> (8 * i)));
    }
    icache_invalidate_span(s, addr, 4);
}

static inline uint64_t mem_read64(CPUState& s, uint64_t addr) {
    auto r = resolve_mem(s, addr);
    uint64_t v;
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 8), 1))
        std::memcpy(&v, r.buf + r.off, 8);
    else {
        v = 0;
        for (int i = 0; i < 8; i++)
            v |= uint64_t(mem_read8(
                     s, addr + static_cast<uint64_t>(i))) << (8 * i);
    }
    return v;
}

static inline void mem_write64(CPUState& s, uint64_t addr, uint64_t val) {
    auto r = resolve_mem(s, addr);
    if (__builtin_expect(resolved_span_is_contiguous(s, addr, r, 8), 1))
        std::memcpy(r.buf + r.off, &val, 8);
    else {
        for (int i = 0; i < 8; i++)
            mem_write8(s, addr + static_cast<uint64_t>(i),
                       static_cast<uint8_t>(val >> (8 * i)));
    }
    icache_invalidate_span(s, addr, 8);
}

void CPUState::register_accel_hook(
        uint64_t addr,
        int hook_id,
        uint64_t code_size) {
    if (code_size == 0 || code_size > 4096)
        throw std::invalid_argument(
            "accelerator hook code size must be between 1 and 4096 bytes");

    MemoryMutationGuard guard(
        *memory,
        "CPUState accelerator hooks cannot be changed while CPUState is in use");
    std::vector<uint8_t> identity;
    identity.reserve(static_cast<std::size_t>(code_size));
    for (uint64_t offset = 0; offset < code_size; offset++)
        identity.push_back(mem_read8(*this, addr + offset));

    for (int index = 0; index < accel_hook_count; index++) {
        if (accel_hooks[index].addr == addr) {
            accel_hooks[index].id = hook_id;
            accel_hooks[index].code_identity = std::move(identity);
            return;
        }
    }
    if (accel_hook_count >= MAX_ACCEL_HOOKS)
        throw std::runtime_error(
            "maximum accelerator hook count exceeded");
    accel_hooks[accel_hook_count++] = {
        addr,
        hook_id,
        std::move(identity),
    };
}

// PC via psel
static inline uint64_t& pc(CPUState& s) { return s.regs[s.psel]; }
static inline uint64_t& rx(CPUState& s) { return s.regs[s.xsel]; }
static inline uint64_t& sp(CPUState& s) { return s.regs[s.spsel]; }

// ---------------------------------------------------------------------------
//  Accelerator hook lookup + native implementations
// ---------------------------------------------------------------------------

static inline int find_accel_hook(CPUState& s, uint64_t target) {
    for (int i = 0; i < s.accel_hook_count; i++) {
        const auto& entry = s.accel_hooks[i];
        if (entry.addr != target)
            continue;
        bool matches = true;
        for (std::size_t offset = 0;
             offset < entry.code_identity.size();
             offset++) {
            if (mem_read8(s, target + offset) !=
                entry.code_identity[offset]) {
                matches = false;
                break;
            }
        }
        if (matches)
            return entry.id;
    }
    return 0;
}

struct AccelHookContext {
    bool has_mmio;
    uint64_t mmio_start;
    uint64_t mmio_end;
};

struct AccelHookResult {
    bool handled;
    int extra_cycles;
};

struct DirectMemoryRegion {
    uint8_t* ptr;
    uint64_t avail;
    int priority;
};

enum class AccelAccessModel {
    SCALAR,
    BYTE,
};

// The native renderer rejects framebuffer dimensions beyond 4096×4096.
// Graphics shortcuts use that same architectural envelope.  VRAM-COPY's
// width is measured in bytes, so allow a full 4096-pixel RGBA scanline.
static constexpr uint64_t MAX_ACCEL_FB_DIMENSION = 4096;
static constexpr uint64_t MAX_ACCEL_COPY_ROW_BYTES =
    MAX_ACCEL_FB_DIMENSION * 4;
static constexpr uint64_t MAX_ACCEL_STRING_GLYPHS = 4096;
static constexpr uint64_t MAX_ACCEL_HOOK_EXTRA_CYCLES =
    static_cast<uint64_t>(std::numeric_limits<int>::max()) - 2;

// Scalar loads/stores use resolve_mem(), whose overlap priority is
// VRAM -> external memory -> HBW -> Bank 0.
static inline DirectMemoryRegion resolve_accel_scalar_region(
        CPUState& s, uint64_t addr) {
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        uint64_t off = addr - s.memory->vram_base;
        return {s.memory->vram_mem + off, s.memory->vram_size - off, 0};
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        uint64_t off = addr - s.memory->ext_mem_base;
        return {s.memory->ext_mem + off, s.memory->ext_mem_size - off, 1};
    }
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        uint64_t off = addr - s.memory->hbw_base;
        return {s.memory->hbw_mem + off, s.memory->hbw_size - off, 2};
    }
    if (s.memory->mem && addr < s.memory->mem_size)
        return {s.memory->mem + addr, s.memory->mem_size - addr, 3};
    return {nullptr, 0, 4};
}

// Byte instructions have an explicit supervisor-mode routing order in
// sys_read8()/sys_write8(): HBW -> external memory -> VRAM -> Bank 0.
static inline DirectMemoryRegion resolve_accel_byte_region(
        CPUState& s, uint64_t addr) {
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        uint64_t off = addr - s.memory->hbw_base;
        return {s.memory->hbw_mem + off, s.memory->hbw_size - off, 0};
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr))
        return {
            s.memory->ext_mem + (addr - s.memory->ext_mem_base),
            s.memory->ext_mem_size - (addr - s.memory->ext_mem_base),
            1,
        };
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr))
        return {
            s.memory->vram_mem + (addr - s.memory->vram_base),
            s.memory->vram_size - (addr - s.memory->vram_base),
            2,
        };
    if (s.memory->mem && addr < s.memory->mem_size)
        return {s.memory->mem + addr, s.memory->mem_size - addr, 3};
    return {nullptr, 0, 4};
}

static inline DirectMemoryRegion resolve_accel_region(
        CPUState& s, uint64_t addr, AccelAccessModel model) {
    return model == AccelAccessModel::SCALAR
        ? resolve_accel_scalar_region(s, addr)
        : resolve_accel_byte_region(s, addr);
}

static inline bool accel_span_overlaps_mmio(
        uint64_t addr, uint64_t size, const AccelHookContext& context) {
    if (!context.has_mmio || context.mmio_start >= context.mmio_end ||
        size == 0)
        return false;
    if (size - 1 > MASK64 - addr)
        return true;
    const uint64_t last = addr + size - 1;
    return addr < context.mmio_end && last >= context.mmio_start;
}

static inline bool guest_region_intersects_span(
        uint64_t region_base,
        uint64_t region_size,
        uint64_t span_base,
        uint64_t span_size) {
    if (region_size == 0 || span_size == 0)
        return false;
    const uint64_t region_last = region_base + region_size - 1;
    const uint64_t span_last = span_base + span_size - 1;
    return region_base <= span_last && span_base <= region_last;
}

static inline bool higher_priority_region_intersects_span(
        CPUState& s,
        uint64_t addr,
        uint64_t size,
        AccelAccessModel model,
        int selected_priority) {
    if (model == AccelAccessModel::SCALAR) {
        if (selected_priority > 0 &&
            guest_region_intersects_span(
                s.memory->vram_base, s.memory->vram_size, addr, size))
            return true;
        if (selected_priority > 1 &&
            guest_region_intersects_span(
                s.memory->ext_mem_base, s.memory->ext_mem_size, addr, size))
            return true;
        if (selected_priority > 2 &&
            guest_region_intersects_span(
                s.memory->hbw_base, s.memory->hbw_size, addr, size))
            return true;
    } else {
        if (selected_priority > 0 &&
            guest_region_intersects_span(
                s.memory->hbw_base, s.memory->hbw_size, addr, size))
            return true;
        if (selected_priority > 1 &&
            guest_region_intersects_span(
                s.memory->ext_mem_base, s.memory->ext_mem_size, addr, size))
            return true;
        if (selected_priority > 2 &&
            guest_region_intersects_span(
                s.memory->vram_base, s.memory->vram_size, addr, size))
            return true;
    }
    return false;
}

static inline bool accel_span_is_direct(
        CPUState& s,
        uint64_t addr,
        uint64_t size,
        AccelAccessModel model,
        const AccelHookContext& context) {
    if (size == 0)
        return true;
    if (size - 1 > MASK64 - addr ||
        accel_span_overlaps_mmio(addr, size, context))
        return false;
    const auto region = resolve_accel_region(s, addr, model);
    return region.ptr && size <= region.avail &&
        !higher_priority_region_intersects_span(
            s, addr, size, model, region.priority);
}

static inline bool accel_stack_is_direct(
        CPUState& s, uint64_t cells, const AccelHookContext& context) {
    return !s.priv_level &&
        accel_span_is_direct(
            s,
            s.regs[14],
            cells * 8,
            AccelAccessModel::SCALAR,
            context);
}

// Peek first and consume only after the complete operation has passed
// preflight.  A declined shortcut therefore leaves the data stack exactly as
// ordinary CALL.L expects before entering the BIOS implementation.
static inline uint64_t peek_data(CPUState& s, uint64_t index) {
    return mem_read64(s, s.regs[14] + index * 8);
}

static inline void consume_data(CPUState& s, uint64_t cells) {
    s.regs[14] += cells * 8;
}

static inline bool checked_accel_cycle_cost(
        uint64_t per_unit,
        uint64_t width,
        uint64_t height,
        uint64_t base,
        int& result) {
    if (per_unit == 0 || base > MAX_ACCEL_HOOK_EXTRA_CYCLES)
        return false;
    const uint64_t unit_budget =
        (MAX_ACCEL_HOOK_EXTRA_CYCLES - base) / per_unit;
    if (height != 0 && width > unit_budget / height)
        return false;
    result = static_cast<int>(per_unit * width * height + base);
    return true;
}

static inline void write_rgb565_le(uint8_t* ptr, uint16_t value) {
    ptr[0] = static_cast<uint8_t>(value);
    ptr[1] = static_cast<uint8_t>(value >> 8);
}

static inline uint8_t read_accel_byte(CPUState& s, uint64_t addr) {
    return *resolve_accel_byte_region(s, addr).ptr;
}

static inline bool host_spans_overlap(
        const uint8_t* first,
        uint64_t first_size,
        const uint8_t* second,
        uint64_t second_size) {
    const uintptr_t a = reinterpret_cast<uintptr_t>(first);
    const uintptr_t b = reinterpret_cast<uintptr_t>(second);
    return a <= b ? b - a < first_size : a - b < second_size;
}

// RECT-FILL ( addr stride w h color16 -- )
static AccelHookResult accel_rect_fill(
        CPUState& s, const AccelHookContext& context) {
    if (!accel_stack_is_direct(s, 5, context))
        return {false, 0};

    const uint16_t color16 = static_cast<uint16_t>(peek_data(s, 0));
    const uint64_t h = peek_data(s, 1);
    const uint64_t w = peek_data(s, 2);
    const uint64_t stride = peek_data(s, 3);
    uint64_t addr = peek_data(s, 4);

    if (w == 0 || h == 0) {
        consume_data(s, 5);
        return {true, 1};
    }

    int cycle_cost = 0;
    if (w > MAX_ACCEL_FB_DIMENSION || h > MAX_ACCEL_FB_DIMENSION ||
        !checked_accel_cycle_cost(5, w, h, 10, cycle_cost))
        return {false, 0};

    const uint64_t row_bytes = w * 2;
    uint64_t row_addr = addr;
    for (uint64_t row = 0; row < h; row++) {
        if (!accel_span_is_direct(
                s,
                row_addr,
                row_bytes,
                AccelAccessModel::SCALAR,
                context))
            return {false, 0};
        row_addr += stride;
    }

    consume_data(s, 5);
    for (uint64_t row = 0; row < h; row++) {
        auto region = resolve_accel_scalar_region(s, addr);
        for (uint64_t col = 0; col < w; col++)
            write_rgb565_le(region.ptr + col * 2, color16);
        icache_invalidate_span(s, addr, row_bytes);
        addr += stride;
    }
    return {true, cycle_cost};
}

// BLIT-GLYPH ( glyph-addr pixel-addr stride fg16 -- )
static AccelHookResult accel_blit_glyph(
        CPUState& s, const AccelHookContext& context) {
    if (!accel_stack_is_direct(s, 4, context))
        return {false, 0};

    const uint16_t fg16 = static_cast<uint16_t>(peek_data(s, 0));
    const uint64_t stride = peek_data(s, 1);
    uint64_t pixel_addr = peek_data(s, 2);
    const uint64_t glyph_addr = peek_data(s, 3);

    if (glyph_addr == 0) {
        consume_data(s, 4);
        return {true, 1};
    }
    if (!accel_span_is_direct(
            s,
            glyph_addr,
            8,
            AccelAccessModel::BYTE,
            context))
        return {false, 0};

    // Read 8 font bytes from guest memory
    uint8_t font_rows[8];
    for (int i = 0; i < 8; i++)
        font_rows[i] = read_accel_byte(s, glyph_addr + i);
    const auto font_region = resolve_accel_byte_region(s, glyph_addr);

    uint64_t row_addr = pixel_addr;
    for (int row = 0; row < 8; row++) {
        if (!accel_span_is_direct(
                s,
                row_addr,
                16,
                AccelAccessModel::SCALAR,
                context))
            return {false, 0};
        const auto output_region =
            resolve_accel_scalar_region(s, row_addr);
        if (host_spans_overlap(
                font_region.ptr, 8, output_region.ptr, 16))
            return {false, 0};
        row_addr += stride;
    }

    // Blit 8x8 glyph — only foreground (set) bits written
    consume_data(s, 4);
    for (int row = 0; row < 8; row++) {
        uint8_t bits = font_rows[row];
        if (bits) {  // skip empty rows entirely
            auto region = resolve_accel_scalar_region(s, pixel_addr);
            for (int col = 0; col < 8; col++) {
                if (bits & 0x80) {
                    write_rgb565_le(region.ptr + col * 2, fg16);
                    icache_invalidate_span(
                        s,
                        pixel_addr + static_cast<uint64_t>(col) * 2,
                        2);
                }
                bits <<= 1;
            }
        }
        pixel_addr += stride;
    }
    return {true, 120};
}

// VRAM-COPY ( src dst stride w h -- )
// Copy a w×h byte rectangle within VRAM.  Overlap-safe (memmove per row).
static AccelHookResult accel_vram_copy(
        CPUState& s, const AccelHookContext& context) {
    if (!accel_stack_is_direct(s, 5, context))
        return {false, 0};

    const uint64_t h = peek_data(s, 0);
    const uint64_t w = peek_data(s, 1);
    const uint64_t stride = peek_data(s, 2);
    const uint64_t dst = peek_data(s, 3);
    const uint64_t src = peek_data(s, 4);

    if (w == 0 || h == 0 || src == dst) {
        consume_data(s, 5);
        return {true, 1};
    }

    int cycle_cost = 0;
    if (w > MAX_ACCEL_COPY_ROW_BYTES || h > MAX_ACCEL_FB_DIMENSION ||
        !checked_accel_cycle_cost(3, w, h, 10, cycle_cost))
        return {false, 0};

    // Determine copy direction for overlap safety
    const bool backward = dst > src;
    uint64_t src_row = backward ? src + (h - 1) * stride : src;
    uint64_t dst_row = backward ? dst + (h - 1) * stride : dst;

    // Preflight every row before changing R14 or guest memory.  The BIOS
    // copies bytes left-to-right within a row, so decline partial same-row
    // overlap where memmove would deliberately produce a different result.
    uint64_t check_src = src_row;
    uint64_t check_dst = dst_row;
    for (uint64_t row = 0; row < h; row++) {
        if (!accel_span_is_direct(
                s, check_src, w, AccelAccessModel::BYTE, context) ||
            !accel_span_is_direct(
                s, check_dst, w, AccelAccessModel::BYTE, context))
            return {false, 0};
        const auto sr = resolve_accel_byte_region(s, check_src);
        const auto dr = resolve_accel_byte_region(s, check_dst);
        if (sr.ptr != dr.ptr &&
            host_spans_overlap(sr.ptr, w, dr.ptr, w))
            return {false, 0};
        if (backward) {
            check_src -= stride;
            check_dst -= stride;
        } else {
            check_src += stride;
            check_dst += stride;
        }
    }

    consume_data(s, 5);
    for (uint64_t row = 0; row < h; row++) {
        const auto sr = resolve_accel_byte_region(s, src_row);
        const auto dr = resolve_accel_byte_region(s, dst_row);
        std::memmove(dr.ptr, sr.ptr, static_cast<size_t>(w));
        icache_invalidate_span(s, dst_row, w);
        if (backward) {
            src_row -= stride;
            dst_row -= stride;
        } else {
            src_row += stride;
            dst_row += stride;
        }
    }
    return {true, cycle_cost};
}

// BLIT-STRING ( c-addr len pixel-addr stride fg16 font-base -- )
// Render a string of 8×8 glyphs.  Foreground-only (transparent bg).
static AccelHookResult accel_blit_string(
        CPUState& s, const AccelHookContext& context) {
    if (!accel_stack_is_direct(s, 6, context))
        return {false, 0};

    const uint64_t font_base = peek_data(s, 0);
    const uint16_t fg16 = static_cast<uint16_t>(peek_data(s, 1));
    const uint64_t stride = peek_data(s, 2);
    uint64_t pixel_addr = peek_data(s, 3);
    const uint64_t len = peek_data(s, 4);
    const uint64_t c_addr = peek_data(s, 5);

    if (len == 0) {
        consume_data(s, 6);
        return {true, 1};
    }

    int cycle_cost = 0;
    if (len > MAX_ACCEL_STRING_GLYPHS ||
        !checked_accel_cycle_cost(120, len, 1, 10, cycle_cost) ||
        !accel_span_is_direct(
            s, c_addr, len, AccelAccessModel::BYTE, context))
        return {false, 0};

    // Every character maps into the fixed 0x20..0xFF font table.  Proving the
    // whole table direct also lets us reject output/input aliasing up front,
    // so drawing cannot change a later character or glyph behind preflight.
    static constexpr uint64_t FONT_TABLE_BYTES = (0x100 - 0x20) * 8;
    if (!accel_span_is_direct(
            s,
            font_base,
            FONT_TABLE_BYTES,
            AccelAccessModel::BYTE,
            context))
        return {false, 0};
    const auto chars_region = resolve_accel_byte_region(s, c_addr);
    const auto font_region = resolve_accel_byte_region(s, font_base);

    // Validate every potential output row before committing.
    uint64_t glyph_pixel_addr = pixel_addr;
    for (uint64_t i = 0; i < len; i++) {
        uint64_t row_addr = glyph_pixel_addr;
        for (int row = 0; row < 8; row++) {
            if (!accel_span_is_direct(
                    s,
                    row_addr,
                    16,
                    AccelAccessModel::SCALAR,
                    context))
                return {false, 0};
            const auto output_region =
                resolve_accel_scalar_region(s, row_addr);
            if (host_spans_overlap(
                    chars_region.ptr, len, output_region.ptr, 16) ||
                host_spans_overlap(
                    font_region.ptr,
                    FONT_TABLE_BYTES,
                    output_region.ptr,
                    16))
                return {false, 0};
            row_addr += stride;
        }
        glyph_pixel_addr += 16;
    }

    consume_data(s, 6);
    for (uint64_t i = 0; i < len; i++) {
        uint8_t ch = read_accel_byte(s, c_addr + i);
        if (ch < 0x20) ch = 0x20;
        const uint64_t glyph_addr =
            font_base + static_cast<uint64_t>(ch - 0x20) * 8;

        // Read 8 font bytes
        uint8_t font_rows[8];
        for (int r = 0; r < 8; r++)
            font_rows[r] = read_accel_byte(s, glyph_addr + r);

        // Blit 8×8 glyph
        uint64_t pa = pixel_addr;
        for (int row = 0; row < 8; row++) {
            uint8_t bits = font_rows[row];
            if (bits) {
                auto region = resolve_accel_scalar_region(s, pa);
                for (int col = 0; col < 8; col++) {
                    if (bits & 0x80) {
                        write_rgb565_le(region.ptr + col * 2, fg16);
                        icache_invalidate_span(
                            s,
                            pa + static_cast<uint64_t>(col) * 2,
                            2);
                    }
                    bits <<= 1;
                }
            }
            pa += stride;
        }
        pixel_addr += 16;  // advance 8 pixels × 2 bytes
    }
    return {true, cycle_cost};
}

static AccelHookResult execute_accel_hook(
        CPUState& s,
        int hook_id,
        const AccelHookContext& context) {
    switch (hook_id) {
        case 1: return accel_rect_fill(s, context);
        case 2: return accel_blit_glyph(s, context);
        case 3: return accel_vram_copy(s, context);
        case 4: return accel_blit_string(s, context);
        default: return {false, 0};
    }
}

static inline void icache_begin_instruction(CPUState& s) {
    s.ifetch_window_valid = false;
    s.icache_undo_count = 0;
    s.icache_undo_hits = s.icache_hits;
    s.icache_undo_misses = s.icache_misses;
}

static inline void icache_record_line_undo(
        CPUState& s,
        std::size_t index) {
    for (std::size_t entry = 0; entry < s.icache_undo_count; entry++) {
        if (s.icache_undo_lines[entry].index == index)
            return;
    }
    if (s.icache_undo_count >= s.icache_undo_lines.size())
        throw std::logic_error(
            "instruction touched too many I-cache lines");
    auto& undo = s.icache_undo_lines[s.icache_undo_count++];
    undo.index = index;
    undo.valid = s.icache_valid[index];
    undo.tag = s.icache_tags[index];
    undo.data = s.icache_data[index];
}

static inline void icache_rollback_instruction(CPUState& s) {
    for (std::size_t entry = 0; entry < s.icache_undo_count; entry++) {
        const auto& undo = s.icache_undo_lines[entry];
        s.icache_valid[undo.index] = undo.valid;
        s.icache_tags[undo.index] = undo.tag;
        s.icache_data[undo.index] = undo.data;
    }
    s.icache_hits = s.icache_undo_hits;
    s.icache_misses = s.icache_undo_misses;
    s.icache_undo_count = 0;
    s.ifetch_window_valid = false;
}

static inline uint64_t read_instruction_dword(
        CPUState& s,
        uint64_t aligned_address) {
    if (s.instruction_bus_access != nullptr) {
        return s.instruction_bus_access->access(
            BusOperation::READ,
            aligned_address,
            BusWidth::DOUBLEWORD,
            0,
            false);
    }
    return mem_read64(s, aligned_address);
}

static inline void load_fetch_window(CPUState& s, uint64_t address) {
    const uint64_t window_address = address & ~uint64_t{7};
    if (!s.icache_enabled) {
        s.ifetch_window_data =
            read_instruction_dword(s, window_address);
        s.ifetch_window_addr = window_address;
        s.ifetch_window_valid = true;
        return;
    }

    const auto [index, tag] = icache_key(window_address);
    if (s.icache_valid[index] && s.icache_tags[index] == tag) {
        s.icache_hits++;
    } else {
        const uint64_t line_address =
            window_address & ~(CPUState::ICACHE_LINE_BYTES - 1);
        const uint64_t lo =
            read_instruction_dword(s, line_address);
        const uint64_t hi =
            read_instruction_dword(s, line_address + 8);
        icache_record_line_undo(s, index);
        std::memcpy(s.icache_data[index].data(), &lo, 8);
        std::memcpy(s.icache_data[index].data() + 8, &hi, 8);
        s.icache_tags[index] = tag;
        s.icache_valid[index] = 1;
        s.icache_misses++;
    }

    const std::size_t half = (window_address & 8) ? 8 : 0;
    std::memcpy(
        &s.ifetch_window_data,
        s.icache_data[index].data() + half,
        8);
    s.ifetch_window_addr = window_address;
    s.ifetch_window_valid = true;
}

static inline uint8_t icache_read_byte(
        CPUState& s,
        uint64_t address) {
    if (s.profile != CoreProfile::FULL)
        return mem_read8(s, address);
    const uint64_t window_address = address & ~uint64_t{7};
    if (!s.ifetch_window_valid ||
        s.ifetch_window_addr != window_address) {
        load_fetch_window(s, address);
    }
    return static_cast<uint8_t>(
        s.ifetch_window_data >> ((address & 7) * 8));
}

static inline uint8_t icache_peek_byte_without_accounting(
        CPUState& s,
        uint64_t address) {
    if (s.profile != CoreProfile::FULL || !s.icache_enabled)
        return mem_read8(s, address);
    const auto [index, tag] = icache_key(address);
    if (!s.icache_valid[index] || s.icache_tags[index] != tag)
        return mem_read8(s, address);
    return s.icache_data[index][
        static_cast<std::size_t>(
            address & (CPUState::ICACHE_LINE_BYTES - 1))];
}

static inline uint8_t fetch8(CPUState& s) {
    uint64_t a = pc(s);
    const uint8_t v = icache_read_byte(s, a);
    pc(s) = a + 1;
    return v;
}

static inline void push64(CPUState& s, uint64_t val) {
    sp(s) -= 8;
    mem_write64(s, sp(s), val);
}

static inline uint64_t pop64(CPUState& s) {
    uint64_t val = mem_read64(s, sp(s));
    sp(s) += 8;
    return val;
}

// ---------------------------------------------------------------------------
//  Flags
// ---------------------------------------------------------------------------

static inline uint8_t flags_pack(const CPUState& s) {
    return s.flag_z | (s.flag_c<<1) | (s.flag_n<<2) | (s.flag_v<<3) |
           (s.flag_p<<4) | (s.flag_g<<5) | (s.flag_i<<6) | (s.flag_s<<7);
}

static inline void flags_unpack(CPUState& s, uint8_t val) {
    s.flag_z = (val>>0)&1; s.flag_c = (val>>1)&1;
    s.flag_n = (val>>2)&1; s.flag_v = (val>>3)&1;
    s.flag_p = (val>>4)&1; s.flag_g = (val>>5)&1;
    s.flag_i = (val>>6)&1; s.flag_s = (val>>7)&1;
}

static inline bool eval_cond(const CPUState& s, int cc) {
    switch (cc) {
        case CC_AL: return true;
        case CC_EQ: return s.flag_z == 1;
        case CC_NE: return s.flag_z == 0;
        case CC_CS: return s.flag_c == 1;
        case CC_CC: return s.flag_c == 0;
        case CC_MI: return s.flag_n == 1;
        case CC_PL: return s.flag_n == 0;
        case CC_VS: return s.flag_v == 1;
        case CC_VC: return s.flag_v == 0;
        case CC_GT: return s.flag_g == 1;
        case CC_LE: return s.flag_g == 0;
        case CC_BQ: return s.q_out == 1;
        case CC_BNQ:return s.q_out == 0;
        case CC_SAT:return s.flag_s == 1;
        case CC_EF: return s.ef_flags != 0;
        case CC_NV: return false;
        default:    return false;
    }
}

static inline void update_flags_arith(CPUState& s, uint64_t a, uint64_t b,
                                       uint64_t result, bool is_sub) {
    s.flag_z = (result == 0) ? 1 : 0;
    s.flag_n = (result >> 63) & 1;
    s.flag_p = parity8(result);
    if (is_sub) {
        s.flag_c = (a >= b) ? 1 : 0;
    } else {
        // Detect carry out: unsigned overflow
        s.flag_c = (result < a || result < b) ? 1 : 0;
        // More precise: check if a+b > MASK64
        // Python does: 1 if (a+b) > MASK64
        // In C++, if a+b wraps, result < a
        // But with b potentially modified by carry, let's use __int128
        __uint128_t wide = (__uint128_t)a + (__uint128_t)b;
        s.flag_c = (wide > MASK64) ? 1 : 0;
    }
    int64_t sa = s64(a), sb = s64(b), sr = s64(result);
    if (is_sub) {
        s.flag_v = ((sa >= 0 && sb < 0 && sr < 0) ||
                    (sa < 0 && sb >= 0 && sr >= 0)) ? 1 : 0;
    } else {
        s.flag_v = ((sa >= 0 && sb >= 0 && sr < 0) ||
                    (sa < 0 && sb < 0 && sr >= 0)) ? 1 : 0;
    }
}

static inline void update_flags_logic(CPUState& s, uint64_t result) {
    s.flag_z = (result == 0) ? 1 : 0;
    s.flag_n = (result >> 63) & 1;
    s.flag_p = parity8(result);
    s.flag_c = 0;
    s.flag_v = 0;
}

static inline void update_flags_cmp(CPUState& s, uint64_t a, uint64_t b,
                                     uint64_t result) {
    update_flags_arith(s, a, b, result, true);
    s.flag_g = (a > b) ? 1 : 0;
}

// ---------------------------------------------------------------------------
//  CSR read/write
// ---------------------------------------------------------------------------

static uint64_t tacc_status(const CPUState& state) noexcept {
    const bool claimed = state.tacc_owner != TACC_OWNER_NONE;
    const bool mine =
        claimed && state.tacc_owner == state.core_id;
    return (
        (claimed ? 1ULL : 0ULL) |
        (mine ? 1ULL << 1 : 0ULL) |
        (state.tacc_valid ? 1ULL << 2 : 0ULL) |
        (state.tacc_dirty ? 1ULL << 3 : 0ULL) |
        (state.tacc_busy ? 1ULL << 4 : 0ULL) |
        (
            static_cast<uint64_t>(
                state.tacc_format_ew & 0x7
            ) << 5
        ) |
        (
            static_cast<uint64_t>(
                state.tacc_format_signed & 0x1
            ) << 8
        ) |
        (state.tacc_force_pending ? 1ULL << 9 : 0ULL) |
        (
            static_cast<uint64_t>(
                state.tacc_owner & 0x1F
            ) << 16
        )
    );
}

static void tacc_control_write(
        CPUState& state,
        uint64_t value) {
    // Reserved bits are ignored, including in user mode. Only the
    // FORCE_RELEASE pulse itself is privileged.
    if ((value & 0x1) == 0)
        return;
    if (state.priv_level != 0) {
        throw std::runtime_error(
            "TRAP:PRIV_FAULT:"
            "TACC force-release requires supervisor privilege");
    }
    if (state.tacc_busy) {
        state.tacc_force_pending = true;
    } else {
        state.reset_tacc();
    }
}

static uint64_t csr_read(CPUState& s, int addr) {
    switch (addr) {
        case CSR_FLAGS:     return flags_pack(s);
        case CSR_PSEL:      return s.psel;
        case CSR_XSEL:      return s.xsel;
        case CSR_SPSEL:     return s.spsel;
        case CSR_IVT_BASE:  return s.ivt_base;
        case CSR_D:         return s.d_reg;
        case CSR_DF:        return s.flag_c;
        case CSR_Q:         return s.q_out;
        case CSR_T:         return s.t_reg;
        case CSR_IE:        return s.flag_i;
        case CSR_PRIV:      return s.priv_level;
        case CSR_MPU_BASE:  return s.mpu_base;
        case CSR_MPU_LIMIT: return s.mpu_limit;
        case CSR_SB:        return s.sb;
        case CSR_SR:        return s.sr;
        case CSR_SC:        return s.sc;
        case CSR_SW:        return s.sw;
        case CSR_TMODE:     return s.tmode;
        case CSR_TCTRL:     return s.tctrl;
        case CSR_TSRC0:     return s.tsrc0;
        case CSR_TSRC1:     return s.tsrc1;
        case CSR_TDST:      return s.tdst;
        case CSR_ACC0:      return s.acc[0];
        case CSR_ACC1:      return s.acc[1];
        case CSR_ACC2:      return s.acc[2];
        case CSR_ACC3:      return s.acc[3];
        case CSR_TACC_STATUS:return tacc_status(s);
        case CSR_TACC_CTL:   return 0;
        case CSR_COREID:    return s.core_id;
        case CSR_NCORES:    return s.num_cores;
        case CSR_MBOX:
            return s.interrupts != nullptr
                ? s.interrupts->pending_mask(s.core_id)
                : 0;
        case CSR_IPIACK:    return 0;
        case CSR_IVEC_ID:   return s.ivec_id;
        case CSR_TRAP_ADDR: return s.trap_addr;
        case CSR_MEGAPAD_SZ:return 64;
        case CSR_CPUID:     return 0x4D503634;  // "MP64"
        case CSR_TSTRIDE_R: return s.tstride_r;
        case CSR_TSTRIDE_C: return s.tstride_c;
        case CSR_TTILE_H:   return s.ttile_h;
        case CSR_TTILE_W:   return s.ttile_w;
        case CSR_BIST_STATUS:    return s.bist_status;
        case CSR_BIST_FAIL_ADDR: return s.bist_fail_addr;
        case CSR_BIST_FAIL_DATA: return s.bist_fail_data;
        case CSR_TILE_SELFTEST:  return s.tile_selftest;
        case CSR_TILE_ST_DETAIL: return s.tile_st_detail;
        case CSR_PERF_CYCLES: return s.perf_cycles;
        case CSR_PERF_STALLS: return s.perf_stalls;
        case CSR_PERF_TILEOPS:return s.perf_tileops;
        case CSR_PERF_EXTMEM: return s.perf_extmem;
        case CSR_PERF_CTRL:   return s.perf_enable;
        case CSR_ICACHE_CTRL: return s.icache_enabled;
        case CSR_ICACHE_HITS: return s.icache_hits;
        case CSR_ICACHE_MISSES:return s.icache_misses;
        case CSR_CRC_ACC:     return s.crc_acc;
        case CSR_CRC_MODE:    return s.crc_mode;
        case CSR_SHA_MODE:    return s.sha_mode;
        case CSR_SHA_MSGLEN:  return s.sha_msglen_lo;
        case CSR_SHA_MSGLEN_HI: return s.sha_msglen_hi;
        case CSR_GF_PRIME_SEL:return s.gf_prime_sel;
        default: return 0;
    }
}

static void csr_write(CPUState& s, int addr, uint64_t val) {
    switch (addr) {
        case CSR_FLAGS:     flags_unpack(s, val & 0xFF); break;
        case CSR_PSEL:      s.psel = val & 0x1F; break;
        case CSR_XSEL:      s.xsel = val & 0x1F; break;
        case CSR_SPSEL:     s.spsel = val & 0x1F; break;
        case CSR_IVT_BASE:  s.ivt_base = val; break;
        case CSR_D:         s.d_reg = val & 0xFF; break;
        case CSR_DF:        s.flag_c = val & 1; break;
        case CSR_Q:         s.q_out = val & 1; break;
        case CSR_T:         s.t_reg = val & 0xFFFF; break;
        case CSR_IE:        s.flag_i = val & 1; break;
        case CSR_PRIV:      s.priv_level = val & 1; break;
        case CSR_MPU_BASE:  s.mpu_base = val; break;
        case CSR_MPU_LIMIT: s.mpu_limit = val; break;
        case CSR_SB:        s.sb = val; break;
        case CSR_SR:        s.sr = val; break;
        case CSR_SC:        s.sc = val; break;
        case CSR_SW:        s.sw = val; break;
        case CSR_TMODE:     s.tmode = val; break;
        case CSR_TCTRL:     s.tctrl = val; break;
        case CSR_TSRC0:     s.tsrc0 = val; break;
        case CSR_TSRC1:     s.tsrc1 = val; break;
        case CSR_TDST:      s.tdst = val; break;
        case CSR_ACC0:      s.acc[0] = val; break;
        case CSR_ACC1:      s.acc[1] = val; break;
        case CSR_ACC2:      s.acc[2] = val; break;
        case CSR_ACC3:      s.acc[3] = val; break;
        case CSR_TACC_STATUS: break;  // read-only
        case CSR_TACC_CTL:  tacc_control_write(s, val); break;
        case CSR_MBOX:
            if (s.interrupts != nullptr)
                s.interrupts->send_ipi(
                    s.core_id, static_cast<uint8_t>(val));
            break;
        case CSR_IPIACK:
            if (s.interrupts != nullptr)
                s.interrupts->acknowledge_ipi(
                    s.core_id, static_cast<uint8_t>(val));
            break;
        case CSR_TSTRIDE_R: s.tstride_r = val; break;
        case CSR_TSTRIDE_C: s.tstride_c = val; break;
        case CSR_TTILE_H:   s.ttile_h = val; break;
        case CSR_TTILE_W:   s.ttile_w = val; break;
        case CSR_BIST_CMD:
            if (val == 1 || val == 2) s.bist_status = 2;  // instant pass
            break;
        case CSR_TILE_SELFTEST:
            if (val == 1) { s.tile_selftest = 2; s.tile_st_detail = 0; }
            break;
        case CSR_PERF_CTRL:
            if (val & 1) s.perf_enable = 1;
            if (val & 2) {
                s.perf_cycles = 0; s.perf_stalls = 0;
                s.perf_tileops = 0; s.perf_extmem = 0;
                s.perf_enable = 1;
            }
            break;
        case CSR_ICACHE_CTRL:
            s.icache_enabled = val & 1;
            if (val & 2)
                icache_invalidate_all(s, true);
            break;
        case CSR_CRC_ACC:  s.crc_acc = val; break;
        case CSR_CRC_MODE: {
            s.crc_mode = (val == 1 || val == 2) ? (uint8_t)val : 0;
            break;
        }
        case CSR_SHA_MODE: s.sha_mode = val & 0x03; break;
        case CSR_SHA_MSGLEN: s.sha_msglen_lo = val; break;
        case CSR_SHA_MSGLEN_HI: s.sha_msglen_hi = val; break;
        case CSR_GF_PRIME_SEL:  s.gf_prime_sel = val & 0x03; break;
        default: break;
    }
}

// ---------------------------------------------------------------------------
//  Trap delivery
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
//  _next_instruction_size — for SKIP mode
// ---------------------------------------------------------------------------

static int next_instruction_size(CPUState& s) {
    // SKIP performs its own cache lookup for the target instruction.
    s.ifetch_window_valid = false;
    uint8_t peek = icache_read_byte(s, pc(s));
    int f = (peek >> 4) & 0xF;
    // Estimate: most instructions are 1 or 2 bytes
    switch (f) {
        case 0x0: { // SYS
            int n = peek & 0xF;
            return (n == 0xD) ? 2 : 1;  // CALL.L is 2 bytes
        }
        case 0x1: case 0x2: return 1;  // INC, DEC
        case 0x3: return 2;  // BR + offset
        case 0x4: return 3;  // LBR + 16-bit offset
        case 0x5: { // MEM
            int sub = peek & 0xF;
            return (sub == 0xF) ? 3 : 2;  // LD.D has extra offset byte
        }
        case 0x6: { // IMM
            int sub = peek & 0xF;
            if (sub == 0x0) return 3;  // LDI Rn, imm8
            if (sub <= 0x7) return 3;  // reg + imm8
            return 2;
        }
        case 0x7: return 2;  // ALU
        case 0x8: return 1;  // MEMALU
        case 0x9: return 1;  // I/O
        case 0xA: case 0xB: return 1;  // SEP, SEX
        case 0xC: {
            int sub = peek & 0xF;
            return (sub == 0xE) ? 3 : 2;  // MULDIV; RORI is 3 bytes
        }
        case 0xD: return 2;  // CSR
        case 0xE: return 2;  // MEX
        case 0xF: return 1;  // EXT prefix (shouldn't reach here)
        default: return 1;
    }
}

// ---------------------------------------------------------------------------
//  FP16 / BF16 conversion helpers (matches megapad64.py _fp16_to_float etc.)
// ---------------------------------------------------------------------------

static inline float fp16_to_float(uint16_t h) {
    uint32_t sign = (h >> 15) & 1;
    uint32_t exp  = (h >> 10) & 0x1F;
    uint32_t frac = h & 0x3FF;
    if (exp == 0) {
        if (frac == 0) {
            // ±0
            uint32_t bits = sign << 31;
            float f; std::memcpy(&f, &bits, 4); return f;
        }
        // Subnormal → normalise
        float val = ldexpf((float)frac / 1024.0f, -14);
        return sign ? -val : val;
    }
    if (exp == 0x1F) {
        if (frac == 0) {
            uint32_t bits = (sign << 31) | 0x7F800000u;
            float f; std::memcpy(&f, &bits, 4); return f;  // ±inf
        }
        uint32_t bits = (sign << 31) | 0x7FC00000u;  // qNaN
        float f; std::memcpy(&f, &bits, 4); return f;
    }
    float val = ldexpf(1.0f + (float)frac / 1024.0f, (int)exp - 15);
    return sign ? -val : val;
}

static inline uint16_t float_to_fp16(float f) {
    uint32_t bits;
    std::memcpy(&bits, &f, 4);
    uint32_t sign   = (bits >> 31) & 1;
    uint32_t exp32  = (bits >> 23) & 0xFF;
    uint32_t frac32 = bits & 0x7FFFFF;

    // NaN
    if (exp32 == 0xFF && frac32 != 0)
        return 0x7E00;  // qNaN
    // Inf
    if (exp32 == 0xFF)
        return (uint16_t)((sign << 15) | 0x7C00);
    // Zero
    if (exp32 == 0 && frac32 == 0)
        return (uint16_t)(sign << 15);

    int new_exp = (int)exp32 - 127 + 15;
    if (new_exp >= 0x1F)
        return (uint16_t)((sign << 15) | 0x7C00);  // overflow → ±inf
    if (new_exp <= 0) {
        if (new_exp < -10)
            return (uint16_t)(sign << 15);  // underflow → ±0
        // Subnormal
        frac32 |= 0x800000;
        int shift = 1 - new_exp;
        uint32_t round_bit = (frac32 >> (12 + shift)) & 1;
        uint32_t sticky    = (frac32 & ((1u << (12 + shift)) - 1)) ? 1 : 0;
        uint32_t result    = frac32 >> (13 + shift);
        if (round_bit && (sticky || (result & 1)))
            result++;
        return (uint16_t)((sign << 15) | (result & 0x3FF));
    }
    // Normal: round mantissa from 23 bits to 10 bits
    uint32_t round_bit = (frac32 >> 12) & 1;
    uint32_t sticky    = (frac32 & 0xFFF) ? 1 : 0;
    uint32_t frac16    = frac32 >> 13;
    if (round_bit && (sticky || (frac16 & 1))) {
        frac16++;
        if (frac16 >= 0x400) {
            frac16 = 0;
            new_exp++;
            if (new_exp >= 0x1F)
                return (uint16_t)((sign << 15) | 0x7C00);
        }
    }
    return (uint16_t)((sign << 15) | (new_exp << 10) | (frac16 & 0x3FF));
}

static inline float bf16_to_float(uint16_t b) {
    uint32_t bits32 = (uint32_t)b << 16;
    float f; std::memcpy(&f, &bits32, 4); return f;
}

static constexpr uint16_t fp32_bits_to_bf16(uint32_t bits) {
    if ((bits & 0x7F800000U) == 0x7F800000U &&
        (bits & 0x007FFFFFU) != 0) {
        // Preserve sign and the representable payload while forcing the BF16
        // quiet bit.  Numeric rounding can carry a maximal NaN payload into
        // a zero encoding, so NaNs must bypass the rounding path.
        return static_cast<uint16_t>((bits >> 16) | 0x0040U);
    }
    uint32_t round_bit = (bits >> 15) & 1;
    uint32_t sticky    = (bits & 0x7FFF) ? 1 : 0;
    uint32_t result    = bits >> 16;
    if (round_bit && (sticky || (result & 1)))
        result++;
    return static_cast<uint16_t>(result & 0xFFFFU);
}

static_assert(fp32_bits_to_bf16(0x7FFFFFFFU) == 0x7FFF);
static_assert(fp32_bits_to_bf16(0xFFFFFFFFU) == 0xFFFF);
static_assert(fp32_bits_to_bf16(0x7F800001U) == 0x7FC0);
static_assert(fp32_bits_to_bf16(0xFF800001U) == 0xFFC0);
static_assert(fp32_bits_to_bf16(0x7FCD0000U) == 0x7FCD);

static inline uint16_t float_to_bf16(float f) {
    uint32_t bits;
    std::memcpy(&bits, &f, 4);
    return fp32_bits_to_bf16(bits);
}

static inline float fp_decode(uint16_t raw, int ew) {
    return (ew == EW_FP16) ? fp16_to_float(raw) : bf16_to_float(raw);
}

static inline uint16_t fp_encode(float val, int ew) {
    return (ew == EW_FP16) ? float_to_fp16(val) : float_to_bf16(val);
}

static inline bool fp_is_nan(uint16_t raw, int ew) {
    if (ew == EW_FP16)
        return ((raw >> 10) & 0x1F) == 0x1F && (raw & 0x3FF) != 0;
    else  // BF16
        return ((raw >> 7) & 0xFF) == 0xFF && (raw & 0x7F) != 0;
}

static inline bool fp_is_finite(uint16_t raw, int ew) {
    if (ew == EW_FP16)
        return ((raw >> 10) & 0x1F) != 0x1F;
    return ((raw >> 7) & 0xFF) != 0xFF;
}

static inline bool fp32_bits_are_finite(uint64_t raw) {
    return ((static_cast<uint32_t>(raw) >> 23) & 0xFF) != 0xFF;
}

static inline uint32_t fp32_to_bits(float f) {
    uint32_t b; std::memcpy(&b, &f, 4); return b;
}

static inline float bits_to_fp32(uint32_t b) {
    float f; std::memcpy(&f, &b, 4); return f;
}

static_assert(sizeof(float) == 4 &&
              std::numeric_limits<float>::is_iec559);
static_assert(sizeof(double) == 8 &&
              std::numeric_limits<double>::is_iec559);
static constexpr double FP32_PACK_OVERFLOW_THRESHOLD =
    0x1.ffffffp+127;

static inline bool fp32_pack_overflows(double value) {
    // The executable oracle rounds finite results at or beyond this midpoint
    // to signed FP32 infinity.  Compare in double precision so identifying
    // that boundary does not itself narrow or overflow the value.
    return std::isfinite(value) &&
           std::fabs(value) >= FP32_PACK_OVERFLOW_THRESHOLD;
}

// ---------------------------------------------------------------------------
//  Tile helpers for MEX
// ---------------------------------------------------------------------------

static constexpr std::size_t TILE_BYTES = 64;
using Tile = std::array<uint8_t, TILE_BYTES>;

template <std::size_t ElemBytes>
static inline uint64_t tile_get_elem_width(const Tile& tile, int lane) {
    static_assert(ElemBytes == 1 || ElemBytes == 2 ||
                  ElemBytes == 4 || ElemBytes == 8);
    if (lane < 0 || static_cast<std::size_t>(lane) >= TILE_BYTES / ElemBytes)
        return 0;
    const std::size_t off = static_cast<std::size_t>(lane) * ElemBytes;
    uint64_t v = 0;
    for (std::size_t i = 0; i < ElemBytes; i++)
        v |= (uint64_t)tile[off + i] << (8 * i);
    return v;
}

static inline uint64_t tile_get_elem(const Tile& tile, int lane, int eb) {
    switch (eb) {
        case 1: return tile_get_elem_width<1>(tile, lane);
        case 2: return tile_get_elem_width<2>(tile, lane);
        case 4: return tile_get_elem_width<4>(tile, lane);
        case 8: return tile_get_elem_width<8>(tile, lane);
        default: return 0;
    }
}

template <std::size_t ElemBytes>
static inline void tile_set_elem_width(Tile& tile, int lane, uint64_t val) {
    static_assert(ElemBytes == 1 || ElemBytes == 2 ||
                  ElemBytes == 4 || ElemBytes == 8);
    if (lane < 0 || static_cast<std::size_t>(lane) >= TILE_BYTES / ElemBytes)
        return;
    const std::size_t off = static_cast<std::size_t>(lane) * ElemBytes;
    for (std::size_t i = 0; i < ElemBytes; i++)
        tile[off + i] = (val >> (8 * i)) & 0xFF;
}

static inline void tile_set_elem(Tile& tile, int lane, int eb, uint64_t val) {
    switch (eb) {
        case 1: tile_set_elem_width<1>(tile, lane, val); break;
        case 2: tile_set_elem_width<2>(tile, lane, val); break;
        case 4: tile_set_elem_width<4>(tile, lane, val); break;
        case 8: tile_set_elem_width<8>(tile, lane, val); break;
        default: break;
    }
}

static inline int64_t to_signed_eb(uint64_t v, int eb) {
    int bits = eb * 8;
    if (eb == 8) {
        if (v & SIGN64) {
            const __int128 signed_v =
                static_cast<__int128>(v) -
                (static_cast<__int128>(1) << 64);
            return static_cast<int64_t>(signed_v);
        }
        return static_cast<int64_t>(v);
    }
    if (v & (1ULL << (bits - 1)))
        return static_cast<int64_t>(v) - static_cast<int64_t>(1ULL << bits);
    return (int64_t)v;
}

static inline uint64_t elem_mask(int elem_bytes) {
    return elem_bytes == 8 ? MASK64 : ((1ULL << (elem_bytes * 8)) - 1);
}

static inline __int128 floor_shift_right(__int128 value, unsigned shift) {
    if (shift == 0)
        return value;
    if (value >= 0)
        return value >> shift;
    const __int128 divisor = static_cast<__int128>(1) << shift;
    return -(((-value) + divisor - 1) >> shift);
}

// ---------------------------------------------------------------------------
//  Unified tile memory access (64-byte reads/writes with address decoding)
// ---------------------------------------------------------------------------

static inline void tile_read_64bytes(CPUState& s, uint64_t addr, Tile& out) {
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        const uint64_t off = addr - s.memory->vram_base;
        if (region_span_fits(s.memory->vram_size, off, TILE_BYTES))
            std::memcpy(out.data(), s.memory->vram_mem + off, TILE_BYTES);
        else
            out.fill(0);
        return;
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        const uint64_t off = addr - s.memory->ext_mem_base;
        if (region_span_fits(s.memory->ext_mem_size, off, TILE_BYTES))
            std::memcpy(out.data(), s.memory->ext_mem + off, TILE_BYTES);
        else
            out.fill(0);
        return;
    }
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        const uint64_t off = addr - s.memory->hbw_base;
        if (region_span_fits(s.memory->hbw_size, off, TILE_BYTES))
            std::memcpy(out.data(), s.memory->hbw_mem + off, TILE_BYTES);
        else
            out.fill(0);
        return;
    }
    if (!s.memory->mem || s.memory->mem_size == 0) {
        out.fill(0);
        return;
    }
    uint64_t a = addr % s.memory->mem_size;
    if (region_span_fits(s.memory->mem_size, a, TILE_BYTES))
        std::memcpy(out.data(), s.memory->mem + a, TILE_BYTES);
    else
        out.fill(0);
}

static inline void tile_write_64bytes(CPUState& s, uint64_t addr, const Tile& data) {
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        const uint64_t off = addr - s.memory->vram_base;
        if (region_span_fits(s.memory->vram_size, off, TILE_BYTES)) {
            std::memcpy(s.memory->vram_mem + off, data.data(), TILE_BYTES);
            icache_invalidate_span(s, addr, TILE_BYTES);
        }
        return;
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        const uint64_t off = addr - s.memory->ext_mem_base;
        if (region_span_fits(s.memory->ext_mem_size, off, TILE_BYTES)) {
            std::memcpy(s.memory->ext_mem + off, data.data(), TILE_BYTES);
            icache_invalidate_span(s, addr, TILE_BYTES);
        }
        return;
    }
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        const uint64_t off = addr - s.memory->hbw_base;
        if (region_span_fits(s.memory->hbw_size, off, TILE_BYTES)) {
            std::memcpy(s.memory->hbw_mem + off, data.data(), TILE_BYTES);
            icache_invalidate_span(s, addr, TILE_BYTES);
        }
        return;
    }
    if (!s.memory->mem || s.memory->mem_size == 0)
        return;
    uint64_t a = addr % s.memory->mem_size;
    if (region_span_fits(s.memory->mem_size, a, TILE_BYTES)) {
        std::memcpy(s.memory->mem + a, data.data(), TILE_BYTES);
        icache_invalidate_span(s, addr, TILE_BYTES);
    }
}

// ---------------------------------------------------------------------------
//  MEX core — native only where behavior is exact and mutation-free fallback
//  is possible.  Returning -1 asks the Python oracle to execute the operation.
// ---------------------------------------------------------------------------

static int exec_mex(CPUState& s, int n) {
    int ss = (n >> 2) & 0x3;
    int op = n & 0x3;

    uint8_t funct_byte = fetch8(s);
    int funct = funct_byte & 0x07;

    int broadcast_reg = -1;
    if (ss == 1)
        broadcast_reg = fetch8(s) & 0xF;

    // Keep the complete Phase-0 TACC namespace on the executable Python
    // oracle until the native arithmetic landing. Match raw bytes before
    // SS=imm8 normalizes its data byte to function zero, and before generic
    // MEX can read a source, clear ACC_ZERO, or write a destination.
    //
    // Function seven is part of the reserved TAMAC boundary and must reach
    // Python so it traps identically. Immediate-splat remains legacy TMUL
    // except for the exact E9 06 encoding reserved as illegal TAMAC.
    const bool raw_tamac =
        op == 0x1 &&
        (
            (
                ss != 0x2 &&
                (
                    (funct_byte & 0x7) == 0x6 ||
                    (funct_byte & 0x7) == 0x7
                )
            ) ||
            (ss == 0x2 && funct_byte == 0x06)
        );
    const bool raw_tacc_lifecycle =
        s.ext_modifier == 0x8 &&
        op == 0x3 &&
        (funct_byte & 0x7) >= 0x2;
    if (raw_tamac || raw_tacc_lifecycle)
        return -1;

    int ew_bits = s.tmode & 0x7;
    bool is_fp = ew_bits >= EW_FP16;

    int elem_bytes = is_fp ? 2 : (1 << ew_bits);
    int num_lanes = 64 / elem_bytes;
    bool is_signed = (s.tmode >> 4) & 1;

    // SS=imm8 uses the function byte as data and forces the operation's
    // sub-function to zero.
    if (ss == 0x2)
        funct = 0;

    // Python owns the 256-bit integer accumulator semantics and the current
    // TSYS instruction map.  Decide this before reading sources or changing
    // ACC/TCTRL/destination state so rewind-and-fallback is transactional.
    const bool fp_bit_reduction = is_fp && op == 0x2 &&
                                  (funct == 3 || funct == 4);
    const bool fp_sum_reduction = is_fp && op == 0x2 &&
                                  (funct == 0 || funct == 5);
    if ((op == 0x1 && !is_fp && funct != 0) ||
        (op == 0x2 && (!is_fp || fp_bit_reduction || fp_sum_reduction)) ||
        op == 0x3) {
        return -1;
    }

    // Read source tiles
    Tile src_a{}, src_b{}, dst{};
    tile_read_64bytes(s, s.tsrc0, src_a);

    if (ss == 0x0) {  // tile-tile
        tile_read_64bytes(s, s.tsrc1, src_b);
    } else if (ss == 0x1) {  // broadcast
        uint64_t bval = (broadcast_reg >= 0) ? s.regs[broadcast_reg] : 0;
        uint64_t mask = (elem_bytes < 8) ? ((1ULL << (elem_bytes*8)) - 1) : MASK64;
        bval &= mask;
        for (int lane = 0; lane < num_lanes; lane++)
            tile_set_elem(src_b, lane, elem_bytes, bval);
    } else if (ss == 0x2) {  // imm8 splat
        src_b = src_a;
        src_a.fill(funct_byte);
    } else {  // ss == 3, in-place
        tile_read_64bytes(s, s.tdst, src_a);
        tile_read_64bytes(s, s.tsrc0, src_b);
    }

    // Python's floating-point helpers own NaN payload/sign propagation and
    // non-finite arithmetic semantics.  These checks are deliberately after
    // safe source reads but before ACC_ZERO, TCTRL, accumulator, or memory
    // mutation, so step_one can rewind and retry transactionally.
    Tile fp_existing{};
    bool fp_existing_loaded = false;
    if (is_fp) {
        const bool fp_talu_arithmetic =
            op == 0x0 && (funct == 0 || funct == 1);
        const bool fp_tmul_arithmetic =
            op == 0x1 && funct >= 0 && funct <= 5;
        const bool fp_tred_ordering =
            op == 0x2 &&
            (funct == 1 || funct == 2 || funct == 6 || funct == 7);
        if (fp_talu_arithmetic || fp_tmul_arithmetic || fp_tred_ordering) {
            auto tile_is_finite = [&](const Tile& tile) {
                for (int lane = 0; lane < num_lanes; lane++) {
                    const uint16_t raw = static_cast<uint16_t>(
                        tile_get_elem(tile, lane, 2));
                    if (!fp_is_finite(raw, ew_bits))
                        return false;
                }
                return true;
            };

            if (!tile_is_finite(src_a) ||
                (!fp_tred_ordering && !tile_is_finite(src_b))) {
                return -1;
            }

            if (op == 0x1 && (funct == 3 || funct == 4)) {
                tile_read_64bytes(s, s.tdst, fp_existing);
                fp_existing_loaded = true;
                if (!tile_is_finite(fp_existing))
                    return -1;
            }

            const bool consumes_acc =
                op == 0x1 && (funct == 1 || funct == 5) &&
                (s.tctrl & 0x1) && !(s.tctrl & 0x2);
            if (consumes_acc) {
                const int acc_count = funct == 1 ? 1 : 4;
                for (int index = 0; index < acc_count; index++) {
                    if (!fp32_bits_are_finite(s.acc[index]))
                        return -1;
                }
            }

            // BF16 spans essentially the full FP32 exponent range.  Preflight
            // results at the FP32 overflow boundary in double precision and
            // fall back transactionally so the executable oracle owns the
            // exact infinity encoding and architectural post-state.  This
            // also avoids relying on native intermediate-rounding details at
            // the edge of the accelerated format.
            if (ew_bits == EW_BF16) {
                auto lane_value = [&](const Tile& tile, int lane) {
                    return static_cast<double>(fp_decode(
                        static_cast<uint16_t>(
                            tile_get_elem(tile, lane, 2)),
                        ew_bits));
                };

                if (fp_talu_arithmetic) {
                    for (int lane = 0; lane < num_lanes; lane++) {
                        const double a = lane_value(src_a, lane);
                        const double b = lane_value(src_b, lane);
                        const double result =
                            funct == 0 ? a + b : a - b;
                        if (fp32_pack_overflows(result))
                            return -1;
                    }
                } else if (fp_tmul_arithmetic) {
                    if (funct == 0 || funct == 2) {
                        for (int lane = 0; lane < num_lanes; lane++) {
                            const double result =
                                lane_value(src_a, lane) *
                                lane_value(src_b, lane);
                            if (fp32_pack_overflows(result))
                                return -1;
                        }
                    } else if (funct == 3 || funct == 4) {
                        for (int lane = 0; lane < num_lanes; lane++) {
                            const double result =
                                lane_value(src_a, lane) *
                                    lane_value(src_b, lane) +
                                lane_value(fp_existing, lane);
                            if (fp32_pack_overflows(result))
                                return -1;
                        }
                    } else if (funct == 1) {
                        double total = 0.0;
                        for (int lane = 0; lane < num_lanes; lane++) {
                            total += lane_value(src_a, lane) *
                                     lane_value(src_b, lane);
                        }
                        if (consumes_acc) {
                            total =
                                static_cast<double>(bits_to_fp32(
                                    static_cast<uint32_t>(s.acc[0]))) +
                                total;
                        }
                        if (fp32_pack_overflows(total))
                            return -1;
                    } else if (funct == 5) {
                        const int chunk_size = num_lanes / 4;
                        for (int chunk = 0; chunk < 4; chunk++) {
                            double total = 0.0;
                            for (int lane = 0; lane < chunk_size; lane++) {
                                const int index =
                                    chunk * chunk_size + lane;
                                total += lane_value(src_a, index) *
                                         lane_value(src_b, index);
                            }
                            if (consumes_acc) {
                                total =
                                    static_cast<double>(bits_to_fp32(
                                        static_cast<uint32_t>(
                                            s.acc[chunk]))) +
                                    total;
                            }
                            if (fp32_pack_overflows(total))
                                return -1;
                        }
                    }
                }
            }
        }
    }

    // Extended Tile ALU (EXT modifier 8)
    if (s.ext_modifier == 8 && op == 0x0) {
        bool rounding = (s.tmode >> 6) & 1;
        for (int lane = 0; lane < num_lanes; lane++) {
            uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
            uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
            int bits = elem_bytes * 8;
            uint64_t mask = elem_mask(elem_bytes);
            int shift_amt = eb_val & (bits - 1);
            uint64_t r = 0;
            if (funct == 0) {  // VSHR
                if (is_signed) {
                    __int128 value = to_signed_eb(ea, elem_bytes);
                    if (rounding && shift_amt > 0)
                        value += static_cast<__int128>(1) << (shift_amt - 1);
                    r = static_cast<uint64_t>(
                            floor_shift_right(value, static_cast<unsigned>(shift_amt))) & mask;
                } else {
                    __uint128_t value = ea;
                    if (rounding && shift_amt > 0)
                        value += static_cast<__uint128_t>(1) << (shift_amt - 1);
                    r = static_cast<uint64_t>(value >> shift_amt) & mask;
                }
            } else if (funct == 1) {  // VSHL
                r = (ea << shift_amt) & mask;
            } else if (funct == 2) {  // VSEL
                r = ea;
            } else if (funct == 3) {  // VCLZ
                if (ea == 0) r = bits;
                else {
                    r = bits;
                    uint64_t tmp = ea;
                    while (tmp) { tmp >>= 1; r--; }
                }
            }
            tile_set_elem(dst, lane, elem_bytes, r);
        }
        tile_write_64bytes(s, s.tdst, dst);
        return 1;
    }

    if (op == 0x0) {  // TALU
        if (is_fp) {
            // ---- Floating-point TALU ----
            uint16_t qnan = (ew_bits == EW_FP16) ? 0x7E00 : 0x7FC0;
            for (int lane = 0; lane < num_lanes; lane++) {
                uint16_t ea  = (uint16_t)tile_get_elem(src_a, lane, 2);
                uint16_t eb_val = (uint16_t)tile_get_elem(src_b, lane, 2);
                uint16_t r = 0;
                switch (funct) {
                    case 2: r = ea & eb_val; break;  // AND — bitwise
                    case 3: r = ea | eb_val; break;  // OR
                    case 4: r = ea ^ eb_val; break;  // XOR
                    case 7: r = ea & 0x7FFF; break;  // ABS — clear sign bit
                    case 5: {  // MIN — NaN-propagating
                        if (fp_is_nan(ea, ew_bits) || fp_is_nan(eb_val, ew_bits))
                            r = qnan;
                        else {
                            float fa = fp_decode(ea, ew_bits);
                            float fb = fp_decode(eb_val, ew_bits);
                            // Python min/max preserve the first operand on
                            // equality, including the sign of zero.
                            r = fp_encode(fb < fa ? fb : fa, ew_bits);
                        }
                        break;
                    }
                    case 6: {  // MAX — NaN-propagating
                        if (fp_is_nan(ea, ew_bits) || fp_is_nan(eb_val, ew_bits))
                            r = qnan;
                        else {
                            float fa = fp_decode(ea, ew_bits);
                            float fb = fp_decode(eb_val, ew_bits);
                            r = fp_encode(fb > fa ? fb : fa, ew_bits);
                        }
                        break;
                    }
                    default: {  // ADD (0) / SUB (1)
                        float fa = fp_decode(ea, ew_bits);
                        float fb = fp_decode(eb_val, ew_bits);
                        r = fp_encode(funct == 0 ? fa + fb : fa - fb, ew_bits);
                        break;
                    }
                }
                tile_set_elem(dst, lane, 2, r);
            }
            tile_write_64bytes(s, s.tdst, dst);
            return 0;
        }

        // ---- Integer TALU ----
        bool saturate = (s.tmode >> 5) & 1;
        for (int lane = 0; lane < num_lanes; lane++) {
            uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
            uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
            int bits = elem_bytes * 8;
            uint64_t mask = elem_mask(elem_bytes);
            uint64_t r = 0;

            switch (funct) {
                case 0: {  // ADD
                    if (saturate) {
                        if (is_signed) {
                            __int128 sum = static_cast<__int128>(
                                               to_signed_eb(ea, elem_bytes)) +
                                           static_cast<__int128>(
                                               to_signed_eb(eb_val, elem_bytes));
                            const __int128 bound =
                                static_cast<__int128>(1) << (bits - 1);
                            const __int128 hi = bound - 1;
                            const __int128 lo = -bound;
                            if (sum > hi) sum = hi;
                            if (sum < lo) sum = lo;
                            r = static_cast<uint64_t>(sum) & mask;
                        } else {
                            const __uint128_t sum =
                                static_cast<__uint128_t>(ea) + eb_val;
                            r = sum > static_cast<__uint128_t>(mask)
                                    ? mask : static_cast<uint64_t>(sum);
                        }
                    } else {
                        r = (ea + eb_val) & mask;
                    }
                    break;
                }
                case 1: {  // SUB
                    if (saturate) {
                        if (is_signed) {
                            __int128 diff = static_cast<__int128>(
                                                to_signed_eb(ea, elem_bytes)) -
                                            static_cast<__int128>(
                                                to_signed_eb(eb_val, elem_bytes));
                            const __int128 bound =
                                static_cast<__int128>(1) << (bits - 1);
                            const __int128 hi = bound - 1;
                            const __int128 lo = -bound;
                            if (diff > hi) diff = hi;
                            if (diff < lo) diff = lo;
                            r = static_cast<uint64_t>(diff) & mask;
                        } else {
                            r = ea < eb_val ? 0 : ea - eb_val;
                        }
                    } else {
                        r = (ea - eb_val) & mask;
                    }
                    break;
                }
                case 2: r = ea & eb_val; break;   // AND
                case 3: r = ea | eb_val; break;   // OR
                case 4: r = ea ^ eb_val; break;   // XOR
                case 5: {  // MIN
                    if (is_signed)
                        r = (to_signed_eb(ea, elem_bytes) < to_signed_eb(eb_val, elem_bytes))
                            ? ea : eb_val;
                    else
                        r = (ea < eb_val) ? ea : eb_val;
                    break;
                }
                case 6: {  // MAX
                    if (is_signed)
                        r = (to_signed_eb(ea, elem_bytes) > to_signed_eb(eb_val, elem_bytes))
                            ? ea : eb_val;
                    else
                        r = (ea > eb_val) ? ea : eb_val;
                    break;
                }
                case 7: {  // ABS
                    if (is_signed) {
                        const int64_t sv = to_signed_eb(ea, elem_bytes);
                        // Compute the magnitude in unsigned arithmetic so
                        // abs(INT64_MIN) is defined and wraps like Python.
                        r = (sv < 0 ? (~ea + 1) : ea) & mask;
                    } else {
                        r = ea;
                    }
                    break;
                }
            }
            tile_set_elem(dst, lane, elem_bytes, r);
        }
        tile_write_64bytes(s, s.tdst, dst);
        return 0;
    }

    if (op == 0x1) {  // TMUL
        if (is_fp) {
            // ---- Floating-point TMUL ----
            if (funct == 0) {  // MUL
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fa * fb, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 1;
            }
            if (funct == 1) {  // DOT — FP16/BF16 → FP32 accumulate
                if (s.tctrl & 0x2) {
                    s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
                    s.tctrl &= ~0x2ULL;
                }
                double total = 0.0;
                for (int lane = 0; lane < num_lanes; lane++) {
                    double fa = fp_decode(
                        (uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    double fb = fp_decode(
                        (uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    total += fa * fb;
                }
                if (s.tctrl & 0x1)  // ACC_ACC
                    total = static_cast<double>(
                                bits_to_fp32((uint32_t)s.acc[0])) + total;
                s.acc[0] = fp32_to_bits(static_cast<float>(total));
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 3;
            }
            if (funct == 2) {  // WMUL — fp16/bf16 → fp32 widening multiply
                Tile dst0{}, dst1{};
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    uint32_t fp32bits = fp32_to_bits(fa * fb);
                    if (lane < 16)
                        tile_set_elem(dst0, lane, 4, fp32bits);
                    else
                        tile_set_elem(dst1, lane - 16, 4, fp32bits);
                }
                tile_write_64bytes(s, s.tdst, dst0);
                tile_write_64bytes(s, s.tdst + 64, dst1);
                return 2;
            }
            if (funct == 3) {  // MAC — fp mul-accumulate: dst += a*b
                // Preloaded by the transactional finite-input check above.
                if (!fp_existing_loaded)
                    tile_read_64bytes(s, s.tdst, fp_existing);
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    float fc = fp_decode((uint16_t)tile_get_elem(fp_existing, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fc + fa * fb, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 2;
            }
            if (funct == 4) {  // FMA — dst = a*b + dst
                // Preloaded by the transactional finite-input check above.
                if (!fp_existing_loaded)
                    tile_read_64bytes(s, s.tdst, fp_existing);
                for (int lane = 0; lane < num_lanes; lane++) {
                    float fa = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);
                    float fb = fp_decode((uint16_t)tile_get_elem(src_b, lane, 2), ew_bits);
                    float fc = fp_decode((uint16_t)tile_get_elem(fp_existing, lane, 2), ew_bits);
                    tile_set_elem(dst, lane, 2, fp_encode(fa * fb + fc, ew_bits));
                }
                tile_write_64bytes(s, s.tdst, dst);
                return 2;
            }
            if (funct == 5) {  // DOTACC — 4-way chunked dot, FP32 accumulate
                int chunk_size = num_lanes / 4;
                if (s.tctrl & 0x2) {
                    s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
                    s.tctrl &= ~0x2ULL;
                }
                for (int k = 0; k < 4; k++) {
                    double dot = 0.0;
                    for (int lane = 0; lane < chunk_size; lane++) {
                        int idx = k * chunk_size + lane;
                        double fa = fp_decode(
                            (uint16_t)tile_get_elem(src_a, idx, 2), ew_bits);
                        double fb = fp_decode(
                            (uint16_t)tile_get_elem(src_b, idx, 2), ew_bits);
                        dot += fa * fb;
                    }
                    if (s.tctrl & 0x1)  // ACC_ACC
                        dot = static_cast<double>(
                                  bits_to_fp32((uint32_t)s.acc[k])) + dot;
                    s.acc[k] = fp32_to_bits(static_cast<float>(dot));
                }
                s.flag_z = (s.acc[0] == 0 && s.acc[1] == 0 &&
                            s.acc[2] == 0 && s.acc[3] == 0) ? 1 : 0;
                return 3;
            }
            return 1;  // unknown FP TMUL funct
        }

        // ---- Integer TMUL ----
        if (funct == 0) {  // MUL (element-wise)
            for (int lane = 0; lane < num_lanes; lane++) {
                uint64_t ea = tile_get_elem(src_a, lane, elem_bytes);
                uint64_t eb_val = tile_get_elem(src_b, lane, elem_bytes);
                const uint64_t mask = elem_mask(elem_bytes);
                uint64_t r = 0;
                if (is_signed) {
                    const __int128 product =
                        static_cast<__int128>(to_signed_eb(ea, elem_bytes)) *
                        static_cast<__int128>(to_signed_eb(eb_val, elem_bytes));
                    r = static_cast<uint64_t>(product) & mask;
                } else {
                    const __uint128_t product =
                        static_cast<__uint128_t>(ea) * eb_val;
                    r = static_cast<uint64_t>(product) & mask;
                }
                tile_set_elem(dst, lane, elem_bytes, r);
            }
            tile_write_64bytes(s, s.tdst, dst);
            return 1;
        }
        // Non-MUL integer functions were routed to Python before source reads.
        return -1;
    }

    if (op == 0x2) {  // TRED (reductions)
        // Keep this guard adjacent to ACC_ZERO as a transactional invariant,
        // even though the same cases are routed before source reads above.
        if (!is_fp || funct == 0 || funct == 3 ||
            funct == 4 || funct == 5)
            return -1;

        // Handle ACC_ZERO (TCTRL bit 1): clear accumulator, one-shot
        if (s.tctrl & 0x2) {
            s.acc[0] = s.acc[1] = s.acc[2] = s.acc[3] = 0;
            s.tctrl &= ~0x2;  // clear the one-shot bit
        }
        bool acc_acc = (s.tctrl & 0x1) != 0;  // ACC_ACC is bit 0

        if (is_fp) {
            // ---- Floating-point TRED ----
            // Decode all lanes
            float fp_vals[32] = {0};
            for (int lane = 0; lane < num_lanes; lane++)
                fp_vals[lane] = fp_decode((uint16_t)tile_get_elem(src_a, lane, 2), ew_bits);

            if (funct == 0) {  // SUM — FP32 accumulate
                float total = 0.0f;
                for (int lane = 0; lane < num_lanes; lane++)
                    total += fp_vals[lane];
                if (acc_acc)
                    total += bits_to_fp32((uint32_t)s.acc[0]);
                s.acc[0] = fp32_to_bits(total);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 0;
            }
            if (funct == 1) {  // MIN
                float best = fp_vals[0];
                for (int lane = 1; lane < num_lanes; lane++) {
                    if (!std::isnan(fp_vals[lane]) && (std::isnan(best) || fp_vals[lane] < best))
                        best = fp_vals[lane];
                }
                s.acc[0] = fp32_to_bits(best);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 2) {  // MAX
                float best = fp_vals[0];
                for (int lane = 1; lane < num_lanes; lane++) {
                    if (!std::isnan(fp_vals[lane]) && (std::isnan(best) || fp_vals[lane] > best))
                        best = fp_vals[lane];
                }
                s.acc[0] = fp32_to_bits(best);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 5) {  // SUMSQ — FP32 accumulate
                float total = 0.0f;
                for (int lane = 0; lane < num_lanes; lane++)
                    total += fp_vals[lane] * fp_vals[lane];
                if (acc_acc)
                    total += bits_to_fp32((uint32_t)s.acc[0]);
                s.acc[0] = fp32_to_bits(total);
                s.acc[1] = s.acc[2] = s.acc[3] = 0;
                s.flag_z = (total == 0.0f) ? 1 : 0;
                return 0;
            }
            if (funct == 6) {  // MINIDX
                int best_idx = 0;
                float best_val = fp_vals[0];
                for (int i = 1; i < num_lanes; i++) {
                    if (!std::isnan(fp_vals[i]) && (std::isnan(best_val) || fp_vals[i] < best_val)) {
                        best_val = fp_vals[i];
                        best_idx = i;
                    }
                }
                s.acc[0] = (uint64_t)best_idx;
                s.acc[1] = fp32_to_bits(best_val);
                s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            if (funct == 7) {  // MAXIDX
                int best_idx = 0;
                float best_val = fp_vals[0];
                for (int i = 1; i < num_lanes; i++) {
                    if (!std::isnan(fp_vals[i]) && (std::isnan(best_val) || fp_vals[i] > best_val)) {
                        best_val = fp_vals[i];
                        best_idx = i;
                    }
                }
                s.acc[0] = (uint64_t)best_idx;
                s.acc[1] = fp32_to_bits(best_val);
                s.acc[2] = s.acc[3] = 0;
                return 0;
            }
            // funct is masked to three bits; FP POPCNT/L1 were routed before
            // ACC_ZERO, and every other value returned above.
            return 0;
        }
        return 0;
    }

    // TSYS and all ambiguous integer accumulator paths returned before any
    // source read or architectural mutation.
    return -1;
}

// ---------------------------------------------------------------------------
//  Single step — returns cycle count, or throws on trap/halt
//  mmio_read8 / mmio_write8 are Python callbacks for MMIO
//  py_on_output is callback for OUT port instruction
//  py_csr_read/write override CSR access (for IPI patching in system.py)
// ---------------------------------------------------------------------------

struct StepCallbacks {
    std::function<uint8_t(uint64_t)> mmio_read8;
    std::function<void(uint64_t, uint8_t)> mmio_write8;
    std::function<void(int, int)> on_output;   // (port, value)
    // CSR overrides for system-level patching (IPI etc.)
    std::function<uint64_t(int)> csr_read_override;  // returns value, or -1 for default
    uint64_t mmio_start;
    uint64_t mmio_end;
    bool has_mmio;
    ResumableBusAccess* bus_access = nullptr;
    bool strict_cycle_dma = false;
};

struct DmaEndpointCallbacks {
    int requester_id = 0;
    std::function<DmaEndpointView(uint64_t)> inspect;
    std::function<void(uint64_t, const BusResult&)> complete;
};

// MPU check — user-mode memory window enforcement
static inline void mpu_check(CPUState& s, uint64_t addr) {
    if (s.priv_level && s.mpu_limit > s.mpu_base) {
        if (addr < s.mpu_base || addr >= s.mpu_limit) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
    }
}

static inline bool uart_geom_span(uint32_t mmio_off, uint32_t width) {
    return mmio_off >= UartGeomDevice::GEOM_BASE &&
           mmio_off < UartGeomDevice::GEOM_END &&
           width <= UartGeomDevice::GEOM_END - mmio_off;
}

// UART geometry is byte-oriented.  Wider guest accesses may stay native, but
// every read8/write8 retains its own device-lock acquisition so adjacent byte
// transactions are not silently combined into a new atomic operation.

static inline void preflight_resumable_bus_access(
        CPUState& s,
        uint64_t address) {
    if (main_bus_address_is_mmio(address) || !s.priv_level)
        return;
    if (s.memory->hbw_mem &&
        region_contains(
            s.memory->hbw_base,
            s.memory->hbw_size,
            address)) {
        s.trap_addr = address;
        throw std::runtime_error("TRAP:PRIV_FAULT");
    }
    mpu_check(s, address);
}

// Memory access with MMIO and HBW intercept
static inline uint8_t sys_read8(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        return static_cast<uint8_t>(cb.bus_access->access(
            BusOperation::READ,
            addr,
            BusWidth::BYTE,
            0,
            port_io));
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        // Try C++ devices first (no Python callback needed)
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic->handles(mmio_off))
            return s.nic->read8(mmio_off);
        if (s.uart->handles(mmio_off))
            return s.uart->read8(mmio_off);
        if (s.trng->handles(mmio_off))
            return s.trng->read8(mmio_off);
        if (s.crypto->handles(mmio_off))
            return s.crypto->read8(mmio_off);
        if (s.fb->handles(mmio_off))
            return s.fb->read8(mmio_off);
        if (s.timer->handles(mmio_off))
            return s.timer->read8(mmio_off);
        if (s.rtc->handles(mmio_off))
            return s.rtc->read8(mmio_off);
        if (uart_geom_span(mmio_off, 1) &&
                s.uart_geom->handles(mmio_off))
            return s.uart_geom->read8(mmio_off);
        return cb.mmio_read8(addr);  // fallback to Python for other devices
    }
    if (s.priv_level) {
        // User mode: block HBW entirely, check MPU for RAM
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        return s.memory->hbw_mem[addr - s.memory->hbw_base];
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        return s.memory->ext_mem[addr - s.memory->ext_mem_base];
    }
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        return s.memory->vram_mem[addr - s.memory->vram_base];
    }
    return mem_read8(s, addr);
}

static inline bool write_native_nic_bytes(
        CPUState& s,
        const StepCallbacks& cb,
        uint32_t mmio_offset,
        uint64_t value,
        int byte_count) {
    if (!s.nic->handles(mmio_offset))
        return false;
    for (int index = 0; index < byte_count; index++) {
        s.nic->write8(
            mmio_offset + static_cast<uint32_t>(index),
            static_cast<uint8_t>(value >> (8 * index)),
            cb.strict_cycle_dma);
    }
    return true;
}

static inline void sys_write8(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        uint8_t val,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        cb.bus_access->access(
            BusOperation::WRITE,
            addr,
            BusWidth::BYTE,
            val,
            port_io);
        icache_invalidate_span(s, addr, 1);
        return;
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        // Try C++ devices first
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (write_native_nic_bytes(
                s, cb, mmio_off, val, 1)) {
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.uart->handles(mmio_off)) {
            s.uart->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.trng->handles(mmio_off)) {
            s.trng->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.crypto->handles(mmio_off)) {
            s.crypto->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            s.fb->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            s.timer->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            s.rtc->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        if (uart_geom_span(mmio_off, 1) &&
                s.uart_geom->handles(mmio_off)) {
            s.uart_geom->write8(mmio_off, val);
            icache_invalidate_span(s, addr, 1);
            return;
        }
        cb.mmio_write8(addr, val);  // fallback to Python for other devices
        icache_invalidate_span(s, addr, 1);
        return;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr;
            throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    } else if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        s.memory->hbw_mem[addr - s.memory->hbw_base] = val;
        icache_invalidate_span(s, addr, 1);
        return;
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        s.memory->ext_mem[addr - s.memory->ext_mem_base] = val;
        icache_invalidate_span(s, addr, 1);
        return;
    }
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        s.memory->vram_mem[addr - s.memory->vram_base] = val;
        icache_invalidate_span(s, addr, 1);
        return;
    }
    mem_write8(s, addr, val);
}

// Wider MMIO/HBW-aware reads/writes
static inline uint64_t sys_read64(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        return cb.bus_access->access(
            BusOperation::READ,
            addr,
            BusWidth::DOUBLEWORD,
            0,
            port_io);
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.nic->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 8))
                throw std::runtime_error("TRAP:BUS_FAULT");
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.trng->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.crypto->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.crypto->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.fb->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.fb->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.timer->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.timer->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.rtc->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.rtc->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (uart_geom_span(mmio_off, 8) &&
                s.uart_geom->handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.uart_geom->read8(mmio_off + i) << (8*i);
            return v;
        }
        uint64_t v = 0;
        for (int i = 0; i < 8; i++)
            v |= (uint64_t)cb.mmio_read8(addr + i) << (8*i);
        return v;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    return mem_read64(s, addr);
}

static inline void sys_write64(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        uint64_t val,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        cb.bus_access->access(
            BusOperation::WRITE,
            addr,
            BusWidth::DOUBLEWORD,
            val,
            port_io);
        icache_invalidate_span(s, addr, 8);
        return;
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (write_native_nic_bytes(
                s, cb, mmio_off, val, 8)) {
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 8))
                throw std::runtime_error("TRAP:BUS_FAULT");
            for (int i = 0; i < 8; i++)
                s.trng->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (s.crypto->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.crypto->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.fb->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.timer->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.rtc->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        if (uart_geom_span(mmio_off, 8) &&
                s.uart_geom->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.uart_geom->write8(
                    mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 8);
            return;
        }
        for (int i = 0; i < 8; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        icache_invalidate_span(s, addr, 8);
        return;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    mem_write64(s, addr, val);
}

static inline uint16_t sys_read16(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        return static_cast<uint16_t>(cb.bus_access->access(
            BusOperation::READ,
            addr,
            BusWidth::HALF,
            0,
            port_io));
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 2))
                throw std::runtime_error("TRAP:BUS_FAULT");
            return s.trng->read8(mmio_off) |
                   ((uint16_t)s.trng->read8(mmio_off + 1) << 8);
        }
        if (s.crypto->handles(mmio_off))
            return s.crypto->read8(mmio_off) | ((uint16_t)s.crypto->read8(mmio_off+1) << 8);
        if (s.fb->handles(mmio_off))
            return s.fb->read8(mmio_off) |
                   ((uint16_t)s.fb->read8(mmio_off + 1) << 8);
        if (s.timer->handles(mmio_off))
            return s.timer->read8(mmio_off) |
                   ((uint16_t)s.timer->read8(mmio_off + 1) << 8);
        if (s.rtc->handles(mmio_off))
            return s.rtc->read8(mmio_off) |
                   ((uint16_t)s.rtc->read8(mmio_off + 1) << 8);
        if (uart_geom_span(mmio_off, 2) &&
                s.uart_geom->handles(mmio_off))
            return s.uart_geom->read8(mmio_off) |
                   ((uint16_t)s.uart_geom->read8(mmio_off + 1) << 8);
        return cb.mmio_read8(addr) | ((uint16_t)cb.mmio_read8(addr+1) << 8);
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    return mem_read16(s, addr);
}

static inline void sys_write16(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        uint16_t val,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        cb.bus_access->access(
            BusOperation::WRITE,
            addr,
            BusWidth::HALF,
            val,
            port_io);
        icache_invalidate_span(s, addr, 2);
        return;
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (write_native_nic_bytes(
                s, cb, mmio_off, val, 2)) {
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 2))
                throw std::runtime_error("TRAP:BUS_FAULT");
            s.trng->write8(mmio_off, val & 0xFF);
            s.trng->write8(mmio_off + 1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (s.crypto->handles(mmio_off)) {
            s.crypto->write8(mmio_off, val & 0xFF);
            s.crypto->write8(mmio_off+1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            s.fb->write8(mmio_off, val & 0xFF);
            s.fb->write8(mmio_off + 1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            s.timer->write8(mmio_off, val & 0xFF);
            s.timer->write8(mmio_off + 1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            s.rtc->write8(mmio_off, val & 0xFF);
            s.rtc->write8(mmio_off + 1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        if (uart_geom_span(mmio_off, 2) &&
                s.uart_geom->handles(mmio_off)) {
            s.uart_geom->write8(mmio_off, val & 0xFF);
            s.uart_geom->write8(mmio_off + 1, (val >> 8) & 0xFF);
            icache_invalidate_span(s, addr, 2);
            return;
        }
        cb.mmio_write8(addr, val & 0xFF);
        cb.mmio_write8(addr+1, (val >> 8) & 0xFF);
        icache_invalidate_span(s, addr, 2);
        return;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    mem_write16(s, addr, val);
}

static inline uint32_t sys_read32(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        return static_cast<uint32_t>(cb.bus_access->access(
            BusOperation::READ,
            addr,
            BusWidth::WORD,
            0,
            port_io));
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 4))
                throw std::runtime_error("TRAP:BUS_FAULT");
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.trng->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.crypto->handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.crypto->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.fb->handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.fb->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.timer->handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.timer->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.rtc->handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.rtc->read8(mmio_off + i) << (8*i);
            return v;
        }
        if (uart_geom_span(mmio_off, 4) &&
                s.uart_geom->handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.uart_geom->read8(mmio_off + i) << (8*i);
            return v;
        }
        uint32_t v = 0;
        for (int i = 0; i < 4; i++)
            v |= (uint32_t)cb.mmio_read8(addr + i) << (8*i);
        return v;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    return mem_read32(s, addr);
}

static inline void sys_write32(
        CPUState& s,
        const StepCallbacks& cb,
        uint64_t addr,
        uint32_t val,
        bool port_io = false) {
    if (cb.bus_access != nullptr) {
        preflight_resumable_bus_access(s, addr);
        cb.bus_access->access(
            BusOperation::WRITE,
            addr,
            BusWidth::WORD,
            val,
            port_io);
        icache_invalidate_span(s, addr, 4);
        return;
    }
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (write_native_nic_bytes(
                s, cb, mmio_off, val, 4)) {
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (s.trng->handles(mmio_off)) {
            if (!s.trng->handles_span(mmio_off, 4))
                throw std::runtime_error("TRAP:BUS_FAULT");
            for (int i = 0; i < 4; i++)
                s.trng->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (s.crypto->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.crypto->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.fb->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.timer->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.rtc->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        if (uart_geom_span(mmio_off, 4) &&
                s.uart_geom->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.uart_geom->write8(
                    mmio_off + i, (val >> (8*i)) & 0xFF);
            icache_invalidate_span(s, addr, 4);
            return;
        }
        for (int i = 0; i < 4; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
        icache_invalidate_span(s, addr, 4);
        return;
    }
    if (s.priv_level) {
        if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
            s.trap_addr = addr; throw std::runtime_error("TRAP:PRIV_FAULT");
        }
        mpu_check(s, addr);
    }
    mem_write32(s, addr, val);
}

static inline uint8_t direct_system_memory_read8(
        CPUState& s,
        uint64_t address) {
    preflight_resumable_bus_access(s, address);
    if (!s.priv_level &&
        s.memory->hbw_mem &&
        region_contains(
            s.memory->hbw_base,
            s.memory->hbw_size,
            address)) {
        return s.memory->hbw_mem[
            address - s.memory->hbw_base];
    }
    if (s.memory->ext_mem &&
        region_contains(
            s.memory->ext_mem_base,
            s.memory->ext_mem_size,
            address)) {
        return s.memory->ext_mem[
            address - s.memory->ext_mem_base];
    }
    if (s.memory->vram_mem &&
        region_contains(
            s.memory->vram_base,
            s.memory->vram_size,
            address)) {
        return s.memory->vram_mem[
            address - s.memory->vram_base];
    }
    return mem_read8(s, address);
}

static inline void direct_system_memory_write8(
        CPUState& s,
        uint64_t address,
        uint8_t value) {
    preflight_resumable_bus_access(s, address);
    if (!s.priv_level &&
        s.memory->hbw_mem &&
        region_contains(
            s.memory->hbw_base,
            s.memory->hbw_size,
            address)) {
        s.memory->hbw_mem[
            address - s.memory->hbw_base] = value;
        icache_invalidate_span(s, address, 1);
        return;
    }
    if (s.memory->ext_mem &&
        region_contains(
            s.memory->ext_mem_base,
            s.memory->ext_mem_size,
            address)) {
        s.memory->ext_mem[
            address - s.memory->ext_mem_base] = value;
        icache_invalidate_span(s, address, 1);
        return;
    }
    if (s.memory->vram_mem &&
        region_contains(
            s.memory->vram_base,
            s.memory->vram_size,
            address)) {
        s.memory->vram_mem[
            address - s.memory->vram_base] = value;
        icache_invalidate_span(s, address, 1);
        return;
    }
    mem_write8(s, address, value);
}

struct DmaTargetAccess {
    std::optional<uint64_t> read_value;
    bool target_effects_committed = false;
};

static uint8_t* resolve_dma_memory_byte(
        MemoryMappings& memory,
        uint64_t address,
        bool allow_vram) {
    if (allow_vram &&
        memory.vram_mem &&
        region_contains(
            memory.vram_base,
            memory.vram_size,
            address)) {
        return memory.vram_mem +
            (address - memory.vram_base);
    }
    if (memory.hbw_mem &&
        region_contains(
            memory.hbw_base,
            memory.hbw_size,
            address)) {
        return memory.hbw_mem +
            (address - memory.hbw_base);
    }
    if (memory.ext_mem &&
        region_contains(
            memory.ext_mem_base,
            memory.ext_mem_size,
            address)) {
        return memory.ext_mem +
            (address - memory.ext_mem_base);
    }
    if (memory.mem && address < memory.mem_size)
        return memory.mem + address;
    return nullptr;
}

static DmaTargetAccess execute_dma_bus_target(
        SystemState& system,
        const BusGrant& grant) {
    const BusRequest& request = grant.request;
    if (request.width != BusWidth::BYTE ||
        request.ordering.port_io) {
        throw std::logic_error(
            "DMA target received a non-byte or port-I/O request");
    }

    // The selected compatibility contract for native NIC DMA has always
    // returned zero for an unmapped read and dropped an unmapped write.
    // Although the RTL fabric classifies the top MMIO aperture by address,
    // making DMA invoke arbitrary MMIO side effects would be a separate
    // guest-visible architecture decision.  Preserve the native behavior
    // while still accounting for the byte beat on its real physical port.
    if (grant.target == BusTarget::MMIO) {
        if (request.operation == BusOperation::READ)
            return {uint64_t{0}, false};
        return {std::nullopt, false};
    }

    uint8_t* byte = resolve_dma_memory_byte(
        system.shared_memory,
        request.address,
        request.requester_id ==
            SystemState::DISK_DMA_REQUESTER_ID);
    if (request.operation == BusOperation::READ) {
        return {
            byte
                ? std::optional<uint64_t>{*byte}
                : std::optional<uint64_t>{uint64_t{0}},
            byte != nullptr,
        };
    }
    if (byte != nullptr) {
        *byte = static_cast<uint8_t>(
            request.write_data);
        return {std::nullopt, true};
    }
    return {std::nullopt, false};
}

static std::optional<uint64_t> execute_granted_bus_target(
        CPUState& s,
        const StepCallbacks& callbacks,
        const BusGrant& grant) {
    const BusRequest& request = grant.request;
    StepCallbacks direct_callbacks = callbacks;
    direct_callbacks.bus_access = nullptr;
    direct_callbacks.strict_cycle_dma = true;

    if (grant.target == BusTarget::MMIO) {
        if (request.operation == BusOperation::READ) {
            switch (request.width) {
                case BusWidth::BYTE:
                    return sys_read8(
                        s,
                        direct_callbacks,
                        request.address,
                        request.ordering.port_io);
                case BusWidth::HALF:
                    return sys_read16(
                        s,
                        direct_callbacks,
                        request.address,
                        request.ordering.port_io);
                case BusWidth::WORD:
                    return sys_read32(
                        s,
                        direct_callbacks,
                        request.address,
                        request.ordering.port_io);
                case BusWidth::DOUBLEWORD:
                    return sys_read64(
                        s,
                        direct_callbacks,
                        request.address,
                        request.ordering.port_io);
            }
        } else {
            switch (request.width) {
                case BusWidth::BYTE:
                    sys_write8(
                        s,
                        direct_callbacks,
                        request.address,
                        static_cast<uint8_t>(request.write_data),
                        request.ordering.port_io);
                    break;
                case BusWidth::HALF:
                    sys_write16(
                        s,
                        direct_callbacks,
                        request.address,
                        static_cast<uint16_t>(request.write_data),
                        request.ordering.port_io);
                    break;
                case BusWidth::WORD:
                    sys_write32(
                        s,
                        direct_callbacks,
                        request.address,
                        static_cast<uint32_t>(request.write_data),
                        request.ordering.port_io);
                    break;
                case BusWidth::DOUBLEWORD:
                    sys_write64(
                        s,
                        direct_callbacks,
                        request.address,
                        request.write_data,
                        request.ordering.port_io);
                    break;
            }
            return std::nullopt;
        }
    } else {
        preflight_resumable_bus_access(s, request.address);
        if (request.operation == BusOperation::READ) {
            switch (request.width) {
                case BusWidth::BYTE:
                    return direct_system_memory_read8(
                        s, request.address);
                case BusWidth::HALF:
                    return mem_read16(s, request.address);
                case BusWidth::WORD:
                    return mem_read32(s, request.address);
                case BusWidth::DOUBLEWORD:
                    return mem_read64(s, request.address);
            }
        } else {
            switch (request.width) {
                case BusWidth::BYTE:
                    direct_system_memory_write8(
                        s,
                        request.address,
                        static_cast<uint8_t>(request.write_data));
                    break;
                case BusWidth::HALF:
                    mem_write16(
                        s,
                        request.address,
                        static_cast<uint16_t>(request.write_data));
                    break;
                case BusWidth::WORD:
                    mem_write32(
                        s,
                        request.address,
                        static_cast<uint32_t>(request.write_data));
                    break;
                case BusWidth::DOUBLEWORD:
                    mem_write64(
                        s,
                        request.address,
                        request.write_data);
                    break;
            }
            return std::nullopt;
        }
    }

    throw std::logic_error("main bus target width is invalid");
}

// Push/pop through MMIO-aware writes
static inline void sys_push64(CPUState& s, const StepCallbacks& cb, uint64_t val) {
    sp(s) -= 8;
    sys_write64(s, cb, sp(s), val);
}

static inline uint64_t sys_pop64(CPUState& s, const StepCallbacks& cb) {
    uint64_t val = sys_read64(s, cb, sp(s));
    sp(s) += 8;
    return val;
}

// ---------------------------------------------------------------------------
//  EXT.STRING (F9) — native C++ implementation
// ---------------------------------------------------------------------------
//  Sub-ops: 00=CMOVE, 01=CMOVE>, 02=BFILL, 03=BCOMP, 04=BSRCH
//  Encoding: F9 <sub-op> <reg-byte[Rd:4][Rs:4]>
//  REX extends Rd/Rs to R16-R31 via ext_modifier set before entry.

static int exec_string(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op   = fetch8(s);
    uint8_t reg_byte = fetch8(s);
    int rd = (rex_d(s.ext_modifier) << 4) | ((reg_byte >> 4) & 0xF);
    int rs = (rex_s(s.ext_modifier) << 4) | (reg_byte & 0xF);

    switch (sub_op) {

    case 0x00: // CMOVE — forward byte copy, len in R0
    case 0x01: // CMOVE> — backward byte copy, len in R0
    {
        uint64_t src = s.regs[rs];
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[0];
        if (ln > 0) {
            if (sub_op == 0x00) {
                for (uint64_t i = 0; i < ln; i++)
                    sys_write8(s, cb, dst + i, sys_read8(s, cb, src + i));
            } else {
                for (uint64_t i = ln; i-- > 0; )
                    sys_write8(s, cb, dst + i, sys_read8(s, cb, src + i));
            }
        }
        s.regs[rs] = src + ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return (int)ln + 2;
    }

    case 0x02: // BFILL — fill block with D[7:0], len in Rs
    {
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[rs];
        uint8_t  fb  = s.d_reg;
        // Fast path: memset when entirely within one RAM region (not MMIO)
        bool in_mmio = cb.has_mmio && dst >= cb.mmio_start && dst < cb.mmio_end;
        // The direct memset shortcut is supervisor-only: user-mode writes
        // must retain the scalar path's MPU checks.  Use subtraction-based
        // bounds so an attacker-controlled length cannot wrap off + len and
        // turn a rejected span into an out-of-bounds host memset.
        if (cb.bus_access == nullptr && !s.priv_level && !in_mmio) {
            auto r = resolve_mem(s, dst);
            if (r.buf && region_span_fits(r.size, r.off, ln)) {
                std::memset(r.buf + r.off, fb, (size_t)ln);
                icache_invalidate_span(s, dst, ln);
                s.regs[rd] = dst + ln;
                s.regs[rs] = 0;
                return (int)ln + 2;
            }
        }
        for (uint64_t i = 0; i < ln; i++)
            sys_write8(s, cb, dst + i, fb);
        s.regs[rd] = dst + ln;
        s.regs[rs] = 0;
        return (int)ln + 2;
    }

    case 0x03: // BCOMP — byte compare, len in R0
    {
        uint64_t src = s.regs[rs];
        uint64_t dst = s.regs[rd];
        uint64_t ln  = s.regs[0];
        int cycles = 2;
        uint64_t remaining = ln;
        for (uint64_t i = 0; i < ln; i++) {
            uint8_t sb = sys_read8(s, cb, src + i);
            uint8_t db = sys_read8(s, cb, dst + i);
            cycles++;
            remaining--;
            if (sb != db) {
                s.flag_z = 0;
                s.flag_g = (db > sb) ? 1 : 0;
                s.regs[rs] = src + i;
                s.regs[rd] = dst + i;
                s.regs[0]  = remaining + 1;
                return cycles;
            }
        }
        // All equal
        s.flag_z = 1;
        s.flag_g = 0;
        s.regs[rs] = src + ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return cycles;
    }

    case 0x04: // BSRCH — search for D[7:0] in block at Rd, len in R0
    {
        uint64_t dst    = s.regs[rd];
        uint64_t ln     = s.regs[0];
        uint8_t  needle = s.d_reg;
        int cycles = 2;
        for (uint64_t i = 0; i < ln; i++) {
            uint8_t b = sys_read8(s, cb, dst + i);
            cycles++;
            if (b == needle) {
                s.flag_z = 1;  // found
                s.regs[rs] = i;          // offset
                s.regs[rd] = dst + i;
                s.regs[0]  = ln - i;
                return cycles;
            }
        }
        // Not found
        s.flag_z = 0;
        s.regs[rs] = ln;
        s.regs[rd] = dst + ln;
        s.regs[0]  = 0;
        return cycles;
    }

    default:
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.STRING reserved sub-op");
    }
}

// ---------------------------------------------------------------------------
//  EXT.DICT (FA) — native C++ implementation
// ---------------------------------------------------------------------------
//  Sub-ops: 00=DFIND, 01=DINS, 02=DDEL, 03=DCLR
//  Encoding: FA <sub-op> <reg-byte[Rd:4][Rs:4]>
//  REX extends Rd/Rs to R16-R31 via ext_modifier set before entry.
//
//  Hash table: 64 sets × 4 ways, FNV-1a 32-bit hash.
//  Name is a counted string in memory: 1 byte len (5-bit) + len name bytes.

static inline uint32_t fnv1a_32(const uint8_t* data, size_t len) {
    uint32_t h = 0x811C9DC5u;
    for (size_t i = 0; i < len; i++)
        h = (h ^ data[i]) * 0x01000193u;
    return h;
}

// Read a counted-string from guest memory.  Returns name length and cycles.
static inline int dict_read_name(CPUState& s, const StepCallbacks& cb,
                                  uint64_t addr, uint8_t* out_name,
                                  uint8_t& out_len, uint32_t& out_hash) {
    uint8_t raw_len = sys_read8(s, cb, addr) & 0x1F;  // 5-bit, max 31
    out_len = raw_len;
    for (int i = 0; i < raw_len; i++)
        out_name[i] = sys_read8(s, cb, addr + 1 + i);
    out_hash = fnv1a_32(out_name, raw_len);
    return 2 + raw_len;  // 1 len byte + N name bytes + 1 hash cycle
}

static int exec_dict(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op   = fetch8(s);
    uint8_t reg_byte = fetch8(s);
    int rd = (rex_d(s.ext_modifier) << 4) | ((reg_byte >> 4) & 0xF);
    int rs = (rex_s(s.ext_modifier) << 4) | (reg_byte & 0xF);

    switch (sub_op) {

    case 0x00: { // DFIND — lookup counted-string at R[rs]; R[rd] ← XT if found
        uint64_t addr = s.regs[rs];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = s.dict_table[set_idx][w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                s.regs[rd] = e.xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        s.regs[rd] = 0;
        s.flag_z = 0;
        s.flag_v = 0;
        return cycles;
    }

    case 0x01: { // DINS — insert name at R[rs] with XT from R[rd]
        uint64_t addr = s.regs[rs];
        uint64_t xt   = s.regs[rd];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        auto* ways = s.dict_table[set_idx];
        // Update existing match
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                e.xt = xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        // Insert into first empty way
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (!e.valid) {
                e.valid = true;
                e.hash = h;
                e.name_len = nlen;
                std::memcpy(e.name, name, nlen);
                e.xt = xt;
                s.flag_z = 1;
                s.flag_v = 0;
                return cycles;
            }
        }
        // Set full — overflow
        s.flag_z = 0;
        s.flag_v = 1;
        return cycles;
    }

    case 0x02: { // DDEL — delete entry matching name at R[rs]
        uint64_t addr = s.regs[rs];
        uint8_t name[31];
        uint8_t nlen;
        uint32_t h;
        int cycles = dict_read_name(s, cb, addr, name, nlen, h);
        int set_idx = h & 0x3F;
        auto* ways = s.dict_table[set_idx];
        for (int w = 0; w < CPUState::DICT_WAYS; w++) {
            auto& e = ways[w];
            if (e.valid && e.hash == h && e.name_len == nlen
                && std::memcmp(e.name, name, nlen) == 0) {
                e.valid = false;
                e.hash = 0;
                e.name_len = 0;
                e.xt = 0;
                s.flag_z = 1;
                return cycles;
            }
        }
        s.flag_z = 0;
        return cycles;
    }

    case 0x03: { // DCLR — clear entire hash table
        s.dict_clear_all();
        return 66;  // ~64 cycles for bulk clear
    }

    default:
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.DICT reserved sub-op");
    }
}

// ---------------------------------------------------------------------------
//  EXT.CRYPTO (prefix FB) — per-core crypto ISA instructions
// ---------------------------------------------------------------------------

// CRC polynomials (normal / MSB-first, non-reflected form): mode 0 uses
// CRC-32/BZIP2 parameters, mode 1 uses Castagnoli non-reflected, and mode 2
// uses CRC-64/WE parameters.
static constexpr uint32_t CRC32_POLY   = 0x04C11DB7u;
static constexpr uint32_t CRC32C_POLY  = 0x1EDC6F41u;
static constexpr uint64_t CRC64_POLY   = 0x42F0E1EBA9EA3693ull;

static inline uint32_t crc_byte_32(uint32_t acc, uint8_t b, uint32_t poly) {
    acc ^= (uint32_t)b << 24;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x80000000u)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

static inline uint64_t crc_byte_64(uint64_t acc, uint8_t b, uint64_t poly) {
    acc ^= (uint64_t)b << 56;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x8000000000000000ull)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

// ---------------------------------------------------------------------------
//  SHA-256 / SHA-512 helpers
// ---------------------------------------------------------------------------

static inline uint32_t rotr32(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }
static inline uint64_t rotr64(uint64_t x, int n) { return (x >> n) | (x << (64 - n)); }

static constexpr uint32_t ISA_SHA256_K[64] = {
    0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
    0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
    0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
    0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
    0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
    0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
    0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
    0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2,
};

static constexpr uint64_t ISA_SHA512_K[80] = {
    0x428a2f98d728ae22ull,0x7137449123ef65cdull,0xb5c0fbcfec4d3b2full,0xe9b5dba58189dbbcull,
    0x3956c25bf348b538ull,0x59f111f1b605d019ull,0x923f82a4af194f9bull,0xab1c5ed5da6d8118ull,
    0xd807aa98a3030242ull,0x12835b0145706fbeull,0x243185be4ee4b28cull,0x550c7dc3d5ffb4e2ull,
    0x72be5d74f27b896full,0x80deb1fe3b1696b1ull,0x9bdc06a725c71235ull,0xc19bf174cf692694ull,
    0xe49b69c19ef14ad2ull,0xefbe4786384f25e3ull,0x0fc19dc68b8cd5b5ull,0x240ca1cc77ac9c65ull,
    0x2de92c6f592b0275ull,0x4a7484aa6ea6e483ull,0x5cb0a9dcbd41fbd4ull,0x76f988da831153b5ull,
    0x983e5152ee66dfabull,0xa831c66d2db43210ull,0xb00327c898fb213full,0xbf597fc7beef0ee4ull,
    0xc6e00bf33da88fc2ull,0xd5a79147930aa725ull,0x06ca6351e003826full,0x142929670a0e6e70ull,
    0x27b70a8546d22ffcull,0x2e1b21385c26c926ull,0x4d2c6dfc5ac42aedull,0x53380d139d95b3dfull,
    0x650a73548baf63deull,0x766a0abb3c77b2a8ull,0x81c2c92e47edaee6ull,0x92722c851482353bull,
    0xa2bfe8a14cf10364ull,0xa81a664bbc423001ull,0xc24b8b70d0f89791ull,0xc76c51a30654be30ull,
    0xd192e819d6ef5218ull,0xd69906245565a910ull,0xf40e35855771202aull,0x106aa07032bbd1b8ull,
    0x19a4c116b8d2d0c8ull,0x1e376c085141ab53ull,0x2748774cdf8eeb99ull,0x34b0bcb5e19b48a8ull,
    0x391c0cb3c5c95a63ull,0x4ed8aa4ae3418acbull,0x5b9cca4f7763e373ull,0x682e6ff3d6b2b8a3ull,
    0x748f82ee5defb2fcull,0x78a5636f43172f60ull,0x84c87814a1f0ab72ull,0x8cc702081a6439ecull,
    0x90befffa23631e28ull,0xa4506cebde82bde9ull,0xbef9a3f7b2c67915ull,0xc67178f2e372532bull,
    0xca273eceea26619cull,0xd186b8c721c0c207ull,0xeada7dd6cde0eb1eull,0xf57d4f7fee6ed178ull,
    0x06f067aa72176fbaull,0x0a637dc5a2c898a6ull,0x113f9804bef90daeull,0x1b710b35131c471bull,
    0x28db77f523047d84ull,0x32caab7b40c72493ull,0x3c9ebe0a15c9bebcull,0x431d67c49c100d4cull,
    0x4cc5d4becb3e42b6ull,0x597f299cfc657e2aull,0x5fcb6fab3ad6faecull,0x6c44198c4a475817ull,
};

static constexpr uint32_t ISA_SHA256_IV[8] = {
    0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,
    0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19,
};
static constexpr uint64_t ISA_SHA384_IV[8] = {
    0xcbbb9d5dc1059ed8ull,0x629a292a367cd507ull,0x9159015a3070dd17ull,0x152fecd8f70e5939ull,
    0x67332667ffc00b31ull,0x8eb44a8768581511ull,0xdb0c2e0d64f98fa7ull,0x47b5481dbefa4fa4ull,
};
static constexpr uint64_t ISA_SHA512_IV[8] = {
    0x6a09e667f3bcc908ull,0xbb67ae8584caa73bull,0x3c6ef372fe94f82bull,0xa54ff53a5f1d36f1ull,
    0x510e527fade682d1ull,0x9b05688c2b3e6c1full,0x1f83d9abfb41bd6bull,0x5be0cd19137e2179ull,
};

// Unpack 8 hash words from ACC0-ACC3 (+ regs[16-19] for SHA-512)
static void sha_unpack(CPUState& s, uint64_t H[8]) {
    if (s.sha_mode == 0) {
        H[0] = (s.acc[0] >> 32) & 0xFFFFFFFFu; H[1] = s.acc[0] & 0xFFFFFFFFu;
        H[2] = (s.acc[1] >> 32) & 0xFFFFFFFFu; H[3] = s.acc[1] & 0xFFFFFFFFu;
        H[4] = (s.acc[2] >> 32) & 0xFFFFFFFFu; H[5] = s.acc[2] & 0xFFFFFFFFu;
        H[6] = (s.acc[3] >> 32) & 0xFFFFFFFFu; H[7] = s.acc[3] & 0xFFFFFFFFu;
    } else {
        H[0] = s.acc[0]; H[1] = s.acc[1]; H[2] = s.acc[2]; H[3] = s.acc[3];
        H[4] = s.regs[16]; H[5] = s.regs[17]; H[6] = s.regs[18]; H[7] = s.regs[19];
    }
}

// Pack 8 hash words back into ACC0-ACC3 (+ regs[16-19] for SHA-512)
static void sha_pack(CPUState& s, const uint64_t H[8]) {
    if (s.sha_mode == 0) {
        s.acc[0] = ((H[0] & 0xFFFFFFFFu) << 32) | (H[1] & 0xFFFFFFFFu);
        s.acc[1] = ((H[2] & 0xFFFFFFFFu) << 32) | (H[3] & 0xFFFFFFFFu);
        s.acc[2] = ((H[4] & 0xFFFFFFFFu) << 32) | (H[5] & 0xFFFFFFFFu);
        s.acc[3] = ((H[6] & 0xFFFFFFFFu) << 32) | (H[7] & 0xFFFFFFFFu);
    } else {
        s.acc[0] = H[0]; s.acc[1] = H[1]; s.acc[2] = H[2]; s.acc[3] = H[3];
        s.regs[16] = H[4]; s.regs[17] = H[5]; s.regs[18] = H[6]; s.regs[19] = H[7];
    }
}

static int sha_block_size(CPUState& s) { return s.sha_mode >= 1 ? 128 : 64; }

// Read one block from memory at TSRC0. The full-core RTL fetches each
// 64-byte SHA-256 block as eight 64-bit main-bus transactions.
static void sha_read_block(
        CPUState& s,
        const StepCallbacks& cb,
        uint8_t* block) {
    const int bsz = sha_block_size(s);
    for (int offset = 0; offset < bsz; offset += 8) {
        const uint64_t word =
            sys_read64(s, cb, s.tsrc0 + offset);
        for (int byte = 0; byte < 8; byte++) {
            block[offset + byte] = static_cast<uint8_t>(
                word >> (byte * 8));
        }
    }
}

// SHA-256 compression (one 64-byte block)
static void sha256_compress(uint64_t H[8], const uint8_t block[64]) {
    uint32_t W[64];
    for (int i = 0; i < 16; i++)
        W[i] = ((uint32_t)block[i*4] << 24) | ((uint32_t)block[i*4+1] << 16) |
               ((uint32_t)block[i*4+2] << 8) | block[i*4+3];
    for (int t = 16; t < 64; t++) {
        uint32_t s0 = rotr32(W[t-15],7) ^ rotr32(W[t-15],18) ^ (W[t-15] >> 3);
        uint32_t s1 = rotr32(W[t-2],17) ^ rotr32(W[t-2],19)  ^ (W[t-2] >> 10);
        W[t] = W[t-16] + s0 + W[t-7] + s1;
    }
    uint32_t a=(uint32_t)H[0], b=(uint32_t)H[1], c=(uint32_t)H[2], d=(uint32_t)H[3];
    uint32_t e=(uint32_t)H[4], f=(uint32_t)H[5], g=(uint32_t)H[6], h=(uint32_t)H[7];
    for (int t = 0; t < 64; t++) {
        uint32_t S1 = rotr32(e,6) ^ rotr32(e,11) ^ rotr32(e,25);
        uint32_t ch = (e & f) ^ ((~e) & g);
        uint32_t temp1 = h + S1 + ch + ISA_SHA256_K[t] + W[t];
        uint32_t S0 = rotr32(a,2) ^ rotr32(a,13) ^ rotr32(a,22);
        uint32_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint32_t temp2 = S0 + maj;
        h=g; g=f; f=e; e=d+temp1; d=c; c=b; b=a; a=temp1+temp2;
    }
    H[0] = ((uint32_t)H[0]+a) & 0xFFFFFFFFu;
    H[1] = ((uint32_t)H[1]+b) & 0xFFFFFFFFu;
    H[2] = ((uint32_t)H[2]+c) & 0xFFFFFFFFu;
    H[3] = ((uint32_t)H[3]+d) & 0xFFFFFFFFu;
    H[4] = ((uint32_t)H[4]+e) & 0xFFFFFFFFu;
    H[5] = ((uint32_t)H[5]+f) & 0xFFFFFFFFu;
    H[6] = ((uint32_t)H[6]+g) & 0xFFFFFFFFu;
    H[7] = ((uint32_t)H[7]+h) & 0xFFFFFFFFu;
}

// SHA-512 compression (one 128-byte block)
static void sha512_compress(uint64_t H[8], const uint8_t block[128]) {
    uint64_t W[80];
    for (int i = 0; i < 16; i++) {
        uint64_t v = 0;
        for (int j = 0; j < 8; j++)
            v = (v << 8) | block[i*8+j];
        W[i] = v;
    }
    for (int t = 16; t < 80; t++) {
        uint64_t s0 = rotr64(W[t-15],1) ^ rotr64(W[t-15],8) ^ (W[t-15] >> 7);
        uint64_t s1 = rotr64(W[t-2],19) ^ rotr64(W[t-2],61) ^ (W[t-2] >> 6);
        W[t] = W[t-16] + s0 + W[t-7] + s1;
    }
    uint64_t a=H[0],b=H[1],c=H[2],d=H[3],e=H[4],f=H[5],g=H[6],h=H[7];
    for (int t = 0; t < 80; t++) {
        uint64_t S1 = rotr64(e,14) ^ rotr64(e,18) ^ rotr64(e,41);
        uint64_t ch = (e & f) ^ ((~e) & g);
        uint64_t temp1 = h + S1 + ch + ISA_SHA512_K[t] + W[t];
        uint64_t S0 = rotr64(a,28) ^ rotr64(a,34) ^ rotr64(a,39);
        uint64_t maj = (a & b) ^ (a & c) ^ (b & c);
        uint64_t temp2 = S0 + maj;
        h=g; g=f; f=e; e=d+temp1; d=c; c=b; b=a; a=temp1+temp2;
    }
    H[0]+=a; H[1]+=b; H[2]+=c; H[3]+=d;
    H[4]+=e; H[5]+=f; H[6]+=g; H[7]+=h;
}

// Run compression on block at M[TSRC0], return cycle count
static int sha_compress(
        CPUState& s,
        const StepCallbacks& cb) {
    uint64_t H[8];
    sha_unpack(s, H);
    uint8_t block[128];
    sha_read_block(s, cb, block);
    if (s.sha_mode == 0) {
        sha256_compress(H, block);
        sha_pack(s, H);
        s.flag_z = 1;
        return 64;
    } else {
        sha512_compress(H, block);
        sha_pack(s, H);
        s.flag_z = 1;
        return 80;
    }
}

// Write FIPS 180-4 padding at M[TSRC0+R0], return true if two-block pad
static bool sha_write_pad(
        CPUState& s,
        const StepCallbacks& cb) {
    int bsz = sha_block_size(s);
    int lsz = s.sha_mode >= 1 ? 16 : 8;
    int pos = (int)(s.regs[0] & 0xFFFFFFFFull) % bsz;
    uint64_t base = s.tsrc0;

    sys_write8(s, cb, base + pos, 0x80);
    pos++;

    bool two_blocks = pos > (bsz - lsz);
    if (two_blocks) {
        while (pos < bsz) {
            sys_write8(s, cb, base + pos, 0x00);
            pos++;
        }
        s.flag_c = 1;
        return true;
    }
    // zero-fill
    while (pos < bsz - lsz) {
        sys_write8(s, cb, base + pos, 0x00);
        pos++;
    }
    // big-endian length
    uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
    if (s.sha_mode >= 1) {
        for (int i = 0; i < 8; i++)
            sys_write8(
                s,
                cb,
                base + bsz - 16 + i,
                static_cast<uint8_t>(hi >> (56 - i * 8)));
    }
    for (int i = 0; i < 8; i++)
        sys_write8(
            s,
            cb,
            base + bsz - 8 + i,
            static_cast<uint8_t>(lo >> (56 - i * 8)));
    s.flag_c = 0;
    return false;
}

// ---------------------------------------------------------------------------
//  Field ALU ISA helpers (§B.5)
// ---------------------------------------------------------------------------

static const BigNum GF_BUILTIN_PRIMES[3] = {
    make_curve25519_p(),
    make_secp256k1_p(),
    make_p256_p(),
};

static BigNum gf_get_prime(const CPUState& s) {
    if (s.gf_prime_sel < 3) return GF_BUILTIN_PRIMES[s.gf_prime_sel];
    if (s.gf_prime_sel == 3 && !s.gf_custom_p.is_zero()) return s.gf_custom_p;
    return GF_BUILTIN_PRIMES[0];
}

static bool gf_is_mont(const CPUState& s) {
    return s.gf_prime_sel == 3 && !s.gf_mont_pinv.is_zero();
}

static BigNum gf_acc_to_bignum(const CPUState& s) {
    BigNum r;
    r.w[0] = s.acc[0]; r.w[1] = s.acc[1];
    r.w[2] = s.acc[2]; r.w[3] = s.acc[3];
    return r;
}

static void gf_bignum_to_acc(CPUState& s, const BigNum& v) {
    s.acc[0] = v.w[0]; s.acc[1] = v.w[1];
    s.acc[2] = v.w[2]; s.acc[3] = v.w[3];
}

static BigNum gf_read_tile_b(
        CPUState& s,
        const StepCallbacks& cb) {
    BigNum value;
    uint64_t base = s.tsrc0;
    for (int limb = 0; limb < 4; limb++) {
        value.w[limb] = sys_read64(
            s,
            cb,
            base + static_cast<uint64_t>(limb * 8));
    }
    return value;
}

static void gf_write_tile_dst(
        CPUState& s,
        const StepCallbacks& cb,
        const BigNum& v) {
    uint64_t base = s.tdst;
    for (int limb = 0; limb < 4; limb++) {
        sys_write64(
            s,
            cb,
            base + static_cast<uint64_t>(limb * 8),
            v.w[limb]);
    }
}

static BigNum gf_mulmod_sel(const CPUState& s, const BigNum& a, const BigNum& b, const BigNum& p) {
    if (gf_is_mont(s)) return bn_mont_mulmod(a, b, p, s.gf_mont_pinv);
    return bn_mulmod(a, b, p);
}

static BigNum gf_sqrmod_sel(const CPUState& s, const BigNum& a, const BigNum& p) {
    if (gf_is_mont(s)) return bn_mont_sqrmod(a, p, s.gf_mont_pinv);
    return bn_sqrmod(a, p);
}

static int exec_field(CPUState& s) {
    // Called with op = sub_op & 0xF already extracted by caller
    // We re-read the op from the caller's context — but actually,
    // let's take it as a parameter.
    return 0; // placeholder, see exec_crypto dispatch below
}

static int exec_crypto(CPUState& s, const StepCallbacks& cb) {
    uint8_t sub_op = fetch8(s);
    int unit = (sub_op >> 4) & 0xF;
    int op   = sub_op & 0xF;

    if (unit == 0x0) {
        // --- CRC unit ---
        bool is64 = (s.crc_mode == 2);
        uint32_t poly32 = (s.crc_mode == 1) ? CRC32C_POLY : CRC32_POLY;

        switch (op) {
        case 0x0: { // CRC.INIT
            s.crc_acc = is64 ? 0xFFFFFFFFFFFFFFFFull : 0xFFFFFFFFu;
            return 1;
        }
        case 0x1: { // CRC.B Rd, Rs — feed one byte
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint8_t b = (uint8_t)(s.regs[rs] & 0xFF);
            if (is64)
                s.crc_acc = crc_byte_64(s.crc_acc, b, CRC64_POLY);
            else
                s.crc_acc = crc_byte_32((uint32_t)s.crc_acc, b, poly32);
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        case 0x2: { // CRC.Q Rd, Rs — feed 8 bytes (LE order)
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint64_t val = s.regs[rs];
            if (is64) {
                uint64_t acc = s.crc_acc;
                for (int i = 0; i < 8; i++)
                    acc = crc_byte_64(acc, (uint8_t)(val >> (i * 8)), CRC64_POLY);
                s.crc_acc = acc;
            } else {
                uint32_t acc = (uint32_t)s.crc_acc;
                for (int i = 0; i < 8; i++)
                    acc = crc_byte_32(acc, (uint8_t)(val >> (i * 8)), poly32);
                s.crc_acc = acc;
            }
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        case 0x3: { // CRC.FIN Rd, Rs — finalize
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            uint64_t mask = is64 ? 0xFFFFFFFFFFFFFFFFull : 0xFFFFFFFFu;
            s.crc_acc ^= mask;
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        case 0x4: { // CRC.MODE imm8
            uint8_t imm = fetch8(s);
            s.crc_mode = (imm == 1 || imm == 2) ? imm : 0;
            return 1;
        }
        case 0x5: { // CRC.SEED Rd, Rs — width-masked accumulator load
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            s.crc_acc = is64 ? s.regs[rs] : (uint32_t)s.regs[rs];
            s.regs[rd] = s.crc_acc;
            return 1;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO CRC reserved sub-op");
        }
    } else if (unit == 0x1) {
        // --- SHA-2 unit ---
        switch (op) {
        case 0x0: { // SHA.INIT imm8
            uint8_t imm = fetch8(s);
            s.sha_mode = imm & 0x03;
            s.sha_msglen_lo = 0;
            s.sha_msglen_hi = 0;
            uint64_t H[8];
            if (s.sha_mode == 0) {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA256_IV[i];
            } else if (s.sha_mode == 1) {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA384_IV[i];
            } else {
                for (int i = 0; i < 8; i++) H[i] = ISA_SHA512_IV[i];
            }
            sha_pack(s, H);
            return 2;
        }
        case 0x1: { // SHA.ROUND — compress block at TSRC0
            return sha_compress(s, cb);
        }
        case 0x2: { // SHA.PAD
            sha_write_pad(s, cb);
            return 3;
        }
        case 0x3: { // SHA.DIN Rd, Rs — feed one byte
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint8_t byte_val = (uint8_t)(s.regs[rs] & 0xFF);
            uint64_t base = s.tsrc0;
            uint64_t r0 = s.regs[0];
            sys_write8(s, cb, base + r0, byte_val);
            r0++;
            // track message length in bits
            uint64_t old = s.sha_msglen_lo;
            s.sha_msglen_lo += 8;
            if (s.sha_msglen_lo < old) s.sha_msglen_hi++;
            // auto-round when block is full
            int bsz = sha_block_size(s);
            int cycles = 1;
            if ((int)r0 >= bsz) {
                cycles += sha_compress(s, cb);
                r0 = 0;
            }
            s.regs[0] = r0;
            s.regs[rd] = r0;
            return cycles;
        }
        case 0x4: { // SHA.DOUT Rd, Rs — read hash word
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            int idx = (int)(s.regs[rs]) & 0x7;
            uint64_t H[8];
            sha_unpack(s, H);
            s.regs[rd] = H[idx];
            return 1;
        }
        case 0x5: { // SHA.FINAL — pad + compress
            bool two_blocks = sha_write_pad(s, cb);
            int cycles = 3;
            if (two_blocks) {
                cycles += sha_compress(s, cb);
                // write second pad block (zeros + length)
                int bsz = sha_block_size(s);
                int lsz = s.sha_mode >= 1 ? 16 : 8;
                uint64_t base = s.tsrc0;
                for (int i = 0; i < bsz - lsz; i++)
                    sys_write8(s, cb, base + i, 0x00);
                uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
                if (s.sha_mode >= 1) {
                    for (int i = 0; i < 8; i++) {
                        sys_write8(
                            s,
                            cb,
                            base + bsz - 16 + i,
                            static_cast<uint8_t>(
                                hi >> (56 - i * 8)));
                    }
                }
                for (int i = 0; i < 8; i++) {
                    sys_write8(
                        s,
                        cb,
                        base + bsz - 8 + i,
                        static_cast<uint8_t>(
                            lo >> (56 - i * 8)));
                }
            }
            cycles += sha_compress(s, cb);
            return cycles;
        }
        case 0x6: // SHA.RELEASE — ownership-only; full-core no-op
            return 1;
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO SHA-2 reserved sub-op");
        }
    } else if (unit == 0x2) {
        // --- Field ALU unit (§B.5) ---
        BigNum p = gf_get_prime(s);
        switch (op) {
        case 0x0: { // GF.ADD
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum r = bn_addmod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x1: { // GF.SUB
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum r = bn_submod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x2: { // GF.MUL
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum r = gf_mulmod_sel(s, a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x3: { // GF.SQR
            BigNum a = gf_acc_to_bignum(s);
            BigNum r = gf_sqrmod_sel(s, a, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x4: { // GF.INV
            BigNum a = gf_acc_to_bignum(s);
            BigNum r = bn_invmod(a, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 767;
        }
        case 0x5: { // GF.POW
            BigNum a = gf_acc_to_bignum(s);
            BigNum e = gf_read_tile_b(s, cb);
            BigNum r = bn_powmod(a, e, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 767;
        }
        case 0x6: { // GF.MULR — raw 256×256→512
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum lo, hi;
            BigNum::mul_wide(a, b, lo, hi);
            gf_bignum_to_acc(s, lo);
            gf_write_tile_dst(s, cb, hi);
            s.gf_prev_lo = lo;
            s.gf_prev_hi = hi;
            return 1;
        }
        case 0x7: { // GF.MAC — (ACC * B + prev) mod p
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum ab = gf_mulmod_sel(s, a, b, p);
            BigNum r = bn_addmod(ab, s.gf_prev_lo, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x8: { // GF.MACR — raw: prev_512 + ACC * B
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum mul_lo, mul_hi;
            BigNum::mul_wide(a, b, mul_lo, mul_hi);
            BigNum sum_lo = s.gf_prev_lo.add(mul_lo);
            BigNum sum_hi = s.gf_prev_hi.add(mul_hi);
            if (sum_lo < s.gf_prev_lo) {
                BigNum one; one.w[0] = 1;
                sum_hi = sum_hi.add(one);
            }
            gf_bignum_to_acc(s, sum_lo);
            gf_write_tile_dst(s, cb, sum_hi);
            s.gf_prev_lo = sum_lo;
            s.gf_prev_hi = sum_hi;
            return 1;
        }
        case 0x9: { // GF.CMOV Rd
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            bool cond = s.regs[rd] != 0;
            BigNum b = gf_read_tile_b(s, cb);
            if (cond) {
                gf_bignum_to_acc(s, b);
                s.gf_prev_lo = b;
            }
            return 1;
        }
        case 0xA: { // GF.CEQ — constant-time equality
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s, cb);
            BigNum eq;
            eq.w[0] = (a == b) ? 1 : 0;
            gf_bignum_to_acc(s, eq);
            s.gf_prev_lo = eq;
            s.flag_z = (a == b) ? 1 : 0;
            return 1;
        }
        case 0xB: { // GF.PRIME imm8
            uint8_t imm = fetch8(s);
            s.gf_prime_sel = imm & 0x03;
            return 1;
        }
        case 0xC: { // GF.LDPRIME
            s.gf_custom_p = gf_acc_to_bignum(s);
            s.gf_mont_pinv = gf_read_tile_b(s, cb);
            return 1;
        }
        case 0xD: { // GF.X25519
            BigNum scalar = gf_acc_to_bignum(s);
            BigNum u_coord = gf_read_tile_b(s, cb);
            uint8_t scalar_bytes[32], u_bytes[32];
            scalar.to_le_bytes(scalar_bytes);
            u_coord.to_le_bytes(u_bytes);
            BigNum r = x25519_scalar_mul(scalar_bytes, u_bytes, GF_BUILTIN_PRIMES[0]);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 4335;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO Field ALU reserved sub-op");
        }
    } else {
        throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO reserved unit");
    }
}

// ---------------------------------------------------------------------------
//  The main step function
// ---------------------------------------------------------------------------

struct SystemInstructionTraits {
    bool needs_bus_journal = false;
    bool has_unjournaled_shared_access = false;
    uint64_t unjournaled_cycle_bound = 0;
    bool tacc_python_fallback = false;
    bool tacc_publish_busy = false;
    bool tacc_validation_trap_expected = false;
};

static bool tacc_mode_is_legal(const CPUState& state) noexcept {
    const int ew = state.tmode & 0x7;
    return (
        ew == EW_U8 ||
        ew == EW_U16 ||
        ew == EW_U32 ||
        ew == EW_FP16 ||
        ew == EW_BF16
    );
}

static bool tacc_mode_matches_latched_format(
        const CPUState& state) noexcept {
    if (!tacc_mode_is_legal(state))
        return false;
    const int ew = state.tmode & 0x7;
    const int signed_mode =
        ew == EW_FP16 || ew == EW_BF16
        ? 0
        : (state.tmode >> 4) & 0x1;
    return (
        state.tacc_format_ew == ew &&
        state.tacc_format_signed == signed_mode
    );
}

static bool native_tacc_span_preflight_valid(
        const CPUState& state,
        uint64_t address,
        uint64_t size) noexcept {
    if (
        state.memory == nullptr ||
        size == 0 ||
        size - 1 >
            std::numeric_limits<uint64_t>::max() -
                address
    ) {
        return false;
    }
    constexpr uint64_t TACC_MMIO_START =
        0xFFFF'FF00'0000'0000ULL;
    constexpr uint64_t TACC_MMIO_END =
        0xFFFF'FF80'0000'0000ULL;
    const uint64_t end = address + size;
    if (
        address < TACC_MMIO_END &&
        end > TACC_MMIO_START
    ) {
        return false;
    }

    const auto contains =
        [address, end](
                const uint8_t* memory,
                uint64_t base,
                uint64_t region_size) noexcept {
            if (
                memory == nullptr ||
                region_size == 0 ||
                region_size >
                    std::numeric_limits<uint64_t>::max() -
                        base
            ) {
                return false;
            }
            return (
                base <= address &&
                end <= base + region_size
            );
        };
    return (
        contains(
            state.memory->vram_mem,
            state.memory->vram_base,
            state.memory->vram_size) ||
        contains(
            state.memory->ext_mem,
            state.memory->ext_mem_base,
            state.memory->ext_mem_size) ||
        contains(
            state.memory->hbw_mem,
            state.memory->hbw_base,
            state.memory->hbw_size) ||
        contains(
            state.memory->mem,
            0,
            state.memory->mem_size)
    );
}

static uint64_t native_tamac_cycle_bound(
        const CPUState& state,
        int source_selector) noexcept {
    const int ew = state.tmode & 0x7;
    const bool broadcast = source_selector == 0x1;
    if (ew == EW_U8)
        return broadcast ? 6 : 7;
    if (ew == EW_U16)
        return broadcast ? 4 : 5;
    if (ew == EW_U32)
        return broadcast ? 3 : 4;
    if (ew == EW_FP16 || ew == EW_BF16)
        return broadcast ? 6 : 7;
    return 1;
}

static SystemInstructionTraits native_tacc_instruction_traits(
        CPUState& state,
        uint64_t opcode_address,
        int modifier,
        int subop) {
    const int source_selector = (subop >> 2) & 0x3;
    const int operation = subop & 0x3;
    const uint8_t function_byte =
        icache_peek_byte_without_accounting(
            state,
            opcode_address + 1);

    const bool raw_tamac =
        operation == 0x1 &&
        (
            (
                source_selector != 0x2 &&
                (
                    (function_byte & 0x7) == 0x6 ||
                    (function_byte & 0x7) == 0x7
                )
            ) ||
            (
                source_selector == 0x2 &&
                function_byte == 0x06
            )
        );
    const bool raw_lifecycle =
        modifier == 0x8 &&
        operation == 0x3 &&
        (function_byte & 0x7) >= 0x2;
    if (!raw_tamac && !raw_lifecycle)
        return {};

    const int lifecycle_function = function_byte & 0x7;
    const bool raw_lifecycle_transfer =
        raw_lifecycle &&
        (
            lifecycle_function == 0x4 ||
            lifecycle_function == 0x5
        );
    // A rejected operation reaches only its architected validation boundary:
    // one cycle for lifecycle control, two for TAMAC or an image transfer.
    // Admission below replaces this bound with the exact normal latency.
    SystemInstructionTraits traits{
        false,
        true,
        raw_tamac || raw_lifecycle_transfer
            ? 2ULL
            : 1ULL,
        true,
        false,
        true,
    };
    if (raw_tamac) {
        const bool canonical =
            source_selector != 0x2 &&
            function_byte == 0x06 &&
            (
                source_selector == 0x0 ||
                source_selector == 0x1 ||
                source_selector == 0x3
            );
        bool source_spans_valid = false;
        if (source_selector == 0x0) {
            source_spans_valid =
                native_tacc_span_preflight_valid(
                    state,
                    state.tsrc0,
                    64) &&
                native_tacc_span_preflight_valid(
                    state,
                    state.tsrc1,
                    64);
        } else if (source_selector == 0x1) {
            source_spans_valid =
                native_tacc_span_preflight_valid(
                    state,
                    state.tsrc0,
                    64);
        } else if (source_selector == 0x3) {
            source_spans_valid =
                native_tacc_span_preflight_valid(
                    state,
                    state.tdst,
                    64) &&
                native_tacc_span_preflight_valid(
                    state,
                    state.tsrc0,
                    64);
        }
        const bool admitted =
            canonical &&
            state.tacc_owner == state.core_id &&
            state.tacc_valid &&
            tacc_mode_matches_latched_format(state) &&
            !state.tacc_busy &&
            !state.tacc_force_pending &&
            source_spans_valid;
        if (admitted) {
            traits.unjournaled_cycle_bound =
                native_tamac_cycle_bound(
                    state,
                    source_selector);
            traits.tacc_publish_busy = true;
            traits.tacc_validation_trap_expected = false;
        }
        return traits;
    }

    const int function = function_byte & 0x7;
    const bool canonical =
        source_selector == 0x0 &&
        function_byte == function &&
        function >= 0x2 &&
        function <= 0x6;
    if (!canonical)
        return traits;

    bool admitted = false;
    switch (function) {
        case 0x2:  // TACC.TRY
            admitted =
                !state.tacc_busy &&
                !state.tacc_force_pending;
            break;
        case 0x3:  // TACC.CLEAR
            admitted =
                state.tacc_owner == state.core_id &&
                tacc_mode_is_legal(state) &&
                !state.tacc_busy &&
                !state.tacc_force_pending;
            break;
        case 0x4:  // TACC.LOAD
            admitted =
                state.tacc_owner == state.core_id &&
                tacc_mode_is_legal(state) &&
                (state.tsrc0 & 0x3F) == 0 &&
                native_tacc_span_preflight_valid(
                    state,
                    state.tsrc0,
                    TACC_IMAGE_BYTES) &&
                !state.tacc_busy &&
                !state.tacc_force_pending;
            break;
        case 0x5:  // TACC.STORE
            admitted =
                state.tacc_owner == state.core_id &&
                state.tacc_valid &&
                (state.tdst & 0x3F) == 0 &&
                native_tacc_span_preflight_valid(
                    state,
                    state.tdst,
                    TACC_IMAGE_BYTES) &&
                !state.tacc_busy &&
                !state.tacc_force_pending;
            break;
        case 0x6:  // TACC.RELEASE
            admitted =
                state.tacc_owner == state.core_id &&
                !state.tacc_busy &&
                !state.tacc_force_pending;
            break;
        default:
            break;
    }
    if (admitted) {
        traits.unjournaled_cycle_bound =
            function == 0x4 || function == 0x5
            ? 6
            : 2;
        traits.tacc_publish_busy = true;
        traits.tacc_validation_trap_expected = false;
    }
    return traits;
}

static uint64_t native_legacy_mex_cycle_bound(
        CPUState& state,
        uint64_t opcode_address,
        int modifier,
        int subop) {
    const int source_selector = (subop >> 2) & 0x3;
    const int operation = subop & 0x3;
    const uint8_t function_byte =
        icache_peek_byte_without_accounting(
            state,
            opcode_address + 1);
    const int function =
        source_selector == 0x2
        ? 0
        : function_byte & 0x7;
    const int element_width = state.tmode & 0x7;
    const bool floating = element_width >= EW_FP16;

    // exec_mex() reports extra cycles beyond the ordinary one-cycle issue.
    // Its native path is data-independent with respect to latency, so decode
    // the exact bound before allowing an unjournaled tile-memory mutation.
    uint64_t extra_cycles = 0;
    if (modifier == 0x8 && operation == 0x0) {
        extra_cycles = 1;
    } else if (operation == 0x1) {
        if (floating) {
            switch (function) {
                case 0:
                    extra_cycles = 1;
                    break;
                case 1:
                case 5:
                    extra_cycles = 3;
                    break;
                case 2:
                case 3:
                case 4:
                    extra_cycles = 2;
                    break;
                default:
                    extra_cycles = 1;
                    break;
            }
        } else if (function == 0) {
            extra_cycles = 1;
        }
    }
    return 1 + (modifier >= 0 ? 1 : 0) + extra_cycles;
}

static SystemInstructionTraits classify_system_instruction(
        CPUState& state) {
    uint64_t address = pc(state);
    uint64_t opcode_address = address;
    uint8_t opcode =
        icache_peek_byte_without_accounting(state, address);
    int family = (opcode >> 4) & 0xF;
    int subop = opcode & 0xF;
    int modifier = -1;

    if (family == 0xF) {
        if (subop == 0x9 || subop == 0xA)
            return {true, false};
        if (subop == 0xB)
            return {true, false};

        modifier = subop;
        opcode_address = address + 1;
        opcode =
            icache_peek_byte_without_accounting(
                state,
                opcode_address);
        family = (opcode >> 4) & 0xF;
        subop = opcode & 0xF;
        if (family == 0xF && (subop == 0x9 || subop == 0xA))
            return {true, false};
        if (family == 0xF && subop == 0xB)
            return {true, false};
    }

    if (family == 0x0) {
        switch (subop) {
            case 0x4:
            case 0x5:
            case 0x6:
            case 0x7:
            case 0x8:
            case 0xD:
            case 0xE:
                return {true, false};
            default:
                return {};
        }
    }
    if (family == 0x5)
        return {true, false};
    if (family == 0x8) {
        switch (subop) {
            case 0x0:
            case 0x1:
            case 0x2:
            case 0x3:
            case 0x4:
            case 0x5:
            case 0x7:
            case 0x8:
            case 0x9:
            case 0xB:
            case 0xF:
                return {true, false};
            default:
                return {};
        }
    }
    if (family == 0x9 && subop != 0x0 && subop != 0x8)
        return {true, false};
    if (family == 0xE) {
        const SystemInstructionTraits tacc =
            native_tacc_instruction_traits(
                state,
                opcode_address,
                modifier,
                subop);
        if (tacc.tacc_python_fallback)
            return tacc;
        return {
            false,
            true,
            native_legacy_mex_cycle_bound(
                state,
                opcode_address,
                modifier,
                subop),
        };
    }
    return {};
}

static bool micro_instruction_fetch_uses_python_route(
        uint64_t address) {
    // The Python reduced-core oracle routes instruction fetches through the
    // cluster scratchpad sentinel and the MMIO aperture. Native MemoryMappings
    // deliberately does not own either route, so conservatively keep any
    // instruction whose maximum decode window can touch one of them on the
    // Python path instead of aliasing raw bank-zero bytes.
    constexpr uint64_t MAX_INSTRUCTION_BYTES =
        CPUState::PRIVATE_DECODE_IDENTITY_BYTES;
    constexpr uint64_t MMIO_START =
        0xFFFF'FF00'0000'0000ULL;
    constexpr uint64_t MMIO_END =
        0xFFFF'FF80'0000'0000ULL;
    for (
        uint64_t offset = 0;
        offset < MAX_INSTRUCTION_BYTES;
        offset++
    ) {
        if (
            address >
            std::numeric_limits<uint64_t>::max() -
                offset
        ) {
            return true;
        }
        const uint64_t candidate = address + offset;
        if (
            static_cast<uint32_t>(candidate >> 32) ==
                0xFFFF'FE00U ||
            (
                candidate >= MMIO_START &&
                candidate < MMIO_END
            )
        ) {
            return true;
        }
    }
    return false;
}

static bool micro_instruction_fetch_window_touches_mmio(
        uint64_t address) {
    constexpr uint64_t MAX_INSTRUCTION_BYTES =
        CPUState::PRIVATE_DECODE_IDENTITY_BYTES;
    constexpr uint64_t MMIO_START =
        0xFFFF'FF00'0000'0000ULL;
    constexpr uint64_t MMIO_END =
        0xFFFF'FF80'0000'0000ULL;
    for (
        uint64_t offset = 0;
        offset < MAX_INSTRUCTION_BYTES;
        offset++
    ) {
        const uint64_t candidate = address + offset;
        if (
            candidate >= MMIO_START &&
            candidate < MMIO_END
        ) {
            return true;
        }
    }
    return false;
}

static bool
micro_decoded_instruction_requires_python_oracle(
        int family,
        int subop,
        int modifier) {
    if (modifier == 0x6 && family == 0x3) {
        // Native next_instruction_size() intentionally remains a shallow
        // compatibility estimate. The Python oracle owns recursive target
        // sizing for EXT.SKIP, including a prefixed skipped instruction.
        return true;
    }

    switch (family) {
        case 0x0:
            // Retain only IDL, NOP, HALT, EI, and DI. Reset, trap/return,
            // stack traffic, and stripped 1802 heritage use the micro oracle.
            return !(
                subop == 0x0 ||
                subop == 0x1 ||
                subop == 0x2 ||
                subop == 0xB ||
                subop == 0xC
            );
        case 0x5:
            // Native scalar memory does not yet recognize cluster scratchpad
            // ownership and would bypass the compatibility MMIO route.
            return true;
        case 0x6:
            // GLO/GHI/PLO/PHI use the stripped D register.
            return subop >= 0xC;
        case 0x8:
        case 0x9:
            // MEMALU and port I/O are not implemented by the reduced core.
            return true;
        case 0xC:
            // MUL/DIV is cluster-shared; Tier-2 bitfield is gated out.
            // Tier-1 POPCNT/CLZ/CTZ/BITREV remains core-local.
            return subop <= 0x7 || subop >= 0xC;
        case 0xD:
            // Restricted and cluster-shared CSR semantics remain authoritative
            // in Megapad64Micro for this element.
            return true;
        case 0xE:
            // The tile engine is a cluster-shared resource.
            return true;
        default:
            return false;
    }
}

static bool micro_instruction_requires_python_oracle(CPUState& state) {
    if (state.profile != CoreProfile::MICRO)
        return false;

    // The reduced core's cluster privilege state remains owned by the Python
    // compatibility model.  Keep every user-mode instruction on the oracle
    // path while that ownership boundary remains in force.
    if (state.priv_level != 0)
        return true;

    uint64_t address = pc(state);
    if (micro_instruction_fetch_uses_python_route(address))
        return true;
    uint8_t opcode = mem_read8(state, address);
    int family = (opcode >> 4) & 0xF;
    int subop = opcode & 0xF;
    int modifier = -1;

    // F0-F8 are modifiers (including REX).  Classify the instruction they
    // prefix without consuming either byte.  The self-contained extended
    // engines are absent or cluster-shared on a micro-core.
    if (family == 0xF) {
        if (subop == 0x9 || subop == 0xA || subop == 0xB)
            return true;
        modifier = subop;
        opcode = mem_read8(state, address + 1);
        family = (opcode >> 4) & 0xF;
        subop = opcode & 0xF;
        if (
            family == 0xF &&
            (subop == 0x9 || subop == 0xA || subop == 0xB)
        ) {
            return true;
        }
    }
    return micro_decoded_instruction_requires_python_oracle(
        family, subop, modifier);
}

struct PrivateInstructionProof {
    const CPUState* core = nullptr;
    uint64_t address = 0;
    bool micro_native_private = false;
};

static int step_one(
        CPUState& s,
        const StepCallbacks& cb,
        const PrivateInstructionProof*
            private_instruction_proof = nullptr) {
    if (s.halted)
        throw std::runtime_error("HALT");
    if (s.idle) {
        s.cycle_count++;
        return 1;
    }

    struct InstructionBusScope {
        CPUState& state;
        ResumableBusAccess* previous;
        InstructionBusScope(
                CPUState& state_value,
                ResumableBusAccess* current)
            : state(state_value),
              previous(state_value.instruction_bus_access) {
            state.instruction_bus_access = current;
        }
        ~InstructionBusScope() {
            state.instruction_bus_access = previous;
        }
    } instruction_bus_scope(s, cb.bus_access);

    bool proven_micro_native_private = false;
    if (private_instruction_proof != nullptr) {
        if (
            private_instruction_proof->core != &s ||
            private_instruction_proof->address != pc(s) ||
            !private_instruction_proof->
                micro_native_private ||
            s.profile != CoreProfile::MICRO
        ) {
            throw std::logic_error(
                "private instruction proof does not match execution");
        }
        proven_micro_native_private = true;
    }

    // This is a transactional boundary: the Python microcore must see the
    // exact original PC and prefix state. The private worker can consume a
    // proof only at the unchanged instruction boundary classified directly
    // above; all other callers retain the authoritative oracle check.
    if (
        !proven_micro_native_private &&
        micro_instruction_requires_python_oracle(s)
    )
        throw std::runtime_error("EXT_ISA_FALLBACK");

    icache_begin_instruction(s);
    uint64_t pc_start = pc(s);  // save so we can rewind for MEX_FALLBACK
    uint8_t byte0 = fetch8(s);
    int f = (byte0 >> 4) & 0xF;
    int n = byte0 & 0xF;
    int cycles = 1;

    // EXT prefix
    if (f == 0xF) {
        // EXT.STRING (F9) — execute natively
        if (n == 0x9) {
            cycles += exec_string(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // EXT.DICT (FA) — execute natively
        if (n == 0xA) {
            cycles += exec_dict(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // EXT.CRYPTO (FB) — execute natively
        if (n == 0xB) {
            cycles += exec_crypto(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        s.ext_modifier = n;
        byte0 = fetch8(s);
        f = (byte0 >> 4) & 0xF;
        n = byte0 & 0xF;
        cycles++;
        // REX + EXT.STRING — execute natively with REX bits active
        if (f == 0xF && n == 0x9) {
            cycles += exec_string(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // REX + EXT.DICT — execute natively with REX bits active
        if (f == 0xF && n == 0xA) {
            cycles += exec_dict(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        // REX + EXT.CRYPTO — execute natively with REX bits active
        if (f == 0xF && n == 0xB) {
            cycles += exec_crypto(s, cb);
            s.ext_modifier = -1;
            s.cycle_count += cycles;
            return cycles;
        }
        if (f == 0xF)
            throw std::runtime_error("TRAP:ILLEGAL_OP:Double EXT prefix");
    }

    switch (f) {
    case 0x0: {  // SYS
        switch (n) {
            case 0x0: s.idle = true; break;
            case 0x1: break;  // NOP
            case 0x2: s.halted = true; break;
            case 0x3: /* RESET — leave to Python */ throw std::runtime_error("TRAP:RESET"); break;
            case 0x4: {  // RTI
                pc(s) = sys_pop64(s, cb);
                uint64_t saved = sys_pop64(s, cb);
                flags_unpack(s, saved & 0xFF);
                s.priv_level = (saved >> 8) & 1;
                cycles++;
                break;
            }
            case 0x5: {  // RET
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint64_t t = sys_pop64(s, cb) & 0xFFFF;
                s.xsel = (t >> 8) & 0x1F;
                s.psel = t & 0x1F;
                s.flag_i = 1;
                cycles++;
                break;
            }
            case 0x6: {  // DIS
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint64_t t = sys_pop64(s, cb) & 0xFFFF;
                s.xsel = (t >> 8) & 0x1F;
                s.psel = t & 0x1F;
                s.flag_i = 0;
                cycles++;
                break;
            }
            case 0x7: {  // MARK
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                uint16_t t = ((s.xsel & 0x1F) << 8) | (s.psel & 0x1F);
                s.t_reg = t;
                sys_push64(s, cb, t);
                s.xsel = s.psel;
                cycles++;
                break;
            }
            case 0x8:  // SAV — store T as 16-bit to M(R(X))
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                sys_write16(s, cb, rx(s), s.t_reg);
                break;
            case 0x9:  // SEQ
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.q_out = 1; break;
            case 0xA:  // REQ
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.q_out = 0; break;
            case 0xB: s.flag_i = 1; break;  // EI
            case 0xC: s.flag_i = 0; break;  // DI
            case 0xD: {  // CALL.L
                uint8_t b1 = fetch8(s);
                int rn = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
                uint64_t target = s.regs[rn];
                // Check accelerator hooks BEFORE pushing return address
                int hook = find_accel_hook(s, target);
                if (hook && cb.bus_access == nullptr) {
                    const AccelHookContext context = {
                        cb.has_mmio,
                        cb.mmio_start,
                        cb.mmio_end,
                    };
                    const AccelHookResult result =
                        execute_accel_hook(s, hook, context);
                    if (result.handled) {
                        // Don't push a return address for an accepted shortcut:
                        // PC remains after CALL.L and the BIOS word is skipped.
                        cycles += result.extra_cycles;
                        break;
                    }
                }
                // Unsafe, unsupported, or non-equivalent shortcuts fall
                // through transactionally to the ordinary BIOS call.
                uint64_t ret_addr = pc(s);
                sys_push64(s, cb, ret_addr);
                pc(s) = target;
                cycles++;
                break;
            }
            case 0xE: {  // RET.L
                pc(s) = sys_pop64(s, cb);
                cycles++;
                break;
            }
            case 0xF:  // TRAP
                throw std::runtime_error("TRAP:SW_TRAP");
                break;
        }
        break;
    }

    case 0x1: {  // INC Rn
        int rn_inc = n | (rex_n(s.ext_modifier) << 4);
        s.regs[rn_inc]++;
        break;
    }

    case 0x2: {  // DEC Rn
        int rn_dec = n | (rex_n(s.ext_modifier) << 4);
        s.regs[rn_dec]--;
        break;
    }

    case 0x3: {  // BR (short branch) / SKIP
        if (s.ext_modifier == 6) {  // SKIP mode
            if (eval_cond(s, n)) {
                int skip = next_instruction_size(s);
                pc(s) += skip;
                cycles++;
            }
        } else {
            uint8_t off_byte = fetch8(s);
            int64_t offset = s64(sign_extend(off_byte, 8));
            if (eval_cond(s, n)) {
                pc(s) += offset;
                cycles++;
            }
        }
        break;
    }

    case 0x4: {  // LBR (long branch)
        uint8_t hi = fetch8(s);
        uint8_t lo = fetch8(s);
        int64_t offset = s64(sign_extend(((uint16_t)hi << 8) | lo, 16));
        if (eval_cond(s, n)) {
            pc(s) += offset;
            cycles++;
        }
        break;
    }

    case 0x5: {  // MEM
        uint8_t b1 = fetch8(s);
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
        switch (n) {
            case 0x0:   // LDN
                s.regs[rd] = sys_read64(s, cb, s.regs[rs]);
                break;
            case 0x1:   // LDA
                s.regs[rd] = sys_read64(s, cb, s.regs[rs]);
                s.regs[rs] += 8;
                break;
            case 0x2:   // LDX
                s.regs[rd] = sys_read64(s, cb, rx(s));
                break;
            case 0x3:   // LDXA
                s.regs[rd] = sys_read64(s, cb, rx(s));
                rx(s) += 8;
                break;
            case 0x4:   // STR
                sys_write64(s, cb, s.regs[rd], s.regs[rs]);
                break;
            case 0x5:   // STXD
                sys_write64(s, cb, rx(s), s.regs[rd]);
                rx(s) -= 8;
                break;
            case 0x6:   // LD.B
                s.regs[rd] = sys_read8(s, cb, s.regs[rs]);
                break;
            case 0x7:   // ST.B
                sys_write8(s, cb, s.regs[rd], s.regs[rs] & 0xFF);
                break;
            case 0x8:   // LD.H
                s.regs[rd] = sys_read16(s, cb, s.regs[rs]);
                break;
            case 0x9:   // ST.H
                sys_write16(s, cb, s.regs[rd], s.regs[rs] & 0xFFFF);
                break;
            case 0xA:   // LD.W
                s.regs[rd] = sys_read32(s, cb, s.regs[rs]);
                break;
            case 0xB:   // ST.W
                sys_write32(s, cb, s.regs[rd], s.regs[rs] & 0xFFFFFFFF);
                break;
            case 0xC:   // LD.SB
                s.regs[rd] = sign_extend(sys_read8(s, cb, s.regs[rs]), 8);
                break;
            case 0xD:   // LD.SH
                s.regs[rd] = sign_extend(sys_read16(s, cb, s.regs[rs]), 16);
                break;
            case 0xE:   // LD.SW
                s.regs[rd] = sign_extend(sys_read32(s, cb, s.regs[rs]), 32);
                break;
            case 0xF: { // LD.D [Rn+off8]
                uint8_t off_byte = fetch8(s);
                int64_t off = s64(sign_extend(off_byte, 8)) * 8;
                s.regs[rd] = sys_read64(s, cb, s.regs[rs] + off);
                cycles++;
                break;
            }
        }
        break;
    }

    case 0x6: {  // IMM
        uint8_t b1 = fetch8(s);
        int rn = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        switch (n) {
            case 0x0: {  // LDI
                if (s.ext_modifier == 0) {  // EXT.IMM64
                    uint64_t imm = 0;
                    for (int i = 0; i < 8; i++)
                        imm |= (uint64_t)fetch8(s) << (8*i);
                    s.regs[rn] = imm;
                } else {
                    s.regs[rn] = fetch8(s);
                }
                break;
            }
            case 0x1: {  // LHI
                uint8_t lo = fetch8(s);
                uint8_t hi = fetch8(s);
                uint16_t imm16 = lo | ((uint16_t)hi << 8);
                s.regs[rn] = (s.regs[rn] & 0x0000FFFFFFFFFFFFULL) | ((uint64_t)imm16 << 48);
                break;
            }
            case 0x2: {  // ADDI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a + s64(imm);
                update_flags_arith(s, a, imm, result, false);
                s.regs[rn] = result;
                break;
            }
            case 0x3: {  // ANDI
                uint8_t imm = fetch8(s);
                s.regs[rn] &= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x4: {  // ORI
                uint8_t imm = fetch8(s);
                s.regs[rn] |= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x5: {  // XORI
                uint8_t imm = fetch8(s);
                s.regs[rn] ^= imm;
                update_flags_logic(s, s.regs[rn]);
                break;
            }
            case 0x6: {  // CMPI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a - s64(imm);
                update_flags_cmp(s, a, imm, result);
                break;
            }
            case 0x7: {  // SUBI
                uint64_t imm = sign_extend(fetch8(s), 8);
                uint64_t a = s.regs[rn];
                uint64_t result = a - s64(imm);
                update_flags_arith(s, a, imm, result, true);
                s.regs[rn] = result;
                break;
            }
            case 0x8: {  // LSLI
                int imm4 = b1 & 0xF;
                s.regs[rn] <<= imm4;
                break;
            }
            case 0x9: {  // LSRI
                int imm4 = b1 & 0xF;
                s.regs[rn] >>= imm4;
                break;
            }
            case 0xA: {  // ASRI
                int imm4 = b1 & 0xF;
                s.regs[rn] = (uint64_t)(s64(s.regs[rn]) >> imm4);
                break;
            }
            case 0xB: {  // ROLI
                int imm4 = b1 & 0xF;
                if (imm4) {
                    uint64_t v = s.regs[rn];
                    s.regs[rn] = (v << imm4) | (v >> (64 - imm4));
                }
                break;
            }
            case 0xC:  // GLO
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.d_reg = s.regs[rn] & 0xFF;
                break;
            case 0xD:  // GHI
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.d_reg = (s.regs[rn] >> 8) & 0xFF;
                break;
            case 0xE:  // PLO
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.regs[rn] = (s.regs[rn] & ~0xFFULL) | (s.d_reg & 0xFF);
                break;
            case 0xF:  // PHI
                if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
                s.regs[rn] = (s.regs[rn] & ~0xFF00ULL) | (((uint64_t)(s.d_reg & 0xFF)) << 8);
                break;
        }
        break;
    }

    case 0x7: {  // ALU
        uint8_t b1 = fetch8(s);
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
        uint64_t a = s.regs[rd];
        uint64_t b = s.regs[rs];
        switch (n) {
            case 0x0: {  // ADD
                uint64_t r = a + b;
                update_flags_arith(s, a, b, r, false);
                s.regs[rd] = r;
                break;
            }
            case 0x1: {  // ADC
                uint64_t r = a + b + s.flag_c;
                update_flags_arith(s, a, b + s.flag_c, r, false);
                s.regs[rd] = r;
                break;
            }
            case 0x2: {  // SUB
                uint64_t r = a - b;
                update_flags_arith(s, a, b, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0x3: {  // SBB
                uint64_t borrow = 1 - s.flag_c;
                uint64_t r = a - b - borrow;
                update_flags_arith(s, a, b + borrow, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0x4: {  // AND
                uint64_t r = a & b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x5: {  // OR
                uint64_t r = a | b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x6: {  // XOR
                uint64_t r = a ^ b;
                update_flags_logic(s, r);
                s.regs[rd] = r;
                break;
            }
            case 0x7: {  // CMP
                uint64_t r = a - b;
                update_flags_cmp(s, a, b, r);
                break;
            }
            case 0x8:  // MOV
                s.regs[rd] = b;
                break;
            case 0x9: {  // NOT
                s.regs[rd] = ~b;
                update_flags_logic(s, s.regs[rd]);
                break;
            }
            case 0xA: {  // NEG
                uint64_t r = -b;  // wraps naturally for uint64_t
                update_flags_arith(s, 0, b, r, true);
                s.regs[rd] = r;
                break;
            }
            case 0xB: {  // SHL
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (64 - shift)) & 1) : 0;
                uint64_t r = a << shift;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xC: {  // SHR
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (shift - 1)) & 1) : 0;
                uint64_t r = a >> shift;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xD: {  // SAR
                int shift = b & 63;
                uint64_t out_bit = shift ? ((a >> (shift - 1)) & 1) : 0;
                uint64_t r = (uint64_t)(s64(a) >> shift);
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_c = out_bit;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xE: {  // ROL
                int shift = b & 63;
                uint64_t r = shift ? ((a << shift) | (a >> (64 - shift))) : a;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
            case 0xF: {  // ROR
                int shift = b & 63;
                uint64_t r = shift ? ((a >> shift) | (a << (64 - shift))) : a;
                s.flag_z = (r == 0) ? 1 : 0;
                s.flag_n = (r >> 63) & 1;
                s.flag_p = parity8(r);
                s.regs[rd] = r;
                break;
            }
        }
        break;
    }

    case 0x8: {  // MEMALU
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        uint8_t m;
        switch (n) {
            case 0x0: s.d_reg = sys_read8(s, cb, rx(s)); break;
            case 0x1:
                s.d_reg = (sys_read8(s, cb, rx(s)) | s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x2:
                s.d_reg = (sys_read8(s, cb, rx(s)) & s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x3:
                s.d_reg = (sys_read8(s, cb, rx(s)) ^ s.d_reg) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            case 0x4: {  // ADD.X
                m = sys_read8(s, cb, rx(s));
                int result = m + s.d_reg;
                s.flag_c = result > 0xFF;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x5: {  // SD.X
                m = sys_read8(s, cb, rx(s));
                int result = m - s.d_reg;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x6: {  // SHR.D
                s.flag_c = s.d_reg & 1;
                s.d_reg = (s.d_reg >> 1) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x7: {  // SM.X
                m = sys_read8(s, cb, rx(s));
                int result = s.d_reg - m;
                s.flag_c = result >= 0;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x8: {  // ADC.X
                m = sys_read8(s, cb, rx(s));
                int result = m + s.d_reg + s.flag_c;
                s.flag_c = result > 0xFF;
                s.d_reg = result & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0x9: {  // STXI — M(R(X)) ← D[7:0]; R(X)++
                sys_write8(s, cb, rx(s), s.d_reg & 0xFF);
                rx(s)++;
                break;
            }
            case 0xA: {  // SHRC.D
                uint8_t old_c = s.flag_c;
                s.flag_c = s.d_reg & 1;
                s.d_reg = ((old_c << 7) | (s.d_reg >> 1)) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xB: {  // STXD.D — M(R(X)) ← D[7:0]; R(X)--
                sys_write8(s, cb, rx(s), s.d_reg & 0xFF);
                rx(s)--;
                break;
            }
            case 0xC: {  // SHL.D
                s.flag_c = (s.d_reg >> 7) & 1;
                s.d_reg = (s.d_reg << 1) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xD: {  // SHLC.D
                uint8_t old_c = s.flag_c;
                s.flag_c = (s.d_reg >> 7) & 1;
                s.d_reg = ((s.d_reg << 1) | old_c) & 0xFF;
                s.flag_z = s.d_reg == 0;
                break;
            }
            case 0xE:  // IRX
                rx(s)++;
                break;
            case 0xF:  // LDXA
                s.d_reg = sys_read8(s, cb, rx(s));
                rx(s)++;
                break;
        }
        break;
    }

    case 0x9: {  // I/O
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        if (n >= 1 && n <= 7) {  // OUT
            uint8_t val = sys_read8(s, cb, rx(s));
            s.port_out[n] = val;
            rx(s)++;
            // Port bridge → route byte to MMIO device
            uint32_t mmio_off = s.port_map[n];
            if (mmio_off < 0x1000 && cb.has_mmio) {
                uint64_t mmio_addr = cb.mmio_start + mmio_off;
                sys_write8(s, cb, mmio_addr, val, true);
            }
            if (cb.on_output)
                cb.on_output(n, val);
        } else if (n >= 9 && n <= 15) {  // INP
            int port = n - 8;
            uint8_t val;
            uint32_t mmio_off = s.port_map[port];
            if (mmio_off < 0x1000 && cb.has_mmio) {
                uint64_t mmio_addr = cb.mmio_start + mmio_off;
                val = sys_read8(s, cb, mmio_addr, true);
            } else {
                val = s.port_in[port];
            }
            sys_write8(s, cb, rx(s), val);
            s.d_reg = val;
        }
        break;
    }

    case 0xA:  // SEP Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.psel = n | (rex_n(s.ext_modifier) << 4);
        break;

    case 0xB:  // SEX Rn
        if (s.priv_level) throw std::runtime_error("TRAP:PRIV_FAULT");
        s.xsel = n | (rex_n(s.ext_modifier) << 4);
        break;

    case 0xC: {  // MULDIV
        uint8_t b1 = fetch8(s);
        int rd = ((b1 >> 4) & 0xF) | (rex_d(s.ext_modifier) << 4);
        int rs = (b1 & 0xF) | (rex_s(s.ext_modifier) << 4);
        uint64_t a = s.regs[rd];
        uint64_t b = s.regs[rs];
        switch (n) {
            case 0x0: {  // MUL signed low
                __int128 r = (__int128)s64(a) * (__int128)s64(b);
                s.regs[rd] = (uint64_t)r;
                break;
            }
            case 0x1: {  // MULH signed high
                __int128 r = (__int128)s64(a) * (__int128)s64(b);
                s.regs[rd] = (uint64_t)(r >> 64);
                break;
            }
            case 0x2: {  // UMUL unsigned low
                __uint128_t r = (__uint128_t)a * (__uint128_t)b;
                s.regs[rd] = (uint64_t)r;
                break;
            }
            case 0x3: {  // UMULH unsigned high
                __uint128_t r = (__uint128_t)a * (__uint128_t)b;
                s.regs[rd] = (uint64_t)(r >> 64);
                break;
            }
            case 0x4: {  // DIV signed
                if (b == 0 || (s64(a) == INT64_MIN && s64(b) == -1))
                    throw std::runtime_error("TRAP:DIV_ZERO");
                int64_t q = s64(a) / s64(b);  // C++ truncates toward zero
                int64_t rem = s64(a) - q * s64(b);
                s.regs[rd] = (uint64_t)q;
                s.regs[0] = (uint64_t)rem;
                break;
            }
            case 0x5: {  // UDIV
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                s.regs[0] = a % b;
                s.regs[rd] = a / b;
                break;
            }
            case 0x6: {  // MOD signed
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                int64_t q = s64(a) / s64(b);
                s.regs[rd] = (uint64_t)(s64(a) - q * s64(b));
                break;
            }
            case 0x7: {  // UMOD
                if (b == 0)
                    throw std::runtime_error("TRAP:DIV_ZERO");
                s.regs[rd] = a % b;
                break;
            }
            // ---- Bitfield ALU (sub-ops 0x8–0xF) ----
            case 0x8: {  // POPCNT
                s.regs[rd] = __builtin_popcountll(b);
                break;
            }
            case 0x9: {  // CLZ
                s.regs[rd] = b ? __builtin_clzll(b) : 64;
                break;
            }
            case 0xA: {  // CTZ
                s.regs[rd] = b ? __builtin_ctzll(b) : 64;
                break;
            }
            case 0xB: {  // BITREV
                uint64_t v = b;
                v = ((v >> 1)  & 0x5555555555555555ULL) | ((v & 0x5555555555555555ULL) << 1);
                v = ((v >> 2)  & 0x3333333333333333ULL) | ((v & 0x3333333333333333ULL) << 2);
                v = ((v >> 4)  & 0x0F0F0F0F0F0F0F0FULL) | ((v & 0x0F0F0F0F0F0F0F0FULL) << 4);
                v = __builtin_bswap64(v);
                s.regs[rd] = v;
                break;
            }
            case 0xC: {  // BEXT (pext)
                // Rd ← pext(Rs, Rd): extract bits from a at positions set in b
                uint64_t src = a, mask = b, result = 0;
                for (int i = 0; mask; i++) {
                    uint64_t lsb = mask & (-mask);
                    if (src & lsb) result |= (1ULL << i);
                    mask &= mask - 1;
                }
                s.regs[rd] = result;
                break;
            }
            case 0xD: {  // BDEP (pdep)
                // Rd ← pdep(Rs, Rd): deposit bits from b into positions set in a
                uint64_t src = b, mask = a, result = 0;
                for (int i = 0; mask; i++) {
                    uint64_t lsb = mask & (-mask);
                    if (src & (1ULL << i)) result |= lsb;
                    mask &= mask - 1;
                }
                s.regs[rd] = result;
                break;
            }
            case 0xE: {  // RORI (3-byte: CE [Rd:4][0000] [imm8])
                uint8_t imm = fetch8(s);
                int shift = imm & 63;
                uint64_t r = shift ? ((a >> shift) | (a << (64 - shift))) : a;
                s.regs[rd] = r;
                break;
            }
            case 0xF: {  // BSWAP
                s.regs[rd] = __builtin_bswap64(b);
                break;
            }
        }
        {
            uint64_t r = s.regs[rd];
            s.flag_z = (r == 0) ? 1 : 0;
            s.flag_n = (r >> 63) & 1;
        }
        if (n <= 0x7) cycles += 3;  // mul/div extra cycles only
        break;
    }

    case 0xD: {  // CSR
        int w_bit = (n >> 3) & 1;
        int rn = n & 0x7;
        uint8_t csr_addr = fetch8(s);
        if (w_bit == 0) {
            // CSRR — check for system-level override first
            if (cb.csr_read_override) {
                uint64_t v = cb.csr_read_override(csr_addr);
                if (v != (uint64_t)-1) {
                    s.regs[rn] = v;
                    break;
                }
            }
            s.regs[rn] = csr_read(s, csr_addr);
        } else {
            // Privilege check for protected CSR writes
            if (s.priv_level && (csr_addr == CSR_PRIV || csr_addr == CSR_IVT_BASE ||
                                 csr_addr == CSR_IE || csr_addr == CSR_BIST_CMD ||
                                 csr_addr == CSR_ICACHE_CTRL ||
                                 csr_addr == CSR_MPU_BASE || csr_addr == CSR_MPU_LIMIT)) {
                throw std::runtime_error("TRAP:PRIV_FAULT");
            }
            csr_write(s, csr_addr, s.regs[rn]);
        }
        break;
    }

    case 0xE: {  // MEX
        int rc = exec_mex(s, n);
        if (rc < 0) {
            // FP tile op — rewind PC to the start of the instruction
            // (including any EXT prefix) so the Python fallback can
            // re-fetch and execute the entire instruction correctly.
            pc(s) = pc_start;
            s.ext_modifier = -1;
            icache_rollback_instruction(s);
            throw std::runtime_error("MEX_FALLBACK");
        }
        cycles += rc;
        if (s.perf_enable)
            s.perf_tileops++;
        break;
    }

    default:
        break;
    }

    // Clear EXT modifier
    s.ext_modifier = -1;
    s.cycle_count += cycles;

    // Perf counters
    if (s.perf_enable) {
        s.perf_cycles += cycles;
        if ((f == 0x5 || f == 0x8) && cycles > 1)
            s.perf_stalls += cycles - 1;
    }

    return cycles;
}

// ---------------------------------------------------------------------------
//  Phase 3 private core command runner
// ---------------------------------------------------------------------------

enum class PrivateInstructionDisposition : uint8_t {
    EXECUTE_PRIVATE = 0,
    ICACHE_BOUNDARY = 1,
    SHARED_INSTRUCTION = 2,
};

struct PrivateInstructionClassification {
    PrivateInstructionDisposition disposition =
        PrivateInstructionDisposition::SHARED_INSTRUCTION;
    bool decode_cache_lookup = false;
    bool decode_cache_hit = false;
    PrivateInstructionProof proof;
};

static std::optional<uint8_t> private_icache_peek(
        const CPUState& state,
        uint64_t address) {
    if (
        state.profile != CoreProfile::FULL ||
        !state.icache_enabled
    ) {
        return std::nullopt;
    }
    const auto [index, tag] = icache_key(address);
    if (
        !state.icache_valid[index] ||
        state.icache_tags[index] != tag
    ) {
        return std::nullopt;
    }
    return state.icache_data[index][
        static_cast<std::size_t>(
            address &
            (CPUState::ICACHE_LINE_BYTES - 1))];
}

static bool private_icache_span_is_resident(
        const CPUState& state,
        uint64_t address,
        int length) {
    for (int offset = 0; offset < length; offset++) {
        if (!private_icache_peek(
                state,
                address +
                    static_cast<uint64_t>(offset)).has_value()) {
            return false;
        }
    }
    return true;
}

static std::size_t private_decode_cache_index(
        uint64_t address) {
    return static_cast<std::size_t>(
        (address ^ (address >> 7)) &
        (CPUState::PRIVATE_DECODE_CACHE_ENTRIES - 1));
}

static std::optional<uint8_t>
private_decode_identity_byte(
        CPUState& state,
        uint64_t address) {
    if (state.profile == CoreProfile::FULL)
        return private_icache_peek(state, address);
    if (state.profile == CoreProfile::MICRO)
        return mem_read8(state, address);
    return std::nullopt;
}

static bool private_decode_cache_hit(
        CPUState& state,
        uint64_t address) {
    const CPUState::PrivateDecodeCacheEntry& entry =
        state.private_decode_cache[
            private_decode_cache_index(address)];
    if (
        !entry.valid ||
        entry.address != address ||
        entry.identity_size == 0 ||
        entry.identity_size >
            CPUState::PRIVATE_DECODE_IDENTITY_BYTES
    ) {
        return false;
    }

    if (state.profile == CoreProfile::FULL) {
        if (!state.icache_enabled)
            return false;
        std::size_t consumed = 0;
        uint64_t current = address;
        while (consumed < entry.identity_size) {
            const auto [index, tag] =
                icache_key(current);
            if (
                !state.icache_valid[index] ||
                state.icache_tags[index] != tag
            ) {
                return false;
            }
            const std::size_t line_offset =
                static_cast<std::size_t>(
                    current &
                    (CPUState::ICACHE_LINE_BYTES - 1));
            const std::size_t chunk =
                std::min<std::size_t>(
                    entry.identity_size - consumed,
                    CPUState::ICACHE_LINE_BYTES -
                        line_offset);
            if (
                std::memcmp(
                    state.icache_data[index].data() +
                        line_offset,
                    entry.identity.data() + consumed,
                    chunk) != 0
            ) {
                return false;
            }
            consumed += chunk;
            current += static_cast<uint64_t>(chunk);
        }
        return true;
    }

    for (
        uint8_t offset = 0;
        offset < entry.identity_size;
        offset++
    ) {
        const std::optional<uint8_t> observed =
            private_decode_identity_byte(
                state,
                address +
                    static_cast<uint64_t>(offset));
        if (
            !observed.has_value() ||
            *observed != entry.identity[offset]
        ) {
            return false;
        }
    }
    return true;
}

static bool private_decode_cache_store(
        CPUState& state,
        uint64_t address,
        int identity_size) {
    if (
        identity_size <= 0 ||
        identity_size >
            static_cast<int>(
                CPUState::
                    PRIVATE_DECODE_IDENTITY_BYTES)
    ) {
        return false;
    }

    CPUState::PrivateDecodeCacheEntry candidate;
    candidate.address = address;
    candidate.identity_size =
        static_cast<uint8_t>(identity_size);
    if (state.profile == CoreProfile::FULL) {
        if (!state.icache_enabled)
            return false;
        std::size_t copied = 0;
        uint64_t current = address;
        while (
            copied <
            static_cast<std::size_t>(identity_size)
        ) {
            const auto [index, tag] =
                icache_key(current);
            if (
                !state.icache_valid[index] ||
                state.icache_tags[index] != tag
            ) {
                return false;
            }
            const std::size_t line_offset =
                static_cast<std::size_t>(
                    current &
                    (CPUState::ICACHE_LINE_BYTES - 1));
            const std::size_t chunk =
                std::min<std::size_t>(
                    static_cast<std::size_t>(
                        identity_size) - copied,
                    CPUState::ICACHE_LINE_BYTES -
                        line_offset);
            std::memcpy(
                candidate.identity.data() + copied,
                state.icache_data[index].data() +
                    line_offset,
                chunk);
            copied += chunk;
            current += static_cast<uint64_t>(chunk);
        }
    } else {
        for (
            int offset = 0;
            offset < identity_size;
            offset++
        ) {
            const std::optional<uint8_t> observed =
                private_decode_identity_byte(
                    state,
                    address +
                        static_cast<uint64_t>(offset));
            if (!observed.has_value())
                return false;
            candidate.identity[
                static_cast<std::size_t>(offset)] =
                    *observed;
        }
    }
    candidate.valid = true;
    state.private_decode_cache[
        private_decode_cache_index(address)] =
            candidate;
    return true;
}

static PrivateInstructionDisposition
classify_private_full_core_instruction(
        const CPUState& state,
        int* cache_identity_size = nullptr,
        bool* cacheable = nullptr) {
    if (cache_identity_size != nullptr)
        *cache_identity_size = 0;
    if (cacheable != nullptr)
        *cacheable = false;
    if (
        state.profile != CoreProfile::FULL ||
        state.ext_modifier != -1
    ) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }

    const uint64_t instruction_address =
        state.regs[state.psel];
    const std::optional<uint8_t> first =
        private_icache_peek(
            state, instruction_address);
    if (!first.has_value()) {
        return PrivateInstructionDisposition::
            ICACHE_BOUNDARY;
    }

    uint8_t opcode = *first;
    int family = (opcode >> 4) & 0xF;
    int subop = opcode & 0xF;
    int modifier = -1;
    int prefix_length = 0;

    if (family == 0xF) {
        // F9-FB enter engines that may access shared memory. F0-F6 and F8
        // are the only accepted single-instruction modifiers; F7 remains
        // reserved. Reserved and double-prefix forms stay on the coordinator.
        if (subop == 0x7 || subop >= 0x9)
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        modifier = subop;
        prefix_length = 1;
        const std::optional<uint8_t> following =
            private_icache_peek(
                state, instruction_address + 1);
        if (!following.has_value()) {
            return PrivateInstructionDisposition::
                ICACHE_BOUNDARY;
        }
        opcode = *following;
        family = (opcode >> 4) & 0xF;
        subop = opcode & 0xF;
        if (family == 0xF) {
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        }
    }

    int instruction_length = 0;
    bool private_instruction = false;
    switch (family) {
        case 0x0:
            instruction_length = 1;
            // EI remains a coordinator boundary. A line can already be
            // asserted while interrupts are masked; executing EI privately
            // and continuing the command would retire past the newly
            // eligible interrupt boundary.
            private_instruction =
                subop == 0x0 ||
                subop == 0x1 ||
                subop == 0x2 ||
                subop == 0x3 ||
                subop == 0x9 ||
                subop == 0xA ||
                subop == 0xC ||
                subop == 0xF;
            break;
        case 0x1:
        case 0x2:
        case 0xA:
        case 0xB:
            instruction_length = 1;
            private_instruction = true;
            break;
        case 0x3:
            instruction_length =
                modifier == 6 ? 1 : 2;
            private_instruction = true;
            break;
        case 0x4:
            instruction_length = 3;
            private_instruction = true;
            break;
        case 0x5:
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        case 0x6:
            if (subop == 0x0) {
                instruction_length =
                    modifier == 0 ? 10 : 3;
            } else if (subop == 0x1) {
                instruction_length = 4;
            } else if (subop <= 0x7) {
                instruction_length = 3;
            } else {
                instruction_length = 2;
            }
            private_instruction = true;
            break;
        case 0x7:
            instruction_length = 2;
            private_instruction = true;
            break;
        case 0x8:
            instruction_length = 1;
            private_instruction =
                subop == 0x6 ||
                subop == 0xA ||
                subop == 0xC ||
                subop == 0xD ||
                subop == 0xE;
            break;
        case 0x9:
        case 0xD:
        case 0xE:
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        case 0xC:
            instruction_length =
                subop == 0xE ? 3 : 2;
            private_instruction = true;
            break;
        default:
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
    }

    if (!private_instruction) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }

    const int total_length =
        prefix_length + instruction_length;
    if (!private_icache_span_is_resident(
            state,
            instruction_address,
            total_length)) {
        return PrivateInstructionDisposition::
            ICACHE_BOUNDARY;
    }

    // EXT.SKIP asks next_instruction_size() to read the first byte at the
    // skipped instruction. Require that exact private-cache read up front
    // only when the condition is taken.
    if (
        family == 0x3 &&
        modifier == 6 &&
        eval_cond(state, subop) &&
        !private_icache_peek(
            state,
            instruction_address +
                static_cast<uint64_t>(
                    total_length)).has_value()
    ) {
        return PrivateInstructionDisposition::
            ICACHE_BOUNDARY;
    }

    // EXT.SKIP depends on current flags and may require a target-byte
    // residency check that changes independently of its encoding. Keep that
    // dynamic instruction out of the host plan cache.
    if (!(family == 0x3 && modifier == 6)) {
        if (cache_identity_size != nullptr)
            *cache_identity_size = total_length;
        if (cacheable != nullptr)
            *cacheable = true;
    }
    return PrivateInstructionDisposition::
        EXECUTE_PRIVATE;
}

static PrivateInstructionDisposition
classify_private_micro_core_instruction(
        CPUState& state,
        int* cache_identity_size = nullptr,
        bool* cacheable = nullptr) {
    if (cache_identity_size != nullptr)
        *cache_identity_size = 0;
    if (cacheable != nullptr)
        *cacheable = false;
    if (
        state.profile != CoreProfile::MICRO ||
        state.ext_modifier != -1 ||
        state.priv_level != 0
    ) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }

    uint64_t instruction_address =
        state.regs[state.psel];
    if (
        micro_instruction_fetch_uses_python_route(
            instruction_address)
    ) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }
    uint8_t opcode =
        mem_read8(state, instruction_address);
    int family = (opcode >> 4) & 0xF;
    int subop = opcode & 0xF;
    int modifier = -1;
    int prefix_length = 0;

    if (family == 0xF) {
        // The reduced core accepts the same single ordinary modifier set as
        // the full core. Extended engines, the reserved F7 encoding, and a
        // second prefix remain coordinator/oracle boundaries.
        if (subop == 0x7 || subop >= 0x9) {
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        }
        modifier = subop;
        prefix_length = 1;
        instruction_address++;
        opcode = mem_read8(
            state, instruction_address);
        family = (opcode >> 4) & 0xF;
        subop = opcode & 0xF;
        if (family == 0xF) {
            return PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        }
    }

    // Proof reuse may bypass the second oracle decode in step_one. Make the
    // private-admission subset structural: an encoding still owned by the
    // Python oracle cannot produce a proof, even if the narrower switch below
    // is accidentally broadened later.
    if (micro_decoded_instruction_requires_python_oracle(
            family, subop, modifier)) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }

    bool private_instruction = false;
    int instruction_length = 0;
    switch (family) {
        case 0x0:
            // IDL, NOP, HALT, and DI are local. EI remains a coordinator
            // boundary so an already asserted line is observed before the
            // following instruction, exactly as on a full core.
            private_instruction =
                subop == 0x0 ||
                subop == 0x1 ||
                subop == 0x2 ||
                subop == 0xC;
            instruction_length = 1;
            break;
        case 0x1:
        case 0x2:
            private_instruction = true;
            instruction_length = 1;
            break;
        case 0x3:
            // Native and Python disagree today about the size of a prefixed
            // target skipped by EXT.SKIP. Keep that exact encoding on the
            // coordinator/oracle until the architectural size rule is fixed.
            private_instruction = modifier != 0x6;
            instruction_length = 2;
            break;
        case 0x4:
            private_instruction = true;
            instruction_length = 3;
            break;
        case 0x7:
            private_instruction = true;
            instruction_length = 2;
            break;
        case 0xA:
        case 0xB:
            private_instruction = true;
            instruction_length = 1;
            break;
        case 0x6:
            // GLO/GHI/PLO/PHI depend on the stripped D register and stay on
            // the reduced-core Python oracle.
            private_instruction = subop < 0xC;
            if (subop == 0x0) {
                instruction_length =
                    modifier == 0 ? 10 : 3;
            } else if (subop == 0x1) {
                instruction_length = 4;
            } else if (subop <= 0x7) {
                instruction_length = 3;
            } else {
                instruction_length = 2;
            }
            break;
        case 0xC:
            // MUL/DIV is cluster-shared and Tier-2 bitfield operations are
            // absent. POPCNT/CLZ/CTZ/BITREV are core-local.
            private_instruction =
                subop >= 0x8 && subop <= 0xB;
            instruction_length =
                subop == 0xE ? 3 : 2;
            break;
        default:
            break;
    }

    if (!private_instruction) {
        return PrivateInstructionDisposition::
            SHARED_INSTRUCTION;
    }
    if (cache_identity_size != nullptr) {
        *cache_identity_size =
            prefix_length + instruction_length;
    }
    if (cacheable != nullptr)
        *cacheable = true;
    return PrivateInstructionDisposition::
        EXECUTE_PRIVATE;
}

static PrivateInstructionClassification
classify_private_core_instruction(
        CPUState& state) {
    PrivateInstructionClassification result;
    const uint64_t address = state.regs[state.psel];

    if (state.profile == CoreProfile::FULL) {
        if (
            state.ext_modifier != -1 ||
            !state.icache_enabled
        ) {
            result.disposition =
                classify_private_full_core_instruction(
                    state);
            return result;
        }
    } else if (state.profile == CoreProfile::MICRO) {
        if (
            state.ext_modifier != -1 ||
            state.priv_level != 0 ||
            micro_instruction_fetch_uses_python_route(
                address)
        ) {
            result.disposition =
                classify_private_micro_core_instruction(
                    state);
            return result;
        }
    } else {
        result.disposition =
            PrivateInstructionDisposition::
                SHARED_INSTRUCTION;
        return result;
    }

    result.decode_cache_lookup = true;
    result.decode_cache_hit =
        private_decode_cache_hit(
            state, address);
    if (result.decode_cache_hit) {
        result.disposition =
            PrivateInstructionDisposition::
                EXECUTE_PRIVATE;
    } else {
        int identity_size = 0;
        bool cacheable = false;
        result.disposition =
            state.profile == CoreProfile::FULL
            ? classify_private_full_core_instruction(
                state,
                &identity_size,
                &cacheable)
            : classify_private_micro_core_instruction(
                state,
                &identity_size,
                &cacheable);
        if (
            result.disposition ==
                PrivateInstructionDisposition::
                    EXECUTE_PRIVATE &&
            cacheable
        ) {
            private_decode_cache_store(
                state,
                address,
                identity_size);
        }
    }

    if (
        result.disposition ==
            PrivateInstructionDisposition::
                EXECUTE_PRIVATE &&
        state.profile == CoreProfile::MICRO
    ) {
        result.proof.core = &state;
        result.proof.address = address;
        result.proof.micro_native_private = true;
    }
    return result;
}

static bool classify_strict_cycle_private_one_cycle(
        const CPUState& state) {
    if (
        classify_private_full_core_instruction(state) !=
        PrivateInstructionDisposition::EXECUTE_PRIVATE
    ) {
        return false;
    }

    const std::optional<uint8_t> first =
        private_icache_peek(
            state, state.regs[state.psel]);
    if (!first.has_value())
        return false;
    const int family = (*first >> 4) & 0xF;
    const int subop = *first & 0xF;

    // Prefix decode itself costs a cycle. Taken short/long branches likewise
    // cost two cycles and remain on the resumable coordinator path. The list
    // below contains only encodings whose established native executor has an
    // exact one-cycle cost and cannot enter a callback or shared-memory path
    // under the stated privilege checks.
    switch (family) {
        case 0x0:
            if (
                subop == 0x0 ||
                subop == 0x1 ||
                subop == 0x2 ||
                subop == 0xC
            ) {
                return true;
            }
            return (
                (subop == 0x9 || subop == 0xA) &&
                state.priv_level == 0
            );
        case 0x1:
        case 0x2:
            return true;
        case 0x3:
        case 0x4:
            return !eval_cond(state, subop);
        case 0x6:
            return subop <= 0xB || state.priv_level == 0;
        case 0x7:
            return true;
        case 0x8:
            return state.priv_level == 0 && (
                subop == 0x6 ||
                subop == 0xA ||
                subop == 0xC ||
                subop == 0xD ||
                subop == 0xE
            );
        case 0xA:
        case 0xB:
            return state.priv_level == 0;
        case 0xC:
            return subop >= 0x8;
        default:
            return false;
    }
}

struct PrivateSharedAccessSignal {};

class PrivateSharedAccessSentinel final
        : public ResumableBusAccess {
public:
    uint64_t access(
            BusOperation,
            uint64_t,
            BusWidth,
            uint64_t,
            bool) override {
        throw PrivateSharedAccessSignal{};
    }
};

static int trap_id_from_runtime_error(const std::string& what);

struct FrontierPrivatePreclassification {
    bool execute_private = false;
    bool classified_instruction = false;
    bool decode_cache_lookup = false;
    bool decode_cache_hit = false;
    PrivateCoreResult result;
};

// Probe one unbounded-scheduler command under the same execution admission
// that protects its logical frontier. The probe performs no architectural
// mutation. A proven zero-progress boundary can therefore stay on the
// coordinator, while a proven-private first instruction is handed to the
// worker with a single-use classification proof.
static FrontierPrivatePreclassification
preclassify_frontier_private_command(
        const PrivateCoreCommand& command) noexcept {
    FrontierPrivatePreclassification probe;
    PrivateCoreResult& result = probe.result;
    result.command_sequence = 0;
    result.wave_epoch = 0;
    result.submission_index =
        command.submission_index;
    result.lane_index = command.lane_index;
    result.core_index = command.core_index;
    // A bypass is not a physical pool command. Sequence, wave, and thread
    // identity therefore remain zero; submission_index preserves the
    // coordinator's position within the planned physical cohort.

    if (command.core == nullptr) {
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.internal_error =
            "private command core is missing";
        return probe;
    }

    CPUState& core = *command.core;
    result.start_pc = pc(core);
    result.end_pc = result.start_pc;

    try {
        PrivateCoreExecutionScope execution_scope(
            core, command.admission);
        if (
            core.profile != CoreProfile::FULL &&
            core.profile != CoreProfile::MICRO
        ) {
            throw std::invalid_argument(
                "private execution requires a supported core profile");
        }
        if (
            core.system_cycle_execution_pending != nullptr &&
            core.system_cycle_execution_pending->load(
                std::memory_order_acquire) &&
            !command.strict_cycle_one_instruction
        ) {
            throw std::runtime_error(
                "private execution cannot enter a suspended "
                "cycle operation");
        }
        if (
            command.strict_cycle_one_instruction &&
            command.max_steps != 1
        ) {
            throw std::invalid_argument(
                "strict-cycle private execution requires exactly "
                "one instruction");
        }

        if (command.pending_interrupt_vector >= 0) {
            result.stop_reason =
                PrivateCoreStopReason::
                    INTERRUPT_BOUNDARY;
            result.interrupt_vector =
                command.pending_interrupt_vector;
        } else if (command.max_steps == 0) {
            result.stop_reason =
                PrivateCoreStopReason::
                    INSTRUCTION_LIMIT;
        } else if (core.halted) {
            result.stop_reason =
                PrivateCoreStopReason::HALTED;
        } else if (core.idle) {
            result.stop_reason =
                PrivateCoreStopReason::IDLE;
        } else {
            probe.classified_instruction = true;
            const PrivateInstructionClassification
                classification =
                    classify_private_core_instruction(
                        core);
            probe.decode_cache_lookup =
                classification.decode_cache_lookup;
            probe.decode_cache_hit =
                classification.decode_cache_hit;
            const PrivateInstructionDisposition
                disposition =
                    classification.disposition;
            if (
                disposition ==
                PrivateInstructionDisposition::
                    EXECUTE_PRIVATE
            ) {
                probe.execute_private = true;
            } else if (
                disposition ==
                PrivateInstructionDisposition::
                    ICACHE_BOUNDARY
            ) {
                result.stop_reason =
                    PrivateCoreStopReason::
                        ICACHE_BOUNDARY;
            } else {
                result.stop_reason =
                    PrivateCoreStopReason::
                        SHARED_INSTRUCTION;
            }
        }
    } catch (const std::exception& error) {
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.interrupt_vector = -1;
        result.internal_error = error.what();
    } catch (...) {
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.interrupt_vector = -1;
        result.internal_error =
            "unknown private execution failure";
    }

    result.end_pc = pc(core);
    return probe;
}

template <bool HOST_PROFILE>
static PrivateCoreResult execute_private_core_command_body(
        const PrivateCoreCommand& command) noexcept {
    PrivateCoreResult result;
    result.command_sequence = command.command_sequence;
    result.wave_epoch = command.wave_epoch;
    result.submission_index =
        command.submission_index;
    result.lane_index = command.lane_index;
    result.core_index = command.core_index;
    result.thread_token =
        current_private_thread_token();

    if (command.core == nullptr) {
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.internal_error =
            "private command core is missing";
        return result;
    }

    CPUState& core = *command.core;
    result.start_pc = pc(core);
    result.end_pc = result.start_pc;
    std::optional<CPUExecutionCheckpoint>
        command_checkpoint;
    auto restore_checkpoint = [&]() {
        if (!command_checkpoint.has_value())
            return;
        std::chrono::steady_clock::time_point
            restore_started{};
        if constexpr (HOST_PROFILE) {
            restore_started =
                std::chrono::steady_clock::now();
        }
        command_checkpoint->restore(core);
        if constexpr (HOST_PROFILE) {
            host_saturating_increment(
                command.host_telemetry->
                    checkpoint_restores);
            host_saturating_add(
                command.host_telemetry->
                    checkpoint_restore_ns,
                host_elapsed_ns(restore_started));
        }
    };
    auto capture_checkpoint = [&]() {
        if (command_checkpoint.has_value())
            return;
        std::chrono::steady_clock::time_point
            checkpoint_started{};
        if constexpr (HOST_PROFILE) {
            checkpoint_started =
                std::chrono::steady_clock::now();
        }
        command_checkpoint.emplace(core);
        if constexpr (HOST_PROFILE) {
            command.host_telemetry->
                checkpoint_captures = 1;
            command.host_telemetry->
                checkpoint_capture_ns =
                host_elapsed_ns(checkpoint_started);
        }
    };

    try {
        std::chrono::steady_clock::time_point
            scope_started{};
        if constexpr (HOST_PROFILE) {
            scope_started =
                std::chrono::steady_clock::now();
        }
        PrivateCoreExecutionScope execution_scope(
            core, command.admission);
        if constexpr (HOST_PROFILE) {
            command.host_telemetry->scope_setup_ns =
                host_elapsed_ns(scope_started);
        }

        if (
            core.profile != CoreProfile::FULL &&
            core.profile != CoreProfile::MICRO
        ) {
            throw std::invalid_argument(
                "private execution requires a supported core profile");
        }
        if (
            core.system_cycle_execution_pending != nullptr &&
            core.system_cycle_execution_pending->load(
                std::memory_order_acquire) &&
            !command.strict_cycle_one_instruction
        ) {
            throw std::runtime_error(
                "private execution cannot enter a suspended "
                "cycle operation");
        }
        if (
            command.strict_cycle_one_instruction &&
            command.max_steps != 1
        ) {
            throw std::invalid_argument(
                "strict-cycle private execution requires exactly "
                "one instruction");
        }

        StepCallbacks callbacks{};
        PrivateSharedAccessSentinel shared_access_sentinel;
        callbacks.bus_access =
            &shared_access_sentinel;

        if (command.pending_interrupt_vector >= 0) {
            result.stop_reason =
                PrivateCoreStopReason::
                    INTERRUPT_BOUNDARY;
            result.interrupt_vector =
                command.pending_interrupt_vector;
            result.end_pc = pc(core);
            return result;
        }

        result.stop_reason =
            PrivateCoreStopReason::INSTRUCTION_LIMIT;
        for (int step_index = 0;
             step_index < command.max_steps;
             step_index++) {
            if (core.halted) {
                result.stop_reason =
                    PrivateCoreStopReason::HALTED;
                break;
            }
            if (core.idle) {
                result.stop_reason =
                    PrivateCoreStopReason::IDLE;
                break;
            }

            PrivateInstructionClassification
                classification;
            classification.disposition =
                PrivateInstructionDisposition::
                    EXECUTE_PRIVATE;
            if (
                step_index != 0 ||
                !command
                    .first_instruction_preclassified_private
            ) {
                if constexpr (HOST_PROFILE) {
                    host_saturating_increment(
                        command.host_telemetry->
                            classification_calls);
                }
                // Full-core frontier admission reuses host plans, but the
                // established in-worker classifier already walks the same
                // resident guest-cache bytes cheaply. Complete identity
                // validation there costs more than decoding, so retain the
                // direct classifier inside a private span. Microcores still
                // use the plan because it also removes their duplicate
                // Python-oracle eligibility decode.
                if (
                    command.strict_cycle_one_instruction ||
                    core.profile == CoreProfile::FULL
                ) {
                    classification.disposition =
                        classify_private_full_core_instruction(
                            core);
                } else {
                    classification =
                        classify_private_core_instruction(
                            core);
                }
                if constexpr (HOST_PROFILE) {
                    if (
                        classification
                            .decode_cache_lookup
                    ) {
                        host_saturating_increment(
                            command.host_telemetry->
                                decode_cache_lookups);
                        if (
                            classification
                                .decode_cache_hit
                        ) {
                            host_saturating_increment(
                                command.host_telemetry->
                                    decode_cache_hits);
                        } else {
                            host_saturating_increment(
                                command.host_telemetry->
                                    decode_cache_misses);
                        }
                    }
                }
            }
            if (
                classification.disposition ==
                PrivateInstructionDisposition::
                    ICACHE_BOUNDARY
            ) {
                result.stop_reason =
                    PrivateCoreStopReason::
                        ICACHE_BOUNDARY;
                break;
            }
            if (
                classification.disposition ==
                PrivateInstructionDisposition::
                    SHARED_INSTRUCTION
            ) {
                result.stop_reason =
                    PrivateCoreStopReason::
                        SHARED_INSTRUCTION;
                break;
            }

            // Everything above is validation or read-only classification.
            // Preserve whole-command rollback by taking the full checkpoint
            // immediately before the first admitted guest mutation.
            capture_checkpoint();
            try {
                const PrivateInstructionProof* proof =
                    classification.proof
                        .micro_native_private
                    ? &classification.proof
                    : nullptr;
                if constexpr (HOST_PROFILE) {
                    if (proof != nullptr) {
                        host_saturating_increment(
                            command.host_telemetry->
                                micro_oracle_proof_reuses);
                    }
                }
                const int cycles =
                    step_one(core, callbacks, proof);
                result.total_cycles += cycles;
                result.steps_executed++;
            } catch (
                    const PrivateSharedAccessSignal&) {
                throw std::logic_error(
                    "private instruction reached a shared "
                    "bus access");
            } catch (const std::runtime_error& error) {
                const std::string what = error.what();
                if (what == "TRAP:RESET") {
                    result.stop_reason =
                        PrivateCoreStopReason::RESET;
                    break;
                }
                if (
                    what.size() >= 5 &&
                    what.compare(0, 5, "TRAP:") == 0
                ) {
                    result.stop_reason =
                        PrivateCoreStopReason::TRAP;
                    result.trap_id =
                        trap_id_from_runtime_error(what);
                    break;
                }
                throw;
            }

            if (core.halted) {
                result.stop_reason =
                    PrivateCoreStopReason::HALTED;
                break;
            }
            if (core.idle) {
                result.stop_reason =
                    PrivateCoreStopReason::IDLE;
                break;
            }
        }
        if (
            command.strict_cycle_one_instruction &&
            (
                result.steps_executed != 1 ||
                result.total_cycles != 1
            )
        ) {
            throw std::logic_error(
                "strict-cycle private preclassification diverged "
                "from one-cycle execution");
        }
    } catch (const std::exception& error) {
        restore_checkpoint();
        result.steps_executed = 0;
        result.total_cycles = 0;
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.trap_id = -1;
        result.interrupt_vector = -1;
        result.internal_error = error.what();
    } catch (...) {
        restore_checkpoint();
        result.steps_executed = 0;
        result.total_cycles = 0;
        result.stop_reason =
            PrivateCoreStopReason::INTERNAL_FAILURE;
        result.trap_id = -1;
        result.interrupt_vector = -1;
        result.internal_error =
            "unknown private execution failure";
    }

    result.end_pc = pc(core);
    return result;
}

static PrivateCoreResult execute_private_core_command(
        const PrivateCoreCommand& command) noexcept {
    if (command.host_telemetry == nullptr) {
        return execute_private_core_command_body<
            false>(
            command);
    }
    const auto started =
        std::chrono::steady_clock::now();
    PrivateCoreResult result =
        execute_private_core_command_body<true>(
            command);
    command.host_telemetry->execution_ns =
        host_elapsed_ns(started);
    return result;
}

// ---------------------------------------------------------------------------
//  run_steps — run N steps in C++, calling back to Python for MMIO
// ---------------------------------------------------------------------------

struct RunResult {
    int64_t total_cycles;
    int steps_executed;
    int stop_reason;
    int trap_id;
};

enum RunStopReason {
    RUN_LIMIT = 0,
    RUN_HALT = 1,
    RUN_IDLE = 2,
    RUN_MEX_FALLBACK = 3,
    RUN_EXT_FALLBACK = 4,
    RUN_TRAP = 5,
    RUN_RESET = 6,
};

static int trap_id_from_runtime_error(const std::string& what) {
    if (what.find("SW_TRAP") != std::string::npos)
        return IVEC_SW_TRAP;
    if (what.find("DIV_ZERO") != std::string::npos)
        return IVEC_DIV_ZERO;
    if (what.find("PRIV_FAULT") != std::string::npos)
        return IVEC_PRIV_FAULT;
    if (what.find("BUS_FAULT") != std::string::npos)
        return IVEC_BUS_FAULT;
    return IVEC_ILLEGAL_OP;
}

static RunResult run_steps(CPUState& s, const StepCallbacks& cb, int max_steps) {
    // GIL is released by the caller (pybind11 binding).  All Python
    // callbacks in StepCallbacks reacquire it as needed.  This lets
    // background threads (display, NIC RX) run freely while the CPU
    // inner loop executes pure C++.
    RunResult result = {0, 0, RUN_LIMIT, -1};

    for (int i = 0; i < max_steps; i++) {
        if (s.halted) { result.stop_reason = RUN_HALT; break; }
        if (s.idle) { result.stop_reason = RUN_IDLE; break; }

        try {
            int cycles = step_one(s, cb);
            result.total_cycles += cycles;
            result.steps_executed++;
        } catch (const std::runtime_error& e) {
            std::string what = e.what();
            if (what == "HALT") {
                result.stop_reason = RUN_HALT;
                break;
            } else if (what.substr(0, 5) == "TRAP:") {
                // Preserve the completed native prefix and let the wrapper
                // perform the same trap/reset path used by single-step
                // execution.  The faulting instruction is pending and is not
                // included in steps_executed or total_cycles.
                if (what == "TRAP:RESET") {
                    result.stop_reason = RUN_RESET;
                } else {
                    result.stop_reason = RUN_TRAP;
                    result.trap_id = trap_id_from_runtime_error(what);
                }
                break;
            } else if (what == "MEX_FALLBACK") {
                // The instruction was rewound transactionally by step_one.
                // Preserve the completed native prefix in RunResult and let
                // the wrapper execute exactly this one instruction in Python.
                result.stop_reason = RUN_MEX_FALLBACK;
                break;
            } else if (what == "EXT_ISA_FALLBACK") {
                // Unhandled EXT ISA op; like MEX fallback, its PC is rewound.
                result.stop_reason = RUN_EXT_FALLBACK;
                break;
            } else {
                throw;
            }
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
//  Native SystemState deterministic scheduler
// ---------------------------------------------------------------------------

enum class SystemStopReason : uint8_t {
    INSTRUCTION_LIMIT = 0,
    CYCLE_LIMIT = 1,
    EVENT_HORIZON = 2,
    ALL_HALTED = 3,
    ALL_IDLE = 4,
    UNHANDLED_INTERRUPT = 5,
    NO_PROGRESS = 6,
};

struct SystemBatchResult {
    int64_t instructions_executed = 0;
    uint64_t system_cycles_advanced = 0;
    std::vector<int64_t> per_core_instructions;
    std::vector<int64_t> per_core_cycles;
    std::vector<uint64_t> per_core_dispatches;
    std::vector<uint64_t> per_core_interrupts;
    std::vector<std::array<uint64_t, 7>> per_core_stop_reasons;
    uint64_t rounds = 0;
    uint64_t continuations = 0;
    uint64_t interrupts_delivered = 0;
    uint64_t external_events_applied = 0;
    int scheduler_cursor = 0;
    SystemStopReason system_stop_reason =
        SystemStopReason::INSTRUCTION_LIMIT;
    uint64_t stop_cycle = 0;
    uint64_t event_source_mask = 0;
    int pending_interrupt_core = -1;
    int pending_interrupt_vector = -1;
};

struct PendingClusterRequest {
    ClusterResourceKind resource =
        ClusterResourceKind::NONE;
    int operation = -1;
    bool sha_transaction = false;
    bool sha_lock_protected = false;
    int continuation_reason = RUN_EXT_FALLBACK;
    uint8_t encoding_length = 0;
    std::array<uint8_t, 16> encoding{};
};

struct CoreDispatchResult {
    int64_t steps = 0;
    int64_t cycles = 0;
    uint64_t dispatches = 0;
    uint64_t continuations = 0;
    std::array<uint64_t, 7> stop_reasons{};
};

struct CoreFrontierReservation {
    int core_index = -1;
    int64_t max_steps = 0;
};

struct FrontierCreditTransfer {
    int donor_core = -1;
    int recipient_core = -1;
    int64_t amount = 0;
};

struct CoreFrontierOutcome {
    int64_t steps = 0;
    int64_t cycles = 0;
    bool interrupt_boundary = false;
    bool coordinator_state_changed = false;
    std::vector<int> interrupt_cores;
    std::vector<int> cluster_deferred_cores;
    std::vector<int> cluster_lost_cores;
    std::vector<FrontierCreditTransfer>
        cluster_credit_transfers;
    std::vector<int> dispatch_boundary_cores;
    std::vector<int> terminal_cores;
};

static std::vector<PrivateCoreResult>
execute_private_core_wave_under_active_batch(
    SystemState& system,
    std::vector<PrivateCoreCommand> commands,
    std::shared_ptr<SharedMemoryExecutionAdmission>
        frontier_admission = nullptr);

static std::vector<PrivateCoreResult>
execute_strict_cycle_private_wave_under_active_batch(
    SystemState& system,
    std::vector<PrivateCoreCommand> commands,
    std::shared_ptr<SharedMemoryExecutionAdmission>
        frontier_admission);

static void run_parallel_core_round(
    SystemState& system,
    const std::vector<CoreFrontierReservation>& reservations,
    const std::vector<StepCallbacks>& callbacks,
    const py::function& settle_continuation,
    const py::function& settle_dispatch_error,
    SystemBatchResult& result,
    CoreFrontierOutcome& outcome);

static int pending_enabled_core_interrupt(
        const SystemState& system,
        const CPUState& core) {
    if (core.halted || !core.flag_i)
        return -1;
    if (system.shared_interrupts.ipi_line(core.core_id))
        return IVEC_IPI;
    if (system.shared_timer.irq_pending)
        return IVEC_TIMER;
    return -1;
}

static PendingClusterRequest classify_pending_cluster_request(
        SystemState& system,
        CPUState& core,
        int continuation_reason) {
    PendingClusterRequest request;
    request.continuation_reason = continuation_reason;
    if (
        core.profile != CoreProfile::MICRO ||
        (
            continuation_reason != RUN_MEX_FALLBACK &&
            continuation_reason != RUN_EXT_FALLBACK
        )
    ) {
        return request;
    }

    uint64_t address = pc(core);
    const uint64_t instruction_address = address;
    if (
        micro_instruction_fetch_window_touches_mmio(
            address)
    ) {
        // Reject the complete possible decode window before reading even an
        // ordinary-looking first byte. A later operand fetch could otherwise
        // reach side-effecting MMIO after cluster classification returned.
        throw std::runtime_error(
            "native system batch does not support "
            "reduced-core MMIO instruction fetch");
    }
    const int micro_index =
        static_cast<int>(core.core_id) -
        system.full_core_count();
    const int cluster_index =
        micro_index /
        SystemState::MICRO_CORES_PER_CLUSTER;
    if (
        micro_index < 0 ||
        cluster_index < 0 ||
        cluster_index >=
            static_cast<int>(
                system.cluster_states.size())
    ) {
        throw std::logic_error(
            "reduced-core instruction fetch has no "
            "owning cluster");
    }
    ClusterState& scratchpad_cluster =
        system.cluster_states[
            static_cast<std::size_t>(
                cluster_index)];
    auto read_instruction_byte =
        [&](uint64_t byte_address) {
            if (
                static_cast<uint32_t>(
                    byte_address >> 32) ==
                0xFFFF'FE00U
            ) {
                return scratchpad_cluster.scratchpad[
                    static_cast<std::size_t>(
                        static_cast<uint32_t>(
                            byte_address) %
                        scratchpad_cluster
                            .scratchpad.size())];
            }
            constexpr uint64_t MMIO_START =
                0xFFFF'FF00'0000'0000ULL;
            constexpr uint64_t MMIO_END =
                0xFFFF'FF80'0000'0000ULL;
            if (
                byte_address >= MMIO_START &&
                byte_address < MMIO_END
            ) {
                // An MMIO classification read can have side effects. Fail
                // before touching that byte rather than changing the stream
                // or silently bypassing cluster arbitration.
                throw std::runtime_error(
                    "native system batch does not support "
                    "reduced-core MMIO instruction fetch");
            }
            return mem_read8(core, byte_address);
        };
    uint8_t opcode =
        read_instruction_byte(address);
    int family = (opcode >> 4) & 0xF;
    int subop = opcode & 0xF;
    int modifier = -1;
    int prefix_length = 0;
    int decoded_length = 0;

    // Every F-family opcode other than the three self-contained engines is a
    // modifier in the Python reduced-core oracle. Preserve it as part of the
    // exact request identity while classifying the following instruction.
    if (
        family == 0xF &&
        subop != 0x9 &&
        subop != 0xA &&
        subop != 0xB
    ) {
        modifier = subop;
        prefix_length = 1;
        address++;
        opcode = read_instruction_byte(address);
        family = (opcode >> 4) & 0xF;
        subop = opcode & 0xF;
    }

    switch (family) {
        case 0x0:
            if (
                subop == 0x4 ||
                subop == 0xD ||
                subop == 0xE ||
                subop == 0xF
            ) {
                request.resource = ClusterResourceKind::BUS;
                request.operation = opcode;
                decoded_length =
                    subop == 0xD ? 2 : 1;
            }
            break;
        case 0x5:
            request.resource = ClusterResourceKind::BUS;
            request.operation = opcode;
            decoded_length =
                subop == 0xF ? 3 : 2;
            break;
        case 0xC:
            if (subop <= 0x7) {
                request.resource =
                    ClusterResourceKind::MUL_DIV;
                request.operation = subop;
                decoded_length = 2;
            }
            break;
        case 0xD: {
            const uint8_t csr =
                read_instruction_byte(address + 1);
            const bool shared_acc =
                csr >= CSR_ACC0 && csr <= CSR_ACC3;
            const bool shared_sha =
                csr == CSR_SHA_MODE ||
                csr == CSR_SHA_MSGLEN ||
                csr == CSR_SHA_MSGLEN_HI;
            if (shared_acc || shared_sha) {
                request.resource =
                    ClusterResourceKind::TILE_ENGINE;
                request.sha_lock_protected = true;
                request.operation =
                    (static_cast<int>(opcode) << 8) |
                    static_cast<int>(csr);
                decoded_length = 2;
            }
            break;
        }
        case 0xE: {
            request.resource =
                ClusterResourceKind::TILE_ENGINE;
            request.operation = subop;
            const uint8_t function =
                read_instruction_byte(address + 1);
            const int source_selector =
                (subop >> 2) & 0x3;
            const int mex_operation =
                subop & 0x3;
            const int effective_function =
                source_selector == 0x2
                ? 0
                : function & 0x7;
            request.sha_lock_protected =
                mex_operation == 0x2 ||
                (
                    mex_operation == 0x1 &&
                    (
                        effective_function == 0x1 ||
                        effective_function == 0x5
                    )
                );
            decoded_length =
                2 +
                (source_selector == 0x1 ? 1 : 0) +
                (
                    mex_operation == 0x3 &&
                    effective_function == 0x7 &&
                    modifier != 0x8
                    ? 1
                    : 0
                );
            break;
        }
        case 0xF:
            if (subop == 0xB) {
                const uint8_t crypto_subop =
                    read_instruction_byte(address + 1);
                const int unit = (crypto_subop >> 4) & 0xF;
                request.operation = crypto_subop & 0xF;
                if (
                    unit == 0x0 &&
                    request.operation <= 0x5
                ) {
                    request.resource =
                        ClusterResourceKind::CRC;
                    decoded_length =
                        request.operation == 0x0
                        ? 2
                        : 3;
                } else if (
                    unit == 0x1 &&
                    request.operation <= 0x6
                ) {
                    request.resource =
                        ClusterResourceKind::TILE_ENGINE;
                    request.sha_transaction = true;
                    request.sha_lock_protected = true;
                    decoded_length =
                        (
                            request.operation == 0x1 ||
                            request.operation == 0x2 ||
                            request.operation == 0x5 ||
                            request.operation == 0x6
                        )
                        ? 2
                        : 3;
                }
            }
            break;
        default:
            break;
    }
    if (request.resource != ClusterResourceKind::NONE) {
        const int encoding_length =
            prefix_length + decoded_length;
        if (
            encoding_length <= 0 ||
            encoding_length >
                static_cast<int>(
                    request.encoding.size())
        ) {
            throw std::logic_error(
                "cluster request encoding length is invalid");
        }
        request.encoding_length =
            static_cast<uint8_t>(encoding_length);
        for (
            int offset = 0;
            offset < encoding_length;
            offset++
        ) {
            request.encoding[
                static_cast<std::size_t>(offset)] =
                    read_instruction_byte(
                        instruction_address +
                            static_cast<uint64_t>(
                                offset));
        }
    }
    return request;
}

struct NativeBatchActiveGuard {
    SystemState& system;

    explicit NativeBatchActiveGuard(SystemState& system_value)
        : system(system_value) {
        if (system.native_batch_active.exchange(
                true, std::memory_order_acq_rel)) {
            throw std::runtime_error(
                "native system batch is already active");
        }
    }

    NativeBatchActiveGuard(const NativeBatchActiveGuard&) = delete;
    NativeBatchActiveGuard& operator=(
        const NativeBatchActiveGuard&) = delete;

    ~NativeBatchActiveGuard() {
        system.native_batch_active.store(
            false, std::memory_order_release);
    }
};

struct ConcurrencyProfileBatchGuard {
    SystemState& system;

    explicit ConcurrencyProfileBatchGuard(
            SystemState& system_value,
            bool enabled)
        : system(system_value) {
        if (system.concurrency_profile_batch_active) {
            throw std::logic_error(
                "native concurrency profile batch is already active");
        }
        system.concurrency_profile_batch_active =
            enabled;
    }

    ConcurrencyProfileBatchGuard(
        const ConcurrencyProfileBatchGuard&) = delete;
    ConcurrencyProfileBatchGuard& operator=(
        const ConcurrencyProfileBatchGuard&) = delete;

    ~ConcurrencyProfileBatchGuard() {
        system.concurrency_profile_batch_active =
            false;
    }
};

static int64_t checked_scheduler_add(
        int64_t current,
        int64_t increment,
        const char* field_name) {
    if (current < 0 || increment < 0 ||
        current > std::numeric_limits<int64_t>::max() - increment) {
        throw std::overflow_error(
            std::string("native scheduler ") +
            field_name + " overflow");
    }
    return current + increment;
}

static void checked_scheduler_increment(
        uint64_t& counter,
        const char* field_name) {
    if (counter == std::numeric_limits<uint64_t>::max()) {
        throw std::overflow_error(
            std::string("native scheduler ") +
            field_name + " overflow");
    }
    counter++;
}

static bool system_all_halted(const SystemState& system) {
    return std::all_of(
        system.execution_cores.begin(),
        system.execution_cores.end(),
        [](const CPUState* core) {
            return core->halted;
        });
}

static bool system_all_idle_or_halted(const SystemState& system) {
    return std::all_of(
        system.execution_cores.begin(),
        system.execution_cores.end(),
        [](const CPUState* core) {
            return core->halted || core->idle;
        });
}

static void merge_core_dispatch(
        SystemBatchResult& result,
        int core_index,
        const CoreDispatchResult& dispatch) {
    const std::size_t index = static_cast<std::size_t>(core_index);
    result.per_core_instructions[index] = checked_scheduler_add(
        result.per_core_instructions[index],
        dispatch.steps,
        "per-core instruction accounting");
    result.per_core_cycles[index] = checked_scheduler_add(
        result.per_core_cycles[index],
        dispatch.cycles,
        "per-core cycle accounting");
    result.per_core_dispatches[index] += dispatch.dispatches;
    result.continuations += dispatch.continuations;
    for (std::size_t reason = 0;
         reason < dispatch.stop_reasons.size();
         reason++) {
        result.per_core_stop_reasons[index][reason] +=
            dispatch.stop_reasons[reason];
    }
}

struct DeferredClusterRequest {
    int core_index = -1;
    int cluster_index = -1;
    int local_core = -1;
    uint64_t instruction_pc = 0;
    PendingClusterRequest request{};
};

static SystemBatchResult run_native_system_batch(
        SystemState& system,
        int64_t max_steps,
        const std::vector<StepCallbacks>& callbacks,
        const py::function& prepare_batch,
        const py::function& settle_continuation,
        const py::function& settle_dispatch_error,
        const py::function& settle_round,
        int max_dispatch_steps) {
    const std::size_t core_count =
        system.execution_cores.size();
    SystemBatchResult result;
    result.per_core_instructions.assign(core_count, 0);
    result.per_core_cycles.assign(core_count, 0);
    result.per_core_dispatches.assign(core_count, 0);
    result.per_core_interrupts.assign(core_count, 0);
    result.per_core_stop_reasons.assign(core_count, {});
    result.scheduler_cursor = system.scheduler_cursor;

    if (max_steps <= 0) {
        result.system_stop_reason =
            SystemStopReason::INSTRUCTION_LIMIT;
        result.stop_cycle = system.shared_clock.cycles();
        return result;
    }
    if (callbacks.size() != core_count)
        throw std::invalid_argument(
            "one callback set is required for every execution core");
    if (max_dispatch_steps <= 0)
        throw std::invalid_argument(
            "max_dispatch_steps must be positive");

    if (system.main_bus.active_timeout_cycle().has_value()) {
        throw std::runtime_error(
            "active main-bus grants require cycle-bounded native execution");
    }
    if (system.has_cycle_execution_pending()) {
        throw std::runtime_error(
            "suspended cycle execution requires cycle-bounded "
            "native execution");
    }
    if (system.external_events.next_cycle().has_value()) {
        throw std::runtime_error(
            "pending external events require cycle-bounded "
            "native execution");
    }
    NativeBatchActiveGuard active_guard(system);
    const auto horizon = system.shared_clock.snapshot();
    if (horizon.has_deadline) {
        throw std::runtime_error(
            "active event horizons require cycle-bounded native execution");
    }

    const uint64_t clock_start = system.shared_clock.cycles();
    checked_scheduler_increment(
        system.native_batch_runs,
        "batch counter");
    ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    const bool host_profile_enabled =
        profile.enabled;
    ConcurrencyProfileBatchGuard
        profile_batch_guard(
            system,
            host_profile_enabled);
    if (host_profile_enabled) {
        host_saturating_increment(
            profile.batches);
    }
    HostProfileWallTimer batch_timer(
        host_profile_enabled,
        &profile.batch_total_ns);
    auto profiled_settle_round = [&](
            int64_t cycles,
            bool deliver_interrupts,
            bool batch_end,
            bool record_boundary) {
        if (host_profile_enabled) {
            host_saturating_increment(
                profile.settle_round_calls);
        }
        HostProfileWallTimer settle_timer(
            host_profile_enabled,
            &profile.settle_round_ns);
        settle_round(
            cycles,
            deliver_interrupts,
            batch_end,
            record_boundary);
    };

    // Wake checks are a Python compatibility boundary, but execute while the
    // native scheduler mutex excludes deadline and clock mutation.
    if (host_profile_enabled) {
        host_saturating_increment(
            profile.prepare_batch_calls);
    }
    {
        HostProfileWallTimer prepare_timer(
            host_profile_enabled,
            &profile.prepare_batch_ns);
        prepare_batch();
    }

    const bool any_active = std::any_of(
        system.execution_cores.begin(),
        system.execution_cores.end(),
        [](const CPUState* core) {
            return !core->halted && !core->idle;
        });
    if (!any_active) {
        result.system_stop_reason =
            system_all_halted(system)
            ? SystemStopReason::ALL_HALTED
            : SystemStopReason::ALL_IDLE;
        result.system_cycles_advanced =
            system.shared_clock.cycles() - clock_start;
        result.stop_cycle =
            system.shared_clock.cycles();
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }

    int64_t remaining = max_steps;
    while (remaining > 0 && !system_all_halted(system)) {
        if (system_all_idle_or_halted(system))
            break;

        const int round_start =
            system.scheduler_cursor %
            static_cast<int>(core_count);
        const int64_t active_count =
            static_cast<int64_t>(
                std::count_if(
                    system.execution_cores.begin(),
                    system.execution_cores.end(),
                    [](const CPUState* core) {
                        return
                            !core->halted &&
                            !core->idle;
                    }));
        if (active_count == 0)
            break;

        const int64_t equal_round_quantum =
            std::min<int64_t>(
                max_dispatch_steps,
                remaining / active_count +
                    (
                        remaining % active_count != 0
                            ? 1
                            : 0
                    ));
        int64_t unreserved = remaining;
        std::vector<CoreFrontierReservation>
            reservations;
        reservations.reserve(
            static_cast<std::size_t>(
                active_count));
        for (
            std::size_t offset = 0;
            offset < core_count;
            offset++
        ) {
            const int core_index = (
                round_start +
                static_cast<int>(offset)
            ) % static_cast<int>(core_count);
            const CPUState& core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        core_index)];
            if (core.halted || core.idle)
                continue;
            const int64_t reservation =
                std::min<int64_t>(
                    equal_round_quantum,
                    unreserved);
            reservations.push_back(
                CoreFrontierReservation{
                    core_index,
                    reservation,
                });
            unreserved -= reservation;
        }
        if (reservations.empty())
            break;

        CoreFrontierOutcome outcome;
        try {
            run_parallel_core_round(
                system,
                reservations,
                callbacks,
                settle_continuation,
                settle_dispatch_error,
                result,
                outcome);
        } catch (...) {
            // Every physical cohort in the failing sub-frontier reached
            // the same private boundary before ordered settlement began.
            // Settle those prefixes, all earlier successful boundaries,
            // and all prior sub-frontiers in this scheduler round.
            profiled_settle_round(
                outcome.cycles,
                true,
                true,
                false);
            throw;
        }

        result.instructions_executed =
            checked_scheduler_add(
                result.instructions_executed,
                outcome.steps,
                "aggregate instruction accounting");
        remaining -= outcome.steps;
        // A complete equal-credit scheduler round, rather than a
        // physical cohort or cache/shared sub-frontier, is the unbounded
        // scheduler's clock, device, and interrupt boundary.
        profiled_settle_round(
            outcome.cycles,
            true,
            false,
            true);
        result.rounds++;

        if (outcome.steps != 0)
            continue;
        if (!outcome.interrupt_boundary)
            break;

        bool interrupt_still_eligible = false;
        for (int core_index : outcome.interrupt_cores) {
            const CPUState& core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        core_index)];
            if (
                pending_enabled_core_interrupt(
                    system, core) >= 0
            ) {
                interrupt_still_eligible = true;
                break;
            }
        }
        if (interrupt_still_eligible)
            break;
    }

    profiled_settle_round(
        0, false, true, false);
    result.system_cycles_advanced =
        system.shared_clock.cycles() -
        clock_start;
    if (remaining == 0) {
        result.system_stop_reason =
            SystemStopReason::INSTRUCTION_LIMIT;
    } else if (system_all_halted(system)) {
        result.system_stop_reason =
            SystemStopReason::ALL_HALTED;
    } else if (system_all_idle_or_halted(system)) {
        result.system_stop_reason =
            SystemStopReason::ALL_IDLE;
    } else {
        result.system_stop_reason =
            SystemStopReason::NO_PROGRESS;
    }
    result.stop_cycle =
        system.shared_clock.cycles();
    result.scheduler_cursor =
        system.scheduler_cursor;
    return result;
}

static uint64_t checked_cycle_add(
        uint64_t cycle,
        uint64_t delta,
        const char* field_name) {
    if (delta > std::numeric_limits<uint64_t>::max() - cycle) {
        throw std::overflow_error(
            std::string(field_name) + " overflow");
    }
    return cycle + delta;
}

static bool restore_cycle_instruction_checkpoint(
        ResumableInstruction& instruction,
        CPUState& core) {
    // The epoch protects validation-only fallbacks too. They do not publish
    // BUSY, but restoring their pre-reset checkpoint would still resurrect
    // detached PC/register state.
    if (
        instruction.tacc_python_fallback &&
        core.tacc_epoch != instruction.tacc_operation_epoch
    ) {
        return false;
    }
    if (!instruction.tacc_python_fallback) {
        instruction.checkpoint.restore(core);
        return true;
    }

    const std::array<uint8_t, TACC_IMAGE_BYTES> live_image =
        core.tacc;
    const uint8_t live_owner = core.tacc_owner;
    const bool live_valid = core.tacc_valid;
    const bool live_dirty = core.tacc_dirty;
    const uint8_t live_format_ew =
        core.tacc_format_ew;
    const uint8_t live_format_signed =
        core.tacc_format_signed;
    const bool live_busy = core.tacc_busy;
    const bool live_force_pending =
        core.tacc_force_pending;
    const uint64_t live_epoch = core.tacc_epoch;
    instruction.checkpoint.restore(core);
    core.tacc = live_image;
    core.tacc_owner = live_owner;
    core.tacc_valid = live_valid;
    core.tacc_dirty = live_dirty;
    core.tacc_format_ew = live_format_ew;
    core.tacc_format_signed = live_format_signed;
    core.tacc_busy = live_busy;
    core.tacc_force_pending = live_force_pending;
    core.tacc_epoch = live_epoch;
    return true;
}

static void finish_cycle_tacc_terminal(CPUState& core) noexcept {
    core.tacc_busy = false;
    if (core.tacc_force_pending)
        core.reset_tacc();
}

static RunResult run_one_system_instruction(
        CPUState& core,
        const StepCallbacks& callbacks) {
    RunResult result{0, 0, RUN_LIMIT, -1};
    try {
        result.total_cycles = step_one(core, callbacks);
        result.steps_executed = 1;
    } catch (const std::runtime_error& error) {
        const std::string what = error.what();
        if (what == "HALT") {
            result.stop_reason = RUN_HALT;
        } else if (what.rfind("TRAP:", 0) == 0) {
            if (what == "TRAP:RESET") {
                result.stop_reason = RUN_RESET;
            } else {
                result.stop_reason = RUN_TRAP;
                result.trap_id =
                    trap_id_from_runtime_error(what);
            }
        } else if (what == "MEX_FALLBACK") {
            result.stop_reason = RUN_MEX_FALLBACK;
        } else if (what == "EXT_ISA_FALLBACK") {
            result.stop_reason = RUN_EXT_FALLBACK;
        } else {
            throw;
        }
    }
    return result;
}

static void execute_cycle_interrupt_entry(
        CPUState& core,
        int interrupt_vector,
        const StepCallbacks& callbacks) {
    if (interrupt_vector < 0 || interrupt_vector > 255)
        throw std::logic_error("cycle interrupt vector is invalid");
    if (core.ivt_base == 0)
        throw std::logic_error(
            "cycle interrupt entry has no installed IVT");

    // Preserve the selected accelerated-emulator trap frame while routing
    // every visible stack/vector access through the resumable main bus.
    // The producer remains level asserted until guest software acknowledges
    // it; accepting an interrupt only masks IE and changes core-private state.
    sys_push64(
        core,
        callbacks,
        flags_pack(core) |
            (static_cast<uint64_t>(core.priv_level) << 8));
    sys_push64(core, callbacks, pc(core));
    core.flag_i = 0;
    core.priv_level = 0;
    core.idle = false;
    core.ivec_id = static_cast<uint64_t>(interrupt_vector);
    pc(core) = sys_read64(
        core,
        callbacks,
        core.ivt_base +
            static_cast<uint64_t>(interrupt_vector) * 8);
}

enum class CycleCoreProgress {
    RETIRED,
    WAITING_BUS,
    BLOCKED_BY_CYCLE_LIMIT,
    TERMINAL,
};

static uint64_t pending_instruction_count(
        const SystemState& system) {
    return static_cast<uint64_t>(std::count_if(
        system.full_core_cycle_states.begin(),
        system.full_core_cycle_states.end(),
        [](const FullCoreCycleState& state) {
            return state.instruction != nullptr;
        }));
}

static uint64_t pending_guest_instruction_count(
        const SystemState& system) {
    return static_cast<uint64_t>(std::count_if(
        system.full_core_cycle_states.begin(),
        system.full_core_cycle_states.end(),
        [](const FullCoreCycleState& state) {
            return state.instruction != nullptr &&
                state.instruction->kind ==
                    CycleOperationKind::GUEST_INSTRUCTION;
        }));
}

static CycleCoreProgress run_cycle_interrupt_once(
        SystemState& system,
        int core_index,
        const StepCallbacks& base_callbacks,
        SystemBatchResult& batch_result) {
    const std::size_t index =
        static_cast<std::size_t>(core_index);
    CPUState& core = *system.cores[index];
    FullCoreCycleState& cycle_state =
        system.full_core_cycle_states[index];
    ResumableInstruction* operation =
        cycle_state.instruction.get();
    if (operation == nullptr ||
        operation->kind !=
            CycleOperationKind::INTERRUPT_ENTRY) {
        throw std::logic_error(
            "cycle interrupt runner has no interrupt operation");
    }
    operation->checkpoint.restore(core);
    operation->replay_cursor = 0;
    JournaledBusAccess bus_access(system, core_index);
    StepCallbacks callbacks = base_callbacks;
    callbacks.bus_access = &bus_access;

    auto logical_guard =
        acquire_shared_memory_use(core, true);
    SystemBatchExecutionPermissionGuard execution_permission(
        system.native_batch_active);
    CPUExecutionGuard execution_guard(core);

    try {
        execute_cycle_interrupt_entry(
            core,
            operation->interrupt_vector,
            callbacks);

        uint64_t completion_cycle = operation->start_cycle;
        if (!operation->completed_accesses.empty()) {
            completion_cycle = std::max(
                completion_cycle,
                operation->completed_accesses.back()
                    .result.completion_cycle);
        }
        const uint64_t elapsed_cycles =
            completion_cycle - operation->start_cycle;
        if (elapsed_cycles >
            static_cast<uint64_t>(
                std::numeric_limits<int64_t>::max())) {
            throw std::overflow_error(
                "cycle interrupt accounting overflow");
        }
        const uint64_t updated_cycle_count = checked_cycle_add(
            core.cycle_count,
            elapsed_cycles,
            "core interrupt cycle counter");
        uint64_t updated_perf_cycles = core.perf_cycles;
        uint64_t updated_perf_stalls = core.perf_stalls;
        if (core.perf_enable) {
            updated_perf_cycles = checked_cycle_add(
                core.perf_cycles,
                elapsed_cycles,
                "core interrupt performance cycle counter");
            updated_perf_stalls = checked_cycle_add(
                core.perf_stalls,
                elapsed_cycles,
                "core interrupt performance stall counter");
        }
        const int64_t updated_batch_cycles =
            checked_scheduler_add(
                batch_result.per_core_cycles[index],
                static_cast<int64_t>(elapsed_cycles),
                "per-core interrupt cycle accounting");
        if (batch_result.interrupts_delivered ==
                std::numeric_limits<uint64_t>::max() ||
            batch_result.per_core_interrupts[index] ==
                std::numeric_limits<uint64_t>::max()) {
            throw std::overflow_error(
                "native scheduler interrupt delivery counter overflow");
        }

        // All potentially failing accounting is complete. Publish the core
        // and scheduler transition as one no-throw commit.
        core.cycle_count = updated_cycle_count;
        if (core.perf_enable) {
            core.perf_cycles = updated_perf_cycles;
            core.perf_stalls = updated_perf_stalls;
        }
        batch_result.per_core_cycles[index] =
            updated_batch_cycles;
        batch_result.interrupts_delivered++;
        batch_result.per_core_interrupts[index]++;
        cycle_state.ready_cycle = completion_cycle;
        cycle_state.instruction.reset();
        system.refresh_cycle_execution_pending();
        return CycleCoreProgress::RETIRED;
    } catch (const BusYieldSignal&) {
        if (!operation->pending_request.has_value()) {
            throw std::logic_error(
                "interrupt bus yield did not publish a request");
        }
        operation->checkpoint.restore(core);
        cycle_state.ready_cycle =
            operation->pending_request->ready_cycle;
        return CycleCoreProgress::WAITING_BUS;
    } catch (...) {
        operation->checkpoint.restore(core);
        throw;
    }
}

static CycleCoreProgress run_cycle_core_once(
        SystemState& system,
        int core_index,
        uint64_t cycle_deadline,
        const StepCallbacks& base_callbacks,
        const py::function& settle_continuation,
        SystemBatchResult& batch_result) {
    const std::size_t index =
        static_cast<std::size_t>(core_index);
    CPUState& core = *system.cores[index];
    FullCoreCycleState& cycle_state =
        system.full_core_cycle_states[index];
    const auto discard_cancelled_tacc_fallback = [&]() {
        if (
            !cycle_state.instruction ||
            !cycle_state.instruction->tacc_python_fallback
        ) {
            throw std::logic_error(
                "TACC cancellation has no suspended fallback");
        }
        system.cancel_tacc_image_stage_for_core(core.core_id);
        cycle_state.ready_cycle =
            system.shared_clock.cycles();
        cycle_state.instruction.reset();
        system.refresh_cycle_execution_pending();
    };

    if (cycle_state.instruction &&
        cycle_state.instruction->kind ==
            CycleOperationKind::INTERRUPT_ENTRY) {
        return run_cycle_interrupt_once(
            system,
            core_index,
            base_callbacks,
            batch_result);
    }

    if (
        cycle_state.instruction &&
        cycle_state.instruction->tacc_python_fallback &&
        core.tacc_epoch !=
            cycle_state.instruction->tacc_operation_epoch
    ) {
        discard_cancelled_tacc_fallback();
        return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
    }
    if (core.halted)
        return CycleCoreProgress::TERMINAL;
    if (core.idle)
        return CycleCoreProgress::TERMINAL;
    if (!cycle_state.instruction &&
        cycle_state.ready_cycle >= cycle_deadline) {
        return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
    }

    if (!cycle_state.instruction) {
        const SystemInstructionTraits traits =
            classify_system_instruction(core);
        const uint64_t remaining =
            cycle_deadline - cycle_state.ready_cycle;
        if (core.profile == CoreProfile::FULL ||
            traits.needs_bus_journal ||
            remaining < 5) {
            cycle_state.instruction =
                std::make_unique<ResumableInstruction>(
                    core,
                    cycle_state.ready_cycle);
            system.cycle_execution_pending.store(
                true,
                std::memory_order_release);
        }
        if (traits.tacc_python_fallback) {
            if (!cycle_state.instruction) {
                throw std::logic_error(
                    "cycle-bounded TACC fallback lacks a checkpoint");
            }
            ResumableInstruction& instruction =
                *cycle_state.instruction;
            instruction.tacc_python_fallback = true;
            instruction.tacc_busy_published =
                traits.tacc_publish_busy;
            instruction.tacc_validation_trap_expected =
                traits.tacc_validation_trap_expected;
            instruction.tacc_operation_epoch =
                core.tacc_epoch;
            const uint64_t predicted_completion =
                checked_cycle_add(
                    instruction.start_cycle,
                    traits.unjournaled_cycle_bound,
                    "cycle-bounded TACC completion");
            instruction.retire_cycle =
                predicted_completion;
            cycle_state.ready_cycle =
                predicted_completion;
            if (traits.tacc_publish_busy)
                core.tacc_busy = true;
        }
        if (
            traits.has_unjournaled_shared_access &&
            remaining < traits.unjournaled_cycle_bound
        ) {
            // Preserve the original issue boundary as a suspended operation.
            // The native MEX body runs only once its complete decoded latency
            // fits, so direct tile-memory writes never need rollback and host
            // call partitioning cannot consume guest cycles silently.
            if (!cycle_state.instruction) {
                throw std::logic_error(
                    "cycle-bounded legacy MEX lacks a checkpoint");
            }
            if (!traits.tacc_python_fallback) {
                const uint64_t predicted_completion =
                    checked_cycle_add(
                        cycle_state.instruction->start_cycle,
                        traits.unjournaled_cycle_bound,
                        "cycle-bounded legacy MEX completion");
                cycle_state.instruction->retire_cycle =
                    predicted_completion;
                cycle_state.ready_cycle =
                    predicted_completion;
            }
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
    }

    ResumableInstruction* instruction =
        cycle_state.instruction.get();
    if (instruction &&
        instruction->retire_cycle.has_value() &&
        *instruction->retire_cycle > cycle_deadline) {
        return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
    }

    if (system.native_dispatches ==
        std::numeric_limits<uint64_t>::max()) {
        throw std::overflow_error(
            "native scheduler dispatch counter overflow");
    }
    system.native_dispatches++;
    batch_result.per_core_dispatches[index]++;

    StepCallbacks callbacks = base_callbacks;
    std::unique_ptr<JournaledBusAccess> bus_access;
    if (instruction) {
        if (!restore_cycle_instruction_checkpoint(
                *instruction,
                core)) {
            discard_cancelled_tacc_fallback();
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
        instruction->replay_cursor = 0;
        bus_access = std::make_unique<JournaledBusAccess>(
            system,
            core_index);
        callbacks.bus_access = bus_access.get();
    }

    auto logical_guard =
        acquire_shared_memory_use(core, true);
    SystemBatchExecutionPermissionGuard execution_permission(
        system.native_batch_active);
    CPUExecutionGuard execution_guard(core);

    RunResult raw{};
    try {
        raw = run_one_system_instruction(core, callbacks);
    } catch (const BusYieldSignal&) {
        if (!instruction ||
            !instruction->pending_request.has_value()) {
            throw std::logic_error(
                "bus yield did not publish an immutable request");
        }
        if (!restore_cycle_instruction_checkpoint(
                *instruction,
                core)) {
            discard_cancelled_tacc_fallback();
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
        cycle_state.ready_cycle =
            instruction->pending_request->ready_cycle;
        return CycleCoreProgress::WAITING_BUS;
    } catch (...) {
        if (
            instruction &&
            !restore_cycle_instruction_checkpoint(
                *instruction,
                core)
        ) {
            discard_cancelled_tacc_fallback();
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
        throw;
    }

    const bool bounded_tacc_fallback =
        instruction &&
        instruction->tacc_python_fallback &&
        raw.stop_reason == RUN_MEX_FALLBACK;
    if (
        (
            raw.stop_reason == RUN_MEX_FALLBACK ||
            raw.stop_reason == RUN_EXT_FALLBACK
        ) &&
        !bounded_tacc_fallback
    ) {
        if (
            instruction &&
            !restore_cycle_instruction_checkpoint(
                *instruction,
                core)
        ) {
            discard_cancelled_tacc_fallback();
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
        throw std::runtime_error(
            "cycle-bounded execution cannot enter an "
            "unbounded Python ISA fallback");
    }

    uint64_t completion_cycle = cycle_state.ready_cycle;
    int64_t retired_cycles = 0;
    int64_t retired_steps = 0;
    bool terminal = false;

    if (bounded_tacc_fallback ||
        raw.stop_reason == RUN_TRAP ||
        raw.stop_reason == RUN_RESET) {
        if (
            bounded_tacc_fallback &&
            core.tacc_epoch !=
                instruction->tacc_operation_epoch
        ) {
            discard_cancelled_tacc_fallback();
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }
        if (!bounded_tacc_fallback) {
            const uint64_t predicted_cycles =
                raw.stop_reason == RUN_TRAP &&
                raw.trap_id == IVEC_SW_TRAP &&
                core.ivt_base != 0
                ? 3
                : 1;
            const uint64_t predicted_completion =
                checked_cycle_add(
                    instruction
                        ? instruction->start_cycle
                        : cycle_state.ready_cycle,
                    predicted_cycles,
                    "cycle-bounded trap completion");
            if (predicted_completion > cycle_deadline) {
                if (!instruction) {
                    throw std::logic_error(
                        "cycle-boundary trap lacked a checkpoint");
                }
                if (!restore_cycle_instruction_checkpoint(
                        *instruction,
                        core)) {
                    discard_cancelled_tacc_fallback();
                    return CycleCoreProgress::
                        BLOCKED_BY_CYCLE_LIMIT;
                }
                instruction->retire_cycle =
                    predicted_completion;
                cycle_state.ready_cycle =
                    predicted_completion;
                return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
            }
        }

        py::object settled_object;
        try {
            settled_object = settle_continuation(
                core_index,
                raw.stop_reason,
                raw.trap_id,
                0,
                0);
        } catch (...) {
            if (bounded_tacc_fallback) {
                system.cancel_tacc_image_stage_for_core(
                    core.core_id);
                finish_cycle_tacc_terminal(core);
                cycle_state.ready_cycle =
                    system.shared_clock.cycles();
                cycle_state.instruction.reset();
                system.refresh_cycle_execution_pending();
            }
            throw;
        }
        py::tuple settled =
            settled_object.cast<py::tuple>();
        const std::size_t expected_tuple_size =
            bounded_tacc_fallback ? 4 : 3;
        if (settled.size() != expected_tuple_size) {
            throw std::runtime_error(
                bounded_tacc_fallback
                    ? "bounded TACC continuation must return "
                      "(invocation_steps, invocation_cycles, "
                      "terminal, cancelled)"
                    : "native continuation must return "
                      "(invocation_steps, invocation_cycles, "
                      "terminal)");
        }
        retired_steps = settled[0].cast<int64_t>();
        retired_cycles = settled[1].cast<int64_t>();
        terminal = settled[2].cast<bool>();
        const bool cancelled =
            bounded_tacc_fallback
            ? settled[3].cast<bool>()
            : false;
        if (retired_steps < 0 ||
            retired_steps > 1 ||
            retired_cycles < 0) {
            throw std::runtime_error(
                "native continuation returned invalid progress");
        }
        if (bounded_tacc_fallback) {
            if (
                cancelled &&
                (
                    retired_steps != 0 ||
                    retired_cycles != 0 ||
                    terminal
                )
            ) {
                throw std::runtime_error(
                    "cancelled bounded TACC continuation "
                    "reported architectural progress");
            }
            if (cancelled) {
                // The control plane won while Python held a detached image.
                // Its live state is authoritative; do not restore or account
                // any part of the discarded instruction.
                discard_cancelled_tacc_fallback();
                return CycleCoreProgress::
                    BLOCKED_BY_CYCLE_LIMIT;
            }
            if (!instruction->retire_cycle.has_value()) {
                throw std::logic_error(
                    "bounded TACC continuation lacks "
                    "a classified completion cycle");
            }
            const uint64_t expected_cycles =
                *instruction->retire_cycle -
                instruction->start_cycle;
            if (
                static_cast<uint64_t>(retired_cycles) !=
                    expected_cycles
            ) {
                throw std::runtime_error(
                    "bounded TACC continuation did not "
                    "match its classified cycle count");
            }
            const bool terminal_trap =
                retired_steps == 0 && terminal;
            const bool normal_retirement =
                retired_steps == 1 && !terminal;
            const bool valid_outcome =
                instruction->tacc_validation_trap_expected
                ? terminal_trap
                : normal_retirement || terminal_trap;
            if (!valid_outcome) {
                throw std::runtime_error(
                    instruction->
                            tacc_validation_trap_expected
                        ? "bounded TACC validation did not "
                          "produce its architected trap"
                        : "admitted bounded TACC operation "
                          "returned an invalid terminal outcome");
            }
        }
        completion_cycle = checked_cycle_add(
            instruction
                ? instruction->start_cycle
                : cycle_state.ready_cycle,
            static_cast<uint64_t>(retired_cycles),
            "cycle-bounded continuation completion");
        if (
            bounded_tacc_fallback &&
            completion_cycle != cycle_state.ready_cycle
        ) {
            throw std::runtime_error(
                "bounded TACC continuation would revise "
                "its classified completion cycle");
        }
        if (completion_cycle > cycle_deadline) {
            throw std::runtime_error(
                "native continuation crossed its cycle bound");
        }
        if (bounded_tacc_fallback)
            finish_cycle_tacc_terminal(core);
        batch_result.continuations++;
        batch_result.per_core_stop_reasons[index][
            static_cast<std::size_t>(raw.stop_reason)]++;
    } else if (raw.stop_reason == RUN_HALT) {
        batch_result.per_core_stop_reasons[index][RUN_HALT]++;
        terminal = true;
    } else {
        if (raw.steps_executed != 1 ||
            raw.total_cycles <= 0) {
            throw std::runtime_error(
                "cycle-bounded core made invalid progress");
        }

        const uint64_t start_cycle =
            instruction
                ? instruction->start_cycle
                : cycle_state.ready_cycle;
        const uint64_t logical_completion =
            checked_cycle_add(
                start_cycle,
                static_cast<uint64_t>(raw.total_cycles),
                "cycle-bounded instruction completion");
        completion_cycle = logical_completion;
        if (instruction &&
            !instruction->completed_accesses.empty()) {
            completion_cycle = std::max(
                completion_cycle,
                instruction->completed_accesses.back()
                    .result.completion_cycle);
        }

        if (completion_cycle > cycle_deadline) {
            if (!instruction) {
                throw std::logic_error(
                    "cycle-boundary instruction lacked a checkpoint");
            }
            instruction->checkpoint.restore(core);
            instruction->retire_cycle =
                completion_cycle;
            cycle_state.ready_cycle =
                completion_cycle;
            return CycleCoreProgress::BLOCKED_BY_CYCLE_LIMIT;
        }

        const uint64_t stall_cycles =
            completion_cycle - logical_completion;
        if (stall_cycles != 0) {
            core.cycle_count = checked_cycle_add(
                core.cycle_count,
                stall_cycles,
                "core cycle counter");
            if (core.perf_enable) {
                core.perf_cycles = checked_cycle_add(
                    core.perf_cycles,
                    stall_cycles,
                    "core performance cycle counter");
                core.perf_stalls = checked_cycle_add(
                    core.perf_stalls,
                    stall_cycles,
                    "core performance stall counter");
            }
        }
        const uint64_t elapsed_cycles =
            completion_cycle - start_cycle;
        if (elapsed_cycles >
            static_cast<uint64_t>(
                std::numeric_limits<int64_t>::max())) {
            throw std::overflow_error(
                "cycle-bounded per-core cycle accounting overflow");
        }
        retired_steps = 1;
        retired_cycles =
            static_cast<int64_t>(elapsed_cycles);
        batch_result.per_core_stop_reasons[index][RUN_LIMIT]++;
    }

    cycle_state.ready_cycle = completion_cycle;
    cycle_state.instruction.reset();
    system.refresh_cycle_execution_pending();
    batch_result.instructions_executed =
        checked_scheduler_add(
            batch_result.instructions_executed,
            retired_steps,
            "aggregate instruction accounting");
    batch_result.per_core_instructions[index] =
        checked_scheduler_add(
            batch_result.per_core_instructions[index],
            retired_steps,
            "per-core instruction accounting");
    batch_result.per_core_cycles[index] =
        checked_scheduler_add(
            batch_result.per_core_cycles[index],
            retired_cycles,
            "per-core cycle accounting");

    if (retired_steps > 0) {
        system.scheduler_cursor =
            (core_index + 1) %
            static_cast<int>(system.cores.size());
    }
    if (terminal)
        return CycleCoreProgress::TERMINAL;
    return CycleCoreProgress::RETIRED;
}

static DmaCycleState& cycle_dma_state_for_requester(
        SystemState& system,
        int requester_id) {
    for (DmaCycleState& state : system.dma_cycle_states) {
        if (state.requester_id == requester_id)
            return state;
    }
    throw std::runtime_error(
        "cycle DMA requester has no endpoint state");
}

static const DmaEndpointCallbacks&
cycle_dma_callbacks_for_requester(
        const std::vector<DmaEndpointCallbacks>& callbacks,
        int requester_id) {
    for (const DmaEndpointCallbacks& endpoint : callbacks) {
        if (endpoint.requester_id == requester_id)
            return endpoint;
    }
    throw std::runtime_error(
        "cycle DMA requester has no callback endpoint");
}

static bool cycle_dma_request_matches_beat(
        const BusRequest& request,
        const DmaBeat& beat) {
    return (
               !beat.ready_cycle.has_value() ||
               request.ready_cycle == *beat.ready_cycle
           ) &&
           request.operation == beat.operation &&
           request.address == beat.address &&
           request.width == BusWidth::BYTE &&
           request.write_data == beat.write_data &&
           !request.ordering.port_io;
}

static void refresh_cycle_dma_requests(
        SystemState& system,
        const std::vector<DmaEndpointCallbacks>& callbacks,
        uint64_t current_cycle) {
    if (callbacks.size() != system.dma_cycle_states.size()) {
        throw std::invalid_argument(
            "one callback endpoint is required for NIC and disk DMA");
    }

    const MainBusSnapshot bus = system.main_bus.snapshot();
    for (DmaCycleState& state : system.dma_cycle_states) {
        const DmaEndpointCallbacks& endpoint =
            cycle_dma_callbacks_for_requester(
                callbacks,
                state.requester_id);
        if (endpoint.requester_id != state.requester_id ||
            !endpoint.inspect ||
            !endpoint.complete) {
            throw std::invalid_argument(
                "cycle DMA callbacks do not match their physical endpoint");
        }

        if (bus.active_grant.has_value() &&
            bus.active_grant->request.requester_id ==
                state.requester_id) {
            if (!state.pending_request.has_value() ||
                !state.pending_token.has_value()) {
                throw std::logic_error(
                    "captured DMA grant has no held endpoint request");
            }
            state.timeline_active = true;
            continue;
        }

        const DmaEndpointView view =
            endpoint.inspect(current_cycle);
        if (view.pending.has_value() && !view.active) {
            throw std::runtime_error(
                "inactive DMA endpoint exposed a pending beat");
        }
        state.timeline_active =
            view.active || view.pending.has_value();
        if (!view.pending.has_value()) {
            state.pending_token.reset();
            state.pending_request.reset();
            continue;
        }

        const DmaBeat& beat = *view.pending;
        if (beat.token == 0) {
            throw std::runtime_error(
                "DMA beat token must be positive");
        }
        if (beat.operation != BusOperation::READ &&
            beat.operation != BusOperation::WRITE) {
            throw std::runtime_error(
                "DMA beat operation is invalid");
        }

        if (state.pending_token.has_value() &&
            *state.pending_token == beat.token) {
            if (!state.pending_request.has_value() ||
                !cycle_dma_request_matches_beat(
                    *state.pending_request,
                    beat)) {
                throw std::runtime_error(
                    "held DMA beat changed before completion");
            }
            continue;
        }
        if (state.pending_token.has_value()) {
            throw std::runtime_error(
                "held DMA endpoint replaced its pending beat");
        }
        if (beat.token <= state.highest_observed_token) {
            throw std::runtime_error(
                "DMA endpoint token did not advance monotonically");
        }
        if (state.next_issue_sequence ==
            std::numeric_limits<uint64_t>::max()) {
            throw std::overflow_error(
                "DMA issue sequence overflow");
        }

        const uint64_t issue_sequence =
            state.next_issue_sequence++;
        state.highest_observed_token = beat.token;
        state.pending_token = beat.token;
        state.pending_request = BusRequest{
            state.requester_id,
            beat.ready_cycle.value_or(current_cycle),
            beat.operation,
            beat.address,
            BusWidth::BYTE,
            beat.write_data,
            BusOrderingMetadata{
                system.main_bus_port_for_requester(
                    state.requester_id),
                issue_sequence,
                false,
            },
        };
        system.validate_main_bus_request(
            *state.pending_request);
    }
    system.refresh_cycle_execution_pending();
}

static bool has_cycle_dma_work(
        const SystemState& system) {
    return std::any_of(
        system.dma_cycle_states.begin(),
        system.dma_cycle_states.end(),
        [](const DmaCycleState& state) {
            return state.timeline_active ||
                   state.pending_request.has_value();
        });
}

static std::vector<BusRequest> collect_cycle_bus_requests(
        const SystemState& system) {
    std::vector<BusRequest> pending;
    pending.reserve(
        system.full_core_cycle_states.size() +
        system.dma_cycle_states.size());
    for (const FullCoreCycleState& state :
         system.full_core_cycle_states) {
        if (state.instruction &&
            state.instruction->pending_request.has_value()) {
            pending.push_back(
                *state.instruction->pending_request);
        }
    }
    for (const DmaCycleState& state :
         system.dma_cycle_states) {
        if (state.pending_request.has_value())
            pending.push_back(*state.pending_request);
    }
    return pending;
}

static void settle_cycle_clock_to(
        SystemState& system,
        uint64_t target_cycle,
        const py::function& settle_round) {
    const uint64_t current = system.shared_clock.cycles();
    if (target_cycle < current)
        throw std::logic_error(
            "cycle scheduler clock moved backwards");
    if (target_cycle == current)
        return;
    uint64_t settled_cycle = current;
    while (settled_cycle < target_cycle) {
        const uint64_t delta = std::min(
            target_cycle - settled_cycle,
            static_cast<uint64_t>(
                std::numeric_limits<int64_t>::max()));
        settle_round(
            static_cast<int64_t>(delta),
            true,
            false,
            false);
        const uint64_t expected =
            settled_cycle + delta;
        if (system.shared_clock.cycles() != expected) {
            throw std::runtime_error(
                "cycle settlement callback did not reach its target");
        }
        settled_cycle = expected;
    }
}

static int full_core_index_for_requester(
        const SystemState& system,
        int requester_id) {
    for (std::size_t index = 0;
         index < system.cores.size();
         index++) {
        if (system.cores[index]->core_id == requester_id)
            return static_cast<int>(index);
    }
    throw std::runtime_error(
        "cycle bus grant does not belong to a full core");
}

static bool held_dma_request_matches_grant(
        const BusRequest& pending,
        const BusRequest& granted) {
    return pending.requester_id == granted.requester_id &&
           pending.ready_cycle == granted.ready_cycle &&
           pending.operation == granted.operation &&
           pending.address == granted.address &&
           pending.width == granted.width &&
           pending.write_data == granted.write_data &&
           pending.ordering.main_port_id ==
               granted.ordering.main_port_id &&
           pending.ordering.issue_sequence ==
               granted.ordering.issue_sequence &&
           pending.ordering.port_io ==
               granted.ordering.port_io;
}

static void complete_cycle_bus_target(
        SystemState& system,
        uint64_t completion_cycle,
        const std::vector<StepCallbacks>& callbacks,
        const std::vector<DmaEndpointCallbacks>& dma_callbacks,
        const py::function& settle_round) {
    const MainBusSnapshot snapshot =
        system.main_bus.snapshot();
    if (!snapshot.active_grant.has_value() ||
        !system.cycle_target_completion_cycle.has_value() ||
        *system.cycle_target_completion_cycle !=
            completion_cycle) {
        throw std::logic_error(
            "cycle target completion has no matching grant");
    }
    const BusGrant grant = *snapshot.active_grant;
    settle_cycle_clock_to(
        system,
        completion_cycle,
        settle_round);

    if (grant.request.requester_id < 0) {
        DmaCycleState& dma_state =
            cycle_dma_state_for_requester(
                system,
                grant.request.requester_id);
        if (!dma_state.pending_request.has_value() ||
            !dma_state.pending_token.has_value() ||
            !held_dma_request_matches_grant(
                *dma_state.pending_request,
                grant.request)) {
            throw std::logic_error(
                "granted DMA endpoint has no matching held request");
        }

        DmaTargetAccess target;
        BusFault fault = BusFault::NONE;
        try {
            auto memory_guard =
                acquire_shared_memory_use(
                    *system.cores.front());
            target = execute_dma_bus_target(
                system,
                grant);
        } catch (...) {
            fault = BusFault::TARGET_FAULT;
            target.read_value.reset();
            // The exact byte target is single-effect, but an exception can
            // occur after the target changed.  Never retry it as untouched.
            target.target_effects_committed = true;
        }

        BusResult result = system.main_bus.complete(
            grant.grant_sequence,
            completion_cycle,
            target.read_value,
            fault,
            target.target_effects_committed);
        const uint64_t completed_token =
            *dma_state.pending_token;
        dma_state.pending_request.reset();
        dma_state.pending_token.reset();
        system.cycle_target_completion_cycle.reset();

        const DmaEndpointCallbacks& endpoint =
            cycle_dma_callbacks_for_requester(
                dma_callbacks,
                grant.request.requester_id);
        endpoint.complete(completed_token, result);
        refresh_cycle_dma_requests(
            system,
            dma_callbacks,
            completion_cycle);
        return;
    }

    const int core_index = full_core_index_for_requester(
        system,
        grant.request.requester_id);
    const std::size_t index =
        static_cast<std::size_t>(core_index);
    FullCoreCycleState& cycle_state =
        system.full_core_cycle_states[index];
    if (!cycle_state.instruction ||
        !cycle_state.instruction->pending_request.has_value()) {
        throw std::logic_error(
            "granted core has no suspended request");
    }
    const BusRequest& pending =
        *cycle_state.instruction->pending_request;
    if (pending.ordering.issue_sequence !=
            grant.request.ordering.issue_sequence ||
        pending.address != grant.request.address ||
        pending.operation != grant.request.operation ||
        pending.width != grant.request.width) {
        throw std::runtime_error(
            "active grant diverged from its suspended request");
    }

    std::optional<uint64_t> read_value;
    BusFault fault = BusFault::NONE;
    bool target_effects_committed = false;
    std::optional<std::string> target_error_message;
    {
        auto memory_guard =
            acquire_shared_memory_use(
                *system.cores[index]);
        try {
            read_value = execute_granted_bus_target(
                *system.cores[index],
                callbacks[index],
                grant);
            target_effects_committed = true;
        } catch (const py::error_already_set& error) {
            target_error_message = error.what();
            fault = BusFault::TARGET_FAULT;
            // A byte-oriented callback may have committed a prefix before
            // throwing.  Never claim the target was untouched or retry it.
            target_effects_committed = true;
            read_value.reset();
        } catch (const std::exception& error) {
            const std::string message = error.what();
            // Only the exact native target bus-fault sentinel is an
            // architectural fault.  A host diagnostic that happens to begin
            // with TRAP: must retain its text instead of being suppressed.
            if (message != "TRAP:BUS_FAULT")
                target_error_message = message;
            fault = BusFault::TARGET_FAULT;
            target_effects_committed = true;
            read_value.reset();
        } catch (...) {
            target_error_message =
                "unknown non-standard target exception";
            fault = BusFault::TARGET_FAULT;
            // A byte-oriented callback may have committed a prefix before
            // throwing.  Never claim the target was untouched or retry it.
            target_effects_committed = true;
            read_value.reset();
        }
    }

    BusResult result = system.main_bus.complete(
        grant.grant_sequence,
        completion_cycle,
        read_value,
        fault,
        target_effects_committed);
    cycle_state.instruction->completed_accesses.push_back(
        BusReplayRecord{
            std::move(result),
            std::move(target_error_message),
        });
    cycle_state.instruction->pending_request.reset();
    cycle_state.ready_cycle = completion_cycle;
    system.cycle_target_completion_cycle.reset();
    refresh_cycle_dma_requests(
        system,
        dma_callbacks,
        completion_cycle);
}

static bool has_cycle_interrupt_operation(
        const FullCoreCycleState& state) {
    return state.instruction &&
        state.instruction->kind ==
            CycleOperationKind::INTERRUPT_ENTRY;
}

static void validate_external_event(
        ExternalEventKind kind,
        const std::vector<uint8_t>& payload,
        uint64_t argument0,
        uint64_t argument1) {
    switch (kind) {
        case ExternalEventKind::UART_RX:
            if (argument0 != 0 || argument1 != 0) {
                throw std::invalid_argument(
                    "UART input events do not accept arguments");
            }
            break;
        case ExternalEventKind::NIC_RX:
            if (payload.empty() ||
                payload.size() >
                    static_cast<std::size_t>(NIC_MAX_FRAME)) {
                throw std::invalid_argument(
                    "NIC input event payload has an invalid frame size");
            }
            if (argument0 != 0 || argument1 != 0) {
                throw std::invalid_argument(
                    "NIC input events do not accept arguments");
            }
            break;
        case ExternalEventKind::NIC_RX_REJECTED:
            if (!payload.empty() ||
                argument0 != 0 ||
                argument1 != 0) {
                throw std::invalid_argument(
                    "rejected NIC input events do not accept data");
            }
            break;
        case ExternalEventKind::UART_GEOMETRY:
            if (!payload.empty()) {
                throw std::invalid_argument(
                    "terminal geometry events do not accept payload data");
            }
            if (argument0 >
                    std::numeric_limits<uint16_t>::max() ||
                argument1 >
                    std::numeric_limits<uint16_t>::max()) {
                throw std::invalid_argument(
                    "terminal geometry exceeds the 16-bit device fields");
            }
            break;
        case ExternalEventKind::UART_GEOMETRY_ACCEPT:
            if (!payload.empty()) {
                throw std::invalid_argument(
                    "terminal resize acceptance does not accept payload data");
            }
            if (argument1 >
                    std::numeric_limits<uint32_t>::max()) {
                throw std::invalid_argument(
                    "terminal resize acceptance dimensions are invalid");
            }
            break;
        case ExternalEventKind::UART_GEOMETRY_DENY:
            if (!payload.empty() || argument1 != 0) {
                throw std::invalid_argument(
                    "terminal resize denial contains invalid data");
            }
            break;
        case ExternalEventKind::
                UART_GEOMETRY_ACCEPT_UNCONDITIONAL:
            if (
                !payload.empty() ||
                argument0 >
                    std::numeric_limits<uint16_t>::max() ||
                argument1 >
                    std::numeric_limits<uint16_t>::max()
            ) {
                throw std::invalid_argument(
                    "unconditional terminal resize acceptance "
                    "contains invalid data");
            }
            break;
        case ExternalEventKind::
                UART_GEOMETRY_DENY_UNCONDITIONAL:
            if (
                !payload.empty() ||
                argument0 != 0 ||
                argument1 != 0
            ) {
                throw std::invalid_argument(
                    "unconditional terminal resize denial "
                    "contains invalid data");
            }
            break;
        default:
            throw std::invalid_argument(
                "unknown external event kind");
    }
}

static void wake_cycle_input_core(
        SystemState& system,
        uint64_t cycle) {
    if (system.cores.empty())
        return;
    CPUState& core = *system.cores.front();
    if (!core.halted && core.idle &&
        (system.shared_uart.has_rx_data() ||
         system.shared_nic.has_rx())) {
        core.idle = false;
        FullCoreCycleState& state =
            system.full_core_cycle_states.front();
        if (!state.instruction &&
            state.ready_cycle < cycle) {
            state.ready_cycle = cycle;
        }
    }
}

static uint64_t apply_due_external_events(
        SystemState& system,
        uint64_t cycle) {
    std::vector<ExternalEventRecord> due =
        system.external_events.take_due(cycle);
    for (const ExternalEventRecord& event : due) {
        switch (event.kind) {
            case ExternalEventKind::UART_RX:
                system.shared_uart.inject(
                    event.payload.data(),
                    event.payload.size());
                break;
            case ExternalEventKind::NIC_RX:
                // Queue saturation is an architecturally visible NIC error,
                // so a rejected frame is still a consumed external event.
                system.shared_nic.inject_frame(
                    event.payload.data(),
                    event.payload.size());
                break;
            case ExternalEventKind::NIC_RX_REJECTED:
                // Invalid host frames are rejected before journaling their
                // contents, but their guest-visible sticky error is replayed.
                system.shared_nic.inject_frame(nullptr, 0);
                break;
            case ExternalEventKind::UART_GEOMETRY:
                system.shared_uart_geom.host_set_size(
                    static_cast<uint16_t>(event.argument0),
                    static_cast<uint16_t>(event.argument1));
                break;
            case ExternalEventKind::UART_GEOMETRY_ACCEPT: {
                const uint16_t cols =
                    static_cast<uint16_t>(event.argument1);
                const uint16_t rows =
                    static_cast<uint16_t>(event.argument1 >> 16);
                if (!system.shared_uart_geom
                         .host_accept_resize_if_pending(
                             event.argument0,
                             cols,
                             rows)) {
                    // The host resize already happened. If firmware replaced
                    // the request in the meantime, publish the actual host
                    // geometry without clearing that newer request.
                    system.shared_uart_geom.host_set_size(
                        cols,
                        rows);
                }
                break;
            }
            case ExternalEventKind::UART_GEOMETRY_DENY:
                system.shared_uart_geom
                    .host_deny_resize_if_pending(
                        event.argument0);
                break;
            case ExternalEventKind::
                    UART_GEOMETRY_ACCEPT_UNCONDITIONAL:
                system.shared_uart_geom.host_accept_resize(
                    static_cast<uint16_t>(event.argument0),
                    static_cast<uint16_t>(event.argument1));
                break;
            case ExternalEventKind::
                    UART_GEOMETRY_DENY_UNCONDITIONAL:
                system.shared_uart_geom.host_deny_resize();
                break;
            default:
                throw std::logic_error(
                    "external event inbox contains an unknown kind");
        }
    }
    wake_cycle_input_core(system, cycle);
    return static_cast<uint64_t>(due.size());
}

static std::optional<uint64_t> next_cycle_timer_irq(
        const SystemState& system) {
    const std::optional<uint64_t> delta =
        system.shared_timer.next_irq_assertion_delta();
    if (!delta.has_value())
        return std::nullopt;
    return checked_cycle_add(
        system.shared_clock.cycles(),
        *delta,
        "timer interrupt frontier");
}

static constexpr int CYCLE_IVEC_TIMER = 0x07;
static constexpr int CYCLE_IVEC_IPI = 0x08;

static bool accept_cycle_interrupts(
        SystemState& system,
        uint64_t cycle,
        SystemBatchResult& result) {
    std::vector<int> selected(
        system.cores.size(),
        -1);

    // Snapshot every line before changing any core.  Hardware priority
    // determines eligibility; equal main-bus round robin determines the
    // ordering of the resulting simultaneous trap-frame transactions.
    for (std::size_t index = 0;
         index < system.cores.size();
         index++) {
        const CPUState& core = *system.cores[index];
        const FullCoreCycleState& state =
            system.full_core_cycle_states[index];
        if (core.halted || !core.flag_i ||
            state.instruction ||
            state.ready_cycle > cycle) {
            continue;
        }
        if (system.shared_interrupts.ipi_line(core.core_id)) {
            selected[index] = CYCLE_IVEC_IPI;
        } else if (system.shared_timer.irq_pending) {
            selected[index] = CYCLE_IVEC_TIMER;
        }
    }

    bool accepted = false;
    int first_unhandled_core = -1;
    int first_unhandled_vector = -1;
    for (std::size_t index = 0;
         index < selected.size();
         index++) {
        if (selected[index] < 0)
            continue;
        if (system.cores[index]->ivt_base == 0) {
            if (first_unhandled_core < 0) {
                first_unhandled_core =
                    static_cast<int>(index);
                first_unhandled_vector = selected[index];
            }
            continue;
        }
        FullCoreCycleState& state =
            system.full_core_cycle_states[index];
        state.ready_cycle = cycle;
        state.instruction =
            std::make_unique<ResumableInstruction>(
                *system.cores[index],
                cycle,
                CycleOperationKind::INTERRUPT_ENTRY,
                selected[index]);
        accepted = true;
    }
    if (accepted)
        system.refresh_cycle_execution_pending();
    const bool accepted_peer_in_flight = std::any_of(
        system.full_core_cycle_states.begin(),
        system.full_core_cycle_states.end(),
        [](const FullCoreCycleState& state) {
            return has_cycle_interrupt_operation(state);
        });
    if (first_unhandled_core >= 0 &&
        !accepted_peer_in_flight) {
        // A core without an IVT cannot enter the bus-eligible trap-frame
        // set, but it must not suppress already accepted peers. Drain any
        // simultaneous valid entry before reporting the first invalid core.
        result.system_stop_reason =
            SystemStopReason::UNHANDLED_INTERRUPT;
        result.pending_interrupt_core =
            first_unhandled_core;
        result.pending_interrupt_vector =
            first_unhandled_vector;
        return false;
    }
    return true;
}

static bool cycle_system_all_halted(
        const SystemState& system) {
    return std::all_of(
        system.cores.begin(),
        system.cores.end(),
        [](const std::unique_ptr<CPUState>& core) {
            return core->halted;
        });
}

static bool cycle_system_all_idle_or_halted(
        const SystemState& system) {
    return std::all_of(
        system.cores.begin(),
        system.cores.end(),
        [](const std::unique_ptr<CPUState>& core) {
            return core->halted || core->idle;
        });
}

static uint64_t run_strict_cycle_private_prefix(
        SystemState& system,
        int round_start,
        uint64_t scheduler_cycle,
        uint64_t effective_deadline,
        int64_t max_instructions,
        SystemBatchResult& result) {
    if (
        scheduler_cycle >= effective_deadline ||
        result.instructions_executed >= max_instructions
    ) {
        return 0;
    }

    const std::size_t core_count = system.cores.size();
    const uint64_t suspended_guest_count =
        pending_guest_instruction_count(system);
    if (
        suspended_guest_count >
        static_cast<uint64_t>(
            max_instructions -
            result.instructions_executed)
    ) {
        throw std::logic_error(
            "strict-cycle suspended instruction reservation "
            "exceeds the instruction budget");
    }
    uint64_t available =
        static_cast<uint64_t>(
            max_instructions -
            result.instructions_executed) -
        suspended_guest_count;
    if (available == 0)
        return 0;

    std::vector<int> candidates;
    candidates.reserve(
        std::min<std::size_t>(
            core_count,
            static_cast<std::size_t>(available)));
    for (std::size_t offset = 0;
         offset < core_count;
         offset++) {
        const int core_index = (
            round_start +
            static_cast<int>(offset)
        ) % static_cast<int>(core_count);
        const std::size_t index =
            static_cast<std::size_t>(core_index);
        const CPUState& core = *system.cores[index];
        const FullCoreCycleState& state =
            system.full_core_cycle_states[index];

        if (
            core.halted ||
            (
                core.idle &&
                !has_cycle_interrupt_operation(state)
            ) ||
            state.ready_cycle > scheduler_cycle ||
            (
                state.instruction &&
                state.instruction->pending_request.has_value()
            )
        ) {
            continue;
        }

        // Preserve the serial failure/commit boundary: parallelize only the
        // cyclic prefix before the first actionable instruction that needs
        // the resumable coordinator.
        if (
            state.instruction ||
            !classify_strict_cycle_private_one_cycle(core)
        ) {
            break;
        }

        candidates.push_back(core_index);
        available--;
        if (available == 0)
            break;
    }
    if (candidates.empty())
        return 0;

    const uint64_t completion_cycle =
        checked_cycle_add(
            scheduler_cycle,
            1,
            "strict private completion");
    if (completion_cycle > effective_deadline) {
        throw std::logic_error(
            "strict private wave crossed its cycle deadline");
    }
    if (
        static_cast<uint64_t>(candidates.size()) >
        std::numeric_limits<uint64_t>::max() -
            system.native_dispatches
    ) {
        throw std::overflow_error(
            "native scheduler dispatch counter overflow");
    }
    if (
        static_cast<int64_t>(candidates.size()) >
        std::numeric_limits<int64_t>::max() -
            result.instructions_executed
    ) {
        throw std::overflow_error(
            "aggregate instruction accounting overflow");
    }
    for (int core_index : candidates) {
        const std::size_t index =
            static_cast<std::size_t>(core_index);
        if (
            result.per_core_dispatches[index] ==
                std::numeric_limits<uint64_t>::max() ||
            result.per_core_stop_reasons[index][RUN_LIMIT] ==
                std::numeric_limits<uint64_t>::max() ||
            result.per_core_instructions[index] ==
                std::numeric_limits<int64_t>::max() ||
            result.per_core_cycles[index] ==
                std::numeric_limits<int64_t>::max()
        ) {
            throw std::overflow_error(
                "strict private accounting overflow");
        }
    }

    const std::size_t lane_count =
        static_cast<std::size_t>(
            system.configured_worker_count);
    const uint64_t cohort_count =
        (
            static_cast<uint64_t>(candidates.size()) +
            static_cast<uint64_t>(lane_count) -
            1
        ) / static_cast<uint64_t>(lane_count);
    system.worker_pool->validate_private_capacity(
        cohort_count,
        static_cast<uint64_t>(candidates.size()),
        static_cast<uint64_t>(candidates.size()));

    std::shared_ptr<SharedMemoryExecutionAdmission>
        frontier_admission;
    {
        py::gil_scoped_release release;
        frontier_admission =
            std::make_shared<
                SharedMemoryExecutionAdmission>(
                    system.shared_memory,
                    "CPUState memory is already in use");
    }
    system.mappings_sealed = true;

    // These commands are private and callback-free. A complete candidate-set
    // checkpoint therefore makes an unexpected helper failure atomic without
    // rolling back any bus or host effect. Publish only after every physical
    // cohort has validated.
    std::vector<CPUExecutionCheckpoint> candidate_checkpoints;
    candidate_checkpoints.reserve(candidates.size());
    for (int core_index : candidates) {
        candidate_checkpoints.emplace_back(
            *system.cores[
                static_cast<std::size_t>(core_index)]);
    }

    try {
        std::size_t candidate_offset = 0;
        while (candidate_offset < candidates.size()) {
            const std::size_t cohort_size =
                std::min(
                    lane_count,
                    candidates.size() - candidate_offset);
            std::vector<PrivateCoreCommand> commands;
            commands.reserve(cohort_size);
            for (std::size_t lane = 0;
                 lane < cohort_size;
                 lane++) {
                commands.push_back(
                    PrivateCoreCommand{
                        0,
                        0,
                        0,
                        static_cast<int>(lane),
                        candidates[candidate_offset + lane],
                        1,
                        -1,
                        true,
                        false,
                        nullptr,
                        nullptr,
                    });
            }
            const std::vector<PrivateCoreResult> private_results =
                execute_strict_cycle_private_wave_under_active_batch(
                    system,
                    std::move(commands),
                    frontier_admission);
            if (private_results.size() != cohort_size) {
                throw std::logic_error(
                    "strict private wave returned an incomplete cohort");
            }
            for (std::size_t lane = 0;
                 lane < cohort_size;
                 lane++) {
                const PrivateCoreResult& private_result =
                    private_results[lane];
                const int expected_core =
                    candidates[candidate_offset + lane];
                if (
                    private_result.core_index != expected_core ||
                    private_result.steps_executed != 1 ||
                    private_result.total_cycles != 1 ||
                    private_result.stop_reason ==
                        PrivateCoreStopReason::INTERNAL_FAILURE
                ) {
                    const std::string detail =
                        private_result.internal_error.empty()
                        ? "result identity or progress diverged"
                        : private_result.internal_error;
                    throw std::runtime_error(
                        "strict private command failed on lane " +
                        std::to_string(
                            private_result.lane_index) +
                        ", core " +
                        std::to_string(
                            private_result.core_index) +
                        ": " + detail);
                }
            }
            candidate_offset += cohort_size;
        }
    } catch (...) {
        for (std::size_t index = 0;
             index < candidates.size();
             index++) {
            candidate_checkpoints[index].restore(
                *system.cores[
                    static_cast<std::size_t>(
                        candidates[index])]);
        }
        throw;
    }

    // Every potentially failing capacity check and helper command completed.
    // Publish architectural scheduler accounting in the frozen cyclic order.
    for (int core_index : candidates) {
        const std::size_t index =
            static_cast<std::size_t>(core_index);
        FullCoreCycleState& state =
            system.full_core_cycle_states[index];
        state.ready_cycle = completion_cycle;
        system.native_dispatches++;
        result.instructions_executed++;
        result.per_core_instructions[index]++;
        result.per_core_cycles[index]++;
        result.per_core_dispatches[index]++;
        result.per_core_stop_reasons[index][RUN_LIMIT]++;
        system.scheduler_cursor =
            (core_index + 1) %
            static_cast<int>(core_count);
    }
    return static_cast<uint64_t>(
        candidates.size());
}

static SystemBatchResult run_full_core_cycle_batch(
        SystemState& system,
        uint64_t max_system_cycles,
        int64_t max_instructions,
        const std::vector<StepCallbacks>& callbacks,
        const std::vector<DmaEndpointCallbacks>& dma_callbacks,
        const py::function& prepare_batch,
        const py::function& settle_continuation,
        const py::function& settle_round) {
    const std::size_t core_count = system.cores.size();
    SystemBatchResult result;
    result.per_core_instructions.assign(core_count, 0);
    result.per_core_cycles.assign(core_count, 0);
    result.per_core_dispatches.assign(core_count, 0);
    result.per_core_interrupts.assign(core_count, 0);
    result.per_core_stop_reasons.assign(core_count, {});
    result.scheduler_cursor = system.scheduler_cursor;

    if (max_instructions < 0)
        throw std::invalid_argument(
            "max_instructions cannot be negative");
    if (callbacks.size() != core_count)
        throw std::invalid_argument(
            "one callback set is required for every full core");
    if (dma_callbacks.size() !=
        system.dma_cycle_states.size()) {
        throw std::invalid_argument(
            "one callback endpoint is required for NIC and disk DMA");
    }
    if (system.shared_rtc.snapshot().realtime) {
        throw std::runtime_error(
            "cycle-bounded execution does not support a realtime RTC");
    }
    if (system.main_bus.active_timeout_cycle().has_value() &&
        !system.cycle_target_completion_cycle.has_value()) {
        throw std::runtime_error(
            "cycle-bounded execution cannot adopt an external "
            "active main-bus grant");
    }

    const uint64_t clock_start =
        system.shared_clock.cycles();
    const uint64_t caller_deadline = checked_cycle_add(
        clock_start,
        max_system_cycles,
        "cycle batch deadline");
    const auto horizon =
        system.shared_clock.snapshot();
    uint64_t cycle_deadline = caller_deadline;
    bool stops_at_event_horizon = false;
    if (horizon.has_deadline &&
        horizon.earliest_deadline <= cycle_deadline) {
        cycle_deadline = horizon.earliest_deadline;
        stops_at_event_horizon = true;
    }

    if (stops_at_event_horizon &&
        cycle_deadline == clock_start) {
        result.system_stop_reason =
            SystemStopReason::EVENT_HORIZON;
        result.stop_cycle = clock_start;
        result.event_source_mask = horizon.source_mask;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }
    if (max_system_cycles == 0) {
        result.system_stop_reason =
            SystemStopReason::CYCLE_LIMIT;
        result.stop_cycle = clock_start;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }
    if (max_instructions == 0) {
        result.system_stop_reason =
            SystemStopReason::INSTRUCTION_LIMIT;
        result.stop_cycle = clock_start;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }

    // Cycle mode owns all wake and interrupt boundaries natively. Calling a
    // Python preparation hook here would let a zero-progress request mutate
    // the guest before its budget checks.
    (void)prepare_batch;
    NativeBatchActiveGuard active_guard(system);
    checked_scheduler_increment(
        system.native_batch_runs,
        "batch counter");

    for (FullCoreCycleState& state :
         system.full_core_cycle_states) {
        if (!state.instruction &&
            state.ready_cycle < clock_start) {
            state.ready_cycle = clock_start;
        }
    }

    result.external_events_applied =
        apply_due_external_events(system, clock_start);
    wake_cycle_input_core(system, clock_start);
    if (!accept_cycle_interrupts(
            system,
            clock_start,
            result)) {
        result.stop_cycle = clock_start;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }
    refresh_cycle_dma_requests(
        system,
        dma_callbacks,
        clock_start);

    const std::optional<uint64_t> initial_timer_cycle =
        next_cycle_timer_irq(system);
    const std::optional<uint64_t> initial_external_cycle =
        system.external_events.next_cycle();
    const bool has_future_virtual_work =
        stops_at_event_horizon ||
        initial_timer_cycle.has_value() ||
        initial_external_cycle.has_value();

    if (pending_instruction_count(system) == 0 &&
        !system.cycle_target_completion_cycle.has_value() &&
        !has_cycle_dma_work(system) &&
        cycle_system_all_halted(system) &&
        !has_future_virtual_work) {
        result.system_stop_reason =
            SystemStopReason::ALL_HALTED;
        result.stop_cycle = clock_start;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }
    if (pending_instruction_count(system) == 0 &&
        !system.cycle_target_completion_cycle.has_value() &&
        !has_cycle_dma_work(system) &&
        cycle_system_all_idle_or_halted(system) &&
        !has_future_virtual_work) {
        result.system_stop_reason =
            SystemStopReason::ALL_IDLE;
        result.stop_cycle = clock_start;
        result.scheduler_cursor = system.scheduler_cursor;
        return result;
    }

    uint64_t scheduler_cycle = clock_start;
    uint64_t last_retirement_cycle = clock_start;
    bool reached_cycle_limit = false;
    std::optional<uint64_t> instruction_stop_cycle;

    while (true) {
        const bool instruction_limit_reached =
            result.instructions_executed >= max_instructions;
        const uint64_t effective_deadline =
            instruction_stop_cycle.has_value()
            ? std::min(cycle_deadline, *instruction_stop_cycle)
            : cycle_deadline;

        const std::optional<uint64_t> next_timer_cycle =
            next_cycle_timer_irq(system);
        const std::optional<uint64_t> next_external_cycle =
            system.external_events.next_cycle();
        if (next_timer_cycle.has_value() &&
            *next_timer_cycle < scheduler_cycle) {
            throw std::logic_error(
                "cycle timer frontier fell behind the scheduler");
        }
        if (next_external_cycle.has_value() &&
            *next_external_cycle < scheduler_cycle) {
            throw std::logic_error(
                "external event frontier fell behind the scheduler");
        }
        if (system.cycle_target_completion_cycle.has_value() &&
            *system.cycle_target_completion_cycle <
                scheduler_cycle) {
            throw std::logic_error(
                "main-bus target completion fell behind the scheduler");
        }
        std::optional<uint64_t> next_core_cycle;
        const uint64_t suspended_guest_count =
            pending_guest_instruction_count(system);
        for (std::size_t index = 0;
             index < core_count;
             index++) {
            const CPUState& core = *system.cores[index];
            const FullCoreCycleState& state =
                system.full_core_cycle_states[index];
            if (core.halted ||
                (core.idle &&
                 !has_cycle_interrupt_operation(state))) {
                continue;
            }
            if (state.instruction &&
                state.instruction->pending_request.has_value()) {
                continue;
            }
            if (!state.instruction &&
                (instruction_limit_reached ||
                 static_cast<uint64_t>(
                     result.instructions_executed) +
                     suspended_guest_count >=
                     static_cast<uint64_t>(max_instructions))) {
                continue;
            }
            if (!next_core_cycle.has_value() ||
                state.ready_cycle < *next_core_cycle) {
                next_core_cycle = state.ready_cycle;
            }
        }

        const std::vector<BusRequest> pending =
            collect_cycle_bus_requests(system);
        std::optional<uint64_t> next_arbitration_cycle;
        if (!system.cycle_target_completion_cycle.has_value()) {
            next_arbitration_cycle =
                system.main_bus.next_arbitration_cycle(
                    pending,
                    scheduler_cycle);
        }

        uint64_t next_cycle = effective_deadline;
        if (next_core_cycle.has_value())
            next_cycle = std::min(next_cycle, *next_core_cycle);
        if (next_arbitration_cycle.has_value()) {
            next_cycle = std::min(
                next_cycle,
                *next_arbitration_cycle);
        }
        if (system.cycle_target_completion_cycle.has_value()) {
            next_cycle = std::min(
                next_cycle,
                *system.cycle_target_completion_cycle);
        }
        if (next_timer_cycle.has_value()) {
            next_cycle = std::min(
                next_cycle,
                *next_timer_cycle);
        }
        if (next_external_cycle.has_value()) {
            next_cycle = std::min(
                next_cycle,
                *next_external_cycle);
        }
        if (next_cycle < scheduler_cycle)
            next_cycle = scheduler_cycle;
        scheduler_cycle = next_cycle;
        result.rounds++;

        const bool timer_frontier =
            next_timer_cycle.has_value() &&
            *next_timer_cycle == scheduler_cycle;
        const bool external_frontier =
            next_external_cycle.has_value() &&
            *next_external_cycle == scheduler_cycle;
        const bool target_frontier =
            system.cycle_target_completion_cycle.has_value() &&
            *system.cycle_target_completion_cycle ==
                scheduler_cycle;

        // Boundary order is fixed: settle virtual devices, commit the bus
        // target that sampled pre-edge state, apply timestamped host input,
        // snapshot interrupt lines, then dispatch cores.
        if (timer_frontier ||
            external_frontier ||
            target_frontier) {
            settle_cycle_clock_to(
                system,
                scheduler_cycle,
                settle_round);
        }
        if (target_frontier) {
            complete_cycle_bus_target(
                system,
                scheduler_cycle,
                callbacks,
                dma_callbacks,
                settle_round);
        }
        if (external_frontier) {
            const uint64_t applied =
                apply_due_external_events(
                    system,
                    scheduler_cycle);
            if (applied >
                std::numeric_limits<uint64_t>::max() -
                    result.external_events_applied) {
                throw std::overflow_error(
                    "external event accounting overflow");
            }
            result.external_events_applied += applied;
            refresh_cycle_dma_requests(
                system,
                dma_callbacks,
                scheduler_cycle);
        }
        if (scheduler_cycle < effective_deadline &&
            !accept_cycle_interrupts(
                system,
                scheduler_cycle,
                result)) {
            break;
        }

        // A core awakened by an in-call event or device frontier cannot issue
        // from its pre-idle ready timestamp. Unsuspended work starts at the
        // frontier that made it runnable, never retroactively in the past.
        for (std::size_t index = 0;
             index < core_count;
             index++) {
            FullCoreCycleState& state =
                system.full_core_cycle_states[index];
            if (
                !state.instruction &&
                state.ready_cycle < scheduler_cycle
            ) {
                state.ready_cycle = scheduler_cycle;
            }
        }

        const int round_start =
            system.scheduler_cursor %
            static_cast<int>(core_count);
        const uint64_t strict_private_retired =
            run_strict_cycle_private_prefix(
                system,
                round_start,
                scheduler_cycle,
                effective_deadline,
                max_instructions,
                result);
        if (strict_private_retired != 0) {
            last_retirement_cycle = std::max(
                last_retirement_cycle,
                checked_cycle_add(
                    scheduler_cycle,
                    1,
                    "strict private retirement"));
            if (
                result.instructions_executed >=
                    max_instructions &&
                !instruction_stop_cycle.has_value()
            ) {
                instruction_stop_cycle =
                    last_retirement_cycle;
            }
        }
        for (std::size_t offset = 0;
             offset < core_count;
             offset++) {
            const int core_index = (
                round_start + static_cast<int>(offset)
            ) % static_cast<int>(core_count);
            FullCoreCycleState& state =
                system.full_core_cycle_states[
                    static_cast<std::size_t>(core_index)];
            CPUState& core =
                *system.cores[
                    static_cast<std::size_t>(core_index)];
            if (core.halted ||
                (core.idle &&
                 !has_cycle_interrupt_operation(state))) {
                continue;
            }
            if (state.instruction &&
                state.instruction->pending_request.has_value()) {
                continue;
            }
            if (state.ready_cycle > scheduler_cycle)
                continue;
            if (!state.instruction &&
                scheduler_cycle >= effective_deadline) {
                continue;
            }
            if (!state.instruction &&
                (result.instructions_executed >=
                     max_instructions ||
                 static_cast<uint64_t>(
                     result.instructions_executed) +
                     pending_guest_instruction_count(system) >=
                     static_cast<uint64_t>(max_instructions))) {
                continue;
            }

            // One cycle is the ordinary speculative commit window. Native
            // legacy MEX cannot be replayed after its direct destination
            // write, so it may use the complete already-clipped event window.
            // run_cycle_core_once() checks the exact decoded native latency
            // before any mutation.
            uint64_t dispatch_deadline =
                effective_deadline;
            const bool direct_legacy_mex =
                !state.instruction &&
                classify_system_instruction(core)
                    .has_unjournaled_shared_access;
            if (
                scheduler_cycle < effective_deadline &&
                !direct_legacy_mex
            ) {
                dispatch_deadline = std::min(
                    effective_deadline,
                    checked_cycle_add(
                        scheduler_cycle,
                        1,
                        "cycle dispatch commit window"));
            }
            const int64_t instructions_before =
                result.instructions_executed;
            const CycleCoreProgress progress =
                run_cycle_core_once(
                    system,
                    core_index,
                    dispatch_deadline,
                    callbacks[
                        static_cast<std::size_t>(core_index)],
                    settle_continuation,
                    result);
            if (progress == CycleCoreProgress::RETIRED ||
                result.instructions_executed >
                    instructions_before) {
                last_retirement_cycle = std::max(
                    last_retirement_cycle,
                    state.ready_cycle);
            }
            if (result.instructions_executed >=
                max_instructions) {
                if (!instruction_stop_cycle.has_value()) {
                    instruction_stop_cycle = std::max(
                        scheduler_cycle,
                        last_retirement_cycle);
                }
                break;
            }
        }

        const uint64_t post_dispatch_deadline =
            instruction_stop_cycle.has_value()
            ? std::min(cycle_deadline, *instruction_stop_cycle)
            : cycle_deadline;
        if (scheduler_cycle < post_dispatch_deadline &&
            !system.cycle_target_completion_cycle.has_value()) {
            const std::vector<BusRequest> ready_pending =
                collect_cycle_bus_requests(system);
            const std::optional<BusGrant> grant =
                system.main_bus.try_grant(
                    ready_pending,
                    scheduler_cycle);
            if (grant.has_value()) {
                system.cycle_target_completion_cycle =
                    checked_cycle_add(
                        grant->grant_cycle,
                        1,
                        "main bus target completion");
            }
        }

        if (scheduler_cycle >= post_dispatch_deadline) {
            reached_cycle_limit =
                cycle_deadline <= post_dispatch_deadline;
            break;
        }

        const bool has_suspended_instruction =
            pending_instruction_count(system) != 0;
        const bool has_future_virtual_work =
            stops_at_event_horizon ||
            next_cycle_timer_irq(system).has_value() ||
            system.external_events.next_cycle().has_value();
        if (!has_suspended_instruction &&
            !system.cycle_target_completion_cycle.has_value() &&
            !has_cycle_dma_work(system) &&
            !instruction_stop_cycle.has_value() &&
            cycle_system_all_halted(system) &&
            !has_future_virtual_work) {
            result.system_stop_reason =
                SystemStopReason::ALL_HALTED;
            break;
        }
        if (!has_suspended_instruction &&
            !system.cycle_target_completion_cycle.has_value() &&
            !has_cycle_dma_work(system) &&
            !instruction_stop_cycle.has_value() &&
            cycle_system_all_idle_or_halted(system) &&
            !has_future_virtual_work) {
            result.system_stop_reason =
                SystemStopReason::ALL_IDLE;
            break;
        }
    }

    uint64_t stop_cycle = scheduler_cycle;
    if (reached_cycle_limit) {
        stop_cycle = cycle_deadline;
        result.system_stop_reason =
            stops_at_event_horizon
            ? SystemStopReason::EVENT_HORIZON
            : SystemStopReason::CYCLE_LIMIT;
        if (stops_at_event_horizon)
            result.event_source_mask = horizon.source_mask;
    } else if (instruction_stop_cycle.has_value()) {
        if (scheduler_cycle != *instruction_stop_cycle) {
            throw std::logic_error(
                "instruction limit stopped before its cycle frontier");
        }
        result.system_stop_reason =
            SystemStopReason::INSTRUCTION_LIMIT;
    } else if (
            result.system_stop_reason ==
                SystemStopReason::ALL_HALTED ||
            result.system_stop_reason ==
                SystemStopReason::ALL_IDLE) {
        stop_cycle = std::max(
            scheduler_cycle,
            last_retirement_cycle);
    }

    settle_cycle_clock_to(
        system,
        stop_cycle,
        settle_round);
    settle_round(0, false, true, false);
    refresh_cycle_dma_requests(
        system,
        dma_callbacks,
        system.shared_clock.cycles());
    result.system_cycles_advanced =
        system.shared_clock.cycles() - clock_start;
    result.stop_cycle = system.shared_clock.cycles();
    result.scheduler_cursor = system.scheduler_cursor;
    system.refresh_cycle_execution_pending();
    return result;
}

static std::unique_lock<std::recursive_mutex>
acquire_system_scheduler_lock(SystemState& system) {
    py::gil_scoped_release release;
    return std::unique_lock<std::recursive_mutex>(
        system.scheduler_mutex);
}

static void record_private_wave_profile(
        SystemState& system,
        const std::vector<PrivateCoreResult>& results,
        const std::vector<
            PrivateCoreHostTelemetry>& telemetry,
        const WorkerWaveHostTiming& wave_timing,
        uint64_t wave_ns) noexcept {
    ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    if (!profile.enabled)
        return;
    if (results.size() != telemetry.size())
        return;

    host_saturating_increment(profile.worker_waves);
    host_saturating_add(
        profile.worker_wave_ns, wave_ns);
    host_saturating_add(
        profile.worker_wave_prepare_ns,
        wave_timing.prepare_ns);
    host_saturating_add(
        profile.worker_wave_wait_ns,
        wave_timing.wait_ns);
    host_saturating_add(
        profile.worker_wave_gather_ns,
        wave_timing.gather_ns);

    for (
        std::size_t index = 0;
        index < results.size();
        index++
    ) {
        const PrivateCoreResult& result =
            results[index];
        const PrivateCoreHostTelemetry&
            host = telemetry[index];
        host_saturating_increment(
            profile.worker_commands);
        if (result.steps_executed == 0) {
            host_saturating_increment(
                profile.zero_step_commands);
        } else if (result.steps_executed > 0) {
            host_saturating_add(
                profile.private_steps,
                static_cast<uint64_t>(
                    result.steps_executed));
        }
        host_saturating_add(
            profile.private_classification_calls,
            host.classification_calls);
        host_saturating_add(
            profile.private_decode_cache_lookups,
            host.decode_cache_lookups);
        host_saturating_add(
            profile.private_decode_cache_hits,
            host.decode_cache_hits);
        host_saturating_add(
            profile.private_decode_cache_misses,
            host.decode_cache_misses);
        host_saturating_add(
            profile.micro_oracle_proof_reuses,
            host.micro_oracle_proof_reuses);
        host_saturating_add(
            profile.checkpoint_captures,
            host.checkpoint_captures);
        host_saturating_add(
            profile.checkpoint_restores,
            host.checkpoint_restores);
        host_saturating_add(
            profile.private_command_sum_ns,
            host.execution_ns);
        profile.private_command_max_ns =
            std::max(
                profile.private_command_max_ns,
                host.execution_ns);
        host_saturating_add(
            profile.private_scope_setup_ns,
            host.scope_setup_ns);
        host_saturating_add(
            profile.checkpoint_capture_ns,
            host.checkpoint_capture_ns);
        host_saturating_add(
            profile.checkpoint_restore_ns,
            host.checkpoint_restore_ns);

        const std::size_t reason =
            static_cast<std::size_t>(
                result.stop_reason);
        if (
            reason <
            profile.private_stop_reasons.size()
        ) {
            host_saturating_increment(
                profile.private_stop_reasons[
                    reason]);
        }

        if (
            result.lane_index >= 0 &&
            static_cast<std::size_t>(
                result.lane_index) <
                HOST_PROFILE_MAX_LANES
        ) {
            const std::size_t lane =
                static_cast<std::size_t>(
                    result.lane_index);
            host_saturating_increment(
                profile.lane_commands[lane]);
            if (result.steps_executed > 0) {
                host_saturating_add(
                    profile.lane_steps[lane],
                    static_cast<uint64_t>(
                        result.steps_executed));
            }
            host_saturating_add(
                profile.lane_active_ns[lane],
                host.execution_ns);
        }
    }
}

static std::vector<PrivateCoreResult>
execute_private_core_wave_under_active_batch(
        SystemState& system,
        std::vector<PrivateCoreCommand> commands,
        std::shared_ptr<
            SharedMemoryExecutionAdmission>
            frontier_admission) {
    if (commands.empty())
        return {};
    if (!system.native_batch_active.load(
            std::memory_order_acquire)) {
        throw std::logic_error(
            "private execution requires an active native system batch");
    }
    if (!system.worker_pool)
        throw std::logic_error(
            "native worker pool is unavailable");
    if (system.main_bus.active_timeout_cycle().has_value()) {
        throw std::runtime_error(
            "private execution cannot enter an active "
            "main-bus grant");
    }
    if (system.has_cycle_execution_pending()) {
        throw std::runtime_error(
            "private execution cannot enter a suspended "
            "cycle operation");
    }
    if (system.external_events.next_cycle().has_value()) {
        throw std::runtime_error(
            "private execution cannot cross a pending "
            "external event");
    }
    if (system.shared_clock.snapshot().has_deadline) {
        throw std::runtime_error(
            "private execution cannot cross an active "
            "event horizon");
    }

    const bool host_profile_enabled =
        system.concurrency_profile_batch_active;
    const bool frontier_fast_path_enabled =
        frontier_admission != nullptr;

    CPUState* inline_core = nullptr;
    std::array<bool, PRIVATE_WORKER_MAX_LANES>
        seen_lanes{};
    std::vector<int> seen_cores;
    seen_cores.reserve(commands.size());
    if (
        frontier_fast_path_enabled &&
        commands.size() >
            static_cast<std::size_t>(
                system.configured_worker_count)
    ) {
        throw std::invalid_argument(
            "a private wave cannot contain more commands than lanes");
    }
    for (
        std::size_t command_index = 0;
        command_index < commands.size();
        command_index++
    ) {
        PrivateCoreCommand& command =
            commands[command_index];
        if (
            command.core_index < 0 ||
            command.core_index >=
                static_cast<int>(
                    system.execution_cores.size())
        ) {
            throw std::out_of_range(
                "private command execution-core index is out of range");
        }
        if (frontier_fast_path_enabled) {
            if (
                command.lane_index < 0 ||
                command.lane_index >=
                    system.configured_worker_count
            ) {
                throw std::out_of_range(
                    "private command lane index is out of range");
            }
            const std::size_t lane =
                static_cast<std::size_t>(
                    command.lane_index);
            if (seen_lanes[lane]) {
                throw std::invalid_argument(
                    "a private wave cannot submit two commands "
                    "to one lane");
            }
            if (command.max_steps < 0) {
                throw std::invalid_argument(
                    "private command step budget cannot be negative");
            }
            if (
                std::find(
                    seen_cores.begin(),
                    seen_cores.end(),
                    command.core_index) !=
                seen_cores.end()
            ) {
                throw std::invalid_argument(
                    "a private wave cannot execute one core twice");
            }
            seen_lanes[lane] = true;
            seen_cores.push_back(
                command.core_index);
        }
        command.core = system.execution_cores[
            static_cast<std::size_t>(
                command.core_index)];
        command.submission_index =
            command_index;
        command.pending_interrupt_vector =
            pending_enabled_core_interrupt(
                system, *command.core);
        if (command.lane_index == 0)
            inline_core = command.core;
    }

    std::unique_ptr<SharedMemoryUseGuard>
        wave_memory;
    std::shared_ptr<SharedMemoryExecutionAdmission>
        admission = std::move(
            frontier_admission);
    if (admission) {
        if (
            &admission->memory() !=
                &system.shared_memory
        ) {
            throw std::invalid_argument(
                "private frontier admission does not "
                "match system memory");
        }
    } else {
        if (inline_core != nullptr) {
            wave_memory =
                acquire_shared_memory_use(
                    *inline_core,
                    /*permit_native_execution=*/true);
        } else {
            py::gil_scoped_release release;
            wave_memory =
                std::make_unique<
                    SharedMemoryUseGuard>(
                        system.shared_memory);
        }
        admission =
            wave_memory->execution_admission();
    }
    for (PrivateCoreCommand& command : commands)
        command.admission = admission;

    system.mappings_sealed = true;

    std::vector<PrivateCoreResult> results(
        commands.size());
    std::vector<std::size_t>
        worker_result_positions;
    worker_result_positions.reserve(
        commands.size());
    std::vector<PrivateCoreCommand>
        worker_commands;
    worker_commands.reserve(commands.size());

    if (frontier_fast_path_enabled) {
        uint64_t classification_calls = 0;
        uint64_t decode_cache_lookups = 0;
        uint64_t decode_cache_hits = 0;
        uint64_t decode_cache_misses = 0;
        uint64_t preclassification_commands = 0;
        uint64_t bypassed_commands = 0;
        std::array<
            uint64_t,
            PRIVATE_CORE_STOP_REASON_COUNT>
            bypass_stop_reasons{};
        const auto preclassification_started =
            host_profile_enabled
            ? std::chrono::steady_clock::now()
            : std::chrono::steady_clock::time_point{};
        {
            py::gil_scoped_release release;
            for (
                std::size_t index = 0;
                index < commands.size();
                index++
            ) {
                if (
                    commands[index].core->profile !=
                        CoreProfile::FULL
                ) {
                    worker_result_positions.push_back(
                        index);
                    worker_commands.push_back(
                        std::move(commands[index]));
                    continue;
                }
                preclassification_commands++;
                FrontierPrivatePreclassification
                    probe =
                        preclassify_frontier_private_command(
                            commands[index]);
                if (probe.classified_instruction)
                    classification_calls++;
                if (probe.decode_cache_lookup) {
                    decode_cache_lookups++;
                    if (probe.decode_cache_hit)
                        decode_cache_hits++;
                    else
                        decode_cache_misses++;
                }
                if (probe.execute_private) {
                    commands[index]
                        .first_instruction_preclassified_private =
                            true;
                    worker_result_positions.push_back(
                        index);
                    worker_commands.push_back(
                        std::move(commands[index]));
                    continue;
                }

                bypassed_commands++;
                const std::size_t reason =
                    static_cast<std::size_t>(
                        probe.result.stop_reason);
                if (reason < bypass_stop_reasons.size())
                    bypass_stop_reasons[reason]++;
                results[index] =
                    std::move(probe.result);
            }
        }
        if (host_profile_enabled) {
            ConcurrencyProfileCounters& profile =
                system.concurrency_profile;
            host_saturating_increment(
                profile.frontier_routing_waves);
            host_saturating_add(
                profile.frontier_routing_commands,
                static_cast<uint64_t>(
                    commands.size()));
            host_saturating_add(
                profile.frontier_preclassification_commands,
                preclassification_commands);
            host_saturating_add(
                profile.frontier_preclassification_calls,
                classification_calls);
            host_saturating_add(
                profile.frontier_decode_cache_lookups,
                decode_cache_lookups);
            host_saturating_add(
                profile.frontier_decode_cache_hits,
                decode_cache_hits);
            host_saturating_add(
                profile.frontier_decode_cache_misses,
                decode_cache_misses);
            host_saturating_add(
                profile.worker_bypassed_commands,
                bypassed_commands);
            for (
                std::size_t reason = 0;
                reason < bypass_stop_reasons.size();
                reason++
            ) {
                host_saturating_add(
                    profile
                        .worker_bypass_stop_reasons[
                            reason],
                    bypass_stop_reasons[reason]);
            }
            host_saturating_add(
                profile.frontier_fast_path_ns,
                host_elapsed_ns(
                    preclassification_started));
        }
    } else {
        for (
            std::size_t index = 0;
            index < commands.size();
            index++
        ) {
            worker_result_positions.push_back(
                index);
            worker_commands.push_back(
                std::move(commands[index]));
        }
    }

    if (worker_commands.empty())
        return results;

    std::vector<PrivateCoreHostTelemetry>
        command_telemetry;
    if (host_profile_enabled) {
        command_telemetry.resize(
            worker_commands.size());
        for (
            std::size_t index = 0;
            index < worker_commands.size();
            index++
        ) {
            worker_commands[index].host_telemetry =
                &command_telemetry[index];
        }
    }

    WorkerWaveHostTiming wave_timing;
    const auto wave_started =
        host_profile_enabled
        ? std::chrono::steady_clock::now()
        : std::chrono::steady_clock::time_point{};
    std::vector<PrivateCoreResult> worker_results;
    {
        py::gil_scoped_release release;
        worker_results =
            system.worker_pool->execute_wave(
                std::move(worker_commands),
                host_profile_enabled
                ? &wave_timing
                : nullptr);
    }
    if (host_profile_enabled) {
        record_private_wave_profile(
            system,
            worker_results,
            command_telemetry,
            wave_timing,
            host_elapsed_ns(wave_started));
    }
    if (
        worker_results.size() !=
            worker_result_positions.size()
    ) {
        throw std::logic_error(
            "private worker returned an incomplete fast-path wave");
    }
    for (
        std::size_t index = 0;
        index < worker_results.size();
        index++
    ) {
        results[
            worker_result_positions[index]] =
                std::move(worker_results[index]);
    }
    return results;
}

static std::vector<PrivateCoreResult>
execute_strict_cycle_private_wave_under_active_batch(
        SystemState& system,
        std::vector<PrivateCoreCommand> commands,
        std::shared_ptr<SharedMemoryExecutionAdmission>
            frontier_admission) {
    if (commands.empty())
        return {};
    if (!system.native_batch_active.load(
            std::memory_order_acquire)) {
        throw std::logic_error(
            "strict-cycle private execution requires an active "
            "native system batch");
    }
    if (!system.worker_pool) {
        throw std::logic_error(
            "native worker pool is unavailable");
    }
    if (
        commands.size() >
        static_cast<std::size_t>(
            system.configured_worker_count)
    ) {
        throw std::invalid_argument(
            "strict-cycle private wave exceeds worker capacity");
    }
    if (
        !frontier_admission ||
        &frontier_admission->memory() !=
            &system.shared_memory
    ) {
        throw std::invalid_argument(
            "strict-cycle private frontier admission is invalid");
    }

    for (PrivateCoreCommand& command : commands) {
        if (
            !command.strict_cycle_one_instruction ||
            command.max_steps != 1
        ) {
            throw std::invalid_argument(
                "strict-cycle private wave contains an invalid command");
        }
        if (
            command.core_index < 0 ||
            command.core_index >=
                static_cast<int>(system.cores.size())
        ) {
            throw std::out_of_range(
                "strict-cycle private core index is out of range");
        }
        command.core = system.cores[
            static_cast<std::size_t>(
                command.core_index)].get();
        command.pending_interrupt_vector = -1;
        command.admission = frontier_admission;
    }

    py::gil_scoped_release release;
    return system.worker_pool->execute_wave(
        std::move(commands));
}

static void throw_private_full_core_internal_failure(
        const std::vector<PrivateCoreResult>& results) {
    for (const PrivateCoreResult& result : results) {
        if (
            result.stop_reason ==
            PrivateCoreStopReason::INTERNAL_FAILURE
        ) {
            throw std::runtime_error(
                "private command failed on lane " +
                std::to_string(result.lane_index) +
                ", core " +
                std::to_string(result.core_index) +
                ": " +
                result.internal_error);
        }
    }
}

static std::vector<PrivateCoreResult>
run_private_full_core_wave(
        SystemState& system,
        std::vector<PrivateCoreCommand> commands) {
    if (commands.empty())
        return {};

    NativeBatchActiveGuard active_guard(system);
    std::vector<PrivateCoreResult> results =
        execute_private_core_wave_under_active_batch(
            system,
            std::move(commands));
    throw_private_full_core_internal_failure(results);
    return results;
}

struct CoordinatorBoundarySettlement {
    int64_t total_steps = 0;
    int64_t total_cycles = 0;
    int stop_reason = -1;
    uint64_t continuations = 0;
    bool closes_dispatch = false;
    bool terminal = false;
};

static CoordinatorBoundarySettlement
validated_coordinator_settlement(
        const py::object& settled_object,
        int64_t prefix_steps,
        int64_t prefix_cycles,
        int64_t max_steps,
        const char* context) {
    py::tuple settled = settled_object.cast<py::tuple>();
    if (settled.size() != 3) {
        throw std::runtime_error(
            std::string(context) +
            " must return "
            "(invocation_steps, invocation_cycles, terminal)");
    }

    CoordinatorBoundarySettlement result;
    result.total_steps = settled[0].cast<int64_t>();
    result.total_cycles = settled[1].cast<int64_t>();
    result.terminal = settled[2].cast<bool>();
    if (
        result.total_steps < prefix_steps ||
        result.total_steps >
            std::min<int64_t>(
                max_steps,
                checked_scheduler_add(
                    prefix_steps,
                    1,
                    "coordinator boundary accounting")) ||
        result.total_cycles < prefix_cycles
    ) {
        throw std::runtime_error(
            std::string(context) +
            " returned invalid progress");
    }
    if (
        !result.terminal &&
        result.total_steps == prefix_steps
    ) {
        if (
            std::string(context) ==
            "coordinator dispatch error settlement"
        ) {
            throw std::runtime_error(
                "nonterminal native dispatch error "
                "settlement made no progress");
        }
        throw std::runtime_error(
            "nonterminal native continuation made no progress");
    }
    return result;
}

static CoordinatorBoundarySettlement
settle_private_core_terminal(
        SystemState& system,
        int core_index,
        const PrivateCoreResult& private_result,
        int64_t max_steps,
        const py::function& settle_continuation) {
    ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    const bool host_profile_enabled =
        profile.enabled;
    const std::size_t profile_origin =
        static_cast<std::size_t>(
            private_result.stop_reason);
    if (host_profile_enabled) {
        host_saturating_increment(
            profile.coordinator_boundaries);
        if (
            profile_origin <
            profile.coordinator_boundary_origins.size()
        ) {
            host_saturating_increment(
                profile.coordinator_boundary_origins[
                    profile_origin]);
        }
    }
    HostProfileWallTimer boundary_timer(
        host_profile_enabled,
        &profile.coordinator_boundary_ns);
    HostProfileWallTimer origin_timer(
        host_profile_enabled &&
            profile_origin <
                profile
                    .coordinator_boundary_origin_ns
                    .size(),
        profile_origin <
                profile
                    .coordinator_boundary_origin_ns
                    .size()
            ? &profile
                .coordinator_boundary_origin_ns[
                    profile_origin]
            : nullptr);
    CPUState& core =
        *system.execution_cores[
            static_cast<std::size_t>(core_index)];
    auto logical_guard =
        acquire_shared_memory_use(
            core,
            /*permit_native_execution=*/true);
    const int continuation_reason =
        private_result.stop_reason ==
            PrivateCoreStopReason::TRAP
        ? RUN_TRAP
        : RUN_RESET;
    CoordinatorBoundarySettlement result =
        validated_coordinator_settlement(
            settle_continuation(
                core_index,
                continuation_reason,
                private_result.trap_id,
                private_result.steps_executed,
                private_result.total_cycles),
            private_result.steps_executed,
            private_result.total_cycles,
            max_steps,
            "private trap/reset settlement");
    result.stop_reason = continuation_reason;
    result.continuations = 1;
    result.closes_dispatch = true;
    return result;
}

static CoordinatorBoundarySettlement
settle_coordinator_dispatch_error(
        int core_index,
        const PrivateCoreResult& private_result,
        int64_t max_steps,
        const py::function& settle_dispatch_error,
        py::error_already_set& error) {
    py::object settled_object =
        settle_dispatch_error(
            core_index,
            error.value());
    if (settled_object.is_none())
        throw;
    CoordinatorBoundarySettlement local =
        validated_coordinator_settlement(
            settled_object,
            0,
            0,
            1,
            "coordinator dispatch error settlement");
    CoordinatorBoundarySettlement result;
    result.total_steps =
        checked_scheduler_add(
            private_result.steps_executed,
            local.total_steps,
            "coordinator dispatch error "
            "instruction accounting");
    result.total_cycles =
        checked_scheduler_add(
            private_result.total_cycles,
            local.total_cycles,
            "coordinator dispatch error "
            "cycle accounting");
    if (result.total_steps > max_steps) {
        throw std::runtime_error(
            "coordinator dispatch error settlement "
            "returned invalid progress");
    }
    result.terminal = local.terminal;
    result.closes_dispatch = true;
    return result;
}

static bool coordinator_dispatch_requires_python(
        const RunResult& raw) {
    return (
        raw.stop_reason >= RUN_MEX_FALLBACK &&
        raw.stop_reason <= RUN_RESET
    );
}

static CoordinatorBoundarySettlement
finalize_coordinator_instruction(
        CPUState& core,
        int core_index,
        const PrivateCoreResult& private_result,
        int64_t max_steps,
        const py::function& settle_continuation,
        const RunResult& raw) {
    if (
        raw.steps_executed < 0 ||
        raw.steps_executed > 1 ||
        raw.total_cycles < 0
    ) {
        throw std::runtime_error(
            "coordinator boundary dispatch returned "
            "invalid progress");
    }

    const int64_t combined_prefix_steps =
        checked_scheduler_add(
            private_result.steps_executed,
            raw.steps_executed,
            "frontier instruction accounting");
    const int64_t combined_prefix_cycles =
        checked_scheduler_add(
            private_result.total_cycles,
            raw.total_cycles,
            "frontier cycle accounting");
    if (combined_prefix_steps > max_steps) {
        throw std::logic_error(
            "coordinator boundary exceeded its reserved budget");
    }

    if (coordinator_dispatch_requires_python(raw)) {
        CoordinatorBoundarySettlement result =
            validated_coordinator_settlement(
                settle_continuation(
                    core_index,
                    raw.stop_reason,
                    raw.trap_id,
                    combined_prefix_steps,
                    combined_prefix_cycles),
                combined_prefix_steps,
                combined_prefix_cycles,
                max_steps,
                "coordinator continuation");
        result.stop_reason = raw.stop_reason;
        result.continuations = 1;
        result.closes_dispatch = true;
        return result;
    }

    int normalized_stop_reason = raw.stop_reason;
    if (
        normalized_stop_reason == RUN_LIMIT &&
        combined_prefix_steps < max_steps
    ) {
        if (core.halted) {
            normalized_stop_reason = RUN_HALT;
        } else if (core.idle) {
            normalized_stop_reason = RUN_IDLE;
        }
    }

    CoordinatorBoundarySettlement result;
    result.total_steps = combined_prefix_steps;
    result.total_cycles = combined_prefix_cycles;
    result.stop_reason = normalized_stop_reason;
    result.terminal =
        normalized_stop_reason == RUN_HALT ||
        normalized_stop_reason == RUN_IDLE;
    result.closes_dispatch = result.terminal;
    return result;
}

class CoordinatorBoundaryProfileScope {
public:
    CoordinatorBoundaryProfileScope(
            ConcurrencyProfileCounters& profile,
            PrivateCoreStopReason origin) {
        const bool enabled = profile.enabled;
        const std::size_t origin_index =
            static_cast<std::size_t>(origin);
        if (enabled) {
            host_saturating_increment(
                profile.coordinator_boundaries);
            if (
                origin_index <
                profile.coordinator_boundary_origins.size()
            ) {
                host_saturating_increment(
                    profile.coordinator_boundary_origins[
                        origin_index]);
            }
        }
        boundary_timer_.emplace(
            enabled,
            &profile.coordinator_boundary_ns);
        origin_timer_.emplace(
            enabled &&
                origin_index <
                    profile
                        .coordinator_boundary_origin_ns
                        .size(),
            origin_index <
                    profile
                        .coordinator_boundary_origin_ns
                        .size()
                ? &profile
                    .coordinator_boundary_origin_ns[
                        origin_index]
                : nullptr);
    }

    CoordinatorBoundaryProfileScope(
        const CoordinatorBoundaryProfileScope&) = delete;
    CoordinatorBoundaryProfileScope& operator=(
        const CoordinatorBoundaryProfileScope&) = delete;

private:
    std::optional<HostProfileWallTimer>
        boundary_timer_;
    std::optional<HostProfileWallTimer>
        origin_timer_;
};

static CoordinatorBoundarySettlement
settle_private_core_coordinator_instruction(
        SystemState& system,
        int core_index,
        const PrivateCoreResult& private_result,
        int64_t max_steps,
        const StepCallbacks& callbacks,
        const py::function& settle_continuation,
        const py::function& settle_dispatch_error) {
    CoordinatorBoundaryProfileScope profile_scope(
        system.concurrency_profile,
        private_result.stop_reason);
    CPUState& core =
        *system.execution_cores[
            static_cast<std::size_t>(core_index)];
    auto logical_guard =
        acquire_shared_memory_use(
            core,
            /*permit_native_execution=*/true);
    RunResult raw{};
    try {
        py::gil_scoped_release release;
        SystemBatchExecutionPermissionGuard
            execution_permission(
                system.native_batch_active);
        CPUExecutionGuard execution_guard(core);
        raw = run_steps(core, callbacks, 1);
    } catch (py::error_already_set& error) {
        return settle_coordinator_dispatch_error(
            core_index,
            private_result,
            max_steps,
            settle_dispatch_error,
            error);
    }
    return finalize_coordinator_instruction(
        core,
        core_index,
        private_result,
        max_steps,
        settle_continuation,
        raw);
}

static void run_parallel_core_subfrontier(
        SystemState& system,
        const std::vector<
            CoreFrontierReservation>& reservations,
        const std::vector<StepCallbacks>& callbacks,
        const py::function& settle_continuation,
        const py::function& settle_dispatch_error,
        SystemBatchResult& result,
        CoreFrontierOutcome& outcome) {
    if (reservations.empty())
        return;
    ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    const bool host_profile_enabled =
        profile.enabled;
    if (host_profile_enabled) {
        host_saturating_increment(
            profile.logical_subfrontiers);
    }
    HostProfileWallTimer subfrontier_timer(
        host_profile_enabled,
        &profile.logical_subfrontier_ns);
    if (
        callbacks.size() !=
        system.execution_cores.size()
    ) {
        throw std::invalid_argument(
            "parallel core frontier callback "
            "topology is incomplete");
    }

    int64_t total_reserved = 0;
    for (
        const CoreFrontierReservation& reservation :
        reservations
    ) {
        if (
            reservation.core_index < 0 ||
            reservation.core_index >=
                static_cast<int>(
                    system.execution_cores.size()) ||
            reservation.max_steps < 0 ||
            reservation.max_steps >
                std::numeric_limits<int>::max()
        ) {
            throw std::logic_error(
                "parallel core frontier reservation "
                "is invalid");
        }
        total_reserved = checked_scheduler_add(
            total_reserved,
            reservation.max_steps,
            "frontier reservation accounting");
    }

    // Physical cohorts are an implementation detail. Even the one-lane
    // reference gathers every cohort before the first shared commit so lane
    // width cannot change exception-visible peer-private progress.
    std::shared_ptr<SharedMemoryExecutionAdmission>
        frontier_admission;
    std::unique_ptr<SharedMemoryLease>
        frontier_memory_lease;
    {
        py::gil_scoped_release release;
        frontier_admission =
            std::make_shared<
                SharedMemoryExecutionAdmission>(
                    system.shared_memory,
                    "CPUState memory is already in use");
    }
    system.mappings_sealed = true;

    std::vector<PrivateCoreResult> private_results(
        reservations.size());
    std::vector<bool> zero_cluster_probe(
        reservations.size(), false);
    std::vector<std::size_t> executable_indices;
    executable_indices.reserve(
        reservations.size());
    for (
        std::size_t index = 0;
        index < reservations.size();
        index++
    ) {
        const CoreFrontierReservation&
            reservation = reservations[index];
        if (reservation.max_steps > 0) {
            executable_indices.push_back(index);
            continue;
        }

        CPUState& core =
            *system.execution_cores[
                static_cast<std::size_t>(
                    reservation.core_index)];
        PrivateCoreResult probe;
        probe.submission_index = index;
        probe.core_index =
            reservation.core_index;
        probe.start_pc = pc(core);
        probe.end_pc = probe.start_pc;
        probe.stop_reason =
            PrivateCoreStopReason::
                INSTRUCTION_LIMIT;
        private_results[index] =
            std::move(probe);
    }

    const std::size_t physical_width =
        static_cast<std::size_t>(
            system.worker_count());
    for (
        std::size_t cohort_start = 0;
        cohort_start < executable_indices.size();
        cohort_start += physical_width
    ) {
        const std::size_t cohort_size =
            std::min<std::size_t>(
                physical_width,
                executable_indices.size() -
                    cohort_start);
        std::vector<PrivateCoreCommand> commands;
        commands.reserve(cohort_size);
        for (
            std::size_t cohort_index = 0;
            cohort_index < cohort_size;
            cohort_index++
        ) {
            const CoreFrontierReservation&
                reservation =
                    reservations[
                        executable_indices[
                            cohort_start +
                            cohort_index]];
            PrivateCoreCommand command;
            command.lane_index =
                static_cast<int>(cohort_index);
            command.core_index =
                reservation.core_index;
            command.max_steps =
                static_cast<int>(
                    reservation.max_steps);
            commands.push_back(std::move(command));
        }
        std::vector<PrivateCoreResult>
            cohort_results =
                execute_private_core_wave_under_active_batch(
                    system,
                    std::move(commands),
                    frontier_admission);
        if (cohort_results.size() != cohort_size) {
            throw std::logic_error(
                "private worker cohort returned an "
                "incomplete frontier");
        }
        for (
            std::size_t cohort_index = 0;
            cohort_index < cohort_results.size();
            cohort_index++
        ) {
            private_results[
                executable_indices[
                    cohort_start +
                    cohort_index]] =
                std::move(
                    cohort_results[
                        cohort_index]);
        }
    }
    {
        py::gil_scoped_release release;
        frontier_memory_lease =
            std::make_unique<SharedMemoryLease>(
                frontier_admission);
    }
    for (
        std::size_t index = 0;
        index < reservations.size();
        index++
    ) {
        if (reservations[index].max_steps != 0)
            continue;
        CPUState& core =
            *system.execution_cores[
                static_cast<std::size_t>(
                    reservations[index].core_index)];
        if (
            core.profile != CoreProfile::MICRO ||
            core.halted ||
            core.idle
        ) {
            continue;
        }
        const PendingClusterRequest request =
            classify_pending_cluster_request(
                system,
                core,
                RUN_EXT_FALLBACK);
        if (
            request.resource !=
                ClusterResourceKind::NONE
        ) {
            private_results[index].stop_reason =
                PrivateCoreStopReason::
                    SHARED_INSTRUCTION;
            zero_cluster_probe[index] = true;
        }
    }

    std::vector<int64_t> per_core_frontier_cycles(
        system.execution_cores.size(),
        0);
    auto merge_fragment = [&](
            int core_index,
            CoreDispatchResult fragment) {
        if (fragment.steps < 0 || fragment.cycles < 0) {
            throw std::logic_error(
                "parallel core frontier produced "
                "negative progress");
        }
        outcome.steps = checked_scheduler_add(
            outcome.steps,
            fragment.steps,
            "frontier aggregate instruction accounting");
        if (outcome.steps > total_reserved) {
            throw std::logic_error(
                "parallel core frontier exceeded "
                "its aggregate reservation");
        }
        const std::size_t index =
            static_cast<std::size_t>(core_index);
        per_core_frontier_cycles[index] =
            checked_scheduler_add(
                per_core_frontier_cycles[index],
                fragment.cycles,
                "frontier per-core cycle accounting");
        outcome.cycles = std::max(
            outcome.cycles,
            per_core_frontier_cycles[index]);
        merge_core_dispatch(
            result, core_index, fragment);
    };

    const PrivateCoreResult*
        first_internal_failure = nullptr;
    std::vector<bool> dispatch_open(
        reservations.size(), false);
    for (
        std::size_t index = 0;
        index < reservations.size();
        index++
    ) {
        const CoreFrontierReservation& reservation =
            reservations[index];
        const PrivateCoreResult& private_result =
            private_results[index];
        if (
            private_result.core_index !=
                reservation.core_index ||
            private_result.steps_executed < 0 ||
            private_result.steps_executed >
                reservation.max_steps ||
            private_result.total_cycles < 0
        ) {
            throw std::logic_error(
                "private worker returned invalid "
                "frontier progress");
        }

        const bool boundary_requires_room =
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    ICACHE_BOUNDARY ||
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    SHARED_INSTRUCTION ||
            private_result.stop_reason ==
                PrivateCoreStopReason::TRAP ||
            private_result.stop_reason ==
                PrivateCoreStopReason::RESET;
        if (
            boundary_requires_room &&
            !zero_cluster_probe[index] &&
            private_result.steps_executed >=
                reservation.max_steps
        ) {
            throw std::logic_error(
                "private boundary exhausted its "
                "coordinator reservation");
        }
        if (
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    INSTRUCTION_LIMIT &&
            private_result.steps_executed !=
                reservation.max_steps
        ) {
            throw std::logic_error(
                "private instruction-limit result "
                "did not exhaust its reservation");
        }
        if (
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    INTERRUPT_BOUNDARY &&
            (
                private_result.steps_executed != 0 ||
                private_result.total_cycles != 0
            )
        ) {
            throw std::logic_error(
                "private interrupt boundary made progress");
        }

        if (
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    INTERNAL_FAILURE
        ) {
            if (first_internal_failure == nullptr)
                first_internal_failure =
                    &private_result;
            continue;
        }

        if (
            private_result.steps_executed > 0 ||
            private_result.total_cycles > 0
        ) {
            CoreDispatchResult prefix;
            prefix.steps =
                private_result.steps_executed;
            prefix.cycles =
                private_result.total_cycles;
            merge_fragment(
                reservation.core_index,
                std::move(prefix));
        }

        if (
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    INTERRUPT_BOUNDARY
        ) {
            outcome.interrupt_boundary = true;
            outcome.interrupt_cores.push_back(
                reservation.core_index);
            continue;
        }

        dispatch_open[index] = true;
        if (
            private_result.stop_reason ==
                PrivateCoreStopReason::
                    INSTRUCTION_LIMIT ||
            private_result.stop_reason ==
                PrivateCoreStopReason::HALTED ||
            private_result.stop_reason ==
                PrivateCoreStopReason::IDLE
        ) {
            int reason = RUN_LIMIT;
            if (
                private_result.steps_executed <
                    reservation.max_steps
            ) {
                if (
                    private_result.stop_reason ==
                    PrivateCoreStopReason::HALTED
                ) {
                    reason = RUN_HALT;
                } else if (
                    private_result.stop_reason ==
                    PrivateCoreStopReason::IDLE
                ) {
                    reason = RUN_IDLE;
                }
            }
            CoreDispatchResult terminal;
            terminal.dispatches = 1;
            terminal.stop_reasons[
                static_cast<std::size_t>(reason)] = 1;
            merge_fragment(
                reservation.core_index,
                std::move(terminal));
            if (
                private_result.stop_reason ==
                    PrivateCoreStopReason::HALTED ||
                private_result.stop_reason ==
                    PrivateCoreStopReason::IDLE
            ) {
                outcome.terminal_cores.push_back(
                    reservation.core_index);
            }
            dispatch_open[index] = false;
        }
    }

    if (first_internal_failure != nullptr) {
        throw std::runtime_error(
            "private command failed on core " +
            std::to_string(
                first_internal_failure->core_index) +
            ": " +
            first_internal_failure->internal_error);
    }

    const int first_micro_index =
        static_cast<int>(system.cores.size());
    auto capture_cluster_request = [&](
            std::size_t index)
            -> std::optional<
                DeferredClusterRequest> {
        const int core_index =
            reservations[index].core_index;
        CPUState& core =
            *system.execution_cores[
                static_cast<std::size_t>(
                    core_index)];
        if (
            core.profile != CoreProfile::MICRO ||
            core.halted ||
            core.idle
        ) {
            return std::nullopt;
        }
        const PendingClusterRequest request =
            classify_pending_cluster_request(
                system,
                core,
                RUN_EXT_FALLBACK);
        if (
            request.resource ==
                ClusterResourceKind::NONE
        ) {
            return std::nullopt;
        }
        if (core_index < first_micro_index) {
            throw std::logic_error(
                "reduced core has an invalid global "
                "execution index");
        }
        const int micro_index =
            core_index - first_micro_index;
        DeferredClusterRequest deferred;
        deferred.core_index = core_index;
        deferred.cluster_index =
            micro_index /
            SystemState::MICRO_CORES_PER_CLUSTER;
        deferred.local_core =
            micro_index %
            SystemState::MICRO_CORES_PER_CLUSTER;
        deferred.instruction_pc = pc(core);
        deferred.request = request;
        if (
            deferred.cluster_index < 0 ||
            deferred.cluster_index >=
                static_cast<int>(
                    system.cluster_states.size())
        ) {
            throw std::logic_error(
                "reduced core cluster index is out of range");
        }
        return deferred;
    };

    // Classify the worker-published boundaries while one admission still
    // freezes every mapping across every physical cohort. The admission is
    // deliberately released before any Python/coordinator continuation.
    std::vector<std::optional<DeferredClusterRequest>>
        initial_cluster_requests(
            reservations.size());
    for (
        std::size_t index = 0;
        index < reservations.size();
        index++
    ) {
        if (
            dispatch_open[index] &&
            (
                private_results[index].stop_reason ==
                    PrivateCoreStopReason::
                        SHARED_INSTRUCTION ||
                zero_cluster_probe[index]
            )
        ) {
            initial_cluster_requests[index] =
                capture_cluster_request(index);
        }
    }
    frontier_memory_lease.reset();
    frontier_admission.reset();

    auto publish_settlement = [&](
            std::size_t index,
            const CoordinatorBoundarySettlement&
                settlement) {
        outcome.coordinator_state_changed = true;
        const CoreFrontierReservation&
            reservation = reservations[index];
        const PrivateCoreResult& private_result =
            private_results[index];
        if (
            settlement.total_steps <
                private_result.steps_executed ||
            settlement.total_cycles <
                private_result.total_cycles
        ) {
            throw std::logic_error(
                "coordinator settlement lost a "
                "private prefix");
        }
        CoreDispatchResult suffix;
        suffix.steps =
            settlement.total_steps -
            private_result.steps_executed;
        suffix.cycles =
            settlement.total_cycles -
            private_result.total_cycles;
        suffix.dispatches = 1;
        suffix.continuations =
            settlement.continuations;
        if (
            settlement.stop_reason >= RUN_LIMIT &&
            settlement.stop_reason <= RUN_RESET
        ) {
            suffix.stop_reasons[
                static_cast<std::size_t>(
                    settlement.stop_reason)] = 1;
        }
        merge_fragment(
            reservation.core_index,
            std::move(suffix));
        if (settlement.closes_dispatch) {
            outcome.dispatch_boundary_cores.push_back(
                reservation.core_index);
        }
        if (settlement.terminal) {
            outcome.terminal_cores.push_back(
                reservation.core_index);
        }
        dispatch_open[index] = false;
    };

    // Preserve the established mixed-topology phase barrier: ordinary
    // boundaries settle first in cyclic order. A later live boundary that
    // has become a cluster request is left for the post-commit snapshot
    // instead of bypassing arbitration with stale metadata.
    bool complete_full_core_ordinary_pass =
        reservations.size() > 1 &&
        !thread_owns_shared_memory(
            system.shared_memory) &&
        !thread_owns_exclusive_memory(
            system.shared_memory);
    if (complete_full_core_ordinary_pass) {
        for (
            std::size_t index = 0;
            index < reservations.size();
            index++
        ) {
            const CoreFrontierReservation&
                reservation = reservations[index];
            const PrivateCoreResult&
                private_result = private_results[index];
            const CPUState& core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        reservation.core_index)];
            if (
                !dispatch_open[index] ||
                initial_cluster_requests[index].has_value() ||
                core.profile != CoreProfile::FULL ||
                (
                    private_result.stop_reason !=
                        PrivateCoreStopReason::
                            ICACHE_BOUNDARY &&
                    private_result.stop_reason !=
                        PrivateCoreStopReason::
                            SHARED_INSTRUCTION
                )
            ) {
                complete_full_core_ordinary_pass = false;
                break;
            }
        }
    }

    if (complete_full_core_ordinary_pass) {
        // One released segment covers every callback-free success. A Python
        // error or continuation restores the GIL while the exact per-core
        // logical guard and profile scope remain alive, then resumes later
        // cyclic peers in a fresh segment only if settlement permits.
        std::size_t index = 0;
        std::optional<CoordinatorBoundaryProfileScope>
            active_profile_scope;
        std::unique_ptr<SharedMemoryUseGuard>
            active_logical_guard;
        RunResult active_raw{};
        while (index < reservations.size()) {
            bool active_requires_python = false;
            try {
                py::gil_scoped_release release;
                while (index < reservations.size()) {
                    const CoreFrontierReservation&
                        reservation = reservations[index];
                    const PrivateCoreResult&
                        private_result =
                            private_results[index];
                    CPUState& core =
                        *system.execution_cores[
                            static_cast<std::size_t>(
                                reservation.core_index)];

                    if (core.halted || core.idle) {
                        outcome.terminal_cores.push_back(
                            reservation.core_index);
                        dispatch_open[index] = false;
                        index++;
                        continue;
                    }

                    active_profile_scope.emplace(
                        system.concurrency_profile,
                        private_result.stop_reason);
                    active_logical_guard =
                        std::make_unique<
                            SharedMemoryUseGuard>(
                                *core.memory,
                                &core);
                    {
                        SystemBatchExecutionPermissionGuard
                            execution_permission(
                                system.native_batch_active);
                        CPUExecutionGuard execution_guard(core);
                        active_raw = run_steps(
                            core,
                            callbacks[
                                static_cast<std::size_t>(
                                    reservation.core_index)],
                            1);
                    }

                    if (
                        coordinator_dispatch_requires_python(
                            active_raw)
                    ) {
                        active_requires_python = true;
                        break;
                    }

                    CoordinatorBoundarySettlement settlement =
                        finalize_coordinator_instruction(
                            core,
                            reservation.core_index,
                            private_result,
                            reservation.max_steps,
                            settle_continuation,
                            active_raw);
                    active_logical_guard.reset();
                    active_profile_scope.reset();
                    publish_settlement(index, settlement);
                    index++;
                }
            } catch (py::error_already_set& error) {
                const CoreFrontierReservation&
                    reservation = reservations[index];
                const PrivateCoreResult&
                    private_result = private_results[index];
                CoordinatorBoundarySettlement settlement =
                    settle_coordinator_dispatch_error(
                        reservation.core_index,
                        private_result,
                        reservation.max_steps,
                        settle_dispatch_error,
                        error);
                active_logical_guard.reset();
                active_profile_scope.reset();
                publish_settlement(index, settlement);
                index++;
                continue;
            }

            if (active_requires_python) {
                const CoreFrontierReservation&
                    reservation = reservations[index];
                const PrivateCoreResult&
                    private_result = private_results[index];
                CPUState& core =
                    *system.execution_cores[
                        static_cast<std::size_t>(
                            reservation.core_index)];
                CoordinatorBoundarySettlement settlement =
                    finalize_coordinator_instruction(
                        core,
                        reservation.core_index,
                        private_result,
                        reservation.max_steps,
                        settle_continuation,
                        active_raw);
                active_logical_guard.reset();
                active_profile_scope.reset();
                publish_settlement(index, settlement);
                index++;
            }
        }
    } else {
        for (
            std::size_t index = 0;
            index < reservations.size();
            index++
        ) {
            if (
                !dispatch_open[index] ||
                initial_cluster_requests[index].has_value()
            ) {
                continue;
            }
            const CoreFrontierReservation&
                reservation = reservations[index];
            const PrivateCoreResult& private_result =
                private_results[index];
            CPUState& core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        reservation.core_index)];

            if (
                core.profile == CoreProfile::MICRO &&
                private_result.stop_reason ==
                    PrivateCoreStopReason::
                        SHARED_INSTRUCTION
            ) {
                std::optional<DeferredClusterRequest>
                    live_request;
                {
                    auto memory_guard =
                        acquire_shared_memory_use(core);
                    live_request =
                        capture_cluster_request(index);
                }
                if (live_request.has_value()) {
                    initial_cluster_requests[index] =
                        std::move(live_request);
                    continue;
                }
            }

            if (core.halted || core.idle) {
                outcome.terminal_cores.push_back(
                    reservation.core_index);
                dispatch_open[index] = false;
                continue;
            }

            CoordinatorBoundarySettlement settlement;
            if (
                private_result.stop_reason ==
                    PrivateCoreStopReason::TRAP ||
                private_result.stop_reason ==
                    PrivateCoreStopReason::RESET
            ) {
                settlement =
                    settle_private_core_terminal(
                        system,
                        reservation.core_index,
                        private_result,
                        reservation.max_steps,
                        settle_continuation);
            } else if (
                private_result.stop_reason ==
                    PrivateCoreStopReason::
                        ICACHE_BOUNDARY ||
                private_result.stop_reason ==
                    PrivateCoreStopReason::
                        SHARED_INSTRUCTION
            ) {
                settlement =
                    settle_private_core_coordinator_instruction(
                        system,
                        reservation.core_index,
                        private_result,
                        reservation.max_steps,
                        callbacks[
                            static_cast<std::size_t>(
                                reservation.core_index)],
                        settle_continuation,
                        settle_dispatch_error);
            } else {
                throw std::logic_error(
                    "private frontier left an unsupported "
                    "coordinator boundary");
            }
            publish_settlement(index, settlement);
        }
    }

    // Re-snapshot cluster requests from live PCs and live cluster state only
    // after every earlier ordinary effect is committed.
    std::vector<std::optional<DeferredClusterRequest>>
        cluster_requests(reservations.size());
    {
        CPUState& guard_core =
            *system.execution_cores[
                static_cast<std::size_t>(
                    reservations.front().core_index)];
        auto memory_guard =
            acquire_shared_memory_use(guard_core);
        for (
            std::size_t index = 0;
            index < reservations.size();
            index++
        ) {
            if (!dispatch_open[index])
                continue;
            cluster_requests[index] =
                capture_cluster_request(index);
            if (!cluster_requests[index].has_value()) {
                outcome.cluster_deferred_cores.push_back(
                    reservations[index].core_index);
                dispatch_open[index] = false;
            }
        }
    }

    struct ClusterGrantGroup {
        int cluster_index = -1;
        ClusterResourceKind resource =
            ClusterResourceKind::NONE;
        std::vector<std::size_t> candidates;
        std::optional<std::size_t> winner;
    };
    std::vector<ClusterGrantGroup> groups;
    for (
        std::size_t cluster_index = 0;
        cluster_index <
            system.cluster_states.size();
        cluster_index++
    ) {
        ClusterState& cluster =
            system.cluster_states[cluster_index];
        for (
            std::size_t resource_index = 1;
            resource_index <
                CLUSTER_RESOURCE_KIND_COUNT;
            resource_index++
        ) {
            ClusterGrantGroup group;
            group.cluster_index =
                static_cast<int>(cluster_index);
            group.resource =
                static_cast<ClusterResourceKind>(
                    resource_index);
            std::vector<int> local_candidates;
            std::vector<bool> sha_lock_protected;
            for (
                std::size_t index = 0;
                index < cluster_requests.size();
                index++
            ) {
                const auto& request =
                    cluster_requests[index];
                if (
                    request.has_value() &&
                    request->cluster_index ==
                        group.cluster_index &&
                    request->request.resource ==
                        group.resource
                ) {
                    group.candidates.push_back(index);
                    local_candidates.push_back(
                        request->local_core);
                    sha_lock_protected.push_back(
                        request->request
                            .sha_lock_protected);
                }
            }
            if (group.candidates.empty())
                continue;
            const std::optional<int> local_winner =
                cluster.choose(
                    group.resource,
                    local_candidates,
                    sha_lock_protected);
            if (local_winner.has_value()) {
                const auto selected =
                    std::find_if(
                        group.candidates.begin(),
                        group.candidates.end(),
                        [&](std::size_t index) {
                            return cluster_requests[index]
                                ->local_core ==
                                *local_winner;
                        });
                if (selected ==
                    group.candidates.end()) {
                    throw std::logic_error(
                        "cluster winner has no immutable "
                        "request");
                }
                group.winner = *selected;
            }
            groups.push_back(std::move(group));
        }
    }

    std::sort(
        groups.begin(),
        groups.end(),
        [](const ClusterGrantGroup& left,
           const ClusterGrantGroup& right) {
            const std::size_t left_index =
                left.winner.value_or(
                    left.candidates.front());
            const std::size_t right_index =
                right.winner.value_or(
                    right.candidates.front());
            return left_index < right_index;
        });

    // Arbitration choices are frozen together, not discovered in winner
    // commit order. Every nonselected request is a loser, including requests
    // that hard eligibility left without a winner. A cyclic-earlier loser can
    // therefore fund a later zero-credit winner even when the loser's own
    // resource group appears later in the commit sequence.
    std::vector<bool> frozen_cluster_losers(
        reservations.size(), false);
    std::vector<int64_t> frozen_transferable_credit(
        reservations.size(), 0);
    for (const ClusterGrantGroup& group : groups) {
        for (std::size_t index :
             group.candidates) {
            if (
                group.winner.has_value() &&
                index == *group.winner
            )
                continue;
            frozen_cluster_losers[index] = true;
            frozen_transferable_credit[index] =
                reservations[index].max_steps -
                private_results[index]
                    .steps_executed;
        }
    }

    // Capacity failure must precede the first guest cluster mutation. Plan
    // the exact initially fundable winners in commit order: later live
    // revalidation may remove one of these grants, but can never add one.
    // This avoids rejecting an unfundable zero-credit request merely because
    // its dormant counter has already reached the representation limit.
    const int64_t initial_cluster_commit_slots =
        total_reserved - outcome.steps;
    if (initial_cluster_commit_slots < 0) {
        throw std::logic_error(
            "cluster frontier exceeded its aggregate credit");
    }
    std::vector<int64_t> planned_transferable_credit =
        frozen_transferable_credit;
    std::vector<bool> planned_winners(
        groups.size(), false);
    int64_t planned_commit_slots =
        initial_cluster_commit_slots;
    for (
        std::size_t group_index = 0;
        group_index < groups.size();
        group_index++
    ) {
        const ClusterGrantGroup& group =
            groups[group_index];
        if (!group.winner.has_value())
            continue;

        const std::size_t winner = *group.winner;
        const PrivateCoreResult& private_result =
            private_results[winner];
        const CoreFrontierReservation& reservation =
            reservations[winner];
        if (
            private_result.steps_executed >=
                reservation.max_steps
        ) {
            const auto donor =
                std::find_if(
                    planned_transferable_credit.begin(),
                    planned_transferable_credit.begin() +
                        static_cast<std::ptrdiff_t>(winner),
                    [](int64_t credit) {
                        return credit > 0;
                    });
            if (
                donor ==
                planned_transferable_credit.begin() +
                    static_cast<std::ptrdiff_t>(winner)
            ) {
                continue;
            }
            (*donor)--;
        }
        if (planned_commit_slots == 0)
            continue;
        planned_winners[group_index] = true;
        planned_commit_slots--;
    }

    std::vector<uint64_t> funded_winners_per_cluster(
        system.cluster_states.size(), 0);
    for (
        std::size_t group_index = 0;
        group_index < groups.size();
        group_index++
    ) {
        if (!planned_winners[group_index])
            continue;
        const ClusterGrantGroup& group =
            groups[group_index];
        ClusterState& cluster =
            system.cluster_states[
                static_cast<std::size_t>(
                    group.cluster_index)];
        const std::size_t resource_index =
            static_cast<std::size_t>(
                group.resource);
        if (
            cluster.grant_counts[resource_index] ==
                std::numeric_limits<uint64_t>::max()
        ) {
            throw std::overflow_error(
                "native cluster grant accounting overflow");
        }
        funded_winners_per_cluster[
            static_cast<std::size_t>(
                group.cluster_index)]++;
    }
    for (
        std::size_t cluster_index = 0;
        cluster_index <
            system.cluster_states.size();
        cluster_index++
    ) {
        const uint64_t required =
            funded_winners_per_cluster[
                cluster_index];
        if (
            required >
            std::numeric_limits<uint64_t>::max() -
                system.cluster_states[
                    cluster_index].grant_sequence
        ) {
            throw std::overflow_error(
                "native cluster grant accounting overflow");
        }
    }

    int64_t cluster_commit_slots =
        initial_cluster_commit_slots;
    std::vector<int64_t> transferable_credit =
        frozen_transferable_credit;
    for (
        std::size_t group_index = 0;
        group_index < groups.size();
        group_index++
    ) {
        const ClusterGrantGroup& group =
            groups[group_index];
        if (!group.winner.has_value()) {
            for (std::size_t index :
                 group.candidates) {
                outcome.cluster_lost_cores.push_back(
                    reservations[index].core_index);
                dispatch_open[index] = false;
            }
            continue;
        }

        const std::size_t winner = *group.winner;
        const DeferredClusterRequest& frozen_winner =
            *cluster_requests[winner];
        bool stable = false;
        {
            CPUState& guard_core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        reservations[winner].core_index)];
            auto memory_guard =
                acquire_shared_memory_use(guard_core);
            const std::optional<
                DeferredClusterRequest> live =
                    capture_cluster_request(
                        winner);
            stable =
                live.has_value() &&
                live->cluster_index ==
                    frozen_winner.cluster_index &&
                live->local_core ==
                    frozen_winner.local_core &&
                live->instruction_pc ==
                    frozen_winner.instruction_pc &&
                live->request.resource ==
                    frozen_winner.request.resource &&
                live->request.operation ==
                    frozen_winner.request.operation &&
                live->request.sha_transaction ==
                    frozen_winner.request.sha_transaction &&
                live->request.sha_lock_protected ==
                    frozen_winner.request
                        .sha_lock_protected &&
                live->request.continuation_reason ==
                    frozen_winner.request
                        .continuation_reason &&
                live->request.encoding_length ==
                    frozen_winner.request
                        .encoding_length &&
                live->request.encoding ==
                    frozen_winner.request.encoding &&
                system.cluster_states[
                    static_cast<std::size_t>(
                        group.cluster_index)]
                    .local_core_is_eligible(
                        group.resource,
                        frozen_winner.local_core,
                        frozen_winner.request
                            .sha_lock_protected);
        }

        if (!stable) {
            for (std::size_t index :
                 group.candidates) {
                if (frozen_cluster_losers[index]) {
                    outcome.cluster_lost_cores.push_back(
                        reservations[index].core_index);
                } else {
                    outcome.cluster_deferred_cores.push_back(
                        reservations[index].core_index);
                }
                dispatch_open[index] = false;
            }
            continue;
        }

        for (std::size_t index :
             group.candidates) {
            if (index == winner)
                continue;
            outcome.cluster_lost_cores.push_back(
                reservations[index].core_index);
            dispatch_open[index] = false;
        }

        if (!planned_winners[group_index]) {
            outcome.cluster_deferred_cores.push_back(
                reservations[winner].core_index);
            dispatch_open[winner] = false;
            continue;
        }

        const PrivateCoreResult& private_result =
            private_results[winner];
        const CoreFrontierReservation& reservation =
            reservations[winner];
        int64_t effective_max_steps =
            reservation.max_steps;
        if (
            private_result.steps_executed >=
                effective_max_steps
        ) {
            std::optional<std::size_t> donor;
            for (
                std::size_t donor_index = 0;
                donor_index < winner;
                donor_index++
            ) {
                if (
                    transferable_credit[
                        donor_index] > 0
                ) {
                    donor = donor_index;
                    break;
                }
            }
            if (
                !donor.has_value() ||
                cluster_commit_slots == 0
            ) {
                outcome.cluster_deferred_cores
                    .push_back(
                        reservation.core_index);
                dispatch_open[winner] = false;
                continue;
            }
            transferable_credit[*donor]--;
            effective_max_steps =
                checked_scheduler_add(
                    private_result.steps_executed,
                    1,
                    "cluster forward-credit accounting");
            outcome.cluster_credit_transfers
                .push_back(
                    FrontierCreditTransfer{
                        reservations[*donor]
                            .core_index,
                        reservation.core_index,
                        1,
                    });
        }
        if (cluster_commit_slots == 0) {
            outcome.cluster_deferred_cores.push_back(
                reservation.core_index);
            dispatch_open[winner] = false;
            continue;
        }

        const DeferredClusterRequest& granted =
            *cluster_requests[winner];
        ClusterState& granted_cluster =
            system.cluster_states[
                static_cast<std::size_t>(
                    granted.cluster_index)];
        const ClusterState cluster_checkpoint =
            granted_cluster;
        CoordinatorBoundarySettlement settlement;
        try {
            settlement =
                settle_private_core_coordinator_instruction(
                    system,
                    reservation.core_index,
                    private_result,
                    effective_max_steps,
                    callbacks[
                        static_cast<std::size_t>(
                            reservation.core_index)],
                    settle_continuation,
                    settle_dispatch_error);
            const int64_t expected_retirement =
                checked_scheduler_add(
                    private_result.steps_executed,
                    1,
                    "cluster grant retirement accounting");
            const bool cancelled_at_terminal =
                settlement.terminal &&
                settlement.total_steps ==
                    private_result.steps_executed;
            if (
                settlement.total_steps !=
                    expected_retirement &&
                !cancelled_at_terminal
            ) {
                throw std::runtime_error(
                    "granted cluster continuation must "
                    "retire one instruction or close at a "
                    "terminal cancellation boundary");
            }
            if (
                settlement.stop_reason !=
                    granted.request.continuation_reason
            ) {
                throw std::logic_error(
                    "cluster settlement crossed an unexpected "
                    "coordinator boundary");
            }

            // Validate and publish every potentially failing scheduler
            // mutation before advancing arbitration. Python cluster
            // continuations can acquire a CRC/SHA lock before raising; the
            // checkpoint prevents a failed winner from orphaning that lock
            // or partially publishing shared-engine state.
            publish_settlement(winner, settlement);
            granted_cluster.commit(
                granted.request.resource,
                granted.local_core,
                granted.request.operation,
                granted.request.sha_transaction);
        } catch (...) {
            granted_cluster = cluster_checkpoint;
            throw;
        }
        const int64_t committed_steps =
            settlement.total_steps -
            private_result.steps_executed;
        if (committed_steps > cluster_commit_slots) {
            throw std::logic_error(
                "cluster settlement exceeded aggregate "
                "frontier credit");
        }
        cluster_commit_slots -= committed_steps;
    }

    for (
        std::size_t index = 0;
        index < dispatch_open.size();
        index++
    ) {
        if (dispatch_open[index]) {
            outcome.cluster_deferred_cores.push_back(
                reservations[index].core_index);
            dispatch_open[index] = false;
        }
    }
}

static void run_parallel_core_round(
        SystemState& system,
        const std::vector<
            CoreFrontierReservation>& reservations,
        const std::vector<StepCallbacks>& callbacks,
        const py::function& settle_continuation,
        const py::function& settle_dispatch_error,
        SystemBatchResult& result,
        CoreFrontierOutcome& outcome) {
    if (reservations.empty())
        return;
    ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    const bool host_profile_enabled =
        profile.enabled;
    if (host_profile_enabled) {
        host_saturating_increment(
            profile.scheduler_rounds);
    }
    HostProfileWallTimer round_timer(
        host_profile_enabled,
        &profile.scheduler_round_ns);
    if (!system.worker_pool)
        throw std::logic_error(
            "native worker pool is unavailable");

    const std::size_t core_count =
        system.execution_cores.size();
    if (callbacks.size() != core_count) {
        throw std::invalid_argument(
            "parallel core round callback "
            "topology is incomplete");
    }

    int64_t total_reserved = 0;
    std::vector<bool> seen_cores(core_count, false);
    for (
        const CoreFrontierReservation& reservation :
        reservations
    ) {
        if (
            reservation.core_index < 0 ||
            reservation.core_index >=
                static_cast<int>(core_count) ||
            reservation.max_steps < 0 ||
            reservation.max_steps >
                std::numeric_limits<int>::max()
        ) {
            throw std::logic_error(
                "parallel core round reservation "
                "is invalid");
        }
        const std::size_t core_index =
            static_cast<std::size_t>(
                reservation.core_index);
        if (seen_cores[core_index]) {
            throw std::logic_error(
                "parallel core round reserved one "
                "core twice");
        }
        seen_cores[core_index] = true;
        total_reserved = checked_scheduler_add(
            total_reserved,
            reservation.max_steps,
            "round reservation accounting");
    }

    // A cluster loser may retry after every progressing instruction frontier,
    // so command/dispatch capacity is bounded by every reservation position
    // participating at every retired step, plus one terminal attempt each.
    // Retired helper steps themselves remain bounded by total_reserved.
    const uint64_t reservation_count =
        static_cast<uint64_t>(
            reservations.size());
    const uint64_t retired_capacity =
        static_cast<uint64_t>(
            total_reserved);
    if (
        retired_capacity != 0 &&
        reservation_count >
            (
                std::numeric_limits<uint64_t>::max() -
                reservation_count
            ) / retired_capacity
    ) {
        throw std::overflow_error(
            "native core round capacity overflow");
    }
    const uint64_t capacity_bound =
        reservation_count *
            retired_capacity +
        reservation_count;
    system.worker_pool->validate_private_capacity(
        capacity_bound,
        capacity_bound,
        retired_capacity);
    if (
        capacity_bound >
            std::numeric_limits<uint64_t>::max() -
                system.native_dispatches
    ) {
        throw std::overflow_error(
            "native scheduler dispatch counter overflow");
    }

    std::vector<int64_t> remaining_steps;
    std::vector<int64_t> round_credit;
    remaining_steps.reserve(reservations.size());
    round_credit.reserve(reservations.size());
    int64_t round_quantum = 0;
    for (
        const CoreFrontierReservation& reservation :
        reservations
    ) {
        remaining_steps.push_back(
            reservation.max_steps);
        round_credit.push_back(
            reservation.max_steps);
        round_quantum = std::max(
            round_quantum,
            reservation.max_steps);
    }
    std::vector<int64_t> per_reservation_cycles(
        reservations.size(), 0);
    std::vector<bool> done(
        reservations.size(), false);
    std::vector<bool> dispatch_open(
        reservations.size(), false);
    std::vector<bool> reservation_progress(
        reservations.size(), false);
    const int round_start_cursor =
        system.scheduler_cursor;

    auto refresh_round_cursor = [&]() {
        int cursor = round_start_cursor;
        for (
            std::size_t index = 0;
            index < reservations.size();
            index++
        ) {
            if (reservation_progress[index]) {
                cursor =
                    (
                        reservations[index].core_index +
                        1
                    ) % static_cast<int>(core_count);
            }
        }
        system.scheduler_cursor = cursor;
    };

    auto close_dispatch = [&](
            std::size_t reservation_index,
            int stop_reason) {
        if (!dispatch_open[reservation_index])
            return;
        CoreDispatchResult completion;
        completion.dispatches = 1;
        if (
            stop_reason >= RUN_LIMIT &&
            stop_reason <= RUN_RESET
        ) {
            completion.stop_reasons[
                static_cast<std::size_t>(
                    stop_reason)] = 1;
        }
        merge_core_dispatch(
            result,
            reservations[
                reservation_index].core_index,
            std::move(completion));
        dispatch_open[reservation_index] = false;
    };

    // Preserve the established work-conserving equal-QoS round. When an
    // earlier core cannot use its credit, a later cyclic peer whose initial
    // reservation was truncated by the aggregate budget may consume that
    // slack up to the common round quantum. Credit never flows backward
    // across an already completed cyclic position.
    auto release_unused_credit = [&](
            std::size_t donor_index) {
        int64_t available =
            remaining_steps[donor_index];
        if (available == 0)
            return;
        if (
            available < 0 ||
            available > round_credit[donor_index]
        ) {
            throw std::logic_error(
                "parallel core round has invalid "
                "unused credit");
        }
        remaining_steps[donor_index] = 0;
        round_credit[donor_index] -= available;

        for (
            std::size_t recipient_index =
                donor_index + 1;
            recipient_index < reservations.size() &&
                available > 0;
            recipient_index++
        ) {
            if (done[recipient_index])
                continue;
            const int64_t headroom =
                round_quantum -
                round_credit[recipient_index];
            if (headroom <= 0)
                continue;
            const int64_t transferred =
                std::min(available, headroom);
            round_credit[recipient_index] +=
                transferred;
            remaining_steps[recipient_index] +=
                transferred;
            available -= transferred;
        }
    };

    auto earlier_reservation_unfinished = [&](
            std::size_t reservation_index) {
        for (
            std::size_t earlier_index = 0;
            earlier_index < reservation_index;
            earlier_index++
        ) {
            if (!done[earlier_index])
                return true;
        }
        return false;
    };

    auto initialize_subfrontier_result = [&]() {
        SystemBatchResult subfrontier;
        subfrontier.per_core_instructions.assign(
            core_count, 0);
        subfrontier.per_core_cycles.assign(
            core_count, 0);
        subfrontier.per_core_dispatches.assign(
            core_count, 0);
        subfrontier.per_core_interrupts.assign(
            core_count, 0);
        subfrontier.per_core_stop_reasons.assign(
            core_count, {});
        return subfrontier;
    };

    while (true) {
        std::vector<bool> immediate_cluster_request(
            reservations.size(), false);
        {
            CPUState& guard_core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        reservations.front().core_index)];
            auto memory_guard =
                acquire_shared_memory_use(guard_core);
            for (
                std::size_t index = 0;
                index < reservations.size();
                index++
            ) {
                if (done[index])
                    continue;
                CPUState& core =
                    *system.execution_cores[
                        static_cast<std::size_t>(
                            reservations[index]
                                .core_index)];
                if (
                    core.profile !=
                        CoreProfile::MICRO ||
                    core.halted ||
                    core.idle
                ) {
                    continue;
                }
                immediate_cluster_request[index] =
                    classify_pending_cluster_request(
                        system,
                        core,
                        RUN_EXT_FALLBACK)
                        .resource !=
                    ClusterResourceKind::NONE;
            }
        }

        std::vector<std::size_t>
            participating_reservations;
        participating_reservations.reserve(
            reservations.size());

        for (
            std::size_t index = 0;
            index < reservations.size();
            index++
        ) {
            if (done[index])
                continue;
            if (remaining_steps[index] == 0) {
                if (
                    earlier_reservation_unfinished(
                        index)
                ) {
                    bool prefix_is_pending_cluster =
                        immediate_cluster_request[index];
                    for (
                        std::size_t earlier_index = 0;
                        earlier_index < index &&
                            prefix_is_pending_cluster;
                        earlier_index++
                    ) {
                        if (
                            !done[earlier_index] &&
                            !immediate_cluster_request[
                                earlier_index]
                        ) {
                            prefix_is_pending_cluster =
                                false;
                        }
                    }
                    if (prefix_is_pending_cluster) {
                        if (!dispatch_open[index]) {
                            checked_scheduler_increment(
                                system.native_dispatches,
                                "dispatch counter");
                            dispatch_open[index] = true;
                        }
                        participating_reservations
                            .push_back(index);
                    }
                    continue;
                }
                close_dispatch(index, RUN_LIMIT);
                done[index] = true;
                continue;
            }

            const int core_index =
                reservations[index].core_index;
            CPUState& core =
                *system.execution_cores[
                    static_cast<std::size_t>(
                        core_index)];
            if (core.halted || core.idle) {
                close_dispatch(
                    index,
                    core.halted
                        ? RUN_HALT
                        : RUN_IDLE);
                release_unused_credit(index);
                done[index] = true;
                continue;
            }
            if (
                pending_enabled_core_interrupt(
                    system, core) >= 0
            ) {
                close_dispatch(index, RUN_LIMIT);
                release_unused_credit(index);
                done[index] = true;
                outcome.interrupt_boundary = true;
                outcome.interrupt_cores.push_back(
                    core_index);
                continue;
            }

            if (!dispatch_open[index]) {
                checked_scheduler_increment(
                    system.native_dispatches,
                    "dispatch counter");
                dispatch_open[index] = true;
            }
            participating_reservations.push_back(
                index);
        }

        if (participating_reservations.empty())
            break;

        std::vector<CoreFrontierReservation>
            subfrontier_reservations;
        subfrontier_reservations.reserve(
            participating_reservations.size());
        const bool multi_participant_mixed_frontier =
            participating_reservations.size() > 1 &&
            system.execution_cores.size() !=
                system.cores.size();
        for (
            std::size_t reservation_index :
            participating_reservations
        ) {
            int64_t subfrontier_steps =
                remaining_steps[
                    reservation_index];
            if (multi_participant_mixed_frontier) {
                // Mixed topologies retain their versioned synchronous
                // frontier whenever more than one logical participant is
                // present. Keep every peer at one instruction so no private
                // prefix can cross an ordered write, callback, or interrupt
                // boundary or change exception-visible callback order.
                //
                // With one executable/coordinator participant, no other
                // reservation can act in this subfrontier. It may consume
                // the existing remaining credit because every instruction
                // is still classified as private immediately before
                // mutation and its command stops at the first uncertain
                // boundary.
                subfrontier_steps =
                    std::min<int64_t>(
                        subfrontier_steps, 1);
            }
            subfrontier_reservations.push_back(
                CoreFrontierReservation{
                    reservations[
                        reservation_index].core_index,
                    subfrontier_steps,
                });
        }

        SystemBatchResult subfrontier_result =
            initialize_subfrontier_result();
        CoreFrontierOutcome
            subfrontier_outcome;
        auto contains_core = [](
                const std::vector<int>& values,
                int core_index) {
            return std::find(
                values.begin(),
                values.end(),
                core_index) != values.end();
        };
        bool absorbed = false;
        auto absorb_subfrontier = [&](
                bool execution_stopped) {
            if (absorbed)
                return;
            if (host_profile_enabled) {
                host_saturating_increment(
                    profile.round_absorptions);
            }
            HostProfileWallTimer absorption_timer(
                host_profile_enabled,
                &profile.round_absorption_ns);

            std::vector<int64_t> next_remaining_steps =
                remaining_steps;
            std::vector<int64_t> next_round_credit =
                round_credit;
            std::vector<int64_t>
                next_per_reservation_cycles =
                    per_reservation_cycles;
            std::vector<bool> next_reservation_progress =
                reservation_progress;
            SystemBatchResult next_result = result;
            CoreFrontierOutcome next_outcome =
                outcome;
            int64_t absorbed_steps = 0;
            int64_t absorbed_max_cycles = 0;

            for (
                const FrontierCreditTransfer& transfer :
                subfrontier_outcome
                    .cluster_credit_transfers
            ) {
                const auto donor =
                    std::find_if(
                        reservations.begin(),
                        reservations.end(),
                        [&](const CoreFrontierReservation&
                                reservation) {
                            return reservation.core_index ==
                                transfer.donor_core;
                        });
                const auto recipient =
                    std::find_if(
                        reservations.begin(),
                        reservations.end(),
                        [&](const CoreFrontierReservation&
                                reservation) {
                            return reservation.core_index ==
                                transfer.recipient_core;
                        });
                if (
                    donor == reservations.end() ||
                    recipient == reservations.end() ||
                    transfer.amount <= 0
                ) {
                    throw std::logic_error(
                        "cluster frontier published an "
                        "invalid credit transfer");
                }
                const std::size_t donor_index =
                    static_cast<std::size_t>(
                        donor -
                        reservations.begin());
                const std::size_t recipient_index =
                    static_cast<std::size_t>(
                        recipient -
                        reservations.begin());
                if (
                    donor_index >= recipient_index ||
                    next_remaining_steps[
                        donor_index] <
                        transfer.amount ||
                    next_round_credit[
                        donor_index] <
                        transfer.amount ||
                    next_round_credit[
                        recipient_index] >
                        round_quantum -
                            transfer.amount
                ) {
                    throw std::logic_error(
                        "cluster forward-credit transfer "
                        "violated cyclic round credit");
                }
                next_remaining_steps[donor_index] -=
                    transfer.amount;
                next_round_credit[donor_index] -=
                    transfer.amount;
                next_remaining_steps[
                    recipient_index] =
                        checked_scheduler_add(
                            next_remaining_steps[
                                recipient_index],
                            transfer.amount,
                            "cluster forward-credit "
                            "accounting");
                next_round_credit[
                    recipient_index] =
                        checked_scheduler_add(
                            next_round_credit[
                                recipient_index],
                            transfer.amount,
                            "cluster forward-credit "
                            "accounting");
            }

            for (
                std::size_t reservation_index :
                participating_reservations
            ) {
                const int core_index =
                    reservations[
                        reservation_index].core_index;
                const std::size_t core_offset =
                    static_cast<std::size_t>(
                        core_index);
                const int64_t steps =
                    subfrontier_result
                        .per_core_instructions[
                            core_offset];
                const int64_t cycles =
                    subfrontier_result
                        .per_core_cycles[
                            core_offset];
                if (
                    steps < 0 ||
                    steps >
                        next_remaining_steps[
                            reservation_index] ||
                    cycles < 0
                ) {
                    throw std::logic_error(
                        "parallel core "
                        "subfrontier returned invalid "
                        "round progress");
                }

                next_remaining_steps[
                    reservation_index] -= steps;
                next_per_reservation_cycles[
                    reservation_index] =
                        checked_scheduler_add(
                            next_per_reservation_cycles[
                                reservation_index],
                            cycles,
                            "cycle accounting");
                const bool terminal =
                    contains_core(
                        subfrontier_outcome
                            .terminal_cores,
                        core_index);
                const bool interrupted =
                    contains_core(
                        subfrontier_outcome
                            .interrupt_cores,
                        core_index);
                if (
                    next_remaining_steps[
                        reservation_index] > 0 &&
                    !execution_stopped &&
                    !terminal &&
                    !interrupted &&
                    next_per_reservation_cycles[
                        reservation_index] ==
                        std::numeric_limits<int64_t>::max()
                ) {
                    throw std::overflow_error(
                        "native scheduler cycle "
                        "accounting overflow");
                }
                next_outcome.steps =
                    checked_scheduler_add(
                        next_outcome.steps,
                        steps,
                        "round aggregate instruction "
                        "accounting");
                if (next_outcome.steps > total_reserved) {
                    throw std::logic_error(
                        "parallel core round "
                        "exceeded its reservation");
                }
                next_outcome.cycles = std::max(
                    next_outcome.cycles,
                    next_per_reservation_cycles[
                        reservation_index]);
                absorbed_steps =
                    checked_scheduler_add(
                        absorbed_steps,
                        steps,
                        "subfrontier instruction "
                        "accounting");
                absorbed_max_cycles = std::max(
                    absorbed_max_cycles,
                    cycles);

                CoreDispatchResult progress;
                progress.steps = steps;
                progress.cycles = cycles;
                merge_core_dispatch(
                    next_result,
                    core_index,
                    std::move(progress));
                if (steps > 0) {
                    next_reservation_progress[
                        reservation_index] = true;
                }
            }

            if (
                absorbed_steps !=
                    subfrontier_outcome.steps ||
                absorbed_max_cycles !=
                    subfrontier_outcome.cycles
            ) {
                throw std::logic_error(
                    "parallel core subfrontier "
                    "aggregate accounting mismatch");
            }
            if (
                subfrontier_result.continuations >
                    std::numeric_limits<uint64_t>::max() -
                        next_result.continuations
            ) {
                throw std::overflow_error(
                    "native scheduler continuation "
                    "counter overflow");
            }
            next_result.continuations +=
                subfrontier_result.continuations;
            if (subfrontier_outcome.interrupt_boundary) {
                next_outcome.interrupt_boundary = true;
                next_outcome.interrupt_cores.insert(
                    next_outcome.interrupt_cores.end(),
                    subfrontier_outcome
                        .interrupt_cores.begin(),
                    subfrontier_outcome
                        .interrupt_cores.end());
            }

            remaining_steps =
                std::move(next_remaining_steps);
            round_credit =
                std::move(next_round_credit);
            per_reservation_cycles =
                std::move(
                    next_per_reservation_cycles);
            reservation_progress =
                std::move(
                    next_reservation_progress);
            result = std::move(next_result);
            outcome = std::move(next_outcome);
            absorbed = true;
            refresh_round_cursor();
        };

        try {
            run_parallel_core_subfrontier(
                system,
                subfrontier_reservations,
                callbacks,
                settle_continuation,
                settle_dispatch_error,
                subfrontier_result,
                subfrontier_outcome);
        } catch (...) {
            absorb_subfrontier(
                /*execution_stopped=*/true);
            throw;
        }
        absorb_subfrontier(
            /*execution_stopped=*/false);

        for (
            std::size_t reservation_index :
            participating_reservations
        ) {
            const int core_index =
                reservations[
                    reservation_index].core_index;
            const std::size_t core_offset =
                static_cast<std::size_t>(
                    core_index);
            int public_stop_reason = -1;
            uint64_t public_stop_count = 0;
            for (
                std::size_t reason = 0;
                reason <
                    subfrontier_result
                        .per_core_stop_reasons[
                            core_offset].size();
                reason++
            ) {
                const uint64_t count =
                    subfrontier_result
                        .per_core_stop_reasons[
                            core_offset][reason];
                public_stop_count += count;
                if (count != 0) {
                    public_stop_reason =
                        static_cast<int>(reason);
                }
            }
            if (
                public_stop_count > 1 ||
                subfrontier_result
                    .per_core_dispatches[
                        core_offset] > 1
            ) {
                throw std::logic_error(
                    "parallel core subfrontier "
                    "closed one dispatch more than once");
            }

            const bool interrupted =
                contains_core(
                    subfrontier_outcome
                        .interrupt_cores,
                    core_index);
            const bool dispatch_boundary =
                contains_core(
                    subfrontier_outcome
                        .dispatch_boundary_cores,
                    core_index);
            const bool terminal =
                contains_core(
                    subfrontier_outcome
                        .terminal_cores,
                    core_index);
            const bool cluster_lost =
                contains_core(
                    subfrontier_outcome
                        .cluster_lost_cores,
                    core_index);
            const bool cluster_deferred =
                contains_core(
                    subfrontier_outcome
                        .cluster_deferred_cores,
                    core_index);

            if (interrupted) {
                close_dispatch(
                    reservation_index,
                    RUN_LIMIT);
                release_unused_credit(
                    reservation_index);
                done[reservation_index] = true;
                continue;
            }
            if (cluster_lost) {
                close_dispatch(
                    reservation_index,
                    RUN_EXT_FALLBACK);
                if (
                    subfrontier_outcome.steps == 0 &&
                    !subfrontier_outcome
                        .coordinator_state_changed
                ) {
                    // An unchanged all-zero request set cannot become
                    // eligible by immediate retry. End only this frozen
                    // round position and release its otherwise stranded
                    // credit forward.
                    release_unused_credit(
                        reservation_index);
                    done[reservation_index] = true;
                }
                continue;
            }
            if (dispatch_boundary) {
                close_dispatch(
                    reservation_index,
                    public_stop_reason);
            }
            if (terminal) {
                if (!dispatch_boundary) {
                    close_dispatch(
                        reservation_index,
                        public_stop_reason);
                }
                release_unused_credit(
                    reservation_index);
                done[reservation_index] = true;
                continue;
            }
            if (cluster_deferred) {
                if (
                    remaining_steps[
                        reservation_index] == 0 &&
                    !earlier_reservation_unfinished(
                        reservation_index)
                ) {
                    close_dispatch(
                        reservation_index,
                        RUN_LIMIT);
                    done[reservation_index] = true;
                }
                continue;
            }
            if (
                remaining_steps[
                    reservation_index] == 0
            ) {
                if (
                    earlier_reservation_unfinished(
                        reservation_index)
                ) {
                    continue;
                }
                if (!dispatch_boundary) {
                    close_dispatch(
                        reservation_index,
                        RUN_LIMIT);
                }
                done[reservation_index] = true;
                continue;
            }
            if (
                !dispatch_boundary &&
                public_stop_reason != RUN_LIMIT
            ) {
                throw std::logic_error(
                    "parallel core subfrontier "
                    "left an unexplained open dispatch");
            }
        }
    }

    for (
        std::size_t index = 0;
        index < reservations.size();
        index++
    ) {
        if (dispatch_open[index]) {
            throw std::logic_error(
                "parallel core round returned "
                "with an open dispatch");
        }
    }
}

static const char* private_full_core_stop_reason_name(
        PrivateCoreStopReason reason) {
    switch (reason) {
        case PrivateCoreStopReason::INSTRUCTION_LIMIT:
            return "instruction_limit";
        case PrivateCoreStopReason::ICACHE_BOUNDARY:
            return "icache_boundary";
        case PrivateCoreStopReason::SHARED_INSTRUCTION:
            return "shared_instruction";
        case PrivateCoreStopReason::INTERRUPT_BOUNDARY:
            return "interrupt_boundary";
        case PrivateCoreStopReason::HALTED:
            return "halted";
        case PrivateCoreStopReason::IDLE:
            return "idle";
        case PrivateCoreStopReason::TRAP:
            return "trap";
        case PrivateCoreStopReason::RESET:
            return "reset";
        case PrivateCoreStopReason::INTERNAL_FAILURE:
            return "internal_failure";
    }
    return "unknown";
}

static py::dict concurrency_profile_snapshot_dict(
        const SystemState& system) {
    const ConcurrencyProfileCounters& profile =
        system.concurrency_profile;
    py::dict private_stop_reasons;
    py::dict worker_bypass_stop_reasons;
    py::dict coordinator_boundary_origins;
    py::dict coordinator_boundary_origin_ns;
    for (
        std::size_t index = 0;
        index < PRIVATE_CORE_STOP_REASON_COUNT;
        index++
    ) {
        const char* name =
            private_full_core_stop_reason_name(
                static_cast<PrivateCoreStopReason>(
                    index));
        private_stop_reasons[name] =
            profile.private_stop_reasons[index];
        worker_bypass_stop_reasons[name] =
            profile.worker_bypass_stop_reasons[
                index];
        coordinator_boundary_origins[name] =
            profile.coordinator_boundary_origins[
                index];
        coordinator_boundary_origin_ns[name] =
            profile.coordinator_boundary_origin_ns[
                index];
    }

    py::list lane_commands;
    py::list lane_steps;
    py::list lane_active_ns;
    for (
        int lane = 0;
        lane < system.configured_worker_count;
        lane++
    ) {
        const std::size_t index =
            static_cast<std::size_t>(lane);
        lane_commands.append(
            profile.lane_commands[index]);
        lane_steps.append(
            profile.lane_steps[index]);
        lane_active_ns.append(
            profile.lane_active_ns[index]);
    }

    py::dict counts;
    counts["batches"] = profile.batches;
    counts["prepare_batch_calls"] =
        profile.prepare_batch_calls;
    counts["scheduler_rounds"] =
        profile.scheduler_rounds;
    counts["logical_subfrontiers"] =
        profile.logical_subfrontiers;
    counts["round_absorptions"] =
        profile.round_absorptions;
    counts["worker_waves"] =
        profile.worker_waves;
    counts["worker_commands"] =
        profile.worker_commands;
    counts["frontier_routing_waves"] =
        profile.frontier_routing_waves;
    counts["frontier_routing_commands"] =
        profile.frontier_routing_commands;
    counts["frontier_preclassification_commands"] =
        profile.frontier_preclassification_commands;
    counts["frontier_preclassification_calls"] =
        profile.frontier_preclassification_calls;
    counts["worker_bypassed_commands"] =
        profile.worker_bypassed_commands;
    counts["private_steps"] =
        profile.private_steps;
    counts["private_classification_calls"] =
        profile.private_classification_calls;
    counts["private_decode_cache_lookups"] =
        profile.private_decode_cache_lookups;
    counts["private_decode_cache_hits"] =
        profile.private_decode_cache_hits;
    counts["private_decode_cache_misses"] =
        profile.private_decode_cache_misses;
    counts["micro_oracle_proof_reuses"] =
        profile.micro_oracle_proof_reuses;
    counts["frontier_decode_cache_lookups"] =
        profile.frontier_decode_cache_lookups;
    counts["frontier_decode_cache_hits"] =
        profile.frontier_decode_cache_hits;
    counts["frontier_decode_cache_misses"] =
        profile.frontier_decode_cache_misses;
    counts["zero_step_commands"] =
        profile.zero_step_commands;
    counts["checkpoint_captures"] =
        profile.checkpoint_captures;
    counts["checkpoint_restores"] =
        profile.checkpoint_restores;
    counts["coordinator_boundaries"] =
        profile.coordinator_boundaries;
    counts["settle_round_calls"] =
        profile.settle_round_calls;
    counts["private_stop_reasons"] =
        std::move(private_stop_reasons);
    counts["worker_bypass_stop_reasons"] =
        std::move(
            worker_bypass_stop_reasons);
    counts["coordinator_boundary_origins"] =
        std::move(
            coordinator_boundary_origins);
    counts["lane_commands"] =
        std::move(lane_commands);
    counts["lane_steps"] =
        std::move(lane_steps);

    py::dict wall_ns;
    wall_ns["batch_total"] =
        profile.batch_total_ns;
    wall_ns["prepare_batch"] =
        profile.prepare_batch_ns;
    wall_ns["scheduler_round"] =
        profile.scheduler_round_ns;
    wall_ns["logical_subfrontier"] =
        profile.logical_subfrontier_ns;
    wall_ns["round_absorption"] =
        profile.round_absorption_ns;
    wall_ns["worker_wave"] =
        profile.worker_wave_ns;
    wall_ns["worker_wave_prepare"] =
        profile.worker_wave_prepare_ns;
    wall_ns["worker_wave_wait"] =
        profile.worker_wave_wait_ns;
    wall_ns["worker_wave_gather"] =
        profile.worker_wave_gather_ns;
    wall_ns["frontier_fast_path"] =
        profile.frontier_fast_path_ns;
    wall_ns["private_command_sum"] =
        profile.private_command_sum_ns;
    wall_ns["private_command_max"] =
        profile.private_command_max_ns;
    wall_ns["private_scope_setup"] =
        profile.private_scope_setup_ns;
    wall_ns["checkpoint_capture"] =
        profile.checkpoint_capture_ns;
    wall_ns["checkpoint_restore"] =
        profile.checkpoint_restore_ns;
    wall_ns["coordinator_boundary"] =
        profile.coordinator_boundary_ns;
    wall_ns["settle_round"] =
        profile.settle_round_ns;
    wall_ns["coordinator_boundary_origins"] =
        std::move(
            coordinator_boundary_origin_ns);

    py::dict result;
    result["schema_version"] = 3;
    result["enabled"] = profile.enabled;
    result["generation"] = profile.generation;
    result["architectural_hash_scope"] =
        "excluded_host_only";
    result["measurement_scope"] =
        "unbounded_native_system_batch_only";
    result["timing_semantics"] =
        "inclusive_nested_host_wall_nanoseconds";
    result["counts"] = std::move(counts);
    result["wall_ns"] = std::move(wall_ns);
    result["lane_active_ns"] =
        std::move(lane_active_ns);
    return result;
}

static ClusterState& checked_cluster_state(
        SystemState& system,
        int cluster_index) {
    if (
        cluster_index < 0 ||
        cluster_index >= static_cast<int>(
            system.cluster_states.size())
    ) {
        throw std::out_of_range(
            "cluster state index is out of range");
    }
    return system.cluster_states[
        static_cast<std::size_t>(cluster_index)];
}

struct PreparedTaccState {
    std::array<uint8_t, TACC_IMAGE_BYTES> image{};
    uint8_t owner = TACC_OWNER_NONE;
    bool valid = false;
    bool dirty = false;
    uint8_t format_ew = 0;
    uint8_t format_signed = 0;
    bool busy = false;
    bool force_pending = false;
    uint64_t epoch = 0;
};

static constexpr std::array<const char*, 9>
    TACC_SNAPSHOT_FIELDS{{
        "tacc",
        "tacc_owner",
        "tacc_valid",
        "tacc_dirty",
        "tacc_format_ew",
        "tacc_format_signed",
        "tacc_busy",
        "tacc_force_pending",
        "tacc_epoch",
    }};

static constexpr std::array<const char*, 13>
    CLUSTER_TILE_SNAPSHOT_FIELDS{{
        "acc",
        "tacc",
        "tacc_owner",
        "tacc_valid",
        "tacc_dirty",
        "tacc_format_ew",
        "tacc_format_signed",
        "tacc_busy",
        "tacc_force_pending",
        "tacc_epoch",
        "sha_mode",
        "sha_msglen_lo",
        "sha_msglen_hi",
    }};

template <std::size_t N>
static void validate_exact_snapshot_schema(
        const py::dict& state,
        const std::array<const char*, N>& expected,
        const char* snapshot_name) {
    if (static_cast<std::size_t>(state.size()) != N) {
        throw std::invalid_argument(
            std::string(snapshot_name) +
            " snapshot must contain exactly " +
            std::to_string(N) +
            " fields");
    }
    for (const char* field : expected) {
        if (!state.contains(py::str(field))) {
            throw std::invalid_argument(
                std::string(snapshot_name) +
                " snapshot is missing field " +
                field);
        }
    }
}

static py::handle snapshot_field(
        const py::dict& state,
        const char* field) {
    return state[py::str(field)];
}

static bool snapshot_bool(
        const py::dict& state,
        const char* field) {
    const py::handle value = snapshot_field(state, field);
    if (!PyBool_Check(value.ptr())) {
        throw std::invalid_argument(
            std::string(field) +
            " must be a bool");
    }
    return value.cast<bool>();
}

static int snapshot_int(
        const py::dict& state,
        const char* field) {
    const py::handle value = snapshot_field(state, field);
    if (
        PyBool_Check(value.ptr()) ||
        !PyLong_Check(value.ptr())
    ) {
        throw std::invalid_argument(
            std::string(field) +
            " must be an integer");
    }
    return value.cast<int>();
}

static uint64_t snapshot_u64(
        const py::dict& state,
        const char* field) {
    const py::handle value = snapshot_field(state, field);
    if (
        PyBool_Check(value.ptr()) ||
        !PyLong_Check(value.ptr())
    ) {
        throw std::invalid_argument(
            std::string(field) +
            " must be an unsigned 64-bit integer");
    }
    return value.cast<uint64_t>();
}

static constexpr std::array<const char*, 16>
    TACC_IMAGE_STAGE_SNAPSHOT_FIELDS{{
        "schema_version",
        "engine_count",
        "active",
        "direction",
        "owner_engine_id",
        "owner_core_id",
        "engine_epoch",
        "caller_epoch",
        "stage_epoch",
        "base_address",
        "format_ew",
        "format_signed",
        "beat_index",
        "image",
        "last_grant_engine_id",
        "grant_sequence",
    }};

struct PreparedTaccImageTransferStage {
    TaccImageTransferStage::Direction direction =
        TaccImageTransferStage::Direction::NONE;
    int owner_engine_id =
        TaccImageTransferStage::NO_OWNER;
    int owner_core_id =
        TaccImageTransferStage::NO_OWNER;
    uint64_t engine_epoch = 0;
    uint64_t caller_epoch = 0;
    uint64_t stage_epoch = 0;
    uint64_t base_address = 0;
    uint8_t format_ew = 0;
    bool format_signed = false;
    uint8_t beat_index = 0;
    std::array<uint8_t, TACC_IMAGE_BYTES> image{};
    int last_grant_engine_id =
        TaccImageTransferStage::NO_OWNER;
    uint64_t grant_sequence = 0;
};

static TaccImageTransferStage::Direction
tacc_image_stage_direction_from_snapshot(
        const py::dict& state) {
    const py::handle value =
        snapshot_field(state, "direction");
    if (!py::isinstance<py::str>(value)) {
        throw std::invalid_argument(
            "TACC image-stage direction must be a string");
    }
    const std::string direction =
        value.cast<std::string>();
    if (direction == "none")
        return TaccImageTransferStage::Direction::NONE;
    if (direction == "load")
        return TaccImageTransferStage::Direction::LOAD;
    if (direction == "store")
        return TaccImageTransferStage::Direction::STORE;
    throw std::invalid_argument(
        "TACC image-stage direction must be none, load, or store");
}

static int optional_snapshot_int(
        const py::dict& state,
        const char* field) {
    const py::handle value = snapshot_field(state, field);
    if (value.is_none())
        return TaccImageTransferStage::NO_OWNER;
    return snapshot_int(state, field);
}

static PreparedTaccImageTransferStage
prepare_tacc_image_transfer_stage(
        SystemState& system,
        const py::dict& state) {
    validate_exact_snapshot_schema(
        state,
        TACC_IMAGE_STAGE_SNAPSHOT_FIELDS,
        "TACC image-transfer stage");
    PreparedTaccImageTransferStage prepared;

    if (snapshot_int(state, "schema_version") != 1) {
        throw std::invalid_argument(
            "TACC image-stage snapshot schema version must be one");
    }
    const int engine_count =
        system.full_core_count() +
        static_cast<int>(system.cluster_states.size());
    if (snapshot_int(state, "engine_count") != engine_count) {
        throw std::invalid_argument(
            "TACC image-stage engine count does not match "
            "the system topology");
    }
    const bool active =
        snapshot_bool(state, "active");
    prepared.direction =
        tacc_image_stage_direction_from_snapshot(state);
    prepared.owner_engine_id =
        optional_snapshot_int(state, "owner_engine_id");
    prepared.owner_core_id =
        optional_snapshot_int(state, "owner_core_id");
    prepared.engine_epoch =
        snapshot_u64(state, "engine_epoch");
    prepared.caller_epoch =
        snapshot_u64(state, "caller_epoch");
    prepared.stage_epoch =
        snapshot_u64(state, "stage_epoch");
    prepared.base_address =
        snapshot_u64(state, "base_address");

    const py::handle image_value =
        snapshot_field(state, "image");
    if (!py::isinstance<py::bytes>(image_value)) {
        throw std::invalid_argument(
            "TACC image-stage image must be bytes");
    }
    const std::string image =
        py::reinterpret_borrow<py::bytes>(
            image_value);
    if (image.size() != prepared.image.size()) {
        throw std::invalid_argument(
            "TACC image-stage image must be "
            "exactly 256 bytes");
    }
    std::copy(
        image.begin(),
        image.end(),
        prepared.image.begin());

    const int format_ew =
        snapshot_int(state, "format_ew");
    if (format_ew < 0 || format_ew > 7) {
        throw std::invalid_argument(
            "TACC image-stage format EW must fit three bits");
    }
    prepared.format_ew =
        static_cast<uint8_t>(format_ew);
    prepared.format_signed =
        snapshot_bool(state, "format_signed");
    const int beat_index =
        snapshot_int(state, "beat_index");
    if (beat_index < 0 || beat_index > 4) {
        throw std::invalid_argument(
            "TACC image-stage beat index must be between "
            "zero and four");
    }
    prepared.beat_index =
        static_cast<uint8_t>(beat_index);
    prepared.last_grant_engine_id =
        optional_snapshot_int(
            state,
            "last_grant_engine_id");
    if (
        prepared.last_grant_engine_id <
            TaccImageTransferStage::NO_OWNER ||
        prepared.last_grant_engine_id >= engine_count
    ) {
        throw std::invalid_argument(
            "TACC image-stage last grant is outside "
            "the physical engine topology");
    }
    prepared.grant_sequence =
        snapshot_u64(state, "grant_sequence");
    if (
        (
            prepared.last_grant_engine_id ==
            TaccImageTransferStage::NO_OWNER
        ) !=
        (prepared.grant_sequence == 0)
    ) {
        throw std::invalid_argument(
            "TACC image-stage RR cursor and grant sequence "
            "must either both be empty or both be established");
    }

    if (!active) {
        if (
            prepared.direction !=
                TaccImageTransferStage::Direction::NONE ||
            prepared.owner_engine_id !=
                TaccImageTransferStage::NO_OWNER ||
            prepared.owner_core_id !=
                TaccImageTransferStage::NO_OWNER ||
            prepared.engine_epoch != 0 ||
            prepared.caller_epoch != 0 ||
            prepared.base_address != 0 ||
            prepared.format_ew != 0 ||
            prepared.format_signed ||
            prepared.beat_index != 0 ||
            std::any_of(
                prepared.image.begin(),
                prepared.image.end(),
                [](uint8_t byte) {
                    return byte != 0;
                })
        ) {
            throw std::invalid_argument(
                "inactive TACC image stage must clear its "
                "direction, owners, transfer fields, and image");
        }
        if (system.tacc_image_stage.active()) {
            throw std::invalid_argument(
                "inactive TACC image-stage restore cannot "
                "clear a live transfer tenure");
        }
        return prepared;
    }

    if (
        prepared.direction ==
            TaccImageTransferStage::Direction::NONE ||
        prepared.owner_engine_id < 0 ||
        prepared.owner_engine_id >= engine_count ||
        prepared.owner_core_id < 0 ||
        prepared.owner_core_id >= TACC_OWNER_NONE
    ) {
        throw std::invalid_argument(
            "active TACC image stage requires valid physical "
            "and absolute owners");
    }
    const int mapped_engine =
        system.tacc_engine_for_core(
            prepared.owner_core_id);
    if (mapped_engine != prepared.owner_engine_id) {
        throw std::invalid_argument(
            "TACC image-stage core owner does not map to "
            "its physical engine owner");
    }
    if (
        prepared.last_grant_engine_id !=
        prepared.owner_engine_id
    ) {
        throw std::invalid_argument(
            "active TACC image stage must match the latest "
            "physical-engine grant");
    }
    if ((prepared.base_address & 0x3F) != 0) {
        throw std::invalid_argument(
            "TACC image-stage base address must be "
            "64-byte aligned");
    }
    if (
        prepared.base_address >
        std::numeric_limits<uint64_t>::max() -
            (TACC_IMAGE_BYTES - 1)
    ) {
        throw std::invalid_argument(
            "TACC image-stage span wraps the address space");
    }
    if (
        prepared.format_ew != EW_U8 &&
        prepared.format_ew != EW_U16 &&
        prepared.format_ew != EW_U32 &&
        prepared.format_ew != EW_FP16 &&
        prepared.format_ew != EW_BF16
    ) {
        throw std::invalid_argument(
            "active TACC image stage requires a legal format");
    }
    if (
        (
            prepared.format_ew == EW_FP16 ||
            prepared.format_ew == EW_BF16
        ) &&
        prepared.format_signed
    ) {
        throw std::invalid_argument(
            "floating TACC image stage cannot be signed");
    }

    const TaccImageTransferStage& current =
        system.tacc_image_stage;
    if (
        !current.active() ||
        current.owner_engine_id !=
            prepared.owner_engine_id ||
        current.owner_core_id !=
            prepared.owner_core_id ||
        current.stage_epoch !=
            prepared.stage_epoch
    ) {
        throw std::invalid_argument(
            "active TACC image-stage restore has no matching "
            "live transfer tenure");
    }
    if (
        prepared.direction != current.direction ||
        prepared.base_address != current.base_address ||
        prepared.format_ew != current.format_ew ||
        prepared.format_signed != current.format_signed ||
        prepared.engine_epoch != current.engine_epoch ||
        prepared.caller_epoch != current.caller_epoch ||
        prepared.last_grant_engine_id !=
            current.last_grant_engine_id ||
        prepared.grant_sequence !=
            current.grant_sequence
    ) {
        throw std::invalid_argument(
            "active TACC image-stage restore cannot change "
            "immutable tenure or round-robin fields");
    }
    if (prepared.beat_index > current.beat_index) {
        throw std::invalid_argument(
            "active TACC image-stage restore cannot fabricate "
            "future beat acknowledgements");
    }
    if (
        prepared.direction ==
            TaccImageTransferStage::Direction::STORE &&
        prepared.image != current.image
    ) {
        throw std::invalid_argument(
            "active TACC STORE restore cannot rewrite its "
            "captured image");
    }
    const std::size_t acknowledged_bytes =
        static_cast<std::size_t>(
            prepared.beat_index) * 64;
    if (
        prepared.direction ==
            TaccImageTransferStage::Direction::LOAD &&
        !std::equal(
            prepared.image.begin(),
            prepared.image.begin() +
                static_cast<std::ptrdiff_t>(
                    acknowledged_bytes),
            current.image.begin())
    ) {
        throw std::invalid_argument(
            "active TACC LOAD rollback cannot rewrite an "
            "acknowledged prefix");
    }
    const uint64_t live_engine_epoch =
        prepared.owner_engine_id <
            system.full_core_count()
        ? system.cores[
              static_cast<std::size_t>(
                  prepared.owner_engine_id)]
              ->tacc_epoch
        : system.cluster_states[
              static_cast<std::size_t>(
                  prepared.owner_engine_id -
                  system.full_core_count())]
              .tacc_epoch;
    if (prepared.engine_epoch != live_engine_epoch) {
        throw std::invalid_argument(
            "TACC image-stage engine epoch is stale");
    }
    const bool engine_busy =
        prepared.owner_engine_id <
            system.full_core_count()
        ? system.cores[
              static_cast<std::size_t>(
                  prepared.owner_engine_id)]
              ->tacc_busy
        : system.cluster_states[
              static_cast<std::size_t>(
                  prepared.owner_engine_id -
                  system.full_core_count())]
              .tacc_busy;
    const uint8_t engine_owner =
        prepared.owner_engine_id <
            system.full_core_count()
        ? system.cores[
              static_cast<std::size_t>(
                  prepared.owner_engine_id)]
              ->tacc_owner
        : system.cluster_states[
              static_cast<std::size_t>(
                  prepared.owner_engine_id -
                  system.full_core_count())]
              .tacc_owner;
    if (
        !engine_busy ||
        engine_owner != prepared.owner_core_id
    ) {
        throw std::invalid_argument(
            "active TACC image-stage restore requires the "
            "caller's BUSY owned engine");
    }
    if (
        prepared.owner_core_id <
            system.full_core_count() &&
        prepared.caller_epoch != 0
    ) {
        throw std::invalid_argument(
            "full-core TACC image stages use caller epoch zero");
    }
    if (prepared.owner_core_id >= system.full_core_count()) {
        ClusterState& cluster =
            system.cluster_states[
                static_cast<std::size_t>(
                    prepared.owner_engine_id -
                    system.full_core_count())];
        const int local_core =
            prepared.owner_core_id -
            cluster.global_id_base;
        if (
            local_core < 0 ||
            local_core >= cluster.core_count ||
            cluster.tacc_caller_epochs[
                static_cast<std::size_t>(
                    local_core)] !=
                prepared.caller_epoch
        ) {
            throw std::invalid_argument(
                "TACC image-stage caller epoch is stale");
        }
    }
    if (
        prepared.direction ==
            TaccImageTransferStage::Direction::LOAD &&
        prepared.beat_index < 4 &&
        std::any_of(
            prepared.image.begin() +
                static_cast<std::ptrdiff_t>(
                    prepared.beat_index * 64),
            prepared.image.end(),
            [](uint8_t byte) {
                return byte != 0;
            })
    ) {
        throw std::invalid_argument(
            "TACC LOAD stage has data beyond its acknowledged prefix");
    }
    if (
        prepared.direction ==
            TaccImageTransferStage::Direction::LOAD &&
        prepared.format_ew != EW_U8 &&
        prepared.format_ew != EW_U16 &&
        (
            std::any_of(
                prepared.image.begin() + 128,
                prepared.image.end(),
                [](uint8_t byte) {
                    return byte != 0;
                })
        )
    ) {
        throw std::invalid_argument(
            "inactive TACC LOAD image bytes must remain zero");
    }
    return prepared;
}

static py::dict snapshot_tacc_image_transfer_stage(
        const SystemState& system) {
    const TaccImageTransferStage& stage =
        system.tacc_image_stage;
    py::dict snapshot;
    snapshot["schema_version"] = 1;
    snapshot["engine_count"] =
        system.full_core_count() +
        static_cast<int>(
            system.cluster_states.size());
    snapshot["active"] = stage.active();
    switch (stage.direction) {
        case TaccImageTransferStage::Direction::NONE:
            snapshot["direction"] = "none";
            break;
        case TaccImageTransferStage::Direction::LOAD:
            snapshot["direction"] = "load";
            break;
        case TaccImageTransferStage::Direction::STORE:
            snapshot["direction"] = "store";
            break;
    }
    if (stage.owner_engine_id == TaccImageTransferStage::NO_OWNER) {
        snapshot["owner_engine_id"] = py::none();
    } else {
        snapshot["owner_engine_id"] =
            stage.owner_engine_id;
    }
    if (stage.owner_core_id == TaccImageTransferStage::NO_OWNER) {
        snapshot["owner_core_id"] = py::none();
    } else {
        snapshot["owner_core_id"] =
            stage.owner_core_id;
    }
    snapshot["engine_epoch"] = stage.engine_epoch;
    snapshot["caller_epoch"] = stage.caller_epoch;
    snapshot["stage_epoch"] = stage.stage_epoch;
    snapshot["base_address"] = stage.base_address;
    snapshot["format_ew"] = stage.format_ew;
    snapshot["format_signed"] = stage.format_signed;
    snapshot["beat_index"] = stage.beat_index;
    snapshot["image"] = py::bytes(
        reinterpret_cast<const char*>(
            stage.image.data()),
        stage.image.size());
    if (
        stage.last_grant_engine_id ==
        TaccImageTransferStage::NO_OWNER
    ) {
        snapshot["last_grant_engine_id"] =
            py::none();
    } else {
        snapshot["last_grant_engine_id"] =
            stage.last_grant_engine_id;
    }
    snapshot["grant_sequence"] = stage.grant_sequence;
    return snapshot;
}

static void commit_tacc_image_transfer_stage(
        TaccImageTransferStage& stage,
        const PreparedTaccImageTransferStage& prepared) noexcept {
    stage.direction = prepared.direction;
    stage.owner_engine_id = prepared.owner_engine_id;
    stage.owner_core_id = prepared.owner_core_id;
    stage.engine_epoch = prepared.engine_epoch;
    stage.caller_epoch = prepared.caller_epoch;
    stage.stage_epoch = prepared.stage_epoch;
    stage.base_address = prepared.base_address;
    stage.format_ew = prepared.format_ew;
    stage.format_signed = prepared.format_signed;
    stage.beat_index = prepared.beat_index;
    stage.image = prepared.image;
    stage.last_grant_engine_id =
        prepared.last_grant_engine_id;
    stage.grant_sequence = prepared.grant_sequence;
}

static bool tacc_image_stage_owner_is_live(
        const SystemState& system,
        const TaccImageTransferStage& stage) noexcept {
    if (!stage.active())
        return false;
    const int full_core_count = system.full_core_count();
    if (
        stage.owner_engine_id < 0 ||
        stage.owner_core_id < 0
    ) {
        return false;
    }
    if (stage.owner_engine_id < full_core_count) {
        if (
            stage.owner_core_id != stage.owner_engine_id ||
            stage.caller_epoch != 0
        ) {
            return false;
        }
        const CPUState& core =
            *system.cores[
                static_cast<std::size_t>(
                    stage.owner_engine_id)];
        return (
            core.tacc_epoch == stage.engine_epoch &&
            core.tacc_busy &&
            core.tacc_owner == stage.owner_core_id
        );
    }

    const int cluster_index =
        stage.owner_engine_id - full_core_count;
    if (
        cluster_index < 0 ||
        cluster_index >=
            static_cast<int>(
                system.cluster_states.size())
    ) {
        return false;
    }
    const ClusterState& cluster =
        system.cluster_states[
            static_cast<std::size_t>(
                cluster_index)];
    const int local_core =
        stage.owner_core_id - cluster.global_id_base;
    if (
        local_core < 0 ||
        local_core >= cluster.core_count
    ) {
        return false;
    }
    return (
        cluster.tacc_epoch == stage.engine_epoch &&
        cluster.tacc_caller_epochs[
            static_cast<std::size_t>(
                local_core)] == stage.caller_epoch &&
        cluster.tacc_busy &&
        cluster.tacc_owner == stage.owner_core_id
    );
}

static bool tacc_ew_is_legal(uint8_t ew) noexcept {
    return (
        ew == EW_U8 ||
        ew == EW_U16 ||
        ew == EW_U32 ||
        ew == EW_FP16 ||
        ew == EW_BF16
    );
}

static PreparedTaccState prepare_tacc_state(
        const py::dict& state) {
    PreparedTaccState prepared;
    const py::handle image_value =
        snapshot_field(state, "tacc");
    if (!py::isinstance<py::bytes>(image_value)) {
        throw std::invalid_argument(
            "TACC image must be bytes");
    }
    const std::string image =
        py::reinterpret_borrow<py::bytes>(
            image_value);
    if (image.size() != prepared.image.size()) {
        throw std::invalid_argument(
            "TACC image must be exactly 256 bytes");
    }
    std::copy(
        image.begin(),
        image.end(),
        prepared.image.begin());

    const int owner =
        snapshot_int(state, "tacc_owner");
    const int format_ew =
        snapshot_int(state, "tacc_format_ew");
    const int format_signed =
        snapshot_int(state, "tacc_format_signed");
    if (owner < 0 || owner > TACC_OWNER_NONE) {
        throw std::invalid_argument(
            "TACC owner must fit the absolute five-bit owner field");
    }
    if (format_ew < 0 || format_ew > 7) {
        throw std::invalid_argument(
            "TACC format EW must fit its three-bit field");
    }
    if (format_signed < 0 || format_signed > 1) {
        throw std::invalid_argument(
            "TACC format signedness must be zero or one");
    }

    prepared.owner = static_cast<uint8_t>(owner);
    prepared.valid = snapshot_bool(state, "tacc_valid");
    prepared.dirty = snapshot_bool(state, "tacc_dirty");
    prepared.format_ew =
        static_cast<uint8_t>(format_ew);
    prepared.format_signed =
        static_cast<uint8_t>(format_signed);
    prepared.busy = snapshot_bool(state, "tacc_busy");
    prepared.force_pending =
        snapshot_bool(state, "tacc_force_pending");
    prepared.epoch =
        snapshot_u64(state, "tacc_epoch");

    if (prepared.owner == TACC_OWNER_NONE) {
        if (
            prepared.valid ||
            prepared.dirty ||
            prepared.format_ew != 0 ||
            prepared.format_signed != 0
        ) {
            throw std::invalid_argument(
                "unowned TACC state must be invalid, clean, "
                "and unformatted");
        }
    } else {
        if (
            !prepared.valid &&
            (
                prepared.dirty ||
                prepared.format_ew != 0 ||
                prepared.format_signed != 0
            )
        ) {
            throw std::invalid_argument(
                "invalid TACC state cannot be dirty or formatted");
        }
        if (
            prepared.valid &&
            !tacc_ew_is_legal(prepared.format_ew)
        ) {
            throw std::invalid_argument(
                "valid TACC state requires a legal element width");
        }
        if (
            prepared.valid &&
            (
                prepared.format_ew == EW_FP16 ||
                prepared.format_ew == EW_BF16
            ) &&
            prepared.format_signed != 0
        ) {
            throw std::invalid_argument(
                "floating TACC state cannot set integer signedness");
        }
    }
    const bool image_is_zero =
        std::all_of(
            prepared.image.begin(),
            prepared.image.end(),
            [](uint8_t byte) {
                return byte == 0;
            });
    if (!prepared.valid && !image_is_zero) {
        throw std::invalid_argument(
            "invalid TACC state must have a zero image");
    }
    if (
        prepared.valid &&
        prepared.format_ew != EW_U8 &&
        prepared.format_ew != EW_U16 &&
        std::any_of(
            prepared.image.begin() + 128,
            prepared.image.end(),
            [](uint8_t byte) {
                return byte != 0;
            })
    ) {
        throw std::invalid_argument(
            "inactive TACC image bytes must be zero");
    }
    if (
        prepared.force_pending &&
        !prepared.busy
    ) {
        throw std::invalid_argument(
            "TACC force-pending state requires an active operation");
    }
    return prepared;
}

template <typename State>
static py::dict snapshot_tacc_state(
        const State& state) {
    py::dict snapshot;
    snapshot["tacc"] = py::bytes(
        reinterpret_cast<const char*>(
            state.tacc.data()),
        state.tacc.size());
    snapshot["tacc_owner"] = state.tacc_owner;
    snapshot["tacc_valid"] = state.tacc_valid;
    snapshot["tacc_dirty"] = state.tacc_dirty;
    snapshot["tacc_format_ew"] =
        state.tacc_format_ew;
    snapshot["tacc_format_signed"] =
        state.tacc_format_signed;
    snapshot["tacc_busy"] = state.tacc_busy;
    snapshot["tacc_force_pending"] =
        state.tacc_force_pending;
    snapshot["tacc_epoch"] = state.tacc_epoch;
    return snapshot;
}

template <typename State>
static void commit_tacc_state(
        State& state,
        const PreparedTaccState& prepared) noexcept {
    state.tacc = prepared.image;
    state.tacc_owner = prepared.owner;
    state.tacc_valid = prepared.valid;
    state.tacc_dirty = prepared.dirty;
    state.tacc_format_ew = prepared.format_ew;
    state.tacc_format_signed =
        prepared.format_signed;
    state.tacc_busy = prepared.busy;
    state.tacc_force_pending =
        prepared.force_pending;
    state.tacc_epoch = prepared.epoch;
}

static std::vector<StepCallbacks> build_system_step_callbacks(
        const SystemState& system,
        const py::list& callback_sets,
        std::size_t expected_core_count,
        const char* topology_name) {
    (void)system;
    if (
        static_cast<std::size_t>(callback_sets.size()) !=
        expected_core_count
    ) {
        throw std::invalid_argument(
            std::string("one callback set is required for every ") +
            topology_name + " core");
    }

    std::vector<StepCallbacks> callbacks;
    callbacks.reserve(expected_core_count);
    for (py::handle item : callback_sets) {
        py::tuple callback_set =
            py::cast<py::tuple>(item);
        if (callback_set.size() != 4) {
            throw std::invalid_argument(
                "each callback set must contain MMIO read, "
                "MMIO write, output, and CSR override entries");
        }
        py::function mmio_read8 =
            callback_set[0].cast<py::function>();
        py::function mmio_write8 =
            callback_set[1].cast<py::function>();
        py::function on_output =
            callback_set[2].cast<py::function>();
        py::object csr_read_override = callback_set[3];

        StepCallbacks core_callbacks;
        core_callbacks.mmio_start =
            0xFFFFFF0000000000ULL;
        core_callbacks.mmio_end =
            0xFFFFFF8000000000ULL;
        core_callbacks.has_mmio = true;
        core_callbacks.mmio_read8 =
            [mmio_read8](uint64_t address) -> uint8_t {
                py::gil_scoped_acquire acquire;
                return mmio_read8(address).cast<uint8_t>();
            };
        core_callbacks.mmio_write8 =
            [mmio_write8](uint64_t address, uint8_t value) {
                py::gil_scoped_acquire acquire;
                mmio_write8(address, value);
            };
        core_callbacks.on_output =
            [on_output](int port, int value) {
                py::gil_scoped_acquire acquire;
                on_output(port, value);
            };
        if (!csr_read_override.is_none()) {
            py::function override_function =
                csr_read_override.cast<py::function>();
            core_callbacks.csr_read_override =
                [override_function](int address) -> uint64_t {
                    py::gil_scoped_acquire acquire;
                    py::object result =
                        override_function(address);
                    if (result.is_none())
                        return static_cast<uint64_t>(-1);
                    return result.cast<uint64_t>();
                };
        }
        callbacks.push_back(
            std::move(core_callbacks));
    }
    return callbacks;
}

static DmaEndpointCallbacks
build_native_nic_dma_callbacks(SystemState& system) {
    DmaEndpointCallbacks endpoint;
    endpoint.requester_id =
        SystemState::NIC_DMA_REQUESTER_ID;
    endpoint.inspect =
        [&system](uint64_t) {
            DmaEndpointView view;
            view.active =
                system.shared_nic
                    .has_cycle_dma_work();
            const std::optional<NICDMABeat> beat =
                system.shared_nic
                    .cycle_dma_beat();
            if (beat.has_value()) {
                view.pending = DmaBeat{
                    beat->token,
                    std::nullopt,
                    beat->write
                        ? BusOperation::WRITE
                        : BusOperation::READ,
                    beat->address,
                    beat->write_data,
                };
            }
            return view;
        };
    endpoint.complete =
        [&system](
                uint64_t token,
                const BusResult& result) {
            std::optional<uint8_t> read_value;
            if (result.grant.request.operation ==
                BusOperation::READ) {
                read_value = static_cast<uint8_t>(
                    result.read_value.value_or(0));
            }
            if (!system.shared_nic
                     .complete_cycle_dma(
                         token,
                         read_value)) {
                throw std::logic_error(
                    "native NIC rejected its DMA completion");
            }
        };
    return endpoint;
}

static DmaEndpointCallbacks
build_inactive_dma_callbacks(int requester_id) {
    DmaEndpointCallbacks endpoint;
    endpoint.requester_id = requester_id;
    endpoint.inspect =
        [](uint64_t) {
            return DmaEndpointView{};
        };
    endpoint.complete =
        [](uint64_t, const BusResult&) {
            throw std::logic_error(
                "inactive DMA endpoint received a completion");
        };
    return endpoint;
}

static std::vector<DmaEndpointCallbacks>
build_system_dma_callbacks(
        SystemState& system,
        const py::list& callback_sets) {
    if (callback_sets.size() !=
        system.dma_cycle_states.size()) {
        throw std::invalid_argument(
            "one callback endpoint is required for NIC and disk DMA");
    }

    std::vector<DmaEndpointCallbacks> callbacks;
    callbacks.reserve(system.dma_cycle_states.size());
    for (std::size_t index = 0;
         index < system.dma_cycle_states.size();
         index++) {
        py::tuple callback_set =
            py::cast<py::tuple>(
                callback_sets[index]);
        if (callback_set.size() != 2) {
            throw std::invalid_argument(
                "each DMA callback endpoint must contain inspect "
                "and completion entries");
        }
        py::object inspect_object = callback_set[0];
        py::object complete_object = callback_set[1];
        if (inspect_object.is_none() !=
            complete_object.is_none()) {
            throw std::invalid_argument(
                "a DMA endpoint must provide both callbacks or neither");
        }

        DmaEndpointCallbacks endpoint;
        endpoint.requester_id =
            system.dma_cycle_states[index].requester_id;
        if (inspect_object.is_none()) {
            if (endpoint.requester_id ==
                SystemState::NIC_DMA_REQUESTER_ID) {
                endpoint =
                    build_native_nic_dma_callbacks(
                        system);
            } else {
                endpoint =
                    build_inactive_dma_callbacks(
                        endpoint.requester_id);
            }
        } else {
            py::function inspect =
                inspect_object.cast<py::function>();
            py::function complete =
                complete_object.cast<py::function>();
            endpoint.inspect =
                [inspect](uint64_t current_cycle) {
                    py::gil_scoped_acquire acquire;
                    py::object view = inspect(current_cycle);
                    if (view.is_none())
                        return DmaEndpointView{};
                    return view.cast<DmaEndpointView>();
                };
            endpoint.complete =
                [complete](
                        uint64_t token,
                        const BusResult& result) {
                    py::gil_scoped_acquire acquire;
                    complete(token, result);
                };
        }
        callbacks.push_back(std::move(endpoint));
    }
    return callbacks;
}


// ---------------------------------------------------------------------------
//  pybind11 module
// ---------------------------------------------------------------------------

PYBIND11_MODULE(_mp64_accel, m) {
    m.doc() = "C++ accelerated core for Megapad-64 emulator";

    py::class_<PythonMemoryUseScope>(m, "_MemoryUseScope")
        .def(
            "__enter__",
            [](PythonMemoryUseScope& scope) -> PythonMemoryUseScope& {
                return scope;
            },
            py::return_value_policy::reference_internal)
        .def(
            "__exit__",
            [](PythonMemoryUseScope& scope,
               py::object, py::object, py::object) {
                scope.close();
                return false;
            })
        ;

    // Expose CPUState
    py::class_<CPUState>(m, "CPUState")
        .def(py::init([]() { return make_cpu_state(); }))
        .def_property_readonly(
            "is_micro_core",
            [](const CPUState& state) {
                return state.profile == CoreProfile::MICRO;
            })
        .def(
            "_memory_use",
            [](CPUState& state) {
                return std::make_unique<PythonMemoryUseScope>(
                    state, /*permit_native_execution=*/false);
            },
            py::keep_alive<0, 1>())
        .def(
            "_logical_memory_use",
            [](CPUState& state) {
                if (
                    state.system_batch_active != nullptr &&
                    state.system_batch_active->load(
                        std::memory_order_acquire)
                ) {
                    throw std::runtime_error(
                        "native system batch is already active; "
                        "CPUState is already executing");
                }
                return std::make_unique<PythonMemoryUseScope>(
                    state, /*permit_native_execution=*/true);
            },
            py::keep_alive<0, 1>())
        .def_readwrite("psel", &CPUState::psel)
        .def_readwrite("xsel", &CPUState::xsel)
        .def_readwrite("spsel", &CPUState::spsel)
        .def_readwrite("flag_z", &CPUState::flag_z)
        .def_readwrite("flag_c", &CPUState::flag_c)
        .def_readwrite("flag_n", &CPUState::flag_n)
        .def_readwrite("flag_v", &CPUState::flag_v)
        .def_readwrite("flag_p", &CPUState::flag_p)
        .def_readwrite("flag_g", &CPUState::flag_g)
        .def_readwrite("flag_i", &CPUState::flag_i)
        .def_readwrite("flag_s", &CPUState::flag_s)
        .def_readwrite("d_reg", &CPUState::d_reg)
        .def_readwrite("q_out", &CPUState::q_out)
        .def_readwrite("t_reg", &CPUState::t_reg)
        .def_readwrite("sb", &CPUState::sb)
        .def_readwrite("sr", &CPUState::sr)
        .def_readwrite("sc", &CPUState::sc)
        .def_readwrite("sw", &CPUState::sw)
        .def_readwrite("tmode", &CPUState::tmode)
        .def_readwrite("tctrl", &CPUState::tctrl)
        .def_readwrite("tsrc0", &CPUState::tsrc0)
        .def_readwrite("tsrc1", &CPUState::tsrc1)
        .def_readwrite("tdst", &CPUState::tdst)
        .def_property(
            "tacc",
            [](const CPUState& state) {
                return py::bytes(
                    reinterpret_cast<const char*>(
                        state.tacc.data()),
                    state.tacc.size());
            },
            [](CPUState& state, const py::bytes& image) {
                const std::string bytes = image;
                if (bytes.size() != state.tacc.size()) {
                    throw std::invalid_argument(
                        "TACC image must be exactly 256 bytes");
                }
                std::copy(
                    bytes.begin(),
                    bytes.end(),
                    state.tacc.begin());
            })
        .def_readwrite(
            "tacc_owner",
            &CPUState::tacc_owner)
        .def_readwrite(
            "tacc_valid",
            &CPUState::tacc_valid)
        .def_readwrite(
            "tacc_dirty",
            &CPUState::tacc_dirty)
        .def_readwrite(
            "tacc_format_ew",
            &CPUState::tacc_format_ew)
        .def_readwrite(
            "tacc_format_signed",
            &CPUState::tacc_format_signed)
        .def_readwrite(
            "tacc_busy",
            &CPUState::tacc_busy)
        .def_readwrite(
            "tacc_force_pending",
            &CPUState::tacc_force_pending)
        .def_readwrite(
            "tacc_epoch",
            &CPUState::tacc_epoch)
        .def(
            "tacc_snapshot",
            [](const CPUState& state) {
                return snapshot_tacc_state(state);
            })
        .def(
            "tacc_restore",
            [](CPUState& state, const py::dict& snapshot) {
                validate_exact_snapshot_schema(
                    snapshot,
                    TACC_SNAPSHOT_FIELDS,
                    "TACC");
                const PreparedTaccState prepared =
                    prepare_tacc_state(snapshot);
                if (
                    state.profile == CoreProfile::FULL &&
                    prepared.owner != TACC_OWNER_NONE &&
                    prepared.owner != state.core_id
                ) {
                    throw std::invalid_argument(
                        "full-core TACC owner must be that core's absolute ID");
                }
                commit_tacc_state(state, prepared);
            },
            py::arg("state"))
        .def(
            "tacc_reset",
            [](CPUState& state) {
                state.reset_tacc();
            })
        .def_readwrite("ivt_base", &CPUState::ivt_base)
        .def_readwrite("ivec_id", &CPUState::ivec_id)
        .def_readwrite("trap_addr", &CPUState::trap_addr)
        .def_readwrite("ef_flags", &CPUState::ef_flags)
        .def_readwrite("halted", &CPUState::halted)
        .def_readwrite("idle", &CPUState::idle)
        .def_readwrite("cycle_count", &CPUState::cycle_count)
        .def_readwrite("tstride_r", &CPUState::tstride_r)
        .def_readwrite("tstride_c", &CPUState::tstride_c)
        .def_readwrite("ttile_h", &CPUState::ttile_h)
        .def_readwrite("ttile_w", &CPUState::ttile_w)
        .def_readwrite("perf_enable", &CPUState::perf_enable)
        .def_readwrite("perf_cycles", &CPUState::perf_cycles)
        .def_readwrite("perf_stalls", &CPUState::perf_stalls)
        .def_readwrite("perf_tileops", &CPUState::perf_tileops)
        .def_readwrite("perf_extmem", &CPUState::perf_extmem)
        .def_readwrite("bist_status", &CPUState::bist_status)
        .def_readwrite("bist_fail_addr", &CPUState::bist_fail_addr)
        .def_readwrite("bist_fail_data", &CPUState::bist_fail_data)
        .def_readwrite("tile_selftest", &CPUState::tile_selftest)
        .def_readwrite("tile_st_detail", &CPUState::tile_st_detail)
        .def_readwrite("icache_enabled", &CPUState::icache_enabled)
        .def_readwrite("icache_hits", &CPUState::icache_hits)
        .def_readwrite("icache_misses", &CPUState::icache_misses)
        .def("icache_reset", [](CPUState& s) {
            icache_reset(s);
        })
        .def("icache_control_write", [](CPUState& s, uint64_t value) {
            if (s.profile != CoreProfile::FULL)
                return;
            s.icache_enabled = value & 1;
            if (value & 2)
                icache_invalidate_all(s, true);
        })
        .def("icache_invalidate_span",
            [](CPUState& s, uint64_t address, uint64_t size) {
                icache_invalidate_span(s, address, size);
            })
        .def("icache_snapshot", [](const CPUState& s) {
            py::tuple result(3);
            result[0] = py::bytes(
                reinterpret_cast<const char*>(
                    s.icache_valid.data()),
                s.icache_valid.size());
            result[1] = py::cast(s.icache_tags);
            result[2] = py::bytes(
                reinterpret_cast<const char*>(
                    s.icache_data.data()),
                sizeof(s.icache_data));
            return result;
        })
        .def("icache_restore",
            [](CPUState& s,
               const py::bytes& valid_bytes,
               const std::array<
                   uint64_t,
                   CPUState::ICACHE_LINES>& tags,
               const py::bytes& data_bytes) {
                const std::string valid = valid_bytes;
                const std::string data = data_bytes;
                if (valid.size() != s.icache_valid.size() ||
                    data.size() != sizeof(s.icache_data)) {
                    throw py::value_error(
                        "invalid I-cache snapshot geometry");
                }
                std::memcpy(
                    s.icache_valid.data(),
                    valid.data(),
                    valid.size());
                s.icache_tags = tags;
                std::memcpy(
                    s.icache_data.data(),
                    data.data(),
                    data.size());
                s.ifetch_window_valid = false;
                s.clear_private_decode_cache();
            })
        .def_readwrite("priv_level", &CPUState::priv_level)
        .def_readwrite("mpu_base", &CPUState::mpu_base)
        .def_readwrite("mpu_limit", &CPUState::mpu_limit)
        .def_readwrite("ext_modifier", &CPUState::ext_modifier)
        .def_readwrite("crc_acc", &CPUState::crc_acc)
        .def_readwrite("crc_mode", &CPUState::crc_mode)
        .def_readwrite("sha_mode", &CPUState::sha_mode)
        .def_readwrite("sha_msglen_lo", &CPUState::sha_msglen_lo)
        .def_readwrite("sha_msglen_hi", &CPUState::sha_msglen_hi)
        .def_readwrite("gf_prime_sel", &CPUState::gf_prime_sel)
        .def_readwrite("core_id", &CPUState::core_id)
        .def_readwrite("num_cores", &CPUState::num_cores)
        .def_property(
            "irq_ipi",
            [](const CPUState& s) {
                if (s.interrupts != nullptr)
                    return s.interrupts->ipi_line(s.core_id);
                return s.private_irq_ipi.load(
                    std::memory_order_acquire);
            },
            [](CPUState& s, bool asserted) {
                if (s.interrupts != nullptr) {
                    s.interrupts->set_ipi_line(
                        s.core_id, asserted);
                } else {
                    s.private_irq_ipi.store(
                        asserted, std::memory_order_release);
                }
            })
        .def("ipi_pending_mask", [](const CPUState& s) {
            return s.interrupts != nullptr
                ? s.interrupts->pending_mask(s.core_id)
                : uint64_t{0};
        })
        .def("ipi_send", [](CPUState& s, uint64_t target_id) {
            return s.interrupts != nullptr &&
                s.interrupts->send_ipi(
                    s.core_id, static_cast<uint8_t>(target_id));
        })
        .def("ipi_ack", [](CPUState& s, uint64_t source_id) {
            return s.interrupts != nullptr &&
                s.interrupts->acknowledge_ipi(
                    s.core_id, static_cast<uint8_t>(source_id));
        })
        .def_property("mem_size",
            [](const CPUState& s) { return s.memory->mem_size; },
            [](CPUState& s, uint64_t size) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                if (size == 0)
                    throw py::value_error(
                        "main memory size must be greater than zero");
                if (!s.memory->mem_lease || size > s.memory->mem_capacity)
                    throw py::value_error(
                        "main memory size exceeds attached buffer capacity");
                s.memory->mem_size = size;
                sync_main_memory_ptrs(s);
            })
        // Register access
        .def("get_reg", [](const CPUState& s, int i) { return s.regs[i & 0x1F]; })
        .def("set_reg", [](CPUState& s, int i, uint64_t v) { s.regs[i & 0x1F] = v; })
        // Accumulator access
        .def("get_acc", [](const CPUState& s, int i) { return s.acc[i & 3]; })
        .def("set_acc", [](CPUState& s, int i, uint64_t v) { s.acc[i & 3] = v; })
        // Port access
        .def("get_port_out", [](const CPUState& s, int i) { return s.port_out[i & 7]; })
        .def("set_port_in", [](CPUState& s, int i, uint8_t v) { s.port_in[i & 7] = v; })
        // Port bridge remap table
        .def("get_port_map", [](const CPUState& s, int i) -> uint32_t { return s.port_map[i & 7]; })
        .def("set_port_map", [](CPUState& s, int i, uint32_t v) { s.port_map[i & 7] = v; })
        // Memory attachment
        .def("attach_mem", [](CPUState& s, py::buffer buf, uint64_t size) {
            require_private_memory_mapping(s);
            PreparedBuffer prepared =
                prepare_writable_byte_buffer(buf, size, true);
            std::unique_ptr<py::buffer_info> old_lease;
            {
                MemoryMutationGuard guard(*s.memory);
                old_lease = std::move(s.memory->mem_lease);
                s.memory->mem_lease = std::move(prepared.lease);
                s.memory->mem = prepared.ptr;
                s.memory->mem_size = size;
                s.memory->mem_capacity = prepared.capacity;
                sync_main_memory_ptrs(s);
            }
            // PyBuffer_Release may invoke arbitrary exporter code.  Release
            // the replaced export only after the mapping lock is free.
        })
        // HBW memory attachment
        .def("attach_hbw_mem", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            require_private_memory_mapping(s);
            validate_guest_region(base, size);
            PreparedBuffer prepared =
                prepare_writable_byte_buffer(buf, size, false);
            std::unique_ptr<py::buffer_info> old_lease;
            {
                MemoryMutationGuard guard(*s.memory);
                old_lease = std::move(s.memory->hbw_lease);
                s.memory->hbw_lease = std::move(prepared.lease);
                s.memory->hbw_mem = prepared.ptr;
                s.memory->hbw_base = base;
                s.memory->hbw_size = size;
                s.memory->hbw_capacity = prepared.capacity;
                sync_nic_memory_ptrs(s);
            }
        })
        .def_property("hbw_base",
            [](const CPUState& s) { return s.memory->hbw_base; },
            [](CPUState& s, uint64_t base) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                validate_guest_region(base, s.memory->hbw_size);
                s.memory->hbw_base = base;
                sync_nic_memory_ptrs(s);
            })
        .def_property("hbw_size",
            [](const CPUState& s) { return s.memory->hbw_size; },
            [](CPUState& s, uint64_t size) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                if ((size != 0 && !s.memory->hbw_lease) || size > s.memory->hbw_capacity)
                    throw py::value_error(
                        "HBW memory size exceeds attached buffer capacity");
                validate_guest_region(s.memory->hbw_base, size);
                s.memory->hbw_size = size;
                sync_nic_memory_ptrs(s);
            })
        // External memory attachment
        .def("attach_ext_mem", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            require_private_memory_mapping(s);
            validate_guest_region(base, size);
            PreparedBuffer prepared =
                prepare_writable_byte_buffer(buf, size, false);
            std::unique_ptr<py::buffer_info> old_lease;
            {
                MemoryMutationGuard guard(*s.memory);
                old_lease = std::move(s.memory->ext_mem_lease);
                s.memory->ext_mem_lease = std::move(prepared.lease);
                s.memory->ext_mem = prepared.ptr;
                s.memory->ext_mem_base = base;
                s.memory->ext_mem_size = size;
                s.memory->ext_mem_capacity = prepared.capacity;
                sync_nic_memory_ptrs(s);
            }
        })
        .def_property("ext_mem_base",
            [](const CPUState& s) { return s.memory->ext_mem_base; },
            [](CPUState& s, uint64_t base) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                validate_guest_region(base, s.memory->ext_mem_size);
                s.memory->ext_mem_base = base;
                sync_nic_memory_ptrs(s);
            })
        .def_property("ext_mem_size",
            [](const CPUState& s) { return s.memory->ext_mem_size; },
            [](CPUState& s, uint64_t size) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                if ((size != 0 && !s.memory->ext_mem_lease) ||
                    size > s.memory->ext_mem_capacity)
                    throw py::value_error(
                        "external memory size exceeds attached buffer capacity");
                validate_guest_region(s.memory->ext_mem_base, size);
                s.memory->ext_mem_size = size;
                sync_nic_memory_ptrs(s);
            })
        // VRAM memory attachment
        .def("attach_vram", [](CPUState& s, py::buffer buf, uint64_t base, uint64_t size) {
            require_private_memory_mapping(s);
            validate_guest_region(base, size);
            PreparedBuffer prepared =
                prepare_writable_byte_buffer(buf, size, false);
            std::unique_ptr<py::buffer_info> old_lease;
            {
                MemoryMutationGuard guard(*s.memory);
                old_lease = std::move(s.memory->vram_lease);
                s.memory->vram_lease = std::move(prepared.lease);
                s.memory->vram_mem = prepared.ptr;
                s.memory->vram_base = base;
                s.memory->vram_size = size;
                s.memory->vram_capacity = prepared.capacity;
            }
        })
        .def_property("vram_base",
            [](const CPUState& s) { return s.memory->vram_base; },
            [](CPUState& s, uint64_t base) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                validate_guest_region(base, s.memory->vram_size);
                s.memory->vram_base = base;
            })
        .def_property("vram_size",
            [](const CPUState& s) { return s.memory->vram_size; },
            [](CPUState& s, uint64_t size) {
                require_private_memory_mapping(s);
                MemoryMutationGuard guard(*s.memory);
                if ((size != 0 && !s.memory->vram_lease) ||
                    size > s.memory->vram_capacity)
                    throw py::value_error(
                        "VRAM size exceeds attached buffer capacity");
                validate_guest_region(s.memory->vram_base, size);
                s.memory->vram_size = size;
            })
        // Native UART
        .def("uart_init", [](CPUState& s) {
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState UART memory cannot be initialized while memory is in use");
            s.uart->init();
            s.uart->attach_mem(s.memory->mem, s.memory->mem_size);
        })
        .def("uart_disable", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.uart->enabled = false;
        })
        .def("uart_enabled", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.uart->enabled;
        })
        .def("uart_read8", [](CPUState& s, uint32_t off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.uart->read8(off);
        })
        .def("uart_write8", [](CPUState& s, uint32_t off, uint8_t value) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.uart->write8(off, value);
        })
        .def("uart_inject", [](CPUState& s, py::bytes payload) {
            std::string data = payload;
            s.uart->inject(reinterpret_cast<const uint8_t*>(data.data()), data.size());
        })
        .def("uart_has_rx", [](const CPUState& s) { return s.uart->has_rx_data(); })
        .def("uart_rx_size", [](const CPUState& s) { return s.uart->rx_size(); })
        .def_property("uart_tx_ring_base",
            [](const CPUState& s) { return s.uart->get_tx_ring_base(); },
            [](CPUState& s, uint64_t value) { s.uart->set_tx_ring_base(value); })
        .def("uart_drain_tx", [](CPUState& s) -> py::bytes {
            const std::vector<uint8_t> data = s.uart->take_tx();
            if (data.empty())
                return py::bytes();
            return py::bytes(reinterpret_cast<const char*>(data.data()), data.size());
        })
        // Flags
        .def("flags_pack", [](const CPUState& s) { return flags_pack(s); })
        .def("flags_unpack", [](CPUState& s, uint8_t v) { flags_unpack(s, v); })
        // Crypto devices — initialize C++ native crypto accelerators
        .def("init_crypto", [](CPUState& s) {
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState crypto memory cannot be initialized while memory is in use");
            s.crypto->init();
            // Ensure WOTS chain has current memory pointer
            s.crypto->wots.mem = s.memory->mem;
            s.crypto->wots.mem_size = s.memory->mem_size;
        })
        .def("disable_crypto", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.crypto->enabled = false;
        })
        .def("crypto_enabled", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.crypto->enabled;
        })
        // Sync crypto state from Python devices (for save/restore)
        .def("crypto_aes_reset", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.crypto->aes.reset();
        })
        .def("crypto_sha3_reset", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.crypto->sha3.reset();
            s.crypto->sha3.mode = 0;
        })
        .def("crypto_wots_reset", [](CPUState& s) {
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState WOTS memory cannot be reset while memory is in use");
            s.crypto->wots.reset();
            s.crypto->wots.sha3 = &s.crypto->sha3;
            s.crypto->wots.mem = s.memory->mem;
            s.crypto->wots.mem_size = s.memory->mem_size;
        })
        .def("crypto_wots_status", [](CPUState& s) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.crypto->wots.status;
        })
        // Direct crypto MMIO access (for testing / Python-side access)
        .def("crypto_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.crypto->read8(mmio_off);
        })
        .def("crypto_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.crypto->write8(mmio_off, val);
        })
        // ── NIC device ────────────────────────────────────────
        .def("nic_init", [](CPUState& s, py::bytes mac_bytes) {
            require_cycle_device_mutation_allowed(s, "native NIC");
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState NIC memory cannot be initialized while memory is in use");
            std::string mac_str = mac_bytes;
            uint8_t mac[6] = {};
            size_t n = std::min(mac_str.size(), (size_t)6);
            std::memcpy(mac, mac_str.data(), n);
            s.nic->init(mac);
            // Wire memory pointers from CPUState
            s.nic->attach_mem_ptrs(
                s.memory->mem, s.memory->mem_size,
                s.memory->hbw_mem, s.memory->hbw_base, s.memory->hbw_size,
                s.memory->ext_mem, s.memory->ext_mem_base, s.memory->ext_mem_size
            );
        })
        .def("nic_sync_mem_ptrs", [](CPUState& s) {
            // Re-sync memory pointers after attach_ext_mem / attach_hbw_mem
            require_cycle_device_mutation_allowed(s, "native NIC");
            MemoryMutationGuard guard(*s.memory);
            sync_nic_memory_ptrs(s);
        })
        .def("nic_set_tx_callback", [](CPUState& s, py::function cb) {
            // tx_callback: called from C++ when NIC sends a frame
            // cb receives (bytes,) and returns bool
            require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic->tx_callback = [cb](const uint8_t* data, size_t len) -> bool {
                py::gil_scoped_acquire gil;
                try {
                    py::bytes frame(reinterpret_cast<const char*>(data), len);
                    py::object result = cb(frame);
                    if (result.is_none()) return true;
                    return result.cast<bool>();
                } catch (...) {
                    return false;
                }
            };
        })
        .def("nic_inject_frame", [](CPUState& s, py::bytes frame) -> bool {
            require_cycle_device_mutation_allowed(s, "native NIC");
            std::string data = frame;
            return s.nic->inject_frame(
                reinterpret_cast<const uint8_t*>(data.data()), data.size()
            );
        })
        .def("nic_has_rx", [](CPUState& s) -> bool {
            return s.nic->has_rx();
        })
        .def("nic_rx_queue_size", [](CPUState& s) -> size_t {
            return s.nic->rx_queue_size();
        })
        .def("nic_tx_queue_size", [](CPUState& s) -> size_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.nic->tx_queue_size();
        })
        .def("nic_drain_one_tx", [](CPUState& s) -> py::bytes {
            auto memory_guard = acquire_shared_memory_use(s);
            auto frame = s.nic->drain_one_tx();
            return py::bytes(reinterpret_cast<const char*>(frame.data()), frame.size());
        })
        .def("nic_set_link_up", [](CPUState& s, bool up) {
            require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic->link_up = up;
        })
        .def("nic_enabled", [](CPUState& s) -> bool {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.nic->enabled;
        })
        .def("nic_disable", [](CPUState& s) {
            require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic->enabled = false;
        })
        .def("nic_reset", [](CPUState& s) {
            require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic->reset_state();
        })
        .def("nic_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.nic->read8(mmio_off);
        })
        .def("nic_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic->write8(mmio_off, val);
        })
        .def("nic_cycle_dma_snapshot", [](CPUState& s) {
            if (
                s.system_batch_active != nullptr &&
                s.system_batch_active->load(
                    std::memory_order_acquire)
            ) {
                throw std::runtime_error(
                    "native NIC DMA state cannot be observed during an "
                    "active native system batch");
            }
            auto memory_guard = acquire_shared_memory_use(s);
            py::dict snapshot;
            snapshot["schema_version"] = 1;
            snapshot["rx_active"] = s.nic->rx_dma_active;
            snapshot["tx_active"] = s.nic->tx_dma_active;
            snapshot["rx_base"] = s.nic->rx_dma_base;
            snapshot["tx_base"] = s.nic->tx_dma_base;
            snapshot["tx_length"] = s.nic->tx_dma_len;
            snapshot["rx_index"] = s.nic->rx_dma_index;
            snapshot["tx_index"] = s.nic->tx_dma_index;
            snapshot["rx_frame"] = py::bytes(
                reinterpret_cast<const char*>(
                    s.nic->rx_dma_frame.data()),
                s.nic->rx_dma_frame.size());
            snapshot["tx_frame"] = py::bytes(
                reinterpret_cast<const char*>(
                    s.nic->tx_dma_frame.data()),
                s.nic->tx_dma_frame.size());
            snapshot["next_token"] = s.nic->next_dma_token;
            if (s.nic->pending_dma_beat.has_value()) {
                const NICDMABeat& beat =
                    *s.nic->pending_dma_beat;
                py::dict pending;
                pending["token"] = beat.token;
                pending["owner"] =
                    static_cast<int>(beat.owner);
                pending["address"] = beat.address;
                pending["write"] = beat.write;
                pending["write_data"] = beat.write_data;
                snapshot["pending"] = pending;
            } else {
                snapshot["pending"] = py::none();
            }
            return snapshot;
        })
        .def("nic_irq_pending", [](const CPUState& s) -> bool {
            return s.nic->irq_pending();
        })
        .def("nic_get_tx_count", [](CPUState& s) -> uint16_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.nic->tx_count;
        })
        .def("nic_get_rx_count", [](const CPUState& s) -> uint16_t {
            return s.nic->rx_count.load(std::memory_order_relaxed);
        })
        // ── TRNG device ───────────────────────────────────────
        .def("init_trng", [](CPUState& s) {
            require_cycle_device_mutation_allowed(s, "native TRNG");
            MemoryMutationGuard guard(
                *s.memory,
                "native TRNG cannot initialize while memory is in use");
            s.trng->init();
        })
        .def("trng_enabled", [](CPUState& s) -> bool {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.trng->is_enabled();
        })
        .def("trng_usable", [](CPUState& s) -> bool {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.trng->is_usable();
        })
        .def("disable_trng", [](CPUState& s) {
            require_cycle_device_mutation_allowed(s, "native TRNG");
            MemoryMutationGuard guard(
                *s.memory,
                "native TRNG cannot disable while memory is in use");
            s.trng->disable();
        })
        .def("_trng_test_health_loss_after",
             [](CPUState& s, std::size_t successful_bytes) {
            require_cycle_device_mutation_allowed(
                s, "native TRNG test seam");
            MemoryMutationGuard guard(
                *s.memory,
                "native TRNG test seam cannot mutate while memory is in use");
            s.trng->test_inject_health_loss_after(
                successful_bytes);
        })
        .def("_trng_test_fail_next_refill", [](CPUState& s) {
            require_cycle_device_mutation_allowed(
                s, "native TRNG test seam");
            MemoryMutationGuard guard(
                *s.memory,
                "native TRNG test seam cannot mutate while memory is in use");
            s.trng->test_fail_next_host_refill();
        })
        .def("_trng_test_zeroized_state", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.trng->test_zeroized_state();
        })
        .def("_native_singleton_read8",
             [](CPUState& s, uint32_t mmio_off) -> int {
            auto memory_guard = acquire_shared_memory_use(s);
            if (s.nic->handles(mmio_off))
                return s.nic->read8(mmio_off);
            if (s.trng->handles(mmio_off))
                return s.trng->read8(mmio_off);
            if (s.crypto->handles(mmio_off))
                return s.crypto->read8(mmio_off);
            return -1;
        })
        .def("_native_singleton_write8",
             [](CPUState& s, uint32_t mmio_off, uint8_t value) -> bool {
            if (s.nic->handles(mmio_off))
                require_cycle_device_mutation_allowed(s, "native NIC");
            auto memory_guard = acquire_shared_memory_use(s);
            if (s.nic->handles(mmio_off)) {
                s.nic->write8(mmio_off, value);
                return true;
            }
            if (s.trng->handles(mmio_off)) {
                s.trng->write8(mmio_off, value);
                return true;
            }
            if (s.crypto->handles(mmio_off)) {
                s.crypto->write8(mmio_off, value);
                return true;
            }
            return false;
        })
        // ── Framebuffer device ────────────────────────────────
        .def("fb_init", [](CPUState& s) {
            s.fb->init();
        })
        .def("fb_enabled", [](CPUState& s) -> bool {
            std::lock_guard<std::mutex> framebuffer_guard(
                s.fb->mutex);
            return s.fb->enabled;
        })
        .def("fb_disable", [](CPUState& s) {
            std::lock_guard<std::mutex> framebuffer_guard(
                s.fb->mutex);
            s.fb->enabled = false;
        })
        .def("fb_tick", [](CPUState& s, uint64_t cycles) {
            s.fb->tick(cycles);
        })
        .def("fb_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.fb->read8(mmio_off);
        })
        .def("fb_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.fb->write8(mmio_off, val);
        })
        .def("fb_irq_pending", [](CPUState& s) -> bool {
            return s.fb->irq_pending();
        })
        .def("fb_host_present", [](CPUState& s) {
            s.fb->host_present();
        })
        // FB properties for display thread access
        .def_property("fb_base_addr",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->fb_base;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->fb_base = v;
            })
        .def_property("fb_width",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->width;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->width = v;
            })
        .def_property("fb_height",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->height;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->height = v;
            })
        .def_property("fb_stride",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->stride;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->stride = v;
            })
        .def_property("fb_mode",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->mode;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->mode = v;
            })
        .def_property("fb_enable",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->enable;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->enable = v;
            })
        .def_property("fb_vsync_count",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->vsync_count;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->vsync_count = v;
            })
        .def_property("fb_vblank",
            [](CPUState& s) -> bool {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->vblank;
            },
            [](CPUState& s, bool v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->vblank = v;
            })
        .def_property("fb_cycles_per_frame",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                return s.fb->cycles_per_frame;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->cycles_per_frame = v;
            })
        .def("fb_get_palette", [](CPUState& s) -> std::vector<uint32_t> {
            const auto framebuffer = s.fb->snapshot();
            return std::vector<uint32_t>(
                framebuffer.palette.begin(),
                framebuffer.palette.end());
        })
        .def("fb_set_palette_entry", [](CPUState& s, int idx, uint32_t rgb) {
            if (idx >= 0 && idx < 256) {
                std::lock_guard<std::mutex> framebuffer_guard(
                    s.fb->mutex);
                s.fb->palette[idx] = rgb & 0x00FFFFFF;
            }
        })
        .def("fb_snapshot", [](CPUState& s) {
            const auto framebuffer = s.fb->snapshot();
            return py::make_tuple(
                framebuffer.fb_base,
                framebuffer.width,
                framebuffer.height,
                framebuffer.stride,
                framebuffer.mode,
                framebuffer.enable,
                framebuffer.vsync_count,
                framebuffer.vblank,
                framebuffer.cycles_per_frame);
        })
        // ── Framebuffer render (C++ pixel conversion) ─────────
        //
        // Converts VRAM pixel data into an RGB888 numpy array suitable
        // for pygame.surfarray.blit_array().  Returns shape (w, h, 3)
        // with dtype uint8.  Runs without GIL for maximum throughput.
        //
        // Modes: 0 = 8-bit indexed (palette lookup)
        //        1 = RGB565
        //        3 = RGBA8888 (alpha discarded)
        //
        // Returns None if the framebuffer base address doesn't map to
        // any attached memory region.
        .def("render_fb_rgb", [](CPUState& s) -> py::object {
            ExclusiveMemoryUseGuard memory_guard(
                *s.memory, "CPUState framebuffer render is busy");
            const auto framebuffer = s.fb->snapshot();
            uint32_t w = framebuffer.width;
            uint32_t h = framebuffer.height;
            uint32_t stride = framebuffer.stride;
            uint8_t  mode = framebuffer.mode;
            uint64_t base = framebuffer.fb_base;

            if (w == 0 || h == 0 || w > 4096 || h > 4096)
                return py::none();

            // Resolve base address to a memory pointer
            const uint8_t* src = nullptr;
            uint64_t mem_size = 0;
            uint64_t mem_off = 0;

            if (s.memory->vram_mem &&
                region_contains(s.memory->vram_base, s.memory->vram_size, base)) {
                src = s.memory->vram_mem;
                mem_off = base - s.memory->vram_base;
                mem_size = s.memory->vram_size;
            } else if (s.memory->ext_mem &&
                       region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, base)) {
                src = s.memory->ext_mem;
                mem_off = base - s.memory->ext_mem_base;
                mem_size = s.memory->ext_mem_size;
            } else if (s.memory->hbw_mem &&
                       region_contains(s.memory->hbw_base, s.memory->hbw_size, base)) {
                src = s.memory->hbw_mem;
                mem_off = base - s.memory->hbw_base;
                mem_size = s.memory->hbw_size;
            }
            if (!src)
                return py::none();

            // Allocate output: shape (w, h, 3) for pygame surfarray
            auto result = py::array_t<uint8_t>({(int)w, (int)h, 3});
            std::memset(result.mutable_data(), 0,
                        static_cast<size_t>(result.nbytes()));
            auto buf = result.mutable_unchecked<3>();
            const auto& palette = framebuffer.palette;

            // Release GIL for the pixel conversion loop
            {
                py::gil_scoped_release release;

                if (mode == 0) {
                    // 8-bit indexed — palette lookup
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + w > mem_size) break;
                        const uint8_t* row = src + row_off;
                        for (uint32_t x = 0; x < w; x++) {
                            uint32_t rgb = palette[row[x]];
                            buf(x, y, 0) = (rgb >> 16) & 0xFF;
                            buf(x, y, 1) = (rgb >>  8) & 0xFF;
                            buf(x, y, 2) =  rgb        & 0xFF;
                        }
                    }
                } else if (mode == 1) {
                    // RGB565
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + (uint64_t)w * 2 > mem_size) break;
                        const uint8_t* row = src + row_off;
                        for (uint32_t x = 0; x < w; x++) {
                            const uint32_t byte_off = x * 2;
                            const uint16_t px =
                                static_cast<uint16_t>(row[byte_off]) |
                                (static_cast<uint16_t>(row[byte_off + 1]) << 8);
                            buf(x, y, 0) = ((px >> 11) & 0x1F) << 3;
                            buf(x, y, 1) = ((px >>  5) & 0x3F) << 2;
                            buf(x, y, 2) = ( px        & 0x1F) << 3;
                        }
                    }
                } else if (mode == 3) {
                    // RGBA8888 — drop alpha
                    for (uint32_t y = 0; y < h; y++) {
                        uint64_t row_off = mem_off + (uint64_t)y * stride;
                        if (row_off + (uint64_t)w * 4 > mem_size) break;
                        const uint8_t* row = src + row_off;
                        for (uint32_t x = 0; x < w; x++) {
                            uint32_t off4 = x * 4;
                            buf(x, y, 0) = row[off4 + 0];
                            buf(x, y, 1) = row[off4 + 1];
                            buf(x, y, 2) = row[off4 + 2];
                        }
                    }
                }
                // Unknown mode: array stays zero-filled (gray/black)
            }

            return result;
        })
        // ── Timer device ──────────────────────────────────────
        .def("timer_init", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.timer->init();
        })
        .def("timer_enabled", [](CPUState& s) -> bool {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.timer->enabled;
        })
        .def("timer_disable", [](CPUState& s) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.timer->enabled = false;
        })
        .def("timer_tick", [](CPUState& s, uint64_t cycles) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.timer->tick(cycles);
        })
        .def("timer_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.timer->read8(mmio_off);
        })
        .def("timer_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.timer->write8(mmio_off, val);
        })
        .def_property("timer_irq_pending",
            [](CPUState& s) -> bool {
                auto memory_guard = acquire_shared_memory_use(s);
                return s.timer->irq_pending;
            },
            [](CPUState& s, bool v) {
                auto memory_guard = acquire_shared_memory_use(s);
                s.timer->irq_pending = v;
            })
        .def_property("timer_counter",
            [](CPUState& s) -> uint32_t {
                auto memory_guard = acquire_shared_memory_use(s);
                return s.timer->counter;
            },
            [](CPUState& s, uint32_t v) {
                auto memory_guard = acquire_shared_memory_use(s);
                s.timer->counter = v;
            })
        .def_property("timer_compare",
            [](CPUState& s) -> uint32_t {
                auto memory_guard = acquire_shared_memory_use(s);
                return s.timer->compare;
            },
            [](CPUState& s, uint32_t v) {
                auto memory_guard = acquire_shared_memory_use(s);
                s.timer->compare = v;
            })
        .def_property("timer_control",
            [](CPUState& s) -> uint8_t {
                auto memory_guard = acquire_shared_memory_use(s);
                return s.timer->control;
            },
            [](CPUState& s, uint8_t v) {
                auto memory_guard = acquire_shared_memory_use(s);
                s.timer->control = v;
            })
        .def_property("timer_status",
            [](CPUState& s) -> uint8_t {
                auto memory_guard = acquire_shared_memory_use(s);
                return s.timer->status;
            },
            [](CPUState& s, uint8_t v) {
                auto memory_guard = acquire_shared_memory_use(s);
                s.timer->status = v;
            })
        // ── RTC device ───────────────────────────────────────────
        .def("rtc_init", [](CPUState& s, bool realtime,
                             uint64_t epoch_ms, uint8_t sec,
                             uint8_t min, uint8_t hour, uint8_t day,
                             uint8_t mon, uint32_t year, uint8_t dow) {
            s.rtc->init(
                realtime, epoch_ms, sec, min, hour, day, mon, year, dow);
        })
        .def("rtc_enabled", [](CPUState& s) -> bool {
            std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
            return s.rtc->enabled;
        })
        .def("rtc_disable", [](CPUState& s) {
            std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
            s.rtc->enabled = false;
        })
        .def("rtc_tick", [](CPUState& s, uint64_t cycles) {
            s.rtc->tick(cycles);
        })
        .def("rtc_sync_realtime", [](CPUState& s) {
            s.rtc->sync_realtime();
        })
        .def("rtc_reanchor_host_clock", [](CPUState& s) {
            s.rtc->reanchor_host_clock();
        })
        .def("rtc_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.rtc->read8(mmio_off);
        })
        .def("rtc_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.rtc->write8(mmio_off, val);
        })
        .def_property("rtc_realtime",
            [](CPUState& s) -> bool {
                return s.rtc->snapshot().realtime;
            },
            [](CPUState& s, bool v) {
                if (s.system_batch_active != nullptr &&
                    s.system_batch_active->load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "RTC mode cannot change during an active native "
                        "system batch");
                }
                if (s.system_cycle_execution_pending != nullptr &&
                    s.system_cycle_execution_pending->load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "RTC mode cannot change while cycle execution is "
                        "suspended");
                }
                s.rtc->set_realtime(v);
            })
        .def_property("rtc_uptime_ms",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->uptime_ms;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->uptime_ms = v;
            })
        .def_property("rtc_epoch_ms",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->epoch_ms;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->epoch_ms = v;
            })
        .def_property("rtc_sec",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->sec;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->sec = v;
            })
        .def_property("rtc_min",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->min;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->min = v;
            })
        .def_property("rtc_hour",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->hour;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->hour = v;
            })
        .def_property("rtc_day",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->day;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->day = v;
            })
        .def_property("rtc_mon",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->mon;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->mon = v;
            })
        .def_property("rtc_year",
            [](CPUState& s) -> uint32_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->year;
            },
            [](CPUState& s, uint32_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->year = v;
            })
        .def_property("rtc_dow",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->dow;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->dow = v;
            })
        .def_property("rtc_ctrl",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->ctrl;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->ctrl = v;
            })
        .def_property("rtc_status",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->status;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->status = v;
            })
        .def_property("rtc_alarm_sec",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->alarm_sec;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->alarm_sec = v;
            })
        .def_property("rtc_alarm_min",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->alarm_min;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->alarm_min = v;
            })
        .def_property("rtc_alarm_hour",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->alarm_hour;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->alarm_hour = v;
            })
        .def_property("rtc_irq_pending",
            [](CPUState& s) -> bool {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->irq_pending;
            },
            [](CPUState& s, bool v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->irq_pending = v;
            })
        .def_property("rtc_ms_prescaler",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->ms_prescaler;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->ms_prescaler = v;
            })
        .def_property("rtc_sec_prescaler",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->sec_prescaler;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->sec_prescaler = v;
            })
        .def_property("rtc_uptime_latch",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->uptime_latch;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->uptime_latch = v;
            })
        .def_property("rtc_epoch_latch",
            [](CPUState& s) -> uint64_t {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->epoch_latch;
            },
            [](CPUState& s, uint64_t v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->epoch_latch = v;
            })
        .def("rtc_snapshot", [](CPUState& s) {
            const auto rtc = s.rtc->snapshot();
            return py::make_tuple(
                rtc.enabled,
                rtc.realtime,
                rtc.uptime_ms,
                rtc.epoch_ms,
                rtc.sec,
                rtc.min,
                rtc.hour,
                rtc.day,
                rtc.mon,
                rtc.year,
                rtc.dow,
                rtc.ctrl,
                rtc.status,
                rtc.alarm_sec,
                rtc.alarm_min,
                rtc.alarm_hour,
                rtc.irq_pending,
                rtc.ms_prescaler,
                rtc.sec_prescaler,
                rtc.uptime_latch,
                rtc.epoch_latch);
        })
        // ── UART Geometry device ──────────────────────────────
        .def("uart_geom_init", [](CPUState& s, uint16_t cols, uint16_t rows) {
            s.uart_geom->init(cols, rows);
        }, py::arg("cols") = 80, py::arg("rows") = 30)
        .def("uart_geom_enabled", [](CPUState& s) -> bool {
            std::lock_guard<std::mutex> geometry_guard(
                s.uart_geom->mutex);
            return s.uart_geom->enabled;
        })
        .def("uart_geom_disable", [](CPUState& s) {
            std::lock_guard<std::mutex> geometry_guard(
                s.uart_geom->mutex);
            s.uart_geom->enabled = false;
        })
        .def("uart_geom_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            return s.uart_geom->read8(mmio_off);
        })
        .def("uart_geom_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            s.uart_geom->write8(mmio_off, val);
        })
        .def_property("uart_geom_cols",
            [](CPUState& s) -> uint16_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->cols;
            },
            [](CPUState& s, uint16_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->cols = v;
            })
        .def_property("uart_geom_rows",
            [](CPUState& s) -> uint16_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->rows;
            },
            [](CPUState& s, uint16_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->rows = v;
            })
        .def_property("uart_geom_status",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->status;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->status = v;
            })
        .def_property("uart_geom_ctrl",
            [](CPUState& s) -> uint8_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->ctrl;
            },
            [](CPUState& s, uint8_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->ctrl = v;
                ++s.uart_geom->request_generation;
            })
        .def_property("uart_geom_req_cols",
            [](CPUState& s) -> uint16_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->req_cols;
            },
            [](CPUState& s, uint16_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->req_cols = v;
                ++s.uart_geom->request_generation;
            })
        .def_property("uart_geom_req_rows",
            [](CPUState& s) -> uint16_t {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                return s.uart_geom->req_rows;
            },
            [](CPUState& s, uint16_t v) {
                std::lock_guard<std::mutex> geometry_guard(
                    s.uart_geom->mutex);
                s.uart_geom->req_rows = v;
                ++s.uart_geom->request_generation;
            })
        .def("uart_geom_host_set_size", [](CPUState& s, uint16_t c, uint16_t r) {
            s.uart_geom->host_set_size(c, r);
        })
        .def("uart_geom_has_resize_request", [](CPUState& s) -> bool {
            return s.uart_geom->has_resize_request();
        })
        .def("uart_geom_snapshot_resize_request",
            [](CPUState& s) -> py::object {
                const auto snapshot =
                    s.uart_geom->snapshot_resize_request();
                if (!snapshot.pending)
                    return py::none();
                return py::make_tuple(
                    snapshot.generation,
                    snapshot.cols,
                    snapshot.rows);
            })
        .def("uart_geom_host_accept_resize", [](CPUState& s, uint16_t c, uint16_t r) {
            s.uart_geom->host_accept_resize(c, r);
        })
        .def("uart_geom_host_deny_resize", [](CPUState& s) {
            s.uart_geom->host_deny_resize();
        })
        .def(
            "uart_geom_host_accept_resize_if_pending",
            [](CPUState& s, uint64_t generation, uint16_t c, uint16_t r) {
                return s.uart_geom->host_accept_resize_if_pending(
                    generation, c, r);
            })
        .def(
            "uart_geom_host_deny_resize_if_pending",
            [](CPUState& s, uint64_t generation) {
                return s.uart_geom->host_deny_resize_if_pending(
                    generation);
            })
        // ── Accelerator hooks ─────────────────────────────────
        .def("register_accel_hook", &CPUState::register_accel_hook)
        .def_readonly("accel_hook_count", &CPUState::accel_hook_count)
        // ── Dictionary cache ──────────────────────────────────
        .def("dict_clear", &CPUState::dict_clear_all)
        ;

    py::enum_<BusOperation>(m, "BusOperation")
        .value("READ", BusOperation::READ)
        .value("WRITE", BusOperation::WRITE);

    py::enum_<BusWidth>(m, "BusWidth")
        .value("BYTE", BusWidth::BYTE)
        .value("HALF", BusWidth::HALF)
        .value("WORD", BusWidth::WORD)
        .value("DOUBLEWORD", BusWidth::DOUBLEWORD);

    py::enum_<BusTarget>(m, "BusTarget")
        .value("MEMORY", BusTarget::MEMORY)
        .value("MMIO", BusTarget::MMIO);

    py::enum_<BusFault>(m, "BusFault")
        .value("NONE", BusFault::NONE)
        .value("MMIO_TIMEOUT", BusFault::MMIO_TIMEOUT)
        .value("MEMORY_TIMEOUT", BusFault::MEMORY_TIMEOUT)
        .value("TARGET_FAULT", BusFault::TARGET_FAULT);

    py::class_<BusOrderingMetadata>(m, "BusOrderingMetadata")
        .def(
            py::init([](
                    int main_port_id,
                    uint64_t issue_sequence,
                    bool port_io) {
                if (main_port_id < 0)
                    throw std::invalid_argument(
                        "main_port_id cannot be negative");
                if (issue_sequence == 0)
                    throw std::invalid_argument(
                        "issue_sequence must be positive");
                return BusOrderingMetadata{
                    main_port_id,
                    issue_sequence,
                    port_io,
                };
            }),
            py::arg("main_port_id"),
            py::arg("issue_sequence"),
            py::arg("port_io") = false)
        .def_readonly(
            "main_port_id",
            &BusOrderingMetadata::main_port_id)
        .def_readonly(
            "issue_sequence",
            &BusOrderingMetadata::issue_sequence)
        .def_readonly("port_io", &BusOrderingMetadata::port_io)
        ;

    py::class_<BusRequest>(m, "BusRequest")
        .def(
            py::init([](
                    int requester_id,
                    uint64_t ready_cycle,
                    BusOperation operation,
                    uint64_t address,
                    BusWidth width,
                    const BusOrderingMetadata& ordering,
                    uint64_t write_data) {
                return BusRequest{
                    requester_id,
                    ready_cycle,
                    operation,
                    address,
                    width,
                    write_data,
                    ordering,
                };
            }),
            py::arg("requester_id"),
            py::arg("ready_cycle"),
            py::arg("operation"),
            py::arg("address"),
            py::arg("width"),
            py::arg("ordering"),
            py::arg("write_data") = 0)
        .def_readonly("requester_id", &BusRequest::requester_id)
        .def_readonly("ready_cycle", &BusRequest::ready_cycle)
        .def_readonly("operation", &BusRequest::operation)
        .def_readonly("address", &BusRequest::address)
        .def_readonly("width", &BusRequest::width)
        .def_readonly("write_data", &BusRequest::write_data)
        .def_readonly("ordering", &BusRequest::ordering)
        ;

    py::class_<BusGrant>(m, "BusGrant")
        .def_readonly("request", &BusGrant::request)
        .def_readonly("grant_sequence", &BusGrant::grant_sequence)
        .def_readonly("grant_cycle", &BusGrant::grant_cycle)
        .def_readonly("target", &BusGrant::target)
        .def_readonly("timeout_cycle", &BusGrant::timeout_cycle)
        ;

    py::class_<BusResult>(m, "BusResult")
        .def_readonly("grant", &BusResult::grant)
        .def_readonly(
            "completion_cycle",
            &BusResult::completion_cycle)
        .def_readonly("read_value", &BusResult::read_value)
        .def_readonly("fault", &BusResult::fault)
        .def_readonly(
            "target_effects_committed",
            &BusResult::target_effects_committed)
        ;

    py::class_<DmaBeat>(m, "DmaBeat")
        .def(
            py::init([](
                    uint64_t token,
                    BusOperation operation,
                    uint64_t address,
                    uint8_t write_data,
                    std::optional<uint64_t> ready_cycle) {
                if (token == 0)
                    throw std::invalid_argument(
                        "DMA beat token must be positive");
                if (operation != BusOperation::READ &&
                    operation != BusOperation::WRITE) {
                    throw std::invalid_argument(
                        "DMA beat operation is invalid");
                }
                return DmaBeat{
                    token,
                    ready_cycle,
                    operation,
                    address,
                    write_data,
                };
            }),
            py::arg("token"),
            py::arg("operation"),
            py::arg("address"),
            py::arg("write_data") = 0,
            py::arg("ready_cycle") = std::nullopt)
        .def_readonly("token", &DmaBeat::token)
        .def_readonly(
            "ready_cycle",
            &DmaBeat::ready_cycle)
        .def_readonly("operation", &DmaBeat::operation)
        .def_readonly("address", &DmaBeat::address)
        .def_readonly("write_data", &DmaBeat::write_data)
        ;

    py::class_<DmaEndpointView>(m, "DmaEndpointView")
        .def(
            py::init<bool, std::optional<DmaBeat>>(),
            py::arg("active"),
            py::arg("pending") = std::nullopt)
        .def_readonly("active", &DmaEndpointView::active)
        .def_readonly("pending", &DmaEndpointView::pending)
        ;

    py::class_<MainBusSnapshot>(m, "MainBusSnapshot")
        .def_readonly(
            "schema_version",
            &MainBusSnapshot::schema_version)
        .def_readonly("port_count", &MainBusSnapshot::port_count)
        .def_readonly("last_grant", &MainBusSnapshot::last_grant)
        .def_readonly(
            "reset_port_zero_credit",
            &MainBusSnapshot::reset_port_zero_credit)
        .def_readonly(
            "next_grant_sequence",
            &MainBusSnapshot::next_grant_sequence)
        .def_readonly(
            "earliest_arbitration_cycle",
            &MainBusSnapshot::earliest_arbitration_cycle)
        .def_readonly("served_last", &MainBusSnapshot::served_last)
        .def_readonly(
            "last_arbitration_cycle",
            &MainBusSnapshot::last_arbitration_cycle)
        .def_readonly(
            "active_grant",
            &MainBusSnapshot::active_grant)
        .def_readonly(
            "last_issue_sequences",
            &MainBusSnapshot::last_issue_sequences)
        .def_readonly(
            "sticky_bus_errors",
            &MainBusSnapshot::sticky_bus_errors)
        ;

    py::enum_<ExternalEventKind>(m, "ExternalEventKind")
        .value("UART_RX", ExternalEventKind::UART_RX)
        .value("NIC_RX", ExternalEventKind::NIC_RX)
        .value(
            "UART_GEOMETRY",
            ExternalEventKind::UART_GEOMETRY)
        .value(
            "UART_GEOMETRY_ACCEPT",
            ExternalEventKind::UART_GEOMETRY_ACCEPT)
        .value(
            "UART_GEOMETRY_DENY",
            ExternalEventKind::UART_GEOMETRY_DENY)
        .value(
            "NIC_RX_REJECTED",
            ExternalEventKind::NIC_RX_REJECTED)
        .value(
            "UART_GEOMETRY_ACCEPT_UNCONDITIONAL",
            ExternalEventKind::
                UART_GEOMETRY_ACCEPT_UNCONDITIONAL)
        .value(
            "UART_GEOMETRY_DENY_UNCONDITIONAL",
            ExternalEventKind::
                UART_GEOMETRY_DENY_UNCONDITIONAL);

    py::enum_<ExternalEventReleasePhase>(
            m,
            "ExternalEventReleasePhase")
        .value(
            "SCHEDULER",
            ExternalEventReleasePhase::SCHEDULER)
        .value(
            "BEFORE_BATCH",
            ExternalEventReleasePhase::BEFORE_BATCH)
        .value(
            "AFTER_BATCH",
            ExternalEventReleasePhase::AFTER_BATCH);

    py::class_<ExternalEventRecord>(m, "ExternalEventRecord")
        .def_readonly("cycle", &ExternalEventRecord::cycle)
        .def_readonly("sequence", &ExternalEventRecord::sequence)
        .def_readonly("kind", &ExternalEventRecord::kind)
        .def_property_readonly(
            "payload",
            [](const ExternalEventRecord& event) {
                return py::bytes(
                    reinterpret_cast<const char*>(
                        event.payload.data()),
                    event.payload.size());
            })
        .def_readonly(
            "argument0",
            &ExternalEventRecord::argument0)
        .def_readonly(
            "argument1",
            &ExternalEventRecord::argument1)
        .def_readonly(
            "release_boundary",
            &ExternalEventRecord::release_boundary)
        .def_readonly(
            "release_phase",
            &ExternalEventRecord::release_phase)
        ;

    py::enum_<SystemStopReason>(m, "SystemStopReason")
        .value(
            "INSTRUCTION_LIMIT",
            SystemStopReason::INSTRUCTION_LIMIT)
        .value("CYCLE_LIMIT", SystemStopReason::CYCLE_LIMIT)
        .value(
            "EVENT_HORIZON",
            SystemStopReason::EVENT_HORIZON)
        .value("ALL_HALTED", SystemStopReason::ALL_HALTED)
        .value("ALL_IDLE", SystemStopReason::ALL_IDLE)
        .value(
            "UNHANDLED_INTERRUPT",
            SystemStopReason::UNHANDLED_INTERRUPT)
        .value(
            "NO_PROGRESS",
            SystemStopReason::NO_PROGRESS);

    py::class_<SystemBatchResult>(m, "SystemBatchResult")
        .def_readonly(
            "instructions_executed",
            &SystemBatchResult::instructions_executed)
        .def_readonly(
            "system_cycles_advanced",
            &SystemBatchResult::system_cycles_advanced)
        .def_readonly(
            "per_core_instructions",
            &SystemBatchResult::per_core_instructions)
        .def_readonly(
            "per_core_cycles",
            &SystemBatchResult::per_core_cycles)
        .def_readonly(
            "per_core_dispatches",
            &SystemBatchResult::per_core_dispatches)
        .def_readonly(
            "per_core_interrupts",
            &SystemBatchResult::per_core_interrupts)
        .def_readonly(
            "per_core_stop_reasons",
            &SystemBatchResult::per_core_stop_reasons)
        .def_readonly("rounds", &SystemBatchResult::rounds)
        .def_readonly(
            "continuations",
            &SystemBatchResult::continuations)
        .def_readonly(
            "interrupts_delivered",
            &SystemBatchResult::interrupts_delivered)
        .def_readonly(
            "external_events_applied",
            &SystemBatchResult::external_events_applied)
        .def_readonly(
            "scheduler_cursor",
            &SystemBatchResult::scheduler_cursor)
        .def_readonly(
            "system_stop_reason",
            &SystemBatchResult::system_stop_reason)
        .def_readonly(
            "stop_cycle",
            &SystemBatchResult::stop_cycle)
        .def_readonly(
            "event_source_mask",
            &SystemBatchResult::event_source_mask)
        .def_readonly(
            "pending_interrupt_core",
            &SystemBatchResult::pending_interrupt_core)
        .def_readonly(
            "pending_interrupt_vector",
            &SystemBatchResult::pending_interrupt_vector)
        ;

    // Native system ownership.  Borrowed core views keep their parent alive
    // and never take ownership of the pointed-to CPUState.
    py::class_<SystemState>(m, "SystemState")
        .def(
             py::init([](
                     int full_core_count,
                     int all_core_count,
                     int main_bus_port_count,
                     py::object worker_count_object) {
                 if (
                     PyBool_Check(worker_count_object.ptr()) ||
                     !PyLong_Check(worker_count_object.ptr())
                 ) {
                     throw py::type_error(
                         "worker_count must be an integer");
                 }
                 long long worker_count;
                 try {
                     worker_count =
                         worker_count_object.cast<long long>();
                 } catch (const py::cast_error&) {
                     throw py::value_error(
                         "worker_count must be exactly 1, 2, or 4");
                 }
                 if (
                     worker_count != 1 &&
                     worker_count != 2 &&
                     worker_count != 4
                 ) {
                     throw py::value_error(
                         "worker_count must be exactly 1, 2, or 4");
                 }
                 return std::make_unique<SystemState>(
                     full_core_count,
                     all_core_count,
                     main_bus_port_count,
                     static_cast<int>(worker_count));
             }),
             py::arg("full_core_count"),
             py::arg("all_core_count") = 0,
             py::arg("main_bus_port_count") = 0,
             py::arg("worker_count") = 1)
        .def_property_readonly(
            "full_core_count", &SystemState::full_core_count)
        .def_property_readonly(
            "micro_core_count", &SystemState::micro_core_count)
        .def_property_readonly(
            "all_core_count", &SystemState::all_core_count)
        .def_property_readonly(
            "worker_count", &SystemState::worker_count)
        .def(
            "_worker_pool_diagnostics",
            [](const SystemState& system) {
                const PersistentWorkerPoolSnapshot snapshot =
                    system.worker_pool_snapshot();
                py::dict result;
                result["schema_version"] = 1;
                result["worker_count"] =
                    snapshot.worker_count;
                result["auxiliary_worker_count"] =
                    snapshot.auxiliary_worker_count;
                result["live_auxiliary_workers"] =
                    snapshot.live_auxiliary_workers;
                result["launch_count"] =
                    snapshot.launch_count;
                result["inline_reference"] =
                    snapshot.inline_reference;
                return result;
            })
        .def(
            "_private_worker_diagnostics",
            [](const SystemState& system) {
                if (!system.worker_pool)
                    throw std::logic_error(
                        "native worker pool is unavailable");
                const PersistentWorkerPrivateSnapshot
                    snapshot =
                        system.worker_pool->
                            private_snapshot();
                py::dict result;
                result["schema_version"] = 1;
                result["wave_epoch"] =
                    snapshot.wave_epoch;
                result["next_command_sequence"] =
                    snapshot.next_command_sequence;
                result["wave_active"] =
                    snapshot.wave_active;
                py::list lanes;
                for (
                    const PersistentWorkerLaneSnapshot&
                        lane : snapshot.lanes
                ) {
                    py::dict lane_result;
                    lane_result["lane_index"] =
                        lane.lane_index;
                    lane_result["auxiliary"] =
                        lane.auxiliary;
                    lane_result["thread_token"] =
                        lane.thread_token;
                    lane_result["completed_commands"] =
                        lane.completed_commands;
                    lane_result["completed_steps"] =
                        lane.completed_steps;
                    lanes.append(
                        std::move(lane_result));
                }
                result["lanes"] = std::move(lanes);
                return result;
            })
        .def(
            "_start_concurrency_profile",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(
                        system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "concurrency profiling cannot start "
                        "during an active native batch");
                }
                system.concurrency_profile
                    .start_session();
                return
                    concurrency_profile_snapshot_dict(
                        system);
            })
        .def(
            "_stop_concurrency_profile",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(
                        system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "concurrency profiling cannot stop "
                        "during an active native batch");
                }
                system.concurrency_profile.enabled =
                    false;
                return
                    concurrency_profile_snapshot_dict(
                        system);
            })
        .def(
            "_concurrency_profile_snapshot",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(
                        system);
                return
                    concurrency_profile_snapshot_dict(
                        system);
            })
        .def(
            "_run_private_full_core_commands",
            [](SystemState& system,
               const py::list& command_objects) {
                auto exact_integer = [](
                        py::handle value,
                        const char* field_name) {
                    if (
                        PyBool_Check(value.ptr()) ||
                        !PyLong_Check(value.ptr())
                    ) {
                        throw py::type_error(
                            std::string(field_name) +
                            " must be an integer");
                    }
                    try {
                        return value.cast<long long>();
                    } catch (const py::cast_error&) {
                        throw py::value_error(
                            std::string(field_name) +
                            " is outside the supported range");
                    }
                };

                std::vector<PrivateCoreCommand>
                    commands;
                commands.reserve(
                    static_cast<std::size_t>(
                        command_objects.size()));
                std::vector<bool> seen_lanes(
                    static_cast<std::size_t>(
                        system.worker_count()),
                    false);
                std::vector<bool> seen_cores(
                    static_cast<std::size_t>(
                        system.full_core_count()),
                    false);
                for (py::handle item : command_objects) {
                    py::tuple command_object;
                    try {
                        command_object =
                            py::cast<py::tuple>(item);
                    } catch (const py::cast_error&) {
                        throw py::type_error(
                            "each private command must be "
                            "(lane_index, core_index, max_steps)");
                    }
                    if (command_object.size() != 3) {
                        throw py::value_error(
                            "each private command must contain "
                            "lane_index, core_index, and max_steps");
                    }
                    const long long lane_value =
                        exact_integer(
                            command_object[0],
                            "lane_index");
                    const long long core_value =
                        exact_integer(
                            command_object[1],
                            "core_index");
                    const long long steps_value =
                        exact_integer(
                            command_object[2],
                            "max_steps");
                    if (
                        lane_value < 0 ||
                        lane_value >=
                            system.worker_count()
                    ) {
                        throw py::value_error(
                            "lane_index is outside the "
                            "configured worker lanes");
                    }
                    if (
                        core_value < 0 ||
                        core_value >=
                            system.full_core_count()
                    ) {
                        throw py::value_error(
                            "core_index is outside the "
                            "full-core topology");
                    }
                    if (
                        steps_value < 0 ||
                        steps_value >
                            std::numeric_limits<int>::max()
                    ) {
                        throw py::value_error(
                            "max_steps must be between zero "
                            "and INT_MAX");
                    }
                    const std::size_t lane_index =
                        static_cast<std::size_t>(
                            lane_value);
                    const std::size_t core_index =
                        static_cast<std::size_t>(
                            core_value);
                    if (seen_lanes[lane_index]) {
                        throw py::value_error(
                            "a private wave cannot submit "
                            "two commands to one lane");
                    }
                    if (seen_cores[core_index]) {
                        throw py::value_error(
                            "a private wave cannot execute "
                            "one core twice");
                    }
                    seen_lanes[lane_index] = true;
                    seen_cores[core_index] = true;

                    PrivateCoreCommand command;
                    command.lane_index =
                        static_cast<int>(lane_value);
                    command.core_index =
                        static_cast<int>(core_value);
                    command.max_steps =
                        static_cast<int>(steps_value);
                    commands.push_back(
                        std::move(command));
                }

                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const std::vector<
                    PrivateCoreResult> native_results =
                        run_private_full_core_wave(
                            system,
                            std::move(commands));
                py::list results;
                for (
                    const PrivateCoreResult& native :
                    native_results
                ) {
                    py::dict result;
                    result["schema_version"] = 1;
                    result["command_sequence"] =
                        native.command_sequence;
                    result["wave_epoch"] =
                        native.wave_epoch;
                    result["lane_index"] =
                        native.lane_index;
                    result["core_index"] =
                        native.core_index;
                    result["thread_token"] =
                        native.thread_token;
                    result["start_pc"] =
                        native.start_pc;
                    result["end_pc"] =
                        native.end_pc;
                    result["steps_executed"] =
                        native.steps_executed;
                    result["total_cycles"] =
                        native.total_cycles;
                    result["stop_reason"] =
                        private_full_core_stop_reason_name(
                            native.stop_reason);
                    if (native.trap_id < 0) {
                        result["trap_id"] = py::none();
                    } else {
                        result["trap_id"] =
                            native.trap_id;
                    }
                    if (native.interrupt_vector < 0) {
                        result["interrupt_vector"] =
                            py::none();
                    } else {
                        result["interrupt_vector"] =
                            native.interrupt_vector;
                    }
                    results.append(std::move(result));
                }
                return results;
            },
            py::arg("commands"))
        .def_property_readonly(
            "cluster_arbiter_count",
            [](const SystemState& system) {
                return static_cast<int>(
                    system.cluster_states.size());
            })
        .def(
            "_tacc_image_stage_snapshot",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return snapshot_tacc_image_transfer_stage(
                    system);
            })
        .def(
            "_tacc_image_stage_restore",
            [](SystemState& system, const py::dict& snapshot) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const PreparedTaccImageTransferStage prepared =
                    prepare_tacc_image_transfer_stage(
                        system,
                        snapshot);
                commit_tacc_image_transfer_stage(
                    system.tacc_image_stage,
                    prepared);
                system.refresh_cycle_execution_pending();
            },
            py::arg("snapshot"))
        .def(
            "_tacc_image_stage_acquire",
            [](SystemState& system,
               int owner_core_id,
               const std::string& direction,
               uint64_t base_address,
               int format_ew,
               bool format_signed,
               uint64_t engine_epoch,
               uint64_t caller_epoch,
               const py::bytes& initial_image) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const int owner_engine_id =
                    system.tacc_engine_for_core(
                    owner_core_id);
                if (owner_core_id >= TACC_OWNER_NONE) {
                    throw std::invalid_argument(
                        "TACC image-stage owner does not fit "
                        "the architectural core-ID field");
                }
                TaccImageTransferStage::Direction parsed_direction;
                if (direction == "load") {
                    parsed_direction =
                        TaccImageTransferStage::Direction::LOAD;
                } else if (direction == "store") {
                    parsed_direction =
                        TaccImageTransferStage::Direction::STORE;
                } else {
                    throw std::invalid_argument(
                        "TACC image-stage acquisition direction "
                        "must be load or store");
                }
                if ((base_address & 0x3F) != 0) {
                    throw std::invalid_argument(
                        "TACC image-stage base address must be "
                        "64-byte aligned");
                }
                if (
                    base_address >
                    std::numeric_limits<uint64_t>::max() -
                        (TACC_IMAGE_BYTES - 1)
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage span wraps the "
                        "address space");
                }
                if (
                    format_ew != EW_U8 &&
                    format_ew != EW_U16 &&
                    format_ew != EW_U32 &&
                    format_ew != EW_FP16 &&
                    format_ew != EW_BF16
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage acquisition requires "
                        "a legal format");
                }
                if (
                    (
                        format_ew == EW_FP16 ||
                        format_ew == EW_BF16
                    ) &&
                    format_signed
                ) {
                    throw std::invalid_argument(
                        "floating TACC image-stage acquisition "
                        "cannot be signed");
                }
                const std::string image = initial_image;
                if (
                    image.size() !=
                    system.tacc_image_stage
                        .image.size()
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage initial image must "
                        "be exactly 256 bytes");
                }
                if (
                    parsed_direction ==
                        TaccImageTransferStage::Direction::LOAD &&
                    std::any_of(
                        image.begin(),
                        image.end(),
                        [](char byte) {
                            return byte != 0;
                        })
                ) {
                    throw std::invalid_argument(
                        "TACC LOAD stage must start with a zero image");
                }
                if (
                    parsed_direction ==
                        TaccImageTransferStage::Direction::STORE &&
                    format_ew != EW_U8 &&
                    format_ew != EW_U16 &&
                    std::any_of(
                        image.begin() + 128,
                        image.end(),
                        [](char byte) {
                            return byte != 0;
                        })
                ) {
                    throw std::invalid_argument(
                        "TACC STORE stage inactive image bytes "
                        "must be zero");
                }
                const uint64_t live_engine_epoch =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_epoch
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_epoch;
                if (engine_epoch != live_engine_epoch) {
                    throw std::invalid_argument(
                        "TACC image-stage acquisition uses a "
                        "stale engine epoch");
                }
                const bool live_busy =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_busy
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_busy;
                const uint8_t live_owner =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_owner
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_owner;
                const bool live_valid =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_valid
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_valid;
                const uint8_t live_format_ew =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_format_ew
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_format_ew;
                const uint8_t live_format_signed =
                    owner_engine_id <
                        system.full_core_count()
                    ? system.cores[
                          static_cast<std::size_t>(
                              owner_engine_id)]
                          ->tacc_format_signed
                    : system.cluster_states[
                          static_cast<std::size_t>(
                              owner_engine_id -
                              system.full_core_count())]
                          .tacc_format_signed;
                if (!live_busy || live_owner != owner_core_id) {
                    throw std::invalid_argument(
                        "TACC image-stage acquisition requires "
                        "the caller's BUSY owned engine");
                }
                if (
                    parsed_direction ==
                        TaccImageTransferStage::Direction::STORE &&
                    (
                        !live_valid ||
                        live_format_ew != format_ew ||
                        live_format_signed !=
                            static_cast<uint8_t>(
                                format_signed ? 1 : 0)
                    )
                ) {
                    throw std::invalid_argument(
                        "TACC STORE stage does not match valid "
                        "latched engine state");
                }
                if (
                    owner_core_id <
                        system.full_core_count() &&
                    caller_epoch != 0
                ) {
                    throw std::invalid_argument(
                        "full-core image-stage acquisition uses "
                        "caller epoch zero");
                }
                if (owner_core_id >= system.full_core_count()) {
                    ClusterState& cluster =
                        system.cluster_states[
                            static_cast<std::size_t>(
                                owner_engine_id -
                                system.full_core_count())];
                    const int local_core =
                        owner_core_id -
                        cluster.global_id_base;
                    if (
                        local_core < 0 ||
                        local_core >= cluster.core_count ||
                        cluster.tacc_caller_epochs[
                            static_cast<std::size_t>(
                                local_core)] !=
                            caller_epoch
                    ) {
                        throw std::invalid_argument(
                            "TACC image-stage acquisition uses a "
                            "stale microcaller epoch");
                    }
                }
                TaccImageTransferStage& stage =
                    system.tacc_image_stage;
                if (
                    stage.active() &&
                    !tacc_image_stage_owner_is_live(
                        system,
                        stage)
                ) {
                    // Keep direct execution excluded across stale-tenure
                    // replacement. Publishing a false pending edge between
                    // cancellation and the replacement grant would expose a
                    // lock-free admission window.
                    system.cycle_execution_pending.store(
                        true,
                        std::memory_order_release);
                    stage.cancel();
                }
                if (stage.active()) {
                    return py::make_tuple(
                        false,
                        stage.stage_epoch);
                }
                if (
                    stage.grant_sequence ==
                        std::numeric_limits<uint64_t>::max() ||
                    stage.stage_epoch ==
                        std::numeric_limits<uint64_t>::max()
                ) {
                    // A stale active tenure may have been cancelled above.
                    // Recompute the aggregate bit before failing so direct
                    // execution is not excluded by a tenure that no longer
                    // exists.
                    system.refresh_cycle_execution_pending();
                    throw std::overflow_error(
                        "TACC image-stage tenure counter overflow");
                }
                // Direct execution checks this atomic without taking the
                // scheduler mutex. Publish exclusion before direction makes
                // the new tenure visible, then publish direction last.
                system.cycle_execution_pending.store(
                    true,
                    std::memory_order_release);
                stage.stage_epoch++;
                stage.grant_sequence++;
                stage.last_grant_engine_id =
                    owner_engine_id;
                stage.owner_engine_id =
                    owner_engine_id;
                stage.owner_core_id = owner_core_id;
                stage.engine_epoch = engine_epoch;
                stage.caller_epoch = caller_epoch;
                stage.base_address = base_address;
                stage.format_ew =
                    static_cast<uint8_t>(format_ew);
                stage.format_signed = format_signed;
                std::copy(
                    image.begin(),
                    image.end(),
                    stage.image.begin());
                stage.beat_index = 0;
                stage.direction = parsed_direction;
                return py::make_tuple(
                    true,
                    stage.stage_epoch);
            },
            py::arg("owner_core_id"),
            py::arg("direction"),
            py::arg("base_address"),
            py::arg("format_ew"),
            py::arg("format_signed"),
            py::arg("engine_epoch"),
            py::arg("caller_epoch"),
            py::arg("initial_image"))
        .def(
            "_tacc_image_stage_update",
            [](SystemState& system,
               int owner_engine_id,
               int owner_core_id,
               uint64_t stage_epoch,
               uint64_t engine_epoch,
               uint64_t caller_epoch,
               int beat_index,
               const py::bytes& image_value) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (
                    system.tacc_engine_for_core(owner_core_id) !=
                    owner_engine_id
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage update owners do not map");
                }
                if (beat_index < 0 || beat_index > 4) {
                    throw std::invalid_argument(
                        "TACC image-stage beat index must be "
                        "between zero and four");
                }
                const std::string image = image_value;
                if (
                    image.size() !=
                    system.tacc_image_stage
                        .image.size()
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage image must "
                        "be exactly 256 bytes");
                }
                TaccImageTransferStage& stage =
                    system.tacc_image_stage;
                if (
                    !stage.owned_by(
                        owner_engine_id,
                        owner_core_id,
                        stage_epoch) ||
                    stage.engine_epoch != engine_epoch ||
                    stage.caller_epoch != caller_epoch
                ) {
                    return false;
                }
                if (!tacc_image_stage_owner_is_live(
                        system,
                        stage)) {
                    stage.cancel();
                    system.refresh_cycle_execution_pending();
                    return false;
                }
                if (
                    beat_index !=
                    static_cast<int>(stage.beat_index) + 1
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage updates must acknowledge "
                        "exactly the next beat");
                }
                if (
                    stage.direction ==
                        TaccImageTransferStage::Direction::STORE &&
                    std::memcmp(
                        image.data(),
                        stage.image.data(),
                        stage.image.size()) != 0
                ) {
                    throw std::invalid_argument(
                        "TACC STORE stage image is immutable "
                        "during its tenure");
                }
                const std::size_t old_prefix =
                    static_cast<std::size_t>(
                        stage.beat_index) * 64;
                if (
                    stage.direction ==
                        TaccImageTransferStage::Direction::LOAD &&
                    std::memcmp(
                        image.data(),
                        stage.image.data(),
                        old_prefix) != 0
                ) {
                    throw std::invalid_argument(
                        "TACC LOAD stage update cannot rewrite "
                        "an acknowledged prefix");
                }
                if (
                    stage.direction ==
                        TaccImageTransferStage::Direction::LOAD &&
                    beat_index < 4 &&
                    std::any_of(
                        image.begin() + beat_index * 64,
                        image.end(),
                        [](char byte) {
                            return byte != 0;
                        })
                ) {
                    throw std::invalid_argument(
                        "TACC LOAD stage update has data beyond "
                        "its acknowledged prefix");
                }
                if (
                    stage.direction ==
                        TaccImageTransferStage::Direction::LOAD &&
                    stage.format_ew != EW_U8 &&
                    stage.format_ew != EW_U16 &&
                    std::any_of(
                        image.begin() + 128,
                        image.end(),
                        [](char byte) {
                            return byte != 0;
                        })
                ) {
                    throw std::invalid_argument(
                        "inactive TACC LOAD image bytes must "
                        "remain zero");
                }
                std::copy(
                    image.begin(),
                    image.end(),
                    stage.image.begin());
                stage.beat_index =
                    static_cast<uint8_t>(beat_index);
                return true;
            },
            py::arg("owner_engine_id"),
            py::arg("owner_core_id"),
            py::arg("stage_epoch"),
            py::arg("engine_epoch"),
            py::arg("caller_epoch"),
            py::arg("beat_index"),
            py::arg("image"))
        .def(
            "_tacc_image_stage_release",
            [](SystemState& system,
               int owner_engine_id,
               int owner_core_id,
               uint64_t stage_epoch,
               uint64_t engine_epoch,
               uint64_t caller_epoch) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (
                    system.tacc_engine_for_core(owner_core_id) !=
                    owner_engine_id
                ) {
                    throw std::invalid_argument(
                        "TACC image-stage release owners do not map");
                }
                TaccImageTransferStage& stage =
                    system.tacc_image_stage;
                if (
                    !stage.owned_by(
                        owner_engine_id,
                        owner_core_id,
                        stage_epoch) ||
                    stage.engine_epoch != engine_epoch ||
                    stage.caller_epoch != caller_epoch
                ) {
                    return false;
                }
                if (!tacc_image_stage_owner_is_live(
                        system,
                        stage)) {
                    stage.cancel();
                    system.refresh_cycle_execution_pending();
                    return false;
                }
                stage.clear_active(false);
                system.refresh_cycle_execution_pending();
                return true;
            },
            py::arg("owner_engine_id"),
            py::arg("owner_core_id"),
            py::arg("stage_epoch"),
            py::arg("engine_epoch"),
            py::arg("caller_epoch"))
        .def(
            "_cancel_tacc_image_stage_for_core",
            [](SystemState& system, int core_id) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                system.cancel_tacc_image_stage_for_core(
                    core_id);
            },
            py::arg("core_id"))
        .def(
            "reset_cluster_arbitration",
            [](SystemState& system, int cluster_index) {
                // External callers wait for a native batch; a guest MMIO
                // callback may reset cluster resources reentrantly at its
                // current instruction boundary.
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                checked_cluster_state(
                    system,
                    cluster_index).reset_arbitration();
            },
            py::arg("cluster_index"))
        .def(
            "reset_cluster_state",
            [](SystemState& system, int cluster_index) {
                // See reset_cluster_arbitration: the recursive scheduler
                // mutex is the serialization boundary for CLUSTER_EN writes.
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                system.cancel_tacc_image_stage_for_cluster(
                    cluster_index);
                checked_cluster_state(
                    system,
                    cluster_index).reset();
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_tacc_caller_epochs_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                py::list epochs;
                for (int index = 0;
                     index < cluster.core_count;
                     index++) {
                    epochs.append(
                        cluster.tacc_caller_epochs[
                            static_cast<std::size_t>(
                                index)]);
                }
                return epochs;
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_tacc_caller_epochs_restore",
            [](SystemState& system,
               int cluster_index,
               const py::sequence& values) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    values.size() !=
                    static_cast<py::size_t>(
                        cluster.core_count)
                ) {
                    throw std::invalid_argument(
                        "cluster TACC caller-epoch image must "
                        "contain one value per microcore");
                }
                std::array<uint64_t, 4> prepared =
                    cluster.tacc_caller_epochs;
                for (int index = 0;
                     index < cluster.core_count;
                     index++) {
                    const py::handle value =
                        values[
                            static_cast<py::ssize_t>(
                                index)];
                    if (
                        PyBool_Check(value.ptr()) ||
                        !PyLong_Check(value.ptr())
                    ) {
                        throw std::invalid_argument(
                            "cluster TACC caller epochs must "
                            "be unsigned 64-bit integers");
                    }
                    prepared[
                        static_cast<std::size_t>(index)] =
                        value.cast<uint64_t>();
                }
                const TaccImageTransferStage& stage =
                    system.tacc_image_stage;
                if (
                    stage.active() &&
                    stage.owner_engine_id ==
                        system.full_core_count() +
                            cluster_index
                ) {
                    const int local =
                        stage.owner_core_id -
                        cluster.global_id_base;
                    if (
                        local < 0 ||
                        local >= cluster.core_count ||
                        prepared[
                            static_cast<std::size_t>(
                                local)] !=
                            stage.caller_epoch
                    ) {
                        throw std::invalid_argument(
                            "cluster caller-epoch restore would "
                            "invalidate the active image stage");
                    }
                }
                cluster.tacc_caller_epochs = prepared;
            },
            py::arg("cluster_index"),
            py::arg("epochs"))
        .def(
            "_cluster_tacc_cancel_caller",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    local_core < 0 ||
                    local_core >= cluster.core_count
                ) {
                    throw std::out_of_range(
                        "cluster TACC caller is out of range");
                }
                cluster.tacc_caller_epochs[
                    static_cast<std::size_t>(
                        local_core)]++;
                system.cancel_tacc_image_stage_for_core(
                    cluster.global_id_base +
                        local_core);
                return cluster.tacc_caller_epochs[
                    static_cast<std::size_t>(
                        local_core)];
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_crc_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                py::dict snapshot;
                snapshot["acc"] = cluster.crc_acc;
                snapshot["mode"] = cluster.crc_mode;
                snapshot["locked"] = cluster.crc_locked;
                if (cluster.crc_lock_owner < 0) {
                    snapshot["owner"] = py::none();
                } else {
                    snapshot["owner"] =
                        cluster.crc_lock_owner;
                }
                return snapshot;
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_crc_update",
            [](SystemState& system,
               int cluster_index,
               const py::dict& changes) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                uint64_t next_acc = cluster.crc_acc;
                int next_mode = cluster.crc_mode;
                bool next_locked = cluster.crc_locked;
                int next_owner = cluster.crc_lock_owner;
                for (auto item : changes) {
                    const std::string field =
                        py::cast<std::string>(item.first);
                    py::handle value = item.second;
                    if (field == "acc") {
                        next_acc =
                            py::cast<uint64_t>(value);
                    } else if (field == "mode") {
                        const int mode =
                            py::cast<int>(value);
                        if (mode < 0 || mode > 2) {
                            throw std::invalid_argument(
                                "cluster CRC mode must be 0, 1, or 2");
                        }
                        next_mode = mode;
                    } else if (field == "locked") {
                        next_locked =
                            py::cast<bool>(value);
                    } else if (field == "owner") {
                        if (value.is_none()) {
                            next_owner = -1;
                        } else {
                            const int owner =
                                py::cast<int>(value);
                            if (
                                owner < 0 ||
                                owner >= cluster.core_count
                            ) {
                                throw std::out_of_range(
                                    "cluster CRC owner is out of range");
                            }
                            next_owner = owner;
                        }
                    } else {
                        throw std::invalid_argument(
                            "unknown cluster CRC state field");
                    }
                }
                const bool owner_is_valid =
                    next_owner >= 0 &&
                    next_owner < cluster.core_count;
                if (next_locked != owner_is_valid) {
                    throw std::invalid_argument(
                        "cluster CRC lock and owner must change atomically");
                }
                cluster.crc_acc = next_acc;
                cluster.crc_mode = next_mode;
                cluster.crc_locked = next_locked;
                cluster.crc_lock_owner = next_owner;
            },
            py::arg("cluster_index"),
            py::arg("changes"))
        .def(
            "_cluster_crc_try_acquire",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    local_core < 0 ||
                    local_core >= cluster.core_count
                ) {
                    return false;
                }
                if (!cluster.crc_locked) {
                    cluster.crc_locked = true;
                    cluster.crc_lock_owner =
                        local_core;
                }
                return (
                    cluster.crc_lock_owner ==
                    local_core
                );
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_crc_is_owner",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                return (
                    cluster.crc_locked &&
                    cluster.crc_lock_owner ==
                        local_core
                );
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_crc_release",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    cluster.crc_locked &&
                    cluster.crc_lock_owner ==
                        local_core
                ) {
                    cluster.crc_locked = false;
                    cluster.crc_lock_owner = -1;
                }
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_sha_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                py::dict snapshot;
                snapshot["locked"] = cluster.sha_locked;
                if (cluster.sha_lock_owner < 0) {
                    snapshot["owner"] = py::none();
                } else {
                    snapshot["owner"] =
                        cluster.sha_lock_owner;
                }
                return snapshot;
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_sha_try_acquire",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    local_core < 0 ||
                    local_core >= cluster.core_count
                ) {
                    return false;
                }
                if (!cluster.sha_locked) {
                    cluster.sha_locked = true;
                    cluster.sha_lock_owner =
                        local_core;
                }
                return (
                    cluster.sha_lock_owner ==
                    local_core
                );
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_sha_is_owner",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                return (
                    cluster.sha_locked &&
                    cluster.sha_lock_owner ==
                        local_core
                );
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_sha_release",
            [](SystemState& system,
               int cluster_index,
               int local_core) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                if (
                    cluster.sha_locked &&
                    cluster.sha_lock_owner ==
                        local_core
                ) {
                    cluster.sha_locked = false;
                    cluster.sha_lock_owner = -1;
                }
            },
            py::arg("cluster_index"),
            py::arg("local_core"))
        .def(
            "_cluster_spad_read8",
            [](SystemState& system,
               int cluster_index,
               uint64_t offset) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                return cluster.scratchpad[
                    static_cast<std::size_t>(
                        offset %
                        cluster.scratchpad.size())];
            },
            py::arg("cluster_index"),
            py::arg("offset"))
        .def(
            "_cluster_spad_write8",
            [](SystemState& system,
               int cluster_index,
               uint64_t offset,
               uint8_t value) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                cluster.scratchpad[
                    static_cast<std::size_t>(
                        offset %
                        cluster.scratchpad.size())] =
                    value;
            },
            py::arg("cluster_index"),
            py::arg("offset"),
            py::arg("value"))
        .def(
            "_cluster_spad_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                return py::bytes(
                    reinterpret_cast<const char*>(
                        cluster.scratchpad.data()),
                    cluster.scratchpad.size());
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_spad_restore",
            [](SystemState& system,
               int cluster_index,
               const py::bytes& image) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                const std::string bytes = image;
                if (bytes.size() !=
                    cluster.scratchpad.size()) {
                    throw std::invalid_argument(
                        "cluster scratchpad image must be exactly 1024 bytes");
                }
                std::copy(
                    bytes.begin(),
                    bytes.end(),
                    cluster.scratchpad.begin());
            },
            py::arg("cluster_index"),
            py::arg("image"))
        .def(
            "_cluster_tile_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                py::dict state =
                    snapshot_tacc_state(cluster);
                state["acc"] = cluster.acc;
                state["sha_mode"] = cluster.sha_mode;
                state["sha_msglen_lo"] =
                    cluster.sha_msglen_lo;
                state["sha_msglen_hi"] =
                    cluster.sha_msglen_hi;
                return state;
            },
            py::arg("cluster_index"))
        .def(
            "_cluster_tile_update",
            [](SystemState& system,
               int cluster_index,
               const py::dict& state) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                validate_exact_snapshot_schema(
                    state,
                    CLUSTER_TILE_SNAPSHOT_FIELDS,
                    "cluster tile-engine");
                const PreparedTaccState tacc =
                    prepare_tacc_state(state);
                if (
                    tacc.owner != TACC_OWNER_NONE &&
                    (
                        tacc.owner < cluster.global_id_base ||
                        tacc.owner >=
                            cluster.global_id_base +
                                cluster.core_count
                    )
                ) {
                    throw std::invalid_argument(
                        "cluster TACC owner is outside the cluster's "
                        "absolute core-ID domain");
                }
                const py::handle acc_value =
                    snapshot_field(state, "acc");
                if (
                    !py::isinstance<py::sequence>(
                        acc_value) ||
                    py::isinstance<py::str>(acc_value) ||
                    py::isinstance<py::bytes>(acc_value)
                ) {
                    throw std::invalid_argument(
                        "cluster ACC must be a four-element sequence");
                }
                const py::sequence acc_sequence =
                    py::reinterpret_borrow<py::sequence>(
                        acc_value);
                if (acc_sequence.size() != 4) {
                    throw std::invalid_argument(
                        "cluster ACC must contain exactly four words");
                }
                std::array<uint64_t, 4> acc{};
                for (std::size_t index = 0;
                     index < acc.size();
                     index++) {
                    const py::handle word =
                        acc_sequence[
                            static_cast<py::ssize_t>(
                                index)];
                    if (
                        PyBool_Check(word.ptr()) ||
                        !PyLong_Check(word.ptr())
                    ) {
                        throw std::invalid_argument(
                            "cluster ACC words must be unsigned "
                            "64-bit integers");
                    }
                    acc[index] = word.cast<uint64_t>();
                }
                const int sha_mode =
                    snapshot_int(state, "sha_mode");
                if (sha_mode < 0 || sha_mode > 3) {
                    throw std::invalid_argument(
                        "cluster SHA mode must fit its two-bit field");
                }
                const uint64_t sha_msglen_lo =
                    snapshot_u64(
                        state,
                        "sha_msglen_lo");
                const uint64_t sha_msglen_hi =
                    snapshot_u64(
                        state,
                        "sha_msglen_hi");
                cluster.acc = acc;
                commit_tacc_state(cluster, tacc);
                cluster.sha_mode = sha_mode;
                cluster.sha_msglen_lo = sha_msglen_lo;
                cluster.sha_msglen_hi = sha_msglen_hi;
            },
            py::arg("cluster_index"),
            py::arg("state"))
        .def(
            "_cluster_arbiter_snapshot",
            [](SystemState& system, int cluster_index) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const ClusterState& cluster =
                    checked_cluster_state(
                        system,
                        cluster_index);
                py::dict last_grants;
                py::dict grant_counts;
                for (
                    std::size_t resource_index = 1;
                    resource_index <
                        CLUSTER_RESOURCE_KIND_COUNT;
                    resource_index++
                ) {
                    const ClusterResourceKind resource =
                        static_cast<ClusterResourceKind>(
                            resource_index);
                    const char* name =
                        cluster_resource_name(resource);
                    last_grants[name] =
                        cluster.last_grants[resource_index];
                    grant_counts[name] =
                        cluster.grant_counts[resource_index];
                }
                py::dict snapshot;
                snapshot["schema_version"] = 2;
                snapshot["cluster_id"] =
                    cluster.cluster_id;
                snapshot["global_id_base"] =
                    cluster.global_id_base;
                snapshot["core_count"] =
                    cluster.core_count;
                snapshot["last_grants"] =
                    last_grants;
                snapshot["grant_counts"] =
                    grant_counts;
                snapshot["grant_sequence"] =
                    cluster.grant_sequence;
                snapshot["crc_locked"] =
                    cluster.crc_locked;
                snapshot["crc_lock_owner"] =
                    cluster.crc_lock_owner;
                snapshot["sha_locked"] =
                    cluster.sha_locked;
                snapshot["sha_lock_owner"] =
                    cluster.sha_lock_owner;
                return snapshot;
            },
            py::arg("cluster_index"))
        .def_property_readonly_static(
            "NIC_DMA_REQUESTER_ID",
            [](py::object) {
                return SystemState::NIC_DMA_REQUESTER_ID;
            })
        .def_property_readonly_static(
            "DISK_DMA_REQUESTER_ID",
            [](py::object) {
                return SystemState::DISK_DMA_REQUESTER_ID;
            })
        .def_property_readonly_static(
            "MAIN_BUS_MMIO_TIMEOUT_CYCLES",
            [](py::object) {
                return MainBusArbiter::MMIO_TIMEOUT_CYCLES;
            })
        .def_property_readonly_static(
            "MAIN_BUS_MEMORY_TIMEOUT_CYCLES",
            [](py::object) {
                return MainBusArbiter::MEMORY_TIMEOUT_CYCLES;
            })
        .def_property_readonly_static(
            "MAIN_BUS_TIMEOUT_SENTINEL",
            [](py::object) {
                return MainBusArbiter::TIMEOUT_SENTINEL;
            })
        .def_property_readonly(
            "main_bus_port_count",
            [](const SystemState& system) {
                return system.main_bus.port_count();
            })
        .def(
            "main_bus_port_for_requester",
            &SystemState::main_bus_port_for_requester,
            py::arg("requester_id"))
        .def(
            "_main_bus_try_grant",
            [](SystemState& system,
               const std::vector<BusRequest>& pending) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "main bus state cannot change during an active "
                        "native system batch");
                }
                if (system.has_cycle_execution_pending()) {
                    throw std::runtime_error(
                        "main bus state cannot change while cycle "
                        "execution is suspended");
                }
                for (const BusRequest& request : pending)
                    system.validate_main_bus_request(request);
                return system.main_bus.try_grant(
                    pending, system.shared_clock.cycles());
            },
            py::arg("pending"))
        .def(
            "_main_bus_next_arbitration_cycle",
            [](SystemState& system,
               const std::vector<BusRequest>& pending) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                for (const BusRequest& request : pending)
                    system.validate_main_bus_request(request);
                return system.main_bus.next_arbitration_cycle(
                    pending, system.shared_clock.cycles());
            },
            py::arg("pending"))
        .def(
            "_main_bus_complete",
            [](SystemState& system,
               uint64_t grant_sequence,
               std::optional<uint64_t> read_value,
               BusFault fault,
               bool target_effects_committed) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "main bus state cannot change during an active "
                        "native system batch");
                }
                if (system.has_cycle_execution_pending()) {
                    throw std::runtime_error(
                        "main bus state cannot change while cycle "
                        "execution is suspended");
                }
                return system.main_bus.complete(
                    grant_sequence,
                    system.shared_clock.cycles(),
                    read_value,
                    fault,
                    target_effects_committed);
            },
            py::arg("grant_sequence"),
            py::arg("read_value") = py::none(),
            py::arg("fault") = BusFault::NONE,
            py::arg("target_effects_committed") = false)
        .def(
            "_main_bus_snapshot",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.main_bus.snapshot();
            })
        .def(
            "_main_bus_reset",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "main bus state cannot change during an active "
                        "native system batch");
                }
                if (system.has_cycle_execution_pending()) {
                    throw std::runtime_error(
                        "main bus state cannot change while cycle "
                        "execution is suspended");
                }
                system.main_bus.reset(
                    system.shared_clock.cycles());
            })
        .def(
            "_reset_cycle_execution",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "cycle execution cannot reset during an active "
                        "native system batch");
                }
                system.reset_cycle_execution();
            })
        .def(
            "_reset_cycle_execution_and_main_bus",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "cycle timeline cannot reset during an active "
                        "native system batch");
                }
                // Cancel the coordinator target and its endpoint cache before
                // releasing the captured fabric grant. Device reset follows
                // only after this atomic native boundary returns.
                system.reset_cycle_execution();
                system.main_bus.reset(
                    system.shared_clock.cycles());
            })
        .def(
            "_adopt_native_nic_cycle_dma",
            [](SystemState& system) {
                std::vector<DmaEndpointCallbacks> callbacks;
                callbacks.reserve(
                    system.dma_cycle_states.size());
                for (const DmaCycleState& state :
                     system.dma_cycle_states) {
                    if (state.requester_id ==
                        SystemState::NIC_DMA_REQUESTER_ID) {
                        callbacks.push_back(
                            build_native_nic_dma_callbacks(
                                system));
                    } else {
                        callbacks.push_back(
                            build_inactive_dma_callbacks(
                                state.requester_id));
                    }
                }

                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "native NIC DMA cannot be adopted during an "
                        "active native system batch");
                }
                if (system.has_cycle_execution_pending() ||
                    system.main_bus.snapshot()
                        .active_grant.has_value()) {
                    throw std::runtime_error(
                        "native NIC DMA adoption requires a clean "
                        "cycle timeline");
                }
                refresh_cycle_dma_requests(
                    system,
                    callbacks,
                    system.shared_clock.cycles());
            })
        .def(
            "_require_storage_mutation_allowed",
            [](SystemState& system) {
                require_cycle_device_mutation_allowed(
                    *system.cores.front(),
                    "storage");
            })
        .def(
            "_require_storage_stall_release_allowed",
            [](SystemState& system) {
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "storage stall release cannot mutate during an "
                        "active native system batch");
                }
            })
        .def_property_readonly(
            "cycle_execution_pending",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.has_cycle_execution_pending();
            })
        .def(
            "_cycle_pending_bus_requests",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return collect_cycle_bus_requests(system);
            })
        .def(
            "_cycle_dma_snapshot",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                py::list endpoints;
                for (const DmaCycleState& state :
                     system.dma_cycle_states) {
                    py::dict endpoint;
                    endpoint["requester_id"] =
                        state.requester_id;
                    endpoint["main_bus_port_id"] =
                        system.main_bus_port_for_requester(
                            state.requester_id);
                    endpoint["next_issue_sequence"] =
                        state.next_issue_sequence;
                    endpoint["highest_observed_token"] =
                        state.highest_observed_token;
                    endpoint["timeline_active"] =
                        state.timeline_active;
                    if (state.pending_token.has_value()) {
                        endpoint["pending_token"] =
                            *state.pending_token;
                    } else {
                        endpoint["pending_token"] =
                            py::none();
                    }
                    if (state.pending_request.has_value()) {
                        endpoint["pending_request"] =
                            py::cast(*state.pending_request);
                    } else {
                        endpoint["pending_request"] =
                            py::none();
                    }
                    endpoints.append(endpoint);
                }
                py::dict snapshot;
                snapshot["schema_version"] = 1;
                snapshot["endpoints"] = endpoints;
                return snapshot;
            })
        .def_property(
            "scheduler_cursor",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.scheduler_cursor;
            },
            [](SystemState& system, int cursor) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (cursor < 0 ||
                    cursor >= system.all_core_count()) {
                    throw std::out_of_range(
                        "scheduler cursor is outside the advertised topology");
                }
                if (system.has_cycle_execution_pending()) {
                    throw std::runtime_error(
                        "scheduler cursor cannot change while cycle "
                        "execution is suspended");
                }
                system.scheduler_cursor = cursor;
            })
        .def_property_readonly(
            "native_batch_runs",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.native_batch_runs;
            })
        .def_property_readonly(
            "native_dispatches",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.native_dispatches;
            })
        .def_property_readonly(
            "native_batch_active",
            [](const SystemState& system) {
                return system.native_batch_active.load(
                    std::memory_order_acquire);
            })
        .def_property_readonly_static(
            "EVENT_TIMER",
            [](py::object) { return SystemClock::EVENT_TIMER; })
        .def_property_readonly_static(
            "EVENT_FRAMEBUFFER",
            [](py::object) {
                return SystemClock::EVENT_FRAMEBUFFER;
            })
        .def_property_readonly_static(
            "EVENT_RTC",
            [](py::object) { return SystemClock::EVENT_RTC; })
        .def_property_readonly_static(
            "EVENT_INTERRUPT",
            [](py::object) {
                return SystemClock::EVENT_INTERRUPT;
            })
        .def_property_readonly_static(
            "EVENT_EXTERNAL",
            [](py::object) {
                return SystemClock::EVENT_EXTERNAL;
            })
        .def_property_readonly_static(
            "EVENT_SOURCE_COUNT",
            [](py::object) {
                return SystemClock::EVENT_SOURCE_COUNT;
            })
        .def_property_readonly(
            "system_cycles",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.shared_clock.cycles();
            })
        .def_property_readonly(
            "main_bus_timeout_cycle",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.main_bus.active_timeout_cycle();
            })
        .def(
            "advance_system_cycles",
            [](SystemState& system, uint64_t delta) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.has_cycle_execution_pending() &&
                    !system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "system time cannot advance while cycle "
                        "execution is suspended");
                }
                const uint64_t current =
                    system.shared_clock.cycles();
                if (delta >
                    std::numeric_limits<uint64_t>::max() - current) {
                    throw std::overflow_error(
                        "system cycle counter overflow");
                }
                const uint64_t target = current + delta;
                const std::optional<uint64_t> external_cycle =
                    system.external_events.next_cycle();
                const std::optional<uint64_t> before_cycle =
                    system.external_events.next_before_cycle();
                if (!system.native_batch_active.load(
                        std::memory_order_acquire) &&
                    delta != 0 &&
                    external_cycle.has_value() &&
                    *external_cycle <= target) {
                    throw std::runtime_error(
                        "system time cannot cross a pending external "
                        "event");
                }
                if (!system.native_batch_active.load(
                        std::memory_order_acquire) &&
                    delta != 0 &&
                    before_cycle.has_value() &&
                    *before_cycle < target) {
                    throw std::runtime_error(
                        "system time cannot cross replayed pre-batch "
                        "ingress");
                }
                auto memory_guard =
                    acquire_system_clock_advance_use(system);
                system.main_bus.validate_clock_target(
                    target);
                system.shared_clock.advance_by(
                    delta,
                    system.shared_timer,
                    system.shared_fb,
                    system.shared_rtc);
            },
            py::arg("delta"))
        .def(
            "advance_system_to",
            [](SystemState& system, uint64_t target) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.has_cycle_execution_pending() &&
                    !system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "system time cannot advance while cycle "
                        "execution is suspended");
                }
                const uint64_t current =
                    system.shared_clock.cycles();
                const std::optional<uint64_t> external_cycle =
                    system.external_events.next_cycle();
                const std::optional<uint64_t> before_cycle =
                    system.external_events.next_before_cycle();
                if (!system.native_batch_active.load(
                        std::memory_order_acquire) &&
                    target > current &&
                    external_cycle.has_value() &&
                    *external_cycle <= target) {
                    throw std::runtime_error(
                        "system time cannot cross a pending external "
                        "event");
                }
                if (!system.native_batch_active.load(
                        std::memory_order_acquire) &&
                    target > current &&
                    before_cycle.has_value() &&
                    *before_cycle < target) {
                    throw std::runtime_error(
                        "system time cannot cross replayed pre-batch "
                        "ingress");
                }
                auto memory_guard =
                    acquire_system_clock_advance_use(system);
                system.main_bus.validate_clock_target(target);
                system.shared_clock.advance_to(
                    target,
                    system.shared_timer,
                    system.shared_fb,
                    system.shared_rtc);
            },
            py::arg("target"))
        .def(
            "set_event_deadline",
            [](SystemState& system, int source_id, uint64_t deadline) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "event deadlines cannot change during an active "
                        "native system batch");
                }
                system.shared_clock.set_event_deadline(
                    source_id, deadline);
            },
            py::arg("source_id"),
            py::arg("deadline"))
        .def(
            "clear_event_deadline",
            [](SystemState& system, int source_id) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "event deadlines cannot change during an active "
                        "native system batch");
                }
                system.shared_clock.clear_event_deadline(source_id);
            },
            py::arg("source_id"))
        .def(
            "event_horizon",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const auto snapshot = system.shared_clock.snapshot();
                py::object deadline = snapshot.has_deadline
                    ? py::cast(snapshot.earliest_deadline)
                    : py::none();
                return py::make_tuple(
                    snapshot.cycles,
                    deadline,
                    snapshot.source_mask);
            })
        .def(
            "system_clock_snapshot",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                const auto snapshot = system.shared_clock.snapshot();
                py::tuple deadlines(SystemClock::EVENT_SOURCE_COUNT);
                for (int source_id = 0;
                     source_id < SystemClock::EVENT_SOURCE_COUNT;
                     source_id++) {
                    const uint64_t source_bit =
                        uint64_t{1} << source_id;
                    if (snapshot.active_sources & source_bit) {
                        deadlines[source_id] = py::cast(
                            snapshot.deadlines[
                                static_cast<std::size_t>(source_id)]);
                    } else {
                        deadlines[source_id] = py::none();
                    }
                }
                py::object earliest = snapshot.has_deadline
                    ? py::cast(snapshot.earliest_deadline)
                    : py::none();
                return py::make_tuple(
                    snapshot.cycles,
                    deadlines,
                    earliest,
                    snapshot.source_mask);
            })
        .def(
            "_schedule_external_event",
            [](SystemState& system,
               ExternalEventKind kind,
               uint64_t event_cycle,
               py::bytes payload,
               uint64_t argument0,
               uint64_t argument1) {
                const std::string bytes = payload;
                std::vector<uint8_t> immutable_payload(
                    bytes.begin(),
                    bytes.end());
                validate_external_event(
                    kind,
                    immutable_payload,
                    argument0,
                    argument1);
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "exact external input cannot be scheduled during "
                        "an active native system batch");
                }
                return system.external_events.enqueue(
                    system.shared_clock.cycles(),
                    event_cycle,
                    kind,
                    std::move(immutable_payload),
                    argument0,
                    argument1);
            },
            py::arg("kind"),
            py::arg("event_cycle"),
            py::arg("payload") = py::bytes(),
            py::arg("argument0") = 0,
            py::arg("argument1") = 0)
        .def(
            "_install_external_event_replay",
            [](SystemState& system,
               py::list replay_records) {
                std::vector<ExternalEventRecord> records;
                records.reserve(
                    static_cast<std::size_t>(
                        replay_records.size()));
                for (py::handle item : replay_records) {
                    py::tuple fields =
                        py::cast<py::tuple>(item);
                    if (fields.size() != 8) {
                        throw std::invalid_argument(
                            "each external replay record must contain "
                            "cycle, sequence, kind, payload, argument0, "
                            "argument1, release_boundary, and "
                            "release_phase");
                    }
                    const uint64_t cycle =
                        fields[0].cast<uint64_t>();
                    const uint64_t sequence =
                        fields[1].cast<uint64_t>();
                    const ExternalEventKind kind =
                        fields[2].cast<ExternalEventKind>();
                    const std::string bytes =
                        fields[3].cast<py::bytes>();
                    std::vector<uint8_t> payload(
                        bytes.begin(),
                        bytes.end());
                    const uint64_t argument0 =
                        fields[4].cast<uint64_t>();
                    const uint64_t argument1 =
                        fields[5].cast<uint64_t>();
                    const uint64_t release_boundary =
                        fields[6].cast<uint64_t>();
                    const ExternalEventReleasePhase
                        release_phase =
                            fields[7].cast<
                                ExternalEventReleasePhase>();
                    validate_external_event(
                        kind,
                        payload,
                        argument0,
                        argument1);
                    records.push_back(
                        ExternalEventRecord{
                            cycle,
                            sequence,
                            kind,
                            std::move(payload),
                            argument0,
                            argument1,
                            release_boundary,
                            release_phase,
                        });
                }

                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "external event replay cannot install during "
                        "an active native system batch");
                }
                if (
                    system.has_cycle_execution_pending() ||
                    system.main_bus.snapshot()
                        .active_grant.has_value()
                ) {
                    throw std::runtime_error(
                        "external event replay requires a clean "
                        "cycle timeline");
                }
                system.external_events.install_replay(
                    system.shared_clock.cycles(),
                    records);
                return static_cast<uint64_t>(
                    records.size());
            },
            py::arg("replay_records"))
        .def(
            "_release_external_events_before_batch",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "pre-batch external input cannot release during "
                        "an active native system batch");
                }
                const uint64_t cycle =
                    system.shared_clock.cycles();
                const uint64_t released =
                    system.external_events
                        .release_before_next_batch(cycle);
                if (released == 0)
                    return uint64_t{0};
                return apply_due_external_events(
                    system,
                    cycle);
            })
        .def(
            "_begin_external_event_staging",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "external input staging cannot open during an "
                        "active native system batch");
                }
                const uint64_t cycle =
                    system.shared_clock.cycles();
                const uint64_t released =
                    system.external_events.begin_staging(cycle);
                if (released == 0)
                    return uint64_t{0};
                return apply_due_external_events(
                    system,
                    cycle);
            })
        .def(
            "_try_stage_external_event",
            [](SystemState& system,
               ExternalEventKind kind,
               py::bytes payload,
               uint64_t argument0,
               uint64_t argument1) {
                const std::string bytes = payload;
                std::vector<uint8_t> immutable_payload(
                    bytes.begin(),
                    bytes.end());
                validate_external_event(
                    kind,
                    immutable_payload,
                    argument0,
                    argument1);
                // This path intentionally avoids the scheduler lock. The
                // inbox gate and mutex make the staging decision atomic with
                // the execution owner's close-and-flush transition.
                return system.external_events.try_stage(
                    kind,
                    std::move(immutable_payload),
                    argument0,
                    argument1);
            },
            py::arg("kind"),
            py::arg("payload") = py::bytes(),
            py::arg("argument0") = 0,
            py::arg("argument1") = 0)
        .def(
            "_close_external_event_staging",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "staged external input cannot flush during an "
                        "active native system batch");
                }
                const uint64_t cycle =
                    system.shared_clock.cycles();
                const uint64_t staged =
                    system.external_events.close_staging(cycle);
                if (staged == 0)
                    return uint64_t{0};
                return apply_due_external_events(
                    system,
                    cycle);
            })
        .def(
            "_apply_due_external_events",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                if (system.native_batch_active.load(
                        std::memory_order_acquire)) {
                    throw std::runtime_error(
                        "external input cannot be applied during an "
                        "active native system batch");
                }
                return apply_due_external_events(
                    system,
                    system.shared_clock.cycles());
            })
        .def_property_readonly(
            "external_event_pending",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events.pending_snapshot();
            })
        .def_property_readonly(
            "external_event_history",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events.history_snapshot();
            })
        .def_property_readonly(
            "external_event_next_cycle",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events.next_cycle();
            })
        .def_property_readonly(
            "external_event_next_sequence",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events.next_sequence();
            })
        .def_property_readonly(
            "external_event_batch_boundaries",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events
                    .completed_staging_boundaries();
            })
        .def_property_readonly(
            "external_event_next_before_cycle",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events
                    .next_before_cycle();
            })
        .def_property_readonly(
            "external_event_replay_sealed",
            [](SystemState& system) {
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return system.external_events.replay_sealed();
            })
        .def(
            "run_full_core_batch",
            [](SystemState& system,
               int64_t max_steps,
               py::list callback_sets,
               py::function prepare_batch,
               py::function settle_continuation,
               py::function settle_dispatch_error,
               py::function settle_round,
               int max_dispatch_steps) {
                std::vector<StepCallbacks> callbacks =
                    build_system_step_callbacks(
                        system,
                        callback_sets,
                        system.execution_cores.size(),
                        "execution");

                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return run_native_system_batch(
                    system,
                    max_steps,
                    callbacks,
                    prepare_batch,
                    settle_continuation,
                    settle_dispatch_error,
                    settle_round,
                    max_dispatch_steps);
            },
            py::arg("max_steps"),
            py::arg("callback_sets"),
            py::arg("prepare_batch"),
            py::arg("settle_continuation"),
            py::arg("settle_dispatch_error"),
            py::arg("settle_round"),
            py::arg("max_dispatch_steps") = 1000)
        .def(
            "run_full_core_cycle_batch",
            [](SystemState& system,
               uint64_t max_system_cycles,
               py::list callback_sets,
               py::list dma_callback_sets,
               py::function prepare_batch,
               py::function settle_continuation,
               py::function settle_round,
               int64_t max_instructions) {
                std::vector<StepCallbacks> callbacks =
                    build_system_step_callbacks(
                        system,
                        callback_sets,
                        system.cores.size(),
                        "full");
                std::vector<DmaEndpointCallbacks> dma_callbacks =
                    build_system_dma_callbacks(
                        system,
                        dma_callback_sets);
                auto scheduler_guard =
                    acquire_system_scheduler_lock(system);
                return run_full_core_cycle_batch(
                    system,
                    max_system_cycles,
                    max_instructions,
                    callbacks,
                    dma_callbacks,
                    prepare_batch,
                    settle_continuation,
                    settle_round);
            },
            py::arg("max_system_cycles"),
            py::arg("callback_sets"),
            py::arg("dma_callback_sets"),
            py::arg("prepare_batch"),
            py::arg("settle_continuation"),
            py::arg("settle_round"),
            py::arg("max_instructions") = 100000)
        .def(
            "ipi_send",
            [](SystemState& system, int requester_id, int target_id) {
                return system.shared_interrupts.send_ipi(
                    requester_id, target_id);
            },
            py::arg("requester_id"),
            py::arg("target_id"))
        .def(
            "ipi_ack",
            [](SystemState& system, int target_id, int source_id) {
                return system.shared_interrupts.acknowledge_ipi(
                    target_id, source_id);
            },
            py::arg("target_id"),
            py::arg("source_id"))
        .def(
            "ipi_pending_mask",
            [](const SystemState& system, int target_id) {
                return system.shared_interrupts.pending_mask(
                    target_id);
            },
            py::arg("target_id"))
        .def(
            "ipi_line",
            [](const SystemState& system, int core_id) {
                return system.shared_interrupts.ipi_line(core_id);
            },
            py::arg("core_id"))
        .def(
            "set_ipi_line",
            [](SystemState& system, int core_id, bool asserted) {
                system.shared_interrupts.set_ipi_line(
                    core_id, asserted);
            },
            py::arg("core_id"),
            py::arg("asserted"))
        .def(
            "ipi_pending_snapshot",
            [](const SystemState& system) {
                return system.shared_interrupts.pending_snapshot();
            })
        .def_property_readonly(
            "mappings_sealed",
            [](const SystemState& system) {
                return system.mappings_sealed;
            })
        .def_property_readonly(
            "mem_size",
            [](const SystemState& system) {
                return system.shared_memory.mem_size;
            })
        .def_property_readonly(
            "hbw_base",
            [](const SystemState& system) {
                return system.shared_memory.hbw_base;
            })
        .def_property_readonly(
            "hbw_size",
            [](const SystemState& system) {
                return system.shared_memory.hbw_size;
            })
        .def_property_readonly(
            "ext_mem_base",
            [](const SystemState& system) {
                return system.shared_memory.ext_mem_base;
            })
        .def_property_readonly(
            "ext_mem_size",
            [](const SystemState& system) {
                return system.shared_memory.ext_mem_size;
            })
        .def_property_readonly(
            "vram_base",
            [](const SystemState& system) {
                return system.shared_memory.vram_base;
            })
        .def_property_readonly(
            "vram_size",
            [](const SystemState& system) {
                return system.shared_memory.vram_size;
            })
        .def_property_readonly(
            "mem_buffer",
            [](const SystemState& system) -> py::object {
                if (!system.shared_memory.mem_exporter)
                    return py::none();
                return system.shared_memory.mem_exporter;
            })
        .def_property_readonly(
            "hbw_buffer",
            [](const SystemState& system) -> py::object {
                if (!system.shared_memory.hbw_exporter)
                    return py::none();
                return system.shared_memory.hbw_exporter;
            })
        .def_property_readonly(
            "ext_mem_buffer",
            [](const SystemState& system) -> py::object {
                if (!system.shared_memory.ext_mem_exporter)
                    return py::none();
                return system.shared_memory.ext_mem_exporter;
            })
        .def_property_readonly(
            "vram_buffer",
            [](const SystemState& system) -> py::object {
                if (!system.shared_memory.vram_exporter)
                    return py::none();
                return system.shared_memory.vram_exporter;
            })
        .def(
            "attach_mem",
            [](SystemState& system, py::buffer buf, uint64_t size) {
                require_unsealed_system_mappings(system);
                PreparedBuffer prepared =
                    prepare_writable_byte_buffer(buf, size, true);
                py::object prepared_exporter = buf;
                std::unique_ptr<py::buffer_info> old_lease;
                py::object old_exporter;
                {
                    MemoryMutationGuard guard(system.shared_memory);
                    require_unsealed_system_mappings(system);
                    old_lease =
                        std::move(system.shared_memory.mem_lease);
                    old_exporter =
                        std::move(system.shared_memory.mem_exporter);
                    system.shared_memory.mem_lease =
                        std::move(prepared.lease);
                    system.shared_memory.mem_exporter =
                        std::move(prepared_exporter);
                    system.shared_memory.mem = prepared.ptr;
                    system.shared_memory.mem_size = size;
                    system.shared_memory.mem_capacity =
                        prepared.capacity;
                    sync_system_main_memory_ptrs(system);
                }
                // Releasing an exporter may invoke Python, so the old lease
                // remains alive until after the mapping lock is free.
            },
            py::arg("buffer"),
            py::arg("size"))
        .def(
            "attach_hbw_mem",
            [](SystemState& system, py::buffer buf,
               uint64_t base, uint64_t size) {
                require_unsealed_system_mappings(system);
                validate_guest_region(base, size);
                PreparedBuffer prepared =
                    prepare_writable_byte_buffer(buf, size, false);
                py::object prepared_exporter = buf;
                std::unique_ptr<py::buffer_info> old_lease;
                py::object old_exporter;
                {
                    MemoryMutationGuard guard(system.shared_memory);
                    require_unsealed_system_mappings(system);
                    old_lease =
                        std::move(system.shared_memory.hbw_lease);
                    old_exporter =
                        std::move(system.shared_memory.hbw_exporter);
                    system.shared_memory.hbw_lease =
                        std::move(prepared.lease);
                    system.shared_memory.hbw_exporter =
                        std::move(prepared_exporter);
                    system.shared_memory.hbw_mem = prepared.ptr;
                    system.shared_memory.hbw_base = base;
                    system.shared_memory.hbw_size = size;
                    system.shared_memory.hbw_capacity =
                        prepared.capacity;
                    sync_system_nic_memory_ptrs(system);
                }
            },
            py::arg("buffer"),
            py::arg("base"),
            py::arg("size"))
        .def(
            "attach_ext_mem",
            [](SystemState& system, py::buffer buf,
               uint64_t base, uint64_t size) {
                require_unsealed_system_mappings(system);
                validate_guest_region(base, size);
                PreparedBuffer prepared =
                    prepare_writable_byte_buffer(buf, size, false);
                py::object prepared_exporter = buf;
                std::unique_ptr<py::buffer_info> old_lease;
                py::object old_exporter;
                {
                    MemoryMutationGuard guard(system.shared_memory);
                    require_unsealed_system_mappings(system);
                    old_lease =
                        std::move(system.shared_memory.ext_mem_lease);
                    old_exporter =
                        std::move(system.shared_memory.ext_mem_exporter);
                    system.shared_memory.ext_mem_lease =
                        std::move(prepared.lease);
                    system.shared_memory.ext_mem_exporter =
                        std::move(prepared_exporter);
                    system.shared_memory.ext_mem = prepared.ptr;
                    system.shared_memory.ext_mem_base = base;
                    system.shared_memory.ext_mem_size = size;
                    system.shared_memory.ext_mem_capacity =
                        prepared.capacity;
                    sync_system_nic_memory_ptrs(system);
                }
            },
            py::arg("buffer"),
            py::arg("base"),
            py::arg("size"))
        .def(
            "attach_vram",
            [](SystemState& system, py::buffer buf,
               uint64_t base, uint64_t size) {
                require_unsealed_system_mappings(system);
                validate_guest_region(base, size);
                PreparedBuffer prepared =
                    prepare_writable_byte_buffer(buf, size, false);
                py::object prepared_exporter = buf;
                std::unique_ptr<py::buffer_info> old_lease;
                py::object old_exporter;
                {
                    MemoryMutationGuard guard(system.shared_memory);
                    require_unsealed_system_mappings(system);
                    old_lease =
                        std::move(system.shared_memory.vram_lease);
                    old_exporter =
                        std::move(system.shared_memory.vram_exporter);
                    system.shared_memory.vram_lease =
                        std::move(prepared.lease);
                    system.shared_memory.vram_exporter =
                        std::move(prepared_exporter);
                    system.shared_memory.vram_mem = prepared.ptr;
                    system.shared_memory.vram_base = base;
                    system.shared_memory.vram_size = size;
                    system.shared_memory.vram_capacity =
                        prepared.capacity;
                }
            },
            py::arg("buffer"),
            py::arg("base"),
            py::arg("size"))
        .def(
            "core",
            &SystemState::core,
            py::arg("index"),
            py::return_value_policy::reference_internal)
        .def(
            "micro_core",
            &SystemState::micro_core,
            py::arg("index"),
            py::return_value_policy::reference_internal)
        ;

    // Kept private because arbitrary profile selection is not a supported
    // public CPUState contract.  The accelerated Megapad64Micro wrapper uses
    // this only for standalone oracle/differential tests; production system
    // microcores borrow their stable SystemState-owned views.
    m.def(
        "_make_micro_cpu_state",
        []() {
            return make_cpu_state(CoreProfile::MICRO);
        });

    // Expose RunResult
    py::class_<RunResult>(m, "RunResult")
        .def_readonly("total_cycles", &RunResult::total_cycles)
        .def_readonly("steps_executed", &RunResult::steps_executed)
        .def_readonly("stop_reason", &RunResult::stop_reason)
        .def_readonly("trap_id", &RunResult::trap_id)
        ;

    // Single step function
    m.def("step_one", [](CPUState& s,
                          py::function mmio_read8,
                          py::function mmio_write8,
                          py::function on_output,
                          py::object csr_read_override,
                          uint64_t mmio_start,
                          uint64_t mmio_end) -> int {
        StepCallbacks cb;
        cb.mmio_start = mmio_start;
        cb.mmio_end = mmio_end;
        cb.has_mmio = true;
        cb.mmio_read8 = [&](uint64_t addr) -> uint8_t {
            return mmio_read8(addr).cast<uint8_t>();
        };
        cb.mmio_write8 = [&](uint64_t addr, uint8_t val) {
            mmio_write8(addr, val);
        };
        cb.on_output = [&](int port, int val) {
            on_output(port, val);
        };
        if (!csr_read_override.is_none()) {
            auto fn = csr_read_override.cast<py::function>();
            cb.csr_read_override = [fn](int addr) -> uint64_t {
                py::object result = fn(addr);
                if (result.is_none()) return (uint64_t)-1;
                return result.cast<uint64_t>();
            };
        }
        // A scanout may briefly own the memory mapping while its conversion
        // loop runs without the GIL.  Release the GIL only for blocking lock
        // acquisition, then retain it for all single-step Python callbacks.
        std::unique_ptr<CPUExecutionGuard> execution_guard;
        {
            py::gil_scoped_release release;
            execution_guard = std::make_unique<CPUExecutionGuard>(s);
        }
        return step_one(s, cb);
    },
    py::arg("state"),
    py::arg("mmio_read8"),
    py::arg("mmio_write8"),
    py::arg("on_output"),
    py::arg("csr_read_override") = py::none(),
    py::arg("mmio_start") = 0xFFFFFF0000000000ULL,
    py::arg("mmio_end")   = 0xFFFFFF8000000000ULL
    );

    // Batch run function (main acceleration entry point)
    //
    // The GIL is released for the entire batch so that background
    // Python threads (display, NIC RX) can run concurrently with
    // instruction execution.  Each Python callback reacquires the
    // GIL only for the duration of the callback.
    m.def("run_steps", [](CPUState& s,
                           py::function mmio_read8,
                           py::function mmio_write8,
                           py::function on_output,
                           py::object csr_read_override,
                           uint64_t mmio_start,
                           uint64_t mmio_end,
                           int max_steps) -> RunResult {
        StepCallbacks cb;
        cb.mmio_start = mmio_start;
        cb.mmio_end = mmio_end;
        cb.has_mmio = true;
        cb.mmio_read8 = [&](uint64_t addr) -> uint8_t {
            py::gil_scoped_acquire acq;
            return mmio_read8(addr).cast<uint8_t>();
        };
        cb.mmio_write8 = [&](uint64_t addr, uint8_t val) {
            py::gil_scoped_acquire acq;
            mmio_write8(addr, val);
        };
        cb.on_output = [&](int port, int val) {
            py::gil_scoped_acquire acq;
            on_output(port, val);
        };
        if (!csr_read_override.is_none()) {
            auto fn = csr_read_override.cast<py::function>();
            cb.csr_read_override = [fn](int addr) -> uint64_t {
                py::gil_scoped_acquire acq;
                py::object result = fn(addr);
                if (result.is_none()) return (uint64_t)-1;
                return result.cast<uint64_t>();
            };
        }
        py::gil_scoped_release release;
        CPUExecutionGuard execution_guard(s);
        return run_steps(s, cb, max_steps);
    },
    py::arg("state"),
    py::arg("mmio_read8"),
    py::arg("mmio_write8"),
    py::arg("on_output"),
    py::arg("csr_read_override") = py::none(),
    py::arg("mmio_start") = 0xFFFFFF0000000000ULL,
    py::arg("mmio_end")   = 0xFFFFFF8000000000ULL,
    py::arg("max_steps") = 1000000
    );
}
