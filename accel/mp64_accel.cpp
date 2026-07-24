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
#include <atomic>
#include <cstdint>
#include <cmath>
#include <cstring>
#include <limits>
#include <memory>
#include <mutex>
#include <shared_mutex>
#include <stdexcept>
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

    // Execution reads mappings under shared ownership.  Attachment/metadata
    // replacement and framebuffer rendering are exclusive users; serializing
    // rendering also avoids races with guest framebuffer writes.  A single
    // execution flag deliberately enforces one-worker access for every core
    // that shares this mapping.
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
//  CPU State — flat execution state plus borrowed memory mappings
// ---------------------------------------------------------------------------

struct CPUState {
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

    // I-cache
    uint8_t  icache_enabled;
    uint64_t icache_hits, icache_misses;

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

    // System-owned cores borrow the central IPI router.  Standalone cores
    // preserve the historical manually settable interrupt-line latch.
    InterruptRouter* interrupts = nullptr;
    std::atomic<bool> private_irq_ipi{false};

    // Standalone states own one private mapping.  System-owned states borrow
    // their parent's shared mapping and leave private_memory empty.
    std::unique_ptr<MemoryMappings> private_memory;
    MemoryMappings* memory = nullptr;

    // C++ native crypto devices (bypass Python MMIO callbacks)
    CryptoDevices crypto;

    // C++ native NIC device (bypass Python MMIO for networking)
    NICDevice nic;

    // C++ native TRNG device (bypass Python MMIO for random bytes)
    TRNGDevice trng;

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

    // C++ native UART device (RX/status/TX and BIOS TX ring)
    UARTDevice uart;

    // Accelerator hooks — intercept CALL.L to known BIOS word addresses
    static constexpr int MAX_ACCEL_HOOKS = 8;
    struct AccelHookEntry {
        uint64_t addr;
        int      id;    // 1=RECT_FILL, 2=BLIT_GLYPH
    };
    AccelHookEntry accel_hooks[MAX_ACCEL_HOOKS];
    int accel_hook_count = 0;

    void register_accel_hook(uint64_t addr, int hook_id);
};

static std::unique_ptr<CPUState> make_cpu_state(
        MemoryMappings* shared_memory = nullptr,
        TimerDevice* shared_timer = nullptr,
        UartGeomDevice* shared_uart_geom = nullptr,
        FramebufferDevice* shared_fb = nullptr,
        RTCDevice* shared_rtc = nullptr,
        InterruptRouter* shared_interrupts = nullptr) {
    auto state = std::make_unique<CPUState>();
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
    state->interrupts = shared_interrupts;
    for (int index = 0; index < 8; index++)
        state->port_map[index] = 0xFFFF;
    state->dict_clear_all();
    state->crc_acc = 0xFFFFFFFF;
    state->crc_mode = 0;
    state->gf_prime_sel = 0;
    state->gf_custom_p = BigNum();
    state->gf_mont_pinv = BigNum();
    state->gf_prev_lo = BigNum();
    state->gf_prev_hi = BigNum();
    return state;
}

// SystemState owns full-core lifetimes, exactly one mapping set, and migrated
// singleton devices.  Other devices and scheduling remain per-core
// compatibility paths for later transactional milestones.  Shared resources
// are declared before cores so borrowed pointers die before their owners.
struct SystemState {
    explicit SystemState(int full_core_count, int all_core_count = 0) {
        if (full_core_count < 1 || full_core_count > 255)
            throw std::invalid_argument(
                "full_core_count must be between 1 and 255");
        if (all_core_count == 0)
            all_core_count = full_core_count;
        if (all_core_count < full_core_count || all_core_count > 255)
            throw std::invalid_argument(
                "all_core_count must include every full core and fit in 8 bits");

        shared_interrupts.configure(all_core_count);
        cores.reserve(static_cast<std::size_t>(full_core_count));
        for (int index = 0; index < full_core_count; index++) {
            auto core = make_cpu_state(
                &shared_memory,
                &shared_timer,
                &shared_uart_geom,
                &shared_fb,
                &shared_rtc,
                &shared_interrupts);
            core->core_id = static_cast<uint8_t>(index);
            core->num_cores = static_cast<uint8_t>(all_core_count);
            cores.push_back(std::move(core));
        }
        advertised_core_count = all_core_count;
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

    int all_core_count() const {
        return advertised_core_count;
    }

    MemoryMappings shared_memory;
    TimerDevice shared_timer{};
    UartGeomDevice shared_uart_geom{};
    FramebufferDevice shared_fb{};
    RTCDevice shared_rtc{};
    InterruptRouter shared_interrupts{};
    std::vector<std::unique_ptr<CPUState>> cores;
    int advertised_core_count = 0;
    bool mappings_sealed = false;
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

// The root shared mapping ownership may be borrowed by nested Python scopes
// and native callbacks.  Keep the CAS and mutex lease alive until the final
// descendant closes, even when same-thread scopes are retained or exited out
// of lexical order.
class SharedMemoryLease {
public:
    SharedMemoryLease(MemoryMappings& memory, const char* busy_message)
        : memory_(memory),
          memory_lock_(memory.mutex, std::defer_lock) {
        bool expected = false;
        if (!memory_.execution_active.compare_exchange_strong(
                expected, true,
                std::memory_order_acq_rel, std::memory_order_acquire)) {
            throw std::runtime_error(busy_message);
        }
        owns_execution_flag_ = true;

        try {
            memory_lock_.lock();
        } catch (...) {
            memory_.execution_active.store(false, std::memory_order_release);
            owns_execution_flag_ = false;
            throw;
        }
    }

    ~SharedMemoryLease() {
        if (memory_lock_.owns_lock())
            memory_lock_.unlock();
        if (owns_execution_flag_)
            memory_.execution_active.store(false, std::memory_order_release);
    }

    SharedMemoryLease(const SharedMemoryLease&) = delete;
    SharedMemoryLease& operator=(const SharedMemoryLease&) = delete;

private:
    MemoryMappings& memory_;
    std::shared_lock<std::shared_mutex> memory_lock_;
    bool owns_execution_flag_ = false;
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

void CPUState::register_accel_hook(uint64_t addr, int hook_id) {
    MemoryMutationGuard guard(
        *memory,
        "CPUState accelerator hooks cannot be changed while CPUState is in use");
    if (accel_hook_count < MAX_ACCEL_HOOKS)
        accel_hooks[accel_hook_count++] = {addr, hook_id};
}

class CPUExecutionGuard {
public:
    explicit CPUExecutionGuard(CPUState& state)
        : state_(state),
          memory_(*state.memory),
          shared_owner_{&memory_, nullptr, nullptr, nullptr},
          native_owner_{&memory_, nullptr} {
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

// Direct DMA-capable device bindings participate in the same one-worker
// mapping scope as instruction execution.  A secondary-core Python MMIO
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
    s.nic.attach_mem_ptrs(
        s.memory->mem, s.memory->mem_size,
        s.memory->hbw_mem, s.memory->hbw_base, s.memory->hbw_size,
        s.memory->ext_mem, s.memory->ext_mem_base, s.memory->ext_mem_size);
}

static inline void sync_main_memory_ptrs(CPUState& s) {
    s.uart.attach_mem(s.memory->mem, s.memory->mem_size);
    s.crypto.wots.mem = s.memory->mem;
    s.crypto.wots.mem_size = s.memory->mem_size;
    sync_nic_memory_ptrs(s);
}

static inline void sync_system_nic_memory_ptrs(SystemState& system) {
    for (const auto& core : system.cores)
        sync_nic_memory_ptrs(*core);
}

static inline void sync_system_main_memory_ptrs(SystemState& system) {
    for (const auto& core : system.cores)
        sync_main_memory_ptrs(*core);
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

static inline void mem_write8(CPUState& s, uint64_t addr, uint8_t val) {
    auto r = resolve_mem(s, addr);
    r.buf[r.off] = val;
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
        if (s.accel_hooks[i].addr == target) return s.accel_hooks[i].id;
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
                if (bits & 0x80)
                    write_rgb565_le(region.ptr + col * 2, fg16);
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
                    if (bits & 0x80)
                        write_rgb565_le(region.ptr + col * 2, fg16);
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

static inline uint8_t fetch8(CPUState& s) {
    uint64_t a = pc(s);
    const auto region = resolve_mem(s, a);
    const uint8_t v = region.buf[region.off];
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
            if (val & 2) { s.icache_hits = 0; s.icache_misses = 0; s.icache_enabled = 1; }
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
    uint8_t peek = mem_read8(s, pc(s));
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
        if (region_span_fits(s.memory->vram_size, off, TILE_BYTES))
            std::memcpy(s.memory->vram_mem + off, data.data(), TILE_BYTES);
        return;
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        const uint64_t off = addr - s.memory->ext_mem_base;
        if (region_span_fits(s.memory->ext_mem_size, off, TILE_BYTES))
            std::memcpy(s.memory->ext_mem + off, data.data(), TILE_BYTES);
        return;
    }
    if (s.memory->hbw_mem && region_contains(s.memory->hbw_base, s.memory->hbw_size, addr)) {
        const uint64_t off = addr - s.memory->hbw_base;
        if (region_span_fits(s.memory->hbw_size, off, TILE_BYTES))
            std::memcpy(s.memory->hbw_mem + off, data.data(), TILE_BYTES);
        return;
    }
    if (!s.memory->mem || s.memory->mem_size == 0)
        return;
    uint64_t a = addr % s.memory->mem_size;
    if (region_span_fits(s.memory->mem_size, a, TILE_BYTES))
        std::memcpy(s.memory->mem + a, data.data(), TILE_BYTES);
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

// Memory access with MMIO and HBW intercept
static inline uint8_t sys_read8(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        // Try C++ devices first (no Python callback needed)
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off))
            return s.nic.read8(mmio_off);
        if (s.uart.handles(mmio_off))
            return s.uart.read8(mmio_off);
        if (s.trng.handles(mmio_off))
            return s.trng.read8(mmio_off);
        if (s.crypto.handles(mmio_off))
            return s.crypto.read8(mmio_off);
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

static inline void sys_write8(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint8_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        // Try C++ devices first
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            s.nic.write8(mmio_off, val);
            return;
        }
        if (s.uart.handles(mmio_off)) {
            s.uart.write8(mmio_off, val);
            return;
        }
        if (s.trng.handles(mmio_off)) {
            s.trng.write8(mmio_off, val);
            return;
        }
        if (s.crypto.handles(mmio_off)) {
            s.crypto.write8(mmio_off, val);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            s.fb->write8(mmio_off, val);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            s.timer->write8(mmio_off, val);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            s.rtc->write8(mmio_off, val);
            return;
        }
        if (uart_geom_span(mmio_off, 1) &&
                s.uart_geom->handles(mmio_off)) {
            s.uart_geom->write8(mmio_off, val);
            return;
        }
        cb.mmio_write8(addr, val);  // fallback to Python for other devices
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
        return;
    }
    if (s.memory->ext_mem && region_contains(s.memory->ext_mem_base, s.memory->ext_mem_size, addr)) {
        s.memory->ext_mem[addr - s.memory->ext_mem_base] = val;
        return;
    }
    if (s.memory->vram_mem && region_contains(s.memory->vram_base, s.memory->vram_size, addr)) {
        s.memory->vram_mem[addr - s.memory->vram_base] = val;
        return;
    }
    mem_write8(s, addr, val);
}

// Wider MMIO/HBW-aware reads/writes
static inline uint64_t sys_read64(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.nic.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.trng.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.trng.read8(mmio_off + i) << (8*i);
            return v;
        }
        if (s.crypto.handles(mmio_off)) {
            uint64_t v = 0;
            for (int i = 0; i < 8; i++)
                v |= (uint64_t)s.crypto.read8(mmio_off + i) << (8*i);
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

static inline void sys_write64(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint64_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.nic.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.nic.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.trng.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.trng.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.crypto.handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.crypto.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.fb->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.timer->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.rtc->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (uart_geom_span(mmio_off, 8) &&
                s.uart_geom->handles(mmio_off)) {
            for (int i = 0; i < 8; i++)
                s.uart_geom->write8(
                    mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        for (int i = 0; i < 8; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
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

static inline uint16_t sys_read16(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off))
            return s.crypto.read8(mmio_off) | ((uint16_t)s.crypto.read8(mmio_off+1) << 8);
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

static inline void sys_write16(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint16_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            s.crypto.write8(mmio_off, val & 0xFF);
            s.crypto.write8(mmio_off+1, (val >> 8) & 0xFF);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            s.fb->write8(mmio_off, val & 0xFF);
            s.fb->write8(mmio_off + 1, (val >> 8) & 0xFF);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            s.timer->write8(mmio_off, val & 0xFF);
            s.timer->write8(mmio_off + 1, (val >> 8) & 0xFF);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            s.rtc->write8(mmio_off, val & 0xFF);
            s.rtc->write8(mmio_off + 1, (val >> 8) & 0xFF);
            return;
        }
        if (uart_geom_span(mmio_off, 2) &&
                s.uart_geom->handles(mmio_off)) {
            s.uart_geom->write8(mmio_off, val & 0xFF);
            s.uart_geom->write8(mmio_off + 1, (val >> 8) & 0xFF);
            return;
        }
        cb.mmio_write8(addr, val & 0xFF);
        cb.mmio_write8(addr+1, (val >> 8) & 0xFF);
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

static inline uint32_t sys_read32(CPUState& s, const StepCallbacks& cb, uint64_t addr) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            uint32_t v = 0;
            for (int i = 0; i < 4; i++)
                v |= (uint32_t)s.crypto.read8(mmio_off + i) << (8*i);
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

static inline void sys_write32(CPUState& s, const StepCallbacks& cb, uint64_t addr, uint32_t val) {
    if (cb.has_mmio && addr >= cb.mmio_start && addr < cb.mmio_end) {
        uint32_t mmio_off = (uint32_t)(addr - cb.mmio_start);
        if (s.crypto.handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.crypto.write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.fb->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.fb->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.timer->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.timer->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (s.rtc->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.rtc->write8(mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        if (uart_geom_span(mmio_off, 4) &&
                s.uart_geom->handles(mmio_off)) {
            for (int i = 0; i < 4; i++)
                s.uart_geom->write8(
                    mmio_off + i, (val >> (8*i)) & 0xFF);
            return;
        }
        for (int i = 0; i < 4; i++)
            cb.mmio_write8(addr + i, (val >> (8*i)) & 0xFF);
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
        if (!in_mmio) {
            auto r = resolve_mem(s, dst);
            if (r.buf && r.off + ln <= r.size) {
                std::memset(r.buf + r.off, fb, (size_t)ln);
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

// Read one block from memory at TSRC0
static void sha_read_block(CPUState& s, uint8_t* block) {
    int bsz = sha_block_size(s);
    for (int i = 0; i < bsz; i++)
        block[i] = mem_read8(s, s.tsrc0 + i);
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
static int sha_compress(CPUState& s) {
    uint64_t H[8];
    sha_unpack(s, H);
    uint8_t block[128];
    sha_read_block(s, block);
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
static bool sha_write_pad(CPUState& s) {
    int bsz = sha_block_size(s);
    int lsz = s.sha_mode >= 1 ? 16 : 8;
    int pos = (int)(s.regs[0] & 0xFFFFFFFFull) % bsz;
    uint64_t base = s.tsrc0;

    mem_write8(s, base + pos, 0x80);
    pos++;

    bool two_blocks = pos > (bsz - lsz);
    if (two_blocks) {
        while (pos < bsz) { mem_write8(s, base + pos, 0x00); pos++; }
        s.flag_c = 1;
        return true;
    }
    // zero-fill
    while (pos < bsz - lsz) { mem_write8(s, base + pos, 0x00); pos++; }
    // big-endian length
    uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
    if (s.sha_mode >= 1) {
        for (int i = 0; i < 8; i++)
            mem_write8(s, base + bsz - 16 + i, (uint8_t)(hi >> (56 - i*8)));
    }
    for (int i = 0; i < 8; i++)
        mem_write8(s, base + bsz - 8 + i, (uint8_t)(lo >> (56 - i*8)));
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

static BigNum gf_read_tile_b(CPUState& s) {
    uint8_t buf[32];
    uint64_t base = s.tsrc0;
    for (int i = 0; i < 32; i++) buf[i] = mem_read8(s, base + i);
    return BigNum::from_le_bytes(buf);
}

static void gf_write_tile_dst(CPUState& s, const BigNum& v) {
    uint8_t buf[32];
    v.to_le_bytes(buf);
    uint64_t base = s.tdst;
    for (int i = 0; i < 32; i++) mem_write8(s, base + i, buf[i]);
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
            return sha_compress(s);
        }
        case 0x2: { // SHA.PAD
            sha_write_pad(s);
            return 3;
        }
        case 0x3: { // SHA.DIN Rd, Rs — feed one byte
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            int rs = (rex_s(s.ext_modifier) << 4) | (rb & 0xF);
            uint8_t byte_val = (uint8_t)(s.regs[rs] & 0xFF);
            uint64_t base = s.tsrc0;
            uint64_t r0 = s.regs[0];
            mem_write8(s, base + r0, byte_val);
            r0++;
            // track message length in bits
            uint64_t old = s.sha_msglen_lo;
            s.sha_msglen_lo += 8;
            if (s.sha_msglen_lo < old) s.sha_msglen_hi++;
            // auto-round when block is full
            int bsz = sha_block_size(s);
            int cycles = 1;
            if ((int)r0 >= bsz) {
                cycles += sha_compress(s);
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
            bool two_blocks = sha_write_pad(s);
            int cycles = 3;
            if (two_blocks) {
                cycles += sha_compress(s);
                // write second pad block (zeros + length)
                int bsz = sha_block_size(s);
                int lsz = s.sha_mode >= 1 ? 16 : 8;
                uint64_t base = s.tsrc0;
                for (int i = 0; i < bsz - lsz; i++)
                    mem_write8(s, base + i, 0x00);
                uint64_t lo = s.sha_msglen_lo, hi = s.sha_msglen_hi;
                if (s.sha_mode >= 1) {
                    for (int i = 0; i < 8; i++)
                        mem_write8(s, base + bsz - 16 + i, (uint8_t)(hi >> (56 - i*8)));
                }
                for (int i = 0; i < 8; i++)
                    mem_write8(s, base + bsz - 8 + i, (uint8_t)(lo >> (56 - i*8)));
            }
            cycles += sha_compress(s);
            return cycles;
        }
        default:
            throw std::runtime_error("TRAP:ILLEGAL_OP:EXT.CRYPTO SHA-2 reserved sub-op");
        }
    } else if (unit == 0x2) {
        // --- Field ALU unit (§B.5) ---
        BigNum p = gf_get_prime(s);
        switch (op) {
        case 0x0: { // GF.ADD
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum r = bn_addmod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x1: { // GF.SUB
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum r = bn_submod(a, b, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 1;
        }
        case 0x2: { // GF.MUL
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
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
            BigNum e = gf_read_tile_b(s);
            BigNum r = bn_powmod(a, e, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return 767;
        }
        case 0x6: { // GF.MULR — raw 256×256→512
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum lo, hi;
            BigNum::mul_wide(a, b, lo, hi);
            gf_bignum_to_acc(s, lo);
            gf_write_tile_dst(s, hi);
            s.gf_prev_lo = lo;
            s.gf_prev_hi = hi;
            return 1;
        }
        case 0x7: { // GF.MAC — (ACC * B + prev) mod p
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum ab = gf_mulmod_sel(s, a, b, p);
            BigNum r = bn_addmod(ab, s.gf_prev_lo, p);
            gf_bignum_to_acc(s, r);
            s.gf_prev_lo = r;
            return gf_is_mont(s) ? 4 : 1;
        }
        case 0x8: { // GF.MACR — raw: prev_512 + ACC * B
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
            BigNum mul_lo, mul_hi;
            BigNum::mul_wide(a, b, mul_lo, mul_hi);
            BigNum sum_lo = s.gf_prev_lo.add(mul_lo);
            BigNum sum_hi = s.gf_prev_hi.add(mul_hi);
            if (sum_lo < s.gf_prev_lo) {
                BigNum one; one.w[0] = 1;
                sum_hi = sum_hi.add(one);
            }
            gf_bignum_to_acc(s, sum_lo);
            gf_write_tile_dst(s, sum_hi);
            s.gf_prev_lo = sum_lo;
            s.gf_prev_hi = sum_hi;
            return 1;
        }
        case 0x9: { // GF.CMOV Rd
            uint8_t rb = fetch8(s);
            int rd = (rex_d(s.ext_modifier) << 4) | ((rb >> 4) & 0xF);
            bool cond = s.regs[rd] != 0;
            BigNum b = gf_read_tile_b(s);
            if (cond) {
                gf_bignum_to_acc(s, b);
                s.gf_prev_lo = b;
            }
            return 1;
        }
        case 0xA: { // GF.CEQ — constant-time equality
            BigNum a = gf_acc_to_bignum(s);
            BigNum b = gf_read_tile_b(s);
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
            s.gf_mont_pinv = gf_read_tile_b(s);
            return 1;
        }
        case 0xD: { // GF.X25519
            BigNum scalar = gf_acc_to_bignum(s);
            BigNum u_coord = gf_read_tile_b(s);
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

static int step_one(CPUState& s, const StepCallbacks& cb) {
    if (s.halted)
        throw std::runtime_error("HALT");
    if (s.idle) {
        s.cycle_count++;
        return 1;
    }

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
                if (hook) {
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
                sys_write8(s, cb, mmio_addr, val);
            }
            if (cb.on_output)
                cb.on_output(n, val);
        } else if (n >= 9 && n <= 15) {  // INP
            int port = n - 8;
            uint8_t val;
            uint32_t mmio_off = s.port_map[port];
            if (mmio_off < 0x1000 && cb.has_mmio) {
                uint64_t mmio_addr = cb.mmio_start + mmio_off;
                val = sys_read8(s, cb, mmio_addr);
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
            s.uart.init();
            s.uart.attach_mem(s.memory->mem, s.memory->mem_size);
        })
        .def("uart_disable", [](CPUState& s) { s.uart.enabled = false; })
        .def("uart_enabled", [](const CPUState& s) { return s.uart.enabled; })
        .def("uart_read8", [](CPUState& s, uint32_t off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.uart.read8(off);
        })
        .def("uart_write8", [](CPUState& s, uint32_t off, uint8_t value) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.uart.write8(off, value);
        })
        .def("uart_inject", [](CPUState& s, py::bytes payload) {
            std::string data = payload;
            s.uart.inject(reinterpret_cast<const uint8_t*>(data.data()), data.size());
        })
        .def("uart_has_rx", [](const CPUState& s) { return s.uart.has_rx_data(); })
        .def("uart_rx_size", [](const CPUState& s) { return s.uart.rx_size(); })
        .def_property("uart_tx_ring_base",
            [](const CPUState& s) { return s.uart.get_tx_ring_base(); },
            [](CPUState& s, uint64_t value) { s.uart.set_tx_ring_base(value); })
        .def("uart_drain_tx", [](CPUState& s) -> py::bytes {
            const std::vector<uint8_t> data = s.uart.take_tx();
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
            s.crypto.init();
            // Ensure WOTS chain has current memory pointer
            s.crypto.wots.mem = s.memory->mem;
            s.crypto.wots.mem_size = s.memory->mem_size;
        })
        .def("disable_crypto", [](CPUState& s) {
            s.crypto.enabled = false;
        })
        .def("crypto_enabled", [](const CPUState& s) {
            return s.crypto.enabled;
        })
        // Sync crypto state from Python devices (for save/restore)
        .def("crypto_aes_reset", [](CPUState& s) { s.crypto.aes.reset(); })
        .def("crypto_sha3_reset", [](CPUState& s) { s.crypto.sha3.reset(); s.crypto.sha3.mode = 0; })
        .def("crypto_wots_reset", [](CPUState& s) {
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState WOTS memory cannot be reset while memory is in use");
            s.crypto.wots.reset();
            s.crypto.wots.sha3 = &s.crypto.sha3;
            s.crypto.wots.mem = s.memory->mem;
            s.crypto.wots.mem_size = s.memory->mem_size;
        })
        .def("crypto_wots_status", [](const CPUState& s) -> uint8_t {
            return s.crypto.wots.status;
        })
        // Direct crypto MMIO access (for testing / Python-side access)
        .def("crypto_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.crypto.read8(mmio_off);
        })
        .def("crypto_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.crypto.write8(mmio_off, val);
        })
        // ── NIC device ────────────────────────────────────────
        .def("nic_init", [](CPUState& s, py::bytes mac_bytes) {
            MemoryMutationGuard guard(
                *s.memory,
                "CPUState NIC memory cannot be initialized while memory is in use");
            std::string mac_str = mac_bytes;
            uint8_t mac[6] = {};
            size_t n = std::min(mac_str.size(), (size_t)6);
            std::memcpy(mac, mac_str.data(), n);
            s.nic.init(mac);
            // Wire memory pointers from CPUState
            s.nic.attach_mem_ptrs(
                s.memory->mem, s.memory->mem_size,
                s.memory->hbw_mem, s.memory->hbw_base, s.memory->hbw_size,
                s.memory->ext_mem, s.memory->ext_mem_base, s.memory->ext_mem_size
            );
        })
        .def("nic_sync_mem_ptrs", [](CPUState& s) {
            // Re-sync memory pointers after attach_ext_mem / attach_hbw_mem
            MemoryMutationGuard guard(*s.memory);
            sync_nic_memory_ptrs(s);
        })
        .def("nic_set_tx_callback", [](CPUState& s, py::function cb) {
            // tx_callback: called from C++ when NIC sends a frame
            // cb receives (bytes,) and returns bool
            s.nic.tx_callback = [cb](const uint8_t* data, size_t len) -> bool {
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
            std::string data = frame;
            return s.nic.inject_frame(
                reinterpret_cast<const uint8_t*>(data.data()), data.size()
            );
        })
        .def("nic_has_rx", [](CPUState& s) -> bool {
            return s.nic.has_rx();
        })
        .def("nic_rx_queue_size", [](CPUState& s) -> size_t {
            return s.nic.rx_queue_size();
        })
        .def("nic_tx_queue_size", [](const CPUState& s) -> size_t {
            return s.nic.tx_queue_size();
        })
        .def("nic_drain_one_tx", [](CPUState& s) -> py::bytes {
            auto frame = s.nic.drain_one_tx();
            return py::bytes(reinterpret_cast<const char*>(frame.data()), frame.size());
        })
        .def("nic_set_link_up", [](CPUState& s, bool up) {
            s.nic.link_up = up;
        })
        .def("nic_enabled", [](const CPUState& s) -> bool {
            return s.nic.enabled;
        })
        .def("nic_disable", [](CPUState& s) {
            s.nic.enabled = false;
        })
        .def("nic_reset", [](CPUState& s) {
            s.nic.reset_state();
        })
        .def("nic_read8", [](CPUState& s, uint32_t mmio_off) -> uint8_t {
            auto memory_guard = acquire_shared_memory_use(s);
            return s.nic.read8(mmio_off);
        })
        .def("nic_write8", [](CPUState& s, uint32_t mmio_off, uint8_t val) {
            auto memory_guard = acquire_shared_memory_use(s);
            s.nic.write8(mmio_off, val);
        })
        .def("nic_irq_pending", [](const CPUState& s) -> bool {
            return s.nic.irq_pending();
        })
        .def("nic_get_tx_count", [](const CPUState& s) -> uint16_t {
            return s.nic.tx_count;
        })
        .def("nic_get_rx_count", [](const CPUState& s) -> uint16_t {
            return s.nic.rx_count.load(std::memory_order_relaxed);
        })
        // ── TRNG device ───────────────────────────────────────
        .def("init_trng", [](CPUState& s) {
            s.trng.init();
        })
        .def("trng_enabled", [](const CPUState& s) -> bool {
            return s.trng.enabled;
        })
        .def("disable_trng", [](CPUState& s) {
            s.trng.enabled = false;
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
        .def("fb_tick", [](CPUState& s, uint32_t cycles) {
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
        .def("timer_tick", [](CPUState& s, uint32_t cycles) {
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
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                return s.rtc->realtime;
            },
            [](CPUState& s, bool v) {
                std::lock_guard<std::mutex> rtc_guard(s.rtc->mutex);
                s.rtc->realtime = v;
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

    // Native system ownership.  Borrowed core views keep their parent alive
    // and never take ownership of the pointed-to CPUState.
    py::class_<SystemState>(m, "SystemState")
        .def(py::init<int, int>(),
             py::arg("full_core_count"),
             py::arg("all_core_count") = 0)
        .def_property_readonly(
            "full_core_count", &SystemState::full_core_count)
        .def_property_readonly(
            "all_core_count", &SystemState::all_core_count)
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
        ;

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
