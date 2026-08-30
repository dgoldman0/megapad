#pragma once

#include <array>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <vector>

#include "../../cpu/mp64/decode.h"

namespace mp64::dbt::x86_64 {

using BlockEntry = uint64_t (*)(
    void*,
    const std::atomic<uint8_t>*,
    uint8_t* const*);

inline constexpr uint8_t NATIVE_BLOCK_EXIT_SHIFT = 16;
inline constexpr uint64_t NATIVE_BLOCK_COMPLETED_INSTRUCTION_MASK =
    (uint64_t{1} << NATIVE_BLOCK_EXIT_SHIFT) - 1;
// Generated blocks return retired steps in the low sixteen bits and these
// backend-private exit flags immediately above them. The execution kernel
// validates and normalizes this transport into the shared MP64 BlockExit.
inline constexpr uint8_t NATIVE_BLOCK_EXIT_INTERRUPT = uint8_t{1} << 0;
inline constexpr uint8_t NATIVE_BLOCK_EXIT_CONDITIONAL_TAKEN =
    uint8_t{1} << 1;
inline constexpr uint64_t NATIVE_BLOCK_EXIT_KNOWN_FLAGS =
    NATIVE_BLOCK_EXIT_INTERRUPT |
    NATIVE_BLOCK_EXIT_CONDITIONAL_TAKEN;

// CPU ownership ends at this layout description. The x86-64 backend receives
// only the displacements it needs and never depends on the mixed CPUState
// definition, scheduler state, Python bindings, or guest-cache ownership.
struct CoreStateLayout {
    std::size_t object_size = 0;
    std::array<int32_t, 32> registers{};
    int32_t program_counter_selector = -1;
    int32_t flag_z = -1;
    int32_t flag_c = -1;
    int32_t flag_n = -1;
    int32_t flag_v = -1;
    int32_t flag_p = -1;
    int32_t flag_g = -1;
    int32_t icache_hits = -1;
    int32_t icache_misses = -1;
    int32_t ifetch_window_valid = -1;
    int32_t icache_undo_count = -1;
    int32_t icache_undo_hits = -1;
    int32_t icache_undo_misses = -1;

    CoreStateLayout() noexcept {
        registers.fill(-1);
    }
};

// The caller retains block storage and proves its architectural identity.
// This non-owning view carries only decoded MP64 semantics and selectors into
// lowering; publication and executable-code lifetime remain outside it.
struct BlockView {
    uint64_t address = 0;
    const cpu::DecodedInstruction* instructions = nullptr;
    std::size_t instruction_count = 0;
    uint8_t psel = 0;
    uint8_t spsel = 0;
};

// A region owns no references beyond its two caller-retained block views.
// Lowering copies both blocks into one native code object and preserves the
// ordinary BlockEntry ABI and packed-exit transport.
struct RegionView {
    BlockView source{};
    BlockView target{};
};

static_assert(std::is_standard_layout_v<CoreStateLayout>);
static_assert(std::is_trivially_copyable_v<CoreStateLayout>);
static_assert(std::is_standard_layout_v<BlockView>);
static_assert(std::is_trivially_copyable_v<BlockView>);
static_assert(std::is_standard_layout_v<RegionView>);
static_assert(std::is_trivially_copyable_v<RegionView>);

bool lowering_available() noexcept;

std::vector<uint8_t> lower_block(
    const CoreStateLayout& layout,
    const BlockView& block);

std::vector<uint8_t> lower_region(
    const CoreStateLayout& layout,
    const RegionView& region);

}  // namespace mp64::dbt::x86_64
