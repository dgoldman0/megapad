#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace mp64::cpu {

enum class BlockExitReason : uint8_t {
    NOT_EXECUTED = 0,
    BLOCK_COMPLETE,
    INSTRUCTION_LIMIT,
    INTERRUPT_BOUNDARY,
    TIMING_BOUNDARY,
};

// Shared result of portable or generated MP64 block execution. Generated
// backends may use a private compact return token, but the execution kernel
// must validate and normalize that transport before exposing this contract to
// the scheduler. It is host-only state and is never serialized.
struct BlockExit {
    uint64_t completed_cycles = 0;
    uint32_t completed_instructions = 0;
    BlockExitReason reason = BlockExitReason::NOT_EXECUTED;
    uint8_t reserved[3]{};

    constexpr bool executed() const noexcept {
        return reason != BlockExitReason::NOT_EXECUTED;
    }

    constexpr bool valid() const noexcept {
        if (reason > BlockExitReason::TIMING_BOUNDARY)
            return false;
        if (!executed()) {
            return
                completed_instructions == 0 &&
                completed_cycles == 0;
        }
        return completed_instructions != 0 && completed_cycles != 0;
    }
};

static_assert(std::is_standard_layout_v<BlockExit>);
static_assert(std::is_trivially_copyable_v<BlockExit>);
static_assert(sizeof(BlockExit) == 16);

// A memory address admitted into a portable block is expressed from state
// available before that block starts. Entry-register and constant sources are
// direct. A prior-read source names an earlier item in the same ordered access
// table; preflight may inspect that already-proved ordinary-RAM value when a
// later address depends on it. Unknown provenance is construction-only and
// must never be published as an executable recipe.
inline constexpr uint8_t BLOCK_MEMORY_ENTRY_REGISTER_LIMIT = 32;
inline constexpr uint8_t BLOCK_MEMORY_PRIOR_READ_BASE = 0x80;
inline constexpr uint8_t BLOCK_MEMORY_CONSTANT_SOURCE = 0xFE;
inline constexpr uint8_t BLOCK_MEMORY_UNKNOWN_SOURCE = 0xFF;

constexpr bool block_memory_source_is_entry_register(
        uint8_t source) noexcept {
    return source < BLOCK_MEMORY_ENTRY_REGISTER_LIMIT;
}

constexpr bool block_memory_source_is_prior_read(
        uint8_t source) noexcept {
    return
        source >= BLOCK_MEMORY_PRIOR_READ_BASE &&
        source < BLOCK_MEMORY_CONSTANT_SOURCE;
}

constexpr uint8_t block_memory_prior_read_source(
        uint8_t access_index) noexcept {
    return static_cast<uint8_t>(
        BLOCK_MEMORY_PRIOR_READ_BASE + access_index);
}

constexpr uint8_t block_memory_prior_read_index(
        uint8_t source) noexcept {
    return static_cast<uint8_t>(
        source - BLOCK_MEMORY_PRIOR_READ_BASE);
}

struct BlockMemoryAddressRecipe {
    uint64_t addend = 0;
    uint8_t source = BLOCK_MEMORY_UNKNOWN_SOURCE;

    constexpr bool known() const noexcept {
        return
            block_memory_source_is_entry_register(source) ||
            block_memory_source_is_prior_read(source) ||
            source == BLOCK_MEMORY_CONSTANT_SOURCE;
    }
};

// Keep the published table compact: its structure-of-arrays layout costs nine
// bytes per possible access instead of padding every recipe to sixteen bytes.
// Capacity remains a property of the caller's bounded block storage.
template <std::size_t Capacity>
struct BlockMemoryAddressRecipes {
    std::array<uint64_t, Capacity> addends{};
    std::array<uint8_t, Capacity> sources{};

    BlockMemoryAddressRecipes() noexcept {
        sources.fill(BLOCK_MEMORY_UNKNOWN_SOURCE);
    }

    constexpr void set(
            std::size_t index,
            BlockMemoryAddressRecipe recipe) noexcept {
        addends[index] = recipe.addend;
        sources[index] = recipe.source;
    }

    constexpr BlockMemoryAddressRecipe get(
            std::size_t index) const noexcept {
        return {addends[index], sources[index]};
    }
};

static_assert(std::is_standard_layout_v<BlockMemoryAddressRecipe>);
static_assert(std::is_trivially_copyable_v<BlockMemoryAddressRecipe>);
static_assert(
    std::is_standard_layout_v<BlockMemoryAddressRecipes<1>>);
static_assert(
    std::is_trivially_copyable_v<BlockMemoryAddressRecipes<1>>);

}  // namespace mp64::cpu
