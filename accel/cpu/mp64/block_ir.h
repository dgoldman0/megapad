#pragma once

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

}  // namespace mp64::cpu
