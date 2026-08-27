#pragma once

#include <cstddef>
#include <cstdint>
#include <type_traits>

namespace mp64::cpu {

enum class DecodedOperation : uint8_t {
    INVALID = 0,
    NOP,
    CALL_LONG,
    RETURN_LONG,
    INCREMENT,
    DECREMENT,
    BRANCH_SHORT,
    BRANCH_LONG,
    LOAD_NATURAL,
    STORE_NATURAL,
    LOAD_BYTE,
    STORE_BYTE,
    LOAD_IMMEDIATE,
    LOAD_HIGH_IMMEDIATE,
    ADD_IMMEDIATE,
    AND_IMMEDIATE,
    OR_IMMEDIATE,
    XOR_IMMEDIATE,
    COMPARE_IMMEDIATE,
    SUBTRACT_IMMEDIATE,
    SHIFT_LEFT_IMMEDIATE,
    SHIFT_RIGHT_LOGICAL_IMMEDIATE,
    SHIFT_RIGHT_ARITHMETIC_IMMEDIATE,
    ROTATE_LEFT_IMMEDIATE,
    ADD,
    ADD_WITH_CARRY,
    SUBTRACT,
    SUBTRACT_WITH_BORROW,
    BITWISE_AND,
    BITWISE_OR,
    BITWISE_XOR,
    COMPARE,
    MOVE,
    BITWISE_NOT,
    NEGATE,
    SHIFT_LEFT,
    SHIFT_RIGHT_LOGICAL,
    SHIFT_RIGHT_ARITHMETIC,
    ROTATE_LEFT,
    ROTATE_RIGHT,
    SELECT_PROGRAM_COUNTER,
};

enum DecodedInstructionTrait : uint8_t {
    NO_DECODED_TRAITS = 0,
    PREFIXED_ENCODING = uint8_t{1} << 0,
    NONCANONICAL_ENCODING = uint8_t{1} << 1,
    TERMINAL_CONTROL = uint8_t{1} << 2,
    MEMORY_OPERATION = uint8_t{1} << 3,
    DIRECT_MEMORY_READ = uint8_t{1} << 4,
    DIRECT_MEMORY_STORE = uint8_t{1} << 5,
    WRITES_DESTINATION = uint8_t{1} << 6,
    CONDITIONAL_CONTROL = uint8_t{1} << 7,
};

// The decoded form is deliberately compact enough to remain cheap in block
// caches: one semantic payload word followed by exactly eight bytes of
// identity, operand, timing, and classification state.
struct DecodedInstruction {
    uint64_t immediate = 0;
    uint8_t opcode = 0;
    DecodedOperation operation = DecodedOperation::INVALID;
    uint8_t rd = 0;
    uint8_t rs = 0;
    uint8_t encoded_size = 0;
    uint8_t cycle_cost = 0;
    uint8_t traits = NO_DECODED_TRAITS;
    uint8_t reserved = 0;

    constexpr uint8_t family() const noexcept {
        return static_cast<uint8_t>(opcode >> 4);
    }

    constexpr uint8_t subop() const noexcept {
        return static_cast<uint8_t>(opcode & 0x0F);
    }

    constexpr bool has_trait(
            DecodedInstructionTrait trait) const noexcept {
        return (traits & static_cast<uint8_t>(trait)) != 0;
    }

    constexpr bool is_terminal_control() const noexcept {
        return has_trait(TERMINAL_CONTROL);
    }

    constexpr bool is_memory() const noexcept {
        return has_trait(MEMORY_OPERATION);
    }

    constexpr bool is_direct_read() const noexcept {
        return has_trait(DIRECT_MEMORY_READ);
    }

    constexpr bool is_direct_store() const noexcept {
        return has_trait(DIRECT_MEMORY_STORE);
    }

    constexpr bool ends_block() const noexcept {
        return is_terminal_control() || is_direct_store();
    }

    constexpr uint32_t register_write_mask() const noexcept {
        return
            has_trait(WRITES_DESTINATION) && rd < 32
                ? uint32_t{1} << rd
                : 0;
    }

    constexpr uint8_t conditional_taken_cycle_cost() const noexcept {
        return has_trait(CONDITIONAL_CONTROL) ? 1 : 0;
    }

    constexpr uint8_t fetch_hit_count(
            uint64_t start_address) const noexcept {
        return encoded_size == 0
            ? 0
            : static_cast<uint8_t>(
                1 +
                (((start_address & uint64_t{7}) + encoded_size - 1) >> 3));
    }
};

static_assert(sizeof(DecodedOperation) == 1);
static_assert(std::is_standard_layout_v<DecodedInstruction>);
static_assert(std::is_trivially_copyable_v<DecodedInstruction>);
static_assert(sizeof(DecodedInstruction) == 16);
static_assert(offsetof(DecodedInstruction, immediate) == 0);
static_assert(offsetof(DecodedInstruction, opcode) == 8);
static_assert(offsetof(DecodedInstruction, traits) == 14);
static_assert(offsetof(DecodedInstruction, reserved) == 15);

enum class DecodeStatus : uint8_t {
    DECODED = 0,
    DEFERRED,
    UNAVAILABLE,
    ILLEGAL_DOUBLE_PREFIX,
};

struct DecodeResult {
    DecodedInstruction instruction{};
    DecodeStatus status = DecodeStatus::UNAVAILABLE;
    uint8_t family = 0;
    uint8_t subop = 0;
    int modifier = -1;
    uint8_t bytes_consumed = 0;
    uint8_t prefix_size = 0;

    constexpr bool decoded() const noexcept {
        return status == DecodeStatus::DECODED;
    }
};

// Reader supplies bool read(uint8_t&) and void observe_prefix(uint8_t).
// Keeping the implementation templated lets the hot architectural reader
// inline fetch8 while observational block construction uses the same semantic
// source without a per-byte indirect call.
template <typename Reader>
DecodeResult decode_instruction(
    Reader& reader,
    int initial_modifier = -1);

}  // namespace mp64::cpu

#include "decode_impl.h"
