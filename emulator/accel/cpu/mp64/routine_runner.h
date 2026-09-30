#pragma once

#include <cstdint>
#include <limits>
#include <optional>
#include <string>
#include <vector>

#include "decode.h"

namespace mp64::cpu::routine_v1 {

// Value/policy definitions for the declared integer routine profile. The
// architectural CPUState, cache, buffer ownership and execution admission
// remain in the existing machine adapter; this header adds no ISA semantics.
inline constexpr uint64_t ROOT_RETURN =
    std::numeric_limits<uint64_t>::max();
inline constexpr uint64_t MAX_CODE_BYTES = 1 << 20;
inline constexpr uint64_t MAX_STACK_BYTES = 8192 * 8;
inline constexpr uint64_t MAX_CONTROL_BYTES = 4 << 20;
inline constexpr uint64_t MAX_CALL_INSTRUCTIONS = 1000000;
inline constexpr uint64_t MAX_DISPATCH_INSTRUCTIONS = 10000000;
inline constexpr std::size_t MAX_BUFFER_SPANS = 16;
inline constexpr std::size_t MAX_PROTECTED_SPANS = 65536;
inline constexpr uint64_t MMIO_BASE = 0xFFFFFF0000000000ULL;
inline constexpr uint64_t MMIO_SIZE = 0x8000000000ULL;

struct Span {
    uint64_t base = 0;
    uint64_t size = 0;

    bool contains(uint64_t address, uint64_t width) const noexcept {
        return address >= base && address - base <= size &&
            width <= size - (address - base);
    }

    bool overlaps(const Span& other) const noexcept {
        if (size == 0 || other.size == 0)
            return false;
        return base <= other.base
            ? other.base - base < size
            : base - other.base < other.size;
    }
};

struct BufferSpan : Span {
    bool read = false;
    bool write = false;
};

struct Spec {
    uint64_t code_base;
    uint64_t code_size;
    uint64_t entry_offset;
    uint64_t input_cells;
    uint64_t output_cells;
    uint64_t stack_base;
    uint64_t stack_size;
    uint64_t max_instructions;

    Span code() const noexcept { return {code_base, code_size}; }
    Span stack() const noexcept { return {stack_base, stack_size}; }
    uint64_t entry() const noexcept { return code_base + entry_offset; }
    uint64_t stack_empty() const noexcept { return stack_base + stack_size; }
};

struct Result {
    std::string exit_kind = "instruction_limit";
    uint64_t instructions = 0;
    uint64_t cycles = 0;
    uint64_t entry_pc = 0;
    uint64_t instruction_pc = 0;
    uint64_t pc = 0;
    std::vector<uint64_t> outputs;
    std::optional<uint64_t> access_address;
    std::optional<uint64_t> access_width;
    std::string access_operation;
    int trap_id = -1;
    std::string detail;
};

// Checked access rejects a whole scalar before its first byte. The existing
// instruction interpreter retains any earlier architectural effects, e.g. a
// CALL.L stack-pointer decrement, before this signal reaches the runner.
struct AccessFault {
    uint64_t address;
    uint64_t width;
    const char* operation;
    const char* detail;
};

inline bool admitted(const DecodedInstruction& decoded) noexcept {
    return decoded.operation != DecodedOperation::INVALID &&
        decoded.operation != DecodedOperation::SELECT_PROGRAM_COUNTER &&
        (decoded.register_write_mask() & (uint32_t{1} << 15)) == 0;
}

}  // namespace mp64::cpu::routine_v1
