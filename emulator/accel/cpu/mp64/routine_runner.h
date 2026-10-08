#pragma once

#include <cstdint>
#include <limits>

#include "decode.h"

namespace mp64::cpu::routine {

// Values shared by the hybrid routine runner. CPUState, the cache and
// execution admission stay in the machine adapter; this header adds no ISA
// semantics.
inline constexpr uint64_t ROOT_RETURN =
    std::numeric_limits<uint64_t>::max();
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

// Checked access rejects a whole scalar before its first byte. The existing
// instruction interpreter retains any earlier architectural effects, e.g. a
// CALL.L stack-pointer decrement, before this signal reaches the runner.
struct AccessFault {
    uint64_t address;
    uint64_t width;
    const char* operation;
    const char* detail;
};

// Routines run integer instructions only, and cannot move the program
// counter selector or write the stack pointer except through CALL.L/RET.L.
inline bool admitted(const DecodedInstruction& decoded) noexcept {
    return decoded.operation != DecodedOperation::INVALID &&
        decoded.operation != DecodedOperation::SELECT_PROGRAM_COUNTER &&
        (decoded.register_write_mask() & (uint32_t{1} << 15)) == 0;
}

}  // namespace mp64::cpu::routine
