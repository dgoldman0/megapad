#pragma once

#include <array>
#include <memory>
#include "routine_runner.h"

namespace mp64::cpu::routine_v2 {

inline constexpr uint64_t MAX_CALLBACK_SITES = 16;
inline constexpr uint64_t MAX_EXPORTS = 64;
inline constexpr uint64_t MAX_CALLBACKS = 1024;
inline constexpr uint64_t MAX_PUBLICATIONS = 64;
inline constexpr uint64_t MAX_TOTAL_CODE_BYTES = 16 << 20;

struct Site {
    uint64_t call_offset, stub_offset, export_id, input_cells, output_cells;
};

struct Spec {
    routine_v1::Spec routine;
    std::vector<uint8_t> code;
    std::vector<Site> callbacks;
    std::vector<uint8_t> boundaries;
};

// Tokens do not keep the runner, CPU, mapped buffers, or invocation alive.
// Authority is the exact native pending object, never these numeric fields.
struct OwnerIdentity {};
struct Token {
    std::weak_ptr<OwnerIdentity> owner;
    uint64_t invocation_id = 0, sequence = 0, publication = 0;
    uint64_t control_slot = 0, return_pc = 0;
    const Spec* spec = nullptr;
    bool consumed = false;
};

struct Callback {
    uint64_t invocation_id = 0, sequence = 0;
    uint64_t call_offset = 0, stub_offset = 0, export_id = 0;
    std::vector<uint64_t> arguments;
};

struct Result : routine_v1::Result {
    uint64_t invocation_id = 0;
    uint64_t invocation_instructions = 0, invocation_cycles = 0;
    std::shared_ptr<Callback> callback;
    std::shared_ptr<Token> token;
};

} // namespace mp64::cpu::routine_v2
