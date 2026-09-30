#pragma once

#include <memory>
#include <optional>
#include "routine_callbacks.h"

namespace mp64::cpu::routine_task_v1 {

inline constexpr uint64_t MAX_ENTRIES = 1024;

// Separate types grant no authority through the private callback transports.
struct Spec {
    routine_v2::Spec sealed;
    uint64_t max_callback_requests = 0;
};
struct Budget {
    uint64_t invocation_instructions_remaining = 0;
    uint64_t root_instructions_remaining = 0;
    uint64_t invocation_callbacks_remaining = 0;
    uint64_t root_callbacks_remaining = 0;
    uint64_t quantum_instructions = 0;
};
struct OwnerIdentity {};
struct RootToken {
    std::weak_ptr<OwnerIdentity> owner;
    uint64_t root_id = 0, generation = 0;
};
struct OperationToken {
    std::weak_ptr<OwnerIdentity> owner;
    uint64_t root_generation = 0, invocation_id = 0, sequence = 0;
    bool consumed = false;
};
struct RequestToken {
    std::weak_ptr<OwnerIdentity> owner;
    uint64_t root_generation = 0, invocation_id = 0, sequence = 0;
    bool consumed = false;
};
// Child authority is deliberately not issued by the one-frame foundation.
struct ChildEdge {};

enum class State : uint8_t { CALLBACK, RETURNED, YIELDED, FAILED };
inline const char* state_name(State value) noexcept {
    switch (value) {
    case State::CALLBACK: return "callback";
    case State::RETURNED: return "returned";
    case State::YIELDED: return "yielded";
    default: return "failed";
    }
}

// POD is committed before any Python event allocation; it survives retirement.
struct Receipt {
    uint64_t root_generation = 0, root_id = 0, invocation_id = 0;
    uint64_t parent_invocation_id = 0, depth = 1, sequence = 0;
    bool invocation_started = false;
    uint64_t root_entries = 0;
    State state = State::YIELDED;
    uint64_t instructions = 0, cycles = 0, callback_requests = 0;
    uint64_t invocation_instructions = 0, invocation_cycles = 0, invocation_callbacks = 0;
    uint64_t root_instructions = 0, root_cycles = 0, root_callbacks = 0;
};
struct Result : routine_v1::Result {
    Receipt receipt;
    std::shared_ptr<OperationToken> operation_token;
    std::shared_ptr<RequestToken> request_token;
    uint64_t site = 0, request_sequence = 0, export_id = 0;
    std::vector<uint64_t> arguments;
};
struct Cancellation {
    std::vector<uint64_t> retired_invocation_ids;
    uint64_t surviving_parent_id = 0;
    std::shared_ptr<RequestToken> surviving_parent_token;
    std::optional<Receipt> receipt;
};

} // namespace mp64::cpu::routine_task_v1
