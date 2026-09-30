#pragma once

#include <memory>
#include "routine_callbacks.h"

namespace mp64::cpu::routine_v3 {

inline constexpr uint64_t MAX_DEPTH = 8;
inline constexpr uint64_t MAX_CHILD_EDGES = 4096;
inline constexpr uint64_t MAX_OWNER_CHILD_EDGES = 65536;

// A separate native type keeps older transport entry points from accepting a
// nested registration. The sealed instruction/site proof is shared internally.
struct Spec {
    routine_v2::Spec sealed;
    uint64_t max_callback_requests = 0;
};

struct OwnerIdentity {};
struct PublicationIdentity {};

// Retained handles do not keep a publication, CPU, pin, or runner alive. Entry
// authority requires this exact object in the live parent's issued table and
// both original publication generations, independently of diagnostic values.
struct ChildEdge {
    std::weak_ptr<OwnerIdentity> owner;
    std::weak_ptr<PublicationIdentity> parent_publication, child_publication;
    const Spec* parent_spec = nullptr;
    const Spec* child_spec = nullptr;
    uint64_t parent_generation = 0, child_generation = 0;
    uint64_t site_index = 0, call_edge_id = 0;
};

struct Token {
    std::weak_ptr<OwnerIdentity> owner;
    std::weak_ptr<PublicationIdentity> publication;
    const Spec* spec = nullptr;
    uint64_t root_invocation_id = 0, invocation_id = 0, sequence = 0;
    uint64_t generation = 0, control_slot = 0, return_pc = 0;
    bool consumed = false;
};

struct Callback {
    uint64_t invocation_id = 0, sequence = 0, site_index = 0;
    uint64_t call_offset = 0, stub_offset = 0, export_id = 0;
    std::vector<uint64_t> arguments;
};

struct Result : routine_v1::Result {
    uint64_t segment_id = 0, root_invocation_id = 0, invocation_id = 0;
    uint64_t parent_invocation_id = 0, depth = 0;
    bool invocation_started = false;
    uint64_t invocation_instructions = 0, invocation_cycles = 0, invocation_callbacks = 0;
    uint64_t chain_instructions = 0, chain_cycles = 0, chain_callbacks = 0;
    std::shared_ptr<Callback> callback;
    std::shared_ptr<Token> token;
};

// This receipt is recorded before any token/result delivery allocation. It
// carries work and identity only, never continuation authority or guest cells.
struct Receipt {
    uint64_t segment_id = 0, root_invocation_id = 0, invocation_id = 0;
    uint64_t parent_invocation_id = 0, depth = 0;
    bool invocation_started = false;
    uint64_t instructions = 0, cycles = 0;
    uint64_t invocation_instructions = 0, invocation_cycles = 0, invocation_callbacks = 0;
    uint64_t chain_instructions = 0, chain_cycles = 0, chain_callbacks = 0;
    bool callback_request = false;
};

} // namespace mp64::cpu::routine_v3
