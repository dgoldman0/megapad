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

} // namespace mp64::cpu::routine_v3
