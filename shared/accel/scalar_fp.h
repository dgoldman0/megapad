#pragma once

#include <cstdint>

namespace megapad::scalar_fp {

// Pure values only. Flags are already shifted to FPCSR[8:4]. FCMP alone
// sets has_relation, with relation -1, 0, 1, or 2 (unordered).
struct Outcome {
    std::uint64_t value = 0;
    std::uint64_t flags = 0;
    int relation = 0;
    bool has_relation = false;
};

// A null diagnostic denotes a legal encoding. The returned diagnostic has
// static lifetime; adapters own instruction fetch, traps, and error wording.
const char* validate(unsigned op, unsigned t_byte,
                     std::uint64_t fpcsr) noexcept;
unsigned instruction_length(unsigned op) noexcept;
unsigned extra_cycles(unsigned op) noexcept;

// The caller validates first. No host floating-point environment, Python
// objects, allocation, guest memory, or architectural state is used here.
// An internal bound violation throws rather than silently truncating a value.
Outcome execute(unsigned op, std::uint64_t rd, std::uint64_t rs,
                std::uint64_t rt, std::uint64_t fpcsr);

}  // namespace megapad::scalar_fp
