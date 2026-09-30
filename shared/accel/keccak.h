#pragma once

// Backend-neutral Keccak-f[1600] values. There is no absorb/squeeze policy,
// padding, guest memory, device ownership, or timing in this interface.
#include <cstddef>
#include <cstdint>

namespace megapad::keccak {

inline constexpr std::size_t lane_count = 25;

// Apply all 24 rounds in place. Lane index is x + 5*y; adapters serialize
// individual lanes little-endian when exposing the 200-byte guest image.
void permute(std::uint64_t (&state)[lane_count]) noexcept;

}  // namespace megapad::keccak
