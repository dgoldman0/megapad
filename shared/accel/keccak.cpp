#include "keccak.h"

namespace megapad::keccak {
namespace {

constexpr std::uint64_t round_constants[24] = {
    0x0000000000000001ULL, 0x0000000000008082ULL, 0x800000000000808AULL,
    0x8000000080008000ULL, 0x000000000000808BULL, 0x0000000080000001ULL,
    0x8000000080008081ULL, 0x8000000000008009ULL, 0x000000000000008AULL,
    0x0000000000000088ULL, 0x0000000080008009ULL, 0x000000008000000AULL,
    0x000000008000808BULL, 0x800000000000008BULL, 0x8000000000008089ULL,
    0x8000000000008003ULL, 0x8000000000008002ULL, 0x8000000000000080ULL,
    0x000000000000800AULL, 0x800000008000000AULL, 0x8000000080008081ULL,
    0x8000000000008080ULL, 0x0000000080000001ULL, 0x8000000080008008ULL,
};

constexpr unsigned rotations[lane_count] = {
     0,  1, 62, 28, 27,
    36, 44,  6, 55, 20,
     3, 10, 43, 25, 39,
    41, 45, 15, 21,  8,
    18,  2, 61, 56, 14,
};

std::uint64_t rotate_left(std::uint64_t value, unsigned shift) noexcept {
    return shift ? ((value << shift) | (value >> (64 - shift))) : value;
}

// Retain the emulator kernel's non-elidable round-scratch erasure without
// depending on its AES implementation or exposing a host-secret API.
void clear_scratch(void* address, std::size_t length) noexcept {
    volatile std::uint8_t* bytes = static_cast<volatile std::uint8_t*>(address);
    while (length-- != 0)
        *bytes++ = 0;
}

}  // namespace

void permute(std::uint64_t (&state)[lane_count]) noexcept {
    for (unsigned round = 0; round < 24; ++round) {
        // Theta: column parity.
        std::uint64_t columns[5];
        for (unsigned x = 0; x < 5; ++x)
            columns[x] = state[x] ^ state[x + 5] ^ state[x + 10] ^
                         state[x + 15] ^ state[x + 20];
        std::uint64_t deltas[5];
        for (unsigned x = 0; x < 5; ++x)
            deltas[x] = columns[(x + 4) % 5] ^
                        rotate_left(columns[(x + 1) % 5], 1);
        for (unsigned index = 0; index < lane_count; ++index)
            state[index] ^= deltas[index % 5];

        // Rho and pi.
        std::uint64_t rotated[lane_count];
        for (unsigned x = 0; x < 5; ++x) {
            for (unsigned y = 0; y < 5; ++y) {
                const unsigned source = x + 5 * y;
                const unsigned destination = y + 5 * ((2 * x + 3 * y) % 5);
                rotated[destination] = rotate_left(state[source], rotations[source]);
            }
        }

        // Chi.
        for (unsigned y = 0; y < 5; ++y) {
            for (unsigned x = 0; x < 5; ++x) {
                state[x + 5 * y] = rotated[x + 5 * y] ^
                    (~rotated[((x + 1) % 5) + 5 * y] &
                       rotated[((x + 2) % 5) + 5 * y]);
            }
        }

        // Iota.
        state[0] ^= round_constants[round];

        clear_scratch(columns, sizeof(columns));
        clear_scratch(deltas, sizeof(deltas));
        clear_scratch(rotated, sizeof(rotated));
    }
}

}  // namespace megapad::keccak
