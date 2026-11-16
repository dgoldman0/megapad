#pragma once

// The six CRC modes' recurrence, written once for the emulator's EXT.CRYPTO
// CRC unit and the simulator engine's CRC words. Modes 0, 1 and 2 are the
// normal MSB-first CRC-32, CRC-32C and CRC-64; modes 4, 5 and 6 are their
// reflected LSB-first forms. A 32-bit mode updates the accumulator's low
// word and leaves its high word zero. No guest memory, ownership, timing or
// Python is here; shared/crc.py states the same values for Python.
#include <cstdint>

namespace megapad::crc {

inline constexpr std::uint32_t crc32_polynomial = 0x04C11DB7u;
inline constexpr std::uint32_t crc32c_polynomial = 0x1EDC6F41u;
inline constexpr std::uint64_t crc64_polynomial = 0x42F0E1EBA9EA3693ull;
inline constexpr std::uint32_t crc32_reflected_polynomial = 0xEDB88320u;
inline constexpr std::uint32_t crc32c_reflected_polynomial = 0x82F63B78u;
inline constexpr std::uint64_t crc64_reflected_polynomial = 0xC96C5795D7870F42ull;

inline constexpr bool mode_valid(std::uint64_t mode) noexcept {
    return mode == 0 || mode == 1 || mode == 2
        || mode == 4 || mode == 5 || mode == 6;
}

inline constexpr bool reflected(std::uint8_t mode) noexcept {
    return (mode & 0x4) != 0;
}

inline constexpr bool wide(std::uint8_t mode) noexcept {
    return (mode & 0x3) == 2;
}

// The accumulator bits a mode uses.
inline constexpr std::uint64_t width_mask(std::uint8_t mode) noexcept {
    return wide(mode) ? 0xFFFFFFFFFFFFFFFFull : 0xFFFFFFFFull;
}

inline constexpr std::uint32_t polynomial32(std::uint8_t mode) noexcept {
    return (mode & 0x3) == 1
        ? (reflected(mode) ? crc32c_reflected_polynomial : crc32c_polynomial)
        : (reflected(mode) ? crc32_reflected_polynomial : crc32_polynomial);
}

inline constexpr std::uint64_t polynomial64(std::uint8_t mode) noexcept {
    return reflected(mode) ? crc64_reflected_polynomial : crc64_polynomial;
}

inline std::uint32_t byte_32(std::uint32_t acc, std::uint8_t b,
                             std::uint32_t poly) noexcept {
    acc ^= static_cast<std::uint32_t>(b) << 24;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x80000000u)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

inline std::uint64_t byte_64(std::uint64_t acc, std::uint8_t b,
                             std::uint64_t poly) noexcept {
    acc ^= static_cast<std::uint64_t>(b) << 56;
    for (int i = 0; i < 8; i++) {
        if (acc & 0x8000000000000000ull)
            acc = (acc << 1) ^ poly;
        else
            acc <<= 1;
    }
    return acc;
}

inline std::uint32_t byte_reflected_32(std::uint32_t acc, std::uint8_t b,
                                       std::uint32_t poly) noexcept {
    acc ^= b;
    for (int i = 0; i < 8; i++) {
        if (acc & 1u)
            acc = (acc >> 1) ^ poly;
        else
            acc >>= 1;
    }
    return acc;
}

inline std::uint64_t byte_reflected_64(std::uint64_t acc, std::uint8_t b,
                                       std::uint64_t poly) noexcept {
    acc ^= b;
    for (int i = 0; i < 8; i++) {
        if (acc & 1ull)
            acc = (acc >> 1) ^ poly;
        else
            acc >>= 1;
    }
    return acc;
}

// One byte through a valid MODE.
inline std::uint64_t feed_byte(std::uint8_t mode, std::uint64_t acc,
                               std::uint8_t b) noexcept {
    if (wide(mode)) {
        const std::uint64_t poly = polynomial64(mode);
        return reflected(mode) ? byte_reflected_64(acc, b, poly)
                               : byte_64(acc, b, poly);
    }
    const std::uint32_t poly = polynomial32(mode);
    const auto low = static_cast<std::uint32_t>(acc);
    return reflected(mode) ? byte_reflected_32(low, b, poly)
                           : byte_32(low, b, poly);
}

// A cell's eight bytes, least significant first, through a valid MODE.
inline std::uint64_t feed_cell(std::uint8_t mode, std::uint64_t acc,
                               std::uint64_t cell) noexcept {
    for (int i = 0; i < 8; i++)
        acc = feed_byte(mode, acc, static_cast<std::uint8_t>(cell >> (i * 8)));
    return acc;
}

}  // namespace megapad::crc
