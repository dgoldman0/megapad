#pragma once

#include <array>
#include <cfenv>
#include <cstddef>
#include <cstdint>
#include <string_view>

namespace megapad::tile_values {

struct TileFloatFormat {
    int width;
    int exponent_bits;
    int fraction_bits;
};
inline constexpr TileFloatFormat TILE_FP16{16, 5, 10};
inline constexpr TileFloatFormat TILE_BF16{16, 8, 7};
inline constexpr TileFloatFormat TILE_FP32{32, 8, 23};
inline constexpr TileFloatFormat TILE_FP64{64, 11, 52};

struct TileFormat {
    int lane_bytes;
    const TileFloatFormat* floating;
    const TileFloatFormat* accumulation;
    constexpr bool defined() const { return lane_bytes != 0; }
    constexpr bool is_float() const { return floating != nullptr; }
    constexpr int lanes() const { return 64 / lane_bytes; }
    constexpr int lane_bits() const { return lane_bytes * 8; }
};
inline constexpr TileFormat TILE_FORMATS[16] = {
    {1, nullptr, nullptr}, {2, nullptr, nullptr},
    {4, nullptr, nullptr}, {8, nullptr, nullptr},
    {2, &TILE_FP16, &TILE_FP32}, {2, &TILE_BF16, &TILE_FP32},
    {4, &TILE_FP32, &TILE_FP64}, {8, &TILE_FP64, &TILE_FP64},
};

// Preserve host rounding, exception flags/masks, and architecture-specific
// flush controls around a complete pure operation, never around callbacks.
class ScopedFloatEnvironment {
public:
    ScopedFloatEnvironment();
    ~ScopedFloatEnvironment() noexcept;
    ScopedFloatEnvironment(const ScopedFloatEnvironment&) = delete;
    ScopedFloatEnvironment& operator=(const ScopedFloatEnvironment&) = delete;
private:
    std::fenv_t environment_{};
    std::uint64_t control_ = 0;
};

constexpr std::uint64_t tile_float_mask(const TileFloatFormat& f) {
    return f.width == 64 ? ~0ULL : (1ULL << f.width) - 1;
}
constexpr std::uint64_t tile_float_sign(const TileFloatFormat& f) {
    return 1ULL << (f.width - 1);
}
constexpr std::uint64_t tile_float_infinity(const TileFloatFormat& f) {
    return ((1ULL << f.exponent_bits) - 1) << f.fraction_bits;
}
constexpr std::uint64_t tile_float_canonical_nan(const TileFloatFormat& f) {
    return tile_float_infinity(f) | (1ULL << (f.fraction_bits - 1));
}
inline const TileFloatFormat& tile_float_format(int ew) {
    return *TILE_FORMATS[ew].floating;
}

// Lane helpers are also reused by architectural accumulator publication and
// TAMAC. Callers enclose a whole arithmetic interval in the environment guard.
bool tile_float_is_nan(const TileFloatFormat&, std::uint64_t);
std::uint64_t tile_float_order_key(const TileFloatFormat&, std::uint64_t);
double tile_float_to_double(const TileFloatFormat&, std::uint64_t);
std::uint64_t tile_float_from_double(const TileFloatFormat&, double);
std::uint64_t tile_float_add(const TileFloatFormat&, std::uint64_t, std::uint64_t);
std::uint64_t tile_float_sub(const TileFloatFormat&, std::uint64_t, std::uint64_t);
std::uint64_t tile_float_product(const TileFloatFormat&, const TileFloatFormat&,
                               std::uint64_t, std::uint64_t);
std::uint64_t tile_float_fma(const TileFloatFormat&, const TileFloatFormat&,
                           std::uint64_t, std::uint64_t, std::uint64_t);
std::uint64_t tile_float_convert(const TileFloatFormat&, const TileFloatFormat&, std::uint64_t);
std::uint64_t tile_float_extreme2(const TileFloatFormat&, std::uint64_t, std::uint64_t, bool);
std::uint64_t tile_float_tree(const TileFloatFormat&, double*, int);
std::uint64_t tile_float_skip_nan_extreme(const TileFloatFormat&, std::uint64_t, std::uint64_t, bool);
bool tile_float_index_replaces(const TileFloatFormat&, std::uint64_t, std::uint64_t, bool);
std::uint64_t tile_float_from_integer(const TileFloatFormat&, bool, std::uint64_t);
std::uint64_t tile_float_to_integer(const TileFloatFormat&, std::uint64_t, int, bool, bool);

enum class Operation {
    Add, Subtract, And, Or, Xor, Minimum, Maximum, Absolute, Multiply,
    FusedMultiplyAdd, WideningMultiply, Dot, Sum, SumSquares, L1,
    ReductionMinimum, ReductionMaximum, MinimumIndex, MaximumIndex,
    Select, Compare, Divide, SquareRoot, Convert, DotChunks,
};

struct ByteView {
    const std::uint8_t* data = nullptr;
    std::size_t size = 0;
};

enum class ResultKind { Bytes, Scalar, Indexed, Quarters };
struct Outcome {
    ResultKind kind = ResultKind::Bytes;
    std::array<std::uint8_t, 512> bytes{};
    std::size_t size = 0;
    // Scalar: [0]; indexed: [0]=index,[1]=value; DOTACC: four quarter trees.
    std::array<std::uint64_t, 4> values{};
};

const char* operation_name(Operation) noexcept;
bool parse_operation(std::string_view name, Operation& result) noexcept;
bool supported(Operation, unsigned mode, unsigned argument = 0) noexcept;
// Pure values only. Adapters validate instruction legality before reads,
// retain their original read/write ordering, and publish ACC/TCTRL/Z/timing.
// No guest address, CPU, Python object, or callback crosses this boundary.
Outcome execute(Operation, unsigned mode, ByteView source0,
                ByteView source1 = {}, ByteView destination = {},
                unsigned argument = 0);

}  // namespace megapad::tile_values
