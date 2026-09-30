#include "tile_values.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <limits>
#include <optional>
#include <stdexcept>
#if defined(__i386__) || defined(__x86_64__)
#include <xmmintrin.h>
#endif

// Tile products, two-sum, and reduction nodes require separately rounded
// operations. Both extension builds compile this unit with -ffp-contract=off.
namespace megapad::tile_values {

static_assert(sizeof(double) == 8 && std::numeric_limits<double>::is_iec559);
static_assert(tile_float_canonical_nan(TILE_FP16) == 0x7E00);
static_assert(tile_float_canonical_nan(TILE_BF16) == 0x7FC0);
static_assert(tile_float_canonical_nan(TILE_FP32) == 0x7FC00000);
static_assert(tile_float_canonical_nan(TILE_FP64) == 0x7FF8000000000000ULL);

ScopedFloatEnvironment::ScopedFloatEnvironment() {
#if defined(__i386__) || defined(__x86_64__)
    control_ = _mm_getcsr();
#elif defined(__aarch64__)
    __asm__ volatile("mrs %0, fpcr" : "=r"(control_));
#endif
    if (std::feholdexcept(&environment_) != 0)
        throw std::runtime_error("cannot preserve host floating-point environment");
    if (std::fesetround(FE_TONEAREST) != 0) {
        std::fesetenv(&environment_);
        throw std::runtime_error("cannot establish tile round-to-nearest environment");
    }
#if defined(__i386__) || defined(__x86_64__)
    // Disable FTZ/DAZ and select nearest; feholdexcept masks exceptions.
    _mm_setcsr(_mm_getcsr() & ~0xE040U);
#elif defined(__aarch64__)
    std::uint64_t selected;
    __asm__ volatile("mrs %0, fpcr" : "=r"(selected));
    selected &= ~((1ULL << 24) | (1ULL << 19) | (3ULL << 22));
    __asm__ volatile("msr fpcr, %0" : : "r"(selected));
#endif
}

ScopedFloatEnvironment::~ScopedFloatEnvironment() noexcept {
    std::fesetenv(&environment_);
#if defined(__i386__) || defined(__x86_64__)
    _mm_setcsr(static_cast<unsigned>(control_));
#elif defined(__aarch64__)
    __asm__ volatile("msr fpcr, %0" : : "r"(control_));
#endif
}

bool tile_float_is_nan(
        const TileFloatFormat& f,
        uint64_t bits) {
    return (bits & tile_float_mask(f) & ~tile_float_sign(f)) >
           tile_float_infinity(f);
}

uint64_t tile_float_order_key(
        const TileFloatFormat& f,
        uint64_t bits) {
    bits &= tile_float_mask(f);
    return (bits & tile_float_sign(f))
        ? (tile_float_mask(f) ^ bits)
        : (bits | tile_float_sign(f));
}

double tile_float_to_double(
        const TileFloatFormat& f,
        uint64_t bits) {
    bits &= tile_float_mask(f);
    const int bias = (1 << (f.exponent_bits - 1)) - 1;
    const uint64_t exponent_max = (1ULL << f.exponent_bits) - 1;
    const uint64_t exponent = (bits >> f.fraction_bits) & exponent_max;
    const uint64_t fraction = bits & ((1ULL << f.fraction_bits) - 1);
    double magnitude;
    if (exponent == exponent_max) {
        magnitude = fraction
            ? std::numeric_limits<double>::quiet_NaN()
            : std::numeric_limits<double>::infinity();
    } else if (exponent == 0) {
        magnitude = std::ldexp(
            static_cast<double>(fraction),
            1 - bias - f.fraction_bits);
    } else {
        magnitude = std::ldexp(
            static_cast<double>(fraction | (1ULL << f.fraction_bits)),
            static_cast<int>(exponent) - bias - f.fraction_bits);
    }
    return (bits & tile_float_sign(f)) ? -magnitude : magnitude;
}

uint64_t double_bits(double value) {
    uint64_t bits;
    std::memcpy(&bits, &value, sizeof(bits));
    return bits;
}

// Round a binary64 value once to a tile format: RNE, canonical NaN.
uint64_t tile_float_from_double(
        const TileFloatFormat& f,
        double value) {
    if (std::isnan(value))
        return tile_float_canonical_nan(f);
    const uint64_t raw = double_bits(value);
    if (f.width == 64)
        return raw;
    const uint64_t sign_bits = (raw >> 63) ? tile_float_sign(f) : 0;
    const uint64_t biased64 = (raw >> 52) & 0x7FF;
    if (biased64 == 0x7FF)
        return sign_bits | tile_float_infinity(f);
    // Zero, or a binary64 subnormal far below every tile format's range.
    if (biased64 == 0)
        return sign_bits;
    const int precision = f.fraction_bits + 1;
    const int bias = (1 << (f.exponent_bits - 1)) - 1;
    const int emin = 1 - bias;
    const uint64_t significand =
        (raw & ((1ULL << 52) - 1)) | (1ULL << 52);
    const int exponent = static_cast<int>(biased64) - 1075;
    int quantum = std::max(
        exponent + 52 - (precision - 1),
        emin - (precision - 1));
    const int shift = quantum - exponent;  // at least 53 - precision
    if (shift > 63)
        return sign_bits;
    uint64_t mantissa = significand >> shift;
    const uint64_t remainder = significand & ((1ULL << shift) - 1);
    const uint64_t half = 1ULL << (shift - 1);
    if (remainder > half || (remainder == half && (mantissa & 1))) {
        mantissa++;
        if (mantissa >> precision) {
            mantissa >>= 1;
            quantum++;
        }
    }
    const uint64_t hidden = 1ULL << (precision - 1);
    if (mantissa >= hidden) {
        const int biased = quantum + (precision - 1) + bias;
        if (biased >= (1 << f.exponent_bits) - 1)
            return sign_bits | tile_float_infinity(f);
        return sign_bits |
               (static_cast<uint64_t>(biased) << f.fraction_bits) |
               (mantissa - hidden);
    }
    return sign_bits | mantissa;
}

// The round-to-odd binary64 value of the exact sum of two binary64 values.
double round_to_odd_sum(double product, double addend) {
    double total = product + addend;
    if (!std::isfinite(total))
        return total;
    const double partial = total - addend;
    const double error =
        (product - partial) + (addend - (total - partial));
    if (error != 0.0 && !(double_bits(total) & 1)) {
        total = std::nextafter(
            total,
            error > 0.0
                ? std::numeric_limits<double>::infinity()
                : -std::numeric_limits<double>::infinity());
    }
    return total;
}

uint64_t tile_float_add(
        const TileFloatFormat& f,
        uint64_t a,
        uint64_t b) {
    return tile_float_from_double(
        f, tile_float_to_double(f, a) + tile_float_to_double(f, b));
}

uint64_t tile_float_sub(
        const TileFloatFormat& f,
        uint64_t a,
        uint64_t b) {
    return tile_float_from_double(
        f, tile_float_to_double(f, a) - tile_float_to_double(f, b));
}

uint64_t tile_float_product(
        const TileFloatFormat& dst,
        const TileFloatFormat& src,
        uint64_t a,
        uint64_t b) {
    return tile_float_from_double(
        dst, tile_float_to_double(src, a) * tile_float_to_double(src, b));
}

// RN_dst(a * b + c) with a, b in src and c in dst, rounded once.  Products
// of precision <= 26 operands are exact in binary64, and the round-to-odd
// sum rounds once more correctly for dst precision <= 51.  Binary64 operands
// use the host's correctly rounded fused multiply-add.
uint64_t tile_float_fma(
        const TileFloatFormat& dst,
        const TileFloatFormat& src,
        uint64_t a,
        uint64_t b,
        uint64_t c) {
    const double x = tile_float_to_double(src, a);
    const double y = tile_float_to_double(src, b);
    const double z = tile_float_to_double(dst, c);
    if (src.width == 64)
        return tile_float_from_double(dst, std::fma(x, y, z));
    const double product = x * y;
    if (dst.width == 64)
        return tile_float_from_double(dst, product + z);
    return tile_float_from_double(dst, round_to_odd_sum(product, z));
}

uint64_t tile_float_convert(
        const TileFloatFormat& dst,
        const TileFloatFormat& src,
        uint64_t bits) {
    return tile_float_from_double(dst, tile_float_to_double(src, bits));
}

// IEEE 754-2019 minimum/maximum: NaN propagates and -0 orders below +0.
uint64_t tile_float_extreme2(
        const TileFloatFormat& f,
        uint64_t a,
        uint64_t b,
        bool largest) {
    if (tile_float_is_nan(f, a) || tile_float_is_nan(f, b))
        return tile_float_canonical_nan(f);
    const uint64_t key_a = tile_float_order_key(f, a);
    const uint64_t key_b = tile_float_order_key(f, b);
    if (largest)
        return (key_a >= key_b ? a : b) & tile_float_mask(f);
    return (key_a <= key_b ? a : b) & tile_float_mask(f);
}

// The canonical pairwise tree over leaves held as exact doubles, rounding
// once to the accumulation format at every node.  A binary64 host addition
// is RN_64 itself, and for binary32 leaves RN_32(RN_64(x + y)) = RN_32(x + y)
// because 53 >= 2 * 24 + 2.
uint64_t tile_float_tree(
        const TileFloatFormat& wide,
        double* values,
        int count) {
    while (count > 1) {
        for (int j = 0; j < count / 2; j++) {
            values[j] = tile_float_to_double(
                wide,
                tile_float_from_double(
                    wide,
                    values[2 * j] + values[2 * j + 1]));
        }
        count /= 2;
    }
    return tile_float_from_double(wide, values[0]);
}

// The running NaN-skipping extreme of TRED MIN/MAX under ACC_ACC.
uint64_t tile_float_skip_nan_extreme(
        const TileFloatFormat& wide,
        uint64_t old_value,
        uint64_t value,
        bool largest) {
    if (tile_float_is_nan(wide, old_value))
        return tile_float_is_nan(wide, value)
            ? tile_float_canonical_nan(wide) : value;
    if (tile_float_is_nan(wide, value))
        return old_value;
    const uint64_t old_key = tile_float_order_key(wide, old_value);
    const uint64_t key = tile_float_order_key(wide, value);
    return (largest ? key > old_key : key < old_key) ? value : old_value;
}

// Whether a MINIDX/MAXIDX tile result replaces ACC0/ACC1 under ACC_ACC.
bool tile_float_index_replaces(
        const TileFloatFormat& wide,
        uint64_t candidate,
        uint64_t old_value,
        bool largest) {
    if (tile_float_is_nan(wide, candidate))
        return false;
    if (tile_float_is_nan(wide, old_value))
        return true;
    const uint64_t candidate_key = tile_float_order_key(wide, candidate);
    const uint64_t old_key = tile_float_order_key(wide, old_value);
    return largest ? candidate_key > old_key : candidate_key < old_key;
}

// Round an exact integer, given as a sign and magnitude, once to a tile
// float format (RNE).  Integer magnitudes are never subnormal.
uint64_t tile_float_from_integer(
        const TileFloatFormat& f,
        bool negative,
        uint64_t magnitude) {
    if (magnitude == 0)
        return 0;
    const uint64_t sign_bits = negative ? tile_float_sign(f) : 0;
    const int precision = f.fraction_bits + 1;
    const int bias = (1 << (f.exponent_bits - 1)) - 1;
    int exponent = 63 - __builtin_clzll(magnitude);
    uint64_t mantissa;
    if (exponent < precision) {
        mantissa = magnitude << (precision - 1 - exponent);
    } else {
        const int shift = exponent - (precision - 1);
        mantissa = magnitude >> shift;
        const uint64_t remainder = magnitude & ((1ULL << shift) - 1);
        const uint64_t half = 1ULL << (shift - 1);
        if (remainder > half || (remainder == half && (mantissa & 1))) {
            mantissa++;
            if (mantissa >> precision) {
                mantissa >>= 1;
                exponent++;
            }
        }
    }
    const int biased = exponent + bias;
    if (biased >= (1 << f.exponent_bits) - 1)
        return sign_bits | tile_float_infinity(f);
    return sign_bits |
           (static_cast<uint64_t>(biased) << f.fraction_bits) |
           (mantissa & ((1ULL << f.fraction_bits) - 1));
}

// A float lane converted to a saturating integer lane of width bits: NaN
// gives 0, and the value rounds toward zero or to nearest-even first.  Every
// tile float format converts exactly to double.
uint64_t tile_float_to_integer(
        const TileFloatFormat& f,
        uint64_t bits,
        int width,
        bool is_signed,
        bool nearest) {
    const double x = tile_float_to_double(f, bits);
    if (std::isnan(x))
        return 0;
    double r = std::trunc(x);
    if (nearest) {
        r = std::floor(x);
        const double fraction = x - r;
        if (fraction > 0.5 || (fraction == 0.5 && std::fmod(r, 2.0) != 0.0))
            r += 1.0;
    }
    const uint64_t mask = width == 64 ? ~0ULL : (1ULL << width) - 1;
    if (is_signed) {
        const double bound = std::ldexp(1.0, width - 1);
        if (r >= bound)
            return (mask >> 1);
        if (r < -bound)
            return (1ULL << (width - 1)) & mask;
        return static_cast<uint64_t>(static_cast<int64_t>(r)) & mask;
    }
    if (r >= std::ldexp(1.0, width))
        return mask;
    if (r <= 0.0)
        return 0;
    return static_cast<uint64_t>(r) & mask;
}


namespace {

constexpr const char* names[] = {
    "add", "subtract", "bitwise_and", "bitwise_or", "bitwise_xor",
    "minimum", "maximum", "absolute", "multiply", "fused_multiply_add",
    "widening_multiply", "dot", "sum", "sum_squares", "l1_norm",
    "reduction_minimum", "reduction_maximum", "minimum_index", "maximum_index",
    "select", "compare_mask", "divide", "square_root", "convert", "dot_chunks",
};

bool binary(Operation op) {
    return (op <= Operation::Multiply && op != Operation::Absolute) ||
           op == Operation::FusedMultiplyAdd || op == Operation::WideningMultiply ||
           op == Operation::Dot || op == Operation::DotChunks ||
           op == Operation::Select || op == Operation::Compare || op == Operation::Divide;
}

bool reduction(Operation op) {
    return (op >= Operation::Dot && op <= Operation::MaximumIndex) ||
           op == Operation::DotChunks;
}

std::uint64_t mask_for(int bits) {
    return bits == 64 ? ~0ULL : (1ULL << bits) - 1;
}

std::int64_t signed_lane(std::uint64_t value, int bits) {
    if (value & (1ULL << (bits - 1)))
        return static_cast<std::int64_t>(static_cast<__int128>(value) -
                                        (static_cast<__int128>(1) << bits));
    return static_cast<std::int64_t>(value);
}

std::uint64_t read_lane(ByteView view, int lane, int width) {
    std::uint64_t result = 0;
    const std::size_t start = static_cast<std::size_t>(lane) * width;
    for (int byte = 0; byte < width; ++byte)
        result |= static_cast<std::uint64_t>(view.data[start + byte]) << (8 * byte);
    return result;
}

void write_lane(Outcome& result, int lane, int width, std::uint64_t value) {
    const std::size_t start = static_cast<std::size_t>(lane) * width;
    for (int byte = 0; byte < width; ++byte)
        result.bytes[start + byte] = static_cast<std::uint8_t>(value >> (8 * byte));
}

void require_view(ByteView view, std::size_t size, const char* diagnostic) {
    if (view.size != size || (size && !view.data))
        throw std::invalid_argument(diagnostic);
}

}  // namespace

const char* operation_name(Operation operation) noexcept {
    const auto index = static_cast<unsigned>(operation);
    return index < sizeof(names) / sizeof(names[0]) ? names[index] : nullptr;
}

bool parse_operation(std::string_view name, Operation& result) noexcept {
    for (unsigned index = 0; index < sizeof(names) / sizeof(names[0]); ++index) {
        if (name == names[index]) {
            result = static_cast<Operation>(index);
            return true;
        }
    }
    return false;
}

bool supported(Operation op, unsigned mode, unsigned argument) noexcept {
    if (mode > 0x7F || !operation_name(op))
        return false;
    const auto& format = TILE_FORMATS[mode & 0xF];
    if (!format.defined())
        return false;
    if (op == Operation::Convert) {
        if (argument > 7 || argument == (mode & 0xF))
            return false;
        return format.is_float() || TILE_FORMATS[argument].is_float();
    }
    if (op == Operation::Compare)
        return argument <= 7;
    if (argument != 0)
        return false;
    if (op == Operation::WideningMultiply)
        return format.is_float() && (mode & 0xF) != 7;
    if (op == Operation::FusedMultiplyAdd || reduction(op) ||
        op == Operation::Divide || op == Operation::SquareRoot)
        return format.is_float();
    return true;
}

Outcome execute(Operation op, unsigned mode, ByteView source0,
                ByteView source1, ByteView destination, unsigned argument) {
    if (!supported(op, mode, argument))
        throw std::invalid_argument("tile operation is not supported in this format");
    const auto& format = TILE_FORMATS[mode & 0xF];
    const int width = format.lane_bytes;
    const int bits = format.lane_bits();
    const bool is_signed = (mode & 0x10) != 0;
    const bool saturate = (mode & 0x20) != 0;
    std::size_t source_bytes = 64;
    int conversion_reads = 1, conversion_writes = 1;
    if (op == Operation::Convert) {
        const int target_width = TILE_FORMATS[argument].lane_bytes;
        const int ratio = std::max(width, target_width) / std::min(width, target_width);
        conversion_reads = target_width < width ? ratio : 1;
        conversion_writes = target_width > width ? ratio : 1;
        source_bytes *= conversion_reads;
    }
    require_view(source0, source_bytes, "source0 has the wrong tile byte extent");
    if (binary(op))
        require_view(source1, 64, "source1 must contain exactly one tile");
    else if (source1.size != 0 && source1.size != 64)
        throw std::invalid_argument("unused source1 must be empty or one tile");
    else if (source1.size && !source1.data)
        throw std::invalid_argument("source1 must reference its tile bytes");
    if (op == Operation::FusedMultiplyAdd || op == Operation::Select)
        require_view(destination, 64, "destination input must contain exactly one tile");
    else
        require_view(destination, 0, "unused destination input must be empty");

    std::optional<ScopedFloatEnvironment> environment;
    if (format.is_float() || op == Operation::Convert)
        environment.emplace();
    Outcome result;
    result.size = 64;
    if (op == Operation::Convert) {
        const auto& target = TILE_FORMATS[argument];
        result.size = 64 * conversion_writes;
        const int lanes = conversion_reads * format.lanes();
        for (int lane = 0; lane < lanes; ++lane) {
            const auto value = read_lane(source0, lane, width);
            std::uint64_t converted;
            if (format.is_float() && target.is_float()) {
                converted = tile_float_convert(*target.floating, *format.floating, value);
            } else if (format.is_float()) {
                converted = tile_float_to_integer(*format.floating, value,
                    target.lane_bits(), is_signed, (mode & 0x40) != 0);
            } else {
                const bool negative = is_signed && (value & (1ULL << (bits - 1)));
                const auto magnitude = negative
                    ? 0ULL - static_cast<std::uint64_t>(signed_lane(value, bits)) : value;
                converted = tile_float_from_integer(*target.floating, negative, magnitude);
            }
            write_lane(result, lane, target.lane_bytes, converted);
        }
        return result;
    }

    if (reduction(op)) {
        const auto& fmt = *format.floating;
        const auto& wide = *format.accumulation;
        result.size = 0;
        result.kind = ResultKind::Scalar;
        if (op == Operation::ReductionMinimum || op == Operation::ReductionMaximum ||
            op == Operation::MinimumIndex || op == Operation::MaximumIndex) {
            const bool largest = op == Operation::ReductionMaximum || op == Operation::MaximumIndex;
            int best_index = -1;
            std::uint64_t best_key = 0;
            for (int lane = 0; lane < format.lanes(); ++lane) {
                const auto value = read_lane(source0, lane, width);
                if (tile_float_is_nan(fmt, value))
                    continue;
                const auto key = tile_float_order_key(fmt, value);
                if (best_index < 0 || (largest ? key > best_key : key < best_key)) {
                    best_index = lane;
                    best_key = key;
                }
            }
            const auto value = best_index < 0 ? tile_float_canonical_nan(wide)
                : tile_float_convert(wide, fmt, read_lane(source0, best_index, width));
            if (op == Operation::MinimumIndex || op == Operation::MaximumIndex) {
                result.kind = ResultKind::Indexed;
                result.values[0] = best_index < 0 ? 0 : best_index;
                result.values[1] = value;
            } else {
                result.values[0] = value;
            }
            return result;
        }
        double leaves[32];
        const auto magnitude = tile_float_mask(fmt) ^ tile_float_sign(fmt);
        for (int lane = 0; lane < format.lanes(); ++lane) {
            const auto value = read_lane(source0, lane, width);
            std::uint64_t leaf;
            if (op == Operation::Dot || op == Operation::DotChunks)
                leaf = tile_float_product(wide, fmt, value, read_lane(source1, lane, width));
            else if (op == Operation::SumSquares)
                leaf = tile_float_product(wide, fmt, value, value);
            else
                leaf = tile_float_convert(wide, fmt, op == Operation::L1 ? value & magnitude : value);
            leaves[lane] = tile_float_to_double(wide, leaf);
        }
        if (op == Operation::DotChunks) {
            result.kind = ResultKind::Quarters;
            const int chunk = format.lanes() / 4;
            for (int index = 0; index < 4; ++index)
                result.values[index] = tile_float_tree(wide, leaves + index * chunk, chunk);
        } else {
            result.values[0] = tile_float_tree(wide, leaves, format.lanes());
        }
        return result;
    }

    if (op == Operation::WideningMultiply)
        result.size = 128;
    const std::uint64_t mask = mask_for(bits);
    for (int lane = 0; lane < format.lanes(); ++lane) {
        const auto a = read_lane(source0, lane, width);
        const auto b = binary(op) ? read_lane(source1, lane, width) : 0;
        std::uint64_t value = 0;
        if (op == Operation::Select) {
            value = (read_lane(destination, lane, width) & (1ULL << (bits - 1))) ? a : b;
        } else if (op == Operation::Compare) {
            bool less, equal, unordered = false;
            if (format.is_float()) {
                const auto x = tile_float_to_double(*format.floating, a);
                const auto y = tile_float_to_double(*format.floating, b);
                unordered = std::isnan(x) || std::isnan(y);
                less = x < y;
                equal = x == y;
            } else if (is_signed) {
                less = signed_lane(a, bits) < signed_lane(b, bits);
                equal = a == b;
            } else {
                less = a < b;
                equal = a == b;
            }
            const bool greater = !unordered && !less && !equal;
            const bool answer = argument == 0 ? equal : argument == 1 ? !equal :
                argument == 2 ? less : argument == 3 ? less || equal :
                argument == 4 ? greater : argument == 5 ? greater || equal :
                argument == 6 ? unordered : !unordered;
            value = answer ? mask : 0;
        } else if (op == Operation::And || op == Operation::Or || op == Operation::Xor) {
            value = op == Operation::And ? a & b : op == Operation::Or ? a | b : a ^ b;
        } else if (format.is_float()) {
            const auto& fmt = *format.floating;
            switch (op) {
                case Operation::Add: value = tile_float_add(fmt, a, b); break;
                case Operation::Subtract: value = tile_float_sub(fmt, a, b); break;
                case Operation::Minimum: value = tile_float_extreme2(fmt, a, b, false); break;
                case Operation::Maximum: value = tile_float_extreme2(fmt, a, b, true); break;
                case Operation::Absolute: value = a & ~tile_float_sign(fmt); break;
                case Operation::Multiply: value = tile_float_product(fmt, fmt, a, b); break;
                case Operation::FusedMultiplyAdd:
                    value = tile_float_fma(fmt, fmt, a, b, read_lane(destination, lane, width)); break;
                case Operation::WideningMultiply:
                    value = tile_float_product(*format.accumulation, fmt, a, b); break;
                case Operation::Divide:
                    value = tile_float_from_double(fmt, tile_float_to_double(fmt, a) /
                                                        tile_float_to_double(fmt, b)); break;
                case Operation::SquareRoot:
                    value = tile_float_from_double(fmt, std::sqrt(tile_float_to_double(fmt, a))); break;
                default: throw std::logic_error("unhandled floating tile value operation");
            }
        } else {
            switch (op) {
                case Operation::Add:
                case Operation::Subtract:
                    if (!saturate) {
                        value = op == Operation::Add ? a + b : a - b;
                    } else if (is_signed) {
                        __int128 wide = signed_lane(a, bits);
                        if (op == Operation::Add) wide += signed_lane(b, bits);
                        else wide -= signed_lane(b, bits);
                        const __int128 bound = static_cast<__int128>(1) << (bits - 1);
                        wide = std::max(-bound, std::min(bound - 1, wide));
                        value = static_cast<std::uint64_t>(wide);
                    } else if (op == Operation::Add) {
                        const __uint128_t wide = static_cast<__uint128_t>(a) + b;
                        value = wide > mask ? mask : static_cast<std::uint64_t>(wide);
                    } else {
                        value = a < b ? 0 : a - b;
                    }
                    break;
                case Operation::Minimum:
                    value = (is_signed ? signed_lane(a, bits) < signed_lane(b, bits) : a < b) ? a : b;
                    break;
                case Operation::Maximum:
                    value = (is_signed ? signed_lane(a, bits) > signed_lane(b, bits) : a > b) ? a : b;
                    break;
                case Operation::Absolute:
                    value = is_signed && (a & (1ULL << (bits - 1))) ? 0ULL - a : a;
                    break;
                case Operation::Multiply: value = a * b; break;
                default: throw std::logic_error("unhandled integer tile value operation");
            }
            value &= mask;
        }
        write_lane(result, lane, op == Operation::WideningMultiply ? width * 2 : width, value);
    }
    return result;
}

}  // namespace megapad::tile_values
