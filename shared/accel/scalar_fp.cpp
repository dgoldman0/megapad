#include "scalar_fp.h"

#include <algorithm>
#include <array>
#include <cstddef>
#include <limits>
#include <stdexcept>

#if !defined(__SIZEOF_INT128__)
#error "MegaPad exact native scalar FP requires unsigned __int128"
#endif

namespace megapad::scalar_fp {
namespace {

using U64 = std::uint64_t;
using U128 = unsigned __int128;
constexpr U64 MASK64 = std::numeric_limits<U64>::max();
constexpr unsigned RNE = 0, RTZ = 1, RDN = 2, RUP = 3, RMM = 4;
constexpr unsigned NX = 1, UF = 2, OF = 4, DZ = 8, NV = 16;

[[noreturn]] void invalid_internal_value() {
    throw std::logic_error("exact scalar FP internal integer bound violated");
}

unsigned word_bits(U64 value) noexcept {
    return value == 0 ? 0 : 64U - static_cast<unsigned>(__builtin_clzll(value));
}

U64 low_mask(unsigned bits) noexcept {
    return bits >= 64 ? MASK64 : (U64{1} << bits) - 1;
}

// A decoded binary64 operand has at most 53 significand bits and exponent
// [-1074, 971]. A product has at most 106 bits and exponent [-2148, 1942].
// FMA adds exactly one such product and one decoded operand. Aligning the
// product above a scalar needs at most 106 + 1942 - (-1074) = 3122 bits;
// aligning the scalar above a product needs at most
// 53 + 971 - (-2148) = 3172 bits. Allowing one carry gives 3173 bits.
// Ordinary addition and integer conversions require less. Thus 4096 bits
// suffice for every scalar operation here. Descriptors stay private so a
// caller cannot feed repeated, unbounded products into this proof.
// Storage is fixed and only initialized limbs are examined during work.
class Magnitude {
public:
    static constexpr unsigned CAPACITY_BITS = 4096;
    static constexpr std::size_t CAPACITY_WORDS = CAPACITY_BITS / 64;

    Magnitude() = default;

    explicit Magnitude(U64 value) {
        if (value != 0) {
            words_[0] = value;
            used_ = 1;
        }
    }

    static Magnitude from_wide(U128 value) {
        Magnitude result;
        result.words_[0] = static_cast<U64>(value);
        result.words_[1] = static_cast<U64>(value >> 64);
        result.used_ = result.words_[1] != 0 ? 2 : (result.words_[0] != 0 ? 1 : 0);
        return result;
    }

    bool zero() const noexcept { return used_ == 0; }

    unsigned bits() const noexcept {
        return zero() ? 0 : static_cast<unsigned>((used_ - 1) * 64)
            + word_bits(words_[used_ - 1]);
    }

    bool bit(unsigned index) const noexcept {
        return index / 64 < used_ && ((words_[index / 64] >> (index % 64)) & 1);
    }

    bool any_below(unsigned count) const noexcept {
        const std::size_t whole = count / 64;
        for (std::size_t i = 0; i < std::min(whole, used_); ++i)
            if (words_[i] != 0)
                return true;
        return whole < used_ && (words_[whole] & low_mask(count % 64)) != 0;
    }

    U64 to_word() const {
        if (used_ > 1)
            invalid_internal_value();
        return zero() ? 0 : words_[0];
    }

    // The caller needs at most 64 retained bits. Discarded low bits are
    // inspected separately, without constructing a wide mask or remainder.
    U64 shifted_word(unsigned shift) const {
        if (shift >= bits())
            return 0;
        if (bits() - shift > 64)
            invalid_internal_value();
        const std::size_t word = shift / 64;
        const unsigned offset = shift % 64;
        U64 value = words_[word] >> offset;
        if (offset != 0 && word + 1 < used_)
            value |= words_[word + 1] << (64 - offset);
        return value;
    }

    Magnitude shifted_left(unsigned shift) const {
        if (zero())
            return {};
        if (shift > CAPACITY_BITS || bits() > CAPACITY_BITS - shift)
            invalid_internal_value();
        Magnitude result;
        const std::size_t whole = shift / 64;
        const unsigned offset = shift % 64;
        for (std::size_t i = 0; i < used_; ++i) {
            result.words_[i + whole] |= words_[i] << offset;
            if (offset != 0) {
                const U64 spill = words_[i] >> (64 - offset);
                if (spill != 0) {
                    if (i + whole + 1 >= CAPACITY_WORDS)
                        invalid_internal_value();
                    result.words_[i + whole + 1] |= spill;
                }
            }
        }
        result.used_ = (bits() + shift + 63) / 64;
        return result;
    }

    int compare(const Magnitude& other) const noexcept {
        if (used_ != other.used_)
            return used_ < other.used_ ? -1 : 1;
        for (std::size_t i = used_; i != 0; --i) {
            if (words_[i - 1] != other.words_[i - 1])
                return words_[i - 1] < other.words_[i - 1] ? -1 : 1;
        }
        return 0;
    }

    Magnitude added(const Magnitude& other) const {
        Magnitude result;
        result.used_ = std::max(used_, other.used_);
        U128 carry = 0;
        for (std::size_t i = 0; i < result.used_; ++i) {
            const U128 sum = U128{words_[i]} + other.words_[i] + carry;
            result.words_[i] = static_cast<U64>(sum);
            carry = sum >> 64;
        }
        if (carry != 0) {
            if (result.used_ == CAPACITY_WORDS)
                invalid_internal_value();
            result.words_[result.used_++] = static_cast<U64>(carry);
        }
        return result;
    }

    Magnitude subtracted(const Magnitude& other) const {
        if (compare(other) < 0)
            invalid_internal_value();
        Magnitude result;
        result.used_ = used_;
        U128 borrow = 0;
        for (std::size_t i = 0; i < used_; ++i) {
            const U128 right = U128{other.words_[i]} + borrow;
            const U128 left = words_[i];
            result.words_[i] = static_cast<U64>(left - right);
            borrow = left < right;
        }
        if (borrow != 0)
            invalid_internal_value();
        while (result.used_ != 0 && result.words_[result.used_ - 1] == 0)
            --result.used_;
        return result;
    }

private:
    std::array<U64, CAPACITY_WORDS> words_{};
    std::size_t used_ = 0;
};

struct Format {
    unsigned width, exponent_bits, fraction_bits;
    unsigned precision() const noexcept { return fraction_bits + 1; }
    int bias() const noexcept { return (1 << (exponent_bits - 1)) - 1; }
    int emin() const noexcept { return 1 - bias(); }
    U64 mask() const noexcept { return low_mask(width); }
    U64 sign() const noexcept { return U64{1} << (width - 1); }
    U64 fraction_mask() const noexcept { return low_mask(fraction_bits); }
    U64 exponent_max() const noexcept { return low_mask(exponent_bits); }
    U64 infinity() const noexcept { return exponent_max() << fraction_bits; }
    U64 nan() const noexcept { return infinity() | (U64{1} << (fraction_bits - 1)); }
};

constexpr Format FP16{16, 5, 10}, BF16{16, 8, 7};
constexpr Format FP32{32, 8, 23}, FP64{64, 11, 52};
enum class Kind { zero, finite, infinity, nan };

struct Exact {
    Kind kind = Kind::zero;
    bool sign = false;
    Magnitude magnitude;
    int exponent = 0;
};

struct ExactResult {
    Exact value;
    unsigned flags = 0;
};

struct Rounded {
    U64 value;
    unsigned flags;
};

Exact special(Kind kind, bool sign = false, bool signalling = false) {
    return {kind, sign, Magnitude(signalling ? 1 : 0), 0};
}

Exact decode(const Format& fmt, U64 value) {
    value &= fmt.mask();
    const bool sign = (value & fmt.sign()) != 0;
    const U64 exponent = (value >> fmt.fraction_bits) & fmt.exponent_max();
    const U64 fraction = value & fmt.fraction_mask();
    if (exponent == fmt.exponent_max()) {
        if (fraction == 0)
            return special(Kind::infinity, sign);
        return special(Kind::nan, sign,
                       (fraction & (U64{1} << (fmt.fraction_bits - 1))) == 0);
    }
    if (exponent == 0) {
        if (fraction == 0)
            return special(Kind::zero, sign);
        return {Kind::finite, sign, Magnitude(fraction),
                fmt.emin() - static_cast<int>(fmt.fraction_bits)};
    }
    return {Kind::finite, sign,
            Magnitude(fraction | (U64{1} << fmt.fraction_bits)),
            static_cast<int>(exponent) - fmt.bias()
                - static_cast<int>(fmt.fraction_bits)};
}

bool signalling(const Exact& value) noexcept {
    return value.kind == Kind::nan && !value.magnitude.zero();
}

bool increment(unsigned rm, bool sign, U64 retained,
               const Magnitude& magnitude, unsigned shift, bool sticky) {
    if (shift == 0)
        invalid_internal_value();
    const bool half = magnitude.bit(shift - 1);
    const bool below = magnitude.any_below(shift - 1) || sticky;
    switch (rm) {
    case RNE: return half && (below || (retained & 1));
    case RTZ: return false;
    case RDN: return sign && (half || below);
    case RUP: return !sign && (half || below);
    case RMM: return half;
    default: invalid_internal_value();
    }
}

U64 overflow(const Format& fmt, bool sign, unsigned rm) {
    const bool infinity = rm == RNE || rm == RMM
        || (rm == RDN && sign) || (rm == RUP && !sign);
    return (sign ? fmt.sign() : 0)
        | (infinity ? fmt.infinity() : fmt.infinity() - 1);
}

bool tiny_after_rounding(const Format& fmt, const Exact& value,
                         int lead, unsigned rm, bool sticky) {
    if (lead < fmt.emin() - 1)
        return true;
    const int shift = lead - static_cast<int>(fmt.precision() - 1)
        - value.exponent;
    if (shift <= 0)
        return true;
    U64 mantissa = value.magnitude.shifted_word(static_cast<unsigned>(shift));
    if (increment(rm, value.sign, mantissa, value.magnitude,
                  static_cast<unsigned>(shift), sticky))
        ++mantissa;
    return (mantissa >> fmt.precision()) == 0;
}

Rounded encode(const Format& fmt, const Exact& value, unsigned rm,
               bool sticky = false) {
    const U64 sign_bits = value.sign ? fmt.sign() : 0;
    if (value.kind == Kind::nan)
        return {fmt.nan(), signalling(value) ? NV : 0};
    if (value.kind == Kind::infinity)
        return {sign_bits | fmt.infinity(), 0};
    if (value.kind == Kind::zero)
        return {sign_bits, 0};
    if (value.magnitude.zero())
        invalid_internal_value();

    const unsigned precision = fmt.precision();
    const int lead = value.exponent + static_cast<int>(value.magnitude.bits()) - 1;
    int quantum = std::max(lead - static_cast<int>(precision - 1),
                           fmt.emin() - static_cast<int>(precision - 1));
    const int shift = quantum - value.exponent;
    unsigned flags = 0;
    U64 mantissa;
    if (shift <= 0) {
        if (sticky || -shift >= 64
            || value.magnitude.bits() + static_cast<unsigned>(-shift) > 64)
            invalid_internal_value();
        mantissa = value.magnitude.to_word() << static_cast<unsigned>(-shift);
    } else {
        const unsigned discard = static_cast<unsigned>(shift);
        mantissa = value.magnitude.shifted_word(discard);
        if (value.magnitude.any_below(discard) || sticky) {
            flags |= NX;
            if (lead < fmt.emin() && tiny_after_rounding(fmt, value, lead, rm, sticky))
                flags |= UF;
        }
        if (increment(rm, value.sign, mantissa, value.magnitude, discard, sticky)) {
            ++mantissa;
            if (mantissa >> precision) {
                mantissa >>= 1;
                ++quantum;
            }
        }
    }
    const U64 hidden = U64{1} << (precision - 1);
    if (mantissa >= hidden) {
        const int biased = quantum + static_cast<int>(precision - 1) + fmt.bias();
        if (biased >= static_cast<int>(fmt.exponent_max()))
            return {overflow(fmt, value.sign, rm), flags | OF | NX};
        if (biased <= 0)
            invalid_internal_value();
        return {sign_bits | (static_cast<U64>(biased) << fmt.fraction_bits)
                    | (mantissa - hidden), flags};
    }
    return {sign_bits | mantissa, flags};
}

ExactResult exact_add(const Exact& left, const Exact& right, unsigned rm) {
    if (left.kind == Kind::nan || right.kind == Kind::nan)
        return {special(Kind::nan), signalling(left) || signalling(right) ? NV : 0};
    if (left.kind == Kind::infinity || right.kind == Kind::infinity) {
        if (left.kind == Kind::infinity && right.kind == Kind::infinity
            && left.sign != right.sign)
            return {special(Kind::nan), NV};
        return {left.kind == Kind::infinity ? left : right, 0};
    }
    if (left.kind == Kind::zero && right.kind == Kind::zero)
        return {special(Kind::zero, rm == RDN ? left.sign || right.sign
                                             : left.sign && right.sign), 0};
    if (left.kind == Kind::zero)
        return {right, 0};
    if (right.kind == Kind::zero)
        return {left, 0};
    const int common = std::min(left.exponent, right.exponent);
    const Magnitude a = left.magnitude.shifted_left(
        static_cast<unsigned>(left.exponent - common));
    const Magnitude b = right.magnitude.shifted_left(
        static_cast<unsigned>(right.exponent - common));
    if (left.sign == right.sign)
        return {{Kind::finite, left.sign, a.added(b), common}, 0};
    const int order = a.compare(b);
    if (order == 0)
        return {special(Kind::zero, rm == RDN), 0};
    return {{Kind::finite, order > 0 ? left.sign : right.sign,
             order > 0 ? a.subtracted(b) : b.subtracted(a), common}, 0};
}

ExactResult exact_mul(const Exact& left, const Exact& right) {
    if (left.kind == Kind::nan || right.kind == Kind::nan)
        return {special(Kind::nan), signalling(left) || signalling(right) ? NV : 0};
    const bool sign = left.sign != right.sign;
    if ((left.kind == Kind::infinity && right.kind == Kind::zero)
        || (left.kind == Kind::zero && right.kind == Kind::infinity))
        return {special(Kind::nan), NV};
    if (left.kind == Kind::infinity || right.kind == Kind::infinity)
        return {special(Kind::infinity, sign), 0};
    if (left.kind == Kind::zero || right.kind == Kind::zero)
        return {special(Kind::zero, sign), 0};
    return {{Kind::finite, sign,
             Magnitude::from_wide(U128{left.magnitude.to_word()}
                                  * right.magnitude.to_word()),
             left.exponent + right.exponent}, 0};
}

Rounded round_exact(const Format& fmt, const ExactResult& result, unsigned rm) {
    Rounded rounded = encode(fmt, result.value, rm);
    rounded.flags |= result.flags;
    return rounded;
}

Rounded fused(const Format& fmt, const Exact& left, const Exact& right,
              const Exact& addend, unsigned rm) {
    const ExactResult product = exact_mul(left, right);
    if (addend.kind == Kind::nan)
        return {fmt.nan(), product.flags | (signalling(addend) ? NV : 0)};
    ExactResult total = exact_add(product.value, addend, rm);
    total.flags |= product.flags;
    return round_exact(fmt, total, rm);
}

U128 shift_wide(U64 value, unsigned shift) {
    if (shift >= 128 || word_bits(value) + shift > 128)
        invalid_internal_value();
    return U128{value} << shift;
}

Rounded divide(const Format& fmt, const Exact& left, const Exact& right,
               unsigned rm) {
    if (left.kind == Kind::nan || right.kind == Kind::nan)
        return {fmt.nan(), signalling(left) || signalling(right) ? NV : 0};
    const bool sign = left.sign != right.sign;
    const U64 sign_bits = sign ? fmt.sign() : 0;
    if ((left.kind == Kind::infinity && right.kind == Kind::infinity)
        || (left.kind == Kind::zero && right.kind == Kind::zero))
        return {fmt.nan(), NV};
    if (left.kind == Kind::infinity)
        return {sign_bits | fmt.infinity(), 0};
    if (right.kind == Kind::infinity || left.kind == Kind::zero)
        return {sign_bits, 0};
    if (right.kind == Kind::zero)
        return {sign_bits | fmt.infinity(), DZ};
    const unsigned extra = static_cast<unsigned>(std::max(
        0, static_cast<int>(fmt.precision() + 3 + right.magnitude.bits())
            - static_cast<int>(left.magnitude.bits())));
    // At most p+3+p = 109 numerator bits for binary64.
    const U128 numerator = shift_wide(left.magnitude.to_word(), extra);
    const U64 denominator = right.magnitude.to_word();
    if (denominator == 0)
        invalid_internal_value();
    return encode(fmt, {Kind::finite, sign,
                       Magnitude::from_wide(numerator / denominator),
                       left.exponent - right.exponent - static_cast<int>(extra)},
                  rm, numerator % denominator != 0);
}

U128 integer_sqrt(U128 value) noexcept {
    U128 root = 0;
    U128 bit = U128{1} << 126;
    while (bit > value)
        bit >>= 2;
    while (bit != 0) {
        if (value >= root + bit) {
            value -= root + bit;
            root = (root >> 1) + bit;
        } else {
            root >>= 1;
        }
        bit >>= 2;
    }
    return root;
}

Rounded square_root(const Format& fmt, const Exact& value, U64 raw, unsigned rm) {
    if (value.kind == Kind::nan)
        return {fmt.nan(), signalling(value) ? NV : 0};
    if (value.kind == Kind::zero)
        return {raw & fmt.mask(), 0};
    if (value.sign)
        return {fmt.nan(), NV};
    if (value.kind == Kind::infinity)
        return {fmt.infinity(), 0};
    U64 significand = value.magnitude.to_word();
    int exponent = value.exponent;
    if (exponent % 2 != 0) {
        significand <<= 1;
        --exponent;
    }
    const unsigned scale = static_cast<unsigned>(std::max(
        0, (2 * static_cast<int>(fmt.precision() + 3)
             - static_cast<int>(word_bits(significand)) + 1) / 2));
    // The scaled radicand is at most 113 bits, including odd-length padding.
    const U128 radicand = shift_wide(significand, 2 * scale);
    const U128 root = integer_sqrt(radicand);
    return encode(fmt, {Kind::finite, false, Magnitude::from_wide(root),
                       (exponent - 2 * static_cast<int>(scale)) / 2},
                  rm, root * root != radicand);
}

struct Integral {
    Magnitude magnitude;
    bool inexact;
};

Integral round_integer(const Exact& value, unsigned rm) {
    if (value.kind == Kind::zero)
        return {{}, false};
    if (value.kind != Kind::finite)
        invalid_internal_value();
    if (value.exponent >= 0)
        return {value.magnitude.shifted_left(static_cast<unsigned>(value.exponent)),
                false};
    const unsigned shift = static_cast<unsigned>(-value.exponent);
    U64 magnitude = value.magnitude.shifted_word(shift);
    const bool inexact = value.magnitude.any_below(shift);
    if (increment(rm, value.sign, magnitude, value.magnitude, shift, false)) {
        if (magnitude == MASK64)
            invalid_internal_value();
        ++magnitude;
    }
    return {Magnitude(magnitude), inexact};
}

Rounded to_integer(const Exact& value, bool is_signed, unsigned rm) {
    const U64 upper = is_signed ? (U64{1} << 63) - 1 : MASK64;
    const U64 lower_bits = is_signed ? U64{1} << 63 : 0;
    if (value.kind == Kind::nan)
        return {0, NV};
    if (value.kind == Kind::infinity)
        return {value.sign ? lower_bits : upper, NV};
    const Integral rounded = round_integer(value, rm);
    const U64 bound = value.sign ? lower_bits : upper;
    if (rounded.magnitude.compare(Magnitude(bound)) > 0)
        return {value.sign ? lower_bits : upper, NV};
    const U64 magnitude = rounded.magnitude.to_word();
    return {value.sign ? U64{0} - magnitude : magnitude, rounded.inexact ? NX : 0};
}

Rounded round_integral(const Format& fmt, const Exact& value, U64 raw, unsigned rm) {
    if (value.kind == Kind::nan)
        return {fmt.nan(), signalling(value) ? NV : 0};
    if (value.kind != Kind::finite)
        return {raw & fmt.mask(), 0};
    const Integral rounded = round_integer(value, rm);
    if (rounded.magnitude.zero())
        return {value.sign ? fmt.sign() : 0, 0};
    const Rounded encoded = encode(fmt, {Kind::finite, value.sign,
                                        rounded.magnitude, 0}, rm);
    return {encoded.value, 0};  // FRND deliberately raises no NX.
}

U64 order_key(const Format& fmt, U64 bits) noexcept {
    bits &= fmt.mask();
    return bits & fmt.sign() ? fmt.mask() ^ bits : bits | fmt.sign();
}

int compare(const Format& fmt, U64 a, U64 b, const Exact& left,
            const Exact& right) noexcept {
    if (left.kind == Kind::nan || right.kind == Kind::nan)
        return 2;
    if (left.kind == Kind::zero && right.kind == Kind::zero)
        return 0;
    const U64 ka = order_key(fmt, a), kb = order_key(fmt, b);
    return ka < kb ? -1 : (ka > kb ? 1 : 0);
}

U64 classify(const Format& fmt, U64 bits, const Exact& value) noexcept {
    unsigned bit;
    if (value.kind == Kind::nan)
        bit = signalling(value) ? 8 : 9;
    else if (value.kind == Kind::infinity)
        bit = value.sign ? 0 : 7;
    else if (value.kind == Kind::zero)
        bit = value.sign ? 3 : 4;
    else if (((bits >> fmt.fraction_bits) & fmt.exponent_max()) == 0)
        bit = value.sign ? 2 : 5;
    else
        bit = value.sign ? 1 : 6;
    return U64{1} << bit;
}

Outcome outcome(Rounded value) noexcept {
    return {value.value, U64{value.flags} << 4, 0, false};
}

bool dynamic_rounding(unsigned code) noexcept {
    return code <= 4 || code == 7 || code == 8
        || (code >= 0x38 && code <= 0x3B) || code == 0x3D
        || (code >= 0x20 && code < 0x38 && (code & 7) == 7);
}

}  // namespace

unsigned instruction_length(unsigned op) noexcept {
    const unsigned code = op & 0x3F;
    return code == 7 || code == 8 ? 4 : 3;
}

unsigned extra_cycles(unsigned op) noexcept {
    const unsigned code = op & 0x3F;
    if (code == 3 || code == 4)
        return (op >> 6) == 0 ? 15 : 30;
    if (code == 5 || code == 6 || (code >= 0x10 && code <= 0x14))
        return 1;
    return 3;
}

const char* validate(unsigned op, unsigned t_byte, U64 fpcsr) noexcept {
    if ((op >> 6) > 1)
        return "reserved FC format";
    const unsigned code = op & 0x3F;
    if (!(code <= 8 || (code >= 0x10 && code <= 0x14)
          || (code >= 0x20 && code <= 0x3E)))
        return "reserved FC operation";
    if ((code == 7 || code == 8) && (t_byte >> 5) != 0)
        return "FC T byte bits [7:5] must be zero";
    if (code >= 0x20 && code < 0x38 && ((code & 7) == 5 || (code & 7) == 6))
        return "reserved FC rounding mode";
    if (dynamic_rounding(code) && (fpcsr & 7) > RMM)
        return "reserved FPCSR.RM for a dynamic mode";
    return nullptr;
}

Outcome execute(unsigned op, U64 rd, U64 rs, U64 rt, U64 fpcsr) {
    const Format& fmt = (op >> 6) == 0 ? FP32 : FP64;
    const unsigned code = op & 0x3F;
    const unsigned dynamic = static_cast<unsigned>(fpcsr & 7);
    const U64 d = rd & fmt.mask(), s = rs & fmt.mask();
    const Exact left = decode(fmt, d), right = decode(fmt, s);
    switch (code) {
    case 0: return outcome(round_exact(fmt, exact_add(left, right, dynamic), dynamic));
    case 1: {
        Exact negated = right;
        if (negated.kind != Kind::nan)
            negated.sign = !negated.sign;
        return outcome(round_exact(fmt, exact_add(left, negated, dynamic), dynamic));
    }
    case 2: return outcome(round_exact(fmt, exact_mul(left, right), dynamic));
    case 3: return outcome(divide(fmt, left, right, dynamic));
    case 4: return outcome(square_root(fmt, right, s, dynamic));
    case 5:
    case 6: {
        const unsigned flags = signalling(left) || signalling(right) ? NV : 0;
        if (left.kind == Kind::nan || right.kind == Kind::nan)
            return outcome({fmt.nan(), flags});
        const U64 ka = order_key(fmt, d), kb = order_key(fmt, s);
        return outcome({(code == 5 ? ka <= kb : ka >= kb) ? d : s, flags});
    }
    case 7:
    case 8: {
        Exact multiplicand = right;
        if (code == 8)
            multiplicand.sign = !multiplicand.sign;
        return outcome(fused(fmt, multiplicand, decode(fmt, rt), left, dynamic));
    }
    case 0x10:
    case 0x11:
    case 0x12:
    case 0x13: {
        const int relation = compare(fmt, d, s, left, right);
        const bool invalid = code >= 0x12
            ? left.kind == Kind::nan || right.kind == Kind::nan
            : signalling(left) || signalling(right);
        if (code == 0x10)
            return {0, invalid ? U64{NV} << 4 : 0, relation, true};
        const bool holds = code == 0x11 ? relation == 0
            : (code == 0x12 ? relation == -1 : relation == -1 || relation == 0);
        return outcome({holds ? MASK64 : 0, invalid ? NV : 0});
    }
    case 0x14: return outcome({classify(fmt, s, right), 0});
    case 0x38:
    case 0x39: {
        const bool negative = code == 0x38 && (rs >> 63) != 0;
        const U64 magnitude = negative ? U64{0} - rs : rs;
        return outcome(encode(fmt, magnitude == 0 ? special(Kind::zero)
            : Exact{Kind::finite, negative, Magnitude(magnitude), 0}, dynamic));
    }
    case 0x3A: return outcome(encode(fmt, decode((op >> 6) == 0 ? FP64 : FP32, rs), dynamic));
    case 0x3B: return outcome(encode(FP16, right, dynamic));
    case 0x3C: return outcome(encode(fmt, decode(FP16, rs), RNE));
    case 0x3D: return outcome(encode(BF16, right, dynamic));
    case 0x3E: return outcome(encode(fmt, decode(BF16, rs), RNE));
    default:
        if (code >= 0x20 && code < 0x38) {
            const unsigned mode = (code & 7) == 7 ? dynamic : code & 7;
            if (code < 0x28)
                return outcome(round_integral(fmt, right, s, mode));
            return outcome(to_integer(right, code < 0x30, mode));
        }
        throw std::logic_error("exact scalar FP execute requires a legal operation");
    }
}

}  // namespace megapad::scalar_fp
