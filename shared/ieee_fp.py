"""Exact IEEE 754 reference arithmetic for MegaPad floating-point formats.

``docs/floating-point.md`` is the normative definition this module
implements.  Every backend (Python emulator, native accelerator, hosted
simulator, RTL vector generators) takes its floating-point values from here.

The module has two layers.

The *exact layer* is the definition.  Values are decoded to exact integer
descriptors ``(kind, sign, significand, exponent)`` meaning
``(-1)**sign * significand * 2**exponent``; operations combine them exactly
with Python integers and ``encode`` rounds once to a destination format under
any IEEE rounding direction, reporting the IEEE exception flags.

The *lane layer* serves tile operations: round-to-nearest-even, no flags, and
fast.  It uses host binary64 arithmetic only where that is provably identical
to the exact layer:

* binary64 addition, subtraction, and multiplication are correctly rounded;
* for formats of precision ``p <= 24`` an addition, subtraction, or
  multiplication computed in binary64 and rounded once to the format is
  correctly rounded, because ``53 >= 2p + 2``;
* a fused multiply-add for ``p <= 24`` uses an exact binary64 product and an
  error-free two-sum to form the round-to-odd binary64 sum, which rounds
  correctly to any format with ``53 >= p + 2``;
* binary64 fused multiply-add uses ``math.fma`` after special values are
  handled.

``tests/test_ieee_fp.py`` checks every lane-layer function against the exact
layer.
"""

from __future__ import annotations

import math
import struct
from dataclasses import dataclass
from typing import Sequence


# Rounding directions, numbered as FPCSR.RM.
RNE = 0
RTZ = 1
RDN = 2
RUP = 3
RMM = 4
ROUNDING_MODES = (RNE, RTZ, RDN, RUP, RMM)

# Exception flags, numbered as FPCSR[8:4] >> 4.
NX = 1 << 0
UF = 1 << 1
OF = 1 << 2
DZ = 1 << 3
NV = 1 << 4

# Exact-descriptor kinds.
ZERO = 0
FINITE = 1
INF = 2
NAN = 3

# Comparison results.
LESS = -1
EQUAL = 0
GREATER = 1
UNORDERED = 2


@dataclass(frozen=True)
class Format:
    """One binary interchange format."""

    name: str
    ew: int
    width: int
    exponent_bits: int
    fraction_bits: int

    @property
    def precision(self) -> int:
        return self.fraction_bits + 1

    @property
    def bias(self) -> int:
        return (1 << (self.exponent_bits - 1)) - 1

    @property
    def emin(self) -> int:
        return 1 - self.bias

    @property
    def mask(self) -> int:
        return (1 << self.width) - 1

    @property
    def sign_bit(self) -> int:
        return 1 << (self.width - 1)

    @property
    def exponent_field_max(self) -> int:
        return (1 << self.exponent_bits) - 1

    @property
    def fraction_mask(self) -> int:
        return (1 << self.fraction_bits) - 1

    @property
    def infinity(self) -> int:
        return self.exponent_field_max << self.fraction_bits

    @property
    def canonical_nan(self) -> int:
        return self.infinity | (1 << (self.fraction_bits - 1))

    @property
    def max_finite(self) -> int:
        return self.infinity - 1


FP16 = Format("fp16", 4, 16, 5, 10)
BF16 = Format("bf16", 5, 16, 8, 7)
FP32 = Format("fp32", 6, 32, 8, 23)
FP64 = Format("fp64", 7, 64, 11, 52)

FORMATS = (FP16, BF16, FP32, FP64)
# Formats whose binary64 double rounding is innocuous (53 >= 2p + 2).
NARROW_FORMATS = (FP16, BF16, FP32)
FORMAT_BY_EW = {fmt.ew: fmt for fmt in FORMATS}
ACCUMULATION_FORMAT = {FP16: FP32, BF16: FP32, FP32: FP64, FP64: FP64}


def accumulation_format(fmt: Format) -> Format:
    """Return the format ``A`` in which ``fmt`` lanes accumulate."""

    return ACCUMULATION_FORMAT[fmt]


# ---------------------------------------------------------------------------
# Raw-bit classification
# ---------------------------------------------------------------------------

def is_nan(fmt: Format, bits: int) -> bool:
    bits &= fmt.mask
    return (bits & ~fmt.sign_bit) > fmt.infinity


def is_signalling_nan(fmt: Format, bits: int) -> bool:
    return is_nan(fmt, bits) and not (bits >> (fmt.fraction_bits - 1)) & 1


def order_key(fmt: Format, bits: int) -> int:
    """Return an integer key that orders non-NaN values, with -0 < +0."""

    bits &= fmt.mask
    if bits & fmt.sign_bit:
        return fmt.mask ^ bits
    return bits | fmt.sign_bit


def classify(fmt: Format, bits: int) -> int:
    """Return the one-hot FCLASS mask of ``bits``."""

    bits &= fmt.mask
    negative = bool(bits & fmt.sign_bit)
    magnitude = bits & ~fmt.sign_bit
    if magnitude > fmt.infinity:
        return 1 << 8 if is_signalling_nan(fmt, bits) else 1 << 9
    if magnitude == fmt.infinity:
        return 1 << 0 if negative else 1 << 7
    if magnitude == 0:
        return 1 << 3 if negative else 1 << 4
    if magnitude < (1 << fmt.fraction_bits):
        return 1 << 2 if negative else 1 << 5
    return 1 << 1 if negative else 1 << 6


# ---------------------------------------------------------------------------
# Exact layer
# ---------------------------------------------------------------------------

def decode(fmt: Format, bits: int) -> tuple[int, int, int, int]:
    """Decode raw bits to an exact descriptor.

    For NaN the significand field is 1 when the NaN is signalling.
    """

    bits &= fmt.mask
    sign = bits >> (fmt.width - 1)
    exponent_field = (bits >> fmt.fraction_bits) & fmt.exponent_field_max
    fraction = bits & fmt.fraction_mask
    if exponent_field == fmt.exponent_field_max:
        if fraction == 0:
            return (INF, sign, 0, 0)
        quiet = (fraction >> (fmt.fraction_bits - 1)) & 1
        return (NAN, sign, 0 if quiet else 1, 0)
    if exponent_field == 0:
        if fraction == 0:
            return (ZERO, sign, 0, 0)
        return (FINITE, sign, fraction, fmt.emin - fmt.fraction_bits)
    return (
        FINITE,
        sign,
        (1 << fmt.fraction_bits) | fraction,
        exponent_field - fmt.bias - fmt.fraction_bits,
    )


def _round_increment(rm: int, sign: int, quotient: int, remainder: int,
                     half: int, sticky: bool) -> bool:
    if rm == RNE:
        return remainder > half or (
            remainder == half and (sticky or bool(quotient & 1))
        )
    if rm == RTZ:
        return False
    if rm == RDN:
        return bool(sign) and (remainder != 0 or sticky)
    if rm == RUP:
        return not sign and (remainder != 0 or sticky)
    if rm == RMM:
        return remainder >= half
    raise ValueError(f"unknown rounding mode {rm}")


def _overflow_result(fmt: Format, sign: int, rm: int) -> int:
    sign_bits = fmt.sign_bit if sign else 0
    if rm in (RNE, RMM):
        return sign_bits | fmt.infinity
    if rm == RTZ:
        return sign_bits | fmt.max_finite
    if rm == RDN:
        return sign_bits | (fmt.infinity if sign else fmt.max_finite)
    return sign_bits | (fmt.max_finite if sign else fmt.infinity)


def encode(fmt: Format, value: tuple[int, int, int, int], rm: int = RNE,
           sticky: bool = False) -> tuple[int, int]:
    """Round an exact descriptor once to ``fmt``; return ``(bits, flags)``.

    ``sticky`` states that the true magnitude lies strictly between
    ``significand * 2**exponent`` and ``(significand + 1) * 2**exponent``.
    Callers that pass it must supply at least one significand bit below the
    rounding position.  Encoding a signalling NaN reports ``NV``, which is the
    conversion rule; arithmetic results are already quiet.
    """

    kind, sign, significand, exponent = value
    sign_bits = fmt.sign_bit if sign else 0
    if kind == NAN:
        return fmt.canonical_nan, NV if significand else 0
    if kind == INF:
        return sign_bits | fmt.infinity, 0
    if kind == ZERO:
        return sign_bits, 0

    precision = fmt.precision
    lead_exponent = exponent + significand.bit_length() - 1
    quantum = max(lead_exponent - (precision - 1), fmt.emin - (precision - 1))
    shift = quantum - exponent
    flags = 0
    if shift <= 0:
        if sticky:
            raise ValueError("sticky value lacks a rounding bit")
        mantissa = significand << -shift
    else:
        mantissa = significand >> shift
        remainder = significand & ((1 << shift) - 1)
        half = 1 << (shift - 1)
        if remainder or sticky:
            flags |= NX
            if lead_exponent < fmt.emin and _tiny_after_rounding(
                fmt, sign, significand, exponent, lead_exponent, rm, sticky
            ):
                flags |= UF
        if _round_increment(rm, sign, mantissa, remainder, half, sticky):
            mantissa += 1
            if mantissa >> precision:
                mantissa >>= 1
                quantum += 1

    hidden = 1 << (precision - 1)
    if mantissa >= hidden:
        biased = quantum + (precision - 1) + fmt.bias
        if biased >= fmt.exponent_field_max:
            return _overflow_result(fmt, sign, rm), flags | OF | NX
        return (
            sign_bits | (biased << fmt.fraction_bits) | (mantissa - hidden),
            flags,
        )
    return sign_bits | mantissa, flags


def _tiny_after_rounding(fmt: Format, sign: int, significand: int,
                         exponent: int, lead_exponent: int, rm: int,
                         sticky: bool) -> bool:
    """IEEE tininess after rounding: round to ``p`` bits, unbounded range."""

    if lead_exponent < fmt.emin - 1:
        return True
    shift = lead_exponent - (fmt.precision - 1) - exponent
    if shift <= 0:
        return True
    mantissa = significand >> shift
    remainder = significand & ((1 << shift) - 1)
    if _round_increment(rm, sign, mantissa, remainder, 1 << (shift - 1),
                        sticky):
        mantissa += 1
    return not (mantissa >> fmt.precision)


def _quiet_nan(flags: int = 0) -> tuple[tuple[int, int, int, int], int]:
    return (NAN, 0, 0, 0), flags


def _nan_flags(*values: tuple[int, int, int, int]) -> int:
    return NV if any(v[0] == NAN and v[2] for v in values) else 0


def exact_negate(x: tuple[int, int, int, int]) -> tuple[int, int, int, int]:
    return (x[0], x[1] ^ 1 if x[0] != NAN else x[1], x[2], x[3])


def exact_add(x, y, rm: int = RNE):
    """Return the exact sum descriptor and its invalid flag."""

    kx, sx, mx, ex = x
    ky, sy, my, ey = y
    if kx == NAN or ky == NAN:
        return _quiet_nan(_nan_flags(x, y))
    if kx == INF or ky == INF:
        if kx == INF and ky == INF and sx != sy:
            return _quiet_nan(NV)
        return ((INF, sx, 0, 0) if kx == INF else (INF, sy, 0, 0)), 0
    if kx == ZERO and ky == ZERO:
        sign = (sx | sy) if rm == RDN else (sx & sy)
        return (ZERO, sign, 0, 0), 0
    if kx == ZERO:
        return y, 0
    if ky == ZERO:
        return x, 0
    common = min(ex, ey)
    total = ((-mx if sx else mx) << (ex - common)) + (
        (-my if sy else my) << (ey - common)
    )
    if total == 0:
        return (ZERO, 1 if rm == RDN else 0, 0, 0), 0
    return (FINITE, 1 if total < 0 else 0, abs(total), common), 0


def exact_mul(x, y):
    """Return the exact product descriptor and its invalid flag."""

    kx, sx, mx, ex = x
    ky, sy, my, ey = y
    if kx == NAN or ky == NAN:
        return _quiet_nan(_nan_flags(x, y))
    sign = sx ^ sy
    if (kx == INF and ky == ZERO) or (kx == ZERO and ky == INF):
        return _quiet_nan(NV)
    if kx == INF or ky == INF:
        return (INF, sign, 0, 0), 0
    if kx == ZERO or ky == ZERO:
        return (ZERO, sign, 0, 0), 0
    return (FINITE, sign, mx * my, ex + ey), 0


def exact_fma(x, y, z, rm: int = RNE):
    """Return the exact ``x * y + z`` descriptor and its invalid flags."""

    product, flags = exact_mul(x, y)
    if z[0] == NAN:
        return _quiet_nan(flags | _nan_flags(z))
    total, add_flags = exact_add(product, z, rm)
    return total, flags | add_flags


def exact_from_int(value: int) -> tuple[int, int, int, int]:
    if value == 0:
        return (ZERO, 0, 0, 0)
    return (FINITE, 1 if value < 0 else 0, abs(value), 0)


def compare_exact(x, y) -> int:
    """Compare two descriptors; -0 equals +0."""

    if x[0] == NAN or y[0] == NAN:
        return UNORDERED
    if x[0] == INF or y[0] == INF:
        if x[0] == INF and y[0] == INF and x[1] == y[1]:
            return EQUAL
        if x[0] == INF:
            return LESS if x[1] else GREATER
        return GREATER if y[1] else LESS
    difference, _ = exact_add(x, exact_negate(y))
    if difference[0] == ZERO:
        return EQUAL
    return LESS if difference[1] else GREATER


# Raw-bit operations with any rounding direction.  These are the definition.

def add(fmt: Format, a: int, b: int, rm: int = RNE) -> tuple[int, int]:
    value, flags = exact_add(decode(fmt, a), decode(fmt, b), rm)
    bits, round_flags = encode(fmt, value, rm)
    return bits, flags | round_flags


def sub(fmt: Format, a: int, b: int, rm: int = RNE) -> tuple[int, int]:
    return add(fmt, a, (b ^ fmt.sign_bit) if not is_nan(fmt, b) else b, rm)


def mul(fmt: Format, a: int, b: int, rm: int = RNE) -> tuple[int, int]:
    value, flags = exact_mul(decode(fmt, a), decode(fmt, b))
    bits, round_flags = encode(fmt, value, rm)
    return bits, flags | round_flags


def fma(fmt: Format, a: int, b: int, c: int, rm: int = RNE
        ) -> tuple[int, int]:
    """Return ``RN(a * b + c)`` with one rounding."""

    value, flags = exact_fma(
        decode(fmt, a), decode(fmt, b), decode(fmt, c), rm
    )
    bits, round_flags = encode(fmt, value, rm)
    return bits, flags | round_flags


def mixed_fma(dst: Format, src: Format, a: int, b: int, c: int,
              rm: int = RNE) -> tuple[int, int]:
    """Return ``RN_dst(a * b + c)`` with ``a, b`` in ``src`` and ``c`` in ``dst``."""

    value, flags = exact_fma(
        decode(src, a), decode(src, b), decode(dst, c), rm
    )
    bits, round_flags = encode(dst, value, rm)
    return bits, flags | round_flags


def product(dst: Format, src: Format, a: int, b: int, rm: int = RNE
            ) -> tuple[int, int]:
    """Return ``RN_dst(a * b)`` with ``a, b`` in ``src``."""

    value, flags = exact_mul(decode(src, a), decode(src, b))
    bits, round_flags = encode(dst, value, rm)
    return bits, flags | round_flags


def div(fmt: Format, a: int, b: int, rm: int = RNE) -> tuple[int, int]:
    x = decode(fmt, a)
    y = decode(fmt, b)
    if x[0] == NAN or y[0] == NAN:
        return fmt.canonical_nan, _nan_flags(x, y)
    sign = x[1] ^ y[1]
    sign_bits = fmt.sign_bit if sign else 0
    if (x[0] == INF and y[0] == INF) or (x[0] == ZERO and y[0] == ZERO):
        return fmt.canonical_nan, NV
    if x[0] == INF:
        return sign_bits | fmt.infinity, 0
    if y[0] == INF or x[0] == ZERO:
        return sign_bits, 0
    if y[0] == ZERO:
        return sign_bits | fmt.infinity, DZ
    extra = max(
        0,
        fmt.precision + 2 + y[2].bit_length() - x[2].bit_length() + 1,
    )
    quotient, remainder = divmod(x[2] << extra, y[2])
    return encode(
        fmt,
        (FINITE, sign, quotient, x[3] - y[3] - extra),
        rm,
        sticky=remainder != 0,
    )


def sqrt(fmt: Format, a: int, rm: int = RNE) -> tuple[int, int]:
    x = decode(fmt, a)
    if x[0] == NAN:
        return fmt.canonical_nan, _nan_flags(x)
    if x[0] == ZERO:
        return a & fmt.mask, 0
    if x[1]:
        return fmt.canonical_nan, NV
    if x[0] == INF:
        return fmt.infinity, 0
    significand, exponent = x[2], x[3]
    if exponent & 1:
        significand <<= 1
        exponent -= 1
    scale = max(0, (2 * (fmt.precision + 3) - significand.bit_length() + 1) // 2)
    radicand = significand << (2 * scale)
    root = math.isqrt(radicand)
    return encode(
        fmt,
        (FINITE, 0, root, (exponent - 2 * scale) // 2),
        rm,
        sticky=root * root != radicand,
    )


def convert(dst: Format, src: Format, bits: int, rm: int = RNE
            ) -> tuple[int, int]:
    """Convert between float formats with one rounding."""

    return encode(dst, decode(src, bits), rm)


def compare(fmt: Format, a: int, b: int) -> int:
    return compare_exact(decode(fmt, a), decode(fmt, b))


def _round_to_integer(value, rm: int) -> tuple[int, bool]:
    """Round a finite descriptor to an integer; return ``(n, inexact)``."""

    kind, sign, significand, exponent = value
    if kind == ZERO:
        return 0, False
    if exponent >= 0:
        magnitude = significand << exponent
        inexact = False
    else:
        shift = -exponent
        magnitude = significand >> shift
        remainder = significand & ((1 << shift) - 1)
        inexact = remainder != 0
        if _round_increment(rm, sign, magnitude, remainder, 1 << (shift - 1),
                            False):
            magnitude += 1
    return (-magnitude if sign else magnitude), inexact


def to_int(fmt: Format, bits: int, width: int, signed: bool,
           rm: int = RTZ) -> tuple[int, int]:
    """Convert to a saturating integer; NaN gives 0.  Return two's complement."""

    value = decode(fmt, bits)
    low = -(1 << (width - 1)) if signed else 0
    high = (1 << (width - 1)) - 1 if signed else (1 << width) - 1
    if value[0] == NAN:
        return 0, NV
    if value[0] == INF:
        result, flags = (low if value[1] else high), NV
    else:
        n, inexact = _round_to_integer(value, rm)
        if n < low:
            result, flags = low, NV
        elif n > high:
            result, flags = high, NV
        else:
            result, flags = n, NX if inexact else 0
    return result & ((1 << width) - 1), flags


def from_int(fmt: Format, value: int, rm: int = RNE) -> tuple[int, int]:
    return encode(fmt, exact_from_int(value), rm)


def round_integral(fmt: Format, bits: int, rm: int) -> tuple[int, int]:
    """Round to an integral value; raises no inexact flag."""

    value = decode(fmt, bits)
    if value[0] == NAN:
        return fmt.canonical_nan, _nan_flags(value)
    if value[0] != FINITE:
        return bits & fmt.mask, 0
    n, _ = _round_to_integer(value, rm)
    if n == 0:
        return fmt.sign_bit if value[1] else 0, 0
    result, _ = encode(fmt, exact_from_int(n), rm)
    return result, 0


def minimum(fmt: Format, a: int, b: int) -> int:
    """IEEE 754-2019 ``minimum``: NaN propagates, -0 < +0."""

    if is_nan(fmt, a) or is_nan(fmt, b):
        return fmt.canonical_nan
    return a if order_key(fmt, a) <= order_key(fmt, b) else b


def maximum(fmt: Format, a: int, b: int) -> int:
    """IEEE 754-2019 ``maximum``: NaN propagates, -0 < +0."""

    if is_nan(fmt, a) or is_nan(fmt, b):
        return fmt.canonical_nan
    return a if order_key(fmt, a) >= order_key(fmt, b) else b


def extreme_index(fmt: Format, values: Sequence[int], largest: bool
                  ) -> tuple[int, int]:
    """Return ``(index, bits)`` of the NaN-skipping minimum or maximum.

    Ties keep the lowest index.  An all-NaN sequence gives
    ``(0, canonical NaN)``.
    """

    best_index = -1
    best_key = 0
    for index, bits in enumerate(values):
        if is_nan(fmt, bits):
            continue
        key = order_key(fmt, bits)
        if best_index < 0 or (key > best_key if largest else key < best_key):
            best_index = index
            best_key = key
    if best_index < 0:
        return 0, fmt.canonical_nan
    return best_index, values[best_index] & fmt.mask


def skip_nan_extreme(fmt: Format, a: int, b: int, largest: bool) -> int:
    """Two-operand NaN-skipping minimum or maximum (TRED under ACC_ACC)."""

    return extreme_index(fmt, (a, b), largest)[1]


# ---------------------------------------------------------------------------
# Host binary64 bridge
# ---------------------------------------------------------------------------

_PACK_D = struct.Struct("<d").pack
_UNPACK_D = struct.Struct("<d").unpack
_PACK_Q = struct.Struct("<Q").pack
_UNPACK_Q = struct.Struct("<Q").unpack
_PACK_F = struct.Struct("<f").pack
_UNPACK_F = struct.Struct("<f").unpack
_PACK_I = struct.Struct("<I").pack
_UNPACK_I = struct.Struct("<I").unpack
_PACK_E = struct.Struct("<e").pack
_UNPACK_E = struct.Struct("<e").unpack
_PACK_H = struct.Struct("<H").pack
_UNPACK_H = struct.Struct("<H").unpack


def to_double(fmt: Format, bits: int) -> float:
    """Return the exact host binary64 value of ``bits`` (NaN is a host NaN)."""

    if fmt is FP16:
        return _UNPACK_E(_PACK_H(bits & 0xFFFF))[0]
    if fmt is BF16:
        return _UNPACK_F(_PACK_I((bits & 0xFFFF) << 16))[0]
    if fmt is FP32:
        return _UNPACK_F(_PACK_I(bits & 0xFFFF_FFFF))[0]
    return _UNPACK_D(_PACK_Q(bits & 0xFFFF_FFFF_FFFF_FFFF))[0]


def double_bits(value: float) -> int:
    return _UNPACK_Q(_PACK_D(value))[0]


def from_double(fmt: Format, value: float) -> int:
    """Round a host binary64 value once to ``fmt`` (RNE, canonical NaN)."""

    if value != value:
        return fmt.canonical_nan
    if fmt is FP64:
        return _UNPACK_Q(_PACK_D(value))[0]
    if fmt is FP32:
        try:
            return _UNPACK_I(_PACK_F(value))[0]
        except OverflowError:
            return 0xFF80_0000 if value < 0.0 else 0x7F80_0000
    if fmt is FP16:
        try:
            return _UNPACK_H(_PACK_E(value))[0]
        except OverflowError:
            return 0xFC00 if value < 0.0 else 0x7C00
    return encode(fmt, decode(FP64, _UNPACK_Q(_PACK_D(value))[0]))[0]


def _round_to_odd_sum(product: float, addend: float) -> float:
    """Return the round-to-odd binary64 value of exact ``product + addend``."""

    total = product + addend
    if total - total != 0.0:  # infinite or NaN
        return total
    partial = total - addend
    error = (product - partial) + (addend - (total - partial))
    if error != 0.0 and not (double_bits(total) & 1):
        total = math.nextafter(total, math.inf if error > 0.0 else -math.inf)
    return total


# ---------------------------------------------------------------------------
# Lane layer: round-to-nearest-even, no flags
# ---------------------------------------------------------------------------

def lane_add(fmt: Format, a: int, b: int) -> int:
    return from_double(fmt, to_double(fmt, a) + to_double(fmt, b))


def lane_sub(fmt: Format, a: int, b: int) -> int:
    return from_double(fmt, to_double(fmt, a) - to_double(fmt, b))


def lane_mul(fmt: Format, a: int, b: int) -> int:
    return from_double(fmt, to_double(fmt, a) * to_double(fmt, b))


def lane_fma(fmt: Format, a: int, b: int, c: int) -> int:
    """Return ``RN(a * b + c)`` with one rounding (TFMA, TMAC)."""

    return lane_mixed_fma(fmt, fmt, a, b, c)


def lane_mixed_fma(dst: Format, src: Format, a: int, b: int, c: int) -> int:
    """Return ``RN_dst(a * b + c)``, ``a, b`` in ``src``, ``c`` in ``dst``."""

    x = to_double(src, a)
    y = to_double(src, b)
    z = to_double(dst, c)
    if src is FP64:
        return _host_fma(dst, x, y, z)
    product_value = x * y  # exact for precision <= 26
    if dst is FP64:
        return from_double(dst, product_value + z)
    return from_double(dst, _round_to_odd_sum(product_value, z))


def _host_fma(dst: Format, x: float, y: float, z: float) -> int:
    if x != x or y != y or z != z:
        return dst.canonical_nan
    try:
        return from_double(dst, math.fma(x, y, z))
    except ValueError:
        return dst.canonical_nan
    except OverflowError:
        return from_double(dst, math.copysign(math.inf, x * y * 0.5 + z * 0.5))


def lane_product(dst: Format, src: Format, a: int, b: int) -> int:
    """Return ``RN_dst(a * b)`` for ``a, b`` in ``src``."""

    return from_double(dst, to_double(src, a) * to_double(src, b))


def lane_convert(dst: Format, src: Format, a: int) -> int:
    """Convert with one RNE rounding (NaN becomes canonical)."""

    return from_double(dst, to_double(src, a))


def reduce_tree(fmt: Format, leaves: Sequence[int]) -> int:
    """Sum ``leaves`` (raw ``fmt`` bits) with the canonical pairwise tree."""

    values = [to_double(fmt, leaf) for leaf in leaves]
    count = len(values)
    if count & (count - 1) or count == 0:
        raise ValueError("tree width must be a power of two")
    if fmt is FP64:
        while count > 1:
            values = [values[2 * j] + values[2 * j + 1]
                      for j in range(count // 2)]
            count //= 2
        return from_double(fmt, values[0])
    rounded = _rounded_double(fmt)
    while count > 1:
        values = [rounded(values[2 * j] + values[2 * j + 1])
                  for j in range(count // 2)]
        count //= 2
    return from_double(fmt, values[0])


def _rounded_double(fmt: Format):
    def round_double(value: float) -> float:
        return to_double(fmt, from_double(fmt, value))
    return round_double


def exact_reduce_tree(fmt: Format, leaves: Sequence[int]) -> int:
    """Exact-layer canonical tree, used to check ``reduce_tree``."""

    values = list(leaves)
    while len(values) > 1:
        values = [add(fmt, values[2 * j], values[2 * j + 1])[0]
                  for j in range(len(values) // 2)]
    return values[0]
