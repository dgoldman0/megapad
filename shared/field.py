"""Backend-neutral 256-bit Field-ALU value operations.

These helpers preserve MegaPad's integer results without owning ACC registers,
CSRs, guest memory, publication order, latency, or flags.  They are ordinary
Python arithmetic and make no constant-time or host-secret claim.
"""

from __future__ import annotations


FIELD_BITS = 256
FIELD_BYTES = FIELD_BITS // 8
FIELD_MASK = (1 << FIELD_BITS) - 1
FIELD_WIDE_MASK = (1 << (FIELD_BITS * 2)) - 1
MONTGOMERY_RADIX = 1 << FIELD_BITS

PRIME_25519 = (1 << 255) - 19
PRIME_SECP256K1 = (
    0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
)
PRIME_P256 = (
    0xFFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF
)
BUILTIN_PRIMES = (PRIME_25519, PRIME_SECP256K1, PRIME_P256)


def _uint(value: int, *, bits: int, label: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise TypeError(f"{label} must be an unsigned integer")
    if not 0 <= value < 1 << bits:
        raise ValueError(f"{label} must fit {bits} unsigned bits")
    return value


def active_prime(selection: int, custom_prime: int) -> int:
    """Resolve the two-bit prime selector, including the native zero fallback."""

    selection = _uint(selection, bits=2, label="Field prime selection")
    custom_prime = _uint(
        custom_prime,
        bits=FIELD_BITS,
        label="Field custom prime",
    )
    if selection < len(BUILTIN_PRIMES):
        return BUILTIN_PRIMES[selection]
    return custom_prime if custom_prime != 0 else PRIME_25519


def field_add(first: int, second: int, prime: int) -> int:
    """Return the executable ALU's one-subtraction sum."""

    first = _uint(first, bits=FIELD_BITS, label="Field addend")
    second = _uint(second, bits=FIELD_BITS, label="Field addend")
    prime = _prime(prime)
    result = first + second
    if result >= prime:
        result -= prime
    return result & FIELD_MASK


def field_subtract(first: int, second: int, prime: int) -> int:
    """Return the executable ALU's subtract-or-compensate result."""

    first = _uint(first, bits=FIELD_BITS, label="Field minuend")
    second = _uint(second, bits=FIELD_BITS, label="Field subtrahend")
    prime = _prime(prime)
    if first >= second:
        return (first - second) % prime
    return (prime - (second - first)) & FIELD_MASK


def field_multiply(first: int, second: int, prime: int) -> int:
    """Return a fully reduced 256-by-256 modular product."""

    first = _uint(first, bits=FIELD_BITS, label="Field factor")
    second = _uint(second, bits=FIELD_BITS, label="Field factor")
    return first * second % _prime(prime)


def field_square(value: int, prime: int) -> int:
    """Return a fully reduced modular square."""

    value = _uint(value, bits=FIELD_BITS, label="Field square input")
    return value * value % _prime(prime)


def field_inverse(value: int, prime: int) -> int:
    """Return the executable Fermat exponent ``value**(prime-2) mod prime``."""

    value = _uint(value, bits=FIELD_BITS, label="Field inverse input")
    prime = _prime(prime)
    exponent = (prime - 2) & FIELD_WIDE_MASK
    return pow(value % prime, exponent, prime)


def field_power(base: int, exponent: int, prime: int) -> int:
    """Return ``base**exponent mod prime`` for exact 256-bit operands."""

    base = _uint(base, bits=FIELD_BITS, label="Field power base")
    exponent = _uint(exponent, bits=FIELD_BITS, label="Field power exponent")
    prime = _prime(prime)
    return pow(base % prime, exponent, prime)


def montgomery_multiply(
    first: int,
    second: int,
    prime: int,
    negative_inverse: int,
) -> int:
    """Apply the native 256-bit REDC path with one final subtraction."""

    first = _uint(first, bits=FIELD_BITS, label="Montgomery factor")
    second = _uint(second, bits=FIELD_BITS, label="Montgomery factor")
    prime = _prime(prime)
    negative_inverse = _uint(
        negative_inverse,
        bits=FIELD_BITS,
        label="Montgomery negative inverse",
    )
    product = first * second
    multiplier = ((product & FIELD_MASK) * negative_inverse) & FIELD_MASK
    reduced = (product + multiplier * prime) >> FIELD_BITS
    if reduced >= prime:
        reduced -= prime
    return reduced & FIELD_MASK


def raw_product(first: int, second: int) -> tuple[int, int]:
    """Return the low and high halves of one 256-by-256 product."""

    first = _uint(first, bits=FIELD_BITS, label="raw Field factor")
    second = _uint(second, bits=FIELD_BITS, label="raw Field factor")
    product = first * second
    return product & FIELD_MASK, (product >> FIELD_BITS) & FIELD_MASK


def raw_multiply_add(
    first: int,
    second: int,
    previous_low: int,
    previous_high: int,
) -> tuple[int, int]:
    """Return the wrapped 512-bit sum of a product and previous result."""

    previous_low = _uint(
        previous_low,
        bits=FIELD_BITS,
        label="raw previous low",
    )
    previous_high = _uint(
        previous_high,
        bits=FIELD_BITS,
        label="raw previous high",
    )
    product_low, product_high = raw_product(first, second)
    total = (
        previous_low
        | (previous_high << FIELD_BITS)
    ) + product_low + (product_high << FIELD_BITS)
    total &= FIELD_WIDE_MASK
    return total & FIELD_MASK, total >> FIELD_BITS


def _prime(value: int) -> int:
    value = _uint(value, bits=FIELD_BITS, label="Field prime")
    if value == 0:
        raise ValueError("Field prime must be nonzero")
    return value


__all__ = [
    "BUILTIN_PRIMES",
    "FIELD_BITS",
    "FIELD_BYTES",
    "FIELD_MASK",
    "FIELD_WIDE_MASK",
    "MONTGOMERY_RADIX",
    "PRIME_25519",
    "PRIME_P256",
    "PRIME_SECP256K1",
    "active_prime",
    "field_add",
    "field_inverse",
    "field_multiply",
    "field_power",
    "field_square",
    "field_subtract",
    "montgomery_multiply",
    "raw_multiply_add",
    "raw_product",
]
