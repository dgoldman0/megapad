"""Backend-neutral RFC 7748 X25519 value operation.

This module owns only the pure mapping from one 32-byte scalar and one
32-byte u-coordinate to the 32-byte little-endian result.  It has no guest
memory, ACC registers, instruction timing, ownership, status, entropy, or
host-secret API.  The Python implementation is deliberately not constant
time and must not be used to protect host secrets.
"""

from __future__ import annotations


X25519_BYTES = 32
CURVE25519_PRIME = (1 << 255) - 19
_A24 = 121665


def _exact_input(value: bytes, *, label: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{label} must be bytes")
    if len(value) != X25519_BYTES:
        raise ValueError(f"{label} must contain exactly 32 bytes")
    return value


def x25519_scalar_multiply(scalar: bytes, u_coordinate: bytes) -> bytes:
    """Return the RFC 7748 X25519 result for two exact byte strings."""

    scalar = _exact_input(scalar, label="X25519 scalar")
    u_coordinate = _exact_input(
        u_coordinate,
        label="X25519 u-coordinate",
    )

    clamped = bytearray(scalar)
    clamped[0] &= 248
    clamped[31] &= 127
    clamped[31] |= 64
    scalar_value = int.from_bytes(clamped, "little")

    coordinate = int.from_bytes(u_coordinate, "little")
    coordinate &= (1 << 255) - 1

    x_1 = coordinate
    x_2, z_2 = 1, 0
    x_3, z_3 = coordinate, 1
    swap = 0

    for bit_index in range(254, -1, -1):
        scalar_bit = (scalar_value >> bit_index) & 1
        swap ^= scalar_bit
        if swap:
            x_2, x_3 = x_3, x_2
            z_2, z_3 = z_3, z_2
        swap = scalar_bit

        add = (x_2 + z_2) % CURVE25519_PRIME
        add_squared = add * add % CURVE25519_PRIME
        subtract = (x_2 - z_2) % CURVE25519_PRIME
        subtract_squared = subtract * subtract % CURVE25519_PRIME
        difference = (add_squared - subtract_squared) % CURVE25519_PRIME
        other_add = (x_3 + z_3) % CURVE25519_PRIME
        other_subtract = (x_3 - z_3) % CURVE25519_PRIME
        product_one = other_subtract * add % CURVE25519_PRIME
        product_two = other_add * subtract % CURVE25519_PRIME

        x_3 = (product_one + product_two) ** 2 % CURVE25519_PRIME
        z_3 = (
            x_1
            * (product_one - product_two) ** 2
            % CURVE25519_PRIME
        )
        x_2 = add_squared * subtract_squared % CURVE25519_PRIME
        z_2 = (
            difference
            * (add_squared + _A24 * difference)
            % CURVE25519_PRIME
        )

    if swap:
        x_2, x_3 = x_3, x_2
        z_2, z_3 = z_3, z_2

    inverse = pow(z_2, CURVE25519_PRIME - 2, CURVE25519_PRIME)
    result = x_2 * inverse % CURVE25519_PRIME
    return result.to_bytes(X25519_BYTES, "little")


__all__ = [
    "CURVE25519_PRIME",
    "X25519_BYTES",
    "x25519_scalar_multiply",
]
