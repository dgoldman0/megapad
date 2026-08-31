"""Backend-neutral values for MegaPad's 256-coefficient NTT engine.

The helpers implement the checked-in device's root selection and radix-2
integer transforms.  They own no MMIO registers, guest-memory transfers,
buffer selection, status lifecycle, timing, or allocation.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable


NTT_SIZE = 256
NTT_COEFFICIENT_BYTES = 4
NTT_POLYNOMIAL_BYTES = NTT_SIZE * NTT_COEFFICIENT_BYTES
NTT_DEFAULT_MODULUS = 3329
NTT_DILITHIUM_MODULUS = 8_380_417
_UINT64_LIMIT = 1 << 64


@dataclass(frozen=True, slots=True)
class NTTRoots:
    """One device-selected primitive root and its inverse scale values."""

    forward: int
    inverse: int
    size_inverse: int


def find_ntt_roots(modulus: int) -> NTTRoots | None:
    """Reproduce the device's bounded primitive-root search for one modulus."""

    modulus = _modulus(modulus)
    if modulus < 2 or (modulus - 1) % NTT_SIZE != 0:
        return None
    for generator in range(2, min(modulus, 10_000)):
        if pow(generator, (modulus - 1) // 2, modulus) == 1:
            continue
        forward = pow(generator, (modulus - 1) // NTT_SIZE, modulus)
        if pow(forward, NTT_SIZE // 2, modulus) == 1:
            continue
        return NTTRoots(
            forward=forward,
            inverse=pow(forward, modulus - 2, modulus),
            size_inverse=pow(NTT_SIZE, modulus - 2, modulus),
        )
    return None


def ntt_forward(
    coefficients: Iterable[int],
    modulus: int,
    *,
    roots: NTTRoots | None = None,
) -> tuple[int, ...]:
    """Return the device-order forward radix-2 transform."""

    modulus = _modulus(modulus)
    roots = _roots(modulus, roots)
    return _transform(
        coefficients,
        modulus,
        root=roots.forward,
        final_scale=None,
    )


def ntt_inverse(
    coefficients: Iterable[int],
    modulus: int,
    *,
    roots: NTTRoots | None = None,
) -> tuple[int, ...]:
    """Return the device-order inverse transform with final 256^-1 scaling."""

    modulus = _modulus(modulus)
    roots = _roots(modulus, roots)
    return _transform(
        coefficients,
        modulus,
        root=roots.inverse,
        final_scale=roots.size_inverse,
    )


def ntt_pointwise_multiply(
    first: Iterable[int],
    second: Iterable[int],
    modulus: int,
) -> tuple[int, ...]:
    """Return 256 independent coefficient products reduced by modulus."""

    modulus = _modulus(modulus)
    left = _coefficients(first)
    right = _coefficients(second)
    return tuple((a * b) % modulus for a, b in zip(left, right))


def ntt_pointwise_add(
    first: Iterable[int],
    second: Iterable[int],
    modulus: int,
) -> tuple[int, ...]:
    """Return 256 independent coefficient sums reduced by modulus."""

    modulus = _modulus(modulus)
    left = _coefficients(first)
    right = _coefficients(second)
    return tuple((a + b) % modulus for a, b in zip(left, right))


def _transform(
    coefficients: Iterable[int],
    modulus: int,
    *,
    root: int,
    final_scale: int | None,
) -> tuple[int, ...]:
    values = [value % modulus for value in _coefficients(coefficients)]
    _bit_reverse(values)
    width = 2
    while width <= NTT_SIZE:
        width_root = pow(root, NTT_SIZE // width, modulus)
        half = width // 2
        for start in range(0, NTT_SIZE, width):
            factor = 1
            for offset in range(half):
                even = values[start + offset]
                odd = values[start + offset + half] * factor % modulus
                values[start + offset] = (even + odd) % modulus
                values[start + offset + half] = (even - odd) % modulus
                factor = factor * width_root % modulus
        width <<= 1
    if final_scale is not None:
        values = [value * final_scale % modulus for value in values]
    return tuple(values)


def _bit_reverse(values: list[int]) -> None:
    reverse = 0
    for index in range(1, NTT_SIZE):
        bit = NTT_SIZE >> 1
        while reverse & bit:
            reverse ^= bit
            bit >>= 1
        reverse ^= bit
        if index < reverse:
            values[index], values[reverse] = values[reverse], values[index]


def _coefficients(values: Iterable[int]) -> tuple[int, ...]:
    if isinstance(values, (bytes, bytearray, memoryview, str)):
        raise TypeError("NTT coefficients must be an iterable of integers")
    try:
        result = tuple(values)
    except TypeError:
        raise TypeError("NTT coefficients must be an iterable of integers") from None
    if len(result) != NTT_SIZE:
        raise ValueError(f"NTT requires exactly {NTT_SIZE} coefficients")
    for value in result:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError("NTT coefficients must be integers")
        if value < 0:
            raise ValueError("NTT coefficients must be non-negative")
    return result


def _modulus(value: int) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise TypeError("NTT modulus must be an unsigned integer")
    if not 0 <= value < _UINT64_LIMIT:
        raise ValueError("NTT modulus must fit 64 unsigned bits")
    return value


def _roots(modulus: int, roots: NTTRoots | None) -> NTTRoots:
    if roots is None:
        roots = find_ntt_roots(modulus)
    if not isinstance(roots, NTTRoots):
        raise ValueError("NTT modulus has no device-selected 256th root")
    return roots


__all__ = [
    "NTT_COEFFICIENT_BYTES",
    "NTT_DEFAULT_MODULUS",
    "NTT_DILITHIUM_MODULUS",
    "NTT_POLYNOMIAL_BYTES",
    "NTT_SIZE",
    "NTTRoots",
    "find_ntt_roots",
    "ntt_forward",
    "ntt_inverse",
    "ntt_pointwise_add",
    "ntt_pointwise_multiply",
]
