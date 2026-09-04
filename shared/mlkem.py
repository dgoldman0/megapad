"""Backend-neutral ML-KEM-512 value operations.

This module owns deterministic key generation, encapsulation, and
decapsulation bytes for MegaPad's architectural KEM device.  It owns no MMIO
registers, retained buffers, guest-memory transfers, status, timing, or
requester state.  The Python implementation is not constant time and must not
be used to protect host secrets.
"""

from __future__ import annotations

import hashlib


MLKEM512_KEYGEN_SEED_BYTES = 64
MLKEM512_ENCAPSULATION_RANDOM_BYTES = 32
MLKEM512_ENCAPSULATION_KEY_BYTES = 800
MLKEM512_DECAPSULATION_KEY_BYTES = 1632
MLKEM512_CIPHERTEXT_BYTES = 768
MLKEM512_SHARED_SECRET_BYTES = 32

_N = 256
_K = 2
_Q = 3329
_ETA1 = 3
_ETA2 = 2
_DU = 10
_DV = 4
_SEED_COMPONENT_BYTES = MLKEM512_KEYGEN_SEED_BYTES // 2


def _exact_bytes(value: bytes, length: int, *, label: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{label} must be bytes")
    if len(value) != length:
        raise ValueError(f"{label} must contain exactly {length} bytes")
    return value


def _bit_reverse7(value: int) -> int:
    result = 0
    for _ in range(7):
        result = (result << 1) | (value & 1)
        value >>= 1
    return result


_ZETAS = tuple(pow(17, _bit_reverse7(index), _Q) for index in range(128))


def _ntt(polynomial: list[int]) -> list[int]:
    transformed = list(polynomial)
    zeta_index = 1
    width = 128
    while width >= 2:
        for start in range(0, _N, 2 * width):
            zeta = _ZETAS[zeta_index]
            zeta_index += 1
            for index in range(start, start + width):
                product = zeta * transformed[index + width] % _Q
                transformed[index + width] = (
                    transformed[index] - product
                ) % _Q
                transformed[index] = (transformed[index] + product) % _Q
        width //= 2
    return transformed


def _inverse_ntt(transformed: list[int]) -> list[int]:
    polynomial = list(transformed)
    zeta_index = 127
    width = 2
    while width <= 128:
        for start in range(0, _N, 2 * width):
            zeta = _ZETAS[zeta_index]
            zeta_index -= 1
            for index in range(start, start + width):
                value = polynomial[index]
                polynomial[index] = (
                    value + polynomial[index + width]
                ) % _Q
                polynomial[index + width] = (
                    zeta * (polynomial[index + width] - value)
                ) % _Q
        width *= 2
    return [value * 3303 % _Q for value in polynomial]


def _base_multiply(first: list[int], second: list[int]) -> list[int]:
    result = [0] * _N
    for group in range(64):
        zeta = _ZETAS[64 + group]
        negative_zeta = -zeta % _Q

        index = 4 * group
        first_low, first_high = first[index], first[index + 1]
        second_low, second_high = second[index], second[index + 1]
        result[index] = (
            first_low * second_low
            + first_high * second_high * zeta
        ) % _Q
        result[index + 1] = (
            first_low * second_high + first_high * second_low
        ) % _Q

        index += 2
        first_low, first_high = first[index], first[index + 1]
        second_low, second_high = second[index], second[index + 1]
        result[index] = (
            first_low * second_low
            + first_high * second_high * negative_zeta
        ) % _Q
        result[index + 1] = (
            first_low * second_high + first_high * second_low
        ) % _Q
    return result


def _add(first: list[int], second: list[int]) -> list[int]:
    return [(first[index] + second[index]) % _Q for index in range(_N)]


def _subtract(first: list[int], second: list[int]) -> list[int]:
    return [(first[index] - second[index]) % _Q for index in range(_N)]


def _centered_binomial(eta: int, payload: bytes) -> list[int]:
    bits = [
        (byte >> bit_index) & 1
        for byte in payload
        for bit_index in range(8)
    ]
    result = [0] * _N
    for index in range(_N):
        offset = 2 * index * eta
        first = sum(bits[offset + bit] for bit in range(eta))
        second = sum(bits[offset + eta + bit] for bit in range(eta))
        result[index] = (first - second) % _Q
    return result


def _byte_encode(polynomial: list[int], width: int) -> bytes:
    result = bytearray(32 * width)
    bit_offset = 0
    for coefficient in polynomial:
        for bit_index in range(width):
            result[bit_offset >> 3] |= (
                (coefficient >> bit_index) & 1
            ) << (bit_offset & 7)
            bit_offset += 1
    return bytes(result)


def _byte_decode(payload: bytes, width: int) -> list[int]:
    bits = [
        (byte >> bit_index) & 1
        for byte in payload
        for bit_index in range(8)
    ]
    result = [0] * _N
    for index in range(_N):
        value = 0
        for bit_index in range(width):
            value |= bits[index * width + bit_index] << bit_index
        result[index] = value % ((1 << width) if width < 12 else _Q)
    return result


def _compress(value: int, width: int) -> int:
    return (
        (value * (1 << width) + _Q // 2) // _Q
    ) % (1 << width)


def _decompress(value: int, width: int) -> int:
    return (value * _Q + (1 << (width - 1))) >> width


def _compress_polynomial(polynomial: list[int], width: int) -> list[int]:
    return [_compress(value, width) for value in polynomial]


def _decompress_polynomial(polynomial: list[int], width: int) -> list[int]:
    return [_decompress(value, width) for value in polynomial]


def _g(payload: bytes) -> bytes:
    return hashlib.sha3_512(payload).digest()


def _h(payload: bytes) -> bytes:
    return hashlib.sha3_256(payload).digest()


def _j(payload: bytes) -> bytes:
    return hashlib.shake_256(payload).digest(MLKEM512_SHARED_SECRET_BYTES)


def _xof(seed: bytes, first: int, second: int) -> bytes:
    return hashlib.shake_128(seed + bytes((first, second))).digest(840)


def _prf(eta: int, seed: bytes, nonce: int) -> bytes:
    return hashlib.shake_256(seed + bytes((nonce,))).digest(64 * eta)


def _sample_ntt(payload: bytes) -> list[int]:
    result: list[int] = []
    offset = 0
    while len(result) < _N:
        first = payload[offset] + 256 * (payload[offset + 1] % 16)
        second = (payload[offset + 1] >> 4) + 16 * payload[offset + 2]
        if first < _Q:
            result.append(first)
        if second < _Q and len(result) < _N:
            result.append(second)
        offset += 3
    return result


def _pke_keygen(seed: bytes) -> tuple[bytes, bytes]:
    expanded = _g(seed + bytes((_K,)))
    rho, sigma = expanded[:32], expanded[32:]
    matrix = [
        [_sample_ntt(_xof(rho, column, row)) for column in range(_K)]
        for row in range(_K)
    ]

    nonce = 0
    secret = []
    for _ in range(_K):
        secret.append(_centered_binomial(_ETA1, _prf(_ETA1, sigma, nonce)))
        nonce += 1
    error = []
    for _ in range(_K):
        error.append(_centered_binomial(_ETA1, _prf(_ETA1, sigma, nonce)))
        nonce += 1

    secret_ntt = [_ntt(polynomial) for polynomial in secret]
    error_ntt = [_ntt(polynomial) for polynomial in error]
    public_vector = []
    for row in range(_K):
        polynomial = [0] * _N
        for column in range(_K):
            polynomial = _add(
                polynomial,
                _base_multiply(matrix[row][column], secret_ntt[column]),
            )
        public_vector.append(_add(polynomial, error_ntt[row]))

    encapsulation_key = b"".join(
        _byte_encode(polynomial, 12) for polynomial in public_vector
    ) + rho
    pke_decapsulation_key = b"".join(
        _byte_encode(polynomial, 12) for polynomial in secret_ntt
    )
    return encapsulation_key, pke_decapsulation_key


def _pke_encrypt(
    encapsulation_key: bytes,
    message: bytes,
    randomness: bytes,
) -> bytes:
    public_vector = [
        _byte_decode(encapsulation_key[384 * index : 384 * (index + 1)], 12)
        for index in range(_K)
    ]
    rho = encapsulation_key[384 * _K :]
    matrix = [
        [_sample_ntt(_xof(rho, column, row)) for column in range(_K)]
        for row in range(_K)
    ]

    nonce = 0
    secret = []
    for _ in range(_K):
        secret.append(
            _centered_binomial(_ETA1, _prf(_ETA1, randomness, nonce))
        )
        nonce += 1
    error_one = []
    for _ in range(_K):
        error_one.append(
            _centered_binomial(_ETA2, _prf(_ETA2, randomness, nonce))
        )
        nonce += 1
    error_two = _centered_binomial(
        _ETA2,
        _prf(_ETA2, randomness, nonce),
    )

    secret_ntt = [_ntt(polynomial) for polynomial in secret]
    first_ciphertext = []
    for row in range(_K):
        polynomial = [0] * _N
        for column in range(_K):
            polynomial = _add(
                polynomial,
                _base_multiply(matrix[column][row], secret_ntt[column]),
            )
        first_ciphertext.append(
            _add(_inverse_ntt(polynomial), error_one[row])
        )

    message_polynomial = _decompress_polynomial(_byte_decode(message, 1), 1)
    second_ciphertext = [0] * _N
    for index in range(_K):
        second_ciphertext = _add(
            second_ciphertext,
            _base_multiply(public_vector[index], secret_ntt[index]),
        )
    second_ciphertext = _add(
        _add(_inverse_ntt(second_ciphertext), error_two),
        message_polynomial,
    )

    encoded_first = b"".join(
        _byte_encode(_compress_polynomial(polynomial, _DU), _DU)
        for polynomial in first_ciphertext
    )
    encoded_second = _byte_encode(
        _compress_polynomial(second_ciphertext, _DV),
        _DV,
    )
    return encoded_first + encoded_second


def _pke_decrypt(decapsulation_key: bytes, ciphertext: bytes) -> bytes:
    chunk_bytes = 32 * _DU
    first_ciphertext = ciphertext[: chunk_bytes * _K]
    second_ciphertext = ciphertext[chunk_bytes * _K :]
    first_vector = [
        _decompress_polynomial(
            _byte_decode(
                first_ciphertext[
                    chunk_bytes * index : chunk_bytes * (index + 1)
                ],
                _DU,
            ),
            _DU,
        )
        for index in range(_K)
    ]
    second_polynomial = _decompress_polynomial(
        _byte_decode(second_ciphertext, _DV),
        _DV,
    )
    secret_vector = [
        _byte_decode(decapsulation_key[384 * index : 384 * (index + 1)], 12)
        for index in range(_K)
    ]
    first_ntt = [_ntt(polynomial) for polynomial in first_vector]
    product = [0] * _N
    for index in range(_K):
        product = _add(
            product,
            _base_multiply(secret_vector[index], first_ntt[index]),
        )
    message = _subtract(second_polynomial, _inverse_ntt(product))
    return _byte_encode(_compress_polynomial(message, 1), 1)


def mlkem512_keygen(seed: bytes) -> tuple[bytes, bytes]:
    """Return deterministic ``(encapsulation_key, decapsulation_key)`` bytes."""

    seed = _exact_bytes(
        seed,
        MLKEM512_KEYGEN_SEED_BYTES,
        label="ML-KEM-512 key-generation seed",
    )
    first = seed[:_SEED_COMPONENT_BYTES]
    fallback = seed[_SEED_COMPONENT_BYTES:]
    encapsulation_key, pke_decapsulation_key = _pke_keygen(first)
    decapsulation_key = (
        pke_decapsulation_key
        + encapsulation_key
        + _h(encapsulation_key)
        + fallback
    )
    return encapsulation_key, decapsulation_key


def mlkem512_encapsulate(
    encapsulation_key: bytes,
    randomness: bytes,
) -> tuple[bytes, bytes]:
    """Return deterministic ``(ciphertext, shared_secret)`` bytes."""

    encapsulation_key = _exact_bytes(
        encapsulation_key,
        MLKEM512_ENCAPSULATION_KEY_BYTES,
        label="ML-KEM-512 encapsulation key",
    )
    randomness = _exact_bytes(
        randomness,
        MLKEM512_ENCAPSULATION_RANDOM_BYTES,
        label="ML-KEM-512 encapsulation randomness",
    )
    expanded = _g(randomness + _h(encapsulation_key))
    shared_secret, encryption_randomness = expanded[:32], expanded[32:]
    ciphertext = _pke_encrypt(
        encapsulation_key,
        randomness,
        encryption_randomness,
    )
    return ciphertext, shared_secret


def mlkem512_decapsulate(
    ciphertext: bytes,
    decapsulation_key: bytes,
) -> bytes:
    """Return the recovered or implicit-rejection shared secret."""

    ciphertext = _exact_bytes(
        ciphertext,
        MLKEM512_CIPHERTEXT_BYTES,
        label="ML-KEM-512 ciphertext",
    )
    decapsulation_key = _exact_bytes(
        decapsulation_key,
        MLKEM512_DECAPSULATION_KEY_BYTES,
        label="ML-KEM-512 decapsulation key",
    )

    pke_key_bytes = 384 * _K
    encapsulation_key_bytes = MLKEM512_ENCAPSULATION_KEY_BYTES
    pke_key = decapsulation_key[:pke_key_bytes]
    encapsulation_key = decapsulation_key[
        pke_key_bytes : pke_key_bytes + encapsulation_key_bytes
    ]
    key_hash = decapsulation_key[
        pke_key_bytes + encapsulation_key_bytes :
        pke_key_bytes + encapsulation_key_bytes + 32
    ]
    fallback = decapsulation_key[
        pke_key_bytes + encapsulation_key_bytes + 32 :
    ]

    message = _pke_decrypt(pke_key, ciphertext)
    expanded = _g(message + key_hash)
    candidate, encryption_randomness = expanded[:32], expanded[32:]
    rejection = _j(fallback + ciphertext)
    expected = _pke_encrypt(
        encapsulation_key,
        message,
        encryption_randomness,
    )
    return candidate if ciphertext == expected else rejection


__all__ = [
    "MLKEM512_CIPHERTEXT_BYTES",
    "MLKEM512_DECAPSULATION_KEY_BYTES",
    "MLKEM512_ENCAPSULATION_KEY_BYTES",
    "MLKEM512_ENCAPSULATION_RANDOM_BYTES",
    "MLKEM512_KEYGEN_SEED_BYTES",
    "MLKEM512_SHARED_SECRET_BYTES",
    "mlkem512_decapsulate",
    "mlkem512_encapsulate",
    "mlkem512_keygen",
]
