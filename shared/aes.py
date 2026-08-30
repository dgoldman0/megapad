"""Backend-neutral AES block and GCM value operations.

The hosted simulator needs the same byte-level AES/GCM results as MegaPad's
architectural accelerator, without importing either backend.  This module is a
small deterministic value model: it has no MMIO, ownership, guest memory,
latency, or transaction state.  It is deliberately not a constant-time host
cryptography API and must not be used to protect host secrets.
"""

from __future__ import annotations


AES_BLOCK_BYTES = 16
AES_128_KEY_BYTES = 16
AES_256_KEY_BYTES = 32
_MASK128 = (1 << 128) - 1
_GHASH_REDUCTION = 0xE1000000000000000000000000000000

_SBOX = bytes.fromhex(
    "637c777bf26b6fc53001672bfed7ab76"
    "ca82c97dfa5947f0add4a2af9ca472c0"
    "b7fd9326363ff7cc34a5e5f171d83115"
    "04c723c31896059a071280e2eb27b275"
    "09832c1a1b6e5aa0523bd6b329e32f84"
    "53d100ed20fcb15b6acbbe394a4c58cf"
    "d0efaafb434d338545f9027f503c9fa8"
    "51a3408f929d38f5bcb6da2110fff3d2"
    "cd0c13ec5f974417c4a77e3d645d1973"
    "60814fdc222a908846eeb814de5e0bdb"
    "e0323a0a4906245cc2d3ac629195e479"
    "e7c8376d8dd54ea96c56f4ea657aae08"
    "ba78252e1ca6b4c6e8dd741f4bbd8b8a"
    "703eb5664803f60e613557b986c11d9e"
    "e1f8981169d98e949b1e87e9ce5528df"
    "8ca1890dbfe6426841992d0fb054bb16"
)
_RCON = bytes((0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36))


def _exact_bytes(value: bytes, length: int, *, label: str) -> bytes:
    if not isinstance(value, bytes):
        raise TypeError(f"{label} must be bytes")
    if len(value) != length:
        raise ValueError(f"{label} must contain exactly {length} bytes")
    return value


def _gm2(value: int) -> int:
    return ((value << 1) ^ (0x1B if value & 0x80 else 0)) & 0xFF


def _gm3(value: int) -> int:
    return _gm2(value) ^ value


def _expand_key(key: bytes) -> tuple[bytes, ...]:
    if not isinstance(key, bytes):
        raise TypeError("AES key must be bytes")
    if len(key) not in (AES_128_KEY_BYTES, AES_256_KEY_BYTES):
        raise ValueError("AES key must contain exactly 16 or 32 bytes")

    key_bytes = len(key)
    rounds = 10 if key_bytes == AES_128_KEY_BYTES else 14
    expanded = bytearray(AES_BLOCK_BYTES * (rounds + 1))
    expanded[:key_bytes] = key
    generated = key_bytes
    rcon_index = 0

    while generated < len(expanded):
        temporary = bytearray(expanded[generated - 4 : generated])
        if generated % key_bytes == 0:
            temporary[:] = (
                _SBOX[temporary[1]] ^ _RCON[rcon_index],
                _SBOX[temporary[2]],
                _SBOX[temporary[3]],
                _SBOX[temporary[0]],
            )
            rcon_index += 1
        elif key_bytes == AES_256_KEY_BYTES and generated % key_bytes == 16:
            temporary[:] = (_SBOX[value] for value in temporary)

        for value in temporary:
            expanded[generated] = expanded[generated - key_bytes] ^ value
            generated += 1

    return tuple(
        bytes(expanded[offset : offset + AES_BLOCK_BYTES])
        for offset in range(0, len(expanded), AES_BLOCK_BYTES)
    )


class AESBlockCipher:
    """One immutable AES-128 or AES-256 encryption key schedule."""

    __slots__ = ("_round_keys",)

    def __init__(self, key: bytes) -> None:
        self._round_keys = _expand_key(key)

    @property
    def rounds(self) -> int:
        return len(self._round_keys) - 1

    def encrypt(self, block: bytes) -> bytes:
        """Encrypt one exact 16-byte block."""

        block = _exact_bytes(block, AES_BLOCK_BYTES, label="AES block")
        state = [
            value ^ round_key
            for value, round_key in zip(block, self._round_keys[0])
        ]

        for round_key in self._round_keys[1:-1]:
            substituted = [_SBOX[value] for value in state]
            state = [
                substituted[0],
                substituted[5],
                substituted[10],
                substituted[15],
                substituted[4],
                substituted[9],
                substituted[14],
                substituted[3],
                substituted[8],
                substituted[13],
                substituted[2],
                substituted[7],
                substituted[12],
                substituted[1],
                substituted[6],
                substituted[11],
            ]
            for column in range(4):
                offset = column * 4
                first, second, third, fourth = state[offset : offset + 4]
                state[offset : offset + 4] = (
                    _gm2(first) ^ _gm3(second) ^ third ^ fourth,
                    first ^ _gm2(second) ^ _gm3(third) ^ fourth,
                    first ^ second ^ _gm2(third) ^ _gm3(fourth),
                    _gm3(first) ^ second ^ third ^ _gm2(fourth),
                )
            state = [
                value ^ key_byte
                for value, key_byte in zip(state, round_key)
            ]

        substituted = [_SBOX[value] for value in state]
        shifted = (
            substituted[0],
            substituted[5],
            substituted[10],
            substituted[15],
            substituted[4],
            substituted[9],
            substituted[14],
            substituted[3],
            substituted[8],
            substituted[13],
            substituted[2],
            substituted[7],
            substituted[12],
            substituted[1],
            substituted[6],
            substituted[11],
        )
        return bytes(
            value ^ key_byte
            for value, key_byte in zip(shifted, self._round_keys[-1])
        )


def ghash_multiply(left: int, right: int) -> int:
    """Multiply two unsigned 128-bit GHASH field elements."""

    for label, value in (("left", left), ("right", right)):
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"GHASH {label} operand must be a uint128 integer")
        if not 0 <= value <= _MASK128:
            raise ValueError(f"GHASH {label} operand must be a uint128 integer")

    product = 0
    multiplicand = right
    for bit in range(127, -1, -1):
        if left & (1 << bit):
            product ^= multiplicand
        low_bit = multiplicand & 1
        multiplicand >>= 1
        if low_bit:
            multiplicand ^= _GHASH_REDUCTION
    return product


def ghash_update(accumulator: int, subkey: int, block: bytes) -> int:
    """Fold one exact 16-byte, big-endian block into a GHASH state."""

    block = _exact_bytes(block, AES_BLOCK_BYTES, label="GHASH block")
    return ghash_multiply(
        accumulator ^ int.from_bytes(block, "big"),
        subkey,
    )


def increment_gcm_counter(counter: bytes) -> bytes:
    """Increment only the low 32 bits of one GCM counter block."""

    counter = _exact_bytes(counter, AES_BLOCK_BYTES, label="GCM counter")
    low = (int.from_bytes(counter[12:], "big") + 1) & 0xFFFF_FFFF
    return counter[:12] + low.to_bytes(4, "big")


__all__ = [
    "AESBlockCipher",
    "AES_128_KEY_BYTES",
    "AES_256_KEY_BYTES",
    "AES_BLOCK_BYTES",
    "ghash_multiply",
    "ghash_update",
    "increment_gcm_counter",
]
