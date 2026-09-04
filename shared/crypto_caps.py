"""Backend-neutral System Info crypto-capability bit assignments."""

from __future__ import annotations

from typing import Final


CRYPTO_CAP_CRC_REFLECT_RAW: Final = 1 << 0
CRYPTO_CAP_SHA3_STREAM: Final = 1 << 1
CRYPTO_CAP_KECCAK_F1600: Final = 1 << 2
CRYPTO_CAP_WOTS_CHAIN: Final = 1 << 3

CRYPTO_CAP_KNOWN_MASK: Final = (
    CRYPTO_CAP_CRC_REFLECT_RAW
    | CRYPTO_CAP_SHA3_STREAM
    | CRYPTO_CAP_KECCAK_F1600
    | CRYPTO_CAP_WOTS_CHAIN
)


__all__ = [
    "CRYPTO_CAP_CRC_REFLECT_RAW",
    "CRYPTO_CAP_KECCAK_F1600",
    "CRYPTO_CAP_KNOWN_MASK",
    "CRYPTO_CAP_SHA3_STREAM",
    "CRYPTO_CAP_WOTS_CHAIN",
]
