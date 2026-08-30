"""Focused hosted/native AES-GCM public-state differential vectors.

This module intentionally lives outside ``tests/simulator``: the lightweight
hosted suite never requires a compiled MP64 backend.  Normal emulator
qualification rebuilds the extension before exercising this direct oracle.
"""

from __future__ import annotations

import _mp64_accel
import pytest

from simulator.aes import (
    AESAccessError,
    AES_AAD_LENGTH,
    AES_COMMAND,
    AES_DATA_INPUT,
    AES_DATA_LENGTH,
    AES_DATA_OUTPUT,
    AES_IV,
    AES_KEY,
    AES_KEY_MODE,
    AES_LIMIT,
    AES_OFFSET,
    AES_STATUS,
    AES_STATUS_ACTIVE,
    AES_STATUS_DONE,
    AES_STATUS_FAILED,
    AES_STATUS_IDLE,
    AES_TAG,
    HostedAESService,
)


KEY = bytes(range(32))
IV = bytes(range(12))
PLAINTEXT = b"A" * 16
CIPHERTEXT = bytes.fromhex("0643975a84a4835acc00d6caf0a8392c")
TAG = bytes.fromhex("0ff145f3786b8fc48a8aeafc45524d80")
AES128_CIPHERTEXT = bytes.fromhex("0388dace60b6a392f328c2b971b2fe78")
AES128_TAG = bytes.fromhex("ab6e47d42cec13bdf53a67b21257bddf")


def _new_pair():
    native = _mp64_accel.CPUState()
    native.init_crypto()
    return HostedAESService(), native


def _write(hosted, native, offset: int, payload: bytes) -> None:
    for index, value in enumerate(payload):
        hosted.write8(offset + index, value)
        native.crypto_write8(offset + index, value)


def _read_pair(hosted, native, offset: int, length: int) -> tuple[bytes, bytes]:
    return (
        bytes(hosted.read8(offset + index) for index in range(length)),
        bytes(native.crypto_read8(offset + index) for index in range(length)),
    )


def _configure(
    hosted,
    native,
    *,
    command: int,
    tag: bytes | None = None,
    key: bytes = KEY,
    iv: bytes = IV,
    aad_length: int = 0,
    data_length: int = 16,
    key_mode: int = 0,
) -> None:
    _write(hosted, native, AES_KEY_MODE, bytes((key_mode,)))
    _write(hosted, native, AES_KEY, key)
    _write(hosted, native, AES_IV, iv)
    _write(
        hosted,
        native,
        AES_AAD_LENGTH,
        aad_length.to_bytes(4, "little"),
    )
    _write(
        hosted,
        native,
        AES_DATA_LENGTH,
        data_length.to_bytes(4, "little"),
    )
    if tag is not None:
        _write(hosted, native, AES_TAG, tag)
    _write(hosted, native, AES_COMMAND, bytes((command,)))


def test_hosted_encrypt_matches_native_public_windows_and_known_answer() -> None:
    hosted, native = _new_pair()
    _configure(hosted, native, command=0)
    _write(hosted, native, AES_DATA_INPUT, PLAINTEXT)

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_DONE,)),
        bytes((AES_STATUS_DONE,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        CIPHERTEXT,
        CIPHERTEXT,
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (TAG, TAG)


def test_hosted_aes128_matches_native_public_windows_and_known_answer() -> None:
    hosted, native = _new_pair()
    _configure(
        hosted,
        native,
        command=0,
        key=bytes(32),
        iv=bytes(12),
        key_mode=1,
    )
    _write(hosted, native, AES_DATA_INPUT, bytes(16))

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_DONE,)),
        bytes((AES_STATUS_DONE,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        AES128_CIPHERTEXT,
        AES128_CIPHERTEXT,
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (
        AES128_TAG,
        AES128_TAG,
    )


def test_aes128_requires_all_32_key_bytes_in_both_transaction_models() -> None:
    hosted, native = _new_pair()
    _write(hosted, native, AES_KEY_MODE, b"\x01")
    _write(hosted, native, AES_KEY, bytes(16))
    _write(hosted, native, AES_IV, bytes(12))
    _write(hosted, native, AES_AAD_LENGTH, bytes(4))
    _write(hosted, native, AES_DATA_LENGTH, bytes(4))
    _write(hosted, native, AES_COMMAND, b"\x00")

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_FAILED,)),
        bytes((AES_STATUS_FAILED,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        bytes(16),
        bytes(16),
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (bytes(16), bytes(16))


def test_hosted_and_native_aes_preflight_admit_the_same_scalar_shapes() -> None:
    hosted, native = _new_pair()

    for width in (1, 2, 4, 8):
        for offset in (AES_OFFSET, AES_LIMIT - width):
            assert offset % width == 0
            for write in (False, True):
                assert native.crypto_preflight(offset, width, write) is True
                assert hosted.preflight(offset, width, write=write) is None

    invalid_shapes = (
        (AES_OFFSET + 1, 2),
        (AES_OFFSET + 2, 4),
        (AES_OFFSET + 4, 8),
        (AES_LIMIT - 1, 2),
        (AES_LIMIT - 2, 4),
        (AES_LIMIT - 4, 8),
        (AES_OFFSET - 1, 1),
        (AES_LIMIT, 1),
    )
    for offset, width in invalid_shapes:
        for write in (False, True):
            assert native.crypto_preflight(offset, width, write) is False
            with pytest.raises(AESAccessError):
                hosted.preflight(offset, width, write=write)


def test_hosted_decrypt_matches_native_good_and_bad_tag_effects() -> None:
    for expected_tag, expected_status, expected_output in (
        (TAG, AES_STATUS_DONE, PLAINTEXT),
        (bytes((TAG[0] ^ 0xFF,)) + TAG[1:], AES_STATUS_FAILED, bytes(16)),
    ):
        hosted, native = _new_pair()
        _configure(hosted, native, command=1, tag=expected_tag)
        _write(hosted, native, AES_DATA_INPUT, CIPHERTEXT)

        assert _read_pair(hosted, native, AES_STATUS, 1) == (
            bytes((expected_status,)),
            bytes((expected_status,)),
        )
        assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
            expected_output,
            expected_output,
        )
        assert _read_pair(hosted, native, AES_TAG, 16) == (bytes(16), bytes(16))


def test_hosted_incomplete_configuration_matches_native_fail_closed_state() -> None:
    hosted, native = _new_pair()
    _write(hosted, native, AES_KEY, KEY[:16])
    _write(hosted, native, AES_IV, IV)
    _write(hosted, native, AES_AAD_LENGTH, bytes(4))
    _write(hosted, native, AES_DATA_LENGTH, bytes(4))
    _write(hosted, native, AES_COMMAND, b"\x00")

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_FAILED,)),
        bytes((AES_STATUS_FAILED,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        bytes(16),
        bytes(16),
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (bytes(16), bytes(16))


def test_active_configuration_write_matches_native_fault_then_publish_order() -> None:
    hosted, native = _new_pair()
    _configure(hosted, native, command=0, data_length=32)
    _write(hosted, native, AES_DATA_INPUT, PLAINTEXT)

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_ACTIVE,)),
        bytes((AES_STATUS_ACTIVE,)),
    )
    first_output = _read_pair(hosted, native, AES_DATA_OUTPUT, 16)
    assert first_output[0] == first_output[1]
    assert first_output[0] != bytes(16)

    # The first configuration byte aborts and wipes the active transaction,
    # then becomes byte zero of the next configuration epoch.
    _write(hosted, native, AES_KEY, b"\xAA")
    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_FAILED,)),
        bytes((AES_STATUS_FAILED,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        bytes(16),
        bytes(16),
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (bytes(16), bytes(16))

    # The next configuration byte acknowledges the terminal fault before it
    # is retained.  Completing bytes 2..31 proves both earlier bytes survived.
    _write(hosted, native, AES_KEY + 1, b"\xBB")
    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_IDLE,)),
        bytes((AES_STATUS_IDLE,)),
    )
    _write(hosted, native, AES_KEY + 2, KEY[2:])
    _write(hosted, native, AES_IV, IV)
    _write(hosted, native, AES_AAD_LENGTH, bytes(4))
    _write(hosted, native, AES_DATA_LENGTH, (16).to_bytes(4, "little"))
    _write(hosted, native, AES_COMMAND, b"\x00")
    _write(hosted, native, AES_DATA_INPUT, PLAINTEXT)

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_DONE,)),
        bytes((AES_STATUS_DONE,)),
    )
    recovered_output = _read_pair(hosted, native, AES_DATA_OUTPUT, 16)
    recovered_tag = _read_pair(hosted, native, AES_TAG, 16)
    assert recovered_output[0] == recovered_output[1]
    assert recovered_tag[0] == recovered_tag[1]


def test_terminal_configuration_write_matches_native_clearing_order() -> None:
    hosted, native = _new_pair()
    _configure(hosted, native, command=0)
    _write(hosted, native, AES_DATA_INPUT, PLAINTEXT)

    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_DONE,)),
        bytes((AES_STATUS_DONE,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        CIPHERTEXT,
        CIPHERTEXT,
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (TAG, TAG)

    # A terminal-state configuration write acknowledges status and clears the
    # published data window before retaining the new byte.  Because this write
    # targets IV, the retained encrypt TAG remains readable.
    _write(hosted, native, AES_IV, b"\xCC")
    assert _read_pair(hosted, native, AES_STATUS, 1) == (
        bytes((AES_STATUS_IDLE,)),
        bytes((AES_STATUS_IDLE,)),
    )
    assert _read_pair(hosted, native, AES_DATA_OUTPUT, 16) == (
        bytes(16),
        bytes(16),
    )
    assert _read_pair(hosted, native, AES_TAG, 16) == (TAG, TAG)
