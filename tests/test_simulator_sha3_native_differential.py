"""Focused hosted/native SHA-3 public-state differential vectors.

This module intentionally lives outside ``tests/simulator``: the lightweight
hosted suite never requires a compiled MP64 backend.  Normal emulator
qualification rebuilds the extension before exercising this direct oracle.

The hosted service deliberately completes permutations synchronously.  These
tests advance the native device to the corresponding terminal boundary before
comparing it; native BUSY visibility and cycle counts are not simulator
compatibility claims.
"""

from __future__ import annotations

import hashlib

import _mp64_accel
import pytest

from shared.crypto_caps import (
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
)
from shared.keccak import keccak_f1600
from simulator.sha3 import (
    SHA3AccessError,
    SHA3_COMMAND,
    SHA3_CONTROL,
    SHA3_DATA_INPUT,
    SHA3_DATA_OUTPUT,
    SHA3_ERROR,
    SHA3_LIMIT,
    SHA3_OFFSET,
    SHA3_STATE_DATA,
    SHA3_STATE_INDEX,
    SHA3_STATUS,
    HostedSHA3Service,
)


FULL_CAPABILITIES = CRYPTO_CAP_SHA3_STREAM | CRYPTO_CAP_KECCAK_F1600
PERMUTATION_CYCLES = 24


def _new_pair(capabilities: int = FULL_CAPABILITIES):
    hosted = HostedSHA3Service(capabilities)
    native = _mp64_accel.CPUState()
    native.init_crypto()
    native._crypto_sha3_test_set_features(
        bool(capabilities & CRYPTO_CAP_SHA3_STREAM),
        bool(capabilities & CRYPTO_CAP_KECCAK_F1600),
    )
    return hosted, native


def _write8(hosted, native, offset: int, value: int) -> None:
    hosted.preflight(offset, 1, write=True)
    hosted.write8(offset, value)
    native.crypto_write8(offset, value)


def _read8_pair(hosted, native, offset: int) -> tuple[int, int]:
    hosted.preflight(offset, 1, write=False)
    return hosted.read8(offset), native.crypto_read8(offset)


def _write64(hosted, native, offset: int, value: int) -> None:
    hosted.preflight(offset, 8, write=True)
    for byte_index in range(8):
        hosted.write8(offset + byte_index, (value >> (8 * byte_index)) & 0xFF)
    native.crypto_write64(offset, value)


def _read64_pair(hosted, native, offset: int) -> tuple[int, int]:
    hosted.preflight(offset, 8, write=False)
    hosted_value = 0
    for byte_index in range(8):
        hosted_value |= hosted.read8(offset + byte_index) << (8 * byte_index)
    return hosted_value, native.crypto_read64(offset)


def _terminal_tuple(hosted, native) -> tuple[tuple[int, ...], tuple[int, ...]]:
    hosted_values = []
    native_values = []
    for offset in (SHA3_STATUS, SHA3_ERROR, SHA3_CONTROL):
        hosted_value, native_value = _read8_pair(hosted, native, offset)
        hosted_values.append(hosted_value)
        native_values.append(native_value)
    return tuple(hosted_values), tuple(native_values)


def _assert_terminal(hosted, native, expected: tuple[int, int, int]) -> None:
    hosted_values, native_values = _terminal_tuple(hosted, native)
    assert hosted_values == native_values == expected


def _settle(native) -> None:
    """Advance any one native SHA operation to its terminal boundary."""

    native.crypto_tick(PERMUTATION_CYCLES)


def _feed(hosted, native, message: bytes) -> None:
    # Advancing after every byte is intentionally insensitive to whether that
    # byte filled the selected rate.  It compares settled semantics without
    # observing or relying on the native model's automatic-absorb BUSY phase.
    for value in message:
        _write8(hosted, native, SHA3_DATA_INPUT, value)
        _settle(native)


def _read_window_pair(hosted, native) -> tuple[bytes, bytes]:
    hosted_bytes = bytearray()
    native_bytes = bytearray()
    for index in range(64):
        hosted_value, native_value = _read8_pair(
            hosted,
            native,
            SHA3_DATA_OUTPUT + index,
        )
        hosted_bytes.append(hosted_value)
        native_bytes.append(native_value)
    return bytes(hosted_bytes), bytes(native_bytes)


def _clear_pair(hosted, native, *, retained_mode: int) -> None:
    _write8(hosted, native, SHA3_COMMAND, 7)
    _settle(native)
    _assert_terminal(hosted, native, (0, 0, retained_mode))
    assert hosted.private_zeroized()
    assert native._crypto_sha3_test_zeroized()


def test_hosted_and_native_sha3_preflight_admit_exactly_the_same_shapes() -> None:
    hosted, native = _new_pair()

    # Include every start byte in and immediately around the aperture.  Width
    # three is also included to prove that only architectural scalar widths
    # enter either model.
    for offset in range(SHA3_OFFSET - 1, SHA3_LIMIT + 1):
        for width in (1, 2, 3, 4, 8):
            for write in (False, True):
                native_admits = native.crypto_preflight(offset, width, write)
                try:
                    hosted.preflight(offset, width, write=write)
                except SHA3AccessError:
                    hosted_admits = False
                else:
                    hosted_admits = True
                assert hosted_admits is native_admits, (
                    offset,
                    width,
                    write,
                )


@pytest.mark.parametrize(
    ("mode", "rate", "constructor", "output_length"),
    (
        (0, 136, hashlib.sha3_256, 32),
        (1, 72, hashlib.sha3_512, 64),
    ),
)
def test_fixed_sha3_rate_edges_match_native_terminal_windows(
    mode: int,
    rate: int,
    constructor,
    output_length: int,
) -> None:
    for length in (rate - 1, rate, rate + 1):
        message = bytes((index * 37 + 11) & 0xFF for index in range(length))
        hosted, native = _new_pair()

        _write8(hosted, native, SHA3_CONTROL, mode)
        _write8(hosted, native, SHA3_COMMAND, 1)
        _assert_terminal(hosted, native, (0x04, 0, mode))
        _feed(hosted, native, message)
        _assert_terminal(hosted, native, (0x04, 0, mode))

        _write8(hosted, native, SHA3_COMMAND, 3)
        _settle(native)
        _assert_terminal(hosted, native, (0x06, 0, mode))
        hosted_window, native_window = _read_window_pair(hosted, native)
        expected = constructor(message).digest()
        expected_window = expected + bytes(64 - output_length)
        assert hosted_window == native_window == expected_window

        _clear_pair(hosted, native, retained_mode=mode)


@pytest.mark.parametrize(
    ("mode", "rate", "constructor"),
    (
        (2, 168, hashlib.shake_128),
        (3, 136, hashlib.shake_256),
    ),
)
def test_shake_terminal_windows_match_native_across_sponge_rates(
    mode: int,
    rate: int,
    constructor,
) -> None:
    message = bytes((index * 19 + 0xA5) & 0xFF for index in range(rate + 1))
    hosted, native = _new_pair()

    _write8(hosted, native, SHA3_CONTROL, mode)
    _write8(hosted, native, SHA3_COMMAND, 1)
    _feed(hosted, native, message)
    _write8(hosted, native, SHA3_COMMAND, 3)
    _settle(native)

    actual_hosted = bytearray()
    actual_native = bytearray()
    for window_index in range(3):
        _assert_terminal(hosted, native, (0x06, 0, mode))
        hosted_window, native_window = _read_window_pair(hosted, native)
        actual_hosted.extend(hosted_window)
        actual_native.extend(native_window)
        if window_index != 2:
            _write8(hosted, native, SHA3_COMMAND, 4)
            _settle(native)

    expected = constructor(message).digest(192)
    assert bytes(actual_hosted) == bytes(actual_native) == expected
    _clear_pair(hosted, native, retained_mode=mode)


def test_raw_keccak_byte_and_qword_lane_mapping_match_native() -> None:
    hosted, native = _new_pair()
    initial = [
        (0x0123456789ABCDEF * (index + 1)) & ((1 << 64) - 1)
        for index in range(25)
    ]

    for lane_index, lane in enumerate(initial):
        _write8(hosted, native, SHA3_STATE_INDEX, lane_index)
        if lane_index % 2 == 0:
            _write64(hosted, native, SHA3_STATE_DATA, lane)
        else:
            lane_bytes = lane.to_bytes(8, "little")
            for byte_index, value in enumerate(lane_bytes):
                _write8(
                    hosted,
                    native,
                    SHA3_STATE_DATA + byte_index,
                    value,
                )
    _assert_terminal(hosted, native, (0x08, 0, 0))

    # Read each lane through the opposite width.  STATE_INDEX must remain
    # explicit, while STATE_DATA maps each lane little-endian in both paths.
    for lane_index, expected in enumerate(initial):
        _write8(hosted, native, SHA3_STATE_INDEX, lane_index)
        if lane_index % 2 == 0:
            hosted_bytes = bytearray()
            native_bytes = bytearray()
            for byte_index in range(8):
                hosted_value, native_value = _read8_pair(
                    hosted,
                    native,
                    SHA3_STATE_DATA + byte_index,
                )
                hosted_bytes.append(hosted_value)
                native_bytes.append(native_value)
            assert bytes(hosted_bytes) == bytes(native_bytes) == (
                expected.to_bytes(8, "little")
            )
        else:
            assert _read64_pair(hosted, native, SHA3_STATE_DATA) == (
                expected,
                expected,
            )

    _write8(hosted, native, SHA3_COMMAND, 6)
    _settle(native)
    _assert_terminal(hosted, native, (0x0A, 0, 0))

    expected_lanes = keccak_f1600(initial)
    for lane_index, expected in enumerate(expected_lanes):
        _write8(hosted, native, SHA3_STATE_INDEX, lane_index)
        assert _read64_pair(hosted, native, SHA3_STATE_DATA) == (
            expected,
            expected,
        )

    _clear_pair(hosted, native, retained_mode=0)


def test_protocol_errors_internal_failure_and_clear_match_native() -> None:
    hosted, native = _new_pair()

    for action, expected in (
        (
            lambda: _write8(hosted, native, SHA3_COMMAND, 2),
            (0x03, 1, 0),
        ),
        (
            lambda: _write8(hosted, native, SHA3_CONTROL, 4),
            (0x03, 3, 0),
        ),
        (
            lambda: _write8(hosted, native, SHA3_STATE_INDEX, 25),
            (0x03, 4, 0),
        ),
    ):
        action()
        _assert_terminal(hosted, native, expected)
        _clear_pair(hosted, native, retained_mode=0)

    # Reading raw state without a raw owner faults only after returning zero.
    assert _read8_pair(hosted, native, SHA3_STATE_DATA) == (0, 0)
    _assert_terminal(hosted, native, (0x03, 2, 0))
    _clear_pair(hosted, native, retained_mode=0)

    # A sponge owner rejects raw state access and retains the packed owner.
    _write8(hosted, native, SHA3_COMMAND, 1)
    _write8(hosted, native, SHA3_STATE_INDEX, 1)
    _assert_terminal(hosted, native, (0x07, 2, 0))
    _clear_pair(hosted, native, retained_mode=0)

    # NEXT in a fixed-output mode reports INVALID_MODE before its state check.
    _write8(hosted, native, SHA3_COMMAND, 1)
    _write8(hosted, native, SHA3_COMMAND, 4)
    _assert_terminal(hosted, native, (0x07, 3, 0))
    _clear_pair(hosted, native, retained_mode=0)

    _write8(hosted, native, SHA3_CONTROL, 3)
    _write8(hosted, native, SHA3_COMMAND, 1)
    _feed(hosted, native, b"terminal failure")
    hosted.inject_operation_failure_once()
    native._crypto_sha3_test_fail_next()
    _write8(hosted, native, SHA3_COMMAND, 3)
    _settle(native)
    _assert_terminal(hosted, native, (0x03, 5, 3))
    _clear_pair(hosted, native, retained_mode=3)


@pytest.mark.parametrize(
    "capabilities",
    (
        0,
        CRYPTO_CAP_SHA3_STREAM,
        CRYPTO_CAP_KECCAK_F1600,
        FULL_CAPABILITIES,
    ),
)
def test_stream_and_raw_feature_masks_match_native(capabilities: int) -> None:
    hosted, native = _new_pair(capabilities)
    stream = bool(capabilities & CRYPTO_CAP_SHA3_STREAM)
    raw = bool(capabilities & CRYPTO_CAP_KECCAK_F1600)
    assert hosted.capabilities == capabilities

    if stream:
        _write8(hosted, native, SHA3_COMMAND, 1)
        _assert_terminal(hosted, native, (0x04, 0, 0))
        _clear_pair(hosted, native, retained_mode=0)
    else:
        assert _read8_pair(hosted, native, SHA3_CONTROL) == (0, 0)
        assert _read8_pair(hosted, native, SHA3_DATA_OUTPUT) == (0, 0)
        _assert_terminal(hosted, native, (0, 0, 0))
        _write8(hosted, native, SHA3_CONTROL, 4)
        _assert_terminal(hosted, native, (0x03, 6, 0))
        _clear_pair(hosted, native, retained_mode=0)
        _write8(hosted, native, SHA3_COMMAND, 1)
        _assert_terminal(hosted, native, (0x03, 6, 0))
        _clear_pair(hosted, native, retained_mode=0)

    if raw:
        _write64(hosted, native, SHA3_STATE_DATA, 0x8877665544332211)
        _write8(hosted, native, SHA3_COMMAND, 6)
        _settle(native)
        _assert_terminal(hosted, native, (0x0A, 0, 0))
        _clear_pair(hosted, native, retained_mode=0)
    else:
        assert _read8_pair(hosted, native, SHA3_STATE_INDEX) == (0, 0)
        assert _read8_pair(hosted, native, SHA3_STATE_DATA) == (0, 0)
        _assert_terminal(hosted, native, (0, 0, 0))
        _write8(hosted, native, SHA3_STATE_DATA, 1)
        _assert_terminal(hosted, native, (0x03, 6, 0))
        _clear_pair(hosted, native, retained_mode=0)


def test_native_priority_cases_with_disabled_opposing_features_match() -> None:
    hosted, native = _new_pair(CRYPTO_CAP_KECCAK_F1600)
    _write64(hosted, native, SHA3_STATE_DATA, 0x0123_4567_89AB_CDEF)
    _assert_terminal(hosted, native, (0x08, 0, 0))

    # Native executable INIT classifies the existing raw owner as a conflict
    # before considering that stream support is absent.
    _write8(hosted, native, SHA3_COMMAND, 1)
    _assert_terminal(hosted, native, (0x0B, 2, 0))
    _clear_pair(hosted, native, retained_mode=0)

    hosted, native = _new_pair(CRYPTO_CAP_KECCAK_F1600)
    _write64(hosted, native, SHA3_STATE_DATA, 0x8877_6655_4433_2211)
    # A disabled DOUT read still records the opposing raw-owner conflict.
    assert _read8_pair(hosted, native, SHA3_DATA_OUTPUT) == (0, 0)
    _assert_terminal(hosted, native, (0x0B, 2, 0))
    _clear_pair(hosted, native, retained_mode=0)

    hosted, native = _new_pair(CRYPTO_CAP_SHA3_STREAM)
    _write8(hosted, native, SHA3_COMMAND, 1)
    # A disabled raw-state read still records the opposing sponge conflict.
    assert _read8_pair(hosted, native, SHA3_STATE_DATA) == (0, 0)
    _assert_terminal(hosted, native, (0x07, 2, 0))
    _clear_pair(hosted, native, retained_mode=0)
