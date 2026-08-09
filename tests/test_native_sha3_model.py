"""Focused qualification for the native SHA3/SHAKE/raw-Keccak MMIO model.

The extension must be rebuilt from the current sources before running this
module.  These tests intentionally exercise the native object directly so a
Python device implementation cannot mask a native contract divergence.
"""

from __future__ import annotations

import hashlib
import random

import pytest

import _mp64_accel
from accel_wrapper import NativeSystemState
from asm import assemble
from devices import MMIO_BASE, SHA3_BASE
from system import MegapadSystem


CMD = 0x00
STATUS = 0x01
CTRL = 0x02
ERROR = 0x03
DIN = 0x08
DOUT = 0x10
STATE_INDEX = 0x50
STATE_DATA = 0x58


def _new_device():
    state = _mp64_accel.CPUState()
    state.init_crypto()
    return state


def _read(state, register: int) -> int:
    return state.crypto_read8(SHA3_BASE + register)


def _write(state, register: int, value: int) -> None:
    state.crypto_write8(SHA3_BASE + register, value)


def _read_window(state) -> bytes:
    return bytes(_read(state, DOUT + index) for index in range(64))


def _reference_window(mode: int, message: bytes) -> bytes:
    if mode == 0:
        return hashlib.sha3_256(message).digest() + bytes(32)
    if mode == 1:
        return hashlib.sha3_512(message).digest()
    if mode == 2:
        return hashlib.shake_128(message).digest(64)
    return hashlib.shake_256(message).digest(64)


def _write_lane(state, index: int, value: int) -> None:
    _write(state, STATE_INDEX, index)
    state.crypto_write64(SHA3_BASE + STATE_DATA, value)


def _read_lane(state, index: int) -> int:
    _write(state, STATE_INDEX, index)
    return state.crypto_read64(SHA3_BASE + STATE_DATA)


def _warm_first_instruction_line(system: MegapadSystem) -> None:
    valid_bytes, tags, data_bytes = system.cpu._cs.icache_snapshot()
    valid = bytearray(valid_bytes)
    tags = list(tags)
    data = bytearray(data_bytes)
    valid[0] = 1
    tags[0] = 0
    data[:16] = system.cpu.mem[:16]
    system.cpu._cs.icache_restore(bytes(valid), tags, bytes(data))


def _rol64(value: int, count: int) -> int:
    value &= (1 << 64) - 1
    return ((value << count) | (value >> ((64 - count) & 63))) & (
        (1 << 64) - 1
    )


def _keccak_oracle(input_lanes: list[int]) -> list[int]:
    """Independent, test-local Keccak-f[1600] permutation."""
    lanes = [value & ((1 << 64) - 1) for value in input_lanes]
    rotations = (
        (0, 36, 3, 41, 18),
        (1, 44, 10, 45, 2),
        (62, 6, 43, 15, 61),
        (28, 55, 25, 21, 56),
        (27, 20, 39, 8, 14),
    )
    round_constants = (
        0x0000000000000001, 0x0000000000008082,
        0x800000000000808A, 0x8000000080008000,
        0x000000000000808B, 0x0000000080000001,
        0x8000000080008081, 0x8000000000008009,
        0x000000000000008A, 0x0000000000000088,
        0x0000000080008009, 0x000000008000000A,
        0x000000008000808B, 0x800000000000008B,
        0x8000000000008089, 0x8000000000008003,
        0x8000000000008002, 0x8000000000000080,
        0x000000000000800A, 0x800000008000000A,
        0x8000000080008081, 0x8000000000008080,
        0x0000000080000001, 0x8000000080008008,
    )
    mask = (1 << 64) - 1

    for round_constant in round_constants:
        parity = [
            lanes[x] ^ lanes[x + 5] ^ lanes[x + 10] ^
            lanes[x + 15] ^ lanes[x + 20]
            for x in range(5)
        ]
        delta = [
            parity[(x - 1) % 5] ^ _rol64(parity[(x + 1) % 5], 1)
            for x in range(5)
        ]
        for y in range(5):
            for x in range(5):
                lanes[x + 5 * y] ^= delta[x]

        mixed = [0] * 25
        for y in range(5):
            for x in range(5):
                mixed[y + 5 * ((2 * x + 3 * y) % 5)] = _rol64(
                    lanes[x + 5 * y], rotations[x][y]
                )
        for y in range(5):
            row = mixed[5 * y:5 * y + 5]
            for x in range(5):
                lanes[x + 5 * y] = (
                    row[x] ^ ((~row[(x + 1) % 5]) & row[(x + 2) % 5])
                ) & mask
        lanes[0] ^= round_constant
    return lanes


def test_native_sha3_window_and_access_preflight_are_exact() -> None:
    state = _new_device()

    assert state._native_singleton_preflight(SHA3_BASE + 0x00, 1, True) == 1
    assert state._native_singleton_preflight(SHA3_BASE + 0x5F, 1, False) == 1
    assert state._native_singleton_preflight(SHA3_BASE + 0x60, 1, False) == -1
    assert state._native_singleton_preflight(SHA3_BASE + 0x04, 1, False) == 0
    assert state._native_singleton_preflight(SHA3_BASE + STATUS, 1, True) == 0
    assert state._native_singleton_preflight(SHA3_BASE + DOUT, 1, True) == 0
    assert state._native_singleton_preflight(SHA3_BASE + DOUT, 2, False) == 0
    assert state._native_singleton_preflight(SHA3_BASE + DOUT, 4, False) == 0
    assert state._native_singleton_preflight(SHA3_BASE + DOUT, 8, False) == 1
    assert state._native_singleton_preflight(SHA3_BASE + DOUT + 1, 8, False) == 0
    assert state._native_singleton_preflight(
        SHA3_BASE + STATE_DATA, 8, True
    ) == 1

    with pytest.raises(ValueError):
        state.crypto_read8(SHA3_BASE + 0x51)
    with pytest.raises(ValueError):
        state.crypto_write8(SHA3_BASE + STATUS, 0)
    assert _read(state, ERROR) == 0
    assert _read(state, STATUS) == 0


def test_native_sha3_has_visible_busy_timing_and_fixed_output() -> None:
    state = _new_device()
    message = b"abc"

    _write(state, CTRL, 0)
    _write(state, CMD, 1)
    assert _read(state, STATUS) == 0x04
    for byte in message:
        _write(state, DIN, byte)
    _write(state, CMD, 3)
    assert _read(state, STATUS) == 0x05
    assert _read(state, DOUT) == 0
    assert _read(state, STATUS) == 0x05

    state.crypto_tick(23)
    assert _read(state, STATUS) == 0x05
    state.crypto_tick(1)
    assert _read(state, STATUS) == 0x06
    assert _read_window(state) == hashlib.sha3_256(message).digest() + bytes(32)

    _write(state, CMD, 7)
    assert _read(state, STATUS) == 0
    assert _read(state, CTRL) == 0
    assert state._crypto_sha3_test_zeroized()


@pytest.mark.parametrize(
    ("mode", "rate"),
    [(0, 136), (1, 72), (2, 168), (3, 136)],
)
def test_native_exact_rate_absorption_holds_the_following_byte(
    mode: int,
    rate: int,
) -> None:
    state = _new_device()
    message = bytes(index & 0xFF for index in range(rate))

    _write(state, CTRL, mode)
    _write(state, CMD, 1)
    for byte in message:
        _write(state, DIN, byte)
    assert _read(state, STATUS) == 0x05

    # Architecturally this write is held behind the automatic permutation.
    # The direct native callback latches it so it cannot be acknowledged and
    # discarded while direct qualification advances time explicitly.
    _write(state, DIN, 0xA5)
    state.crypto_tick(24)
    assert _read(state, STATUS) == 0x04
    _write(state, CMD, 3)
    state.crypto_tick(24)

    assert _read_window(state) == _reference_window(mode, message + b"\xA5")


def test_native_shake_windows_are_sequential_across_rate_boundary() -> None:
    state = _new_device()
    message = b"native SHAKE window boundary"

    _write(state, CTRL, 3)
    _write(state, CMD, 1)
    for byte in message:
        _write(state, DIN, byte)
    _write(state, CMD, 3)
    state.crypto_tick(24)
    output = bytearray(_read_window(state))

    _write(state, CMD, 4)
    assert _read(state, STATUS) == 0x05
    state.crypto_tick(1)
    output.extend(_read_window(state))

    _write(state, CMD, 4)
    state.crypto_tick(23)
    assert _read(state, STATUS) == 0x05
    state.crypto_tick(1)
    output.extend(_read_window(state))

    assert bytes(output) == hashlib.shake_256(message).digest(192)


def test_native_raw_keccak_lane_mapping_and_permutation() -> None:
    state = _new_device()
    initial = [
        (0x0123456789ABCDEF * (index + 1)) & ((1 << 64) - 1)
        for index in range(25)
    ]
    expected = _keccak_oracle(initial)

    for index, lane in enumerate(initial):
        _write_lane(state, index, lane)
    assert _read(state, STATUS) == 0x08

    # Byte access is little endian and does not advance STATE_INDEX.
    _write(state, STATE_INDEX, 7)
    assert _read(state, STATE_INDEX) == 7
    assert bytes(_read(state, STATE_DATA + i) for i in range(8)) == (
        initial[7].to_bytes(8, "little")
    )
    assert _read(state, STATE_INDEX) == 7

    _write(state, CMD, 6)
    assert _read(state, STATUS) == 0x09
    assert _read(state, STATE_DATA) == 0
    assert _read(state, STATUS) == 0x09
    state.crypto_tick(24)
    assert _read(state, STATUS) == 0x0A
    assert [_read_lane(state, index) for index in range(25)] == expected


def test_native_raw_zero_state_matches_published_keccak_vector() -> None:
    state = _new_device()
    expected = _keccak_oracle([0] * 25)
    assert expected[:5] == [
        0xF1258F7940E1DDE7,
        0x84D5CCF933C0478A,
        0xD598261EA65AA9EE,
        0xBD1547306F80494D,
        0x8B284E056253D057,
    ]

    _write(state, CMD, 6)
    state.crypto_tick(24)
    assert [_read_lane(state, index) for index in range(25)] == expected


def test_native_seeded_random_differential_hash_xof_and_raw_states() -> None:
    """Seeded varied inputs agree with independent library/local oracles."""
    rng = random.Random(0x4D50_3634_5348_4133)
    cases = (
        (0, 136, lambda message: hashlib.sha3_256(message).digest()),
        (1, 72, lambda message: hashlib.sha3_512(message).digest()),
        (2, 168, lambda message: hashlib.shake_128(message).digest(192)),
        (3, 136, lambda message: hashlib.shake_256(message).digest(192)),
    )

    for mode, rate, oracle in cases:
        lengths = (0, rate - 1, rate, rate + 1, rng.randrange(2, 3 * rate))
        for length in lengths:
            message = rng.randbytes(length)
            state = _new_device()
            _write(state, CTRL, mode)
            _write(state, CMD, 1)
            for byte in message:
                _write(state, DIN, byte)
                if _read(state, STATUS) == 0x05:
                    state.crypto_tick(24)
            _write(state, CMD, 3)
            state.crypto_tick(24)

            expected = oracle(message)
            if mode < 2:
                assert _read_window(state)[:len(expected)] == expected
            else:
                actual = bytearray(_read_window(state))
                for _ in range(2):
                    _write(state, CMD, 4)
                    state.crypto_tick(24)
                    actual.extend(_read_window(state))
                assert bytes(actual) == expected
            _write(state, CMD, 7)
            assert state._crypto_sha3_test_zeroized()

    for _ in range(4):
        initial = [rng.getrandbits(64) for _ in range(25)]
        state = _new_device()
        for index, lane in enumerate(initial):
            _write_lane(state, index, lane)
        _write(state, CMD, 6)
        state.crypto_tick(24)
        assert [_read_lane(state, index) for index in range(25)] == (
            _keccak_oracle(initial)
        )


def test_native_error_owner_clear_and_zeroization_contract() -> None:
    state = _new_device()

    _write(state, CMD, 2)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 1)
    _write(state, CMD, 7)
    _write(state, CTRL, 4)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 3)
    _write(state, CMD, 7)
    _write(state, STATE_INDEX, 25)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 4)
    _write(state, CMD, 7)

    assert _read(state, STATE_DATA) == 0
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 2)
    _write(state, CMD, 7)

    assert state._crypto_sha3_test_claim_wots()
    assert _read(state, STATUS) == 0x0D
    _write(state, CMD, 1)
    _write(state, CMD, 7)
    _write(state, STATE_INDEX, 3)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x0D, 0)
    state._crypto_sha3_test_release_wots()
    assert _read(state, STATUS) == 0

    _write_lane(state, 0, 0xDEADBEEFCAFEBABE)
    _write(state, CMD, 6)
    _write(state, CMD, 7)
    assert _read(state, STATUS) == 0x09
    state.crypto_tick(1)
    assert _read(state, STATUS) == 0
    assert state._crypto_sha3_test_zeroized()

    _write(state, CMD, 6)
    state._crypto_sha3_test_fail_next()
    state.crypto_tick(24)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 5)
    _write(state, CMD, 7)
    assert state._crypto_sha3_test_zeroized()


def test_native_feature_unavailability_is_independent() -> None:
    state = _new_device()
    state._crypto_sha3_test_set_features(False, True)
    assert _read(state, CTRL) == 0
    assert _read(state, DOUT) == 0
    assert (_read(state, STATUS), _read(state, ERROR)) == (0, 0)
    _write(state, CMD, 1)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 6)

    _write(state, CMD, 7)
    state._crypto_sha3_test_set_features(True, False)
    assert _read(state, STATE_INDEX) == 0
    assert (_read(state, STATUS), _read(state, ERROR)) == (0, 0)
    _write(state, STATE_DATA, 1)
    assert (_read(state, STATUS), _read(state, ERROR)) == (0x03, 6)


def test_native_system_clock_advances_shared_sha_service() -> None:
    owner = NativeSystemState(1)
    core = owner.core(0)

    core.crypto_write8(SHA3_BASE + CMD, 1)
    core.crypto_write8(SHA3_BASE + CMD, 3)
    assert core.crypto_read8(SHA3_BASE + STATUS) == 0x05
    owner.advance_system_cycles(23)
    assert core.crypto_read8(SHA3_BASE + STATUS) == 0x05
    owner.advance_system_cycles(1)
    assert core.crypto_read8(SHA3_BASE + STATUS) == 0x06
    assert bytes(
        core.crypto_read8(SHA3_BASE + DOUT + index) for index in range(32)
    ) == hashlib.sha3_256(b"").digest()


def test_cycle_bus_holds_post_rate_din_until_absorb_boundary() -> None:
    code = assemble("st.b r1, r2\nhalt")
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    _warm_first_instruction_line(system)

    native = system.cpu._cs
    message = bytes(range(136))
    native.crypto_write8(SHA3_BASE + CMD, 1)
    for byte in message:
        native.crypto_write8(SHA3_BASE + DIN, byte)
    assert native.crypto_read8(SHA3_BASE + STATUS) == 0x05

    system.cpu.regs[1] = MMIO_BASE + SHA3_BASE + DIN
    system.cpu.regs[2] = 0xA5

    first = system.run_cycle_batch(4, max_instructions=1)
    assert first.instructions_executed == 0
    assert system.cpu.pc == 0
    assert system._native_system.cycle_execution_pending
    active = system._native_system._main_bus_snapshot().active_grant
    assert active is not None
    assert active.request.address == MMIO_BASE + SHA3_BASE + DIN

    system.run_cycle_batch(19, max_instructions=1)
    assert system._native_system.system_cycles == 23
    assert native.crypto_read8(SHA3_BASE + STATUS) == 0x05
    assert system.cpu.pc == 0
    assert system._native_system._main_bus_snapshot().active_grant is not None

    boundary = system.run_cycle_batch(1, max_instructions=1)
    assert boundary.instructions_executed == 1
    assert system._native_system.system_cycles == 24
    assert native.crypto_read8(SHA3_BASE + STATUS) == 0x04
    assert system._native_system._main_bus_snapshot().active_grant is None
    assert not system._native_system.cycle_execution_pending

    native.crypto_write8(SHA3_BASE + CMD, 3)
    native.crypto_tick(24)
    assert bytes(
        native.crypto_read8(SHA3_BASE + DOUT + index)
        for index in range(32)
    ) == hashlib.sha3_256(message + b"\xA5").digest()
