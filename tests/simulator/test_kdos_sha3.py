"""Contiguous unchanged-source acceptance for KDOS SHA-3 and entropy."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from shared.keccak import KECCAK_LANES, keccak_f1600
from simulator.entropy import (
    TRNG_LIMIT,
    TRNG_OFFSET,
    TRNG_RAND8,
    TRNG_RAND64,
    TRNG_SEED,
    TRNG_STATUS,
    TRNGAccessError,
    TRNGUnavailableError,
)
from simulator.errors import ExecutionError
from simulator.memory import (
    EXTERNAL_BASE,
    HBW_BASE,
    MMIO_BASE,
    VRAM_BASE,
    MMIOAccessError,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import ExecutionContext, MegaForthRuntime
from simulator.sha3 import (
    CRYPTO_STATUS_HARDWARE,
    CRYPTO_STATUS_OK,
    CRYPTO_STATUS_PROTECTED,
    CRYPTO_STATUS_RANGE,
    CRYPTO_STATUS_STATE,
    CRYPTO_STATUS_UNSUPPORTED,
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
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
    _load_aes,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-sha3-1072-1216.f"

FIRST_LINE = 1072
LAST_LINE = 1216
SLICE_SHA256 = "a37aa3609e32f4b7fc966c575dbc2f5f0a59362a89c18ea076b0143e52c622f2"
SLICE_GIT_BLOB = "d547de0490759ee0ebb6bfed1e070488b409dcfa"
DEFINITIONS = (
    b"SHA3-256-MODE",
    b"SHA3-512-MODE",
    b"SHAKE128-MODE",
    b"SHAKE256-MODE",
    b"CRYPTO-OK",
    b"CRYPTO-UNSUPPORTED",
    b"CRYPTO-STATE",
    b"CRYPTO-RANGE",
    b"CRYPTO-PROTECTED",
    b"CRYPTO-TIMEOUT",
    b"CRYPTO-HARDWARE",
    b"CRYPTO-CAP-SHA3-STREAM",
    b"CRYPTO-CAP-KECCAK-F1600",
    b"_CRYPTO-SPAN-STATUS",
    b"SHA3",
    b"SHA3-512",
    b"_SHAKE-CLEAN-ERROR",
    b"(SHAKE)",
    b"SHAKE128",
    b"SHAKE256",
    b"SHAKE-STREAM",
    b".SHA3-STATUS",
    b".SHA3",
    b"RANDOM32",
    b"RANDOM16",
    b"RAND-RANGE",
)
BIOS_WORDS = (
    "2SWAP",
    "MOD",
    "ABS",
    "LSHIFT",
    "EMIT",
    "CALLER-SPAN-STATUS",
    "SHA3-BEGIN",
    "SHA3-UPDATE",
    "SHA3-FINAL",
    "SHAKE-FINAL",
    "SHAKE-READ",
    "SHA3-CLEAR",
    "SHA3-STATUS@",
    "SHA3-MODE@",
    "KECCAK-F1600",
    "RANDOM",
    "RANDOM8",
    "SEED-RNG",
)

SOURCE_ADDRESS = 0x20_000
OUTPUT_ADDRESS = 0x21_000
STATE_ADDRESS = 0x22_000
GUARD_ADDRESS = 0x23_000


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_sha3(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_sha3(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_sha3(_load_aes(runtime))


@pytest.fixture
def loaded_sha3() -> MegaForthRuntime:
    return _load_sha3()


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name, step_budget=250_000)
    result = context.data.snapshot()
    context.data.clear()
    assert context.returns.snapshot() == ()
    return result


def _write_mmio(runtime: MegaForthRuntime, offset: int, payload: bytes) -> None:
    for index, value in enumerate(payload):
        runtime.memory.write8(MMIO_BASE + offset + index, value)


def _read_mmio(runtime: MegaForthRuntime, offset: int, length: int) -> bytes:
    return bytes(
        runtime.memory.read8(MMIO_BASE + offset + index)
        for index in range(length)
    )


def test_sha3_slice_is_exact_and_publishes_complete_ledger(
    loaded_sha3: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_sha3.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_sha3.find(name) is not None
    assert _execute(loaded_sha3, "CRYPTO-CAPS@") == (7,)
    assert _execute(loaded_sha3, "SHA3-STATUS@") == (0,)
    assert _execute(loaded_sha3, "SHA3-MODE@") == (0,)
    assert loaded_sha3.uart_output == b""


def test_new_scalar_bios_closure_matches_executable_cell_semantics() -> None:
    runtime = MegaForthRuntime()

    assert _execute(runtime, "2SWAP", 1, 2, 3, 4) == (3, 4, 1, 2)
    assert _execute(runtime, "LSHIFT", 1, 0) == (1,)
    assert _execute(runtime, "LSHIFT", 1, 65) == (2,)
    assert _execute(runtime, "ABS", -7) == (7,)
    assert _execute(runtime, "ABS", 1 << 63) == (1 << 63,)
    for dividend, divisor, expected in (
        (7, 3, 1),
        (-7, 3, -1),
        (7, -3, 1),
        (-7, -3, -1),
    ):
        assert _execute(runtime, "MOD", dividend, divisor) == (
            expected & MASK64,
        )
    with pytest.raises(ExecutionError, match="modulo trapped"):
        _execute(runtime, "MOD", 1, 0)

    assert _execute(runtime, "EMIT", 0x141) == ()
    assert runtime.drain_uart_output() == b"A"


def test_pure_keccak_has_exact_shape_and_published_zero_state_oracle() -> None:
    result = keccak_f1600([0] * KECCAK_LANES)
    assert len(result) == KECCAK_LANES
    assert result[:5] == (
        0xF1258F7940E1DDE7,
        0x84D5CCF933C0478A,
        0xD598261EA65AA9EE,
        0xBD1547306F80494D,
        0x8B284E056253D057,
    )
    with pytest.raises(ValueError, match="exactly 25"):
        keccak_f1600([0] * 24)
    with pytest.raises(ValueError, match="uint64"):
        keccak_f1600([0] * 24 + [-1])
    with pytest.raises(TypeError, match="uint64"):
        keccak_f1600([0] * 24 + [True])


def test_caller_span_status_enforces_geometry_prefix_and_result_slot() -> None:
    memory = create_one_core_address_space(
        external_size=0x2000,
        vram_size=0x2000,
        hbw_size=0x2000,
    )
    runtime = MegaForthRuntime(memory=memory)
    floor = runtime.dictionary.numeric_rollback_floor
    boundary = runtime.main_context.data.pointer - 8

    for address in (0, MASK64, MMIO_BASE):
        assert runtime.caller_span_status(
            runtime.main_context,
            address,
            0,
        ) == 0
    for address, length in (
        (0, 1),
        (1 << 63, 1),
        (floor, 1 << 63),
        (MASK64 - 1, 4),
        (0x0F_FFFF, 2),
        (MMIO_BASE, 1),
    ):
        assert runtime.caller_span_status(
            runtime.main_context,
            address,
            length,
        ) == 2

    assert runtime.caller_span_status(
        runtime.main_context,
        floor - 1,
        1,
    ) == 3
    assert runtime.caller_span_status(
        runtime.main_context,
        floor,
        boundary - floor,
    ) == 0
    assert runtime.caller_span_status(
        runtime.main_context,
        boundary,
        1,
    ) == 3
    for address in (EXTERNAL_BASE, VRAM_BASE, HBW_BASE):
        assert runtime.caller_span_status(
            runtime.main_context,
            address,
            0x2000,
        ) == 0

    scratch = ExecutionContext()
    assert runtime.caller_span_status(scratch, floor, 1) == 0
    assert runtime.caller_span_status(scratch, boundary, 1) == 3


def test_caller_span_public_word_uses_post_argument_result_boundary() -> None:
    runtime = MegaForthRuntime()
    floor = runtime.dictionary.numeric_rollback_floor
    boundary = runtime.main_context.data.pointer - 8

    assert _execute(runtime, "CALLER-SPAN-STATUS", floor, 1) == (0,)
    assert _execute(runtime, "CALLER-SPAN-STATUS", boundary, 1) == (3,)
    assert _execute(runtime, "CALLER-SPAN-STATUS", MASK64, 0) == (0,)


@pytest.mark.parametrize("mode", range(4))
def test_direct_sha_mmio_matches_hashlib_at_rate_boundaries(mode: int) -> None:
    rate = (136, 72, 168, 136)[mode]
    oracle = (
        lambda value: hashlib.sha3_256(value).digest(),
        lambda value: hashlib.sha3_512(value).digest(),
        lambda value: hashlib.shake_128(value).digest(64),
        lambda value: hashlib.shake_256(value).digest(64),
    )[mode]

    for length in (0, rate - 1, rate, rate + 1):
        runtime = MegaForthRuntime()
        message = bytes((index * 37 + mode) & 0xFF for index in range(length))
        runtime.memory.write8(MMIO_BASE + SHA3_CONTROL, mode)
        runtime.memory.write8(MMIO_BASE + SHA3_COMMAND, 1)
        assert runtime.memory.read8(MMIO_BASE + SHA3_STATUS) == 0x04
        for byte in message:
            runtime.memory.write8(MMIO_BASE + SHA3_DATA_INPUT, byte)
        runtime.memory.write8(MMIO_BASE + SHA3_COMMAND, 3)
        assert runtime.memory.read8(MMIO_BASE + SHA3_STATUS) == 0x06
        expected = oracle(message)
        assert _read_mmio(runtime, SHA3_DATA_OUTPUT, len(expected)) == expected


def test_direct_sha_mmio_access_shapes_and_raw_qword_staging_are_exact() -> None:
    runtime = MegaForthRuntime()
    memory = runtime.memory

    assert memory.read8(MMIO_BASE + SHA3_COMMAND) == 0
    assert memory.read8(MMIO_BASE + SHA3_DATA_INPUT) == 0
    invalid = (
        (SHA3_OFFSET + 1, 2, False),
        (SHA3_OFFSET + 4, 1, False),
        (SHA3_DATA_OUTPUT, 4, False),
        (SHA3_DATA_OUTPUT, 1, True),
        (SHA3_LIMIT, 1, False),
    )
    for offset, width, write in invalid:
        operation = (
            {1: memory.write8, 2: memory.write16, 4: memory.write32}[width]
            if write
            else {1: memory.read8, 2: memory.read16, 4: memory.read32}[width]
        )
        with pytest.raises(MMIOAccessError) as caught:
            if write:
                operation(MMIO_BASE + offset, 0)
            else:
                operation(MMIO_BASE + offset)
        if offset != SHA3_LIMIT:
            assert isinstance(caught.value.__cause__, SHA3AccessError)

    lane = 0x0123_4567_89AB_CDEF
    memory.write8(MMIO_BASE + SHA3_STATE_INDEX, 7)
    memory.write64(MMIO_BASE + SHA3_STATE_DATA, lane)
    assert memory.read8(MMIO_BASE + SHA3_STATUS) == 0x08
    assert memory.read64(MMIO_BASE + SHA3_STATE_DATA) == lane
    assert _read_mmio(runtime, SHA3_STATE_DATA, 8) == lane.to_bytes(8, "little")
    memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
    assert memory.read8(MMIO_BASE + SHA3_STATUS) == 0
    assert runtime.sha3.private_zeroized()


@pytest.mark.parametrize(
    ("word", "oracle", "length"),
    (
        ("SHA3", hashlib.sha3_256, 32),
        ("SHA3-512", hashlib.sha3_512, 64),
    ),
)
def test_unchanged_fixed_hash_wrappers_match_hashlib_and_allow_aliases(
    loaded_sha3: MegaForthRuntime,
    word: str,
    oracle,
    length: int,
) -> None:
    for message in (b"", b"abc", bytes(range(137))):
        loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, message)
        loaded_sha3.memory.fill(OUTPUT_ADDRESS, length, 0xA5)
        assert _execute(
            loaded_sha3,
            word,
            SOURCE_ADDRESS,
            len(message),
            OUTPUT_ADDRESS,
        ) == (CRYPTO_STATUS_OK,)
        assert loaded_sha3.memory.read_bytes(OUTPUT_ADDRESS, length) == (
            oracle(message).digest()
        )

    message = bytes(range(64))
    loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, message)
    assert _execute(
        loaded_sha3,
        word,
        SOURCE_ADDRESS,
        len(message),
        SOURCE_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_sha3.memory.read_bytes(SOURCE_ADDRESS, length) == (
        oracle(message).digest()
    )


def test_checked_segmented_sha3_preserves_one_owner_and_stages_publication(
    loaded_sha3: MegaForthRuntime,
) -> None:
    message = bytes((index * 19) & 0xFF for index in range(300))
    loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, message)

    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (CRYPTO_STATUS_OK,)
    assert loaded_sha3.sha3.checked_owner == (0, 0)
    for offset, length in ((0, 0), (0, 17), (17, 119), (136, 164)):
        assert _execute(
            loaded_sha3,
            "SHA3-UPDATE",
            SOURCE_ADDRESS + offset,
            length,
        ) == (CRYPTO_STATUS_OK,)
    loaded_sha3.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    assert _execute(
        loaded_sha3,
        "SHA3-FINAL",
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_sha3.memory.read_bytes(OUTPUT_ADDRESS, 32) == (
        hashlib.sha3_256(message).digest()
    )
    assert loaded_sha3.sha3.checked_owner is None
    assert loaded_sha3.sha3.private_zeroized()


@pytest.mark.parametrize("mode", (2, 3))
@pytest.mark.parametrize("output_length", (0, 1, 31, 32, 33, 64, 65, 137, 201))
def test_unchanged_shake_wrappers_cross_windows_and_rates(
    loaded_sha3: MegaForthRuntime,
    mode: int,
    output_length: int,
) -> None:
    message = bytes(range(173))
    loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, message)
    loaded_sha3.memory.fill(OUTPUT_ADDRESS, max(output_length, 1), 0xA5)
    word = "SHAKE128" if mode == 2 else "SHAKE256"
    oracle = hashlib.shake_128 if mode == 2 else hashlib.shake_256

    assert _execute(
        loaded_sha3,
        word,
        SOURCE_ADDRESS,
        len(message),
        OUTPUT_ADDRESS,
        output_length,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_sha3.memory.read_bytes(OUTPUT_ADDRESS, output_length) == (
        oracle(message).digest(output_length)
    )
    assert loaded_sha3.sha3.checked_owner is None


def test_shake_stream_uses_finalized_owner_and_rejects_bad_block_counts(
    loaded_sha3: MegaForthRuntime,
) -> None:
    message = b"stream me"
    loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, message)
    assert _execute(loaded_sha3, "SHA3-BEGIN", 3) == (0,)
    assert _execute(
        loaded_sha3,
        "SHA3-UPDATE",
        SOURCE_ADDRESS,
        len(message),
    ) == (0,)
    assert _execute(loaded_sha3, "SHAKE-FINAL") == (0,)
    assert _execute(
        loaded_sha3,
        "SHAKE-STREAM",
        OUTPUT_ADDRESS,
        5,
    ) == (0,)
    assert loaded_sha3.memory.read_bytes(OUTPUT_ADDRESS, 160) == (
        hashlib.shake_256(message).digest(160)
    )
    assert loaded_sha3.sha3.checked_owner is None

    assert _execute(loaded_sha3, "SHA3-BEGIN", 2) == (0,)
    assert _execute(loaded_sha3, "SHAKE-FINAL") == (0,)
    assert _execute(
        loaded_sha3,
        "SHAKE-STREAM",
        OUTPUT_ADDRESS,
        1 << 63,
    ) == (CRYPTO_STATUS_RANGE,)
    assert loaded_sha3.sha3.checked_owner is None


def test_checked_failures_clean_up_without_partial_destination_publication(
    loaded_sha3: MegaForthRuntime,
) -> None:
    assert _execute(loaded_sha3, "SHA3-BEGIN", 4) == (
        CRYPTO_STATUS_RANGE,
    )
    assert loaded_sha3.sha3.checked_owner is None

    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (0,)
    assert _execute(loaded_sha3, "SHA3-BEGIN", 1) == (
        CRYPTO_STATUS_STATE,
    )
    assert loaded_sha3.sha3.checked_owner == (0, 0)
    assert _execute(loaded_sha3, "SHA3-CLEAR") == (0,)

    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (0,)
    assert _execute(
        loaded_sha3,
        "SHA3-UPDATE",
        1,
        1,
    ) == (CRYPTO_STATUS_PROTECTED,)
    assert loaded_sha3.sha3.checked_owner is None

    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (0,)
    loaded_sha3.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    loaded_sha3.sha3.inject_operation_failure_once()
    assert _execute(loaded_sha3, "SHA3-FINAL", OUTPUT_ADDRESS) == (
        CRYPTO_STATUS_HARDWARE,
    )
    assert loaded_sha3.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        [0xA5] * 32
    )
    assert loaded_sha3.sha3.checked_owner is None


def test_unchanged_shake_preflights_complete_multiwindow_output() -> None:
    memory = create_one_core_address_space(external_size=32)
    runtime = _load_sha3(MegaForthRuntime(memory=memory))
    message = b"complete output preflight"
    runtime.memory.write_bytes(SOURCE_ADDRESS, message)
    runtime.memory.fill(EXTERNAL_BASE, 32, 0xA5)

    assert _execute(
        runtime,
        "SHAKE256",
        SOURCE_ADDRESS,
        len(message),
        EXTERNAL_BASE,
        64,
    ) == (CRYPTO_STATUS_RANGE,)
    assert runtime.memory.read_bytes(EXTERNAL_BASE, 32) == bytes([0xA5] * 32)
    assert runtime.sha3.checked_owner is None
    assert runtime.sha3.private_zeroized()


def test_clear_failure_retains_checked_owner_fail_closed(
    loaded_sha3: MegaForthRuntime,
) -> None:
    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (0,)
    loaded_sha3.sha3.inject_clear_failure_once()
    assert _execute(loaded_sha3, "SHA3-CLEAR") == (
        CRYPTO_STATUS_HARDWARE,
    )
    assert loaded_sha3.sha3.checked_owner == (0, 0)
    assert _execute(loaded_sha3, "SHA3-BEGIN", 0) == (
        CRYPTO_STATUS_STATE,
    )


def test_checked_keccak_is_in_place_staged_and_matches_pure_oracle(
    loaded_sha3: MegaForthRuntime,
) -> None:
    lanes = [
        (0x0123_4567_89AB_CDEF * (index + 1)) & MASK64
        for index in range(KECCAK_LANES)
    ]
    source = b"".join(lane.to_bytes(8, "little") for lane in lanes)
    expected = b"".join(
        lane.to_bytes(8, "little") for lane in keccak_f1600(lanes)
    )
    loaded_sha3.memory.write_bytes(STATE_ADDRESS, source)

    assert _execute(
        loaded_sha3,
        "KECCAK-F1600",
        STATE_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_sha3.memory.read_bytes(STATE_ADDRESS, 200) == expected
    assert loaded_sha3.sha3.private_zeroized()

    loaded_sha3.memory.write_bytes(STATE_ADDRESS, source)
    loaded_sha3.sha3.inject_operation_failure_once()
    assert _execute(
        loaded_sha3,
        "KECCAK-F1600",
        STATE_ADDRESS,
    ) == (CRYPTO_STATUS_HARDWARE,)
    assert loaded_sha3.memory.read_bytes(STATE_ADDRESS, 200) == source


def test_capability_disabled_profile_returns_unsupported_without_ownership() -> None:
    memory = create_one_core_address_space(crypto_capabilities=0)
    runtime = MegaForthRuntime(memory=memory)

    assert _execute(runtime, "SHA3-BEGIN", 99) == (
        CRYPTO_STATUS_UNSUPPORTED,
    )
    assert _execute(runtime, "KECCAK-F1600", 0) == (
        CRYPTO_STATUS_UNSUPPORTED,
    )
    assert _execute(runtime, "SHA3-CLEAR") == (
        CRYPTO_STATUS_UNSUPPORTED,
    )
    assert runtime.sha3.checked_owner is None


def test_entropy_stream_is_deterministic_isolated_and_bios_byte_exact() -> None:
    seed = b"exact replay seed"
    first = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    second = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )

    expected = bytes(
        second.memory.read8(MMIO_BASE + TRNG_RAND64 + index)
        for index in range(8)
    )
    assert _execute(first, "RANDOM") == (
        int.from_bytes(expected, "little"),
    )
    assert first.entropy.pool_position == 8
    assert second.entropy.pool_position == 8

    isolated = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    assert _execute(isolated, "RANDOM8") == (expected[0],)
    assert first.entropy.pool_position == 8


def test_entropy_preflight_accepts_every_in_window_supported_width() -> None:
    runtime = MegaForthRuntime()
    for offset in range(TRNG_OFFSET - 1, TRNG_LIMIT + 1):
        for width in (1, 2, 3, 4, 8):
            admitted = (
                width in (1, 2, 4, 8)
                and TRNG_OFFSET <= offset
                and offset + width <= TRNG_LIMIT
            )
            for write in (False, True):
                if admitted:
                    runtime.entropy.preflight(offset, width, write=write)
                else:
                    with pytest.raises(TRNGAccessError):
                        runtime.entropy.preflight(offset, width, write=write)


def test_entropy_unaligned_wide_access_decomposes_bytes_after_one_preflight() -> None:
    seed = b"unaligned TRNG spans"
    wide = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    bytewise = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )

    expected = (
        bytewise.memory.read8(MMIO_BASE + TRNG_RAND64 + 1)
        | bytewise.memory.read8(MMIO_BASE + TRNG_RAND64 + 2) << 8
    )
    assert wide.memory.read16(MMIO_BASE + TRNG_RAND64 + 1) == expected

    bridge = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    reference = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    first_random = reference.memory.read8(MMIO_BASE + TRNG_RAND8)
    assert bridge.memory.read16(MMIO_BASE + TRNG_RAND64 - 1) == (
        first_random << 8
    )

    mixed = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    baseline = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    mixed.memory.write32(MMIO_BASE + TRNG_SEED - 1, 0x4433_2211)
    plain = bytes(
        baseline.memory.read8(MMIO_BASE + TRNG_RAND8) for _ in range(3)
    )
    changed = bytes(
        mixed.memory.read8(MMIO_BASE + TRNG_RAND8) for _ in range(3)
    )
    assert changed == bytes(
        value ^ supplement
        for value, supplement in zip(plain, b"\x22\x33\x44")
    )

    crossing = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    untouched = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    with pytest.raises(MMIOAccessError):
        crossing.memory.read16(MMIO_BASE + TRNG_LIMIT - 1)
    assert crossing.memory.read8(MMIO_BASE + TRNG_RAND8) == (
        untouched.memory.read8(MMIO_BASE + TRNG_RAND8)
    )


def test_guest_seed_mixes_little_endian_unread_bytes_but_cannot_recover() -> None:
    seed = b"guest-mix"
    baseline = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    mixed = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_seed=seed)
    )
    supplemental = 0x0123_4567_89AB_CDEF
    _execute(mixed, "SEED-RNG", supplemental)

    plain = bytes(
        baseline.memory.read8(MMIO_BASE + TRNG_RAND8) for _ in range(8)
    )
    changed = bytes(
        mixed.memory.read8(MMIO_BASE + TRNG_RAND8) for _ in range(8)
    )
    assert changed == bytes(
        value ^ seed_byte
        for value, seed_byte in zip(
            plain,
            supplemental.to_bytes(8, "little"),
        )
    )

    mixed.entropy.latch_unusable()
    assert mixed.memory.read8(MMIO_BASE + TRNG_STATUS) == 0
    _execute(mixed, "SEED-RNG", supplemental)
    assert mixed.memory.read8(MMIO_BASE + TRNG_STATUS) == 0
    assert mixed.entropy.zeroized_state == (True, True)


def test_unusable_entropy_data_faults_while_window_and_status_remain_decoded() -> None:
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(entropy_usable=False)
    )

    assert runtime.memory.read8(MMIO_BASE + TRNG_STATUS) == 0
    assert runtime.memory.read8(MMIO_BASE + TRNG_STATUS + 1) == 0
    runtime.memory.write8(MMIO_BASE + TRNG_SEED, 0xA5)
    assert runtime.entropy.zeroized_state == (True, True)
    with pytest.raises(MMIOAccessError) as caught:
        runtime.memory.read8(MMIO_BASE + TRNG_RAND8)
    assert isinstance(caught.value.__cause__, TRNGUnavailableError)
    with pytest.raises(MMIOAccessError):
        _execute(runtime, "RANDOM8")

    runtime.entropy.inject_seed(b"recovered host input")
    assert runtime.memory.read8(MMIO_BASE + TRNG_STATUS) == 1
    assert 0 <= _execute(runtime, "RANDOM8")[0] <= 0xFF


def test_unchanged_random_helpers_mask_and_bound_the_deterministic_stream() -> None:
    seed = b"mask comparison"
    direct32 = _load_sha3(
        MegaForthRuntime(
            memory=create_one_core_address_space(entropy_seed=seed)
        )
    )
    wrapped32 = _load_sha3(
        MegaForthRuntime(
            memory=create_one_core_address_space(entropy_seed=seed)
        )
    )
    assert _execute(wrapped32, "RANDOM32") == (
        _execute(direct32, "RANDOM")[0] & 0xFFFF_FFFF,
    )

    direct16 = _load_sha3(
        MegaForthRuntime(
            memory=create_one_core_address_space(entropy_seed=seed)
        )
    )
    wrapped16 = _load_sha3(
        MegaForthRuntime(
            memory=create_one_core_address_space(entropy_seed=seed)
        )
    )
    assert _execute(wrapped16, "RANDOM16") == (
        _execute(direct16, "RANDOM")[0] & 0xFFFF,
    )

    for _ in range(32):
        value = _execute(wrapped16, "RAND-RANGE", 17)[0]
        assert 0 <= value < 17


def test_kdos_hex_printer_and_known_source_limitations_remain_visible(
    loaded_sha3: MegaForthRuntime,
) -> None:
    loaded_sha3.memory.write_bytes(SOURCE_ADDRESS, b"\x00\x1f\xa5\xff")
    assert _execute(loaded_sha3, ".SHA3", SOURCE_ADDRESS, 4) == ()
    assert loaded_sha3.drain_uart_output() == b"001FA5FF"

    source = _verified_slice()
    assert b": .SHA3  ( addr len -- )\n    0 DO" in source
    assert b": RAND-RANGE  ( max -- n )\n    RANDOM SWAP MOD ABS ;" in source
    with pytest.raises(ExecutionError, match="modulo trapped"):
        _execute(loaded_sha3, "RAND-RANGE", 0)


def test_sha3_status_diagnostic_uses_the_normal_uart_path(
    loaded_sha3: MegaForthRuntime,
) -> None:
    assert _execute(loaded_sha3, ".SHA3-STATUS") == ()
    assert loaded_sha3.drain_uart_output() == b" SHA3: idle\r\n"

    loaded_sha3.memory.write8(MMIO_BASE + SHA3_CONTROL, 0)
    loaded_sha3.memory.write8(MMIO_BASE + SHA3_COMMAND, 1)
    loaded_sha3.memory.write8(MMIO_BASE + SHA3_COMMAND, 3)
    assert _execute(loaded_sha3, ".SHA3-STATUS") == ()
    assert loaded_sha3.drain_uart_output() == b" SHA3: done\r\n"
    loaded_sha3.memory.write8(MMIO_BASE + SHA3_COMMAND, 7)


def test_direct_interference_is_visible_to_checked_owner_and_cleanup() -> None:
    runtime = MegaForthRuntime()
    assert _execute(runtime, "SHA3-BEGIN", 0) == (0,)
    runtime.memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
    assert _execute(runtime, "SHA3-UPDATE", 0, 0) == (0,)
    assert _execute(runtime, "SHA3-FINAL", OUTPUT_ADDRESS) == (
        CRYPTO_STATUS_STATE,
    )
    assert runtime.sha3.checked_owner is None
    assert runtime.sha3.private_zeroized()


def test_direct_invalid_command_error_and_clear_are_terminal_and_exact() -> None:
    runtime = MegaForthRuntime()
    runtime.memory.write8(MMIO_BASE + SHA3_COMMAND, 0xFF)
    assert runtime.memory.read8(MMIO_BASE + SHA3_STATUS) == 0x03
    assert runtime.memory.read8(MMIO_BASE + SHA3_ERROR) == 1
    runtime.memory.write8(MMIO_BASE + SHA3_COMMAND, 7)
    assert runtime.memory.read8(MMIO_BASE + SHA3_STATUS) == 0
    assert runtime.memory.read8(MMIO_BASE + SHA3_ERROR) == 0
