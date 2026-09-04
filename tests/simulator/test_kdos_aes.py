"""Contiguous unchanged-source acceptance for KDOS AES-256-GCM."""

from __future__ import annotations

import hashlib
from itertools import chain
from pathlib import Path

import pytest

from shared.aes import AESBlockCipher, ghash_multiply, increment_gcm_counter
from shared.cells import MASK64
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
)
from simulator.errors import ForthAbort
from simulator.memory import (
    MMIO_BASE,
    AddressClass,
    MMIOAccessError,
    UnmappedAddressError,
)
from simulator.platform import OneCorePlatformMMIO, create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_diagnostics import _load_diagnostics


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-aes-903-1071.f"

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"
FIRST_LINE = 903
LAST_LINE = 1071
SLICE_SHA256 = "e60ede1c4ad4370dd50f79a763a8835e6421700e4c0e8ea1b04b6331b13179cf"
SLICE_GIT_BLOB = "568faa0aa53cd94a88151474d509b34c1a7eec97"
DEFINITIONS = (
    b"AES-BLK-IN",
    b"AES-BLK-OUT",
    b"AES-TAG-BUF",
    b"AES-ENCRYPT-BLK",
    b"AES-ENCRYPT",
    b"AES-DECRYPT",
    b".AES-STATUS",
    b"AES-AAD-PAD",
    b"AES-PARTIAL-PAD",
    b"_AEAD-AAD",
    b"_AEAD-AADLEN",
    b"_AEAD-REM",
    b"AES-ENCRYPT-AEAD",
    b"AES-DECRYPT-AEAD",
)
BIOS_WORDS = (
    "AES-KEY!",
    "AES-IV!",
    "AES-AAD-LEN!",
    "AES-DATA-LEN!",
    "AES-CMD!",
    "AES-STATUS@",
    "AES-KEY-MODE!",
    "AES-DIN!",
    "AES-DOUT@",
    "AES-TAG@",
    "AES-TAG!",
)

KEY = bytes(range(32))
IV = bytes(range(12))
ONE_BLOCK = b"A" * 16
ONE_BLOCK_CIPHERTEXT = bytes.fromhex("0643975a84a4835acc00d6caf0a8392c")
ONE_BLOCK_TAG = bytes.fromhex("0ff145f3786b8fc48a8aeafc45524d80")
TWO_BLOCKS = b"A" * 16 + b"B" * 16
TWO_BLOCK_CIPHERTEXT = bytes.fromhex(
    "0643975a84a4835acc00d6caf0a8392c"
    "c194c576b2391d3e7a25a7c75f2b42f0"
)
TWO_BLOCK_TAG = bytes.fromhex("61f3ad860a90ca7ede2074f793b887c1")

KEY_ADDRESS = 0x20_000
IV_ADDRESS = 0x20_100
SOURCE_ADDRESS = 0x20_200
DESTINATION_ADDRESS = 0x20_400
ROUNDTRIP_ADDRESS = 0x20_600
TAG_ADDRESS = 0x20_800
AAD_ADDRESS = 0x20_900


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_aes(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_aes(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_aes(_load_diagnostics(runtime))


@pytest.fixture
def loaded_aes() -> MegaForthRuntime:
    return _load_aes()


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name)
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


def _configure_direct(
    runtime: MegaForthRuntime,
    *,
    key: bytes = KEY,
    iv: bytes = IV,
    aad_length: int = 0,
    data_length: int = 16,
    command: int = 0,
    tag: bytes | None = None,
    key_mode: int = 0,
) -> None:
    runtime.memory.write8(MMIO_BASE + AES_KEY_MODE, key_mode)
    _write_mmio(runtime, AES_KEY, key)
    _write_mmio(runtime, AES_IV, iv)
    runtime.memory.write32(MMIO_BASE + AES_AAD_LENGTH, aad_length)
    runtime.memory.write32(MMIO_BASE + AES_DATA_LENGTH, data_length)
    if tag is not None:
        _write_mmio(runtime, AES_TAG, tag)
    runtime.memory.write8(MMIO_BASE + AES_COMMAND, command)


def _install_guest_material(runtime: MegaForthRuntime) -> None:
    runtime.memory.write_bytes(KEY_ADDRESS, KEY)
    runtime.memory.write_bytes(IV_ADDRESS, IV)


def test_aes_slice_is_exact_and_publishes_complete_ledger(
    loaded_aes: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_aes.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_aes.find(name) is not None

    sized_bodies = (
        ("AES-BLK-IN", "AES-BLK-OUT", 16),
        ("AES-BLK-OUT", "AES-TAG-BUF", 16),
        ("AES-TAG-BUF", "AES-ENCRYPT-BLK", 16),
        ("AES-AAD-PAD", "AES-PARTIAL-PAD", 16),
        ("AES-PARTIAL-PAD", "_AEAD-AAD", 16),
        ("_AEAD-AAD", "_AEAD-AADLEN", 8),
        ("_AEAD-AADLEN", "_AEAD-REM", 8),
        ("_AEAD-REM", "AES-ENCRYPT-AEAD", 8),
    )
    for name, following, size in sized_bodies:
        word = loaded_aes.find(name)
        next_word = loaded_aes.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == size
    for name in ("_AEAD-AAD", "_AEAD-AADLEN", "_AEAD-REM"):
        word = loaded_aes.find(name)
        assert word is not None
        assert loaded_aes.memory.read64(word.body_address) == 0
    assert loaded_aes.uart_output == b""


def test_shared_aes_and_ghash_match_independent_known_answers() -> None:
    plaintext = bytes.fromhex("00112233445566778899aabbccddeeff")
    assert AESBlockCipher(bytes(range(16))).encrypt(plaintext) == bytes.fromhex(
        "69c4e0d86a7b0430d8cdb78070b4c55a"
    )
    assert AESBlockCipher(bytes(range(32))).encrypt(plaintext) == bytes.fromhex(
        "8ea2b7ca516745bfeafc49904b496089"
    )
    assert ghash_multiply(
        int("0388dace60b6a392f328c2b971b2fe78", 16),
        int("66e94bd4ef8a2c3b884cfa59ca342b2e", 16),
    ) == int("5e2ec746917062882c85b0685353deb7", 16)
    assert increment_gcm_counter(bytes.fromhex("00" * 12 + "ffffffff")) == (
        bytes(16)
    )

    with pytest.raises(ValueError, match="16 or 32 bytes"):
        AESBlockCipher(bytes(24))
    with pytest.raises(TypeError, match="must be bytes"):
        AESBlockCipher(bytes(16)).encrypt(bytearray(16))  # type: ignore[arg-type]


def test_bios_and_direct_mmio_share_one_aes_transaction_both_directions() -> None:
    runtime = MegaForthRuntime()
    assert isinstance(runtime.memory.mmio, OneCorePlatformMMIO)
    assert runtime.memory.mmio.aes is runtime.aes

    _write_mmio(runtime, AES_KEY, KEY)
    runtime.memory.write_bytes(IV_ADDRESS, IV)
    _execute(runtime, "AES-IV!", IV_ADDRESS)
    _execute(runtime, "AES-AAD-LEN!", 0)
    runtime.memory.write32(MMIO_BASE + AES_DATA_LENGTH, 16)
    _execute(runtime, "AES-CMD!", 0)
    runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
    _execute(runtime, "AES-DIN!", SOURCE_ADDRESS)

    ciphertext = _read_mmio(runtime, AES_DATA_OUTPUT, 16)
    tag = _read_mmio(runtime, AES_TAG, 16)
    assert ciphertext == ONE_BLOCK_CIPHERTEXT
    assert tag == ONE_BLOCK_TAG
    assert _execute(runtime, "AES-STATUS@") == (AES_STATUS_DONE,)

    runtime.memory.write_bytes(TAG_ADDRESS, tag)
    _execute(runtime, "AES-TAG!", TAG_ADDRESS)
    runtime.memory.write_bytes(KEY_ADDRESS, KEY)
    _execute(runtime, "AES-KEY!", KEY_ADDRESS)
    _write_mmio(runtime, AES_IV, IV)
    runtime.memory.write32(MMIO_BASE + AES_AAD_LENGTH, 0)
    _execute(runtime, "AES-DATA-LEN!", 16)
    runtime.memory.write8(MMIO_BASE + AES_COMMAND, 1)
    _write_mmio(runtime, AES_DATA_INPUT, ciphertext)
    _execute(runtime, "AES-DOUT@", ROUNDTRIP_ADDRESS)

    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, 16) == ONE_BLOCK
    assert _execute(runtime, "AES-STATUS@") == (AES_STATUS_DONE,)


def test_aes_mmio_window_has_exact_native_scalar_shapes_and_directions() -> None:
    runtime = MegaForthRuntime()
    for width, read, write in (
        (1, runtime.memory.read8, runtime.memory.write8),
        (2, runtime.memory.read16, runtime.memory.write16),
        (4, runtime.memory.read32, runtime.memory.write32),
        (8, runtime.memory.read64, runtime.memory.write64),
    ):
        address = MMIO_BASE + AES_OFFSET
        write(address, MASK64)
        assert read(address) == 0
        assert address % width == 0

    before = runtime.aes.status
    runtime.memory.write8(MMIO_BASE + AES_STATUS, 0xFF)
    assert runtime.aes.status == before

    for address, read in (
        (MMIO_BASE + AES_OFFSET + 1, runtime.memory.read16),
        (MMIO_BASE + AES_LIMIT - 1, runtime.memory.read16),
    ):
        with pytest.raises(MMIOAccessError, match="preflight") as caught:
            read(address)
        assert isinstance(caught.value.__cause__, AESAccessError)


def test_aes128_mode_and_aes256_mode_match_external_gcm_vectors() -> None:
    empty = MegaForthRuntime()
    _configure_direct(
        empty,
        key=bytes(32),
        iv=bytes(12),
        data_length=0,
        key_mode=1,
    )
    assert empty.aes.status == AES_STATUS_DONE
    assert _read_mmio(empty, AES_TAG, 16) == bytes.fromhex(
        "58e2fccefa7e3061367f1d57a4e7455a"
    )

    aes128 = MegaForthRuntime()
    _configure_direct(
        aes128,
        key=bytes(32),
        iv=bytes(12),
        data_length=16,
        key_mode=1,
    )
    _write_mmio(aes128, AES_DATA_INPUT, bytes(16))
    assert _read_mmio(aes128, AES_DATA_OUTPUT, 16) == bytes.fromhex(
        "0388dace60b6a392f328c2b971b2fe78"
    )
    assert _read_mmio(aes128, AES_TAG, 16) == bytes.fromhex(
        "ab6e47d42cec13bdf53a67b21257bddf"
    )

    aes256 = MegaForthRuntime()
    _configure_direct(
        aes256,
        key=bytes(32),
        iv=bytes(12),
        data_length=16,
    )
    _write_mmio(aes256, AES_DATA_INPUT, bytes(16))
    assert _read_mmio(aes256, AES_DATA_OUTPUT, 16) == bytes.fromhex(
        "cea7403d4d606b6e074ec5d3baf39d18"
    )
    assert _read_mmio(aes256, AES_TAG, 16) == bytes.fromhex(
        "d0d1c8a799996bf0265b98b5d48ab919"
    )


def test_aes_state_machine_fails_closed_recovers_and_truncates_lengths() -> None:
    incomplete = MegaForthRuntime()
    _execute(incomplete, "AES-CMD!", 0)
    assert incomplete.aes.status == AES_STATUS_FAILED
    assert _read_mmio(incomplete, AES_DATA_OUTPUT, 16) == bytes(16)
    assert _read_mmio(incomplete, AES_TAG, 16) == bytes(16)

    _configure_direct(incomplete)
    _write_mmio(incomplete, AES_DATA_INPUT, ONE_BLOCK)
    assert incomplete.aes.status == AES_STATUS_DONE
    assert _read_mmio(incomplete, AES_DATA_OUTPUT, 16) == ONE_BLOCK_CIPHERTEXT

    _execute(incomplete, "AES-AAD-LEN!", (1 << 40) | 5)
    _execute(incomplete, "AES-DATA-LEN!", (1 << 48) | 32)
    assert incomplete.aes.aad_length == 5
    assert incomplete.aes.data_length == 32

    active = MegaForthRuntime()
    _configure_direct(active, data_length=32)
    assert active.aes.status == AES_STATUS_ACTIVE
    _write_mmio(active, AES_DATA_INPUT, ONE_BLOCK)
    assert active.aes.status == AES_STATUS_ACTIVE
    assert active.aes.data_processed == 16
    active.memory.write8(MMIO_BASE + AES_KEY, 0xAA)
    assert active.aes.status == AES_STATUS_FAILED
    active.memory.write8(MMIO_BASE + AES_KEY + 1, 0xBB)
    assert active.aes.status == AES_STATUS_IDLE
    _configure_direct(active, data_length=32)
    _write_mmio(active, AES_DATA_INPUT, ONE_BLOCK)
    assert active.aes.status == AES_STATUS_ACTIVE
    _write_mmio(active, AES_DATA_INPUT, ONE_BLOCK)
    assert active.aes.status == AES_STATUS_DONE

    _write_mmio(active, AES_DATA_INPUT, ONE_BLOCK)
    assert active.aes.status == AES_STATUS_FAILED
    assert _read_mmio(active, AES_DATA_OUTPUT, 16) == bytes(16)
    assert _read_mmio(active, AES_TAG, 16) == bytes(16)

    _configure_direct(active)
    _write_mmio(active, AES_DATA_INPUT, ONE_BLOCK)
    assert active.aes.status == AES_STATUS_DONE

    partial_key = MegaForthRuntime()
    _write_mmio(partial_key, AES_KEY, bytes(16))
    _write_mmio(partial_key, AES_IV, bytes(12))
    partial_key.memory.write32(MMIO_BASE + AES_AAD_LENGTH, 0)
    partial_key.memory.write32(MMIO_BASE + AES_DATA_LENGTH, 0)
    partial_key.memory.write8(MMIO_BASE + AES_COMMAND, 0)
    assert partial_key.aes.status == AES_STATUS_FAILED

    masked = MegaForthRuntime()
    masked.memory.write8(MMIO_BASE + AES_KEY_MODE, 3)
    assert masked.aes.key_mode == 1
    masked.memory.write8(MMIO_BASE + AES_KEY_MODE, 2)
    assert masked.aes.key_mode == 0
    _configure_direct(masked, command=2)
    _write_mmio(masked, AES_DATA_INPUT, ONE_BLOCK)
    assert _read_mmio(masked, AES_DATA_OUTPUT, 16) == ONE_BLOCK_CIPHERTEXT

    masked_decrypt = MegaForthRuntime()
    _configure_direct(
        masked_decrypt,
        command=3,
        tag=ONE_BLOCK_TAG,
    )
    _write_mmio(masked_decrypt, AES_DATA_INPUT, ONE_BLOCK_CIPHERTEXT)
    assert masked_decrypt.aes.status == AES_STATUS_DONE
    assert _read_mmio(masked_decrypt, AES_DATA_OUTPUT, 16) == ONE_BLOCK


def test_aes_service_is_shared_by_contexts_but_isolated_between_runtimes() -> None:
    first = MegaForthRuntime()
    second = MegaForthRuntime()
    scratch = first.new_context()

    scratch.data.push(7)
    first.execute("AES-AAD-LEN!", context=scratch)
    assert scratch.data.snapshot() == ()
    assert first.aes.aad_length == 7
    assert second.aes.aad_length == 0
    assert first.aes is not second.aes


def test_active_aes_survives_guest_unwind_and_completes_across_contexts() -> None:
    runtime = _load_aes()
    _configure_direct(runtime)
    runtime.evaluate(b": AES-GUEST-THROW -77 THROW ; : AES-GUEST-ABORT ABORT ;")

    throw_word = runtime.find("AES-GUEST-THROW")
    assert throw_word is not None
    assert _execute(runtime, "CATCH", throw_word.xt) == (MASK64 - 76,)
    assert runtime.aes.status == AES_STATUS_ACTIVE

    abort_context = runtime.new_context()
    with pytest.raises(ForthAbort):
        runtime.execute("AES-GUEST-ABORT", context=abort_context)
    assert runtime.aes.status == AES_STATUS_ACTIVE

    runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
    feed_context = runtime.new_context()
    feed_context.data.push(SOURCE_ADDRESS)
    runtime.execute("AES-DIN!", context=feed_context)
    assert feed_context.data.snapshot() == ()
    assert runtime.aes.status == AES_STATUS_DONE

    _execute(runtime, "AES-DOUT@", DESTINATION_ADDRESS)
    assert runtime.memory.read_bytes(DESTINATION_ADDRESS, 16) == (
        ONE_BLOCK_CIPHERTEXT
    )


def test_incremental_guest_faults_preserve_native_aes_transfer_prefixes() -> None:
    source_memory = create_one_core_address_space(external_size=31)
    source_runtime = MegaForthRuntime(memory=source_memory)
    external = next(
        region
        for region in source_memory.regions
        if region.kind is AddressClass.EXTERNAL
    )
    source_memory.write_bytes(external.base, KEY[:31])
    source_runtime.main_context.data.push(external.base)

    with pytest.raises(UnmappedAddressError):
        source_runtime.execute("AES-KEY!")

    assert source_runtime.main_context.data.snapshot() == ()
    assert source_runtime.main_context.returns.snapshot() == ()
    source_memory.write8(MMIO_BASE + AES_KEY + 31, KEY[31])
    _write_mmio(source_runtime, AES_IV, IV)
    source_memory.write32(MMIO_BASE + AES_AAD_LENGTH, 0)
    source_memory.write32(MMIO_BASE + AES_DATA_LENGTH, 16)
    source_memory.write8(MMIO_BASE + AES_COMMAND, 0)
    _write_mmio(source_runtime, AES_DATA_INPUT, ONE_BLOCK)
    assert _read_mmio(source_runtime, AES_DATA_OUTPUT, 16) == (
        ONE_BLOCK_CIPHERTEXT
    )

    destination_memory = create_one_core_address_space(external_size=15)
    destination_runtime = MegaForthRuntime(memory=destination_memory)
    destination = next(
        region
        for region in destination_memory.regions
        if region.kind is AddressClass.EXTERNAL
    )
    _configure_direct(destination_runtime)
    _write_mmio(destination_runtime, AES_DATA_INPUT, ONE_BLOCK)
    destination_runtime.main_context.data.push(destination.base)

    with pytest.raises(UnmappedAddressError):
        destination_runtime.execute("AES-DOUT@")

    assert destination_runtime.main_context.data.snapshot() == ()
    assert destination_memory.read_bytes(destination.base, 15) == (
        ONE_BLOCK_CIPHERTEXT[:15]
    )
    assert _read_mmio(destination_runtime, AES_DATA_OUTPUT, 16) == (
        ONE_BLOCK_CIPHERTEXT
    )


@pytest.mark.parametrize(
    ("plaintext", "ciphertext", "expected_tag"),
    (
        (ONE_BLOCK, ONE_BLOCK_CIPHERTEXT, ONE_BLOCK_TAG),
        (TWO_BLOCKS, TWO_BLOCK_CIPHERTEXT, TWO_BLOCK_TAG),
    ),
)
def test_real_aes_encrypt_decrypt_matches_external_vectors(
    loaded_aes: MegaForthRuntime,
    plaintext: bytes,
    ciphertext: bytes,
    expected_tag: bytes,
) -> None:
    runtime = loaded_aes
    _install_guest_material(runtime)
    runtime.memory.write_bytes(SOURCE_ADDRESS, plaintext)
    runtime.memory.write_bytes(DESTINATION_ADDRESS, bytes((0xA5,)) * len(plaintext))

    result = _execute(
        runtime,
        "AES-ENCRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        len(plaintext),
    )
    tag_word = runtime.find("AES-TAG-BUF")
    assert tag_word is not None
    assert result == (tag_word.body_address,)
    assert runtime.memory.read_bytes(DESTINATION_ADDRESS, len(plaintext)) == (
        ciphertext
    )
    assert runtime.memory.read_bytes(result[0], 16) == expected_tag

    runtime.memory.write_bytes(TAG_ADDRESS, expected_tag)
    runtime.memory.write_bytes(ROUNDTRIP_ADDRESS, bytes((0x5A,)) * len(plaintext))
    assert _execute(
        runtime,
        "AES-DECRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        DESTINATION_ADDRESS,
        ROUNDTRIP_ADDRESS,
        len(plaintext),
        TAG_ADDRESS,
    ) == (0,)
    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, len(plaintext)) == (
        plaintext
    )


def test_bad_tag_returns_true_and_preserves_native_streamed_prefix_effect() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    runtime.memory.write_bytes(SOURCE_ADDRESS, TWO_BLOCKS)
    tag_address = _execute(
        runtime,
        "AES-ENCRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        len(TWO_BLOCKS),
    )[0]
    bad_tag = bytearray(runtime.memory.read_bytes(tag_address, 16))
    bad_tag[0] ^= 0xFF
    runtime.memory.write_bytes(TAG_ADDRESS, bad_tag)
    runtime.memory.write_bytes(ROUNDTRIP_ADDRESS, bytes((0xCC,)) * 32)

    assert _execute(
        runtime,
        "AES-DECRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        DESTINATION_ADDRESS,
        ROUNDTRIP_ADDRESS,
        32,
        TAG_ADDRESS,
    ) == (MASK64,)
    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, 32) == (
        TWO_BLOCKS[:16] + bytes(16)
    )
    assert runtime.aes.status == AES_STATUS_FAILED


def test_real_encrypt_block_and_exact_in_place_roundtrip() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
    _execute(runtime, "AES-IV!", IV_ADDRESS)
    _execute(runtime, "AES-KEY!", KEY_ADDRESS)
    _execute(runtime, "AES-AAD-LEN!", 0)
    _execute(runtime, "AES-DATA-LEN!", 16)
    _execute(runtime, "AES-CMD!", 0)
    assert _execute(
        runtime,
        "AES-ENCRYPT-BLK",
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
    ) == ()
    assert runtime.memory.read_bytes(DESTINATION_ADDRESS, 16) == (
        ONE_BLOCK_CIPHERTEXT
    )

    runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
    tag_address = _execute(
        runtime,
        "AES-ENCRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        SOURCE_ADDRESS,
        16,
    )[0]
    assert runtime.memory.read_bytes(SOURCE_ADDRESS, 16) == ONE_BLOCK_CIPHERTEXT
    runtime.memory.write_bytes(
        TAG_ADDRESS,
        runtime.memory.read_bytes(tag_address, 16),
    )
    assert _execute(
        runtime,
        "AES-DECRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        SOURCE_ADDRESS,
        16,
        TAG_ADDRESS,
    ) == (0,)
    assert runtime.memory.read_bytes(SOURCE_ADDRESS, 16) == ONE_BLOCK


@pytest.mark.parametrize(
    ("state", "expected"),
    (
        ("idle", b" AES: idle\r\n"),
        ("active", b" AES: busy\r\n"),
        ("done", b" AES: done (OK)\r\n"),
        ("failed", b" AES: AUTH FAIL\r\n"),
    ),
)
def test_real_aes_status_renders_every_executable_state(
    state: str,
    expected: bytes,
) -> None:
    runtime = _load_aes()
    if state == "active":
        _configure_direct(runtime, data_length=32)
    elif state == "done":
        _configure_direct(runtime)
        _write_mmio(runtime, AES_DATA_INPUT, ONE_BLOCK)
    elif state == "failed":
        runtime.memory.write8(MMIO_BASE + AES_COMMAND, 0)

    assert _execute(runtime, ".AES-STATUS") == ()
    assert runtime.drain_uart_output() == expected


def test_real_aead_partial_known_answer_and_roundtrip_preserve_guard_bytes() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    aad = bytes((23, 3, 3, 0, 25))
    plaintext = b"B" * 25
    expected_ciphertext = bytes.fromhex(
        "0540945987a78059cf03d5c9f3ab3a2fc194c576b2391d3e7a"
    )
    expected_tag = bytes.fromhex("31a2ff057adffb689e9ab3dceabbfa4e")
    runtime.memory.write_bytes(AAD_ADDRESS, aad)
    runtime.memory.write_bytes(SOURCE_ADDRESS, plaintext)
    runtime.memory.write_bytes(DESTINATION_ADDRESS, bytes((0xA5,)) * 40)

    tag_address = _execute(
        runtime,
        "AES-ENCRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        len(aad),
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        len(plaintext),
    )[0]
    assert runtime.memory.read_bytes(DESTINATION_ADDRESS, 25) == (
        expected_ciphertext
    )
    assert runtime.memory.read_bytes(DESTINATION_ADDRESS + 25, 15) == bytes(
        (0xA5,)
    ) * 15
    assert runtime.memory.read_bytes(tag_address, 16) == expected_tag
    aad_pad = runtime.find("AES-AAD-PAD")
    partial_pad = runtime.find("AES-PARTIAL-PAD")
    assert aad_pad is not None
    assert partial_pad is not None
    assert runtime.memory.read_bytes(aad_pad.body_address, 16) == aad + bytes(11)
    assert runtime.memory.read_bytes(partial_pad.body_address, 16) == (
        expected_ciphertext[16:] + bytes(7)
    )
    assert runtime.memory.read64(
        runtime.find("_AEAD-AAD").body_address  # type: ignore[union-attr]
    ) == AAD_ADDRESS
    assert runtime.memory.read64(
        runtime.find("_AEAD-AADLEN").body_address  # type: ignore[union-attr]
    ) == len(aad)
    assert runtime.memory.read64(
        runtime.find("_AEAD-REM").body_address  # type: ignore[union-attr]
    ) == 9

    runtime.memory.write_bytes(TAG_ADDRESS, expected_tag)
    runtime.memory.write_bytes(ROUNDTRIP_ADDRESS, bytes((0x5A,)) * 40)
    assert _execute(
        runtime,
        "AES-DECRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        len(aad),
        DESTINATION_ADDRESS,
        ROUNDTRIP_ADDRESS,
        len(plaintext),
        TAG_ADDRESS,
    ) == (0,)
    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, 25) == plaintext
    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS + 25, 15) == bytes(
        (0x5A,)
    ) * 15


def test_real_aead_bad_tag_returns_true_and_zeros_only_final_partial() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    aad = bytes((23, 3, 3, 0, 25))
    plaintext = b"B" * 25
    runtime.memory.write_bytes(AAD_ADDRESS, aad)
    runtime.memory.write_bytes(SOURCE_ADDRESS, plaintext)
    tag_address = _execute(
        runtime,
        "AES-ENCRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        len(aad),
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        len(plaintext),
    )[0]
    bad_tag = bytearray(runtime.memory.read_bytes(tag_address, 16))
    bad_tag[-1] ^= 1
    runtime.memory.write_bytes(TAG_ADDRESS, bad_tag)
    runtime.memory.write_bytes(ROUNDTRIP_ADDRESS, bytes((0xA5,)) * 32)

    assert _execute(
        runtime,
        "AES-DECRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        len(aad),
        DESTINATION_ADDRESS,
        ROUNDTRIP_ADDRESS,
        len(plaintext),
        TAG_ADDRESS,
    ) == (MASK64,)
    assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, 32) == (
        plaintext[:16] + bytes(9) + bytes((0xA5,)) * 7
    )


def test_global_tag_buffer_is_reused_and_overwritten_by_later_encrypt() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
    first = _execute(
        runtime,
        "AES-ENCRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        16,
    )[0]
    first_tag = runtime.memory.read_bytes(first, 16)
    runtime.memory.write_bytes(SOURCE_ADDRESS, b"C" * 16)
    second = _execute(
        runtime,
        "AES-ENCRYPT",
        KEY_ADDRESS,
        IV_ADDRESS,
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        16,
    )[0]

    assert second == first
    assert runtime.memory.read_bytes(second, 16) != first_tag


def test_real_aead_covers_every_partial_data_tail_in_the_safe_aad_domain() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)
    aad = bytes((23, 3, 3, 0, 0))
    runtime.memory.write_bytes(AAD_ADDRESS, aad)

    for length in chain(range(1, 16), range(17, 32)):
        plaintext = bytes((index * 7 + length) & 0xFF for index in range(length))
        runtime.memory.write_bytes(SOURCE_ADDRESS, plaintext)
        runtime.memory.write_bytes(DESTINATION_ADDRESS, bytes((0xA5,)) * 40)
        tag_address = _execute(
            runtime,
            "AES-ENCRYPT-AEAD",
            KEY_ADDRESS,
            IV_ADDRESS,
            AAD_ADDRESS,
            len(aad),
            SOURCE_ADDRESS,
            DESTINATION_ADDRESS,
            length,
        )[0]
        runtime.memory.write_bytes(
            TAG_ADDRESS,
            runtime.memory.read_bytes(tag_address, 16),
        )
        runtime.memory.write_bytes(ROUNDTRIP_ADDRESS, bytes((0x5A,)) * 40)
        assert _execute(
            runtime,
            "AES-DECRYPT-AEAD",
            KEY_ADDRESS,
            IV_ADDRESS,
            AAD_ADDRESS,
            len(aad),
            DESTINATION_ADDRESS,
            ROUNDTRIP_ADDRESS,
            length,
            TAG_ADDRESS,
        ) == (0,)
        assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, length) == plaintext
        assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS + length, 1) == b"Z"


def test_real_aead_covers_one_full_aad_block_and_aad_only_message() -> None:
    runtime = _load_aes()
    _install_guest_material(runtime)

    for aad_length in (1, 15, 16):
        aad = bytes(range(aad_length))
        runtime.memory.write_bytes(AAD_ADDRESS, aad)
        runtime.memory.write_bytes(SOURCE_ADDRESS, ONE_BLOCK)
        tag_address = _execute(
            runtime,
            "AES-ENCRYPT-AEAD",
            KEY_ADDRESS,
            IV_ADDRESS,
            AAD_ADDRESS,
            aad_length,
            SOURCE_ADDRESS,
            DESTINATION_ADDRESS,
            16,
        )[0]
        runtime.memory.write_bytes(
            TAG_ADDRESS,
            runtime.memory.read_bytes(tag_address, 16),
        )
        assert _execute(
            runtime,
            "AES-DECRYPT-AEAD",
            KEY_ADDRESS,
            IV_ADDRESS,
            AAD_ADDRESS,
            aad_length,
            DESTINATION_ADDRESS,
            ROUNDTRIP_ADDRESS,
            16,
            TAG_ADDRESS,
        ) == (0,)
        assert runtime.memory.read_bytes(ROUNDTRIP_ADDRESS, 16) == ONE_BLOCK

    aad = bytes(range(16))
    runtime.memory.write_bytes(AAD_ADDRESS, aad)
    tag_address = _execute(
        runtime,
        "AES-ENCRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        16,
        SOURCE_ADDRESS,
        DESTINATION_ADDRESS,
        0,
    )[0]
    runtime.memory.write_bytes(
        TAG_ADDRESS,
        runtime.memory.read_bytes(tag_address, 16),
    )
    assert _execute(
        runtime,
        "AES-DECRYPT-AEAD",
        KEY_ADDRESS,
        IV_ADDRESS,
        AAD_ADDRESS,
        16,
        DESTINATION_ADDRESS,
        ROUNDTRIP_ADDRESS,
        0,
        TAG_ADDRESS,
    ) == (0,)


def test_fixture_keeps_known_length_domain_defects_visible() -> None:
    source = _verified_slice()
    assert source.count(b"AES-AAD-PAD AES-DIN!") == 2
    assert b"_AEAD-AADLEN @ 16 U>" not in source
    assert source.count(b"R> 0 DO") == 2
    assert source.count(b"R> 0 ?DO") == 2
