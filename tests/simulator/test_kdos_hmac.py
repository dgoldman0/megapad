"""Contiguous unchanged-source acceptance for KDOS HMAC-SHA3-256."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError
from simulator.memory import EXTERNAL_BASE, MMIO_BASE
from simulator.memory import UnmappedAddressError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.sha3 import (
    CRYPTO_STATUS_HARDWARE,
    CRYPTO_STATUS_OK,
    CRYPTO_STATUS_PROTECTED,
    CRYPTO_STATUS_RANGE,
    CRYPTO_STATUS_STATE,
    CRYPTO_STATUS_UNSUPPORTED,
)
from simulator.spinlocks import (
    HostedSpinlockBank,
    SPINLOCK_ACQUIRED,
    SPINLOCK_BUSY,
    SPINLOCK_COUNT,
)
from tests.simulator.test_kdos_aes import (
    DESTINATION_ADDRESS as AES_DESTINATION_ADDRESS,
    IV_ADDRESS as AES_IV_ADDRESS,
    KDOS_GIT_BLOB,
    KEY_ADDRESS as AES_KEY_ADDRESS,
    MEGAPAD_REVISION,
    ONE_BLOCK,
    ONE_BLOCK_CIPHERTEXT,
    ONE_BLOCK_TAG,
    ROUNDTRIP_ADDRESS as AES_ROUNDTRIP_ADDRESS,
    SOURCE_ADDRESS as AES_SOURCE_ADDRESS,
    TAG_ADDRESS as AES_TAG_ADDRESS,
    _git_blob_id,
    _install_guest_material,
)
from tests.simulator.test_kdos_sha2 import _load_sha2
from tests.simulator.test_kdos_sha3 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-hmac-1270-1431.f"

FIRST_LINE = 1270
LAST_LINE = 1431
SLICE_SHA256 = "d8f8acbfba910c3b7ebe5c55d79612a9bc18cb15c3a1ec00d1a3bb5a7e4a1449"
SLICE_GIT_BLOB = "00f5a0c08e9c25c430da11e6f79f136994f390d8"
DEFINITIONS = (
    b"HMAC-HKDF-LOCK",
    b"_HMAC-HKDF-TRY",
    b"_HMAC-HKDF-RELEASE",
    b"_HMAC-HKDF-DROP-ARGS",
    b"_HMAC-HKDF-GUARD",
    b"HMAC-BLKSZ",
    b"HMAC-IPAD",
    b"HMAC-OPAD",
    b"HMAC-INNER",
    b"HMAC-KEY",
    b"_HMAC-PAD-PTR",
    b"_HMAC-XBYTE",
    b"_HMAC-OUT",
    b"_HMAC-KEY-PTR",
    b"_HMAC-KEY-LEN",
    b"_HMAC-MSG-PTR",
    b"_HMAC-MSG-LEN",
    b"_VERIFY-ACC",
    b"_HMAC-WIPE",
    b"HMAC-PAD",
    b"_HMAC-BEGIN-NOLOCK",
    b"_HMAC-FINISH-NOLOCK",
    b"_HMAC-NOLOCK",
    b"HMAC",
    b"ENCRYPT",
    b"DECRYPT",
    b"VERIFY",
)
BIOS_WORDS = ("SPIN@", "SPIN!")

KEY_ADDRESS = 0x2A_000
MESSAGE_ADDRESS = 0x2B_000
OUTPUT_ADDRESS = 0x2C_000
SECOND_ADDRESS = 0x2D_000

HMAC_KEY_ABC = bytes.fromhex(
    "09b6dbab8d11795ca7c8d82f1cf916820"
    "13c7cb980abbb25473be4ae7f7b5683"
)
HMAC_EMPTY = bytes.fromhex(
    "e841c164e5b4f10c9f3985587962af72"
    "fd607a951196fc92fb3a5251941784ea"
)
HMAC_EXACT_BLOCK_KEY = bytes.fromhex(
    "9d7b3c586ae9795d6d363907b9538f34"
    "f7917d2cdaed78a34761d934dac800cf"
)
HMAC_LONG_KEY = bytes.fromhex(
    "04a97cb33bde0ee866b3a2f4d59737ac"
    "a766e9f73ca3e1f052b570ebc870fe3b"
)

# These four byte arrays and seven cells are the complete HMAC-owned scratch
# named by this slice: 136 + 136 + 32 + 32 + 7*8 = 392 bytes.
HMAC_SCRATCH = (
    ("HMAC-IPAD", 136),
    ("HMAC-OPAD", 136),
    ("HMAC-INNER", 32),
    ("HMAC-KEY", 32),
    ("_HMAC-PAD-PTR", 8),
    ("_HMAC-XBYTE", 8),
    ("_HMAC-OUT", 8),
    ("_HMAC-KEY-PTR", 8),
    ("_HMAC-KEY-LEN", 8),
    ("_HMAC-MSG-PTR", 8),
    ("_HMAC-MSG-LEN", 8),
)


class _InjectOperationFailureAfterBegin:
    """Arm the real shared SHA device only after checked INIT resets it."""

    def __init__(self, service) -> None:
        self._service = service
        self._armed = False

    def begin(self, *args, **kwargs) -> int:
        status = self._service.begin(*args, **kwargs)
        if status == CRYPTO_STATUS_OK and not self._armed:
            self._service.inject_operation_failure_once()
            self._armed = True
        return status

    def __getattr__(self, name):
        return getattr(self._service, name)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_hmac(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}"
        ),
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_hmac(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_hmac(_load_sha2(runtime))


@pytest.fixture
def loaded_hmac() -> MegaForthRuntime:
    return _load_hmac()


def _scratch_spans(runtime: MegaForthRuntime) -> tuple[tuple[int, int], ...]:
    spans: list[tuple[int, int]] = []
    for name, length in HMAC_SCRATCH:
        word = runtime.find(name)
        assert word is not None
        spans.append((word.body_address, length))
    assert sum(length for _address, length in spans) == 392
    return tuple(spans)


def _seed_hmac_scratch(runtime: MegaForthRuntime) -> None:
    for index, (address, length) in enumerate(_scratch_spans(runtime), start=1):
        runtime.memory.fill(address, length, index)


def _assert_hmac_scratch_zero(runtime: MegaForthRuntime) -> None:
    for address, length in _scratch_spans(runtime):
        assert runtime.memory.read_bytes(address, length) == bytes(length)


def test_hmac_slice_is_exact_and_publishes_complete_ledger(
    loaded_hmac: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_hmac.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_hmac.find(name) is not None

    assert _execute(loaded_hmac, "HMAC-HKDF-LOCK") == (9,)
    assert _execute(loaded_hmac, "HMAC-BLKSZ") == (136,)
    assert loaded_hmac.spinlocks.lock_count == SPINLOCK_COUNT
    assert loaded_hmac.spinlocks.owners == (None,) * SPINLOCK_COUNT

    sized_bodies = (
        ("HMAC-IPAD", "HMAC-OPAD", 136),
        ("HMAC-OPAD", "HMAC-INNER", 136),
        ("HMAC-INNER", "HMAC-KEY", 32),
        ("HMAC-KEY", "_HMAC-PAD-PTR", 32),
        ("_HMAC-PAD-PTR", "_HMAC-XBYTE", 8),
        ("_HMAC-XBYTE", "_HMAC-OUT", 8),
        ("_HMAC-OUT", "_HMAC-KEY-PTR", 8),
        ("_HMAC-KEY-PTR", "_HMAC-KEY-LEN", 8),
        ("_HMAC-KEY-LEN", "_HMAC-MSG-PTR", 8),
        ("_HMAC-MSG-PTR", "_HMAC-MSG-LEN", 8),
        ("_HMAC-MSG-LEN", "_VERIFY-ACC", 8),
        ("_VERIFY-ACC", "_HMAC-WIPE", 8),
    )
    for name, following, size in sized_bodies:
        word = loaded_hmac.find(name)
        next_word = loaded_hmac.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == size
    _assert_hmac_scratch_zero(loaded_hmac)
    assert loaded_hmac.uart_output == b""


def test_hosted_spinlock_bank_is_owner_tracked_reentrant_and_depthless() -> None:
    bank = HostedSpinlockBank(core_count=2)
    assert bank.core_count == 2
    assert bank.lock_count == SPINLOCK_COUNT
    assert bank.owner(4) is None

    assert bank.acquire(4, 0) == SPINLOCK_ACQUIRED
    assert bank.owner(4) == 0
    assert bank.acquire(4, 0) == SPINLOCK_ACQUIRED
    assert bank.acquire(4, 1) == SPINLOCK_BUSY

    bank.release(4, 1)
    assert bank.owner(4) == 0
    bank.release(4, 0)
    assert bank.owner(4) is None

    # Reacquisition does not add depth: one owner release makes it free.
    assert bank.acquire(4, 1) == SPINLOCK_ACQUIRED
    assert bank.acquire(4, 1) == SPINLOCK_ACQUIRED
    bank.release(4, 1)
    assert bank.owner(4) is None
    assert bank.acquire(4, 0) == SPINLOCK_ACQUIRED
    bank.reset()
    assert bank.owners == (None,) * SPINLOCK_COUNT


def test_spin_words_preserve_peer_busy_wrong_owner_and_depthless_rules() -> None:
    runtime = MegaForthRuntime()
    runtime.spinlocks = HostedSpinlockBank(core_count=2)

    assert _execute(runtime, "SPIN@", 7) == (SPINLOCK_ACQUIRED,)
    assert _execute(runtime, "SPIN@", 7) == (SPINLOCK_ACQUIRED,)
    assert runtime.spinlocks.owner(7) == 0
    assert _execute(runtime, "SPIN!", 7) == ()
    assert runtime.spinlocks.owner(7) is None

    assert runtime.spinlocks.acquire(7, 1) == SPINLOCK_ACQUIRED
    assert _execute(runtime, "SPIN@", 7) == (SPINLOCK_BUSY,)
    assert _execute(runtime, "SPIN!", 7) == ()
    assert runtime.spinlocks.owner(7) == 1
    runtime.spinlocks.release(7, 1)
    assert runtime.spinlocks.owner(7) is None


@pytest.mark.parametrize("word", ("SPIN@", "SPIN!"))
def test_spin_words_reject_ids_outside_the_admitted_bank(word: str) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(SPINLOCK_COUNT)

    with pytest.raises(ExecutionError):
        runtime.execute(word, context=context)

    assert context.data.snapshot() == ()
    assert runtime.spinlocks.owners == (None,) * SPINLOCK_COUNT


@pytest.mark.parametrize(
    ("key", "message", "expected"),
    (
        (b"key", b"abc", HMAC_KEY_ABC),
        (b"", b"", HMAC_EMPTY),
        (bytes(range(136)), b"abc", HMAC_EXACT_BLOCK_KEY),
        (bytes(range(137)), b"abc", HMAC_LONG_KEY),
    ),
)
def test_hmac_matches_hard_coded_vectors_and_normalizes_only_long_keys(
    loaded_hmac: MegaForthRuntime,
    key: bytes,
    message: bytes,
    expected: bytes,
) -> None:
    loaded_hmac.memory.write_bytes(KEY_ADDRESS, key)
    loaded_hmac.memory.write_bytes(MESSAGE_ADDRESS, message)
    loaded_hmac.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)

    assert _execute(
        loaded_hmac,
        "HMAC",
        KEY_ADDRESS,
        len(key),
        MESSAGE_ADDRESS,
        len(message),
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hmac.memory.read_bytes(OUTPUT_ADDRESS, 32) == expected
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner is None
    assert loaded_hmac.sha3.private_zeroized()
    _assert_hmac_scratch_zero(loaded_hmac)


def test_hmac_empty_key_and_message_ignore_their_unused_addresses(
    loaded_hmac: MegaForthRuntime,
) -> None:
    loaded_hmac.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)

    assert _execute(
        loaded_hmac,
        "HMAC",
        MASK64,
        0,
        MMIO_BASE,
        0,
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hmac.memory.read_bytes(OUTPUT_ADDRESS, 32) == HMAC_EMPTY
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner is None
    _assert_hmac_scratch_zero(loaded_hmac)


@pytest.mark.parametrize(
    ("key_address", "key_length", "message_address", "message_length", "status"),
    (
        (MMIO_BASE, 1, MESSAGE_ADDRESS, 3, CRYPTO_STATUS_RANGE),
        (8, 1, MESSAGE_ADDRESS, 3, CRYPTO_STATUS_PROTECTED),
        (KEY_ADDRESS, 3, MMIO_BASE, 1, CRYPTO_STATUS_RANGE),
    ),
)
def test_hmac_span_failures_preserve_output_and_wipe_acquired_scratch(
    loaded_hmac: MegaForthRuntime,
    key_address: int,
    key_length: int,
    message_address: int,
    message_length: int,
    status: int,
) -> None:
    loaded_hmac.memory.write_bytes(KEY_ADDRESS, b"key")
    loaded_hmac.memory.write_bytes(MESSAGE_ADDRESS, b"abc")
    loaded_hmac.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    _seed_hmac_scratch(loaded_hmac)

    assert _execute(
        loaded_hmac,
        "HMAC",
        key_address,
        key_length,
        message_address,
        message_length,
        OUTPUT_ADDRESS,
    ) == (status,)
    assert loaded_hmac.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        [0xA5] * 32
    )
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner is None
    assert loaded_hmac.sha3.private_zeroized()
    _assert_hmac_scratch_zero(loaded_hmac)


def test_hmac_final_preflights_complete_output_before_publication() -> None:
    memory = create_one_core_address_space(external_size=31)
    runtime = _load_hmac(MegaForthRuntime(memory=memory))
    runtime.memory.write_bytes(KEY_ADDRESS, b"key")
    runtime.memory.write_bytes(MESSAGE_ADDRESS, b"abc")
    runtime.memory.fill(EXTERNAL_BASE, 31, 0xA5)

    assert _execute(
        runtime,
        "HMAC",
        KEY_ADDRESS,
        3,
        MESSAGE_ADDRESS,
        3,
        EXTERNAL_BASE,
    ) == (CRYPTO_STATUS_RANGE,)
    assert runtime.memory.read_bytes(EXTERNAL_BASE, 31) == bytes([0xA5] * 31)
    assert runtime.spinlocks.owner(9) is None
    assert runtime.sha3.checked_owner is None
    assert runtime.sha3.private_zeroized()
    _assert_hmac_scratch_zero(runtime)


def test_hmac_operation_failure_preserves_output_releases_and_wipes(
    loaded_hmac: MegaForthRuntime,
) -> None:
    loaded_hmac.memory.write_bytes(KEY_ADDRESS, b"key")
    loaded_hmac.memory.write_bytes(MESSAGE_ADDRESS, b"abc")
    loaded_hmac.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    _seed_hmac_scratch(loaded_hmac)
    loaded_hmac.sha3 = _InjectOperationFailureAfterBegin(loaded_hmac.sha3)

    assert _execute(
        loaded_hmac,
        "HMAC",
        KEY_ADDRESS,
        3,
        MESSAGE_ADDRESS,
        3,
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_HARDWARE,)
    assert loaded_hmac.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        [0xA5] * 32
    )
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner is None
    assert loaded_hmac.sha3.private_zeroized()
    _assert_hmac_scratch_zero(loaded_hmac)


def test_hmac_priority_is_capability_then_lock_then_argument_range() -> None:
    unavailable = _load_hmac(
        MegaForthRuntime(
            memory=create_one_core_address_space(crypto_capabilities=0)
        )
    )
    unavailable.spinlocks = HostedSpinlockBank(core_count=2)
    assert unavailable.spinlocks.acquire(9, 1) == SPINLOCK_ACQUIRED
    assert _execute(
        unavailable,
        "HMAC",
        MMIO_BASE,
        1,
        MMIO_BASE,
        1,
        MMIO_BASE,
    ) == (CRYPTO_STATUS_UNSUPPORTED,)
    assert unavailable.spinlocks.owner(9) == 1

    contended = _load_hmac()
    contended.spinlocks = HostedSpinlockBank(core_count=2)
    assert contended.spinlocks.acquire(9, 1) == SPINLOCK_ACQUIRED
    assert _execute(
        contended,
        "HMAC",
        MMIO_BASE,
        1,
        MMIO_BASE,
        1,
        MMIO_BASE,
    ) == (CRYPTO_STATUS_STATE,)
    assert contended.spinlocks.owner(9) == 1

    admitted = _load_hmac()
    admitted.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    _seed_hmac_scratch(admitted)
    assert _execute(
        admitted,
        "HMAC",
        MMIO_BASE,
        1,
        MMIO_BASE,
        1,
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_RANGE,)
    assert admitted.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        [0xA5] * 32
    )
    assert admitted.spinlocks.owner(9) is None
    _assert_hmac_scratch_zero(admitted)


def test_hmac_guard_rethrows_after_clear_wipe_and_release(
    loaded_hmac: MegaForthRuntime,
) -> None:
    loaded_hmac.evaluate(
        b": _TEST-HMAC-THROW-WORK 2DROP 2DROP DROP -77 THROW ;\n"
        b": _TEST-HMAC-GUARD-THROW "
        b"11 22 33 44 55 ['] _TEST-HMAC-THROW-WORK "
        b"['] _HMAC-WIPE ['] SHA3-CLEAR _HMAC-HKDF-GUARD ;\n"
    )
    wrapper = loaded_hmac.find("_TEST-HMAC-GUARD-THROW")
    assert wrapper is not None

    _seed_hmac_scratch(loaded_hmac)
    assert _execute(loaded_hmac, "SHA3-BEGIN", 0) == (CRYPTO_STATUS_OK,)
    assert loaded_hmac.spinlocks.acquire(9, 0) == SPINLOCK_ACQUIRED

    assert _execute(loaded_hmac, "CATCH", wrapper.xt) == (MASK64 - 76,)
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner is None
    assert loaded_hmac.sha3.private_zeroized()
    _assert_hmac_scratch_zero(loaded_hmac)


def test_hmac_guard_failed_clear_overrides_throw_and_retains_lock(
    loaded_hmac: MegaForthRuntime,
) -> None:
    loaded_hmac.evaluate(
        b": _TEST-HMAC-THROW-WORK 2DROP 2DROP DROP -77 THROW ;\n"
        b": _TEST-HMAC-GUARD-THROW "
        b"11 22 33 44 55 ['] _TEST-HMAC-THROW-WORK "
        b"['] _HMAC-WIPE ['] SHA3-CLEAR _HMAC-HKDF-GUARD ;\n"
    )
    wrapper = loaded_hmac.find("_TEST-HMAC-GUARD-THROW")
    assert wrapper is not None

    _seed_hmac_scratch(loaded_hmac)
    assert _execute(loaded_hmac, "SHA3-BEGIN", 0) == (CRYPTO_STATUS_OK,)
    assert loaded_hmac.spinlocks.acquire(9, 0) == SPINLOCK_ACQUIRED
    loaded_hmac.sha3.inject_clear_failure_once()

    assert _execute(loaded_hmac, "CATCH", wrapper.xt) == (
        CRYPTO_STATUS_HARDWARE,
    )
    assert loaded_hmac.spinlocks.owner(9) == 0
    assert loaded_hmac.sha3.checked_owner == (0, 0)
    _assert_hmac_scratch_zero(loaded_hmac)

    # Restore this focused fixture after observing the fail-closed state.
    assert _execute(loaded_hmac, "SHA3-CLEAR") == (CRYPTO_STATUS_OK,)
    assert _execute(loaded_hmac, "SPIN!", 9) == ()
    assert loaded_hmac.spinlocks.owner(9) is None


def test_same_core_hmac_reentry_is_not_mutual_exclusion(
    loaded_hmac: MegaForthRuntime,
) -> None:
    loaded_hmac.memory.write_bytes(KEY_ADDRESS, b"key")
    loaded_hmac.memory.write_bytes(MESSAGE_ADDRESS, b"abc")
    loaded_hmac.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)

    assert _execute(loaded_hmac, "_HMAC-HKDF-TRY") == (
        SPINLOCK_ACQUIRED,
    )
    assert _execute(
        loaded_hmac,
        "_HMAC-BEGIN-NOLOCK",
        KEY_ADDRESS,
        3,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hmac.spinlocks.owner(9) == 0
    assert loaded_hmac.sha3.checked_owner == (0, 0)

    # Hardware SPIN@ is owner-reentrant and depthless, so a second same-core
    # entry reaches SHA3-BEGIN instead of observing the shared lock as busy.
    assert _execute(
        loaded_hmac,
        "HMAC",
        KEY_ADDRESS,
        3,
        MESSAGE_ADDRESS,
        3,
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_STATE,)
    assert loaded_hmac.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        [0xA5] * 32
    )
    assert loaded_hmac.spinlocks.owner(9) is None
    assert loaded_hmac.sha3.checked_owner == (0, 0)
    _assert_hmac_scratch_zero(loaded_hmac)

    assert _execute(loaded_hmac, "SHA3-CLEAR") == (CRYPTO_STATUS_OK,)


def test_encrypt_and_decrypt_aliases_reach_the_existing_aes_words(
    loaded_hmac: MegaForthRuntime,
) -> None:
    _install_guest_material(loaded_hmac)
    loaded_hmac.memory.write_bytes(AES_SOURCE_ADDRESS, ONE_BLOCK)
    loaded_hmac.memory.fill(AES_DESTINATION_ADDRESS, len(ONE_BLOCK), 0xA5)

    tag_address = _execute(
        loaded_hmac,
        "ENCRYPT",
        AES_KEY_ADDRESS,
        AES_IV_ADDRESS,
        AES_SOURCE_ADDRESS,
        AES_DESTINATION_ADDRESS,
        len(ONE_BLOCK),
    )[0]
    assert loaded_hmac.memory.read_bytes(
        AES_DESTINATION_ADDRESS,
        len(ONE_BLOCK),
    ) == ONE_BLOCK_CIPHERTEXT
    assert loaded_hmac.memory.read_bytes(tag_address, 16) == ONE_BLOCK_TAG

    loaded_hmac.memory.write_bytes(AES_TAG_ADDRESS, ONE_BLOCK_TAG)
    loaded_hmac.memory.fill(AES_ROUNDTRIP_ADDRESS, len(ONE_BLOCK), 0x5A)
    assert _execute(
        loaded_hmac,
        "DECRYPT",
        AES_KEY_ADDRESS,
        AES_IV_ADDRESS,
        AES_DESTINATION_ADDRESS,
        AES_ROUNDTRIP_ADDRESS,
        len(ONE_BLOCK),
        AES_TAG_ADDRESS,
    ) == (0,)
    assert loaded_hmac.memory.read_bytes(
        AES_ROUNDTRIP_ADDRESS,
        len(ONE_BLOCK),
    ) == ONE_BLOCK


def test_verify_reports_equal_different_and_empty_inputs(
    loaded_hmac: MegaForthRuntime,
) -> None:
    value = bytes((index * 29 + 7) & 0xFF for index in range(73))
    loaded_hmac.memory.write_bytes(MESSAGE_ADDRESS, value)
    loaded_hmac.memory.write_bytes(SECOND_ADDRESS, value)

    assert _execute(
        loaded_hmac,
        "VERIFY",
        MESSAGE_ADDRESS,
        SECOND_ADDRESS,
        len(value),
    ) == (0,)

    changed = bytearray(value)
    changed[36] ^= 0x80
    loaded_hmac.memory.write_bytes(SECOND_ADDRESS, changed)
    assert _execute(
        loaded_hmac,
        "VERIFY",
        MESSAGE_ADDRESS,
        SECOND_ADDRESS,
        len(value),
    ) == (MASK64,)

    # Unchanged VERIFY uses 0 DO rather than ?DO. Equal zero bounds enter the
    # body, so zero is not an empty comparison and the bad address faults.
    with pytest.raises(UnmappedAddressError):
        _execute(loaded_hmac, "VERIFY", MASK64, MMIO_BASE, 0)
