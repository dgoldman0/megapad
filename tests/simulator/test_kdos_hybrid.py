"""Contiguous unchanged-source acceptance for KDOS HKDF and hybrid PQ."""

from __future__ import annotations

import hashlib
import hmac
from pathlib import Path

import pytest

from shared.mlkem import mlkem512_decapsulate, mlkem512_encapsulate
from shared.x25519 import x25519_scalar_multiply
from simulator.memory import MASK64
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.sha2 import (
    SHA2_STATUS_OK,
    SHA2_STATUS_RANGE,
    SHA2_STATUS_STATE,
)
from simulator.sha3 import (
    CRYPTO_STATUS_OK,
    CRYPTO_STATUS_RANGE,
    CRYPTO_STATUS_STATE,
    CRYPTO_STATUS_UNSUPPORTED,
)
from simulator.spinlocks import HostedSpinlockBank, SPINLOCK_ACQUIRED
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_kem import (
    KEM_STATUS_DONE,
    _load_kem,
)
from tests.simulator.test_kdos_x25519 import (
    RFC_SCALAR_ONE,
    RFC_SCALAR_TWO,
    _execute,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / (
    "kdos-hybrid-1635-2043.f"
)

FIRST_LINE = 1635
LAST_LINE = 2043
SLICE_SHA256 = (
    "58576925d341dd4b03bbc5c37863ac6ec5ecbb76c6189e1b2ffd3f481124c38c"
)
SLICE_GIT_BLOB = "649a6800647257b7997715c8f2d4f5b988fd9833"
DEFINITIONS = (
    b"_PQ-SS-X",
    b"_PQ-SS-K",
    b"_PQ-CAT",
    b"_PQ-PRK",
    b"_PQ-COIN",
    b"_PQ-INFO",
    b"_PQ-INFO-INIT",
    b"HKDF-HASHLEN",
    b"_HKDF-ZERO-SALT",
    b"_HKDF-T",
    b"_HKDF-PRK-PTR",
    b"_HKDF-INFO-PTR",
    b"_HKDF-INFO-LEN",
    b"_HKDF-OUT-PTR",
    b"_HKDF-REMAIN",
    b"_HKDF-TPREV-LEN",
    b"_HKDF-COUNTER",
    b"_HKDF-WIPE",
    b"_HKDF-OUTPUT-ALIASES-INPUT?",
    b"_HKDF-EXTRACT-NOLOCK",
    b"HKDF-EXTRACT",
    b"_HKDF-EXPAND-NOLOCK",
    b"HKDF-EXPAND",
    b"HMAC256-BLKSZ",
    b"HMAC256-IPAD",
    b"HMAC256-OPAD",
    b"HMAC256-INNER",
    b"HMAC256-KEY",
    b"_HMAC256-PAD-PTR",
    b"_HMAC256-XBYTE",
    b"_HMAC256-OUT",
    b"_HMAC256-KEY-PTR",
    b"_HMAC256-KEY-LEN",
    b"_HMAC256-MSG-PTR",
    b"_HMAC256-MSG-LEN",
    b"_HMAC256-WIPE",
    b"HMAC256-PAD",
    b"_HMAC256-BEGIN-NOLOCK",
    b"_HMAC256-FINISH-NOLOCK",
    b"_HMAC256-NOLOCK",
    b"HMAC-SHA256",
    b"_HKDF256-ZERO-SALT",
    b"_HKDF256-T",
    b"_HKDF256-PRK-PTR",
    b"_HKDF256-INFO-PTR",
    b"_HKDF256-INFO-LEN",
    b"_HKDF256-OUT-PTR",
    b"_HKDF256-REMAIN",
    b"_HKDF256-TPREV-LEN",
    b"_HKDF256-COUNTER",
    b"_HKDF256-WIPE",
    b"_HKDF256-OUTPUT-ALIASES-INPUT?",
    b"_HKDF256-EXTRACT-NOLOCK",
    b"HKDF-SHA256-EXTRACT",
    b"_HKDF256-EXPAND-NOLOCK",
    b"HKDF-SHA256-EXPAND",
    b"PQ-DERIVE",
    b"PQ-EXCHANGE-INIT",
    b"PQ-EXCHANGE-RESP",
)

KEY_ADDRESS = 0x45_000
MESSAGE_ADDRESS = 0x45_200
SALT_ADDRESS = 0x45_400
IKM_ADDRESS = 0x45_600
INFO_ADDRESS = 0x45_800
PRK_ADDRESS = 0x45_A00
OUTPUT_ADDRESS = 0x45_C00
SECOND_OUTPUT_ADDRESS = 0x46_000

KEM_SEED_ADDRESS = 0x50_000
PUBLIC_KEY_ADDRESS = 0x51_000
SECRET_KEY_ADDRESS = 0x52_000
CIPHERTEXT_ADDRESS = 0x53_000
SHARED_SECRET_ADDRESS = 0x54_000
SECOND_SECRET_ADDRESS = 0x54_100
PEER_PUBLIC_ADDRESS = 0x55_000

HMAC_SHA256_JEFE = bytes.fromhex(
    "5bdcc146bf60754e6a042426089575c7"
    "5a003f089d2739839dec58b964ec3843"
)
HMAC_SHA256_LONG = bytes.fromhex(
    "af722cb81121a6da931dc4d5ba8722c"
    "38123c4458fd11c58fd644b5a3355fe38"
)
HKDF_SHA3_PRK = bytes.fromhex(
    "43c5f9188c5dbf8e6ff35a002b71949f"
    "134aeac0ae3367a13c971e504af62d3b"
)
HKDF_SHA3_OKM_65 = bytes.fromhex(
    "4913f3f2cd3d1955349e5ad5dc919a2e"
    "6b6ca9e72f78608dc7bbcfcf515db0ac"
    "795d0cab6c127d9614738841de5079c0d"
    "9f767bcd55734996442e02534fdc0a26e"
)
HKDF_SHA3_NULL_PRK = bytes.fromhex(
    "b509429db12ce33b5b3250a5dfdd8204"
    "59f5235ac5cd08463ed4a6b370859679"
)
HKDF_SHA256_PRK = bytes.fromhex(
    "077709362c2e32df0ddc3f0dc47bba63"
    "90b6c73bb50f9c3122ec844ad7c2b3e5"
)
HKDF_SHA256_OKM_42 = bytes.fromhex(
    "3cb25f25faacd57a90434f64d0362f2a"
    "2d2d0a90cf1a5a4c5db02d56ecc4c5bf"
    "34007208d5b887185865"
)
PQ_RANGE_PRK = bytes.fromhex(
    "c1d9dde1d3d5e9c6f1e4e681adc801dc"
    "7fa03f1bfe37918cd3e238224755ef5c"
)
PQ_RANGE_OUTPUT = bytes.fromhex(
    "09ca800f5f5bbdf24bb2e64c6d764a7"
    "39ec84dba1c2de4f68d73f9857595b8f9"
)

HMAC_SHA3_SCRATCH = (
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
    ("_HKDF-ZERO-SALT", 32),
    ("_HKDF-T", 32),
    ("_HKDF-PRK-PTR", 8),
    ("_HKDF-INFO-PTR", 8),
    ("_HKDF-INFO-LEN", 8),
    ("_HKDF-OUT-PTR", 8),
    ("_HKDF-REMAIN", 8),
    ("_HKDF-TPREV-LEN", 8),
    ("_HKDF-COUNTER", 8),
)
HMAC_SHA256_SCRATCH = (
    ("HMAC256-IPAD", 64),
    ("HMAC256-OPAD", 64),
    ("HMAC256-INNER", 32),
    ("HMAC256-KEY", 32),
    ("_HMAC256-PAD-PTR", 8),
    ("_HMAC256-XBYTE", 8),
    ("_HMAC256-OUT", 8),
    ("_HMAC256-KEY-PTR", 8),
    ("_HMAC256-KEY-LEN", 8),
    ("_HMAC256-MSG-PTR", 8),
    ("_HMAC256-MSG-LEN", 8),
    ("_HKDF256-ZERO-SALT", 32),
    ("_HKDF256-T", 32),
    ("_HKDF256-PRK-PTR", 8),
    ("_HKDF256-INFO-PTR", 8),
    ("_HKDF256-INFO-LEN", 8),
    ("_HKDF256-OUT-PTR", 8),
    ("_HKDF256-REMAIN", 8),
    ("_HKDF256-TPREV-LEN", 8),
    ("_HKDF256-COUNTER", 8),
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 14_088
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert lines[LAST_LINE] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_hybrid(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_hybrid(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_hybrid(_load_kem(runtime))


@pytest.fixture
def loaded_hybrid() -> MegaForthRuntime:
    return _load_hybrid()


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _scratch_zero(
    runtime: MegaForthRuntime,
    spans: tuple[tuple[str, int], ...],
) -> bool:
    return all(
        runtime.memory.read_bytes(_body(runtime, name), length) == bytes(length)
        for name, length in spans
    )


def _poison_scratch(
    runtime: MegaForthRuntime,
    spans: tuple[tuple[str, int], ...],
) -> None:
    for name, length in spans:
        runtime.memory.fill(_body(runtime, name), length, 0x5A)


def _hkdf_expand_reference(
    prk: bytes,
    info: bytes,
    length: int,
    digestmod,
) -> bytes:
    result = bytearray()
    previous = b""
    counter = 1
    while len(result) < length:
        previous = hmac.new(
            prk,
            previous + info + bytes((counter,)),
            digestmod,
        ).digest()
        result.extend(previous)
        counter += 1
    return bytes(result[:length])


def test_hybrid_slice_is_exact_and_publishes_complete_initialized_ledger(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    assert len(DEFINITIONS) == 59
    for name in DEFINITIONS:
        assert loaded_hybrid.find(name) is not None

    assert _execute(loaded_hybrid, "HKDF-HASHLEN") == (32,)
    assert _execute(loaded_hybrid, "HMAC256-BLKSZ") == (64,)
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-INFO"),
        9,
    ) == b"pq-hybrid"
    assert _scratch_zero(loaded_hybrid, HMAC_SHA3_SCRATCH)
    assert _scratch_zero(loaded_hybrid, HMAC_SHA256_SCRATCH)

    sized_bodies = (
        ("_PQ-SS-X", "_PQ-SS-K", 32),
        ("_PQ-SS-K", "_PQ-CAT", 32),
        ("_PQ-CAT", "_PQ-PRK", 64),
        ("_PQ-PRK", "_PQ-COIN", 32),
        ("_PQ-COIN", "_PQ-INFO", 32),
        ("_PQ-INFO", "_PQ-INFO-INIT", 9),
        ("_HKDF-ZERO-SALT", "_HKDF-T", 32),
        ("_HKDF-T", "_HKDF-PRK-PTR", 32),
        ("HMAC256-IPAD", "HMAC256-OPAD", 64),
        ("HMAC256-OPAD", "HMAC256-INNER", 64),
        ("HMAC256-INNER", "HMAC256-KEY", 32),
        ("HMAC256-KEY", "_HMAC256-PAD-PTR", 32),
        ("_HKDF256-ZERO-SALT", "_HKDF256-T", 32),
        ("_HKDF256-T", "_HKDF256-PRK-PTR", 32),
    )
    for name, following, size in sized_bodies:
        word = loaded_hybrid.find(name)
        next_word = loaded_hybrid.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == size

    for name in ("_PQ-SS-X", "_PQ-SS-K", "_PQ-CAT", "_PQ-PRK", "_PQ-COIN"):
        word = loaded_hybrid.find(name)
        assert word is not None
        following = loaded_hybrid.find(
            DEFINITIONS[DEFINITIONS.index(name.encode()) + 1]
        )
        assert following is not None
        length = following.header_address - word.body_address
        assert loaded_hybrid.memory.read_bytes(word.body_address, length) == (
            bytes(length)
        )
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.uart_output == b""


@pytest.mark.parametrize(
    ("key", "message", "expected"),
    (
        (b"Jefe", b"what do ya want for nothing?", HMAC_SHA256_JEFE),
        (bytes(range(65)), b"checkpoint-2", HMAC_SHA256_LONG),
        (b"", b"", hmac.new(b"", b"", hashlib.sha256).digest()),
    ),
)
def test_hmac_sha256_matches_independent_full_vectors_and_wipes(
    loaded_hybrid: MegaForthRuntime,
    key: bytes,
    message: bytes,
    expected: bytes,
) -> None:
    loaded_hybrid.memory.write_bytes(KEY_ADDRESS, key)
    loaded_hybrid.memory.write_bytes(MESSAGE_ADDRESS, message)
    loaded_hybrid.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)

    assert _execute(
        loaded_hybrid,
        "HMAC-SHA256",
        KEY_ADDRESS if key else MASK64,
        len(key),
        MESSAGE_ADDRESS if message else MASK64,
        len(message),
        OUTPUT_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(OUTPUT_ADDRESS, 32) == expected
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha2.private_zeroized("sha256")
    assert _scratch_zero(loaded_hybrid, HMAC_SHA256_SCRATCH)


def test_hkdf_sha256_matches_rfc5869_extract_and_multiblock_expand(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    salt = bytes(range(13))
    ikm = bytes((0x0B,)) * 22
    info = bytes(range(0xF0, 0xFA))
    loaded_hybrid.memory.write_bytes(SALT_ADDRESS, salt)
    loaded_hybrid.memory.write_bytes(IKM_ADDRESS, ikm)
    loaded_hybrid.memory.write_bytes(INFO_ADDRESS, info)

    assert _execute(
        loaded_hybrid,
        "HKDF-SHA256-EXTRACT",
        SALT_ADDRESS,
        len(salt),
        IKM_ADDRESS,
        len(ikm),
        PRK_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == HKDF_SHA256_PRK

    assert _execute(
        loaded_hybrid,
        "HKDF-SHA256-EXPAND",
        PRK_ADDRESS,
        INFO_ADDRESS,
        len(info),
        len(HKDF_SHA256_OKM_42),
        OUTPUT_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(
        OUTPUT_ADDRESS,
        len(HKDF_SHA256_OKM_42),
    ) == HKDF_SHA256_OKM_42
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha2.private_zeroized("sha256")
    assert _scratch_zero(loaded_hybrid, HMAC_SHA256_SCRATCH)


def test_hkdf_sha3_matches_full_null_salt_extract_and_three_block_expand(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    salt = bytes(range(32))
    ikm = b"input key material"
    info = b"tls13 derived"
    loaded_hybrid.memory.write_bytes(SALT_ADDRESS, salt)
    loaded_hybrid.memory.write_bytes(IKM_ADDRESS, ikm)
    loaded_hybrid.memory.write_bytes(INFO_ADDRESS, info)

    assert _execute(
        loaded_hybrid,
        "HKDF-EXTRACT",
        SALT_ADDRESS,
        len(salt),
        IKM_ADDRESS,
        len(ikm),
        PRK_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == HKDF_SHA3_PRK

    assert _execute(
        loaded_hybrid,
        "HKDF-EXPAND",
        PRK_ADDRESS,
        INFO_ADDRESS,
        len(info),
        len(HKDF_SHA3_OKM_65),
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(
        OUTPUT_ADDRESS,
        len(HKDF_SHA3_OKM_65),
    ) == HKDF_SHA3_OKM_65

    assert _execute(
        loaded_hybrid,
        "HKDF-EXTRACT",
        MASK64,
        0,
        IKM_ADDRESS,
        len(ikm),
        SECOND_OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(SECOND_OUTPUT_ADDRESS, 32) == (
        HKDF_SHA3_NULL_PRK
    )
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha3.checked_owner is None
    assert loaded_hybrid.sha3.private_zeroized()
    assert _scratch_zero(loaded_hybrid, HMAC_SHA3_SCRATCH)


@pytest.mark.parametrize(
    ("word", "range_status"),
    (
        ("HKDF-EXPAND", CRYPTO_STATUS_RANGE),
        ("HKDF-SHA256-EXPAND", SHA2_STATUS_RANGE),
    ),
)
def test_hkdf_expand_rejects_limit_and_input_aliases_before_publication(
    loaded_hybrid: MegaForthRuntime,
    word: str,
    range_status: int,
) -> None:
    loaded_hybrid.memory.write_bytes(PRK_ADDRESS, bytes(range(32)))
    loaded_hybrid.memory.write_bytes(INFO_ADDRESS, b"alias-check-info")
    loaded_hybrid.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)

    assert _execute(
        loaded_hybrid,
        word,
        PRK_ADDRESS,
        INFO_ADDRESS,
        16,
        8161,
        OUTPUT_ADDRESS,
    ) == (range_status,)
    assert loaded_hybrid.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32

    assert _execute(
        loaded_hybrid,
        word,
        PRK_ADDRESS,
        INFO_ADDRESS,
        16,
        16,
        PRK_ADDRESS,
    ) == (range_status,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == bytes(range(32))

    assert _execute(
        loaded_hybrid,
        word,
        PRK_ADDRESS,
        INFO_ADDRESS,
        16,
        8,
        INFO_ADDRESS + 4,
    ) == (range_status,)
    assert loaded_hybrid.memory.read_bytes(INFO_ADDRESS, 16) == (
        b"alias-check-info"
    )

    assert _execute(
        loaded_hybrid,
        word,
        PRK_ADDRESS,
        INFO_ADDRESS,
        16,
        0,
        MASK64,
    ) == (0,)
    assert loaded_hybrid.spinlocks.owner(9) is None


def test_null_salt_is_selected_by_zero_length_not_a_zero_pointer(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    ikm = b"input key material"
    loaded_hybrid.memory.write_bytes(IKM_ADDRESS, ikm)
    loaded_hybrid.memory.fill(PRK_ADDRESS, 32, 0xA5)

    assert _execute(
        loaded_hybrid,
        "HKDF-EXTRACT",
        0,
        1,
        IKM_ADDRESS,
        18,
        PRK_ADDRESS,
    ) == (CRYPTO_STATUS_RANGE,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32

    assert _execute(
        loaded_hybrid,
        "HKDF-EXTRACT",
        MASK64,
        0,
        IKM_ADDRESS,
        18,
        PRK_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == (
        HKDF_SHA3_NULL_PRK
    )

    assert _execute(
        loaded_hybrid,
        "HKDF-SHA256-EXTRACT",
        0,
        1,
        IKM_ADDRESS,
        len(ikm),
        PRK_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    one_byte_salt = bytes((loaded_hybrid.memory.read8(0),))
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == hmac.new(
        one_byte_salt,
        ikm,
        hashlib.sha256,
    ).digest()

    assert _execute(
        loaded_hybrid,
        "HKDF-SHA256-EXTRACT",
        MASK64,
        0,
        IKM_ADDRESS,
        len(ikm),
        PRK_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(PRK_ADDRESS, 32) == hmac.new(
        bytes(32),
        ikm,
        hashlib.sha256,
    ).digest()


def test_lock_contention_preserves_family_specific_priority_and_arguments() -> None:
    memory = create_one_core_address_space(crypto_capabilities=0)
    runtime = _load_hybrid(MegaForthRuntime(memory=memory))
    runtime.spinlocks = HostedSpinlockBank(core_count=2)
    assert runtime.spinlocks.acquire(9, 1) == SPINLOCK_ACQUIRED

    assert _execute(
        runtime,
        "HKDF-EXTRACT",
        MASK64,
        1,
        MASK64,
        1,
        MASK64,
    ) == (CRYPTO_STATUS_UNSUPPORTED,)
    assert _execute(
        runtime,
        "HKDF-SHA256-EXTRACT",
        MASK64,
        1,
        MASK64,
        1,
        MASK64,
    ) == (SHA2_STATUS_STATE,)
    assert runtime.spinlocks.owner(9) == 1
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_hkdf_success_and_early_failure_release_and_wipe_owned_scratch(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    loaded_hybrid.memory.write_bytes(PRK_ADDRESS, bytes(range(32)))
    loaded_hybrid.memory.write_bytes(INFO_ADDRESS, b"wipe")
    _poison_scratch(loaded_hybrid, HMAC_SHA3_SCRATCH)

    assert _execute(
        loaded_hybrid,
        "HKDF-EXPAND",
        PRK_ADDRESS,
        INFO_ADDRESS,
        4,
        33,
        OUTPUT_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert _scratch_zero(loaded_hybrid, HMAC_SHA3_SCRATCH)
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha3.private_zeroized()

    _poison_scratch(loaded_hybrid, HMAC_SHA256_SCRATCH)
    loaded_hybrid.memory.fill(SECOND_OUTPUT_ADDRESS, 32, 0xA5)
    assert _execute(
        loaded_hybrid,
        "HKDF-SHA256-EXPAND",
        PRK_ADDRESS,
        INFO_ADDRESS,
        4,
        8161,
        SECOND_OUTPUT_ADDRESS,
    ) == (SHA2_STATUS_RANGE,)
    assert loaded_hybrid.memory.read_bytes(SECOND_OUTPUT_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32
    assert _scratch_zero(loaded_hybrid, HMAC_SHA256_SCRATCH)
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha2.private_zeroized("sha256")


def test_pq_derive_matches_independent_hkdf_and_retains_pq_material(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    cat_address = _body(loaded_hybrid, "_PQ-CAT")
    prk_address = _body(loaded_hybrid, "_PQ-PRK")
    loaded_hybrid.memory.write_bytes(cat_address, bytes(range(64)))

    assert _execute(loaded_hybrid, "PQ-DERIVE", OUTPUT_ADDRESS) == (
        CRYPTO_STATUS_OK,
    )
    assert loaded_hybrid.memory.read_bytes(prk_address, 32) == PQ_RANGE_PRK
    assert loaded_hybrid.memory.read_bytes(OUTPUT_ADDRESS, 32) == (
        PQ_RANGE_OUTPUT
    )
    assert loaded_hybrid.memory.read_bytes(cat_address, 64) == bytes(range(64))
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-INFO"),
        9,
    ) == b"pq-hybrid"
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha3.private_zeroized()


def test_pq_derive_releases_between_extract_and_expand_and_retains_prk(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    class StealLockAfterExtract(HostedSpinlockBank):
        def __init__(self) -> None:
            super().__init__(core_count=2)
            self.stolen = False

        def release(self, lock_id: int, requester_core: int) -> None:
            super().release(lock_id, requester_core)
            if lock_id == 9 and requester_core == 0 and not self.stolen:
                self.stolen = True
                assert self.acquire(9, 1) == SPINLOCK_ACQUIRED

    cat_address = _body(loaded_hybrid, "_PQ-CAT")
    prk_address = _body(loaded_hybrid, "_PQ-PRK")
    loaded_hybrid.memory.write_bytes(cat_address, bytes(range(64)))
    loaded_hybrid.memory.fill(prk_address, 32, 0x5A)
    loaded_hybrid.memory.fill(OUTPUT_ADDRESS, 32, 0xA5)
    stealing_bank = StealLockAfterExtract()
    loaded_hybrid.spinlocks = stealing_bank

    assert _execute(loaded_hybrid, "PQ-DERIVE", OUTPUT_ADDRESS) == (
        CRYPTO_STATUS_STATE,
    )
    assert stealing_bank.stolen
    assert stealing_bank.owner(9) == 1
    assert loaded_hybrid.memory.read_bytes(prk_address, 32) == PQ_RANGE_PRK
    assert loaded_hybrid.memory.read_bytes(OUTPUT_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32
    assert loaded_hybrid.sha3.private_zeroized()
    assert _scratch_zero(loaded_hybrid, HMAC_SHA3_SCRATCH)


def test_pq_exchange_two_party_roundtrip_matches_composed_oracles(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    basepoint = b"\x09" + bytes(31)
    initiator_public = x25519_scalar_multiply(RFC_SCALAR_ONE, basepoint)
    responder_public = x25519_scalar_multiply(RFC_SCALAR_TWO, basepoint)
    x_shared = x25519_scalar_multiply(RFC_SCALAR_ONE, responder_public)
    assert x_shared == x25519_scalar_multiply(
        RFC_SCALAR_TWO,
        initiator_public,
    )
    loaded_hybrid.memory.write_bytes(
        _body(loaded_hybrid, "X25519-PRIV"),
        RFC_SCALAR_ONE,
    )
    loaded_hybrid.memory.write_bytes(PEER_PUBLIC_ADDRESS, responder_public)
    loaded_hybrid.memory.write_bytes(KEM_SEED_ADDRESS, bytes(64))
    assert _execute(
        loaded_hybrid,
        "KYBER-KEYGEN",
        KEM_SEED_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        SECRET_KEY_ADDRESS,
    ) == ()

    public_key = loaded_hybrid.memory.read_bytes(PUBLIC_KEY_ADDRESS, 800)
    secret_key = loaded_hybrid.memory.read_bytes(SECRET_KEY_ADDRESS, 1632)
    assert loaded_hybrid.entropy.pool_position == 0
    assert _execute(
        loaded_hybrid,
        "PQ-EXCHANGE-INIT",
        PEER_PUBLIC_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        CIPHERTEXT_ADDRESS,
        SHARED_SECRET_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)

    coin = loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-COIN"),
        32,
    )
    expected_ciphertext, kem_shared = mlkem512_encapsulate(public_key, coin)
    ciphertext = loaded_hybrid.memory.read_bytes(CIPHERTEXT_ADDRESS, 768)
    assert ciphertext == expected_ciphertext
    assert mlkem512_decapsulate(ciphertext, secret_key) == kem_shared
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-SS-X"),
        32,
    ) == x_shared
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-SS-K"),
        32,
    ) == kem_shared

    combined = x_shared + kem_shared
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-CAT"),
        64,
    ) == combined
    expected_prk = hmac.new(bytes(32), combined, hashlib.sha3_256).digest()
    expected_secret = _hkdf_expand_reference(
        expected_prk,
        b"pq-hybrid",
        32,
        hashlib.sha3_256,
    )
    assert loaded_hybrid.memory.read_bytes(SHARED_SECRET_ADDRESS, 32) == (
        expected_secret
    )
    assert loaded_hybrid.entropy.pool_position == 32

    loaded_hybrid.memory.write_bytes(
        _body(loaded_hybrid, "X25519-PRIV"),
        RFC_SCALAR_TWO,
    )
    loaded_hybrid.memory.write_bytes(PEER_PUBLIC_ADDRESS, initiator_public)
    assert _execute(
        loaded_hybrid,
        "PQ-EXCHANGE-RESP",
        PEER_PUBLIC_ADDRESS,
        CIPHERTEXT_ADDRESS,
        SECRET_KEY_ADDRESS,
        SECOND_SECRET_ADDRESS,
    ) == (CRYPTO_STATUS_OK,)
    assert loaded_hybrid.memory.read_bytes(SECOND_SECRET_ADDRESS, 32) == (
        expected_secret
    )
    assert loaded_hybrid.kem.status == KEM_STATUS_DONE
    assert loaded_hybrid.spinlocks.owner(9) is None
    assert loaded_hybrid.sha3.private_zeroized()


def test_pq_init_sha3_absence_follows_raw_effects_without_guard_cleanup(
) -> None:
    memory = create_one_core_address_space(crypto_capabilities=0)
    runtime = _load_hybrid(MegaForthRuntime(memory=memory))
    basepoint = b"\x09" + bytes(31)
    peer_public = x25519_scalar_multiply(RFC_SCALAR_TWO, basepoint)
    runtime.memory.write_bytes(
        _body(runtime, "X25519-PRIV"),
        RFC_SCALAR_ONE,
    )
    runtime.memory.write_bytes(PEER_PUBLIC_ADDRESS, peer_public)
    runtime.memory.write_bytes(KEM_SEED_ADDRESS, bytes(64))
    assert _execute(
        runtime,
        "KYBER-KEYGEN",
        KEM_SEED_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        SECRET_KEY_ADDRESS,
    ) == ()

    runtime.memory.fill(CIPHERTEXT_ADDRESS, 768, 0xA5)
    runtime.memory.fill(SHARED_SECRET_ADDRESS, 32, 0xA5)
    runtime.memory.fill(_body(runtime, "_PQ-PRK"), 32, 0x5A)
    _poison_scratch(runtime, HMAC_SHA3_SCRATCH)

    assert _execute(
        runtime,
        "PQ-EXCHANGE-INIT",
        PEER_PUBLIC_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        CIPHERTEXT_ADDRESS,
        SHARED_SECRET_ADDRESS,
    ) == (CRYPTO_STATUS_UNSUPPORTED,)
    assert runtime.memory.read_bytes(SHARED_SECRET_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32
    assert runtime.memory.read_bytes(CIPHERTEXT_ADDRESS, 768) != bytes(
        (0xA5,)
    ) * 768
    assert runtime.memory.read_bytes(_body(runtime, "_PQ-PRK"), 32) == bytes(
        (0x5A,)
    ) * 32
    assert any(runtime.memory.read_bytes(_body(runtime, "_PQ-CAT"), 64))
    assert runtime.entropy.pool_position == 32
    assert runtime.kem.status == KEM_STATUS_DONE
    assert runtime.spinlocks.owner(9) is None
    assert all(
        runtime.memory.read_bytes(_body(runtime, name), length)
        == bytes((0x5A,)) * length
        for name, length in HMAC_SHA3_SCRATCH
    )


def test_pq_exchange_hkdf_contention_is_nontransactional_but_preserves_key_out(
    loaded_hybrid: MegaForthRuntime,
) -> None:
    basepoint = b"\x09" + bytes(31)
    public_coordinate = x25519_scalar_multiply(RFC_SCALAR_ONE, basepoint)
    loaded_hybrid.memory.write_bytes(
        _body(loaded_hybrid, "X25519-PRIV"),
        RFC_SCALAR_ONE,
    )
    loaded_hybrid.memory.write_bytes(PEER_PUBLIC_ADDRESS, public_coordinate)
    loaded_hybrid.memory.write_bytes(KEM_SEED_ADDRESS, bytes(64))
    assert _execute(
        loaded_hybrid,
        "KYBER-KEYGEN",
        KEM_SEED_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        SECRET_KEY_ADDRESS,
    ) == ()

    loaded_hybrid.spinlocks = HostedSpinlockBank(core_count=2)
    assert loaded_hybrid.spinlocks.acquire(9, 1) == SPINLOCK_ACQUIRED
    loaded_hybrid.memory.fill(CIPHERTEXT_ADDRESS, 768, 0xA5)
    loaded_hybrid.memory.fill(SHARED_SECRET_ADDRESS, 32, 0xA5)
    loaded_hybrid.memory.fill(_body(loaded_hybrid, "_PQ-PRK"), 32, 0x5A)

    assert _execute(
        loaded_hybrid,
        "PQ-EXCHANGE-INIT",
        PEER_PUBLIC_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        CIPHERTEXT_ADDRESS,
        SHARED_SECRET_ADDRESS,
    ) == (CRYPTO_STATUS_STATE,)
    assert loaded_hybrid.memory.read_bytes(SHARED_SECRET_ADDRESS, 32) == bytes(
        (0xA5,)
    ) * 32
    assert loaded_hybrid.memory.read_bytes(CIPHERTEXT_ADDRESS, 768) != bytes(
        (0xA5,)
    ) * 768
    assert loaded_hybrid.memory.read_bytes(
        _body(loaded_hybrid, "_PQ-PRK"),
        32,
    ) == bytes((0x5A,)) * 32
    assert any(
        loaded_hybrid.memory.read_bytes(_body(loaded_hybrid, "_PQ-CAT"), 64)
    )
    assert loaded_hybrid.entropy.pool_position == 32
    assert loaded_hybrid.kem.status == KEM_STATUS_DONE
    assert loaded_hybrid.spinlocks.owner(9) == 1
