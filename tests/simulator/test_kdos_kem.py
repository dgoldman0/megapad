"""Contiguous unchanged-source acceptance for the KDOS ML-KEM slice."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from emulator.devices import KemDevice
from shared.mlkem import (
    MLKEM512_CIPHERTEXT_BYTES,
    MLKEM512_DECAPSULATION_KEY_BYTES,
    MLKEM512_ENCAPSULATION_KEY_BYTES,
    MLKEM512_ENCAPSULATION_RANDOM_BYTES,
    MLKEM512_KEYGEN_SEED_BYTES,
    MLKEM512_SHARED_SECRET_BYTES,
    mlkem512_decapsulate,
    mlkem512_encapsulate,
    mlkem512_keygen,
)
from simulator.kem import (
    HostedKEMService,
    KEM_BUFFER_CIPHERTEXT,
    KEM_BUFFER_PUBLIC_KEY,
    KEM_BUFFER_SECRET_KEY,
    KEM_BUFFER_SEED,
    KEM_BUFFER_SHARED_SECRET,
    KEM_BUFFER_SIZES,
    KEM_STATUS_DONE,
    KEM_STATUS_IDLE,
)
from simulator.memory import SparseAddressSpace, UnmappedAddressError
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_ntt import _load_ntt
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-kem-1586-1633.f"

FIRST_LINE = 1586
LAST_LINE = 1633
SLICE_SHA256 = (
    "e1c247f7d1c370b225eb2c6d8d0e0691d9670c8243631f51ee9a7265129a681f"
)
SLICE_GIT_BLOB = "47ecf1ca20fd82aed42f0179843945ad9e785d00"
DEFINITIONS = (
    b"KBUF-SEED",
    b"KBUF-PK",
    b"KBUF-SK",
    b"KBUF-CT",
    b"KBUF-SS",
    b"KEM-SEED-SIZE",
    b"KEM-PK-SIZE",
    b"KEM-SK-SIZE",
    b"KEM-CT-SIZE",
    b"KEM-SS-SIZE",
    b"KYBER-KEYGEN",
    b"KYBER-ENCAPS",
    b"KYBER-DECAPS",
    b".KEM-STATUS",
)
BIOS_WORDS = (
    "KEM-SEL!",
    "KEM-LOAD",
    "KEM-STORE",
    "KEM-KEYGEN",
    "KEM-ENCAPS",
    "KEM-DECAPS",
    "KEM-STATUS@",
)

SEED_ADDRESS = 0x38_000
PUBLIC_KEY_ADDRESS = 0x39_000
SECRET_KEY_ADDRESS = 0x3A_000
COIN_ADDRESS = 0x3B_000
CIPHERTEXT_ADDRESS = 0x3C_000
SHARED_SECRET_ADDRESS = 0x3D_000
SECOND_SECRET_ADDRESS = 0x3E_000
OVERLAP_ADDRESS = 0x3F_000

ZERO_PUBLIC_KEY_SHA256 = (
    "52b46f0597ac5cb10c6281ad5731f18d599feaa92ce24d897d4084195b27e448"
)
ZERO_SECRET_KEY_SHA256 = (
    "3a19948fd8e0d7af1e2f3bb32bf2299b91f40c66b3faeb773b8fc3dc2f140092"
)
ZERO_CIPHERTEXT_SHA256 = (
    "b9f7694fa5a2be9fb849d0c0ea8f55fce6d91eaecb9c34dffe47b5b5d6034de3"
)
ZERO_SHARED_SECRET = bytes.fromhex(
    "4ad53a06b29f12568421a552c08195b58673c82f870cc1ccd65a08e4325feb27"
)
ZERO_REJECTION_SECRET = bytes.fromhex(
    "4c77f51692e3623e52fb0c3fadb698b9ea7a89eb977d65af436429ff656572ba"
)


def _sha256(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 1510
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


def _evaluate_kem(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_kem(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_kem(_load_ntt(runtime))


@pytest.fixture
def loaded_kem() -> MegaForthRuntime:
    return _load_kem()


def _device_buffer(device: KemDevice, selector: int, size: int) -> bytes:
    device.write8(0x08, selector)
    return bytes(device.read8(0x18) for _ in range(size))


def test_kem_slice_is_exact_and_publishes_complete_ledger(
    loaded_kem: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_kem.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_kem.find(name) is not None

    expected_constants = (
        ("KBUF-SEED", KEM_BUFFER_SEED),
        ("KBUF-PK", KEM_BUFFER_PUBLIC_KEY),
        ("KBUF-SK", KEM_BUFFER_SECRET_KEY),
        ("KBUF-CT", KEM_BUFFER_CIPHERTEXT),
        ("KBUF-SS", KEM_BUFFER_SHARED_SECRET),
        # The unchanged source names one 32-byte component here even though
        # KYBER-KEYGEN explicitly transfers the complete 64-byte d || z input.
        ("KEM-SEED-SIZE", 32),
        ("KEM-PK-SIZE", MLKEM512_ENCAPSULATION_KEY_BYTES),
        ("KEM-SK-SIZE", MLKEM512_DECAPSULATION_KEY_BYTES),
        ("KEM-CT-SIZE", MLKEM512_CIPHERTEXT_BYTES),
        ("KEM-SS-SIZE", MLKEM512_SHARED_SECRET_BYTES),
    )
    for name, expected in expected_constants:
        assert _execute(loaded_kem, name) == (expected,)

    assert KEM_BUFFER_SIZES == (64, 800, 1632, 768, 32)
    assert MLKEM512_ENCAPSULATION_RANDOM_BYTES == 32
    assert b"KBUF-SEED KEM-SEL!  64 KEM-LOAD" in _verified_slice()
    assert loaded_kem.kem.status == KEM_STATUS_IDLE
    assert loaded_kem.uart_output == b""


def test_shared_mlkem_zero_vector_matches_independent_openssl_oracle() -> None:
    public_key, secret_key = mlkem512_keygen(bytes(64))
    ciphertext, shared_secret = mlkem512_encapsulate(public_key, bytes(32))

    assert len(public_key) == MLKEM512_ENCAPSULATION_KEY_BYTES
    assert len(secret_key) == MLKEM512_DECAPSULATION_KEY_BYTES
    assert len(ciphertext) == MLKEM512_CIPHERTEXT_BYTES
    assert _sha256(public_key) == ZERO_PUBLIC_KEY_SHA256
    assert _sha256(secret_key) == ZERO_SECRET_KEY_SHA256
    assert _sha256(ciphertext) == ZERO_CIPHERTEXT_SHA256
    assert shared_secret == ZERO_SHARED_SECRET
    assert mlkem512_decapsulate(ciphertext, secret_key) == shared_secret

    corrupted = bytes((ciphertext[0] ^ 0xFF,)) + ciphertext[1:]
    assert mlkem512_decapsulate(corrupted, secret_key) == ZERO_REJECTION_SECRET


def test_shared_mlkem_rejects_wrong_types_and_lengths() -> None:
    public_key, secret_key = mlkem512_keygen(bytes(64))
    ciphertext, _ = mlkem512_encapsulate(public_key, bytes(32))

    with pytest.raises(TypeError, match="must be bytes"):
        mlkem512_keygen(bytearray(64))  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="exactly 64 bytes"):
        mlkem512_keygen(bytes(63))
    with pytest.raises(ValueError, match="exactly 800 bytes"):
        mlkem512_encapsulate(public_key[:-1], bytes(32))
    with pytest.raises(ValueError, match="exactly 32 bytes"):
        mlkem512_encapsulate(public_key, bytes(31))
    with pytest.raises(ValueError, match="exactly 768 bytes"):
        mlkem512_decapsulate(ciphertext[:-1], secret_key)
    with pytest.raises(ValueError, match="exactly 1632 bytes"):
        mlkem512_decapsulate(ciphertext, secret_key[:-1])


def test_hosted_service_and_executable_emulator_share_exact_value_bytes() -> None:
    seed = bytes(range(64))
    coin = bytes(reversed(range(32)))
    service = HostedKEMService()
    device = KemDevice()

    service.select(KEM_BUFFER_SEED)
    for value in seed:
        service.write_data(value)
        device.write8(0x10, value)
    service.keygen()
    device.write8(0x01, 1)

    for selector, size in (
        (KEM_BUFFER_PUBLIC_KEY, MLKEM512_ENCAPSULATION_KEY_BYTES),
        (KEM_BUFFER_SECRET_KEY, MLKEM512_DECAPSULATION_KEY_BYTES),
    ):
        assert service.buffer(selector) == _device_buffer(device, selector, size)

    service.select(KEM_BUFFER_SEED)
    device.write8(0x08, KEM_BUFFER_SEED)
    for value in coin:
        service.write_data(value)
        device.write8(0x10, value)
    service.encapsulate()
    device.write8(0x01, 2)
    for selector, size in (
        (KEM_BUFFER_CIPHERTEXT, MLKEM512_CIPHERTEXT_BYTES),
        (KEM_BUFFER_SHARED_SECRET, MLKEM512_SHARED_SECRET_BYTES),
    ):
        assert service.buffer(selector) == _device_buffer(device, selector, size)

    service.decapsulate()
    device.write8(0x01, 3)
    assert service.buffer(KEM_BUFFER_SHARED_SECRET) == _device_buffer(
        device,
        KEM_BUFFER_SHARED_SECRET,
        MLKEM512_SHARED_SECRET_BYTES,
    )
    assert service.status == device.status == KEM_STATUS_DONE


def test_selector_stream_bounds_short_load_and_zero_count_semantics() -> None:
    memory = SparseAddressSpace(bank0_size=64)
    memory.write_bytes(0, b"\x11\x22")
    service = HostedKEMService()

    service.select(KEM_BUFFER_SEED)
    for value in range(64):
        service.write_data(value)
    service.select(KEM_BUFFER_SEED)
    service.load(0, 2, memory)
    assert service.buffer(KEM_BUFFER_SEED) == b"\x11\x22" + bytes(range(2, 64))
    assert service.index == 2

    service.select(0x1_0105)
    assert service.selector == KEM_BUFFER_SHARED_SECRET
    assert service.index == 0
    service.load(0xDEAD_BEEF, 0, memory)
    service.store(0xDEAD_BEEF, 0, memory)
    assert service.index == 0

    for value in range(40):
        service.write_data(value)
    assert service.buffer(KEM_BUFFER_SHARED_SECRET) == bytes(range(32))
    assert service.index == MLKEM512_SHARED_SECRET_BYTES
    assert service.read_data() == 0
    assert service.index == MLKEM512_SHARED_SECRET_BYTES


def test_load_fault_happens_before_the_faulting_device_write() -> None:
    memory = SparseAddressSpace(bank0_size=3)
    memory.write_bytes(0, b"abc")
    service = HostedKEMService()

    with pytest.raises(UnmappedAddressError):
        service.load(0, 4, memory)
    assert service.buffer(KEM_BUFFER_SEED)[:4] == b"abc\x00"
    assert service.index == 3

    full = SparseAddressSpace(bank0_size=MLKEM512_SHARED_SECRET_BYTES)
    full.write_bytes(0, bytes(range(MLKEM512_SHARED_SECRET_BYTES)))
    service.select(KEM_BUFFER_SHARED_SECRET)
    with pytest.raises(UnmappedAddressError):
        service.load(0, MLKEM512_SHARED_SECRET_BYTES + 1, full)
    assert service.buffer(KEM_BUFFER_SHARED_SECRET) == bytes(range(32))
    assert service.index == MLKEM512_SHARED_SECRET_BYTES


def test_store_fault_consumes_the_faulting_byte_and_overrun_writes_zero() -> None:
    service = HostedKEMService()
    service.select(KEM_BUFFER_SHARED_SECRET)
    for value in range(MLKEM512_SHARED_SECRET_BYTES):
        service.write_data(value + 1)
    service.select(KEM_BUFFER_SHARED_SECRET)

    short = SparseAddressSpace(bank0_size=2)
    with pytest.raises(UnmappedAddressError):
        service.store(0, 3, short)
    assert short.read_bytes(0, 2) == b"\x01\x02"
    assert service.index == 3

    service.select(KEM_BUFFER_SHARED_SECRET)
    long = SparseAddressSpace(bank0_size=34)
    service.store(0, 34, long)
    assert long.read_bytes(0, 34) == bytes(range(1, 33)) + b"\x00\x00"
    assert service.index == MLKEM512_SHARED_SECRET_BYTES


def test_seven_raw_words_preserve_stack_transfer_and_status_contract(
    loaded_kem: MegaForthRuntime,
) -> None:
    payload = bytes(range(64))
    loaded_kem.memory.write_bytes(SEED_ADDRESS, payload)

    assert _execute(loaded_kem, "KEM-STATUS@") == (KEM_STATUS_IDLE,)
    assert _execute(loaded_kem, "KEM-SEL!", KEM_BUFFER_SEED) == ()
    assert _execute(
        loaded_kem,
        "KEM-LOAD",
        SEED_ADDRESS,
        MLKEM512_KEYGEN_SEED_BYTES,
    ) == ()
    assert loaded_kem.kem.buffer(KEM_BUFFER_SEED) == payload
    assert _execute(loaded_kem, "KEM-KEYGEN") == ()
    assert _execute(loaded_kem, "KEM-STATUS@") == (KEM_STATUS_DONE,)

    assert _execute(loaded_kem, "KEM-SEL!", KEM_BUFFER_PUBLIC_KEY) == ()
    assert _execute(
        loaded_kem,
        "KEM-STORE",
        PUBLIC_KEY_ADDRESS,
        MLKEM512_ENCAPSULATION_KEY_BYTES,
    ) == ()
    assert loaded_kem.memory.read_bytes(
        PUBLIC_KEY_ADDRESS,
        MLKEM512_ENCAPSULATION_KEY_BYTES,
    ) == loaded_kem.kem.buffer(KEM_BUFFER_PUBLIC_KEY)

    assert _execute(loaded_kem, "KEM-ENCAPS") == ()
    assert _execute(loaded_kem, "KEM-DECAPS") == ()


def test_raw_transfer_faults_consume_stack_and_preserve_partial_device_state(
    loaded_kem: MegaForthRuntime,
) -> None:
    last_bank_byte = (1 << 20) - 1
    context = loaded_kem.main_context
    loaded_kem.memory.write8(last_bank_byte, 0xA5)
    loaded_kem.kem._status = KEM_STATUS_DONE
    assert _execute(loaded_kem, "KEM-SEL!", KEM_BUFFER_SEED) == ()

    context.data.push(last_bank_byte)
    context.data.push(2)
    with pytest.raises(UnmappedAddressError):
        loaded_kem.execute("KEM-LOAD")
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable
    assert loaded_kem.kem.buffer(KEM_BUFFER_SEED)[:2] == b"\xA5\x00"
    assert loaded_kem.kem.index == 1
    assert loaded_kem.kem.status == KEM_STATUS_DONE

    loaded_kem.kem.select(KEM_BUFFER_SHARED_SECRET)
    loaded_kem.kem.write_data(0x11)
    loaded_kem.kem.write_data(0x22)
    loaded_kem.kem.select(KEM_BUFFER_SHARED_SECRET)
    context.data.push(last_bank_byte)
    context.data.push(2)
    with pytest.raises(UnmappedAddressError):
        loaded_kem.execute("KEM-STORE")
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable
    assert loaded_kem.memory.read8(last_bank_byte) == 0x11
    assert loaded_kem.kem.index == 2
    assert loaded_kem.kem.status == KEM_STATUS_DONE


def test_unchanged_kdos_wrappers_match_full_vectors_and_implicit_rejection(
    loaded_kem: MegaForthRuntime,
) -> None:
    loaded_kem.memory.write_bytes(SEED_ADDRESS, bytes(64))
    loaded_kem.memory.write_bytes(COIN_ADDRESS, bytes(32))

    assert _execute(
        loaded_kem,
        "KYBER-KEYGEN",
        SEED_ADDRESS,
        PUBLIC_KEY_ADDRESS,
        SECRET_KEY_ADDRESS,
    ) == ()
    public_key = loaded_kem.memory.read_bytes(PUBLIC_KEY_ADDRESS, 800)
    secret_key = loaded_kem.memory.read_bytes(SECRET_KEY_ADDRESS, 1632)
    assert _sha256(public_key) == ZERO_PUBLIC_KEY_SHA256
    assert _sha256(secret_key) == ZERO_SECRET_KEY_SHA256

    assert _execute(
        loaded_kem,
        "KYBER-ENCAPS",
        PUBLIC_KEY_ADDRESS,
        COIN_ADDRESS,
        CIPHERTEXT_ADDRESS,
        SHARED_SECRET_ADDRESS,
    ) == ()
    ciphertext = loaded_kem.memory.read_bytes(CIPHERTEXT_ADDRESS, 768)
    shared_secret = loaded_kem.memory.read_bytes(SHARED_SECRET_ADDRESS, 32)
    assert _sha256(ciphertext) == ZERO_CIPHERTEXT_SHA256
    assert shared_secret == ZERO_SHARED_SECRET

    assert _execute(
        loaded_kem,
        "KYBER-DECAPS",
        CIPHERTEXT_ADDRESS,
        SECRET_KEY_ADDRESS,
        SECOND_SECRET_ADDRESS,
    ) == ()
    assert loaded_kem.memory.read_bytes(SECOND_SECRET_ADDRESS, 32) == shared_secret

    loaded_kem.memory.write8(CIPHERTEXT_ADDRESS, ciphertext[0] ^ 0xFF)
    assert _execute(
        loaded_kem,
        "KYBER-DECAPS",
        CIPHERTEXT_ADDRESS,
        SECRET_KEY_ADDRESS,
        SECOND_SECRET_ADDRESS,
    ) == ()
    assert loaded_kem.memory.read_bytes(SECOND_SECRET_ADDRESS, 32) == (
        ZERO_REJECTION_SECRET
    )


def test_unchanged_wrapper_aliases_follow_completed_input_and_store_order(
    loaded_kem: MegaForthRuntime,
) -> None:
    loaded_kem.memory.write_bytes(SEED_ADDRESS, bytes(64))
    assert _execute(
        loaded_kem,
        "KYBER-KEYGEN",
        SEED_ADDRESS,
        OVERLAP_ADDRESS,
        OVERLAP_ADDRESS,
    ) == ()
    assert loaded_kem.memory.read_bytes(OVERLAP_ADDRESS, 1632) == (
        loaded_kem.kem.buffer(KEM_BUFFER_SECRET_KEY)
    )

    loaded_kem.memory.write_bytes(PUBLIC_KEY_ADDRESS, loaded_kem.kem.buffer(1))
    loaded_kem.memory.write_bytes(COIN_ADDRESS, bytes(32))
    assert _execute(
        loaded_kem,
        "KYBER-ENCAPS",
        PUBLIC_KEY_ADDRESS,
        COIN_ADDRESS,
        OVERLAP_ADDRESS,
        OVERLAP_ADDRESS,
    ) == ()
    ciphertext = loaded_kem.kem.buffer(KEM_BUFFER_CIPHERTEXT)
    shared_secret = loaded_kem.kem.buffer(KEM_BUFFER_SHARED_SECRET)
    assert loaded_kem.memory.read_bytes(OVERLAP_ADDRESS, 768) == (
        shared_secret + ciphertext[32:]
    )

    loaded_kem.memory.write_bytes(CIPHERTEXT_ADDRESS, ciphertext)
    loaded_kem.memory.write_bytes(
        SECRET_KEY_ADDRESS,
        loaded_kem.kem.buffer(KEM_BUFFER_SECRET_KEY),
    )
    assert _execute(
        loaded_kem,
        "KYBER-DECAPS",
        CIPHERTEXT_ADDRESS,
        SECRET_KEY_ADDRESS,
        CIPHERTEXT_ADDRESS,
    ) == ()
    assert loaded_kem.memory.read_bytes(CIPHERTEXT_ADDRESS, 32) == shared_secret


def test_status_rendering_and_total_reset(loaded_kem: MegaForthRuntime) -> None:
    states = (
        (KEM_STATUS_IDLE, b" KEM: idle\r\n"),
        (KEM_STATUS_DONE, b" KEM: done\r\n"),
        (1, b" KEM: unknown\r\n"),
    )
    for status, expected in states:
        loaded_kem.kem._status = status
        assert _execute(loaded_kem, ".KEM-STATUS") == ()
        assert loaded_kem.drain_uart_output() == expected

    loaded_kem.kem.select(KEM_BUFFER_SEED)
    loaded_kem.kem.write_data(0xA5)
    loaded_kem.kem.keygen()
    loaded_kem.kem.select(KEM_BUFFER_SECRET_KEY)
    loaded_kem.kem.read_data()
    loaded_kem.kem.reset()
    assert loaded_kem.kem.status == KEM_STATUS_IDLE
    assert loaded_kem.kem.selector == KEM_BUFFER_SEED
    assert loaded_kem.kem.index == 0
    for selector, size in enumerate(KEM_BUFFER_SIZES):
        assert loaded_kem.kem.buffer(selector) == bytes(size)
