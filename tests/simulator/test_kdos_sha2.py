"""Contiguous unchanged-source acceptance for checked KDOS SHA-2."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.memory import (
    EXTERNAL_BASE,
    HBW_BASE,
    MMIO_BASE,
    VRAM_BASE,
    SparseAddressSpace,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.sha2 import (
    HostedSHA2Service,
    SHA2_STATUS_CONTEXT_ALIAS,
    SHA2_STATUS_LENGTH_OVERFLOW,
    SHA2_STATUS_OK,
    SHA2_STATUS_RANGE,
    SHA2_STATUS_STATE,
    SHA256_ALGORITHM,
    SHA512_ALGORITHM,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_sha3 import _execute, _load_sha3


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-sha2-1217-1269.f"

FIRST_LINE = 1217
LAST_LINE = 1269
SLICE_SHA256 = "ec5c329cc6e61bfbf9d6dce7fcb2c3081325b9d47f603d0218553561054f3bdd"
SLICE_GIT_BLOB = "6cfec1c5eb762f35afc52fa242c5a9b88142c4fa"
DEFINITIONS = (
    b"HASH",
    b"SHA256-OK",
    b"SHA256-STATE",
    b"SHA256-RANGE",
    b"SHA256-CONTEXT-ALIAS",
    b"SHA256-LENGTH-OVERFLOW",
    b"SHA256",
    b"SHA512-OK",
    b"SHA512-STATE",
    b"SHA512-RANGE",
    b"SHA512-CONTEXT-ALIAS",
    b"SHA512-LENGTH-OVERFLOW",
    b"SHA512",
)
BIOS_WORDS = (
    "SHA2-SPAN-STATUS",
    "SHA256-INIT",
    "SHA256-UPDATE",
    "SHA256-FINAL",
    "SHA256-CLEAR",
    "SHA512-INIT",
    "SHA512-UPDATE",
    "SHA512-FINAL",
    "SHA512-CLEAR",
)

SOURCE_ADDRESS = 0x24_000
OUTPUT_ADDRESS = 0x25_000

SHA256_EMPTY = bytes.fromhex(
    "e3b0c44298fc1c149afbf4c8996fb924"
    "27ae41e4649b934ca495991b7852b855"
)
SHA256_ABC = bytes.fromhex(
    "ba7816bf8f01cfea414140de5dae2223"
    "b00361a396177a9cb410ff61f20015ad"
)
SHA512_EMPTY = bytes.fromhex(
    "cf83e1357eefb8bdf1542850d66d8007"
    "d620e4050b5715dc83f4a921d36ce9ce"
    "47d0d13c5d85f2b0ff8318d2877eec2f"
    "63b931bd47417a81a538327af927da3e"
)
SHA512_ABC = bytes.fromhex(
    "ddaf35a193617abacc417349ae204131"
    "12e6fa4e89a97ea20a9eeee64b55d39a"
    "2192992a274fc1a836ba3c23a3feebbd"
    "454d4423643ce80e2a9ac94fa54ca49f"
)
SHA3_256_ABC = bytes.fromhex(
    "3a985da74fe225b2045c172d6bd390bd"
    "855f086e3e9d525b46bfe24511431532"
)

# Pinned externally visible digests for message[i] = (37*i + 11) & 0xff.
# Keeping these bytes literal prevents the hosted hashlib implementation from
# serving as its own padding and split-boundary oracle.
SHA256_BOUNDARIES = {
    0: SHA256_EMPTY,
    55: bytes.fromhex(
        "2900465fcb533e05a158fd2b3be0e5e3"
        "b03740d83060aa3580e0d98a96bf2384"
    ),
    56: bytes.fromhex(
        "31454ff48ef36af2f08fd511bdc37d9d"
        "5855ac23e992e5ff5445cb6b7674a674"
    ),
    63: bytes.fromhex(
        "5f6401b96532c36de4e65beec0409b69"
        "b1d181864c8009b7a04f43e5d56350d1"
    ),
    64: bytes.fromhex(
        "94eb5de4943613fd048dc93393ab0687"
        "7405faa39c11f53e9386083339833e7e"
    ),
    65: bytes.fromhex(
        "fc518669b6eb4b4dd91827ecacef8668"
        "9c725bd5bab888fd3b26dbb196eec954"
    ),
}
SHA512_BOUNDARIES = {
    0: SHA512_EMPTY,
    111: bytes.fromhex(
        "4d1db900250c96436052fbca79c13acb"
        "f378aad9c35b87d94c3803264df61fd2"
        "2cbd327c8938d024db372abf4208934e"
        "e09367d571d6c670bf74ee07b83e7506"
    ),
    112: bytes.fromhex(
        "dfb715ca3478a894302ace39c42d1d66"
        "46e1044f2247a6274d8b42d155d2fdbe"
        "7017195e85cfba96bedc51f84c446389"
        "78a540039ff09c64cef6c0c5ccc8f7b6"
    ),
    127: bytes.fromhex(
        "f93a0e7465b294188e8aa2b1cc2e98bc"
        "8d5115d46f51c7a9ec599b9d9f96a80f"
        "ef6a4f226b648c89bd9eac23b3d64264"
        "898b568d915c66666c44cd0319e2ef56"
    ),
    128: bytes.fromhex(
        "0b4815d35f9d07b1a30de2790e1be2a7"
        "20234295cd7b4d9e9af51719ff90019f"
        "1fe6d4e402a7dcc4177085023dc460ab"
        "743dad9b2c1dda42662bda5d3b2e155b"
    ),
    129: bytes.fromhex(
        "1809db04d02717483e04bc4333a14308"
        "bd2d0213ba7bf2c63f11eb1b8a0af825"
        "2e67fd104fd466fb95f945539824d8e4"
        "183155fa5ced0bee3dad46d9384a0bd5"
    ),
}


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_sha2(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_sha2(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_sha2(_load_sha3(runtime))


@pytest.fixture
def loaded_sha2() -> MegaForthRuntime:
    return _load_sha2()


def _service_methods(service: HostedSHA2Service, algorithm: str):
    return (
        getattr(service, f"{algorithm}_init"),
        getattr(service, f"{algorithm}_update"),
        getattr(service, f"{algorithm}_final"),
        getattr(service, f"{algorithm}_clear"),
    )


class _RecordingAddressSpace(SparseAddressSpace):
    """Record complete block publications without changing memory rules."""

    def __init__(self, *, bank0_size: int) -> None:
        super().__init__(bank0_size=bank0_size)
        self.publications: list[tuple[int, bytes]] = []

    def write_bytes(self, address: int, payload) -> None:
        raw = bytes(payload)
        self.publications.append((address, raw))
        super().write_bytes(address, raw)


def test_sha2_slice_is_exact_and_publishes_complete_ledger(
    loaded_sha2: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_sha2.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_sha2.find(name) is not None

    expected_constants = {
        "SHA256-OK": 0,
        "SHA256-STATE": 1,
        "SHA256-RANGE": 2,
        "SHA256-CONTEXT-ALIAS": 3,
        "SHA256-LENGTH-OVERFLOW": 4,
        "SHA512-OK": 0,
        "SHA512-STATE": 1,
        "SHA512-RANGE": 2,
        "SHA512-CONTEXT-ALIAS": 3,
        "SHA512-LENGTH-OVERFLOW": 4,
    }
    for name, value in expected_constants.items():
        assert _execute(loaded_sha2, name) == (value,)
    assert loaded_sha2.sha2.core_count == 1
    assert loaded_sha2.sha2.context_alias_ranges == ()
    assert loaded_sha2.uart_output == b""


@pytest.mark.parametrize(
    ("word", "expected", "digest_bytes"),
    (
        ("HASH", SHA3_256_ABC, 32),
        ("SHA256", SHA256_ABC, 32),
        ("SHA512", SHA512_ABC, 64),
    ),
)
def test_unchanged_hash_wrappers_match_hard_coded_abc_vectors(
    loaded_sha2: MegaForthRuntime,
    word: str,
    expected: bytes,
    digest_bytes: int,
) -> None:
    loaded_sha2.memory.write_bytes(SOURCE_ADDRESS, b"abc")
    loaded_sha2.memory.fill(OUTPUT_ADDRESS, digest_bytes, 0xA5)

    assert _execute(
        loaded_sha2,
        word,
        SOURCE_ADDRESS,
        3,
        OUTPUT_ADDRESS,
    ) == (SHA2_STATUS_OK,)
    assert loaded_sha2.memory.read_bytes(OUTPUT_ADDRESS, digest_bytes) == (
        expected
    )
    assert loaded_sha2.sha2.private_zeroized(SHA256_ALGORITHM)
    assert loaded_sha2.sha2.private_zeroized(SHA512_ALGORITHM)


@pytest.mark.parametrize(
    ("algorithm", "block_bytes", "digest_bytes", "boundaries"),
    (
        (SHA256_ALGORITHM, 64, 32, SHA256_BOUNDARIES),
        (SHA512_ALGORITHM, 128, 64, SHA512_BOUNDARIES),
    ),
)
def test_streaming_padding_rate_and_irregular_split_boundaries_are_pinned(
    loaded_sha2: MegaForthRuntime,
    algorithm: str,
    block_bytes: int,
    digest_bytes: int,
    boundaries: dict[int, bytes],
) -> None:
    init_word = f"{algorithm.upper()}-INIT"
    update_word = f"{algorithm.upper()}-UPDATE"
    final_word = f"{algorithm.upper()}-FINAL"

    for length, expected in boundaries.items():
        message = bytes((index * 37 + 11) & 0xFF for index in range(length))
        loaded_sha2.memory.write_bytes(SOURCE_ADDRESS, message)
        loaded_sha2.memory.fill(OUTPUT_ADDRESS, digest_bytes, 0xA5)
        assert _execute(loaded_sha2, init_word) == (SHA2_STATUS_OK,)

        # Empty updates must still require INIT, but then ignore the address.
        assert _execute(loaded_sha2, update_word, MASK64, 0) == (
            SHA2_STATUS_OK,
        )
        cuts = sorted(
            {
                0,
                length,
                min(length, 1),
                min(length, block_bytes - 9),
                min(length, block_bytes - 1),
                min(length, block_bytes),
            }
        )
        for start, end in zip(cuts, cuts[1:]):
            assert _execute(
                loaded_sha2,
                update_word,
                SOURCE_ADDRESS + start,
                end - start,
            ) == (SHA2_STATUS_OK,)

        assert _execute(loaded_sha2, final_word, OUTPUT_ADDRESS) == (
            SHA2_STATUS_OK,
        )
        assert loaded_sha2.memory.read_bytes(
            OUTPUT_ADDRESS,
            digest_bytes,
        ) == expected
        assert loaded_sha2.sha2.private_zeroized(algorithm)


@pytest.mark.parametrize("word", ("SHA256", "SHA512"))
def test_source_sha2_wrappers_propagate_range_and_clean_up(
    loaded_sha2: MegaForthRuntime,
    word: str,
) -> None:
    digest_bytes = 32 if word == "SHA256" else 64
    algorithm = word.lower()
    loaded_sha2.memory.fill(OUTPUT_ADDRESS, digest_bytes, 0xA5)

    assert _execute(
        loaded_sha2,
        word,
        MMIO_BASE,
        1,
        OUTPUT_ADDRESS,
    ) == (SHA2_STATUS_RANGE,)
    assert loaded_sha2.memory.read_bytes(OUTPUT_ADDRESS, digest_bytes) == (
        bytes([0xA5] * digest_bytes)
    )
    assert loaded_sha2.sha2.private_zeroized(algorithm)


def test_sha2_span_status_is_physical_not_caller_owned_policy() -> None:
    memory = create_one_core_address_space(
        external_size=0x1000,
        vram_size=0x1000,
        hbw_size=0x1000,
    )
    runtime = _load_sha2(MegaForthRuntime(memory=memory))
    floor = runtime.dictionary.numeric_rollback_floor

    # Unlike CALLER-SPAN-STATUS, the SHA-2 service may read physical Bank 0
    # address zero and the protected dictionary prefix.
    assert _execute(runtime, "SHA2-SPAN-STATUS", 0, 1) == (
        SHA2_STATUS_OK,
    )
    assert _execute(runtime, "CALLER-SPAN-STATUS", 0, 1) == (
        SHA2_STATUS_RANGE,
    )
    assert _execute(runtime, "SHA2-SPAN-STATUS", floor - 1, 1) == (
        SHA2_STATUS_OK,
    )
    assert _execute(runtime, "CALLER-SPAN-STATUS", floor - 1, 1) == (
        SHA2_STATUS_CONTEXT_ALIAS,
    )

    for region in memory.regions:
        assert _execute(
            runtime,
            "SHA2-SPAN-STATUS",
            region.base,
            region.size,
        ) == (SHA2_STATUS_OK,)
        assert _execute(
            runtime,
            "SHA2-SPAN-STATUS",
            region.limit - 1,
            2,
        ) == (SHA2_STATUS_RANGE,)

    for address in (0, MASK64, MMIO_BASE):
        assert _execute(runtime, "SHA2-SPAN-STATUS", address, 0) == (
            SHA2_STATUS_OK,
        )
    for address, length in (
        (MASK64, 2),
        (EXTERNAL_BASE + 0x1000, 1),
        (VRAM_BASE + 0x1000, 1),
        (HBW_BASE + 0x1000, 1),
        (MMIO_BASE, 1),
    ):
        assert _execute(runtime, "SHA2-SPAN-STATUS", address, length) == (
            SHA2_STATUS_RANGE,
        )


def test_optional_context_alias_union_returns_three_and_wipes_failures() -> None:
    memory = SparseAddressSpace(bank0_size=0x1000)
    aliases = ((0x300, 0x380), (0x700, 0x780))
    service = HostedSHA2Service(
        core_count=1,
        context_alias_ranges=aliases,
    )
    assert service.context_alias_ranges == aliases
    assert service.span_status(memory, 0x200, 0x100) == SHA2_STATUS_OK
    assert service.span_status(memory, 0x2FF, 2) == (
        SHA2_STATUS_CONTEXT_ALIAS
    )
    assert service.span_status(memory, 0x380, 1) == SHA2_STATUS_OK
    assert service.span_status(memory, 0x300, 0) == SHA2_STATUS_OK
    # Geometry has priority over aliasing.
    assert service.span_status(memory, 0xFFF, 2) == SHA2_STATUS_RANGE

    for algorithm, digest_bytes in (
        (SHA256_ALGORITHM, 32),
        (SHA512_ALGORITHM, 64),
    ):
        init, update, final, _clear = _service_methods(service, algorithm)
        for alias_base, _alias_limit in aliases:
            assert init(0) == SHA2_STATUS_OK
            assert update(0, alias_base, 1, memory) == (
                SHA2_STATUS_CONTEXT_ALIAS
            )
            assert service.private_zeroized(algorithm)

            memory.fill(alias_base, digest_bytes, 0xA5)
            assert init(0) == SHA2_STATUS_OK
            assert final(0, alias_base, memory) == (
                SHA2_STATUS_CONTEXT_ALIAS
            )
            assert memory.read_bytes(alias_base, digest_bytes) == bytes(
                [0xA5] * digest_bytes
            )
            assert service.private_zeroized(algorithm)


@pytest.mark.parametrize(
    ("algorithm", "block_bytes", "digest_bytes"),
    (
        (SHA256_ALGORITHM, 64, 32),
        (SHA512_ALGORITHM, 128, 64),
    ),
)
def test_inactive_and_forged_metadata_fail_before_span_validation(
    algorithm: str,
    block_bytes: int,
    digest_bytes: int,
) -> None:
    memory = SparseAddressSpace(bank0_size=0x1000)
    service = HostedSHA2Service(core_count=1)
    init, update, final, _clear = _service_methods(service, algorithm)

    assert update(0, MASK64, 0, memory) == SHA2_STATUS_STATE
    assert final(0, MASK64, memory) == SHA2_STATUS_STATE
    assert service.private_zeroized(algorithm)

    corruptions = (
        {"active_marker": 2},
        {"partial_offset": block_bytes},
        {"bit_length_low": 1},
        {"bit_length_low": 8, "partial_offset": 0},
    )
    for corruption in corruptions:
        assert init(0) == SHA2_STATUS_OK
        service.inject_context_metadata_for_test(
            algorithm,
            **corruption,
        )
        assert update(0, MASK64, 1, memory) == SHA2_STATUS_STATE
        assert service.private_zeroized(algorithm)

        assert init(0) == SHA2_STATUS_OK
        service.inject_context_metadata_for_test(
            algorithm,
            **corruption,
        )
        assert final(0, MASK64, memory) == SHA2_STATUS_STATE
        assert service.private_zeroized(algorithm)

    memory.fill(0x200, digest_bytes, 0xA5)
    assert final(0, 0x200, memory) == SHA2_STATUS_STATE
    assert memory.read_bytes(0x200, digest_bytes) == bytes(
        [0xA5] * digest_bytes
    )


@pytest.mark.parametrize(
    ("algorithm", "block_bytes"),
    (
        (SHA256_ALGORITHM, 64),
        (SHA512_ALGORITHM, 128),
    ),
)
def test_length_width_overflow_follows_state_range_and_alias_precedence(
    algorithm: str,
    block_bytes: int,
) -> None:
    memory = SparseAddressSpace(bank0_size=0x1000)
    memory.write8(0x100, 0x41)
    service = HostedSHA2Service(
        core_count=1,
        context_alias_ranges=((0x300, 0x380),),
    )
    init, update, final, clear = _service_methods(service, algorithm)
    high = 0 if algorithm == SHA256_ALGORITHM else MASK64

    def prime_last_byte() -> None:
        assert init(0) == SHA2_STATUS_OK
        service.inject_context_metadata_for_test(
            algorithm,
            bit_length_low=MASK64 - 7,
            bit_length_high=high,
            partial_offset=block_bytes - 1,
        )

    prime_last_byte()
    assert update(0, MASK64, 1, memory) == SHA2_STATUS_RANGE
    assert service.private_zeroized(algorithm)

    prime_last_byte()
    assert update(0, 0x300, 1, memory) == SHA2_STATUS_CONTEXT_ALIAS
    assert service.private_zeroized(algorithm)

    prime_last_byte()
    assert update(0, 0x100, 1, memory) == SHA2_STATUS_LENGTH_OVERFLOW
    assert service.private_zeroized(algorithm)

    # A zero-length update performs no addition and does not inspect address.
    prime_last_byte()
    assert update(0, MASK64, 0, memory) == SHA2_STATUS_OK
    assert not service.private_zeroized(algorithm)
    assert clear(0) == SHA2_STATUS_OK

    assert init(0) == SHA2_STATUS_OK
    service.inject_context_metadata_for_test(
        algorithm,
        bit_length_high=1,
    )
    if algorithm == SHA256_ALGORITHM:
        # A nonzero high half is already a malformed 64-bit length and wins
        # before even an invalid caller span is considered.
        assert update(0, MASK64, 1, memory) == (
            SHA2_STATUS_LENGTH_OVERFLOW
        )
        assert service.private_zeroized(algorithm)

        assert init(0) == SHA2_STATUS_OK
        service.inject_context_metadata_for_test(
            algorithm,
            bit_length_high=1,
        )
        assert final(0, MASK64, memory) == SHA2_STATUS_LENGTH_OVERFLOW
        assert service.private_zeroized(algorithm)
    else:
        # SHA-512 owns the complete 128-bit counter, so the same high half is
        # valid and an empty update remains active.
        assert update(0, MASK64, 0, memory) == SHA2_STATUS_OK
        assert not service.private_zeroized(algorithm)
        assert clear(0) == SHA2_STATUS_OK


@pytest.mark.parametrize(
    ("algorithm", "digest_bytes", "expected"),
    (
        (SHA256_ALGORITHM, 32, SHA256_ABC),
        (SHA512_ALGORITHM, 64, SHA512_ABC),
    ),
)
def test_final_stages_one_complete_publication_and_failure_publishes_nothing(
    algorithm: str,
    digest_bytes: int,
    expected: bytes,
) -> None:
    memory = _RecordingAddressSpace(bank0_size=0x1000)
    service = HostedSHA2Service(core_count=1)
    init, update, final, _clear = _service_methods(service, algorithm)

    memory.write_bytes(0x100, b"abc")
    assert init(0) == SHA2_STATUS_OK
    assert update(0, 0x100, 3, memory) == SHA2_STATUS_OK
    memory.publications.clear()
    assert final(0, 0x100, memory) == SHA2_STATUS_OK
    assert memory.publications == [(0x100, expected)]
    assert memory.read_bytes(0x100, digest_bytes) == expected
    assert service.private_zeroized(algorithm)

    bad_destination = 0x1000 - digest_bytes // 2
    memory.write_bytes(0x100, b"abc")
    memory.fill(bad_destination, digest_bytes // 2, 0xA5)
    assert init(0) == SHA2_STATUS_OK
    assert update(0, 0x100, 3, memory) == SHA2_STATUS_OK
    memory.publications.clear()
    assert final(0, bad_destination, memory) == SHA2_STATUS_RANGE
    assert memory.publications == []
    assert memory.read_bytes(
        bad_destination,
        digest_bytes // 2,
    ) == bytes([0xA5] * (digest_bytes // 2))
    assert service.private_zeroized(algorithm)


def test_sha2_families_and_full_core_contexts_are_independent() -> None:
    memory = SparseAddressSpace(bank0_size=0x4000)
    service = HostedSHA2Service(core_count=2)
    transactions = (
        (SHA256_ALGORITHM, 0, 0x100, 0x1000, b"sha256 core zero"),
        (SHA256_ALGORITHM, 1, 0x200, 0x1100, b"sha256 core one"),
        (SHA512_ALGORITHM, 0, 0x300, 0x1200, b"sha512 core zero"),
        (SHA512_ALGORITHM, 1, 0x400, 0x1300, b"sha512 core one"),
    )

    for algorithm, core_id, source, _output, message in transactions:
        memory.write_bytes(source, message)
        init, update, _final, _clear = _service_methods(service, algorithm)
        assert init(core_id) == SHA2_STATUS_OK
        assert update(core_id, source, len(message), memory) == SHA2_STATUS_OK

    for algorithm, core_id, _source, output, message in reversed(transactions):
        _init, _update, final, _clear = _service_methods(service, algorithm)
        digest_bytes = 32 if algorithm == SHA256_ALGORITHM else 64
        assert final(core_id, output, memory) == SHA2_STATUS_OK
        assert memory.read_bytes(output, digest_bytes) == hashlib.new(
            algorithm,
            message,
        ).digest()
        assert service.private_zeroized(algorithm, core_id=core_id)


@pytest.mark.parametrize(
    ("algorithm", "digest_bytes", "empty_digest"),
    (
        (SHA256_ALGORITHM, 32, SHA256_EMPTY),
        (SHA512_ALGORITHM, 64, SHA512_EMPTY),
    ),
)
def test_clear_is_idempotent_and_reinit_logically_zeroizes_prior_state(
    algorithm: str,
    digest_bytes: int,
    empty_digest: bytes,
) -> None:
    memory = SparseAddressSpace(bank0_size=0x1000)
    service = HostedSHA2Service(core_count=1)
    init, update, final, clear = _service_methods(service, algorithm)
    assert service.private_zeroized(algorithm)

    memory.write_bytes(0x100, b"abc")
    assert init(0) == SHA2_STATUS_OK
    assert update(0, 0x100, 3, memory) == SHA2_STATUS_OK
    assert not service.private_zeroized(algorithm)

    # INIT is a fresh transaction, not a STATE failure or continuation.
    assert init(0) == SHA2_STATUS_OK
    assert final(0, 0x200, memory) == SHA2_STATUS_OK
    assert memory.read_bytes(0x200, digest_bytes) == empty_digest
    assert service.private_zeroized(algorithm)

    assert init(0) == SHA2_STATUS_OK
    assert update(0, 0x100, 3, memory) == SHA2_STATUS_OK
    assert clear(0) == SHA2_STATUS_OK
    assert service.private_zeroized(algorithm)
    assert clear(0) == SHA2_STATUS_OK
    assert service.private_zeroized(algorithm)

    memory.fill(0x200, digest_bytes, 0xA5)
    assert final(0, 0x200, memory) == SHA2_STATUS_STATE
    assert memory.read_bytes(0x200, digest_bytes) == bytes(
        [0xA5] * digest_bytes
    )
    assert service.private_zeroized(algorithm)
