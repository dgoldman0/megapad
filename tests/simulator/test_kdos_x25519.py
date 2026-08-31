"""Contiguous unchanged-source acceptance for KDOS X25519."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.x25519 import X25519_BYTES, x25519_scalar_multiply
from simulator.field import HostedFieldALUService
from simulator.memory import (
    EXTERNAL_BASE,
    SparseAddressSpace,
    UnmappedAddressError,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_hmac import _load_hmac


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / (
    "kdos-x25519-1433-1481.f"
)

FIRST_LINE = 1433
LAST_LINE = 1481
SLICE_SHA256 = "2177b310911ce4ea4eba937a26797d265e83466c67a2089a82fd6690840e3f8f"
SLICE_GIT_BLOB = "2e46939cc9ada82480e83b0e3e1cda7db2a9d88e"
DEFINITIONS = (
    b"X25519-PRIV",
    b"X25519-PUB",
    b"X25519-SHARED",
    b"X25519-BASE",
    b"X25519",
    b"X25519-KEYGEN",
    b"X25519-DH",
)
BIOS_WORDS = (
    "X25519-SCALAR!",
    "X25519-POINT!",
    "X25519-GO",
    "X25519-WAIT",
    "X25519-STATUS@",
    "X25519-RESULT@",
)

SCALAR_ADDRESS = 0x2A_000
POINT_ADDRESS = 0x2A_100
RESULT_ADDRESS = 0x2A_200
PEER_ADDRESS = 0x2A_300

BASEPOINT = b"\x09" + bytes(31)
RFC_SCALAR_ONE = bytes.fromhex(
    "a546e36bf0527c9d3b16154b82465edd"
    "62144c0ac1fc5a18506a2244ba449ac4"
)
RFC_POINT_ONE = bytes.fromhex(
    "e6db6867583030db3594c1a424b15f7c"
    "726624ec26b3353b10a903a6d0ab1c4c"
)
RFC_RESULT_ONE = bytes.fromhex(
    "c3da55379de9c6908e94ea4df28d084f"
    "32eccf03491c71f754b4075577a28552"
)
RFC_SCALAR_TWO = bytes.fromhex(
    "4b66e9d4d1b4673c5ad22691957d6af5"
    "c11b6421e0ea01d42ca4169e7918ba0d"
)
RFC_POINT_TWO = bytes.fromhex(
    "e5210f12786811d3f4b7959d0538ae2c"
    "31dbe7106fc03c3efc4cd549c715a493"
)
RFC_RESULT_TWO = bytes.fromhex(
    "95cbde9476e8907d7aade45cb4b873f8"
    "8b595a68799fa152e6f8f7647aac7957"
)

KEYGEN_SEED = b"KDOS X25519 deterministic keygen"
KEYGEN_PRIVATE = bytes.fromhex(
    "052ceb5a8d53fda0a330815cb798ef21"
    "a4380b1d5675e4d62462d62d09323876"
)
KEYGEN_PUBLIC = bytes.fromhex(
    "caa7b05e5b18f05c12f13914d781d1f9"
    "d650a35146b1423370739eefe4ce4375"
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2089
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


def _evaluate_x25519(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_x25519(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_x25519(_load_hmac(runtime))


@pytest.fixture
def loaded_x25519() -> MegaForthRuntime:
    return _load_x25519()


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


def _body_address(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def test_x25519_slice_is_exact_and_publishes_complete_ledger(
    loaded_x25519: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_x25519.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_x25519.find(name) is not None

    for name, following in (
        ("X25519-PRIV", "X25519-PUB"),
        ("X25519-PUB", "X25519-SHARED"),
        ("X25519-SHARED", "X25519-BASE"),
        ("X25519-BASE", "X25519"),
    ):
        word = loaded_x25519.find(name)
        next_word = loaded_x25519.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == X25519_BYTES

    for name in ("X25519-PRIV", "X25519-PUB", "X25519-SHARED"):
        address = _body_address(loaded_x25519, name)
        assert loaded_x25519.memory.read_bytes(address, X25519_BYTES) == bytes(32)
    base_address = _body_address(loaded_x25519, "X25519-BASE")
    assert loaded_x25519.memory.read_bytes(base_address, 32) == BASEPOINT
    assert loaded_x25519.field.accumulator(0) == bytes(32)
    assert loaded_x25519.field.operand_address(0) == 0
    assert loaded_x25519.field.previous_low(0) == 0
    assert loaded_x25519.uart_output == b""


@pytest.mark.parametrize(
    ("scalar", "point", "expected"),
    (
        (RFC_SCALAR_ONE, RFC_POINT_ONE, RFC_RESULT_ONE),
        (RFC_SCALAR_TWO, RFC_POINT_TWO, RFC_RESULT_TWO),
    ),
)
def test_shared_x25519_matches_rfc_7748_vectors(
    scalar: bytes,
    point: bytes,
    expected: bytes,
) -> None:
    assert x25519_scalar_multiply(scalar, point) == expected


def test_shared_x25519_clamps_scalar_and_masks_coordinate_top_bit() -> None:
    scalar_variant = bytearray(RFC_SCALAR_ONE)
    scalar_variant[0] ^= 0x07
    scalar_variant[31] ^= 0xC0
    point_variant = bytearray(RFC_POINT_ONE)
    point_variant[31] ^= 0x80

    assert x25519_scalar_multiply(bytes(scalar_variant), RFC_POINT_ONE) == (
        RFC_RESULT_ONE
    )
    assert x25519_scalar_multiply(RFC_SCALAR_ONE, bytes(point_variant)) == (
        RFC_RESULT_ONE
    )
    with pytest.raises(TypeError, match="scalar must be bytes"):
        x25519_scalar_multiply(bytearray(32), bytes(32))  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="exactly 32"):
        x25519_scalar_multiply(bytes(31), bytes(32))
    with pytest.raises(ValueError, match="exactly 32"):
        x25519_scalar_multiply(bytes(32), bytes(33))


def test_field_service_is_per_core_and_point_reads_are_deferred() -> None:
    memory = SparseAddressSpace(bank0_size=0x1000)
    service = HostedFieldALUService(core_count=2)
    scalar_one = 0x100
    scalar_two = 0x140
    point = 0x180
    memory.write_bytes(scalar_one, RFC_SCALAR_ONE)
    memory.write_bytes(scalar_two, RFC_SCALAR_TWO)
    memory.write_bytes(point, RFC_POINT_ONE)

    service.load_accumulator(0, scalar_one, memory)
    service.load_accumulator(1, scalar_two, memory)
    service.set_operand_address(0, point)
    service.set_operand_address(1, point)

    # POINT! stores only TSRC0. GO must observe the later memory contents.
    memory.write_bytes(point, RFC_POINT_TWO)
    service.x25519(0, memory)
    expected_zero = x25519_scalar_multiply(RFC_SCALAR_ONE, RFC_POINT_TWO)
    assert service.accumulator(0) == expected_zero
    assert service.previous_low(0) == int.from_bytes(expected_zero, "little")
    assert service.accumulator(1) == RFC_SCALAR_TWO
    assert service.previous_low(1) == 0

    service.x25519(1, memory)
    assert service.accumulator(1) == RFC_RESULT_TWO
    assert service.previous_low(1) == int.from_bytes(RFC_RESULT_TWO, "little")


def test_field_service_preserves_sequential_partial_effects_and_resets() -> None:
    memory = SparseAddressSpace(bank0_size=16, external_size=64)
    service = HostedFieldALUService(core_count=2)
    memory.write_bytes(EXTERNAL_BASE, RFC_SCALAR_ONE)
    memory.write_bytes(EXTERNAL_BASE + 32, RFC_POINT_ONE)
    service.load_accumulator(0, EXTERNAL_BASE, memory)

    first_half = bytes(range(0xA0, 0xB0))
    memory.write_bytes(0, first_half)
    with pytest.raises(UnmappedAddressError):
        service.load_accumulator(0, 0, memory)
    partially_loaded = first_half + RFC_SCALAR_ONE[16:]
    assert service.accumulator(0) == partially_loaded

    service.set_operand_address(0, 0)
    with pytest.raises(UnmappedAddressError):
        service.x25519(0, memory)
    assert service.accumulator(0) == partially_loaded
    assert service.previous_low(0) == 0

    memory.fill(0, 16, 0)
    with pytest.raises(UnmappedAddressError):
        service.store_accumulator(0, 0, memory)
    assert memory.read_bytes(0, 16) == partially_loaded[:16]

    service.set_operand_address(0, EXTERNAL_BASE + 32)
    service.x25519(0, memory)
    assert service.previous_low(0) != 0
    service.set_operand_address(1, 0xDEAD_BEEF)
    service.reset()
    for core_id in range(2):
        assert service.accumulator(core_id) == bytes(32)
        assert service.operand_address(core_id) == 0
        assert service.previous_low(core_id) == 0


def test_six_bios_words_preserve_staged_synchronous_contract(
    loaded_x25519: MegaForthRuntime,
) -> None:
    memory = loaded_x25519.memory
    memory.write_bytes(SCALAR_ADDRESS, RFC_SCALAR_ONE)
    memory.write_bytes(POINT_ADDRESS, RFC_POINT_ONE)
    memory.fill(RESULT_ADDRESS, 32, 0xA5)

    assert _execute(loaded_x25519, "X25519-STATUS@") == (2,)
    assert _execute(loaded_x25519, "X25519-SCALAR!", SCALAR_ADDRESS) == ()
    assert loaded_x25519.field.accumulator(0) == RFC_SCALAR_ONE
    assert _execute(loaded_x25519, "X25519-POINT!", POINT_ADDRESS) == ()
    assert loaded_x25519.field.operand_address(0) == POINT_ADDRESS
    assert _execute(loaded_x25519, "X25519-GO") == ()
    assert _execute(loaded_x25519, "X25519-WAIT") == ()
    assert _execute(loaded_x25519, "X25519-STATUS@") == (2,)
    assert _execute(loaded_x25519, "X25519-RESULT@", RESULT_ADDRESS) == ()
    assert memory.read_bytes(RESULT_ADDRESS, 32) == RFC_RESULT_ONE


def test_x25519_has_no_crypto_capability_gate() -> None:
    runtime = _load_x25519(
        MegaForthRuntime(
            memory=create_one_core_address_space(crypto_capabilities=0)
        )
    )
    runtime.memory.write_bytes(SCALAR_ADDRESS, RFC_SCALAR_ONE)
    runtime.memory.write_bytes(POINT_ADDRESS, RFC_POINT_ONE)
    assert _execute(
        runtime,
        "X25519",
        SCALAR_ADDRESS,
        POINT_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert runtime.memory.read_bytes(RESULT_ADDRESS, 32) == RFC_RESULT_ONE


def test_unchanged_x25519_allows_result_to_alias_either_input(
    loaded_x25519: MegaForthRuntime,
) -> None:
    memory = loaded_x25519.memory
    memory.write_bytes(SCALAR_ADDRESS, RFC_SCALAR_ONE)
    memory.write_bytes(POINT_ADDRESS, RFC_POINT_ONE)
    assert _execute(
        loaded_x25519,
        "X25519",
        SCALAR_ADDRESS,
        POINT_ADDRESS,
        SCALAR_ADDRESS,
    ) == ()
    assert memory.read_bytes(SCALAR_ADDRESS, 32) == RFC_RESULT_ONE
    assert memory.read_bytes(POINT_ADDRESS, 32) == RFC_POINT_ONE

    memory.write_bytes(SCALAR_ADDRESS, RFC_SCALAR_ONE)
    memory.write_bytes(POINT_ADDRESS, RFC_POINT_ONE)
    assert _execute(
        loaded_x25519,
        "X25519",
        SCALAR_ADDRESS,
        POINT_ADDRESS,
        POINT_ADDRESS,
    ) == ()
    assert memory.read_bytes(SCALAR_ADDRESS, 32) == RFC_SCALAR_ONE
    assert memory.read_bytes(POINT_ADDRESS, 32) == RFC_RESULT_ONE


def test_unchanged_keygen_consumes_exact_deterministic_private_key() -> None:
    runtime = _load_x25519(
        MegaForthRuntime(
            memory=create_one_core_address_space(entropy_seed=KEYGEN_SEED)
        )
    )
    assert _execute(runtime, "X25519-KEYGEN") == ()

    private = _body_address(runtime, "X25519-PRIV")
    public = _body_address(runtime, "X25519-PUB")
    shared = _body_address(runtime, "X25519-SHARED")
    assert runtime.memory.read_bytes(private, 32) == KEYGEN_PRIVATE
    assert runtime.memory.read_bytes(public, 32) == KEYGEN_PUBLIC
    assert x25519_scalar_multiply(KEYGEN_PRIVATE, BASEPOINT) == KEYGEN_PUBLIC
    assert runtime.memory.read_bytes(shared, 32) == bytes(32)
    assert runtime.entropy.pool_position == 32
    # Clamping is internal to GO; source retains the exact entropy bytes.
    assert KEYGEN_PRIVATE[0] & 0x07


def test_unchanged_dh_uses_retained_private_key(
    loaded_x25519: MegaForthRuntime,
) -> None:
    alice_public = x25519_scalar_multiply(RFC_SCALAR_ONE, BASEPOINT)
    bob_public = x25519_scalar_multiply(RFC_SCALAR_TWO, BASEPOINT)
    shared = x25519_scalar_multiply(RFC_SCALAR_ONE, bob_public)
    assert shared == x25519_scalar_multiply(RFC_SCALAR_TWO, alice_public)

    private_address = _body_address(loaded_x25519, "X25519-PRIV")
    shared_address = _body_address(loaded_x25519, "X25519-SHARED")
    loaded_x25519.memory.write_bytes(private_address, RFC_SCALAR_ONE)
    loaded_x25519.memory.write_bytes(PEER_ADDRESS, bob_public)
    assert _execute(loaded_x25519, "X25519-DH", PEER_ADDRESS) == ()
    assert loaded_x25519.memory.read_bytes(shared_address, 32) == shared
    assert loaded_x25519.memory.read_bytes(private_address, 32) == RFC_SCALAR_ONE
    assert loaded_x25519.memory.read_bytes(PEER_ADDRESS, 32) == bob_public


def test_unchanged_x25519_publishes_all_zero_result_without_rejection(
    loaded_x25519: MegaForthRuntime,
) -> None:
    loaded_x25519.memory.write_bytes(SCALAR_ADDRESS, bytes(32))
    loaded_x25519.memory.write_bytes(POINT_ADDRESS, bytes(32))
    loaded_x25519.memory.fill(RESULT_ADDRESS, 32, 0xA5)
    assert _execute(
        loaded_x25519,
        "X25519",
        SCALAR_ADDRESS,
        POINT_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert loaded_x25519.memory.read_bytes(RESULT_ADDRESS, 32) == bytes(32)


def test_next_contiguous_field_slice_is_now_admitted(
    loaded_x25519: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    assert lines[1481] == b"\n"  # source line 1482
    next_source = b"".join(lines[1482:1515])
    assert next_source.startswith(b"\\ =================================")
    assert next_source.endswith(b"CREATE _FRH 32 ALLOT\n")

    result = loaded_x25519.evaluate(
        next_source,
        source_name="kdos.f:1483-1515",
    )
    assert tuple(word.name for word in result.definitions) == (
        b"PRIME-25519",
        b"PRIME-SECP",
        b"PRIME-P256",
        b"PRIME-CUSTOM",
        b"_FA",
        b"_FB",
        b"_FR",
        b"_FRH",
    )
