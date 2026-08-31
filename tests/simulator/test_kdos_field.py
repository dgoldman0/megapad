"""Contiguous unchanged-source acceptance for the KDOS Field-ALU slice."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.field import (
    BUILTIN_PRIMES,
    FIELD_BYTES,
    FIELD_MASK,
    MONTGOMERY_RADIX,
    PRIME_25519,
    PRIME_P256,
    PRIME_SECP256K1,
    active_prime,
    field_add,
    field_inverse,
    field_multiply,
    field_power,
    field_square,
    field_subtract,
    montgomery_multiply,
    raw_multiply_add,
    raw_product,
)
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
from tests.simulator.test_kdos_x25519 import (
    RFC_POINT_ONE,
    RFC_RESULT_ONE,
    RFC_SCALAR_ONE,
    _execute,
    _load_x25519,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-field-1483-1515.f"

FIRST_LINE = 1483
LAST_LINE = 1515
SLICE_SHA256 = "c21426c25b912423ff9ad3a0d32aafb9910c2612416a394c4fea4f8388e96b9c"
SLICE_GIT_BLOB = "878fb49b86ff3dc888d394100b1f48389411472e"
DEFINITIONS = (
    b"PRIME-25519",
    b"PRIME-SECP",
    b"PRIME-P256",
    b"PRIME-CUSTOM",
    b"_FA",
    b"_FB",
    b"_FR",
    b"_FRH",
)
BIOS_WORDS = (
    "GF-A!",
    "GF-R@",
    "GF-PRIME",
    "LOAD-PRIME",
    "FADD",
    "FSUB",
    "FMUL",
    "FSQR",
    "FINV",
    "FPOW",
    "FMUL-RAW",
    "FCMOV",
    "FCEQ",
    "FMAC",
    "FMUL-ADD-RAW",
)

A_ADDRESS = 0x2B_000
B_ADDRESS = 0x2B_040
RESULT_ADDRESS = 0x2B_080
HIGH_ADDRESS = 0x2B_0C0
PRIME_ADDRESS = 0x2B_100
INVERSE_ADDRESS = 0x2B_140
CONDITION_ADDRESS = 0x2B_180
X_RESULT_ADDRESS = 0x2B_1C0


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 1403
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


def _evaluate_field(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_field(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_field(_load_x25519(runtime))


@pytest.fixture
def loaded_field() -> MegaForthRuntime:
    return _load_field()


def _write_u256(runtime: MegaForthRuntime, address: int, value: int) -> None:
    assert 0 <= value <= FIELD_MASK
    runtime.memory.write_bytes(address, value.to_bytes(FIELD_BYTES, "little"))


def _read_u256(runtime: MegaForthRuntime, address: int) -> int:
    return int.from_bytes(
        runtime.memory.read_bytes(address, FIELD_BYTES),
        "little",
    )


def test_field_slice_is_exact_and_publishes_complete_ledger(
    loaded_field: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_field.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_field.find(name) is not None

    assert _execute(loaded_field, "PRIME-25519") == ()
    assert loaded_field.field.prime_selection(0) == 0
    assert _execute(loaded_field, "PRIME-SECP") == ()
    assert loaded_field.field.prime_selection(0) == 1
    assert _execute(loaded_field, "PRIME-P256") == ()
    assert loaded_field.field.prime_selection(0) == 2
    assert _execute(loaded_field, "PRIME-CUSTOM") == ()
    assert loaded_field.field.prime_selection(0) == 3

    for name, following in zip(DEFINITIONS[4:], DEFINITIONS[5:]):
        word = loaded_field.find(name)
        next_word = loaded_field.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == FIELD_BYTES
        assert loaded_field.memory.read_bytes(word.body_address, FIELD_BYTES) == (
            bytes(FIELD_BYTES)
        )
    final = loaded_field.find("_FRH")
    assert final is not None
    assert loaded_field.dictionary.here - final.body_address == FIELD_BYTES
    assert loaded_field.memory.read_bytes(final.body_address, FIELD_BYTES) == (
        bytes(FIELD_BYTES)
    )
    assert loaded_field.uart_output == b""


def test_shared_field_values_cover_builtin_and_native_edge_semantics() -> None:
    assert BUILTIN_PRIMES == (PRIME_25519, PRIME_SECP256K1, PRIME_P256)
    assert active_prime(0, 0) == PRIME_25519
    assert active_prime(1, 0) == PRIME_SECP256K1
    assert active_prime(2, 0) == PRIME_P256
    assert active_prime(3, 251) == 251
    assert active_prime(3, 0) == PRIME_25519

    assert field_add(PRIME_25519 - 1, 1, PRIME_25519) == 0
    assert field_subtract(5, 12, PRIME_25519) == PRIME_25519 - 7
    assert field_multiply(123, 456, PRIME_25519) == 123 * 456
    assert field_square(123, PRIME_25519) == 123 * 123
    assert field_inverse(5, PRIME_25519) * 5 % PRIME_25519 == 1
    assert field_power(7, 13, PRIME_25519) == pow(7, 13, PRIME_25519)

    # The C++/RTL ALU assumes canonical addends and subtracts p only once.
    # This deliberately differs from the Python emulator's full `% p` here.
    assert field_add(20, 20, 17) == 23
    assert field_subtract(3, 40, 17) == (17 - 37) & FIELD_MASK


def test_shared_montgomery_and_raw_results_include_low_to_high_carry() -> None:
    prime = 251
    inverse = (-pow(prime, -1, MONTGOMERY_RADIX)) % MONTGOMERY_RADIX
    first = 11 * MONTGOMERY_RADIX % prime
    second = 17 * MONTGOMERY_RADIX % prime
    assert montgomery_multiply(first, second, prime, inverse) == (
        11 * 17 * MONTGOMERY_RADIX % prime
    )

    assert raw_product(FIELD_MASK, FIELD_MASK) == (1, FIELD_MASK - 1)
    assert raw_multiply_add(FIELD_MASK, 1, 1, 0) == (0, 1)


def test_bios_accumulator_transfer_and_custom_prime_latch(
    loaded_field: MegaForthRuntime,
) -> None:
    value = int.from_bytes(bytes(range(FIELD_BYTES)), "little")
    _write_u256(loaded_field, A_ADDRESS, value)
    loaded_field.memory.fill(RESULT_ADDRESS, FIELD_BYTES, 0xA5)

    assert _execute(loaded_field, "GF-A!", A_ADDRESS) == ()
    assert loaded_field.field.accumulator(0) == bytes(range(FIELD_BYTES))
    assert _execute(loaded_field, "GF-R@", RESULT_ADDRESS) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == value

    prime = 251
    inverse = (-pow(prime, -1, MONTGOMERY_RADIX)) % MONTGOMERY_RADIX
    _write_u256(loaded_field, PRIME_ADDRESS, prime)
    _write_u256(loaded_field, INVERSE_ADDRESS, inverse)
    assert _execute(loaded_field, "PRIME-P256") == ()
    assert _execute(
        loaded_field,
        "LOAD-PRIME",
        PRIME_ADDRESS,
        INVERSE_ADDRESS,
    ) == ()
    assert loaded_field.field.custom_prime(0) == prime
    assert loaded_field.field.montgomery_inverse(0) == inverse
    assert loaded_field.field.prime_selection(0) == 2
    assert loaded_field.field.accumulator(0) == prime.to_bytes(32, "little")
    assert loaded_field.field.operand_address(0) == INVERSE_ADDRESS

    assert _execute(loaded_field, "GF-PRIME", 7) == ()
    assert loaded_field.field.prime_selection(0) == 3


@pytest.mark.parametrize(
    ("name", "first", "second", "expected"),
    (
        ("FADD", 12, 30, 42),
        ("FSUB", 5, 12, PRIME_25519 - 7),
        ("FMUL", 123, 456, 123 * 456),
        ("FPOW", 7, 13, pow(7, 13, PRIME_25519)),
    ),
)
def test_binary_bios_words_use_address_operands(
    loaded_field: MegaForthRuntime,
    name: str,
    first: int,
    second: int,
    expected: int,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, first)
    _write_u256(loaded_field, B_ADDRESS, second)
    loaded_field.memory.fill(RESULT_ADDRESS, FIELD_BYTES, 0xA5)
    assert _execute(
        loaded_field,
        name,
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == expected
    assert loaded_field.field.previous_low(0) == expected


@pytest.mark.parametrize(
    ("name", "value", "expected"),
    (
        ("FSQR", 123, 123 * 123),
        ("FINV", 5, pow(5, PRIME_25519 - 2, PRIME_25519)),
    ),
)
def test_unary_bios_words_use_address_operands(
    loaded_field: MegaForthRuntime,
    name: str,
    value: int,
    expected: int,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, value)
    assert _execute(
        loaded_field,
        name,
        A_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == expected


def test_only_multiply_and_square_use_custom_montgomery_mode(
    loaded_field: MegaForthRuntime,
) -> None:
    prime = 251
    inverse = (-pow(prime, -1, MONTGOMERY_RADIX)) % MONTGOMERY_RADIX
    first = 11 * MONTGOMERY_RADIX % prime
    second = 17 * MONTGOMERY_RADIX % prime
    _write_u256(loaded_field, PRIME_ADDRESS, prime)
    _write_u256(loaded_field, INVERSE_ADDRESS, inverse)
    _write_u256(loaded_field, A_ADDRESS, first)
    _write_u256(loaded_field, B_ADDRESS, second)
    assert _execute(
        loaded_field,
        "LOAD-PRIME",
        PRIME_ADDRESS,
        INVERSE_ADDRESS,
    ) == ()
    assert _execute(loaded_field, "PRIME-CUSTOM") == ()

    assert _execute(
        loaded_field,
        "FMUL",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == (
        11 * 17 * MONTGOMERY_RADIX % prime
    )
    assert _execute(
        loaded_field,
        "FSQR",
        A_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == (
        11 * 11 * MONTGOMERY_RADIX % prime
    )

    assert _execute(
        loaded_field,
        "FINV",
        A_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == pow(
        first,
        prime - 2,
        prime,
    )


def test_raw_multiply_mac_and_raw_mac_share_previous_result(
    loaded_field: MegaForthRuntime,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, 3)
    _write_u256(loaded_field, B_ADDRESS, 5)
    assert _execute(
        loaded_field,
        "FMUL-RAW",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
        HIGH_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == 15
    assert _read_u256(loaded_field, HIGH_ADDRESS) == 0

    _write_u256(loaded_field, A_ADDRESS, 7)
    _write_u256(loaded_field, B_ADDRESS, 11)
    assert _execute(
        loaded_field,
        "FMAC",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == 92

    _write_u256(loaded_field, A_ADDRESS, FIELD_MASK)
    _write_u256(loaded_field, B_ADDRESS, 1)
    assert _execute(
        loaded_field,
        "FMUL-RAW",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
        HIGH_ADDRESS,
    ) == ()
    _write_u256(loaded_field, A_ADDRESS, 1)
    assert _execute(
        loaded_field,
        "FMUL-ADD-RAW",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
        HIGH_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == 0
    assert _read_u256(loaded_field, HIGH_ADDRESS) == 1
    assert loaded_field.field.previous_low(0) == 0
    assert loaded_field.field.previous_high(0) == 1


def test_raw_low_and_high_destinations_may_alias(
    loaded_field: MegaForthRuntime,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, FIELD_MASK)
    _write_u256(loaded_field, B_ADDRESS, FIELD_MASK)
    assert _execute(
        loaded_field,
        "FMUL-RAW",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    low, high = raw_product(FIELD_MASK, FIELD_MASK)
    assert _read_u256(loaded_field, RESULT_ADDRESS) == low
    assert loaded_field.field.previous_low(0) == low
    assert loaded_field.field.previous_high(0) == high


def test_conditional_move_and_equality_preserve_the_hardware_data_contract(
    loaded_field: MegaForthRuntime,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, 2)
    _write_u256(loaded_field, B_ADDRESS, 3)
    assert _execute(
        loaded_field,
        "FADD",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert loaded_field.field.previous_low(0) == 5

    _write_u256(loaded_field, A_ADDRESS, 42)
    _write_u256(loaded_field, B_ADDRESS, 99)
    assert _execute(loaded_field, "GF-A!", A_ADDRESS) == ()
    loaded_field.memory.write8(CONDITION_ADDRESS, 0)
    assert _execute(
        loaded_field,
        "FCMOV",
        B_ADDRESS,
        CONDITION_ADDRESS,
    ) == ()
    assert int.from_bytes(loaded_field.field.accumulator(0), "little") == 42
    assert loaded_field.field.previous_low(0) == 5

    loaded_field.memory.write8(CONDITION_ADDRESS, 0x80)
    assert _execute(
        loaded_field,
        "FCMOV",
        B_ADDRESS,
        CONDITION_ADDRESS,
    ) == ()
    assert int.from_bytes(loaded_field.field.accumulator(0), "little") == 99
    assert loaded_field.field.previous_low(0) == 99

    for right, expected in ((42, 1), (43, 0)):
        _write_u256(loaded_field, A_ADDRESS, 42)
        _write_u256(loaded_field, B_ADDRESS, right)
        assert _execute(
            loaded_field,
            "FCEQ",
            A_ADDRESS,
            B_ADDRESS,
            RESULT_ADDRESS,
        ) == ()
        assert _read_u256(loaded_field, RESULT_ADDRESS) == expected
        assert loaded_field.field.previous_low(0) == expected
        # This is the raw GF.CEQ diagnostic.  BIOS result stores overwrite
        # architectural flags before the Forth word returns.
        assert loaded_field.field.zero_flag(0) is bool(expected)


def test_false_conditional_move_still_reads_operand(
    loaded_field: MegaForthRuntime,
) -> None:
    loaded_field.memory.write8(CONDITION_ADDRESS, 0)
    context = loaded_field.main_context
    context.data.push(0xDEAD_BEEF)
    context.data.push(CONDITION_ADDRESS)
    with pytest.raises(UnmappedAddressError):
        loaded_field.execute("FCMOV", step_budget=250_000)
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert loaded_field.field.operand_address(0) == 0xDEAD_BEEF


def test_binary_results_can_alias_either_complete_input(
    loaded_field: MegaForthRuntime,
) -> None:
    _write_u256(loaded_field, A_ADDRESS, 123)
    _write_u256(loaded_field, B_ADDRESS, 456)
    assert _execute(
        loaded_field,
        "FMUL",
        A_ADDRESS,
        B_ADDRESS,
        A_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, A_ADDRESS) == 123 * 456
    assert _read_u256(loaded_field, B_ADDRESS) == 456

    _write_u256(loaded_field, A_ADDRESS, 123)
    _write_u256(loaded_field, B_ADDRESS, 456)
    assert _execute(
        loaded_field,
        "FMUL",
        A_ADDRESS,
        B_ADDRESS,
        B_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, A_ADDRESS) == 123
    assert _read_u256(loaded_field, B_ADDRESS) == 123 * 456


def test_raw_high_fault_keeps_acc_but_not_previous_result() -> None:
    memory = SparseAddressSpace(bank0_size=0x80)
    service = HostedFieldALUService(core_count=1)
    memory.write_bytes(0x00, FIELD_MASK.to_bytes(32, "little"))
    memory.write_bytes(0x20, FIELD_MASK.to_bytes(32, "little"))
    service.load_accumulator(0, 0x00, memory)
    service.set_operand_address(0, 0x20)
    service.set_result_address(0, 0x70)

    with pytest.raises(UnmappedAddressError):
        service.multiply_raw(0, memory)
    low, high = raw_product(FIELD_MASK, FIELD_MASK)
    assert int.from_bytes(service.accumulator(0), "little") == low
    assert memory.read_bytes(0x70, 16) == high.to_bytes(32, "little")[:16]
    assert service.previous_low(0) == 0
    assert service.previous_high(0) == 0


def test_raw_low_fault_follows_high_and_previous_commit() -> None:
    memory = create_one_core_address_space(external_size=0x100)
    runtime = _load_field(MegaForthRuntime(memory=memory))
    _write_u256(runtime, A_ADDRESS, FIELD_MASK)
    _write_u256(runtime, B_ADDRESS, FIELD_MASK)
    low_address = EXTERNAL_BASE + 0xF0
    high_address = EXTERNAL_BASE + 0x80
    memory.fill(low_address, 16, 0xA5)
    memory.fill(high_address, 32, 0xA5)
    context = runtime.main_context
    for value in (A_ADDRESS, B_ADDRESS, low_address, high_address):
        context.data.push(value)

    with pytest.raises(UnmappedAddressError):
        runtime.execute("FMUL-RAW", step_budget=250_000)
    low, high = raw_product(FIELD_MASK, FIELD_MASK)
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert memory.read_bytes(high_address, 32) == high.to_bytes(32, "little")
    assert memory.read_bytes(low_address, 16) == low.to_bytes(32, "little")[:16]
    assert int.from_bytes(runtime.field.accumulator(0), "little") == low
    assert runtime.field.previous_low(0) == low
    assert runtime.field.previous_high(0) == high


def test_custom_prime_latches_before_a_deferred_inverse_fault() -> None:
    memory = SparseAddressSpace(bank0_size=0x80)
    service = HostedFieldALUService(core_count=1)
    memory.write_bytes(0x00, (251).to_bytes(32, "little"))
    service.load_accumulator(0, 0x00, memory)
    service.set_operand_address(0, 0x70)

    with pytest.raises(UnmappedAddressError):
        service.latch_custom_prime(0, memory)
    assert service.custom_prime(0) == 251
    assert service.montgomery_inverse(0) == 0


def test_x25519_and_field_mac_share_one_per_core_previous_result(
    loaded_field: MegaForthRuntime,
) -> None:
    loaded_field.memory.write_bytes(A_ADDRESS, RFC_SCALAR_ONE)
    loaded_field.memory.write_bytes(B_ADDRESS, RFC_POINT_ONE)
    assert _execute(
        loaded_field,
        "X25519",
        A_ADDRESS,
        B_ADDRESS,
        X_RESULT_ADDRESS,
    ) == ()
    assert loaded_field.memory.read_bytes(X_RESULT_ADDRESS, 32) == RFC_RESULT_ONE
    previous = int.from_bytes(RFC_RESULT_ONE, "little")
    assert loaded_field.field.previous_low(0) == previous

    _write_u256(loaded_field, A_ADDRESS, 1)
    _write_u256(loaded_field, B_ADDRESS, 1)
    assert _execute(loaded_field, "PRIME-25519") == ()
    assert _execute(
        loaded_field,
        "FMAC",
        A_ADDRESS,
        B_ADDRESS,
        RESULT_ADDRESS,
    ) == ()
    assert _read_u256(loaded_field, RESULT_ADDRESS) == field_add(
        1,
        previous,
        PRIME_25519,
    )


def test_service_reset_clears_all_per_core_field_state() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)
    service = HostedFieldALUService(core_count=2)
    memory.write_bytes(0, (251).to_bytes(32, "little"))
    memory.write_bytes(32, (17).to_bytes(32, "little"))
    service.load_accumulator(1, 0, memory)
    service.set_operand_address(1, 32)
    service.set_result_address(1, 64)
    service.select_prime(1, 3)
    service.latch_custom_prime(1, memory)
    service.add(1, memory)
    service.equal(1, memory)
    service.reset()

    for core_id in range(2):
        assert service.accumulator(core_id) == bytes(32)
        assert service.operand_address(core_id) == 0
        assert service.result_address(core_id) == 0
        assert service.prime_selection(core_id) == 0
        assert service.custom_prime(core_id) == 0
        assert service.montgomery_inverse(core_id) == 0
        assert service.previous_low(core_id) == 0
        assert service.previous_high(core_id) == 0
        assert service.zero_flag(core_id) is False
