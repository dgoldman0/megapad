"""Contiguous unchanged-source acceptance for the KDOS NTT slice."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.ntt import (
    NTT_DEFAULT_MODULUS,
    NTT_DILITHIUM_MODULUS,
    NTT_POLYNOMIAL_BYTES,
    NTT_SIZE,
    NTTRoots,
    find_ntt_roots,
    ntt_forward,
    ntt_inverse,
    ntt_pointwise_add,
    ntt_pointwise_multiply,
)
from simulator.errors import SourceError, StepBudgetExceeded
from simulator.memory import SparseAddressSpace, UnmappedAddressError
from simulator.ntt import (
    HostedNTTService,
    NTT_STATUS_BUSY,
    NTT_STATUS_DONE,
    NTT_STATUS_IDLE,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_field import _load_field
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-ntt-1517-1584.f"

FIRST_LINE = 1517
LAST_LINE = 1584
SLICE_SHA256 = "094749eafb3c51c5ed7e5f9da929e502546c53b9965a948b3056a99775a01e80"
SLICE_GIT_BLOB = "176adf2b63b6cfd5d64f71cdad6837e305c56607"
DEFINITIONS = (
    b"Q-KYBER",
    b"Q-DILITHIUM",
    b"NTT-BUF-A",
    b"NTT-BUF-B",
    b"_NTT-TMP-A",
    b"_NTT-TMP-B",
    b"NTT-POLYMUL",
    b".NTT-STATUS",
)
BIOS_WORDS = (
    "NTT-SETQ",
    "NTT-IDX!",
    "NTT-LOAD",
    "NTT-STORE",
    "NTT-FWD",
    "NTT-INV",
    "NTT-PMUL",
    "NTT-PADD",
    "NTT-STATUS@",
    "NTT-WAIT",
)

POLY_A_ADDRESS = 0x30_000
POLY_B_ADDRESS = 0x30_400
RESULT_ADDRESS = 0x30_800
TEMP_ADDRESS = 0x30_C00


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2773
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


def _evaluate_ntt(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_ntt(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    return _evaluate_ntt(_load_field(runtime))


@pytest.fixture
def loaded_ntt() -> MegaForthRuntime:
    return _load_ntt()


def _poly_bytes(coefficients: tuple[int, ...] | list[int]) -> bytes:
    assert len(coefficients) == NTT_SIZE
    return b"".join(value.to_bytes(4, "little") for value in coefficients)


def _write_poly(
    runtime: MegaForthRuntime,
    address: int,
    coefficients: tuple[int, ...] | list[int],
) -> None:
    runtime.memory.write_bytes(address, _poly_bytes(coefficients))


def _read_poly(runtime: MegaForthRuntime, address: int) -> tuple[int, ...]:
    payload = runtime.memory.read_bytes(address, NTT_POLYNOMIAL_BYTES)
    return tuple(
        int.from_bytes(payload[offset : offset + 4], "little")
        for offset in range(0, len(payload), 4)
    )


def _cyclic_product(
    first: tuple[int, ...],
    second: tuple[int, ...],
    modulus: int,
) -> tuple[int, ...]:
    result = [0] * NTT_SIZE
    for first_index, first_value in enumerate(first):
        for second_index, second_value in enumerate(second):
            result[(first_index + second_index) % NTT_SIZE] += (
                first_value * second_value
            )
    return tuple(value % modulus for value in result)


def test_ntt_slice_is_exact_and_publishes_complete_ledger(
    loaded_ntt: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_ntt.find(name) is not None
    for name in BIOS_WORDS:
        assert loaded_ntt.find(name) is not None

    assert _execute(loaded_ntt, "Q-KYBER") == (NTT_DEFAULT_MODULUS,)
    assert _execute(loaded_ntt, "Q-DILITHIUM") == (
        NTT_DILITHIUM_MODULUS,
    )
    assert _execute(loaded_ntt, "NTT-BUF-A") == (0,)
    assert _execute(loaded_ntt, "NTT-BUF-B") == (1,)

    first = loaded_ntt.find("_NTT-TMP-A")
    second = loaded_ntt.find("_NTT-TMP-B")
    following = loaded_ntt.find("NTT-POLYMUL")
    assert first is not None
    assert second is not None
    assert following is not None
    assert second.header_address - first.body_address == NTT_POLYNOMIAL_BYTES
    assert following.header_address - second.body_address == NTT_POLYNOMIAL_BYTES
    assert loaded_ntt.memory.read_bytes(
        first.body_address,
        NTT_POLYNOMIAL_BYTES,
    ) == bytes(NTT_POLYNOMIAL_BYTES)
    assert loaded_ntt.memory.read_bytes(
        second.body_address,
        NTT_POLYNOMIAL_BYTES,
    ) == bytes(NTT_POLYNOMIAL_BYTES)
    assert loaded_ntt.ntt.status == NTT_STATUS_IDLE
    assert loaded_ntt.uart_output == b""


def test_next_contiguous_frontier_stops_at_kem_select(
    loaded_ntt: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[1584:1608])
    assert next_source.startswith(b"\n")
    assert next_source.endswith(b"    KBUF-SEED KEM-SEL!  64 KEM-LOAD\n")

    with pytest.raises(SourceError, match="unknown word") as caught:
        loaded_ntt.evaluate(next_source, source_name="kdos.f:1585-1608")
    assert caught.value.location.line == 24
    assert caught.value.location.column == 14
    assert caught.value.message == "unknown word b'KEM-SEL!'"


@pytest.mark.parametrize(
    ("modulus", "roots", "prefix"),
    (
        (
            NTT_DEFAULT_MODULUS,
            NTTRoots(3061, 2298, 3316),
            (6, 1881, 3161, 837, 1602, 693, 2647, 835),
        ),
        (
            NTT_DILITHIUM_MODULUS,
            NTTRoots(5801164, 5121960, 8347681),
            (6, 5086702, 5526928, 4065924, 4838850, 1069222, 1768499, 2155199),
        ),
    ),
)
def test_shared_ntt_matches_device_roots_oracles_and_roundtrips(
    modulus: int,
    roots: NTTRoots,
    prefix: tuple[int, ...],
) -> None:
    assert find_ntt_roots(modulus) == roots
    source = (1, 2, 3) + (0,) * (NTT_SIZE - 3)
    transformed = ntt_forward(source, modulus)
    assert transformed[: len(prefix)] == prefix
    assert ntt_inverse(transformed, modulus) == source

    impulse = (1,) + (0,) * (NTT_SIZE - 1)
    assert ntt_forward(impulse, modulus) == (1,) * NTT_SIZE
    assert ntt_inverse((1,) * NTT_SIZE, modulus) == impulse


def test_shared_pointwise_operations_and_unsupported_modulus() -> None:
    first = (NTT_DEFAULT_MODULUS - 1, 2) + (0,) * (NTT_SIZE - 2)
    second = (2, NTT_DEFAULT_MODULUS - 1) + (0,) * (NTT_SIZE - 2)
    assert ntt_pointwise_add(first, second, NTT_DEFAULT_MODULUS)[:2] == (1, 1)
    assert ntt_pointwise_multiply(
        first,
        second,
        NTT_DEFAULT_MODULUS,
    )[:2] == (NTT_DEFAULT_MODULUS - 2, NTT_DEFAULT_MODULUS - 2)
    assert find_ntt_roots(17) is None
    with pytest.raises(ValueError, match="no device-selected"):
        ntt_forward(first, 17)


def test_service_load_selects_exact_zero_for_a_and_reduces_uint32_values() -> None:
    memory = SparseAddressSpace(bank0_size=NTT_POLYNOMIAL_BYTES)
    service = HostedNTTService()
    first = [NTT_DEFAULT_MODULUS + 5] + list(range(1, NTT_SIZE))
    memory.write_bytes(0, _poly_bytes(first))

    service.set_index(0x1_2345)
    assert service.index == 0x2345
    service.load(0, 0, memory)
    assert service.polynomial_a() == tuple(
        value % NTT_DEFAULT_MODULUS for value in first
    )
    assert service.polynomial_b() == (0,) * NTT_SIZE
    assert service.index == 0
    assert service.load_stage(0) == first[-1].to_bytes(4, "little")

    second = [0xFFFF_FFFF] + [7] * (NTT_SIZE - 1)
    memory.write_bytes(0, _poly_bytes(second))
    service.load(0, 7, memory)
    assert service.polynomial_b() == tuple(
        value % NTT_DEFAULT_MODULUS for value in second
    )
    assert service.polynomial_a()[0] == 5
    assert service.load_stage(7) == (7).to_bytes(4, "little")


def test_load_fault_commits_only_complete_coefficients_and_retains_stage() -> None:
    memory = SparseAddressSpace(bank0_size=6)
    memory.write_bytes(0, b"\x05\x00\x00\x00\xaa\xbb")
    service = HostedNTTService()

    with pytest.raises(UnmappedAddressError):
        service.load(0, 0, memory)
    assert service.polynomial_a()[0] == 5
    assert service.polynomial_a()[1:] == (0,) * (NTT_SIZE - 1)
    assert service.index == 1
    assert service.load_stage(0) == b"\xaa\xbb\x00\x00"

    zero_memory = SparseAddressSpace(bank0_size=4)
    zero_memory.write_bytes(0, b"\x01\x02\x03\x04")
    zero_service = HostedNTTService()
    zero_service.set_modulus(0)
    with pytest.raises(ZeroDivisionError):
        zero_service.load(0, 0, zero_memory)
    assert zero_service.load_stage(0) == b"\x01\x02\x03\x04"
    assert zero_service.polynomial_a() == (0,) * NTT_SIZE
    assert zero_service.index == 0


def test_store_fault_advances_index_before_the_fourth_guest_write() -> None:
    destination = 0x800
    memory = SparseAddressSpace(bank0_size=destination + 3)
    first = [1000] + [0] * (NTT_SIZE - 1)
    second = [2000] + [0] * (NTT_SIZE - 1)
    memory.write_bytes(0, _poly_bytes(first))
    memory.write_bytes(NTT_POLYNOMIAL_BYTES, _poly_bytes(second))
    memory.fill(destination, 3, 0xA5)
    service = HostedNTTService()
    service.load(0, 0, memory)
    service.load(NTT_POLYNOMIAL_BYTES, 1, memory)
    service.pointwise_add()

    with pytest.raises(UnmappedAddressError):
        service.store(destination, memory)
    assert memory.read_bytes(destination, 3) == (3000).to_bytes(4, "little")[:3]
    assert service.index == 1
    assert service.status == NTT_STATUS_DONE


def test_ten_bios_words_preserve_status_wait_and_transfer_contract(
    loaded_ntt: MegaForthRuntime,
) -> None:
    assert _execute(loaded_ntt, "NTT-STATUS@") == (NTT_STATUS_IDLE,)
    assert _execute(loaded_ntt, "NTT-IDX!", 0x1_0001) == ()
    assert loaded_ntt.ntt.index == 1
    assert _execute(loaded_ntt, "NTT-SETQ", NTT_DEFAULT_MODULUS) == ()

    with pytest.raises(StepBudgetExceeded) as caught:
        loaded_ntt.execute("NTT-WAIT", step_budget=8)
    assert caught.value.budget == 8
    assert loaded_ntt.main_context.data.snapshot() == ()
    assert loaded_ntt.main_context.returns.snapshot() == ()
    assert loaded_ntt.main_context.reusable

    assert _execute(loaded_ntt, "NTT-FWD") == ()
    assert loaded_ntt.ntt.status == NTT_STATUS_DONE
    assert _execute(loaded_ntt, "NTT-WAIT") == ()
    assert _execute(loaded_ntt, "NTT-STATUS@") == (NTT_STATUS_DONE,)


def test_bios_forward_inverse_and_pointwise_commands(
    loaded_ntt: MegaForthRuntime,
) -> None:
    modulus = NTT_DEFAULT_MODULUS
    first = tuple((index * 17 + 3) % modulus for index in range(NTT_SIZE))
    second = tuple((index * 29 + 5) % modulus for index in range(NTT_SIZE))
    _write_poly(loaded_ntt, POLY_A_ADDRESS, first)
    _write_poly(loaded_ntt, POLY_B_ADDRESS, second)
    assert _execute(loaded_ntt, "NTT-SETQ", modulus) == ()
    assert _execute(loaded_ntt, "NTT-LOAD", POLY_A_ADDRESS, 0) == ()
    assert _execute(loaded_ntt, "NTT-LOAD", POLY_B_ADDRESS, 1) == ()

    assert _execute(loaded_ntt, "NTT-PADD") == ()
    assert _execute(loaded_ntt, "NTT-WAIT") == ()
    assert _execute(loaded_ntt, "NTT-STORE", RESULT_ADDRESS) == ()
    assert _read_poly(loaded_ntt, RESULT_ADDRESS) == ntt_pointwise_add(
        first,
        second,
        modulus,
    )

    assert _execute(loaded_ntt, "NTT-PMUL") == ()
    assert _execute(loaded_ntt, "NTT-STORE", RESULT_ADDRESS) == ()
    assert _read_poly(loaded_ntt, RESULT_ADDRESS) == ntt_pointwise_multiply(
        first,
        second,
        modulus,
    )

    assert _execute(loaded_ntt, "NTT-FWD") == ()
    assert _execute(loaded_ntt, "NTT-STORE", TEMP_ADDRESS) == ()
    assert _read_poly(loaded_ntt, TEMP_ADDRESS) == ntt_forward(first, modulus)
    assert _execute(loaded_ntt, "NTT-LOAD", TEMP_ADDRESS, 0) == ()
    assert _execute(loaded_ntt, "NTT-INV") == ()
    assert _execute(loaded_ntt, "NTT-STORE", RESULT_ADDRESS) == ()
    assert _read_poly(loaded_ntt, RESULT_ADDRESS) == first
    assert loaded_ntt.ntt.index == 0


def test_unchanged_ntt_polymul_is_cyclic_and_output_may_alias_inputs(
    loaded_ntt: MegaForthRuntime,
) -> None:
    first_values = [0] * NTT_SIZE
    second_values = [0] * NTT_SIZE
    first_values[0] = 3
    first_values[1] = 2
    first_values[NTT_SIZE - 1] = 1
    second_values[0] = 5
    second_values[1] = 1
    second_values[2] = 7
    first = tuple(first_values)
    second = tuple(second_values)
    expected = _cyclic_product(first, second, NTT_DEFAULT_MODULUS)
    # In particular, x^255 * x wraps to +1 in the ordinary cyclic ring.
    assert expected[0] == first[0] * second[0] + 1
    assert _execute(loaded_ntt, "NTT-SETQ", NTT_DEFAULT_MODULUS) == ()

    for output in (RESULT_ADDRESS, POLY_A_ADDRESS, POLY_B_ADDRESS):
        _write_poly(loaded_ntt, POLY_A_ADDRESS, first)
        _write_poly(loaded_ntt, POLY_B_ADDRESS, second)
        assert _execute(
            loaded_ntt,
            "NTT-POLYMUL",
            POLY_A_ADDRESS,
            POLY_B_ADDRESS,
            output,
        ) == ()
        assert _read_poly(loaded_ntt, output) == expected


def test_ntt_status_renders_all_retained_status_bit_patterns(
    loaded_ntt: MegaForthRuntime,
) -> None:
    states = (
        (False, False, NTT_STATUS_IDLE, b" NTT: idle\r\n"),
        (True, False, NTT_STATUS_BUSY, b" NTT: busy\r\n"),
        (False, True, NTT_STATUS_DONE, b" NTT: done\r\n"),
        (True, True, 3, b" NTT: unknown\r\n"),
    )
    for busy, done, status, expected in states:
        # Commands are synchronous in the hosted backend, so inject the two
        # retained bits to exercise every unchanged-source diagnostic branch.
        loaded_ntt.ntt._busy = busy
        loaded_ntt.ntt._done = done
        assert loaded_ntt.ntt.status == status
        assert _execute(loaded_ntt, ".NTT-STATUS") == ()
        assert loaded_ntt.drain_uart_output() == expected


def test_unsupported_root_finishes_without_result_change_and_reset_is_total() -> None:
    memory = SparseAddressSpace(bank0_size=NTT_POLYNOMIAL_BYTES)
    memory.write_bytes(0, _poly_bytes([9] + [0] * (NTT_SIZE - 1)))
    service = HostedNTTService()
    service.load(0, 0, memory)
    service.forward()
    retained_result = service.result()
    retained_a = service.polynomial_a()
    assert retained_result == (9,) * NTT_SIZE
    assert service.status == NTT_STATUS_DONE

    service.set_modulus(17)
    assert service.roots is None
    assert service.status == NTT_STATUS_DONE
    assert service.result() == retained_result
    assert service.polynomial_a() == retained_a
    service.forward()
    assert service.status == NTT_STATUS_DONE
    assert service.result() == retained_result

    service.set_index(0xFFFF)
    service.reset()
    assert service.modulus == NTT_DEFAULT_MODULUS
    assert service.roots == find_ntt_roots(NTT_DEFAULT_MODULUS)
    assert service.index == 0
    assert service.status == NTT_STATUS_IDLE
    assert service.polynomial_a() == (0,) * NTT_SIZE
    assert service.polynomial_b() == (0,) * NTT_SIZE
    assert service.result() == (0,) * NTT_SIZE
    assert service.load_stage(0) == bytes(4)
    assert service.load_stage(1) == bytes(4)
