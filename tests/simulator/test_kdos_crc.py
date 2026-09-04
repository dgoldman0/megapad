"""Contiguous unchanged-source acceptance for KDOS CRC support."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE, u64
from simulator.crc import (
    CRC_STATUS_OK,
    CRC_STATUS_RANGE,
    CRC_STATUS_STATE,
    CRC_STATUS_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.memory import (
    MMIO_BASE,
    AddressClass,
    CrossRegionAccessError,
    MMIOAccessError,
    SparseAddressSpace,
    UnmappedAddressError,
)
from simulator.platform import (
    SYSINFO_CRYPTO_CAPS,
    SYSINFO_NUM_CORES,
    SYSINFO_NUM_FULL,
    create_one_core_address_space,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_dictionary_task_hooks import _load_hooks


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-crc-720-855.f"

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"
FIRST_LINE = 720
LAST_LINE = 855
SLICE_SHA256 = "5c6853fab0b95696d8e9a682c74e2d740dcaa84ebcd01283c66c9417ac3d0aa8"
SLICE_GIT_BLOB = "1fa770d5ab3a7da722ed4daa0181c923f07d2286"
DEFINITIONS = (
    b"_CRC-REQUIRE-OK",
    b"_CRC-BUF-CHECKED",
    b"CRC-BUF",
    b"CRC32-BUF",
    b"CRC32C-BUF",
    b"CRC64-BUF",
    b"CRC32-STR",
    b".CRC32",
    b"_CRC-DIAG-DATA",
    b"_CRC-DIAG-RAW",
    b"_CRC-DIAG-EXPECT",
    b"_CRC-DIAG-RUN?",
    b"_CRC-DIAG-ONE",
    b"CRC-DIAG?",
    b".CRC-DIAG",
)

EXPECTED_123456789 = {
    0: 0xFC891918,
    1: 0x05440F15,
    2: 0x62EC59E3F1A4F00A,
    4: 0xCBF43926,
    5: 0xE3069283,
    6: 0x995DC9BBDF1939FA,
}
ORACLE_MODES = {
    0: (0x04C11DB7, 32, False),
    1: (0x1EDC6F41, 32, False),
    2: (0x42F0E1EBA9EA3693, 64, False),
    4: (0xEDB88320, 32, True),
    5: (0x82F63B78, 32, True),
    6: (0xC96C5795D7870F42, 64, True),
}


class _SysInfoProfileMMIO:
    def __init__(
        self,
        capabilities: int,
        *,
        num_cores: int = 1,
        num_full: int = 1,
        reject: bool = False,
    ) -> None:
        self.values = {
            SYSINFO_NUM_CORES: num_cores,
            SYSINFO_NUM_FULL: num_full,
            SYSINFO_CRYPTO_CAPS: capabilities,
        }
        self.reject = reject

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        if self.reject:
            raise RuntimeError("rejected SysInfo profile")
        if write or offset not in self.values or width != 8:
            raise RuntimeError("access is outside the admitted SysInfo qwords")

    def read8(self, offset: int) -> int:
        base = next(
            base
            for base in self.values
            if base <= offset < base + 8
        )
        shift = (offset - base) * 8
        return (self.values[base] >> shift) & 0xFF

    def write8(self, _offset: int, _value: int) -> None:
        raise RuntimeError("capability profile is read-only")


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


def _evaluate_crc_slice(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_crc(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    runtime, _bios_words = _load_hooks(runtime)
    return _evaluate_crc_slice(runtime)


@pytest.fixture
def loaded_crc() -> MegaForthRuntime:
    return _load_crc()


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


def _allocate_bytes(
    runtime: MegaForthRuntime,
    name: str,
    payload: bytes,
    *,
    leading_pad: int = 0,
) -> int:
    runtime.evaluate(
        b"CREATE "
        + name.encode("ascii")
        + b" "
        + str(len(payload) + leading_pad).encode("ascii")
        + b" ALLOT"
    )
    word = runtime.find(name)
    assert word is not None
    address = word.body_address + leading_pad
    runtime.memory.write_bytes(address, payload)
    return address


def _oracle_crc(data: bytes, mode: int, *, seed: int | None = None) -> int:
    polynomial, width, reflected = ORACLE_MODES[mode]
    mask = MASK64 if width == 64 else 0xFFFF_FFFF
    accumulator = mask if seed is None else seed & mask
    for byte in data:
        if reflected:
            accumulator ^= byte
            for _ in range(8):
                accumulator = (
                    (accumulator >> 1) ^ polynomial
                    if accumulator & 1
                    else accumulator >> 1
                )
        else:
            accumulator ^= byte << (width - 8)
            for _ in range(8):
                accumulator = (
                    ((accumulator << 1) & mask) ^ polynomial
                    if accumulator & (1 << (width - 1))
                    else (accumulator << 1) & mask
                )
        accumulator &= mask
    return accumulator ^ mask


def test_crc_slice_is_exact_and_publishes_the_complete_definition_ledger(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    data = runtime.find("_CRC-DIAG-DATA")
    raw = runtime.find("_CRC-DIAG-RAW")
    expected = runtime.find("_CRC-DIAG-EXPECT")
    assert data is not None
    assert raw is not None
    assert expected is not None
    assert runtime.memory.read_bytes(data.body_address, 9) == b"123456789"
    assert runtime.memory.read64(raw.body_address) == 0
    assert runtime.memory.read64(expected.body_address) == 0


def test_crc_capability_word_and_sysinfo_are_one_coherent_profile(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    assert _execute(runtime, "CRYPTO-CAPS@") == (7,)
    assert runtime.memory.read64(MMIO_BASE + SYSINFO_CRYPTO_CAPS) == 7
    assert runtime.crc.capabilities == 1


def test_runtime_fails_closed_without_one_admitted_sysinfo_profile() -> None:
    missing = SparseAddressSpace()
    with pytest.raises(MMIOAccessError, match="no MMIO service"):
        MegaForthRuntime(memory=missing)

    rejected = SparseAddressSpace(mmio=_SysInfoProfileMMIO(0, reject=True))
    with pytest.raises(MMIOAccessError, match="rejected read preflight"):
        MegaForthRuntime(memory=rejected)

    unsupported = SparseAddressSpace(mmio=_SysInfoProfileMMIO(8))
    with pytest.raises(ValueError, match="unknown crypto capabilities"):
        MegaForthRuntime(memory=unsupported)

    sysinfo_only = SparseAddressSpace(mmio=_SysInfoProfileMMIO(0))
    with pytest.raises(ValueError, match="one-core platform MMIO"):
        MegaForthRuntime(memory=sysinfo_only)

    zero_full = SparseAddressSpace(
        mmio=_SysInfoProfileMMIO(0, num_full=0)
    )
    with pytest.raises(ValueError, match="one advertised full core"):
        MegaForthRuntime(memory=zero_full)

    multicore = SparseAddressSpace(
        mmio=_SysInfoProfileMMIO(0, num_cores=2, num_full=1)
    )
    with pytest.raises(ValueError, match="one advertised full core"):
        MegaForthRuntime(memory=multicore)


def test_crc_mode_validation_has_range_then_capability_then_owner_priority() -> None:
    runtime = MegaForthRuntime()
    initial = (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner)
    for mode in (3, 7, 0xFF, 0x1_0000_0003):
        assert _execute(runtime, "CRC-MODE!", mode) == (CRC_STATUS_RANGE,)
        assert (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner) == (
            initial
        )

    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-INIT!", 0x12345678) == (CRC_STATUS_OK,)
    owned = (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner)
    assert _execute(runtime, "CRC-MODE!", 3) == (CRC_STATUS_RANGE,)
    assert _execute(runtime, "CRC-MODE!", 5) == (CRC_STATUS_STATE,)
    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_STATE,)
    assert (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner) == owned
    assert _execute(runtime, "CRC-FINAL@") == (0xEDCBA987,)
    released_accumulator = runtime.crc.accumulator
    assert _execute(runtime, "CRC-MODE!", 2) == (CRC_STATUS_OK,)
    assert runtime.crc.accumulator == released_accumulator
    _execute(runtime, "CRC-FINAL@")


def test_crc_primitives_consume_inputs_and_report_unowned_state_without_mutation() -> None:
    runtime = MegaForthRuntime()
    initial = (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner)
    assert _execute(runtime, "CRC-RESET") == (CRC_STATUS_STATE,)
    assert _execute(runtime, "CRC-INIT!", 0x1234) == (CRC_STATUS_STATE,)
    assert _execute(runtime, "CRC-FEED", 0x0102030405060708) == (
        CRC_STATUS_STATE,
    )
    assert _execute(runtime, "CRC-FEED-BYTE", 0x141) == (CRC_STATUS_STATE,)
    assert _execute(runtime, "CRC@") == (0, CRC_STATUS_STATE)
    assert _execute(runtime, "CRC-RAW-FINAL@") == (0, CRC_STATUS_STATE)
    assert _execute(runtime, "CRC-FINAL@") == (0,)
    assert (runtime.crc.mode, runtime.crc.accumulator, runtime.crc.owner) == initial


def test_crc_seed_reset_and_mode_widths_match_the_checked_bios() -> None:
    runtime = MegaForthRuntime()
    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-INIT!", 0xDEADBEEF89ABCDEF) == (
        CRC_STATUS_OK,
    )
    assert _execute(runtime, "CRC@") == (0x89ABCDEF, CRC_STATUS_OK)
    assert _execute(runtime, "CRC-RESET") == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC@") == (0xFFFFFFFF, CRC_STATUS_OK)
    _execute(runtime, "CRC-FINAL@")

    assert _execute(runtime, "CRC-MODE!", 2) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-INIT!", 0xDEADBEEF89ABCDEF) == (
        CRC_STATUS_OK,
    )
    assert _execute(runtime, "CRC@") == (
        0xDEADBEEF89ABCDEF,
        CRC_STATUS_OK,
    )
    _execute(runtime, "CRC-FINAL@")


def test_crc_mode_change_preserves_and_fetches_the_complete_accumulator() -> None:
    runtime = MegaForthRuntime()
    seed = 0x0123456789ABCDEF
    finalized = seed ^ MASK64

    assert _execute(runtime, "CRC-MODE!", 2) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-INIT!", seed) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-FINAL@") == (finalized,)

    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC@") == (finalized, CRC_STATUS_OK)
    _execute(runtime, "CRC-FINAL@")


def test_all_six_modes_match_pinned_standard_vectors_through_bios_words() -> None:
    runtime = MegaForthRuntime()
    for mode, expected in EXPECTED_123456789.items():
        assert _execute(runtime, "CRC-MODE!", mode) == (CRC_STATUS_OK,)
        assert _execute(runtime, "CRC-RESET") == (CRC_STATUS_OK,)
        for byte in b"123456789":
            assert _execute(runtime, "CRC-FEED-BYTE", byte) == (CRC_STATUS_OK,)
        assert _execute(runtime, "CRC-FINAL@") == (expected,)


def test_quad_plus_tail_is_little_endian_and_raw_final_releases_atomically() -> None:
    runtime = MegaForthRuntime()
    assert _execute(runtime, "CRC-MODE!", 5) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-RESET") == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-FEED", 0x3837363534333231) == (
        CRC_STATUS_OK,
    )
    assert _execute(runtime, "CRC-FEED-BYTE", ord("9")) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC@") == (0x1CF96D7C, CRC_STATUS_OK)
    assert _execute(runtime, "CRC-RAW-FINAL@") == (
        0x1CF96D7C,
        CRC_STATUS_OK,
    )
    assert runtime.crc.owner is None
    assert _execute(runtime, "CRC@") == (0, CRC_STATUS_STATE)


def test_host_scratch_contexts_share_the_only_current_guest_crc_identity() -> None:
    runtime = MegaForthRuntime()
    foreground = runtime.main_context
    scratch = runtime.new_context()
    foreground.data.push(0)
    runtime.execute("CRC-MODE!", context=foreground)
    assert foreground.data.pop() == CRC_STATUS_OK

    scratch.data.push(1)
    runtime.execute("CRC-MODE!", context=scratch)
    assert scratch.data.pop() == CRC_STATUS_STATE
    scratch.data.push(ord("A"))
    runtime.execute("CRC-FEED-BYTE", context=scratch)
    assert scratch.data.pop() == CRC_STATUS_OK
    runtime.execute("CRC-FINAL@", context=scratch)
    assert scratch.data.pop() == _oracle_crc(b"A", 0)
    assert runtime.crc.owner is None


def test_zero_capability_profile_rejects_reflection_and_cleans_raw_final() -> None:
    memory = create_one_core_address_space(crypto_capabilities=0)
    runtime = MegaForthRuntime(memory=memory)
    assert _execute(runtime, "CRYPTO-CAPS@") == (0,)
    assert _execute(runtime, "CRC-MODE!", 5) == (CRC_STATUS_UNSUPPORTED,)

    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    assert _execute(runtime, "CRC-MODE!", 5) == (CRC_STATUS_UNSUPPORTED,)
    assert _execute(runtime, "CRC-RAW-FINAL@") == (
        0,
        CRC_STATUS_UNSUPPORTED,
    )
    assert runtime.crc.owner is None
    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    _execute(runtime, "CRC-FINAL@")


def test_runtime_instances_hold_independent_crc_transactions() -> None:
    first = MegaForthRuntime()
    second = MegaForthRuntime()
    assert _execute(first, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    assert _execute(second, "CRC-MODE!", 5) == (CRC_STATUS_OK,)
    assert _execute(first, "CRC-FEED-BYTE", ord("A")) == (CRC_STATUS_OK,)
    assert _execute(second, "CRC-FEED-BYTE", ord("B")) == (CRC_STATUS_OK,)
    assert first.crc.mode == 0
    assert second.crc.mode == 5
    assert first.crc.accumulator != second.crc.accumulator
    _execute(first, "CRC-FINAL@")
    _execute(second, "CRC-FINAL@")


def test_base_is_one_guest_cell_for_parser_hex_decimal_dot_and_unsigned_dot() -> None:
    runtime = MegaForthRuntime()
    base = runtime.find("BASE")
    assert base is not None
    assert runtime.memory.read64(base.body_address) == 10

    runtime.evaluate(b"16 BASE ! FF")
    assert runtime.main_context.data.pop() == 0xFF
    assert runtime.numeric_base == 16
    runtime.main_context.data.push(MASK64)
    runtime.execute("U.")
    runtime.main_context.data.push(MASK64)
    runtime.execute(".")
    assert runtime.drain_uart_output() == b"FFFFFFFFFFFFFFFF -1 "

    runtime.set_numeric_base(7)
    assert runtime.memory.read64(base.body_address) == 7
    runtime.memory.write64(base.body_address, 10)
    assert runtime.numeric_base == 10
    runtime.execute("HEX")
    assert runtime.memory.read64(base.body_address) == 16
    runtime.execute("DECIMAL")
    assert runtime.memory.read64(base.body_address) == 10


def test_zero_greater_uses_signed_guest_cells() -> None:
    runtime = MegaForthRuntime()
    assert _execute(runtime, "0>", 0) == (0,)
    assert _execute(runtime, "0>", 1) == (TRUE,)
    assert _execute(runtime, "0>", u64(-1)) == (0,)


def test_real_buffer_words_cover_every_tail_two_quads_and_three_modes(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    payload = bytes((index * 37 + 11) & 0xFF for index in range(24))
    address = _allocate_bytes(
        runtime,
        "CRC-MATRIX-DATA",
        payload,
        leading_pad=1,
    )
    for word, mode in (
        ("CRC32-BUF", 0),
        ("CRC32C-BUF", 5),
        ("CRC64-BUF", 2),
    ):
        for length in range(18):
            assert _execute(runtime, word, address, length) == (
                _oracle_crc(payload[:length], mode),
            )
            assert runtime.crc.owner is None


def test_crc32_string_alias_and_dot_crc32_preserve_the_live_base_cell(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    address = runtime.find("_CRC-DIAG-DATA").body_address  # type: ignore[union-attr]
    expected = EXPECTED_123456789[0]
    assert _execute(runtime, "CRC32-STR", address, 9) == (expected,)

    runtime.set_numeric_base(7)
    assert _execute(runtime, ".CRC32", address, 9) == ()
    assert runtime.drain_uart_output() == b"FC891918 "
    assert runtime.numeric_base == 7


@pytest.mark.parametrize("length", (1, 8))
def test_checked_buffer_returns_owner_status_with_balanced_stacks(
    loaded_crc: MegaForthRuntime,
    length: int,
) -> None:
    runtime = loaded_crc
    address = runtime.find("_CRC-DIAG-DATA").body_address  # type: ignore[union-attr]
    assert _execute(runtime, "_CRC-BUF-CHECKED", address, length) == (
        CRC_STATUS_STATE,
    )
    assert runtime.crc.owner is None


def test_checked_buffer_preserves_a_late_quad_or_tail_failure_status() -> None:
    runtime, _bios_words = _load_hooks()
    quad_calls: list[int] = []

    def fail_second_quad(context) -> None:
        quad_calls.append(context.data.pop())
        context.data.push(0 if len(quad_calls) == 1 else 7)

    runtime.define_primitive("CRC-FEED", fail_second_quad)
    _evaluate_crc_slice(runtime)
    payload = b"ABCDEFGHIJKLMNOP"
    address = _allocate_bytes(runtime, "CRC-QUAD-FAIL-DATA", payload)
    assert _execute(runtime, "_CRC-BUF-CHECKED", address, 16) == (7,)
    assert quad_calls == [
        int.from_bytes(payload[:8], "little"),
        int.from_bytes(payload[8:16], "little"),
    ]

    runtime, _bios_words = _load_hooks()
    tail_calls: list[int] = []

    def accept_quad(context) -> None:
        context.data.pop()
        context.data.push(0)

    def fail_second_tail(context) -> None:
        tail_calls.append(context.data.pop() & 0xFF)
        context.data.push(0 if len(tail_calls) == 1 else 9)

    runtime.define_primitive("CRC-FEED", accept_quad)
    runtime.define_primitive("CRC-FEED-BYTE", fail_second_tail)
    _evaluate_crc_slice(runtime)
    address = _allocate_bytes(runtime, "CRC-TAIL-FAIL-DATA", b"ABCDEFGHIJ")
    assert _execute(runtime, "_CRC-BUF-CHECKED", address, 10) == (9,)
    assert tail_calls == [ord("I"), ord("J")]


def test_crc_buf_throws_status_without_releasing_another_guest_owner(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    address = runtime.find("_CRC-DIAG-DATA").body_address  # type: ignore[union-attr]
    foreign_identity = (0, 1)
    assert runtime.crc.select_mode(foreign_identity, 0) == CRC_STATUS_OK
    runtime.evaluate(
        b": CRC-BUF-FOREIGN-OWNER "
        + str(address).encode("ascii")
        + b" 8 CRC-BUF ;"
    )
    action = runtime.find("CRC-BUF-FOREIGN-OWNER")
    assert action is not None
    context = runtime.main_context
    context.data.push(action.xt)

    runtime.execute("CATCH")

    assert context.data.snapshot() == (CRC_STATUS_STATE,)
    assert context.returns.snapshot() == ()
    assert runtime.crc.owner == foreign_identity
    assert runtime.crc.final(foreign_identity) == 0
    context.data.clear()


def test_faulting_source_qword_keeps_address_and_crc_seed_unchanged() -> None:
    memory = create_one_core_address_space(external_size=7)
    runtime = _load_crc(MegaForthRuntime(memory=memory))
    external = next(
        region
        for region in memory.regions
        if region.kind is AddressClass.EXTERNAL
    )
    payload = b"ABCDEFG"
    memory.write_bytes(external.base, payload)
    context = runtime.main_context
    context.data.push(external.base)
    context.data.push(8)

    with pytest.raises(CrossRegionAccessError):
        runtime.execute("CRC32-BUF")

    assert context.data.snapshot() == (external.base, 8, external.base)
    assert runtime.crc.owner == (0, 0)
    raw, status = runtime.crc.fetch((0, 0))
    assert status == CRC_STATUS_OK
    assert raw == 0xFFFFFFFF
    assert context.returns.snapshot() == ()
    assert context.reusable
    runtime.crc.final((0, 0))
    context.data.clear()


def test_faulting_source_tail_keeps_exact_stack_and_incremental_crc() -> None:
    memory = create_one_core_address_space(external_size=10)
    runtime = _load_crc(MegaForthRuntime(memory=memory))
    external = next(
        region
        for region in memory.regions
        if region.kind is AddressClass.EXTERNAL
    )
    payload = b"ABCDEFGHIJ"
    memory.write_bytes(external.base, payload)
    context = runtime.main_context
    context.data.push(external.base)
    context.data.push(11)

    with pytest.raises(UnmappedAddressError):
        runtime.execute("CRC32-BUF")

    fault_address = external.base + len(payload)
    assert context.data.snapshot() == (fault_address, 1, fault_address)
    assert runtime.crc.owner == (0, 0)
    raw, status = runtime.crc.fetch((0, 0))
    assert status == CRC_STATUS_OK
    assert raw == (_oracle_crc(payload, 0) ^ 0xFFFFFFFF)
    assert context.returns.snapshot() == ()
    assert context.reusable
    runtime.crc.final((0, 0))
    context.data.clear()


def test_throw_and_abort_do_not_silently_release_a_crc_transaction(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    runtime.evaluate(b": CRC-THROW-WITH-OWNER -77 THROW ;")

    runtime.evaluate(b"' CRC-THROW-WITH-OWNER CATCH")

    assert runtime.main_context.data.snapshot() == (u64(-77),)
    runtime.main_context.data.clear()
    assert runtime.crc.owner == (0, 0)

    with pytest.raises(ForthAbort):
        runtime.execute("ABORT")

    assert runtime.crc.owner == (0, 0)
    assert _execute(runtime, "CRC-FINAL@") == (0,)
    assert runtime.crc.owner is None


def test_real_crc_diagnostic_passes_exact_vectors_prints_and_releases(
    loaded_crc: MegaForthRuntime,
) -> None:
    runtime = loaded_crc
    assert _execute(runtime, "CRC-DIAG?") == (TRUE,)
    assert runtime.crc.owner is None
    assert _execute(runtime, ".CRC-DIAG") == ()
    assert runtime.drain_uart_output() == (
        b"\r\n  CRC Standard Vectors\r\n"
        b"    PASS (modes 0,1,2,4,5,6 and mode-5 raw)\r\n"
    )
    assert runtime.crc.owner is None
    assert _execute(runtime, "CRC-MODE!", 0) == (CRC_STATUS_OK,)
    _execute(runtime, "CRC-FINAL@")
