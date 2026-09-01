"""Contiguous unchanged-source acceptance for KDOS hardware diagnostics."""

from __future__ import annotations

import hashlib
from pathlib import Path
import re

import pytest

from emulator.megapad64 import Megapad64 as PythonMegapad64
from shared.cells import MASK64
import simulator.diagnostics as diagnostic_module
from simulator.diagnostics import (
    BIST_FAIL,
    BIST_IDLE,
    BIST_PASS,
    BIST_RUNNING,
    HostedDiagnosticsService,
    TILE_FAIL,
    TILE_IDLE,
    TILE_PASS,
)
from simulator.errors import ExecutionError
from simulator.runtime import MegaForthRuntime
from simulator.tile import (
    TILE_BYTES,
    tile_add_u8,
    tile_dot_u8,
    tile_multiply_u8,
    tile_sum_u8,
)
from tests.simulator.test_kdos_crc import _load_crc


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-diagnostics-856-902.f"

MEGAPAD_REVISION = "ed451faccfddb5f3fbb4e2200eb0dd0fdc314f4c"
KDOS_GIT_BLOB = "fd017b16dbd3ef4746d0e3467e980c015cf5a664"
FIRST_LINE = 856
LAST_LINE = 902
SLICE_SHA256 = "df3190d5a704349eb60b673be18cf386f12339f177bfcf6bcde6cb3bbb302e92"
SLICE_GIT_BLOB = "e69def5bf86407a60359e2afc1acd85efeb6cd2f"
DEFINITIONS = (
    b".PERF",
    b".BIST-STATUS",
    b".TILE-DIAG",
    b".ICACHE",
    b"DIAG",
)

_CYCLE_ROW = re.compile(rb"    Cycles:   -?[0-9]+ \r\n")


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


def _evaluate_diagnostics(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_diagnostics(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_diagnostics(_load_crc(runtime))


@pytest.fixture
def loaded_diagnostics() -> MegaForthRuntime:
    return _load_diagnostics()


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


def _normalize_cycles(output: bytes) -> bytes:
    normalized, count = _CYCLE_ROW.subn(
        b"    Cycles:   <HOST-WORK> \r\n",
        output,
    )
    assert count == 1
    return normalized


def test_cycles_is_a_separate_low32_semantic_work_clock() -> None:
    runtime = MegaForthRuntime()

    assert _execute(runtime, "CYCLES") == (1,)
    assert runtime.diagnostics.semantic_cycles == 1
    assert runtime.diagnostics.perf_cycles == 1

    assert _execute(runtime, "PERF-RESET") == ()
    assert runtime.diagnostics.semantic_cycles == 2
    assert runtime.diagnostics.perf_cycles == 0
    assert _execute(runtime, "CYCLES") == (3,)
    assert runtime.diagnostics.perf_cycles == 1

    second_context = runtime.new_context()
    runtime.execute("CYCLES", context=second_context)
    assert second_context.data.snapshot() == (4,)
    isolated = MegaForthRuntime()
    assert _execute(isolated, "CYCLES") == (1,)

    wrapped = MegaForthRuntime(
        diagnostics=HostedDiagnosticsService(semantic_cycles=0xFFFF_FFFF)
    )
    assert _execute(wrapped, "CYCLES") == (0,)
    assert wrapped.diagnostics.semantic_cycles == 0x1_0000_0000


def test_diagnostic_slice_is_exact_and_publishes_complete_ledger(
    loaded_diagnostics: MegaForthRuntime,
) -> None:
    for name in DEFINITIONS:
        assert loaded_diagnostics.find(name) is not None


def test_minimal_tile_kernel_matches_pinned_selftest_values() -> None:
    indexes = bytes(range(TILE_BYTES))
    add = tile_add_u8(indexes, bytes((100,)) * TILE_BYTES)
    multiply = tile_multiply_u8(indexes, bytes((3,)) * TILE_BYTES)

    assert hashlib.sha256(add).hexdigest() == (
        "79e345848693e89d691bd394b8c6a33ab39db6fc8fbded6aff83e39dfafcd681"
    )
    assert hashlib.sha256(multiply).hexdigest() == (
        "1a0e0ecf84382961a85aa8629e98aefcfeffdcf0fd74a6dd49d55d9706477ab2"
    )
    ones = bytes((1,)) * TILE_BYTES
    assert tile_dot_u8(ones, ones) == 64
    assert tile_sum_u8(bytes((2,)) * TILE_BYTES) == 128
    assert tile_add_u8(bytes((255,)) * TILE_BYTES, ones) == bytes(TILE_BYTES)
    assert tile_multiply_u8(
        bytes((255,)) * TILE_BYTES,
        bytes((2,)) * TILE_BYTES,
    ) == bytes((254,)) * TILE_BYTES

    with pytest.raises(ValueError, match="exactly 64 lanes"):
        tile_add_u8(b"short", bytes(TILE_BYTES))
    with pytest.raises(TypeError, match="must be bytes"):
        tile_sum_u8(bytearray(TILE_BYTES))  # type: ignore[arg-type]


def test_hosted_tile_selftest_matches_architectural_emulator_vector() -> None:
    emulator = PythonMegapad64()
    scratch_address = 0xFFF80
    scratch = bytes((index * 17) & 0xFF for index in range(128))
    emulator.mem[scratch_address : scratch_address + len(scratch)] = scratch

    hosted = HostedDiagnosticsService()
    hosted.run_tile_test()
    emulator._tile_selftest_write(1)

    assert (hosted.tile_status, hosted.tile_detail) == (
        emulator.tile_selftest,
        emulator.tile_st_detail,
    ) == (TILE_PASS, 0)
    assert bytes(
        emulator.mem[scratch_address : scratch_address + len(scratch)]
    ) == scratch


def test_all_eighteen_diagnostic_words_have_honest_hosted_semantics() -> None:
    runtime = MegaForthRuntime()
    service = runtime.diagnostics

    assert _execute(runtime, "PERF-CYCLES")[0] >= 1
    assert _execute(runtime, "PERF-STALLS") == (0,)
    assert _execute(runtime, "PERF-TILEOPS") == (0,)
    assert _execute(runtime, "PERF-EXTMEM") == (0,)
    assert _execute(runtime, "PERF-RESET") == ()
    assert _execute(runtime, "PERF-CYCLES") == (1,)

    sentinel_address = 0x800
    sentinel = b"hosted-bist-must-not-touch-this"
    runtime.memory.write_bytes(sentinel_address, sentinel)
    dictionary_snapshot = (
        runtime.dictionary.here,
        runtime.dictionary.latest_word,
    )
    bist_snapshot = (
        service.bist_status,
        service.bist_fail_address,
        service.bist_fail_data,
    )
    perf_before_bist = service.perf_cycles
    for dispatch_count, name in enumerate(
        ("BIST-FULL", "BIST-QUICK"),
        start=1,
    ):
        with pytest.raises(ExecutionError, match="destructive and unavailable"):
            runtime.execute(name)
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()
        assert runtime.main_context.reusable
        assert runtime.memory.read_bytes(sentinel_address, len(sentinel)) == sentinel
        assert (
            service.bist_status,
            service.bist_fail_address,
            service.bist_fail_data,
        ) == bist_snapshot
        assert (
            runtime.dictionary.here,
            runtime.dictionary.latest_word,
        ) == dictionary_snapshot
        assert service.perf_cycles == (
            perf_before_bist + dispatch_count
        ) & MASK64
    assert _execute(runtime, "BIST-STATUS") == (BIST_IDLE,)
    assert _execute(runtime, "BIST-FAIL-ADDR") == (0,)
    assert _execute(runtime, "BIST-FAIL-DATA") == (0,)

    tile_scratch_address = 0xFFF80
    tile_sentinel = bytes(range(128))
    runtime.memory.write_bytes(tile_scratch_address, tile_sentinel)
    assert _execute(runtime, "TILE-TEST") == ()
    scratch = runtime.new_context()
    runtime.execute("TILE-TEST@", context=scratch)
    assert scratch.data.pop() == TILE_PASS
    assert scratch.returns.snapshot() == ()
    assert _execute(runtime, "TILE-DETAIL@") == (0,)
    assert runtime.memory.read_bytes(sentinel_address, len(sentinel)) == sentinel
    assert runtime.memory.read_bytes(
        tile_scratch_address,
        len(tile_sentinel),
    ) == tile_sentinel

    assert service.icache_enabled
    assert _execute(runtime, "ICACHE-OFF") == ()
    assert not service.icache_enabled
    assert _execute(runtime, "ICACHE-ON") == ()
    assert service.icache_enabled
    assert _execute(runtime, "ICACHE-OFF") == ()
    assert _execute(runtime, "ICACHE-INV") == ()
    assert service.icache_enabled
    assert _execute(runtime, "ICACHE-HITS") == (0,)
    assert _execute(runtime, "ICACHE-MISSES") == (0,)


def test_performance_work_is_wrapping_per_runtime_and_shared_by_contexts() -> None:
    with pytest.raises(TypeError, match="HostedDiagnosticsService"):
        MegaForthRuntime(diagnostics=object())  # type: ignore[arg-type]
    with pytest.raises(TypeError, match="BIST status"):
        HostedDiagnosticsService(bist_status=True)
    with pytest.raises(ValueError, match="0, 1, 2, or 3"):
        HostedDiagnosticsService(bist_status=4)

    wrapped = HostedDiagnosticsService(perf_cycles=MASK64)
    wrapped.account_work()
    assert wrapped.perf_cycles == 0

    first = MegaForthRuntime()
    second = MegaForthRuntime()
    first.execute("PERF-RESET")
    second.execute("PERF-RESET")
    scratch = first.new_context()
    first.execute("TRUE", context=scratch)
    assert scratch.data.pop() == MASK64

    assert _execute(first, "PERF-CYCLES") == (2,)
    assert _execute(second, "PERF-CYCLES") == (1,)
    assert first.diagnostics is not second.diagnostics


def test_injected_diagnostic_profile_is_cloned_per_runtime() -> None:
    profile = HostedDiagnosticsService(
        perf_cycles=9,
        bist_status=BIST_PASS,
    )
    first = MegaForthRuntime(diagnostics=profile)
    second = MegaForthRuntime(diagnostics=profile)

    assert first.diagnostics is not profile
    assert second.diagnostics is not profile
    assert first.diagnostics is not second.diagnostics

    first.diagnostics.reset_performance()
    first.diagnostics.disable_icache()
    first.diagnostics.run_tile_test()

    for untouched in (profile, second.diagnostics):
        assert untouched.perf_cycles == 9
        assert untouched.bist_status == BIST_PASS
        assert untouched.icache_enabled
        assert untouched.tile_status == TILE_IDLE
        assert untouched.tile_detail == 0


def test_real_perf_renders_wrapped_cell_as_signed_number() -> None:
    runtime = _load_diagnostics(
        MegaForthRuntime(
            diagnostics=HostedDiagnosticsService(perf_cycles=1 << 63)
        )
    )

    assert _execute(runtime, ".PERF") == ()
    output = runtime.drain_uart_output()
    cycle_match = _CYCLE_ROW.search(output)
    assert cycle_match is not None
    assert int(cycle_match.group().split()[1]) < 0
    assert _normalize_cycles(output) == (
        b"\r\n  Performance Counters\r\n"
        b"    Cycles:   <HOST-WORK> \r\n"
        b"    Stalls:   0 \r\n"
        b"    Tile ops: 0 \r\n"
        b"    Ext mem:  0 \r\n"
    )


def test_real_perf_and_icache_render_hosted_observations(
    loaded_diagnostics: MegaForthRuntime,
) -> None:
    runtime = loaded_diagnostics
    before = runtime.diagnostics.perf_cycles
    assert _execute(runtime, ".PERF") == ()
    perf_output = runtime.drain_uart_output()
    cycle_match = _CYCLE_ROW.search(perf_output)
    assert cycle_match is not None
    rendered_cycle = int(cycle_match.group().split()[1])
    assert rendered_cycle > before
    assert _normalize_cycles(perf_output) == (
        b"\r\n  Performance Counters\r\n"
        b"    Cycles:   <HOST-WORK> \r\n"
        b"    Stalls:   0 \r\n"
        b"    Tile ops: 0 \r\n"
        b"    Ext mem:  0 \r\n"
    )

    assert _execute(runtime, ".ICACHE") == ()
    assert runtime.drain_uart_output() == (
        b"\r\n  I-Cache Statistics\r\n"
        b"    Hits:     0 \r\n"
        b"    Misses:   0 \r\n"
    )


@pytest.mark.parametrize(
    ("status", "fail_address", "fail_data", "expected"),
    (
        (
            BIST_IDLE,
            0,
            0,
            b"\r\n  Memory BIST Status\r\n"
            b"    idle (no BIST run)\r\n",
        ),
        (
            BIST_RUNNING,
            0,
            0,
            b"\r\n  Memory BIST Status\r\n"
            b"    running...\r\n",
        ),
        (
            BIST_PASS,
            0,
            0,
            b"\r\n  Memory BIST Status\r\n"
            b"    PASS\r\n",
        ),
        (
            BIST_FAIL,
            4660,
            3735928559,
            b"\r\n  Memory BIST Status\r\n"
            b"    FAIL at addr 4660 \r\n"
            b"    Expected/Actual: 3735928559 \r\n",
        ),
    ),
)
def test_real_bist_renderer_covers_every_retained_boot_status(
    status: int,
    fail_address: int,
    fail_data: int,
    expected: bytes,
) -> None:
    runtime = MegaForthRuntime(
        diagnostics=HostedDiagnosticsService(
            bist_status=status,
            bist_fail_address=fail_address,
            bist_fail_data=fail_data,
        )
    )
    _load_diagnostics(runtime)

    assert _execute(runtime, ".BIST-STATUS") == ()
    assert runtime.drain_uart_output() == expected


def test_real_tile_diagnostic_passes_through_production_value_kernel(
    loaded_diagnostics: MegaForthRuntime,
) -> None:
    runtime = loaded_diagnostics
    assert _execute(runtime, ".TILE-DIAG") == ()
    assert runtime.drain_uart_output() == (
        b"\r\n  Tile Datapath Self-Test...\r\n"
        b"    PASS (ADD, MUL, DOT, SUM)\r\n"
    )
    assert runtime.diagnostics.tile_status == TILE_PASS
    assert runtime.diagnostics.tile_detail == 0


@pytest.mark.parametrize(
    ("operation", "failure_bit"),
    (
        ("tile_add_u8", 0x1),
        ("tile_multiply_u8", 0x2),
        ("tile_dot_u8", 0x4),
        ("tile_sum_u8", 0x8),
    ),
)
def test_real_tile_diagnostic_reports_each_semantic_subtest_failure(
    monkeypatch: pytest.MonkeyPatch,
    operation: str,
    failure_bit: int,
) -> None:
    runtime = _load_diagnostics()

    def fail_operation(*_args: object) -> bytes:
        raise RuntimeError("injected semantic tile failure")

    monkeypatch.setattr(diagnostic_module, operation, fail_operation)
    assert _execute(runtime, ".TILE-DIAG") == ()
    assert runtime.drain_uart_output() == (
        b"\r\n  Tile Datapath Self-Test...\r\n"
        b"    FAIL \xe2\x80\x94 failed sub-tests: "
        + str(failure_bit).encode("ascii")
        + b" \r\n"
    )
    assert runtime.diagnostics.tile_status == TILE_FAIL
    assert runtime.diagnostics.tile_detail == failure_bit


def test_complete_real_diag_composes_crc_and_hosted_diagnostics(
    loaded_diagnostics: MegaForthRuntime,
) -> None:
    runtime = loaded_diagnostics
    assert _execute(runtime, "DIAG") == ()
    assert _normalize_cycles(runtime.drain_uart_output()) == (
        b"\r\n ======== Hardware Diagnostics ========\r\n"
        b"\r\n  Performance Counters\r\n"
        b"    Cycles:   <HOST-WORK> \r\n"
        b"    Stalls:   0 \r\n"
        b"    Tile ops: 0 \r\n"
        b"    Ext mem:  0 \r\n"
        b"\r\n  CRC Standard Vectors\r\n"
        b"    PASS (modes 0,1,2,4,5,6 and mode-5 raw)\r\n"
        b"\r\n  Memory BIST Status\r\n"
        b"    idle (no BIST run)\r\n"
        b"\r\n  Tile Datapath Self-Test...\r\n"
        b"    PASS (ADD, MUL, DOT, SUM)\r\n"
        b"\r\n  I-Cache Statistics\r\n"
        b"    Hits:     0 \r\n"
        b"    Misses:   0 \r\n"
        b" ======================================\r\n"
    )
    assert runtime.crc.owner is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
