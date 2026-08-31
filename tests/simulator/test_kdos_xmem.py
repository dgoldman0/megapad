"""Contiguous unchanged-source acceptance for KDOS external memory."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ForthAbort, SourceError
from simulator.memory import BANK0_DEFAULT_SIZE, EXTERNAL_BASE, MMIO_BASE
from simulator.platform import (
    SYSINFO_EXTERNAL_BASE,
    SYSINFO_EXTERNAL_SIZE,
    create_one_core_address_space,
)
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_hbw import _load_hbw
from tests.simulator.test_kdos_x25519 import _execute


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-xmem-2110-2388.f"

FIRST_LINE = 2110
LAST_LINE = 2388
SLICE_SHA256 = (
    "d1afea7d6dba6c5d6e7d40d78812454e9b246b3c74929e0be9bba79342e15900"
)
SLICE_GIT_BLOB = "ffe1a68cbd1a02bf35978b46ab584627ce237fa1"
DEFINITIONS = (
    b"XMEM-HERE",
    b"XMEM-LIMIT",
    b"XMEM-INIT-DONE",
    b"XMEM-FL",
    b"FL-PREV",
    b"FL-CURR",
    b"FL-NEED",
    b"(_XMEM-FREE-SPAN-CHECK)",
    b"_XMEM-FREE-SPAN-CHECK",
    b"_XMEM-NORMALIZE-SIZE",
    b"XMEM-FREE-BLOCK",
    b"_XMEM-FL-REPLACE",
    b"(XMEM-FL-FIND)",
    b"XMEM?",
    b"XMEM-INIT",
    b"XMEM-ALLOT",
    b"XMEM-ALLOT?",
    b"ALLOCATE",
    b"FREE",
    b"_RS-OLD",
    b"RESIZE",
    b"DMA-ALLOCATE",
    b"DMA-FREE",
    b"DMA-RESIZE",
    b"XMEM-TALIGN",
    b"XMEM-FLOOR",
    b"(XMEM-RESET)",
    b"XMEM-RESET",
    b"XMEM-FREE",
    b".XMEM",
    b"XBUF",
)
BIOS_WORDS = (b"EXT-MEM-BASE", b"EXT-MEM-SIZE", b"NIP", b"TUCK")

CANONICAL_EXTERNAL_SIZE = 128 * (1 << 20)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 10_924
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_xmem(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_xmem(runtime: MegaForthRuntime | None = None) -> MegaForthRuntime:
    if runtime is None:
        runtime = MegaForthRuntime(
            memory=create_one_core_address_space(
                external_size=CANONICAL_EXTERNAL_SIZE
            )
        )
    return _evaluate_xmem(_load_hbw(runtime))


@pytest.fixture
def loaded_xmem() -> MegaForthRuntime:
    return _load_xmem()


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _pointer(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(_body(runtime, name))


def _small_xmem(size: int = 0x200) -> MegaForthRuntime:
    return _load_xmem(
        MegaForthRuntime(memory=create_one_core_address_space(external_size=size))
    )


def _expect_abort(
    runtime: MegaForthRuntime,
    name: str,
    output: bytes,
    *inputs: int,
) -> None:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute(name, step_budget=250_000)
    assert runtime.drain_uart_output() == output
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_xmem_slice_is_exact_and_initializes_complete_ledger(
    loaded_xmem: MegaForthRuntime,
) -> None:
    assert len(DEFINITIONS) == 31
    for name in DEFINITIONS + BIOS_WORDS:
        assert loaded_xmem.find(name) is not None

    assert _execute(loaded_xmem, "XMEM?") == (TRUE,)
    assert _pointer(loaded_xmem, "XMEM-HERE") == EXTERNAL_BASE
    assert _pointer(loaded_xmem, "XMEM-LIMIT") == (
        EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE
    )
    assert _pointer(loaded_xmem, "XMEM-INIT-DONE") == 1
    assert _pointer(loaded_xmem, "XMEM-FL") == 0
    assert _pointer(loaded_xmem, "XMEM-FLOOR") == 0
    assert _execute(loaded_xmem, "XMEM-FREE") == (CANONICAL_EXTERNAL_SIZE,)
    assert loaded_xmem.uart_output == b""


def test_external_bios_words_read_bound_sysinfo_and_stack_primitives() -> None:
    external_size = 0x12_340
    runtime = MegaForthRuntime(
        memory=create_one_core_address_space(external_size=external_size)
    )

    assert (
        runtime.memory.read64(MMIO_BASE + SYSINFO_EXTERNAL_BASE)
        == EXTERNAL_BASE
    )
    assert (
        runtime.memory.read64(MMIO_BASE + SYSINFO_EXTERNAL_SIZE)
        == external_size
    )
    assert _execute(runtime, "EXT-MEM-BASE") == (EXTERNAL_BASE,)
    assert _execute(runtime, "EXT-MEM-SIZE") == (external_size,)
    assert _execute(runtime, "NIP", 0x11, 0x22) == (0x22,)
    assert _execute(runtime, "TUCK", 0x11, 0x22) == (0x22, 0x11, 0x22)


def test_xmem_checked_bump_allocation_normalizes_and_fails_atomically() -> None:
    runtime = _small_xmem(128)

    assert _execute(runtime, "XMEM-ALLOT", 1) == (EXTERNAL_BASE,)
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 16
    assert _execute(runtime, "XMEM-ALLOT?", 17) == (EXTERNAL_BASE + 16, 0)
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 48
    assert _execute(runtime, "XMEM-ALLOT?", 80) == (EXTERNAL_BASE + 48, 0)
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 128

    before = _pointer(runtime, "XMEM-HERE")
    for request in (0, MASK64, 129):
        assert _execute(runtime, "XMEM-ALLOT?", request) == (0, MASK64)
        assert _pointer(runtime, "XMEM-HERE") == before

    _expect_abort(runtime, "XMEM-ALLOT", b"Invalid ext mem size", 0)
    _expect_abort(runtime, "XMEM-ALLOT", b"Ext mem overflow", 129)
    assert _pointer(runtime, "XMEM-HERE") == before


def test_xmem_free_list_is_lifo_first_fit_and_splits_recyclable_tails() -> None:
    runtime = _small_xmem()
    first = _execute(runtime, "XMEM-ALLOT", 64)[0]
    second = _execute(runtime, "XMEM-ALLOT", 32)[0]
    assert _execute(runtime, "XMEM-ALLOT", 16) == (EXTERNAL_BASE + 96,)
    high_water = EXTERNAL_BASE + 112

    assert _execute(runtime, "XMEM-FREE-BLOCK", first, 64) == ()
    assert _execute(runtime, "XMEM-FREE-BLOCK", second, 32) == ()
    assert _pointer(runtime, "XMEM-FL") == second
    assert runtime.memory.read64(second + 8) == first

    assert _execute(runtime, "XMEM-ALLOT", 48) == (first,)
    tail = first + 48
    assert _pointer(runtime, "XMEM-FL") == second
    assert runtime.memory.read64(second + 8) == tail
    assert runtime.memory.read64(tail) == 16
    assert runtime.memory.read64(tail + 8) == 0

    assert _execute(runtime, "XMEM-ALLOT", 16) == (second,)
    assert _pointer(runtime, "XMEM-FL") == second + 16
    assert runtime.memory.read64(second + 16) == 16
    assert runtime.memory.read64(second + 24) == tail
    assert _pointer(runtime, "XMEM-HERE") == high_water
    assert _execute(runtime, "XMEM-FREE") == (0x200 - 112,)


def test_xmem_free_validation_preflights_before_guest_memory_writes() -> None:
    runtime = _small_xmem(128)
    assert _execute(runtime, "XMEM-ALLOT", 64) == (EXTERNAL_BASE,)
    runtime.memory.fill(EXTERNAL_BASE, 64, 0xA5)

    failures = (
        (EXTERNAL_BASE, 0, b"XMEM-FREE: block too small"),
        (EXTERNAL_BASE - 16, 16, b"XMEM-FREE: addr below base"),
        (EXTERNAL_BASE + 128, 16, b"XMEM-FREE: exceeds limit"),
        (EXTERNAL_BASE + 48, 17, b"XMEM-FREE: above high water"),
        (EXTERNAL_BASE + 64, 16, b"XMEM-FREE: above high water"),
    )
    for address, size, message in failures:
        _expect_abort(runtime, "XMEM-FREE-BLOCK", message, address, size)
        assert _pointer(runtime, "XMEM-FL") == 0
        assert runtime.memory.read_bytes(EXTERNAL_BASE, 64) == bytes([0xA5] * 64)


def test_xmem_source_accepts_interior_and_double_free_list_nodes() -> None:
    runtime = _small_xmem(128)
    assert _execute(runtime, "XMEM-ALLOT", 64) == (EXTERNAL_BASE,)
    interior = EXTERNAL_BASE + 16

    assert _execute(runtime, "XMEM-FREE-BLOCK", interior, 1) == ()
    assert _pointer(runtime, "XMEM-FL") == interior
    assert runtime.memory.read64(interior) == 16
    assert runtime.memory.read64(interior + 8) == 0

    assert _execute(runtime, "XMEM-FREE-BLOCK", interior, 1) == ()
    assert _pointer(runtime, "XMEM-FL") == interior
    assert runtime.memory.read64(interior + 8) == interior


def test_public_allocate_free_and_resize_use_xmem_prefix_and_preserve_data() -> None:
    runtime = _small_xmem(0x200)
    address, status = _execute(runtime, "ALLOCATE", 20)
    assert status == 0
    assert address == EXTERNAL_BASE + 8
    assert runtime.memory.read64(EXTERNAL_BASE) == 32
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 32

    payload = bytes(range(24))
    runtime.memory.write_bytes(address, payload)
    resized, status = _execute(runtime, "RESIZE", address, 40)
    assert status == 0
    assert resized == EXTERNAL_BASE + 40
    assert runtime.memory.read64(EXTERNAL_BASE + 32) == 48
    assert runtime.memory.read_bytes(resized, 24) == payload
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 80
    assert _pointer(runtime, "XMEM-FL") == EXTERNAL_BASE
    assert runtime.memory.read64(EXTERNAL_BASE) == 32

    assert _execute(runtime, "FREE", resized) == ()
    recycled, status = _execute(runtime, "ALLOCATE", 1)
    assert (recycled, status) == (EXTERNAL_BASE + 40, 0)


def test_xmem_resize_oom_returns_original_address_without_mutation() -> None:
    runtime = _small_xmem(64)
    address, status = _execute(runtime, "ALLOCATE", 20)
    assert status == 0
    runtime.memory.write_bytes(address, b"retained on allocation failure")
    here = _pointer(runtime, "XMEM-HERE")

    assert _execute(runtime, "RESIZE", address, 64) == (address, MASK64)
    assert _pointer(runtime, "XMEM-HERE") == here
    assert runtime.memory.read_bytes(address, 30) == b"retained on allocation failure"
    assert _pointer(runtime, "XMEM-FL") == 0


def test_dma_allocation_stays_in_bank0_when_public_heap_routes_to_xmem() -> None:
    runtime = _small_xmem()

    ordinary, ordinary_status = _execute(runtime, "ALLOCATE", 32)
    dma, dma_status = _execute(runtime, "DMA-ALLOCATE", 32)
    assert ordinary_status == dma_status == 0
    assert ordinary >= EXTERNAL_BASE
    assert 0 < dma < BANK0_DEFAULT_SIZE

    assert _execute(runtime, "DMA-FREE", dma) == ()
    assert _execute(runtime, "FREE", ordinary) == ()


def test_xbuf_sets_reset_floor_and_reset_reclaims_without_wiping() -> None:
    runtime = _small_xmem(256)
    result = runtime.evaluate(b"64 XBUF PERSIST\n", source_name="xbuf-test")
    assert tuple(word.name for word in result.definitions) == (b"PERSIST",)
    assert _execute(runtime, "PERSIST") == (EXTERNAL_BASE,)
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 64
    assert _pointer(runtime, "XMEM-FLOOR") == EXTERNAL_BASE + 64

    temporary = _execute(runtime, "XMEM-ALLOT", 32)[0]
    runtime.memory.write_bytes(temporary + 16, b"stale tail bytes")
    assert _execute(runtime, "XMEM-FREE-BLOCK", temporary, 32) == ()
    assert _pointer(runtime, "XMEM-FL") == temporary

    assert _execute(runtime, "XMEM-RESET") == ()
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 64
    assert _pointer(runtime, "XMEM-FL") == 0
    assert _execute(runtime, "XMEM-FREE") == (192,)
    assert runtime.memory.read_bytes(temporary + 16, 16) == b"stale tail bytes"


def test_absent_xmem_routes_public_allocation_and_xbuf_to_bank0() -> None:
    runtime = _load_xmem(
        MegaForthRuntime(memory=create_one_core_address_space(external_size=0))
    )

    assert _execute(runtime, "EXT-MEM-BASE") == (0,)
    assert _execute(runtime, "EXT-MEM-SIZE") == (0,)
    assert _execute(runtime, "XMEM?") == (0,)
    assert _pointer(runtime, "XMEM-HERE") == 0
    assert _pointer(runtime, "XMEM-LIMIT") == 0
    assert _execute(runtime, "XMEM-ALLOT?", 1) == (0, MASK64)
    assert _execute(runtime, "XMEM-FREE") == (0,)
    _expect_abort(runtime, "XMEM-ALLOT", b"No external memory", 1)

    allocated, status = _execute(runtime, "ALLOCATE", 24)
    assert status == 0
    assert 0 < allocated < BANK0_DEFAULT_SIZE
    assert _execute(runtime, "FREE", allocated) == ()

    before = runtime.dictionary.here
    result = runtime.evaluate(b"32 XBUF LOCAL-BUF\n", source_name="xbuf-bank0")
    assert tuple(word.name for word in result.definitions) == (b"LOCAL-BUF",)
    local = runtime.find("LOCAL-BUF")
    assert local is not None
    assert _execute(runtime, "LOCAL-BUF") == (local.body_address,)
    assert runtime.dictionary.here == local.body_address + 32
    assert runtime.dictionary.here > before


def test_xmem_status_reports_only_live_bump_tail_for_present_and_absent() -> None:
    present = _small_xmem(256)
    assert _execute(present, "XMEM-ALLOT", 16) == (EXTERNAL_BASE,)
    assert _execute(present, "XMEM-FREE-BLOCK", EXTERNAL_BASE, 16) == ()
    assert _execute(present, "XMEM-FREE") == (240,)
    assert _execute(present, ".XMEM") == ()
    assert present.drain_uart_output() == (
        b" External RAM:\r\n"
        b"   Base  = 1048576 \r\n"
        b"   Size  = 256  bytes\r\n"
        b"   Used  = 16  bytes\r\n"
        b"   Free  = 240  bytes\r\n"
    )

    absent = _load_xmem(
        MegaForthRuntime(memory=create_one_core_address_space(external_size=0))
    )
    assert _execute(absent, ".XMEM") == ()
    assert absent.drain_uart_output() == (
        b" External RAM:\r\n"
        b"   (not present)\r\n"
    )


def test_xmem_alignment_can_cross_a_nonaligned_limit() -> None:
    runtime = _small_xmem(63)
    assert _execute(runtime, "XMEM-ALLOT", 1) == (EXTERNAL_BASE,)
    assert _execute(runtime, "XMEM-TALIGN") == ()
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 64
    assert _execute(runtime, "XMEM-FREE") == (MASK64,)


def test_xmem_pointer_and_free_list_are_shared_across_contexts() -> None:
    runtime = _small_xmem(128)
    peer = runtime.new_context()
    peer.data.push(16)
    runtime.execute("XMEM-ALLOT", context=peer, step_budget=250_000)
    assert peer.data.snapshot() == (EXTERNAL_BASE,)
    assert peer.returns.snapshot() == ()
    assert _execute(runtime, "XMEM-ALLOT", 16) == (EXTERNAL_BASE + 16,)

    peer.data.clear()
    peer.data.push(EXTERNAL_BASE)
    peer.data.push(16)
    runtime.execute("XMEM-FREE-BLOCK", context=peer, step_budget=250_000)
    assert peer.data.snapshot() == ()
    assert _pointer(runtime, "XMEM-FL") == EXTERNAL_BASE
    assert _execute(runtime, "XMEM-ALLOT", 16) == (EXTERNAL_BASE,)


def test_next_contiguous_frontier_stops_at_dictionary_power_words(
    loaded_xmem: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[2388:2395])
    assert next_source.startswith(b"\n")
    assert next_source.endswith(b"        2/ SWAP 2* SWAP\n")

    with pytest.raises(SourceError, match="unknown word") as caught:
        loaded_xmem.evaluate(next_source, source_name="kdos.f:2389-2395")
    assert caught.value.location.line == 7
    assert caught.value.location.column == 8
    assert caught.value.message == "unknown word b'2/'"
