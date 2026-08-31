"""Contiguous unchanged-source acceptance for the KDOS Arena allocator."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, u64
from simulator.errors import ForthAbort, SourceError
from simulator.memory import BANK0_DEFAULT_SIZE, EXTERNAL_BASE, HBW_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_dictionary_index import CANONICAL_INDEX_BYTES
from tests.simulator.test_kdos_hbw import CANONICAL_HBW_SIZE
from tests.simulator.test_kdos_userland import _catch, _load_userland
from tests.simulator.test_kdos_x25519 import _execute
from tests.simulator.test_kdos_xmem import (
    CANONICAL_EXTERNAL_SIZE,
    _pointer,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-arena-2576-2780.f"

FIRST_LINE = 2576
LAST_LINE = 2780
SLICE_SHA256 = (
    "71b9c742b769e18620e4ab60c592e5c0517dc46e924717dc29c4a8ea1e16e97a"
)
SLICE_GIT_BLOB = "33b7677f98f72a4973fe4d312d913622cc2ad24a"
DEFINITIONS = (
    b"A-HEAP",
    b"A-XMEM",
    b"A-HBW",
    b"A.BASE",
    b"A.SIZE",
    b"A.PTR",
    b"A.SOURCE",
    b"AR-SZ",
    b"AR-SRC",
    b"AR-BLK",
    b"(AR-ALLOC-BACKING)",
    b"(AR-FREE-BACKING)",
    b"ARENA-NEW",
    b"ARENA-NEW-AT",
    b"ARENA-USED",
    b"ARENA-FREE",
    b"ARENA-ALLOT",
    b"ARENA-ALLOT?",
    b"ARENA-RESET",
    b"ARENA-DESTROY",
    b"ARENA-SNAP",
    b"ARENA-ROLLBACK",
    b"ARENA-SNAP-DROP",
    b"ARENA-STK-DEPTH",
    b"ARENA-STK",
    b"ARENA-SP",
    b"CURRENT-ARENA",
    b"ARENA-PUSH",
    b"ARENA-POP",
    b"AALLOT",
    b".ARENA",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 8_303
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


def _evaluate_arena(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_arena(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_arena(_load_userland(runtime))


def _runtime_with_regions(
    *,
    external_size: int = CANONICAL_EXTERNAL_SIZE,
    hbw_size: int = CANONICAL_HBW_SIZE,
) -> MegaForthRuntime:
    return _load_arena(
        MegaForthRuntime(
            memory=create_one_core_address_space(
                external_size=external_size,
                hbw_size=hbw_size,
            )
        )
    )


@pytest.fixture
def loaded_arena() -> MegaForthRuntime:
    return _load_arena()


def _descriptor(runtime: MegaForthRuntime, arena: int) -> tuple[int, ...]:
    return tuple(runtime.memory.read64(arena + offset) for offset in range(0, 32, 8))


def _new(
    runtime: MegaForthRuntime,
    size: int,
    source: int,
) -> tuple[int, int]:
    result = _execute(runtime, "ARENA-NEW", size, source)
    assert len(result) == 2
    return result


def _expect_abort(
    runtime: MegaForthRuntime,
    name: str,
    message: bytes,
    *inputs: int,
) -> None:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute(name, step_budget=250_000)
    assert runtime.drain_uart_output() == message
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_arena_slice_is_exact_and_loads_complete_source_state(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    assert len(DEFINITIONS) == 31
    for name in DEFINITIONS:
        assert runtime.find(name) is not None

    assert _execute(runtime, "A-HEAP") == (0,)
    assert _execute(runtime, "A-XMEM") == (1,)
    assert _execute(runtime, "A-HBW") == (2,)
    assert _execute(runtime, "A.BASE", 0x1234) == (0x1234,)
    assert _execute(runtime, "A.SIZE", 0x1234) == (0x123C,)
    assert _execute(runtime, "A.PTR", 0x1234) == (0x1244,)
    assert _execute(runtime, "A.SOURCE", 0x1234) == (0x124C,)
    assert tuple(_pointer(runtime, name) for name in ("AR-SZ", "AR-SRC", "AR-BLK")) == (
        0,
        0,
        0,
    )
    assert _execute(runtime, "ARENA-STK-DEPTH") == (4,)
    arena_stack = _execute(runtime, "ARENA-STK")[0]
    assert runtime.memory.read_bytes(arena_stack, 32) == bytes(32)
    assert _pointer(runtime, "ARENA-SP") == 0
    assert runtime.uart_output == b""


@pytest.mark.parametrize(
    ("source", "base_offset", "xmem_advance", "hbw_advance"),
    (
        (0, 8, 80, 0),
        (1, 0, 64, 0),
        (2, 0, 0, 64),
    ),
)
def test_arena_new_and_destroy_follow_each_backing_route(
    source: int,
    base_offset: int,
    xmem_advance: int,
    hbw_advance: int,
) -> None:
    runtime = _runtime_with_regions()
    dictionary_here = runtime.dictionary.here
    xmem_here = _pointer(runtime, "XMEM-HERE")
    hbw_here = _pointer(runtime, "HBW-HERE")
    backing_here = hbw_here if source == 2 else xmem_here

    arena, status = _new(runtime, 64, source)
    assert status == 0
    assert arena == dictionary_here
    base = backing_here + base_offset
    assert _descriptor(runtime, arena) == (base, 64, base, source)
    assert runtime.dictionary.here == dictionary_here + 32
    assert _pointer(runtime, "XMEM-HERE") == xmem_here + xmem_advance
    assert _pointer(runtime, "HBW-HERE") == hbw_here + hbw_advance

    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _descriptor(runtime, arena) == (0, 0, 0, 0)
    assert runtime.dictionary.here == dictionary_here + 32
    if source == 0:
        assert _pointer(runtime, "XMEM-FL") == base - 8
    elif source == 1:
        assert _pointer(runtime, "XMEM-FL") == base
    else:
        assert _pointer(runtime, "XMEM-FL") == 0
        assert _pointer(runtime, "HBW-HERE") == hbw_here + 64

    second, second_status = _new(runtime, 64, source)
    assert second_status == 0
    second_base = _descriptor(runtime, second)[0]
    if source in (0, 1):
        assert second_base == base
        assert _pointer(runtime, "XMEM-HERE") == xmem_here + xmem_advance
    else:
        assert second_base == base + 64
        assert _pointer(runtime, "HBW-HERE") == hbw_here + 128
    assert _execute(runtime, "ARENA-DESTROY", second) == ()


def test_a_heap_falls_back_to_reclaimable_bank0_without_external_memory() -> None:
    runtime = _runtime_with_regions(external_size=0)
    assert _execute(runtime, "HEAP-SETUP") == ()
    free_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    dictionary_here = runtime.dictionary.here

    arena, status = _new(runtime, 128, 0)
    assert status == 0
    base, size, pointer, source = _descriptor(runtime, arena)
    assert 0 < base < BANK0_DEFAULT_SIZE
    assert (size, pointer, source) == (128, base, 0)
    assert runtime.dictionary.here == dictionary_here + 32
    assert _execute(runtime, "HEAP-FREE-BYTES")[0] < free_before

    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _execute(runtime, "HEAP-FREE-BYTES") == (free_before,)


@pytest.mark.parametrize(
    ("size", "source"),
    ((0, 0), (64, 99), (MASK64, 0), (MASK64, 1)),
)
def test_arena_new_failure_does_not_allocate_or_publish_a_descriptor(
    loaded_arena: MegaForthRuntime,
    size: int,
    source: int,
) -> None:
    runtime = loaded_arena
    dictionary_here = runtime.dictionary.here
    runtime.memory.fill(dictionary_here, 32, 0xA5)
    before = (
        _pointer(runtime, "XMEM-HERE"),
        _pointer(runtime, "HBW-HERE"),
        _pointer(runtime, "XMEM-FL"),
    )

    assert _new(runtime, size, source) == (0, MASK64)
    assert runtime.dictionary.here == dictionary_here
    assert runtime.memory.read_bytes(dictionary_here, 32) == bytes([0xA5] * 32)
    assert (
        _pointer(runtime, "XMEM-HERE"),
        _pointer(runtime, "HBW-HERE"),
        _pointer(runtime, "XMEM-FL"),
    ) == before


def test_arena_creation_failure_scratch_matches_source_order(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    for name, value in (("AR-SZ", 11), ("AR-SRC", 22), ("AR-BLK", 33)):
        word = runtime.find(name)
        assert word is not None
        runtime.memory.write64(word.body_address, value)

    assert _new(runtime, 0, 1) == (0, MASK64)
    assert tuple(_pointer(runtime, name) for name in ("AR-SZ", "AR-SRC", "AR-BLK")) == (
        11,
        22,
        33,
    )

    assert _new(runtime, 64, 99) == (0, MASK64)
    assert tuple(_pointer(runtime, name) for name in ("AR-SZ", "AR-SRC", "AR-BLK")) == (
        64,
        99,
        33,
    )


def test_arena_hbw_creation_inherits_the_raw_high_cell_wrap() -> None:
    runtime = _runtime_with_regions()
    hbw_here = _pointer(runtime, "HBW-HERE")

    arena, status = _new(runtime, MASK64, 2)
    assert status == 0
    assert _descriptor(runtime, arena) == (hbw_here, MASK64, hbw_here, 2)
    assert _pointer(runtime, "HBW-HERE") == hbw_here - 1

    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _descriptor(runtime, arena) == (0, 0, 0, 0)
    assert _pointer(runtime, "HBW-HERE") == hbw_here - 1


def test_arena_new_at_reuses_caller_descriptor_and_xmem_backing(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    runtime.evaluate(b"CREATE AR-TEST-DESC 32 ALLOT\n", source_name="arena-desc")
    descriptor = _execute(runtime, "AR-TEST-DESC")[0]
    dictionary_here = runtime.dictionary.here
    xmem_here = _pointer(runtime, "XMEM-HERE")
    runtime.memory.fill(descriptor, 32, 0xA5)

    assert _execute(runtime, "ARENA-NEW-AT", descriptor, 33, 1) == (0,)
    assert _descriptor(runtime, descriptor) == (xmem_here, 33, xmem_here, 1)
    assert _pointer(runtime, "XMEM-HERE") == xmem_here + 48
    assert runtime.dictionary.here == dictionary_here
    assert _execute(runtime, "ARENA-ALLOT", descriptor, 1) == (xmem_here,)
    assert _execute(runtime, "ARENA-USED", descriptor) == (8,)
    assert _execute(runtime, "ARENA-RESET", descriptor) == ()
    assert _execute(runtime, "ARENA-USED", descriptor) == (0,)

    assert _execute(runtime, "ARENA-DESTROY", descriptor) == ()
    assert _descriptor(runtime, descriptor) == (0, 0, 0, 0)
    assert _execute(runtime, "ARENA-NEW-AT", descriptor, 33, 1) == (0,)
    assert _descriptor(runtime, descriptor) == (xmem_here, 33, xmem_here, 1)
    assert _pointer(runtime, "XMEM-HERE") == xmem_here + 48
    assert runtime.dictionary.here == dictionary_here
    assert _execute(runtime, "ARENA-DESTROY", descriptor) == ()


def test_arena_new_at_failure_preserves_caller_descriptor() -> None:
    runtime = _runtime_with_regions()
    runtime.evaluate(b"CREATE AR-FAIL-DESC 32 ALLOT\n", source_name="arena-fail-desc")
    descriptor = _execute(runtime, "AR-FAIL-DESC")[0]
    dictionary_here = runtime.dictionary.here
    sentinel = bytes(range(32))

    for size, source in ((0, 0), (64, 99)):
        runtime.memory.write_bytes(descriptor, sentinel)
        before = (
            _pointer(runtime, "XMEM-HERE"),
            _pointer(runtime, "HBW-HERE"),
            _pointer(runtime, "XMEM-FL"),
        )
        assert _execute(runtime, "ARENA-NEW-AT", descriptor, size, source) == (
            MASK64,
        )
        assert runtime.memory.read_bytes(descriptor, 32) == sentinel
        assert runtime.dictionary.here == dictionary_here
        assert (
            _pointer(runtime, "XMEM-HERE"),
            _pointer(runtime, "HBW-HERE"),
            _pointer(runtime, "XMEM-FL"),
        ) == before

    remaining = _execute(runtime, "XMEM-FREE")[0]
    assert _execute(runtime, "XMEM-ALLOT", remaining)[0] != 0
    runtime.memory.write_bytes(descriptor, sentinel)
    assert _execute(runtime, "ARENA-NEW-AT", descriptor, 16, 1) == (MASK64,)
    assert runtime.memory.read_bytes(descriptor, 32) == sentinel
    assert runtime.dictionary.here == dictionary_here


def test_arena_new_at_can_overwrite_a_live_descriptor_and_lose_backing(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    runtime.evaluate(b"CREATE AR-LIVE-DESC 32 ALLOT\n", source_name="arena-live-desc")
    descriptor = _execute(runtime, "AR-LIVE-DESC")[0]
    xmem_here = _pointer(runtime, "XMEM-HERE")

    assert _execute(runtime, "ARENA-NEW-AT", descriptor, 32, 1) == (0,)
    first_base = _descriptor(runtime, descriptor)[0]
    assert first_base == xmem_here
    assert _execute(runtime, "ARENA-NEW-AT", descriptor, 32, 1) == (0,)
    second_base = _descriptor(runtime, descriptor)[0]
    assert second_base == first_base + 32
    assert _pointer(runtime, "XMEM-HERE") == xmem_here + 64
    assert _pointer(runtime, "XMEM-FL") == 0

    assert _execute(runtime, "ARENA-DESTROY", descriptor) == ()
    assert _pointer(runtime, "XMEM-FL") == second_base
    assert _pointer(runtime, "XMEM-HERE") == xmem_here + 64


def test_arena_allot_alignment_exact_fit_and_destroyed_failures(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    arena, status = _new(runtime, 40, 1)
    assert status == 0
    base = _descriptor(runtime, arena)[0]

    assert _execute(runtime, "ARENA-ALLOT", arena, 1) == (base,)
    assert _execute(runtime, "ARENA-ALLOT?", arena, 9) == (base + 8, 0)
    assert _execute(runtime, "ARENA-ALLOT", arena, 0) == (base + 24,)
    assert _execute(runtime, "ARENA-USED", arena) == (24,)
    assert _execute(runtime, "ARENA-FREE", arena) == (16,)
    assert _execute(runtime, "ARENA-ALLOT?", arena, 16) == (base + 24, 0)
    assert _execute(runtime, "ARENA-USED", arena) == (40,)
    pointer = _descriptor(runtime, arena)[2]

    assert _execute(runtime, "ARENA-ALLOT?", arena, 1) == (0, MASK64)
    assert _descriptor(runtime, arena)[2] == pointer
    _expect_abort(runtime, "ARENA-ALLOT", b"arena full", arena, 1)
    assert _descriptor(runtime, arena)[2] == pointer

    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _execute(runtime, "ARENA-ALLOT?", arena, 1) == (0, MASK64)
    _expect_abort(runtime, "ARENA-ALLOT", b"arena destroyed", arena, 1)
    assert _descriptor(runtime, arena) == (0, 0, 0, 0)


def test_arena_allot_exposes_wrapped_and_signed_high_cell_requests(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    arena, status = _new(runtime, 64, 1)
    assert status == 0
    base = _descriptor(runtime, arena)[0]

    assert _execute(runtime, "ARENA-ALLOT?", arena, MASK64) == (base, 0)
    assert _descriptor(runtime, arena)[2] == base

    negative_eight = MASK64 - 7
    assert _execute(runtime, "ARENA-ALLOT?", arena, negative_eight) == (base, 0)
    assert _descriptor(runtime, arena)[2] == base - 8
    assert _execute(runtime, "ARENA-USED", arena) == (negative_eight,)
    assert _execute(runtime, "ARENA-FREE", arena) == (72,)
    assert _execute(runtime, "ARENA-DESTROY", arena) == ()


def test_arena_signed_capacity_can_reject_a_small_request(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    runtime.evaluate(
        b"CREATE AR-SIGNED-DESC 32 ALLOT\n",
        source_name="arena-signed-desc",
    )
    descriptor = _execute(runtime, "AR-SIGNED-DESC")[0]
    base = EXTERNAL_BASE + CANONICAL_INDEX_BYTES
    runtime.memory.write64(descriptor, base)
    runtime.memory.write64(descriptor + 8, 1 << 63)
    runtime.memory.write64(descriptor + 16, base)
    runtime.memory.write64(descriptor + 24, 2)

    assert _execute(runtime, "ARENA-FREE", descriptor) == (1 << 63,)
    assert _execute(runtime, "ARENA-ALLOT?", descriptor, 8) == (0, MASK64)
    assert _descriptor(runtime, descriptor)[2] == base


def test_arena_snapshots_validate_only_the_descriptor_interval(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    arena, status = _new(runtime, 64, 1)
    assert status == 0
    base = _descriptor(runtime, arena)[0]

    assert _execute(runtime, "ARENA-ALLOT", arena, 7) == (base,)
    snap = _execute(runtime, "ARENA-SNAP", arena)[0]
    assert snap == base + 8
    assert _execute(runtime, "ARENA-ALLOT", arena, 9) == (base + 8,)
    assert _execute(runtime, "ARENA-USED", arena) == (24,)
    assert _execute(runtime, "ARENA-ROLLBACK", arena, snap) == ()
    assert _execute(runtime, "ARENA-USED", arena) == (8,)
    assert _execute(runtime, "ARENA-SNAP-DROP", snap) == ()

    # Tokens are bare addresses: a forward or unaligned in-range value is
    # accepted even when it was never returned by ARENA-SNAP.
    assert _execute(runtime, "ARENA-ROLLBACK", arena, base + 48) == ()
    assert _execute(runtime, "ARENA-USED", arena) == (48,)
    assert _execute(runtime, "ARENA-ROLLBACK", arena, base + 3) == ()
    assert _execute(runtime, "ARENA-USED", arena) == (3,)
    pointer = _descriptor(runtime, arena)[2]

    _expect_abort(
        runtime,
        "ARENA-ROLLBACK",
        b"rollback: snap below base",
        arena,
        base - 1,
    )
    assert _descriptor(runtime, arena)[2] == pointer
    _expect_abort(
        runtime,
        "ARENA-ROLLBACK",
        b"rollback: snap above limit",
        arena,
        base + 65,
    )
    assert _descriptor(runtime, arena)[2] == pointer

    assert _execute(runtime, "ARENA-ROLLBACK", arena, base + 64) == ()
    assert _execute(runtime, "ARENA-FREE", arena) == (0,)
    assert _execute(runtime, "ARENA-RESET", arena) == ()
    assert _execute(runtime, "ARENA-USED", arena) == (0,)
    assert _execute(runtime, "ARENA-DESTROY", arena) == ()
    assert _execute(runtime, "ARENA-ROLLBACK", arena, 0) == ()
    assert _descriptor(runtime, arena) == (0, 0, 0, 0)


def test_scoped_arena_stack_is_bounded_and_runtime_global(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    _expect_abort(runtime, "CURRENT-ARENA", b"no current arena")
    _expect_abort(runtime, "ARENA-POP", b"arena stack underflow")

    first, first_status = _new(runtime, 64, 1)
    second, second_status = _new(runtime, 64, 1)
    assert first_status == second_status == 0
    assert _execute(runtime, "ARENA-PUSH", first) == ()
    assert _execute(runtime, "CURRENT-ARENA") == (first,)

    peer = runtime.new_context()
    runtime.execute("CURRENT-ARENA", context=peer, step_budget=250_000)
    assert peer.data.snapshot() == (first,)
    peer.data.clear()
    peer.data.push(9)
    runtime.execute("AALLOT", context=peer, step_budget=250_000)
    first_base = _descriptor(runtime, first)[0]
    assert peer.data.snapshot() == (first_base,)
    assert peer.returns.snapshot() == ()
    peer.data.clear()
    assert _execute(runtime, "ARENA-USED", first) == (16,)

    assert _execute(runtime, "ARENA-PUSH", second) == ()
    second_base = _descriptor(runtime, second)[0]
    assert _execute(runtime, "AALLOT", 1) == (second_base,)
    assert _execute(runtime, "ARENA-POP") == ()
    assert _execute(runtime, "CURRENT-ARENA") == (first,)
    assert _execute(runtime, "ARENA-POP") == ()

    for _ in range(4):
        assert _execute(runtime, "ARENA-PUSH", first) == ()
    arena_stack = _execute(runtime, "ARENA-STK")[0]
    assert tuple(
        runtime.memory.read64(arena_stack + offset) for offset in range(0, 32, 8)
    ) == (first, first, first, first)
    _expect_abort(runtime, "ARENA-PUSH", b"arena stack full", second)
    assert _pointer(runtime, "ARENA-SP") == 4
    for _ in range(4):
        assert _execute(runtime, "ARENA-POP") == ()
    assert tuple(
        runtime.memory.read64(arena_stack + offset) for offset in range(0, 32, 8)
    ) == (first, first, first, first)
    _expect_abort(runtime, "ARENA-POP", b"arena stack underflow")

    assert _execute(runtime, "ARENA-DESTROY", second) == ()
    assert _execute(runtime, "ARENA-DESTROY", first) == ()


def test_arena_diagnostic_renders_live_descriptor_state(
    loaded_arena: MegaForthRuntime,
) -> None:
    runtime = loaded_arena
    arena, status = _new(runtime, 24, 1)
    assert status == 0
    base = _descriptor(runtime, arena)[0]
    assert _execute(runtime, "ARENA-ALLOT", arena, 1) == (base,)

    assert _execute(runtime, ".ARENA", arena) == ()
    assert runtime.drain_uart_output() == (
        f"Arena: base={base}  size=24  used=8  free=16  src=xmem\r\n".encode(
            "ascii"
        )
    )
    assert _execute(runtime, "ARENA-DESTROY", arena) == ()


@pytest.mark.parametrize("published_cells", (0, 1, 2, 3))
def test_arena_new_can_consume_backing_before_partial_descriptor_fault(
    published_cells: int,
) -> None:
    runtime = _runtime_with_regions()
    runtime.evaluate(
        b": AR-DICT-FAIL 64 A-HBW ARENA-NEW ;\n",
        source_name="arena-dictionary-failure",
    )
    system_here = runtime.dictionary.here
    assert _execute(runtime, "USERLAND-INIT") == ()
    assert _execute(runtime, "ENTER-USERLAND") == ()
    user_limit = _pointer(runtime, "U-DICT-LIMIT")
    remaining = published_cells * 8
    assert _execute(
        runtime,
        "ALLOT",
        _execute(runtime, "U-FREE")[0] - remaining,
    ) == ()
    descriptor = runtime.dictionary.here
    assert descriptor == user_limit - remaining
    runtime.memory.fill(descriptor, 32, 0xA5)
    hbw_here = _pointer(runtime, "HBW-HERE")

    assert _catch(runtime, "AR-DICT-FAIL") == (u64(-8),)
    assert runtime.dictionary.here == user_limit
    assert _pointer(runtime, "HBW-HERE") == hbw_here + 64
    assert _pointer(runtime, "AR-BLK") == hbw_here
    expected = (hbw_here, 64, hbw_here, 2)
    for index, value in enumerate(expected):
        observed = runtime.memory.read64(descriptor + index * 8)
        if index < published_cells:
            assert observed == value
        else:
            assert observed == 0xA5A5_A5A5_A5A5_A5A5

    assert _execute(runtime, "LEAVE-USERLAND") == ()
    assert runtime.dictionary.here == system_here
    assert _execute(runtime, "HBW-RESET") == ()


def test_next_contiguous_frontier_stops_at_buffer_idle_left_bracket(
    loaded_arena: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[LAST_LINE:2796])
    assert next_source.startswith(b"\n")
    assert next_source.endswith(
        (
            ": IDLE  ( -- )  [ 0 C, ] ;  \\ IDL opcode — "
            "yield CPU until next interrupt\n"
        ).encode()
    )

    with pytest.raises(SourceError, match="unknown word") as caught:
        loaded_arena.evaluate(
            next_source,
            source_name="kdos.f:2781-2796",
        )
    assert caught.value.location.line == 16
    assert caught.value.location.column == 16
    assert caught.value.message == "unknown word b'['"
