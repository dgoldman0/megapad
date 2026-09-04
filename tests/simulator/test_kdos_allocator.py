"""Unchanged-source acceptance for KDOS's complete Bank-0 allocator section."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from simulator.errors import ForthAbort
from simulator.memory import BANK0_DEFAULT_SIZE
from simulator.runtime import MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE_DIRECTORY = Path(__file__).with_name("fixtures")
BASE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-39-69.f"
PARSE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-71-115.f"
ALLOCATOR_FIXTURE = FIXTURE_DIRECTORY / "kdos-allocator-116-545.f"

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"

BASE_FIRST_LINE = 39
BASE_LAST_LINE = 69
BASE_SHA256 = "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c"
BASE_GIT_BLOB = "ecef2fef19b54559367f1a162a97558776ab6ee8"

PARSE_FIRST_LINE = 71
PARSE_LAST_LINE = 115
PARSE_SHA256 = "a59c8811eef09b2a1bd31b5c0801b68a29cf1434c67bdc17a63d15e60d69a99c"
PARSE_GIT_BLOB = "fbfea6100b2dff8925dde073a7bd35a3f88544dc"

ALLOCATOR_FIRST_LINE = 116
ALLOCATOR_LAST_LINE = 545
ALLOCATOR_SHA256 = "0a7d819a0a17ab96378771f69e6ca3dbf2bc2570028977a713bcba0742e22106"
ALLOCATOR_GIT_BLOB = "46dcb6e2c82d57904f7d92d43292bf3670ba5347"
ALLOCATOR_DEFINITIONS = (
    b"/ALLOC-HDR",
    b"ALLOC-MAGIC",
    b"HEAP-BASE",
    b"HEAP-FREE",
    b"HEAP-INIT",
    b"?DICT-ROOM",
    b"MEM-SIZE",
    b"MICRO-CORE?",
    b"FULL-CORE?",
    b"N-FULL-CORES",
    b"A-PREV",
    b"A-CURR",
    b"A-SIZE",
    b"HEAP-GUARD",
    b"LATE-DICT-RESERVE",
    b"HEAP-SETUP",
    b"(LINK-PREV!)",
    b"?CORE0",
    b"(BANK0-ALLOCATE)",
    b"(COALESCE)",
    b"(BANK0-FREE)",
    b"R-BLK",
    b"R-OLD",
    b"R-NEW",
    b"(TRY-GROW)",
    b"(BANK0-RESIZE)",
    b"HEAP-FREE-BYTES",
    b"HEAP-FRAG",
    b"HEAP-LARGEST",
    b"HEAP-CHECK",
    b".HEAP",
    b"HEAP-VERIFY",
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice(
    fixture: Path,
    *,
    first_line: int,
    last_line: int,
    sha256: str,
    git_blob: str,
) -> bytes:
    source = fixture.read_bytes()
    assert hashlib.sha256(source).hexdigest() == sha256
    assert _git_blob_id(source) == git_blob

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[first_line - 1 : last_line])
    return source


def _load_allocator() -> MegaForthRuntime:
    slices = (
        (
            BASE_FIXTURE,
            BASE_FIRST_LINE,
            BASE_LAST_LINE,
            BASE_SHA256,
            BASE_GIT_BLOB,
        ),
        (
            PARSE_FIXTURE,
            PARSE_FIRST_LINE,
            PARSE_LAST_LINE,
            PARSE_SHA256,
            PARSE_GIT_BLOB,
        ),
        (
            ALLOCATOR_FIXTURE,
            ALLOCATOR_FIRST_LINE,
            ALLOCATOR_LAST_LINE,
            ALLOCATOR_SHA256,
            ALLOCATOR_GIT_BLOB,
        ),
    )
    runtime = MegaForthRuntime()
    results = []
    for fixture, first, last, sha256, git_blob in slices:
        source = _verified_slice(
            fixture,
            first_line=first,
            last_line=last,
            sha256=sha256,
            git_blob=git_blob,
        )
        results.append(
            runtime.evaluate(
                source,
                source_name=f"kdos.f@{MEGAPAD_REVISION}:{first}-{last}",
            )
        )

    assert tuple(word.name for word in results[-1].definitions) == (
        ALLOCATOR_DEFINITIONS
    )
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


@pytest.fixture
def loaded_allocator() -> MegaForthRuntime:
    return _load_allocator()


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _cell(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(_body(runtime, name))


def _execute_cells(
    runtime: MegaForthRuntime,
    name: str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name)
    result = context.data.snapshot()
    context.data.clear()
    assert context.returns.snapshot() == ()
    return result


def _setup_heap(runtime: MegaForthRuntime) -> tuple[int, int]:
    before = runtime.dictionary.here
    assert _execute_cells(runtime, "HEAP-SETUP") == ()
    aligned_here = (before + 63) & ~63
    heap_base = aligned_here + 32768
    heap_end = BANK0_DEFAULT_SIZE // 2 - 4096
    initial_size = heap_end - heap_base - 24
    assert runtime.dictionary.here == aligned_here
    assert _cell(runtime, "HEAP-BASE") == heap_base
    assert _cell(runtime, "HEAP-FREE") == heap_base
    assert _cell(runtime, "HEAP-INIT") == 1
    assert runtime.memory.read64(heap_base) == 0
    assert runtime.memory.read64(heap_base + 8) == initial_size
    assert runtime.memory.read64(heap_base + 16) == 0
    return heap_base, initial_size


def test_allocator_slice_loads_exact_definitions_and_zero_state(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator

    assert _execute_cells(runtime, "/ALLOC-HDR") == (24,)
    assert _execute_cells(runtime, "ALLOC-MAGIC") == (0xA110_CA7E_DEAD_BEEF,)
    assert _execute_cells(runtime, "HEAP-GUARD") == (4096,)
    assert _execute_cells(runtime, "LATE-DICT-RESERVE") == (32768,)
    for name in (
        "HEAP-BASE",
        "HEAP-FREE",
        "HEAP-INIT",
        "A-PREV",
        "A-CURR",
        "A-SIZE",
        "R-BLK",
        "R-OLD",
        "R-NEW",
    ):
        assert _cell(runtime, name) == 0


def test_source_defined_sysinfo_and_core_classification_share_one_core_profile(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator

    runtime.evaluate(
        b"MEM-SIZE N-FULL N-FULL-CORES "
        b"0 FULL-CORE? 0 MICRO-CORE? "
        b"1 FULL-CORE? 1 MICRO-CORE?"
    )

    assert runtime.main_context.data.snapshot() == (
        BANK0_DEFAULT_SIZE,
        1,
        1,
        TRUE,
        0,
        0,
        TRUE,
    )


def test_dictionary_room_guard_rejects_negative_and_exact_heap_collision(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    context = runtime.main_context

    context.data.push(-1)
    with pytest.raises(ForthAbort):
        runtime.execute("?DICT-ROOM")
    assert runtime.drain_uart_output() == b"Invalid dictionary size"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()

    heap_base, _ = _setup_heap(runtime)
    exact_collision = heap_base - 256 - runtime.dictionary.here
    assert _execute_cells(runtime, "?DICT-ROOM", exact_collision - 1) == ()

    context.data.push(exact_collision)
    with pytest.raises(ForthAbort):
        runtime.execute("?DICT-ROOM")
    assert runtime.drain_uart_output() == b"dictionary into heap"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_dictionary_room_guard_uses_exact_live_stack_subtraction_geometry(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    context = runtime.main_context
    exact_collision = (
        context.data.empty_pointer - 272 - runtime.dictionary.here
    )

    assert _execute_cells(runtime, "?DICT-ROOM", exact_collision - 1) == ()

    for requested_size in (exact_collision, (1 << 63) - 1):
        context.data.push(requested_size)
        with pytest.raises(ForthAbort):
            runtime.execute("?DICT-ROOM")
        assert runtime.drain_uart_output() == b"dictionary overflow"
        assert context.data.snapshot() == ()
        assert context.returns.snapshot() == ()


def test_heap_setup_publishes_exact_header_and_is_idempotent(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)
    here = runtime.dictionary.here
    header = runtime.memory.read_bytes(heap_base, 24)

    assert _execute_cells(runtime, "HEAP-SETUP") == ()

    assert runtime.dictionary.here == here
    assert runtime.memory.read_bytes(heap_base, 24) == header
    assert runtime.memory.read64(heap_base + 8) == initial_size


@pytest.mark.parametrize("requested_size", (0, -1, 0x7FFF_FFFF_FFFF_FFF9))
def test_invalid_allocate_requests_do_not_lazily_initialize_heap(
    loaded_allocator: MegaForthRuntime,
    requested_size: int,
) -> None:
    runtime = loaded_allocator

    assert _execute_cells(runtime, "(BANK0-ALLOCATE)", requested_size) == (
        0,
        MASK64,
    )
    assert _cell(runtime, "HEAP-INIT") == 0
    assert _cell(runtime, "HEAP-BASE") == 0
    assert _cell(runtime, "HEAP-FREE") == 0


def test_first_allocation_rounds_to_minimum_and_splits_guest_header(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)

    address, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 1)

    assert status == 0
    assert address == heap_base + 24
    assert runtime.memory.read64(heap_base + 8) == 16
    assert runtime.memory.read64(heap_base + 16) == 0xA110_CA7E_DEAD_BEEF
    remainder = heap_base + 24 + 16
    assert _cell(runtime, "HEAP-FREE") == remainder
    assert runtime.memory.read64(remainder) == 0
    assert runtime.memory.read64(remainder + 8) == initial_size - 16 - 24
    assert runtime.memory.read64(remainder + 16) == 0


def test_whole_block_first_fit_skip_and_oom_preserve_the_guest_free_list(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)
    first, first_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    _blocker, blocker_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    assert (first_status, blocker_status) == (0, 0)
    assert _execute_cells(runtime, "(BANK0-FREE)", first) == ()

    whole, whole_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 24)
    assert (whole, whole_status) == (first, 0)
    assert runtime.memory.read64(first - 16) == 32
    assert _execute_cells(runtime, "(BANK0-FREE)", whole) == ()

    skipped, skipped_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 48)
    assert skipped_status == 0
    assert skipped != first
    assert _cell(runtime, "HEAP-FREE") == heap_base
    second_free = runtime.memory.read64(heap_base)
    assert second_free != 0
    before = (
        runtime.memory.read_bytes(heap_base, 24),
        runtime.memory.read_bytes(second_free, 24),
    )

    assert _execute_cells(runtime, "(BANK0-ALLOCATE)", initial_size) == (
        0,
        MASK64,
    )
    assert (
        runtime.memory.read_bytes(heap_base, 24),
        runtime.memory.read_bytes(second_free, 24),
    ) == before


@pytest.mark.parametrize("reverse_order", (False, True))
def test_both_free_orders_coalesce_back_to_one_exact_block(
    loaded_allocator: MegaForthRuntime,
    reverse_order: bool,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)
    first, first_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    second, second_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 48)
    assert (first_status, second_status) == (0, 0)

    order = (second, first) if reverse_order else (first, second)
    for address in order:
        assert _execute_cells(runtime, "(BANK0-FREE)", address) == ()

    assert _cell(runtime, "HEAP-FREE") == heap_base
    assert runtime.memory.read64(heap_base) == 0
    assert runtime.memory.read64(heap_base + 8) == initial_size
    assert runtime.memory.read64(heap_base + 16) == 0
    assert _execute_cells(runtime, "HEAP-FRAG") == (1,)
    assert _execute_cells(runtime, "HEAP-VERIFY") == (TRUE,)


def test_null_free_is_noop_and_double_free_aborts_with_exact_diagnostic(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)
    header = runtime.memory.read_bytes(heap_base, 24)
    assert _execute_cells(runtime, "(BANK0-FREE)", 0) == ()
    assert runtime.memory.read_bytes(heap_base, 24) == header

    address, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 24)
    assert status == 0
    assert _execute_cells(runtime, "(BANK0-FREE)", address) == ()

    runtime.main_context.data.push(address)
    with pytest.raises(ForthAbort):
        runtime.execute("(BANK0-FREE)")
    assert runtime.drain_uart_output() == b"FREE: invalid or double-free"
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.memory.read64(heap_base + 8) == initial_size


def test_resize_shrink_in_place_grow_and_fallback_copy(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    _setup_heap(runtime)

    shrinking, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 128)
    assert status == 0
    runtime.memory.write_bytes(shrinking, bytes(range(128)))
    resized, resize_status = _execute_cells(
        runtime,
        "(BANK0-RESIZE)",
        shrinking,
        24,
    )
    assert (resized, resize_status) == (shrinking, 0)
    assert runtime.memory.read64(shrinking - 16) == 24
    assert runtime.memory.read_bytes(shrinking, 24) == bytes(range(24))

    assert _execute_cells(runtime, "(BANK0-FREE)", shrinking) == ()
    first, first_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    adjacent, adjacent_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 64)
    assert (first_status, adjacent_status) == (0, 0)
    assert _execute_cells(runtime, "(BANK0-FREE)", adjacent) == ()
    grown, grow_status = _execute_cells(runtime, "(BANK0-RESIZE)", first, 80)
    assert (grown, grow_status) == (first, 0)
    assert runtime.memory.read64(first - 16) == 80

    assert _execute_cells(runtime, "(BANK0-FREE)", first) == ()
    source, source_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    blocker, blocker_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 64)
    assert (source_status, blocker_status) == (0, 0)
    payload = b"semantic allocator copy path!!!!"
    assert len(payload) == 32
    runtime.memory.write_bytes(source, payload)

    replacement, fallback_status = _execute_cells(
        runtime,
        "(BANK0-RESIZE)",
        source,
        96,
    )

    assert fallback_status == 0
    assert replacement != source
    assert runtime.memory.read_bytes(replacement, 32) == payload
    assert runtime.memory.read64(source - 8) == 0
    assert runtime.memory.read64(replacement - 8) == 0xA110_CA7E_DEAD_BEEF


def test_resize_consumes_an_exact_isolated_adjacent_block_without_a_remnant(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    _setup_heap(runtime)
    address, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    adjacent, adjacent_status = _execute_cells(
        runtime,
        "(BANK0-ALLOCATE)",
        48,
    )
    blocker, blocker_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    assert (status, adjacent_status, blocker_status) == (0, 0, 0)
    assert _execute_cells(runtime, "(BANK0-FREE)", adjacent) == ()

    resized, resize_status = _execute_cells(
        runtime,
        "(BANK0-RESIZE)",
        address,
        104,
    )

    assert (resized, resize_status) == (address, 0)
    assert runtime.memory.read64(address - 16) == 104
    assert address + 104 == blocker - 24


@pytest.mark.parametrize("requested_size", (0, -1, 0x7FFF_FFFF_FFFF_FFF9))
def test_invalid_resize_records_current_zero_address_failure_discrepancy(
    loaded_allocator: MegaForthRuntime,
    requested_size: int,
) -> None:
    runtime = loaded_allocator
    _setup_heap(runtime)
    address, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    assert status == 0
    payload = b"r" * 32
    runtime.memory.write_bytes(address, payload)

    assert _execute_cells(
        runtime,
        "(BANK0-RESIZE)",
        address,
        requested_size,
    ) == (0, MASK64)
    assert runtime.memory.read_bytes(address, 32) == payload
    assert runtime.memory.read64(address - 16) == 32
    assert runtime.memory.read64(address - 8) == 0xA110_CA7E_DEAD_BEEF


def test_failed_resize_preserves_original_allocation_and_bytes(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    _setup_heap(runtime)
    address, status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    assert status == 0
    payload = b"x" * 32
    runtime.memory.write_bytes(address, payload)

    original, failure = _execute_cells(
        runtime,
        "(BANK0-RESIZE)",
        address,
        1 << 62,
    )

    assert (original, failure) == (address, MASK64)
    assert runtime.memory.read_bytes(address, 32) == payload
    assert runtime.memory.read64(address - 16) == 32
    assert runtime.memory.read64(address - 8) == 0xA110_CA7E_DEAD_BEEF


def test_fragmented_statistics_walk_every_guest_free_list_node(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    _setup_heap(runtime)
    first, first_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 32)
    middle, middle_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 64)
    last, last_status = _execute_cells(runtime, "(BANK0-ALLOCATE)", 96)
    assert (first_status, middle_status, last_status) == (0, 0, 0)
    assert _execute_cells(runtime, "(BANK0-FREE)", first) == ()
    assert _execute_cells(runtime, "(BANK0-FREE)", last) == ()

    head = _cell(runtime, "HEAP-FREE")
    second = runtime.memory.read64(head)
    assert head == first - 24
    assert second == last - 24
    assert runtime.memory.read64(second) == 0
    sizes = (
        runtime.memory.read64(head + 8),
        runtime.memory.read64(second + 8),
    )

    assert _execute_cells(runtime, "HEAP-FREE-BYTES") == (sum(sizes),)
    assert _execute_cells(runtime, "HEAP-FRAG") == (2,)
    assert _execute_cells(runtime, "HEAP-LARGEST") == (max(sizes),)
    assert _execute_cells(runtime, "HEAP-VERIFY") == (TRUE,)
    assert _execute_cells(runtime, "(BANK0-FREE)", middle) == ()
    assert _execute_cells(runtime, "HEAP-FRAG") == (1,)


def test_heap_statistics_verifier_and_summary_use_live_guest_list(
    loaded_allocator: MegaForthRuntime,
) -> None:
    runtime = loaded_allocator
    heap_base, initial_size = _setup_heap(runtime)

    assert _execute_cells(runtime, "HEAP-FREE-BYTES") == (initial_size,)
    assert _execute_cells(runtime, "HEAP-FRAG") == (1,)
    assert _execute_cells(runtime, "HEAP-LARGEST") == (initial_size,)
    assert _execute_cells(runtime, "HEAP-CHECK") == (TRUE,)
    assert _execute_cells(runtime, "HEAP-VERIFY") == (TRUE,)

    assert _execute_cells(runtime, ".HEAP") == ()
    assert runtime.drain_uart_output() == (
        f" Heap: base={heap_base}   free={initial_size}  bytes"
        f"  blocks=1   largest={initial_size}   safe=yes\r\n"
    ).encode("ascii")

    runtime.memory.write64(heap_base + 16, 1)
    assert _execute_cells(runtime, "HEAP-VERIFY") == (0,)
    assert runtime.drain_uart_output() == (
        b"heap: free block has non-zero magic\r\n"
    )
