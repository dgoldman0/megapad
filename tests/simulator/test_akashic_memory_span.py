"""Unchanged-source acceptance for Akashic's bounded memory-span sets."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import FALSE, MASK64, TRUE
from simulator.bootstrap_loader import BootstrapModule, BootstrapSourceLoader
from simulator.memory import RegionAllocator
from simulator.runtime import MegaForthRuntime


FIXTURES = Path(__file__).with_name("fixtures")
UINT_RANGE_FIXTURE = FIXTURES / "uint-range.f"
MEMORY_SPAN_FIXTURE = FIXTURES / "memory-span.f"

# The cross-repository input is pinned to this immutable Akashic revision and
# Git blob.  A later Akashic checkout may advance independently without
# silently changing the semantic acceptance proof kept in MegaPad.
AKASHIC_REVISION = "8e65ccf5e62d00b47e4cb846a379d12ae9297f3b"
MEMORY_SPAN_GIT_BLOB = "1645e8a6a50cb40cd2b12cd642e894a19fe4a0a6"
UINT_RANGE_GIT_BLOB = "a696d3f979dd08a4c744c81b3db08f2d933ef86b"
MEMORY_SPAN_SHA256 = (
    "13368afd08eac391b65344567aeab0ed00b3a72e51363af203e0514b98d98d52"
)
UINT_RANGE_SHA256 = (
    "11b9b0d2a87466aec24b1952f921226f4ae4681e396a677136a5d17152b103e8"
)

MEMORY_SPAN_DEFINITIONS = (
    b"MSPAN-NONWRAPPING?",
    b"MSPAN-OVERLAP?",
    b"MSPAN-SET-S-OK",
    b"MSPAN-SET-S-INVALID",
    b"MSPAN-SET-S-OVERLAP",
    b"MSPAN-SET-S-CAPACITY",
    b"MSPAN-SET-ENTRY-SIZE",
    b"MSPAN-SET-HEADER-SIZE",
    b"_MSS-COUNT",
    b"_MSS-CAPACITY",
    b"_MSS-ENTRIES",
    b"MSPAN-SET.COUNT",
    b"_MSPAN-SET.CAPACITY",
    b"_MSPAN-SET.ENTRIES",
    b"_MSPAN-SET-CAPACITY-MAX",
    b"MSPAN-SET-BYTES",
    b"MSPAN-SET-COUNT@",
    b"MSPAN-SET-CAPACITY@",
    b"_MSPAN-SET-NTH",
    b"_MSPAN-SET-ENTRIES-VALID?",
    b"MSPAN-SET-VALID?",
    b"MSPAN-SET-INIT",
    b"MSPAN-SET-CLEAR",
    b"MSPAN-SET-OVERLAP?",
    b"MSPAN-SET-PUSH",
    b"MSPAN-SET-ADD",
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _read_verified_sources() -> tuple[bytes, bytes]:
    uint_range = UINT_RANGE_FIXTURE.read_bytes()
    memory_span = MEMORY_SPAN_FIXTURE.read_bytes()
    assert hashlib.sha256(uint_range).hexdigest() == UINT_RANGE_SHA256
    assert hashlib.sha256(memory_span).hexdigest() == MEMORY_SPAN_SHA256
    assert _git_blob_id(uint_range) == UINT_RANGE_GIT_BLOB
    assert _git_blob_id(memory_span) == MEMORY_SPAN_GIT_BLOB
    return uint_range, memory_span


@pytest.fixture
def runtime() -> MegaForthRuntime:
    uint_range, memory_span = _read_verified_sources()
    hosted = MegaForthRuntime()
    loader = BootstrapSourceLoader(
        hosted,
        (
            BootstrapModule(
                request_name=b"uint-range.f",
                provided_id=b"akashic-uint-range",
                source_name=f"akashic@{AKASHIC_REVISION}/utils/uint-range.f",
                source=uint_range,
            ),
            BootstrapModule(
                request_name=b"memory-span.f",
                provided_id=b"akashic-memory-span",
                source_name=f"akashic@{AKASHIC_REVISION}/utils/memory-span.f",
                source=memory_span,
            ),
        ),
    )
    loader.install()
    result = loader.load(b"memory-span.f")

    assert result is not None
    assert result.source_name == (
        f"akashic@{AKASHIC_REVISION}/utils/memory-span.f"
    )
    assert tuple(word.name for word in result.definitions) == MEMORY_SPAN_DEFINITIONS
    assert hosted.provided_modules == frozenset(
        (b"akashic-memory-span", b"akashic-uint-range")
    )
    assert loader.load(b"memory-span.f") is None
    return hosted


def _run(
    runtime: MegaForthRuntime,
    name: bytes | str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.new_context()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name, context=context)
    assert context.returns.snapshot() == ()
    return context.data.snapshot()


def _allocate_set(runtime: MegaForthRuntime, capacity: int) -> tuple[int, int]:
    allocator = RegionAllocator(runtime.memory, 0x10_000, 0x20_000)
    size, = _run(runtime, "MSPAN-SET-BYTES", capacity)
    address = allocator.allocate(size)
    assert address is not None
    return address, size


def test_snapshot_provenance_and_exact_source_load(runtime: MegaForthRuntime) -> None:
    assert runtime.find("URANGE-VALID?") is not None
    assert runtime.find("URANGE-OVERLAP?") is not None
    for name in MEMORY_SPAN_DEFINITIONS:
        assert runtime.find(name) is not None


def test_exported_constants_and_inline_field_addresses(
    runtime: MegaForthRuntime,
) -> None:
    expected_constants = {
        "MSPAN-SET-S-OK": 0,
        "MSPAN-SET-S-INVALID": 1,
        "MSPAN-SET-S-OVERLAP": 2,
        "MSPAN-SET-S-CAPACITY": 3,
        "MSPAN-SET-ENTRY-SIZE": 16,
        "MSPAN-SET-HEADER-SIZE": 16,
        "_MSS-COUNT": 0,
        "_MSS-CAPACITY": 8,
        "_MSS-ENTRIES": 16,
        "_MSPAN-SET-CAPACITY-MAX": (MASK64 >> 5) - 1,
    }
    for name, expected in expected_constants.items():
        assert _run(runtime, name) == (expected,)

    set_address = 0x10_000
    assert _run(runtime, "MSPAN-SET.COUNT", set_address) == (set_address,)
    assert _run(runtime, "_MSPAN-SET.CAPACITY", set_address) == (
        set_address + 8,
    )
    assert _run(runtime, "_MSPAN-SET.ENTRIES", set_address) == (
        set_address + 16,
    )
    assert _run(runtime, "_MSPAN-SET-NTH", 2, set_address) == (
        set_address + 48,
    )


@pytest.mark.parametrize(
    ("word", "inputs", "expected"),
    (
        ("MSPAN-NONWRAPPING?", (1000, 16), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (1000, 0), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (0, 0), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (0, 1), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (1000, -1), (FALSE,)),
        ("MSPAN-NONWRAPPING?", (-1, 0), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (-8, 7), (TRUE,)),
        ("MSPAN-NONWRAPPING?", (-8, 8), (FALSE,)),
        ("MSPAN-OVERLAP?", (1000, 10, 1010, 5), (FALSE,)),
        ("MSPAN-OVERLAP?", (1010, 5, 1000, 10), (FALSE,)),
        ("MSPAN-OVERLAP?", (1000, 11, 1010, 5), (TRUE,)),
        ("MSPAN-OVERLAP?", (1010, 5, 1000, 11), (TRUE,)),
        ("MSPAN-OVERLAP?", (1000, 20, 1005, 5), (TRUE,)),
        ("MSPAN-OVERLAP?", (1000, 20, 1000, 20), (TRUE,)),
        ("MSPAN-OVERLAP?", (1000, 0, 1000, 1), (FALSE,)),
        ("MSPAN-OVERLAP?", (0, 4, 2, 2), (TRUE,)),
        ("MSPAN-OVERLAP?", (-8, 8, -4, 1), (FALSE,)),
        ("MSPAN-OVERLAP?", (-16, 8, -12, 4), (TRUE,)),
    ),
)
def test_gate2a_scalar_span_vectors(
    runtime: MegaForthRuntime,
    word: str,
    inputs: tuple[int, ...],
    expected: tuple[int, ...],
) -> None:
    assert _run(runtime, word, *inputs) == expected


def test_set_layout_init_and_capacity_bounds(runtime: MegaForthRuntime) -> None:
    capacity_max = (MASK64 >> 5) - 1
    assert _run(runtime, "MSPAN-SET-BYTES", 0) == (16,)
    assert _run(runtime, "MSPAN-SET-BYTES", 4) == (80,)
    assert _run(runtime, "MSPAN-SET-BYTES", -1) == (0,)
    assert _run(runtime, "MSPAN-SET-BYTES", capacity_max) == (
        (1 << 63) - 16,
    )
    assert _run(runtime, "MSPAN-SET-BYTES", capacity_max + 1) == (0,)

    set_address, set_size = _allocate_set(runtime, 4)
    runtime.memory.fill(set_address, set_size, 0xA5)
    assert _run(runtime, "MSPAN-SET-INIT", 4, set_address) == (0,)
    assert runtime.memory.read_bytes(set_address, set_size) == (
        bytes(8) + (4).to_bytes(8, "little") + bytes(64)
    )
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (TRUE,)
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-CAPACITY@", set_address) == (4,)

    empty_set = 0x12_000
    runtime.memory.fill(empty_set, 16, 0xA5)
    assert _run(runtime, "MSPAN-SET-INIT", 0, empty_set) == (0,)
    assert runtime.memory.read_bytes(empty_set, 16) == bytes(16)
    assert _run(runtime, "MSPAN-SET-VALID?", empty_set) == (TRUE,)


def test_add_preserves_adjacent_entries_and_rejects_overlap_atomically(
    runtime: MegaForthRuntime,
) -> None:
    set_address, set_size = _allocate_set(runtime, 4)
    assert _run(runtime, "MSPAN-SET-INIT", 4, set_address) == (0,)

    assert _run(runtime, "MSPAN-SET-ADD", 1000, 10, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-ADD", 1010, 5, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (2,)
    assert (
        runtime.memory.read64(set_address + 16),
        runtime.memory.read64(set_address + 24),
        runtime.memory.read64(set_address + 32),
        runtime.memory.read64(set_address + 40),
    ) == (1000, 10, 1010, 5)

    before_failure = runtime.memory.read_bytes(set_address, set_size)
    assert _run(runtime, "MSPAN-SET-ADD", 1000, 10, set_address) == (2,)
    assert runtime.memory.read_bytes(set_address, set_size) == before_failure
    assert _run(runtime, "MSPAN-SET-ADD", 1005, 2, set_address) == (2,)
    assert runtime.memory.read_bytes(set_address, set_size) == before_failure

    assert _run(runtime, "MSPAN-SET-OVERLAP?", 1009, 2, set_address) == (
        TRUE,
    )
    assert _run(runtime, "MSPAN-SET-OVERLAP?", 1015, 1, set_address) == (
        FALSE,
    )
    assert _run(runtime, "MSPAN-SET-ADD", 0, 0, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-OVERLAP?", 1000, 0, set_address) == (
        FALSE,
    )
    assert _run(runtime, "MSPAN-SET-ADD", 2000, 1, set_address) == (0,)

    full = runtime.memory.read_bytes(set_address, set_size)
    assert _run(runtime, "MSPAN-SET-ADD", 1001, 1, set_address) == (2,)
    assert runtime.memory.read_bytes(set_address, set_size) == full
    assert _run(runtime, "MSPAN-SET-ADD", 3000, 1, set_address) == (3,)
    assert runtime.memory.read_bytes(set_address, set_size) == full
    assert _run(runtime, "MSPAN-SET-PUSH", 3000, 1, set_address) == (3,)
    assert runtime.memory.read_bytes(set_address, set_size) == full
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (4,)


def test_push_clear_and_borrowed_geometry_contract(runtime: MegaForthRuntime) -> None:
    allocator = RegionAllocator(runtime.memory, 0x30_000, 0x40_000)
    set_size, = _run(runtime, "MSPAN-SET-BYTES", 3)
    set_address = allocator.allocate(set_size)
    borrowed = allocator.allocate(16)
    assert set_address is not None
    assert borrowed is not None
    assert _run(runtime, "MSPAN-SET-INIT", 3, set_address) == (0,)

    assert _run(runtime, "MSPAN-SET-PUSH", 1000, 10, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-PUSH", 1005, 10, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (2,)
    assert runtime.memory.read_bytes(set_address + 16, 32) == (
        (1000).to_bytes(8, "little")
        + (10).to_bytes(8, "little")
        + (1005).to_bytes(8, "little")
        + (10).to_bytes(8, "little")
    )

    stale_entries = runtime.memory.read_bytes(set_address + 16, 32)
    assert _run(runtime, "MSPAN-SET-CLEAR", set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-CAPACITY@", set_address) == (3,)
    assert runtime.memory.read_bytes(set_address + 16, 32) == stale_entries
    assert _run(runtime, "MSPAN-SET-CLEAR", set_address) == (0,)
    runtime.memory.fill(borrowed, 16, ord("a"))
    assert _run(runtime, "MSPAN-SET-ADD", borrowed, 16, set_address) == (0,)
    runtime.memory.fill(borrowed, 16, ord("z"))
    assert _run(runtime, "MSPAN-SET-OVERLAP?", borrowed, 1, set_address) == (
        TRUE,
    )

    entry_bytes = runtime.memory.read_bytes(set_address + 16, 16)
    assert _run(runtime, "MSPAN-SET-CLEAR", set_address) == (0,)
    assert runtime.memory.read_bytes(borrowed, 16) == b"z" * 16
    assert runtime.memory.read_bytes(set_address + 16, 16) == entry_bytes
    assert _run(runtime, "MSPAN-SET-COUNT@", set_address) == (0,)


def test_invalid_geometry_and_malformed_sets_fail_without_mutation(
    runtime: MegaForthRuntime,
) -> None:
    set_address, set_size = _allocate_set(runtime, 3)
    runtime.memory.fill(set_address, set_size, 0x5A)
    untouched = runtime.memory.read_bytes(set_address, set_size)

    assert _run(runtime, "MSPAN-SET-INIT", -1, set_address) == (1,)
    assert runtime.memory.read_bytes(set_address, set_size) == untouched
    assert _run(runtime, "MSPAN-SET-INIT", 3, 0) == (1,)
    assert _run(runtime, "MSPAN-SET-INIT", 3, MASK64 - 31) == (1,)
    assert runtime.memory.read_bytes(set_address, set_size) == untouched

    assert _run(runtime, "MSPAN-SET-INIT", 3, set_address) == (0,)
    assert _run(runtime, "MSPAN-SET-ADD", 1000, 10, set_address) == (0,)
    valid = runtime.memory.read_bytes(set_address, set_size)
    assert _run(runtime, "MSPAN-SET-ADD", -8, 8, set_address) == (1,)
    assert _run(runtime, "MSPAN-SET-PUSH", 1000, -1, set_address) == (1,)
    assert runtime.memory.read_bytes(set_address, set_size) == valid

    runtime.memory.write64(set_address + 24, MASK64)
    malformed = runtime.memory.read_bytes(set_address, set_size)
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (FALSE,)
    assert _run(runtime, "MSPAN-SET-OVERLAP?", 1000, 1, set_address) == (
        FALSE,
    )
    assert _run(runtime, "MSPAN-SET-ADD", 2000, 1, set_address) == (1,)
    assert _run(runtime, "MSPAN-SET-CLEAR", set_address) == (1,)
    assert runtime.memory.read_bytes(set_address, set_size) == malformed

    runtime.memory.write64(set_address + 24, 10)
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (TRUE,)
    runtime.memory.write64(set_address, 4)
    overfull = runtime.memory.read_bytes(set_address, set_size)
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (FALSE,)
    assert _run(runtime, "MSPAN-SET-PUSH", 2000, 1, set_address) == (1,)
    assert runtime.memory.read_bytes(set_address, set_size) == overfull

    runtime.memory.write64(set_address, 1)
    runtime.memory.write64(set_address + 8, MASK64)
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (FALSE,)
    runtime.memory.write64(set_address + 8, ((MASK64 >> 5) - 1) + 1)
    assert _run(runtime, "MSPAN-SET-VALID?", set_address) == (FALSE,)
    assert _run(runtime, "MSPAN-SET-VALID?", 0) == (FALSE,)
    assert _run(runtime, "MSPAN-SET-VALID?", MASK64 - 7) == (FALSE,)
