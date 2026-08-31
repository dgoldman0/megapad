"""Unchanged KDOS boot initialization of the caller-backed dictionary index."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.dictionary_index import (
    DICT_INDEX_AUTHORITATIVE,
    DICT_INDEX_BOUND,
    DICT_INDEX_SATURATED,
)
from simulator.errors import SourceError
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_x25519 import _execute
from tests.simulator.test_kdos_xmem import (
    CANONICAL_EXTERNAL_SIZE,
    _load_xmem,
    _pointer,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-dictionary-index-2390-2423.f"
)

FIRST_LINE = 2390
LAST_LINE = 2423
SLICE_SHA256 = (
    "58d2e25043e7c1c8c6442f60adee8d437610ee9f988533c1132ee4942004e3f3"
)
SLICE_GIT_BLOB = "9db364a360abe07bc4893c0e8970a412134e6a27"
DEFINITIONS = (
    b"_DICT-POW2-FLOOR",
    b"_DICT-INDEX-DONE",
    b"_DICT-INDEX-INIT",
)
BIOS_WORDS = (b"2/", b"2*", b"DICT-INDEX!", b"DICT-INDEX@")

CANONICAL_INDEX_SLOTS = 65_536
CANONICAL_INDEX_BYTES = CANONICAL_INDEX_SLOTS * 16


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 1_388
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


def _evaluate_dictionary_index(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_dictionary_index(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    return _evaluate_dictionary_index(_load_xmem(runtime))


@pytest.fixture
def loaded_dictionary_index() -> MegaForthRuntime:
    return _load_dictionary_index()


def _fold_ascii(name: bytes) -> bytes:
    return bytes(
        byte - 0x20 if 0x61 <= byte <= 0x7A else byte for byte in name
    )


def _fnv1a32(name: bytes) -> int:
    result = 0x811C_9DC5
    for byte in _fold_ascii(name):
        result ^= byte
        result = (result * 0x0100_0193) & 0xFFFF_FFFF
    return result


def _table_probe(
    runtime: MegaForthRuntime,
    name: bytes,
) -> tuple[int | None, int, int]:
    base, slots, _count, _flags = _execute(runtime, "DICT-INDEX@")
    if slots == 0:
        return None, 0, 0
    name_hash = _fnv1a32(name)
    slot_index = name_hash & (slots - 1)
    for _ in range(slots):
        slot = base + slot_index * 16
        entry = runtime.memory.read64(slot)
        metadata = runtime.memory.read64(slot + 8)
        if entry == 0:
            return slot, 0, metadata
        if (
            metadata & 0xFFFF_FFFF == name_hash
            and (metadata >> 32) & 0x7F == len(name)
        ):
            flags_length = runtime.memory.read8(entry + 8)
            candidate = runtime.memory.read_bytes(entry + 9, len(name))
            if (
                flags_length & 0x7F == len(name)
                and _fold_ascii(candidate) == _fold_ascii(name)
            ):
                return slot, entry, metadata
        slot_index = (slot_index + 1) & (slots - 1)
    return None, 0, 0


def _runtime_with_external_size(size: int) -> MegaForthRuntime:
    return _load_dictionary_index(
        MegaForthRuntime(memory=create_one_core_address_space(external_size=size))
    )


def test_dictionary_index_slice_is_exact_and_reserves_canonical_table(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    for name in DEFINITIONS + BIOS_WORDS:
        assert runtime.find(name) is not None

    unique_names = {word.name.upper() for word in runtime.dictionary.words}
    assert _execute(runtime, "DICT-INDEX@") == (
        EXTERNAL_BASE,
        CANONICAL_INDEX_SLOTS,
        len(unique_names),
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
    )
    assert _pointer(runtime, "_DICT-INDEX-DONE") == 1
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + CANONICAL_INDEX_BYTES
    assert _pointer(runtime, "XMEM-FLOOR") == EXTERNAL_BASE + CANONICAL_INDEX_BYTES
    assert _execute(runtime, "XMEM-FREE") == (
        CANONICAL_EXTERNAL_SIZE - CANONICAL_INDEX_BYTES,
    )

    here = _pointer(runtime, "XMEM-HERE")
    state = _execute(runtime, "DICT-INDEX@")
    assert _execute(runtime, "_DICT-INDEX-INIT") == ()
    assert _pointer(runtime, "XMEM-HERE") == here
    assert _execute(runtime, "DICT-INDEX@") == state


def test_power_floor_and_double_words_follow_executable_bios_cells(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    vectors = (
        (0, 0),
        (1, 1),
        (2, 2),
        (3, 2),
        (63, 32),
        (64, 64),
        (65, 64),
        (65_535, 32_768),
        (65_536, 65_536),
    )
    for value, expected in vectors:
        assert _execute(runtime, "_DICT-POW2-FLOOR", value) == (expected,)

    assert _execute(runtime, "2*", MASK64) == (MASK64 - 1,)
    assert _execute(runtime, "2/", MASK64) == (0x7FFF_FFFF_FFFF_FFFF,)


def test_rebuild_indexes_every_latest_binding_with_exact_slot_bytes(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    latest_by_name = {
        word.name.upper(): word for word in runtime.dictionary.words
    }
    occupied_slots: set[int] = set()

    for word in latest_by_name.values():
        slot, entry, metadata = _table_probe(runtime, word.name.swapcase())
        assert slot is not None
        assert entry == word.header_address
        assert metadata & 0xFFFF_FFFF == _fnv1a32(word.name)
        assert (metadata >> 32) & 0xFF == len(word.name)
        assert metadata >> 40 == 0
        occupied_slots.add(slot)

    assert len(occupied_slots) == len(latest_by_name)


def test_invalid_index_geometry_preserves_binding_and_complete_table(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    before_state = _execute(runtime, "DICT-INDEX@")
    before_table = hashlib.sha256(
        runtime.memory.read_bytes(EXTERNAL_BASE, CANONICAL_INDEX_BYTES)
    ).digest()
    invalid = (
        (0, 1),
        (EXTERNAL_BASE, 0),
        (EXTERNAL_BASE + 8, 2),
        (EXTERNAL_BASE, 3),
        (EXTERNAL_BASE, 1 << 60),
        (EXTERNAL_BASE - 16, 1),
        (0xFFFF_FFFF_FFFF_FFF0, 2),
        (EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE, 1),
        (EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE - 16, 2),
    )

    for base, slots in invalid:
        assert _execute(runtime, "DICT-INDEX!", base, slots) == (1,)
        assert _execute(runtime, "DICT-INDEX@") == before_state
        assert hashlib.sha256(
            runtime.memory.read_bytes(EXTERNAL_BASE, CANONICAL_INDEX_BYTES)
        ).digest() == before_table


def test_exact_external_end_is_valid_and_can_install_saturated() -> None:
    runtime = _load_dictionary_index()
    final_slot = EXTERNAL_BASE + CANONICAL_EXTERNAL_SIZE - 16

    assert _execute(runtime, "DICT-INDEX!", final_slot, 1) == (2,)
    assert _execute(runtime, "DICT-INDEX@") == (
        final_slot,
        1,
        1,
        DICT_INDEX_BOUND | DICT_INDEX_SATURATED,
    )
    newest = runtime.dictionary.latest_word
    assert newest is not None
    assert runtime.memory.read64(final_slot) == newest.header_address


def test_disable_leaves_table_bytes_and_linked_lookup_available(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    before_table = hashlib.sha256(
        runtime.memory.read_bytes(EXTERNAL_BASE, CANONICAL_INDEX_BYTES)
    ).digest()

    assert _execute(runtime, "DICT-INDEX!", 0, 0) == (0,)
    assert _execute(runtime, "DICT-INDEX@") == (0, 0, 0, 0)
    assert hashlib.sha256(
        runtime.memory.read_bytes(EXTERNAL_BASE, CANONICAL_INDEX_BYTES)
    ).digest() == before_table

    runtime.evaluate(b": LINKED-ONLY 77 ;\n", source_name="linked-only")
    assert _execute(runtime, "LINKED-ONLY") == (77,)
    assert _execute(runtime, "DICT-INDEX@") == (0, 0, 0, 0)
    assert hashlib.sha256(
        runtime.memory.read_bytes(EXTERNAL_BASE, CANONICAL_INDEX_BYTES)
    ).digest() == before_table

    assert _execute(
        runtime,
        "DICT-INDEX!",
        EXTERNAL_BASE,
        CANONICAL_INDEX_SLOTS,
    ) == (0,)
    _slot, entry, _metadata = _table_probe(runtime, b"linked-only")
    word = runtime.find("LINKED-ONLY")
    assert word is not None
    assert entry == word.header_address


def test_definition_publication_upserts_shadows_and_updates_count(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    before_count = _execute(runtime, "DICT-INDEX@")[2]

    runtime.evaluate(b": Index-Shadow 1 ;\n", source_name="index-shadow-one")
    first = runtime.find("INDEX-SHADOW")
    assert first is not None
    first_slot, first_entry, _metadata = _table_probe(runtime, b"index-shadow")
    assert first_entry == first.header_address
    assert _execute(runtime, "DICT-INDEX@")[2] == before_count + 1

    runtime.evaluate(b": index-shadow 2 ;\n", source_name="index-shadow-two")
    second = runtime.find("INDEX-SHADOW")
    assert second is not None
    second_slot, second_entry, _metadata = _table_probe(runtime, b"INDEX-SHADOW")
    assert second.header_address != first.header_address
    assert second_slot == first_slot
    assert second_entry == second.header_address
    assert _execute(runtime, "DICT-INDEX@")[2] == before_count + 1
    assert _execute(runtime, "INDEX-SHADOW") == (2,)


def test_dictionary_rollback_rebuilds_and_removes_reclaimed_bindings(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    runtime.evaluate(
        b": INDEX-ROLLBACK-A 11 ;\n",
        source_name="index-rollback-base",
    )
    retained = runtime.find("INDEX-ROLLBACK-A")
    assert retained is not None
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_count = _execute(runtime, "DICT-INDEX@")[2]

    runtime.evaluate(
        b": index-rollback-a 33 ;\n: INDEX-ROLLBACK-B 22 ;\n",
        source_name="index-rollback",
    )
    shadow = runtime.find("INDEX-ROLLBACK-A")
    assert shadow is not None
    assert shadow.header_address != retained.header_address
    assert _execute(runtime, "DICT-INDEX@")[2] == saved_count + 1
    assert _table_probe(runtime, b"index-rollback-a")[1] == shadow.header_address

    assert _execute(runtime, "DICT-ROLLBACK", saved_here, saved_latest) == ()
    assert runtime.find("INDEX-ROLLBACK-A") == retained
    assert runtime.find("INDEX-ROLLBACK-B") is None
    assert _execute(runtime, "DICT-INDEX@")[2:] == (
        saved_count,
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
    )
    assert _table_probe(runtime, b"index-rollback-a")[1] == retained.header_address
    assert _table_probe(runtime, b"index-rollback-b")[1] == 0
    assert _execute(runtime, "INDEX-ROLLBACK-A") == (11,)


def test_one_slot_boot_index_is_installed_saturated_fallback() -> None:
    runtime = _runtime_with_external_size(2_048)
    newest = runtime.find("_DICT-INDEX-INIT")
    assert newest is not None

    assert _execute(runtime, "DICT-INDEX@") == (
        EXTERNAL_BASE,
        1,
        1,
        DICT_INDEX_BOUND | DICT_INDEX_SATURATED,
    )
    assert runtime.memory.read64(EXTERNAL_BASE) == newest.header_address
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 16
    assert _pointer(runtime, "XMEM-FLOOR") == EXTERNAL_BASE + 16
    assert _execute(runtime, "XMEM-FREE") == (2_032,)

    runtime.evaluate(
        b": SATURATED-LINKED 91 ;\n",
        source_name="saturated-linked",
    )
    assert _execute(runtime, "SATURATED-LINKED") == (91,)
    assert _execute(runtime, "DICT-INDEX@")[2:] == (
        1,
        DICT_INDEX_BOUND | DICT_INDEX_SATURATED,
    )


@pytest.mark.parametrize("external_size", [0, 1_024])
def test_absent_or_too_small_external_memory_leaves_index_disabled(
    external_size: int,
) -> None:
    runtime = _runtime_with_external_size(external_size)
    assert _execute(runtime, "DICT-INDEX@") == (0, 0, 0, 0)
    assert _pointer(runtime, "_DICT-INDEX-DONE") == 1
    assert _pointer(runtime, "XMEM-FLOOR") == 0
    expected_here = 0 if external_size == 0 else EXTERNAL_BASE
    assert _pointer(runtime, "XMEM-HERE") == expected_here
    assert runtime.find("_DICT-INDEX-INIT") is not None


def test_next_contiguous_frontier_reaches_userland_dictionary_bounds(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    next_source = b"".join(lines[2423:2521])
    assert next_source.startswith(b"\n")
    assert next_source.endswith(b"    2DUP DICT-BOUNDS! DICT-BOUNDS-OFF\n")

    with pytest.raises(SourceError, match="unknown word") as caught:
        loaded_dictionary_index.evaluate(
            next_source,
            source_name="kdos.f:2424-2521",
        )
    assert caught.value.location.line == 98
    assert caught.value.location.column == 9
    assert caught.value.message == "unknown word b'DICT-BOUNDS!'"
