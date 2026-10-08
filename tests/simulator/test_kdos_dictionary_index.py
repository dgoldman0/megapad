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
    / "kdos-dictionary-index-2399-2487.f"
)

FIRST_LINE = 2399
LAST_LINE = 2487
SLICE_SHA256 = (
    "c0a96826bdf91ede3a101e5c0ff32e97cd1a7074d99812c38e0dc2b66a84e054"
)
SLICE_GIT_BLOB = "d73e709df413f2a32d6193d6e5558342e5c3acbf"
DEFINITIONS = (
    b"_DICT-POW2-FLOOR",
    b"_DICT-INDEX-DONE",
    b"_DICT-INDEX-GROW-XT",
    b"_DICT-INDEX-ARM",
    b"_DICT-INDEX-WATERMARK",
    b"_DICT-INDEX-TAKE",
    b"_DICT-INDEX-GROW",
    b"_DICT-INDEX-INIT",
    b"_DICT-XMEM-RESET",
)
BIOS_WORDS = (
    b"2/",
    b"2*",
    b"DICT-INDEX!",
    b"DICT-INDEX@",
    b"DICT-INDEX-NOTIFY!",
)

CANONICAL_INDEX_SLOTS = 65_536
CANONICAL_INDEX_BYTES = CANONICAL_INDEX_SLOTS * 16


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 4_066
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
    assert _execute(runtime, "2/", MASK64) == (MASK64,)
    assert _execute(runtime, "2/", MASK64 - 2) == (MASK64 - 1,)


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


def _define_words(runtime: MegaForthRuntime, prefix: str, count: int) -> None:
    source = "".join(f": {prefix}-{i} {i} ;\n" for i in range(count))
    runtime.evaluate(source.encode("ascii"), source_name=prefix.lower())


def _grow_xt(runtime: MegaForthRuntime) -> int:
    word = runtime.find("_DICT-INDEX-GROW")
    assert word is not None
    return word.xt


def test_one_slot_boot_index_doubles_until_half_the_free_tail_refuses() -> None:
    runtime = _runtime_with_external_size(2_048)

    # The one boot slot below the floor saturated at installation.  Defining
    # _DICT-XMEM-RESET reached the armed count of zero, so the slice already
    # doubled the table once and returned the boot slot to the free list.
    assert _execute(runtime, "DICT-INDEX@") == (
        EXTERNAL_BASE + 16,
        2,
        2,
        DICT_INDEX_BOUND | DICT_INDEX_SATURATED,
    )
    assert runtime.dictionary_index.notification == (1, _grow_xt(runtime))
    assert _pointer(runtime, "XMEM-FLOOR") == EXTERNAL_BASE + 16
    assert _pointer(runtime, "XMEM-HERE") == EXTERNAL_BASE + 48
    assert _pointer(runtime, "XMEM-FL") == EXTERNAL_BASE
    assert runtime.memory.read64(EXTERNAL_BASE) == 16

    # While saturated, each definition reaches the re-armed count and doubles
    # the table again, until the next table would exceed half the free tail.
    geometry = []
    for i in range(6):
        _define_words(runtime, f"TINY-{i}", 1)
        base, slots, count, flags = _execute(runtime, "DICT-INDEX@")
        geometry.append((base - EXTERNAL_BASE, slots))
        assert count == slots
        assert flags == DICT_INDEX_BOUND | DICT_INDEX_SATURATED
    assert geometry == [
        (48, 4),
        (112, 8),
        (240, 16),
        (496, 32),
        (496, 32),
        (496, 32),
    ]
    # A 1,024-byte table is more than half of the 1,040-byte tail, and a full
    # table is not re-armed after that refusal.
    assert _execute(runtime, "XMEM-FREE") == (1_040,)
    assert runtime.dictionary_index.notification == (0, 0)

    # Lookup beyond the saturated table follows the linked dictionary.
    assert _execute(runtime, "TINY-5-0") == (0,)
    runtime.evaluate(b": SATURATED-LINKED 91 ;\n", source_name="saturated-linked")
    assert _execute(runtime, "SATURATED-LINKED") == (91,)


def test_index_doubles_at_three_quarters_and_frees_the_old_table() -> None:
    runtime = _runtime_with_external_size(2 << 20)
    grow = _grow_xt(runtime)
    table_bytes = 1_024 * 16
    assert _execute(runtime, "DICT-INDEX@") == (
        EXTERNAL_BASE,
        1_024,
        745,
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
    )
    assert runtime.dictionary_index.notification == (768, grow)
    floor = EXTERNAL_BASE + table_bytes
    assert _pointer(runtime, "XMEM-FLOOR") == floor

    # A general allocation first, so the grown table does not start at the
    # floor.  The 767th name stays in the boot table.
    assert _execute(runtime, "XMEM-ALLOT", 100) == (floor,)
    _define_words(runtime, "BELOW", 22)
    assert _execute(runtime, "DICT-INDEX@")[:3] == (EXTERNAL_BASE, 1_024, 767)

    _define_words(runtime, "CROSS", 1)
    grown = floor + 112
    assert _execute(runtime, "DICT-INDEX@") == (
        grown,
        2_048,
        768,
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
    )
    assert runtime.dictionary_index.notification == (1_536, grow)
    assert _pointer(runtime, "XMEM-HERE") == grown + 2 * table_bytes
    assert _pointer(runtime, "XMEM-FLOOR") == floor
    assert _pointer(runtime, "XMEM-FL") == EXTERNAL_BASE
    assert runtime.memory.read64(EXTERNAL_BASE) == table_bytes

    for word in {word.name.upper(): word for word in runtime.dictionary.words}.values():
        assert _table_probe(runtime, word.name)[1] == word.header_address
    assert _execute(runtime, "CROSS-0") == (0,)


def test_xmem_reset_rebinds_a_grown_table_at_the_floor() -> None:
    runtime = _runtime_with_external_size(2 << 20)
    floor = EXTERNAL_BASE + 1_024 * 16
    assert _execute(runtime, "XMEM-ALLOT", 100) == (floor,)
    _define_words(runtime, "GROWN", 23)
    assert _execute(runtime, "DICT-INDEX@")[:2] == (floor + 112, 2_048)

    assert _execute(runtime, "XMEM-RESET") == ()
    raised = floor + 2_048 * 16
    assert _execute(runtime, "DICT-INDEX@") == (
        floor,
        2_048,
        768,
        DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE,
    )
    assert _pointer(runtime, "XMEM-HERE") == raised
    assert _pointer(runtime, "XMEM-FLOOR") == raised
    assert _pointer(runtime, "XMEM-FL") == 0
    for name in (b"GROWN-0", b"GROWN-22", b"_DICT-XMEM-RESET"):
        word = runtime.find(name)
        assert word is not None
        assert _table_probe(runtime, name)[1] == word.header_address

    # The rebound table keeps indexing new definitions.
    _define_words(runtime, "AFTER-RESET", 1)
    word = runtime.find("AFTER-RESET-0")
    assert word is not None
    assert _table_probe(runtime, b"AFTER-RESET-0")[1] == word.header_address


def test_xmem_reset_leaves_a_table_below_the_floor_in_place(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    state = _execute(runtime, "DICT-INDEX@")
    floor = _pointer(runtime, "XMEM-FLOOR")
    assert _execute(runtime, "XMEM-ALLOT", 4_096) == (floor,)

    assert _execute(runtime, "XMEM-RESET") == ()
    assert _execute(runtime, "DICT-INDEX@") == state
    assert _pointer(runtime, "XMEM-HERE") == floor
    assert _pointer(runtime, "XMEM-FLOOR") == floor


def test_boot_arms_growth_at_three_quarters_of_the_canonical_table(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    assert runtime.dictionary_index.notification == (
        CANONICAL_INDEX_SLOTS * 3 // 4,
        _grow_xt(runtime),
    )
    assert _execute(runtime, "_DICT-INDEX-WATERMARK", 4) == (3,)
    assert _execute(runtime, "_DICT-INDEX-WATERMARK", 2) == (1,)
    assert _execute(runtime, "_DICT-INDEX-WATERMARK", 1) == (0,)


def test_notification_runs_once_after_each_kind_of_publication(
    loaded_dictionary_index: MegaForthRuntime,
) -> None:
    runtime = loaded_dictionary_index
    runtime.evaluate(
        b"VARIABLE NTF-HITS  VARIABLE NTF-SEEN\n"
        b": NTF-HOOK  1 NTF-HITS +!  DICT-INDEX@ DROP NIP NIP NTF-SEEN ! ;\n"
        b": NTF-ARM  ( n -- )  DICT-INDEX@ DROP NIP NIP +"
        b"  ['] NTF-HOOK DICT-INDEX-NOTIFY! ;\n",
        source_name="notify-hook",
    )

    def hits() -> int:
        return _pointer(runtime, "NTF-HITS")

    # Armed two names ahead: the first new name does not reach the count.
    runtime.evaluate(b"2 NTF-ARM\n: NTF-ONE 1 ;\n", source_name="notify-one")
    assert hits() == 0
    runtime.evaluate(b": NTF-TWO 2 ;\n", source_name="notify-two")
    assert hits() == 1
    assert _pointer(runtime, "NTF-SEEN") == _execute(runtime, "DICT-INDEX@")[2]
    assert runtime.dictionary_index.notification == (0, 0)
    runtime.evaluate(b": NTF-THREE 3 ;\n", source_name="notify-three")
    assert hits() == 1

    # Every named-definition builder makes the check.
    for number, line in enumerate(
        (
            b"CREATE NTF-CREATED",
            b"VARIABLE NTF-VARIABLE",
            b"7 CONSTANT NTF-CONSTANT",
            b"8 VALUE NTF-VALUE",
        ),
        start=2,
    ):
        runtime.evaluate(b"1 NTF-ARM\n" + line + b"\n", source_name="notify-kind")
        assert hits() == number

    # LATEST! rebuilds without a new name, so it fires at the current count.
    runtime.evaluate(b"0 NTF-ARM LATEST LATEST!\n", source_name="notify-latest")
    assert hits() == 6

    # An xt of zero disarms.
    runtime.evaluate(
        b"1 NTF-ARM  0 0 DICT-INDEX-NOTIFY!\n: NTF-DISARMED 9 ;\n",
        source_name="notify-disarmed",
    )
    assert hits() == 6


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
