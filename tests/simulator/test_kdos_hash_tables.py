"""Unchanged-source acceptance for KDOS hash-table primitives."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.crc import CRC_STATUS_OK
from simulator.errors import ExecutionError
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    MegaForthRuntime,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_pipeline_bundles import _registry_state
from tests.simulator.test_kdos_ring_buffers import _load_ring_buffers
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-hash-tables-9215-9384.f"
)

FIRST_LINE = 9215
FIXTURE_LAST_LINE = 9384
LAST_LINE = 9383
FIXTURE_BYTES = 5_424
FIXTURE_SHA256 = (
    "9379a85c46423efe2d14242f61bb974f6d1fa746cd9449b046cfbc3dbebdb467"
)
FIXTURE_GIT_BLOB = "b75a16f60f80d7885323443843919b8946af38ea"
SLICE_BYTES = 5_352
SLICE_SHA256 = (
    "ce5fc5c20a4905a0092ec28cd647c0d1679317334968db81084aba7bf6410e24"
)
SLICE_GIT_BLOB = "3c465404ec02b189269d5c982ee360c9d070e638"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 775
HASH_TABLE_HEADER_BYTES = 40
HASH_TABLE_LOCK_ID = 5
MODE0_CRC32 = {
    b"\x01": 0xB536_5DFC,
    b"\x05": 0xA632_2B20,
    b"\x09": 0x933E_B044,
    b"\x0D": 0x803A_C698,
    b"\x02": 0xB875_7B25,
}

SOURCE_LEDGER = (
    ("VARIABLE", b"_HT-KSIZE", CELL_BYTES),
    ("VARIABLE", b"_HT-VSIZE", CELL_BYTES),
    ("VARIABLE", b"_HT-NSLOTS", CELL_BYTES),
    (":", b"HASHTABLE", 0),
    (":", b"HT.KSIZE", 0),
    (":", b"HT.VSIZE", 0),
    (":", b"HT.SLOTS", 0),
    (":", b"HT.COUNT", 0),
    (":", b"HT.LOCK", 0),
    (":", b"HT.DATA", 0),
    (":", b"HT.STRIDE", 0),
    (":", b"HT-SLOT", 0),
    (":", b"HT-HASH", 0),
    (":", b"HT-KEY", 0),
    (":", b"HT-VAL", 0),
    (":", b"HT-COUNT", 0),
    ("VARIABLE", b"_HTP-KEY", CELL_BYTES),
    ("VARIABLE", b"_HTP-VAL", CELL_BYTES),
    ("VARIABLE", b"_HTP-HT", CELL_BYTES),
    (":", b"HT-PUT", 0),
    ("VARIABLE", b"_HTG-KEY", CELL_BYTES),
    ("VARIABLE", b"_HTG-HT", CELL_BYTES),
    (":", b"HT-GET", 0),
    ("VARIABLE", b"_HTD-KEY", CELL_BYTES),
    (":", b"HT-DEL", 0),
    ("VARIABLE", b"_HTE-XT", CELL_BYTES),
    ("VARIABLE", b"_HTE-HT", CELL_BYTES),
    (":", b"HT-EACH", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
ZERO_VARIABLES = tuple(
    name for definer, name, _body in SOURCE_LEDGER if definer == "VARIABLE"
)


def _verified_slice() -> bytes:
    fixture = FIXTURE.read_bytes()
    assert len(fixture) == FIXTURE_BYTES
    assert fixture.count(b"\n") == FIXTURE_LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(fixture).hexdigest() == FIXTURE_SHA256
    assert _git_blob_id(fixture) == FIXTURE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert fixture == b"".join(lines[FIRST_LINE - 1 : FIXTURE_LAST_LINE])
    boundary = b"\\ =====================================================================\n"
    assert lines[LAST_LINE - 1] == b"\n"
    assert lines[LAST_LINE] == boundary
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa720  Module System\n"
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_hash_tables(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_hash_tables() -> MegaForthRuntime:
    return _evaluate_hash_tables(_load_ring_buffers())


def _cells(
    runtime: MegaForthRuntime,
    address: int,
    count: int,
) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(address + index * CELL_BYTES)
        for index in range(count)
    )


def _bytes(
    runtime: MegaForthRuntime,
    name: str,
    payload: bytes,
) -> int:
    return runtime.define_created(name, initial_body=payload).body_address


def _slot(runtime: MegaForthRuntime, table: int, index: int) -> int:
    return _execute(runtime, "HT-SLOT", index, table)[0]


def test_hash_table_slice_is_exact_linked_initialized_and_load_time_pure() -> None:
    runtime = _load_ring_buffers()
    runtime.inject_uart_input(b"\x00H")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0706_0504_0302)
    assert runtime.spinlocks.acquire(3, 0) == 0
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    registry_before = _registry_state(runtime)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    crc_identity = (0, 0)
    assert runtime.crc.select_mode(crc_identity, 5) == CRC_STATUS_OK
    assert runtime.crc.seed(crc_identity, 0x1234_5678) == CRC_STATUS_OK
    assert runtime.crc.feed_byte(crc_identity, 0xA5) == CRC_STATUS_OK
    crc_before = (
        runtime.crc.mode,
        runtime.crc.accumulator,
        runtime.crc.owner,
    )
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_hash_tables(runtime)

    assert len(SOURCE_LEDGER) == 28
    assert sum(definer == ":" for definer, _name, _body in SOURCE_LEDGER) == 17
    assert sum(
        definer == "VARIABLE" for definer, _name, _body in SOURCE_LEDGER
    ) == 11
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 211
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == 88
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((definer, _name, body_span), word) in enumerate(
        zip(SOURCE_LEDGER, published)
    ):
        assert runtime.memory.read64(word.header_address) == prior_header
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following - word.body_address == body_span
        expected_type = {
            ":": ColonDefinition,
            "VARIABLE": CreatedDefinition,
        }[definer]
        assert isinstance(word.implementation, expected_type)
        prior_header = word.header_address

    assert tuple(_variable(runtime, name) for name in ZERO_VARIABLES) == (
        0,
    ) * len(ZERO_VARIABLES)
    assert _constant(runtime, "HT-LOCK") == HASH_TABLE_LOCK_ID
    assert _registry_state(runtime) == registry_before
    assert runtime.timer.counter > counter_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.spinlocks.owners == locks_before
    assert (
        runtime.crc.mode,
        runtime.crc.accumulator,
        runtime.crc.owner,
    ) == crc_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00H"
    assert runtime.drain_uart_output() == b"retained-output"


def test_constructor_geometry_accessors_and_zero_fill_are_exact() -> None:
    runtime = _load_hash_tables()
    name = b"GEOMETRY-TABLE"
    key_size = 2
    value_size = 3
    slots = 4
    stride = 1 + key_size + value_size
    data_bytes = slots * stride
    dynamic_growth = (
        HASH_TABLE_HEADER_BYTES
        + data_bytes
        + HOSTED_WORD_FIXED_BYTES
        + len(name)
    )
    descriptor = runtime.dictionary.here
    runtime.memory.fill(descriptor, dynamic_growth, 0xA5)

    result = runtime.evaluate(
        b"2 3 4 HASHTABLE GEOMETRY-TABLE",
        source_name="hash-table-constructor-geometry.f",
    )

    assert tuple(word.name for word in result.definitions) == (name,)
    word = runtime.find(name)
    assert word is not None
    assert isinstance(word.implementation, ConstantDefinition)
    assert word.header_address == descriptor + HASH_TABLE_HEADER_BYTES + data_bytes
    assert runtime.dictionary.here == descriptor + dynamic_growth
    assert _constant(runtime, name) == descriptor
    assert _cells(runtime, descriptor, 5) == (
        key_size,
        value_size,
        slots,
        0,
        HASH_TABLE_LOCK_ID,
    )
    assert runtime.memory.read_bytes(
        descriptor + HASH_TABLE_HEADER_BYTES,
        data_bytes,
    ) == bytes(data_bytes)
    assert _execute(runtime, "HT.KSIZE", descriptor) == (key_size,)
    assert _execute(runtime, "HT.VSIZE", descriptor) == (value_size,)
    assert _execute(runtime, "HT.SLOTS", descriptor) == (slots,)
    assert _execute(runtime, "HT.COUNT", descriptor) == (0,)
    assert _execute(runtime, "HT-COUNT", descriptor) == (0,)
    assert _execute(runtime, "HT.LOCK", descriptor) == (HASH_TABLE_LOCK_ID,)
    assert _execute(runtime, "HT.DATA", descriptor) == (
        descriptor + HASH_TABLE_HEADER_BYTES,
    )
    assert _execute(runtime, "HT.STRIDE", descriptor) == (stride,)
    assert tuple(_slot(runtime, descriptor, index) for index in range(slots)) == (
        descriptor + HASH_TABLE_HEADER_BYTES,
        descriptor + HASH_TABLE_HEADER_BYTES + stride,
        descriptor + HASH_TABLE_HEADER_BYTES + 2 * stride,
        descriptor + HASH_TABLE_HEADER_BYTES + 3 * stride,
    )
    slot = _slot(runtime, descriptor, 2)
    assert _execute(runtime, "HT-KEY", slot) == (slot + 1,)
    assert _execute(runtime, "HT-VAL", slot, descriptor) == (
        slot + 1 + key_size,
    )
    assert (
        _variable(runtime, "_HT-KSIZE"),
        _variable(runtime, "_HT-VSIZE"),
        _variable(runtime, "_HT-NSLOTS"),
    ) == (key_size, value_size, slots)


def test_crc_hash_insert_update_full_table_and_lookup_are_exact() -> None:
    runtime = _load_hash_tables()
    runtime.evaluate(
        b"1 1 4 HASHTABLE FULL-TABLE",
        source_name="full-hash-table.f",
    )
    table = _constant(runtime, "FULL-TABLE")
    keys = {
        payload: _bytes(runtime, f"FULL-KEY-{index}", payload)
        for index, payload in enumerate(MODE0_CRC32)
    }
    values = {
        payload: _bytes(runtime, f"VALUE-{index}", payload)
        for index, payload in enumerate(
            (b"\x11", b"\x55", b"\x99", b"\xDD", b"\x66", b"\x22")
        )
    }
    owners_before = runtime.spinlocks.owners

    for payload, expected_crc in MODE0_CRC32.items():
        expected_slot = expected_crc % 4
        assert _execute(runtime, "HT-HASH", keys[payload], table) == (
            expected_slot,
        )
        assert runtime.crc.accumulator == expected_crc
        assert runtime.crc.owner is None

    for key, value in zip(
        (b"\x01", b"\x05", b"\x09", b"\x0D"),
        (b"\x11", b"\x55", b"\x99", b"\xDD"),
    ):
        assert _execute(runtime, "HT-PUT", keys[key], values[value], table) == ()
        assert runtime.spinlocks.owners == owners_before

    assert _execute(runtime, "HT-COUNT", table) == (4,)
    assert runtime.memory.read_bytes(_execute(runtime, "HT.DATA", table)[0], 12) == (
        b"\x01\x01\x11"
        b"\x01\x05\x55"
        b"\x01\x09\x99"
        b"\x01\x0D\xDD"
    )
    for key, value in zip(
        (b"\x01", b"\x05", b"\x09", b"\x0D"),
        (b"\x11", b"\x55", b"\x99", b"\xDD"),
    ):
        pointer = _execute(runtime, "HT-GET", keys[key], table)[0]
        assert runtime.memory.read_bytes(pointer, 1) == value

    table_before = runtime.memory.read_bytes(table, HASH_TABLE_HEADER_BYTES + 12)
    assert _execute(
        runtime,
        "HT-PUT",
        keys[b"\x02"],
        values[b"\x22"],
        table,
    ) == ()
    assert runtime.memory.read_bytes(table, HASH_TABLE_HEADER_BYTES + 12) == table_before
    assert _execute(runtime, "HT-GET", keys[b"\x02"], table) == (0,)
    assert _execute(runtime, "HT-COUNT", table) == (4,)
    assert runtime.spinlocks.owners == owners_before

    assert _execute(
        runtime,
        "HT-PUT",
        keys[b"\x09"],
        values[b"\x66"],
        table,
    ) == ()
    updated = _execute(runtime, "HT-GET", keys[b"\x09"], table)[0]
    assert runtime.memory.read_bytes(updated, 1) == b"\x66"
    assert _execute(runtime, "HT-COUNT", table) == (4,)
    assert runtime.spinlocks.owners == owners_before


def test_tombstone_first_insertion_duplicates_a_later_matching_key() -> None:
    runtime = _load_hash_tables()
    runtime.evaluate(
        b"1 1 4 HASHTABLE TOMBSTONE-TABLE",
        source_name="tombstone-hash-table.f",
    )
    table = _constant(runtime, "TOMBSTONE-TABLE")
    first_key = _bytes(runtime, "TOMBSTONE-FIRST-KEY", b"\x01")
    duplicate_key = _bytes(runtime, "TOMBSTONE-DUPLICATE-KEY", b"\x05")
    first_value = _bytes(runtime, "TOMBSTONE-FIRST-VALUE", b"\x11")
    old_value = _bytes(runtime, "TOMBSTONE-OLD-VALUE", b"\x55")
    new_value = _bytes(runtime, "TOMBSTONE-NEW-VALUE", b"\x66")
    slot_zero = _slot(runtime, table, 0)
    slot_one = _slot(runtime, table, 1)
    owners_before = runtime.spinlocks.owners

    assert MODE0_CRC32[b"\x01"] % 4 == 0
    assert MODE0_CRC32[b"\x05"] % 4 == 0
    assert _execute(runtime, "HT-PUT", first_key, first_value, table) == ()
    assert _execute(runtime, "HT-PUT", duplicate_key, old_value, table) == ()
    assert runtime.memory.read_bytes(slot_zero, 3) == b"\x01\x01\x11"
    assert runtime.memory.read_bytes(slot_one, 3) == b"\x01\x05\x55"
    assert _execute(runtime, "HT-COUNT", table) == (2,)

    assert _execute(runtime, "HT-DEL", first_key, table) == (MASK64,)
    assert runtime.memory.read8(slot_zero) == 2
    assert runtime.memory.read_bytes(slot_zero, 3) == b"\x02\x01\x11"
    assert _execute(runtime, "HT-COUNT", table) == (1,)
    old_pointer = _execute(runtime, "HT-GET", duplicate_key, table)[0]
    assert old_pointer == slot_one + 2
    assert runtime.memory.read_bytes(old_pointer, 1) == b"\x55"

    assert _execute(runtime, "HT-PUT", duplicate_key, new_value, table) == ()
    assert runtime.memory.read_bytes(slot_zero, 3) == b"\x01\x05\x66"
    assert runtime.memory.read_bytes(slot_one, 3) == b"\x01\x05\x55"
    assert _execute(runtime, "HT-COUNT", table) == (2,)
    new_pointer = _execute(runtime, "HT-GET", duplicate_key, table)[0]
    assert new_pointer == slot_zero + 2
    assert runtime.memory.read_bytes(new_pointer, 1) == b"\x66"

    assert _execute(runtime, "HT-DEL", duplicate_key, table) == (MASK64,)
    assert runtime.memory.read8(slot_zero) == 2
    assert _execute(runtime, "HT-COUNT", table) == (1,)
    resurrected = _execute(runtime, "HT-GET", duplicate_key, table)[0]
    assert resurrected == slot_one + 2
    assert runtime.memory.read_bytes(resurrected, 1) == b"\x55"
    assert _execute(runtime, "HT-DEL", duplicate_key, table) == (MASK64,)
    assert _execute(runtime, "HT-GET", duplicate_key, table) == (0,)
    assert _execute(runtime, "HT-COUNT", table) == (0,)
    assert _execute(runtime, "HT-DEL", duplicate_key, table) == (0,)
    assert runtime.spinlocks.owners == owners_before


def test_zero_key_and_value_sizes_retain_their_literal_aliasing_behavior() -> None:
    runtime = _load_hash_tables()
    runtime.evaluate(
        b"0 2 3 HASHTABLE ZERO-KEY-TABLE "
        b"1 0 2 HASHTABLE ZERO-VALUE-TABLE",
        source_name="zero-width-hash-tables.f",
    )
    zero_key = _constant(runtime, "ZERO-KEY-TABLE")
    zero_value = _constant(runtime, "ZERO-VALUE-TABLE")
    first_value = _bytes(runtime, "ZERO-KEY-FIRST-VALUE", b"A1")
    second_value = _bytes(runtime, "ZERO-KEY-SECOND-VALUE", b"B2")
    key = _bytes(runtime, "ZERO-VALUE-KEY", b"\x01")
    owners_before = runtime.spinlocks.owners

    assert _execute(runtime, "HT.STRIDE", zero_key) == (3,)
    assert _execute(runtime, "HT-PUT", 0, first_value, zero_key) == ()
    assert _execute(runtime, "HT-COUNT", zero_key) == (1,)
    first_pointer = _execute(runtime, "HT-GET", MASK64, zero_key)[0]
    assert first_pointer == _slot(runtime, zero_key, 0) + 1
    assert runtime.memory.read_bytes(first_pointer, 2) == b"A1"
    assert _execute(runtime, "HT-PUT", MASK64, second_value, zero_key) == ()
    assert _execute(runtime, "HT-COUNT", zero_key) == (1,)
    second_pointer = _execute(runtime, "HT-GET", 0, zero_key)[0]
    assert second_pointer == first_pointer
    assert runtime.memory.read_bytes(second_pointer, 2) == b"B2"

    assert _execute(runtime, "HT.STRIDE", zero_value) == (2,)
    assert _execute(runtime, "HT-PUT", key, MASK64, zero_value) == ()
    assert _execute(runtime, "HT-COUNT", zero_value) == (1,)
    zero_value_pointer = _execute(runtime, "HT-GET", key, zero_value)[0]
    assert zero_value_pointer == _slot(runtime, zero_value, 1)
    assert runtime.memory.read8(zero_value_pointer) == 0
    assert runtime.spinlocks.owners == owners_before


def test_ht_each_visits_physical_occupied_slots_and_skips_tombstones() -> None:
    runtime = _load_hash_tables()
    runtime.evaluate(
        b"1 1 4 HASHTABLE EACH-TABLE",
        source_name="each-hash-table.f",
    )
    table = _constant(runtime, "EACH-TABLE")
    key_zero = _bytes(runtime, "EACH-KEY-ZERO", b"\x01")
    key_one = _bytes(runtime, "EACH-KEY-ONE", b"\x05")
    key_two = _bytes(runtime, "EACH-KEY-TWO", b"\x09")
    value_zero = _bytes(runtime, "EACH-VALUE-ZERO", b"\x11")
    value_one = _bytes(runtime, "EACH-VALUE-ONE", b"\x55")
    value_two = _bytes(runtime, "EACH-VALUE-TWO", b"\x99")
    for key, value in (
        (key_zero, value_zero),
        (key_one, value_one),
        (key_two, value_two),
    ):
        assert _execute(runtime, "HT-PUT", key, value, table) == ()
    assert _execute(runtime, "HT-DEL", key_one, table) == (MASK64,)
    owners_before = runtime.spinlocks.owners
    visits: list[tuple[int, int, bytes, bytes]] = []

    def collect(context: object) -> None:
        data = context.data  # type: ignore[attr-defined]
        value_address = data.pop()
        key_address = data.pop()
        visits.append(
            (
                key_address,
                value_address,
                runtime.memory.read_bytes(key_address, 1),
                runtime.memory.read_bytes(value_address, 1),
            )
        )

    collector = runtime.define_primitive("COLLECT-HASH-SLOT", collect)

    assert _execute(runtime, "HT-EACH", collector.xt, table) == ()

    slot_zero = _slot(runtime, table, 0)
    slot_two = _slot(runtime, table, 2)
    assert visits == [
        (slot_zero + 1, slot_zero + 2, b"\x01", b"\x11"),
        (slot_two + 1, slot_two + 2, b"\x09", b"\x99"),
    ]
    assert _variable(runtime, "_HTE-XT") == collector.xt
    assert _variable(runtime, "_HTE-HT") == table
    assert runtime.spinlocks.owners == owners_before

    runtime.evaluate(
        b"1 1 2 HASHTABLE OUTER-EACH-TABLE "
        b"1 1 2 HASHTABLE INNER-EACH-TABLE",
        source_name="nested-each-tables.f",
    )
    outer = _constant(runtime, "OUTER-EACH-TABLE")
    inner = _constant(runtime, "INNER-EACH-TABLE")
    for target in (outer, inner):
        assert _execute(runtime, "HT-PUT", key_zero, value_zero, target) == ()
        assert _execute(runtime, "HT-PUT", key_one, value_one, target) == ()
    runtime.evaluate(
        b"VARIABLE OUTER-EACH-CALLS 0 OUTER-EACH-CALLS ! "
        b"VARIABLE INNER-EACH-CALLS 0 INNER-EACH-CALLS ! "
        b"VARIABLE NESTED-EACH-DONE 0 NESTED-EACH-DONE ! "
        b": INNER-EACH-CALLBACK 2DROP 1 INNER-EACH-CALLS +! ; "
        b": OUTER-EACH-CALLBACK 2DROP 1 OUTER-EACH-CALLS +! "
        b"  NESTED-EACH-DONE @ 0= IF "
        b"    1 NESTED-EACH-DONE ! "
        b"    ['] INNER-EACH-CALLBACK INNER-EACH-TABLE HT-EACH "
        b"  THEN ;",
        source_name="nested-each-callbacks.f",
    )
    outer_callback = runtime.find("OUTER-EACH-CALLBACK")
    inner_callback = runtime.find("INNER-EACH-CALLBACK")
    assert outer_callback is not None
    assert inner_callback is not None

    assert _execute(runtime, "HT-EACH", outer_callback.xt, outer) == ()

    assert _variable(runtime, "OUTER-EACH-CALLS") == 1
    assert _variable(runtime, "INNER-EACH-CALLS") == 3
    assert _variable(runtime, "NESTED-EACH-DONE") == 1
    assert _variable(runtime, "_HTE-XT") == inner_callback.xt
    assert _variable(runtime, "_HTE-HT") == inner
    assert _execute(runtime, "HT-COUNT", outer) == (2,)
    assert _execute(runtime, "HT-COUNT", inner) == (2,)
    assert runtime.spinlocks.owners == owners_before


def test_zero_slot_table_aliases_its_constant_header_and_hash_traps() -> None:
    runtime = _load_hash_tables()
    runtime.evaluate(
        b"1 1 0 HASHTABLE ZERO-SLOT-TABLE",
        source_name="zero-slot-hash-table.f",
    )
    table = _constant(runtime, "ZERO-SLOT-TABLE")
    word = runtime.find("ZERO-SLOT-TABLE")
    assert word is not None
    key = _bytes(runtime, "ZERO-SLOT-KEY", b"K")

    assert _cells(runtime, table, 5) == (1, 1, 0, 0, HASH_TABLE_LOCK_ID)
    assert _execute(runtime, "HT.STRIDE", table) == (3,)
    assert _execute(runtime, "HT.DATA", table) == (word.header_address,)
    assert _execute(runtime, "HT-COUNT", table) == (0,)

    context = runtime.new_context()
    context.data.push(key)
    context.data.push(table)
    with pytest.raises(ExecutionError, match="signed modulo trapped on zero"):
        runtime.execute("HT-HASH", context=context, step_budget=250_000)
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.crc.owner is None
    assert runtime.spinlocks.owner(HASH_TABLE_LOCK_ID) is None
