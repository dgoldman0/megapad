"""Unchanged-source acceptance for KDOS ring-buffer primitives."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64
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
from tests.simulator.test_kdos_pipeline_bundles import (
    _load_pipeline_bundles,
    _registry_state,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-ring-buffers-9122-9215.f"
)

FIRST_LINE = 9122
FIXTURE_LAST_LINE = 9215
LAST_LINE = 9214
FIXTURE_BYTES = 3_089
FIXTURE_SHA256 = (
    "35d6d117f53e8b9cc98729f6989e057d83ffdb344fa381d30c352b0058a1cce2"
)
FIXTURE_GIT_BLOB = "b12f9a37059a1b15ae86056d645c24510b2811d5"
SLICE_BYTES = 3_017
SLICE_SHA256 = (
    "1da96005485469573790f5c8e90a4aaa9480f361008b87dd918c3e9c7727866f"
)
SLICE_GIT_BLOB = "c52812c6db04665c7ac620613e7a14989743aa69"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 396
RING_HEADER_BYTES = 48
RING_LOCK_ID = 4

SOURCE_LEDGER = (
    (":", b"RING", 0),
    (":", b"RING.ESIZE", 0),
    (":", b"RING.CAP", 0),
    (":", b"RING.HEAD", 0),
    (":", b"RING.TAIL", 0),
    (":", b"RING.COUNT", 0),
    (":", b"RING.LOCK", 0),
    (":", b"RING.DATA", 0),
    (":", b"RING-FULL?", 0),
    (":", b"RING-EMPTY?", 0),
    (":", b"RING-COUNT", 0),
    ("VARIABLE", b"_RP-RING", CELL_BYTES),
    (":", b"RING-PUSH", 0),
    (":", b"RING-POP", 0),
    (":", b"RING-PEEK", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)


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
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa719  Hash Table Primitives\n"
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_ring_buffers(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_ring_buffers() -> MegaForthRuntime:
    return _evaluate_ring_buffers(_load_pipeline_bundles())


def _cells(
    runtime: MegaForthRuntime,
    address: int,
    count: int,
) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(address + index * CELL_BYTES)
        for index in range(count)
    )


def test_ring_slice_is_exact_linked_initialized_and_load_time_pure() -> None:
    runtime = _load_pipeline_bundles()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
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
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_ring_buffers(runtime)

    assert len(SOURCE_LEDGER) == 15
    assert sum(definer == ":" for definer, _name, _body in SOURCE_LEDGER) == 14
    assert sum(
        definer == "VARIABLE" for definer, _name, _body in SOURCE_LEDGER
    ) == 1
    assert sum(len(name) for _definer, name, _body in SOURCE_LEDGER) == 133
    assert sum(body for _definer, _name, body in SOURCE_LEDGER) == CELL_BYTES
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

    assert _variable(runtime, "_RP-RING") == 0
    assert _constant(runtime, "RING-LOCK") == RING_LOCK_ID
    assert _registry_state(runtime) == registry_before
    assert runtime.timer.counter > counter_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.spinlocks.owners == locks_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"


def test_ring_constructor_has_a_48_byte_header_and_retains_payload_poison() -> None:
    runtime = _load_ring_buffers()
    name = b"GEOMETRY-RING"
    element_size = 3
    capacity = 4
    payload_bytes = element_size * capacity
    dynamic_growth = (
        RING_HEADER_BYTES
        + payload_bytes
        + HOSTED_WORD_FIXED_BYTES
        + len(name)
    )
    descriptor = runtime.dictionary.here
    runtime.memory.fill(descriptor, dynamic_growth, 0xA5)

    result = runtime.evaluate(
        b"3 4 RING GEOMETRY-RING",
        source_name="ring-constructor-geometry.f",
    )

    assert tuple(word.name for word in result.definitions) == (name,)
    word = runtime.find(name)
    assert word is not None
    assert isinstance(word.implementation, ConstantDefinition)
    assert word.header_address == descriptor + RING_HEADER_BYTES + payload_bytes
    assert runtime.dictionary.here == descriptor + dynamic_growth
    assert _constant(runtime, name) == descriptor
    assert _cells(runtime, descriptor, 6) == (
        element_size,
        capacity,
        0,
        0,
        0,
        RING_LOCK_ID,
    )
    assert _execute(runtime, "RING.ESIZE", descriptor) == (element_size,)
    assert _execute(runtime, "RING.CAP", descriptor) == (capacity,)
    assert _execute(runtime, "RING.HEAD", descriptor) == (descriptor + 16,)
    assert _execute(runtime, "RING.TAIL", descriptor) == (descriptor + 24,)
    assert _execute(runtime, "RING.COUNT", descriptor) == (0,)
    assert _execute(runtime, "RING.LOCK", descriptor) == (RING_LOCK_ID,)
    assert _execute(runtime, "RING.DATA", descriptor) == (
        descriptor + RING_HEADER_BYTES,
    )
    assert runtime.memory.read_bytes(
        descriptor + RING_HEADER_BYTES,
        payload_bytes,
    ) == b"\xA5" * payload_bytes
    assert _execute(runtime, "RING-EMPTY?", descriptor) == (MASK64,)
    assert _execute(runtime, "RING-FULL?", descriptor) == (0,)
    assert _execute(runtime, "RING-COUNT", descriptor) == (0,)


def test_multibyte_ring_fifo_full_empty_and_copy_boundaries_are_exact() -> None:
    runtime = _load_ring_buffers()
    runtime.evaluate(b"4 2 RING FIFO-RING", source_name="fifo-ring.f")
    ring = _constant(runtime, "FIFO-RING")
    data = _execute(runtime, "RING.DATA", ring)[0]
    source = runtime.define_created(
        "FIFO-SOURCE",
        initial_body=b"\xA0\x01\x02\x03\x04\xA1",
    ).body_address
    destination = runtime.define_created(
        "FIFO-DESTINATION",
        initial_body=b"\xB0" + b"\xCC" * 4 + b"\xB1",
    ).body_address
    owners_before = runtime.spinlocks.owners

    assert _execute(runtime, "RING-PUSH", source + 1, ring) == (MASK64,)
    assert runtime.memory.read_bytes(data, 4) == b"\x01\x02\x03\x04"
    assert _cells(runtime, ring + 16, 3) == (0, 1, 1)
    assert _variable(runtime, "_RP-RING") == ring
    assert runtime.spinlocks.owners == owners_before

    runtime.memory.write_bytes(source + 1, b"\x05\x06\x07\x08")
    assert _execute(runtime, "RING-PUSH", source + 1, ring) == (MASK64,)
    assert runtime.memory.read_bytes(data, 8) == (
        b"\x01\x02\x03\x04\x05\x06\x07\x08"
    )
    assert _cells(runtime, ring + 16, 3) == (0, 0, 2)
    assert _execute(runtime, "RING-FULL?", ring) == (MASK64,)
    assert runtime.spinlocks.owners == owners_before

    runtime.memory.write_bytes(source + 1, b"\x09\x0A\x0B\x0C")
    payload_before = runtime.memory.read_bytes(data, 8)
    assert _execute(runtime, "RING-PUSH", source + 1, ring) == (0,)
    assert runtime.memory.read_bytes(data, 8) == payload_before
    assert _cells(runtime, ring + 16, 3) == (0, 0, 2)
    assert runtime.spinlocks.owners == owners_before

    assert _execute(runtime, "RING-POP", destination + 1, ring) == (MASK64,)
    assert runtime.memory.read_bytes(destination, 6) == (
        b"\xB0\x01\x02\x03\x04\xB1"
    )
    assert _cells(runtime, ring + 16, 3) == (1, 0, 1)
    assert runtime.spinlocks.owners == owners_before

    assert _execute(runtime, "RING-POP", destination + 1, ring) == (MASK64,)
    assert runtime.memory.read_bytes(destination, 6) == (
        b"\xB0\x05\x06\x07\x08\xB1"
    )
    assert _cells(runtime, ring + 16, 3) == (0, 0, 0)
    assert _execute(runtime, "RING-EMPTY?", ring) == (MASK64,)
    assert runtime.spinlocks.owners == owners_before

    runtime.memory.write_bytes(destination + 1, b"\xCC" * 4)
    assert _execute(runtime, "RING-POP", destination + 1, ring) == (0,)
    assert runtime.memory.read_bytes(destination, 6) == (
        b"\xB0" + b"\xCC" * 4 + b"\xB1"
    )
    assert _cells(runtime, ring + 16, 3) == (0, 0, 0)
    assert runtime.spinlocks.owners == owners_before


def test_ring_wraparound_peek_and_count_preserve_fifo_order() -> None:
    runtime = _load_ring_buffers()
    runtime.evaluate(b"2 3 RING WRAP-RING", source_name="wrap-ring.f")
    ring = _constant(runtime, "WRAP-RING")
    data = _execute(runtime, "RING.DATA", ring)[0]
    source = runtime.define_created(
        "WRAP-SOURCE",
        initial_body=bytes(2),
    ).body_address
    destination = runtime.define_created(
        "WRAP-DESTINATION",
        initial_body=bytes(2),
    ).body_address

    for payload in (b"A1", b"B2", b"C3"):
        runtime.memory.write_bytes(source, payload)
        assert _execute(runtime, "RING-PUSH", source, ring) == (MASK64,)
    for expected in (b"A1", b"B2"):
        assert _execute(runtime, "RING-POP", destination, ring) == (MASK64,)
        assert runtime.memory.read_bytes(destination, 2) == expected
    for payload in (b"D4", b"E5"):
        runtime.memory.write_bytes(source, payload)
        assert _execute(runtime, "RING-PUSH", source, ring) == (MASK64,)

    assert _cells(runtime, ring + 16, 3) == (2, 2, 3)
    assert runtime.memory.read_bytes(data, 6) == b"D4E5C3"
    peek_addresses = tuple(
        _execute(runtime, "RING-PEEK", index, ring)[0]
        for index in range(3)
    )
    assert peek_addresses == (data + 4, data, data + 2)
    assert tuple(
        runtime.memory.read_bytes(address, 2) for address in peek_addresses
    ) == (b"C3", b"D4", b"E5")
    assert _execute(runtime, "RING-PEEK", 3, ring) == (0,)
    assert _execute(runtime, "RING-PEEK", 9, ring) == (0,)
    assert _execute(runtime, "RING-COUNT", ring) == (3,)

    for expected in (b"C3", b"D4", b"E5"):
        assert _execute(runtime, "RING-POP", destination, ring) == (MASK64,)
        assert runtime.memory.read_bytes(destination, 2) == expected
    assert _cells(runtime, ring + 16, 3) == (2, 2, 0)
    assert runtime.spinlocks.owner(RING_LOCK_ID) is None


def test_zero_capacity_is_guarded_and_negative_peek_reaches_the_lock_cell() -> None:
    runtime = _load_ring_buffers()
    runtime.evaluate(
        b"5 0 RING ZERO-CAPACITY-RING "
        b"8 4 RING SIGNED-PEEK-RING",
        source_name="ring-edge-domain.f",
    )
    zero = _constant(runtime, "ZERO-CAPACITY-RING")
    signed = _constant(runtime, "SIGNED-PEEK-RING")
    zero_constant = runtime.find("ZERO-CAPACITY-RING")
    assert zero_constant is not None
    source = runtime.define_created(
        "EDGE-SOURCE",
        initial_body=b"negative",
    ).body_address
    destination = runtime.define_created(
        "EDGE-DESTINATION",
        initial_body=b"\xCC" * 8,
    ).body_address

    assert _execute(runtime, "RING-EMPTY?", zero) == (MASK64,)
    assert _execute(runtime, "RING-FULL?", zero) == (MASK64,)
    assert _execute(runtime, "RING.DATA", zero) == (
        zero_constant.header_address,
    )
    assert _execute(runtime, "RING-PUSH", source, zero) == (0,)
    assert _execute(runtime, "RING-POP", destination, zero) == (0,)
    assert _execute(runtime, "RING-PEEK", 0, zero) == (0,)
    assert _execute(runtime, "RING-COUNT", zero) == (0,)
    assert runtime.memory.read_bytes(destination, 8) == b"\xCC" * 8
    assert runtime.spinlocks.owner(RING_LOCK_ID) is None

    assert _execute(runtime, "RING-PUSH", source, signed) == (MASK64,)
    data = _execute(runtime, "RING.DATA", signed)[0]
    assert _execute(runtime, "RING-PEEK", 0, signed) == (data,)
    assert _execute(runtime, "RING-PEEK", MASK64, signed) == (signed + 40,)
    assert runtime.memory.read64(signed + 40) == RING_LOCK_ID
    assert _execute(runtime, "RING-COUNT", signed) == (1,)
    assert runtime.spinlocks.owner(RING_LOCK_ID) is None


def test_all_rings_share_lock_four_and_sequential_paths_release_ownership() -> None:
    runtime = _load_ring_buffers()
    runtime.evaluate(
        b"1 2 RING FIRST-SHARED-RING "
        b"1 2 RING SECOND-SHARED-RING",
        source_name="shared-ring-lock.f",
    )
    first = _constant(runtime, "FIRST-SHARED-RING")
    second = _constant(runtime, "SECOND-SHARED-RING")
    source = runtime.define_created(
        "SHARED-RING-SOURCE",
        initial_body=b"Z",
    ).body_address
    destination = runtime.define_created(
        "SHARED-RING-DESTINATION",
        initial_body=b"?",
    ).body_address
    owners_before = runtime.spinlocks.owners

    assert _execute(runtime, "RING.LOCK", first) == (RING_LOCK_ID,)
    assert _execute(runtime, "RING.LOCK", second) == (RING_LOCK_ID,)
    assert _execute(runtime, "RING-PUSH", source, first) == (MASK64,)
    assert _variable(runtime, "_RP-RING") == first
    assert runtime.spinlocks.owners == owners_before
    assert _execute(runtime, "RING-PUSH", source, second) == (MASK64,)
    assert _variable(runtime, "_RP-RING") == second
    assert runtime.spinlocks.owners == owners_before
    assert _execute(runtime, "RING-POP", destination, first) == (MASK64,)
    assert runtime.memory.read_bytes(destination, 1) == b"Z"
    assert runtime.spinlocks.owners == owners_before
    assert _execute(runtime, "RING-POP", destination, second) == (MASK64,)
    assert runtime.memory.read_bytes(destination, 1) == b"Z"
    assert runtime.spinlocks.owners == owners_before
