from __future__ import annotations

from dataclasses import replace

import pytest

from shared.cells import MASK64, u64
from simulator.dictionary import (
    Dictionary,
    HEADER_FIXED_BYTES,
    IMMEDIATE_FLAG,
    SEMANTIC_CODE_SLOT_BYTES,
)
from simulator.memory import EXTERNAL_BASE, SparseAddressSpace


def test_definition_uses_native_no_padding_header_geometry() -> None:
    dictionary = Dictionary(start_address=0x2000)

    word = dictionary.define(b"DUP", implementation="primitive", immediate=True)

    assert word.name == b"DUP"
    assert word.header_address == 0x2000
    assert word.xt == word.header_address + 9 + len(word.name)
    assert HEADER_FIXED_BYTES == 9
    assert word.immediate is True
    assert word.implementation == "primitive"
    assert dictionary.latest == word.header_address
    assert dictionary.latest_word is word
    assert dictionary.here == word.xt + SEMANTIC_CODE_SLOT_BYTES
    assert word.body_address == dictionary.here
    assert word.xt != 0


def test_dictionary_rejects_zero_as_the_empty_link_sentinel() -> None:
    with pytest.raises(ValueError, match="empty-link sentinel"):
        Dictionary(start_address=0)


def test_memory_backed_definition_emits_exact_link_flags_name_and_code_slot() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000)
    dictionary = Dictionary(start_address=0x2000, memory=memory)

    first = dictionary.define(b"DUP", implementation="primitive", immediate=True)
    second = dictionary.define(b"SWAP", implementation="primitive")

    first_header = memory.read_bytes(
        first.header_address,
        first.body_address - first.header_address,
    )
    assert first_header == (
        bytes(8) + bytes((IMMEDIATE_FLAG | 3,)) + b"DUP" + bytes(8)
    )
    assert memory.read_bytes(
        second.header_address,
        second.body_address - second.header_address,
    ) == (
        first.header_address.to_bytes(8, "little")
        + bytes((4,))
        + b"SWAP"
        + bytes(8)
    )
    assert second.header_address == first.body_address
    assert dictionary.here == second.body_address


def test_definition_atomically_emits_an_exact_initial_body() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000)
    dictionary = Dictionary(start_address=0x2000, memory=memory)
    initial_body = bytes.fromhex("ef cd ab 89 67 45 23 01")

    word = dictionary.define("CELL", initial_body=initial_body)

    assert memory.read_bytes(word.body_address, len(initial_body)) == initial_body
    assert dictionary.here == word.body_address + len(initial_body)


def test_initial_body_capacity_fault_does_not_emit_or_publish_header() -> None:
    header_size = HEADER_FIXED_BYTES + 1 + SEMANTIC_CODE_SLOT_BYTES
    start = 0x1000
    memory = SparseAddressSpace(bank0_size=start + header_size)
    dictionary = Dictionary(start_address=start, memory=memory)

    with pytest.raises(OverflowError, match="memory region"):
        dictionary.define("X", initial_body=bytes(8))

    assert dictionary.here == start
    assert dictionary.latest_word is None
    assert dictionary.find("X") is None
    assert memory.read_bytes(start, header_size) == bytes(header_size)


def test_initial_body_requires_immutable_bytes() -> None:
    dictionary = Dictionary()

    with pytest.raises(TypeError, match="initial body must be bytes"):
        dictionary.define("X", initial_body=bytearray(8))  # type: ignore[arg-type]


def test_lookup_is_ascii_case_insensitive_and_newest_binding_wins() -> None:
    dictionary = Dictionary()
    old = dictionary.define(b"Render", implementation="old")
    new = dictionary.define("rENDER", implementation="new")

    assert dictionary.find(b"RENDER") is new
    assert dictionary.find("render") is new
    assert dictionary.resolve(old.xt) is old
    assert dictionary.resolve(new.xt) is new


def test_stored_execution_token_keeps_its_compiled_binding_after_shadowing() -> None:
    dictionary = Dictionary()
    original = dictionary.define("DRAW", implementation=lambda: "original")
    compiled_call_xt = original.xt

    replacement = dictionary.define("draw", implementation=lambda: "replacement")

    assert dictionary.find("DRAW") is replacement
    assert dictionary.resolve(compiled_call_xt) is original
    assert dictionary.resolve(compiled_call_xt).implementation() == "original"


def test_rollback_removes_new_execution_tokens_and_restores_shadowed_binding() -> None:
    dictionary = Dictionary(start_address=0x4000)
    original = dictionary.define("OPEN", implementation="original")
    checkpoint = dictionary.checkpoint()
    replacement = dictionary.define("open", implementation="replacement")
    transient = dictionary.define("CLOSE", implementation="transient")

    dictionary.rollback(checkpoint)

    assert dictionary.find("OPEN") is original
    assert dictionary.find("CLOSE") is None
    assert dictionary.resolve(original.xt) is original
    with pytest.raises(KeyError):
        dictionary.resolve(replacement.xt)
    with pytest.raises(KeyError):
        dictionary.resolve(transient.xt)
    assert dictionary.here == checkpoint.here
    assert dictionary.latest == checkpoint.latest == original.header_address


def test_rollback_restores_metadata_but_leaves_stale_header_bytes() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000)
    dictionary = Dictionary(start_address=0x4000, memory=memory)
    original = dictionary.define("OPEN", implementation="original")
    checkpoint = dictionary.checkpoint()
    replacement = dictionary.define("open", implementation="replacement")
    stale = memory.read_bytes(
        replacement.header_address,
        replacement.body_address - replacement.header_address,
    )

    dictionary.rollback(checkpoint)

    assert dictionary.latest_word is original
    assert dictionary.find("OPEN") is original
    assert dictionary.here == checkpoint.here
    assert memory.read_bytes(replacement.header_address, len(stale)) == stale


def test_opaque_rollback_restores_the_checkpoint_dictionary_zone() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000, external_size=0x1000)
    dictionary = Dictionary(start_address=0x4000, memory=memory)
    retained = dictionary.define("SYSTEM", implementation="retained")
    checkpoint = dictionary.checkpoint()
    bank0_zone = dictionary.active_zone

    dictionary.move_here(
        EXTERNAL_BASE,
        floor=EXTERNAL_BASE,
        limit=EXTERNAL_BASE + 0x1000,
    )
    external = dictionary.define("EXTERNAL", implementation="removed")
    assert dictionary.active_zone == (EXTERNAL_BASE, EXTERNAL_BASE + 0x1000)

    dictionary.rollback(checkpoint)

    assert dictionary.here == checkpoint.here
    assert dictionary.active_zone == bank0_zone
    assert dictionary.latest_word is retained
    assert dictionary.find("EXTERNAL") is None
    with pytest.raises(KeyError):
        dictionary.resolve(external.xt)


def test_rollback_rejects_checkpoint_ahead_of_here_without_mutation() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000)
    dictionary = Dictionary(start_address=0x4000, memory=memory)
    live = dictionary.define("LIVE", implementation="live")
    dictionary.allot(8)
    checkpoint = dictionary.checkpoint()
    dictionary.allot(u64(-4))
    active_here = dictionary.here

    with pytest.raises(ValueError, match="ahead of the active dictionary"):
        dictionary.rollback(checkpoint)

    assert dictionary.here == active_here
    assert dictionary.latest_word is live
    assert dictionary.resolve(live.xt) is live


@pytest.mark.parametrize(("field", "value"), (("here", -1), ("latest", 0)))
def test_rollback_rejects_copied_checkpoint_with_altered_coordinates(
    field: str,
    value: int,
) -> None:
    dictionary = Dictionary(start_address=0x4000)
    live = dictionary.define("LIVE", implementation="live")
    checkpoint = dictionary.checkpoint()
    forged = replace(checkpoint, **{field: value})

    with pytest.raises(ValueError, match="sealed state"):
        dictionary.rollback(forged)

    assert dictionary.here == live.body_address
    assert dictionary.latest_word is live
    assert dictionary.resolve(live.xt) is live


def test_definition_rejects_uint64_address_wrap_without_publication() -> None:
    allocation_size = HEADER_FIXED_BYTES + 1 + SEMANTIC_CODE_SLOT_BYTES
    dictionary = Dictionary(start_address=MASK64 - allocation_size + 1)
    checkpoint = dictionary.checkpoint()

    with pytest.raises(OverflowError, match="wrap uint64"):
        dictionary.define("X")

    assert dictionary.here == checkpoint.here
    assert dictionary.latest == 0
    assert dictionary.find("X") is None


def test_name_validation_preserves_the_seven_bit_header_length() -> None:
    dictionary = Dictionary()

    with pytest.raises(ValueError, match="ASCII"):
        dictionary.define("café")
    with pytest.raises(ValueError, match="127"):
        dictionary.define(b"X" * 128)


def test_allot_interprets_a_signed_cell_without_clearing_memory() -> None:
    memory = SparseAddressSpace(bank0_size=0x3000)
    dictionary = Dictionary(start_address=0x1000, memory=memory)
    memory.write_bytes(0x1000, b"abcdefgh")

    dictionary.allot(8)
    dictionary.allot(u64(-4))

    assert dictionary.here == 0x1004
    assert memory.read_bytes(0x1000, 8) == b"abcdefgh"

    with pytest.raises(OverflowError, match="below"):
        dictionary.allot(u64(-5))
    assert dictionary.here == 0x1004


def test_comma_and_c_comma_emit_little_endian_low_bits() -> None:
    memory = SparseAddressSpace(bank0_size=0x3000)
    dictionary = Dictionary(start_address=0x1000, memory=memory)

    dictionary.comma(0x0123_4567_89AB_CDEF)
    dictionary.c_comma(0x1A5)

    assert memory.read_bytes(0x1000, 9) == bytes.fromhex(
        "ef cd ab 89 67 45 23 01 a5"
    )
    assert dictionary.here == 0x1009


def test_transient_dictionary_tail_write_does_not_advance_here() -> None:
    memory = SparseAddressSpace(bank0_size=0x3000)
    dictionary = Dictionary(start_address=0x1000, memory=memory)

    address = dictionary.write_transient(b"\x05alpha\0")

    assert address == 0x1000
    assert dictionary.here == 0x1000
    assert memory.read_bytes(address, 7) == b"\x05alpha\0"

    word = dictionary.define("X")
    assert word.header_address == address
    assert memory.read64(address) == 0


def test_dictionary_store_fault_is_atomic_for_memory_and_here() -> None:
    memory = SparseAddressSpace(bank0_size=0x1004)
    dictionary = Dictionary(start_address=0x1000, memory=memory)
    original_here = dictionary.here

    with pytest.raises(OverflowError, match="memory region"):
        dictionary.comma(0xDEAD_BEEF)

    assert dictionary.here == original_here
    assert memory.read_bytes(0x1000, 4) == bytes(4)

    dictionary.allot(4)
    with pytest.raises(OverflowError, match="memory region"):
        dictionary.c_comma(0xFF)
    assert dictionary.here == 0x1004


def test_definition_after_rewind_cannot_overlap_a_live_header_or_code_slot() -> None:
    memory = SparseAddressSpace(bank0_size=0x8000)
    dictionary = Dictionary(start_address=0x2000, memory=memory)
    live = dictionary.define("LIVE", implementation="still-live")
    header = memory.read_bytes(
        live.header_address,
        live.body_address - live.header_address,
    )
    dictionary.allot(live.header_address - dictionary.here)

    with pytest.raises(ValueError, match="overlap a live"):
        dictionary.define("OTHER")

    assert dictionary.here == live.header_address
    assert dictionary.latest_word is live
    assert dictionary.resolve(live.xt) is live
    assert memory.read_bytes(live.header_address, len(header)) == header


def test_memory_backed_dictionary_rejects_unmapped_start_and_region_overrun() -> None:
    memory = SparseAddressSpace(bank0_size=0x1020)

    with pytest.raises(ValueError, match="mapped ordinary memory"):
        Dictionary(start_address=0x2000, memory=memory)

    dictionary = Dictionary(start_address=0x1000, memory=memory)
    dictionary.allot(0x20)
    with pytest.raises(OverflowError, match="memory region"):
        dictionary.define("X")
    assert dictionary.latest_word is None
