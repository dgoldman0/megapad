"""Focused BIOS defining, dictionary-data, and terminal-word contracts."""

from __future__ import annotations

import pytest

from shared.cells import CELL_BYTES, MASK64, s64
from simulator.dictionary import HEADER_FIXED_BYTES, SEMANTIC_CODE_SLOT_BYTES
from simulator.errors import ExecutionError, ForthAbort
from simulator.memory import SparseAddressSpace
from simulator.runtime import MegaForthRuntime


def _compare(
    runtime: MegaForthRuntime,
    left_address: int,
    left_length: int,
    right_address: int,
    right_length: int,
) -> int:
    context = runtime.new_context()
    for cell in (left_address, left_length, right_address, right_length):
        context.data.push(cell)
    runtime.execute("COMPARE", context=context)
    assert context.data.depth() == 1
    return s64(context.data.pop())


def test_create_comma_and_c_comma_publish_body_bytes_and_exact_here() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    result = runtime.evaluate(
        b"CREATE PAYLOAD 0x8877665544332211 , 0x1A5 C, HERE PAYLOAD",
        context=context,
    )

    payload = runtime.find("PAYLOAD")
    assert payload is not None
    assert [word.name for word in result.definitions] == [b"PAYLOAD"]
    assert runtime.memory.read_bytes(payload.body_address, 9) == bytes.fromhex(
        "11 22 33 44 55 66 77 88 A5"
    )
    assert runtime.dictionary.here == payload.body_address + 9
    assert context.data.snapshot() == (
        payload.body_address + 9,
        payload.body_address,
    )


def test_variable_owns_one_zeroed_cell_and_executes_as_its_body_address() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b"VARIABLE COUNTER HERE COUNTER", context=context)

    counter = runtime.find("COUNTER")
    assert counter is not None
    assert runtime.memory.read64(counter.body_address) == 0
    assert runtime.dictionary.here == counter.body_address + CELL_BYTES
    assert context.data.snapshot() == (
        counter.body_address + CELL_BYTES,
        counter.body_address,
    )


def test_variable_body_capacity_fault_does_not_publish_header_or_metadata() -> None:
    memory = SparseAddressSpace(bank0_size=0x20000)
    runtime = MegaForthRuntime(memory=memory)
    name = b"NO-ROOM"
    definition_bytes = (
        HEADER_FIXED_BYTES + len(name) + SEMANTIC_CODE_SLOT_BYTES
    )
    region_limit = memory.regions[0].limit
    fault_here = region_limit - definition_bytes
    runtime.dictionary.allot(fault_here - runtime.dictionary.here)
    previous_latest = runtime.dictionary.latest_word
    untouched = memory.read_bytes(fault_here, definition_bytes)

    with pytest.raises(OverflowError, match="memory region"):
        runtime.evaluate(b"VARIABLE " + name)

    assert runtime.dictionary.here == fault_here
    assert runtime.dictionary.latest_word is previous_latest
    assert runtime.find(name) is None
    assert memory.read_bytes(fault_here, definition_bytes) == untouched


def test_signed_allot_rewinds_without_erasing_stale_dictionary_bytes() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b"CREATE SCRATCH 0xAA C, 7 ALLOT")
    scratch = runtime.find("SCRATCH")
    assert scratch is not None
    high_water = scratch.body_address + CELL_BYTES
    assert runtime.dictionary.here == high_water

    context = runtime.new_context()
    runtime.evaluate(b"-8 ALLOT HERE 8 ALLOT HERE", context=context)

    assert context.data.snapshot() == (scratch.body_address, high_water)
    assert runtime.memory.read8(scratch.body_address) == 0xAA
    assert runtime.memory.read_bytes(scratch.body_address + 1, 7) == b"\x00" * 7


def test_tick_is_line_local_and_bracket_tick_compiles_the_found_xt() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": TARGET 42 ; : CAPTURE ['] TARGET ;")
    target = runtime.find("TARGET")
    assert target is not None

    context = runtime.new_context()
    runtime.evaluate(
        b"' TARGET ' unknown '\nTARGET CAPTURE",
        context=context,
    )

    assert context.data.snapshot() == (
        target.xt,
        0,
        0,
        42,
        target.xt,
    )


def test_to_body_rejects_non_create_words_instead_of_fabricating_an_address() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": COLON-WORD 1 ;")
    colon_word = runtime.find("COLON-WORD")
    assert colon_word is not None
    context = runtime.new_context()
    context.data.push(colon_word.xt)

    with pytest.raises(ExecutionError, match="CREATE-family"):
        runtime.execute(">BODY", context=context)


def test_compare_is_unsigned_lexicographic_for_every_length_shape() -> None:
    runtime = MegaForthRuntime()
    left = 0x101
    right = 0x209
    runtime.memory.write_bytes(left, b"abc\x80z")
    runtime.memory.write_bytes(right, b"abc\x7fz")

    assert _compare(runtime, left, 3, right, 3) == 0
    assert _compare(runtime, left, 5, right, 5) == 1
    assert _compare(runtime, right, 5, left, 5) == -1
    assert _compare(runtime, left, 2, right, 3) == -1
    assert _compare(runtime, left, 3, right, 2) == 1
    assert _compare(runtime, MASK64, 0, MASK64 - 1, 0) == 0


def test_abort_is_nonreturning_and_clears_both_complete_task_stacks() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": DIE 99 >R 17 ABORT 23 ;")
    context = runtime.new_context()
    context.data.push(11)
    context.returns.push(13)

    with pytest.raises(ForthAbort, match="ABORT"):
        runtime.execute("DIE", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_dot_renders_signed_cells_with_bios_uppercase_digits_and_space() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(
        b"0 . -1 . -9223372036854775808 . HEX 0xFF . -0xFF .",
        context=context,
    )
    runtime.set_numeric_base(36)
    context.data.push(35)
    runtime.execute(".", context=context)

    assert runtime.uart_output == b"0 -1 -9223372036854775808 FF -FF Z "
    assert context.data.snapshot() == ()


def test_jit_toggles_are_explicit_semantic_noops() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    result = runtime.evaluate(b"JIT-ON JIT-OFF 7", context=context)

    assert result.semantic_steps == 2
    assert context.data.snapshot() == (7,)
