"""Focused BIOS S" guest-address and lifetime contracts."""

from __future__ import annotations

import pytest

from simulator.errors import ExecutionError, SourceError
from simulator.ir import PushStringLiteral, Return
from simulator.memory import SparseAddressSpace
from simulator.runtime import ColonDefinition, ExecutionContext, MegaForthRuntime
from simulator.stacks import DataStack, ReturnStack, StackOverflow


def test_s_quote_interpret_mode_reuses_one_protected_transient_buffer() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    here = runtime.dictionary.here

    runtime.evaluate(b'S" alpha"', context=context)

    address, length = context.data.snapshot()
    assert length == 5
    assert address + 256 == here
    assert runtime.memory.read_bytes(address, 6) == b"alpha\0"
    assert runtime.dictionary.here == here
    assert runtime.caller_span_status(context, address, 1) == 3

    context.data.clear()
    runtime.evaluate(b'S" z"', context=context)
    replacement, replacement_length = context.data.snapshot()
    assert (replacement, replacement_length) == (address, 1)
    assert runtime.memory.read_bytes(address, 6) == b"z\0pha\0"
    assert runtime.dictionary.here == here


def test_s_quote_interpret_mode_is_line_local_and_clamps_to_255_bytes() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b'S" unterminated\n7', context=context)
    address, length, following = context.data.snapshot()
    assert length == len(b"unterminated")
    assert following == 7
    assert runtime.memory.read_bytes(address, length + 1) == b"unterminated\0"

    context.data.clear()
    result = runtime.evaluate(
        b'S" ' + b"x" * 255 + b" \\ ignored tail",
        context=context,
    )
    clamped_address, clamped_length = context.data.snapshot()
    assert clamped_address == address
    assert clamped_length == 255
    assert runtime.memory.read_bytes(address, 256) == b"x" * 255 + b"\0"
    assert result.token_count == 2

    context.data.clear()
    runtime.evaluate(b'S"', context=context)
    assert context.data.snapshot() == (address, 0)
    assert runtime.memory.read8(address) == 0


def test_s_quote_compiles_distinct_nul_terminated_dictionary_body_literals() -> None:
    runtime = MegaForthRuntime()
    here = runtime.dictionary.here
    pool = b"one\0two\0one\0"

    result = runtime.evaluate(b': STRINGS S" one" S" two" S" one" ;')

    strings = runtime.find("STRINGS")
    assert strings is not None
    assert isinstance(strings.implementation, ColonDefinition)
    literals = tuple(
        operation
        for operation in strings.implementation.operations
        if isinstance(operation, PushStringLiteral)
    )
    assert literals == (
        PushStringLiteral(0, 3),
        PushStringLiteral(4, 3),
        PushStringLiteral(8, 3),
    )
    assert [word.name for word in result.definitions] == [b"STRINGS"]
    assert runtime.memory.read_bytes(strings.body_address, len(pool)) == pool
    assert runtime.dictionary.here - here == runtime.dictionary.definition_size(
        b"STRINGS",
        initial_body=pool,
    )

    context = runtime.new_context()
    runtime.execute("STRINGS", context=context)
    assert context.data.snapshot() == (
        strings.body_address,
        3,
        strings.body_address + 4,
        3,
        strings.body_address + 8,
        3,
    )

    runtime.evaluate(b'S" transient"', context=runtime.new_context())
    runtime.evaluate(b': LATER 9 ; : FIRST S" hi" DROP C@ ; : SHOW S" ok" TYPE ;')
    assert runtime.memory.read_bytes(strings.body_address, len(pool)) == pool

    first = runtime.new_context()
    runtime.execute("FIRST", context=first)
    assert first.data.snapshot() == (ord("h"),)

    runtime.drain_uart_output()
    runtime.execute("SHOW", context=runtime.new_context())
    assert runtime.drain_uart_output() == b"ok"


def test_s_quote_empty_and_does_suffix_resolve_against_defining_colon_body() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b': EMPTY S" " ; '
        b': LABELER CREATE DOES> DROP S" child" ; '
        b"LABELER ITEM"
    )

    empty = runtime.find("EMPTY")
    labeler = runtime.find("LABELER")
    item = runtime.find("ITEM")
    assert empty is not None
    assert labeler is not None
    assert item is not None
    assert runtime.memory.read8(empty.body_address) == 0
    assert runtime.memory.read_bytes(labeler.body_address, 6) == b"child\0"

    empty_context = runtime.new_context()
    runtime.execute("EMPTY", context=empty_context)
    assert empty_context.data.snapshot() == (empty.body_address, 0)

    item_context = runtime.new_context()
    runtime.execute("ITEM", context=item_context)
    assert item_context.data.snapshot() == (labeler.body_address, 5)
    assert item_context.data.snapshot()[0] != item.body_address

    runtime.evaluate(b': LINE-BOUND S" abc\n; : RAW S"  \\raw" ;')
    line_bound = runtime.find("LINE-BOUND")
    raw = runtime.find("RAW")
    assert line_bound is not None
    assert raw is not None
    assert runtime.memory.read_bytes(line_bound.body_address, 4) == b"abc\0"
    assert runtime.memory.read_bytes(raw.body_address, 6) == b" \\raw\0"


def test_s_quote_body_offsets_survive_branches_and_interpret_state_here_moves() -> None:
    runtime = MegaForthRuntime()
    here = runtime.dictionary.here
    runtime.evaluate(
        b': RELOC S" before" [ 8 ALLOT ] S" after" ; '
        b': CHOOSE IF S" yes" ELSE S" no" THEN ;'
    )

    reloc = runtime.find("RELOC")
    assert reloc is not None
    assert reloc.header_address == here + 8
    assert runtime.memory.read_bytes(reloc.body_address, 13) == b"before\0after\0"
    relocated = runtime.new_context()
    runtime.execute("RELOC", context=relocated)
    assert relocated.data.snapshot() == (
        reloc.body_address,
        6,
        reloc.body_address + 7,
        5,
    )

    selected = runtime.new_context()
    selected.data.push(-1)
    runtime.execute("CHOOSE", context=selected)
    yes_address, yes_length = selected.data.snapshot()
    assert runtime.memory.read_bytes(yes_address, yes_length) == b"yes"

    selected.data.clear()
    selected.data.push(0)
    runtime.execute("CHOOSE", context=selected)
    no_address, no_length = selected.data.snapshot()
    assert runtime.memory.read_bytes(no_address, no_length) == b"no"


def test_s_quote_unfinished_definition_does_not_publish_its_buffered_pool() -> None:
    runtime = MegaForthRuntime()
    here = runtime.dictionary.here
    latest = runtime.dictionary.latest_word
    runtime.memory.fill(here, 64, 0xA5)

    with pytest.raises(SourceError, match="no terminating ;"):
        runtime.evaluate(b': BROKEN S" pooled"')

    assert runtime.find("BROKEN") is None
    assert runtime.dictionary.latest_word is latest
    assert runtime.dictionary.here == here
    assert runtime.memory.read_bytes(here, 64) == bytes((0xA5,)) * 64

    with pytest.raises(SourceError, match="embedded NUL"):
        runtime.evaluate(b': BAD-NUL S" a\0b" ;')
    assert runtime.find("BAD-NUL") is None
    assert runtime.dictionary.latest_word is latest
    assert runtime.dictionary.here == here


def test_s_quote_trusted_ir_requires_a_nul_terminated_literal_pool() -> None:
    runtime = MegaForthRuntime()

    word = runtime.define_colon(
        "MANUAL-STRING",
        (PushStringLiteral(0, 2), Return()),
        literal_pool=b"hi\0",
    )
    context = runtime.new_context()
    runtime.execute(word.xt, context=context)
    assert context.data.snapshot() == (word.body_address, 2)

    with pytest.raises(ValueError, match="NUL-terminated body pool"):
        runtime.define_colon(
            "BAD-STRING",
            (PushStringLiteral(0, 2), Return()),
            literal_pool=b"hi",
        )
    assert runtime.find("BAD-STRING") is None


def test_s_quote_semicolon_preflights_the_complete_literal_pool() -> None:
    runtime = MegaForthRuntime()

    def escape_dictionary_fault(_context) -> None:
        raise ExecutionError("guarded S-quote capacity")

    hook = runtime.define_primitive("HOST-SQUOTE-FAULT", escape_dictionary_fault)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")

    name = b"NO-POOL"
    header_size = runtime.dictionary.definition_size(name)
    guarded_ceiling = runtime.main_context.data.pointer - 256
    fault_here = guarded_ceiling - header_size
    runtime.dictionary.allot(fault_here - runtime.dictionary.here)
    latest = runtime.dictionary.latest_word
    untouched = bytes((0xA5,)) * (header_size + len(b"payload\0"))
    runtime.memory.write_bytes(fault_here, untouched)

    with pytest.raises(ExecutionError, match="guarded S-quote capacity"):
        runtime.evaluate(b': NO-POOL S" payload" ;')

    assert runtime.find(name) is None
    assert runtime.dictionary.latest_word is latest
    assert runtime.dictionary.here == fault_here
    assert runtime.memory.read_bytes(fault_here, len(untouched)) == untouched


def test_s_quote_runtime_pair_push_preflights_a_bounded_data_stack() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b': STRING S" value" ;')
    stack_memory = SparseAddressSpace(bank0_size=0x200)
    context = ExecutionContext(
        data=DataStack(
            memory=stack_memory,
            floor=0x100,
            empty_pointer=0x108,
        ),
        returns=ReturnStack(),
    )

    with pytest.raises(StackOverflow):
        runtime.execute("STRING", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
