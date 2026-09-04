"""Focused VALUE/TO tests for the hosted MegaForth compiler."""

from __future__ import annotations

import pytest

from shared.cells import CELL_BYTES, MASK64
from simulator.errors import SourceError
from simulator.ir import StoreValue
from simulator.runtime import ColonDefinition, MegaForthRuntime, ValueDefinition
from simulator.stacks import StackUnderflow


def _value_store(runtime: MegaForthRuntime, name: str) -> StoreValue:
    word = runtime.find(name)
    assert word is not None
    assert isinstance(word.implementation, ColonDefinition)
    stores = [
        operation
        for operation in word.implementation.operations
        if isinstance(operation, StoreValue)
    ]
    assert len(stores) == 1
    return stores[0]


def test_value_to_interpret_and_compile_share_the_named_body_cell() -> None:
    runtime = MegaForthRuntime()

    result = runtime.evaluate(
        b"39 VALUE COUNTER : BUMP COUNTER 1+ TO COUNTER ;"
    )
    counter = runtime.find("counter")
    assert counter is not None
    assert isinstance(counter.implementation, ValueDefinition)
    assert [word.name for word in result.definitions] == [b"COUNTER", b"BUMP"]
    assert runtime.memory.read64(counter.body_address) == 39
    assert counter.body_address + CELL_BYTES <= runtime.dictionary.here
    assert _value_store(runtime, "BUMP").address == counter.body_address

    direct = runtime.new_context()
    runtime.execute(counter.xt, context=direct)
    assert direct.data.snapshot() == (39,)

    interpreted = runtime.new_context()
    interpreted.data.push(70)
    runtime.evaluate(b"TO counter", context=interpreted)
    assert interpreted.data.snapshot() == ()
    assert runtime.memory.read64(counter.body_address) == 70

    runtime.execute("BUMP")
    assert runtime.memory.read64(counter.body_address) == 71

    # Execution fetches the live body rather than a definition-time constant.
    runtime.memory.write64(counter.body_address, MASK64)
    fetched = runtime.new_context()
    runtime.execute("COUNTER", context=fetched)
    assert fetched.data.snapshot() == (MASK64,)


def test_value_to_compilation_keeps_the_original_address_after_shadowing() -> None:
    runtime = MegaForthRuntime()
    first_result = runtime.evaluate(
        b"7 VALUE LIMIT : WRITE-FIRST TO LIMIT ; : READ-FIRST LIMIT ;"
    )
    first = first_result.definitions[0]
    assert isinstance(first.implementation, ValueDefinition)
    assert _value_store(runtime, "WRITE-FIRST").address == first.body_address

    runtime.evaluate(b"90 VALUE limit : WRITE-CURRENT TO LIMIT ;")
    current = runtime.find("LIMIT")
    assert current is not None
    assert current is not first
    assert isinstance(current.implementation, ValueDefinition)
    assert _value_store(runtime, "WRITE-CURRENT").address == current.body_address

    old_assignment = runtime.new_context()
    old_assignment.data.push(11)
    runtime.execute("WRITE-FIRST", context=old_assignment)
    assert old_assignment.data.snapshot() == ()
    assert runtime.memory.read64(first.body_address) == 11
    assert runtime.memory.read64(current.body_address) == 90

    new_assignment = runtime.new_context()
    new_assignment.data.push(22)
    runtime.execute("WRITE-CURRENT", context=new_assignment)
    assert runtime.memory.read64(first.body_address) == 11
    assert runtime.memory.read64(current.body_address) == 22

    old_read = runtime.new_context()
    runtime.execute("READ-FIRST", context=old_read)
    assert old_read.data.snapshot() == (11,)


def test_value_to_rejects_missing_and_non_value_targets_without_consuming_x() -> None:
    runtime = MegaForthRuntime()

    missing_name = runtime.new_context()
    missing_name.data.push(1)
    with pytest.raises(SourceError, match="TO requires a following word"):
        runtime.evaluate(b"TO", context=missing_name, source_name="missing-to.f")
    assert missing_name.data.snapshot() == (1,)

    undefined = runtime.new_context()
    undefined.data.push(2)
    with pytest.raises(SourceError, match="TO target .* is undefined"):
        runtime.evaluate(b"TO NO-SUCH-VALUE", context=undefined)
    assert undefined.data.snapshot() == (2,)

    wrong_kind = runtime.new_context()
    wrong_kind.data.push(3)
    with pytest.raises(SourceError, match="TO requires a VALUE target"):
        runtime.evaluate(b"TO DROP", context=wrong_kind)
    assert wrong_kind.data.snapshot() == (3,)

    with pytest.raises(SourceError, match="TO requires a VALUE target"):
        runtime.evaluate(b": BAD-ASSIGN TO DROP ;")
    assert runtime.find("BAD-ASSIGN") is None

    consumed = runtime.new_context()
    consumed.data.push(4)
    with pytest.raises(SourceError, match="VALUE requires a following word"):
        runtime.evaluate(b"VALUE", context=consumed)
    assert consumed.data.snapshot() == ()


def test_value_to_underflow_and_rollback_leave_a_coherent_live_binding() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b"5 VALUE MODE : SET-MODE TO MODE ;")
    original = runtime.find("MODE")
    assert original is not None
    assert isinstance(original.implementation, ValueDefinition)

    with pytest.raises(StackUnderflow):
        runtime.evaluate(b"TO MODE", context=runtime.new_context())
    assert runtime.memory.read64(original.body_address) == 5

    with pytest.raises(StackUnderflow):
        runtime.execute("SET-MODE", context=runtime.new_context())
    assert runtime.memory.read64(original.body_address) == 5

    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    runtime.evaluate(b"8 VALUE mode")
    shadow = runtime.find("MODE")
    assert shadow is not None and shadow is not original

    runtime.rollback_dictionary(
        saved_here,
        saved_latest,
        runtime.main_context,
    )
    assert runtime.find("MODE") is original

    assignment = runtime.new_context()
    assignment.data.push(12)
    runtime.evaluate(b"TO mode", context=assignment)
    assert assignment.data.snapshot() == ()
    assert runtime.memory.read64(original.body_address) == 12

