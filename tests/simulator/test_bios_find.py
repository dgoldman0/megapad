"""Focused BIOS ``FIND`` contracts for the semantic dictionary."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.ir import Call, Return
from simulator.memory import AddressClass, UnmappedAddressError
from simulator.runtime import ColonDefinition, ExecutionContext, MegaForthRuntime
from simulator.stacks import DataStack, StackOverflow


_SCRATCH = 0x0007_0000


def _put_counted(
    runtime: MegaForthRuntime,
    payload: bytes,
    *,
    address: int = _SCRATCH,
    trailing: bytes = b"",
) -> int:
    assert len(payload) <= 0xFF
    runtime.memory.write_bytes(
        address,
        bytes((len(payload),)) + payload + trailing,
    )
    return address


def _execute_find(
    runtime: MegaForthRuntime,
    address: int,
) -> ExecutionContext:
    context = runtime.new_context()
    context.data.push(address)
    runtime.execute("FIND", context=context)
    return context


def _bank0_limit(runtime: MegaForthRuntime) -> int:
    return next(
        region.limit
        for region in runtime.memory.regions
        if region.kind is AddressClass.BANK0
    )


def test_find_returns_exact_normal_and_immediate_flags() -> None:
    runtime = MegaForthRuntime()
    find = runtime.find("FIND")
    normal = runtime.find("DUP")
    immediate = runtime.find(";")
    assert find is not None
    assert normal is not None
    assert immediate is not None
    assert find.immediate is False
    assert normal.immediate is False
    assert immediate.immediate is True

    normal_context = runtime.new_context()
    normal_context.data.push(0xA5)
    normal_context.data.push(_put_counted(runtime, b"dUp"))
    runtime.execute("FIND", context=normal_context)
    assert normal_context.data.snapshot() == (0xA5, normal.xt, MASK64)
    assert normal_context.returns.snapshot() == ()

    immediate_address = _put_counted(
        runtime,
        b";",
        address=_SCRATCH + 0x20,
    )
    immediate_context = _execute_find(runtime, immediate_address)
    assert immediate_context.data.snapshot() == (immediate.xt, 1)
    assert immediate_context.returns.snapshot() == ()


def test_find_selects_the_newest_case_insensitive_live_binding_and_rollback() -> None:
    runtime = MegaForthRuntime()
    original = runtime.define_primitive("Case-Probe", lambda _context: None)
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    shadow = runtime.define_primitive(
        "cASE-pROBE",
        lambda _context: None,
        immediate=True,
    )
    address = _put_counted(runtime, b"CASE-PROBE")

    shadow_context = _execute_find(runtime, address)
    assert shadow_context.data.snapshot() == (shadow.xt, 1)

    rollback_context = runtime.new_context()
    rollback_context.data.push(saved_here)
    rollback_context.data.push(saved_latest)
    runtime.execute("DICT-ROLLBACK", context=rollback_context)
    assert rollback_context.data.snapshot() == ()
    assert runtime.find("case-probe") is original
    with pytest.raises(KeyError):
        runtime.dictionary.resolve(shadow.xt)

    restored_context = _execute_find(runtime, address)
    assert restored_context.data.snapshot() == (original.xt, MASK64)


def test_find_uses_live_metadata_instead_of_mutated_guest_header_bytes() -> None:
    runtime = MegaForthRuntime()
    word = runtime.define_primitive("RAW-PROBE", lambda _context: None)
    original_address = _put_counted(runtime, b"RAW-PROBE")
    mutated_address = _put_counted(
        runtime,
        b"RAW-MUTED",
        address=_SCRATCH + 0x20,
    )
    runtime.memory.write8(word.header_address + 8, 0x80 | len(word.name))
    runtime.memory.write_bytes(word.header_address + 9, b"RAW-MUTED")

    original = _execute_find(runtime, original_address)
    mutated = _execute_find(runtime, mutated_address)

    assert original.data.snapshot() == (word.xt, MASK64)
    assert mutated.data.snapshot() == (mutated_address, 0)


@pytest.mark.parametrize(
    "payload",
    (
        b"NO-SUCH-FIND-WORD",
        b"",
        b"\xff",
    ),
)
def test_find_misses_preserve_the_original_counted_address(payload: bytes) -> None:
    runtime = MegaForthRuntime()
    address = _put_counted(runtime, payload)
    context = runtime.new_context()
    context.data.push(0x1122_3344)
    context.data.push(address)

    runtime.execute("FIND", context=context)

    assert context.data.snapshot() == (0x1122_3344, address, 0)
    assert context.returns.snapshot() == ()


def test_find_impossible_length_at_last_bank0_byte_reads_only_the_count() -> None:
    runtime = MegaForthRuntime()
    address = _bank0_limit(runtime) - 1
    runtime.memory.write8(address, 128)
    context = runtime.new_context()
    context.data.push(0x5566_7788)
    context.data.push(address)
    context.returns.push(0x99AA_BBCC)

    runtime.execute("FIND", context=context)

    assert context.data.snapshot() == (0x5566_7788, address, 0)
    assert context.returns.snapshot() == (0x99AA_BBCC,)


def test_find_uses_the_count_without_nul_or_trailing_byte_semantics() -> None:
    runtime = MegaForthRuntime()
    dup = runtime.find("DUP")
    assert dup is not None
    address = _put_counted(runtime, b"dUp", trailing=b"NOT-A-TERMINATOR")
    original = runtime.memory.read_bytes(address, 1 + 3 + 16)

    found = _execute_find(runtime, address)

    assert found.data.snapshot() == (dup.xt, MASK64)
    assert runtime.memory.read_bytes(address, len(original)) == original

    embedded_nul = _put_counted(
        runtime,
        b"DU\0",
        address=_SCRATCH + 0x40,
        trailing=b"P",
    )
    missed = _execute_find(runtime, embedded_nul)
    assert missed.data.snapshot() == (embedded_nul, 0)


def test_find_later_payload_fault_preserves_data_and_return_stacks() -> None:
    runtime = MegaForthRuntime()
    candidate = runtime.define_primitive("ABCD", lambda _context: None)
    assert runtime.dictionary.latest_word is candidate
    address = _bank0_limit(runtime) - 3
    runtime.memory.write_bytes(address, b"\x04AB")
    context = runtime.new_context()
    context.data.push(0x0123_4567_89AB_CDEF)
    context.data.push(address)
    context.returns.push(0x0BAD_F00D)

    with pytest.raises(UnmappedAddressError):
        runtime.execute("FIND", context=context)

    assert context.data.snapshot() == (0x0123_4567_89AB_CDEF, address)
    assert context.returns.snapshot() == (0x0BAD_F00D,)


def test_find_bounded_result_capacity_is_checked_after_lookup_without_mutation() -> None:
    runtime = MegaForthRuntime()
    address = _put_counted(runtime, b"DUP")
    context = ExecutionContext(
        data=DataStack(
            memory=runtime.memory,
            floor=0x800,
            empty_pointer=0x808,
        )
    )
    context.data.push(address)

    with pytest.raises(StackOverflow):
        runtime.execute("FIND", context=context)

    assert context.data.snapshot() == (address,)
    assert context.returns.snapshot() == ()


def test_find_payload_fault_precedes_bounded_result_capacity_fault() -> None:
    runtime = MegaForthRuntime()
    runtime.define_primitive("WXYZ", lambda _context: None)
    address = _bank0_limit(runtime) - 3
    runtime.memory.write_bytes(address, b"\x04WX")
    context = ExecutionContext(
        data=DataStack(
            memory=runtime.memory,
            floor=0x800,
            empty_pointer=0x808,
        )
    )
    context.data.push(address)

    with pytest.raises(UnmappedAddressError):
        runtime.execute("FIND", context=context)

    assert context.data.snapshot() == (address,)
    assert context.returns.snapshot() == ()


def test_find_compiles_as_an_ordinary_call_and_runs_inside_a_colon() -> None:
    runtime = MegaForthRuntime()
    find = runtime.find("FIND")
    dup = runtime.find("DUP")
    assert find is not None
    assert dup is not None
    result = runtime.evaluate(b": FIND-WRAPPER FIND ;")
    wrapper, = result.definitions
    assert isinstance(wrapper.implementation, ColonDefinition)
    assert wrapper.implementation.operations == (Call(find.xt), Return())
    address = _put_counted(runtime, b"DUP")
    context = runtime.new_context()
    context.data.push(address)

    runtime.execute("FIND-WRAPPER", context=context)

    assert context.data.snapshot() == (dup.xt, MASK64)
    assert context.returns.snapshot() == ()


def test_word_transient_counted_string_feeds_find_without_advancing_here() -> None:
    runtime = MegaForthRuntime()
    dup = runtime.find("DUP")
    assert dup is not None
    runtime.evaluate(b": FIND-NEXT BL WORD FIND ;")
    transient = runtime.dictionary.here

    runtime.evaluate(b"FIND-NEXT dUp")

    assert runtime.main_context.data.snapshot() == (dup.xt, MASK64)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.dictionary.here == transient
    assert runtime.memory.read_bytes(transient, 5) == b"\x03dUp\0"
