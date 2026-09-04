"""Focused scalar source prerequisites discovered by the Desktop closure."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError
from simulator.runtime import MegaForthRuntime


def test_cmove_up_copies_high_to_low_and_consumes_its_arguments() -> None:
    runtime = MegaForthRuntime()
    address = 0x800
    runtime.memory.write_bytes(address, b"abcdef")
    context = runtime.new_context()
    for value in (address + 2, address, 4):
        context.data.push(value)

    runtime.execute("CMOVE>", context=context)

    assert context.data.snapshot() == ()
    assert runtime.memory.read_bytes(address, 6) == b"efefef"


def test_on_and_double_cell_words_match_the_bios_pair_layout() -> None:
    runtime = MegaForthRuntime()
    address = 0x900
    context = runtime.new_context()
    context.data.push(address)
    runtime.execute("ON", context=context)
    assert runtime.memory.read64(address) == MASK64

    first = 0x1111_2222_3333_4444
    second = 0xAAAA_BBBB_CCCC_DDDD
    for value in (first, second, address):
        context.data.push(value)
    runtime.execute("2!", context=context)
    assert context.data.snapshot() == ()
    assert runtime.memory.read64(address) == second
    assert runtime.memory.read64(address + 8) == first

    context.data.push(address)
    runtime.execute("2@", context=context)
    assert context.data.snapshot() == (first, second)


def test_relocation_cells_are_distinct_mutable_zeroed_storage() -> None:
    runtime = MegaForthRuntime()
    addresses: list[int] = []
    for name in ("_RELOC-ACTIVE", "_RELOC-COUNT", "_RELOC-BUF"):
        runtime.execute(name)
        address = runtime.main_context.data.pop()
        addresses.append(address)
        assert runtime.memory.read64(address) == 0

    assert len(set(addresses)) == 3
    runtime.memory.write64(addresses[1], 17)
    assert runtime.memory.read64(addresses[1]) == 17


def test_latest_store_hides_a_semantic_suffix_without_rewinding_here() -> None:
    runtime = MegaForthRuntime()
    retained = runtime.evaluate(b": RETAINED-LATEST 11 ;").definitions[0]
    removed = runtime.evaluate(b": REMOVED-LATEST 22 ;").definitions[0]
    here = runtime.dictionary.here
    runtime.main_context.data.push(retained.header_address)

    runtime.execute("LATEST!")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.dictionary.here == here
    assert runtime.dictionary.latest_word is retained
    assert runtime.find("RETAINED-LATEST") is retained
    assert runtime.find("REMOVED-LATEST") is None
    with pytest.raises(KeyError):
        runtime.dictionary.resolve(removed.xt)


def test_latest_store_rejects_unknown_machine_code_chains_without_consuming() -> None:
    runtime = MegaForthRuntime()

    def reject(_context) -> None:
        raise ExecutionError("semantic LATEST rejection")

    hook = runtime.define_primitive("LATEST-FAULT", reject)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    unknown = 0x70000
    runtime.memory.write64(unknown, 0)
    before_latest = runtime.dictionary.latest
    before_here = runtime.dictionary.here
    runtime.main_context.data.push(unknown)

    with pytest.raises(ExecutionError, match="semantic LATEST rejection"):
        runtime.execute("LATEST!")

    assert runtime.main_context.data.snapshot() == (unknown,)
    assert runtime.dictionary.latest == before_latest
    assert runtime.dictionary.here == before_here
