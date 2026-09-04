"""Focused scalar source prerequisites discovered by the Desktop closure."""

from __future__ import annotations

from shared.cells import MASK64
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
