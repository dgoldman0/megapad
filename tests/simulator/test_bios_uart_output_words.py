"""Focused pseudo-BIOS TYPE and SPACE UART output coverage."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.memory import AddressClass, UnmappedAddressError
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition


def test_type_and_space_publish_exact_bytes_in_native_header_order() -> None:
    runtime = MegaForthRuntime()
    constant = runtime.find("CONSTANT")
    type_word = runtime.find("TYPE")
    space = runtime.find("SPACE")

    assert constant is not None
    assert type_word is not None
    assert space is not None
    assert runtime.memory.read64(type_word.header_address) == constant.header_address
    assert runtime.memory.read64(space.header_address) == type_word.header_address
    assert isinstance(type_word.implementation, PrimitiveDefinition)
    assert isinstance(space.implementation, PrimitiveDefinition)

    payload = b"A\x00\xff\n"
    address = runtime.dictionary.here + 64
    runtime.memory.write_bytes(address, payload)
    context = runtime.new_context()
    context.data.push(address)
    context.data.push(len(payload))

    runtime.execute("TYPE", context=context)

    assert context.data.snapshot() == ()
    assert runtime.uart_output == payload

    context.data.push(MASK64)
    context.data.push(0)
    runtime.execute("TYPE", context=context)
    assert context.data.snapshot() == ()
    assert runtime.uart_output == payload

    runtime.execute("SPACE", context=context)
    assert context.data.snapshot() == ()
    assert runtime.uart_output == payload + b" "


def test_type_consumes_both_arguments_and_retains_a_prefix_on_late_fault() -> None:
    runtime = MegaForthRuntime()
    bank0 = next(
        region
        for region in runtime.memory.regions
        if region.kind is AddressClass.BANK0
    )
    address = bank0.limit - 2
    runtime.memory.write_bytes(address, b"XY")
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(address)
    context.data.push(3)
    context.returns.push(0xBEEF)

    with pytest.raises(UnmappedAddressError) as caught:
        runtime.execute("TYPE", context=context)

    assert caught.value.address == bank0.limit
    assert caught.value.length == 1
    assert context.data.snapshot() == (0xCAFE,)
    assert context.returns.snapshot() == (0xBEEF,)
    assert runtime.uart_output == b"XY"


def test_type_wraps_each_increment_and_routes_each_byte_independently(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    runtime = MegaForthRuntime()
    reads: list[int] = []

    def read8(address: int) -> int:
        reads.append(address)
        return {MASK64: ord("Z"), 0: ord("A")}[address]

    monkeypatch.setattr(runtime.memory, "read8", read8)
    context = runtime.new_context()
    context.data.push(MASK64)
    context.data.push(2)

    runtime.execute("TYPE", context=context)

    assert reads == [MASK64, 0]
    assert context.data.snapshot() == ()
    assert runtime.uart_output == b"ZA"
