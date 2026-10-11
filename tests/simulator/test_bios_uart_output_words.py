"""Focused pseudo-BIOS TYPE and SPACE UART output coverage."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.memory import MMIO_BASE, AddressClass, UnmappedAddressError
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


def test_type_sends_a_span_of_ordinary_memory_in_one_write(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """A long frame costs the host one copy, not a call for each byte."""

    runtime = MegaForthRuntime()
    payload = bytes(range(256)) * 64
    address = runtime.dictionary.here + 64
    runtime.memory.write_bytes(address, payload)
    writes: list[bytes] = []
    write = runtime.write_uart_bytes

    def recording_write(data: bytes) -> None:
        writes.append(data)
        write(data)

    def read8(_address: int) -> int:
        raise AssertionError("an ordinary span is read whole")

    monkeypatch.setattr(runtime, "write_uart_bytes", recording_write)
    monkeypatch.setattr(runtime.memory, "read8", read8)
    context = runtime.new_context()
    context.data.push(address)
    context.data.push(len(payload))

    runtime.execute("TYPE", context=context)

    assert writes == [payload]
    assert context.data.snapshot() == ()
    assert runtime.uart_output == payload


def test_type_reads_device_registers_one_byte_at_a_time(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    runtime = MegaForthRuntime()
    reads: list[int] = []

    def read8(address: int) -> int:
        reads.append(address)
        return ord("D")

    monkeypatch.setattr(runtime.memory, "read8", read8)
    context = runtime.new_context()
    context.data.push(MMIO_BASE)
    context.data.push(3)

    runtime.execute("TYPE", context=context)

    assert reads == [MMIO_BASE, MMIO_BASE + 1, MMIO_BASE + 2]
    assert context.data.snapshot() == ()
    assert runtime.uart_output == b"DDD"


def test_tx_flush_preserves_immediately_published_bytes_and_stack() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xA55A)
    runtime.write_uart_bytes(b"complete frame")

    runtime.execute("TX-FLUSH", context=context)

    assert context.data.snapshot() == (0xA55A,)
    assert runtime.uart_output == b"complete frame"
    assert runtime.drain_uart_output() == b"complete frame"
