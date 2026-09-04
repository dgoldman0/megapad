"""Focused unconfigured-network pseudo-BIOS coverage."""

from __future__ import annotations

import pytest

from simulator.memory import AddressClass, MMIO_BASE, MMIOAccessError
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition


NIC_ADDRESS = MMIO_BASE + 0x0400


def test_net_status_reports_unconfigured_without_stack_side_effects() -> None:
    runtime = MegaForthRuntime()
    other = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.returns.push(0xBEEF)

    runtime.execute("NET-STATUS", context=context)

    assert context.data.snapshot() == (0xCAFE, 0)
    assert context.returns.snapshot() == (0xBEEF,)
    other_context = other.new_context()
    other.execute("NET-STATUS", context=other_context)
    assert other_context.data.snapshot() == (0,)


def test_net_status_is_a_primitive_without_a_direct_nic_mmio_claim() -> None:
    runtime = MegaForthRuntime()
    status = runtime.find("NET-STATUS")
    assert status is not None
    assert isinstance(status.implementation, PrimitiveDefinition)

    with pytest.raises(MMIOAccessError, match="preflight"):
        runtime.memory.read8(NIC_ADDRESS + 1)

    context = runtime.new_context()
    runtime.execute("NET-STATUS", context=context)
    assert context.data.snapshot() == (0,)


def test_unconfigured_network_keeps_the_native_send_receive_stack_contract() -> None:
    runtime = MegaForthRuntime()
    destination = runtime.define_created(
        b"RX-CANARY",
        initial_body=b"unchanged",
    )
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.data.push(NIC_ADDRESS)
    context.data.push(0xFFFF_FFFF_FFFF_FFFF)

    runtime.execute("NET-SEND", context=context)

    assert context.data.snapshot() == (0xCAFE,)
    context.data.push(destination.body_address)
    runtime.execute("NET-RECV", context=context)
    assert context.data.snapshot() == (0xCAFE, 0)
    assert runtime.memory.read_bytes(destination.body_address, 9) == b"unchanged"


def test_net_mac_returns_stable_ordinary_storage_that_mac_init_can_copy() -> None:
    runtime = MegaForthRuntime()
    mac_word = runtime.find("NET-MAC@")
    assert mac_word is not None
    assert isinstance(mac_word.implementation, PrimitiveDefinition)
    destination = runtime.define_created(
        b"MAC-COPY",
        initial_body=b"\xA5" * 6,
    )
    context = runtime.new_context()

    runtime.execute("NET-MAC@", context=context)
    first = context.data.pop()
    runtime.execute("NET-MAC@", context=context)
    second = context.data.pop()

    assert first == second == mac_word.body_address
    assert runtime.memory.classify(first) is AddressClass.BANK0
    assert runtime.memory.read_bytes(first, 6) == bytes(6)

    context.data.push(first)
    context.data.push(destination.body_address)
    context.data.push(6)
    runtime.execute("CMOVE", context=context)
    assert context.data.snapshot() == ()
    assert runtime.memory.read_bytes(destination.body_address, 6) == bytes(6)
