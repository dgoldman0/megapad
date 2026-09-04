"""Focused absent-NIC pseudo-BIOS status coverage."""

from __future__ import annotations

import pytest

from simulator.memory import MMIO_BASE, MMIOAccessError
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition


NIC_ADDRESS = MMIO_BASE + 0x0400


def test_net_status_reports_an_absent_nic_without_stack_side_effects() -> None:
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
