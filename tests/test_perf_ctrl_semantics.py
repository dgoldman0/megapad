"""Exact PERF_CTRL semantics required by checked BIOS timing scopes."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64
from asm import assemble
from megapad64 import CSR_PERF_CTRL


@pytest.mark.parametrize(
    ("value", "enabled", "reset"),
    [(0, 0, False), (1, 1, False), (2, 0, True), (3, 1, True)],
)
def test_python_csr_facade_decodes_enable_and_reset_independently(
    value: int,
    enabled: int,
    reset: bool,
) -> None:
    cpu = Megapad64(mem_size=256)
    cpu.perf_enable = 1
    cpu.perf_cycles = 11
    cpu.perf_stalls = 12
    cpu.perf_tileops = 13
    cpu.perf_extmem = 14

    cpu.csr_write(CSR_PERF_CTRL, value)

    assert cpu.perf_enable == enabled
    expected = (0, 0, 0, 0) if reset else (11, 12, 13, 14)
    assert (
        cpu.perf_cycles,
        cpu.perf_stalls,
        cpu.perf_tileops,
        cpu.perf_extmem,
    ) == expected


@pytest.mark.parametrize(
    ("value", "enabled", "reset"),
    [(0, 0, False), (1, 1, False), (2, 0, True), (3, 1, True)],
)
def test_native_guest_csr_write_uses_the_same_truth_table(
    value: int,
    enabled: int,
    reset: bool,
) -> None:
    cpu = Megapad64(mem_size=256)
    cpu.load_bytes(0, assemble(f"csrw {CSR_PERF_CTRL}, r1"))
    cpu.regs[1] = value
    cpu.pc = 0
    cpu.perf_enable = 1
    cpu.perf_cycles = 11
    cpu.perf_stalls = 12
    cpu.perf_tileops = 13
    cpu.perf_extmem = 14

    cpu.step()

    assert cpu.perf_enable == enabled
    if reset:
        # CSRW may itself be counted after enabling, but it cannot create any
        # stall, tile, or external-memory event after the reset edge.
        assert cpu.perf_stalls == 0
        assert cpu.perf_tileops == 0
        assert cpu.perf_extmem == 0
    else:
        assert cpu.perf_stalls == 12
        assert cpu.perf_tileops == 13
        assert cpu.perf_extmem == 14
