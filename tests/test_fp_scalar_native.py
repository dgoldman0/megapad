"""Native FC retirement preserves the Python ISA without a fallback crossing."""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import CSR_FPCSR, CSR_ICACHE_CTRL, IVEC_ILLEGAL_OP, TrapError
from tests.test_fp_scalar import (
    MASK64,
    NV,
    NX,
    RUP,
    Megapad64,
    NativeMegapad64,
    _prepared,
    _state,
    d32,
    d64,
)


def _forbid_fallback(monkeypatch, cpu):
    def unexpected_fallback(*args, **kwargs):
        pytest.fail("full-core FC entered the Python ISA fallback")

    monkeypatch.setattr(cpu, "_step_python_fallback", unexpected_fallback)


def _observed_state(cpu):
    return _state(cpu) + (
        cpu._ext_modifier,
        cpu.perf_cycles,
        cpu.perf_stalls,
        cpu.icache_hits,
        cpu.icache_misses,
    )


@pytest.mark.parametrize("mode", range(5))
@pytest.mark.parametrize("batch", (False, True), ids=("step", "batch"))
def test_native_fc_program_completes_without_python_fallback(
    monkeypatch, mode, batch,
):
    python = _prepared(Megapad64, mode)
    native = _prepared(NativeMegapad64, mode)
    for cpu in (python, native):
        cpu.perf_enable = 1
        cpu.flags_unpack(0xA5)
    _forbid_fallback(monkeypatch, native)

    expected_cycles = 0
    for _ in range(11):
        cycles = python.step()
        expected_cycles += cycles
        if not batch:
            assert native.step() == cycles
            assert _observed_state(native) == _observed_state(python)
    if batch:
        stats = native.run_steps_stats(11)
        assert stats.steps_executed == 11
        assert stats.total_cycles == expected_cycles
        assert stats.stop_reason == 0
    assert _observed_state(native) == _observed_state(python)
    assert native.perf_cycles == expected_cycles


@pytest.mark.parametrize(
    ("source", "registers", "mode"),
    (
        ("fma.d r17, r18, r19", {17: d64(1), 18: d64(2), 19: d64(3)}, RUP),
        ("fms.d r17, r17, r17", {17: d64(3)}, RUP),
        ("fcmp.d r17, r18", {17: d64(1), 18: 0x7FF8000000000000}, 7),
        ("fcmp.s r1, r6", {1: d32(1), 6: 0x7F800001}, 7),
        ("fadd.s r1, r6", {1: 0xBAD00000000 | d32(1), 6: d32(2)}, RUP),
        ("fcvt.d.h r1, r6", {1: MASK64, 6: 0x3C00}, 7),
        ("fadd.d r3, r6", {6: 0}, RUP),
        ("fma.d r17, r18, r3", {17: d64(1), 18: d64(2)}, RUP),
    ),
)
def test_native_fc_register_aliases_rex_and_comparison_flags(
    monkeypatch, source, registers, mode,
):
    cpus = (Megapad64(mem_size=4096), NativeMegapad64(mem_size=4096))
    _forbid_fallback(monkeypatch, cpus[1])
    cycles = []
    for cpu in cpus:
        cpu.load_bytes(0, assemble(source))
        cpu.pc = 0
        cpu.flags_unpack(0xFF)
        cpu.csr_write(CSR_FPCSR, mode | NV)
        cpu.perf_enable = 1
        for register, value in registers.items():
            cpu.regs[register] = value
        cycles.append(cpu.step())
    assert cycles[0] == cycles[1]
    assert _observed_state(cpus[0]) == _observed_state(cpus[1])


@pytest.mark.parametrize(
    ("encoding", "mode"),
    (
        (bytes([0xFC, 0x80, 0x16]), 0),
        (bytes([0xFC, 0x87, 0x16, 0x07]), 0),
        (bytes([0xF3, 0xFC, 0x47, 0x12, 0x33]), 0),
        (bytes([0xFC, 0x09, 0x16]), 0),
        (bytes([0xFC, 0x25, 0x16]), 0),
        (assemble("fadd.d r17, r18"), 7),
        (assemble("fcvt.d.s r1, r6"), 6),
    ),
)
def test_native_fc_fault_consumes_full_encoding_before_any_fp_effect(
    monkeypatch, encoding, mode,
):
    cpus = (Megapad64(mem_size=4096), NativeMegapad64(mem_size=4096))
    _forbid_fallback(monkeypatch, cpus[1])
    for cpu in cpus:
        # Cross an I-cache line so fault fetches also cover the trailing byte.
        cpu.load_bytes(14, encoding)
        cpu.pc = 14
        cpu.regs[1] = d64(1)
        cpu.regs[6] = d64(3)
        cpu.regs[17] = d64(2)
        cpu.regs[18] = d64(4)
        cpu.flags_unpack(0xFF)
        cpu.csr_write(CSR_FPCSR, mode | NX)
        cpu.perf_enable = 1
        before = _state(cpu)
        with pytest.raises(TrapError) as error:
            cpu.step()
        assert error.value.ivec_id == IVEC_ILLEGAL_OP
        assert cpu.pc == 14 + len(encoding)
        assert cpu.regs[1] == before[0][1]
        assert cpu.regs[17] == before[0][17]
        assert cpu.flags_pack() == before[2]
        assert cpu.csr_read(CSR_FPCSR) == before[3]
        assert cpu.cycle_count == cpu.perf_cycles == 0
    assert _observed_state(cpus[0]) == _observed_state(cpus[1])


def test_native_fc_uses_resident_instruction_bytes_until_cache_invalidation(
    monkeypatch,
):
    cpus = (Megapad64(mem_size=4096), NativeMegapad64(mem_size=4096))
    _forbid_fallback(monkeypatch, cpus[1])
    address = 13
    for cpu in cpus:
        cpu.load_bytes(address, assemble("fma.d r17, r18, r19"))
        cpu.perf_enable = 1

    for phase, expected in enumerate((7.0, 7.0, -5.0)):
        for cpu in cpus:
            if phase == 1:
                # Backing-memory writes intentionally do not invalidate the
                # guest's instruction cache. Change FMA to FMS under the line.
                cpu.mem[address + 2] = 0x48
            if phase == 2:
                cpu.csr_write(CSR_ICACHE_CTRL, 3)
            cpu.pc = address
            cpu.regs[17], cpu.regs[18], cpu.regs[19] = d64(1), d64(2), d64(3)
            assert cpu.step() == 5
            assert cpu.regs[17] == d64(expected)
        assert _observed_state(cpus[0]) == _observed_state(cpus[1])


def test_native_fc_strict_cycle_slices_publish_only_at_retirement(monkeypatch):
    from tests.test_native_cycle_execution import _system

    code = assemble("fdiv.d r1, r6\nhalt")
    sliced, whole = _system(code), _system(code)
    for system in (sliced, whole):
        cpu = system.cpu
        cpu.regs[1], cpu.regs[6] = d64(1), d64(3)
        cpu.csr_write(CSR_FPCSR, RUP | NV)
        cpu.flags_unpack(0xA5)
        cpu.perf_enable = 1
        _forbid_fallback(monkeypatch, cpu)

    original = _observed_state(sliced.cpu)
    pending = sliced.run_cycle_batch(30, max_instructions=1)
    assert pending.instructions_executed == 0
    assert pending.system_cycles_advanced == 30
    assert pending.per_core_cycles == (0,)
    assert _observed_state(sliced.cpu) == original
    assert sliced._native_system.cycle_execution_pending

    retired = sliced.run_cycle_batch(1, max_instructions=1)
    assert retired.instructions_executed == 1
    assert retired.per_core_cycles == (31,)
    assert not sliced._native_system.cycle_execution_pending
    complete = whole.run_cycle_batch(31, max_instructions=1)
    assert complete.instructions_executed == 1
    assert _observed_state(sliced.cpu) == _observed_state(whole.cpu)
    assert sliced.cpu.pc == 3
    assert sliced.cpu.cycle_count == sliced.cpu.perf_cycles == 31
    assert sliced.cpu.csr_read(CSR_FPCSR) == RUP | NV | NX
