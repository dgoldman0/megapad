"""Phase 2 element-5 native microcore execution boundaries."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64Micro
from asm import assemble
from megapad64 import (
    CPUID_MICRO,
    CSR_CPUID,
    IVEC_ILLEGAL_OP,
    Megapad64Micro as PythonMegapad64Micro,
    TrapError,
)


_RETAINED_SCALAR_PROGRAM = assemble(
    """
    ldi r16, 0x12
    ldi r17, 0x03
    add r16, r17
    addi r16, 5
    popcnt r18, r16
    bitrev r19, r17
    cmpi r16, 0x1a
    breq equal
    ldi r20, 0xff
equal:
    dec r1
    halt
"""
)


def _local_state(cpu) -> tuple:
    return (
        tuple(cpu.regs),
        cpu.pc,
        cpu.psel,
        cpu.xsel,
        cpu.spsel,
        cpu.flags_pack(),
        cpu.halted,
        cpu.idle,
        cpu.cycle_count,
        cpu.perf_cycles,
        cpu._ext_modifier,
    )


def test_retained_scalar_and_rex_execution_matches_python_microcore():
    """Core-local reduced instructions stay native and match the ISA oracle."""
    native = Megapad64Micro(mem_size=256, core_id=1, num_cores=5)
    oracle = PythonMegapad64Micro(mem_size=256, core_id=1, num_cores=5)
    native.load_bytes(0, _RETAINED_SCALAR_PROGRAM)
    oracle.load_bytes(0, _RETAINED_SCALAR_PROGRAM)

    def reject_fallback():
        raise AssertionError("retained scalar instruction used Python")

    native._step_python_fallback_in_memory_scope = reject_fallback
    native_cycles = 0
    oracle_cycles = 0
    steps = 0
    while not native.halted:
        native_cycles += native.step()
        oracle_cycles += oracle.step()
        steps += 1

    assert steps == 10
    assert oracle.halted
    assert native_cycles == oracle_cycles
    assert _local_state(native) == _local_state(oracle)
    assert native.regs[16] == 0x1A
    assert native.regs[17] == 3
    assert native.regs[18] == 3
    assert native.regs[20] == 0


def test_memory_and_csr_instructions_use_one_transactional_oracle_step():
    """Memory routing and reduced CSR semantics yield before native mutation."""
    program = assemble(
        f"""
        ld.b r1, r2
        csrr r4, {CSR_CPUID}
        halt
"""
    )
    native = Megapad64Micro(mem_size=256, core_id=1, num_cores=5)
    oracle = PythonMegapad64Micro(mem_size=256, core_id=1, num_cores=5)
    native.load_bytes(0, program)
    oracle.load_bytes(0, program)
    native.regs[2] = oracle.regs[2] = 0x80
    native.mem[0x80] = oracle.mem[0x80] = 0xA5

    fallback_calls = 0
    original_fallback = native._step_python_fallback_in_memory_scope

    def count_fallback():
        nonlocal fallback_calls
        fallback_calls += 1
        return original_fallback()

    native._step_python_fallback_in_memory_scope = count_fallback
    for _ in range(2):
        assert native.step() == oracle.step()
        assert _local_state(native) == _local_state(oracle)

    assert fallback_calls == 2
    assert native.regs[1] == 0xA5
    assert native.regs[4] == CPUID_MICRO


@pytest.mark.parametrize(
    "instruction",
    (
        bytes((0x80,)),          # stripped MEMALU family
        bytes((0xF9, 0, 0)),    # absent EXT.STRING engine
        assemble("bext r1, r2"),  # gated Tier-2 bitfield
    ),
)
def test_reduced_illegal_instruction_traps_match_python_oracle(instruction):
    """Fallback preserves the oracle's consumed PC and illegal-op vector."""
    native = Megapad64Micro(mem_size=64, core_id=1, num_cores=5)
    oracle = PythonMegapad64Micro(mem_size=64, core_id=1, num_cores=5)
    native.load_bytes(0, instruction)
    oracle.load_bytes(0, instruction)

    with pytest.raises(TrapError) as native_error:
        native.step()
    with pytest.raises(TrapError) as oracle_error:
        oracle.step()

    assert native_error.value.ivec_id == IVEC_ILLEGAL_OP
    assert oracle_error.value.ivec_id == IVEC_ILLEGAL_OP
    assert _local_state(native) == _local_state(oracle)
