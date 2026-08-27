"""Compact cross-backend MP64 semantic boundary matrix."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64 as NativeMegapad64
from asm import assemble
from megapad64 import Megapad64 as PythonMegapad64


MMIO_START = 0xFFFF_FF00_0000_0000
CPU_TYPES = (
    pytest.param(PythonMegapad64, id="python"),
    pytest.param(NativeMegapad64, id="native"),
)


class _StackAccessFailure(RuntimeError):
    pass


def _semantic_state(cpu) -> tuple:
    return (
        tuple(cpu.regs),
        cpu.psel,
        cpu.xsel,
        cpu.spsel,
        cpu.flags_pack(),
        cpu.q_out,
        cpu.ef_flags,
        cpu.t_reg,
        cpu.cycle_count,
        cpu._ext_modifier,
        bytes(cpu.mem),
    )


def _cpu_pair(code: bytes):
    oracle = PythonMegapad64(mem_size=4096)
    native = NativeMegapad64(mem_size=4096)
    for cpu in (oracle, native):
        cpu.load_bytes(0, code)
        cpu.pc = 0
        cpu.perf_enable = 1
    return oracle, native


@pytest.mark.parametrize("condition", range(16))
def test_every_short_branch_condition_matches_python_oracle(
    condition: int,
) -> None:
    oracle, native = _cpu_pair(bytes((0x30 | condition, 0x05)))
    for cpu in (oracle, native):
        cpu.flags_unpack(0b1010_1101)
        cpu.q_out = 0
        cpu.ef_flags = 0x4

    expected_taken = oracle.eval_cond(condition)
    oracle_cycles = oracle.step()
    native_cycles = native.step()

    assert native_cycles == oracle_cycles == (2 if expected_taken else 1)
    assert native.pc == oracle.pc == (7 if expected_taken else 2)
    assert _semantic_state(native) == _semantic_state(oracle)


def test_call_samples_stack_selector_target_before_push() -> None:
    oracle, native = _cpu_pair(assemble("call.l r15"))
    stack_top = 0x280
    for cpu in (oracle, native):
        cpu.regs[cpu.spsel] = stack_top

    assert oracle.step() == native.step() == 2

    assert native.pc == oracle.pc == stack_top
    assert native.regs[native.spsel] == stack_top - 8
    assert bytes(native.mem[stack_top - 8:stack_top]) == (2).to_bytes(
        8,
        "little",
    )
    assert _semantic_state(native) == _semantic_state(oracle)


def test_long_return_matches_python_stack_order() -> None:
    oracle, native = _cpu_pair(assemble("ret.l"))
    stack_slot = 0x280
    target = 0x345
    for cpu in (oracle, native):
        cpu.regs[cpu.spsel] = stack_slot
        cpu.mem[stack_slot:stack_slot + 8] = target.to_bytes(8, "little")

    assert oracle.step() == native.step() == 2

    assert native.pc == oracle.pc == target
    assert native.regs[native.spsel] == stack_slot + 8
    assert _semantic_state(native) == _semantic_state(oracle)


@pytest.mark.parametrize("cpu_type", CPU_TYPES)
def test_call_write_failure_retains_decremented_stack_and_return_pc(
    cpu_type,
) -> None:
    cpu = cpu_type(mem_size=4096)
    cpu.load_bytes(0, assemble("call.l r4"))
    cpu.pc = 0
    cpu.regs[4] = 0x340
    failure = _StackAccessFailure("CALL stack write")

    if cpu_type is PythonMegapad64:
        stack_top = 0x280

        def fail_write64(_address: int, _value: int) -> None:
            raise failure

        cpu.mem_write64 = fail_write64
    else:
        stack_top = MMIO_START + 0x208

        def fail_write8(_address: int, _value: int) -> None:
            raise failure

        cpu.mem_write8 = fail_write8

    cpu.regs[cpu.spsel] = stack_top

    with pytest.raises(_StackAccessFailure) as raised:
        cpu.step()

    assert raised.value is failure
    assert cpu.pc == 2
    assert cpu.regs[cpu.spsel] == stack_top - 8
    assert cpu.regs[4] == 0x340
    assert cpu.cycle_count == 0


@pytest.mark.parametrize("cpu_type", CPU_TYPES)
def test_return_read_failure_retains_stack_and_post_fetch_pc(cpu_type) -> None:
    cpu = cpu_type(mem_size=4096)
    cpu.load_bytes(0, assemble("ret.l"))
    cpu.pc = 0
    failure = _StackAccessFailure("RET stack read")

    if cpu_type is PythonMegapad64:
        stack_slot = 0x280
        # Populate the instruction window before replacing the same natural-
        # width primitive used by a cold fetch fill.
        cpu._icache_read_byte(0)

        def fail_read64(_address: int) -> int:
            raise failure

        cpu.mem_read64 = fail_read64
    else:
        stack_slot = MMIO_START + 0x280

        def fail_read8(_address: int) -> int:
            raise failure

        cpu.mem_read8 = fail_read8

    cpu.regs[cpu.spsel] = stack_slot

    with pytest.raises(_StackAccessFailure) as raised:
        cpu.step()

    assert raised.value is failure
    assert cpu.pc == 1
    assert cpu.regs[cpu.spsel] == stack_slot
    assert cpu.cycle_count == 0
