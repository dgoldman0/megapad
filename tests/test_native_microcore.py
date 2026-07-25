"""Phase 2 element-5 native microcore execution boundaries."""

from __future__ import annotations

import pytest

from accel_wrapper import Megapad64Micro
from asm import assemble
from bench_phase2_microcore import (
    REPORT_SCHEMA,
    REPORT_SCHEMA_VERSION,
    STATE_SCHEMA,
    STATE_SCHEMA_VERSION,
    run_report,
)
from megapad64 import (
    CPUID_MICRO,
    CSR_CPUID,
    IVEC_ILLEGAL_OP,
    Megapad64 as PythonMegapad64,
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
    dec r17
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
    assert native.regs[17] == 2
    assert native.regs[18] == 3
    assert native.regs[20] == 0


@pytest.mark.parametrize(
    "cpu_type",
    (PythonMegapad64, PythonMegapad64Micro),
)
def test_python_isa_rex_inc_and_dec_select_high_registers(cpu_type):
    """The Python oracle applies the REX nibble bit to unary GPR ops."""
    cpu = cpu_type(mem_size=64, core_id=0, num_cores=1)
    cpu.load_bytes(
        0,
        assemble(
            """
            ldi r16, 7
            ldi r17, 9
            inc r16
            dec r17
            halt
"""
        ),
    )

    while not cpu.halted:
        cpu.step()

    assert cpu.regs[16] == 8
    assert cpu.regs[17] == 8
    assert cpu.regs[0] == 0
    assert cpu.regs[1] == 0


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


def test_single_active_microcore_benchmark_is_versioned_and_deterministic():
    """The element-5 benchmark pins native execution without fairness claims."""
    report = run_report(
        instructions=9,
        repeats=2,
        warmups=0,
        warmup_instructions=1,
    )

    assert report["schema"] == REPORT_SCHEMA
    assert report["schema_version"] == REPORT_SCHEMA_VERSION
    assert report["determinism"]["canonical_state_matches"]
    assert report["determinism"]["behavior_oracle_matches"]
    assert report["semantics"]["native_scheduler_expected"] is False
    scope = report["semantics"]["qos_and_fairness_scope"]
    assert scope["contention_exercised"] is False
    assert scope["qos_claim"] == "excluded"
    assert scope["fairness_claim"] == "excluded"

    hashes = set()
    for sample in report["samples"]:
        observation = sample["observation"]
        assert observation["state_schema"] == STATE_SCHEMA
        assert observation["state_schema_version"] == STATE_SCHEMA_VERSION
        hashes.add(observation["behavior_oracle_sha256"])
        state = observation["canonical_state"]
        execution = state["execution"]
        assert execution["instructions_executed"] == 9
        assert execution["per_core_instructions"] == [0, 9, 0, 0, 0]
        assert execution["per_core_cycles"][0] == 0
        assert execution["per_core_cycles"][1] == 13
        assert execution["per_core_cycles"][2:] == [0, 0, 0]
        assert execution["system_cycles_advanced"] == 13
        assert execution["native_scheduler"] is False
        assert execution["native_batch_runs_counter_delta"] == 0
        assert execution["native_dispatches_counter_delta"] == 0
        assert execution["python_fallback_instantiated"] is False

        topology = state["topology"]
        assert topology["active_core_is_system_owned_micro_profile"]
        assert topology["cluster_enabled"] is False
        assert state["workload"]["contention_exercised"] is False
        assert state["cores"][1]["profile"] == "micro"
        assert state["cores"][1]["accelerated_wrapper"]
        assert state["cores"][1]["common_gprs_r0_r15"][1] == 5
        assert all(
            core["halted"]
            for index, core in enumerate(state["cores"])
            if index != 1
        )
        assert state["main_bus"]["active_grant"] is False
        assert state["main_bus"]["pending_request_count"] == 0

    assert len(hashes) == 1
