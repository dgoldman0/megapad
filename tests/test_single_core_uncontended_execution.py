"""Exact-singleton native scheduler fast-path contracts."""

from __future__ import annotations

import pytest

from asm import assemble
from devices import MMIO_BASE, SYSINFO_BASE
from megapad64 import IVEC_TIMER, Megapad64 as PythonMegapad64
from system import MegapadSystem


SHARED_WORD = 0x800
SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE
REGISTER_BLOCK_SOURCE = """
loop:
    ldi64 r13, 0xfedcba98765432a5
    inc r13
    ldi r12, 0xa5
    inc r12
    inc r4
    addi r5, 3
    xor r6, r5
    nop
    ori r9, 0x5a
    roli r6, 7
    add r4, r5
    dec r8
    nop
    mov r7, r6
    mov r11, r3
    inc r10
    lbr loop
"""
REGISTER_BLOCK_SLICES = (1, 1, 2, 7, 19, 1_003)


def _system(*, reference: bool = False) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2 if reference else 1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    return system


def _cpu_execution_signature(cpu) -> tuple:
    if hasattr(cpu, "_cs"):
        cache_valid, cache_tags, cache_data = cpu._cs.icache_snapshot()
    else:
        cache_valid = bytes(cpu._icache_valid)
        cache_tags = tuple(cpu._icache_tags)
        cache_data = bytes(cpu._icache_data)
    return (
        tuple(cpu.regs),
        cpu.pc,
        cpu.psel,
        cpu.xsel,
        cpu.spsel,
        cpu.flags_pack(),
        cpu.d_reg,
        cpu.halted,
        cpu.idle,
        cpu.cycle_count,
        cpu.perf_enable,
        cpu.perf_cycles,
        cpu.perf_stalls,
        cpu.perf_tileops,
        cpu._ext_modifier,
        bytes(cpu.mem),
        cpu.icache_hits,
        cpu.icache_misses,
        bytes(cache_valid),
        tuple(cache_tags),
        bytes(cache_data),
    )


def _core_signature(system: MegapadSystem) -> tuple:
    cpu_signature = _cpu_execution_signature(system.cpu)
    return (
        *cpu_signature[:16],
        system.timer.counter,
        system._native_system.system_cycles,
        *cpu_signature[16:],
    )


def _initialize_register_workload(cpu) -> None:
    cpu.regs[2] = 4096
    cpu.regs[15] = 4096
    cpu.regs[4] = 0x1020_3040
    cpu.regs[5] = 0xFFFF_FFFF_FFFF_FFF0
    cpu.regs[6] = 0x55AA
    cpu.regs[7] = 0xAA55
    cpu.regs[8] = 2
    cpu.regs[9] = 0x100
    cpu.regs[10] = 0xABCD
    cpu.perf_enable = 1


def _run_shared_memory_workload(*, reference: bool) -> tuple:
    system = _system(reference=reference)
    system.load_binary(
        0,
        assemble(
            """
loop:
    st.w r5, r4
    ld.w r6, r5
    add r4, r6
    xori r4, 0x5a
    br loop
"""
        ),
    )
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.regs[4] = 0x1020_3040
    system.cpu.regs[5] = SHARED_WORD
    system.timer.control = 1

    stats = system.run_batch_stats(5_003)
    stats_signature = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions[0],
        stats.per_core_cycles[0],
        stats.per_core_dispatches[0],
        stats.per_core_stop_reasons[0],
        stats.native_rounds,
        stats.native_continuations,
        stats.system_stop_reason,
    )
    return stats_signature, _core_signature(system)


def test_single_core_ram_loop_matches_generic_coordinator_reference() -> None:
    assert _run_shared_memory_workload(
        reference=False
    ) == _run_shared_memory_workload(reference=True)


def _run_register_block_workload(*, reference: bool) -> tuple:
    system = _system(reference=reference)
    system.load_binary(
        0,
        assemble(REGISTER_BLOCK_SOURCE),
    )
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    _initialize_register_workload(system.cpu)
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()

    batch_signatures = []
    for budget in REGISTER_BLOCK_SLICES:
        stats = system.run_batch_stats(budget)
        batch_signatures.append(
            (
                stats.instructions_executed,
                stats.system_cycles_advanced,
                stats.per_core_instructions[0],
                stats.per_core_cycles[0],
                stats.per_core_dispatches[0],
                stats.per_core_stop_reasons[0],
                stats.native_rounds,
                stats.native_continuations,
                stats.system_stop_reason,
                _core_signature(system),
            )
        )

    profile_snapshot = None
    if not reference:
        profile_snapshot = dict(owner._stop_concurrency_profile())
    return (
        tuple(batch_signatures),
        _core_signature(system),
        _cpu_execution_signature(system.cpu),
        profile_snapshot,
    )


def _run_python_register_workload() -> tuple:
    cpu = PythonMegapad64(mem_size=4096, num_cores=1)
    cpu.load_bytes(0, assemble(REGISTER_BLOCK_SOURCE))
    cpu.pc = 0
    _initialize_register_workload(cpu)
    for budget in REGISTER_BLOCK_SLICES:
        cpu.run(max_steps=budget)
    return _cpu_execution_signature(cpu)


def test_decoded_register_blocks_match_generic_reference_across_slices() -> None:
    (
        fast_batches,
        fast_core,
        fast_cpu,
        profile_snapshot,
    ) = _run_register_block_workload(reference=False)
    (
        reference_batches,
        reference_core,
        reference_cpu,
        _,
    ) = _run_register_block_workload(reference=True)

    assert fast_batches == reference_batches
    assert fast_core == reference_core
    assert fast_cpu == reference_cpu == _run_python_register_workload()
    assert profile_snapshot is not None
    assert profile_snapshot["schema_version"] == 6
    counts = dict(profile_snapshot["counts"])
    assert counts["uncontended_block_lookups"] == (
        counts["uncontended_block_hits"] +
        counts["uncontended_block_misses"]
    )
    assert counts["uncontended_block_builds"] > 0
    assert counts["uncontended_block_hits"] > 0
    assert counts["uncontended_block_executions"] > 0
    assert counts["uncontended_block_steps"] > (
        counts["uncontended_block_executions"]
    )
    jit_fields = (
        "uncontended_jit_compile_attempts",
        "uncontended_jit_compilations",
        "uncontended_jit_compile_failures",
        "uncontended_jit_executions",
        "uncontended_jit_steps",
    )
    if profile_snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert (
            counts["uncontended_jit_steps"]
            <= counts["uncontended_block_steps"]
        )
    else:
        assert all(counts[name] == 0 for name in jit_fields)


def test_mmio_asserted_interrupt_stops_before_the_next_instruction() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
    ei
    st.b r1, r2
    inc r4
"""
        ),
    )
    system.boot(entry=0)
    system.cpu.flag_i = 0
    system.cpu.regs[1] = SYSINFO_SINK
    system.cpu.regs[2] = 0xA5
    deliveries = []

    def assert_timer(_address: int, _value: int) -> None:
        system.timer.irq_pending = True

    def observe_trap(vector: int) -> None:
        deliveries.append((vector, system.cpu.regs[4]))
        system.timer.irq_pending = False
        system.cpu.flag_i = 0

    system.cpu._mmio_write8 = assert_timer
    system.cpu._trap = observe_trap
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(3)
    profile = dict(owner._stop_concurrency_profile())
    profile_counts = dict(profile["counts"])

    assert deliveries == [(IVEC_TIMER, 0)]
    assert system.cpu.regs[4] == 1
    assert stats.instructions_executed == 3
    assert stats.per_core_dispatches == (2,)
    assert stats.native_rounds == 2
    assert profile_counts["uncontended_interrupt_boundaries"] == 1
    assert profile_counts["uncontended_dispatches"] == 2
    assert profile_counts["uncontended_steps"] == 3


class _CallbackFailure(RuntimeError):
    pass


def _run_callback_failure(*, reference: bool) -> tuple:
    system = _system(reference=reference)
    system.load_binary(
        0,
        assemble(
            """
    inc r5
    inc r5
    st.b r2, r4
    inc r5
"""
        ),
    )
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.regs[2] = SYSINFO_SINK
    system.cpu.regs[4] = 0x5A
    system.timer.control = 1
    failure = _CallbackFailure("single-core callback probe")

    def fail_write(_address: int, _value: int) -> None:
        raise failure

    system.cpu._mmio_write8 = fail_write
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    with pytest.raises(_CallbackFailure) as raised:
        system.run_batch_stats(4)
    assert raised.value is failure
    if not reference:
        profile_counts = dict(
            dict(owner._stop_concurrency_profile())["counts"]
        )
        assert profile_counts["uncontended_callback_errors"] == 1
        assert profile_counts["uncontended_dispatches"] == 1
        assert profile_counts["uncontended_steps"] == 2
    return _core_signature(system)


def test_callback_error_settles_exact_completed_prefix() -> None:
    fast = _run_callback_failure(reference=False)
    reference = _run_callback_failure(reference=True)

    assert fast == reference
    assert fast[0][5] == 2
    assert fast[9] == 2
    assert fast[16] == 2
    assert fast[17] == 2


def test_injected_internal_failure_retains_and_clocks_completed_prefix() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
    inc r4
    inc r4
    inc r4
    inc r4
"""
        ),
    )
    system.boot(entry=0)
    system.timer.control = 1
    system._native_system._inject_uncontended_failure_after_native_steps(3)

    with pytest.raises(
        RuntimeError,
        match="injected uncontended single-core scheduler failure",
    ):
        system.run_batch_stats(4)

    assert system.cpu.regs[4] == 3
    assert system.cpu.pc == 3
    assert system.cpu.cycle_count == 3
    assert system.timer.counter == 3
    assert system._native_system.system_cycles == 3

    stats = system.run_batch_stats(1)
    assert stats.instructions_executed == 1
    assert system.cpu.regs[4] == 4


def test_host_profile_attributes_singleton_work_to_uncontended_path() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    inc r4
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(2_503)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])
    wall_ns = dict(snapshot["wall_ns"])

    assert snapshot["schema_version"] == 6
    assert counts["uncontended_rounds"] == stats.native_rounds == 3
    assert counts["uncontended_dispatches"] == sum(
        stats.per_core_dispatches
    )
    assert counts["uncontended_steps"] == stats.instructions_executed
    assert counts["uncontended_continuations"] == 0
    assert counts["uncontended_callback_errors"] == 0
    assert system.cpu.regs[4] == 1_252
    assert system.cpu.pc == 1
    assert system.cpu.cycle_count == 3_754
    # The cold INC and the first BR remain authoritative; all subsequent
    # work requires the two-instruction INC/BR block.
    assert counts["uncontended_block_steps"] == (
        counts["uncontended_steps"] - 2
    )
    jit_fields = (
        "uncontended_jit_compile_attempts",
        "uncontended_jit_compilations",
        "uncontended_jit_compile_failures",
        "uncontended_jit_executions",
        "uncontended_jit_steps",
    )
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert counts["uncontended_jit_steps"] <= (
            counts["uncontended_block_steps"]
        )
    else:
        assert all(counts[name] == 0 for name in jit_fields)
    assert counts["logical_subfrontiers"] == 0
    assert counts["worker_commands"] == 0
    assert counts["private_steps"] == 0
    assert wall_ns["uncontended_round"] > 0
    assert wall_ns["uncontended_dispatch"] > 0


def _assert_jit_used_when_available(snapshot: dict, counts: dict) -> None:
    jit_fields = (
        "uncontended_jit_compile_attempts",
        "uncontended_jit_compilations",
        "uncontended_jit_compile_failures",
        "uncontended_jit_executions",
        "uncontended_jit_steps",
    )
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert counts["uncontended_jit_steps"] <= (
            counts["uncontended_block_steps"]
        )
    else:
        assert all(counts[name] == 0 for name in jit_fields)


def _assert_repeated_ldi_block(
    instruction: str,
    loaded_value: int,
    expected_cycles: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    {instruction}
    inc r4
    br loop
"""
        ),
    )
    system.boot(entry=0)
    system.cpu.regs[4] = 0xFFFF_FFFF_FFFF_FF00
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(999)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 999
    assert stats.system_cycles_advanced == expected_cycles
    assert stats.per_core_cycles[0] == expected_cycles
    assert counts["uncontended_steps"] == 999
    assert system.cpu.regs[4] == loaded_value + 1
    assert system.cpu.pc == 0
    assert system.cpu.cycle_count == expected_cycles
    # Only the first cold LDI is authoritative. The resident INC/BR suffix
    # and every subsequent complete LDI/INC/BR loop execute as blocks.
    assert counts["uncontended_block_steps"] == 998
    _assert_jit_used_when_available(snapshot, counts)


def test_unprefixed_ldi_executes_in_repeated_native_block() -> None:
    _assert_repeated_ldi_block("ldi r4, 0xa5", 0xA5, 1_332)


def test_ext_imm64_ldi_executes_in_repeated_native_block() -> None:
    _assert_repeated_ldi_block(
        "ldi64 r4, 0xfedcba98765432a5",
        0xFEDC_BA98_7654_32A5,
        1_665,
    )


def test_unconditional_long_branch_executes_in_native_block() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    inc r4
    lbr far

    .org 0x100
far:
    inc r5
    lbr loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1_000)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 1_000
    assert stats.system_cycles_advanced == 1_500
    assert stats.per_core_cycles[0] == 1_500
    assert counts["uncontended_steps"] == 1_000
    assert system.cpu.regs[4] == 250
    assert system.cpu.regs[5] == 250
    assert system.cpu.pc == 0
    assert system.cpu.cycle_count == 1_500
    # The two cold INC/LBR pairs remain authoritative. Thereafter the
    # +252 and -260 edges each require a complete terminal LBR block.
    assert counts["uncontended_block_steps"] == 996
    _assert_jit_used_when_available(snapshot, counts)
