"""Exact-singleton native scheduler fast-path contracts."""

from __future__ import annotations

import pytest

from asm import assemble
from devices import MMIO_BASE, SYSINFO_BASE
from megapad64 import IVEC_TIMER
from system import MegapadSystem


SHARED_WORD = 0x800
SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE


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


def _core_signature(system: MegapadSystem) -> tuple:
    cpu = system.cpu
    return (
        tuple(cpu.regs),
        cpu.pc,
        cpu.flags_pack(),
        cpu.d_reg,
        cpu.halted,
        cpu.idle,
        cpu.cycle_count,
        bytes(cpu.mem),
        system.timer.counter,
        system._native_system.system_cycles,
    )


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
    assert fast[6] == 2
    assert fast[8] == 2
    assert fast[9] == 2


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

    assert snapshot["schema_version"] == 4
    assert counts["uncontended_rounds"] == stats.native_rounds == 3
    assert counts["uncontended_dispatches"] == sum(
        stats.per_core_dispatches
    )
    assert counts["uncontended_steps"] == stats.instructions_executed
    assert counts["uncontended_continuations"] == 0
    assert counts["uncontended_callback_errors"] == 0
    assert counts["logical_subfrontiers"] == 0
    assert counts["worker_commands"] == 0
    assert counts["private_steps"] == 0
    assert wall_ns["uncontended_round"] > 0
    assert wall_ns["uncontended_dispatch"] > 0
