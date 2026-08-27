"""Exact-singleton native scheduler fast-path contracts."""

from __future__ import annotations

from pathlib import Path

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
    subi r8, 1
    lbr loop
"""
REGISTER_BLOCK_SLICES = (1, 1, 2, 7, 19, 1_003)
JIT_PROFILE_COUNT_FIELDS = (
    "uncontended_jit_compile_attempts",
    "uncontended_jit_compilations",
    "uncontended_jit_compile_failures",
    "uncontended_jit_plan_evictions",
    "uncontended_jit_arena_allocations",
    "uncontended_jit_arena_allocation_failures",
    "uncontended_jit_slot_publications",
    "uncontended_jit_slot_rewrites",
    "uncontended_jit_code_bytes",
    "uncontended_jit_max_code_bytes",
    "uncontended_jit_executions",
    "uncontended_jit_steps",
)
JIT_PROFILE_WALL_FIELDS = (
    "uncontended_jit_compile",
    "uncontended_jit_arena_allocation",
    "uncontended_jit_publication",
)


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
    assert profile_snapshot["schema_version"] == 8
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
    if profile_snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert (
            counts["uncontended_jit_slot_publications"]
            == counts["uncontended_jit_compilations"]
        )
        assert (
            0 <= counts["uncontended_jit_slot_rewrites"]
            <= counts["uncontended_jit_slot_publications"]
        )
        assert (
            counts["uncontended_jit_code_bytes"]
            >= counts["uncontended_jit_max_code_bytes"]
            > 0
        )
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert (
            counts["uncontended_jit_steps"]
            <= counts["uncontended_block_steps"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


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

    assert snapshot["schema_version"] == 8
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
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert (
            counts["uncontended_jit_slot_publications"]
            == counts["uncontended_jit_compilations"]
        )
        assert counts["uncontended_jit_slot_rewrites"] == 0
        assert (
            counts["uncontended_jit_code_bytes"]
            >= counts["uncontended_jit_max_code_bytes"]
            > 0
        )
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert counts["uncontended_jit_steps"] <= (
            counts["uncontended_block_steps"]
        )
        assert 0 < wall_ns["uncontended_jit_arena_allocation"] <= (
            wall_ns["uncontended_jit_compile"]
        )
        assert 0 < wall_ns["uncontended_jit_publication"] <= (
            wall_ns["uncontended_jit_compile"]
        )
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["kind"] == "memfd-dual-mapped-fixed-slots"
        assert storage["w_x_model"] == "distinct-rw-and-rx-aliases"
        assert storage["ready"]
        assert not storage["failed"]
        assert storage["slot_count"] == 128
        assert storage["slot_bytes"] > counts[
            "uncontended_jit_max_code_bytes"
        ]
        assert storage["mapped_bytes_per_alias"] == (
            storage["slot_count"] * storage["slot_bytes"]
        )
        mapping_permissions = {
            line.split()[1]
            for line in Path("/proc/self/maps").read_text(
                encoding="ascii"
            ).splitlines()
            if "mp64-single-core-jit" in line
        }
        assert {"rw-s", "r-xs"} <= mapping_permissions
        assert all(
            not ("w" in permissions and "x" in permissions)
            for permissions in mapping_permissions
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )
        assert all(
            wall_ns[name] == 0
            for name in JIT_PROFILE_WALL_FIELDS
        )
    assert counts["uncontended_block_evictions"] == 0
    assert counts["logical_subfrontiers"] == 0
    assert counts["worker_commands"] == 0
    assert counts["private_steps"] == 0
    assert wall_ns["uncontended_round"] > 0
    assert wall_ns["uncontended_dispatch"] > 0


def test_host_profile_attributes_direct_mapped_translation_evictions() -> None:
    system = _system()
    first_block = assemble(
        """
loop:
    inc r4
    br loop
"""
    )
    colliding_block = assemble(
        """
loop:
    inc r5
    br loop
"""
    )
    first_address = 0
    colliding_address = 0x810
    system.load_binary(first_address, first_block)
    system.load_binary(colliding_address, colliding_block)
    system.boot(entry=first_address)
    owner = system._native_system
    owner._start_concurrency_profile()

    for address in (
        first_address,
        colliding_address,
        first_address,
    ):
        system.cpu.pc = address
        stats = system.run_batch_stats(6)
        assert stats.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])
    wall_ns = dict(snapshot["wall_ns"])

    assert system.cpu.regs[4] == 6
    assert system.cpu.regs[5] == 3
    assert counts["uncontended_block_evictions"] == 2
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 3
        assert counts["uncontended_jit_compilations"] == 3
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 2
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == 3
        assert counts["uncontended_jit_slot_rewrites"] == 2
        assert counts["uncontended_jit_code_bytes"] > 0
        assert counts["uncontended_jit_max_code_bytes"] > 0
        assert 0 < wall_ns["uncontended_jit_arena_allocation"] <= (
            wall_ns["uncontended_jit_compile"]
        )
        assert 0 < wall_ns["uncontended_jit_publication"] <= (
            wall_ns["uncontended_jit_compile"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )
        assert all(
            wall_ns[name] == 0
            for name in JIT_PROFILE_WALL_FIELDS
        )


def test_reusable_jit_arena_bounds_alternating_collision_churn() -> None:
    system = _system()
    first_address = 0
    colliding_address = 0x810
    system.load_binary(
        first_address,
        assemble(
            """
loop:
    inc r4
    br loop
"""
        ),
    )
    system.load_binary(
        colliding_address,
        assemble(
            """
loop:
    inc r5
    br loop
"""
        ),
    )
    system.boot(entry=first_address)
    owner = system._native_system
    owner._start_concurrency_profile()
    visit_count = 32

    for visit in range(visit_count):
        system.cpu.pc = (
            first_address if visit % 2 == 0 else colliding_address
        )
        stats = system.run_batch_stats(6)
        assert stats.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 3 * (visit_count // 2)
    assert system.cpu.regs[5] == 3 * (visit_count // 2)
    assert counts["uncontended_block_evictions"] == visit_count - 1
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == visit_count
        assert counts["uncontended_jit_compilations"] == visit_count
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == visit_count - 1
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == visit_count
        assert counts["uncontended_jit_slot_rewrites"] == visit_count - 1
        assert counts["uncontended_jit_code_bytes"] > 0
        assert counts["uncontended_jit_max_code_bytes"] > 0
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["ready"]
        assert not storage["failed"]
        assert storage["mapped_bytes_per_alias"] == (
            storage["slot_count"] * storage["slot_bytes"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_warm_boot_reuses_arena_after_invalidating_native_plans() -> None:
    system = _system()
    first_address = 0
    colliding_address = 0x810
    system.load_binary(
        first_address,
        assemble(
            """
loop:
    inc r4
    br loop
"""
        ),
    )
    system.load_binary(
        colliding_address,
        assemble(
            """
loop:
    inc r5
    br loop
"""
        ),
    )
    system.boot(entry=first_address)
    owner = system._native_system
    owner._start_concurrency_profile()

    first = system.run_batch_stats(6)
    assert first.instructions_executed == 6
    assert system.cpu.regs[4] == 3
    assert system.cpu.regs[5] == 0

    system.boot(entry=colliding_address)
    second = system.run_batch_stats(6)
    assert second.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 0
    assert system.cpu.regs[5] == 3
    assert counts["uncontended_block_lookups"] == 8
    assert counts["uncontended_block_misses"] == 6
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_builds"] == 2
    assert counts["uncontended_block_executions"] == 4
    assert counts["uncontended_block_steps"] == 8
    assert counts["uncontended_block_evictions"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 2
        assert counts["uncontended_jit_compilations"] == 2
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == 2
        assert counts["uncontended_jit_slot_rewrites"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["ready"]
        assert not storage["failed"]
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


@pytest.mark.parametrize(
    "invalidation",
    ("same-byte-write", "invalidate-all"),
)
def test_native_plan_survives_architectural_icache_refill(
    invalidation: str,
) -> None:
    system = _system()
    program = assemble(
        """
loop:
    inc r4
    br loop
"""
    )
    system.load_binary(0, program)
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    first = system.run_batch_stats(6)
    assert first.instructions_executed == 6
    if invalidation == "same-byte-write":
        system.cpu.mem_write8(0, program[0])
    else:
        system.cpu._cs.icache_control_write(3)
    second = system.run_batch_stats(6)
    assert second.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 6
    assert counts["uncontended_block_lookups"] == 8
    assert counts["uncontended_block_misses"] == 5
    assert counts["uncontended_block_hits"] == 3
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_executions"] == 4
    assert counts["uncontended_block_steps"] == 8
    assert counts["uncontended_block_evictions"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_slot_publications"] == 1
        assert counts["uncontended_jit_slot_rewrites"] == 0
        assert counts["uncontended_jit_executions"] == 3
        assert counts["uncontended_jit_steps"] == 6
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_changed_code_replaces_retained_native_plan_after_refill() -> None:
    system = _system()
    original = assemble(
        """
loop:
    inc r4
    br loop
"""
    )
    replacement = assemble(
        """
loop:
    inc r5
    br loop
"""
    )
    assert len(original) == len(replacement)
    assert original[1:] == replacement[1:]
    system.load_binary(0, original)
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    first = system.run_batch_stats(6)
    assert first.instructions_executed == 6
    system.cpu.mem_write8(0, replacement[0])
    second = system.run_batch_stats(6)
    assert second.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 3
    assert system.cpu.regs[5] == 3
    assert counts["uncontended_block_lookups"] == 8
    assert counts["uncontended_block_misses"] == 6
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_builds"] == 2
    assert counts["uncontended_block_executions"] == 4
    assert counts["uncontended_block_steps"] == 8
    assert counts["uncontended_block_evictions"] == 1
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 2
        assert counts["uncontended_jit_compilations"] == 2
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 1
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_slot_publications"] == 2
        assert counts["uncontended_jit_slot_rewrites"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_terminal_native_store_invalidates_retained_target_plan() -> None:
    system = _system()
    victim_address = 0x100
    writer = assemble(
        """
    inc r4
    st.b r5, r6
"""
    )
    original = assemble(
        """
loop:
    inc r7
    br loop
"""
    )
    replacement = assemble(
        """
loop:
    inc r8
    br loop
"""
    )
    assert len(original) == len(replacement)
    assert original[1:] == replacement[1:]
    system.load_binary(0, writer)
    system.load_binary(victim_address, original)
    system.boot(entry=victim_address)

    victim_warm = system.run_batch_stats(6)
    assert victim_warm.instructions_executed == 6
    for _ in range(3):
        system.cpu.pc = 0
        system.cpu.regs[5] = victim_address
        system.cpu.regs[6] = original[0]
        writer_warm = system.run_batch_stats(2)
        assert writer_warm.instructions_executed == 2
    system.cpu.pc = victim_address
    victim_refill = system.run_batch_stats(6)
    assert victim_refill.instructions_executed == 6
    original_executions = system.cpu.regs[7]
    assert original_executions == 6

    owner = system._native_system
    owner._start_concurrency_profile()
    system.cpu.pc = 0
    system.cpu.regs[5] = victim_address
    system.cpu.regs[6] = replacement[0]
    native_write = system.run_batch_stats(2)
    assert native_write.instructions_executed == 2
    system.cpu.pc = victim_address
    changed = system.run_batch_stats(6)
    assert changed.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[7] == original_executions
    assert system.cpu.regs[8] == 3
    assert system.cpu.mem[victim_address] == replacement[0]
    assert counts["uncontended_block_lookups"] == 5
    assert counts["uncontended_block_misses"] == 3
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_evictions"] == 1
    assert counts["uncontended_block_executions"] == 3
    assert counts["uncontended_block_steps"] == 6
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 1
        assert counts["uncontended_jit_arena_allocations"] == 0
        assert counts["uncontended_jit_slot_publications"] == 1
        assert counts["uncontended_jit_slot_rewrites"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def _assert_jit_used_when_available(snapshot: dict, counts: dict) -> None:
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] > 0
        assert counts["uncontended_jit_compilations"] > 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert (
            counts["uncontended_jit_slot_publications"]
            == counts["uncontended_jit_compilations"]
        )
        assert (
            counts["uncontended_jit_code_bytes"]
            >= counts["uncontended_jit_max_code_bytes"]
            > 0
        )
        assert counts["uncontended_jit_executions"] > 0
        assert counts["uncontended_jit_steps"] > 0
        assert counts["uncontended_jit_steps"] <= (
            counts["uncontended_block_steps"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


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


@pytest.mark.parametrize(
    (
        "load_instruction",
        "subi_immediate",
        "expected_value",
        "expected_arithmetic_flags",
        "expected_cycles",
    ),
    (
        (
            "ldi r4, 1",
            1,
            0,
            0x33,  # G preserved; zero, C/no-borrow, and even parity set.
            1_332,
        ),
        (
            "ldi r4, 0",
            1,
            0xFFFF_FFFF_FFFF_FFFF,
            0x34,  # G is preserved; N/P set and C clear on borrow.
            1_332,
        ),
        (
            "ldi64 r4, 0x8000000000000000",
            1,
            0x7FFF_FFFF_FFFF_FFFF,
            0x3A,  # G preserved; C/no-borrow, overflow, and even parity.
            1_665,
        ),
        (
            "ldi r4, 0",
            -128,
            0x80,
            0x20,  # G is preserved; all arithmetic flags are clear.
            1_332,
        ),
    ),
    ids=("zero", "borrow", "signed-overflow", "negative-immediate"),
)
def test_subi_executes_with_exact_native_flags(
    load_instruction: str,
    subi_immediate: int,
    expected_value: int,
    expected_arithmetic_flags: int,
    expected_cycles: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    {load_instruction}
    subi r4, {subi_immediate}
    br loop
"""
        ),
    )
    system.boot(entry=0)
    system.cpu.flag_g = 1
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(999)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 999
    assert stats.system_cycles_advanced == expected_cycles
    assert stats.per_core_cycles[0] == expected_cycles
    assert counts["uncontended_steps"] == 999
    assert system.cpu.regs[4] == expected_value
    assert system.cpu.pc == 0
    assert system.cpu.cycle_count == expected_cycles
    assert system.cpu.flags_pack() & 0x3F == expected_arithmetic_flags
    assert counts["uncontended_block_steps"] == 998
    _assert_jit_used_when_available(snapshot, counts)


def test_logical_immediate_shifts_preserve_guest_flags_natively() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    lsli r4, 0
    lsli r5, 15
    lsri r6, 0
    lsri r7, 15
    br loop
"""
        ),
    )
    system.boot(entry=0)
    system.cpu.regs[4] = 0x0123_4567_89AB_CDEF
    system.cpu.regs[5] = 0x8000_0000_0000_0001
    system.cpu.regs[6] = 0xFEDC_BA98_7654_3210
    system.cpu.regs[7] = 0x8000_0000_0000_0000
    owner = system._native_system
    owner._start_concurrency_profile()

    warmup = system.run_batch_stats(10)
    system.cpu.flags_unpack(0xAB)
    native = system.run_batch_stats(5)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert warmup.instructions_executed == 10
    assert warmup.system_cycles_advanced == 12
    assert warmup.per_core_cycles[0] == 12
    assert native.instructions_executed == 5
    assert native.system_cycles_advanced == 6
    assert native.per_core_cycles[0] == 6
    assert counts["uncontended_steps"] == 15
    assert counts["uncontended_block_steps"] == 14
    assert system.cpu.regs[4] == 0x0123_4567_89AB_CDEF
    assert system.cpu.regs[5] == 0x0000_2000_0000_0000
    assert system.cpu.regs[6] == 0xFEDC_BA98_7654_3210
    assert system.cpu.regs[7] == 0x0000_0000_0004_0000
    assert system.cpu.pc == 0
    assert system.cpu.flags_pack() == 0xAB
    assert system.cpu.cycle_count == 18
    assert owner.system_cycles == 18
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 1
        assert counts["uncontended_jit_steps"] == 5


@pytest.mark.parametrize(
    (
        "mask",
        "native_input",
        "native_pre_flags",
        "expected_value",
        "expected_flags",
    ),
    (
        (0x80, 0xFFFF_FFFF_FFFF_FFFF, 0xBF, 0x80, 0xA0),
        (0xFF, 0x8000_0000_0000_0000, 0xAE, 0, 0xB1),
    ),
    ids=("high-bit-mask", "full-byte-mask"),
)
def test_andi_executes_natively_with_zero_extended_mask_and_flags(
    mask: int,
    native_input: int,
    native_pre_flags: int,
    expected_value: int,
    expected_flags: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    andi r4, 0x{mask:02x}
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    warmup = system.run_batch_stats(4)
    system.cpu.regs[4] = native_input
    system.cpu.flags_unpack(native_pre_flags)
    native = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert warmup.instructions_executed == 4
    assert warmup.system_cycles_advanced == 6
    assert warmup.per_core_cycles[0] == 6
    assert native.instructions_executed == 2
    assert native.system_cycles_advanced == 3
    assert native.per_core_cycles[0] == 3
    assert counts["uncontended_steps"] == 6
    assert counts["uncontended_block_steps"] == 4
    assert system.cpu.regs[4] == expected_value
    assert system.cpu.pc == 0
    assert system.cpu.flags_pack() == expected_flags
    assert system.cpu.cycle_count == 9
    assert owner.system_cycles == 9
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 1
        assert counts["uncontended_jit_steps"] == 2


@pytest.mark.parametrize(
    ("immediate", "native_input", "native_pre_flags", "expected_flags"),
    (
        (1, 1, 0xAC, 0x93),
        (-128, 0x7FFF_FFFF_FFFF_FFFE, 0xA3, 0x9C),
        (2, 0x8000_0000_0000_0000, 0x95, 0xAA),
    ),
    ids=("equal", "negative-overflow", "unsigned-greater-overflow"),
)
def test_cmpi_executes_natively_with_exact_comparison_flags(
    immediate: int,
    native_input: int,
    native_pre_flags: int,
    expected_flags: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    cmpi r4, {immediate}
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    warmup = system.run_batch_stats(4)
    system.cpu.regs[4] = native_input
    system.cpu.flags_unpack(native_pre_flags)
    native = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert warmup.instructions_executed == 4
    assert warmup.system_cycles_advanced == 6
    assert warmup.per_core_cycles[0] == 6
    assert native.instructions_executed == 2
    assert native.system_cycles_advanced == 3
    assert native.per_core_cycles[0] == 3
    assert counts["uncontended_steps"] == 6
    assert counts["uncontended_block_steps"] == 4
    assert system.cpu.regs[4] == native_input
    assert system.cpu.pc == 0
    assert system.cpu.flags_pack() == expected_flags
    assert system.cpu.cycle_count == 9
    assert owner.system_cycles == 9
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 1
        assert counts["uncontended_jit_steps"] == 2


@pytest.mark.parametrize(
    ("target", "native_z", "native_taken", "taken_pc"),
    (
        ("loop", 1, True, 0),
        ("forward", 1, True, 0x200),
        ("loop", 0, False, 0),
    ),
    ids=(
        "taken-negative",
        "taken-positive",
        "not-taken",
    ),
)
def test_long_equal_branch_uses_live_flags_and_dynamic_cycles_natively(
    target: str,
    native_z: int,
    native_taken: bool,
    taken_pc: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    inc r4
    lbreq {target}
    inc r5

    .org 0x200
forward:
    nop
"""
        ),
    )
    system.boot(entry=0)
    warm_taken = not native_taken
    warm_z = 1 - native_z
    warm_flags = 0xAA | warm_z
    native_flags = 0xAA | native_z
    warm_pc = taken_pc if warm_taken else 4
    warm_cycles = 3 if warm_taken else 2
    native_pc = taken_pc if native_taken else 4
    native_cycles = 3 if native_taken else 2
    system.cpu.flags_unpack(warm_flags)
    owner = system._native_system
    owner._start_concurrency_profile()

    cold = system.run_batch_stats(2)
    assert cold.instructions_executed == 2
    assert cold.system_cycles_advanced == warm_cycles
    assert cold.per_core_cycles[0] == warm_cycles
    assert system.cpu.pc == warm_pc
    assert system.cpu.flags_pack() == warm_flags

    system.cpu.pc = 0
    decoded = system.run_batch_stats(2)
    assert decoded.instructions_executed == 2
    assert decoded.system_cycles_advanced == warm_cycles
    assert decoded.per_core_cycles[0] == warm_cycles
    assert system.cpu.pc == warm_pc
    assert system.cpu.flags_pack() == warm_flags

    system.cpu.pc = 0
    system.cpu.regs[4] = 0x1234
    system.cpu.regs[5] = 0x5678
    system.cpu.flags_unpack(native_flags)
    native = system.run_batch_stats(2)

    assert native.instructions_executed == 2
    assert native.system_cycles_advanced == native_cycles
    assert native.per_core_cycles[0] == native_cycles
    assert system.cpu.regs[4] == 0x1235
    assert system.cpu.regs[5] == 0x5678
    assert system.cpu.pc == native_pc
    assert system.cpu.flags_pack() == native_flags

    system.cpu.pc = 0
    system.cpu.regs[4] = 0x9ABC
    system.cpu.regs[5] = 0xDEF0
    system.cpu.flags_unpack(warm_flags)
    live = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert live.instructions_executed == 2
    assert live.system_cycles_advanced == warm_cycles
    assert live.per_core_cycles[0] == warm_cycles
    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_steps"] == 6
    assert system.cpu.regs[4] == 0x9ABD
    assert system.cpu.regs[5] == 0xDEF0
    assert system.cpu.pc == warm_pc
    assert system.cpu.flags_pack() == warm_flags
    expected_cycles = warm_cycles * 3 + native_cycles
    assert system.cpu.cycle_count == expected_cycles
    assert owner.system_cycles == expected_cycles
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4


def test_register_cmp_executes_natively_with_exact_comparison_flags(
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    cmp r4, r5
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    warmup = system.run_batch_stats(4)
    assert warmup.instructions_executed == 4
    assert warmup.system_cycles_advanced == 6
    assert warmup.per_core_cycles[0] == 6

    cases = (
        (1, 1, 0xAC, 0x93),
        (
            0x7FFF_FFFF_FFFF_FFFE,
            0xFFFF_FFFF_FFFF_FF80,
            0xA3,
            0x9C,
        ),
        (0x8000_0000_0000_0000, 2, 0x95, 0xAA),
    )
    for lhs, rhs, pre_flags, expected_flags in cases:
        system.cpu.pc = 0
        system.cpu.regs[4] = lhs
        system.cpu.regs[5] = rhs
        system.cpu.flags_unpack(pre_flags)

        native = system.run_batch_stats(2)

        assert native.instructions_executed == 2
        assert native.system_cycles_advanced == 3
        assert native.per_core_cycles[0] == 3
        assert system.cpu.regs[4] == lhs
        assert system.cpu.regs[5] == rhs
        assert system.cpu.pc == 0
        assert system.cpu.flags_pack() == expected_flags

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 10
    assert counts["uncontended_block_steps"] == 8
    assert system.cpu.cycle_count == 15
    assert owner.system_cycles == 15
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 3
        assert counts["uncontended_jit_steps"] == 6


@pytest.mark.parametrize(
    (
        "mnemonic",
        "taken_lhs",
        "taken_rhs",
        "taken_pre_flags",
        "taken_expected_flags",
        "live_rhs",
        "live_expected_flags",
    ),
    (
        (
            "lbrne",
            0x8000_0000_0000_0000,
            2,
            0x95,
            0xAA,
            7,
            0x93,
        ),
        ("lbrcc", 0, 1, 0xAB, 0x94, 6, 0xA2),
    ),
    ids=("not-equal", "carry-clear"),
)
def test_register_cmp_drives_long_flag_branch_natively(
    mnemonic: str,
    taken_lhs: int,
    taken_rhs: int,
    taken_pre_flags: int,
    taken_expected_flags: int,
    live_rhs: int,
    live_expected_flags: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    cmp r4, r5
    {mnemonic} mismatch
    inc r6

    .org 0x200
mismatch:
    nop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    for _ in range(2):
        system.cpu.pc = 0
        system.cpu.regs[4] = 1
        system.cpu.regs[5] = 1
        system.cpu.regs[6] = 0x1234
        system.cpu.flags_unpack(0xAC)

        warm = system.run_batch_stats(2)

        assert warm.instructions_executed == 2
        assert warm.system_cycles_advanced == 2
        assert warm.per_core_cycles[0] == 2
        assert system.cpu.regs[4] == 1
        assert system.cpu.regs[5] == 1
        assert system.cpu.regs[6] == 0x1234
        assert system.cpu.pc == 5
        assert system.cpu.flags_pack() == 0x93

    system.cpu.pc = 0
    system.cpu.regs[4] = taken_lhs
    system.cpu.regs[5] = taken_rhs
    system.cpu.regs[6] = 0x5678
    system.cpu.flags_unpack(taken_pre_flags)
    taken = system.run_batch_stats(2)

    assert taken.instructions_executed == 2
    assert taken.system_cycles_advanced == 3
    assert taken.per_core_cycles[0] == 3
    assert system.cpu.regs[4] == taken_lhs
    assert system.cpu.regs[5] == taken_rhs
    assert system.cpu.regs[6] == 0x5678
    assert system.cpu.pc == 0x200
    assert system.cpu.flags_pack() == taken_expected_flags

    system.cpu.pc = 0
    system.cpu.regs[4] = 7
    system.cpu.regs[5] = live_rhs
    system.cpu.regs[6] = 0x9ABC
    system.cpu.flags_unpack(0xAC)
    live = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert live.instructions_executed == 2
    assert live.system_cycles_advanced == 2
    assert live.per_core_cycles[0] == 2
    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_steps"] == 6
    assert system.cpu.regs[4] == 7
    assert system.cpu.regs[5] == live_rhs
    assert system.cpu.regs[6] == 0x9ABC
    assert system.cpu.pc == 5
    assert system.cpu.flags_pack() == live_expected_flags
    assert system.cpu.cycle_count == 9
    assert owner.system_cycles == 9
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4


@pytest.mark.parametrize(
    ("mnemonic", "not_taken_flags", "taken_flags"),
    (
        ("breq", 0xAA, 0xAB),
        ("brne", 0xAB, 0xAA),
        ("brcc", 0xAA, 0xA8),
    ),
    ids=("equal", "not-equal", "carry-clear"),
)
def test_short_byte_flag_branch_sign_extends_and_uses_live_flags_natively(
    mnemonic: str,
    not_taken_flags: int,
    taken_flags: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    inc r4
    {mnemonic} loop
    inc r5
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    warm_cases = (
        (not_taken_flags, 3, 2),
        (taken_flags, 0, 3),
    )
    for flags, expected_pc, expected_cycles in warm_cases:
        system.cpu.pc = 0
        system.cpu.regs[4] = 0x1000
        system.cpu.regs[5] = 0x2000
        system.cpu.flags_unpack(flags)

        warm = system.run_batch_stats(2)

        assert warm.instructions_executed == 2
        assert warm.system_cycles_advanced == expected_cycles
        assert warm.per_core_cycles[0] == expected_cycles
        assert system.cpu.regs[4] == 0x1001
        assert system.cpu.regs[5] == 0x2000
        assert system.cpu.pc == expected_pc
        assert system.cpu.flags_pack() == flags

    system.cpu.pc = 0
    system.cpu.regs[4] = 0x1234
    system.cpu.regs[5] = 0x5678
    system.cpu.flags_unpack(taken_flags)
    taken = system.run_batch_stats(2)

    assert taken.instructions_executed == 2
    assert taken.system_cycles_advanced == 3
    assert taken.per_core_cycles[0] == 3
    assert system.cpu.regs[4] == 0x1235
    assert system.cpu.regs[5] == 0x5678
    assert system.cpu.pc == 0
    assert system.cpu.flags_pack() == taken_flags

    system.cpu.pc = 0
    system.cpu.regs[4] = 0x9ABC
    system.cpu.regs[5] = 0xDEF0
    system.cpu.flags_unpack(not_taken_flags)
    live = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert live.instructions_executed == 2
    assert live.system_cycles_advanced == 2
    assert live.per_core_cycles[0] == 2
    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_steps"] == 6
    assert system.cpu.regs[4] == 0x9ABD
    assert system.cpu.regs[5] == 0xDEF0
    assert system.cpu.pc == 3
    assert system.cpu.flags_pack() == not_taken_flags
    assert system.cpu.cycle_count == 10
    assert owner.system_cycles == 10
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4


def test_register_sub_executes_natively_with_live_operands_and_flags(
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    sub r4, r5
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    warmup = system.run_batch_stats(4)
    assert warmup.instructions_executed == 4
    assert warmup.system_cycles_advanced == 6
    assert warmup.per_core_cycles[0] == 6

    cases = (
        (0, 1, 0xAB, 0xFFFF_FFFF_FFFF_FFFF, 0xB4),
        (
            0x8000_0000_0000_0000,
            2,
            0xB5,
            0x7FFF_FFFF_FFFF_FFFE,
            0xAA,
        ),
    )
    for lhs, rhs, pre_flags, expected_value, expected_flags in cases:
        system.cpu.pc = 0
        system.cpu.regs[4] = lhs
        system.cpu.regs[5] = rhs
        system.cpu.flags_unpack(pre_flags)

        native = system.run_batch_stats(2)

        assert native.instructions_executed == 2
        assert native.system_cycles_advanced == 3
        assert native.per_core_cycles[0] == 3
        assert system.cpu.regs[4] == expected_value
        assert system.cpu.regs[5] == rhs
        assert system.cpu.pc == 0
        assert system.cpu.flags_pack() == expected_flags

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_steps"] == 6
    assert system.cpu.cycle_count == 12
    assert owner.system_cycles == 12
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4


@pytest.mark.parametrize(
    ("mnemonic", "payloads"),
    (
        (
            "ldn",
            (
                (0x200, bytes.fromhex("10 32 54 76 98 ba dc fe")),
                (0x208, bytes.fromhex("11 22 33 44 55 66 77 88")),
                (0x210, bytes.fromhex("ef cd ab 89 67 45 23 01")),
                (0x218, bytes.fromhex("08 07 06 05 04 03 02 01")),
            ),
        ),
        (
            "ld.b",
            (
                (0x240, b"\x12"),
                (0x241, b"\x34"),
                (0x242, b"\xA5"),
                (0x243, b"\x3C"),
            ),
        ),
    ),
    ids=("qword", "byte"),
)
def test_leading_direct_read_executes_natively_with_live_ram(
    mnemonic: str,
    payloads: tuple[tuple[int, bytes], ...],
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    {mnemonic} r4, r5
    br loop
"""
        ),
    )
    for address, payload in payloads[:2]:
        system.load_binary(address, payload)
    system.boot(entry=0)
    system.cpu.regs[5] = payloads[0][0]
    system.cpu.flags_unpack(0xAA)
    owner = system._native_system
    owner._start_concurrency_profile()

    cold = system.run_batch_stats(2)
    assert cold.instructions_executed == 2
    assert cold.system_cycles_advanced == 3
    assert cold.per_core_cycles[0] == 3
    assert system.cpu.regs[4] == int.from_bytes(payloads[0][1], "little")

    system.cpu.regs[5] = payloads[1][0]
    planned = system.run_batch_stats(2)
    assert planned.instructions_executed == 2
    assert planned.system_cycles_advanced == 3
    assert planned.per_core_cycles[0] == 3
    assert system.cpu.regs[4] == int.from_bytes(payloads[1][1], "little")

    system.load_binary(*payloads[2])
    system.cpu.regs[5] = payloads[2][0]
    system.cpu.regs[4] = 0xFFFF_FFFF_FFFF_FFFF
    first_native = system.run_batch_stats(2)
    assert first_native.instructions_executed == 2
    assert first_native.system_cycles_advanced == 3
    assert first_native.per_core_cycles[0] == 3
    assert system.cpu.regs[4] == int.from_bytes(payloads[2][1], "little")

    system.load_binary(*payloads[3])
    system.cpu.regs[5] = payloads[3][0]
    system.cpu.regs[4] = 0xFFFF_FFFF_FFFF_FFFF
    cached_native = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert cached_native.instructions_executed == 2
    assert cached_native.system_cycles_advanced == 3
    assert cached_native.per_core_cycles[0] == 3
    assert counts["uncontended_steps"] == 8
    assert system.cpu.regs[4] == int.from_bytes(payloads[3][1], "little")
    assert system.cpu.regs[5] == payloads[3][0]
    assert system.cpu.pc == 0
    assert system.cpu.flags_pack() == 0xAA
    for address, payload in payloads:
        assert bytes(
            system.cpu.mem[address:address + len(payload)]
        ) == payload
    assert system.cpu.cycle_count == 12
    assert owner.system_cycles == 12

    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


@pytest.mark.parametrize(
    ("target_reg", "block_size", "cycles_per_pass"),
    (
        (5, 2, 2),
        (20, 3, 3),
    ),
    ids=("bare", "canonical-high-register"),
)
def test_terminal_sep_executes_natively_with_live_target(
    target_reg: int,
    block_size: int,
    cycles_per_pass: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
    inc r4
    sep r{target_reg}
"""
        ),
    )
    system.boot(entry=0)
    system.cpu.flags_unpack(0xAA)
    original_xsel = system.cpu.xsel
    original_spsel = system.cpu.spsel
    owner = system._native_system
    owner._start_concurrency_profile()

    targets = (0x180, 0x1A0, 0x1C0, 0x1E0)
    for pass_index, target in enumerate(targets, start=1):
        system.cpu.psel = 3
        system.cpu.pc = 0
        system.cpu.regs[target_reg] = target

        stats = system.run_batch_stats(2)

        assert stats.instructions_executed == 2
        assert stats.system_cycles_advanced == cycles_per_pass
        assert stats.per_core_cycles[0] == cycles_per_pass
        assert system.cpu.regs[4] == pass_index
        assert system.cpu.psel == target_reg
        assert system.cpu.pc == target
        assert system.cpu.regs[target_reg] == target
        assert system.cpu.regs[3] == block_size
        assert system.cpu.flags_pack() == 0xAA
        assert system.cpu.xsel == original_xsel
        assert system.cpu.spsel == original_spsel

        if pass_index == 2:
            planned = dict(owner._concurrency_profile_snapshot())
            planned_counts = dict(planned["counts"])
            assert planned_counts["uncontended_jit_compile_attempts"] == 0
            assert planned_counts["uncontended_jit_compilations"] == 0
            assert planned_counts["uncontended_jit_compile_failures"] == 0
            assert planned_counts["uncontended_jit_executions"] == 0
            assert planned_counts["uncontended_jit_steps"] == 0

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_steps"] == 6
    expected_cycles = cycles_per_pass * len(targets)
    assert system.cpu.cycle_count == expected_cycles
    assert owner.system_cycles == expected_cycles
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


def test_terminal_byte_store_executes_natively_with_live_operands(
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
    inc r4
    st.b r5, r6
"""
        ),
    )
    targets = (0x300, 0x310, 0x320, 0x330)
    values = (
        0x0123_4567_89AB_CD11,
        0xFEDC_BA98_7654_3222,
        0x1020_3040_5060_70A5,
        0x8877_6655_4433_223C,
    )
    for target in targets:
        system.load_binary(target, b"\xEE\xEE")
    system.boot(entry=0)
    system.cpu.flags_unpack(0xAA)
    original_xsel = system.cpu.xsel
    original_psel = system.cpu.psel
    original_spsel = system.cpu.spsel
    owner = system._native_system
    owner._start_concurrency_profile()

    for pass_index, (target, value) in enumerate(
        zip(targets, values, strict=True),
        start=1,
    ):
        system.cpu.pc = 0
        system.cpu.regs[5] = target
        system.cpu.regs[6] = value

        stats = system.run_batch_stats(2)

        assert stats.instructions_executed == 2
        assert stats.system_cycles_advanced == 2
        assert stats.per_core_cycles[0] == 2
        assert system.cpu.regs[4] == pass_index
        assert system.cpu.regs[5] == target
        assert system.cpu.regs[6] == value
        assert system.cpu.pc == 3
        assert system.cpu.flags_pack() == 0xAA
        assert system.cpu.xsel == original_xsel
        assert system.cpu.psel == original_psel
        assert system.cpu.spsel == original_spsel
        for target_index, observed_target in enumerate(targets):
            expected = (
                values[target_index] & 0xFF
                if target_index < pass_index
                else 0xEE
            )
            assert system.cpu.mem[observed_target] == expected
            assert system.cpu.mem[observed_target + 1] == 0xEE

        if pass_index == 2:
            planned = dict(owner._concurrency_profile_snapshot())
            planned_counts = dict(planned["counts"])
            assert planned_counts["uncontended_jit_compile_attempts"] == 0
            assert planned_counts["uncontended_jit_compilations"] == 0
            assert planned_counts["uncontended_jit_compile_failures"] == 0
            assert planned_counts["uncontended_jit_executions"] == 0
            assert planned_counts["uncontended_jit_steps"] == 0

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    assert system.cpu.cycle_count == 8
    assert owner.system_cycles == 8
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


def test_terminal_scalar_store_executes_natively_with_live_operands(
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
    inc r4
    str r5, r6
    inc r7
"""
        ),
    )
    targets = (0x301, 0x312, 0x323, 0x334)
    values = (
        0x0123_4567_89AB_CDEF,
        0xFEDC_BA98_7654_3210,
        0x1020_3040_5060_7080,
        0x8877_6655_4433_2211,
    )
    for target in targets:
        system.load_binary(target - 1, b"\xEE" * 10)
    system.boot(entry=0)
    system.cpu.flags_unpack(0xAA)
    original_xsel = system.cpu.xsel
    original_psel = system.cpu.psel
    original_spsel = system.cpu.spsel
    owner = system._native_system
    owner._start_concurrency_profile()

    for pass_index, (target, value) in enumerate(
        zip(targets, values, strict=True),
        start=1,
    ):
        system.cpu.pc = 0
        system.cpu.regs[5] = target
        system.cpu.regs[6] = value

        stats = system.run_batch_stats(2)

        assert stats.instructions_executed == 2
        assert stats.system_cycles_advanced == 2
        assert stats.per_core_cycles[0] == 2
        assert system.cpu.regs[4] == pass_index
        assert system.cpu.regs[5] == target
        assert system.cpu.regs[6] == value
        assert system.cpu.regs[7] == 0
        assert system.cpu.pc == 3
        assert system.cpu.flags_pack() == 0xAA
        assert system.cpu.xsel == original_xsel
        assert system.cpu.psel == original_psel
        assert system.cpu.spsel == original_spsel
        for target_index, observed_target in enumerate(targets):
            expected = (
                b"\xEE"
                + values[target_index].to_bytes(8, "little")
                + b"\xEE"
                if target_index < pass_index
                else b"\xEE" * 10
            )
            assert bytes(
                system.cpu.mem[
                    observed_target - 1:observed_target + 9
                ]
            ) == expected

        if pass_index == 2:
            planned = dict(owner._concurrency_profile_snapshot())
            planned_counts = dict(planned["counts"])
            assert planned_counts["uncontended_jit_compile_attempts"] == 0
            assert planned_counts["uncontended_jit_compilations"] == 0
            assert planned_counts["uncontended_jit_compile_failures"] == 0
            assert planned_counts["uncontended_jit_executions"] == 0
            assert planned_counts["uncontended_jit_steps"] == 0

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    assert system.cpu.cycle_count == 8
    assert owner.system_cycles == 8
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


@pytest.mark.parametrize(
    (
        "prefix",
        "constant_target",
        "return_address",
        "cycles_per_pass",
    ),
    (
        pytest.param("inc r4", None, 3, 3, id="live-target"),
        pytest.param(
            "ldi64 r5, 0x13579bdf2468ace0",
            0x1357_9BDF_2468_ACE0,
            13,
            4,
            id="loaded-constant-target",
        ),
    ),
)
def test_terminal_long_call_executes_natively_with_live_target_and_stack(
    prefix: str,
    constant_target: int | None,
    return_address: int,
    cycles_per_pass: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            f"""
    {prefix}
    call.l r5
    inc r7
"""
        ),
    )
    stack_tops = (0x900, 0x980, 0xA00, 0xA80)
    targets = (
        0x0123_4567_89AB_CDEF,
        0xFEDC_BA98_7654_3210,
        0x1020_3040_5060_7080,
        0x8877_6655_4433_2211,
    )
    for stack_top in stack_tops:
        system.load_binary(stack_top - 9, b"\xEE" * 10)
    system.boot(entry=0)
    system.cpu.flags_unpack(0xAA)
    original_xsel = system.cpu.xsel
    original_psel = system.cpu.psel
    original_spsel = system.cpu.spsel
    owner = system._native_system
    owner._start_concurrency_profile()

    for pass_index, (stack_top, target) in enumerate(
        zip(stack_tops, targets, strict=True),
        start=1,
    ):
        system.cpu.pc = 0
        system.cpu.regs[5] = target
        system.cpu.regs[original_spsel] = stack_top

        stats = system.run_batch_stats(2)

        assert stats.instructions_executed == 2
        expected_target = (
            target if constant_target is None else constant_target
        )
        assert stats.system_cycles_advanced == cycles_per_pass
        assert stats.per_core_cycles[0] == cycles_per_pass
        assert system.cpu.regs[4] == (
            pass_index if constant_target is None else 0
        )
        assert system.cpu.regs[5] == expected_target
        assert system.cpu.regs[7] == 0
        assert system.cpu.pc == expected_target
        assert system.cpu.regs[original_spsel] == stack_top - 8
        assert system.cpu.flags_pack() == 0xAA
        assert system.cpu.xsel == original_xsel
        assert system.cpu.psel == original_psel
        assert system.cpu.spsel == original_spsel
        for stack_index, observed_top in enumerate(stack_tops):
            expected = (
                b"\xEE"
                + return_address.to_bytes(8, "little")
                + b"\xEE"
                if stack_index < pass_index
                else b"\xEE" * 10
            )
            assert bytes(
                system.cpu.mem[observed_top - 9:observed_top + 1]
            ) == expected

        if pass_index == 2:
            planned = dict(owner._concurrency_profile_snapshot())
            planned_counts = dict(planned["counts"])
            assert planned_counts["uncontended_jit_compile_attempts"] == 0
            assert planned_counts["uncontended_jit_compilations"] == 0
            assert planned_counts["uncontended_jit_compile_failures"] == 0
            assert planned_counts["uncontended_jit_executions"] == 0
            assert planned_counts["uncontended_jit_steps"] == 0

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    expected_cycles = cycles_per_pass * len(stack_tops)
    assert system.cpu.cycle_count == expected_cycles
    assert owner.system_cycles == expected_cycles
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


def test_terminal_long_return_executes_natively_with_live_stack(
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
    inc r4
    ret.l
    inc r7
"""
        ),
    )
    stack_slots = (0x900, 0x980, 0xA00, 0xA80)
    targets = (
        0x0123_4567_89AB_CDEF,
        0xFEDC_BA98_7654_3210,
        0x1020_3040_5060_7080,
        0x8877_6655_4433_2211,
    )
    for stack_slot, target in zip(stack_slots, targets, strict=True):
        system.load_binary(
            stack_slot - 1,
            b"\xEE" + target.to_bytes(8, "little") + b"\xEE",
        )
    system.boot(entry=0)
    system.cpu.flags_unpack(0xAA)
    original_xsel = system.cpu.xsel
    original_psel = system.cpu.psel
    original_spsel = system.cpu.spsel
    owner = system._native_system
    owner._start_concurrency_profile()

    for pass_index, (stack_slot, target) in enumerate(
        zip(stack_slots, targets, strict=True),
        start=1,
    ):
        system.cpu.pc = 0
        system.cpu.regs[original_spsel] = stack_slot

        stats = system.run_batch_stats(2)

        assert stats.instructions_executed == 2
        assert stats.system_cycles_advanced == 3
        assert stats.per_core_cycles[0] == 3
        assert system.cpu.regs[4] == pass_index
        assert system.cpu.regs[7] == 0
        assert system.cpu.pc == target
        assert system.cpu.regs[original_spsel] == stack_slot + 8
        assert system.cpu.flags_pack() == 0xAA
        assert system.cpu.xsel == original_xsel
        assert system.cpu.psel == original_psel
        assert system.cpu.spsel == original_spsel
        for observed_slot, observed_target in zip(
            stack_slots,
            targets,
            strict=True,
        ):
            assert bytes(
                system.cpu.mem[observed_slot - 1:observed_slot + 9]
            ) == (
                b"\xEE"
                + observed_target.to_bytes(8, "little")
                + b"\xEE"
            )

        if pass_index == 2:
            planned = dict(owner._concurrency_profile_snapshot())
            planned_counts = dict(planned["counts"])
            assert planned_counts["uncontended_jit_compile_attempts"] == 0
            assert planned_counts["uncontended_jit_compilations"] == 0
            assert planned_counts["uncontended_jit_compile_failures"] == 0
            assert planned_counts["uncontended_jit_executions"] == 0
            assert planned_counts["uncontended_jit_steps"] == 0

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert counts["uncontended_steps"] == 8
    assert system.cpu.cycle_count == 12
    assert owner.system_cycles == 12
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0
