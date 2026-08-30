"""Exact-singleton native scheduler fast-path contracts."""

from __future__ import annotations

from pathlib import Path

import pytest

from asm import assemble
from devices import MMIO_BASE, SYSINFO_BASE
from megapad64 import IVEC_IPI, IVEC_TIMER, Megapad64 as PythonMegapad64
from system import MegapadSystem


SHARED_WORD = 0x800
SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE
NATURAL_CALLBACK_BASE = MMIO_BASE + 0x1_0000
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
BYTE_COPY_MOTIF_SOURCE = """
    ld.b r0, r9
    st.b r7, r0
"""
NATURAL_COPY_MOTIF_SOURCE = """
    ldn r0, r9
    str r7, r0
"""
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
    "uncontended_jit_native_entries",
    "uncontended_jit_native_returns",
)
JIT_PROFILE_WALL_FIELDS = (
    "uncontended_jit_compile",
    "uncontended_jit_arena_allocation",
    "uncontended_jit_publication",
)
JIT_REGION_PROFILE_COUNT_FIELDS = (
    "uncontended_jit_region_compile_attempts",
    "uncontended_jit_region_compilations",
    "uncontended_jit_region_compile_failures",
    "uncontended_jit_region_entries",
    "uncontended_jit_region_blocks",
    "uncontended_jit_region_steps",
    "uncontended_jit_region_target_identity_misses",
)
TWO_BLOCK_REGION_RING_SOURCE = """
first:
    inc r4
    br second
second:
    inc r5
    br first
"""
CONDITIONAL_TARGET_REGION_SOURCE = """
source:
    inc r4
    br target
target:
    inc r5
    breq source
"""
PSEL_OPERAND_REGION_SOURCE = """
source:
    umul r4, r3
    br target
target:
    inc r5
    br source
"""


def _assert_block_cache_profile_reconciles(snapshot: dict) -> None:
    assert snapshot["schema_version"] == 17
    assert dict(snapshot["single_core_block_cache"]) == {
        "kind": "set-associative-exact-icache-span",
        "sets": 1_024,
        "ways": 4,
        "entries": 4_096,
        "identity_bytes": 16,
    }
    rejection_metadata = dict(
        snapshot["single_core_block_rejection_cache"]
    )
    assert rejection_metadata == {
        "kind": "set-associative-exact-icache-span",
        "sets": 512,
        "ways": 4,
        "entries": 2_048,
        "identity_bytes": 16,
    }
    counts = dict(snapshot["counts"])
    assert counts["uncontended_block_misses"] == (
        counts["uncontended_block_rejection_cache_hits"]
        + counts["uncontended_block_build_attempts"]
    )
    assert counts["uncontended_block_build_attempts"] == (
        counts["uncontended_block_builds"]
        + counts["uncontended_block_nonresident_rejections"]
        + counts["uncontended_block_zero_instruction_rejections"]
        + counts["uncontended_block_one_instruction_rejections"]
    )
    assert counts["uncontended_block_rejection_cache_stores"] == (
        counts["uncontended_block_zero_instruction_rejections"]
        + counts["uncontended_block_one_instruction_rejections"]
    )
    assert (
        counts["uncontended_block_rejection_cache_replacements"]
        <= counts["uncontended_block_rejection_cache_stores"]
    )
    assert counts["uncontended_jit_native_entries"] == (
        counts["uncontended_jit_native_returns"]
    )
    assert counts["uncontended_jit_executions"] == (
        counts["uncontended_jit_native_entries"]
        + counts["uncontended_jit_region_entries"]
    )
    assert counts["uncontended_jit_region_blocks"] == (
        2 * counts["uncontended_jit_region_entries"]
    )
    assert counts["uncontended_jit_region_blocks"] <= (
        counts["uncontended_jit_executions"]
    )
    assert counts["uncontended_jit_region_steps"] <= (
        counts["uncontended_jit_steps"]
    )
    region_storage = dict(snapshot["single_core_jit_region_storage"])
    assert isinstance(region_storage["enabled"], bool)
    assert region_storage["kind"] == (
        "memfd-dual-mapped-fixed-slots"
        if snapshot["single_core_jit_backend"] == "x86_64"
        else "unavailable"
    )
    assert region_storage["w_x_model"] == (
        "distinct-rw-and-rx-aliases"
        if snapshot["single_core_jit_backend"] == "x86_64"
        else "unavailable"
    )
    if region_storage["ready"]:
        assert not region_storage["failed"]
        assert region_storage["slot_count"] == 4_096
        assert region_storage["slot_bytes"] == 1_344
        assert region_storage["mapped_bytes_per_alias"] == (
            region_storage["slot_count"] * region_storage["slot_bytes"]
        )
    successor_profile = dict(
        snapshot["single_core_jit_successor_profile"]
    )
    assert successor_profile["kind"] == (
        "bounded-set-associative-space-saving"
    )
    assert successor_profile["scope"] == (
        "consecutive-complete-helper-free-register-control-x86_64-"
        "blocks-within-one-uncontended-segment"
    )
    assert successor_profile["sets"] == 1_024
    assert successor_profile["ways"] == 8
    assert successor_profile["entries"] == 8_192
    assert successor_profile["candidate_block_completions"] >= (
        successor_profile["observations"]
    )
    assert successor_profile["replacements"] <= (
        successor_profile["observations"]
    )
    assert successor_profile["exact"] == (
        successor_profile["replacements"] == 0
        and not successor_profile["counter_saturated"]
    )
    assert isinstance(successor_profile["edges"], list)


def _system(
    *,
    reference: bool = False,
    ext_mem_size: int = 0,
) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2 if reference else 1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=ext_mem_size,
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
    _assert_block_cache_profile_reconciles(profile_snapshot)
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


REGISTER_AND_SOURCE = """
loop:
    ldi r7, 0xff
    and r7, r3
    and r4, r5
    and r6, r6
    br loop
"""
REGISTER_AND_SLICES = (1, 1, 2, 1, 5, 10)
REGISTER_AND_INITIAL_FLAGS = 0xEA


def _initialize_register_and_workload(
    cpu,
    *,
    lhs: int,
    rhs: int,
    alias_value: int,
) -> None:
    cpu.regs[2] = 4096
    cpu.regs[15] = 4096
    cpu.regs[4] = lhs
    cpu.regs[5] = rhs
    cpu.regs[6] = alias_value
    cpu.regs[7] = 0
    cpu.flags_unpack(REGISTER_AND_INITIAL_FLAGS)
    cpu.perf_enable = 1


def _run_register_and_workload(
    *,
    reference: bool,
    lhs: int,
    rhs: int,
    alias_value: int,
) -> tuple:
    system = _system(reference=reference)
    system.load_binary(0, assemble(REGISTER_AND_SOURCE))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    _initialize_register_and_workload(
        system.cpu,
        lhs=lhs,
        rhs=rhs,
        alias_value=alias_value,
    )
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()

    batch_signatures = []
    for budget in REGISTER_AND_SLICES:
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


def _run_python_register_and_workload(
    *,
    lhs: int,
    rhs: int,
    alias_value: int,
) -> tuple:
    cpu = PythonMegapad64(mem_size=4096, num_cores=1)
    cpu.load_bytes(0, assemble(REGISTER_AND_SOURCE))
    cpu.pc = 0
    _initialize_register_and_workload(
        cpu,
        lhs=lhs,
        rhs=rhs,
        alias_value=alias_value,
    )
    for budget in REGISTER_AND_SLICES:
        cpu.run(max_steps=budget)
    return _cpu_execution_signature(cpu)


@pytest.mark.parametrize(
    ("lhs", "rhs", "alias_value", "expected_flags"),
    (
        pytest.param(
            0xFFFF_0000_F0F0_0F0F,
            0x0FF0_FFFF_00FF_FF00,
            0,
            0xF1,
            id="zero-alias-result",
        ),
        pytest.param(
            0xFEDC_BA98_7654_3210,
            0x0F0F_0F0F_F0F0_F0F0,
            0x8000_0000_0000_0001,
            0xE4,
            id="negative-alias-result",
        ),
    ),
)
def test_register_and_blocks_match_generic_and_python_across_slices(
    lhs: int,
    rhs: int,
    alias_value: int,
    expected_flags: int,
) -> None:
    fast_batches, fast_core, fast_cpu, snapshot = (
        _run_register_and_workload(
            reference=False,
            lhs=lhs,
            rhs=rhs,
            alias_value=alias_value,
        )
    )
    reference_batches, reference_core, reference_cpu, _ = (
        _run_register_and_workload(
            reference=True,
            lhs=lhs,
            rhs=rhs,
            alias_value=alias_value,
        )
    )

    assert fast_batches == reference_batches
    assert fast_core == reference_core
    assert fast_cpu == reference_cpu == _run_python_register_and_workload(
        lhs=lhs,
        rhs=rhs,
        alias_value=alias_value,
    )
    assert snapshot is not None
    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == sum(REGISTER_AND_SLICES) == 20
    assert counts["uncontended_block_steps"] == 18
    assert fast_cpu[0][4] == lhs & rhs
    assert fast_cpu[0][6] == alias_value
    # R3 is the selected PC. AND must sample its post-fetch value even when
    # generated code would otherwise keep the PC private in host R9.
    assert fast_cpu[0][7] == 5
    assert fast_cpu[1] == 0
    assert fast_cpu[9] == 24
    assert fast_cpu[10] == 1
    assert fast_cpu[11] == 24
    assert fast_cpu[5] == expected_flags
    assert (expected_flags & 0x08) == 0
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 10


@pytest.mark.parametrize(
    (
        "instruction",
        "initial_registers",
        "initial_flags",
        "expected_encoding",
        "expected_register",
        "expected_value",
        "expected_flags",
        "expected_cycles",
    ),
    (
        pytest.param(
            "umul r4, r4",
            ((4, 0),),
            0xBE,
            bytes((0xC2, 0x44)),
            4,
            0,
            0xBB,
            4,
            id="same-register-zero",
        ),
        pytest.param(
            "umul r17, r18",
            ((17, 0xFFFF_FFFF_FFFF_FFFF), (18, 2)),
            0xBB,
            bytes((0xF3, 0xC2, 0x12)),
            17,
            0xFFFF_FFFF_FFFF_FFFE,
            0xBE,
            5,
            id="rex-unsigned-wrap",
        ),
    ),
)
def test_shared_umul_matches_python_semantics_and_prefix_cycles(
    instruction: str,
    initial_registers: tuple[tuple[int, int], ...],
    initial_flags: int,
    expected_encoding: bytes,
    expected_register: int,
    expected_value: int,
    expected_flags: int,
    expected_cycles: int,
) -> None:
    encoding = bytes(assemble(instruction))
    assert encoding == expected_encoding

    observed = []
    for reference in (False, True):
        system = _system(reference=reference)
        system.load_binary(0, encoding)
        system.boot(entry=0)
        if reference:
            system.cores[1].halted = True
            system.cores[1].idle = False
        for reg, value in initial_registers:
            system.cpu.regs[reg] = value
        system.cpu.flags_unpack(initial_flags)
        system.cpu.perf_enable = 1

        stats = system.run_batch_stats(1)
        observed.append(
            (
                stats.instructions_executed,
                stats.system_cycles_advanced,
                system.cpu.pc,
                system.cpu.regs[expected_register],
                system.cpu.flags_pack(),
                system.cpu.cycle_count,
                system.cpu.perf_cycles,
                system.cpu.perf_stalls,
            )
        )

    python = PythonMegapad64(mem_size=4096, num_cores=1)
    python.load_bytes(0, encoding)
    python.pc = 0
    for reg, value in initial_registers:
        python.regs[reg] = value
    python.flags_unpack(initial_flags)
    python.perf_enable = 1
    python_cycles = python.run(max_steps=1)
    expected = (
        1,
        expected_cycles,
        len(encoding),
        expected_value,
        expected_flags,
        expected_cycles,
        expected_cycles,
        0,
    )

    assert observed == [expected, expected]
    assert (
        1,
        python_cycles,
        python.pc,
        python.regs[expected_register],
        python.flags_pack(),
        python.cycle_count,
        python.perf_cycles,
        python.perf_stalls,
    ) == expected


UMUL_PSEL_SOURCE = """
loop:
    umul r4, r3
    br loop
"""
UMUL_PSEL_SLICES = (1, 1, 2, 2)


def _initialize_umul_psel_workload(cpu) -> None:
    cpu.regs[2] = 4096
    cpu.regs[15] = 4096
    cpu.regs[4] = 3
    cpu.flags_unpack(0xBF)
    cpu.perf_enable = 1


def _run_umul_psel_workload(*, reference: bool) -> tuple:
    system = _system(reference=reference)
    system.load_binary(0, assemble(UMUL_PSEL_SOURCE))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    _initialize_umul_psel_workload(system.cpu)
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()

    batches = []
    for budget in UMUL_PSEL_SLICES:
        stats = system.run_batch_stats(budget)
        batches.append(
            (
                stats.instructions_executed,
                stats.system_cycles_advanced,
                stats.per_core_cycles[0],
                _core_signature(system),
            )
        )
    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    return (
        tuple(batches),
        _core_signature(system),
        _cpu_execution_signature(system.cpu),
        snapshot,
    )


def _run_python_umul_psel_workload() -> tuple:
    cpu = PythonMegapad64(mem_size=4096, num_cores=1)
    cpu.load_bytes(0, assemble(UMUL_PSEL_SOURCE))
    cpu.pc = 0
    _initialize_umul_psel_workload(cpu)
    for budget in UMUL_PSEL_SLICES:
        cpu.run(max_steps=budget)
    return _cpu_execution_signature(cpu)


def test_umul_psel_source_executes_natively_across_sliced_budgets() -> None:
    fast_batches, fast_core, fast_cpu, snapshot = (
        _run_umul_psel_workload(reference=False)
    )
    reference_batches, reference_core, reference_cpu, _ = (
        _run_umul_psel_workload(reference=True)
    )

    assert fast_batches == reference_batches
    assert fast_core == reference_core
    assert fast_cpu == reference_cpu == _run_python_umul_psel_workload()
    assert tuple(batch[1] for batch in fast_batches) == (4, 2, 6, 6)
    assert fast_cpu[0][4] == 24
    assert fast_cpu[1] == 0
    assert fast_cpu[5] == 0xBA
    assert fast_cpu[9] == 18
    assert fast_cpu[10] == 1
    assert fast_cpu[11] == 18
    assert fast_cpu[12] == 0

    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == sum(UMUL_PSEL_SLICES) == 6
    assert counts["uncontended_block_steps"] == 4
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 1
        assert counts["uncontended_jit_steps"] == 2


def test_umul_psel_destination_declines_exact_single_block() -> None:
    encoding = bytes(assemble("nop\numul r3, r4"))
    observed = []
    snapshot = None
    for reference in (False, True):
        system = _system(reference=reference)
        system.load_binary(0, encoding)
        system.boot(entry=0)
        if reference:
            system.cores[1].halted = True
            system.cores[1].idle = False
        prime = system.run_batch_stats(1)
        assert prime.instructions_executed == 1
        assert system.cpu.pc == 1
        system.cpu.regs[4] = 1
        system.cpu.flags_unpack(0xBF)
        system.cpu.perf_enable = 1
        owner = system._native_system
        if not reference:
            owner._start_concurrency_profile()

        stats = system.run_batch_stats(1)
        if not reference:
            snapshot = dict(owner._stop_concurrency_profile())
        observed.append(
            (
                stats.instructions_executed,
                stats.system_cycles_advanced,
                system.cpu.pc,
                system.cpu.regs[3],
                system.cpu.flags_pack(),
            )
        )

    python = PythonMegapad64(mem_size=4096, num_cores=1)
    python.load_bytes(0, encoding)
    python.pc = 0
    assert python.run(max_steps=1) == 1
    assert python.pc == 1
    python.regs[4] = 1
    python.flags_unpack(0xBF)
    python_observed = (
        1,
        python.run(max_steps=1),
        python.pc,
        python.regs[3],
        python.flags_pack(),
    )

    assert observed == [python_observed] * 2
    assert python_observed == (1, 4, 3, 3, 0xBA)
    assert snapshot is not None
    counts = dict(snapshot["counts"])
    assert counts["uncontended_block_zero_instruction_rejections"] == 1
    assert counts["uncontended_block_steps"] == 0
    assert counts["uncontended_jit_executions"] == 0
    assert counts["uncontended_jit_steps"] == 0


def _warmed_umul_loop_system(*, reference: bool) -> MegapadSystem:
    system = _system(reference=reference)
    system.load_binary(
        0,
        assemble(
            """
loop:
    umul r4, r5
    br loop
"""
        ),
    )
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.perf_enable = 1
    for _ in range(3):
        system.cpu.pc = 0
        system.cpu.regs[4] = 3
        system.cpu.regs[5] = 2
        system.cpu.flags_unpack(0xBF)
        warm = system.run_batch_stats(2)
        assert warm.instructions_executed == 2
        assert warm.system_cycles_advanced == 6
    return system


def test_native_umul_ipi_exit_publishes_four_cycle_prefix() -> None:
    system = _warmed_umul_loop_system(reference=False)
    owner = system._native_system
    owner._set_single_core_jit_regions_enabled_for_test(True)
    if (
        owner._concurrency_profile_snapshot()[
            "single_core_jit_backend"
        ] != "x86_64"
    ):
        pytest.skip("native completed-prefix oracle requires x86-64")

    system.cpu.pc = 0
    system.cpu.regs[4] = 0xFFFF_FFFF_FFFF_FFFF
    system.cpu.regs[5] = 2
    system.cpu.flags_unpack(0xBB)
    system.cpu.flag_i = True
    deliveries: list[tuple[int, tuple]] = []

    def observe_ipi(vector: int) -> None:
        deliveries.append((vector, _core_signature(system)))
        owner.set_ipi_line(0, False)
        system.cpu.flag_i = False
        system.cpu.halted = True

    system.cpu._trap = observe_ipi
    owner._start_concurrency_profile()
    owner._inject_uncontended_ipi_at_next_native_entry()

    stats = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    reference = _warmed_umul_loop_system(reference=True)
    reference.cpu.pc = 0
    reference.cpu.regs[4] = 0xFFFF_FFFF_FFFF_FFFF
    reference.cpu.regs[5] = 2
    reference.cpu.flags_unpack(0xBB)
    reference.cpu.flag_i = True
    reference_stats = reference.run_batch_stats(1)

    assert reference_stats.instructions_executed == 1
    assert reference_stats.system_cycles_advanced == 4
    assert deliveries == [(IVEC_IPI, _core_signature(reference))]
    assert stats.instructions_executed == 1
    assert stats.system_cycles_advanced == 4
    assert system.cpu.regs[4] == 0xFFFF_FFFF_FFFF_FFFE
    assert system.cpu.pc == 2
    assert counts["uncontended_steps"] == 1
    assert counts["uncontended_interrupt_boundaries"] == 1
    assert counts["uncontended_block_executions"] == 1
    assert counts["uncontended_block_steps"] == 1
    assert counts["uncontended_jit_executions"] == 1
    assert counts["uncontended_jit_steps"] == 1
    assert counts["uncontended_jit_native_entries"] == 1
    assert counts["uncontended_jit_native_returns"] == 1
    assert all(
        counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
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
    assert profile_counts["settle_round_calls"] == 3
    assert profile_counts["settle_round_native_calls"] == 1
    assert profile_counts["settle_round_python_calls"] == 2


def test_timer_match_inside_round_uses_exact_python_interrupt_boundary() -> None:
    system = _system()
    system.load_binary(0, assemble("\n".join(["nop"] * 2_001)))
    system.boot(entry=0)
    system.cpu.flag_i = True
    system.timer.counter = 0
    system.timer.compare = 1_500
    system.timer.control = 0x03
    deliveries: list[tuple[int, int, int]] = []

    def observe_trap(vector: int) -> None:
        deliveries.append(
            (
                vector,
                int(system._native_system.system_cycles),
                int(system.cpu.cycle_count),
            )
        )
        system.timer.irq_pending = False
        system.cpu.flag_i = False

    system.cpu._trap = observe_trap
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(2_001)
    counts = dict(owner._stop_concurrency_profile()["counts"])

    assert deliveries == [(IVEC_TIMER, 2_000, 2_000)]
    assert stats.instructions_executed == 2_001
    assert stats.native_rounds == 3
    assert stats.system_cycles_advanced == 2_001
    assert owner.system_cycles == 2_001
    assert system.timer.counter == 2_001
    assert counts["settle_round_calls"] == 4
    assert counts["settle_round_native_calls"] == 2
    assert counts["settle_round_python_calls"] == 2


def test_wrapped_clock_settlement_retains_the_python_round_boundary() -> None:
    system = _system()
    system.load_binary(0, assemble("\n".join(["nop"] * 1_001)))
    system.boot(entry=0)
    advances: list[int] = []
    canonical_advance = system._advance_system_cycles_locked

    def observed_advance(cycles: int) -> None:
        advances.append(cycles)
        canonical_advance(cycles)

    system._advance_system_cycles_locked = observed_advance
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1_001)
    counts = dict(owner._stop_concurrency_profile()["counts"])

    assert advances == [1_000, 1]
    assert stats.instructions_executed == 1_001
    assert stats.system_cycles_advanced == 1_001
    assert owner.system_cycles == 1_001
    assert counts["settle_round_calls"] == 3
    assert counts["settle_round_native_calls"] == 0
    assert counts["settle_round_python_calls"] == 3


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


def _run_natural_callback_success(
    operation: str,
    *,
    reference: bool,
) -> tuple:
    system = _system(reference=reference)
    system.load_binary(0, assemble(operation))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False

    events: list[tuple] = []
    value = 0x8877_6655_4433_2211
    if operation.startswith("ldn"):
        system.cpu.regs[4] = 0xDEAD_BEEF
        system.cpu.regs[5] = NATURAL_CALLBACK_BASE

        def read_byte(address: int) -> int:
            events.append(("read", address))
            offset = address - NATURAL_CALLBACK_BASE
            return (value >> (8 * offset)) & 0xFF

        system.cpu._mmio_read8 = read_byte
    else:
        system.cpu.regs[4] = NATURAL_CALLBACK_BASE
        system.cpu.regs[5] = value

        def write_byte(address: int, byte: int) -> None:
            events.append(("write", address, byte))

        system.cpu._mmio_write8 = write_byte

    stats = system.run_batch_stats(1)
    stats_signature = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions[0],
        stats.per_core_cycles[0],
    )
    return tuple(events), _core_signature(system), stats_signature


@pytest.mark.parametrize(
    "operation",
    (
        pytest.param("ldn r4, r5", id="load"),
        pytest.param("str r4, r5", id="store"),
    ),
)
def test_natural_width_callbacks_preserve_little_endian_byte_order(
    operation: str,
) -> None:
    fast = _run_natural_callback_success(operation, reference=False)
    reference = _run_natural_callback_success(operation, reference=True)

    assert fast == reference
    events, state, stats = fast
    value = 0x8877_6655_4433_2211
    if operation.startswith("ldn"):
        assert events == tuple(
            ("read", NATURAL_CALLBACK_BASE + offset)
            for offset in range(8)
        )
        assert state[0][4] == value
    else:
        assert events == tuple(
            (
                "write",
                NATURAL_CALLBACK_BASE + offset,
                (value >> (8 * offset)) & 0xFF,
            )
            for offset in range(8)
        )
    assert stats == (1, 1, 1, 1)


def _run_prefixed_natural_callback_failure(*, reference: bool) -> tuple:
    system = _system(reference=reference)
    system.load_binary(0, assemble("ldn r16, r17"))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.regs[16] = 0xDEAD_BEEF_CAFE_BABE
    system.cpu.regs[17] = NATURAL_CALLBACK_BASE
    failure = _CallbackFailure("prefixed natural-width read")
    addresses: list[int] = []

    def fail_later_byte(address: int) -> int:
        addresses.append(address)
        if address == NATURAL_CALLBACK_BASE + 3:
            raise failure
        return address - NATURAL_CALLBACK_BASE

    system.cpu._mmio_read8 = fail_later_byte
    with pytest.raises(_CallbackFailure) as raised:
        system.run_batch_stats(1)
    assert raised.value is failure
    return tuple(addresses), _core_signature(system)


def test_prefixed_natural_read_failure_stops_at_exact_callback_byte() -> None:
    fast = _run_prefixed_natural_callback_failure(reference=False)
    reference = _run_prefixed_natural_callback_failure(reference=True)

    assert fast == reference
    addresses, state = fast
    assert addresses == tuple(
        NATURAL_CALLBACK_BASE + offset for offset in range(4)
    )
    assert state[0][16] == 0xDEAD_BEEF_CAFE_BABE
    assert state[0][3] == 3
    assert state[9] == 0
    assert state[16] == 0
    assert state[17] == 0


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
    system.timer.control = 1
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(2_503)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])
    wall_ns = dict(snapshot["wall_ns"])

    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_rounds"] == stats.native_rounds == 3
    assert counts["uncontended_dispatches"] == sum(
        stats.per_core_dispatches
    )
    assert counts["uncontended_steps"] == stats.instructions_executed
    assert counts["uncontended_continuations"] == 0
    assert counts["uncontended_callback_errors"] == 0
    assert counts["settle_round_calls"] == 4
    assert counts["settle_round_native_calls"] == 3
    assert counts["settle_round_python_calls"] == 1
    assert system.cpu.regs[4] == 1_252
    assert system.cpu.pc == 1
    assert system.cpu.cycle_count == 3_754
    assert stats.system_cycles_advanced == 3_754
    assert owner.system_cycles == 3_754
    assert system.timer.counter == 3_754
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
        assert storage["slot_count"] == 4_096
        assert storage["slot_bytes"] == 1_344
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


def test_exact_rejection_cache_skips_repeated_one_instruction_builds() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(8)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 8
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 8
    assert counts["uncontended_block_hits"] == 0
    assert counts["uncontended_block_misses"] == 8
    assert counts["uncontended_block_build_attempts"] == 2
    assert counts["uncontended_block_builds"] == 0
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_zero_instruction_rejections"] == 0
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 6
    assert counts["uncontended_block_rejection_cache_stores"] == 1
    assert counts["uncontended_block_rejection_cache_replacements"] == 0
    assert counts["uncontended_block_executions"] == 0
    assert counts["uncontended_block_steps"] == 0


def _run_self_modifying_rejection_cache(
    *,
    reference: bool,
) -> tuple[tuple, dict]:
    system = _system(reference=reference)
    program = assemble(
        """
loop:
    st.b r5, r6
    br loop
"""
    )
    replacement_opcode = assemble("add r5, r6")[0]
    assert program[0] != replacement_opcode
    assert program[1] == assemble("add r5, r6")[1]
    scratch = 0x80
    system.load_binary(0, program)
    system.load_binary(scratch, b"\xEE")
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.regs[5] = scratch
    system.cpu.regs[6] = replacement_opcode

    warm = system.run_batch_stats(4)
    assert warm.instructions_executed == 4
    assert system.cpu.mem[0] == program[0]
    assert system.cpu.mem[scratch] == replacement_opcode

    system.cpu.pc = 0
    system.cpu.regs[5] = 0
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    changed = system.run_batch_stats(6)
    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )

    assert changed.instructions_executed == 6
    assert system.cpu.mem[0] == replacement_opcode
    assert system.cpu.regs[5] == 2 * replacement_opcode
    assert system.cpu.pc == 0
    return _core_signature(system), snapshot


def test_exact_rejection_cache_reproves_self_modified_identity() -> None:
    fast, snapshot = _run_self_modifying_rejection_cache(reference=False)
    reference, _ = _run_self_modifying_rejection_cache(reference=True)

    assert fast == reference
    _assert_block_cache_profile_reconciles(snapshot)
    counts = dict(snapshot["counts"])
    assert counts["uncontended_block_lookups"] == 4
    assert counts["uncontended_block_rejection_cache_hits"] == 1
    assert counts["uncontended_block_build_attempts"] == 2
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_one_instruction_rejections"] == 0
    assert counts["uncontended_block_rejection_cache_stores"] == 0
    assert counts["uncontended_block_rejection_cache_replacements"] == 0
    assert counts["uncontended_block_executions"] == 2
    assert counts["uncontended_block_steps"] == 4


def test_exact_rejection_cache_classifies_zero_instruction_starts() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    ei
    br loop
"""
        ),
    )
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(8)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 8
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 8
    assert counts["uncontended_block_misses"] == 8
    assert counts["uncontended_block_build_attempts"] == 3
    assert counts["uncontended_block_builds"] == 0
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_zero_instruction_rejections"] == 1
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 5
    assert counts["uncontended_block_rejection_cache_stores"] == 2
    assert counts["uncontended_block_rejection_cache_replacements"] == 0


def test_exact_rejection_survives_same_byte_icache_refill() -> None:
    system = _system()
    program = assemble(
        """
loop:
    br loop
"""
    )
    system.load_binary(0, program)
    system.boot(entry=0)

    warm = system.run_batch_stats(4)
    assert warm.instructions_executed == 4
    system.cpu.mem_write8(0, program[0])
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(4)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 4
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 4
    assert counts["uncontended_block_misses"] == 4
    assert counts["uncontended_block_build_attempts"] == 1
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 3
    assert counts["uncontended_block_rejection_cache_stores"] == 0
    assert counts["uncontended_block_rejection_cache_replacements"] == 0


@pytest.mark.parametrize(
    (
        "boundary",
        "expected_attempts",
        "expected_nonresident",
        "expected_hits",
    ),
    (
        pytest.param("boot", 2, 1, 2, id="boot"),
        pytest.param("icache-restore", 1, 0, 3, id="icache-restore"),
    ),
)
def test_hard_plan_boundaries_clear_exact_rejections(
    boundary: str,
    expected_attempts: int,
    expected_nonresident: int,
    expected_hits: int,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    br loop
"""
        ),
    )
    system.boot(entry=0)

    warm = system.run_batch_stats(4)
    assert warm.instructions_executed == 4
    hot_icache = system.cpu._cs.icache_snapshot()
    if boundary == "boot":
        system.boot(entry=0)
    else:
        system.cpu._cs.icache_restore(*hot_icache)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(4)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 4
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 4
    assert counts["uncontended_block_misses"] == 4
    assert counts["uncontended_block_build_attempts"] == expected_attempts
    assert (
        counts["uncontended_block_nonresident_rejections"]
        == expected_nonresident
    )
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert (
        counts["uncontended_block_rejection_cache_hits"]
        == expected_hits
    )
    assert counts["uncontended_block_rejection_cache_stores"] == 1
    assert counts["uncontended_block_rejection_cache_replacements"] == 0


@pytest.mark.parametrize("selector", ("psel", "spsel"))
def test_exact_rejection_identity_includes_active_selectors(
    selector: str,
) -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    br loop
"""
        ),
    )
    system.boot(entry=0)

    warm = system.run_batch_stats(4)
    assert warm.instructions_executed == 4
    if selector == "psel":
        system.cpu.psel = 4
        system.cpu.pc = 0
    else:
        system.cpu.spsel = 14
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 1
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 1
    assert counts["uncontended_block_misses"] == 1
    assert counts["uncontended_block_build_attempts"] == 1
    assert counts["uncontended_block_nonresident_rejections"] == 0
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 0
    assert counts["uncontended_block_rejection_cache_stores"] == 1
    assert counts["uncontended_block_rejection_cache_replacements"] == 0


@pytest.mark.parametrize("selector", ("psel", "spsel"))
def test_decoded_block_identity_includes_active_selectors(
    selector: str,
) -> None:
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

    warm = system.run_batch_stats(6)
    assert warm.instructions_executed == 6
    if selector == "psel":
        system.cpu.psel = 2
        system.cpu.pc = 0
    else:
        system.cpu.spsel = 14
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(4)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 4
    assert system.cpu.pc == 0
    assert system.cpu.regs[4] == 5
    assert counts["uncontended_block_lookups"] == 2
    assert counts["uncontended_block_hits"] == 1
    assert counts["uncontended_block_misses"] == 1
    assert counts["uncontended_block_build_attempts"] == 1
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_evictions"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 0
        assert counts["uncontended_jit_slot_publications"] == 1
        assert counts["uncontended_jit_slot_rewrites"] == 0
        assert counts["uncontended_jit_executions"] == 1
        assert counts["uncontended_jit_steps"] == 2
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_changed_bytes_replace_exact_rejection_with_decoded_block() -> None:
    system = _system()
    rejected = assemble(
        """
loop:
    br loop
"""
    )
    replacement = assemble(
        """
loop:
    inc r4
    br loop
"""
    )
    system.load_binary(0, rejected)
    system.boot(entry=0)

    warm = system.run_batch_stats(4)
    assert warm.instructions_executed == 4
    for offset, value in enumerate(replacement):
        system.cpu.mem_write8(offset, value)
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(6)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert stats.instructions_executed == 6
    assert system.cpu.regs[4] == 3
    assert system.cpu.pc == 0
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 4
    assert counts["uncontended_block_hits"] == 1
    assert counts["uncontended_block_misses"] == 3
    assert counts["uncontended_block_build_attempts"] == 3
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_zero_instruction_rejections"] == 0
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 0
    assert counts["uncontended_block_rejection_cache_stores"] == 1
    assert counts["uncontended_block_rejection_cache_replacements"] == 0
    assert counts["uncontended_block_executions"] == 2
    assert counts["uncontended_block_steps"] == 4


def test_set_associative_rejection_cache_retains_two_colliders() -> None:
    system = _system()
    first_address = 0
    colliding_address = 0x204
    one_instruction_loop = assemble(
        """
loop:
    br loop
"""
    )
    system.load_binary(first_address, one_instruction_loop)
    system.load_binary(colliding_address, one_instruction_loop)
    system.boot(entry=first_address)
    owner = system._native_system
    owner._start_concurrency_profile()

    for address in (
        first_address,
        colliding_address,
        first_address,
    ):
        system.cpu.pc = address
        stats = system.run_batch_stats(2)
        assert stats.instructions_executed == 2

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.pc == first_address
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_block_lookups"] == 6
    assert counts["uncontended_block_misses"] == 6
    assert counts["uncontended_block_build_attempts"] == 4
    assert counts["uncontended_block_builds"] == 0
    assert counts["uncontended_block_nonresident_rejections"] == 2
    assert counts["uncontended_block_one_instruction_rejections"] == 2
    assert counts["uncontended_block_rejection_cache_hits"] == 2
    assert counts["uncontended_block_rejection_cache_stores"] == 2
    assert counts["uncontended_block_rejection_cache_replacements"] == 0


def test_host_profile_attributes_set_associative_translation_coexistence() -> None:
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
    assert counts["uncontended_block_builds"] == 2
    assert counts["uncontended_block_evictions"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 2
        assert counts["uncontended_jit_compilations"] == 2
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == 2
        assert counts["uncontended_jit_slot_rewrites"] == 0
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


def test_expanded_set_geometry_retains_a_prior_128_entry_collision() -> None:
    system = _system()
    first_address = 0
    prior_128_collision = 0x81
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
        prior_128_collision,
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

    for address in (
        first_address,
        prior_128_collision,
        first_address,
    ):
        system.cpu.pc = address
        stats = system.run_batch_stats(6)
        assert stats.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 6
    assert system.cpu.regs[5] == 3
    assert counts["uncontended_block_builds"] == 2
    assert counts["uncontended_block_evictions"] == 0
    assert counts["uncontended_jit_plan_evictions"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["slot_count"] == 4_096


def test_reusable_jit_arena_bounds_five_way_set_churn() -> None:
    system = MegapadSystem(
        ram_size=1 << 16,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    colliding_addresses = (0, 0x810, 0x1020, 0x1830, 0x2040)
    assert {
        (address ^ (address >> 7)) & 1_023
        for address in colliding_addresses
    } == {0}
    destination_registers = tuple(range(4, 9))
    for address, register in zip(
        colliding_addresses,
        destination_registers,
        strict=True,
    ):
        system.load_binary(
            address,
            assemble(
                f"""
loop:
    inc r{register}
    br loop
"""
            ),
        )
    system.boot(entry=colliding_addresses[0])
    owner = system._native_system
    owner._start_concurrency_profile()
    visit_count = 40

    for visit in range(visit_count):
        system.cpu.pc = colliding_addresses[
            visit % len(colliding_addresses)
        ]
        stats = system.run_batch_stats(6)
        assert stats.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    expected_register_value = 3 * (
        visit_count // len(colliding_addresses)
    )
    assert all(
        system.cpu.regs[register] == expected_register_value
        for register in destination_registers
    )
    assert counts["uncontended_block_builds"] == visit_count
    expected_replacements = visit_count - 4
    assert counts["uncontended_block_evictions"] == expected_replacements
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == visit_count
        assert counts["uncontended_jit_compilations"] == visit_count
        assert counts["uncontended_jit_compile_failures"] == 0
        assert (
            counts["uncontended_jit_plan_evictions"]
            == expected_replacements
        )
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == visit_count
        assert (
            counts["uncontended_jit_slot_rewrites"]
            == expected_replacements
        )
        assert counts["uncontended_jit_code_bytes"] > 0
        assert counts["uncontended_jit_max_code_bytes"] > 0
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["ready"]
        assert not storage["failed"]
        assert storage["slot_count"] == 4_096
        assert storage["mapped_bytes_per_alias"] == (
            storage["slot_count"] * storage["slot_bytes"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_dense_jit_slot_accepts_maximal_register_cmp_line() -> None:
    system = _system()
    maximal_line = assemble("cmp r4, r5\n" * 8)
    assert len(maximal_line) == 16
    system.load_binary(0, maximal_line)
    system.boot(entry=0)
    owner = system._native_system
    owner._start_concurrency_profile()

    for _ in range(3):
        system.cpu.pc = 0
        stats = system.run_batch_stats(8)
        assert stats.instructions_executed == 8
        assert stats.system_cycles_advanced == 8

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.pc == 16
    assert system.cpu.cycle_count == 24
    if snapshot["single_core_jit_backend"] == "x86_64":
        storage = dict(snapshot["single_core_jit_storage"])
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        # Eight resident PC advances use R9, while the entry ABI keeps the
        # core and IPI pointers in RDI/RSI instead of copying them into a
        # larger callee-saved frame.
        assert counts["uncontended_jit_max_code_bytes"] == 834
        assert storage["slot_bytes"] == 1_344
        assert counts["uncontended_jit_max_code_bytes"] < (
            storage["slot_bytes"]
        )
    else:
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def test_dense_neighbor_slot_survives_shorter_rewrite() -> None:
    system = MegapadSystem(
        ram_size=1 << 16,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    first_address = 0x810
    neighbor_address = 0x4080
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
    nop
    br loop
"""
    )
    neighbor = assemble(
        """
loop:
    inc r5
    br loop
"""
    )
    assert len(original) == len(replacement) == len(neighbor)
    system.load_binary(first_address, original)
    system.load_binary(neighbor_address, neighbor)
    system.boot(entry=first_address)
    owner = system._native_system
    owner._start_concurrency_profile()

    for address in (first_address, neighbor_address):
        system.cpu.pc = address
        stats = system.run_batch_stats(6)
        assert stats.instructions_executed == 6
    system.cpu.mem_write8(first_address, replacement[0])
    system.cpu.pc = first_address
    rewritten = system.run_batch_stats(6)
    assert rewritten.instructions_executed == 6
    system.cpu.pc = neighbor_address
    retained = system.run_batch_stats(6)
    assert retained.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[4] == 3
    assert system.cpu.regs[5] == 6
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compilations"] == 3
        assert counts["uncontended_jit_slot_publications"] == 3
        assert counts["uncontended_jit_slot_rewrites"] == 1
        storage = dict(snapshot["single_core_jit_storage"])
        assert storage["slot_bytes"] == 1_344
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
    # The retained self-loop region executes two logical blocks through one
    # host admission after the refill. The refill still rebuilds no decoded or
    # native plan; it only changes the physical-entry landmark.
    assert counts["uncontended_block_lookups"] == 7
    assert counts["uncontended_block_misses"] == 5
    assert counts["uncontended_block_hits"] == 2
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
        assert counts["uncontended_jit_native_entries"] == 2
        assert counts["uncontended_jit_native_returns"] == 2
        assert counts["uncontended_jit_region_compile_attempts"] == 1
        assert counts["uncontended_jit_region_compilations"] == 1
        assert counts["uncontended_jit_region_compile_failures"] == 0
        assert counts["uncontended_jit_region_entries"] == 1
        assert counts["uncontended_jit_region_blocks"] == 2
        assert counts["uncontended_jit_region_steps"] == 4
        assert (
            counts["uncontended_jit_region_target_identity_misses"]
            == 0
        )
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
    target_descriptor = 0x300
    value_descriptor = 0x310
    scratch_target = 0x380
    writer = assemble(
        """
    ldn r5, r10
    ldn r6, r11
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
    system.load_binary(
        target_descriptor,
        scratch_target.to_bytes(8, "little"),
    )
    system.load_binary(
        value_descriptor,
        original[0].to_bytes(8, "little"),
    )
    system.load_binary(scratch_target, b"\xEE")
    system.boot(entry=victim_address)

    victim_warm = system.run_batch_stats(6)
    assert victim_warm.instructions_executed == 6
    for _ in range(3):
        system.cpu.pc = 0
        system.cpu.regs[10] = target_descriptor
        system.cpu.regs[11] = value_descriptor
        writer_warm = system.run_batch_stats(3)
        assert writer_warm.instructions_executed == 3
    system.cpu.pc = victim_address
    victim_refill = system.run_batch_stats(6)
    assert victim_refill.instructions_executed == 6
    original_executions = system.cpu.regs[7]
    assert original_executions == 6

    system.load_binary(
        target_descriptor,
        victim_address.to_bytes(8, "little"),
    )
    system.load_binary(
        value_descriptor,
        replacement[0].to_bytes(8, "little"),
    )
    owner = system._native_system
    owner._start_concurrency_profile()
    system.cpu.pc = 0
    system.cpu.regs[5] = 0
    system.cpu.regs[6] = 0
    system.cpu.regs[10] = target_descriptor
    system.cpu.regs[11] = value_descriptor
    native_write = system.run_batch_stats(3)
    assert native_write.instructions_executed == 3
    system.cpu.pc = victim_address
    changed = system.run_batch_stats(6)
    assert changed.instructions_executed == 6

    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    assert system.cpu.regs[7] == original_executions
    assert system.cpu.regs[8] == 3
    assert system.cpu.regs[5] == victim_address
    assert system.cpu.regs[6] == replacement[0]
    assert system.cpu.mem[victim_address] == replacement[0]
    assert system.cpu.mem[scratch_target] == original[0]
    assert counts["uncontended_steps"] == 9
    assert counts["uncontended_block_lookups"] == 5
    assert counts["uncontended_block_misses"] == 3
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_evictions"] == 1
    assert counts["uncontended_block_executions"] == 3
    assert counts["uncontended_block_steps"] == 7
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 1
        assert counts["uncontended_jit_arena_allocations"] == 0
        assert counts["uncontended_jit_slot_publications"] == 1
        assert counts["uncontended_jit_slot_rewrites"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 5
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


def _warmed_two_block_region_ring(
    *,
    reference: bool,
    regions_enabled: bool = True,
    source: str = TWO_BLOCK_REGION_RING_SOURCE,
    warm_flags: int | None = None,
) -> MegapadSystem:
    system = _system(reference=reference)
    system.load_binary(0, assemble(source))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    else:
        system._native_system._set_single_core_jit_regions_enabled_for_test(
            regions_enabled
        )
    system.cpu.perf_enable = 1
    if warm_flags is not None:
        system.cpu.flags_unpack(warm_flags)

    warm = system.run_batch_stats(16)
    assert warm.instructions_executed == 16
    assert system.cpu.pc == 0
    return system


def _architectural_batch_signature(stats) -> tuple:
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions[0],
        stats.per_core_cycles[0],
        stats.per_core_stop_reasons[0],
        stats.system_stop_reason,
    )


def _assert_one_two_block_region_dispatch(
    enabled_snapshot: dict,
    disabled_snapshot: dict,
) -> None:
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])
    if enabled_snapshot["single_core_jit_backend"] != "x86_64":
        assert all(
            enabled_counts[name] == disabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )
        return

    assert enabled_counts["uncontended_jit_executions"] == 2
    assert enabled_counts["uncontended_jit_steps"] == 4
    assert enabled_counts["uncontended_jit_native_entries"] == 1
    assert enabled_counts["uncontended_jit_native_returns"] == 1
    assert enabled_counts["uncontended_jit_region_compile_attempts"] == 0
    assert enabled_counts["uncontended_jit_region_compilations"] == 0
    assert enabled_counts["uncontended_jit_region_compile_failures"] == 0
    assert enabled_counts["uncontended_jit_region_entries"] == 1
    assert enabled_counts["uncontended_jit_region_blocks"] == 2
    assert enabled_counts["uncontended_jit_region_steps"] == 4
    assert (
        enabled_counts["uncontended_jit_region_target_identity_misses"]
        == 0
    )
    assert disabled_counts["uncontended_jit_executions"] == 2
    assert disabled_counts["uncontended_jit_steps"] == 4
    assert disabled_counts["uncontended_jit_native_entries"] == 2
    assert disabled_counts["uncontended_jit_native_returns"] == 2
    assert all(
        disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )


def test_warm_two_block_region_halves_native_returns_exactly() -> None:
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
    )
    reference = _warmed_two_block_region_ring(reference=True)
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(1_000)
    disabled_stats = disabled.run_batch_stats(1_000)
    reference_stats = reference.run_batch_stats(1_000)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == enabled.cpu.regs[5] == 254
    if enabled_snapshot["single_core_jit_backend"] != "x86_64":
        assert all(
            enabled_counts[name] == disabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )
        return

    assert enabled_counts["uncontended_jit_executions"] == 500
    assert enabled_counts["uncontended_jit_steps"] == 1_000
    assert enabled_counts["uncontended_jit_native_entries"] == 250
    assert enabled_counts["uncontended_jit_native_returns"] == 250
    assert enabled_counts["uncontended_jit_region_compile_attempts"] == 0
    assert enabled_counts["uncontended_jit_region_compilations"] == 0
    assert enabled_counts["uncontended_jit_region_compile_failures"] == 0
    assert enabled_counts["uncontended_jit_region_entries"] == 250
    assert enabled_counts["uncontended_jit_region_blocks"] == 500
    assert enabled_counts["uncontended_jit_region_steps"] == 1_000
    assert (
        enabled_counts["uncontended_jit_region_target_identity_misses"]
        == 0
    )

    assert disabled_counts["uncontended_jit_executions"] == 500
    assert disabled_counts["uncontended_jit_steps"] == 1_000
    assert disabled_counts["uncontended_jit_native_entries"] == 500
    assert disabled_counts["uncontended_jit_native_returns"] == 500
    assert all(
        disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )


@pytest.mark.parametrize(
    ("flags", "expected_pc", "expected_cycles"),
    (
        pytest.param(0xAB, 0, 6, id="taken"),
        pytest.param(0xAA, 6, 5, id="not-taken"),
    ),
)
def test_two_block_region_settles_target_conditional_exactly(
    flags: int,
    expected_pc: int,
    expected_cycles: int,
) -> None:
    program = assemble(CONDITIONAL_TARGET_REGION_SOURCE)
    assert len(program) == 6
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
        source=CONDITIONAL_TARGET_REGION_SOURCE,
        warm_flags=0xAB,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
        source=CONDITIONAL_TARGET_REGION_SOURCE,
        warm_flags=0xAB,
    )
    reference = _warmed_two_block_region_ring(
        reference=True,
        source=CONDITIONAL_TARGET_REGION_SOURCE,
        warm_flags=0xAB,
    )
    for system in (enabled, disabled, reference):
        system.cpu.pc = 0
        system.cpu.regs[4] = 0x1234
        system.cpu.regs[5] = 0x5678
        system.cpu.flags_unpack(flags)
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(4)
    disabled_stats = disabled.run_batch_stats(4)
    reference_stats = reference.run_batch_stats(4)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert enabled_stats.system_cycles_advanced == expected_cycles
    assert enabled_stats.per_core_cycles[0] == expected_cycles
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == 0x1235
    assert enabled.cpu.regs[5] == 0x5679
    assert enabled.cpu.pc == expected_pc
    assert enabled.cpu.flags_pack() == flags
    _assert_one_two_block_region_dispatch(
        enabled_snapshot,
        disabled_snapshot,
    )


def test_two_block_region_preserves_memory_pc_for_psel_operand() -> None:
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
        source=PSEL_OPERAND_REGION_SOURCE,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
        source=PSEL_OPERAND_REGION_SOURCE,
    )
    reference = _warmed_two_block_region_ring(
        reference=True,
        source=PSEL_OPERAND_REGION_SOURCE,
    )
    for system in (enabled, disabled, reference):
        system.cpu.pc = 0
        system.cpu.regs[4] = 3
        system.cpu.regs[5] = 7
        system.cpu.flags_unpack(0xBF)
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(4)
    disabled_stats = disabled.run_batch_stats(4)
    reference_stats = reference.run_batch_stats(4)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert enabled_stats.system_cycles_advanced == 9
    assert enabled_stats.per_core_cycles[0] == 9
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == 6
    assert enabled.cpu.regs[5] == 8
    assert enabled.cpu.pc == 0
    assert enabled.cpu.psel == 3
    assert enabled.cpu.flags_pack() == 0xBA
    _assert_one_two_block_region_dispatch(
        enabled_snapshot,
        disabled_snapshot,
    )


@pytest.mark.parametrize(
    ("budget", "expected_jit_executions", "expected_jit_steps"),
    (
        pytest.param(1, 0, 0, id="one-step"),
        pytest.param(2, 1, 2, id="one-block"),
        pytest.param(3, 1, 2, id="one-block-plus-prefix"),
    ),
)
def test_two_block_region_declines_budgets_below_combined_size_exactly(
    budget: int,
    expected_jit_executions: int,
    expected_jit_steps: int,
) -> None:
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
    )
    reference = _warmed_two_block_region_ring(reference=True)
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(budget)
    disabled_stats = disabled.run_batch_stats(budget)
    reference_stats = reference.run_batch_stats(budget)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    if enabled_snapshot["single_core_jit_backend"] != "x86_64":
        assert all(
            enabled_counts[name] == disabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )
        return

    for counts in (enabled_counts, disabled_counts):
        assert counts["uncontended_jit_executions"] == (
            expected_jit_executions
        )
        assert counts["uncontended_jit_steps"] == expected_jit_steps
        assert counts["uncontended_jit_native_entries"] == (
            expected_jit_executions
        )
        assert counts["uncontended_jit_native_returns"] == (
            expected_jit_executions
        )
        assert all(
            counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )


def test_two_block_region_rechecks_changed_target_identity_exactly() -> None:
    source = """
first:
    inc r4
    lbr second

    .org 0x20
second:
    inc r5
    lbr first
"""
    original = assemble(source)
    replacement = assemble("dec r5")
    assert len(replacement) == 1
    assert original[0x20] != replacement[0]
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
        source=source,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
        source=source,
    )
    reference = _warmed_two_block_region_ring(
        reference=True,
        source=source,
    )
    for system in (enabled, disabled, reference):
        system.cpu.mem_write8(0x20, replacement[0])
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(4)
    disabled_stats = disabled.run_batch_stats(4)
    reference_stats = reference.run_batch_stats(4)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == 5
    assert enabled.cpu.regs[5] == 3
    assert enabled.cpu.pc == 0
    assert all(
        disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_snapshot["single_core_jit_backend"] == "x86_64":
        assert (
            enabled_counts[
                "uncontended_jit_region_target_identity_misses"
            ]
            == 1
        )
        assert enabled_counts["uncontended_jit_region_entries"] == 0
        assert enabled_counts["uncontended_jit_region_blocks"] == 0
        assert enabled_counts["uncontended_jit_region_steps"] == 0
        assert enabled_counts["uncontended_jit_native_entries"] == (
            enabled_counts["uncontended_jit_native_returns"]
        )
    else:
        assert all(
            enabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )


def test_two_block_region_reconsiders_identity_bound_negative_target() -> None:
    ineligible_source = """
source:
    inc r4
    br target

    .org 0x20
target:
    ld.b r5, r6
    br source
"""
    eligible_source = """
source:
    inc r4
    br target

    .org 0x20
target:
    mov r5, r7
    br source
"""
    ineligible_program = assemble(ineligible_source)
    eligible_program = assemble(eligible_source)
    target_address = 0x20
    assert ineligible_program[:target_address] == (
        eligible_program[:target_address]
    )
    assert len(ineligible_program) == len(eligible_program) == 0x24
    assert (
        len(ineligible_program[target_address:])
        == len(eligible_program[target_address:])
        == 4
    )
    assert ineligible_program[target_address:] != (
        eligible_program[target_address:]
    )

    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
        source=ineligible_source,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
        source=ineligible_source,
    )
    reference = _warmed_two_block_region_ring(
        reference=True,
        source=ineligible_source,
    )
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system

    # The memory target has its own native plan, but it is not an admissible
    # region constituent. Once that exact target identity has been rejected,
    # unchanged visits must not repeat a negative region compilation.
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()
    enabled_negative = enabled.run_batch_stats(16)
    disabled_negative = disabled.run_batch_stats(16)
    reference_negative = reference.run_batch_stats(16)
    enabled_negative_snapshot = dict(
        enabled_owner._stop_concurrency_profile()
    )
    disabled_negative_snapshot = dict(
        disabled_owner._stop_concurrency_profile()
    )
    enabled_negative_counts = dict(
        enabled_negative_snapshot["counts"]
    )
    disabled_negative_counts = dict(
        disabled_negative_snapshot["counts"]
    )

    assert (
        _architectural_batch_signature(enabled_negative)
        == _architectural_batch_signature(disabled_negative)
        == _architectural_batch_signature(reference_negative)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert all(
        enabled_negative_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    assert all(
        disabled_negative_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_negative_snapshot["single_core_jit_backend"] == "x86_64":
        for counts in (
            enabled_negative_counts,
            disabled_negative_counts,
        ):
            assert counts["uncontended_jit_executions"] == 8
            assert counts["uncontended_jit_steps"] == 16
            assert counts["uncontended_jit_native_entries"] == 8
            assert counts["uncontended_jit_native_returns"] == 8

    replacement = eligible_program[target_address:]
    for system in (enabled, disabled, reference):
        for offset, byte in enumerate(replacement):
            system.cpu.mem_write8(target_address + offset, byte)
        system.cpu.regs[7] = 0xA5

    # Only the target bytes changed. Its ordinary execution-plan refill and
    # native compilation happen naturally in this batch. The unchanged source
    # must invalidate the negative result bound to the old target identity,
    # reconsider the new eligible target, and eventually enter the new region.
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()
    enabled_stats = enabled.run_batch_stats(32)
    disabled_stats = disabled.run_batch_stats(32)
    reference_stats = reference.run_batch_stats(32)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == 16
    assert enabled.cpu.regs[5] == 0xA5
    assert enabled.cpu.pc == 0
    assert all(
        disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_snapshot["single_core_jit_backend"] == "x86_64":
        # Once the target becomes eligible, both forced directions publish;
        # this bounded batch reaches one of the two completed pairs.
        assert enabled_counts["uncontended_jit_region_compile_attempts"] == 2
        assert enabled_counts["uncontended_jit_region_compilations"] == 2
        assert enabled_counts["uncontended_jit_region_compile_failures"] == 0
        assert enabled_counts["uncontended_jit_region_entries"] > 0
        assert enabled_counts["uncontended_jit_region_blocks"] == (
            2 * enabled_counts["uncontended_jit_region_entries"]
        )
        assert enabled_counts["uncontended_jit_region_steps"] == (
            4 * enabled_counts["uncontended_jit_region_entries"]
        )
        assert (
            enabled_counts[
                "uncontended_jit_region_target_identity_misses"
            ]
            == 0
        )
    else:
        assert all(
            enabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )


def test_two_block_region_reconsiders_one_instruction_rejection() -> None:
    rejected_source = """
source:
    inc r4
    nop
    br target

    .org 0x20
target:
    lbr source
"""
    eligible_source = """
source:
    inc r4
    nop
    br target

    .org 0x20
target:
    inc r5
    br source
"""
    rejected_program = assemble(rejected_source)
    eligible_program = assemble(eligible_source)
    target_address = 0x20
    assert rejected_program[:target_address] == (
        eligible_program[:target_address]
    )
    assert len(rejected_program) == len(eligible_program) == 0x23
    assert (
        len(rejected_program[target_address:])
        == len(eligible_program[target_address:])
        == 3
    )
    assert rejected_program[target_address:] != (
        eligible_program[target_address:]
    )

    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
        source=rejected_source,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
        source=rejected_source,
    )
    reference = _warmed_two_block_region_ring(
        reference=True,
        source=rejected_source,
    )
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system

    # The exact one-instruction target lives in the rejection cache. The
    # source-owned negative pair may suppress repeated target probes, but it
    # must remain bound to these three target bytes rather than to the source.
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()
    enabled_negative = enabled.run_batch_stats(16)
    disabled_negative = disabled.run_batch_stats(16)
    reference_negative = reference.run_batch_stats(16)
    enabled_negative_snapshot = dict(
        enabled_owner._stop_concurrency_profile()
    )
    disabled_negative_snapshot = dict(
        disabled_owner._stop_concurrency_profile()
    )
    enabled_negative_counts = dict(
        enabled_negative_snapshot["counts"]
    )
    disabled_negative_counts = dict(
        disabled_negative_snapshot["counts"]
    )

    assert (
        _architectural_batch_signature(enabled_negative)
        == _architectural_batch_signature(disabled_negative)
        == _architectural_batch_signature(reference_negative)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert all(
        enabled_negative_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    assert all(
        disabled_negative_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_negative_snapshot["single_core_jit_backend"] == "x86_64":
        for counts in (
            enabled_negative_counts,
            disabled_negative_counts,
        ):
            assert counts["uncontended_jit_executions"] == 4
            assert counts["uncontended_jit_steps"] == 12
            assert counts["uncontended_jit_native_entries"] == 4
            assert counts["uncontended_jit_native_returns"] == 4

    replacement = eligible_program[target_address:]
    for system in (enabled, disabled, reference):
        for offset, byte in enumerate(replacement):
            system.cpu.mem_write8(target_address + offset, byte)

    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()
    enabled_stats = enabled.run_batch_stats(40)
    disabled_stats = disabled.run_batch_stats(40)
    reference_stats = reference.run_batch_stats(40)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert enabled.cpu.regs[4] == 16
    assert enabled.cpu.regs[5] == 8
    assert enabled.cpu.pc == 0
    assert all(
        disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_snapshot["single_core_jit_backend"] == "x86_64":
        # Once the target becomes eligible, both forced directions publish;
        # this bounded batch reaches one of the two completed pairs.
        assert enabled_counts["uncontended_jit_region_compile_attempts"] == 2
        assert enabled_counts["uncontended_jit_region_compilations"] == 2
        assert enabled_counts["uncontended_jit_region_compile_failures"] == 0
        assert enabled_counts["uncontended_jit_region_entries"] > 0
        assert enabled_counts["uncontended_jit_region_blocks"] == (
            2 * enabled_counts["uncontended_jit_region_entries"]
        )
        assert enabled_counts["uncontended_jit_region_steps"] == (
            5 * enabled_counts["uncontended_jit_region_entries"]
        )
        assert (
            enabled_counts[
                "uncontended_jit_region_target_identity_misses"
            ]
            == 0
        )
    else:
        assert all(
            enabled_counts[name] == 0
            for name in JIT_REGION_PROFILE_COUNT_FIELDS
        )


def test_two_block_region_declines_when_interrupts_are_enabled() -> None:
    enabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=True,
    )
    disabled = _warmed_two_block_region_ring(
        reference=False,
        regions_enabled=False,
    )
    reference = _warmed_two_block_region_ring(reference=True)
    for system in (enabled, disabled, reference):
        system.cpu.flag_i = True
    enabled_owner = enabled._native_system
    disabled_owner = disabled._native_system
    enabled_owner._start_concurrency_profile()
    disabled_owner._start_concurrency_profile()

    enabled_stats = enabled.run_batch_stats(1_000)
    disabled_stats = disabled.run_batch_stats(1_000)
    reference_stats = reference.run_batch_stats(1_000)
    enabled_snapshot = dict(enabled_owner._stop_concurrency_profile())
    disabled_snapshot = dict(disabled_owner._stop_concurrency_profile())
    enabled_counts = dict(enabled_snapshot["counts"])
    disabled_counts = dict(disabled_snapshot["counts"])

    assert (
        _architectural_batch_signature(enabled_stats)
        == _architectural_batch_signature(disabled_stats)
        == _architectural_batch_signature(reference_stats)
    )
    assert (
        _core_signature(enabled)
        == _core_signature(disabled)
        == _core_signature(reference)
    )
    assert all(
        enabled_counts[name] == disabled_counts[name] == 0
        for name in JIT_REGION_PROFILE_COUNT_FIELDS
    )
    if enabled_snapshot["single_core_jit_backend"] == "x86_64":
        for counts in (enabled_counts, disabled_counts):
            assert counts["uncontended_jit_executions"] == 500
            assert counts["uncontended_jit_steps"] == 1_000
            assert counts["uncontended_jit_native_entries"] == 500
            assert counts["uncontended_jit_native_returns"] == 500


def test_jit_successor_profile_counts_warm_two_block_ring_exactly() -> None:
    system = _system()
    program = assemble(
        """
first:
    inc r4
    br second
second:
    inc r5
    br first
"""
    )
    assert len(program) == 6
    system.load_binary(0, program)
    system.boot(entry=0)

    warm = system.run_batch_stats(16)
    assert warm.instructions_executed == 16
    assert system.cpu.pc == 0
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1_000)
    snapshot = dict(owner._stop_concurrency_profile())
    _assert_block_cache_profile_reconciles(snapshot)
    successor_profile = dict(
        snapshot["single_core_jit_successor_profile"]
    )

    assert stats.instructions_executed == 1_000
    assert system.cpu.pc == 0
    if snapshot["single_core_jit_backend"] != "x86_64":
        assert successor_profile["candidate_block_completions"] == 0
        assert successor_profile["observations"] == 0
        assert successor_profile["edges"] == []
        return

    assert successor_profile["candidate_block_completions"] == 500
    assert successor_profile["observations"] == 499
    assert successor_profile["replacements"] == 0
    assert successor_profile["exact"]
    assert not successor_profile["counter_saturated"]
    edges = {
        (edge["source_address"], edge["target_address"]): dict(edge)
        for edge in successor_profile["edges"]
    }
    assert set(edges) == {(0, 3), (3, 0)}
    assert edges[(0, 3)]["estimated_count"] == 250
    assert edges[(3, 0)]["estimated_count"] == 249
    edge_fields = {
        "source_address",
        "source_psel",
        "source_spsel",
        "source_identity_size",
        "source_identity_fingerprint",
        "target_address",
        "target_psel",
        "target_spsel",
        "target_identity_size",
        "target_identity_fingerprint",
        "estimated_count",
        "max_overcount",
    }
    assert all(set(edge) == edge_fields for edge in edges.values())
    assert all(edge["max_overcount"] == 0 for edge in edges.values())
    assert all(
        edge["source_psel"] == edge["target_psel"] == 3
        and edge["source_spsel"] == edge["target_spsel"] == 15
        and edge["source_identity_size"] == 3
        and edge["target_identity_size"] == 3
        and edge["source_identity_fingerprint"] != 0
        and edge["target_identity_fingerprint"] != 0
        for edge in edges.values()
    )
    assert (
        edges[(0, 3)]["source_identity_fingerprint"]
        == edges[(3, 0)]["target_identity_fingerprint"]
    )
    assert (
        edges[(0, 3)]["target_identity_fingerprint"]
        == edges[(3, 0)]["source_identity_fingerprint"]
    )


def test_jit_successor_profile_does_not_bridge_batch_segments() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
first:
    inc r4
    br second
second:
    inc r5
    br first
"""
        ),
    )
    system.boot(entry=0)
    assert system.run_batch_stats(16).instructions_executed == 16
    assert system.cpu.pc == 0

    owner = system._native_system
    owner._start_concurrency_profile()
    assert system.run_batch_stats(500).instructions_executed == 500
    assert system.cpu.pc == 0
    assert system.run_batch_stats(500).instructions_executed == 500
    assert system.cpu.pc == 0
    snapshot = dict(owner._stop_concurrency_profile())
    successor_profile = dict(
        snapshot["single_core_jit_successor_profile"]
    )

    if snapshot["single_core_jit_backend"] != "x86_64":
        assert successor_profile["candidate_block_completions"] == 0
        assert successor_profile["observations"] == 0
        assert successor_profile["edges"] == []
        return

    assert successor_profile["candidate_block_completions"] == 500
    assert successor_profile["observations"] == 498
    assert successor_profile["replacements"] == 0
    assert successor_profile["exact"]
    edges = {
        (edge["source_address"], edge["target_address"]): dict(edge)
        for edge in successor_profile["edges"]
    }
    assert edges[(0, 3)]["estimated_count"] == 250
    assert edges[(3, 0)]["estimated_count"] == 248


def test_jit_successor_profile_stops_at_ext_dict_bios_shape() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
loop:
    mov r13, r15
    addi r13, 16
    dfind r0, r13
    br loop
"""
        ),
    )
    system.load_binary(0x210, b"\x01X")
    system.boot(entry=0)
    system.cpu.regs[15] = 0x200
    owner = system._native_system
    owner._set_single_core_jit_regions_enabled_for_test(True)

    warm = system.run_batch_stats(40)
    assert warm.instructions_executed == 40
    assert system.cpu.pc == 0
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1_000)
    snapshot = dict(owner._stop_concurrency_profile())
    _assert_block_cache_profile_reconciles(snapshot)
    successor_profile = dict(
        snapshot["single_core_jit_successor_profile"]
    )

    assert stats.instructions_executed == 1_000
    assert system.cpu.pc == 0
    assert successor_profile["observations"] == 0
    assert successor_profile["replacements"] == 0
    assert successor_profile["exact"]
    assert not successor_profile["counter_saturated"]
    assert successor_profile["edges"] == []
    counts = dict(snapshot["counts"])
    assert counts["uncontended_jit_region_entries"] == 0
    assert counts["uncontended_jit_region_blocks"] == 0
    assert counts["uncontended_jit_region_steps"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert successor_profile["candidate_block_completions"] == 250
        assert counts["uncontended_jit_executions"] == 250
        assert counts["uncontended_jit_steps"] == 500
        assert counts["uncontended_jit_native_entries"] == 250
        assert counts["uncontended_jit_native_returns"] == 250
    else:
        assert successor_profile["candidate_block_completions"] == 0


def test_jit_successor_profile_does_not_cross_native_memory_block() -> None:
    system = _system()
    system.load_binary(
        0,
        assemble(
            """
first:
    inc r4
    br memory
memory:
    ld.b r5, r9
    br first
"""
        ),
    )
    system.load_binary(0x200, b"\xA5")
    system.boot(entry=0)
    system.cpu.regs[9] = 0x200
    owner = system._native_system
    owner._set_single_core_jit_regions_enabled_for_test(True)

    warm = system.run_batch_stats(16)
    assert warm.instructions_executed == 16
    assert system.cpu.pc == 0
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(1_000)
    snapshot = dict(owner._stop_concurrency_profile())
    _assert_block_cache_profile_reconciles(snapshot)
    successor_profile = dict(
        snapshot["single_core_jit_successor_profile"]
    )

    assert stats.instructions_executed == 1_000
    assert system.cpu.pc == 0
    assert successor_profile["observations"] == 0
    assert successor_profile["replacements"] == 0
    assert successor_profile["exact"]
    assert not successor_profile["counter_saturated"]
    assert successor_profile["edges"] == []
    counts = dict(snapshot["counts"])
    assert counts["uncontended_jit_region_entries"] == 0
    assert counts["uncontended_jit_region_blocks"] == 0
    assert counts["uncontended_jit_region_steps"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert successor_profile["candidate_block_completions"] == 250
        assert counts["uncontended_jit_executions"] == 500
        assert counts["uncontended_jit_steps"] == 1_000
        assert counts["uncontended_jit_native_entries"] == 500
        assert counts["uncontended_jit_native_returns"] == 500
    else:
        assert successor_profile["candidate_block_completions"] == 0


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


def _memory_motif_system(
    source: str,
    *,
    reference: bool,
    ext_mem_size: int = 0,
) -> MegapadSystem:
    system = _system(
        reference=reference,
        ext_mem_size=ext_mem_size,
    )
    system.load_binary(0, assemble("nop"))
    system.load_binary(1, assemble(source, base_addr=1))
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    prime = system.run_batch_stats(1)
    assert prime.instructions_executed == 1
    assert system.cpu.pc == 1
    return system


def _assert_multi_memory_profile(
    snapshot: dict,
    *,
    instruction_count: int,
) -> None:
    _assert_block_cache_profile_reconciles(snapshot)
    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == 1 + 2 * instruction_count
    assert counts["uncontended_block_lookups"] == 3
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_misses"] == 1
    assert counts["uncontended_block_build_attempts"] == 1
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_evictions"] == 0
    assert counts["uncontended_block_nonresident_rejections"] == 0
    assert counts["uncontended_block_zero_instruction_rejections"] == 0
    assert counts["uncontended_block_one_instruction_rejections"] == 0
    assert counts["uncontended_block_rejection_cache_hits"] == 0
    assert counts["uncontended_block_rejection_cache_stores"] == 0
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_executions"] == 2
        assert counts["uncontended_block_steps"] == 2 * instruction_count
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_plan_evictions"] == 0
        assert counts["uncontended_jit_arena_allocations"] == 1
        assert counts["uncontended_jit_arena_allocation_failures"] == 0
        assert counts["uncontended_jit_slot_publications"] == 1
        assert counts["uncontended_jit_slot_rewrites"] == 0
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 2 * instruction_count
    else:
        assert counts["uncontended_block_executions"] == 0
        assert counts["uncontended_block_steps"] == 0
        assert all(
            counts[name] == 0
            for name in JIT_PROFILE_COUNT_FIELDS
        )


def _run_byte_copy_motif(*, reference: bool) -> tuple[tuple, dict]:
    system = _memory_motif_system(
        BYTE_COPY_MOTIF_SOURCE,
        reference=reference,
    )
    sources = ((0x200, 0x12), (0x220, 0xA5))
    destinations = (0x300, 0x320)
    for address, value in sources:
        system.load_binary(address, bytes((value,)))
    for address in destinations:
        system.load_binary(address, b"\xEE")

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[9] = sources[0][0]
    system.cpu.regs[7] = destinations[0]
    sliced = system.run_batch_stats(1)
    assert sliced.instructions_executed == 1

    for (source, value), destination in zip(
        sources,
        destinations,
        strict=True,
    ):
        system.cpu.pc = 1
        system.cpu.regs[0] = 0xFFFF_FFFF_FFFF_FFFF
        system.cpu.regs[9] = source
        system.cpu.regs[7] = destination
        copied = system.run_batch_stats(2)
        assert copied.instructions_executed == 2
        assert copied.system_cycles_advanced == 2
        assert system.cpu.regs[0] == value
        assert system.cpu.mem[destination] == value

    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    assert system.cpu.cycle_count == 6
    assert owner.system_cycles == 6
    return _core_signature(system), snapshot


def test_hot_byte_copy_pair_uses_one_generic_two_span_block() -> None:
    fast, snapshot = _run_byte_copy_motif(reference=False)
    reference, _ = _run_byte_copy_motif(reference=True)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=2)


def _run_indexed_byte_copy_motif(*, reference: bool) -> tuple[tuple, dict]:
    system = _memory_motif_system(
        """
    addi r9, 3
    add r9, r10
    ld.b r0, r9
    st.b r7, r0
""",
        reference=reference,
    )
    cases = (
        (0x180, 0x7D, 0x200, 0x12, 0x300),
        (
            (1 << 64) - 1 - 0x100,
            0x33E,
            0x240,
            0xA5,
            0x320,
        ),
    )
    for _, _, source, value, destination in cases:
        system.load_binary(source, bytes((value,)))
        system.load_binary(destination, b"\xEE")

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[9] = cases[0][0]
    system.cpu.regs[10] = cases[0][1]
    sliced = system.run_batch_stats(1)
    assert sliced.instructions_executed == 1

    for base, index, source, value, destination in cases:
        system.cpu.pc = 1
        system.cpu.regs[0] = 0xFFFF_FFFF_FFFF_FFFF
        system.cpu.regs[7] = destination
        system.cpu.regs[9] = base
        system.cpu.regs[10] = index
        copied = system.run_batch_stats(4)
        assert copied.instructions_executed == 4
        assert copied.system_cycles_advanced == 4
        assert system.cpu.regs[0] == value
        assert system.cpu.regs[7] == destination
        assert system.cpu.regs[9] == source
        assert system.cpu.regs[10] == index
        assert system.cpu.mem[destination] == value

    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    assert system.cpu.cycle_count == 10
    assert owner.system_cycles == 10
    return _core_signature(system), snapshot


def test_indexed_byte_copy_uses_two_entry_sources_and_wrapping_addend(
) -> None:
    fast, snapshot = _run_indexed_byte_copy_motif(reference=False)
    reference, _ = _run_indexed_byte_copy_motif(reference=True)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=4)


def _run_external_copy_motif(
    *,
    reference: bool,
    width: int,
) -> tuple[tuple, dict]:
    assert width in (1, 8)
    system = _memory_motif_system(
        (
            BYTE_COPY_MOTIF_SOURCE
            if width == 1
            else NATURAL_COPY_MOTIF_SOURCE
        ),
        reference=reference,
        ext_mem_size=4096,
    )
    base = system.ext_mem_base
    values = (
        (0x12, 0xA5)
        if width == 1
        else (
            0x0123_4567_89AB_CDEF,
            0xFEDC_BA98_7654_3210,
        )
    )
    sources = tuple(
        (base + offset, value)
        for offset, value in zip((0x101, 0x121), values, strict=True)
    )
    destinations = (base + 0x203, base + 0x223)
    for address, value in sources:
        system.load_binary(address, value.to_bytes(width, "little"))
    for address in destinations:
        system.load_binary(address, b"\xEE" * width)

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[9] = sources[0][0]
    system.cpu.regs[7] = destinations[0]
    sliced = system.run_batch_stats(1)
    assert sliced.instructions_executed == 1

    for (source, value), destination in zip(
        sources,
        destinations,
        strict=True,
    ):
        system.cpu.pc = 1
        system.cpu.regs[0] = 0xFFFF_FFFF_FFFF_FFFF
        system.cpu.regs[9] = source
        system.cpu.regs[7] = destination
        copied = system.run_batch_stats(2)
        assert copied.instructions_executed == 2
        assert copied.system_cycles_advanced == 2
        assert system.cpu.regs[0] == value
        assert bytes(
            system._ext_mem[
                destination - base:destination - base + width
            ]
        ) == value.to_bytes(width, "little")

    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    assert system.cpu.cycle_count == 6
    assert owner.system_cycles == 6
    signature = (
        _core_signature(system),
        bytes(system._ext_mem),
    )
    return signature, snapshot


def test_hot_external_byte_copy_uses_one_generic_two_span_block() -> None:
    fast, snapshot = _run_external_copy_motif(reference=False, width=1)
    reference, _ = _run_external_copy_motif(reference=True, width=1)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=2)


def test_hot_unaligned_external_natural_copy_uses_native_block() -> None:
    fast, snapshot = _run_external_copy_motif(reference=False, width=8)
    reference, _ = _run_external_copy_motif(reference=True, width=8)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=2)


def _run_external_natural_cross_aperture(
    *,
    reference: bool,
) -> tuple[tuple, dict]:
    system = _memory_motif_system(
        NATURAL_COPY_MOTIF_SOURCE,
        reference=reference,
        ext_mem_size=4096,
    )
    base = system.ext_mem_base
    source = base + len(system._ext_mem) - 4
    destination = base + 0x303
    external_tail = bytes.fromhex("10 32 54 76")
    system.load_binary(source, external_tail)
    system.load_binary(destination, b"\xEE" * 8)
    expected = external_tail + bytes(system.cpu.mem[:4])

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[0] = 0
    system.cpu.regs[9] = source
    system.cpu.regs[7] = destination
    copied = system.run_batch_stats(2)
    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )

    assert copied.instructions_executed == 2
    assert copied.system_cycles_advanced == 2
    assert system.cpu.regs[0] == int.from_bytes(expected, "little")
    assert bytes(
        system._ext_mem[
            destination - base:destination - base + 8
        ]
    ) == expected
    signature = (
        _core_signature(system),
        bytes(system._ext_mem),
    )
    return signature, snapshot


def test_external_natural_cross_aperture_declines_native_preflight() -> None:
    fast, snapshot = _run_external_natural_cross_aperture(reference=False)
    reference, _ = _run_external_natural_cross_aperture(reference=True)

    assert fast == reference
    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == 2
    assert counts["uncontended_block_steps"] == 0
    assert counts["uncontended_jit_executions"] == 0
    assert counts["uncontended_jit_steps"] == 0


def _warmed_byte_copy_system(*, reference: bool) -> MegapadSystem:
    system = _memory_motif_system(
        BYTE_COPY_MOTIF_SOURCE,
        reference=reference,
    )
    system.load_binary(0x200, b"\x12")
    system.load_binary(0x300, b"\xEE")
    for _ in range(2):
        system.cpu.pc = 1
        system.cpu.regs[9] = 0x200
        system.cpu.regs[7] = 0x300
        warm = system.run_batch_stats(2)
        assert warm.instructions_executed == 2
        assert warm.system_cycles_advanced == 2
    return system


def _run_later_span_alias_fallback(
    *,
    reference: bool,
) -> tuple[tuple, dict]:
    system = _warmed_byte_copy_system(reference=reference)
    system.load_binary(0x220, b"\xA5")
    system.load_binary(0x320, b"\xEE")
    system.cpu.pc = 1
    system.cpu.regs[0] = 0
    system.cpu.regs[9] = 0x220
    system.cpu.regs[7] = len(system.cpu.mem) + 0x320
    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()

    stats = system.run_batch_stats(2)
    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )

    assert stats.instructions_executed == 2
    assert stats.system_cycles_advanced == 2
    assert system.cpu.regs[0] == 0xA5
    assert system.cpu.regs[9] == 0x220
    assert system.cpu.regs[7] == len(system.cpu.mem) + 0x320
    assert system.cpu.pc == 5
    assert system.cpu.mem[0x320] == 0xA5
    return _core_signature(system), snapshot


def test_later_span_preflight_failure_makes_zero_block_progress() -> None:
    fast, snapshot = _run_later_span_alias_fallback(reference=False)
    reference, _ = _run_later_span_alias_fallback(reference=True)

    assert fast == reference
    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == 2
    assert counts["uncontended_block_lookups"] == 2
    assert counts["uncontended_block_hits"] == 1
    assert counts["uncontended_block_executions"] == 0
    assert counts["uncontended_block_steps"] == 0
    assert counts["uncontended_jit_executions"] == 0
    assert counts["uncontended_jit_steps"] == 0


def test_native_multi_memory_ipi_exit_publishes_exact_prefix() -> None:
    system = _warmed_byte_copy_system(reference=False)
    owner = system._native_system
    if (
        owner._concurrency_profile_snapshot()[
            "single_core_jit_backend"
        ] != "x86_64"
    ):
        pytest.skip("native completed-prefix oracle requires x86-64")

    source = 0x220
    destination = 0x320
    system.load_binary(source, b"\xA5")
    system.load_binary(destination, b"\xEE")
    system.cpu.pc = 1
    system.cpu.regs[0] = 0
    system.cpu.regs[9] = source
    system.cpu.regs[7] = destination
    system.cpu.flag_i = True
    deliveries: list[tuple[int, tuple]] = []

    def observe_ipi(vector: int) -> None:
        deliveries.append((vector, _core_signature(system)))
        owner.set_ipi_line(0, False)
        system.cpu.flag_i = False
        system.cpu.halted = True

    system.cpu._trap = observe_ipi
    owner._start_concurrency_profile()
    owner._inject_uncontended_ipi_at_next_native_entry()

    stats = system.run_batch_stats(2)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])

    reference = _warmed_byte_copy_system(reference=True)
    reference.load_binary(source, b"\xA5")
    reference.load_binary(destination, b"\xEE")
    reference.cpu.pc = 1
    reference.cpu.regs[0] = 0
    reference.cpu.regs[9] = source
    reference.cpu.regs[7] = destination
    reference.cpu.flag_i = True
    reference_stats = reference.run_batch_stats(1)

    assert reference_stats.instructions_executed == 1
    assert deliveries == [(IVEC_IPI, _core_signature(reference))]
    assert stats.instructions_executed == 1
    assert stats.system_cycles_advanced == 1
    assert system.cpu.regs[0] == 0xA5
    assert system.cpu.pc == 3
    assert system.cpu.mem[destination] == 0xEE
    assert counts["uncontended_steps"] == 1
    assert counts["uncontended_interrupt_boundaries"] == 1
    assert counts["uncontended_block_lookups"] == 1
    assert counts["uncontended_block_hits"] == 1
    assert counts["uncontended_block_executions"] == 1
    assert counts["uncontended_block_steps"] == 1
    assert counts["uncontended_jit_executions"] == 1
    assert counts["uncontended_jit_steps"] == 1


def _run_forth_plus_motif(*, reference: bool) -> tuple[tuple, dict]:
    system = _memory_motif_system(
        """
    ldn r1, r14
    addi r14, 8
    ldn r0, r14
    add r0, r1
    str r14, r0
""",
        reference=reference,
    )
    stacks = (
        (0x400, 0x1020_3040_5060_7080, 0x0102_0304_0506_0708),
        (0x480, 0xFEDC_BA98_7654_3210, 0x1111_2222_3333_4444),
    )
    for stack, first, second in stacks:
        system.load_binary(
            stack,
            first.to_bytes(8, "little") +
            second.to_bytes(8, "little"),
        )

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[14] = stacks[0][0]
    sliced = system.run_batch_stats(1)
    assert sliced.instructions_executed == 1

    for stack, first, second in stacks:
        system.cpu.pc = 1
        system.cpu.regs[0] = 0
        system.cpu.regs[1] = 0
        system.cpu.regs[14] = stack
        completed = system.run_batch_stats(5)
        assert completed.instructions_executed == 5
        assert completed.system_cycles_advanced == 5
        expected = (first + second) & 0xFFFF_FFFF_FFFF_FFFF
        assert system.cpu.regs[0] == expected
        assert system.cpu.regs[1] == first
        assert system.cpu.regs[14] == stack + 8
        assert bytes(system.cpu.mem[stack:stack + 8]) == first.to_bytes(
            8,
            "little",
        )
        assert bytes(system.cpu.mem[stack + 8:stack + 16]) == (
            expected.to_bytes(8, "little")
        )

    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    assert system.cpu.cycle_count == 12
    assert owner.system_cycles == 12
    return _core_signature(system), snapshot


def test_forth_plus_uses_affine_multi_memory_recipes() -> None:
    fast, snapshot = _run_forth_plus_motif(reference=False)
    reference, _ = _run_forth_plus_motif(reference=True)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=5)


def _run_forth_fetch_motif(*, reference: bool) -> tuple[tuple, dict]:
    system = _memory_motif_system(
        """
    ldn r1, r14
    ldn r0, r1
    str r14, r0
""",
        reference=reference,
    )
    cases = (
        (0x500, 0x600, 0x0123_4567_89AB_CDEF),
        (0x520, 0x680, 0xFEDC_BA98_7654_3210),
    )
    for stack, pointee, value in cases:
        system.load_binary(stack, pointee.to_bytes(8, "little"))
        system.load_binary(pointee, value.to_bytes(8, "little"))

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    system.cpu.regs[14] = cases[0][0]
    sliced = system.run_batch_stats(1)
    assert sliced.instructions_executed == 1

    for stack, pointee, value in cases:
        system.cpu.pc = 1
        system.cpu.regs[0] = 0
        system.cpu.regs[1] = 0
        system.cpu.regs[14] = stack
        completed = system.run_batch_stats(3)
        assert completed.instructions_executed == 3
        assert completed.system_cycles_advanced == 3
        assert system.cpu.regs[0] == value
        assert system.cpu.regs[1] == pointee
        assert system.cpu.regs[14] == stack
        assert bytes(system.cpu.mem[stack:stack + 8]) == value.to_bytes(
            8,
            "little",
        )

    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    assert system.cpu.cycle_count == 8
    assert owner.system_cycles == 8
    return _core_signature(system), snapshot


def test_forth_fetch_uses_a_prior_read_address_recipe() -> None:
    fast, snapshot = _run_forth_fetch_motif(reference=False)
    reference, _ = _run_forth_fetch_motif(reference=True)

    assert fast == reference
    _assert_multi_memory_profile(snapshot, instruction_count=3)


def _cross_line_link_loop_system(
    *, reference: bool
) -> tuple[MegapadSystem, bytes, int, int]:
    system = _system(reference=reference)
    loop_address = 15
    loop = assemble(
        """
loop:
    ldn r13, r13
    br loop
""",
        base_addr=loop_address,
    )
    assert bytes(loop) == bytes.fromhex("50 dd 30 fc")
    system.load_binary(0, assemble("nop"))
    system.load_binary(loop_address, loop)
    link_address = 0x200
    system.load_binary(
        link_address,
        link_address.to_bytes(8, "little"),
    )
    system.boot(entry=0)
    if reference:
        system.cores[1].halted = True
        system.cores[1].idle = False
    system.cpu.regs[13] = link_address

    # Populate only the starting guest I-cache line. The first loop visit
    # must decline without retaining an identity that can mask the plan after
    # architectural execution fills the following line.
    prime = system.run_batch_stats(1)
    assert prime.instructions_executed == 1
    assert system.cpu.pc == 1
    system.cpu.pc = loop_address
    return system, bytes(loop), loop_address, link_address


def _run_cross_line_link_loop(*, reference: bool) -> tuple[tuple, dict]:
    system, _, loop_address, link_address = (
        _cross_line_link_loop_system(reference=reference)
    )

    owner = system._native_system
    if not reference:
        owner._start_concurrency_profile()
    for _ in range(4):
        stats = system.run_batch_stats(2)
        assert stats.instructions_executed == 2
        assert stats.system_cycles_advanced == 3
        assert stats.per_core_cycles[0] == 3
        assert system.cpu.pc == loop_address
        assert system.cpu.regs[13] == link_address
    snapshot = (
        {} if reference else dict(owner._stop_concurrency_profile())
    )
    return _core_signature(system), snapshot


def test_cross_line_ldn_branch_builds_one_generic_native_block() -> None:
    fast, snapshot = _run_cross_line_link_loop(reference=False)
    reference, _ = _run_cross_line_link_loop(reference=True)

    assert fast == reference
    counts = dict(snapshot["counts"])
    _assert_block_cache_profile_reconciles(snapshot)
    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_lookups"] == 6
    assert counts["uncontended_block_hits"] == 2
    assert counts["uncontended_block_misses"] == 4
    assert counts["uncontended_block_build_attempts"] == 3
    assert counts["uncontended_block_builds"] == 1
    assert counts["uncontended_block_nonresident_rejections"] == 1
    assert counts["uncontended_block_zero_instruction_rejections"] == 0
    assert counts["uncontended_block_one_instruction_rejections"] == 1
    assert counts["uncontended_block_rejection_cache_hits"] == 1
    assert counts["uncontended_block_rejection_cache_stores"] == 1
    _assert_jit_used_when_available(snapshot, counts)
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_block_executions"] == 2
        assert counts["uncontended_block_steps"] == 4
        assert counts["uncontended_jit_compile_attempts"] == 1
        assert counts["uncontended_jit_compilations"] == 1
        assert counts["uncontended_jit_executions"] == 2
        assert counts["uncontended_jit_steps"] == 4
    else:
        assert counts["uncontended_block_executions"] == 0
        assert counts["uncontended_block_steps"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0
        assert counts["uncontended_jit_steps"] == 0


@pytest.mark.parametrize(
    ("changed", "expected_builds", "expected_evictions"),
    (
        pytest.param(False, 0, 0, id="same-bytes"),
        pytest.param(True, 1, 1, id="changed-bytes"),
    ),
)
def test_cross_line_identity_revalidates_second_line_refill(
    changed: bool,
    expected_builds: int,
    expected_evictions: int,
) -> None:
    def run(*, reference: bool) -> tuple[tuple, dict]:
        system, loop, loop_address, link_address = (
            _cross_line_link_loop_system(reference=reference)
        )
        for _ in range(4):
            warm = system.run_batch_stats(2)
            assert warm.instructions_executed == 2

        assert loop[1] == 0xDD
        # The changed form selects R12 instead of R13 as the LDN address.
        # Both registers retain the self-link, so architectural results stay
        # comparable while the second-line identity must be replaced.
        replacement_operand = 0xDC if changed else loop[1]
        system.cpu.regs[12] = link_address
        system.cpu.mem_write8(loop_address + 1, replacement_operand)
        owner = system._native_system
        if not reference:
            owner._start_concurrency_profile()
        for _ in range(4):
            stats = system.run_batch_stats(2)
            assert stats.instructions_executed == 2
            assert stats.system_cycles_advanced == 3
            assert stats.per_core_cycles[0] == 3
            assert system.cpu.pc == loop_address
            assert system.cpu.regs[13] == link_address
        snapshot = (
            {} if reference else dict(owner._stop_concurrency_profile())
        )
        return _core_signature(system), snapshot

    fast, snapshot = run(reference=False)
    reference, _ = run(reference=True)

    assert fast == reference
    counts = dict(snapshot["counts"])
    assert counts["uncontended_steps"] == 8
    assert counts["uncontended_block_builds"] == expected_builds
    assert counts["uncontended_block_evictions"] == expected_evictions
    if snapshot["single_core_jit_backend"] == "x86_64":
        assert counts["uncontended_jit_compile_attempts"] == expected_builds
        assert counts["uncontended_jit_compilations"] == expected_builds
        assert counts["uncontended_jit_plan_evictions"] == expected_evictions
        assert counts["uncontended_jit_executions"] >= 2
    else:
        assert counts["uncontended_block_executions"] == 0
        assert counts["uncontended_jit_compile_attempts"] == 0
        assert counts["uncontended_jit_compilations"] == 0
        assert counts["uncontended_jit_compile_failures"] == 0
        assert counts["uncontended_jit_executions"] == 0


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
