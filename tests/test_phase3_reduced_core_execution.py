"""Phase 3 element-4 reduced-core and cluster coordinator oracles."""

from __future__ import annotations

import threading

import pytest

from asm import assemble
from bench_phase2_cluster import (
    _all_core_trace,
    _cluster_crc_trace,
)
from devices import MMIO_BASE, SYSINFO_BASE
from megapad64 import CLUSTER_SPAD_ADDR
from system import MegapadSystem


SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE


def _system(*, worker_count: int) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )


def test_phase2_cluster_gold_is_exact_across_worker_counts() -> None:
    traces = {
        worker_count: _cluster_crc_trace(
            worker_count=worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert traces[2] == traces[1]
    assert traces[4] == traces[1]


def test_phase2_all_core_trace_is_exact_across_worker_counts() -> None:
    traces = {
        worker_count: _all_core_trace(
            worker_count=worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert traces[2] == traces[1]
    assert traces[4] == traces[1]


def _repeated_mul_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    first, second = system.clusters[0].cores[:2]
    address = 0x100
    system.load_binary(
        address,
        assemble(
            """
loop:
    mul r1, r2
    br loop
"""
        ),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, multiplier in (
        (first, 2),
        (second, 3),
    ):
        cpu.pc = address
        cpu.regs[1] = 1
        cpu.regs[2] = multiplier
        cpu.halted = False

    stats = system.run_batch_stats(12)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        first.regs[1],
        second.regs[1],
        arbiter["grant_counts"]["mul_div"],
        arbiter["last_grants"]["mul_div"],
        arbiter["grant_sequence"],
    )


def test_repeated_cluster_contention_retains_equal_round_credit() -> None:
    signatures = {
        worker_count: _repeated_mul_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 12
    assert reference[2] == (0, 6, 6, 0, 0)
    assert reference[9:11] == (8, 27)
    assert reference[11:] == (6, 0, 6)


def _coherent_fetch_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    micro = system.clusters[0].cores[0]
    micro_address = 0x100
    halt_opcode = assemble("halt")[0]
    system.load_binary(
        0,
        assemble("st.b r1, r2\nhalt"),
    )
    system.load_binary(
        micro_address,
        assemble("nop\ninc r4\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    full = system.cores[0]
    full.pc = 0
    full.regs[1] = micro_address + 1
    full.regs[2] = halt_opcode
    full.halted = False
    micro.pc = micro_address
    micro.halted = False

    first = system.run_batch_stats(3)
    second = system.run_batch_stats(1)
    return (
        first.instructions_executed,
        first.per_core_instructions,
        first.system_cycles_advanced,
        second.instructions_executed,
        second.per_core_instructions,
        second.system_cycles_advanced,
        system.cpu.mem[micro_address + 1],
        micro.pc,
        micro.regs[4],
        micro.halted,
    )


def test_micro_fetch_observes_write_at_the_next_frontier() -> None:
    signatures = {
        worker_count: _coherent_fetch_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 3
    assert reference[3] == 1
    assert reference[6] == assemble("halt")[0]
    assert reference[8] == 0
    assert reference[9] is True


def _same_frontier_fetch_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    full = system.cores[0]
    micro = system.clusters[0].cores[0]
    micro_address = 0x100
    halt_opcode = assemble("halt")[0]
    system.load_binary(0, assemble("st.b r1, r2\nhalt"))
    system.load_binary(
        micro_address,
        assemble("loop:\n    br loop"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    full.pc = 0
    full.regs[1] = micro_address
    full.regs[2] = halt_opcode
    full.halted = False
    micro.pc = micro_address
    micro.halted = False

    gathered = system.run_batch_stats(2)
    gathered_state = (
        gathered.instructions_executed,
        gathered.per_core_instructions,
        gathered.system_cycles_advanced,
        system.cpu.mem[micro_address],
        micro.pc,
        micro.halted,
    )
    full.halted = True
    observed = system.run_batch_stats(1)
    return (
        gathered_state,
        observed.instructions_executed,
        observed.per_core_instructions,
        observed.system_cycles_advanced,
        micro.pc,
        micro.halted,
    )


def test_same_frontier_fetch_precedes_ordered_shared_commit() -> None:
    """Version the interim gather-before-commit code-observation boundary."""
    signatures = {
        worker_count: _same_frontier_fetch_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == (
        2,
        (1, 1, 0, 0, 0),
        2,
        assemble("halt")[0],
        0x100,
        False,
    )
    assert reference[1] == 1
    assert reference[2] == (0, 1, 0, 0, 0)
    assert reference[5] is True


def _mixed_commit_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    full = system.cores[0]
    micro = system.clusters[0].cores[0]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False

    system.load_binary(
        0,
        assemble("inc r4\nst.b r1, r2\nhalt"),
    )
    system.load_binary(
        0x100,
        assemble("inc r4\nst.b r1, r2\nhalt"),
    )
    full.pc = 0
    micro.pc = 0x100
    full.halted = False
    micro.halted = False
    full.regs[1] = micro.regs[1] = SYSINFO_SINK
    full.regs[2] = 0xF0
    micro.regs[2] = 0xA0

    caller_thread = threading.get_ident()
    commits = []
    full._mmio_write8 = lambda address, value: commits.append(
        ("full", address, value, threading.get_ident())
    )
    original_micro_fallback = micro._step_python_fallback

    def observe_micro_fallback():
        commits.append(
            (
                "micro_boundary",
                micro.pc,
                None,
                threading.get_ident(),
            )
        )
        return original_micro_fallback()

    micro._step_python_fallback = observe_micro_fallback

    stats = system.run_batch_stats(4)
    assert all(
        thread == caller_thread
        for _kind, _address, _value, thread in commits
    )
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        tuple(
            (kind, address, value)
            for kind, address, value, _thread in commits
        ),
    )


def test_mixed_shared_commits_are_coordinator_only_and_stable() -> None:
    signatures = {
        worker_count: _mixed_commit_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1][-1] == (
        ("full", SYSINFO_SINK, 0xF0),
        ("micro_boundary", 0x101, None),
    )


def _sha_handoff_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    first, second = cluster.cores[:2]
    programs = (
        assemble("sha.init 0\nsha.final\nsha.release\nhalt"),
        assemble("sha.init 1\nsha.final\nsha.release\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, address, program in (
        (first, 0x100, programs[0]),
        (second, 0x180, programs[1]),
    ):
        system.load_binary(address, program)
        cpu.pc = address
        cpu.halted = False

    stats = system.run_batch_stats(6)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        tuple(cpu.pc for cpu in (first, second)),
        cluster.sha_locked,
        cluster.sha_owner,
        arbiter["grant_counts"]["sha"],
        arbiter["last_grants"]["sha"],
        arbiter["grant_sequence"],
    )


def test_sha_lock_handoff_is_exact_across_worker_counts() -> None:
    signatures = {
        worker_count: _sha_handoff_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 6
    assert reference[2] == (0, 3, 3, 0, 0)
    assert reference[10:12] == (False, None)
    assert reference[12:] == (6, 0, 6)


def _mex_contention_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    first, second = cluster.cores[:2]
    program = assemble("t.add\nhalt")
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, address in (
        (first, 0x100),
        (second, 0x180),
    ):
        system.load_binary(address, program)
        cpu.pc = address
        cpu.halted = False
    first.tsrc0 = 0x300
    first.tsrc1 = 0x340
    first.tdst = 0x380
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )

    stats = system.run_batch_stats(2)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        bytes(system.cpu.mem[0x380:0x3C0]),
        arbiter["grant_counts"]["mex"],
        arbiter["last_grants"]["mex"],
        arbiter["grant_sequence"],
    )


def test_mex_contention_is_exact_across_worker_counts() -> None:
    signatures = {
        worker_count: _mex_contention_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 2
    assert reference[2] == (0, 1, 1, 0, 0)
    assert reference[10:] == (2, 0, 2)


def _cross_resource_credit_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    mul_loser, mex_winner, mul_winner = cluster.cores[:3]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, address, source in (
        (mul_loser, 0x100, "mul r1, r2\nhalt"),
        (mex_winner, 0x180, "t.add\nhalt"),
        (mul_winner, 0x200, "mul r1, r2\nhalt"),
    ):
        system.load_binary(address, assemble(source))
        cpu.pc = address
        cpu.halted = False
    mul_loser.regs[1] = 3
    mul_loser.regs[2] = 7
    mul_winner.regs[1] = 5
    mul_winner.regs[2] = 11
    mex_winner.tsrc0 = 0x300
    mex_winner.tsrc1 = 0x340
    mex_winner.tdst = 0x380
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )

    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        mul_loser.pc,
        mex_winner.pc,
        mul_winner.pc,
        arbiter["grant_counts"]["mex"],
        arbiter["last_grants"]["mex"],
        arbiter["grant_counts"]["mul_div"],
        arbiter["last_grants"]["mul_div"],
        arbiter["grant_sequence"],
    )


def test_frozen_loser_funds_an_earlier_cross_resource_winner() -> None:
    signatures = {
        worker_count: _cross_resource_credit_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[:5] == (
        1,
        (0, 0, 1, 0, 0),
        (0, 1, 1, 1, 0),
        1,
        3,
    )
    assert reference[5:8] == (0x100, 0x182, 0x200)
    assert reference[8:] == (1, 1, 0, 0, 1)


def _locked_crc_stall_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    owner, contender = cluster.cores[:2]
    address = 0x100
    system.load_binary(
        address,
        assemble("crc.mode 1\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    contender.pc = address
    contender.halted = False
    assert cluster.crc_try_acquire(owner.core_id)

    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._native_system.system_cycles,
        system._scheduler_cursor,
        contender.pc,
        cluster.crc_locked,
        cluster.crc_owner,
        arbiter["grant_counts"]["crc"],
        arbiter["last_grants"]["crc"],
        arbiter["grant_sequence"],
        stats.system_stop_reason,
        stats.stop_cycle,
    )


def test_unchanged_locked_contender_closes_without_spinning() -> None:
    signatures = {
        worker_count: _locked_crc_stall_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0:5] == (
        0,
        0,
        (0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0),
        (0, 0, 1, 0, 0),
    )
    assert reference[5][2] == (
        0, 0, 0, 0, 1, 0, 0
    )
    assert reference[6:11] == (0, 1, 0, 0, 0x100)
    assert reference[11:16] == (
        True, 0, 0, 0, 0
    )
    assert reference[16:] == ("no_progress", 0)


def _hard_ineligible_credit_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    crc_loser, mex_winner, crc_owner = cluster.cores[:3]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    system.load_binary(
        0x100,
        assemble("crc.mode 1\nhalt"),
    )
    system.load_binary(
        0x180,
        assemble("t.add\nhalt"),
    )
    crc_loser.pc = 0x100
    crc_loser.halted = False
    mex_winner.pc = 0x180
    mex_winner.halted = False
    mex_winner.tsrc0 = 0x300
    mex_winner.tsrc1 = 0x340
    mex_winner.tdst = 0x380
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )
    assert cluster.crc_try_acquire(crc_owner.core_id)

    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        crc_loser.pc,
        mex_winner.pc,
        cluster.crc_locked,
        cluster.crc_owner,
        arbiter["grant_counts"]["crc"],
        arbiter["grant_counts"]["mex"],
        arbiter["last_grants"]["mex"],
        arbiter["grant_sequence"],
    )


def test_hard_ineligible_request_releases_credit_forward() -> None:
    signatures = {
        worker_count: _hard_ineligible_credit_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[:7] == (
        1,
        (0, 0, 1, 0, 0),
        (0, 1, 1, 0, 0),
        1,
        3,
        0x100,
        0x182,
    )
    assert reference[7:] == (
        True,
        2,
        0,
        1,
        1,
        1,
    )


def _frozen_ineligible_release_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    mex_winner, crc_loser, crc_owner = cluster.cores[:3]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    system.load_binary(0x100, assemble("t.add\nhalt"))
    system.load_binary(
        0x180,
        assemble("crc.mode 1\nhalt"),
    )
    mex_winner.pc = 0x100
    mex_winner.halted = False
    mex_winner.tsrc0 = 0x300
    mex_winner.tsrc1 = 0x340
    mex_winner.tdst = 0x380
    crc_loser.pc = 0x180
    crc_loser.halted = False
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )
    assert cluster.crc_try_acquire(crc_owner.core_id)
    original_fallback = mex_winner._step_python_fallback

    def release_after_mex():
        cycles = original_fallback()
        cluster.crc_release(crc_owner.core_id)
        return cycles

    mex_winner._step_python_fallback = release_after_mex
    stats = system.run_batch_stats(2)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        mex_winner.pc,
        crc_loser.pc,
        cluster.crc_locked,
        cluster.crc_owner,
        arbiter["grant_counts"]["mex"],
        arbiter["grant_counts"]["crc"],
        arbiter["last_grants"]["crc"],
        arbiter["grant_sequence"],
    )


def test_later_lock_release_does_not_retroactively_rerun_arbitration() -> None:
    signatures = {
        worker_count:
            _frozen_ineligible_release_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[:7] == (
        2,
        (0, 1, 1, 0, 0),
        (0, 1, 2, 0, 0),
        2,
        3,
        0x102,
        0x183,
    )
    assert reference[7:] == (
        True,
        1,
        1,
        1,
        1,
        2,
    )


def _ext_skip_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    micro = system.clusters[0].cores[0]
    address = 0x100
    system.load_binary(
        address,
        assemble(
            """
    cmpi r0, 0
    skip.eq
    ldi r16, 99
    inc r2
    halt
"""
        ),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    micro.pc = address
    micro.halted = False
    fallback_calls = 0
    original_fallback = micro._step_python_fallback

    def observe_fallback():
        nonlocal fallback_calls
        fallback_calls += 1
        return original_fallback()

    micro._step_python_fallback = observe_fallback
    stats = system.run_batch_stats(3)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        micro.pc,
        micro.regs[2],
        micro.regs[16],
        fallback_calls,
    )


def test_ext_skip_uses_recursive_python_target_sizing() -> None:
    signatures = {
        worker_count: _ext_skip_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 3
    assert reference[2] == (0, 3, 0, 0, 0)
    assert reference[8:] == (1, 0, 1)


def _scratchpad_fetch_signature(worker_count: int) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    micro = cluster.cores[0]
    code = assemble("inc r4\nhalt")
    for offset, value in enumerate(code):
        cluster.spad_write8(offset, value)
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    micro.pc = CLUSTER_SPAD_ADDR
    micro.halted = False
    fallback_calls = 0
    original_fallback = micro._step_python_fallback

    def observe_fallback():
        nonlocal fallback_calls
        fallback_calls += 1
        return original_fallback()

    micro._step_python_fallback = observe_fallback
    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        micro.pc,
        micro.regs[4],
        fallback_calls,
        arbiter["grant_sequence"],
    )


def test_scratchpad_instruction_fetch_uses_the_routed_oracle() -> None:
    signatures = {
        worker_count: _scratchpad_fetch_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 1
    assert reference[2] == (0, 1, 0, 0, 0)
    assert reference[7:] == (
        CLUSTER_SPAD_ADDR + 1,
        1,
        1,
        0,
    )


def _scratchpad_cluster_resource_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    first, second = cluster.cores[:2]
    code = assemble("mul r1, r2\nhalt")
    for offset, value in enumerate(code):
        cluster.spad_write8(offset, value)
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, left, right in (
        (first, 2, 3),
        (second, 4, 5),
    ):
        cpu.pc = CLUSTER_SPAD_ADDR
        cpu.regs[1] = left
        cpu.regs[2] = right
        cpu.halted = False

    stats = system.run_batch_stats(2)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        first.pc,
        second.pc,
        first.regs[1],
        second.regs[1],
        arbiter["grant_counts"]["mul_div"],
        arbiter["last_grants"]["mul_div"],
        arbiter["grant_sequence"],
    )


def test_scratchpad_shared_opcode_still_uses_cluster_arbitration() -> None:
    signatures = {
        worker_count:
            _scratchpad_cluster_resource_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[:5] == (
        2,
        (0, 1, 1, 0, 0),
        (0, 2, 1, 0, 0),
        2,
        3,
    )
    assert reference[5:9] == (
        CLUSTER_SPAD_ADDR + 2,
        CLUSTER_SPAD_ADDR + 2,
        6,
        20,
    )
    assert reference[9:] == (2, 0, 2)


def _scratchpad_route_boundary_signature(
    worker_count: int,
    *,
    scratchpad_first: bool,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    micro = cluster.cores[0]
    code = assemble("mul r1, r2")
    assert len(code) == 2
    if scratchpad_first:
        address = CLUSTER_SPAD_ADDR + 0xFFFF_FFFF
        cluster.spad_write8(0xFFFF_FFFF, code[0])
        system.load_binary(address + 1, code[1:])
    else:
        address = CLUSTER_SPAD_ADDR - 1
        system.load_binary(address, code[:1])
        cluster.spad_write8(0, code[1])
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    micro.pc = address
    micro.regs[1] = 6
    micro.regs[2] = 7
    micro.halted = False

    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.native_continuations,
        system._scheduler_cursor,
        micro.pc,
        micro.regs[1],
        arbiter["grant_counts"]["mul_div"],
        arbiter["last_grants"]["mul_div"],
        arbiter["grant_sequence"],
    )


@pytest.mark.parametrize(
    "scratchpad_first",
    (False, True),
)
def test_shared_decode_routes_each_scratchpad_boundary_byte(
    scratchpad_first: bool,
) -> None:
    signatures = {
        worker_count:
            _scratchpad_route_boundary_signature(
                worker_count,
                scratchpad_first=scratchpad_first,
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    start = (
        CLUSTER_SPAD_ADDR + 0xFFFF_FFFF
        if scratchpad_first
        else CLUSTER_SPAD_ADDR - 1
    )
    assert reference == (
        1,
        (0, 1, 0, 0, 0),
        1,
        2,
        start + 2,
        42,
        1,
        0,
        1,
    )


@pytest.mark.parametrize(
    "instruction_address",
    (SYSINFO_SINK, MMIO_BASE - 1),
)
@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_mmio_instruction_fetch_is_explicitly_unsupported(
    worker_count: int,
    instruction_address: int,
) -> None:
    system = _system(worker_count=worker_count)
    micro = system.clusters[0].cores[0]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    if instruction_address == MMIO_BASE - 1:
        system.load_binary(
            instruction_address,
            bytes((0x30,)),
        )
    micro.pc = instruction_address
    micro.halted = False

    with pytest.raises(
        RuntimeError,
        match=(
            "native system batch does not support "
            "reduced-core MMIO instruction fetch"
        ),
    ):
        system.run_batch_stats(1)

    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert micro.pc == instruction_address
    assert micro.cycle_count == 0
    assert arbiter["grant_sequence"] == 0


def _nonengine_f_modifier_signature(
    worker_count: int,
    modifier: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    micro = system.clusters[0].cores[0]
    address = 0x100
    instruction = (
        bytes((0xF0 | modifier,)) +
        assemble("st.b r1, r2")
    )
    system.load_binary(address, instruction)
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    micro.pc = address
    micro.regs[1] = 0x300
    micro.regs[2] = 0xA5
    micro.halted = False

    stats = system.run_batch_stats(1)
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.native_continuations,
        system._scheduler_cursor,
        micro.pc,
        system.cpu.mem[0x300],
        arbiter["grant_counts"]["bus"],
        arbiter["last_grants"]["bus"],
        arbiter["grant_sequence"],
    )


@pytest.mark.parametrize("modifier", (0xC, 0xF))
def test_nonengine_f_modifier_cannot_bypass_bus_arbitration(
    modifier: int,
) -> None:
    signatures = {
        worker_count:
            _nonengine_f_modifier_signature(
                worker_count,
                modifier,
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1] == (
        1,
        (0, 1, 0, 0, 0),
        1,
        2,
        0x103,
        0xA5,
        1,
        0,
        1,
    )


@pytest.mark.parametrize(
    ("resource", "instruction", "acquire"),
    (
        (
            "crc",
            bytes((0xFB, 0x06)),
            lambda cluster, core:
                cluster.crc_try_acquire(core.core_id),
        ),
        (
            "sha",
            bytes((0xFB, 0x17)),
            lambda cluster, core:
                cluster.sha_try_acquire(core.core_id),
        ),
    ),
)
def test_reserved_crypto_ops_trap_without_cluster_grants(
    resource,
    instruction,
    acquire,
) -> None:
    signatures = {}
    for worker_count in (1, 2, 4):
        system = _system(worker_count=worker_count)
        cluster = system.clusters[0]
        owner, contender = cluster.cores[:2]
        address = 0x100
        system.load_binary(address, instruction)
        for cpu in system.cores:
            cpu.halted = True
            cpu.idle = False
        contender.pc = address
        contender.halted = False
        assert acquire(cluster, owner)

        stats = system.run_batch_stats(1)
        arbiter = system._native_system._cluster_arbiter_snapshot(0)
        signatures[worker_count] = (
            stats.instructions_executed,
            stats.system_cycles_advanced,
            stats.per_core_instructions,
            stats.per_core_dispatches,
            stats.native_continuations,
            contender.pc,
            arbiter["grant_counts"][resource],
            arbiter["grant_sequence"],
            arbiter[f"{resource}_locked"],
            arbiter[f"{resource}_lock_owner"],
        )

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1] == (
        1,
        0,
        (0, 0, 1, 0, 0),
        (0, 0, 1, 0, 0),
        1,
        0x102,
        0,
        0,
        True,
        0,
    )


def _mex_encoding_identity_signature(
    worker_count: int,
    *,
    code: bytes,
    mutation_offset: int,
    replacement: int,
) -> tuple[tuple, int]:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    earlier, later = cluster.cores[:2]
    earlier_address = 0x100
    later_address = 0x180
    system.load_binary(
        earlier_address,
        assemble("mul r1, r2\nhalt"),
    )
    system.load_binary(later_address, code)
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    earlier.pc = earlier_address
    earlier.regs[1] = 3
    earlier.regs[2] = 7
    earlier.halted = False
    later.pc = later_address
    later.tsrc0 = 0x300
    later.tsrc1 = 0x340
    later.tdst = 0x380
    later.halted = False
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )

    trace = []
    original_earlier_fallback = (
        earlier._step_python_fallback
    )
    original_later_fallback = (
        later._step_python_fallback
    )

    def mutate_later_encoding():
        cycles = original_earlier_fallback()
        system.cpu.mem[
            later_address + mutation_offset
        ] = replacement
        trace.append(
            (
                "mutated",
                mutation_offset,
                replacement,
            )
        )
        return cycles

    def observe_later_encoding():
        trace.append(
            (
                "later",
                system.cpu.mem[
                    later_address + mutation_offset
                ],
            )
        )
        return original_later_fallback()

    earlier._step_python_fallback = (
        mutate_later_encoding
    )
    later._step_python_fallback = (
        observe_later_encoding
    )
    diagnostics_before = dict(
        system._native_system._private_worker_diagnostics()
    )
    stats = system.run_batch_stats(2)
    diagnostics_after = dict(
        system._native_system._private_worker_diagnostics()
    )
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    architectural = (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.native_continuations,
        system._scheduler_cursor,
        earlier.pc,
        later.pc,
        tuple(trace),
        arbiter["grant_counts"]["mul_div"],
        arbiter["grant_counts"]["mex"],
        arbiter["grant_sequence"],
    )
    return (
        architectural,
        diagnostics_after["wave_epoch"] -
            diagnostics_before["wave_epoch"],
    )


@pytest.mark.parametrize(
    (
        "code",
        "mutation_offset",
        "replacement",
        "actual_length",
        "selected_request_changes",
    ),
    (
        pytest.param(
            bytes((0xE7, 0x07, 0x01, 0x00, 0x02)),
            3,
            0x04,
            4,
            True,
            id="broadcast-rrot-control",
        ),
        pytest.param(
            bytes((0xF8, 0xE3, 0x07, 0x01)),
            3,
            0x02,
            3,
            False,
            id="extended-tsys-following-opcode",
        ),
        pytest.param(
            bytes((0xEB, 0x07, 0x01)),
            2,
            0x02,
            2,
            False,
            id="immediate-splat-tsys-following-opcode",
        ),
    ),
)
def test_mex_request_identity_matches_python_decode(
    code: bytes,
    mutation_offset: int,
    replacement: int,
    actual_length: int,
    selected_request_changes: bool,
) -> None:
    signatures = {
        worker_count:
            _mex_encoding_identity_signature(
                worker_count,
                code=code,
                mutation_offset=mutation_offset,
                replacement=replacement,
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2][0] == signatures[1][0]
    assert signatures[4][0] == signatures[1][0]
    reference = signatures[1][0]
    assert reference[:7] == (
        2,
        (0, 1, 1, 0, 0),
        (0, 1, 1, 0, 0),
        2,
        3,
        0x102,
        0x180 + actual_length,
    )
    assert reference[7] == (
        (
            "mutated",
            mutation_offset,
            replacement,
        ),
        ("later", replacement),
    )
    assert reference[8:] == (1, 1, 2)
    for worker_count in (1, 2, 4):
        expected_waves = (
            (2 + worker_count - 1) // worker_count
        )
        if selected_request_changes:
            expected_waves += 1
        assert signatures[worker_count][1] == expected_waves


def _bus_opcode_mutation_signature(
    worker_count: int,
) -> tuple[tuple, int]:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    earlier, later = cluster.cores[:2]
    earlier_address = 0x100
    later_address = 0x180
    load_opcode = assemble("ld.b r1, r2")[0]
    store_opcode = assemble("st.b r1, r2")[0]
    system.load_binary(
        earlier_address,
        assemble("t.add\nhalt"),
    )
    system.load_binary(
        later_address,
        assemble("ld.b r1, r2\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    earlier.pc = earlier_address
    earlier.halted = False
    earlier.tsrc0 = 0x300
    earlier.tsrc1 = 0x340
    earlier.tdst = 0x380
    later.pc = later_address
    later.regs[1] = 0x3C0
    later.regs[2] = 0xA5
    later.halted = False
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )
    system.cpu.mem[0x3C0] = 0
    assert system.cpu.mem[later_address] == load_opcode

    trace = []
    original_earlier_fallback = (
        earlier._step_python_fallback
    )
    original_later_fallback = (
        later._step_python_fallback
    )

    def mutate_later_opcode():
        cycles = original_earlier_fallback()
        system.cpu.mem[later_address] = store_opcode
        trace.append(("mutated", store_opcode))
        return cycles

    def observe_later_boundary():
        trace.append(
            (
                "later",
                system.cpu.mem[later_address],
            )
        )
        return original_later_fallback()

    earlier._step_python_fallback = mutate_later_opcode
    later._step_python_fallback = observe_later_boundary
    diagnostics_before = dict(
        system._native_system._private_worker_diagnostics()
    )
    stats = system.run_batch_stats(2)
    diagnostics_after = dict(
        system._native_system._private_worker_diagnostics()
    )
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        system.cpu.mem[0x3C0],
        tuple(trace),
        arbiter["grant_counts"]["mex"],
        arbiter["grant_counts"]["bus"],
        arbiter["grant_sequence"],
    )
    return (
        architectural,
        diagnostics_after["wave_epoch"] -
            diagnostics_before["wave_epoch"],
    )


def test_changed_bus_opcode_defers_the_selected_request() -> None:
    signatures = {
        worker_count: _bus_opcode_mutation_signature(
            worker_count
        )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2][0] == signatures[1][0]
    assert signatures[4][0] == signatures[1][0]
    reference = signatures[1][0]
    assert reference[0] == 2
    assert reference[2] == (0, 1, 1, 0, 0)
    assert reference[8] == 0xA5
    assert reference[9] == (
        ("mutated", assemble("st.b r1, r2")[0]),
        ("later", assemble("st.b r1, r2")[0]),
    )
    assert reference[10:] == (1, 1, 2)
    for worker_count in (1, 2, 4):
        expected_waves = (
            (2 + worker_count - 1) // worker_count
        ) + 1
        assert signatures[worker_count][1] == expected_waves


def _zero_retirement_grant_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    cluster = system.clusters[0]
    micro = cluster.cores[0]
    address = 0x100
    system.load_binary(
        address,
        assemble("crc.mode 0\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    micro.pc = address
    micro.halted = False

    def settle_zero(
        _core_index,
        _stop_reason,
        _trap_id,
        prefix_steps,
        prefix_cycles,
    ):
        assert cluster.crc_try_acquire(
            micro.core_id
        )
        return prefix_steps, prefix_cycles, True

    system._settle_native_core_continuation = settle_zero
    with pytest.raises(
        RuntimeError,
        match=(
            "granted cluster continuation must "
            "retire exactly one instruction"
        ),
    ):
        system.run_batch_stats(1)

    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    return (
        micro.pc,
        micro.cycle_count,
        system._native_system.system_cycles,
        system._scheduler_cursor,
        cluster.crc_locked,
        cluster.crc_owner,
        arbiter["grant_counts"]["crc"],
        arbiter["last_grants"]["crc"],
        arbiter["grant_sequence"],
    )


def test_zero_retirement_settlement_cannot_publish_a_cluster_grant() -> None:
    signatures = {
        worker_count:
            _zero_retirement_grant_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1] == (
        0x100,
        0,
        0,
        0,
        False,
        None,
        0,
        0,
        0,
    )


def _reduced_callback_failure_signature(
    worker_count: int,
) -> tuple:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=2,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False

    full = system.cores[0]
    system.load_binary(
        0,
        assemble("inc r4\nst.b r1, r2\nhalt"),
    )
    full.pc = 0
    full.regs[1] = SYSINFO_SINK
    full.regs[2] = 0xA5
    full.halted = False

    active_microcores = []
    for cluster_index, cluster in enumerate(
        system.clusters
    ):
        for local_index, cpu in enumerate(
            cluster.cores[:2]
        ):
            address = (
                0x100 +
                cluster_index * 0x200 +
                local_index * 0x80
            )
            system.load_binary(
                address,
                assemble(
                    f"inc r4\ncrc.mode {local_index}\nhalt"
                ),
            )
            cpu.pc = address
            cpu.halted = False
            active_microcores.append(cpu)

    trace = []
    full._mmio_write8 = (
        lambda address, value:
        trace.append(("earlier_full", address, value))
    )
    failure = RuntimeError(
        "reduced cluster callback failure oracle"
    )

    def fail_cluster_winner():
        winner = system.clusters[0].cores[1]
        assert system.clusters[0].crc_try_acquire(
            winner.core_id
        )
        trace.append(("failing_cluster", 0, 1))
        raise failure

    system.clusters[0].cores[1]._step_python_fallback = (
        fail_cluster_winner
    )
    later = system.clusters[1].cores[1]
    original_later_fallback = later._step_python_fallback

    def observe_later_cluster():
        trace.append(("later_cluster", 1, 1))
        return original_later_fallback()

    later._step_python_fallback = observe_later_cluster

    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(10)
    assert raised.value is failure

    arbiters = tuple(
        system._native_system._cluster_arbiter_snapshot(
            cluster_index
        )
        for cluster_index in range(2)
    )
    return (
        tuple(
            cpu.regs[4]
            for cpu in (full, *active_microcores)
        ),
        tuple(
            cpu.pc
            for cpu in (full, *active_microcores)
        ),
        tuple(
            cpu.cycle_count
            for cpu in (full, *active_microcores)
        ),
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(trace),
        tuple(
            (
                snapshot["grant_counts"]["crc"],
                snapshot["last_grants"]["crc"],
                snapshot["grant_sequence"],
                snapshot["crc_locked"],
                snapshot["crc_lock_owner"],
            )
            for snapshot in arbiters
        ),
    )


def test_reduced_callback_failure_preserves_prefixes_without_a_grant() -> None:
    signatures = {
        worker_count:
            _reduced_callback_failure_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == (1, 1, 1, 1, 1)
    assert reference[5] == (
        ("earlier_full", SYSINFO_SINK, 0xA5),
        ("failing_cluster", 0, 1),
    )
    assert reference[6] == (
        (0, 0, 0, False, -1),
        (0, 0, 0, False, -1),
    )
