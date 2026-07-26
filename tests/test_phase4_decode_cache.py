"""Phase 4 Element 4 host decode/admission-cache oracles."""

from __future__ import annotations

from asm import assemble
from megapad64 import (
    Megapad64 as PythonMegapad64,
    Megapad64Micro as PythonMegapad64Micro,
)
from system import MegapadSystem


VARIABLE_PRIVATE_LOOP = assemble(
    """
loop:
    lhi r1, 0x1234
    ldi r2, 0x56
    add r6, r2
    xor r4, r6
    roli r4, 7
    addi r5, 1
    br loop
"""
)
VARIABLE_PRIVATE_STEPS = 257
LINE_BYTES = 16


def _system(*, worker_count: int) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
        cpu.flag_i = 0
    return system


def _core_signature(cpu) -> tuple:
    return (
        cpu.pc,
        tuple(cpu.regs),
        cpu.psel,
        cpu.xsel,
        cpu.spsel,
        cpu.flags_pack(),
        cpu.d_reg,
        cpu.q_out,
        cpu.t_reg,
        cpu.cycle_count,
        cpu.halted,
        cpu.idle,
    )


def _python_variable_loop_signature(*, profile: str) -> tuple:
    if profile == "full":
        cpu = PythonMegapad64(
            mem_size=4096,
            core_id=0,
            num_cores=5,
        )
    elif profile == "micro":
        cpu = PythonMegapad64Micro(
            mem_size=4096,
            core_id=1,
            num_cores=5,
        )
    else:
        raise AssertionError(f"unknown core profile {profile!r}")
    cpu.mem[:len(VARIABLE_PRIVATE_LOOP)] = VARIABLE_PRIVATE_LOOP
    cpu.pc = 0
    cpu.halted = False
    cpu.idle = False
    cpu.flag_i = 0
    for _ in range(VARIABLE_PRIVATE_STEPS):
        cpu.step()
    return _core_signature(cpu)


def _native_variable_loop_signature(
    worker_count: int,
    *,
    profile: str,
) -> tuple:
    system = _system(worker_count=worker_count)
    system.load_binary(0, VARIABLE_PRIVATE_LOOP)
    if profile == "full":
        active = system.cores[0]
    elif profile == "micro":
        active = system.clusters[0].cores[0]

        def reject_python_fallback():
            raise AssertionError(
                "private decode-cache corpus reached Python fallback"
            )

        active._step_python_fallback_in_memory_scope = (
            reject_python_fallback
        )
    else:
        raise AssertionError(f"unknown core profile {profile!r}")
    active.pc = 0
    active.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    stats = system.run_batch_stats(VARIABLE_PRIVATE_STEPS)
    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    cache_counts = (
        counts["private_decode_cache_lookups"],
        counts["private_decode_cache_hits"],
        counts["private_decode_cache_misses"],
        counts["frontier_decode_cache_lookups"],
        counts["frontier_decode_cache_hits"],
        counts["frontier_decode_cache_misses"],
        counts["micro_oracle_proof_reuses"],
    )
    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.system_stop_reason,
        owner.system_cycles,
        system._scheduler_cursor,
        _core_signature(active),
        bytes(system.cpu.mem[:len(VARIABLE_PRIVATE_LOOP)]),
    )
    return architectural, cache_counts


def test_reused_variable_length_decode_matches_python_across_lanes() -> None:
    for profile in ("full", "micro"):
        oracle = _python_variable_loop_signature(
            profile=profile
        )
        signatures = {
            worker_count:
                _native_variable_loop_signature(
                    worker_count,
                    profile=profile,
                )
            for worker_count in (1, 2, 4)
        }

        assert signatures[2] == signatures[1]
        assert signatures[4] == signatures[1]
        architectural, cache_counts = signatures[1]
        assert architectural[0] == VARIABLE_PRIVATE_STEPS
        assert architectural[11] == oracle
        (
            private_lookups,
            private_hits,
            private_misses,
            frontier_lookups,
            frontier_hits,
            frontier_misses,
            proof_reuses,
        ) = cache_counts
        assert private_lookups == private_hits + private_misses
        assert frontier_lookups == frontier_hits + frontier_misses
        assert private_hits + frontier_hits > 0
        assert proof_reuses == (
            VARIABLE_PRIVATE_STEPS
            if profile == "micro"
            else 0
        )


def _full_icache_identity_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    address = 0x100
    old_instruction = assemble("inc r4")
    new_instruction = assemble("inc r5")
    system.load_binary(address, old_instruction)
    full = system.cores[0]
    full.pc = address
    full.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    batches = []

    def execute_once() -> None:
        full.pc = address
        stats = system.run_batch_stats(1)
        batches.append((
            stats.instructions_executed,
            stats.system_cycles_advanced,
            stats.per_core_instructions,
            stats.per_core_cycles,
            stats.per_core_stop_reasons,
        ))

    execute_once()  # cold fill and old instruction
    execute_once()  # create the private host plan
    old_cache = full._cs.icache_snapshot()

    # Host mutation changes backing RAM but intentionally does not snoop this
    # full core's guest I-cache or its byte-validated host plan.
    system.load_binary(address, new_instruction)
    execute_once()

    # Explicit invalidation observes new backing code. Restoring the old guest
    # cache snapshot must then restore its old instruction observation too.
    full._cs.icache_control_write(3)
    execute_once()
    full._cs.icache_restore(*old_cache)
    execute_once()

    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    cache_counts = (
        counts["frontier_decode_cache_lookups"],
        counts["frontier_decode_cache_hits"],
        counts["frontier_decode_cache_misses"],
        counts["private_decode_cache_lookups"],
        counts["coordinator_boundaries"],
    )
    architectural = (
        tuple(batches),
        _core_signature(full),
        system.cpu.mem[address],
        full._cs.icache_snapshot(),
        owner.system_cycles,
        system._scheduler_cursor,
    )
    return architectural, cache_counts


def test_full_decode_identity_follows_guest_icache_and_restore() -> None:
    signatures = {
        worker_count:
            _full_icache_identity_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    architectural, cache_counts = signatures[1]
    assert architectural[1][1][4:6] == (4, 1)
    assert architectural[2] == assemble("inc r5")[0]
    assert cache_counts == (5, 1, 4, 0, 2)

    trailing_signatures = {
        worker_count:
            _full_trailing_identity_after_eviction_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }
    assert trailing_signatures[2] == trailing_signatures[1]
    assert trailing_signatures[4] == trailing_signatures[1]
    trailing_architectural, trailing_counts = (
        trailing_signatures[1]
    )
    assert trailing_architectural[0] == (
        0x34,
        0x34,
        0x34,
        0x34,
        0x56,
        0x56,
        0x56,
    )
    assert trailing_architectural[1] == assemble(
        "ldi r4, 0x56"
    )
    assert trailing_counts == (7, 2, 5, 0, 3)


def _full_trailing_identity_after_eviction_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    address = LINE_BYTES - 2
    collision_address = 4096 + LINE_BYTES + 1
    initial = assemble("ldi r4, 0x34")
    replacement = assemble("ldi r4, 0x56")
    assert initial[:-1] == replacement[:-1]
    system.load_binary(address, initial)
    # This physical address wraps to the same backing RAM but carries a
    # different guest I-cache tag for the line containing the immediate.
    system.load_binary(collision_address, assemble("nop"))
    full = system.cores[0]
    full.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    values = []

    def execute_once(instruction_address: int) -> None:
        full.pc = instruction_address
        stats = system.run_batch_stats(1)
        assert stats.instructions_executed == 1
        values.append(full.regs[4])

    execute_once(address)  # cold guest-cache fill
    execute_once(address)  # populate the host admission entry
    system.load_binary(
        address + len(initial) - 1,
        replacement[-1:],
    )
    execute_once(address)  # stale guest cache and matching host entry
    execute_once(collision_address)  # replace the immediate's cache-line tag
    execute_once(address)  # guest-cache refill observes replacement bytes
    execute_once(address)  # old host identity rejects the new final byte
    execute_once(address)  # replacement host identity now hits

    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    architectural = (
        tuple(values),
        bytes(
            system.cpu.mem[
                address:address + len(initial)
            ]
        ),
        _core_signature(full),
        owner.system_cycles,
        system._scheduler_cursor,
    )
    cache_counts = (
        counts["frontier_decode_cache_lookups"],
        counts["frontier_decode_cache_hits"],
        counts["frontier_decode_cache_misses"],
        counts["private_decode_cache_lookups"],
        counts["coordinator_boundaries"],
    )
    return architectural, cache_counts


def _cross_line_immediate_mutation_signature(
    worker_count: int,
    *,
    profile: str,
) -> tuple:
    system = _system(worker_count=worker_count)
    address = LINE_BYTES - 2
    initial = assemble("ldi r4, 0x34")
    replacement = assemble("ldi r4, 0x56")
    assert len(initial) == len(replacement) == 3
    assert initial[:-1] == replacement[:-1]
    assert address + len(initial) - 1 == LINE_BYTES
    system.load_binary(address, initial)
    if profile == "full":
        active = system.cores[0]
        active._cs.icache_enabled = 0
    elif profile == "micro":
        active = system.clusters[0].cores[0]
    else:
        raise AssertionError(f"unknown core profile {profile!r}")
    active.pc = address
    active.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    first = system.run_batch_stats(1)
    first_value = active.regs[4]
    active.pc = address
    system.load_binary(
        address + len(initial) - 1,
        replacement[-1:],
    )
    second = system.run_batch_stats(1)
    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    architectural = (
        first.instructions_executed,
        first.per_core_instructions,
        second.instructions_executed,
        second.per_core_instructions,
        first_value,
        active.regs[4],
        active.pc,
        bytes(
            system.cpu.mem[
                address:address + len(initial)
            ]
        ),
        _core_signature(active),
    )
    cache_counts = (
        counts["private_decode_cache_lookups"],
        counts["private_decode_cache_hits"],
        counts["private_decode_cache_misses"],
        counts["frontier_decode_cache_lookups"],
        counts["micro_oracle_proof_reuses"],
        counts["coordinator_boundaries"],
    )
    return architectural, cache_counts


def test_final_immediate_mutation_is_observed_across_cache_line() -> None:
    for profile in ("full", "micro"):
        signatures = {
            worker_count:
                _cross_line_immediate_mutation_signature(
                    worker_count,
                    profile=profile,
                )
            for worker_count in (1, 2, 4)
        }

        assert signatures[2] == signatures[1]
        assert signatures[4] == signatures[1]
        architectural, cache_counts = signatures[1]
        assert architectural[4:6] == (0x34, 0x56)
        assert architectural[7] == assemble("ldi r4, 0x56")
        assert cache_counts == (
            (0, 0, 0, 0, 0, 2)
            if profile == "full"
            else (2, 0, 2, 0, 2, 0)
        )
