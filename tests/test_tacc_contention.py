"""Cross-worker TACC ownership, contention, FORCE, and reset oracle.

RESOURCE-SAFETY NOTE: constructing ``MegapadSystem`` with worker counts two
and four starts native workers.  This whole file is therefore an explicitly
approved worker-spawning gate and must be run sequentially, never beside
another test suite.  Every batch below is bounded to at most four retired
instructions.
"""

from __future__ import annotations

from asm import assemble
from megapad64 import (
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    TACC_OWNER_NONE,
)
from system import MegapadSystem


# Static marker for resource-safety audits.  The comparison helper below is
# the only place this file constructs worker-backed systems.
WORKER_SPAWNING_GATE = True
SUPPORTED_WORKER_COUNTS = (1, 2, 4)


def _system(*, worker_count: int) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=4,
        num_clusters=3,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )


def _halt_all(system: MegapadSystem) -> None:
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False


def _status(cpu) -> tuple[bool, bool, bool, bool, bool, bool, int]:
    value = cpu.csr_read(CSR_TACC_STATUS)
    return (
        bool(value & (1 << 0)),
        bool(value & (1 << 1)),
        bool(value & (1 << 2)),
        bool(value & (1 << 3)),
        bool(value & (1 << 4)),
        bool(value & (1 << 9)),
        (value >> 16) & 0x1F,
    )


def _domain(state: dict) -> tuple:
    return (
        bytes(state["tacc"]),
        int(state["tacc_owner"]),
        bool(state["tacc_valid"]),
        bool(state["tacc_dirty"]),
        int(state["tacc_format_ew"]),
        int(state["tacc_format_signed"]),
        bool(state["tacc_busy"]),
        bool(state["tacc_force_pending"]),
        int(state["tacc_epoch"]),
    )


def _all_domains(system: MegapadSystem) -> tuple[tuple, ...]:
    full = tuple(
        _domain(dict(cpu._cs.tacc_snapshot()))
        for cpu in system.cores[:system.num_full_cores]
    )
    clusters = tuple(
        _domain(dict(cluster._shared_engine_snapshot()))
        for cluster in system.clusters
    )
    return full + clusters


def _is_wiped(domain: tuple) -> bool:
    (
        image,
        owner,
        valid,
        dirty,
        format_ew,
        format_signed,
        busy,
        force_pending,
        _epoch,
    ) = domain
    return (
        image == bytes(256)
        and owner == TACC_OWNER_NONE
        and not valid
        and not dirty
        and format_ew == 0
        and format_signed == 0
        and not busy
        and not force_pending
    )


def _batch_signature(stats) -> tuple:
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_scheduler,
        stats.native_rounds,
        stats.native_continuations,
        stats.system_stop_reason,
    )


def _write_distinct_domain_images(
    system: MegapadSystem,
    cluster_owners: tuple,
) -> None:
    for value, cpu in enumerate(
        system.cores[:system.num_full_cores],
        start=1,
    ):
        cpu.tacc[0] = value
        cpu.tacc_valid = True
        cpu.tacc_dirty = True
        cpu.tacc_format_ew = 0
        cpu.tacc_format_signed = 0

    for value, (cluster, owner) in enumerate(
        zip(system.clusters, cluster_owners),
        start=5,
    ):
        cluster.load_shared_engine_state(owner)
        owner.tacc[0] = value
        owner.tacc_valid = True
        owner.tacc_dirty = True
        owner.tacc_format_ew = 0
        owner.tacc_format_signed = 0
        assert cluster.store_shared_engine_state(owner)


def _contention_signature(worker_count: int) -> tuple:
    """Run one bounded production-topology contention scenario."""
    system = _system(worker_count=worker_count)
    for cluster in system.clusters:
        cluster.set_enabled(True)
        cluster.cl_priv_level = 0

    assert tuple(cpu.core_id for cpu in system.cores) == tuple(range(16))
    assert tuple(
        tuple(cpu.core_id for cpu in cluster.cores)
        for cluster in system.clusters
    ) == (
        (4, 5, 6, 7),
        (8, 9, 10, 11),
        (12, 13, 14, 15),
    )

    # All four full cores claim at one frontier.  Their private engines admit
    # independently; none of these requests participates in cluster RR.
    _halt_all(system)
    full_try = assemble("t.acc.try")
    system.load_binary(0x000, full_try)
    for cpu in system.cores[:4]:
        cpu.pc = 0x000
        cpu.halted = False

    full_claims = system.run_batch_stats(4)

    assert full_claims.instructions_executed == 4
    assert full_claims.per_core_instructions[:4] == (1, 1, 1, 1)
    assert full_claims.per_core_instructions[4:] == (0,) * 12
    assert tuple(_status(cpu)[:2] for cpu in system.cores[:4]) == (
        (True, True),
    ) * 4
    assert tuple(_status(cpu)[-1] for cpu in system.cores[:4]) == (
        0,
        1,
        2,
        3,
    )

    # A fresh cluster RR cursor selects local core 1 from two same-frontier
    # claimants.  Local core 0 remains pending, then retires a losing TRY and
    # immediately remains eligible for stateless MEX service.
    cluster_claim_records = []
    cluster_owners = []
    cluster_loser = None
    cluster0_owned = None
    cluster_program = assemble("t.acc.try\nt.add\nhalt")
    system.load_binary(0x100, cluster_program)
    for cluster_index, cluster in enumerate(system.clusters):
        _halt_all(system)
        loser, winner = cluster.cores[:2]
        loser.pc = 0x100
        winner.pc = 0x100
        loser.halted = False
        winner.halted = False

        claimed = system.run_batch_stats(1)
        arbiter = dict(
            system._native_system._cluster_arbiter_snapshot(
                cluster_index
            )
        )

        assert claimed.per_core_instructions[loser.core_id] == 0
        assert claimed.per_core_instructions[winner.core_id] == 1
        assert loser.pc == 0x100
        assert winner.pc == 0x100 + len(full_try)
        assert _status(loser)[:2] == (True, False)
        assert _status(winner)[:2] == (True, True)
        assert _status(loser)[-1] == winner.core_id
        assert _status(winner)[-1] == winner.core_id
        assert arbiter["grant_counts"]["tile_engine"] == 1
        assert arbiter["last_grants"]["tile_engine"] == 1
        assert arbiter["grant_sequence"] == 1

        cluster_claim_records.append(
            (
                _batch_signature(claimed),
                loser.core_id,
                winner.core_id,
                _status(loser),
                _status(winner),
                arbiter["grant_counts"]["tile_engine"],
                arbiter["last_grants"]["tile_engine"],
                arbiter["grant_sequence"],
            )
        )
        cluster_owners.append(winner)
        if cluster_index == 0:
            cluster_loser = loser
            cluster0_owned = _domain(
                dict(cluster._shared_engine_snapshot())
            )

    assert tuple(owner.core_id for owner in cluster_owners) == (5, 9, 13)
    assert cluster_loser is not None
    assert cluster0_owned is not None

    _halt_all(system)
    cluster_loser.tsrc0 = 0x600
    cluster_loser.tsrc1 = 0x640
    cluster_loser.tdst = 0x680
    cluster_loser.mem[0x600:0x640] = bytes([2]) * 64
    cluster_loser.mem[0x640:0x680] = bytes([5]) * 64
    cluster_loser.halted = False

    losing_try = system.run_batch_stats(1)

    assert losing_try.per_core_instructions[cluster_loser.core_id] == 1
    assert cluster_loser.pc == 0x100 + len(full_try)
    assert _status(cluster_loser)[:2] == (True, False)
    assert _status(cluster_loser)[-1] == cluster_owners[0].core_id
    assert _domain(
        dict(system.clusters[0]._shared_engine_snapshot())
    ) == cluster0_owned

    stateless_mex = system.run_batch_stats(1)
    cluster0_arbiter = dict(
        system._native_system._cluster_arbiter_snapshot(0)
    )

    assert stateless_mex.per_core_instructions[cluster_loser.core_id] == 1
    assert bytes(cluster_loser.mem[0x680:0x6C0]) == bytes([7]) * 64
    assert _domain(
        dict(system.clusters[0]._shared_engine_snapshot())
    ) == cluster0_owned
    assert cluster0_arbiter["grant_counts"]["tile_engine"] == 3
    assert cluster0_arbiter["last_grants"]["tile_engine"] == 0
    assert cluster0_arbiter["grant_sequence"] == 3

    # A privileged FORCE write and a new claim arrive at the same frontier.
    # FORCE wins, wipes the physical bank, and fences only the stateful claim;
    # an unrelated nonowner MEX still retires at that frontier.
    cluster0 = system.clusters[0]
    old_owner = cluster_owners[0]
    system.load_binary(0x200, assemble("t.acc.clear"))
    old_owner.pc = 0x200
    old_owner.halted = False
    old_owner.step()
    old_owner.halted = True
    cluster0.load_shared_engine_state(old_owner)
    old_owner.tacc[0] = 0xA5
    old_owner.tacc_valid = True
    old_owner.tacc_dirty = True
    assert cluster0.store_shared_engine_state(old_owner)

    claimant, _old_owner, force_writer, legacy_mex = cluster0.cores
    force_address = 0x280
    claim_address = 0x300
    mex_address = 0x380
    system.load_binary(
        force_address,
        assemble(f"csrw {CSR_TACC_CTL}, r1\nhalt"),
    )
    system.load_binary(claim_address, assemble("t.acc.try\nhalt"))
    system.load_binary(mex_address, assemble("t.add\nhalt"))
    _halt_all(system)
    force_writer.pc = force_address
    force_writer.regs[1] = 1
    force_writer.halted = False
    claimant.pc = claim_address
    claimant.halted = False
    legacy_mex.pc = mex_address
    legacy_mex.tsrc0 = 0x800
    legacy_mex.tsrc1 = 0x840
    legacy_mex.tdst = 0x880
    legacy_mex.mem[0x800:0x840] = bytes([3]) * 64
    legacy_mex.mem[0x840:0x880] = bytes([4]) * 64
    legacy_mex.halted = False
    # Put the claimant first in ordinary global scan order.  A passing test
    # therefore demonstrates FORCE priority, rather than merely arranging
    # for the control write to be visited before the stateful request.
    system._scheduler_cursor = claimant.core_id
    pre_force_arbiter = dict(
        system._native_system._cluster_arbiter_snapshot(0)
    )

    forced = system.run_batch_stats(2)
    released = _domain(dict(cluster0._shared_engine_snapshot()))
    post_force_arbiter = dict(
        system._native_system._cluster_arbiter_snapshot(0)
    )

    assert forced.per_core_instructions[force_writer.core_id] == 1
    assert forced.per_core_instructions[legacy_mex.core_id] == 1
    assert forced.per_core_instructions[claimant.core_id] == 0
    assert claimant.pc == claim_address
    assert bytes(legacy_mex.mem[0x880:0x8C0]) == bytes([7]) * 64
    assert _is_wiped(released)
    assert (
        post_force_arbiter["grant_counts"]["tile_engine"]
        == pre_force_arbiter["grant_counts"]["tile_engine"] + 1
    )
    assert post_force_arbiter["last_grants"]["tile_engine"] == 3

    force_writer.halted = True
    legacy_mex.halted = True
    admitted = system.run_batch_stats(1)

    assert admitted.per_core_instructions[claimant.core_id] == 1
    assert claimant.pc == claim_address + len(full_try)
    assert _status(claimant)[:2] == (True, True)
    assert _status(claimant)[-1] == claimant.core_id == 4
    cluster_owners[0] = claimant

    # Seed all seven physical banks distinctly, then reset one domain at a
    # time.  Full-core reset wipes only its paired engine; microcore reset
    # advances only that caller's cancellation epoch; cluster disable/reset
    # wipes only the selected cluster engine.
    _write_distinct_domain_images(system, tuple(cluster_owners))
    baseline = _all_domains(system)
    assert tuple(domain[0][0] for domain in baseline) == tuple(range(1, 8))
    assert tuple(domain[1] for domain in baseline) == (
        0,
        1,
        2,
        3,
        4,
        9,
        13,
    )

    system.cores[0]._reset_state()
    after_full_reset = _all_domains(system)
    assert _is_wiped(after_full_reset[0])
    assert after_full_reset[1:] == baseline[1:]

    caller_epochs = tuple(
        system._native_system._cluster_tacc_caller_epochs_snapshot(0)
    )
    claimant._reset_state()
    after_micro_reset = _all_domains(system)
    next_caller_epochs = tuple(
        system._native_system._cluster_tacc_caller_epochs_snapshot(0)
    )
    assert next_caller_epochs[0] == caller_epochs[0] + 1
    assert next_caller_epochs[1:] == caller_epochs[1:]
    assert after_micro_reset == after_full_reset

    system.clusters[1].set_enabled(False)
    after_cluster_disable = _all_domains(system)
    assert _is_wiped(after_cluster_disable[5])
    assert (
        after_cluster_disable[:5] + after_cluster_disable[6:]
        == after_micro_reset[:5] + after_micro_reset[6:]
    )

    system.clusters[2].reset_shared_resources()
    after_cluster_reset = _all_domains(system)
    assert _is_wiped(after_cluster_reset[6])
    assert after_cluster_reset[:6] == after_cluster_disable[:6]

    return (
        _batch_signature(full_claims),
        tuple(cluster_claim_records),
        _batch_signature(losing_try),
        _batch_signature(stateless_mex),
        bytes(cluster_loser.mem[0x680:0x6C0]),
        (
            cluster0_arbiter["grant_counts"]["tile_engine"],
            cluster0_arbiter["last_grants"]["tile_engine"],
            cluster0_arbiter["grant_sequence"],
        ),
        _batch_signature(forced),
        _batch_signature(admitted),
        released,
        baseline,
        after_full_reset,
        after_micro_reset,
        after_cluster_disable,
        after_cluster_reset,
        caller_epochs,
        next_caller_epochs,
    )


def test_tacc_contention_is_exact_across_supported_worker_counts() -> None:
    signatures = {
        worker_count: _contention_signature(worker_count)
        for worker_count in SUPPORTED_WORKER_COUNTS
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
