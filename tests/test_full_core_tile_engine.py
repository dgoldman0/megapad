"""Physical tile-engine topology and caller-private configuration oracles."""

from __future__ import annotations

import pytest

from asm import assemble
from megapad64 import (
    CSR_ACC0,
    CSR_SB,
    CSR_SC,
    CSR_SR,
    CSR_SW,
    CSR_TCTRL,
    CSR_TDST,
    CSR_TMODE,
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    CSR_TSRC0,
    CSR_TSRC1,
    CSR_TSTRIDE_C,
    CSR_TSTRIDE_R,
    CSR_TTILE_H,
    CSR_TTILE_W,
    IVEC_ILLEGAL_OP,
    IVEC_PRIV_FAULT,
    TrapError,
    TACC_OWNER_NONE,
)
from system import MegapadSystem, MicroCluster


def _system(*, full_cores: int = 4, clusters: int = 3) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )


def _cluster_tacc_domain(cluster: MicroCluster) -> dict:
    return {
        name: value
        for name, value in cluster._shared_engine_snapshot().items()
        if name.startswith("tacc")
    }


def test_production_topology_has_four_full_core_and_three_cluster_domains():
    system = _system()

    assert [cpu.core_id for cpu in system.cores[:4]] == [0, 1, 2, 3]
    assert [
        [cpu.core_id for cpu in cluster.cores]
        for cluster in system.clusters
    ] == [
        [4, 5, 6, 7],
        [8, 9, 10, 11],
        [12, 13, 14, 15],
    ]
    assert [
        dict(system._native_system._cluster_tile_snapshot(index))
        for index in range(3)
    ] == [
        dict(cluster._shared_engine_snapshot())
        for cluster in system.clusters
    ]
    assert set(system.clusters[0]._shared_engine_snapshot()) == {
        "acc",
        "tacc",
        "tacc_owner",
        "tacc_valid",
        "tacc_dirty",
        "tacc_format_ew",
        "tacc_format_signed",
        "tacc_busy",
        "tacc_force_pending",
        "tacc_epoch",
        "sha_mode",
        "sha_msglen_lo",
        "sha_msglen_hi",
    }


def test_every_full_core_keeps_private_configuration_and_accumulator():
    system = _system(clusters=0)

    for index, cpu in enumerate(system.cores):
        cpu.csr_write(CSR_TSRC0, 0x100 + index * 0x40)
        cpu.csr_write(CSR_TMODE, index)
        cpu.csr_write(CSR_ACC0, 0xA000 + index)

    assert [
        (
            cpu.csr_read(CSR_TSRC0),
            cpu.csr_read(CSR_TMODE),
            cpu.csr_read(CSR_ACC0),
        )
        for cpu in system.cores
    ] == [
        (0x100, 0, 0xA000),
        (0x140, 1, 0xA001),
        (0x180, 2, 0xA002),
        (0x1C0, 3, 0xA003),
    ]


def test_all_four_full_cores_execute_mex_on_their_private_engine():
    system = _system(clusters=0)

    for index, cpu in enumerate(system.cores):
        code_address = index * 0x20
        data_address = 0x200 + index * 0xC0
        system.load_binary(code_address, assemble("t.add\nhalt"))
        cpu.pc = code_address
        cpu.tsrc0 = data_address
        cpu.tsrc1 = data_address + 0x40
        cpu.tdst = data_address + 0x80
        cpu.mem[data_address:data_address + 0x40] = bytes([index]) * 64
        cpu.mem[data_address + 0x40:data_address + 0x80] = (
            bytes([0x10 + index]) * 64
        )

    stats = system.run_batch_stats(4)

    assert stats.per_core_instructions == (1, 1, 1, 1)
    for index, cpu in enumerate(system.cores):
        data_address = 0x200 + index * 0xC0
        assert bytes(
            cpu.mem[data_address + 0x80:data_address + 0xC0]
        ) == bytes([0x10 + 2 * index]) * 64


def test_microcore_configuration_is_private_but_acc_is_cluster_shared():
    system = _system(full_cores=1, clusters=2)
    first_cluster, other_cluster = system.clusters
    first_cluster.set_enabled(True)
    other_cluster.set_enabled(True)

    private_values = (
        (CSR_SB, 1, 2),
        (CSR_SR, 0x101, 0x201),
        (CSR_SC, 0x102, 0x202),
        (CSR_SW, 0x103, 0x203),
        (CSR_TMODE, 0x00, 0x01),
        (CSR_TCTRL, 0x02, 0x03),
        (CSR_TSRC0, 0x400, 0x800),
        (CSR_TSRC1, 0x440, 0x840),
        (CSR_TDST, 0x480, 0x880),
        (CSR_TSTRIDE_R, 0x20, 0x40),
        (CSR_TSTRIDE_C, 0x04, 0x08),
        (CSR_TTILE_H, 3, 6),
        (CSR_TTILE_W, 16, 32),
    )
    first, sibling = first_cluster.cores[:2]
    for csr, first_value, sibling_value in private_values:
        first.csr_write(csr, first_value)
        sibling.csr_write(csr, sibling_value)

    for csr, first_value, sibling_value in private_values:
        assert first.csr_read(csr) == first_value
        assert sibling.csr_read(csr) == sibling_value

    first.csr_write(CSR_ACC0, 0xCAFE_BABE)

    assert first.csr_read(CSR_ACC0) == 0xCAFE_BABE
    assert sibling.csr_read(CSR_ACC0) == 0xCAFE_BABE
    assert other_cluster.cores[0].csr_read(CSR_ACC0) == 0


def test_reduced_system_ids_remain_compact_and_cluster_domains_stay_distinct():
    system = _system(full_cores=2, clusters=2)
    for cluster in system.clusters:
        cluster.set_enabled(True)

    assert [cpu.core_id for cpu in system.cores] == list(range(10))
    assert [
        [cpu.core_id for cpu in cluster.cores]
        for cluster in system.clusters
    ] == [[2, 3, 4, 5], [6, 7, 8, 9]]

    system.clusters[0].cores[0].csr_write(CSR_ACC0, 0x1111)
    system.clusters[1].cores[0].csr_write(CSR_ACC0, 0x2222)

    assert {
        cpu.csr_read(CSR_ACC0)
        for cpu in system.clusters[0].cores
    } == {0x1111}
    assert {
        cpu.csr_read(CSR_ACC0)
        for cpu in system.clusters[1].cores
    } == {0x2222}


def test_all_seven_physical_tacc_domains_can_be_claimed_independently():
    system = _system()
    system.load_binary(0, assemble("t.acc.try"))
    for cluster in system.clusters:
        cluster.set_enabled(True)

    for cpu in system.cores[:4]:
        cpu.pc = 0
        cpu.step()
    for cluster in system.clusters:
        cluster.cores[0].pc = 0
        cluster.cores[0].step()

    for cpu in system.cores[:4]:
        status = cpu.csr_read(CSR_TACC_STATUS)
        assert status & 0b11 == 0b11
        assert (status >> 16) & 0x1F == cpu.core_id

    for cluster in system.clusters:
        owner, sibling = cluster.cores[:2]
        owner_status = owner.csr_read(CSR_TACC_STATUS)
        sibling_status = sibling.csr_read(CSR_TACC_STATUS)
        assert owner_status & 0b11 == 0b11
        assert sibling_status & 0b11 == 0b01
        assert (owner_status >> 16) & 0x1F == owner.core_id
        assert (sibling_status >> 16) & 0x1F == owner.core_id

    for index, cpu in enumerate(system.cores[:4], start=1):
        cpu.tacc[0] = index
        cpu.csr_write(CSR_ACC0, 0x100 + index)
    for index, cluster in enumerate(system.clusters, start=5):
        owner = cluster.cores[0]
        cluster.load_shared_engine_state(owner)
        owner.tacc[0] = index
        owner.tacc_valid = True
        owner.tacc_dirty = True
        owner.tacc_format_ew = 0
        cluster.store_shared_engine_state(owner)
        owner.csr_write(CSR_ACC0, 0x100 + index)

    assert [cpu.tacc[0] for cpu in system.cores[:4]] + [
        cluster._shared_engine_snapshot()["tacc"][0]
        for cluster in system.clusters
    ] == list(range(1, 8))
    assert [
        cpu.csr_read(CSR_ACC0)
        for cpu in system.cores[:4]
    ] + [
        cluster.cores[0].csr_read(CSR_ACC0)
        for cluster in system.clusters
    ] == list(range(0x101, 0x108))


def test_same_frontier_full_core_claims_use_four_private_tacc_engines():
    system = _system(clusters=0)
    addresses = (0x00, 0x40, 0x80, 0xC0)
    instruction = assemble("t.acc.try")

    for cpu, address in zip(system.cores, addresses):
        system.load_binary(address, instruction)
        cpu.pc = address
        cpu.halted = False
        cpu.idle = False

    stats = system.run_batch_stats(4)

    assert stats.native_scheduler
    assert stats.native_rounds == 1
    assert stats.per_core_instructions == (1, 1, 1, 1)
    assert stats.per_core_cycles == (2, 2, 2, 2)
    assert tuple(cpu.pc for cpu in system.cores) == tuple(
        address + len(instruction)
        for address in addresses
    )
    for cpu in system.cores:
        status = cpu.csr_read(CSR_TACC_STATUS)
        assert status & 0b11 == 0b11
        assert status & (1 << 4) == 0
        assert (status >> 16) & 0x1F == cpu.core_id


def test_competing_cluster_try_uses_rr_and_failed_claim_keeps_mex_eligible():
    system = _system(full_cores=1, clusters=1)
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    loser, winner = cluster.cores[:2]
    loser_address = 0x100
    winner_address = 0x180
    try_size = len(assemble("t.acc.try"))

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    system.load_binary(
        loser_address,
        assemble("t.acc.try\nt.add\nhalt"),
    )
    system.load_binary(
        winner_address,
        assemble("t.acc.try\nhalt"),
    )
    loser.pc = loser_address
    loser.halted = False
    winner.pc = winner_address
    winner.halted = False
    loser.tsrc0 = 0x300
    loser.tsrc1 = 0x340
    loser.tdst = 0x380
    loser.mem[0x300:0x340] = bytes([2]) * 64
    loser.mem[0x340:0x380] = bytes([5]) * 64

    claimed = system.run_batch_stats(1)

    assert claimed.per_core_instructions[winner.core_id] == 1
    assert loser.pc == loser_address
    assert winner.pc == winner_address + try_size
    assert (winner.csr_read(CSR_TACC_STATUS) >> 16) & 0x1F == winner.core_id
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 1
    assert arbiter["last_grants"]["tile_engine"] == 1

    winner.halted = True
    owned = _cluster_tacc_domain(cluster)
    failed = system.run_batch_stats(1)

    assert failed.system_stop_reason == "instruction_limit"
    assert failed.per_core_instructions[loser.core_id] == 1
    assert loser.pc == loser_address + try_size
    assert loser.csr_read(CSR_TACC_STATUS) & 0b11 == 0b01
    assert _cluster_tacc_domain(cluster) == owned
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 2
    assert arbiter["last_grants"]["tile_engine"] == 0

    stateless = system.run_batch_stats(1)

    assert stateless.per_core_instructions[loser.core_id] == 1
    assert bytes(loser.mem[0x380:0x3C0]) == bytes([7]) * 64
    assert _cluster_tacc_domain(cluster) == owned
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 3
    assert arbiter["last_grants"]["tile_engine"] == 0


def test_same_frontier_force_fences_tacc_but_not_stateless_mex():
    system = _system(full_cores=1, clusters=1)
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    cluster.cl_priv_level = 0
    claimant, force_writer, legacy_mex, owner = cluster.cores

    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0x20, assemble(instruction))
        owner.pc = 0x20
        owner.halted = False
        owner.step()

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    force_address = 0x100
    legacy_address = 0x180
    claim_address = 0x200
    system.load_binary(
        force_address,
        assemble(f"csrw {CSR_TACC_CTL}, r1\nhalt"),
    )
    system.load_binary(legacy_address, assemble("t.add\nhalt"))
    system.load_binary(claim_address, assemble("t.acc.try\nhalt"))
    force_writer.pc = force_address
    force_writer.regs[1] = 1
    force_writer.halted = False
    legacy_mex.pc = legacy_address
    legacy_mex.halted = False
    legacy_mex.tsrc0 = 0x300
    legacy_mex.tsrc1 = 0x340
    legacy_mex.tdst = 0x380
    legacy_mex.mem[0x300:0x340] = bytes([3]) * 64
    legacy_mex.mem[0x340:0x380] = bytes([4]) * 64
    claimant.pc = claim_address
    claimant.halted = False

    fenced = system.run_batch_stats(2)

    assert fenced.per_core_instructions[force_writer.core_id] == 1
    assert fenced.per_core_instructions[legacy_mex.core_id] == 1
    assert fenced.per_core_instructions[claimant.core_id] == 0
    assert claimant.pc == claim_address
    assert bytes(legacy_mex.mem[0x380:0x3C0]) == bytes([7]) * 64
    released = _cluster_tacc_domain(cluster)
    assert released["tacc_owner"] == TACC_OWNER_NONE
    assert not released["tacc_valid"]
    assert not released["tacc_busy"]
    assert not released["tacc_force_pending"]
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 1
    assert arbiter["last_grants"]["tile_engine"] == 2

    admitted = system.run_batch_stats(1)

    assert admitted.per_core_instructions[claimant.core_id] == 1
    assert claimant.pc == claim_address + len(assemble("t.acc.try"))
    status = claimant.csr_read(CSR_TACC_STATUS)
    assert status & 0b11 == 0b11
    assert (status >> 16) & 0x1F == claimant.core_id
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 2
    assert arbiter["last_grants"]["tile_engine"] == 0


def test_granted_nonowner_store_faults_before_memory_or_stage_mutation():
    system = _system(full_cores=1, clusters=1)
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    nonowner, owner = cluster.cores[:2]
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.halted = False
        owner.step()

    cluster.load_shared_engine_state(owner)
    image = bytearray(owner.tacc)
    image[0] = 0xA5
    owner.tacc = bytes(image)
    assert cluster.store_shared_engine_state(owner)

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    destination = 0x400
    sentinel = bytes([0xCC]) * 256
    nonowner.mem[destination:destination + 256] = sentinel
    nonowner.tdst = destination
    nonowner.perf_enable = 1
    system.load_binary(0x100, assemble("t.acc.store"))
    nonowner.pc = 0x100
    nonowner.halted = False
    nonowner.ivt_base = 0x800
    nonowner.sp = 0xF00
    handler = 0xA00
    vector = nonowner.ivt_base + IVEC_ILLEGAL_OP * 8
    nonowner.mem[vector:vector + 8] = handler.to_bytes(8, "little")
    destination_writes = []
    original_write8 = nonowner.mem_write8

    def observe_write(address: int, value: int):
        if destination <= address < destination + 256:
            destination_writes.append((address, value))
        return original_write8(address, value)

    nonowner.mem_write8 = observe_write
    before_tacc = _cluster_tacc_domain(cluster)
    before_stage = dict(
        system._native_system._tacc_image_stage_snapshot()
    )

    stats = system.run_batch_stats(1)

    assert stats.native_scheduler
    assert nonowner.ivec_id == IVEC_ILLEGAL_OP
    assert nonowner.pc == handler
    assert nonowner.perf_tileops == 0
    assert destination_writes == []
    assert bytes(nonowner.mem[destination:destination + 256]) == sentinel
    assert _cluster_tacc_domain(cluster) == before_tacc
    assert dict(
        system._native_system._tacc_image_stage_snapshot()
    ) == before_stage
    arbiter = system._native_system._cluster_arbiter_snapshot(0)
    assert arbiter["grant_counts"]["tile_engine"] == 1
    assert arbiter["last_grants"]["tile_engine"] == 0


def test_acc_zero_is_sampled_and_cleared_only_on_the_granted_caller():
    system = _system(full_cores=1, clusters=1)
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    granted, sibling = cluster.cores[:2]
    system.load_binary(0, assemble("t.sum"))

    granted_private = (
        (CSR_SB, 0x3),
        (CSR_SR, 0x11),
        (CSR_SC, 0x12),
        (CSR_SW, 0x13),
        (CSR_TMODE, 0),
        (CSR_TSRC0, 0x100),
        (CSR_TSRC1, 0x140),
        (CSR_TDST, 0x180),
        (CSR_TSTRIDE_R, 0x20),
        (CSR_TSTRIDE_C, 0x4),
        (CSR_TTILE_H, 4),
        (CSR_TTILE_W, 16),
    )
    for csr, value in granted_private:
        granted.csr_write(csr, value)
    granted.csr_write(CSR_TCTRL, 0x2)
    sibling.csr_write(CSR_TSRC0, 0x500)
    sibling.csr_write(CSR_TSRC1, 0x540)
    sibling.csr_write(CSR_TDST, 0x580)
    sibling.csr_write(CSR_TMODE, 1)
    sibling.csr_write(CSR_TCTRL, 0x2)
    granted.csr_write(CSR_ACC0, 0xDEAD)
    granted.mem[0x100:0x140] = bytes([1] * 64)
    granted.pc = 0

    granted.step()

    assert granted.csr_read(CSR_TCTRL) == 0
    for csr, value in granted_private:
        assert granted.csr_read(csr) == value
    assert sibling.csr_read(CSR_TCTRL) == 0x2
    assert sibling.csr_read(CSR_TSRC0) == 0x500
    assert sibling.csr_read(CSR_TSRC1) == 0x540
    assert sibling.csr_read(CSR_TDST) == 0x580
    assert sibling.csr_read(CSR_TMODE) == 1
    assert granted.csr_read(CSR_ACC0) == 64
    assert sibling.csr_read(CSR_ACC0) == 64


def test_reset_scopes_follow_physical_engine_ownership():
    system = _system(full_cores=2, clusters=2)
    first_cluster, second_cluster = system.clusters
    first_cluster.set_enabled(True)
    second_cluster.set_enabled(True)
    system.load_binary(0, assemble("t.acc.try"))
    full0, full1 = system.cores[:2]
    owner, sibling = first_cluster.cores[:2]
    other_owner = second_cluster.cores[0]

    for cpu in (full0, full1, owner, other_owner):
        cpu.pc = 0
        cpu.step()

    full0.tacc[0] = 1
    full1.tacc[0] = 2
    for cpu in (full0, full1):
        cpu.tacc_valid = True
        cpu.tacc_dirty = True
    for value, cluster, cluster_owner in (
        (3, first_cluster, owner),
        (4, second_cluster, other_owner),
    ):
        cluster.load_shared_engine_state(cluster_owner)
        cluster_owner.tacc[0] = value
        cluster_owner.tacc_valid = True
        cluster_owner.tacc_dirty = True
        cluster.store_shared_engine_state(cluster_owner)

    full0._reset_state()

    assert full0.csr_read(CSR_TACC_STATUS) & 1 == 0
    assert full1.csr_read(CSR_TACC_STATUS) & 0b11 == 0b11
    assert full1.csr_read(CSR_TACC_STATUS) & 0xF == 0xF
    assert full1.tacc[0] == 2
    assert first_cluster._shared_engine_snapshot()["tacc"][0] == 3
    assert second_cluster._shared_engine_snapshot()["tacc"][0] == 4
    assert owner.csr_read(CSR_TACC_STATUS) & 0xF == 0xF
    assert other_owner.csr_read(CSR_TACC_STATUS) & 0xF == 0xF

    owner._reset_state()

    assert sibling.csr_read(CSR_TACC_STATUS) & 1
    assert (sibling.csr_read(CSR_TACC_STATUS) >> 16) & 0x1F == owner.core_id
    assert first_cluster._shared_engine_snapshot()["tacc"][0] == 3
    assert sibling.csr_read(CSR_TACC_STATUS) & 0xD == 0xD

    first_cluster.set_enabled(False)

    assert (
        first_cluster._shared_engine_snapshot()["tacc_owner"]
        == TACC_OWNER_NONE
    )
    assert not any(first_cluster._shared_engine_snapshot()["tacc"])
    assert full1.tacc[0] == 2
    assert second_cluster._shared_engine_snapshot()["tacc"][0] == 4
    assert full1.csr_read(CSR_TACC_STATUS) & 0xF == 0xF
    assert other_owner.csr_read(CSR_TACC_STATUS) & 0xF == 0xF

    second_cluster.reset_shared_resources()

    assert not any(second_cluster._shared_engine_snapshot()["tacc"])
    assert full1.tacc[0] == 2
    assert full1.csr_read(CSR_TACC_STATUS) & 0xF == 0xF


def test_micro_force_release_uses_cluster_privilege_not_caller_shadow():
    system = _system(full_cores=1, clusters=1)
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner, sibling = cluster.cores[:2]
    system.load_binary(0, assemble("t.acc.try"))
    owner.pc = 0
    owner.step()

    cluster.cl_priv_level = 1
    sibling.priv_level = 0
    with pytest.raises(TrapError) as raised:
        sibling.csr_write(CSR_TACC_CTL, 1)
    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert owner.csr_read(CSR_TACC_STATUS) & 0b11 == 0b11

    cluster.cl_priv_level = 0
    sibling.priv_level = 1
    sibling.csr_write(CSR_TACC_CTL, 1)

    status = owner.csr_read(CSR_TACC_STATUS)
    assert status & 0x3FF == 0
    assert (status >> 16) & 0x1F == TACC_OWNER_NONE


@pytest.mark.parametrize("fault_at_completion", (False, True))
def test_reentrant_force_observes_busy_and_wins_at_terminal_boundary(
    fault_at_completion: bool,
):
    memory = bytearray(4096)
    cluster = MicroCluster(
        cluster_id=0,
        id_base=4,
        shared_mem=memory,
        mem_size=len(memory),
    )
    cluster.set_enabled(True)
    owner, sibling = cluster.cores[:2]
    owner.tmode = 0
    owner.mem[0:3] = assemble("t.acc.try")
    owner.pc = 0
    owner.step()
    owner.mem[0:3] = assemble("t.acc.clear")
    owner.pc = 0
    owner.step()

    owner.tsrc0 = 0x100
    owner.mem[0:3] = assemble("t.acc.load")
    owner.mem[0x100:0x200] = bytes(range(256))
    owner.pc = 0
    original_read8 = owner.mem_read8
    probed = False
    injected = RuntimeError("injected TACC transfer failure")

    def force_during_read(address: int) -> int:
        nonlocal probed
        if address == 0x100 and not probed:
            probed = True
            active = sibling.csr_read(CSR_TACC_STATUS)
            assert active & (1 << 4)
            assert (active >> 16) & 0x1F == owner.core_id
            sibling.csr_write(CSR_TACC_CTL, 1)
            pending = sibling.csr_read(CSR_TACC_STATUS)
            assert pending & (1 << 4)
            assert pending & (1 << 9)
            if fault_at_completion:
                raise injected
        return original_read8(address)

    owner.mem_read8 = force_during_read
    if fault_at_completion:
        with pytest.raises(RuntimeError) as raised:
            owner.step()
        assert raised.value is injected
    else:
        owner.step()

    assert probed
    terminal = sibling.csr_read(CSR_TACC_STATUS)
    assert terminal & 0x3FF == 0
    assert (terminal >> 16) & 0x1F == TACC_OWNER_NONE
    assert not any(cluster._shared_engine_snapshot()["tacc"])


def test_stale_cluster_stage_cannot_commit_after_engine_reset():
    memory = bytearray(4096)
    cluster = MicroCluster(
        cluster_id=0,
        id_base=4,
        shared_mem=memory,
        mem_size=len(memory),
    )
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.mem[0:3] = assemble("t.acc.try")
    owner.pc = 0
    owner.step()
    cluster.load_shared_engine_state(owner)
    old_epoch = owner.tacc_epoch

    cluster.reset_shared_resources()
    owner.tacc[0] = 0xA5
    owner.tacc_valid = True
    owner.tacc_dirty = True
    owner.tacc_format_ew = 0

    assert cluster.store_shared_engine_state(owner) is False
    state = cluster._shared_engine_snapshot()
    assert state["tacc_epoch"] == old_epoch + 1
    assert state["tacc_owner"] == TACC_OWNER_NONE
    assert not state["tacc_valid"]
    assert not any(state["tacc"])


@pytest.mark.parametrize(
    "mutate",
    (
        lambda state: state.update(tacc=bytes(255)),
        lambda state: state.update(tacc=256),
        lambda state: state.update(tacc_owner=8),
        lambda state: state.update(
            tacc_valid=True,
            tacc_format_ew=7,
        ),
        lambda state: state.update(tacc_force_pending=True),
    ),
)
def test_cluster_snapshot_validation_is_atomic_and_deep(mutate):
    memory = bytearray(4096)
    cluster = MicroCluster(
        cluster_id=0,
        id_base=4,
        shared_mem=memory,
        mem_size=len(memory),
    )
    before = cluster._shared_engine_snapshot()
    candidate = cluster._shared_engine_snapshot()
    mutate(candidate)

    with pytest.raises(ValueError):
        cluster._commit_shared_engine_state(candidate)

    assert cluster._shared_engine_snapshot() == before
    candidate["acc"][0] = 0xDEAD
    if (
        isinstance(candidate["tacc"], bytearray)
        and len(candidate["tacc"]) == 256
    ):
        candidate["tacc"][0] = 0xA5
    assert cluster._shared_engine_snapshot() == before
