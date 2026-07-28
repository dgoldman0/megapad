"""Physical tile-engine topology and caller-private configuration oracles."""

from __future__ import annotations

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
    CSR_TACC_STATUS,
    CSR_TSRC0,
    CSR_TSRC1,
    CSR_TSTRIDE_C,
    CSR_TSTRIDE_R,
    CSR_TTILE_H,
    CSR_TTILE_W,
    TACC_OWNER_NONE,
)
from system import MegapadSystem


def _system(*, full_cores: int = 4, clusters: int = 3) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )


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
