"""Phase 2 native microcore execution and cluster-resource boundaries."""

from __future__ import annotations

import weakref
from pathlib import Path

import pytest

import bench_phase2_microcore as microcore_benchmark
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
    CLUSTER_SPAD_ADDR,
    CPUID_MICRO,
    CSR_ACC0,
    CSR_SB,
    CSR_SC,
    CSR_CPUID,
    CSR_CRC_ACC,
    CSR_CRC_MODE,
    CSR_SR,
    CSR_SW,
    CSR_SHA_MODE,
    CSR_SHA_MSGLEN,
    CSR_SHA_MSGLEN_HI,
    CSR_PERF_CTRL,
    CSR_PERF_STALLS,
    CSR_PERF_TILEOPS,
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    CSR_TCTRL,
    CSR_TDST,
    CSR_TMODE,
    CSR_TSRC0,
    CSR_TSRC1,
    CSR_TSTRIDE_C,
    CSR_TSTRIDE_R,
    CSR_TTILE_H,
    CSR_TTILE_W,
    IVEC_ILLEGAL_OP,
    Megapad64 as PythonMegapad64,
    Megapad64Micro as PythonMegapad64Micro,
    TrapError,
)
from system import MegapadSystem


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


def test_single_active_microcore_benchmark_is_versioned_and_deterministic(
    monkeypatch,
):
    """The versioned baseline records native scheduling without peer claims."""
    owner_refs = []
    original_build = microcore_benchmark._build_workload

    def tracked_build(*, worker_count: int):
        system, micro = original_build(worker_count=worker_count)
        owner_refs.append(weakref.ref(system._native_system))
        return system, micro

    monkeypatch.setattr(
        microcore_benchmark,
        "_build_workload",
        tracked_build,
    )
    report = run_report(
        instructions=9,
        worker_counts=(1, 2, 4),
        repeats=2,
        warmups=0,
        warmup_instructions=1,
        host_profile=True,
    )

    assert report["schema"] == REPORT_SCHEMA
    assert report["schema_version"] == REPORT_SCHEMA_VERSION
    assert all(report["validation"].values())
    repository = report["repository"]
    assert Path(repository["root"]).is_dir()
    assert len(repository["commit"]) == 40
    int(repository["commit"], 16)
    assert isinstance(repository["branch"], str)
    assert isinstance(repository["dirty"], bool)
    accelerator = report["accelerator"]
    artifact = Path(accelerator["loaded_artifact_path"])
    assert artifact.is_file()
    assert accelerator["loaded_artifact_size_bytes"] == artifact.stat().st_size
    assert len(accelerator["loaded_artifact_sha256"]) == 64
    int(accelerator["loaded_artifact_sha256"], 16)
    assert accelerator["elf_build_id"]
    assert report["semantics"]["native_scheduler_expected"] is True
    scope = report["semantics"]["qos_and_fairness_scope"]
    assert scope["contention_exercised"] is False
    assert scope["qos_claim"] == "excluded"
    assert scope["fairness_claim"] == "excluded"

    cross = report["cross_worker_equivalence"]["validation"]
    assert all(cross.values())
    hashes = set()
    for worker_report in report["worker_reports"]:
        assert all(worker_report["validation"].values())
        assert len(worker_report["timed_samples"]) == 2
        samples = [
            *worker_report["timed_samples"],
            worker_report["accounting_probe"],
        ]
        for sample in samples:
            timing_hygiene = sample["timing_hygiene"]
            assert timing_hygiene["gc_enabled_during_timing"] is False
            assert timing_hygiene["gc_restored_to_prior_state"] is True
            assert (
                timing_hygiene["collected_objects_after_sample"]
                is not None
            )
            observation = sample["observation"]
            assert observation["state_schema"] == STATE_SCHEMA
            assert (
                observation["state_schema_version"]
                == STATE_SCHEMA_VERSION
            )
            hashes.add(observation["behavior_oracle_sha256"])
            state = observation["canonical_state"]
            execution = state["execution"]
            assert execution["instructions_executed"] == 9
            assert execution["per_core_instructions"] == [
                0, 9, 0, 0, 0,
            ]
            assert execution["per_core_cycles"][0] == 0
            assert execution["per_core_cycles"][1] == 13
            assert execution["per_core_cycles"][2:] == [0, 0, 0]
            assert execution["system_cycles_advanced"] == 13
            assert execution["native_scheduler"] is True
            assert execution["native_batch_runs_counter_delta"] == 1
            assert execution["native_dispatches_counter_delta"] == 1
            assert execution["python_fallback_instantiated"] is False

            topology = state["topology"]
            assert topology[
                "active_core_is_system_owned_micro_profile"
            ]
            assert (
                topology["cluster_enable_mask"]
                == 0xFFFF_FFFF_FFFF_FFFF
            )
            assert topology["cluster_enabled"] is True
            workload = state["workload"]
            assert workload["cluster_enable_policy"] == (
                "all_ones_reset_then_host_selects_one_runnable_core"
            )
            assert (
                workload["cluster_enable_mask"]
                == 0xFFFF_FFFF_FFFF_FFFF
            )
            assert (
                workload[
                    "other_cores_halted_by_host_for_single_core_baseline"
                ]
                is True
            )
            assert workload["contention_exercised"] is False
            assert state["cores"][1]["profile"] == "micro"
            assert state["cores"][1]["accelerated_wrapper"]
            assert state["cores"][1][
                "common_gprs_r0_r15"
            ][1] == 5
            assert all(
                core["halted"]
                for index, core in enumerate(state["cores"])
                if index != 1
            )
            assert state["main_bus"]["active_grant"] is False
            assert state["main_bus"]["pending_request_count"] == 0

        profile = worker_report["accounting_probe"][
            "host_profile_probe"
        ]
        assert profile is not None
        assert all(profile["validation"].values())
        counts = profile["native_snapshot"]["counts"]
        assert counts["private_steps"] == 9
        assert 1 <= counts["worker_commands"] <= 9
        assert (
            counts["worker_commands"]
            == counts["worker_waves"]
            == counts["checkpoint_captures"]
            == counts["logical_subfrontiers"]
        )

    assert len(hashes) == 1
    assert owner_refs
    assert all(owner_ref() is None for owner_ref in owner_refs)


@pytest.mark.parametrize(
    "worker_counts",
    ((1,), (2, 4), (1, 2)),
)
def test_single_active_microcore_report_requires_all_lane_widths(
    worker_counts: tuple[int, ...],
) -> None:
    with pytest.raises(
        ValueError,
        match="exactly 1, 2, and 4",
    ):
        run_report(
            instructions=1,
            worker_counts=worker_counts,
            repeats=1,
            warmups=0,
            warmup_instructions=1,
        )


def test_all_advertised_cores_share_the_native_scheduler_budget():
    """Full and reduced cores receive one equal cyclic turn per small batch."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=4,
        num_clusters=3,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(
        0x100,
        assemble(
            """
loop:
    inc r1
    br loop
"""
        ),
    )

    for cpu in system.cores:
        cpu.pc = 0x100
        cpu.halted = False
        cpu.idle = False

    for cpu in system.cores[system.num_full_cores:]:
        def reject_python_fallback(_cpu=cpu):
            raise AssertionError(
                f"microcore {_cpu.core_id} left native scalar execution"
            )

        cpu._step_python_fallback_in_memory_scope = reject_python_fallback

    runs_before = system._native_system.native_batch_runs
    stats = system.run_batch_stats(system.num_cores * 2)

    assert stats.native_scheduler
    assert system._native_system.native_batch_runs == runs_before + 1
    assert stats.instructions_executed == system.num_cores * 2
    assert stats.per_core_instructions == (2,) * system.num_cores
    assert stats.per_core_dispatches == (1,) * system.num_cores
    assert tuple(cpu.regs[1] for cpu in system.cores) == (
        1,
    ) * system.num_cores
    assert system._scheduler_cursor == 0


def test_cluster_mul_uses_independent_equal_round_robin_credit():
    """A shared MUL grant retires one contender and rotates local credit."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.sysinfo.write8(0x18, 0x01)
    first, second = system.clusters[0].cores[:2]
    first_pc = 0x100
    second_pc = 0x180
    system.load_binary(first_pc, assemble("mul r1, r2\nhalt"))
    system.load_binary(second_pc, assemble("mul r1, r2\nhalt"))

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, address, left, right in (
        (first, first_pc, 6, 7),
        (second, second_pc, 8, 9),
    ):
        cpu.pc = address
        cpu.regs[1] = left
        cpu.regs[2] = right
        cpu.halted = False

    first_grant = system.run_batch_stats(1)

    assert first_grant.native_scheduler
    assert first_grant.per_core_instructions == (0, 0, 1, 0, 0)
    assert first.pc == first_pc
    assert first.regs[1] == 6
    assert second.regs[1] == 72
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["last_grants"]["mul_div"] == 1
    assert snapshot["grant_counts"]["mul_div"] == 1

    second.halted = True
    second_grant = system.run_batch_stats(1)

    assert second_grant.per_core_instructions == (0, 1, 0, 0, 0)
    assert first.regs[1] == 42
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["last_grants"]["mul_div"] == 0
    assert snapshot["grant_counts"]["mul_div"] == 2


def test_cluster_tile_engine_rotates_across_acc_sha_and_mex_producers():
    """All legacy ACC producers share one deterministic physical-engine turn."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    acc_writer, sha_instruction, mex_writer, sha_csr_reader = (
        cluster.cores
    )
    programs = (
        f"csrw {CSR_ACC0}, r1\nhalt",
        "sha.release\nhalt",
        "t.add\nhalt",
        f"csrr r4, {CSR_SHA_MODE}\nhalt",
    )

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    for cpu, address, source in zip(
        cluster.cores,
        (0x100, 0x180, 0x200, 0x280),
        programs,
    ):
        system.load_binary(address, assemble(source))
        cpu.pc = address
        cpu.halted = False

    acc_writer.regs[1] = 0xCAFE_BABE
    mex_writer.tsrc0 = 0x300
    mex_writer.tsrc1 = 0x340
    mex_writer.tdst = 0x380
    system.cpu.mem[0x300:0x340] = bytes(range(64))
    system.cpu.mem[0x340:0x380] = bytes(
        reversed(range(64))
    )

    for grant_count, expected_local in enumerate(
        (1, 2, 3, 0),
        start=1,
    ):
        stats = system.run_batch_stats(1)

        expected_progress = [0] * len(system.cores)
        expected_progress[system.num_full_cores + expected_local] = 1
        assert stats.per_core_instructions == tuple(expected_progress)
        snapshot = system._native_system._cluster_arbiter_snapshot(0)
        assert snapshot["schema_version"] == 2
        assert set(snapshot["last_grants"]) == {
            "bus",
            "mul_div",
            "crc",
            "tile_engine",
        }
        assert snapshot["last_grants"]["tile_engine"] == expected_local
        assert snapshot["grant_counts"]["tile_engine"] == grant_count
        cluster.cores[expected_local].halted = True

    assert acc_writer.csr_read(CSR_ACC0) == 0xCAFE_BABE
    assert sha_csr_reader.regs[4] == 0
    assert bytes(system.cpu.mem[0x380:0x3C0]) == bytes([63]) * 64


def test_cluster_tile_engine_recontention_is_equal_round_robin():
    """Pending callers receive a second turn only after every peer's first."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    program = assemble(
        f"csrw {CSR_ACC0}, r1\n"
        f"csrw {CSR_ACC0}, r1\n"
        "halt"
    )
    for local, cpu in enumerate(cluster.cores):
        address = 0x100 + local * 0x40
        system.load_binary(address, program)
        cpu.pc = address
        cpu.regs[1] = 0x100 + local
        cpu.halted = False

    observed = []
    for expected_local in (1, 2, 3, 0, 1, 2, 3, 0):
        stats = system.run_batch_stats(1)
        expected_progress = [0] * len(system.cores)
        expected_progress[system.num_full_cores + expected_local] = 1
        assert stats.per_core_instructions == tuple(expected_progress)
        observed.append(
            system._native_system
            ._cluster_arbiter_snapshot(0)["last_grants"]["tile_engine"]
        )

    assert observed == [1, 2, 3, 0, 1, 2, 3, 0]
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["grant_counts"]["tile_engine"] == 8


def test_public_step_uses_the_same_cluster_tile_admission_as_batch():
    """The public execution APIs select the same first common-engine winner."""
    def configured_system() -> MegapadSystem:
        candidate = MegapadSystem(
            ram_size=4096,
            num_cores=1,
            num_clusters=1,
            hbw_size=0,
            ext_mem_size=0,
            vram_size=0,
            worker_count=1,
        )
        candidate.sysinfo.write8(0x18, 0x01)
        for cpu in candidate.cores:
            cpu.halted = True
            cpu.idle = False
        for local, cpu in enumerate(candidate.clusters[0].cores):
            address = 0x100 + local * 0x40
            candidate.load_binary(
                address,
                assemble(f"csrw {CSR_ACC0}, r1\nhalt"),
            )
            cpu.pc = address
            cpu.regs[1] = 0xA0 + local
            cpu.halted = False
        return candidate

    stepped = configured_system()
    batched = configured_system()

    stepped.step()
    batch = batched.run_batch_stats(4)

    assert batch.instructions_executed == 4
    assert tuple(cpu.pc for cpu in stepped.cores) == tuple(
        cpu.pc for cpu in batched.cores
    )
    assert stepped.clusters[0].cores[0].csr_read(CSR_ACC0) == (
        batched.clusters[0].cores[0].csr_read(CSR_ACC0)
    )
    assert dict(
        stepped._native_system._cluster_arbiter_snapshot(0)
    ) == dict(
        batched._native_system._cluster_arbiter_snapshot(0)
    )


@pytest.mark.parametrize(
    "private_csr",
    (
        CSR_SB,
        CSR_SR,
        CSR_SC,
        CSR_SW,
        CSR_TMODE,
        CSR_TCTRL,
        CSR_TSRC0,
        CSR_TSRC1,
        CSR_TDST,
        CSR_TSTRIDE_R,
        CSR_TSTRIDE_C,
        CSR_TTILE_H,
        CSR_TTILE_W,
    ),
)
def test_private_tile_csr_instruction_bypasses_engine_admission(
    private_csr: int,
) -> None:
    """A caller shadow can retire beside one admitted shared ACC CSR access."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.sysinfo.write8(0x18, 0x01)
    private_writer, acc_writer = system.clusters[0].cores[:2]
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    system.load_binary(
        0x100,
        assemble(f"csrw {private_csr}, r1\nhalt"),
    )
    system.load_binary(
        0x180,
        assemble(f"csrw {CSR_ACC0}, r1\nhalt"),
    )
    private_writer.pc = 0x100
    private_writer.regs[1] = 1
    private_writer.halted = False
    acc_writer.pc = 0x180
    acc_writer.regs[1] = 0x1234
    acc_writer.halted = False

    stats = system.run_batch_stats(2)

    assert stats.per_core_instructions == (0, 1, 1, 0, 0)
    assert private_writer.csr_read(private_csr) == 1
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["grant_counts"]["tile_engine"] == 1
    assert snapshot["last_grants"]["tile_engine"] == 1


def test_cluster_crc_lock_blocks_without_retiring_the_contender():
    """A locked-out CRC request preserves its PC and instruction budget."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    first, second = cluster.cores[:2]
    first_pc = 0x100
    second_pc = 0x180
    system.load_binary(
        first_pc,
        assemble("crc.mode 0\ncrc.fin r4, r0\nhalt"),
    )
    system.load_binary(
        second_pc,
        assemble("crc.mode 1\ncrc.fin r4, r0\nhalt"),
    )

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    first.pc = first_pc
    second.pc = second_pc
    first.halted = False
    second.halted = False

    acquire = system.run_batch_stats(1)

    assert acquire.per_core_instructions == (0, 0, 1, 0, 0)
    assert first.pc == first_pc
    assert second.pc > second_pc
    assert cluster.crc_locked
    assert cluster.crc_owner == 1
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["crc_locked"]
    assert snapshot["crc_lock_owner"] == 1

    release = system.run_batch_stats(1)

    assert release.per_core_instructions == (0, 0, 1, 0, 0)
    assert first.pc == first_pc
    assert not cluster.crc_locked
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert not snapshot["crc_locked"]
    assert snapshot["grant_counts"]["crc"] == 2

    second.halted = True
    next_owner = system.run_batch_stats(1)

    assert next_owner.per_core_instructions == (0, 1, 0, 0, 0)
    assert first.pc > first_pc
    assert cluster.crc_locked
    assert cluster.crc_owner == 0
    snapshot = system._native_system._cluster_arbiter_snapshot(0)
    assert snapshot["crc_locked"]
    assert snapshot["crc_lock_owner"] == 0
    assert snapshot["grant_counts"]["crc"] == 3


def test_cluster_crc_finraw_commits_before_native_release():
    """Native FINRAW makes its result visible with the unlocked snapshot."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    owner = cluster.cores[0]
    address = 0x100
    system.load_binary(
        address,
        assemble("crc.mode 5\ncrc.init\ncrc.finraw r4, r0\nhalt"),
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    owner.pc = address
    owner.halted = False

    system.run_batch_stats(3)

    snapshot = cluster.crc_snapshot()
    assert snapshot == {
        "acc": 0xFFFF_FFFF,
        "mode": 5,
        "locked": False,
        "owner": None,
    }
    assert owner.regs[4] == 0xFFFF_FFFF


def test_direct_sha_transaction_blocks_nonowner_release_until_owner_release():
    """FINAL retains ownership; only the owner's RELEASE permits handoff."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    owner, contender = cluster.cores[:2]
    owner_pc = 0x100
    contender_pc = 0x200
    system.load_binary(
        owner_pc,
        assemble(
            "sha.init 0\n"
            "sha.final\n"
            "sha.dout r4, r0\n"
            "sha.release\n"
            "halt"
        ),
    )
    system.load_binary(
        contender_pc,
        assemble("sha.release\nsha.init 1\nhalt"),
    )
    owner.pc = owner_pc
    contender.pc = contender_pc
    owner.halted = False
    contender.halted = False

    owner.step()

    assert cluster.sha_locked
    assert cluster.sha_owner == 0
    assert contender.csr_read(CSR_SHA_MODE) == 0

    contender.step()

    assert contender.pc == contender_pc
    assert cluster.sha_owner == 0
    assert contender.csr_read(CSR_SHA_MODE) == 0

    owner.step()

    assert cluster.sha_locked
    assert cluster.sha_owner == 0

    contender.step()

    assert contender.pc == contender_pc
    assert cluster.sha_locked
    assert cluster.sha_owner == 0

    owner.step()

    assert owner.regs[4] != 0
    assert cluster.sha_locked
    assert cluster.sha_owner == 0

    contender.step()

    assert contender.pc == contender_pc
    assert cluster.sha_locked
    assert cluster.sha_owner == 0

    owner.step()

    assert not cluster.sha_locked
    assert cluster.sha_owner is None

    contender.step()

    assert contender.pc == contender_pc + 2
    assert not cluster.sha_locked
    assert cluster.sha_owner is None

    contender.step()

    assert contender.pc > contender_pc
    assert cluster.sha_locked
    assert cluster.sha_owner == 1
    assert owner.csr_read(CSR_SHA_MODE) == 1


def test_sha_lock_protects_acc_producers_but_allows_stateless_mex():
    """A live digest excludes ACC writers without monopolizing tile ALU work."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    owner, acc_writer, stateless, reducer = cluster.cores
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False

    system.load_binary(
        0x100,
        assemble("sha.init 0\nsha.release\nhalt"),
    )
    owner.pc = 0x100
    owner.halted = False
    assert system.run_batch_stats(1).instructions_executed == 1
    assert cluster.sha_locked
    assert cluster.sha_owner == 0
    digest = tuple(owner.csr_read(csr) for csr in (
        CSR_ACC0,
        CSR_ACC0 + 1,
        CSR_ACC0 + 2,
        CSR_ACC0 + 3,
    ))
    assert digest[0] == 0x6A09_E667_BB67_AE85
    owner.halted = True

    system.load_binary(
        0x180,
        assemble(f"csrw {CSR_ACC0}, r1\nhalt"),
    )
    acc_writer.pc = 0x180
    acc_writer.regs[1] = 0xDEAD_BEEF
    acc_writer.halted = False

    system.load_binary(0x200, assemble("t.add\nhalt"))
    stateless.pc = 0x200
    stateless.tsrc0 = 0x300
    stateless.tsrc1 = 0x340
    stateless.tdst = 0x380
    stateless.mem[0x300:0x340] = bytes(range(64))
    stateless.mem[0x340:0x380] = bytes(reversed(range(64)))
    stateless.halted = False

    system.load_binary(0x280, assemble("t.sum\nhalt"))
    reducer.pc = 0x280
    reducer.tsrc0 = 0x3C0
    reducer.mem[0x3C0:0x400] = bytes([1]) * 64
    reducer.halted = False

    stateless_result = system.run_batch_stats(1)

    assert stateless_result.per_core_instructions == (0, 0, 0, 1, 0)
    assert acc_writer.pc == 0x180
    assert reducer.pc == 0x280
    assert tuple(owner.csr_read(csr) for csr in (
        CSR_ACC0,
        CSR_ACC0 + 1,
        CSR_ACC0 + 2,
        CSR_ACC0 + 3,
    )) == digest
    assert bytes(stateless.mem[0x380:0x3C0]) == bytes([63]) * 64

    owner.halted = False
    assert system.run_batch_stats(1).per_core_instructions == (
        0, 1, 0, 0, 0
    )
    assert not cluster.sha_locked

    assert system.run_batch_stats(1).per_core_instructions == (
        0, 0, 1, 0, 0
    )
    assert owner.csr_read(CSR_ACC0) == 0xDEAD_BEEF


def test_direct_micro_steps_apply_the_same_sha_acc_exclusion():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system.sysinfo.write8(0x18, 0x01)
    cluster = system.clusters[0]
    owner, acc_writer, stateless, reducer = cluster.cores

    system.load_binary(
        0x100,
        assemble("sha.init 0\nsha.release\nhalt"),
    )
    owner.pc = 0x100
    owner.step()
    digest = tuple(
        owner.csr_read(CSR_ACC0 + offset)
        for offset in range(4)
    )
    assert cluster.sha_locked

    system.load_binary(
        0x180,
        assemble(f"csrw {CSR_ACC0}, r1\nhalt"),
    )
    acc_writer.pc = 0x180
    acc_writer.regs[1] = 0xDEAD_BEEF
    acc_writer.step()
    assert acc_writer.pc == 0x180

    system.load_binary(0x200, assemble("t.add\nhalt"))
    stateless.pc = 0x200
    stateless.tsrc0 = 0x300
    stateless.tsrc1 = 0x340
    stateless.tdst = 0x380
    stateless.mem[0x300:0x340] = bytes(range(64))
    stateless.mem[0x340:0x380] = bytes(reversed(range(64)))
    stateless.step()
    assert stateless.pc == 0x202
    assert bytes(stateless.mem[0x380:0x3C0]) == bytes([63]) * 64

    system.load_binary(0x280, assemble("t.sum\nhalt"))
    reducer.pc = 0x280
    reducer.tsrc0 = 0x3C0
    reducer.mem[0x3C0:0x400] = bytes([1]) * 64
    reducer.step()
    assert reducer.pc == 0x280
    assert tuple(
        owner.csr_read(CSR_ACC0 + offset)
        for offset in range(4)
    ) == digest

    owner.step()
    assert not cluster.sha_locked
    acc_writer.step()
    assert acc_writer.pc == 0x182
    assert owner.csr_read(CSR_ACC0) == 0xDEAD_BEEF


def test_cluster_sha_samples_the_granted_callers_private_tsrc0():
    payload = bytes(range(64))
    decoy = bytes(reversed(range(64)))
    program = assemble("sha.init 0\nsha.round\nhalt")

    full_system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    full_system.load_binary(0x100, program)
    full_system.cpu.pc = 0x100
    full_system.cpu.tsrc0 = 0x300
    full_system.cpu.mem[0x300:0x340] = payload
    assert full_system.run_batch_stats(2).instructions_executed == 2
    expected = tuple(
        full_system.cpu.csr_read(CSR_ACC0 + offset)
        for offset in range(4)
    )

    micro_system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    micro_system.sysinfo.write8(0x18, 0x01)
    cluster = micro_system.clusters[0]
    owner, sibling = cluster.cores[:2]
    for cpu in micro_system.cores:
        cpu.halted = True
        cpu.idle = False
    micro_system.load_binary(0x100, program)
    owner.pc = 0x100
    owner.tsrc0 = 0x300
    owner.halted = False
    sibling.tsrc0 = 0x340
    owner.mem[0x300:0x340] = payload
    owner.mem[0x340:0x380] = decoy

    assert micro_system.run_batch_stats(2).instructions_executed == 2

    assert tuple(
        owner.csr_read(CSR_ACC0 + offset)
        for offset in range(4)
    ) == expected
    assert owner.csr_read(CSR_TSRC0) == 0x300
    assert sibling.csr_read(CSR_TSRC0) == 0x340


def test_native_cluster_state_is_shared_locally_and_isolated_globally():
    """Tile configuration is private while engine results stay cluster-local."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=2,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    cluster0, cluster1 = system.clusters
    first, sibling = cluster0.cores[:2]
    other = cluster1.cores[0]

    first.csr_write(CSR_TSRC0, 0x1234_5678)
    first.csr_write(CSR_ACC0, 0xCAFE_BABE)
    first.csr_write(CSR_SHA_MODE, 2)
    first.csr_write(CSR_SHA_MSGLEN, 0x808)
    first.csr_write(CSR_SHA_MSGLEN_HI, 1)

    assert first.csr_read(CSR_TSRC0) == 0x1234_5678
    assert sibling.csr_read(CSR_TSRC0) == 0
    assert sibling.csr_read(CSR_ACC0) == 0xCAFE_BABE
    assert sibling.csr_read(CSR_SHA_MODE) == 2
    assert sibling.csr_read(CSR_SHA_MSGLEN) == 0x808
    assert sibling.csr_read(CSR_SHA_MSGLEN_HI) == 1
    assert other.csr_read(CSR_TSRC0) == 0
    assert other.csr_read(CSR_ACC0) == 0
    assert other.csr_read(CSR_SHA_MODE) == 0
    assert other.csr_read(CSR_SHA_MSGLEN) == 0
    assert other.csr_read(CSR_SHA_MSGLEN_HI) == 0

    first.mem_write8(CLUSTER_SPAD_ADDR + 17, 0xA5)
    other.mem_write8(CLUSTER_SPAD_ADDR + 17, 0x5A)

    assert sibling.mem_read8(CLUSTER_SPAD_ADDR + 17) == 0xA5
    assert other.mem_read8(CLUSTER_SPAD_ADDR + 17) == 0x5A
    assert system._shared_mem[17] == 0

    cluster0.crc_acc = 0x1111_2222
    cluster0.crc_mode = 1
    cluster1.crc_acc = 0xAAAA_BBBB_CCCC_DDDD
    cluster1.crc_mode = 2

    assert sibling.csr_read(CSR_CRC_ACC) == 0x1111_2222
    assert sibling.csr_read(CSR_CRC_MODE) == 1
    assert other.csr_read(CSR_CRC_ACC) == 0xAAAA_BBBB_CCCC_DDDD
    assert other.csr_read(CSR_CRC_MODE) == 2


def test_tacc_fallback_keeps_microcore_performance_state_caller_private():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner, sibling = cluster.cores[:2]
    system.load_binary(0, assemble("t.acc.try"))
    owner.csr_write(CSR_PERF_CTRL, 1)
    sibling.csr_write(CSR_PERF_CTRL, 1)
    owner.perf_stalls = 7
    sibling.perf_stalls = 11
    owner.pc = 0

    owner.step()

    status = sibling.csr_read(CSR_TACC_STATUS)
    assert (status >> 16) & 0x1F == owner.core_id
    assert owner.csr_read(CSR_PERF_TILEOPS) == 1
    assert sibling.csr_read(CSR_PERF_TILEOPS) == 0
    assert owner.csr_read(CSR_PERF_STALLS) == 7
    assert sibling.csr_read(CSR_PERF_STALLS) == 11

    cluster.load_shared_engine_state(sibling)
    assert cluster.store_shared_engine_state(sibling)
    assert owner.csr_read(CSR_PERF_TILEOPS) == 1
    assert sibling.csr_read(CSR_PERF_TILEOPS) == 0
    assert owner.csr_read(CSR_PERF_STALLS) == 7
    assert sibling.csr_read(CSR_PERF_STALLS) == 11


def test_exceptional_micro_tacc_fallback_restores_authoritative_cluster_state():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()
    before = cluster._shared_engine_snapshot()

    source = 0x100
    owner.tsrc0 = source
    owner.mem[source:source + 256] = bytes(range(256))
    system.load_binary(0, assemble("t.acc.load"))
    owner.pc = 0
    original_read8 = owner.mem_read8
    injected = RuntimeError("injected micro TACC read failure")

    def fail_first_tacc_read(address: int) -> int:
        if address == source:
            raise injected
        return original_read8(address)

    owner.mem_read8 = fail_first_tacc_read
    with pytest.raises(RuntimeError) as raised:
        owner.step()
    assert raised.value is injected

    after = cluster._shared_engine_snapshot()
    assert after == before
    staged = dict(owner._cs.tacc_snapshot())
    for name in (
        "tacc_owner",
        "tacc_valid",
        "tacc_dirty",
        "tacc_format_ew",
        "tacc_format_signed",
        "tacc_busy",
        "tacc_force_pending",
        "tacc_epoch",
    ):
        assert staged[name] == after[name]
    assert staged["tacc"] == bytes(after["tacc"])


@pytest.mark.parametrize(
    "execution_surface",
    ("step", "core-batch", "system-batch"),
)
def test_micro_tacc_reset_callback_cancels_without_retirement(
    execution_surface: str,
):
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()

    before = cluster._shared_engine_snapshot()
    arbiter_before = dict(
        system._native_system._cluster_arbiter_snapshot(0)
    )
    caller_epoch_before = cluster._caller_tacc_epoch(owner.core_id)
    cycles_before = owner.cycle_count
    perf_cycles_before = owner.perf_cycles
    tileops_before = owner.perf_tileops
    source0 = 0x100
    source1 = 0x140
    owner.tsrc0 = source0
    owner.tsrc1 = source1
    owner.mem[source0:source0 + 64] = bytes([2]) * 64
    owner.mem[source1:source1 + 64] = bytes([3]) * 64
    owner.regs[5] = 0xA5A5_5A5A_DEAD_BEEF
    system.load_binary(0, assemble("t.amac"))
    owner.pc = 0
    original_read8 = owner.mem_read8
    callback_count = 0

    def reset_during_source_read(address: int) -> int:
        nonlocal callback_count
        if address == source0 and callback_count == 0:
            callback_count += 1
            owner._reset_state_in_memory_scope()
        return original_read8(address)

    owner.mem_read8 = reset_during_source_read
    if execution_surface == "step":
        assert owner.step() == 0
    elif execution_surface == "core-batch":
        stats = owner.run_steps_stats(1)
        assert stats.steps_executed == 0
        assert stats.total_cycles == 0
        assert stats.stop_reason == 0
    else:
        for cpu in system.cores:
            cpu.halted = cpu is not owner
            cpu.idle = False
        stats = system.run_batch_stats(1)
        assert stats.instructions_executed == 0
        assert stats.per_core_instructions == (0,) * system.num_cores
        assert stats.per_core_cycles == (0,) * system.num_cores

    assert callback_count == 1
    assert owner.pc == 0
    assert owner.regs[5] == 0
    assert owner.cycle_count == cycles_before
    assert owner.perf_cycles == perf_cycles_before
    assert owner.perf_tileops == tileops_before
    assert cluster._caller_tacc_epoch(owner.core_id) == caller_epoch_before + 1
    assert cluster._shared_engine_snapshot() == before
    arbiter_after = dict(
        system._native_system._cluster_arbiter_snapshot(0)
    )
    assert (
        arbiter_after["grant_counts"]["tile_engine"]
        == arbiter_before["grant_counts"]["tile_engine"]
    )
    assert (
        arbiter_after["last_grants"]["tile_engine"]
        == arbiter_before["last_grants"]["tile_engine"]
    )
    assert (
        arbiter_after["grant_sequence"]
        == arbiter_before["grant_sequence"]
    )
    staged = dict(owner._cs.tacc_snapshot())
    assert staged["tacc"] == bytes(before["tacc"])
    for name in (
        "tacc_owner",
        "tacc_valid",
        "tacc_dirty",
        "tacc_format_ew",
        "tacc_format_signed",
        "tacc_busy",
        "tacc_force_pending",
        "tacc_epoch",
    ):
        assert staged[name] == before[name]


@pytest.mark.parametrize(
    "execution_surface",
    ("step", "core-batch", "system-batch"),
)
def test_guest_micro_reset_retires_without_discarding_shared_tacc(
    execution_surface: str,
):
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()

    before = cluster._shared_engine_snapshot()
    caller_epoch_before = cluster._caller_tacc_epoch(owner.core_id)
    cycles_before = owner.cycle_count
    owner.regs[5] = 0xA5A5_5A5A_DEAD_BEEF
    system.load_binary(0, assemble("reset"))
    owner.pc = 0

    if execution_surface == "step":
        assert owner.step() == 1
    elif execution_surface == "core-batch":
        stats = owner.run_steps_stats(1)
        assert stats.steps_executed == 1
        assert stats.total_cycles == 1
        assert stats.stop_reason == 0
    else:
        for cpu in system.cores:
            cpu.halted = cpu is not owner
            cpu.idle = False
        stats = system.run_batch_stats(1)
        assert stats.instructions_executed == 1
        assert stats.per_core_instructions[owner.core_id] == 1
        assert stats.per_core_cycles[owner.core_id] == 1

    assert owner.pc == 0
    assert owner.regs[5] == 0
    assert owner.cycle_count == cycles_before + 1
    assert cluster._caller_tacc_epoch(owner.core_id) == caller_epoch_before + 1
    assert cluster._shared_engine_snapshot() == before


def test_scratchpad_tacc_reset_callback_uses_executed_route_for_cancellation():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()

    before = cluster._shared_engine_snapshot()
    caller_epoch_before = cluster._caller_tacc_epoch(owner.core_id)
    cycles_before = owner.cycle_count
    tileops_before = owner.perf_tileops
    source0 = 0x100
    source1 = 0x140
    owner.tsrc0 = source0
    owner.tsrc1 = source1
    owner.mem[source0:source0 + 64] = bytes([2]) * 64
    owner.mem[source1:source1 + 64] = bytes([3]) * 64
    system.load_binary(0, assemble("nop"))
    scratchpad_code = assemble("t.amac")
    for offset, value in enumerate(scratchpad_code):
        cluster.spad_write8(offset, value)
    owner.pc = CLUSTER_SPAD_ADDR
    original_read8 = owner.mem_read8
    callback_count = 0

    def reset_during_source_read(address: int) -> int:
        nonlocal callback_count
        if address == source0 and callback_count == 0:
            callback_count += 1
            owner._reset_state_in_memory_scope()
        return original_read8(address)

    owner.mem_read8 = reset_during_source_read

    assert owner.step() == 0
    assert callback_count == 1
    assert owner.pc == 0
    assert owner.cycle_count == cycles_before
    assert owner.perf_tileops == tileops_before
    assert cluster._caller_tacc_epoch(owner.core_id) == caller_epoch_before + 1
    assert cluster._shared_engine_snapshot() == before


def test_scratchpad_guest_reset_ignores_bank_zero_tacc_alias():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner = cluster.cores[0]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()

    before = cluster._shared_engine_snapshot()
    caller_epoch_before = cluster._caller_tacc_epoch(owner.core_id)
    cycles_before = owner.cycle_count
    owner.regs[5] = 0xA5A5_5A5A_DEAD_BEEF
    system.load_binary(0, assemble("t.amac"))
    scratchpad_code = assemble("reset")
    for offset, value in enumerate(scratchpad_code):
        cluster.spad_write8(offset, value)
    owner.pc = CLUSTER_SPAD_ADDR

    assert owner.step() == 1
    assert owner.pc == 0
    assert owner.regs[5] == 0
    assert owner.cycle_count == cycles_before + 1
    assert cluster._caller_tacc_epoch(owner.core_id) == caller_epoch_before + 1
    assert cluster._shared_engine_snapshot() == before


@pytest.mark.parametrize("fault_at_completion", (False, True))
def test_accelerated_micro_tacc_reentrant_force_wins_at_terminal_boundary(
    fault_at_completion: bool,
):
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    cluster.cl_priv_level = 0
    owner, sibling = cluster.cores[:2]
    owner.tmode = 0
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.step()

    source = 0x100
    owner.tsrc0 = source
    owner.mem[source:source + 256] = bytes(range(256))
    system.load_binary(0, assemble("t.acc.load"))
    owner.pc = 0
    original_read8 = owner.mem_read8
    injected = TrapError(
        IVEC_ILLEGAL_OP,
        "injected accelerated micro TACC failure",
    )
    probed = False

    def force_during_read(address: int) -> int:
        nonlocal probed
        if address == source and not probed:
            probed = True
            active = sibling.csr_read(CSR_TACC_STATUS)
            assert active & (1 << 4)
            assert active & (1 << 1) == 0
            assert (active >> 16) & 0x1F == owner.core_id

            sibling.csr_write(CSR_TACC_CTL, 1)
            pending = sibling.csr_read(CSR_TACC_STATUS)
            assert pending & (1 << 4)
            assert pending & (1 << 9)
            assert (pending >> 16) & 0x1F == owner.core_id
            if fault_at_completion:
                raise injected
        return original_read8(address)

    owner.mem_read8 = force_during_read
    if fault_at_completion:
        with pytest.raises(TrapError) as raised:
            owner.step()
        assert raised.value is injected
    else:
        owner.step()

    assert probed
    terminal = sibling.csr_read(CSR_TACC_STATUS)
    assert terminal & 0x3FF == 0
    assert (terminal >> 16) & 0x1F == 31
    assert not any(cluster._shared_engine_snapshot()["tacc"])


def test_native_cluster_reset_preserves_scratchpad_and_api_boundaries():
    """Arbiter-only reset preserves engines; cluster reset preserves RAM."""
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    cluster = system.clusters[0]
    first, sibling = cluster.cores[:2]

    first.csr_write(CSR_TSRC0, 0x1234)
    first.mem_write8(CLUSTER_SPAD_ADDR + 23, 0xA5)
    cluster.crc_acc = 0x5678
    cluster.crc_mode = 2
    with pytest.raises(ValueError, match="crc_try_acquire"):
        cluster.crc_locked = True
    assert not cluster.crc_locked
    assert cluster.crc_owner is None
    assert cluster.crc_try_acquire(first.core_id)

    system._native_system.reset_cluster_arbitration(0)

    assert first.csr_read(CSR_TSRC0) == 0x1234
    assert sibling.csr_read(CSR_TSRC0) == 0
    assert sibling.csr_read(CSR_CRC_ACC) == 0x5678
    assert sibling.csr_read(CSR_CRC_MODE) == 2
    assert not cluster.crc_locked

    cluster.reset_shared_resources()

    assert sibling.csr_read(CSR_TSRC0) == 0
    assert sibling.csr_read(CSR_CRC_ACC) == 0xFFFF_FFFF
    assert sibling.csr_read(CSR_CRC_MODE) == 0
    assert sibling.mem_read8(CLUSTER_SPAD_ADDR + 23) == 0xA5
