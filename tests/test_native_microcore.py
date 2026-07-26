"""Phase 2 native microcore execution and cluster-resource boundaries."""

from __future__ import annotations

import pytest

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
    CSR_CPUID,
    CSR_CRC_ACC,
    CSR_CRC_MODE,
    CSR_SHA_MODE,
    CSR_SHA_MSGLEN,
    CSR_SHA_MSGLEN_HI,
    CSR_TSRC0,
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


def test_single_active_microcore_benchmark_is_versioned_and_deterministic():
    """The versioned baseline records native scheduling without peer claims."""
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


def test_direct_sha_transaction_blocks_sibling_until_final():
    """Direct stepping observes the same SHA INIT-to-FINAL ownership."""
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
        assemble("sha.init 0\nsha.final\nhalt"),
    )
    system.load_binary(
        contender_pc,
        assemble("sha.init 1\nhalt"),
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

    assert not cluster.sha_locked
    assert cluster.sha_owner is None

    contender.step()

    assert contender.pc > contender_pc
    assert cluster.sha_locked
    assert cluster.sha_owner == 1
    assert owner.csr_read(CSR_SHA_MODE) == 1


def test_native_cluster_state_is_shared_locally_and_isolated_globally():
    """Scratchpad, tile, and CRC state have exactly one owner per cluster."""
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

    assert sibling.csr_read(CSR_TSRC0) == 0x1234_5678
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

    assert sibling.csr_read(CSR_TSRC0) == 0x1234
    assert sibling.csr_read(CSR_CRC_ACC) == 0x5678
    assert sibling.csr_read(CSR_CRC_MODE) == 2
    assert not cluster.crc_locked

    cluster.reset_shared_resources()

    assert sibling.csr_read(CSR_TSRC0) == 0
    assert sibling.csr_read(CSR_CRC_ACC) == 0xFFFF_FFFF
    assert sibling.csr_read(CSR_CRC_MODE) == 0
    assert sibling.mem_read8(CLUSTER_SPAD_ADDR + 23) == 0xA5
