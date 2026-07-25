"""Phase 2 resumable full-core and cycle-boundary oracles."""

from __future__ import annotations

from types import SimpleNamespace

import pytest

import _mp64_accel
from asm import assemble
from devices import AUDIO_BASE, MMIO_BASE
from system import MegapadSystem


def _system(code: bytes, *, cores: int = 1) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    return system


def test_multicycle_instruction_retires_only_at_exact_cycle_boundary():
    code = assemble("mul r1, r2\nhalt")
    sliced = _system(code)
    sliced.cpu.regs[1] = 6
    sliced.cpu.regs[2] = 7

    first = sliced.run_cycle_batch(3, max_instructions=10)

    assert first.system_stop_reason == "cycle_limit"
    assert first.system_cycles_advanced == 3
    assert first.instructions_executed == 0
    assert first.per_core_cycles == (0,)
    assert sliced.cpu.pc == 0
    assert sliced.cpu.regs[1] == 6
    assert sliced.cpu.cycle_count == 0
    assert sliced._native_system.cycle_execution_pending
    with pytest.raises(
        RuntimeError,
        match="suspended cycle execution requires",
    ):
        sliced.run_batch(1)

    second = sliced.run_cycle_batch(1, max_instructions=10)

    assert second.system_stop_reason == "cycle_limit"
    assert second.system_cycles_advanced == 1
    assert second.instructions_executed == 1
    assert second.per_core_cycles == (4,)
    assert sliced.cpu.pc == 2
    assert sliced.cpu.regs[1] == 42
    assert sliced.cpu.cycle_count == 4
    assert not sliced._native_system.cycle_execution_pending

    uninterrupted = _system(code)
    uninterrupted.cpu.regs[1] = 6
    uninterrupted.cpu.regs[2] = 7
    whole = uninterrupted.run_cycle_batch(4, max_instructions=10)

    assert whole.instructions_executed == 1
    assert (
        sliced.cpu.pc,
        sliced.cpu.regs[1],
        sliced.cpu.cycle_count,
    ) == (
        uninterrupted.cpu.pc,
        uninterrupted.cpu.regs[1],
        uninterrupted.cpu.cycle_count,
    )


def test_suspended_instruction_owns_core_bus_clock_and_scheduler_state():
    system = _system(assemble("mul r1, r2\nhalt"))
    system.cpu.regs[1] = 6
    system.cpu.regs[2] = 7
    system.run_cycle_batch(3, max_instructions=1)
    owner = system._native_system
    before = (
        system.cpu.pc,
        system.cpu.regs[1],
        system.cpu.cycle_count,
        owner.system_cycles,
        owner.scheduler_cursor,
        owner._main_bus_snapshot().next_grant_sequence,
    )

    blocked_mutations = (
        lambda: system.cpu.step(),
        lambda: system.advance_system_cycles(1),
        lambda: owner.advance_system_cycles(1),
        lambda: owner.advance_system_to(owner.system_cycles + 1),
        lambda: owner._main_bus_try_grant([]),
        lambda: owner._main_bus_reset(),
        lambda: setattr(owner, "scheduler_cursor", 0),
    )
    for mutation in blocked_mutations:
        with pytest.raises(RuntimeError, match="cycle execution|suspended"):
            mutation()

    assert (
        system.cpu.pc,
        system.cpu.regs[1],
        system.cpu.cycle_count,
        owner.system_cycles,
        owner.scheduler_cursor,
        owner._main_bus_snapshot().next_grant_sequence,
    ) == before

    resumed = system.run_cycle_batch(1, max_instructions=1)

    assert resumed.instructions_executed == 1
    assert system.cpu.regs[1] == 42
    assert not owner.cycle_execution_pending


def test_event_horizon_wins_tied_cycle_limit_and_remains_armed():
    system = _system(assemble("\n".join(["nop"] * 16)))
    owner = system._native_system
    owner.set_event_deadline(owner.EVENT_TIMER, 5)
    owner.set_event_deadline(owner.EVENT_EXTERNAL, 5)

    result = system.run_cycle_batch(5, max_instructions=100)

    expected_mask = (
        (1 << owner.EVENT_TIMER)
        | (1 << owner.EVENT_EXTERNAL)
    )
    assert result.system_stop_reason == "event_horizon"
    assert result.stop_cycle == 5
    assert result.system_cycles_advanced == 5
    assert result.instructions_executed == 5
    assert result.event_source_mask == expected_mask
    assert owner.event_horizon() == (5, 5, expected_mask)

    blocked = system.run_cycle_batch(10, max_instructions=100)

    assert blocked.system_stop_reason == "event_horizon"
    assert blocked.system_cycles_advanced == 0
    assert blocked.instructions_executed == 0
    assert blocked.event_source_mask == expected_mask
    assert system.cpu.pc == 5


def test_zero_budgets_and_terminal_system_reasons_are_exact():
    system = _system(assemble("nop\nhalt"))

    no_cycles = system.run_cycle_batch(0, max_instructions=10)
    no_instructions = system.run_cycle_batch(10, max_instructions=0)

    assert no_cycles.system_stop_reason == "cycle_limit"
    assert no_instructions.system_stop_reason == "instruction_limit"
    assert no_cycles.instructions_executed == 0
    assert no_instructions.instructions_executed == 0
    assert system.cpu.pc == 0
    assert system._native_system.system_cycles == 0

    halted = system.run_cycle_batch(10, max_instructions=10)

    assert halted.system_stop_reason == "all_halted"
    assert halted.instructions_executed == 2
    assert halted.stop_cycle == 2
    assert system.cpu.halted

    idle = _system(assemble("idl"))
    idled = idle.run_cycle_batch(10, max_instructions=10)

    assert idled.system_stop_reason == "all_idle"
    assert idled.instructions_executed == 1
    assert idled.stop_cycle == 1
    assert idle.cpu.idle


def test_cycle_api_does_not_adopt_an_external_active_bus_grant():
    system = _system(assemble("nop"))
    owner = system._native_system
    request = _mp64_accel.BusRequest(
        requester_id=0,
        ready_cycle=0,
        operation=_mp64_accel.BusOperation.READ,
        address=0x100,
        width=_mp64_accel.BusWidth.BYTE,
        write_data=0,
        ordering=_mp64_accel.BusOrderingMetadata(
            main_port_id=0,
            issue_sequence=1,
            port_io=False,
        ),
    )
    grant = owner._main_bus_try_grant([request])
    before = (
        system.cpu.pc,
        system.cpu.cycle_count,
        owner.system_cycles,
    )

    with pytest.raises(RuntimeError, match="cannot adopt an external"):
        system.run_cycle_batch(10, max_instructions=1)

    assert (
        system.cpu.pc,
        system.cpu.cycle_count,
        owner.system_cycles,
    ) == before
    assert owner._main_bus_snapshot().active_grant.grant_sequence == (
        grant.grant_sequence
    )

    owner._main_bus_reset()


def test_simultaneous_stores_use_equal_round_robin_and_persist_request():
    system = _system(
        assemble("st.b r1, r2\nhalt"),
        cores=2,
    )
    primary, secondary = system.cores
    for cpu, value in ((primary, 0x11), (secondary, 0x22)):
        cpu.regs[1] = 0x100
        cpu.regs[2] = value

    first = system.run_cycle_batch(1, max_instructions=2)

    assert first.per_core_instructions == (1, 0)
    assert primary.mem[0x100] == 0x11
    pending = system._native_system._cycle_pending_bus_requests()
    assert len(pending) == 1
    assert pending[0].requester_id == 1
    assert pending[0].ready_cycle == 0
    assert pending[0].ordering.main_port_id == 1
    assert pending[0].ordering.issue_sequence == 1
    snapshot = system._native_system._main_bus_snapshot()
    assert snapshot.last_grant == 0

    boundary = system.run_cycle_batch(1, max_instructions=1)

    assert boundary.instructions_executed == 0
    assert primary.mem[0x100] == 0x11
    persisted = system._native_system._cycle_pending_bus_requests()
    assert len(persisted) == 1
    assert (
        persisted[0].requester_id,
        persisted[0].ready_cycle,
        persisted[0].ordering.issue_sequence,
    ) == (1, 0, 1)

    final = system.run_cycle_batch(1, max_instructions=1)

    assert final.per_core_instructions == (0, 1)
    assert final.per_core_cycles == (0, 3)
    assert primary.mem[0x100] == 0x22
    assert not system._native_system.cycle_execution_pending
    assert system._native_system._main_bus_snapshot().last_grant == 1


def test_out_bridge_slices_do_not_duplicate_state_or_callbacks():
    system = _system(assemble("out1\nhalt"))
    cpu = system.cpu
    cpu.regs[2] = 0x100
    cpu.mem[0x100] = 0x41
    observed = []
    cpu.on_output = lambda port, value: observed.append((port, value))

    first = system.run_cycle_batch(1, max_instructions=1)

    assert first.instructions_executed == 0
    assert observed == []
    assert cpu.pc == 0
    assert cpu.regs[2] == 0x100
    assert cpu.port_out[1] == 0

    middle = system.run_cycle_batch(2, max_instructions=1)

    assert middle.instructions_executed == 0
    assert observed == []
    assert cpu.pc == 0
    assert cpu.regs[2] == 0x100
    assert cpu.port_out[1] == 0

    final = system.run_cycle_batch(1, max_instructions=1)

    assert final.instructions_executed == 1
    assert final.per_core_cycles == (4,)
    assert observed == [(1, 0x41)]
    assert cpu.pc == 1
    assert cpu.regs[2] == 0x101
    assert cpu.port_out[1] == 0x41


def test_partitioned_cmove_commits_each_target_byte_once():
    system = _system(assemble("cmove r7, r9\nhalt"))
    cpu = system.cpu
    cpu.regs[0] = 3
    cpu.regs[7] = MMIO_BASE + AUDIO_BASE
    cpu.regs[9] = 0x100
    cpu.mem[0x100:0x103] = b"abc"
    original_write = cpu._mmio_write8
    writes = []

    def counting_write(address, value):
        writes.append((address, value))
        original_write(address, value)

    cpu._mmio_write8 = counting_write
    retired = 0
    for _ in range(24):
        result = system.run_cycle_batch(1, max_instructions=1)
        retired += result.instructions_executed
        if retired:
            break

    assert retired == 1
    assert writes == [
        (MMIO_BASE + AUDIO_BASE + 0, ord("a")),
        (MMIO_BASE + AUDIO_BASE + 1, ord("b")),
        (MMIO_BASE + AUDIO_BASE + 2, ord("c")),
    ]
    assert cpu.pc == 3
    assert cpu.regs[0] == 0
    assert cpu.regs[7] == MMIO_BASE + AUDIO_BASE + 3
    assert cpu.regs[9] == 0x103
    assert not system._native_system.cycle_execution_pending


def test_wide_mmio_target_keeps_one_bus_transaction_and_byte_order():
    address = MMIO_BASE + 0x7FFF_0000
    system = _system(assemble("st.w r1, r2\nhalt"))
    system.cpu.regs[1] = address
    system.cpu.regs[2] = 0x4433_2211
    writes = []
    system.cpu._mmio_write8 = (
        lambda target, value: writes.append((target, value))
    )

    result = system.run_cycle_batch(4, max_instructions=1)

    assert result.instructions_executed == 1
    assert writes == [
        (address + 0, 0x11),
        (address + 1, 0x22),
        (address + 2, 0x33),
        (address + 3, 0x44),
    ]
    snapshot = system._native_system._main_bus_snapshot()
    assert snapshot.next_grant_sequence == 2
    assert snapshot.last_issue_sequences[0] == 1
    assert not system._native_system.cycle_execution_pending


def test_partial_wide_target_failure_is_recorded_and_never_replayed():
    address = MMIO_BASE + 0x7FFE_0000
    system = _system(assemble("st.w r1, r2\nhalt"))
    system.cpu.regs[1] = address
    system.cpu.regs[2] = 0x4433_2211
    writes = []

    def fail_on_second_byte(target, value):
        writes.append((target, value))
        if target == address + 1:
            raise ValueError("synthetic target failure")

    system.cpu._mmio_write8 = fail_on_second_byte

    with pytest.raises(RuntimeError, match="synthetic target failure"):
        system.run_cycle_batch(4, max_instructions=1)

    owner = system._native_system
    assert writes == [
        (address + 0, 0x11),
        (address + 1, 0x22),
    ]
    assert system.cpu.pc == 0
    assert system.cpu.cycle_count == 0
    assert owner.system_cycles == 1
    assert owner.cycle_execution_pending
    assert owner._main_bus_snapshot().active_grant is None

    with pytest.raises(RuntimeError, match="synthetic target failure"):
        system.run_cycle_batch(4, max_instructions=1)

    assert writes == [
        (address + 0, 0x11),
        (address + 1, 0x22),
    ]
    assert owner.system_cycles == 1

    system.boot(entry=0)
    assert not owner.cycle_execution_pending


def test_partitioned_field_operand_uses_four_ordered_dword_requests():
    code = assemble("gf.add\nhalt")
    sliced = _system(code)
    sliced.cpu.acc[0] = 5
    sliced.cpu.tsrc0 = 0x100
    sliced.cpu.mem[0x100:0x120] = (7).to_bytes(32, "little")

    first = sliced.run_cycle_batch(3, max_instructions=1)
    second = sliced.run_cycle_batch(7, max_instructions=1)

    assert first.instructions_executed == 0
    assert second.instructions_executed == 1
    assert sliced.cpu.acc[0] == 12
    assert sliced.cpu.cycle_count == 10
    snapshot = sliced._native_system._main_bus_snapshot()
    assert snapshot.next_grant_sequence == 5
    assert snapshot.last_issue_sequences[0] == 4
    assert not sliced._native_system.cycle_execution_pending

    uninterrupted = _system(code)
    uninterrupted.cpu.acc[0] = 5
    uninterrupted.cpu.tsrc0 = 0x100
    uninterrupted.cpu.mem[0x100:0x120] = (7).to_bytes(32, "little")
    whole = uninterrupted.run_cycle_batch(10, max_instructions=1)

    assert whole.instructions_executed == 1
    assert (
        sliced.cpu.pc,
        tuple(sliced.cpu.acc),
        sliced.cpu.cycle_count,
    ) == (
        uninterrupted.cpu.pc,
        tuple(uninterrupted.cpu.acc),
        uninterrupted.cpu.cycle_count,
    )


def test_partitioned_sha_round_matches_one_shot_and_eight_dword_loads():
    code = assemble("sha.init 0\nsha.round\nhalt")
    payload = bytes(range(64))

    sliced = _system(code)
    sliced.cpu.tsrc0 = 0x100
    sliced.cpu.mem[0x100:0x140] = payload
    partitions = (3, 4, 4, 57)
    results = [
        sliced.run_cycle_batch(limit, max_instructions=2)
        for limit in partitions
    ]

    assert [result.instructions_executed for result in results] == [
        1,
        0,
        0,
        1,
    ]
    assert sum(
        result.per_core_cycles[0]
        for result in results
    ) == 68
    assert sliced.cpu.cycle_count == 68
    snapshot = sliced._native_system._main_bus_snapshot()
    assert snapshot.next_grant_sequence == 9
    assert snapshot.last_issue_sequences[0] == 8
    assert not sliced._native_system.cycle_execution_pending

    uninterrupted = _system(code)
    uninterrupted.cpu.tsrc0 = 0x100
    uninterrupted.cpu.mem[0x100:0x140] = payload
    whole = uninterrupted.run_cycle_batch(68, max_instructions=2)

    assert whole.instructions_executed == 2
    assert (
        sliced.cpu.pc,
        tuple(sliced.cpu.acc),
        sliced.cpu.flag_z,
        sliced.cpu.cycle_count,
    ) == (
        uninterrupted.cpu.pc,
        tuple(uninterrupted.cpu.acc),
        uninterrupted.cpu.flag_z,
        uninterrupted.cpu.cycle_count,
    )


def test_cycle_api_rejects_unresolved_full_core_mex_before_mutation():
    system = _system(assemble("t.add\nhalt"))
    system.cpu.tsrc0 = 0x100
    system.cpu.tsrc1 = 0x140
    system.cpu.tdst = 0x180
    system.cpu.mem[0x100:0x1C0] = bytes(range(192))
    before = (
        system.cpu.pc,
        system.cpu.cycle_count,
        bytes(system.cpu.mem[0x100:0x1C0]),
        system._native_system.system_cycles,
        system._native_system._main_bus_snapshot().next_grant_sequence,
    )

    with pytest.raises(RuntimeError, match="tile topology remains unresolved"):
        system.run_cycle_batch(100, max_instructions=1)

    assert (
        system.cpu.pc,
        system.cpu.cycle_count,
        bytes(system.cpu.mem[0x100:0x1C0]),
        system._native_system.system_cycles,
        system._native_system._main_bus_snapshot().next_grant_sequence,
    ) == before
    assert not system._native_system.cycle_execution_pending


def test_cold_boot_cancels_suspended_execution_and_restores_bus_credit():
    system = _system(
        assemble("st.b r1, r2\nhalt"),
        cores=2,
    )
    for cpu, value in zip(system.cores, (0x11, 0x22)):
        cpu.regs[1] = 0x100
        cpu.regs[2] = value
    system.run_cycle_batch(1, max_instructions=2)

    assert system._native_system.cycle_execution_pending

    system.boot(entry=0)

    snapshot = system._native_system._main_bus_snapshot()
    assert not system._native_system.cycle_execution_pending
    assert system._native_system._cycle_pending_bus_requests() == []
    assert snapshot.active_grant is None
    assert snapshot.next_grant_sequence == 1
    assert snapshot.reset_port_zero_credit
    assert snapshot.last_issue_sequences == [0, 0, 0, 0]


def test_phase0_oracle_captures_bus_state_and_requires_quiescence():
    import bench_phase0_concurrency as phase0

    system = _system(
        assemble("st.b r1, r2\nhalt"),
        cores=2,
    )
    for cpu, value in zip(system.cores, (0x11, 0x22)):
        cpu.regs[1] = 0x100
        cpu.regs[2] = value
    system.run_cycle_batch(1, max_instructions=2)

    bus_state = phase0._main_bus_state(system)

    assert phase0.STATE_SCHEMA_VERSION == 6
    assert bus_state["cycle_execution_pending"]
    assert len(bus_state["cycle_pending_requests"]) == 1
    assert bus_state["last_issue_sequences"] == [1, 0, 0, 0]
    with pytest.raises(RuntimeError, match="requires quiescent"):
        phase0._state_observation(
            SimpleNamespace(system=system, metrics={})
        )

    system.boot(entry=0)
    observation = phase0._state_observation(
        SimpleNamespace(system=system, metrics={})
    )

    captured = observation["canonical_state"]["shared_devices"]["main_bus"]
    assert captured["cycle_execution_pending"] is False
    assert captured["cycle_pending_requests"] == []
    assert captured["next_grant_sequence"] == 1


def test_cycle_api_rejects_invalid_or_unsupported_calls_before_mutation():
    system = _system(assemble("nop"))
    before = (
        system.cpu.pc,
        system.cpu.cycle_count,
        system._native_system.system_cycles,
    )

    with pytest.raises(ValueError, match="cannot be negative"):
        system.run_cycle_batch(-1)
    with pytest.raises(ValueError, match="cannot be negative"):
        system.run_cycle_batch(1, max_instructions=-1)

    assert (
        system.cpu.pc,
        system.cpu.cycle_count,
        system._native_system.system_cycles,
    ) == before

    heterogeneous = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    heterogeneous.load_binary(0, assemble("nop"))
    heterogeneous.boot(entry=0)
    with pytest.raises(RuntimeError, match="without micro-core clusters"):
        heterogeneous.run_cycle_batch(1)
    assert heterogeneous.cpu.pc == 0
    assert heterogeneous.cpu.cycle_count == 0

    overflow = _system(assemble("nop"))
    overflow.advance_system_cycles(1)
    before_overflow = (
        overflow.cpu.pc,
        overflow.cpu.cycle_count,
        overflow._native_system.system_cycles,
    )
    with pytest.raises(OverflowError, match="cycle batch deadline"):
        overflow.run_cycle_batch((1 << 64) - 1)
    assert (
        overflow.cpu.pc,
        overflow.cpu.cycle_count,
        overflow._native_system.system_cycles,
    ) == before_overflow
