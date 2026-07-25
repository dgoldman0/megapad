"""Phase 2 resumable full-core and cycle-boundary oracles."""

from __future__ import annotations

import threading
from types import SimpleNamespace

import pytest

import _mp64_accel
from asm import assemble
from devices import AUDIO_BASE, MMIO_BASE, NIC_BASE, UART_BASE
from system import MegapadSystem


def _system(
    code: bytes,
    *,
    cores: int = 1,
    realtime_clock: bool = False,
) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        realtime_clock=realtime_clock,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    return system


def _install_vector(
    system: MegapadSystem,
    vector: int,
    *,
    handler: int = 0x300,
) -> int:
    system.cpu.ivt_base = 0x200
    entry = system.cpu.ivt_base + vector * 8
    system.cpu.mem[entry:entry + 8] = handler.to_bytes(8, "little")
    return handler


def _run_until_interrupt(
    system: MegapadSystem,
    *,
    max_slices: int = 16,
):
    saw_suspended_entry = system._native_system.cycle_execution_pending
    for _ in range(max_slices):
        result = system.run_cycle_batch(1, max_instructions=1)
        saw_suspended_entry |= (
            system._native_system.cycle_execution_pending
        )
        if result.interrupts_delivered:
            return result, saw_suspended_entry
    pytest.fail("interrupt entry did not finish within bounded cycle slices")


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
        lambda: setattr(system.rtc, "realtime", True),
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


def test_warm_boot_cancels_suspended_execution_and_restores_bus_credit():
    system = _system(
        assemble("st.b r1, r2\nhalt"),
        cores=2,
    )
    for cpu, value in zip(system.cores, (0x11, 0x22)):
        cpu.regs[1] = 0x100
        cpu.regs[2] = value
    system.run_cycle_batch(1, max_instructions=2)

    assert system._native_system.cycle_execution_pending
    event_cycle = system._native_system.system_cycles + 5
    sequence = system.schedule_uart_input(b"after-boot", at_cycle=event_cycle)

    system.boot(entry=0)

    snapshot = system._native_system._main_bus_snapshot()
    assert not system._native_system.cycle_execution_pending
    assert system._native_system._cycle_pending_bus_requests() == []
    assert snapshot.active_grant is None
    assert snapshot.next_grant_sequence == 1
    assert snapshot.reset_port_zero_credit
    assert snapshot.last_issue_sequences == [0, 0, 0, 0]
    pending = system._native_system.external_event_pending
    assert [(event.sequence, event.cycle) for event in pending] == [
        (sequence, event_cycle)
    ]


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

    assert phase0.SCHEMA_VERSION == 6
    assert phase0.STATE_SCHEMA_VERSION == 7
    assert bus_state["cycle_execution_pending"]
    assert len(bus_state["cycle_pending_requests"]) == 1
    assert bus_state["last_issue_sequences"] == [1, 0, 0, 0]
    with pytest.raises(RuntimeError, match="requires quiescent"):
        phase0._state_observation(
            SimpleNamespace(system=system, metrics={})
        )

    system.boot(entry=0)
    event_cycle = system._native_system.system_cycles + 3
    system.schedule_uart_input(b"oracle", at_cycle=event_cycle)
    observation = phase0._state_observation(
        SimpleNamespace(system=system, metrics={})
    )

    captured = observation["canonical_state"]["shared_devices"]["main_bus"]
    assert captured["cycle_execution_pending"] is False
    assert captured["cycle_pending_requests"] == []
    assert captured["next_grant_sequence"] == 1
    journal = observation["canonical_state"]["shared_devices"][
        "external_events"
    ]
    assert journal["next_cycle"] == event_cycle
    assert journal["next_sequence"] == 2
    assert journal["pending"] == journal["history"]
    assert journal["pending"][0]["kind"] == "uart_rx"
    assert journal["pending"][0]["payload"] == phase0._blob_summary(
        b"oracle"
    )


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


def test_timer_frontier_wakes_idle_core_through_resumable_interrupt_entry():
    system = _system(assemble("nop"))
    cpu = system.cpu
    owner = system._native_system
    handler = _install_vector(system, 7)
    initial_sp = cpu.sp
    cpu.flag_i = True
    initial_flags = cpu.flags_pack()
    cpu.idle = True
    system.timer.counter = 0
    system.timer.compare = 2
    system.timer.status = 0
    system.timer.irq_pending = False
    system.timer.control = 0x03

    before_frontier = system.run_cycle_batch(1, max_instructions=1)

    assert before_frontier.system_cycles_advanced == 1
    assert before_frontier.interrupts_delivered == 0
    assert system.timer.counter == 1
    assert not system.timer.irq_pending
    assert cpu.idle

    at_frontier = system.run_cycle_batch(1, max_instructions=1)

    assert at_frontier.stop_cycle == 2
    assert at_frontier.instructions_executed == 0
    assert at_frontier.interrupts_delivered == 0
    assert system.timer.counter == 2
    assert system.timer.status & 0x01
    assert system.timer.irq_pending
    assert cpu.pc == 0
    assert cpu.sp == initial_sp
    assert cpu.idle
    assert not owner.cycle_execution_pending

    delivered, saw_suspended_entry = _run_until_interrupt(system)

    assert saw_suspended_entry
    assert delivered.interrupts_delivered == 1
    assert delivered.per_core_interrupts == (1,)
    assert delivered.per_core_cycles == (7,)
    assert delivered.instructions_executed == 0
    assert cpu.pc == handler
    assert cpu.sp == initial_sp - 16
    assert cpu.ivec_id == 7
    assert cpu.cycle_count == 7
    assert owner.system_cycles == 9
    assert not cpu.flag_i
    assert not cpu.idle
    assert int.from_bytes(
        cpu.mem[initial_sp - 16:initial_sp - 8],
        "little",
    ) == 0
    assert int.from_bytes(
        cpu.mem[initial_sp - 8:initial_sp],
        "little",
    ) == initial_flags
    bus = owner._main_bus_snapshot()
    assert bus.next_grant_sequence == 4
    assert bus.last_issue_sequences[0] == 3
    assert not owner.cycle_execution_pending
    assert system.timer.status & 0x01
    assert system.timer.irq_pending


def test_simultaneous_ipi_precedes_timer_without_acknowledging_timer():
    system = _system(assemble("nop"), cores=2)
    primary, secondary = system.cores
    owner = system._native_system
    ipi_handler = _install_vector(system, 8)
    timer_handler = 0x380
    secondary.ivt_base = primary.ivt_base
    timer_entry = primary.ivt_base + 7 * 8
    primary.mem[timer_entry:timer_entry + 8] = timer_handler.to_bytes(
        8,
        "little",
    )
    primary.sp = 0x1000
    secondary.sp = 0xE00
    for cpu in (primary, secondary):
        cpu.flag_i = True
        cpu.idle = True
    system.timer.status = 0x01
    system.timer.irq_pending = True
    owner.ipi_send(1, 0)

    first = system.run_cycle_batch(3, max_instructions=1)

    assert first.interrupts_delivered == 0
    assert first.per_core_interrupts == (0, 0)
    assert owner.cycle_execution_pending
    bus = owner._main_bus_snapshot()
    assert bus.next_grant_sequence == 3
    assert bus.last_issue_sequences[:2] == [1, 1]
    assert bus.last_grant == 1

    delivered, _ = _run_until_interrupt(system)

    assert delivered.interrupts_delivered == 1
    assert delivered.per_core_interrupts == (1, 0)
    assert delivered.per_core_cycles == (9, 0)
    assert primary.pc == ipi_handler
    assert primary.ivec_id == 8
    assert secondary.pc == 0
    assert secondary.ivec_id == 0
    assert owner.ipi_pending_mask(0) == 1 << 1
    assert primary.irq_ipi
    assert system.timer.status & 0x01
    assert system.timer.irq_pending
    assert owner.cycle_execution_pending


def test_unhandled_peer_does_not_block_valid_simultaneous_interrupt_entry():
    system = _system(assemble("nop"), cores=2)
    primary, secondary = system.cores
    handler = _install_vector(system, 7)
    primary.sp = 0x1000
    secondary.sp = 0xE00
    for cpu in (primary, secondary):
        cpu.flag_i = True
        cpu.idle = True
    system.timer.status = 0x01
    system.timer.irq_pending = True
    secondary_before = (
        secondary.pc,
        secondary.sp,
        secondary.flags_pack(),
        secondary.ivec_id,
        secondary.cycle_count,
    )

    result = system.run_cycle_batch(16, max_instructions=1)

    assert result.system_stop_reason == "unhandled_interrupt"
    assert result.pending_interrupt_core == 1
    assert result.pending_interrupt_vector == 7
    assert result.interrupts_delivered == 1
    assert result.per_core_interrupts == (1, 0)
    assert primary.pc == handler
    assert primary.ivec_id == 7
    assert (
        secondary.pc,
        secondary.sp,
        secondary.flags_pack(),
        secondary.ivec_id,
        secondary.cycle_count,
    ) == secondary_before


def test_arriving_interrupt_waits_for_suspended_guest_instruction():
    system = _system(assemble("mul r1, r2\nhalt"), cores=2)
    primary, secondary = system.cores
    owner = system._native_system
    handler = _install_vector(system, 8)
    primary.regs[1] = 6
    primary.regs[2] = 7
    primary.flag_i = True
    secondary.halted = True

    suspended = system.run_cycle_batch(2, max_instructions=1)

    assert suspended.instructions_executed == 0
    assert primary.pc == 0
    assert primary.regs[1] == 6
    assert owner.cycle_execution_pending

    owner.ipi_send(1, 0)
    retired = system.run_cycle_batch(2, max_instructions=1)

    assert retired.instructions_executed == 1
    assert retired.interrupts_delivered == 0
    assert primary.pc == 2
    assert primary.regs[1] == 42
    assert primary.flag_i
    assert primary.ivec_id == 0
    assert primary.irq_ipi

    delivered, _ = _run_until_interrupt(system)

    assert delivered.interrupts_delivered == 1
    assert primary.pc == handler
    assert primary.ivec_id == 8
    assert not primary.halted


def test_zero_cycle_or_instruction_budget_preserves_pending_inputs_and_lines():
    system = _system(assemble("nop"))
    cpu = system.cpu
    owner = system._native_system
    cpu.flag_i = True
    cpu.idle = True
    cpu.irq_ipi = True
    system.timer.status = 0x01
    system.timer.irq_pending = True
    system.uart.inject_input(b"U")
    assert system.nic.inject_frame(b"N")
    # Immediate ingress correctly wakes core 0. Re-enter WFI so this oracle
    # specifically proves that a zero-budget execution call does not wake it.
    cpu.idle = True

    def snapshot():
        return (
            tuple(cpu.regs),
            cpu.flags_pack(),
            cpu.priv_level,
            cpu.halted,
            cpu.idle,
            cpu.ivec_id,
            cpu.cycle_count,
            owner.system_cycles,
            owner.scheduler_cursor,
            owner.cycle_execution_pending,
            owner._main_bus_snapshot().next_grant_sequence,
            system.timer.counter,
            system.timer.status,
            system.timer.irq_pending,
            cpu.irq_ipi,
            system.uart.rx_pending,
            cpu._cs.nic_rx_queue_size(),
        )

    before = snapshot()
    no_cycles = system.run_cycle_batch(0, max_instructions=1)
    no_instructions = system.run_cycle_batch(4, max_instructions=0)

    assert no_cycles.system_stop_reason == "cycle_limit"
    assert no_instructions.system_stop_reason == "instruction_limit"
    for result in (no_cycles, no_instructions):
        assert result.system_cycles_advanced == 0
        assert result.instructions_executed == 0
        assert result.interrupts_delivered == 0
        assert result.per_core_interrupts == (0,)
        assert result.external_events_applied == 0
        assert result.pending_interrupt_core == -1
        assert result.pending_interrupt_vector == -1
    assert snapshot() == before


def test_live_ingress_gate_linearizes_arrivals_on_both_sides_of_close():
    system = _system(assemble("nop"))
    owner = system._native_system
    release_post_close = threading.Event()
    worker_started = threading.Event()
    worker_result = []

    def post_close_injector():
        worker_started.set()
        release_post_close.wait()
        worker_result.append(system.schedule_uart_input(b"B"))

    worker = threading.Thread(target=post_close_injector, daemon=True)
    with system._scheduler_lock:
        owner._begin_external_event_staging()
        staged_sequence = system.schedule_uart_input(b"A")
        assert owner.external_event_pending == []
        assert owner.external_event_history == []

        worker.start()
        assert worker_started.wait(timeout=2)
        assert owner._close_external_event_staging() == 1
        release_post_close.set()

    worker.join(timeout=2)

    assert not worker.is_alive()
    assert staged_sequence == 1
    assert worker_result == [2]
    assert owner.external_event_pending == []
    assert [
        (
            event.sequence,
            event.cycle,
            bytes(event.payload),
        )
        for event in owner.external_event_history
    ] == [(1, 0, b"A"), (2, 0, b"B")]
    assert system.uart.read8(0x01) == ord("A")
    assert system.uart.read8(0x01) == ord("B")


def test_nic_transport_starts_only_after_journal_route_is_installed():
    frame = b"eager-backend-frame"

    class EagerBackend:
        def __init__(self):
            self.on_rx_frame = None
            self._up = False

        @property
        def link_up(self):
            return self._up

        def start(self):
            self._up = True
            assert self.on_rx_frame is not None
            self.on_rx_frame(frame)

        def stop(self):
            self._up = False

        def send(self, _frame):
            return True

    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        nic_backend=EagerBackend(),
    )

    assert system.cpu._cs.nic_rx_queue_size() == 1
    history = system._native_system.external_event_history
    assert len(history) == 1
    assert history[0].kind == _mp64_accel.ExternalEventKind.NIC_RX
    assert bytes(history[0].payload) == frame


def test_system_nic_invalid_ingress_preserves_false_and_sticky_error():
    system = _system(assemble("nop"))

    assert not system.nic.inject_frame(b"")
    assert system.cpu._cs.nic_rx_queue_size() == 0
    assert system.cpu._cs.nic_read8(NIC_BASE + 0x01) & 0x08
    assert system._native_system.external_event_history == []


def test_timestamped_external_events_apply_at_cycle_then_wake_without_vector():
    system = _system(assemble("nop"))
    cpu = system.cpu
    owner = system._native_system
    cpu.flag_i = True
    cpu.idle = True
    system.uart_geom.cols = 80
    system.uart_geom.rows = 24
    system.uart_geom.status = 0

    sequences = (
        system.schedule_uart_input(b"A", at_cycle=3),
        system.schedule_nic_frame(b"N", at_cycle=3),
        system.schedule_terminal_resize(100, 30, at_cycle=3),
        system.schedule_uart_input(b"B", at_cycle=3),
        system.schedule_terminal_resize(120, 40, at_cycle=3),
    )
    expected = (
        (3, 1, _mp64_accel.ExternalEventKind.UART_RX, b"A", 0, 0),
        (3, 2, _mp64_accel.ExternalEventKind.NIC_RX, b"N", 0, 0),
        (
            3,
            3,
            _mp64_accel.ExternalEventKind.UART_GEOMETRY,
            b"",
            100,
            30,
        ),
        (3, 4, _mp64_accel.ExternalEventKind.UART_RX, b"B", 0, 0),
        (
            3,
            5,
            _mp64_accel.ExternalEventKind.UART_GEOMETRY,
            b"",
            120,
            40,
        ),
    )

    def event_rows(records):
        return tuple(
            (
                record.cycle,
                record.sequence,
                record.kind,
                bytes(record.payload),
                record.argument0,
                record.argument1,
            )
            for record in records
        )

    assert sequences == (1, 2, 3, 4, 5)
    assert event_rows(owner.external_event_pending) == expected
    assert event_rows(owner.external_event_history) == expected
    with pytest.raises(RuntimeError, match="pending external event"):
        owner.advance_system_cycles(3)
    with pytest.raises(RuntimeError, match="pending external event"):
        owner.advance_system_to(3)
    with pytest.raises(RuntimeError, match="pending external events"):
        system._run_native_full_core_batch(1)
    assert owner.system_cycles == 0
    assert event_rows(owner.external_event_pending) == expected

    before = system.run_cycle_batch(2, max_instructions=1)

    assert before.stop_cycle == 2
    assert before.external_events_applied == 0
    assert system.uart.rx_pending == 0
    assert cpu._cs.nic_rx_queue_size() == 0
    assert (system.uart_geom.cols, system.uart_geom.rows) == (80, 24)
    assert cpu.idle

    applied = system.run_cycle_batch(1, max_instructions=1)

    assert applied.stop_cycle == 3
    assert applied.instructions_executed == 0
    assert applied.external_events_applied == 5
    assert applied.interrupts_delivered == 0
    assert applied.pending_interrupt_core == -1
    assert applied.pending_interrupt_vector == -1
    assert cpu.pc == 0
    assert cpu.ivec_id == 0
    assert not cpu.idle
    assert system.uart.rx_pending == 2
    assert system.uart.read8(0x01) == ord("A")
    assert system.uart.read8(0x01) == ord("B")
    assert cpu._cs.nic_rx_queue_size() == 1
    assert (system.uart_geom.cols, system.uart_geom.rows) == (120, 40)
    assert event_rows(owner.external_event_pending) == ()
    assert event_rows(owner.external_event_history) == expected


def test_exact_external_frontier_beyond_uint32_advances_in_one_transition():
    system = _system(assemble("nop"))
    system.cpu.idle = True
    event_cycle = (1 << 32) + 3
    system.schedule_uart_input(b"L", at_cycle=event_cycle)

    result = system.run_cycle_batch(event_cycle, max_instructions=1)

    assert result.stop_cycle == event_cycle
    assert result.system_cycles_advanced == event_cycle
    assert result.external_events_applied == 1
    assert system._native_system.system_cycles == event_cycle
    assert system.uart.read8(0x01) == ord("L")


def test_same_cycle_uart_read_samples_before_timestamped_host_input():
    system = _system(assemble("ld.b r4, r1\nhalt"))
    system.cpu.regs[1] = MMIO_BASE + UART_BASE + 1
    system.schedule_uart_input(b"X", at_cycle=1)

    result = system.run_cycle_batch(1, max_instructions=1)

    assert result.instructions_executed == 1
    assert result.external_events_applied == 1
    assert system.cpu.regs[4] == 0
    assert system.uart.rx_pending == 1
    assert system.uart.read8(0x01) == ord("X")


def test_internal_timer_stop_never_exposes_future_multicycle_core_state():
    system = _system(assemble("mul r1, r2\nhalt"), cores=2)
    primary, secondary = system.cores
    primary.regs[1] = 6
    primary.regs[2] = 7
    secondary.flag_i = True
    secondary.idle = True
    system.timer.counter = 0
    system.timer.compare = 2
    system.timer.status = 0
    system.timer.irq_pending = False
    system.timer.control = 0x03

    result = system.run_cycle_batch(8, max_instructions=1)

    assert result.system_stop_reason == "unhandled_interrupt"
    assert result.stop_cycle == 2
    assert result.pending_interrupt_core == 1
    assert result.pending_interrupt_vector == 7
    assert primary.pc == 0
    assert primary.regs[1] == 6
    assert primary.cycle_count == 0
    assert system._native_system.cycle_execution_pending


def test_instruction_cap_drains_owned_bus_target_without_starting_guest_work():
    system = _system(assemble("nop\nhalt"), cores=2)
    interrupt_core, guest_core = system.cores
    handler = _install_vector(system, 8)
    interrupt_core.sp = 0x1000
    interrupt_core.flag_i = True
    interrupt_core.idle = True
    guest_core.pc = 0
    guest_core.halted = False
    guest_core.idle = False
    system._native_system.ipi_send(1, 0)

    result = system.run_cycle_batch(16, max_instructions=1)

    assert result.system_stop_reason == "instruction_limit"
    assert result.stop_cycle == 1
    assert result.instructions_executed == 1
    assert result.interrupts_delivered == 0
    assert guest_core.pc == 1
    assert interrupt_core.pc == 0
    assert interrupt_core.sp == 0x1000
    assert system._native_system.system_cycles == 1
    bus = system._native_system._main_bus_snapshot()
    assert bus.active_grant is None
    assert bus.next_grant_sequence == 2
    pending = system._native_system._cycle_pending_bus_requests()
    assert len(pending) == 1
    assert pending[0].requester_id == 0
    assert system._native_system.cycle_execution_pending

    delivered, _ = _run_until_interrupt(system)
    assert delivered.interrupts_delivered == 1
    assert interrupt_core.pc == handler


def test_missing_ivt_reports_unhandled_interrupt_without_core_mutation():
    system = _system(assemble("nop"))
    cpu = system.cpu
    owner = system._native_system
    cpu.flag_i = True
    cpu.idle = True
    system.timer.status = 0x01
    system.timer.irq_pending = True
    before = (
        tuple(cpu.regs),
        cpu.flags_pack(),
        cpu.priv_level,
        cpu.halted,
        cpu.idle,
        cpu.ivt_base,
        cpu.ivec_id,
        cpu.cycle_count,
        bytes(cpu.mem),
        owner.system_cycles,
        owner.scheduler_cursor,
        owner._main_bus_snapshot().next_grant_sequence,
    )

    result = system.run_cycle_batch(8, max_instructions=1)

    assert result.system_stop_reason == "unhandled_interrupt"
    assert result.system_cycles_advanced == 0
    assert result.stop_cycle == 0
    assert result.instructions_executed == 0
    assert result.interrupts_delivered == 0
    assert result.per_core_interrupts == (0,)
    assert result.pending_interrupt_core == 0
    assert result.pending_interrupt_vector == 7
    assert not owner.cycle_execution_pending
    assert system.timer.status & 0x01
    assert system.timer.irq_pending
    assert (
        tuple(cpu.regs),
        cpu.flags_pack(),
        cpu.priv_level,
        cpu.halted,
        cpu.idle,
        cpu.ivt_base,
        cpu.ivec_id,
        cpu.cycle_count,
        bytes(cpu.mem),
        owner.system_cycles,
        owner.scheduler_cursor,
        owner._main_bus_snapshot().next_grant_sequence,
    ) == before


def test_cycle_execution_rejects_realtime_rtc_before_mutation():
    system = _system(assemble("nop"), realtime_clock=True)
    before = (
        system.cpu.pc,
        system.cpu.cycle_count,
        system._native_system.system_cycles,
    )

    with pytest.raises(RuntimeError, match="realtime"):
        system.run_cycle_batch(1, max_instructions=1)
    with pytest.raises(RuntimeError, match="realtime"):
        system._run_native_full_core_cycle_batch(1, 1)

    assert (
        system.cpu.pc,
        system.cpu.cycle_count,
        system._native_system.system_cycles,
    ) == before
