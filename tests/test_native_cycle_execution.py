"""Phase 2 resumable full-core and cycle-boundary oracles."""

from __future__ import annotations

import threading
from types import SimpleNamespace

import pytest

import _mp64_accel
from asm import assemble
from devices import (
    AUDIO_BASE,
    MMIO_BASE,
    NIC_BASE,
    SECTOR_SIZE,
    STORAGE_BASE,
    STORAGE_CMD_READ,
    STORAGE_CMD_RESET,
    STORAGE_CMD_WRITE,
    STORAGE_RESULT_DMA_FAILURE,
    STORAGE_RESULT_DMA_INVALID,
    STORAGE_RESULT_OK,
    STORAGE_RESULT_PARTIAL,
    STORAGE_RESULT_RESET_ABORTED,
    STORAGE_STATUS_BUSY,
    STORAGE_STATUS_PRESENT,
    STORAGE_STATUS_RESULT_VALID,
    UART_BASE,
)
from system import MegapadSystem


def _prime_instruction_cache(
    system: MegapadSystem,
    address: int,
    size: int,
) -> None:
    """Start a data/timing oracle from an explicitly warm guest cache."""
    if size <= 0:
        return
    first_line = address & ~0xF
    last_line = (address + size - 1) & ~0xF
    for cpu in system.cores[:system.num_full_cores]:
        valid_bytes, tags, data_bytes = cpu._cs.icache_snapshot()
        valid = bytearray(valid_bytes)
        tags = list(tags)
        data = bytearray(data_bytes)
        line_address = first_line
        while line_address <= last_line:
            index = (line_address >> 4) & 0xFF
            valid[index] = 1
            tags[index] = line_address >> 12
            data_offset = index * 16
            for byte_offset in range(16):
                data[data_offset + byte_offset] = cpu.mem[
                    (line_address + byte_offset) % cpu.mem_size
                ]
            line_address += 16
        cpu._cs.icache_restore(bytes(valid), tags, bytes(data))


def _system(
    code: bytes,
    *,
    cores: int = 1,
    realtime_clock: bool = False,
    storage_image: str | None = None,
    cold_instruction_cache: bool = False,
) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        realtime_clock=realtime_clock,
        storage_image=storage_image,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    if not cold_instruction_cache:
        _prime_instruction_cache(system, 0, len(code))
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

    assert phase0.SCHEMA_VERSION == 10
    assert phase0.STATE_SCHEMA_VERSION == 9
    assert bus_state["arbitration_contract"] == {
        "hard_qos_role": (
            "determines must/may eligibility and reserved entitlement only"
        ),
        "simultaneously_eligible_peer_order": "equal_round_robin",
        "unused_reserved_capacity": "work_conserving",
        "best_effort_weights": "none",
        "secondary_ordering_biases": [],
    }
    assert bus_state["cycle_execution_pending"]
    assert len(bus_state["cycle_pending_requests"]) == 1
    assert bus_state["last_issue_sequences"] == [1, 0, 0, 0]
    assert bus_state["dma_coordinator"] == {
        "schema_version": 1,
        "endpoints": [
            {
                "requester_id": -1,
                "main_bus_port_id": 2,
                "next_issue_sequence": 1,
                "highest_observed_token": 0,
                "timeline_active": False,
                "pending_token": None,
                "pending_request": None,
            },
            {
                "requester_id": -2,
                "main_bus_port_id": 3,
                "next_issue_sequence": 1,
                "highest_observed_token": 0,
                "timeline_active": False,
                "pending_token": None,
                "pending_request": None,
            },
        ],
    }
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
    storage_dma = observation["canonical_state"]["shared_devices"][
        "storage"
    ]["dma_fsm"]
    assert storage_dma["next_token"] == 1
    assert storage_dma["pending"] is None
    nic_dma = observation["canonical_state"][
        "native_devices_per_full_core"
    ][0]["nic"]["cycle_dma"]
    assert nic_dma["next_token"] == 1
    assert nic_dma["pending"] is None
    journal = observation["canonical_state"]["shared_devices"][
        "external_events"
    ]
    assert journal["next_cycle"] == event_cycle
    assert journal["next_sequence"] == 2
    assert journal["completed_batch_boundaries"] == 1
    assert journal["next_before_cycle"] is None
    assert journal["replay_sealed"] is False
    assert journal["pending"] == journal["history"]
    assert journal["pending"][0]["kind"] == "uart_rx"
    assert journal["pending"][0]["payload"] == phase0._blob_summary(
        b"oracle"
    )
    assert journal["pending"][0]["release_boundary"] == 0
    assert journal["pending"][0]["release_phase"] == "scheduler"


def test_native_nic_dma_snapshot_rejects_active_system_batch():
    system = _system(assemble("st.b r1, r2\nhalt"))
    system.cpu.regs[1] = MMIO_BASE + STORAGE_BASE
    system.cpu.regs[2] = STORAGE_CMD_RESET
    snapshot_errors = []
    original_write8 = system.storage.write8

    def write8_with_reentrant_snapshot(offset, value):
        original_write8(offset, value)
        if offset == 0x00:
            try:
                system.cpu._cs.nic_cycle_dma_snapshot()
            except RuntimeError as error:
                snapshot_errors.append(str(error))

    system.storage.write8 = write8_with_reentrant_snapshot

    result = system.run_cycle_batch(16, max_instructions=10)

    assert result.system_stop_reason == "all_halted"
    assert snapshot_errors == [
        "native NIC DMA state cannot be observed during an "
        "active native system batch"
    ]


@pytest.mark.parametrize(
    "payload_bytes",
    [SECTOR_SIZE, SECTOR_SIZE * 2],
)
def test_phase0_strict_dma_probe_uses_real_equal_round_robin_ports(
    payload_bytes,
):
    import bench_phase0_concurrency as phase0

    report = phase0._strict_nic_disk_dma_report(
        payload_bytes,
        repeats=1,
        warmups=0,
    )

    assert all(report["validation"].values())
    sample = report["timed_samples"][0]
    assert sample["stop_reason"] == "all_halted"
    assert sample["payload_bytes_per_endpoint"] == payload_bytes
    assert sample["total_dma_payload_bytes"] == payload_bytes * 2
    assert sample["main_bus"]["port_count"] == 4
    assert sample["main_bus"]["nic_port_id"] == 2
    assert sample["main_bus"]["disk_port_id"] == 3
    assert sample["main_bus"]["issue_sequence_deltas"] == [
        3,
        3,
        payload_bytes,
        payload_bytes,
    ]
    assert sample["main_bus"]["grant_sequence_delta"] == (
        payload_bytes * 2 + 6
    )
    assert not any(sample["main_bus"]["sticky_bus_errors"])

    trace = report["service_trace"]["ports"]
    assert trace == [
        0,
        1,
        0,
        1,
        0,
        1,
        *[
            port
            for _ in range(payload_bytes)
            for port in (2, 3)
        ],
    ]
    assert report["arbitration_contract"][
        "simultaneously_eligible_peer_order"
    ] == "equal_round_robin"
    assert report["arbitration_contract"]["best_effort_weights"] == "none"
    assert report["arbitration_contract"][
        "secondary_ordering_biases"
    ] == []
    assert report["configuration"]["one_shot_cycle_budget"] == (
        payload_bytes * 8 + 256
    )
    assert report["configuration"][
        "max_instructions_per_cycle_batch"
    ] == phase0.STRICT_DMA_MAX_INSTRUCTIONS
    assert report["configuration"]["ordering_evidence_scope"] == (
        "two continuously eligible default-policy NIC and disk peers"
    )

    observation = sample["observation"]
    assert observation["state_schema_version"] == 9
    metrics = observation["workload_metrics"]["strict_nic_disk_dma"]
    assert metrics["published_nic_frames"]["entries"] == [
        metrics["nic_source"]
    ]
    assert metrics["storage_destination"] == metrics["storage_media"]
    assert metrics["storage_status"] == (
        STORAGE_STATUS_PRESENT | STORAGE_STATUS_RESULT_VALID
    )
    assert metrics["storage_transferred_sectors"] == (
        payload_bytes // SECTOR_SIZE
    )
    assert all(metrics["validation"].values())

    canonical = observation["canonical_state"]
    coordinator = canonical["shared_devices"]["main_bus"][
        "dma_coordinator"
    ]
    assert coordinator["endpoints"] == [
        {
            "requester_id": -1,
            "main_bus_port_id": 2,
            "next_issue_sequence": payload_bytes + 1,
            "highest_observed_token": payload_bytes,
            "timeline_active": False,
            "pending_token": None,
            "pending_request": None,
        },
        {
            "requester_id": -2,
            "main_bus_port_id": 3,
            "next_issue_sequence": payload_bytes + 1,
            "highest_observed_token": payload_bytes,
            "timeline_active": False,
            "pending_token": None,
            "pending_request": None,
        },
    ]
    storage_dma = canonical["shared_devices"]["storage"]["dma_fsm"]
    assert storage_dma["strict_cycle_submission"] is False
    assert storage_dma["async"] is False
    assert storage_dma["phase"] is None
    assert storage_dma["sector_index"] == 0
    assert storage_dma["byte_index"] == 0
    assert storage_dma["sector_data"]["size_bytes"] == 0
    assert storage_dma["write_sector"]["size_bytes"] == 0
    assert storage_dma["read_port_prefix"]["size_bytes"] == 0
    assert storage_dma["next_token"] == payload_bytes + 1
    assert storage_dma["pending"] is None
    nic_dma = canonical["native_devices_per_full_core"][0]["nic"][
        "cycle_dma"
    ]
    assert set(nic_dma) == {
        "schema_version",
        "rx_active",
        "tx_active",
        "rx_base",
        "tx_base",
        "tx_length",
        "rx_index",
        "tx_index",
        "rx_frame",
        "tx_frame",
        "next_token",
        "pending",
    }
    assert nic_dma["schema_version"] == 1
    assert nic_dma["rx_active"] is False
    assert nic_dma["tx_active"] is False
    assert nic_dma["next_token"] == payload_bytes + 1
    assert nic_dma["pending"] is None


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
            event.release_boundary,
            event.release_phase,
        )
        for event in owner.external_event_history
    ] == [
        (
            1,
            0,
            b"A",
            1,
            _mp64_accel.ExternalEventReleasePhase.AFTER_BATCH,
        ),
        (
            2,
            0,
            b"B",
            2,
            _mp64_accel.ExternalEventReleasePhase.BEFORE_BATCH,
        ),
    ]
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


def test_system_nic_invalid_ingress_journals_false_and_sticky_error():
    system = _system(assemble("nop"))

    assert not system.nic.inject_frame(b"")
    assert system.cpu._cs.nic_rx_queue_size() == 0
    assert system.cpu._cs.nic_read8(NIC_BASE + 0x01) & 0x08
    history = system._native_system.external_event_history
    assert len(history) == 1
    assert (
        history[0].kind ==
        _mp64_accel.ExternalEventKind.NIC_RX_REJECTED
    )
    assert bytes(history[0].payload) == b""
    assert history[0].release_boundary == 1
    assert (
        history[0].release_phase ==
        _mp64_accel.ExternalEventReleasePhase.BEFORE_BATCH
    )


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


class _CycleDmaStub:
    """Pure endpoint view plus exactly-once completion for scheduler oracles."""

    def __init__(self, requester_id, beats, completion_log=None):
        self.requester_id = requester_id
        self.beats = list(beats)
        self.inspect_cycles = []
        self.completions = []
        self.completion_log = (
            completion_log if completion_log is not None else []
        )

    def inspect(self, current_cycle):
        self.inspect_cycles.append(int(current_cycle))
        if not self.beats:
            return None
        return _mp64_accel.DmaEndpointView(
            active=True,
            pending=self.beats[0],
        )

    def complete(self, token, result):
        assert self.beats
        beat = self.beats[0]
        assert token == beat.token
        assert result.grant.request.requester_id == self.requester_id
        assert result.grant.request.address == beat.address
        assert result.grant.request.operation == beat.operation
        assert result.grant.request.write_data == beat.write_data
        record = (
            self.requester_id,
            int(token),
            int(result.grant.grant_cycle),
            int(result.completion_cycle),
            int(result.grant.request.address),
            None if result.read_value is None else int(result.read_value),
        )
        self.completions.append(record)
        self.completion_log.append(record)
        self.beats.pop(0)


def _dma_beat(
    token,
    *,
    ready_cycle=0,
    operation=_mp64_accel.BusOperation.WRITE,
    address,
    write_data=0,
):
    return _mp64_accel.DmaBeat(
        token=token,
        ready_cycle=ready_cycle,
        operation=operation,
        address=address,
        write_data=write_data,
    )


def _attach_cycle_dma_stubs(system, nic=None, disk=None):
    system._cycle_dma_callback_sets = lambda: [
        (
            (nic.inspect, nic.complete)
            if nic is not None
            else (None, None)
        ),
        (
            (disk.inspect, disk.complete)
            if disk is not None
            else (None, None)
        ),
    ]


def _run_equal_dma_case(*, sliced):
    system = _system(assemble("halt"))
    owner = system._native_system
    completion_log = []
    nic = _CycleDmaStub(
        owner.NIC_DMA_REQUESTER_ID,
        [
            _dma_beat(1, address=0x180, write_data=0xA1),
            _dma_beat(2, address=0x181, write_data=0xA2),
        ],
        completion_log,
    )
    disk = _CycleDmaStub(
        owner.DISK_DMA_REQUESTER_ID,
        [
            _dma_beat(101, address=0x182, write_data=0xB1),
            _dma_beat(102, address=0x183, write_data=0xB2),
        ],
        completion_log,
    )
    _attach_cycle_dma_stubs(system, nic, disk)

    if sliced:
        final = None
        for _ in range(16):
            final = system.run_cycle_batch(
                1,
                max_instructions=100,
            )
            if (
                not nic.beats
                and not disk.beats
                and not owner.cycle_execution_pending
                and final.system_stop_reason == "all_halted"
            ):
                break
        else:
            pytest.fail("sliced DMA endpoints did not quiesce")
    else:
        final = system.run_cycle_batch(
            32,
            max_instructions=100,
        )

    snapshot = owner._main_bus_snapshot()
    return (
        bytes(system.cpu.mem[0x180:0x184]),
        completion_log,
        int(owner.system_cycles),
        str(final.system_stop_reason),
        int(snapshot.last_grant),
        int(snapshot.next_grant_sequence),
        tuple(int(value) for value in snapshot.last_issue_sequences),
        bool(owner.cycle_execution_pending),
    )


def test_dma_endpoints_are_equal_peers_and_slice_identical():
    whole = _run_equal_dma_case(sliced=False)
    sliced = _run_equal_dma_case(sliced=True)

    assert whole == sliced
    memory, completions, cycles, stop, last_grant, grants, issues, pending = (
        whole
    )
    assert memory == b"\xA1\xA2\xB1\xB2"
    assert [
        (requester, grant_cycle, completion_cycle)
        for (
            requester,
            _token,
            grant_cycle,
            completion_cycle,
            _address,
            _read_value,
        ) in completions
    ] == [
        (-1, 0, 1),
        (-2, 2, 3),
        (-1, 4, 5),
        (-2, 6, 7),
    ]
    assert cycles == 7
    assert stop == "all_halted"
    assert last_grant == 2
    assert grants == 5
    assert issues == (0, 2, 2)
    assert not pending


def test_dma_zero_budgets_do_not_inspect_or_mutate_endpoints():
    system = _system(assemble("halt"))
    owner = system._native_system
    nic = _CycleDmaStub(
        owner.NIC_DMA_REQUESTER_ID,
        [_dma_beat(1, address=0x180, write_data=0x5A)],
    )
    _attach_cycle_dma_stubs(system, nic=nic)
    before = owner._main_bus_snapshot()

    no_cycles = system.run_cycle_batch(
        0,
        max_instructions=10,
    )
    no_instructions = system.run_cycle_batch(
        10,
        max_instructions=0,
    )

    after = owner._main_bus_snapshot()
    assert no_cycles.system_stop_reason == "cycle_limit"
    assert no_instructions.system_stop_reason == "instruction_limit"
    assert nic.inspect_cycles == []
    assert nic.completions == []
    assert owner.system_cycles == 0
    assert system.cpu.pc == 0
    assert bytes(system.cpu.mem[0x180:0x181]) == b"\x00"
    assert after.next_grant_sequence == before.next_grant_sequence
    assert after.last_issue_sequences == before.last_issue_sequences
    assert owner._cycle_pending_bus_requests() == []


def test_denied_dma_beat_is_stable_and_cancellation_precedes_replacement():
    system = _system(assemble("halt"))
    owner = system._native_system
    nic = _CycleDmaStub(
        owner.NIC_DMA_REQUESTER_ID,
        [
            _dma_beat(
                1,
                ready_cycle=5,
                address=0x180,
                write_data=0x31,
            )
        ],
    )
    _attach_cycle_dma_stubs(system, nic=nic)

    system.run_cycle_batch(1, max_instructions=100)
    first = owner._cycle_pending_bus_requests()
    system.run_cycle_batch(1, max_instructions=100)
    second = owner._cycle_pending_bus_requests()

    assert len(first) == len(second) == 1
    assert (
        first[0].requester_id,
        first[0].ready_cycle,
        first[0].address,
        first[0].write_data,
        first[0].ordering.issue_sequence,
    ) == (
        second[0].requester_id,
        second[0].ready_cycle,
        second[0].address,
        second[0].write_data,
        second[0].ordering.issue_sequence,
    )

    nic.beats[0] = _dma_beat(
        1,
        ready_cycle=5,
        address=0x180,
        write_data=0x32,
    )
    with pytest.raises(RuntimeError, match="held DMA beat changed"):
        system.run_cycle_batch(1, max_instructions=100)

    replacement = _system(assemble("halt"))
    replacement_owner = replacement._native_system
    endpoint = _CycleDmaStub(
        replacement_owner.NIC_DMA_REQUESTER_ID,
        [
            _dma_beat(
                1,
                ready_cycle=10,
                address=0x180,
                write_data=0x41,
            )
        ],
    )
    _attach_cycle_dma_stubs(replacement, nic=endpoint)
    replacement.run_cycle_batch(1, max_instructions=100)

    endpoint.beats.clear()
    cancelled = replacement.run_cycle_batch(
        1,
        max_instructions=100,
    )
    assert cancelled.system_stop_reason == "all_halted"
    assert replacement_owner._cycle_pending_bus_requests() == []
    assert not replacement_owner.cycle_execution_pending

    endpoint.beats.append(
        _dma_beat(
            2,
            ready_cycle=replacement_owner.system_cycles,
            address=0x180,
            write_data=0x42,
        )
    )
    completed = replacement.run_cycle_batch(
        8,
        max_instructions=100,
    )
    assert completed.system_stop_reason == "all_halted"
    assert replacement.cpu.mem[0x180] == 0x42
    assert len(endpoint.completions) == 1


def test_dma_same_address_effects_and_lone_port_bubbles_follow_bus_order():
    system = _system(assemble("halt"))
    owner = system._native_system
    completion_log = []
    system.cpu.mem[0x190] = 0x11
    nic = _CycleDmaStub(
        owner.NIC_DMA_REQUESTER_ID,
        [_dma_beat(1, address=0x190, write_data=0xAA)],
        completion_log,
    )
    disk = _CycleDmaStub(
        owner.DISK_DMA_REQUESTER_ID,
        [
            _dma_beat(
                1,
                operation=_mp64_accel.BusOperation.READ,
                address=0x190,
            )
        ],
        completion_log,
    )
    _attach_cycle_dma_stubs(system, nic, disk)

    result = system.run_cycle_batch(16, max_instructions=100)

    assert result.system_stop_reason == "all_halted"
    assert system.cpu.mem[0x190] == 0xAA
    assert [
        (entry[0], entry[5])
        for entry in completion_log
    ] == [
        (owner.NIC_DMA_REQUESTER_ID, None),
        (owner.DISK_DMA_REQUESTER_ID, 0xAA),
    ]

    lone = _system(assemble("halt"))
    lone_owner = lone._native_system
    endpoint = _CycleDmaStub(
        lone_owner.NIC_DMA_REQUESTER_ID,
        [
            _dma_beat(1, address=0x1A0, write_data=1),
            _dma_beat(2, address=0x1A1, write_data=2),
            _dma_beat(3, address=0x1A2, write_data=3),
        ],
    )
    _attach_cycle_dma_stubs(lone, nic=endpoint)

    lone_result = lone.run_cycle_batch(
        16,
        max_instructions=100,
    )

    assert lone_result.system_stop_reason == "all_halted"
    assert bytes(lone.cpu.mem[0x1A0:0x1A3]) == b"\x01\x02\x03"
    assert [
        (entry[2], entry[3])
        for entry in endpoint.completions
    ] == [(0, 1), (3, 4), (6, 7)]


def test_warm_boot_clears_cached_dma_endpoint_and_bus_frontier():
    system = _system(assemble("halt"))
    owner = system._native_system
    nic = _CycleDmaStub(
        owner.NIC_DMA_REQUESTER_ID,
        [
            _dma_beat(
                1,
                ready_cycle=10,
                address=0x180,
                write_data=0x7A,
            )
        ],
    )
    _attach_cycle_dma_stubs(system, nic=nic)
    system.run_cycle_batch(1, max_instructions=100)

    assert owner._cycle_pending_bus_requests()
    assert owner.cycle_execution_pending

    reset_observations = []
    original_storage_reset = system.storage.reset

    def checked_storage_reset():
        reset_observations.append((
            owner.cycle_execution_pending,
            owner._main_bus_snapshot().active_grant,
            owner._cycle_pending_bus_requests(),
        ))
        original_storage_reset()

    system.storage.reset = checked_storage_reset
    system.boot(entry=0)

    snapshot = owner._main_bus_snapshot()
    assert reset_observations == [(False, None, [])]
    assert owner._cycle_pending_bus_requests() == []
    assert not owner.cycle_execution_pending
    assert snapshot.active_grant is None
    assert snapshot.next_grant_sequence == 1
    assert snapshot.last_issue_sequences == [0, 0, 0]


def _write_native_nic_register(system, offset, value, width):
    native = system.cores[0]._cs
    for index in range(width):
        native.nic_write8(
            NIC_BASE + offset + index,
            (value >> (8 * index)) & 0xFF,
        )


def _pending_nic_requests(system):
    requester = system._native_system.NIC_DMA_REQUESTER_ID
    return [
        request
        for request in system._native_system._cycle_pending_bus_requests()
        if request.requester_id == requester
    ]


def _drain_strict_nic(system, done, *, max_slices=64):
    observed = {}
    final = None
    for _ in range(max_slices):
        for request in _pending_nic_requests(system):
            observed.setdefault(
                int(request.ordering.issue_sequence),
                (
                    request.operation,
                    int(request.address),
                    int(request.write_data),
                ),
            )
        final = system.run_cycle_batch(
            1,
            max_instructions=100,
        )
        if done() and not _pending_nic_requests(system):
            break
    else:
        pytest.fail("strict NIC DMA did not quiesce")
    return [observed[key] for key in sorted(observed)], final


def test_native_nic_rx_is_byte_resumable_and_publishes_length_last():
    system = _system(assemble("st.b r1, r2\nhalt"))
    cpu = system.cpu
    native = cpu._cs
    frame = b"abc"
    target = 0x180
    _write_native_nic_register(system, 0x02, target, 8)
    _write_native_nic_register(system, 0x0A, 0x55, 2)
    assert native.nic_inject_frame(frame)
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x02

    command = system.run_cycle_batch(
        2,
        max_instructions=100,
    )

    assert command.system_stop_reason == "cycle_limit"
    assert bytes(cpu.mem[target:target + len(frame)]) == b"\x00" * len(frame)
    assert native.nic_read8(NIC_BASE + 0x0A) == 0x55
    assert native.nic_read8(NIC_BASE + 0x01) & 0x12 == 0x12
    pending = _pending_nic_requests(system)
    assert len(pending) == 1
    assert pending[0].operation == _mp64_accel.BusOperation.WRITE
    assert pending[0].address == target
    assert pending[0].write_data == frame[0]
    with pytest.raises(
        RuntimeError,
        match="native NIC cannot mutate while cycle execution is suspended",
    ):
        native.nic_write8(NIC_BASE + 0x02, 0xFF)
    blocked_host_mutations = (
        lambda: native.nic_init(b"\x00" * 6),
        native.nic_sync_mem_ptrs,
        lambda: native.nic_set_tx_callback(lambda _frame: True),
    )
    for mutation in blocked_host_mutations:
        with pytest.raises(
            RuntimeError,
            match="native NIC cannot mutate while cycle execution is suspended",
        ):
            mutation()

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            native.nic_read8(NIC_BASE + 0x01) & 0x10
        ) == 0,
    )

    assert beats == [
        (_mp64_accel.BusOperation.WRITE, target + 0, ord("a")),
        (_mp64_accel.BusOperation.WRITE, target + 1, ord("b")),
        (_mp64_accel.BusOperation.WRITE, target + 2, ord("c")),
    ]
    assert bytes(cpu.mem[target:target + len(frame)]) == frame
    assert native.nic_read8(NIC_BASE + 0x0A) == len(frame)
    assert native.nic_read8(NIC_BASE + 0x0B) == 0
    assert native.nic_read8(NIC_BASE + 0x01) & 0x12 == 0
    nic_port = system._native_system.main_bus_port_for_requester(
        system._native_system.NIC_DMA_REQUESTER_ID
    )
    assert (
        system._native_system
        ._main_bus_snapshot()
        .last_issue_sequences[nic_port]
    ) == len(frame)


def test_native_nic_tx_defers_publication_until_final_bus_read():
    system = _system(assemble("st.b r1, r2\nhalt"))
    cpu = system.cpu
    native = cpu._cs
    frame = b"xyz"
    source = 0x180
    cpu.mem[source:source + len(frame)] = frame
    _write_native_nic_register(system, 0x02, source, 8)
    _write_native_nic_register(system, 0x0A, len(frame), 2)
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x01

    system.run_cycle_batch(2, max_instructions=100)

    assert native.nic_read8(NIC_BASE + 0x01) & 0x01
    assert native.nic_get_tx_count() == 0
    assert native.nic_tx_queue_size() == 0
    assert native.nic_read8(NIC_BASE + 0x0D) & 0x02 == 0
    assert list(system.nic.tx_queue) == []

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            native.nic_read8(NIC_BASE + 0x01) & 0x01
        ) == 0,
    )

    assert beats == [
        (_mp64_accel.BusOperation.READ, source + 0, 0),
        (_mp64_accel.BusOperation.READ, source + 1, 0),
        (_mp64_accel.BusOperation.READ, source + 2, 0),
    ]
    assert native.nic_get_tx_count() == 1
    assert native.nic_tx_queue_size() == 1
    assert native.nic_read8(NIC_BASE + 0x0D) & 0x02
    assert list(system.nic.tx_queue) == [frame]


@pytest.mark.parametrize(
    "store",
    ("st.h", "st.w", "str"),
)
def test_native_nic_wide_command_stores_remain_strict(store):
    system = _system(assemble(f"{store} r1, r2\nhalt"))
    cpu = system.cpu
    native = cpu._cs
    frame = b"wide"
    source = 0x180
    cpu.mem[source:source + len(frame)] = frame
    _write_native_nic_register(system, 0x02, source, 8)
    _write_native_nic_register(system, 0x0A, len(frame), 2)
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x01

    system.run_cycle_batch(2, max_instructions=100)

    assert native.nic_read8(NIC_BASE + 0x01) & 0x01
    assert native.nic_get_tx_count() == 0
    pending = _pending_nic_requests(system)
    assert len(pending) == 1
    assert pending[0].operation == _mp64_accel.BusOperation.READ
    assert pending[0].address == source

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            cpu.halted
            and native.nic_read8(NIC_BASE + 0x01) & 0x01 == 0
        ),
    )

    assert beats == [
        (_mp64_accel.BusOperation.READ, source + index, 0)
        for index in range(len(frame))
    ]
    assert cpu.halted
    assert list(system.nic.tx_queue) == [frame]


def test_native_nic_preserves_held_tx_then_uses_local_rx_priority():
    system = _system(
        assemble("st.b r1, r2\nst.b r1, r4\nhalt")
    )
    cpu = system.cpu
    native = cpu._cs
    source = 0x180
    cpu.mem[source:source + 3] = b"ABC"
    _write_native_nic_register(system, 0x02, source, 8)
    _write_native_nic_register(system, 0x0A, 3, 2)
    assert native.nic_inject_frame(b"xy")
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x01
    cpu.regs[4] = 0x02

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            cpu.halted
            and native.nic_read8(NIC_BASE + 0x01) & 0x11 == 0
        ),
    )

    assert beats == [
        (_mp64_accel.BusOperation.READ, source + 0, 0),
        (_mp64_accel.BusOperation.READ, source + 1, 0),
        (_mp64_accel.BusOperation.WRITE, source + 0, ord("x")),
        (_mp64_accel.BusOperation.WRITE, source + 1, ord("y")),
        (_mp64_accel.BusOperation.READ, source + 2, 0),
    ]
    assert bytes(cpu.mem[source:source + 3]) == b"xyC"
    assert list(system.nic.tx_queue) == [b"ABC"]
    assert native.nic_read8(NIC_BASE + 0x0A) == 2


def test_native_nic_guest_reset_cancels_tail_and_next_token_recovers():
    system = _system(
        assemble("st.b r1, r2\nst.b r1, r4\nhalt")
    )
    cpu = system.cpu
    native = cpu._cs
    source = 0x180
    frame = b"ABC"
    cpu.mem[source:source + len(frame)] = frame
    _write_native_nic_register(system, 0x02, source, 8)
    _write_native_nic_register(system, 0x0A, len(frame), 2)
    _write_native_nic_register(system, 0x0C, 0x03, 1)
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x01
    cpu.regs[4] = 0x04

    reset = system.run_cycle_batch(32, max_instructions=100)

    assert reset.system_stop_reason == "all_halted"
    assert native.nic_get_tx_count() == 0
    assert list(system.nic.tx_queue) == []
    assert native.nic_read8(NIC_BASE + 0x01) & 0x11 == 0
    assert native.nic_read8(NIC_BASE + 0x0C) == 0x03
    assert sum(
        native.nic_read8(NIC_BASE + 0x02 + index) << (8 * index)
        for index in range(8)
    ) == source
    assert _pending_nic_requests(system) == []
    assert not system._native_system.cycle_execution_pending

    # Host reconfiguration is legal once the timeline is clean, but it must
    # not rewind the endpoint token below coordinator history.
    native.nic_init(bytes(system.nic.mac))
    _write_native_nic_register(system, 0x02, source, 8)
    retry_entry = 0x20
    system.load_binary(
        retry_entry,
        assemble("st.b r1, r2\nhalt"),
    )
    _write_native_nic_register(system, 0x0A, len(frame), 2)
    cpu.pc = retry_entry
    cpu.halted = False

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            cpu.halted
            and native.nic_read8(NIC_BASE + 0x01) & 0x01 == 0
        ),
    )

    assert beats == [
        (_mp64_accel.BusOperation.READ, source + index, 0)
        for index in range(len(frame))
    ]
    assert cpu.halted
    assert native.nic_get_tx_count() == 1
    assert list(system.nic.tx_queue) == [frame]


def test_warm_boot_readopts_native_nic_dma_before_unbounded_execution():
    system = _system(assemble("st.b r1, r2\nhalt"))
    cpu = system.cpu
    native = cpu._cs
    frame = b"abc"
    target = 0x180
    _write_native_nic_register(system, 0x02, target, 8)
    assert native.nic_inject_frame(frame)
    cpu.regs[1] = MMIO_BASE + NIC_BASE
    cpu.regs[2] = 0x02
    system.run_cycle_batch(2, max_instructions=100)
    held = _pending_nic_requests(system)[0]

    system.boot(entry=2)

    pending = _pending_nic_requests(system)
    assert len(pending) == 1
    assert (
        pending[0].operation,
        pending[0].address,
        pending[0].write_data,
    ) == (
        held.operation,
        held.address,
        held.write_data,
    )
    assert system._native_system.cycle_execution_pending
    with pytest.raises(
        RuntimeError,
        match="suspended cycle execution requires",
    ):
        system.run_batch(1)

    beats, _final = _drain_strict_nic(
        system,
        lambda: (
            cpu.halted
            and native.nic_read8(NIC_BASE + 0x01) & 0x10 == 0
        ),
    )

    assert beats == [
        (_mp64_accel.BusOperation.WRITE, target + index, value)
        for index, value in enumerate(frame)
    ]
    assert cpu.halted
    assert bytes(cpu.mem[target:target + len(frame)]) == frame


def _write_storage_register(storage, offset, value, width):
    for index in range(width):
        storage.write8(
            offset + index,
            (value >> (8 * index)) & 0xFF,
        )


def _pending_storage_requests(system):
    requester = system._native_system.DISK_DMA_REQUESTER_ID
    return [
        request
        for request in system._native_system._cycle_pending_bus_requests()
        if request.requester_id == requester
    ]


def _record_storage_requests(system, observed):
    for request in _pending_storage_requests(system):
        observed.setdefault(
            int(request.ordering.issue_sequence),
            (
                request.operation,
                int(request.address),
                int(request.write_data),
            ),
        )


def _drain_strict_storage(system, done, *, max_slices=520):
    observed = {}
    final = None
    for _ in range(max_slices):
        _record_storage_requests(system, observed)
        final = system.run_cycle_batch(
            3,
            max_instructions=100,
        )
        if done() and not _pending_storage_requests(system):
            break
    else:
        pytest.fail("strict storage DMA did not quiesce")
    return [observed[key] for key in sorted(observed)], final


def _prepare_strict_storage_command(
    tmp_path,
    *,
    command,
    media,
    dma_address,
):
    path = tmp_path / "strict-cycle-storage.img"
    path.write_bytes(media)
    system = _system(
        assemble("st.b r1, r2\nhalt"),
        storage_image=str(path),
    )
    storage = system.storage
    _write_storage_register(storage, 0x02, 0, 4)
    _write_storage_register(storage, 0x06, dma_address, 8)
    storage.write8(0x0E, 1)
    system.cpu.regs[1] = MMIO_BASE + STORAGE_BASE
    system.cpu.regs[2] = command
    return system, storage


def test_storage_read_is_strict_byte_dma_and_publishes_terminal_state(
    tmp_path,
):
    payload = bytes(
        (index * 13 + 7) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    target = 0x400
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_READ,
        media=payload,
        dma_address=target,
    )

    command = system.run_cycle_batch(
        2,
        max_instructions=100,
    )

    assert command.system_stop_reason == "cycle_limit"
    assert storage.read8(0x01) & STORAGE_STATUS_BUSY
    assert storage.completion == 0
    assert storage.transferred == 0
    assert bytes(system.cpu.mem[target:target + SECTOR_SIZE]) == (
        b"\x00" * SECTOR_SIZE
    )
    pending = _pending_storage_requests(system)
    assert len(pending) == 1
    assert pending[0].operation == _mp64_accel.BusOperation.WRITE
    assert pending[0].address == target
    assert pending[0].write_data == payload[0]
    with pytest.raises(
        RuntimeError,
        match="storage cannot mutate while cycle execution is suspended",
    ):
        storage.reset()

    beats, final = _drain_strict_storage(
        system,
        lambda: not storage.busy,
    )

    assert beats == [
        (
            _mp64_accel.BusOperation.WRITE,
            target + index,
            value,
        )
        for index, value in enumerate(payload)
    ]
    assert final.system_stop_reason == "all_halted"
    assert bytes(system.cpu.mem[target:target + SECTOR_SIZE]) == payload
    assert storage.result == STORAGE_RESULT_OK
    assert storage.completion == 1
    assert storage.transferred == 1
    assert storage.read8(0x01) & STORAGE_STATUS_RESULT_VALID
    assert not storage.read8(0x01) & STORAGE_STATUS_BUSY
    assert bytes(storage.data_port_buf) == payload
    disk_port = system._native_system.main_bus_port_for_requester(
        system._native_system.DISK_DMA_REQUESTER_ID
    )
    assert (
        system._native_system
        ._main_bus_snapshot()
        .last_issue_sequences[disk_port]
    ) == SECTOR_SIZE


def test_storage_stall_release_rejects_native_batch_reentry(tmp_path):
    target = 0x400
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_READ,
        media=b"\x6D" * SECTOR_SIZE,
        dma_address=target,
    )
    storage.inject_fault(
        "start",
        command=STORAGE_CMD_READ,
        action="stall",
    )
    release_errors = []
    original_write8 = storage.write8

    def write8_with_reentrant_release(offset, value):
        original_write8(offset, value)
        if offset == 0x00 and storage.stalled:
            try:
                storage.release_stall()
            except RuntimeError as error:
                release_errors.append(str(error))

    storage.write8 = write8_with_reentrant_release

    result = system.run_cycle_batch(
        2,
        max_instructions=100,
    )

    assert result.system_stop_reason == "cycle_limit"
    assert release_errors == [
        "storage stall release cannot mutate during an "
        "active native system batch"
    ]
    assert storage.busy
    assert storage.stalled
    assert storage.cycle_dma_view() == (True, None)
    assert _pending_storage_requests(system) == []


def test_storage_stall_resumes_between_strict_cycle_slices(tmp_path):
    payload = bytes(
        (index * 17 + 9) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    target = 0x400
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_READ,
        media=payload,
        dma_address=target,
    )
    storage.inject_fault(
        "start",
        command=STORAGE_CMD_READ,
        action="stall",
    )

    stalled = system.run_cycle_batch(
        2,
        max_instructions=100,
    )

    assert stalled.system_stop_reason == "cycle_limit"
    assert storage.busy
    assert storage.stalled
    assert storage.cycle_dma_view() == (True, None)
    assert _pending_storage_requests(system) == []
    assert system._native_system.cycle_execution_pending

    # This test-only transition is intentionally legal between suspended
    # slices.  It stages the disk beat locally; the native coordinator adopts
    # that immutable beat at the next cycle boundary.
    assert storage.release_stall()
    assert not storage.stalled
    active, staged = storage.cycle_dma_view()
    assert active
    assert staged is not None
    assert staged.address == target
    first_beat = (
        _mp64_accel.BusOperation.WRITE,
        staged.address,
        staged.write_data,
    )
    assert _pending_storage_requests(system) == []

    adopted = system.run_cycle_batch(
        1,
        max_instructions=100,
    )
    pending = _pending_storage_requests(system)
    assert adopted.system_stop_reason == "cycle_limit"
    assert len(pending) == 1
    assert pending[0].operation == _mp64_accel.BusOperation.WRITE
    assert pending[0].address == target + 1
    assert pending[0].write_data == payload[1]
    assert system.cpu.mem[target] == payload[0]

    beats, final = _drain_strict_storage(
        system,
        lambda: not storage.busy,
    )
    for _ in range(8):
        if system.cpu.halted:
            break
        final = system.run_cycle_batch(
            3,
            max_instructions=100,
        )
    else:
        pytest.fail("core did not settle after resumed storage DMA")
    final = system.run_cycle_batch(
        1,
        max_instructions=100,
    )

    assert [first_beat, *beats] == [
        (
            _mp64_accel.BusOperation.WRITE,
            target + index,
            value,
        )
        for index, value in enumerate(payload)
    ]
    assert final.system_stop_reason == "all_halted"
    assert bytes(system.cpu.mem[target:target + SECTOR_SIZE]) == payload
    assert storage.result == STORAGE_RESULT_OK
    assert storage.completion == 1
    assert storage.transferred == 1


def test_storage_write_captures_full_sector_before_media_commit(tmp_path):
    payload = bytes(
        (index * 5 + 3) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    source = 0x400
    original_media = b"\x39" * SECTOR_SIZE
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_WRITE,
        media=original_media,
        dma_address=source,
    )
    system.cpu.mem[source:source + SECTOR_SIZE] = payload

    system.run_cycle_batch(2, max_instructions=100)

    observed = {}
    for _ in range(SECTOR_SIZE):
        _record_storage_requests(system, observed)
        pending = _pending_storage_requests(system)
        if (
            pending
            and pending[0].ordering.issue_sequence == SECTOR_SIZE
        ):
            break
        system.run_cycle_batch(3, max_instructions=100)
    else:
        pytest.fail("strict storage WRITE did not expose its final beat")

    assert bytes(storage._image_data) == original_media
    assert storage.busy
    assert storage.completion == 0
    assert storage.transferred == 0
    final = system.run_cycle_batch(3, max_instructions=100)

    assert [
        observed[key]
        for key in sorted(observed)
    ] == [
        (
            _mp64_accel.BusOperation.READ,
            source + index,
            0,
        )
        for index in range(SECTOR_SIZE)
    ]
    assert final.system_stop_reason == "all_halted"
    assert bytes(storage._image_data) == payload
    assert storage.result == STORAGE_RESULT_OK
    assert storage.completion == 1
    assert storage.transferred == 1
    assert not storage.busy


def test_storage_strict_preflight_failure_emits_no_dma_beat(tmp_path):
    invalid = 4096 - SECTOR_SIZE + 1
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_READ,
        media=b"\x5A" * SECTOR_SIZE,
        dma_address=invalid,
    )

    result = system.run_cycle_batch(16, max_instructions=100)

    assert result.system_stop_reason == "all_halted"
    assert storage.result == STORAGE_RESULT_DMA_INVALID
    assert storage.completion == 1
    assert storage.transferred == 0
    assert _pending_storage_requests(system) == []
    disk_port = system._native_system.main_bus_port_for_requester(
        system._native_system.DISK_DMA_REQUESTER_ID
    )
    assert (
        system._native_system
        ._main_bus_snapshot()
        .last_issue_sequences[disk_port]
    ) == 0


def test_storage_guest_reset_follows_captured_beat_and_cancels_tail(tmp_path):
    payload = bytes(
        (index * 11 + 1) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    target = 0x400
    path = tmp_path / "strict-cycle-reset.img"
    path.write_bytes(payload)
    system = _system(
        assemble("st.b r1, r2\nst.b r1, r4\nhalt"),
        storage_image=str(path),
    )
    storage = system.storage
    _write_storage_register(storage, 0x02, 0, 4)
    _write_storage_register(storage, 0x06, target, 8)
    storage.write8(0x0E, 1)
    system.cpu.regs[1] = MMIO_BASE + STORAGE_BASE
    system.cpu.regs[2] = STORAGE_CMD_READ
    system.cpu.regs[4] = STORAGE_CMD_RESET

    result = system.run_cycle_batch(32, max_instructions=100)

    assert result.system_stop_reason == "all_halted"
    assert bytes(system.cpu.mem[target:target + 2]) == (
        payload[:1] + b"\x00"
    )
    assert storage.result == (
        STORAGE_RESULT_PARTIAL | STORAGE_RESULT_RESET_ABORTED
    )
    assert storage.completion == 1
    assert storage.transferred == 0
    assert not storage.busy
    assert _pending_storage_requests(system) == []
    assert not system._native_system.cycle_execution_pending


def test_storage_strict_dma_fault_reports_exact_applied_prefix(tmp_path):
    payload = bytes(
        (index * 9 + 5) & 0xFF
        for index in range(SECTOR_SIZE)
    )
    target = 0x400
    system, storage = _prepare_strict_storage_command(
        tmp_path,
        command=STORAGE_CMD_READ,
        media=payload,
        dma_address=target,
    )
    storage.inject_fault(
        "dma",
        STORAGE_RESULT_DMA_FAILURE,
        command=STORAGE_CMD_READ,
        sector_index=0,
        byte_index=10,
    )

    result = system.run_cycle_batch(64, max_instructions=100)

    assert result.system_stop_reason == "all_halted"
    assert bytes(system.cpu.mem[target:target + 10]) == payload[:10]
    assert bytes(system.cpu.mem[target + 10:target + SECTOR_SIZE]) == (
        b"\x00" * (SECTOR_SIZE - 10)
    )
    assert storage.result == (
        STORAGE_RESULT_PARTIAL | STORAGE_RESULT_DMA_FAILURE
    )
    assert storage.completion == 1
    assert storage.transferred == 0
    assert not storage.busy
    disk_port = system._native_system.main_bus_port_for_requester(
        system._native_system.DISK_DMA_REQUESTER_ID
    )
    assert (
        system._native_system
        ._main_bus_snapshot()
        .last_issue_sequences[disk_port]
    ) == 10
