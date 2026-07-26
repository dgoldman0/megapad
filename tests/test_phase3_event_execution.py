"""Phase 3 element-5 strict event, DMA, replay, and stop oracles."""

from __future__ import annotations

from copy import deepcopy

import pytest

import _mp64_accel
from asm import assemble
from devices import NIC_BASE
from system import MegapadSystem


LINE_BYTES = 16


def _system(
    code: bytes,
    *,
    cores: int = 2,
    worker_count: int = 1,
) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    _prime_instruction_cache(system, 0, len(code))
    return system


def _prime_instruction_cache(
    system: MegapadSystem,
    address: int,
    size: int,
) -> None:
    first_line = address & ~(LINE_BYTES - 1)
    last_line = (address + size - 1) & ~(LINE_BYTES - 1)
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
            data_offset = index * LINE_BYTES
            data[data_offset:data_offset + LINE_BYTES] = cpu.mem[
                line_address:line_address + LINE_BYTES
            ]
            line_address += LINE_BYTES
        cpu._cs.icache_restore(
            bytes(valid),
            tags,
            bytes(data),
        )


def _worker_lanes(system: MegapadSystem) -> tuple[dict, ...]:
    diagnostics = dict(
        system._native_system._private_worker_diagnostics()
    )
    return tuple(dict(lane) for lane in diagnostics["lanes"])


def _result_signature(result) -> tuple:
    return (
        result.instructions_executed,
        result.system_cycles_advanced,
        result.per_core_instructions,
        result.per_core_cycles,
        result.per_core_dispatches,
        result.per_core_stop_reasons,
        result.system_stop_reason,
        result.stop_cycle,
        result.event_source_mask,
        result.per_core_interrupts,
        result.interrupts_delivered,
        result.external_events_applied,
        result.pending_interrupt_core,
        result.pending_interrupt_vector,
    )


class _DmaEndpoint:
    def __init__(self, requester_id: int, beats: list, completion_log: list):
        self.requester_id = requester_id
        self.beats = list(beats)
        self.completion_log = completion_log

    def inspect(self, current_cycle: int):
        del current_cycle
        return _mp64_accel.DmaEndpointView(
            bool(self.beats),
            self.beats[0] if self.beats else None,
        )

    def complete(self, token: int, result) -> None:
        assert self.beats
        assert self.beats[0].token == token
        self.completion_log.append(
            (
                self.requester_id,
                token,
                result.grant.grant_sequence,
                result.grant.grant_cycle,
                result.completion_cycle,
                result.grant.request.address,
                result.grant.request.write_data,
                result.target_effects_committed,
            )
        )
        self.beats.pop(0)


class _FailingDmaEndpoint(_DmaEndpoint):
    def complete(self, token: int, result) -> None:
        super().complete(token, result)
        raise RuntimeError("completion boom")


def _attach_dma_endpoints(
    system: MegapadSystem,
    nic: _DmaEndpoint,
    disk: _DmaEndpoint,
) -> None:
    system._cycle_dma_callback_sets = lambda: [
        (nic.inspect, nic.complete),
        (disk.inspect, disk.complete),
    ]


def _bus_signature(system: MegapadSystem) -> tuple:
    bus = system._native_system._main_bus_snapshot()
    active = bus.active_grant
    return (
        bus.port_count,
        bus.last_grant,
        bus.reset_port_zero_credit,
        bus.next_grant_sequence,
        bus.earliest_arbitration_cycle,
        bus.served_last,
        bus.last_arbitration_cycle,
        None
        if active is None
        else (
            active.request.requester_id,
            active.grant_sequence,
            active.grant_cycle,
            active.timeout_cycle,
        ),
        tuple(bus.last_issue_sequences),
        tuple(bus.sticky_bus_errors),
    )


def _event_dma_case(
    worker_count: int,
    *,
    sliced: bool,
    replay_recording: dict | None = None,
) -> dict:
    code = assemble(
        """
    inc r4
    idl
    halt
"""
    )
    system = _system(
        code,
        cores=4,
        worker_count=worker_count,
    )
    completion_log: list[tuple] = []
    nic = _DmaEndpoint(
        -1,
        [
            _mp64_accel.DmaBeat(
                1,
                _mp64_accel.BusOperation.WRITE,
                0x180,
                0xA1,
            )
        ],
        completion_log,
    )
    disk = _DmaEndpoint(
        -2,
        [
            _mp64_accel.DmaBeat(
                1,
                _mp64_accel.BusOperation.WRITE,
                0x181,
                0xB2,
            )
        ],
        completion_log,
    )
    _attach_dma_endpoints(system, nic, disk)

    if replay_recording is None:
        assert system.schedule_uart_input(b"R", at_cycle=2) == 1
        assert (
            system.schedule_terminal_resize(
                100,
                30,
                at_cycle=2,
            )
            == 2
        )
    else:
        assert (
            system.install_external_ingress_replay(
                replay_recording
            )
            == 2
        )

    recording = system.export_external_ingress_recording()
    results = []
    if sliced:
        for _ in range(8):
            result = system.run_cycle_batch(
                1,
                max_instructions=10,
            )
            results.append(result)
            if result.system_stop_reason in {
                "all_halted",
                "all_idle",
            }:
                break
        else:
            pytest.fail("strict event/DMA case did not terminate")
    else:
        results.append(
            system.run_cycle_batch(
                16,
                max_instructions=10,
            )
        )

    lanes = _worker_lanes(system)
    aggregate = (
        sum(result.instructions_executed for result in results),
        sum(result.system_cycles_advanced for result in results),
        sum(result.external_events_applied for result in results),
        results[-1].system_stop_reason,
        results[-1].stop_cycle,
    )
    final_state = (
        tuple(
            (
                cpu.pc,
                cpu.regs[4],
                cpu.cycle_count,
                cpu.halted,
                cpu.idle,
            )
            for cpu in system.cores
        ),
        int(system._native_system.system_cycles),
        int(system._native_system.scheduler_cursor),
        bytes(system.cpu.mem[0x180:0x182]),
        tuple(completion_log),
        _bus_signature(system),
        system.uart.rx_pending,
        (
            system.uart_geom.cols,
            system.uart_geom.rows,
        ),
        system.export_external_ingress_recording(),
        tuple(system._native_system.external_event_pending),
        int(system._native_system.external_event_next_sequence),
    )
    return {
        "recording": recording,
        "result_signatures": tuple(
            _result_signature(result) for result in results
        ),
        "aggregate": aggregate,
        "final_state": final_state,
        "replay_sealed": bool(
            system._native_system.external_event_replay_sealed
        ),
        "auxiliary_commands": sum(
            lane["completed_commands"]
            for lane in lanes
            if lane["auxiliary"]
        ),
        "lane_commands": tuple(
            lane["completed_commands"]
            for lane in lanes
        ),
    }


def test_strict_event_dma_replay_is_one_two_four_lane_and_slice_exact():
    live = {}
    replayed = {}
    for sliced in (False, True):
        for worker_count in (1, 2, 4):
            observed = _event_dma_case(
                worker_count,
                sliced=sliced,
            )
            replay = _event_dma_case(
                worker_count,
                sliced=sliced,
                replay_recording=observed["recording"],
            )
            live[(sliced, worker_count)] = observed
            replayed[(sliced, worker_count)] = replay

            assert not observed["replay_sealed"]
            assert replay["replay_sealed"]
            assert (
                replay["result_signatures"]
                == observed["result_signatures"]
            )
            assert replay["aggregate"] == observed["aggregate"]
            assert replay["final_state"] == observed["final_state"]
            if worker_count == 1:
                assert observed["auxiliary_commands"] == 0
            else:
                assert observed["auxiliary_commands"] > 0
                assert replay["auxiliary_commands"] > 0
            assert len(observed["lane_commands"]) == worker_count
            assert all(
                commands > 0
                for commands in observed["lane_commands"]
            )
            assert all(
                commands > 0
                for commands in replay["lane_commands"]
            )

    for sliced in (False, True):
        reference = live[(sliced, 1)]
        replay_reference = replayed[(sliced, 1)]
        for worker_count in (2, 4):
            assert (
                live[(sliced, worker_count)]["result_signatures"]
                == reference["result_signatures"]
            )
            assert (
                live[(sliced, worker_count)]["aggregate"]
                == reference["aggregate"]
            )
            assert (
                live[(sliced, worker_count)]["final_state"]
                == reference["final_state"]
            )
            assert (
                replayed[(sliced, worker_count)][
                    "result_signatures"
                ]
                == replay_reference["result_signatures"]
            )

    assert live[(False, 1)]["aggregate"] == live[(True, 1)]["aggregate"]
    assert live[(False, 1)]["final_state"] == live[(True, 1)]["final_state"]


def _post_batch_staged_ingress_case(
    worker_count: int,
    *,
    recording: dict | None = None,
) -> tuple:
    system = _system(
        assemble("idl\nhalt"),
        cores=4,
        worker_count=worker_count,
    )
    if recording is None:
        original_settle = system._settle_native_system_round
        injected = False

        def settle_and_inject(
            cycles: int,
            advance_clock: bool,
            drain_uart: bool,
            deliver_interrupts: bool,
        ) -> None:
            nonlocal injected
            original_settle(
                cycles,
                advance_clock,
                drain_uart,
                deliver_interrupts,
            )
            if drain_uart and not injected:
                injected = True
                system.uart.inject_input(b"S")

        system._settle_native_system_round = settle_and_inject
    else:
        assert system.install_external_ingress_replay(recording) == 1

    first = system.run_cycle_batch(8, max_instructions=8)
    exported = system.export_external_ingress_recording()
    after_first = (
        _result_signature(first),
        tuple(
            (
                cpu.pc,
                cpu.halted,
                cpu.idle,
                cpu.cycle_count,
            )
            for cpu in system.cores
        ),
        int(system._native_system.system_cycles),
        int(system._native_system.external_event_batch_boundaries),
        system.uart.rx_pending,
    )
    second = system.run_cycle_batch(8, max_instructions=8)
    final = (
        _result_signature(second),
        tuple(
            (
                cpu.pc,
                cpu.halted,
                cpu.idle,
                cpu.cycle_count,
            )
            for cpu in system.cores
        ),
        int(system._native_system.system_cycles),
        int(system._native_system.external_event_batch_boundaries),
        system.uart.rx_pending,
    )
    return exported, after_first, final


def test_live_staged_ingress_replays_at_the_same_post_batch_boundary():
    references = {}
    for worker_count in (1, 2, 4):
        recording, live_first, live_final = (
            _post_batch_staged_ingress_case(worker_count)
        )
        replayed, replay_first, replay_final = (
            _post_batch_staged_ingress_case(
                worker_count,
                recording=recording,
            )
        )

        assert replayed == recording
        assert replay_first == live_first
        assert replay_final == live_final
        event = recording["events"][0]
        assert event["cycle"] == 1
        assert event["release_boundary"] == 1
        assert event["release_phase"] == "after_batch"
        assert live_first[0][6:8] == ("external_ingress", 1)
        assert live_first[0][11] == 1
        references[worker_count] = (
            live_first,
            live_final,
        )

    assert references[1] == references[2] == references[4]


def _between_batch_ingress_case(
    worker_count: int,
    *,
    recording: dict | None = None,
) -> tuple:
    system = _system(
        assemble("idl\nhalt"),
        cores=4,
        worker_count=worker_count,
    )
    if recording is not None:
        assert system.install_external_ingress_replay(recording) == 1

    first = system.run_cycle_batch(8, max_instructions=8)
    after_first = (
        _result_signature(first),
        tuple((cpu.pc, cpu.halted, cpu.idle) for cpu in system.cores),
    )
    if recording is None:
        system.uart.inject_input(b"B")
        recording = system.export_external_ingress_recording()
    second = system.run_cycle_batch(8, max_instructions=8)
    return (
        recording,
        after_first,
        _result_signature(second),
        tuple((cpu.pc, cpu.halted, cpu.idle) for cpu in system.cores),
        system.uart.rx_pending,
    )


def test_between_batch_live_ingress_replays_before_the_next_batch():
    references = {}
    for worker_count in (1, 2, 4):
        live = _between_batch_ingress_case(worker_count)
        replay = _between_batch_ingress_case(
            worker_count,
            recording=live[0],
        )

        assert replay == live
        event = live[0]["events"][0]
        assert event["cycle"] == 1
        assert event["release_boundary"] == 2
        assert event["release_phase"] == "before_batch"
        assert live[1][0][6] == "all_idle"
        assert live[2][6] == "all_idle"
        assert live[2][11] == 0
        references[worker_count] = live[1:]

    assert references[1] == references[2] == references[4]


def test_later_pre_batch_boundary_still_limits_explicit_clock_progression():
    recording = _between_batch_ingress_case(1)[0]
    replay = _system(
        assemble("idl\nhalt"),
        cores=4,
        worker_count=1,
    )
    assert replay.install_external_ingress_replay(recording) == 1

    with pytest.raises(
        RuntimeError,
        match="cannot cross replayed pre-batch ingress",
    ):
        replay.advance_system_cycles(2)
    with pytest.raises(
        RuntimeError,
        match="cannot cross replayed pre-batch ingress",
    ):
        replay._native_system.advance_system_cycles(2)
    with pytest.raises(
        RuntimeError,
        match="cannot cross replayed pre-batch ingress",
    ):
        replay._native_system.advance_system_to(2)

    assert int(replay._native_system.system_cycles) == 0
    assert replay.uart.rx_pending == 0
    assert (
        int(replay._native_system.external_event_batch_boundaries)
        == 0
    )


def test_pre_batch_replay_enforces_each_recorded_absolute_cycle():
    source = _system(assemble("halt"), cores=1)
    source.uart.inject_input(b"A")
    source.advance_system_cycles(1)
    source.uart.inject_input(b"B")
    recording = source.export_external_ingress_recording()

    assert [
        (
            event["cycle"],
            event["release_boundary"],
            event["release_phase"],
        )
        for event in recording["events"]
    ] == [
        (0, 1, "before_batch"),
        (1, 1, "before_batch"),
    ]

    replay = _system(assemble("halt"), cores=1)
    replay.install_external_ingress_replay(recording)

    with pytest.raises(
        RuntimeError,
        match="cannot cross replayed pre-batch ingress",
    ):
        replay.advance_system_cycles(2)

    assert replay.uart.rx_pending == 1
    replay.advance_system_cycles(1)
    assert replay.uart.rx_pending == 1
    replay.advance_system_cycles(0)
    assert replay.uart.rx_pending == 2
    assert int(replay._native_system.system_cycles) == 1


def test_replay_preserves_history_order_and_cycle_delivery_order():
    source = _system(assemble("halt"), cores=1)
    source.schedule_terminal_resize(90, 30, at_cycle=5)
    source.schedule_terminal_resize(120, 40, at_cycle=2)
    recording = source.export_external_ingress_recording()

    assert tuple(
        event["sequence"] for event in recording["events"]
    ) == (1, 2)
    assert tuple(
        event["cycle"] for event in recording["events"]
    ) == (5, 2)
    assert all(
        event["release_phase"] == "scheduler"
        for event in recording["events"]
    )

    target = _system(assemble("halt"), cores=1)
    target.cpu.halted = True
    assert target.install_external_ingress_replay(recording) == 2

    early = target.run_cycle_batch(2, max_instructions=1)

    assert early.stop_cycle == 2
    assert early.external_events_applied == 1
    assert (target.uart_geom.cols, target.uart_geom.rows) == (120, 40)
    assert [
        event.sequence
        for event in target._native_system.external_event_pending
    ] == [1]
    assert [
        event.sequence
        for event in target._native_system.external_event_history
    ] == [1, 2]

    late = target.run_cycle_batch(3, max_instructions=1)

    assert late.stop_cycle == 5
    assert late.external_events_applied == 1
    assert (target.uart_geom.cols, target.uart_geom.rows) == (90, 30)


@pytest.mark.parametrize(
    ("instruction", "terminal_reason"),
    [
        ("halt", "all_halted"),
        ("idl", "all_idle"),
    ],
)
def test_instruction_cap_reaches_terminal_retirement_frontier_before_stop(
    instruction: str,
    terminal_reason: str,
):
    signatures = {}
    for worker_count in (1, 2, 4):
        system = _system(
            assemble(instruction),
            cores=4,
            worker_count=worker_count,
        )

        capped = system.run_cycle_batch(
            16,
            max_instructions=4,
        )
        terminal = system.run_cycle_batch(
            16,
            max_instructions=4,
        )

        assert capped.system_stop_reason == "instruction_limit"
        assert capped.instructions_executed == 4
        assert capped.stop_cycle == 1
        assert capped.system_cycles_advanced == 1
        assert terminal.system_stop_reason == terminal_reason
        assert terminal.instructions_executed == 0
        assert terminal.stop_cycle == 1
        assert terminal.system_cycles_advanced == 0
        signatures[worker_count] = (
            _result_signature(capped),
            _result_signature(terminal),
            tuple(
                (
                    cpu.pc,
                    cpu.cycle_count,
                    cpu.halted,
                    cpu.idle,
                )
                for cpu in system.cores
            ),
        )

    assert signatures[1] == signatures[2] == signatures[4]


def test_event_wake_rebases_serial_instruction_ready_cycle_across_lanes():
    signatures = {}
    for worker_count in (1, 2, 4):
        # EI is intentionally coordinator-owned, so this exercises the
        # resumable serial path rather than the strict private helper path.
        system = _system(
            assemble("ei\nhalt"),
            cores=1,
            worker_count=worker_count,
        )
        system.cpu.flag_i = False
        system.cpu.idle = True
        system.schedule_uart_input(b"W", at_cycle=3)

        result = system.run_cycle_batch(
            10,
            max_instructions=1,
        )

        assert result.system_stop_reason == "instruction_limit"
        assert result.external_events_applied == 1
        assert result.instructions_executed == 1
        assert result.stop_cycle == 4
        assert result.system_cycles_advanced == 4
        assert system.cpu.pc == 1
        assert system.cpu.flag_i
        assert system.cpu.cycle_count == 1
        signatures[worker_count] = (
            _result_signature(result),
            (
                system.cpu.pc,
                system.cpu.flags_pack(),
                system.cpu.cycle_count,
                int(system._native_system.system_cycles),
            ),
        )

    assert signatures[1] == signatures[2] == signatures[4]


@pytest.mark.parametrize(
    ("with_horizon", "expected_reason"),
    [
        (False, "cycle_limit"),
        (True, "event_horizon"),
    ],
)
def test_cycle_and_event_limits_win_ties_with_instruction_cap_across_lanes(
    with_horizon: bool,
    expected_reason: str,
):
    signatures = {}
    for worker_count in (1, 2, 4):
        system = _system(
            assemble("nop\nhalt"),
            cores=4,
            worker_count=worker_count,
        )
        if with_horizon:
            system._native_system.set_event_deadline(
                system._native_system.EVENT_EXTERNAL,
                1,
            )

        result = system.run_cycle_batch(
            1,
            max_instructions=4,
        )

        assert result.system_stop_reason == expected_reason
        assert result.instructions_executed == 4
        assert result.stop_cycle == 1
        assert result.system_cycles_advanced == 1
        signatures[worker_count] = (
            _result_signature(result),
            tuple(
                (
                    cpu.pc,
                    cpu.cycle_count,
                    cpu.halted,
                )
                for cpu in system.cores
            ),
        )

    assert signatures[1] == signatures[2] == signatures[4]


def test_strict_private_hardware_counters_wrap_across_lanes():
    signatures = {}
    maximum = (1 << 64) - 1
    for worker_count in (1, 2, 4):
        system = _system(
            assemble("nop\nhalt"),
            cores=4,
            worker_count=worker_count,
        )
        for cpu in system.cores:
            cpu.cycle_count = maximum
            cpu.perf_enable = 1
            cpu.perf_cycles = maximum
            cpu.icache_hits = maximum

        result = system.run_cycle_batch(
            2,
            max_instructions=4,
        )

        assert result.system_stop_reason == "instruction_limit"
        assert result.stop_cycle == 1
        assert all(cpu.pc == 1 for cpu in system.cores)
        assert all(cpu.cycle_count == 0 for cpu in system.cores)
        assert all(cpu.perf_cycles == 0 for cpu in system.cores)
        assert all(cpu.icache_hits == 0 for cpu in system.cores)
        signatures[worker_count] = (
            _result_signature(result),
            tuple(
                (
                    cpu.pc,
                    cpu.cycle_count,
                    cpu.perf_cycles,
                    cpu.icache_hits,
                )
                for cpu in system.cores
            ),
        )

    assert signatures[1] == signatures[2] == signatures[4]


def _dma_completion_failure_signature(worker_count: int) -> tuple:
    system = _system(
        assemble("halt"),
        cores=1,
        worker_count=worker_count,
    )
    system.cpu.halted = True
    completion_log: list[tuple] = []
    nic = _FailingDmaEndpoint(
        -1,
        [
            _mp64_accel.DmaBeat(
                1,
                _mp64_accel.BusOperation.WRITE,
                0x190,
                0x5A,
            )
        ],
        completion_log,
    )
    disk = _DmaEndpoint(-2, [], completion_log)
    _attach_dma_endpoints(system, nic, disk)

    with pytest.raises(RuntimeError, match="completion boom"):
        system.run_cycle_batch(
            8,
            max_instructions=10,
        )

    after_failure = (
        system.cpu.mem[0x190],
        tuple(completion_log),
        _bus_signature(system),
        int(system._native_system.system_cycles),
        tuple(
            dict(endpoint)
            for endpoint in
            system._native_system._cycle_dma_snapshot()[
                "endpoints"
            ]
        ),
    )
    resumed = system.run_cycle_batch(
        8,
        max_instructions=10,
    )
    assert resumed.system_stop_reason == "all_halted"
    assert system.cpu.mem[0x190] == 0x5A
    assert len(completion_log) == 1
    return (
        after_failure,
        _result_signature(resumed),
        _bus_signature(system),
        tuple(completion_log),
    )


def test_dma_completion_failure_never_replays_committed_target_or_token():
    signatures = {
        worker_count: _dma_completion_failure_signature(worker_count)
        for worker_count in (1, 2, 4)
    }
    assert signatures[1] == signatures[2] == signatures[4]


def test_external_ingress_replay_validation_is_transactional_and_sealed():
    source = _system(assemble("idl"), cores=1)
    source.schedule_nic_frame(b"N", at_cycle=5)
    recording = source.export_external_ingress_recording()

    invalid_sequence = deepcopy(recording)
    invalid_sequence["events"][0]["sequence"] = 2
    target = _system(assemble("idl"), cores=1)
    with pytest.raises(ValueError, match="contiguous"):
        target.install_external_ingress_replay(
            invalid_sequence
        )
    assert target._native_system.external_event_history == []
    assert target._native_system.external_event_pending == []
    assert not target._native_system.external_event_replay_sealed

    invalid_payload = deepcopy(recording)
    invalid_payload["events"][0]["payload"] = b""
    with pytest.raises(ValueError, match="frame size"):
        target.install_external_ingress_replay(
            invalid_payload
        )
    assert target._native_system.external_event_history == []
    assert target._native_system.external_event_pending == []
    assert not target._native_system.external_event_replay_sealed

    assert target.install_external_ingress_replay(recording) == 1
    assert target.export_external_ingress_recording() == recording
    assert target._native_system.external_event_replay_sealed
    with pytest.raises(RuntimeError, match="disabled during replay"):
        target.schedule_uart_input(b"live", at_cycle=6)
    assert target.export_external_ingress_recording() == recording


def test_rejected_nic_ingress_and_geometry_facades_are_sealed_and_replayed():
    source = _system(assemble("halt"), cores=1)

    assert not source.nic.inject_frame(b"")
    recording = source.export_external_ingress_recording()
    event = recording["events"][0]
    assert event["kind"] == "nic_rx_rejected"
    assert event["payload"] == b""
    assert event["release_boundary"] == 1
    assert event["release_phase"] == "before_batch"
    assert source.cpu._cs.nic_read8(NIC_BASE + 0x01) & 0x08

    replay = _system(assemble("halt"), cores=1)
    assert replay.install_external_ingress_replay(recording) == 1
    result = replay.run_cycle_batch(4, max_instructions=1)

    assert result.external_events_applied == 0
    assert replay.cpu._cs.nic_read8(NIC_BASE + 0x01) & 0x08
    assert replay.export_external_ingress_recording() == recording

    empty_source = _system(assemble("halt"), cores=1)
    sealed = _system(assemble("halt"), cores=1)
    sealed.uart_geom.req_cols = 100
    sealed.uart_geom.req_rows = 35
    sealed.uart_geom.ctrl = 0x03
    generation, cols, rows = (
        sealed.uart_geom.snapshot_resize_request()
    )
    assert (
        sealed.install_external_ingress_replay(
            empty_source.export_external_ingress_recording()
        )
        == 0
    )
    live_facades = (
        lambda: sealed.uart.inject_input(b"U"),
        lambda: sealed.nic.inject_frame(b"N"),
        lambda: sealed.nic.inject_frame(b""),
        lambda: sealed.uart_geom.host_set_size(90, 30),
        lambda: sealed.uart_geom.host_accept_resize(90, 30),
        lambda: sealed.uart_geom.host_deny_resize(),
        lambda: sealed.uart_geom.host_accept_resize_if_pending(
            generation,
            cols,
            rows,
        ),
        lambda: sealed.uart_geom.host_deny_resize_if_pending(
            generation
        ),
    )
    for facade in live_facades:
        with pytest.raises(
            RuntimeError,
            match="disabled during replay",
        ):
            facade()

    assert sealed.uart_geom.snapshot_resize_request() == (
        generation,
        cols,
        rows,
    )


@pytest.mark.parametrize(
    ("action", "expected_kind"),
    [
        ("host_size", "uart_geometry"),
        ("conditional_accept", "uart_geometry_accept"),
        ("conditional_deny", "uart_geometry_deny"),
        (
            "unconditional_accept",
            "uart_geometry_accept_unconditional",
        ),
        (
            "unconditional_deny",
            "uart_geometry_deny_unconditional",
        ),
    ],
)
def test_every_geometry_host_transition_replays_exactly(
    action: str,
    expected_kind: str,
):
    def prepare(system: MegapadSystem) -> tuple | None:
        if action.startswith("conditional"):
            system.uart_geom.req_cols = 100
            system.uart_geom.req_rows = 35
            system.uart_geom.ctrl = 0x03
            return system.uart_geom.snapshot_resize_request()
        return None

    source = _system(assemble("halt"), cores=1)
    request = prepare(source)
    if action == "host_size":
        source.uart_geom.host_set_size(90, 30)
    elif action == "conditional_accept":
        assert request is not None
        assert source.uart_geom.host_accept_resize_if_pending(
            request[0],
            100,
            35,
        )
    elif action == "conditional_deny":
        assert request is not None
        assert source.uart_geom.host_deny_resize_if_pending(
            request[0]
        )
    elif action == "unconditional_accept":
        source.uart_geom.host_accept_resize(110, 40)
    else:
        source.uart_geom.host_deny_resize()

    recording = source.export_external_ingress_recording()
    source_result = source.run_cycle_batch(2, max_instructions=1)
    source_state = (
        _result_signature(source_result),
        source.uart_geom.cols,
        source.uart_geom.rows,
        source.uart_geom.status,
        source.uart_geom.ctrl,
        source.uart_geom.snapshot_resize_request(),
    )

    replay = _system(assemble("halt"), cores=1)
    prepare(replay)
    replay.install_external_ingress_replay(recording)
    replay_result = replay.run_cycle_batch(2, max_instructions=1)
    replay_state = (
        _result_signature(replay_result),
        replay.uart_geom.cols,
        replay.uart_geom.rows,
        replay.uart_geom.status,
        replay.uart_geom.ctrl,
        replay.uart_geom.snapshot_resize_request(),
    )

    assert recording["events"][0]["kind"] == expected_kind
    assert recording["events"][0]["release_phase"] == "before_batch"
    assert replay.export_external_ingress_recording() == recording
    assert replay_state == source_state


def test_replay_install_rejects_dirty_past_suspended_and_active_timelines():
    future_source = _system(assemble("halt"), cores=1)
    future_source.schedule_uart_input(b"F", at_cycle=5)
    future_recording = (
        future_source.export_external_ingress_recording()
    )

    dirty = _system(assemble("halt"), cores=1)
    dirty.schedule_uart_input(b"D", at_cycle=7)
    before_dirty = dirty.export_external_ingress_recording()
    with pytest.raises(RuntimeError, match="fresh journal"):
        dirty.install_external_ingress_replay(future_recording)
    assert dirty.export_external_ingress_recording() == before_dirty
    assert not dirty._native_system.external_event_replay_sealed

    immediate_source = _system(assemble("halt"), cores=1)
    immediate_source.schedule_uart_input(b"P")
    immediate_recording = (
        immediate_source.export_external_ingress_recording()
    )
    past = _system(assemble("halt"), cores=1)
    past.advance_system_cycles(1)
    with pytest.raises(ValueError, match="precedes"):
        past.install_external_ingress_replay(immediate_recording)
    assert past._native_system.external_event_history == []
    assert not past._native_system.external_event_replay_sealed

    suspended = _system(assemble("mul r1, r2\nhalt"), cores=1)
    suspended.cpu.regs[1] = 6
    suspended.cpu.regs[2] = 7
    suspended.run_cycle_batch(3, max_instructions=1)
    assert suspended._native_system.cycle_execution_pending
    with pytest.raises(RuntimeError, match="clean cycle timeline"):
        suspended.install_external_ingress_replay(future_recording)
    assert not suspended._native_system.external_event_replay_sealed

    active = _system(assemble("halt"), cores=1)
    request = _mp64_accel.BusRequest(
        requester_id=0,
        ready_cycle=0,
        operation=_mp64_accel.BusOperation.READ,
        address=0x180,
        width=_mp64_accel.BusWidth.BYTE,
        write_data=0,
        ordering=_mp64_accel.BusOrderingMetadata(
            main_port_id=0,
            issue_sequence=1,
            port_io=False,
        ),
    )
    active._native_system._main_bus_try_grant([request])
    with pytest.raises(RuntimeError, match="clean cycle timeline"):
        active.install_external_ingress_replay(future_recording)
    assert not active._native_system.external_event_replay_sealed
    assert (
        active._native_system._main_bus_snapshot().active_grant
        is not None
    )


def test_step_replay_releases_between_call_ingress_before_execution():
    source = _system(assemble("idl\nhalt"), cores=1)

    first = source.step()
    source.uart.inject_input(b"S")
    recording = source.export_external_ingress_recording()
    second = source.step()
    source_state = (
        first,
        second,
        source.cpu.pc,
        source.cpu.halted,
        source.cpu.idle,
        source.cpu.cycle_count,
        int(source._native_system.system_cycles),
        source.uart.rx_pending,
    )

    replay = _system(assemble("idl\nhalt"), cores=1)
    replay.install_external_ingress_replay(recording)
    replay_state = (
        replay.step(),
        replay.step(),
        replay.cpu.pc,
        replay.cpu.halted,
        replay.cpu.idle,
        replay.cpu.cycle_count,
        int(replay._native_system.system_cycles),
        replay.uart.rx_pending,
    )

    assert replay_state == source_state


@pytest.mark.parametrize(
    ("instruction", "terminal_reason"),
    [
        ("halt", "all_halted"),
        ("idl", "all_idle"),
    ],
)
def test_unbounded_native_stop_reason_and_cycle_are_lane_invariant(
    instruction: str,
    terminal_reason: str,
):
    signatures = {}
    for worker_count in (1, 2, 4):
        system = _system(
            assemble(instruction),
            cores=4,
            worker_count=worker_count,
        )
        result = system.run_batch_stats(8)
        assert result.system_stop_reason == terminal_reason
        assert result.stop_cycle == 1
        signatures[worker_count] = (
            _result_signature(result),
            tuple(
                (
                    cpu.pc,
                    cpu.cycle_count,
                    cpu.halted,
                    cpu.idle,
                )
                for cpu in system.cores
            ),
        )
    assert signatures[1] == signatures[2] == signatures[4]


def test_unbounded_instruction_limit_and_zero_budget_report_stop_cycle():
    system = _system(
        assemble("nop\nnop\nnop\nnop"),
        cores=2,
        worker_count=4,
    )
    limited = system.run_batch_stats(2)

    assert limited.system_stop_reason == "instruction_limit"
    assert limited.instructions_executed == 2
    assert limited.stop_cycle == 1

    no_progress = system.run_batch_stats(0)

    assert no_progress.system_stop_reason == "instruction_limit"
    assert no_progress.instructions_executed == 0
    assert no_progress.system_cycles_advanced == 0
    assert no_progress.stop_cycle == 1
