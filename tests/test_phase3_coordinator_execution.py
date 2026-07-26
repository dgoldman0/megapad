"""Phase 3 all-core coordinator and ordered-commit oracles."""

from __future__ import annotations

import threading
from collections.abc import Iterable

import pytest

from asm import assemble
from devices import MMIO_BASE, SYSINFO_BASE
from system import MegapadSystem


LINE_BYTES = 16
SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE
INT64_MAX = (1 << 63) - 1


def _system(
    *,
    num_cores: int,
    worker_count: int,
    num_clusters: int = 0,
) -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=num_cores,
        num_clusters=num_clusters,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )


def _prime_instruction_cache(
    cores: Iterable,
    memory: bytearray,
    address: int,
    size: int,
) -> None:
    first_line = address & ~(LINE_BYTES - 1)
    last_line = (
        address + size - 1
    ) & ~(LINE_BYTES - 1)
    for core in cores:
        valid_bytes, tags, data_bytes = core.icache_snapshot()
        valid = bytearray(valid_bytes)
        tags = list(tags)
        data = bytearray(data_bytes)
        line_address = first_line
        while line_address <= last_line:
            index = (line_address >> 4) & 0xFF
            valid[index] = 1
            tags[index] = line_address >> 12
            data_offset = index * LINE_BYTES
            data[
                data_offset:data_offset + LINE_BYTES
            ] = memory[
                line_address:line_address + LINE_BYTES
            ]
            line_address += LINE_BYTES
        core.icache_restore(
            bytes(valid),
            tags,
            bytes(data),
        )


def _lane_diagnostics(system: MegapadSystem) -> tuple[dict, ...]:
    diagnostics = dict(
        system._native_system._private_worker_diagnostics()
    )
    return tuple(
        dict(lane)
        for lane in diagnostics["lanes"]
    )


def _ordered_commit_signature(
    worker_count: int,
) -> tuple[tuple, tuple]:
    system = _system(
        num_cores=4,
        worker_count=worker_count,
    )
    code = assemble(
        """
    nop
    mul r4, r5
    st.b r1, r2
"""
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    system._scheduler_cursor = 3

    caller_thread = threading.get_ident()
    callback_trace = []
    for core_index, cpu in enumerate(system.cores):
        cpu.regs[1] = SYSINFO_SINK
        cpu.regs[2] = 0x40 + core_index
        cpu.regs[4] = 2
        cpu.regs[5] = 3

        def observe_write(
            address,
            value,
            *,
            observed_core=core_index,
        ):
            callback_trace.append(
                (
                    observed_core,
                    address,
                    value,
                    threading.get_ident(),
                )
            )

        cpu._mmio_write8 = observe_write

    before = dict(
        system._native_system._private_worker_diagnostics()
    )
    before_lanes = _lane_diagnostics(system)
    stats = system.run_batch_stats(12)
    after = dict(
        system._native_system._private_worker_diagnostics()
    )
    after_lanes = _lane_diagnostics(system)

    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        tuple(cpu.regs[4] for cpu in system.cores),
        tuple(
            (core, address, value)
            for core, address, value, _thread in callback_trace
        ),
    )
    physical = (
        after["wave_epoch"] - before["wave_epoch"],
        tuple(
            after_lane["completed_commands"] -
            before_lane["completed_commands"]
            for before_lane, after_lane in zip(
                before_lanes, after_lanes, strict=True
            )
        ),
        tuple(
            after_lane["completed_steps"] -
            before_lane["completed_steps"]
            for before_lane, after_lane in zip(
                before_lanes, after_lanes, strict=True
            )
        ),
    )

    assert all(
        thread == caller_thread
        for _core, _address, _value, thread
        in callback_trace
    )
    return architectural, physical


def test_complete_logical_frontier_is_lane_width_independent() -> None:
    observed = {
        worker_count: _ordered_commit_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    reference = observed[1][0]
    assert observed[2][0] == reference
    assert observed[4][0] == reference
    assert reference == (
        12,
        6,
        (3, 3, 3, 3),
        (6, 6, 6, 6),
        (1, 1, 1, 1),
        (
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
        ),
        0,
        1,
        3,
        (6, 6, 6, 6),
        (
            (3, SYSINFO_SINK, 0x43),
            (0, SYSINFO_SINK, 0x40),
            (1, SYSINFO_SINK, 0x41),
            (2, SYSINFO_SINK, 0x42),
        ),
    )
    assert observed[1][1] == (4, (4,), (4,))
    assert observed[2][1] == (2, (2, 2), (2, 2))
    assert observed[4][1] == (
        1,
        (1, 1, 1, 1),
        (1, 1, 1, 1),
    )


def _equal_credit_signature(worker_count: int) -> tuple:
    system = _system(
        num_cores=2,
        worker_count=worker_count,
    )
    system.load_binary(
        0,
        assemble(
            f"""
loop:
    st.b r1, r2
    br loop
"""
        ),
    )
    system.load_binary(
        0x100,
        assemble(
            """
loop:
    inc r4
    br loop
"""
        ),
    )
    system.boot(entry=0)
    system.cores[1].pc = 0x100
    system.cores[0].regs[1] = SYSINFO_SINK
    system.cores[0].regs[2] = 0xA5
    writes = []
    system.cores[0]._mmio_write8 = (
        lambda address, value:
        writes.append((address, value))
    )

    stats = system.run_batch_stats(20)

    return (
        stats.instructions_executed,
        stats.per_core_instructions,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_rounds,
        system._scheduler_cursor,
        system.cores[1].regs[4],
        tuple(writes),
    )


def test_shared_boundaries_do_not_create_a_secondary_qos_weight() -> None:
    signatures = {
        worker_count: _equal_credit_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1] == (
        20,
        (10, 10),
        (1, 1),
        (
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
        ),
        1,
        0,
        5,
        ((SYSINFO_SINK, 0xA5),) * 5,
    )


@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_unused_early_credit_flows_forward_in_the_same_round(
    worker_count: int,
) -> None:
    system = _system(
        num_cores=2,
        worker_count=worker_count,
    )
    system.load_binary(0, assemble("halt"))
    survivor_code = assemble(
        """
loop:
    inc r1
    br loop
"""
    )
    survivor_address = 0x40
    system.load_binary(
        survivor_address,
        survivor_code,
    )
    system.boot(entry=0)
    system.cores[1].pc = survivor_address

    stats = system.run_batch_stats(3)

    assert stats.instructions_executed == 3
    assert stats.per_core_instructions == (1, 2)
    assert stats.per_core_dispatches == (1, 1)
    assert stats.per_core_stop_reasons == (
        (0, 1, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0),
    )
    assert stats.native_rounds == 1
    assert system.cores[1].regs[1] == 1


@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_last_peer_slack_does_not_wrap_or_exceed_the_quantum(
    worker_count: int,
) -> None:
    system = _system(
        num_cores=3,
        worker_count=worker_count,
    )
    loop = assemble(
        """
loop:
    inc r1
    br loop
"""
    )
    system.load_binary(0, loop)
    system.load_binary(0x40, assemble("halt"))
    system.load_binary(0x80, loop)
    system.boot(entry=0)
    system.cores[1].pc = 0x40
    system.cores[2].pc = 0x80

    stats = system.run_batch_stats(6)

    assert stats.per_core_instructions == (3, 1, 2)
    assert stats.per_core_dispatches == (2, 1, 1)
    assert stats.per_core_stop_reasons == (
        (2, 0, 0, 0, 0, 0, 0),
        (0, 1, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0),
    )
    assert stats.native_rounds == 2
    assert tuple(
        cpu.regs[1] for cpu in system.cores
    ) == (2, 0, 1)


def test_released_credit_activates_a_zero_reservation_peer() -> None:
    system = _system(
        num_cores=2,
        worker_count=2,
    )
    system.load_binary(0, assemble("inc r1"))
    system.boot(entry=0)
    system.cores[0].flag_i = 1
    system.cores[0].irq_ipi = True
    deliveries = []

    def observe_interrupt(vector):
        deliveries.append(vector)
        system.cores[0].flag_i = 0

    system.cores[0]._trap = observe_interrupt

    stats = system.run_batch_stats(1)

    assert deliveries == [8]
    assert stats.per_core_instructions == (0, 1)
    assert stats.per_core_dispatches == (0, 1)
    assert stats.per_core_stop_reasons == (
        (0, 0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0),
    )
    assert stats.native_rounds == 1
    assert tuple(
        cpu.regs[1] for cpu in system.cores
    ) == (0, 1)


def _callback_failure_signature(worker_count: int) -> tuple:
    system = _system(
        num_cores=3,
        worker_count=worker_count,
    )
    addresses = (0, 0x40, 0x80)
    prefixes = (
        assemble("inc r4"),
        assemble("inc r4\nmul r5, r6"),
        assemble(
            "inc r4\nmul r5, r6\nmul r7, r8"
        ),
    )
    programs = tuple(
        prefix + assemble("out1")
        for prefix in prefixes
    )
    for address, program in zip(
        addresses, programs, strict=True
    ):
        system.load_binary(address, program)
    system.boot(entry=0)
    for cpu, address, program in zip(
        system.cores,
        addresses,
        programs,
        strict=True,
    ):
        cpu.pc = address
        cpu.regs[2] = 0x300
        cpu.regs[4] = 0
        cpu.regs[5] = 2
        cpu.regs[6] = 3
        cpu.regs[7] = 4
        cpu.regs[8] = 5
        _prime_instruction_cache(
            (cpu._cs,),
            system.cpu.mem,
            address,
            len(program),
        )
    system.cpu.mem[0x300] = 0x5A

    callback_trace = []
    failure = RuntimeError(
        "ordered callback failure oracle"
    )

    def fail_second(_port, _value):
        callback_trace.append(1)
        raise failure

    system.cores[0].on_output = (
        lambda _port, _value:
        callback_trace.append(0)
    )
    system.cores[1].on_output = fail_second
    system.cores[2].on_output = (
        lambda _port, _value:
        callback_trace.append(2)
    )

    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(12)

    assert raised.value is failure
    return (
        tuple(cpu.regs[4] for cpu in system.cores),
        tuple(cpu.regs[5] for cpu in system.cores),
        tuple(cpu.regs[7] for cpu in system.cores),
        tuple(cpu.pc for cpu in system.cores),
        tuple(cpu.cycle_count for cpu in system.cores),
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(callback_trace),
        addresses[2] + len(prefixes[2]),
    )


def test_callback_failure_preserves_the_complete_private_frontier() -> None:
    signatures = {
        worker_count: _callback_failure_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1][0:3] == (
        (1, 1, 1),
        (2, 6, 6),
        (4, 4, 20),
    )
    assert signatures[1][3][2] == (
        signatures[1][8]
    )
    assert signatures[1][4:8] == (
        (2, 5, 9),
        9,
        0,
        (0, 1),
    )


def _immediate_boundary_failure_signature(
    worker_count: int,
) -> tuple:
    system = _system(
        num_cores=3,
        worker_count=worker_count,
    )
    addresses = (0, 0x40, 0x80)
    immediate = assemble("out1")
    private_prefix = assemble(
        "inc r4\nmul r5, r6\nmul r7, r8"
    )
    programs = (
        immediate,
        immediate,
        private_prefix + immediate,
    )
    for address, program in zip(
        addresses, programs, strict=True
    ):
        system.load_binary(address, program)
    system.boot(entry=0)
    for cpu, address, program in zip(
        system.cores,
        addresses,
        programs,
        strict=True,
    ):
        cpu.pc = address
        cpu.regs[2] = 0x300
        cpu.regs[4] = 0
        cpu.regs[5] = 2
        cpu.regs[6] = 3
        cpu.regs[7] = 4
        cpu.regs[8] = 5
        _prime_instruction_cache(
            (cpu._cs,),
            system.cpu.mem,
            address,
            len(program),
        )
    system.cpu.mem[0x300] = 0xA5

    callback_trace = []
    failure = RuntimeError(
        "immediate boundary callback failure oracle"
    )

    system.cores[0].on_output = (
        lambda _port, _value:
        callback_trace.append(0)
    )

    def fail_second(_port, _value):
        callback_trace.append(1)
        raise failure

    system.cores[1].on_output = fail_second
    system.cores[2].on_output = (
        lambda _port, _value:
        callback_trace.append(2)
    )

    diagnostics_before = dict(
        system._native_system._private_worker_diagnostics()
    )
    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(12)
    diagnostics_after = dict(
        system._native_system._private_worker_diagnostics()
    )

    assert raised.value is failure
    return (
        tuple(cpu.regs[4] for cpu in system.cores),
        tuple(cpu.regs[5] for cpu in system.cores),
        tuple(cpu.regs[7] for cpu in system.cores),
        tuple(cpu.pc for cpu in system.cores),
        tuple(cpu.cycle_count for cpu in system.cores),
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(callback_trace),
        addresses[2] + len(private_prefix),
        (
            diagnostics_after["wave_epoch"]
            - diagnostics_before["wave_epoch"]
        ),
        (
            diagnostics_after["next_command_sequence"]
            - diagnostics_before["next_command_sequence"]
        ),
    )


def test_immediate_boundary_bypass_retains_every_peer_private_prefix() -> None:
    signatures = {
        worker_count:
            _immediate_boundary_failure_signature(
                worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0:3] == (
        (0, 0, 1),
        (2, 2, 6),
        (4, 4, 20),
    )
    assert reference[3][2] == reference[8]
    assert reference[7] == (0, 1)
    assert reference[9:] == (1, 1)


def test_exact_cycle_ceiling_does_not_mask_callback_failure() -> None:
    system = _system(
        num_cores=1,
        worker_count=2,
    )
    code = assemble(
        """
    inc r4
    t.sum
    inc r4
    out1
"""
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    system.cpu.mem[0x100:0x140] = bytes(
        range(64)
    )
    system.cpu.tsrc0 = 0x100
    system.cpu.regs[2] = 0x300
    system.cpu.mem[0x300] = 0x5A
    _prime_instruction_cache(
        (system.cpu._cs,),
        system.cpu.mem,
        0,
        len(code),
    )
    original_continuation = (
        system._settle_native_core_continuation
    )
    failure = RuntimeError(
        "cycle ceiling callback failure"
    )
    settled_rounds = []

    def inflate_first_continuation(*args):
        steps, _cycles, terminal = (
            original_continuation(*args)
        )
        return steps, INT64_MAX - 1, terminal

    def fail_output(_port, _value):
        raise failure

    system.cpu.on_output = fail_output
    callback_sets = [
        (
            system.cpu._mmio_read8,
            system.cpu._mmio_write8,
            system.cpu._do_output,
            getattr(
                system.cpu,
                "_csr_read_override",
                None,
            ),
        )
    ]

    with pytest.raises(RuntimeError) as raised:
        system._native_system.run_full_core_batch(
            4,
            callback_sets,
            system._prepare_native_full_core_batch,
            inflate_first_continuation,
            system._settle_native_core_dispatch_error,
            lambda *args: settled_rounds.append(args),
            1000,
        )

    assert raised.value is failure
    assert settled_rounds == [
        (INT64_MAX, True, True, False)
    ]
    assert system.cpu.regs[4] == 2


def test_ei_cannot_run_past_an_already_asserted_interrupt() -> None:
    system = _system(
        num_cores=1,
        worker_count=2,
    )
    system.load_binary(
        0,
        assemble("ei\ninc r1\nhalt"),
    )
    system.boot(entry=0)
    system.cpu.flag_i = 0
    system.timer.irq_pending = True
    caller_thread = threading.get_ident()
    deliveries = []

    def observe_trap(vector):
        deliveries.append(
            (
                vector,
                system.cpu.regs[1],
                threading.get_ident(),
            )
        )
        system.cpu.flag_i = 0

    system.cpu._trap = observe_trap

    stats = system.run_batch_stats(2)

    assert deliveries == [(7, 0, caller_thread)]
    assert stats.instructions_executed == 2
    assert stats.system_cycles_advanced == 2
    assert stats.per_core_instructions == (2,)
    assert stats.per_core_cycles == (2,)
    assert stats.per_core_dispatches == (2,)
    assert stats.per_core_stop_reasons == (
        (2, 0, 0, 0, 0, 0, 0),
    )
    assert stats.native_continuations == 0
    assert stats.native_rounds == 2
    assert system.cpu.regs[1] == 1


def test_shared_ram_commits_in_frozen_cyclic_frontier_order() -> None:
    system = _system(
        num_cores=2,
        worker_count=2,
    )
    program = assemble(
        """
    st.b r1, r2
    inc r2
    st.b r1, r2
"""
    )
    system.load_binary(0, program)
    system.load_binary(0x40, program)
    system.boot(entry=0)
    system.cores[1].pc = 0x40
    system._scheduler_cursor = 1
    target = 0x300
    system.cores[0].regs[1] = target
    system.cores[0].regs[2] = 0xA0
    system.cores[1].regs[1] = target
    system.cores[1].regs[2] = 0xB1

    stats = system.run_batch_stats(6)

    assert stats.per_core_instructions == (3, 3)
    assert stats.per_core_dispatches == (1, 1)
    assert stats.per_core_stop_reasons == (
        (1, 0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0),
    )
    assert system.cpu.mem[target] == 0xA1
    assert tuple(
        cpu.regs[2] for cpu in system.cores
    ) == (0xA1, 0xB2)
    assert system._scheduler_cursor == 1


@pytest.mark.parametrize(
    ("source", "state_name", "reason_index"),
    (
        pytest.param(
            "halt",
            "halted",
            1,
            id="halt",
        ),
        pytest.param(
            "idl",
            "idle",
            2,
            id="idle",
        ),
    ),
)
def test_cold_terminal_is_reported_when_credit_remains(
    source: str,
    state_name: str,
    reason_index: int,
) -> None:
    system = _system(
        num_cores=2,
        worker_count=2,
    )
    system.load_binary(0, assemble(source))
    system.boot(entry=0)

    stats = system.run_batch_stats(4)

    expected_reasons = [0] * 7
    expected_reasons[reason_index] = 1
    assert stats.instructions_executed == 2
    assert stats.system_cycles_advanced == 1
    assert stats.per_core_instructions == (1, 1)
    assert stats.per_core_cycles == (1, 1)
    assert stats.per_core_dispatches == (1, 1)
    assert stats.per_core_stop_reasons == (
        tuple(expected_reasons),
        tuple(expected_reasons),
    )
    assert stats.native_continuations == 0
    assert stats.native_rounds == 1
    assert system._scheduler_cursor == 0
    assert all(
        getattr(cpu, state_name)
        for cpu in system.cores
    )


@pytest.mark.parametrize(
    (
        "boundary",
        "expected_reason",
        "expected_trap",
        "expected_cycles",
    ),
    (
        pytest.param(
            "trap",
            5,
            6,
            1,
            id="trap",
        ),
        pytest.param(
            "reset",
            6,
            -1,
            2,
            id="reset",
        ),
    ),
)
@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_hot_private_trap_reset_prefix_is_settled_once(
    boundary: str,
    expected_reason: int,
    expected_trap: int,
    expected_cycles: int,
    worker_count: int,
) -> None:
    system = _system(
        num_cores=4,
        worker_count=worker_count,
    )
    code = assemble(f"inc r1\n{boundary}")
    system.load_binary(0, code)
    system.boot(entry=0)
    _prime_instruction_cache(
        tuple(cpu._cs for cpu in system.cores),
        system.cpu.mem,
        0,
        len(code),
    )
    original_settlement = (
        system._settle_native_core_continuation
    )
    caller_thread = threading.get_ident()
    settlements = []

    def observe_settlement(*args):
        settlements.append(
            (*args, threading.get_ident())
        )
        return original_settlement(*args)

    system._settle_native_core_continuation = (
        observe_settlement
    )

    stats = system.run_batch_stats(8)

    assert settlements == [
        (
            core_id,
            expected_reason,
            expected_trap,
            1,
            1,
            caller_thread,
        )
        for core_id in range(4)
    ]
    expected_reasons = [0] * 7
    expected_reasons[expected_reason] = 1
    assert stats.instructions_executed == 8
    assert stats.system_cycles_advanced == (
        expected_cycles
    )
    assert stats.per_core_instructions == (2, 2, 2, 2)
    assert stats.per_core_cycles == (
        expected_cycles,
        expected_cycles,
        expected_cycles,
        expected_cycles,
    )
    assert stats.per_core_dispatches == (1, 1, 1, 1)
    assert stats.per_core_stop_reasons == (
        tuple(expected_reasons),
        tuple(expected_reasons),
        tuple(expected_reasons),
        tuple(expected_reasons),
    )
    assert stats.native_continuations == 4


def _micro_private_frontier_signature(
    worker_count: int,
) -> tuple[tuple, tuple]:
    system = _system(
        num_cores=1,
        num_clusters=1,
        worker_count=worker_count,
    )
    address = 0x100
    system.load_binary(
        address,
        assemble(
            """
loop:
    inc r1
    br loop
"""
        ),
    )
    system.cores[0].halted = True
    system.cores[0].idle = False
    for cpu in system.cores[1:]:
        cpu.pc = address
        cpu.halted = False
        cpu.idle = False
    owner = system._native_system
    dispatches_before = owner.native_dispatches
    before = _lane_diagnostics(system)

    stats = system.run_batch_stats(8)
    after = _lane_diagnostics(system)

    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        system._scheduler_cursor,
        tuple(cpu.regs[1] for cpu in system.cores),
        owner.native_dispatches - dispatches_before,
    )
    physical = tuple(
        (
            after_lane["completed_commands"] -
                before_lane["completed_commands"],
            after_lane["completed_steps"] -
                before_lane["completed_steps"],
        )
        for before_lane, after_lane in zip(
            before, after, strict=True
        )
    )
    return architectural, physical


def test_reduced_private_frontier_uses_configured_workers() -> None:
    observed = {
        worker_count:
            _micro_private_frontier_signature(
                worker_count
            )
        for worker_count in (1, 2, 4)
    }

    assert observed[2][0] == observed[1][0]
    assert observed[4][0] == observed[1][0]
    assert observed[1][0] == (
        8,
        3,
        (0, 2, 2, 2, 2),
        (0, 3, 3, 3, 3),
        (0, 1, 1, 1, 1),
        (
            (0, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
        ),
        0,
        1,
        0,
        (0, 1, 1, 1, 1),
        4,
    )
    for worker_count in (2, 4):
        auxiliary = observed[worker_count][1][1:]
        assert all(
            commands > 0 and steps > 0
            for commands, steps in auxiliary
        )
