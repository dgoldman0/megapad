"""Phase 4 Element 3 proven-private span oracles."""

from __future__ import annotations

import pytest

from asm import assemble
from system import MegapadSystem


PRIVATE_LOOP = assemble(
    """
loop:
    add r4, r5
    xor r6, r4
    roli r6, 7
    addi r7, 1
    br loop
"""
)


def _system(*, worker_count: int) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
        cpu.flag_i = 0
    return system


def _core_signature(system: MegapadSystem) -> tuple:
    return tuple(
        (
            cpu.pc,
            tuple(cpu.regs[index] for index in range(9)),
            cpu.cycle_count,
            cpu.halted,
            cpu.idle,
            cpu.flag_i,
            cpu.irq_ipi,
        )
        for cpu in system.cores
    )


def _sole_participant_span_signature(
    worker_count: int,
    *,
    profile: str,
) -> tuple[tuple, tuple]:
    system = _system(worker_count=worker_count)
    system.load_binary(0, PRIVATE_LOOP)
    if profile == "full":
        active = system.cores[0]
    elif profile == "micro":
        active = system.clusters[0].cores[0]
    else:
        raise AssertionError(f"unknown test profile {profile!r}")
    active.pc = 0
    active.regs[4] = 1
    active.regs[5] = 3
    active.regs[6] = 7
    active.regs[7] = 0
    active.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    stats = system.run_batch_stats(2_000)
    snapshot = dict(owner._stop_concurrency_profile())
    counts = dict(snapshot["counts"])
    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.system_stop_reason,
        owner.system_cycles,
        system._scheduler_cursor,
        _core_signature(system),
        bytes(system.cpu.mem[:len(PRIVATE_LOOP)]),
    )
    structure = (
        counts["logical_subfrontiers"],
        counts["frontier_routing_commands"],
        counts["worker_bypassed_commands"],
        counts["worker_commands"],
        counts["worker_waves"],
        counts["private_steps"],
        counts["checkpoint_captures"],
        counts["coordinator_boundaries"],
    )
    return architectural, structure


@pytest.mark.parametrize(
    ("profile", "expected_structure"),
    (
        pytest.param(
            "full",
            (3, 3, 1, 2, 2, 1_999, 2, 1),
            id="full-core",
        ),
        pytest.param(
            "micro",
            (2, 2, 0, 2, 2, 2_000, 2, 0),
            id="microcore",
        ),
    ),
)
def test_sole_mixed_topology_participant_uses_long_private_spans(
    profile: str,
    expected_structure: tuple,
) -> None:
    signatures = {
        worker_count:
            _sole_participant_span_signature(
                worker_count,
                profile=profile,
            )
        for worker_count in (1, 2, 4)
    }

    assert signatures[2][0] == signatures[1][0]
    assert signatures[4][0] == signatures[1][0]
    assert signatures[1][0][0] == 2_000
    assert signatures[1][0][2] == (
        (2_000, 0, 0, 0, 0)
        if profile == "full"
        else (0, 2_000, 0, 0, 0)
    )
    for _worker_count, (_architectural, structure) in signatures.items():
        assert structure == expected_structure


def _shrinking_mixed_frontier_signature(
    worker_count: int,
) -> tuple[tuple, tuple]:
    system = _system(worker_count=worker_count)
    system.load_binary(0, assemble("halt"))
    system.load_binary(0x100, PRIVATE_LOOP)
    full = system.cores[0]
    micro = system.clusters[0].cores[0]
    full.pc = 0
    full.halted = False
    micro.pc = 0x100
    micro.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    stats = system.run_batch_stats(2_000)
    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    architectural = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.system_stop_reason,
        owner.system_cycles,
        system._scheduler_cursor,
        _core_signature(system),
    )
    structure = (
        counts["logical_subfrontiers"],
        counts["frontier_routing_commands"],
        counts["worker_bypassed_commands"],
        counts["worker_commands"],
        counts["private_steps"],
        counts["checkpoint_captures"],
        counts["coordinator_boundaries"],
    )
    return architectural, structure


def test_mixed_frontier_shrinks_then_widens_the_survivor() -> None:
    signatures = {
        worker_count:
            _shrinking_mixed_frontier_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2][0] == signatures[1][0]
    assert signatures[4][0] == signatures[1][0]
    reference, structure = signatures[1]
    assert reference[0] == 2_000
    assert reference[2] == (1, 1_999, 0, 0, 0)
    assert reference[4] == (1, 2, 0, 0, 0)
    assert reference[11][0][3] is True
    assert structure == (3, 4, 1, 3, 1_999, 3, 1)
    assert signatures[2][1] == structure
    assert signatures[4][1] == structure


def _sole_micro_self_modifying_signature(
    worker_count: int,
) -> tuple:
    labels: dict[str, int] = {}
    program = assemble(
        """
    inc r4
    inc r4
    inc r4
    inc r4
    st.b r1, r2
target:
    inc r4
    halt
""",
        labels_out=labels,
    )
    target = labels["target"]
    halt_opcode = assemble("halt")[0]
    system = _system(worker_count=worker_count)
    system.load_binary(0, program)
    micro = system.clusters[0].cores[0]
    micro.pc = 0
    micro.regs[1] = target
    micro.regs[2] = halt_opcode
    micro.regs[4] = 0
    micro.halted = False

    owner = system._native_system
    owner._start_concurrency_profile()
    stats = system.run_batch_stats(6)
    counts = dict(
        dict(owner._stop_concurrency_profile())["counts"]
    )
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_rounds,
        stats.system_stop_reason,
        owner.system_cycles,
        system._scheduler_cursor,
        _core_signature(system),
        system.cpu.mem[target],
        target,
        (
            counts["logical_subfrontiers"],
            counts["frontier_routing_commands"],
            counts["worker_commands"],
            counts["private_steps"],
            counts["checkpoint_captures"],
            counts["coordinator_boundaries"],
        ),
    )


def test_sole_micro_span_stops_before_self_modifying_store() -> None:
    signatures = {
        worker_count:
            _sole_micro_self_modifying_signature(
                worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 6
    assert reference[2] == (0, 6, 0, 0, 0)
    assert reference[10][1][1][4] == 4
    assert reference[10][1][3] is True
    assert reference[11] == assemble("halt")[0]
    assert reference[13] == (2, 2, 2, 5, 2, 1)


def _sole_micro_callback_failure_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    program = assemble(
        """
    inc r4
    inc r4
    inc r4
    inc r4
    out1
"""
    )
    system.load_binary(0, program)
    micro = system.clusters[0].cores[0]
    micro.pc = 0
    micro.regs[2] = 0x300
    micro.regs[4] = 0
    micro.halted = False
    system.cpu.mem[0x300] = 0xA5

    trace = []
    failure = RuntimeError(
        "sole microcore span callback failure oracle"
    )

    def fail_fallback():
        trace.append(system.cpu.mem[0x300])
        raise failure

    micro._step_python_fallback = fail_fallback
    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(6)
    assert raised.value is failure
    return (
        _core_signature(system),
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(trace),
    )


def test_sole_micro_callback_failure_retains_long_private_prefix() -> None:
    signatures = {
        worker_count:
            _sole_micro_callback_failure_signature(
                worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0][1][1][4] == 4
    assert reference[1:] == (4, 2, (0xA5,))


def _mixed_callback_order_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    full = system.cores[0]
    micro = system.clusters[0].cores[0]
    system.load_binary(
        0,
        assemble("inc r4\nout1\nhalt"),
    )
    system.load_binary(
        0x100,
        assemble("out1\nhalt"),
    )
    full.pc = 0
    full.regs[2] = 0x300
    full.regs[4] = 0
    full.halted = False
    micro.pc = 0x100
    micro.regs[2] = 0x301
    micro.halted = False
    system.cpu.mem[0x300] = 0xF0
    system.cpu.mem[0x301] = 0xA0

    trace = []
    full.on_output = (
        lambda _port, _value:
        trace.append("full")
    )
    original_micro_fallback = micro._step_python_fallback

    def observe_micro_fallback():
        trace.append("micro")
        return original_micro_fallback()

    micro._step_python_fallback = observe_micro_fallback
    stats = system.run_batch_stats(3)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.system_stop_reason,
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(trace),
        _core_signature(system),
    )


def test_active_mixed_frontier_keeps_versioned_callback_order() -> None:
    signatures = {
        worker_count:
            _mixed_callback_order_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[0] == 3
    assert reference[2] == (2, 1, 0, 0, 0)
    assert reference[11] == ("micro", "full")
    assert reference[12][0][1][4] == 1


def _mixed_interrupt_boundary_signature(
    worker_count: int,
) -> tuple:
    system = _system(worker_count=worker_count)
    full = system.cores[0]
    micro = system.clusters[0].cores[0]
    system.load_binary(
        0,
        assemble("loop:\ninc r4\nbr loop"),
    )
    system.load_binary(
        0x100,
        assemble("out1\nhalt"),
    )
    full.pc = 0
    full.regs[4] = 0
    full.flag_i = 1
    full.halted = False
    micro.pc = 0x100
    micro.regs[2] = 0x300
    micro.halted = False
    system.cpu.mem[0x300] = 0x5A

    trace = []

    original_micro_fallback = micro._step_python_fallback

    def assert_ipi():
        trace.append("callback")
        full.irq_ipi = True
        return original_micro_fallback()

    def halt_at_interrupt(vector):
        trace.append(("interrupt", vector))
        full.irq_ipi = False
        full.flag_i = 0
        full.halted = True

    micro._step_python_fallback = assert_ipi
    full._trap = halt_at_interrupt
    stats = system.run_batch_stats(10)
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_interrupts,
        stats.per_core_stop_reasons,
        stats.native_continuations,
        stats.native_rounds,
        stats.system_stop_reason,
        system._native_system.system_cycles,
        system._scheduler_cursor,
        tuple(trace),
        _core_signature(system),
    )


def test_active_mixed_frontier_cannot_cross_peer_asserted_interrupt() -> None:
    signatures = {
        worker_count:
            _mixed_interrupt_boundary_signature(
                worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    reference = signatures[1]
    assert reference[2] == (1, 2, 0, 0, 0)
    assert reference[12] == (
        "callback",
        ("interrupt", 8),
    )
    assert reference[13][0][1][4] == 1
    assert reference[13][0][3] is True
