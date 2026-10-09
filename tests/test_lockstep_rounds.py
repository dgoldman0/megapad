"""Lock-step rounds on the scheduler thread match the generic coordinator.

A round with several awake cores of a cluster-free topology runs the
coordinator's passes directly on the scheduler thread.  Each workload here
runs twice from the same start, once on that path and once on the
coordinator, and every batch must leave identical guest-visible results:
batch statistics, the scheduler cursor, device time, every core's
architectural state and I-cache, all of RAM, and the order of every Python
callback.
"""

from __future__ import annotations

import pytest

from asm import assemble
from devices import MBOX_BASE, MMIO_BASE, SHA3_BASE, SYSINFO_BASE
from megapad64 import (
    IVEC_BUS_FAULT,
    IVEC_IPI,
    IVEC_PRIV_FAULT,
    IVEC_SW_TRAP,
)
from system import MegapadSystem


RAM_SIZE = 0x10000
SYSINFO_SINK = MMIO_BASE + SYSINFO_BASE
MBOX_SEND = MMIO_BASE + MBOX_BASE + 0x08
MBOX_ACK = MMIO_BASE + MBOX_BASE + 0x0A
SHA3_COMMAND = MMIO_BASE + SHA3_BASE
IVT_BASE = 0x0100
COUNTER = 0x8000
SLOTS = 0x8100
STACK_TOP = 0xF000
STACK_BYTES = 0x400
BUDGETS = (1, 2, 3, 5, 7, 11, 13, 64, 999, 1_001, 4_003, 10_007)


def _system(cores: int, *, lockstep: bool) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=RAM_SIZE,
        num_cores=cores,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    system._native_system.lockstep_fast_path = lockstep
    return system


# After boot R3 is the program counter, R2 the X register and R15 the
# stack pointer, so workloads keep their data elsewhere.


def _load(system: MegapadSystem, address: int, source: str) -> None:
    system.load_binary(address, assemble(source, base_addr=address))


def _start(system: MegapadSystem, entries: dict[int, int]) -> None:
    """Boot, then give each listed core its entry and a private stack."""
    system.boot(entry=0)
    for index, cpu in enumerate(system.cores):
        cpu.halted = index not in entries
        cpu.idle = False
        if index in entries:
            cpu.pc = entries[index]
        cpu.regs[cpu.spsel] = STACK_TOP - index * STACK_BYTES


def _install_vector(system: MegapadSystem, vector: int, handler: int) -> None:
    for cpu in system.cores:
        cpu.ivt_base = IVT_BASE
    address = IVT_BASE + vector * 8
    system.cpu.mem[address:address + 8] = handler.to_bytes(8, "little")


def _core_signature(cpu) -> tuple:
    return (
        tuple(cpu.regs),
        cpu.pc,
        cpu.psel,
        cpu.xsel,
        cpu.spsel,
        cpu.flags_pack(),
        cpu.priv_level,
        cpu.halted,
        cpu.idle,
        cpu.cycle_count,
        cpu.ivec_id,
        cpu._cs.icache_snapshot(),
    )


def _signature(system: MegapadSystem, stats) -> tuple:
    return (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        stats.native_rounds,
        stats.native_continuations,
        stats.system_stop_reason,
        system._scheduler_cursor,
        system.timer.counter,
        system._native_system.system_cycles,
        tuple(_core_signature(cpu) for cpu in system.cores),
        bytes(system.cpu.mem[:RAM_SIZE]),
    )


def _compare(build, *, cores: int, budgets=BUDGETS) -> None:
    """Run BUILD's workload on both paths and require equal batches."""
    observed = {}
    for lockstep in (True, False):
        system = _system(cores, lockstep=lockstep)
        trace: list = []
        build(system, trace)
        system.start_host_profile()
        signatures = []
        for budget in budgets:
            stats = system.run_batch_stats(budget)
            signatures.append((_signature(system, stats), tuple(trace)))
        counts = dict(system.stop_host_profile()["counts"])
        observed[lockstep] = (signatures, counts)

    lockstep_signatures, lockstep_counts = observed[True]
    reference_signatures, reference_counts = observed[False]
    for index, (lockstep, reference) in enumerate(
        zip(lockstep_signatures, reference_signatures, strict=True)
    ):
        assert lockstep == reference, f"batch {index} diverged"
    assert lockstep_counts["lockstep_rounds"] > 0
    assert lockstep_counts["lockstep_passes"] > 0
    assert lockstep_counts["logical_subfrontiers"] == 0
    assert reference_counts["lockstep_rounds"] == 0
    assert reference_counts["logical_subfrontiers"] > 0


RACE = """
race:
    ldn r6, r5
    inc r6
    str r5, r6
    inc r4
    addi r7, 3
    xor r8, r7
    br race
"""


def _race(system: MegapadSystem, _trace: list) -> None:
    _load(system, 0, RACE)
    _start(system, {index: 0 for index in range(len(system.cores))})
    for cpu in system.cores:
        cpu.regs[5] = COUNTER


@pytest.mark.parametrize("cores", (2, 3, 4))
def test_unlocked_shared_counter_matches_the_coordinator(cores: int) -> None:
    _compare(_race, cores=cores)


def _uneven(system: MegapadSystem, _trace: list) -> None:
    # Core i runs i register-only instructions between its memory accesses;
    # the last core of four does register work only.
    entries = {}
    for index in range(len(system.cores)):
        address = 0x1000 + index * 0x400
        if index == 3:
            _load(system, address, "spin:\n inc r4\n addi r5, 1\n br spin\n")
        else:
            filler = "\n".join(
                " inc r4" if step % 2 == 0 else " addi r8, 5"
                for step in range(index)
            )
            _load(
                system,
                address,
                f"""
loop:
    ldn r6, r5
{filler}
    str r9, r6
    add r6, r4
    str r5, r6
    br loop
""",
            )
        entries[index] = address
    _start(system, entries)
    for index, cpu in enumerate(system.cores):
        cpu.regs[5] = COUNTER
        cpu.regs[9] = SLOTS + 8 * index


@pytest.mark.parametrize("cores", (2, 4))
def test_uneven_private_runs_and_credit_match_the_coordinator(
    cores: int,
) -> None:
    _compare(_uneven, cores=cores)


def _mmio_sink(system: MegapadSystem, trace: list) -> None:
    _load(
        system,
        0,
        """
loop:
    inc r4
    st.b r1, r2
    addi r2, 1
    ld.b r9, r1
    add r4, r9
    mul r4, r5
    br loop
""",
    )
    _start(system, {index: 0 for index in range(len(system.cores))})
    for index, cpu in enumerate(system.cores):
        cpu.regs[1] = SYSINFO_SINK
        cpu.regs[2] = 0x40 * index
        cpu.regs[5] = 3

        def write(address, value, *, core=index):
            trace.append(("w", core, address, value))

        def read(address, *, core=index):
            trace.append(("r", core, address))
            return (address + core + len(trace)) & 0xFF

        cpu._mmio_write8 = write
        cpu._mmio_read8 = read


@pytest.mark.parametrize("cores", (2, 4))
def test_python_mmio_callbacks_run_in_the_coordinators_order(
    cores: int,
) -> None:
    _compare(_mmio_sink, cores=cores, budgets=BUDGETS[:10])


def _ipi_wake(system: MegapadSystem, _trace: list) -> None:
    # Core 0 works and sends core 1 an IPI every few iterations.  Core 1
    # sleeps in IDL with interrupts masked, so the IPI only wakes it, and it
    # acknowledges the mailbox before sleeping again.
    _load(
        system,
        0x1000,
        f"""
    ldi64 r11, {MBOX_SEND:#x}
    ldi r1, 1
sender:
    ldn r6, r5
    inc r6
    str r5, r6
    inc r4
    mov r7, r6
    andi r7, 7
    cmpi r7, 0
    brne sender
    st.b r11, r1
    br sender
""",
    )
    _load(
        system,
        0x1400,
        f"""
    ldi64 r11, {MBOX_ACK:#x}
    ldi r0, 0
sleeper:
    idl
    st.b r11, r0
    ldn r6, r5
    add r4, r6
    br sleeper
""",
    )
    _start(system, {0: 0x1000, 1: 0x1400})
    for cpu in system.cores:
        cpu.regs[5] = COUNTER
        cpu.flag_i = 0


def test_ipi_wake_from_idle_matches_the_coordinator() -> None:
    _compare(_ipi_wake, cores=2)


def _ipi_interrupts(system: MegapadSystem, _trace: list) -> None:
    # As above, but core 1 takes each IPI as an interrupt and returns.
    _ipi_wake(system, _trace)
    _load(
        system,
        0x1800,
        f"""
    ldi64 r11, {MBOX_ACK:#x}
    ldi r0, 0
    st.b r11, r0
    inc r10
    rti
""",
    )
    _load(
        system,
        0x1400,
        """
worker:
    ldn r6, r5
    add r4, r6
    inc r12
    br worker
""",
    )
    _install_vector(system, IVEC_IPI, 0x1800)
    system.cores[1].flag_i = 1


def test_ipi_interrupts_match_the_coordinator() -> None:
    _compare(_ipi_interrupts, cores=2)


def _halts_traps_and_reset(system: MegapadSystem, _trace: list) -> None:
    # Core 1 raises a software trap every fourth iteration, core 2 halts
    # after 37 iterations, and core 3 resets itself after 50.  A core that
    # resets restarts at address 0, whose stub sends it back into the race.
    _load(
        system,
        0,
        f"""
    ldi64 r5, {COUNTER:#x}
    lbr race
.org 0x40
race:
    ldn r6, r5
    inc r6
    str r5, r6
    inc r4
    addi r7, 3
    xor r8, r7
    br race
""",
    )
    _load(
        system,
        0x1000,
        """
trapper:
    ldn r6, r5
    inc r6
    str r5, r6
    inc r4
    mov r7, r4
    andi r7, 3
    cmpi r7, 0
    brne trapper
    trap
    br trapper
""",
    )
    _load(system, 0x1800, "inc r10\n rti\n")
    _load(
        system,
        0x1C00,
        """
    ldi r10, 37
halter:
    ldn r6, r5
    inc r6
    str r5, r6
    dec r10
    cmpi r10, 0
    brne halter
    halt
""",
    )
    _load(
        system,
        0x2000,
        """
    ldi r10, 50
resetter:
    ldn r6, r5
    inc r6
    str r5, r6
    dec r10
    cmpi r10, 0
    brne resetter
    reset
""",
    )
    _start(system, {0: 0, 1: 0x1000, 2: 0x1C00, 3: 0x2000})
    _install_vector(system, IVEC_SW_TRAP, 0x1800)
    for cpu in system.cores:
        cpu.regs[5] = COUNTER


def test_halts_traps_and_resets_match_the_coordinator() -> None:
    _compare(_halts_traps_and_reset, cores=4)


def _self_modifying_code(system: MegapadSystem, _trace: list) -> None:
    # Core 1 alternates between two lines that share an I-cache index, so it
    # refills each one on every visit and sees the byte core 0 keeps
    # rewriting.  Core 0 also rewrites a byte inside its own loop.
    _load(
        system,
        0x1000,
        """
writer:
    st.b r11, r12
    xori r12, 0x30
    st.b r13, r14
    xori r14, 0x30
    inc r4
own:
    inc r5
    br writer
""",
    )
    own = 0x1000 + len(
        assemble(
            "st.b r11, r12\nxori r12, 0x30\nst.b r13, r14\n"
            "xori r14, 0x30\ninc r4"
        )
    )
    _load(
        system,
        0x2000,
        """
first:
    inc r4
    lbr second
.org 0x3000
second:
    inc r5
    ldn r6, r7
    lbr first
""",
    )
    _start(system, {0: 0x1000, 1: 0x2000})
    writer = system.cores[0]
    writer.regs[11] = 0x2000
    writer.regs[12] = 0x14 ^ 0x30
    writer.regs[13] = own
    writer.regs[14] = 0x15 ^ 0x30
    system.cores[1].regs[7] = COUNTER


def test_self_modifying_code_and_icache_fills_match_the_coordinator() -> None:
    _compare(_self_modifying_code, cores=2)


def _user_faults(system: MegapadSystem, _trace: list) -> None:
    # Cores 0 and 1 run in user mode and every eighth iteration load from
    # outside their MPU window.  The fault reaches the shared instruction as
    # a trap that Python delivers, and the handler moves the address back
    # inside the window before returning.  Core 2 races on the counter.
    _load(system, 0, RACE)
    _load(
        system,
        0x1000,
        f"""
loop:
    ldn r6, r7
    add r8, r6
    inc r4
    mov r7, r4
    andi r7, 7
    cmpi r7, 0
    brne inside
    ldi64 r7, 0xA000
    br loop
inside:
    ldi64 r7, {COUNTER:#x}
    br loop
""",
    )
    _load(system, 0x1800, f"inc r10\n ldi64 r7, {COUNTER:#x}\n rti\n")
    _start(system, {0: 0x1000, 1: 0x1000, 2: 0})
    for vector in (IVEC_BUS_FAULT, IVEC_PRIV_FAULT):
        _install_vector(system, vector, 0x1800)
    for cpu in system.cores[:2]:
        cpu.regs[7] = COUNTER
        cpu.mpu_base = 0x1000
        cpu.mpu_limit = 0x9000
        cpu.priv_level = 1
    system.cores[2].regs[5] = COUNTER


def test_user_mode_faults_settled_by_python_match_the_coordinator() -> None:
    _compare(_user_faults, cores=3)


def _device_fence(system: MegapadSystem, _trace: list) -> None:
    # Core 0 restarts a SHA3 digest whenever the last one has had time to
    # finish, so the engine stays busy across rounds while the others race.
    _load(system, 0, RACE)
    _load(
        system,
        0x1000,
        f"""
    ldi64 r9, {SHA3_COMMAND:#x}
again:
    ldi r1, 7
    st.b r9, r1
    ldi r1, 1
    st.b r9, r1
    ldi r1, 3
    st.b r9, r1
    ldi r10, 12
poll:
    inc r4
    addi r5, 3
    ld.b r12, r9
    dec r10
    cmpi r10, 0
    brne poll
    br again
""",
    )
    entries = {index: 0 for index in range(len(system.cores))}
    entries[0] = 0x1000
    _start(system, entries)
    for cpu in system.cores[1:]:
        cpu.regs[5] = COUNTER


@pytest.mark.parametrize("cores", (2, 4))
def test_device_fence_rounds_match_the_coordinator(cores: int) -> None:
    _compare(_device_fence, cores=cores)


def _awake_sets(system: MegapadSystem, trace: list) -> None:
    _race(system, trace)


def test_changing_awake_sets_match_the_coordinator() -> None:
    sets = ({0, 1}, {0, 1, 2, 3}, {2}, {1, 3}, {0, 2, 3}, {3, 0})
    observed = {}
    for lockstep in (True, False):
        system = _system(4, lockstep=lockstep)
        _race(system, [])
        signatures = []
        for awake in sets:
            for index, cpu in enumerate(system.cores):
                cpu.halted = index not in awake
                cpu.idle = False
            for budget in (7, 1_003):
                stats = system.run_batch_stats(budget)
                signatures.append(_signature(system, stats))
        observed[lockstep] = signatures
    assert observed[True] == observed[False]


def _failing_callback(lockstep: bool) -> tuple:
    system = _system(4, lockstep=lockstep)
    trace: list = []
    _mmio_sink(system, trace)
    failure = RuntimeError("third write on core 2 fails")
    writes_seen = []

    def failing_write(address, value):
        trace.append(("w", 2, address, value))
        writes_seen.append(value)
        if len(writes_seen) == 3:
            raise failure

    system.cores[2]._mmio_write8 = failing_write
    with pytest.raises(RuntimeError) as raised:
        system.run_batch_stats(10_007)
    assert raised.value is failure
    after_failure = (
        system._scheduler_cursor,
        system.timer.counter,
        system._native_system.system_cycles,
        tuple(_core_signature(cpu) for cpu in system.cores),
        bytes(system.cpu.mem[:RAM_SIZE]),
        tuple(trace),
    )
    system.cores[2]._mmio_write8 = lambda address, value: trace.append(
        ("w", 2, address, value)
    )
    stats = system.run_batch_stats(4_003)
    return after_failure, _signature(system, stats), tuple(trace)


def test_python_error_mid_pass_leaves_the_coordinators_state() -> None:
    assert _failing_callback(True) == _failing_callback(False)
