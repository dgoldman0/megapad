"""Phase 0 scheduler and shared-state characterization tests.

The passing tests below pin useful current behavior and provide controls for
the strict xfails.  Each strict xfail describes a known concurrency defect
that later heterogeneous/event scheduling or architecture work must fix;
keeping those assertions red avoids turning incomplete behavior into a
permanent compatibility contract.
"""

import threading

import pytest

from asm import assemble
from devices import (
    FB_BASE,
    MBOX_BASE,
    MMIO_BASE,
    NIC_BASE,
    RTC_BASE,
    SHA3_BASE,
    SPINLOCK_BASE,
    SYSINFO_BASE,
    TIMER_BASE,
    TRNG_BASE,
    UART_BASE,
    UART_GEOM_BASE,
)
from megapad64 import CSR_IPIACK, CSR_MBOX, IVEC_IPI
from system import MegapadSystem


_SPIN = assemble(
    """
loop:
    nop
    br loop
"""
)

_COUNTING_SPIN = assemble(
    """
loop:
    inc r1
    br loop
"""
)

_MULTICYCLE_SPIN = assemble(
    """
loop:
    mul r1, r2
    br loop
"""
)

_NOP_SLED = assemble("\n".join(["nop"] * 1_001))

_UART_GEOMETRY_SPIN = assemble(
    f"""
    ldi64 r4, {MMIO_BASE + SYSINFO_BASE}
    ld.b r0, r4
    ldi64 r1, {MMIO_BASE + UART_GEOM_BASE}
loop:
    ld.b r2, r1
    br loop
"""
)


def _new_system(*, full_cores=1, clusters=0, code=_SPIN):
    system = MegapadSystem(
        ram_size=1 << 16,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        vram_size=0,
    )
    system.load_binary(0, code)
    system.boot(entry=0)
    return system


def _run_only_secondary(system, code, *, address=0x100, budget=100):
    """Run *code* natively on full core 1 while all other cores are halted."""
    system.load_binary(address, code)
    for core in system.cores:
        core.halted = True
        core.idle = False
    secondary = system.cores[1]
    secondary.pc = address
    secondary.halted = False
    return system.run_batch(budget)


def _install_ipi_probe(system):
    events = []
    system.cpu.flag_i = 1
    system.cpu.irq_ipi = True

    def record_masked_trap(vector):
        events.append(vector)
        # Match architectural trap entry: the I flag is masked while the
        # handler runs even though the mailbox remains pending until ACK.
        system.cpu.flag_i = 0

    system.cpu._trap = record_masked_trap
    return events


def test_sole_active_secondary_full_core_makes_batch_progress():
    """A full core remains runnable when core 0 is halted."""
    system = _new_system(full_cores=2)
    primary, secondary = system.cores
    primary.halted = True

    initial_cycles = secondary._cs.cycle_count
    system.run_batch(2_500)

    assert secondary._cs.cycle_count > initial_cycles


def test_disabled_cluster_holds_micro_cores_in_reset():
    """An explicit zero enable mask holds micro-cores in reset."""
    system = _new_system(full_cores=1, clusters=1)

    for offset in range(0x18, 0x20):
        system.sysinfo.write8(offset, 0)

    assert all(core.halted for core in system.clusters[0].cores)


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason=(
        "SystemInfo currently resets CLUSTER_EN to zero, while mp64_soc.v "
        "resets the 64-bit register to all ones"
    ),
)
def test_cluster_enable_reset_value_matches_rtl():
    """Reset enables every configured cluster through an all-ones mask."""
    system = _new_system(full_cores=1, clusters=3)

    assert system.sysinfo.cluster_en == 0xFFFF_FFFF_FFFF_FFFF


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason=(
        "enabling a cluster currently leaves its micro-cores halted instead "
        "of releasing the RTL reset gate"
    ),
)
def test_enabling_cluster_releases_micro_cores_from_reset():
    """An enabled cluster's cores must leave their reset-held state."""
    system = _new_system(full_cores=1, clusters=1)
    cluster = system.clusters[0]

    system.sysinfo.write8(0x18, 0x01)

    assert all(not core.halted for core in cluster.cores)


def test_manually_released_micro_core_can_step():
    """The Python stepping path can execute a manually released micro-core."""
    system = _new_system(full_cores=1, clusters=1)
    system.sysinfo.write8(0x18, 0x01)

    system.load_binary(0x100, _COUNTING_SPIN)
    system.cpu.halted = True
    micro = system.clusters[0].cores[0]
    micro.pc = 0x100
    micro.halted = False

    system.step()

    assert micro.regs[1] == 1


@pytest.mark.xfail(
    strict=True,
    raises=AttributeError,
    reason=(
        "run_batch_stats requires native structured execution results, while "
        "Python-only micro-core batching is deferred"
    ),
)
def test_run_batch_executes_an_active_micro_core():
    """An active advertised micro-core must participate in batched execution."""
    system = _new_system(full_cores=1, clusters=1)
    system.sysinfo.write8(0x18, 0x01)
    system.cpu.halted = True

    system.load_binary(0x100, _COUNTING_SPIN)
    micro = system.clusters[0].cores[0]
    micro.pc = 0x100
    micro.halted = False

    assert system.run_batch(8) == 8
    assert micro.regs[1] == 4


def test_step_delivers_a_pending_ipi_at_its_execution_boundary():
    """The per-round stepping path is the current IPI-boundary oracle."""
    system = _new_system()
    events = _install_ipi_probe(system)

    system.step()

    assert events == [IVEC_IPI]
    assert system.cpu.irq_ipi


def test_bus_requester_metadata_preserves_mailbox_payload_and_ack_contract():
    """Each byte transaction carries its requester without ambient bus state."""
    system = _new_system(full_cores=3)
    bus = system.bus

    assert not hasattr(bus, "requester_id")
    assert not hasattr(system.mailbox, "_requester_id")

    payload0 = 0x0102_0304_0506_0708
    payload1 = 0xA1A2_A3A4_A5A6_A7A8
    for sender, payload in ((0, payload0), (1, payload1)):
        for index in range(8):
            bus.write8(
                MBOX_BASE + index,
                (payload >> (8 * index)) & 0xFF,
                requester_id=sender,
            )
        bus.write8(MBOX_BASE + 0x08, 2, requester_id=sender)

    assert system.mailbox.data == [payload0, payload1, payload1]
    assert system.mailbox.pending == [0, 0, 0b011]
    assert system.cores[2].irq_ipi
    assert sum(
        bus.read8(MBOX_BASE + index, requester_id=2) << (8 * index)
        for index in range(8)
    ) == payload1

    bus.write8(MBOX_BASE + 0x0A, 0, requester_id=2)
    assert system.mailbox.pending[2] == 0b010
    assert system.cores[2].irq_ipi

    bus.write8(MBOX_BASE + 0x0A, 1, requester_id=2)
    assert system.mailbox.pending[2] == 0
    assert not system.cores[2].irq_ipi


def test_bus_requester_metadata_preserves_spinlock_ownership_contract():
    """Spinlock test-and-set and owner release use the request metadata."""
    system = _new_system(full_cores=2)
    bus = system.bus

    assert not hasattr(system.spinlock, "_requester_id")
    assert bus.read8(SPINLOCK_BASE, requester_id=1) == 0
    assert bus.read8(SPINLOCK_BASE, requester_id=1) == 0
    assert bus.read8(SPINLOCK_BASE, requester_id=0) == 1
    assert system.spinlock.owner[0] == 1

    bus.write8(SPINLOCK_BASE + 1, 0, requester_id=0)
    assert bus.read8(SPINLOCK_BASE, requester_id=0) == 1
    assert system.spinlock.owner[0] == 1

    bus.write8(SPINLOCK_BASE + 1, 0, requester_id=1)
    assert bus.read8(SPINLOCK_BASE, requester_id=0) == 0
    assert system.spinlock.owner[0] == 0


def test_native_ipi_router_does_not_replay_legacy_line_callbacks():
    """Routed pending/line transitions cannot be overwritten after unlocking."""
    system = _new_system(full_cores=2)
    target = system.cores[1]

    # These deliberately stale line mutations model the legacy callback path.
    # A router-attached mailbox must not invoke them after its atomic update.
    system.mailbox.on_ipi = lambda core_id: setattr(
        system.cores[core_id],
        "irq_ipi",
        False,
    )
    system.mailbox.on_ack = lambda core_id: setattr(
        system.cores[core_id],
        "irq_ipi",
        True,
    )

    assert system.mailbox.send_ipi(0, 1)
    assert system.mailbox.pending[1] == 1
    assert target.irq_ipi

    assert system.mailbox.acknowledge_ipi(1, 0)
    assert system.mailbox.pending[1] == 0
    assert not target.irq_ipi


def test_native_mailbox_csrs_share_pending_and_ipi_routing_state():
    """Native CSR send/read/ACK uses the same router as mailbox MMIO."""
    system = _new_system(full_cores=3)
    system.mailbox.data[1] = 0xA5
    system.mailbox.data[2] = 0x5A
    sender_code = assemble(
        f"""
        ldi64 r1, 2
        csrw {CSR_MBOX}, r1
        halt
        """
    )
    _run_only_secondary(system, sender_code)

    sender = system.cores[1]
    target = system.cores[2]
    assert system.mailbox.data == [0, 0xA5, 0x5A]
    assert system.mailbox.pending == [0, 0, 0b010]
    assert target.irq_ipi
    assert target._cs.irq_ipi
    assert sender._cs.ipi_pending_mask() == 0

    target_code = assemble(
        f"""
        csrr r4, {CSR_MBOX}
        ldi64 r1, 1
        csrw {CSR_IPIACK}, r1
        csrr r5, {CSR_MBOX}
        halt
        """
    )
    system.load_binary(0x200, target_code)
    for core in system.cores:
        core.halted = True
        core.idle = False
    target.pc = 0x200
    target.halted = False
    system.run_batch(100)

    assert target.regs[4:6] == [0b010, 0]
    assert system.mailbox.pending == [0, 0, 0]
    assert not target.irq_ipi
    assert not target._cs.irq_ipi

    target.irq_ipi = True
    assert target._cs.irq_ipi
    target._reset_state()
    assert not target.irq_ipi
    assert not target._cs.irq_ipi


def test_secondary_native_mailbox_mmio_carries_its_requester_identity():
    """A native callback preserves the issuing core on every mailbox byte."""
    system = _new_system(full_cores=2)
    mailbox_addr = MMIO_BASE + MBOX_BASE
    send_from_secondary = assemble(
        f"""
        ldi64 r1, {mailbox_addr}
        ldi64 r2, 0x5a
        st.b r1, r2
        ldi64 r1, {mailbox_addr + 0x08}
        ldi64 r2, 0
        st.b r1, r2
        halt
        """
    )

    _run_only_secondary(system, send_from_secondary)

    assert system.mailbox.data == [0x5A, 0x5A]
    assert system.mailbox.pending == [0b10, 0]
    assert system.cores[0].irq_ipi

    system.bus.write8(MBOX_BASE + 0x0A, 1, requester_id=0)
    assert system.mailbox.pending == [0, 0]
    assert not system.cores[0].irq_ipi


def test_native_csr_ipi_preserves_advertised_micro_core_reachability():
    """SystemState routing retains the emulator's all-core IPI reach."""
    system = _new_system(full_cores=2, clusters=1)
    micro = system.clusters[0].cores[0]
    send_to_micro = assemble(
        f"""
        ldi64 r1, {micro.core_id}
        csrw {CSR_MBOX}, r1
        halt
        """
    )

    _run_only_secondary(system, send_to_micro)

    assert micro.irq_ipi
    assert micro.csr_read(CSR_MBOX) == 0b10
    assert system.mailbox.pending[micro.core_id] == 0b10

    micro.csr_write(CSR_IPIACK, 1)
    assert not micro.irq_ipi
    assert micro.csr_read(CSR_MBOX) == 0

    micro.csr_write(CSR_MBOX, 0)
    assert system.cores[0].irq_ipi
    assert system.cores[0].csr_read(CSR_MBOX) == 1 << micro.core_id

    system.cores[0].csr_write(CSR_IPIACK, micro.core_id)
    assert not system.cores[0].irq_ipi
    assert system.cores[0].csr_read(CSR_MBOX) == 0


def test_core0_native_batch_delivers_a_pending_ipi_at_its_boundary():
    """A native batch boundary must not leave an enabled IPI unobserved."""
    system = _new_system()
    events = _install_ipi_probe(system)

    system.run_batch(10)

    assert events == [IVEC_IPI]


def test_multicore_batch_never_overshoots_its_instruction_budget():
    """The compatibility wrapper's aggregate instruction limit is a hard cap."""
    system = _new_system(full_cores=4)
    requested = 10_001

    executed = system.run_batch(requested)

    assert executed == requested


def test_repeated_small_batch_budgets_rotate_across_active_cores():
    """The hard aggregate cap must not turn into fixed-priority starvation."""
    system = _new_system(full_cores=2, code=_COUNTING_SPIN)

    assert system.run_batch(1) == 1
    assert [core.regs[1] for core in system.cores] == [1, 0]
    assert system.run_batch(1) == 1
    assert [core.regs[1] for core in system.cores] == [1, 1]
    assert system._scheduler_cursor == 0


def test_single_core_batch_ticks_devices_by_native_cycles():
    """Long-latency instructions and devices must share one time basis."""
    system = _new_system(code=_MULTICYCLE_SPIN)
    system.timer.control = 1

    stats = system.run_batch_stats(10)
    native_cycles = system.cpu._cs.cycle_count

    if native_cycles <= stats.instructions_executed:
        raise RuntimeError("multicycle timing workload did not exceed its step count")
    assert stats.instructions_executed == 10
    assert stats.system_cycles_advanced == native_cycles
    assert stats.per_core_instructions == (10,)
    assert stats.per_core_cycles == (native_cycles,)
    assert system.timer.counter == native_cycles
    assert system._native_system.system_cycles == native_cycles


def test_single_step_advances_shared_time_by_its_native_cycle_cost():
    """The one-instruction path uses the same cycle clock as batch execution."""
    system = _new_system(code=_MULTICYCLE_SPIN)
    system.timer.control = 1

    returned_cycles = system.step()

    assert returned_cycles == 4
    assert system.cpu.cycle_count == 4
    assert system.timer.counter == 4
    assert system._native_system.system_cycles == 4


def test_private_parallel_progress_advances_one_shared_device_clock():
    """Two independent 1000-cycle cores represent 1000 elapsed SoC cycles."""
    system = _new_system(full_cores=2, code=_NOP_SLED)
    system.timer.control = 1

    system.run_batch(2_000)
    core_cycles = [core._cs.cycle_count for core in system.cores]

    if not core_cycles or max(core_cycles) == 0:
        raise RuntimeError("private-progress workload executed no core cycles")
    assert system.timer.counter == max(core_cycles)
    assert system._native_system.system_cycles == max(core_cycles)


def test_secondary_native_rtc_access_uses_the_shared_core0_instance():
    """Secondary guest MMIO reaches the one shared native RTC directly."""
    system = _new_system(full_cores=2)
    system.rtc.ctrl = 0
    system.rtc.uptime_ms = 0x5A
    read_uptime = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + RTC_BASE}
        ld.b r4, r1
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    original_read = secondary._mmio_read8

    def counted_read(address):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(address)

    secondary._mmio_read8 = counted_read
    _run_only_secondary(system, read_uptime)

    assert secondary._cs.rtc_enabled()
    assert secondary.regs[4] == 0x5A
    assert callback_reads == 0


def test_secondary_native_framebuffer_reaches_shared_host_state():
    """Secondary guest MMIO and the host facade use one framebuffer."""
    system = _new_system(full_cores=2)
    system.fb.width = 0x0102
    system.fb.height = 0x00F1
    read_and_configure = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + FB_BASE + 0x08}
        ld.w r4, r1
        ldi64 r2, {MMIO_BASE + FB_BASE + 0x20}
        ldi64 r5, 0x03
        st.b r2, r5
        ldi64 r2, {MMIO_BASE + FB_BASE + 0x28}
        ldi64 r5, 0x03
        st.b r2, r5
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    callback_writes = 0
    original_read = secondary._mmio_read8
    original_write = secondary._mmio_write8

    def counted_read(address):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(address)

    def counted_write(address, value):
        nonlocal callback_writes
        callback_writes += 1
        return original_write(address, value)

    secondary._mmio_read8 = counted_read
    secondary._mmio_write8 = counted_write
    _run_only_secondary(system, read_and_configure)

    assert secondary.regs[4] == 0x0102
    assert (system.fb.width, system.fb.height) == (0x0102, 0x00F1)
    assert (system.fb.mode, system.fb.enable) == (0x03, 0x03)
    assert callback_reads == callback_writes == 0


def test_secondary_native_remaining_singletons_reach_shared_state():
    """UART, NIC, crypto, and TRNG dispatch through SystemState ownership."""
    system = _new_system(full_cores=2)
    exercise_singletons = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + UART_BASE}
        ldi64 r2, 0x5a
        st.b r1, r2
        ldi64 r1, {MMIO_BASE + NIC_BASE + 0x02}
        ldi64 r2, 0xa5
        st.b r1, r2
        ldi64 r1, {MMIO_BASE + SHA3_BASE + 0x02}
        ldi64 r2, 0x03
        st.b r1, r2
        ldi64 r1, {MMIO_BASE + TRNG_BASE + 0x10}
        ld.b r4, r1
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    callback_writes = 0
    original_read = secondary._mmio_read8
    original_write = secondary._mmio_write8

    def counted_read(address):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(address)

    def counted_write(address, value):
        nonlocal callback_writes
        callback_writes += 1
        return original_write(address, value)

    secondary._mmio_read8 = counted_read
    secondary._mmio_write8 = counted_write
    _run_only_secondary(system, exercise_singletons)

    primary = system.cores[0]._cs
    assert system._tx_log == [0x5A]
    assert primary.nic_read8(NIC_BASE + 0x02) == 0xA5
    assert primary.crypto_read8(SHA3_BASE + 0x02) == 3
    assert secondary.regs[4] == 1
    assert callback_reads == callback_writes == 0


def test_secondary_native_uart_geometry_reaches_shared_host_state():
    """Secondary guest MMIO and host resize helpers use one geometry block."""
    system = _new_system(full_cores=2)
    system.uart_geom.host_set_size(0x015A, 0x012B)
    read_and_request_resize = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + UART_GEOM_BASE}
        ld.b r4, r1
        ld.h r6, r1
        ld.w r7, r1
        ld.d r8, r1
        ldi64 r2, {MMIO_BASE + UART_GEOM_BASE + 0x06}
        ldi64 r5, 0xBEEF
        st.h r2, r5
        ld.h r9, r2
        ldi64 r5, 0x00AB1234
        st.w r2, r5
        ld.w r10, r2
        ldi64 r2, {MMIO_BASE + UART_GEOM_BASE + 0x08}
        ldi64 r5, 0x0156
        str r2, r5
        ld.h r11, r2
        ldi64 r2, {MMIO_BASE + UART_GEOM_BASE + 0x05}
        ldi64 r5, 0x02
        st.b r2, r5
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    callback_writes = 0
    original_read = secondary._mmio_read8
    original_write = secondary._mmio_write8

    def counted_read(address):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(address)

    def counted_write(address, value):
        nonlocal callback_writes
        callback_writes += 1
        return original_write(address, value)

    secondary._mmio_read8 = counted_read
    secondary._mmio_write8 = counted_write
    _run_only_secondary(system, read_and_request_resize)

    assert secondary.regs[4] == 0x5A
    assert secondary.regs[6] == 0x015A
    assert secondary.regs[7] == 0x012B015A
    assert secondary.regs[8] == 0x00000001012B015A
    assert secondary.regs[9] == 0xBEEF
    assert secondary.regs[10] == 0x00AB1234
    assert secondary.regs[11] == 0x0156
    assert system.uart_geom.req_cols == 0x1234
    assert system.uart_geom.req_rows == 0x0156
    assert system.uart_geom.has_resize_request()
    assert callback_reads == callback_writes == 0


def test_uart_geometry_access_crossing_its_boundary_falls_back_bytewise():
    """A partial geometry span must not be captured as native wide MMIO."""
    system = _new_system(full_cores=2)
    cross_boundary_access = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + UART_GEOM_BASE + 0x0F}
        ld.h r4, r1
        ldi64 r5, 0x1234
        st.h r1, r5
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    callback_writes = 0

    def counted_read(_address):
        nonlocal callback_reads
        callback_reads += 1
        return 0xA5

    def counted_write(_address, _value):
        nonlocal callback_writes
        callback_writes += 1

    secondary._mmio_read8 = counted_read
    secondary._mmio_write8 = counted_write
    _run_only_secondary(system, cross_boundary_access)

    assert secondary.regs[4] == 0xA5A5
    assert callback_reads == callback_writes == 2


@pytest.mark.parametrize("execution_mode", ("step", "batch"))
def test_uart_geometry_host_updates_progress_during_native_execution(
    execution_mode,
):
    """Host geometry access and native execution must both finish."""
    system = _new_system(full_cores=1, code=_UART_GEOMETRY_SPIN)
    assert not hasattr(system.cpu._cs, "_uart_geom_use")
    start = threading.Event()
    first_update = threading.Event()
    stop = threading.Event()
    failures = []
    host_update_counts = []
    original_read = system.cpu._mmio_read8

    def signal_execution_entry(address):
        result = original_read(address)
        if address == MMIO_BASE + SYSINFO_BASE:
            start.set()
            if not first_update.wait(timeout=2):
                raise RuntimeError(
                    "host geometry update did not enter active execution")
        return result

    system.cpu._mmio_read8 = signal_execution_entry

    def update_host_geometry():
        start.wait()
        count = 0
        try:
            while not stop.is_set():
                system.uart_geom.host_set_size(
                    80 + (count % 40),
                    24 + (count % 20),
                )
                count += 1
                first_update.set()
        except BaseException as exc:
            failures.append(exc)
        finally:
            host_update_counts.append(count)

    worker = threading.Thread(
        target=update_host_geometry,
        daemon=True,
        name=f"uart-geometry-{execution_mode}",
    )
    worker.start()
    start.set()
    try:
        if execution_mode == "step":
            # The trigger callback proves one host access while step_one is
            # active; repeated calls then pin progress at GIL boundaries.
            for _ in range(3_000):
                system.step()
        else:
            # The trigger callback waits for the first host update from inside
            # run_steps; the remaining native geometry loop then overlaps it.
            system.run_batch(10_000)
    finally:
        stop.set()
        start.set()
        first_update.set()
    worker.join(timeout=5)

    assert not worker.is_alive()
    assert failures == []
    assert len(host_update_counts) == 1
    assert host_update_counts[0] > 0
    assert system.cpu._cs.cycle_count > 0


def test_micro_core_uart_geometry_fallback_reaches_the_shared_instance():
    """Python-only micro-core MMIO remains attached to the same geometry."""
    system = _new_system(full_cores=2, clusters=1)
    geometry_addr = MMIO_BASE + UART_GEOM_BASE
    micro = system.clusters[0].cores[0]
    system.uart_geom.host_set_size(77, 33)

    assert micro.mem_read8(geometry_addr) == 77
    assert micro.mem_read8(geometry_addr + 0x02) == 33

    micro.mem_write8(geometry_addr + 0x06, 88)
    micro.mem_write8(geometry_addr + 0x08, 44)
    micro.mem_write8(geometry_addr + 0x05, 0x02)

    assert (system.uart_geom.req_cols, system.uart_geom.req_rows) == (88, 44)
    assert system.uart_geom.has_resize_request()


def test_core0_timer_proxy_advances_when_the_bus_ticks():
    """The device-bus facade delegates to the authoritative native clock."""
    system = _new_system(full_cores=2)
    system.timer.control = 1

    system.bus.tick(17)

    assert system.timer.counter == 17
    assert system._native_system.system_cycles == 17


def test_scheduler_and_direct_bus_ticks_share_the_native_clock():
    """Scheduled and explicit device time use one authoritative clock."""
    system = _new_system(full_cores=1)
    system.timer.control = 1

    system.run_batch(10)
    scheduled_cycles = system._native_system.system_cycles
    assert scheduled_cycles == system.timer.counter

    system.bus.tick(17)

    assert system.timer.counter == scheduled_cycles + 17
    assert system._native_system.system_cycles == scheduled_cycles + 17
    assert system._native_system.event_horizon() == (
        scheduled_cycles + 17,
        None,
        0,
    )


def test_batch_rejects_an_active_horizon_before_guest_state_changes():
    """Post-hoc core stats cannot safely execute against an active deadline."""
    system = _new_system(full_cores=1)
    system._native_system.set_event_deadline(
        system._native_system.EVENT_EXTERNAL,
        5,
    )
    pc_before = system.cpu.pc
    cycles_before = system.cpu.cycle_count

    with pytest.raises(RuntimeError, match="cycle-bounded native execution"):
        system.run_batch(10)

    assert system.cpu.pc == pc_before
    assert system.cpu.cycle_count == cycles_before
    assert system._native_system.event_horizon() == (
        0,
        5,
        1 << system._native_system.EVENT_EXTERNAL,
    )


def test_secondary_native_timer_observes_shared_ticking_state():
    """Every requester must read the same architecturally singleton timer."""
    system = _new_system(full_cores=2)
    system.timer.control = 1
    system.bus.tick(17)
    system.timer.control = 0
    system.timer.compare = 0xFFFF_FFFF

    read_and_write_timer = assemble(
        f"""
        ldi64 r1, {MMIO_BASE + TIMER_BASE}
        ld.b r4, r1
        ldi64 r2, {MMIO_BASE + TIMER_BASE + 0x04}
        ldi64 r5, 0x34
        st.b r2, r5
        halt
        """
    )
    secondary = system.cores[1]
    callback_reads = 0
    callback_writes = 0
    original_read = secondary._mmio_read8
    original_write = secondary._mmio_write8

    def counted_read(address):
        nonlocal callback_reads
        callback_reads += 1
        return original_read(address)

    def counted_write(address, value):
        nonlocal callback_writes
        callback_writes += 1
        return original_write(address, value)

    secondary._mmio_read8 = counted_read
    secondary._mmio_write8 = counted_write
    _run_only_secondary(system, read_and_write_timer)

    assert secondary.regs[4] == 17
    assert system.timer.compare == 0xFFFF_FF34
    assert callback_reads == callback_writes == 0
