"""Phase 0 scheduler and shared-state characterization tests.

The passing tests below pin useful current behavior and provide controls for
the strict xfails.  Each strict xfail describes a known concurrency defect
that later scheduler work is expected to fix; keeping those assertions red
avoids turning accidental chunking or duplicated-device behavior into a
permanent compatibility contract.
"""

import threading

import pytest

from asm import assemble
from devices import (
    MMIO_BASE,
    RTC_BASE,
    SYSINFO_BASE,
    TIMER_BASE,
    UART_GEOM_BASE,
)
from megapad64 import IVEC_IPI
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
    ldi64 r3, {MMIO_BASE + SYSINFO_BASE}
    ld.b r0, r3
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
        "run_batch assumes every active core has native run_steps, but "
        "Megapad64Micro is still a Python-only core"
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


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="the core-0 one-call native fast path omits post-batch IPI delivery",
)
def test_core0_native_batch_delivers_a_pending_ipi_at_its_boundary():
    """A native batch boundary must not leave an enabled IPI unobserved."""
    system = _new_system()
    events = _install_ipi_probe(system)

    system.run_batch(10)

    assert events == [IVEC_IPI]


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason=(
        "four active cores currently execute a final 4x1000-instruction "
        "round, returning 12000 for a 10001-instruction aggregate budget"
    ),
)
def test_multicore_batch_never_overshoots_its_instruction_budget():
    """The compatibility wrapper's aggregate instruction limit is a hard cap."""
    system = _new_system(full_cores=4)
    requested = 10_001

    executed = system.run_batch(requested)

    assert executed <= requested


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="run_batch discards native total_cycles and ticks devices by steps",
)
def test_single_core_batch_ticks_devices_by_native_cycles():
    """Long-latency instructions and devices must share one time basis."""
    system = _new_system(code=_MULTICYCLE_SPIN)
    system.timer.control = 1

    executed = system.run_batch(10)
    native_cycles = system.cpu._cs.cycle_count

    if native_cycles <= executed:
        raise RuntimeError("multicycle timing workload did not exceed its step count")
    assert system.timer.counter == native_cycles


@pytest.mark.xfail(
    strict=True,
    raises=AssertionError,
    reason="device time currently advances by the sum of all core steps",
)
def test_private_parallel_progress_advances_one_shared_device_clock():
    """Two independent 1000-cycle cores represent 1000 elapsed SoC cycles."""
    system = _new_system(full_cores=2, code=_NOP_SLED)
    system.timer.control = 1

    system.run_batch(2_000)
    core_cycles = [core._cs.cycle_count for core in system.cores]

    if not core_cycles or max(core_cycles) == 0:
        raise RuntimeError("private-progress workload executed no core cycles")
    assert system.timer.counter == max(core_cycles)


def test_secondary_native_rtc_access_uses_the_shared_core0_instance():
    """The deliberately disabled secondary RTC provides a singleton control."""
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

    _run_only_secondary(system, read_uptime)

    assert system.cores[1].regs[4] == 0x5A


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
    """The core-0 timer proxy is the current device-tick control."""
    system = _new_system(full_cores=2)
    system.timer.control = 1

    system.bus.tick(17)

    assert system.timer.counter == 17


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
