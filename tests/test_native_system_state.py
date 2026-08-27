"""Ownership and lifetime contracts for the first native SystemState slice."""

from __future__ import annotations

import array
import gc
import weakref

import pytest

import _mp64_accel
from accel_wrapper import Megapad64, NativeSystemState, TrapError
from asm import assemble
from devices import (
    AES_BASE,
    FB_BASE,
    MMIO_BASE,
    NIC_BASE,
    RTC,
    RTC_BASE,
    SHA3_BASE,
    TIMER_BASE,
    TRNG_BASE,
    UART_BASE,
    UART_GEOM_BASE,
    WOTS_BASE,
)
from nic_backends import LoopbackBackend
from megapad64 import (
    CSR_TACC_CTL,
    CSR_TACC_STATUS,
    IVEC_IPI,
    IVEC_PRIV_FAULT,
    TACC_OWNER_NONE,
)
from system import MegapadSystem


REGIONS = (
    ("main", "attach_mem", None, "mem_size"),
    ("hbw", "attach_hbw_mem", 0x1000, "hbw_size"),
    ("ext_mem", "attach_ext_mem", 0x2000, "ext_mem_size"),
    ("vram", "attach_vram", 0x3000, "vram_size"),
)

EVENT_SOURCE_TIMER = 0
EVENT_SOURCE_FRAMEBUFFER = 1
EVENT_SOURCE_RTC = 2
EVENT_SOURCE_INTERRUPT = 3
EVENT_SOURCE_EXTERNAL = 4


class _CountingExporter:
    def __init__(self, size: int, on_acquire=None):
        self.storage = bytearray(size)
        self.acquisitions = 0
        self.releases = 0
        self.on_acquire = on_acquire

    def __buffer__(self, _flags):
        self.acquisitions += 1
        if self.on_acquire is not None:
            self.on_acquire()
        return memoryview(self.storage)

    def __release_buffer__(self, _view):
        self.releases += 1


def _attach_system_region(
    owner,
    attach_name: str,
    base: int | None,
    buffer,
    size: int,
) -> None:
    attach = getattr(owner, attach_name)
    if base is None:
        attach(buffer, size)
    else:
        attach(buffer, base, size)


def _native_step(state, *, on_output=lambda _port, _value: None) -> int:
    return _mp64_accel.step_one(
        state,
        mmio_read8=lambda _addr: 0,
        mmio_write8=lambda _addr, _value: None,
        on_output=on_output,
        csr_read_override=None,
        mmio_start=0xFFFF_FF00_0000_0000,
        mmio_end=0xFFFF_FF80_0000_0000,
    )


def _set_pc(state, value: int) -> None:
    state.psel = 3
    state.xsel = 2
    state.set_reg(3, value)


def _valid_tacc_state(
    owner: int,
    *,
    epoch: int = 7,
    busy: bool = True,
    force_pending: bool = True,
) -> dict:
    return {
        "tacc": bytes((index * 13 + 5) & 0xFF for index in range(256)),
        "tacc_owner": owner,
        "tacc_valid": True,
        "tacc_dirty": True,
        "tacc_format_ew": 1,
        "tacc_format_signed": 1,
        "tacc_busy": busy,
        "tacc_force_pending": force_pending,
        "tacc_epoch": epoch,
    }


def test_native_full_tacc_restore_is_exact_validated_and_atomic() -> None:
    core = _mp64_accel.CPUState()
    core.core_id = 3
    state = _valid_tacc_state(3)

    core.tacc_restore(state)

    assert dict(core.tacc_snapshot()) == state
    state["tacc"] = bytes(256)
    assert dict(core.tacc_snapshot())["tacc"] != bytes(256)
    baseline = dict(core.tacc_snapshot())

    transient_try = {
        "tacc": bytes(256),
        "tacc_owner": TACC_OWNER_NONE,
        "tacc_valid": False,
        "tacc_dirty": False,
        "tacc_format_ew": 0,
        "tacc_format_signed": 0,
        "tacc_busy": True,
        "tacc_force_pending": True,
        "tacc_epoch": 8,
    }
    core.tacc_restore(transient_try)
    assert dict(core.tacc_snapshot()) == transient_try
    core.tacc_restore(baseline)

    malformed_states = []

    extra = dict(baseline)
    extra["unexpected"] = 0
    malformed_states.append(extra)

    wrong_owner = dict(baseline)
    wrong_owner["tacc_owner"] = 2
    malformed_states.append(wrong_owner)

    idle_pending = dict(baseline)
    idle_pending["tacc_busy"] = False
    malformed_states.append(idle_pending)

    unowned_dirty = dict(baseline)
    unowned_dirty.update({
        "tacc": bytes(256),
        "tacc_owner": TACC_OWNER_NONE,
        "tacc_valid": False,
        "tacc_dirty": True,
        "tacc_format_ew": 0,
        "tacc_format_signed": 0,
        "tacc_busy": True,
        "tacc_force_pending": False,
    })
    malformed_states.append(unowned_dirty)

    invalid_image = dict(baseline)
    invalid_image.update({
        "tacc_owner": 3,
        "tacc_valid": False,
        "tacc_dirty": False,
        "tacc_format_ew": 0,
        "tacc_format_signed": 0,
        "tacc_busy": False,
        "tacc_force_pending": False,
    })
    malformed_states.append(invalid_image)

    for malformed in malformed_states:
        with pytest.raises(ValueError):
            core.tacc_restore(malformed)
        assert dict(core.tacc_snapshot()) == baseline


@pytest.mark.parametrize(
    ("full_cores", "all_cores", "cluster_index", "owner_id"),
    [
        pytest.param(4, 16, 0, 4, id="production-cluster-0"),
        pytest.param(4, 16, 1, 8, id="production-cluster-1"),
        pytest.param(4, 16, 2, 12, id="production-cluster-2"),
        pytest.param(1, 5, 0, 1, id="compact-cluster"),
    ],
)
def test_native_cluster_tacc_restore_uses_absolute_owner_domains(
    full_cores: int,
    all_cores: int,
    cluster_index: int,
    owner_id: int,
) -> None:
    system = NativeSystemState(full_cores, all_cores)
    state = dict(system._cluster_tile_snapshot(cluster_index))
    state.update(_valid_tacc_state(owner_id, epoch=23))
    state["acc"] = [11, 22, 33, 44]
    state["sha_mode"] = 3
    state["sha_msglen_lo"] = 0x1234
    state["sha_msglen_hi"] = 0x5678

    system._cluster_tile_update(cluster_index, state)

    assert dict(system._cluster_tile_snapshot(cluster_index)) == state
    state["acc"][0] = 0xFFFF
    assert dict(
        system._cluster_tile_snapshot(cluster_index)
    )["acc"][0] == 11
    baseline = dict(system._cluster_tile_snapshot(cluster_index))

    wrong_owner = dict(baseline)
    wrong_owner["tacc_owner"] = owner_id + 4
    with pytest.raises(ValueError, match="absolute core-ID domain"):
        system._cluster_tile_update(cluster_index, wrong_owner)
    assert dict(system._cluster_tile_snapshot(cluster_index)) == baseline

    late_invalid_field = dict(baseline)
    late_invalid_field["acc"] = [99, 98, 97, 96]
    late_invalid_field["sha_mode"] = 4
    with pytest.raises(ValueError, match="SHA mode"):
        system._cluster_tile_update(cluster_index, late_invalid_field)
    assert dict(system._cluster_tile_snapshot(cluster_index)) == baseline

    extra = dict(baseline)
    extra["unexpected"] = 0
    with pytest.raises(ValueError, match="exactly 13 fields"):
        system._cluster_tile_update(cluster_index, extra)
    assert dict(system._cluster_tile_snapshot(cluster_index)) == baseline


def test_native_guest_tacc_csrs_are_not_silent_placeholders() -> None:
    cpu = Megapad64(mem_size=4096, core_id=2)
    cpu._cs.tacc_restore(
        _valid_tacc_state(
            2,
            epoch=41,
            busy=True,
            force_pending=False,
        )
    )
    cpu.regs[1] = 1
    cpu.load_bytes(
        0,
        assemble(
            f"csrr r0, {CSR_TACC_STATUS}\n"
            f"csrw {CSR_TACC_CTL}, r1"
        ),
    )
    cpu.pc = 0
    cpu._step_python_fallback = lambda: pytest.fail(
        "native TACC CSR entered Python fallback"
    )

    assert cpu.step() == 1
    assert cpu.regs[0] == (
        0x1 |
        0x2 |
        0x4 |
        0x8 |
        0x10 |
        (1 << 5) |
        (1 << 8) |
        (2 << 16)
    )
    assert cpu.step() == 1
    assert cpu.tacc_force_pending
    assert cpu.tacc_epoch == 41

    cpu._cs.tacc_restore(
        _valid_tacc_state(
            2,
            epoch=41,
            busy=False,
            force_pending=False,
        )
    )
    cpu.pc = 2
    assert cpu.step() == 1
    assert cpu.tacc_owner == TACC_OWNER_NONE
    assert bytes(cpu.tacc) == bytes(256)
    assert cpu.tacc_epoch == 42

    cpu._cs.tacc_restore(
        _valid_tacc_state(
            2,
            epoch=50,
            busy=False,
            force_pending=False,
        )
    )
    cpu.priv_level = 1
    cpu.pc = 2
    with pytest.raises(TrapError) as raised:
        cpu.step()
    assert raised.value.ivec_id == IVEC_PRIV_FAULT
    assert dict(cpu._cs.tacc_snapshot()) == _valid_tacc_state(
        2,
        epoch=50,
        busy=False,
        force_pending=False,
    )


def test_cluster_tacc_ownership_survives_interrupt_and_preemption() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    cluster = system.clusters[0]
    cluster.set_enabled(True)
    owner, sibling = cluster.cores[:2]
    for instruction in ("t.acc.try", "t.acc.clear"):
        system.load_binary(0, assemble(instruction))
        owner.pc = 0
        owner.halted = False
        owner.step()
    owned = dict(cluster._shared_engine_snapshot())

    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False
    handler = 0x600
    handler_encoding = assemble("nop")
    system.load_binary(handler, handler_encoding)
    owner.ivt_base = 0x400
    owner.sp = 0xF00
    owner.mem[
        owner.ivt_base + IVEC_IPI * 8:
        owner.ivt_base + IVEC_IPI * 8 + 8
    ] = handler.to_bytes(8, "little")
    owner.pc = 0x100
    owner.flag_i = True
    owner.halted = False
    assert system.cores[0]._cs.ipi_send(owner.core_id)

    interrupted = system.run_batch_stats(1)

    # The unbounded scheduler accepts the interrupt before the next fetch,
    # then may spend the still-available instruction budget in the handler.
    assert interrupted.instructions_executed == 1
    assert interrupted.per_core_instructions[owner.core_id] == 1
    assert owner.ivec_id == IVEC_IPI
    assert owner.pc == handler + len(handler_encoding)
    assert dict(cluster._shared_engine_snapshot()) == owned

    owner.halted = True
    system.load_binary(0x180, assemble("nop"))
    sibling.pc = 0x180
    sibling.halted = False

    preempted = system.run_batch_stats(1)

    assert preempted.per_core_instructions[sibling.core_id] == 1
    assert dict(cluster._shared_engine_snapshot()) == owned
    status = owner.csr_read(CSR_TACC_STATUS)
    assert status & 0b11 == 0b11
    assert (status >> 16) & 0x1F == owner.core_id


def test_micro_reset_cancels_only_its_cluster_caller_epoch() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=2,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )
    for cluster in system.clusters:
        cluster.set_enabled(True)
    system.load_binary(0, assemble("t.acc.try\nt.acc.clear"))
    full_owner = system.cores[0]
    cluster_owners = (
        system.clusters[0].cores[0],
        system.clusters[1].cores[0],
    )
    for owner in (full_owner,) + cluster_owners:
        owner.pc = 0
        owner.halted = False
        owner.step()
        owner.step()

    full_domains = tuple(
        dict(cpu._cs.tacc_snapshot())
        for cpu in system.cores[:system.num_full_cores]
    )
    cluster_domains = tuple(
        dict(cluster._shared_engine_snapshot())
        for cluster in system.clusters
    )
    caller_epochs = tuple(
        tuple(
            system._native_system
            ._cluster_tacc_caller_epochs_snapshot(index)
        )
        for index in range(2)
    )
    stage = dict(system._native_system._tacc_image_stage_snapshot())
    cancelled = system.clusters[0].cores[2]

    cancelled._reset_state()

    expected_first_epochs = list(caller_epochs[0])
    expected_first_epochs[2] += 1
    assert tuple(
        system._native_system._cluster_tacc_caller_epochs_snapshot(0)
    ) == tuple(expected_first_epochs)
    assert tuple(
        system._native_system._cluster_tacc_caller_epochs_snapshot(1)
    ) == caller_epochs[1]
    assert tuple(
        dict(cpu._cs.tacc_snapshot())
        for cpu in system.cores[:system.num_full_cores]
    ) == full_domains
    assert tuple(
        dict(cluster._shared_engine_snapshot())
        for cluster in system.clusters
    ) == cluster_domains
    assert dict(system._native_system._tacc_image_stage_snapshot()) == stage


def test_native_system_state_validates_topology_and_core_bounds() -> None:
    owner = NativeSystemState(2, 6)

    assert owner.full_core_count == 2
    assert owner.micro_core_count == 4
    assert owner.all_core_count == 6
    assert (owner.core(0).core_id, owner.core(1).core_id) == (0, 1)
    assert owner.core(0).num_cores == owner.core(1).num_cores == 6
    assert not owner.core(0).is_micro_core
    assert tuple(owner.micro_core(i).core_id for i in range(4)) == (
        2,
        3,
        4,
        5,
    )
    assert all(owner.micro_core(i).is_micro_core for i in range(4))
    assert all(owner.micro_core(i).num_cores == 6 for i in range(4))
    assert tuple(
        owner.main_bus_port_for_requester(core_id)
        for core_id in range(2, 6)
    ) == (2, 2, 2, 2)

    with pytest.raises(IndexError):
        owner.core(-1)
    with pytest.raises(IndexError):
        owner.core(2)
    with pytest.raises(IndexError):
        owner.micro_core(-1)
    with pytest.raises(IndexError):
        owner.micro_core(4)
    with pytest.raises(ValueError):
        NativeSystemState(0)
    with pytest.raises(ValueError):
        NativeSystemState(256)
    with pytest.raises(ValueError):
        NativeSystemState(2, 1)


def test_native_system_state_owns_stable_isolated_core_objects() -> None:
    owner = NativeSystemState(2, 6)
    core0 = owner.core(0)
    core1 = owner.core(1)
    micro0 = owner.micro_core(0)
    micro3 = owner.micro_core(3)

    core0.set_reg(7, 0x1111)
    core1.set_reg(7, 0x2222)
    micro0.set_reg(7, 0x3333)
    micro3.set_reg(7, 0x4444)

    assert owner.core(0) is core0
    assert owner.core(1) is core1
    assert owner.micro_core(0) is micro0
    assert owner.micro_core(3) is micro3
    assert core0.get_reg(7) == 0x1111
    assert core1.get_reg(7) == 0x2222
    assert micro0.get_reg(7) == 0x3333
    assert micro3.get_reg(7) == 0x4444


def test_native_system_run_loop_owns_budget_cursor_and_exact_results() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(
        0,
        assemble(
            """
loop:
    inc r1
    br loop
"""
        ),
    )
    system.boot(entry=0)

    native_runs_before = system._native_system.native_batch_runs

    stats = system.run_batch_stats(2_001)

    assert stats.native_scheduler
    assert system._native_system.native_batch_runs == native_runs_before + 1
    assert stats.instructions_executed == 2_001
    assert stats.per_core_instructions == (1_001, 1_000)
    assert stats.per_core_cycles == (1_501, 1_500)
    assert stats.system_cycles_advanced == 1_501
    assert stats.per_core_dispatches == (2, 1)
    assert stats.per_core_stop_reasons == (
        (2, 0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 0, 0),
    )
    assert system._scheduler_cursor == 1

    followup = system.run_batch_stats(1)

    assert followup.native_scheduler
    assert followup.per_core_instructions == (0, 1)
    assert system._scheduler_cursor == 0

    system.boot(entry=0)
    assert system._scheduler_cursor == 0


def test_native_system_loop_resumes_around_python_fallback() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("nop\nt.sum\nnop"))
    system.cpu.mem[0x100:0x140] = bytes(range(64))
    system.cpu.tsrc0 = 0x100
    system.cpu.pc = 0
    native_runs_before = system._native_system.native_batch_runs

    stats = system.run_batch_stats(3)

    assert stats.native_scheduler
    assert system._native_system.native_batch_runs == native_runs_before + 1
    assert stats.instructions_executed == 3
    assert stats.system_cycles_advanced == 3
    assert stats.per_core_instructions == (3,)
    assert stats.per_core_cycles == (3,)
    assert stats.per_core_dispatches == (2,)
    assert stats.per_core_stop_reasons == (
        (1, 0, 0, 1, 0, 0, 0),
    )
    assert stats.native_continuations == 1


def test_native_system_batch_rejects_uart_observer_reentry() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(
        0,
        assemble(
            f"""
    ldi64 r1, {MMIO_BASE + UART_BASE}
    ldi64 r2, 0x41
    st.b r1, r2
    idl
"""
        ),
    )
    system.cpu.pc = 0
    rejections = []

    def reject_reentrant_execution(_value):
        attempts = (
            ("batch", lambda: system.run_batch_stats(1)),
            ("step", system.step),
            ("direct core", lambda: system.cpu.run_steps_stats(1)),
            ("direct idle step", system.cpu.step),
        )
        for label, attempt in attempts:
            with pytest.raises(
                RuntimeError,
                match="native system batch is already active",
            ):
                attempt()
            rejections.append(label)

    system.uart.on_tx = reject_reentrant_execution
    native_runs_before = system._native_system.native_batch_runs

    stats = system.run_batch_stats(4)

    assert rejections == [
        "batch",
        "step",
        "direct core",
        "direct idle step",
    ]
    assert stats.native_scheduler
    assert stats.instructions_executed == 4
    assert stats.per_core_instructions == (4,)
    assert stats.system_cycles_advanced == stats.per_core_cycles[0]
    assert system.cpu.cycle_count == stats.per_core_cycles[0]
    assert system._native_system.native_batch_runs == native_runs_before + 1


def test_native_system_batch_publishes_one_uart_batch_at_batch_end() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(
        0,
        assemble(
            f"""
    ldi64 r1, {MMIO_BASE + UART_BASE}
    ldi r2, 0x41
    st.b r1, r2
    ldi r2, 0x42
    st.b r1, r2
    idl
"""
        ),
    )
    system.cpu.pc = 0
    published = []
    system.uart.on_tx_batch = published.append
    owner = system._native_system
    owner._start_concurrency_profile()

    stats = system.run_batch_stats(6)
    counts = dict(owner._stop_concurrency_profile()["counts"])

    assert stats.instructions_executed == 6
    assert published == [b"AB"]
    assert bytes(system.uart.tx_buffer) == b"AB"
    assert counts["settle_round_calls"] == 2
    assert counts["settle_round_native_calls"] == 1
    assert counts["settle_round_python_calls"] == 1


def test_native_system_batch_rejects_mid_dispatch_deadline_mutation() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("out1\nnop"))
    system.cpu.regs[system.cpu.xsel] = 0x80
    system.cpu.mem[0x80] = 0xA5
    system.cpu.pc = 0
    rejections = []

    def reject_deadline_mutation(_port, _value):
        with pytest.raises(
            RuntimeError,
            match="event deadlines cannot change during an active",
        ):
            system._native_system.set_event_deadline(
                EVENT_SOURCE_EXTERNAL,
                1,
            )
        rejections.append(True)

    system.cpu.on_output = reject_deadline_mutation

    stats = system.run_batch_stats(2)

    assert rejections == [True]
    assert stats.native_scheduler
    assert stats.instructions_executed == 2
    assert system._native_system.event_horizon() == (2, None, 0)


def test_native_system_batch_rejects_nonterminal_zero_step_settlement() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("t.sum"))
    system.cpu.mem[0x100:0x140] = bytes(range(64))
    system.cpu.tsrc0 = 0x100
    system.cpu.pc = 0
    original_settlement = system._settle_native_core_continuation
    system._settle_native_core_continuation = (
        lambda *_args: (0, 0, False)
    )

    with pytest.raises(
        RuntimeError,
        match="nonterminal native continuation made no progress",
    ):
        system.run_batch_stats(1)

    assert system.cpu.pc == 0
    assert system._native_system.system_cycles == 0

    system._settle_native_core_continuation = original_settlement
    stats = system.run_batch_stats(1)
    assert stats.native_scheduler
    assert stats.instructions_executed == 1


def test_native_system_batch_rejects_cycle_overflow_after_settled_prefix(
) -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, assemble("t.sum"))
    system.cpu.pc = 0
    original_settlement = system._settle_native_core_continuation
    system._settle_native_core_continuation = (
        lambda *_args: (1, (1 << 63) - 1, False)
    )

    with pytest.raises(
        OverflowError,
        match="native scheduler per-core cycle accounting overflow",
    ):
        system.run_batch_stats(2)

    assert system.cpu.pc == 0
    # The first continuation is a completed coordinator prefix.  Current
    # scheduler exception semantics retain and clock that prefix before the
    # second continuation's cumulative accounting overflow is rethrown.
    assert system._native_system.system_cycles == (1 << 63) - 1

    system._settle_native_core_continuation = original_settlement
    stats = system.run_batch_stats(1)
    assert stats.native_scheduler
    assert stats.instructions_executed == 1


def test_native_system_clock_advances_all_shared_cycle_devices_once() -> None:
    owner = NativeSystemState(2)
    core0 = owner.core(0)
    core1 = owner.core(1)
    delta = RTC.MS_DIVISOR

    core0.timer_init()
    core0.timer_control = 1
    core0.timer_counter = 9
    core0.fb_init()
    core0.fb_enable = 1
    # One bulk tick accounts for every completed virtual frame.
    core0.fb_cycles_per_frame = delta // 2
    core0.rtc_init(False, 1_000, 1, 2, 3, 4, 5, 2026, 6)
    core0.rtc_uptime_ms = 41

    assert owner.system_cycles == 0

    owner.advance_system_cycles(delta)

    assert owner.system_cycles == delta
    assert core0.timer_counter == core1.timer_counter == 9 + delta
    assert core0.fb_vsync_count == core1.fb_vsync_count == 2
    assert core0.fb_vblank and core1.fb_vblank
    assert core0.rtc_uptime_ms == core1.rtc_uptime_ms == 42
    assert core0.rtc_epoch_ms == core1.rtc_epoch_ms == 1_001


def test_native_system_clock_rejects_invalid_moves_atomically() -> None:
    owner = NativeSystemState(1)
    core = owner.core(0)
    core.timer_init()
    core.timer_control = 1
    core.fb_init()
    core.fb_enable = 1
    core.fb_cycles_per_frame = 100
    core.rtc_init(False, 1_000, 1, 2, 3, 4, 5, 2026, 6)
    owner.advance_system_cycles(7)
    owner.advance_system_to(9)

    assert owner.system_cycles == 9
    assert core.timer_counter == 9

    def state():
        return (
            owner.system_cycles,
            core.timer_counter,
            core.fb_snapshot(),
            core.rtc_snapshot(),
            owner.event_horizon(),
        )

    before = state()
    owner.advance_system_to(owner.system_cycles)
    assert state() == before

    invalid_advances = (
        lambda: owner.advance_system_to(owner.system_cycles - 1),
    )
    for advance in invalid_advances:
        with pytest.raises(ValueError):
            advance()
        assert state() == before

    owner.advance_system_cycles(1 << 32)
    assert owner.system_cycles == 9 + (1 << 32)
    assert core.timer_counter == 9


def test_native_system_clock_rejects_reentrant_guest_callbacks() -> None:
    owner = NativeSystemState(1)
    memory = bytearray(16)
    memory[0] = 0x91  # OUT1
    owner.attach_mem(memory, len(memory))
    core = owner.core(0)
    core.timer_init()
    core.timer_control = 1
    _set_pc(core, 0)
    rejected = []

    def try_reentrant_advance(_port, _value):
        with pytest.raises(
            RuntimeError,
            match="time cannot advance during guest execution",
        ):
            owner.advance_system_cycles(1)
        rejected.append(True)

    assert _native_step(core, on_output=try_reentrant_advance) > 0
    assert rejected == [True]
    assert owner.system_cycles == 0
    assert core.timer_counter == 0

    owner.advance_system_cycles(1)
    assert owner.system_cycles == 1
    assert core.timer_counter == 1


def test_native_event_horizon_tracks_ties_reschedules_and_clears() -> None:
    owner = NativeSystemState(1)

    assert (
        owner.EVENT_TIMER,
        owner.EVENT_FRAMEBUFFER,
        owner.EVENT_RTC,
        owner.EVENT_INTERRUPT,
        owner.EVENT_EXTERNAL,
        owner.EVENT_SOURCE_COUNT,
    ) == (0, 1, 2, 3, 4, 5)
    assert owner.event_horizon() == (0, None, 0)
    assert owner.system_clock_snapshot() == (
        0,
        (None, None, None, None, None),
        None,
        0,
    )

    owner.set_event_deadline(EVENT_SOURCE_TIMER, 40)
    owner.set_event_deadline(EVENT_SOURCE_RTC, 20)
    owner.set_event_deadline(EVENT_SOURCE_EXTERNAL, 20)
    assert owner.event_horizon() == (
        0,
        20,
        (1 << EVENT_SOURCE_RTC) | (1 << EVENT_SOURCE_EXTERNAL),
    )

    owner.set_event_deadline(EVENT_SOURCE_TIMER, 20)
    assert owner.event_horizon() == (
        0,
        20,
        (1 << EVENT_SOURCE_TIMER)
        | (1 << EVENT_SOURCE_RTC)
        | (1 << EVENT_SOURCE_EXTERNAL),
    )

    before_crossing = owner.event_horizon()
    with pytest.raises(ValueError, match="cannot cross the event horizon"):
        owner.advance_system_cycles(21)
    assert owner.event_horizon() == before_crossing

    owner.advance_system_to(20)
    assert owner.event_horizon() == (
        20,
        20,
        (1 << EVENT_SOURCE_TIMER)
        | (1 << EVENT_SOURCE_RTC)
        | (1 << EVENT_SOURCE_EXTERNAL),
    )
    with pytest.raises(ValueError, match="cannot cross the event horizon"):
        owner.advance_system_cycles(1)

    owner.set_event_deadline(EVENT_SOURCE_RTC, 50)
    owner.clear_event_deadline(EVENT_SOURCE_EXTERNAL)
    assert owner.event_horizon() == (20, 20, 1 << EVENT_SOURCE_TIMER)
    assert owner.system_clock_snapshot() == (
        20,
        (20, None, 50, None, None),
        20,
        1 << EVENT_SOURCE_TIMER,
    )

    owner.clear_event_deadline(EVENT_SOURCE_TIMER)
    assert owner.event_horizon() == (20, 50, 1 << EVENT_SOURCE_RTC)

    owner.clear_event_deadline(EVENT_SOURCE_RTC)
    assert owner.event_horizon() == (20, None, 0)


def test_native_event_deadlines_reject_past_cycles_and_unknown_sources() -> None:
    owner = NativeSystemState(1)
    owner.advance_system_to(10)
    owner.set_event_deadline(EVENT_SOURCE_FRAMEBUFFER, 12)
    owner.set_event_deadline(EVENT_SOURCE_INTERRUPT, 10)

    assert owner.event_horizon() == (
        10,
        10,
        1 << EVENT_SOURCE_INTERRUPT,
    )

    with pytest.raises(ValueError):
        owner.set_event_deadline(EVENT_SOURCE_FRAMEBUFFER, 9)
    for source_id in (-1, EVENT_SOURCE_EXTERNAL + 1):
        with pytest.raises(ValueError):
            owner.set_event_deadline(source_id, 10)
        with pytest.raises(ValueError):
            owner.clear_event_deadline(source_id)

    assert owner.event_horizon() == (
        10,
        10,
        1 << EVENT_SOURCE_INTERRUPT,
    )
    owner.clear_event_deadline(EVENT_SOURCE_INTERRUPT)
    assert owner.event_horizon() == (
        10,
        12,
        1 << EVENT_SOURCE_FRAMEBUFFER,
    )


def test_borrowed_core_view_retains_its_native_owner() -> None:
    def make_retained_core():
        owner = NativeSystemState(1)
        return owner.core(0)

    core = make_retained_core()
    gc.collect()

    core.set_reg(9, 0xCAFE)
    assert core.get_reg(9) == 0xCAFE
    core.timer_init()
    core.timer_control = 1
    core.timer_tick(3)
    assert core.timer_counter == 3
    core.uart_geom_init(91, 31)
    core.uart_geom_host_set_size(92, 32)
    assert (core.uart_geom_cols, core.uart_geom_rows) == (92, 32)
    core.fb_init()
    core.fb_width = 512
    core.fb_height = 288
    assert core.fb_snapshot()[1:3] == (512, 288)
    core.rtc_init(False, 1_000, 1, 2, 3, 4, 5, 2026, 6)
    core.rtc_uptime_ms = 17
    core.rtc_tick(RTC.MS_DIVISOR)
    assert core.rtc_snapshot()[2] == 18


def test_borrowed_micro_core_view_retains_its_native_owner() -> None:
    def make_retained_micro():
        owner = NativeSystemState(1, 5)
        return owner.micro_core(2)

    micro = make_retained_micro()
    gc.collect()

    assert micro.is_micro_core
    assert (micro.core_id, micro.num_cores) == (3, 5)
    micro.set_reg(9, 0xBEEF)
    assert micro.get_reg(9) == 0xBEEF
    assert micro.ipi_send(0)


def test_borrowed_core_views_retain_the_shared_interrupt_router() -> None:
    def make_retained_cores():
        owner = NativeSystemState(2, 6)
        return owner.core(0), owner.core(1)

    core0, core1 = make_retained_cores()
    gc.collect()

    assert core0.ipi_send(1)
    assert core1.ipi_pending_mask() == 0b1
    assert core1.irq_ipi
    assert core1.ipi_ack(0)
    assert core1.ipi_pending_mask() == 0
    assert not core1.irq_ipi


def test_standalone_native_ipi_latches_remain_private() -> None:
    first = _mp64_accel.CPUState()
    second = _mp64_accel.CPUState()

    first.irq_ipi = True

    assert first.irq_ipi
    assert not second.irq_ipi
    assert first.ipi_pending_mask() == second.ipi_pending_mask() == 0
    assert not first.ipi_send(0)
    assert not first.ipi_ack(0)


def test_megapad_system_wraps_native_owned_full_cores() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    owner = system._native_system

    assert owner.full_core_count == 2
    assert owner.all_core_count == system.num_cores == 6
    assert owner.main_bus_port_count == 6
    assert owner.main_bus_port_for_requester(owner.NIC_DMA_REQUESTER_ID) == 3
    assert owner.main_bus_port_for_requester(owner.DISK_DMA_REQUESTER_ID) == 4
    assert owner.main_bus_port_for_requester(owner.WOTS_DMA_REQUESTER_ID) == 5
    assert all(cpu._system_owner is owner for cpu in system.cores[:2])

    owner.core(0).set_reg(10, 0x1234)
    system.cores[1].regs[10] = 0x5678
    assert system.cores[0].regs[10] == 0x1234
    assert owner.core(1).get_reg(10) == 0x5678

    system.cores[0].mem[0x180] = 0xA5
    assert system.cores[1].mem[0x180] == 0xA5


def test_megapad_system_preserves_extended_full_core_configuration() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=8,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )

    assert system._native_system.full_core_count == 8
    assert system._native_system.all_core_count == 8
    assert len(system.cores) == system.sysinfo.num_full_cores == 8


@pytest.mark.parametrize("with_backend", [False, True], ids=["facade", "backend"])
def test_nic_callbacks_do_not_hide_a_native_owner_cycle(
    with_backend: bool,
) -> None:
    backend = LoopbackBackend() if with_backend else None
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        nic_backend=backend,
    )
    owner_ref = weakref.ref(system._native_system)
    core_ref = weakref.ref(system.cores[0]._cs)
    nic_ref = weakref.ref(system.nic)
    late_rx = backend.on_rx_frame if backend is not None else None

    del system
    gc.collect()

    assert owner_ref() is None
    assert core_ref() is None
    assert nic_ref() is None
    if late_rx is not None:
        late_rx(b"late frame")
        backend.stop()


def test_standalone_cpu_ownership_and_execution_remain_available() -> None:
    cpu = Megapad64(mem_size=256)
    program = assemble("nop")
    cpu.load_bytes(0, program)
    cpu.pc = 0

    assert cpu._system_owner is None
    assert cpu.step() == 1
    assert cpu.pc == len(program)


def test_system_remaining_native_peripherals_are_singletons() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)

    assert core0.uart_enabled() and core1.uart_enabled()
    core1.uart_inject(b"Q")
    assert core0.uart_read8(UART_BASE + 0x01) == ord("Q")
    core1.uart_write8(UART_BASE, ord("A"))
    assert core0.uart_drain_tx() == b"A"

    core1.nic_write8(NIC_BASE + 0x02, 0xA5)
    assert core0.nic_read8(NIC_BASE + 0x02) == 0xA5
    assert core1.nic_inject_frame(b"shared frame")
    assert core0.nic_rx_queue_size() == 1

    core1.crypto_write8(SHA3_BASE + 0x02, 3)
    assert core0.crypto_read8(SHA3_BASE + 0x02) == 3
    core1.crypto_write8(WOTS_BASE + 0x00, 0xA5)
    assert core0.crypto_read8(WOTS_BASE + 0x00) == 0xA5
    assert core0.crypto_wots_status() == 0

    core1.disable_trng()
    assert not core0.trng_enabled()
    core0.init_trng()
    assert core1.trng_enabled()


def test_native_trng_unavailable_contract_and_explicit_reinit() -> None:
    cpu = Megapad64(mem_size=256)
    state = cpu._cs
    state.init_trng()

    assert state.trng_enabled()
    assert state.trng_usable()
    assert state._native_singleton_read8(TRNG_BASE + 0x10) == 0x01

    state.disable_trng()

    assert not state.trng_enabled()
    assert not state.trng_usable()
    assert state._native_singleton_read8(TRNG_BASE + 0x10) == 0
    assert state._native_singleton_read8(TRNG_BASE + 0x11) == 0
    assert state._trng_test_zeroized_state() == (True, True)
    assert state._native_singleton_write8(TRNG_BASE + 0x18, 0xA5)
    assert state._trng_test_zeroized_state() == (True, True)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        state._native_singleton_read8(TRNG_BASE)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        state._native_singleton_read8(TRNG_BASE + 0x08)

    state.init_trng()

    assert state.trng_enabled()
    assert state.trng_usable()
    assert state._native_singleton_read8(TRNG_BASE + 0x10) == 0x01
    assert 0 <= state._native_singleton_read8(TRNG_BASE) <= 0xFF


def test_native_trng_pending_seed_is_wiped_on_failure_and_disable() -> None:
    cpu = Megapad64(mem_size=256)
    state = cpu._cs

    def stage_pending_seed() -> None:
        state.init_trng()
        # With 57 bytes consumed, SEED[7] lies just beyond this pool and is
        # retained for the next checked host refill.
        for _ in range(57):
            state._native_singleton_read8(TRNG_BASE)
        assert state._trng_test_zeroized_state()[1]
        assert state._native_singleton_write8(
            TRNG_BASE + 0x1F,
            0xA5,
        )
        assert not state._trng_test_zeroized_state()[1]

    stage_pending_seed()
    state._trng_test_health_loss_after(0)

    assert state._trng_test_zeroized_state() == (True, True)

    stage_pending_seed()
    state.disable_trng()

    assert state._trng_test_zeroized_state() == (True, True)


def test_native_trng_health_loss_boundary_is_shared_and_sticky() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)
    core0.init_trng()

    core1._trng_test_health_loss_after(0)

    assert not core0.trng_usable()
    assert core0._native_singleton_read8(TRNG_BASE + 0x10) == 0
    assert core1._trng_test_zeroized_state() == (True, True)
    assert core0._native_singleton_write8(TRNG_BASE + 0x18, 0x5A)
    assert core1._trng_test_zeroized_state() == (True, True)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        core0._native_singleton_read8(TRNG_BASE)

    core0.init_trng()
    core1._trng_test_health_loss_after(8)

    for index in range(8):
        reader = core0 if index % 2 == 0 else core1
        value = reader._native_singleton_read8(
            TRNG_BASE + 0x08 + index
        )
        assert 0 <= value <= 0xFF

    # The eighth byte is delivered, then the shared source fails closed.
    assert core0._native_singleton_read8(TRNG_BASE + 0x10) == 0
    assert core1._native_singleton_read8(TRNG_BASE + 0x10) == 0
    assert core0._trng_test_zeroized_state() == (True, True)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        core1._native_singleton_read8(TRNG_BASE + 0x08)

    core1.init_trng()

    assert core0.trng_usable()
    assert core0._native_singleton_read8(TRNG_BASE + 0x10) == 0x01


def test_native_trng_refill_failure_is_guest_visible_and_recoverable() -> None:
    cpu = Megapad64(mem_size=256)
    state = cpu._cs
    state.init_trng()
    state._trng_test_fail_next_refill()

    for _ in range(64):
        assert 0 <= state._native_singleton_read8(TRNG_BASE) <= 0xFF

    assert state.trng_enabled()
    assert not state.trng_usable()
    assert state._native_singleton_read8(TRNG_BASE + 0x10) == 0
    assert state._trng_test_zeroized_state() == (True, True)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        state._native_singleton_read8(TRNG_BASE)
    with pytest.raises(RuntimeError, match=r"^TRAP:BUS_FAULT$"):
        state._native_singleton_read8(TRNG_BASE + 0x0F)

    state.init_trng()

    assert state.trng_usable()
    assert state._native_singleton_read8(TRNG_BASE + 0x10) == 0x01
    assert 0 <= state._native_singleton_read8(TRNG_BASE) <= 0xFF


def test_shared_crypto_initialization_does_not_depend_on_wrapper_order() -> None:
    owner = NativeSystemState(2)
    owner.attach_mem(bytearray(4096), 4096)

    secondary = Megapad64._from_system_state(owner, 1, num_cores=2)
    assert secondary._cs.crypto_enabled()
    secondary._cs.crypto_write8(SHA3_BASE + 0x02, 3)

    primary = Megapad64._from_system_state(owner, 0, num_cores=2)
    assert primary._cs.crypto_enabled()
    assert primary._cs.crypto_read8(SHA3_BASE + 0x02) == 3


def test_standalone_remaining_native_peripherals_remain_private() -> None:
    first = Megapad64(mem_size=4096)
    second = Megapad64(mem_size=4096)
    first_cs = first._cs
    second_cs = second._cs

    for cs, mac in (
        (first_cs, b"\x02\x00\x00\x00\x00\x01"),
        (second_cs, b"\x02\x00\x00\x00\x00\x02"),
    ):
        cs.uart_init()
        cs.nic_init(mac)
        cs.init_trng()

    first_cs.uart_inject(b"X")
    first_cs.nic_write8(NIC_BASE + 0x02, 0x5A)
    first_cs.crypto_write8(SHA3_BASE + 0x02, 2)
    first_cs.disable_trng()

    assert first_cs.uart_has_rx()
    assert not second_cs.uart_has_rx()
    assert first_cs.nic_read8(NIC_BASE + 0x02) == 0x5A
    assert second_cs.nic_read8(NIC_BASE + 0x02) == 0
    assert first_cs.crypto_read8(SHA3_BASE + 0x02) == 2
    assert second_cs.crypto_read8(SHA3_BASE + 0x02) == 0
    assert not first_cs.trng_enabled()
    assert second_cs.trng_enabled()


@pytest.mark.parametrize(
    ("mmio_offset", "expected"),
    (
        pytest.param(UART_BASE + 0x01, ord("Q"), id="uart"),
        pytest.param(NIC_BASE + 0x02, 0xA5, id="nic"),
        pytest.param(AES_BASE + 0x3A, 0x01, id="crypto"),
        pytest.param(TRNG_BASE + 0x10, 0x01, id="trng"),
        pytest.param(WOTS_BASE + 0x08, 0x0B, id="wots-steps"),
    ),
)
def test_full_core_python_fallback_reads_system_native_singletons(
    mmio_offset: int,
    expected: int,
) -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    primary = system.cores[0]._cs
    secondary = system.cores[1]
    if mmio_offset == UART_BASE + 0x01:
        primary.uart_inject(b"Q")
    elif mmio_offset == NIC_BASE + 0x02:
        primary.nic_write8(mmio_offset, expected)
    elif mmio_offset == AES_BASE + 0x3A:
        primary.crypto_write8(mmio_offset, expected)
    elif mmio_offset == WOTS_BASE + 0x08:
        primary.crypto_write8(mmio_offset, expected)

    # LOAD2D is a transactional Python fallback. Keep its eager TSRC0 tile
    # read in RAM, then gather through the target byte from its 64-byte-aligned
    # MMIO cursor.
    cursor_address = (MMIO_BASE + mmio_offset) & ~0x3F
    cursor_offset = (MMIO_BASE + mmio_offset) - cursor_address
    program = assemble("t.load2d")
    secondary.load_bytes(0, program)
    secondary.pc = 0
    secondary.tsrc0 = 0x100
    secondary.tdst = 0x200
    secondary.sb = cursor_address // (4 * 1024 * 1024)
    secondary.sr = 0
    secondary.sc = (cursor_address // 64) & 0xFFFF
    secondary.sw = 1
    secondary.tstride_r = 1
    secondary.ttile_h = 1
    secondary.ttile_w = cursor_offset + 1

    secondary.step()

    assert secondary._py_fallback is not None
    assert secondary.mem[secondary.tdst + cursor_offset] == expected


@pytest.mark.parametrize(
    "mmio_offset",
    (
        pytest.param(NIC_BASE + 0x02, id="nic"),
        pytest.param(WOTS_BASE + 0x00, id="crypto"),
    ),
)
def test_full_core_python_fallback_writes_system_native_singletons(
    mmio_offset: int,
) -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    primary = system.cores[0]._cs
    secondary = system.cores[1]
    cursor_address = (MMIO_BASE + mmio_offset) & ~0x3F
    cursor_offset = (MMIO_BASE + mmio_offset) - cursor_address
    written = 0x5A
    secondary.mem[0x100 + cursor_offset] = written
    secondary.load_bytes(0, assemble("t.store2d"))
    secondary.pc = 0
    secondary.tsrc0 = 0x100
    secondary.tdst = 0x200
    secondary.sb = cursor_address // (4 * 1024 * 1024)
    secondary.sr = 0
    secondary.sc = (cursor_address // 64) & 0xFFFF
    secondary.sw = 1
    secondary.tstride_r = 1
    secondary.ttile_h = 1
    secondary.ttile_w = cursor_offset + 1

    secondary.step()

    assert secondary._py_fallback is not None
    if mmio_offset == NIC_BASE + 0x02:
        assert primary.nic_read8(mmio_offset) == written
    else:
        assert primary.crypto_read8(mmio_offset) == written


def test_system_timer_is_one_native_instance_for_every_full_core() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)
    system.timer.counter = 23
    system.timer.compare = 99
    system.timer.control = 1
    system.timer.status = 1
    system.timer.irq_pending = True

    assert (
        core1.timer_counter,
        core1.timer_compare,
        core1.timer_control,
        core1.timer_status,
        core1.timer_irq_pending,
    ) == (23, 99, 1, 1, True)

    core1.timer_compare = 123
    core1.timer_write8(TIMER_BASE + 0x09, 0x01)
    core1.timer_tick(5)

    assert core0.timer_counter == system.timer.counter == 28
    assert core0.timer_compare == system.timer.compare == 123
    assert core0.timer_status == system.timer.status == 0
    assert not core0.timer_irq_pending
    assert not system.timer.irq_pending


def test_standalone_native_timers_remain_private() -> None:
    first = _mp64_accel.CPUState()
    second = _mp64_accel.CPUState()
    first.timer_init()
    second.timer_init()
    first.timer_control = 1
    first.timer_tick(17)
    first.timer_compare = 41

    assert first.timer_counter == 17
    assert first.timer_compare == 41
    assert second.timer_counter == 0
    assert second.timer_compare == 0xFFFF_FFFF


def test_system_framebuffer_is_one_native_instance_for_every_full_core() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)

    system.fb.fb_base = 0x1234_5678
    system.fb.width = 640
    system.fb.height = 360
    system.fb.stride = 2560
    system.fb.mode = 3
    system.fb.enable = 3
    core1.fb_set_palette_entry(7, 0x12AB34)

    assert (
        core0.fb_base_addr,
        core1.fb_base_addr,
        core0.fb_width,
        core1.fb_width,
        core0.fb_height,
        core1.fb_height,
        core0.fb_stride,
        core1.fb_stride,
        core0.fb_mode,
        core1.fb_mode,
        core0.fb_enable,
        core1.fb_enable,
    ) == (
        0x1234_5678,
        0x1234_5678,
        640,
        640,
        360,
        360,
        2560,
        2560,
        3,
        3,
        3,
        3,
    )
    assert system.fb.palette[7] == core0.fb_get_palette()[7] == 0x12AB34
    assert system.fb.snapshot() == (
        0x1234_5678,
        640,
        360,
        2560,
        3,
        3,
        0,
        False,
        33333,
    )

    core1.fb_write8(FB_BASE + 0x20, 1)
    core1.fb_write8(FB_BASE + 0x28, 2)
    assert (system.fb.mode, system.fb.enable) == (1, 2)

    core1.fb_enable = 3
    core1.fb_cycles_per_frame = 5
    core1.fb_tick(5)
    assert (
        core0.fb_vsync_count,
        system.fb.vsync_count,
        core0.fb_vblank,
        system.fb.vblank,
        core0.fb_irq_pending(),
    ) == (1, 1, True, True, True)

    for index, value in enumerate((1, 0, 0, 0)):
        core0.fb_write8(FB_BASE + 0x30 + index, value)
    assert not core1.fb_vblank
    assert not system.fb.irq_pending

    core0.fb_vsync_count = 0xFFFF_FFFF
    core0.fb_vblank = False
    core1.fb_host_present()
    assert (
        system.fb.vsync_count,
        system.fb.vblank,
        system.fb.irq_pending,
    ) == (0, True, True)


def test_shared_framebuffer_render_uses_one_palette_snapshot() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=16,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)

    system._vram_mem[0] = 7
    core1.fb_base_addr = system.vram_base
    core1.fb_width = 1
    core1.fb_height = 1
    core1.fb_stride = 1
    core1.fb_mode = 0
    system.fb.palette = [
        0x000000 if index != 7 else 0x12AB34
        for index in range(256)
    ]

    image = core0.render_fb_rgb()

    assert image.shape == (1, 1, 3)
    assert image[0, 0, :].tolist() == [0x12, 0xAB, 0x34]


def test_standalone_native_framebuffers_remain_private() -> None:
    first = _mp64_accel.CPUState()
    second = _mp64_accel.CPUState()
    first.fb_init()
    second.fb_init()

    first.fb_base_addr = 0x2000
    first.fb_width = 800
    first.fb_height = 600
    first.fb_mode = 3
    first.fb_set_palette_entry(9, 0xABCDEF)

    assert (
        first.fb_base_addr,
        first.fb_width,
        first.fb_height,
        first.fb_mode,
        first.fb_get_palette()[9],
    ) == (0x2000, 800, 600, 3, 0xABCDEF)
    assert (
        second.fb_base_addr,
        second.fb_width,
        second.fb_height,
        second.fb_mode,
        second.fb_get_palette()[9],
    ) == (0, 320, 240, 0, 0x090909)


def test_system_rtc_is_one_native_instance_for_every_full_core() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        realtime_clock=False,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)

    assert core0.rtc_enabled()
    assert core1.rtc_enabled()

    system.rtc.realtime = False
    system.rtc.uptime_ms = 0x0102_0304_0506_0708
    system.rtc.epoch_ms = 0x1112_1314_1516_1718
    system.rtc.sec = 59
    system.rtc.min = 59
    system.rtc.hour = 23
    system.rtc.day = 28
    system.rtc.mon = 2
    system.rtc.year = 2024
    system.rtc.dow = 3
    system.rtc.ctrl = 3
    system.rtc.status = 0
    system.rtc.alarm_sec = 0
    system.rtc.alarm_min = 0
    system.rtc.alarm_hour = 0
    system.rtc.irq_pending = False
    system.rtc._ms_prescaler = RTC.MS_DIVISOR - 1
    system.rtc._sec_prescaler = 999

    assert (
        core1.rtc_uptime_ms,
        core1.rtc_epoch_ms,
        core1.rtc_sec,
        core1.rtc_min,
        core1.rtc_hour,
        core1.rtc_day,
        core1.rtc_mon,
        core1.rtc_year,
        core1.rtc_dow,
        core1.rtc_ctrl,
        core1.rtc_alarm_sec,
        core1.rtc_alarm_min,
        core1.rtc_alarm_hour,
    ) == (
        0x0102_0304_0506_0708,
        0x1112_1314_1516_1718,
        59,
        59,
        23,
        28,
        2,
        2024,
        3,
        3,
        0,
        0,
        0,
    )
    assert core0.rtc_snapshot() == core1.rtc_snapshot() == system.rtc.snapshot()

    core1.rtc_tick(1)
    assert (
        core0.rtc_uptime_ms,
        system.rtc.uptime_ms,
        core0.rtc_sec,
        system.rtc.sec,
        core0.rtc_day,
        system.rtc.day,
        core0.rtc_dow,
        system.rtc.dow,
        core0.rtc_status,
        system.rtc.status,
        core0.rtc_irq_pending,
        system.rtc.irq_pending,
    ) == (
        0x0102_0304_0506_0709,
        0x0102_0304_0506_0709,
        0,
        0,
        29,
        29,
        4,
        4,
        0x07,
        0x07,
        True,
        True,
    )

    core0.rtc_write8(RTC_BASE + 0x19, 0x07)
    assert core1.rtc_status == system.rtc.status == 0
    assert not core1.rtc_irq_pending
    assert not system.rtc.irq_pending


def test_system_rtc_latches_are_shared_across_full_cores() -> None:
    owner = NativeSystemState(2)
    core0 = owner.core(0)
    core1 = owner.core(1)
    core0.rtc_init(False, 0, 0, 0, 0, 1, 1, 2000, 6)
    old_uptime = 0x0102_0304_0506_0708
    old_epoch = 0x1112_1314_1516_1718
    new_uptime = 0xA1A2_A3A4_A5A6_A7A8
    new_epoch = 0xB1B2_B3B4_B5B6_B7B8
    core0.rtc_uptime_ms = old_uptime
    core0.rtc_epoch_ms = old_epoch

    uptime_low = core0.rtc_read8(RTC_BASE)
    epoch_low = core0.rtc_read8(RTC_BASE + 0x08)
    core1.rtc_uptime_ms = new_uptime
    core1.rtc_epoch_ms = new_epoch

    latched_uptime = uptime_low | sum(
        core1.rtc_read8(RTC_BASE + index) << (8 * index)
        for index in range(1, 8)
    )
    latched_epoch = epoch_low | sum(
        core1.rtc_read8(RTC_BASE + 0x08 + index) << (8 * index)
        for index in range(1, 8)
    )
    assert (latched_uptime, latched_epoch) == (old_uptime, old_epoch)

    refreshed_uptime = core1.rtc_read8(RTC_BASE) | sum(
        core0.rtc_read8(RTC_BASE + index) << (8 * index)
        for index in range(1, 8)
    )
    refreshed_epoch = core1.rtc_read8(RTC_BASE + 0x08) | sum(
        core0.rtc_read8(RTC_BASE + 0x08 + index) << (8 * index)
        for index in range(1, 8)
    )
    assert (refreshed_uptime, refreshed_epoch) == (new_uptime, new_epoch)


def test_standalone_native_rtcs_remain_private() -> None:
    first = _mp64_accel.CPUState()
    second = _mp64_accel.CPUState()
    first.rtc_init(False, 1_000, 1, 2, 3, 4, 5, 2026, 6)
    second.rtc_init(False, 2_000, 7, 8, 9, 10, 11, 2030, 1)

    first.rtc_uptime_ms = 41
    first.rtc_tick(RTC.MS_DIVISOR)
    first.rtc_alarm_sec = 17

    assert (
        first.rtc_uptime_ms,
        first.rtc_epoch_ms,
        first.rtc_alarm_sec,
    ) == (42, 1_001, 17)
    assert (
        second.rtc_uptime_ms,
        second.rtc_epoch_ms,
        second.rtc_alarm_sec,
    ) == (0, 2_000, 0)


def test_system_uart_geometry_is_one_native_instance_for_every_full_core() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = (cpu._cs for cpu in system.cores)

    core1.uart_geom_status = 0xA2
    core1.uart_geom_req_cols = 111
    core1.uart_geom_req_rows = 37
    core1.uart_geom_ctrl = 0xA3
    system.uart_geom.host_set_size(132, 43)
    assert (
        core0.uart_geom_cols,
        core1.uart_geom_cols,
        core0.uart_geom_rows,
        core1.uart_geom_rows,
        core0.uart_geom_status,
        core1.uart_geom_status,
        core0.uart_geom_ctrl,
        core1.uart_geom_ctrl,
        core0.uart_geom_req_cols,
        core1.uart_geom_req_rows,
    ) == (132, 132, 43, 43, 0xA3, 0xA3, 0xA3, 0xA3, 111, 37)

    core1.uart_geom_status = 0xA2
    core1.uart_geom_req_cols = 120
    core1.uart_geom_req_rows = 40
    core1.uart_geom_ctrl = 0xA3
    assert system.uart_geom.has_resize_request()
    assert (system.uart_geom.req_cols, system.uart_geom.req_rows) == (120, 40)

    system.uart_geom.host_accept_resize(120, 40)
    assert (core0.uart_geom_cols, core1.uart_geom_cols) == (120, 120)
    assert (core0.uart_geom_rows, core1.uart_geom_rows) == (40, 40)
    assert core0.uart_geom_ctrl == core1.uart_geom_ctrl == 0xA1
    assert core0.uart_geom_status == core1.uart_geom_status == 0xA3

    core1.uart_geom_write8(UART_GEOM_BASE + 0x04, 0x01)
    assert core0.uart_geom_status == system.uart_geom.status == 0xA2

    core1.uart_geom_req_cols = 200
    core1.uart_geom_req_rows = 60
    core1.uart_geom_ctrl = 0xA3
    core1.uart_geom_status = 0xA1
    system.uart_geom.host_deny_resize()
    assert (system.uart_geom.cols, system.uart_geom.rows) == (120, 40)
    assert core0.uart_geom_ctrl == core1.uart_geom_ctrl == 0xA1
    assert core0.uart_geom_status == core1.uart_geom_status == 0xA3


def test_uart_geometry_completion_rejects_stale_request_snapshots() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    geometry = system.uart_geom
    geometry.host_set_size(90, 30)
    geometry.status = 0xA0
    geometry.req_cols = 100
    geometry.req_rows = 35
    geometry.ctrl = 0xA3
    generation, cols, rows = geometry.snapshot_resize_request()
    assert (cols, rows) == (100, 35)

    geometry.req_cols = 132
    geometry.req_rows = 43
    assert not geometry.host_accept_resize_if_pending(
        generation, cols, rows)
    assert not geometry.host_deny_resize_if_pending(generation)
    assert (
        geometry.cols,
        geometry.rows,
        geometry.status,
        geometry.ctrl,
        geometry.req_cols,
        geometry.req_rows,
    ) == (90, 30, 0xA0, 0xA3, 132, 43)

    replacement_generation, replacement_cols, replacement_rows = (
        geometry.snapshot_resize_request()
    )
    assert (replacement_cols, replacement_rows) == (132, 43)
    assert geometry.host_deny_resize_if_pending(replacement_generation)
    assert (
        geometry.cols,
        geometry.rows,
        geometry.status,
        geometry.ctrl,
    ) == (90, 30, 0xA2, 0xA1)
    assert geometry.snapshot_resize_request() is None
    assert not geometry.host_accept_resize_if_pending(
        replacement_generation, replacement_cols, replacement_rows)
    assert not geometry.host_deny_resize_if_pending(replacement_generation)


@pytest.mark.parametrize(
    ("offset", "value"),
    (
        pytest.param(0x05, 0xA3, id="control"),
        pytest.param(0x06, 0x79, id="requested-columns-low"),
        pytest.param(0x07, 0x9A, id="requested-columns-high"),
        pytest.param(0x08, 0xBC, id="requested-rows-low"),
        pytest.param(0x09, 0xDE, id="requested-rows-high"),
    ),
)
def test_uart_geometry_guest_writes_invalidate_host_request_snapshots(
    offset: int,
    value: int,
) -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    geometry = system.uart_geom
    geometry.req_cols = 0x1234
    geometry.req_rows = 0x5678
    geometry.ctrl = 0xA3
    generation, cols, rows = geometry.snapshot_resize_request()

    geometry.write8(offset, value)
    assert not geometry.host_accept_resize_if_pending(
        generation, cols, rows)
    assert not geometry.host_deny_resize_if_pending(generation)
    assert geometry.ctrl & 0x02

    replacement_generation, replacement_cols, replacement_rows = (
        geometry.snapshot_resize_request()
    )
    assert geometry.host_accept_resize_if_pending(
        replacement_generation,
        replacement_cols,
        replacement_rows,
    )
    assert (
        geometry.cols,
        geometry.rows,
        geometry.status,
        geometry.ctrl,
    ) == (replacement_cols, replacement_rows, 0x01, 0xA1)


def test_uart_geometry_control_setter_invalidates_host_request_snapshot() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    geometry = system.uart_geom
    geometry.req_cols = 120
    geometry.req_rows = 40
    geometry.ctrl = 0xA3
    generation, cols, rows = geometry.snapshot_resize_request()

    geometry.ctrl = 0xA3

    assert not geometry.host_accept_resize_if_pending(
        generation, cols, rows)
    assert not geometry.host_deny_resize_if_pending(generation)
    assert geometry.snapshot_resize_request()[1:] == (120, 40)


@pytest.mark.parametrize(
    ("field", "value"),
    (
        pytest.param("req_cols", 132, id="requested-columns"),
        pytest.param("req_rows", 43, id="requested-rows"),
    ),
)
def test_uart_geometry_request_setters_invalidate_host_request_snapshot(
    field: str,
    value: int,
) -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    geometry = system.uart_geom
    geometry.req_cols = 120
    geometry.req_rows = 40
    geometry.ctrl = 0xA3
    generation, cols, rows = geometry.snapshot_resize_request()

    setattr(geometry, field, value)

    assert not geometry.host_accept_resize_if_pending(
        generation, cols, rows)
    assert not geometry.host_deny_resize_if_pending(generation)
    assert geometry.snapshot_resize_request() is not None


def test_standalone_native_uart_geometry_remains_private() -> None:
    first = _mp64_accel.CPUState()
    second = _mp64_accel.CPUState()
    first.uart_geom_init(80, 24)
    second.uart_geom_init(132, 43)

    first.uart_geom_host_set_size(90, 30)
    first.uart_geom_req_cols = 100
    first.uart_geom_req_rows = 35
    first.uart_geom_ctrl = 0x02

    assert (
        first.uart_geom_cols,
        first.uart_geom_rows,
        first.uart_geom_req_cols,
        first.uart_geom_req_rows,
        first.uart_geom_ctrl,
    ) == (90, 30, 100, 35, 0x02)
    assert (
        second.uart_geom_cols,
        second.uart_geom_rows,
        second.uart_geom_req_cols,
        second.uart_geom_req_rows,
        second.uart_geom_ctrl,
    ) == (132, 43, 0, 0, 0)


@pytest.mark.parametrize(
    ("region", "attach_name", "base", "size_attr"),
    REGIONS,
    ids=lambda value: value if isinstance(value, str) else None,
)
def test_system_mapping_is_one_export_visible_to_every_core(
    region: str,
    attach_name: str,
    base: int | None,
    size_attr: str,
) -> None:
    store = assemble("st.b r4, r1")
    load = assemble("ld.b r5, r4")
    main_exporter = _CountingExporter(256)
    main = main_exporter.storage
    main[: len(store + load)] = store + load
    owner = NativeSystemState(2)
    owner.attach_mem(main_exporter, len(main))
    assert main_exporter.acquisitions == 1

    if region == "main":
        exporter = main_exporter
        backing = main
        target = 0x80
    else:
        exporter = _CountingExporter(16)
        backing = exporter.storage
        target = base + 3
        _attach_system_region(
            owner, attach_name, base, exporter, len(backing)
        )
        assert exporter.acquisitions == 1

    core0 = owner.core(0)
    core1 = owner.core(1)
    _set_pc(core0, 0)
    _set_pc(core1, len(store))
    core0.set_reg(4, target)
    core0.set_reg(1, 0xA5)
    core1.set_reg(4, target)
    core1.set_reg(5, 0)

    assert _native_step(core0) > 0
    assert _native_step(core1) > 0

    offset = target if region == "main" else target - base
    assert backing[offset] == 0xA5
    assert core1.get_reg(5) == 0xA5
    assert getattr(core0, size_attr) == getattr(core1, size_attr)
    if base is not None:
        base_attr = f"{region}_base"
        assert getattr(core0, base_attr) == getattr(core1, base_attr) == base
    assert exporter.acquisitions == 1


@pytest.mark.parametrize(
    ("region", "attach_name", "base", "size_attr"),
    REGIONS,
    ids=lambda value: value if isinstance(value, str) else None,
)
def test_system_mapping_replacement_is_transactional_before_sealing(
    region: str,
    attach_name: str,
    base: int | None,
    size_attr: str,
) -> None:
    owner = NativeSystemState(2)
    load = assemble("ld.b r5, r4")
    if region != "main":
        main = bytearray(64)
        main[: len(load)] = load
        owner.attach_mem(main, len(main))
    original = _CountingExporter(16)
    _attach_system_region(owner, attach_name, base, original, 8)
    original_base = (
        getattr(owner, f"{region}_base") if base is not None else None
    )

    with pytest.raises(BufferError):
        _attach_system_region(
            owner,
            attach_name,
            None if base is None else base + 0x100,
            bytes(16),
            16,
        )

    assert getattr(owner, size_attr) == 8
    if base is not None:
        assert getattr(owner, f"{region}_base") == original_base
    assert original.acquisitions == 1
    assert original.releases == 0

    replacement = _CountingExporter(24)
    replacement_base = None if base is None else base + 0x100
    target = 8 if base is None else replacement_base + 3
    if base is None:
        replacement.storage[: len(load)] = load
        replacement.storage[target] = 0xC3
    else:
        replacement.storage[target - replacement_base] = 0xC3
    _attach_system_region(
        owner,
        attach_name,
        replacement_base,
        replacement,
        12,
    )

    assert original.releases == 1
    assert replacement.acquisitions == 1
    assert replacement.releases == 0
    assert getattr(owner, size_attr) == 12
    buffer_attr = "mem_buffer" if region == "main" else f"{region}_buffer"
    assert getattr(owner, buffer_attr) is replacement
    core0 = owner.core(0)
    core1 = owner.core(1)
    assert getattr(core0, size_attr) == getattr(core1, size_attr) == 12
    if base is not None:
        assert (
            getattr(core0, f"{region}_base")
            == getattr(core1, f"{region}_base")
            == replacement_base
        )
    for core in (core0, core1):
        _set_pc(core, 0)
        core.set_reg(4, target)
        core.set_reg(5, 0)
        assert _native_step(core) > 0
        assert core.get_reg(5) == 0xC3


def test_system_mappings_seal_and_owned_cores_reject_divergent_mutation() -> None:
    owner = NativeSystemState(2)
    owner.attach_mem(bytearray(32), 32)
    owner.attach_hbw_mem(bytearray(16), 0x1000, 16)
    owner.attach_ext_mem(bytearray(16), 0x2000, 16)
    owner.attach_vram(bytearray(16), 0x3000, 16)
    core = owner.core(0)

    assert owner.mappings_sealed
    owner_replacements = (
        ("attach_mem", None),
        ("attach_hbw_mem", 0x1100),
        ("attach_ext_mem", 0x2100),
        ("attach_vram", 0x3100),
    )
    for attach_name, base in owner_replacements:
        rejected_exporter = _CountingExporter(64)
        with pytest.raises(RuntimeError, match="sealed"):
            _attach_system_region(
                owner, attach_name, base, rejected_exporter, 64
            )
        assert rejected_exporter.acquisitions == 0

    rejected_exporter = _CountingExporter(64)
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.attach_mem(rejected_exporter, 64)
    assert rejected_exporter.acquisitions == 0
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.attach_hbw_mem(bytearray(8), 0x1100, 8)
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.attach_ext_mem(bytearray(8), 0x2100, 8)
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.attach_vram(bytearray(8), 0x3100, 8)
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.mem_size = 16
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.hbw_base = 0x1100
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.hbw_size = 8
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.ext_mem_base = 0x2100
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.ext_mem_size = 8
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.vram_base = 0x3100
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        core.vram_size = 8

    assert (
        core.mem_size,
        core.hbw_base,
        core.hbw_size,
        core.ext_mem_base,
        core.ext_mem_size,
        core.vram_base,
        core.vram_size,
    ) == (
        32,
        0x1000,
        16,
        0x2000,
        16,
        0x3000,
        16,
    )


def test_borrowing_a_micro_core_seals_system_mappings() -> None:
    owner = NativeSystemState(1, 5)
    owner.attach_mem(bytearray(32), 32)

    micro = owner.micro_core(0)

    assert owner.mappings_sealed
    assert micro.mem_size == 32
    with pytest.raises(RuntimeError, match="sealed"):
        owner.attach_mem(bytearray(64), 64)
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        micro.attach_mem(bytearray(64), 64)


def test_buffer_callback_sealing_aborts_system_replacement_transactionally() -> None:
    owner = NativeSystemState(1)
    original = bytearray(8)
    owner.attach_mem(original, len(original))
    replacement = _CountingExporter(
        16,
        on_acquire=lambda: owner.core(0),
    )

    with pytest.raises(RuntimeError, match="sealed"):
        owner.attach_mem(replacement, len(replacement.storage))

    assert owner.mappings_sealed
    assert owner.mem_size == len(original)
    assert replacement.acquisitions == replacement.releases == 1
    with pytest.raises(BufferError):
        original.extend(b"x")


def test_system_mapping_lease_follows_a_retained_core_view() -> None:
    owner = NativeSystemState(1)
    exported = array.array("B", [0x01])
    exported_ref = weakref.ref(exported)
    owner.attach_mem(exported, len(exported))
    core = owner.core(0)
    del owner
    del exported
    gc.collect()

    assert exported_ref() is not None
    _set_pc(core, 0)
    assert _native_step(core) == 1

    del core
    gc.collect()
    assert exported_ref() is None


def test_cross_core_execution_and_render_use_one_mapping_guard() -> None:
    owner = NativeSystemState(2)
    owner.attach_mem(bytearray([0x91]), 1)
    owner.attach_vram(bytearray([0xE0, 0x07]), 0x3000, 2)
    core0 = owner.core(0)
    core1 = owner.core(1)
    _set_pc(core0, 0)
    _set_pc(core1, 0)
    core1.fb_base_addr = 0x3000
    core1.fb_width = 1
    core1.fb_height = 1
    core1.fb_stride = 2
    core1.fb_mode = 1
    rejected = []
    core1_pc = core1.get_reg(core1.psel)
    core1_cycles = core1.cycle_count

    def on_output(_port, _value):
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core1)
        rejected.append("execution")
        with pytest.raises(
            RuntimeError, match="^CPUState framebuffer render is busy$"
        ):
            core1.render_fb_rgb()
        rejected.append("render")

    assert _native_step(core0, on_output=on_output) > 0
    assert rejected == ["execution", "render"]
    assert core1.get_reg(core1.psel) == core1_pc
    assert core1.cycle_count == core1_cycles
    assert core1.render_fb_rgb()[:, 0, :].tolist() == [[0, 252, 0]]
    assert _native_step(core1) > 0
    assert core1.get_reg(core1.psel) > core1_pc


def test_secondary_proxy_dma_borrows_the_outer_mapping_scope() -> None:
    memory = bytearray(256)
    memory[0] = 0x91  # OUT1
    memory[0x80:0x88] = (1).to_bytes(8, "little")
    memory[0x88] = ord("A")
    owner = NativeSystemState(2)
    owner.attach_mem(memory, len(memory))
    core0 = owner.core(0)
    core1 = owner.core(1)
    core0.uart_init()
    core0.uart_tx_ring_base = 0x80
    _set_pc(core1, 0)

    assert _native_step(
        core1,
        on_output=lambda _port, _value: core0.uart_write8(0x06, 0),
    ) > 0

    assert core0.uart_drain_tx() == b"A"
    assert memory[0x80:0x88] == bytes(8)


def test_python_memory_scope_retains_the_borrowed_core_and_system_owner() -> None:
    owner = NativeSystemState(1)
    owner.attach_mem(bytearray([0x01]), 1)
    core = owner.core(0)
    owner_ref = weakref.ref(owner)
    core_ref = weakref.ref(core)
    scope = core._memory_use()
    del owner
    del core
    gc.collect()

    assert owner_ref() is not None
    assert core_ref() is not None

    scope.__exit__(None, None, None)
    del scope
    gc.collect()
    assert core_ref() is None
    assert owner_ref() is None


def test_out_of_order_scope_close_retains_the_root_mapping_lease() -> None:
    owner = NativeSystemState(2)
    owner.attach_mem(bytearray([0x01, 0x01]), 2)
    core0 = owner.core(0)
    core1 = owner.core(1)
    _set_pc(core0, 0)
    _set_pc(core1, 0)
    outer = core0._logical_memory_use()
    inner = core0._logical_memory_use()

    # Closing the root first must unlink only that owner.  The borrowed child
    # keeps both the shared mutex and the mapping-wide execution flag alive.
    outer.__exit__(None, None, None)
    with pytest.raises(RuntimeError, match="already executing"):
        _native_step(core1)

    # The child's logical permission remains usable exactly once.
    assert _native_step(core0) == 1
    with pytest.raises(RuntimeError, match="already executing"):
        _native_step(core0)
    with pytest.raises(RuntimeError, match="already executing"):
        _native_step(core1)

    inner.__exit__(None, None, None)
    assert _native_step(core1) == 1


def test_callback_retained_scope_keeps_native_root_ownership_alive() -> None:
    owner = NativeSystemState(2)
    owner.attach_mem(bytearray([0x91, 0x01]), 2)
    core0 = owner.core(0)
    core1 = owner.core(1)
    _set_pc(core0, 0)
    _set_pc(core1, 1)
    retained = []

    assert _native_step(
        core0,
        on_output=lambda _port, _value: retained.append(
            core0._memory_use()
        ),
    ) > 0
    assert len(retained) == 1

    # CPUExecutionGuard has unwound, but the escaped child still owns its
    # shared root lease and must exclude every core until explicitly closed.
    with pytest.raises(RuntimeError, match="already executing"):
        _native_step(core1)
    retained.pop().__exit__(None, None, None)
    assert _native_step(core1) == 1


def test_python_fallback_holds_the_system_one_worker_scope() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = system.cores
    system._shared_mem[0] = 0x01  # NOP for the post-fallback probe
    _set_pc(core0._cs, 0)
    _set_pc(core1._cs, 0)
    core0_pc = core0.pc
    core0_cycles = core0.cycle_count
    core1_pc = core1.pc
    core1_cycles = core1.cycle_count
    fallback = core0._get_fallback()
    rejected = []

    def fallback_step():
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core0._cs)
        rejected.append("same-core execution")
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core1._cs)
        rejected.append("cross-core execution")
        return 7

    fallback.step = fallback_step
    assert core0._step_python_fallback() == 7
    assert rejected == ["same-core execution", "cross-core execution"]
    assert core0.pc == core0_pc
    assert core0.cycle_count == core0_cycles
    assert core1.pc == core1_pc
    assert core1.cycle_count == core1_cycles
    assert _native_step(core0._cs) == 1
    assert _native_step(core1._cs) == 1


def test_run_preserves_overridable_step_and_trap_dispatch() -> None:
    cpu = Megapad64(mem_size=64)
    cpu.mem[0] = 0x01
    cpu.pc = 0
    cpu.ivt_base = 8
    calls = []

    def overridden_step():
        calls.append("step")
        raise TrapError(7)

    def overridden_trap(ivec_id: int) -> None:
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(cpu._cs)
        calls.append(("trap", ivec_id))

    cpu.step = overridden_step
    cpu._trap = overridden_trap

    assert cpu.run(max_steps=1) == 0
    assert calls == ["step", ("trap", 7)]
    assert _native_step(cpu._cs) == 1


def test_native_batch_handoff_keeps_one_scope_through_python_fallback() -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = system.cores
    program = assemble("out1\nt.fma")
    core0.load_bytes(0, program)
    core0.regs[core0.xsel] = 0x80
    core0.mem[0x80] = 0xA5
    core0.pc = 0
    core1.mem[0x40] = 0x01  # NOP for exclusion and post-scope probes
    core1.pc = 0x40
    core1_pc = core1.pc
    core1_cycles = core1.cycle_count
    fallback = core0._get_fallback()
    rejected = []

    def reject_sibling(label: str) -> None:
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core1._cs)
        assert core1.pc == core1_pc
        assert core1.cycle_count == core1_cycles
        rejected.append(label)

    core0.on_output = lambda _port, _value: reject_sibling(
        "native dispatch"
    )

    def fallback_step():
        reject_sibling("Python continuation")
        return 7

    fallback.step = fallback_step

    assert core0.run_steps(2) == (2, 0)
    assert rejected == ["native dispatch", "Python continuation"]
    assert _native_step(core1._cs) == 1


@pytest.mark.parametrize("operation", ["step", "run_batch"])
def test_system_trap_catch_stays_inside_the_core_operation_scope(
    operation: str,
) -> None:
    system = MegapadSystem(
        ram_size=256,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = system.cores
    core0.ivt_base = 8
    core0.load_bytes(0, assemble("t.fma"))
    core0.pc = 0
    core1.halted = True
    core1.mem[0x40] = 0x01
    core1.pc = 0x40
    core0_pc = core0.pc
    core0_cycles = core0.cycle_count
    core1_pc = core1.pc
    core1_cycles = core1.cycle_count
    fallback = core0._get_fallback()
    rejected = []

    def raise_trap():
        raise TrapError(7)

    def deliver_trap(ivec_id: int) -> None:
        assert ivec_id == 7
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core0._cs)
        rejected.append("same-core trap delivery")
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core1._cs)
        rejected.append("cross-core trap delivery")

    fallback.step = raise_trap
    core0._trap = deliver_trap
    if operation == "step":
        assert system.step() == 1
    else:
        assert system.run_batch(1) == 1

    assert rejected == [
        "same-core trap delivery",
        "cross-core trap delivery",
    ]
    assert core0.pc == core0_pc
    assert core0.cycle_count == core0_cycles
    assert core1.pc == core1_pc
    assert core1.cycle_count == core1_cycles
    core0.mem[0x41] = 0x01
    core0.pc = 0x41
    assert _native_step(core0._cs) == 1
    core1.halted = False
    assert _native_step(core1._cs) == 1


def test_system_step_trap_catch_masks_an_unused_override_permission() -> None:
    system = MegapadSystem(
        ram_size=64,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    core0, core1 = system.cores
    core0.mem[0] = 0x01
    core0.pc = 0
    core0.ivt_base = 8
    core1.halted = True
    rejected = []

    def raise_before_native(*_args, **_kwargs):
        raise TrapError(7)

    def overridden_trap(ivec_id: int) -> None:
        assert ivec_id == 7
        with pytest.raises(RuntimeError, match="already executing"):
            _native_step(core0._cs)
        rejected.append("same-core trap continuation")

    core0._trap = overridden_trap
    core0.step = raise_before_native
    assert system.step() == 1

    assert rejected == ["same-core trap continuation"]
    assert core0.pc == 0
    assert core0.cycle_count == 0
    assert _native_step(core0._cs) == 1


def test_megapad_system_wrappers_retain_the_central_python_buffers() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=64,
        ext_mem_size=32,
        vram_size=16,
    )

    assert system._native_system.mappings_sealed
    assert system._native_system.mem_buffer is system._shared_mem
    assert system._native_system.hbw_buffer is system._hbw_mem
    assert system._native_system.ext_mem_buffer is system._ext_mem
    assert system._native_system.vram_buffer is system._vram_mem
    for cpu in system.cores:
        assert cpu.mem is system._shared_mem
        assert cpu._hbw_buf is system._hbw_mem
        assert cpu._ext_mem_buf is system._ext_mem
        assert cpu._vram_buf is system._vram_mem

    original = system.cores[0].mem
    with pytest.raises(RuntimeError, match="system-owned CPUState"):
        system.cores[0].mem = bytearray(8192)
    assert system.cores[0].mem is original
    assert system.cores[1].mem is original
    with pytest.raises(RuntimeError, match="owned by SystemState"):
        system.cores[0].mem_size = 16
    assert system.cores[0].mem_size == system.cores[1].mem_size == 4096

    original_regions = (
        ("attach_hbw", "_hbw_buf", 0x1000),
        ("attach_ext_mem", "_ext_mem_buf", 0x2000),
        ("attach_vram", "_vram_buf", 0x3000),
    )
    for method_name, owner_attr, base in original_regions:
        original_buffer = getattr(system.cores[0], owner_attr)
        with pytest.raises(RuntimeError, match="system-owned CPUState"):
            getattr(system.cores[0], method_name)(bytearray(8), base, 8)
        assert getattr(system.cores[0], owner_attr) is original_buffer
        assert getattr(system.cores[1], owner_attr) is original_buffer
