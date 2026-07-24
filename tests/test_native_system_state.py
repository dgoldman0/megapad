"""Ownership and lifetime contracts for the first native SystemState slice."""

from __future__ import annotations

import gc
import weakref

import pytest

from accel_wrapper import Megapad64, NativeSystemState
from asm import assemble
from nic_backends import LoopbackBackend
from system import MegapadSystem


def test_native_system_state_validates_topology_and_core_bounds() -> None:
    owner = NativeSystemState(2, 6)

    assert owner.full_core_count == 2
    assert owner.all_core_count == 6
    assert (owner.core(0).core_id, owner.core(1).core_id) == (0, 1)
    assert owner.core(0).num_cores == owner.core(1).num_cores == 6

    with pytest.raises(IndexError):
        owner.core(-1)
    with pytest.raises(IndexError):
        owner.core(2)
    with pytest.raises(ValueError):
        NativeSystemState(0)
    with pytest.raises(ValueError):
        NativeSystemState(256)
    with pytest.raises(ValueError):
        NativeSystemState(2, 1)


def test_native_system_state_owns_stable_isolated_core_objects() -> None:
    owner = NativeSystemState(2)
    core0 = owner.core(0)
    core1 = owner.core(1)

    core0.set_reg(7, 0x1111)
    core1.set_reg(7, 0x2222)

    assert owner.core(0) is core0
    assert owner.core(1) is core1
    assert core0.get_reg(7) == 0x1111
    assert core1.get_reg(7) == 0x2222


def test_borrowed_core_view_retains_its_native_owner() -> None:
    def make_retained_core():
        owner = NativeSystemState(1)
        return owner.core(0)

    core = make_retained_core()
    gc.collect()

    core.set_reg(9, 0xCAFE)
    assert core.get_reg(9) == 0xCAFE


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
