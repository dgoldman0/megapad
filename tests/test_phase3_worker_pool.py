"""Phase 3 element 1 contracts for fixed persistent native worker lanes."""

from __future__ import annotations

import array
import gc
import weakref

import pytest

from accel_wrapper import NativeSystemState
from asm import assemble
from system import MegapadSystem


def _pool_diagnostics(owner: NativeSystemState) -> dict:
    return dict(owner._worker_pool_diagnostics())


def _pool_lifecycle(owner: NativeSystemState) -> dict:
    diagnostics = _pool_diagnostics(owner)
    return {
        key: diagnostics[key]
        for key in (
            "worker_count",
            "auxiliary_worker_count",
            "live_auxiliary_workers",
            "launch_count",
            "inline_reference",
        )
    }


def _make_private_compute_system(worker_count: int) -> MegapadSystem:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
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
    return system


def _private_compute_signature(worker_count: int) -> tuple:
    system = _make_private_compute_system(worker_count)
    lifecycle_before = _pool_lifecycle(system._native_system)
    stats = system.run_batch_stats(2_001)

    assert _pool_lifecycle(system._native_system) == lifecycle_before

    signature = (
        stats.instructions_executed,
        stats.system_cycles_advanced,
        stats.per_core_instructions,
        stats.per_core_cycles,
        stats.per_core_dispatches,
        stats.per_core_stop_reasons,
        system._scheduler_cursor,
        tuple(
            (
                cpu.pc,
                cpu.regs[1],
                cpu.cycle_count,
                cpu.halted,
                cpu.idle,
            )
            for cpu in system.cores
        ),
    )

    system.boot(entry=0)
    assert _pool_lifecycle(system._native_system) == lifecycle_before
    return signature


def test_worker_count_defaults_to_inline_reference() -> None:
    owner = NativeSystemState(1)
    system = MegapadSystem(
        ram_size=4096,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )

    expected_lifecycle = {
        "worker_count": 1,
        "auxiliary_worker_count": 0,
        "live_auxiliary_workers": 0,
        "launch_count": 0,
        "inline_reference": True,
    }
    assert owner.worker_count == system.worker_count == 1
    assert _pool_diagnostics(owner)["schema_version"] == 1
    assert _pool_diagnostics(system._native_system)["schema_version"] == 1
    assert _pool_lifecycle(owner) == expected_lifecycle
    assert _pool_lifecycle(system._native_system) == expected_lifecycle

    with pytest.raises(AttributeError):
        system.worker_count = 2


@pytest.mark.parametrize(
    "worker_count",
    [-(1 << 31), -1, 0, 3, 5, 8, 1 << 100],
)
def test_worker_count_rejects_nonfixed_integer_values(
    worker_count: int,
) -> None:
    with pytest.raises(ValueError, match="exactly 1, 2, or 4"):
        NativeSystemState(1, worker_count=worker_count)
    with pytest.raises(ValueError, match="exactly 1, 2, or 4"):
        MegapadSystem(
            ram_size=4096,
            hbw_size=0,
            ext_mem_size=0,
            vram_size=0,
            worker_count=worker_count,
        )


@pytest.mark.parametrize("worker_count", [True, 1.0, "2", None])
def test_native_and_facade_reject_noninteger_worker_counts(
    worker_count,
) -> None:
    with pytest.raises(TypeError, match="must be an integer"):
        NativeSystemState(1, worker_count=worker_count)
    with pytest.raises(TypeError, match="must be an integer"):
        MegapadSystem(
            ram_size=4096,
            hbw_size=0,
            ext_mem_size=0,
            vram_size=0,
            worker_count=worker_count,
        )


@pytest.mark.parametrize(
    ("worker_count", "auxiliary_worker_count"),
    [(2, 1), (4, 3)],
)
def test_fixed_worker_pool_starts_once_and_survives_warm_boot(
    worker_count: int,
    auxiliary_worker_count: int,
) -> None:
    system = _make_private_compute_system(worker_count)
    expected_lifecycle = {
        "worker_count": worker_count,
        "auxiliary_worker_count": auxiliary_worker_count,
        "live_auxiliary_workers": auxiliary_worker_count,
        "launch_count": auxiliary_worker_count,
        "inline_reference": False,
    }

    assert system.worker_count == worker_count
    assert _pool_lifecycle(system._native_system) == expected_lifecycle

    system.run_batch_stats(2_001)
    system.boot(entry=0)
    system.run_batch_stats(1)

    assert _pool_lifecycle(system._native_system) == expected_lifecycle


def test_worker_pool_lifetime_follows_a_borrowed_core_and_exporter() -> None:
    owner = NativeSystemState(1, worker_count=4)
    exported = array.array("B", [0x01])
    exported_ref = weakref.ref(exported)
    owner.attach_mem(exported, len(exported))
    core = owner.core(0)
    owner_ref = weakref.ref(owner)

    assert _pool_diagnostics(owner)["live_auxiliary_workers"] == 3

    del owner
    del exported
    gc.collect()

    assert owner_ref() is not None
    assert exported_ref() is not None
    assert core.mem_size == 1

    del core
    gc.collect()

    assert owner_ref() is None
    assert exported_ref() is None


def test_helper_pool_does_not_hide_the_facade_callback_cycle() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=4,
    )
    owner_ref = weakref.ref(system._native_system)
    core_ref = weakref.ref(system.cores[0]._cs)
    nic_ref = weakref.ref(system.nic)

    assert _pool_lifecycle(system._native_system)[
        "live_auxiliary_workers"
    ] == 3

    del system
    gc.collect()

    assert owner_ref() is None
    assert core_ref() is None
    assert nic_ref() is None


def test_fixed_worker_modes_preserve_the_one_worker_reference_result() -> None:
    signatures = {
        worker_count: _private_compute_signature(worker_count)
        for worker_count in (1, 2, 4)
    }

    assert signatures[2] == signatures[1]
    assert signatures[4] == signatures[1]
    assert signatures[1][0:7] == (
        2_001,
        1_501,
        (1_001, 1_000),
        (1_501, 1_500),
        (2, 1),
        (
            (2, 0, 0, 0, 0, 0, 0),
            (1, 0, 0, 0, 0, 0, 0),
        ),
        1,
    )
