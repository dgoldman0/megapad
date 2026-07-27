"""Phase 5 production rollout policy and entry-point contracts."""

from __future__ import annotations

import pytest

import system as system_module
from system import MegapadSystem


@pytest.mark.parametrize(
    ("execution_cores", "host_cpus", "expected"),
    (
        (1, 64, 1),
        (2, 64, 2),
        (3, 64, 4),
        (16, 64, 4),
        (16, 1, 1),
        (16, 2, 2),
        (16, 3, 4),
    ),
)
def test_auto_lane_policy_respects_guest_and_host_capacity(
    execution_cores: int,
    host_cpus: int,
    expected: int,
) -> None:
    assert system_module._resolve_worker_count(
        None,
        execution_cores,
        host_cpu_count=host_cpus,
    ) == expected


@pytest.mark.parametrize("worker_count", (1, 2, 4))
def test_explicit_lane_width_is_never_silently_clamped(
    worker_count: int,
) -> None:
    assert system_module._resolve_worker_count(
        worker_count,
        total_execution_cores=1,
        host_cpu_count=1,
    ) == worker_count


def test_host_capacity_prefers_process_affinity(monkeypatch) -> None:
    monkeypatch.setattr(
        system_module.os,
        "sched_getaffinity",
        lambda _pid: {2, 4},
    )
    monkeypatch.setattr(system_module.os, "cpu_count", lambda: 64)

    assert system_module._available_host_cpu_count() == 2


@pytest.mark.parametrize(
    ("full_cores", "clusters", "expected"),
    (
        (1, 0, 1),
        (2, 0, 2),
        (3, 0, 4),
        (1, 1, 4),
    ),
)
def test_system_auto_width_is_resolved_once_from_advertised_topology(
    monkeypatch,
    full_cores: int,
    clusters: int,
    expected: int,
) -> None:
    monkeypatch.setattr(
        system_module,
        "_available_host_cpu_count",
        lambda: 64,
    )
    system = MegapadSystem(
        ram_size=4096,
        num_cores=full_cores,
        num_clusters=clusters,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )

    assert system.worker_count == expected
    with pytest.raises(AttributeError):
        system.worker_count = 1


def test_explicit_one_lane_remains_the_helper_free_reference() -> None:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=4,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=1,
    )

    diagnostics = dict(system._native_system._worker_pool_diagnostics())
    assert system.worker_count == 1
    assert diagnostics["auxiliary_worker_count"] == 0
    assert diagnostics["live_auxiliary_workers"] == 0
    assert diagnostics["inline_reference"]
