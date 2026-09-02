from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

import bench_compare_kdos as comparison


def test_parser_selects_balanced_fresh_process_defaults() -> None:
    args = comparison.build_parser().parse_args([])

    assert args.runtime_root == comparison.ROOT
    assert args.pairs == 10
    assert args.warmups == 1
    assert args.child_timeout == 180.0
    assert args.cpu is None
    assert args.emulator_harness == Path("bench_bios_kdos_load.py")
    assert args.simulator_harness == Path("bench_simulator_kdos_load.py")

    with pytest.raises(SystemExit):
        comparison.build_parser().parse_args(["--pairs", "3"])
    with pytest.raises(SystemExit):
        comparison.build_parser().parse_args(["--warmups", "-1"])


def test_python_resolution_preserves_a_virtualenv_symlink(tmp_path: Path) -> None:
    base = tmp_path / "base-python"
    base.write_text("#!/bin/sh\n", encoding="ascii")
    base.chmod(0o755)
    venv_python = tmp_path / "venv-python"
    venv_python.symlink_to(base)

    resolved = comparison._resolve_executable(str(venv_python))

    assert resolved == venv_python.absolute()
    assert resolved != base.resolve()


def test_recorded_orders_are_balanced_and_summary_uses_within_pair_ratios() -> None:
    assert [comparison._recorded_pair_order(index) for index in range(1, 5)] == [
        (comparison.EMULATOR, comparison.SIMULATOR),
        (comparison.SIMULATOR, comparison.EMULATOR),
        (comparison.EMULATOR, comparison.SIMULATOR),
        (comparison.SIMULATOR, comparison.EMULATOR),
    ]
    assert comparison._warmup_order(1) == (
        comparison.SIMULATOR,
        comparison.EMULATOR,
    )

    samples = []
    prepared = ((8.0, 2.0), (12.0, 3.0), (10.0, 2.0), (18.0, 3.0))
    process = ((10.0, 2.5), (14.0, 3.5), (12.0, 3.0), (20.0, 4.0))
    entries = enumerate(zip(prepared, process), start=1)
    for ordinal, (
        (emulator, simulator),
        (emulator_process, simulator_process),
    ) in entries:
        order = comparison._recorded_pair_order(ordinal)
        values = {
            comparison.EMULATOR: (emulator, emulator_process),
            comparison.SIMULATOR: (simulator, simulator_process),
        }
        for position, backend in enumerate(order, start=1):
            wall, process_wall = values[backend]
            samples.append(
                {
                    "backend": backend,
                    "ordinal": ordinal,
                    "position": position,
                    "measurement": {"wall_time_s": wall},
                    "process_wall_time_s": process_wall,
                }
            )

    summary = comparison._comparison_summary(samples)

    assert summary["prepared"]["paired_emulator_to_simulator_ratio"]["median"] == 4.5
    assert summary["prepared"]["simulator_faster_pairs"] == 4
    assert summary["fresh_process"]["paired_emulator_to_simulator_ratio"]["median"] == 4.0
    assert [pair["order"] for pair in summary["pairs"]] == [
        list(comparison._recorded_pair_order(index)) for index in range(1, 5)
    ]
    assert summary["prepared"]["by_position"][comparison.EMULATOR]["1"][
        "count"
    ] == 2
    assert summary["prepared"]["by_position"][comparison.EMULATOR]["2"][
        "count"
    ] == 2


def _child_report(root: Path, harness: Path) -> dict:
    accelerator = root / "_mp64_accel-test.so"
    accelerator.write_bytes(b"deterministic accelerator")
    source_values = {
        "bios_sha256": "e" * 64,
        "kdos_source_sha256": "a" * 64,
        "kdos_source_bytes": 341_355,
        "packed_kdos_sha256": "b" * 64,
        "packed_kdos_bytes": 222_049,
        "autoexec_sha256": "c" * 64,
        "image_sha256": "d" * 64,
        "image_bytes": 33_554_432,
        "mp64fs_total_sectors": 65_536,
        "mp64fs_fixture_mtime": 0,
    }
    return {
        "schema": comparison.EXPECTED_SCHEMAS[comparison.EMULATOR],
        "schema_version": comparison.EXPECTED_SCHEMA_VERSIONS[
            comparison.EMULATOR
        ],
        "outcome": "pass",
        "repository": {
            "root": str(root),
            "commit": "1" * 40,
            "branch": "simulator-runtime",
            "dirty": False,
        },
        "harness": {
            "path": str(harness),
            "sha256": hashlib.sha256(harness.read_bytes()).hexdigest(),
            "schema": comparison.EXPECTED_SCHEMAS[comparison.EMULATOR],
            "schema_version": 11,
        },
        "host": {
            "platform": "test-platform",
            "python": "3.test",
            "cpu": "test-cpu",
        },
        "accelerator": {
            "path": str(accelerator),
            "sha256": hashlib.sha256(accelerator.read_bytes()).hexdigest(),
            "bytes": accelerator.stat().st_size,
        },
        "sources": source_values,
        "configuration": {
            "full_cores": 1,
            "clusters": 0,
            "lanes": 1,
            "ram_kib": 1_024,
            "ext_mem_mib": 128,
            "vram_mib": 4,
            "terminal": [280, 84],
        },
        "measurement": {
            "wall_time_s": 1.25,
            "timing_qualified_for_throughput": True,
        },
        "measurement_semantics": {
            "start": "before first BIOS instruction",
            "stop": "idle after KDOS and autoexec",
            "source_mode": "mp64fs-packed-forth-source",
        },
        "validation": {"exact_state": True},
    }


def test_child_report_validation_requires_exact_harness_and_clean_provenance(
    tmp_path: Path,
) -> None:
    root = tmp_path / "runtime"
    root.mkdir()
    harness = root / "bench_bios_kdos_load.py"
    harness.write_text("# deterministic harness\n", encoding="utf-8")
    report = _child_report(root, harness)

    qualified = comparison._validate_child_report(
        report,
        backend=comparison.EMULATOR,
        runtime_root=root.resolve(),
        harness=harness.resolve(),
    )

    assert qualified["wall_time_s"] == 1.25
    assert qualified["repository"]["dirty"] is False
    assert qualified["sources"]["packed_kdos_bytes"] == 222_049
    assert qualified["accelerator"]["bytes"] == len(b"deterministic accelerator")

    report["repository"]["dirty"] = True
    with pytest.raises(comparison.ComparisonError, match="dirty repository"):
        comparison._validate_child_report(
            report,
            backend=comparison.EMULATOR,
            runtime_root=root.resolve(),
            harness=harness.resolve(),
        )

    report["repository"]["dirty"] = False
    report["harness"]["sha256"] = "0" * 64
    with pytest.raises(comparison.ComparisonError, match="harness hash"):
        comparison._validate_child_report(
            report,
            backend=comparison.EMULATOR,
            runtime_root=root.resolve(),
            harness=harness.resolve(),
        )
