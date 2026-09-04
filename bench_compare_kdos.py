#!/usr/bin/env python3
"""Compare emulator and simulator KDOS load speed in fresh processes.

The coordinator deliberately does not import either backend.  Every warm-up and
recorded sample is a new child process, and children are run one at a time in
position-balanced emulator/simulator pairs.  Each child remains responsible for
its own exact state oracle; this layer validates the shared source, repository,
host, and harness provenance before reporting a speed ratio.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import os
import platform
import shutil
import statistics
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Sequence


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.kdos-load-comparison"
SCHEMA_VERSION = 1
EMULATOR = "emulator"
SIMULATOR = "simulator"
BACKENDS = (EMULATOR, SIMULATOR)
EXPECTED_SCHEMAS = {
    EMULATOR: "megapad.bios-kdos-source-load",
    SIMULATOR: "megapad.simulator-kdos-source-load",
}
EXPECTED_SCHEMA_VERSIONS = {
    EMULATOR: 11,
    SIMULATOR: 1,
}
DEFAULT_HARNESSES = {
    EMULATOR: Path("bench_bios_kdos_load.py"),
    SIMULATOR: Path("bench_simulator_kdos_load.py"),
}
DEFAULT_PAIRS = 10
DEFAULT_WARMUPS = 1
DEFAULT_CHILD_TIMEOUT_S = 180.0
PORTABLE_SOURCE_KEYS = (
    "bios_sha256",
    "kdos_source_sha256",
    "kdos_source_bytes",
    "packed_kdos_sha256",
    "packed_kdos_bytes",
    "autoexec_sha256",
    "image_sha256",
    "image_bytes",
    "mp64fs_total_sectors",
    "mp64fs_fixture_mtime",
)
PORTABLE_CONFIGURATION_KEYS = (
    "full_cores",
    "clusters",
    "lanes",
    "ram_kib",
    "ext_mem_mib",
    "vram_mib",
    "terminal",
)


class ComparisonError(RuntimeError):
    """The paired comparison cannot produce qualified timing evidence."""


def _positive_int(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def _positive_even_int(value: str) -> int:
    parsed = _positive_int(value)
    if parsed % 2:
        raise argparse.ArgumentTypeError("must be even for exact AB/BA balance")
    return parsed


def _nonnegative_int(value: str) -> int:
    parsed = int(value)
    if parsed < 0:
        raise argparse.ArgumentTypeError("must not be negative")
    return parsed


def _positive_float(value: str) -> float:
    parsed = float(value)
    if not math.isfinite(parsed) or parsed <= 0:
        raise argparse.ArgumentTypeError("must be a finite value greater than zero")
    return parsed


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--runtime-root",
        type=Path,
        default=ROOT,
        help="MegaPad checkout measured by both child harnesses",
    )
    parser.add_argument(
        "--python",
        default=sys.executable,
        help="one Python executable used for both child harnesses",
    )
    parser.add_argument(
        "--emulator-harness",
        type=Path,
        default=DEFAULT_HARNESSES[EMULATOR],
    )
    parser.add_argument(
        "--simulator-harness",
        type=Path,
        default=DEFAULT_HARNESSES[SIMULATOR],
    )
    parser.add_argument(
        "--pairs",
        type=_positive_even_int,
        default=DEFAULT_PAIRS,
        help="recorded two-run pairs; must be even for exact position balance",
    )
    parser.add_argument(
        "--warmups",
        type=_nonnegative_int,
        default=DEFAULT_WARMUPS,
        help="unrecorded validation runs per backend",
    )
    parser.add_argument(
        "--child-timeout",
        type=_positive_float,
        default=DEFAULT_CHILD_TIMEOUT_S,
        metavar="SECONDS",
    )
    parser.add_argument(
        "--cpu",
        type=_nonnegative_int,
        help=(
            "pin every child to this logical CPU; otherwise the coordinator "
            "must itself have one-CPU affinity"
        ),
    )
    parser.add_argument(
        "--reports-dir",
        type=Path,
        help="retain child JSON reports outside the measured checkout",
    )
    parser.add_argument("--json", action="store_true", help="write JSON to stdout")
    parser.add_argument("--output", type=Path, help="also write aggregate JSON here")
    return parser


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _sha256_text(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def _path_within(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
    except ValueError:
        return False
    return True


def _resolve_executable(value: str) -> Path:
    candidate = Path(value).expanduser()
    if candidate.is_absolute() or candidate.parent != Path("."):
        absolute = Path(os.path.abspath(candidate))
        if not absolute.is_file() or not os.access(absolute, os.X_OK):
            raise ComparisonError(f"Python executable is unavailable: {absolute}")
        # Do not resolve this symlink.  A venv's python normally points at the
        # base interpreter, but its invoked path is what activates pyvenv.cfg.
        return absolute
    found = shutil.which(value)
    if found is None:
        raise ComparisonError(f"Python executable is unavailable: {value}")
    return Path(os.path.abspath(found))


def _resolve_harness(runtime_root: Path, value: Path, *, backend: str) -> Path:
    candidate = value.expanduser()
    resolved = (
        candidate.resolve()
        if candidate.is_absolute()
        else (runtime_root / candidate).resolve()
    )
    if not resolved.is_file():
        raise ComparisonError(f"{backend} harness is unavailable: {resolved}")
    if not _path_within(resolved, runtime_root):
        raise ComparisonError(
            f"{backend} harness must be inside --runtime-root: {resolved}"
        )
    return resolved


def _require_clean_runtime_root(runtime_root: Path) -> None:
    completed = subprocess.run(
        ["git", "-C", str(runtime_root), "status", "--porcelain"],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        raise ComparisonError(
            "cannot establish clean repository provenance: "
            f"{_tail(completed.stderr).strip()}"
        )
    if completed.stdout:
        raise ComparisonError(
            "qualified timing requires a clean repository; status is: "
            + completed.stdout.strip().replace("\n", "; ")
        )


def _selected_affinity(cpu: int | None) -> tuple[list[str], list[int]]:
    inherited = (
        sorted(os.sched_getaffinity(0))
        if hasattr(os, "sched_getaffinity")
        else None
    )
    if cpu is None:
        if inherited is None or len(inherited) != 1:
            raise ComparisonError(
                "timing requires one-CPU affinity; use --cpu N or launch the "
                "coordinator with taskset -c N"
            )
        return [], inherited

    if inherited is not None and cpu not in inherited:
        raise ComparisonError(
            f"requested CPU {cpu} is outside inherited affinity {inherited}"
        )
    taskset = shutil.which("taskset")
    if taskset is None:
        raise ComparisonError("--cpu requires the taskset executable")
    return [taskset, "-c", str(cpu)], [cpu]


def _require_mapping(value: object, *, label: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise ComparisonError(f"{label} must be a JSON object")
    return value


def _require_positive_seconds(value: object, *, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ComparisonError(f"{label} must be numeric")
    seconds = float(value)
    if not math.isfinite(seconds) or seconds <= 0:
        raise ComparisonError(f"{label} must be finite and greater than zero")
    return seconds


def _portable_subset(
    mapping: dict[str, Any],
    keys: Sequence[str],
    *,
    label: str,
) -> dict[str, Any]:
    missing = [key for key in keys if key not in mapping]
    if missing:
        raise ComparisonError(f"{label} is missing {', '.join(missing)}")
    return {key: mapping[key] for key in keys}


def _repository_identity(report: dict[str, Any], *, label: str) -> dict[str, Any]:
    repository = _require_mapping(
        report.get("repository"), label=f"{label} repository"
    )
    root = repository.get("root")
    commit = repository.get("commit")
    dirty = repository.get("dirty")
    if not isinstance(root, str) or not root:
        raise ComparisonError(f"{label} repository root is missing")
    if not isinstance(commit, str) or not commit:
        raise ComparisonError(f"{label} repository commit is missing")
    if not isinstance(dirty, bool):
        raise ComparisonError(f"{label} repository dirty flag is missing")
    if dirty:
        raise ComparisonError(
            f"{label} measured a dirty repository; commit or remove changes "
            "before collecting qualified timing"
        )
    return {
        "root": str(Path(root).resolve()),
        "commit": commit,
        "branch": repository.get("branch"),
        "dirty": dirty,
    }


def _validate_child_report(
    report: dict[str, Any],
    *,
    backend: str,
    runtime_root: Path,
    harness: Path,
) -> dict[str, Any]:
    label = f"{backend} child report"
    if report.get("schema") != EXPECTED_SCHEMAS[backend]:
        raise ComparisonError(
            f"{label} has unsupported schema {report.get('schema')!r}"
        )
    if report.get("schema_version") != EXPECTED_SCHEMA_VERSIONS[backend]:
        raise ComparisonError(
            f"{label} has unsupported schema version "
            f"{report.get('schema_version')!r}"
        )
    if report.get("outcome") != "pass":
        raise ComparisonError(f"{label} outcome is not pass")

    validation = _require_mapping(report.get("validation"), label=f"{label} validation")
    failed = [name for name, passed in validation.items() if passed is not True]
    if failed:
        raise ComparisonError(
            f"{label} has failed validation: {', '.join(sorted(failed))}"
        )

    measurement = _require_mapping(
        report.get("measurement"), label=f"{label} measurement"
    )
    wall_time_s = _require_positive_seconds(
        measurement.get("wall_time_s"), label=f"{label} wall_time_s"
    )
    if measurement.get("timing_qualified_for_throughput") is not True:
        raise ComparisonError(f"{label} does not qualify its wall time")

    repository = _repository_identity(report, label=label)
    if Path(repository["root"]).resolve() != runtime_root:
        raise ComparisonError(
            f"{label} measured {repository['root']}, expected {runtime_root}"
        )

    harness_report = _require_mapping(
        report.get("harness"), label=f"{label} harness"
    )
    reported_harness = harness_report.get("path")
    if not isinstance(reported_harness, str):
        raise ComparisonError(f"{label} harness path is missing")
    if Path(reported_harness).resolve() != harness:
        raise ComparisonError(
            f"{label} used harness {reported_harness}, expected {harness}"
        )
    expected_harness_sha256 = _sha256_file(harness)
    if harness_report.get("sha256") != expected_harness_sha256:
        raise ComparisonError(f"{label} harness hash does not match its file")
    if harness_report.get("schema") != EXPECTED_SCHEMAS[backend]:
        raise ComparisonError(f"{label} harness schema is inconsistent")
    if harness_report.get("schema_version") != EXPECTED_SCHEMA_VERSIONS[backend]:
        raise ComparisonError(f"{label} harness schema version is inconsistent")

    sources = _require_mapping(report.get("sources"), label=f"{label} sources")
    portable_sources = _portable_subset(
        sources,
        PORTABLE_SOURCE_KEYS,
        label=f"{label} sources",
    )
    configuration = _require_mapping(
        report.get("configuration"), label=f"{label} configuration"
    )
    portable_configuration = _portable_subset(
        configuration,
        PORTABLE_CONFIGURATION_KEYS,
        label=f"{label} configuration",
    )
    host = _require_mapping(report.get("host"), label=f"{label} host")
    for key in ("platform", "python", "cpu"):
        if key not in host:
            raise ComparisonError(f"{label} host is missing {key}")
    measurement_semantics = _require_mapping(
        report.get("measurement_semantics"),
        label=f"{label} measurement semantics",
    )
    for key in ("start", "stop", "source_mode"):
        value = measurement_semantics.get(key)
        if not isinstance(value, str) or not value:
            raise ComparisonError(
                f"{label} measurement semantics are missing {key}"
            )

    accelerator = None
    if backend == EMULATOR:
        accelerator_report = _require_mapping(
            report.get("accelerator"), label=f"{label} accelerator"
        )
        accelerator_path = accelerator_report.get("path")
        if not isinstance(accelerator_path, str):
            raise ComparisonError(f"{label} accelerator path is missing")
        resolved_accelerator = Path(accelerator_path).resolve()
        if not resolved_accelerator.is_file() or not _path_within(
            resolved_accelerator, runtime_root
        ):
            raise ComparisonError(
                f"{label} accelerator is unavailable or outside the runtime root"
            )
        accelerator_sha256 = _sha256_file(resolved_accelerator)
        if accelerator_report.get("sha256") != accelerator_sha256:
            raise ComparisonError(f"{label} accelerator hash does not match its file")
        accelerator_bytes = resolved_accelerator.stat().st_size
        if accelerator_report.get("bytes") != accelerator_bytes:
            raise ComparisonError(f"{label} accelerator size does not match its file")
        accelerator = {
            "path": str(resolved_accelerator),
            "sha256": accelerator_sha256,
            "bytes": accelerator_bytes,
        }

    return {
        "backend": backend,
        "wall_time_s": wall_time_s,
        "repository": repository,
        "harness": {
            "path": str(harness),
            "sha256": expected_harness_sha256,
            "schema": harness_report.get("schema"),
            "schema_version": harness_report.get("schema_version"),
        },
        "host": {key: host[key] for key in ("platform", "python", "cpu")},
        "sources": portable_sources,
        "configuration": portable_configuration,
        "measurement_semantics": measurement_semantics,
        "accelerator": accelerator,
    }


def _tail(value: str | bytes, *, limit: int = 4_000) -> str:
    if isinstance(value, bytes):
        value = value.decode("utf-8", errors="replace")
    return value if len(value) <= limit else value[-limit:]


def _run_child(
    *,
    backend: str,
    phase: str,
    ordinal: int,
    position: int,
    runtime_root: Path,
    python: Path,
    harness: Path,
    launcher: Sequence[str],
    report_path: Path,
    timeout_s: float,
) -> tuple[dict[str, Any], dict[str, Any]]:
    command = [
        *launcher,
        str(python),
        str(harness),
        "--runtime-root",
        str(runtime_root),
        "--json",
        "--output",
        str(report_path),
    ]
    environment = os.environ.copy()
    environment["PYTHONHASHSEED"] = "0"
    started = time.perf_counter()
    try:
        completed = subprocess.run(
            command,
            cwd=runtime_root,
            env=environment,
            check=False,
            capture_output=True,
            text=True,
            timeout=timeout_s,
        )
    except subprocess.TimeoutExpired as exc:
        raise ComparisonError(
            f"{phase} {ordinal} position {position} {backend} child exceeded "
            f"{timeout_s:.3f}s; stdout={_tail(exc.stdout or '')!r}; "
            f"stderr={_tail(exc.stderr or '')!r}"
        ) from exc
    process_wall_time_s = time.perf_counter() - started
    if completed.returncode != 0:
        raise ComparisonError(
            f"{phase} {ordinal} position {position} {backend} child exited "
            f"{completed.returncode}; stdout={_tail(completed.stdout)!r}; "
            f"stderr={_tail(completed.stderr)!r}"
        )
    if not report_path.is_file():
        raise ComparisonError(f"{backend} child did not write {report_path}")

    try:
        file_report = json.loads(report_path.read_text(encoding="utf-8"))
        stdout_report = json.loads(completed.stdout)
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise ComparisonError(f"{backend} child emitted invalid JSON: {exc}") from exc
    report = _require_mapping(file_report, label=f"{backend} report file")
    stdout_mapping = _require_mapping(stdout_report, label=f"{backend} stdout")
    if report != stdout_mapping:
        raise ComparisonError(f"{backend} stdout and --output reports differ")

    qualified = _validate_child_report(
        report,
        backend=backend,
        runtime_root=runtime_root,
        harness=harness,
    )
    sample = {
        "backend": backend,
        "phase": phase,
        "ordinal": ordinal,
        "position": position,
        "command": command,
        "process_wall_time_s": process_wall_time_s,
        "measurement": report["measurement"],
        "measurement_semantics": report["measurement_semantics"],
        "machine_state": report.get("machine_state"),
        "validation": report["validation"],
        "child_report_sha256": _sha256_file(report_path),
        "stdout_sha256": _sha256_text(completed.stdout),
        "stderr": completed.stderr,
        "environment": {"PYTHONHASHSEED": "0"},
    }
    return sample, qualified


def _assert_same(label: str, values: Sequence[object]) -> object:
    if not values:
        raise ComparisonError(f"no values supplied for {label}")
    first = values[0]
    if any(value != first for value in values[1:]):
        raise ComparisonError(f"child reports disagree on {label}")
    return first


def _validate_cross_sample_provenance(
    qualified: Sequence[dict[str, Any]],
) -> dict[str, Any]:
    repository = _assert_same(
        "repository provenance", [item["repository"] for item in qualified]
    )
    host = _assert_same("host provenance", [item["host"] for item in qualified])
    sources = _assert_same(
        "portable source provenance", [item["sources"] for item in qualified]
    )
    configuration = _assert_same(
        "portable configuration", [item["configuration"] for item in qualified]
    )
    for backend in BACKENDS:
        _assert_same(
            f"{backend} harness provenance",
            [
                item["harness"]
                for item in qualified
                if item["backend"] == backend
            ],
        )
        _assert_same(
            f"{backend} measurement semantics",
            [
                item["measurement_semantics"]
                for item in qualified
                if item["backend"] == backend
            ],
        )
    _assert_same(
        "emulator accelerator provenance",
        [
            item["accelerator"]
            for item in qualified
            if item["backend"] == EMULATOR
        ],
    )
    return {
        "repository": repository,
        "host": host,
        "sources": sources,
        "configuration": configuration,
    }


def _distribution(values: Sequence[float]) -> dict[str, float | int]:
    if not values:
        raise ComparisonError("cannot summarize an empty timing distribution")
    median = statistics.median(values)
    absolute_deviations = [abs(value - median) for value in values]
    return {
        "count": len(values),
        "min": min(values),
        "median": median,
        "max": max(values),
        "mean": statistics.fmean(values),
        "median_absolute_deviation": statistics.median(absolute_deviations),
    }


def _comparison_summary(samples: Sequence[dict[str, Any]]) -> dict[str, Any]:
    pairs: list[dict[str, Any]] = []
    for ordinal in sorted({int(sample["ordinal"]) for sample in samples}):
        members = [sample for sample in samples if sample["ordinal"] == ordinal]
        if len(members) != 2 or {member["backend"] for member in members} != set(BACKENDS):
            raise ComparisonError(f"recorded pair {ordinal} is incomplete")
        by_backend = {member["backend"]: member for member in members}
        emulator = by_backend[EMULATOR]
        simulator = by_backend[SIMULATOR]
        emulator_prepared = float(emulator["measurement"]["wall_time_s"])
        simulator_prepared = float(simulator["measurement"]["wall_time_s"])
        emulator_process = float(emulator["process_wall_time_s"])
        simulator_process = float(simulator["process_wall_time_s"])
        pairs.append(
            {
                "ordinal": ordinal,
                "order": [
                    member["backend"]
                    for member in sorted(members, key=lambda item: item["position"])
                ],
                "prepared_wall_time_s": {
                    EMULATOR: emulator_prepared,
                    SIMULATOR: simulator_prepared,
                },
                "prepared_emulator_to_simulator_ratio": (
                    emulator_prepared / simulator_prepared
                ),
                "process_wall_time_s": {
                    EMULATOR: emulator_process,
                    SIMULATOR: simulator_process,
                },
                "process_emulator_to_simulator_ratio": (
                    emulator_process / simulator_process
                ),
            }
        )

    prepared = {
        backend: [
            float(sample["measurement"]["wall_time_s"])
            for sample in samples
            if sample["backend"] == backend
        ]
        for backend in BACKENDS
    }
    process = {
        backend: [
            float(sample["process_wall_time_s"])
            for sample in samples
            if sample["backend"] == backend
        ]
        for backend in BACKENDS
    }
    prepared_ratios = [
        pair["prepared_emulator_to_simulator_ratio"] for pair in pairs
    ]
    process_ratios = [
        pair["process_emulator_to_simulator_ratio"] for pair in pairs
    ]

    def position_distribution(backend: str, field: str) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for position in (1, 2):
            values = [
                float(
                    sample["measurement"]["wall_time_s"]
                    if field == "prepared"
                    else sample["process_wall_time_s"]
                )
                for sample in samples
                if sample["backend"] == backend and sample["position"] == position
            ]
            result[str(position)] = _distribution(values)
        return result

    return {
        "pairs": pairs,
        "prepared": {
            "wall_time_s": {
                backend: _distribution(prepared[backend]) for backend in BACKENDS
            },
            "paired_emulator_to_simulator_ratio": _distribution(prepared_ratios),
            "ratio_of_backend_medians": (
                statistics.median(prepared[EMULATOR])
                / statistics.median(prepared[SIMULATOR])
            ),
            "simulator_faster_pairs": sum(ratio > 1 for ratio in prepared_ratios),
            "by_position": {
                backend: position_distribution(backend, "prepared")
                for backend in BACKENDS
            },
        },
        "fresh_process": {
            "wall_time_s": {
                backend: _distribution(process[backend]) for backend in BACKENDS
            },
            "paired_emulator_to_simulator_ratio": _distribution(process_ratios),
            "ratio_of_backend_medians": (
                statistics.median(process[EMULATOR])
                / statistics.median(process[SIMULATOR])
            ),
            "simulator_faster_pairs": sum(ratio > 1 for ratio in process_ratios),
            "by_position": {
                backend: position_distribution(backend, "process")
                for backend in BACKENDS
            },
        },
    }


def _recorded_pair_order(ordinal: int) -> tuple[str, str]:
    if isinstance(ordinal, bool) or not isinstance(ordinal, int) or ordinal < 1:
        raise ValueError("pair ordinal must be a positive integer")
    return (
        (EMULATOR, SIMULATOR)
        if ordinal % 2
        else (SIMULATOR, EMULATOR)
    )


def _warmup_order(ordinal: int) -> tuple[str, str]:
    first, second = _recorded_pair_order(ordinal)
    return second, first


def _run_comparison(args: argparse.Namespace, reports_dir: Path) -> dict[str, Any]:
    runtime_root = args.runtime_root.expanduser().resolve()
    if not runtime_root.is_dir():
        raise ComparisonError(f"runtime root is unavailable: {runtime_root}")
    _require_clean_runtime_root(runtime_root)
    if args.pairs % 2:
        # Keep this guard for programmatic callers which construct Namespace
        # directly instead of passing through argparse.
        raise ComparisonError("--pairs must be even for exact AB/BA balance")
    python = _resolve_executable(args.python)
    harnesses = {
        EMULATOR: _resolve_harness(
            runtime_root, args.emulator_harness, backend=EMULATOR
        ),
        SIMULATOR: _resolve_harness(
            runtime_root, args.simulator_harness, backend=SIMULATOR
        ),
    }
    launcher, affinity = _selected_affinity(args.cpu)
    reports_dir.mkdir(parents=True, exist_ok=True)

    warmup_samples: list[dict[str, Any]] = []
    recorded_samples: list[dict[str, Any]] = []
    qualified_reports: list[dict[str, Any]] = []

    for ordinal in range(1, args.warmups + 1):
        order = _warmup_order(ordinal)
        for position, backend in enumerate(order, start=1):
            report_path = reports_dir / (
                f"warmup-{ordinal:02d}-position-{position}-{backend}.json"
            )
            sample, qualified = _run_child(
                backend=backend,
                phase="warmup",
                ordinal=ordinal,
                position=position,
                runtime_root=runtime_root,
                python=python,
                harness=harnesses[backend],
                launcher=launcher,
                report_path=report_path,
                timeout_s=args.child_timeout,
            )
            warmup_samples.append(sample)
            qualified_reports.append(qualified)

    for ordinal in range(1, args.pairs + 1):
        order = _recorded_pair_order(ordinal)
        for position, backend in enumerate(order, start=1):
            report_path = reports_dir / (
                f"pair-{ordinal:02d}-position-{position}-{backend}.json"
            )
            sample, qualified = _run_child(
                backend=backend,
                phase="recorded",
                ordinal=ordinal,
                position=position,
                runtime_root=runtime_root,
                python=python,
                harness=harnesses[backend],
                launcher=launcher,
                report_path=report_path,
                timeout_s=args.child_timeout,
            )
            recorded_samples.append(sample)
            qualified_reports.append(qualified)

    shared = _validate_cross_sample_provenance(qualified_reports)
    summary = _comparison_summary(recorded_samples)
    backend_provenance = {}
    for backend in BACKENDS:
        item = next(
            qualified
            for sample, qualified in zip(
                [*warmup_samples, *recorded_samples], qualified_reports
            )
            if sample["backend"] == backend
        )
        backend_provenance[backend] = {
            "harness": item["harness"],
            "measurement_semantics": item["measurement_semantics"],
            "accelerator": item["accelerator"],
        }

    return {
        "schema": SCHEMA,
        "schema_version": SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "outcome": "pass",
        "coordinator": {
            "path": str(Path(__file__).resolve()),
            "sha256": _sha256_file(Path(__file__).resolve()),
            "python": str(python),
            "platform": platform.platform(),
        },
        "configuration": {
            "runtime_root": str(runtime_root),
            "recorded_pairs": args.pairs,
            "warmups_per_backend": args.warmups,
            "execution_order": "odd pairs emulator/simulator; even pairs simulator/emulator",
            "fresh_process_per_sample": True,
            "sequential": True,
            "affinity": affinity,
            "child_timeout_s": args.child_timeout,
            "child_environment": {"PYTHONHASHSEED": "0"},
        },
        "shared_provenance": shared,
        "backends": backend_provenance,
        "warmups": warmup_samples,
        "samples": recorded_samples,
        "comparison": summary,
        "measurement_semantics": {
            "prepared": (
                "backend-specific normal KDOS-ready interval: the emulator starts "
                "before its first BIOS instruction and includes BIOS plus MP64FS "
                "kdos.f transfer, while the simulator starts before its first checked "
                "KDOS line and semantically substitutes both operations"
            ),
            "prepared_work_scopes_are_identical": False,
            "prepared_ratio_interpretation": (
                "product-mode time-to-ready comparison that credits semantic "
                "substitution; not an execution-engine or per-instruction speedup"
            ),
            "fresh_process": (
                "coordinator wall interval around one new child process, including "
                "imports, backend construction, fixture setup, validation, and JSON"
            ),
            "fresh_process_is_diagnostic_only": True,
            "ratio": "emulator wall time divided by simulator wall time",
            "primary_statistic": (
                "median of within-pair prepared emulator/simulator ratios"
            ),
            "statistical_superiority_claim": False,
            "profiled_runs": False,
        },
    }


def run_comparison(args: argparse.Namespace) -> dict[str, Any]:
    if args.reports_dir is not None:
        reports_dir = args.reports_dir.expanduser().resolve()
        runtime_root = args.runtime_root.expanduser().resolve()
        if _path_within(reports_dir, runtime_root):
            raise ComparisonError(
                "--reports-dir must be outside --runtime-root so child reports "
                "cannot perturb repository provenance"
            )
        return _run_comparison(args, reports_dir)
    with tempfile.TemporaryDirectory(prefix="megapad-kdos-comparison-") as td:
        return _run_comparison(args, Path(td))


def print_human(result: dict[str, Any]) -> None:
    comparison = result["comparison"]
    prepared = comparison["prepared"]
    process = comparison["fresh_process"]
    pair_count = result["configuration"]["recorded_pairs"]
    print(f"KDOS emulator/simulator comparison: {result['outcome'].upper()}")
    print(
        f"  {pair_count} position-balanced pairs on CPU "
        f"{result['configuration']['affinity'][0]}"
    )
    print(
        "  prepared median: "
        f"emulator {prepared['wall_time_s'][EMULATOR]['median']:.6f}s; "
        f"simulator {prepared['wall_time_s'][SIMULATOR]['median']:.6f}s"
    )
    print(
        "  prepared emulator/simulator ratio: "
        f"{prepared['paired_emulator_to_simulator_ratio']['median']:.3f}x median; "
        f"faster in {prepared['simulator_faster_pairs']}/{pair_count} pairs"
    )
    print(
        "  diagnostic process ratio: "
        f"{process['paired_emulator_to_simulator_ratio']['median']:.3f}x median"
    )


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        result = run_comparison(args)
    except (ComparisonError, OSError) as exc:
        print(f"KDOS comparison failed: {exc}", file=sys.stderr)
        return 2
    encoded = json.dumps(result, indent=2, sort_keys=True)
    if args.output is not None:
        target = args.output.expanduser().resolve()
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        print_human(result)
        if args.output is not None:
            print(f"  JSON report: {args.output.expanduser().resolve()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
