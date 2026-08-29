#!/usr/bin/env python3
"""Measure the normal BIOS + KDOS source boot before any product modules."""

from __future__ import annotations

import argparse
import hashlib
import importlib
import json
import os
import platform
import resource
import subprocess
import sys
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.bios-kdos-source-load"
SCHEMA_VERSION = 8
COMPLETION_MARKER = "[megapad-bench] BIOS+KDOS source load complete"
KDOS_HRULE = "-" * 60
DEFAULT_MAX_STEPS = 2_000_000_000
DEFAULT_TIMEOUT_S = 120.0
DEFAULT_BATCH_STEPS = 500_000
DEFAULT_RAM_KIB = 1024
DEFAULT_EXT_MEM_MIB = 128
DEFAULT_VRAM_MIB = 4
DEFAULT_COLS = 280
DEFAULT_ROWS = 84
DESKTOP_MP64FS_SECTORS = 65536


def _positive_int(value: str) -> int:
    parsed = int(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def _positive_float(value: str) -> float:
    parsed = float(value)
    if parsed <= 0:
        raise argparse.ArgumentTypeError("must be greater than zero")
    return parsed


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--runtime-root",
        type=Path,
        default=ROOT,
        help="MegaPad checkout whose BIOS, KDOS, and accelerator are measured",
    )
    parser.add_argument(
        "--host-profile",
        action="store_true",
        help=(
            "collect opt-in DBT attribution in a diagnostic replay; "
            "profiled wall time is not clean throughput"
        ),
    )
    parser.add_argument("--max-steps", type=_positive_int, default=DEFAULT_MAX_STEPS)
    parser.add_argument("--timeout", type=_positive_float, default=DEFAULT_TIMEOUT_S)
    parser.add_argument(
        "--batch-steps", type=_positive_int, default=DEFAULT_BATCH_STEPS
    )
    parser.add_argument("--ram-kib", type=_positive_int, default=DEFAULT_RAM_KIB)
    parser.add_argument(
        "--ext-mem-mib", type=_positive_int, default=DEFAULT_EXT_MEM_MIB
    )
    parser.add_argument(
        "--vram-mib", type=_positive_int, default=DEFAULT_VRAM_MIB
    )
    parser.add_argument("--cols", type=_positive_int, default=DEFAULT_COLS)
    parser.add_argument("--rows", type=_positive_int, default=DEFAULT_ROWS)
    parser.add_argument("--json", action="store_true", help="write JSON to stdout")
    parser.add_argument("--output", type=Path, help="also write JSON to this path")
    return parser


def _path_within(path: Path, root: Path) -> bool:
    try:
        path.resolve().relative_to(root.resolve())
    except ValueError:
        return False
    return True


def _activate_runtime(runtime_root: Path) -> SimpleNamespace:
    root = runtime_root.expanduser().resolve()
    required = ("bios.asm", "kdos.f", "diskutil.py", "session.py")
    missing = [name for name in required if not (root / name).is_file()]
    if missing:
        raise RuntimeError(
            f"invalid MegaPad runtime root {root}: missing {', '.join(missing)}"
        )

    script_root = ROOT.resolve()
    retained_paths = []
    for entry in sys.path:
        try:
            resolved = Path(entry or os.curdir).resolve()
        except OSError:
            retained_paths.append(entry)
            continue
        if root != script_root and resolved == script_root:
            continue
        retained_paths.append(entry)
    sys.path[:] = [str(root), *retained_paths]

    accel = importlib.import_module("_mp64_accel")
    accelerator_path = Path(accel.__file__).resolve()
    if not _path_within(accelerator_path, root):
        raise RuntimeError(
            "loaded accelerator is outside --runtime-root: "
            f"{accelerator_path}"
        )
    diskutil = importlib.import_module("diskutil")
    session = importlib.import_module("session")
    system = importlib.import_module("system")
    for module in (diskutil, session, system):
        if not _path_within(Path(module.__file__), root):
            raise RuntimeError(
                f"loaded {module.__name__} outside --runtime-root: "
                f"{module.__file__}"
            )
    return SimpleNamespace(
        root=root,
        accel=accel,
        accelerator_path=accelerator_path,
        MP64FS=diskutil.MP64FS,
        FTYPE_FORTH=diskutil.FTYPE_FORTH,
        FLAG_SYSTEM=diskutil.FLAG_SYSTEM,
        SECTOR_SIZE=diskutil.SECTOR_SIZE,
        DIR_ENTRY_SIZE=diskutil.DIR_ENTRY_SIZE,
        pack_forth_source=diskutil.pack_forth_source,
        MachineSession=session.MachineSession,
    )


def _sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _git(runtime_root: Path, *args: str) -> str | None:
    completed = subprocess.run(
        ["git", "-C", str(runtime_root), *args],
        check=False,
        capture_output=True,
        text=True,
    )
    if completed.returncode != 0:
        return None
    return completed.stdout.strip()


def _repository_provenance(runtime_root: Path) -> dict:
    status = _git(runtime_root, "status", "--porcelain")
    return {
        "root": str(runtime_root),
        "commit": _git(runtime_root, "rev-parse", "HEAD"),
        "branch": _git(runtime_root, "branch", "--show-current"),
        "dirty": None if status is None else bool(status),
        "status_porcelain": None if status is None else status.splitlines(),
    }


def _cpu_model() -> str | None:
    cpuinfo = Path("/proc/cpuinfo")
    if not cpuinfo.is_file():
        return platform.processor() or None
    for line in cpuinfo.read_text(encoding="utf-8", errors="replace").splitlines():
        if line.startswith("model name") and ":" in line:
            return line.split(":", 1)[1].strip()
    return platform.processor() or None


def _build_boot_image(runtime: SimpleNamespace, target: Path) -> dict:
    kdos_source = (runtime.root / "kdos.f").read_bytes()
    packed_kdos = runtime.pack_forth_source(kdos_source)
    autoexec = (
        "\\ benchmark boundary: return normally so KDOS reaches final JIT-OFF\n"
        f'." {COMPLETION_MARKER}" CR\n'
    ).encode("ascii")
    fs = runtime.MP64FS(total_sectors=DESKTOP_MP64FS_SECTORS)
    fs.format()
    fs.inject_file(
        "kdos.f",
        packed_kdos,
        ftype=runtime.FTYPE_FORTH,
        flags=runtime.FLAG_SYSTEM,
    )
    fs.inject_file("autoexec.f", autoexec, ftype=runtime.FTYPE_FORTH)

    # inject_file records the host epoch second.  That timestamp is irrelevant
    # to execution but KDOS copies the directory into its dictionary cache,
    # making otherwise identical A/B images and final memory fingerprints
    # differ.  A benchmark fixture must be byte-reproducible.
    for name in ("kdos.f", "autoexec.f"):
        found = fs.find_file(name)
        if found is None:
            raise RuntimeError(f"benchmark image lost injected file {name!r}")
        slot, _entry = found
        offset = (
            fs.dir_start * runtime.SECTOR_SIZE
            + slot * runtime.DIR_ENTRY_SIZE
            + 36
        )
        fs.img[offset : offset + 4] = b"\x00" * 4

    fs.save(target)
    return {
        "bios_sha256": _sha256_file(runtime.root / "bios.asm"),
        "kdos_source_sha256": _sha256_bytes(kdos_source),
        "kdos_source_bytes": len(kdos_source),
        "packed_kdos_sha256": _sha256_bytes(packed_kdos),
        "packed_kdos_bytes": len(packed_kdos),
        "autoexec_sha256": _sha256_bytes(autoexec),
        "image_sha256": _sha256_file(target),
        "image_bytes": target.stat().st_size,
        "mp64fs_total_sectors": DESKTOP_MP64FS_SECTORS,
        "mp64fs_fixture_mtime": 0,
    }


def _json_native(value):
    if isinstance(value, dict):
        return {str(key): _json_native(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_native(item) for item in value]
    if isinstance(value, (bool, int, float, str)) or value is None:
        return value
    try:
        return int(value)
    except (TypeError, ValueError):
        return str(value)


def _ratio(numerator: int | float, denominator: int | float) -> float | None:
    return None if denominator == 0 else numerator / denominator


def profile_derived(profile: dict | None) -> dict | None:
    if profile is None:
        return None
    counts = profile["counts"]
    wall_ns = profile["wall_ns"]
    resident_rejections = (
        counts["uncontended_block_zero_instruction_rejections"]
        + counts["uncontended_block_one_instruction_rejections"]
    )
    return {
        "native_settlement_fraction": _ratio(
            counts["settle_round_native_calls"],
            counts["settle_round_calls"],
        ),
        "python_settlement_fraction": _ratio(
            counts["settle_round_python_calls"],
            counts["settle_round_calls"],
        ),
        "block_cache_hit_fraction": _ratio(
            counts["uncontended_block_hits"],
            counts["uncontended_block_lookups"],
        ),
        "block_lookups_per_1000_steps": (
            None
            if counts["uncontended_steps"] == 0
            else counts["uncontended_block_lookups"]
            / counts["uncontended_steps"]
            * 1_000
        ),
        "block_rejection_hit_fraction_of_lookups": _ratio(
            counts["uncontended_block_rejection_cache_hits"],
            counts["uncontended_block_lookups"],
        ),
        "block_build_attempt_fraction_of_lookups": _ratio(
            counts["uncontended_block_build_attempts"],
            counts["uncontended_block_lookups"],
        ),
        "block_rejection_cache_hit_fraction": _ratio(
            counts["uncontended_block_rejection_cache_hits"],
            (
                counts["uncontended_block_rejection_cache_hits"]
                + counts["uncontended_block_build_attempts"]
            ),
        ),
        "block_build_success_fraction": _ratio(
            counts["uncontended_block_builds"],
            counts["uncontended_block_build_attempts"],
        ),
        "resident_zero_instruction_rejection_fraction": _ratio(
            counts["uncontended_block_zero_instruction_rejections"],
            resident_rejections,
        ),
        "resident_one_instruction_rejection_fraction": _ratio(
            counts["uncontended_block_one_instruction_rejections"],
            resident_rejections,
        ),
        "decoded_block_step_fraction": _ratio(
            counts["uncontended_block_steps"],
            counts["uncontended_steps"],
        ),
        "jit_step_fraction": _ratio(
            counts["uncontended_jit_steps"],
            counts["uncontended_steps"],
        ),
        "jit_steps_per_execution": _ratio(
            counts["uncontended_jit_steps"],
            counts["uncontended_jit_executions"],
        ),
        "jit_compile_us_per_attempt": (
            None
            if counts["uncontended_jit_compile_attempts"] == 0
            else wall_ns["uncontended_jit_compile"]
            / counts["uncontended_jit_compile_attempts"]
            / 1_000
        ),
        "jit_arena_allocation_us_per_attempt": (
            None
            if (
                counts["uncontended_jit_arena_allocations"]
                + counts["uncontended_jit_arena_allocation_failures"]
            ) == 0
            else wall_ns["uncontended_jit_arena_allocation"]
            / (
                counts["uncontended_jit_arena_allocations"]
                + counts["uncontended_jit_arena_allocation_failures"]
            )
            / 1_000
        ),
        "jit_publication_us_per_compilation": (
            None
            if counts["uncontended_jit_compilations"] == 0
            else wall_ns["uncontended_jit_publication"]
            / counts["uncontended_jit_compilations"]
            / 1_000
        ),
        "jit_publication_fraction_of_compile_time": _ratio(
            wall_ns["uncontended_jit_publication"],
            wall_ns["uncontended_jit_compile"],
        ),
        "block_evictions_per_build": _ratio(
            counts["uncontended_block_evictions"],
            counts["uncontended_block_builds"],
        ),
        "plan_evictions_per_compilation": _ratio(
            counts["uncontended_jit_plan_evictions"],
            counts["uncontended_jit_compilations"],
        ),
        "slot_rewrites_per_publication": _ratio(
            counts["uncontended_jit_slot_rewrites"],
            counts["uncontended_jit_slot_publications"],
        ),
        "average_jit_code_bytes": _ratio(
            counts["uncontended_jit_code_bytes"],
            counts["uncontended_jit_compilations"],
        ),
    }


def _profile_rejection_cache_validation(profile: dict) -> dict[str, bool]:
    counts = profile["counts"]
    metadata = profile["single_core_block_rejection_cache"]
    attempts = counts["uncontended_block_build_attempts"]
    zero_rejections = counts[
        "uncontended_block_zero_instruction_rejections"
    ]
    one_rejections = counts[
        "uncontended_block_one_instruction_rejections"
    ]
    stores = counts["uncontended_block_rejection_cache_stores"]
    return {
        "block_rejection_cache_metadata_supported": (
            metadata["kind"] == "direct-mapped-exact-icache-span"
            and metadata["entries"] == 512
            and metadata["identity_bytes"] == 16
        ),
        "block_build_attempts_reconcile": (
            attempts
            == counts["uncontended_block_builds"]
            + counts["uncontended_block_nonresident_rejections"]
            + zero_rejections
            + one_rejections
        ),
        "block_rejection_cache_stores_reconcile": (
            stores == zero_rejections + one_rejections
        ),
        "block_rejection_cache_replacements_are_bounded": (
            counts["uncontended_block_rejection_cache_replacements"]
            <= stores
        ),
        "block_rejection_activity_reconciles_with_misses": (
            counts["uncontended_block_rejection_cache_hits"] + attempts
            == counts["uncontended_block_misses"]
        ),
    }


def _output_lines(raw: str) -> list[str]:
    """Return every substantive boot line without hiding diagnostic text."""

    return [line for line in raw.replace("\r", "").splitlines() if line]


def _expected_output_lines(ram_bytes: int) -> list[str]:
    """Exact normal transcript for this deliberately minimal boot image.

    BIOS FSLOAD historically continues after some per-line evaluator errors.
    Requiring the whole transcript, rather than matching a sample of known
    diagnostics, makes any additional BIOS, KDOS, ABORT\", or fault output a
    validation failure while the source and harness hashes explain which
    transcript contract was measured.
    """

    return [
        "Megapad-64 Forth BIOS v1.0",
        f"RAM: {ram_bytes:08X} bytes",
        " ok",
        KDOS_HRULE,
        "  KDOS v1.1 — Kernel Dashboard OS",
        KDOS_HRULE,
        " Type HELP for commands, HELP <word> for details.",
        " Type SCREENS for interactive TUI (or N SCREEN for screen N).",
        " Type TOPICS or LESSONS for documentation.",
        " MP64FS loaded",
        " Running autoexec.f...",
        COMPLETION_MARKER,
        "> ",
    ]


def run_benchmark(args: argparse.Namespace) -> dict:
    runtime = _activate_runtime(args.runtime_root)
    setup_started = time.perf_counter()
    with tempfile.TemporaryDirectory(prefix="megapad-bios-kdos-") as directory:
        image_path = Path(directory) / "bios-kdos.img"
        sources = _build_boot_image(runtime, image_path)
        with runtime.MachineSession.from_bios(
            runtime.root / "bios.asm",
            storage_image=image_path,
            ram_size=args.ram_kib << 10,
            ext_mem_size=args.ext_mem_mib << 20,
            vram_size=args.vram_mib << 20,
            num_cores=1,
            num_clusters=0,
            lanes=1,
            cols=args.cols,
            rows=args.rows,
            batch_steps=args.batch_steps,
            realtime_clock=False,
        ) as session:
            setup_elapsed_s = time.perf_counter() - setup_started
            session.boot()
            profile_started = False
            if args.host_profile:
                if not hasattr(session.system, "start_host_profile"):
                    raise RuntimeError(
                        "--host-profile requires the public host-profile API"
                    )
                session.system.start_host_profile()
                profile_started = True

            usage_before = resource.getrusage(resource.RUSAGE_SELF)
            started = time.perf_counter()
            run_error = None
            report = None
            try:
                report = session.run(
                    max_steps=args.max_steps,
                    wall_timeout_s=args.timeout,
                )
            except Exception as exc:
                run_error = f"{type(exc).__name__}: {exc}"
            elapsed_s = time.perf_counter() - started
            usage_after = resource.getrusage(resource.RUSAGE_SELF)
            host_profile = (
                _json_native(session.system.stop_host_profile())
                if profile_started
                else None
            )

            cpu = session.system.cpu
            raw = session.raw_text()
            output_lines = _output_lines(raw)
            expected_output_lines = _expected_output_lines(args.ram_kib << 10)
            transcript_matches = output_lines == expected_output_lines
            labels = session.bios_labels
            here = int(cpu.mem_read64(labels["var_here"]))
            latest = int(cpu.mem_read64(labels["var_latest"]))
            jit_enabled = int(cpu.mem_read64(labels["var_jit_enabled"]))
            steps = 0 if report is None else int(report.steps)
            cycles = int(cpu.cycle_count)
            reason = "exception" if report is None else report.reason
            batches = 0 if report is None else int(report.batches)

            validation = {
                "normal_mp64fs_autoboot_source_mode": True,
                "exact_single_full_core_topology": (
                    session.system.num_full_cores == 1
                    and session.system.num_clusters == 0
                    and session.system.worker_count == 1
                ),
                "completion_marker_seen": COMPLETION_MARKER in raw,
                "kdos_tail_returned_to_bios_idle": (
                    reason == "idle" and cpu.idle and not cpu.halted
                ),
                "guest_jit_disabled_by_kdos_tail": jit_enabled == 0,
                "exact_error_free_boot_transcript": transcript_matches,
                "no_host_exception": run_error is None,
                "guest_made_progress": steps > 0 and cycles > 0,
                "host_profile_presence_matches_request": (
                    (host_profile is not None) == args.host_profile
                ),
            }
            if host_profile is not None:
                profile_counts = host_profile["counts"]
                jit_storage = host_profile["single_core_jit_storage"]
                jit_available = (
                    host_profile["single_core_jit_backend"] == "x86_64"
                )
                validation.update(
                    {
                        "host_profile_schema_supported": (
                            host_profile["schema_version"] == 14
                        ),
                        "settlement_routes_reconcile": (
                            profile_counts["settle_round_calls"]
                            == profile_counts[
                                "settle_round_native_calls"
                            ]
                            + profile_counts[
                                "settle_round_python_calls"
                            ]
                        ),
                        "host_profile_frozen": not host_profile["enabled"],
                        "profiled_steps_match_run": (
                            profile_counts["uncontended_steps"] == steps
                        ),
                        "jit_publications_match_compilations": (
                            profile_counts[
                                "uncontended_jit_slot_publications"
                            ]
                            == profile_counts["uncontended_jit_compilations"]
                        ),
                        "fresh_jit_arena_is_single_and_bounded": (
                            (
                                jit_storage["ready"]
                                and not jit_storage["failed"]
                                and profile_counts[
                                    "uncontended_jit_arena_allocations"
                                ]
                                == 1
                                and profile_counts[
                                    "uncontended_jit_arena_allocation_failures"
                                ]
                                == 0
                                and jit_storage["slot_count"] > 0
                                and jit_storage["mapped_bytes_per_alias"]
                                == jit_storage["slot_count"]
                                * jit_storage["slot_bytes"]
                                and profile_counts[
                                    "uncontended_jit_max_code_bytes"
                                ]
                                <= jit_storage["slot_bytes"]
                            )
                            if jit_available
                            else not jit_storage["ready"]
                        ),
                    }
                )
                validation.update(
                    _profile_rejection_cache_validation(host_profile)
                )

            measurement = {
                "reason": reason,
                "steps": steps,
                "cycles": cycles,
                "batches": batches,
                "wall_time_s": elapsed_s,
                "host_user_time_s": usage_after.ru_utime - usage_before.ru_utime,
                "host_system_time_s": usage_after.ru_stime - usage_before.ru_stime,
                "max_rss_kib": int(usage_after.ru_maxrss),
                "million_steps_per_s": steps / elapsed_s / 1_000_000,
                "million_cycles_per_s": cycles / elapsed_s / 1_000_000,
                "cycles_per_step": _ratio(cycles, steps),
                "raw_output_bytes": session.raw_output_end,
                "output_batches": session.output_batches,
                "byte_callbacks": session.output_byte_callbacks,
                "setup_time_s": setup_elapsed_s,
                "timing_qualified_for_throughput": not args.host_profile,
            }
            return {
                "schema": SCHEMA,
                "schema_version": SCHEMA_VERSION,
                "generated_at_utc": datetime.now(timezone.utc).isoformat(),
                "outcome": "pass" if all(validation.values()) else "fail",
                "repository": _repository_provenance(runtime.root),
                "harness": {
                    "path": str(Path(__file__).resolve()),
                    "sha256": _sha256_file(Path(__file__).resolve()),
                    "schema": SCHEMA,
                    "schema_version": SCHEMA_VERSION,
                    "repository": _repository_provenance(ROOT),
                },
                "accelerator": {
                    "path": str(runtime.accelerator_path),
                    "sha256": _sha256_file(runtime.accelerator_path),
                    "bytes": runtime.accelerator_path.stat().st_size,
                },
                "host": {
                    "platform": platform.platform(),
                    "python": platform.python_version(),
                    "cpu": _cpu_model(),
                },
                "configuration": {
                    "full_cores": 1,
                    "clusters": 0,
                    "lanes": 1,
                    "ram_kib": args.ram_kib,
                    "ext_mem_mib": args.ext_mem_mib,
                    "vram_mib": args.vram_mib,
                    "terminal": [args.cols, args.rows],
                    "batch_steps": args.batch_steps,
                    "max_steps": args.max_steps,
                    "timeout_s": args.timeout,
                    "host_profile": args.host_profile,
                },
                "measurement_semantics": {
                    "start": "after architectural reset, before first BIOS instruction",
                    "stop": (
                        "BIOS idle after packed kdos.f and marker-only autoexec "
                        "return through KDOS final JIT-OFF"
                    ),
                    "source_mode": "mp64fs-packed-forth-source",
                    "compiled_forth_cache": False,
                    "includes": [
                        "BIOS execution",
                        "MP64FS autoboot",
                        "complete KDOS source evaluation",
                        "KDOS filesystem and heap initialization",
                    ],
                    "excludes": [
                        "MegaPad optional system modules",
                        "Akashic",
                        "Desk",
                        "rich terminal",
                    ],
                    "profiled_timing_is_diagnostic": args.host_profile,
                },
                "sources": sources,
                "measurement": measurement,
                "machine_state": {
                    "idle": bool(cpu.idle),
                    "halted": bool(cpu.halted),
                    "here": here,
                    "latest": latest,
                    "guest_jit_enabled": jit_enabled,
                    "dictionary": {
                        "start": int(labels["dict_free"]),
                        "bytes": here - int(labels["dict_free"]),
                        "sha256": _sha256_bytes(
                            bytes(cpu.mem[int(labels["dict_free"]):here])
                        ),
                    },
                },
                "host_profile": host_profile,
                "host_profile_derived": profile_derived(host_profile),
                "errors": (
                    []
                    if transcript_matches
                    else [
                        "boot transcript differs from the exact error-free oracle"
                    ]
                )
                + ([run_error] if run_error is not None else []),
                "output_lines": output_lines,
                "expected_output_lines": expected_output_lines,
                "output_tail": raw.replace("\r", "").splitlines()[-30:],
                "validation": validation,
            }


def print_human(result: dict) -> None:
    measurement = result["measurement"]
    repository = result["repository"]
    print(f"BIOS + KDOS source load: {result['outcome'].upper()}")
    print(f"  revision: {repository['commit']}")
    print(
        f"  {measurement['steps']:,} steps; "
        f"{measurement['cycles']:,} cycles; "
        f"{measurement['wall_time_s']:.3f}s; "
        f"{measurement['million_steps_per_s']:.3f} Msteps/s"
    )
    print(
        "  timing: "
        + (
            "diagnostic host-profile replay"
            if result["configuration"]["host_profile"]
            else "clean unprofiled throughput"
        )
    )
    profile = result["host_profile"]
    if profile is not None:
        counts = profile["counts"]
        wall_ns = profile["wall_ns"]
        derived = result["host_profile_derived"]

        def percent(value: float | None) -> str:
            return "n/a" if value is None else f"{value:.1%}"

        def decimal(value: float | None) -> str:
            return "n/a" if value is None else f"{value:.3f}"

        print(
            "  settlement: "
            f"{counts['settle_round_calls']:,} logical; "
            f"{counts['settle_round_native_calls']:,} native "
            f"({percent(derived['native_settlement_fraction'])}); "
            f"{counts['settle_round_python_calls']:,} Python "
            f"({percent(derived['python_settlement_fraction'])})"
        )
        print(
            "  DBT: "
            f"{counts['uncontended_jit_compilations']:,} compilations; "
            f"{counts['uncontended_jit_steps']:,} JIT steps; "
            f"{counts['uncontended_block_evictions']:,} block evictions; "
            f"{counts['uncontended_jit_plan_evictions']:,} plan evictions; "
            f"{counts['uncontended_jit_slot_rewrites']:,} slot rewrites"
        )
        print(
            "  DBT block admission: "
            f"{decimal(derived['block_lookups_per_1000_steps'])} "
            "lookups/1k; "
            "cached rejection/build-attempt shares "
            f"{percent(derived['block_rejection_hit_fraction_of_lookups'])}/"
            f"{percent(derived['block_build_attempt_fraction_of_lookups'])}; "
            f"{counts['uncontended_block_rejection_cache_hits']:,} "
            "rejection-cache hits "
            f"({percent(derived['block_rejection_cache_hit_fraction'])}); "
            f"{counts['uncontended_block_builds']:,}/"
            f"{counts['uncontended_block_build_attempts']:,} builds "
            f"({percent(derived['block_build_success_fraction'])}); "
            "resident zero/one "
            f"{counts['uncontended_block_zero_instruction_rejections']:,}/"
            f"{counts['uncontended_block_one_instruction_rejections']:,}"
        )
        print(
            "  DBT compile/arena/publication: "
            f"{wall_ns['uncontended_jit_compile'] / 1e9:.3f}s / "
            f"{wall_ns['uncontended_jit_arena_allocation'] / 1e9:.3f}s / "
            f"{wall_ns['uncontended_jit_publication'] / 1e9:.3f}s"
        )
    failed = [name for name, ok in result["validation"].items() if not ok]
    if failed:
        print(f"  failed validation: {', '.join(failed)}")


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    result = run_benchmark(args)
    encoded = json.dumps(result, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        print_human(result)
        if args.output is not None:
            print(f"  JSON report: {args.output}")
    return 0 if result["outcome"] == "pass" else 1


if __name__ == "__main__":
    raise SystemExit(main())
