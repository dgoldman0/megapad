#!/usr/bin/env python3
"""Measure deterministic guest-JIT source compilation and external-RAM code."""

from __future__ import annotations

import argparse
import json
import resource
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path

import bench_bios_kdos_load as bios_bench


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.guest-jit-source-load"
SCHEMA_VERSION = 1
START_MARKER = "[megapad-bench] guest JIT source start"
COMPILED_MARKER = "[megapad-bench] guest JIT source compiled"
COMPLETION_PREFIX = "[megapad-bench] guest JIT source complete value="
DEFAULT_DEFINITIONS = 2_048
DEFAULT_ITERATIONS = 2_000_000
DEFAULT_MAX_STEPS = 2_000_000_000
DEFAULT_TIMEOUT_S = 120.0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime-root", type=Path, default=ROOT)
    parser.add_argument(
        "--definitions",
        type=bios_bench._positive_int,
        default=DEFAULT_DEFINITIONS,
    )
    parser.add_argument(
        "--iterations",
        type=bios_bench._positive_int,
        default=DEFAULT_ITERATIONS,
    )
    parser.add_argument(
        "--max-steps",
        type=bios_bench._positive_int,
        default=DEFAULT_MAX_STEPS,
    )
    parser.add_argument(
        "--timeout",
        type=bios_bench._positive_float,
        default=DEFAULT_TIMEOUT_S,
    )
    parser.add_argument(
        "--host-profile",
        action="store_true",
        help="collect diagnostic host-DBT counters; do not cite its wall time",
    )
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--output", type=Path)
    return parser


def synthetic_source(definitions: int, iterations: int) -> bytes:
    """Return deterministic Forth that is compiled and executed in XMEM.

    Generated words combine ordinary inlines, literal folds, sparse bigrams,
    string literals, and data definitions in the external userland dictionary.
    The final loop repeatedly calls a safe external-cell load/update/store
    word, combining Forth compiler churn with the external-memory runtime
    shape that the raw-assembly Phase-0 circuits cannot represent.
    """

    lines = ["CREATE GJIT-CELL 0 ,"]
    base_body = (
        "OVER XOR NIP INVERT NEGATE SWAP OVER "
        "XOR NIP INVERT NEGATE SWAP OVER"
    )
    for index in range(definitions):
        body = base_body
        # Five literal/ALU pairs every eighth definition approximate the
        # fold density observed in the real Desktop compiler window.
        if index % 8 == 0:
            body += " 7 + 7 AND 7 OR 7 XOR 7 +"
        # One sparse true bigram keeps peephole activity representative
        # without turning this into a pathological fusion-only fixture.
        if index % 15 == 0:
            body += " DUP DROP"
        # String compilation and data entries keep this from being a repeated
        # colon-only loop while retaining bounded, deterministic source lines.
        if index % 4 == 0:
            body += f' S" deterministic-{index:06d}" 2DROP'
        lines.append(f": GJIT-{index:06d} {body} ;")
        if index % 2 == 0:
            lines.append(f"VARIABLE GJIT-V-{index:06d}")
    lines.extend(
        (
            ": GJIT-TOUCH GJIT-CELL @ 1+ GJIT-CELL ! ;",
            ": GJIT-SCALAR 123 INVERT NEGATE 7 XOR DROP ;",
            ": GJIT-RUN",
            f"    {iterations} 0 DO GJIT-TOUCH GJIT-SCALAR LOOP ;",
        )
    )
    return ("\n".join(lines) + "\n").encode("ascii")


def synthetic_autoexec() -> bytes:
    return (
        "\\ deterministic guest-JIT source benchmark\n"
        "ENTER-USERLAND\n"
        "JIT-ON JIT-RESET\n"
        f'." {START_MARKER}" CR\n'
        "KEY DROP\n"
        "REQUIRE guest-jit-source.f\n"
        f'." {COMPILED_MARKER}" CR\n'
        "GJIT-RUN\n"
        f'." {COMPLETION_PREFIX}" GJIT-CELL @ . CR\n'
    ).encode("ascii")


def _zero_fixture_mtime(runtime, fs, name: str) -> None:
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


def build_image(runtime, target: Path, definitions: int, iterations: int) -> dict:
    kdos_source = (runtime.root / "kdos.f").read_bytes()
    generated_source = synthetic_source(definitions, iterations)
    packed_kdos = runtime.pack_forth_source(kdos_source)
    packed_generated = runtime.pack_forth_source(generated_source)
    autoexec = synthetic_autoexec()
    fs = runtime.MP64FS(total_sectors=bios_bench.DESKTOP_MP64FS_SECTORS)
    fs.format()
    fs.inject_file(
        "kdos.f",
        packed_kdos,
        ftype=runtime.FTYPE_FORTH,
        flags=runtime.FLAG_SYSTEM,
    )
    fs.inject_file(
        "guest-jit-source.f",
        packed_generated,
        ftype=runtime.FTYPE_FORTH,
    )
    fs.inject_file("autoexec.f", autoexec, ftype=runtime.FTYPE_FORTH)
    for name in ("kdos.f", "guest-jit-source.f", "autoexec.f"):
        _zero_fixture_mtime(runtime, fs, name)
    fs.save(target)
    return {
        "bios_sha256": bios_bench._sha256_file(runtime.root / "bios.asm"),
        "kdos_source_sha256": bios_bench._sha256_bytes(kdos_source),
        "generated_source_sha256": bios_bench._sha256_bytes(generated_source),
        "generated_source_bytes": len(generated_source),
        "packed_generated_source_sha256": bios_bench._sha256_bytes(
            packed_generated
        ),
        "packed_generated_source_bytes": len(packed_generated),
        "autoexec_sha256": bios_bench._sha256_bytes(autoexec),
        "image_sha256": bios_bench._sha256_file(target),
        "image_bytes": target.stat().st_size,
        "mp64fs_fixture_mtime": 0,
    }


def _guest_cell(cpu, labels: dict[str, int], name: str) -> int:
    return int(cpu.mem_read64(labels[name]))


def run_benchmark(args: argparse.Namespace) -> dict:
    runtime = bios_bench._activate_runtime(args.runtime_root)
    with tempfile.TemporaryDirectory(prefix="megapad-guest-jit-source-") as td:
        image = Path(td) / "guest-jit-source.img"
        sources = build_image(
            runtime,
            image,
            args.definitions,
            args.iterations,
        )
        with runtime.MachineSession.from_bios(
            runtime.root / "bios.asm",
            storage_image=image,
            ram_size=bios_bench.DEFAULT_RAM_KIB << 10,
            ext_mem_size=bios_bench.DEFAULT_EXT_MEM_MIB << 20,
            vram_size=bios_bench.DEFAULT_VRAM_MIB << 20,
            num_cores=1,
            num_clusters=0,
            lanes=1,
            cols=bios_bench.DEFAULT_COLS,
            rows=bios_bench.DEFAULT_ROWS,
            batch_steps=bios_bench.DEFAULT_BATCH_STEPS,
            realtime_clock=False,
        ) as session:
            session.boot()
            setup_started = time.perf_counter()
            setup_report = session.run(
                max_steps=args.max_steps,
                wall_timeout_s=args.timeout,
                until_text=START_MARKER,
            )
            if START_MARKER not in session.raw_text():
                raise RuntimeError(
                    "guest-JIT fixture did not reach its timing boundary"
                )
            if not session.system.cpu.idle:
                boundary_report = session.run(
                    max_steps=1_000_000,
                    wall_timeout_s=5.0,
                )
                if not session.system.cpu.idle:
                    raise RuntimeError(
                        "guest-JIT fixture did not stop at its KEY boundary: "
                        f"{boundary_report.reason}"
                    )
            setup_elapsed_s = time.perf_counter() - setup_started
            session.send_text("x")
            if args.host_profile:
                session.system.start_host_profile()
            start_cycles = int(session.system.cpu.cycle_count)
            usage_before = resource.getrusage(resource.RUSAGE_SELF)
            started = time.perf_counter()
            report = session.run(
                max_steps=args.max_steps,
                wall_timeout_s=args.timeout,
            )
            elapsed_s = time.perf_counter() - started
            usage_after = resource.getrusage(resource.RUSAGE_SELF)
            host_profile = (
                bios_bench._json_native(session.system.stop_host_profile())
                if args.host_profile
                else None
            )
            cpu = session.system.cpu
            labels = session.bios_labels
            here = _guest_cell(cpu, labels, "var_here")
            latest = _guest_cell(cpu, labels, "var_latest")
            guest_jit = {
                name.removeprefix("var_jit_"): _guest_cell(cpu, labels, name)
                for name in (
                    "var_jit_enabled",
                    "var_jit_inlines",
                    "var_jit_bytes_saved",
                    "var_jit_folds",
                    "var_jit_peepholes",
                )
            }
            raw = session.raw_text().replace("\r", "")
            expected_value = str(args.iterations)
            completion = COMPLETION_PREFIX + expected_value
            xmem_base = session.system.ext_mem_base
            xmem_used = max(0, here - xmem_base)
            xmem_dictionary = bytes(session.system._ext_mem[:xmem_used])
            forbidden = (
                "Unknown word",
                "ABORT",
                "Bus fault",
                "Dictionary overflow",
            )
            validation = {
                "exact_single_full_core_topology": (
                    session.system.num_full_cores == 1
                    and session.system.num_clusters == 0
                    and session.system.worker_count == 1
                ),
                "normal_mp64fs_source_mode": True,
                "guest_jit_explicitly_enabled_before_fixture": True,
                "start_marker_seen": START_MARKER in raw,
                "compiled_marker_seen": COMPILED_MARKER in raw,
                "completion_value_exact": completion in raw,
                "returned_through_kdos_tail": (
                    report.reason == "idle" and cpu.idle and not cpu.halted
                ),
                "guest_jit_disabled_by_kdos_tail": guest_jit["enabled"] == 0,
                "guest_jit_compiled_fixture": (
                    guest_jit["inlines"] > 0 and guest_jit["bytes_saved"] > 0
                ),
                "external_dictionary_used": (
                    xmem_base <= latest < here <= session.system.ext_mem_end
                    and xmem_used > 0
                ),
                "no_known_guest_failure": not any(item in raw for item in forbidden),
                "host_profile_presence_matches_request": (
                    (host_profile is not None) == args.host_profile
                ),
            }
            if host_profile is not None:
                validation.update(
                    {
                        "host_profile_frozen": not host_profile["enabled"],
                        "profiled_steps_match_run": (
                            host_profile["counts"]["uncontended_steps"]
                            == report.steps
                        ),
                    }
                )
            measurement = {
                "reason": report.reason,
                "steps": int(report.steps),
                "cycles": int(cpu.cycle_count) - start_cycles,
                "batches": int(report.batches),
                "wall_time_s": elapsed_s,
                "million_steps_per_s": report.steps / elapsed_s / 1_000_000,
                "host_user_time_s": usage_after.ru_utime - usage_before.ru_utime,
                "host_system_time_s": usage_after.ru_stime - usage_before.ru_stime,
                "max_rss_kib": int(usage_after.ru_maxrss),
                "timing_qualified_for_throughput": not args.host_profile,
            }
            return {
                "schema": SCHEMA,
                "schema_version": SCHEMA_VERSION,
                "generated_at_utc": datetime.now(timezone.utc).isoformat(),
                "outcome": "pass" if all(validation.values()) else "fail",
                "repository": bios_bench._repository_provenance(runtime.root),
                "accelerator": {
                    "path": str(runtime.accelerator_path),
                    "sha256": bios_bench._sha256_file(runtime.accelerator_path),
                },
                "configuration": {
                    "definitions": args.definitions,
                    "iterations": args.iterations,
                    "max_steps": args.max_steps,
                    "timeout_s": args.timeout,
                    "host_profile": args.host_profile,
                    "guest_jit": "explicitly on for fixture compile and run",
                    "compiled_forth_cache": False,
                },
                "sources": sources,
                "measurement": measurement,
                "setup": {
                    "reason": setup_report.reason,
                    "steps": int(setup_report.steps),
                    "wall_time_s": setup_elapsed_s,
                    "excluded_from_throughput": True,
                    "boundary": "idle KEY after explicit guest JIT-ON and JIT-RESET",
                },
                "machine_state": {
                    "here": here,
                    "latest": latest,
                    "guest_jit": guest_jit,
                    "external_dictionary_bytes": xmem_used,
                    "external_dictionary_sha256": bios_bench._sha256_bytes(
                        xmem_dictionary
                    ),
                },
                "host_profile": host_profile,
                "host_profile_derived": bios_bench.profile_derived(host_profile),
                "output_tail": raw.splitlines()[-30:],
                "validation": validation,
            }


def print_human(result: dict) -> None:
    measurement = result["measurement"]
    guest_jit = result["machine_state"]["guest_jit"]
    print(f"Guest-JIT source load: {result['outcome'].upper()}")
    print(
        f"  {measurement['steps']:,} steps; {measurement['wall_time_s']:.3f}s; "
        f"{measurement['million_steps_per_s']:.3f} Msteps/s"
    )
    print(
        "  guest JIT fixture: "
        f"{guest_jit['inlines']:,} inlines, {guest_jit['folds']:,} folds, "
        f"{guest_jit['peepholes']:,} peepholes, "
        f"{guest_jit['bytes_saved']:,} bytes saved; final state "
        f"{guest_jit['enabled']}"
    )


def main() -> int:
    args = build_parser().parse_args()
    result = run_benchmark(args)
    payload = json.dumps(result, indent=2, sort_keys=True) + "\n"
    if args.output is not None:
        args.output.write_text(payload, encoding="utf-8")
    if args.json:
        print(payload, end="")
    else:
        print_human(result)
    return 0 if result["outcome"] == "pass" else 1


if __name__ == "__main__":
    raise SystemExit(main())
