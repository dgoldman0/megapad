#!/usr/bin/env python3
"""Measure an ordinary semantic KDOS source load before product modules."""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
import resource
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path
from types import SimpleNamespace

import bench_bios_kdos_load as bios_bench
import diskutil
from shared.cells import TRUE
from simulator.dictionary_index import (
    DICT_INDEX_AUTHORITATIVE,
    DICT_INDEX_BOUND,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.simulator-kdos-source-load"
SCHEMA_VERSION = 1
DEFAULT_RAM_KIB = bios_bench.DEFAULT_RAM_KIB
DEFAULT_EXT_MEM_MIB = bios_bench.DEFAULT_EXT_MEM_MIB
DEFAULT_HBW_MIB = 3
DEFAULT_VRAM_MIB = bios_bench.DEFAULT_VRAM_MIB
DEFAULT_COLS = bios_bench.DEFAULT_COLS
DEFAULT_ROWS = bios_bench.DEFAULT_ROWS

KDOS_LINES = 9_894
KDOS_BYTES = 341_355
KDOS_SHA256 = "99e71114ed141c14522d687a3bef3110ead94de7b0a055ae693c135a94772fb8"
SUBMITTED_LINES = 6_693
SUBMITTED_PAYLOAD_BYTES = 215_356
PACKED_KDOS_BYTES = 222_049
MAX_SUBMITTED_LINE = 99
CORE_WORDS = 319
KDOS_WORDS = 1_452
UNIQUE_BINDINGS = 1_764

STARTUP_BANNER = (
    b"\r\n"
    + b"-" * 60
    + b"\r\n"
    + b"  KDOS v1.1 \xe2\x80\x94 Kernel Dashboard OS\r\n"
    + b"-" * 60
    + b"\r\n"
    + b" Type HELP for commands, HELP <word> for details.\r\n"
    + b" Type SCREENS for interactive TUI (or N SCREEN for screen N).\r\n"
    + b" Type TOPICS or LESSONS for documentation.\r\n"
)
EXPECTED_STARTUP_TRANSCRIPT = (
    STARTUP_BANNER
    + b" MP64FS loaded\r\n"
    + b" Running autoexec.f...\r\n"
    + bios_bench.COMPLETION_MARKER.encode("ascii")
    + b"\r\n\r\n"
)

REPRESENTATIVE_WORDS = (
    b".R",
    b"CRC32-STR",
    b"XMEM-INIT",
    b"ALLOCATE",
    b"BUFFER",
    b"KERNEL",
    b"PIPELINE",
    b"FS-LOAD",
    b"LOAD",
    b"DOC",
    b"WORDS-LIKE",
    b"SPAWN",
    b"PREEMPT-ON",
    b"CORE-RUN",
    b"SCREENS",
    b"PORT!",
    b"RING",
    b"HASHTABLE",
    b"REQUIRE",
    b"_AUTOEXEC-RUN",
)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--runtime-root",
        type=Path,
        default=ROOT,
        help="MegaPad checkout whose hosted runtime and KDOS are measured",
    )
    parser.add_argument(
        "--ram-kib", type=bios_bench._positive_int, default=DEFAULT_RAM_KIB
    )
    parser.add_argument(
        "--ext-mem-mib",
        type=bios_bench._positive_int,
        default=DEFAULT_EXT_MEM_MIB,
    )
    parser.add_argument(
        "--hbw-mib", type=bios_bench._positive_int, default=DEFAULT_HBW_MIB
    )
    parser.add_argument(
        "--vram-mib", type=bios_bench._positive_int, default=DEFAULT_VRAM_MIB
    )
    parser.add_argument(
        "--cols", type=bios_bench._positive_int, default=DEFAULT_COLS
    )
    parser.add_argument(
        "--rows", type=bios_bench._positive_int, default=DEFAULT_ROWS
    )
    parser.add_argument("--json", action="store_true", help="write JSON to stdout")
    parser.add_argument("--output", type=Path, help="also write JSON to this path")
    return parser


def _activate_runtime(runtime_root: Path) -> Path:
    root = runtime_root.expanduser().resolve()
    if root != ROOT.resolve():
        raise RuntimeError(
            "the simulator benchmark must execute from the measured checkout; "
            f"got --runtime-root {root}, harness checkout {ROOT.resolve()}"
        )
    required = (
        "bios.asm",
        "kdos.f",
        "diskutil.py",
        "simulator/runtime.py",
        "simulator/platform.py",
    )
    missing = [name for name in required if not (root / name).is_file()]
    if missing:
        raise RuntimeError(
            f"invalid MegaPad runtime root {root}: missing {', '.join(missing)}"
        )
    return root


def _verified_source(root: Path) -> bytes:
    source = (root / "kdos.f").read_bytes()
    if len(source) != KDOS_BYTES:
        raise RuntimeError(f"unexpected kdos.f size: {len(source)}")
    if source.count(b"\n") != KDOS_LINES:
        raise RuntimeError("unexpected kdos.f physical line count")
    if hashlib.sha256(source).hexdigest() != KDOS_SHA256:
        raise RuntimeError("unexpected kdos.f content hash")
    if not source.endswith(b"JIT-OFF\nCR\n"):
        raise RuntimeError("kdos.f no longer ends at the qualified startup frontier")
    return source


def _packed_lines(source: bytes) -> tuple[bytes, tuple[tuple[int, bytes], ...]]:
    packed = diskutil.pack_forth_source(source)
    submitted = tuple(
        (line_number, line)
        for line_number, line in enumerate(source.splitlines(), start=1)
        if line.strip() and not line.lstrip().startswith(b"\\")
    )
    if len(submitted) != SUBMITTED_LINES:
        raise RuntimeError(f"unexpected submitted line count: {len(submitted)}")
    payload_bytes = sum(len(line) for _line_number, line in submitted)
    if payload_bytes != SUBMITTED_PAYLOAD_BYTES:
        raise RuntimeError(f"unexpected submitted payload bytes: {payload_bytes}")
    if len(packed) != PACKED_KDOS_BYTES:
        raise RuntimeError(f"unexpected packed KDOS bytes: {len(packed)}")
    if max(len(line) for _line_number, line in submitted) != MAX_SUBMITTED_LINE:
        raise RuntimeError("unexpected maximum submitted KDOS line length")
    if packed != b"".join(line + b"\n" for _number, line in submitted):
        raise RuntimeError("packed KDOS differs from the checked semantic line stream")
    return packed, submitted


def _fixture_runtime(root: Path) -> SimpleNamespace:
    return SimpleNamespace(
        root=root,
        MP64FS=diskutil.MP64FS,
        FTYPE_FORTH=diskutil.FTYPE_FORTH,
        FLAG_SYSTEM=diskutil.FLAG_SYSTEM,
        SECTOR_SIZE=diskutil.SECTOR_SIZE,
        DIR_ENTRY_SIZE=diskutil.DIR_ENTRY_SIZE,
        pack_forth_source=diskutil.pack_forth_source,
    )


def _evaluate_checked_line(
    runtime: MegaForthRuntime,
    *,
    evaluator_xt: int,
    line_cell: int,
    source_address: int,
    line_number: int,
    source: bytes,
) -> int:
    runtime.memory.write64(line_cell, line_number)
    runtime.memory.write_bytes(source_address, source)
    runtime.main_context.data.push(source_address)
    runtime.main_context.data.push(len(source))
    result = runtime.execute(evaluator_xt)
    status = runtime.main_context.data.pop()
    if status != 0:
        raise RuntimeError(
            f"checked EVALUATE failed at kdos.f:{line_number}: "
            f"status={status}, source={source!r}"
        )
    return result.semantic_steps


def _execute(
    runtime: MegaForthRuntime,
    name: bytes | str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    if context.data.snapshot() != () or context.returns.snapshot() != ():
        raise RuntimeError(f"dirty stack before executing {name!r}")
    for value in inputs:
        context.data.push(value)
    runtime.execute(name)
    result = context.data.snapshot()
    context.data.clear()
    if context.returns.snapshot() != ():
        raise RuntimeError(f"return-stack leak after executing {name!r}")
    return result


def _variable(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return runtime.memory.read64(_execute(runtime, name)[0])


def _dictionary_name_hash(runtime: MegaForthRuntime) -> str:
    digest = hashlib.sha256()
    for word in runtime.dictionary.words:
        digest.update(len(word.name).to_bytes(2, "little"))
        digest.update(word.name)
    return digest.hexdigest()


def run_benchmark(args: argparse.Namespace) -> dict:
    run_entry = time.perf_counter()
    root = _activate_runtime(args.runtime_root)
    setup_started = time.perf_counter()
    source = _verified_source(root)
    packed, submitted = _packed_lines(source)

    with tempfile.TemporaryDirectory(prefix="megapad-simulator-kdos-") as directory:
        image_path = Path(directory) / "bios-kdos.img"
        sources = bios_bench._build_boot_image(_fixture_runtime(root), image_path)
        if sources["packed_kdos_sha256"] != hashlib.sha256(packed).hexdigest():
            raise RuntimeError("boot image does not contain the checked packed KDOS")
        fixture_preparation_elapsed_s = time.perf_counter() - setup_started

        runtime_construction_started = time.perf_counter()
        memory = create_one_core_address_space(
            bank0_size=args.ram_kib << 10,
            external_size=args.ext_mem_mib << 20,
            hbw_size=args.hbw_mib << 20,
            vram_size=args.vram_mib << 20,
        )
        runtime = MegaForthRuntime(memory=memory)
        runtime_construction_elapsed_s = (
            time.perf_counter() - runtime_construction_started
        )
        backend_preparation_started = time.perf_counter()
        core_words = runtime.dictionary.words
        checked_evaluator = runtime.find("EVALUATE-CHECKED")
        evaluator_finish = runtime.find("EVALUATE-FINISH")
        evaluator_line = runtime.find("EVAL-LINE")
        if checked_evaluator is None or evaluator_finish is None or evaluator_line is None:
            raise RuntimeError("semantic BIOS evaluator vocabulary is incomplete")
        checked_evaluator_xt = checked_evaluator.xt
        evaluator_finish_xt = evaluator_finish.xt
        source_address = runtime.dictionary.start_address - 256
        if source_address < 0:
            raise RuntimeError("no scratch space below the hosted dictionary")
        runtime.memory.fill(source_address, 256, 0)
        runtime.storage.attach(image_path.read_bytes())
        media_before = runtime.storage.image_bytes
        media_before_sha256 = hashlib.sha256(media_before).hexdigest()
        backend_preparation_elapsed_s = (
            time.perf_counter() - backend_preparation_started
        )
        setup_elapsed_s = time.perf_counter() - setup_started

        usage_before = resource.getrusage(resource.RUSAGE_SELF)
        started = time.perf_counter()
        semantic_steps = 0
        for line_number, line in submitted:
            semantic_steps += _evaluate_checked_line(
                runtime,
                evaluator_xt=checked_evaluator_xt,
                line_cell=evaluator_line.body_address,
                source_address=source_address,
                line_number=line_number,
                source=line,
            )
        finish_result = runtime.execute(evaluator_finish_xt)
        semantic_steps += finish_result.semantic_steps
        finish_status = runtime.main_context.data.pop()
        elapsed_s = time.perf_counter() - started
        usage_after = resource.getrusage(resource.RUSAGE_SELF)
        ready_from_run_entry_s = time.perf_counter() - run_entry

        loaded_words = runtime.dictionary.words[len(core_words) :]
        unique_bindings = {word.name.upper() for word in runtime.dictionary.words}
        index = runtime.dictionary_index.state
        uart = runtime.drain_uart_output()
        media_after = runtime.storage.image_bytes
        media_after_sha256 = hashlib.sha256(media_after).hexdigest()
        validation = {
            "ordinary_checked_source_mode": True,
            "canonical_one_core_memory_shape": (
                args.ram_kib == DEFAULT_RAM_KIB
                and args.ext_mem_mib == DEFAULT_EXT_MEM_MIB
                and args.hbw_mib == DEFAULT_HBW_MIB
                and args.vram_mib == DEFAULT_VRAM_MIB
            ),
            "exact_source_payload": (
                len(submitted) == SUBMITTED_LINES
                and len(packed) == PACKED_KDOS_BYTES
                and sources["kdos_source_sha256"] == KDOS_SHA256
            ),
            "evaluator_finish_succeeded": finish_status == 0,
            "exact_core_and_kdos_publications": (
                len(core_words) == CORE_WORDS and len(loaded_words) == KDOS_WORDS
            ),
            "qualified_definition_frontier": (
                bool(loaded_words)
                and loaded_words[0].name == b".R"
                and tuple(word.name for word in loaded_words[-2:])
                == (b"_AUTOEXEC-NAME", b"_AUTOEXEC-RUN")
            ),
            "representative_vocabulary_present": all(
                runtime.find(name) is not None for name in REPRESENTATIVE_WORDS
            ),
            "temporary_interpreter_control_absent": runtime.find(b"<interpret-if>") is None,
            "authoritative_dictionary_index": (
                index.flags == DICT_INDEX_BOUND | DICT_INDEX_AUTHORITATIVE
                and index.slots == 65_536
                and index.count == len(unique_bindings) == UNIQUE_BINDINGS
            ),
            "filesystem_and_heap_initialized": (
                _variable(runtime, "FS-OK") == TRUE
                and _variable(runtime, "HEAP-INIT") == 1
                and _execute(runtime, "HEAP-VERIFY") == (TRUE,)
            ),
            "startup_registries_complete": (
                _variable(runtime, "BUF-COUNT") == 6
                and _variable(runtime, "KERN-COUNT") == 23
                and _variable(runtime, "PIPE-COUNT") == 3
                and _variable(runtime, "NSCREENS") == 9
            ),
            "storage_media_unchanged": (
                media_after == media_before
                and runtime.storage.completion == 7
            ),
            "exact_semantic_startup_transcript": uart == EXPECTED_STARTUP_TRANSCRIPT,
            "stacks_and_locks_balanced": (
                runtime.main_context.data.snapshot() == ()
                and runtime.main_context.returns.snapshot() == ()
                and all(owner is None for owner in runtime.spinlocks.owners)
            ),
        }

        measurement = {
            "reason": "semantic-evaluator-finished",
            "wall_time_s": elapsed_s,
            "host_user_time_s": usage_after.ru_utime - usage_before.ru_utime,
            "host_system_time_s": usage_after.ru_stime - usage_before.ru_stime,
            "max_rss_kib": int(usage_after.ru_maxrss),
            "setup_time_s": setup_elapsed_s,
            "fixture_preparation_time_s": fixture_preparation_elapsed_s,
            "runtime_construction_time_s": runtime_construction_elapsed_s,
            "backend_preparation_time_s": backend_preparation_elapsed_s,
            "ready_from_run_entry_s": ready_from_run_entry_s,
            "submitted_lines": len(submitted),
            "submitted_payload_bytes": SUBMITTED_PAYLOAD_BYTES,
            "packed_source_bytes": len(packed),
            "source_bytes_per_s": len(packed) / elapsed_s,
            "kdos_publications_per_s": len(loaded_words) / elapsed_s,
            "backend_local_semantic_steps": semantic_steps,
            "timing_qualified_for_throughput": True,
        }

        return {
            "schema": SCHEMA,
            "schema_version": SCHEMA_VERSION,
            "generated_at_utc": datetime.now(timezone.utc).isoformat(),
            "outcome": "pass" if all(validation.values()) else "fail",
            "repository": bios_bench._repository_provenance(root),
            "harness": {
                "path": str(Path(__file__).resolve()),
                "sha256": bios_bench._sha256_file(Path(__file__).resolve()),
                "schema": SCHEMA,
                "schema_version": SCHEMA_VERSION,
                "repository": bios_bench._repository_provenance(ROOT),
            },
            "host": {
                "platform": platform.platform(),
                "python": platform.python_version(),
                "cpu": bios_bench._cpu_model(),
            },
            "configuration": {
                "full_cores": 1,
                "clusters": 0,
                "lanes": 1,
                "ram_kib": args.ram_kib,
                "ext_mem_mib": args.ext_mem_mib,
                "hbw_mib": args.hbw_mib,
                "vram_mib": args.vram_mib,
                "terminal": [args.cols, args.rows],
                "terminal_model": "hosted-uart-byte-stream",
                "compiled_forth_cache": False,
            },
            "measurement_semantics": {
                "start": "prepared semantic BIOS immediately before first checked KDOS line",
                "stop": (
                    "successful EVALUATE-FINISH after the complete packed KDOS "
                    "source and its normal marker-only autoexec"
                ),
                "source_mode": "same executable LF records as MP64FS packed source",
                "compiled_forth_cache": False,
                "includes": [
                    "complete hosted KDOS source evaluation",
                    "KDOS filesystem and heap initialization",
                    "normal KDOS autoexec lookup and evaluation",
                ],
                "excludes": [
                    "MP64 instruction execution",
                    "BIOS ROM execution",
                    "MP64FS transfer of kdos.f into a BIOS load buffer",
                    "MegaPad optional system modules",
                    "Akashic",
                    "Desk",
                    "rich terminal",
                ],
                "backend_local_semantic_steps_are_mp64_cycles": False,
            },
            "sources": sources,
            "measurement": measurement,
            "machine_state": {
                "here": runtime.dictionary.here,
                "latest": runtime.dictionary.latest,
                "core_words": len(core_words),
                "kdos_words": len(loaded_words),
                "dictionary_words": len(runtime.dictionary.words),
                "unique_bindings": len(unique_bindings),
                "dictionary_name_sha256": _dictionary_name_hash(runtime),
                "dictionary_index_slots": index.slots,
                "dictionary_index_count": index.count,
                "storage_completion": runtime.storage.completion,
                "storage_media_before_sha256": media_before_sha256,
                "storage_media_after_sha256": media_after_sha256,
                "uart_output_sha256": hashlib.sha256(uart).hexdigest(),
                "uart_output_utf8": uart.decode("utf-8", errors="replace"),
            },
            "errors": [name for name, passed in validation.items() if not passed],
            "validation": validation,
        }


def print_human(result: dict) -> None:
    measurement = result["measurement"]
    repository = result["repository"]
    print(f"Semantic KDOS source load: {result['outcome'].upper()}")
    print(f"  revision: {repository['commit']}")
    print(
        f"  {measurement['submitted_lines']:,} lines / "
        f"{measurement['packed_source_bytes']:,} bytes; "
        f"{measurement['wall_time_s']:.3f}s"
    )
    print(
        f"  setup: {measurement['setup_time_s']:.3f}s; "
        f"run-entry to ready: {measurement['ready_from_run_entry_s']:.3f}s"
    )
    failed = [name for name, passed in result["validation"].items() if not passed]
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
