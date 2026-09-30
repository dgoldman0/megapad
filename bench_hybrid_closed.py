#!/usr/bin/env python3
"""Bounded closed-callback/control measurements, with independent result checks.

Compare an ordinary semantic array clamp with an MP64 array loop that calls
the same declared closed policy. These are different execution paths with
separately reported work; this harness makes no acceleration claim. Every
validation, warmup and measured trial owns a fresh prepared runtime. Source
compilation, registration, input setup, checking and cleanup are not timed.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import importlib
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import time
from typing import Callable


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.hybrid-closed-control.v1"
EXECUTORS = ("python", "native")
PATHS = ("semantic_control", "hybrid_callbacks")
MAX_ITERATIONS = 1024
MASK64 = (1 << 64) - 1
LOW, HIGH = -17, 23
INPUT_PATTERN = (-(1 << 63), -100, -18, -17, -16, -1, 0, 1, 23, 24, 25, 100, (1 << 63) - 1)
GUARD = b"\xa5" * 16
POLICY_SOURCE = b": BENCH-CLAMP ROT MIN MAX ;\n"


class BenchmarkError(RuntimeError):
    pass


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise BenchmarkError(message)


def _bounded(value: int, label: str, low: int, high: int) -> int:
    if type(value) is not int or not low <= value <= high:
        raise ValueError(f"{label} must be an exact integer in {low}..{high}")
    return value


def _number(low: int, high: int):
    def parse(value: str) -> int:
        try:
            return _bounded(int(value), "value", low, high)
        except ValueError as exc:
            raise argparse.ArgumentTypeError(str(exc)) from exc
    return parse


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executor", nargs="+", choices=EXECUTORS, default=list(EXECUTORS))
    parser.add_argument("--iterations", type=_number(1, MAX_ITERATIONS), default=128,
                        help="array cells and callback requests per hybrid invocation (default: 128)")
    parser.add_argument("--trials", type=_number(1, 10), default=3)
    parser.add_argument("--warmup", type=_number(0, 3), default=1)
    parser.add_argument("--timeout", type=_number(1, 120), default=30,
                        help="wall seconds per executor subprocess, including preparation and validation")
    parser.add_argument("--output", type=Path, help="also write the complete JSON report here")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def _hash(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _file_hash(path: Path) -> str:
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def _repository() -> dict:
    def git(*arguments):
        result = subprocess.run(["git", "--no-optional-locks", *arguments], cwd=ROOT,
                                capture_output=True, text=True, timeout=5, check=False)
        return result.stdout.strip() if result.returncode == 0 else None
    return {"root": str(ROOT), "commit": git("rev-parse", "HEAD"),
            "status": git("status", "--porcelain=v1", "--untracked-files=all")}


def _sources() -> dict[str, str]:
    paths = {Path(__file__).resolve(), ROOT / "asm.py", ROOT / "setup_accel.py",
             ROOT / "setup_simulator_accel.py"}
    for directory in ("hybrid", "simulator", "shared", "emulator"):
        paths.update(path for path in (ROOT / directory).rglob("*")
                     if path.is_file() and path.suffix in (".py", ".cpp", ".h"))
    return {str(path.relative_to(ROOT)): _file_hash(path) for path in sorted(paths)}


def _artifacts(executor: str) -> list[dict]:
    names = ("_mp64_accel", "_megaforth_native") if executor == "native" else ("_mp64_accel",)
    artifacts = []
    for name in names:
        module = importlib.import_module(name)
        path = Path(module.__file__).resolve()
        _require(path.parent == ROOT, f"{name} was loaded outside this checkout: {path}")
        artifacts.append({"module": name, "path": str(path), "bytes": path.stat().st_size,
                          "sha256": _file_hash(path)})
    return artifacts


@contextmanager
def _unprofiled():
    previous = os.environ.get("MEGAFORTH_NATIVE_PROFILE")
    os.environ["MEGAFORTH_NATIVE_PROFILE"] = "0"
    try:
        yield
    finally:
        if previous is None:
            os.environ.pop("MEGAFORTH_NATIVE_PROFILE", None)
        else:
            os.environ["MEGAFORTH_NATIVE_PROFILE"] = previous


def fixture_values(iterations: int) -> tuple[tuple[int, ...], tuple[int, ...]]:
    """Independent signed-integer oracle, without either runtime or assembler."""
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    inputs = tuple(INPUT_PATTERN[index % len(INPUT_PATTERN)] for index in range(iterations))
    expected = tuple(max(LOW, min(HIGH, value)) & MASK64 for value in inputs)
    return tuple(value & MASK64 for value in inputs), expected


def _pack(cells: tuple[int, ...]) -> bytes:
    return b"".join(value.to_bytes(8, "little") for value in cells)


def expected_counts(path: str, iterations: int) -> dict[str, int]:
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    if path == "semantic_control":
        # Three setup literals + DO + Return; per iteration: 3 literals,
        # 9 primitive calls at two ticks, CLAMP Call + seven ticks, and LOOP.
        return dict(semantic_steps=30 * iterations + 5, machine_instructions=0,
                    machine_cycles=0, transitions=0, callback_requests=0,
                    callback_semantic_steps=0, machine_segments=0)
    if path != "hybrid_callbacks":
        raise ValueError("unknown execution path")
    # Eight setup instructions, ten per element (including real callback RET),
    # then MOV + root RET. CALL/RET add two cycles per iteration; N-1 taken
    # branches add N-1; root RET adds one. No crossing invents an extra cycle.
    return dict(semantic_steps=7 * iterations + 1,
                machine_instructions=10 * iterations + 10,
                machine_cycles=13 * iterations + 10, transitions=1,
                callback_requests=iterations, callback_semantic_steps=7 * iterations,
                machine_segments=iterations + 1)


def machine_image(iterations: int):
    """Use the qualified PC-relative local CALL/RET gate, never an absolute XT."""
    from asm import assemble
    from shared.hybrid_abi import BufferRuleV1, CallbackExportV3, CallbackSiteV3, RoutineImageV3

    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    source = """mov r9, r4
mov r11, r5
mov r13, r6
mov r14, r7
ldi r0, 0
mov r12, r3
after_pc:
ldi r1, 0
add r12, r1
again:
ldn r4, r9
mov r5, r13
mov r6, r14
call:
call.l r12
str r9, r4
add r0, r4
addi r9, 8
subi r11, 1
brne again
mov r4, r0
ret.l
stub:
ret.l
"""
    labels = {}
    assemble(source, labels_out=labels)
    source = source.replace("ldi r1, 0", f"ldi r1, {labels['stub'] - labels['after_pc']}")
    final_labels = {}
    code = bytes(assemble(source, labels_out=final_labels))
    _require(final_labels == labels, "PC-relative callback assembly changed instruction boundaries")
    export = CallbackExportV3(export_id=17, name="BENCH-CLAMP", input_cells=3,
                              output_cells=1, effect="closed_integer_colon", max_semantic_steps=7)
    image = RoutineImageV3(
        name="BENCH-MACHINE", code=code, entry_offset=0, input_cells=4, output_cells=1,
        buffers=(BufferRuleV1(address_argument=0, length_argument=1, element_bytes=8,
                              max_bytes=iterations * 8, access="read_write"),),
        max_instructions=expected_counts("hybrid_callbacks", iterations)["machine_instructions"],
        return_stack_cells=16,
        callbacks=(CallbackSiteV3(call_offset=labels["call"], stub_offset=labels["stub"], export=export),),
    )
    return image, source.encode("ascii")


@dataclass
class Prepared:
    runtime: object
    memory: object
    action: Callable
    observe: Callable
    close: Callable
    description: dict


def prepare(path: str, executor: str, iterations: int) -> Prepared:
    from hybrid.runtime import HybridRuntime
    from simulator.memory import EXTERNAL_BASE
    from simulator.platform import create_one_core_address_space
    from simulator.runtime import MegaForthRuntime

    if executor not in EXECUTORS or path not in PATHS:
        raise ValueError("unknown executor or execution path")
    inputs, expected = fixture_values(iterations)
    expected_work = expected_counts(path, iterations)
    memory = create_one_core_address_space(bank0_size=65536, external_size=65536,
                                           dense_backing=True)
    base = EXTERNAL_BASE + 256
    initial = GUARD + _pack(inputs) + GUARD
    final = GUARD + _pack(expected) + GUARD
    memory.write_bytes(base - len(GUARD), initial)
    checksum = sum(expected) & MASK64
    hybrid = None
    if path == "hybrid_callbacks":
        hybrid = HybridRuntime.create(
            executor=executor, memory=memory,
            dispatch_instruction_limit=expected_work["machine_instructions"],
            dispatch_callback_limit=iterations,
            dispatch_callback_semantic_limit=7 * iterations,
        )
        runtime = hybrid.semantic
        close = hybrid.close
    else:
        runtime = MegaForthRuntime(memory=memory, execution_backend=executor)
        close = runtime.memory.mmio.audio.release_host_sink
    try:
        semantic_source = POLICY_SOURCE
        machine_source = code = None
        if path == "semantic_control":
            semantic_source += (
                f": BENCH-CONTROL 0 {iterations} 0 DO "
                f"{base} I CELLS + DUP @ {LOW} {HIGH} BENCH-CLAMP "
                "DUP ROT ! + LOOP ;\n"
            ).encode("ascii")
        runtime.evaluate(semantic_source, source_name="bench_hybrid_closed.f", step_budget=10000)
        _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty data stack")
        _require(runtime.main_context.returns.snapshot() == (), "preparation left a dirty return stack")
        if hybrid is not None:
            image, machine_source = machine_image(iterations)
            word = hybrid.register_routine_v3(image)
            code = hybrid.declaration_for(word).code
            for value in (base, iterations, LOW & MASK64, HIGH):
                runtime.main_context.data.push(value)
            action = lambda: hybrid.execute_xt(word.xt, step_budget=expected_work["semantic_steps"])
        else:
            action = lambda: runtime.execute("BENCH-CONTROL", step_budget=expected_work["semantic_steps"])
        before_cycles = runtime.diagnostics.semantic_cycles
        before_timer = runtime.timer.counter
        description = {
            "path": path, "semantic_executor": runtime.execution_backend,
            "callback_executor": "python_reference" if hybrid is not None else None,
            "metadata_version": 3 if hybrid is not None else None,
            "native_transport_version": 2 if hybrid is not None else None,
            "iterations": iterations, "signed_bounds": [LOW, HIGH],
            "input_pattern_signed": list(INPUT_PATTERN),
            "buffer_address": base, "buffer_bytes": iterations * 8,
            "guard_bytes_each_side": len(GUARD), "input_span_sha256": _hash(initial),
            "semantic_source_utf8": semantic_source.decode("ascii"),
            "semantic_source_sha256": _hash(semantic_source),
            "machine_source_utf8": None if machine_source is None else machine_source.decode("ascii"),
            "machine_source_sha256": None if machine_source is None else _hash(machine_source),
            "sealed_machine_code_sha256": None if code is None else _hash(code),
            "selected_bounds": {
                "semantic_steps": expected_work["semantic_steps"],
                "machine_instructions": expected_work["machine_instructions"],
                "callback_requests": expected_work["callback_requests"],
                "callback_semantic_steps": expected_work["callback_semantic_steps"],
                "callback_local_steps": 7 if hybrid is not None else None,
                "private_return_cells": 16 if hybrid is not None else None,
            },
        }

        def observe(result):
            counts = ({"semantic_steps": result.semantic_result.semantic_steps,
                       **{key: getattr(result, key) for key in expected_work if key != "semantic_steps"}}
                      if hybrid is not None else
                      {**expected_work, "semantic_steps": result.semantic_steps})
            _require(counts == expected_work, f"work counters differ: {counts} != {expected_work}")
            cells = runtime.main_context.data.snapshot()
            _require(cells == (checksum,), f"final cells differ: {cells} != {(checksum,)}")
            returns = runtime.main_context.returns
            _require(returns.snapshot() == () and returns.pointer == returns.empty_pointer,
                     "return stack did not balance")
            actual = memory.read_bytes(base - len(GUARD), len(final))
            _require(actual == final, "clamped shared buffer or guard bytes differ from independent oracle")
            _require(runtime.diagnostics.semantic_cycles - before_cycles == counts["semantic_steps"],
                     "semantic diagnostics differ from charged work")
            _require(runtime.timer.counter - before_timer == counts["semantic_steps"],
                     "hosted timer did not follow semantic work")
            return {**counts, "final_cells": list(cells), "buffer_sha256": _hash(actual[len(GUARD):-len(GUARD)]),
                    "shared_span_sha256": _hash(actual), "return_depth": 0}

        return Prepared(runtime, memory, action, observe, close, description)
    except BaseException:
        close()
        raise


def perform_once(path: str, executor: str, iterations: int, *, timed: bool) -> dict:
    started = time.perf_counter()
    fixture = prepare(path, executor, iterations)
    preparation_seconds = time.perf_counter() - started
    try:
        if timed:
            wall, cpu = time.perf_counter(), time.process_time()
            result = fixture.action()
            cpu_seconds, wall_seconds = time.process_time() - cpu, time.perf_counter() - wall
        else:
            result = fixture.action()
            cpu_seconds = wall_seconds = None
        check_started = time.perf_counter()
        observation = fixture.observe(result)
        return {"wall_seconds": wall_seconds, "process_cpu_seconds": cpu_seconds,
                "preparation_seconds": preparation_seconds,
                "validation_seconds": time.perf_counter() - check_started,
                "description": fixture.description, "guest": observation}
    finally:
        fixture.close()


def run_case(executor: str, *, iterations: int, trials: int, warmup: int) -> dict:
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    _bounded(trials, "trials", 1, 10)
    _bounded(warmup, "warmup", 0, 3)
    if executor not in EXECUTORS:
        raise ValueError("unknown executor")
    with _unprofiled():
        return _run_case(executor, iterations=iterations, trials=trials, warmup=warmup)


def _run_case(executor: str, *, iterations: int, trials: int, warmup: int) -> dict:
    artifacts = _artifacts(executor)
    validation = {path: perform_once(path, executor, iterations, timed=False) for path in PATHS}
    for key in ("final_cells", "buffer_sha256", "shared_span_sha256"):
        _require(validation[PATHS[0]]["guest"][key] == validation[PATHS[1]]["guest"][key],
                 f"independently validated paths disagree on {key}")
    for _ in range(warmup):
        for path in PATHS:
            perform_once(path, executor, iterations, timed=False)
    samples = {path: [] for path in PATHS}
    for trial in range(trials):
        # Alternate pair order without carrying a runtime/cache across trials.
        for path in PATHS if trial % 2 == 0 else reversed(PATHS):
            sample = perform_once(path, executor, iterations, timed=True)
            _require(sample["guest"] == validation[path]["guest"],
                     "timed trial differs from separate validation instance")
            samples[path].append(sample)
    for artifact in artifacts:
        _require(_file_hash(Path(artifact["path"])) == artifact["sha256"],
                 "native artifact changed during measurement")
    return {"semantic_executor": executor, "iterations": iterations,
            "native_artifacts": artifacts, "validation": validation,
            "warmup_pairs": warmup, "paths": {
                path: {"samples": rows,
                       "median_wall_seconds": statistics.median(row["wall_seconds"] for row in rows),
                       "median_process_cpu_seconds": statistics.median(row["process_cpu_seconds"] for row in rows)}
                for path, rows in samples.items()
            }}


def run_suite(args: argparse.Namespace) -> dict:
    repository, sources = _repository(), _sources()
    cases = []
    for executor in dict.fromkeys(args.executor):
        command = [sys.executable, str(Path(__file__).resolve()), "--worker", "--executor", executor,
                   "--iterations", str(args.iterations), "--trials", str(args.trials),
                   "--warmup", str(args.warmup)]
        environment = dict(os.environ, MEGAFORTH_NATIVE_PROFILE="0")
        try:
            completed = subprocess.run(command, cwd=ROOT, env=environment, capture_output=True,
                                       text=True, timeout=args.timeout, check=False)
        except subprocess.TimeoutExpired as exc:
            raise BenchmarkError(f"{executor} exceeded the {args.timeout}s wall watchdog") from exc
        _require(completed.returncode == 0, f"{executor} worker failed: {completed.stderr.strip()}")
        cases.append(json.loads(completed.stdout))
    _require(_sources() == sources, "runtime sources changed during measurement")
    _require(_repository()["commit"] == repository["commit"], "repository commit changed during measurement")
    return {"schema": SCHEMA, "generated_at": datetime.now(timezone.utc).isoformat(),
            "repository": repository, "source_sha256": sources,
            "host": {"platform": platform.platform(), "machine": platform.machine(),
                     "python": sys.version, "python_executable": sys.executable},
            "measurement": {"case_timeout_seconds": args.timeout,
                            "order": "sequential executor subprocesses; alternating path order per trial",
                            "trial": "fresh owner; only action timed; preparation/checking/cleanup excluded",
                            "validation": "fresh untimed pair before warmups and measured trials; integer oracle",
                            "warmup": "discarded fresh pair; no retained guest cache",
                            "callback_executor": "python_reference",
                            "scope": "bounded synchronous integer clamp/control; no speed or application claim"},
            "cases": cases}


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.worker and len(args.executor) != 1:
        parser.error("worker requires exactly one executor")
    try:
        report = (run_case(args.executor[0], iterations=args.iterations, trials=args.trials, warmup=args.warmup)
                  if args.worker else run_suite(args))
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired) as exc:
        print(f"benchmark failed: {exc}", file=sys.stderr)
        return 1
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
