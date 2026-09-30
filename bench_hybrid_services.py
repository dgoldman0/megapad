#!/usr/bin/env python3
"""Bounded paired scalar-service measurements; no acceleration claim.

Each group owns one prepared HybridRuntime and its original scalar FP owner.
The direct semantic baseline brackets the V5 service and two differing-work
integer controls. Setup, reset, checking and close are outside timed actions.
Unprofiled latency and optional Python attribution use separate fresh groups.
"""

from __future__ import annotations

import argparse
import cProfile
from dataclasses import dataclass
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import time

from bench_hybrid_closed import (
    BenchmarkError, _artifacts, _bounded, _file_hash, _hash, _number, _repository,
    _require, _sources as _runtime_sources, _unprofiled,
)
from bench_runtime_hotspots import FP_RESULTS, FP_XOR, MASK64, _profile_summary


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.hybrid-scalar-service.v1"
EXECUTORS = ("python", "native")
PATHS = ("semantic_fp", "service_callbacks", "machine_control", "integer_callbacks")
DEFAULT_ITERATIONS = (64, 256)
MAX_ITERATIONS = 256
GUARD = b"\xa5" * 16
BUFFER_BYTES = 32
OPERATIONS = (
    ("F64+", (0x3FF8000000000000, 0x4004000000000000)),
    ("F64FMA", (0x4000000000000000, 0x4008000000000000, 0x4010000000000000)),
    ("F64/", (0x3FF0000000000000, 0x4008000000000000)),
    ("F64SQRT", (0x4000000000000000,)),
)
REPORT_COUNTERS = ("machine_instructions", "machine_cycles", "transitions",
                   "callback_requests", "callback_semantic_steps", "machine_segments")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--executor", nargs="+", choices=EXECUTORS, default=list(EXECUTORS))
    parser.add_argument("--iterations", nargs="+", type=_number(1, MAX_ITERATIONS),
                        default=list(DEFAULT_ITERATIONS), help="default: 64 256; four FP operations per iteration")
    parser.add_argument("--trials", type=_number(1, 10), default=3)
    parser.add_argument("--warmup", type=_number(0, 3), default=1)
    parser.add_argument("--timeout", type=_number(1, 120), default=60,
                        help="wall seconds per executor/iteration subprocess, including all setup")
    parser.add_argument("--attribution", action="store_true",
                        help="separate untimed Python attribution group; excluded from latency summaries")
    parser.add_argument("--output", type=Path)
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def _sources() -> dict[str, str]:
    result = _runtime_sources()
    for name in ("bench_hybrid_services.py", "bench_hybrid_closed.py", "bench_runtime_hotspots.py"):
        result[name] = _file_hash(ROOT / name)
    return result


def fixture_values(iterations: int) -> dict:
    """Known binary64 RNE answers shared with the pre-existing hotspot oracle."""
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    return {"result_bits": list(FP_RESULTS), "checksum": (FP_XOR * iterations) & MASK64,
            "fpcsr": 16, "fp_operations": 4 * iterations}


def expected_counts(path: str, iterations: int) -> dict[str, int]:
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    if path not in PATHS:
        raise ValueError("unknown execution path")
    if path == "semantic_fp":
        # Initial accumulator/limits/DO/Return: five ticks. Per iteration:
        # eight operands, four FP Calls+primitives, four DUP/literal/! stores,
        # three XOR Calls+primitives, one + Call+primitive, and LOOP.
        return dict(semantic_steps=45 * iterations + 5, machine_instructions=0,
                    machine_cycles=0, transitions=0, callback_requests=0,
                    callback_semantic_steps=0, machine_segments=0)
    callbacks = path != "machine_control"
    # Four setup + two final instructions; 45 instructions per callback loop
    # (8 operand LDIs, 4 target LDIs, 4 real CALL/RET pairs), 21 without calls.
    # Prefixed LDI64 adds a cycle; CALL/RET each add one, taken LBR adds one.
    return dict(semantic_steps=4 * iterations + 1 if callbacks else 1,
                machine_instructions=(45 if callbacks else 21) * iterations + 6,
                machine_cycles=(66 if callbacks else 26) * iterations + 6,
                transitions=1, callback_requests=4 * iterations if callbacks else 0,
                callback_semantic_steps=4 * iterations if callbacks else 0,
                machine_segments=4 * iterations + 1 if callbacks else 1)


def semantic_source(iterations: int, address: int) -> bytes:
    _bounded(iterations, "iterations", 1, MAX_ITERATIONS)
    words = [f": BENCH-FP 0 {iterations} 0 DO"]
    for index, (name, arguments) in enumerate(OPERATIONS):
        words.extend(hex(value) for value in arguments)
        words.extend((name, "DUP", str(address + 8 * index), "!"))
        if index:
            words.append("XOR")
    words.append("+ LOOP ;")
    return (" ".join(words) + "\n").encode("ascii")


def machine_image(path: str, iterations: int):
    """Identical result stores/checksum; controls disclose their different work."""
    from asm import assemble
    from shared.hybrid_abi import BufferRuleV1, CallbackExportV2, CallbackSiteV2, RoutineImageV1, RoutineImageV2
    from shared.hybrid_services import CallbackSiteV5, RoutineImageV5, ServiceExportV5

    if path not in PATHS[1:]:
        raise ValueError("machine image requires a machine path")
    counts = expected_counts(path, iterations)
    callbacks = path != "machine_control"
    lines = ["mov r9, r4", "mov r10, r6", "ldi r11, 0", "mov r12, r3",
             "after_pc:", "again:", "mov r13, r9", "ldi r14, 0"]
    for index, (_name, operands) in enumerate(OPERATIONS):
        arguments = operands if path == "service_callbacks" else (FP_RESULTS[index], 0)
        if callbacks:
            lines.extend(f"ldi64 r{4 + register}, {value}" for register, value in enumerate(arguments))
            lines.extend(("mov r0, r12", f"ldi64 r1, OFFSET_{index}", "add r0, r1",
                          f"call_{index}:", "call.l r0"))
        else:
            lines.append(f"ldi64 r4, {FP_RESULTS[index]}")
        lines.extend(("str r13, r4", "addi r13, 8", "xor r14, r4"))
    lines.extend(("add r11, r14", "subi r10, 1", "lbrne again", "mov r4, r11", "ret.l"))
    if callbacks:
        for index in range(4):
            lines.extend((f"stub_{index}:", "ret.l"))
    template = "\n".join(lines) + "\n"
    provisional = template
    for index in range(4):
        provisional = provisional.replace(f"OFFSET_{index}", "0")
    labels = {}
    assemble(provisional, labels_out=labels)
    source = template
    for index in range(4):
        if callbacks:
            source = source.replace(f"OFFSET_{index}", str(labels[f"stub_{index}"] - labels["after_pc"]))
    final_labels = {}
    code = bytes(assemble(source, labels_out=final_labels))
    _require(final_labels == labels, "PC-relative assembly changed instruction boundaries")
    fields = dict(name="BENCH-" + path.upper().replace("_", "-"), code=code,
                  entry_offset=0, input_cells=3, output_cells=1,
                  buffers=(BufferRuleV1(address_argument=0, length_argument=1, element_bytes=1,
                                        max_bytes=BUFFER_BYTES, access="read_write"),),
                  max_instructions=counts["machine_instructions"], return_stack_cells=16)
    if not callbacks:
        return RoutineImageV1(**fields), source.encode("ascii")
    sites = []
    for index, (name, arguments) in enumerate(OPERATIONS):
        export = (ServiceExportV5(export_id=32 + index, name=name, input_cells=len(arguments), output_cells=1)
                  if path == "service_callbacks" else
                  CallbackExportV2(export_id=5, name="XOR", input_cells=2, output_cells=1))
        site_type = CallbackSiteV5 if path == "service_callbacks" else CallbackSiteV2
        sites.append(site_type(call_offset=labels[f"call_{index}"], stub_offset=labels[f"stub_{index}"], export=export))
    if path == "service_callbacks":
        return RoutineImageV5(**fields, callbacks=tuple(sites), max_callback_requests=4 * iterations), source.encode("ascii")
    return RoutineImageV2(**fields, callbacks=tuple(sites)), source.encode("ascii")


def scalar_selection(runtime) -> dict:
    """Observe the actual pinned value route; never rebind it for a benchmark."""
    owner = runtime.scalar_float
    executor = runtime._native_execution
    kernel = owner._native_execute
    if executor is None:
        _require(runtime.execution_backend == "python" and kernel is None,
                 "Python semantic selection has a substituted scalar value kernel")
        return {"scalar_value_executor": "python", "scalar_value_kernel": "shared.scalar_fp.execute"}
    _require(runtime.execution_backend == "native" and executor.scalar_float is owner
             and kernel is executor.extension.scalar_fp_execute,
             "native scalar kernel is not bound to the original semantic owner")
    _require(executor.extension.__name__ == "_megaforth_native", "scalar native extension identity differs")
    return {"scalar_value_executor": "native", "scalar_value_kernel": "_megaforth_native.scalar_fp_execute"}


@dataclass
class Prepared:
    hybrid: object
    memory: object
    scalar_owner: object
    iterations: int
    address: int
    words: dict
    description: dict

    def close(self):
        self.hybrid.close()

    def reset(self, path: str):
        if path not in PATHS:
            raise ValueError("unknown execution path")
        runtime = self.hybrid.semantic
        _require(runtime.scalar_float is self.scalar_owner, "scalar owner changed between paired actions")
        _require(scalar_selection(runtime) == self.description["scalar_selection"], "scalar value route changed")
        _require(self.hybrid.service_callback_value_executor == self.description["service_callback_value_executor"],
                 "public service value executor attribution changed")
        returns = runtime.main_context.returns
        _require(returns.snapshot() == () and returns.pointer == returns.empty_pointer, "return stack is not balanced")
        runtime.main_context.data.clear()
        self.scalar_owner.write_fpcsr(0)
        self.memory.write_bytes(self.address - len(GUARD), GUARD + bytes(BUFFER_BYTES) + GUARD)
        if path != "semantic_fp":
            for value in (self.address, BUFFER_BYTES, self.iterations):
                runtime.main_context.data.push(value)
        return runtime.diagnostics.semantic_cycles, runtime.timer.counter

    def action(self, path: str):
        return self.hybrid.execute_xt(self.words[path].xt, step_budget=expected_counts(path, self.iterations)["semantic_steps"])

    def observe(self, path: str, report, before: tuple[int, int]) -> dict:
        runtime = self.hybrid.semantic
        counts = {"semantic_steps": report.semantic_result.semantic_steps,
                  **{key: getattr(report, key) for key in REPORT_COUNTERS}}
        _require(counts == expected_counts(path, self.iterations), f"{path} work counters differ: {counts}")
        expected = fixture_values(self.iterations)
        cells = runtime.main_context.data.snapshot()
        _require(cells == (expected["checksum"],), f"{path} checksum differs: {cells}")
        actual = self.memory.read_bytes(self.address - len(GUARD), len(GUARD) * 2 + BUFFER_BYTES)
        known = b"".join(value.to_bytes(8, "little") for value in FP_RESULTS)
        _require(actual == GUARD + known + GUARD, f"{path} exact result bits or guard bytes differ")
        fp = path in ("semantic_fp", "service_callbacks")
        fpcsr = self.scalar_owner.fpcsr
        _require(fpcsr == (16 if fp else 0), f"{path} FPCSR differs: {fpcsr}")
        _require(runtime.scalar_float is self.scalar_owner, "paired execution replaced the scalar owner")
        _require(scalar_selection(runtime) == self.description["scalar_selection"], "scalar value route changed")
        returns = runtime.main_context.returns
        _require(returns.snapshot() == () and returns.pointer == returns.empty_pointer, "return stack did not balance")
        _require(runtime.diagnostics.semantic_cycles - before[0] == counts["semantic_steps"], "semantic diagnostics differ")
        _require(runtime.timer.counter - before[1] == counts["semantic_steps"], "semantic timer differs")
        if path == "service_callbacks":
            _require(self.hybrid.max_machine_depth == 1, "receipt-backed service invocation depth differs")
        # Every completed callback uses the qualified private transport-2
        # profile. There is exactly one parked frame and no child admission;
        # this is contract-derived evidence, not a profiling hook in the loop.
        return {**counts, "result_bits": list(FP_RESULTS), "checksum": cells[0], "fpcsr": fpcsr,
                "fp_operations": 4 * self.iterations if fp else 0,
                "integer_leaf_operations": 4 * self.iterations if path == "integer_callbacks" else 0,
                "precomputed_values": 4 * self.iterations if path == "machine_control" else 0,
                "max_parked_depth": int(counts["callback_requests"] > 0), "return_depth": 0,
                "exit_reason": "returned", "buffer_sha256": _hash(known), "guarded_span_sha256": _hash(actual)}


def prepare(executor: str, iterations: int) -> Prepared:
    from hybrid.runtime import HybridRuntime
    from simulator.memory import EXTERNAL_BASE
    from simulator.platform import create_one_core_address_space

    fixture_values(iterations)
    if executor not in EXECUTORS:
        raise ValueError("unknown executor")
    memory = create_one_core_address_space(bank0_size=65536, external_size=65536, dense_backing=True)
    hybrid = HybridRuntime.create(executor=executor, memory=memory, require_service_callbacks=True,
                                  dispatch_instruction_limit=expected_counts("service_callbacks", iterations)["machine_instructions"],
                                  dispatch_callback_limit=4 * iterations, dispatch_callback_semantic_limit=4 * iterations)
    try:
        _require(hybrid.service_callback_abi_available, "qualified private scalar service callbacks are unavailable")
        runtime, address = hybrid.semantic, EXTERNAL_BASE + 256
        selected = scalar_selection(runtime)
        _require(runtime.execution_backend == executor, "requested semantic executor was not selected")
        value_executor = "python_reference" if executor == "python" else "shared_native_kernel"
        _require(hybrid.service_callback_value_executor == value_executor,
                 "public service value attribution differs from the actual scalar kernel")
        source = semantic_source(iterations, address)
        runtime.evaluate(source, source_name="bench_hybrid_services.f", step_budget=10000)
        words = {"semantic_fp": runtime.find("BENCH-FP")}
        machine_sources = {}
        for path, register in (("service_callbacks", hybrid.register_routine_v5),
                               ("machine_control", hybrid.register_routine_v1),
                               ("integer_callbacks", hybrid.register_routine_v2)):
            image, text = machine_image(path, iterations)
            words[path] = register(image)
            machine_sources[path] = {"source_utf8": text.decode("ascii"), "source_sha256": _hash(text),
                                     "sealed_code_sha256": _hash(hybrid.declaration_for(words[path]).code)}
        _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty stack")
        description = {"semantic_executor": executor, "scalar_selection": selected, "iterations": iterations,
                       "buffer_address": address, "buffer_bytes": BUFFER_BYTES, "guard_bytes_each_side": len(GUARD),
                       "metadata_versions": {"semantic_fp": None, "service_callbacks": 5,
                                             "machine_control": 1, "integer_callbacks": 2},
                       "native_transport_versions": {"semantic_fp": None, "service_callbacks": 2,
                                                     "machine_control": 1, "integer_callbacks": 2},
                       "callback_dispatcher": "python_reference", "service_effect": "scalar_fp_state",
                       "service_callback_value_executor": value_executor,
                       "service_capability": "private_scalar_fp_v1", "suspension_supported": False,
                       "parked_depth_evidence": "completed requests under the single-frame private transport-2 contract",
                       "owner_pairing": "all paths and before/after baselines share this exact runtime.scalar_float owner",
                       "semantic_source_utf8": source.decode("ascii"), "semantic_source_sha256": _hash(source),
                       "machine_sources": machine_sources,
                       "operations": [{"name": name, "input_bits": list(arguments), "output_bits": FP_RESULTS[index]}
                                      for index, (name, arguments) in enumerate(OPERATIONS)],
                       "expected_work": {path: expected_counts(path, iterations) for path in PATHS},
                       "selected_bounds": {
                           "dispatch_machine_instructions": expected_counts("service_callbacks", iterations)["machine_instructions"],
                           "dispatch_callback_requests": 4 * iterations,
                           "dispatch_callback_semantic_steps": 4 * iterations,
                           "per_path_semantic_steps": {path: expected_counts(path, iterations)["semantic_steps"] for path in PATHS},
                           "per_machine_invocation_instructions": {path: expected_counts(path, iterations)["machine_instructions"] for path in PATHS[1:]},
                           "service_invocation_callback_requests": 4 * iterations,
                       },
                       "callback_local_semantic_steps": 1, "private_return_cells": 16}
        return Prepared(hybrid, memory, runtime.scalar_float, iterations, address, words, description)
    except BaseException:
        hybrid.close()
        raise


def perform_group(executor: str, iterations: int, *, timed: bool, reverse: bool = False,
                  attribution: bool = False) -> dict:
    _require(not (timed and attribution), "attribution must not enter latency samples")
    started = time.perf_counter()
    fixture = prepare(executor, iterations)
    setup_seconds = time.perf_counter() - started
    try:
        middle = list(PATHS[1:])
        if reverse:
            middle.reverse()
        order = ["semantic_fp", *middle, "semantic_fp"]
        samples, baseline_recheck = {}, None
        for index, path in enumerate(order):
            before = fixture.reset(path)
            profiler = cProfile.Profile() if attribution else None
            wall, cpu = time.perf_counter(), time.process_time()
            if profiler is not None:
                profiler.enable()
            try:
                result = fixture.action(path)
            finally:
                if profiler is not None:
                    profiler.disable()
            cpu_seconds, wall_seconds = time.process_time() - cpu, time.perf_counter() - wall
            checking = time.perf_counter()
            guest = fixture.observe(path, result, before)
            row = {"wall_seconds": wall_seconds if timed else None,
                   "process_cpu_seconds": cpu_seconds if timed else None,
                   "validation_seconds": time.perf_counter() - checking, "guest": guest,
                   "observed_owner_max_machine_depth": fixture.hybrid.max_machine_depth}
            if profiler is not None:
                row["attribution"] = _profile_summary(profiler)
            if index == len(order) - 1:
                _require(guest == samples["semantic_fp"]["guest"], "same-owner direct baseline changed after controls")
                baseline_recheck = row
            else:
                samples[path] = row
        for key in ("result_bits", "checksum", "fpcsr", "fp_operations"):
            _require(samples["semantic_fp"]["guest"][key] == samples["service_callbacks"]["guest"][key],
                     f"paired direct/service result differs for {key}")
        return {"preparation_seconds": setup_seconds, "description": fixture.description,
                "order": order, "paths": samples, "baseline_recheck": baseline_recheck}
    finally:
        fixture.close()


def run_case(executor: str, *, iterations: int, trials: int, warmup: int, attribution: bool = False) -> dict:
    fixture_values(iterations)
    _bounded(trials, "trials", 1, 10)
    _bounded(warmup, "warmup", 0, 3)
    if executor not in EXECUTORS or type(attribution) is not bool:
        raise ValueError("invalid executor or attribution selection")
    with _unprofiled():
        artifacts = _artifacts(executor)
        validation = perform_group(executor, iterations, timed=False)
        for _ in range(warmup):
            perform_group(executor, iterations, timed=False)
        groups = [perform_group(executor, iterations, timed=True, reverse=bool(trial % 2)) for trial in range(trials)]
        attributed = perform_group(executor, iterations, timed=False, attribution=True) if attribution else None
        for group in [*groups, *([attributed] if attributed is not None else [])]:
            for path in PATHS:
                _require(group["paths"][path]["guest"] == validation["paths"][path]["guest"],
                         "measured or attributed work differs from independent validation")
        for artifact in artifacts:
            _require(_file_hash(Path(artifact["path"])) == artifact["sha256"], "native artifact changed during measurement")
        return {"semantic_executor": executor, "iterations": iterations, "native_artifacts": artifacts,
                "validation": validation, "warmup_groups": warmup, "groups": groups, "attribution_group": attributed,
                "latency": {path: {"median_wall_seconds": statistics.median(group["paths"][path]["wall_seconds"] for group in groups),
                                   "median_process_cpu_seconds": statistics.median(group["paths"][path]["process_cpu_seconds"] for group in groups)}
                            for path in PATHS},
                "baseline_recheck_median_wall_seconds": statistics.median(group["baseline_recheck"]["wall_seconds"] for group in groups)}


def run_suite(args: argparse.Namespace) -> dict:
    repository, sources = _repository(), _sources()
    cases = []
    for executor in dict.fromkeys(args.executor):
        for iterations in dict.fromkeys(args.iterations):
            command = [sys.executable, str(Path(__file__).resolve()), "--worker", "--executor", executor,
                       "--iterations", str(iterations), "--trials", str(args.trials), "--warmup", str(args.warmup)]
            if args.attribution:
                command.append("--attribution")
            try:
                result = subprocess.run(command, cwd=ROOT, env=dict(os.environ, MEGAFORTH_NATIVE_PROFILE="0"),
                                        capture_output=True, text=True, timeout=args.timeout, check=False)
            except subprocess.TimeoutExpired as exc:
                raise BenchmarkError(f"{executor}/{iterations} exceeded {args.timeout}s wall watchdog") from exc
            _require(result.returncode == 0, f"{executor}/{iterations} worker failed: {result.stderr.strip()}")
            cases.append(json.loads(result.stdout))
    _require(bool(cases), "no executor/iteration pairs selected")
    _require(_sources() == sources, "runtime sources changed during measurement")
    _require(_repository()["commit"] == repository["commit"], "repository commit changed during measurement")
    return {"schema": SCHEMA, "generated_at": datetime.now(timezone.utc).isoformat(),
            "repository": repository, "source_sha256": sources,
            "host": {"platform": platform.platform(), "machine": platform.machine(), "python": sys.version,
                     "python_executable": sys.executable, "cpu_count": os.cpu_count()},
            "measurement": {"case_timeout_seconds": args.timeout, "sequential_subprocesses": True,
                            "setup_reset_checking_cleanup_timed": False, "native_profiling": False,
                            "attribution": "separate fresh group, no latency reported",
                            "baselines": "same-owner direct FP before and after each group; current-host rechecks retained",
                            "cache": "fresh owner per group; retained caches between paired actions within a group",
                            "controls": "precomputed-value machine loop and XOR(value,0) callbacks perform different work; timings are not subtracted",
                            "scope": "bounded scalar-service crossing; no Desktop, throughput or acceleration claim"},
            "cases": cases}


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.worker and (len(args.executor) != 1 or len(args.iterations) != 1):
        parser.error("worker requires exactly one executor and iteration count")
    try:
        report = (run_case(args.executor[0], iterations=args.iterations[0], trials=args.trials,
                           warmup=args.warmup, attribution=args.attribution) if args.worker else run_suite(args))
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
