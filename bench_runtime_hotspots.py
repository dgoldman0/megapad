#!/usr/bin/env python3
"""Bounded, sequential hotspot measurements with independently checked results.

Each executor/workload pair runs in a fresh subprocess with a wall watchdog.
Each trial has a fresh prepared instance; discarded warmups warm process code,
not a retained guest/JIT cache. Setup and validation are outside the measured
action. Optional attribution repeats the action on another fresh instance and
is never included in the unprofiled timing samples.

This covers scalar FP, hosted FP64 tiles, SHA3, PCM capture, and synthetic
source compilation. It does not measure Desktop, full KDOS boot, audio playback,
or architectural/semantic timing equivalence. Use bench_simulator_kdos_load.py
and bench_bios_kdos_load.py for their separately qualified full-source cases.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
import cProfile
from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import importlib
import json
import os
from pathlib import Path
import platform
import pstats
import resource
import statistics
import struct
import subprocess
import sys
import time
from typing import Callable


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.runtime-hotspots"
WORKLOADS = ("fp64", "tile-fp64", "sha3", "audio", "source-load")
EXECUTORS = ("emulator-native", "simulator-python", "simulator-native")
MASK64 = (1 << 64) - 1
# Independent binary64 known answers, round-to-nearest-even.
FP_RESULTS = (0x4010000000000000, 0x4024000000000000,
              0x3FD5555555555555, 0x3FF6A09E667F3BCD)
FP_XOR = FP_RESULTS[0] ^ FP_RESULTS[1] ^ FP_RESULTS[2] ^ FP_RESULTS[3]
# Below the default Bank-0 data-stack boundary (0x80000), above the
# protected dictionary prefix, and outside these small compiled kernels.
SOURCE_ADDRESS, SOURCE1_ADDRESS, DESTINATION_ADDRESS = 0x20000, 0x20100, 0x20200
AUDIO_BYTES = 4096


class BenchmarkError(RuntimeError):
    pass


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise BenchmarkError(message)


def _bounded_int(lower: int, upper: int):
    def parse(value: str) -> int:
        try:
            result = int(value)
        except ValueError as exc:
            raise argparse.ArgumentTypeError("must be an integer") from exc
        if not lower <= result <= upper:
            raise argparse.ArgumentTypeError(f"must be between {lower} and {upper}")
        return result
    return parse


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workload", nargs="+", choices=WORKLOADS, default=list(WORKLOADS))
    parser.add_argument("--executor", nargs="+", choices=EXECUTORS, default=list(EXECUTORS))
    parser.add_argument("--iterations", type=_bounded_int(1, 100_000), default=64)
    parser.add_argument("--trials", type=_bounded_int(1, 10), default=3)
    parser.add_argument("--warmup", type=_bounded_int(0, 3), default=1)
    parser.add_argument("--timeout", type=_bounded_int(1, 120), default=30,
                        help="wall seconds per executor/workload subprocess (default: 30)")
    parser.add_argument("--attribution", action="store_true",
                        help="separate cProfile and native attribution run")
    parser.add_argument("--output", type=Path, help="also write the complete JSON report here")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def supported(workload: str, executor: str) -> bool:
    return executor != "emulator-native" or workload == "fp64"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _repository() -> dict:
    def git(*arguments):
        result = subprocess.run(
            ["git", "--no-optional-locks", *arguments], cwd=ROOT,
            capture_output=True, text=True, check=False,
        )
        return result.stdout.strip() if result.returncode == 0 else None
    status = git("status", "--porcelain=v1", "--untracked-files=all")
    return {"root": str(ROOT), "commit": git("rev-parse", "HEAD"),
            "dirty": None if status is None else bool(status), "status": status}


def _source_manifest() -> dict[str, str]:
    paths = {Path(__file__).resolve()}
    for directory in ("emulator", "simulator", "shared", "rich_terminal"):
        paths.update(path for path in (ROOT / directory).rglob("*")
                     if path.is_file() and path.suffix in (".py", ".cpp", ".h"))
    paths.update(path for path in ROOT.iterdir() if path.is_file()
                 and path.suffix in (".py", ".asm", ".f")
                 and not path.name.startswith("bench_"))
    return {str(path.relative_to(ROOT)): _sha256(path) for path in sorted(paths)}


def _native_artifact(executor: str) -> dict | None:
    if executor == "simulator-python":
        return None
    name = "_mp64_accel" if executor == "emulator-native" else "_megaforth_native"
    module = importlib.import_module(name)
    path = Path(module.__file__).resolve()
    _require(path.parent == ROOT, f"{name} was loaded outside this checkout: {path}")
    return {"module": name, "path": str(path), "bytes": path.stat().st_size,
            "sha256": _sha256(path)}


def _peak_rss_bytes() -> int:
    value = int(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss)
    return value if sys.platform == "darwin" else value * 1024


def _delta(before: dict, after: dict) -> dict:
    result = {}
    for name, value in after.items():
        if isinstance(value, dict):
            result[name] = _delta(before.get(name, {}), value)
        elif isinstance(value, (int, float)) and not isinstance(value, bool):
            result[name] = value - before.get(name, 0)
    return result


@contextmanager
def _native_profiling(enabled: bool):
    old = os.environ.get("MEGAFORTH_NATIVE_PROFILE")
    os.environ["MEGAFORTH_NATIVE_PROFILE"] = "1" if enabled else "0"
    try:
        yield
    finally:
        if old is None:
            os.environ.pop("MEGAFORTH_NATIVE_PROFILE", None)
        else:
            os.environ["MEGAFORTH_NATIVE_PROFILE"] = old


@dataclass
class PreparedWorkload:
    action: Callable
    observe: Callable
    close: Callable
    counters: Callable
    description: dict
    start_attribution: Callable | None = None
    stop_attribution: Callable | None = None


def _fp_source(iterations: int) -> bytes:
    return (
        f": HOTSPOT 0 {iterations} 0 DO "
        "0x3FF8000000000000 0x4004000000000000 F64+ "
        "0x4000000000000000 0x4008000000000000 0x4010000000000000 F64FMA XOR "
        "0x3FF0000000000000 0x4008000000000000 F64/ XOR "
        "0x4000000000000000 F64SQRT XOR + LOOP ;"
    ).encode("ascii")


def prepare_emulator_fp(iterations: int) -> PreparedWorkload:
    from asm import assemble
    from emulator.megapad64 import CSR_FPCSR
    from emulator.session import MachineSession
    from emulator.system import MegapadSystem

    source = """
loop:
    mov r1, r12
    fadd.d r1, r13
    mov r4, r14
    fma.d r4, r8, r9
    mov r5, r7
    fdiv.d r5, r9
    fsqrt.d r6, r8
    xor r1, r4
    xor r1, r5
    xor r1, r6
    add r11, r1
    dec r10
    cmpi r10, 0
    lbrne loop
    halt
"""
    code = bytes(assemble(source))
    system = MegapadSystem(ram_size=64 << 10, ext_mem_size=0, vram_size=0,
                           num_cores=1, num_clusters=0, worker_count=1)
    session = MachineSession(system)
    try:
        system.load_binary(0, code)
        session.boot()
        cpu = system.cpu
        for register, value in {
            7: 0x3FF0000000000000, 8: 0x4000000000000000,
            9: 0x4008000000000000, 10: iterations, 11: 0,
            12: 0x3FF8000000000000, 13: 0x4004000000000000,
            14: 0x4010000000000000,
        }.items():
            cpu.regs[register] = value
        cpu.csr_write(CSR_FPCSR, 0)
    except BaseException:
        session.close()
        raise

    def observe(result):
        expected = (FP_XOR * iterations) & MASK64
        _require(system.all_halted, "FP kernel did not halt inside its instruction budget")
        _require(cpu.regs[10] == 0 and cpu.regs[11] == expected,
                 "architectural FP iteration/checksum mismatch")
        _require(tuple(cpu.regs[index] for index in (1, 4, 5, 6)) ==
                 (FP_XOR, *FP_RESULTS[1:]), "architectural FP result bits differ")
        _require(cpu.csr_read(CSR_FPCSR) == 16, "architectural FPCSR differs")
        return {"checksum": expected, "fpcsr": 16, "fp_operations": 4 * iterations,
                "instructions": result.instructions_executed,
                "system_cycles": result.system_cycles_advanced,
                "stop_reason": result.system_stop_reason}

    return PreparedWorkload(
        action=lambda: system.run_batch_stats(iterations * 20 + 32),
        observe=observe, close=session.close, counters=lambda: {},
        description={"scope": "one-core native scheduler; assembled counted FP loop",
                     "program_sha256": hashlib.sha256(code).hexdigest(),
                     "operations_per_iteration": 4, "work_unit": "scalar_fp64_operation"},
        start_attribution=system.start_host_profile,
        stop_attribution=system.stop_host_profile,
    )


def prepare_semantic(workload: str, executor: str, iterations: int) -> PreparedWorkload:
    from simulator.memory import MMIO_BASE
    from simulator.runtime import MegaForthRuntime

    backend = executor.removeprefix("simulator-")
    runtime = MegaForthRuntime(execution_backend=backend)
    _require(runtime.execution_backend == backend, "requested semantic executor was not selected")
    before_words = len(runtime.dictionary.words)
    expected_stack: tuple[int, ...] = ()
    expected_memory: bytes | None = None
    description = {"scope": "hosted guest word dispatch", "work_unit": workload}
    if workload == "fp64":
        source = _fp_source(iterations)
        expected_stack = ((FP_XOR * iterations) & MASK64,)
        description["operations_per_iteration"] = 4
    elif workload == "tile-fp64":
        runtime.memory.write_bytes(SOURCE_ADDRESS, struct.pack("<8d", *range(1, 9)))
        runtime.memory.write_bytes(SOURCE1_ADDRESS, struct.pack("<8d", *range(2, 10)))
        runtime.tile.set_mode(7)
        runtime.tile.set_source0(SOURCE_ADDRESS)
        runtime.tile.set_source1(SOURCE1_ADDRESS)
        runtime.tile.set_destination(DESTINATION_ADDRESS)
        source = f": HOTSPOT {iterations} 0 DO TADD LOOP ;".encode("ascii")
        expected_memory = struct.pack("<8d", *range(3, 18, 2))
        description["lanes_per_iteration"] = 8
    elif workload == "sha3":
        message = bytes(range(256))
        runtime.memory.write_bytes(SOURCE_ADDRESS, message)
        source = (
            f": HOTSPOT 0 {iterations} 0 DO 0 SHA3-BEGIN + "
            f"{SOURCE_ADDRESS} 256 SHA3-UPDATE + "
            f"{DESTINATION_ADDRESS} SHA3-FINAL + SHA3-CLEAR + LOOP ;"
        ).encode("ascii")
        expected_stack = (0,)
        expected_memory = hashlib.sha3_256(message).digest()
        description["message_bytes_per_iteration"] = len(message)
    elif workload == "audio":
        pcm = bytes(range(256)) * (AUDIO_BYTES // 256)
        runtime.memory.write_bytes(SOURCE_ADDRESS, pcm)
        base = MMIO_BASE + 0xC00
        runtime.memory.write64(base + 8, SOURCE_ADDRESS)
        runtime.memory.write32(base + 16, len(pcm) // 2)
        source = f": HOTSPOT {iterations} 0 DO 1 {base} C! LOOP ;".encode("ascii")
        expected_memory = pcm
        description.update(scope="hosted guest MMIO; synchronous headless PCM capture",
                           pcm_bytes_per_iteration=len(pcm))
    elif workload == "source-load":
        source = b"\n".join(
            f": HS{index} {index} ;".encode("ascii") for index in range(iterations)
        ) + f"\n: HOTSPOT HS0 HS{iterations - 1} + ;\n".encode("ascii")
        description.update(scope="synthetic colon-source compilation; verification execution excluded",
                           definitions=iterations + 1, source_bytes=len(source))
    else:
        raise ValueError(f"unsupported semantic workload: {workload}")

    description["source_sha256"] = hashlib.sha256(source).hexdigest()
    budget = iterations * 200 + 10_000
    if workload == "source-load":
        action = lambda: runtime.evaluate(source, source_name="hotspot-source.f", step_budget=budget)
    else:
        runtime.evaluate(source, source_name="hotspot-kernel.f", step_budget=budget)
        _require(runtime.main_context.data.snapshot() == (), "preparation left a dirty stack")
        action = lambda: runtime.execute("HOTSPOT", step_budget=budget)

    def observe(result):
        observation = {"semantic_steps": result.semantic_steps}
        if workload == "source-load":
            _require(len(runtime.dictionary.words) - before_words == iterations + 1,
                     "source-load definition count differs")
            runtime.execute("HOTSPOT", step_budget=100)
            stack = (iterations - 1,)
            observation["definitions"] = iterations + 1
            observation["source_bytes"] = len(source)
        else:
            stack = expected_stack
        actual_stack = runtime.main_context.data.snapshot()
        _require(actual_stack == stack,
                 f"semantic result stack differs: actual={actual_stack!r}, expected={stack!r}")
        actual_returns = runtime.main_context.returns.snapshot()
        _require(actual_returns == (),
                 f"semantic return stack differs: actual={actual_returns!r}, expected=()")
        observation["stack"] = list(stack)
        if workload == "fp64":
            _require(runtime.scalar_float.fpcsr == 16, "semantic FPCSR differs")
            observation.update(checksum=stack[0], fpcsr=16, fp_operations=iterations * 4)
        if workload == "tile-fp64":
            _require(runtime.diagnostics.perf_tileops == iterations, "tile operation count differs")
            observation["tile_operations"] = iterations
        if expected_memory is not None:
            if workload == "audio":
                audio = runtime.memory.mmio.audio
                actual = audio.last_pcm
                _require(audio.generation == iterations and audio.error == 0,
                         "PCM submission count or error differs")
                _require((audio.last_frames, audio.last_channels, audio.last_rate) ==
                         (AUDIO_BYTES // 2, 1, 8000), "PCM metadata differs")
                observation["submissions"] = audio.generation
            else:
                actual = runtime.memory.read_bytes(DESTINATION_ADDRESS, len(expected_memory))
            _require(actual == expected_memory, f"{workload} output bytes differ")
            observation["output_sha256"] = hashlib.sha256(actual).hexdigest()
        return observation

    return PreparedWorkload(
        action=action, observe=observe,
        close=runtime.memory.mmio.audio.release_host_sink,
        counters=lambda: runtime.native_execution_stats,
        description=description,
    )


def prepare(workload: str, executor: str, iterations: int) -> PreparedWorkload:
    if not supported(workload, executor):
        raise ValueError(f"{workload} is not provided for {executor}")
    if executor == "emulator-native":
        return prepare_emulator_fp(iterations)
    return prepare_semantic(workload, executor, iterations)


def _profile_summary(profile: cProfile.Profile) -> dict:
    stats = pstats.Stats(profile).stats
    records = []
    fallbacks = {name: 0 for name in (
        "_step_python_fallback_in_memory_scope", "_sync_cs_to_py", "_sync_py_to_cs",
    )}
    for (filename, line, name), (primitive, total, own, cumulative, _callers) in stats.items():
        if name in fallbacks and filename.endswith("emulator/accel_wrapper.py"):
            fallbacks[name] += total
        try:
            label = str(Path(filename).relative_to(ROOT))
        except ValueError:
            label = filename
        records.append({"file": label, "line": line, "function": name,
                        "primitive_calls": primitive, "total_calls": total,
                        "own_seconds": own, "cumulative_seconds": cumulative})
    records.sort(key=lambda row: row["cumulative_seconds"], reverse=True)
    return {"python_fallback_and_sync_calls": fallbacks,
            "top_functions_by_cumulative_time": records[:25]}


def measure_once(workload: str, executor: str, iterations: int, *, attribution: bool) -> dict:
    with _native_profiling(attribution):
        setup_wall, setup_cpu = time.perf_counter(), time.process_time()
        fixture = prepare(workload, executor, iterations)
        setup = {"wall_seconds": time.perf_counter() - setup_wall,
                 "process_cpu_seconds": time.process_time() - setup_cpu}
        profile = cProfile.Profile() if attribution else None
        try:
            before = fixture.counters()
            if attribution and fixture.start_attribution:
                fixture.start_attribution()
            wall, cpu = time.perf_counter(), time.process_time()
            if profile is not None:
                profile.enable()
            try:
                result = fixture.action()
            finally:
                if profile is not None:
                    profile.disable()
            cpu_seconds, wall_seconds = time.process_time() - cpu, time.perf_counter() - wall
            after = fixture.counters()
            host_profile = (
                fixture.stop_attribution()
                if attribution and fixture.stop_attribution else None
            )
            # Capture counters before result checking; source-load validation
            # executes its generated word and must not inflate the load sample.
            observation = fixture.observe(result)
            record = {
                "wall_seconds": wall_seconds, "process_cpu_seconds": cpu_seconds,
                "process_lifetime_peak_rss_bytes": _peak_rss_bytes(),
                "setup": setup, "description": fixture.description,
                "guest": observation, "native_counter_delta": _delta(before, after),
            }
            if profile is not None:
                record["attribution"] = _profile_summary(profile)
                record["attribution"]["native_host_profile"] = host_profile
            return record
        finally:
            fixture.close()


def run_case(workload: str, executor: str, *, iterations: int, trials: int,
             warmup: int, attribution: bool) -> dict:
    artifact = _native_artifact(executor)
    for _ in range(warmup):
        measure_once(workload, executor, iterations, attribution=False)
    samples = [measure_once(workload, executor, iterations, attribution=False)
               for _ in range(trials)]
    expected = samples[0]["guest"]
    _require(all(sample["guest"] == expected for sample in samples),
             "guest observations differ across unprofiled trials")
    attributed = None
    if attribution:
        attributed = measure_once(workload, executor, iterations, attribution=True)
        _require(attributed["guest"] == expected, "profiling changed guest observations")
    if artifact is not None:
        _require(_sha256(Path(artifact["path"])) == artifact["sha256"],
                 "native binary changed during the case")
    return {"workload": workload, "executor": executor, "iterations": iterations,
            "warmup_instances": warmup, "native_artifact": artifact,
            "samples": samples, "attribution_run": attributed,
            "median_wall_seconds": statistics.median(row["wall_seconds"] for row in samples),
            "median_process_cpu_seconds": statistics.median(
                row["process_cpu_seconds"] for row in samples)}


def run_suite(args: argparse.Namespace) -> dict:
    repository, sources = _repository(), _source_manifest()
    cases, unsupported = [], []
    for workload in dict.fromkeys(args.workload):
        for executor in dict.fromkeys(args.executor):
            if not supported(workload, executor):
                unsupported.append({"workload": workload, "executor": executor})
                continue
            command = [sys.executable, str(Path(__file__).resolve()), "--worker",
                       "--workload", workload, "--executor", executor,
                       "--iterations", str(args.iterations), "--trials", str(args.trials),
                       "--warmup", str(args.warmup)]
            if args.attribution:
                command.append("--attribution")
            completed = subprocess.run(command, cwd=ROOT, capture_output=True, text=True,
                                       timeout=args.timeout, check=False)
            _require(completed.returncode == 0,
                     f"{workload}/{executor} failed: {completed.stderr.strip()}")
            cases.append(json.loads(completed.stdout))
    _require(bool(cases), "no supported workload/executor pair selected")
    _require(_source_manifest() == sources, "bound runtime sources changed during measurement")
    _require(_repository()["commit"] == repository["commit"], "repository commit changed during measurement")
    return {
        "schema": SCHEMA, "schema_version": 1,
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "repository": repository, "source_sha256": sources,
        "host": {"platform": platform.platform(), "machine": platform.machine(),
                 "python": sys.version, "python_executable": sys.executable,
                 "cpu_count": os.cpu_count()},
        "measurement": {
            "order": "sequential fresh subprocess per workload/executor",
            "trials": "fresh prepared instance; setup, validation and cleanup excluded",
            "warmup": "discarded fresh instances; no retained guest/JIT cache",
            "attribution": "separate fresh instance, excluded from timing summaries",
            "rss": "process lifetime high-water mark, not a per-trial allocation delta",
            "case_timeout_seconds": args.timeout,
            "scope": "bounded kernels and synthetic source; no Desktop or full boot claim",
        },
        "unsupported_pairs": unsupported, "cases": cases,
    }


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.worker and (len(args.workload) != 1 or len(args.executor) != 1):
        parser.error("worker requires one workload and one executor")
    try:
        if args.worker:
            report = run_case(args.workload[0], args.executor[0], iterations=args.iterations,
                              trials=args.trials, warmup=args.warmup, attribution=args.attribution)
        else:
            report = run_suite(args)
    except (OSError, RuntimeError, ValueError, subprocess.TimeoutExpired) as exc:
        print(f"benchmark failed: {exc}", file=sys.stderr)
        return 1
    rendered = json.dumps(report, indent=2, sort_keys=True) + "\n"
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(rendered, encoding="utf-8")
    print(rendered, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
