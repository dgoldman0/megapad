#!/usr/bin/env python3
"""Bounded functional-round and strict-clock wake/contention comparisons.

The two models are reported separately. Only strict-clock replay reports
modeled wake observations, at one-cycle settled boundaries. Host wall/process
times come from separate unobserved trials. These small assembled fixtures do
not reproduce an external solver or establish physical RTL latency.
"""

from __future__ import annotations

import argparse
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import platform
import statistics
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parent
SCHEMA = "megapad.execution-timing"
MODELS = ("instruction_batched", "strict_shared_clock")
WORKLOADS = ("wake", "compute", "memory")
COUNTS = (1, 2, 4)
SLOT_BASE = 0x1000
RECEIVER_ADDRESS = 0x100
BACKGROUND_ADDRESS = 0x200


class BenchmarkError(RuntimeError):
    pass


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise BenchmarkError(message)


def _bounded(value, label, minimum, maximum):
    if type(value) is not int or not minimum <= value <= maximum:
        raise ValueError(f"{label} must be an integer in {minimum}..{maximum}")


def _argument_int(minimum, maximum):
    def parse(value):
        try:
            result = int(value)
            _bounded(result, "value", minimum, maximum)
        except ValueError as exc:
            raise argparse.ArgumentTypeError(str(exc)) from exc
        return result
    return parse


def build_parser():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--workload", nargs="+", choices=WORKLOADS, default=list(WORKLOADS))
    parser.add_argument("--model", nargs="+", choices=MODELS, default=list(MODELS))
    parser.add_argument("--cores", nargs="+", type=int, choices=COUNTS, default=list(COUNTS))
    parser.add_argument("--workers", nargs="+", type=int, choices=COUNTS, default=list(COUNTS))
    parser.add_argument("--cache", choices=("warm", "cold"), default="warm")
    parser.add_argument("--iterations", type=_argument_int(4, 4096), default=256)
    parser.add_argument("--trials", type=_argument_int(1, 5), default=3)
    parser.add_argument("--warmup", type=_argument_int(0, 2), default=1)
    parser.add_argument("--timeout", type=_argument_int(1, 120), default=30)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    return parser


def supported(workload, cores):
    return workload != "wake" or cores in (2, 4)


def _compute_source(memory=False):
    transfer = "str r8, r4\nldn r5, r8" if memory else "mov r5, r4"
    return f"""
loop:
    inc r4
    {transfer}
    add r6, r5
    dec r10
    cmpi r10, 0
    lbrne loop
    halt
"""


def _warm_code(system, images):
    # Explicit fixture preparation, excluded from measurement. Ordinary guest
    # code writes still retain the architecture's noncoherent cache behavior.
    for cpu in system.cores:
        valid_bytes, tags, data_bytes = cpu._cs.icache_snapshot()
        valid, tags, data = bytearray(valid_bytes), list(tags), bytearray(data_bytes)
        for address, code in images:
            for line in range(address & ~15, (address + len(code) + 15) & ~15, 16):
                index = (line >> 4) & 255
                valid[index] = 1
                tags[index] = line >> 12
                data[index * 16:index * 16 + 16] = cpu.mem[line:line + 16]
        cpu._cs.icache_restore(bytes(valid), tags, bytes(data))


@dataclass
class Fixture:
    system: object
    workload: str
    iterations: int
    per_core_iterations: tuple[int, ...]
    instruction_budget: int
    cycle_budget: int
    program_sha256: str

    def guest(self):
        values = []
        for index, cpu in enumerate(self.system.cores):
            count = self.per_core_iterations[index]
            if self.workload == "wake" and index == 1:
                expected = (1, 0, 0, 0)
            elif self.workload == "wake" and index == 0:
                expected = (count, 0, 0, 0)
            else:
                expected = (count, count, count * (count + 1) // 2, 0)
            actual = tuple(int(cpu.regs[register]) for register in (4, 5, 6, 10))
            _require(actual == expected, f"core {index} result mismatch: {actual} != {expected}")
            _require(cpu.halted and not cpu.idle, f"core {index} did not halt")
            values.append(actual)
        memory = []
        for index in range(len(self.system.cores)):
            address = SLOT_BASE + 8 * index
            actual = int.from_bytes(self.system.cpu.mem[address:address + 8], "little")
            expected = (self.per_core_iterations[index] if self.workload == "memory"
                        else int(self.workload == "wake" and index == 1))
            _require(actual == expected, f"core {index} data publication mismatch")
            memory.append(actual)
        pending = int(self.system._native_system.ipi_pending_mask(1)) if self.workload == "wake" else 0
        _require(pending == int(self.workload == "wake"), "unexpected IPI state")
        return {"register_results": values, "data_cells": memory, "pending_ipi_mask": pending}


@contextmanager
def prepare(workload, cores, workers, iterations, cache="warm"):
    if workload not in WORKLOADS or not supported(workload, cores):
        raise ValueError("unsupported workload/core combination")
    if cores not in COUNTS or workers not in COUNTS:
        raise ValueError("cores and workers must be 1, 2, or 4")
    _bounded(iterations, "iterations", 4, 4096)
    if cache not in ("warm", "cold"):
        raise ValueError("cache must be warm or cold")
    from asm import assemble
    from emulator.session import MachineSession
    from emulator.system import MegapadSystem

    if workload == "wake":
        sender = bytes(assemble("""
    csrw 0x22, r1
loop:
    inc r4
    dec r10
    cmpi r10, 0
    lbrne loop
    halt
"""))
        receiver = bytes(assemble("ldi r4, 1\nstr r8, r4\nhalt"))
        background = bytes(assemble(_compute_source()))
        images = [(0, sender), (RECEIVER_ADDRESS, receiver), (BACKGROUND_ADDRESS, background)]
        counts = tuple(0 if index == 1 else iterations for index in range(cores))
    else:
        images = [(0, bytes(assemble(_compute_source(workload == "memory"))))]
        # Fixed total work permits a guest-core scaling comparison. Host lane
        # count is an independent axis and never changes these assignments.
        counts = tuple(iterations // cores + int(index < iterations % cores) for index in range(cores))
    digest = hashlib.sha256()
    for address, code in images:
        digest.update(address.to_bytes(8, "little"))
        digest.update(len(code).to_bytes(8, "little"))
        digest.update(code)
    system = MegapadSystem(ram_size=64 << 10, num_cores=cores, num_clusters=0,
                           hbw_size=0, ext_mem_size=0, vram_size=0,
                           worker_count=workers, realtime_clock=False)
    with MachineSession(system) as session:
        for address, code in images:
            system.load_binary(address, code)
        session.boot()
        for index, cpu in enumerate(system.cores):
            cpu.halted = False
            cpu.idle = False
            cpu.flag_i = False
            cpu.pc = 0
            for register in (4, 5, 6):
                cpu.regs[register] = 0
            cpu.regs[8] = SLOT_BASE + 8 * index
            cpu.regs[10] = counts[index]
            if workload == "wake":
                if index == 0:
                    cpu.regs[1] = 1
                elif index == 1:
                    cpu.pc = RECEIVER_ADDRESS
                    cpu.idle = True
                else:
                    cpu.pc = BACKGROUND_ADDRESS
        if cache == "warm":
            _warm_code(system, images)
        instruction_budget = 8 * sum(counts) + 8 * cores
        yield Fixture(system, workload, iterations, counts, instruction_budget,
                      instruction_budget * 32 + 1024, digest.hexdigest())


def _latency_interval(start, end):
    return {"minimum": max(0, end[0] - start[1]),
            "maximum": max(0, end[1] - start[0])}


def execute(fixture, model, *, cycle_slice=None, observe_wake=False):
    if model not in MODELS:
        raise ValueError("unknown timing model")
    if cycle_slice is not None:
        _bounded(cycle_slice, "cycle slice", 1, fixture.cycle_budget)
        if model != "strict_shared_clock":
            raise ValueError("cycle slices require strict_shared_clock")
    if observe_wake and (model != "strict_shared_clock" or cycle_slice != 1 or fixture.workload != "wake"):
        raise ValueError("wake observations require a one-cycle strict wake replay")
    owner = fixture.system._native_system
    start_cycle = int(owner.system_cycles)
    instructions = cycles = calls = rounds = 0
    per_core_instructions = [0] * len(fixture.system.cores)
    per_core_cycles = [0] * len(fixture.system.cores)
    observations = {}
    last = None
    # Both work budgets and invocation count are finite. A stalled fixture is
    # a failed benchmark, never a valid latency or throughput sample.
    for _ in range(fixture.cycle_budget + 1 if cycle_slice else 8):
        remaining_instructions = fixture.instruction_budget - instructions
        remaining_cycles = fixture.cycle_budget - cycles
        _require(remaining_instructions > 0 and remaining_cycles > 0,
                 "fixture exhausted its bounded execution allowance")
        previous_cycle = int(owner.system_cycles)
        if model == "strict_shared_clock":
            last = fixture.system.run_cycle_batch(
                min(cycle_slice or remaining_cycles, remaining_cycles),
                max_instructions=remaining_instructions,
            )
        else:
            # Retain a large instruction request. Reducing this to one would
            # change functional-round wake cadence rather than observe it.
            last = fixture.system.run_batch_stats(remaining_instructions)
        _require(last.timing_model == model, "execution returned the wrong timing identity")
        _require(last.models_shared_clock_latency == (model == "strict_shared_clock"),
                 "execution returned inconsistent latency eligibility")
        instructions += int(last.instructions_executed)
        cycles += int(last.system_cycles_advanced)
        rounds += int(last.native_rounds)
        calls += 1
        for index in range(len(per_core_instructions)):
            per_core_instructions[index] += int(last.per_core_instructions[index])
            per_core_cycles[index] += int(last.per_core_cycles[index])
        if observe_wake:
            interval = (previous_cycle, int(owner.system_cycles))
            _require(0 <= interval[1] - interval[0] <= 1,
                     "one-cycle replay advanced beyond its observation bound")
            if owner.ipi_pending_mask(1) & 1:
                observations.setdefault("ipi_asserted", interval)
            if not fixture.system.cores[1].idle:
                observations.setdefault("receiver_runnable", interval)
            if fixture.system.cpu.mem[SLOT_BASE + 8] == 1:
                observations.setdefault("receiver_marker_committed", interval)
        if fixture.system.all_halted and not owner.cycle_execution_pending:
            break
        _require(last.system_stop_reason in ("cycle_limit", "instruction_limit"),
                 f"unexpected unfinished stop: {last.system_stop_reason}")
    else:
        raise BenchmarkError("fixture exceeded its bounded invocation count")
    _require(last is not None and last.system_stop_reason == "all_halted", "fixture did not terminate")
    _require(cycles == int(owner.system_cycles) - start_cycle, "shared clock accounting mismatch")
    _require(instructions == sum(per_core_instructions), "instruction accounting mismatch")
    result = {"timing_model": model,
              "models_shared_clock_latency": model == "strict_shared_clock",
              "instructions": instructions, "system_cycles": cycles,
              "per_core_instructions": per_core_instructions, "per_core_cycles": per_core_cycles,
              "per_core_architectural_cycles": [int(cpu.cycle_count) for cpu in fixture.system.cores],
              "calls": calls, "native_rounds": rounds, "stop_reason": last.system_stop_reason}
    if observe_wake:
        _require(set(observations) == {"ipi_asserted", "receiver_runnable", "receiver_marker_committed"},
                 "wake replay missed a required milestone")
        _require(observations["ipi_asserted"][0] <= observations["receiver_runnable"][1]
                 and observations["receiver_runnable"][0] <= observations["receiver_marker_committed"][1],
                 "wake milestones violate causal ordering")
        result["wake_observations"] = {
            "scope": "masked IPI wake from IDL; no interrupt-vector entry",
            "maximum_observation_interval_cycles": 1,
            "milestone_cycle_intervals": observations,
            "ipi_to_runnable_cycles": _latency_interval(observations["ipi_asserted"], observations["receiver_runnable"]),
            "ipi_to_marker_cycles": _latency_interval(observations["ipi_asserted"], observations["receiver_marker_committed"]),
        }
    return result


def _observable(execution):
    return {key: execution[key] for key in (
        "timing_model", "models_shared_clock_latency", "instructions", "system_cycles",
        "per_core_instructions", "per_core_cycles", "per_core_architectural_cycles", "stop_reason",
    )}


def run_case(workload, model, cores, workers, *, iterations=256, trials=3, warmup=1, cache="warm"):
    _bounded(trials, "trials", 1, 5)
    _bounded(warmup, "warmup", 0, 2)
    samples = []
    guest = reference = None
    for index in range(warmup + trials):
        with prepare(workload, cores, workers, iterations, cache) as fixture:
            wall_start, process_start = time.perf_counter(), time.process_time()
            execution = execute(fixture, model)
            process_seconds = time.process_time() - process_start
            wall_seconds = time.perf_counter() - wall_start
            observed = fixture.guest()
            if reference is None:
                guest, reference = observed, _observable(execution)
            _require(observed == guest and _observable(execution) == reference,
                     "fresh trials disagree on guest results or accounting")
            description = {"program_sha256": fixture.program_sha256,
                           "per_core_iterations": fixture.per_core_iterations,
                           "instruction_allowance": fixture.instruction_budget,
                           "cycle_allowance": fixture.cycle_budget}
            if index >= warmup:
                samples.append({"wall_seconds": wall_seconds, "process_seconds": process_seconds,
                                "execution": execution})
    replay = None
    if model == "strict_shared_clock":
        with prepare(workload, cores, workers, iterations, cache) as fixture:
            replay = execute(fixture, model, cycle_slice=1, observe_wake=workload == "wake")
            _require(fixture.guest() == guest and _observable(replay) == reference,
                     "one-cycle strict replay differs from unobserved trials")
    return {"workload": workload, "timing_model": model, "cores": cores, "workers": workers,
            "cache": cache, "iterations": iterations, "discarded_warmups": warmup,
            "description": description, "guest": guest, "samples": samples,
            "median_wall_seconds": statistics.median(item["wall_seconds"] for item in samples),
            "median_process_seconds": statistics.median(item["process_seconds"] for item in samples),
            "strict_replay": replay,
            "latency_interpretation": ("strict emulator observation intervals" if model == "strict_shared_clock"
                                       else "functional round accounting; no shared-clock latency result")}


def compare_workers(cases):
    groups = {}
    for case in cases:
        key = (case["workload"], case["timing_model"], case["cores"], case["cache"], case["iterations"])
        groups.setdefault(key, []).append(case)
    comparisons = []
    for key, members in groups.items():
        baseline = next((case for case in members if case["workers"] == 1), None)
        equivalent = baseline is not None and all(
            case["guest"] == baseline["guest"]
            and _observable(case["samples"][0]["execution"]) == _observable(baseline["samples"][0]["execution"])
            and (case["strict_replay"] or {}).get("wake_observations") ==
                (baseline["strict_replay"] or {}).get("wake_observations")
            for case in members
        )
        comparisons.append({"workload": key[0], "timing_model": key[1], "cores": key[2],
                            "workers": [case["workers"] for case in members],
                            "one_worker_reference_available": baseline is not None,
                            "guest_and_clock_equivalent": equivalent})
        if baseline is not None:
            _require(equivalent, "host worker count changed guest results or model accounting")
    return comparisons


def main(argv=None):
    args = build_parser().parse_args(argv)
    if args.worker:
        _require(all(len(getattr(args, field)) == 1 for field in ("workload", "model", "cores", "workers")),
                 "worker requires one exact case")
        print(json.dumps(run_case(args.workload[0], args.model[0], args.cores[0], args.workers[0],
                                  iterations=args.iterations, trials=args.trials, warmup=args.warmup, cache=args.cache)))
        return
    cases, unsupported = [], []
    for workload in args.workload:
        for cores in args.cores:
            if not supported(workload, cores):
                unsupported.append({"workload": workload, "cores": cores, "reason": "IPI peer wake needs at least two cores"})
                continue
            for model in args.model:
                for workers in args.workers:
                    command = [sys.executable, str(Path(__file__).resolve()), "--worker", "--workload", workload,
                               "--model", model, "--cores", str(cores), "--workers", str(workers),
                               "--iterations", str(args.iterations), "--trials", str(args.trials),
                               "--warmup", str(args.warmup), "--cache", args.cache]
                    child = subprocess.run(command, cwd=ROOT, capture_output=True, text=True, timeout=args.timeout)
                    _require(child.returncode == 0, f"case failed: {workload}/{model}/{cores}/{workers}\n{child.stderr}")
                    cases.append(json.loads(child.stdout))
    import _mp64_accel
    from bench_runtime_hotspots import _repository, _source_manifest, _sha256
    sources = _source_manifest()
    sources[Path(__file__).name] = _sha256(Path(__file__).resolve())
    artifact = Path(_mp64_accel.__file__).resolve()
    _require(artifact.parent == ROOT, "native artifact is outside the current checkout")
    report = {"schema": SCHEMA, "schema_version": 1, "generated_at": datetime.now(timezone.utc).isoformat(),
              "repository": _repository(), "source_sha256": sources,
              "native_artifact": {"path": str(artifact), "sha256": _sha256(artifact)},
              "host": {"python": sys.version, "platform": platform.platform()},
              "measurement": {"timed": "fresh instances; setup and result validation excluded; no wake observers",
                              "strict_replay": "separate untimed one-cycle API replay; event times bounded by observed frontiers",
                              "scaling": "compute/memory divide fixed total iterations; wake tail per active sender/background core",
                              "limits": "no external solver, BIOS boot, microcores, ISR latency, physical RTL or 4x claim",
                              "case_timeout_seconds": args.timeout},
              "unsupported": unsupported, "cases": cases, "worker_comparisons": compare_workers(cases)}
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output:
        args.output.write_text(encoded + "\n", encoding="utf-8")
    print(encoded)


if __name__ == "__main__":
    main()
