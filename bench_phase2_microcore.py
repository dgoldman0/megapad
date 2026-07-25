#!/usr/bin/env python3
"""Versioned Phase 2 baseline for one active accelerated microcore.

This diagnostic benchmark began as the element-5 ownership baseline.  Version
3 preserves that workload while recording both the element-6 native all-core
system scheduler and the selected all-ones CLUSTER_EN reset contract.  It
deliberately does not exercise cluster contention, hard-QoS eligibility,
equal-round-robin ordering among peers, shared engines, or strict-cycle
execution.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import platform
import statistics
import time
from datetime import datetime, timezone
from pathlib import Path

from asm import assemble
from megapad64 import MICRO_PER_CLUSTER
from system import MegapadSystem


REPORT_SCHEMA = "megapad.phase2-single-active-microcore-baseline"
REPORT_SCHEMA_VERSION = 3
STATE_SCHEMA = "megapad.phase2-single-active-microcore-state"
STATE_SCHEMA_VERSION = 3

RAM_SIZE = 1 << 16
CODE_BASE = 0x100
DEFAULT_INSTRUCTIONS = 1_000_000
QUICK_INSTRUCTIONS = 100_000

PROGRAM = assemble(
    """
loop:
    inc r1
    br loop
"""
)
PROGRAM_SHA256 = hashlib.sha256(PROGRAM).hexdigest()

QOS_AND_FAIRNESS_SCOPE = {
    "contention_exercised": False,
    "qos_claim": "excluded",
    "fairness_claim": "excluded",
    "architectural_rule_not_measured": (
        "hard QoS filters eligibility/reserved entitlement; equal "
        "round-robin orders simultaneously eligible peers; unused reserved "
        "capacity remains work-conserving"
    ),
}

EXPLICIT_EXCLUSIONS = [
    {
        "state_or_behavior": "R16-R31 and REX",
        "reason": (
            "element-5 behavior tests cover widened registers; this baseline "
            "keeps a common R0-R15 state surface"
        ),
    },
    {
        "state_or_behavior": (
            "multiple runnable microcores, bus contention, QoS eligibility, "
            "and equal-round-robin ordering"
        ),
        "reason": (
            "only one core is runnable and the program performs no data "
            "access"
        ),
    },
    {
        "state_or_behavior": (
            "cluster scratchpad, MUL/DIV, CRC, MEX, barrier, BIST, and MPU"
        ),
        "reason": (
            "cluster-shared resource arbitration is covered by the "
            "versioned element-6 cluster oracle"
        ),
    },
    {
        "state_or_behavior": "strict-cycle hardware CPI",
        "reason": (
            "heterogeneous cycle-bounded scheduling remains rejected; "
            "reported cycles are unbounded native-scheduler accounting"
        ),
    },
    {
        "state_or_behavior": "complete restorable machine snapshots",
        "reason": (
            "the canonical state is a behavior oracle, not a save/restore "
            "format"
        ),
    },
]


def _canonical_json_bytes(value: object) -> bytes:
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("ascii")


def _json_sha256(value: object) -> str:
    return hashlib.sha256(_canonical_json_bytes(value)).hexdigest()


def _blob_summary(data: bytes | bytearray | memoryview) -> dict:
    return {
        "size_bytes": len(data),
        "sha256": hashlib.sha256(data).hexdigest(),
    }


def _core_state(system: MegapadSystem, cpu) -> dict:
    return {
        "core_id": int(cpu.core_id),
        "profile": "micro" if cpu._cs.is_micro_core else "full",
        "accelerated_wrapper": bool(cpu._accel_backend),
        "main_bus_port_id": int(
            system._native_system.main_bus_port_for_requester(cpu.core_id)
        ),
        "common_gprs_r0_r15": [
            int(cpu.regs[index]) for index in range(16)
        ],
        "pc": int(cpu.pc),
        "psel": int(cpu.psel),
        "xsel": int(cpu.xsel),
        "spsel": int(cpu.spsel),
        "flags": int(cpu.flags_pack()),
        "halted": bool(cpu.halted),
        "idle": bool(cpu.idle),
        "cycle_count": int(cpu.cycle_count),
        "perf_enable": int(cpu.perf_enable),
        "perf_cycles": int(cpu.perf_cycles),
        "perf_stalls": int(cpu.perf_stalls),
        "ext_modifier": int(cpu._ext_modifier),
        "irq_ipi": bool(cpu.irq_ipi),
    }


def _main_bus_state(system: MegapadSystem) -> dict:
    snapshot = system._native_system._main_bus_snapshot()
    return {
        "schema_version": int(snapshot.schema_version),
        "port_count": int(snapshot.port_count),
        "last_grant": int(snapshot.last_grant),
        "reset_port_zero_credit": bool(snapshot.reset_port_zero_credit),
        "next_grant_sequence": int(snapshot.next_grant_sequence),
        "earliest_arbitration_cycle": int(
            snapshot.earliest_arbitration_cycle
        ),
        "served_last": bool(snapshot.served_last),
        "last_arbitration_cycle": (
            None
            if snapshot.last_arbitration_cycle is None
            else int(snapshot.last_arbitration_cycle)
        ),
        "active_grant": snapshot.active_grant is not None,
        "last_issue_sequences": [
            int(value) for value in snapshot.last_issue_sequences
        ],
        "sticky_bus_errors": [
            int(value) for value in snapshot.sticky_bus_errors
        ],
        "cycle_execution_pending": bool(
            system._native_system.cycle_execution_pending
        ),
        "pending_request_count": len(
            system._native_system._cycle_pending_bus_requests()
        ),
    }


def _build_workload() -> tuple[MegapadSystem, object]:
    system = MegapadSystem(
        ram_size=RAM_SIZE,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(CODE_BASE, PROGRAM)
    system.boot(entry=0)
    for core in system.cores:
        core.halted = True
        core.idle = False
        core.flag_i = 0

    micro = system.clusters[0].cores[0]
    micro.pc = CODE_BASE
    micro.halted = False

    def reject_python_fallback():
        raise AssertionError(
            "the retained benchmark loop crossed into Python fallback"
        )

    micro._step_python_fallback_in_memory_scope = reject_python_fallback
    return system, micro


def _observe(
    system: MegapadSystem,
    micro,
    stats,
    *,
    instruction_budget: int,
    native_batch_runs_before: int,
    native_dispatches_before: int,
) -> dict:
    owner = system._native_system
    workload = {
        "instruction_budget": instruction_budget,
        "program_sha256": PROGRAM_SHA256,
        "active_global_core_id": int(micro.core_id),
        "active_local_microcore_index": 0,
        "cluster_enable_policy": (
            "all_ones_reset_then_host_selects_one_runnable_core"
        ),
        "cluster_enable_mask": int(system.sysinfo.cluster_en),
        "other_cores_halted_by_host_for_single_core_baseline": True,
        "retained_native_program": True,
        "runnable_core_count": 1,
        "contention_exercised": False,
    }
    execution = {
        "instructions_executed": int(stats.instructions_executed),
        "system_cycles_advanced": int(stats.system_cycles_advanced),
        "per_core_instructions": [
            int(value) for value in stats.per_core_instructions
        ],
        "per_core_cycles": [
            int(value) for value in stats.per_core_cycles
        ],
        "per_core_dispatches": [
            int(value) for value in stats.per_core_dispatches
        ],
        "native_scheduler": bool(stats.native_scheduler),
        "system_stop_reason": str(stats.system_stop_reason),
        "scheduler_cursor": int(system._scheduler_cursor),
        "authoritative_system_cycles": int(owner.system_cycles),
        "native_batch_runs_counter_delta": (
            int(owner.native_batch_runs) - native_batch_runs_before
        ),
        "native_dispatches_counter_delta": (
            int(owner.native_dispatches) - native_dispatches_before
        ),
        "python_fallback_instantiated": micro._py_fallback is not None,
    }
    topology = {
        "num_full_cores": int(system.num_full_cores),
        "num_clusters": int(system.num_clusters),
        "microcores_per_cluster": MICRO_PER_CLUSTER,
        "num_micro_cores": int(system.num_micro_cores),
        "num_all_cores": int(system.num_cores),
        "active_core_is_system_owned_micro_profile": bool(
            micro._cs.is_micro_core and
            micro._cs is owner.micro_core(0)
        ),
        "cluster_enable_mask": int(system.sysinfo.cluster_en),
        "cluster_enabled": bool(system.clusters[0].enabled),
    }
    canonical_state = {
        "schema": STATE_SCHEMA,
        "schema_version": STATE_SCHEMA_VERSION,
        "topology": topology,
        "workload": workload,
        "execution": execution,
        "cores": [_core_state(system, cpu) for cpu in system.cores],
        "memory": {
            "shared_ram": _blob_summary(system._shared_mem),
        },
        "main_bus": _main_bus_state(system),
    }
    canonical_state_sha256 = _json_sha256(canonical_state)
    behavior_oracle_sha256 = _json_sha256(
        {
            "state_schema": STATE_SCHEMA,
            "state_schema_version": STATE_SCHEMA_VERSION,
            "canonical_state_sha256": canonical_state_sha256,
            "workload": workload,
            "qos_and_fairness_scope": QOS_AND_FAIRNESS_SCOPE,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
        }
    )
    return {
        "state_schema": STATE_SCHEMA,
        "state_schema_version": STATE_SCHEMA_VERSION,
        "canonical_state_sha256": canonical_state_sha256,
        "behavior_oracle_sha256": behavior_oracle_sha256,
        "component_sha256": {
            "topology": _json_sha256(topology),
            "execution": _json_sha256(execution),
            "cores": _json_sha256(canonical_state["cores"]),
            "memory": _json_sha256(canonical_state["memory"]),
            "main_bus": _json_sha256(canonical_state["main_bus"]),
        },
        "canonical_state": canonical_state,
    }


def run_sample(instruction_budget: int) -> dict:
    if instruction_budget <= 0:
        raise ValueError("instruction budget must be positive")

    system, micro = _build_workload()
    owner = system._native_system
    native_batch_runs_before = int(owner.native_batch_runs)
    native_dispatches_before = int(owner.native_dispatches)

    wall_start = time.perf_counter()
    process_start = time.process_time()
    stats = system.run_batch_stats(instruction_budget)
    process_time_s = time.process_time() - process_start
    wall_time_s = time.perf_counter() - wall_start

    observation = _observe(
        system,
        micro,
        stats,
        instruction_budget=instruction_budget,
        native_batch_runs_before=native_batch_runs_before,
        native_dispatches_before=native_dispatches_before,
    )
    return {
        "wall_time_s": wall_time_s,
        "process_cpu_time_s": process_time_s,
        "selected_microcore_instructions_per_s": (
            stats.instructions_executed / wall_time_s
            if wall_time_s
            else None
        ),
        "observation": observation,
    }


def run_report(
    *,
    instructions: int = DEFAULT_INSTRUCTIONS,
    repeats: int = 3,
    warmups: int = 1,
    warmup_instructions: int = QUICK_INSTRUCTIONS,
) -> dict:
    if instructions <= 0:
        raise ValueError("instructions must be positive")
    if repeats <= 0:
        raise ValueError("repeats must be positive")
    if warmups < 0:
        raise ValueError("warmups cannot be negative")
    if warmup_instructions <= 0:
        raise ValueError("warmup instructions must be positive")

    for _ in range(warmups):
        run_sample(warmup_instructions)
    samples = [run_sample(instructions) for _ in range(repeats)]
    rates = [
        sample["selected_microcore_instructions_per_s"]
        for sample in samples
    ]
    state_hashes = [
        sample["observation"]["canonical_state_sha256"]
        for sample in samples
    ]
    oracle_hashes = [
        sample["observation"]["behavior_oracle_sha256"]
        for sample in samples
    ]
    return {
        "schema": REPORT_SCHEMA,
        "schema_version": REPORT_SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "host": {
            "python": platform.python_version(),
            "platform": platform.platform(),
            "machine": platform.machine(),
        },
        "configuration": {
            "instructions": instructions,
            "repeats": repeats,
            "warmups": warmups,
            "warmup_instructions": warmup_instructions,
        },
        "semantics": {
            "classification": "behavior_oracle_and_diagnostic_baseline",
            "execution_engine": (
                "native microcore scalar execution under the native "
                "all-core system scheduler"
            ),
            "native_scheduler_expected": True,
            "qos_and_fairness_scope": QOS_AND_FAIRNESS_SCOPE,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
        },
        "determinism": {
            "canonical_state_matches": len(set(state_hashes)) == 1,
            "behavior_oracle_matches": len(set(oracle_hashes)) == 1,
            "canonical_state_sha256": state_hashes,
            "behavior_oracle_sha256": oracle_hashes,
        },
        "throughput": {
            "selected_microcore_instructions_per_s_median":
                statistics.median(rates),
            "selected_microcore_instructions_per_s_min": min(rates),
            "selected_microcore_instructions_per_s_max": max(rates),
        },
        "samples": samples,
    }


def _positive_int(text: str) -> int:
    value = int(text)
    if value <= 0:
        raise argparse.ArgumentTypeError("value must be positive")
    return value


def _nonnegative_int(text: str) -> int:
    value = int(text)
    if value < 0:
        raise argparse.ArgumentTypeError("value cannot be negative")
    return value


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Run the versioned Phase 2 single-active-microcore baseline."
        )
    )
    parser.add_argument(
        "--instructions",
        type=_positive_int,
        default=DEFAULT_INSTRUCTIONS,
    )
    parser.add_argument("--repeats", type=_positive_int, default=3)
    parser.add_argument("--warmups", type=_nonnegative_int, default=1)
    parser.add_argument(
        "--warmup-instructions",
        type=_positive_int,
        default=QUICK_INSTRUCTIONS,
    )
    parser.add_argument(
        "--quick",
        action="store_true",
        help="100k instructions, one repeat, and no warmup",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        help="print the complete JSON report",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="also write the complete JSON report to this path",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if args.quick:
        args.instructions = QUICK_INSTRUCTIONS
        args.repeats = 1
        args.warmups = 0
    report = run_report(
        instructions=args.instructions,
        repeats=args.repeats,
        warmups=args.warmups,
        warmup_instructions=args.warmup_instructions,
    )
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        throughput = report["throughput"][
            "selected_microcore_instructions_per_s_median"
        ]
        print(
            f"Phase 2 single microcore: {throughput:,.0f} instructions/s"
        )
        print(
            "Deterministic state/oracle: "
            f"{report['determinism']['canonical_state_matches']}/"
            f"{report['determinism']['behavior_oracle_matches']}"
        )
        if args.output is not None:
            print(f"JSON report: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
