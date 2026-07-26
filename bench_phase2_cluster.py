#!/usr/bin/env python3
"""Versioned Phase 2 oracle for all-core and cluster-resource scheduling.

The benchmark records two deliberately small deterministic traces:

* seventeen one-instruction slices across the advertised 4+3x4 topology;
* three CRC arbitration rounds across two independent reduced clusters.

Timing is diagnostic and excluded from the behavior hashes.  The oracle makes
no strict-cycle, main-bus reservation, or RTL-latency claim.
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
from megapad64 import CSR_CRC_MODE, MICRO_PER_CLUSTER
from system import MegapadSystem


REPORT_SCHEMA = "megapad.phase2-cluster-resource-baseline"
REPORT_SCHEMA_VERSION = 1
STATE_SCHEMA = "megapad.phase2-cluster-resource-state"
STATE_SCHEMA_VERSION = 1

MILESTONE = {
    "phase": 2,
    "element": 6,
    "elements_total": 7,
    "label": "Phase 2 \u2014 element 6 of 7",
}

ARBITRATION_CONTRACT = {
    "hard_qos_role": (
        "determines must/may eligibility and reserved entitlement only"
    ),
    "simultaneously_eligible_peer_order": "equal_round_robin",
    "unused_reserved_capacity": "work_conserving",
    "best_effort_weights": "none",
    "secondary_ordering_biases": [],
}

MEASURED_CLAIMS = [
    "equal cyclic retirement budgets among runnable advertised cores",
    (
        "equal round-robin among requests pending at the same unbounded "
        "cluster boundary"
    ),
    (
        "CRC transaction ownership removes nonowners from eligibility "
        "without retiring their instructions"
    ),
    "cluster-local CRC arbitration and state isolation",
]

EXPLICIT_EXCLUSIONS = [
    {
        "state_or_behavior": (
            "general main-bus hard QoS, reservations, and transfer of "
            "unused reserved capacity"
        ),
        "reason": (
            "the oracle records the architectural rule but configures no "
            "main-bus reservation pressure"
        ),
    },
    {
        "state_or_behavior": (
            "same-hardware-cycle simultaneity, request persistence, exact "
            "shared-engine latency, and RTL CPI"
        ),
        "reason": (
            "resource requests meet at cooperative unbounded instruction "
            "boundaries rather than a strict-cycle fabric"
        ),
    },
    {
        "state_or_behavior": (
            "barrier pulse timing and scratchpad byte-lane RTL parity"
        ),
        "reason": (
            "the current RTL and emulator contracts differ; neither surface "
            "is needed by these traces"
        ),
    },
    {
        "state_or_behavior": (
            "guest I-cache invalidation and RTL trace closure"
        ),
        "reason": "those contracts belong to Phase 2 element 7",
    },
]

RESOURCE_NAMES = ("bus", "mul_div", "crc", "sha", "mex")
ALL_CORE_CODE_BASE = 0x100
ALL_CORE_PROGRAM = assemble(
    """
    inc r1
    inc r1
    halt
"""
)
ALL_CORE_PROGRAM_SHA256 = hashlib.sha256(ALL_CORE_PROGRAM).hexdigest()

CRC_PROGRAMS = {
    "cluster0_local0": assemble("crc.mode 0\nhalt"),
    "cluster0_local1": assemble("crc.mode 1\ncrc.fin r4, r0\nhalt"),
    "cluster1_local0": assemble("crc.mode 2\nhalt"),
    "cluster1_local1": assemble("crc.mode 2\ncrc.fin r4, r0\nhalt"),
}
CRC_PROGRAM_BASES = {
    "cluster0_local0": 0x100,
    "cluster0_local1": 0x180,
    "cluster1_local0": 0x200,
    "cluster1_local1": 0x280,
}


def _canonical_json_bytes(value: object) -> bytes:
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("ascii")


def _json_sha256(value: object) -> str:
    return hashlib.sha256(_canonical_json_bytes(value)).hexdigest()


def _one_hot_winner(values) -> int:
    winners = [
        index for index, value in enumerate(values)
        if int(value) == 1
    ]
    if len(winners) != 1 or sum(int(value) for value in values) != 1:
        raise RuntimeError("expected exactly one retired instruction")
    return winners[0]


def _stats_trace(stats) -> dict:
    instructions = [
        int(value) for value in stats.per_core_instructions
    ]
    return {
        "winner_core_ids": [
            index for index, value in enumerate(instructions) if value
        ],
        "instructions_executed": int(stats.instructions_executed),
        "system_cycles_advanced": int(stats.system_cycles_advanced),
        "per_core_instructions": instructions,
        "per_core_cycles": [
            int(value) for value in stats.per_core_cycles
        ],
        "per_core_dispatches": [
            int(value) for value in stats.per_core_dispatches
        ],
        "native_scheduler": bool(stats.native_scheduler),
        "native_rounds": int(stats.native_rounds),
        "native_continuations": int(stats.native_continuations),
        "system_stop_reason": str(stats.system_stop_reason),
    }


def _arbiter_snapshot(system: MegapadSystem, cluster_index: int) -> dict:
    raw = system._native_system._cluster_arbiter_snapshot(cluster_index)
    return {
        "schema_version": int(raw["schema_version"]),
        "cluster_id": int(raw["cluster_id"]),
        "global_id_base": int(raw["global_id_base"]),
        "core_count": int(raw["core_count"]),
        "core_index_space": "cluster_local",
        "round_robin_cursors": {
            name: int(raw["last_grants"][name])
            for name in RESOURCE_NAMES
        },
        "grant_counts": {
            name: int(raw["grant_counts"][name])
            for name in RESOURCE_NAMES
        },
        "grant_sequence": int(raw["grant_sequence"]),
        "crc_locked": bool(raw["crc_locked"]),
        "crc_lock_owner": (
            None
            if int(raw["crc_lock_owner"]) < 0
            else int(raw["crc_lock_owner"])
        ),
        "sha_locked": bool(raw["sha_locked"]),
        "sha_lock_owner": (
            None
            if int(raw["sha_lock_owner"]) < 0
            else int(raw["sha_lock_owner"])
        ),
    }


def _crc_snapshot(system: MegapadSystem, cluster_index: int) -> dict:
    raw = system._native_system._cluster_crc_snapshot(cluster_index)
    return {
        "acc": int(raw["acc"]),
        "mode": int(raw["mode"]),
        "locked": bool(raw["locked"]),
        "owner_local_core": (
            None if raw["owner"] is None else int(raw["owner"])
        ),
    }


def _all_core_trace(*, worker_count: int = 1) -> dict:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=4,
        num_clusters=3,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    system.load_binary(ALL_CORE_CODE_BASE, ALL_CORE_PROGRAM)
    for cpu in system.cores:
        cpu.pc = ALL_CORE_CODE_BASE
        cpu.regs[1] = 0
        cpu.halted = False
        cpu.idle = False

    for cpu in system.cores[system.num_full_cores:]:
        def reject_python_fallback(_cpu=cpu):
            raise AssertionError(
                f"microcore {_cpu.core_id} left native scalar execution"
            )

        cpu._step_python_fallback_in_memory_scope = reject_python_fallback

    owner = system._native_system
    batch_runs_before = int(owner.native_batch_runs)
    dispatches_before = int(owner.native_dispatches)
    slices = []
    winner_order = []
    for _ in range(system.num_cores + 1):
        stats = system.run_batch_stats(1)
        winner_order.append(
            _one_hot_winner(stats.per_core_instructions)
        )
        slices.append(_stats_trace(stats))

    return {
        "topology": {
            "num_full_cores": int(system.num_full_cores),
            "num_clusters": int(system.num_clusters),
            "microcores_per_cluster": MICRO_PER_CLUSTER,
            "num_all_cores": int(system.num_cores),
            "cluster_enable_mask": int(system.sysinfo.cluster_en),
            "clusters_enabled": [
                bool(cluster.enabled) for cluster in system.clusters
            ],
        },
        "workload": {
            "program_sha256": ALL_CORE_PROGRAM_SHA256,
            "slice_count": int(system.num_cores + 1),
            "instructions_per_slice": 1,
        },
        "execution": {
            "winner_order": winner_order,
            "slices": slices,
            "scheduler_cursor": int(system._scheduler_cursor),
            "r1_by_core": [
                int(cpu.regs[1]) for cpu in system.cores
            ],
            "pc_by_core": [
                int(cpu.pc) for cpu in system.cores
            ],
            "native_batch_runs_delta": (
                int(owner.native_batch_runs) - batch_runs_before
            ),
            "native_dispatches_delta": (
                int(owner.native_dispatches) - dispatches_before
            ),
            "authoritative_system_cycles": int(owner.system_cycles),
        },
    }


def _crc_round_state(system: MegapadSystem, stats, active) -> dict:
    return {
        "execution": _stats_trace(stats),
        "scheduler_cursor": int(system._scheduler_cursor),
        "active_pcs": {
            name: int(cpu.pc) for name, cpu in active.items()
        },
        "crc": [
            _crc_snapshot(system, index)
            for index in range(len(system.clusters))
        ],
        "arbiters": [
            _arbiter_snapshot(system, index)
            for index in range(len(system.clusters))
        ],
    }


def _cluster_crc_trace(*, worker_count: int = 1) -> dict:
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=2,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
    )
    for cpu in system.cores:
        cpu.halted = True
        cpu.idle = False

    active = {}
    for name, program in CRC_PROGRAMS.items():
        cluster_index = 0 if name.startswith("cluster0") else 1
        local_index = 0 if name.endswith("local0") else 1
        cpu = system.clusters[cluster_index].cores[local_index]
        system.load_binary(CRC_PROGRAM_BASES[name], program)
        cpu.pc = CRC_PROGRAM_BASES[name]
        cpu.halted = False
        active[name] = cpu

    system._scheduler_cursor = 0
    owner = system._native_system
    batch_runs_before = int(owner.native_batch_runs)
    dispatches_before = int(owner.native_dispatches)
    losers = [
        active["cluster0_local0"],
        active["cluster1_local0"],
    ]
    winners = [
        active["cluster0_local1"],
        active["cluster1_local1"],
    ]

    first = system.run_batch_stats(2)
    round_one = _crc_round_state(system, first, active)
    round_one["sibling_visible_modes"] = [
        int(losers[0].csr_read(CSR_CRC_MODE)),
        int(losers[1].csr_read(CSR_CRC_MODE)),
    ]

    second = system.run_batch_stats(2)
    round_two = _crc_round_state(system, second, active)

    # Requeue the previous winners at MODE after they release the lock. Both
    # local peers contend again, so the cursor rotation to local core 0 is
    # observed under equal eligibility rather than through a sole requester.
    for name, cpu in (
        ("cluster0_local1", winners[0]),
        ("cluster1_local1", winners[1]),
    ):
        cpu.pc = CRC_PROGRAM_BASES[name]
    third = system.run_batch_stats(2)
    round_three = _crc_round_state(system, third, active)

    return {
        "topology": {
            "num_full_cores": int(system.num_full_cores),
            "num_clusters": int(system.num_clusters),
            "microcores_per_cluster": MICRO_PER_CLUSTER,
            "num_all_cores": int(system.num_cores),
            "cluster_enable_mask": int(system.sysinfo.cluster_en),
            "active_global_core_ids": [
                int(active[name].core_id)
                for name in CRC_PROGRAMS
            ],
        },
        "workload": {
            "program_sha256": {
                name: hashlib.sha256(program).hexdigest()
                for name, program in CRC_PROGRAMS.items()
            },
            "program_bases": dict(CRC_PROGRAM_BASES),
            "retirement_budget_per_round": 2,
            "clusters_contending_at_unbounded_boundary": 2,
            "hard_eligibility_exercised": True,
            "equal_peer_recontention_exercised": True,
            "host_requeues_after_round_two": [
                {
                    "global_core_id": int(
                        active["cluster0_local1"].core_id
                    ),
                    "pc": CRC_PROGRAM_BASES["cluster0_local1"],
                },
                {
                    "global_core_id": int(
                        active["cluster1_local1"].core_id
                    ),
                    "pc": CRC_PROGRAM_BASES["cluster1_local1"],
                },
            ],
            "arbitration_contract": ARBITRATION_CONTRACT,
        },
        "rounds": [round_one, round_two, round_three],
        "final": {
            "native_batch_runs_delta": (
                int(owner.native_batch_runs) - batch_runs_before
            ),
            "native_dispatches_delta": (
                int(owner.native_dispatches) - dispatches_before
            ),
            "authoritative_system_cycles": int(owner.system_cycles),
        },
    }


def _observe() -> dict:
    all_core = _all_core_trace()
    cluster_crc = _cluster_crc_trace()
    canonical_state = {
        "schema": STATE_SCHEMA,
        "schema_version": STATE_SCHEMA_VERSION,
        "milestone": MILESTONE,
        "all_core_scheduling": all_core,
        "cluster_crc_eligibility": cluster_crc,
    }
    canonical_state_sha256 = _json_sha256(canonical_state)
    behavior_oracle_sha256 = _json_sha256(
        {
            "state_schema": STATE_SCHEMA,
            "state_schema_version": STATE_SCHEMA_VERSION,
            "canonical_state_sha256": canonical_state_sha256,
            "arbitration_contract": ARBITRATION_CONTRACT,
            "measured_claims": MEASURED_CLAIMS,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
        }
    )
    return {
        "state_schema": STATE_SCHEMA,
        "state_schema_version": STATE_SCHEMA_VERSION,
        "canonical_state_sha256": canonical_state_sha256,
        "behavior_oracle_sha256": behavior_oracle_sha256,
        "component_sha256": {
            "all_core_scheduling": _json_sha256(all_core),
            "cluster_crc_eligibility": _json_sha256(cluster_crc),
        },
        "canonical_state": canonical_state,
    }


def run_sample() -> dict:
    wall_start = time.perf_counter()
    process_start = time.process_time()
    observation = _observe()
    process_time_s = time.process_time() - process_start
    wall_time_s = time.perf_counter() - wall_start
    retired_instructions = 23
    return {
        "wall_time_s": wall_time_s,
        "process_cpu_time_s": process_time_s,
        "oracle_retired_instructions_per_s": (
            retired_instructions / wall_time_s
            if wall_time_s
            else None
        ),
        "observation": observation,
    }


def run_report(*, repeats: int = 3, warmups: int = 1) -> dict:
    if repeats <= 0:
        raise ValueError("repeats must be positive")
    if warmups < 0:
        raise ValueError("warmups cannot be negative")

    for _ in range(warmups):
        run_sample()
    samples = [run_sample() for _ in range(repeats)]
    state_hashes = [
        sample["observation"]["canonical_state_sha256"]
        for sample in samples
    ]
    oracle_hashes = [
        sample["observation"]["behavior_oracle_sha256"]
        for sample in samples
    ]
    rates = [
        sample["oracle_retired_instructions_per_s"]
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
            "repeats": repeats,
            "warmups": warmups,
            "retired_instructions_per_sample": 23,
        },
        "semantics": {
            "classification": "behavior_oracle_and_diagnostic_baseline",
            "milestone": MILESTONE,
            "arbitration_contract": ARBITRATION_CONTRACT,
            "measured_claims": MEASURED_CLAIMS,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
        },
        "determinism": {
            "canonical_state_matches": len(set(state_hashes)) == 1,
            "behavior_oracle_matches": len(set(oracle_hashes)) == 1,
            "canonical_state_sha256": state_hashes,
            "behavior_oracle_sha256": oracle_hashes,
        },
        "throughput": {
            "oracle_retired_instructions_per_s_median":
                statistics.median(rates),
            "oracle_retired_instructions_per_s_min": min(rates),
            "oracle_retired_instructions_per_s_max": max(rates),
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
            "Run the versioned Phase 2 all-core and cluster-resource oracle."
        )
    )
    parser.add_argument("--repeats", type=_positive_int, default=3)
    parser.add_argument("--warmups", type=_nonnegative_int, default=1)
    parser.add_argument(
        "--quick",
        action="store_true",
        help="one repeat and no warmup",
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
        args.repeats = 1
        args.warmups = 0
    report = run_report(repeats=args.repeats, warmups=args.warmups)
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        throughput = report["throughput"][
            "oracle_retired_instructions_per_s_median"
        ]
        print(
            "Phase 2 cluster oracle: "
            f"{throughput:,.0f} retired instructions/s"
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
