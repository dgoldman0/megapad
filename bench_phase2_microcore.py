#!/usr/bin/env python3
"""Versioned baseline for one active accelerated microcore.

This diagnostic benchmark began as the element-5 ownership baseline.  Version
3 preserves that workload while recording both the element-6 native all-core
system scheduler and the selected all-ones CLUSTER_EN reset contract.  It
deliberately does not exercise cluster contention, hard-QoS eligibility,
equal-round-robin ordering among peers, shared engines, or strict-cycle
execution.

Version 4 retains the Phase 2 architectural state schema and adds the Phase 4
measurement surface: one/two/four-lane reports, ordered public-accounting
hashes, clean source/artifact provenance, and an optional separate host-profile
replay. Timed samples remain unprofiled.

Version 5 makes the complete one/two/four-lane set mandatory and isolates
timed samples from cyclic-GC and persistent-pool teardown.

Version 6 adds host-only private decode/admission-cache counters and the
single-use micro-oracle proof-reuse count. Architectural state schema 3 and
the timed-workload semantics remain unchanged.

Version 7 carries native host-profile schema 4 and the exact-singleton
full-core counters, which remain zero for this microcore topology.

Version 8 carries native host-profile schema 5 and its exact-single-core
decoded-block counters. Timed-workload and architectural-state semantics
remain unchanged.

Version 9 carries native host-profile schema 6 and its exact-single-core JIT
telemetry. The JIT remains ineligible for this microcore topology; timed and
architectural-state semantics remain unchanged.
"""

from __future__ import annotations

import argparse
import gc
import hashlib
import json
import platform
import statistics
import subprocess
import time
from collections.abc import Iterable
from datetime import datetime, timezone
from pathlib import Path

import _mp64_accel

from asm import assemble
from megapad64 import MICRO_PER_CLUSTER
from system import MegapadSystem


REPORT_SCHEMA = "megapad.phase2-single-active-microcore-baseline"
REPORT_SCHEMA_VERSION = 9
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


def _git_output(*args: str) -> str | None:
    try:
        completed = subprocess.run(
            ["git", *args],
            cwd=Path(__file__).resolve().parent,
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    return completed.stdout.strip()


def _repository_metadata() -> dict:
    status = _git_output("status", "--porcelain")
    return {
        "root": str(Path(__file__).resolve().parent),
        "commit": _git_output("rev-parse", "HEAD"),
        "branch": _git_output("branch", "--show-current"),
        "dirty": None if status is None else bool(status),
    }


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1 << 20), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _elf_build_id(path: Path) -> str | None:
    try:
        completed = subprocess.run(
            ["readelf", "-n", str(path)],
            check=True,
            capture_output=True,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return None
    marker = "Build ID:"
    for line in completed.stdout.splitlines():
        if marker in line:
            value = line.split(marker, 1)[1].strip()
            return value or None
    return None


def _accelerator_metadata() -> dict:
    artifact = Path(_mp64_accel.__file__).resolve()
    return {
        "module": "_mp64_accel",
        "loaded_artifact_path": str(artifact),
        "loaded_artifact_size_bytes": artifact.stat().st_size,
        "loaded_artifact_sha256": _sha256_file(artifact),
        "elf_build_id": _elf_build_id(artifact),
    }


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


def _ordered_public_accounting(
    system: MegapadSystem,
    stats,
) -> dict:
    return {
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
        "per_core_interrupts": [
            int(value) for value in stats.per_core_interrupts
        ],
        "per_core_stop_reasons": [
            [int(value) for value in reasons]
            for reasons in stats.per_core_stop_reasons
        ],
        "native_rounds": int(stats.native_rounds),
        "native_continuations": int(stats.native_continuations),
        "system_stop_reason": str(stats.system_stop_reason),
        "stop_cycle": int(stats.stop_cycle),
        "event_source_mask": int(stats.event_source_mask),
        "interrupts_delivered": int(stats.interrupts_delivered),
        "external_events_applied": int(stats.external_events_applied),
        "pending_interrupt_core": int(stats.pending_interrupt_core),
        "pending_interrupt_vector": int(stats.pending_interrupt_vector),
        "scheduler_cursor": int(system._scheduler_cursor),
        "authoritative_system_cycles": int(
            system._native_system.system_cycles
        ),
    }


def _json_native(value):
    if isinstance(value, dict):
        return {
            str(key): _json_native(item)
            for key, item in value.items()
        }
    if isinstance(value, (list, tuple)):
        return [_json_native(item) for item in value]
    if isinstance(value, bool):
        return value
    if isinstance(value, int):
        return int(value)
    if isinstance(value, str) or value is None:
        return value
    try:
        return int(value)
    except (TypeError, ValueError):
        return str(value)


def _profile_probe(
    snapshot: dict,
    *,
    stats,
    worker_count: int,
) -> dict:
    normalized = _json_native(snapshot)
    counts = normalized["counts"]
    lane_commands = counts["lane_commands"]
    lane_steps = counts["lane_steps"]
    private_stop_reason_total = sum(
        counts["private_stop_reasons"].values()
    )
    bypass_stop_reason_total = sum(
        counts["worker_bypass_stop_reasons"].values()
    )
    coordinator_origin_total = sum(
        counts["coordinator_boundary_origins"].values()
    )
    validation = {
        "schema_is_version_6":
            normalized["schema_version"] == 6,
        "profile_is_frozen": not normalized["enabled"],
        "profile_generation_is_positive":
            normalized["generation"] > 0,
        "architectural_hash_scope_is_host_only":
            normalized["architectural_hash_scope"]
            == "excluded_host_only",
        "measurement_scope_is_unbounded_batch_only": (
            normalized["measurement_scope"]
            == "unbounded_native_system_batch_only"
        ),
        "timers_are_inclusive_nested_wall_time": (
            normalized["timing_semantics"]
            == "inclusive_nested_host_wall_nanoseconds"
        ),
        "one_native_batch":
            counts["batches"] == 1,
        "prepare_batch_calls_match_batches":
            counts["prepare_batch_calls"] == counts["batches"],
        "rounds_match_public_accounting":
            counts["scheduler_rounds"]
            == int(stats.native_rounds),
        "uncontended_path_is_ineligible_for_micro_topology": (
            all(
                counts[name] == 0
                for name in (
                    "uncontended_rounds",
                    "uncontended_dispatches",
                    "uncontended_steps",
                    "uncontended_continuations",
                    "uncontended_callback_errors",
                    "uncontended_interrupt_boundaries",
                    "uncontended_block_lookups",
                    "uncontended_block_hits",
                    "uncontended_block_misses",
                    "uncontended_block_builds",
                    "uncontended_block_executions",
                    "uncontended_block_steps",
                    "uncontended_jit_compile_attempts",
                    "uncontended_jit_compilations",
                    "uncontended_jit_compile_failures",
                    "uncontended_jit_executions",
                    "uncontended_jit_steps",
                )
            )
            and normalized["wall_ns"]["uncontended_round"] == 0
            and normalized["wall_ns"]["uncontended_dispatch"] == 0
        ),
        "absorptions_match_subfrontiers":
            counts["round_absorptions"]
            == counts["logical_subfrontiers"],
        "routing_commands_reconcile": (
            counts["frontier_routing_commands"]
            == counts["worker_commands"]
            + counts["worker_bypassed_commands"]
        ),
        "worker_waves_within_routing_waves": (
            0 <= counts["worker_waves"]
            <= counts["frontier_routing_waves"]
        ),
        "worker_commands_match_lanes":
            counts["worker_commands"] == sum(lane_commands),
        "private_steps_match_lanes":
            counts["private_steps"] == sum(lane_steps),
        "private_steps_match_public_execution":
            counts["private_steps"]
            == int(stats.instructions_executed),
        "private_decode_cache_counts_reconcile": (
            counts["private_decode_cache_lookups"]
            == counts["private_decode_cache_hits"]
            + counts["private_decode_cache_misses"]
        ),
        "all_private_classifications_use_decode_cache": (
            counts["private_decode_cache_lookups"]
            == counts["private_classification_calls"]
        ),
        "frontier_decode_cache_counts_reconcile": (
            counts["frontier_decode_cache_lookups"]
            == counts["frontier_decode_cache_hits"]
            + counts["frontier_decode_cache_misses"]
        ),
        "micro_workload_has_no_frontier_decode_cache": (
            counts["frontier_decode_cache_lookups"] == 0
        ),
        "micro_oracle_proof_reused_for_every_private_step": (
            counts["micro_oracle_proof_reuses"]
            == counts["private_steps"]
        ),
        "one_command_per_sole_participant_wave":
            counts["worker_waves"] == counts["worker_commands"],
        "all_progressing_commands_have_checkpoints": (
            counts["checkpoint_captures"]
            == counts["worker_commands"]
            - counts["zero_step_commands"]
        ),
        "checkpoint_restores_within_captures": (
            0 <= counts["checkpoint_restores"]
            <= counts["checkpoint_captures"]
        ),
        "no_checkpoint_restore":
            counts["checkpoint_restores"] == 0,
        "private_stop_reasons_match_worker_commands": (
            private_stop_reason_total == counts["worker_commands"]
        ),
        "bypass_stop_reasons_match_bypassed_commands": (
            bypass_stop_reason_total
            == counts["worker_bypassed_commands"]
        ),
        "coordinator_origins_match_boundaries": (
            coordinator_origin_total
            == counts["coordinator_boundaries"]
        ),
        "no_coordinator_boundary":
            counts["coordinator_boundaries"] == 0,
        "lane_arrays_match_worker_count": (
            len(lane_commands) == worker_count
            and len(lane_steps) == worker_count
            and len(normalized["lane_active_ns"]) == worker_count
        ),
        "sole_command_uses_only_lane_zero": (
            all(value == 0 for value in lane_commands[1:])
            and all(value == 0 for value in lane_steps[1:])
        ),
    }
    return {
        "schema": "megapad.phase4-concurrency-host-profile",
        "schema_version": 6,
        "architectural_hash_scope": "excluded_host_only",
        "used_for_throughput": False,
        "native_snapshot": normalized,
        "validation": validation,
    }


def _build_workload(
    *,
    worker_count: int = 1,
) -> tuple[MegapadSystem, object]:
    system = MegapadSystem(
        ram_size=RAM_SIZE,
        num_cores=1,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
        worker_count=worker_count,
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
    public_accounting = _ordered_public_accounting(
        system,
        stats,
    )
    return {
        "state_schema": STATE_SCHEMA,
        "state_schema_version": STATE_SCHEMA_VERSION,
        "canonical_state_sha256": canonical_state_sha256,
        "behavior_oracle_sha256": behavior_oracle_sha256,
        "ordered_public_accounting": public_accounting,
        "ordered_public_accounting_sha256":
            _json_sha256(public_accounting),
        "component_sha256": {
            "topology": _json_sha256(topology),
            "execution": _json_sha256(execution),
            "cores": _json_sha256(canonical_state["cores"]),
            "memory": _json_sha256(canonical_state["memory"]),
            "main_bus": _json_sha256(canonical_state["main_bus"]),
        },
        "canonical_state": canonical_state,
    }


def run_sample(
    instruction_budget: int,
    *,
    worker_count: int = 1,
    host_profile: bool = False,
    used_for_throughput: bool = True,
) -> dict:
    if instruction_budget <= 0:
        raise ValueError("instruction budget must be positive")
    if worker_count not in (1, 2, 4):
        raise ValueError("worker_count must be exactly 1, 2, or 4")
    if host_profile and used_for_throughput:
        raise ValueError(
            "a profiled replay cannot be used for throughput"
        )

    system = None
    micro = None
    owner = None
    stats = None
    profile_snapshot = None
    sample = None
    gc_enabled_before = gc.isenabled()
    try:
        system, micro = _build_workload(
            worker_count=worker_count,
        )
        owner = system._native_system
        collected_before_timing = gc.collect()
        native_batch_runs_before = int(owner.native_batch_runs)
        native_dispatches_before = int(owner.native_dispatches)

        if host_profile:
            owner._start_concurrency_profile()
        if gc_enabled_before:
            gc.disable()
        gc_enabled_during_timing = gc.isenabled()
        try:
            wall_start = time.perf_counter()
            process_start = time.process_time()
            stats = system.run_batch_stats(instruction_budget)
            process_time_s = time.process_time() - process_start
            wall_time_s = time.perf_counter() - wall_start
        except BaseException:
            if (
                host_profile and
                dict(owner._concurrency_profile_snapshot())["enabled"]
            ):
                owner._stop_concurrency_profile()
            raise
        finally:
            if gc.isenabled() != gc_enabled_before:
                if gc_enabled_before:
                    gc.enable()
                else:
                    gc.disable()
        gc_restored_to_prior_state = (
            gc.isenabled() == gc_enabled_before
        )
        profile_snapshot = (
            dict(owner._stop_concurrency_profile())
            if host_profile
            else None
        )

        observation = _observe(
            system,
            micro,
            stats=stats,
            instruction_budget=instruction_budget,
            native_batch_runs_before=native_batch_runs_before,
            native_dispatches_before=native_dispatches_before,
        )
        sample = {
            "worker_count": worker_count,
            "host_execution_lanes": worker_count,
            "auxiliary_worker_count": worker_count - 1,
            "used_for_throughput": used_for_throughput,
            "wall_time_s": wall_time_s,
            "process_cpu_time_s": process_time_s,
            "selected_microcore_instructions_per_s": (
                stats.instructions_executed / wall_time_s
                if wall_time_s
                else None
            ),
            "timing_hygiene": {
                "collected_objects_before_timing":
                    collected_before_timing,
                "gc_enabled_before_timing_setup":
                    gc_enabled_before,
                "gc_enabled_during_timing":
                    gc_enabled_during_timing,
                "gc_restored_to_prior_state":
                    gc_restored_to_prior_state,
                "collected_objects_after_sample": None,
            },
            "observation": observation,
        }
        sample["host_profile_probe"] = (
            None
            if profile_snapshot is None
            else _profile_probe(
                profile_snapshot,
                stats=stats,
                worker_count=worker_count,
            )
        )
    finally:
        if gc.isenabled() != gc_enabled_before:
            if gc_enabled_before:
                gc.enable()
            else:
                gc.disable()
        system = None
        micro = None
        owner = None
        stats = None
        profile_snapshot = None
        collected_after_sample = gc.collect()
        if sample is not None:
            sample["timing_hygiene"][
                "collected_objects_after_sample"
            ] = collected_after_sample
    return sample


def _worker_report(
    *,
    worker_count: int,
    instructions: int,
    repeats: int,
    warmups: int,
    warmup_instructions: int,
    host_profile: bool,
) -> dict:
    for _ in range(warmups):
        run_sample(
            warmup_instructions,
            worker_count=worker_count,
            used_for_throughput=False,
        )
    samples = [
        run_sample(
            instructions,
            worker_count=worker_count,
        )
        for _ in range(repeats)
    ]
    accounting = run_sample(
        instructions,
        worker_count=worker_count,
        host_profile=host_profile,
        used_for_throughput=False,
    )
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
    public_hashes = [
        sample["observation"][
            "ordered_public_accounting_sha256"
        ]
        for sample in samples
    ]
    accounting_observation = accounting["observation"]
    profile_probe = accounting["host_profile_probe"]
    validation = {
        "timed_samples_are_unprofiled": all(
            sample["host_profile_probe"] is None
            and sample["used_for_throughput"]
            for sample in samples
        ),
        "timed_samples_disable_gc_and_restore_state": all(
            not sample["timing_hygiene"]["gc_enabled_during_timing"]
            and sample["timing_hygiene"]["gc_restored_to_prior_state"]
            and sample["timing_hygiene"][
                "collected_objects_after_sample"
            ] is not None
            for sample in samples
        ),
        "accounting_probe_disables_gc_and_restores_state": (
            not accounting["timing_hygiene"][
                "gc_enabled_during_timing"
            ]
            and accounting["timing_hygiene"][
                "gc_restored_to_prior_state"
            ]
            and accounting["timing_hygiene"][
                "collected_objects_after_sample"
            ] is not None
        ),
        "timed_canonical_state_deterministic":
            len(set(state_hashes)) == 1,
        "timed_behavior_oracle_deterministic":
            len(set(oracle_hashes)) == 1,
        "timed_public_accounting_deterministic":
            len(set(public_hashes)) == 1,
        "accounting_replay_not_used_for_throughput":
            not accounting["used_for_throughput"],
        "accounting_canonical_state_matches_timed": (
            bool(state_hashes)
            and all(
                value ==
                accounting_observation[
                    "canonical_state_sha256"
                ]
                for value in state_hashes
            )
        ),
        "accounting_behavior_oracle_matches_timed": (
            bool(oracle_hashes)
            and all(
                value ==
                accounting_observation[
                    "behavior_oracle_sha256"
                ]
                for value in oracle_hashes
            )
        ),
        "accounting_public_accounting_matches_timed": (
            bool(public_hashes)
            and all(
                value ==
                accounting_observation[
                    "ordered_public_accounting_sha256"
                ]
                for value in public_hashes
            )
        ),
        "profile_presence_matches_request":
            (profile_probe is not None) == host_profile,
        "profile_reconciliations_pass": (
            profile_probe is None
            or all(profile_probe["validation"].values())
        ),
    }
    return {
        "worker_count": worker_count,
        "host_execution_lanes": worker_count,
        "auxiliary_worker_count": worker_count - 1,
        "timed_samples": samples,
        "accounting_probe": accounting,
        "throughput": {
            "selected_microcore_instructions_per_s_median":
                statistics.median(rates),
            "selected_microcore_instructions_per_s_min": min(rates),
            "selected_microcore_instructions_per_s_max": max(rates),
        },
        "validation": validation,
    }


def run_report(
    *,
    instructions: int = DEFAULT_INSTRUCTIONS,
    worker_counts: Iterable[int] = (1, 2, 4),
    repeats: int = 3,
    warmups: int = 1,
    warmup_instructions: int = QUICK_INSTRUCTIONS,
    host_profile: bool = False,
) -> dict:
    if instructions <= 0:
        raise ValueError("instructions must be positive")
    if repeats <= 0:
        raise ValueError("repeats must be positive")
    if warmups < 0:
        raise ValueError("warmups cannot be negative")
    if warmup_instructions <= 0:
        raise ValueError("warmup instructions must be positive")
    normalized_worker_counts = list(
        dict.fromkeys(int(value) for value in worker_counts)
    )
    if set(normalized_worker_counts) != {1, 2, 4}:
        raise ValueError(
            "worker_counts must contain exactly 1, 2, and 4"
        )
    normalized_worker_counts = [1, 2, 4]

    worker_reports = [
        _worker_report(
            worker_count=worker_count,
            instructions=instructions,
            repeats=repeats,
            warmups=warmups,
            warmup_instructions=warmup_instructions,
            host_profile=host_profile,
        )
        for worker_count in normalized_worker_counts
    ]
    accounting_observations = [
        report["accounting_probe"]["observation"]
        for report in worker_reports
    ]
    cross_worker_members = [
        {
            "worker_count": report["worker_count"],
            "canonical_state_sha256":
                observation["canonical_state_sha256"],
            "behavior_oracle_sha256":
                observation["behavior_oracle_sha256"],
            "ordered_public_accounting_sha256":
                observation[
                    "ordered_public_accounting_sha256"
                ],
            "selected_microcore_instructions_per_s_median":
                report["throughput"][
                    "selected_microcore_instructions_per_s_median"
                ],
        }
        for report, observation in zip(
            worker_reports,
            accounting_observations,
            strict=True,
        )
    ]
    baseline_rate = next(
        (
            member[
                "selected_microcore_instructions_per_s_median"
            ]
            for member in cross_worker_members
            if member["worker_count"] == 1
        ),
        None,
    )
    for member in cross_worker_members:
        member["one_lane_relative_throughput"] = (
            None
            if baseline_rate is None or baseline_rate == 0
            else member[
                "selected_microcore_instructions_per_s_median"
            ] / baseline_rate
        )
    cross_worker_validation = {
        "one_lane_reference_present":
            any(
                member["worker_count"] == 1
                for member in cross_worker_members
            ),
        "canonical_state_equal": len({
            member["canonical_state_sha256"]
            for member in cross_worker_members
        }) == 1,
        "behavior_oracle_equal": len({
            member["behavior_oracle_sha256"]
            for member in cross_worker_members
        }) == 1,
        "ordered_public_accounting_equal": len({
            member["ordered_public_accounting_sha256"]
            for member in cross_worker_members
        }) == 1,
    }
    cross_worker_validation["equivalent"] = all(
        cross_worker_validation.values()
    )
    validation = {
        "all_worker_reports_valid": all(
            all(report["validation"].values())
            for report in worker_reports
        ),
        "cross_worker_equivalent":
            cross_worker_validation["equivalent"],
    }
    return {
        "schema": REPORT_SCHEMA,
        "schema_version": REPORT_SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "repository": _repository_metadata(),
        "accelerator": _accelerator_metadata(),
        "host": {
            "python": platform.python_version(),
            "platform": platform.platform(),
            "machine": platform.machine(),
        },
        "configuration": {
            "instructions": instructions,
            "worker_counts": normalized_worker_counts,
            "repeats": repeats,
            "warmups": warmups,
            "warmup_instructions": warmup_instructions,
            "host_profile": host_profile,
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
        "cross_worker_equivalence": {
            "members": cross_worker_members,
            "validation": cross_worker_validation,
        },
        "worker_reports": worker_reports,
        "validation": validation,
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


def _worker_counts(text: str) -> list[int]:
    try:
        values = [
            int(value.strip())
            for value in text.split(",")
            if value.strip()
        ]
    except ValueError as error:
        raise argparse.ArgumentTypeError(
            "worker counts must contain exactly 1,2,4"
        ) from error
    values = list(dict.fromkeys(values))
    if set(values) != {1, 2, 4}:
        raise argparse.ArgumentTypeError(
            "worker counts must contain exactly 1,2,4"
        )
    return [1, 2, 4]


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
    parser.add_argument(
        "--worker-counts",
        type=_worker_counts,
        default=[1, 2, 4],
        help="host lane counts; must contain exactly 1,2,4",
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
        "--host-profile",
        action="store_true",
        help=(
            "run one separate host-profile accounting replay per lane "
            "width; timed samples remain unprofiled"
        ),
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
        worker_counts=args.worker_counts,
        repeats=args.repeats,
        warmups=args.warmups,
        warmup_instructions=args.warmup_instructions,
        host_profile=args.host_profile,
    )
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        print("Single active microcore:")
        for worker_report in report["worker_reports"]:
            throughput = worker_report["throughput"][
                "selected_microcore_instructions_per_s_median"
            ]
            print(
                f"  {worker_report['worker_count']} lane(s): "
                f"{throughput:,.0f} instructions/s"
            )
        cross = report["cross_worker_equivalence"]["validation"]
        print(
            "Cross-lane state/behavior/accounting: "
            f"{cross['canonical_state_equal']}/"
            f"{cross['behavior_oracle_equal']}/"
            f"{cross['ordered_public_accounting_equal']}"
        )
        if args.output is not None:
            print(f"JSON report: {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
