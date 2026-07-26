#!/usr/bin/env python3
"""Versioned Phase 2 instruction-cache and host-code-safety oracle.

Behavior hashes exclude host timing.  Hot-cache and disabled-cache
architectural results must match; their host-throughput ratio remains
diagnostic with no pass/fail threshold.
"""

from __future__ import annotations

import argparse
import gc
import hashlib
import importlib.metadata
import json
import platform
import statistics
import subprocess
import time
from datetime import datetime, timezone
from pathlib import Path

import _mp64_accel
from accel_wrapper import (
    Megapad64 as NativeMegapad64,
    Megapad64Micro as NativeMegapad64Micro,
)
from asm import assemble
from megapad64 import (
    CSR_ICACHE_CTRL,
    CSR_ICACHE_HITS,
    CSR_ICACHE_MISSES,
    Megapad64 as PythonMegapad64,
    Megapad64Micro as PythonMegapad64Micro,
)
from system import MegapadSystem


REPORT_SCHEMA = "megapad.phase2-instruction-cache-baseline"
# Version 3 refreshes the host-code-safety contract for the Phase 4 private
# decode/admission cache. The canonical guest machine-state schema remains 1.
REPORT_SCHEMA_VERSION = 3
STATE_SCHEMA = "megapad.phase2-instruction-cache-state"
STATE_SCHEMA_VERSION = 1
ROOT = Path(__file__).resolve().parent

MILESTONE = {
    "phase": 2,
    "element": 7,
    "elements_total": 7,
    "label": "Phase 2 \u2014 element 7 of 7",
}

ARBITRATION_CONTRACT = {
    "scope": "physical peers after each full core's local source mux",
    "hard_qos_role": (
        "determines must/may eligibility and reserved entitlement only"
    ),
    "simultaneously_eligible_peer_order": "equal_round_robin",
    "unused_reserved_capacity": "work_conserving",
    "best_effort_weights": "none",
    "secondary_ordering_biases": [],
}

CACHE_CONTRACT = {
    "profiles": {
        "full_core": "private_instruction_cache",
        "microcore": "no_instruction_cache",
    },
    "geometry": {
        "line_bytes": 16,
        "line_count": 256,
        "capacity_bytes": 4096,
        "mapping": "direct",
        "tags": "full_physical_line_address",
    },
    "fetch": {
        "lookup": "one_per_aligned_eight_byte_fetch_window",
        "cold_refill": "two_aligned_64_bit_bus_reads",
        "post_refill_synthetic_hit": False,
        "disabled": "one_aligned_64_bit_read_without_allocation_or_counting",
    },
    "control": {
        "0": "disable_preserve_lines_and_counters",
        "1": "enable_preserve_lines_and_counters",
        "2": "disable_invalidate_all_and_zero_counters",
        "3": "enable_invalidate_all_and_zero_counters",
    },
    "reset": "enabled_all_lines_invalid_hits_zero_misses_zero",
    "coherence": {
        "own_completed_store": "invalidate_matching_private_line",
        "other_core_cluster_dma_or_direct_host_write": "no_snoop",
        "visibility": "explicit_guest_or_host_invalidation_required",
    },
}

HOST_CODE_SAFETY_CONTRACT = {
    "general_decoded_or_jit_cache": (
        "private_admission_plans_only_no_translated_execution"
    ),
    "private_decode_admission_cache": {
        "ownership": "per_core_host_only_nonarchitectural",
        "full_core_identity": (
            "exact_complete_bytes_visible_through_guest_instruction_cache"
        ),
        "microcore_identity": (
            "exact_complete_bytes_visible_through_current_mapped_memory"
        ),
        "dynamic_policy": (
            "live_privilege_route_and_uncached_ext_skip_checks"
        ),
        "execution": "authoritative_native_fetch_and_step_remain_unchanged",
    },
    "accelerator_hook_registration": "snapshot_exact_guest_code_span",
    "accelerator_hook_mismatch": "decline_to_ordinary_call",
    "official_load_while_strict_instruction_suspended": "rejected",
    "official_load_outside_suspended_execution": (
        "allowed_noncoherent_host_write_requires_explicit_invalidation"
    ),
    "raw_backing_mutation": "explicit_noncoherent_unsafe_host_seam",
    "completed_strict_fetch": "journaled_bytes_are_replayed",
}

RTL_TRACE_CONTRACT = {
    "miss": [
        "aligned_low_64_bit_read",
        "aligned_high_64_bit_read",
        "publish_after_second_response",
    ],
    "disabled": ["one_aligned_64_bit_read", "no_allocation"],
    "invalidation": "tag_aware_line_or_canceling_invalidate_all",
    "full_core_local_port_mux": {
        "module": "mp64_core_bus_mux",
        "idle_simultaneous_sources": "instruction_cache_first",
        "ownership": "captured_with_payload_until_registered_response",
        "policy_scope": (
            "local instruction-versus-data selection, not physical-peer QoS"
        ),
    },
    "integrated_physical_main_bus": {
        "module": "mp64_bus",
        "qos_programming_path": "tied_off_in_mp64_soc",
        "hardware_weight_register_reset": "all_ones",
        "hardware_bandwidth_limit_register_reset": "all_zero_unlimited",
        "architectural_best_effort_weights": "absent",
        "simultaneously_eligible_physical_peers": "equal_round_robin",
        "arbitration_contract": ARBITRATION_CONTRACT,
    },
    "tile_write_coherence": {
        "core0_completed_write": (
            "invalidate_writer_local_private_instruction_cache"
        ),
        "cluster_completed_write": "noncoherent_to_full_core_private_caches",
        "completion": "ack_bounded_exactly_once_at_physical_target",
    },
    "generic_weighted_testbench_mode_is_architectural_policy": False,
}

REPORT_MEASURED_CLAIMS = [
    "native and Python full-core cache geometry and control semantics",
    "full physical tags across a same-index Bank-0/high-address conflict",
    "private noncoherence followed by explicit per-core invalidation",
    "strict cold-refill two-beat journaling and suspended-load rejection",
    "noncoherent official host loading outside suspended execution",
    "exact accelerator-hook code-span identity including its final byte",
    "hot-cache and disabled-cache architectural terminal-state equivalence",
]

EVIDENCE_MATRIX = {
    "runtime_report": {
        "cache_behavior": (
            "canonical_state.native_cache_trace and python_cache_trace"
        ),
        "strict_fetch_and_host_mutation": (
            "canonical_state.strict_cold_refill"
        ),
        "accelerator_hook_identity": (
            "canonical_state.accelerator_code_identity"
        ),
        "benchmark_semantic_equivalence": (
            "benchmark.cache_hot_matches_disabled"
        ),
    },
    "separate_python_native_gate": {
        "command": (
            "env MP64_RUNTIME_NAMESPACE=phase2-icache "
            "make test-sequential TEST_PATH='"
            "tests/test_phase2_instruction_cache.py "
            "tests/test_phase2_instruction_cache_oracle.py'"
        ),
        "scope": (
            "store invalidation, cache controls, tag conflicts, strict refill, "
            "and the versioned report"
        ),
    },
    "separate_rtl_gates": {
        "private_cache_trace": "make -C rtl/sim icache",
        "full_core_local_mux": "make -C rtl/sim core_bus_mux",
        "tile_ack_and_payload": "make -C rtl/sim tile",
        "tile_peer_order": "make -C rtl/sim tile_port_arbiter",
        "tile_writer_local_coherence": "make -C rtl/sim soc_tile_icache",
        "production_elaboration": "make -C rtl/sim soc_elaborate",
        "full_soc_smoke": "make -C rtl/sim soc_smoke",
    },
    "static_integration_sources": {
        "soc": "rtl/soc/mp64_soc.v",
        "local_mux": "rtl/soc/mp64_core_bus_mux.v",
        "main_bus": "rtl/bus/mp64_bus.v",
        "tile_arbiter": "rtl/soc/mp64_tile_port_arbiter.v",
        "yosys_manifest": "fpga/synth_yosys_soc.tcl",
        "vivado_manifest": "fpga/synth_genesys2.tcl",
    },
}

EXPLICIT_EXCLUSIONS = [
    {
        "state_or_behavior": (
            "Phase 4 private decode/admission cache throughput and hit counts"
        ),
        "reason": (
            "this retained Phase 2 report measures the architectural guest "
            "I-cache; Phase 4 host-profile benchmarks own admission-cache "
            "performance evidence"
        ),
    },
    {
        "state_or_behavior": (
            "generic weighted RTL modes and programmable best-effort weights"
        ),
        "reason": (
            "the integrated SoC ties QoS writes off, uses all-one weights, "
            "and has no emulator secondary bias"
        ),
    },
    {
        "state_or_behavior": "main-bus contention in this one-requester trace",
        "reason": (
            "physical main-bus ordering is measured by the Phase 0 contention "
            "oracle; element 6 instead measures cluster-local shared engines"
        ),
    },
    {
        "state_or_behavior": "RTL simulator execution inside this report",
        "reason": "RTL cache and production-elaboration benches are separate",
    },
    {
        "state_or_behavior": (
            "automatic cache snooping of other cores, DMA, or raw host writes"
        ),
        "reason": "the selected private instruction cache is noncoherent",
    },
    {
        "state_or_behavior": "minimum hot-cache speedup",
        "reason": "host timing is diagnostic and has no hard gate",
    },
    {
        "state_or_behavior": "cross-backend full-core BIST postconditions",
        "reason": (
            "the native accelerator currently reports an instant pass while "
            "Python and RTL BIST paths perform destructive memory checks"
        ),
    },
    {
        "state_or_behavior": "external unified-memory routing for LOAD2D/STORE2D",
        "reason": (
            "the current RTL two-dimensional paths remain internal-tile-port "
            "operations and are not claimed by this cache oracle"
        ),
    },
    {
        "state_or_behavior": "external tile addresses above 32 bits",
        "reason": (
            "the current RTL external-memory PHY path truncates the high "
            "address half; no high-address parity claim is made"
        ),
    },
    {
        "state_or_behavior": "full FPGA synthesis, timing closure, and place-route",
        "reason": (
            "source manifests and hierarchy/elaboration are gated separately; "
            "implementation-tool closure is outside this report"
        ),
    },
]

DEFAULT_BENCHMARK_INSTRUCTIONS = 200_000
QUICK_BENCHMARK_INSTRUCTIONS = 20_000
MEM_SIZE = 4096
TARGET = 0x100
RESET_TARGET = 0x220
TAG_HIGH_TARGET = 1 << 20
HOOK_TARGET = 0x600
HOOK_DATA_SP = 0x900
HOOK_RETURN_SP = 0xA00
HOOK_DESTINATION = 0x700

HOT_LOOP = assemble("loop:\ninc r1\nbr loop")
BACKEND_INC_R5 = assemble("inc r5")
BACKEND_INC_R6 = assemble("inc r6")
BACKEND_INC_R7 = assemble("inc r7")
BACKEND_INC_R8 = assemble("inc r8")
BACKEND_RESET = assemble("reset")
TAG_LOW_INC_R9 = assemble("inc r9")
TAG_HIGH_INC_R10 = assemble("inc r10")
STRICT_INITIAL = assemble("nop\nhalt")
STRICT_MUTATION = assemble("inc r1")
HOOK_CALLER = assemble("call.l r4\nhalt")
HOOK_BODY = assemble("nop\nnop\nnop")
HOOK_LAST_BYTE_MUTATION = assemble("inc r9")

HOT_LOOP_SHA256 = hashlib.sha256(HOT_LOOP).hexdigest()
STRICT_LOAD_REJECTION_REASON = "strict_cycle_execution_suspended"


def _fixture_entry(address: int, payload: bytes, role: str) -> dict:
    return {
        "address": address,
        "size_bytes": len(payload),
        "sha256": hashlib.sha256(payload).hexdigest(),
        "role": role,
    }


FIXTURE_MANIFEST = {
    "benchmark.hot_loop": _fixture_entry(
        0, HOT_LOOP, "timed hot-cache and disabled-cache loop"
    ),
    "backend.initial_target": _fixture_entry(
        TARGET, BACKEND_INC_R5, "first same-line cache fill"
    ),
    "backend.same_line": _fixture_entry(
        TARGET + 8, BACKEND_INC_R6, "same-line cache hit"
    ),
    "backend.first_mutation": _fixture_entry(
        TARGET, BACKEND_INC_R7, "noncoherent shared-backing mutation"
    ),
    "backend.second_mutation": _fixture_entry(
        TARGET, BACKEND_INC_R8, "guest-reset refetch mutation"
    ),
    "backend.reset": _fixture_entry(
        RESET_TARGET, BACKEND_RESET, "guest RESET cache reset"
    ),
    "micro.initial": _fixture_entry(
        TARGET, BACKEND_INC_R5, "uncached microcore first instruction"
    ),
    "micro.mutation": _fixture_entry(
        TARGET, BACKEND_INC_R6, "uncached microcore replacement"
    ),
    "tag.low": _fixture_entry(
        0, TAG_LOW_INC_R9, "same-index low physical tag"
    ),
    "tag.high": _fixture_entry(
        TAG_HIGH_TARGET, TAG_HIGH_INC_R10, "same-index high physical tag"
    ),
    "strict.initial": _fixture_entry(
        0, STRICT_INITIAL, "journaled strict cold refill"
    ),
    "strict.mutation": _fixture_entry(
        0, STRICT_MUTATION, "suspended and post-suspension host mutation"
    ),
    "hook.caller": _fixture_entry(
        0, HOOK_CALLER, "CALL.L dispatch into registered accelerator hook"
    ),
    "hook.body": _fixture_entry(
        HOOK_TARGET, HOOK_BODY, "three-byte registered code identity"
    ),
    "hook.last_byte_mutation": _fixture_entry(
        HOOK_TARGET + len(HOOK_BODY) - 1,
        HOOK_LAST_BYTE_MUTATION,
        "mutation of the registered span's final byte",
    ),
}


def _json_bytes(value: object) -> bytes:
    return json.dumps(
        value,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=True,
    ).encode("ascii")


def _sha(value: object) -> str:
    return hashlib.sha256(_json_bytes(value)).hexdigest()


def _git_output(*args: str) -> str | None:
    try:
        completed = subprocess.run(
            ["git", *args],
            cwd=ROOT,
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
        "root": str(ROOT),
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
    distribution_versions = {}
    for distribution in ("mp64_accel", "mp64-accel"):
        try:
            distribution_versions[distribution] = (
                importlib.metadata.version(distribution)
            )
        except importlib.metadata.PackageNotFoundError:
            continue
    return {
        "module": "_mp64_accel",
        "loaded_artifact_path": str(artifact),
        "loaded_artifact_size_bytes": artifact.stat().st_size,
        "loaded_artifact_sha256": _sha256_file(artifact),
        "elf_build_id": _elf_build_id(artifact),
        "module_version": getattr(_mp64_accel, "__version__", None),
        "distribution_versions": distribution_versions,
    }


def _cache_state(cpu) -> dict:
    return {
        "enabled": int(cpu.csr_read(CSR_ICACHE_CTRL)),
        "hits": int(cpu.csr_read(CSR_ICACHE_HITS)),
        "misses": int(cpu.csr_read(CSR_ICACHE_MISSES)),
    }


def _execute(cpu, address: int) -> int:
    cpu.halted = False
    cpu.idle = False
    cpu.pc = address
    return int(cpu.step())


def _geometry(cpu) -> dict:
    if hasattr(cpu, "_cs"):
        valid, tags, data = cpu._cs.icache_snapshot()
    else:
        valid, tags, data = (
            cpu._icache_valid,
            cpu._icache_tags,
            cpu._icache_data,
        )
    return {
        "valid_entries": len(valid),
        "tag_entries": len(tags),
        "data_bytes": len(data),
    }


def _backend_trace(full_type, micro_type) -> dict:
    backing = bytearray(MEM_SIZE)
    backing[TARGET:TARGET + len(BACKEND_INC_R5)] = BACKEND_INC_R5
    backing[
        TARGET + 8:TARGET + 8 + len(BACKEND_INC_R6)
    ] = BACKEND_INC_R6
    backing[
        RESET_TARGET:RESET_TARGET + len(BACKEND_RESET)
    ] = BACKEND_RESET
    first = full_type(mem_size=MEM_SIZE, core_id=0, num_cores=2)
    second = full_type(mem_size=MEM_SIZE, core_id=1, num_cores=2)
    first.mem = backing
    second.mem = backing

    _execute(first, TARGET)
    _execute(first, TARGET + 8)
    _execute(second, TARGET)
    cold = {
        "first_r5_r6": [int(first.regs[5]), int(first.regs[6])],
        "first_cache": _cache_state(first),
        "second_cache": _cache_state(second),
    }

    backing[TARGET:TARGET + len(BACKEND_INC_R7)] = BACKEND_INC_R7
    _execute(first, TARGET)
    _execute(second, TARGET)
    stale = {
        "first_r5_r7": [int(first.regs[5]), int(first.regs[7])],
        "second_r5_r7": [int(second.regs[5]), int(second.regs[7])],
    }

    first.csr_write(CSR_ICACHE_CTRL, 0)
    _execute(first, TARGET)
    control_0 = {
        "r7": int(first.regs[7]),
        "cache": _cache_state(first),
    }
    first.csr_write(CSR_ICACHE_CTRL, 1)
    _execute(first, TARGET)
    control_1 = {
        "r5": int(first.regs[5]),
        "cache": _cache_state(first),
    }
    first.csr_write(CSR_ICACHE_CTRL, 3)
    _execute(first, TARGET)
    _execute(second, TARGET)
    first_only_invalidate = {
        "first_r5_r7": [int(first.regs[5]), int(first.regs[7])],
        "second_r5_r7": [int(second.regs[5]), int(second.regs[7])],
        "first_cache": _cache_state(first),
    }

    second.csr_write(CSR_ICACHE_CTRL, 2)
    control_2 = _cache_state(second)
    _execute(second, TARGET)
    bypass_after_2 = {
        "r7": int(second.regs[7]),
        "cache": _cache_state(second),
    }
    second.csr_write(CSR_ICACHE_CTRL, 3)
    control_3 = _cache_state(second)
    _execute(second, TARGET)
    refill_after_3 = {
        "r7": int(second.regs[7]),
        "cache": _cache_state(second),
    }

    backing[TARGET:TARGET + len(BACKEND_INC_R8)] = BACKEND_INC_R8
    _execute(first, RESET_TARGET)
    reset_state = _cache_state(first)
    _execute(first, TARGET)
    reset_refetch = {
        "r5_r7_r8": [
            int(first.regs[5]),
            int(first.regs[7]),
            int(first.regs[8]),
        ],
        "cache": _cache_state(first),
    }

    micro_backing = bytearray(MEM_SIZE)
    micro_backing[
        TARGET:TARGET + len(BACKEND_INC_R5)
    ] = BACKEND_INC_R5
    micro = micro_type(mem_size=MEM_SIZE, core_id=1, num_cores=2)
    micro.mem = micro_backing
    _execute(micro, TARGET)
    micro_backing[
        TARGET:TARGET + len(BACKEND_INC_R6)
    ] = BACKEND_INC_R6
    _execute(micro, TARGET)

    tag_backing = bytearray(TAG_HIGH_TARGET + 16)
    tag_backing[0:len(TAG_LOW_INC_R9)] = TAG_LOW_INC_R9
    tag_backing[
        TAG_HIGH_TARGET:TAG_HIGH_TARGET + len(TAG_HIGH_INC_R10)
    ] = TAG_HIGH_INC_R10
    tag_cpu = full_type(mem_size=len(tag_backing))
    tag_cpu.mem = tag_backing
    _execute(tag_cpu, 0)
    _execute(tag_cpu, TAG_HIGH_TARGET)
    _execute(tag_cpu, 0)

    return {
        "geometry": _geometry(first),
        "cold_and_same_line": cold,
        "shared_backing_mutation_stays_stale": stale,
        "control_0_bypass": control_0,
        "control_1_preserved_line": control_1,
        "first_core_only_control_3": first_only_invalidate,
        "second_control_2": control_2,
        "second_disabled_bypass": bypass_after_2,
        "second_control_3": control_3,
        "second_refill_after_3": refill_after_3,
        "guest_reset": reset_state,
        "guest_reset_refetch": reset_refetch,
        "microcore": {
            "r5_r6": [int(micro.regs[5]), int(micro.regs[6])],
            "cache": _cache_state(micro),
        },
        "full_physical_tag": {
            "addresses": [0, TAG_HIGH_TARGET],
            "same_direct_map_index": True,
            "r9_r10": [int(tag_cpu.regs[9]), int(tag_cpu.regs[10])],
            "cache": _cache_state(tag_cpu),
        },
    }


def _system_stats(stats) -> dict:
    return {
        "instructions": int(stats.instructions_executed),
        "cycles": int(stats.system_cycles_advanced),
        "stop_reason": str(stats.system_stop_reason),
    }


def _strict_refill_trace() -> dict:
    system = MegapadSystem(
        ram_size=MEM_SIZE,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    system.load_binary(0, STRICT_INITIAL)
    system.boot(entry=0)
    owner = system._native_system
    before = owner._main_bus_snapshot()
    first = system.run_cycle_batch(1, max_instructions=1)
    pending = owner._cycle_pending_bus_requests()
    if len(pending) != 1:
        raise RuntimeError("expected one pending high refill beat")
    request = pending[0]
    cache_after_first_slice = _cache_state(system.cpu)

    try:
        system.load_binary(0, STRICT_MUTATION)
    except RuntimeError as error:
        if "cycle execution is suspended" not in str(error):
            raise
        load_rejection = {
            "reason_code": STRICT_LOAD_REJECTION_REASON,
            "exception_type": type(error).__name__,
        }
    else:
        raise RuntimeError("official load changed a suspended instruction")
    system.cpu.mem[0] = STRICT_MUTATION[0]

    resume = []
    for _ in range(4):
        result = system.run_cycle_batch(1, max_instructions=1)
        resume.append(_system_stats(result))
        if result.instructions_executed:
            break
    if sum(item["instructions"] for item in resume) != 1:
        raise RuntimeError("cold-refill instruction did not retire")
    after = owner._main_bus_snapshot()
    retired = {
        "pc": int(system.cpu.pc),
        "r1": int(system.cpu.regs[1]),
        "cache": _cache_state(system.cpu),
    }

    # Once strict execution is no longer suspended, the official host-loading
    # seam accepts the write but deliberately does not snoop private caches.
    # The stale journaled line remains visible until explicit invalidation.
    system.load_binary(0, STRICT_MUTATION)
    _execute(system.cpu, 0)
    outside_load_stale = {
        "pc": int(system.cpu.pc),
        "r1": int(system.cpu.regs[1]),
        "cache": _cache_state(system.cpu),
    }
    system.cpu.csr_write(CSR_ICACHE_CTRL, 3)
    _execute(system.cpu, 0)
    outside_load_after_invalidate = {
        "pc": int(system.cpu.pc),
        "r1": int(system.cpu.regs[1]),
        "cache": _cache_state(system.cpu),
    }

    return {
        "first_slice": _system_stats(first),
        "pending": {
            "requester_id": int(request.requester_id),
            "ready_cycle": int(request.ready_cycle),
            "operation": (
                "read"
                if request.operation == _mp64_accel.BusOperation.READ
                else "write"
            ),
            "address": int(request.address),
            "width": (
                "doubleword"
                if request.width == _mp64_accel.BusWidth.DOUBLEWORD
                else "other"
            ),
            "main_port_id": int(request.ordering.main_port_id),
            "issue_sequence": int(request.ordering.issue_sequence),
        },
        "cache_after_first_slice": cache_after_first_slice,
        "official_load_rejection": load_rejection,
        "raw_mutation_after_low_beat": "nop_to_inc_r1",
        "resume_slices": resume,
        "retired": retired,
        "official_load_outside_suspension": {
            "accepted": True,
            "without_invalidation": outside_load_stale,
            "after_explicit_invalidation": outside_load_after_invalidate,
        },
        "bus_deltas": {
            "issues": (
                int(after.last_issue_sequences[0])
                - int(before.last_issue_sequences[0])
            ),
            "grants": (
                int(after.next_grant_sequence)
                - int(before.next_grant_sequence)
            ),
        },
    }


def _hook_cpu() -> tuple[NativeMegapad64, dict]:
    cpu = NativeMegapad64(mem_size=0x1000)
    cpu.load_bytes(0, HOOK_CALLER)
    cpu.load_bytes(HOOK_TARGET, HOOK_BODY)
    cpu.pc = 0
    cpu.regs[4] = HOOK_TARGET
    cpu.regs[14] = HOOK_DATA_SP
    cpu.regs[15] = HOOK_RETURN_SP
    for index, value in enumerate(
        (0xBEEF, 1, 2, 4, HOOK_DESTINATION)
    ):
        start = HOOK_DATA_SP + index * 8
        cpu.mem[start:start + 8] = value.to_bytes(8, "little")
    cpu.register_accel_hook(HOOK_TARGET, 1, len(HOOK_BODY))
    return cpu, {
        "target": HOOK_TARGET,
        "data_sp": HOOK_DATA_SP,
        "return_sp": HOOK_RETURN_SP,
        "destination": HOOK_DESTINATION,
        "code_size": len(HOOK_BODY),
        "code_sha256": hashlib.sha256(HOOK_BODY).hexdigest(),
    }


def _hook_identity_trace() -> dict:
    accepted, layout = _hook_cpu()
    accepted_cycles = int(accepted.step())
    accepted_state = {
        "cycles": accepted_cycles,
        "pc": int(accepted.pc),
        "data_sp": int(accepted.regs[14]),
        "return_sp": int(accepted.regs[15]),
        "destination": bytes(
            accepted.mem[
                layout["destination"]:layout["destination"] + 4
            ]
        ).hex(),
    }

    declined, _ = _hook_cpu()
    mutation_offset = layout["code_size"] - 1
    mutation_address = layout["target"] + mutation_offset
    declined.mem[mutation_address] = HOOK_LAST_BYTE_MUTATION[0]
    mutated_span = bytes(
        declined.mem[
            layout["target"]:layout["target"] + layout["code_size"]
        ]
    )
    declined_cycles = int(declined.step())
    declined_state = {
        "mutation_offset": mutation_offset,
        "mutation_address": mutation_address,
        "mutation_instruction_sha256": hashlib.sha256(
            HOOK_LAST_BYTE_MUTATION
        ).hexdigest(),
        "mutated_span_sha256": hashlib.sha256(mutated_span).hexdigest(),
        "cycles": declined_cycles,
        "pc": int(declined.pc),
        "data_sp": int(declined.regs[14]),
        "return_sp": int(declined.regs[15]),
        "return_address": int.from_bytes(
            declined.mem[
                layout["return_sp"] - 8:layout["return_sp"]
            ],
            "little",
        ),
        "destination": bytes(
            declined.mem[
                layout["destination"]:layout["destination"] + 4
            ]
        ).hex(),
    }
    return {
        "registration": layout,
        "matching_identity": accepted_state,
        "changed_identity": declined_state,
    }


def _observe() -> dict:
    native = _backend_trace(NativeMegapad64, NativeMegapad64Micro)
    python = _backend_trace(PythonMegapad64, PythonMegapad64Micro)
    strict = _strict_refill_trace()
    hooks = _hook_identity_trace()
    state = {
        "schema": STATE_SCHEMA,
        "schema_version": STATE_SCHEMA_VERSION,
        "milestone": MILESTONE,
        "fixture_manifest": FIXTURE_MANIFEST,
        "native_cache_trace": native,
        "python_cache_trace": python,
        "backend_trace_matches": native == python,
        "strict_cold_refill": strict,
        "accelerator_code_identity": hooks,
    }
    state_hash = _sha(state)
    oracle_hash = _sha(
        {
            "state_sha256": state_hash,
            "cache_contract": CACHE_CONTRACT,
            "host_code_safety_contract": HOST_CODE_SAFETY_CONTRACT,
            "rtl_trace_contract": RTL_TRACE_CONTRACT,
            "arbitration_contract": ARBITRATION_CONTRACT,
            "report_measured_claims": REPORT_MEASURED_CLAIMS,
            "evidence_matrix": EVIDENCE_MATRIX,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
        }
    )
    return {
        "state_schema": STATE_SCHEMA,
        "state_schema_version": STATE_SCHEMA_VERSION,
        "canonical_state_sha256": state_hash,
        "behavior_oracle_sha256": oracle_hash,
        "component_sha256": {
            "fixture_manifest": _sha(FIXTURE_MANIFEST),
            "native_cache": _sha(native),
            "python_cache": _sha(python),
            "strict_cold_refill": _sha(strict),
            "accelerator_code_identity": _sha(hooks),
        },
        "canonical_state": state,
    }


def _warm_cache_snapshot() -> tuple[bytes, list[int], bytes]:
    warm = NativeMegapad64(mem_size=MEM_SIZE)
    warm.load_bytes(0, HOT_LOOP)
    warm.csr_write(CSR_ICACHE_CTRL, 3)
    _execute(warm, 0)
    valid, tags, data = warm._cs.icache_snapshot()
    return bytes(valid), list(tags), bytes(data)


def _benchmark_mode(
    instructions: int,
    *,
    enabled: bool,
    hot_cache_snapshot: tuple[bytes, list[int], bytes],
) -> dict:
    # Every timed mode starts from a fresh architectural CPU.  Hot mode
    # restores only the separately warmed cache arrays; disabled mode starts
    # from the same architecture and explicitly disables/invalidates caching.
    cpu = NativeMegapad64(mem_size=MEM_SIZE)
    cpu.load_bytes(0, HOT_LOOP)
    if enabled:
        cpu.csr_write(CSR_ICACHE_CTRL, 1)
        cpu._cs.icache_restore(*hot_cache_snapshot)
    else:
        cpu.csr_write(CSR_ICACHE_CTRL, 2)
    hits_before = int(cpu.csr_read(CSR_ICACHE_HITS))
    misses_before = int(cpu.csr_read(CSR_ICACHE_MISSES))

    gc_enabled_before = gc.isenabled()
    if gc_enabled_before:
        gc.disable()
    gc_enabled_during_timing = gc.isenabled()
    try:
        wall_start = time.perf_counter()
        process_start = time.process_time()
        stats = cpu.run_steps_stats(instructions)
        process_time = time.process_time() - process_start
        wall_time = time.perf_counter() - wall_start
    finally:
        if gc_enabled_before:
            gc.enable()
    gc_restored = gc.isenabled() == gc_enabled_before

    architectural_state = {
        "steps": int(stats.steps_executed),
        "cycles": int(stats.total_cycles),
        "stop_reason": int(stats.stop_reason),
        "pc": int(cpu.pc),
        "r1": int(cpu.regs[1]),
        "core_cycle_count": int(cpu.cycle_count),
        "halted": bool(cpu.halted),
        "idle": bool(cpu.idle),
    }
    cache_diagnostics = {
        "enabled": int(cpu.csr_read(CSR_ICACHE_CTRL)),
        "hit_delta": int(cpu.csr_read(CSR_ICACHE_HITS)) - hits_before,
        "miss_delta": (
            int(cpu.csr_read(CSR_ICACHE_MISSES)) - misses_before
        ),
    }
    state = {
        "architectural": architectural_state,
        "cache_diagnostics": cache_diagnostics,
    }
    return {
        "wall_time_s": wall_time,
        "process_cpu_time_s": process_time,
        "instructions_per_s": stats.steps_executed / wall_time,
        "state": state,
        "state_sha256": _sha(state),
        "architectural_state_sha256": _sha(architectural_state),
        "cyclic_gc": {
            "enabled_before_timing": gc_enabled_before,
            "enabled_during_timing": gc_enabled_during_timing,
            "restored_to_prior_state": gc_restored,
        },
    }


def run_sample(
    *,
    benchmark_instructions: int,
    mode_order: tuple[str, str] = ("cache_hot", "cache_disabled"),
) -> dict:
    if set(mode_order) != {"cache_hot", "cache_disabled"}:
        raise ValueError("mode_order must contain hot and disabled exactly once")
    hot_cache_snapshot = _warm_cache_snapshot()
    modes = {}
    for mode in mode_order:
        modes[mode] = _benchmark_mode(
            benchmark_instructions,
            enabled=mode == "cache_hot",
            hot_cache_snapshot=hot_cache_snapshot,
        )
    hot = modes["cache_hot"]
    disabled = modes["cache_disabled"]
    hot_matches_disabled = (
        hot["state"]["architectural"]
        == disabled["state"]["architectural"]
    )
    return {
        "observation": _observe(),
        "benchmark": {
            "program_sha256": HOT_LOOP_SHA256,
            "execution_order": list(mode_order),
            "cache_hot": hot,
            "cache_disabled": disabled,
            "cache_hot_matches_disabled": hot_matches_disabled,
            "hot_to_disabled_ratio": (
                hot["instructions_per_s"] / disabled["instructions_per_s"]
            ),
        },
    }


def run_report(
    *,
    repeats: int = 3,
    warmups: int = 1,
    benchmark_instructions: int = DEFAULT_BENCHMARK_INSTRUCTIONS,
) -> dict:
    if repeats <= 0:
        raise ValueError("repeats must be positive")
    if warmups < 0:
        raise ValueError("warmups cannot be negative")
    if benchmark_instructions <= 0:
        raise ValueError("benchmark_instructions must be positive")
    mode_orders = (
        ("cache_hot", "cache_disabled"),
        ("cache_disabled", "cache_hot"),
    )
    for _ in range(warmups):
        run_sample(
            benchmark_instructions=benchmark_instructions,
            mode_order=mode_orders[_ % len(mode_orders)],
        )
    samples = [
        run_sample(
            benchmark_instructions=benchmark_instructions,
            mode_order=mode_orders[index % len(mode_orders)],
        )
        for index in range(repeats)
    ]
    states = [
        sample["observation"]["canonical_state_sha256"]
        for sample in samples
    ]
    oracles = [
        sample["observation"]["behavior_oracle_sha256"]
        for sample in samples
    ]
    benchmark_states = [
        (
            sample["benchmark"]["cache_hot"][
                "architectural_state_sha256"
            ],
            sample["benchmark"]["cache_disabled"][
                "architectural_state_sha256"
            ],
        )
        for sample in samples
    ]
    hot_matches_disabled = [
        sample["benchmark"]["cache_hot_matches_disabled"]
        for sample in samples
    ]
    hot_rates = [
        sample["benchmark"]["cache_hot"]["instructions_per_s"]
        for sample in samples
    ]
    disabled_rates = [
        sample["benchmark"]["cache_disabled"]["instructions_per_s"]
        for sample in samples
    ]
    ratios = [
        sample["benchmark"]["hot_to_disabled_ratio"]
        for sample in samples
    ]
    validation = {
        "all_backend_traces_match": all(
            sample["observation"]["canonical_state"][
                "backend_trace_matches"
            ]
            for sample in samples
        ),
        "all_suspended_loads_rejected_with_normalized_reason": all(
            sample["observation"]["canonical_state"][
                "strict_cold_refill"
            ]["official_load_rejection"]["reason_code"]
            == STRICT_LOAD_REJECTION_REASON
            for sample in samples
        ),
        "all_hook_final_byte_mutations_decline": all(
            sample["observation"]["canonical_state"][
                "accelerator_code_identity"
            ]["changed_identity"]["mutation_offset"]
            == len(HOOK_BODY) - 1
            for sample in samples
        ),
        "all_cache_hot_modes_match_disabled_architecture": all(
            hot_matches_disabled
        ),
        "all_timed_modes_disable_cyclic_gc": all(
            not sample["benchmark"][mode]["cyclic_gc"][
                "enabled_during_timing"
            ]
            for sample in samples
            for mode in ("cache_hot", "cache_disabled")
        ),
        "all_timed_modes_restore_prior_gc_state": all(
            sample["benchmark"][mode]["cyclic_gc"][
                "restored_to_prior_state"
            ]
            for sample in samples
            for mode in ("cache_hot", "cache_disabled")
        ),
    }
    return {
        "schema": REPORT_SCHEMA,
        "schema_version": REPORT_SCHEMA_VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "repository": _repository_metadata(),
        "host": {
            "python": platform.python_version(),
            "platform": platform.platform(),
            "machine": platform.machine(),
            "accelerator": _accelerator_metadata(),
        },
        "configuration": {
            "repeats": repeats,
            "warmups": warmups,
            "benchmark_instructions_per_mode": benchmark_instructions,
            "timed_mode_order": "alternating_by_sample",
        },
        "semantics": {
            "classification": "behavior_oracle_and_diagnostic_baseline",
            "milestone": MILESTONE,
            "cache_contract": CACHE_CONTRACT,
            "host_code_safety_contract": HOST_CODE_SAFETY_CONTRACT,
            "rtl_trace_contract": RTL_TRACE_CONTRACT,
            "arbitration_contract": ARBITRATION_CONTRACT,
            "report_measured_claims": REPORT_MEASURED_CLAIMS,
            "evidence_matrix": EVIDENCE_MATRIX,
            "explicit_exclusions": EXPLICIT_EXCLUSIONS,
            "performance_gate": "none",
        },
        "determinism": {
            "canonical_state_repeats_match": len(set(states)) == 1,
            "behavior_oracle_repeats_match": len(set(oracles)) == 1,
            "benchmark_architectural_state_repeats_match": (
                len(set(benchmark_states)) == 1
            ),
            "cache_hot_matches_disabled": all(hot_matches_disabled),
        },
        "hashes": {
            "canonical_state_sha256": states,
            "behavior_oracle_sha256": oracles,
            "benchmark_architectural_state_sha256": benchmark_states,
        },
        "validation": validation,
        "throughput": {
            "cache_hot_instructions_per_s_median": statistics.median(
                hot_rates
            ),
            "cache_disabled_instructions_per_s_median": statistics.median(
                disabled_rates
            ),
            "hot_to_disabled_ratio_median": statistics.median(ratios),
        },
        "samples": samples,
    }


def _positive(text: str) -> int:
    value = int(text)
    if value <= 0:
        raise argparse.ArgumentTypeError("value must be positive")
    return value


def _nonnegative(text: str) -> int:
    value = int(text)
    if value < 0:
        raise argparse.ArgumentTypeError("value cannot be negative")
    return value


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repeats", type=_positive, default=3)
    parser.add_argument("--warmups", type=_nonnegative, default=1)
    parser.add_argument(
        "--instructions",
        type=_positive,
        default=DEFAULT_BENCHMARK_INSTRUCTIONS,
    )
    parser.add_argument("--quick", action="store_true")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--output", type=Path)
    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    if args.quick:
        args.repeats = 1
        args.warmups = 0
        args.instructions = QUICK_BENCHMARK_INSTRUCTIONS
    report = run_report(
        repeats=args.repeats,
        warmups=args.warmups,
        benchmark_instructions=args.instructions,
    )
    encoded = json.dumps(report, indent=2, sort_keys=True)
    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(encoded + "\n", encoding="utf-8")
    if args.json:
        print(encoded)
    else:
        rates = report["throughput"]
        print(
            "Phase 2 instruction-cache oracle: "
            f"{rates['cache_hot_instructions_per_s_median']:,.0f} hot; "
            f"{rates['cache_disabled_instructions_per_s_median']:,.0f} "
            "disabled instructions/s"
        )
        print(
            "Diagnostic hot/disabled ratio: "
            f"{rates['hot_to_disabled_ratio_median']:.3f}"
        )
        if args.output is not None:
            print(f"JSON report: {args.output}")
    return (
        0
        if all(report["determinism"].values())
        and all(report["validation"].values())
        else 1
    )


if __name__ == "__main__":
    raise SystemExit(main())
