"""Versioned Phase 2 element-7 instruction-cache oracle."""

import hashlib
from pathlib import Path

import _mp64_accel

from bench_phase2_icache import (
    ARBITRATION_CONTRACT,
    CACHE_CONTRACT,
    EVIDENCE_MATRIX,
    EXPLICIT_EXCLUSIONS,
    FIXTURE_MANIFEST,
    HOOK_BODY,
    HOST_CODE_SAFETY_CONTRACT,
    MILESTONE,
    REPORT_MEASURED_CLAIMS,
    REPORT_SCHEMA,
    REPORT_SCHEMA_VERSION,
    RTL_TRACE_CONTRACT,
    STATE_SCHEMA,
    STATE_SCHEMA_VERSION,
    STRICT_LOAD_REJECTION_REASON,
    run_report,
)


def test_instruction_cache_oracle_is_versioned_and_deterministic():
    report = run_report(
        repeats=2,
        warmups=0,
        benchmark_instructions=128,
    )
    assert report["schema"] == REPORT_SCHEMA
    assert report["schema_version"] == REPORT_SCHEMA_VERSION
    semantics = report["semantics"]
    assert semantics["milestone"] == MILESTONE
    assert MILESTONE["label"] == "Phase 2 \u2014 element 7 of 7"
    assert semantics["cache_contract"] == CACHE_CONTRACT
    assert (
        semantics["host_code_safety_contract"]
        == HOST_CODE_SAFETY_CONTRACT
    )
    assert semantics["rtl_trace_contract"] == RTL_TRACE_CONTRACT
    assert semantics["arbitration_contract"] == ARBITRATION_CONTRACT
    assert semantics["report_measured_claims"] == REPORT_MEASURED_CLAIMS
    assert semantics["evidence_matrix"] == EVIDENCE_MATRIX
    assert semantics["explicit_exclusions"] == EXPLICIT_EXCLUSIONS
    assert semantics["performance_gate"] == "none"
    assert ARBITRATION_CONTRACT["best_effort_weights"] == "none"
    assert ARBITRATION_CONTRACT["secondary_ordering_biases"] == []
    assert (
        RTL_TRACE_CONTRACT["full_core_local_port_mux"][
            "idle_simultaneous_sources"
        ]
        == "instruction_cache_first"
    )
    integrated_bus = RTL_TRACE_CONTRACT["integrated_physical_main_bus"]
    assert integrated_bus["hardware_weight_register_reset"] == "all_ones"
    assert (
        integrated_bus["hardware_bandwidth_limit_register_reset"]
        == "all_zero_unlimited"
    )
    assert integrated_bus["architectural_best_effort_weights"] == "absent"
    assert not RTL_TRACE_CONTRACT[
        "generic_weighted_testbench_mode_is_architectural_policy"
    ]
    assert all(report["determinism"].values())
    assert all(report["validation"].values())

    excluded_states = {
        entry["state_or_behavior"] for entry in EXPLICIT_EXCLUSIONS
    }
    assert "cross-backend full-core BIST postconditions" in excluded_states
    assert (
        "external unified-memory routing for LOAD2D/STORE2D"
        in excluded_states
    )
    assert "external tile addresses above 32 bits" in excluded_states

    expected_fixtures = {
        "benchmark.hot_loop",
        "backend.initial_target",
        "backend.same_line",
        "backend.first_mutation",
        "backend.second_mutation",
        "backend.reset",
        "micro.initial",
        "micro.mutation",
        "tag.low",
        "tag.high",
        "strict.initial",
        "strict.mutation",
        "hook.caller",
        "hook.body",
        "hook.last_byte_mutation",
    }
    assert set(FIXTURE_MANIFEST) == expected_fixtures
    assert all(
        entry["size_bytes"] > 0
        and len(entry["sha256"]) == 64
        and entry["role"]
        for entry in FIXTURE_MANIFEST.values()
    )

    repository = report["repository"]
    assert repository["commit"]
    assert repository["root"]
    artifact = report["host"]["accelerator"]
    artifact_path = Path(_mp64_accel.__file__).resolve()
    assert artifact["loaded_artifact_path"] == str(artifact_path)
    assert artifact["loaded_artifact_size_bytes"] == artifact_path.stat().st_size
    assert artifact["loaded_artifact_sha256"] == hashlib.sha256(
        artifact_path.read_bytes()
    ).hexdigest()

    oracle_hashes = set()
    for sample in report["samples"]:
        observation = sample["observation"]
        assert observation["state_schema"] == STATE_SCHEMA
        assert observation["state_schema_version"] == STATE_SCHEMA_VERSION
        oracle_hashes.add(observation["behavior_oracle_sha256"])
        state = observation["canonical_state"]
        assert state["fixture_manifest"] == FIXTURE_MANIFEST
        assert (
            len(observation["component_sha256"]["fixture_manifest"]) == 64
        )
        assert state["backend_trace_matches"]
        trace = state["native_cache_trace"]
        assert trace["geometry"] == {
            "valid_entries": 256,
            "tag_entries": 256,
            "data_bytes": 4096,
        }
        assert trace["cold_and_same_line"]["first_cache"] == {
            "enabled": 1,
            "hits": 1,
            "misses": 1,
        }
        assert trace["shared_backing_mutation_stays_stale"] == {
            "first_r5_r7": [2, 0],
            "second_r5_r7": [2, 0],
        }
        assert trace["control_0_bypass"]["r7"] == 1
        assert trace["control_1_preserved_line"]["r5"] == 3
        assert trace["first_core_only_control_3"][
            "second_r5_r7"
        ] == [3, 0]
        assert trace["second_control_2"] == {
            "enabled": 0,
            "hits": 0,
            "misses": 0,
        }
        assert trace["second_disabled_bypass"]["r7"] == 1
        assert trace["second_control_3"] == {
            "enabled": 1,
            "hits": 0,
            "misses": 0,
        }
        assert trace["guest_reset"] == {
            "enabled": 1,
            "hits": 0,
            "misses": 0,
        }
        assert trace["guest_reset_refetch"]["r5_r7_r8"] == [0, 0, 1]
        assert trace["microcore"] == {
            "r5_r6": [1, 1],
            "cache": {"enabled": 0, "hits": 0, "misses": 0},
        }
        assert trace["full_physical_tag"] == {
            "addresses": [0, 1 << 20],
            "same_direct_map_index": True,
            "r9_r10": [2, 1],
            "cache": {"enabled": 1, "hits": 0, "misses": 3},
        }

        strict = state["strict_cold_refill"]
        assert strict["first_slice"]["instructions"] == 0
        assert strict["pending"]["operation"] == "read"
        assert strict["pending"]["width"] == "doubleword"
        assert strict["pending"]["address"] == 8
        assert strict["pending"]["issue_sequence"] == 2
        assert strict["official_load_rejection"] == {
            "reason_code": STRICT_LOAD_REJECTION_REASON,
            "exception_type": "RuntimeError",
        }
        assert strict["retired"] == {
            "pc": 1,
            "r1": 0,
            "cache": {"enabled": 1, "hits": 0, "misses": 1},
        }
        assert strict["official_load_outside_suspension"] == {
            "accepted": True,
            "without_invalidation": {
                "pc": 1,
                "r1": 0,
                "cache": {"enabled": 1, "hits": 1, "misses": 1},
            },
            "after_explicit_invalidation": {
                "pc": 1,
                "r1": 1,
                "cache": {"enabled": 1, "hits": 0, "misses": 1},
            },
        }
        assert strict["bus_deltas"] == {"issues": 2, "grants": 2}

        hooks = state["accelerator_code_identity"]
        layout = hooks["registration"]
        accepted = hooks["matching_identity"]
        assert layout["code_size"] == len(HOOK_BODY) == 3
        assert accepted["cycles"] == 21
        assert accepted["pc"] == 2
        assert accepted["destination"] == "efbeefbe"
        declined = hooks["changed_identity"]
        assert declined["mutation_offset"] == 2
        assert declined["mutation_address"] == layout["target"] + 2
        assert declined["mutated_span_sha256"] != layout["code_sha256"]
        assert declined["cycles"] == 2
        assert declined["pc"] == layout["target"]
        assert declined["return_address"] == 2
        assert declined["destination"] == "00000000"

        benchmark = sample["benchmark"]
        assert benchmark["program_sha256"] == FIXTURE_MANIFEST[
            "benchmark.hot_loop"
        ]["sha256"]
        assert benchmark["cache_hot_matches_disabled"]
        hot = benchmark["cache_hot"]
        disabled = benchmark["cache_disabled"]
        assert hot["state"]["architectural"] == disabled["state"][
            "architectural"
        ]
        architecture = hot["state"]["architectural"]
        assert architecture == {
            "steps": 128,
            "cycles": 192,
            "stop_reason": 0,
            "pc": 0,
            "r1": 64,
            "core_cycle_count": 192,
            "halted": False,
            "idle": False,
        }
        assert hot["state"]["cache_diagnostics"] == {
            "enabled": 1,
            "hit_delta": 128,
            "miss_delta": 0,
        }
        assert disabled["state"]["cache_diagnostics"] == {
            "enabled": 0,
            "hit_delta": 0,
            "miss_delta": 0,
        }
        for mode in (hot, disabled):
            assert not mode["cyclic_gc"]["enabled_during_timing"]
            assert mode["cyclic_gc"]["restored_to_prior_state"]

    assert len(oracle_hashes) == 1
    assert [
        sample["benchmark"]["execution_order"]
        for sample in report["samples"]
    ] == [
        ["cache_hot", "cache_disabled"],
        ["cache_disabled", "cache_hot"],
    ]
