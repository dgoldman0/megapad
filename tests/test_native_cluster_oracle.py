"""Versioned Phase 2 element-6 cluster concurrency oracle."""

from __future__ import annotations

from bench_phase2_cluster import (
    ARBITRATION_CONTRACT,
    MILESTONE,
    REPORT_SCHEMA,
    REPORT_SCHEMA_VERSION,
    STATE_SCHEMA,
    STATE_SCHEMA_VERSION,
    run_report,
)


def test_cluster_concurrency_oracle_is_versioned_and_deterministic():
    report = run_report(repeats=2, warmups=0)

    assert report["schema"] == REPORT_SCHEMA
    assert report["schema_version"] == REPORT_SCHEMA_VERSION
    assert report["semantics"]["milestone"] == MILESTONE
    assert MILESTONE["label"] == "Phase 2 \u2014 element 6 of 7"
    assert (
        report["semantics"]["arbitration_contract"]
        == ARBITRATION_CONTRACT
    )
    assert ARBITRATION_CONTRACT == {
        "hard_qos_role": (
            "determines must/may eligibility and reserved entitlement only"
        ),
        "simultaneously_eligible_peer_order": "equal_round_robin",
        "unused_reserved_capacity": "work_conserving",
        "best_effort_weights": "none",
        "secondary_ordering_biases": [],
    }
    exclusions = report["semantics"]["explicit_exclusions"]
    assert any(
        "main-bus hard QoS" in item["state_or_behavior"]
        for item in exclusions
    )
    assert any(
        "same-hardware-cycle simultaneity" in item["state_or_behavior"]
        for item in exclusions
    )
    assert report["determinism"]["canonical_state_matches"]
    assert report["determinism"]["behavior_oracle_matches"]

    oracle_hashes = set()
    for sample in report["samples"]:
        observation = sample["observation"]
        assert observation["state_schema"] == STATE_SCHEMA
        assert observation["state_schema_version"] == STATE_SCHEMA_VERSION
        oracle_hashes.add(observation["behavior_oracle_sha256"])
        state = observation["canonical_state"]
        assert state["milestone"] == MILESTONE

        all_core = state["all_core_scheduling"]
        assert all_core["topology"] == {
            "num_full_cores": 4,
            "num_clusters": 3,
            "microcores_per_cluster": 4,
            "num_all_cores": 16,
            "cluster_enable_mask": 0xFFFF_FFFF_FFFF_FFFF,
            "clusters_enabled": [True, True, True],
        }
        execution = all_core["execution"]
        assert execution["winner_order"] == list(range(16)) + [0]
        assert execution["scheduler_cursor"] == 1
        assert execution["r1_by_core"] == [2] + [1] * 15
        assert execution["pc_by_core"] == [0x102] + [0x101] * 15
        assert execution["native_batch_runs_delta"] == 17
        assert execution["native_dispatches_delta"] == 17
        assert execution["authoritative_system_cycles"] == 17
        for slice_index, slice_state in enumerate(execution["slices"]):
            winner = slice_index % 16
            expected = [0] * 16
            expected[winner] = 1
            assert slice_state["winner_core_ids"] == [winner]
            assert slice_state["per_core_instructions"] == expected
            assert slice_state["per_core_cycles"] == expected
            assert slice_state["per_core_dispatches"] == expected
            assert slice_state["instructions_executed"] == 1
            assert slice_state["system_cycles_advanced"] == 1
            assert slice_state["native_scheduler"] is True
            assert slice_state["native_rounds"] == 1
            assert slice_state["native_continuations"] == 0
            assert slice_state["system_stop_reason"] == "instruction_limit"

        crc = state["cluster_crc_eligibility"]
        assert crc["topology"]["active_global_core_ids"] == [1, 2, 5, 6]
        assert (
            crc["workload"]["clusters_contending_at_unbounded_boundary"]
            == 2
        )
        assert crc["workload"]["hard_eligibility_exercised"] is True
        assert crc["workload"]["equal_peer_recontention_exercised"] is True
        assert crc["workload"]["host_requeues_after_round_two"] == [
            {"global_core_id": 2, "pc": 0x180},
            {"global_core_id": 6, "pc": 0x280},
        ]
        assert (
            crc["workload"]["arbitration_contract"]
            == ARBITRATION_CONTRACT
        )
        rounds = crc["rounds"]
        owner_instructions = [0, 0, 1, 0, 0, 0, 1, 0, 0]
        attempted_dispatches = [0, 1, 1, 0, 0, 1, 1, 0, 0]
        owner_cycles = [0, 0, 5, 0, 0, 0, 5, 0, 0]
        for round_state in rounds[:2]:
            observed = round_state["execution"]
            assert observed["per_core_instructions"] == owner_instructions
            assert observed["per_core_dispatches"] == attempted_dispatches
            assert observed["per_core_cycles"] == owner_cycles
            assert observed["instructions_executed"] == 2
            assert observed["system_cycles_advanced"] == 5
            assert observed["native_scheduler"] is True
            assert observed["native_rounds"] == 1
            assert observed["native_continuations"] == 2
            assert observed["system_stop_reason"] == "instruction_limit"
        assert [round_state["scheduler_cursor"] for round_state in rounds] == [
            7, 7, 6
        ]
        assert rounds[0]["active_pcs"] == {
            "cluster0_local0": 0x100,
            "cluster0_local1": 0x183,
            "cluster1_local0": 0x200,
            "cluster1_local1": 0x283,
        }
        assert rounds[0]["sibling_visible_modes"] == [1, 2]
        assert rounds[0]["crc"] == [
            {
                "acc": 0xFFFF_FFFF,
                "mode": 1,
                "locked": True,
                "owner_local_core": 1,
            },
            {
                "acc": 0xFFFF_FFFF,
                "mode": 2,
                "locked": True,
                "owner_local_core": 1,
            },
        ]
        assert rounds[1]["active_pcs"] == {
            "cluster0_local0": 0x100,
            "cluster0_local1": 0x186,
            "cluster1_local0": 0x200,
            "cluster1_local1": 0x286,
        }
        assert rounds[1]["crc"] == [
            {
                "acc": 0,
                "mode": 1,
                "locked": False,
                "owner_local_core": None,
            },
            {
                "acc": 0xFFFF_FFFF_0000_0000,
                "mode": 2,
                "locked": False,
                "owner_local_core": None,
            },
        ]

        eventual = rounds[2]["execution"]
        eventual_instructions = [0, 1, 0, 0, 0, 1, 0, 0, 0]
        eventual_dispatches = [0, 1, 1, 0, 0, 1, 1, 0, 0]
        assert eventual["per_core_instructions"] == eventual_instructions
        assert eventual["per_core_dispatches"] == eventual_dispatches
        assert eventual["per_core_cycles"] == [
            0, 5, 0, 0, 0, 5, 0, 0, 0
        ]
        assert eventual["instructions_executed"] == 2
        assert eventual["system_cycles_advanced"] == 5
        assert eventual["native_scheduler"] is True
        assert eventual["native_rounds"] == 1
        assert eventual["native_continuations"] == 2
        assert eventual["system_stop_reason"] == "instruction_limit"
        assert rounds[2]["active_pcs"] == {
            "cluster0_local0": 0x103,
            "cluster0_local1": 0x180,
            "cluster1_local0": 0x203,
            "cluster1_local1": 0x280,
        }
        assert rounds[2]["crc"] == [
            {
                "acc": 0,
                "mode": 0,
                "locked": True,
                "owner_local_core": 0,
            },
            {
                "acc": 0xFFFF_FFFF_0000_0000,
                "mode": 2,
                "locked": True,
                "owner_local_core": 0,
            },
        ]
        for index, round_state in enumerate(rounds, start=1):
            for arbiter in round_state["arbiters"]:
                assert arbiter["grant_counts"]["crc"] == index
                assert arbiter["grant_sequence"] == index
                expected_cursor = 1 if index < 3 else 0
                assert (
                    arbiter["round_robin_cursors"]["crc"]
                    == expected_cursor
                )
        assert crc["final"] == {
            "native_batch_runs_delta": 3,
            "native_dispatches_delta": 12,
            "authoritative_system_cycles": 15,
        }

    assert len(oracle_hashes) == 1
