from devices import SECTOR_SIZE

import bench_phase0_concurrency as phase0


def test_phase3_benchmark_compares_one_two_and_four_lanes_exactly():
    default_args = phase0.build_parser().parse_args([])
    assert default_args.worker_counts == [1, 2, 4]
    assert not default_args.host_profile

    report = phase0.run_report(
        core_counts=[4],
        worker_counts=[1, 2, 4],
        scenario_names=["private_compute"],
        instructions=4_096,
        repeats=1,
        warmups=0,
        warmup_instructions=1,
        strict_dma_bytes=SECTOR_SIZE,
    )

    assert report["schema"] == phase0.SCHEMA
    assert report["schema_version"] == 19
    assert report["configuration"]["full_core_counts"] == [4]
    assert report["configuration"]["worker_counts"] == [1, 2, 4]
    assert not report["configuration"]["host_profile"]
    assert report["configuration"]["execution_order"][
        "worker_count_order"
    ] == [1, 2, 4]
    assert all(report["validation"].values())

    fixture_manifest = dict(report["fixture_manifest"])
    fixture_hash = fixture_manifest.pop("canonical_json_sha256")
    assert fixture_hash == phase0._json_sha256(fixture_manifest)
    assert set(fixture_manifest["assembled_programs"]) == {
        "private_compute",
        "shared_memory",
        "mmio_poll",
        "timer_interrupt",
        "legacy_storage_display_orchestration",
        "strict_nic_disk_dma",
    }

    assert [result["worker_count"] for result in report["results"]] == [
        1,
        2,
        4,
    ]
    for result in report["results"]:
        assert result["host_profile_probe"] is None
        assert result["lane_participation"] == {
            "required": True,
            "observed": True,
            "requirement_satisfied": True,
        }
        observation = result["accounting_probe"]["observation"]
        assert observation["state_schema_version"] == 12
        for core in observation["canonical_state"]["cores"]:
            assert core["instruction_cache"]["valid_lines"][
                "size_bytes"
            ] > 0
            assert core["instruction_cache"]["tags"][
                "element_count"
            ] > 0
            assert core["instruction_cache"]["data"]["size_bytes"] > 0

    group = report["cross_worker_equivalence_groups"][0]
    assert group["reference_worker_count"] == 1
    assert group["validation"] == {
        "one_lane_reference_present": True,
        "canonical_state_equal": True,
        "behavior_oracle_equal": True,
        "ordered_public_accounting_cycles_dispatches_stops_equal": True,
        "equivalent": True,
    }
    assert [
        member["one_lane_relative_throughput"]
        for member in group["members"]
    ][0] == 1.0

    strict = report["strict_nic_disk_dma"]
    assert [
        worker_report["configuration"]["worker_count"]
        for worker_report in strict["worker_reports"]
    ] == [1, 2, 4]
    assert strict["cross_worker_equivalence"]["reference_worker_count"] == 1
    assert strict["cross_worker_equivalence"]["validation"]["equivalent"]

    groups_without_reference = phase0._cross_worker_equivalence(
        [
            result
            for result in report["results"]
            if result["worker_count"] != 1
        ]
    )
    assert not groups_without_reference[0]["validation"]["equivalent"]
    strict_without_reference = phase0._strict_dma_cross_worker_equivalence(
        [
            worker_report
            for worker_report in strict["worker_reports"]
            if worker_report["configuration"]["worker_count"] != 1
        ]
    )
    assert not strict_without_reference["validation"]["equivalent"]


def test_single_core_fast_path_does_not_require_worker_participation():
    report = phase0.run_report(
        core_counts=[1],
        worker_counts=[1, 2, 4],
        scenario_names=["private_compute"],
        instructions=1_024,
        repeats=1,
        warmups=0,
        warmup_instructions=1,
        strict_dma_bytes=SECTOR_SIZE,
    )

    assert [
        result["worker_count"] for result in report["results"]
    ] == [1, 2, 4]
    for result in report["results"]:
        assert result["lane_participation"] == {
            "required": False,
            "observed": False,
            "requirement_satisfied": True,
        }
    assert all(report["validation"].values())
