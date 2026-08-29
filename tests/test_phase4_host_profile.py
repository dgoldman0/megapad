import pytest

import bench_phase0_concurrency as phase0
from devices import SECTOR_SIZE


EMPTY_JIT_SUCCESSOR_PROFILE = {
    "kind": "bounded-set-associative-space-saving",
    "scope": (
        "consecutive-complete-helper-free-register-control-x86_64-blocks-"
        "within-one-uncontended-segment"
    ),
    "sets": 1_024,
    "ways": 8,
    "entries": 8_192,
    "candidate_block_completions": 0,
    "observations": 0,
    "replacements": 0,
    "exact": True,
    "counter_saturated": False,
    "edges": [],
}

JIT_REGION_COUNT_FIELDS = (
    "uncontended_jit_region_compile_attempts",
    "uncontended_jit_region_compilations",
    "uncontended_jit_region_compile_failures",
    "uncontended_jit_region_entries",
    "uncontended_jit_region_blocks",
    "uncontended_jit_region_steps",
    "uncontended_jit_region_target_identity_misses",
)


def _assert_completed_native_region_accounting(counts: dict) -> None:
    assert (
        counts["uncontended_jit_native_entries"]
        == counts["uncontended_jit_native_returns"]
    )
    assert (
        counts["uncontended_jit_region_compile_attempts"]
        == counts["uncontended_jit_region_compilations"]
        + counts["uncontended_jit_region_compile_failures"]
    )
    assert (
        counts["uncontended_jit_native_entries"]
        + counts["uncontended_jit_region_entries"]
        == counts["uncontended_jit_executions"]
    )
    assert (
        counts["uncontended_jit_region_blocks"]
        == 2 * counts["uncontended_jit_region_entries"]
    )
    assert (
        counts["uncontended_jit_region_entries"]
        <= counts["uncontended_jit_native_entries"]
    )
    assert (
        counts["uncontended_jit_region_blocks"]
        <= counts["uncontended_jit_region_steps"]
        <= counts["uncontended_jit_steps"]
    )


def test_bios_admission_mix_matches_production_profile_shape_exactly():
    accounting, probe = phase0._accounting_probe(
        phase0.SCENARIOS["bios_admission_mix"],
        1,
        1_000,
        host_profile=True,
    )

    assert probe is not None
    assert all(probe["validation"].values())
    counts = probe["native_snapshot"]["counts"]
    assert counts["uncontended_steps"] == 1_000
    assert counts["uncontended_block_lookups"] == 190
    assert counts["uncontended_block_hits"] == 170
    assert counts["uncontended_block_misses"] == 20
    assert counts["uncontended_block_rejection_cache_hits"] == 20
    assert counts["uncontended_block_build_attempts"] == 0
    assert counts["uncontended_block_builds"] == 0
    assert counts["uncontended_block_evictions"] == 0
    assert counts["uncontended_block_executions"] == 340
    assert counts["uncontended_block_steps"] == 980
    assert counts["uncontended_jit_compilations"] == 0
    assert counts["uncontended_jit_slot_publications"] == 0
    assert counts["uncontended_jit_executions"] == 340
    assert counts["uncontended_jit_steps"] == 980
    assert counts["uncontended_jit_native_entries"] == 170
    assert counts["uncontended_jit_native_returns"] == 170
    assert counts["uncontended_jit_region_compile_attempts"] == 0
    assert counts["uncontended_jit_region_compilations"] == 0
    assert counts["uncontended_jit_region_compile_failures"] == 0
    assert counts["uncontended_jit_region_entries"] == 170
    assert counts["uncontended_jit_region_blocks"] == 340
    assert counts["uncontended_jit_region_steps"] == 980
    assert counts["uncontended_jit_region_target_identity_misses"] == 0
    _assert_completed_native_region_accounting(counts)
    region_storage = probe["native_snapshot"][
        "single_core_jit_region_storage"
    ]
    assert region_storage["enabled"]
    assert region_storage["ready"]
    assert not region_storage["failed"]
    assert region_storage["slot_count"] == 4_096
    assert region_storage["slot_bytes"] > 0
    assert (
        region_storage["mapped_bytes_per_alias"]
        == region_storage["slot_count"] * region_storage["slot_bytes"]
    )
    successor = probe["native_snapshot"][
        "single_core_jit_successor_profile"
    ]
    assert successor["candidate_block_completions"] == 340
    assert successor["observations"] == 330
    assert successor["replacements"] == 0
    assert successor["exact"]
    assert not successor["counter_saturated"]
    assert len(successor["edges"]) == 33
    assert sum(
        edge["estimated_count"] for edge in successor["edges"]
    ) == 330
    assert all(
        edge["estimated_count"] == 10
        and edge["max_overcount"] == 0
        for edge in successor["edges"]
    )
    assert (
        counts["uncontended_steps"]
        - counts["uncontended_block_steps"]
        == 20
    )
    metrics = accounting["observation"]["workload_metrics"]
    assert metrics["expected_per_core_per_segment"] == {
        "block_lookups": 190,
        "positive_block_hits": 170,
        "rejection_cache_hits": 20,
        "block_executions": 340,
        "block_steps": 980,
        "native_region_entries": 170,
        "native_region_blocks": 340,
        "scalar_steps": 20,
    }


def test_phase4_host_profile_is_opt_in_and_reconciles_accounting():
    parser = phase0.build_parser()
    assert not parser.parse_args([]).host_profile
    assert parser.parse_args(["--host-profile"]).host_profile

    report = phase0.run_report(
        core_counts=[4],
        worker_counts=[1, 2],
        scenario_names=["mmio_poll"],
        instructions=4_096,
        repeats=1,
        warmups=0,
        warmup_instructions=1,
        strict_dma_bytes=SECTOR_SIZE,
        host_profile=True,
    )

    assert report["schema_version"] == 26
    assert report["configuration"]["host_profile"]
    assert report["validation"]["host_profile_presence_matches_request"]
    assert report["validation"]["all_host_profile_probes_valid"]
    assert all(report["validation"].values())

    for result in report["results"]:
        accounting = result["accounting_probe"]
        probe = result["host_profile_probe"]
        assert probe is not None
        assert probe["schema"] == "megapad.phase4-concurrency-host-profile"
        assert probe["schema_version"] == 17
        assert probe["architectural_hash_scope"] == "excluded_host_only"
        assert not probe["used_for_throughput"]
        assert all(probe["validation"].values())

        native = probe["native_snapshot"]
        counts = native["counts"]
        jit_storage = native["single_core_jit_storage"]
        jit_region_storage = native[
            "single_core_jit_region_storage"
        ]
        block_cache = native["single_core_block_cache"]
        block_rejection_cache = native[
            "single_core_block_rejection_cache"
        ]
        jit_successor_profile = native[
            "single_core_jit_successor_profile"
        ]
        assert not native["enabled"]
        assert native["generation"] > 0
        assert counts["batches"] == accounting["execution"]["run_batch_calls"]
        assert counts["scheduler_rounds"] > 0
        assert counts["uncontended_rounds"] == 0
        assert counts["uncontended_dispatches"] == 0
        assert counts["uncontended_steps"] == 0
        assert counts["uncontended_continuations"] == 0
        assert counts["uncontended_callback_errors"] == 0
        assert counts["uncontended_interrupt_boundaries"] == 0
        assert counts["logical_subfrontiers"] > 0
        assert counts["worker_waves"] > 0
        assert counts["worker_commands"] > 0
        assert counts["worker_bypassed_commands"] > 0
        assert (
            counts["frontier_routing_commands"]
            == counts["worker_commands"]
            + counts["worker_bypassed_commands"]
        )
        assert (
            counts["frontier_preclassification_commands"]
            == counts["frontier_routing_commands"]
        )
        assert (
            counts["frontier_preclassification_calls"]
            == counts["frontier_routing_commands"]
        )
        assert (
            counts["frontier_routing_waves"]
            > counts["worker_waves"]
        )
        assert (
            sum(counts["worker_bypass_stop_reasons"].values())
            == counts["worker_bypassed_commands"]
        )
        assert counts["worker_bypass_stop_reasons"] == {
            "halted": 0,
            "icache_boundary": 4,
            "idle": 0,
            "instruction_limit": 0,
            "internal_failure": 0,
            "interrupt_boundary": 0,
            "reset": 0,
            "shared_instruction":
                counts["worker_bypassed_commands"] - 4,
            "trap": 0,
        }
        assert counts["private_steps"] > 0
        assert (
            counts["checkpoint_captures"]
            == counts["worker_commands"]
            - counts["zero_step_commands"]
        )
        assert counts["checkpoint_restores"] == 0
        assert len(counts["lane_commands"]) == result["worker_count"]
        assert len(counts["lane_steps"]) == result["worker_count"]
        assert len(native["lane_active_ns"]) == result["worker_count"]
        assert native["wall_ns"]["frontier_fast_path"] > 0
        assert native["wall_ns"]["uncontended_round"] == 0
        assert native["wall_ns"]["uncontended_dispatch"] == 0
        for name in (
            "uncontended_block_build_attempts",
            "uncontended_block_nonresident_rejections",
            "uncontended_block_zero_instruction_rejections",
            "uncontended_block_one_instruction_rejections",
            "uncontended_block_rejection_cache_hits",
            "uncontended_block_rejection_cache_stores",
            "uncontended_block_rejection_cache_replacements",
            "uncontended_jit_plan_evictions",
            "uncontended_jit_arena_allocations",
            "uncontended_jit_arena_allocation_failures",
            "uncontended_jit_slot_publications",
            "uncontended_jit_slot_rewrites",
            "uncontended_jit_code_bytes",
            "uncontended_jit_max_code_bytes",
            "uncontended_jit_native_entries",
            "uncontended_jit_native_returns",
            *JIT_REGION_COUNT_FIELDS,
        ):
            assert counts[name] == 0
        assert block_cache == {
            "kind": "set-associative-exact-icache-span",
            "sets": 1_024,
            "ways": 4,
            "entries": 4_096,
            "identity_bytes": 16,
        }
        assert block_rejection_cache == {
            "kind": "set-associative-exact-icache-span",
            "sets": 512,
            "ways": 4,
            "entries": 2_048,
            "identity_bytes": 16,
        }
        assert jit_successor_profile == EMPTY_JIT_SUCCESSOR_PROFILE
        assert native["wall_ns"][
            "uncontended_jit_arena_allocation"
        ] == 0
        assert native["wall_ns"]["uncontended_jit_publication"] == 0
        assert not jit_storage["ready"]
        assert jit_storage["slot_count"] == 0
        assert jit_storage["slot_bytes"] == 0
        assert jit_storage["mapped_bytes_per_alias"] == 0
        assert isinstance(jit_region_storage["enabled"], bool)
        assert not jit_region_storage["ready"]
        assert not jit_region_storage["failed"]
        assert jit_region_storage["slot_count"] == 0
        assert jit_region_storage["slot_bytes"] == 0
        assert jit_region_storage["mapped_bytes_per_alias"] == 0

        callbacks = probe["python_callbacks"]
        assert callbacks["mmio_read_calls"] > 0
        assert callbacks["mmio_read_calls"] == sum(
            core["python_mmio_reads"] for core in accounting["per_core"]
        )
        assert callbacks["mmio_write_calls"] == sum(
            core["python_mmio_writes"] for core in accounting["per_core"]
        )
        assert (
            callbacks["device_tick_calls"]
            == accounting["device_bus_tick_calls"]
        )

        ratios = probe["structural_ratios"]
        assert ratios["uncontended_steps_per_dispatch"] is None
        assert (
            ratios["uncontended_step_fraction_of_returned_instructions"]
            == 0
        )
        assert ratios["worker_commands_per_wave"] is not None
        assert ratios["worker_wave_bypass_fraction"] > 0
        assert ratios["private_steps_per_worker_command"] is not None
        assert ratios["private_steps_per_logical_command"] is not None
        assert ratios["worker_bypass_fraction"] > 0
        assert (
            ratios["returned_instructions_per_logical_subfrontier"]
            is not None
        )


def test_single_core_profile_attributes_work_across_worker_counts():
    report = phase0.run_report(
        core_counts=[1],
        worker_counts=[1, 2, 4],
        scenario_names=["shared_memory"],
        instructions=2_503,
        repeats=1,
        warmups=0,
        warmup_instructions=1,
        strict_dma_bytes=SECTOR_SIZE,
        host_profile=True,
    )

    assert report["schema_version"] == 26
    assert all(report["validation"].values())
    for result in report["results"]:
        probe = result["host_profile_probe"]
        assert probe["schema_version"] == 17
        assert all(probe["validation"].values())
        native = probe["native_snapshot"]
        counts = native["counts"]
        jit_storage = native["single_core_jit_storage"]
        jit_region_storage = native[
            "single_core_jit_region_storage"
        ]
        block_cache = native["single_core_block_cache"]
        block_rejection_cache = native[
            "single_core_block_rejection_cache"
        ]
        jit_successor_profile = native[
            "single_core_jit_successor_profile"
        ]
        accounting = result["accounting_probe"]
        returned = accounting[
            "aggregate_instructions_from_per_core"
        ]

        assert counts["uncontended_rounds"] == counts["scheduler_rounds"]
        assert counts["uncontended_dispatches"] == accounting[
            "scheduler_provenance"
        ]["reported_native_dispatches"]
        assert counts["uncontended_steps"] == returned
        assert counts["logical_subfrontiers"] == 0
        assert counts["worker_commands"] == 0
        assert counts["private_steps"] == 0
        assert native["wall_ns"]["uncontended_round"] > 0
        assert native["wall_ns"]["uncontended_dispatch"] > 0
        assert block_cache == {
            "kind": "set-associative-exact-icache-span",
            "sets": 1_024,
            "ways": 4,
            "entries": 4_096,
            "identity_bytes": 16,
        }
        assert block_rejection_cache == {
            "kind": "set-associative-exact-icache-span",
            "sets": 512,
            "ways": 4,
            "entries": 2_048,
            "identity_bytes": 16,
        }
        assert {
            name: jit_successor_profile[name]
            for name in ("kind", "scope", "sets", "ways", "entries")
        } == {
            "kind": "bounded-set-associative-space-saving",
            "scope": (
                "consecutive-complete-helper-free-register-control-x86_64-"
                "blocks-within-one-uncontended-segment"
            ),
            "sets": 1_024,
            "ways": 8,
            "entries": 8_192,
        }
        assert (
            0
            <= jit_successor_profile["replacements"]
            <= jit_successor_profile["observations"]
            <= jit_successor_profile["candidate_block_completions"]
        )
        assert len(jit_successor_profile["edges"]) <= 8_192
        assert jit_successor_profile["exact"] == (
            jit_successor_profile["replacements"] == 0
            and not jit_successor_profile["counter_saturated"]
        )
        assert (
            counts["uncontended_block_build_attempts"]
            == counts["uncontended_block_builds"]
            + counts["uncontended_block_nonresident_rejections"]
            + counts["uncontended_block_zero_instruction_rejections"]
            + counts["uncontended_block_one_instruction_rejections"]
        )
        assert (
            counts["uncontended_block_rejection_cache_stores"]
            == counts["uncontended_block_zero_instruction_rejections"]
            + counts["uncontended_block_one_instruction_rejections"]
        )
        assert (
            counts["uncontended_block_rejection_cache_replacements"]
            <= counts["uncontended_block_rejection_cache_stores"]
        )
        assert (
            counts["uncontended_block_rejection_cache_hits"]
            + counts["uncontended_block_build_attempts"]
            == counts["uncontended_block_misses"]
        )
        assert (
            counts["uncontended_jit_slot_publications"]
            == counts["uncontended_jit_compilations"]
        )
        assert (
            counts["uncontended_jit_slot_rewrites"]
            <= counts["uncontended_jit_slot_publications"]
        )
        _assert_completed_native_region_accounting(counts)
        assert all(counts[name] == 0 for name in JIT_REGION_COUNT_FIELDS)
        assert (
            counts["uncontended_jit_plan_evictions"]
            <= counts["uncontended_block_builds"]
        )
        assert (
            counts["uncontended_jit_arena_allocations"]
            <= counts["uncontended_jit_compile_attempts"]
        )
        assert (
            counts["uncontended_jit_arena_allocation_failures"]
            <= counts["uncontended_jit_compile_attempts"]
        )
        assert (
            native["wall_ns"]["uncontended_jit_arena_allocation"]
            <= native["wall_ns"]["uncontended_jit_compile"]
        )
        assert (
            native["wall_ns"]["uncontended_jit_publication"]
            <= native["wall_ns"]["uncontended_jit_compile"]
        )
        if counts["uncontended_jit_slot_publications"] > 0:
            assert jit_storage["ready"]
            assert not jit_storage["failed"]
            assert jit_storage["slot_count"] > 0
            assert jit_storage["slot_bytes"] > 0
            assert (
                jit_storage["mapped_bytes_per_alias"]
                == jit_storage["slot_count"]
                * jit_storage["slot_bytes"]
            )
            assert (
                counts["uncontended_jit_max_code_bytes"]
                <= jit_storage["slot_bytes"]
            )
        assert isinstance(jit_region_storage["enabled"], bool)
        assert not jit_region_storage["ready"]
        assert not jit_region_storage["failed"]
        assert jit_region_storage["slot_count"] == 0
        assert jit_region_storage["slot_bytes"] == 0
        assert jit_region_storage["mapped_bytes_per_alias"] == 0

        ratios = probe["structural_ratios"]
        assert ratios["uncontended_steps_per_dispatch"] > 0
        assert (
            ratios["uncontended_step_fraction_of_returned_instructions"]
            == 1
        )


def test_profile_session_cannot_reset_or_freeze_reentrantly():
    workload = phase0.build_mmio_poll(
        num_cores=1,
        worker_count=1,
    )
    owner = workload.system._native_system
    cpu = workload.system.cores[0]
    original_read = cpu._mmio_read8
    callback_addresses = []

    def guarded_read(address: int):
        if not callback_addresses:
            with pytest.raises(
                RuntimeError,
                match="cannot start during an active native batch",
            ):
                owner._start_concurrency_profile()
            with pytest.raises(
                RuntimeError,
                match="cannot stop during an active native batch",
            ):
                owner._stop_concurrency_profile()
            callback_addresses.append(address)
        return original_read(address)

    cpu._mmio_read8 = guarded_read
    started = dict(owner._start_concurrency_profile())
    try:
        workload.execute(256)
        active = dict(owner._concurrency_profile_snapshot())
        assert callback_addresses
        assert active["enabled"]
        assert active["generation"] == started["generation"]
        assert int(active["counts"]["batches"]) > 0

        frozen = dict(owner._stop_concurrency_profile())
        assert not frozen["enabled"]
        assert dict(owner._concurrency_profile_snapshot()) == frozen
    finally:
        if dict(owner._concurrency_profile_snapshot())["enabled"]:
            owner._stop_concurrency_profile()
        workload.close()


def test_profile_scope_excludes_direct_private_diagnostics():
    workload = phase0.build_private_compute(
        num_cores=1,
        worker_count=1,
    )
    owner = workload.system._native_system
    owner._start_concurrency_profile()
    try:
        owner._run_private_full_core_commands([(0, 0, 1)])
        frozen = dict(owner._stop_concurrency_profile())
        assert not frozen["enabled"]
        counts = dict(frozen["counts"])
        assert all(
            int(value) == 0
            for value in counts.values()
            if not isinstance(value, (dict, list))
        )
        assert all(
            int(value) == 0
            for field in counts.values()
            if isinstance(field, dict)
            for value in field.values()
        )
        assert all(
            int(value) == 0
            for field in counts.values()
            if isinstance(field, list)
            for value in field
        )
        wall_ns = dict(frozen["wall_ns"])
        assert all(
            int(value) == 0
            for value in wall_ns.values()
            if not isinstance(value, dict)
        )
        assert all(
            int(value) == 0
            for field in wall_ns.values()
            if isinstance(field, dict)
            for value in field.values()
        )
        assert all(
            int(value) == 0
            for value in frozen["lane_active_ns"]
        )
    finally:
        if dict(owner._concurrency_profile_snapshot())["enabled"]:
            owner._stop_concurrency_profile()
        workload.close()
