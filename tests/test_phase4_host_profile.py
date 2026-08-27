import pytest

import bench_phase0_concurrency as phase0
from devices import SECTOR_SIZE


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

    assert report["schema_version"] == 16
    assert report["configuration"]["host_profile"]
    assert report["validation"]["host_profile_presence_matches_request"]
    assert report["validation"]["all_host_profile_probes_valid"]
    assert all(report["validation"].values())

    for result in report["results"]:
        accounting = result["accounting_probe"]
        probe = result["host_profile_probe"]
        assert probe is not None
        assert probe["schema"] == "megapad.phase4-concurrency-host-profile"
        assert probe["schema_version"] == 7
        assert probe["architectural_hash_scope"] == "excluded_host_only"
        assert not probe["used_for_throughput"]
        assert all(probe["validation"].values())

        native = probe["native_snapshot"]
        counts = native["counts"]
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

    assert report["schema_version"] == 16
    assert all(report["validation"].values())
    for result in report["results"]:
        probe = result["host_profile_probe"]
        assert probe["schema_version"] == 7
        assert all(probe["validation"].values())
        native = probe["native_snapshot"]
        counts = native["counts"]
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
