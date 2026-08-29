from pathlib import Path

import pytest

import bench_bios_kdos_load as benchmark


def test_parser_selects_the_canonical_unprofiled_desktop_boot_shape() -> None:
    args = benchmark.build_parser().parse_args([])

    assert benchmark.SCHEMA_VERSION == 10
    assert args.runtime_root == benchmark.ROOT
    assert not args.host_profile
    assert args.max_steps == 2_000_000_000
    assert args.timeout == 120.0
    assert args.batch_steps == 500_000
    assert args.ram_kib == 1024
    assert args.ext_mem_mib == 128
    assert args.vram_mib == 4
    assert (args.cols, args.rows) == (280, 84)


@pytest.mark.parametrize("option", ("--max-steps", "--timeout", "--batch-steps"))
def test_parser_rejects_nonpositive_execution_bounds(option: str) -> None:
    with pytest.raises(SystemExit):
        benchmark.build_parser().parse_args([option, "0"])


def test_profile_derivation_exposes_coverage_churn_and_arena_cost() -> None:
    profile = {
        "counts": {
            "settle_round_calls": 100,
            "settle_round_native_calls": 75,
            "settle_round_python_calls": 25,
            "uncontended_block_hits": 75,
            "uncontended_block_lookups": 125,
            "uncontended_block_misses": 50,
            "uncontended_block_build_attempts": 40,
            "uncontended_block_nonresident_rejections": 3,
            "uncontended_block_zero_instruction_rejections": 6,
            "uncontended_block_one_instruction_rejections": 6,
            "uncontended_block_rejection_cache_hits": 10,
            "uncontended_block_rejection_cache_stores": 12,
            "uncontended_block_rejection_cache_replacements": 5,
            "uncontended_block_steps": 600,
            "uncontended_steps": 1_000,
            "uncontended_jit_steps": 400,
            "uncontended_jit_executions": 100,
            "uncontended_jit_compile_attempts": 10,
            "uncontended_jit_compilations": 8,
            "uncontended_block_evictions": 20,
            "uncontended_block_builds": 25,
            "uncontended_jit_plan_evictions": 4,
            "uncontended_jit_arena_allocations": 1,
            "uncontended_jit_arena_allocation_failures": 0,
            "uncontended_jit_slot_publications": 8,
            "uncontended_jit_slot_rewrites": 3,
            "uncontended_jit_code_bytes": 1_600,
        },
        "wall_ns": {
            "uncontended_jit_compile": 2_000_000,
            "uncontended_jit_arena_allocation": 80_000,
            "uncontended_jit_publication": 400_000,
        },
    }

    derived = benchmark.profile_derived(profile)

    assert derived == {
        "native_settlement_fraction": 0.75,
        "python_settlement_fraction": 0.25,
        "block_cache_hit_fraction": 0.6,
        "block_lookups_per_1000_steps": 125.0,
        "block_rejection_hit_fraction_of_lookups": 0.08,
        "block_build_attempt_fraction_of_lookups": 0.32,
        "block_rejection_cache_hit_fraction": 0.2,
        "block_build_success_fraction": 0.625,
        "resident_zero_instruction_rejection_fraction": 0.5,
        "resident_one_instruction_rejection_fraction": 0.5,
        "decoded_block_step_fraction": 0.6,
        "jit_step_fraction": 0.4,
        "jit_steps_per_execution": 4.0,
        "jit_compile_us_per_attempt": 200.0,
        "jit_arena_allocation_us_per_attempt": 80.0,
        "jit_publication_us_per_compilation": 50.0,
        "jit_publication_fraction_of_compile_time": 0.2,
        "block_evictions_per_build": 0.8,
        "plan_evictions_per_compilation": 0.5,
        "slot_rewrites_per_publication": 0.375,
        "average_jit_code_bytes": 200.0,
    }


def test_profile_cache_metadata_and_rejection_counters_reconcile() -> None:
    profile = {
        "single_core_block_cache": {
            "kind": "set-associative-exact-icache-span",
            "sets": 1_024,
            "ways": 4,
            "entries": 4_096,
            "identity_bytes": 16,
        },
        "single_core_block_rejection_cache": {
            "kind": "set-associative-exact-icache-span",
            "sets": 512,
            "ways": 4,
            "entries": 2_048,
            "identity_bytes": 16,
        },
        "single_core_jit_successor_profile": {
            "kind": "bounded-set-associative-space-saving",
            "scope": (
                "consecutive-complete-helper-free-register-control-x86_64-"
                "blocks-within-one-uncontended-segment"
            ),
            "sets": 1_024,
            "ways": 8,
            "entries": 8_192,
            "candidate_block_completions": 4,
            "observations": 3,
            "replacements": 0,
            "exact": True,
            "counter_saturated": False,
            "edges": [
                {
                    "source_address": 0x100,
                    "source_psel": 0,
                    "source_spsel": 0,
                    "source_identity_size": 4,
                    "source_identity_fingerprint": 0x1234,
                    "target_address": 0x104,
                    "target_psel": 0,
                    "target_spsel": 0,
                    "target_identity_size": 4,
                    "target_identity_fingerprint": 0x5678,
                    "estimated_count": 3,
                    "max_overcount": 0,
                }
            ],
        },
        "counts": {
            "uncontended_block_lookups": 125,
            "uncontended_block_misses": 50,
            "uncontended_block_build_attempts": 40,
            "uncontended_block_builds": 25,
            "uncontended_block_nonresident_rejections": 3,
            "uncontended_block_zero_instruction_rejections": 6,
            "uncontended_block_one_instruction_rejections": 6,
            "uncontended_block_rejection_cache_hits": 10,
            "uncontended_block_rejection_cache_stores": 12,
            "uncontended_block_rejection_cache_replacements": 5,
        },
    }

    validation = benchmark._profile_cache_validation(profile)

    assert validation == {
        "block_cache_metadata_supported": True,
        "block_rejection_cache_metadata_supported": True,
        "jit_successor_profile_metadata_supported": True,
        "jit_successor_profile_counters_are_bounded": True,
        "jit_successor_profile_exactness_is_explicit": True,
        "jit_successor_profile_edges_are_valid": True,
        "jit_successor_profile_order_is_deterministic": True,
        "block_build_attempts_reconcile": True,
        "block_rejection_cache_stores_reconcile": True,
        "block_rejection_cache_replacements_are_bounded": True,
        "block_rejection_activity_reconciles_with_misses": True,
    }


def test_runtime_root_must_contain_the_boot_sources(tmp_path: Path) -> None:
    with pytest.raises(RuntimeError, match="missing"):
        benchmark._activate_runtime(tmp_path)


def test_boot_image_is_byte_reproducible(tmp_path: Path) -> None:
    runtime = benchmark._activate_runtime(benchmark.ROOT)
    first = tmp_path / "first.img"
    second = tmp_path / "second.img"

    first_sources = benchmark._build_boot_image(runtime, first)
    second_sources = benchmark._build_boot_image(runtime, second)

    assert first.read_bytes() == second.read_bytes()
    assert first_sources["image_sha256"] == second_sources["image_sha256"]
    assert first_sources["mp64fs_fixture_mtime"] == 0


def test_exact_boot_transcript_exposes_any_extra_diagnostic() -> None:
    expected = benchmark._expected_output_lines(1 << 20)
    raw = "\r\n".join(["", *expected, ""])

    assert benchmark._output_lines(raw) == expected

    damaged = raw.replace(
        " Running autoexec.f...",
        "Stack overflow\r\n Running autoexec.f...",
    )
    assert benchmark._output_lines(damaged) != expected


def test_expected_boot_transcript_binds_configured_ram_and_boundary() -> None:
    lines = benchmark._expected_output_lines(2 << 20)

    assert "RAM: 00200000 bytes" in lines
    assert lines[-2:] == [benchmark.COMPLETION_MARKER, "> "]
