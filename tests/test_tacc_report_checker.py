"""Synthetic, lightweight coverage for the routed TACC report contract."""

from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
import subprocess

import pytest

from fpga import check_tacc_reports as checker
from fpga import run_tacc_impl as runner


_AVAILABLE = {
    "lut": 203_800.0,
    "ff": 407_600.0,
    "bram": 445.0,
    "dsp": 840.0,
}

_FULL_COMMIT = "f" * 40
_FULL_MANIFEST_SHA256 = "d" * 64
_HARNESS_MANIFEST_SHA256 = "e" * 64
_CAMPAIGN_ID = "tacc-physical-acceptance-test-campaign"

_STRATEGY = {
    "top": "mp64_soc",
    "synth_flatten_hierarchy": "none",
    "synth_directive": "AreaOptimized_high",
    "synth_retiming": "on",
    "synth_verilog_define": "SIMULATION=0",
    "opt_directive": "tool-default",
    "place_directive": "tool-default",
    "phys_opt_directive": "tool-default",
    "route_directive": "tool-default",
    "memory_configuration": (
        "effective_mem_depth=4096;fixed_banks=4;row_bits=512;"
        "address_contract=depth-derived"
    ),
}


def _resources(used: dict[str, float]) -> dict[str, dict[str, float]]:
    return {
        resource: {
            "used": float(used[resource]),
            "available": available,
        }
        for resource, available in _AVAILABLE.items()
    }


def _report(label: str) -> dict:
    source_commits = {
        "current-main": checker.CURRENT_MAIN_SHA,
        "topology-only": checker.TOPOLOGY_ONLY_SHA,
        "full-tacc": _FULL_COMMIT,
    }
    source_manifests = {
        "current-main": checker.CURRENT_MAIN_MANIFEST_SHA256,
        "topology-only": checker.TOPOLOGY_ONLY_MANIFEST_SHA256,
        "full-tacc": _FULL_MANIFEST_SHA256,
    }
    source_kind = "tree" if label == "full-tacc" else "ref"
    source_requested = (
        "/tmp/full-tacc"
        if label == "full-tacc"
        else source_commits[label]
    )
    synth_used = {
        "current-main": {
            "lut": 1_000,
            "ff": 2_000,
            "bram": 40,
            "dsp": 80,
        },
        "topology-only": {
            "lut": 2_000,
            "ff": 3_000,
            "bram": 50,
            "dsp": 100,
        },
        "full-tacc": {
            "lut": 2_200,
            "ff": 3_180,
            "bram": 50,
            "dsp": 104,
        },
    }[label]
    route_used = {
        resource: value * 1.02 for resource, value in synth_used.items()
    }
    hierarchy = {
        "current-main": {"mp64_tile": 4, "mp64_tacc": 0},
        "topology-only": {"mp64_tile": 7, "mp64_tacc": 0},
        "full-tacc": {"mp64_tile": 7, "mp64_tacc": 7},
    }[label]
    fmax = {
        "current-main": 125.0,
        "topology-only": 120.0,
        "full-tacc": 110.0,
    }[label]
    wns = (1000.0 / checker.TARGET_CLOCK_MHZ) - (1000.0 / fmax)
    hierarchical_resources = {
        "mp64_tile": {
            resource: float(synth_used[resource]) * 0.6
            for resource in checker.RESOURCE_NAMES
        },
        "mp64_tacc": {
            resource: (
                float(synth_used[resource]) * 0.1
                if label == "full-tacc"
                else 0.0
            )
            for resource in checker.RESOURCE_NAMES
        },
    }
    structural = {
        "tacc_specific_multiplier_arrays": 0,
        "max_fp_feedback_lanes_per_engine": (
            16 if label == "full-tacc" else 0
        ),
        "persistent_tacc_bits": (
            checker.PERSISTENT_TACC_BITS if label == "full-tacc" else 0
        ),
        "shared_tacc_stage_bits": (
            checker.SHARED_TACC_STAGE_BITS if label == "full-tacc" else 0
        ),
        "tacc_bram_cells": 0,
        "multiplier_sharing_verified": True,
        "fp_adder_sharing_verified": True,
        "bounded_feedback_path_verified": True,
    }
    reports = {
        key: {
            "path": f"build/{filename}",
            "sha256": "a" * 64,
        }
        for key, filename in checker.RAW_REPORT_FILES.items()
    }
    return {
        "schema": checker.REPORT_SCHEMA,
        "schema_version": checker.REPORT_SCHEMA_VERSION,
        "label": label,
        "provenance": {
            "source": {
                "kind": source_kind,
                "requested": source_requested,
                "commit": source_commits[label],
                "manifest_path": "source_manifest.json",
                "manifest_sha256": source_manifests[label],
                "root": "source",
                "dirty": label == "full-tacc",
            },
            "measurement_harness": {
                "manifest_path": "measurement_harness_manifest.json",
                "manifest_sha256": _HARNESS_MANIFEST_SHA256,
                "root": "measurement_harness",
                "campaign_id": _CAMPAIGN_ID,
            },
            "tool": {
                "name": "Vivado",
                "version": (
                    "Vivado v2025.2 (64-bit) | SW Build 1234567 | "
                    "IP Build 1234500"
                ),
            },
            "target": {
                "part": "xc7k325tffg900-2",
                "clock_mhz": 100.0,
                "constraints_path": "fpga/constraints/genesys2.xdc",
                "constraints_sha256": "c" * 64,
            },
            "strategy": copy.deepcopy(_STRATEGY),
            "routed": True,
            "reports": reports,
        },
        "resources": {
            "post_synth": _resources(synth_used),
            "post_route": _resources(route_used),
        },
        "timing": {
            "post_route": {
                "wns_ns": wns,
                "tns_ns": 0.0,
                "fmax_mhz": fmax,
                "target_mhz": 100.0,
                "unconstrained_paths": 0,
            }
        },
        "hierarchy": hierarchy,
        "hierarchical_resources": hierarchical_resources,
        "route_status": {
            "is_route_design": True,
            "status": "routed",
            "errors_in_routes": False,
        },
        "structural": structural,
    }


def _passing_reports() -> dict[str, dict]:
    return {
        label: _report(label)
        for label in ("current-main", "topology-only", "full-tacc")
    }


def _compare(reports: dict[str, dict]) -> dict:
    return dict(
        checker.compare_reports(
            reports["current-main"],
            reports["topology-only"],
            reports["full-tacc"],
            expected_full_commit=_FULL_COMMIT,
            expected_full_manifest_sha256=_FULL_MANIFEST_SHA256,
        )
    )


def _set_nested(value: dict, path: tuple[str, ...], replacement: object) -> None:
    target = value
    for component in path[:-1]:
        target = target[component]
    target[path[-1]] = replacement


def test_locked_acceptance_matrix_passes_and_reports_both_deltas() -> None:
    result = _compare(_passing_reports())

    assert result["passed"] is True
    assert result["issues"] == []
    assert result["expected_full_source"] == {
        "commit": _FULL_COMMIT,
        "manifest_sha256": _FULL_MANIFEST_SHA256,
    }
    comparisons = result["comparisons"]
    assert (
        comparisons["topology_to_tacc_post_synth_growth_percent"]["lut"]
        == pytest.approx(10.0)
    )
    assert (
        comparisons["whole_design_resource_delta"]
        ["current_main_to_topology"]["post_synth"]["lut"]
        == 1_000.0
    )
    assert (
        comparisons["post_route_timing_delta"]
        ["topology_to_full_tacc"]["fmax_mhz"]
        == -10.0
    )
    assert (
        comparisons["hierarchical_resource_delta"]
        ["topology_to_full_tacc"]["mp64_tacc"]["lut"]
        > 0.0
    )


@pytest.mark.parametrize(
    ("path", "value", "issue_fragment"),
    [
        (
            ("resources", "post_synth", "lut", "used"),
            2_241.0,
            "lut growth",
        ),
        (
            ("resources", "post_synth", "ff", "used"),
            3_211.0,
            "ff growth",
        ),
        (
            ("resources", "post_synth", "dsp", "used"),
            106.0,
            "dsp growth",
        ),
        (
            ("resources", "post_synth", "bram", "used"),
            51.0,
            "BRAM delta",
        ),
        (
            ("resources", "post_route", "lut", "used"),
            194_000.0,
            "lut headroom",
        ),
        (
            ("resources", "post_route", "ff", "used"),
            390_000.0,
            "ff headroom",
        ),
        (
            ("resources", "post_route", "dsp", "used"),
            800.0,
            "dsp headroom",
        ),
        (
            ("timing", "post_route", "wns_ns"),
            -0.001,
            "WNS is negative",
        ),
        (
            ("timing", "post_route", "tns_ns"),
            -0.001,
            "TNS is negative",
        ),
        (
            ("timing", "post_route", "fmax_mhz"),
            100.0,
            "Fmax regression",
        ),
        (
            ("timing", "post_route", "unconstrained_paths"),
            1,
            "unconstrained timing paths",
        ),
        (
            ("hierarchy", "mp64_tacc"),
            6,
            "expected 7",
        ),
        (
            ("hierarchy", "mp64_tile"),
            6,
            "mp64_tile instances, expected 7",
        ),
        (
            ("structural", "tacc_specific_multiplier_arrays"),
            1,
            "multiplier arrays are present",
        ),
        (
            ("structural", "tacc_bram_cells"),
            1,
            "consumes BRAM",
        ),
        (
            ("structural", "max_fp_feedback_lanes_per_engine"),
            17,
            "outside the locked 1..16 bound",
        ),
        (
            ("structural", "persistent_tacc_bits"),
            checker.PERSISTENT_TACC_BITS - 1,
            "14,336 bits",
        ),
        (
            ("structural", "shared_tacc_stage_bits"),
            checker.SHARED_TACC_STAGE_BITS - 1,
            "2,048 bits",
        ),
        (
            ("structural", "multiplier_sharing_verified"),
            False,
            "multiplier sharing review",
        ),
        (
            ("structural", "fp_adder_sharing_verified"),
            False,
            "FP-adder sharing review",
        ),
        (
            ("structural", "bounded_feedback_path_verified"),
            False,
            "bounded product/add/ownership",
        ),
    ],
)
def test_each_locked_full_tacc_gate_fails_closed(
    path: tuple[str, ...],
    value: object,
    issue_fragment: str,
) -> None:
    reports = _passing_reports()
    _set_nested(reports["full-tacc"], path, value)

    result = _compare(reports)

    assert result["passed"] is False
    assert any(issue_fragment in issue for issue in result["issues"])


def test_zero_resource_baseline_is_strict_json_and_fails() -> None:
    reports = _passing_reports()
    reports["topology-only"]["resources"]["post_synth"]["lut"]["used"] = 0.0
    reports["full-tacc"]["resources"]["post_synth"]["lut"]["used"] = 1.0

    result = _compare(reports)

    assert result["passed"] is False
    assert (
        result["comparisons"]["topology_to_tacc_post_synth_growth_percent"][
            "lut"
        ]
        is None
    )
    json.dumps(result, allow_nan=False)


def test_provenance_capacity_and_route_evidence_are_like_for_like() -> None:
    mutations = [
        (
            ("provenance", "tool", "version"),
            (
                "Vivado v2026.1 (64-bit) | SW Build 7654321 | "
                "IP Build 7654300"
            ),
            "tool.version differs",
        ),
        (
            ("provenance", "strategy", "route_directive"),
            "Explore",
            "strategy differs",
        ),
        (
            ("resources", "post_route", "dsp", "available"),
            900.0,
            "capacity",
        ),
        (
            ("route_status", "is_route_design"),
            False,
            "not implementation",
        ),
        (
            ("route_status", "status"),
            "unrouted",
            "not routed",
        ),
        (
            ("route_status", "errors_in_routes"),
            True,
            "route errors",
        ),
        (
            ("provenance", "routed"),
            False,
            "report is not post-route",
        ),
        (
            ("provenance", "target", "clock_mhz"),
            99.0,
            "target clock",
        ),
    ]
    for path, value, issue_fragment in mutations:
        reports = _passing_reports()
        _set_nested(reports["full-tacc"], path, value)
        result = _compare(reports)
        assert result["passed"] is False
        assert any(issue_fragment in issue for issue in result["issues"])


@pytest.mark.parametrize(
    ("path", "value", "issue_fragment"),
    [
        (("provenance", "tool", "name"), "Other", "physical tool must be Vivado"),
        (
            ("provenance", "tool", "version"),
            "2025.2",
            "identity must include version",
        ),
        (
            ("provenance", "target", "part"),
            "xc7k160tffg676-2",
            "target part must be xc7k325tffg900-2",
        ),
        (
            ("provenance", "strategy", "top"),
            "mp64_synth_top",
            "comparison top must be mp64_soc",
        ),
    ],
)
def test_locked_physical_tool_and_target_are_enforced(
    path: tuple[str, ...],
    value: object,
    issue_fragment: str,
) -> None:
    reports = _passing_reports()
    _set_nested(reports["current-main"], path, value)

    result = _compare(reports)

    assert result["passed"] is False
    assert any(issue_fragment in issue for issue in result["issues"])


@pytest.mark.parametrize(
    ("configuration", "issue_fragment"),
    [
        (
            "effective_mem_depth=unresolved;fixed_banks=4;row_bits=512;"
            "address_contract=fixed-16384",
            "memory configuration is not concrete",
        ),
        (
            "effective_mem_depth=16384;fixed_banks=4;row_bits=512;"
            "address_contract=fixed-16384",
            "at least 1024 RAMB36",
        ),
        (
            "effective_mem_depth=4096;fixed_banks=4;row_bits=512;"
            "address_contract=fixed-16384",
            "lacks a depth-derived address contract",
        ),
    ],
)
def test_physical_memory_configuration_must_be_concrete_and_fit(
    configuration: str,
    issue_fragment: str,
) -> None:
    reports = _passing_reports()
    for report in reports.values():
        report["provenance"]["strategy"]["memory_configuration"] = configuration

    result = _compare(reports)

    assert result["passed"] is False
    assert any(issue_fragment in issue for issue in result["issues"])


def test_immutable_baselines_require_clean_full_sha_ref_materialization() -> None:
    reports = _passing_reports()
    reports["current-main"]["provenance"]["source"].update(
        {
            "kind": "tree",
            "dirty": True,
            "requested": "main",
        }
    )

    result = _compare(reports)

    assert result["passed"] is False
    assert any("materialized by ref" in issue for issue in result["issues"])
    assert any("provenance is dirty" in issue for issue in result["issues"])
    assert any("locked full commit SHA" in issue for issue in result["issues"])


@pytest.mark.parametrize(
    ("field", "replacement", "issue_fragment"),
    [
        ("commit", "0" * 40, "does not match expected full commit"),
        (
            "manifest_sha256",
            "0" * 64,
            "does not match expected full manifest",
        ),
    ],
)
def test_full_source_is_bound_to_explicit_expected_identity(
    field: str,
    replacement: str,
    issue_fragment: str,
) -> None:
    reports = _passing_reports()
    reports["full-tacc"]["provenance"]["source"][field] = replacement

    result = _compare(reports)

    assert result["passed"] is False
    assert any(issue_fragment in issue for issue in result["issues"])


def test_baseline_source_manifests_are_locked_to_prepared_snapshots() -> None:
    assert (
        checker.CURRENT_MAIN_MANIFEST_SHA256
        == "064cdb7f06c88afa9107887b084ad19796cb9d65459410790e89e0c4706c95eb"
    )
    assert (
        checker.TOPOLOGY_ONLY_MANIFEST_SHA256
        == "87601b49375ce86be7218d8f10cf75611e97df902cb45f18a4232516f3e54e09"
    )
    for label in ("current-main", "topology-only"):
        reports = _passing_reports()
        reports[label]["provenance"]["source"]["manifest_sha256"] = "0" * 64

        result = _compare(reports)

        assert result["passed"] is False
        assert any(
            f"{label}: source manifest is not the locked baseline manifest"
            in issue
            for issue in result["issues"]
        )


@pytest.mark.parametrize(
    ("field", "replacement", "issue_fragment"),
    [
        (
            "manifest_sha256",
            "0" * 64,
            "measurement-harness manifest differs",
        ),
        ("campaign_id", "different-campaign", "measurement campaign differs"),
    ],
)
def test_all_reports_share_one_attested_measurement_campaign(
    field: str,
    replacement: str,
    issue_fragment: str,
) -> None:
    reports = _passing_reports()
    reports["full-tacc"]["provenance"]["measurement_harness"][
        field
    ] = replacement

    result = _compare(reports)

    assert result["passed"] is False
    assert any(issue_fragment in issue for issue in result["issues"])


def _utilization_text(
    *,
    lut: float,
    ff: float,
    bram: float,
    dsp: float,
) -> str:
    return "\n".join(
        (
            "| Site Type       | Used | Fixed | Available | Util% |",
            f"| Slice LUTs      | {lut} | 0 | {_AVAILABLE['lut']} | 1.0 |",
            f"| Slice Registers | {ff} | 0 | {_AVAILABLE['ff']} | 1.0 |",
            f"| Block RAM Tile  | {bram} | 0 | {_AVAILABLE['bram']} | 1.0 |",
            f"| DSPs            | {dsp} | 0 | {_AVAILABLE['dsp']} | 1.0 |",
        )
    )


def _hierarchy_text(label: str) -> str:
    report = _report(label)
    lines = [
        "| Instance | Module | Total LUTs | Registers | Block RAM Tile | DSPs |"
    ]
    for module, count in report["hierarchy"].items():
        lines.extend(
            f"| /synthetic/{module}_{index} | {module} | 1 | 1 | 0 | 0 |"
            for index in range(count)
        )
    lines.extend(
        f"TACC_HIERARCHY {module} {count}"
        for module, count in report["hierarchy"].items()
    )
    for module, resources in report["hierarchical_resources"].items():
        lines.extend(
            f"TACC_HIER_RESOURCE {module}.{resource} {value}"
            for resource, value in resources.items()
        )
    return "\n".join(lines) + "\n"


def _timing_text(label: str) -> str:
    timing = _report(label)["timing"]["post_route"]
    return (
        "\n".join(
            (
                "Design Timing Summary",
                "WNS(ns) TNS(ns) TNS Failing Endpoints "
                "TNS Total Endpoints WHS(ns)",
                "------- ------- --------------------- "
                "------------------- -------",
                f"{timing['wns_ns']:.12f} {timing['tns_ns']:.12f} 0 1 0.0",
                "Unconstrained Paths : 0",
                "TACC_TIMING clock_period_ns 10.0",
                f"TACC_TIMING wns_ns {timing['wns_ns']}",
                f"TACC_TIMING tns_ns {timing['tns_ns']}",
                f"TACC_TIMING fmax_mhz {timing['fmax_mhz']}",
                "TACC_TIMING unconstrained_paths 0",
            )
        )
        + "\n"
    )


def _structural_text(label: str) -> str:
    return (
        "\n".join(
            f"TACC_STRUCTURAL {field} "
            f"{str(value).lower() if isinstance(value, bool) else value}"
            for field, value in _report(label)["structural"].items()
        )
        + "\n"
    )


def _write_raw_set(root: Path, label: str) -> None:
    report = _report(label)
    build = root / "build"
    build.mkdir(parents=True)
    synth_used = {
        resource: values["used"]
        for resource, values in report["resources"]["post_synth"].items()
    }
    route_used = {
        resource: values["used"]
        for resource, values in report["resources"]["post_route"].items()
    }
    (build / "utilisation.rpt").write_text(
        _utilization_text(**synth_used),
        encoding="utf-8",
    )
    (build / "utilisation_post_route.rpt").write_text(
        _utilization_text(**route_used),
        encoding="utf-8",
    )
    (build / "timing_post_route.rpt").write_text(
        _timing_text(label),
        encoding="utf-8",
    )
    (build / "utilisation_post_route_hier.rpt").write_text(
        _hierarchy_text(label),
        encoding="utf-8",
    )
    (build / "tacc_route_status.rpt").write_text(
        "TACC_ROUTE_STATUS is_route_design true\n"
        "TACC_ROUTE_STATUS status routed\n"
        "TACC_ROUTE_STATUS errors_in_routes false\n",
        encoding="utf-8",
    )
    (build / "tacc_structure.rpt").write_text(
        _structural_text(label),
        encoding="utf-8",
    )


def _file_sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _write_file_manifest(
    manifest_path: Path,
    snapshot_root: Path,
    relative_paths: tuple[str, ...],
) -> str:
    entries = []
    for relative_text in relative_paths:
        relative = Path(relative_text)
        path = snapshot_root / relative
        entries.append(
            {
                "path": relative.as_posix(),
                "mode": f"{path.stat().st_mode & 0o7777:04o}",
                "type": "file",
                "size": path.stat().st_size,
                "sha256": _file_sha256(path),
            }
        )
    manifest_path.write_text(
        json.dumps(
            {
                "schema": checker.SOURCE_MANIFEST_SCHEMA,
                "schema_version": checker.SOURCE_MANIFEST_SCHEMA_VERSION,
                "entries": entries,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="utf-8",
    )
    return _file_sha256(manifest_path)


def _raw_provenance(root: Path, label: str) -> dict:
    template = _report(label)["provenance"]
    source = root / "source"
    source_anchor = source / "rtl/source_anchor.v"
    source_anchor.parent.mkdir(parents=True, exist_ok=True)
    source_anchor.write_text("module source_anchor; endmodule\n", encoding="utf-8")
    source_manifest_sha256 = _write_file_manifest(
        root / "source_manifest.json",
        source,
        ("rtl/source_anchor.v",),
    )

    measurement_harness = root / "measurement_harness"
    constraints = measurement_harness / "fpga/constraints/genesys2.xdc"
    constraints.parent.mkdir(parents=True, exist_ok=True)
    constraints.write_text("create_clock -period 5.000 [get_ports clk]\n")
    measurement_manifest_sha256 = _write_file_manifest(
        root / "measurement_harness_manifest.json",
        measurement_harness,
        ("fpga/constraints/genesys2.xdc",),
    )
    template["source"]["manifest_sha256"] = source_manifest_sha256
    template["measurement_harness"].update(
        {
            "manifest_sha256": measurement_manifest_sha256,
            "campaign_id": _CAMPAIGN_ID,
        }
    )
    template["target"]["constraints_sha256"] = _file_sha256(constraints)
    template["reports"] = {}
    template["routed"] = False
    return template


def test_raw_parsers_create_an_attested_report_and_detect_json_tampering(
    tmp_path: Path,
) -> None:
    run_root = tmp_path / "full-tacc"
    _write_raw_set(run_root, "full-tacc")
    provenance = _raw_provenance(run_root, "full-tacc")
    report = checker.create_report_from_raw(
        label="full-tacc",
        provenance=provenance,
        build_dir=run_root / "build",
        report_root=run_root,
    )
    checker.atomic_write_json(run_root / "tacc_report.json", report)

    loaded = checker.load_report(run_root)
    assert loaded["hierarchy"] == {"mp64_tile": 7, "mp64_tacc": 7}
    assert loaded["route_status"]["is_route_design"] is True

    tampered = copy.deepcopy(report)
    tampered["resources"]["post_synth"]["lut"]["used"] += 1.0
    checker.atomic_write_json(run_root / "tacc_report.json", tampered)
    with pytest.raises(checker.ReportError, match="does not match"):
        checker.load_report(run_root)

    bad_target_hash = copy.deepcopy(report)
    bad_target_hash["provenance"]["target"]["constraints_sha256"] = "0" * 64
    checker.atomic_write_json(run_root / "tacc_report.json", bad_target_hash)
    with pytest.raises(
        checker.ReportError,
        match="target constraints SHA-256 mismatch",
    ):
        checker.load_report(run_root)

    checker.atomic_write_json(run_root / "tacc_report.json", report)
    (
        run_root
        / "measurement_harness/fpga/constraints/genesys2.xdc"
    ).write_text(
        "create_clock -period 6.000 [get_ports clk]\n",
        encoding="utf-8",
    )
    with pytest.raises(
        checker.ReportError,
        match="attested measurement harness SHA-256",
    ):
        checker.load_report(run_root)


def test_attested_raw_hash_and_exact_report_discovery_fail_closed(
    tmp_path: Path,
) -> None:
    run_root = tmp_path / "topology-only"
    _write_raw_set(run_root, "topology-only")
    provenance = _raw_provenance(run_root, "topology-only")
    report = checker.create_report_from_raw(
        label="topology-only",
        provenance=provenance,
        build_dir=run_root / "build",
        report_root=run_root,
    )
    checker.atomic_write_json(run_root / "tacc_report.json", report)
    (run_root / "build/utilisation.rpt").write_text(
        "tampered\n",
        encoding="utf-8",
    )
    with pytest.raises(checker.ReportError, match="SHA-256 mismatch"):
        checker.load_report(run_root)

    nested_only = tmp_path / "nested-only"
    (nested_only / "child").mkdir(parents=True)
    (nested_only / "child/tacc_report.json").write_text("{}\n")
    with pytest.raises(checker.ReportError, match="missing required report"):
        checker.discover_report(nested_only)


def test_attested_source_and_harness_reject_unmanifested_files(
    tmp_path: Path,
) -> None:
    run_root = tmp_path / "full-tacc"
    _write_raw_set(run_root, "full-tacc")
    provenance = _raw_provenance(run_root, "full-tacc")
    report = checker.create_report_from_raw(
        label="full-tacc",
        provenance=provenance,
        build_dir=run_root / "build",
        report_root=run_root,
    )
    checker.atomic_write_json(run_root / "tacc_report.json", report)
    checker.load_report(run_root)

    source_extra = run_root / "source/rtl/injected.v"
    source_extra.write_text("module injected; endmodule\n", encoding="utf-8")
    with pytest.raises(checker.ReportError, match="unexpected rtl/injected.v"):
        checker.load_report(run_root)
    source_extra.unlink()

    harness_extra = run_root / "measurement_harness/injected.tcl"
    harness_extra.write_text("puts injected\n", encoding="utf-8")
    with pytest.raises(checker.ReportError, match="unexpected injected.tcl"):
        checker.load_report(run_root)


def test_missing_canonical_raw_file_is_not_guessed(tmp_path: Path) -> None:
    build = tmp_path / "build"
    build.mkdir()
    for filename in checker.RAW_REPORT_FILES.values():
        if filename != "timing_post_route.rpt":
            (build / filename).write_text("\n")
    (build / "some_other_timing_post_route.rpt").write_text("\n")

    with pytest.raises(checker.ReportError, match="timing_post_route.rpt"):
        checker.discover_raw_reports(build)


def test_timing_markers_must_agree_with_native_vivado_evidence() -> None:
    report_timing = _report("full-tacc")["timing"]["post_route"]
    native_and_markers = _timing_text("full-tacc")

    marker_only = "\n".join(
        line
        for line in native_and_markers.splitlines()
        if line.startswith("TACC_TIMING ")
    )
    with pytest.raises(checker.ReportError, match="native WNS/TNS"):
        checker.parse_vivado_timing(marker_only)

    contradictory_wns = native_and_markers.replace(
        f"TACC_TIMING wns_ns {report_timing['wns_ns']}",
        "TACC_TIMING wns_ns -1.0",
    )
    with pytest.raises(checker.ReportError, match="wns_ns=.*disagrees"):
        checker.parse_vivado_timing(contradictory_wns)

    arbitrary_fmax = native_and_markers.replace(
        f"TACC_TIMING fmax_mhz {report_timing['fmax_mhz']}",
        "TACC_TIMING fmax_mhz 999.0",
    )
    with pytest.raises(checker.ReportError, match="fmax_mhz=.*disagrees"):
        checker.parse_vivado_timing(arbitrary_fmax)

    without_fmax_marker = "\n".join(
        line
        for line in native_and_markers.splitlines()
        if not line.startswith("TACC_TIMING fmax_mhz ")
    )
    parsed = checker.parse_vivado_timing(without_fmax_marker)
    assert parsed["fmax_mhz"] == pytest.approx(report_timing["fmax_mhz"])

    wrong_clock = native_and_markers.replace(
        "TACC_TIMING clock_period_ns 10.0",
        "TACC_TIMING clock_period_ns 12.0",
    )
    with pytest.raises(checker.ReportError, match="does not match"):
        checker.parse_vivado_timing(wrong_clock)

    missing_clock = "\n".join(
        line
        for line in native_and_markers.splitlines()
        if not line.startswith("TACC_TIMING clock_period_ns ")
    )
    with pytest.raises(checker.ReportError, match="clock_period_ns"):
        checker.parse_vivado_timing(missing_clock)


def test_hierarchy_markers_cannot_replace_or_contradict_native_rows() -> None:
    marker_only = "\n".join(
        (
            "TACC_HIERARCHY mp64_tile 7",
            "TACC_HIERARCHY mp64_tacc 7",
        )
    )
    with pytest.raises(checker.ReportError, match="native module rows"):
        checker.parse_vivado_hierarchy(marker_only)

    contradictory = _hierarchy_text("full-tacc").replace(
        "TACC_HIERARCHY mp64_tacc 7",
        "TACC_HIERARCHY mp64_tacc 6",
    )
    with pytest.raises(checker.ReportError, match="disagrees"):
        checker.parse_vivado_hierarchy(contradictory)


def test_cli_rejects_output_collision_with_an_input_report(
    tmp_path: Path,
    capsys: pytest.CaptureFixture[str],
) -> None:
    input_paths = []
    for label in ("current-main", "topology-only", "full-tacc"):
        run_root = tmp_path / label
        run_root.mkdir()
        report_path = run_root / "tacc_report.json"
        report_path.write_text("{}\n", encoding="utf-8")
        input_paths.append(report_path)

    assert (
        checker.main(
            [
                "--current-main",
                str(input_paths[0]),
                "--topology-only",
                str(input_paths[1]),
                "--full-tacc",
                str(input_paths[2]),
                "--expected-full-commit",
                _FULL_COMMIT,
                "--expected-full-manifest-sha256",
                _FULL_MANIFEST_SHA256,
                "--output",
                str(input_paths[2].parent / "build/utilisation.rpt"),
            ]
        )
        == 2
    )
    assert input_paths[2].read_text(encoding="utf-8") == "{}\n"
    assert "outside every input report package" in capsys.readouterr().err


def test_nonfinite_json_and_boolean_schema_version_are_rejected() -> None:
    report = _report("full-tacc")
    report["resources"]["post_synth"]["lut"]["used"] = float("nan")
    with pytest.raises(checker.ReportError, match="non-finite"):
        checker.validate_report(report)

    report = _report("full-tacc")
    report["schema_version"] = True
    with pytest.raises(checker.ReportError, match="schema_version"):
        checker.validate_report(report)


def test_runner_defaults_to_prepare_only_without_calling_heavy_tool(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    prepared = {
        "output": tmp_path / "prepared",
        "source": tmp_path / "prepared/source",
        "provenance": {
            "source": {
                "commit": "d" * 40,
                "manifest_sha256": "e" * 64,
            },
            "measurement_harness": {
                "manifest_sha256": "f" * 64,
            },
            "strategy": {
                "memory_configuration": (
                    "effective_mem_depth=unresolved;"
                    "fixed_banks=4;row_bits=512;"
                    "address_contract=fixed-16384"
                ),
            },
        },
        "flow": {},
    }
    monkeypatch.setattr(runner, "_prepare_output", lambda **_kwargs: prepared)

    def forbidden_heavy_tool(*_args: object, **_kwargs: object) -> None:
        raise AssertionError("default mode launched the heavyweight tool")

    monkeypatch.setattr(runner, "_run_vivado", forbidden_heavy_tool)

    assert (
        runner.main(
            [
                "--source-ref",
                "HEAD",
                "--label",
                "full-tacc",
                "--campaign-id",
                "test-campaign",
                "--out",
                str(tmp_path / "prepared"),
            ]
        )
        == 0
    )


def _git(repository: Path, *arguments: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(repository), *arguments],
        check=True,
        stdout=subprocess.PIPE,
    ).stdout


def _minimal_synthesis_repository(root: Path) -> None:
    (root / "fpga/constraints").mkdir(parents=True)
    (root / "rtl/soc").mkdir(parents=True)
    (root / "rtl/mem").mkdir(parents=True)
    (root / "rtl/target/xilinx7").mkdir(parents=True)
    (root / "fpga/synth_genesys2.tcl").write_text(
        "\n".join(
            (
                "create_project -in_memory -part xc7k325tffg900-2",
                "read_xdc ${CONST_DIR}/genesys2.xdc",
                "synth_design \\",
                "  -top mp64_synth_top \\",
                "  -part xc7k325tffg900-2 \\",
                "  -flatten_hierarchy rebuilt \\",
                "  -directive AreaOptimized_high \\",
                "  -retiming on \\",
                "  -verilog_define SIMULATION=0",
            )
        )
        + "\n",
        encoding="utf-8",
    )
    (root / "fpga/constraints/genesys2.xdc").write_text(
        "create_clock -period 5.000 [get_ports clk]\n",
        encoding="utf-8",
    )
    (root / "rtl/soc/mp64_soc.v").write_text(
        "module mp64_soc #("
        "parameter MEM_DEPTH = 4096, parameter UNUSED = 0"
        ")(); endmodule\n",
        encoding="utf-8",
    )
    (root / "rtl/mem/mp64_memory.v").write_text(
        "module mp64_memory #("
        "parameter BANK_DEPTH = 4096, "
        "parameter ADDR_W_TILE = 14, "
        "parameter ADDR_W_CPU = 17"
        ")(); endmodule\n",
        encoding="utf-8",
    )
    (root / "rtl/target/xilinx7/mp64_synth_top.v").write_text(
        "module mp64_synth_top; mp64_soc u_soc(); endmodule\n",
        encoding="utf-8",
    )
    (root / "tracked.txt").write_text("tracked\n", encoding="utf-8")
    _git(root, "init", "-q")
    _git(root, "config", "user.name", "TACC Test")
    _git(root, "config", "user.email", "tacc-test@example.invalid")
    _git(root, "add", ".")
    _git(root, "commit", "-q", "-m", "Synthetic source")


def test_ref_and_tree_preparation_are_isolated_and_preserve_source(
    tmp_path: Path,
) -> None:
    repository = tmp_path / "source-repository"
    repository.mkdir()
    _minimal_synthesis_repository(repository)
    clean_status = _git(repository, "status", "--porcelain=v1", "-z")

    ref_output = tmp_path / "ref-output"
    assert (
        runner.main(
            [
                "--repo",
                str(repository),
                "--source-ref",
                "HEAD",
                "--label",
                "synthetic-ref",
                "--campaign-id",
                "synthetic-campaign",
                "--out",
                str(ref_output),
            ]
        )
        == 0
    )
    assert (ref_output / "source/tracked.txt").read_text() == "tracked\n"
    assert _git(repository, "status", "--porcelain=v1", "-z") == clean_status

    (repository / "untracked.txt").write_text("tree-only\n", encoding="utf-8")
    dirty_status = _git(
        repository,
        "status",
        "--porcelain=v1",
        "-z",
        "--untracked-files=all",
    )
    tree_output = tmp_path / "tree-output"
    assert (
        runner.main(
            [
                "--source-tree",
                str(repository),
                "--label",
                "synthetic-tree",
                "--campaign-id",
                "synthetic-campaign",
                "--out",
                str(tree_output),
            ]
        )
        == 0
    )
    assert (tree_output / "source/untracked.txt").read_text() == "tree-only\n"
    assert (
        _git(
            repository,
            "status",
            "--porcelain=v1",
            "-z",
            "--untracked-files=all",
        )
        == dirty_status
    )


def test_heavy_flow_rejects_known_overcapacity_memory_before_vivado() -> None:
    flow = {
        "tcl_text": "\n".join(
            (
                "opt_design",
                "place_design",
                "phys_opt_design",
                "route_design",
                *CANONICAL_REPORT_LINES,
                *runner.REQUIRED_REPORT_MARKERS,
            )
        ),
        "strategy": {
            "memory_configuration": (
                "effective_mem_depth=16384;fixed_banks=4;row_bits=512"
                ";address_contract=fixed-16384"
            )
        },
    }
    with pytest.raises(runner.RunnerError, match="at least 1024 RAMB36"):
        runner._validate_heavy_flow(flow)


def test_heavy_flow_rejects_reduced_depth_with_fixed_addresses() -> None:
    flow = {
        "tcl_text": "\n".join(
            (
                "opt_design",
                "place_design",
                "phys_opt_design",
                "route_design",
                *CANONICAL_REPORT_LINES,
                *runner.REQUIRED_REPORT_MARKERS,
            )
        ),
        "strategy": {
            "memory_configuration": (
                "effective_mem_depth=4096;fixed_banks=4;row_bits=512;"
                "address_contract=fixed-16384"
            )
        },
    }
    with pytest.raises(runner.RunnerError, match="fixed 14/17-bit"):
        runner._validate_heavy_flow(flow)


CANONICAL_REPORT_LINES = tuple(
    f"puts {filename}" for filename in runner.CANONICAL_RAW_REPORTS
)
