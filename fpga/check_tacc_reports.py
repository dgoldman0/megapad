#!/usr/bin/env python3
"""Parse and compare the three physical TACC acceptance reports.

The checker deliberately consumes a small, versioned JSON report rather than
trying to infer acceptance from a Vivado log.  ``run_tacc_impl.py`` creates
that report after discovering and parsing the required post-synthesis and
post-route reports.  Directories passed on the command line must contain
exactly ``tacc_report.json`` at their root.

This module also owns the narrowly scoped text parsers used by the runner.
Missing rows, ambiguous files, absent timing fields, and unknown hierarchy
counts are errors: physical acceptance must fail closed.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import math
import os
from pathlib import Path, PurePosixPath
import posixpath
import re
import stat
import sys
import tempfile
from typing import Any, Mapping, Sequence


REPORT_SCHEMA = "megapad.tacc.physical-report"
REPORT_SCHEMA_VERSION = 1
CHECK_SCHEMA = "megapad.tacc.physical-acceptance"
CHECK_SCHEMA_VERSION = 1
SOURCE_MANIFEST_SCHEMA = "megapad.tacc.source-manifest"
SOURCE_MANIFEST_SCHEMA_VERSION = 1

CURRENT_MAIN_SHA = "c8e8118e82a899ec3f101f63d277a1bf4ef5f84a"
TOPOLOGY_ONLY_SHA = "364d44283ba5c2fad8187b63da6917af60344c26"
CURRENT_MAIN_MANIFEST_SHA256 = (
    "064cdb7f06c88afa9107887b084ad19796cb9d65459410790e89e0c4706c95eb"
)
TOPOLOGY_ONLY_MANIFEST_SHA256 = (
    "87601b49375ce86be7218d8f10cf75611e97df902cb45f18a4232516f3e54e09"
)

RESOURCE_NAMES = ("lut", "ff", "bram", "dsp")

MAX_DSP_GROWTH_PERCENT = 5.0
MAX_FF_GROWTH_PERCENT = 7.0
MAX_LUT_GROWTH_PERCENT = 12.0
MIN_HEADROOM_PERCENT = 5.0
MAX_FMAX_REGRESSION_PERCENT = 10.0
TARGET_CLOCK_MHZ = 100.0
MAX_FP_FEEDBACK_LANES_PER_ENGINE = 16
PERSISTENT_TACC_BITS = 7 * 2048
SHARED_TACC_STAGE_BITS = 2048

RAW_REPORT_FILES = {
    "post_synth_utilization": "utilisation.rpt",
    "post_route_utilization": "utilisation_post_route.rpt",
    "post_route_timing": "timing_post_route.rpt",
    "post_route_hierarchy": "utilisation_post_route_hier.rpt",
    "route_status": "tacc_route_status.rpt",
    "structural": "tacc_structure.rpt",
}

_STRATEGY_FIELDS = (
    "top",
    "synth_flatten_hierarchy",
    "synth_directive",
    "synth_retiming",
    "synth_verilog_define",
    "opt_directive",
    "place_directive",
    "phys_opt_directive",
    "route_directive",
    "memory_configuration",
)


class ReportError(ValueError):
    """A report is missing, malformed, ambiguous, or incomparable."""


def _ensure_strict_json(value: Any, context: str = "report") -> None:
    if value is None or isinstance(value, (str, bool, int)):
        return
    if isinstance(value, float):
        if not math.isfinite(value):
            raise ReportError(f"{context} contains a non-finite number")
        return
    if isinstance(value, Mapping):
        for key, nested in value.items():
            if not isinstance(key, str):
                raise ReportError(f"{context} contains a non-string object key")
            _ensure_strict_json(nested, f"{context}.{key}")
        return
    if isinstance(value, list):
        for index, nested in enumerate(value):
            _ensure_strict_json(nested, f"{context}[{index}]")
        return
    raise ReportError(f"{context} contains a non-JSON value")


def _json_object(value: Any, context: str) -> Mapping[str, Any]:
    if not isinstance(value, Mapping):
        raise ReportError(f"{context} must be a JSON object")
    return value


def _required(mapping: Mapping[str, Any], key: str, context: str) -> Any:
    if key not in mapping:
        raise ReportError(f"{context} is missing {key!r}")
    return mapping[key]


def _finite_number(value: Any, context: str, *, minimum: float | None = None) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ReportError(f"{context} must be a finite number")
    result = float(value)
    if not math.isfinite(result):
        raise ReportError(f"{context} must be a finite number")
    if minimum is not None and result < minimum:
        raise ReportError(f"{context} must be at least {minimum}")
    return result


def _integer(value: Any, context: str, *, minimum: int = 0) -> int:
    if isinstance(value, bool) or not isinstance(value, int):
        raise ReportError(f"{context} must be an integer")
    if value < minimum:
        raise ReportError(f"{context} must be at least {minimum}")
    return value


def _boolean(value: Any, context: str) -> bool:
    if not isinstance(value, bool):
        raise ReportError(f"{context} must be a boolean")
    return value


def _nonempty_string(value: Any, context: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ReportError(f"{context} must be a nonempty string")
    return value


def _sha256_string(value: Any, context: str) -> str:
    result = _nonempty_string(value, context).lower()
    if not re.fullmatch(r"[0-9a-f]{64}", result):
        raise ReportError(f"{context} must be a 64-digit SHA-256")
    return result


def _commit_string(value: Any, context: str) -> str:
    result = _nonempty_string(value, context).lower()
    if not re.fullmatch(r"[0-9a-f]{40}", result):
        raise ReportError(f"{context} must be a full 40-digit commit SHA")
    return result


def _safe_relative_path(value: Any, context: str) -> Path:
    text = _nonempty_string(value, context)
    path = Path(text)
    if path.is_absolute() or any(part in {"", ".", ".."} for part in path.parts):
        raise ReportError(f"{context} must be a normalized relative path")
    return path


def _resource_stage(report: Mapping[str, Any], stage: str) -> Mapping[str, Any]:
    resources = _json_object(
        _required(report, "resources", "report"),
        "report.resources",
    )
    stage_data = _json_object(
        _required(resources, stage, "report.resources"),
        f"report.resources.{stage}",
    )
    if set(stage_data) != set(RESOURCE_NAMES):
        raise ReportError(
            f"report.resources.{stage} must contain exactly "
            + ", ".join(RESOURCE_NAMES)
        )
    for resource in RESOURCE_NAMES:
        row = _json_object(
            _required(stage_data, resource, f"report.resources.{stage}"),
            f"report.resources.{stage}.{resource}",
        )
        used = _finite_number(
            _required(row, "used", f"report.resources.{stage}.{resource}"),
            f"report.resources.{stage}.{resource}.used",
            minimum=0.0,
        )
        available = _finite_number(
            _required(row, "available", f"report.resources.{stage}.{resource}"),
            f"report.resources.{stage}.{resource}.available",
            minimum=0.0,
        )
        if available <= 0.0:
            raise ReportError(
                f"report.resources.{stage}.{resource}.available must be positive"
            )
        if used > available:
            # Keep the report parseable so the acceptance result can name the
            # over-capacity resource instead of disguising it as bad JSON.
            continue
    return stage_data


def validate_report(report: Mapping[str, Any]) -> None:
    """Validate the complete versioned report schema."""

    _ensure_strict_json(report)
    if _required(report, "schema", "report") != REPORT_SCHEMA:
        raise ReportError(f"report.schema must be {REPORT_SCHEMA!r}")
    schema_version = _required(report, "schema_version", "report")
    if (
        type(schema_version) is not int
        or schema_version != REPORT_SCHEMA_VERSION
    ):
        raise ReportError(
            f"report.schema_version must be {REPORT_SCHEMA_VERSION}"
        )
    _nonempty_string(_required(report, "label", "report"), "report.label")

    provenance = _json_object(
        _required(report, "provenance", "report"),
        "report.provenance",
    )
    source = _json_object(
        _required(provenance, "source", "report.provenance"),
        "report.provenance.source",
    )
    source_kind = _nonempty_string(
        _required(source, "kind", "report.provenance.source"),
        "report.provenance.source.kind",
    )
    if source_kind not in {"ref", "tree"}:
        raise ReportError("report.provenance.source.kind must be 'ref' or 'tree'")
    _nonempty_string(
        _required(source, "requested", "report.provenance.source"),
        "report.provenance.source.requested",
    )
    _commit_string(
        _required(source, "commit", "report.provenance.source"),
        "report.provenance.source.commit",
    )
    _sha256_string(
        _required(source, "manifest_sha256", "report.provenance.source"),
        "report.provenance.source.manifest_sha256",
    )
    _safe_relative_path(
        _required(source, "manifest_path", "report.provenance.source"),
        "report.provenance.source.manifest_path",
    )
    _safe_relative_path(
        _required(source, "root", "report.provenance.source"),
        "report.provenance.source.root",
    )
    _boolean(
        _required(source, "dirty", "report.provenance.source"),
        "report.provenance.source.dirty",
    )

    measurement_harness = _json_object(
        _required(provenance, "measurement_harness", "report.provenance"),
        "report.provenance.measurement_harness",
    )
    _safe_relative_path(
        _required(
            measurement_harness,
            "manifest_path",
            "report.provenance.measurement_harness",
        ),
        "report.provenance.measurement_harness.manifest_path",
    )
    _sha256_string(
        _required(
            measurement_harness,
            "manifest_sha256",
            "report.provenance.measurement_harness",
        ),
        "report.provenance.measurement_harness.manifest_sha256",
    )
    _safe_relative_path(
        _required(
            measurement_harness,
            "root",
            "report.provenance.measurement_harness",
        ),
        "report.provenance.measurement_harness.root",
    )
    _nonempty_string(
        _required(
            measurement_harness,
            "campaign_id",
            "report.provenance.measurement_harness",
        ),
        "report.provenance.measurement_harness.campaign_id",
    )

    tool = _json_object(
        _required(provenance, "tool", "report.provenance"),
        "report.provenance.tool",
    )
    _nonempty_string(
        _required(tool, "name", "report.provenance.tool"),
        "report.provenance.tool.name",
    )
    _nonempty_string(
        _required(tool, "version", "report.provenance.tool"),
        "report.provenance.tool.version",
    )

    target = _json_object(
        _required(provenance, "target", "report.provenance"),
        "report.provenance.target",
    )
    _nonempty_string(
        _required(target, "part", "report.provenance.target"),
        "report.provenance.target.part",
    )
    _finite_number(
        _required(target, "clock_mhz", "report.provenance.target"),
        "report.provenance.target.clock_mhz",
        minimum=0.0,
    )
    _sha256_string(
        _required(target, "constraints_sha256", "report.provenance.target"),
        "report.provenance.target.constraints_sha256",
    )
    _safe_relative_path(
        _required(target, "constraints_path", "report.provenance.target"),
        "report.provenance.target.constraints_path",
    )
    strategy = _json_object(
        _required(provenance, "strategy", "report.provenance"),
        "report.provenance.strategy",
    )
    if set(strategy) != set(_STRATEGY_FIELDS):
        missing = sorted(set(_STRATEGY_FIELDS) - set(strategy))
        extra = sorted(set(strategy) - set(_STRATEGY_FIELDS))
        details = []
        if missing:
            details.append("missing " + ", ".join(missing))
        if extra:
            details.append("unexpected " + ", ".join(extra))
        raise ReportError(
            "report.provenance.strategy has the wrong fields ("
            + "; ".join(details)
            + ")"
        )
    for field in _STRATEGY_FIELDS:
        _nonempty_string(
            strategy[field],
            f"report.provenance.strategy.{field}",
        )
    _boolean(
        _required(provenance, "routed", "report.provenance"),
        "report.provenance.routed",
    )
    report_files = _json_object(
        _required(provenance, "reports", "report.provenance"),
        "report.provenance.reports",
    )
    if not report_files:
        raise ReportError("report.provenance.reports must not be empty")
    if set(report_files) != set(RAW_REPORT_FILES):
        raise ReportError(
            "report.provenance.reports must contain exactly "
            + ", ".join(sorted(RAW_REPORT_FILES))
        )
    for name, record_value in report_files.items():
        _nonempty_string(name, "report.provenance.reports key")
        record = _json_object(
            record_value,
            f"report.provenance.reports.{name}",
        )
        report_path = _safe_relative_path(
            _required(record, "path", f"report.provenance.reports.{name}"),
            f"report.provenance.reports.{name}.path",
        )
        if report_path.name != RAW_REPORT_FILES[name]:
            raise ReportError(
                f"report.provenance.reports.{name}.path must end in "
                f"{RAW_REPORT_FILES[name]!r}"
            )
        _sha256_string(
            _required(record, "sha256", f"report.provenance.reports.{name}"),
            f"report.provenance.reports.{name}.sha256",
        )

    _resource_stage(report, "post_synth")
    _resource_stage(report, "post_route")

    timing = _json_object(
        _required(report, "timing", "report"),
        "report.timing",
    )
    post_route_timing = _json_object(
        _required(timing, "post_route", "report.timing"),
        "report.timing.post_route",
    )
    _finite_number(
        _required(post_route_timing, "wns_ns", "report.timing.post_route"),
        "report.timing.post_route.wns_ns",
    )
    _finite_number(
        _required(post_route_timing, "tns_ns", "report.timing.post_route"),
        "report.timing.post_route.tns_ns",
    )
    _finite_number(
        _required(post_route_timing, "fmax_mhz", "report.timing.post_route"),
        "report.timing.post_route.fmax_mhz",
        minimum=0.0,
    )
    _finite_number(
        _required(post_route_timing, "target_mhz", "report.timing.post_route"),
        "report.timing.post_route.target_mhz",
        minimum=0.0,
    )
    _integer(
        _required(
            post_route_timing,
            "unconstrained_paths",
            "report.timing.post_route",
        ),
        "report.timing.post_route.unconstrained_paths",
    )

    hierarchy = _json_object(
        _required(report, "hierarchy", "report"),
        "report.hierarchy",
    )
    for module in ("mp64_tile", "mp64_tacc"):
        _integer(
            _required(hierarchy, module, "report.hierarchy"),
            f"report.hierarchy.{module}",
        )

    hierarchical_resources = _json_object(
        _required(report, "hierarchical_resources", "report"),
        "report.hierarchical_resources",
    )
    if set(hierarchical_resources) != {"mp64_tile", "mp64_tacc"}:
        raise ReportError(
            "report.hierarchical_resources must contain exactly "
            "mp64_tile and mp64_tacc"
        )
    for module in ("mp64_tile", "mp64_tacc"):
        module_resources = _json_object(
            hierarchical_resources[module],
            f"report.hierarchical_resources.{module}",
        )
        if set(module_resources) != set(RESOURCE_NAMES):
            raise ReportError(
                f"report.hierarchical_resources.{module} must contain "
                + ", ".join(RESOURCE_NAMES)
            )
        for resource in RESOURCE_NAMES:
            _finite_number(
                module_resources[resource],
                f"report.hierarchical_resources.{module}.{resource}",
                minimum=0.0,
            )

    route_status = _json_object(
        _required(report, "route_status", "report"),
        "report.route_status",
    )
    _boolean(
        _required(route_status, "is_route_design", "report.route_status"),
        "report.route_status.is_route_design",
    )
    _nonempty_string(
        _required(route_status, "status", "report.route_status"),
        "report.route_status.status",
    )
    _boolean(
        _required(route_status, "errors_in_routes", "report.route_status"),
        "report.route_status.errors_in_routes",
    )

    structural = _json_object(
        _required(report, "structural", "report"),
        "report.structural",
    )
    for field in (
        "tacc_specific_multiplier_arrays",
        "max_fp_feedback_lanes_per_engine",
        "persistent_tacc_bits",
        "shared_tacc_stage_bits",
        "tacc_bram_cells",
    ):
        _integer(
            _required(structural, field, "report.structural"),
            f"report.structural.{field}",
        )
    for field in (
        "multiplier_sharing_verified",
        "fp_adder_sharing_verified",
        "bounded_feedback_path_verified",
    ):
        _boolean(
            _required(structural, field, "report.structural"),
            f"report.structural.{field}",
        )


def _verified_attestation_path(
    report_path: Path,
    relative_path: Any,
    expected_sha256: Any,
    context: str,
) -> Path:
    relative = _safe_relative_path(relative_path, f"{context}.path")
    root = report_path.parent.resolve()
    candidate = (root / relative).resolve()
    try:
        candidate.relative_to(root)
    except ValueError as exc:
        raise ReportError(f"{context}.path escapes the report directory") from exc
    if not candidate.is_file():
        raise ReportError(f"{context} is missing: {candidate}")
    expected = _sha256_string(expected_sha256, f"{context}.sha256")
    try:
        observed = _sha256_file(candidate)
    except OSError as exc:
        raise ReportError(f"cannot hash {context} {candidate}: {exc}") from exc
    if observed != expected:
        raise ReportError(
            f"{context} SHA-256 mismatch: expected {expected}, got {observed}"
        )
    return candidate


def _read_strict_json(path: Path, context: str) -> Any:
    def reject_constant(token: str) -> None:
        raise ReportError(f"{context} contains non-standard constant {token}")

    try:
        return json.loads(
            path.read_text(encoding="utf-8"),
            parse_constant=reject_constant,
        )
    except (OSError, UnicodeError) as exc:
        raise ReportError(f"cannot read {context} {path}: {exc}") from exc
    except json.JSONDecodeError as exc:
        raise ReportError(f"{context} {path} is not valid JSON: {exc}") from exc


def _validate_manifest_symlink(
    path: Path,
    target: str,
    snapshot_name: str,
) -> None:
    pure_target = PurePosixPath(target)
    if pure_target.is_absolute():
        raise ReportError(
            f"{snapshot_name} manifest symlink {path} has an absolute target"
        )
    normalized = posixpath.normpath(
        posixpath.join(path.parent.as_posix(), target)
    )
    if normalized == ".." or normalized.startswith("../"):
        raise ReportError(
            f"{snapshot_name} manifest symlink {path} escapes "
            f"{snapshot_name} root"
        )


def _verify_manifest_snapshot(
    report_root: str | os.PathLike[str],
    snapshot: Mapping[str, Any],
    *,
    snapshot_name: str,
    provenance_context: str,
) -> None:
    """Verify every manifest entry against one isolated attested tree."""

    root = Path(report_root).expanduser().resolve()
    manifest_context = f"{snapshot_name} manifest"
    manifest_path = _verified_attestation_path(
        root / "tacc_report.json",
        snapshot["manifest_path"],
        snapshot["manifest_sha256"],
        manifest_context,
    )
    manifest = _json_object(
        _read_strict_json(manifest_path, manifest_context),
        manifest_context,
    )
    _ensure_strict_json(manifest, manifest_context)
    if manifest.get("schema") != SOURCE_MANIFEST_SCHEMA:
        raise ReportError(
            f"{manifest_context} schema must be {SOURCE_MANIFEST_SCHEMA!r}"
        )
    version = manifest.get("schema_version")
    if (
        type(version) is not int
        or version != SOURCE_MANIFEST_SCHEMA_VERSION
    ):
        raise ReportError(
            f"{manifest_context} schema_version must be "
            f"{SOURCE_MANIFEST_SCHEMA_VERSION}"
        )
    entries = manifest.get("entries")
    if not isinstance(entries, list) or not entries:
        raise ReportError(
            f"{manifest_context} entries must be a nonempty list"
        )

    snapshot_relative = _safe_relative_path(
        snapshot["root"],
        f"{provenance_context}.root",
    )
    snapshot_root = (root / snapshot_relative).resolve()
    try:
        snapshot_root.relative_to(root)
    except ValueError as exc:
        raise ReportError(
            f"isolated {snapshot_name} root escapes the report directory"
        ) from exc
    if not snapshot_root.is_dir():
        raise ReportError(
            f"isolated {snapshot_name} root is missing: {snapshot_root}"
        )

    seen: set[Path] = set()
    for index, raw_entry in enumerate(entries):
        context = f"{snapshot_name} manifest entry {index}"
        entry = _json_object(raw_entry, context)
        relative = _safe_relative_path(
            _required(entry, "path", context),
            f"{context}.path",
        )
        if relative in seen:
            raise ReportError(
                f"{manifest_context} repeats path {relative.as_posix()!r}"
            )
        seen.add(relative)
        entry_type = _nonempty_string(
            _required(entry, "type", context),
            f"{context}.type",
        )
        mode_text = _nonempty_string(
            _required(entry, "mode", context),
            f"{context}.mode",
        )
        if not re.fullmatch(r"[0-7]{4}", mode_text):
            raise ReportError(f"{context}.mode must be four octal digits")

        candidate = snapshot_root / relative
        resolved_parent = candidate.parent.resolve()
        try:
            resolved_parent.relative_to(snapshot_root)
        except ValueError as exc:
            raise ReportError(
                f"{manifest_context} path {relative} escapes through a symlink"
            ) from exc
        try:
            metadata = candidate.lstat()
        except OSError as exc:
            raise ReportError(
                f"attested {snapshot_name} path is missing: {candidate}"
            ) from exc
        observed_mode = stat.S_IMODE(metadata.st_mode)
        if observed_mode != int(mode_text, 8):
            raise ReportError(
                f"attested {snapshot_name} mode changed for {relative}: "
                f"{observed_mode:04o} != {mode_text}"
            )

        if entry_type == "file":
            if set(entry) != {"path", "mode", "type", "size", "sha256"}:
                raise ReportError(f"{context} has the wrong regular-file fields")
            if not stat.S_ISREG(metadata.st_mode):
                raise ReportError(
                    f"attested {snapshot_name} file changed type: {relative}"
                )
            expected_size = _integer(
                entry["size"],
                f"{context}.size",
            )
            if metadata.st_size != expected_size:
                raise ReportError(
                    f"attested {snapshot_name} size changed for {relative}"
                )
            expected_sha = _sha256_string(
                entry["sha256"],
                f"{context}.sha256",
            )
            try:
                observed_sha = _sha256_file(candidate)
            except OSError as exc:
                raise ReportError(
                    f"cannot hash attested {snapshot_name} file "
                    f"{candidate}: {exc}"
                ) from exc
            if observed_sha != expected_sha:
                raise ReportError(
                    f"attested {snapshot_name} SHA-256 changed for {relative}"
                )
        elif entry_type == "symlink":
            if set(entry) != {"path", "mode", "type", "target"}:
                raise ReportError(f"{context} has the wrong symlink fields")
            if not stat.S_ISLNK(metadata.st_mode):
                raise ReportError(
                    f"attested {snapshot_name} symlink changed type: {relative}"
                )
            expected_target = _nonempty_string(
                entry["target"],
                f"{context}.target",
            )
            _validate_manifest_symlink(
                relative,
                expected_target,
                snapshot_name,
            )
            resolved_target = (candidate.parent / expected_target).resolve()
            try:
                resolved_target.relative_to(snapshot_root)
            except ValueError as exc:
                raise ReportError(
                    f"attested {snapshot_name} symlink target escapes: {relative}"
                ) from exc
            if os.readlink(candidate) != expected_target:
                raise ReportError(
                    f"attested {snapshot_name} symlink target changed "
                    f"for {relative}"
                )
        else:
            raise ReportError(f"{context}.type must be 'file' or 'symlink'")

    observed: set[Path] = set()
    for directory, directory_names, file_names in os.walk(
        snapshot_root,
        topdown=True,
        followlinks=False,
    ):
        directory_path = Path(directory)
        relative_directory = directory_path.relative_to(snapshot_root)
        retained_directories: list[str] = []
        for name in directory_names:
            candidate = directory_path / name
            relative = relative_directory / name
            if candidate.is_symlink():
                observed.add(relative)
            else:
                retained_directories.append(name)
        directory_names[:] = retained_directories
        observed.update(relative_directory / name for name in file_names)

    if observed != seen:
        unexpected = sorted(
            (path.as_posix() for path in observed - seen)
        )
        missing = sorted((path.as_posix() for path in seen - observed))
        details = []
        if unexpected:
            details.append("unexpected " + ", ".join(unexpected))
        if missing:
            details.append("missing " + ", ".join(missing))
        raise ReportError(
            f"attested {snapshot_name} tree differs from its manifest ("
            + "; ".join(details)
            + ")"
        )


def verify_source_snapshot(
    report_root: str | os.PathLike[str],
    source: Mapping[str, Any],
) -> None:
    """Verify every attested source-manifest entry against the isolated tree."""

    _verify_manifest_snapshot(
        report_root,
        source,
        snapshot_name="source",
        provenance_context="report.provenance.source",
    )


def verify_measurement_harness(
    report_root: str | os.PathLike[str],
    measurement_harness: Mapping[str, Any],
) -> None:
    """Verify the immutable measurement files used for one physical run."""

    _verify_manifest_snapshot(
        report_root,
        measurement_harness,
        snapshot_name="measurement harness",
        provenance_context="report.provenance.measurement_harness",
    )


def _verify_provenance_files(path: Path, report: Mapping[str, Any]) -> None:
    provenance = report["provenance"]
    source = provenance["source"]
    verify_source_snapshot(path.parent, source)
    measurement_harness = provenance["measurement_harness"]
    verify_measurement_harness(path.parent, measurement_harness)

    measurement_harness_root = _safe_relative_path(
        measurement_harness["root"],
        "report.provenance.measurement_harness.root",
    )
    constraints_path = (
        measurement_harness_root
        / _safe_relative_path(
            provenance["target"]["constraints_path"],
            "report.provenance.target.constraints_path",
        )
    )
    _verified_attestation_path(
        path,
        constraints_path.as_posix(),
        provenance["target"]["constraints_sha256"],
        "target constraints",
    )

    verified_reports: dict[str, Path] = {}
    for name, record in provenance["reports"].items():
        verified_reports[name] = _verified_attestation_path(
            path,
            record["path"],
            record["sha256"],
            f"raw report {name}",
        )

    try:
        raw_text = {
            name: report_path.read_text(encoding="utf-8", errors="replace")
            for name, report_path in verified_reports.items()
        }
    except OSError as exc:
        raise ReportError(f"cannot reread an attested raw report: {exc}") from exc

    target_mhz = float(provenance["target"]["clock_mhz"])
    derived = {
        "resources": {
            "post_synth": parse_vivado_utilization(
                raw_text["post_synth_utilization"]
            ),
            "post_route": parse_vivado_utilization(
                raw_text["post_route_utilization"]
            ),
        },
        "timing": {
            "post_route": parse_vivado_timing(
                raw_text["post_route_timing"],
                target_mhz=target_mhz,
            )
        },
        "hierarchy": parse_vivado_hierarchy(
            raw_text["post_route_hierarchy"]
        ),
        "hierarchical_resources": parse_vivado_hierarchical_resources(
            raw_text["post_route_hierarchy"]
        ),
        "route_status": parse_route_status(raw_text["route_status"]),
        "structural": parse_structural_report(raw_text["structural"]),
    }
    for section, observed in derived.items():
        if _canonical_json(report[section]) != _canonical_json(observed):
            raise ReportError(
                f"report.{section} does not match its attested raw report"
            )

    derived_routed = bool(
        derived["route_status"]["is_route_design"]
        and not derived["route_status"]["errors_in_routes"]
        and derived["route_status"]["status"].lower()
        in {"routed", "fully_routed", "fully-routed"}
    )
    if provenance["routed"] is not derived_routed:
        raise ReportError(
            "report.provenance.routed does not match route-status evidence"
        )


def _load_json(path: Path) -> Mapping[str, Any]:
    value = _read_strict_json(path, "report")
    report = _json_object(value, f"report {path}")
    validate_report(report)
    _verify_provenance_files(path, report)
    return report


def discover_report(location: str | os.PathLike[str]) -> Path:
    """Resolve one report without recursive or heuristic discovery."""

    path = Path(location).expanduser().resolve()
    if path.is_file():
        if path.name != "tacc_report.json":
            raise ReportError(
                f"report file must be named tacc_report.json: {path}"
            )
        return path
    if not path.is_dir():
        raise ReportError(f"report location does not exist: {path}")
    expected = path / "tacc_report.json"
    if not expected.is_file():
        raise ReportError(f"missing required report: {expected}")
    return expected


def load_report(location: str | os.PathLike[str]) -> Mapping[str, Any]:
    return _load_json(discover_report(location))


def _resource_value(
    report: Mapping[str, Any],
    stage: str,
    resource: str,
    field: str,
) -> float:
    return float(report["resources"][stage][resource][field])


def _growth_percent(before: float, after: float) -> float:
    if before == 0.0:
        return 0.0 if after == 0.0 else math.inf
    return ((after - before) / before) * 100.0


def _headroom_percent(used: float, available: float) -> float:
    return ((available - used) / available) * 100.0


def _machine_number(value: float) -> float | None:
    """Return a strict-JSON number, using null for an undefined ratio."""

    return value if math.isfinite(value) else None


def _canonical_json(value: Any) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"))


def compare_reports(
    current_main: Mapping[str, Any],
    topology_only: Mapping[str, Any],
    full_tacc: Mapping[str, Any],
    *,
    expected_full_commit: str,
    expected_full_manifest_sha256: str,
) -> Mapping[str, Any]:
    """Apply every locked physical acceptance gate."""

    expected_full_commit = _commit_string(
        expected_full_commit,
        "expected_full_commit",
    )
    expected_full_manifest_sha256 = _sha256_string(
        expected_full_manifest_sha256,
        "expected_full_manifest_sha256",
    )
    reports = {
        "current-main": current_main,
        "topology-only": topology_only,
        "full-tacc": full_tacc,
    }
    for report in reports.values():
        validate_report(report)

    issues: list[str] = []
    for expected_label, report in reports.items():
        if report["label"] != expected_label:
            issues.append(
                f"{expected_label}: embedded label is {report['label']!r}"
            )
        if not report["provenance"]["routed"]:
            issues.append(f"{expected_label}: report is not post-route")

    current_source = current_main["provenance"]["source"]
    topology_source = topology_only["provenance"]["source"]
    full_source = full_tacc["provenance"]["source"]
    if current_source["commit"] != CURRENT_MAIN_SHA:
        issues.append(
            "current-main: source commit is not the locked Phase-0 baseline"
        )
    if topology_source["commit"] != TOPOLOGY_ONLY_SHA:
        issues.append(
            "topology-only: source commit is not the immutable topology checkpoint"
        )
    for label, source, locked_sha, locked_manifest_sha256 in (
        (
            "current-main",
            current_source,
            CURRENT_MAIN_SHA,
            CURRENT_MAIN_MANIFEST_SHA256,
        ),
        (
            "topology-only",
            topology_source,
            TOPOLOGY_ONLY_SHA,
            TOPOLOGY_ONLY_MANIFEST_SHA256,
        ),
    ):
        if source["kind"] != "ref":
            issues.append(f"{label}: immutable baseline was not materialized by ref")
        if source["dirty"]:
            issues.append(f"{label}: immutable baseline provenance is dirty")
        if source["requested"].lower() != locked_sha:
            issues.append(
                f"{label}: requested ref is not the locked full commit SHA"
            )
        if source["manifest_sha256"] != locked_manifest_sha256:
            issues.append(
                f"{label}: source manifest is not the locked baseline manifest"
            )

    if full_source["commit"] != expected_full_commit:
        issues.append(
            "full-tacc: source commit does not match expected full commit"
        )
    if full_source["manifest_sha256"] != expected_full_manifest_sha256:
        issues.append(
            "full-tacc: source manifest does not match expected full manifest"
        )

    reference_harness = current_main["provenance"]["measurement_harness"]
    for label in ("topology-only", "full-tacc"):
        harness = reports[label]["provenance"]["measurement_harness"]
        if harness["manifest_sha256"] != reference_harness["manifest_sha256"]:
            issues.append(
                f"{label}: measurement-harness manifest differs from current-main"
            )
        if harness["campaign_id"] != reference_harness["campaign_id"]:
            issues.append(
                f"{label}: measurement campaign differs from current-main"
            )

    comparison_keys = (
        ("tool", "name"),
        ("tool", "version"),
        ("target", "part"),
        ("target", "clock_mhz"),
        ("target", "constraints_path"),
        ("target", "constraints_sha256"),
    )
    reference_provenance = current_main["provenance"]
    if reference_provenance["tool"]["name"] != "Vivado":
        issues.append("current-main: physical tool must be Vivado")
    tool_identity = reference_provenance["tool"]["version"]
    if not (
        re.search(r"\bVivado\s+v?[A-Za-z0-9_.-]+", tool_identity)
        and re.search(r"\bSW\s+Build\b", tool_identity)
        and re.search(r"\bIP\s+Build\b", tool_identity)
    ):
        issues.append(
            "current-main: Vivado identity must include version, SW Build, "
            "and IP Build"
        )
    if reference_provenance["target"]["part"] != "xc7k325tffg900-2":
        issues.append(
            "current-main: target part must be xc7k325tffg900-2"
        )
    reference_strategy = reference_provenance["strategy"]
    if reference_strategy["top"] != "mp64_soc":
        issues.append("current-main: physical comparison top must be mp64_soc")
    memory_match = re.fullmatch(
        r"effective_mem_depth=(\d+);fixed_banks=4;row_bits=512;"
        r"address_contract=(fixed-16384|depth-derived)",
        reference_strategy["memory_configuration"],
    )
    if memory_match is None:
        issues.append(
            "current-main: physical memory configuration is not concrete"
        )
    else:
        memory_depth = int(memory_match.group(1), 10)
        address_contract = memory_match.group(2)
        if (
            memory_depth < 512
            or memory_depth > 16384
            or memory_depth & (memory_depth - 1)
        ):
            issues.append(
                "current-main: physical MEM_DEPTH is outside the supported "
                "power-of-two range"
            )
        if memory_depth != 16384 and address_contract != "depth-derived":
            issues.append(
                "current-main: reduced physical MEM_DEPTH lacks a "
                "depth-derived address contract"
            )
        minimum_ramb36 = 4 * 8 * ((memory_depth + 511) // 512)
        if minimum_ramb36 > 445:
            issues.append(
                "current-main: physical memory configuration needs at least "
                f"{minimum_ramb36} RAMB36 blocks on a 445-block target"
            )
    for label in ("topology-only", "full-tacc"):
        provenance = reports[label]["provenance"]
        for section, key in comparison_keys:
            expected = reference_provenance[section][key]
            observed = provenance[section][key]
            if observed != expected:
                issues.append(
                    f"{label}: provenance {section}.{key} differs from current-main"
                )
        if _canonical_json(provenance["strategy"]) != _canonical_json(
            reference_provenance["strategy"]
        ):
            issues.append(
                f"{label}: synthesis/implementation strategy differs from current-main"
            )

    for label, report in reports.items():
        route_status = report["route_status"]
        if not route_status["is_route_design"]:
            issues.append(f"{label}: route-status evidence is not implementation")
        if route_status["status"].lower() not in {
            "routed",
            "fully_routed",
            "fully-routed",
        }:
            issues.append(
                f"{label}: route status is {route_status['status']!r}, not routed"
            )
        if route_status["errors_in_routes"]:
            issues.append(f"{label}: route-status evidence contains route errors")

        target_clock = float(report["provenance"]["target"]["clock_mhz"])
        timing_clock = float(report["timing"]["post_route"]["target_mhz"])
        if not math.isclose(
            target_clock,
            TARGET_CLOCK_MHZ,
            rel_tol=0.0,
            abs_tol=1e-6,
        ):
            issues.append(
                f"{label}: target clock is {target_clock:g} MHz, expected 100 MHz"
            )
        if not math.isclose(
            timing_clock,
            target_clock,
            rel_tol=0.0,
            abs_tol=1e-6,
        ):
            issues.append(
                f"{label}: timing target does not match provenance target"
            )
        unconstrained = report["timing"]["post_route"]["unconstrained_paths"]
        if unconstrained != 0:
            issues.append(
                f"{label}: {unconstrained} unconstrained timing paths remain"
            )

        for stage in ("post_synth", "post_route"):
            for resource in RESOURCE_NAMES:
                used = _resource_value(report, stage, resource, "used")
                available = _resource_value(
                    report,
                    stage,
                    resource,
                    "available",
                )
                if used > available:
                    issues.append(
                        f"{label}: {stage} {resource} usage {used:g} "
                        f"exceeds {available:g}"
                    )

    for resource in RESOURCE_NAMES:
        expected_available = _resource_value(
            current_main,
            "post_synth",
            resource,
            "available",
        )
        for label, report in reports.items():
            for stage in ("post_synth", "post_route"):
                observed_available = _resource_value(
                    report,
                    stage,
                    resource,
                    "available",
                )
                if not math.isclose(
                    observed_available,
                    expected_available,
                    rel_tol=0.0,
                    abs_tol=1e-9,
                ):
                    issues.append(
                        f"{label}: {stage} {resource} capacity "
                        f"{observed_available:g} differs from like-for-like "
                        f"capacity {expected_available:g}"
                    )

    growth_limits = {
        "lut": MAX_LUT_GROWTH_PERCENT,
        "ff": MAX_FF_GROWTH_PERCENT,
        "dsp": MAX_DSP_GROWTH_PERCENT,
    }
    tacc_growth: dict[str, float | None] = {}
    for resource, limit in growth_limits.items():
        before = _resource_value(
            topology_only,
            "post_synth",
            resource,
            "used",
        )
        after = _resource_value(full_tacc, "post_synth", resource, "used")
        growth = _growth_percent(before, after)
        tacc_growth[resource] = _machine_number(growth)
        if growth > limit:
            formatted = "infinite" if math.isinf(growth) else f"{growth:.3f}%"
            issues.append(
                f"full-tacc: post-synthesis {resource} growth is {formatted}, "
                f"limit {limit:g}%"
            )

    topology_bram = _resource_value(
        topology_only,
        "post_synth",
        "bram",
        "used",
    )
    full_bram = _resource_value(full_tacc, "post_synth", "bram", "used")
    bram_delta = full_bram - topology_bram
    if not math.isclose(bram_delta, 0.0, rel_tol=0.0, abs_tol=1e-9):
        issues.append(
            f"full-tacc: post-synthesis BRAM delta is {bram_delta:g}, expected 0"
        )

    full_headroom: dict[str, float] = {}
    for resource in ("lut", "ff", "dsp"):
        used = _resource_value(full_tacc, "post_route", resource, "used")
        available = _resource_value(
            full_tacc,
            "post_route",
            resource,
            "available",
        )
        headroom = _headroom_percent(used, available)
        full_headroom[resource] = headroom
        if headroom < MIN_HEADROOM_PERCENT:
            issues.append(
                f"full-tacc: post-route {resource} headroom is "
                f"{headroom:.3f}%, minimum {MIN_HEADROOM_PERCENT:g}%"
            )

    for label in ("topology-only", "full-tacc"):
        timing = reports[label]["timing"]["post_route"]
        if float(timing["wns_ns"]) < 0.0:
            issues.append(f"{label}: post-route WNS is negative")
        if float(timing["tns_ns"]) < 0.0:
            issues.append(f"{label}: post-route TNS is negative")

    topology_fmax = float(topology_only["timing"]["post_route"]["fmax_mhz"])
    full_fmax = float(full_tacc["timing"]["post_route"]["fmax_mhz"])
    if topology_fmax <= 0.0:
        issues.append("topology-only: Fmax must be positive")
        fmax_regression = math.inf
    else:
        fmax_regression = max(
            0.0,
            ((topology_fmax - full_fmax) / topology_fmax) * 100.0,
        )
        if fmax_regression > MAX_FMAX_REGRESSION_PERCENT:
            issues.append(
                f"full-tacc: Fmax regression is {fmax_regression:.3f}%, "
                f"limit {MAX_FMAX_REGRESSION_PERCENT:g}%"
            )

    expected_hierarchy = {
        "current-main": {"mp64_tile": 4, "mp64_tacc": 0},
        "topology-only": {"mp64_tile": 7, "mp64_tacc": 0},
        "full-tacc": {"mp64_tile": 7, "mp64_tacc": 7},
    }
    for label, expected in expected_hierarchy.items():
        hierarchy = reports[label]["hierarchy"]
        for module, count in expected.items():
            if hierarchy[module] != count:
                issues.append(
                    f"{label}: hierarchy has {hierarchy[module]} {module} "
                    f"instances, expected {count}"
                )

    structural = full_tacc["structural"]
    if structural["tacc_specific_multiplier_arrays"] != 0:
        issues.append("full-tacc: TACC-specific multiplier arrays are present")
    if structural["tacc_bram_cells"] != 0:
        issues.append("full-tacc: TACC state consumes BRAM cells")
    lanes = structural["max_fp_feedback_lanes_per_engine"]
    if lanes <= 0 or lanes > MAX_FP_FEEDBACK_LANES_PER_ENGINE:
        issues.append(
            "full-tacc: FP feedback lanes per engine are outside the locked "
            "1..16 bound"
        )
    if structural["persistent_tacc_bits"] != PERSISTENT_TACC_BITS:
        issues.append(
            "full-tacc: persistent TACC storage is not exactly 14,336 bits"
        )
    if structural["shared_tacc_stage_bits"] != SHARED_TACC_STAGE_BITS:
        issues.append(
            "full-tacc: shared TACC transfer stage is not exactly 2,048 bits"
        )
    if not structural["multiplier_sharing_verified"]:
        issues.append("full-tacc: multiplier sharing review is not verified")
    if not structural["fp_adder_sharing_verified"]:
        issues.append("full-tacc: FP-adder sharing review is not verified")
    if not structural["bounded_feedback_path_verified"]:
        issues.append(
            "full-tacc: bounded product/add/ownership feedback path is not verified"
        )

    whole_design_deltas: dict[str, dict[str, dict[str, float]]] = {}
    for transition, before_report, after_report in (
        ("current_main_to_topology", current_main, topology_only),
        ("topology_to_full_tacc", topology_only, full_tacc),
    ):
        whole_design_deltas[transition] = {}
        for stage in ("post_synth", "post_route"):
            whole_design_deltas[transition][stage] = {
                resource: (
                    _resource_value(after_report, stage, resource, "used")
                    - _resource_value(before_report, stage, resource, "used")
                )
                for resource in RESOURCE_NAMES
            }

    hierarchical_deltas: dict[str, dict[str, dict[str, float]]] = {}
    for transition, before_report, after_report in (
        ("current_main_to_topology", current_main, topology_only),
        ("topology_to_full_tacc", topology_only, full_tacc),
    ):
        hierarchical_deltas[transition] = {}
        for module in ("mp64_tile", "mp64_tacc"):
            hierarchical_deltas[transition][module] = {
                resource: (
                    float(after_report["hierarchical_resources"][module][resource])
                    - float(
                        before_report["hierarchical_resources"][module][resource]
                    )
                )
                for resource in RESOURCE_NAMES
            }

    timing_deltas: dict[str, dict[str, float]] = {}
    for transition, before_report, after_report in (
        ("current_main_to_topology", current_main, topology_only),
        ("topology_to_full_tacc", topology_only, full_tacc),
    ):
        before_timing = before_report["timing"]["post_route"]
        after_timing = after_report["timing"]["post_route"]
        timing_deltas[transition] = {
            field: float(after_timing[field]) - float(before_timing[field])
            for field in ("wns_ns", "tns_ns", "fmax_mhz")
        }

    return {
        "schema": CHECK_SCHEMA,
        "schema_version": CHECK_SCHEMA_VERSION,
        "expected_full_source": {
            "commit": expected_full_commit,
            "manifest_sha256": expected_full_manifest_sha256,
        },
        "passed": not issues,
        "issues": issues,
        "comparisons": {
            "whole_design_resource_delta": whole_design_deltas,
            "hierarchical_resource_delta": hierarchical_deltas,
            "post_route_timing_delta": timing_deltas,
            "topology_to_tacc_post_synth_growth_percent": tacc_growth,
            "topology_to_tacc_post_synth_bram_delta": bram_delta,
            "full_tacc_post_route_headroom_percent": full_headroom,
            "topology_to_tacc_fmax_regression_percent": _machine_number(
                fmax_regression
            ),
        },
    }


_RESOURCE_ALIASES = {
    "slice luts": "lut",
    "slice registers": "ff",
    "block ram tile": "bram",
    "dsps": "dsp",
}


def _table_number(value: str) -> float | None:
    token = value.strip().replace(",", "")
    if not token or token.lower() in {"n/a", "na", "-"}:
        return None
    if token.startswith("<"):
        token = token[1:]
    try:
        return float(token)
    except ValueError:
        return None


def parse_vivado_utilization(text: str) -> Mapping[str, Mapping[str, float]]:
    """Parse total LUT/FF/BRAM/DSP rows from a Vivado utilization report."""

    parsed: dict[str, Mapping[str, float]] = {}
    for raw_line in text.splitlines():
        if "|" not in raw_line:
            continue
        cells = [cell.strip() for cell in raw_line.strip().strip("|").split("|")]
        if len(cells) < 3:
            continue
        name = re.sub(r"\s+", " ", cells[0].lower().replace("*", "")).strip()
        resource = _RESOURCE_ALIASES.get(name)
        if resource is None:
            continue
        numbers = [
            number
            for number in (_table_number(cell) for cell in cells[1:])
            if number is not None
        ]
        if len(numbers) < 2:
            raise ReportError(
                f"Vivado utilization row {cells[0]!r} lacks used/available values"
            )
        if resource in parsed:
            raise ReportError(
                f"Vivado utilization report repeats total row for {resource}"
            )
        parsed[resource] = {
            "used": numbers[0],
            # Vivado total rows put utilization percentage last and available
            # capacity immediately before it.
            "available": numbers[-2] if len(numbers) >= 3 else numbers[-1],
        }
    missing = sorted(set(RESOURCE_NAMES) - set(parsed))
    if missing:
        raise ReportError(
            "Vivado utilization report is missing totals for "
            + ", ".join(missing)
        )
    return parsed


def _marker_values(text: str, marker: str) -> Mapping[str, str]:
    result: dict[str, str] = {}
    pattern = re.compile(
        rf"^\s*{re.escape(marker)}\s+([A-Za-z0-9_.-]+)\s+(\S+)\s*$"
    )
    for line in text.splitlines():
        match = pattern.match(line)
        if not match:
            continue
        key, value = match.groups()
        if key in result:
            raise ReportError(f"{marker} repeats field {key!r}")
        result[key] = value
    return result


def parse_vivado_timing(
    text: str,
    *,
    target_mhz: float = TARGET_CLOCK_MHZ,
) -> Mapping[str, float | int]:
    """Parse routed timing from native evidence and cross-check helper markers."""

    markers = _marker_values(text, "TACC_TIMING")
    unknown_markers = sorted(
        set(markers)
        - {
            "wns_ns",
            "tns_ns",
            "fmax_mhz",
            "unconstrained_paths",
            "clock_period_ns",
        }
    )
    if unknown_markers:
        raise ReportError(
            "TACC_TIMING contains unknown fields: "
            + ", ".join(unknown_markers)
        )

    def marker_float(name: str) -> float | None:
        if name not in markers:
            return None
        try:
            value = float(markers[name])
        except ValueError as exc:
            raise ReportError(f"TACC_TIMING {name} is not numeric") from exc
        if not math.isfinite(value):
            raise ReportError(f"TACC_TIMING {name} is not finite")
        return value

    native_candidates: list[tuple[float, float]] = []
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if "WNS(ns)" not in line or "TNS(ns)" not in line:
            continue
        for candidate in lines[index + 1 : index + 12]:
            row = candidate.strip().strip("|").strip()
            if not row or set(row) <= {"-", " "}:
                continue
            if not re.match(
                r"^[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:\s|$)",
                row,
            ):
                continue
            numbers = re.findall(
                r"(?<![A-Za-z])[-+]?(?:\d+(?:\.\d*)?|\.\d+)",
                row,
            )
            if len(numbers) < 2:
                continue
            candidate_values = (float(numbers[0]), float(numbers[1]))
            if not all(math.isfinite(value) for value in candidate_values):
                raise ReportError(
                    "Vivado timing summary contains non-finite WNS/TNS"
                )
            native_candidates.append(candidate_values)
            break

    if not native_candidates:
        raise ReportError(
            "Vivado timing report lacks native WNS/TNS summary evidence"
        )
    wns, tns = native_candidates[0]
    for other_wns, other_tns in native_candidates[1:]:
        if not (
            math.isclose(wns, other_wns, rel_tol=0.0, abs_tol=5e-4)
            and math.isclose(tns, other_tns, rel_tol=0.0, abs_tol=5e-4)
        ):
            raise ReportError(
                "Vivado timing report contains contradictory native "
                "WNS/TNS summaries"
            )

    marker_wns = marker_float("wns_ns")
    marker_tns = marker_float("tns_ns")
    if marker_wns is not None and not math.isclose(
        marker_wns,
        wns,
        rel_tol=0.0,
        abs_tol=5e-4,
    ):
        raise ReportError(
            f"TACC_TIMING wns_ns={marker_wns:g} disagrees with "
            f"native WNS {wns:g}"
        )
    if marker_tns is not None and not math.isclose(
        marker_tns,
        tns,
        rel_tol=0.0,
        abs_tol=5e-4,
    ):
        raise ReportError(
            f"TACC_TIMING tns_ns={marker_tns:g} disagrees with "
            f"native TNS {tns:g}"
        )

    marker_unconstrained: int | None = None
    if "unconstrained_paths" in markers:
        try:
            marker_unconstrained = int(markers["unconstrained_paths"], 10)
        except ValueError as exc:
            raise ReportError(
                "TACC_TIMING unconstrained_paths is not an integer"
            ) from exc
        if marker_unconstrained < 0:
            raise ReportError("unconstrained path count cannot be negative")

    native_unconstrained_values: set[int] = set()
    patterns = (
        r"Unconstrained\s+Paths?\s*[:|]\s*(\d+)",
        r"\b(\d+)\s+unconstrained\s+paths?\b",
    )
    for pattern in patterns:
        native_unconstrained_values.update(
            int(match.group(1), 10)
            for match in re.finditer(pattern, text, flags=re.IGNORECASE)
        )
    if len(native_unconstrained_values) > 1:
        raise ReportError(
            "Vivado timing report contains contradictory native "
            "unconstrained-path counts"
        )
    native_unconstrained = (
        next(iter(native_unconstrained_values))
        if native_unconstrained_values
        else None
    )
    if (
        marker_unconstrained is not None
        and native_unconstrained is not None
        and marker_unconstrained != native_unconstrained
    ):
        raise ReportError(
            "TACC_TIMING unconstrained_paths disagrees with native evidence"
        )
    unconstrained = (
        native_unconstrained
        if native_unconstrained is not None
        else marker_unconstrained
    )
    if unconstrained is None:
        raise ReportError(
            "Vivado timing report does not state the unconstrained path count"
        )
    if target_mhz <= 0.0 or not math.isfinite(target_mhz):
        raise ReportError("target_mhz must be positive and finite")

    target_period_ns = 1000.0 / target_mhz
    actual_period_ns = marker_float("clock_period_ns")
    if actual_period_ns is None:
        raise ReportError(
            "TACC_TIMING must report the applied clock_period_ns"
        )
    if not math.isclose(
        actual_period_ns,
        target_period_ns,
        rel_tol=0.0,
        abs_tol=5e-4,
    ):
        raise ReportError(
            f"applied clock period {actual_period_ns:g} ns does not match "
            f"the {target_period_ns:g} ns target"
        )
    estimated_critical_period_ns = actual_period_ns - wns
    if estimated_critical_period_ns <= 0.0:
        raise ReportError("native WNS implies a nonpositive critical period")
    fmax = 1000.0 / estimated_critical_period_ns
    marker_fmax = marker_float("fmax_mhz")
    if marker_fmax is not None and not math.isclose(
        marker_fmax,
        fmax,
        rel_tol=1e-5,
        abs_tol=0.01,
    ):
        raise ReportError(
            f"TACC_TIMING fmax_mhz={marker_fmax:g} disagrees with "
            f"native-WNS-derived Fmax {fmax:g}"
        )

    return {
        "wns_ns": wns,
        "tns_ns": tns,
        "fmax_mhz": fmax,
        "target_mhz": target_mhz,
        "unconstrained_paths": unconstrained,
    }


def parse_vivado_hierarchy(text: str) -> Mapping[str, int]:
    """Count native tile/TACC rows and cross-check optional helper markers."""

    markers = _marker_values(text, "TACC_HIERARCHY")
    parsed: dict[str, int] = {}
    for module in ("mp64_tile", "mp64_tacc"):
        if module in markers:
            try:
                parsed[module] = int(markers[module], 10)
            except ValueError as exc:
                raise ReportError(
                    f"TACC_HIERARCHY {module} is not an integer"
                ) from exc

    row_counts = {"mp64_tile": 0, "mp64_tacc": 0}
    saw_hierarchy_row = False
    for raw_line in text.splitlines():
        if "|" not in raw_line:
            continue
        cells = [
            cell.strip()
            for cell in raw_line.strip().strip("|").split("|")
        ]
        for module in row_counts:
            if module in cells:
                row_counts[module] += 1
                saw_hierarchy_row = True

    if not saw_hierarchy_row:
        raise ReportError(
            "Vivado hierarchy report lacks recognizable native module rows"
        )
    for module, row_count in row_counts.items():
        if module in parsed and parsed[module] != row_count:
            raise ReportError(
                f"TACC_HIERARCHY {module}={parsed[module]} disagrees "
                f"with {row_count} native hierarchy rows"
            )
    for module in row_counts:
        parsed.setdefault(module, row_counts[module])

    for module, count in parsed.items():
        if count < 0:
            raise ReportError(f"hierarchy count for {module} cannot be negative")
    return parsed


def parse_vivado_hierarchical_resources(
    text: str,
) -> Mapping[str, Mapping[str, float]]:
    """Parse aggregate module resources from explicit Vivado query markers."""

    markers = _marker_values(text, "TACC_HIER_RESOURCE")
    result: dict[str, dict[str, float]] = {}
    missing: list[str] = []
    for module in ("mp64_tile", "mp64_tacc"):
        result[module] = {}
        for resource in RESOURCE_NAMES:
            key = f"{module}.{resource}"
            if key not in markers:
                missing.append(key)
                continue
            try:
                value = float(markers[key])
            except ValueError as exc:
                raise ReportError(
                    f"TACC_HIER_RESOURCE {key} is not numeric"
                ) from exc
            if not math.isfinite(value) or value < 0.0:
                raise ReportError(
                    f"TACC_HIER_RESOURCE {key} must be finite and nonnegative"
                )
            result[module][resource] = value
    if missing:
        raise ReportError(
            "hierarchy report is missing aggregate resource markers for "
            + ", ".join(missing)
        )
    return result


def parse_route_status(text: str) -> Mapping[str, Any]:
    """Parse explicit post-route design-state evidence emitted by Vivado."""

    markers = _marker_values(text, "TACC_ROUTE_STATUS")
    missing = sorted(
        {"is_route_design", "status", "errors_in_routes"} - set(markers)
    )
    if missing:
        raise ReportError(
            "route-status report is missing markers for " + ", ".join(missing)
        )
    route_token = markers["is_route_design"].lower()
    if route_token not in {"0", "1", "false", "true"}:
        raise ReportError(
            "TACC_ROUTE_STATUS is_route_design must be true/false or 1/0"
        )
    status = markers["status"].strip()
    if not status:
        raise ReportError("TACC_ROUTE_STATUS status must not be empty")
    errors_token = markers["errors_in_routes"].lower()
    if errors_token not in {"0", "1", "false", "true"}:
        raise ReportError(
            "TACC_ROUTE_STATUS errors_in_routes must be true/false or 1/0"
        )
    return {
        "is_route_design": route_token in {"1", "true"},
        "status": status,
        "errors_in_routes": errors_token in {"1", "true"},
    }


def parse_structural_report(text: str) -> Mapping[str, Any]:
    """Parse the checked structural review marker file."""

    markers = _marker_values(text, "TACC_STRUCTURAL")
    integer_fields = (
        "tacc_specific_multiplier_arrays",
        "max_fp_feedback_lanes_per_engine",
        "persistent_tacc_bits",
        "shared_tacc_stage_bits",
        "tacc_bram_cells",
    )
    boolean_fields = (
        "multiplier_sharing_verified",
        "fp_adder_sharing_verified",
        "bounded_feedback_path_verified",
    )
    missing = [
        field
        for field in (*integer_fields, *boolean_fields)
        if field not in markers
    ]
    if missing:
        raise ReportError(
            "structural report is missing markers for " + ", ".join(missing)
        )

    result: dict[str, Any] = {}
    for field in integer_fields:
        try:
            result[field] = int(markers[field], 10)
        except ValueError as exc:
            raise ReportError(
                f"TACC_STRUCTURAL {field} is not an integer"
            ) from exc
        if result[field] < 0:
            raise ReportError(
                f"TACC_STRUCTURAL {field} cannot be negative"
            )
    for field in boolean_fields:
        token = markers[field].lower()
        if token not in {"0", "1", "false", "true"}:
            raise ReportError(
                f"TACC_STRUCTURAL {field} must be true/false or 1/0"
            )
        result[field] = token in {"1", "true"}
    return result


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def discover_raw_reports(build_dir: str | os.PathLike[str]) -> Mapping[str, Path]:
    """Require the canonical raw report set at one exact build directory."""

    root = Path(build_dir).expanduser().resolve()
    if not root.is_dir():
        raise ReportError(f"Vivado report directory does not exist: {root}")
    reports: dict[str, Path] = {}
    for key, filename in RAW_REPORT_FILES.items():
        path = root / filename
        if not path.is_file():
            raise ReportError(f"missing required Vivado report: {path}")
        reports[key] = path
    return reports


def create_report_from_raw(
    *,
    label: str,
    provenance: Mapping[str, Any],
    build_dir: str | os.PathLike[str],
    report_root: str | os.PathLike[str] | None = None,
) -> Mapping[str, Any]:
    """Create the canonical machine-readable report from raw Vivado files."""

    raw = discover_raw_reports(build_dir)
    post_synth = parse_vivado_utilization(
        raw["post_synth_utilization"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )
    post_route = parse_vivado_utilization(
        raw["post_route_utilization"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )
    timing = parse_vivado_timing(
        raw["post_route_timing"].read_text(
            encoding="utf-8",
            errors="replace",
        ),
        target_mhz=float(provenance["target"]["clock_mhz"]),
    )
    hierarchy = parse_vivado_hierarchy(
        raw["post_route_hierarchy"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )
    hierarchical_resources = parse_vivado_hierarchical_resources(
        raw["post_route_hierarchy"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )
    route_status = parse_route_status(
        raw["route_status"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )
    structural = parse_structural_report(
        raw["structural"].read_text(
            encoding="utf-8",
            errors="replace",
        )
    )

    report_provenance = copy.deepcopy(dict(provenance))
    report_provenance["routed"] = bool(
        route_status["is_route_design"]
        and not route_status["errors_in_routes"]
        and route_status["status"].lower()
        in {"routed", "fully_routed", "fully-routed"}
    )
    raw_root = Path(build_dir).expanduser().resolve()
    output_root = (
        raw_root
        if report_root is None
        else Path(report_root).expanduser().resolve()
    )
    try:
        raw_root.relative_to(output_root)
    except ValueError as exc:
        raise ReportError(
            f"Vivado report directory {raw_root} is outside report root "
            f"{output_root}"
        ) from exc
    report_provenance["reports"] = {
        key: {
            "path": path.relative_to(output_root).as_posix(),
            "sha256": _sha256_file(path),
        }
        for key, path in sorted(raw.items())
    }
    report = {
        "schema": REPORT_SCHEMA,
        "schema_version": REPORT_SCHEMA_VERSION,
        "label": label,
        "provenance": report_provenance,
        "resources": {
            "post_synth": post_synth,
            "post_route": post_route,
        },
        "timing": {"post_route": timing},
        "hierarchy": hierarchy,
        "hierarchical_resources": hierarchical_resources,
        "route_status": route_status,
        "structural": structural,
    }
    validate_report(report)
    return report


def atomic_write_json(path: Path, value: Mapping[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.",
        suffix=".tmp",
        dir=path.parent,
        text=True,
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(value, stream, indent=2, sort_keys=True, allow_nan=False)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary_name, path)
    except BaseException:
        try:
            os.unlink(temporary_name)
        except FileNotFoundError:
            pass
        raise


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Compare locked current-main, topology-only, and full-TACC "
            "post-route acceptance reports."
        )
    )
    parser.add_argument(
        "--current-main",
        required=True,
        help="directory containing current-main/tacc_report.json",
    )
    parser.add_argument(
        "--topology-only",
        required=True,
        help="directory containing topology-only/tacc_report.json",
    )
    parser.add_argument(
        "--full-tacc",
        required=True,
        help="directory containing full-tacc/tacc_report.json",
    )
    parser.add_argument(
        "--expected-full-commit",
        required=True,
        help="exact 40-digit full-TACC source commit expected in the report",
    )
    parser.add_argument(
        "--expected-full-manifest-sha256",
        required=True,
        help="exact full-TACC source-manifest SHA-256 expected in the report",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="optional path for the machine-readable acceptance result",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = _build_parser()
    arguments = parser.parse_args(argv)
    try:
        report_paths = {
            "current-main": discover_report(arguments.current_main),
            "topology-only": discover_report(arguments.topology_only),
            "full-tacc": discover_report(arguments.full_tacc),
        }
        output_path = (
            arguments.output.expanduser().resolve()
            if arguments.output is not None
            else None
        )
        if output_path is not None:
            for label, report_path in report_paths.items():
                try:
                    output_path.relative_to(report_path.parent)
                except ValueError:
                    continue
                raise ReportError(
                    "--output must be outside every input report package; "
                    f"it falls under {label}"
                )
        result = compare_reports(
            _load_json(report_paths["current-main"]),
            _load_json(report_paths["topology-only"]),
            _load_json(report_paths["full-tacc"]),
            expected_full_commit=arguments.expected_full_commit,
            expected_full_manifest_sha256=(
                arguments.expected_full_manifest_sha256
            ),
        )
        rendered = json.dumps(
            result,
            indent=2,
            sort_keys=True,
            allow_nan=False,
        )
        if output_path is not None:
            atomic_write_json(output_path, result)
        print(rendered)
        return 0 if result["passed"] else 1
    except (OSError, ReportError) as exc:
        print(f"check_tacc_reports.py: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
