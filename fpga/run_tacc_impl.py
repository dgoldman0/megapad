#!/usr/bin/env python3
"""Materialize and, only when explicitly requested, implement one TACC source.

The default operation is deliberately lightweight: it creates an isolated,
content-addressed source snapshot plus provenance, but never starts Vivado.
``--run-vivado`` is the sole opt-in for the heavyweight implementation.  The
implementation flow must itself advertise every canonical routed report before
the tool is launched, so an old synthesis-only Tcl script fails closed.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path, PurePosixPath
import posixpath
import re
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
from typing import Any, Mapping, Sequence

try:
    from .check_tacc_reports import (
        RAW_REPORT_FILES,
        ReportError,
        SOURCE_MANIFEST_SCHEMA,
        SOURCE_MANIFEST_SCHEMA_VERSION,
        TARGET_CLOCK_MHZ,
        atomic_write_json,
        create_report_from_raw,
        verify_measurement_harness,
        verify_source_snapshot,
    )
except ImportError:  # Direct ``python fpga/run_tacc_impl.py`` execution.
    from check_tacc_reports import (  # type: ignore[no-redef]
        RAW_REPORT_FILES,
        ReportError,
        SOURCE_MANIFEST_SCHEMA,
        SOURCE_MANIFEST_SCHEMA_VERSION,
        TARGET_CLOCK_MHZ,
        atomic_write_json,
        create_report_from_raw,
        verify_measurement_harness,
        verify_source_snapshot,
    )


PREPARATION_SCHEMA = "megapad.tacc.implementation-preparation"
PREPARATION_SCHEMA_VERSION = 1

EXPECTED_PART = "xc7k325tffg900-2"
EXPECTED_TOP = "mp64_soc"
IMPLEMENTATION_TCL = Path("fpga/tacc_impl_harness.tcl")
MEASUREMENT_XDC = Path("fpga/constraints/tacc_measurement.xdc")
MEASUREMENT_HARNESS_FILES = (
    IMPLEMENTATION_TCL,
    MEASUREMENT_XDC,
    Path("fpga/run_tacc_impl.py"),
    Path("fpga/check_tacc_reports.py"),
)

CANONICAL_RAW_REPORTS = tuple(RAW_REPORT_FILES.values())
REQUIRED_REPORT_MARKERS = (
    "TACC_TIMING",
    "TACC_HIERARCHY",
    "TACC_HIER_RESOURCE",
    "TACC_ROUTE_STATUS",
    "TACC_STRUCTURAL",
)

_LABEL_PATTERN = re.compile(r"[a-z0-9][a-z0-9._-]*\Z")
_CAMPAIGN_PATTERN = re.compile(r"[A-Za-z0-9][A-Za-z0-9._-]*\Z")


class RunnerError(RuntimeError):
    """Preparation or explicitly requested implementation could not complete."""


def _sha256_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _json_bytes(value: Any) -> bytes:
    return (
        json.dumps(
            value,
            indent=2,
            sort_keys=True,
            allow_nan=False,
        )
        + "\n"
    ).encode("utf-8")


def _write_bytes_atomically(path: Path, payload: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(
        prefix=f".{path.name}.",
        suffix=".tmp",
        dir=path.parent,
    )
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(payload)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary_name, path)
    except BaseException:
        try:
            os.unlink(temporary_name)
        except FileNotFoundError:
            pass
        raise


def _run_git(
    repository: Path,
    arguments: Sequence[str],
    *,
    context: str,
) -> bytes:
    try:
        completed = subprocess.run(
            ["git", "-C", str(repository), *arguments],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
        )
    except OSError as exc:
        raise RunnerError(f"cannot execute git for {context}: {exc}") from exc
    if completed.returncode != 0:
        detail = completed.stderr.decode("utf-8", errors="replace").strip()
        raise RunnerError(f"git failed while {context}: {detail or 'no detail'}")
    return completed.stdout


def _git_root(path: Path) -> Path:
    output = _run_git(
        path,
        ("rev-parse", "--show-toplevel"),
        context="finding the source-tree repository",
    )
    return Path(output.decode("utf-8", errors="strict").strip()).resolve()


def _worktree_roots(repository: Path) -> tuple[Path, ...]:
    output = _run_git(
        repository,
        ("worktree", "list", "--porcelain"),
        context="enumerating protected git worktrees",
    )
    roots = []
    for line in output.decode("utf-8", errors="surrogateescape").splitlines():
        if line.startswith("worktree "):
            roots.append(Path(line.removeprefix("worktree ")).resolve())
    if not roots:
        raise RunnerError("git did not report any protected worktree roots")
    return tuple(roots)


def _reject_output_in_worktree(repository: Path, output: Path) -> None:
    for root in _worktree_roots(repository):
        try:
            output.relative_to(root)
        except ValueError:
            continue
        raise RunnerError(
            f"output {output} is inside git worktree {root}; use an isolated "
            "directory such as /tmp/megapad-tacc-reports"
        )


def _resolve_commit(repository: Path, source_ref: str) -> str:
    output = _run_git(
        repository,
        (
            "rev-parse",
            "--verify",
            "--end-of-options",
            f"{source_ref}^{{commit}}",
        ),
        context=f"resolving source ref {source_ref!r}",
    )
    commit = output.decode("ascii", errors="strict").strip().lower()
    if not re.fullmatch(r"[0-9a-f]{40}", commit):
        raise RunnerError(f"git returned an invalid full commit SHA: {commit!r}")
    return commit


def _visible_source_paths(repository: Path) -> list[Path]:
    raw = _run_git(
        repository,
        ("ls-files", "-z", "--cached", "--others", "--exclude-standard"),
        context="enumerating tracked and non-ignored source files",
    )
    deleted_raw = _run_git(
        repository,
        ("ls-files", "-z", "--deleted"),
        context="enumerating deleted tracked source files",
    )
    deleted = {
        encoded.decode("utf-8", errors="surrogateescape")
        for encoded in deleted_raw.split(b"\0")
        if encoded
    }
    paths: list[Path] = []
    for encoded in raw.split(b"\0"):
        if not encoded:
            continue
        text = encoded.decode("utf-8", errors="surrogateescape")
        if text in deleted:
            continue
        path = Path(text)
        if path.is_absolute() or ".." in path.parts:
            raise RunnerError(f"git returned an unsafe source path: {text!r}")
        paths.append(path)
    if len(paths) != len(set(paths)):
        raise RunnerError("git returned duplicate source paths")
    return sorted(paths, key=lambda item: item.as_posix())


def _validate_symlink_target(path: Path, target: str) -> None:
    pure_target = PurePosixPath(target)
    if pure_target.is_absolute():
        raise RunnerError(f"source symlink {path} has an absolute target")
    normalized = posixpath.normpath(
        posixpath.join(path.parent.as_posix(), target)
    )
    if normalized == ".." or normalized.startswith("../"):
        raise RunnerError(f"source symlink {path} escapes the source snapshot")


def _entry_for_path(root: Path, relative: Path) -> Mapping[str, Any]:
    path = root / relative
    try:
        metadata = path.lstat()
    except OSError as exc:
        raise RunnerError(f"cannot stat source path {path}: {exc}") from exc
    mode = stat.S_IMODE(metadata.st_mode)
    common = {
        "path": relative.as_posix(),
        "mode": f"{mode:04o}",
    }
    if stat.S_ISREG(metadata.st_mode):
        return {
            **common,
            "type": "file",
            "size": metadata.st_size,
            "sha256": _sha256_file(path),
        }
    if stat.S_ISLNK(metadata.st_mode):
        target = os.readlink(path)
        _validate_symlink_target(relative, target)
        return {
            **common,
            "type": "symlink",
            "target": target,
        }
    raise RunnerError(
        f"source path {path} is neither a regular file nor a symbolic link"
    )


def _manifest_for_paths(
    root: Path,
    paths: Sequence[Path],
) -> Mapping[str, Any]:
    return {
        "schema": SOURCE_MANIFEST_SCHEMA,
        "schema_version": SOURCE_MANIFEST_SCHEMA_VERSION,
        "entries": [_entry_for_path(root, path) for path in paths],
    }


def _manifest_for_tree(root: Path) -> Mapping[str, Any]:
    paths: list[Path] = []
    for directory, directory_names, file_names in os.walk(
        root,
        topdown=True,
        followlinks=False,
    ):
        directory_path = Path(directory)
        relative_directory = directory_path.relative_to(root)
        retained_directories: list[str] = []
        for name in directory_names:
            candidate = directory_path / name
            if candidate.is_symlink():
                paths.append((relative_directory / name))
            else:
                retained_directories.append(name)
        directory_names[:] = retained_directories
        for name in file_names:
            paths.append(relative_directory / name)
    paths.sort(key=lambda item: item.as_posix())
    return _manifest_for_paths(root, paths)


def _manifest_digest(manifest: Mapping[str, Any]) -> str:
    return hashlib.sha256(_json_bytes(manifest)).hexdigest()


def _source_status(repository: Path) -> Mapping[str, Any]:
    commit = _resolve_commit(repository, "HEAD")
    status = _run_git(
        repository,
        ("status", "--porcelain=v1", "-z", "--untracked-files=all"),
        context="recording source-tree status",
    )
    paths = _visible_source_paths(repository)
    manifest = _manifest_for_paths(repository, paths)
    return {
        "commit": commit,
        "status_sha256": hashlib.sha256(status).hexdigest(),
        "status_entries": [
            entry.decode("utf-8", errors="surrogateescape")
            for entry in status.split(b"\0")
            if entry
        ],
        "paths": paths,
        "manifest": manifest,
        "manifest_sha256": _manifest_digest(manifest),
    }


def _copy_source_tree(repository: Path, destination: Path) -> Mapping[str, Any]:
    before = _source_status(repository)
    symlink_paths = {
        path
        for path in before["paths"]
        if (repository / path).is_symlink()
    }
    for path in before["paths"]:
        if any(parent in symlink_paths for parent in path.parents):
            raise RunnerError(
                f"tracked path {path} descends through a source symlink"
            )

    destination.mkdir(parents=True, exist_ok=False)
    for relative in before["paths"]:
        source = repository / relative
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        try:
            if source.is_symlink():
                link_target = os.readlink(source)
                _validate_symlink_target(relative, link_target)
                os.symlink(link_target, target)
            elif source.is_file():
                shutil.copy2(source, target, follow_symlinks=False)
            else:
                raise RunnerError(
                    f"source path disappeared or changed type: {source}"
                )
        except OSError as exc:
            raise RunnerError(f"cannot copy source path {source}: {exc}") from exc

    after = _source_status(repository)
    for key in ("commit", "status_sha256", "manifest_sha256"):
        if before[key] != after[key]:
            raise RunnerError(
                "source tree changed while its isolated snapshot was created"
            )

    copied_manifest = _manifest_for_tree(destination)
    copied_digest = _manifest_digest(copied_manifest)
    if copied_digest != before["manifest_sha256"]:
        raise RunnerError(
            "isolated source does not match the source-tree content manifest"
        )
    return {
        "commit": before["commit"],
        "dirty": bool(before["status_entries"]),
        "status_entries": before["status_entries"],
        "manifest": copied_manifest,
        "manifest_sha256": copied_digest,
    }


def _copy_measurement_harness(
    repository: Path,
    destination: Path,
) -> Mapping[str, Any]:
    """Copy only the audited implementation machinery into the run.

    Historical source refs intentionally do not supply their own measurement
    scripts.  All three measurements use this one external, hash-bound
    harness so an old synthesis-only Tcl file cannot weaken the campaign.
    """

    paths = tuple(sorted(MEASUREMENT_HARNESS_FILES, key=lambda item: item.as_posix()))
    for relative in paths:
        source = repository / relative
        if not source.is_file() or source.is_symlink():
            raise RunnerError(
                "measurement harness requires a regular file at "
                f"{source}"
            )

    before = _manifest_for_paths(repository, paths)
    before_digest = _manifest_digest(before)
    destination.mkdir(parents=True, exist_ok=False)
    for relative in paths:
        source = repository / relative
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        try:
            shutil.copy2(source, target, follow_symlinks=False)
        except OSError as exc:
            raise RunnerError(
                f"cannot copy measurement-harness file {source}: {exc}"
            ) from exc

    after = _manifest_for_paths(repository, paths)
    if _manifest_digest(after) != before_digest:
        raise RunnerError(
            "measurement harness changed while its isolated copy was created"
        )
    copied = _manifest_for_tree(destination)
    copied_digest = _manifest_digest(copied)
    if copied_digest != before_digest:
        raise RunnerError(
            "isolated measurement harness does not match its content manifest"
        )
    return {
        "manifest": copied,
        "manifest_sha256": copied_digest,
    }


def _archive_ref(
    repository: Path,
    commit: str,
    archive_path: Path,
) -> None:
    _run_git(
        repository,
        (
            "archive",
            "--format=tar",
            f"--output={archive_path}",
            commit,
        ),
        context=f"archiving commit {commit}",
    )


def _safe_archive_member(name: str) -> Path:
    pure = PurePosixPath(name)
    if pure.is_absolute() or any(part in {"", ".", ".."} for part in pure.parts):
        raise RunnerError(f"git archive contains unsafe path {name!r}")
    return Path(*pure.parts)


def _extract_git_archive(archive_path: Path, destination: Path) -> None:
    destination.mkdir(parents=True, exist_ok=False)
    symlinks: list[tuple[Path, str, int]] = []
    seen: set[Path] = set()
    try:
        archive = tarfile.open(archive_path, mode="r:")
    except (OSError, tarfile.TarError) as exc:
        raise RunnerError(f"cannot open git archive: {exc}") from exc
    with archive:
        for member in archive:
            relative = _safe_archive_member(member.name)
            if relative in seen:
                raise RunnerError(
                    f"git archive repeats source path {relative.as_posix()}"
                )
            seen.add(relative)
            target = destination / relative
            if member.isdir():
                target.mkdir(parents=True, exist_ok=True)
                os.chmod(target, member.mode & 0o777)
                continue
            target.parent.mkdir(parents=True, exist_ok=True)
            if member.isfile():
                extracted = archive.extractfile(member)
                if extracted is None:
                    raise RunnerError(
                        f"cannot read archived source {relative.as_posix()}"
                    )
                try:
                    with extracted, target.open("xb") as output:
                        shutil.copyfileobj(extracted, output)
                    os.chmod(target, member.mode & 0o777)
                except OSError as exc:
                    raise RunnerError(
                        f"cannot extract source {relative.as_posix()}: {exc}"
                    ) from exc
                continue
            if member.issym():
                _validate_symlink_target(relative, member.linkname)
                symlinks.append(
                    (relative, member.linkname, member.mode & 0o777)
                )
                continue
            raise RunnerError(
                f"git archive contains unsupported entry {relative.as_posix()}"
            )

    for relative, link_target, _mode in symlinks:
        target = destination / relative
        try:
            os.symlink(link_target, target)
        except OSError as exc:
            raise RunnerError(
                f"cannot create archived symlink {relative}: {exc}"
            ) from exc


def _materialize_ref(
    repository: Path,
    source_ref: str,
    destination: Path,
    temporary_root: Path,
) -> Mapping[str, Any]:
    commit = _resolve_commit(repository, source_ref)
    archive_path = temporary_root / "source.tar"
    _archive_ref(repository, commit, archive_path)
    _extract_git_archive(archive_path, destination)
    archive_path.unlink()
    manifest = _manifest_for_tree(destination)
    return {
        "commit": commit,
        "dirty": False,
        "status_entries": [],
        "manifest": manifest,
        "manifest_sha256": _manifest_digest(manifest),
    }


def _active_tcl(text: str) -> str:
    return "\n".join(
        line for line in text.splitlines() if not line.lstrip().startswith("#")
    )


def _option(
    text: str,
    option: str,
    *,
    command: str | None = None,
    default: str | None = None,
) -> str:
    scope = text
    if command is not None:
        match = re.search(
            rf"(?ms)^\s*{re.escape(command)}\b(.*?)(?=^\s*[A-Za-z_]"
            rf"[A-Za-z0-9_]*\b|\Z)",
            text,
        )
        if match is None:
            if default is not None:
                return default
            raise RunnerError(f"synthesis Tcl lacks active {command}")
        scope = match.group(1)
    match = re.search(
        rf"(?:^|\s)-{re.escape(option)}\s+([^\s\\]+)",
        scope,
    )
    if match is None:
        if default is not None:
            return default
        context = command or "Tcl"
        raise RunnerError(f"{context} lacks required -{option} option")
    return match.group(1).strip("{}\"'")


def _command_directive(text: str, command: str) -> str:
    return _option(
        text,
        "directive",
        command=command,
        default="tool-default",
    )


def _memory_configuration(
    memory_depth: int | None,
    source_root: Path,
) -> str:
    depth = "unresolved" if memory_depth is None else str(memory_depth)
    memory_path = source_root / "rtl/mem/mp64_memory.v"
    try:
        memory_text = memory_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise RunnerError(f"cannot read memory contract {memory_path}: {exc}") from exc
    tile_width = re.search(
        r"\bparameter\s+ADDR_W_TILE\s*=\s*([^,\n)]+)",
        memory_text,
    )
    cpu_width = re.search(
        r"\bparameter\s+ADDR_W_CPU\s*=\s*([^,\n)]+)",
        memory_text,
    )
    address_contract = "fixed-16384"
    if (
        tile_width is not None
        and cpu_width is not None
        and "$clog2" in tile_width.group(1)
        and "$clog2" in cpu_width.group(1)
        and "BANK_DEPTH" in tile_width.group(1)
        and "BANK_DEPTH" in cpu_width.group(1)
    ):
        address_contract = "depth-derived"
    return (
        f"effective_mem_depth={depth};fixed_banks=4;row_bits=512;"
        f"address_contract={address_contract}"
    )


def _parse_flow_metadata(
    harness_root: Path,
    *,
    source_root: Path,
    memory_depth: int | None,
) -> Mapping[str, Any]:
    tcl_path = harness_root / IMPLEMENTATION_TCL
    try:
        raw_tcl = tcl_path.read_text(encoding="utf-8")
    except OSError as exc:
        raise RunnerError(f"cannot read synthesis Tcl {tcl_path}: {exc}") from exc
    tcl = _active_tcl(raw_tcl)

    part = _option(tcl, "part", command="create_project")
    top = _option(tcl, "top", command="synth_design")
    if part != EXPECTED_PART:
        raise RunnerError(
            f"synthesis target is {part!r}, expected {EXPECTED_PART!r}"
        )
    if top != EXPECTED_TOP:
        raise RunnerError(f"synthesis top is {top!r}, expected {EXPECTED_TOP!r}")

    xdc_names = sorted(set(re.findall(r"([A-Za-z0-9_.-]+\.xdc)\b", tcl)))
    if len(xdc_names) != 1:
        raise RunnerError(
            "synthesis Tcl must reference exactly one unambiguous XDC file"
        )
    constraints_relative = MEASUREMENT_XDC
    if xdc_names != [MEASUREMENT_XDC.name]:
        raise RunnerError(
            "implementation harness must reference exactly "
            f"{MEASUREMENT_XDC.name}"
        )
    constraints_path = harness_root / constraints_relative
    if not constraints_path.is_file():
        raise RunnerError(f"referenced constraints are missing: {constraints_path}")

    strategy = {
        "top": top,
        "synth_flatten_hierarchy": _option(
            tcl,
            "flatten_hierarchy",
            command="synth_design",
        ),
        "synth_directive": _option(
            tcl,
            "directive",
            command="synth_design",
        ),
        "synth_retiming": _option(
            tcl,
            "retiming",
            command="synth_design",
        ),
        "synth_verilog_define": _option(
            tcl,
            "verilog_define",
            command="synth_design",
        ),
        "opt_directive": _command_directive(tcl, "opt_design"),
        "place_directive": _command_directive(tcl, "place_design"),
        "phys_opt_directive": _command_directive(tcl, "phys_opt_design"),
        "route_directive": _command_directive(tcl, "route_design"),
        "memory_configuration": _memory_configuration(
            memory_depth,
            source_root,
        ),
    }
    return {
        "tcl_text": tcl,
        "part": part,
        "constraints_path": constraints_relative.as_posix(),
        "constraints_sha256": _sha256_file(constraints_path),
        "strategy": strategy,
    }


def _validate_heavy_flow(flow: Mapping[str, Any]) -> None:
    tcl = str(flow["tcl_text"])
    for command in (
        "opt_design",
        "place_design",
        "phys_opt_design",
        "route_design",
    ):
        if re.search(rf"(?m)^\s*{re.escape(command)}\b", tcl) is None:
            raise RunnerError(
                f"--run-vivado requires an active {command} command"
            )
    missing = [
        filename for filename in CANONICAL_RAW_REPORTS if filename not in tcl
    ]
    if missing:
        raise RunnerError(
            "--run-vivado requires the Tcl flow to emit canonical reports: "
            + ", ".join(missing)
        )
    missing_markers = [
        marker for marker in REQUIRED_REPORT_MARKERS if marker not in tcl
    ]
    if missing_markers:
        raise RunnerError(
            "--run-vivado requires explicit machine-readable report markers: "
            + ", ".join(missing_markers)
        )

    memory = str(flow["strategy"]["memory_configuration"])
    depth_match = re.fullmatch(
        r"effective_mem_depth=(\d+);fixed_banks=4;row_bits=512;"
        r"address_contract=(fixed-16384|depth-derived)",
        memory,
    )
    if depth_match is None:
        raise RunnerError(
            "--run-vivado requires a concrete integer production MEM_DEPTH"
        )
    depth = int(depth_match.group(1), 10)
    address_contract = depth_match.group(2)
    if depth < 512 or depth > 16384 or depth & (depth - 1):
        raise RunnerError(
            "--run-vivado requires a power-of-two MEM_DEPTH from 512 "
            "through 16384 rows"
        )
    if depth != 16384 and address_contract != "depth-derived":
        raise RunnerError(
            "--run-vivado refuses reduced MEM_DEPTH because this source keeps "
            "fixed 14/17-bit memory addresses; land a depth-derived memory "
            "contract or preserve the full 16384-row design on a larger target"
        )
    minimum_ramb36 = 4 * 8 * ((depth + 511) // 512)
    if minimum_ramb36 > 445:
        raise RunnerError(
            "production memory configuration is not physically fixed: "
            f"MEM_DEPTH={depth} requires at least {minimum_ramb36} RAMB36 "
            "blocks before other storage on a 445-block device"
        )


def _build_provenance(
    *,
    source_kind: str,
    requested: str,
    source_state: Mapping[str, Any],
    harness_state: Mapping[str, Any],
    campaign_id: str,
    flow: Mapping[str, Any],
) -> Mapping[str, Any]:
    return {
        "source": {
            "kind": source_kind,
            "requested": requested,
            "commit": source_state["commit"],
            "dirty": source_state["dirty"],
            "status_entries": source_state["status_entries"],
            "root": "source",
            "manifest_path": "source_manifest.json",
            "manifest_sha256": source_state["manifest_sha256"],
        },
        "measurement_harness": {
            "root": "measurement_harness",
            "manifest_path": "measurement_harness_manifest.json",
            "manifest_sha256": harness_state["manifest_sha256"],
            "campaign_id": campaign_id,
        },
        "tool": {
            "name": "Vivado",
            "version": "not-run",
        },
        "target": {
            "part": flow["part"],
            "clock_mhz": TARGET_CLOCK_MHZ,
            "constraints_path": flow["constraints_path"],
            "constraints_sha256": flow["constraints_sha256"],
        },
        "strategy": flow["strategy"],
        "routed": False,
        "reports": {},
    }


def _prepare_output(
    *,
    repository: Path,
    harness_repository: Path,
    source_ref: str | None,
    source_tree: Path | None,
    label: str,
    campaign_id: str,
    memory_depth: int | None,
    requested_mode: str,
    output: Path,
) -> Mapping[str, Any]:
    if output.exists() or output.is_symlink():
        raise RunnerError(f"output already exists; refusing to mix reports: {output}")
    if source_ref is not None:
        source_repository: Path | None = None
        _reject_output_in_worktree(repository, output)
    else:
        assert source_tree is not None
        source_repository = _git_root(source_tree)
        _reject_output_in_worktree(source_repository, output)
    if (
        harness_repository != repository
        and harness_repository != source_repository
    ):
        _reject_output_in_worktree(harness_repository, output)
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = Path(
        tempfile.mkdtemp(
            prefix=f".{output.name}.prepare.",
            dir=output.parent,
        )
    )
    try:
        source_destination = temporary / "source"
        if source_ref is not None:
            source_state = _materialize_ref(
                repository,
                source_ref,
                source_destination,
                temporary,
            )
            source_kind = "ref"
            requested = source_ref
        else:
            assert source_repository is not None
            source_state = _copy_source_tree(
                source_repository,
                source_destination,
            )
            source_kind = "tree"
            requested = str(source_repository)

        harness_destination = temporary / "measurement_harness"
        harness_state = _copy_measurement_harness(
            harness_repository,
            harness_destination,
        )

        manifest_path = temporary / "source_manifest.json"
        manifest_payload = _json_bytes(source_state["manifest"])
        _write_bytes_atomically(manifest_path, manifest_payload)
        manifest_file_sha = _sha256_file(manifest_path)
        if manifest_file_sha != source_state["manifest_sha256"]:
            raise RunnerError("written source manifest digest is inconsistent")

        harness_manifest_path = temporary / "measurement_harness_manifest.json"
        _write_bytes_atomically(
            harness_manifest_path,
            _json_bytes(harness_state["manifest"]),
        )
        if _sha256_file(harness_manifest_path) != harness_state["manifest_sha256"]:
            raise RunnerError(
                "written measurement-harness manifest digest is inconsistent"
            )

        flow = _parse_flow_metadata(
            harness_destination,
            source_root=source_destination,
            memory_depth=memory_depth,
        )
        provenance = _build_provenance(
            source_kind=source_kind,
            requested=requested,
            source_state=source_state,
            harness_state=harness_state,
            campaign_id=campaign_id,
            flow=flow,
        )
        atomic_write_json(temporary / "provenance.json", provenance)
        atomic_write_json(
            temporary / "preparation.json",
            {
                "schema": PREPARATION_SCHEMA,
                "schema_version": PREPARATION_SCHEMA_VERSION,
                "label": label,
                "stage": "immutable-source-preparation",
                "requested_mode": requested_mode,
                "pre_run_provenance": provenance,
            },
        )
        try:
            verify_source_snapshot(temporary, provenance["source"])
            verify_measurement_harness(
                temporary,
                provenance["measurement_harness"],
            )
        except ReportError as exc:
            raise RunnerError(
                "isolated source or measurement harness failed preparation "
                f"manifest verification: {exc}"
            ) from exc
        os.replace(temporary, output)
        return {
            "output": output,
            "source": output / "source",
            "measurement_harness": output / "measurement_harness",
            "provenance": provenance,
            "flow": flow,
        }
    except BaseException:
        if temporary.exists():
            shutil.rmtree(temporary)
        raise


def _vivado_version(executable: str) -> str:
    try:
        completed = subprocess.run(
            [executable, "-version"],
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
        )
    except OSError as exc:
        raise RunnerError(f"cannot execute Vivado {executable!r}: {exc}") from exc
    if completed.returncode != 0:
        raise RunnerError(
            f"Vivado version query failed with status {completed.returncode}"
        )
    lines = [
        line.strip()
        for line in completed.stdout.splitlines()
        if line.strip()
    ]
    version_lines = [
        line for line in lines if re.search(r"\bVivado\s+v?[A-Za-z0-9_.-]+", line)
    ]
    build_lines = [
        line
        for line in lines
        if re.search(r"\b(?:SW|IP|SharedData)\s+Build\b", line)
    ]
    if len(version_lines) != 1 or not any(
        line.startswith("SW Build") for line in build_lines
    ):
        raise RunnerError(
            "Vivado version output lacks a unique version and full SW build "
            "identity"
        )
    return " | ".join([version_lines[0], *build_lines])


def _verify_prepared_inputs(
    prepared: Mapping[str, Any],
    *,
    phase: str,
) -> None:
    try:
        verify_source_snapshot(
            prepared["output"],
            prepared["provenance"]["source"],
        )
        verify_measurement_harness(
            prepared["output"],
            prepared["provenance"]["measurement_harness"],
        )
    except ReportError as exc:
        raise RunnerError(
            "isolated source or measurement harness failed "
            f"{phase} manifest verification: {exc}"
        ) from exc


def _run_vivado(
    prepared: Mapping[str, Any],
    *,
    label: str,
    executable: str,
) -> Mapping[str, Any]:
    _validate_heavy_flow(prepared["flow"])
    _verify_prepared_inputs(prepared, phase="pre-Vivado")
    output = Path(prepared["output"])
    source = Path(prepared["source"])
    harness = Path(prepared["measurement_harness"])
    build = output / "build"
    build.mkdir(exist_ok=False)
    memory_configuration = prepared["flow"]["strategy"][
        "memory_configuration"
    ]
    depth_match = re.fullmatch(
        r"effective_mem_depth=(\d+);fixed_banks=4;row_bits=512;"
        r"address_contract=(?:fixed-16384|depth-derived)",
        str(memory_configuration),
    )
    if depth_match is None:
        raise RunnerError("validated Vivado flow lost its concrete MEM_DEPTH")
    memory_depth = depth_match.group(1)
    version = _vivado_version(executable)
    command = [
        executable,
        "-mode",
        "batch",
        "-nojournal",
        "-nolog",
        "-notrace",
        "-source",
        str(harness / IMPLEMENTATION_TCL),
    ]
    environment = os.environ.copy()
    environment.update(
        {
            "TACC_SOURCE_ROOT": str(source),
            "TACC_BUILD_DIR": str(build),
            "TACC_MEM_DEPTH": memory_depth,
        }
    )
    stdout_path = output / "vivado.stdout.log"
    stderr_path = output / "vivado.stderr.log"
    try:
        with stdout_path.open("wb") as stdout, stderr_path.open("wb") as stderr:
            completed = subprocess.run(
                command,
                cwd=output,
                check=False,
                stdout=stdout,
                stderr=stderr,
                env=environment,
            )
    except OSError as exc:
        raise RunnerError(f"cannot run Vivado implementation: {exc}") from exc

    execution = {
        "heavy_tool_opt_in": True,
        "command": command,
        "tool": {"name": "Vivado", "version": version},
        "exit_code": completed.returncode,
        "stdout": stdout_path.name,
        "stderr": stderr_path.name,
    }
    atomic_write_json(output / "execution.json", execution)
    if completed.returncode != 0:
        raise RunnerError(
            f"Vivado implementation failed with status {completed.returncode}; "
            f"see {stdout_path} and {stderr_path}"
        )

    _verify_prepared_inputs(prepared, phase="post-Vivado")
    provenance = dict(prepared["provenance"])
    provenance["tool"] = {"name": "Vivado", "version": version}
    try:
        report = create_report_from_raw(
            label=label,
            provenance=provenance,
            build_dir=build,
            report_root=output,
        )
    except (OSError, ReportError) as exc:
        raise RunnerError(
            f"Vivado completed but routed report creation failed closed: {exc}"
        ) from exc
    atomic_write_json(output / "provenance.json", report["provenance"])
    atomic_write_json(output / "tacc_report.json", report)
    return report


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description=(
            "Create an isolated TACC implementation source snapshot. "
            "The default is prepare-only; Vivado runs only with --run-vivado."
        )
    )
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument(
        "--source-ref",
        help="git ref to archive from --repo without touching a worktree",
    )
    source.add_argument(
        "--source-tree",
        type=Path,
        help="git worktree to copy, including non-ignored uncommitted files",
    )
    parser.add_argument(
        "--repo",
        type=Path,
        default=Path(__file__).resolve().parents[1],
        help="repository used to resolve --source-ref (default: this checkout)",
    )
    parser.add_argument("--label", required=True, help="physical run label")
    parser.add_argument(
        "--campaign-id",
        required=True,
        help="shared identifier used by all three attested measurements",
    )
    parser.add_argument(
        "--mem-depth",
        type=int,
        help=(
            "rows per one of four 512-bit banks; required for --run-vivado, "
            "optional for preparation while the production choice is open"
        ),
    )
    parser.add_argument(
        "--out",
        required=True,
        type=Path,
        help="new output directory; an existing path is rejected",
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--prepare-only",
        action="store_true",
        help="materialize and validate only (also the default)",
    )
    mode.add_argument(
        "--run-vivado",
        action="store_true",
        help="explicitly opt in to the heavyweight routed implementation",
    )
    parser.add_argument(
        "--vivado",
        default="vivado",
        help="Vivado executable used only with --run-vivado",
    )
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    arguments = _build_parser().parse_args(argv)
    if not _LABEL_PATTERN.fullmatch(arguments.label):
        print(
            "run_tacc_impl.py: --label must use lowercase letters, digits, "
            "dot, underscore, or hyphen",
            file=sys.stderr,
        )
        return 2
    if not _CAMPAIGN_PATTERN.fullmatch(arguments.campaign_id):
        print(
            "run_tacc_impl.py: --campaign-id must use letters, digits, dot, "
            "underscore, or hyphen",
            file=sys.stderr,
        )
        return 2
    if arguments.mem_depth is not None and (
        arguments.mem_depth < 512
        or arguments.mem_depth > 16384
        or arguments.mem_depth & (arguments.mem_depth - 1)
    ):
        print(
            "run_tacc_impl.py: --mem-depth must be a power of two from "
            "512 through 16384",
            file=sys.stderr,
        )
        return 2
    if arguments.run_vivado and arguments.mem_depth is None:
        print(
            "run_tacc_impl.py: --run-vivado requires an explicit --mem-depth",
            file=sys.stderr,
        )
        return 2

    repository = arguments.repo.expanduser().resolve()
    output = arguments.out.expanduser().resolve()
    source_tree = (
        None
        if arguments.source_tree is None
        else arguments.source_tree.expanduser().resolve()
    )
    try:
        harness_repository = _git_root(Path(__file__).resolve().parents[1])
        prepared = _prepare_output(
            repository=repository,
            harness_repository=harness_repository,
            source_ref=arguments.source_ref,
            source_tree=source_tree,
            label=arguments.label,
            campaign_id=arguments.campaign_id,
            memory_depth=arguments.mem_depth,
            requested_mode=(
                "run-vivado" if arguments.run_vivado else "prepare-only"
            ),
            output=output,
        )
        if arguments.run_vivado:
            _run_vivado(
                prepared,
                label=arguments.label,
                executable=arguments.vivado,
            )
            mode = "run-vivado"
        else:
            mode = "prepare-only"
        print(
            json.dumps(
                {
                    "label": arguments.label,
                    "mode": mode,
                    "output": str(output),
                    "campaign_id": arguments.campaign_id,
                    "source_commit": prepared["provenance"]["source"]["commit"],
                    "source_manifest_sha256": prepared["provenance"]["source"][
                        "manifest_sha256"
                    ],
                    "measurement_harness_manifest_sha256": prepared[
                        "provenance"
                    ]["measurement_harness"]["manifest_sha256"],
                    "memory_configuration": prepared["provenance"][
                        "strategy"
                    ]["memory_configuration"],
                },
                indent=2,
                sort_keys=True,
                allow_nan=False,
            )
        )
        return 0
    except (OSError, RunnerError) as exc:
        print(f"run_tacc_impl.py: {exc}", file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
