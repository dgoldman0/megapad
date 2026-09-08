#!/usr/bin/env python3
"""Compare Python/native execution of the unchanged, bounded Akashic CELL fixture.

Run from a built MegaPad checkout with --akashic-root pointing at the checkout
containing local_testing/test_rich_terminal_cell_feed.py. The sibling Akashic
rich-terminal worktree is the default when present. One Python runtime and then
one native runtime execute sequentially; geometry and fixture watchdogs are
fixed. This measures two CELL rows, not Desktop or physical presentation.
"""

from __future__ import annotations

import argparse
from collections import defaultdict
from datetime import datetime, timezone
import hashlib
import importlib
import importlib.util
import json
import os
from pathlib import Path
import platform
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parent
DEFAULT_AKASHIC = ROOT.parent / "akashic-rich-terminal-vertical"


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _repository(root: Path) -> dict:
    def git(*arguments):
        return subprocess.check_output(
            ["git", "--no-optional-locks", *arguments], cwd=root, text=True
        ).strip()

    return {
        "root": str(root),
        "head": git("rev-parse", "HEAD"),
        "status": git("status", "--porcelain=v1", "--untracked-files=all"),
    }


def _sources(akashic_root: Path, fixture, extension_path: Path) -> dict:
    paths = {
        Path(__file__).resolve(), extension_path,
        ROOT / "setup_simulator_accel.py",
        ROOT / "simulator/accel/semantic_executor.cpp",
        ROOT / "rich-terminal.f", ROOT / "kdos.f",
        Path(fixture.__file__).resolve(),
    }
    # Bind imported support/test modules as well as the runtime implementation;
    # include lazy simulator modules before either backend gets to import them.
    for directory in ("simulator", "rich_terminal", "shared"):
        paths.update((ROOT / directory).rglob("*.py"))
    for module in tuple(sys.modules.values()):
        filename = getattr(module, "__file__", None)
        if filename:
            path = Path(filename).resolve()
            if path.suffix == ".py" and (
                path.is_relative_to(ROOT) or path.is_relative_to(akashic_root)
            ):
                paths.add(path)
    for relative in (
        "utils/uint-range.f", "utils/memory-span.f",
        "tui/rich-terminal/phase-profile.f", "tui/rich-terminal/apt1-engine.f",
    ):
        paths.add(akashic_root / "akashic" / relative)
    from tests.simulator import test_kdos_exceptions as exceptions

    for name in (
        "PREFIX_FIXTURE", "PARSE_FIXTURE", "ALLOCATOR_FIXTURE",
        "SNAPSHOT_FIXTURE", "EXCEPTION_FIXTURE",
    ):
        paths.add(getattr(exceptions, name).resolve())
    return {str(path): _sha256(path) for path in sorted(paths)}


def _load_fixture(akashic_root: Path):
    # The Akashic fixture otherwise chooses its paired MegaPad feature checkout.
    os.environ["MEGAPAD_ROOT"] = str(ROOT)
    sys.path.insert(0, str(ROOT))
    path = akashic_root / "local_testing/test_rich_terminal_cell_feed.py"
    spec = importlib.util.spec_from_file_location("_semantic_cell_feed_fixture", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot import CELL fixture at {path}")
    fixture = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = fixture
    spec.loader.exec_module(fixture)
    if fixture.MEGAPAD_ROOT.resolve() != ROOT:
        raise RuntimeError("CELL fixture did not bind to this MegaPad checkout")
    if fixture.RICH_TERMINAL_SOURCE.resolve() != ROOT / "rich-terminal.f":
        raise RuntimeError("CELL fixture imported terminal source from another checkout")
    runtime = importlib.import_module("simulator.runtime")
    if Path(runtime.__file__).resolve() != ROOT / "simulator/runtime.py":
        raise RuntimeError("simulator imported from a different checkout")
    importlib.import_module("simulator.native_execution")
    extension = importlib.import_module("_megaforth_native")
    extension_path = Path(extension.__file__).resolve()
    if extension_path.parent != ROOT:
        raise RuntimeError("native extension must be built in this checkout")
    return fixture, extension_path


def _run(fixture_module, backend: str, engine_source: bytes) -> dict:
    class MeasuredFeed(fixture_module._FeedHarness):
        def __init__(self, *args, **kwargs):
            self.calls = defaultdict(lambda: {"count": 0, "seconds": 0.0, "steps": 0})
            super().__init__(*args, **kwargs)

        def call(self, word, *inputs):
            started = time.perf_counter()
            values, steps = super().call(word, *inputs)
            record = self.calls[word]
            record["count"] += 1
            record["seconds"] += time.perf_counter() - started
            record["steps"] += steps
            return values, steps

    os.environ["MEGAFORTH_EXECUTOR"] = backend
    started = time.perf_counter()
    feed = MeasuredFeed(engine_source, cols=280)
    setup_seconds = time.perf_counter() - started
    try:
        if feed.runtime.execution_backend != backend:
            raise AssertionError(f"requested {backend} execution was not selected")
        native_after_setup = feed.runtime.native_execution_stats
        attach_started = time.perf_counter()
        feed.attach()
        attach_seconds = time.perf_counter() - attach_started
        native_before_feed = feed.runtime.native_execution_stats
        result = fixture_module._exercise(feed)
        result.update(
            backend=backend,
            setup_seconds=setup_seconds,
            attach_seconds=attach_seconds,
            initial_pt_rows=dict(feed.calls["CF-INITIAL-ROW"]),
            calls=dict(feed.calls),
            native_after_setup=native_after_setup,
            native_before_feed=native_before_feed,
            native_after_feed=feed.runtime.native_execution_stats,
            final_frame_bytes=feed.driver.core.frame_bytes_received,
            final_frames_by_type=dict(feed.driver.core.frames_received_by_type),
            final_semantic_cycles=feed.runtime.diagnostics.semantic_cycles,
            final_perf_cycles=feed.runtime.diagnostics.perf_cycles,
            final_timer={name: getattr(feed.runtime.timer, name) for name in
                         ("counter", "compare", "control", "status", "irq_pending")},
        )
    finally:
        feed.close()
    result["total_fixture_seconds"] = time.perf_counter() - started
    return result


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--akashic-root", type=Path,
                        default=DEFAULT_AKASHIC if DEFAULT_AKASHIC.is_dir() else None)
    parser.add_argument("--output", type=Path, help="also write the JSON report here")
    args = parser.parse_args()
    if args.akashic_root is None:
        parser.error("--akashic-root is required without the sibling rich worktree")
    akashic_root = args.akashic_root.expanduser().resolve()
    if not (akashic_root / "local_testing/test_rich_terminal_cell_feed.py").is_file():
        parser.error("--akashic-root does not contain the current CELL fixture")

    previous = {name: os.environ.get(name) for name in
                ("MEGAPAD_ROOT", "MEGAFORTH_EXECUTOR")}
    try:
        fixture, extension_path = _load_fixture(akashic_root)
        bindings = {"megapad": _repository(ROOT), "akashic": _repository(akashic_root)}
        sources = _sources(akashic_root, fixture, extension_path)
        engine_source = fixture.SOURCE.read_bytes()
        results = []
        # Each runtime is closed before construction of the next. No workers,
        # larger step budgets, warm source image, or parallel execution.
        for backend in ("python", "native"):
            results.append(_run(fixture, backend, engine_source))
        python, native = results
        equal_fields = (
            "geometry", "caller_maxima", "cells", "semantic_steps",
            "cell_sha256", "committed_frame_bytes", "revision",
            "final_frame_bytes", "final_frames_by_type",
            "final_semantic_cycles", "final_perf_cycles", "final_timer",
        )
        for name in equal_fields:
            if python[name] != native[name]:
                raise AssertionError(f"Python/native mismatch for {name}")
        for result in results:
            if result["geometry"] != [280, 2] or result["caller_maxima"] != [280, 84]:
                raise AssertionError("fixture geometry or caller bounds changed")
            if result["initial_pt_rows"]["count"] != 2:
                raise AssertionError("initial PT snapshot did not contain two rows")
        if python["initial_pt_rows"]["steps"] != native["initial_pt_rows"]["steps"]:
            raise AssertionError("initial PT rows consumed different semantic work")
        if native["native_after_feed"]["semantic_steps"] <= native["native_before_feed"]["semantic_steps"]:
            raise AssertionError("the native CELL exercise executed no native work")
        if any(_sha256(Path(path)) != digest for path, digest in sources.items()):
            raise RuntimeError("a bound source or native extension changed during the run")
        if bindings != {"megapad": _repository(ROOT), "akashic": _repository(akashic_root)}:
            raise RuntimeError("repository revisions/status changed during the run")
        report = {
            "schema": "megapad.semantic-cell-feed-comparison", "schema_version": 1,
            "generated_at": datetime.now(timezone.utc).isoformat(),
            "scope": "280x2 CELL fixture; no Desktop or physical presentation",
            "order": ["python", "native"], "bindings": bindings,
            "python": {"executable": sys.executable, "version": sys.version},
            "host": {"platform": platform.platform(), "machine": platform.machine()},
            "native_extension": {"path": str(extension_path), "sha256": _sha256(extension_path)},
            "source_sha256": sources,
            "generated_fixture_sha256": hashlib.sha256(fixture._fixture_source(280)).hexdigest(),
            "budgets": {"source_steps": fixture.SIMULATOR_SOURCE_MAX_STEPS,
                        "per_row_or_word_steps": fixture.ROW_STEP_BUDGET},
            "timing_boundaries": {
                "setup_seconds": "runtime construction, source compilation and CF-INIT; imports excluded",
                "attach_seconds": "host attachment, negotiation, initial PT snapshot and retained discovery",
                "initial_pt_rows": "sum of two CF-INITIAL-ROW calls inside attach; begin/commit excluded",
                "cell_feed_seconds": "existing fixture sum of two CF-ROW-WRITE calls; source, attach, begin, cursor, commit and presentation excluded",
                "total_fixture_seconds": "construction through close, including all above phases",
            },
            "timing_qualification": "one fixed-order sequential pair in one process; unprofiled, not a balanced pinned-host benchmark",
            "equivalent_fields": list(equal_fields) + ["initial_pt_rows.steps"],
            "results": results,
            "speedup": python["cell_feed_seconds"] / native["cell_feed_seconds"],
            "initial_snapshot_row_speedup": python["initial_pt_rows"]["seconds"] / native["initial_pt_rows"]["seconds"],
        }
        encoded = json.dumps(report, indent=2, sort_keys=True) + "\n"
        if args.output is not None:
            args.output.write_text(encoded)
        sys.stdout.write(encoded)
        return 0
    finally:
        for name, value in previous.items():
            if value is None:
                os.environ.pop(name, None)
            else:
                os.environ[name] = value


if __name__ == "__main__":
    raise SystemExit(main())
