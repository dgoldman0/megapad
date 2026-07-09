#!/usr/bin/env python3
"""Run a deterministic headless MegaPad development scenario."""

from __future__ import annotations

import argparse
import json
import sys
import time
from pathlib import Path

from session import MachineSession, RunReport


class ScenarioFailure(RuntimeError):
    pass


def resolve_path(base: Path, value: str | None) -> Path | None:
    if value is None:
        return None
    path = Path(value).expanduser()
    return path if path.is_absolute() else (base / path).resolve()


def report_record(index: int, action: dict, report: RunReport) -> dict:
    return {
        "index": index,
        "action": action.get("type"),
        **report.to_dict(),
    }


def run_scenario(path: Path) -> dict:
    path = path.expanduser().resolve()
    base = path.parent
    scenario = json.loads(path.read_text(encoding="utf-8"))
    machine = scenario.get("machine", {})
    bios = resolve_path(base, machine.get("bios"))
    if bios is None:
        raise ScenarioFailure("scenario machine.bios is required")
    storage = resolve_path(base, machine.get("storage"))

    started = time.perf_counter()
    action_reports: list[dict] = []
    captures: list[dict] = []

    with MachineSession.from_bios(
        bios,
        storage_image=storage,
        ram_size=int(machine.get("ram_kib", 1024)) << 10,
        ext_mem_size=int(machine.get("ext_mem_mib", 16)) << 20,
        vram_size=int(machine.get("vram_mib", 4)) << 20,
        num_cores=int(machine.get("cores", 1)),
        num_clusters=int(machine.get("clusters", 0)),
        cols=int(machine.get("cols", 80)),
        rows=int(machine.get("rows", 30)),
        batch_steps=int(machine.get("batch_steps", 100_000)),
    ) as session:
        session.boot(int(machine.get("entry", 0)))

        def fail(message: str):
            recent = session.raw_text()[-500:]
            if recent:
                message += f"\nrecent UART output:\n{recent}"
            raise ScenarioFailure(message)

        for index, action in enumerate(scenario.get("actions", []), start=1):
            kind = action.get("type")
            max_steps = int(action.get("max_steps", 10_000_000))
            wall_timeout = float(action.get("wall_timeout_s", 10.0))

            if kind == "run":
                report = session.run(
                    max_steps=max_steps,
                    wall_timeout_s=wall_timeout,
                )
                action_reports.append(report_record(index, action, report))
            elif kind == "wait_idle":
                report = session.wait_for_idle(
                    max_steps=max_steps,
                    wall_timeout_s=wall_timeout,
                )
                action_reports.append(report_record(index, action, report))
                if report.reason != "idle":
                    fail(
                        f"action {index}: expected idle, got {report.reason}"
                    )
            elif kind == "wait_text":
                expected = action.get("text")
                if not isinstance(expected, str):
                    raise ScenarioFailure(f"action {index}: wait_text requires text")
                report = session.wait_for_text(
                    expected,
                    scope=action.get("scope", "raw"),
                    max_steps=max_steps,
                    wall_timeout_s=wall_timeout,
                )
                action_reports.append(report_record(index, action, report))
                if not report.matched:
                    fail(
                        f"action {index}: text {expected!r} not found; "
                        f"stopped at {report.reason}"
                    )
            elif kind == "send_text":
                value = action.get("text")
                if not isinstance(value, str):
                    raise ScenarioFailure(f"action {index}: send_text requires text")
                session.send_text(value)
            elif kind == "send_key":
                value = action.get("key")
                if not isinstance(value, str):
                    raise ScenarioFailure(f"action {index}: send_key requires key")
                session.send_key(value)
            elif kind == "resize":
                session.resize(int(action["cols"]), int(action["rows"]))
            elif kind == "clear_output":
                session.clear_output()
            elif kind == "capture":
                capture_started = time.perf_counter()
                snapshot = session.snapshot()
                outputs: dict[str, str] = {}
                text_path = resolve_path(base, action.get("text"))
                json_path = resolve_path(base, action.get("json"))
                png_path = resolve_path(base, action.get("png"))
                font_path = resolve_path(base, action.get("font"))
                if text_path:
                    snapshot.write_text(text_path)
                    outputs["text"] = str(text_path)
                if json_path:
                    snapshot.write_json(json_path)
                    outputs["json"] = str(json_path)
                if png_path:
                    snapshot.write_png(
                        png_path,
                        font_path=font_path,
                        font_size=int(action.get("font_size", 16)),
                    )
                    outputs["png"] = str(png_path)
                captures.append({
                    "index": index,
                    "outputs": outputs,
                    "elapsed_s": time.perf_counter() - capture_started,
                    "cursor": [snapshot.cursor_row, snapshot.cursor_col],
                    "size": [snapshot.cols, snapshot.rows],
                })
            elif kind == "print_screen":
                print(session.screen_text(trim_right=True))
            else:
                raise ScenarioFailure(f"action {index}: unknown type {kind!r}")

        summary = {
            "scenario": scenario.get("name", path.stem),
            "scenario_path": str(path),
            "success": True,
            "elapsed_s": time.perf_counter() - started,
            "actions": action_reports,
            "captures": captures,
            "uart": {
                "bytes": len(session.raw_output),
                "batch_callbacks": session.output_batches,
                "byte_callbacks": session.output_byte_callbacks,
            },
            "terminal": {
                "cols": session.terminal.cols,
                "rows": session.terminal.rows,
                "cursor": [session.terminal.cy, session.terminal.cx],
            },
        }

        report_path = resolve_path(base, scenario.get("report"))
        if report_path:
            summary["report_path"] = str(report_path)
            report_path.parent.mkdir(parents=True, exist_ok=True)
            report_path.write_text(
                json.dumps(summary, indent=2) + "\n",
                encoding="utf-8",
            )
        return summary


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Run a headless MegaPad machine scenario"
    )
    parser.add_argument("scenario", type=Path, help="JSON scenario file")
    args = parser.parse_args()

    try:
        summary = run_scenario(args.scenario)
    except (OSError, ValueError, KeyError, json.JSONDecodeError,
            ScenarioFailure) as exc:
        print(f"scenario failed: {exc}", file=sys.stderr)
        return 2
    print(json.dumps(summary, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
