#!/usr/bin/env python3
"""
Live test monitor for Megapad-64 test suite.

Reads the status file written by the conftest.py LiveTestMonitor plugin
and displays a dashboard of test progress.

Usage:
    python test_monitor.py              # one-shot status
    python test_monitor.py --watch      # auto-refresh every 5s
    python test_monitor.py --watch 2    # auto-refresh every 2s
    python test_monitor.py --json       # raw JSON dump
    python test_monitor.py --failures   # show only failures
"""

import json
import os
import sys
import time

STATUS_FILE = "/tmp/megapad_test_status.json"

# ANSI colors
GREEN = "\033[32m"
RED = "\033[31m"
YELLOW = "\033[33m"
CYAN = "\033[36m"
BOLD = "\033[1m"
DIM = "\033[2m"
RESET = "\033[0m"

HANG_THRESHOLD_S = 120  # warn if a test runs > 2 min


def read_status():
    try:
        with open(STATUS_FILE) as f:
            return json.load(f)
    except FileNotFoundError:
        return None
    except json.JSONDecodeError:
        return None


def render_bar(done, total, width=40):
    if total == 0:
        return "[" + " " * width + "]"
    filled = int(width * done / total)
    return "[" + "=" * filled + ">" * min(1, width - filled) + " " * max(0, width - filled - 1) + "]"


def display(status, *, show_failures_only=False):
    if status is None:
        print(f"{RED}No test status file found.{RESET}")
        print(f"{DIM}Run 'make test-bg' to start tests with monitoring.{RESET}")
        return

    pid = status["pid"]
    alive = _pid_alive(pid)

    # Header
    if status["finished"]:
        ec = status["exit_code"]
        color = GREEN if ec == 0 else RED
        state = f"{color}{BOLD}FINISHED (exit {ec}){RESET}"
    elif alive:
        state = f"{CYAN}{BOLD}RUNNING{RESET}"
    else:
        state = f"{RED}{BOLD}DEAD (pid {pid} gone){RESET}"

    print(f"\n{BOLD}Megapad-64 Test Monitor{RESET}  {state}")
    print(f"{DIM}{'─' * 60}{RESET}")

    # Progress
    done = status["done"]
    total = status["total"]
    pct = status["progress_pct"]
    bar = render_bar(done, total)
    elapsed = status["elapsed_human"]
    print(f"  {bar} {pct:5.1f}%  ({done}/{total})  {DIM}{elapsed}{RESET}")

    # Counts
    p = status["passed"]
    f = status["failed"]
    s = status["skipped"]
    e = status["errors"]
    parts = [f"{GREEN}{p} passed{RESET}"]
    if f:
        parts.append(f"{RED}{f} failed{RESET}")
    if e:
        parts.append(f"{RED}{e} errors{RESET}")
    if s:
        parts.append(f"{YELLOW}{s} skipped{RESET}")
    print(f"  {', '.join(parts)}")

    # ETA
    if not status["finished"] and done > 0 and total > done:
        rate = done / status["elapsed_s"]
        remaining = (total - done) / rate
        print(f"  {DIM}ETA: ~{_fmt_duration(remaining)}{RESET}")

    # Currently running
    running = status.get("running", {})
    if running and not show_failures_only:
        print(f"\n  {BOLD}Currently running:{RESET}")
        idle = status.get("idle_s", 0)
        for wid, nodeid in sorted(running.items()):
            short = _short_nodeid(nodeid)
            warn = ""
            if idle > HANG_THRESHOLD_S:
                warn = f"  {RED}⚠ idle {_fmt_duration(idle)} — possible hang!{RESET}"
            print(f"    {DIM}[{wid}]{RESET} {short}{warn}")

    # Failures
    failures = status.get("failures", [])
    if failures:
        print(f"\n  {RED}{BOLD}Failures:{RESET}")
        for fail in failures:
            test = _short_nodeid(fail["test"])
            dur = fail.get("duration", 0)
            print(f"    {RED}✗{RESET} {test} ({dur}s)")
            if not show_failures_only:
                msg = fail.get("message", "")
                # show first 3 lines of message
                for line in msg.split("\n")[:3]:
                    print(f"      {DIM}{line}{RESET}")

    # Recent completed
    recent = status.get("recent_completed", [])
    if recent and not show_failures_only and not status["finished"]:
        print(f"\n  {DIM}Recently completed:{RESET}")
        for nodeid in recent[-5:]:
            print(f"    {DIM}✓ {_short_nodeid(nodeid)}{RESET}")

    print()


def _short_nodeid(nodeid):
    """Shorten test_system.py::TestFoo::test_bar to TestFoo::test_bar"""
    if "::" in nodeid:
        parts = nodeid.split("::")
        return "::".join(parts[1:])
    return nodeid


def _pid_alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except (ProcessLookupError, PermissionError):
        return False


def _fmt_duration(seconds):
    m, s = divmod(int(seconds), 60)
    h, m = divmod(m, 60)
    if h:
        return f"{h}h{m:02d}m{s:02d}s"
    elif m:
        return f"{m}m{s:02d}s"
    else:
        return f"{s}s"


def main():
    args = sys.argv[1:]

    if "--json" in args:
        status = read_status()
        if status:
            print(json.dumps(status, indent=2))
        else:
            print("{}")
        return

    show_failures = "--failures" in args

    if "--watch" in args:
        idx = args.index("--watch")
        interval = float(args[idx + 1]) if idx + 1 < len(args) and args[idx + 1].replace(".", "").isdigit() else 5
        try:
            while True:
                os.system("clear")
                status = read_status()
                display(status, show_failures_only=show_failures)
                if status and status.get("finished"):
                    print(f"{DIM}Tests finished. Exiting watch mode.{RESET}")
                    break
                time.sleep(interval)
        except KeyboardInterrupt:
            print()
    else:
        status = read_status()
        display(status, show_failures_only=show_failures)


if __name__ == "__main__":
    main()
