"""
Pytest configuration for Megapad-64 test suite.

Always use the Makefile targets — they build the C++ accelerator,
run tests in the background, and provide a live dashboard:

    make test               # background + live dashboard (DEFAULT)
    make test-one K=X       # single test/class + monitoring
    make test-quick         # quick BIOS+CPU smoke test
    make test-status        # show progress

Running `python -m pytest` directly will fail.  No exceptions.
"""

import json
import os
import sys
import time
import pytest

STATUS_FILE = "/tmp/megapad_test_status.json"


def pytest_configure(config):
    """Register plugins, markers, and enforce Makefile usage."""
    # Register custom markers
    config.addinivalue_line("markers",
        "realnet: tests requiring a real TAP network device (deselected by default)")

    # --- Guard against raw `python -m pytest` without Make ---
    # All Makefile targets set MP64_VIA_MAKE=1.  Direct invocation
    # is unconditionally blocked — no escape hatch.
    if not os.environ.get("MP64_VIA_MAKE"):
        msg = (
            "\n"
            "═══════════════════════════════════════════════════════\n"
            "  ERROR: Do not run pytest directly.\n"
            "\n"
            "  Use the Makefile targets instead:\n"
            "\n"
            "    make test               # background + dashboard (~1 min)\n"
            "    make test-one K=TestFoo  # single test/class + monitoring\n"
            "    make test-quick          # quick BIOS+CPU smoke test\n"
            "═══════════════════════════════════════════════════════\n"
        )
        pytest.exit(msg, returncode=1)

    # Register live monitor plugin — but NOT on xdist workers.
    # With xdist (-n N), each worker gets its own pytest session.
    # If a worker writes status, it clobbers the controller's status
    # (e.g. marking "finished" when only that worker is done).
    # Workers are identified by having 'workerinput' on config.
    is_xdist_worker = hasattr(config, "workerinput")
    if not is_xdist_worker:
        monitor = LiveTestMonitor()
        config._live_monitor = monitor
        config.pluginmanager.register(monitor, "live_test_monitor")


class LiveTestMonitor:
    """Pytest plugin that writes live test status to a JSON file."""

    def __init__(self):
        self.start_time = time.time()
        self.total = 0
        self.passed = 0
        self.failed = 0
        self.skipped = 0
        self.errors = 0
        self.running = {}          # worker -> test nodeid
        self.failures = []         # list of {test, duration, message}
        self.completed_tests = []  # last N completed test names
        self.last_activity = time.time()
        self.finished = False
        self.exit_code = None
        self._max_completed_history = 20

    def _elapsed(self):
        return round(time.time() - self.start_time, 1)

    def _write_status(self):
        done = self.passed + self.failed + self.skipped + self.errors
        status = {
            "pid": os.getpid(),
            "start_time": self.start_time,
            "elapsed_s": self._elapsed(),
            "elapsed_human": _fmt_duration(self._elapsed()),
            "total": self.total,
            "done": done,
            "passed": self.passed,
            "failed": self.failed,
            "skipped": self.skipped,
            "errors": self.errors,
            "progress_pct": round(100.0 * done / self.total, 1) if self.total else 0,
            "running": dict(self.running),
            "idle_s": round(time.time() - self.last_activity, 1),
            "failures": self.failures[-20:],  # last 20 failures
            "recent_completed": self.completed_tests[-self._max_completed_history:],
            "finished": self.finished,
            "exit_code": self.exit_code,
        }
        try:
            tmp = STATUS_FILE + ".tmp"
            with open(tmp, "w") as f:
                json.dump(status, f, indent=2)
            os.replace(tmp, STATUS_FILE)
        except OSError:
            pass  # non-critical

    # --- pytest hooks ---

    @pytest.hookimpl(trylast=True)
    def pytest_collection_modifyitems(self, items):
        self.total = len(items)
        self._write_status()

    def pytest_deselected(self, items):
        # -k filtering deselects items after collection
        self.total -= len(items)
        self._write_status()

    def pytest_xdist_node_collection_finished(self, node, ids):
        """xdist: controller receives each worker's collected test ids.
        Use the first worker's count (all workers collect the same set)."""
        if self.total == 0:
            self.total = len(ids)
            self._write_status()

    @pytest.hookimpl(trylast=True)
    def pytest_runtest_logreport(self, report):
        self.last_activity = time.time()

        if report.when == "setup" and report.passed:
            worker = getattr(report, "node", None)
            wid = worker.gateway.id if worker else "main"
            self.running[wid] = report.nodeid

        if report.when == "call":
            worker = getattr(report, "node", None)
            wid = worker.gateway.id if worker else "main"
            self.running.pop(wid, None)

            if report.passed:
                self.passed += 1
            elif report.failed:
                self.failed += 1
                self.failures.append({
                    "test": report.nodeid,
                    "duration": round(report.duration, 2),
                    "message": _truncate(str(report.longrepr), 500),
                })
            elif report.skipped:
                self.skipped += 1
            self.completed_tests.append(report.nodeid)
            self._write_status()

        elif report.when == "setup" and report.failed:
            self.errors += 1
            self.failures.append({
                "test": report.nodeid,
                "duration": 0,
                "message": "SETUP ERROR: " + _truncate(str(report.longrepr), 500),
            })
            worker = getattr(report, "node", None)
            wid = worker.gateway.id if worker else "main"
            self.running.pop(wid, None)
            self._write_status()

    def pytest_sessionfinish(self, exitstatus):
        self.finished = True
        self.exit_code = int(exitstatus)
        self.running.clear()
        self._write_status()


def _truncate(s, maxlen):
    if len(s) <= maxlen:
        return s
    return s[:maxlen] + "... [truncated]"


def _fmt_duration(seconds):
    m, s = divmod(int(seconds), 60)
    h, m = divmod(m, 60)
    if h:
        return f"{h}h{m:02d}m{s:02d}s"
    elif m:
        return f"{m}m{s:02d}s"
    else:
        return f"{s}s"


