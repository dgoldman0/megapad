"""
Pytest configuration for Megapad-64 test suite.

Recommended: run tests with PyPy for ~5× speedup on CPU emulation.
    make test               # PyPy + 8 xdist workers (~4 min)
    make test-seq           # PyPy sequential (~8 min)
    make test-cpython       # CPython fallback (~40 min)

For parallel execution with xdist:
    pypy3 -m pytest -n 8            # 8 workers
    pypy3 -m pytest -n auto         # auto-detect CPU count

Without -n, tests run sequentially.

Live monitoring (with background runs):
    make test-bg            # start tests in background
    make test-status        # show live progress
    make test-watch         # auto-refresh every 5s
"""

import json
import os
import time
import pytest

STATUS_FILE = "/tmp/megapad_test_status.json"


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


# Register the plugin
def pytest_configure(config):
    monitor = LiveTestMonitor()
    config._live_monitor = monitor
    config.pluginmanager.register(monitor, "live_test_monitor")
