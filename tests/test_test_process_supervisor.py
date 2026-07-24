"""Focused lifecycle tests for Make's detached test-process supervisor."""

from __future__ import annotations

import json
import os
import signal
import stat
import subprocess
import sys
import time
from collections.abc import Callable
from pathlib import Path

import pytest

import test_process_supervisor as supervisor


ROOT = Path(__file__).resolve().parents[1]
SUPERVISOR = ROOT / "test_process_supervisor.py"


def _run_cli(*arguments: str, timeout: float = 8.0) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SUPERVISOR), *arguments],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
        timeout=timeout,
    )


def _wait_until(
    predicate: Callable[[], bool],
    *,
    timeout: float = 5.0,
) -> None:
    deadline = time.monotonic() + timeout
    while not predicate():
        if time.monotonic() >= deadline:
            pytest.fail("timed out waiting for supervised process state")
        time.sleep(0.02)


def _read_json_when_ready(path: Path) -> dict[str, object]:
    result: dict[str, object] | None = None

    def load() -> bool:
        nonlocal result
        try:
            result = json.loads(path.read_text())
        except (FileNotFoundError, json.JSONDecodeError):
            return False
        return True

    _wait_until(load)
    assert result is not None
    return result


def _process_is_live(pid: int) -> bool:
    try:
        raw = (Path("/proc") / str(pid) / "stat").read_text()
    except (FileNotFoundError, ProcessLookupError):
        return False
    close_paren = raw.rfind(")")
    return close_paren >= 0 and raw[close_paren + 2 :].split()[0] != "Z"


def _stop_if_owned(state_path: Path) -> None:
    if state_path.exists():
        _run_cli(
            "stop",
            "--state",
            str(state_path),
            "--grace-seconds",
            "0.1",
        )


def test_start_uses_private_session_and_cleans_state_on_natural_exit(tmp_path):
    state_path = tmp_path / "test.pid"
    output_path = tmp_path / "test-output.txt"
    status_path = tmp_path / "test-status.json"
    child_info_path = tmp_path / "child.json"
    status_path.write_text("stale status")
    child_code = """
import json
import os
import sys
import time
with open(sys.argv[1], "w", encoding="utf-8") as stream:
    json.dump(
        {"pid": os.getpid(), "pgid": os.getpgrp(), "sid": os.getsid(0)},
        stream,
    )
time.sleep(0.35)
"""

    started = _run_cli(
        "start",
        "--state",
        str(state_path),
        "--status",
        str(status_path),
        "--output",
        str(output_path),
        "--",
        sys.executable,
        "-c",
        child_code,
        str(child_info_path),
    )

    assert started.returncode == 0, started.stderr
    state = json.loads(state_path.read_text())
    assert isinstance(state["pid"], int) and state["pid"] > 0
    assert state["pid"] == state["pgid"] == state["sid"]
    assert isinstance(state["start_token"], str)
    assert ":" in state["start_token"]
    assert stat.S_IMODE(state_path.stat().st_mode) == 0o600
    assert not status_path.exists()

    child = _read_json_when_ready(child_info_path)
    assert child["pid"] != state["pid"]
    assert child["pgid"] == state["pid"]
    assert child["sid"] == state["pid"]

    _wait_until(lambda: not state_path.exists())
    _wait_until(lambda: not _process_is_live(int(state["pid"])))


def test_stop_terminates_only_owned_session_including_stubborn_descendants(
    tmp_path,
):
    state_path = tmp_path / "test.pid"
    output_path = tmp_path / "test-output.txt"
    tree_info_path = tmp_path / "tree.json"
    child_ready_path = tmp_path / "child-ready"
    child_code = """
import pathlib
import signal
import sys
import time
signal.signal(signal.SIGTERM, signal.SIG_IGN)
pathlib.Path(sys.argv[1]).write_text("ready")
time.sleep(60)
"""
    root_code = """
import json
import os
import signal
import subprocess
import sys
import time
from pathlib import Path
signal.signal(signal.SIGTERM, signal.SIG_IGN)
child = subprocess.Popen([sys.executable, "-c", sys.argv[3], sys.argv[2]])
deadline = time.monotonic() + 5
while not Path(sys.argv[2]).exists():
    if time.monotonic() >= deadline:
        raise RuntimeError("child did not become ready")
    time.sleep(0.01)
with open(sys.argv[1], "w", encoding="utf-8") as stream:
    json.dump({"pid": os.getpid(), "child_pid": child.pid}, stream)
time.sleep(60)
"""
    unrelated = subprocess.Popen(
        [sys.executable, "-c", "import time; time.sleep(60)"],
        start_new_session=True,
    )

    try:
        started = _run_cli(
            "start",
            "--state",
            str(state_path),
            "--output",
            str(output_path),
            "--",
            sys.executable,
            "-c",
            root_code,
            str(tree_info_path),
            str(child_ready_path),
            child_code,
        )
        assert started.returncode == 0, started.stderr
        state = json.loads(state_path.read_text())
        tree = _read_json_when_ready(tree_info_path)

        stopped = _run_cli(
            "stop",
            "--state",
            str(state_path),
            "--grace-seconds",
            "0.15",
        )

        assert stopped.returncode == 0, stopped.stderr
        assert "Terminated owned test process group" in stopped.stdout
        assert not state_path.exists()
        for pid in (int(state["pid"]), int(tree["pid"]), int(tree["child_pid"])):
            _wait_until(lambda pid=pid: not _process_is_live(pid))
        assert unrelated.poll() is None
    finally:
        _stop_if_owned(state_path)
        if unrelated.poll() is None:
            unrelated.terminate()
            try:
                unrelated.wait(timeout=2)
            except subprocess.TimeoutExpired:
                unrelated.kill()
                unrelated.wait(timeout=2)


def test_stop_refuses_reused_pid_identity_without_signaling_process(tmp_path):
    state_path = tmp_path / "test.pid"
    unrelated = subprocess.Popen(
        [sys.executable, "-c", "import time; time.sleep(60)"],
        start_new_session=True,
    )
    try:
        identity = supervisor._read_process_identity(unrelated.pid)
        boot_id, start_ticks = identity.start_token.split(":")
        state_path.write_text(
            json.dumps(
                {
                    "version": 1,
                    "pid": unrelated.pid,
                    "pgid": unrelated.pid,
                    "sid": unrelated.pid,
                    "start_token": f"{boot_id}:{int(start_ticks) + 1}",
                    "owner_token": "a" * 32,
                }
            )
        )
        state_path.chmod(0o600)

        stopped = _run_cli("stop", "--state", str(state_path))

        assert stopped.returncode == 0, stopped.stderr
        assert "Refused to signal stale test ownership state" in stopped.stdout
        assert "different process identity" in stopped.stdout
        assert unrelated.poll() is None
        assert not state_path.exists()
    finally:
        if unrelated.poll() is None:
            unrelated.terminate()
            unrelated.wait(timeout=2)


def test_stop_rejects_malformed_nonpositive_pid_without_signaling(tmp_path):
    state_path = tmp_path / "test.pid"
    state_path.write_text(
        json.dumps(
            {
                "version": 1,
                "pid": -1,
                "pgid": -1,
                "sid": -1,
                "start_token": "not-a-start-token",
                "owner_token": "not-an-owner-token",
            }
        )
    )
    state_path.chmod(0o600)
    unrelated = subprocess.Popen(
        [sys.executable, "-c", "import time; time.sleep(60)"],
        start_new_session=True,
    )
    try:
        stopped = _run_cli("stop", "--state", str(state_path))

        assert stopped.returncode == 2
        assert "Refused to signal from malformed" in stopped.stderr
        assert unrelated.poll() is None
        assert not state_path.exists()
    finally:
        unrelated.terminate()
        unrelated.wait(timeout=2)


def test_second_start_refuses_live_owner_and_preserves_original_state(tmp_path):
    state_path = tmp_path / "test.pid"
    output_path = tmp_path / "first-output.txt"
    second_output_path = tmp_path / "second-output.txt"

    try:
        first = _run_cli(
            "start",
            "--state",
            str(state_path),
            "--output",
            str(output_path),
            "--",
            sys.executable,
            "-c",
            "import time; time.sleep(60)",
        )
        assert first.returncode == 0, first.stderr
        original_state = state_path.read_bytes()

        second = _run_cli(
            "start",
            "--state",
            str(state_path),
            "--output",
            str(second_output_path),
            "--",
            sys.executable,
            "-c",
            "pass",
        )

        assert second.returncode == 1
        assert "tests already running" in second.stderr
        assert state_path.read_bytes() == original_state
        assert not second_output_path.exists()
    finally:
        _stop_if_owned(state_path)


@pytest.mark.parametrize(
    ("target", "verb"),
    [
        ("test-quick", "start"),
        ("test-one", "start"),
        ("test-bg", "start"),
        ("test-net", "start"),
        ("test-kill", "stop"),
    ],
)
def test_make_background_targets_delegate_ownership(target, verb):
    result = subprocess.run(
        ["make", "--no-print-directory", "-n", target, "K=SupervisorProbe"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )

    assert f"test_process_supervisor.py {verb}" in result.stdout
    if verb == "start":
        assert "--state \"$pid_file\"" in result.stdout
        assert "--status \"$status_file\"" in result.stdout
        assert "--output \"$output_file\"" in result.stdout
        assert "nohup " not in result.stdout


def test_plain_make_keeps_background_test_as_the_default_goal():
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "K=SupervisorDefaultProbe",
        ],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )

    assert "Starting tests in background (C++ accel)" in result.stdout
    assert "test_process_supervisor.py start" in result.stdout
