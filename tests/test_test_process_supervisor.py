"""Focused lifecycle tests for Make's detached test-process supervisor."""

from __future__ import annotations

import json
import os
import re
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


def _assert_pytest_commands_are_sequential(output: str) -> None:
    logical_output = output.replace("\\\n", " ")
    commands = re.findall(
        r"env MP64_VIA_MAKE=1\b[^;]*",
        logical_output,
    )
    for command in commands:
        if "pytest" in command:
            assert not re.search(
                r"(?:^|\s)-n(?:\s+|=)?\S+",
                command,
            )
            assert "--dist" not in command
            assert "--numprocesses" not in command
            assert "PYTEST_ADDOPTS=" in command
            assert "-o addopts=" in command


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

        foreground = _run_cli(
            "foreground",
            "--state",
            str(state_path),
            "--",
            sys.executable,
            "-c",
            "pass",
        )

        assert foreground.returncode == 1
        assert "tests already running" in foreground.stderr
        assert state_path.read_bytes() == original_state
    finally:
        _stop_if_owned(state_path)


def test_foreground_claim_blocks_background_start_until_completion(tmp_path):
    state_path = tmp_path / "test.pid"
    status_path = tmp_path / "test-status.json"
    output_path = tmp_path / "background-output.txt"
    ready_path = tmp_path / "foreground-ready"
    release_path = tmp_path / "foreground-release"
    status_path.write_text("stale status")
    foreground_code = """
import pathlib
import sys
import time
ready = pathlib.Path(sys.argv[1])
release = pathlib.Path(sys.argv[2])
ready.write_text("ready")
while not release.exists():
    time.sleep(0.01)
"""
    foreground_process = subprocess.Popen(
        [
            sys.executable,
            str(SUPERVISOR),
            "foreground",
            "--state",
            str(state_path),
            "--status",
            str(status_path),
            "--",
            sys.executable,
            "-c",
            foreground_code,
            str(ready_path),
            str(release_path),
        ],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    try:
        _wait_until(ready_path.exists)
        assert not state_path.exists()
        assert not status_path.exists()

        background = _run_cli(
            "start",
            "--state",
            str(state_path),
            "--output",
            str(output_path),
            "--",
            sys.executable,
            "-c",
            "pass",
        )

        assert background.returncode == 1
        assert "tests already running" in background.stderr
        assert not output_path.exists()

        release_path.write_text("release")
        _, stderr = foreground_process.communicate(timeout=3)
        assert foreground_process.returncode == 0, stderr
        assert not state_path.exists()
    finally:
        _stop_if_owned(state_path)
        if foreground_process.poll() is None:
            release_path.write_text("release")
            try:
                foreground_process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                foreground_process.terminate()
                foreground_process.wait(timeout=2)


def test_foreground_exec_preserves_status_and_releases_lock_on_signal(
    tmp_path,
):
    state_path = tmp_path / "test.pid"
    ready_path = tmp_path / "foreground-ready"
    foreground_process = subprocess.Popen(
        [
            sys.executable,
            str(SUPERVISOR),
            "foreground",
            "--state",
            str(state_path),
            "--",
            sys.executable,
            "-c",
            (
                "import pathlib, sys, time; "
                "pathlib.Path(sys.argv[1]).write_text('ready'); "
                "time.sleep(60)"
            ),
            str(ready_path),
        ],
        cwd=ROOT,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )

    try:
        _wait_until(ready_path.exists)
        assert not state_path.exists()

        foreground_process.terminate()
        foreground_process.wait(timeout=2)
        assert foreground_process.returncode == -signal.SIGTERM

        followup = _run_cli(
            "foreground",
            "--state",
            str(state_path),
            "--",
            sys.executable,
            "-c",
            "raise SystemExit(7)",
        )

        assert followup.returncode == 7
        assert not state_path.exists()
    finally:
        if foreground_process.poll() is None:
            foreground_process.kill()
            foreground_process.wait(timeout=2)


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
    _assert_pytest_commands_are_sequential(result.stdout)


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
    _assert_pytest_commands_are_sequential(result.stdout)


def test_make_sequential_target_uses_owned_foreground_pytest_process():
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "test-sequential",
            "TEST_PATH=tests/test_test_process_supervisor.py",
            "VENV_PY=python3",
        ],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )

    assert "MP64_VIA_MAKE=1 PYTEST_ADDOPTS=" in result.stdout
    assert "python3 -m pytest" in result.stdout
    assert "tests/test_test_process_supervisor.py" in result.stdout
    assert "exec python3 test_process_supervisor.py foreground" in result.stdout
    assert "--state \"$pid_file\"" in result.stdout
    assert "--status \"$status_file\"" in result.stdout
    _assert_pytest_commands_are_sequential(result.stdout)


def test_make_sanitizer_runner_is_isolated_and_sequential():
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "_test-sanitize-run",
            "SANITIZER=thread",
            "SANITIZE_TEST_PATHS=tests/test_phase3_worker_pool.py",
            "VENV_PY=python3",
        ],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )

    assert "MP64_ACCEL_SANITIZER=\"thread\"" in result.stdout
    assert "--build-temp \"$sanitizer_temp\"" in result.stdout
    assert "--build-lib \"$sanitizer_lib\"" in result.stdout
    assert "--inplace" not in result.stdout
    assert "libtsan.so" in result.stdout
    assert "libstdc++.so" in result.stdout
    assert "setarch \"$sanitizer_arch\" -R true" in result.stdout
    assert 'sanitizer_launcher="setarch $sanitizer_arch -R"' in result.stdout
    assert 'sanitizer_preload="$preload_runtime' in result.stdout
    assert 'env LD_PRELOAD="$sanitizer_preload"' in result.stdout
    assert "PYTHONSAFEPATH=1" in result.stdout
    assert "PYTHONPATH=\"$sanitizer_lib:" in result.stdout
    assert "python3 -P -m pytest" in result.stdout
    assert "tests/test_phase3_worker_pool.py" in result.stdout
    assert "-p no:xdist" in result.stdout
    _assert_pytest_commands_are_sequential(result.stdout)


def test_public_sanitizer_target_rejects_uninstrumented_mode():
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "test-sanitize",
            "SANITIZER=none",
        ],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 2
    assert (
        "SANITIZER must be address-undefined or thread"
        in result.stderr
    )
    assert "running build_ext" not in result.stdout
