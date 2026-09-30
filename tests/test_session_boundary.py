"""The terminal frontend and hosted owner work without emulator imports."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from textwrap import dedent

import pytest


ROOT = Path(__file__).resolve().parents[1]
BLOCKED_MODULES = (
    "emulator",
    "session",
    "system",
    "devices",
    "megapad64",
    "accel_wrapper",
    "asm",
    "_mp64_accel",
    "_megaforth_native",
    "pygame",
)


def _without_emulator(
    source: str, *arguments: str,
) -> subprocess.CompletedProcess[str]:
    # A fresh interpreter matters: a warmed pytest process can hide a forbidden
    # dependency in sys.modules before the import blocker gets a chance to run.
    guard = f"""
import importlib.abc
import sys

class BlockImports(importlib.abc.MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split('.')[0] in {BLOCKED_MODULES!r}:
            raise AssertionError('unexpected frontend dependency: ' + fullname)

sys.meta_path.insert(0, BlockImports())
"""
    return subprocess.run(
        [sys.executable, "-c", dedent(guard) + dedent(source), *arguments],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=20,
    )


@pytest.mark.parametrize(
    ("module_name", "help_option"),
    [("simulator.server", "--executor"), ("session_viewer", "--fps")],
)
def test_hosted_server_and_viewer_help_have_no_emulator_dependency(
    module_name, help_option
):
    result = _without_emulator(
        f"""
        import importlib
        from shared.session import (
            RichTerminalSessionConfig,
            TerminalDisplayOffer,
            TerminalSession,
            TerminalSnapshot,
        )
        from shared_session import SessionClient, SessionServer, SharedSessionOwner

        sys.argv = [{module_name!r}, '--help']
        importlib.import_module({module_name!r}).main()
        """
    )
    assert result.returncode == 0, result.stderr
    assert help_option in result.stdout


@pytest.mark.parametrize("transport", ["direct", "socket"])
def test_python_session_runs_and_releases_ownership_without_emulator(transport):
    result = _without_emulator(
        r'''
        import tempfile
        import time
        from contextlib import contextmanager
        from pathlib import Path

        from shared.session import TerminalSession
        from shared_session import (
            SessionClient,
            SessionServer,
            SharedSessionOwner,
            snapshot_from_wire,
        )
        from simulator.runtime import MegaForthRuntime
        from simulator.session import SimulatorMachineSession, SimulatorSharedMachine

        transport = sys.argv[1]

        def start(server):
            if transport == "direct":
                server.machine.start()
            else:
                try:
                    server.serve_in_thread()
                except PermissionError:
                    # Other failures remain errors. Some execution sandboxes
                    # prohibit AF_UNIX even for a caller-owned temporary path.
                    raise SystemExit(77)

        class DirectClient:
            def __init__(self, server):
                self.server = server

            def request(self, method, **params):
                return self.server.dispatch(method, params)

        @contextmanager
        def connect(server):
            if transport == "direct":
                yield DirectClient(server)
            else:
                with SessionClient(server.socket_path) as client:
                    yield client

        runtime = MegaForthRuntime(execution_backend="python")
        runtime.evaluate(b': BOUNDARY-ROOT ." ready>" KEY EMIT ;')
        session = SimulatorMachineSession(runtime, "BOUNDARY-ROOT", cols=20, rows=3)
        backend = session.backend
        machine = SimulatorSharedMachine(session)
        assert isinstance(session, TerminalSession)
        assert isinstance(machine, SharedSessionOwner)
        machine.paused = True

        with tempfile.TemporaryDirectory(prefix="mp64-boundary-") as directory:
            socket_path = Path(directory) / "session.sock"
            server = SessionServer(machine, str(socket_path))
            try:
                start(server)
                with connect(server) as client:
                    initial = client.request("status")
                    assert initial["backend"] == "simulator"
                    assert initial["semantic_execution"]["backend"] == "python"
                    assert initial["state"] == "paused"
                    assert initial["simulator"]["booted"]
                    assert initial["steps"] == 0
                    if transport == "socket":
                        assert initial["clients"] == 1
                    assert initial["terminal"] == [20, 3]
                    assert not any(key in initial for key in ("cpu", "clock", "nic"))
                    generation = initial["generation"]

                    stepped = client.request("step", count=1)
                    assert stepped["boundaries"] == 1
                    assert stepped["semantic_steps"] > 0
                    assert stepped["stop_reason"] == "idle"
                    assert stepped["status"]["idle"]
                    assert stepped["status"]["paused"]
                    assert "cycles" not in stepped
                    assert client.request("raw", since=0)["text"] == "ready>"
                    assert client.request("text")["text"].splitlines()[0] == "ready>"
                    screen = client.request("screen", since=-1)
                    assert snapshot_from_wire(screen["snapshot"]).lines()[0].rstrip() == "ready>"

                    rejected = client.request(
                        "send_text", text="!", generation=generation - 1,
                    )
                    assert rejected == {"status": "stale_generation", "accepted_bytes": 0}
                    accepted = client.request(
                        "send_text", text="K", generation=generation,
                    )
                    assert accepted == {"status": "progress", "accepted_bytes": 1}
                    assert not client.request("resume")["paused"]
                    deadline = time.monotonic() + 3
                    while True:
                        finished = client.request("status")
                        assert finished["error"] is None, finished["error"]
                        if finished["halted"]:
                            break
                        assert time.monotonic() < deadline, finished
                        time.sleep(0.005)
                    assert finished["state"] == "halted"
                    assert finished["steps"] > stepped["semantic_steps"]
                    assert client.request("raw", since=0)["text"] == "ready>K"
                    assert client.request("text")["text"].splitlines()[0] == "ready>K"
            finally:
                server.stop()

            assert backend.closed
            assert not socket_path.exists()

            # Reusing the runtime catches leaked ownership beyond merely
            # observing a close flag. Socket mode also reuses the same listener
            # path, catching leaked socket ownership locks.
            runtime.evaluate(b': NEXT-ROOT 88 EMIT ;')
            replacement = SimulatorMachineSession(runtime, "NEXT-ROOT", cols=20, rows=3)
            successor = SimulatorSharedMachine(replacement)
            successor.paused = True
            second_server = SessionServer(successor, str(socket_path))
            try:
                start(second_server)
                with connect(second_server) as client:
                    assert client.request("step", count=1)["stop_reason"] == "completed"
                    assert client.request("raw", since=0)["text"] == "X"
            finally:
                second_server.stop()
            assert not socket_path.exists()
        ''',
        transport,
    )
    if transport == "socket" and result.returncode == 77:
        pytest.skip("Unix sockets are unavailable in this sandbox")
    assert result.returncode == 0, result.stderr
