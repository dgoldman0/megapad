"""Consumer-level checks for namespaced harness and endpoint isolation."""

from __future__ import annotations

import json
import os
import shutil
import stat
import subprocess
import sys
import uuid
from pathlib import Path
from unittest.mock import patch

import pytest

import cli
import shared_session
from runtime_paths import (
    HeadlessStatusOwner,
    RUNTIME_NAMESPACE_ENV,
    RuntimeOwnershipLock,
    all_runtime_paths,
    read_headless_status,
    runtime_directory,
    shared_session_socket,
)
from shared_session import SessionClient, SessionServer


ROOT = Path(__file__).resolve().parents[1]


def _namespace(label: str) -> str:
    return f"{label}-{uuid.uuid4().hex[:12]}"


class TestRuntimeConsumers:
    def test_make_command_line_namespace_reaches_runtime_recipe(self):
        namespace = _namespace("make")
        environ = {RUNTIME_NAMESPACE_ENV: namespace}
        directory = Path(runtime_directory(environ))
        try:
            result = subprocess.run(
                [
                    "make",
                    "--no-print-directory",
                    f"{RUNTIME_NAMESPACE_ENV}={namespace}",
                    "runtime-paths",
                ],
                cwd=ROOT,
                check=True,
                capture_output=True,
                text=True,
            )
            assert json.loads(result.stdout) == all_runtime_paths(environ)
        finally:
            shutil.rmtree(directory)

    def test_shared_session_module_selects_namespaced_default(self):
        namespace = _namespace("session")
        environ = {RUNTIME_NAMESPACE_ENV: namespace}
        directory = Path(runtime_directory(environ))
        process_env = os.environ.copy()
        process_env[RUNTIME_NAMESPACE_ENV] = namespace
        try:
            result = subprocess.run(
                [
                    sys.executable,
                    "-c",
                    "import shared_session; print(shared_session.DEFAULT_SOCKET)",
                ],
                cwd=ROOT,
                env=process_env,
                check=True,
                capture_output=True,
                text=True,
            )
            assert result.stdout.strip() == shared_session_socket(environ)
        finally:
            shutil.rmtree(directory)

    @pytest.mark.parametrize("connect_args", [["--connect"], ["--connect", "auto"]])
    def test_cli_connect_without_endpoint_uses_discovery(
        self,
        tmp_path,
        connect_args,
    ):
        status_path = tmp_path / "headless.json"
        owner = HeadlessStatusOwner.claim(
            str(status_path),
            port=54321,
            pid=os.getpid(),
        )
        try:
            with (
                patch.object(cli, "_HEADLESS_STATUS", str(status_path)),
                patch.object(cli, "headless_connect") as connect,
                patch.object(sys, "argv", ["cli.py", *connect_args]),
            ):
                cli.main()
            connect.assert_called_once_with("localhost", 54321)
        finally:
            owner.release()

    def test_two_headless_servers_cannot_share_discovery_or_cross_cleanup(
        self,
        tmp_path,
    ):
        class FakeListener:
            next_port = 41000

            def __init__(self, *_args, **_kwargs):
                type(self).next_port += 1
                self.bound_port = type(self).next_port

            def setsockopt(self, *_args):
                pass

            def bind(self, address):
                assert address == ("0.0.0.0", 0)

            def getsockname(self):
                return ("0.0.0.0", self.bound_port)

            def listen(self, _backlog):
                pass

            def settimeout(self, _timeout):
                pass

            def close(self):
                pass

        status_path = str(tmp_path / "headless.json")
        first = cli.HeadlessServer(object(), port=0, status_path=status_path)
        second = cli.HeadlessServer(object(), port=0, status_path=status_path)
        try:
            with patch("socket.socket", side_effect=[FakeListener(), FakeListener()]):
                first._open_listener()
                second._open_listener()
            assert first.port > 0
            assert second.port > 0

            first._publish_discovery()
            assert read_headless_status(status_path)["port"] == first.port
            assert stat.S_IMODE(Path(status_path).stat().st_mode) == 0o600
            with pytest.raises(RuntimeError, match="live owner"):
                second._publish_discovery()

            replacement = tmp_path / "replacement.json"
            replacement.write_text(json.dumps({
                "pid": os.getpid(),
                "port": second.port,
                "owner": "replacement-owner",
            }))
            replacement.chmod(0o600)
            os.replace(replacement, status_path)
            assert first._remove_discovery() is False

            second._publish_discovery()
            assert read_headless_status(status_path)["port"] == second.port
            assert second._remove_discovery() is True
            assert not Path(status_path).exists()
            lock_path = Path(f"{status_path}.lock")
            assert lock_path.exists()
            assert stat.S_IMODE(lock_path.stat().st_mode) == 0o600
        finally:
            first._remove_discovery()
            second._remove_discovery()
            if first._srv is not None:
                first._srv.close()
            if second._srv is not None:
                second._srv.close()

    def test_headless_recovers_discovery_left_by_crashed_lock_owner(
        self,
        tmp_path,
    ):
        status_path = str(tmp_path / "headless.json")
        crashed = HeadlessStatusOwner.claim(status_path, port=41001)
        old_token = crashed.token
        crashed._ownership_lock.release()
        crashed._ownership_lock = None

        recovered = HeadlessStatusOwner.claim(status_path, port=41002)
        try:
            status = read_headless_status(status_path)
            assert status["port"] == 41002
            assert status["owner"] != old_token
        finally:
            recovered.release()

        assert Path(f"{status_path}.lock").exists()

    def test_headless_blocks_live_legacy_owner_and_recovers_dead_one(
        self,
        tmp_path,
    ):
        status_path = tmp_path / "headless.json"
        status_path.write_text(json.dumps({
            "pid": os.getpid(),
            "port": 41001,
        }))
        status_path.chmod(0o664)

        with pytest.raises(RuntimeError, match="live legacy owner"):
            HeadlessStatusOwner.claim(str(status_path), port=41002)
        assert json.loads(status_path.read_text())["port"] == 41001

        status_path.write_text(json.dumps({
            "pid": 2_147_483_647,
            "port": 41001,
        }))
        status_path.chmod(0o664)
        recovered = HeadlessStatusOwner.claim(str(status_path), port=41002)
        try:
            assert read_headless_status(str(status_path))["port"] == 41002
        finally:
            recovered.release()

    def test_session_socket_ownership_serializes_bind_and_preserves_replacement(
        self,
        tmp_path,
    ):
        class FakeUnixSocket:
            def __init__(self, *, live_probe=False):
                self.live_probe = live_probe

            def connect(self, _path):
                if not self.live_probe:
                    raise ConnectionRefusedError

            def bind(self, path):
                Path(path).write_text("bound")

            def setsockopt(self, *_args):
                pass

            def listen(self, _backlog):
                pass

            def settimeout(self, _timeout):
                pass

            def close(self):
                pass

        socket_path = tmp_path / "session.sock"
        first = SessionServer(object(), str(socket_path))
        second = SessionServer(object(), str(socket_path))
        factory = lambda *_args: FakeUnixSocket()
        validation = staticmethod(lambda _path, _info: None)
        with (
            patch.object(shared_session.socket, "socket", side_effect=factory),
            patch.object(SessionServer, "_validate_socket_path", validation),
        ):
            first._bind()
            first_identity = first._socket_identity
            with pytest.raises(RuntimeError, match="live owner"):
                second._bind()

            replacement = tmp_path / "replacement.sock"
            replacement.write_text("replacement")
            os.replace(replacement, socket_path)
            assert first._close_owned_listener() is False
            assert socket_path.read_text() == "replacement"
            assert first_identity != (
                socket_path.stat().st_dev,
                socket_path.stat().st_ino,
            )

            second._bind()
            assert socket_path.read_text() == "bound"
            assert second._close_owned_listener() is True
            assert not socket_path.exists()

        lock_path = Path(f"{socket_path}.lock")
        assert lock_path.exists()
        assert stat.S_IMODE(lock_path.stat().st_mode) == 0o600

    def test_session_socket_blocks_live_legacy_listener(self, tmp_path):
        class LiveProbe:
            def connect(self, _path):
                pass

            def close(self):
                pass

        socket_path = tmp_path / "session.sock"
        socket_path.write_text("legacy")
        server = SessionServer(object(), str(socket_path))
        validation = staticmethod(lambda _path, _info: None)
        with (
            patch.object(shared_session.socket, "socket", return_value=LiveProbe()),
            patch.object(SessionServer, "_validate_socket_path", validation),
        ):
            with pytest.raises(RuntimeError, match="already listening"):
                server._bind()

        assert socket_path.read_text() == "legacy"
        ownership = RuntimeOwnershipLock.acquire(str(socket_path))
        ownership.release()

    def test_explicit_headless_overrides_are_preserved(self, tmp_path):
        status_path = str(tmp_path / "custom-status.json")
        server = cli.HeadlessServer(
            object(),
            port=43210,
            status_path=status_path,
        )

        assert server.port == 43210
        assert server.status_path == status_path

    def test_explicit_session_socket_override_is_preserved(self, tmp_path):
        socket_path = str(tmp_path / "explicit.sock")

        client = SessionClient(socket_path)

        assert client.socket_path == socket_path
