"""Runtime artifact isolation for parallel MegaPad checkouts."""

import os
import stat

import pytest

import runtime_paths
from runtime_paths import (
    RUNTIME_NAMESPACE_ENV,
    RuntimeOwnershipLock,
    default_headless_port,
    headless_status_path,
    runtime_namespace,
    shared_session_socket,
    test_output_path as runtime_test_output_path,
    test_pid_path as runtime_test_pid_path,
    test_status_path as runtime_test_status_path,
)


class TestRuntimePaths:
    def test_unset_namespace_preserves_legacy_defaults(self):
        environ = {}

        assert runtime_namespace(environ) is None
        assert runtime_test_status_path(environ) == "/tmp/megapad_test_status.json"
        assert runtime_test_pid_path(environ) == "/tmp/megapad_test_pid.txt"
        assert runtime_test_output_path(environ) == "/tmp/megapad_test_output.txt"
        assert shared_session_socket(environ, uid=123) == (
            "/tmp/megapad-session-123.sock"
        )
        assert headless_status_path(environ) == "/tmp/megapad_headless.json"
        assert default_headless_port(environ) == 6464

    def test_namespace_isolates_every_runtime_artifact(
        self,
        monkeypatch,
        tmp_path,
    ):
        environ = {RUNTIME_NAMESPACE_ENV: "megapad-concurrency"}
        monkeypatch.setattr(runtime_paths, "_TMP_ROOT", str(tmp_path))
        runtime_dir = (
            tmp_path
            / f"megapad-runtime-{os.getuid()}-megapad-concurrency"
        )

        assert runtime_namespace(environ) == "megapad-concurrency"
        assert runtime_test_status_path(environ) == str(
            runtime_dir / "test-status.json"
        )
        assert runtime_test_pid_path(environ) == str(runtime_dir / "test.pid")
        assert runtime_test_output_path(environ) == str(
            runtime_dir / "test-output.txt"
        )
        assert shared_session_socket(environ) == str(runtime_dir / "session.sock")
        assert headless_status_path(environ) == str(runtime_dir / "headless.json")
        assert default_headless_port(environ) == 0
        info = runtime_dir.stat()
        assert stat.S_IMODE(info.st_mode) == 0o700
        assert info.st_uid == os.getuid()

    def test_namespaced_runtime_rejects_unsafe_existing_directory(
        self,
        monkeypatch,
        tmp_path,
    ):
        namespace = "unsafe-mode"
        environ = {RUNTIME_NAMESPACE_ENV: namespace}
        monkeypatch.setattr(runtime_paths, "_TMP_ROOT", str(tmp_path))
        runtime_dir = tmp_path / f"megapad-runtime-{os.getuid()}-{namespace}"
        runtime_dir.mkdir(mode=0o755)
        runtime_dir.chmod(0o755)

        with pytest.raises(RuntimeError, match="expected 0700"):
            runtime_test_status_path(environ)

    def test_namespaced_runtime_rejects_preexisting_symlink(
        self,
        monkeypatch,
        tmp_path,
    ):
        namespace = "unsafe-link"
        environ = {RUNTIME_NAMESPACE_ENV: namespace}
        monkeypatch.setattr(runtime_paths, "_TMP_ROOT", str(tmp_path))
        target = tmp_path / "target"
        target.mkdir()
        runtime_dir = tmp_path / f"megapad-runtime-{os.getuid()}-{namespace}"
        runtime_dir.symlink_to(target, target_is_directory=True)

        with pytest.raises(RuntimeError, match="not a directory"):
            runtime_test_status_path(environ)

    def test_runtime_lock_rejects_unsafe_preexisting_mode(self, tmp_path):
        resource = tmp_path / "resource"
        lock_path = tmp_path / "resource.lock"
        lock_path.write_text("")
        lock_path.chmod(0o644)

        with pytest.raises(RuntimeError, match="expected 0600"):
            RuntimeOwnershipLock.acquire(str(resource))

    @pytest.mark.parametrize(
        "namespace",
        [
            "-leading-dash",
            "contains/slash",
            "contains space",
            "x" * 33,
        ],
    )
    def test_unsafe_namespace_is_rejected(self, namespace):
        with pytest.raises(ValueError, match=RUNTIME_NAMESPACE_ENV):
            runtime_namespace({RUNTIME_NAMESPACE_ENV: namespace})
