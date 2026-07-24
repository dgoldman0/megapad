#!/usr/bin/env python3
"""Resolve process-local runtime artifacts for parallel MegaPad checkouts.

Set ``MP64_RUNTIME_NAMESPACE`` to give a checkout its own test monitor files,
shared-session socket, and headless-server discovery file.  Leaving it unset
preserves all historical paths and the historical headless TCP port.
"""

from __future__ import annotations

import argparse
import fcntl
import json
import os
import re
import secrets
import stat
from collections.abc import Mapping
from pathlib import Path
from typing import Any


RUNTIME_NAMESPACE_ENV = "MP64_RUNTIME_NAMESPACE"
_NAMESPACE_RE = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,31}\Z")
_TMP_ROOT = "/tmp"
_MAX_DISCOVERY_BYTES = 16 * 1024


def runtime_namespace(environ: Mapping[str, str] | None = None) -> str | None:
    """Return the validated optional runtime namespace."""
    environ = os.environ if environ is None else environ
    value = environ.get(RUNTIME_NAMESPACE_ENV, "")
    if not value:
        return None
    if not _NAMESPACE_RE.fullmatch(value):
        raise ValueError(
            f"{RUNTIME_NAMESPACE_ENV} must be 1-32 characters using only "
            "ASCII letters, digits, '.', '_' or '-', and must start with "
            "a letter or digit"
        )
    return value


def runtime_directory(
    environ: Mapping[str, str] | None = None,
    *,
    uid: int | None = None,
) -> str:
    """Return and securely create the private directory for a namespace."""
    namespace = runtime_namespace(environ)
    if namespace is None:
        raise ValueError("a runtime directory requires MP64_RUNTIME_NAMESPACE")

    path = Path(_TMP_ROOT) / (
        f"megapad-runtime-{os.getuid() if uid is None else int(uid)}-{namespace}"
    )
    try:
        os.mkdir(path, 0o700)
    except FileExistsError:
        pass
    else:
        os.chmod(path, 0o700)

    info = os.lstat(path)
    mode = stat.S_IMODE(info.st_mode)
    if not stat.S_ISDIR(info.st_mode):
        raise RuntimeError(f"unsafe MegaPad runtime path is not a directory: {path}")
    if info.st_uid != os.getuid():
        raise RuntimeError(
            f"unsafe MegaPad runtime directory is owned by uid {info.st_uid}, "
            f"expected {os.getuid()}: {path}"
        )
    if mode != 0o700:
        raise RuntimeError(
            f"unsafe MegaPad runtime directory mode is {mode:04o}, "
            f"expected 0700: {path}"
        )
    return str(path)


def _artifact_path(
    legacy_path: str,
    namespaced_name: str,
    environ: Mapping[str, str] | None = None,
    *,
    uid: int | None = None,
) -> str:
    if runtime_namespace(environ) is None:
        return legacy_path
    return str(Path(runtime_directory(environ, uid=uid)) / namespaced_name)


def test_status_path(environ: Mapping[str, str] | None = None) -> str:
    return _artifact_path(
        "/tmp/megapad_test_status.json", "test-status.json", environ
    )


def test_pid_path(environ: Mapping[str, str] | None = None) -> str:
    return _artifact_path("/tmp/megapad_test_pid.txt", "test.pid", environ)


def test_output_path(environ: Mapping[str, str] | None = None) -> str:
    return _artifact_path(
        "/tmp/megapad_test_output.txt", "test-output.txt", environ
    )


def shared_session_socket(
    environ: Mapping[str, str] | None = None,
    *,
    uid: int | None = None,
) -> str:
    owner = os.getuid() if uid is None else int(uid)
    return _artifact_path(
        f"/tmp/megapad-session-{owner}.sock",
        "session.sock",
        environ,
        uid=owner,
    )


def headless_status_path(environ: Mapping[str, str] | None = None) -> str:
    return _artifact_path(
        "/tmp/megapad_headless.json", "headless.json", environ
    )


def default_headless_port(environ: Mapping[str, str] | None = None) -> int:
    """Use the legacy port normally and an ephemeral port when namespaced."""
    return 0 if runtime_namespace(environ) else 6464


def all_runtime_paths(
    environ: Mapping[str, str] | None = None,
) -> dict[str, str | int]:
    """Return every resolved artifact for diagnostics and Make integration."""
    return {
        "test_status": test_status_path(environ),
        "test_pid": test_pid_path(environ),
        "test_output": test_output_path(environ),
        "session_socket": shared_session_socket(environ),
        "headless_status": headless_status_path(environ),
        "headless_port": default_headless_port(environ),
    }


def _read_discovery(
    path: str,
    *,
    allow_legacy_permissions: bool = False,
) -> tuple[dict[str, Any], os.stat_result]:
    flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0)
    flags |= getattr(os, "O_NOFOLLOW", 0)
    descriptor = os.open(path, flags)
    try:
        info = os.fstat(descriptor)
        if not stat.S_ISREG(info.st_mode):
            raise RuntimeError(f"headless discovery is not a regular file: {path}")
        if info.st_uid != os.getuid():
            raise RuntimeError(
                f"headless discovery is owned by uid {info.st_uid}, "
                f"expected {os.getuid()}: {path}"
            )
        if (
            not allow_legacy_permissions
            and stat.S_IMODE(info.st_mode) & 0o022
        ):
            raise RuntimeError(
                f"headless discovery is writable by another user: {path}"
            )
        chunks = []
        size = 0
        while size <= _MAX_DISCOVERY_BYTES:
            chunk = os.read(
                descriptor,
                min(4096, _MAX_DISCOVERY_BYTES + 1 - size),
            )
            if not chunk:
                break
            chunks.append(chunk)
            size += len(chunk)
        data = b"".join(chunks)
        if len(data) > _MAX_DISCOVERY_BYTES:
            raise RuntimeError(f"headless discovery is too large: {path}")
    finally:
        os.close(descriptor)

    try:
        payload = json.loads(data)
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise RuntimeError(f"invalid headless discovery JSON: {path}") from exc
    if not isinstance(payload, dict):
        raise RuntimeError(f"invalid headless discovery payload: {path}")
    pid = payload.get("pid")
    port = payload.get("port")
    if isinstance(pid, bool) or not isinstance(pid, int) or pid <= 0:
        raise RuntimeError(f"invalid headless discovery pid: {path}")
    if isinstance(port, bool) or not isinstance(port, int) or not 1 <= port <= 65535:
        raise RuntimeError(f"invalid headless discovery port: {path}")
    return payload, info


def read_headless_status(path: str | None = None) -> dict[str, Any]:
    """Safely read and validate a headless discovery document."""
    resolved = headless_status_path() if path is None else str(path)
    payload, _ = _read_discovery(resolved)
    return payload


class RuntimeOwnershipLock:
    """A permanent sidecar inode with an exclusive lock held by one owner."""

    def __init__(self, path: str, descriptor: int):
        self.path = path
        self._descriptor = descriptor

    @classmethod
    def acquire(cls, resource_path: str) -> "RuntimeOwnershipLock":
        lock_path = f"{resource_path}.lock"
        common_flags = os.O_RDWR | getattr(os, "O_CLOEXEC", 0)
        common_flags |= getattr(os, "O_NOFOLLOW", 0)
        created = False
        try:
            descriptor = os.open(
                lock_path,
                common_flags | os.O_CREAT | os.O_EXCL,
                0o600,
            )
            created = True
        except FileExistsError:
            descriptor = os.open(lock_path, common_flags)

        try:
            if created:
                os.fchmod(descriptor, 0o600)
            info = os.fstat(descriptor)
            mode = stat.S_IMODE(info.st_mode)
            if not stat.S_ISREG(info.st_mode):
                raise RuntimeError(
                    f"runtime ownership lock is not a regular file: {lock_path}"
                )
            if info.st_uid != os.getuid():
                raise RuntimeError(
                    f"runtime ownership lock is owned by uid {info.st_uid}, "
                    f"expected {os.getuid()}: {lock_path}"
                )
            if mode != 0o600:
                raise RuntimeError(
                    f"runtime ownership lock mode is {mode:04o}, "
                    f"expected 0600: {lock_path}"
                )
            try:
                fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except BlockingIOError as exc:
                raise RuntimeError(
                    f"runtime resource already has a live owner: {resource_path}"
                ) from exc

            current = os.stat(lock_path, follow_symlinks=False)
            if (current.st_dev, current.st_ino) != (info.st_dev, info.st_ino):
                fcntl.flock(descriptor, fcntl.LOCK_UN)
                raise RuntimeError(
                    f"runtime ownership lock changed during acquisition: {lock_path}"
                )
        except Exception:
            os.close(descriptor)
            raise
        return cls(lock_path, descriptor)

    @property
    def held(self) -> bool:
        return self._descriptor >= 0

    def release(self) -> None:
        descriptor = self._descriptor
        if descriptor < 0:
            return
        self._descriptor = -1
        try:
            fcntl.flock(descriptor, fcntl.LOCK_UN)
        finally:
            os.close(descriptor)

    def __del__(self):
        try:
            self.release()
        except Exception:
            pass


def _pid_is_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _unlink_matching_file(path: str, info: os.stat_result) -> bool:
    """Unlink path only when it still names the inspected inode."""
    try:
        current = os.stat(path, follow_symlinks=False)
    except FileNotFoundError:
        return False
    if (current.st_dev, current.st_ino) != (info.st_dev, info.st_ino):
        return False
    os.unlink(path)
    return True


class HeadlessStatusOwner:
    """A lock-held, token-checked claim on one discovery document."""

    def __init__(
        self,
        path: str,
        token: str,
        file_identity: tuple[int, int],
        ownership_lock: RuntimeOwnershipLock,
    ):
        self.path = path
        self.token = token
        self.file_identity = file_identity
        self._ownership_lock: RuntimeOwnershipLock | None = ownership_lock

    @classmethod
    def claim(
        cls,
        path: str,
        *,
        port: int,
        pid: int | None = None,
    ) -> "HeadlessStatusOwner":
        """Atomically publish a complete status file without replacing one."""
        process_id = os.getpid() if pid is None else int(pid)
        if process_id <= 0:
            raise ValueError("headless discovery pid must be positive")
        if isinstance(port, bool) or not isinstance(port, int):
            raise ValueError("headless discovery port must be an integer")
        if not 1 <= port <= 65535:
            raise ValueError("headless discovery port must be in 1..65535")

        token = secrets.token_hex(16)
        resolved = str(path)
        ownership_lock = RuntimeOwnershipLock.acquire(resolved)
        temporary = f"{resolved}.{token}.tmp"
        payload = json.dumps(
            {"pid": process_id, "port": port, "owner": token},
            separators=(",", ":"),
        ).encode("utf-8")
        try:
            try:
                existing, existing_info = _read_discovery(
                    resolved,
                    allow_legacy_permissions=True,
                )
            except FileNotFoundError:
                pass
            else:
                if "owner" not in existing and _pid_is_alive(existing["pid"]):
                    raise RuntimeError(
                        f"headless discovery belongs to a live legacy owner "
                        f"(pid {existing['pid']}): {resolved}"
                    )
                if not _unlink_matching_file(resolved, existing_info):
                    raise RuntimeError(
                        f"headless discovery changed during stale recovery: {resolved}"
                    )

            flags = os.O_WRONLY | os.O_CREAT | os.O_EXCL
            flags |= getattr(os, "O_CLOEXEC", 0)
            flags |= getattr(os, "O_NOFOLLOW", 0)
            descriptor = os.open(temporary, flags, 0o600)
            try:
                os.fchmod(descriptor, 0o600)
                with os.fdopen(descriptor, "wb", closefd=True) as stream:
                    descriptor = -1
                    stream.write(payload)
                    stream.flush()
                    os.fsync(stream.fileno())
                try:
                    os.link(temporary, resolved, follow_symlinks=False)
                except FileExistsError as exc:
                    raise RuntimeError(
                        f"headless discovery appeared while ownership was held: "
                        f"{resolved}"
                    ) from exc
                info = os.stat(resolved, follow_symlinks=False)
            finally:
                if descriptor >= 0:
                    os.close(descriptor)
                try:
                    os.unlink(temporary)
                except FileNotFoundError:
                    pass
        except Exception:
            ownership_lock.release()
            raise
        return cls(
            resolved,
            token,
            (info.st_dev, info.st_ino),
            ownership_lock,
        )

    def release(self) -> bool:
        """Remove this claim before allowing the next owner to acquire."""
        ownership_lock = self._ownership_lock
        if ownership_lock is None:
            return False
        self._ownership_lock = None
        try:
            try:
                payload, info = _read_discovery(self.path)
            except (OSError, RuntimeError):
                return False
            if payload.get("owner") != self.token:
                return False
            if (info.st_dev, info.st_ino) != self.file_identity:
                return False
            return _unlink_matching_file(self.path, info)
        finally:
            ownership_lock.release()


_COMMANDS = {
    "test-status": test_status_path,
    "test-pid": test_pid_path,
    "test-output": test_output_path,
    "session-socket": shared_session_socket,
    "headless-status": headless_status_path,
    "headless-port": default_headless_port,
}


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Print a MegaPad runtime path for the active namespace"
    )
    parser.add_argument("artifact", choices=[*sorted(_COMMANDS), "all"])
    args = parser.parse_args()
    try:
        if args.artifact == "all":
            print(json.dumps(all_runtime_paths(), sort_keys=True))
        else:
            print(_COMMANDS[args.artifact]())
    except (RuntimeError, ValueError) as exc:
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
