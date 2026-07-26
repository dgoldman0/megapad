#!/usr/bin/env python3
"""Launch foreground/background tests with exclusive execution ownership.

The public ``start`` command launches a small supervisor as the leader of a
new session and process group.  The supervisor owns the test command until it
finishes and removes its state record on natural completion.  The public
``foreground`` command holds the same execution interlock and replaces itself
with the test command, preserving normal terminal signals and exit status.
The public ``stop`` command signals a private background session only after
the recorded process identity and unguessable owner token both match the live
supervisor.
"""

from __future__ import annotations

import argparse
import contextlib
import dataclasses
import fcntl
import json
import os
import re
import secrets
import signal
import stat
import subprocess
import sys
import tempfile
import time
from collections.abc import Iterator, Sequence
from pathlib import Path
from typing import Any, NoReturn


_STATE_VERSION = 1
_MAX_STATE_BYTES = 16 * 1024
_OWNER_TOKEN_RE = re.compile(r"[0-9a-f]{32}\Z")
_START_TOKEN_RE = re.compile(
    r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-"
    r"[0-9a-f]{4}-[0-9a-f]{12}:[1-9][0-9]*\Z"
)
_SCRIPT_PATH = str(Path(__file__).resolve())
_BOOT_ID_PATH = "/proc/sys/kernel/random/boot_id"
_PROC_ROOT = "/proc"


class SupervisorError(RuntimeError):
    """A safe, user-facing supervisor failure."""


class StateFormatError(SupervisorError):
    """An ownership document is securely readable but malformed."""

    def __init__(
        self,
        message: str,
        file_identity: "FileIdentity | None" = None,
    ):
        super().__init__(message)
        self.file_identity = file_identity


class AlreadyRunningError(SupervisorError):
    """The ownership document still identifies a live supervisor."""


@dataclasses.dataclass(frozen=True)
class FileIdentity:
    device: int
    inode: int


@dataclasses.dataclass(frozen=True)
class ProcessIdentity:
    pid: int
    pgid: int
    sid: int
    start_token: str
    state: str


@dataclasses.dataclass(frozen=True)
class OwnershipRecord:
    pid: int
    pgid: int
    sid: int
    start_token: str
    owner_token: str

    @classmethod
    def from_payload(cls, payload: Any) -> "OwnershipRecord":
        if not isinstance(payload, dict):
            raise StateFormatError("ownership state must be a JSON object")
        version = payload.get("version")
        if (
            isinstance(version, bool)
            or not isinstance(version, int)
            or version != _STATE_VERSION
        ):
            raise StateFormatError("ownership state has an unsupported version")

        pid = _positive_int(payload.get("pid"), "pid")
        pgid = _positive_int(payload.get("pgid"), "pgid")
        sid = _positive_int(payload.get("sid"), "sid")
        if pid != pgid or pid != sid:
            raise StateFormatError(
                "ownership pid, process group, and session must identify "
                "the same leader"
            )

        start_token = payload.get("start_token")
        if (
            not isinstance(start_token, str)
            or not _START_TOKEN_RE.fullmatch(start_token)
        ):
            raise StateFormatError("ownership state has an invalid start token")

        owner_token = payload.get("owner_token")
        if (
            not isinstance(owner_token, str)
            or not _OWNER_TOKEN_RE.fullmatch(owner_token)
        ):
            raise StateFormatError("ownership state has an invalid owner token")

        return cls(
            pid=pid,
            pgid=pgid,
            sid=sid,
            start_token=start_token,
            owner_token=owner_token,
        )

    def to_payload(self) -> dict[str, int | str]:
        return {
            "version": _STATE_VERSION,
            "pid": self.pid,
            "pgid": self.pgid,
            "sid": self.sid,
            "start_token": self.start_token,
            "owner_token": self.owner_token,
        }


def _positive_int(value: Any, label: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise StateFormatError(f"ownership state has an invalid {label}")
    return value


def _boot_id() -> str:
    try:
        value = Path(_BOOT_ID_PATH).read_text(encoding="ascii").strip().lower()
    except OSError as exc:
        raise SupervisorError(f"cannot read Linux boot identity: {exc}") from exc
    if not re.fullmatch(
        r"[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-"
        r"[0-9a-f]{4}-[0-9a-f]{12}",
        value,
    ):
        raise SupervisorError("Linux boot identity has an unexpected format")
    return value


def _read_process_identity(pid: int, *, include_zombie: bool = False) -> ProcessIdentity:
    if isinstance(pid, bool) or not isinstance(pid, int) or pid <= 0:
        raise ProcessLookupError(f"invalid process id: {pid!r}")

    proc_dir = Path(_PROC_ROOT) / str(pid)
    try:
        proc_info = os.stat(proc_dir)
        raw = (proc_dir / "stat").read_text(encoding="ascii")
    except (FileNotFoundError, ProcessLookupError) as exc:
        raise ProcessLookupError(pid) from exc
    except PermissionError as exc:
        raise SupervisorError(f"cannot inspect process {pid}: permission denied") from exc
    except OSError as exc:
        raise ProcessLookupError(pid) from exc

    if proc_info.st_uid != os.getuid():
        raise SupervisorError(
            f"process {pid} belongs to uid {proc_info.st_uid}, "
            f"expected {os.getuid()}"
        )

    close_paren = raw.rfind(")")
    if close_paren < 0:
        raise SupervisorError(f"process {pid} has malformed /proc state")
    fields = raw[close_paren + 2 :].split()
    if len(fields) < 20:
        raise SupervisorError(f"process {pid} has incomplete /proc state")
    try:
        pgid = int(fields[2])
        sid = int(fields[3])
        start_ticks = int(fields[19])
    except ValueError as exc:
        raise SupervisorError(f"process {pid} has invalid /proc state") from exc

    process_state = fields[0]
    if process_state == "Z" and not include_zombie:
        raise ProcessLookupError(pid)
    return ProcessIdentity(
        pid=pid,
        pgid=pgid,
        sid=sid,
        start_token=f"{_boot_id()}:{start_ticks}",
        state=process_state,
    )


def _supervisor_command_matches(record: OwnershipRecord) -> bool:
    try:
        raw = (Path(_PROC_ROOT) / str(record.pid) / "cmdline").read_bytes()
    except (FileNotFoundError, ProcessLookupError):
        return False
    except PermissionError as exc:
        raise SupervisorError(
            f"cannot inspect supervisor command for process {record.pid}"
        ) from exc
    argv = [
        item.decode(errors="surrogateescape")
        for item in raw.rstrip(b"\0").split(b"\0")
        if item
    ]
    if len(argv) < 3 or argv[2] != "_run":
        return False
    try:
        script_matches = str(Path(argv[1]).resolve()) == _SCRIPT_PATH
        token_index = argv.index("--token", 3)
    except OSError:
        return False
    except ValueError:
        return False
    return (
        script_matches
        and token_index + 1 < len(argv)
        and argv[token_index + 1] == record.owner_token
    )


def _record_matches_live_process(
    record: OwnershipRecord,
) -> tuple[bool, str]:
    try:
        identity = _read_process_identity(record.pid)
    except ProcessLookupError:
        return False, "recorded supervisor has exited"
    if (
        identity.pgid != record.pgid
        or identity.sid != record.sid
        or identity.start_token != record.start_token
    ):
        return False, "recorded PID now has a different process identity"
    if not _supervisor_command_matches(record):
        return False, "recorded PID is not the owning test supervisor"
    return True, ""


def _validate_owned_regular(info: os.stat_result, path: str) -> None:
    if not stat.S_ISREG(info.st_mode):
        raise SupervisorError(f"unsafe ownership path is not a regular file: {path}")
    if info.st_uid != os.getuid():
        raise SupervisorError(
            f"unsafe ownership path belongs to uid {info.st_uid}, "
            f"expected {os.getuid()}: {path}"
        )
    if stat.S_IMODE(info.st_mode) & 0o022:
        raise SupervisorError(
            f"unsafe ownership path is writable by another user: {path}"
        )


@contextlib.contextmanager
def _state_lock(state_path: str) -> Iterator[None]:
    lock_path = f"{state_path}.lock"
    flags = os.O_RDWR | os.O_CREAT | getattr(os, "O_CLOEXEC", 0)
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(lock_path, flags, 0o600)
    except OSError as exc:
        raise SupervisorError(f"cannot open ownership lock {lock_path}: {exc}") from exc
    try:
        _validate_owned_regular(os.fstat(descriptor), lock_path)
        fcntl.flock(descriptor, fcntl.LOCK_EX)
        yield
    finally:
        os.close(descriptor)


def _acquire_execution_lock(state_path: str) -> int:
    lock_path = f"{state_path}.run.lock"
    flags = os.O_RDWR | os.O_CREAT | getattr(os, "O_CLOEXEC", 0)
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(lock_path, flags, 0o600)
    except OSError as exc:
        raise SupervisorError(
            f"cannot open test execution lock {lock_path}: {exc}"
        ) from exc
    try:
        info = os.fstat(descriptor)
        _validate_owned_regular(info, lock_path)
        if info.st_nlink != 1:
            raise SupervisorError(
                f"unsafe test execution lock has {info.st_nlink} hard links: "
                f"{lock_path}"
            )
        try:
            fcntl.flock(
                descriptor,
                fcntl.LOCK_EX | fcntl.LOCK_NB,
            )
        except BlockingIOError as exc:
            raise AlreadyRunningError(
                "tests already running (foreground execution lock is held)"
            ) from exc
        current = os.stat(lock_path, follow_symlinks=False)
        if (
            current.st_dev != info.st_dev or
            current.st_ino != info.st_ino
        ):
            fcntl.flock(descriptor, fcntl.LOCK_UN)
            raise SupervisorError(
                f"test execution lock changed during acquisition: {lock_path}"
            )
    except BaseException:
        os.close(descriptor)
        raise
    return descriptor


def _release_execution_lock(descriptor: int) -> None:
    try:
        fcntl.flock(descriptor, fcntl.LOCK_UN)
    finally:
        os.close(descriptor)


def _read_state(
    state_path: str,
) -> tuple[OwnershipRecord | None, FileIdentity | None]:
    flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0)
    flags |= getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(state_path, flags)
    except FileNotFoundError:
        return None, None
    except OSError as exc:
        raise SupervisorError(f"cannot open ownership state {state_path}: {exc}") from exc

    try:
        info = os.fstat(descriptor)
        _validate_owned_regular(info, state_path)
        chunks: list[bytes] = []
        size = 0
        while size <= _MAX_STATE_BYTES:
            chunk = os.read(
                descriptor,
                min(4096, _MAX_STATE_BYTES + 1 - size),
            )
            if not chunk:
                break
            chunks.append(chunk)
            size += len(chunk)
        data = b"".join(chunks)
    finally:
        os.close(descriptor)

    identity = FileIdentity(info.st_dev, info.st_ino)
    if len(data) > _MAX_STATE_BYTES:
        raise StateFormatError("ownership state is too large", identity)
    try:
        payload = json.loads(data)
    except (UnicodeDecodeError, json.JSONDecodeError) as exc:
        raise StateFormatError(
            "ownership state is not valid JSON",
            identity,
        ) from exc
    try:
        record = OwnershipRecord.from_payload(payload)
    except StateFormatError as exc:
        raise StateFormatError(str(exc), identity) from exc
    return record, identity


def _unlink_if_same(state_path: str, identity: FileIdentity | None) -> bool:
    if identity is None:
        return False
    try:
        current = os.lstat(state_path)
    except FileNotFoundError:
        return False
    if (
        current.st_dev != identity.device
        or current.st_ino != identity.inode
        or not stat.S_ISREG(current.st_mode)
    ):
        return False
    os.unlink(state_path)
    return True


def _write_state(state_path: str, record: OwnershipRecord) -> None:
    parent = os.path.dirname(os.path.abspath(state_path))
    prefix = f".{os.path.basename(state_path)}."
    try:
        descriptor, temporary_path = tempfile.mkstemp(prefix=prefix, dir=parent)
    except OSError as exc:
        raise SupervisorError(f"cannot create ownership state: {exc}") from exc

    try:
        os.fchmod(descriptor, 0o600)
        data = (
            json.dumps(record.to_payload(), sort_keys=True, separators=(",", ":"))
            + "\n"
        ).encode("ascii")
        offset = 0
        while offset < len(data):
            offset += os.write(descriptor, data[offset:])
        os.fsync(descriptor)
        os.close(descriptor)
        descriptor = -1
        os.replace(temporary_path, state_path)
    finally:
        if descriptor >= 0:
            os.close(descriptor)
        try:
            os.unlink(temporary_path)
        except FileNotFoundError:
            pass


def _open_output(output_path: str) -> int:
    flags = os.O_WRONLY | os.O_CREAT
    flags |= getattr(os, "O_CLOEXEC", 0) | getattr(os, "O_NOFOLLOW", 0)
    try:
        descriptor = os.open(output_path, flags, 0o600)
    except OSError as exc:
        raise SupervisorError(f"cannot open test output {output_path}: {exc}") from exc
    try:
        info = os.fstat(descriptor)
        _validate_owned_regular(info, output_path)
        if info.st_nlink != 1:
            raise SupervisorError(
                f"unsafe test output has {info.st_nlink} hard links: {output_path}"
            )
        os.fchmod(descriptor, 0o600)
        os.ftruncate(descriptor, 0)
    except BaseException:
        os.close(descriptor)
        raise
    return descriptor


def _remove_status_file(status_path: str | None) -> None:
    if not status_path:
        return
    try:
        info = os.lstat(status_path)
    except FileNotFoundError:
        return
    if stat.S_ISDIR(info.st_mode):
        raise SupervisorError(f"test status path is a directory: {status_path}")
    try:
        os.unlink(status_path)
    except OSError as exc:
        raise SupervisorError(f"cannot remove stale test status: {exc}") from exc


def _normalize_command(command: Sequence[str]) -> list[str]:
    normalized = list(command)
    if normalized and normalized[0] == "--":
        normalized.pop(0)
    if not normalized:
        raise SupervisorError("a test command is required after '--'")
    return normalized


def _reject_live_owner_or_discard_stale(state_path: str) -> None:
    try:
        existing, existing_file = _read_state(state_path)
    except StateFormatError as exc:
        raise SupervisorError(
            f"refusing to replace malformed ownership state: {exc}; "
            "run 'make test-kill' once to clear it safely"
        ) from exc

    if existing is None:
        return
    matches, reason = _record_matches_live_process(existing)
    if matches:
        raise AlreadyRunningError(
            f"tests already running (PID {existing.pid}); "
            "use 'make test-kill' first"
        )
    _unlink_if_same(state_path, existing_file)
    print(f"Discarded stale test ownership state: {reason}.", file=sys.stderr)


def _launch_supervisor(
    *,
    state_path: str,
    output_path: str,
    status_path: str | None,
    command: Sequence[str],
) -> tuple[subprocess.Popen[bytes], OwnershipRecord]:
    """Claim the test slot and launch its private session leader."""
    normalized_command = _normalize_command(command)
    with _state_lock(state_path):
        _reject_live_owner_or_discard_stale(state_path)
        execution_lock = _acquire_execution_lock(state_path)
        output_descriptor = -1
        gate_read = -1
        gate_write = -1
        process: subprocess.Popen[bytes] | None = None
        record: OwnershipRecord | None = None
        try:
            _remove_status_file(status_path)
            output_descriptor = _open_output(output_path)
            gate_read, gate_write = os.pipe()
            owner_token = secrets.token_hex(16)
            runner_args = [
                sys.executable,
                _SCRIPT_PATH,
                "_run",
                "--state",
                state_path,
                "--token",
                owner_token,
                "--gate-fd",
                str(gate_read),
                "--",
                *normalized_command,
            ]
            process = subprocess.Popen(
                runner_args,
                stdin=subprocess.DEVNULL,
                stdout=output_descriptor,
                stderr=subprocess.STDOUT,
                close_fds=True,
                pass_fds=(gate_read,),
                start_new_session=True,
            )
            os.close(gate_read)
            gate_read = -1

            identity = _read_process_identity(process.pid)
            if (
                process.pid <= 0
                or identity.pgid != process.pid
                or identity.sid != process.pid
            ):
                raise SupervisorError(
                    "detached test supervisor did not become its own "
                    "session and process-group leader"
                )
            record = OwnershipRecord(
                pid=process.pid,
                pgid=identity.pgid,
                sid=identity.sid,
                start_token=identity.start_token,
                owner_token=owner_token,
            )
            _write_state(state_path, record)
            os.write(gate_write, b"1")
        except BaseException:
            if process is not None:
                terminated_session = False
                if record is not None:
                    try:
                        _terminate_owned_session(
                            record,
                            grace_seconds=0.2,
                        )
                        terminated_session = True
                    except SupervisorError:
                        pass
                if not terminated_session:
                    try:
                        process.kill()
                    except ProcessLookupError:
                        pass
                try:
                    process.wait(timeout=1)
                except (subprocess.TimeoutExpired, ChildProcessError):
                    pass
            if record is not None:
                try:
                    current, current_file = _read_state(state_path)
                except SupervisorError:
                    current = None
                    current_file = None
                if current == record:
                    _unlink_if_same(state_path, current_file)
            raise
        finally:
            if gate_read >= 0:
                os.close(gate_read)
            if gate_write >= 0:
                os.close(gate_write)
            if output_descriptor >= 0:
                os.close(output_descriptor)
            _release_execution_lock(execution_lock)

    assert process is not None
    assert record is not None
    return process, record


def start(
    *,
    state_path: str,
    output_path: str,
    status_path: str | None,
    command: Sequence[str],
) -> int:
    """Start a detached supervisor and return its positive session-leader PID."""
    _, record = _launch_supervisor(
        state_path=state_path,
        output_path=output_path,
        status_path=status_path,
        command=command,
    )
    print(f"PID: {record.pid}")
    return record.pid


def foreground(
    *,
    state_path: str,
    status_path: str | None,
    command: Sequence[str],
) -> NoReturn:
    """Replace this process with one foreground command under the run lock."""
    normalized_command = _normalize_command(command)
    execution_lock = -1
    try:
        with _state_lock(state_path):
            _reject_live_owner_or_discard_stale(state_path)
            execution_lock = _acquire_execution_lock(state_path)
            _remove_status_file(status_path)
        os.set_inheritable(execution_lock, True)
        os.execvpe(
            normalized_command[0],
            normalized_command,
            os.environ,
        )
    except OSError as exc:
        raise SupervisorError(
            f"cannot execute foreground test command: {exc}"
        ) from exc
    finally:
        if execution_lock >= 0:
            _release_execution_lock(execution_lock)


def _session_members(sid: int) -> list[ProcessIdentity]:
    members: list[ProcessIdentity] = []
    try:
        entries = os.scandir(_PROC_ROOT)
    except OSError as exc:
        raise SupervisorError(f"cannot enumerate processes: {exc}") from exc
    with entries:
        for entry in entries:
            if not entry.name.isdigit():
                continue
            try:
                identity = _read_process_identity(
                    int(entry.name),
                    include_zombie=True,
                )
            except (ProcessLookupError, SupervisorError):
                continue
            if identity.sid == sid and identity.state != "Z":
                members.append(identity)
    return members


def _signal_if_same(identity: ProcessIdentity, signal_number: int) -> None:
    try:
        current = _read_process_identity(identity.pid)
    except ProcessLookupError:
        return
    if (
        current.pgid != identity.pgid
        or current.sid != identity.sid
        or current.start_token != identity.start_token
    ):
        return
    try:
        os.kill(identity.pid, signal_number)
    except ProcessLookupError:
        pass


def _signal_session_members(
    sid: int,
    signal_number: int,
    *,
    exclude: frozenset[int] = frozenset(),
) -> None:
    for identity in _session_members(sid):
        if identity.pid not in exclude:
            _signal_if_same(identity, signal_number)


def _wait_for_empty_session(
    sid: int,
    timeout: float,
    *,
    exclude: frozenset[int] = frozenset(),
) -> bool:
    deadline = time.monotonic() + max(0.0, timeout)
    while True:
        live = [
            identity
            for identity in _session_members(sid)
            if identity.pid not in exclude
        ]
        if not live:
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(min(0.05, max(0.0, deadline - time.monotonic())))


def _terminate_owned_session(record: OwnershipRecord, grace_seconds: float) -> None:
    matches, reason = _record_matches_live_process(record)
    if not matches:
        raise SupervisorError(
            f"refusing to signal changed test ownership: {reason}"
        )

    try:
        os.killpg(record.pgid, signal.SIGTERM)
    except ProcessLookupError:
        return
    _signal_session_members(record.sid, signal.SIGTERM)
    if _wait_for_empty_session(record.sid, grace_seconds):
        return

    try:
        os.killpg(record.pgid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    _signal_session_members(record.sid, signal.SIGKILL)
    _wait_for_empty_session(record.sid, min(1.0, grace_seconds))


def _terminate_residual_descendants(sid: int) -> None:
    exclude = frozenset({os.getpid()})
    _signal_session_members(sid, signal.SIGTERM, exclude=exclude)
    if _wait_for_empty_session(sid, 0.5, exclude=exclude):
        return
    _signal_session_members(sid, signal.SIGKILL, exclude=exclude)
    _wait_for_empty_session(sid, 0.5, exclude=exclude)


def _remove_owned_state(
    state_path: str,
    *,
    pid: int,
    start_token: str,
    owner_token: str,
) -> None:
    with _state_lock(state_path):
        try:
            record, file_identity = _read_state(state_path)
        except SupervisorError:
            return
        if (
            record is not None
            and record.pid == pid
            and record.start_token == start_token
            and record.owner_token == owner_token
        ):
            _unlink_if_same(state_path, file_identity)


def run_supervisor(
    *,
    state_path: str,
    owner_token: str,
    gate_fd: int,
    command: Sequence[str],
) -> int:
    """Internal detached runner.  Wait for ownership publication, then test."""
    normalized_command = _normalize_command(command)
    try:
        gate_value = os.read(gate_fd, 1)
    finally:
        os.close(gate_fd)
    if gate_value != b"1":
        return 125

    with _state_lock(state_path):
        try:
            record, _ = _read_state(state_path)
        except SupervisorError as exc:
            print(f"test supervisor could not read ownership state: {exc}", file=sys.stderr)
            return 125
        if record is None:
            print("test supervisor ownership state disappeared", file=sys.stderr)
            return 125
        try:
            identity = _read_process_identity(os.getpid())
        except SupervisorError as exc:
            print(f"test supervisor could not verify itself: {exc}", file=sys.stderr)
            return 125
        if (
            record.pid != os.getpid()
            or record.owner_token != owner_token
            or record.start_token != identity.start_token
        ):
            print("test supervisor ownership identity changed", file=sys.stderr)
            return 125

    exit_code = 125
    try:
        try:
            child = subprocess.Popen(normalized_command)
        except OSError as exc:
            print(f"cannot start background test command: {exc}", file=sys.stderr)
            return 127
        exit_code = child.wait()
        return exit_code
    finally:
        _terminate_residual_descendants(os.getsid(0))
        _remove_owned_state(
            state_path,
            pid=os.getpid(),
            start_token=record.start_token,
            owner_token=owner_token,
        )


def stop(*, state_path: str, grace_seconds: float) -> int:
    """Terminate only the live, identity-matched private test session."""
    with _state_lock(state_path):
        try:
            record, file_identity = _read_state(state_path)
        except StateFormatError as exc:
            # A malformed document grants no signal authority.  A securely
            # owned regular file can still be removed so the next run is not
            # permanently wedged.
            _unlink_if_same(state_path, exc.file_identity)
            print(
                f"Refused to signal from malformed test ownership state: {exc}. "
                "Removed the invalid state file.",
                file=sys.stderr,
            )
            return 2

        if record is None:
            print("No tests running (no ownership state).")
            return 0

        matches, reason = _record_matches_live_process(record)
        if not matches:
            _unlink_if_same(state_path, file_identity)
            print(
                f"Refused to signal stale test ownership state: {reason}. "
                "Removed the stale state file."
            )
            return 0

        _terminate_owned_session(record, grace_seconds)
        _unlink_if_same(state_path, file_identity)
        print(f"Terminated owned test process group (PID {record.pid}).")
        return 0


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Own one MegaPad test execution at a time."
    )
    subparsers = parser.add_subparsers(dest="action", required=True)

    start_parser = subparsers.add_parser("start")
    start_parser.add_argument("--state", required=True)
    start_parser.add_argument("--output", required=True)
    start_parser.add_argument("--status")
    start_parser.add_argument("command", nargs=argparse.REMAINDER)

    foreground_parser = subparsers.add_parser("foreground")
    foreground_parser.add_argument("--state", required=True)
    foreground_parser.add_argument("--status")
    foreground_parser.add_argument("command", nargs=argparse.REMAINDER)

    stop_parser = subparsers.add_parser("stop")
    stop_parser.add_argument("--state", required=True)
    stop_parser.add_argument("--grace-seconds", type=float, default=3.0)

    run_parser = subparsers.add_parser("_run", help=argparse.SUPPRESS)
    run_parser.add_argument("--state", required=True)
    run_parser.add_argument("--token", required=True)
    run_parser.add_argument("--gate-fd", required=True, type=int)
    run_parser.add_argument("command", nargs=argparse.REMAINDER)
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = _parser().parse_args(argv)
    try:
        if args.action == "start":
            start(
                state_path=args.state,
                output_path=args.output,
                status_path=args.status,
                command=args.command,
            )
            return 0
        if args.action == "foreground":
            foreground(
                state_path=args.state,
                status_path=args.status,
                command=args.command,
            )
        if args.action == "stop":
            if args.grace_seconds < 0:
                raise SupervisorError("--grace-seconds must be non-negative")
            return stop(
                state_path=args.state,
                grace_seconds=args.grace_seconds,
            )
        if args.action == "_run":
            return run_supervisor(
                state_path=args.state,
                owner_token=args.token,
                gate_fd=args.gate_fd,
                command=args.command,
            )
    except AlreadyRunningError as exc:
        print(str(exc), file=sys.stderr)
        return 1
    except SupervisorError as exc:
        print(f"test process supervisor: {exc}", file=sys.stderr)
        return 2
    raise AssertionError(f"unknown action: {args.action}")


if __name__ == "__main__":
    raise SystemExit(main())
