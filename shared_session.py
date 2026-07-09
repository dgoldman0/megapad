"""Shared single-owner MegaPad runtime and local JSON control protocol."""

from __future__ import annotations

import json
import os
import socket
import threading
import time
from pathlib import Path
from typing import Any

from session import MachineSession, TerminalCell, TerminalSnapshot


PROTOCOL_VERSION = 1
DEFAULT_SOCKET = f"/tmp/megapad-session-{os.getuid()}.sock"
MAX_REQUEST_BYTES = 1 << 20


def _rgb_pack(color: tuple[int, int, int]) -> int:
    return (color[0] << 16) | (color[1] << 8) | color[2]


def _rgb_unpack(value: int) -> tuple[int, int, int]:
    return ((value >> 16) & 0xFF, (value >> 8) & 0xFF, value & 0xFF)


def snapshot_to_wire(snapshot: TerminalSnapshot) -> dict:
    """Run-length encode a terminal snapshot for the local viewer protocol."""
    runs: list[list[Any]] = []
    current = None
    count = 0
    for row in snapshot.cells:
        for cell in row:
            value = (
                cell.char,
                _rgb_pack(cell.fg),
                _rgb_pack(cell.bg),
                cell.attrs,
            )
            if value == current:
                count += 1
                continue
            if current is not None:
                runs.append([count, *current])
            current = value
            count = 1
    if current is not None:
        runs.append([count, *current])
    return {
        "cols": snapshot.cols,
        "rows": snapshot.rows,
        "cursor": [
            snapshot.cursor_row,
            snapshot.cursor_col,
            snapshot.cursor_visible,
        ],
        "alternate_screen": snapshot.alternate_screen,
        "runs": runs,
    }


def snapshot_from_wire(data: dict) -> TerminalSnapshot:
    """Decode a wire snapshot into the immutable public snapshot type."""
    cols = int(data["cols"])
    rows = int(data["rows"])
    flat: list[TerminalCell] = []
    for run in data["runs"]:
        count, char, fg, bg, attrs = run
        cell = TerminalCell(
            char=str(char),
            fg=_rgb_unpack(int(fg)),
            bg=_rgb_unpack(int(bg)),
            attrs=int(attrs),
        )
        flat.extend([cell] * int(count))
    expected = cols * rows
    if len(flat) != expected:
        raise ValueError(f"snapshot has {len(flat)} cells, expected {expected}")
    cells = tuple(
        tuple(flat[row * cols:(row + 1) * cols])
        for row in range(rows)
    )
    cursor_row, cursor_col, cursor_visible = data["cursor"]
    return TerminalSnapshot(
        cols=cols,
        rows=rows,
        cells=cells,
        cursor_col=int(cursor_col),
        cursor_row=int(cursor_row),
        cursor_visible=bool(cursor_visible),
        alternate_screen=bool(data.get("alternate_screen", False)),
    )


class SharedMachine:
    """Continuously runs one MachineSession and serializes all mutations."""

    def __init__(
        self,
        session: MachineSession,
        *,
        idle_tick_cycles: int = 200_000,
        idle_sleep_s: float = 0.002,
    ):
        self.session = session
        self.idle_tick_cycles = int(idle_tick_cycles)
        self.idle_sleep_s = float(idle_sleep_s)
        self.lock = threading.RLock()
        self.condition = threading.Condition(self.lock)
        self.paused = False
        self.total_steps = 0
        self.total_batches = 0
        self.last_error: str | None = None
        self.started_at = time.time()
        self._stopping = False
        self._thread: threading.Thread | None = None

    def start(self):
        with self.lock:
            if self._thread is not None:
                return
            self.session.boot()
            self._thread = threading.Thread(
                target=self._run_loop,
                name="megapad-shared-machine",
                daemon=True,
            )
            self._thread.start()

    def stop(self):
        with self.condition:
            self._stopping = True
            self.condition.notify_all()
        if self._thread is not None and self._thread is not threading.current_thread():
            self._thread.join(timeout=3.0)
        self.session.close()

    def _run_loop(self):
        while True:
            idle_wait = False
            with self.condition:
                if self._stopping:
                    return
                if self.paused:
                    self.condition.wait(timeout=0.1)
                    continue
                system = self.session.system
                if system.all_halted:
                    self.condition.wait(timeout=0.05)
                    continue
                if system.all_idle_or_halted and not system.uart.has_rx_data:
                    idle_wait = True
                else:
                    try:
                        executed = system.run_batch(self.session.batch_steps)
                        if executed > 0:
                            self.total_steps += executed
                            self.total_batches += 1
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True

            if idle_wait:
                with self.condition:
                    self.condition.wait(timeout=self.idle_sleep_s)
                    if self._stopping or self.paused:
                        continue
                    system = self.session.system
                    try:
                        system.bus.tick(self.idle_tick_cycles)

                        # Match MegapadSystem.run() wake semantics while using
                        # larger idle ticks. Without this handoff an interrupt
                        # can become pending while every core stays asleep.
                        if system.timer.irq_pending:
                            for cpu in system.cores:
                                if cpu.idle and cpu.flag_i:
                                    cpu.idle = False
                                    break
                        for cpu in system.cores:
                            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                                cpu.idle = False
                        core0 = system.cores[0]
                        if core0.idle and system._any_nic_rx():
                            core0.idle = False
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True
            else:
                time.sleep(0)

    def status(self) -> dict:
        with self.lock:
            system = self.session.system
            if self.last_error:
                state = "error"
            elif self.paused:
                state = "paused"
            elif system.all_halted:
                state = "halted"
            elif system.all_idle_or_halted and not system.uart.has_rx_data:
                state = "idle"
            else:
                state = "running"
            return {
                "protocol": PROTOCOL_VERSION,
                "state": state,
                "paused": self.paused,
                "halted": system.all_halted,
                "idle": system.all_idle_or_halted,
                "steps": self.total_steps,
                "batches": self.total_batches,
                "revision": self.session.revision,
                "raw_bytes": len(self.session.raw_output),
                "output_batches": self.session.output_batches,
                "byte_callbacks": self.session.output_byte_callbacks,
                "terminal": [self.session.terminal.cols, self.session.terminal.rows],
                "uptime_s": time.time() - self.started_at,
                "error": self.last_error,
            }

    def pause(self) -> dict:
        with self.condition:
            self.paused = True
            self.condition.notify_all()
            return self.status()

    def resume(self) -> dict:
        with self.condition:
            self.paused = False
            self.last_error = None
            self.condition.notify_all()
            return self.status()

    def step(self, count: int = 1) -> dict:
        count = int(count)
        if count <= 0 or count > 1_000_000:
            raise ValueError("step count must be between 1 and 1000000")
        with self.condition:
            if not self.paused:
                raise RuntimeError("machine must be paused before stepping")
            executed = 0
            cycles = 0
            for _ in range(count):
                if self.session.system.all_halted:
                    break
                cycles += self.session.step()
                executed += 1
            self.total_steps += executed
            return {"executed": executed, "cycles": cycles, "status": self.status()}

    def reset(self, *, paused: bool | None = None) -> dict:
        with self.condition:
            self.session.reset()
            self.total_steps = 0
            self.total_batches = 0
            self.last_error = None
            if paused is not None:
                self.paused = bool(paused)
            self.condition.notify_all()
            return self.status()

    def send_text(self, text: str) -> dict:
        with self.condition:
            self.session.send_text(text)
            self.condition.notify_all()
            return {"accepted_bytes": len(text.encode("utf-8"))}

    def send_key(self, key: str) -> dict:
        with self.condition:
            before = self.session.system.uart.rx_pending
            self.session.send_key(key)
            after = self.session.system.uart.rx_pending
            self.condition.notify_all()
            return {"accepted_bytes": max(0, after - before)}

    def resize(self, cols: int, rows: int) -> dict:
        cols = int(cols)
        rows = int(rows)
        if not (20 <= cols <= 400 and 5 <= rows <= 200):
            raise ValueError("terminal size must be within 20x5 and 400x200")
        with self.condition:
            self.session.resize(cols, rows)
            self.condition.notify_all()
            return {"cols": cols, "rows": rows, "revision": self.session.revision}

    def screen(self, since: int = -1) -> dict:
        with self.lock:
            revision = self.session.revision
            if int(since) == revision:
                return {"changed": False, "revision": revision}
            return {
                "changed": True,
                "revision": revision,
                "snapshot": snapshot_to_wire(self.session.snapshot()),
            }

    def text(self, trim_right: bool = True) -> dict:
        with self.lock:
            return {
                "revision": self.session.revision,
                "text": self.session.screen_text(trim_right=trim_right),
            }

    def raw(self, since: int = 0) -> dict:
        with self.lock:
            start = max(0, min(int(since), len(self.session.raw_output)))
            data = bytes(self.session.raw_output[start:])
            return {
                "offset": len(self.session.raw_output),
                "text": data.decode("utf-8", errors="replace"),
            }

    def capture(self, params: dict) -> dict:
        with self.lock:
            snapshot = self.session.snapshot()
            outputs = {}
            if params.get("text"):
                snapshot.write_text(params["text"])
                outputs["text"] = str(Path(params["text"]).resolve())
            if params.get("json"):
                snapshot.write_json(params["json"])
                outputs["json"] = str(Path(params["json"]).resolve())
            if params.get("png"):
                snapshot.write_png(
                    params["png"],
                    font_path=params.get("font"),
                    font_size=int(params.get("font_size", 16)),
                )
                outputs["png"] = str(Path(params["png"]).resolve())
            return {"revision": self.session.revision, "outputs": outputs}


class SessionServer:
    """Unix-domain JSON request server for one SharedMachine."""

    def __init__(self, machine: SharedMachine, socket_path: str = DEFAULT_SOCKET):
        self.machine = machine
        self.socket_path = str(Path(socket_path).expanduser())
        self._socket: socket.socket | None = None
        self._stopping = threading.Event()
        self._clients: set[socket.socket] = set()
        self._clients_lock = threading.Lock()
        self._serve_thread: threading.Thread | None = None

    def start(self):
        self._bind()
        self.machine.start()

    def serve_in_thread(self):
        self.start()
        self._serve_thread = threading.Thread(
            target=self.serve_forever,
            name="megapad-session-server",
            daemon=True,
        )
        self._serve_thread.start()

    def _bind(self):
        path = Path(self.socket_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists():
            probe = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                probe.connect(self.socket_path)
            except OSError:
                path.unlink()
            else:
                probe.close()
                raise RuntimeError(f"shared session already listening at {path}")
            finally:
                probe.close()
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(self.socket_path)
        os.chmod(self.socket_path, 0o600)
        server.listen(8)
        server.settimeout(0.25)
        self._socket = server

    def serve_forever(self):
        if self._socket is None:
            self.start()
        try:
            while not self._stopping.is_set():
                try:
                    client, _ = self._socket.accept()
                except socket.timeout:
                    continue
                except OSError:
                    break
                with self._clients_lock:
                    self._clients.add(client)
                threading.Thread(
                    target=self._handle_client,
                    args=(client,),
                    daemon=True,
                    name="megapad-session-client",
                ).start()
        finally:
            self.stop()

    def _handle_client(self, client: socket.socket):
        try:
            reader = client.makefile("rb")
            while not self._stopping.is_set():
                line = reader.readline(MAX_REQUEST_BYTES + 1)
                if not line:
                    break
                if len(line) > MAX_REQUEST_BYTES:
                    self._send(client, {"id": None, "ok": False, "error": "request too large"})
                    break
                request = None
                try:
                    request = json.loads(line)
                    result = self.dispatch(request.get("method"), request.get("params") or {})
                    response = {"id": request.get("id"), "ok": True, "result": result}
                except Exception as exc:
                    response = {
                        "id": request.get("id") if isinstance(request, dict) else None,
                        "ok": False,
                        "error": f"{type(exc).__name__}: {exc}",
                    }
                self._send(client, response)
        finally:
            with self._clients_lock:
                self._clients.discard(client)
            try:
                client.close()
            except OSError:
                pass

    @staticmethod
    def _send(client: socket.socket, response: dict):
        payload = json.dumps(response, ensure_ascii=False, separators=(",", ":"))
        client.sendall(payload.encode("utf-8") + b"\n")

    def dispatch(self, method: str, params: dict) -> Any:
        if method == "ping":
            return {"protocol": PROTOCOL_VERSION, "time": time.time()}
        if method == "status":
            result = self.machine.status()
            with self._clients_lock:
                result["clients"] = len(self._clients)
            return result
        if method == "pause":
            return self.machine.pause()
        if method == "resume":
            return self.machine.resume()
        if method == "step":
            return self.machine.step(params.get("count", 1))
        if method == "reset":
            return self.machine.reset(paused=params.get("paused"))
        if method == "send_text":
            return self.machine.send_text(str(params.get("text", "")))
        if method == "send_key":
            return self.machine.send_key(str(params["key"]))
        if method == "resize":
            return self.machine.resize(params["cols"], params["rows"])
        if method == "screen":
            return self.machine.screen(params.get("since", -1))
        if method == "text":
            return self.machine.text(bool(params.get("trim_right", True)))
        if method == "raw":
            return self.machine.raw(params.get("since", 0))
        if method == "capture":
            return self.machine.capture(params)
        if method == "shutdown":
            timer = threading.Timer(0.05, self.stop)
            timer.daemon = True
            timer.start()
            return {"stopping": True}
        raise ValueError(f"unknown method: {method!r}")

    def stop(self):
        if self._stopping.is_set():
            return
        self._stopping.set()
        if self._socket is not None:
            try:
                self._socket.close()
            except OSError:
                pass
            self._socket = None
        with self._clients_lock:
            clients = list(self._clients)
            self._clients.clear()
        for client in clients:
            try:
                client.close()
            except OSError:
                pass
        self.machine.stop()
        try:
            Path(self.socket_path).unlink()
        except FileNotFoundError:
            pass


class SessionClient:
    """Thread-safe request client for the local shared-session socket."""

    def __init__(self, socket_path: str = DEFAULT_SOCKET, timeout: float = 5.0):
        self.socket_path = str(Path(socket_path).expanduser())
        self.timeout = float(timeout)
        self._socket: socket.socket | None = None
        self._reader = None
        self._lock = threading.Lock()
        self._next_id = 1

    def connect(self):
        if self._socket is not None:
            return
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(self.timeout)
        client.connect(self.socket_path)
        self._socket = client
        self._reader = client.makefile("rb")

    def close(self):
        if self._reader is not None:
            self._reader.close()
            self._reader = None
        if self._socket is not None:
            self._socket.close()
            self._socket = None

    def __enter__(self) -> "SessionClient":
        self.connect()
        return self

    def __exit__(self, exc_type, exc, traceback):
        self.close()

    def request(self, method: str, **params):
        with self._lock:
            self.connect()
            request_id = self._next_id
            self._next_id += 1
            request = {"id": request_id, "method": method, "params": params}
            payload = json.dumps(request, ensure_ascii=False, separators=(",", ":"))
            self._socket.sendall(payload.encode("utf-8") + b"\n")
            line = self._reader.readline()
            if not line:
                self.close()
                raise ConnectionError("shared session closed the connection")
            response = json.loads(line)
            if response.get("id") != request_id:
                raise RuntimeError("shared session response id mismatch")
            if not response.get("ok"):
                raise RuntimeError(response.get("error", "shared session request failed"))
            return response.get("result")
