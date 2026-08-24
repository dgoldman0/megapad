"""Tests for the shared single-owner session protocol."""

from __future__ import annotations

import base64
import threading
import time
from pathlib import Path

import pytest

from presentation_terminal import (
    Cell,
    Cursor,
    DriverLimits,
    DriverStatus,
    EgressWatermarks,
    HostPortLimits,
    TerminalConfig,
    TerminalView,
)
from session import MachineSession, PresentationSessionConfig
from shared_session import (
    PROTOCOL_VERSION,
    SessionClient,
    SessionServer,
    SharedMachine,
    snapshot_from_wire,
    snapshot_to_wire,
)
from system import MegapadSystem, SystemRunStats


ROOT = Path(__file__).resolve().parents[1]
BIOS = ROOT / "bios.asm"


def _presentation_config(
    *,
    ansi_history_bytes: int = 32,
) -> PresentationSessionConfig:
    return PresentationSessionConfig(
        host_limits=HostPortLimits(
            egress=EgressWatermarks(8_192, 1_024, 16, 2),
            retained_publication_bytes=4_608,
            ingress_bytes=8_192,
            ingress_events=16,
            ingress_control_bytes=4_096,
            ingress_control_events=8,
            geometry_events=2,
        ),
        terminal_config=TerminalConfig(
            max_payload=256,
            max_transaction_bytes=512,
            terminal_receive_credit=1_024,
            max_cells=16,
            max_feed_bytes=4_608,
            cols=2,
            rows=2,
        ),
        driver_limits=DriverLimits(4_096, 8),
        ansi_history_bytes=ansi_history_bytes,
        service_batches=2,
    )


def wait_until(predicate, timeout=3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = predicate()
        if value:
            return value
        time.sleep(0.01)
    raise AssertionError("condition did not become true")


class _RunLoopUART:
    has_rx_data = False


class _RunLoopSystem:
    def __init__(self):
        self.all_halted = True
        self.all_idle_or_halted = True
        self.uart = _RunLoopUART()


class _RunLoopSession:
    def __init__(self, *, remain_pending: bool):
        self.system = _RunLoopSystem()
        self.batch_steps = 17
        self.presentation_enabled = True
        self.presentation_failure = None
        self.presentation_lost = False
        self.presentation_work_pending = True
        self.last_batch_made_progress = False
        self.remain_pending = remain_pending
        self.calls = 0
        self.called = threading.Event()
        self.closed = False

    def boot(self):
        pass

    def close(self):
        self.closed = True

    def run_batch_stats(self, steps):
        assert steps == self.batch_steps
        self.calls += 1
        self.called.set()
        if not self.remain_pending:
            self.presentation_work_pending = False
            self.last_batch_made_progress = True
            return SystemRunStats(
                instructions_executed=0,
                system_cycles_advanced=0,
                per_core_instructions=(0,),
                per_core_cycles=(0,),
                system_stop_reason="all_halted",
                external_events_applied=1,
            )
        self.last_batch_made_progress = False
        return SystemRunStats(
            instructions_executed=0,
            system_cycles_advanced=0,
            per_core_instructions=(0,),
            per_core_cycles=(0,),
            system_stop_reason="host_backpressure",
        )


def test_shared_owner_services_presentation_work_after_guest_halt():
    session = _RunLoopSession(remain_pending=False)
    machine = SharedMachine(session, idle_sleep_s=0.005)

    machine.start()
    try:
        assert session.called.wait(timeout=1.0)
        wait_until(lambda: not session.presentation_work_pending)
        assert session.calls == 1
        assert machine.total_steps == 0
        assert machine.last_stop_reason == "all_halted"
        assert machine.last_error is None
    finally:
        machine.stop()

    assert session.closed


def test_shared_owner_waits_on_zero_progress_host_backpressure():
    session = _RunLoopSession(remain_pending=True)
    machine = SharedMachine(session, idle_sleep_s=0.02)

    machine.start()
    try:
        assert session.called.wait(timeout=1.0)
        time.sleep(0.07)
        assert session.calls < 20
        assert machine.total_steps == 0
        assert machine.total_batches == 0
        assert machine.last_stop_reason == "host_backpressure"
        assert machine.last_error is None
        assert not machine.paused
    finally:
        machine.stop()


def test_snapshot_wire_round_trip():
    with MachineSession.from_bios(BIOS, cols=40, rows=12) as session:
        session.boot()
        session.wait_for_idle(max_steps=2_000_000)
        original = session.snapshot()
        restored = snapshot_from_wire(snapshot_to_wire(original))
        assert restored == original


def test_shared_screen_round_trips_the_selected_rich_view():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(),
    ) as session:
        session._receive_presentation_view(
            TerminalView(
                attachment_epoch=session.presentation_driver.attachment_epoch,
                session_id=7,
                presentation_epoch=1,
                revision=9,
                cols=2,
                rows=2,
                cells=(
                    (
                        Cell(ord("A"), 1, 2, 1 << 6),
                        Cell(ord("B"), 3, 4, 1 << 5),
                    ),
                    (
                        Cell(ord("C"), 5, 6, 0),
                        Cell(ord("D"), 7, 0, 1),
                    ),
                ),
                dirty_spans=(),
                cursor=Cursor(1, 1, True),
            )
        )
        machine = SharedMachine(session)
        result = machine.screen(since=-1)
        restored = snapshot_from_wire(result["snapshot"])

        assert restored.cols == 2 and restored.rows == 2
        assert restored.cursor_col == 1 and restored.cursor_row == 1
        assert restored.cursor_visible
        assert restored.cells[0][0].attrs == 0x80
        assert restored.cells[0][1].attrs == 0x20

        # A committed resize can precede its required replacement snapshot.
        # Status and screen must both continue to describe the retained rich
        # view while the hidden ANSI fallback tracks the new geometry.
        session.presentation_driver.core.select_ansi_geometry(4, 1)
        session._sync_presentation_geometry()
        assert session.visible_geometry == (2, 2)
        assert (session.terminal.cols, session.terminal.rows) == (4, 1)
        assert machine.status(detailed=False)["terminal"] == [2, 2]


def test_shared_raw_uses_absolute_bounded_cursors_across_reset():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(ansi_history_bytes=4),
    ) as session:
        machine = SharedMachine(session)
        session._receive_presentation_ansi(b"abcdef")

        rolled = machine.raw(since=0)
        assert rolled == {
            "start": 2,
            "available_from": 2,
            "offset": 6,
            "truncated": True,
            "text": "cdef",
            "data_base64": base64.b64encode(b"cdef").decode("ascii"),
        }
        assert machine.raw(since=4)["text"] == "ef"

        session.reset()
        cleared = machine.raw(since=4)
        assert cleared["available_from"] == 6
        assert cleared["start"] == 6
        assert cleared["offset"] == 6
        assert cleared["truncated"]
        assert cleared["text"] == ""


@pytest.mark.parametrize(
    ("returned", "expected", "accepted"),
    (
        (None, DriverStatus.PROGRESS, True),
        (DriverStatus.PROGRESS, DriverStatus.PROGRESS, True),
        (DriverStatus.BACKPRESSURED, DriverStatus.BACKPRESSURED, False),
        (DriverStatus.INVALID, DriverStatus.INVALID, False),
        (DriverStatus.STALE, DriverStatus.STALE, False),
        (DriverStatus.FAILED, DriverStatus.FAILED, False),
    ),
)
def test_shared_input_reports_exact_admission_status(
    monkeypatch,
    returned,
    expected,
    accepted,
):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        monkeypatch.setattr(session, "send_text", lambda text: returned)
        monkeypatch.setattr(session, "send_key", lambda key: returned)
        monkeypatch.setattr(session, "resize", lambda cols, rows: returned)

        text = machine.send_text("é")
        key = machine.send_key("enter")
        resize = machine.resize(2, 2)

        assert text == {
            "status": expected.value,
            "accepted_bytes": 2 if accepted else 0,
        }
        assert key == {
            "status": expected.value,
            "accepted_events": 1 if accepted else 0,
        }
        assert resize["status"] == expected.value
        assert resize["accepted"] is accepted
        assert resize["requested"] == [2, 2]
        assert (resize["cols"], resize["rows"]) == session.visible_geometry
        if expected in {DriverStatus.STALE, DriverStatus.FAILED}:
            assert machine.paused
            assert machine.last_error is not None
        else:
            assert not machine.paused


def test_session_dispatch_rejects_input_from_an_old_reset_generation(tmp_path):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        server = SessionServer(machine, str(tmp_path / "unused.sock"))
        reset = machine.reset(paused=True)
        generation = reset["generation"]
        before = session.system.uart.rx_pending

        stale = server.dispatch(
            "send_text",
            {"text": "old", "generation": generation - 1},
        )
        assert stale == {"status": "stale_generation", "accepted_bytes": 0}
        assert session.system.uart.rx_pending == before

        accepted = server.dispatch(
            "send_text",
            {"text": "new", "generation": generation},
        )
        assert accepted == {"status": "progress", "accepted_bytes": 3}
        assert session.system.uart.rx_pending == before + 3

        with pytest.raises(ValueError, match="requires generation"):
            server.dispatch("send_key", {"key": "enter"})


@pytest.mark.parametrize("presentation", (None, _presentation_config()))
def test_shared_paused_step_counts_only_guest_instructions(
    monkeypatch,
    presentation,
):
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=presentation,
    ) as session:
        machine = SharedMachine(session)
        machine.paused = True
        results = iter(
            (
                SystemRunStats(
                    instructions_executed=0,
                    system_cycles_advanced=7,
                    per_core_instructions=(0,),
                    per_core_cycles=(7,),
                    system_stop_reason="host_backpressure",
                ),
                SystemRunStats(
                    instructions_executed=1,
                    system_cycles_advanced=11,
                    per_core_instructions=(1,),
                    per_core_cycles=(11,),
                    system_stop_reason="instruction_limit",
                ),
            )
        )
        monkeypatch.setattr(session, "run_batch_stats", lambda count: next(results))

        blocked = machine.step(count=3)
        assert blocked["executed"] == 0
        assert blocked["cycles"] == 7
        assert blocked["stop_reason"] == "host_backpressure"
        assert machine.total_steps == 0

        progressed = machine.step(count=1)
        assert progressed["executed"] == 1
        assert progressed["cycles"] == 11
        assert progressed["stop_reason"] == "instruction_limit"
        assert machine.total_steps == 1


def test_shared_lost_session_requires_successful_reset():
    system = MegapadSystem(
        ram_size=64 * 1024,
        terminal_cols=2,
        terminal_rows=2,
    )
    with MachineSession(
        system,
        cols=2,
        rows=2,
        presentation=_presentation_config(),
    ) as session:
        machine = SharedMachine(session)
        machine.paused = True
        session.presentation_driver.close()

        status = machine.status(detailed=False)
        assert status["state"] == "lost"
        assert not status["idle"]
        assert status["presentation"]["lost"]
        with pytest.raises(RuntimeError, match="requires a machine reset"):
            machine.resume()
        with pytest.raises(RuntimeError, match="requires a machine reset"):
            machine.step()

        reset = machine.reset(paused=True)
        assert reset["state"] == "paused"
        assert not reset["presentation"]["lost"]
        assert reset["presentation"]["failure"] is None


def test_shared_failed_reset_remains_paused_and_visible(monkeypatch):
    with MachineSession(MegapadSystem(ram_size=64 * 1024)) as session:
        machine = SharedMachine(session)
        machine.paused = False

        def fail_reset():
            raise RuntimeError("injected reset failure")

        monkeypatch.setattr(session, "reset", fail_reset)
        with pytest.raises(RuntimeError, match="injected reset failure"):
            machine.reset(paused=False)

        assert machine.paused
        assert machine.last_error == "RuntimeError: injected reset failure"


def test_shared_machine_wakes_idle_cpu_for_timer_irq():
    session = MachineSession.from_bios(BIOS)
    machine = SharedMachine(
        session,
        idle_tick_cycles=1_000,
        idle_sleep_s=0.001,
    )
    machine.start()
    try:
        wait_until(lambda: session.system.all_idle_or_halted)
        before = machine.total_steps
        with machine.condition:
            cpu = session.system.cpu
            timer = session.system.timer
            cpu.flag_i = True
            timer.counter = 0
            timer.compare = 1_000
            timer.control = 0x03
            machine.condition.notify_all()

        wait_until(lambda: machine.total_steps > before)
        assert machine.last_error is None
    finally:
        machine.stop()


def test_lightweight_status_skips_forth_diagnostics(monkeypatch):
    with MachineSession.from_bios(BIOS) as session:
        machine = SharedMachine(session)
        calls = []

        def diagnostics(cpu):
            calls.append(cpu)
            return {"sentinel": True}

        monkeypatch.setattr(machine, "_forth_diagnostics", diagnostics)

        lightweight = machine.status(detailed=False)
        assert calls == []
        assert lightweight["protocol"] == PROTOCOL_VERSION == 2
        assert "state" in lightweight
        assert "steps" in lightweight
        assert "revision" in lightweight
        assert "forth" not in lightweight
        assert "cpu" not in lightweight
        assert "nic" not in lightweight

        detailed = machine.status()
        assert calls == [session.system.cpu]
        assert detailed["forth"] == {"sentinel": True}
        assert "cpu" in detailed
        assert "nic" in detailed


def test_screen_encodes_snapshot_outside_machine_lock(monkeypatch):
    with MachineSession.from_bios(BIOS, cols=40, rows=12) as session:
        machine = SharedMachine(session)
        conversion_started = threading.Event()
        allow_conversion = threading.Event()
        original = snapshot_to_wire

        def blocking_conversion(snapshot):
            conversion_started.set()
            assert allow_conversion.wait(timeout=2.0)
            return original(snapshot)

        monkeypatch.setattr(
            "shared_session.snapshot_to_wire",
            blocking_conversion,
        )
        result = []
        failure = []

        def request_screen():
            try:
                result.append(machine.screen(since=-1))
            except BaseException as exc:  # propagate worker failures below
                failure.append(exc)

        worker = threading.Thread(target=request_screen)
        worker.start()
        assert conversion_started.wait(timeout=2.0)
        acquired = machine.lock.acquire(timeout=0.5)
        if acquired:
            machine.lock.release()
        allow_conversion.set()
        worker.join(timeout=2.0)

        assert acquired, "screen RLE conversion held the machine lock"
        assert not worker.is_alive()
        assert failure == []
        assert result[0]["changed"]
        assert snapshot_from_wire(result[0]["snapshot"]).cols == 40


def test_shared_server_clients_control_one_machine(tmp_path):
    socket_path = tmp_path / "shared.sock"
    session = MachineSession.from_bios(BIOS, cols=60, rows=20)
    machine = SharedMachine(session)
    server = SessionServer(machine, str(socket_path))
    try:
        server.serve_in_thread()
    except PermissionError:
        session.close()
        pytest.skip("Unix sockets are unavailable in this sandbox")

    try:
        with SessionClient(str(socket_path)) as controller, \
                SessionClient(str(socket_path)) as viewer:
            status = wait_until(
                lambda: (
                    current
                    if ((current := controller.request("status"))["state"] == "idle"
                        and current["clients"] == 2)
                    else None
                )
            )
            assert status["clients"] == 2
            generation = status["generation"]
            assert status["clock"]["mode"] == "virtual"
            assert status["cpu"]["cycles"] >= 0
            assert len(status["cpu"]["registers"]) == 32
            assert "return_words" in status["forth"]
            lightweight = viewer.request("status", detailed=False)
            assert lightweight["clients"] == 2
            assert lightweight["state"] == "idle"
            assert "forth" not in lightweight
            assert "cpu" not in lightweight
            network = controller.request("network")
            assert network["backend"] == "loopback"
            assert network["guest_rx_queued"] == 0
            forth = controller.request("forth", names=["STATE"])
            assert forth["words"]["STATE"]["name"] == "STATE"
            peek = controller.request("peek", address=0, count=2)
            assert peek["cell_size"] == 8
            assert len(peek["values"]) == 2

            initial = viewer.request("screen", since=-1)
            assert initial["changed"]
            revision = initial["revision"]
            assert not viewer.request("screen", since=revision)["changed"]

            controller.request(
                "send_text",
                text="6 7 * .\n",
                generation=generation,
            )
            raw = wait_until(
                lambda: (
                    result
                    if "42 " in (result := controller.request("raw", since=0))["text"]
                    else None
                )
            )
            assert "42  ok" in raw["text"]

            updated = wait_until(
                lambda: (
                    result
                    if (result := viewer.request("screen", since=revision))["changed"]
                    else None
                )
            )
            snapshot = snapshot_from_wire(updated["snapshot"])
            assert snapshot.find("42")

            paused = controller.request("pause")
            assert paused["paused"]
            stepped = controller.request("step", count=1)
            assert stepped["executed"] == 1
            assert stepped["status"]["paused"]
            assert not controller.request("resume")["paused"]

            resized = controller.request(
                "resize",
                cols=72,
                rows=24,
                generation=generation,
            )
            assert resized["cols"] == 72
            resized_screen = viewer.request("screen", since=updated["revision"])
            assert resized_screen["snapshot"]["cols"] == 72
            assert resized_screen["snapshot"]["rows"] == 24

            png = tmp_path / "shared.png"
            capture = controller.request("capture", png=str(png))
            assert Path(capture["outputs"]["png"]).is_file()

            reset = controller.request("reset", paused=False)
            assert not reset["paused"]
            wait_until(lambda: controller.request("status")["state"] == "idle")
            assert "42" not in controller.request("text")["text"]
    finally:
        server.stop()

    assert not socket_path.exists()
