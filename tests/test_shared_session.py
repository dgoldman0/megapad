"""Tests for the shared single-owner session protocol."""

from __future__ import annotations

import time
from pathlib import Path

import pytest

from session import MachineSession
from shared_session import (
    SessionClient,
    SessionServer,
    SharedMachine,
    snapshot_from_wire,
    snapshot_to_wire,
)


ROOT = Path(__file__).resolve().parents[1]
BIOS = ROOT / "bios.asm"


def wait_until(predicate, timeout=3.0):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = predicate()
        if value:
            return value
        time.sleep(0.01)
    raise AssertionError("condition did not become true")


def test_snapshot_wire_round_trip():
    with MachineSession.from_bios(BIOS, cols=40, rows=12) as session:
        session.boot()
        session.wait_for_idle(max_steps=2_000_000)
        original = session.snapshot()
        restored = snapshot_from_wire(snapshot_to_wire(original))
        assert restored == original


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

            initial = viewer.request("screen", since=-1)
            assert initial["changed"]
            revision = initial["revision"]
            assert not viewer.request("screen", since=revision)["changed"]

            controller.request("send_text", text="6 7 * .\n")
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

            resized = controller.request("resize", cols=72, rows=24)
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
