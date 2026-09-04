"""Focused shared-owner facade coverage for semantic simulator sessions."""

from __future__ import annotations

import pytest

from shared_session import SessionServer, SharedMachine, snapshot_from_wire
from simulator.runtime import MegaForthRuntime
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.simulator.test_simulator_session import (
    SESSION_ROOT_SOURCE,
    _rich_terminal_config,
)
from tests.test_rich_terminal_dual_backend import (
    LIVE_HANDSHAKE_SCENARIO_SOURCE,
    ONE_CORE_UART_LOCK_SHIMS,
    RICH_TERMINAL_SOURCE,
)


IDLE_ROOT_SOURCE = b"""
: IDLE  [ 0 C, ] ;
: SIM-IDLE-ROOT  BEGIN IDLE AGAIN ;
"""


def _cell_session() -> SimulatorMachineSession:
    runtime = _load_exceptions()
    runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + RICH_TERMINAL_SOURCE.read_bytes(),
        source_name="one-core-uart-lock-shims+rich-terminal.f",
        step_budget=1_000_000,
    )
    runtime.evaluate(
        LIVE_HANDSHAKE_SCENARIO_SOURCE + SESSION_ROOT_SOURCE,
        source_name="simulator-shared-session-cell-root.f",
    )
    return SimulatorMachineSession(
        runtime,
        "DBS-RUN",
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(),
    )


def test_facade_reports_semantic_work_without_hardware_statistics() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(IDLE_ROOT_SOURCE, source_name="simulator-idle-root.f")
    session = SimulatorMachineSession(runtime, "SIM-IDLE-ROOT")

    with pytest.raises(ValueError, match="host profiling is unavailable"):
        SimulatorSharedMachine(session, host_profile=True)

    machine = SimulatorSharedMachine(session)
    assert isinstance(machine, SharedMachine)
    machine.paused = True
    machine.start()
    try:
        result = machine.step(4)

        assert result["boundaries"] == 1
        assert result["semantic_steps"] > 0
        assert result["external_events_applied"] == 0
        assert result["stop_reason"] == "idle"
        assert "cycles" not in result

        status = machine.status()
        assert status["backend"] == "simulator"
        assert status["state"] == "paused"
        assert status["paused"]
        assert status["idle"]
        assert not status["halted"]
        assert status["step_unit"] == "semantic_step"
        assert status["steps"] == result["semantic_steps"]
        assert status["batch_unit"] == "semantic_boundary"
        assert status["batches"] == 1
        assert status["external_events_applied"] == 0
        assert status["simulator"] == {
            "booted": True,
            "suspended": True,
            "semantic_steps": result["semantic_steps"],
            "semantic_boundaries": 1,
            "external_events_applied": 0,
        }
        for absent in ("cpu", "clock", "nic", "host_profile"):
            assert absent not in status

        for diagnostic in (
            machine.network,
            lambda: machine.forth(["SIM-IDLE-ROOT"]),
            lambda: machine.peek(0),
            machine.phase_profile,
        ):
            with pytest.raises(RuntimeError, match="without emulator hardware"):
                diagnostic()

        with pytest.raises(RuntimeError, match="rebuilding the prepared runtime"):
            machine.reset(paused=True)
        assert machine.paused
        assert machine.last_error is not None
    finally:
        machine.stop()


def test_unchanged_server_dispatch_reaches_cell_view_and_input_flow() -> None:
    machine = SimulatorSharedMachine(_cell_session())
    machine.paused = True
    server = SessionServer(machine, "unused-simulator-session.sock")
    machine.start()
    try:
        stepped = server.dispatch("step", {"count": 32})
        assert stepped["boundaries"] > 0
        assert stepped["semantic_steps"] > 0
        assert stepped["stop_reason"] == "idle"
        assert "cycles" not in stepped

        status = server.dispatch("status", {"detailed": False})
        assert status["backend"] == "simulator"
        assert status["clients"] == 0
        assert status["generation"] == 1
        assert status["revision"] == 1
        assert status["terminal"] == [2, 2]
        assert status["rich_terminal"]["state"] == "ACTIVE"
        assert not status["rich_terminal"]["pending"]
        assert "simulator" not in status

        screen = server.dispatch("screen", {"since": -1})
        assert screen["changed"]
        assert screen["revision"] == 1
        snapshot = snapshot_from_wire(screen["snapshot"])
        assert snapshot.lines() == ["AB", "C "]
        assert (snapshot.cursor_row, snapshot.cursor_col) == (1, 1)
        assert snapshot.cursor_visible
        assert server.dispatch("screen", {"since": 1}) == {
            "changed": False,
            "revision": 1,
        }
        assert server.dispatch("text", {}) == {
            "revision": 1,
            "text": "AB\nC",
        }

        stale = server.dispatch(
            "send_text",
            {"text": "stale", "generation": 0},
        )
        assert stale == {"status": "stale_generation", "accepted_bytes": 0}

        sent = server.dispatch(
            "send_text",
            {"text": "x", "generation": status["generation"]},
        )
        assert sent == {"status": "progress", "accepted_bytes": 1}

        delivered = server.dispatch("step", {"count": 32})
        assert delivered["boundaries"] > 0
        assert delivered["external_events_applied"] > 0
        assert delivered["stop_reason"] == "idle"
        assert machine.semantic_session.idle
    finally:
        machine.stop()
