"""Focused shared-owner facade coverage for semantic simulator sessions."""

from __future__ import annotations

import importlib.util

import pytest

from shared_session import SessionServer, SharedMachine, snapshot_from_wire
from simulator.runtime import MegaForthRuntime
from simulator.session import (
    DEFAULT_SEMANTIC_QUANTUM_STEPS,
    SEMANTIC_QUANTUM_ENVIRONMENT,
    SimulatorMachineSession,
    SimulatorSharedMachine,
)
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

        root = runtime.find("SIM-IDLE-ROOT")
        assert root is not None
        named = machine.forth(["sim-idle-root", "missing"])
        assert named == {
            "here": runtime.dictionary.here,
            "words": {
                "SIM-IDLE-ROOT": {
                    "name": "SIM-IDLE-ROOT",
                    "header": root.header_address,
                    "code": root.xt,
                }
            },
        }
        assert machine.peek(root.header_address, 1) == {
            "address": root.header_address,
            "cell_size": 8,
            "values": [runtime.memory.read64(root.header_address)],
        }

        with pytest.raises(RuntimeError, match="without emulator hardware"):
            machine.network()

        with pytest.raises(RuntimeError, match="rebuilding the prepared runtime"):
            machine.reset(paused=True)
        assert machine.paused
        assert machine.last_error is not None
    finally:
        machine.stop()


def test_semantic_quantum_prefers_caller_then_environment_then_executor(
    monkeypatch,
) -> None:
    def idle_session(backend: str, **options) -> SimulatorMachineSession:
        runtime = MegaForthRuntime(execution_backend=backend)
        runtime.evaluate(IDLE_ROOT_SOURCE, source_name="simulator-idle-root.f")
        return SimulatorMachineSession(runtime, "SIM-IDLE-ROOT", **options)

    backends = ["python"]
    if importlib.util.find_spec("_megaforth_native") is not None:
        backends.append("native")
    assert DEFAULT_SEMANTIC_QUANTUM_STEPS == {"native": 65_536, "python": 8_192}

    monkeypatch.delenv(SEMANTIC_QUANTUM_ENVIRONMENT, raising=False)
    for backend in backends:
        with idle_session(backend) as session:
            assert session.runtime.execution_backend == backend
            expected = DEFAULT_SEMANTIC_QUANTUM_STEPS[backend]
            assert session.semantic_quantum_steps == expected
            assert session.backend._semantic_quantum_steps == expected

    monkeypatch.setenv(SEMANTIC_QUANTUM_ENVIRONMENT, "40000")
    for backend in backends:
        with idle_session(backend) as session:
            assert session.semantic_quantum_steps == 40_000
            assert session.backend._semantic_quantum_steps == 40_000

    machine = SimulatorSharedMachine(
        idle_session(backends[-1], semantic_quantum_steps=96)
    )
    try:
        assert machine.semantic_session.backend._semantic_quantum_steps == 96
        semantic = machine.status()["semantic_execution"]
        assert semantic["backend"] == backends[-1]
        assert semantic["quantum_steps"] == 96
    finally:
        machine.stop()

    for invalid in ("0", "-5", "fast", ""):
        monkeypatch.setenv(SEMANTIC_QUANTUM_ENVIRONMENT, invalid)
        with pytest.raises(ValueError, match=SEMANTIC_QUANTUM_ENVIRONMENT):
            idle_session("python")


# Two phase changes separated by more work than one small quantum, then idle.
# Each event packs a sequence above the low phase byte.
PHASE_ROOT_SOURCE = b"""
VARIABLE SIM-PHASE
: IDLE  [ 0 C, ] ;
: SIM-PHASE-WORK  0 400 0 DO I + LOOP DROP ;
: SIM-PHASE-ROOT
    259 SIM-PHASE ! SIM-PHASE-WORK
    512 SIM-PHASE ! SIM-PHASE-WORK
    BEGIN IDLE AGAIN ;
"""


def test_phase_profile_samples_semantic_boundaries() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(PHASE_ROOT_SOURCE, source_name="simulator-phase-root.f")
    cell = runtime.find("SIM-PHASE").body_address
    machine = SimulatorSharedMachine(
        SimulatorMachineSession(
            runtime, "SIM-PHASE-ROOT", semantic_quantum_steps=96
        )
    )
    machine.paused = True
    machine.start()
    try:
        with pytest.raises(ValueError, match="complete RAM or external"):
            machine.start_phase_profile(1 << 62, 8, generation=1)
        with pytest.raises(RuntimeError, match="stale phase profile generation"):
            machine.start_phase_profile(cell, 8, generation=2)

        started = machine.start_phase_profile(cell, 8, generation=1)
        assert started["status"] == "active"
        assert started["batch_step_bound"] is None
        assert started["initial"] == {"event": 0, "sequence": 0, "phase": 0}

        while not machine.semantic_session.idle:
            machine.step(1)

        profile = machine.stop_phase_profile()
        assert profile["status"] == "stopped"
        assert profile["stopped_steps"] == machine.total_steps
        assert profile["successful_samples"] == machine.total_batches + 1
        assert profile["observed_transitions"] == 2
        assert profile["coalesced_transitions"] == 0
        transitions = profile["transitions"]
        assert [(item["sequence"], item["phase"]) for item in transitions] == [
            (1, 3),
            (2, 0),
        ]
        assert transitions[0]["step_upper_bound"] <= transitions[1][
            "step_lower_bound"
        ]
        for item in transitions:
            assert item["source"] == "semantic_boundary"
            assert item["step_upper_bound"] > item["step_lower_bound"]
        assert machine.phase_profile()["status"] == "disabled"
    finally:
        machine.stop()


def test_forth_diagnostics_resolve_newest_created_binding_and_live_value() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        IDLE_ROOT_SOURCE
        + b" VARIABLE SIM-DIAGNOSTIC 41 SIM-DIAGNOSTIC !"
        + b" VARIABLE SIM-DIAGNOSTIC 42 SIM-DIAGNOSTIC !",
        source_name="simulator-diagnostic-binding.f",
    )
    newest = runtime.find("SIM-DIAGNOSTIC")
    assert newest is not None
    machine = SimulatorSharedMachine(
        SimulatorMachineSession(runtime, "SIM-IDLE-ROOT")
    )

    named = machine.forth(["sim-diagnostic", "SIM-DIAGNOSTIC"])

    assert named == {
        "here": runtime.dictionary.here,
        "words": {
            "SIM-DIAGNOSTIC": {
                "name": "SIM-DIAGNOSTIC",
                "header": newest.header_address,
                "code": newest.xt,
                "data_address": newest.body_address,
                "value": 42,
            }
        },
    }
    runtime.memory.write64(newest.body_address, 43)
    assert machine.forth(["SIM-DIAGNOSTIC"])["words"]["SIM-DIAGNOSTIC"][
        "value"
    ] == 43
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


@pytest.mark.parametrize("progress", [True, False])
def test_host_handoffs_preserve_each_semantic_boundary_without_per_batch_sleep(monkeypatch, progress):
    from types import SimpleNamespace
    import simulator.session as module
    from simulator.rich_terminal_host import SemanticBatchStop
    from simulator.session import SimulatorSessionRun

    clock = SimpleNamespace(now=0.0)
    records = []
    machine = SimulatorSharedMachine.__new__(SimulatorSharedMachine)
    machine._stopping = machine.paused = False
    machine.last_error = None
    machine.total_steps = machine.total_batches = machine.total_external_events = 0
    machine.idle_sleep_s = 0.002
    machine._phase_profile = None
    locked = False

    class Condition:
        def __enter__(self):
            nonlocal locked
            assert not locked
            locked = True

        def __exit__(self, *args):
            nonlocal locked
            locked = False

        def wait(self, *, timeout):
            assert locked
            records.append(("wait", timeout))
            machine._stopping = True

    machine.condition = Condition()

    def boundary():
        assert locked
        clock.now += 0.001
        records.append(("boundary", machine.total_batches + 1))
        if machine.total_batches == 7:
            machine._stopping = True
        return SimulatorSessionRun(
            semantic_steps=8192 if progress else 0,
            external_events_applied=1 if progress else 0,
            stop_reason=SemanticBatchStop.YIELDED,
            terminal_progress=False,
        )

    def handoff(delay):
        assert not locked
        assert delay == 0
        records.append(("handoff", machine.total_batches))

    machine.session = SimpleNamespace(
        rich_terminal_failure=None, rich_terminal_lost=False, halted=False,
        idle=False, rich_terminal_work_pending=False,
        last_batch_made_progress=progress, run_boundary=boundary,
    )
    monkeypatch.setattr(module, "time", SimpleNamespace(
        monotonic=lambda: clock.now, sleep=handoff,
    ))
    monkeypatch.setattr(module, "sys", SimpleNamespace(getswitchinterval=lambda: 0.005))
    machine._run_loop()
    assert machine.last_error is None
    if progress:
        assert [r for r in records if r[0] == "boundary"] == [
            ("boundary", n) for n in range(1, 9)
        ]
        assert [r for r in records if r[0] == "handoff"] == [("handoff", 5)]
        assert machine.total_steps == 8 * 8192
        assert machine.total_external_events == 8
    else:
        assert records == [("boundary", 1), ("wait", 0.002)]
        assert machine.total_steps == machine.total_external_events == 0


def test_owner_sleeps_until_a_blocked_idle_deadline() -> None:
    # The root naps in IDLE-MS.  The owner waits for each deadline instead of
    # spinning, so the guest does only a few steps per nap.
    import time as _time

    runtime = MegaForthRuntime()
    runtime.rtc.bind_monotonic_clock(_time.monotonic_ns)
    runtime.evaluate(
        b"VARIABLE NAPS 0 NAPS ! "
        b": SIM-NAP-ROOT BEGIN 30 IDLE-MS 1 NAPS +! AGAIN ;",
        source_name="simulator-nap-root.f",
    )
    naps = runtime.find("NAPS").body_address
    session = SimulatorMachineSession(runtime, "SIM-NAP-ROOT")
    machine = SimulatorSharedMachine(session)
    machine.start()
    try:
        started = _time.monotonic()
        deadline = started + 5.0
        while runtime.memory.read64(naps) < 3 and _time.monotonic() < deadline:
            _time.sleep(0.01)
        elapsed = _time.monotonic() - started
        assert runtime.memory.read64(naps) >= 3
        assert elapsed >= 0.08              # three 30 ms naps
        assert machine.total_steps < 200    # asleep between naps
        assert machine.last_error is None
    finally:
        machine.stop()
