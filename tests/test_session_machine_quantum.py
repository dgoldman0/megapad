"""Machine scheduling options preserve ordinary session wake/backpressure rules."""

import pytest
from types import SimpleNamespace

from rich_terminal import AdmissionStatus
from simulator.ir import Idle, Return
from simulator.rich_terminal_host import SemanticBatchStop, SimulatorSessionBackend
from simulator.runtime import MegaForthRuntime
from simulator.session import SimulatorMachineSession, SimulatorSharedMachine
from tests.simulator.test_rich_terminal_host import _limits


class Indexable:
    def __index__(self):
        return 1


class IntegerSubclass(int):
    pass


@pytest.mark.parametrize("value,error", (
    (True, TypeError), (1.0, TypeError), ("1", TypeError),
    (Indexable(), TypeError), (IntegerSubclass(1), TypeError),
    (0, ValueError), (-1, ValueError),
))
def test_invalid_machine_quantum_does_not_claim_runtime_owner(value, error):
    runtime = MegaForthRuntime(execution_backend="python")
    with pytest.raises(error, match="machine_quantum_instructions"):
        SimulatorSessionBackend(runtime, legacy_output_sink=lambda payload: None,
                                machine_quantum_instructions=value)
    assert runtime._session_owner_token is None
    runtime.evaluate(b"1 DROP")


@pytest.mark.parametrize("quantum", (None, 1, 1 << 40))
def test_session_forwards_machine_quantum_only_at_original_dispatch(monkeypatch, quantum):
    runtime = MegaForthRuntime(execution_backend="python")
    runtime.evaluate(b": POLL BEGIN KEY? UNTIL KEY ;")
    original, calls = runtime.run_until_blocked, []

    def run(*args, **kwargs):
        calls.append(dict(kwargs))
        return original(*args, **kwargs)

    monkeypatch.setattr(runtime, "run_until_blocked", run)
    session = SimulatorMachineSession(runtime, "POLL", semantic_quantum_steps=64,
                                      machine_quantum_instructions=quantum)
    try:
        assert session.machine_quantum_instructions == quantum
        assert session.backend.machine_quantum_instructions == quantum
        session.boot()
        first = session.run_boundary()
        assert first.stop_reason is SemanticBatchStop.YIELDED
        assert session.last_batch_made_progress and not session.idle
        assert len(calls) == 1
        if quantum is None:
            assert "machine_quantum_instructions" not in calls[0]
        else:
            assert calls[0]["machine_quantum_instructions"] == quantum
        session.backend.inject_legacy_uart_input(b"K")
        final = session.run_boundary()
        assert final.stop_reason is SemanticBatchStop.COMPLETED
        assert len(calls) == 1
        assert runtime.main_context.data.snapshot() == (ord("K"),)
    finally:
        session.close()


def test_machine_quantum_does_not_create_an_ordinary_idle_wake():
    runtime = MegaForthRuntime(execution_backend="python")
    word = runtime.define_colon("WAIT", (Idle(), Return()))
    session = SimulatorMachineSession(runtime, word.xt, machine_quantum_instructions=1)
    try:
        session.boot()
        first = session.run_boundary()
        assert first.stop_reason is SemanticBatchStop.IDLE and session.idle
        token = runtime._suspended_execution.handle
        before = runtime.timer.counter
        repeated = session.run_boundary()
        assert repeated.stop_reason is SemanticBatchStop.IDLE
        assert repeated.semantic_steps == 0 and not session.last_batch_made_progress
        assert runtime._suspended_execution.handle is token
        assert runtime.timer.counter == before
        session.backend.inject_legacy_uart_input(b"K")
        assert session.run_boundary().stop_reason is SemanticBatchStop.COMPLETED
    finally:
        session.close()


def test_machine_quantum_preserves_ordinary_terminal_backpressure():
    runtime = MegaForthRuntime(execution_backend="python")
    runtime.evaluate(b": POLL BEGIN 65 EMIT AGAIN ;")
    backend = SimulatorSessionBackend(runtime, legacy_output_sink=lambda payload: None,
        semantic_quantum_steps=12, machine_quantum_instructions=1)
    lease = backend.attach_rich_terminal(_limits(high_batches=1, low_batches=0))
    try:
        assert backend.run_semantic_batch(entry="POLL", step_budget=1000).stop_reason is SemanticBatchStop.YIELDED
        before = runtime.timer.counter
        paused = backend.run_semantic_batch()
        assert paused.stop_reason is SemanticBatchStop.HOST_BACKPRESSURE
        assert paused.semantic_steps == 0 and runtime.timer.counter == before
        delivery = lease.poll_egress().delivery
        assert delivery is not None and set(delivery.batch.payload) == {65}
        assert delivery.release() is AdmissionStatus.ACCEPTED
        assert backend.run_semantic_batch().stop_reason is SemanticBatchStop.YIELDED
    finally:
        lease.close()
        backend.close()


@pytest.mark.parametrize("poll", ("idle", "idle_wake_delay_s"))
def test_owner_loop_pauses_and_reports_poll_failure_instead_of_exiting(monkeypatch, poll):
    import simulator.session as module

    failure = RuntimeError("parked task clock/proof rejected")
    machine = SimulatorSharedMachine.__new__(SimulatorSharedMachine)
    machine._stopping = machine.paused = False
    machine.last_error = None
    machine.total_steps = machine.total_batches = machine.total_external_events = 0
    machine.idle_sleep_s, machine.idle_wait_cap_s = 0.002, 1.0
    waits = []

    class Condition:
        def __enter__(self):
            return self

        def __exit__(self, *args):
            pass

        def wait(self, *, timeout):
            # The surviving owner loop reaches its ordinary paused wait. No
            # clock advancement, second scheduler or real sleeping is needed.
            assert machine.paused
            assert machine.last_error == "RuntimeError: parked task clock/proof rejected"
            waits.append(timeout)
            machine._stopping = True

    class Session:
        halted = rich_terminal_work_pending = rich_terminal_lost = False
        rich_terminal_failure = None

        @property
        def idle(self):
            if poll == "idle":
                raise failure
            return True

        @property
        def idle_wake_delay_s(self):
            raise failure

        def run_boundary(self):
            raise AssertionError("a failed idle poll cannot enter guest execution")

    machine.condition, machine.session = Condition(), Session()
    monkeypatch.setattr(module, "time", SimpleNamespace(monotonic=lambda: 0.0))
    monkeypatch.setattr(module, "sys", SimpleNamespace(getswitchinterval=lambda: 0.005))
    machine._run_loop()
    assert waits == [0.1]
    assert machine.paused and machine.total_batches == machine.total_steps == 0
