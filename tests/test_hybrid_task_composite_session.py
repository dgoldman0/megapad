"""Real parked task callbacks share the prepared session's input/close owner."""

from contextlib import contextmanager
from pathlib import Path

import pytest

from asm import assemble
from hybrid.session import HybridSession, HybridSharedMachine
from shared.cells import u64
from shared_session import SessionServer
from simulator.errors import ExecutionError
from simulator.foreign_control import ForeignContinuation
from simulator.ir import Call, Literal, Return
from tests.test_hybrid_task_session import (
    EXCEPTIONS, _callback_code, _signature, _task_grants, owner,
)


IDLE = Path(__file__).parent / "simulator" / "fixtures" / "kdos-idle-2791-2805.f"


def _prepare(hybrid, adapter, *, rethrow=False, parent_idle=False, machine_noops=0,
             deadline=False):
    runtime = hybrid.semantic
    runtime.evaluate(EXCEPTIONS.read_bytes(), source_name=str(EXCEPTIONS))
    runtime.evaluate(IDLE.read_bytes(), source_name=str(IDLE))
    runtime.evaluate(b": LEAF " + (b"5 IDLE-UNTIL" if deadline else b"IDLE")
                     + b" -17 THROW ;")
    code, call, stub = _callback_code()
    if machine_noops:
        source = ("mov r12, r3\nafter_pc:\naddi r12, 0\n"
                  + "mov r12, r12\n" * machine_noops
                  + "call:\ncall.l r12\nret.l\nstub:\nret.l")
        labels = {}
        assemble(source, labels_out=labels)
        source = source.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
        code, call, stub = bytes(assemble(source)), labels["call"], labels["stub"]
    with adapter.registration_batch() as batch:
        child = batch.define_operation("CHILD", code, _signature(),
            max_instructions=100, max_callbacks=1)
        leaf = batch.capture_export("LEAF", _signature(), task_grants=_task_grants(runtime))
        batch.set_callbacks(child, ((call, stub, leaf),))
        operations = ((Call(runtime.find("IDLE").xt),) if parent_idle else ())
        operations += (Literal(child.xt), Call(runtime.find("CATCH").xt))
        if rethrow:
            operations += (Call(runtime.find("THROW").xt),)
        operations += (Return(),)
        policy = batch.define_colon("POLICY", operations)
        signature = _signature(0, 0 if rethrow else 1)
        parent = batch.define_operation("PARENT", code, signature,
            max_instructions=100, max_callbacks=1)
        export = batch.capture_export(policy, signature, task_grants=_task_grants(runtime),
            dynamic_targets=(child,))
        batch.set_callbacks(parent, ((call, stub, export),))
    runtime.evaluate(b": ROOT 99 " + (b"['] PARENT CATCH" if rethrow else b"PARENT")
                     + b" KEY EMIT 5 ;")


@contextmanager
def _session(hybrid, tmp_path, *, machine_quantum=None):
    session = HybridSession(hybrid, "ROOT", semantic_step_budget=4096,
                            semantic_quantum_steps=4096,
                            machine_quantum_instructions=machine_quantum)
    machine = HybridSharedMachine(session)
    machine.paused = True
    path = tmp_path / "prepared-composite.sock"
    server = SessionServer(machine, str(path))
    try:
        machine.start()
        assert server._socket is None and not path.exists()
        yield session, server
    finally:
        server.stop()
    assert not path.exists()


def _until(server, expected, *, max_turns=2048):
    """Bound host scheduling turns without creating a guest wake or timeout API."""
    turns = []
    for _ in range(max_turns):
        result = server.dispatch("step", {"count": 1})
        turns.append(result)
        if result["stop_reason"] == expected:
            return turns
        assert result["stop_reason"] == "yielded", result
    raise TimeoutError("test host watchdog exhausted its bounded session turns")


def _parked(hybrid, adapter, depth):
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    suspended = runtime._suspended_execution
    task = runtime._foreign_tasks._task_root
    assert suspended is not None and task is not None
    assert suspended.task_root is task and suspended.context is context
    assert context.suspended and not context.reusable
    assert len(task.frames) == depth == len(adapter.active_invocations)
    cookies = tuple(frame.cookie for frame in task.frames)
    assert all(type(cookie) is ForeignContinuation and not cookie.retired for cookie in cookies)
    assert all(runtime.memory.read64(cookie.slot_address) == cookie.raw_cookie for cookie in cookies)
    assert task.control.live_count == depth
    handler = runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address)
    # The parent-only fixture pauses before entering its child CATCH. Both
    # depth-two fixtures have executed the actual unchanged CATCH prologue.
    assert bool(handler) is (depth == 2)
    return suspended, task, cookies, adapter.last_receipt()


def _input(server, text):
    generation = server.dispatch("status", {})["generation"]
    assert server.dispatch("send_text", {"text": text, "generation": generation}) == {
        "status": "progress", "accepted_bytes": len(text.encode()),
    }


@pytest.mark.parametrize("rethrow", (False, True))
@pytest.mark.parametrize("machine_quantum", (None, 1, 2, 3, 5, 64))
def test_real_child_idle_input_wake_throw_and_outer_key_share_one_session_owner(
    owner, tmp_path, rethrow, machine_quantum,
):
    hybrid, adapter, executor = owner
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    _prepare(hybrid, adapter, rethrow=rethrow)
    data, returns = context.data, context.returns
    with _session(hybrid, tmp_path, machine_quantum=machine_quantum) as (session, server):
        first = _until(server, "idle")
        suspended, task, cookies, receipt = _parked(hybrid, adapter, 2)
        original_meter = task.ledger.meter
        initial_steps = original_meter.steps
        assert context.data is data and context.returns is returns
        assert receipt.root_entries == receipt.root_callbacks == 2
        assert (receipt.root_instructions, receipt.root_cycles) == (6, 8)
        assert server.dispatch("raw", {"since": 0})["text"] == ""
        status = server.dispatch("status", {})
        assert status["semantic_execution"]["backend"] == executor
        assert status["machine_execution"]["callback_semantic_steps"] == task.ledger.semantic_steps > 0
        assert status["machine_execution"]["max_machine_depth"] == 2
        task_status = status["task_execution"]
        assert task_status["profile"] == "shared_task_stack"
        assert task_status["callback_executor"] == "python_reference"
        assert task_status["quantum_instructions"] == machine_quantum
        assert task_status["registered_words"] == ["CHILD", "PARENT"]
        assert task_status["active_depth"] == 2
        assert (task_status["instructions"], task_status["cycles"],
                task_status["transitions"], task_status["callback_requests"]) == (6, 8, 2, 2)
        assert task_status["callback_semantic_steps"] == task.ledger.semantic_steps
        # Rejected input does not wake, validate native work, rotate tokens or
        # advance either clock. Valid input is consumed only by outer KEY.
        assert server.dispatch("send_text", {
            "text": "!", "generation": status["generation"] - 1,
        }) == {"status": "stale_generation", "accepted_bytes": 0}
        assert runtime._suspended_execution is suspended
        assert adapter.last_receipt() is receipt and original_meter.steps == initial_steps
        _input(server, "K")
        assert runtime.uart_input_pending == 1
        second = _until(server, "completed")

        assert session.halted and not session.backend.suspended
        assert server.dispatch("raw", {"since": 0})["text"] == "K"
        assert context.data is data and context.returns is returns
        assert context.data.snapshot() == (99, u64(-17), 5)
        assert context.returns.snapshot() == ()
        assert context.returns.pointer == context.returns.empty_pointer
        assert context.reusable and not context.suspended
        assert all(cookie.retired for cookie in cookies)
        assert adapter.active_invocations == () and runtime._foreign_tasks._task_root is None
        assert runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address) == 0
        report, final = runtime._foreign_tasks.last_dispatch, adapter.last_receipt()
        assert report.root_id == task.ledger.root_id == final.root_id
        assert task.ledger.meter is original_meter
        assert report.completed and report.cancelled
        assert (report.machine_instructions, report.machine_cycles, report.entries, report.callbacks) == (
            6 if rethrow else 8, 8 if rethrow else 12, 2, 2)
        assert report.semantic_steps > status["machine_execution"]["callback_semantic_steps"]
        completed_status = server.dispatch("status", {})
        execution = completed_status["machine_execution"]
        assert execution["callback_semantic_steps"] == report.semantic_steps
        assert execution["instructions"] == report.machine_instructions
        assert execution["cycles"] == report.machine_cycles
        assert execution["max_machine_depth"] == 2
        final_task = completed_status["task_execution"]
        assert final_task["active_depth"] == 0
        assert final_task["quantum_instructions"] == machine_quantum
        assert final_task["instructions"] == report.machine_instructions
        assert final_task["cycles"] == report.machine_cycles
        assert final_task["callback_semantic_steps"] == report.semantic_steps
        assert final_task["transitions"] == final_task["callback_requests"] == 2
        assert final_task["segments"] == execution["segments"]
        assert final_task["max_machine_depth"] == 2
        assert session.semantic_steps_total == sum(turn["semantic_steps"] for turn in first + second)
        # This legacy session injects the accepted byte immediately under its
        # owner. No enhanced-terminal queued event is applied by a later turn.
        assert sum(turn["external_events_applied"] for turn in second) == 0
        assert runtime.uart_input_pending == 0


def test_multi_step_keeps_zero_semantic_machine_yields_as_real_progress(owner, tmp_path):
    hybrid, adapter, _executor = owner
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    _prepare(hybrid, adapter, machine_noops=5)
    with _session(hybrid, tmp_path, machine_quantum=1) as (session, server):
        first = server.dispatch("step", {"count": 1})
        assert first["boundaries"] == 1 and first["stop_reason"] == "yielded"
        assert first["semantic_steps"] > 0 and hybrid.machine_instructions == 1
        task = runtime._foreign_tasks._task_root
        meter, before = task.ledger.meter, session.semantic_steps_total
        turns = server.dispatch("step", {"count": 2})
        assert turns["boundaries"] == 2 and turns["stop_reason"] == "yielded"
        assert turns["semantic_steps"] == turns["external_events_applied"] == 0
        assert session.semantic_steps_total == before == meter.steps
        assert hybrid.machine_instructions == hybrid.machine_cycles == 3
        assert hybrid.callback_requests == hybrid.callback_semantic_steps == 0
        assert runtime._foreign_tasks._task_root is task and len(adapter.active_invocations) == 1

        _until(server, "idle")
        _suspended, same_task, cookies, receipt = _parked(hybrid, adapter, 2)
        assert same_task is task and task.ledger.meter is meter
        assert (receipt.root_instructions, receipt.root_cycles) == (16, 18)
        _input(server, "K")
        _until(server, "completed")
        assert server.dispatch("raw", {"since": 0})["text"] == "K"
        assert context.data.snapshot() == (99, u64(-17), 5) and context.returns.snapshot() == ()
        assert context.reusable and all(cookie.retired for cookie in cookies)
        assert (hybrid.machine_instructions, hybrid.machine_cycles) == (18, 22)
        assert hybrid.transitions == hybrid.callback_requests == 2
        assert hybrid.callback_semantic_steps == runtime._foreign_tasks.last_dispatch.semantic_steps


@pytest.mark.parametrize("poll", ("idle", "idle_wake_delay_s"))
def test_task_deadline_poll_rejects_changed_witness_and_retires_backend_handle(owner, tmp_path, poll):
    hybrid, adapter, _executor = owner
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    _prepare(hybrid, adapter, deadline=True)
    with _session(hybrid, tmp_path) as (session, server):
        _until(server, "idle")
        _suspended, task, cookies, receipt = _parked(hybrid, adapter, 2)
        before = (context.data.snapshot(), task.ledger.semantic_steps,
                  runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address))
        assert runtime.idle_deadline_ms == 5
        # The engine's independent issued association must identify the old
        # task owner even when its public witness projection was damaged.
        object.__setattr__(runtime._foreign_tasks._parked_task, "root", object())
        with pytest.raises(ExecutionError):
            getattr(session, poll)
        assert not session.backend.suspended and not context.suspended
        assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
        assert adapter.active_invocations == () and adapter.last_receipt() is receipt
        assert all(cookie.retired for cookie in cookies)
        assert context.data.snapshot() == before[0] and context.returns.snapshot() == ()
        assert not context.reusable and context.host_control_fault is not None
        assert runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address) == before[2]
        assert hybrid.callback_semantic_steps == before[1]
        assert (hybrid.machine_instructions, hybrid.machine_cycles) == (6, 8)


@pytest.mark.parametrize("depth", (1, 2))
@pytest.mark.parametrize("host_timeout", (False, True))
def test_production_stop_cancels_real_parked_chain_and_preserves_host_timeout(
    owner, tmp_path, depth, host_timeout,
):
    hybrid, adapter, _executor = owner
    runtime, context = hybrid.semantic, hybrid.semantic.main_context
    _prepare(hybrid, adapter, parent_idle=True)
    session = HybridSession(hybrid, "ROOT", semantic_step_budget=4096,
                            semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "cancel-composite.sock"))
    backend, cpu, control = session.backend, hybrid._cpu, hybrid._control_buffer
    machine.start()
    try:
        _until(server, "idle")
        if depth == 2:
            # Wakes parent IDLE; the byte remains queued because callbacks do
            # not admit KEY. Child now parks at its own unchanged IDLE.
            _input(server, "K")
            _until(server, "idle")
        suspended, task, cookies, receipt = _parked(hybrid, adapter, depth)
        prefix = context.data.snapshot()
        handler_address = runtime.find("_TASK-HANDLERS").body_address
        handler_prefix = runtime.memory.read64(handler_address)
        assert (receipt.root_instructions, receipt.root_cycles) == (3 * depth, 4 * depth)
        charged = task.ledger.semantic_steps
        if host_timeout:
            # This is a host watchdog failure followed by existing production
            # stop/cleanup, not a synthesized guest fault or session timeout API.
            failure = TimeoutError("host watchdog expired at the parked task boundary")
            with pytest.raises(TimeoutError) as caught:
                try:
                    raise failure
                finally:
                    server.stop()
            assert caught.value is failure
        else:
            server.stop()
        assert backend.closed and hybrid.closed
        assert adapter.active_invocations == ()
        assert runtime._suspended_execution is None and runtime._foreign_tasks._task_root is None
        assert not context.suspended
        assert context.reusable is (depth == 1)
        assert (context.host_control_fault is None) is (depth == 1)
        assert context.data.snapshot() == prefix and context.returns.snapshot() == ()
        # Host cancellation restores control metadata but cannot fabricate the
        # guest THROW stores that would unwind the retained CATCH handler.
        assert runtime.memory.read64(handler_address) == handler_prefix
        assert all(cookie.retired for cookie in cookies)
        assert adapter.last_receipt() is receipt
        assert hybrid.callback_semantic_steps == charged
        assert hybrid.machine_instructions == 3 * depth and hybrid.machine_cycles == 4 * depth
        assert not runtime._active_dispatches
        control.extend(b"native ownership released")
        cpu.set_reg(4, 123)
        assert cpu.get_reg(4) == 123
        assert suspended.context is context
    finally:
        server.stop()
