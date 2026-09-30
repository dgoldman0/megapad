"""Host-prepared task routines use the actual shared session dispatch boundary."""

from contextlib import contextmanager
from pathlib import Path

import pytest

from asm import assemble
from hybrid.runtime import HybridRuntime
from hybrid.session import HybridSession, HybridSharedMachine
from hybrid.task_adapter import NativeTaskAdapter
from shared.cells import u64
from shared.foreign_abi import ForeignSignatureV1, ForeignSpanV1
from shared_session import SessionServer
from simulator.ir import Call, Literal, Return
from simulator.memory import EXTERNAL_BASE


EXCEPTIONS = Path(__file__).parent / "simulator" / "fixtures" / "kdos-exceptions-618-675.f"


@pytest.fixture(params=("python", "native"))
def owner(request):
    native = pytest.importorskip("_mp64_accel")
    if getattr(native, "_TASK_ROUTINE_TRANSPORT_REVISION", 0) < 2:
        pytest.skip("native task transport revision 2 is required")
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    hybrid = HybridRuntime.create(executor=request.param,
        geometry={"bank0_size": 65536, "external_size": 65536})
    runtime, base = hybrid.semantic, EXTERNAL_BASE + 0x1000
    runtime.configure_dictionary_bounds(base, EXTERNAL_BASE + 0x10000, runtime.main_context)
    runtime.allot_dictionary(base - runtime.dictionary.here, runtime.main_context)
    adapter = NativeTaskAdapter(hybrid)
    try:
        yield hybrid, adapter, request.param
    finally:
        hybrid.close()


def _signature(inputs=0, outputs=0):
    return ForeignSignatureV1(input_cells=inputs, output_cells=outputs)


def _task_grants(runtime):
    context = runtime.main_context
    spans = tuple(ForeignSpanV1(base=stack.empty_pointer - 2048, size=2048,
                               access="read_write")
                  for stack in (context.data, context.returns))
    handlers = runtime.find("_TASK-HANDLERS")
    if handlers is not None:
        spans += (ForeignSpanV1(base=handlers.body_address, size=8, access="read_write"),)
    return spans


def _callback_code():
    source = "mov r12, r3\nafter_pc:\naddi r12, 0\ncall:\ncall.l r12\nret.l\nstub:\nret.l"
    labels = {}
    assemble(source, labels_out=labels)
    source = source.replace("addi r12, 0", f"addi r12, {labels['stub'] - labels['after_pc']}")
    return bytes(assemble(source)), labels["call"], labels["stub"]


def _prepare_words(hybrid, adapter, outcome):
    runtime = hybrid.semantic
    runtime.evaluate(EXCEPTIONS.read_bytes(), source_name=str(EXCEPTIONS))
    if outcome != "normal":
        runtime.evaluate(b": RAISE -17 THROW ;")
    code, call, stub = _callback_code()
    child_signature = _signature(1, 1) if outcome == "normal" else _signature()
    parent_signature = child_signature if outcome != "parent_catch" else _signature(0, 1)
    with adapter.registration_batch() as batch:
        child = batch.define_operation("CHILD", code, child_signature,
            max_instructions=100, max_callbacks=1)
        child_export = batch.capture_export("ABS" if outcome == "normal" else "RAISE",
            child_signature, task_grants=_task_grants(runtime))
        batch.set_callbacks(child, ((call, stub, child_export),))
        operations = ((Literal(child.xt), Call(runtime.find("CATCH").xt), Return())
                      if outcome == "parent_catch" else (Call(child.xt), Return()))
        policy = batch.define_colon("POLICY", operations)
        parent = batch.define_operation("PARENT", code, parent_signature,
            max_instructions=100, max_callbacks=1)
        parent_export = batch.capture_export(policy, parent_signature,
            task_grants=_task_grants(runtime),
            dynamic_targets=(child,) if outcome == "parent_catch" else ())
        batch.set_callbacks(parent, ((call, stub, parent_export),))
    source = {
        "normal": b": ROOT 99 -7 PARENT 5 ;",
        "parent_catch": b": ROOT 99 PARENT 5 ;",
        "outer_catch": b": ROOT 99 ['] PARENT CATCH 5 ;",
    }[outcome]
    runtime.evaluate(source)


@contextmanager
def _prepared_session(hybrid, tmp_path):
    session = HybridSession(hybrid, "ROOT", semantic_step_budget=4096,
                            semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    socket_path = tmp_path / "prepared-task.sock"
    server = SessionServer(machine, str(socket_path))
    try:
        # Exercise the production in-process protocol without binding AF_UNIX.
        machine.start()
        assert server._socket is None and not socket_path.exists()
        yield session, machine, server
    finally:
        server.stop()
    assert not socket_path.exists()


@pytest.mark.parametrize("outcome,instructions,cycles,segments", (
    ("normal", 10, 16, 8),
    ("parent_catch", 8, 12, 6),
    ("outer_catch", 6, 8, 4),
))
def test_prepared_session_preserves_task_stacks_and_settles_real_work(
    owner, tmp_path, outcome, instructions, cycles, segments,
):
    hybrid, adapter, executor = owner
    runtime = hybrid.semantic
    _prepare_words(hybrid, adapter, outcome)
    context = runtime.main_context
    original_data, original_returns = context.data, context.returns
    expected = (99, 7 if outcome == "normal" else u64(-17), 5)
    cpu = hybrid._cpu
    cycle_start = cpu.cycle_count

    with _prepared_session(hybrid, tmp_path) as (session, machine, server):
        before = server.dispatch("status", {})
        assert before["semantic_execution"]["backend"] == executor
        assert before["machine_execution"]["instructions"] == 0
        capabilities = before["runtime"]["capabilities"]
        assert capabilities["callback_suspension"] is False
        assert capabilities.get("shared_task_exceptions", False) is False
        assert capabilities.get("composite_suspension", False) is False
        stepped = server.dispatch("step", {"count": 1})
        assert stepped["stop_reason"] == "completed"
        assert session.halted and not session.backend.suspended

        assert context.data is original_data and context.returns is original_returns
        assert context.data.snapshot() == expected
        assert runtime.memory.read_bytes(context.data.pointer, 24) == b"".join(
            value.to_bytes(8, "little") for value in reversed(expected))
        assert context.returns.snapshot() == ()
        assert context.returns.pointer == context.returns.empty_pointer
        assert context.returns._foreign_control is None
        assert context.data._task_effect_guard is context.returns._task_effect_guard is None
        assert context.reusable and adapter.active_invocations == ()
        assert runtime.memory.read64(runtime.find("_TASK-HANDLERS").body_address) == 0

        receipt = adapter.last_receipt()
        report = runtime._foreign_tasks.last_dispatch
        assert report.completed and report.cancelled is (outcome != "normal")
        assert runtime._foreign_tasks._task_root is None
        assert receipt.root_id == report.root_id
        assert receipt.root_instructions == report.machine_instructions == instructions
        assert receipt.root_cycles == report.machine_cycles == cycles == cpu.cycle_count - cycle_start
        assert receipt.root_entries == report.entries == 2
        assert receipt.root_callbacks == report.callbacks == 2
        assert receipt.sequence == segments
        semantic = runtime._foreign_tasks.task_semantic_receipt(
            adapter, adapter._root_token, report.root_id)
        assert semantic.semantic_steps == report.semantic_steps == hybrid.callback_semantic_steps
        assert semantic.sequence == 1 and report.semantic_steps > 0
        if outcome == "normal":
            # Call CHILD + machine-entry tick + ABS + POLICY Return, each once.
            assert report.semantic_steps == 4

        execution = stepped["status"]["machine_execution"]
        assert (execution["instructions"], execution["cycles"], execution["transitions"],
                execution["segments"], execution["callback_requests"],
                execution["callback_semantic_steps"], execution["max_machine_depth"]) == (
            instructions, cycles, 2, segments, 2, report.semantic_steps, 2)
        assert stepped["semantic_steps"] == session.semantic_steps_total
        assert report.semantic_steps < session.semantic_steps_total
        # Terminal status/step polling does not settle the final receipts twice.
        repeated = server.dispatch("step", {"count": 1})
        assert repeated["semantic_steps"] == 0
        assert repeated["status"]["machine_execution"] == execution
        assert runtime._foreign_tasks.task_semantic_receipt(
            adapter, adapter._root_token, report.root_id) is semantic

    assert hybrid.closed


def test_prepared_session_close_reaches_native_owner_after_semantic_close_failure(
    owner, tmp_path, monkeypatch,
):
    hybrid, adapter, _executor = owner
    _prepare_words(hybrid, adapter, "normal")
    session = HybridSession(hybrid, "ROOT", semantic_quantum_steps=4096)
    machine = HybridSharedMachine(session)
    machine.paused = True
    server = SessionServer(machine, str(tmp_path / "close-task.sock"))
    machine.start()
    server.dispatch("step", {"count": 1})
    backend, facade, cpu, control = session.backend, adapter._runner, hybrid._cpu, hybrid._control_buffer
    original = session._close_terminal_frontend
    failure = RuntimeError("semantic terminal close failed after release")

    def fail_close():
        original()
        raise failure

    monkeypatch.setattr(session, "_close_terminal_frontend", fail_close)
    with pytest.raises(BufferError):
        control.extend(b"still owned")
    with pytest.raises(RuntimeError) as caught:
        server.stop()
    assert caught.value is failure
    assert backend.closed and hybrid.closed and adapter.active_invocations == ()
    with pytest.raises(RuntimeError):
        facade.bind_root(99, 10, 0, entry_limit=1)
    control.extend(b"released")
    cpu.set_reg(4, 123)
    assert cpu.get_reg(4) == 123
    session.close()
