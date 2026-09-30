"""Machine scheduling spends one turn allowance without renewing task fuel."""

from dataclasses import dataclass, replace

import pytest

from shared.foreign_abi import ForeignBudgetV1, ForeignSpanV1
from simulator.errors import ExecutionError, StepBudgetExceeded
from simulator.foreign_cursor import MachineTurn
from simulator.foreign_runtime import ForeignTaskBudgetExceeded, ForeignTaskError
from simulator.memory import EXTERNAL_BASE
from simulator.runtime import ExecutionResult, IdleWake, YieldedExecution
from tests.simulator.foreign_reference import Callback, Input, Reply, Return, Store
from tests.simulator.test_foreign_dispatch import (
    ObservedAdapter, adapter, assert_finished, capture, runtime, signature,
)


@dataclass(frozen=True)
class Transition:
    turn: int
    name: str
    quantum: int
    inputs: tuple
    data: tuple
    pointer: int
    event: object


class QuantumAdapter:
    """Observe real reference transitions without inventing scheduling work."""

    def __init__(self, inner, result):
        self.inner, self.result = inner, result
        self.turn = 1
        self.transitions = []
        self.validations = []
        self.cancelled = []
        self.zero_advance = False

    def _record(self, name, args, kwargs):
        context = self.result.main_context
        data, pointer = context.data.snapshot(), context.data.pointer
        actual_kwargs = kwargs
        if name == "advance" and self.zero_advance:
            actual_kwargs = dict(kwargs, budget=replace(kwargs["budget"], quantum_instructions=0))
        event = getattr(self.inner, name)(*args, **actual_kwargs)
        values = args[1] if name in ("begin", "reply") else ()
        self.transitions.append(Transition(self.turn, name, kwargs["budget"].quantum_instructions,
                                           values, data, pointer, event))
        return event

    def begin(self, *args, **kwargs):
        return self._record("begin", args, kwargs)

    def advance(self, *args, **kwargs):
        return self._record("advance", args, kwargs)

    def reply(self, *args, **kwargs):
        return self._record("reply", args, kwargs)

    def cancel_suffix(self, *args, **kwargs):
        outcome = self.inner.cancel_suffix(*args, **kwargs)
        self.cancelled.append(outcome)
        return outcome

    def cancel_all(self):
        outcome = self.inner.cancel_all()
        self.cancelled.append(outcome)
        return outcome

    def last_receipt(self):
        return self.inner.last_receipt()

    def validate_parked(self, root_token, operation_token, request_token=None):
        receipt = self.inner.last_receipt()
        answer = self.inner.validate_parked(root_token, operation_token, request_token)
        assert answer is True and self.inner.last_receipt() is receipt
        self.validations.append((self.turn, root_token, operation_token, request_token, receipt))
        return True


def register(result, inner, observed, name, script, *, inputs=0, outputs=0,
             grants=(), instructions=100, callbacks=10):
    operation = inner.register(signature(inputs, outputs), script, machine_grants=grants,
                               max_instructions=instructions, max_callbacks=callbacks)
    return result._foreign_tasks.define_operation(name, observed, operation), operation


def fixture():
    result = runtime()
    inner = adapter(result)
    return result, inner, QuantumAdapter(inner, result)


def resume(result, observed, yielded):
    assert type(yielded) is YieldedExecution
    observed.turn += 1
    return result.resume_yielded(yielded.suspension)


def finish(result, observed, current, *, limit=32):
    yields = []
    for _ in range(limit):
        if type(current) is ExecutionResult:
            return current, yields
        assert type(current) is YieldedExecution
        yields.append(current)
        current = resume(result, observed, current)
    pytest.fail("bounded reference script failed to finish")


def assert_turn_allowance(observed, quantum):
    for turn in {item.turn for item in observed.transitions}:
        work = sum(item.event.receipt.instructions for item in observed.transitions if item.turn == turn)
        assert work <= quantum
    assert all(item.quantum > 0 for item in observed.transitions if item.name == "advance")
    assert all(item.quantum == 0 for item in observed.transitions if item.name in ("begin", "reply"))


def run_stored_callback(quantum):
    result, inner, observed = fixture()
    export = capture(result, "DUP", 1, 2)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Store(address=EXTERNAL_BASE, data=b"A"),
        Callback(export=export, arguments=(Input(0),), cycles=2),
        Store(address=EXTERNAL_BASE + 1, data=b"B"),
        Return(outputs=(Reply(0), Reply(1)), cycles=2)), inputs=1, outputs=2,
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=2, access="write"),))
    context = result.main_context
    context.data.push(99)
    context.data.push(7)
    completed, yields = finish(result, observed, result.run_until_blocked(
        word.xt, machine_quantum_instructions=quantum))
    report = result._foreign_tasks.last_dispatch
    receipt = inner.last_receipt()
    state = (context.data.snapshot(), context.data.pointer, context.returns.pointer,
             result.memory.read_bytes(context.returns.empty_pointer - 128, 128),
             result.memory.read_bytes(EXTERNAL_BASE, 2), completed.semantic_steps,
             receipt.root_instructions, receipt.root_cycles, receipt.root_callbacks,
             report.entries, report.semantic_steps, inner.admitted_inputs, inner.replies)
    assert_finished(result, inner)
    return state, observed, yields


@pytest.mark.parametrize("quantum,expected_yields", [(1, 3), (2, 1), (100, 0)])
def test_sliced_stores_callback_and_return_match_uninterrupted_stack_and_work(quantum, expected_yields):
    expected, _synchronous, no_yields = run_stored_callback(None)
    actual, observed, yields = run_stored_callback(quantum)
    assert no_yields == [] and len(yields) == expected_yields
    assert actual == expected
    assert actual[0] == (99, 7, 7) and actual[4] == b"AB"
    assert actual[5:11] == (2, 4, 6, 1, 1, 1)
    assert_turn_allowance(observed, quantum)


def test_spent_turn_accepts_second_zero_work_entry_and_pops_its_input_only_once():
    result, inner, observed = fixture()
    register(result, inner, observed, "FIRST", (
        Store(address=EXTERNAL_BASE, data=b"A"), Return(outputs=(), cycles=2)),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    register(result, inner, observed, "SECOND", (Return(outputs=(Input(0),), cycles=2),),
             inputs=1, outputs=1)
    result.evaluate(b": OUTER FIRST 41 SECOND ;")
    context = result.main_context
    context.data.push(99)
    first = result.run_until_blocked("OUTER", machine_quantum_instructions=2)
    assert type(first) is YieldedExecution and first.semantic_steps == 5
    assert context.data.snapshot() == (99,)
    assert [(item.name, item.quantum) for item in observed.transitions] == [
        ("begin", 0), ("advance", 2), ("begin", 0)]
    admission = observed.transitions[-1]
    assert admission.data == (99, 41) and admission.inputs == (41,)
    assert admission.event.receipt.invocation_started
    assert admission.event.receipt.instructions == admission.event.receipt.cycles == 0
    assert (admission.event.receipt.root_entries, admission.event.receipt.root_instructions) == (2, 2)
    assert inner.admitted_inputs == ((1, ()), (2, (41,)))
    suspended = result._suspended_execution
    task, meter = suspended.task_root, suspended.meter
    assert len(task.frames) == 1 and task.frames[0].token is admission.event.operation_token
    assert observed.validations[-1][3] is None
    with pytest.raises(ExecutionError):
        result.deliver_idle_wake(first.suspension, IdleWake.INTERRUPT)
    completed = resume(result, observed, first)
    assert type(completed) is ExecutionResult and completed.semantic_steps == 6
    assert context.data.snapshot() == (99, 41) and result.memory.read8(EXTERNAL_BASE) == ord("A")
    assert inner.admitted_inputs == ((1, ()), (2, (41,)))
    assert task.ledger.meter is meter
    assert (inner.last_receipt().root_instructions, inner.last_receipt().root_cycles) == (3, 5)
    assert_turn_allowance(observed, 2)
    assert_finished(result, inner)


def test_rejected_second_admission_at_spent_quantum_does_not_consume_operands():
    result, inner, observed = fixture()
    register(result, inner, observed, "FIRST", (Return(outputs=()),))
    register(result, inner, observed, "SECOND", (Return(outputs=(Input(0),)),), inputs=1, outputs=1)
    result.evaluate(b": OUTER FIRST 41 SECOND ;")
    result._foreign_tasks.configure_limits(entry_limit=1)
    with pytest.raises(ForeignTaskBudgetExceeded):
        result.run_until_blocked("OUTER", machine_quantum_instructions=1)
    assert result.main_context.data.snapshot() == (41,)
    assert [item.name for item in observed.transitions] == ["begin", "advance"]
    assert inner.admitted_inputs == ((1, ()),) and inner.last_receipt().root_entries == 1
    assert result._suspended_execution is None
    assert_finished(result, inner)


def test_last_quantum_call_runs_leaf_and_stages_zero_quantum_reply_exactly_once():
    result, inner, observed = fixture()
    export = capture(result, "DUP", 1, 2)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Callback(export=export, arguments=(Input(0),), cycles=2),
        Return(outputs=(Reply(0), Reply(1)), cycles=2)), inputs=1, outputs=2)
    result.main_context.data.push(99)
    result.main_context.data.push(7)
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert type(first) is YieldedExecution and first.semantic_steps == 2
    assert [(item.name, item.quantum) for item in observed.transitions] == [
        ("begin", 0), ("advance", 1), ("reply", 0)]
    staged = observed.transitions[-1]
    assert staged.inputs == (7, 7) and staged.data == (99,)
    assert staged.event.receipt.instructions == staged.event.receipt.cycles == 0
    assert staged.event.receipt.sequence == 3 and inner.replies == ((1, 1, (7, 7)),)
    task = result._suspended_execution.task_root
    frame, = task.frames
    assert frame.request is frame.cookie is frame.capture is None
    assert frame.token is staged.event.operation_token and observed.validations[-1][3] is None
    receipt = inner.last_receipt()
    with pytest.raises((TypeError, ValueError)):
        inner.reply(observed.transitions[1].event.request_token, (7, 7),
                    budget=ForeignBudgetV1(
                        invocation_instructions_remaining=99, root_instructions_remaining=99,
                        invocation_callbacks_remaining=9, root_callbacks_remaining=9,
                        quantum_instructions=0))
    assert inner.last_receipt() is receipt
    completed = resume(result, observed, first)
    assert type(completed) is ExecutionResult and completed.semantic_steps == 2
    assert result.main_context.data.snapshot() == (99, 7, 7)
    assert inner.replies == ((1, 1, (7, 7)),)
    assert (inner.last_receipt().root_instructions, inner.last_receipt().root_cycles) == (2, 4)
    assert_finished(result, inner)


def test_return_on_final_quantum_completes_without_synthetic_yield_or_wake():
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (Return(outputs=(17,), cycles=2),), outputs=1)
    completed = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert type(completed) is ExecutionResult and completed.semantic_steps == 1
    assert result.main_context.data.snapshot() == (17,) and result._suspended_execution is None
    assert [item.name for item in observed.transitions] == ["begin", "advance"]
    assert inner.last_receipt().root_instructions == 1 and inner.last_receipt().root_cycles == 2
    assert observed.validations == []
    assert_finished(result, inner)


def test_zero_quantum_reply_preserves_terminal_instruction_failure_instead_of_yield():
    result, inner, observed = fixture()
    export = capture(result, "DROP", 1, 0)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Callback(export=export, arguments=(7,), cycles=2), Return(outputs=())), instructions=1)
    with pytest.raises(ForeignTaskError) as caught:
        result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert caught.value.event.kind.value == "instruction_limit"
    assert [item.name for item in observed.transitions] == ["begin", "advance", "reply"]
    assert observed.transitions[-1].quantum == 0 and observed.transitions[-1].event.receipt.instructions == 0
    assert inner.replies == ((1, 1, ()),) and result._suspended_execution is None
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert inner.last_receipt().root_instructions == 1 and inner.last_receipt().root_cycles == 2
    assert_finished(result, inner)


@pytest.mark.parametrize("limit", ["invocation", "root"])
def test_instruction_fuel_stays_terminal_after_repeated_scheduling_turns(limit):
    result, inner, observed = fixture()
    script = (Store(address=EXTERNAL_BASE, data=b"A"),
              Store(address=EXTERNAL_BASE + 1, data=b"B"), Return(outputs=()))
    word, _operation = register(result, inner, observed, "MACHINE", script,
        instructions=2 if limit == "invocation" else 100,
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=2, access="write"),))
    if limit == "root":
        result._foreign_tasks.configure_limits(instruction_limit=2)
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert type(first) is YieldedExecution and result.memory.read_bytes(EXTERNAL_BASE, 2) == b"A\0"
    with pytest.raises(ForeignTaskError) as caught:
        resume(result, observed, first)
    assert caught.value.event.kind.value == "instruction_limit"
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.last_receipt().root_instructions == inner.last_receipt().root_cycles == 2
    assert len(inner.admitted_inputs) == 1 and result._suspended_execution is None
    assert_finished(result, inner)


@pytest.mark.parametrize("limit", ["callbacks", "semantic", "public_meter"])
def test_callback_and_semantic_limits_are_not_refilled_by_machine_turns(limit):
    result, inner, observed = fixture()
    export = capture(result, "DROP", 1, 0)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Callback(export=export, arguments=(7,), cycles=2),
        Callback(export=export, arguments=(8,), cycles=2), Return(outputs=())))
    if limit == "callbacks":
        result._foreign_tasks.configure_limits(callback_limit=1)
    elif limit == "semantic":
        result._foreign_tasks.configure_limits(semantic_limit=1)
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1,
                                     step_budget=2 if limit == "public_meter" else 30)
    assert type(first) is YieldedExecution and inner.replies == ((1, 1, ()),)
    expected = StepBudgetExceeded if limit == "public_meter" else ForeignTaskError
    with pytest.raises(expected):
        resume(result, observed, first)
    assert inner.replies == ((1, 1, ()),) and inner.last_receipt().root_instructions == 2
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result.main_context.data.snapshot() == (() if limit == "callbacks" else (8,))
    assert_finished(result, inner)


@pytest.mark.parametrize("quantum", [1, 2, 3])
def test_many_empty_chain_reentries_share_turn_allowance_and_original_root(quantum):
    result, inner, observed = fixture()
    register(result, inner, observed, "MACHINE", (Return(outputs=(), cycles=2),))
    result.evaluate(b": OUTER MACHINE MACHINE MACHINE MACHINE MACHINE ;")
    result._foreign_tasks.configure_limits(instruction_limit=5, entry_limit=5)
    current = result.run_until_blocked("OUTER", machine_quantum_instructions=quantum)
    roots, meters = [], []
    for _ in range(8):
        if type(current) is ExecutionResult:
            break
        suspended = result._suspended_execution
        roots.append(suspended.task_root)
        meters.append(suspended.meter)
        current = resume(result, observed, current)
    else:
        pytest.fail("five single-instruction entries did not finish")
    assert current.semantic_steps == 11
    assert all(root is roots[0] for root in roots) and all(meter is meters[0] for meter in meters)
    assert (inner.last_receipt().root_entries, inner.last_receipt().root_instructions,
            inner.last_receipt().root_cycles) == (5, 5, 10)
    assert inner.admitted_inputs == tuple((index, ()) for index in range(1, 6))
    assert_turn_allowance(observed, quantum)
    assert_finished(result, inner)


def test_semantic_quanta_refresh_scheduling_but_not_empty_chain_execution_fuel():
    result, inner, observed = fixture()
    register(result, inner, observed, "MACHINE", (Return(outputs=()),))
    result.evaluate(b": OUTER 1 DROP MACHINE 2 DROP MACHINE ;")
    result._foreign_tasks.configure_limits(instruction_limit=1)
    original_root = original_meter = None
    with pytest.raises(ForeignTaskBudgetExceeded):
        current = result.run_until_blocked("OUTER", quantum_steps=1, machine_quantum_instructions=3)
        for _ in range(24):
            assert type(current) is YieldedExecution
            suspended = result._suspended_execution
            if suspended.task_root is not None:
                if original_root is None:
                    original_root, original_meter = suspended.task_root, suspended.meter
                assert suspended.task_root is original_root and suspended.meter is original_meter
            current = resume(result, observed, current)
        pytest.fail("semantic quanta renewed empty-chain execution fuel")
    assert original_root is not None
    assert inner.admitted_inputs == ((1, ()),) and inner.last_receipt().root_instructions == 1
    assert result._foreign_tasks.last_dispatch.root_id == original_root.ledger.root_id
    assert result._foreign_tasks.last_dispatch.semantic_steps == 0
    assert_turn_allowance(observed, 3)
    assert_finished(result, inner)


def test_child_and_parent_segments_spend_the_same_turn_allowance_without_extra_entry():
    result, inner, observed = fixture()
    grant = (ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),)
    _child, child_operation = register(result, inner, observed, "CHILD", (
        Store(address=EXTERNAL_BASE, data=b"B"), Return(outputs=(), cycles=2)), grants=grant)
    result.evaluate(b": PARENT-CB CHILD ;")
    export = capture(result, "PARENT-CB")
    parent, _operation = register(result, inner, observed, "PARENT", (
        Callback(export=export, arguments=(), children=(child_operation,), cycles=2),
        Return(outputs=(), cycles=2)), grants=grant)
    first = result.run_until_blocked(parent.xt, machine_quantum_instructions=1)
    assert type(first) is YieldedExecution and first.semantic_steps == 3
    task = result._suspended_execution.task_root
    parent_frame, child_frame = task.frames
    pending = parent_frame.request
    assert pending is not None and parent_frame.cookie is not None
    assert child_frame.request is None
    receipt = inner.last_receipt()
    assert receipt.depth == receipt.root_entries == 2
    assert receipt.invocation_started and receipt.instructions == receipt.cycles == 0
    assert receipt.root_instructions == 1 and result.memory.read8(EXTERNAL_BASE) == 0
    second = resume(result, observed, first)
    assert type(second) is YieldedExecution and result.memory.read8(EXTERNAL_BASE) == ord("B")
    assert task.frames[0] is parent_frame and parent_frame.request is pending
    third = resume(result, observed, second)
    assert type(third) is YieldedExecution and len(task.frames) == 1
    assert task.frames[0] is parent_frame and parent_frame.request is None
    assert inner.replies == ((1, 1, ()),)
    completed = resume(result, observed, third)
    assert type(completed) is ExecutionResult and completed.semantic_steps == 4
    assert (inner.last_receipt().root_entries, inner.last_receipt().root_instructions,
            inner.last_receipt().root_cycles, inner.last_receipt().root_callbacks) == (2, 4, 7, 1)
    assert inner.admitted_inputs == ((1, ()), (2, ()))
    assert_turn_allowance(observed, 1)
    assert_finished(result, inner)


def test_positive_advance_with_zero_work_is_an_error_not_an_infinite_series_of_host_yields():
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (Return(outputs=(17,)),), outputs=1)
    observed.zero_advance = True
    with pytest.raises(ForeignTaskError, match="progress"):
        result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert [item.name for item in observed.transitions] == ["begin", "advance"]
    assert inner.last_receipt().root_instructions == inner.last_receipt().root_cycles == 0
    assert result.main_context.data.snapshot() == () and result._suspended_execution is None
    assert_finished(result, inner)


@pytest.mark.parametrize("value,error", [(True, TypeError), (1.0, TypeError), ("1", TypeError),
                                         (0, ValueError), (-1, ValueError), (10_000_001, ValueError)])
def test_invalid_machine_quantum_rejects_before_semantic_or_adapter_effect(value, error):
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (Return(outputs=(Input(0),)),),
                                inputs=1, outputs=1)
    result.main_context.data.push(7)
    before = result.main_context.data.pointer
    with pytest.raises(error):
        result.run_until_blocked(word.xt, machine_quantum_instructions=value)
    assert observed.transitions == [] and inner.last_receipt() is None
    assert result.main_context.data.snapshot() == (7,) and result.main_context.data.pointer == before
    assert result._suspended_execution is None and result.main_context.reusable


def test_missing_parked_validation_keeps_synchronous_use_but_rejects_live_machine_detach():
    result, inner, _trace = fixture()
    observed = ObservedAdapter(inner, result.main_context)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Store(address=EXTERNAL_BASE, data=b"A"), Return(outputs=())),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    assert type(result.run_until_blocked(word.xt)) is ExecutionResult
    with pytest.raises(ForeignTaskError):
        result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert result.memory.read8(EXTERNAL_BASE) == ord("A")
    assert inner.last_receipt().root_instructions == 1 and inner.active_invocations == ()
    assert result._suspended_execution is None


def test_copied_machine_cursor_is_not_authority_and_failure_cancels_before_more_work():
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (
        Store(address=EXTERNAL_BASE, data=b"A"), Return(outputs=(17,))), outputs=1,
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    suspended = result._suspended_execution
    original = suspended.cursor
    suspended.cursor = replace(original)
    assert suspended.cursor is not original
    receipt = inner.last_receipt()
    with pytest.raises(ExecutionError):
        resume(result, observed, first)
    assert inner.last_receipt() is receipt and result.main_context.data.snapshot() == ()
    assert result.memory.read8(EXTERNAL_BASE) == ord("A") and inner.active_invocations == ()
    assert result._suspended_execution is None
    with pytest.raises(ExecutionError):
        result.resume_yielded(first.suspension)


def test_changed_persisted_machine_quantum_cannot_replace_original_resume_selection():
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (
        Store(address=EXTERNAL_BASE, data=b"A"), Return(outputs=(17,))), outputs=1,
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    receipt = inner.last_receipt()
    result._suspended_execution.machine_quantum_instructions = 2
    with pytest.raises(ExecutionError):
        resume(result, observed, first)
    assert inner.last_receipt() is receipt and result.main_context.data.snapshot() == ()
    assert inner.active_invocations == () and result._suspended_execution is None


@pytest.mark.parametrize("replacement_limit", [1, 100])
def test_callback_accounting_cannot_replace_original_outer_machine_turn(monkeypatch, replacement_limit):
    result, inner, observed = fixture()
    export = capture(result, "DROP", 1, 0)
    word, _operation = register(result, inner, observed, "MACHINE", (
        Callback(export=export, arguments=(7,), cycles=2), Return(outputs=())))
    account, changed = result._account_semantic_step, []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is not None and task.active:
            frame = result._active_dispatches[0]
            original = frame.machine_turn
            changed.append(original)
            object.__setattr__(frame, "machine_turn", MachineTurn(replacement_limit))

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    with pytest.raises(ExecutionError):
        result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    assert len(changed) == 1 and changed[0].limit == 1
    assert inner.last_receipt().root_instructions == 1 and inner.replies == ()
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert result._suspended_execution is None and inner.active_invocations == ()


def test_cancel_accepted_zero_entry_keeps_popped_input_and_prior_prefix_without_running_it():
    result, inner, observed = fixture()
    register(result, inner, observed, "FIRST", (Return(outputs=()),))
    register(result, inner, observed, "SECOND", (
        Store(address=EXTERNAL_BASE, data=b"B"), Return(outputs=(Input(0),))),
        inputs=1, outputs=1, grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.evaluate(b": OUTER FIRST 41 SECOND ;")
    first = result.run_until_blocked("OUTER", machine_quantum_instructions=1)
    receipt = inner.last_receipt()
    assert receipt.invocation_started and receipt.instructions == 0
    assert result.main_context.data.snapshot() == () and result.memory.read8(EXTERNAL_BASE) == 0
    result.cancel_suspension(first.suspension)
    assert inner.last_receipt() is receipt and result.memory.read8(EXTERNAL_BASE) == 0
    assert result.main_context.data.snapshot() == () and inner.active_invocations == ()
    assert inner.admitted_inputs == ((1, ()), (2, (41,)))
    with pytest.raises(ExecutionError):
        result.resume_yielded(first.suspension)
    assert_finished(result, inner)


def test_second_zero_work_admission_delivery_failure_settles_receipt_before_any_input_pop():
    result, inner, observed = fixture()
    register(result, inner, observed, "FIRST", (Return(outputs=()),))
    register(result, inner, observed, "SECOND", (Return(outputs=(Input(0),)),), inputs=1, outputs=1)
    result.evaluate(b": OUTER FIRST 41 SECOND ;")
    error = KeyboardInterrupt("zero-work second admission delivery")
    inner.fail_delivery_after(3, error)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.run_until_blocked("OUTER", machine_quantum_instructions=1)
    assert caught.value is error and result.main_context.data.snapshot() == (41,)
    receipt = inner.last_receipt()
    assert receipt.invocation_started and receipt.instructions == receipt.cycles == 0
    assert (receipt.root_entries, receipt.root_instructions) == (2, 1)
    report = result._foreign_tasks.last_dispatch
    assert report.entries == 2 and report.machine_instructions == 1
    assert inner.active_invocations == () and result._suspended_execution is None
    assert_finished(result, inner)


def test_failed_later_machine_suspension_publication_keeps_exact_prefix_and_error(monkeypatch):
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (
        Store(address=EXTERNAL_BASE, data=b"A"), Store(address=EXTERNAL_BASE + 1, data=b"B"),
        Return(outputs=())), grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=2, access="write"),))
    first = result.run_until_blocked(word.xt, machine_quantum_instructions=1)
    original = KeyboardInterrupt("second machine cursor handle")

    def fail_handle():
        raise original

    monkeypatch.setattr(result, "_allocate_suspension_handle", fail_handle)
    with pytest.raises(KeyboardInterrupt) as caught:
        resume(result, observed, first)
    assert caught.value is original
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.last_receipt().root_instructions == 2 and len(inner.admitted_inputs) == 1
    assert result._suspended_execution is None and inner.active_invocations == ()
    assert_finished(result, inner)


@pytest.mark.parametrize("damage,diagnostic", [("helper", "route changed"),
                                              ("api_pair", "API identity changed")])
def test_first_tick_cannot_shadow_machine_turn_admission_before_any_task_root(monkeypatch, damage, diagnostic):
    result, inner, observed = fixture()
    word, _operation = register(result, inner, observed, "MACHINE", (Return(outputs=(Input(0),)),),
                                inputs=1, outputs=1)
    result.main_context.data.push(7)
    engine = result._foreign_tasks
    account, meters, replacements = result._account_semantic_step, [], []

    def replacement(*args, **kwargs):
        replacements.append("dispatcher")
        return None

    class FakeSeal:
        def verify(self):
            replacements.append("verifier")

    def tick():
        account()
        assert engine._task_root is None
        meters.append(result._active_dispatches[0].meter)
        if damage == "helper":
            monkeypatch.setattr(engine, "require_machine_turn", replacement)
        else:
            monkeypatch.setattr(engine, "_machine_api", (replacement, FakeSeal()))

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    with pytest.raises(ForeignTaskError, match=diagnostic):
        result.run_until_blocked(word.xt, step_budget=10, machine_quantum_instructions=1)
    assert replacements == [] and len(meters) == 1 and meters[0].steps == 1
    assert observed.transitions == [] and inner.last_receipt() is None
    assert result.main_context.data.snapshot() == (7,)
    assert result.main_context.returns.snapshot() == ()
    assert result._active_dispatches == [] and result._suspended_execution is None
    assert engine._task_root is None and engine._machine_selection is engine._machine_host_turn is None


def test_later_ordinary_publication_failure_clears_machine_selection_before_first_task(monkeypatch):
    result, _inner, observed = fixture()
    result.evaluate(b": OUTER 1 DROP 2 DROP ;")
    first = result.run_until_blocked("OUTER", quantum_steps=1, machine_quantum_instructions=3)
    assert type(first) is YieldedExecution and first.semantic_steps == 1
    assert result.main_context.data.snapshot() == (1,)
    engine = result._foreign_tasks
    assert engine._task_root is None and engine._machine_selection is not None
    meter = result._suspended_execution.meter
    original = KeyboardInterrupt("ordinary second handle before first foreign entry")
    allocator = result._allocate_suspension_handle

    def fail_handle():
        raise original

    monkeypatch.setattr(result, "_allocate_suspension_handle", fail_handle)
    with pytest.raises(KeyboardInterrupt) as caught:
        resume(result, observed, first)
    assert caught.value is original and meter.steps == 3
    assert result.main_context.data.snapshot() == () and observed.transitions == []
    assert engine._machine_selection is engine._machine_host_turn is None
    assert result._active_dispatches == [] and result._suspended_execution is None
    assert result.main_context.reusable
    with pytest.raises(ExecutionError):
        result.resume_yielded(first.suspension)

    monkeypatch.setattr(result, "_allocate_suspension_handle", allocator)
    completed, _yields = finish(result, observed, result.run_until_blocked(
        "OUTER", quantum_steps=1, machine_quantum_instructions=3))
    assert completed.semantic_steps == 7 and result.main_context.data.snapshot() == ()
    assert engine._machine_selection is engine._machine_host_turn is None
    assert result.main_context.reusable and observed.transitions == []
