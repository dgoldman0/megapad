"""Original task-tail frontiers survive real KDOS multi-stage unwinding."""

from dataclasses import replace
from pathlib import Path

import pytest

from shared.cells import u64
from shared.foreign_abi import ForeignSpanV1
from simulator.errors import ExecutionError
from simulator.foreign_control import ForeignContinuation
from simulator.foreign_runtime import ForeignTaskBudgetExceeded, ForeignTaskError
from simulator.memory import EXTERNAL_BASE
from simulator.stacks import Continuation
from tests.simulator.foreign_reference import Callback, Reply, Return, Store
from tests.simulator.test_foreign_dispatch import (
    adapter, assert_finished, capture, runtime, signature,
)


class RecordingAdapter:
    """Keep exact native-shaped cancellation observations without guest hooks."""

    def __init__(self, inner, result):
        self.inner, self.result = inner, result
        self.events = []
        self.cancellations = []
        self.cancel_attempts = 0
        self.fail_at = 0
        self.error = None
        self.after_retirement = False
        self.accounting = []

    def begin(self, *args, **kwargs):
        event = self.inner.begin(*args, **kwargs)
        self.events.append(event)
        return event

    def advance(self, *args, **kwargs):
        event = self.inner.advance(*args, **kwargs)
        self.events.append(event)
        return event

    def reply(self, *args, **kwargs):
        event = self.inner.reply(*args, **kwargs)
        self.events.append(event)
        return event

    def cancel_suffix(self, token):
        self.cancel_attempts += 1
        ledger = self.result._foreign_tasks._task_root.ledger
        self.accounting.append((ledger.semantic_steps,
                                tuple((scope.invocation_id, scope.steps)
                                      for scope in ledger._state.semantic_scopes)))
        if self.cancel_attempts == self.fail_at and not self.after_retirement:
            raise self.error
        outcome = self.inner.cancel_suffix(token)
        self.cancellations.append(outcome)
        if self.cancel_attempts == self.fail_at:
            raise self.error
        return outcome

    def cancel_all(self):
        return self.inner.cancel_all()

    def last_receipt(self):
        return self.inner.last_receipt()


def nested(*, callback=b"HANDLER @ RP! R> HANDLER ! -42 THROW",
           parent=b"['] CHILD CATCH", child_steps=4096, extra=()):
    result = runtime(exceptions=True)
    result.evaluate(b": LEAF " + callback + b" ;")
    inner = adapter(result)
    observed = RecordingAdapter(inner, result)
    leaf = capture(result, "LEAF", steps=child_steps, extra=extra)
    child_operation = inner.register(signature(), (
        Store(address=EXTERNAL_BASE + 1, data=b"B"),
        Callback(export=leaf, arguments=()), Return(outputs=())),
        machine_grants=(ForeignSpanV1(base=EXTERNAL_BASE + 1, size=1, access="write"),))
    child = result._foreign_tasks.define_operation("CHILD", observed, child_operation)
    result.evaluate(b": PARENT-CB " + parent + b" ;")
    parent_export = capture(result, "PARENT-CB", outputs=1, dynamic=(child,))
    parent_operation = inner.register(signature(0, 1), (
        Store(address=EXTERNAL_BASE, data=b"A"),
        Callback(export=parent_export, arguments=(), children=(child_operation,)),
        Return(outputs=(Reply(0),))),
        machine_grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=2, access="write"),))
    result._foreign_tasks.define_operation("PARENT", observed, parent_operation)
    result.evaluate(b": OUTER 99 ['] PARENT CATCH 7 ;")
    return result, inner, observed


def assert_two_discards(result, inner, observed, code=-42):
    assert result.main_context.data.snapshot() == (99, u64(code), 7)
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.replies == ()
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(2,), (1,)]
    parent_request = next(event for event in observed.events
                          if hasattr(event, "request_token") and event.receipt.depth == 1)
    first, second = observed.cancellations
    assert first.surviving_parent_id == 1
    assert first.surviving_parent_token is parent_request.request_token
    assert second.surviving_parent_id is None and second.surviving_parent_token is None
    assert first.receipt is second.receipt is inner.last_receipt()
    report = result._foreign_tasks.last_dispatch
    assert report.entries == report.callbacks == 2
    assert report.machine_instructions == report.machine_cycles == 4
    assert report.completed and report.cancelled
    assert_finished(result, inner)


def test_real_two_frontier_unwind_retires_child_then_parent_and_finishes_guest_catch():
    result, inner, observed = nested()
    result.execute("OUTER")
    assert_two_discards(result, inner, observed)
    # After the first RP!, only the surviving parent's callback guard remains.
    assert [item[0] for item in observed.accounting[0][1]] == [1, 2]
    assert [item[0] for item in observed.accounting[1][1]] == [1]


def test_real_nested_catch_rethrows_after_releasing_first_tail():
    result, inner, observed = nested(callback=b"-42 THROW", parent=b"['] CHILD CATCH THROW")
    result.execute("OUTER")
    assert_two_discards(result, inner, observed)


def test_second_ordinary_unwind_advances_tail_when_no_foreign_frame_remains():
    result = runtime(exceptions=True)
    result.evaluate(b": SKIP-INNER HANDLER @ RP! R> HANDLER ! -42 THROW ;")
    inner = adapter(result)
    observed = RecordingAdapter(inner, result)
    export = capture(result, "SKIP-INNER")
    operation = inner.register(signature(), (
        Callback(export=export, arguments=()), Return(outputs=())))
    result._foreign_tasks.define_operation("MACHINE", observed, operation)
    result.evaluate(b": INNER ['] MACHINE CATCH ; : OUTER 99 ['] INNER CATCH 7 ;")
    result.execute("OUTER")
    assert result.main_context.data.snapshot() == (99, u64(-42), 7)
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(1,)]
    assert inner.replies == () and inner.last_receipt().root_instructions == 1
    assert_finished(result, inner)


def test_real_throw_through_defer_does_and_loop_reaches_parent_then_outer_catch():
    result = runtime(exceptions=True)
    prefix = Path(__file__).with_name("fixtures") / "kdos-prefix-39-69.f"
    result.evaluate(prefix.read_bytes(), source_name=str(prefix))
    result.evaluate(b": BOOM 5 0 DO I 2 = IF -23 THROW THEN LOOP 999 ; "
                    b"DEFER ACTION ' BOOM IS ACTION")
    inner = adapter(result)
    observed = RecordingAdapter(inner, result)
    leaf = capture(result, "ACTION", dynamic=(result.find("BOOM"),),
                   extra=(ForeignSpanV1(base=result.find("ACTION").body_address,
                                       size=8, access="read"),))
    child_operation = inner.register(signature(), (
        Store(address=EXTERNAL_BASE + 1, data=b"B"),
        Callback(export=leaf, arguments=()), Return(outputs=())),
        machine_grants=(ForeignSpanV1(base=EXTERNAL_BASE + 1, size=1, access="write"),))
    child = result._foreign_tasks.define_operation("CHILD", observed, child_operation)
    result.evaluate(b": PARENT-CB ['] CHILD CATCH THROW ;")
    parent_export = capture(result, "PARENT-CB", outputs=1, dynamic=(child,))
    parent_operation = inner.register(signature(0, 1), (
        Store(address=EXTERNAL_BASE, data=b"A"),
        Callback(export=parent_export, arguments=(), children=(child_operation,)),
        Return(outputs=(Reply(0),))),
        machine_grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=2, access="write"),))
    result._foreign_tasks.define_operation("PARENT", observed, parent_operation)
    result.evaluate(b": OUTER 99 ['] PARENT CATCH 7 ;")
    result.execute("OUTER")
    assert_two_discards(result, inner, observed, -23)


@pytest.mark.parametrize("damage", ["raw", "metadata", "copy"])
def test_repair_cannot_move_a_retained_tail_back_to_an_invalidated_frontier(monkeypatch, damage):
    result, inner, observed = nested()
    account = result._account_semantic_step
    saved, positions = [], []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is None or task.tail is None:
            return
        tail = task.tail
        positions.append((tail.frontiers, tail.index))
        if not saved:
            original = tail.frontiers[tail.index]
            entry, address, raw, _values = original
            assert type(entry) is not ForeignContinuation
            saved.append((original, tail.index))
            if damage == "raw":
                result.memory.write64(address, raw ^ 1)
            elif damage == "metadata":
                del result.main_context.returns._continuations[address]
            else:
                result.main_context.returns._continuations[address] = (replace(entry), raw)
        elif len(saved) == 1:
            (entry, address, raw, _values), initial = saved[0]
            assert tail.index > initial
            result.memory.write64(address, raw)
            result.main_context.returns._continuations[address] = (entry, raw)
            saved.append(tail.index)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    result.execute("OUTER")
    assert len(saved) == 2 and len(positions) > 2
    assert all(vector is positions[0][0] for vector, _index in positions)
    assert [index for _vector, index in positions] == sorted(index for _vector, index in positions)
    assert_two_discards(result, inner, observed)


@pytest.mark.parametrize("stage", ["callback", "tail"])
@pytest.mark.parametrize("damage", ["raw", "metadata"])
def test_later_original_frontier_cannot_be_repaired_before_it_is_selected(monkeypatch, stage, damage):
    result, inner, observed = nested()
    account = result._account_semantic_step
    saved = []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is None:
            return
        if not saved:
            if stage == "callback":
                if task.tail is not None or len(task.frames) != 2 or task.frames[-1].capture is None:
                    return
                owner = task.frames[-1]
                vector, selected = owner.frontiers, 0
            else:
                if task.tail is None:
                    return
                owner = None
                vector, selected = task.tail.frontiers, task.tail.index
            index, frontier = next((index, row) for index, row in enumerate(vector)
                                   if index > selected and type(row[0]) is Continuation
                                   and not row[0].root)
            entry, address, raw, _values = frontier
            saved.append((owner, vector, selected, index, frontier))
            if damage == "raw":
                result.memory.write64(address, raw ^ 1)
            else:
                del result.main_context.returns._continuations[address]
        elif len(saved) == 1:
            owner, vector, selected, index, (entry, address, raw, _values) = saved[0]
            losses = owner.frontier_losses if owner is not None else task.tail.losses
            assert losses & (1 << index)
            if owner is None:
                assert task.tail.frontiers is vector and task.tail.index == selected
            result.memory.write64(address, raw)
            result.main_context.returns._continuations[address] = (entry, raw)
            task.reconcile()
            losses = owner.frontier_losses if owner is not None else task.tail.losses
            assert losses & (1 << index)
            saved.append(True)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    # The repaired OUTER continuation is physically popped by real THROW,
    # but cannot release the tail's captured authority. OUTER stays unadmitted.
    with pytest.raises(ForeignTaskError, match="target was not captured"):
        result.execute("OUTER")
    assert len(saved) == 2
    assert result.main_context.data.snapshot() == (99, u64(-42))
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(2,), (1,)]
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.active_invocations == () and inner.replies == ()
    assert not result.main_context.reusable


def test_identical_frontier_cookie_write_preserves_original_identity(monkeypatch):
    result, inner, observed = nested()
    account = result._account_semantic_step
    evidence = []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is not None and task.tail is not None and not evidence:
            tail = task.tail
            entry, address, raw, _values = tail.frontiers[tail.index]
            result.memory.write64(address, raw)
            task.reconcile()
            evidence.append(task.tail.index == tail.index and
                            task.tail.frontiers[task.tail.index][0] is entry)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    result.execute("OUTER")
    assert evidence == [True]
    assert_two_discards(result, inner, observed)


def test_repaired_parent_tombstone_cannot_reenter_original_frontier_vector(monkeypatch):
    result, inner, observed = nested()
    account = result._account_semantic_step
    saved = []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is None or task.tail is None:
            return
        if not saved:
            frontier = next(row for row in task.tail.frontiers
                            if type(row[0]) is ForeignContinuation)
            entry, address, raw, _values = frontier
            saved.append((frontier, task.tail.frontiers.index(frontier)))
            result.memory.write64(address, raw ^ 1)
        elif len(saved) == 1:
            (entry, address, raw, _values), original_index = saved[0]
            assert entry.retired and task.tail.index > original_index
            assert task.frames == [] and task.control.live_count == 0
            result.memory.write64(address, raw)
            result.main_context.returns._continuations[address] = (entry, raw)
            object.__setattr__(entry, "_retirement", None)
            task.reconcile()
            assert task.tail.index > original_index and task.control.live_count == 0
            saved.append(task.tail.index)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    result.execute("OUTER")
    assert len(saved) == 2
    assert_two_discards(result, inner, observed)


def test_new_helper_cannot_replace_all_lost_original_tail_frontiers(monkeypatch):
    result, inner, observed = nested()
    account = result._account_semantic_step
    changed = []

    def tick():
        account()
        task = result._foreign_tasks._task_root
        if task is None or task.tail is None or changed:
            return
        tail = task.tail
        returns = result.main_context.returns
        for _entry, address, _raw, _values in tail.frontiers:
            returns._continuations.pop(address, None)
        helper = returns.push_continuation(result.find("HANDLER").xt, 0)
        changed.append(helper)

    monkeypatch.setattr(result, "_account_semantic_step", tick)
    with pytest.raises(ForeignTaskError, match="lost its pre-unwind continuation"):
        result.execute("OUTER")
    assert len(changed) == 1
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(2,), (1,)]
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.active_invocations == () and inner.replies == ()
    assert not result.main_context.reusable


def test_retained_tail_uses_root_and_parent_fuel_after_child_allowance_is_retired():
    result, inner, observed = nested(
        callback=b"HANDLER @ RP! R> HANDLER ! 80 0 DO I DROP LOOP -42 THROW", child_steps=64)
    result.execute("OUTER")
    assert_two_discards(result, inner, observed)
    assert result._foreign_tasks.last_dispatch.semantic_steps > 64
    assert observed.accounting[0][1][-1][1] < 64
    assert observed.accounting[1][1][0][1] > 64


def test_retained_tail_does_not_renew_original_root_semantic_allowance():
    result, inner, observed = nested(
        callback=b"HANDLER @ RP! R> HANDLER ! 80 0 DO I DROP LOOP -42 THROW")
    result._foreign_tasks.configure_limits(semantic_limit=80)
    with pytest.raises(ForeignTaskBudgetExceeded, match="root semantic allowance"):
        result.execute("OUTER")
    assert result._foreign_tasks.last_dispatch.semantic_steps == 80
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(2,)]
    assert inner.active_invocations == () and inner.replies == ()
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert not result.main_context.reusable


def test_retained_tail_keeps_its_original_grants_and_scalar_fault_prefix():
    denied = EXTERNAL_BASE + 32
    result, inner, observed = nested(
        callback=f"HANDLER @ RP! R> HANDLER ! 65 {denied} C! -42 THROW".encode())
    with pytest.raises(ForeignTaskError, match="outside its original captured grants"):
        result.execute("OUTER")
    assert result.memory.read8(denied) == 0
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert [item.retired_invocation_ids for item in observed.cancellations] == [(2,)]
    assert inner.active_invocations == () and inner.replies == ()
    # C! consumed both operands before its denied memory effect.
    assert 65 not in result.main_context.data.snapshot()
    assert denied not in result.main_context.data.snapshot()
    assert not result.main_context.reusable


def test_frontier_scan_bound_precedes_callback_argument_and_cookie_effects():
    result = runtime()
    inner = adapter(result)
    observed = RecordingAdapter(inner, result)
    export = capture(result, "DROP", inputs=1)
    operation = inner.register(signature(), (
        Callback(export=export, arguments=(7,)), Return(outputs=())))
    result._foreign_tasks.define_operation("MACHINE", observed, operation)
    result.evaluate(b": INNER MACHINE ; : OUTER INNER ;")
    result._foreign_tasks.configure_limits(semantic_limit=1)
    result.main_context.data.push(99)
    with pytest.raises(ForeignTaskError, match="finite scan allowance"):
        result.execute("OUTER")
    assert result.main_context.data.snapshot() == (99,)
    assert not any(type(record[0]) is ForeignContinuation
                   for record in result.main_context.returns._continuations.values())
    report = result._foreign_tasks.last_dispatch
    assert report.semantic_steps == 0
    assert report.entries == report.callbacks == report.machine_instructions == 1
    assert inner.active_invocations == () and inner.replies == ()
    assert result.main_context.reusable


def test_inactive_continuation_history_does_not_enlarge_original_frontier_scan():
    result = runtime()
    returns = result.main_context.returns
    inactive = []
    for index in range(64):
        address = returns.floor + index * 8
        entry = Continuation(result.find("DUP").xt, 0)
        result.memory.write64(address, index + 1)
        returns._continuations[address] = (entry, index + 1)
        inactive.append((address, entry))
    inner = adapter(result)
    observed = RecordingAdapter(inner, result)
    export = capture(result, "DROP", inputs=1)
    operation = inner.register(signature(), (
        Callback(export=export, arguments=(7,)), Return(outputs=())))
    result._foreign_tasks.define_operation("MACHINE", observed, operation)
    result.evaluate(b": OUTER MACHINE ;")
    result._foreign_tasks.configure_limits(semantic_limit=1)
    result.execute("OUTER")
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert inner.replies == ((1, 1, ()),)
    assert all(returns._continuations[address][0] is entry for address, entry in inactive)
    assert_finished(result, inner)


@pytest.mark.parametrize("after_retirement", [False, True])
def test_second_suffix_cancellation_failure_preserves_error_and_completed_prefix(after_retirement):
    result, inner, observed = nested()
    error = KeyboardInterrupt("second suffix cancellation failed")
    observed.fail_at, observed.error = 2, error
    observed.after_retirement = after_retirement
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute("OUTER")
    assert caught.value is error
    assert observed.cancel_attempts == 2
    assert result.memory.read_bytes(EXTERNAL_BASE, 2) == b"AB"
    assert inner.replies == () and inner.active_invocations == ()
    report = result._foreign_tasks.last_dispatch
    assert report.machine_instructions == report.machine_cycles == 4
    assert report.cancelled and not report.completed
    assert not result.main_context.reusable
    with pytest.raises(ExecutionError):
        result.execute("PARENT")
