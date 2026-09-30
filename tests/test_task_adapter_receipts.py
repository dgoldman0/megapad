"""Optional task accounting and parked proof use the real shared native owner."""

from dataclasses import replace
import sys

import pytest

from simulator.foreign_control import ForeignContinuation
from simulator.foreign_runtime import ForeignTaskError
from tests.test_hybrid_callbacks import _image as private_callback_image
from tests.test_native_task_adapter import (
    assert_idle, machine_snapshot, owner, register_callback,
)


def semantic_receipt(hybrid, adapter):
    report = hybrid.semantic._foreign_tasks.last_dispatch
    return hybrid.semantic._foreign_tasks.task_semantic_receipt(
        adapter, adapter._root_token, report.root_id)


def totals(hybrid):
    return (hybrid.machine_instructions, hybrid.machine_cycles, hybrid.transitions,
            hybrid.callback_requests, hybrid.callback_semantic_steps, hybrid.machine_segments)


def test_exact_semantic_receipt_replay_is_idempotent_and_copies_or_old_roots_are_rejected(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word, _ = register_callback(hybrid, adapter)
    runtime.main_context.data.push(7)
    report = hybrid.execute(word.xt)
    receipt = semantic_receipt(hybrid, adapter)
    assert receipt.sequence == receipt.semantic_steps == report.callback_semantic_steps == 1
    assert hybrid.callback_semantic_steps == 1
    task_status = hybrid.task_execution_status
    assert task_status["callback_semantic_steps"] == 1
    assert task_status["instructions"] == adapter.machine_instructions == 5
    assert task_status["cycles"] == adapter.machine_cycles
    assert task_status["callback_requests"] == 1 and task_status["active_depth"] == 0
    before = totals(hybrid), machine_snapshot(hybrid), adapter.last_receipt()
    for _ in range(3):
        adapter.settle_semantic_receipt(receipt)
        assert (totals(hybrid), machine_snapshot(hybrid), adapter.last_receipt()) == before
        assert hybrid.task_execution_status == task_status
    copied = replace(receipt)
    with pytest.raises(ForeignTaskError):
        adapter.settle_semantic_receipt(copied)
    assert (totals(hybrid), machine_snapshot(hybrid), adapter.last_receipt()) == before
    assert semantic_receipt(hybrid, adapter) is receipt

    runtime.main_context.data.clear()
    runtime.main_context.data.push(9)
    second = hybrid.execute(word.xt)
    current = semantic_receipt(hybrid, adapter)
    assert current is not receipt and current.root_token is not receipt.root_token
    assert current.root_id > receipt.root_id and current.sequence == 1
    assert current.semantic_steps == second.callback_semantic_steps == 1
    assert hybrid.callback_semantic_steps == 2
    task_status = hybrid.task_execution_status
    assert task_status["callback_semantic_steps"] == 2
    assert task_status["instructions"] == adapter.machine_instructions == 10
    assert task_status["cycles"] == adapter.machine_cycles
    assert task_status["callback_requests"] == 2
    before = totals(hybrid), machine_snapshot(hybrid)
    with pytest.raises(ForeignTaskError):
        adapter.settle_semantic_receipt(receipt)
    assert (totals(hybrid), machine_snapshot(hybrid)) == before
    adapter.settle_semantic_receipt(current)
    assert (totals(hybrid), machine_snapshot(hybrid)) == before
    assert hybrid.task_execution_status == task_status
    assert_idle(hybrid, adapter)


def test_settled_task_receipt_replay_cannot_refund_later_private_callback_work(owner):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    task, _ = register_callback(hybrid, adapter)
    private = hybrid.register_routine_v2(private_callback_image("PRIVATE", callback="ABS"))
    runtime.main_context.data.push(7)
    hybrid.execute(task.xt)
    receipt = semantic_receipt(hybrid, adapter)
    assert hybrid.callback_semantic_steps == receipt.semantic_steps == 1
    task_status = hybrid.task_execution_status
    assert task_status["callback_semantic_steps"] == 1
    assert task_status["instructions"] == adapter.machine_instructions == 5
    runtime.main_context.data.clear()
    runtime.main_context.data.push(11)
    report = hybrid.execute(private.xt)
    assert report.callback_semantic_steps == 1 and hybrid.callback_semantic_steps == 2
    assert hybrid.machine_instructions > adapter.machine_instructions
    assert hybrid.task_execution_status == task_status
    assert semantic_receipt(hybrid, adapter) is receipt
    before = totals(hybrid), machine_snapshot(hybrid), adapter.last_receipt()
    adapter.settle_semantic_receipt(receipt)
    assert (totals(hybrid), machine_snapshot(hybrid), adapter.last_receipt()) == before
    assert hybrid.task_execution_status == task_status
    assert runtime.main_context.data.snapshot() == (11,)
    assert_idle(hybrid, adapter)


@pytest.mark.parametrize("stage", ("before_projection", "after_projection"))
def test_interrupted_semantic_projection_retains_exact_work_and_original_error(owner, stage):
    hybrid, adapter = owner
    runtime = hybrid.semantic
    word, _ = register_callback(hybrid, adapter)
    runtime.main_context.data.push(99)
    runtime.main_context.data.push(7)
    accounting = adapter._composition_authority.accounting
    failure = KeyboardInterrupt("task semantic receipt projection interrupted")
    fired = []
    previous_trace = sys.gettrace()

    def interrupt_projection(frame, event, argument):
        if (not fired and event == "line" and frame.f_code is accounting.__code__
                and frame.f_locals.get("action") == "semantic"):
            receipt = frame.f_locals.get("receipt")
            snapshot = frame.f_locals.get("semantic_state")
            # The immutable receipt has been published, but its projection has
            # not yet been acknowledged. Interrupt either side of the raw write.
            if (receipt is not None and type(snapshot) is tuple and len(snapshot) == 8
                    and snapshot[3] is receipt and snapshot[6] is True
                    and hybrid.callback_semantic_steps == (0 if stage == "before_projection" else 1)):
                fired.append(True)
                raise failure
        return interrupt_projection

    try:
        sys.settrace(interrupt_projection)
        with pytest.raises(KeyboardInterrupt) as caught:
            hybrid.execute(word.xt)
    finally:
        sys.settrace(previous_trace)
    assert fired == [True] and caught.value is failure
    receipt = semantic_receipt(hybrid, adapter)
    report = runtime._foreign_tasks.last_dispatch
    assert receipt.semantic_steps == report.semantic_steps == hybrid.callback_semantic_steps == 1
    assert adapter.machine_instructions == report.machine_instructions == hybrid.machine_instructions == 5
    task_status = hybrid.task_execution_status
    assert task_status["callback_semantic_steps"] == 1
    assert task_status["instructions"] == 5 and task_status["cycles"] == adapter.machine_cycles
    assert adapter.last_receipt().state == "returned"
    assert runtime.main_context.data.snapshot() == (99, 7, 7)
    assert report.cancelled and not report.completed
    before = totals(hybrid), machine_snapshot(hybrid)
    adapter.settle_semantic_receipt(receipt)
    assert (totals(hybrid), machine_snapshot(hybrid)) == before
    assert hybrid.task_execution_status == task_status
    assert_idle(hybrid, adapter)


def test_real_parked_validation_preserves_native_state_tokens_receipts_and_semantic_work(owner, monkeypatch):
    hybrid, adapter = owner
    if not hasattr(adapter._runner, "validate_parked"):
        pytest.skip("native parked-state query is required")
    runtime = hybrid.semantic
    word, _ = register_callback(hybrid, adapter)
    original = runtime._account_semantic_step
    observed = []

    def account():
        original()
        if observed or not any(type(entry) is ForeignContinuation
                               for entry in runtime.main_context.returns.snapshot()):
            return
        frame = adapter._frames[-1]
        root = adapter._root_token
        operation = frame.operation_token
        request = frame.request.request_token
        receipt = adapter.last_receipt()
        before = (machine_snapshot(hybrid), adapter.totals, totals(hybrid),
                  runtime._foreign_tasks._task_root.ledger.semantic_steps)
        for _ in range(3):
            assert adapter.validate_parked(root, operation, request) is True
            assert adapter.last_receipt() is receipt
            assert adapter._frames[-1] is frame
            assert (machine_snapshot(hybrid), adapter.totals, totals(hybrid),
                    runtime._foreign_tasks._task_root.ledger.semantic_steps) == before
        for arguments in ((object(), operation, request), (root, object(), request),
                          (root, operation, object()), (root, operation, None)):
            with pytest.raises(ForeignTaskError):
                adapter.validate_parked(*arguments)
            assert adapter.last_receipt() is receipt and adapter._frames[-1] is frame
            assert (machine_snapshot(hybrid), adapter.totals, totals(hybrid),
                    runtime._foreign_tasks._task_root.ledger.semantic_steps) == before
        observed.append((root, operation, request))

    monkeypatch.setattr(runtime, "_account_semantic_step", account)
    runtime.main_context.data.push(7)
    report = hybrid.execute(word.xt)
    assert len(observed) == 1 and runtime.main_context.data.snapshot() == (7, 7)
    assert report.machine_instructions == 5 and report.callback_requests == 1
    assert report.callback_semantic_steps == 1
    assert adapter.last_receipt().sequence == 4
    before = machine_snapshot(hybrid), totals(hybrid), adapter.last_receipt()
    with pytest.raises(ForeignTaskError):
        adapter.validate_parked(*observed[0])
    assert (machine_snapshot(hybrid), totals(hybrid), adapter.last_receipt()) == before
    assert_idle(hybrid, adapter)
