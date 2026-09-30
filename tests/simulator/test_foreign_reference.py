"""Data-only task adapter transitions, before dispatcher integration."""

from dataclasses import replace

import pytest

from shared.foreign_abi import (
    ForeignBudgetV1, ForeignCallbackRequestV1, ForeignCompletedV1, ForeignExportV1,
    ForeignFailedV1, ForeignRunnableYieldV1, ForeignSignatureV1, ForeignSpanV1,
)
from tests.simulator.foreign_reference import (
    Callback, Failure, Input, Reply, Return, ScriptedForeignAdapter, Store,
)


def signature(inputs=0, outputs=0):
    return ForeignSignatureV1(input_cells=inputs, output_cells=outputs)


def budget(quantum=0, *, instructions=20, callbacks=10):
    return ForeignBudgetV1(invocation_instructions_remaining=instructions,
                           root_instructions_remaining=instructions,
                           invocation_callbacks_remaining=callbacks,
                           root_callbacks_remaining=callbacks,
                           quantum_instructions=quantum)


def start(adapter, operation, arguments=(), *, token=None):
    return adapter.begin(operation, arguments, root_token=object() if token is None else token,
                         root_id=1, budget=budget())


def test_zero_quantum_admission_has_no_store_or_instruction_effect():
    memory = bytearray(8)
    adapter = ScriptedForeignAdapter(memory, memory_base=0x1000)
    operation = adapter.register(signature(1, 1),
        (Store(address=0x1000, data=b"done"), Return(outputs=(Input(0),))),
        machine_grants=(ForeignSpanV1(base=0x1000, size=8, access="write"),))
    event = start(adapter, operation, (37,))
    assert type(event) is ForeignRunnableYieldV1
    assert (event.receipt.instructions, event.receipt.cycles, event.receipt.root_entries) == (0, 0, 1)
    assert event.receipt.invocation_started and memory == bytearray(8)
    done = adapter.advance(event.operation_token, budget=budget(20))
    assert type(done) is ForeignCompletedV1 and done.outputs == (37,)
    assert done.receipt.root_instructions == 2 and memory[:4] == b"done"


def test_rejected_begin_spends_no_entry_or_memory_and_copied_operation_has_no_authority():
    adapter = ScriptedForeignAdapter()
    operation = adapter.register(signature(), (Return(outputs=()),))
    with pytest.raises(ValueError, match="not issued"):
        start(adapter, replace(operation))
    with pytest.raises(ValueError, match="before admission"):
        adapter.begin(operation, (), root_token=object(), root_id=1,
                      budget=budget(instructions=0))
    assert adapter.last_receipt() is None and adapter.active_invocations == ()
    assert adapter.admitted_inputs == ()


def test_every_runnable_segment_and_reply_requires_current_one_shot_token():
    adapter = ScriptedForeignAdapter()
    export = ForeignExportV1(export=object(), signature=signature(1, 1))
    operation = adapter.register(signature(1, 1),
        (Callback(export=export, arguments=(Input(0),)), Return(outputs=(Reply(0),))))
    admitted = start(adapter, operation, (5,))
    paused = adapter.advance(admitted.operation_token, budget=budget(0))
    assert paused.operation_token is not admitted.operation_token
    with pytest.raises(ValueError, match="stale"):
        adapter.advance(admitted.operation_token, budget=budget(1))
    requested = adapter.advance(paused.operation_token, budget=budget(1))
    assert type(requested) is ForeignCallbackRequestV1 and requested.arguments == (5,)
    runnable = adapter.reply(requested.request_token, (8,), budget=budget(0))
    with pytest.raises(ValueError, match="consumed"):
        adapter.reply(requested.request_token, (9,), budget=budget(0))
    completed = adapter.advance(runnable.operation_token, budget=budget(1))
    assert completed.outputs == (8,) and adapter.replies == ((1, 1, (8,)),)


def test_child_suffix_cancel_preserves_exact_parent_request_and_original_counters():
    adapter = ScriptedForeignAdapter()
    child = adapter.register(signature(), (Return(outputs=()),))
    export = ForeignExportV1(export=object(), signature=signature(0, 1))
    parent = adapter.register(signature(0, 1),
        (Callback(export=export, arguments=(), children=(child,)), Return(outputs=(Reply(0),))))
    root = object()
    initial = start(adapter, parent, token=root)
    request = adapter.advance(initial.operation_token, budget=budget(1))
    entered = adapter.begin(child, (), root_token=root, root_id=1, budget=budget(), parent=request)
    cancellation = adapter.cancel_suffix(entered.operation_token)
    assert cancellation.retired_invocation_ids == (2,)
    assert cancellation.surviving_parent_token is request.request_token
    assert cancellation.receipt is entered.receipt and cancellation.receipt.root_entries == 2
    completed = adapter.reply(request.request_token, (17,), budget=budget(1))
    assert completed.outputs == (17,) and completed.receipt.root_instructions == 2
    assert completed.receipt.root_callbacks == 1 and completed.receipt.root_entries == 2


def test_unknown_child_rejected_without_replacing_parent_authority():
    adapter = ScriptedForeignAdapter()
    child = adapter.register(signature(), (Return(outputs=()),))
    export = ForeignExportV1(export=object(), signature=signature())
    parent = adapter.register(signature(), (Callback(export=export, arguments=()), Return(outputs=())))
    root = object()
    entered = start(adapter, parent, token=root)
    request = adapter.advance(entered.operation_token, budget=budget(1))
    with pytest.raises(ValueError, match="static callback edges"):
        adapter.begin(child, (), root_token=root, root_id=1, budget=budget(), parent=request)
    assert adapter.last_receipt() is request.receipt and adapter.active_invocations == (1,)
    assert type(adapter.reply(request.request_token, (), budget=budget(1))) is ForeignCompletedV1


def test_terminal_limit_retains_completed_store_prefix_and_cannot_be_resumed():
    memory = bytearray(8)
    adapter = ScriptedForeignAdapter(memory, memory_base=0x1000)
    operation = adapter.register(signature(),
        (Store(address=0x1000, data=b"yes"), Return(outputs=())), max_instructions=1,
        machine_grants=(ForeignSpanV1(base=0x1000, size=8, access="write"),))
    entered = start(adapter, operation)
    failed = adapter.advance(entered.operation_token, budget=budget(20))
    assert type(failed) is ForeignFailedV1 and failed.kind == "instruction_limit"
    assert failed.receipt.instructions == 1 and memory[:3] == b"yes"
    with pytest.raises(ValueError, match="not runnable"):
        adapter.advance(failed.operation_token, budget=budget(20))
    cancellation = adapter.cancel_all()
    assert cancellation.retired_invocation_ids == (1,) and cancellation.receipt is failed.receipt


def test_prefix_failure_is_typed_but_host_delivery_failure_preserves_exact_exception():
    adapter = ScriptedForeignAdapter()
    operation = adapter.register(signature(), (Failure(kind="decode_fault", instructions=2, cycles=3),))
    entered = start(adapter, operation)
    failed = adapter.advance(entered.operation_token, budget=budget(20))
    assert type(failed) is ForeignFailedV1 and failed.kind == "decode_fault"
    assert (failed.receipt.root_instructions, failed.receipt.root_cycles) == (2, 3)
    adapter.cancel_all()
    error = RuntimeError("event allocation failed")
    adapter.fail_delivery_after(1, error)
    with pytest.raises(RuntimeError) as caught:
        adapter.begin(operation, (), root_token=object(), root_id=2, budget=budget())
    assert caught.value is error
    receipt = adapter.last_receipt()
    assert receipt.invocation_started and receipt.instructions == receipt.cycles == 0
    cancellation = adapter.cancel_all()
    assert cancellation.receipt is receipt and cancellation.retired_invocation_ids == (2,)


def test_empty_chain_does_not_renew_original_root_or_accept_old_root_replay():
    adapter = ScriptedForeignAdapter()
    operation = adapter.register(signature(), (Return(outputs=()),))
    root = object()
    admitted = adapter.begin(operation, (), root_token=root, root_id=1,
                             budget=budget(instructions=1))
    completed = adapter.advance(admitted.operation_token, budget=budget(1))
    assert adapter.active_invocations == ()
    with pytest.raises(ValueError, match="before admission"):
        adapter.begin(operation, (), root_token=root, root_id=1, budget=budget())
    assert adapter.last_receipt() is completed.receipt
    newer = adapter.begin(operation, (), root_token=object(), root_id=2, budget=budget())
    adapter.cancel_all()
    with pytest.raises(ValueError, match="stale"):
        adapter.begin(operation, (), root_token=root, root_id=1, budget=budget())
    assert adapter.last_receipt() is newer.receipt
