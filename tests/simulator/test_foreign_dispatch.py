"""Task foreign callbacks run ordinary guest control on the original stacks."""

from pathlib import Path

import pytest

from shared.cells import u64
from shared.foreign_abi import ForeignSignatureV1, ForeignSpanV1
from simulator.errors import ExecutionError, ForthAbort, IllegalInstructionFault, StepBudgetExceeded
from simulator.foreign_control import ForeignContinuation, ForeignRetirementReason
from simulator.foreign_runtime import ForeignTaskBudgetExceeded, ForeignTaskError
from simulator.memory import EXTERNAL_BASE
from simulator.platform import create_one_core_address_space
from simulator.runtime import ExecutionResult, MegaForthRuntime, YieldedExecution
from simulator.stacks import ReturnStackShapeError
from tests.simulator.foreign_reference import (
    Callback, Failure, Input, Reply, Return, ScriptedForeignAdapter, Store,
)


EXCEPTIONS = Path(__file__).with_name("fixtures") / "kdos-exceptions-618-675.f"


def signature(inputs=0, outputs=0):
    return ForeignSignatureV1(input_cells=inputs, output_cells=outputs)


def runtime(*, exceptions=False):
    result = MegaForthRuntime(memory=create_one_core_address_space(
        external_size=0x10000, dense_backing=True), execution_backend="python")
    if exceptions:
        result.evaluate(EXCEPTIONS.read_bytes(), source_name=str(EXCEPTIONS))
    return result


def task_grants(result, *extra):
    context = result.main_context
    grants = tuple(ForeignSpanV1(base=stack.empty_pointer - 2048, size=2048, access="read_write")
                   for stack in (context.data, context.returns))
    handler = result.find("_TASK-HANDLERS")
    if handler is not None:
        grants += (ForeignSpanV1(base=handler.body_address, size=8, access="read_write"),)
    return grants + tuple(extra)


def adapter(result):
    view = result.memory.dense_backing.buffer_at(EXTERNAL_BASE)
    backing = view.obj
    view.release()
    assert type(backing) is bytearray
    return ScriptedForeignAdapter(backing, memory_base=EXTERNAL_BASE)


class ObservedAdapter:
    """Observe real transitions without replacing guest exception behavior."""

    def __init__(self, inner, context):
        self.inner, self.context = inner, context
        self.observations = []

    def _observe(self, name, value=None):
        self.observations.append((name, self.context.data.snapshot(), self.context.data.pointer,
                                  self.context.returns.pointer, value))

    def begin(self, *args, **kwargs):
        self._observe("begin", kwargs["budget"].quantum_instructions)
        result = self.inner.begin(*args, **kwargs)
        self._observe("admitted", result.receipt)
        return result

    def advance(self, *args, **kwargs):
        self._observe("advance", kwargs["budget"].quantum_instructions)
        return self.inner.advance(*args, **kwargs)

    def reply(self, token, outputs, **kwargs):
        self._observe("reply", outputs)
        return self.inner.reply(token, outputs, **kwargs)

    def cancel_suffix(self, token):
        self._observe("cancel_suffix")
        return self.inner.cancel_suffix(token)

    def cancel_all(self):
        self._observe("cancel_all")
        return self.inner.cancel_all()

    def last_receipt(self):
        return self.inner.last_receipt()


def capture(result, target, inputs=0, outputs=0, *, extra=(), dynamic=(), fault=None, steps=4096):
    return result._foreign_tasks.capture_export(target, signature(inputs, outputs),
        task_grants=task_grants(result, *extra), dynamic_targets=dynamic,
        fault_target=fault, max_semantic_steps=steps)


def register(result, inner, script, *, inputs=0, outputs=0, grants=(), name="MACHINE", observed=False,
             instructions=100, callbacks=10):
    operation = inner.register(signature(inputs, outputs), script, machine_grants=grants,
                               max_instructions=instructions, max_callbacks=callbacks)
    used = ObservedAdapter(inner, result.main_context) if observed else inner
    word = result._foreign_tasks.define_operation(name, used, operation)
    return word, used


def observe_foreign_cells(result, monkeypatch):
    original = result._account_semantic_step
    entries, samples = {}, []

    def account():
        original()
        for entry in result.main_context.returns.snapshot():
            if type(entry) is ForeignContinuation:
                entries[id(entry)] = entry
                samples.append((entry, result.memory.read64(entry.slot_address)))

    monkeypatch.setattr(result, "_account_semantic_step", account)
    return entries, samples


def assert_finished(result, inner):
    assert inner.active_invocations == ()
    assert result.main_context.returns.snapshot() == ()
    assert result.main_context.returns.pointer == result.main_context.returns.empty_pointer
    assert result.main_context.reusable
    handler = result.find("_TASK-HANDLERS")
    if handler is not None:
        assert result.memory.read64(handler.body_address) == 0


def test_admission_precedes_input_pop_and_normal_callback_uses_exact_original_stack_cells(monkeypatch):
    result = runtime()
    context, inner = result.main_context, adapter(result)
    original_data, original_returns = context.data, context.returns
    export = capture(result, "DUP", 1, 2)
    word, observed = register(result, inner,
        (Callback(export=export, arguments=(Input(0),)), Return(outputs=(Reply(0), Reply(1)))),
        inputs=1, outputs=2, observed=True)
    entries, samples = observe_foreign_cells(result, monkeypatch)
    context.data.push(99)
    context.data.push(7)
    initial_pointer = context.data.pointer
    report = result.execute(word.xt)

    assert type(report) is ExecutionResult
    assert context.data is original_data and context.returns is original_returns
    assert context.data.snapshot() == (99, 7, 7)
    assert context.data.pointer == initial_pointer - 8
    assert result.memory.read_bytes(context.data.pointer, 24) == b"".join(
        value.to_bytes(8, "little") for value in (7, 7, 99))
    first, admitted, advancing = observed.observations[:3]
    assert first[:3] == ("begin", (99, 7), initial_pointer) and first[4] == 0
    assert admitted[:3] == ("admitted", (99, 7), initial_pointer)
    assert admitted[4].invocation_started and admitted[4].instructions == admitted[4].cycles == 0
    assert advancing[:3] == ("advance", (99,), initial_pointer + 8)
    replies = [item for item in observed.observations if item[0] == "reply"]
    assert len(replies) == 1 and replies[0][1] == (99,) and replies[0][4] == (7, 7)
    assert inner.admitted_inputs == ((1, (7,)),) and inner.replies == ((1, 1, (7, 7)),)
    assert inner.last_receipt().root_instructions == 2 and inner.last_receipt().root_callbacks == 1
    assert len(entries) == 1 and samples
    entry, = entries.values()
    assert all(raw == current.raw_cookie for current, raw in samples)
    assert entry.retired and entry.retirement_reason is ForeignRetirementReason.RETURNED
    assert result.memory.read64(entry.slot_address) == entry.raw_cookie
    dispatch = result._foreign_tasks.last_dispatch
    assert (dispatch.machine_instructions, dispatch.machine_cycles, dispatch.callbacks,
            dispatch.entries, dispatch.semantic_steps) == (2, 2, 1, 1, 1)
    assert dispatch.completed and not dispatch.cancelled
    assert_finished(result, inner)


def test_real_throw_zero_continues_callback_then_returns_normal_machine_outputs():
    result = runtime(exceptions=True)
    result.evaluate(b": ZERO-CALLBACK 7 0 THROW 8 ;")
    inner = adapter(result)
    export = capture(result, "ZERO-CALLBACK", outputs=2)
    word, _ = register(result, inner,
        (Callback(export=export, arguments=()), Return(outputs=(Reply(0), Reply(1)))), outputs=2)
    result.main_context.data.push(99)
    result.execute(word.xt)
    assert result.main_context.data.snapshot() == (99, 7, 8)
    assert inner.replies == ((1, 1, (7, 8)),)
    assert inner.last_receipt().state == "returned" and inner.last_receipt().root_instructions == 2
    assert_finished(result, inner)


def test_callback_may_change_granted_cells_below_its_data_frontier_without_stack_rollback():
    result = runtime()
    context, inner = result.main_context, adapter(result)
    saved_cell = context.data.empty_pointer - 8
    result.evaluate(f": CHANGE-BASE 123 {saved_cell} ! 7 ;".encode())
    export = capture(result, "CHANGE-BASE", outputs=1)
    word, _ = register(result, inner,
        (Callback(export=export, arguments=()), Return(outputs=(Reply(0),))), inputs=1, outputs=1)
    context.data.push(99)
    context.data.push(55)
    result.execute(word.xt)
    assert context.data.snapshot() == (123, 7)
    assert result.memory.read64(saved_cell) == 123
    assert inner.admitted_inputs == ((1, (55,)),) and inner.replies == ((1, 1, (7,)),)
    assert_finished(result, inner)


def test_wrong_callback_arity_preserves_actual_stack_effects_without_machine_reply():
    result = runtime()
    inner = adapter(result)
    export = capture(result, "DUP", 1, 1)
    word, _ = register(result, inner,
        (Callback(export=export, arguments=(Input(0),)), Return(outputs=(Reply(0),))), inputs=1, outputs=1)
    result.main_context.data.push(99)
    result.main_context.data.push(7)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert result.main_context.data.snapshot() == (99, 7, 7)
    assert inner.replies == () and inner.active_invocations == ()
    assert inner.last_receipt().root_instructions == 1
    assert result.main_context.returns.snapshot() == ()


def test_denied_semantic_store_keeps_operand_pops_and_completed_machine_store_prefix():
    result = runtime()
    result.evaluate(f": DENIED 90 {EXTERNAL_BASE + 8} C! ;".encode())
    inner = adapter(result)
    export = capture(result, "DENIED")  # No ordinary external task grant.
    word, _ = register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=()), Return(outputs=())),
        inputs=1, grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.main_context.data.push(99)
    result.main_context.data.push(7)
    with pytest.raises(ForeignTaskError):
        result.execute(word.xt)
    assert result.memory.read8(EXTERNAL_BASE) == ord("p")
    assert result.memory.read8(EXTERNAL_BASE + 8) == 0
    assert result.main_context.data.snapshot() == (99,)
    assert inner.replies == () and inner.active_invocations == ()
    assert inner.last_receipt().root_instructions == 2 and inner.last_receipt().root_callbacks == 1
    assert result.main_context.returns.snapshot() == ()


def test_typed_machine_failure_keeps_prefix_and_does_not_enter_guest_fault_hook():
    result = runtime()
    result.evaluate(f": HOOK DROP 77 {EXTERNAL_BASE + 8} C! ;".encode())
    result.set_fault_xt(result.find("HOOK").xt)
    inner = adapter(result)
    word, _ = register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Failure(kind="decode_fault", instruction_pc=0x1234)),
        inputs=1, grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.main_context.data.push(99)
    result.main_context.data.push(7)
    with pytest.raises(ForeignTaskError) as caught:
        result.execute(word.xt)
    assert caught.value.event.kind == "decode_fault" and caught.value.event.instruction_pc == 0x1234
    assert result.memory.read8(EXTERNAL_BASE) == ord("p") and result.memory.read8(EXTERNAL_BASE + 8) == 0
    assert result.main_context.data.snapshot() == (99,)
    assert inner.last_receipt().root_instructions == 1 and inner.last_receipt().root_callbacks == 0
    assert inner.active_invocations == ()


@pytest.mark.parametrize("boundary", ["admission", "completed_prefix"])
def test_actual_adapter_delivery_exception_settles_receipt_before_cancel_without_recreating_inputs(boundary):
    result = runtime()
    context, inner = result.main_context, adapter(result)
    export = capture(result, "DUP", 1, 2)
    word, _ = register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=(Input(0),)),
         Return(outputs=(Reply(0), Reply(1)))), inputs=1, outputs=2,
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    error = KeyboardInterrupt("owned adapter delivery interrupted")
    inner.fail_delivery_after(1 if boundary == "admission" else 2, error)
    context.data.push(99)
    context.data.push(7)
    with pytest.raises(KeyboardInterrupt) as caught:
        result.execute(word.xt)
    assert caught.value is error
    receipt = inner.last_receipt()
    dispatch = result._foreign_tasks.last_dispatch
    assert dispatch.entries == receipt.root_entries == 1
    if boundary == "admission":
        assert context.data.snapshot() == (99, 7)
        assert result.memory.read8(EXTERNAL_BASE) == 0
        assert receipt.invocation_started and receipt.instructions == receipt.cycles == 0
        assert dispatch.machine_instructions == dispatch.callbacks == 0
    else:
        assert context.data.snapshot() == (99,)
        assert result.memory.read8(EXTERNAL_BASE) == ord("p")
        assert receipt.root_instructions == dispatch.machine_instructions == 2
        assert receipt.root_callbacks == dispatch.callbacks == 1
    assert dispatch.semantic_steps == 0 and dispatch.cancelled and not dispatch.completed
    assert inner.active_invocations == () and inner.replies == ()
    assert context.returns.snapshot() == ()


def test_admitted_division_fault_uses_captured_guest_throw_hook_and_outer_real_catch():
    result = runtime(exceptions=True)
    result.evaluate(b": BROKEN 1 0 / ;")
    hook = result.find("THROW")
    result.set_fault_xt(hook.xt)
    inner = adapter(result)
    export = capture(result, "BROKEN", fault=hook)
    register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=()), Return(outputs=())),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    result.execute("OUTER")
    assert result.main_context.data.snapshot() == (99, u64(-10), 5)
    assert result.memory.read8(EXTERNAL_BASE) == ord("p")
    assert inner.replies == () and inner.last_receipt().root_instructions == 2
    assert_finished(result, inner)


def test_raw_accounting_instruction_fault_is_preserved_and_never_becomes_guest_throw(monkeypatch):
    result = runtime(exceptions=True)
    result.evaluate(f": HOOK 77 {EXTERNAL_BASE + 8} C! THROW ;".encode())
    hook = result.find("HOOK")
    result.set_fault_xt(hook.xt)
    inner = adapter(result)
    export = capture(result, "DROP", 1, 0, fault=hook,
                     extra=(ForeignSpanV1(base=EXTERNAL_BASE + 8, size=1, access="write"),))
    register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=(7,)), Return(outputs=())),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    original, error = result._account_semantic_step, IllegalInstructionFault("host accounting fault")
    observed = []

    def account():
        original()
        if any(type(entry) is ForeignContinuation for entry in result.main_context.returns.snapshot()):
            observed.append(True)
            raise error

    monkeypatch.setattr(result, "_account_semantic_step", account)
    with pytest.raises(IllegalInstructionFault) as caught:
        result.execute("OUTER")
    assert caught.value is error and observed == [True]
    assert result.memory.read8(EXTERNAL_BASE) == ord("p") and result.memory.read8(EXTERNAL_BASE + 8) == 0
    assert inner.replies == () and inner.active_invocations == ()
    assert inner.last_receipt().root_instructions == 2
    assert result._foreign_tasks.last_dispatch.semantic_steps == 1
    assert not result.main_context.reusable


def test_abort_in_callback_is_not_caught_as_a_guest_throw_and_keeps_machine_prefix():
    result = runtime(exceptions=True)
    result.evaluate(b": STOP 11 ABORT 88 ;")
    inner = adapter(result)
    export = capture(result, "STOP")
    register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=()), Return(outputs=())),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),))
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    with pytest.raises(ForthAbort) as caught:
        result.execute("OUTER")
    assert caught.value.origin_context is result.main_context
    assert result.main_context.data.snapshot() == () and result.main_context.returns.snapshot() == ()
    assert not result.main_context.reusable
    assert result.memory.read8(EXTERNAL_BASE) == ord("p")
    assert inner.active_invocations == () and inner.replies == ()


def test_original_semantic_budget_escape_in_callback_does_not_finish_guest_catch():
    result = runtime(exceptions=True)
    result.evaluate(b": SPIN BEGIN 1 DROP AGAIN ;")
    inner = adapter(result)
    export = capture(result, "SPIN")
    register(result, inner, (Callback(export=export, arguments=()), Return(outputs=())))
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    with pytest.raises(StepBudgetExceeded):
        result.execute("OUTER", step_budget=200)
    assert inner.last_receipt().root_instructions == 1 and inner.replies == ()
    assert inner.active_invocations == () and result.main_context.returns.snapshot() == ()
    assert not result.main_context.reusable
    assert result.memory.read64(result.find("_TASK-HANDLERS").body_address) != 0


def test_callback_semantic_allowance_counts_completed_first_callback_across_empty_chain():
    result = runtime()
    inner = adapter(result)
    export = capture(result, "NEGATE", 1, 1)
    register(result, inner, (Callback(export=export, arguments=(7,)), Return(outputs=())))
    result.evaluate(b": TWO MACHINE MACHINE ;")
    result._foreign_tasks.configure_limits(semantic_limit=1)
    with pytest.raises(ForeignTaskBudgetExceeded):
        result.execute("TWO")
    assert inner.replies == ((1, 1, (u64(-7),)),)
    report = result._foreign_tasks.last_dispatch
    assert report.semantic_steps == 1
    assert report.machine_instructions == 3 and report.callbacks == report.entries == 2
    assert inner.last_receipt().state == "callback" and inner.active_invocations == ()


@pytest.mark.parametrize("quantum", [None, 1, 3])
def test_empty_chain_and_outer_semantic_quanta_do_not_renew_original_machine_allowance(quantum):
    result = runtime()
    inner = adapter(result)
    register(result, inner, (Return(outputs=()),))
    result.evaluate(b": TWO MACHINE MACHINE ;")
    result._foreign_tasks.configure_limits(instruction_limit=1)
    with pytest.raises(ForeignTaskBudgetExceeded):
        current = result.run_until_blocked("TWO", quantum_steps=quantum)
        for _ in range(30):
            assert type(current) is YieldedExecution
            assert inner.active_invocations == ()
            current = result.resume_yielded(current.suspension)
        pytest.fail("second foreign admission incorrectly renewed its root allowance")
    assert inner.admitted_inputs == ((1, ()),)
    assert inner.last_receipt().state == "returned" and inner.last_receipt().root_entries == 1
    report = result._foreign_tasks.last_dispatch
    assert report.machine_instructions == report.entries == 1 and report.callbacks == 0
    assert inner.active_invocations == ()
    assert result.main_context.reusable
    result.execute("MACHINE")
    fresh = result._foreign_tasks.last_dispatch
    assert fresh.root_id > report.root_id and fresh.machine_instructions == fresh.entries == 1
    assert fresh.completed and not fresh.cancelled


def test_real_local_catch_preserves_foreign_return_and_replies_with_throw_code(monkeypatch):
    result = runtime(exceptions=True)
    result.evaluate(b": RAISE 11 22 -17 THROW 88 ;")
    inner = adapter(result)
    raise_word = result.find("RAISE")
    export = capture(result, "CATCH", 1, 1, dynamic=(raise_word,))
    word, _ = register(result, inner,
        (Callback(export=export, arguments=(raise_word.xt,)), Return(outputs=(Reply(0),))), outputs=1)
    entries, samples = observe_foreign_cells(result, monkeypatch)
    result.main_context.data.push(99)
    result.execute(word.xt)
    assert result.main_context.data.snapshot() == (99, u64(-17))
    assert inner.replies == ((1, 1, (u64(-17),)),)
    assert inner.last_receipt().state == "returned" and inner.last_receipt().root_instructions == 2
    assert len(entries) == 1 and samples
    entry, = entries.values()
    assert entry.retirement_reason is ForeignRetirementReason.RETURNED
    assert_finished(result, inner)


def test_real_throw_to_outer_catch_cancels_foreign_suffix_and_keeps_guest_handler_cleanup(monkeypatch):
    result = runtime(exceptions=True)
    result.evaluate(b": RAISE 11 22 -17 THROW 88 ;")
    inner = adapter(result)
    export = capture(result, "RAISE")
    _, observed = register(result, inner,
        (Callback(export=export, arguments=()), Return(outputs=())), observed=True)
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    entries, _ = observe_foreign_cells(result, monkeypatch)
    result.execute("OUTER")
    assert result.main_context.data.snapshot() == (99, u64(-17), 5)
    assert inner.replies == () and inner.last_receipt().state == "callback"
    assert inner.last_receipt().root_instructions == inner.last_receipt().root_callbacks == 1
    cancellations = [item for item in observed.observations if item[0].startswith("cancel")]
    assert len(cancellations) == 1
    assert len(entries) == 1
    entry, = entries.values()
    assert entry.retired and entry.retirement_reason is ForeignRetirementReason.FRONTIER
    assert_finished(result, inner)


def test_retired_semantic_tail_keeps_unwind_code_but_cannot_enter_a_captured_machine_child():
    result = runtime(exceptions=True)
    inner = adapter(result)
    child_operation = inner.register(signature(), (Return(outputs=()),))
    result._foreign_tasks.define_operation("CHILD", inner, child_operation)
    result.evaluate(b": DISCARD HANDLER @ RP! CHILD ;")
    export = capture(result, "DISCARD")
    register(result, inner,
        (Callback(export=export, arguments=(), children=(child_operation,)), Return(outputs=())))
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    with pytest.raises(ForeignTaskError, match="retired|machine-entry|machine.*authority"):
        result.execute("OUTER")
    assert inner.admitted_inputs == ((1, ()),)
    assert inner.last_receipt().root_entries == inner.last_receipt().root_instructions == 1
    assert inner.active_invocations == () and inner.replies == ()
    assert not result.main_context.reusable


def test_raw_foreign_cookie_repair_in_a_later_operation_never_restores_reply_authority(monkeypatch):
    result = runtime(exceptions=True)
    observed_address = EXTERNAL_BASE + 16
    result.evaluate(f": REPAIR RP@ DUP @ SWAP 0 OVER ! ! 1 {observed_address} C! ;".encode())
    inner = adapter(result)
    export = capture(result, "REPAIR",
                     extra=(ForeignSpanV1(base=observed_address, size=1, access="write"),))
    _, observed = register(result, inner,
        (Callback(export=export, arguments=()), Return(outputs=())), observed=True)
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    entries, _ = observe_foreign_cells(result, monkeypatch)
    with pytest.raises((ExecutionError, ReturnStackShapeError)):
        result.execute("OUTER")
    assert len(entries) == 1
    entry, = entries.values()
    assert entry.retired and entry.retirement_reason is ForeignRetirementReason.COOKIE
    cancellations = [item for item in observed.observations if item[0] == "cancel_suffix"]
    assert len(cancellations) == 1
    assert cancellations[0][1][-2:] == (entry.raw_cookie, entry.slot_address)
    assert result.memory.read8(observed_address) == 1
    assert result.memory.read64(entry.slot_address) == entry.raw_cookie
    assert inner.replies == () and inner.active_invocations == ()
    assert inner.last_receipt().root_instructions == 1
    assert not result.main_context.reusable


@pytest.mark.parametrize("after_retirement", [False, True])
def test_cancellation_failure_keeps_completed_rp_store_pop_and_initial_exception(after_retirement):
    result = runtime(exceptions=True)
    result.evaluate(b": RAISE -17 THROW ;")
    inner = adapter(result)
    export = capture(result, "RAISE")
    _, observed = register(result, inner,
        (Store(address=EXTERNAL_BASE, data=b"p"), Callback(export=export, arguments=()), Return(outputs=())),
        grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=1, access="write"),), observed=True)
    result.evaluate(b": OUTER 99 ['] MACHINE CATCH 5 ;")
    error = RuntimeError("reference suffix cancellation failed")
    inner.fail_next_cancel(error, after_retirement=after_retirement)
    with pytest.raises(RuntimeError) as caught:
        result.execute("OUTER")
    assert caught.value is error
    cancellations = [item for item in observed.observations if item[0] == "cancel_suffix"]
    assert cancellations
    handler_pointer = result.memory.read64(result.find("_TASK-HANDLERS").body_address)
    assert handler_pointer != 0
    assert cancellations[0][1] == (99, u64(-17))
    assert cancellations[0][3] == handler_pointer
    assert result.memory.read8(EXTERNAL_BASE) == ord("p")
    assert inner.replies == () and inner.last_receipt().root_instructions == 2
    assert result._foreign_tasks.last_dispatch.machine_instructions == 2
    assert not result.main_context.reusable
    with pytest.raises(ExecutionError):
        result.execute("MACHINE")
    inner.cancel_all()  # Test-owner cleanup if adapter cancellation failed before retiring its frame.
