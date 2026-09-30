"""Task capture and root-accounting foundation; no foreign dispatch exposure."""

from __future__ import annotations

from dataclasses import replace
from pathlib import Path

import pytest

from shared.foreign_abi import (
    ForeignOperationV1, ForeignReceiptV1, ForeignSignatureV1, ForeignSpanV1,
)
from simulator import core_words
from simulator import foreign_runtime
from simulator.dictionary import Word
from simulator.foreign_runtime import (
    ForeignDefinition, ForeignRootLedger, ForeignTaskBudgetExceeded,
    ForeignTaskError,
)
from simulator.ir import Call, Idle, Literal, Return
from simulator.memory import EXTERNAL_BASE, MMIO_BASE, MemoryAccessError
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime, _StepMeter


class _Adapter:
    """Registration fixture only; every execution route is deliberately fatal."""

    def begin(self, *args, **kwargs):
        raise AssertionError("capture must not execute the adapter")

    def advance(self, *args, **kwargs):
        raise AssertionError("capture must not execute the adapter")

    def reply(self, *args, **kwargs):
        raise AssertionError("capture must not execute the adapter")

    def cancel_suffix(self, *args, **kwargs):
        raise AssertionError("capture must not execute the adapter")

    def cancel_all(self):
        raise AssertionError("capture must not execute the adapter")

    def last_receipt(self):
        raise AssertionError("capture must not execute the adapter")


def signature(inputs=0, outputs=0):
    return ForeignSignatureV1(input_cells=inputs, output_cells=outputs)


def operation(*, instructions=20, callbacks=4, grants=()):
    return ForeignOperationV1(registration=object(), signature=signature(),
                              machine_grants=grants, max_instructions=instructions,
                              max_callbacks=callbacks)


def runtime():
    result = MegaForthRuntime(memory=create_one_core_address_space(external_size=0x10000),
                             execution_backend="python")
    return result


def load_exceptions(result):
    source = Path(__file__).with_name("fixtures") / "kdos-exceptions-618-675.f"
    result.evaluate(source.read_bytes(), source_name=str(source))


def test_real_kdos_exception_dependencies_capture_without_replacing_stacks():
    result = runtime()
    data, returns = result.main_context.data, result.main_context.returns
    load_exceptions(result)
    result.evaluate(b": RAISE -17 THROW ;")
    handler = result.find("_TASK-HANDLERS").body_address
    descriptor = result._foreign_tasks.capture_export(
        "CATCH", signature(1, 1), dynamic_targets=(result.find("RAISE"),),
        task_grants=(ForeignSpanV1(base=handler, size=8, access="read_write"),),
    )
    capture = result._foreign_tasks.require_export(descriptor)
    names = {item.word.name for item in capture.words}
    assert {b"CATCH", b"THROW", b"HANDLER", b"EXECUTE", b"SP@", b"RP@"} <= names
    assert result.main_context.data is data and result.main_context.returns is returns
    assert not returns.has_foreign_state
    assert data.snapshot() == () and returns.snapshot() == ()


def test_dynamic_target_requires_exact_capture_and_name_shadow_does_not_retarget():
    result = runtime()
    original = result.find("DUP")
    descriptor = result._foreign_tasks.capture_export(original, signature(1, 2))
    replacement = result.define_primitive("DUP", lambda context: None)
    capture = result._foreign_tasks.require_export(descriptor)
    assert capture.require_target(result._foreign_tasks, original).word is original
    with pytest.raises(ForeignTaskError, match="not captured"):
        capture.require_target(result._foreign_tasks, replacement)
    with pytest.raises(ForeignTaskError, match="original admitted core"):
        result._foreign_tasks.capture_export(replacement, signature())


def test_copied_export_and_foreign_definition_do_not_grant_authority():
    result = runtime()
    descriptor = result._foreign_tasks.capture_export("DUP", signature(1, 2))
    with pytest.raises(ForeignTaskError, match="issued identity"):
        result._foreign_tasks.require_export(replace(descriptor))
    word = result._foreign_tasks.define_operation("MACHINE", _Adapter(), operation())
    assert type(word.implementation) is ForeignDefinition
    forged = result.dictionary.define("FORGED", replace(word.implementation))
    with pytest.raises(ForeignTaskError, match="issued registration"):
        result._foreign_tasks.require_definition(forged)


@pytest.mark.parametrize("field", ("signature", "task_grants", "max_semantic_steps"))
def test_valid_late_export_mutation_does_not_expand_capture(field):
    result = runtime()
    descriptor = result._foreign_tasks.capture_export("DUP", signature(1, 2), max_semantic_steps=4)
    replacement = {
        "signature": signature(0, 2),
        "task_grants": (ForeignSpanV1(base=EXTERNAL_BASE, size=8, access="read_write"),),
        "max_semantic_steps": 100,
    }[field]
    object.__setattr__(descriptor, field, replacement)
    with pytest.raises(ForeignTaskError, match="descriptor changed"):
        result._foreign_tasks.require_export(descriptor)


@pytest.mark.parametrize("field", ("signature", "grant"))
def test_nested_operation_metadata_has_independent_original_evidence(field):
    result = runtime()
    grant = ForeignSpanV1(base=EXTERNAL_BASE, size=8, access="read")
    descriptor = operation(grants=(grant,))
    word = result._foreign_tasks.define_operation("MACHINE", _Adapter(), descriptor)
    if field == "signature":
        object.__setattr__(descriptor.signature, "input_cells", 1)
    else:
        object.__setattr__(grant, "size", 16)
    with pytest.raises(ForeignTaskError, match="descriptor changed"):
        result._foreign_tasks.require_definition(word)


def test_protected_storage_remains_original_after_caller_changes_span():
    result = runtime()
    protected = ForeignSpanV1(base=EXTERNAL_BASE, size=32, access="read_write")
    result._foreign_tasks.define_operation("MACHINE", _Adapter(), operation(),
                                           protected_spans=(protected,))
    object.__setattr__(protected, "size", 0)
    with pytest.raises(ForeignTaskError, match="protected machine storage"):
        result._foreign_tasks.capture_export("DUP", signature(1, 2),
            task_grants=(ForeignSpanV1(base=EXTERNAL_BASE, size=8, access="read"),))


def test_created_action_captures_only_its_existing_does_suffix_and_selected_execute():
    result = runtime()
    result.evaluate(b": MAKER CREATE 0 , DOES> @ EXECUTE ; MAKER ACTION : TARGET 11 ;")
    created, target = result.find("ACTION"), result.find("TARGET")
    result.memory.write64(created.body_address, target.xt)
    action = created.implementation.action
    descriptor = result._foreign_tasks.capture_export(
        created, signature(0, 1), dynamic_targets=(target,),
        task_grants=(ForeignSpanV1(base=created.body_address, size=8, access="read"),),
    )
    capture = result._foreign_tasks.require_export(descriptor)
    defining_word = result.dictionary.resolve(action.source_xt)
    capture.require_target(result._foreign_tasks, defining_word, entry_ip=action.entry_ip)
    with pytest.raises(ForeignTaskError, match="captured suffix"):
        capture.require_target(result._foreign_tasks, defining_word, entry_ip=0)
    created.implementation.action = None
    with pytest.raises(ForeignTaskError, match="CREATE action changed"):
        result._foreign_tasks.require_export(descriptor)


@pytest.mark.parametrize("change", ("word_property", "ir_slot", "namespace", "helper_code"))
def test_changed_getter_or_helper_is_rejected_before_caller_code(change, monkeypatch):
    result = runtime()
    word = result.define_colon("ADMITTED", (Literal(7), Call(result.find("@").xt), Return()))
    descriptor = result._foreign_tasks.capture_export(word, signature(0, 1))
    calls = []

    def unexpected(*args):
        calls.append(args)
        raise AssertionError("changed getter must not be invoked")

    if change == "word_property":
        monkeypatch.setattr(Word, "body_address", property(unexpected))
    elif change == "ir_slot":
        monkeypatch.setattr(Literal, "value", property(unexpected))
    elif change == "namespace":
        result.memory.__dict__[object()] = "not an attribute name"
    else:
        def replacement_fetch(runtime, context):
            raise AssertionError("changed helper must not be invoked")

        original_code = core_words._fetch.__code__
        monkeypatch.setattr(core_words._fetch, "__code__", replacement_fetch.__code__)
        assert core_words._fetch.__code__ is not original_code
    with pytest.raises(ForeignTaskError, match="routing changed|implementation changed|namespace is not canonical"):
        result._foreign_tasks.require_export(descriptor)
    assert calls == []


def test_changed_ir_removed_word_and_fault_hook_reject_existing_capture():
    result = runtime()
    checkpoint = result.dictionary.checkpoint()
    literal = Literal(7)
    word = result.define_colon("TEMP", (literal, Return()))
    descriptor = result._foreign_tasks.capture_export(word, signature(0, 1))
    object.__setattr__(literal, "value", 8)
    with pytest.raises(ForeignTaskError, match="operation field changed"):
        result._foreign_tasks.require_export(descriptor)
    object.__setattr__(literal, "value", 7)
    result.dictionary.rollback(checkpoint)
    with pytest.raises(ForeignTaskError, match="removed"):
        result._foreign_tasks.require_export(descriptor)
    fault = result.define_colon("FAULT", (Return(),))
    result.set_fault_xt(fault.xt)
    descriptor = result._foreign_tasks.capture_export("DUP", signature(1, 2), fault_target=fault)
    result.set_fault_xt(0)
    with pytest.raises(ForeignTaskError, match="fault hook changed"):
        result._foreign_tasks.require_export(descriptor)


@pytest.mark.parametrize("grant_kind", ("header", "mmio", "cross_region", "machine_stack"))
def test_nonordinary_or_protected_grants_reject_before_registration(grant_kind):
    result = runtime()
    grant = {
        "header": ForeignSpanV1(base=result.find("DUP").xt, size=8, access="write"),
        "mmio": ForeignSpanV1(base=MMIO_BASE, size=8, access="read"),
        "cross_region": ForeignSpanV1(base=EXTERNAL_BASE + 0xFFFF, size=8, access="read"),
        "machine_stack": ForeignSpanV1(base=result.main_context.returns.floor, size=8, access="read"),
    }[grant_kind]
    before = result.dictionary.checkpoint()
    with pytest.raises((ForeignTaskError, ValueError, MemoryAccessError)):
        result._foreign_tasks.define_operation("INVALID", _Adapter(), operation(grants=(grant,)))
    assert result.dictionary.here == before.here and result.find("INVALID") is None


def test_registration_allocation_failure_rolls_back_exact_definition(monkeypatch):
    result = runtime()
    checkpoint = result.dictionary.checkpoint()
    words = result.dictionary.words
    failure = MemoryError("binding allocation")

    def fail_binding(*args, **kwargs):
        raise failure

    monkeypatch.setattr(foreign_runtime, "_ForeignBinding", fail_binding)
    with pytest.raises(MemoryError) as caught:
        result._foreign_tasks.define_operation("UNPUBLISHED", _Adapter(), operation())
    assert caught.value is failure
    assert result.dictionary.here == checkpoint.here
    assert result.dictionary.words == words and result.find("UNPUBLISHED") is None
    assert not result._foreign_tasks._bindings


def test_registration_growth_rejects_without_dispatching_guest_fault_hook():
    result = runtime()
    called = []
    hook = result.define_primitive("HOST-FAULT", lambda context: called.append(context))
    result.set_dictionary_fault_xt(hook.xt)
    result._dictionary_base = result.dictionary.here
    result._dictionary_limit = result.dictionary.here + 1
    before = result.main_context.data.snapshot()
    with pytest.raises(ForeignTaskError, match="dictionary growth rejected"):
        result._foreign_tasks.define_operation("TOO-LARGE", _Adapter(), operation())
    assert called == [] and result.main_context.data.snapshot() == before
    assert result.find("TOO-LARGE") is None


def test_failed_rollback_preserves_original_exception_and_disables_future_admission(monkeypatch):
    result = runtime()
    existing = result._foreign_tasks.capture_export("DUP", signature(1, 2))
    failure = MemoryError("binding allocation")
    rebuilds = []

    def fail_binding(*args, **kwargs):
        raise failure

    def fail_repair(index):
        assert index is result.dictionary_index
        rebuilds.append(None)
        raise RuntimeError("index repair")

    monkeypatch.setattr(foreign_runtime, "_ForeignBinding", fail_binding)
    monkeypatch.setattr(type(result.dictionary_index), "rebuild", fail_repair)
    with pytest.raises(MemoryError) as caught:
        result._foreign_tasks.define_operation("UNPUBLISHED", _Adapter(), operation())
    assert caught.value is failure
    assert rebuilds == [None]
    assert any("rollback failed" in note for note in failure.__notes__)
    with pytest.raises(ForeignTaskError, match="admission is disabled"):
        result._foreign_tasks.require_export(existing)
    with pytest.raises(ForeignTaskError, match="admission is disabled"):
        result._foreign_tasks.capture_export("DUP", signature(1, 2))
    with pytest.raises(ForeignTaskError, match="admission is disabled"):
        result._foreign_tasks.define_operation("LATER", _Adapter(), operation())


def test_reentrant_registration_during_publication_is_rejected_and_rolled_back(monkeypatch):
    result = runtime()
    checkpoint = result.dictionary.checkpoint()
    original_define = result._define_public_dictionary_word
    entered = []

    def reenter_after_publication(*args, **kwargs):
        word = original_define(*args, **kwargs)
        assert result._foreign_tasks._registration_active
        entered.append(word)
        result._foreign_tasks.define_operation("INNER", _Adapter(), operation())
        return word

    monkeypatch.setattr(result, "_define_public_dictionary_word", reenter_after_publication)
    with pytest.raises(ForeignTaskError, match="already in progress"):
        result._foreign_tasks.define_operation("OUTER", _Adapter(), operation())
    assert result.find("INNER") is None and result.find("OUTER") is None
    assert len(entered) == 1 and entered[0].name == b"OUTER"
    assert result.dictionary.here == checkpoint.here and not result._foreign_tasks._bindings
    assert result._foreign_tasks._publication_failure is None


def test_capture_limit_is_global_and_rejected_capture_spends_no_slots():
    result = runtime()
    body = (Literal(1),) * 2047 + (Return(),)
    word = result.define_colon("LARGE", body)
    first = result._foreign_tasks.capture_export(word, signature())
    second = result._foreign_tasks.capture_export(word, signature())
    with pytest.raises(ForeignTaskError, match="4096"):
        result._foreign_tasks.capture_export(word, signature())
    assert len(result._foreign_tasks._exports) == 2
    assert result._foreign_tasks.require_export(first).entry is word
    assert result._foreign_tasks.require_export(second).entry is word
    # A primitive capture consumes no IR capacity.
    result._foreign_tasks.capture_export("DUP", signature(1, 2))


def test_unsupported_unused_ir_is_rejected_without_publishing_capture():
    result = runtime()
    word = result.define_colon("IDL-CALLBACK", (Idle(), Return()))
    with pytest.raises(ForeignTaskError, match="unsupported operation"):
        result._foreign_tasks.capture_export(word, signature())
    assert not result._foreign_tasks._exports


def receipt(**changes):
    values = dict(root_id=1, invocation_id=1, parent_invocation_id=None, depth=1,
                  sequence=1, invocation_started=True, root_entries=1, state="yielded",
                  instructions=0, cycles=0, callback_requests=0,
                  invocation_instructions=0, invocation_cycles=0, invocation_callbacks=0,
                  root_instructions=0, root_cycles=0, root_callbacks=0)
    values.update(changes)
    return ForeignReceiptV1(**values)


def ledger(**limits):
    return ForeignRootLedger(_StepMeter(None, lambda: None), 1, **limits)


def settle(account, event, *, starting=None):
    return account.settle(event, issued_receipt=event, starting_operation=starting)


def test_zero_work_entry_then_exact_return_preserves_empty_chain_counters_and_meter():
    account = ledger(entry_limit=1)
    meter = account.meter
    entered = receipt()
    assert settle(account, entered, starting=operation())
    assert account.depth == account.entries == 1 and account.instructions == 0
    completed = receipt(sequence=2, invocation_started=False, state="returned",
                        instructions=1, cycles=2, invocation_instructions=1,
                        invocation_cycles=2, root_instructions=1, root_cycles=2)
    assert settle(account, completed)
    assert not settle(account, completed)
    assert account.depth == 0 and account.entries == 1 and account.instructions == 1
    assert account.cycles == 2 and account.meter is meter and meter.steps == 0
    with pytest.raises(ForeignTaskBudgetExceeded, match="entry allowance"):
        account.require_entry()


@pytest.mark.parametrize("field", ("meter", "root_id", "root_token", "instruction_limit",
                                   "callback_limit", "entry_limit", "semantic_limit"))
def test_original_root_meter_and_ceilings_are_read_only(field):
    account = ledger()
    original = getattr(account, field)
    with pytest.raises(AttributeError):
        setattr(account, field, object())
    assert getattr(account, field) is original


def test_issued_receipt_identity_and_sequence_are_not_numerical_authority():
    account = ledger()
    entered = receipt()
    with pytest.raises(ForeignTaskError, match="issued identity"):
        account.settle(replace(entered), issued_receipt=entered, starting_operation=operation())
    assert account.entries == account.depth == 0
    settle(account, entered, starting=operation())
    object.__setattr__(entered, "sequence", 10)
    with pytest.raises(ForeignTaskError, match="values changed"):
        settle(account, entered)
    next_event = receipt(sequence=2, invocation_started=False)
    assert settle(account, next_event)


def test_local_limits_are_pinned_independently_of_mutated_operation():
    account = ledger()
    descriptor = operation(instructions=1)
    settle(account, receipt(), starting=descriptor)
    object.__setattr__(descriptor, "max_instructions", 20)
    excess = receipt(sequence=2, invocation_started=False, state="returned",
                     instructions=2, cycles=2, invocation_instructions=2,
                     invocation_cycles=2, root_instructions=2, root_cycles=2)
    with pytest.raises(ForeignTaskError, match="invocation receipt counters"):
        settle(account, excess)
    assert account.instructions == 0 and account.depth == 1


def test_suffix_retirement_keeps_settled_child_receipt_and_parent_account():
    account = ledger()
    settle(account, receipt(), starting=operation())
    callback = receipt(sequence=2, invocation_started=False, state="callback",
                       instructions=1, cycles=1, callback_requests=1,
                       invocation_instructions=1, invocation_cycles=1, invocation_callbacks=1,
                       root_instructions=1, root_cycles=1, root_callbacks=1)
    settle(account, callback)
    child = receipt(sequence=3, invocation_id=2, parent_invocation_id=1, depth=2,
                    root_entries=2, root_instructions=1, root_cycles=1, root_callbacks=1)
    settle(account, child, starting=operation())
    with pytest.raises(ForeignTaskError, match="exact active suffix"):
        account.retire_suffix((1,))
    account.retire_suffix((2,))
    assert account.depth == 1 and account.entries == 2 and account.last_receipt is child
    assert not settle(account, child)
    completed = receipt(sequence=4, invocation_started=False, state="returned", root_entries=2,
                        instructions=1, cycles=1, invocation_instructions=2,
                        invocation_cycles=2, invocation_callbacks=1,
                        root_instructions=2, root_cycles=2, root_callbacks=1)
    settle(account, completed)
    assert account.depth == 0 and account.callbacks == 1 and account.instructions == 2


def test_failed_receipt_stays_owned_until_cancellation_without_extra_work():
    account = ledger()
    settle(account, receipt(), starting=operation())
    failed = receipt(sequence=2, invocation_started=False, state="failed",
                     instructions=1, cycles=3, invocation_instructions=1,
                     invocation_cycles=3, root_instructions=1, root_cycles=3)
    settle(account, failed)
    with pytest.raises(ForeignTaskError, match="must be canceled"):
        account.require_entry()
    account.retire_suffix((1,))
    assert account.depth == 0 and account.last_receipt is failed
    assert (account.instructions, account.cycles, account.entries) == (1, 3, 1)
