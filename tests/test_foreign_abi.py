"""Task transport values are bounded descriptions, never execution authority."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
from pathlib import Path
import subprocess
import sys

import pytest

from shared.cells import MASK64
from shared.foreign_abi import (
    FOREIGN_ABI,
    FOREIGN_ABI_VERSION,
    ForeignAccessV1,
    ForeignAdapterV1,
    ForeignBudgetV1,
    ForeignCallbackRequestV1,
    ForeignCancellationV1,
    ForeignCompletedV1,
    ForeignExportV1,
    ForeignFailedV1,
    ForeignFailureKindV1,
    ForeignOperationV1,
    ForeignReceiptV1,
    ForeignRunnableYieldV1,
    ForeignSignatureV1,
    ForeignSpanV1,
    ForeignStateV1,
)


def _signature(**changes):
    return ForeignSignatureV1(**(dict(input_cells=2, output_cells=1) | changes))


def _span(**changes):
    return ForeignSpanV1(**(dict(base=0x1000, size=32, access="read_write") | changes))


def _operation(**changes):
    return ForeignOperationV1(**(dict(registration=object(), signature=_signature()) | changes))


def _export(**changes):
    return ForeignExportV1(**(dict(export=object(), signature=_signature()) | changes))


def _budget(**changes):
    return ForeignBudgetV1(**(dict(
        invocation_instructions_remaining=100, root_instructions_remaining=1000,
        invocation_callbacks_remaining=8, root_callbacks_remaining=16,
        quantum_instructions=10,
    ) | changes))


def _receipt(**changes):
    return ForeignReceiptV1(**(dict(
        root_id=1, invocation_id=1, parent_invocation_id=None, depth=1, sequence=1,
        invocation_started=True, root_entries=1, state="yielded",
        instructions=0, cycles=0, callback_requests=0,
        invocation_instructions=0, invocation_cycles=0, invocation_callbacks=0,
        root_instructions=0, root_cycles=0, root_callbacks=0,
    ) | changes))


def _callback_receipt(**changes):
    return _receipt(**(dict(
        state="callback", instructions=3, cycles=7, callback_requests=1,
        invocation_instructions=3, invocation_cycles=7, invocation_callbacks=1,
        root_instructions=3, root_cycles=7, root_callbacks=1,
    ) | changes))


def _callback(**changes):
    return ForeignCallbackRequestV1(**(dict(
        operation_token=object(), request_token=object(), receipt=_callback_receipt(),
        request_sequence=1, site=0, export=_export(), arguments=(7, MASK64),
    ) | changes))


def _completed(**changes):
    receipt = _receipt(state="returned", instructions=1, cycles=2,
                       invocation_instructions=1, invocation_cycles=2,
                       root_instructions=1, root_cycles=2)
    return ForeignCompletedV1(**(dict(operation_token=object(), receipt=receipt,
                                     outputs=(7,)) | changes))


def _yielded(**changes):
    return ForeignRunnableYieldV1(**(dict(operation_token=object(), receipt=_receipt()) | changes))


def _failed(**changes):
    return ForeignFailedV1(**(dict(operation_token=object(), receipt=_receipt(state="failed"),
                                  kind="instruction_limit") | changes))


def _cancellation(**changes):
    return ForeignCancellationV1(**(dict(retired_invocation_ids=()) | changes))


@pytest.mark.parametrize("factory", [
    _signature, _span, _operation, _export, _budget, _receipt,
    _callback, _completed, _yielded, _failed, _cancellation,
])
def test_values_are_frozen_slotted_and_use_a_separate_versioned_family(factory):
    value = factory()
    assert (value.abi, value.version) == (FOREIGN_ABI, FOREIGN_ABI_VERSION)
    assert not hasattr(value, "__dict__")
    with pytest.raises(FrozenInstanceError):
        value.version = 2
    with pytest.raises(ValueError, match="ABI identity"):
        factory(abi="megapad.hybrid.integer-routine")
    with pytest.raises(ValueError, match="ABI version"):
        factory(version=2)
    with pytest.raises(TypeError, match="exact integer"):
        factory(version=True)


@pytest.mark.parametrize("bad", [True, 1.0, "1"])
def test_cells_counts_and_addresses_do_not_coerce_host_values(bad):
    for factory, field in ((_signature, "input_cells"), (_span, "base"),
                           (_budget, "root_instructions_remaining"),
                           (_receipt, "root_id"), (_receipt, "cycles")):
        with pytest.raises(TypeError, match="exact integer"):
            factory(**{field: bad})
    with pytest.raises(TypeError, match="exact integer"):
        _callback(arguments=(bad, 0))


def test_signatures_and_spans_cover_uint64_edges_without_address_wrap():
    assert _signature(input_cells=0, output_cells=8).output_cells == 8
    assert _span(base=MASK64, size=0).limit == MASK64
    assert _span(base=MASK64, size=1).limit == MASK64 + 1
    assert _span(access="read").access is ForeignAccessV1.READ
    for changes in (dict(base=MASK64, size=2), dict(base=-1), dict(size=MASK64 + 1)):
        with pytest.raises(ValueError):
            _span(**changes)
    for changes in (dict(input_cells=-1), dict(output_cells=9)):
        with pytest.raises(ValueError):
            _signature(**changes)
    with pytest.raises(ValueError):
        _span(access="execute")


def test_machine_and_task_grants_are_separate_bounded_numerical_descriptions():
    span = _span()
    operation = _operation(machine_grants=(span,) * 16)
    export = _export(task_grants=(span,) * 16)
    assert operation.machine_grants[0] is span and export.task_grants[0] is span
    assert not hasattr(operation, "task_grants") and not hasattr(export, "machine_grants")
    with pytest.raises(ValueError, match="at most 16"):
        _operation(machine_grants=(span,) * 17)
    with pytest.raises(ValueError, match="at most 16"):
        _export(task_grants=(span,) * 17)
    with pytest.raises(ValueError):
        _operation(max_instructions=1_000_001)
    with pytest.raises(ValueError):
        _export(max_semantic_steps=4097)
    assert _operation(max_callbacks=0).max_callbacks == 0


def test_containers_reject_iterable_hooks_and_nested_subclasses_without_running_them():
    class HostileList(list):
        def __iter__(self):
            raise AssertionError("untrusted iteration")

    class SpanSubclass(ForeignSpanV1):
        pass

    for factory, key in ((_operation, "machine_grants"), (_export, "task_grants"),
                         (_callback, "arguments"), (_completed, "outputs"),
                         (_cancellation, "retired_invocation_ids")):
        with pytest.raises(TypeError, match="exact tuple"):
            factory(**{key: HostileList()})
    with pytest.raises(TypeError, match="exact ForeignSpanV1"):
        _operation(machine_grants=(SpanSubclass(base=0, size=1, access="read"),))


def test_nested_frozen_values_are_revalidated_before_their_fields_are_used():
    signature = _signature()
    object.__setattr__(signature, "input_cells", True)
    with pytest.raises(TypeError, match="exact integer"):
        _operation(signature=signature)
    span = _span()
    object.__setattr__(span, "size", -1)
    with pytest.raises(ValueError, match="span size"):
        _export(task_grants=(span,))
    receipt = _receipt()
    object.__setattr__(receipt, "root_cycles", True)
    with pytest.raises(TypeError, match="exact integer"):
        _yielded(receipt=receipt)


def test_opaque_references_are_not_inspected_compared_formatted_or_coerced():
    class Opaque:
        def __bool__(self):
            raise AssertionError("opaque bool")

        def __eq__(self, other):
            raise AssertionError("opaque equality")

        def __hash__(self):
            raise AssertionError("opaque hash")

        def __repr__(self):
            raise AssertionError("opaque repr")

    opaque = Opaque()
    operation = _operation(registration=opaque)
    export = _export(export=opaque)
    callback = _callback(operation_token=opaque, request_token=opaque, export=export)
    copy = replace(callback)
    assert callback is not copy and callback != copy
    assert callback.request_token is opaque and operation.registration is opaque
    assert "ForeignCallbackRequestV1" in repr(callback)
    assert "ForeignOperationV1" in repr(operation)
    assert not hasattr(callback, "resume") and not hasattr(operation, "execute")
    for factory, key in ((_operation, "registration"), (_export, "export"),
                         (_callback, "request_token"), (_yielded, "operation_token")):
        with pytest.raises(TypeError, match="non-None opaque"):
            factory(**{key: None})


def test_zero_work_preinstruction_yield_is_not_a_terminal_limit_failure():
    budget = _budget(quantum_instructions=0)
    yielded = _yielded()
    assert budget.quantum_instructions == 0
    assert yielded.receipt.invocation_started and yielded.receipt.sequence == 1
    assert (yielded.receipt.instructions, yielded.receipt.cycles) == (0, 0)
    assert not yielded.receipt.terminal
    failed = _failed()
    assert failed.receipt.terminal
    assert failed.kind is ForeignFailureKindV1.INSTRUCTION_LIMIT
    with pytest.raises(ValueError, match="state differ"):
        _yielded(receipt=failed.receipt)
    with pytest.raises(ValueError, match="completed RET"):
        _receipt(state="returned")
    with pytest.raises(ValueError, match="completed CALL"):
        _receipt(state="callback")


def test_remaining_local_and_root_budgets_do_not_collapse_into_one_counter():
    budget = _budget(invocation_instructions_remaining=30, root_instructions_remaining=2,
                     invocation_callbacks_remaining=0, root_callbacks_remaining=4)
    assert budget.invocation_instructions_remaining == 30
    assert budget.root_instructions_remaining == 2
    assert budget.invocation_callbacks_remaining == 0
    assert budget.root_callbacks_remaining == 4
    for name, limit in (("invocation_instructions_remaining", 1_000_000),
                        ("root_instructions_remaining", 10_000_000),
                        ("invocation_callbacks_remaining", 1024),
                        ("root_callbacks_remaining", 1024),
                        ("quantum_instructions", 1_000_000)):
        with pytest.raises(ValueError):
            _budget(**{name: limit + 1})
        with pytest.raises(ValueError):
            _budget(**{name: -1})


def test_nested_receipts_keep_parent_depth_and_root_work_separate():
    child = _receipt(root_id=MASK64, invocation_id=2, parent_invocation_id=1,
                     depth=2, sequence=3, root_entries=2, instructions=2, cycles=5,
                     invocation_instructions=2, invocation_cycles=5,
                     root_instructions=5, root_cycles=12, root_callbacks=1)
    assert child.invocation_started and child.root_entries == 2
    parent = _receipt(root_id=MASK64, sequence=4, invocation_started=False, root_entries=2,
                      instructions=1, cycles=2, invocation_instructions=4,
                      invocation_cycles=9, invocation_callbacks=1,
                      root_instructions=6, root_cycles=14, root_callbacks=1)
    assert parent.invocation_id == 1 and parent.invocation_instructions == 4
    assert parent.root_instructions == 6


@pytest.mark.parametrize("changes,message", [
    (dict(sequence=0), "sequence"),
    (dict(root_id=MASK64 + 1), "root_id"),
    (dict(depth=9), "depth"),
    (dict(depth=2), "depth one"),
    (dict(parent_invocation_id=2), "parent invocation"),
    (dict(parent_invocation_id=1, depth=2), "parent invocation"),
    (dict(invocation_started=1), "exact boolean"),
    (dict(root_entries=1025), "root entries"),
    (dict(root_entries=2), "root segment sequence"),
    (dict(instructions=1), "counters are inconsistent"),
    (dict(invocation_instructions=1, root_instructions=1), "prior own work"),
    (dict(cycles=1, invocation_cycles=1, root_cycles=1), "cycle counters"),
    (dict(invocation_started=False), "first root receipt"),
    (dict(callback_requests=2), "callback_requests"),
])
def test_receipts_reject_incoherent_work_and_ancestry(changes, message):
    with pytest.raises((TypeError, ValueError), match=message):
        _receipt(**changes)


def test_callbacks_bind_signature_site_and_request_sequence_to_completed_call_receipt():
    callback = _callback(site=15)
    assert callback.receipt.state is ForeignStateV1.CALLBACK
    assert not callback.receipt.terminal and callback.arguments == (7, MASK64)
    for changes, message in ((dict(site=16), "callback site"),
                             (dict(arguments=(7,)), "arity"),
                             (dict(request_sequence=2), "callback count"),
                             (dict(arguments=(0, MASK64 + 1)), "cell")):
        with pytest.raises(ValueError, match=message):
            _callback(**changes)
    with pytest.raises(TypeError, match="exact tuple"):
        _completed(outputs=[7])
    with pytest.raises(ValueError, match="at most 8"):
        _completed(outputs=(0,) * 9)


def test_failure_data_is_bounded_and_cannot_translate_a_host_exception():
    failure = _failed(detail="prefix retained", instruction_pc=MASK64)
    assert failure.receipt.terminal and not hasattr(failure, "outputs")
    with pytest.raises(TypeError, match="failure kind"):
        _failed(kind=RuntimeError("host failure"))
    with pytest.raises(ValueError):
        _failed(kind="yielded")
    with pytest.raises(ValueError, match="1024"):
        _failed(detail="x" * 1025)
    with pytest.raises(TypeError, match="exact string"):
        _failed(detail=object())


def test_suffix_cancellation_preserves_parent_token_and_latest_child_receipt_without_work():
    parent_token = object()
    receipt = _receipt(invocation_id=3, parent_invocation_id=2, depth=3,
                       sequence=4, root_entries=3, root_instructions=2,
                       root_cycles=4, root_callbacks=2)
    cancellation = _cancellation(retired_invocation_ids=(3, 2), surviving_parent_id=1,
                                 surviving_parent_token=parent_token, receipt=receipt)
    assert cancellation.retired_invocation_ids == (3, 2)
    assert cancellation.surviving_parent_token is parent_token
    assert cancellation.receipt is receipt and cancellation.receipt.sequence == 4
    assert not hasattr(cancellation, "instructions")
    inactive = _cancellation(receipt=receipt)
    assert inactive.retired_invocation_ids == () and inactive.receipt is receipt


@pytest.mark.parametrize("changes,error,message", [
    (dict(retired_invocation_ids=(1,)), ValueError, "retained receipt"),
    (dict(retired_invocation_ids=(1, 1)), ValueError, "distinct"),
    (dict(retired_invocation_ids=(True,)), TypeError, "exact integer"),
    (dict(retired_invocation_ids=tuple(range(1, 10))), ValueError, "at most 8"),
    (dict(surviving_parent_token=object()), ValueError, "appear together"),
    (dict(surviving_parent_id=1), TypeError, "non-None opaque"),
    (dict(surviving_parent_id=1, surviving_parent_token=object()), ValueError, "nonempty"),
])
def test_cancellation_cannot_describe_an_unbounded_or_incoherent_suffix(changes, error, message):
    with pytest.raises(error, match=message):
        _cancellation(**changes)


def test_retired_parent_cannot_simultaneously_survive():
    with pytest.raises(ValueError, match="outside"):
        _cancellation(retired_invocation_ids=(1,), surviving_parent_id=1,
                      surviving_parent_token=object(), receipt=_receipt())


def test_module_stays_backend_neutral_and_has_no_registration_or_cookie_implementation():
    root = Path(__file__).resolve().parents[1]
    code = """
import sys
sys.path.insert(0, sys.argv[1])
import shared.foreign_abi as values
assert not any(name == 'hybrid' or name.startswith('hybrid.') for name in sys.modules)
assert not any(name == 'simulator' or name.startswith('simulator.') for name in sys.modules)
assert '_mp64_accel' not in sys.modules and '_megaforth_native' not in sys.modules
assert not hasattr(values, 'ForeignDefinition')
assert not hasattr(values, 'ForeignContinuation')
assert not hasattr(values, 'ForeignRegistration')
"""
    subprocess.run([sys.executable, "-I", "-B", "-c", code, str(root)],
                   check=True, capture_output=True, text=True)
    assert all(hasattr(ForeignAdapterV1, name) for name in (
        "begin", "advance", "reply", "cancel_suffix", "cancel_all", "last_receipt",
    ))
