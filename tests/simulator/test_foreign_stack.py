"""Issued foreign return cells retire permanently without running an adapter."""

from __future__ import annotations

from dataclasses import FrozenInstanceError, replace
import gc
import weakref

import pytest

from shared.cells import MASK64
from simulator.foreign_control import (
    ForeignContinuation,
    ForeignControlError,
    ForeignRetirementReason,
    ForeignReturnControl,
)
from simulator.memory import SparseAddressSpace
from simulator.stacks import (
    Continuation,
    ReturnStack,
    ReturnStackShapeError,
    StackOverflow,
    StackPointerError,
)


def _stack(*, floor=0x100, empty=0x200):
    memory = SparseAddressSpace(bank0_size=0x400)
    stack = ReturnStack(memory=memory, floor=floor, empty_pointer=empty)
    issuer = object()
    control = stack.bind_foreign_control(issuer)
    return memory, stack, issuer, control


def _push(control, issuer, frame=1, **changes):
    fields = dict(root_id=1, frame_id=frame, request_id=1)
    fields.update(changes)
    return control.push(issuer, **fields)


def _reason(control, issuer):
    records = control.drain_retired(issuer)
    assert len(records) == 1
    return records[0].reason


def test_control_is_opt_in_exact_backed_stack_authority():
    ordinary = ReturnStack()
    assert not ordinary.has_foreign_state
    with pytest.raises(TypeError, match="exact backed"):
        ordinary.bind_foreign_control(object())
    memory = SparseAddressSpace(bank0_size=0x400)
    stack = ReturnStack(memory=memory, floor=0x100, empty_pointer=0x200)
    with pytest.raises(TypeError, match="non-None"):
        stack.bind_foreign_control(None)
    with pytest.raises(TypeError, match="issued by ReturnStack"):
        ForeignReturnControl(stack, object())
    issuer = object()
    control = stack.bind_foreign_control(issuer)
    assert stack.has_foreign_state and control.live_count == 0
    assert stack.bind_foreign_control(issuer) is control
    with pytest.raises(ForeignControlError, match="issuer"):
        stack.bind_foreign_control(object())


def test_opaque_issuer_never_runs_equality_hash_repr_or_call_hooks():
    class Issuer:
        def __eq__(self, other):
            raise AssertionError("issuer equality")

        def __hash__(self):
            raise AssertionError("issuer hash")

        def __repr__(self):
            raise AssertionError("issuer repr")

        def __bool__(self):
            raise AssertionError("issuer truth")

        def __call__(self):
            raise AssertionError("issuer callback")

    memory = SparseAddressSpace(bank0_size=0x400)
    stack = ReturnStack(memory=memory, floor=0x100, empty_pointer=0x200)
    issuer = Issuer()
    control = stack.bind_foreign_control(issuer)
    entry = _push(control, issuer)
    assert "ForeignReturnControl" in repr(control)
    assert "ForeignContinuation" in repr(entry)
    with pytest.raises(ForeignControlError, match="issuer"):
        control.consume(Issuer(), entry)
    stack.set_pointer(0x200)
    assert _reason(control, issuer) is ForeignRetirementReason.FRONTIER
    control.close(issuer)


@pytest.mark.parametrize("field,bad,error", [
    ("root_id", True, TypeError), ("frame_id", 1.0, TypeError),
    ("request_id", "1", TypeError), ("root_id", 0, ValueError),
    ("frame_id", -1, ValueError), ("request_id", MASK64 + 1, ValueError),
])
def test_bad_identifiers_reject_before_stack_or_cookie_mutation(field, bad, error):
    memory, stack, issuer, control = _stack()
    before = (stack.pointer, stack._continuation_cookie, memory.read_bytes(0x100, 0x100))
    with pytest.raises(error):
        _push(control, issuer, **{field: bad})
    assert (stack.pointer, stack._continuation_cookie, memory.read_bytes(0x100, 0x100)) == before
    assert control.live_count == control.pending_count == 0


def test_copy_and_constructed_entry_do_not_inherit_issued_authority():
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer, root_id=MASK64, request_id=MASK64)
    copied = replace(entry)
    assert entry is not copied and entry != copied
    assert not isinstance(entry, Continuation)
    with pytest.raises(FrozenInstanceError):
        entry.frame_id = 9
    pointer = stack.pointer
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, copied)
    assert stack.pointer == pointer and control.live_count == 1 and not entry.retired
    assert control.consume(issuer, entry) is entry
    assert _reason(control, issuer) is ForeignRetirementReason.RETURNED


@pytest.mark.parametrize("operation", [
    lambda stack: stack.pop(), lambda stack: stack.peek(),
    lambda stack: stack.pop_continuation(),
    lambda stack: stack.peek_pair(), lambda stack: stack.pop_pair(),
    lambda stack: stack.i(), lambda stack: stack.loop(),
])
def test_foreign_cell_is_control_state_for_user_and_ordinary_return_operations(operation):
    _, stack, issuer, control = _stack()
    stack.push(5)
    entry = _push(control, issuer)
    pointer = stack.pointer
    with pytest.raises(ReturnStackShapeError, match="foreign continuation"):
        operation(stack)
    assert stack.pointer == pointer and control.live_count == 1 and not entry.retired


def test_deeper_foreign_cell_is_not_a_loop_limit_or_pair_user_cell():
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    stack.push(2)
    for operation in (stack.peek_pair, stack.pop_pair, stack.i, stack.loop):
        with pytest.raises(ReturnStackShapeError, match="foreign continuation"):
            operation()
    assert not entry.retired and stack.snapshot() == (entry, 2)


def test_invalid_rp_restore_has_no_retirement_or_capture_effect():
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    captured = stack.capture_pointer()
    generation = stack.pointer_capture_checkpoint()
    for pointer in (0xFF, 0x101, 0x208):
        with pytest.raises(StackPointerError):
            stack.set_pointer(pointer)
    assert stack.pointer == captured and stack.pointer_capture_checkpoint() == generation
    assert control.live_count == 1 and control.drain_retired(issuer) == ()
    assert not entry.retired


@pytest.mark.parametrize("kind", ["int_subclass", "bool"])
def test_bound_rp_requires_exact_int_before_pointer_or_capture_effects(kind):
    class Pointer(int):
        pass

    memory, stack, issuer, control = _stack(floor=0)
    entry = _push(control, issuer)
    stack.capture_pointer()
    before = (stack.pointer, stack.pointer_capture_checkpoint(),
              memory.read_bytes(0, 0x200), control.live_count, control.pending_count)
    pointer = Pointer(0x200) if kind == "int_subclass" else False
    with pytest.raises(TypeError, match="exact integer"):
        stack.set_pointer(pointer)
    assert (stack.pointer, stack.pointer_capture_checkpoint(),
            memory.read_bytes(0, 0x200), control.live_count, control.pending_count) == before
    assert not entry.retired and control.drain_retired(issuer) == ()


def test_rp_discard_retires_only_the_discarded_suffix_deepest_first():
    _, stack, issuer, control = _stack()
    parent = _push(control, issuer, 1)
    child = _push(control, issuer, 2)
    grandchild = _push(control, issuer, 3)
    capture = stack.capture_pointer()
    generation = stack.pointer_capture_checkpoint()
    stack.set_pointer(parent.slot_address)
    assert control.live_count == 1 and control.pending_count == 2
    assert not parent.retired and child.retired and grandchild.retired
    records = control.drain_retired(issuer)
    assert tuple(record.entry for record in records) == (grandchild, child)
    assert tuple(record.frame_id for record in records) == (3, 2)
    assert tuple(record.generation for record in records) == (1, 2)
    assert all(record.reason is ForeignRetirementReason.FRONTIER for record in records)
    assert stack.pointer_capture_checkpoint() == generation
    # Restoring even the exact older RP exposes tombstones, never live returns.
    stack.set_pointer(capture)
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, grandchild)
    with pytest.raises(ReturnStackShapeError, match="foreign continuation"):
        stack.pop()
    assert control.live_count == 1


def test_pointer_motion_that_keeps_the_foreign_slot_live_preserves_it():
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    stack.push(7)
    stack.push_continuation(0x1234, 0)
    stack.set_pointer(entry.slot_address)
    assert not entry.retired and control.drain_retired(issuer) == ()
    assert control.consume(issuer, entry) is entry


@pytest.mark.parametrize("observe", ["reconcile", "snapshot", "peek"])
def test_raw_cookie_mismatch_is_permanent_even_after_metadata_erasure_and_byte_repair(observe):
    memory, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    memory.write64(entry.slot_address, 0xCAFE)
    if observe == "reconcile":
        control.reconcile(issuer)
    elif observe == "snapshot":
        assert stack.snapshot() == (0xCAFE,)
        assert entry.slot_address not in stack._continuations
    else:
        assert stack.peek() == 0xCAFE
        assert entry.slot_address not in stack._continuations
    assert _reason(control, issuer) is ForeignRetirementReason.COOKIE
    assert entry.retired
    memory.write64(entry.slot_address, entry.raw_cookie)
    stack.set_pointer(entry.slot_address)
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, entry)
    with pytest.raises(ReturnStackShapeError):
        stack.pop_continuation()


def test_identical_raw_write_retains_exact_live_metadata_authority():
    memory, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    memory.write64(entry.slot_address, entry.raw_cookie)
    control.reconcile(issuer)
    assert stack.snapshot() == (entry,) and not entry.retired
    assert control.drain_retired(issuer) == ()
    assert control.consume(issuer, entry) is entry


@pytest.mark.parametrize("replacement", ["copy", "remove"])
def test_metadata_identity_is_required_independently_of_raw_cookie(replacement):
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    if replacement == "copy":
        stack._continuations[entry.slot_address] = (replace(entry), entry.raw_cookie)
    else:
        del stack._continuations[entry.slot_address]
    control.reconcile(issuer)
    assert _reason(control, issuer) is ForeignRetirementReason.METADATA
    # Repairing the exact original typed metadata after retirement is too late.
    stack._continuations[entry.slot_address] = (entry, entry.raw_cookie)
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, entry)


def test_corrupted_entry_diagnostics_do_not_run_user_equality_or_corrupt_retirement_ids():
    class Untrusted:
        def __eq__(self, other):
            raise AssertionError("diagnostic equality")

        def __repr__(self):
            raise AssertionError("diagnostic repr")

    _, _, issuer, control = _stack()
    entry = _push(control, issuer, 7)
    object.__setattr__(entry, "frame_id", Untrusted())
    control.reconcile(issuer)
    record, = control.drain_retired(issuer)
    assert record.frame_id == 7 and record.reason is ForeignRetirementReason.METADATA
    assert "frame_id=7" in repr(record)


@pytest.mark.parametrize("field", ["_generation", "_live", "_pending", "_closed"])
def test_control_metadata_rejects_callback_capable_values_before_using_them(field):
    class Untrusted:
        def __getattr__(self, name):
            raise AssertionError("metadata attribute lookup")

        def __bool__(self):
            raise AssertionError("metadata truth")

        def __len__(self):
            raise AssertionError("metadata length")

        def __lt__(self, other):
            raise AssertionError("metadata ordering")

        def __eq__(self, other):
            raise AssertionError("metadata equality")

        def __add__(self, other):
            raise AssertionError("metadata addition")

    _, stack, issuer, control = _stack()
    _push(control, issuer)
    pointer = stack.pointer
    setattr(control, field, Untrusted())
    with pytest.raises(ForeignControlError):
        control.reconcile(issuer)
    assert stack.pointer == pointer


def test_live_and_pending_records_are_checked_before_field_comparison():
    class Untrusted:
        def __getattr__(self, name):
            raise AssertionError("record attribute lookup")

        def __eq__(self, other):
            raise AssertionError("record equality")

        def __lt__(self, other):
            raise AssertionError("record ordering")

    _, _, issuer, control = _stack()
    _push(control, issuer)
    record = control._live[0]
    control._live[0] = Untrusted()
    with pytest.raises(ForeignControlError, match="record identity"):
        control.reconcile(issuer)
    control._live[0] = record
    object.__setattr__(record, "frame_id", Untrusted())
    with pytest.raises(ForeignControlError, match="exact uint64"):
        control.reconcile(issuer)

    _, stack, issuer, control = _stack()
    _push(control, issuer)
    stack.clear()
    object.__setattr__(control._pending[0], "generation", Untrusted())
    with pytest.raises(ForeignControlError, match="pending record values"):
        control.drain_retired(issuer)


def test_malformed_foreign_raw_metadata_does_not_invoke_equality_from_snapshot_or_decode():
    class Untrusted:
        def __eq__(self, other):
            raise AssertionError("raw metadata equality")

    for observe in (lambda stack: stack.snapshot(), lambda stack: stack.peek()):
        _, stack, issuer, control = _stack()
        entry = _push(control, issuer)
        stack._continuations[entry.slot_address] = (entry, Untrusted())
        with pytest.raises(ForeignControlError, match="payload"):
            observe(stack)
        assert entry.retired and control.pending_count == 1


def test_consumed_cookie_retires_once_and_retains_nonresumable_backing():
    memory, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    raw = memory.read64(entry.slot_address)
    control.consume(issuer, entry)
    assert memory.read64(entry.slot_address) == raw and stack.pointer == 0x200
    record, = control.drain_retired(issuer)
    assert record.reason is ForeignRetirementReason.RETURNED and record.entry is entry
    stack.set_pointer(entry.slot_address)
    assert stack.snapshot() == (entry,)
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, entry)
    assert control.drain_retired(issuer) == ()


def test_live_and_pending_tables_are_bounded_and_pending_retirements_block_new_entry():
    _, stack, issuer, control = _stack()
    entries = [_push(control, issuer, frame) for frame in range(1, 9)]
    pointer = stack.pointer
    with pytest.raises(ForeignControlError, match="eight"):
        _push(control, issuer, 9)
    assert stack.pointer == pointer
    stack.set_pointer(0x200)
    assert control.live_count == 0 and control.pending_count == 8
    with pytest.raises(ForeignControlError, match="drain"):
        _push(control, issuer, 9)
    records = control.drain_retired(issuer)
    assert tuple(record.entry for record in records) == tuple(reversed(entries))
    replacement = _push(control, issuer, 9)
    assert not replacement.retired and control.live_count == 1
    assert all(entry.retired for entry in entries)


def test_distinct_pending_frames_share_one_root_and_cannot_be_returned_out_of_order():
    _, stack, issuer, control = _stack()
    parent = _push(control, issuer)
    for changes, message in ((dict(root_id=2, frame_id=2), "share their root"),
                             (dict(frame_id=1, request_id=2), "already owns")):
        with pytest.raises(ForeignControlError, match=message):
            _push(control, issuer, **changes)
    child = _push(control, issuer, 2)
    pointer = stack.pointer
    with pytest.raises(ForeignControlError, match="exact live top"):
        control.consume(issuer, parent)
    assert stack.pointer == pointer
    control.consume(issuer, child)
    control.drain_retired(issuer)
    control.consume(issuer, parent)


def test_foreign_snapshot_restore_rejects_before_any_pointer_or_authority_change():
    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    captured = stack.snapshot()
    pointer = stack.pointer
    with pytest.raises(TypeError, match="cannot install foreign"):
        stack.restore(captured)
    assert stack.pointer == pointer and not entry.retired and control.pending_count == 0
    stack.set_pointer(0x200)
    control.drain_retired(issuer)
    with pytest.raises(TypeError, match="cannot install foreign"):
        stack.restore(captured)
    assert stack.pointer == 0x200 and entry.retired


def test_ordinary_restore_validates_before_retirement_and_keeps_ordinary_cookie_rules():
    _, stack, issuer, control = _stack(floor=0x1E0)
    entry = _push(control, issuer)
    pointer = stack.pointer
    for snapshot, error in (((1, object()), TypeError), ((1,) * 5, StackOverflow)):
        with pytest.raises(error):
            stack.restore(snapshot)
        assert stack.pointer == pointer and not entry.retired and control.pending_count == 0
    ordinary = Continuation(0x1234, 9)
    stack.restore((7, ordinary))
    assert _reason(control, issuer) is ForeignRetirementReason.RESTORED
    restored_pointer = stack.pointer
    assert stack.pop_continuation() == ordinary
    stack.set_pointer(restored_pointer)
    assert stack.pop_continuation() == ordinary
    assert stack.pop() == 7


def test_forged_ordinary_continuation_restore_cannot_retire_foreign_state_or_run_coercion():
    class Untrusted:
        def __and__(self, other):
            raise AssertionError("ordinary snapshot coercion")

        def __bool__(self):
            raise AssertionError("ordinary snapshot truth")

    _, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    pointer = stack.pointer
    ordinary = Continuation(7, 9)
    object.__setattr__(ordinary, "xt", Untrusted())
    with pytest.raises(TypeError, match="exact uint64"):
        stack.restore((3, ordinary))
    assert stack.pointer == pointer and not entry.retired
    assert control.pending_count == 0 and control.live_count == 1


def test_bound_restore_rejects_unknown_entry_without_probing_its_class_property():
    class Untrusted:
        @property
        def __class__(self):
            raise AssertionError("snapshot class property")

    memory, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    stack.capture_pointer()
    before = (stack.pointer, stack.pointer_capture_checkpoint(),
              memory.read_bytes(0x100, 0x100), control.live_count, control.pending_count)
    with pytest.raises(TypeError, match="exact ordinary"):
        stack.restore((Untrusted(),))
    assert (stack.pointer, stack.pointer_capture_checkpoint(),
            memory.read_bytes(0x100, 0x100), control.live_count, control.pending_count) == before
    assert not entry.retired


def test_foreign_cookie_counter_is_checked_before_ordinary_cookie_arithmetic():
    class Untrusted:
        def __iadd__(self, other):
            raise AssertionError("cookie counter arithmetic")

    _, stack, issuer, control = _stack()
    stack._continuation_cookie = Untrusted()
    with pytest.raises(ForeignControlError, match="cookie counter"):
        _push(control, issuer)
    assert stack.pointer == 0x200 and control.live_count == control.pending_count == 0


def test_clear_records_retirement_without_erasing_capture_evidence_or_retained_cookie():
    memory, stack, issuer, control = _stack()
    entry = _push(control, issuer)
    stack.capture_pointer()
    checkpoint = stack.pointer_capture_checkpoint()
    stack.clear()
    assert stack.pointer == 0x200 and stack.pointer_capture_checkpoint() == checkpoint
    assert memory.read64(entry.slot_address) == entry.raw_cookie
    assert _reason(control, issuer) is ForeignRetirementReason.CLEARED


def test_close_detaches_authority_and_retained_tombstone_does_not_keep_owners_alive():
    class Issuer:
        pass

    memory = SparseAddressSpace(bank0_size=0x400)
    stack = ReturnStack(memory=memory, floor=0x100, empty_pointer=0x200)
    issuer = Issuer()
    control = stack.bind_foreign_control(issuer)
    entry = _push(control, issuer)
    issuer_ref, stack_ref, memory_ref = weakref.ref(issuer), weakref.ref(stack), weakref.ref(memory)
    records = control.close(issuer)
    assert records[0].entry is entry and records[0].reason is ForeignRetirementReason.CLOSED
    assert control.closed and not stack.has_foreign_state
    assert control.close(issuer) == ()
    with pytest.raises(ForeignControlError, match="closed"):
        control.reconcile(issuer)
    assert stack.snapshot() == (entry,)
    with pytest.raises(ReturnStackShapeError, match="foreign continuation"):
        stack.pop()
    del control, issuer, stack, memory, records
    gc.collect()
    assert issuer_ref() is None and stack_ref() is None and memory_ref() is None
    assert entry.retired


@pytest.mark.parametrize("profile", ("leaf", "closed"))
@pytest.mark.parametrize("executor", ("python", "native"))
def test_private_callback_rejects_foreign_return_authority_bound_during_tick(profile, executor, monkeypatch):
    from shared.hybrid_abi import CallbackExportV2, CallbackExportV3
    from simulator.interop_exports import CallbackExportError
    from simulator.runtime import MegaForthRuntime

    if executor == "native":
        pytest.importorskip("_megaforth_native")
    runtime = MegaForthRuntime(execution_backend=executor)
    if profile == "leaf":
        descriptor = CallbackExportV2(export_id=0, name="ABS", input_cells=1, output_cells=1)
        arguments = (MASK64,)
    else:
        runtime.evaluate(b": CLAMP ROT MIN MAX ;")
        descriptor = CallbackExportV3(export_id=0, name="CLAMP", input_cells=3, output_cells=1,
                                      max_semantic_steps=7, effect="closed_integer_colon")
        arguments = (9, 2, 7)
    handle = runtime.bind_callback_export(descriptor)
    captured = []
    account = runtime._account_semantic_step

    def bind_foreign():
        account()
        context = runtime._callback_exports._active_context
        if context is not None:
            captured.append(context)
            context.returns.bind_foreign_control(object())

    monkeypatch.setattr(runtime, "_account_semantic_step", bind_foreign)
    before = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, arguments)
    assert runtime.diagnostics.semantic_cycles - before == 1
    assert len(captured) == 1 and captured[0].returns.has_foreign_state
    assert captured[0].data.snapshot() == arguments
