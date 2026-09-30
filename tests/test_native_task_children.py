"""Task children keep parent authority across real quanta and suffix retirement."""
from __future__ import annotations

import copy

import _mp64_accel as native
import pytest

from tests.test_native_hybrid_nested_child import (
    CALL, CHILD_BUFFER, CONTROL_BASE, EXT_BASE, MASK64, PARENT_BUFFER, STACK_SIZE,
    OrdinaryReference, Owner as PrivateOwner, _view,
)
from tests.test_native_task_routine import SPEC_FIELDS, budget, receipt


class Owner(PrivateOwner):
    def __init__(self):
        super().__init__()
        self.private = self.runner
        self.runner = self.private.task_v1()
        self.root = None

    def spec(self, *args, stack_base=None, **kwargs):
        temporary = super().spec(*args, **kwargs)
        fields = {name: getattr(temporary, name) for name in SPEC_FIELDS}
        if stack_base is not None:
            fields["stack_base"] = stack_base
        spec = native.TaskRoutineSpecV1(**fields)
        self.labels[id(spec)] = self.labels.pop(id(temporary))
        return spec

    def publish(self, *rows):
        for spec, _children in rows:
            self.runner.prepare_code(spec)
        return self.runner.seal_publications(tuple(rows))

    def bind(self, root_id=1, *, instructions=1000, callbacks=1024, entries=1024):
        self.root = self.runner.bind_root(root_id, instructions, callbacks, entry_limit=entries)
        return self.root

    def admit(self, spec, arguments=(7,), spans=(), *, parent=None, edge=None, allowance=None, protected=()):
        return self.runner.begin(spec, arguments, spans, root_token=self.root,
            budget=budget() if allowance is None else allowance,
            parent_token=None if parent is None else parent.request_token,
            child_edge=edge, protected_spans=protected)

    def park(self, spec, arguments=(7,), spans=(), *, parent=None, edge=None):
        entered = self.admit(spec, arguments, spans, parent=parent, edge=edge)
        callback = self.runner.advance(entered.operation_token, budget=budget(100))
        assert callback.receipt.state == "callback"
        return callback

    def pair(self, source="inc r4\nret.l", *, callbacks=(), instructions=100, child_stack=None):
        parent = self.spec(0)
        child = self.spec(1, source, callbacks=callbacks, callback_limit=int(bool(callbacks)),
                          instructions=instructions, stack_base=child_stack)
        (edge,), _ = self.publish((parent, ((0, child),)), (child, ()))
        self.bind()
        return parent, child, edge


def assert_latest(owner, event):
    assert receipt(owner.runner.last_receipt()) == receipt(event.receipt)
    assert (event.instructions, event.cycles) == (event.receipt.instructions, event.receipt.cycles)


def test_nested_every_instruction_matches_ordinary_execution_and_restoration_cold_and_warm():
    owner = Owner()
    parent = owner.spec(0, PARENT_BUFFER, inputs=3, callbacks=(("call", "stub", 7, 2, 1),))
    child = owner.spec(1, CHILD_BUFFER, inputs=2, callbacks=(), callback_limit=0)
    (edge,), _ = owner.publish((parent, ((0, child),)), (child, ()))
    reference = OrdinaryReference(owner)
    for root_id in (1, 2):
        owner.bind(root_id)
        owner.external[:8] = reference.memory[:8] = (41).to_bytes(8, "little")
        before = owner.snapshot()
        current = owner.admit(parent, (EXT_BASE, 7, 3), ((EXT_BASE, 16, "read_write"),))
        assert owner.snapshot() == before and current.receipt.invocation_started
        reference.enter(parent, (EXT_BASE, 7, 3))
        for index in range(9):
            prior_cycles = reference.cycles
            reference.step()
            current = owner.runner.advance(current.operation_token, budget=budget(1))
            assert (current.instructions, current.cycles) == (1, reference.cycles - prior_cycles)
            assert current.receipt.state == ("callback" if index == 8 else "yielded")
            assert_latest(owner, current)
            reference.assert_equal()
        request = current
        parent_view, parent_cells = _view(owner.state), bytes(owner.control[:STACK_SIZE])
        parent_reference = _view(reference.state)
        before = owner.snapshot()
        current = owner.admit(child, (EXT_BASE, 999), ((EXT_BASE, 8, "read_write"),),
                              parent=request, edge=edge)
        assert current.receipt.invocation_started and current.receipt.depth == 2
        assert current.receipt.parent_invocation_id == request.receipt.invocation_id
        assert current.receipt.root_entries == 2 and current.receipt.root_instructions == 9
        assert owner.snapshot() == before
        current = owner.runner.advance(current.operation_token, budget=budget(0))
        assert current.receipt.state == "yielded" and owner.snapshot() == before
        reference.enter(child, (EXT_BASE, 999))
        for index in range(9):
            prior_cycles = reference.cycles
            reference.step()
            current = owner.runner.advance(current.operation_token, budget=budget(1))
            assert (current.instructions, current.cycles) == (1, reference.cycles - prior_cycles)
            assert current.receipt.state == ("returned" if index == 8 else "yielded")
            if index == 8:
                reference.restore(parent_reference)
            assert_latest(owner, current)
            reference.assert_equal()
        assert current.outputs == (42,) and current.pc == MASK64
        assert current.receipt.invocation_instructions == 9 and current.receipt.root_instructions == 18
        assert _view(owner.state) == parent_view and bytes(owner.control[:STACK_SIZE]) == parent_cells
        before = owner.snapshot()
        current = owner.runner.reply(request.request_token, (42,), budget=budget(0))
        assert current.receipt.state == "yielded" and current.receipt.depth == 1
        assert owner.snapshot() == before
        reference.reply((42,))
        for index in range(4):
            prior_cycles = reference.cycles
            reference.step()
            current = owner.runner.advance(current.operation_token, budget=budget(1))
            assert (current.instructions, current.cycles) == (1, reference.cycles - prior_cycles)
            assert current.receipt.state == ("returned" if index == 3 else "yielded")
            reference.assert_equal()
        assert current.outputs == (84,) and current.receipt.invocation_instructions == 13
        assert current.receipt.root_instructions == 22 and current.receipt.root_callbacks == 1
        assert_latest(owner, current)
    owner.runner.close()


@pytest.mark.parametrize("phase", ["uninitialized", "yielded", "callback", "failed"])
def test_suffix_cancel_restores_parent_without_ret_or_rewinding_child_prefix(phase):
    owner = Owner()
    source = "ldi r6,90\nst.b r4,r6\n" + CALL
    callbacks = (("call", "stub", 1, 1, 1),)
    if phase == "failed":
        source, callbacks = "ldi r6,90\nst.b r4,r6\naddi r4,8\nstr r4,r6\nret.l", ()
    parent, child, edge = owner.pair(source, callbacks=callbacks)
    request = owner.park(parent, (EXT_BASE,), ((EXT_BASE, 16, "read_write"),))
    saved = _view(owner.state)
    child_event = owner.admit(child, (EXT_BASE,), ((EXT_BASE, 1, "write"),), parent=request, edge=edge)
    if phase != "uninitialized":
        child_event = owner.runner.advance(child_event.operation_token,
            budget=budget(2 if phase == "yielded" else 100))
    assert child_event.receipt.state == ("yielded" if phase == "uninitialized" else phase)
    retained = receipt(owner.runner.last_receipt())
    before_bytes = bytes(owner.ram), bytes(owner.external), bytes(owner.control)
    before_cycles = owner.state.cycle_count
    cancelled = owner.runner.cancel_suffix(child_event.operation_token)
    assert cancelled.retired_invocation_ids == (child_event.receipt.invocation_id,)
    assert cancelled.surviving_parent_id == request.receipt.invocation_id
    assert cancelled.surviving_parent_token is request.request_token
    assert receipt(cancelled.receipt) == retained == receipt(owner.runner.last_receipt())
    assert _view(owner.state) == saved and owner.state.cycle_count == before_cycles
    assert (bytes(owner.ram), bytes(owner.external), bytes(owner.control)) == before_bytes
    assert owner.external[0] == (0 if phase == "uninitialized" else 90)
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.advance(child_event.operation_token, budget=budget(1))
    if child_event.request_token is not None:
        with pytest.raises((ValueError, RuntimeError)):
            owner.runner.reply(child_event.request_token, (99,), budget=budget(1))
    final = owner.runner.reply(request.request_token, (33,), budget=budget(100))
    assert final.outputs == (33,) and final.receipt.invocation_instructions == 4
    assert final.receipt.root_instructions == child_event.receipt.root_instructions + 2
    owner.runner.close()


def test_cyclic_publications_preserve_issued_edges_but_active_recursion_is_rejected():
    owner = Owner()
    first, second = owner.spec(0), owner.spec(1)
    rows = ((first, ((0, second), (0, first))), (second, ((0, first),)))
    edges = owner.publish(*rows)
    again = owner.runner.seal_publications(rows)
    assert again[0][0] is edges[0][0] and again[0][1] is edges[0][1]
    assert type(edges[0][0]) is native.TaskChildEdgeV1
    with pytest.raises(TypeError):
        native.TaskChildEdgeV1()
    with pytest.raises(TypeError):
        copy.copy(edges[0][0])
    owner.bind()
    parent = owner.park(first)
    before, retained = owner.snapshot(), receipt(owner.runner.last_receipt())
    with pytest.raises(ValueError):
        owner.admit(first, parent=parent, edge=edges[0][1])
    assert owner.snapshot() == before and receipt(owner.runner.last_receipt()) == retained
    child = owner.park(second, parent=parent, edge=edges[0][0])
    before, retained = owner.snapshot(), receipt(owner.runner.last_receipt())
    with pytest.raises(ValueError):
        owner.admit(first, parent=child, edge=edges[1][0])
    assert owner.snapshot() == before and receipt(owner.runner.last_receipt()) == retained
    assert owner.runner.reply(child.request_token, (9,), budget=budget(100)).outputs == (9,)
    assert owner.runner.reply(parent.request_token, (11,), budget=budget(100)).outputs == (11,)
    owner.runner.close()


def test_reachable_stale_generation_is_rejected_before_entry_even_after_same_spec_reprepare():
    owner = Owner()
    first, second, third = [owner.spec(index) for index in range(3)]
    rows = ((first, ((0, second),)), (second, ((0, third),)), (third, ((0, first),)))
    edges = owner.publish(*rows)
    owner.runner.revoke_code(third)
    owner.runner.prepare_code(third)
    owner.runner.seal_publications(((third, ((0, first),)),))
    owner.bind()
    before = owner.snapshot()
    with pytest.raises(ValueError, match="generation"):
        owner.admit(first)
    assert owner.snapshot() == before and owner.runner.last_receipt() is None
    with pytest.raises(ValueError):
        owner.runner.seal_publications(((second, ((0, third),)),))
    assert owner.runner.is_code_published(first) and owner.runner.is_code_published(second)
    assert type(edges[1][0]) is native.TaskChildEdgeV1
    owner.runner.close()


@pytest.mark.parametrize("malformed", ["duplicate", "site", "bool_site", "list_row", "foreign", "outside_batch"])
def test_late_child_row_rejection_leaves_entire_prepared_batch_and_cache_unchanged(malformed):
    owner, other = Owner(), Owner()
    first, second, absent = [owner.spec(index) for index in range(3)]
    foreign = other.spec(0)
    for spec in (first, second, absent):
        owner.runner.prepare_code(spec)
    bad = {"duplicate": ((0, second), (0, second)), "site": ((1, second),),
           "bool_site": ((True, second),), "list_row": ([0, second],),
           "foreign": ((0, foreign),), "outside_batch": ((0, absent),)}[malformed]
    before = owner.snapshot()
    with pytest.raises((TypeError, ValueError)):
        owner.runner.seal_publications(((first, ((0, second),)), (second, bad)))
    assert owner.snapshot() == before
    assert not owner.runner.is_code_published(first) and not owner.runner.is_code_published(second)
    assert owner.runner.seal_publications(((first, ((0, second),)), (second, ((0, first),))))
    owner.runner.close()
    other.runner.close()


@pytest.mark.parametrize("failure", ["overlap", "write_escalation", "two_grants", "protected", "wrong_edge",
                                      "wrong_root", "wrong_spec", "positive_quantum"])
def test_child_preflight_preserves_parent_authority_receipt_and_all_machine_state(failure):
    owner, other = Owner(), Owner()
    parent, child, edge = owner.pair(child_stack=CONTROL_BASE if failure == "overlap" else None)
    sibling = owner.spec(2, "ret.l", callbacks=(), callback_limit=0)
    owner.runner.prepare_code(sibling)
    owner.runner.seal_publications(((sibling, ()),))
    other_parent, other_child, other_edge = other.pair()
    parent_grants = ((EXT_BASE, 8, "read"), (EXT_BASE + 8, 8, "read"))
    request = owner.park(parent, spans=parent_grants)
    spans, kwargs, chosen = ((EXT_BASE, 8, "read"),), {}, child
    if failure == "write_escalation":
        spans = ((EXT_BASE, 8, "write"),)
    elif failure == "two_grants":
        spans = ((EXT_BASE, 16, "read"),)
    elif failure == "protected":
        kwargs["protected_spans"] = ((EXT_BASE, 8),)
    elif failure == "wrong_edge":
        edge = other_edge
    elif failure == "wrong_root":
        kwargs["root_token"] = other.root
    elif failure == "wrong_spec":
        chosen = sibling
    allowance = budget(1 if failure == "positive_quantum" else 0)
    before, retained = owner.snapshot(), receipt(owner.runner.last_receipt())
    with pytest.raises((ValueError, RuntimeError)):
        owner.runner.begin(chosen, (7,), spans, root_token=kwargs.pop("root_token", owner.root),
            budget=allowance, parent_token=request.request_token, child_edge=edge, **kwargs)
    assert owner.snapshot() == before and receipt(owner.runner.last_receipt()) == retained
    assert owner.runner.reply(request.request_token, (9,), budget=budget(100)).outputs == (9,)
    owner.runner.close()
    other.runner.close()


def test_eight_live_frames_and_suffix_retirement_keep_original_surviving_request():
    owner = Owner()
    specs = [owner.spec(index) for index in range(9)]
    rows = tuple((spec, ((0, specs[index + 1]),) if index < 8 else ())
                 for index, spec in enumerate(specs))
    edges = owner.publish(*rows)
    owner.bind()
    callbacks = [owner.park(specs[0])]
    for index in range(1, 8):
        event = owner.park(specs[index], parent=callbacks[-1], edge=edges[index - 1][0])
        assert event.receipt.depth == index + 1 and event.receipt.root_entries == index + 1
        callbacks.append(event)
    before, retained = owner.snapshot(), receipt(owner.runner.last_receipt())
    with pytest.raises(ValueError):
        owner.admit(specs[8], parent=callbacks[-1], edge=edges[7][0])
    assert owner.snapshot() == before and receipt(owner.runner.last_receipt()) == retained
    cancelled = owner.runner.cancel_suffix(callbacks[2].operation_token)
    assert cancelled.retired_invocation_ids == tuple(event.receipt.invocation_id for event in reversed(callbacks[2:]))
    assert cancelled.surviving_parent_token is callbacks[1].request_token
    assert cancelled.surviving_parent_id == callbacks[1].receipt.invocation_id
    assert receipt(cancelled.receipt) == retained
    second = owner.runner.reply(callbacks[1].request_token, (22,), budget=budget(100))
    assert second.outputs == (22,) and second.receipt.depth == 2 and second.receipt.root_entries == 8
    first = owner.runner.reply(callbacks[0].request_token, (33,), budget=budget(100))
    assert first.outputs == (33,) and first.receipt.root_instructions == 20
    owner.runner.close()


def test_child_work_and_cancel_do_not_replenish_parent_or_original_root_allowances():
    owner = Owner()
    parent, child, edge = owner.pair()
    owner.bind(2, instructions=3, entries=2)
    request = owner.park(parent)
    entered = owner.admit(child, parent=request, edge=edge)
    failed = owner.runner.advance(entered.operation_token, budget=budget(100))
    assert failed.receipt.state == "failed" and failed.receipt.invocation_instructions == 1
    assert failed.receipt.root_instructions == 3
    owner.runner.cancel_suffix(failed.operation_token)
    before = owner.snapshot()
    failed_parent = owner.runner.reply(request.request_token, (99,), budget=budget(0))
    assert failed_parent.receipt.state == "failed" and failed_parent.instructions == 0
    assert failed_parent.receipt.invocation_instructions == 2 and failed_parent.receipt.root_entries == 2
    assert owner.snapshot() == before
    owner.runner.cancel_all()
    with pytest.raises(ValueError):
        owner.admit(parent)
    owner.runner.close()


@pytest.mark.parametrize("via_reply", [False, True])
def test_real_child_return_delivery_failure_keeps_retired_child_and_restored_parent(via_reply):
    owner = Owner()
    parent, child, edge = owner.pair(CALL if via_reply else "inc r4\nret.l",
        callbacks=(("call", "stub", 1, 1, 1),) if via_reply else ())
    owner.runner._test_fail_marshalling_after_results(5 if via_reply else 4)
    request = owner.park(parent)
    saved = _view(owner.state)
    entered = owner.admit(child, parent=request, edge=edge)
    if via_reply:
        callback = owner.runner.advance(entered.operation_token, budget=budget(100))
        operation = lambda: owner.runner.reply(callback.request_token, (17,), budget=budget(100))
    else:
        operation = lambda: owner.runner.advance(entered.operation_token, budget=budget(100))
    with pytest.raises(MemoryError):
        operation()
    latest = owner.runner.last_receipt()
    assert latest.state == "returned" and latest.depth == 2 and not latest.invocation_started
    assert _view(owner.state) == saved
    with pytest.raises(RuntimeError):
        owner.runner.reply(request.request_token, (99,), budget=budget(100))
    with pytest.raises(RuntimeError):
        owner.runner.cancel_suffix(request.operation_token)
    cancelled = owner.runner.cancel_all()
    assert cancelled.retired_invocation_ids == (request.receipt.invocation_id,)
    assert receipt(cancelled.receipt) == receipt(latest)
    retry = owner.park(parent)
    assert retry.receipt.root_entries == 3 and retry.receipt.sequence > latest.sequence
    owner.runner.cancel_all()
    owner.runner.close()


def test_real_suffix_delivery_failure_keeps_restored_parent_and_only_actual_remaining_frames():
    owner = Owner()
    parent, child, edge = owner.pair(CALL, callbacks=(("call", "stub", 1, 1, 1),))
    owner.runner._test_fail_next_cancel_delivery()
    request = owner.park(parent)
    saved = _view(owner.state)
    nested = owner.park(child, parent=request, edge=edge)
    retained = receipt(owner.runner.last_receipt())
    before_bytes = bytes(owner.external), bytes(owner.control)
    cycles = owner.state.cycle_count
    with pytest.raises(MemoryError):
        owner.runner.cancel_suffix(nested.operation_token)
    assert _view(owner.state) == saved and owner.state.cycle_count == cycles
    assert (bytes(owner.external), bytes(owner.control)) == before_bytes
    assert receipt(owner.runner.last_receipt()) == retained
    for action in (lambda: owner.runner.reply(request.request_token, (99,), budget=budget(100)),
                   lambda: owner.runner.cancel_suffix(request.operation_token), owner.private.close,
                   lambda: owner.state.set_reg(4, 99)):
        with pytest.raises(RuntimeError):
            action()
    cancelled = owner.runner.cancel_all()
    assert cancelled.retired_invocation_ids == (request.receipt.invocation_id,)
    assert receipt(cancelled.receipt) == retained
    retry = owner.park(parent)
    assert retry.receipt.root_entries == 3
    owner.runner.cancel_all()
    owner.runner.close()


def test_failed_parent_restoration_preserves_prefix_and_permanently_disables_admission():
    owner = Owner()
    parent, child, edge = owner.pair("ldi r6,90\nst.b r4,r6\ninc r4\nret.l")
    request = owner.park(parent, (EXT_BASE,), ((EXT_BASE, 8, "write"),))
    entered = owner.admit(child, (EXT_BASE,), ((EXT_BASE, 8, "write"),), parent=request, edge=edge)
    yielded = owner.runner.advance(entered.operation_token, budget=budget(2))
    slot = parent.stack_base + parent.stack_size - 16 - CONTROL_BASE
    owner.control[slot] ^= 1
    before, retained = owner.snapshot(), receipt(owner.runner.last_receipt())
    with pytest.raises(ValueError, match="control"):
        owner.runner.cancel_suffix(yielded.operation_token)
    assert owner.snapshot() == before and receipt(owner.runner.last_receipt()) == retained
    assert owner.external[0] == 90
    owner.control[slot] ^= 1
    with pytest.raises(RuntimeError):
        owner.runner.advance(yielded.operation_token, budget=budget(100))
    cancelled = owner.runner.cancel_all()
    assert cancelled.retired_invocation_ids == (yielded.receipt.invocation_id, request.receipt.invocation_id)
    assert receipt(cancelled.receipt) == retained
    with pytest.raises(RuntimeError):
        owner.admit(parent)
    owner.runner.close()
