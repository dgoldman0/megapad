"""Parked validation proves exact task authority without creating machine work."""

from __future__ import annotations

import pytest

import _mp64_accel as native
from tests.test_native_task_routine import Harness, budget, receipt
from tests.test_native_task_children import CALL, CONTROL_BASE, Owner


def check_unchanged(harness, root, event, *, repeats=3):
    before = harness.snapshot(), receipt(harness.runner.last_receipt())
    for _ in range(repeats):
        assert harness.runner.validate_parked(root, event.operation_token, event.request_token) is True
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before


def test_validation_never_initializes_an_admitted_root_or_writes_its_sentinel():
    harness = Harness()
    try:
        for index in range(32):
            harness.state.set_reg(index, 0xB000 + index)
        harness.state.psel, harness.state.xsel, harness.state.spsel = 8, 9, 10
        harness.state.sw = 7
        harness.state.flags_unpack(255)
        harness.state.d_reg = 17
        harness.state.ext_modifier = 0
        harness.state.halted = harness.state.idle = True
        root = harness.bind()
        before = harness.snapshot()
        entered = harness.begin(root)
        assert entered.receipt.invocation_started and entered.receipt.root_instructions == 0
        check_unchanged(harness, root, entered)
        assert harness.snapshot() == before
        cancelled = harness.runner.cancel_suffix(entered.operation_token)
        assert cancelled.retired_invocation_ids == (entered.receipt.invocation_id,)
        assert harness.snapshot() == before
    finally:
        harness.runner.close()


def test_validation_does_not_rotate_tokens_lower_limits_or_consume_staged_callback_outputs():
    harness = Harness(instructions=5)
    try:
        root = harness.bind(instructions=5, callbacks=1)
        entered = harness.begin(root, allowance=budget(own=5, root=5, callbacks=1, root_callbacks=1))
        check_unchanged(harness, root, entered)
        yielded = harness.runner.advance(entered.operation_token, budget=budget(1))
        assert yielded.receipt.state == "yielded" and yielded.instructions == 1
        check_unchanged(harness, root, yielded)
        pending = harness.runner.advance(yielded.operation_token, budget=budget(100))
        assert pending.receipt.state == "callback" and pending.receipt.root_instructions == 3
        check_unchanged(harness, root, pending)
        before = harness.snapshot()
        staged = harness.runner.reply(pending.request_token, (99,), budget=budget(0))
        assert staged.receipt.state == "yielded" and harness.snapshot() == before
        check_unchanged(harness, root, staged)
        assert harness.state.get_reg(4) == 7
        completed = harness.runner.advance(staged.operation_token, budget=budget(100))
        assert completed.outputs == (99,) and completed.receipt.state == "returned"
        assert (completed.receipt.sequence, completed.receipt.root_entries,
                completed.receipt.root_instructions, completed.receipt.root_cycles,
                completed.receipt.root_callbacks) == (5, 1, 5, 9, 1)
        before = harness.snapshot(), receipt(harness.runner.last_receipt())
        with pytest.raises(ValueError):
            harness.runner.validate_parked(root, completed.operation_token)
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before
    finally:
        harness.runner.close()


@pytest.mark.parametrize("wrong", ("root", "operation", "request", "missing_request", "stale_operation",
                                   "null_root", "null_operation", "wrong_root_type", "wrong_request_type"))
def test_invalid_authority_preserves_pending_request_and_all_receipts_for_retry(wrong):
    harness, foreign = Harness(), Harness()
    try:
        root, foreign_root = harness.bind(), foreign.bind()
        entered = harness.begin(root)
        pending = harness.runner.advance(entered.operation_token, budget=budget(100))
        alien = foreign.begin(foreign_root)
        alien = foreign.runner.advance(alien.operation_token, budget=budget(100))
        arguments = [root, pending.operation_token, pending.request_token]
        index, value = {
            "root": (0, foreign_root), "operation": (1, alien.operation_token),
            "request": (2, alien.request_token), "missing_request": (2, None),
            "stale_operation": (1, entered.operation_token), "null_root": (0, None),
            "null_operation": (1, None), "wrong_root_type": (0, pending.operation_token),
            "wrong_request_type": (2, pending.operation_token),
        }[wrong]
        arguments[index] = value
        before = harness.snapshot(), receipt(harness.runner.last_receipt())
        with pytest.raises((TypeError, ValueError)):
            harness.runner.validate_parked(*arguments)
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before
        check_unchanged(harness, root, pending)
        assert harness.runner.reply(pending.request_token, (33,), budget=budget(100)).outputs == (33,)
    finally:
        harness.runner.close()
        foreign.runner.close()


def test_consumed_request_is_forbidden_on_a_yielded_frame_even_with_its_current_operation():
    harness = Harness()
    try:
        root = harness.bind()
        entered = harness.begin(root)
        pending = harness.runner.advance(entered.operation_token, budget=budget(100))
        staged = harness.runner.reply(pending.request_token, (19,), budget=budget(0))
        before = harness.snapshot(), receipt(harness.runner.last_receipt())
        with pytest.raises(ValueError, match="yielded"):
            harness.runner.validate_parked(root, staged.operation_token, pending.request_token)
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before
        assert harness.runner.validate_parked(root, staged.operation_token) is True
        assert harness.runner.advance(staged.operation_token, budget=budget(100)).outputs == (19,)
    finally:
        harness.runner.close()


def test_validation_covers_uninitialized_and_running_child_without_retargeting_parent_request():
    owner = Owner()
    try:
        parent, child, edge = owner.pair(CALL, callbacks=(("call", "stub", 1, 1, 1),))
        pending_parent = owner.park(parent)
        check_unchanged(owner, owner.root, pending_parent)
        parent_view = owner.snapshot()
        entered = owner.admit(child, parent=pending_parent, edge=edge)
        assert owner.snapshot() == parent_view
        check_unchanged(owner, owner.root, entered)
        assert owner.snapshot() == parent_view
        before = owner.snapshot(), receipt(owner.runner.last_receipt())
        with pytest.raises(ValueError):
            owner.runner.validate_parked(owner.root, pending_parent.operation_token, pending_parent.request_token)
        assert (owner.snapshot(), receipt(owner.runner.last_receipt())) == before
        yielded = owner.runner.advance(entered.operation_token, budget=budget(1))
        check_unchanged(owner, owner.root, yielded)
        pending_child = owner.runner.advance(yielded.operation_token, budget=budget(100))
        check_unchanged(owner, owner.root, pending_child)
        with pytest.raises(ValueError):
            owner.runner.validate_parked(owner.root, pending_child.operation_token, pending_parent.request_token)
        assert owner.runner.reply(pending_child.request_token, (11,), budget=budget(100)).outputs == (11,)
        check_unchanged(owner, owner.root, pending_parent)
        final = owner.runner.reply(pending_parent.request_token, (22,), budget=budget(100))
        assert final.outputs == (22,)
        assert (final.receipt.root_entries, final.receipt.root_instructions, final.receipt.root_callbacks) == (2, 8, 2)
    finally:
        owner.runner.close()


@pytest.mark.parametrize("phase,evidence", tuple(
    (phase, evidence)
    for phase in ("uninitialized", "yielded", "callback")
    for evidence in ("leaf_code", "ancestor_code", "ancestor_control", "leaf_control")
    if (phase, evidence) != ("uninitialized", "leaf_control")
))
def test_changed_live_code_and_control_fail_before_any_resume_effect_and_allow_exact_repair(phase, evidence):
    owner = Owner()
    try:
        parent, child, edge = owner.pair(CALL, callbacks=(("call", "stub", 1, 1, 1),))
        pending = owner.park(parent)
        event = owner.admit(child, parent=pending, edge=edge)
        if phase != "uninitialized":
            event = owner.runner.advance(event.operation_token, budget=budget(1 if phase == "yielded" else 100))
        if evidence.endswith("code"):
            storage = owner.ram
            offset = child.code_base if evidence == "leaf_code" else parent.code_base
        else:
            storage = owner.control
            selected = child if evidence == "leaf_control" else parent
            offset = selected.stack_base + selected.stack_size - 8 - CONTROL_BASE
        storage[offset] ^= 1
        before = owner.snapshot(), receipt(owner.runner.last_receipt())
        with pytest.raises(ValueError, match="seal|code|control"):
            owner.runner.validate_parked(owner.root, event.operation_token, event.request_token)
        assert (owner.snapshot(), receipt(owner.runner.last_receipt())) == before
        storage[offset] ^= 1
        check_unchanged(owner, owner.root, event)
        cancelled = owner.runner.cancel_suffix(event.operation_token)
        assert cancelled.surviving_parent_token is pending.request_token
        check_unchanged(owner, owner.root, pending)
        assert owner.runner.reply(pending.request_token, (17,), budget=budget(100)).outputs == (17,)
    finally:
        owner.runner.close()


def test_failed_frame_is_cancel_only_and_cannot_be_validated_for_detachment():
    harness = Harness(instructions=1)
    try:
        root = harness.bind()
        event = harness.begin(root)
        failed = harness.runner.advance(event.operation_token, budget=budget(100))
        assert failed.receipt.state == "failed" and failed.receipt.instructions == 1
        before = harness.snapshot(), receipt(harness.runner.last_receipt())
        with pytest.raises(ValueError, match="resumable"):
            harness.runner.validate_parked(root, failed.operation_token)
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before
        assert harness.runner.cancel_suffix(failed.operation_token).retired_invocation_ids == (failed.receipt.invocation_id,)
        assert receipt(harness.runner.last_receipt()) == before[1]
    finally:
        harness.runner.close()


def test_query_does_not_consume_delivery_failpoint_and_failed_delivery_blocks_validation():
    harness = Harness()
    try:
        harness.runner._test_fail_marshalling_after_results(2)
        root = harness.bind()
        entered = harness.begin(root)
        check_unchanged(harness, root, entered, repeats=5)
        with pytest.raises(MemoryError):
            harness.runner.advance(entered.operation_token, budget=budget(100))
        latest = receipt(harness.runner.last_receipt())
        before = harness.snapshot()
        with pytest.raises(RuntimeError, match="delivery"):
            harness.runner.validate_parked(root, entered.operation_token)
        assert harness.snapshot() == before and receipt(harness.runner.last_receipt()) == latest
        harness.runner.cancel_all()
        entered = harness.begin(root)
        check_unchanged(harness, root, entered)
    finally:
        harness.runner.close()


@pytest.mark.parametrize("private_version", (2, 3))
def test_private_transport_cannot_supply_task_validation_authority(private_version):
    harness = Harness()
    try:
        root = harness.bind()
        old = harness.begin(root)
        harness.runner.cancel_all()
        if private_version == 2:
            spec = native.RoutineSpecV2(**{name: value for name, value in harness.fields.items()
                                          if name != "max_callback_requests"})
            runner = harness.private.legacy_v2()
            runner.publish_code_v2(spec)
            pending = runner.begin_v2(spec, (7, 3), (), 100)
            finish = lambda: runner.resume_callback(pending.token, (31,))
        else:
            spec = native.RoutineSpecV3(**harness.fields)
            runner = harness.private
            runner.publish_code_v3(spec)
            pending = runner.begin_root_v3(spec, (7, 3), (), 100)
            finish = lambda: runner.resume_callback_v3(pending.token, (31,))
        before = harness.snapshot(), receipt(harness.runner.last_receipt())
        with pytest.raises((ValueError, RuntimeError)):
            harness.runner.validate_parked(root, old.operation_token)
        assert (harness.snapshot(), receipt(harness.runner.last_receipt())) == before
        assert finish().outputs == (31,)
    finally:
        harness.runner.close()


def test_cancel_close_and_public_cpu_exclusion_cannot_be_bypassed_by_the_query():
    harness = Harness()
    root = harness.bind()
    entered = harness.begin(root)
    with pytest.raises(RuntimeError):
        harness.state.set_reg(4, 99)
    check_unchanged(harness, root, entered)
    harness.runner.cancel_all()
    latest = receipt(harness.runner.last_receipt())
    with pytest.raises(ValueError):
        harness.runner.validate_parked(root, entered.operation_token)
    harness.runner.close()
    with pytest.raises(RuntimeError):
        harness.runner.validate_parked(root, entered.operation_token)
    assert receipt(harness.runner.last_receipt()) == latest
    assert native._TASK_ROUTINE_TRANSPORT_REVISION == 2
    assert not hasattr(native, "HYBRID_TASK_ROUTINE_ABI_VERSION")
