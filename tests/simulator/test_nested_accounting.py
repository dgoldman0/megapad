"""Nested ledger invariants, without enabling a machine dispatch profile."""

from __future__ import annotations

import copy
import pickle
from types import SimpleNamespace

import pytest

from simulator.errors import StepBudgetExceeded
from simulator.interop_exports import CallbackExportBudgetExceeded, CallbackExportError
from simulator.interop_nested import (
    CapturedMachineCall, MachineCallUse, NestedCallbackCheckpoint, NestedChainAccounting,
    NestedChainToken, capture_machine_call,
)
from simulator.ir import Call, Return
from simulator.runtime import MegaForthRuntime, _StepMeter


class _ProvedMachineFixture:
    """Model an already-proved dependency; this fixture never executes it."""

    def __init__(self, owner, target):
        self.owner, self.target = owner, target

    def require(self, owner=None):
        if owner is not None and owner is not self.owner:
            raise CallbackExportError("foreign test owner")
        return self.owner

    def call(self, operation, target):
        assert operation == "_verify_nested_target" and target is self.target


def ledger(*, limit=100, budget=None, on_tick=None, repeat_operation=False):
    runtime = MegaForthRuntime()
    target = SimpleNamespace(word=runtime.find("ABS"))
    operation = Call(target.word.xt)
    word = runtime.define_colon("STATIC", ((operation, operation, Return())
                                           if repeat_operation else (operation, Return())))
    engine = runtime._callback_exports
    owner, handle, native_edge = object(), object(), object()
    adapter = _ProvedMachineFixture(owner, target)
    engine._nested_owner = adapter
    captured = capture_machine_call(engine, handle, word, operation, 0, target)
    meter = _StepMeter(budget, on_tick or (lambda: None))
    chain = NestedChainAccounting(engine, adapter, owner, meter, limit)
    engine._nested_chain = chain
    return SimpleNamespace(runtime=runtime, engine=engine, meter=meter, chain=chain,
                           handle=handle, captured=captured, edge=native_edge,
                           word=word, operation=operation, target=target)


def enter(state, invocation=1, local_limit=100, *, via_use=None):
    checkpoint = state.chain.begin_callback(
        state.handle, invocation, local_limit, via_use=via_use,
    )
    state.chain.enter_callback(checkpoint, state.handle)
    return checkpoint


def child_use(state, checkpoint):
    use = state.chain.issue_machine_use(checkpoint, state.captured, state.edge)
    captured, edge = state.chain.consume_machine_use(use)
    assert captured.token is state.captured and edge is state.edge
    return use


def finish_callback(state, checkpoint, *, completed=True):
    state.chain.leave_callback(checkpoint, completed=completed)
    return state.chain.consume_callback(checkpoint)


def test_descendant_ticks_charge_root_once_and_ancestors_inclusively():
    state = ledger()
    parent = enter(state)
    for _ in range(3):
        state.chain.tick(parent)
    use = child_use(state, parent)
    child = enter(state, invocation=2, via_use=use)
    state.chain.tick(child)
    state.chain.tick(child)
    child_receipt = finish_callback(state, child)
    assert (child_receipt.inclusive_semantic_steps, child_receipt.chain_semantic_steps) == (2, 5)
    state.chain.finish_machine_use(use)
    state.chain.tick(parent)
    parent_receipt = finish_callback(state, parent)
    assert (parent_receipt.inclusive_semantic_steps, parent_receipt.chain_semantic_steps) == (6, 6)
    final = state.engine._finish_nested_chain(state.chain.token)
    assert final.chain_semantic_steps == state.meter.steps == 6
    assert final.callbacks_entered == 2 and final.maximum_callback_depth == 2
    assert not final.cancelled and state.engine._nested_chain is None


def test_full_depth_is_bounded_before_ninth_checkpoint_or_tick():
    state = ledger()
    checkpoint = enter(state)
    for depth in range(2, 9):
        state.chain.tick(checkpoint)
        use = child_use(state, checkpoint)
        checkpoint = enter(state, invocation=depth, via_use=use)
    state.chain.tick(checkpoint)
    use = child_use(state, checkpoint)
    before = state.meter.steps
    with pytest.raises(CallbackExportError, match="depth exceeds eight"):
        enter(state, invocation=9, via_use=use)
    assert state.meter.steps == before == 8
    receipt = state.engine._finish_nested_chain(state.chain.token, cancelled=True)
    assert receipt.maximum_callback_depth == 8 and receipt.callbacks_entered == 8
    assert receipt.chain_semantic_steps == 8 and receipt.cancelled
    with pytest.raises(CallbackExportError):
        state.chain.consume_machine_use(use)


@pytest.mark.parametrize("which", ("root", "parent", "child", "ordinary"))
def test_every_active_allowance_rejects_next_tick_before_effect(which):
    state = ledger(limit=2 if which == "root" else 100,
                   budget=2 if which == "ordinary" else None)
    parent = enter(state, local_limit=2 if which == "parent" else 100)
    state.chain.tick(parent)
    use = child_use(state, parent)
    child = enter(state, invocation=2, local_limit=1 if which == "child" else 100, via_use=use)
    state.chain.tick(child)
    expected = StepBudgetExceeded if which == "ordinary" else CallbackExportBudgetExceeded
    with pytest.raises(expected) as caught:
        state.chain.tick(child)
    if which != "ordinary":
        assert caught.value.reason == ("callback_semantic_limit" if which == "root" else "callback_local_limit")
        assert state.engine.consume_budget_failure(caught.value)
    assert state.meter.steps == 2
    assert state.engine._finish_nested_chain(state.chain.token, cancelled=True).chain_semantic_steps == 2


def test_repeated_siblings_require_fresh_one_shot_call_authority():
    state = ledger()
    parent = enter(state)
    state.chain.tick(parent)
    uses = []
    for invocation in (2, 3):
        use = child_use(state, parent)
        uses.append(use)
        with pytest.raises(CallbackExportError, match="already consumed"):
            state.chain.consume_machine_use(use)
        child = enter(state, invocation=invocation, via_use=use)
        with pytest.raises(CallbackExportError):
            state.chain.finish_machine_use(use)
        state.chain.tick(child)
        finish_callback(state, child)
        state.chain.finish_machine_use(use)
    assert uses[0] is not uses[1]
    with pytest.raises(CallbackExportError):
        state.chain.consume_machine_use(uses[0])
    receipt = finish_callback(state, parent)
    assert receipt.inclusive_semantic_steps == receipt.chain_semantic_steps == 3


def test_checkpoint_cannot_be_reentered_or_consumed_during_execution():
    state = ledger()
    checkpoint = enter(state)
    with pytest.raises(CallbackExportError, match="one exact"):
        state.chain.enter_callback(checkpoint, state.handle)
    with pytest.raises(CallbackExportError, match="still executing"):
        state.chain.consume_callback(checkpoint)
    with pytest.raises(CallbackExportError, match="active captured"):
        enter(state, invocation=2)
    with pytest.raises(CallbackExportError, match="still owns"):
        state.engine._finish_nested_chain(state.chain.token)
    state.chain.tick(checkpoint)
    finish_callback(state, checkpoint)
    with pytest.raises(CallbackExportError):
        state.chain.consume_callback(checkpoint)


@pytest.mark.parametrize("mode", ("reset", "inflate", "foreign"))
def test_host_exception_retains_tick_and_repairs_meter_without_user_arithmetic(mode):
    called = []
    original = RuntimeError("host hook failed")

    class ForeignSteps:
        def __sub__(self, other):
            called.append("sub")
            raise AssertionError("foreign arithmetic")

    state = ledger()
    def fail():
        state.meter.steps = {"reset": 0, "inflate": 999, "foreign": ForeignSteps()}[mode]
        raise original
    state.meter._on_tick = fail
    state.chain.on_tick = fail
    checkpoint = enter(state)
    with pytest.raises(RuntimeError) as caught:
        state.chain.tick(checkpoint)
    assert caught.value is original and called == []
    assert state.meter.steps == 1
    receipt = finish_callback(state, checkpoint, completed=False)
    assert receipt.entered and not receipt.completed
    assert receipt.inclusive_semantic_steps == receipt.chain_semantic_steps == 1
    assert state.engine._registration_failure is not None
    assert state.engine._finish_nested_chain(state.chain.token).chain_semantic_steps == 1


def test_post_dispatch_wrapper_mutation_cannot_refund_independent_receipt():
    state = ledger()
    checkpoint = enter(state)
    state.chain.tick(checkpoint)
    state.chain.leave_callback(checkpoint, completed=True)
    state.meter.steps = 0
    receipt = state.chain.consume_callback(checkpoint)
    assert receipt.completed and receipt.chain_semantic_steps == 1
    assert state.meter.steps == 1 and state.engine._registration_failure is not None


@pytest.mark.parametrize("control", ("budget", "hook", "route"))
def test_host_error_with_changed_meter_controls_is_preserved_and_fail_closed(control, monkeypatch):
    original = RuntimeError("hook changed meter controls")
    state = ledger(budget=50)
    def fail():
        if control == "budget":
            state.meter.budget = 9999
        elif control == "hook":
            state.meter._on_tick = lambda: None
        else:
            monkeypatch.setattr(_StepMeter, "tick", lambda _self: None)
        raise original
    state.meter._on_tick = fail
    state.chain.on_tick = fail
    checkpoint = enter(state)
    with pytest.raises(RuntimeError) as caught:
        state.chain.tick(checkpoint)
    assert caught.value is original
    assert state.engine._registration_failure is not None
    receipt = finish_callback(state, checkpoint, completed=False)
    assert receipt.chain_semantic_steps == receipt.inclusive_semantic_steps == 1
    assert state.engine._finish_nested_chain(state.chain.token).chain_semantic_steps == 1


def test_before_invocation_failure_has_zero_work_and_no_completed_claim():
    state = ledger()
    checkpoint = state.chain.begin_callback(state.handle, 1, 10)
    receipt = state.chain.consume_callback(checkpoint)
    assert not receipt.entered and not receipt.completed
    assert receipt.inclusive_semantic_steps == receipt.chain_semantic_steps == 0


def test_foreign_owner_tokens_and_callback_handles_do_not_grant_authority():
    first, second = ledger(), ledger()
    checkpoint = first.chain.begin_callback(first.handle, 1, 10)
    with pytest.raises(CallbackExportError, match="one exact"):
        first.chain.enter_callback(checkpoint, second.handle)
    with pytest.raises(CallbackExportError):
        second.chain.enter_callback(checkpoint, second.handle)
    with pytest.raises(CallbackExportError):
        second.engine._finish_nested_chain(first.chain.token, cancelled=True)
    first.chain.enter_callback(checkpoint, first.handle)
    assert first.meter.steps == second.meter.steps == 0


@pytest.mark.parametrize("kind", (NestedChainToken, NestedCallbackCheckpoint, CapturedMachineCall, MachineCallUse))
def test_opaque_authorities_are_not_constructible_copyable_or_serializable(kind):
    state = ledger()
    checkpoint = enter(state)
    use = state.chain.issue_machine_use(checkpoint, state.captured, state.edge)
    value = {NestedChainToken: state.chain.token, NestedCallbackCheckpoint: checkpoint,
             CapturedMachineCall: state.captured, MachineCallUse: use}[kind]
    with pytest.raises(TypeError):
        kind()
    for operation in (copy.copy, copy.deepcopy, pickle.dumps):
        with pytest.raises(TypeError):
            operation(value)


def test_mutated_static_call_is_rejected_before_consuming_its_issued_use():
    state = ledger()
    checkpoint = enter(state)
    use = state.chain.issue_machine_use(checkpoint, state.captured, state.edge)
    before = state.operation.xt
    object.__setattr__(state.operation, "xt", state.runtime.find("MIN").xt)
    with pytest.raises(CallbackExportError, match="captured machine Call changed"):
        state.chain.consume_machine_use(use)
    object.__setattr__(state.operation, "xt", before)
    assert state.chain.consume_machine_use(use)[0].token is state.captured
    assert state.meter.steps == 0


def test_reused_call_object_at_two_indices_has_distinct_static_site_authority():
    state = ledger(repeat_operation=True)
    state.engine._finish_nested_chain(state.chain.token, cancelled=True)
    second = capture_machine_call(state.engine, state.handle, state.word,
                                  state.operation, 1, state.target)
    assert second is not state.captured
    assert state.engine._nested_calls[state.captured].operation_index == 0
    assert state.engine._nested_calls[second].operation_index == 1
    assert capture_machine_call(state.engine, state.handle, state.word,
                                state.operation, 1, state.target) is second


def test_static_capture_is_not_mutable_during_a_chain():
    state = ledger()
    with pytest.raises(CallbackExportError, match="idle export"):
        capture_machine_call(state.engine, state.handle, state.word,
                             state.operation, 0, state.target)


def test_chain_checkpoint_excludes_unrelated_source_entry_and_publication():
    state = ledger()
    with pytest.raises(CallbackExportError, match="nested callback accounting"):
        state.runtime.evaluate(b"1")
    with pytest.raises(CallbackExportError, match="nested callback accounting"):
        state.runtime.define_colon("UNRELATED", (Return(),))
    assert state.meter.steps == 0
    state.engine._finish_nested_chain(state.chain.token, cancelled=True)
    state.runtime.evaluate(b"1")
    assert state.runtime.main_context.data.snapshot() == (1,)


def test_legacy_closed_accounting_cannot_overlap_a_nested_chain():
    state = ledger()
    with pytest.raises(CallbackExportError, match="active owner"):
        state.runtime.begin_closed_callback_accounting(state.handle)
    assert state.engine._closed_accounting is None
    checkpoint = enter(state)
    state.chain.tick(checkpoint)
    assert finish_callback(state, checkpoint).chain_semantic_steps == 1


@pytest.fixture
def hybrid_owner():
    pytest.importorskip("_mp64_accel")
    from hybrid.runtime import HybridRuntime
    owner = HybridRuntime.create(geometry={"bank0_size": 65536, "external_size": 4096})
    yield owner
    owner.close()


def test_nested_owner_installation_is_single_and_exact(hybrid_owner):
    engine = hybrid_owner.semantic._callback_exports
    with pytest.raises(CallbackExportError, match="one fresh idle"):
        engine._install_nested_owner(hybrid_owner)
    assert engine._nested_owner.require(hybrid_owner) is hybrid_owner
    assert engine._nested_chain is None


@pytest.mark.parametrize("version", (1, 2, 3))
def test_legacy_embedding_subclass_executes_without_acquiring_nested_authority(version):
    pytest.importorskip("_mp64_accel")
    from asm import assemble
    from hybrid.runtime import HybridRuntime
    from shared.hybrid_abi import RoutineImageV1, RoutineImageV2, RoutineImageV3

    class EmbeddedHybrid(HybridRuntime):
        def execute(self, name_or_xt, **kwargs):
            result = super().execute(name_or_xt, **kwargs)
            self.completed_embedding_calls = getattr(self, "completed_embedding_calls", 0) + 1
            return result

    owner = EmbeddedHybrid.create(geometry={"bank0_size": 65536, "external_size": 4096})
    try:
        assert type(owner) is EmbeddedHybrid
        assert owner.semantic._callback_exports._nested_owner is None
        assert owner.nested_callback_abi_available is False
        image_type = (RoutineImageV1, RoutineImageV2, RoutineImageV3)[version - 1]
        image = image_type(name="EMBEDDED-INC", code=bytes(assemble("inc r4\nret.l")),
                           entry_offset=0, input_cells=1, output_cells=1, buffers=(),
                           return_stack_cells=16, max_instructions=10,
                           **({"callbacks": ()} if version > 1 else {}))
        word = getattr(owner, f"register_routine_v{version}")(image)
        owner.semantic.main_context.data.push(41)
        report = owner.execute(word.xt)
        assert owner.semantic.main_context.data.snapshot() == (42,)
        assert report.machine_instructions == 2 and report.transitions == 1
        assert owner.completed_embedding_calls == 1
        with pytest.raises(CallbackExportError, match="exact HybridRuntime"):
            owner.semantic._callback_exports._install_nested_owner(owner)
    finally:
        owner.close()


@pytest.mark.parametrize("route", ("_validate_registration", "_require_open", "_require_authority"))
def test_owner_validation_route_replacement_cannot_weaken_child_admission(hybrid_owner, monkeypatch, route):
    adapter = hybrid_owner.semantic._callback_exports._nested_owner
    with monkeypatch.context() as patch:
        patch.setitem(vars(hybrid_owner), route, lambda *_args: None)
        with pytest.raises(CallbackExportError, match="owner route changed"):
            adapter.require()
    assert adapter.require() is hybrid_owner


@pytest.mark.parametrize("field", ("semantic", "_memory"))
def test_owner_descriptor_replacement_is_rejected_before_getter(hybrid_owner, monkeypatch, field):
    adapter = hybrid_owner.semantic._callback_exports._nested_owner
    calls = []
    def forged(_owner):
        calls.append(field)
        raise AssertionError("custom owner getter ran")
    with monkeypatch.context() as patch:
        patch.setattr(type(hybrid_owner), field, property(forged), raising=False)
        with pytest.raises(CallbackExportError, match="attribute routing"):
            adapter.require()
    assert calls == []


def test_chain_uses_exact_original_active_meter(hybrid_owner):
    runtime, engine = hybrid_owner.semantic, hybrid_owner.semantic._callback_exports
    foreign = _StepMeter(100, lambda: None)
    with pytest.raises(CallbackExportError, match="original outer meter"):
        engine._begin_nested_chain(hybrid_owner, foreign, 10)
    observed = []
    def host(_context):
        meter = runtime._active_dispatches[0].meter
        with pytest.raises(CallbackExportError, match="original outer meter"):
            engine._begin_nested_chain(hybrid_owner, foreign, 10)
        token = engine._begin_nested_chain(hybrid_owner, meter, 10)
        observed.append(engine._finish_nested_chain(token).chain_semantic_steps)
    runtime.define_primitive("CHAIN-OWNER-TEST", host)
    runtime.execute("CHAIN-OWNER-TEST")
    assert observed == [0] and engine._nested_chain is None


@pytest.mark.parametrize("value", (0, 1, None, "true"))
def test_required_nested_capability_flag_is_exact_boolean(value):
    from hybrid.runtime import HybridRuntime
    with pytest.raises(TypeError, match="exact boolean"):
        HybridRuntime.create(require_nested_callbacks=value)


def test_required_nested_profile_fails_before_any_semantic_owner(monkeypatch):
    import hybrid.runtime as module
    def forbidden(*_args, **_kwargs):
        raise AssertionError("unavailable nested profile constructed semantic ownership")
    monkeypatch.setattr(module, "MegaForthRuntime", forbidden)
    with pytest.raises(RuntimeError, match="fully qualified semantic V4"):
        module.HybridRuntime.create(require_nested_callbacks=True)
