"""Closed callbacks prove live IR, then retain ordinary guarded reference ticks."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import CallbackExportV2, CallbackExportV3
from simulator.errors import IllegalInstructionFault, StepBudgetExceeded
from simulator.interop_exports import CallbackExportError, CallbackExportBudgetExceeded
from simulator.ir import Literal, Call, CallSelf, Branch, BranchZero, Return, Idle, RPush
from simulator.runtime import MegaForthRuntime, ColonDefinition
from simulator.stacks import DataStack, ReturnStack


@pytest.fixture(params=("python", "native"))
def runtime(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return MegaForthRuntime(execution_backend=request.param)


def descriptor(name="CLAMP", *, inputs=3, outputs=1, steps=7, export_id=0):
    return CallbackExportV3(export_id=export_id, name=name, input_cells=inputs,
                            output_cells=outputs, max_semantic_steps=steps,
                            effect="closed_integer_colon")


def clamp(runtime):
    runtime.evaluate(b": CLAMP ROT MIN MAX ;")
    return runtime.bind_callback_export(descriptor())


def caller_state(runtime):
    context = runtime.main_context
    return context.data.snapshot(), context.returns.snapshot(), runtime.memory.read_bytes(
        context.data.floor, context.returns.empty_pointer - context.data.floor
    )


@pytest.mark.parametrize("arguments,expected", (
    ((9, 2, 7), 7), ((1, 2, 7), 2), ((4, 2, 7), 4),
    ((MASK64, MASK64 - 4, 3), MASK64),
))
def test_clamp_matches_semantics_with_exact_work_and_isolated_stacks(runtime, arguments, expected):
    handle = clamp(runtime)
    before = caller_state(runtime)
    ticks = runtime.diagnostics.semantic_cycles
    result = runtime.invoke_callback_export(handle, arguments)
    assert result.outputs == (expected,) and result.semantic_steps == 7
    assert runtime.diagnostics.semantic_cycles - ticks == 7
    assert caller_state(runtime) == before
    proof = runtime.inspect_callback_export(handle)
    assert proof.required_input_cells == 3 and proof.net_data_cells == -2
    assert proof.max_data_depth(3) == 3 and proof.max_return_cells == 1
    assert proof.min_semantic_steps == proof.max_semantic_steps == 7
    object.__setattr__(proof, "max_semantic_steps", 0)
    assert runtime.inspect_callback_export(handle).max_semantic_steps == 7


def test_nested_colon_calls_use_typed_return_cells_and_inferred_helper_signatures(runtime):
    runtime.evaluate(b": INNER DUP MIN ; : OUTER INNER ABS ;")
    handle = runtime.bind_callback_export(descriptor("OUTER", inputs=1, steps=9))
    result = runtime.invoke_callback_export(handle, (MASK64,))
    assert result.outputs == (1,) and result.semantic_steps == 9
    assert runtime.inspect_callback_export(handle).max_return_cells == 2


def test_zero_input_literal_policy_uses_finite_materialized_private_stacks(runtime):
    runtime.define_colon("SEVEN", (Literal(7), Return()))
    handle = runtime.bind_callback_export(descriptor("SEVEN", inputs=0, steps=2))
    assert runtime.invoke_callback_export(handle, ()).outputs == (7,)


def test_forward_branch_charges_only_chosen_path_under_remaining_allowance(runtime):
    runtime.define_colon("CHOICE", (BranchZero(3), Literal(7), Branch(4), Literal(9), Return()))
    handle = runtime.bind_callback_export(descriptor("CHOICE", inputs=1, steps=4))
    proof = runtime.inspect_callback_export(handle)
    assert (proof.min_semantic_steps, proof.max_semantic_steps) == (3, 4)
    assert runtime.invoke_callback_export(handle, (0,), semantic_step_limit=3).outputs == (9,)
    ticks = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportBudgetExceeded) as caught:
        runtime.invoke_callback_export(handle, (1,), semantic_step_limit=3)
    assert caught.value.reason == "callback_semantic_limit"
    assert runtime.diagnostics.semantic_cycles - ticks == 3
    assert runtime.consume_callback_budget_failure(caught.value)
    assert not runtime.consume_callback_budget_failure(caught.value)


def test_nested_callback_uses_original_outer_meter_without_replacement(runtime):
    handle = clamp(runtime)
    runtime.main_context.data.push(0xCAFE)
    runtime.define_primitive("HOST", lambda context: runtime.invoke_callback_export(handle, (4, 2, 7)))
    before = caller_state(runtime)
    ticks = runtime.diagnostics.semantic_cycles
    with pytest.raises(StepBudgetExceeded):
        runtime.execute("HOST", step_budget=3)
    assert runtime.diagnostics.semantic_cycles - ticks == 3
    assert caller_state(runtime) == before


def test_closed_dispatch_bypasses_native_and_colon_accelerator_routes_without_predicates(runtime, monkeypatch):
    handle = clamp(runtime)
    calls = []
    def forbidden(*args, **kwargs):
        calls.append(True)
        raise AssertionError("closed callback used an optimization route")
    monkeypatch.setattr(runtime, "_try_colon_accelerator", forbidden)
    if runtime._native_execution is not None:
        monkeypatch.setattr(runtime._native_execution, "run", forbidden)
    assert runtime.invoke_callback_export(handle, (4, 2, 7)).outputs == (4,)
    assert calls == []


@pytest.mark.parametrize("operations", (
    (Return(), Idle(), Return()), (RPush(), Return()), (CallSelf(), Return()),
    (Literal(1), Branch(0), Return()), (Literal(1), BranchZero(1), Return()),
    (Literal(1), BranchZero(3), Literal(2), Return()),
))
def test_unsupported_cycles_and_unequal_join_depths_fail_before_execution(runtime, operations):
    runtime.define_colon("BAD", operations)
    before = caller_state(runtime), runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError):
        runtime.bind_callback_export(descriptor("BAD", inputs=0, outputs=0, steps=4096))
    assert (caller_state(runtime), runtime.diagnostics.semantic_cycles) == before


@pytest.mark.parametrize("mutation", ("boolean_literal", "subclass", "fallthrough", "unknown_xt"))
def test_forged_or_nonexact_operation_metadata_is_rejected(runtime, mutation):
    operation = Literal(1)
    operations = (operation, Return())
    word = runtime.define_colon("BAD", operations)
    if mutation == "boolean_literal":
        object.__setattr__(operation, "value", True)
    elif mutation == "subclass":
        class CustomLiteral(Literal):
            pass
        object.__setattr__(word.implementation, "operations", (CustomLiteral(1), Return()))
    elif mutation == "fallthrough":
        object.__setattr__(word.implementation, "operations", (Literal(1),))
    else:
        object.__setattr__(word.implementation, "operations", (Call(MASK64), Return()))
    with pytest.raises(CallbackExportError):
        runtime.bind_callback_export(descriptor("BAD", inputs=0, steps=4096))


def test_mutual_recursion_hidden_after_return_is_rejected(runtime):
    first = runtime.define_colon("A", (Return(),))
    second = runtime.define_colon("B", (Call(first.xt), Return()))
    object.__setattr__(first.implementation, "operations", (Return(), Call(second.xt), Return()))
    with pytest.raises(CallbackExportError):
        runtime.bind_callback_export(descriptor("A", inputs=0, outputs=0, steps=4096))


@pytest.mark.parametrize("bound", ("data", "returns", "operations", "words", "work"))
def test_engine_enforces_all_finite_closure_bounds(runtime, bound):
    if bound == "data":
        word = runtime.define_colon("BAD", tuple(Literal(1) for _ in range(9)) + (Return(),))
    elif bound == "operations":
        word = runtime.define_colon("BAD", (Return(),) * 4097)
    elif bound in ("returns", "words"):
        word = runtime.find("ABS")
        for index in range(9 if bound == "returns" else 64):
            word = runtime.define_colon(f"CHAIN-{index}", (Call(word.xt), Return()))
    else:
        runtime.evaluate(b": BAD DUP DROP ;")
        word = runtime.find("BAD")
    with pytest.raises(CallbackExportError):
        runtime.bind_callback_export(descriptor(word.name.decode(), inputs=1, steps=4 if bound == "work" else 4096))


def test_shadowing_keeps_original_closure_but_xt_reuse_revokes_it(runtime):
    checkpoint = runtime.dictionary.checkpoint()
    handle = clamp(runtime)
    runtime.evaluate(b": CLAMP 999 ; : MIN 999 ;")
    assert runtime.invoke_callback_export(handle, (4, 2, 7)).outputs == (4,)
    runtime.dictionary.rollback(checkpoint)
    runtime.evaluate(b": CLAMP ROT MIN MAX ;")
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (4, 2, 7))


def test_canonical_stack_words_are_internal_dependencies_not_v2_leaf_exports(runtime):
    original = runtime.callback_policy_core_xt("ROT")
    runtime.evaluate(b": ROT 999 ;")
    assert runtime.callback_policy_core_xt("ROT") == original
    with pytest.raises(ValueError):
        CallbackExportV2(export_id=1, name="ROT", input_cells=3, output_cells=3)


def test_registration_transaction_preserves_existing_leaf_and_rolls_back_closed_binding(runtime):
    leaf = runtime.bind_callback_export(CallbackExportV2(export_id=1, name="ABS", input_cells=1, output_cells=1))
    runtime.evaluate(b": CLAMP ROT MIN MAX ;")
    with pytest.raises(RuntimeError, match="publication"):
        with runtime.callback_export_registration((descriptor(),)) as handles:
            failed = handles[0]
            assert runtime.inspect_callback_export(failed).max_semantic_steps == 7
            raise RuntimeError("publication")
    with pytest.raises(CallbackExportError):
        runtime.verify_callback_export(failed)
    assert runtime.invoke_callback_export(leaf, (MASK64,)).outputs == (1,)


@pytest.mark.parametrize("kind", (
    "data", "inactive_byte", "cookie", "continuation_field", "table", "geometry",
    "pop_route", "restore_route", "parent_call", "frame_meter", "setter_route",
))
def test_post_tick_mutations_decline_before_effect_and_never_call_tampered_routes(runtime, monkeypatch, kind):
    runtime.evaluate(b": COPY DUP DROP ;")
    word = runtime.find("COPY")
    handle = runtime.bind_callback_export(descriptor("COPY", inputs=1, steps=5))
    account = runtime._account_semantic_step
    touched, calls = [], []
    class Equal:
        def __eq__(self, other):
            calls.append("equality")
            return True
    def mutate():
        account()
        active = runtime._callback_exports._active
        if active is None or touched:
            return
        if kind == "parent_call" and active.meter.steps - active.starting_steps != 2:
            return
        touched.append(True)
        context = active.context
        if kind == "data":
            context.data._memory_view.write64(context.data.pointer, 999)
        elif kind == "inactive_byte":
            active.data_seal.page[0] = 1
        elif kind == "cookie":
            context.returns._continuation_cookie = True
        elif kind == "continuation_field":
            continuation = next(iter(context.returns._continuations.values()))[0]
            object.__setattr__(continuation, "ip", Equal())
        elif kind == "table":
            context.returns._continuations = {120: [Equal(), 0]}
        elif kind == "geometry":
            context.data._floor = Equal()
        elif kind == "pop_route":
            context.data.pop = lambda: calls.append("pop")
        elif kind == "restore_route":
            context.returns.restore = lambda value: calls.append("restore")
        elif kind == "parent_call":
            object.__setattr__(word.implementation.operations[0], "xt", runtime.find("ABS").xt)
        elif kind == "frame_meter":
            object.__setattr__(active.frame, "meter", Equal())
        else:
            monkeypatch.setattr(DataStack, "__setattr__", lambda *args: calls.append("setter"))
    monkeypatch.setattr(runtime, "_account_semantic_step", mutate)
    before = caller_state(runtime)
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (7,))
    assert touched and calls == []
    assert caller_state(runtime) == before


@pytest.mark.parametrize("action", ("execute", "evaluate", "define", "allot", "invoke"))
def test_accounting_hook_cannot_start_public_dispatch_or_publication(runtime, monkeypatch, action):
    handle = clamp(runtime)
    account = runtime._account_semantic_step
    blocked = []
    def probe():
        account()
        if blocked:
            return
        actions = {
            "execute": lambda: runtime.execute("ABS"),
            "evaluate": lambda: runtime.evaluate(b"123"),
            "define": lambda: runtime.define_colon("FORBIDDEN", (Return(),)),
            "allot": lambda: runtime.dictionary.allot(1),
            "invoke": lambda: runtime.invoke_callback_export(handle, (4, 2, 7)),
        }
        with pytest.raises(CallbackExportError):
            actions[action]()
        blocked.append(True)
    monkeypatch.setattr(runtime, "_account_semantic_step", probe)
    assert runtime.invoke_callback_export(handle, (4, 2, 7)).semantic_steps == 7
    assert blocked == [True] and runtime.find("FORBIDDEN") is None


@pytest.mark.parametrize("error", (
    IllegalInstructionFault("host fault"), KeyboardInterrupt("host interrupt"),
    CallbackExportBudgetExceeded("callback_semantic_limit", 0, 0),
))
def test_original_hook_exceptions_remain_exact_and_are_not_owned_budget_failures(runtime, monkeypatch, error):
    handle = clamp(runtime)
    account = runtime._account_semantic_step
    def fail():
        account()
        raise error
    with monkeypatch.context() as patch:
        patch.setattr(runtime, "_account_semantic_step", fail)
        with pytest.raises(type(error)) as caught:
            runtime.invoke_callback_export(handle, (4, 2, 7))
    assert caught.value is error
    assert not runtime.consume_callback_budget_failure(error)
    assert runtime.invoke_callback_export(handle, (4, 2, 7)).outputs == (4,)


@pytest.mark.parametrize("when", ("before_bind", "during_tick"))
@pytest.mark.parametrize("kind", ("literal", "word", "frame", "runtime_alias"))
def test_metadata_routes_are_rejected_without_invoking_replacement(runtime, monkeypatch, when, kind):
    from simulator import runtime as runtime_module
    from simulator.dictionary import Word
    from simulator.runtime import _DispatchFrame

    word = runtime.define_colon("SEVEN", (Literal(7), Return()))
    calls = []
    def getter(instance):
        calls.append("getter")
        return 7
    def replace_route():
        if kind == "literal":
            monkeypatch.setattr(Literal, "value", property(getter))
        elif kind == "word":
            monkeypatch.setattr(Word, "implementation", property(getter))
        elif kind == "frame":
            monkeypatch.setattr(_DispatchFrame, "meter", property(getter))
        else:
            class Trap:
                def __instancecheck__(self, instance):
                    calls.append("instancecheck")
                    return True
            monkeypatch.setattr(runtime_module, "Literal", Trap())
    if when == "before_bind":
        replace_route()
        with pytest.raises(CallbackExportError):
            runtime.bind_callback_export(descriptor("SEVEN", inputs=0, steps=2))
    else:
        handle = runtime.bind_callback_export(descriptor("SEVEN", inputs=0, steps=2))
        account = runtime._account_semantic_step
        def mutate():
            account()
            replace_route()
        monkeypatch.setattr(runtime, "_account_semantic_step", mutate)
        with pytest.raises(CallbackExportError):
            runtime.invoke_callback_export(handle, ())
    assert calls == []


def test_failure_before_begin_verification_does_not_replace_original(runtime, monkeypatch):
    from simulator.interop_closed import ClosedDispatch
    handle = clamp(runtime)
    error = MemoryError("verification allocation")
    def fail(self, word, root_id):
        raise error
    monkeypatch.setattr(ClosedDispatch, "begin", fail)
    with pytest.raises(MemoryError) as caught:
        runtime.invoke_callback_export(handle, (4, 2, 7))
    assert caught.value is error
    assert runtime._active_dispatches == []


@pytest.mark.parametrize("mutation", ("replace", "clear"))
def test_dispatch_list_cleanup_preserves_outer_error_and_completed_prefix(runtime, monkeypatch, mutation):
    handle = clamp(runtime)
    error = RuntimeError("original accounting escape")
    account = runtime._account_semantic_step
    def mutate():
        account()
        if runtime._callback_exports._active is not None:
            if mutation == "replace":
                runtime._active_dispatches = []
            else:
                runtime._active_dispatches.clear()
            raise error
    monkeypatch.setattr(runtime, "_account_semantic_step", mutate)
    def callback(context):
        runtime.invoke_callback_export(handle, (4, 2, 7))
    runtime.define_primitive("POLICY", callback)
    with pytest.raises(RuntimeError) as caught:
        runtime.evaluate(b"123 POLICY")
    assert caught.value is error
    assert runtime.main_context.data.snapshot() == (123,)
    assert runtime._active_dispatches == []
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (4, 2, 7))


@pytest.mark.parametrize("route", (
    "restore", "push", "push_continuation", "restore_pointer_captures",
    "has_pointer_captures_after", "_mark_host_control_fault",
))
@pytest.mark.parametrize("outer", ("execute", "evaluate", "resume"))
def test_unsafe_shared_cleanup_routes_never_run_during_outer_unwind(runtime, monkeypatch, route, outer):
    from simulator.runtime import ExecutionContext
    handle = clamp(runtime)
    account = runtime._account_semantic_step
    calls = []
    error = RuntimeError("host escape after shared route mutation")
    def mutate():
        account()
        if runtime._callback_exports._active is not None:
            owner = ExecutionContext if route == "_mark_host_control_fault" else ReturnStack
            monkeypatch.setattr(owner, route, lambda *args: calls.append(route))
            raise error
    monkeypatch.setattr(runtime, "_account_semantic_step", mutate)
    def callback(context):
        runtime.invoke_callback_export(handle, (4, 2, 7))
    runtime.define_primitive("POLICY", callback)
    runtime.evaluate(b": DRIVER 123 POLICY ;")
    with pytest.raises(RuntimeError) as caught:
        if outer == "evaluate":
            runtime.evaluate(b"DRIVER")
        elif outer == "execute":
            runtime.execute("DRIVER")
        else:
            # Stop after the literal, then enter the callback in the resumed guard.
            report = runtime.run_until_blocked("DRIVER", quantum_steps=1)
            runtime.resume_yielded(report.suspension)
    assert caught.value is error
    assert calls == []
    assert runtime.main_context.data.snapshot() == (123,)
    assert not runtime.main_context.reusable
    assert runtime._active_dispatches == []


@pytest.mark.parametrize("replacement", (0, "hostile", "namespace"))
@pytest.mark.parametrize("raises", (False, True))
def test_closed_tick_receipt_preserves_charge_and_original_error_on_meter_corruption(runtime, monkeypatch, replacement, raises):
    handle = clamp(runtime)
    checkpoint = runtime.begin_closed_callback_accounting(handle)
    account = runtime._account_semantic_step
    calls = []
    original = KeyboardInterrupt("accounting interrupted")
    class Hostile:
        def __sub__(self, other):
            calls.append("subtract")
            raise AssertionError("untrusted arithmetic")
    observed = []
    def mutate():
        account()
        active = runtime._callback_exports._active
        observed.append((active.meter, active.starting_steps))
        if replacement == "namespace":
            active.meter.__dict__ = {"steps": Hostile(), "budget": active.meter.budget, "_on_tick": active.meter._on_tick}
        else:
            active.meter.steps = 0 if replacement == 0 else Hostile()
        if raises:
            raise original
    monkeypatch.setattr(runtime, "_account_semantic_step", mutate)
    with pytest.raises(KeyboardInterrupt if raises else CallbackExportError) as caught:
        runtime.invoke_callback_export(handle, (4, 2, 7))
    if raises:
        assert caught.value is original
    receipt = runtime.consume_closed_callback_accounting(checkpoint, handle)
    assert (receipt.semantic_steps, receipt.entered, receipt.completed) == (1, True, False)
    meter, starting = observed[0]
    assert type(meter.steps) is int and meter.steps == starting + 1
    assert calls == []
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (4, 2, 7))


def test_closed_accounting_is_bounded_owner_bound_and_one_shot(runtime):
    handle = clamp(runtime)
    checkpoint = runtime.begin_closed_callback_accounting(handle)
    with pytest.raises(CallbackExportError):
        runtime.begin_closed_callback_accounting(handle)
    with pytest.raises(CallbackExportError):
        runtime.consume_closed_callback_accounting(object(), handle)
    assert runtime.invoke_callback_export(handle, (4, 2, 7)).semantic_steps == 7
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (4, 2, 7))
    receipt = runtime.consume_closed_callback_accounting(checkpoint, handle)
    assert (receipt.semantic_steps, receipt.entered, receipt.completed) == (7, True, True)
    with pytest.raises(CallbackExportError):
        runtime.consume_closed_callback_accounting(checkpoint, handle)
    unused = runtime.begin_closed_callback_accounting(handle)
    receipt = runtime.consume_closed_callback_accounting(unused, handle)
    assert (receipt.semantic_steps, receipt.entered, receipt.completed) == (0, False, False)


@pytest.mark.parametrize("stage", ("before", "after"))
@pytest.mark.parametrize("action", ("execute", "evaluate", "allot", "define"))
def test_checkpoint_excludes_unrelated_work_until_consumed(runtime, stage, action):
    handle = clamp(runtime)
    checkpoint = runtime.begin_closed_callback_accounting(handle)
    if stage == "after":
        runtime.invoke_callback_export(handle, (4, 2, 7))
    before = runtime.diagnostics.semantic_cycles
    actions = {
        "execute": lambda: runtime.execute("ABS"),
        "evaluate": lambda: runtime.evaluate(b"1 ABS"),
        "allot": lambda: runtime.dictionary.allot(1),
        "define": lambda: runtime.define_colon("EXCLUDED", (Return(),)),
    }
    with pytest.raises(CallbackExportError):
        actions[action]()
    assert runtime.diagnostics.semantic_cycles == before
    receipt = runtime.consume_closed_callback_accounting(checkpoint, handle)
    assert receipt.semantic_steps == (7 if stage == "after" else 0)
    assert runtime.find("EXCLUDED") is None


def test_receipt_rejects_changed_engine_consumption_route_before_call(runtime, monkeypatch):
    from simulator.interop_exports import ClosedCallbackReceipt
    handle = clamp(runtime)
    checkpoint = runtime.begin_closed_callback_accounting(handle)
    runtime.invoke_callback_export(handle, (4, 2, 7))
    calls = []
    def forge(*args):
        calls.append(True)
        return ClosedCallbackReceipt(0, True, True)
    with monkeypatch.context() as patch:
        # Restore absence from the instance namespace, not a bound-method
        # shadow of the canonical class route.
        patch.setitem(vars(runtime._callback_exports), "consume_closed_accounting", forge)
        with pytest.raises(CallbackExportError):
            runtime.consume_closed_callback_accounting(checkpoint, handle)
    assert calls == []
    assert runtime.consume_closed_callback_accounting(checkpoint, handle).semantic_steps == 7


def test_policy_core_lookup_rejects_replaced_metadata_route_before_getter(runtime, monkeypatch):
    from simulator.dictionary import Word
    calls = []
    monkeypatch.setattr(Word, "xt", property(lambda instance: calls.append(True)))
    with pytest.raises(CallbackExportError):
        runtime.callback_policy_core_xt("ABS")
    assert calls == []
