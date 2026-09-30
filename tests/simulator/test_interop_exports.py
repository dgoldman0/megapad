"""Canonical callback admission, isolation and existing-meter ownership."""

from __future__ import annotations

from dataclasses import replace

import pytest

from shared.cells import MASK64
from shared.hybrid_abi import CallbackExportV2, MAX_CALLBACK_EXPORTS
from simulator import core_words
from simulator.errors import IllegalInstructionFault, StepBudgetExceeded
from simulator.interop_exports import CallbackExportError, verify_callback_export
from simulator.runtime import MegaForthRuntime, PrimitiveDefinition
from simulator.stacks import DataStack, StackOverflow


@pytest.fixture(params=("python", "native"))
def backend(request):
    if request.param == "native":
        pytest.importorskip("_megaforth_native")
    return request.param


@pytest.fixture
def runtime(backend):
    return MegaForthRuntime(execution_backend=backend)


def descriptor(name="MIN", export_id=0):
    return CallbackExportV2(
        export_id=export_id, name=name,
        input_cells=1 if name == "ABS" else 2, output_cells=1,
    )


def caller_state(runtime):
    context = runtime.main_context
    return (
        context.data.snapshot(), context.returns.snapshot(),
        context.data.pointer, context.returns.pointer,
        runtime.memory.read_bytes(context.data.empty_pointer - 64, 64),
        runtime.memory.read_bytes(context.returns.empty_pointer - 64, 64),
        runtime.dictionary.here, runtime.dictionary.latest, runtime.uart_output,
    )


@pytest.mark.parametrize("name,arguments,outputs", (
    ("MIN", (MASK64, 7), (MASK64,)),
    ("MAX", (MASK64, 7), (7,)),
    ("ABS", (MASK64,), (1,)),
    ("ABS", (1 << 63,), (1 << 63,)),
    ("AND", (0xAA, 0x3C), (0x28,)),
    ("OR", (0xAA, 0x3C), (0xBE,)),
    ("XOR", (0xAA, 0x3C), (0x96,)),
))
def test_exports_preserve_caller_state_and_charge_one_operation(
    runtime, name, arguments, outputs,
):
    runtime.main_context.data.push(0x1234)
    runtime.main_context.returns.push(0x5678)
    before = caller_state(runtime)
    cycles = runtime.diagnostics.semantic_cycles
    native_steps = runtime.native_execution_stats["semantic_steps"]

    handle = runtime.bind_callback_export(descriptor(name))
    result = runtime.invoke_callback_export(handle, arguments)

    assert result.outputs == outputs
    assert result.semantic_steps == 1
    assert runtime.diagnostics.semantic_cycles - cycles == 1
    assert caller_state(runtime) == before
    # Top-level canonical primitives currently use the ordinary reference
    # dispatcher even in a native-selected runtime; do not claim native work.
    assert runtime.native_execution_stats["semantic_steps"] == native_steps


def test_fresh_private_context_has_real_eight_cell_stack_limits(runtime, monkeypatch):
    observed = []
    execute = runtime.execute

    def observe(word, **kwargs):
        observed.append(kwargs["context"])
        return execute(word, **kwargs)

    monkeypatch.setattr(runtime, "execute", observe)
    handle = runtime.bind_callback_export(descriptor("ABS"))
    for _ in range(2):
        assert runtime.invoke_callback_export(handle, (MASK64,)).outputs == (1,)

    assert observed[0] is not observed[1]
    for context in observed:
        assert context is not runtime.main_context
        assert type(context.data) is DataStack
        assert context.data.capacity == context.returns.capacity == 8
        assert context.data._memory is context.returns._memory
        assert context.data._memory is not runtime.memory
        for value in range(7):
            context.data.push(value)
        with pytest.raises(StackOverflow):
            context.data.push(8)


def test_nested_exports_share_the_outer_meter_without_extra_accounting(runtime):
    handle = runtime.bind_callback_export(descriptor("MAX"))
    results = []
    meters = []

    def bridge(_context):
        meter = runtime._active_dispatches[-1].meter
        meters.append(meter)
        results.append(runtime.invoke_callback_export(handle, (3, 9)))
        assert runtime._active_dispatches[-1].meter is meter

    runtime.define_primitive("EXPORT-BRIDGE", bridge)
    before = caller_state(runtime)
    cycles = runtime.diagnostics.semantic_cycles
    result = runtime.execute("EXPORT-BRIDGE", step_budget=2)

    assert result.semantic_steps == 2
    assert results[0].semantic_steps == 1
    assert results[0].outputs == (9,)
    assert meters[0].steps == 2
    assert runtime.diagnostics.semantic_cycles - cycles == 2
    assert caller_state(runtime) == before

    results.clear()
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises(StepBudgetExceeded):
        runtime.execute("EXPORT-BRIDGE", step_budget=1)
    assert results == []
    assert runtime.diagnostics.semantic_cycles - cycles == 1
    assert caller_state(runtime) == before
    assert runtime.invoke_callback_export(handle, (3, 9)).semantic_steps == 1


def test_binding_keeps_original_word_through_source_and_host_shadowing(runtime):
    original = runtime.find("MIN")
    first = runtime.bind_callback_export(descriptor())
    runtime.evaluate(b": MIN 999 ;")
    unexpected = []
    runtime.define_primitive("MIN", lambda context: unexpected.append(context))
    second = runtime.bind_callback_export(descriptor(export_id=1))

    assert runtime.find("MIN") is not original
    assert runtime.invoke_callback_export(first, (MASK64, 7)).outputs == (MASK64,)
    assert runtime.invoke_callback_export(second, (MASK64, 7)).outputs == (MASK64,)
    assert unexpected == []


def test_no_core_install_cannot_export_same_named_host_primitive(backend):
    runtime = MegaForthRuntime(execution_backend=backend, install_core_words=False)
    runtime.define_primitive("MIN", core_words._minimum)
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError, match="unavailable"):
        runtime.bind_callback_export(descriptor())
    assert runtime.diagnostics.semantic_cycles == cycles


def test_export_ids_are_bounded_and_identical_duplicates_are_idempotent(runtime):
    value = descriptor()
    first = runtime.bind_callback_export(value)
    assert runtime.bind_callback_export(replace(value)) is first
    for export_id in range(1, MAX_CALLBACK_EXPORTS):
        runtime.bind_callback_export(descriptor(export_id=export_id))
    assert len(runtime._callback_exports._exports) == 64
    with pytest.raises(CallbackExportError, match="conflicting"):
        runtime.bind_callback_export(descriptor("MAX"))
    forged = replace(value)
    object.__setattr__(forged, "export_id", 64)
    with pytest.raises(ValueError):
        runtime.bind_callback_export(forged)
    assert len(runtime._callback_exports._exports) == 64


def test_verifier_returns_only_independent_metadata(runtime):
    value = descriptor()
    handle = runtime.bind_callback_export(value)
    metadata = verify_callback_export(runtime, handle)
    assert metadata == value and metadata is not value
    assert not hasattr(metadata, "word") and not hasattr(handle, "word")
    object.__setattr__(metadata, "name", "MAX")
    assert runtime.verify_callback_export(handle).name == "MIN"
    assert runtime.invoke_callback_export(handle, (MASK64, 7)).outputs == (MASK64,)


def test_foreign_copied_and_numerical_handles_fail_before_effects(runtime, backend):
    handle = runtime.bind_callback_export(descriptor())
    foreign = MegaForthRuntime(execution_backend=backend)
    before = caller_state(runtime)
    cycles = runtime.diagnostics.semantic_cycles
    for invalid in (replace(handle), handle.export_id, runtime.find("MIN").xt):
        with pytest.raises((CallbackExportError, TypeError)):
            runtime.invoke_callback_export(invalid, (1, 2))
    with pytest.raises(CallbackExportError, match="different owner"):
        foreign.invoke_callback_export(handle, (1, 2))
    assert caller_state(runtime) == before
    assert runtime.diagnostics.semantic_cycles == cycles
    assert foreign.diagnostics.semantic_cycles == 0


@pytest.mark.parametrize("arguments", ([], (), (1,), (1, 2, 3), (True, 2), (-1, 2),
                                       (MASK64 + 1, 2), (1.0, 2)))
def test_argument_shape_fails_before_any_admitted_step(runtime, arguments):
    handle = runtime.bind_callback_export(descriptor())
    before = caller_state(runtime)
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises((CallbackExportError, TypeError)):
        runtime.invoke_callback_export(handle, arguments)
    assert runtime.diagnostics.semantic_cycles == cycles
    assert caller_state(runtime) == before


def test_forged_unbalanced_descriptor_is_revalidated_before_publication(runtime):
    value = descriptor()
    object.__setattr__(value, "output_cells", 2)
    with pytest.raises(ValueError, match="arity"):
        runtime.bind_callback_export(value)
    assert runtime._callback_exports._exports == {}
    assert runtime.diagnostics.semantic_cycles == 0


@pytest.mark.parametrize("reuse_xt", (False, True))
def test_rollback_and_exact_xt_reuse_revoke_original_export(backend, monkeypatch, reuse_xt):
    checkpoints = []
    define = MegaForthRuntime.define_primitive

    def capture(self, name, callback, **kwargs):
        if name == b"MIN":
            checkpoints.append(self.dictionary.checkpoint())
        return define(self, name, callback, **kwargs)

    with monkeypatch.context() as capture_patch:
        capture_patch.setattr(MegaForthRuntime, "define_primitive", capture)
        runtime = MegaForthRuntime(execution_backend=backend)
    original = runtime.find("MIN")
    handle = runtime.bind_callback_export(descriptor())
    runtime.dictionary.rollback(checkpoints[0])
    if reuse_xt:
        replacement = runtime.define_primitive("MIN", core_words._minimum)
        assert replacement.xt == original.xt
        assert replacement is not original
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError):
        runtime.verify_callback_export(handle)
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (1, 2))
    assert runtime.diagnostics.semantic_cycles == cycles


@pytest.mark.parametrize("part", ("implementation", "callback"))
def test_replaced_primitive_authority_is_rejected_before_effects(runtime, part):
    word = runtime.find("MIN")
    handle = runtime.bind_callback_export(descriptor())
    unexpected = []
    callback = lambda context: unexpected.append(context)
    if part == "implementation":
        object.__setattr__(word, "implementation", PrimitiveDefinition(callback))
    else:
        object.__setattr__(word.implementation, "callback", callback)
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError, match="implementation"):
        runtime.invoke_callback_export(handle, (1, 2))
    assert runtime.diagnostics.semantic_cycles == cycles
    assert unexpected == []


@pytest.mark.parametrize("part", (
    "implementation", "callback", "stack", "backing", "stack_class",
    "engine_none", "engine_foreign",
))
def test_accounting_hook_cannot_replace_callback_authority_after_tick(
    runtime, monkeypatch, part,
):
    word = runtime.find("MIN")
    handle = runtime.bind_callback_export(descriptor())
    unexpected = []
    callback = lambda context: unexpected.append(context)
    account = runtime._account_semantic_step
    foreign = (
        MegaForthRuntime(execution_backend=runtime.execution_backend)
        if part == "engine_foreign" else None
    )
    # A redirected private stack would still have the expected input values;
    # snapshot equality alone must not authorize writes into caller storage.
    runtime.main_context.data.push(1)
    runtime.main_context.data.push(2)

    class ReplacedDataStack(DataStack):
        __slots__ = ()

    def mutate_after_tick():
        account()
        if part == "implementation":
            object.__setattr__(word, "implementation", PrimitiveDefinition(callback))
        elif part == "callback":
            object.__setattr__(word.implementation, "callback", callback)
        elif part == "stack":
            runtime._active_dispatches[-1].context.data = runtime.main_context.data
        elif part == "backing":
            private = runtime._active_dispatches[-1].context.data
            private.__dict__.update(runtime.main_context.data.__dict__)
        elif part == "stack_class":
            runtime._active_dispatches[-1].context.data.__class__ = ReplacedDataStack
        else:
            object.__setattr__(word.implementation, "callback", callback)
            runtime._callback_exports = (
                None if foreign is None else foreign._callback_exports
            )

    monkeypatch.setattr(runtime, "_account_semantic_step", mutate_after_tick)
    before = caller_state(runtime)
    cycles = runtime.diagnostics.semantic_cycles
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (1, 2))
    assert runtime.diagnostics.semantic_cycles - cycles == 1
    assert unexpected == []
    assert caller_state(runtime) == before


@pytest.mark.parametrize("unbalanced", ("data", "returns"))
def test_unbalanced_completion_cannot_publish_outputs(runtime, monkeypatch, unbalanced):
    handle = runtime.bind_callback_export(descriptor())
    execute = runtime.execute

    def corrupt(word, **kwargs):
        result = execute(word, **kwargs)
        getattr(kwargs["context"], unbalanced).push(7)
        return result

    monkeypatch.setattr(runtime, "execute", corrupt)
    before = caller_state(runtime)
    with pytest.raises(CallbackExportError):
        runtime.invoke_callback_export(handle, (1, 2))
    assert caller_state(runtime) == before
    assert runtime.diagnostics.semantic_cycles == 1


def test_unexpected_instruction_fault_never_enters_guest_fault_callback(runtime, monkeypatch):
    handle = runtime.bind_callback_export(descriptor())
    callbacks = []
    runtime._fault_xt = runtime.define_primitive(
        "FAULT-CALLBACK", lambda context: callbacks.append(context)
    ).xt

    def fail_pop(_self):
        raise IllegalInstructionFault("unexpected private stack fault")

    monkeypatch.setattr(DataStack, "pop", fail_pop)
    before = caller_state(runtime)
    with pytest.raises(IllegalInstructionFault, match="private stack"):
        runtime.invoke_callback_export(handle, (1, 2))
    assert callbacks == []
    assert runtime.diagnostics.semantic_cycles == 1
    assert caller_state(runtime) == before
