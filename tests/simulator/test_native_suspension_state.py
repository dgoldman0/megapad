"""Suspension acceleration retains shared bytes and exact control authority."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace
from simulator.runtime import MegaForthRuntime, YieldedExecution
from simulator.stacks import Continuation, DataStack, ReturnStack


def test_snapshot_fallback_preserves_custom_and_unbacked_stack_behavior():
    class ObservedStack(DataStack):
        def peek(self, offset=0):
            return super().peek(offset) + 100

    runtime = MegaForthRuntime(execution_backend="native")
    stack = ObservedStack([1, 2], memory=runtime.memory,
                          floor=0x100, empty_pointer=0x200)
    assert runtime._native_execution.snapshot_stack(stack) is None
    assert runtime._stack_snapshot(stack) == (101, 102)
    for stack in (DataStack([3, 4]), ReturnStack(),
                  DataStack([5], memory=SparseAddressSpace(bank0_size=0x400),
                            floor=0x100, empty_pointer=0x200)):
        assert runtime._native_execution.snapshot_stack(stack) is None
        assert runtime._stack_snapshot(stack) == stack.snapshot()


def test_data_mutation_rejection_precedes_return_metadata_cleanup():
    runtime = MegaForthRuntime(execution_backend="native")
    runtime.evaluate(b": RUN 17 18 + ;")
    result = runtime.run_until_blocked("RUN", quantum_steps=1)
    assert isinstance(result, YieldedExecution)
    context = runtime.main_context
    slot = context.returns.pointer
    metadata = dict(context.returns._continuations)
    assert slot in metadata
    runtime.memory.write64(slot, 0xBEEF)
    runtime.memory.write64(context.data.pointer, 0xBEEF)
    with pytest.raises(ExecutionError, match="data stack changed"):
        runtime.resume_yielded(result.suspension)
    assert context.returns._continuations == metadata
    runtime.cancel_suspension(result.suspension)


def test_interned_callsite_values_keep_fresh_raw_cookies_and_invalidation():
    runtime = MegaForthRuntime(execution_backend="native")
    observations = []

    def observe(context):
        for slot, (frame, cookie) in context.returns._continuations.items():
            if context.returns.pointer <= slot < context.returns.empty_pointer:
                if not frame.root and frame.xt == runtime.dictionary.find("RUN").xt:
                    assert runtime.memory.read64(slot) == cookie
                    observations.append((frame, cookie))

    runtime.define_primitive("OBSERVE", observe)
    runtime.evaluate(b": INNER 1+ OBSERVE ; : RUN 0 4 0 DO INNER LOOP ;")
    runtime.execute("RUN", step_budget=100)
    assert runtime.main_context.data.snapshot() == (4,)
    assert len(observations) == 4
    first = observations[0][0]
    assert isinstance(first, Continuation)
    assert all(frame is first for frame, _ in observations)
    assert len({cookie for _, cookie in observations}) == 4
    # A dictionary generation change drops interned values with native plans.
    runtime.evaluate(b": NEW-WORD 17 ;")
    observations.clear()
    runtime.execute("RUN", step_budget=100)
    assert len(observations) == 4
    assert all(frame == first and frame is not first for frame, _ in observations)
    assert runtime.main_context.returns.snapshot() == ()
