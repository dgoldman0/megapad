"""Bulk snapshots retain complete shared bytes and continuation identity."""
from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError
from simulator.memory import SparseAddressSpace
from simulator.runtime import MegaForthRuntime, YieldedExecution
from simulator.stacks import Continuation, DataStack, ReturnStack


@pytest.fixture(params=["reference", "native"])
def snapshot(request):
    if request.param == "reference":
        return lambda stack: stack.snapshot()
    extension = pytest.importorskip("_megaforth_native")

    def native(stack):
        memory = stack._memory
        program = extension.NativeProgram(
            [(region.spec.base, region.spec.size, region.pages)
             for region in memory._regions], memory.page_size, Continuation,
        )
        result = program.snapshot_stack(
            (stack.floor, stack.empty_pointer, stack.pointer),
            stack._continuations if type(stack) is ReturnStack else None,
        )
        assert result is not None
        return result
    return native


@pytest.mark.parametrize("page_size", [1, 4, 8, 16, 4096])
def test_data_snapshot_crosses_pages_preserves_order_and_observes_raw_writes(page_size, snapshot):
    memory = SparseAddressSpace(bank0_size=0x4000, page_size=page_size)
    expected = tuple((index * 0x0102030405060708) & MASK64 for index in range(600))
    stack = DataStack(expected, memory=memory, floor=0x100, empty_pointer=0x3008)
    saved = snapshot(stack)
    assert saved == expected
    before = memory.resident_page_count
    memory.write64(stack.empty_pointer - 8, MASK64)
    assert snapshot(stack) == (MASK64, *expected[1:])
    assert saved == expected
    assert memory.resident_page_count == before
    pointer = stack.pointer
    assert stack.pop() == expected[-1]
    assert snapshot(stack) == (MASK64, *expected[1:-1])
    stack.set_pointer(pointer)
    assert snapshot(stack) == (MASK64, *expected[1:])


def test_snapshots_read_sparse_zero_cells_without_materializing_pages(snapshot):
    memory = SparseAddressSpace(bank0_size=0x4000, page_size=32)
    for stack in (DataStack(memory=memory, floor=0x100, empty_pointer=0x200),
                  ReturnStack(memory=memory, floor=0x200, empty_pointer=0x300)):
        assert snapshot(stack) == ()
        stack.set_pointer(stack.floor)
        assert snapshot(stack) == (0,) * 32
        assert memory.resident_page_count == 0


@pytest.mark.parametrize("page_size", [1, 4, 16, 4096])
def test_return_snapshot_preserves_types_and_removes_only_active_stale_metadata(page_size, snapshot):
    memory = SparseAddressSpace(bank0_size=0x4000, page_size=page_size)
    stack = ReturnStack(memory=memory, floor=0x100, empty_pointer=0x3008)
    expected = []
    for index in range(600):
        if index % 100 == 0:
            expected.append(stack.push_continuation(
                xt=index + 1, ip=index, root=index == 0,
                dispatch_id=1000 if index == 0 else 0, fault_abort=index == 100,
            ))
        else:
            stack.push(index)
            expected.append(index)
    saved = snapshot(stack)
    assert saved == tuple(expected)
    # An inactive retained continuation must survive snapshots until RP!
    # restores it. A stale active cookie must be removed during the snapshot.
    extra = stack.push_continuation(xt=1234, ip=42)
    extra_slot = stack.pointer
    assert stack.pop_continuation() == extra
    active_slot = stack.empty_pointer - 8
    memory.write64(active_slot, 0xBEEF)
    assert snapshot(stack) == (0xBEEF, *expected[1:])
    assert active_slot not in stack._continuations
    assert extra_slot in stack._continuations
    assert isinstance(saved[0], Continuation)
    stack.set_pointer(extra_slot)
    assert snapshot(stack) == (0xBEEF, *expected[1:], extra)


def test_custom_data_stack_snapshot_keeps_its_overridden_peek():
    class ObservedStack(DataStack):
        def peek(self, offset=0):
            return super().peek(offset) + 100

    stack = ObservedStack([1, 2], memory=SparseAddressSpace(bank0_size=0x400),
                          floor=0x100, empty_pointer=0x200)
    assert stack.snapshot() == (101, 102)


@pytest.mark.parametrize("backend", ["python", "native"])
@pytest.mark.parametrize("stack_name", ["data", "returns"])
def test_suspended_execution_still_rejects_same_depth_raw_cell_mutation(backend, stack_name):
    if backend == "native":
        pytest.importorskip("_megaforth_native")
    runtime = MegaForthRuntime(execution_backend=backend)
    runtime.evaluate(b": RUN 17 18 + ;")
    result = runtime.run_until_blocked("RUN", quantum_steps=1)
    assert isinstance(result, YieldedExecution)
    stack = getattr(runtime.main_context, stack_name)
    runtime.memory.write64(stack.pointer, 0xBEEF)
    with pytest.raises(ExecutionError, match="stack changed"):
        runtime.resume_yielded(result.suspension)
    runtime.cancel_suspension(result.suspension)
