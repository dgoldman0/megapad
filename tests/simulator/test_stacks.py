"""Focused ordering tests for the hosted MegaForth stacks."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.memory import SparseAddressSpace
from simulator.stacks import (
    Continuation,
    DataStack,
    ReturnStack,
    ReturnStackShapeError,
    StackOverflow,
    StackPointerError,
    StackUnderflow,
)


def test_data_stack_wraps_cells_and_snapshots_bottom_to_top() -> None:
    stack = DataStack([-1])

    stack.push(1 << 64)

    assert stack.depth() == 2
    assert stack.peek() == 0
    assert stack.peek(1) == MASK64
    assert stack.snapshot() == (MASK64, 0)
    assert stack.pop() == 0
    assert stack.pop() == MASK64


@pytest.mark.parametrize(
    ("operation", "required"),
    [
        (lambda stack: stack.pop(), 1),
        (lambda stack: stack.peek(), 1),
        (lambda stack: stack.peek(2), 3),
    ],
)
def test_data_stack_underflow_reports_operation_shape(operation, required: int) -> None:
    stack = DataStack()

    with pytest.raises(StackUnderflow) as caught:
        operation(stack)

    assert caught.value.stack == "data"
    assert caught.value.required == required
    assert caught.value.available == 0


def test_stack_pair_methods_preserve_order_and_preflight_whole_pairs() -> None:
    data = DataStack([0xAA])

    data.push_pair(-1, 1 << 64)
    assert data.snapshot() == (0xAA, MASK64, 0)
    assert data.pop_pair("2>R") == (MASK64, 0)
    assert data.snapshot() == (0xAA,)

    with pytest.raises(StackUnderflow) as caught:
        data.pop_pair("2>R")
    assert caught.value.operation == "2>R"
    assert caught.value.required == 2
    assert caught.value.available == 1
    assert data.snapshot() == (0xAA,)

    memory = SparseAddressSpace(bank0_size=0x200)
    bounded = DataStack(
        [0xBB],
        memory=memory,
        floor=0xE8,
        empty_pointer=0xF8,
    )
    with pytest.raises(StackOverflow):
        bounded.push_pair(1, 2)
    assert bounded.snapshot() == (0xBB,)


def test_return_stack_pair_methods_accept_raw_do_cells_and_reject_continuations() -> None:
    stack = ReturnStack()
    stack.enter_do(limit=9, index=3)

    assert stack.peek_pair("2R@") == (9, 3)
    assert stack.snapshot() == (9, 3)
    assert stack.pop_pair("2R>") == (9, 3)
    assert stack.snapshot() == ()

    stack.push(0x11)
    top_continuation = stack.push_continuation(xt=0x100, ip=0x200)
    before = stack.snapshot()
    with pytest.raises(ReturnStackShapeError, match="2R>.*top user cell"):
        stack.pop_pair("2R>")
    assert stack.snapshot() == before
    assert stack.pop_continuation() == top_continuation
    assert stack.pop() == 0x11

    deeper_continuation = stack.push_continuation(xt=0x300, ip=0x400)
    stack.push(0x22)
    before = stack.snapshot()
    with pytest.raises(ReturnStackShapeError, match="2R@.*deeper user cell"):
        stack.peek_pair("2R@")
    assert stack.snapshot() == before
    assert stack.pop() == 0x22
    assert stack.pop_continuation() == deeper_continuation


def test_backed_return_stack_pair_capacity_failure_is_atomic() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0xE8,
        empty_pointer=0xF8,
    )
    stack.push(0xCC)

    with pytest.raises(StackOverflow):
        stack.push_pair(1, 2)

    assert stack.snapshot() == (0xCC,)


def test_user_return_cell_temporarily_sits_above_loop_index() -> None:
    stack = ReturnStack()
    stack.enter_do(limit=9, index=3)

    assert stack.peek() == 3
    stack.push(-1)
    assert stack.peek() == MASK64
    assert stack.pop() == MASK64
    assert stack.peek() == 3
    assert stack.snapshot() == (9, 3)


def test_continuation_is_an_ordered_entry_and_blocks_loop_lookup() -> None:
    stack = ReturnStack()
    stack.enter_do(limit=9, index=3)
    continuation = stack.push_continuation(xt=-1, ip=1 << 64, root=True)

    assert continuation == Continuation(xt=MASK64, ip=0, root=True)
    assert stack.snapshot() == (9, 3, continuation)
    with pytest.raises(ReturnStackShapeError, match="I.*continuation"):
        stack.i()

    assert stack.pop_continuation() == continuation
    assert stack.i() == 3


def test_user_return_operations_do_not_consume_continuations() -> None:
    stack = ReturnStack()
    continuation = stack.push_continuation(xt=1, ip=2)

    with pytest.raises(ReturnStackShapeError, match="R@.*continuation"):
        stack.peek()
    with pytest.raises(ReturnStackShapeError, match="R>.*continuation"):
        stack.pop()

    assert stack.pop_continuation() == continuation
    assert stack.depth() == 0


def test_nested_loop_indices_have_fixed_positions() -> None:
    stack = ReturnStack()
    stack.enter_do(limit=20, index=10)
    stack.enter_do(limit=8, index=4)

    assert stack.i() == 4
    assert stack.j() == 10

    stack.unloop()
    assert stack.snapshot() == (20, 10)
    assert stack.i() == 10


def test_loop_wraps_index_and_removes_only_terminated_frame() -> None:
    stack = ReturnStack()
    stack.push(0xA5)
    stack.enter_do(limit=0, index=MASK64 - 1)

    assert stack.loop() is True
    assert stack.i() == MASK64
    assert stack.loop() is False
    assert stack.snapshot() == (0xA5,)


def test_loop_shape_failure_does_not_search_or_mutate() -> None:
    stack = ReturnStack()
    stack.enter_do(limit=4, index=1)
    continuation = stack.push_continuation(xt=0x100, ip=0x200)
    before = stack.snapshot()

    with pytest.raises(ReturnStackShapeError, match="LOOP.*continuation"):
        stack.loop()

    assert stack.snapshot() == before
    assert stack.pop_continuation() == continuation


@pytest.mark.parametrize(
    ("operation", "required"),
    [
        (lambda stack: stack.pop(), 1),
        (lambda stack: stack.peek(), 1),
        (lambda stack: stack.pop_continuation(), 1),
        (lambda stack: stack.i(), 2),
        (lambda stack: stack.loop(), 2),
        (lambda stack: stack.unloop(), 2),
        (lambda stack: stack.j(), 4),
    ],
)
def test_return_stack_underflow_is_precise(operation, required: int) -> None:
    stack = ReturnStack()

    with pytest.raises(StackUnderflow) as caught:
        operation(stack)

    assert caught.value.stack == "return"
    assert caught.value.required == required
    assert caught.value.available == 0


def test_unloop_rejects_continuation_without_partial_removal() -> None:
    stack = ReturnStack()
    stack.push(7)
    continuation = stack.push_continuation(xt=8, ip=9)

    with pytest.raises(ReturnStackShapeError, match="UNLOOP.*continuation"):
        stack.unloop()

    assert stack.snapshot() == (7, continuation)


def test_return_stack_restores_an_exact_ordered_snapshot() -> None:
    stack = ReturnStack()
    stack.push(7)
    stack.enter_do(limit=4, index=1)
    checkpoint = stack.snapshot()

    stack.loop()
    stack.push_continuation(xt=8, ip=9)
    stack.restore(checkpoint)

    assert stack.snapshot() == (7, 4, 1)


def test_return_stack_restore_rejects_untrusted_entry_shapes() -> None:
    stack = ReturnStack()

    with pytest.raises(TypeError, match="snapshot must be a tuple"):
        stack.restore([])  # type: ignore[arg-type]
    with pytest.raises(TypeError, match="cells or continuations"):
        stack.restore((object(),))  # type: ignore[arg-type]


def test_backed_data_stack_uses_downward_little_endian_guest_cells() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = DataStack(
        [0x0102_0304_0506_0708, -1],
        memory=memory,
        floor=0x40,
        empty_pointer=0x100,
    )

    assert stack.backed
    assert stack.floor == 0x40
    assert stack.empty_pointer == 0x100
    assert stack.capacity == 24
    assert stack.pointer == 0xF0
    assert memory.read_bytes(0xF0, 16) == (
        bytes.fromhex("ffffffffffffffff")
        + bytes.fromhex("0807060504030201")
    )
    assert stack.snapshot() == (0x0102_0304_0506_0708, MASK64)


def test_backed_data_stack_reads_raw_mutations_and_retains_popped_bytes() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = DataStack(
        memory=memory,
        floor=0x40,
        empty_pointer=0x100,
    )
    stack.push(7)
    occupied = stack.pointer

    memory.write64(occupied, 9)
    assert stack.peek() == 9
    assert stack.pop() == 9
    assert stack.pointer == 0x100
    assert memory.read64(occupied) == 9

    stack.set_pointer(occupied)
    assert stack.snapshot() == (9,)


def test_sp_store_restores_pointer_from_tos_without_normally_popping_it() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = DataStack(
        memory=memory,
        floor=0x40,
        empty_pointer=0x100,
    )
    stack.push(0xAA)
    saved_pointer = stack.pointer
    stack.push(0xBB)
    stack.push(saved_pointer)
    sp_store_slot = stack.pointer

    stack.restore_from_top()

    assert stack.pointer == saved_pointer
    assert stack.snapshot() == (0xAA,)
    assert memory.read64(sp_store_slot) == saved_pointer


@pytest.mark.parametrize("stack_type", [DataStack, ReturnStack])
def test_backed_stack_pointer_bounds_alignment_overflow_and_clear(stack_type) -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = stack_type(
        memory=memory,
        floor=0xE0,
        empty_pointer=0x100,
    )

    for value in range(4):
        stack.push(value)
    retained = stack.pointer
    assert retained == 0xE0
    with pytest.raises(StackOverflow):
        stack.push(4)

    stack.clear()
    assert stack.pointer == 0x100
    assert stack.depth() == 0
    stack.set_pointer(retained)
    assert stack.snapshot() == (0, 1, 2, 3)

    for invalid in (0xD8, 0x101, 0x108):
        with pytest.raises(StackPointerError):
            stack.set_pointer(invalid)
    assert stack.pointer == retained


def test_backed_return_stack_recovers_typed_continuation_after_rp_restore() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0x100,
        empty_pointer=0x180,
    )
    stack.push(7)
    continuation = stack.push_continuation(xt=0x1234, ip=5, root=True)
    saved_pointer = stack.pointer

    raw_cookie = memory.read64(saved_pointer)
    assert raw_cookie != continuation.xt
    assert stack.pop_continuation() == continuation
    assert stack.snapshot() == (7,)

    stack.set_pointer(saved_pointer)
    assert stack.snapshot() == (7, continuation)
    assert stack.pop_continuation() == continuation
    assert stack.pop() == 7


def test_raw_overwrite_invalidates_retained_continuation_metadata() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0x100,
        empty_pointer=0x180,
    )
    stack.push_continuation(xt=0x1234, ip=5)
    address = stack.pointer

    memory.write64(address, 0xBEEF)

    assert stack.peek() == 0xBEEF
    with pytest.raises(ReturnStackShapeError, match="return.*user cell"):
        stack.pop_continuation()
    assert stack.pop() == 0xBEEF


def test_writing_the_semantic_xt_does_not_preserve_continuation_metadata() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0x100,
        empty_pointer=0x180,
    )
    continuation = stack.push_continuation(xt=0x1234, ip=5)
    address = stack.pointer

    memory.write64(address, continuation.xt)

    assert stack.peek() == continuation.xt
    with pytest.raises(ReturnStackShapeError, match="return.*user cell"):
        stack.pop_continuation()


def test_rp_capture_registration_survives_until_host_checkpoint_restore() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0x100,
        empty_pointer=0x180,
    )
    stack.push_continuation(xt=0x10, ip=0, root=True)
    stack.push(0xAA)
    checkpoint = stack.pointer_capture_checkpoint()
    captured = stack.capture_pointer()
    stack.push_continuation(xt=0x20, ip=1)

    stack.set_pointer(captured)
    assert stack.pop() == 0xAA
    assert stack.has_pointer_captures_after(checkpoint)

    nested_checkpoint = stack.pointer_capture_checkpoint()
    stack.capture_pointer()
    assert stack.has_pointer_captures_after(nested_checkpoint)
    stack.restore_pointer_captures(nested_checkpoint)
    assert not stack.has_pointer_captures_after(nested_checkpoint)
    assert stack.has_pointer_captures_after(checkpoint)

    stack.restore_pointer_captures(checkpoint)
    assert not stack.has_pointer_captures_after(checkpoint)


def test_backed_return_stack_host_restore_rewrites_mutated_loop_and_types() -> None:
    memory = SparseAddressSpace(bank0_size=0x200)
    stack = ReturnStack(
        memory=memory,
        floor=0x100,
        empty_pointer=0x180,
    )
    stack.push(7)
    stack.enter_do(limit=4, index=1)
    continuation = stack.push_continuation(xt=8, ip=9)
    checkpoint = stack.snapshot()

    assert checkpoint == (7, 4, 1, continuation)
    assert stack.pop_continuation() == continuation
    assert stack.loop() is True
    stack.push(99)
    stack.restore(checkpoint)

    assert stack.snapshot() == checkpoint
    assert stack.pop_continuation() == continuation
    assert stack.i() == 1


def test_unbacked_stacks_reject_guest_pointer_operations() -> None:
    data = DataStack([1])
    returns = ReturnStack()

    with pytest.raises(RuntimeError, match="unbacked data stack"):
        _ = data.pointer
    with pytest.raises(RuntimeError, match="unbacked data stack"):
        data.restore_from_top()
    with pytest.raises(RuntimeError, match="unbacked return stack"):
        returns.set_pointer(0)
