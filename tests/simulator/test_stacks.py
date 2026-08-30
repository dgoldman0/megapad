"""Focused ordering tests for the hosted MegaForth stacks."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.stacks import (
    Continuation,
    DataStack,
    ReturnStack,
    ReturnStackShapeError,
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
