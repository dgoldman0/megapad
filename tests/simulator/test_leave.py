"""Focused compiler and dispatch tests for counted-loop ``LEAVE``."""

from __future__ import annotations

import pytest

from simulator.errors import SourceError
from simulator.ir import Branch, Loop, QuestionDo, Unloop
from simulator.runtime import ColonDefinition, MegaForthRuntime


def _colon(runtime: MegaForthRuntime, name: str) -> ColonDefinition:
    word = runtime.find(name)
    assert word is not None
    assert isinstance(word.implementation, ColonDefinition)
    return word.implementation


def test_leave_inside_if_exits_the_innermost_counted_loop() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": INNER-ONLY "
        b"  0 3 0 DO "
        b"    5 0 DO I 2 = IF LEAVE THEN 1+ LOOP "
        b"    10 + "
        b"  LOOP ;"
    )

    runtime.execute("INNER-ONLY")

    assert runtime.main_context.data.snapshot() == (36,)
    assert runtime.main_context.returns.snapshot() == ()


def test_loop_patches_every_leave_after_its_back_edge() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": MANY-EXITS "
        b"  0 8 0 DO "
        b"    I 1 = IF LEAVE THEN "
        b"    I 3 = IF LEAVE THEN "
        b"    1+ "
        b"  LOOP ;"
    )

    definition = _colon(runtime, "MANY-EXITS")
    loop_index = next(
        index
        for index, operation in enumerate(definition.operations)
        if isinstance(operation, Loop)
    )
    leave_indices = [
        index
        for index, operation in enumerate(definition.operations[:-1])
        if isinstance(operation, Unloop)
        and isinstance(definition.operations[index + 1], Branch)
    ]

    assert len(leave_indices) == 2
    assert all(
        definition.operations[index + 1].target == loop_index + 1
        for index in leave_indices
    )

    runtime.execute("MANY-EXITS")
    assert runtime.main_context.data.snapshot() == (1,)
    assert runtime.main_context.returns.snapshot() == ()


def test_question_do_and_leave_share_the_post_loop_exit() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": MAYBE-LEAVE 0 4 0 ?DO I 0 = IF LEAVE THEN 1+ LOOP ; "
        b": ZERO-TRIP 5 5 ?DO LEAVE LOOP 77 ;"
    )

    definition = _colon(runtime, "MAYBE-LEAVE")
    question_do = next(
        operation
        for operation in definition.operations
        if isinstance(operation, QuestionDo)
    )
    leave_branch = next(
        definition.operations[index + 1]
        for index, operation in enumerate(definition.operations[:-1])
        if isinstance(operation, Unloop)
    )
    assert isinstance(leave_branch, Branch)
    assert question_do.target == leave_branch.target

    runtime.execute("MAYBE-LEAVE")
    runtime.execute("ZERO-TRIP")
    assert runtime.main_context.data.snapshot() == (0, 77)
    assert runtime.main_context.returns.snapshot() == ()


def test_leave_is_compile_only_and_requires_a_current_do() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="compile-only"):
        runtime.evaluate(b"LEAVE", source_name="leave.f")
    with pytest.raises(SourceError, match="LEAVE has no matching DO"):
        runtime.evaluate(
            b": OUTSIDE IF LEAVE THEN ;",
            source_name="leave.f",
        )
    with pytest.raises(SourceError, match="LEAVE has no matching DO"):
        runtime.evaluate(
            b": AFTER 1 0 DO LOOP LEAVE ;",
            source_name="leave.f",
        )

    assert runtime.find("OUTSIDE") is None
    assert runtime.find("AFTER") is None
