"""Focused compiler and dispatch tests for indefinite source loops."""

from __future__ import annotations

import pytest

from simulator.errors import SourceError, StepBudgetExceeded
from simulator.ir import Branch, BranchZero
from simulator.runtime import ColonDefinition, MegaForthRuntime


def _colon(runtime: MegaForthRuntime, name: str) -> ColonDefinition:
    word = runtime.find(name)
    assert word is not None
    assert isinstance(word.implementation, ColonDefinition)
    return word.implementation


def test_begin_until_compiles_a_backward_zero_branch_and_terminates() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": DOWN BEGIN 1- DUP 0= UNTIL ;")

    definition = _colon(runtime, "DOWN")
    assert isinstance(definition.operations[-2], BranchZero)
    assert definition.operations[-2].target == 0

    context = runtime.main_context
    context.data.push(3)
    runtime.execute("DOWN")

    assert context.data.snapshot() == (0,)
    assert context.returns.snapshot() == ()


def test_begin_again_compiles_an_absolute_back_edge_and_obeys_budget() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FOREVER BEGIN AGAIN ;")

    definition = _colon(runtime, "FOREVER")
    assert isinstance(definition.operations[0], Branch)
    assert definition.operations[0].target == 0

    with pytest.raises(StepBudgetExceeded):
        runtime.execute("FOREVER", step_budget=8)

    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable


def test_begin_while_repeat_resolves_its_exit_after_the_back_edge() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": DOWN-WHILE BEGIN DUP WHILE 1- REPEAT ;")

    definition = _colon(runtime, "DOWN-WHILE")
    zero_branch = next(
        operation
        for operation in definition.operations
        if isinstance(operation, BranchZero)
    )
    back_edge = next(
        operation
        for operation in definition.operations
        if isinstance(operation, Branch)
    )
    assert zero_branch.target == len(definition.operations) - 1
    assert back_edge.target == 0

    context = runtime.main_context
    context.data.push(4)
    runtime.execute("DOWN-WHILE")

    assert context.data.snapshot() == (0,)
    assert context.returns.snapshot() == ()


def test_indefinite_frames_nest_with_if_do_and_each_other() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": MIXED-BEGIN "
        b"  BEGIN "
        b"    DUP IF 2 0 DO 1- LOOP THEN "
        b"    DUP 0= "
        b"  UNTIL ; "
        b": NESTED-BEGIN "
        b"  BEGIN 2 BEGIN 1- DUP 0= UNTIL DROP 1- DUP 0= UNTIL ;"
    )

    context = runtime.main_context
    context.data.push(4)
    runtime.execute("MIXED-BEGIN")
    assert context.data.snapshot() == (0,)
    assert context.returns.snapshot() == ()

    context.data.clear()
    context.data.push(2)
    runtime.execute("NESTED-BEGIN")
    assert context.data.snapshot() == (0,)
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize("token", [b"BEGIN", b"UNTIL", b"AGAIN", b"WHILE", b"REPEAT"])
def test_indefinite_control_words_are_compile_only(token: bytes) -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="compile-only"):
        runtime.evaluate(token, source_name="loops.f")


@pytest.mark.parametrize(
    ("source", "message"),
    [
        (b": BAD UNTIL ;", "UNTIL has no matching BEGIN"),
        (b": BAD AGAIN ;", "AGAIN has no matching BEGIN"),
        (b": BAD WHILE ;", "WHILE has no matching BEGIN"),
        (b": BAD BEGIN REPEAT ;", "REPEAT has no matching WHILE"),
        (b": BAD BEGIN IF REPEAT THEN ;", "REPEAT has no matching WHILE"),
    ],
)
def test_mismatched_indefinite_control_is_rejected_without_publication(
    source: bytes,
    message: str,
) -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match=message):
        runtime.evaluate(source, source_name="loops.f")

    assert runtime.find("BAD") is None


def test_unclosed_begin_or_while_is_not_published() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="unresolved control flow"):
        runtime.evaluate(b": OPEN-BEGIN BEGIN 1 ;", source_name="loops.f")
    with pytest.raises(SourceError, match="unresolved control flow"):
        runtime.evaluate(b": OPEN-WHILE BEGIN 1 WHILE 2 ;", source_name="loops.f")

    assert runtime.find("OPEN-BEGIN") is None
    assert runtime.find("OPEN-WHILE") is None
