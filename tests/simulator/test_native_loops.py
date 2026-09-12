"""Counted native loops preserve ordered stacks, faults, and every tick."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from shared.cells import MASK64, TRUE
from simulator.errors import StepBudgetExceeded
from simulator.ir import Loop, PlusLoop, Return, Unloop
from simulator.runtime import YieldedExecution
from simulator.stacks import DataStack, ReturnStack, ReturnStackShapeError, StackOverflow
from tests.simulator.test_native_execution import _compare, _runtimes


@pytest.mark.parametrize(("source", "expected"), [
    (b": TWICE 2 * ; : RUN 0 3 0 DO 4 1 DO I J + TWICE + LOOP LOOP ;", (54,)),
    (b": RUN 7 0 0 ?DO 99 LOOP 3 3 DO I UNLOOP EXIT LOOP ;", (7, 3)),
    (b": RUN 0 2 DO I -1 +LOOP ;", (2, 1)),
    (b": RUN 2 -1 DO R@ LOOP ;", (MASK64, 0, 1)),
    (b": RUN 0x8000000000000002 0x7FFFFFFFFFFFFFFF DO I LOOP ;",
     (0x7FFFFFFFFFFFFFFF, 0x8000000000000000, 0x8000000000000001)),
    (b": RUN 2 0 DO 99 >R I R> DROP UNLOOP EXIT LOOP ;", (99,)),
    (b": RUN 0 9 0 DO I 3 = IF LEAVE THEN I + LOOP ;", (3,)),
    (b": RUN TRUE FALSE -1 CELLS -1 -1 UM* ;", (TRUE, 0, MASK64 - 7, 1, MASK64 - 1)),
])
def test_counted_loops_and_basic_words_stay_in_one_native_interval(source, expected):
    runtimes = _runtimes(source)
    before = runtimes[1].native_execution_stats["entries"]
    observed = _compare(runtimes, "RUN")
    assert observed["error"] is None
    assert observed["data"] == expected
    assert runtimes[1].native_execution_stats["entries"] - before == 1


@pytest.mark.parametrize("budget", range(1, 61))
def test_every_loop_budget_boundary_preserves_partial_effects(budget):
    runtimes = _runtimes(
        b": RUN 0 3 0 DO I + LOOP 4 0 ?DO I + 2 +LOOP TRUE FALSE ;"
    )
    _compare(runtimes, "RUN", step_budget=budget, require_native=False)


@pytest.mark.parametrize("increment", [0, 2, -2])
def test_plus_loop_does_not_end_on_crossing_or_zero_increment(increment):
    runtimes = _runtimes(
        f": RUN 0 3 0 DO I + {increment} +LOOP ;".encode()
    )
    observed = _compare(runtimes, "RUN", step_budget=61)
    assert observed["error"][0] is StepBudgetExceeded
    assert observed["counted_steps"] == 61


@pytest.mark.parametrize("word", [b"I", b"J"])
def test_loop_index_cannot_search_past_a_callee_continuation(word):
    runtimes = _runtimes(
        b": INDEX " + word + b" ; : RUN 2 0 DO 2 0 DO INDEX LOOP LOOP ;"
    )
    observed = _compare(runtimes, "RUN")
    assert observed["error"][0] is ReturnStackShapeError


@pytest.mark.parametrize("operation", [Loop(0), PlusLoop(0), Unloop()])
def test_loop_shape_fault_preserves_python_operand_consumption(operation):
    runtimes = _runtimes()
    for runtime in runtimes:
        runtime.define_colon("RUN", (operation, Return()))
    observed = _compare(runtimes, "RUN", inputs=(17,), require_native=False)
    assert observed["error"] is not None
    assert observed["data"] == (() if isinstance(operation, PlusLoop) else (17,))


@pytest.mark.parametrize("capacity", [1, 2])
@pytest.mark.parametrize("opener", [b"DO", b"?DO"])
def test_do_overflow_keeps_partial_limit_push(capacity, opener):
    runtimes = _runtimes(b": RUN 2 0 " + opener + b" I LOOP ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.returns.empty_pointer
        context.returns = ReturnStack(memory=runtime.memory,
                                     floor=empty - capacity * 8, empty_pointer=empty)
    observed = _compare(runtimes, "RUN")
    assert observed["error"][0] is StackOverflow
    assert observed["data"] == ()


def test_equal_question_do_does_not_require_return_capacity():
    runtimes = _runtimes(b": RUN 7 7 ?DO 99 LOOP 42 ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.returns.empty_pointer
        context.returns = ReturnStack(memory=runtime.memory,
                                     floor=empty - 8, empty_pointer=empty)
    observed = _compare(runtimes, "RUN")
    assert observed["error"] is None
    assert observed["data"] == (42,)


def test_index_output_overflow_keeps_the_loop_frame_and_backed_bytes():
    runtimes = _runtimes(b": RUN 2 0 DO 17 18 I DROP DROP DROP LOOP ;")
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.data.empty_pointer
        context.data = DataStack(memory=runtime.memory,
                                 floor=empty - 16, empty_pointer=empty)
    observed = _compare(runtimes, "RUN")
    assert observed["error"][0] is StackOverflow
    assert observed["data"] == (17, 18)


def test_loop_return_pointer_restore_observes_retained_cells():
    runtimes = _runtimes(b"VARIABLE SLOT : RUN 2 0 DO RP@ SLOT ! LOOP ;")
    observed = _compare(runtimes, "RUN")
    assert observed["error"] is None
    restored = []
    for runtime in runtimes:
        slot = runtime.memory.read64(runtime.find("SLOT").body_address)
        runtime.main_context.returns.set_pointer(slot)
        restored.append(runtime.main_context.returns.snapshot())
    assert restored[0] == restored[1]


@pytest.mark.parametrize("word", ["TRUE", "FALSE", "CELLS", "UM*", "I", "J"])
def test_new_same_named_callbacks_keep_their_python_behavior(word):
    runtimes = _runtimes()
    for runtime in runtimes:
        runtime.define_primitive(word, lambda context: context.data.push(123))
        runtime.evaluate(f": RUN 1 DROP {word} ;".encode())
    assert _compare(runtimes, "RUN")["data"] == (123,)


@pytest.mark.parametrize("quantum", [1, 2, 7, 19])
def test_native_loop_host_yields_preserve_every_observable_boundary(quantum):
    observations = []
    for runtime in _runtimes(b": RUN 0 4 0 DO I + LOOP ;"):
        result = runtime.run_until_blocked("RUN", quantum_steps=quantum)
        rows = []
        for _ in range(100):
            context = runtime.main_context
            rows.append((type(result), result.semantic_steps,
                         context.data.snapshot(), context.returns.snapshot(),
                         runtime.memory.read_bytes(context.returns.empty_pointer - 64, 64)))
            if not isinstance(result, YieldedExecution):
                break
            result = runtime.resume_yielded(result.suspension)
        else:
            pytest.fail("bounded loop did not finish")
        observations.append(rows)
    assert observations[0] == observations[1]
