"""CREATE ... DOES> children run in the native engine as they do in Python."""
from __future__ import annotations

import pytest

pytest.importorskip("_megaforth_native")

from simulator import native_execution
from simulator.stacks import DataStack, ReturnStack
from tests.simulator.test_native_direct_path import BOUND, _agree, _direct_steps, _engines


# Field accessors, a counter, a deferred word and a table, each a child of a
# defining word, used the way component state and dispatch tables use them.
DOES_SOURCE = b" ".join((
    b": FIELD: ( offset -- ) CREATE , DOES> ( base -- address ) @ + ;",
    b": COUNTER: ( start -- ) CREATE , DOES> ( -- n ) DUP @ 1 ROT +! ;",
    b": VECTOR: ( -- ) CREATE 0 , DOES> ( i*x -- j*x ) @ EXECUTE ;",
    b": TABLE: ( n -- ) CREATE CELLS ALLOT DOES> ( index -- address ) SWAP CELLS + ;",
    b"8 FIELD: >SECOND  24 FIELD: >FOURTH  100 COUNTER: TICKET  VECTOR: HOOK",
    b"4 TABLE: SLOTS  CREATE RECORD 64 ALLOT",
    b": TRIPLE ( n -- n' ) 3 * ;  ' TRIPLE ' HOOK >BODY !",
    b": RUN ( rounds -- sum )",
    b"  0 SWAP 0 ?DO",
    b"    I RECORD >SECOND !  I 2* RECORD >FOURTH !",
    b"    RECORD >SECOND @ RECORD >FOURTH @ + I 3 AND SLOTS !",
    b"    I 3 AND SLOTS @ HOOK + TICKET +",
    b"  LOOP ;",
))


def _expected(rounds: int) -> int:
    total = 0
    for index in range(rounds):
        total += (index + 2 * index) * 3 + 100 + index
    return total


@pytest.mark.parametrize("rounds", [0, 1, 7, 200])
def test_does_children_match_the_reference(rounds):
    runtimes = _engines(DOES_SOURCE)
    result = _agree(runtimes, "RUN", inputs=(rounds,), step_budget=BOUND,
                    require_native=rounds > 0)
    assert result["error"] is None
    assert result["data"] == (_expected(rounds),)


@pytest.mark.parametrize("dense", [False, True])
def test_does_children_run_without_leaving_the_engine(monkeypatch, dense):
    # A first run binds the vector's target, which a cold EXECUTE leaves to
    # Python once. After it, a whole run is one native entry with no stop in
    # Python, and every pass over the body takes the direct path alone.
    monkeypatch.setattr(native_execution, "UNQUANTIZED_NATIVE_INTERVAL_STEPS", 1 << 40)
    observed = []
    for rounds in (8, 16):
        runtime = _engines(DOES_SOURCE)[2 if dense else 1]
        runtime.main_context.data.push(1)
        runtime.execute("RUN", step_budget=BOUND)
        runtime.main_context.data.clear()
        before = dict(runtime.native_execution_stats)
        direct_before = _direct_steps(runtime)
        runtime.main_context.data.push(rounds)
        runtime.execute("RUN", step_budget=BOUND)
        stats = runtime.native_execution_stats
        assert stats["entries"] - before["entries"] == 1
        observed.append((stats["semantic_steps"] - before["semantic_steps"],
                         _direct_steps(runtime) - direct_before))
    (steps, direct), (more_steps, more_direct) = observed
    assert more_steps - steps > 400
    assert more_direct - direct == more_steps - steps


def test_every_step_budget_stops_a_does_call_where_the_reference_stops():
    runtimes = _engines(DOES_SOURCE)
    whole = _agree(runtimes, "RUN", inputs=(3,), step_budget=BOUND)
    assert whole["error"] is None
    total = whole["counted_steps"]
    for budget in range(1, total + 2):
        runtimes = _engines(DOES_SOURCE)
        try:
            _agree(runtimes, "RUN", inputs=(3,), step_budget=budget, require_native=False)
        except AssertionError as error:
            raise AssertionError(f"step budget {budget} of {total}: {error}") from error


@pytest.mark.parametrize("interval", [1, 2, 3, 4, 5, 7, 16, 64])
def test_does_calls_resume_across_native_intervals(monkeypatch, interval):
    monkeypatch.setattr(native_execution, "UNQUANTIZED_NATIVE_INTERVAL_STEPS", interval)
    runtimes = _engines(DOES_SOURCE)
    result = _agree(runtimes, "RUN", inputs=(5,), step_budget=BOUND)
    assert result["data"] == (_expected(5),)


def test_children_of_different_definers_keep_their_own_entry():
    runtimes = _engines(
        b": BUMPER: ( n -- ) CREATE , DOES> @ 1+ ; "
        b": DOUBLER: ( n -- ) CREATE , DOES> @ 2* ; "
        b"10 BUMPER: BUMPED  10 DOUBLER: DOUBLED  7 BUMPER: OTHER "
        b": RUN 0 6 0 DO BUMPED + DOUBLED + OTHER + LOOP ;"
    )
    result = _agree(runtimes, "RUN", step_budget=BOUND)
    assert result["error"] is None
    assert result["data"] == (6 * (11 + 20 + 8),)


@pytest.mark.parametrize("room", [0, 1, 2])
def test_a_does_call_without_room_leaves_the_fault_to_python(room):
    source = (b": FIELD: CREATE , DOES> @ + ; 8 FIELD: >SECOND "
              b": RUN NEGATE NEGATE >SECOND ;")
    for stack in ("data", "returns"):
        runtimes = _engines(source)
        for runtime in runtimes:
            context = runtime.main_context
            if stack == "data":
                empty = context.data.empty_pointer
                context.data = DataStack((40,), memory=runtime.memory,
                                         floor=empty - 8 * (1 + room), empty_pointer=empty)
            else:
                context.data.push(40)
                empty = context.returns.empty_pointer
                # The dispatcher's own root cell takes the first one.
                context.returns = ReturnStack(memory=runtime.memory,
                                              floor=empty - 8 * (1 + room),
                                              empty_pointer=empty)
        _agree(runtimes, "RUN", step_budget=BOUND, require_native=False)


def test_executing_a_child_by_token_still_matches():
    runtimes = _engines(
        b": FIELD: CREATE , DOES> @ + ; 8 FIELD: >SECOND "
        b": RUN 0 20 0 DO I ['] >SECOND EXECUTE + LOOP ;"
    )
    result = _agree(runtimes, "RUN", step_budget=BOUND)
    assert result["data"] == (sum(index + 8 for index in range(20)),)
