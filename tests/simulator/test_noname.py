"""``:NONAME`` in the hosted simulator, on both executors.

Native BIOS ``:NONAME`` builds no dictionary header: it pushes ``HERE``,
which is the definition's execution token, compiles the body there, and ``;``
ends it.  The definition has no name, ``LATEST`` does not move, and the token
is good for as long as the code at it survives.  The simulator keeps such a
definition by its token and marks its code slot, so compiling over the slot
ends it.
"""

from __future__ import annotations

import pytest

from shared.cells import CELL_BYTES
from simulator.dictionary import HEADER_FIXED_BYTES
from simulator.errors import ExecutionError, SourceError
from simulator.runtime import MegaForthRuntime


def _backends() -> list[str]:
    try:
        import _megaforth_native  # noqa: F401
    except ImportError:
        return ["python"]
    return ["python", "native"]


@pytest.fixture(params=_backends())
def runtime(request: pytest.FixtureRequest) -> MegaForthRuntime:
    return MegaForthRuntime(execution_backend=request.param)


def _stack(runtime: MegaForthRuntime, source: bytes) -> tuple[int, ...]:
    runtime.evaluate(source, source_name="noname-test.f", step_budget=1_000_000)
    stack = runtime.main_context.data.snapshot()
    runtime.main_context.data.clear()
    assert runtime.main_context.returns.snapshot() == ()
    return stack


# KDOS's deferred words, which is how Akashic's Library applet installs the
# handlers it defines with :NONAME.
DEFER_SOURCE = (
    b": DEFER  ( \"name\" -- )  CREATE ['] ABORT , DOES> @ EXECUTE ;\n"
    b": IS     ( xt \"name\" -- )  ' >BODY ! ;\n"
)


def test_noname_pushes_here_and_adds_no_name(runtime: MegaForthRuntime) -> None:
    words_before = runtime.dictionary.words
    latest_before = runtime.dictionary.latest
    here_before = runtime.dictionary.here

    (token,) = _stack(runtime, b":NONAME 3 4 + ;")

    assert token == here_before
    # A code slot at the token and nothing else: no header, no link.
    assert runtime.dictionary.here == here_before + CELL_BYTES
    assert runtime.dictionary.latest == latest_before
    assert runtime.dictionary.words == words_before
    assert runtime.dictionary.live_word(token) is None
    assert runtime.find(b"<noname>") is None
    assert _stack(runtime, f"{token} EXECUTE {token} EXECUTE".encode()) == (7, 7)


def test_the_token_is_on_the_stack_while_the_body_compiles(
    runtime: MegaForthRuntime,
) -> None:
    here = runtime.dictionary.here
    # The body copies the token from the stack and compiles it as a literal,
    # across a line break, so the definition returns its own token.
    assert _stack(
        runtime,
        b":NONAME  ( -- xt )\n"
        b"    [ DUP ] LITERAL ;\n"
        b"DUP EXECUTE\n",
    ) == (here, here)


def test_a_handler_installed_through_a_deferred_word(
    runtime: MegaForthRuntime,
) -> None:
    assert _stack(
        runtime,
        DEFER_SOURCE
        + b": HANDLER-RUN  ( n xt -- n' )  EXECUTE 1+ ;\n"
        b": DOUBLE-BODY  ( n -- n' )  2* ;\n"
        b"DEFER STATUS-HANDLER\n"
        b":NONAME  ( n -- n' )\n"
        b"    ['] DOUBLE-BODY HANDLER-RUN ;\n"
        b"IS STATUS-HANDLER\n"
        b"20 STATUS-HANDLER\n",
    ) == (41,)


def test_tokens_in_a_table_run_from_a_compiled_loop(
    runtime: MegaForthRuntime,
) -> None:
    assert _stack(
        runtime,
        # The code of a :NONAME lies at HERE, so the three are compiled
        # before the table that holds their tokens is laid out.
        b":NONAME 1+ ;  :NONAME 2* ;  :NONAME 3 - ;\n"
        b"CREATE STEPS  ROT , SWAP , ,\n"
        b": WALK  ( n -- n' )  3 0 DO  STEPS I CELLS + @ EXECUTE  LOOP ;\n"
        b": ROUNDS  ( n count -- n' )  0 ?DO WALK LOOP ;\n"
        b"5 WALK  5 200 ROUNDS\n",
    ) == ((5 + 1) * 2 - 3, _rounds(5, 200))


def _rounds(value: int, count: int) -> int:
    for _ in range(count):
        value = ((value + 1) * 2 - 3) & (2**64 - 1)
    return value


def test_recurse_strings_and_calls_out_of_the_body(
    runtime: MegaForthRuntime,
) -> None:
    assert _stack(
        runtime,
        b": SQUARE  DUP * ;\n"
        b":NONAME  ( n -- n! )  DUP 1 > IF DUP 1- RECURSE * THEN ;\n"
        b"CONSTANT FACTORIAL\n"
        b"6 FACTORIAL EXECUTE\n"
        b":NONAME  7 SQUARE 1+ ; EXECUTE\n",
    ) == (720, 50)

    here = runtime.dictionary.here
    address, length = _stack(runtime, b':NONAME S" kept text" ; EXECUTE')
    assert runtime.memory.read_bytes(address, length) == b"kept text"
    # The literal lies in the definition's own body, after its code slot.
    assert address == here + CELL_BYTES
    assert runtime.dictionary.here == here + CELL_BYTES + len(b"kept text") + 1

    runtime.evaluate(b':NONAME ." printed" ; EXECUTE', source_name="print.f")
    assert runtime.drain_uart_output() == b"printed"


def test_a_definition_cannot_open_inside_another(
    runtime: MegaForthRuntime,
) -> None:
    with pytest.raises(SourceError, match="nested : is not supported"):
        runtime.evaluate(b": OUTER :NONAME 1 ; ;", source_name="nested.f")
    runtime.main_context.data.clear()

    fresh = MegaForthRuntime(execution_backend=runtime.execution_backend)
    with pytest.raises(SourceError, match="nested : is not supported"):
        fresh.evaluate(b":NONAME : INNER 1 ; ;", source_name="nested.f")

    unfinished = MegaForthRuntime(execution_backend=runtime.execution_backend)
    with pytest.raises(SourceError, match="no terminating ;"):
        unfinished.evaluate(b":NONAME 1 2 +", source_name="open.f")


def test_compiling_over_the_code_ends_the_token(
    runtime: MegaForthRuntime,
) -> None:
    here = runtime.dictionary.here
    (token,) = _stack(runtime, b":NONAME 11 ;")
    assert token == here
    assert _stack(runtime, f"{token} EXECUTE".encode()) == (11,)

    # Give the storage back and leave it untouched: the code is still
    # there, as the machine code would be, and still runs.
    runtime.dictionary.allot(-CELL_BYTES)
    assert runtime.dictionary.here == here
    assert _stack(runtime, f"{token} EXECUTE".encode()) == (11,)

    # A definition compiled over it ends it.
    _stack(runtime, b": OVERWRITER 22 ;")
    with pytest.raises((ExecutionError, KeyError)):
        runtime.evaluate(f"{token} EXECUTE".encode(), source_name="dead.f")
    runtime.main_context.data.clear()
    assert _stack(runtime, b"OVERWRITER") == (22,)


def test_the_mark_in_a_code_slot_is_the_definition_s_own(
    runtime: MegaForthRuntime,
) -> None:
    (first,) = _stack(runtime, b":NONAME 1 ;")
    (second,) = _stack(runtime, b":NONAME 2 ;")
    # The second definition's code slot copied over the first's does not
    # stand for the first.
    runtime.memory.write64(first, runtime.memory.read64(second))
    with pytest.raises((ExecutionError, KeyError)):
        runtime.evaluate(f"{first} EXECUTE".encode(), source_name="copied.f")
    runtime.main_context.data.clear()
    assert _stack(runtime, f"{second} EXECUTE".encode()) == (2,)


def test_a_dictionary_word_that_owns_the_token_wins(
    runtime: MegaForthRuntime,
) -> None:
    runtime.dictionary.allot(32)
    (token,) = _stack(runtime, b":NONAME 11 ;")
    mark = runtime.memory.read64(token)
    # A named word laid out so that its code slot is the anonymous one's.
    header = token - HEADER_FIXED_BYTES - len(b"SAME")
    runtime.dictionary.allot(header - runtime.dictionary.here)
    _stack(runtime, b": SAME 22 ;")
    same = runtime.find(b"SAME")
    assert same is not None and same.xt == token
    # Even with the old mark put back into the slot.
    runtime.memory.write64(token, mark)
    assert _stack(runtime, f"{token} EXECUTE SAME".encode()) == (22, 22)


def test_a_second_definition_at_the_same_token_replaces_the_first(
    runtime: MegaForthRuntime,
) -> None:
    (first,) = _stack(runtime, b":NONAME 1 ;")
    runtime.dictionary.allot(-CELL_BYTES)
    (second,) = _stack(runtime, b":NONAME 2 ;")
    assert second == first
    assert _stack(runtime, f"{second} EXECUTE".encode()) == (2,)


def test_storage_claimed_under_an_open_body_is_refused(
    runtime: MegaForthRuntime,
) -> None:
    # The token was promised as HERE; a body that moves HERE before ; has
    # put something else there.
    with pytest.raises(SourceError, match="moved HERE"):
        runtime.evaluate(b":NONAME [ 8 ALLOT ] 1 ;", source_name="moved.f")


def test_a_token_that_a_live_word_owns_is_refused(
    runtime: MegaForthRuntime,
) -> None:
    _stack(runtime, b": KEPT 1 ;")
    kept = runtime.find(b"KEPT")
    assert kept is not None
    # An unsafe rewind onto the live word's code slot.
    runtime.dictionary.allot(kept.xt - runtime.dictionary.here)
    with pytest.raises(SourceError, match="overlap a live execution token"):
        runtime.evaluate(b":NONAME 2 ;", source_name="overlap.f")
    assert runtime.main_context.data.snapshot() == ()
    assert _stack(runtime, b"KEPT") == (1,)


def test_a_defining_word_without_a_name_gives_its_children_their_action(
    runtime: MegaForthRuntime,
) -> None:
    assert _stack(
        runtime,
        b":NONAME  ( n \"name\" -- )  CREATE , DOES> @ 1+ ;\n"
        b"CONSTANT MAKER\n"
        b"5 MAKER EXECUTE FIVE  9 MAKER EXECUTE NINE\n"
        b": BOTH  FIVE NINE ;\n"
        b"BOTH FIVE\n",
    ) == (6, 10, 6)


def test_control_flow_and_the_return_stack_inside_the_body(
    runtime: MegaForthRuntime,
) -> None:
    assert _stack(
        runtime,
        b":NONAME  ( n -- sum )\n"
        b"    0 SWAP 10 0 DO\n"
        b"        SWAP I + SWAP  DUP I = IF DROP UNLOOP EXIT THEN\n"
        b"    LOOP DROP ;\n"
        b"DUP 5 SWAP EXECUTE  SWAP 99 SWAP EXECUTE\n"
        b":NONAME  ( a b -- b a+1 )  >R 1+ R> SWAP ;\n"
        b"3 9 ROT EXECUTE\n"
        b":NONAME  CASE 1 OF 10 ENDOF 2 OF 20 ENDOF 99 SWAP ENDCASE ;\n"
        b"DUP 2 SWAP EXECUTE  SWAP 7 SWAP EXECUTE\n",
    ) == (15, 45, 9, 4, 20, 99)


def test_definitions_compiled_over_leave_the_table(
    runtime: MegaForthRuntime,
) -> None:
    # A definition whose code slot has been written over leaves the table
    # when it is looked up, or at the next pass over a table that has doubled.
    for _ in range(100):
        (token,) = _stack(runtime, b":NONAME 1 ;")
        runtime.memory.write64(token, 0)
    assert len(runtime._anonymous_words) <= 3

    # Live definitions all stay.
    tokens = [_stack(runtime, b":NONAME 7 ;")[0] for _ in range(100)]
    assert all(token in runtime._anonymous_words for token in tokens)
    assert _stack(
        runtime, f"{tokens[0]} EXECUTE {tokens[-1]} EXECUTE".encode()
    ) == (7, 7)
