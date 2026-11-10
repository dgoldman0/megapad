"""The native engine's direct path computes exactly what the reference does."""
from __future__ import annotations

import random

import pytest

pytest.importorskip("_megaforth_native")

from simulator import native_execution
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.stacks import DataStack, ReturnStack
from tests.simulator.test_native_execution import _observe


# No program here needs a hundredth of this. A fault that corrupts a loop
# then ends in a step-budget error the comparison reports, not in a hang.
BOUND = 1_000_000


def _engines(source: bytes = b""):
    """The Python reference, then the native engine on sparse pages and on
    dense memory. The direct path sees one stack page and one ordinary page at
    a time in the first and a whole region in the second."""

    runtimes = []
    for backend, dense in (("python", False), ("native", False), ("native", True)):
        runtime = MegaForthRuntime(
            execution_backend=backend,
            memory=create_one_core_address_space(dense_backing=True if dense else None),
        )
        if source:
            runtime.evaluate(source, source_name="native-direct-path.f")
        runtimes.append(runtime)
    return runtimes


def _agree(runtimes, word, *, require_native=True, **kwargs):
    """Run ``word`` on every engine and require one outcome.

    Beyond what a run leaves in the stacks, memory, clocks and output, the
    return stack's next continuation cookie must agree: it decides the bytes
    of every later call.
    """

    observed = [_observe(runtime, word, **kwargs) for runtime in runtimes]
    reference, reference_native_steps = observed[0]
    assert reference_native_steps == 0
    for state, native_steps in observed[1:]:
        assert state == reference
        if require_native:
            assert native_steps > 0, "the executed source must reach native work"
    cookies = [runtime.main_context.returns._continuation_cookie for runtime in runtimes]
    assert cookies == [cookies[0]] * len(cookies)
    return reference


def _direct_steps(runtime):
    return runtime._native_execution.program.direct_steps


# The operations the direct path performs, in a body that leaves both stacks
# as it found them: plain and fused literals and constants, every shuffle,
# unary and binary word, division, every fetch and store width, the
# return-stack words singly and in pairs, counted loops with both indexes,
# calls, EXECUTE, early exits and a false ABORT".
FUSED_BINARY = (b"+", b"-", b"*", b"AND", b"OR", b"XOR", b"LSHIFT", b"RSHIFT",
                b"=", b"<>", b"U<", b"U>", b"<", b"<=", b">", b">=")
DIRECT_SOURCE = b" ".join((
    b"VARIABLE CELL-A  CREATE BYTES 32 ALLOT  7 VALUE SETTING  5 CONSTANT FIVE",
    b": LEAF ( n -- n' ) DUP 0= IF 1+ EXIT THEN 2* ;",
    b": SCAN ( -- n ) 5 0 DO I 2 = IF I UNLOOP EXIT THEN LOOP 99 ;",
    b": BODY ( -- )",
    b"  1 2 SWAP OVER NIP TUCK ROT -ROT 2DUP 2DROP DROP 2DROP",
    b"  TRUE FALSE ?DUP DROP ?DUP 2DROP  SP@ DROP",
    b"  9 NEGATE ABS 1+ 1- 2* 2/ CELLS CELL+ INVERT BSWAP 0= 0<> 0< 0> DROP",
    b" ".join(b"-6 3 %s DROP  -6 3 SWAP %s DROP  6 FIVE %s DROP" % (op, op, op)
              for op in FUSED_BINARY),
    b"  6 3 MIN 6 3 MAX 2DROP",
    b"  0x1122334455667788 CELL-A !  CELL-A @ DROP  CELL-A DUP @ 2DROP",
    b"  1 CELL-A +!  CELL-A OFF  CELL-A ON",
    b"  65 BYTES C!  0x4243 BYTES 1+ W!  0x44454647 BYTES 4 + L!",
    b"  BYTES C@ BYTES W@ BYTES L@ 2DROP DROP",
    b"  BYTES DUP C@ OVER 1+ W@ ROT 4 + L@ 2DROP DROP  BYTES COUNT 2DROP",
    b"  SETTING 1+ TO SETTING  S\" direct\" 2DROP  0 ABORT\" never\"",
    b"  11 >R R@ R> + DROP",
    b"  0 4 0 DO I + LOOP DROP  0 6 0 DO I + 2 +LOOP DROP  3 3 ?DO 99 LOOP",
    b"  SCAN DROP  0 LEAF LEAF DROP  0 ['] LEAF EXECUTE DROP",
    b"  1 2 3 4 2SWAP 2OVER 2DROP 2DROP 2DROP  7 8 1 PICK 2DROP DROP",
    b"  5 3 9 WITHIN -1 2 UM* 2DROP DROP  -7 2 / -7 2 MOD -7 2 /MOD 2DROP 2DROP",
    b"  21 22 2>R 2R@ 2R> 2DROP 2DROP  2 0 DO 2 0 DO J I + DROP LOOP LOOP",
    b"  7 DUP IF DROP THEN  0 DUP IF 99 THEN DROP  3 BEGIN DUP WHILE 1- REPEAT DROP ;",
    b": RUN ( rounds -- ) 0 ?DO BODY LOOP ;",
))
DIRECT_SPANS = (b"CELL-A", b"BYTES", b"SETTING")


def _spans(runtime):
    return tuple((runtime.find(name).body_address, 32) for name in DIRECT_SPANS)


def test_every_direct_operation_matches_the_reference():
    runtimes = _engines(DIRECT_SOURCE)
    result = _agree(runtimes, "RUN", inputs=(3,), spans=_spans(runtimes[0]),
                    step_budget=BOUND)
    assert result["error"] is None
    assert result["data"] == ()
    assert all(_direct_steps(runtime) > 0 for runtime in runtimes[1:])


def test_a_second_run_continues_where_the_first_left_the_machine():
    runtimes = _engines(DIRECT_SOURCE)
    for rounds in (2, 1, 3):
        result = _agree(runtimes, "RUN", inputs=(rounds,), spans=_spans(runtimes[0]),
                        step_budget=BOUND)
        assert result["error"] is None


@pytest.mark.parametrize("dense", [False, True])
def test_every_direct_operation_runs_on_the_direct_path(monkeypatch, dense):
    # In one native interval the first pass qualifies the stacks' pages, the
    # ordinary page and each call site. Every later pass must then run on the
    # direct path alone, so doubling the passes adds the same steps to both
    # counts.
    monkeypatch.setattr(native_execution, "UNQUANTIZED_NATIVE_INTERVAL_STEPS", 1 << 40)
    observed = []
    for rounds in (4, 8):
        runtime = MegaForthRuntime(
            execution_backend="native",
            memory=create_one_core_address_space(dense_backing=True if dense else None),
        )
        runtime.evaluate(DIRECT_SOURCE, source_name="native-direct-path.f")
        runtime.main_context.data.push(rounds)
        runtime.execute("RUN", step_budget=BOUND)
        stats = runtime.native_execution_stats
        assert stats["entries"] == 1
        observed.append((stats["semantic_steps"], _direct_steps(runtime)))
    (steps, direct), (more_steps, more_direct) = observed
    assert more_steps - steps > 1000
    assert more_direct - direct == more_steps - steps


def test_every_step_budget_stops_where_the_reference_stops():
    runtimes = _engines(DIRECT_SOURCE)
    whole = _agree(runtimes, "RUN", inputs=(1,), step_budget=BOUND)
    assert whole["error"] is None
    total = whole["counted_steps"]
    assert total > 600
    for budget in range(1, total + 2):
        runtimes = _engines(DIRECT_SOURCE)
        try:
            _agree(runtimes, "RUN", inputs=(1,), step_budget=budget,
                   spans=_spans(runtimes[0]), require_native=False)
        except AssertionError as error:
            raise AssertionError(f"step budget {budget} of {total}: {error}") from error


@pytest.mark.parametrize("interval", [1, 2, 3, 4, 5, 7, 16, 64, 257])
def test_short_native_intervals_resume_loops_and_calls_made_earlier(monkeypatch, interval):
    # Each interval starts with untyped return cells and unqualified pages, so
    # loops and returns begun in an earlier one take the general path first.
    monkeypatch.setattr(native_execution, "UNQUANTIZED_NATIVE_INTERVAL_STEPS", interval)
    runtimes = _engines(DIRECT_SOURCE)
    result = _agree(runtimes, "RUN", inputs=(2,), spans=_spans(runtimes[0]),
                    step_budget=BOUND)
    assert result["error"] is None
    assert result["data"] == ()


# Return cells this run did not type: the dispatcher's root continuation, and
# a call or a loop made in an earlier native interval. The direct path must
# decline them, so that the general path asks Python's dictionary. Each
# program first works the return stack itself, which qualifies the stack's
# page before the foreign cell is reached.
FOREIGN_CELL_SOURCES = (
    b": RUN 1 >R R> DROP %s ;",
    b": INNER 1 >R R> DROP %s ; : RUN 5 INNER ;",
    b": INNER 1 >R R> DROP %s ; : RUN 3 0 DO 5 INNER DROP LOOP ;",
    b": RUN 3 0 DO 1 >R R> DROP %s DROP LOOP ;",
)


@pytest.mark.parametrize("operation", [
    b"R@", b"R>", b"2R@", b"2R>", b"I", b"J", b"UNLOOP", b"EXIT",
    b"R> DROP", b"R> R>", b"2R> 2DROP",
])
@pytest.mark.parametrize("interval", [1, 2, 3, 4, 5, 6, 7, 8, 11, 1 << 40])
def test_return_cells_typed_elsewhere_keep_pythons_answer(monkeypatch, operation, interval):
    monkeypatch.setattr(native_execution, "UNQUANTIZED_NATIVE_INTERVAL_STEPS", interval)
    for source in FOREIGN_CELL_SOURCES:
        runtimes = _engines(source % operation)
        try:
            _agree(runtimes, "RUN", step_budget=10_000, require_native=False)
        except AssertionError as error:
            raise AssertionError(f"{(source % operation).decode()}: {error}") from error


def test_one_execute_site_follows_its_target_as_it_changes():
    # The direct path enters only the plan bound to the site. Each change of
    # target goes back to the general path, which binds the new one.
    runtimes = _engines(
        b": ONE 1 ; : TEN 10 ; : HUNDRED 100 ; "
        b"CREATE TARGETS ' ONE , ' TEN , ' HUNDRED , ' TEN , "
        b": RUN 0 40 0 DO I 3 AND CELLS TARGETS + @ EXECUTE + LOOP ;"
    )
    result = _agree(runtimes, "RUN", step_budget=BOUND)
    assert result["error"] is None
    assert result["data"] == (10 * (1 + 10 + 100 + 10),)


DIRECT_WORDS = (
    b"DUP", b"DROP", b"SWAP", b"OVER", b"NIP", b"TUCK", b"ROT", b"-ROT", b"2DUP",
    b"2DROP", b"?DUP", b"TRUE", b"FALSE", b"NEGATE", b"ABS", b"1+", b"1-", b"2*",
    b"2/", b"CELLS", b"CELL+", b"INVERT", b"BSWAP", b"0=", b"0<>", b"0<", b"0>",
    b"+", b"-", b"*", b"MIN", b"MAX", b"AND", b"OR", b"XOR", b"LSHIFT", b"RSHIFT",
    b"=", b"<>", b"U<", b"U>", b"<", b"<=", b">", b">=",
    b"2SWAP", b"2OVER", b"WITHIN", b"UM*", b"/", b"MOD", b"/MOD", b"PICK",
)
# Words only the general path performs, mixed in so that control passes
# between the two paths inside one interval.
GENERAL_WORDS = (b"CMOVE", b"FILL", b"COMPARE", b"MOVE")
RETURN_WORDS = (b">R", b"R>", b"R@", b"2>R", b"2R>", b"2R@", b"I", b"J", b"UNLOOP")
LITERALS = (0, 1, 2, 3, 7, 8, 63, 64, 65, 255, 256, -1, -2, -8,
            0x7FFFFFFFFFFFFFFF, -0x8000000000000000, 0x0123456789ABCDEF)


def _random_tokens(rng: random.Random, length: int, depth: int = 0) -> list[bytes]:
    tokens: list[bytes] = []
    while len(tokens) < length:
        choice = rng.random()
        if choice < 0.28:
            tokens.append(b"%d" % rng.choice(LITERALS))
        elif choice < 0.66:
            tokens.append(rng.choice(DIRECT_WORDS))
        elif choice < 0.70:
            first = [b"BUF", b"%d" % rng.randrange(0, 32), b"+"]
            second = [b"BUF", b"%d" % rng.randrange(0, 32), b"+"]
            span = b"%d" % rng.randrange(0, 33)
            word = rng.choice(GENERAL_WORDS)
            if word == b"FILL":
                tokens += [*first, span, b"%d" % rng.randrange(0, 256), word]
            elif word == b"COMPARE":
                tokens += [*first, span, *second, b"%d" % rng.randrange(0, 33), word]
            else:
                tokens += [*first, *second, span, word]
        elif choice < 0.76:
            offset = rng.randrange(0, 56)
            tokens += [b"BUF", b"%d" % offset, b"+",
                       rng.choice((b"@", b"C@", b"W@", b"L@", b"COUNT"))]
        elif choice < 0.82:
            offset = rng.randrange(0, 56)
            tokens += [b"BUF", b"%d" % offset, b"+",
                       rng.choice((b"!", b"C!", b"W!", b"L!", b"+!", b"OFF", b"ON"))]
        elif choice < 0.87:
            tokens.append(rng.choice(RETURN_WORDS))
        elif choice < 0.92:
            tokens.append(rng.choice((b"HELPER", b"['] HELPER EXECUTE", b"FIVE",
                                      b"SETTING", b"TO SETTING", b"SP@", b"RP@ DROP")))
        elif depth < 2 and choice < 0.96:
            tokens += [b"IF", *_random_tokens(rng, rng.randrange(0, 4), depth + 1),
                       b"ELSE", *_random_tokens(rng, rng.randrange(0, 4), depth + 1),
                       b"THEN"]
        elif depth < 2:
            tokens += [b"%d" % rng.randrange(0, 5), b"0", b"?DO",
                       *_random_tokens(rng, rng.randrange(0, 4), depth + 1),
                       rng.choice((b"I", b"")), b"LOOP"]
    return tokens


def test_random_programs_match_the_reference_whole_and_cut_short():
    rng = random.Random(20261110)
    prelude = (b"CREATE BUF 64 ALLOT  7 VALUE SETTING  5 CONSTANT FIVE "
               b": HELPER ( n -- n' ) DUP 0< IF NEGATE EXIT THEN 3 + ; ")
    for program in range(240):
        body = b" ".join(token for token in _random_tokens(rng, rng.randrange(4, 40))
                         if token)
        # A call and a push before the body qualify the return stack's page,
        # so that the body's return-stack words meet the direct path.
        source = prelude + b": RUN 0 HELPER >R R> DROP " + body + b" ;"
        inputs = tuple(rng.choice(LITERALS) & ((1 << 64) - 1)
                       for _ in range(rng.randrange(0, 5)))
        budgets = [BOUND, *(rng.randrange(1, 120) for _ in range(3))]
        for budget in budgets:
            runtimes = _engines(source)
            buffer = runtimes[0].find(b"BUF").body_address
            for runtime in runtimes:
                runtime.memory.write_bytes(buffer, bytes(range(1, 65)))
            try:
                _agree(runtimes, "RUN", inputs=inputs, step_budget=budget,
                       spans=((buffer, 64),), require_native=False)
            except AssertionError as error:
                raise AssertionError(
                    f"program {program}, budget {budget}, inputs {inputs}: "
                    f"{body.decode()}: {error}"
                ) from error


# One operation after the stack's page is qualified: the direct path must
# leave an operation it cannot fit to the general one, and that to Python,
# which raises the fault.
EDGE_OPERATIONS = (
    b"DUP", b"DROP", b"2DROP", b"SWAP", b"OVER", b"NIP", b"TUCK", b"ROT", b"-ROT",
    b"2DUP", b"?DUP", b"TRUE", b"SP@", b"NEGATE", b"0=", b"+", b"MAX", b"<",
    b"7", b"FIVE", b"7 +", b"FIVE *", b"SETTING", b"TO SETTING", b"S\" ab\"",
    b"BUF @", b"BUF C@", b"@", b"C@", b"COUNT", b"!", b"C!", b"+!", b"OFF",
    b">R R>", b">R R@ R> DROP", b"IF 1 THEN", b"DUP IF 1 THEN",
    b"0 ?DO I LOOP", b"3 0 DO I LOOP", b"HELPER", b"['] HELPER EXECUTE",
    b"2SWAP", b"2OVER", b"PICK", b"0 PICK", b"WITHIN", b"UM*", b"/MOD",
    b">R >R 2R@ 2R> 2DROP", b"2>R 2R>",
)


@pytest.mark.parametrize("operation", EDGE_OPERATIONS)
def test_stack_edges_leave_the_fault_to_python(operation):
    source = (b"CREATE BUF 64 ALLOT  7 VALUE SETTING  5 CONSTANT FIVE "
              b": HELPER ( n -- n' ) DUP 0< IF NEGATE EXIT THEN 3 + ; "
              b": RUN NEGATE NEGATE " + operation + b" ;")
    for held in range(1, 5):
        for slack in range(0, 4):
            runtimes = _engines(source)
            buffer = runtimes[0].find(b"BUF").body_address
            inputs = tuple([buffer + 8] * held)
            for runtime in runtimes:
                context = runtime.main_context
                empty = context.data.empty_pointer
                context.data = DataStack(
                    inputs, memory=runtime.memory,
                    floor=empty - 8 * (held + slack), empty_pointer=empty,
                )
            try:
                _agree(runtimes, "RUN", spans=((buffer, 64),), step_budget=BOUND,
                       require_native=False)
            except AssertionError as error:
                raise AssertionError(
                    f"{held} cells held, room for {slack} more: {error}"
                ) from error


@pytest.mark.parametrize("operation", [
    b"7 >R R> DROP", b"1 2 2 0 DO I + LOOP 2DROP", b"HELPER DROP",
    b"3 0 DO 5 HELPER DROP LOOP", b"7 >R 8 >R R> R> 2DROP",
    b"7 8 2>R 2R@ 2R> 2DROP 2DROP", b"2 0 DO 2 0 DO J DROP LOOP LOOP",
    b"5 ['] HELPER EXECUTE DROP",
])
@pytest.mark.parametrize("room", [0, 1, 2, 3])
def test_return_stack_edges_leave_the_fault_to_python(operation, room):
    source = (b": HELPER ( n -- n' ) DUP 0< IF NEGATE EXIT THEN 3 + ; "
              b": RUN 1 >R R> DROP " + operation + b" ;")
    runtimes = _engines(source)
    for runtime in runtimes:
        context = runtime.main_context
        empty = context.returns.empty_pointer
        # The dispatcher's own root cell takes the first one.
        context.returns = ReturnStack(
            memory=runtime.memory, floor=empty - 8 * (2 + room), empty_pointer=empty,
        )
    _agree(runtimes, "RUN", step_budget=BOUND, require_native=False)
