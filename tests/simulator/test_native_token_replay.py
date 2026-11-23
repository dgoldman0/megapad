"""The native walker's record lines against the runtime's replay loop.

Each case replays one token stream on two native-backend runtimes: one whose
replay loop runs every record in Python, which is the reference, and one
whose record lines the native line walker takes.  Everything the guest or the
evaluator can observe must match: all guest memory, both stack pointers, the
continuation cookie counter, semantic steps, every definition with its IR,
the side index, the open definition, diagnostics and UART output.  A case
also says how many record lines the walker evaluated and how many it left to
the loop.

The recorded cases take the sources of ``test_native_walker.py`` through the
recorder first, so the same lines are compared as text and as records.
"""

from __future__ import annotations

from pathlib import Path

import pytest

pytest.importorskip("_megaforth_native")

from shared.boot_identity import boot_identity  # noqa: E402
from shared.token_form import TokenWriter, pack_token_file  # noqa: E402
from simulator.memory import EXTERNAL_BASE  # noqa: E402
from simulator.platform import create_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.source_acceleration import install_kdos_source_accelerators  # noqa: E402
from simulator.storage import HostedStorageService  # noqa: E402
from simulator.token_recorder import TokenRecorder  # noqa: E402
from tests.simulator.test_kdos_module_system import _variable  # noqa: E402
from tests.simulator.test_native_walker import _state  # noqa: E402
from tests.token_module_cases import SKIPPED, records, volume  # noqa: E402
from tests.token_streams import STREAMS  # noqa: E402

KDOS = (Path(__file__).resolve().parents[2] / "kdos.f").read_bytes()
TABLE = EXTERNAL_BASE + 0x10000


def _reference(runtime: MegaForthRuntime) -> MegaForthRuntime:
    """Leave RUNTIME's replay to its own loop, as the reference backend's is."""

    runtime._native_token_walker_sought = True
    assert runtime._token_walker() is None
    return runtime


def _kdos() -> MegaForthRuntime:
    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(volume({}).img))
    )
    runtime.evaluate(KDOS, source_name="kdos.f")
    install_kdos_source_accelerators(runtime)
    runtime.drain_uart_output()
    return runtime


def _bare() -> MegaForthRuntime:
    return MegaForthRuntime(
        memory=create_address_space(external_size=1 << 20)
    )


@pytest.fixture
def native_executor(monkeypatch):
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "native")


@pytest.fixture
def runtimes(native_executor):
    return _reference(_kdos()), _kdos()


@pytest.fixture
def bare(native_executor):
    return _reference(_bare()), _bare()


def _record(source: bytes) -> bytes:
    """SOURCE's token file, as the recorder writes it."""

    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(volume({}).img))
    )
    recorder = TokenRecorder()
    recorder.attach(runtime)
    runtime.evaluate(KDOS, source_name="kdos.f")
    install_kdos_source_accelerators(runtime)
    held = runtime.define_created("RECORDED-SOURCE", initial_body=source)
    runtime.main_context.data.push(held.body_address)
    runtime.main_context.data.push(len(source))
    runtime.execute("SOURCE-EVALUATE-CHECKED")
    assert runtime.main_context.data.pop() == 0
    unit = recorder.units[-1]
    assert unit.source == source
    return unit.token_file(boot_id=boot_identity())


def _file(made: tuple[bytes, int]) -> bytes:
    """A token file around hand-made records."""

    recorded, slots = made
    return pack_token_file(
        recorded, slots, source=b"", lines=0, boot_id=boot_identity()
    )


def _outcome(call):
    try:
        result = call()
    except KeyboardInterrupt:
        raise
    except BaseException as error:  # noqa: BLE001 - both sides must agree
        # Guest fault requests and aborts derive from BaseException.
        return ("raised", type(error).__name__, str(error))
    return ("returned", result.semantic_steps)


def _load(runtime: MegaForthRuntime, name: str, file: bytes):
    """Load FILE with TOKEN-EVALUATE-CHECKED and report all that shows."""

    held = runtime.define_created(name, initial_body=file)
    runtime.main_context.data.push(held.body_address)
    runtime.main_context.data.push(len(file))
    outcome = _outcome(lambda: runtime.execute("TOKEN-EVALUATE-CHECKED"))
    stack = runtime.main_context.data.snapshot()
    # Capture the state before reading cells, which runs guest words.
    state = _state(runtime)
    if outcome[0] == "raised":
        return outcome, stack, None, runtime.drain_uart_output(), state
    runtime.main_context.data.clear()
    cells = tuple(
        _variable(runtime, variable)
        for variable in (
            "EVAL-STATUS", "EVAL-LINE", "EVAL-COLUMN", "EVAL-DEPTH", "EVAL-THROW",
        )
    )
    return outcome, stack, cells, runtime.drain_uart_output(), state


def _replay(runtime, made, *, table=TABLE, before=b"", budget=None):
    """Run hand-made records with TOKEN-REPLAY itself."""

    recorded, slots = made
    runtime.memory.write_bytes(EXTERNAL_BASE, recorded)
    if before:
        runtime.evaluate(before)
    line = f"{EXTERNAL_BASE} {len(recorded)} {table} {slots} TOKEN-REPLAY".encode()
    outcome = _outcome(lambda: runtime.evaluate(line, step_budget=budget))
    stack = runtime.main_context.data.snapshot()
    return outcome, stack, runtime.drain_uart_output(), _state(runtime)


def _lines(runtime: MegaForthRuntime) -> tuple[int, int, int]:
    """Record lines the walker evaluated, handed back, and was barred from."""

    return runtime._token_walker().stats()[:3]


def _same(pair, run):
    reference, native = pair
    expected = run(reference)
    before = _lines(native)
    observed = run(native)
    after = _lines(native)
    assert observed == expected
    return observed, tuple(b - a for a, b in zip(before, after))


def _loaded(runtimes, source: bytes):
    file = _record(source)
    return _same(runtimes, lambda runtime: _load(runtime, "TOKEN-FILE", file))


# ---- the walker's own cases, as records -----------------------------------------


def test_definitions_and_control_structures_replay_identically(runtimes) -> None:
    observed, lines = _loaded(runtimes, (
        b": T-IF DUP 0= IF DROP 41 ELSE 1+ THEN ;\n"
        b": T-BEGIN BEGIN 1- DUP 0= UNTIL ;\n"
        b": T-WHILE BEGIN DUP WHILE 1- REPEAT ;\n"
        b": T-AGAIN BEGIN EXIT AGAIN ;\n"
        b": T-DO 0 SWAP 0 DO I + LOOP ;\n"
        b": T-QDO 0 SWAP 0 ?DO I + 2 +LOOP ;\n"
        b": T-LEAVE 10 0 DO I 3 = IF LEAVE THEN LOOP ;\n"
        b": T-UNLOOP 10 0 DO UNLOOP EXIT LOOP ;\n"
        b": T-R >R R@ R> + 2>R 2R@ 2R> ;\n"
        b": T-CASE CASE 1 OF 10 ENDOF 2 OF 20 ENDOF 99 SWAP ENDCASE ;\n"
        b": T-RECURSE DUP IF 1- RECURSE THEN ;\n"
        b": T-STRINGS S\" alpha\" .\" beta\" 0 ABORT\" gamma\" ;\n"
        b": T-TICK ['] DUP [CHAR] Z ;\n"
        b": T-DOES CREATE , DOES> @ ;\n"
        b": T-STACKS SP! RP! ;\n"
        b"7 T-IF 0 T-IF\n"
    ))
    assert observed[1] == (8, 41, 0)
    # Every line but the last, which runs colon words, is native.
    assert lines == (15, 1, 0)


def test_definitions_span_lines_and_python_lines_in_between(runtimes) -> None:
    _, lines = _loaded(runtimes, (
        b": SPANNED ( a b -- c )\n"
        b"  DUP 0= IF\n"
        b"    DROP [ 1 2 + ] LITERAL\n"
        b"  ELSE\n"
        b"    S\" multi\" DROP + \\ trailing comment\n"
        b"  THEN ;\n"
        b"5 6 SPANNED 0 0 SPANNED\n"
    ))
    # The bracketed line and the last line run guest words.
    assert lines == (5, 2, 0)


def test_an_anonymous_definition_stays_with_python(runtimes) -> None:
    observed, lines = _loaded(runtimes, (
        b": NAMED-BEFORE 1 ;\n"
        b":NONAME ( n -- n' )\n"
        b"  DUP 0= IF\n"
        b"    DROP S\" anon\" NIP\n"
        b"  THEN ;\n"
        b"CONSTANT ANON\n"
        b": NAMED-AFTER 2 ;\n"
        b"0 ANON EXECUTE NAMED-BEFORE NAMED-AFTER\n"
    ))
    assert observed[1] == (4, 1, 2, 0)
    # The walker rejects the line that opens the anonymous definition and
    # the last line, which runs guest words.  It is barred from the three
    # lines of the anonymous body, which it never holds.
    assert lines == (3, 2, 3)


def test_interpreted_words_and_numbers_match(runtimes) -> None:
    observed, lines = _loaded(runtimes, (
        b"11 22 + CONSTANT C1\n"
        b"VARIABLE V1\n"
        b"99 VALUE VAL1\n"
        b"C1 VAL1 + V1 DROP\n"
        b": SETV TO VAL1 ;\n"
        b"0x10 -5 0X1f -0x2 1 2 3 ROT -ROT SWAP OVER NIP TUCK 2DUP 2DROP\n"
        b"1 2 3 4 2SWAP 2OVER 4 PICK 2 ROLL ?DUP 0 ?DUP\n"
        b"7 2 / 7 2 MOD -7 2 / -7 2 MOD 3 NEGATE ABS 3 9 MIN 3 9 MAX\n"
        b"1+ 1- 2* 2/ 6 3 AND 6 3 OR 6 3 XOR 1 4 LSHIFT 256 4 RSHIFT INVERT\n"
        b"1 2 = 1 2 <> 0 0= 5 0<> -1 0< 1 0> 1 2 U< 1 2 U> -1 2 < 2 2 <= 2 1 >= 2 1 >\n"
        b"3 CELLS 4 CELL+\n"
    ))
    assert observed[0][0] == "returned"
    assert lines == (11, 0, 0)


def test_comments_leave_their_words_and_no_lines_of_their_own(runtimes) -> None:
    _, lines = _loaded(runtimes, (
        b"\\ a comment line\r\n"
        b"\r\n"
        b"\n"
        b"1 ( nested ( parens ) here ) 2 ( unterminated\r\n"
        b"3 \\ tail\n"
        b"   \n"
        b": CMT ( x -- x ) \\ comment after colon\n"
        b"  ( inside ) ;\n"
        b"4"
    ))
    # Five lines hold a word or a number; the others leave no record.
    assert lines == (5, 0, 0)


PYTHON_LINES = (
    b"1 2\n"
    b".\" printed\" CR\n"
    b"CREATE TABLE 1 , 2 C, 3 ALLOT\n"
    b"' DUP CONSTANT DUP-XT\n"
    b"VARIABLE A1 VARIABLE A2\n"
    b"[DEFINED] DUP [IF] 5 [ELSE] 6 [THEN]\n"
    b"HERE DROP\n"
    b"3\n"
)


def test_lines_python_must_evaluate_still_match(runtimes) -> None:
    observed, lines = _loaded(runtimes, PYTHON_LINES)
    assert observed[0][0] == "returned"
    assert lines == (2, 6, 0)


def test_a_rejected_line_says_why(runtimes) -> None:
    _, native = runtimes
    walker = native._token_walker()
    walker.collect_reasons()

    _load(native, "TOKEN-FILE", _record(PYTHON_LINES))

    assert walker.stats()[3] == {
        'the directive ."': 1,
        "runs the primitive CREATE": 1,
        "runs the primitive '": 1,
        "follows a definition in its line": 1,
        "the directive [DEFINED]": 1,
        "runs the primitive HERE": 1,
    }


def test_a_word_defined_and_used_in_one_line_goes_to_python(runtimes) -> None:
    observed, lines = _loaded(runtimes, (
        b": ONE 1 ; ( a comment may follow )\n"
        b": TWO 2 ; TWO\n"
        b"5 CONSTANT FIVE FIVE ONE +\n"
        b": THREE ONE TWO + ;\n"
    ))
    assert observed[1] == (2, 6, 0)
    # The first and last lines are native.  The walker holds a line's
    # definition back until the line commits, so a line that goes on to use
    # its own definition, or to run anything after it, is Python's.
    assert lines == (2, 2, 0)


# ---- streams the loop must judge ------------------------------------------------


def test_a_name_that_is_not_defined_reports_the_same_diagnostics(runtimes) -> None:
    made = records(11, 22, "+", "\n", 5, "missing-token", 99, "\n", 77)
    observed, lines = _same(
        runtimes, lambda runtime: _load(runtime, "TOKEN-FILE", _file(made))
    )
    assert observed[1] == (33, 5, 1)
    assert "missing-token ? (not found)" in observed[3].decode("latin-1")
    assert lines == (1, 1, 0)


@pytest.mark.parametrize("items", (
    (1, 0, "/"),
    ("DROP", "DROP", "DROP", "DROP"),
    ((":", " BROKEN"), "ELSE", ";"),
    ((":", " OUTER"), (":", " INNER"), ";"),
    (1, 2, (":", " SPLIT"), "DUP", "\n", 3, ";"),
))
def test_errors_raise_the_same_way(runtimes, items) -> None:
    made = records(77, "\n", *items)
    _, lines = _same(
        runtimes, lambda runtime: _load(runtime, "TOKEN-FILE", _file(made))
    )
    assert lines[0] >= 1


def _after_two_lines(tail: bytes, slots: int = 8) -> tuple[bytes, int]:
    """Two lines the walker takes, then TAIL in place of the END record."""

    recorded, _ = records(1, 2, "+", "\n", (":", " KEPT"), 3, ";")
    assert recorded.endswith(b"\xff")
    return recorded[:-1] + b"\xf6" + tail, slots


BROKEN = {
    "a number cut short": b"\xf3\x01\x02",
    "a slot nothing bound": b"\x00\x07\xff",
    "an opcode nobody knows": b"\xfa\xff",
    "a skip nothing began": b"\xf8\xff",
    "a skip the record does not have": b"\xf1\x00\xf0\x04[IF]\xf1\x09\xff",
    "no END record": b"\xf1\x05",
    "an import past the table": (
        b"\xf0\x03DUP\xf0\x04DROP\xf0\x04SWAP\xf0\x04OVER\xf0\x03NIP\xf0\x04TUCK\xff"
    ),
    "DEFINED past the table": b"\xf9\xf9\xf9\xf9\xf9\xf9\xff",
    "a text no word reads, then the end": b"\xf5\x03 ab\xff",
    "a text no word reads, then a line": b"\xf5\x03 ab\xf6\xf1\x07\xff",
    "a word that takes entries out of the dictionary": (
        b"\xf0\x04HERE\xf0\x06LATEST\xf0\x0dDICT-ROLLBACK\xf1\x05\xff"
    ),
}


@pytest.mark.parametrize("name", sorted(BROKEN))
def test_a_stream_only_the_loop_can_judge_ends_alike(runtimes, name: str) -> None:
    made = _after_two_lines(BROKEN[name])
    _, lines = _same(
        runtimes, lambda runtime: _load(runtime, "TOKEN-FILE", _file(made))
    )
    # The two lines before the fault are native on the way to it.
    assert lines[0] == 2


@pytest.mark.parametrize("name", sorted(STREAMS))
def test_every_stream_case_replays_alike(bare, name: str) -> None:
    make, _expected = STREAMS[name]
    made = make()

    _same(bare, lambda runtime: _replay(runtime, made))


def test_a_replay_nested_in_a_record_line_keeps_each_stream_its_own(runtimes) -> None:
    inner = _file(records(
        (":", " INNER-A"), 10, ";", "\n",
        "INNER-A", 1, "+", ("CONSTANT", " INNER-B"), "\n",
        (":", " INNER-C"), "INNER-A", "INNER-B", "+", ";",
    ))
    outer = _file(records(
        (":", " OUTER-A"), 1, ";", "\n",
        "RUN-INNER", "\n",
        (":", " OUTER-B"), "OUTER-A", "INNER-C", "+", ";", "\n",
        "OUTER-B",
    ))

    def run(runtime: MegaForthRuntime):
        held = runtime.define_created("INNER-FILE", initial_body=inner)
        runtime.evaluate(
            f": RUN-INNER {held.body_address} {len(inner)} "
            "TOKEN-EVALUATE-CHECKED DROP ;".encode()
        )
        return _load(runtime, "OUTER-FILE", outer)

    observed, lines = _same(runtimes, run)
    assert observed[1] == (22, 0)
    # Each stream's two definitions are native.  The inner stream's middle
    # line and the outer's second and last lines run colon words.
    assert lines == (4, 3, 0)


def test_a_step_budget_ends_both_replays_at_the_same_word(native_executor) -> None:
    made = records(*((1, 2, "+", "DROP", "\n") * 6), 7)
    ended = set()
    for budget in range(1, 20):
        pair = _reference(_bare()), _bare()
        observed, _ = _same(pair, lambda runtime: _replay(runtime, made, budget=budget))
        ended.add(observed[0][0])
    # Some budgets end the replay inside a line the walker would take.
    assert ended == {"raised", "returned"}


# ---- writes a record line holds back ---------------------------------------------


def _reasons(runtime: MegaForthRuntime) -> dict[str, int]:
    return runtime._token_walker().stats()[3]


def test_a_table_in_the_data_stack_goes_to_python(bare) -> None:
    reference, native = bare
    native._token_walker().collect_reasons()
    made = records(5, 6, 7, "DUP", "DROP", "\n", 8)
    table = reference.main_context.data._pointer - 16
    assert table == native.main_context.data._pointer - 16

    _same(bare, lambda runtime: _replay(runtime, made, table=table))

    assert _reasons(native) == {"a table cell in the data stack": 1}


def test_a_table_over_a_value_it_reads_goes_to_python(bare) -> None:
    reference, native = bare
    native._token_walker().collect_reasons()
    made = records("HELD", "\n", 8)
    tables = set()

    def run(runtime: MegaForthRuntime):
        runtime.evaluate(b"99 VALUE HELD")
        table = runtime.find(b"HELD").body_address
        tables.add(table)
        return _replay(runtime, made, table=table)

    observed, _ = _same(bare, run)

    assert len(tables) == 1
    # The word reads the header address its own binding wrote over the 99.
    assert observed[1] == (native.find(b"HELD").header_address, 8)
    assert _reasons(native) == {"a failed preflight of HELD": 1}


def test_a_table_over_the_definition_its_line_publishes_goes_to_python(bare) -> None:
    reference, native = bare
    native._token_walker().collect_reasons()
    writer = TokenWriter()
    writer.line(1)
    writer.word("colon", b":", b" MADE")
    writer.number(1)
    writer.word("semi", b";")
    writer.defined("made")
    writer.line(2)
    writer.number(8)
    made = writer.finish(), 3
    tables = set()

    def run(runtime: MegaForthRuntime):
        tables.add(runtime.dictionary.here)
        return _replay(runtime, made, table=runtime.dictionary.here)

    _same(bare, run)

    assert len(tables) == 1
    assert _reasons(native) == {
        "a table cell in the definition its line publishes": 1
    }


def test_the_reference_backend_never_asks_for_the_walker(monkeypatch) -> None:
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "python")
    runtime = _bare()

    outcome, stack, _uart, _state_now = _replay(runtime, records(1, 2, "+"))

    assert outcome[0] == "returned" and stack == (3,)
    assert runtime._token_walker() is None
