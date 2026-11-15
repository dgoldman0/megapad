"""The native line walker's tap against the reference recording.

A token recorder is told what the interpreter did with every token of a
source file.  The reference evaluator tells it as it takes each token, and
the native line walker tells it of the lines it evaluates itself
(docs/simulator-native-evaluator.md).  Each case here loads one source three
ways on native-backend KDOS runtimes:

- with no recorder;
- with a recorder whose ``native_tap`` is off, so that the walker hands
  every line to the reference evaluator, which is the reference recording;
- with a recorder as a build attaches it, where the walker evaluates the
  lines it admits and reports them.

The two recordings must give the same items and the same token files, byte
for byte, and so must a recording on the Python backend, which has no
walker.  All three native runs must leave the same guest: all memory, both
stack pointers, the cookie counter, semantic steps, every definition with its
IR, the side index, the open definition, diagnostics and UART output.  A
case also states how many lines the walker evaluated and how many it handed
back, so that none passes with the walker standing aside, and that a
recorder changes neither number.
"""

from __future__ import annotations

from pathlib import Path

import pytest

pytest.importorskip("_megaforth_native")

from shared.boot_identity import boot_identity  # noqa: E402
from shared.token_form import (  # noqa: E402
    TokenFormError,
    iter_records,
    unpack_token_file,
)
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.source_acceleration import install_kdos_source_accelerators  # noqa: E402
from simulator.storage import HostedStorageService  # noqa: E402
from simulator.token_recorder import TokenRecorder  # noqa: E402
from tests.simulator.test_native_walker import _evaluate, _state  # noqa: E402
from tests.token_module_cases import (  # noqa: E402
    RECORDED_SOURCES,
    load,
    volume,
)

KDOS = (Path(__file__).resolve().parents[2] / "kdos.f").read_bytes()
MASK = (1 << 64) - 1


def _kdos(image: bytes, *, backend: str = "native", tap: bool | None):
    """A KDOS runtime over the volume IMAGE, and its recorder: none when
    TAP is None, else one with the native tap on or off."""

    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytearray(image)),
        execution_backend=backend,
    )
    recorder = None
    if tap is not None:
        recorder = TokenRecorder()
        recorder.native_tap = tap
        recorder.attach(runtime)
    runtime.evaluate(KDOS, source_name="kdos.f")
    install_kdos_source_accelerators(runtime)
    runtime.drain_uart_output()
    return runtime, recorder


def _walked(runtime: MegaForthRuntime) -> tuple[int, int]:
    """Lines the walker evaluated and lines it handed back, over both of
    the overlays that call it."""

    native = python = 0
    for name in ("SOURCE-EVALUATE-CHECKED", "_LD-WALK"):
        overlay = runtime._colon_accelerators[runtime.find(name).xt]
        for cell in overlay.callback.__closure__:
            if type(cell.cell_contents).__name__ == "NativeSourceWalker":
                evaluated, handed = cell.cell_contents.stats()
                native += evaluated
                python += handed
    return native, python


def _recorded(recorder: TokenRecorder) -> list:
    """Every file the recorder holds: what it is, its items, and its token
    file or the reason it has none."""

    units = []
    for unit in recorder.units:
        items = [
            (item[0], item[1] & MASK) if item[0] == "number" else tuple(item)
            for item in unit.items
        ]
        try:
            file = unit.token_file(boot_id=boot_identity())
        except TokenFormError as error:
            file = str(error)
        units.append((unit.kind, unit.source, unit.finished, unit.unfit, items, file))
    return units


def _checked(source: bytes, runtime: MegaForthRuntime):
    """Evaluate SOURCE with SOURCE-EVALUATE-CHECKED."""

    return _evaluate(runtime, "RECORDED-SOURCE", source)


def _loaded(top: str):
    """Load the file TOP with REQUIRE."""

    def run(runtime: MegaForthRuntime):
        runtime.evaluate(load(top).encode(), source_name="record")
        return runtime.drain_uart_output(), _state(runtime)

    return run


def _record(run, files: dict[str, bytes] | None = None, *, lines: tuple[int, int]):
    """Run RUN each way, hold the runs to one another, and return what the
    walker's recording gave: the units, and the guest's outcome."""

    # One image for every run: a volume states when its files were made.
    image = bytes(volume(files or {}).img)
    plain, _ = _kdos(image, tap=None)
    reference, by_reference = _kdos(image, tap=False)
    tapped, by_tap = _kdos(image, tap=True)
    python, by_python = _kdos(image, backend="python", tap=True)

    expected = run(plain)
    assert run(reference) == expected
    observed = run(tapped)
    assert observed == expected
    run(python)

    # The walker took the same lines with a recorder as with none, and none
    # at all for the reference recording.
    assert _walked(plain) == lines
    assert _walked(tapped) == lines
    assert _walked(reference) == (0, sum(lines))

    units = _recorded(by_tap)
    assert units == _recorded(by_reference)
    assert units == _recorded(by_python)
    return units, observed


def _records(file: bytes) -> list[tuple]:
    return list(iter_records(unpack_token_file(file).records))


def _one(units: list) -> list[tuple]:
    """The records of the one file a case recorded."""

    (unit,) = units
    assert unit[2] and unit[3] is None
    return _records(unit[5])


def test_a_word_is_imported_once_and_then_named_by_its_slot() -> None:
    units, _ = _record(lambda runtime: _checked((
        b": R-ONE DUP DUP DROP ;\n"
        b": R-TWO R-ONE R-ONE DUP ;\n"
        b"VARIABLE R-V\n"
        b": R-THREE R-V @ R-TWO ;\n"
    ), runtime), lines=(4, 0))
    decoded = _one(units)
    imports = [record[1] for record in decoded if record[0] == "import"]
    assert imports == [b":", b"DUP", b"DROP", b";", b"VARIABLE", b"@"]
    # R-ONE, R-TWO and R-V are used after their definitions and take a slot
    # there; R-THREE is not, and has none.
    assert decoded.count(("defined",)) == 3
    assert unpack_token_file(units[0][5]).slots == 9
    # : is slot 0 and DUP slot 1.  Each is imported once and named by its
    # slot afterwards.
    assert decoded.count(("word", 0)) == 2
    assert decoded.count(("word", 1)) == 2


def test_numbers_of_every_size_compiled_and_interpreted() -> None:
    units, observed = _record(lambda runtime: _checked((
        b": R-NUM 1 -1 300 -300 70000 -70000 1099511627776 -1099511627776 ;\n"
        b"-5 300 70000 1099511627776 + + + CONSTANT R-SUM\n"
        b"9223372036854775807 CONSTANT R-MOST\n"
    ), runtime), lines=(3, 0))
    # The writer gives a number the smallest of its four sizes: one, two,
    # four and eight bytes here, of either sign.
    assert [record[1] for record in _one(units) if record[0] == "number"] == [
        1, -1, 300, -300, 70000, -70000, 1 << 40, -(1 << 40),
        -5, 300, 70000, 1 << 40,
        (1 << 63) - 1,
    ]
    # Only the status of the evaluation is left on the stack.
    assert observed[1] == (0,)


def test_the_text_each_parsing_word_took() -> None:
    units, _ = _record(lambda runtime: _checked((
        b": R-STR  S\" alpha beta\" ;\n"
        b": R-SAY .\" gamma  delta\" ;\n"
        b": R-ABORT 0 ABORT\" epsilon\" ;\n"
        b": R-TICK ['] DUP [CHAR] Z ;\n"
        b"0 VALUE R-VAL\n"
        b": R-TO 7 TO R-VAL ;\n"
        b"VARIABLE R-VAR   ( a comment, whose text no record holds )\n"
        b"9 CONSTANT R-CON \\ and a comment that leaves nothing at all\n"
    ), runtime), lines=(8, 0))
    decoded = _one(units)
    assert [record[1] for record in decoded if record[0] == "text"] == [
        b" R-STR", b' alpha beta"',
        b" R-SAY", b' gamma  delta"',
        b" R-ABORT", b' epsilon"',
        b" R-TICK", b" DUP", b" Z",
        b" R-VAL",
        b" R-TO", b" R-VAL",
        b" R-VAR",
        b" R-CON",
    ]
    # The comment word itself stays: ( is imported, with no text before it.
    at = decoded.index(("import", b"("))
    assert decoded[at - 1][0] != "text"


def test_line_marks_follow_blank_comment_and_continued_lines() -> None:
    units, _ = _record(lambda runtime: _checked((
        b"\\ a comment line, which leaves no record\n"
        b"\n"
        b": R-A 1 ;\r\n"
        b": R-B 2 ;\r\n"
        b"( a comment word alone )\n"
        b"\n"
        b"   \n"
        b": R-C\n"
        b"   R-A\n"
        b"\n"
        b"   R-B ;"
    ), runtime), lines=(8, 0))
    assert [record[1] for record in _one(units) if record[0] == "line"] == [
        3, 1, 1, 3, 1, 2,
    ]
    assert unpack_token_file(units[0][5]).lines == 11


def test_conditional_lines_go_to_python_between_native_lines() -> None:
    units, _ = _record(lambda runtime: _checked((
        b": R-BEFORE 1 ;\n"
        b"1 [IF] : R-YES 1 ; [ELSE] : R-YES 2 ; [THEN]\n"
        b"0 [IF]\n"
        b": R-SKIPPED 1 ;\n"
        b"[THEN]\n"
        b": R-AFTER R-BEFORE R-YES ;\n"
        b"[DEFINED] R-SKIPPED [IF] : R-D 1 ; [ELSE] : R-D 0 ; [THEN]\n"
        b": R-LAST R-D R-AFTER ;\n"
    ), runtime), lines=(3, 5))
    decoded = _one(units)
    # One skip ends in its own line, one two lines on, and one where the
    # line ends.
    assert decoded.count(("skipped",)) == 3
    # R-YES and R-D were defined in lines Python ran, and the native lines
    # after them name both by the slots those lines gave them.
    assert decoded.count(("defined",)) == 4
    assert ("import", b"R-YES") not in decoded
    assert ("import", b"R-D") not in decoded


def test_lines_that_run_guest_words_between_native_lines() -> None:
    units, observed = _record(lambda runtime: _checked((
        b": R-MAKER CREATE , DOES> @ ;\n"
        b"41 R-MAKER R-41\n"
        b": R-USE R-41 ;\n"
        b"CREATE R-TAB 10 , 20 , 30 ,\n"
        b": R-AT CELLS R-TAB + @ ;\n"
        b"R-USE 2 R-AT + CONSTANT R-SUM\n"
        b": R-END R-SUM ;\n"
        b"R-END\n"
    ), runtime), lines=(4, 4))
    decoded = _one(units)
    # R-41, R-TAB and R-SUM were made in lines Python ran, whose taps gave
    # each its slot, and the walker's lines name them by those slots.
    assert decoded.count(("defined",)) == 7
    for name in (b"R-41", b"R-TAB", b"R-SUM", b"R-MAKER", b"R-USE"):
        assert ("import", name) not in decoded
    assert observed[1] == (71, 0)


def test_a_file_and_the_file_it_requires() -> None:
    units, (output, _) = _record(
        _loaded("top.f"), RECORDED_SOURCES, lines=(9, 15)
    )
    top, lib = units
    assert (top[0], lib[0]) == ("loader", "loader")
    assert top[1] == RECORDED_SOURCES["top.f"]
    assert lib[1] == RECORDED_SOURCES["lib.f"]
    assert top[2] and lib[2]
    assert unpack_token_file(top[5]).module_name == b"rec-top"
    assert unpack_token_file(lib[5]).module_name == b"rec-lib"
    assert b"[lib][top 49 ]" in output and b"[THROW 0 ]" in output


@pytest.mark.parametrize("source, lines, reason", (
    # An undefined word, in a line Python takes after a native one.
    (b": R-OK 1 ;\nR-NOPE\n: R-NEVER 2 ;\n", (1, 1), "did not finish"),
    # A definition still open when the source ends.
    (b": R-OK 1 ;\n: R-OPEN 2\n", (2, 0), "did not finish"),
))
def test_a_load_that_fails_has_the_same_items_and_no_file(
    source: bytes, lines: tuple[int, int], reason: str,
) -> None:
    units, observed = _record(
        lambda runtime: _checked(source, runtime), lines=lines
    )
    (unit,) = units
    assert not unit[2]
    assert reason in unit[5]
    assert [item for item in unit[4] if item[0] == "word"][0][2] == b":"


def test_a_line_outside_a_recorded_file_is_not_recorded() -> None:
    """The walker reports a line by its address, and the recorder places it
    in the file its walk covers.  A second walk of the same bytes is a
    second file, and each has its own lines."""

    def twice(runtime: MegaForthRuntime):
        first = _checked(b": R-ONCE 1 ;\n", runtime)
        return first, _evaluate(runtime, "RECORDED-AGAIN", b": R-AGAIN R-ONCE ;\n")

    units, _ = _record(twice, lines=(2, 0))
    first, second = units
    assert _records(first[5])[:3] == [("line", 1), ("text", b" R-ONCE"), ("import", b":")]
    assert ("import", b"R-ONCE") in _records(second[5])
