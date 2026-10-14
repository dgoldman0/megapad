"""The native line walker against the Python fast path it replaces.

Each case evaluates the same source through ``SOURCE-EVALUATE-CHECKED`` on
two native-backend KDOS runtimes: one whose overlays loop in Python, and one
whose overlays use the native line walker.  Everything the guest or the
evaluator can observe must match: all guest memory, both stack pointers, the
continuation cookie counter, semantic steps, every definition with its IR,
the side index, the open definition, diagnostics and UART output.
"""

from __future__ import annotations

import hashlib

import pytest

pytest.importorskip("_megaforth_native")

from simulator import source_acceleration  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.source_acceleration import install_kdos_source_accelerators  # noqa: E402
from tests.simulator.test_kdos_module_system import (  # noqa: E402
    _execute,
    _load_module_system,
    _variable,
)


@pytest.fixture
def runtimes(monkeypatch):
    monkeypatch.setenv("MEGAFORTH_EXECUTOR", "native")
    reference = _load_module_system()
    with monkeypatch.context() as patch:
        patch.setattr(source_acceleration, "_native_walker", lambda runtime, **_: None)
        install_kdos_source_accelerators(reference)
    native = _load_module_system()
    install_kdos_source_accelerators(native)
    return reference, native


def _memory_digest(runtime: MegaForthRuntime) -> str:
    digest = hashlib.sha256()
    for region in runtime.memory._regions:
        for index in sorted(region.pages):
            page = bytes(region.pages[index])
            if any(page):
                digest.update(region.spec.base.to_bytes(8, "little"))
                digest.update(index.to_bytes(8, "little"))
                digest.update(page)
    return digest.hexdigest()


def _words(runtime: MegaForthRuntime) -> tuple:
    words = []
    for word in runtime.dictionary.words:
        implementation = word.implementation
        operations = getattr(implementation, "operations", None)
        detail = (
            repr(tuple(operations)) if operations is not None
            else repr(getattr(implementation, "value", None))
        )
        action = getattr(implementation, "action", None)
        words.append((
            word.name, word.header_address, word.xt, word.immediate,
            source_acceleration._implementation_kind(implementation), detail,
            repr(action),
        ))
    return tuple(words)


def _compiler(runtime: MegaForthRuntime):
    state = runtime._require_bios_evaluator().compiler_state
    compiler = state.compiler
    if compiler is None:
        return None, state.conditional_skip
    return (
        compiler.name,
        compiler.location,
        compiler.compile_mode,
        tuple(compiler.operations),
        repr(compiler.controls),
        bytes(compiler.literal_pool),
        compiler.temporary,
    ), state.conditional_skip


def _state(runtime: MegaForthRuntime) -> tuple:
    evaluator = runtime._require_bios_evaluator()
    context = runtime.main_context
    return (
        _memory_digest(runtime),
        context.data._pointer,
        context.returns._pointer,
        context.returns._continuation_cookie,
        runtime.dictionary.here,
        runtime.dictionary.latest,
        _words(runtime),
        runtime.dictionary_index.state,
        _compiler(runtime),
        evaluator.token_length,
        len(evaluator.frames),
        runtime.diagnostics.semantic_cycles,
        runtime.timer.counter,
    )


def _evaluate(runtime: MegaForthRuntime, name: str, source: bytes):
    source_word = runtime.define_created(name, initial_body=source)
    runtime.main_context.data.push(source_word.body_address)
    runtime.main_context.data.push(len(source))
    try:
        result = runtime.execute("SOURCE-EVALUATE-CHECKED")
    except KeyboardInterrupt:
        raise
    except BaseException as error:  # noqa: BLE001 - both sides must agree
        # Guest fault requests and aborts derive from BaseException.
        outcome = ("raised", type(error).__name__, str(error))
    else:
        outcome = ("returned", result.semantic_steps)
    stack = runtime.main_context.data.snapshot()
    # Capture the state before reading cells, which runs guest words.
    state = _state(runtime)
    if outcome[0] == "raised":
        # The context may no longer be reusable after a host escape.
        return outcome, stack, None, runtime.drain_uart_output(), state
    runtime.main_context.data.clear()
    cells = tuple(
        _variable(runtime, variable)
        for variable in (
            "EVAL-STATUS", "EVAL-LINE", "EVAL-COLUMN", "EVAL-DEPTH",
            "EVAL-THROW", "_SEC-CUR", "_SEC-REM", "_SEC-RAW-LEN",
            "_SEC-EVAL-LEN", "_SEC-LINE",
        )
    )
    return outcome, stack, cells, runtime.drain_uart_output(), state


def _walker_stats(runtime: MegaForthRuntime) -> tuple[int, int]:
    overlay = runtime._colon_accelerators[runtime.find("SOURCE-EVALUATE-CHECKED").xt]
    walker = next(
        cell.cell_contents for cell in overlay.callback.__closure__
        if type(cell.cell_contents).__name__ == "NativeSourceWalker"
    )
    return walker.stats()


def _same(runtimes, source: bytes, name: str = "WALKED-SOURCE"):
    reference, native = runtimes
    expected = _evaluate(reference, name, source)
    before = _walker_stats(native)
    observed = _evaluate(native, name, source)
    after = _walker_stats(native)
    assert observed == expected
    return observed, (after[0] - before[0], after[1] - before[1])


def test_definitions_and_control_structures_compile_identically(runtimes) -> None:
    observed, lines = _same(runtimes, (
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
    assert lines == (15, 1)


def test_definitions_span_lines_and_python_lines_in_between(runtimes) -> None:
    _, lines = _same(runtimes, (
        b": SPANNED ( a b -- c )\n"
        b"  DUP 0= IF\n"
        b"    DROP [ 1 2 + ] LITERAL\n"
        b"  ELSE\n"
        b"    S\" multi\" DROP + \\ trailing comment\n"
        b"  THEN ;\n"
        b"5 6 SPANNED 0 0 SPANNED\n"
    ))
    # The bracketed line and the last line run guest words.
    assert lines == (5, 2)


def test_interpreted_words_and_numbers_match(runtimes) -> None:
    observed, lines = _same(runtimes, (
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
    assert lines == (11, 0)


def test_comments_blank_lines_and_line_endings_match(runtimes) -> None:
    _same(runtimes, (
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


def test_lines_python_must_evaluate_still_match(runtimes) -> None:
    observed, lines = _same(runtimes, (
        b"1 2\n"
        b".\" printed\" CR\n"
        b"CREATE TABLE 1 , 2 C, 3 ALLOT\n"
        b"' DUP CONSTANT DUP-XT\n"
        b"VARIABLE A1 VARIABLE A2\n"
        b"[DEFINED] DUP [IF] 5 [ELSE] 6 [THEN]\n"
        b"HERE DROP\n"
        b"3\n"
    ))
    assert observed[0][0] == "returned"
    assert lines == (2, 6)


def test_undefined_token_reports_the_same_diagnostics(runtimes) -> None:
    observed, lines = _same(runtimes, b"11 22 +\n5 missing-token 99\n77\n")
    assert observed[1] == (33, 5, 1)
    assert lines == (1, 1)


@pytest.mark.parametrize("source", (
    b"1 0 /\n",
    b"DROP DROP DROP DROP\n",
    b": BROKEN ELSE ;\n",
    b": OUTER : INNER ;\n",
    b"1 2 : SPLIT DUP\n3 ;\n",
))
def test_errors_raise_the_same_way(runtimes, source: bytes) -> None:
    _same(runtimes, source)


def test_long_lines_and_status_two_match(runtimes) -> None:
    _same(runtimes, b"1 " * 200 + b"\n2\n")
