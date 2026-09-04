"""Focused source-control words needed by the Akashic Desktop closure."""

from __future__ import annotations

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError, SourceError
from simulator.runtime import MegaForthRuntime


def _execute(runtime: MegaForthRuntime, name: str) -> tuple[int, ...]:
    context = runtime.new_context()
    runtime.execute(name, context=context)
    assert context.returns.snapshot() == ()
    return context.data.snapshot()


def test_char_and_bracket_char_consume_the_next_token_as_one_byte() -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()

    runtime.evaluate(b"CHAR Zebra CHAR", context=context)
    assert context.data.snapshot() == (ord("Z"), 0)

    runtime.evaluate(
        b": COMPILED-CHARS [CHAR] [ [CHAR] word [CHAR]\n 41 ;"
    )
    assert _execute(runtime, "COMPILED-CHARS") == (
        ord("["),
        ord("w"),
        41,
    )


@pytest.mark.parametrize(
    ("dividend", "divisor", "remainder", "quotient"),
    (
        (7, 3, 1, 2),
        (-7, 3, -1, -2),
        (7, -3, 1, -2),
        (-7, -3, -1, 2),
    ),
)
def test_slash_mod_publishes_signed_remainder_then_quotient(
    dividend: int,
    divisor: int,
    remainder: int,
    quotient: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(dividend)
    context.data.push(divisor)

    runtime.execute("/MOD", context=context)

    assert context.data.snapshot() == (
        remainder & MASK64,
        quotient & MASK64,
    )


@pytest.mark.parametrize("dividend, divisor", ((1, 0), (-(1 << 63), -1)))
def test_slash_mod_reports_the_native_division_traps(
    dividend: int,
    divisor: int,
) -> None:
    runtime = MegaForthRuntime()
    context = runtime.new_context()
    context.data.push(dividend)
    context.data.push(divisor)

    with pytest.raises(ExecutionError, match="/MOD trapped"):
        runtime.execute("/MOD", context=context)

    assert context.data.snapshot() == ()


def test_bracket_defined_selects_source_without_compiling_the_other_arm() -> None:
    runtime = MegaForthRuntime()

    runtime.evaluate(
        b"[DEFINED] DUP [IF]\n"
        b"  : SELECTED 17 ;\n"
        b"[ELSE]\n"
        b"  this unknown source is skipped\n"
        b"[THEN]\n"
        b": FALLBACK [UNDEFINED] NEVER-DEFINED [IF] 23 [ELSE] nope [THEN] ;"
    )

    assert _execute(runtime, "SELECTED") == (17,)
    assert _execute(runtime, "FALLBACK") == (23,)


def test_conditional_skip_is_raw_nested_and_case_insensitive() -> None:
    runtime = MegaForthRuntime()

    runtime.evaluate(
        b"0 [if]\n"
        b"  \\ [IF] in skipped comment text is still a raw nested opener\n"
        b"  this unknown source remains skipped\n"
        b"  [THEN]\n"
        b"[eLsE] 91 [tHeN]"
    )

    assert runtime.main_context.data.snapshot() == (91,)


def test_unterminated_host_conditional_is_a_source_error() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match=r"no terminating \[THEN\]"):
        runtime.evaluate(b"0 [IF] ignored")


def test_case_selects_a_clause_and_preserves_default_fallthrough() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": CLASSIFY CASE "
        b"1 OF 11 ENDOF "
        b"2 OF 22 ENDOF "
        b"99 SWAP ENDCASE ;"
    )

    for selector, expected in ((1, 11), (2, 22), (3, 99)):
        context = runtime.new_context()
        context.data.push(selector)
        runtime.execute("CLASSIFY", context=context)
        assert context.data.snapshot() == (expected,)


def test_nested_case_and_if_close_their_own_control_frames() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": NESTED CASE "
        b"1 OF CASE 7 OF 70 ENDOF 90 SWAP ENDCASE ENDOF "
        b"2 OF IF 20 ELSE 21 THEN ENDOF "
        b"99 SWAP ENDCASE ;"
    )

    cases = (
        ((7, 1), (70,)),
        ((8, 1), (90,)),
        ((0, 2), (21,)),
        ((5, 2), (20,)),
        ((3,), (99,)),
    )
    for inputs, expected in cases:
        context = runtime.new_context()
        for value in inputs:
            context.data.push(value)
        runtime.execute("NESTED", context=context)
        assert context.data.snapshot() == expected


@pytest.mark.parametrize(
    ("source", "message"),
    (
        (b": BAD OF ;", "OF has no matching CASE"),
        (b": BAD CASE ENDOF ;", "ENDOF has no matching OF"),
        (b": BAD CASE 1 OF ENDCASE ;", "ENDCASE has no matching CASE"),
        (b": BAD ENDCASE ;", "ENDCASE has no matching CASE"),
    ),
)
def test_case_control_words_reject_malformed_nesting(
    source: bytes,
    message: str,
) -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match=message):
        runtime.evaluate(source)


def test_recurse_calls_the_definition_being_compiled() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": SUM-DOWN DUP 0= IF DROP 0 EXIT THEN DUP 1- RECURSE + ; "
        b": CALL-ORIGINAL 4 SUM-DOWN ; "
        b": SUM-DOWN 999 ;"
    )

    assert _execute(runtime, "CALL-ORIGINAL") == (10,)
    assert _execute(runtime, "SUM-DOWN") == (999,)


def test_recurse_is_compile_only_and_rejects_temporary_interpret_control() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(SourceError, match="RECURSE is compile-only"):
        runtime.evaluate(b"RECURSE")

    with pytest.raises(
        SourceError,
        match="RECURSE requires a named colon definition",
    ):
        runtime.evaluate(b"1 IF RECURSE THEN")
