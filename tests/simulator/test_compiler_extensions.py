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
