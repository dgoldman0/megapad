"""Focused state-smart ``ABORT\"`` semantics."""

from __future__ import annotations

import pytest

from simulator.errors import ForthAbort
from simulator.ir import AbortIf
from simulator.runtime import ColonDefinition, MegaForthRuntime


def test_interpreted_abort_quote_consumes_false_flag_and_input_payload() -> None:
    runtime = MegaForthRuntime()
    here = runtime.dictionary.here

    runtime.evaluate(b'0 ABORT" silent assertion" 41')

    assert runtime.main_context.data.snapshot() == (41,)
    assert runtime.dictionary.here == here
    assert runtime.drain_uart_output() == b""


def test_checked_bios_evaluator_accepts_a_top_level_abort_quote_assertion() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b"0 CONSTANT SCB-S-OK")
    source = b'SCB-S-OK 0 <> ABORT" screen backend init failed" 73'
    address = runtime.dictionary.here
    runtime.memory.write_bytes(address, source)
    context = runtime.new_context()
    context.data.push_pair(address, len(source))

    runtime.execute("EVALUATE-CHECKED", context=context)

    assert context.data.snapshot() == (73, 0)
    assert runtime.drain_uart_output() == b""


def test_interpreted_abort_quote_reports_true_flag_and_clears_task() -> None:
    runtime = MegaForthRuntime()

    with pytest.raises(ForthAbort, match='Forth ABORT"') as caught:
        runtime.evaluate(b'11 -1 ABORT" exact assertion" 99')

    assert caught.value.origin_context is runtime.main_context
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"exact assertion"


def test_compiled_abort_quote_keeps_the_conditional_ir_operation() -> None:
    runtime = MegaForthRuntime()

    runtime.evaluate(b': CHECK-ASSERT ABORT" compiled assertion" 73 ;')

    word = runtime.find("CHECK-ASSERT")
    assert word is not None
    assert isinstance(word.implementation, ColonDefinition)
    assert [
        operation.payload
        for operation in word.implementation.operations
        if isinstance(operation, AbortIf)
    ] == [b"compiled assertion"]

    false_context = runtime.new_context()
    false_context.data.push(0)
    runtime.execute(word.xt, context=false_context)
    assert false_context.data.snapshot() == (73,)

    true_context = runtime.new_context()
    true_context.data.push(1)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute(word.xt, context=true_context)
    assert true_context.data.snapshot() == ()
    assert runtime.drain_uart_output() == b"compiled assertion"
