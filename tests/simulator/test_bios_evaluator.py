"""Semantic BIOS acceptance for the guest checked-evaluator surface."""

from __future__ import annotations

import pytest

from shared.cells import MASK64, u64
from simulator.errors import ForthAbort, StepBudgetExceeded
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_exceptions import _load_exceptions


EVALUATOR_WORDS = (
    "EVALUATE",
    "EVALUATE-CHECKED",
    "EVALUATE-FINISH",
    "EVALUATOR-RESET",
    "EVALUATOR-UNWIND",
    "EVAL-STATUS",
    "EVAL-LINE",
    "EVAL-COLUMN",
    "EVAL-DEPTH",
    "EVAL-THROW",
    "EVAL-TOKEN",
)

EVALUATOR_CELLS = (
    "EVAL-STATUS",
    "EVAL-LINE",
    "EVAL-COLUMN",
    "EVAL-DEPTH",
    "EVAL-THROW",
)


def _body(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.body_address


def _cell(runtime: MegaForthRuntime, name: str) -> int:
    return runtime.memory.read64(_body(runtime, name))


def _put_source(runtime: MegaForthRuntime, source: bytes) -> tuple[int, int]:
    address = runtime.dictionary.here
    runtime.memory.write_bytes(address, source)
    return address, len(source)


def _evaluate_checked(
    runtime: MegaForthRuntime,
    source: bytes,
    *,
    context=None,
) -> tuple[int, ...]:
    active = runtime.main_context if context is None else context
    address, length = _put_source(runtime, source)
    active.data.push(address)
    active.data.push(length)
    runtime.execute("EVALUATE-CHECKED", context=active)
    return active.data.snapshot()


def _eval_token(runtime: MegaForthRuntime) -> tuple[int, bytes]:
    context = runtime.new_context()
    runtime.execute("EVAL-TOKEN", context=context)
    address, length = context.data.snapshot()
    return address, runtime.memory.read_bytes(address, length)


def test_evaluator_abi_uses_protected_zeroed_cells_and_token_storage() -> None:
    runtime = MegaForthRuntime()

    assert all(runtime.find(name) is not None for name in EVALUATOR_WORDS)
    addresses = tuple(_body(runtime, name) for name in EVALUATOR_CELLS)
    assert len(set(addresses)) == len(addresses)
    assert tuple(runtime.memory.read64(address) for address in addresses) == (
        0,
    ) * len(addresses)

    token_address, token = _eval_token(runtime)
    assert token == b""
    assert runtime.memory.read_bytes(token_address, 256) == bytes(256)
    assert token_address + 256 <= runtime.dictionary.numeric_rollback_floor


def test_evaluate_checked_preserves_effects_and_stable_undefined_diagnostics() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context

    assert _evaluate_checked(runtime, b"11 22 +") == (33, 0)
    assert runtime.drain_uart_output() == b""
    context.data.clear()
    runtime.memory.write64(_body(runtime, "EVAL-LINE"), 7)

    assert _evaluate_checked(runtime, b"11 missing 22") == (11, 1)
    assert runtime.drain_uart_output() == b"missing ? (not found)\n"
    assert _cell(runtime, "EVAL-STATUS") == 1
    assert _cell(runtime, "EVAL-LINE") == 7
    assert _cell(runtime, "EVAL-COLUMN") == 3
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert _cell(runtime, "EVAL-THROW") == 0
    _token_address, token = _eval_token(runtime)
    assert token == b"missing"


def test_guest_evaluate_retains_raw_carriage_return_token_bytes() -> None:
    runtime = MegaForthRuntime()

    assert _evaluate_checked(runtime, b"7\r") == (1,)
    assert runtime.drain_uart_output() == b"7\r ? (not found)\n"
    assert _cell(runtime, "EVAL-COLUMN") == 0
    _token_address, token = _eval_token(runtime)
    assert token == b"7\r"


def test_overlong_evaluate_rejects_before_read_and_retains_line_context() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    runtime.memory.write64(_body(runtime, "EVAL-LINE"), 19)
    context.data.push(MASK64)
    context.data.push(256)

    runtime.execute("EVALUATE-CHECKED")

    assert context.data.snapshot() == (2,)
    assert runtime.drain_uart_output() == (
        b"EVALUATE input exceeds 255 bytes\n"
    )
    assert _cell(runtime, "EVAL-STATUS") == 2
    assert _cell(runtime, "EVAL-LINE") == 19
    assert _cell(runtime, "EVAL-COLUMN") == 0
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert _eval_token(runtime)[1] == b""


def test_guest_compiler_and_control_stack_persist_across_evaluate_calls() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    sources = (
        b": CROSS-LINE",
        b"DUP 0= IF",
        b"DROP 41 ELSE",
        b"1+ THEN ;",
    )

    for source in sources:
        assert _evaluate_checked(runtime, source) == (0,)
        context.data.clear()

    runtime.execute("EVALUATE-FINISH")
    assert context.data.snapshot() == (0,)
    context.data.clear()
    context.data.push(0)
    runtime.execute("CROSS-LINE")
    assert context.data.snapshot() == (41,)
    context.data.clear()
    context.data.push(8)
    runtime.execute("CROSS-LINE")
    assert context.data.snapshot() == (9,)


def test_evaluate_finish_reports_unfinished_and_reset_retains_diagnostics() -> None:
    runtime = MegaForthRuntime()
    context = runtime.main_context
    here = runtime.dictionary.here
    latest = runtime.dictionary.latest

    assert _evaluate_checked(runtime, b": NEVER-FINISHED 1") == (0,)
    context.data.clear()
    runtime.execute("EVALUATE-FINISH")

    assert context.data.snapshot() == (4,)
    assert _cell(runtime, "EVAL-STATUS") == 4
    assert runtime.dictionary.here == here
    assert runtime.dictionary.latest == latest
    assert runtime.find("NEVER-FINISHED") is None
    context.data.clear()
    runtime.execute("EVALUATOR-RESET")
    assert _cell(runtime, "EVAL-STATUS") == 4
    assert _cell(runtime, "EVAL-DEPTH") == 0

    assert _evaluate_checked(runtime, b": AFTER-RESET 9 ;") == (0,)
    context.data.clear()
    runtime.execute("AFTER-RESET")
    assert context.data.snapshot() == (9,)


def test_nested_evaluate_failure_is_sticky_and_stops_the_outer_source() -> None:
    runtime = MegaForthRuntime()
    inner = b"missing-inner 222"
    source_word = runtime.define_created(
        "INNER-EVAL-SOURCE",
        initial_body=inner,
    )
    runtime.evaluate(
        (
            f": RUN-INNER-EVAL {source_word.body_address} "
            f"{len(inner)} EVALUATE ;"
        ).encode("ascii")
    )

    assert _evaluate_checked(runtime, b"RUN-INNER-EVAL 111") == (1,)
    assert runtime.drain_uart_output() == (
        b"missing-inner ? (not found)\n"
    )
    assert _cell(runtime, "EVAL-STATUS") == 1
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert _eval_token(runtime)[1] == b"missing-inner"


def test_nested_evaluate_depth_limit_is_sticky_and_stops_every_tail() -> None:
    runtime = MegaForthRuntime()
    nested_source = b"DESCEND-EVAL 777"
    source_word = runtime.define_created(
        "DESCEND-EVAL-SOURCE",
        initial_body=nested_source,
    )
    runtime.evaluate(
        (
            f": DESCEND-EVAL {source_word.body_address} "
            f"{len(nested_source)} EVALUATE ;"
        ).encode("ascii")
    )

    assert _evaluate_checked(runtime, b"DESCEND-EVAL 999") == (3,)
    assert runtime.drain_uart_output() == (
        b"EVALUATE depth limit exceeded\n"
    )
    assert _cell(runtime, "EVAL-STATUS") == 3
    assert _cell(runtime, "EVAL-COLUMN") == 0
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert _eval_token(runtime)[1] == b""


def test_throw_leaves_a_logical_frame_until_valid_evaluator_unwind() -> None:
    runtime = _load_exceptions()
    source = b"-77 THROW"
    source_word = runtime.define_created(
        "THROW-EVAL-SOURCE",
        initial_body=source,
    )
    context = runtime.main_context
    context.data.push(source_word.body_address)
    context.data.push(len(source))

    runtime.evaluate(
        b"' EVALUATE CATCH "
        b"-1 EVALUATOR-UNWIND EVAL-DEPTH @ "
        b"2 EVALUATOR-UNWIND EVAL-DEPTH @ "
        b"0 EVALUATOR-UNWIND 123"
    )

    values = context.data.snapshot()
    assert len(values) == 6
    assert values[2:] == (
        u64(-77),
        1,
        1,
        123,
    )
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert context.returns.snapshot() == ()
    task_handlers = runtime.find("_TASK-HANDLERS")
    assert task_handlers is not None
    assert runtime.memory.read64(task_handlers.body_address) == 0


def test_host_abort_clears_hidden_evaluator_depth_and_compiler_state() -> None:
    runtime = MegaForthRuntime()
    source = b": ABANDONED [ ABORT"
    source_word = runtime.define_created(
        "ABORT-EVAL-SOURCE",
        initial_body=source,
    )
    runtime.main_context.data.push(source_word.body_address)
    runtime.main_context.data.push(len(source))

    with pytest.raises(ForthAbort):
        runtime.execute("EVALUATE")

    assert _cell(runtime, "EVAL-DEPTH") == 0
    runtime.execute("EVALUATE-FINISH")
    assert runtime.main_context.data.snapshot() == (0,)
    assert runtime.find("ABANDONED") is None


def test_unroutable_guest_transfer_fail_closes_at_outer_run_boundary() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b"VARIABLE SAVED-RP "
        b": CAPTURE-DEEP-RP RP@ SAVED-RP ! ; "
        b": USE-STALE-RP SAVED-RP @ RP! ;"
    )

    def capture_nested_pointer(context) -> None:
        runtime.execute("CAPTURE-DEEP-RP", context=context)

    runtime.define_primitive("CAPTURE-NESTED-RP", capture_nested_pointer)
    runtime.evaluate(b": CAPTURE-STALE-RP CAPTURE-NESTED-RP ;")
    runtime.execute("CAPTURE-STALE-RP")

    source = b": ABANDONED-TRANSFER [ USE-STALE-RP"
    source_word = runtime.define_created(
        "STALE-TRANSFER-SOURCE",
        initial_body=source,
    )
    context = runtime.main_context
    context.data.push(source_word.body_address)
    context.data.push(len(source))

    with pytest.raises(BaseException, match="guest control transferred") as caught:
        runtime.execute("EVALUATE")

    assert type(caught.value).__name__ == "_GuestControlTransfer"
    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert runtime.find("ABANDONED-TRANSFER") is None
    context.data.clear()
    context.returns.clear()
    runtime.execute("EVALUATE-FINISH")
    assert context.data.snapshot() == (0,)


def test_inherited_budget_escape_clears_evaluator_depth_and_compiler() -> None:
    runtime = MegaForthRuntime()
    source = b": ABANDONED-BUDGET [ 0 DROP 0 DROP"
    source_word = runtime.define_created(
        "BUDGET-EVAL-SOURCE",
        initial_body=source,
    )
    context = runtime.main_context
    context.data.push(source_word.body_address)
    context.data.push(len(source))

    with pytest.raises(StepBudgetExceeded, match="2-step budget"):
        runtime.execute("EVALUATE", step_budget=2)

    assert _cell(runtime, "EVAL-DEPTH") == 0
    assert runtime.find("ABANDONED-BUDGET") is None
    context.data.clear()
    runtime.execute("EVALUATE-FINISH")
    assert context.data.snapshot() == (0,)
