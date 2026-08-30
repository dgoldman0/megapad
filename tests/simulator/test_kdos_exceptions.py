"""Unchanged-source acceptance for KDOS's first exception layer."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import u64
from simulator.errors import ExecutionError, ForthAbort, StepBudgetExceeded
from simulator.runtime import MegaForthRuntime


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE_DIRECTORY = Path(__file__).with_name("fixtures")
PREFIX_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-39-69.f"
EXCEPTION_FIXTURE = FIXTURE_DIRECTORY / "kdos-exceptions-618-675.f"

MEGAPAD_REVISION = "ed451faccfddb5f3fbb4e2200eb0dd0fdc314f4c"
KDOS_GIT_BLOB = "fd017b16dbd3ef4746d0e3467e980c015cf5a664"

PREFIX_FIRST_LINE = 39
PREFIX_LAST_LINE = 69
PREFIX_SHA256 = "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c"
PREFIX_GIT_BLOB = "ecef2fef19b54559367f1a162a97558776ab6ee8"

EXCEPTION_FIRST_LINE = 618
EXCEPTION_LAST_LINE = 675
EXCEPTION_SHA256 = (
    "e6c436d23bacead66f8d54ce247c70cd81edd2c0aebeef5d7638db8e14071a40"
)
EXCEPTION_GIT_BLOB = "ad070934246e7094bcf8a4c22ded4c068dc3cd4c"
EXCEPTION_DEFINITIONS = (
    b"_HANDLERS",
    b"_TASK-HANDLERS",
    b"HANDLER",
    b"CATCH",
    b"THROW",
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice(
    fixture: Path,
    *,
    first_line: int,
    last_line: int,
    sha256: str,
    git_blob: str,
) -> bytes:
    source = fixture.read_bytes()
    assert hashlib.sha256(source).hexdigest() == sha256
    assert _git_blob_id(source) == git_blob

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[first_line - 1 : last_line])
    return source


def _load_exceptions() -> MegaForthRuntime:
    prefix = _verified_slice(
        PREFIX_FIXTURE,
        first_line=PREFIX_FIRST_LINE,
        last_line=PREFIX_LAST_LINE,
        sha256=PREFIX_SHA256,
        git_blob=PREFIX_GIT_BLOB,
    )
    exceptions = _verified_slice(
        EXCEPTION_FIXTURE,
        first_line=EXCEPTION_FIRST_LINE,
        last_line=EXCEPTION_LAST_LINE,
        sha256=EXCEPTION_SHA256,
        git_blob=EXCEPTION_GIT_BLOB,
    )

    runtime = MegaForthRuntime()
    runtime.evaluate(
        prefix,
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:"
            f"{PREFIX_FIRST_LINE}-{PREFIX_LAST_LINE}"
        ),
    )
    result = runtime.evaluate(
        exceptions,
        source_name=(
            f"kdos.f@{MEGAPAD_REVISION}:"
            f"{EXCEPTION_FIRST_LINE}-{EXCEPTION_LAST_LINE}"
        ),
    )

    assert tuple(word.name for word in result.definitions) == EXCEPTION_DEFINITIONS
    worker_handlers = runtime.find("_HANDLERS")
    task_handlers = runtime.find("_TASK-HANDLERS")
    assert worker_handlers is not None
    assert task_handlers is not None
    assert runtime.memory.read_bytes(worker_handlers.body_address, 8) == bytes(8)
    assert runtime.memory.read_bytes(task_handlers.body_address, 32) == bytes(32)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


@pytest.fixture
def loaded_exceptions() -> MegaForthRuntime:
    return _load_exceptions()


def _assert_handler_clear(runtime: MegaForthRuntime) -> None:
    task_handlers = runtime.find("_TASK-HANDLERS")
    assert task_handlers is not None

    runtime.execute("HANDLER")
    address = runtime.main_context.data.pop()

    assert address == task_handlers.body_address
    assert runtime.memory.read64(address) == 0
    assert runtime.main_context.returns.snapshot() == ()


def test_one_core_stack_pointer_words_report_live_guest_addresses(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    context = loaded_exceptions.main_context
    data_pointer = context.data.pointer
    return_pointer = context.returns.pointer

    loaded_exceptions.execute("SP@")
    assert context.data.pop() == data_pointer
    assert context.data.pointer == data_pointer

    loaded_exceptions.execute("RP@")
    assert context.data.pop() == return_pointer
    assert context.returns.pointer == return_pointer

    loaded_exceptions.evaluate(b"NCORES COREID TASK-ID")
    assert context.data.snapshot() == (1, 0, 0)
    context.data.clear()


def test_catch_normal_return_preserves_effects_and_appends_zero(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    initial_return_pointer = context.returns.pointer
    runtime.evaluate(b": EX-OK 11 22 ;")
    context.data.push(99)

    runtime.evaluate(b"' EX-OK CATCH")

    assert context.data.snapshot() == (99, 11, 22, 0)
    assert context.returns.pointer == initial_return_pointer
    _assert_handler_clear(runtime)


def test_throw_zero_continues_without_unwinding(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    runtime.evaluate(b": EX-ZERO 7 0 THROW 8 ;")

    runtime.evaluate(b"' EX-ZERO CATCH")

    assert context.data.snapshot() == (7, 8, 0)
    assert context.returns.snapshot() == ()
    _assert_handler_clear(runtime)


def test_nonzero_throw_restores_data_and_ordered_return_frames(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    initial_return_pointer = context.returns.pointer
    runtime.evaluate(
        b": EX-INNER-R 111 >R 222 >R 1 2 3 -77 THROW ; "
        b": EX-OUTER-R 444 >R ['] EX-INNER-R CATCH R> ;"
    )
    context.data.push(88)

    runtime.execute("EX-OUTER-R")

    assert context.data.snapshot() == (88, u64(-77), 444)
    assert context.returns.pointer == initial_return_pointer
    assert context.returns.snapshot() == ()
    _assert_handler_clear(runtime)


def test_nested_catch_handles_inner_throw_and_restores_both_handlers(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    runtime.evaluate(
        b": EX-INNER -5 THROW ; "
        b": EX-MIDDLE ['] EX-INNER CATCH ; "
        b": EX-OUTER ['] EX-MIDDLE CATCH ;"
    )

    runtime.execute("EX-OUTER")

    assert runtime.main_context.data.snapshot() == (u64(-5), 0)
    assert runtime.main_context.returns.snapshot() == ()
    _assert_handler_clear(runtime)


def test_nested_catch_can_rethrow_the_exact_code(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    runtime.evaluate(
        b": EX-INNER -42 THROW ; "
        b": EX-MIDDLE ['] EX-INNER CATCH THROW ; "
        b": EX-OUTER ['] EX-MIDDLE CATCH ;"
    )

    runtime.execute("EX-OUTER")

    assert runtime.main_context.data.snapshot() == (u64(-42),)
    assert runtime.main_context.returns.snapshot() == ()
    _assert_handler_clear(runtime)


def test_throw_unwinds_through_defer_does_and_an_active_do_loop(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    initial_return_pointer = context.returns.pointer
    runtime.evaluate(
        b": EX-LOOP-BOOM "
        b"  5 0 DO I 2 = IF -23 THROW THEN LOOP 999 ; "
        b"DEFER EX-ACTION "
        b"' EX-LOOP-BOOM IS EX-ACTION"
    )
    context.data.push(314)

    runtime.evaluate(b"' EX-ACTION CATCH")

    assert context.data.snapshot() == (314, u64(-23))
    assert context.returns.pointer == initial_return_pointer
    assert context.returns.snapshot() == ()
    _assert_handler_clear(runtime)


def test_abort_is_not_caught_as_a_source_throw(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    runtime.evaluate(b": EX-ABORT 123 >R 456 ABORT ;")
    context.data.push(789)

    with pytest.raises(ForthAbort):
        runtime.evaluate(b"' EX-ABORT CATCH")

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "ForthAbort"


def test_host_budget_escape_inside_catch_fail_closes_the_context(
    loaded_exceptions: MegaForthRuntime,
) -> None:
    runtime = loaded_exceptions
    context = runtime.main_context
    runtime.evaluate(b": EX-FOREVER 0 0 DO LOOP ;")

    with pytest.raises(StepBudgetExceeded):
        runtime.evaluate(
            b"' EX-FOREVER CATCH",
            step_budget=24,
        )

    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "StepBudgetExceeded"
    with pytest.raises(ExecutionError, match="not reusable"):
        runtime.execute("SP@")


def test_budget_escape_cannot_leave_a_stale_handler_in_a_reusable_context() -> None:
    stale_handler_budgets: list[int] = []
    completed_budget: int | None = None

    for budget in range(1, 128):
        runtime = _load_exceptions()
        runtime.evaluate(b": EX-QUICK 17 ;")
        task_handlers = runtime.find("_TASK-HANDLERS")
        assert task_handlers is not None

        try:
            runtime.evaluate(b"' EX-QUICK CATCH", step_budget=budget)
        except StepBudgetExceeded:
            handler = runtime.memory.read64(task_handlers.body_address)
            if handler != 0:
                stale_handler_budgets.append(budget)
                assert not runtime.main_context.reusable
                assert runtime.main_context.host_control_fault == (
                    "StepBudgetExceeded"
                )
        else:
            completed_budget = budget
            assert runtime.memory.read64(task_handlers.body_address) == 0
            assert runtime.main_context.reusable
            break

    assert stale_handler_budgets, "budget sweep did not reach CATCH cleanup"
    assert completed_budget is not None, "budget sweep did not reach completion"
