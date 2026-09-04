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
PARSE_FIXTURE = FIXTURE_DIRECTORY / "kdos-prefix-71-115.f"
ALLOCATOR_FIXTURE = FIXTURE_DIRECTORY / "kdos-allocator-116-545.f"
SNAPSHOT_FIXTURE = FIXTURE_DIRECTORY / "kdos-snapshots-546-617.f"
EXCEPTION_FIXTURE = FIXTURE_DIRECTORY / "kdos-exceptions-618-675.f"

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"

PREFIX_FIRST_LINE = 39
PREFIX_LAST_LINE = 69
PREFIX_SHA256 = "e3918ffeab18446da9e9b190b4d0b82382a3ed5e9fcc220680b5164ab261d01c"
PREFIX_GIT_BLOB = "ecef2fef19b54559367f1a162a97558776ab6ee8"
PREFIX_DEFINITIONS = (b".R", b"DEFER", b"IS", b"SAMESTR?")

PARSE_FIRST_LINE = 71
PARSE_LAST_LINE = 115
PARSE_SHA256 = "a59c8811eef09b2a1bd31b5c0801b68a29cf1434c67bdc17a63d15e60d69a99c"
PARSE_GIT_BLOB = "fbfea6100b2dff8925dde073a7bd35a3f88544dc"
PARSE_DEFINITIONS = (
    b"NAMEBUF",
    b"PATHBUF",
    b"PN-LEN",
    b"PARSE-NAME",
    b"NEEDS",
    b"ASSERT",
    b".DEPTH",
    b"0>=",
)

ALLOCATOR_FIRST_LINE = 116
ALLOCATOR_LAST_LINE = 545
ALLOCATOR_SHA256 = "0a7d819a0a17ab96378771f69e6ca3dbf2bc2570028977a713bcba0742e22106"
ALLOCATOR_GIT_BLOB = "46dcb6e2c82d57904f7d92d43292bf3670ba5347"
ALLOCATOR_DEFINITIONS = (
    b"/ALLOC-HDR",
    b"ALLOC-MAGIC",
    b"HEAP-BASE",
    b"HEAP-FREE",
    b"HEAP-INIT",
    b"?DICT-ROOM",
    b"MEM-SIZE",
    b"MICRO-CORE?",
    b"FULL-CORE?",
    b"N-FULL-CORES",
    b"A-PREV",
    b"A-CURR",
    b"A-SIZE",
    b"HEAP-GUARD",
    b"LATE-DICT-RESERVE",
    b"HEAP-SETUP",
    b"(LINK-PREV!)",
    b"?CORE0",
    b"(BANK0-ALLOCATE)",
    b"(COALESCE)",
    b"(BANK0-FREE)",
    b"R-BLK",
    b"R-OLD",
    b"R-NEW",
    b"(TRY-GROW)",
    b"(BANK0-RESIZE)",
    b"HEAP-FREE-BYTES",
    b"HEAP-FRAG",
    b"HEAP-LARGEST",
    b"HEAP-CHECK",
    b".HEAP",
    b"HEAP-VERIFY",
)

SNAPSHOT_FIRST_LINE = 546
SNAPSHOT_LAST_LINE = 617
SNAPSHOT_SHA256 = "9380a7828dfaae383501cee5566f058b783c85ce450763e091d52e7d19c17d56"
SNAPSHOT_GIT_BLOB = "3a78ac1da4d8df75dfa0d31bd3b49dee029592ea"
SNAPSHOT_DEFINITIONS = (
    b"MARKER",
    b"(ENTRY>NAME)",
    b"FG-A",
    b"FG-L",
    b"FORGET",
)

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


def _load_exceptions(
    runtime: MegaForthRuntime | None = None,
) -> MegaForthRuntime:
    slices = (
        (
            PREFIX_FIXTURE,
            PREFIX_FIRST_LINE,
            PREFIX_LAST_LINE,
            PREFIX_SHA256,
            PREFIX_GIT_BLOB,
            PREFIX_DEFINITIONS,
        ),
        (
            PARSE_FIXTURE,
            PARSE_FIRST_LINE,
            PARSE_LAST_LINE,
            PARSE_SHA256,
            PARSE_GIT_BLOB,
            PARSE_DEFINITIONS,
        ),
        (
            ALLOCATOR_FIXTURE,
            ALLOCATOR_FIRST_LINE,
            ALLOCATOR_LAST_LINE,
            ALLOCATOR_SHA256,
            ALLOCATOR_GIT_BLOB,
            ALLOCATOR_DEFINITIONS,
        ),
        (
            SNAPSHOT_FIXTURE,
            SNAPSHOT_FIRST_LINE,
            SNAPSHOT_LAST_LINE,
            SNAPSHOT_SHA256,
            SNAPSHOT_GIT_BLOB,
            SNAPSHOT_DEFINITIONS,
        ),
        (
            EXCEPTION_FIXTURE,
            EXCEPTION_FIRST_LINE,
            EXCEPTION_LAST_LINE,
            EXCEPTION_SHA256,
            EXCEPTION_GIT_BLOB,
            EXCEPTION_DEFINITIONS,
        ),
    )

    if runtime is None:
        runtime = MegaForthRuntime()
    for fixture, first, last, sha256, git_blob, definitions in slices:
        source = _verified_slice(
            fixture,
            first_line=first,
            last_line=last,
            sha256=sha256,
            git_blob=git_blob,
        )
        result = runtime.evaluate(
            source,
            source_name=f"kdos.f@{MEGAPAD_REVISION}:{first}-{last}",
        )
        assert tuple(word.name for word in result.definitions) == definitions

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
