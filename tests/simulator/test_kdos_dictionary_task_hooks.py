"""Contiguous unchanged-source acceptance for KDOS dictionary/task hooks."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import u64
from simulator.errors import ExecutionError, ForthAbort
from simulator.memory import SparseAddressSpace
from simulator.runtime import ExecutionContext, MegaForthRuntime
from simulator.stacks import ReturnStack
from tests.simulator.test_kdos_exceptions import _load_exceptions


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / (
    "kdos-dictionary-task-hooks-676-719.f"
)

MEGAPAD_REVISION = "9576065668114ffdf9b08c015cf4d16c8b2e6e89"
KDOS_GIT_BLOB = "4580b4075b3114ef6e5b2c8121b6e4fa1cfb2c70"
FIRST_LINE = 676
LAST_LINE = 719
SLICE_SHA256 = "45d4378e494a235d9d91f7c4c11e4d15830b49161c14c88f3699178d69781b8e"
SLICE_GIT_BLOB = "0d956cf70f2697737c604d92c8cbb00543343021"
DEFINITIONS = (
    b"U-DICT-E-FULL",
    b"_KDOS-DICT-FAULT",
    b"_BIOS-BACKGROUND-XT",
    b"_BIOS-BACKGROUND2-XT",
    b"_BIOS-BACKGROUND3-XT",
    b"_BIOS-TASK-STOP-XT",
    b"_TASK-HANDLER-RESET",
    b"BACKGROUND",
    b"BACKGROUND2",
    b"BACKGROUND3",
    b"TASK-STOP",
)
TASK_CAPTURES = (
    ("BACKGROUND", "_BIOS-BACKGROUND-XT"),
    ("BACKGROUND2", "_BIOS-BACKGROUND2-XT"),
    ("BACKGROUND3", "_BIOS-BACKGROUND3-XT"),
    ("TASK-STOP", "_BIOS-TASK-STOP-XT"),
)


def _git_blob_id(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _load_hooks(
    runtime: MegaForthRuntime | None = None,
) -> tuple[MegaForthRuntime, dict[str, object]]:
    runtime = _load_exceptions(runtime)
    bios_words = {}
    for name, _capture in TASK_CAPTURES:
        word = runtime.find(name)
        assert word is not None
        bios_words[name] = word

    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )

    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime, bios_words


@pytest.fixture
def loaded_hooks() -> tuple[MegaForthRuntime, dict[str, object]]:
    return _load_hooks()


def _constant_value(runtime: MegaForthRuntime, name: str) -> int:
    context = runtime.new_context()
    runtime.execute(name, context=context)
    value = context.data.pop()
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    return value


def _handler_cells(runtime: MegaForthRuntime) -> tuple[int, int, int, int]:
    table = runtime.find("_TASK-HANDLERS")
    assert table is not None
    return tuple(
        runtime.memory.read64(table.body_address + slot * 8)
        for slot in range(4)
    )


def test_hook_load_captures_the_live_pre_shadow_bios_task_words(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, bios_words = loaded_hooks
    captured_xts = []
    for name, capture in TASK_CAPTURES:
        bios_word = bios_words[name]
        captured_xt = _constant_value(runtime, capture)
        captured_xts.append(captured_xt)
        assert captured_xt != 0
        assert captured_xt == bios_word.xt
        assert runtime.dictionary.resolve(captured_xt) is bios_word

        wrapper = runtime.find(name)
        assert wrapper is not None
        assert wrapper is not bios_word
        assert wrapper.xt != captured_xt

    assert len(set(captured_xts)) == len(captured_xts)
    assert _constant_value(runtime, "U-DICT-E-FULL") == u64(-8)
    fault = runtime.find("_KDOS-DICT-FAULT")
    assert fault is not None
    assert runtime.dictionary_fault_xt == fault.xt


@pytest.mark.parametrize("slot", (1, 2, 3))
def test_task_handler_reset_changes_only_the_selected_background_cell(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
    slot: int,
) -> None:
    runtime, _bios_words = loaded_hooks
    table = runtime.find("_TASK-HANDLERS")
    assert table is not None
    for index, value in enumerate((101, 102, 103, 104)):
        runtime.memory.write64(table.body_address + index * 8, value)

    runtime.main_context.data.push(slot)
    runtime.execute("_TASK-HANDLER-RESET")

    expected = [101, 102, 103, 104]
    expected[slot] = 0
    assert _handler_cells(runtime) == tuple(expected)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_kdos_dictionary_fault_throws_through_the_existing_guest_catch(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, _bios_words = loaded_hooks

    runtime.evaluate(b"' _KDOS-DICT-FAULT CATCH")

    assert runtime.main_context.data.snapshot() == (u64(-8),)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""
    assert _handler_cells(runtime)[0] == 0


def test_failing_c_comma_enters_the_hook_before_mutating_dictionary_state(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, _bios_words = loaded_hooks
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ;")
    context = runtime.main_context
    guarded_ceiling = context.data.pointer - 256
    runtime.dictionary.allot(guarded_ceiling - runtime.dictionary.here)
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_bytes = runtime.memory.read_bytes(saved_here, 16)

    runtime.evaluate(b"' FAIL-C-COMMA CATCH")

    assert context.data.snapshot() == (u64(-8),)
    assert context.returns.snapshot() == ()
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest == saved_latest
    assert runtime.memory.read_bytes(saved_here, len(saved_bytes)) == saved_bytes
    assert runtime.drain_uart_output() == b""
    assert _handler_cells(runtime)[0] == 0


def test_pre_shadow_task_entry_points_fail_without_consuming_arguments(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, bios_words = loaded_hooks
    target = runtime.find("_KDOS-DICT-FAULT")
    assert target is not None

    for name in ("BACKGROUND", "BACKGROUND2", "BACKGROUND3"):
        context = runtime.new_context()
        context.data.push(target.xt)
        with pytest.raises(ExecutionError, match="cooperative task scheduling"):
            runtime.execute(bios_words[name].xt, context=context)
        assert context.data.snapshot() == (target.xt,)
        assert context.returns.snapshot() == ()

    for slot in (0, 1, 4):
        context = runtime.new_context()
        context.data.push(slot)
        message = "slot must" if slot != 1 else "cooperative task scheduling"
        with pytest.raises(ExecutionError, match=message):
            runtime.execute(bios_words["TASK-STOP"].xt, context=context)
        assert context.data.snapshot() == (slot,)
        assert context.returns.snapshot() == ()


def test_shadow_task_wrappers_expose_only_the_source_order_reached_before_failure(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, _bios_words = loaded_hooks
    table = runtime.find("_TASK-HANDLERS")
    target = runtime.find("_KDOS-DICT-FAULT")
    assert table is not None
    assert target is not None

    for slot, name in ((1, "BACKGROUND"), (2, "BACKGROUND2"), (3, "BACKGROUND3")):
        for index, value in enumerate((101, 102, 103, 104)):
            runtime.memory.write64(table.body_address + index * 8, value)
        runtime.main_context.data.push(target.xt)

        with pytest.raises(ExecutionError, match="cooperative task scheduling"):
            runtime.execute(name)

        expected = [101, 102, 103, 104]
        expected[slot] = 0
        assert _handler_cells(runtime) == tuple(expected)
        assert runtime.main_context.data.snapshot() == (target.xt,)
        assert runtime.main_context.returns.snapshot() == ()
        runtime.main_context.data.clear()

    for index, value in enumerate((201, 202, 203, 204)):
        runtime.memory.write64(table.body_address + index * 8, value)
    runtime.main_context.data.push(2)

    with pytest.raises(ExecutionError, match="cooperative task scheduling"):
        runtime.execute("TASK-STOP")

    # Source orders cancellation before handler reset.  The unavailable BIOS
    # seam cannot return, so the reset is intentionally unreachable for now.
    assert _handler_cells(runtime) == (201, 202, 203, 204)
    assert runtime.main_context.data.snapshot() == (2, 2)
    assert runtime.main_context.returns.snapshot() == ()


def test_dictionary_fault_outside_catch_retains_kdos_diagnostic_and_aborts(
    loaded_hooks: tuple[MegaForthRuntime, dict[str, object]],
) -> None:
    runtime, _bios_words = loaded_hooks

    with pytest.raises(ForthAbort):
        runtime.execute("_KDOS-DICT-FAULT")

    assert runtime.drain_uart_output() == b"dictionary overflow"
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


@pytest.mark.parametrize(
    ("source", "here_delta", "absent_name"),
    (
        (b"1 ALLOT", 0, None),
        (b"165 ,", 0, None),
        (b"165 C,", 0, None),
        (b"TALIGN", 1, None),
        (b"BL WORD payload", 0, None),
        (b"123 CONSTANT LATE-CONSTANT", 0, "LATE-CONSTANT"),
        (b"CREATE LATE-CREATE", 0, "LATE-CREATE"),
        (b"VARIABLE LATE-VARIABLE", 0, "LATE-VARIABLE"),
        (b": LATE-COLON ;", 0, "LATE-COLON"),
    ),
)
def test_every_current_dictionary_emitter_routes_nested_faults_to_guest_catch(
    source: bytes,
    here_delta: int,
    absent_name: str | None,
) -> None:
    runtime, _bios_words = _load_hooks()

    def evaluate_faulting_source(context) -> None:
        runtime.evaluate(source, source_name="nested-fault.f", context=context)

    action = runtime.define_primitive("HOST-DICTIONARY-FAULT", evaluate_faulting_source)
    context = runtime.main_context
    target_here = context.data.pointer - 256 + here_delta
    runtime.dictionary.allot(target_here - runtime.dictionary.here)
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_bytes = runtime.memory.read_bytes(saved_here, 32)
    context.data.push(action.xt)

    runtime.execute("CATCH")

    assert context.data.snapshot() == (u64(-8),)
    assert context.returns.snapshot() == ()
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest == saved_latest
    assert runtime.memory.read_bytes(saved_here, len(saved_bytes)) == saved_bytes
    assert runtime.drain_uart_output() == b""
    assert _handler_cells(runtime)[0] == 0
    if absent_name is not None:
        assert runtime.find(absent_name) is None


@pytest.mark.parametrize("callback_kind", ("zero", "returning"))
def test_zero_or_returning_dictionary_fault_callback_aborts_fail_closed(
    callback_kind: str,
) -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ; : RETURNING-HOOK ;")
    hook = runtime.find("RETURNING-HOOK")
    assert hook is not None
    callback_xt = 0 if callback_kind == "zero" else hook.xt
    runtime.main_context.data.push(callback_xt)
    runtime.execute("DICT-FAULT-XT!")

    context = runtime.main_context
    target_here = context.data.pointer - 256
    runtime.dictionary.allot(target_here - runtime.dictionary.here)
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_bytes = runtime.memory.read_bytes(saved_here, 16)

    with pytest.raises(ForthAbort, match="dictionary fault callback returned"):
        runtime.execute("FAIL-C-COMMA")

    assert runtime.drain_uart_output() == b"dictionary overflow\r\n"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest == saved_latest
    assert runtime.memory.read_bytes(saved_here, len(saved_bytes)) == saved_bytes


def test_exact_guarded_fit_does_not_call_the_installed_fault_callback() -> None:
    runtime = MegaForthRuntime()
    calls = []

    def returning_hook(_context) -> None:
        calls.append("called")

    hook = runtime.define_primitive("HOST-RETURNING-HOOK", returning_hook)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    target_here = context.data.pointer - 257
    runtime.dictionary.allot(target_here - runtime.dictionary.here)
    context.data.push(0xA5)

    runtime.execute("C,")

    assert calls == []
    assert runtime.dictionary.here == target_here + 1
    assert runtime.memory.read8(target_here) == 0xA5
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_invalid_rollback_pair_is_still_present_when_the_callback_runs() -> None:
    runtime = MegaForthRuntime()
    observed = []

    def capture_pair(context) -> None:
        observed.append(context.data.snapshot())

    hook = runtime.define_primitive("HOST-CAPTURE-ROLLBACK", capture_pair)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    invalid_here = runtime.dictionary.here + 1
    live_latest = runtime.dictionary.latest
    context.data.push(invalid_here)
    context.data.push(live_latest)

    with pytest.raises(ForthAbort):
        runtime.execute("DICT-ROLLBACK")

    assert observed == [(invalid_here, live_latest)]
    assert runtime.drain_uart_output() == b"dictionary overflow\r\n"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_unresolved_nonzero_fault_xt_remains_an_execution_target_error() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ;")
    unresolved_xt = 0xDEAD_BEEF
    runtime.main_context.data.push(unresolved_xt)
    runtime.execute("DICT-FAULT-XT!")
    assert runtime.dictionary_fault_xt == unresolved_xt

    context = runtime.main_context
    target_here = context.data.pointer - 256
    runtime.dictionary.allot(target_here - runtime.dictionary.here)
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest
    saved_bytes = runtime.memory.read_bytes(saved_here, 16)

    with pytest.raises(ExecutionError, match="not a live execution token"):
        runtime.execute("FAIL-C-COMMA")

    assert runtime.drain_uart_output() == b""
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable
    assert runtime.dictionary.here == saved_here
    assert runtime.dictionary.latest == saved_latest
    assert runtime.memory.read_bytes(saved_here, len(saved_bytes)) == saved_bytes


def test_recursive_fault_inside_a_colon_hook_aborts_without_reentry() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(
        b": RECURSIVE-DICTIONARY-HOOK 1 C, ; "
        b": FAIL-C-COMMA 165 C, ;"
    )
    hook = runtime.find("RECURSIVE-DICTIONARY-HOOK")
    assert hook is not None
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")

    context = runtime.main_context
    target_here = context.data.pointer - 256
    runtime.dictionary.allot(target_here - runtime.dictionary.here)
    saved_latest = runtime.dictionary.latest

    with pytest.raises(ForthAbort, match="dictionary fault callback returned"):
        runtime.execute("FAIL-C-COMMA", step_budget=128)

    assert runtime.drain_uart_output() == b"dictionary overflow\r\n"
    assert runtime.dictionary.here == target_here
    assert runtime.dictionary.latest == saved_latest
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize("nested_kind", ("execute", "evaluate"))
def test_primitive_hook_can_throw_across_a_nested_public_dispatch(
    nested_kind: str,
) -> None:
    runtime, _bios_words = _load_hooks()
    runtime.evaluate(
        b": GUEST-DICTIONARY-THROW -8 THROW ; "
        b": FAIL-C-COMMA 165 C, ;"
    )

    def nested_guest_throw(context) -> None:
        if nested_kind == "execute":
            runtime.execute("GUEST-DICTIONARY-THROW", context=context)
        else:
            # The trailing literal proves evaluation cannot continue after
            # guest THROW crosses the nested Python boundary.
            runtime.evaluate(b"-8 THROW 123", context=context)

    hook = runtime.define_primitive("HOST-NESTED-THROW", nested_guest_throw)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )
    action = runtime.find("FAIL-C-COMMA")
    assert action is not None
    context.data.push(action.xt)

    runtime.execute("CATCH")

    assert context.data.snapshot() == (u64(-8),)
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""
    assert _handler_cells(runtime)[0] == 0


def test_nested_fault_preserves_rp_capture_evidence_for_a_host_escape() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": CAPTURE-THEN-FAULT RP@ 165 C, ;")

    def escaping_hook(_context) -> None:
        raise ExecutionError("fault hook escaped to the host")

    def nested_fault(context) -> None:
        runtime.execute("CAPTURE-THEN-FAULT", context=context)

    hook = runtime.define_primitive("HOST-ESCAPING-HOOK", escaping_hook)
    action = runtime.define_primitive("HOST-NESTED-FAULT", nested_fault)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ExecutionError, match="escaped to the host"):
        runtime.execute(action.xt)

    assert context.data.depth() == 1
    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "ExecutionError"


def test_successful_nested_dispatch_preserves_rp_capture_for_outer_escape() -> None:
    runtime = MegaForthRuntime()

    def nested_capture_then_escape(context) -> None:
        runtime.execute("RP@", context=context)
        raise ExecutionError("outer dispatch escaped")

    runtime.define_primitive(
        "HOST-NESTED-CAPTURE-ESCAPE",
        nested_capture_then_escape,
    )
    runtime.evaluate(b": OUTER-CAPTURE-ESCAPE HOST-NESTED-CAPTURE-ESCAPE ;")
    context = runtime.main_context

    with pytest.raises(ExecutionError, match="outer dispatch escaped"):
        runtime.execute("OUTER-CAPTURE-ESCAPE")

    assert context.data.depth() == 1
    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "ExecutionError"


def test_complete_evaluation_preserves_earlier_rp_capture_until_host_escape() -> None:
    runtime = MegaForthRuntime()

    def escape_later(_context) -> None:
        raise ExecutionError("later evaluation token escaped")

    runtime.define_primitive("HOST-LATER-ESCAPE", escape_later)
    runtime.evaluate(b": CAPTURE-EVALUATION-RP RP@ ;")
    context = runtime.main_context

    with pytest.raises(ExecutionError, match="later evaluation token escaped"):
        runtime.evaluate(b"CAPTURE-EVALUATION-RP HOST-LATER-ESCAPE")

    assert context.data.depth() == 1
    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "ExecutionError"


def test_top_level_dictionary_hook_escape_retains_prior_evaluation_rp_capture() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": CAPTURE-EVALUATION-RP RP@ ;")

    def escaping_hook(_context) -> None:
        raise ExecutionError("top-level dictionary hook escaped")

    hook = runtime.define_primitive("HOST-ESCAPING-HOOK", escaping_hook)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ExecutionError, match="dictionary hook escaped"):
        runtime.evaluate(
            b"CAPTURE-EVALUATION-RP : NO-DIRECT-CAPACITY ;"
        )

    # The colon directive preflights its header directly in the evaluator, so
    # the earlier RP@ value remains as evidence on the data stack.
    assert context.data.depth() == 1
    assert context.returns.snapshot() == ()
    assert not context.reusable
    assert context.host_control_fault == "ExecutionError"
    assert runtime.find("NO-DIRECT-CAPACITY") is None


def test_guest_throw_crosses_an_intervening_context_and_cleans_it() -> None:
    runtime = _load_exceptions()
    outer = runtime.main_context
    intervening = runtime.new_context()
    runtime.evaluate(b": A-CROSS-CONTEXT-THROW -42 THROW ;")

    def enter_outer_from_intervening(_context) -> None:
        runtime.execute("A-CROSS-CONTEXT-THROW", context=outer)

    runtime.define_primitive(
        "HOST-ENTER-OUTER-THROW",
        enter_outer_from_intervening,
    )
    runtime.evaluate(b": B-CROSS-CONTEXT-BRIDGE HOST-ENTER-OUTER-THROW ;")

    def enter_intervening(_context) -> None:
        runtime.execute("B-CROSS-CONTEXT-BRIDGE", context=intervening)

    action = runtime.define_primitive(
        "HOST-ENTER-INTERVENING",
        enter_intervening,
    )
    outer.data.push(action.xt)

    runtime.execute("CATCH")

    assert outer.data.snapshot() == (u64(-42),)
    assert outer.returns.snapshot() == ()
    assert intervening.data.snapshot() == ()
    assert intervening.returns.snapshot() == ()
    assert intervening.reusable


def test_foreign_context_abort_does_not_clear_the_calling_task() -> None:
    runtime = MegaForthRuntime()
    caller = runtime.main_context
    aborting = runtime.new_context()
    aborting.data.push(909)

    def abort_other_context(_context) -> None:
        runtime.execute("ABORT", context=aborting)

    runtime.define_primitive(
        "HOST-ABORT-OTHER-CONTEXT",
        abort_other_context,
    )
    runtime.evaluate(b": CALL-FOREIGN-ABORT 77 HOST-ABORT-OTHER-CONTEXT ;")
    caller.data.push(11)

    with pytest.raises(ForthAbort) as caught:
        runtime.execute("CALL-FOREIGN-ABORT")

    assert caught.value.origin_context is aborting
    assert aborting.data.snapshot() == ()
    assert aborting.returns.snapshot() == ()
    assert aborting.reusable
    assert caller.data.snapshot() == (11, 77)
    assert caller.returns.snapshot() == ()
    assert caller.reusable


@pytest.mark.parametrize("intervening_kind", ("execute", "evaluate"))
def test_dictionary_fault_crosses_an_intervening_context_to_outer_catch(
    intervening_kind: str,
) -> None:
    runtime, _bios_words = _load_hooks()
    outer = runtime.main_context
    intervening = runtime.new_context()
    runtime.evaluate(b": A-CROSS-CONTEXT-FAULT 165 C, ;")

    def enter_outer_from_intervening(_context) -> None:
        runtime.execute("A-CROSS-CONTEXT-FAULT", context=outer)

    runtime.define_primitive(
        "HOST-ENTER-OUTER-FAULT",
        enter_outer_from_intervening,
    )
    runtime.evaluate(b": B-CROSS-CONTEXT-BRIDGE HOST-ENTER-OUTER-FAULT ;")

    def enter_intervening(_context) -> None:
        if intervening_kind == "execute":
            runtime.execute("B-CROSS-CONTEXT-BRIDGE", context=intervening)
        else:
            runtime.evaluate(
                b"B-CROSS-CONTEXT-BRIDGE",
                context=intervening,
            )

    action = runtime.define_primitive(
        "HOST-ENTER-INTERVENING",
        enter_intervening,
    )
    runtime.dictionary.allot(
        outer.data.pointer - 256 - runtime.dictionary.here
    )
    outer.data.push(action.xt)

    runtime.execute("CATCH")

    assert outer.data.snapshot() == (u64(-8),)
    assert outer.returns.snapshot() == ()
    assert intervening.data.snapshot() == ()
    assert intervening.returns.snapshot() == ()
    assert intervening.reusable
    assert _handler_cells(runtime)[0] == 0


def test_foreign_dictionary_request_marks_prior_intervening_rp_capture() -> None:
    runtime, _bios_words = _load_hooks()
    outer = runtime.main_context
    stack_memory = SparseAddressSpace(bank0_size=0x1000)
    intervening = ExecutionContext(
        returns=ReturnStack(
            memory=stack_memory,
            floor=0,
            empty_pointer=0x1000,
        )
    )
    runtime.evaluate(
        b": A-CROSS-CONTEXT-FAULT 165 C, ; "
        b": B-CAPTURE-EVALUATION-RP RP@ ;"
    )

    def enter_outer_from_intervening(_context) -> None:
        runtime.execute("A-CROSS-CONTEXT-FAULT", context=outer)

    runtime.define_primitive(
        "HOST-ENTER-OUTER-FAULT",
        enter_outer_from_intervening,
    )

    def enter_intervening(_context) -> None:
        runtime.evaluate(
            b"B-CAPTURE-EVALUATION-RP HOST-ENTER-OUTER-FAULT",
            context=intervening,
        )

    action = runtime.define_primitive(
        "HOST-ENTER-INTERVENING-EVALUATION",
        enter_intervening,
    )
    runtime.dictionary.allot(
        outer.data.pointer - 256 - runtime.dictionary.here
    )
    outer.data.push(action.xt)

    runtime.execute("CATCH")

    assert outer.data.snapshot() == (u64(-8),)
    assert outer.returns.snapshot() == ()
    assert intervening.data.depth() == 1
    assert intervening.returns.snapshot() == ()
    assert not intervening.reusable
    assert intervening.host_control_fault == "_DictionaryFaultRequest"
    assert _handler_cells(runtime)[0] == 0


def test_nested_other_context_routes_the_fault_to_that_context() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ;")
    other = runtime.new_context()
    observed = []

    def context_hook(context) -> None:
        observed.append(context)
        raise ExecutionError("other-context hook reached")

    def execute_in_other(_context) -> None:
        runtime.execute("FAIL-C-COMMA", context=other)

    hook = runtime.define_primitive("HOST-CONTEXT-HOOK", context_hook)
    action = runtime.define_primitive("HOST-OTHER-CONTEXT", execute_in_other)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    runtime.dictionary.allot(
        runtime.main_context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ExecutionError, match="other-context hook reached"):
        runtime.execute(action.xt)

    assert observed == [other]
    assert other.data.snapshot() == ()
    assert other.returns.snapshot() == ()
    assert other.reusable
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable


def test_direct_defining_callback_uses_its_active_dispatch_context() -> None:
    runtime = MegaForthRuntime()
    other = runtime.new_context()
    observed = []

    def context_hook(context) -> None:
        observed.append(context)
        raise ExecutionError("definition hook reached")

    def define_in_active_context(_context) -> None:
        runtime.define_constant("NO-CAPACITY", 1)

    hook = runtime.define_primitive("HOST-CONTEXT-HOOK", context_hook)
    action = runtime.define_primitive("HOST-DEFINE-CONSTANT", define_in_active_context)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    runtime.dictionary.allot(
        runtime.main_context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ExecutionError, match="definition hook reached"):
        runtime.execute(action.xt, context=other)

    assert observed == [other]
    assert runtime.find("NO-CAPACITY") is None
    assert other.data.snapshot() == ()
    assert other.returns.snapshot() == ()
    assert other.reusable


def test_primitive_hook_forth_abort_is_normalized_to_a_complete_task_reset() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ;")

    def aborting_hook(context) -> None:
        context.data.push(999)
        raise ForthAbort("host primitive requested ABORT")

    hook = runtime.define_primitive("HOST-ABORTING-HOOK", aborting_hook)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ForthAbort, match="host primitive requested ABORT"):
        runtime.execute("FAIL-C-COMMA")

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable


def test_dictionary_rollback_preflights_a_now_unsafe_valid_target() -> None:
    runtime = MegaForthRuntime()
    observed = []

    def capture_pair(context) -> None:
        observed.append(context.data.snapshot())

    hook = runtime.define_primitive("HOST-CAPTURE-ROLLBACK", capture_pair)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    saved_here = context.data.pointer - 257
    runtime.dictionary.allot(saved_here - runtime.dictionary.here)
    saved_latest = runtime.dictionary.latest
    runtime.dictionary.allot(1)
    current_here = runtime.dictionary.here
    context.data.push(saved_here)
    context.data.push(saved_latest)

    with pytest.raises(ForthAbort):
        runtime.execute("DICT-ROLLBACK")

    assert observed == [(saved_here, saved_latest)]
    assert runtime.dictionary.here == current_here
    assert runtime.dictionary.latest == saved_latest
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_already_aligned_talign_is_a_true_noop_without_preflight() -> None:
    runtime = MegaForthRuntime()
    calls = []

    def returning_hook(_context) -> None:
        calls.append("called")

    hook = runtime.define_primitive("HOST-RETURNING-HOOK", returning_hook)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    aligned_here = (context.data.pointer - 128) & ~63
    assert aligned_here > context.data.pointer - 256
    runtime.dictionary.allot(aligned_here - runtime.dictionary.here)

    runtime.execute("TALIGN")

    assert calls == []
    assert runtime.dictionary.here == aligned_here
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_zero_hook_guard_is_active_before_dict_fault_xt_store_runs() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": FAIL-C-COMMA 165 C, ;")
    assert runtime.dictionary_fault_xt == 0
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ForthAbort, match="dictionary fault callback returned"):
        runtime.execute("FAIL-C-COMMA")

    assert runtime.drain_uart_output() == b"dictionary overflow\r\n"
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_direct_definition_api_routes_capacity_failure_without_leaking_request() -> None:
    runtime = MegaForthRuntime()

    def returning_hook(_context) -> None:
        pass

    hook = runtime.define_primitive("HOST-RETURNING-HOOK", returning_hook)
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ForthAbort, match="dictionary fault callback returned"):
        runtime.define_constant("NO-DIRECT-CAPACITY", 7)

    assert runtime.find("NO-DIRECT-CAPACITY") is None
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_colon_capacity_fault_precedes_immediate_body_side_effects() -> None:
    runtime = MegaForthRuntime()
    side_effects = []

    def immediate_side_effect(_context) -> None:
        side_effects.append("ran")

    def escape_dictionary_fault(_context) -> None:
        raise ExecutionError("colon header rejected")

    runtime.define_primitive(
        "HOST-IMMEDIATE-SIDE-EFFECT",
        immediate_side_effect,
        immediate=True,
    )
    hook = runtime.define_primitive(
        "HOST-COLON-FAULT",
        escape_dictionary_fault,
    )
    runtime.main_context.data.push(hook.xt)
    runtime.execute("DICT-FAULT-XT!")
    context = runtime.main_context
    runtime.dictionary.allot(
        context.data.pointer - 256 - runtime.dictionary.here
    )

    with pytest.raises(ExecutionError, match="colon header rejected"):
        runtime.evaluate(
            b": NO-COLON-CAPACITY HOST-IMMEDIATE-SIDE-EFFECT ;"
        )

    assert side_effects == []
    assert runtime.find("NO-COLON-CAPACITY") is None
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
