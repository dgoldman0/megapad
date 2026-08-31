"""Compile-state and resumable IDL coverage at the KDOS Buffer frontier."""

from __future__ import annotations

from dataclasses import replace
import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.errors import (
    ExecutionBlocked,
    ExecutionError,
    SourceError,
    StepBudgetExceeded,
)
from simulator.ir import Idle, Return
from simulator.memory import MemoryAccessError
from simulator.runtime import (
    BlockedExecution,
    ColonDefinition,
    DirectiveKind,
    ExecutionResult,
    IdleWake,
    MegaForthRuntime,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).parent / "fixtures" / "kdos-idle-2782-2796.f"
FIRST_LINE = 2782
LAST_LINE = 2796
SOURCE_SHA256 = "a2f9030f6bf1f3e1bd7c17dfcbd9695b2b6ae844844da48c322ee01ba3a0ab7c"
SOURCE_GIT_BLOB = "1d89babfba5514cd1b969aca4ee7a9e9b0f37f70"


def _git_blob_id(payload: bytes) -> str:
    framed = f"blob {len(payload)}\0".encode() + payload
    return hashlib.sha1(framed).hexdigest()


def _load_idle(runtime: MegaForthRuntime) -> None:
    runtime.evaluate(FIXTURE.read_bytes(), source_name=FIXTURE.name)


def test_exact_idle_slice_compiles_one_semantic_boundary_without_a_raw_byte() -> None:
    source = FIXTURE.read_bytes()
    lines = KDOS_SOURCE.read_bytes().splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert lines[LAST_LINE] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert len(source.splitlines()) == 15
    assert len(source) == 616
    assert hashlib.sha256(source).hexdigest() == SOURCE_SHA256
    assert _git_blob_id(source) == SOURCE_GIT_BLOB

    runtime = MegaForthRuntime()
    here_before = runtime.dictionary.here
    result = runtime.evaluate(source, source_name=FIXTURE.name)
    idle = runtime.find("IDLE")
    left = runtime.find("[")
    right = runtime.find("]")
    c_comma = runtime.find("C,")

    assert idle is not None
    assert left is not None and left.immediate
    assert right is not None and not right.immediate
    assert c_comma is not None and not c_comma.immediate
    assert [word.name for word in result.definitions] == [b"IDLE"]
    assert result.semantic_steps == 1
    assert isinstance(idle.implementation, ColonDefinition)
    assert idle.implementation.operations == (Idle(), Return())
    assert runtime.dictionary.here - here_before == runtime.dictionary.definition_size(
        b"IDLE"
    )
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_open_definition_survives_bracketed_interpretation_across_lines() -> None:
    runtime = MegaForthRuntime()
    result = runtime.evaluate(
        b": BRACKETED [\n3 4 + DROP\n]\n9 ;",
        source_name="bracketed.f",
    )
    context = runtime.new_context()

    assert [word.name for word in result.definitions] == [b"BRACKETED"]
    assert result.semantic_steps == 2
    assert runtime.execute("BRACKETED", context=context).semantic_steps == 2
    assert context.data.snapshot() == (9,)
    assert context.returns.snapshot() == ()


def test_c_comma_admits_any_cell_whose_emitted_low_byte_is_idl() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": HIGH-IDLE [ 0x100 C, ] ;")
    word = runtime.find("HIGH-IDLE")

    assert word is not None
    assert isinstance(word.implementation, ColonDefinition)
    assert word.implementation.operations == (Idle(), Return())


def test_bracketed_raw_emission_fails_closed_but_top_level_c_comma_stays_raw() -> None:
    runtime = MegaForthRuntime()
    runtime.evaluate(b": KEPT 7 ;")
    kept = runtime.find("KEPT")
    assert kept is not None
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    source = b": BAD [ 0xFF C, ] ;"

    with pytest.raises(SourceError, match="supports only MP64 IDL byte 0") as caught:
        runtime.evaluate(source, source_name="raw-op.f")

    assert caught.value.location.line == 1
    assert caught.value.location.column == source.index(b"C,")
    assert runtime.find("BAD") is None
    assert runtime.find("KEPT") is kept
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert runtime.main_context.data.snapshot() == ()

    context = runtime.new_context()
    context.data.push(0x1AB)
    runtime.execute("C,", context=context)
    assert runtime.memory.read8(here_before) == 0xAB
    assert runtime.dictionary.here == here_before + 1


@pytest.mark.parametrize(
    ("source", "message"),
    (
        (b": BAD [ 0 C, ;", "SEMICOLON is compile-only"),
        (b": BAD [ 0 C,", "has no terminating ;"),
        (b": BAD ] ;", "cannot be compiled until persistent STATE"),
        (b"]", "] requires an open definition"),
    ),
)
def test_bracket_state_misuse_never_publishes_a_partial_definition(
    source: bytes,
    message: str,
) -> None:
    runtime = MegaForthRuntime()
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    with pytest.raises(SourceError, match=message):
        runtime.evaluate(source, source_name="bad-bracket.f")

    assert runtime.find("BAD") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before


def test_non_immediate_non_executable_directive_fails_during_compilation() -> None:
    runtime = MegaForthRuntime()
    runtime.define_directive(
        "HOST-DIRECTIVE",
        DirectiveKind.LEFT_BRACKET,
        immediate=False,
    )
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    with pytest.raises(SourceError, match="non-executable directive"):
        runtime.evaluate(b": BAD HOST-DIRECTIVE ;", source_name="directive.f")

    assert runtime.find("BAD") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before


def test_nested_idle_preserves_semantic_continuations_until_interrupt_wake() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": INNER 2 IDLE 3 ; : OUTER 1 INNER 4 ;")
    context = runtime.new_context()
    context.returns.push(0xA5)

    blocked = runtime.run_until_blocked("OUTER", context=context)
    assert isinstance(blocked, BlockedExecution)
    assert blocked.semantic_steps == 5
    assert context.data.snapshot() == (1, 2)
    assert context.returns.depth() == 4
    assert context.returns.snapshot()[0] == 0xA5
    assert context.suspended
    assert not context.reusable

    with pytest.raises(ExecutionError, match="suspended"):
        runtime.evaluate(b"1", context=context)
    with pytest.raises(ExecutionError, match="suspended"):
        runtime.run_until_blocked("OUTER", context=runtime.new_context())

    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    completed = runtime.resume(blocked.suspension, receipt)
    assert isinstance(completed, ExecutionResult)
    assert completed.semantic_steps == 10
    assert context.data.snapshot() == (1, 2, 3, 4)
    assert context.returns.snapshot() == (0xA5,)
    assert context.reusable
    assert not context.suspended

    with pytest.raises(ExecutionError, match="stale"):
        runtime.resume(blocked.suspension, receipt)


def test_each_idle_requires_a_distinct_one_shot_wake_receipt() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": TWICE IDLE IDLE ;")
    context = runtime.new_context()

    first = runtime.run_until_blocked("TWICE", context=context)
    assert isinstance(first, BlockedExecution)
    assert first.semantic_steps == 2
    cloned_handle = replace(first.suspension)
    assert cloned_handle == first.suspension
    assert cloned_handle is not first.suspension
    with pytest.raises(ExecutionError, match="stale"):
        runtime.deliver_idle_wake(cloned_handle, IdleWake.INTERRUPT)
    interrupt = runtime.deliver_idle_wake(first.suspension, IdleWake.INTERRUPT)
    cloned_receipt = replace(interrupt)
    assert cloned_receipt == interrupt
    assert cloned_receipt is not interrupt
    with pytest.raises(ExecutionError, match="stale, foreign, or already consumed"):
        runtime.resume(first.suspension, cloned_receipt)
    with pytest.raises(ExecutionError, match="already has"):
        runtime.deliver_idle_wake(first.suspension, IdleWake.DMA)

    second = runtime.resume(first.suspension, interrupt)
    assert isinstance(second, BlockedExecution)
    assert second.semantic_steps == 5
    assert second.suspension != first.suspension
    with pytest.raises(ExecutionError, match="stale"):
        runtime.resume(second.suspension, interrupt)

    dma = runtime.deliver_idle_wake(second.suspension, IdleWake.DMA)
    completed = runtime.resume(second.suspension, dma)
    assert isinstance(completed, ExecutionResult)
    assert completed.semantic_steps == 7
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()


def test_execute_exposes_block_and_cancel_unwinds() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    context = runtime.new_context()
    context.returns.push(0x55)

    with pytest.raises(ExecutionBlocked) as caught:
        runtime.execute("IDLE", context=context)

    assert caught.value.semantic_steps == 1
    assert context.suspended
    runtime.cancel_suspension(caught.value.suspension)
    assert context.returns.snapshot() == (0x55,)
    assert context.reusable
    with pytest.raises(ExecutionError, match="stale"):
        runtime.cancel_suspension(caught.value.suspension)


def test_suspension_leases_stacks_and_dictionary_until_cancel_or_resume() -> None:
    runtime = MegaForthRuntime()
    checkpoint = runtime.dictionary.checkpoint()
    _load_idle(runtime)
    idle = runtime.find("IDLE")
    assert idle is not None
    context = runtime.new_context()
    blocked = runtime.run_until_blocked("IDLE", context=context)
    assert isinstance(blocked, BlockedExecution)

    with pytest.raises(ExecutionError, match="dictionary"):
        runtime.define_constant("TOO-EARLY", 1)
    with pytest.raises(ExecutionError, match="dictionary"):
        runtime.dictionary.rollback(checkpoint)
    with pytest.raises(ExecutionError, match="dictionary"):
        runtime.set_dictionary_fault_xt(idle.xt)
    assert runtime.find("IDLE") is idle
    assert runtime.dictionary_fault_xt == 0
    context.data.push(99)
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    with pytest.raises(ExecutionError, match="data stack changed"):
        runtime.resume(blocked.suspension, receipt)

    runtime.cancel_suspension(blocked.suspension)
    assert context.data.snapshot() == (99,)
    assert context.returns.snapshot() == ()
    assert context.reusable
    runtime.define_constant("AFTER-CANCEL", 2)

    blocked = runtime.run_until_blocked("IDLE", context=context)
    assert isinstance(blocked, BlockedExecution)
    context.returns.push(0x77)
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.DMA)
    with pytest.raises(ExecutionError, match="return stack changed"):
        runtime.resume(blocked.suspension, receipt)
    runtime.cancel_suspension(blocked.suspension)
    assert context.returns.snapshot() == ()
    assert context.reusable


def test_rp_capture_remains_valid_across_idle_and_clears_on_completion() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": CAPTURE-IDLE RP@ DROP IDLE 7 ;")
    context = runtime.main_context

    blocked = runtime.run_until_blocked("CAPTURE-IDLE", context=context)
    assert isinstance(blocked, BlockedExecution)
    assert context.host_control_fault is None
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.DMA)
    completed = runtime.resume(blocked.suspension, receipt)

    assert isinstance(completed, ExecutionResult)
    assert context.data.snapshot() == (7,)
    assert context.returns.snapshot() == ()
    assert context.host_control_fault is None
    assert context.reusable


def test_cancel_after_rp_capture_fails_the_context_closed() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": LEAK-RP RP@ IDLE ;")
    context = runtime.main_context
    blocked = runtime.run_until_blocked("LEAK-RP", context=context)
    assert isinstance(blocked, BlockedExecution)
    leaked_pointer = context.data.snapshot()
    blocked_returns = context.returns.snapshot()
    context.returns.restore(blocked_returns)
    assert context.returns.snapshot() == blocked_returns

    runtime.cancel_suspension(blocked.suspension)

    assert len(leaked_pointer) == 1
    assert context.data.snapshot() == leaked_pointer
    assert context.returns.snapshot() == ()
    assert context.host_control_fault == "ExecutionError"
    assert not context.reusable
    with pytest.raises(ExecutionError, match="not reusable"):
        runtime.execute("DROP", context=context)


def test_identical_return_restore_cannot_hide_rp_capture_from_resumed_failure() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": LEAK-THEN-FAIL RP@ IDLE -1 @ ;")
    context = runtime.main_context
    blocked = runtime.run_until_blocked("LEAK-THEN-FAIL", context=context)
    assert isinstance(blocked, BlockedExecution)
    leaked_pointer = context.data.snapshot()
    blocked_returns = context.returns.snapshot()
    context.returns.restore(blocked_returns)
    assert context.returns.snapshot() == blocked_returns
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.DMA)

    with pytest.raises(MemoryAccessError):
        runtime.resume(blocked.suspension, receipt)

    assert context.data.snapshot() == leaked_pointer + (MASK64,)
    assert context.returns.snapshot() == ()
    assert context.host_control_fault == "AddressOverflowError"
    assert not context.suspended
    assert not context.reusable


def test_second_idle_publication_failure_after_rp_capture_fails_closed() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": TWO-IDLES RP@ IDLE IDLE ;")
    context = runtime.main_context
    blocked = runtime.run_until_blocked("TWO-IDLES", context=context)
    assert isinstance(blocked, BlockedExecution)
    leaked_pointer = context.data.snapshot()
    runtime._next_suspension_sequence = MASK64 + 1
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)

    with pytest.raises(ExecutionError, match="suspension identity space exhausted"):
        runtime.resume(blocked.suspension, receipt)

    assert len(leaked_pointer) == 1
    assert context.data.snapshot() == leaked_pointer
    assert context.returns.snapshot() == ()
    assert context.host_control_fault == "ExecutionError"
    assert not context.suspended
    assert not context.reusable
    with pytest.raises(ExecutionError, match="stale"):
        runtime.cancel_suspension(blocked.suspension)
    runtime.define_constant("RUNTIME-STILL-OPEN", 1)


def test_resume_retains_the_original_budget_and_guard_on_failure() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)
    runtime.evaluate(b": TWICE IDLE IDLE ; : FAIL-AFTER IDLE -1 @ ;")

    budget_context = runtime.new_context()
    budget_context.returns.push(0x11)
    blocked = runtime.run_until_blocked(
        "TWICE",
        context=budget_context,
        step_budget=4,
    )
    assert isinstance(blocked, BlockedExecution)
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.INTERRUPT)
    with pytest.raises(StepBudgetExceeded):
        runtime.resume(blocked.suspension, receipt)
    assert budget_context.returns.snapshot() == (0x11,)
    assert budget_context.reusable

    fault_context = runtime.new_context()
    fault_context.returns.push(0x22)
    blocked = runtime.run_until_blocked("FAIL-AFTER", context=fault_context)
    assert isinstance(blocked, BlockedExecution)
    receipt = runtime.deliver_idle_wake(blocked.suspension, IdleWake.DMA)
    with pytest.raises(MemoryAccessError):
        runtime.resume(blocked.suspension, receipt)
    assert fault_context.data.snapshot() == (MASK64,)
    assert fault_context.returns.snapshot() == (0x22,)
    assert fault_context.reusable


def test_idle_fails_explicitly_in_the_unresumable_evaluator_boundary() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)

    with pytest.raises(ExecutionError, match="cannot suspend source evaluation"):
        runtime.evaluate(b"IDLE", source_name="interpret-idle.f")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.main_context.reusable


def test_idle_fails_explicitly_across_a_nested_python_dispatch() -> None:
    runtime = MegaForthRuntime()
    _load_idle(runtime)

    def nested_idle(nested_context) -> None:
        runtime.execute("IDLE", context=nested_context)

    runtime.define_primitive("NESTED-IDLE", nested_idle)
    context = runtime.new_context()

    with pytest.raises(ExecutionError, match="cannot suspend .*nested host dispatch"):
        runtime.execute("NESTED-IDLE", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert context.reusable
