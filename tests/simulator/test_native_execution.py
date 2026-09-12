"""Native execution must preserve observable source-level dispatch behavior."""

from __future__ import annotations

import pytest


pytest.importorskip("_megaforth_native")

from shared.cells import MASK64, TRUE  # noqa: E402
from simulator.diagnostics import HostedDiagnosticsService  # noqa: E402
from simulator.dictionary import HEADER_FIXED_BYTES  # noqa: E402
from simulator.errors import ExecutionError, StepBudgetExceeded  # noqa: E402
from simulator.ir import Call, Literal, Return  # noqa: E402
from simulator.memory import AddressClass  # noqa: E402
from simulator.platform import create_one_core_address_space  # noqa: E402
from simulator.runtime import MegaForthRuntime  # noqa: E402
from simulator.stacks import Continuation, ReturnStackShapeError  # noqa: E402
from simulator.timer import HostedTimerService  # noqa: E402


def _runtimes(source: bytes = b"", *, external_size: int = 0):
    runtimes = []
    for backend in ("python", "native"):
        runtime = MegaForthRuntime(
            execution_backend=backend,
            memory=create_one_core_address_space(external_size=external_size),
        )
        if source:
            runtime.evaluate(source, source_name="native-equivalence.f")
        runtimes.append(runtime)
    return runtimes


def _observe(runtime, word, *, inputs=(), spans=(), step_budget=None):
    context = runtime.main_context
    for value in inputs:
        context.data.push(value)
    before_steps = runtime.diagnostics.semantic_cycles
    before_native = runtime.native_execution_stats["semantic_steps"]
    result = None
    error = None
    try:
        result = runtime.execute(word, step_budget=step_budget)
    except Exception as caught:
        error = (type(caught), str(caught))
    observation = {
        "result_steps": None if result is None else result.semantic_steps,
        "counted_steps": runtime.diagnostics.semantic_cycles - before_steps,
        "data": context.data.snapshot(),
        "returns": context.returns.snapshot(),
        "sp": context.data.pointer,
        "rp": context.returns.pointer,
        # Popped guest stack bytes remain observable through SP!/RP! and @.
        "data_bytes": runtime.memory.read_bytes(
            context.data.empty_pointer - 128, 128
        ),
        "return_bytes": runtime.memory.read_bytes(
            context.returns.empty_pointer - 128, 128
        ),
        "memory": tuple(runtime.memory.read_bytes(a, n) for a, n in spans),
        "here": runtime.dictionary.here,
        "latest": runtime.dictionary.latest,
        "uart": runtime.uart_output,
        "error": error,
        "reusable": context.reusable,
        "suspended": context.suspended,
        "host_control_fault": context.host_control_fault,
        "clock": (runtime.rtc.uptime_ms, runtime.rtc.epoch_ms),
    }
    native_steps = runtime.native_execution_stats["semantic_steps"] - before_native
    return observation, native_steps


def _compare(runtimes, word, *, require_native=True, **kwargs):
    observed = [_observe(runtime, word, **kwargs) for runtime in runtimes]
    assert observed[1][0] == observed[0][0]
    assert observed[0][1] == 0
    if require_native:
        assert observed[1][1] > 0, "the executed source must reach native work"
    return observed[0][0]


def test_nested_arithmetic_and_branches_preserve_steps_and_backed_stacks():
    runtimes = _runtimes(
        b": STEP DUP 1 AND IF 3 * 1+ ELSE 2/ THEN ; "
        b": RUN 0 64 BEGIN DUP WHILE SWAP OVER STEP + SWAP 1- REPEAT DROP ;"
    )
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert result["data"] == (3632,)
    assert result["returns"] == ()
    assert result["result_steps"] == result["counted_steps"]
    assert result["clock"] == (0, 0)


def test_opt_in_profile_reports_original_exit_boundaries_without_guest_effects(monkeypatch):
    monkeypatch.setenv("MEGAFORTH_NATIVE_PROFILE", "1")
    runtimes = _runtimes(b": RUN 7 >R R@ R> + ;")
    result = _compare(runtimes, "RUN")
    assert result["data"] == (14,)
    profile = runtimes[1].native_execution_stats["profile"]
    assert sum(profile["exits"].values()) >= 1
    assert profile["native_run_ns"] > 0
    assert profile["settlement_ns"] > 0
    assert any(key.endswith("Return") for key in profile["exits"])


@pytest.mark.parametrize("budget", [1, 2, 3])
def test_call_budget_keeps_separate_ir_and_primitive_ticks(budget):
    runtimes = _runtimes(b": RUN DUP ;")
    result = _compare(
        runtimes, "RUN", inputs=(7,), step_budget=budget, require_native=False
    )
    assert result["counted_steps"] == budget
    assert result["data"] == ((7,) if budget == 1 else (7, 7))
    if budget < 3:
        assert result["error"][0] is StepBudgetExceeded
    else:
        assert result["error"] is None
        assert result["result_steps"] == 3
    assert result["returns"] == ()


def test_signed_boundaries_wrapping_and_shift_counts_match_python():
    runtimes = _runtimes(
        b": RUN -1 2 * 0 1- "
        b"0x8000000000000000 0x7FFFFFFFFFFFFFFF < "
        b"0x8000000000000000 63 RSHIFT "
        b"0x8000000000000001 64 RSHIFT "
        b"0x8000000000000000 65 RSHIFT 1 64 LSHIFT -3 2/ ;"
    )
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert result["data"] == (
        MASK64 - 1, MASK64, TRUE, 1, 0x8000000000000001,
        0x4000000000000000, 1, MASK64 - 1,
    )


def test_unaligned_writes_materialize_missing_pages_without_losing_cells():
    runtimes = _runtimes(external_size=3 * 4096)
    addresses = []
    for runtime in runtimes:
        external = next(
            r for r in runtime.memory.regions if r.kind is AddressClass.EXTERNAL
        )
        address = external.base + 4093
        addresses.append(address)
        assert runtime.memory.read_bytes(address, 12) == bytes(12)
        runtime.define_constant("TARGET", address)
        runtime.evaluate(
            b": RUN 0x8877665544332211 TARGET ! TARGET @ "
            b"0x1AB TARGET 8 + C! TARGET 8 + C@ ;"
        )
    assert addresses[0] == addresses[1]
    result = _compare(runtimes, "RUN", spans=((addresses[0], 12),))
    assert result["error"] is None
    assert result["data"] == (0x8877665544332211, 0xAB)
    assert result["memory"] == (bytes.fromhex("1122334455667788ab000000"),)


def test_guest_store_into_the_live_data_stack_is_seen_by_later_arithmetic():
    runtimes = _runtimes(b": RUN 11 22 SP@ 8 + 99 SWAP ! + ;")
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert result["data"] == (121,)


def test_return_pointer_restore_keeps_nonlocal_control_flow():
    runtimes = _runtimes(b": ESCAPE RP! 42 ; : RUN RP@ ESCAPE 99 ;")
    result = _compare(runtimes, "RUN", require_native=False)
    assert result["error"] is None
    assert result["data"] == (42,)
    assert result["returns"] == ()


def test_popped_native_continuations_remain_typed_after_pointer_restore():
    runtimes = _runtimes(b": SAVE 1 2 + DROP RP@ ; : RUN SAVE ;")
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    restored = []
    for runtime in runtimes:
        context = runtime.main_context
        context.returns.set_pointer(context.data.peek())
        restored.append(context.returns.snapshot())
        assert len(restored[-1]) == 2
        assert all(isinstance(entry, Continuation) for entry in restored[-1])
        context.returns.set_pointer(context.returns.empty_pointer)
    assert restored[0] == restored[1]


def test_raw_return_slot_overwrite_cannot_keep_a_cached_continuation():
    runtimes = _runtimes(
        b": CLOBBER 17 0xBEEF RP@ ! 23 ; : RUN CLOBBER 99 ;"
    )
    result = _compare(runtimes, "RUN")
    assert result["error"][0] is ReturnStackShapeError
    assert result["data"] == (17, 23)
    assert result["returns"] == ()


def test_dictionary_rollback_reuses_an_xt_without_reusing_its_native_body():
    runtimes = _runtimes()
    checkpoints = [runtime.dictionary.checkpoint() for runtime in runtimes]
    old_xt = []
    for runtime in runtimes:
        runtime.evaluate(b": ITEM 10 ; : RUN ITEM 1+ ;")
        old_xt.append(runtime.find("ITEM").xt)
    first = _compare(runtimes, "RUN")
    assert first["error"] is None
    assert first["data"] == (11,)
    for runtime, checkpoint, previous_xt in zip(runtimes, checkpoints, old_xt):
        runtime.main_context.data.clear()
        runtime.dictionary.rollback(checkpoint)
        runtime.evaluate(b": ITEM 20 ; : RUN ITEM 1+ ;")
        assert runtime.find("ITEM").xt == previous_xt
    second = _compare(runtimes, "RUN")
    assert second["error"] is None
    assert second["data"] == (21,)


def test_compiled_calls_keep_their_original_definition_after_shadowing():
    runtimes = _runtimes(b": ITEM 10 ; : RUN ITEM 1+ ;")
    assert _compare(runtimes, "RUN")["data"] == (11,)
    for runtime in runtimes:
        runtime.main_context.data.clear()
        runtime.evaluate(b": ITEM 90 ;")
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert result["data"] == (11,)


def test_cached_created_caller_observes_the_later_does_action():
    runtimes = _runtimes()
    observed_bodies = []
    warmed_steps = []
    for runtime in runtimes:
        bodies = []
        observed_bodies.append(bodies)
        native_steps = []
        warmed_steps.append(native_steps)

        def visit_created(
            context, *, runtime=runtime, bodies=bodies, native_steps=native_steps
        ):
            child = runtime.dictionary.latest_word
            before = runtime.native_execution_stats["semantic_steps"]
            runtime.execute("CACHED", context=context)
            native_steps.append(
                runtime.native_execution_stats["semantic_steps"] - before
            )
            bodies.append(context.data.pop())
            assert bodies[-1] == child.body_address * 2

        runtime.define_primitive("VISIT-CREATED", visit_created)
        runtime.evaluate(
            b": BOX CREATE , VISIT-CREATED DOES> @ 1+ ;"
        )
        # Publish the bound caller before CREATE, so ITEM remains the latest
        # word at DOES>. Its forward XT follows the public header geometry.
        child_header = (
            runtime.dictionary.here + runtime.dictionary.definition_size("CACHED")
        )
        child_xt = child_header + HEADER_FIXED_BYTES + len(b"ITEM")
        runtime.define_colon(
            "CACHED",
            (Call(child_xt), Literal(2), Call(runtime.find("*").xt), Return()),
        )
        assert runtime.dictionary.here == child_header
        runtime.evaluate(b"10 BOX ITEM")
        assert runtime.find("ITEM").xt == child_xt
        # No definition is published after the cache is warmed: the ordinary
        # DOES> action installation itself must invalidate the plain-body call.
    assert observed_bodies[0] == observed_bodies[1]
    assert warmed_steps[0] == [0]
    assert len(warmed_steps[1]) == 1 and warmed_steps[1][0] > 0
    result = _compare(runtimes, "CACHED")
    assert result["error"] is None
    assert result["data"] == (22,)


@pytest.mark.parametrize("hook", ["runtime", "diagnostics", "timer"])
def test_custom_tick_observers_keep_every_tick_on_the_python_path(hook):
    runtimes = _runtimes(
        b": RUN 0 16 BEGIN DUP WHILE SWAP OVER + SWAP 1- REPEAT DROP ;"
    )
    observations = []
    for runtime in runtimes:
        ticks = []
        observations.append(ticks)
        if hook == "runtime":
            original = runtime._account_semantic_step

            def tick(*, runtime=runtime, ticks=ticks, original=original):
                ticks.append(runtime.main_context.data.snapshot())
                original()

            runtime._account_semantic_step = tick
        elif hook == "diagnostics":
            class ObservedDiagnostics(HostedDiagnosticsService):
                def account_work(self, *, runtime=runtime, ticks=ticks):
                    ticks.append(runtime.main_context.data.snapshot())
                    super().account_work()

            runtime.diagnostics = ObservedDiagnostics()
        else:
            class ObservedTimer(HostedTimerService):
                def advance(self, *, runtime=runtime, ticks=ticks):
                    ticks.append(runtime.main_context.data.snapshot())
                    super().advance()

            runtime.timer = ObservedTimer()

    before = runtimes[1].native_execution_stats
    result = _compare(runtimes, "RUN", require_native=False)
    assert result["error"] is None
    assert result["data"] == (136,)
    assert observations[0] == observations[1]
    assert len(observations[0]) == result["counted_steps"]
    after = runtimes[1].native_execution_stats
    assert after["semantic_steps"] == before["semantic_steps"]
    assert after["entries"] == before["entries"]


def test_value_reads_current_storage_and_compiled_to_keeps_its_binding():
    runtimes = _runtimes(
        b"7 VALUE COUNT : READ COUNT 1+ ; : SET TO COUNT ;"
    )
    assert _compare(runtimes, "READ")["data"] == (8,)
    for runtime in runtimes:
        runtime.main_context.data.clear()
        runtime.memory.write64(runtime.find("COUNT").body_address, 41)
    assert _compare(runtimes, "READ")["data"] == (42,)
    for runtime in runtimes:
        runtime.main_context.data.clear()
        runtime.evaluate(b"99 VALUE COUNT")
    changed = _compare(runtimes, "SET", inputs=(50,), require_native=False)
    assert changed["error"] is None
    assert changed["data"] == ()
    assert _compare(runtimes, "READ")["data"] == (51,)
    for runtime in runtimes:
        assert runtime.memory.read64(runtime.find("COUNT").body_address) == 99


def test_memory_fault_preserves_prior_writes_and_python_operand_consumption():
    runtimes = _runtimes(b"VARIABLE MARK : RUN 123 MARK C! 456 -1 @ 999 ;")
    mark = runtimes[0].find("MARK").body_address
    result = _compare(runtimes, "RUN", spans=((mark, 8),))
    assert result["error"] is not None
    assert result["data"] == (456, MASK64)
    assert result["memory"] == (b"\x7b" + bytes(7),)
    assert result["returns"] == ()


def test_unknown_callback_keeps_partial_memory_stack_and_output_effects():
    runtimes = _runtimes(b"VARIABLE MARK")
    calls = [[], []]
    for runtime, records in zip(runtimes, calls):
        address = runtime.find("MARK").body_address

        def callback(context, *, runtime=runtime, records=records, address=address):
            records.append(context.data.snapshot())
            runtime.memory.write8(address + 1, 0x55)
            runtime.write_uart_bytes(b"prefix")
            context.data.push(71)
            raise ExecutionError("callback stopped")

        runtime.define_primitive("CALLBACK", callback)
        runtime.evaluate(b": RUN 11 1+ MARK C! 20 2 + CALLBACK 99 ;")
    mark = runtimes[0].find("MARK").body_address
    result = _compare(runtimes, "RUN", spans=((mark, 8),))
    assert result["error"] == (ExecutionError, "callback stopped")
    assert calls == [[(22,)], [(22,)]]
    assert result["data"] == (22, 71)
    assert result["memory"] == (b"\x0c\x55" + bytes(6),)
    assert result["uart"] == b"prefix"


def test_counted_loop_fallback_keeps_native_callee_continuations():
    runtimes = _runtimes(b": DOUBLE 2 * ; : RUN 0 8 0 DO I DOUBLE + LOOP ;")
    result = _compare(runtimes, "RUN")
    assert result["error"] is None
    assert result["data"] == (56,)
    assert result["returns"] == ()
