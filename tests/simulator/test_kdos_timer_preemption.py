"""Unchanged-source acceptance for KDOS timer-preemption setup."""

from __future__ import annotations

import hashlib
from pathlib import Path

from simulator.ir import Idle
from simulator.runtime import ColonDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_scheduler_prefix import (
    _load_scheduler_prefix,
    _register_task,
    _status,
    _store_variable,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-timer-preemption-6725-6758.f"
)

FIRST_LINE = 6725
LAST_LINE = 6758
SLICE_BYTES = 1_143
SLICE_SHA256 = (
    "e55c6bf6e2df1fd6f543105822ac24217083dbeebe94bae0f631ac34d6dcd653"
)
SLICE_GIT_BLOB = "a1955ae8ee10c8bee1de5455a55c725d752462ff"
HOSTED_DICTIONARY_GROWTH = 134
HOSTED_WORD_FIXED_BYTES = 17
MASK32 = (1 << 32) - 1

SOURCE_LEDGER = (
    ("VARIABLE", b"PREEMPT-ENABLED", 8),
    (":", b"PREEMPT-ON", 0),
    (":", b"PREEMPT-OFF", 0),
    (":", b"_CORE-CHECKPOINT-TIMER", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 3] == b"    ELSE DROP THEN ;\n"
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ " + b"=" * 69 + b"\n"
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa78.1  Multicore Dispatch\n"
    return source


def _evaluate_timer_preemption(
    runtime: MegaForthRuntime,
) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_timer_preemption() -> MegaForthRuntime:
    return _evaluate_timer_preemption(_load_scheduler_prefix())


def _force_match(runtime: MegaForthRuntime, control: int) -> None:
    """Match on the next explicit timer tick with the requested control."""

    runtime.timer.write_compare((runtime.timer.counter + 1) & MASK32)
    runtime.timer.write_control(control)
    runtime.timer.acknowledge()
    runtime.timer.advance()
    assert runtime.timer.status == 1


def test_timer_preemption_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_scheduler_prefix()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    _store_variable(runtime, "PREEMPT-FLAG", 0xC0DE)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    task_table = _execute(runtime, "TASK-TABLE")[0]
    task_state_before = (
        _variable(runtime, "TASK-COUNT"),
        runtime.memory.read_bytes(task_table, 64),
        _variable(runtime, "CURRENT-TASK"),
        _variable(runtime, "SCHED-RUNNING"),
        _variable(runtime, "PREEMPT-FLAG"),
        _variable(runtime, "SPAWN-COUNT"),
    )

    _force_match(runtime, 3)
    runtime.timer.write_compare(0x89AB_CDEF)
    runtime.timer.write_control(0xA0)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_timer_preemption(runtime)

    assert len(SOURCE_LEDGER) == 4
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((_definer, name, body_span), word) in enumerate(
        zip(SOURCE_LEDGER, published)
    ):
        assert word.name == name
        assert runtime.memory.read64(word.header_address) == prior_header
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following - word.body_address == body_span
        prior_header = word.header_address

    assert _variable(runtime, "PREEMPT-ENABLED") == 0
    checkpoint = runtime.find("CORE-CHECKPOINT")
    timer_checkpoint = runtime.find("_CORE-CHECKPOINT-TIMER")
    assert checkpoint is not None
    assert timer_checkpoint is not None
    assert runtime.memory.read64(checkpoint.body_address) == timer_checkpoint.xt
    for name in ("PREEMPT-ON", "PREEMPT-OFF", "_CORE-CHECKPOINT-TIMER"):
        word = runtime.find(name)
        assert word is not None
        assert isinstance(word.implementation, ColonDefinition)
        assert not any(
            isinstance(operation, Idle)
            for operation in word.implementation.operations
        )

    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert (
        _variable(runtime, "TASK-COUNT"),
        runtime.memory.read_bytes(task_table, 64),
        _variable(runtime, "CURRENT-TASK"),
        _variable(runtime, "SCHED-RUNNING"),
        _variable(runtime, "PREEMPT-FLAG"),
        _variable(runtime, "SPAWN-COUNT"),
    ) == task_state_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"
    assert runtime.numeric_base == 10
    assert runtime.main_context.reusable
    assert not runtime.main_context.suspended


def test_preempt_on_and_off_only_program_the_timer_and_software_gate() -> None:
    runtime = _load_timer_preemption()
    _force_match(runtime, 7)
    assert runtime.timer.counter == 0
    assert runtime.timer.irq_pending
    runtime.timer.write_control(1)
    for _index in range(123):
        runtime.timer.advance()
    runtime.timer.write_control(0)
    assert runtime.timer.counter == 123
    assert runtime.timer.status == 1
    assert runtime.timer.irq_pending

    time_slice = 0xA5A5_A5A5_7FFF_FF00
    _store_variable(runtime, "TIME-SLICE", time_slice)
    _store_variable(runtime, "PREEMPT-FLAG", 0)
    counter_before_on = runtime.timer.counter

    assert _execute(runtime, "PREEMPT-ON") == ()

    assert runtime.timer.compare == (time_slice & MASK32)
    assert runtime.timer.control == 5
    assert runtime.timer.counter >= counter_before_on
    assert runtime.timer.status == 1
    assert runtime.timer.irq_pending
    assert _variable(runtime, "PREEMPT-ENABLED") == 1
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    counter_before_off = runtime.timer.counter

    assert _execute(runtime, "PREEMPT-OFF") == ()

    assert runtime.timer.compare == (time_slice & MASK32)
    assert runtime.timer.control == 1
    assert runtime.timer.counter >= counter_before_off
    assert runtime.timer.status == 1
    assert runtime.timer.irq_pending
    assert _variable(runtime, "PREEMPT-ENABLED") == 0
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    counter_after_off = runtime.timer.counter
    runtime.timer.advance()
    assert runtime.timer.counter == ((counter_after_off + 1) & MASK32)


def test_timer_match_never_raises_the_forth_flag_or_switches_tasks() -> None:
    runtime = _load_timer_preemption()
    calls: list[str] = []
    first_body = runtime.define_primitive(
        "TIMER-FIRST-BODY",
        lambda _context: calls.append("first"),
    )
    second_body = runtime.define_primitive(
        "TIMER-SECOND-BODY",
        lambda _context: calls.append("second"),
    )
    first = _register_task(runtime, first_body.xt, 255, "TIMER-FIRST-TASK")
    second = _register_task(runtime, second_body.xt, 0, "TIMER-SECOND-TASK")
    runtime.evaluate(
        b"VARIABLE TIMER-CHECKPOINT-TRACE "
        b": TIMER-CHECKPOINT-CONTINUES "
        b"CORE-CHECKPOINT 9 TIMER-CHECKPOINT-TRACE ! ; "
        b": TIMER-YIELD-QUERY-CONTINUES "
        b"YIELD? 8 TIMER-CHECKPOINT-TRACE ! ;",
        source_name="timer-checkpoint-continuation.f",
    )
    _store_variable(runtime, "CURRENT-TASK", first)
    _store_variable(runtime, "PREEMPT-ENABLED", 1)
    _store_variable(runtime, "PREEMPT-FLAG", 0)

    _force_match(runtime, 5)
    assert runtime.timer.counter == 0
    assert not runtime.timer.irq_pending
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    assert _execute(runtime, "TIMER-CHECKPOINT-CONTINUES") == ()
    assert _variable(runtime, "TIMER-CHECKPOINT-TRACE") == 9
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    assert (_status(runtime, first), _status(runtime, second)) == (1, 1)
    assert _variable(runtime, "CURRENT-TASK") == first
    assert runtime.timer.status == 1
    assert not runtime.timer.irq_pending
    assert calls == []

    _store_variable(runtime, "PREEMPT-ENABLED", 0)
    _store_variable(runtime, "PREEMPT-FLAG", 1)
    _store_variable(runtime, "TIMER-CHECKPOINT-TRACE", 0)
    assert _execute(runtime, "TIMER-YIELD-QUERY-CONTINUES") == ()
    assert _variable(runtime, "TIMER-CHECKPOINT-TRACE") == 8
    assert _variable(runtime, "PREEMPT-FLAG") == 1
    assert (_status(runtime, first), _status(runtime, second)) == (1, 1)
    assert _variable(runtime, "CURRENT-TASK") == first
    assert calls == []

    _force_match(runtime, 3)
    assert runtime.timer.irq_pending
    runtime.timer.write_control(0)
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    _store_variable(runtime, "PREEMPT-ENABLED", 1)
    _store_variable(runtime, "TIMER-CHECKPOINT-TRACE", 0)

    assert _execute(runtime, "TIMER-CHECKPOINT-CONTINUES") == ()

    assert _variable(runtime, "TIMER-CHECKPOINT-TRACE") == 9
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    assert (_status(runtime, first), _status(runtime, second)) == (4, 1)
    assert _variable(runtime, "CURRENT-TASK") == first
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert calls == []
