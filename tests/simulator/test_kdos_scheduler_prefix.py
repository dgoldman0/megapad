"""Unchanged-source acceptance for the synchronous KDOS scheduler prefix."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from simulator.errors import ExecutionError, SourceError
from simulator.ir import Idle
from simulator.runtime import ColonDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_dictionary_search import (
    _load_dictionary_search,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-scheduler-prefix-6511-6724.f"
)

FIRST_LINE = 6511
LAST_LINE = 6724
SLICE_BYTES = 6_935
SLICE_SHA256 = (
    "cc28cfab7033390f4efc885cc043feafecc136e913aa34cc6338f7ad1b6a1f4c"
)
SLICE_GIT_BLOB = "ccdee7bbf513495f25eb77ad4c0f13f63b07532c"

DESCRIPTOR_CELLS = 6
DESCRIPTOR_BYTES = 48
TASK_CAPACITY = 8
TASK_STACK_STRIDE = 256
TASK_STACK_MIDPOINT = 128
TASK_TABLE_BYTES = 64
TASK_STACKS_BYTES = 2_055
RAW_VARIABLE_BYTES = 2_175
DEFERRED_BODY_BYTES = 8
HOSTED_DICTIONARY_GROWTH = 3_174
HOSTED_WORD_FIXED_BYTES = 17

SOURCE_LEDGER = (
    ("CONSTANT", b"T.FREE", 0),
    ("CONSTANT", b"T.READY", 0),
    ("CONSTANT", b"T.RUNNING", 0),
    ("CONSTANT", b"T.BLOCKED", 0),
    ("CONSTANT", b"T.DONE", 0),
    ("VARIABLE", b"TASK-COUNT", 8),
    ("VARIABLE", b"TASK-TABLE", TASK_TABLE_BYTES),
    ("VARIABLE", b"CURRENT-TASK", 8),
    ("VARIABLE", b"SCHED-RUNNING", 8),
    ("VARIABLE", b"PREEMPT-FLAG", 8),
    ("VARIABLE", b"TIME-SLICE", 8),
    ("VARIABLE", b"TASK-STACKS", TASK_STACKS_BYTES),
    (":", b"T.STATUS", 0),
    (":", b"T.PRIORITY", 0),
    (":", b"T.XT", 0),
    (":", b"T.DSP", 0),
    (":", b"T.RSP", 0),
    (":", b"T.NAME", 0),
    (":", b"T.STATUS!", 0),
    (":", b"T.DSP!", 0),
    (":", b"T.RSP!", 0),
    ("VARIABLE", b"TDESC-TEMP", 8),
    (":", b"TASK", 0),
    (":", b"T.INFO", 0),
    (":", b"TASKS", 0),
    (":", b"FIND-READY", 0),
    (":", b"RUN-TASK", 0),
    (":", b"SCHEDULE", 0),
    (":", b"SCHED-YIELD", 0),
    (":", b"YIELD", 0),
    (":", b"_CORE-CHECKPOINT-BOOT", 0),
    ("DEFER", b"CORE-CHECKPOINT", DEFERRED_BODY_BYTES),
    (":", b"YIELD?", 0),
    ("VARIABLE", b"SPAWN-COUNT", 8),
    (":", b"SPAWN", 0),
    (":", b"KILL", 0),
    (":", b"RESTART", 0),
    (":", b"BG", 0),
    (":", b"TASK-COUNT-READY", 0),
)

DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
VARIABLE_BODY_SPANS = tuple(
    (name, body)
    for definer, name, body in SOURCE_LEDGER
    if definer == "VARIABLE"
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 3] == b"    DROP DROP CR ;\n"
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ -- Timer preemption setup --\n"
    return source


def _evaluate_scheduler_prefix(
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


def _load_scheduler_prefix() -> MegaForthRuntime:
    return _evaluate_scheduler_prefix(_load_dictionary_search())


def _task_table(runtime: MegaForthRuntime) -> tuple[int, ...]:
    table = _execute(runtime, "TASK-TABLE")[0]
    return tuple(
        runtime.memory.read64(table + index * 8)
        for index in range(TASK_CAPACITY)
    )


def _descriptor(runtime: MegaForthRuntime, address: int) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(address + index * 8)
        for index in range(DESCRIPTOR_CELLS)
    )


def _store_variable(
    runtime: MegaForthRuntime,
    name: str,
    value: int,
) -> None:
    address = _execute(runtime, name)[0]
    runtime.memory.write64(address, value)


def _register_task(
    runtime: MegaForthRuntime,
    xt: int,
    priority: int,
    name: str,
) -> int:
    descriptor = runtime.dictionary.here
    result = runtime.evaluate(
        f"{xt} {priority} TASK {name}".encode("ascii"),
        source_name=f"register-{name.lower()}.f",
    )
    assert tuple(word.name for word in result.definitions) == (
        name.encode("ascii"),
    )
    assert _constant(runtime, name) == descriptor
    constant = runtime.find(name)
    assert constant is not None
    assert constant.header_address == descriptor + DESCRIPTOR_BYTES
    return descriptor


def _status(runtime: MegaForthRuntime, descriptor: int) -> int:
    return _execute(runtime, "T.STATUS", descriptor)[0]


def test_scheduler_prefix_is_exact_and_pins_raw_allocation_and_load_state() -> None:
    runtime = _load_dictionary_search()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_scheduler_prefix(runtime)

    assert len(SOURCE_LEDGER) == 39
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    assert sum(body for _name, body in VARIABLE_BODY_SPANS) == (
        RAW_VARIABLE_BYTES
    )
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((definer, name, body_span), word) in enumerate(
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
        if definer == "VARIABLE":
            expected = 50_000 if name == b"TIME-SLICE" else 0
            assert runtime.memory.read64(word.body_address) == expected
        prior_header = word.header_address

    assert tuple(
        _constant(runtime, name)
        for name in ("T.FREE", "T.READY", "T.RUNNING", "T.BLOCKED", "T.DONE")
    ) == (0, 1, 2, 3, 4)
    assert runtime.memory.read_bytes(
        runtime.find("TASK-TABLE").body_address + 8,  # type: ignore[union-attr]
        TASK_TABLE_BYTES - 8,
    ) == b"\xA5" * (TASK_TABLE_BYTES - 8)
    assert runtime.memory.read_bytes(
        runtime.find("TASK-STACKS").body_address + 8,  # type: ignore[union-attr]
        TASK_STACKS_BYTES - 8,
    ) == b"\xA5" * (TASK_STACKS_BYTES - 8)

    checkpoint = runtime.find("CORE-CHECKPOINT")
    boot = runtime.find("_CORE-CHECKPOINT-BOOT")
    assert checkpoint is not None
    assert boot is not None
    assert runtime.memory.read64(checkpoint.body_address) == boot.xt
    assert _execute(runtime, "CORE-CHECKPOINT") == ()

    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"
    assert runtime.numeric_base == 10
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_task_publishes_a_name_constant_and_all_descriptor_fields() -> None:
    runtime = _load_scheduler_prefix()
    body = runtime.define_primitive("DESCRIPTOR-BODY", lambda _context: None)
    stacks = _execute(runtime, "TASK-STACKS")[0]

    descriptor = _register_task(runtime, body.xt, 37, "NAMED-TASK")

    assert _variable(runtime, "TASK-COUNT") == 1
    assert _variable(runtime, "TDESC-TEMP") == descriptor
    assert _task_table(runtime)[:2] == (descriptor, 0)
    assert _descriptor(runtime, descriptor) == (
        1,
        37,
        body.xt,
        stacks + TASK_STACK_MIDPOINT,
        0,
        0,
    )
    assert _execute(runtime, "T.STATUS", descriptor) == (1,)
    assert _execute(runtime, "T.PRIORITY", descriptor) == (37,)
    assert _execute(runtime, "T.XT", descriptor) == (body.xt,)
    assert _execute(runtime, "T.DSP", descriptor) == (
        stacks + TASK_STACK_MIDPOINT,
    )
    assert _execute(runtime, "T.RSP", descriptor) == (0,)
    assert _execute(runtime, "T.NAME", descriptor) == (0,)

    assert _execute(runtime, "T.STATUS!", 3, descriptor) == ()
    assert _execute(runtime, "T.DSP!", 0x1122, descriptor) == ()
    assert _execute(runtime, "T.RSP!", 0x3344, descriptor) == ()
    assert _descriptor(runtime, descriptor) == (
        3,
        37,
        body.xt,
        0x1122,
        0x3344,
        0,
    )


def test_listing_find_ready_and_schedule_use_slot_order_not_priority() -> None:
    runtime = _load_scheduler_prefix()
    runtime.evaluate(
        b"VARIABLE RUN-ORDER "
        b": SLOT-ZERO-BODY RUN-ORDER @ 10 * 1 + RUN-ORDER ! ; "
        b": SLOT-ONE-BODY RUN-ORDER @ 10 * 2 + RUN-ORDER ! ;",
        source_name="scheduler-order-bodies.f",
    )
    slot_zero_body = runtime.find("SLOT-ZERO-BODY")
    slot_one_body = runtime.find("SLOT-ONE-BODY")
    assert slot_zero_body is not None
    assert slot_one_body is not None
    slot_zero = _register_task(
        runtime,
        slot_zero_body.xt,
        255,
        "SLOT-ZERO-TASK",
    )
    slot_one = _register_task(
        runtime,
        slot_one_body.xt,
        0,
        "SLOT-ONE-TASK",
    )

    assert _execute(runtime, "TASKS") == ()
    assert runtime.drain_uart_output() == (
        b" --- Tasks (2  ) ---\r\n"
        + f"0  :  [task  st=1   pri=255   xt={slot_zero_body.xt}  ]\r\n".encode(
            "ascii"
        )
        + f"1  :  [task  st=1   pri=0   xt={slot_one_body.xt}  ]\r\n".encode(
            "ascii"
        )
    )
    assert _execute(runtime, "FIND-READY") == (slot_zero,)
    assert _execute(runtime, "T.STATUS!", 4, slot_zero) == ()
    assert _execute(runtime, "FIND-READY") == (slot_one,)
    assert _execute(runtime, "T.STATUS!", 3, slot_one) == ()
    assert _execute(runtime, "FIND-READY") == (0,)

    assert _execute(runtime, "RESTART", slot_zero) == ()
    assert _execute(runtime, "RESTART", slot_one) == ()
    assert _execute(runtime, "TASK-COUNT-READY") == (2,)
    assert _execute(runtime, "SCHEDULE") == ()
    assert _variable(runtime, "RUN-ORDER") == 12
    assert (_status(runtime, slot_zero), _status(runtime, slot_one)) == (4, 4)
    assert _execute(runtime, "TASK-COUNT-READY") == (0,)
    assert _variable(runtime, "SCHED-RUNNING") == 0
    assert _variable(runtime, "CURRENT-TASK") == slot_one


def test_run_task_uses_the_callers_live_stacks_not_recorded_dsp_or_rsp() -> None:
    runtime = _load_scheduler_prefix()
    runtime.evaluate(
        b"VARIABLE OBSERVED-CURRENT "
        b"VARIABLE OBSERVED-STATUS "
        b"VARIABLE OBSERVED-DSP "
        b"VARIABLE OBSERVED-RSP "
        b": OBSERVE-CALLER-TASK "
        b"CURRENT-TASK @ OBSERVED-CURRENT ! "
        b"CURRENT-TASK @ T.STATUS OBSERVED-STATUS ! "
        b"SP@ OBSERVED-DSP ! "
        b"RP@ OBSERVED-RSP ! "
        b"1+ ;",
        source_name="observe-caller-task.f",
    )
    body = runtime.find("OBSERVE-CALLER-TASK")
    assert body is not None
    descriptor = _register_task(runtime, body.xt, 19, "CALLER-STACK-TASK")
    recorded_dsp = _execute(runtime, "T.DSP", descriptor)[0]
    recorded_rsp = _execute(runtime, "T.RSP", descriptor)[0]
    context = runtime.main_context
    expected_caller_dsp = context.data.empty_pointer - 8

    context.data.push(40)
    context.data.push(descriptor)
    runtime.execute("RUN-TASK", step_budget=250_000)

    assert context.data.snapshot() == (41,)
    context.data.clear()
    assert _variable(runtime, "OBSERVED-CURRENT") == descriptor
    assert _variable(runtime, "OBSERVED-STATUS") == 2
    assert _variable(runtime, "OBSERVED-DSP") == expected_caller_dsp
    assert context.returns.floor <= _variable(runtime, "OBSERVED-RSP") < (
        context.returns.empty_pointer
    )
    assert _variable(runtime, "OBSERVED-DSP") != recorded_dsp
    assert recorded_rsp == 0
    assert _variable(runtime, "OBSERVED-RSP") != recorded_rsp
    assert _execute(runtime, "T.DSP", descriptor) == (recorded_dsp,)
    assert _execute(runtime, "T.RSP", descriptor) == (recorded_rsp,)
    assert _status(runtime, descriptor) == 4
    assert _variable(runtime, "CURRENT-TASK") == descriptor
    assert context.returns.snapshot() == ()


def test_yield_words_mark_state_but_never_transfer_control_or_idle() -> None:
    runtime = _load_scheduler_prefix()
    runtime.evaluate(
        b"VARIABLE YIELD-TRACE "
        b": YIELDING-BODY 1 YIELD-TRACE ! YIELD 2 YIELD-TRACE ! ; "
        b": CHECKPOINT-CONTINUES YIELD? 9 YIELD-TRACE ! ;",
        source_name="synchronous-yield-bodies.f",
    )
    body = runtime.find("YIELDING-BODY")
    assert body is not None
    descriptor = _register_task(runtime, body.xt, 128, "YIELDING-TASK")

    assert _execute(runtime, "RUN-TASK", descriptor) == ()
    assert _variable(runtime, "YIELD-TRACE") == 2
    assert _status(runtime, descriptor) == 4

    for name in ("SCHEDULE", "SCHED-YIELD", "YIELD", "YIELD?"):
        word = runtime.find(name)
        assert word is not None
        assert isinstance(word.implementation, ColonDefinition)
        assert not any(
            isinstance(operation, Idle)
            for operation in word.implementation.operations
        )

    assert _execute(runtime, "RESTART", descriptor) == ()
    _store_variable(runtime, "CURRENT-TASK", descriptor)
    assert _execute(runtime, "SCHED-YIELD") == ()
    assert _status(runtime, descriptor) == 4

    assert _execute(runtime, "RESTART", descriptor) == ()
    assert _execute(runtime, "YIELD") == ()
    assert _status(runtime, descriptor) == 4

    assert _execute(runtime, "RESTART", descriptor) == ()
    _store_variable(runtime, "PREEMPT-FLAG", 0)
    assert _execute(runtime, "YIELD?") == ()
    assert _status(runtime, descriptor) == 1

    _store_variable(runtime, "PREEMPT-FLAG", 1)
    assert _execute(runtime, "CHECKPOINT-CONTINUES") == ()
    assert _variable(runtime, "PREEMPT-FLAG") == 0
    assert _status(runtime, descriptor) == 4
    assert _variable(runtime, "YIELD-TRACE") == 9


def test_spawn_kill_restart_bg_and_ready_count_are_synchronous() -> None:
    runtime = _load_scheduler_prefix()
    calls: list[str] = []
    first = runtime.define_primitive(
        "SPAWN-FIRST",
        lambda _context: calls.append("first"),
    )
    second = runtime.define_primitive(
        "SPAWN-SECOND",
        lambda _context: calls.append("second"),
    )
    words_before = runtime.dictionary.words
    first_descriptor = runtime.dictionary.here

    assert _execute(runtime, "SPAWN", first.xt) == ()

    assert runtime.dictionary.words == words_before
    assert runtime.dictionary.here == first_descriptor + DESCRIPTOR_BYTES
    assert _descriptor(runtime, first_descriptor)[:3] == (1, 128, first.xt)
    assert _variable(runtime, "TASK-COUNT") == 1
    assert _variable(runtime, "SPAWN-COUNT") == 1
    assert _execute(runtime, "TASK-COUNT-READY") == (1,)
    assert _execute(runtime, "KILL", first_descriptor) == ()
    assert _execute(runtime, "TASK-COUNT-READY") == (0,)
    assert _execute(runtime, "RESTART", first_descriptor) == ()
    assert _execute(runtime, "TASK-COUNT-READY") == (1,)

    second_descriptor = runtime.dictionary.here
    assert _execute(runtime, "BG", second.xt) == ()

    assert calls == ["first", "second"]
    assert runtime.dictionary.here == second_descriptor + DESCRIPTOR_BYTES
    assert _variable(runtime, "TASK-COUNT") == 2
    assert _variable(runtime, "SPAWN-COUNT") == 2
    assert _task_table(runtime)[:3] == (
        first_descriptor,
        second_descriptor,
        0,
    )
    assert (
        _status(runtime, first_descriptor),
        _status(runtime, second_descriptor),
    ) == (4, 4)
    assert _execute(runtime, "TASK-COUNT-READY") == (0,)
    assert _variable(runtime, "SCHED-RUNNING") == 0
    assert _variable(runtime, "CURRENT-TASK") == second_descriptor


def test_registry_saturation_leaks_bounded_orphan_descriptors_and_constant() -> None:
    runtime = _load_scheduler_prefix()
    noop = runtime.define_primitive("SATURATION-NOOP", lambda _context: None)
    stacks = _execute(runtime, "TASK-STACKS")[0]
    descriptors = []
    for slot in range(TASK_CAPACITY):
        descriptor = runtime.dictionary.here
        assert _execute(runtime, "SPAWN", noop.xt) == ()
        descriptors.append(descriptor)
        assert _execute(runtime, "T.DSP", descriptor) == (
            stacks + slot * TASK_STACK_STRIDE + TASK_STACK_MIDPOINT,
        )

    table_before = _task_table(runtime)
    assert table_before == tuple(descriptors)
    assert _variable(runtime, "TASK-COUNT") == TASK_CAPACITY
    assert _variable(runtime, "SPAWN-COUNT") == TASK_CAPACITY
    assert _execute(runtime, "TASK-COUNT-READY") == (TASK_CAPACITY,)

    orphan_spawn = runtime.dictionary.here
    assert _execute(runtime, "SPAWN", noop.xt) == ()
    assert runtime.dictionary.here == orphan_spawn + DESCRIPTOR_BYTES
    assert _variable(runtime, "TASK-COUNT") == TASK_CAPACITY
    assert _variable(runtime, "SPAWN-COUNT") == TASK_CAPACITY + 1
    assert _variable(runtime, "TDESC-TEMP") == orphan_spawn
    assert _task_table(runtime) == table_before
    assert _descriptor(runtime, orphan_spawn) == (
        1,
        128,
        noop.xt,
        stacks + TASK_CAPACITY * TASK_STACK_STRIDE + TASK_STACK_MIDPOINT,
        0,
        0,
    )
    assert _execute(runtime, "T.DSP", orphan_spawn)[0] > (
        stacks + TASK_STACKS_BYTES
    )

    orphan_task = runtime.dictionary.here
    result = runtime.evaluate(
        f"{noop.xt} 7 TASK OVERFLOW-TASK".encode("ascii"),
        source_name="overflow-task.f",
    )
    assert tuple(word.name for word in result.definitions) == (b"OVERFLOW-TASK",)
    overflow_constant = runtime.find("OVERFLOW-TASK")
    assert overflow_constant is not None
    assert overflow_constant.header_address == orphan_task + DESCRIPTOR_BYTES
    assert _constant(runtime, "OVERFLOW-TASK") == orphan_task
    assert runtime.dictionary.here - orphan_task == (
        DESCRIPTOR_BYTES
        + HOSTED_WORD_FIXED_BYTES
        + len(b"OVERFLOW-TASK")
    )
    assert _variable(runtime, "TASK-COUNT") == TASK_CAPACITY
    assert _variable(runtime, "SPAWN-COUNT") == TASK_CAPACITY + 1
    assert _variable(runtime, "TDESC-TEMP") == orphan_task
    assert _task_table(runtime) == table_before
    assert _descriptor(runtime, orphan_task) == (
        1,
        7,
        noop.xt,
        stacks + TASK_CAPACITY * TASK_STACK_STRIDE + TASK_STACK_MIDPOINT,
        0,
        0,
    )


def test_task_missing_name_keeps_its_already_registered_descriptor() -> None:
    runtime = _load_scheduler_prefix()
    body = runtime.define_primitive("NAME-FAILURE-BODY", lambda _context: None)
    descriptor = runtime.dictionary.here
    words_before = runtime.dictionary.words

    with pytest.raises(SourceError, match="CONSTANT requires a following word"):
        runtime.evaluate(
            f"{body.xt} 23 TASK".encode("ascii"),
            source_name="task-without-name.f",
        )

    assert runtime.dictionary.words == words_before
    assert runtime.dictionary.here == descriptor + DESCRIPTOR_BYTES
    assert _variable(runtime, "TASK-COUNT") == 1
    assert _variable(runtime, "TDESC-TEMP") == descriptor
    assert _task_table(runtime)[:2] == (descriptor, 0)
    assert _descriptor(runtime, descriptor)[:3] == (1, 23, body.xt)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_throwing_task_leaves_running_scheduler_residue() -> None:
    runtime = _load_scheduler_prefix()

    def fail_task(_context) -> None:
        raise ExecutionError("task body failed")

    body = runtime.define_primitive("FAILING-SCHEDULER-BODY", fail_task)
    descriptor = _register_task(runtime, body.xt, 11, "FAILING-TASK")

    with pytest.raises(ExecutionError, match="task body failed"):
        runtime.execute("SCHEDULE", step_budget=250_000)

    assert _status(runtime, descriptor) == 2
    assert _variable(runtime, "CURRENT-TASK") == descriptor
    assert _variable(runtime, "SCHED-RUNNING") == 1
    assert _execute(runtime, "TASK-COUNT-READY") == (0,)
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
