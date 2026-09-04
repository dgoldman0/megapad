"""Unchanged-source acceptance for KDOS run queues through shared locks."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError, ForthAbort, StepBudgetExceeded
from simulator.ir import Do
from simulator.runtime import ColonDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_multicore_dispatch import (
    _load_multicore_dispatch,
)
from tests.simulator.test_kdos_scheduler_prefix import (
    _descriptor,
    _store_variable,
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
    / "kdos-run-queues-6923-7461.f"
)

FIRST_LINE = 6923
LAST_LINE = 7461
SLICE_BYTES = 17_203
SLICE_SHA256 = (
    "4e36452b9d65c41843f8b015065303375efae8667824c5bf606c30da6af32625"
)
SLICE_GIT_BLOB = "022981afa233362debb10678b250ac044d8454d9"
HOSTED_DICTIONARY_GROWTH = 7_365
HOSTED_WORD_FIXED_BYTES = 17
RAW_VARIABLE_BYTES = 4_959

SOURCE_LEDGER = (
    ("CONSTANT", b"RQ-DEPTH", 0),
    ("CONSTANT", b"NCORES_MAX", 0),
    ("VARIABLE", b"RQ-SLOTS", 1_031),
    ("VARIABLE", b"RQ-HEADS", 135),
    ("VARIABLE", b"RQ-TAILS", 135),
    (":", b"RQ-INIT", 0),
    (":", b"RQ-SLOT", 0),
    (":", b"RQ-COUNT", 0),
    (":", b"RQ-EMPTY?", 0),
    (":", b"RQ-FULL?", 0),
    (":", b"RQ-PUSH", 0),
    (":", b"RQ-POP", 0),
    (":", b"RQ-CLEAR", 0),
    (":", b"SCHED-CORE", 0),
    (":", b"SCHED-ALL", 0),
    (":", b"RQ-INFO", 0),
    (":", b"STEAL-FROM", 0),
    (":", b"RQ-BUSIEST", 0),
    (":", b"WORK-STEAL", 0),
    (":", b"BALANCE", 0),
    (":", b"SCHED-BALANCED", 0),
    ("VARIABLE", b"AFF-TABLE", 71),
    (":", b"AFF-INIT", 0),
    (":", b"AFFINITY!", 0),
    (":", b"AFFINITY@", 0),
    (":", b"SPAWN-ON", 0),
    (":", b"SCHED-AFFINE", 0),
    (":", b"AFF-INFO", 0),
    ("VARIABLE", b"PREEMPT-FLAGS", 135),
    (":", b"PREEMPT-FLAGS-INIT", 0),
    (":", b"PREEMPT-FLAG!", 0),
    (":", b"PREEMPT-FLAG@", 0),
    (":", b"PREEMPT-SET", 0),
    (":", b"PREEMPT-CLR", 0),
    (":", b"PREEMPT-ON-ALL", 0),
    (":", b"PREEMPT-OFF-ALL", 0),
    (":", b"WORKER-CHECKPOINT", 0),
    (":", b"_CORE-CHECKPOINT-PER-CORE", 0),
    (":", b"PREEMPT-INFO", 0),
    ("CONSTANT", b"MSG-DEPTH", 0),
    ("CONSTANT", b"MSG-CELLS", 0),
    ("CONSTANT", b"MSG-SLOCK", 0),
    ("VARIABLE", b"MSG-INBOX", 3_079),
    ("VARIABLE", b"MSG-IHEAD", 135),
    ("VARIABLE", b"MSG-ITAIL", 135),
    ("CONSTANT", b"MSG-CALL", 0),
    ("CONSTANT", b"MSG-DATA", 0),
    ("CONSTANT", b"MSG-SIGNAL", 0),
    ("CONSTANT", b"MSG-USER", 0),
    (":", b"MSG-ISLOT", 0),
    (":", b"MSG-ICOUNT", 0),
    (":", b"MSG-IFULL?", 0),
    (":", b"MSG-IEMPTY?", 0),
    (":", b"MSG-INIT", 0),
    ("VARIABLE", b"MS-T", 8),
    ("VARIABLE", b"MS-P", 8),
    ("VARIABLE", b"MS-G", 8),
    (":", b"MSG-SEND", 0),
    ("VARIABLE", b"MR-T", 8),
    ("VARIABLE", b"MR-S", 8),
    ("VARIABLE", b"MR-P", 8),
    (":", b"MSG-RECV", 0),
    (":", b"MSG-PEEK", 0),
    ("CONSTANT", b"MSG-HTYPES", 0),
    ("VARIABLE", b"MSG-HTABLE", 39),
    (":", b"MSG-HINIT", 0),
    (":", b"MSG-HANDLER!", 0),
    (":", b"MSG-HANDLER@", 0),
    (":", b"MSG-DISPATCH", 0),
    ("VARIABLE", b"MB-T", 8),
    ("VARIABLE", b"MB-P", 8),
    (":", b"MSG-BROADCAST", 0),
    (":", b"MSG-FLUSH", 0),
    (":", b"MSG-INFO", 0),
    ("CONSTANT", b"DICT-LOCK", 0),
    ("CONSTANT", b"UART-LOCK", 0),
    ("CONSTANT", b"FS-LOCK", 0),
    ("CONSTANT", b"HEAP-LOCK", 0),
    ("CONSTANT", b"RING-LOCK", 0),
    ("CONSTANT", b"HT-LOCK", 0),
    ("CONSTANT", b"APP-LOCK", 0),
    (":", b"DICT-ACQUIRE", 0),
    (":", b"DICT-RELEASE", 0),
    (":", b"UART-ACQUIRE", 0),
    (":", b"UART-RELEASE", 0),
    (":", b"FS-ACQUIRE", 0),
    (":", b"FS-RELEASE", 0),
    (":", b"HEAP-ACQUIRE", 0),
    (":", b"HEAP-RELEASE", 0),
    (":", b"WITH-LOCK", 0),
    (":", b"LOCK-INFO", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
PLAIN_ZERO_VARIABLES = (
    "MS-T",
    "MS-P",
    "MS-G",
    "MR-T",
    "MR-S",
    "MR-P",
    "MB-T",
    "MB-P",
)
ARRAY_INITIALIZERS = (
    ("RQ-SLOTS", 1_024, 0x00),
    ("RQ-HEADS", 128, 0x00),
    ("RQ-TAILS", 128, 0x00),
    ("AFF-TABLE", 64, 0xFF),
    ("PREEMPT-FLAGS", 128, 0x00),
    ("MSG-INBOX", 3_072, 0x00),
    ("MSG-IHEAD", 128, 0x00),
    ("MSG-ITAIL", 128, 0x00),
    ("MSG-HTABLE", 32, 0x00),
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
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ " + b"=" * 69 + b"\n"
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa78.8  Micro-Cluster Support\n"
    return source


def _evaluate_run_queues(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_run_queues() -> MegaForthRuntime:
    return _evaluate_run_queues(_load_multicore_dispatch())


def _xt(runtime: MegaForthRuntime, name: str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.xt


def test_run_queue_slice_is_exact_linked_and_initializes_only_its_tables() -> None:
    runtime = _load_multicore_dispatch()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    locks_before = runtime.spinlocks.owners
    task_before = (
        _variable(runtime, "TASK-COUNT"),
        _variable(runtime, "CURRENT-TASK"),
        _variable(runtime, "SCHED-RUNNING"),
        _variable(runtime, "PREEMPT-FLAG"),
    )
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_run_queues(runtime)

    assert len(SOURCE_LEDGER) == 91
    assert sum(definer == "CONSTANT" for definer, _name, _body in SOURCE_LEDGER) == 17
    assert sum(definer == "VARIABLE" for definer, _name, _body in SOURCE_LEDGER) == 17
    assert sum(definer == ":" for definer, _name, _body in SOURCE_LEDGER) == 57
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    assert sum(
        body for definer, _name, body in SOURCE_LEDGER if definer == "VARIABLE"
    ) == RAW_VARIABLE_BYTES

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
        assert isinstance(word.implementation, ColonDefinition) == (definer == ":")
        prior_header = word.header_address

    assert sum(size for _name, size, _fill in ARRAY_INITIALIZERS) + (
        len(PLAIN_ZERO_VARIABLES) * 8
    ) == 4_896
    for name, initialized_bytes, fill in ARRAY_INITIALIZERS:
        word = runtime.find(name)
        assert word is not None
        assert runtime.memory.read_bytes(word.body_address, initialized_bytes) == (
            bytes([fill]) * initialized_bytes
        )
        assert runtime.memory.read_bytes(
            word.body_address + initialized_bytes, 7
        ) == b"\xA5" * 7
    assert tuple(_variable(runtime, name) for name in PLAIN_ZERO_VARIABLES) == (
        0,
    ) * len(PLAIN_ZERO_VARIABLES)

    assert tuple(
        _constant(runtime, name)
        for name in (
            "RQ-DEPTH",
            "NCORES_MAX",
            "MSG-DEPTH",
            "MSG-CELLS",
            "MSG-SLOCK",
            "MSG-CALL",
            "MSG-DATA",
            "MSG-SIGNAL",
            "MSG-USER",
            "MSG-HTYPES",
            "DICT-LOCK",
            "UART-LOCK",
            "FS-LOCK",
            "HEAP-LOCK",
            "RING-LOCK",
            "HT-LOCK",
            "APP-LOCK",
        )
    ) == (8, 16, 8, 3, 7, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 5, 6)
    checkpoint = runtime.find("CORE-CHECKPOINT")
    per_core = runtime.find("_CORE-CHECKPOINT-PER-CORE")
    assert checkpoint is not None
    assert per_core is not None
    assert runtime.memory.read64(checkpoint.body_address) == per_core.xt
    assert (
        _variable(runtime, "TASK-COUNT"),
        _variable(runtime, "CURRENT-TASK"),
        _variable(runtime, "SCHED-RUNNING"),
        _variable(runtime, "PREEMPT-FLAG"),
    ) == task_before
    assert runtime.spinlocks.owners == locks_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"


def test_core_zero_queue_is_fifo_with_seven_slot_capacity_and_wraparound() -> None:
    runtime = _load_run_queues()

    assert _execute(runtime, "RQ-COUNT", 0) == (0,)
    assert _execute(runtime, "RQ-EMPTY?", 0) == (MASK64,)
    for value in range(0x11, 0x18):
        assert _execute(runtime, "RQ-PUSH", value, 0) == ()
    assert _execute(runtime, "RQ-COUNT", 0) == (7,)
    assert _execute(runtime, "RQ-FULL?", 0) == (MASK64,)

    full_context = runtime.new_context()
    full_context.data.push(0x18)
    full_context.data.push(0)
    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("RQ-PUSH", context=full_context, step_budget=500)
    assert runtime.drain_uart_output() == b"Run queue full"
    assert _execute(runtime, "RQ-COUNT", 0) == (7,)

    assert tuple(_execute(runtime, "RQ-POP", 0)[0] for _index in range(3)) == (
        0x11,
        0x12,
        0x13,
    )
    for value in (0x18, 0x19, 0x1A):
        assert _execute(runtime, "RQ-PUSH", value, 0) == ()
    assert tuple(_execute(runtime, "RQ-POP", 0)[0] for _index in range(7)) == (
        0x14,
        0x15,
        0x16,
        0x17,
        0x18,
        0x19,
        0x1A,
    )
    assert _execute(runtime, "RQ-POP", 0) == (0,)

    assert _execute(runtime, "RQ-PUSH", 0x99, 0) == ()
    stale_slot = _execute(runtime, "RQ-SLOT", 2, 0)[0]
    assert runtime.memory.read64(stale_slot) == 0x99
    assert _execute(runtime, "RQ-CLEAR", 0) == ()
    assert _execute(runtime, "RQ-COUNT", 0) == (0,)
    assert runtime.memory.read64(stale_slot) == 0x99
    assert _execute(runtime, "RQ-PUSH", 0, 0) == ()
    assert _execute(runtime, "RQ-POP", 0) == (0,)

    runtime.evaluate(
        b"VARIABLE RQ-TRACE "
        b": RQ-FIRST RQ-TRACE @ 10 * 1 + RQ-TRACE ! ; "
        b": RQ-SECOND RQ-TRACE @ 10 * 2 + RQ-TRACE ! ;",
        source_name="core-zero-run-queue-bodies.f",
    )
    assert _execute(runtime, "RQ-PUSH", _xt(runtime, "RQ-FIRST"), 0) == ()
    assert _execute(runtime, "RQ-PUSH", _xt(runtime, "RQ-SECOND"), 0) == ()
    assert _execute(runtime, "SCHED-CORE", 0) == ()
    assert _variable(runtime, "RQ-TRACE") == 12
    assert _execute(runtime, "RQ-COUNT", 0) == (0,)

    def fail_after_pop(_context: object) -> None:
        raise ExecutionError("queued body failed")

    failing = runtime.define_primitive("RQ-FAIL", fail_after_pop)
    assert _execute(runtime, "RQ-PUSH", failing.xt, 0) == ()
    assert _execute(runtime, "RQ-PUSH", _xt(runtime, "RQ-FIRST"), 0) == ()
    failing_context = runtime.new_context()
    failing_context.data.push(0)
    with pytest.raises(ExecutionError, match="queued body failed"):
        runtime.execute("SCHED-CORE", context=failing_context)
    assert _execute(runtime, "RQ-COUNT", 0) == (1,)
    assert _execute(runtime, "RQ-POP", 0) == (_xt(runtime, "RQ-FIRST"),)


def test_sched_all_plain_do_cannot_zero_trip_on_the_one_core_profile() -> None:
    runtime = _load_run_queues()
    sched_all = runtime.find("SCHED-ALL")
    assert sched_all is not None
    assert isinstance(sched_all.implementation, ColonDefinition)
    assert sum(
        isinstance(operation, Do)
        for operation in sched_all.implementation.operations
    ) == 2

    noop = runtime.define_primitive("SCHED-ALL-NOOP", lambda _context: None)
    assert _execute(runtime, "RQ-PUSH", noop.xt, 0) == ()
    context = runtime.new_context()
    with pytest.raises(StepBudgetExceeded, match="80-step budget"):
        runtime.execute("SCHED-ALL", context=context, step_budget=80)
    assert _execute(runtime, "RQ-COUNT", 0) == (1,)
    assert runtime.drain_uart_output() == b""


def test_one_core_stealing_balancing_and_spawn_affinity_are_literal() -> None:
    runtime = _load_run_queues()
    runtime.evaluate(b": RQ-NOOP ;", source_name="run-queue-noop.f")
    noop = _xt(runtime, "RQ-NOOP")

    assert tuple(_execute(runtime, "AFFINITY@", index)[0] for index in range(8)) == (
        MASK64,
    ) * 8
    assert _execute(runtime, "RQ-PUSH", noop, 0) == ()
    assert _execute(runtime, "RQ-PUSH", noop, 0) == ()
    assert _execute(runtime, "BALANCE") == ()
    assert _execute(runtime, "RQ-COUNT", 0) == (2,)
    assert _execute(runtime, "RQ-BUSIEST", 0) == (MASK64,)
    assert _execute(runtime, "RQ-BUSIEST", 1) == (0,)
    assert _execute(runtime, "WORK-STEAL", 1) == (MASK64,)
    assert _execute(runtime, "RQ-COUNT", 0) == (1,)
    assert _execute(runtime, "RQ-COUNT", 1) == (1,)
    assert _execute(runtime, "RQ-POP", 1) == (noop,)
    assert _execute(runtime, "RQ-CLEAR", 0) == ()

    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    assert _execute(runtime, "SPAWN-ON", noop, 0) == ()
    descriptor = runtime.memory.read64(_execute(runtime, "TASK-TABLE")[0])
    assert runtime.dictionary.here - here_before == 48
    assert runtime.dictionary.latest == latest_before
    assert _variable(runtime, "TASK-COUNT") == 1
    assert _variable(runtime, "SPAWN-COUNT") == 0
    assert _execute(runtime, "AFFINITY@", 0) == (0,)
    assert _execute(runtime, "RQ-COUNT", 0) == (1,)
    assert _descriptor(runtime, descriptor) == (1, 128, noop, 0, 0, 0)

    with pytest.raises(StepBudgetExceeded, match="200-step budget"):
        runtime.execute(
            "SCHED-AFFINE",
            context=runtime.new_context(),
            step_budget=200,
        )
    assert _execute(runtime, "RQ-COUNT", 0) == (2,)
    assert _descriptor(runtime, descriptor) == (2, 128, noop, 0, 0, 0)


def test_per_core_checkpoint_consumes_only_the_core_zero_table_flag() -> None:
    runtime = _load_run_queues()
    _store_variable(runtime, "PREEMPT-FLAG", 0xCAFE)

    assert _execute(runtime, "PREEMPT-SET", 0) == ()
    assert _execute(runtime, "CORE-CHECKPOINT") == ()
    assert _execute(runtime, "PREEMPT-FLAG@", 0) == (1,)
    assert _execute(runtime, "PREEMPT-ON-ALL") == ()
    assert runtime.timer.compare == (_variable(runtime, "TIME-SLICE") & 0xFFFF_FFFF)
    assert runtime.timer.control == 7
    assert _variable(runtime, "PREEMPT-ENABLED") == 1

    assert _execute(runtime, "CORE-CHECKPOINT") == ()
    assert _execute(runtime, "PREEMPT-FLAG@", 0) == (0,)
    assert _variable(runtime, "PREEMPT-FLAG") == 0xCAFE
    assert _execute(runtime, "PREEMPT-SET", 1) == ()
    assert _execute(runtime, "PREEMPT-OFF-ALL") == ()
    assert runtime.timer.control == 1
    assert _variable(runtime, "PREEMPT-ENABLED") == 0
    assert _execute(runtime, "PREEMPT-FLAG@", 1) == (0,)


def test_message_receive_preserves_the_accidental_extra_core_cell() -> None:
    runtime = _load_run_queues()

    assert _execute(runtime, "MSG-RECV") == (0, 0, 0, 0)
    assert _execute(runtime, "MSG-SEND", 1, 0xBEEF, 0) == (MASK64,)
    assert runtime.spinlocks.owner(7) is None
    assert _execute(runtime, "MSG-PEEK") == (MASK64,)
    assert _execute(runtime, "MSG-ICOUNT", 0) == (1,)
    assert _execute(runtime, "MSG-RECV") == (0, 1, 0, 0xBEEF, MASK64)
    assert _execute(runtime, "MSG-PEEK") == (0,)
    assert _execute(runtime, "MSG-ICOUNT", 0) == (0,)


def test_message_dispatch_and_flush_propagate_the_leaked_core_cells() -> None:
    runtime = _load_run_queues()
    runtime.evaluate(
        b"VARIABLE MSG-SEEN-SENDER "
        b"VARIABLE MSG-SEEN-PAYLOAD "
        b"VARIABLE MSG-SEEN-TYPE "
        b": MSG-RECORD "
        b"MSG-SEEN-TYPE ! MSG-SEEN-PAYLOAD ! MSG-SEEN-SENDER ! ;",
        source_name="message-handler.f",
    )
    assert _execute(runtime, "MSG-HANDLER!", _xt(runtime, "MSG-RECORD"), 1) == ()

    assert _execute(runtime, "MSG-SEND", 1, 0xCAFE, 0) == (MASK64,)
    assert _execute(runtime, "MSG-DISPATCH") == (0, MASK64)
    assert (
        _variable(runtime, "MSG-SEEN-SENDER"),
        _variable(runtime, "MSG-SEEN-PAYLOAD"),
        _variable(runtime, "MSG-SEEN-TYPE"),
    ) == (0, 0xCAFE, 1)
    assert _execute(runtime, "MSG-SEND", 3, 0x1234, 0) == (MASK64,)
    assert _execute(runtime, "MSG-DISPATCH") == (0, 0)
    assert _execute(runtime, "MSG-DISPATCH") == (0,)

    assert _execute(runtime, "MSG-SEND", 0, 0x11, 0) == (MASK64,)
    assert _execute(runtime, "MSG-SEND", 2, 0x22, 0) == (MASK64,)
    assert _execute(runtime, "MSG-FLUSH") == (0, 1, 1)
    assert _execute(runtime, "MSG-ICOUNT", 0) == (0,)
    assert _execute(runtime, "MSG-BROADCAST", 1, 0x44) == (0,)


def test_named_lock_wrappers_are_balanced_only_on_normal_return() -> None:
    runtime = _load_run_queues()
    runtime.evaluate(
        b"VARIABLE LOCK-TRACE : LOCK-BODY 77 LOCK-TRACE ! ;",
        source_name="lock-body.f",
    )

    assert _execute(runtime, "WITH-LOCK", _xt(runtime, "LOCK-BODY"), 3) == ()
    assert _variable(runtime, "LOCK-TRACE") == 77
    assert runtime.spinlocks.owner(3) is None
    assert _execute(runtime, "DICT-ACQUIRE") == ()
    assert _execute(runtime, "DICT-ACQUIRE") == ()
    assert runtime.spinlocks.owner(0) == 0
    assert _execute(runtime, "DICT-RELEASE") == ()
    assert runtime.spinlocks.owner(0) is None

    def fail_while_locked(_context: object) -> None:
        raise ExecutionError("locked body failed")

    failing = runtime.define_primitive("LOCK-FAIL", fail_while_locked)
    context = runtime.new_context()
    context.data.push(failing.xt)
    context.data.push(2)
    with pytest.raises(ExecutionError, match="locked body failed"):
        runtime.execute("WITH-LOCK", context=context, step_budget=100)
    assert runtime.spinlocks.owner(2) == 0
