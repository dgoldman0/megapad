"""Unchanged-source acceptance for KDOS one-core multicore dispatch."""

from __future__ import annotations

import hashlib
from pathlib import Path
import re

import pytest

from shared.cells import MASK64
from simulator.errors import ExecutionError, ForthAbort
from simulator.runtime import ColonDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_scheduler_prefix import _store_variable
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)
from tests.simulator.test_kdos_timer_preemption import (
    _load_timer_preemption,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-multicore-dispatch-6759-6922.f"
)

FIRST_LINE = 6759
LAST_LINE = 6922
SLICE_BYTES = 5_713
SLICE_SHA256 = (
    "03dc68d356a186f11b63fedd818863e75da51886d6290b38ba2c769325ffa90f"
)
SLICE_GIT_BLOB = "c919439c3c81cf5e35a270f47b7b122867df6a89"
HOSTED_DICTIONARY_GROWTH = 415
HOSTED_WORD_FIXED_BYTES = 17
VARIABLE_BODY_BYTES = 8
UINT32_MASK = (1 << 32) - 1

SOURCE_LEDGER = (
    (":", b"CORE-RUN", 0),
    (":", b"CORE-WAIT", 0),
    (":", b"ALL-CORES-WAIT", 0),
    (":", b"ALL-FULL-WAIT", 0),
    (":", b"BARRIER", 0),
    (":", b"LOCK", 0),
    (":", b"UNLOCK", 0),
    (":", b"CORES", 0),
    ("VARIABLE", b"PAR-PIPE", VARIABLE_BODY_BYTES),
    ("VARIABLE", b"PAR-STEP", VARIABLE_BODY_BYTES),
    ("VARIABLE", b"PAR-CORE", VARIABLE_BODY_BYTES),
    ("VARIABLE", b"PAR-P", VARIABLE_BODY_BYTES),
    ("VARIABLE", b"PAR-N", VARIABLE_BODY_BYTES),
    (":", b"P.RUN-PAR", 0),
    (":", b"P.BENCH-PAR", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)
PARALLEL_VARIABLES = (
    "PAR-PIPE",
    "PAR-STEP",
    "PAR-CORE",
    "PAR-P",
    "PAR-N",
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
    assert lines[FIRST_LINE - 3] == (
        b"' _CORE-CHECKPOINT-TIMER IS CORE-CHECKPOINT\n"
    )
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ " + b"=" * 69 + b"\n"
    assert lines[LAST_LINE + 1] == b"\\  \xc2\xa78.2  Per-Core Run Queues\n"
    return source


def _evaluate_multicore_dispatch(
    runtime: MegaForthRuntime,
) -> MegaForthRuntime:
    counter_before = runtime.timer.counter
    control_before = runtime.timer.control
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    if control_before == 1:
        assert runtime.timer.counter == (
            counter_before + result.semantic_steps
        ) & UINT32_MASK
    return runtime


def _load_multicore_dispatch() -> MegaForthRuntime:
    return _evaluate_multicore_dispatch(_load_timer_preemption())


def _install_test_pipelines(
    runtime: MegaForthRuntime,
) -> tuple[int, int]:
    result = runtime.evaluate(
        b"VARIABLE PAR-TRACE "
        b": PAR-FIRST PAR-TRACE @ 10 * 1 + PAR-TRACE ! ; "
        b": PAR-SECOND PAR-TRACE @ 10 * 2 + PAR-TRACE ! ; "
        b"2 PIPELINE PAR-TWO "
        b"' PAR-FIRST PAR-TWO P.ADD "
        b"' PAR-SECOND PAR-TWO P.ADD "
        b"0 PIPELINE PAR-EMPTY",
        source_name="ordinary-one-core-pipelines.f",
    )
    assert tuple(word.name for word in result.definitions) == (
        b"PAR-TRACE",
        b"PAR-FIRST",
        b"PAR-SECOND",
        b"PAR-TWO",
        b"PAR-EMPTY",
    )
    return _execute(runtime, "PAR-TWO")[0], _execute(runtime, "PAR-EMPTY")[0]


def test_multicore_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_timer_preemption()
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    assert _execute(runtime, "SPIN@", 3) == (0,)
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest
    words_before = runtime.dictionary.words
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    locks_before = runtime.spinlocks.owners
    timer_before = (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    )
    counter_before = runtime.timer.counter
    runtime.memory.fill(here_before, HOSTED_DICTIONARY_GROWTH, 0xA5)

    runtime = _evaluate_multicore_dispatch(runtime)

    assert len(SOURCE_LEDGER) == 15
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
        assert isinstance(word.implementation, ColonDefinition) == (
            definer == ":"
        )
        prior_header = word.header_address

    assert tuple(_variable(runtime, name) for name in PARALLEL_VARIABLES) == (
        0,
        0,
        0,
        0,
        0,
    )
    assert runtime.timer.counter > counter_before
    assert (
        runtime.timer.compare,
        runtime.timer.control,
        runtime.timer.status,
        runtime.timer.irq_pending,
    ) == timer_before
    assert runtime.spinlocks.owners == locks_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"


def test_one_core_boundary_rejects_dispatch_before_wake_and_waits_core_zero() -> None:
    runtime = _load_multicore_dispatch()
    noop = runtime.define_primitive("CORE-RUN-NOOP", lambda _context: None)

    assert _execute(runtime, "NCORES") == (1,)
    assert _execute(runtime, "N-FULL-CORES") == (1,)
    assert _execute(runtime, "COREID") == (0,)
    assert _execute(runtime, "CORE-STATUS", 0) == (0,)
    assert _execute(runtime, "CORE-WAIT", 0) == ()

    for core_id, output in (
        (0, b"Cannot dispatch to self"),
        (MASK64, b"Invalid core ID"),
        (1, b"Invalid core ID"),
    ):
        context = runtime.main_context
        context.data.clear()
        context.data.push(noop.xt)
        context.data.push(core_id)
        with pytest.raises(ForthAbort, match='Forth ABORT"'):
            runtime.execute("CORE-RUN", step_budget=1_000)
        assert runtime.drain_uart_output() == output
        assert context.data.snapshot() == ()
        assert context.returns.snapshot() == ()

    # The direct BIOS boundary is still explicit and non-consuming.  Every
    # CORE-RUN case above stopped in source validation before reaching it.
    wake_context = runtime.new_context()
    wake_context.data.push(noop.xt)
    wake_context.data.push(0)
    with pytest.raises(ExecutionError, match="WAKE-CORE is unavailable"):
        runtime.execute("WAKE-CORE", context=wake_context)
    assert wake_context.data.snapshot() == (noop.xt, 0)
    assert runtime.spinlocks.owners == (None,) * runtime.spinlocks.lock_count


def test_one_core_listing_and_depthless_lock_wrappers_are_source_exact() -> None:
    runtime = _load_multicore_dispatch()

    assert _execute(runtime, "LOCK", 7) == ()
    assert runtime.spinlocks.owner(7) == 0
    assert _execute(runtime, "LOCK", 7) == ()
    assert runtime.spinlocks.owner(7) == 0
    assert _execute(runtime, "UNLOCK", 7) == ()
    assert runtime.spinlocks.owner(7) is None
    assert _execute(runtime, "UNLOCK", 7) == ()
    assert runtime.spinlocks.owner(7) is None

    assert _execute(runtime, "CORES") == ()
    assert runtime.drain_uart_output() == (
        b" --- Cores (1  ) ---\r\n"
        b"   Core 0   [self] RUNNING\r\n"
    )


def test_parallel_pipeline_words_are_sequential_fallbacks_without_speedup() -> None:
    runtime = _load_multicore_dispatch()
    pipeline, empty = _install_test_pipelines(runtime)

    assert _execute(runtime, "P.RUN-PAR", pipeline) == ()
    assert _variable(runtime, "PAR-TRACE") == 12
    assert tuple(_variable(runtime, name) for name in PARALLEL_VARIABLES) == (
        0,
        0,
        0,
        0,
        0,
    )
    _store_variable(runtime, "PAR-TRACE", 0)
    assert _execute(runtime, "P.RUN-PAR", empty) == ()
    assert _variable(runtime, "PAR-TRACE") == 0

    _store_variable(runtime, "PAR-TRACE", 0)
    assert _execute(runtime, "P.BENCH-PAR", pipeline) == (pipeline,)
    assert _variable(runtime, "PAR-TRACE") == 12
    assert re.fullmatch(
        rb" Parallel pipeline \(2  steps, 1  cores\):\r\n"
        rb"   total = [1-9][0-9]*  cycles\r\n",
        runtime.drain_uart_output(),
    )


@pytest.mark.parametrize(
    "word",
    ("ALL-CORES-WAIT", "ALL-FULL-WAIT", "BARRIER"),
)
def test_equal_bound_plain_do_reaches_phantom_core_one(word: str) -> None:
    runtime = _load_multicore_dispatch()
    context = runtime.new_context()

    # NCORES and N-FULL-CORES are both one.  Plain DO does not zero-trip when
    # start equals limit, so strict CORE-STATUS exposes I=1 immediately and
    # keeps this source bug bounded without a wraparound-size execution.
    with pytest.raises(ExecutionError, match="accepts only core ID 0"):
        runtime.execute(word, context=context, step_budget=500)

    assert runtime.drain_uart_output() == b""
