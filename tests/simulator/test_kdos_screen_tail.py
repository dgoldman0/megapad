"""Unchanged-source acceptance for KDOS screen dispatch and event loop."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64
from simulator.runtime import BlockedExecution, ColonDefinition, MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_interactive_screens import (
    _address,
    _store_variable,
)
from tests.simulator.test_kdos_screen_definitions import (
    _load_screen_definitions,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = Path(__file__).with_name("fixtures") / "kdos-screen-tail-8340-8569.f"

FIRST_LINE = 8340
FIXTURE_LAST_LINE = 8569
LAST_LINE = 8568
FIXTURE_BYTES = 7_844
FIXTURE_SHA256 = (
    "b4b1421ed629128bdbfd0b63870cdd57707407659140765a97689ed6afb04726"
)
FIXTURE_GIT_BLOB = "81e6c48c8e20b5dfa0eb1312add859954f8e46aa"
SLICE_BYTES = 7_772
SLICE_SHA256 = (
    "6294e7f8f2170e73bf7188481a8ae0575564e11b75e8fb61ae808ed305f155c1"
)
SLICE_GIT_BLOB = "9de3741357f813221f0f44216340cc55c2f51cd0"

CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 604
RAW_CELL = int.from_bytes(b"\xA5" * CELL_BYTES, "little")

DEFINITIONS = (
    b"LBL-HOME",
    b"LBL-BUFS",
    b"LBL-KERN",
    b"LBL-PIPE",
    b"LBL-TASK",
    b"LBL-HELP",
    b"LBL-DOCS",
    b"LBL-STOR",
    b"LBL-CORE",
    b"LBL-OVERVIEW",
    b"LBL-MEMORY",
    b"LBL-NET",
    b"LBL-BLIST",
    b"LBL-BSTATS",
    b"RENDER-SCREEN",
    b"SWITCH-SCREEN",
    b"TASK-KEYS",
    b"CALL-SCREEN-KEY",
    b"DO-SELECT",
    b"HANDLE-KEY",
    b"SCREEN-LOOP",
    b"SCREENS",
    b"SCREEN",
)

SCREEN_ROWS = (
    (b"SCR-HOME", b"LBL-HOME", 0),
    (b"SCR-BUFFERS", b"LBL-BUFS", 1),
    (b"SCR-KERNELS", b"LBL-KERN", 0),
    (b"SCR-PIPES", b"LBL-PIPE", 0),
    (b"SCR-TASKS", b"LBL-TASK", 1),
    (b"SCR-HELP", b"LBL-HELP", 0),
    (b"SCR-DOCS", b"LBL-DOCS", 1),
    (b"SCR-STORAGE", b"LBL-STOR", 1),
    (b"SCR-CORES", b"LBL-CORE", 0),
)

SUBSCREEN_ROWS = (
    (0, 0, b"SCR-HOME-OVERVIEW", b"LBL-OVERVIEW"),
    (0, 1, b"SCR-HOME-MEMORY", b"LBL-MEMORY"),
    (0, 2, b"SCR-HOME-NET", b"LBL-NET"),
    (1, 0, b"SCR-BUF-LIST", b"LBL-BLIST"),
    (1, 1, b"SCR-BUF-STATS", b"LBL-BSTATS"),
)


def _verified_slice() -> bytes:
    fixture = FIXTURE.read_bytes()
    assert len(fixture) == FIXTURE_BYTES
    assert fixture.count(b"\n") == FIXTURE_LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(fixture).hexdigest() == FIXTURE_SHA256
    assert _git_blob_id(fixture) == FIXTURE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"\n"
    assert fixture == b"".join(lines[FIRST_LINE - 1 : FIXTURE_LAST_LINE])
    boundary = b"\\ =====================================================================\n"
    assert lines[LAST_LINE - 1] == b"\n"
    assert lines[LAST_LINE] == boundary
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_screen_tail(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_screen_tail() -> MegaForthRuntime:
    return _evaluate_screen_tail(_load_screen_definitions())


def _xt(runtime: MegaForthRuntime, name: bytes | str) -> int:
    word = runtime.find(name)
    assert word is not None
    return word.xt


def _table_store(
    runtime: MegaForthRuntime,
    name: str,
    index: int,
    value: int,
) -> None:
    runtime.memory.write64(_address(runtime, name) + index * CELL_BYTES, value)


def _table_fetch(runtime: MegaForthRuntime, name: str, index: int) -> int:
    return runtime.memory.read64(
        _address(runtime, name) + index * CELL_BYTES
    )


def test_screen_tail_slice_is_exact_linked_and_registers_expected_rows() -> None:
    runtime = _load_screen_definitions()
    max_screens = _constant(runtime, "MAX-SCREENS")
    max_subs = _constant(runtime, "MAX-SUBS")
    for name in (
        "SCR-XT",
        "SCR-LBL-XT",
        "SCR-FLAGS",
        "SCR-KEY-XT",
        "SCR-ACT-XT",
        "SUB-COUNTS",
    ):
        runtime.memory.fill(_address(runtime, name), max_screens * CELL_BYTES, 0xA5)
    for name in ("SUB-XT", "SUB-LBL-XT"):
        runtime.memory.fill(
            _address(runtime, name),
            max_screens * max_subs * CELL_BYTES,
            0xA5,
        )

    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
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

    runtime = _evaluate_screen_tail(runtime)

    assert len(DEFINITIONS) == 23
    assert sum(map(len, DEFINITIONS)) == 213
    assert (
        len(DEFINITIONS) * HOSTED_WORD_FIXED_BYTES + sum(map(len, DEFINITIONS))
        == HOSTED_DICTIONARY_GROWTH
    )
    published = runtime.dictionary.words[-len(DEFINITIONS) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(DEFINITIONS)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, word in enumerate(published):
        assert runtime.memory.read64(word.header_address) == prior_header
        assert isinstance(word.implementation, ColonDefinition)
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following == word.body_address
        prior_header = word.header_address

    assert _variable(runtime, "NSCREENS") == len(SCREEN_ROWS)
    assert _variable(runtime, "SCREEN-ID") == 1
    assert _variable(runtime, "SCR-SEL") == MASK64
    assert _variable(runtime, "SCR-MAX") == 0
    assert _variable(runtime, "SUBSCREEN-ID") == 0
    assert _variable(runtime, "_ASUB-P") == 1
    assert _variable(runtime, "_ASUB-I") == 1

    for index, (render, label, flags) in enumerate(SCREEN_ROWS):
        assert _table_fetch(runtime, "SCR-XT", index) == _xt(runtime, render)
        assert _table_fetch(runtime, "SCR-LBL-XT", index) == _xt(runtime, label)
        assert _table_fetch(runtime, "SCR-FLAGS", index) == flags
        expected_key = _xt(runtime, b"TASK-KEYS") if index == 4 else 0
        assert _table_fetch(runtime, "SCR-KEY-XT", index) == expected_key
        assert _table_fetch(runtime, "SCR-ACT-XT", index) == 0
    assert tuple(
        _table_fetch(runtime, "SUB-COUNTS", index)
        for index in range(len(SCREEN_ROWS))
    ) == (3, 2, 0, 0, 0, 0, 0, 0, 0)

    for parent, child, render, label in SUBSCREEN_ROWS:
        slot = parent * max_subs + child
        assert _table_fetch(runtime, "SUB-XT", slot) == _xt(runtime, render)
        assert _table_fetch(runtime, "SUB-LBL-XT", slot) == _xt(runtime, label)

    for name in (
        "SCR-XT",
        "SCR-LBL-XT",
        "SCR-FLAGS",
        "SCR-KEY-XT",
        "SCR-ACT-XT",
        "SUB-COUNTS",
    ):
        assert _table_fetch(runtime, name, len(SCREEN_ROWS)) == RAW_CELL
    assert _table_fetch(runtime, "SUB-XT", 3) == RAW_CELL
    assert _table_fetch(runtime, "SUB-LBL-XT", 3) == RAW_CELL
    assert _table_fetch(runtime, "SUB-XT", max_subs + 2) == RAW_CELL
    assert _table_fetch(runtime, "SUB-LBL-XT", max_subs + 2) == RAW_CELL

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


def test_screen_tail_labels_publish_exact_registry_bytes() -> None:
    runtime = _load_screen_tail()
    labels = (
        ("LBL-HOME", b"Home"),
        ("LBL-BUFS", b"Bufs"),
        ("LBL-KERN", b"Kern"),
        ("LBL-PIPE", b"Pipe"),
        ("LBL-TASK", b"Task"),
        ("LBL-HELP", b"Help"),
        ("LBL-DOCS", b"Docs"),
        ("LBL-STOR", b"Stor"),
        ("LBL-CORE", b"Core"),
        ("LBL-OVERVIEW", b"Overview"),
        ("LBL-MEMORY", b"Memory"),
        ("LBL-NET", b"Network"),
        ("LBL-BLIST", b"List"),
        ("LBL-BSTATS", b"Stats"),
    )
    for name, expected in labels:
        assert _execute(runtime, name) == ()
        assert runtime.drain_uart_output() == expected


def test_screen_tail_render_leaks_subscreen_index_and_uses_raw_invalid_id() -> None:
    runtime = _load_screen_tail()
    calls: list[str] = []
    quiet = runtime.define_primitive("QUIET-SCREEN", lambda _context: None)
    marker = runtime.define_primitive(
        "RAW-ID-SCREEN",
        lambda _context: calls.append("raw-id"),
    )

    _table_store(runtime, "SUB-XT", 0, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", 1)
    _store_variable(runtime, "SUBSCREEN-ID", 0)
    assert _execute(runtime, "RENDER-SCREEN") == (0,)
    runtime.drain_uart_output()

    _table_store(runtime, "SCR-XT", 2, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", 3)
    assert _execute(runtime, "RENDER-SCREEN") == ()
    runtime.drain_uart_output()

    max_subs = _constant(runtime, "MAX-SUBS")
    raw_parent = 9
    raw_slot = raw_parent * max_subs
    _table_store(runtime, "SUB-COUNTS", raw_parent, 1)
    _table_store(runtime, "SUB-XT", raw_slot, marker.xt)
    _table_store(runtime, "SUB-LBL-XT", raw_slot, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", raw_parent + 1)
    assert _execute(runtime, "RENDER-SCREEN") == (0,)
    assert calls == ["raw-id"]
    runtime.drain_uart_output()


def test_screen_tail_task_keys_and_document_fallback_are_executable() -> None:
    runtime = _load_screen_tail()
    quiet = runtime.define_primitive("QUIET-TASK-REFRESH", lambda _context: None)
    _table_store(runtime, "SCR-XT", 2, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", 3)

    task = runtime.define_created("TAIL-TEST-TASK", initial_body=bytes(48))
    runtime.memory.write64(task.body_address, 1)
    _table_store(runtime, "TASK-TABLE", 0, task.body_address)
    _store_variable(runtime, "TASK-COUNT", 1)
    _store_variable(runtime, "SCR-SEL", 0)

    assert _execute(runtime, "TASK-KEYS", ord("k")) == (MASK64,)
    assert runtime.memory.read64(task.body_address) == _constant(runtime, "T.DONE")
    runtime.drain_uart_output()
    assert _execute(runtime, "TASK-KEYS", ord("s")) == (MASK64,)
    assert runtime.memory.read64(task.body_address) == _constant(runtime, "T.READY")
    runtime.drain_uart_output()
    assert _execute(runtime, "TASK-KEYS", ord("x")) == (0,)
    assert runtime.drain_uart_output() == b""

    _store_variable(runtime, "SCREEN-ID", 7)
    _store_variable(runtime, "SCR-SEL", 4)
    _store_variable(runtime, "FS-OK", 0)
    assert _execute(runtime, "DO-SELECT") == ()
    assert _variable(runtime, "DOC-SEL-N") == 4
    assert runtime.drain_uart_output() == b""


def test_screen_tail_caught_renderer_throw_exposes_saved_stack_pointer() -> None:
    runtime = _load_screen_tail()
    runtime.evaluate(b": TAIL-THROWING-SCREEN -77 THROW ;")
    throwing = runtime.find("TAIL-THROWING-SCREEN")
    assert throwing is not None
    _table_store(runtime, "SCR-XT", 2, throwing.xt)
    _store_variable(runtime, "SCREEN-ID", 3)
    saved_catch_sp = runtime.main_context.data.pointer - 2 * CELL_BYTES

    assert _execute(runtime, "RENDER-SCREEN") == (saved_catch_sp,)
    output = runtime.drain_uart_output()
    assert b"[screen error]\r\n" in output
    assert output.endswith(b"\x1b[0m\r\n")


def test_screen_tail_handle_key_pins_csi_and_empty_selection_defects() -> None:
    runtime = _load_screen_tail()
    quiet = runtime.define_primitive("QUIET-KEY-REFRESH", lambda _context: None)
    for child in range(3):
        _table_store(runtime, "SUB-XT", child, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", 1)
    _store_variable(runtime, "SUBSCREEN-ID", 0)

    assert _execute(runtime, "HANDLE-KEY", ord("[")) == (0,)
    assert _variable(runtime, "SUBSCREEN-ID") == 2
    runtime.drain_uart_output()
    assert _execute(runtime, "HANDLE-KEY", ord("]")) == (0,)
    assert _variable(runtime, "SUBSCREEN-ID") == 0
    runtime.drain_uart_output()

    runtime.inject_uart_input(b"[1;5D")
    assert _execute(runtime, "HANDLE-KEY", 27) == ()
    assert runtime.uart_input == b";5D"
    assert runtime.drain_uart_output() == b""
    for expected in b";5D":
        assert _execute(runtime, "KEY") == (expected,)

    runtime.evaluate(b": HANDLE-BARE-CSI 27 HANDLE-KEY ;")
    runtime.inject_uart_input(b"[")
    blocked = runtime.run_until_blocked("HANDLE-BARE-CSI", step_budget=5_000)
    assert isinstance(blocked, BlockedExecution)
    runtime.cancel_suspension(blocked.suspension)

    _table_store(runtime, "SCR-XT", 7, quiet.xt)
    _store_variable(runtime, "SCREEN-ID", 8)
    for key, selection in ((ord("n"), MASK64), (ord("p"), 0)):
        _store_variable(runtime, "SCR-SEL", selection)
        _store_variable(runtime, "SCR-MAX", 0)
        assert _execute(runtime, "HANDLE-KEY", key) == ()
        assert _variable(runtime, "SCR-SEL") == 0
        runtime.drain_uart_output()


def test_screen_tail_q_terminates_loop_and_bounded_public_entries() -> None:
    runtime = _load_screen_tail()
    runtime.inject_uart_input(b"q")
    assert _execute(runtime, "SCREEN-LOOP") == ()
    assert _variable(runtime, "SCREEN-RUN") == 0
    assert runtime.drain_uart_output() == (
        b"\x1b[2J\x1b[H Returned to REPL.\r\n"
    )

    quiet = runtime.define_primitive("QUIET-ENTRY-SCREEN", lambda _context: None)
    _table_store(runtime, "SUB-XT", 0, quiet.xt)
    runtime.inject_uart_input(b"q")
    assert _execute(runtime, "SCREENS") == (0,)
    assert _variable(runtime, "SCREEN-ID") == 1
    assert _variable(runtime, "SCR-SEL") == MASK64
    assert _variable(runtime, "SUBSCREEN-ID") == 0
    output = runtime.drain_uart_output()
    assert output.startswith(b"\x1b[2J\x1b[H")
    assert output.endswith(b"\x1b[2J\x1b[H Returned to REPL.\r\n")

    _table_store(runtime, "SCR-XT", 2, quiet.xt)
    runtime.inject_uart_input(b"q")
    assert _execute(runtime, "SCREEN", 3) == ()
    assert _variable(runtime, "SCREEN-ID") == 3
    runtime.drain_uart_output()


def test_screen_tail_reloading_is_source_literally_non_idempotent() -> None:
    runtime = _load_screen_tail()
    first_tail_words = runtime.dictionary.words[-len(DEFINITIONS) :]

    runtime = _evaluate_screen_tail(runtime)

    assert _variable(runtime, "NSCREENS") == _constant(runtime, "MAX-SCREENS")
    assert _table_fetch(runtime, "SUB-COUNTS", 0) == 6
    assert _table_fetch(runtime, "SUB-COUNTS", 1) == 4
    assert _table_fetch(runtime, "SCR-XT", 9) == _xt(runtime, b"SCR-HOME")
    assert _table_fetch(runtime, "SCR-XT", 15) == _xt(runtime, b"SCR-DOCS")
    assert _table_fetch(runtime, "SCR-KEY-XT", 4) == _xt(runtime, b"TASK-KEYS")
    assert runtime.dictionary.words[-2 * len(DEFINITIONS) : -len(DEFINITIONS)] == (
        first_tail_words
    )
