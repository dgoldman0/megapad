"""Unchanged-source acceptance for the KDOS screen registry/control layer."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64
from simulator.runtime import (
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    MegaForthRuntime,
)
from simulator.stacks import StackUnderflow
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_micro_clusters import _load_micro_clusters
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-interactive-screens-7569-7839.f"
)

FIRST_LINE = 7569
FIXTURE_LAST_LINE = 7839
LAST_LINE = 7838
FIXTURE_BYTES = 8_940
FIXTURE_SHA256 = (
    "740ed36cafdb1b83489acfdd0f6c1e32cc24a17d4434aaa973aca136f910b59a"
)
FIXTURE_GIT_BLOB = "c75ef264668947dd3a9ac3d56e1c6f46483c7db3"
SLICE_BYTES = 8_868
SLICE_SHA256 = (
    "c982515e55f9e94af0122ae1cd9e02af902774105bf59f65eae5a491973dfb82"
)
SLICE_GIT_BLOB = "467892ab2c4d04851a9c8db7dc95eafe860f3ec8"

MAX_SCREENS = 16
MAX_SUBS = 8
CELL_BYTES = 8
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 4_519

SOURCE_LEDGER = (
    ("VARIABLE", b"PORT-COUNT", 8),
    ("VARIABLE", b"PORT-RX", 8),
    ("VARIABLE", b"PORT-DROP", 8),
    (":", b"NET-RX?", 0),
    ("CONSTANT", b"MAX-SCREENS", 0),
    ("CONSTANT", b"MAX-SUBS", 0),
    ("CREATE", b"SCR-XT", 128),
    ("CREATE", b"SCR-LBL-XT", 128),
    ("CREATE", b"SCR-FLAGS", 128),
    ("CREATE", b"SCR-KEY-XT", 128),
    ("CREATE", b"SCR-ACT-XT", 128),
    ("CREATE", b"SUB-XT", 1_024),
    ("CREATE", b"SUB-LBL-XT", 1_024),
    ("CREATE", b"SUB-COUNTS", 128),
    ("VARIABLE", b"NSCREENS", 8),
    (":", b".HEXDIG", 0),
    (":", b"AT-XY", 0),
    (":", b"PAGE", 0),
    (":", b"CLS", 0),
    (":", b"BOLD", 0),
    (":", b"REVERSE", 0),
    (":", b"FG", 0),
    (":", b"BG-COLOR", 0),
    (":", b"HBAR", 0),
    (":", b".LABEL", 0),
    (":", b"./LABEL", 0),
    ("VARIABLE", b"SCREEN-ID", 8),
    ("VARIABLE", b"SCREEN-RUN", 8),
    ("VARIABLE", b"SCR-SEL", 8),
    ("VARIABLE", b"SCR-MAX", 8),
    ("VARIABLE", b"AUTO-REFRESH", 8),
    ("VARIABLE", b"REFRESH-LAST", 8),
    ("VARIABLE", b"SUBSCREEN-ID", 8),
    ("VARIABLE", b"FNA-WANT", 8),
    ("VARIABLE", b"FNA-FOUND", 8),
    (":", b"FIND-NTH-ACTIVE", 0),
    ("VARIABLE", b"DOC-SEL-N", 8),
    ("VARIABLE", b"DOC-SEL-FOUND", 8),
    (":", b"SHOW-NTH-DOC", 0),
    ("VARIABLE", b"STOR-N", 8),
    ("VARIABLE", b"DOC-N", 8),
    ("VARIABLE", b"DOC-TUT-COUNT", 8),
    ("VARIABLE", b"_ASUB-P", 8),
    ("VARIABLE", b"_ASUB-I", 8),
    (":", b"REGISTER-SCREEN", 0),
    (":", b"SET-SCREEN-KEYS", 0),
    (":", b"SET-SCREEN-ACT", 0),
    ("VARIABLE", b"_UNR-I", 8),
    ("VARIABLE", b"_UNR-N", 8),
    (":", b"(SHIFT-ARRAY)", 0),
    (":", b"(SHIFT-SUB-ARRAYS)", 0),
    (":", b"UNREGISTER-SCREEN", 0),
    (":", b"ADD-SUBSCREEN", 0),
    (":", b"SCREEN-SUBS", 0),
    (":", b"SCREEN-SELECTABLE?", 0),
    (":", b"SCREEN-HEADER", 0),
    (":", b"SUB-TABS", 0),
    (":", b"SCREEN-FOOTER", 0),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)

RAW_TABLES = {
    b"SCR-XT": 128,
    b"SCR-LBL-XT": 128,
    b"SCR-FLAGS": 128,
    b"SCR-KEY-XT": 128,
    b"SCR-ACT-XT": 128,
    b"SUB-XT": 1_024,
    b"SUB-LBL-XT": 1_024,
    b"SUB-COUNTS": 128,
}

INITIAL_VARIABLES = {
    b"PORT-COUNT": 0,
    b"PORT-RX": 0,
    b"PORT-DROP": 0,
    b"NSCREENS": 0,
    b"SCREEN-ID": 1,
    b"SCREEN-RUN": 0,
    b"SCR-SEL": MASK64,
    b"SCR-MAX": 0,
    b"AUTO-REFRESH": 0,
    b"REFRESH-LAST": 0,
    b"SUBSCREEN-ID": 0,
    b"FNA-WANT": 0,
    b"FNA-FOUND": 0,
    b"DOC-SEL-N": 0,
    b"DOC-SEL-FOUND": 0,
    b"STOR-N": 0,
    b"DOC-N": 0,
    b"DOC-TUT-COUNT": 0,
    b"_ASUB-P": 0,
    b"_ASUB-I": 0,
    b"_UNR-I": 0,
    b"_UNR-N": 0,
}


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
    boundary = (
        b"\\ =====================================================================\n"
    )
    assert lines[LAST_LINE - 1] == b"\n"
    assert lines[LAST_LINE] == boundary
    assert lines[LAST_LINE + 1] == (
        "\\ §9.5  Screen Definition Language (SDL) — Widget Vocabulary\n".encode(
            "utf-8"
        )
    )
    assert fixture.endswith(boundary)
    source = fixture[: -len(boundary)]
    assert len(source) == SLICE_BYTES
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    return source


def _evaluate_interactive_screens(
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


def _load_interactive_screens() -> MegaForthRuntime:
    return _evaluate_interactive_screens(_load_micro_clusters())


def _address(runtime: MegaForthRuntime, name: bytes | str) -> int:
    return _execute(runtime, name)[0]


def _cells(
    runtime: MegaForthRuntime,
    name: bytes | str,
    count: int,
    *,
    start: int = 0,
) -> tuple[int, ...]:
    base = _address(runtime, name)
    return tuple(
        runtime.memory.read64(base + (start + index) * CELL_BYTES)
        for index in range(count)
    )


def _store_variable(
    runtime: MegaForthRuntime,
    name: bytes | str,
    value: int,
) -> None:
    runtime.memory.write64(_address(runtime, name), value)


def test_interactive_screen_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_micro_clusters()
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

    runtime = _evaluate_interactive_screens(runtime)

    assert len(SOURCE_LEDGER) == 58
    assert sum(
        HOSTED_WORD_FIXED_BYTES + len(name) + body
        for _definer, name, body in SOURCE_LEDGER
    ) == HOSTED_DICTIONARY_GROWTH
    assert sum(RAW_TABLES.values()) == 2_816
    published = runtime.dictionary.words[-len(SOURCE_LEDGER) :]
    assert tuple(word.name for word in published) == DEFINITIONS
    assert runtime.dictionary.words[: -len(SOURCE_LEDGER)] == words_before
    assert runtime.dictionary.here - here_before == HOSTED_DICTIONARY_GROWTH
    assert runtime.dictionary.latest == published[-1].header_address

    prior_header = latest_before
    for index, ((definer, name, body_span), word) in enumerate(
        zip(SOURCE_LEDGER, published)
    ):
        assert runtime.memory.read64(word.header_address) == prior_header
        following = (
            published[index + 1].header_address
            if index + 1 < len(published)
            else runtime.dictionary.here
        )
        assert following - word.body_address == body_span
        expected_type = {
            "CONSTANT": ConstantDefinition,
            "CREATE": CreatedDefinition,
            "VARIABLE": CreatedDefinition,
            ":": ColonDefinition,
        }[definer]
        assert isinstance(word.implementation, expected_type)
        prior_header = word.header_address

    assert _constant(runtime, "MAX-SCREENS") == MAX_SCREENS
    assert _constant(runtime, "MAX-SUBS") == MAX_SUBS
    for name, value in INITIAL_VARIABLES.items():
        assert _variable(runtime, name) == value
    for name, length in RAW_TABLES.items():
        assert runtime.memory.read_bytes(_address(runtime, name), length) == (
            b"\xA5" * length
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


def test_absent_network_and_ansi_helpers_preserve_their_exact_byte_abi() -> None:
    runtime = _load_interactive_screens()
    context = runtime.new_context()
    context.data.push(0xCAFE)
    context.returns.push(0xBEEF)

    runtime.execute("NET-RX?", context=context, step_budget=100)

    assert context.data.snapshot() == (0xCAFE, 0)
    assert context.returns.snapshot() == (0xBEEF,)
    for value, expected in ((0, b"0"), (9, b"9"), (10, b"A"), (15, b"F")):
        assert _execute(runtime, ".HEXDIG", value) == ()
        assert runtime.drain_uart_output() == expected

    assert _execute(runtime, "AT-XY", 12, 34) == ()
    assert runtime.drain_uart_output() == b"\x1b[34;12H"
    for name in ("PAGE", "CLS"):
        assert _execute(runtime, name) == ()
        assert runtime.drain_uart_output() == b"\x1b[2J\x1b[H"
    for name, inputs, expected in (
        ("BOLD", (), b"\x1b[1m"),
        ("REVERSE", (), b"\x1b[7m"),
        ("FG", (6,), b"\x1b[36m"),
        ("BG-COLOR", (4,), b"\x1b[44m"),
        (".LABEL", (), b"\x1b[1m"),
        ("./LABEL", (), b"\x1b[0m"),
    ):
        assert _execute(runtime, name, *inputs) == ()
        assert runtime.drain_uart_output() == expected
    assert _execute(runtime, "HBAR") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[2m" + b"\xC4" * 60 + b"\x1b[0m\r\n"
    )


def test_directory_lookup_exposes_the_source_matched_path_extra_drop() -> None:
    runtime = _load_interactive_screens()
    directory = _address(runtime, "FS-DIR")
    entry_size = _constant(runtime, "FS-ENTRY-SIZE")

    assert _execute(runtime, "FIND-NTH-ACTIVE", 0) == (MASK64,)
    for slot in (3, 64, 127):
        runtime.memory.write8(directory + slot * entry_size, ord("A"))

    empty_caller = runtime.new_context()
    empty_caller.data.push(0)
    with pytest.raises(StackUnderflow, match="data stack underflow"):
        runtime.execute(
            "FIND-NTH-ACTIVE",
            context=empty_caller,
            step_budget=5_000,
        )
    assert empty_caller.data.snapshot() == ()
    assert empty_caller.returns.snapshot() == ()
    assert _variable(runtime, "FNA-WANT") == 0
    assert _variable(runtime, "FNA-FOUND") == 3

    canary_caller = runtime.new_context()
    canary_caller.data.push(0xCAFE)
    canary_caller.data.push(1)
    runtime.execute(
        "FIND-NTH-ACTIVE",
        context=canary_caller,
        step_budget=5_000,
    )
    assert canary_caller.data.snapshot() == (64,)
    assert canary_caller.returns.snapshot() == ()

    no_match_caller = runtime.new_context()
    no_match_caller.data.push(0xCAFE)
    no_match_caller.data.push(3)
    runtime.execute(
        "FIND-NTH-ACTIVE",
        context=no_match_caller,
        step_budget=5_000,
    )
    assert no_match_caller.data.snapshot() == (0xCAFE, MASK64)
    assert no_match_caller.returns.snapshot() == ()


def test_unmounted_doc_path_is_bounded_and_quiet() -> None:
    runtime = _load_interactive_screens()

    runtime.inject_uart_input(b"K")
    completion_before = runtime.storage.completion
    assert _execute(runtime, "SHOW-NTH-DOC", 7) == ()
    assert _variable(runtime, "DOC-SEL-N") == 7
    assert _variable(runtime, "DOC-SEL-FOUND") == 0
    assert runtime.storage.completion == completion_before
    assert runtime.uart_input == b"K"
    assert runtime.drain_uart_output() == b""


def test_registry_initializes_published_cells_and_enforces_capacity() -> None:
    runtime = _load_interactive_screens()
    renders = tuple(0x1000 + index for index in range(MAX_SCREENS))
    labels = tuple(0x2000 + index for index in range(MAX_SCREENS))
    flags = tuple(1 if index % 2 == 0 else 0 for index in range(MAX_SCREENS))
    for name, length in RAW_TABLES.items():
        runtime.memory.fill(_address(runtime, name), length, 0xA5)

    for index in range(MAX_SCREENS):
        assert _execute(
            runtime,
            "REGISTER-SCREEN",
            renders[index],
            labels[index],
            flags[index],
        ) == (index,)
        assert _cells(runtime, "SCR-KEY-XT", 1, start=index) == (0,)
        assert _cells(runtime, "SCR-ACT-XT", 1, start=index) == (0,)
        assert _cells(runtime, "SUB-COUNTS", 1, start=index) == (0,)
        assert _execute(runtime, "SET-SCREEN-KEYS", 0x3000 + index, index) == ()
        assert _execute(runtime, "SET-SCREEN-ACT", 0x4000 + index, index) == ()

    assert _variable(runtime, "NSCREENS") == MAX_SCREENS
    assert _cells(runtime, "SCR-XT", MAX_SCREENS) == renders
    assert _cells(runtime, "SCR-LBL-XT", MAX_SCREENS) == labels
    assert _cells(runtime, "SCR-FLAGS", MAX_SCREENS) == flags
    assert _cells(runtime, "SCR-KEY-XT", MAX_SCREENS) == tuple(
        0x3000 + index for index in range(MAX_SCREENS)
    )
    assert _cells(runtime, "SCR-ACT-XT", MAX_SCREENS) == tuple(
        0x4000 + index for index in range(MAX_SCREENS)
    )
    assert _cells(runtime, "SUB-COUNTS", MAX_SCREENS) == (0,) * MAX_SCREENS
    assert _variable(runtime, "SCR-SEL") == 0
    assert _variable(runtime, "SCR-MAX") == 0
    assert _execute(runtime, "SCREEN-SELECTABLE?") == (MASK64,)

    tables_before = {
        name: runtime.memory.read_bytes(_address(runtime, name), length)
        for name, length in RAW_TABLES.items()
    }
    assert _execute(runtime, "REGISTER-SCREEN", 0xAAAA, 0xBBBB, 1) == (
        MASK64,
    )
    assert _variable(runtime, "NSCREENS") == MAX_SCREENS
    for name, expected in tables_before.items():
        assert runtime.memory.read_bytes(_address(runtime, name), len(expected)) == (
            expected
        )

    _store_variable(runtime, "SCREEN-ID", 2)
    assert _execute(runtime, "SCREEN-SELECTABLE?") == (0,)


def test_subscreen_registration_caps_each_parent_at_eight_entries() -> None:
    runtime = _load_interactive_screens()
    assert _execute(runtime, "REGISTER-SCREEN", 1, 2, 1) == (0,)

    for index in range(MAX_SUBS):
        assert _execute(
            runtime,
            "ADD-SUBSCREEN",
            0x5000 + index,
            0x6000 + index,
            0,
        ) == ()
    tables_before = (
        _cells(runtime, "SUB-XT", MAX_SUBS),
        _cells(runtime, "SUB-LBL-XT", MAX_SUBS),
    )
    assert _execute(runtime, "ADD-SUBSCREEN", 0xAAAA, 0xBBBB, 0) == ()

    assert _cells(runtime, "SUB-COUNTS", 1) == (MAX_SUBS,)
    assert _execute(runtime, "SCREEN-SUBS") == (MAX_SUBS,)
    assert _cells(runtime, "SUB-XT", MAX_SUBS) == tuple(
        0x5000 + index for index in range(MAX_SUBS)
    )
    assert _cells(runtime, "SUB-LBL-XT", MAX_SUBS) == tuple(
        0x6000 + index for index in range(MAX_SUBS)
    )
    assert (
        _cells(runtime, "SUB-XT", MAX_SUBS),
        _cells(runtime, "SUB-LBL-XT", MAX_SUBS),
    ) == tables_before


def test_unregister_shifts_live_rows_but_preserves_source_stale_tails() -> None:
    runtime = _load_interactive_screens()
    for index in range(3):
        assert _execute(
            runtime,
            "REGISTER-SCREEN",
            0x100 + index,
            0x200 + index,
            index & 1,
        ) == (index,)
        assert _execute(runtime, "SET-SCREEN-KEYS", 0x300 + index, index) == ()
        assert _execute(runtime, "SET-SCREEN-ACT", 0x400 + index, index) == ()
        assert _execute(
            runtime,
            "ADD-SUBSCREEN",
            0x500 + index,
            0x600 + index,
            index,
        ) == ()

    _store_variable(runtime, "SCREEN-ID", 3)
    _store_variable(runtime, "SCR-SEL", 7)
    _store_variable(runtime, "SCR-MAX", 9)
    _store_variable(runtime, "SUBSCREEN-ID", 4)
    assert _execute(runtime, "UNREGISTER-SCREEN", 1) == ()

    assert _variable(runtime, "NSCREENS") == 2
    assert _variable(runtime, "SCREEN-ID") == 2
    assert _variable(runtime, "SCR-SEL") == 7
    assert _variable(runtime, "SCR-MAX") == 9
    assert _variable(runtime, "SUBSCREEN-ID") == 4
    assert _cells(runtime, "SCR-XT", 3) == (0x100, 0x102, 0x102)
    assert _cells(runtime, "SCR-LBL-XT", 3) == (0x200, 0x202, 0x202)
    assert _cells(runtime, "SCR-FLAGS", 3) == (0, 0, 0)
    assert _cells(runtime, "SCR-KEY-XT", 3) == (0x300, 0x302, 0x302)
    assert _cells(runtime, "SCR-ACT-XT", 3) == (0x400, 0x402, 0x402)
    assert _cells(runtime, "SUB-COUNTS", 3) == (1, 1, 1)
    assert _cells(runtime, "SUB-XT", 1, start=MAX_SUBS) == (0x502,)
    assert _cells(runtime, "SUB-XT", 1, start=2 * MAX_SUBS) == (0x502,)
    assert _cells(runtime, "SUB-LBL-XT", 1, start=MAX_SUBS) == (0x602,)
    assert _cells(runtime, "SUB-LBL-XT", 1, start=2 * MAX_SUBS) == (0x602,)

    assert _execute(runtime, "UNREGISTER-SCREEN", 1) == ()
    assert _variable(runtime, "NSCREENS") == 1
    assert _variable(runtime, "SCREEN-ID") == 1
    assert _variable(runtime, "SCR-SEL") == MASK64
    assert _variable(runtime, "SCR-MAX") == 0
    assert _variable(runtime, "SUBSCREEN-ID") == 4

    assert _execute(runtime, "UNREGISTER-SCREEN", 0) == ()
    assert _variable(runtime, "NSCREENS") == 0
    assert _variable(runtime, "SCREEN-ID") == 1
    assert _cells(runtime, "SCR-XT", 1) == (0x100,)
    assert _cells(runtime, "SUB-COUNTS", 1) == (1,)
    assert _execute(runtime, "UNREGISTER-SCREEN", 0) == ()
    assert _execute(runtime, "UNREGISTER-SCREEN", MASK64) == ()
    assert _variable(runtime, "NSCREENS") == 0


def test_header_tabs_and_footer_dispatch_registered_label_words() -> None:
    runtime = _load_interactive_screens()
    home = runtime.define_primitive(
        "TEST-HOME-LABEL",
        lambda _context: runtime.write_uart_bytes(b"Home"),
    )
    tasks = runtime.define_primitive(
        "TEST-TASKS-LABEL",
        lambda _context: runtime.write_uart_bytes(b"Tasks"),
    )
    main = runtime.define_primitive(
        "TEST-MAIN-LABEL",
        lambda _context: runtime.write_uart_bytes(b"Main"),
    )
    assert _execute(runtime, "REGISTER-SCREEN", 0, home.xt, 1) == (0,)
    assert _execute(runtime, "REGISTER-SCREEN", 0, tasks.xt, 0) == (1,)
    assert _execute(runtime, "ADD-SUBSCREEN", 0, main.xt, 0) == ()

    assert _execute(runtime, "SCREEN-HEADER") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[1;1H\x1b[7m  KDOS v1.1 \x1b[0m "
        b"\x1b[7m [0]Home \x1b[0m"
        b" [1]Tasks \x1b[0m\r\n"
        + b"\x1b[2m"
        + b"\xC4" * 60
        + b"\x1b[0m\r\n"
    )

    assert _execute(runtime, "SUB-TABS") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[2m   \x1b[1m[Main] \x1b[0m\x1b[2m\x1b[0m\r\n"
    )

    assert _execute(runtime, "SCREEN-FOOTER") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[2m  [0-1] Switch  [n/p] Select"
        b"  [[/]] Sub  [r] Refresh  [A]Auto   [q] Quit"
        b"\x1b[0m\r\n"
    )


def test_throwing_dynamic_labels_expose_the_source_catch_stack_leak() -> None:
    runtime = _load_interactive_screens()
    runtime.evaluate(
        b": TEST-THROWING-LABEL -77 THROW ;",
        source_name="throwing-screen-label",
    )
    throwing_label = runtime.find("TEST-THROWING-LABEL")
    assert throwing_label is not None
    assert _execute(
        runtime,
        "REGISTER-SCREEN",
        0,
        throwing_label.xt,
        1,
    ) == (0,)
    assert _execute(
        runtime,
        "ADD-SUBSCREEN",
        0,
        throwing_label.xt,
        0,
    ) == ()
    # CATCH snapshots the backed stack while label-xt and EXECUTE's xt are
    # live; this exact thrown path exposes that saved pointer as its leak.
    saved_catch_sp = runtime.main_context.data.pointer - 2 * CELL_BYTES

    assert _execute(runtime, "SCREEN-HEADER") == (saved_catch_sp,)
    header = runtime.drain_uart_output()
    assert b"[0]?" in header
    assert header.endswith(b"\x1b[0m\r\n")

    assert _execute(runtime, "SUB-TABS") == (saved_catch_sp,)
    assert b"[?]" in runtime.drain_uart_output()
