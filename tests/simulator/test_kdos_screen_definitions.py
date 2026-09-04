"""Unchanged-source acceptance for KDOS SDL and ordinary ANSI screens."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import MASK64
from simulator.ir import PushStringLiteral
from simulator.runtime import (
    BlockedExecution,
    ColonDefinition,
    ConstantDefinition,
    CreatedDefinition,
    MegaForthRuntime,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_interactive_screens import (
    _address,
    _load_interactive_screens,
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
    / "kdos-screen-definitions-7839-8340.f"
)

FIRST_LINE = 7839
FIXTURE_LAST_LINE = 8340
LAST_LINE = 8339
FIXTURE_BYTES = 18_097
FIXTURE_SHA256 = (
    "77df42902c4a666ade0d81a7d8402f56f2b29e1b937d34c0c963157c6f191058"
)
FIXTURE_GIT_BLOB = "d628a6cd1b1960e6e5c5cfc37cf286f09dd22fc6"
SLICE_BYTES = 18_051
SLICE_SHA256 = (
    "a47d29e51c6754e24852bea08261b3119389e8a1849b9e39322bf1e9013cce7d"
)
SLICE_GIT_BLOB = "01a3e0eff93567b66441e071003b3e7a25809d3d"

CELL_BYTES = 8
WVEC_SIZE = 15
HOSTED_WORD_FIXED_BYTES = 17
HOSTED_DICTIONARY_GROWTH = 4_297
LITERAL_POOL_BYTES = 1_939

STRING_BODY_BYTES = {
    b".TASK-DETAIL": 22,
    b"SCR-HOME": 168,
    b"SCR-BUFFERS": 8,
    b"SCR-KERNELS": 8,
    b"SCR-PIPES": 10,
    b"SCR-TASKS": 6,
    b"SCR-HELP": 1_054,
    b".DOCS-BODY": 48,
    b"SCR-DOCS": 14,
    b".STOR-BODY": 54,
    b"SCR-STORAGE": 8,
    b".CORES-BODY": 283,
    b"SCR-CORES": 6,
    b"SCR-HOME-MEMORY": 108,
    b"SCR-HOME-NET": 79,
    b".BSTATS-BODY": 31,
    b"SCR-BUF-STATS": 32,
}
STRING_BODY_SHA256 = {
    b".TASK-DETAIL": "02dd21c892f731f7d26845567e9aa35be43457de313a983c84fd46da10ebe97c",
    b"SCR-HOME": "ad4d51aa049bc5704db8bbbb4179482b584e23dea70ec2dd07a17baf88f7bfeb",
    b"SCR-BUFFERS": "77ec02a2004ce075e4cd07876781a1f6eef4d854e588408a26289b8ea4682a40",
    b"SCR-KERNELS": "504d8347ca75a3771e264995592fe8652d7e520cc99fef9ede7d97df088b7272",
    b"SCR-PIPES": "ee2e13251d30babdd65870c9062f03d19acfd81db8fc354685b7eae7dc573a2d",
    b"SCR-TASKS": "6ce7bd4525d61076b94a02c193e22c5e6540234f4acf6ea5aba1221972ba3a4c",
    b"SCR-HELP": "70b1ed33921fb16a5caef77511869531d338f501b3bd3733f6d0ad800c694725",
    b".DOCS-BODY": "f019ce04dd101b86806ff3d2b2a15b601eb09559947032d8a47355c3fb81d186",
    b"SCR-DOCS": "ae2dad353efc8163cdf543e964c02fe81db695b87d75aaee1a3b92dbf6d2cc35",
    b".STOR-BODY": "e2680c64f4a6c3cac99ed4d0fcb16d578a4b9bf29753d8e7ed9c99d0edaa99b8",
    b"SCR-STORAGE": "5d900d0360b23184f91961f89202753d8b2b8a1b65ad65934e6b725cc160e874",
    b".CORES-BODY": "d82a9a1b08d294ba899f07ffea9c29d471e005ed273ce90a582ac2ab40773612",
    b"SCR-CORES": "cef0e07367b28011128361e66b36e6cec1a5dacec15bf8b636a96a3c57d30b0a",
    b"SCR-HOME-MEMORY": "1105cccb80700eecc83a7f5201dac5d8721d7411e26567163a67f51d07996428",
    b"SCR-HOME-NET": "d3cbe82272d2fcddb4135490df6dc6bfc7e5c91bbd213680b922362f4013164a",
    b".BSTATS-BODY": "21557dce9cb1cd2f861fa2e950f8627354c57de9fdece9b4abcd016a69ca421c",
    b"SCR-BUF-STATS": "f336cbfe8b0acce4365e68b8d666852d4bd793a3f72789e1e43d6be96e7b6805",
}

SOURCE_LEDGER = (
    ("CONSTANT", b"WVEC-SIZE", 0),
    ("CREATE", b"WVEC", WVEC_SIZE * CELL_BYTES),
    ("CONSTANT", b"WV-TITLE", 0),
    ("CONSTANT", b"WV-SECTION", 0),
    ("CONSTANT", b"WV-LINE", 0),
    ("CONSTANT", b"WV-KV", 0),
    ("CONSTANT", b"WV-KV-XT", 0),
    ("CONSTANT", b"WV-FLAG", 0),
    ("CONSTANT", b"WV-FLAG-2", 0),
    ("CONSTANT", b"WV-HBAR", 0),
    ("CONSTANT", b"WV-GAP", 0),
    ("CONSTANT", b"WV-LIST", 0),
    ("CONSTANT", b"WV-DETAIL", 0),
    ("CONSTANT", b"WV-HINT", 0),
    ("CONSTANT", b"WV-CUSTOM", 0),
    ("CONSTANT", b"WV-NONE", 0),
    ("CONSTANT", b"WV-INPUT", 0),
    (":", b"WV@", 0),
    (":", b"WV!", 0),
    (":", b"TUI-TITLE", 0),
    (":", b"TUI-SECTION", 0),
    (":", b"TUI-LINE", 0),
    (":", b"TUI-KV", 0),
    (":", b"TUI-KV-XT", 0),
    (":", b"TUI-FLAG", 0),
    (":", b"TUI-FLAG-2", 0),
    (":", b"TUI-HBAR", 0),
    (":", b"TUI-GAP", 0),
    (":", b"TUI-LIST", 0),
    (":", b"TUI-DETAIL", 0),
    (":", b"TUI-HINT", 0),
    (":", b"TUI-CUSTOM", 0),
    (":", b"TUI-INPUT", 0),
    (":", b"INSTALL-TUI", 0),
    (":", b"W.TITLE", 0),
    (":", b"W.SECTION", 0),
    (":", b"W.LINE", 0),
    (":", b"W.KV", 0),
    (":", b"W.KV-XT", 0),
    (":", b"W.FLAG", 0),
    (":", b"W.FLAG-2", 0),
    (":", b"W.HBAR", 0),
    (":", b"W.GAP", 0),
    (":", b"W.LIST", 0),
    (":", b"W.DETAIL", 0),
    (":", b"W.HINT", 0),
    (":", b"W.CUSTOM", 0),
    (":", b"W.INPUT", 0),
    (":", b"W.TITLE-N", 0),
    (":", b".BTYPE", 0),
    (":", b".BUF-ROW", 0),
    (":", b".BUF-DETAIL", 0),
    (":", b".KERN-ROW", 0),
    (":", b".PIPE-ROW", 0),
    (":", b".TASK-STATUS", 0),
    (":", b".TASK-ROW", 0),
    (":", b".TASK-DETAIL", STRING_BODY_BYTES[b".TASK-DETAIL"]),
    (":", b".CORE-ROW", 0),
    (":", b".PORT-ROW", 0),
    (":", b".DOC-FILE-LIST", 0),
    (":", b".STOR-ROW", 0),
    (":", b".HOME-CORES-VAL", 0),
    (":", b".HOME-PORTS-VAL", 0),
    (":", b"SCR-HOME", STRING_BODY_BYTES[b"SCR-HOME"]),
    (":", b"SCR-BUFFERS", STRING_BODY_BYTES[b"SCR-BUFFERS"]),
    (":", b"SCR-KERNELS", STRING_BODY_BYTES[b"SCR-KERNELS"]),
    (":", b"SCR-PIPES", STRING_BODY_BYTES[b"SCR-PIPES"]),
    (":", b"SCR-TASKS", STRING_BODY_BYTES[b"SCR-TASKS"]),
    (":", b"SCR-HELP", STRING_BODY_BYTES[b"SCR-HELP"]),
    (":", b".DOCS-BODY", STRING_BODY_BYTES[b".DOCS-BODY"]),
    (":", b"SCR-DOCS", STRING_BODY_BYTES[b"SCR-DOCS"]),
    (":", b".STOR-BODY", STRING_BODY_BYTES[b".STOR-BODY"]),
    (":", b"SCR-STORAGE", STRING_BODY_BYTES[b"SCR-STORAGE"]),
    (":", b".CORES-BODY", STRING_BODY_BYTES[b".CORES-BODY"]),
    (":", b"SCR-CORES", STRING_BODY_BYTES[b"SCR-CORES"]),
    (":", b"SCR-HOME-OVERVIEW", 0),
    (":", b".HOME-MEM-BUFS", 0),
    (":", b"SCR-HOME-MEMORY", STRING_BODY_BYTES[b"SCR-HOME-MEMORY"]),
    (":", b"SCR-HOME-NET", STRING_BODY_BYTES[b"SCR-HOME-NET"]),
    (":", b"SCR-BUF-LIST", 0),
    ("VARIABLE", b"_SRAW", CELL_BYTES),
    ("VARIABLE", b"_SREC", CELL_BYTES),
    ("VARIABLE", b"_STIL", CELL_BYTES),
    ("VARIABLE", b"_SBIT", CELL_BYTES),
    (":", b".BSTATS-BODY", STRING_BODY_BYTES[b".BSTATS-BODY"]),
    (":", b"SCR-BUF-STATS", STRING_BODY_BYTES[b"SCR-BUF-STATS"]),
)
DEFINITIONS = tuple(name for _definer, name, _body in SOURCE_LEDGER)

WVEC_BINDINGS = (
    (0, b"TUI-TITLE"),
    (1, b"TUI-SECTION"),
    (2, b"TUI-LINE"),
    (3, b"TUI-KV"),
    (4, b"TUI-KV-XT"),
    (5, b"TUI-FLAG"),
    (6, b"TUI-FLAG-2"),
    (7, b"TUI-HBAR"),
    (8, b"TUI-GAP"),
    (9, b"TUI-LIST"),
    (10, b"TUI-DETAIL"),
    (11, b"TUI-HINT"),
    (12, b"TUI-CUSTOM"),
    (14, b"TUI-INPUT"),
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
    boundary = b"\\ ---- Screen label words (for registry) ----\n"
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


def _evaluate_screen_definitions(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_screen_definitions() -> MegaForthRuntime:
    return _evaluate_screen_definitions(_load_interactive_screens())


def _text(
    runtime: MegaForthRuntime,
    name: str,
    payload: bytes,
) -> tuple[int, int]:
    word = runtime.define_created(name, initial_body=payload)
    return word.body_address, len(payload)


def test_screen_definition_slice_is_exact_linked_and_load_time_pure() -> None:
    runtime = _load_interactive_screens()
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

    runtime = _evaluate_screen_definitions(runtime)

    assert len(SOURCE_LEDGER) == 86
    assert sum(STRING_BODY_BYTES.values()) == LITERAL_POOL_BYTES
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
    literal_count = 0
    for index, ((definer, _name, body_span), word) in enumerate(
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
        if isinstance(word.implementation, ColonDefinition):
            literals = tuple(
                operation
                for operation in word.implementation.operations
                if isinstance(operation, PushStringLiteral)
            )
            literal_count += len(literals)
            assert sum(literal.length + 1 for literal in literals) == body_span
            expected_offset = 0
            for literal in literals:
                assert literal.offset == expected_offset
                expected_offset += literal.length + 1
                assert runtime.memory.read8(
                    word.body_address + literal.offset + literal.length
                ) == 0
            if body_span:
                assert hashlib.sha256(
                    runtime.memory.read_bytes(word.body_address, body_span)
                ).hexdigest() == STRING_BODY_SHA256[word.name]
        prior_header = word.header_address

    assert literal_count == 102
    assert _constant(runtime, "WVEC-SIZE") == WVEC_SIZE
    for index, name in enumerate(
        (
            "WV-TITLE",
            "WV-SECTION",
            "WV-LINE",
            "WV-KV",
            "WV-KV-XT",
            "WV-FLAG",
            "WV-FLAG-2",
            "WV-HBAR",
            "WV-GAP",
            "WV-LIST",
            "WV-DETAIL",
            "WV-HINT",
            "WV-CUSTOM",
            "WV-NONE",
            "WV-INPUT",
        )
    ):
        assert _constant(runtime, name) == index
    assert _variable(runtime, "_SRAW") == 0
    assert _variable(runtime, "_SREC") == 0
    assert _variable(runtime, "_STIL") == 0
    assert _variable(runtime, "_SBIT") == 0
    wvec = _address(runtime, "WVEC")
    for index, target_name in WVEC_BINDINGS:
        target = runtime.find(target_name)
        assert target is not None
        assert runtime.memory.read64(wvec + index * CELL_BYTES) == target.xt
    assert runtime.memory.read64(wvec + 13 * CELL_BYTES) == int.from_bytes(
        b"\xA5" * CELL_BYTES,
        "little",
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


def test_screen_definition_public_widgets_preserve_tui_byte_output() -> None:
    runtime = _load_screen_definitions()
    name = _text(runtime, "WIDGET-NAME", b"Name")
    yes = _text(runtime, "WIDGET-YES", b"YES")
    no = _text(runtime, "WIDGET-NO", b"NO")

    cases = (
        ("W.TITLE", name, b"\x1b[1m  Name\x1b[0m\r\n\r\n"),
        ("W.SECTION", name, b"\r\n\x1b[1m  Name:\x1b[0m\r\n"),
        ("W.LINE", name, b"   Name\r\n"),
        ("W.KV", (42, *name), b"   Name : 42\r\n"),
        (
            "W.FLAG",
            (MASK64, *name),
            b"   Name : \x1b[32mON\x1b[0m\r\n",
        ),
        (
            "W.FLAG",
            (0, *name),
            b"   Name : \x1b[2mOFF\x1b[0m\r\n",
        ),
        ("W.HINT", name, b"\x1b[2m  Name\x1b[0m\r\n"),
        ("W.GAP", (), b"\r\n"),
        (
            "W.TITLE-N",
            (12, *name),
            b"\x1b[1m  Name (12)\x1b[0m\r\n\r\n",
        ),
    )
    for word, inputs, expected in cases:
        assert _execute(runtime, word, *inputs) == ()
        assert runtime.drain_uart_output() == expected

    for flag, expected in (
        (MASK64, b"\x1b[32mYES"),
        (0, b"\x1b[2mNO"),
    ):
        assert _execute(runtime, "W.FLAG-2", flag, *yes, *no, *name) == ()
        assert runtime.drain_uart_output() == (
            b"   Name : " + expected + b"\x1b[0m\r\n"
        )


def test_screen_definition_list_and_reversed_detail_selection_are_explicit() -> None:
    runtime = _load_screen_definitions()
    number = runtime.find(".N")
    assert number is not None

    assert _execute(runtime, "W.LIST", 0, number.xt) == ()
    assert runtime.drain_uart_output() == b"   (none)\r\n"
    assert _variable(runtime, "SCR-MAX") == 0

    _store_variable(runtime, "SCR-SEL", 1)
    assert _execute(runtime, "W.LIST", 3, number.xt) == ()
    assert runtime.drain_uart_output() == (
        b"   0\r\n"
        b"\x1b[32m > \x1b[0m1\r\n"
        b"   2\r\n"
    )
    assert _variable(runtime, "SCR-MAX") == 3

    expected_calls: list[str] = []
    accidental_calls: list[str] = []
    expected = runtime.define_primitive(
        "EXPECTED-DETAIL",
        lambda _context: expected_calls.append("expected"),
    )
    accidental = runtime.define_primitive(
        "ACCIDENTAL-DETAIL",
        lambda _context: accidental_calls.append("accidental"),
    )

    for selection in (MASK64, 1, 3):
        _store_variable(runtime, "SCR-SEL", selection)
        assert _execute(runtime, "W.DETAIL", 3, expected.xt) == ()
        assert runtime.drain_uart_output() == b""
    assert expected_calls == []
    assert accidental_calls == []

    _store_variable(runtime, "SCR-SEL", accidental.xt)
    assert _execute(runtime, "W.DETAIL", 3, expected.xt) == (expected.xt,)
    assert accidental_calls == ["accidental"]
    assert expected_calls == []
    assert runtime.drain_uart_output() == (
        b"\r\n\x1b[2m" + b"\xC4" * 60 + b"\x1b[0m\r\n"
    )


def test_screen_definition_input_handles_simple_edits_and_exposes_csi_leak() -> None:
    runtime = _load_screen_definitions()
    buffer = runtime.define_created("INPUT-BUFFER", initial_body=bytes(9))
    prompt = _text(runtime, "INPUT-PROMPT", b"> ")

    runtime.inject_uart_input(b"ab\x08C\r")
    assert _execute(
        runtime,
        "W.INPUT",
        buffer.body_address,
        8,
        *prompt,
    ) == (2,)
    assert runtime.memory.read_bytes(buffer.body_address, 3) == b"aC\0"
    assert runtime.drain_uart_output() == b"> ab\x08 \x08C"

    runtime.memory.fill(buffer.body_address, 9, 0)
    runtime.inject_uart_input(b"abc\r")
    assert _execute(
        runtime,
        "W.INPUT",
        buffer.body_address,
        2,
        *prompt,
    ) == (2,)
    assert runtime.memory.read_bytes(buffer.body_address, 3) == b"ab\0"
    assert runtime.drain_uart_output() == b"> ab"

    runtime.memory.fill(buffer.body_address, 9, 0xA5)
    runtime.inject_uart_input(b"\x1b")
    assert _execute(
        runtime,
        "W.INPUT",
        buffer.body_address,
        8,
        *prompt,
    ) == (0,)
    assert runtime.memory.read8(buffer.body_address) == 0
    assert runtime.drain_uart_output() == b"> "

    runtime.memory.fill(buffer.body_address, 9, 0xA5)
    runtime.inject_uart_input(b"\x1b[A\r")
    assert _execute(
        runtime,
        "W.INPUT",
        buffer.body_address,
        8,
        *prompt,
    ) == (0,)
    assert runtime.memory.read8(buffer.body_address) == 0
    assert runtime.drain_uart_output() == b"> "

    runtime.evaluate(
        f": PARAMETER-CSI {buffer.body_address} 8 {prompt[0]} {prompt[1]} "
        "W.INPUT ;".encode("ascii")
    )
    runtime.inject_uart_input(b"\x1b[1;5A")
    context = runtime.new_context()
    blocked = runtime.run_until_blocked(
        "PARAMETER-CSI",
        context=context,
        step_budget=5_000,
    )
    assert isinstance(blocked, BlockedExecution)
    assert context.data.snapshot() == (
        buffer.body_address,
        8,
        0,
        ord("1"),
        ord(";"),
        ord("5"),
    )
    assert runtime.drain_uart_output() == b"> "
    runtime.cancel_suspension(blocked.suspension)


def test_screen_definition_scalar_rows_preserve_tags_and_slot_leak() -> None:
    runtime = _load_screen_definitions()

    for value, expected in enumerate((b"raw", b"rec", b"til", b"bit", b"?")):
        assert _execute(runtime, ".BTYPE", value) == ()
        assert runtime.drain_uart_output() == expected

    for status, expected in (
        (0, b"\x1b[2mFREE \x1b[0m"),
        (1, b"\x1b[32mREADY\x1b[0m"),
        (2, b"\x1b[33mRUN  \x1b[0m"),
        (3, b"\x1b[31mBLOCK\x1b[0m"),
        (4, b"\x1b[2mDONE \x1b[0m"),
        (5, b"?    "),
    ):
        assert _execute(runtime, ".TASK-STATUS", status) == ()
        assert runtime.drain_uart_output() == expected

    assert _execute(runtime, ".CORE-ROW", 0) == ()
    assert runtime.drain_uart_output() == (
        b"0  [full] \x1b[33mRUNNING\x1b[0m (self)"
    )
    assert _execute(runtime, ".PORT-ROW", 5) == ()
    assert runtime.drain_uart_output() == b"port 5"
    assert _execute(runtime, ".STOR-ROW", 23, 4) == (23,)
    assert runtime.drain_uart_output() == b"4  "


def test_screen_definition_document_list_retains_source_count_semantics() -> None:
    runtime = _load_screen_definitions()
    document_type = _constant(runtime, "FTYPE-DOC")
    _store_variable(runtime, "DOC-TUT-COUNT", 77)
    completion = runtime.storage.completion

    assert _execute(runtime, ".DOC-FILE-LIST", document_type) == (0,)
    assert runtime.drain_uart_output() == (
        b"    (no filesystem loaded)\r\n"
    )
    assert _variable(runtime, "DOC-N") == 0
    assert _variable(runtime, "DOC-TUT-COUNT") == 77

    directory = _address(runtime, "FS-DIR")
    entry_size = _constant(runtime, "FS-ENTRY-SIZE")
    max_files = _constant(runtime, "FS-MAX-FILES")
    runtime.memory.fill(directory, entry_size * max_files, 0)
    for slot, name in enumerate((b"alpha", b"beta")):
        entry = directory + slot * entry_size
        runtime.memory.write_bytes(entry, name + b"\0")
        runtime.memory.write8(entry + 32, document_type)
    _store_variable(runtime, "FS-OK", 1)
    _store_variable(runtime, "SCR-SEL", 1)

    assert _execute(runtime, ".DOC-FILE-LIST", document_type) == (2,)
    assert runtime.drain_uart_output() == (
        b"    0  alpha\r\n"
        b"\x1b[32m > \x1b[0m1  beta\r\n"
    )
    assert _variable(runtime, "DOC-N") == 2
    assert _variable(runtime, "DOC-TUT-COUNT") == 2
    assert runtime.storage.completion == completion


def test_screen_definition_safe_composed_screens_use_ordinary_widgets() -> None:
    runtime = _load_screen_definitions()

    assert _execute(runtime, "SCR-STORAGE") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[1m  Storage\x1b[0m\r\n\r\n"
        b"   (no storage attached)\r\n"
    )
    assert _variable(runtime, "SCR-MAX") == 0

    assert _execute(runtime, "SCR-CORES") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[1m  Cores (1)\x1b[0m\r\n\r\n"
        b"   Single-core mode -- no secondary cores available.\r\n"
    )

    assert _execute(runtime, "SCR-HOME-NET") == ()
    assert runtime.drain_uart_output() == (
        b"\x1b[1m  Network Status\x1b[0m\r\n\r\n"
        b"   NIC state : \x1b[2midle\x1b[0m\r\n"
        b"   Ports : 0\r\n"
        b"   RX count : 0\r\n"
        b"   Drops : 0\r\n"
        b"\r\n\x1b[1m  Port Bindings:\x1b[0m\r\n"
        b"   (none)\r\n"
    )
    assert _variable(runtime, "SCR-MAX") == 0


def test_screen_definition_zero_buffer_stats_retain_stale_counters() -> None:
    runtime = _load_screen_definitions()
    _store_variable(runtime, "BUF-COUNT", 0)
    for name, value in (
        ("_SRAW", 1),
        ("_SREC", 2),
        ("_STIL", 3),
        ("_SBIT", 4),
    ):
        _store_variable(runtime, name, value)

    assert _execute(runtime, ".BSTATS-BODY") == ()
    assert runtime.drain_uart_output() == b""
    assert tuple(
        _variable(runtime, name)
        for name in ("_SRAW", "_SREC", "_STIL", "_SBIT")
    ) == (1, 2, 3, 4)
