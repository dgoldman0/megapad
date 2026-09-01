"""Unchanged-source acceptance for the KDOS Documentation Browser."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_FLUSH,
    STORAGE_RESULT_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_fd_pool import (
    FD_MAX,
    FD_SLOT_SIZE,
    _fd_snapshot,
    _open,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
    _store,
)
from tests.simulator.test_kdos_mp64fs_mutation import _mount
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)
from tests.simulator.test_kdos_subdirectory_navigation import (
    _load_subdirectory_navigation,
    _load_subdirectory_navigation_service,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-documentation-browser-6297-6427.f"
)

FIRST_LINE = 6297
LAST_LINE = 6427
SLICE_BYTES = 3_945
SLICE_SHA256 = (
    "442e5e39598d71a589bf19d6345c5bb042d678ba9f51607a878ae5030fbdcee6"
)
SLICE_GIT_BLOB = "242fc879957ba14f3a00b3284e8af921a4fa365c"

SOURCE_LEDGER = (
    ("CONSTANT", b"FTYPE-DOC"),
    ("CONSTANT", b"FTYPE-TUT"),
    ("CREATE", b"DOC-BUF"),
    ("VARIABLE", b"DOC-LINES"),
    ("CONSTANT", b"PAGE-LINES"),
    (":", b".DOC-CHUNK"),
    (":", b"SHOW-FILE"),
    (":", b"TOPICS"),
    (":", b"LESSONS"),
    (":", b"DOC"),
    (":", b"TUTORIAL"),
    (":", b"OPEN-BY-SLOT"),
    (":", b"DESCRIBE"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)


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
        b'    ."  Removed dir: " NAMEBUF .ZSTR CR ;\n'
    )
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == (
        b"\\ =====================================================================\n"
    )
    assert lines[LAST_LINE + 1] == (
        "\\  §7.8  Dictionary Search — WORDS-LIKE, APROPOS\n".encode(
            "utf-8"
        )
    )
    return source


def _evaluate_documentation_browser(
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


def _load_documentation_browser(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_documentation_browser(
        _load_subdirectory_navigation(image)
    )


def _load_documentation_browser_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_documentation_browser(
        _load_subdirectory_navigation_service(storage)
    )


def _write_file(
    image: bytearray,
    slot: int,
    name: bytes,
    sector: int,
    payload: bytes,
    *,
    entry_type: int = 4,
    parent: int = 0xFF,
    flags: int = 0,
) -> int:
    count = max(1, (len(payload) + SECTOR_SIZE - 1) // SECTOR_SIZE)
    entry = _write_entry(
        image,
        slot,
        name=name + b"\0",
        start=sector,
        count=count,
        used=len(payload),
        entry_type=entry_type,
        parent=parent,
    )
    image[entry + 33] = flags
    start = sector * SECTOR_SIZE
    image[start : start + len(payload)] = payload
    return entry


def _fd_headers(runtime: MegaForthRuntime) -> tuple[int, ...]:
    pool = _execute(runtime, "FD-POOL")[0]
    return tuple(
        runtime.memory.read64(pool + index * FD_SLOT_SIZE)
        for index in range(FD_MAX)
    )


def _publish_chunk(
    runtime: MegaForthRuntime,
    payload: bytes,
    *,
    name: str,
) -> None:
    source = runtime.define_created(name, initial_body=payload)
    assert _execute(
        runtime,
        ".DOC-CHUNK",
        source.body_address,
        len(payload),
    ) == ()


def test_documentation_browser_slice_is_exact_and_load_time_pure() -> None:
    runtime = _load_subdirectory_navigation(_formatted_image())
    _store(runtime, "CWD", 7)
    namebuf = _execute(runtime, "NAMEBUF")[0]
    pathbuf = _execute(runtime, "PATHBUF")[0]
    runtime.memory.write_bytes(namebuf, bytes(range(24)))
    runtime.memory.write_bytes(pathbuf, bytes(range(128)))
    _store(runtime, "PN-LEN", 17)
    parser_before = (
        runtime.memory.read_bytes(namebuf, 24),
        runtime.memory.read_bytes(pathbuf, 128),
        _variable(runtime, "PN-LEN"),
    )
    runtime.rtc.set_epoch_ms(0x0102_0304_0506)
    runtime.inject_uart_input(b"\x00K")
    runtime.write_uart_bytes(b"retained-output")
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    runtime = _evaluate_documentation_browser(runtime)

    assert len(SOURCE_LEDGER) == 13
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _constant(runtime, "FTYPE-DOC") == 4
    assert _constant(runtime, "FTYPE-TUT") == 6
    assert _constant(runtime, "PAGE-LINES") == 20
    assert _variable(runtime, "DOC-LINES") == 0
    doc_buffer = runtime.find("DOC-BUF")
    doc_lines = runtime.find("DOC-LINES")
    page_lines = runtime.find("PAGE-LINES")
    assert doc_buffer is not None
    assert doc_lines is not None
    assert page_lines is not None
    assert doc_lines.header_address - doc_buffer.body_address == SECTOR_SIZE
    assert page_lines.header_address - doc_lines.body_address == 8

    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (
        runtime.memory.read_bytes(namebuf, 24),
        runtime.memory.read_bytes(pathbuf, 128),
        _variable(runtime, "PN-LEN"),
    ) == parser_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.uart_input == b"\x00K"
    assert runtime.drain_uart_output() == b"retained-output"
    assert runtime.spinlocks.owner(2) is None


def test_doc_chunk_preserves_controls_and_pages_across_chunk_boundaries() -> None:
    runtime = _load_documentation_browser()
    first = b"A\x00\t\rB\n" + b"\n" * 18
    _publish_chunk(runtime, first, name="DOC-CHUNK-FIRST")

    assert runtime.drain_uart_output() == b"A\x00\t\rB" + b"\r\n" * 19
    assert _variable(runtime, "DOC-LINES") == 19
    assert runtime.uart_input == b""

    runtime.inject_uart_input(b"xy")
    _publish_chunk(runtime, b"tail\n", name="DOC-CHUNK-TWENTIETH")
    assert runtime.drain_uart_output() == (
        b"tail\r\n\x1b[2m --- more ---\x1b[0m\r\n"
    )
    assert _variable(runtime, "DOC-LINES") == 0
    assert runtime.uart_input == b"y"

    _publish_chunk(runtime, b"next\n", name="DOC-CHUNK-AFTER-PAGE")
    assert runtime.drain_uart_output() == b"next\r\n"
    assert _variable(runtime, "DOC-LINES") == 1
    assert runtime.uart_input == b"y"
    assert runtime.storage.completion == 0


def test_show_file_covers_empty_sector_boundary_and_nonzero_cursor() -> None:
    image = _formatted_image(23)
    ordinary_cases = (
        (b"empty", 14, b"", 0, b"", 0),
        (b"short", 15, b"A\nB\x00", 0, b"A\r\nB\x00", 1),
        (b"exact", 16, b"Q" * SECTOR_SIZE, 0, b"Q" * SECTOR_SIZE, 1),
        (
            b"cross",
            17,
            b"R" * (SECTOR_SIZE + 1),
            0,
            b"R" * (SECTOR_SIZE + 1),
            2,
        ),
        (b"cursor", 19, b"prefix-rest", 7, b"rest", 1),
    )
    for slot, (name, sector, payload, _cursor, _output, _reads) in enumerate(
        ordinary_cases
    ):
        _write_file(image, slot, name, sector, payload)

    logical_split = b"P" * SECTOR_SIZE + b"E"
    _write_entry(
        image,
        len(ordinary_cases),
        name=b"split\0",
        start=20,
        count=1,
        used=len(logical_split),
        entry_type=4,
        secondary_start=22,
        secondary_count=1,
    )
    image[20 * SECTOR_SIZE : 21 * SECTOR_SIZE] = b"P" * SECTOR_SIZE
    image[21 * SECTOR_SIZE] = ord("L")
    image[22 * SECTOR_SIZE] = ord("E")
    cases = ordinary_cases + (
        (
            b"split",
            20,
            logical_split,
            0,
            b"P" * SECTOR_SIZE + b"L",
            2,
        ),
    )

    runtime = _load_documentation_browser(image)
    _mount(runtime)

    for name, _sector, payload, cursor, expected, reads in cases:
        descriptor = _open(runtime, name.decode("ascii"))
        assert descriptor != 0
        if cursor:
            assert _execute(runtime, "FSEEK", cursor, descriptor) == ()
        completion_before = runtime.storage.completion

        assert _execute(runtime, "SHOW-FILE", descriptor) == ()

        assert runtime.drain_uart_output() == expected
        assert _execute(runtime, "F.CURSOR", descriptor) == (len(payload),)
        assert runtime.storage.completion == completion_before + reads
        assert runtime.memory.read64(descriptor - 8) == MASK64
        assert _variable(runtime, "DOC-LINES") == expected.count(b"\r\n")
        assert _execute(runtime, "(FCLOSE-NOFS)", descriptor) == ()
        assert runtime.memory.read64(descriptor - 8) == 0

    assert runtime.uart_input == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_topics_and_lessons_scan_all_slots_without_cwd_filtering() -> None:
    image = _formatted_image(19)
    _write_file(image, 0, b"alpha", 14, b"")
    _write_entry(
        image,
        1,
        name=b"section\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    _write_file(image, 2, b"child", 15, b"", parent=1)
    _write_file(
        image,
        3,
        b"lesson",
        16,
        b"",
        entry_type=6,
        parent=1,
    )
    _write_file(image, 4, b"ignored", 17, b"", entry_type=2)
    _write_file(image, 127, b"last", 18, b"", entry_type=6)
    runtime = _load_documentation_browser(image)
    _mount(runtime)
    _store(runtime, "CWD", 1)
    completion_before = runtime.storage.completion
    media_before = runtime.storage.image_bytes

    assert _execute(runtime, "TOPICS") == ()
    assert runtime.drain_uart_output() == (
        b" Available topics:\r\n"
        b"   alpha\r\n"
        b"   child\r\n"
        b" (2  topics)\r\n"
    )
    assert _execute(runtime, "LESSONS") == ()
    assert runtime.drain_uart_output() == (
        b" Available lessons:\r\n"
        b"   lesson\r\n"
        b"   last\r\n"
        b" (2  lessons)\r\n"
    )
    assert _variable(runtime, "CWD") == 1
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before
    assert _fd_headers(runtime) == (0,) * FD_MAX


def test_doc_and_tutorial_open_current_cwd_without_type_or_flag_checks() -> None:
    image = _formatted_image(16)
    _write_file(
        image,
        0,
        b"page",
        14,
        b"root-raw",
        entry_type=2,
        flags=0xE4,
    )
    _write_entry(
        image,
        1,
        name=b"section\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    _write_file(
        image,
        2,
        b"page",
        15,
        b"child-raw",
        entry_type=4,
        parent=1,
        flags=0xA4,
    )
    runtime = _load_documentation_browser(image)
    _mount(runtime)
    media_before = runtime.storage.image_bytes

    completion_before = runtime.storage.completion
    runtime.evaluate(b"DOC page", source_name="doc-root-ordinary-open")
    assert runtime.drain_uart_output() == b"\r\nroot-raw\r\n"
    assert runtime.storage.completion == completion_before + 4
    assert _fd_headers(runtime) == (0,) * FD_MAX

    _store(runtime, "CWD", 1)
    completion_before = runtime.storage.completion
    runtime.evaluate(
        b"TUTORIAL page",
        source_name="tutorial-child-ordinary-open",
    )
    assert runtime.drain_uart_output() == b"\r\nchild-raw\r\n"
    assert runtime.storage.completion == completion_before + 4

    # Both successful closes rewrite unchanged metadata and flush it.
    assert runtime.storage.image_bytes == media_before
    assert _variable(runtime, "CWD") == 1
    assert _fd_headers(runtime) == (0,) * FD_MAX
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_open_by_slot_populates_the_lowest_fd_and_reports_exhaustion() -> None:
    image = _formatted_image(15)
    _write_file(image, 7, b"slot-file", 14, b"payload", entry_type=2)
    runtime = _load_documentation_browser(image)
    _mount(runtime)
    pool = _execute(runtime, "FD-POOL")[0]
    sentinel = 0xA1A2_A3A4_A5A6_A7A8
    runtime.memory.write64(pool + 64, sentinel)
    completion_before = runtime.storage.completion
    mount_before = _mount_snapshot(runtime)

    descriptor = _execute(runtime, "OPEN-BY-SLOT", 7)[0]
    assert descriptor == pool + 8
    assert _fd_snapshot(runtime, descriptor) == (
        14,
        1,
        7,
        0,
        7,
        0,
        0,
        sentinel,
    )
    assert _execute(runtime, "OPEN-BY-SLOT", 8) == (0,)
    assert _fd_headers(runtime) == (MASK64,) + (0,) * (FD_MAX - 1)

    allocated = [_execute(runtime, "FD-ALLOC")[0] for _ in range(FD_MAX - 1)]
    assert allocated == [
        pool + index * FD_SLOT_SIZE + 8 for index in range(1, FD_MAX)
    ]
    assert _execute(runtime, "OPEN-BY-SLOT", 7) == (0,)
    assert runtime.drain_uart_output() == b" No free FD slots\r\n"
    assert _fd_headers(runtime) == (MASK64,) * FD_MAX
    assert runtime.storage.completion == completion_before
    assert _mount_snapshot(runtime) == mount_before


def test_describe_is_global_case_sensitive_truncated_and_first_match_wins() -> None:
    image = _formatted_image(16)
    name = b"x" * 23
    _write_file(image, 2, name, 14, b"first")
    _write_entry(
        image,
        5,
        name=b"section\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    _write_file(image, 7, name, 15, b"second", parent=5)
    runtime = _load_documentation_browser(image)
    _mount(runtime)
    _store(runtime, "CWD", 5)
    completion_before = runtime.storage.completion

    runtime.evaluate(
        b"DESCRIBE " + b"x" * 30,
        source_name="describe-truncated-global-duplicate",
    )
    assert runtime.drain_uart_output() == b"\r\nfirst\r\n"
    assert runtime.storage.completion == completion_before + 4
    assert _variable(runtime, "PN-LEN") == 23
    namebuf = _execute(runtime, "NAMEBUF")[0]
    assert runtime.memory.read_bytes(namebuf, 24) == name + b"\0"
    assert _fd_headers(runtime) == (0,) * FD_MAX

    completion_before = runtime.storage.completion
    runtime.evaluate(
        b"DESCRIBE " + b"X" * 30,
        source_name="describe-case-sensitive-miss",
    )
    assert runtime.drain_uart_output() == (
        b" No doc for: " + b"X" * 23 + b"\r\n"
        b" Use TOPICS to list available documentation.\r\n"
    )
    assert runtime.storage.completion == completion_before
    assert _variable(runtime, "CWD") == 5
    assert _fd_headers(runtime) == (0,) * FD_MAX


def test_browser_open_failures_do_not_leak_descriptors_and_pin_parse_order() -> None:
    absent = _load_documentation_browser()
    pool = _execute(absent, "FD-POOL")[0]
    pool_before = absent.memory.read_bytes(pool, FD_MAX * FD_SLOT_SIZE)

    doc_context = absent.new_context()
    absent.evaluate(b"DOC 41", context=doc_context, source_name="doc-no-fs")
    assert doc_context.data.snapshot() == (0, 41)
    assert doc_context.returns.snapshot() == ()
    assert absent.drain_uart_output() == b" No filesystem\r\n"

    tutorial_context = absent.new_context()
    absent.evaluate(
        b"TUTORIAL 42",
        context=tutorial_context,
        source_name="tutorial-no-fs",
    )
    assert tutorial_context.data.snapshot() == (0, 42)
    assert tutorial_context.returns.snapshot() == ()
    assert absent.drain_uart_output() == b" No filesystem\r\n"

    describe_context = absent.new_context()
    absent.evaluate(
        b"DESCRIBE 43",
        context=describe_context,
        source_name="describe-no-fs",
    )
    assert describe_context.data.snapshot() == ()
    assert describe_context.returns.snapshot() == ()
    assert absent.drain_uart_output() == b" No filesystem\r\n"
    assert _variable(absent, "PN-LEN") == 2
    namebuf = _execute(absent, "NAMEBUF")[0]
    assert absent.memory.read_bytes(namebuf, 24) == b"43" + bytes(22)

    absent.evaluate(b"DESCRIBE", source_name="describe-missing-operand")
    assert absent.drain_uart_output() == b" Usage: DESCRIBE <word>\r\n"
    assert absent.memory.read_bytes(pool, FD_MAX * FD_SLOT_SIZE) == pool_before
    assert absent.storage.completion == 0

    image = _formatted_image(15)
    _write_file(image, 0, b"present", 14, b"")
    mounted = _load_documentation_browser(image)
    _mount(mounted)
    completion_before = mounted.storage.completion
    pool = _execute(mounted, "FD-POOL")[0]
    pool_before = mounted.memory.read_bytes(pool, FD_MAX * FD_SLOT_SIZE)

    doc_context = mounted.new_context()
    mounted.evaluate(
        b"DOC absent",
        context=doc_context,
        source_name="doc-missing",
    )
    assert doc_context.data.snapshot() == (0,)
    assert doc_context.returns.snapshot() == ()
    assert mounted.drain_uart_output() == b" Not found: absent\r\n"
    tutorial_context = mounted.new_context()
    mounted.evaluate(
        b"TUTORIAL absent",
        context=tutorial_context,
        source_name="tutorial-missing",
    )
    assert tutorial_context.data.snapshot() == (0,)
    assert tutorial_context.returns.snapshot() == ()
    assert mounted.drain_uart_output() == b" Not found: absent\r\n"
    describe_context = mounted.new_context()
    mounted.evaluate(
        b"DESCRIBE absent",
        context=describe_context,
        source_name="describe-missing",
    )
    assert describe_context.data.snapshot() == ()
    assert describe_context.returns.snapshot() == ()
    assert mounted.drain_uart_output() == (
        b" No doc for: absent\r\n"
        b" Use TOPICS to list available documentation.\r\n"
    )
    assert mounted.memory.read_bytes(pool, FD_MAX * FD_SLOT_SIZE) == pool_before
    assert mounted.storage.completion == completion_before

    descriptors = [_execute(mounted, "FD-ALLOC")[0] for _ in range(FD_MAX)]
    assert descriptors == [
        pool + index * FD_SLOT_SIZE + 8 for index in range(FD_MAX)
    ]
    describe_context = mounted.new_context()
    mounted.evaluate(
        b"DESCRIBE present",
        context=describe_context,
        source_name="describe-fd-exhausted",
    )
    assert describe_context.data.snapshot() == (0,)
    assert describe_context.returns.snapshot() == ()
    assert mounted.drain_uart_output() == b" No free FD slots\r\n"
    assert _fd_headers(mounted) == (MASK64,) * FD_MAX
    assert mounted.storage.completion == completion_before


def test_doc_late_close_failure_retains_its_allocated_descriptor() -> None:
    image = _formatted_image(15)
    _write_file(image, 0, b"page", 14, b"abc")
    storage = HostedStorageService(
        image,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_documentation_browser_service(storage)
    _mount(runtime)
    pool = _execute(runtime, "FD-POOL")[0]
    completion_before = storage.completion
    media_before = storage.image_bytes

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(b"DOC page", source_name="doc-late-close-failure")

    descriptor = pool + 8
    assert runtime.drain_uart_output() == b"\r\nabc\r\nDisk flush failed"
    assert storage.completion == completion_before + 3
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.memory.read64(pool) == MASK64
    assert _fd_snapshot(runtime, descriptor)[:7] == (14, 1, 3, 3, 0, 0, 0)
    assert _fd_headers(runtime) == (MASK64,) + (0,) * (FD_MAX - 1)
    assert storage.image_bytes == media_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.spinlocks.owner(2) is None
