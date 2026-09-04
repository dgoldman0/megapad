"""Unchanged-source harness for the MP64FS source loader."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.cells import TRUE, u64
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CMD_READ,
    STORAGE_RESULT_MEDIA_REMOVED,
)
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import (
    _formatted_image,
    _write_entry,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_fd_pool import (
    _load_mp64fs_fd_pool,
    _load_mp64fs_fd_pool_service,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
    _store,
)
from tests.simulator.test_kdos_mp64fs_mutation import (
    _mount,
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
    / "kdos-mp64fs-load-5611-5944.f"
)

FIRST_LINE = 5611
LAST_LINE = 5944
SLICE_BYTES = 11_980
SLICE_SHA256 = (
    "6a30453c933ac8666c1b798a98a4fb3e6a331afeb4c2d3048299a83a0ea79a7c"
)
SLICE_GIT_BLOB = "f2bea50138ca04e235358debd734a4fc234e002a"

# Exact defining-word/name order in kdos.f.  Keeping the defining word in
# the ledger makes source-layout changes visible even if a replacement happens
# to publish the same dictionary name.
SOURCE_LEDGER = (
    ("VARIABLE", b"LD-BUF"),
    ("VARIABLE", b"LD-SZ"),
    ("VARIABLE", b"LD-CUR"),
    ("VARIABLE", b"LD-LEN"),
    ("VARIABLE", b"LD-LINE"),
    ("CONSTANT", b"_LD-FRAME"),
    ("CONSTANT", b"_LD-MAXLVL"),
    ("CREATE", b"_LD-STK"),
    ("VARIABLE", b"_LD-SP"),
    (":", b"_LD-ACTIVE-FRAME"),
    (":", b"_LD-EVAL-CHECKPOINT"),
    (":", b"_LD-TXN-HEAD"),
    (":", b"_LD-TXN-NOOP"),
    ("DEFER", b"_LD-TXN-COMMIT"),
    ("DEFER", b"_LD-TXN-ROLLBACK"),
    ("DEFER", b"_LD-TXN-AFTER-RELEASE"),
    (":", b"_LD-SAVE"),
    (":", b"_LD-RESTORE"),
    ("VARIABLE", b"_LD-RUN-SEC"),
    ("VARIABLE", b"_LD-RUN-CNT"),
    ("VARIABLE", b"_LD-RUN-ADDR"),
    (":", b"_LD-READ-RUN"),
    (":", b"_LD-SLOT-BYTES"),
    (":", b"_LD-READ-SLOT"),
    ("CREATE", b"_RP-PATH"),
    ("CREATE", b"_RP-COMP"),
    ("VARIABLE", b"_RP-I"),
    (":", b"_HAS-SLASH?"),
    (":", b"_RP-NEXT-SEP"),
    (":", b"_RP-IS-DOTDOT?"),
    (":", b"_RP-CD-COMP"),
    (":", b"_RESOLVE-PATH"),
    ("CONSTANT", b"EVAL-S-OK"),
    ("CONSTANT", b"EVAL-S-UNDEFINED"),
    ("CONSTANT", b"EVAL-S-LINE-TOO-LONG"),
    ("CONSTANT", b"EVAL-S-DEPTH"),
    ("CONSTANT", b"EVAL-S-UNFINISHED"),
    ("CONSTANT", b"EVAL-S-THROW"),
    (":", b"EVALUATE-CHECKED"),
    ("VARIABLE", b"_SEC-CUR"),
    ("VARIABLE", b"_SEC-REM"),
    ("VARIABLE", b"_SEC-RAW-LEN"),
    ("VARIABLE", b"_SEC-EVAL-LEN"),
    ("VARIABLE", b"_SEC-LINE"),
    (":", b"_SEC-MEASURE"),
    (":", b"_SEC-ADVANCE"),
    (":", b"SOURCE-EVALUATE-CHECKED"),
    (":", b"_LD-STATUS-THROW"),
    (":", b"_LD-WALK"),
    (":", b"_LD-RELEASE"),
    (":", b"_LD-FAIL"),
    (":", b"_LD-GUARDED"),
    (":", b"_LD-WALK-GUARDED"),
    (":", b"_LD-READ-WALK"),
    (":", b"LOAD"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)

CONSTANTS = (
    ("_LD-FRAME", 88),
    ("_LD-MAXLVL", 16),
    ("EVAL-S-OK", 0),
    ("EVAL-S-UNDEFINED", 1),
    ("EVAL-S-LINE-TOO-LONG", 2),
    ("EVAL-S-DEPTH", 3),
    ("EVAL-S-UNFINISHED", 4),
    ("EVAL-S-THROW", 5),
)

VARIABLES = (
    "LD-BUF",
    "LD-SZ",
    "LD-CUR",
    "LD-LEN",
    "LD-LINE",
    "_LD-SP",
    "_LD-RUN-SEC",
    "_LD-RUN-CNT",
    "_LD-RUN-ADDR",
    "_RP-I",
    "_SEC-CUR",
    "_SEC-REM",
    "_SEC-RAW-LEN",
    "_SEC-EVAL-LEN",
    "_SEC-LINE",
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
    assert lines[LAST_LINE] == (
        "\\ ── Application Loading " + "─" * 46 + "\n"
    ).encode("utf-8")
    return source


def _evaluate_mp64fs_load(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_load(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_load(_load_mp64fs_fd_pool(image))


def _load_mp64fs_load_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_load(_load_mp64fs_fd_pool_service(storage))


def _source_evaluate_checked(
    runtime: MegaForthRuntime,
    source: bytes,
    *,
    name: str,
) -> tuple[int, ...]:
    source_word = runtime.define_created(name, initial_body=source)
    return _execute(
        runtime,
        "SOURCE-EVALUATE-CHECKED",
        source_word.body_address,
        len(source),
    )


def _eval_token(runtime: MegaForthRuntime) -> bytes:
    address, length = _execute(runtime, "EVAL-TOKEN")
    return runtime.memory.read_bytes(address, length)


def _install_loader_trace(runtime: MegaForthRuntime) -> None:
    runtime.evaluate(
        b"VARIABLE LOAD-HOOK-TRACE 0 LOAD-HOOK-TRACE ! "
        b": TEST-LD-COMMIT LOAD-HOOK-TRACE @ 10 * 1 + "
        b"LOAD-HOOK-TRACE ! ; "
        b": TEST-LD-ROLLBACK LOAD-HOOK-TRACE @ 10 * 2 + "
        b"LOAD-HOOK-TRACE ! ; "
        b": TEST-LD-AFTER LOAD-HOOK-TRACE @ 10 * 3 + "
        b"LOAD-HOOK-TRACE ! ; "
        b"' TEST-LD-COMMIT IS _LD-TXN-COMMIT "
        b"' TEST-LD-ROLLBACK IS _LD-TXN-ROLLBACK "
        b"' TEST-LD-AFTER IS _LD-TXN-AFTER-RELEASE"
    )


def _seed_loader_globals(runtime: MegaForthRuntime) -> tuple[int, ...]:
    values = (
        0x1111_1111_1111_1111,
        0x2222_2222_2222_2222,
        0x3333_3333_3333_3333,
        0x4444_4444_4444_4444,
        0x5555_5555_5555_5555,
    )
    for name, value in zip(
        ("LD-BUF", "LD-SZ", "LD-CUR", "LD-LEN", "LD-LINE"),
        values,
    ):
        _store(runtime, name, value)
    return values


def _loader_globals(runtime: MegaForthRuntime) -> tuple[int, ...]:
    return tuple(
        _variable(runtime, name)
        for name in ("LD-BUF", "LD-SZ", "LD-CUR", "LD-LEN", "LD-LINE")
    )


def _sector_allocation(source: bytes, sectors: int) -> bytes:
    capacity = sectors * SECTOR_SIZE
    if len(source) > capacity:
        raise ValueError("test source exceeds its allocated sectors")
    poison = b": LOAD-PADDING-RAN 999 ;\n"
    repeats = (capacity - len(source) + len(poison) - 1) // len(poison)
    return source + (poison * repeats)[: capacity - len(source)]


def test_load_slice_is_exact_and_publishes_complete_source_ledger() -> None:
    runtime = _load_mp64fs_load()

    assert len(SOURCE_LEDGER) == 55
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert tuple(_constant(runtime, name) for name, _value in CONSTANTS) == (
        tuple(value for _name, value in CONSTANTS)
    )
    assert all(_variable(runtime, name) == 0 for name in VARIABLES)

    sized_bodies = (
        ("_LD-STK", "_LD-SP", 88 * 16),
        ("_RP-PATH", "_RP-COMP", 128),
        ("_RP-COMP", "_RP-I", 24),
    )
    for name, following, size in sized_bodies:
        word = runtime.find(name)
        next_word = runtime.find(following)
        assert word is not None
        assert next_word is not None
        assert next_word.header_address - word.body_address == size

    noop = runtime.find("_LD-TXN-NOOP")
    assert noop is not None
    for name in (
        "_LD-TXN-COMMIT",
        "_LD-TXN-ROLLBACK",
        "_LD-TXN-AFTER-RELEASE",
    ):
        hook = runtime.find(name)
        assert hook is not None
        assert runtime.memory.read64(hook.body_address) == noop.xt

    assert sum(
        word.name == b"EVALUATE-CHECKED"
        for word in runtime.dictionary.words
    ) == 2
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_checked_source_walks_blank_crlf_and_cross_line_compiler_state() -> None:
    runtime = _load_mp64fs_load()
    source = (
        b"\r\n"
        b": WALKED\r\n"
        b"DUP 0= IF\r\n"
        b"DROP 41 ELSE\r\n"
        b"1+ THEN ;\r\n"
        b"0 WALKED\r\n"
        b"8 WALKED"
    )

    assert _source_evaluate_checked(
        runtime,
        source,
        name="CHECKED-SUCCESS-SOURCE",
    ) == (41, 9, 0)

    assert _variable(runtime, "EVAL-STATUS") == 0
    assert _variable(runtime, "EVAL-LINE") == 7
    assert _variable(runtime, "EVAL-COLUMN") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "EVAL-THROW") == 0
    assert _eval_token(runtime) == b""
    assert _execute(runtime, "WALKED", 0) == (41,)
    assert _execute(runtime, "WALKED", 8) == (9,)
    assert runtime.drain_uart_output() == b""


def test_checked_source_stops_at_first_undefined_token() -> None:
    runtime = _load_mp64fs_load()
    source = b"11 22 +\n5 missing-token 99\n77\n"

    assert _source_evaluate_checked(
        runtime,
        source,
        name="CHECKED-UNDEFINED-SOURCE",
    ) == (33, 5, 1)

    assert _variable(runtime, "EVAL-STATUS") == 1
    assert _variable(runtime, "EVAL-LINE") == 2
    assert _variable(runtime, "EVAL-COLUMN") == 2
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "EVAL-THROW") == 0
    assert _eval_token(runtime) == b"missing-token"
    assert runtime.drain_uart_output() == b"missing-token ? (not found)\n"


def test_checked_source_reports_overlong_and_caught_throw_statuses() -> None:
    runtime = _load_mp64fs_load()

    assert _source_evaluate_checked(
        runtime,
        b"x" * 256 + b"\n77\n",
        name="CHECKED-OVERLONG-SOURCE",
    ) == (2,)
    assert _variable(runtime, "EVAL-STATUS") == 2
    assert _variable(runtime, "EVAL-LINE") == 1
    assert _variable(runtime, "EVAL-COLUMN") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _eval_token(runtime) == b""
    assert runtime.drain_uart_output() == (
        b"EVALUATE input exceeds 255 bytes\n"
    )

    assert _source_evaluate_checked(
        runtime,
        b"11\n-77 THROW\n99\n",
        name="CHECKED-THROW-SOURCE",
    ) == (11, 5)
    assert _variable(runtime, "EVAL-STATUS") == 5
    assert _variable(runtime, "EVAL-LINE") == 2
    assert _variable(runtime, "EVAL-COLUMN") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "EVAL-THROW") == u64(-77)
    assert _eval_token(runtime) == b""
    assert runtime.drain_uart_output() == b""


def test_checked_source_propagates_nested_evaluator_depth_exhaustion() -> None:
    runtime = _load_mp64fs_load()
    nested_source = b"DESCEND-CHECKED 777"
    nested_word = runtime.define_created(
        "DESCEND-CHECKED-SOURCE",
        initial_body=nested_source,
    )
    runtime.evaluate(
        (
            f": DESCEND-CHECKED {nested_word.body_address} "
            f"{len(nested_source)} EVALUATE ;"
        ).encode("ascii")
    )

    assert _source_evaluate_checked(
        runtime,
        b"DESCEND-CHECKED 999\n888\n",
        name="CHECKED-DEPTH-SOURCE",
    ) == (3,)
    assert _variable(runtime, "EVAL-STATUS") == 3
    assert _variable(runtime, "EVAL-LINE") == 1
    assert _variable(runtime, "EVAL-COLUMN") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _eval_token(runtime) == b""
    assert runtime.drain_uart_output() == (
        b"EVALUATE depth limit exceeded\n"
    )


def test_checked_source_unfinished_state_uses_caller_rollback_and_reset() -> None:
    runtime = _load_mp64fs_load()
    source = (
        b": COMMITTED-BEFORE-UNFINISHED 66 ;\n"
        b": HALF-BUILT 123\n"
    )
    source_word = runtime.define_created(
        "CHECKED-UNFINISHED-SOURCE",
        initial_body=source,
    )
    saved_here = runtime.dictionary.here
    saved_latest = runtime.dictionary.latest

    assert _execute(
        runtime,
        "SOURCE-EVALUATE-CHECKED",
        source_word.body_address,
        len(source),
    ) == (4,)
    assert runtime.find("COMMITTED-BEFORE-UNFINISHED") is not None
    assert runtime.find("HALF-BUILT") is None
    assert _variable(runtime, "EVAL-STATUS") == 4
    assert _variable(runtime, "EVAL-LINE") == 2
    assert _variable(runtime, "EVAL-DEPTH") == 0

    assert _execute(
        runtime,
        "DICT-ROLLBACK",
        saved_here,
        saved_latest,
    ) == ()
    assert _execute(runtime, "EVALUATOR-RESET") == ()
    assert runtime.find("COMMITTED-BEFORE-UNFINISHED") is None
    assert runtime.find("HALF-BUILT") is None
    assert _variable(runtime, "EVAL-STATUS") == 4
    assert _execute(runtime, "EVALUATE-FINISH") == (0,)

    assert _source_evaluate_checked(
        runtime,
        b": AFTER-CHECKED-RESET 77 ;\n",
        name="CHECKED-AFTER-RESET-SOURCE",
    ) == (0,)
    assert _execute(runtime, "AFTER-CHECKED-RESET") == (77,)


def test_load_reads_two_extents_and_restores_nested_loader_state() -> None:
    image = _formatted_image(20)
    _write_entry(
        image,
        7,
        name=b"pkg\0",
        start=0,
        count=0,
        entry_type=8,
        parent=0xFF,
    )
    outer_source = (
        b"\\ p\n" * 125
        + b": OUTER-CROSS 42 ;\n"
        + b"LOAD inner.f\n"
    )
    assert len(outer_source) == 532
    outer_allocation = _sector_allocation(outer_source, 2)
    _write_entry(
        image,
        8,
        name=b"outer.f\0",
        start=14,
        count=1,
        used=len(outer_source),
        entry_type=3,
        parent=7,
        secondary_start=17,
        secondary_count=1,
    )
    inner_source = b": INNER-LOADED 73 ;\n"
    inner_allocation = _sector_allocation(inner_source, 1)
    _write_entry(
        image,
        9,
        name=b"inner.f\0",
        start=15,
        count=1,
        used=len(inner_source),
        entry_type=3,
        parent=7,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = outer_allocation[:SECTOR_SIZE]
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = outer_allocation[SECTOR_SIZE:]
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = inner_allocation

    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"LOAD pkg/outer.f", source_name="nested-two-extent-load")

    assert _execute(runtime, "OUTER-CROSS") == (42,)
    assert _execute(runtime, "INNER-LOADED") == (73,)
    assert runtime.find("LOAD-PADDING-RAN") is None
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 1313
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:6] == mount_before[:6]
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.storage.completion == completion_before + 3
    assert runtime.storage.image_bytes == media_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_nested_load_restores_parent_line_before_later_token_failure() -> None:
    outer_source = b"LOAD inner.f missing-after-nested-load\n"
    inner_source = b": INNER-FIRST 1 ;\n: INNER-SECOND 2 ;\n"
    image = _formatted_image(20)
    _write_entry(
        image,
        6,
        name=b"outer.f\0",
        start=14,
        count=1,
        used=len(outer_source),
        entry_type=3,
    )
    _write_entry(
        image,
        7,
        name=b"inner.f\0",
        start=15,
        count=1,
        used=len(inner_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(
        outer_source,
        1,
    )
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(
        inner_source,
        1,
    )
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    dictionary_before = (runtime.dictionary.here, runtime.dictionary.latest)

    runtime.evaluate(
        b"' LOAD CATCH outer.f",
        source_name="nested-load-parent-line-diagnostic",
    )

    assert runtime.main_context.data.snapshot() == (1,)
    runtime.main_context.data.clear()
    assert _variable(runtime, "EVAL-STATUS") == 1
    assert _variable(runtime, "EVAL-LINE") == 1
    assert _variable(runtime, "EVAL-COLUMN") == len(b"LOAD inner.f ")
    assert _eval_token(runtime) == b"missing-after-nested-load"
    assert runtime.find("INNER-FIRST") is None
    assert runtime.find("INNER-SECOND") is None
    assert (runtime.dictionary.here, runtime.dictionary.latest) == dictionary_before
    assert _variable(runtime, "_LD-SP") == 0
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.spinlocks.owner(2) is None


def test_load_clean_guards_misses_empty_and_missing_path_restore_state() -> None:
    runtime = _load_mp64fs_load()
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)

    runtime.evaluate(b"LOAD 41", source_name="load-without-filesystem")
    assert runtime.main_context.data.snapshot() == (41,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0

    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"empty.f\0",
        start=14,
        count=1,
        used=0,
        entry_type=3,
    )
    runtime.storage.attach(image)
    _mount(runtime)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]

    runtime.evaluate(b"LOAD missing.f", source_name="load-missing")
    assert runtime.drain_uart_output() == b" Not found: missing.f\r\n"
    runtime.evaluate(b"LOAD empty.f", source_name="load-empty")
    assert runtime.drain_uart_output() == b" Empty file\r\n"
    runtime.evaluate(
        b"LOAD missing-dir/ignored.f",
        source_name="load-missing-path",
    )
    assert runtime.drain_uart_output() == (
        b" Path component not found: missing-dir\r\n"
        b" Not found: missing-dir\r\n"
    )

    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_load_allocation_failure_restores_without_reading_or_hooks() -> None:
    source = b": MUST-NOT-LOAD 99 ;\n"
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"oom.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(source, 1)
    runtime = _load_mp64fs_fd_pool(image)
    runtime.evaluate(b": ALLOCATE DROP 0 -1 ;")
    _evaluate_mp64fs_load(runtime)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"LOAD oom.f", source_name="load-allocation-failure")

    assert runtime.drain_uart_output() == b" File buffer allocation failed\r\n"
    assert runtime.find("MUST-NOT-LOAD") is None
    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before


def test_resolver_failure_reports_then_loads_rejected_component() -> None:
    source = b": RESOLVER-WRONG-TARGET 91 ;\n"
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"plain\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=1,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(source, 1)
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(
        b"LOAD plain/ignored.f",
        source_name="load-resolver-defect",
    )

    assert runtime.drain_uart_output() == (
        b" Path component not found: plain\r\n"
    )
    assert _execute(runtime, "RESOLVER-WRONG-TARGET") == (91,)
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 13
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:6] == mount_before[:6]
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.storage.completion == completion_before + 1
    assert runtime.storage.image_bytes == media_before


def test_load_throw_rolls_back_dictionary_then_allows_reuse() -> None:
    bad_source = (
        b": BEFORE-LOAD-THROW 17 ;\r\n"
        b"-77 THROW\r\n"
        b": AFTER-LOAD-THROW 99 ;\r\n"
    )
    good_source = b": AFTER-LOAD-RECOVERY 55 ;\r\n"
    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"bad.f\0",
        start=14,
        count=1,
        used=len(bad_source),
        entry_type=3,
    )
    _write_entry(
        image,
        7,
        name=b"good.f\0",
        start=15,
        count=1,
        used=len(good_source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(
        bad_source,
        1,
    )
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(
        good_source,
        1,
    )
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    runtime.evaluate(b"' LOAD CATCH bad.f", source_name="caught-load-throw")

    assert runtime.main_context.data.snapshot() == (u64(-77),)
    runtime.main_context.data.clear()
    assert runtime.find("BEFORE-LOAD-THROW") is None
    assert runtime.find("AFTER-LOAD-THROW") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    assert runtime.drain_uart_output() == b""

    runtime.evaluate(b"LOAD good.f", source_name="load-after-caught-throw")

    assert _execute(runtime, "AFTER-LOAD-RECOVERY") == (55,)
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 2313
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:6] == mount_before[:6]
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.storage.completion == completion_before + 2
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b""


def test_load_stops_at_undefined_status_and_rolls_back_definitions() -> None:
    source = (
        b": BEFORE-LOAD-UNDEFINED 1 ;\n"
        b"missing-load-token\n"
        b": AFTER-LOAD-UNDEFINED 2 ;\n"
    )
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"unchecked.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(source, 1)
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion
    media_before = runtime.storage.image_bytes
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    runtime.evaluate(
        b"' LOAD CATCH unchecked.f",
        source_name="checked-load-status",
    )

    assert runtime.main_context.data.snapshot() == (1,)
    runtime.main_context.data.clear()
    assert runtime.find("BEFORE-LOAD-UNDEFINED") is None
    assert runtime.find("AFTER-LOAD-UNDEFINED") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert _variable(runtime, "EVAL-STATUS") == 1
    assert _variable(runtime, "EVAL-LINE") == 2
    assert _eval_token(runtime) == b"missing-load-token"
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    assert runtime.storage.completion == completion_before + 1
    assert runtime.storage.image_bytes == media_before
    assert runtime.drain_uart_output() == b"missing-load-token ? (not found)\n"


def test_load_rejects_unfinished_compiler_state_and_resets_evaluator() -> None:
    source = b": LOAD-LEFT-OPEN 123\n"
    image = _formatted_image()
    _write_entry(
        image,
        6,
        name=b"unfinished.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(source, 1)
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion
    here_before = runtime.dictionary.here
    latest_before = runtime.dictionary.latest

    runtime.evaluate(
        b"' LOAD CATCH unfinished.f",
        source_name="unfinished-checked-load",
    )

    assert runtime.main_context.data.snapshot() == (4,)
    runtime.main_context.data.clear()
    assert runtime.find("LOAD-LEFT-OPEN") is None
    assert runtime.dictionary.here == here_before
    assert runtime.dictionary.latest == latest_before
    assert _variable(runtime, "EVAL-STATUS") == 4
    assert _variable(runtime, "EVAL-LINE") == 1
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _loader_globals(runtime) == loader_before
    assert runtime.storage.completion == completion_before + 1
    assert runtime.drain_uart_output() == b""
    assert _execute(runtime, "EVALUATE-FINISH") == (0,)


def test_second_extent_read_failure_runs_guarded_cleanup() -> None:
    image = _formatted_image(20)
    _write_entry(
        image,
        7,
        name=b"pkg\0",
        start=0,
        count=0,
        entry_type=8,
        parent=0xFF,
    )
    source = b"\\ p\n" * 125 + b": MUST-NOT-REACH 42 ;\nLOAD never.f\n"
    assert len(source) > SECTOR_SIZE
    allocation = _sector_allocation(source, 2)
    _write_entry(
        image,
        8,
        name=b"split.f\0",
        start=14,
        count=1,
        used=len(source),
        entry_type=3,
        parent=7,
        secondary_start=17,
        secondary_count=1,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = allocation[:SECTOR_SIZE]
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = allocation[SECTOR_SIZE:]

    class SwapOnSecondLoadRead(HostedStorageService):
        def __init__(self, payload: bytes | bytearray) -> None:
            super().__init__(payload)
            self.armed = False
            self.read_acceptances = 0

        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            if not self.armed or command != STORAGE_CMD_READ:
                return
            assert expected_generation == self.media_generation
            self.read_acceptances += 1
            if self.read_acceptances == 2:
                self.attach(self.image_bytes)

    storage = SwapOnSecondLoadRead(image)
    runtime = _load_mp64fs_load_service(storage)
    _mount(runtime)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    mount_before = _mount_snapshot(runtime)
    media_before = storage.image_bytes
    completion_before = storage.completion
    expected_ior = _execute(
        runtime,
        "IOR-FROM-BLOCK-RESULT",
        STORAGE_RESULT_MEDIA_REMOVED,
    )[0]
    storage.armed = True

    runtime.evaluate(
        b"' LOAD CATCH pkg/split.f",
        source_name="load-second-extent-stale",
    )

    assert runtime.main_context.data.snapshot() == (expected_ior,)
    runtime.main_context.data.clear()
    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "CWD") == 0xFF
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_MEDIA_REMOVED,
        0,
        expected_ior,
    )
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:2] == mount_before[:2]
    assert mount_after[3:6] == mount_before[3:6]
    assert storage.read_acceptances == 2
    assert storage.completion == completion_before + 2
    assert storage.image_bytes == media_before
    assert runtime.find("MUST-NOT-REACH") is None
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None
