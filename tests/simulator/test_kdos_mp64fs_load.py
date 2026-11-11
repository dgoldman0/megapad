"""Unchanged-source harness for the MP64FS source loader."""

from __future__ import annotations

import hashlib
import struct
from pathlib import Path

from shared.cells import TRUE, u64
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CMD_READ,
    STORAGE_RESULT_MEDIA_REMOVED,
)
from simulator.runtime import ExecutionContext, MegaForthRuntime
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
    / "kdos-mp64fs-load-5745-6486.f"
)

FIRST_LINE = 5745
LAST_LINE = 6486
SLICE_BYTES = 30_015
SLICE_SHA256 = (
    "bd01a2e2ab9083e97f986be1b42a3d197091121151b22b28620e28627bdeae19"
)
SLICE_GIT_BLOB = "9f4195a6c9b62030b599a34e51e329b4294dd8f2"

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
    ("CONSTANT", b"/MODULE-SOURCE"),
    (":", b"MS.RESOLVE"),
    (":", b"MS.READ"),
    (":", b"MS.DIR@"),
    (":", b"MS.DIR!"),
    (":", b"MS.CONTEXT"),
    ("CONSTANT", b"/MODULE-FILE"),
    (":", b"MF.ID"),
    (":", b"MF.USED"),
    (":", b"MF.RESERVE"),
    (":", b"MF.HANDLE"),
    ("CONSTANT", b"MS-E-NOT-FOUND"),
    ("CONSTANT", b"MS-E-NO-FILESYSTEM"),
    ("VARIABLE", b"_FMS-FILE"),
    (":", b"_FS-MS-RESOLVE"),
    (":", b"_FS-MS-READ"),
    (":", b"_FS-MS-DIR@"),
    (":", b"_FS-MS-DIR!"),
    ("CREATE", b"FS-MODULE-SOURCE"),
    ("VARIABLE", b"MODULE-SOURCE"),
    (":", b"MODULE-SOURCE!"),
    ("CREATE", b"_LD-FILE"),
    (":", b"_LD-PATH"),
    ("VARIABLE", b"MODULE-TOKENS"),
    ("CREATE", b"_LD-TNAME"),
    ("VARIABLE", b"_LD-TNAME-U"),
    ("VARIABLE", b"_LD-NAME-U"),
    ("VARIABLE", b"_LD-TOKENS-CHOSEN"),
    (":", b"_LD-NAME-RESOLVE"),
    (":", b"_LD-RESOLVE"),
    (":", b"_LD-READ"),
    (":", b"_LD-FOUND?"),
    ("CREATE", b"_LD-NAMES"),
    (":", b"_LD-NAME-SLOT"),
    (":", b"_LD-NAME-KEEP"),
    (":", b"_LD-NAME"),
    ("CREATE", b"_LD-FROM-STK"),
    ("VARIABLE", b"_LD-FROM-SP"),
    (":", b"_LD-FROM-SAVE"),
    (":", b"_LD-FROM-END"),
    ("CONSTANT", b"EVAL-S-OK"),
    ("CONSTANT", b"EVAL-S-UNDEFINED"),
    ("CONSTANT", b"EVAL-S-LINE-TOO-LONG"),
    ("CONSTANT", b"EVAL-S-DEPTH"),
    ("CONSTANT", b"EVAL-S-UNFINISHED"),
    ("CONSTANT", b"EVAL-S-THROW"),
    (":", b"_EVALUATION-GUARDED"),
    (":", b"_EVALUATE-GUARDED"),
    (":", b"EVALUATE"),
    (":", b"EVALUATE-CHECKED"),
    (":", b"_LINE-LEN"),
    (":", b"_LINE-TEXT"),
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
    ("CONSTANT", b"EVAL-S-TOKENS"),
    ("CONSTANT", b"EVAL-S-MISMATCH"),
    ("CONSTANT", b"/TOKEN-HEADER"),
    (":", b"TKF.HEADER"),
    (":", b"TKF.SLOTS"),
    (":", b"TKF.RECORDS"),
    (":", b"TKF.SOURCE"),
    (":", b"TKF.FLAGS"),
    (":", b"TKF.BOOT-ID"),
    ("CONSTANT", b"TKF-F-SOURCE-BESIDE"),
    (":", b"_TEXT-ID"),
    (":", b"TKF.TEXT-ID"),
    (":", b"TKF.ID"),
    ("CREATE", b"_TKF-MAGIC"),
    (":", b"TOKEN-FILE?"),
    ("VARIABLE", b"_TKF-A"),
    ("VARIABLE", b"_TKF-U"),
    (":", b"_TKF-VALID?"),
    (":", b"_TKF-ARGS"),
    (":", b"TOKENS-CURRENT?"),
    (":", b"TOKEN-EVALUATE-CHECKED"),
    (":", b"_LD-TOKENS-NAMED"),
    ("CREATE", b"_LD-SRC-FILE"),
    ("VARIABLE", b"_LD-SRC-BUF"),
    ("VARIABLE", b"_LD-SRC-U"),
    (":", b"_LD-SIBLING"),
    (":", b"_LD-SRC-READ"),
    (":", b"_LD-SOURCE-BESIDE"),
    (":", b"_LD-TOKENS?"),
    (":", b"_LD-WALK-TOKENS"),
    (":", b"_LD-TOKENS-MODULE"),
    (":", b"_LD-RELEASE"),
    (":", b"_LD-FAIL"),
    (":", b"_LD-GUARDED"),
    (":", b"_LD-WALK-GUARDED"),
    (":", b"_LD-READ-WALK"),
    (":", b"_LOAD-BODY"),
    (":", b"LOAD"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)

CONSTANTS = (
    ("_LD-FRAME", 88),
    ("_LD-MAXLVL", 16),
    ("/MODULE-SOURCE", 40),
    ("/MODULE-FILE", 40),
    ("MS-E-NOT-FOUND", u64(-4110)),
    ("MS-E-NO-FILESYSTEM", u64(-4111)),
    ("EVAL-S-OK", 0),
    ("EVAL-S-UNDEFINED", 1),
    ("EVAL-S-LINE-TOO-LONG", 2),
    ("EVAL-S-DEPTH", 3),
    ("EVAL-S-UNFINISHED", 4),
    ("EVAL-S-THROW", 5),
    ("EVAL-S-TOKENS", 6),
    ("EVAL-S-MISMATCH", 7),
    ("/TOKEN-HEADER", 64),
    ("TKF-F-SOURCE-BESIDE", 1),
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
    "_FMS-FILE",
    "_LD-FROM-SP",
    "_SEC-CUR",
    "_SEC-REM",
    "_SEC-RAW-LEN",
    "_SEC-EVAL-LEN",
    "_SEC-LINE",
    "MODULE-TOKENS",
    "_LD-TNAME-U",
    "_LD-NAME-U",
    "_LD-TOKENS-CHOSEN",
    "_TKF-A",
    "_TKF-U",
    "_LD-SRC-BUF",
    "_LD-SRC-U",
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


MS_E_NOT_FOUND = u64(-4110)
MS_E_NO_FILESYSTEM = u64(-4111)
MODULE_FILE_BYTES = 40


class _HostSource:
    """A module source the test serves from a table of paths.

    It keeps one current directory and records every call the loader makes.
    Each DIR@ hands out a new token, and DIR! accepts a token once, so a
    token that comes back twice, or never, fails the test.
    """

    def __init__(
        self,
        runtime: MegaForthRuntime,
        name: str,
        files: dict[bytes, bytes],
        *,
        identities: dict[bytes, tuple[int, int]] | None = None,
        reserve_slack: int = 0,
    ) -> None:
        self.runtime = runtime
        self.files = dict(files)
        self.identities = dict(identities or {})
        self.reserve_slack = reserve_slack
        self.directory: tuple[bytes, ...] = ()
        self.calls: list[tuple[object, ...]] = []
        self.live_tokens: dict[int, tuple[bytes, ...]] = {}
        self.handles: dict[int, bytes] = {}
        self.resolve_ior = 0
        self.read_ior = 0
        self.context = 0x00C0_FFEE
        self._next = 0x5000
        words = tuple(
            runtime.define_primitive(f"{name}-{suffix}", callback)
            for suffix, callback in (
                ("RESOLVE", self._resolve),
                ("READ", self._read),
                ("DIR@", self._directory_fetch),
                ("DIR!", self._directory_store),
            )
        )
        self.operations = tuple(word.xt for word in words)
        record = b"".join(
            value.to_bytes(8, "little")
            for value in (*self.operations, self.context)
        )
        self.address = runtime.define_created(
            name,
            initial_body=record,
        ).body_address

    def _fresh(self) -> int:
        self._next += 1
        return self._next

    def reads(self) -> list[bytes]:
        return [call[1] for call in self.calls if call[0] == "read"]

    def resolves(self) -> list[bytes]:
        return [call[1] for call in self.calls if call[0] == "resolve"]

    def _resolve(self, context: ExecutionContext) -> None:
        source = context.data.pop()
        record = context.data.pop()
        length = context.data.pop()
        address = context.data.pop()
        assert source == self.address
        memory = self.runtime.memory
        path = memory.read_bytes(address, length)
        self.calls.append(
            ("resolve", path, memory.read_bytes(record, MODULE_FILE_BYTES))
        )
        if self.resolve_ior:
            context.data.push(self.resolve_ior)
            return
        directory = () if path.startswith(b"/") else self.directory
        *folders, leaf = path.lstrip(b"/").split(b"/")
        for folder in folders:
            directory = (
                directory[:-1] if folder == b".." else (*directory, folder)
            )
        # A failed resolve may leave any directory current.
        self.directory = directory
        full = b"/".join((*directory, leaf))
        text = self.files.get(full)
        if text is None:
            context.data.push(MS_E_NOT_FOUND)
            return
        handle = self._fresh()
        self.handles[handle] = full
        low, high = self.identities.get(full, (0, 0))
        for offset, value in (
            (0, low),
            (8, high),
            (16, len(text)),
            (24, len(text) + self.reserve_slack),
            (32, handle),
        ):
            memory.write64(record + offset, value)
        context.data.push(0)

    def _read(self, context: ExecutionContext) -> None:
        source = context.data.pop()
        record = context.data.pop()
        address = context.data.pop()
        assert source == self.address
        memory = self.runtime.memory
        full = self.handles[memory.read64(record + 32)]
        self.calls.append(("read", full, address))
        if self.read_ior:
            context.data.push(self.read_ior)
            return
        # READ may use all of the span it asked for.
        memory.write_bytes(
            address,
            self.files[full] + b"\xEE" * self.reserve_slack,
        )
        context.data.push(0)

    def _directory_fetch(self, context: ExecutionContext) -> None:
        assert context.data.pop() == self.address
        token = self._fresh()
        self.live_tokens[token] = self.directory
        self.calls.append(("dir@", token))
        context.data.push(token)

    def _directory_store(self, context: ExecutionContext) -> None:
        assert context.data.pop() == self.address
        token = context.data.pop()
        self.directory = self.live_tokens.pop(token)
        self.calls.append(("dir!", token))


def _select_source(runtime: MegaForthRuntime, source: _HostSource) -> None:
    assert _execute(runtime, "MODULE-SOURCE!", source.address) == ()
    assert _variable(runtime, "MODULE-SOURCE") == source.address


def test_load_slice_is_exact_and_publishes_complete_source_ledger() -> None:
    runtime = _load_mp64fs_load()

    assert len(SOURCE_LEDGER) == 132
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert tuple(_constant(runtime, name) for name, _value in CONSTANTS) == (
        tuple(value for _name, value in CONSTANTS)
    )
    assert all(_variable(runtime, name) == 0 for name in VARIABLES)

    sized_bodies = (
        ("_LD-STK", "_LD-SP", 88 * 16),
        ("_RP-PATH", "_RP-COMP", 128),
        ("_RP-COMP", "_RP-I", 24),
        ("FS-MODULE-SOURCE", "MODULE-SOURCE", 40),
        ("_LD-FILE", "_LD-PATH", 40),
        ("_LD-NAMES", "_LD-NAME-SLOT", 128 * 16),
        ("_LD-FROM-STK", "_LD-FROM-SP", 16 * 16),
        ("_LD-SRC-FILE", "_LD-SRC-BUF", 40),
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

    builtin = runtime.find("FS-MODULE-SOURCE")
    assert builtin is not None
    assert tuple(
        runtime.memory.read64(builtin.body_address + offset)
        for offset in range(0, 40, 8)
    ) == (
        *(
            runtime.find(name).xt  # type: ignore[union-attr]
            for name in (
                "_FS-MS-RESOLVE",
                "_FS-MS-READ",
                "_FS-MS-DIR@",
                "_FS-MS-DIR!",
            )
        ),
        0,
    )
    assert _variable(runtime, "MODULE-SOURCE") == builtin.body_address
    for name, offset in (
        ("MS.RESOLVE", 0),
        ("MS.READ", 8),
        ("MS.DIR@", 16),
        ("MS.DIR!", 24),
        ("MS.CONTEXT", 32),
        ("MF.ID", 0),
        ("MF.USED", 16),
        ("MF.RESERVE", 24),
        ("MF.HANDLE", 32),
    ):
        assert _execute(runtime, name, 0x1000) == (0x1000 + offset,)

    # KDOS shadows both BIOS evaluator words.
    for shadowed in (b"EVALUATE", b"EVALUATE-CHECKED"):
        assert sum(
            word.name == shadowed for word in runtime.dictionary.words
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


def test_line_words_state_what_a_physical_line_is() -> None:
    """_LINE-LEN is the bytes before a line's LF, or all that remains;
    _LINE-TEXT is that length without one terminal CR."""

    runtime = _load_mp64fs_load()
    text = b"ab\ncd\r\n\nlast"
    base = runtime.define_created("LINE-WORD-SOURCE", initial_body=text).body_address

    def line_len(offset: int, remaining: int) -> tuple[int, ...]:
        return _execute(runtime, "_LINE-LEN", base + offset, remaining)

    def line_text(offset: int, length: int) -> tuple[int, ...]:
        return _execute(runtime, "_LINE-TEXT", base + offset, length)

    assert line_len(0, len(text)) == (2,)
    assert line_len(3, len(text) - 3) == (3,)
    assert line_len(7, len(text) - 7) == (0,)
    # No LF follows: the line is all that remains.
    assert line_len(8, 4) == (4,)
    # The count bounds the line, an LF past it or not.
    assert line_len(0, 2) == (2,)
    assert line_len(0, 0) == (0,)
    assert line_len(0, (-5) % 2**64) == (0,)

    assert line_text(3, 3) == (2,)
    assert line_text(0, 2) == (2,)
    assert line_text(0, 0) == (0,)
    assert line_text(5, 1) == (0,)
    assert runtime.main_context.data.snapshot() == ()


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
    # The THROW carries the failure; the loader leaves no status set.
    assert _variable(runtime, "EVAL-STATUS") == 0
    assert _variable(runtime, "EVAL-LINE") == 1
    assert _variable(runtime, "EVAL-COLUMN") == len(b"LOAD inner.f ")
    assert _eval_token(runtime) == b"missing-after-nested-load"
    assert runtime.find("INNER-FIRST") is None
    assert runtime.find("INNER-SECOND") is None
    assert (runtime.dictionary.here, runtime.dictionary.latest) == dictionary_before
    assert _variable(runtime, "_LD-SP") == 0
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.spinlocks.owner(2) is None


def test_a_caught_load_failure_lets_the_loading_file_go_on() -> None:
    """A word that catches a failed load while its own file is loading finds
    the evaluator as it was: its line continues, the next load runs, and its
    file loads to the end."""

    outer_source = (
        b': TRY-BAD S" LOAD bad.f" EVALUATE ;\n'
        b": CATCH-BAD ['] TRY-BAD CATCH ;\n"
        b"CATCH-BAD 7 +\n"
        b"LOAD good.f\n"
        b": OUTER-DONE 99 ;\n"
    )
    bad_source = b": BAD-FIRST 1 ;\n-77 THROW\n: BAD-TAIL 2 ;\n"
    good_source = b": GOOD-WORD 4242 ;\n"
    image = _formatted_image(20)
    for slot, (name, source) in enumerate(
        (
            (b"outer.f\0", outer_source),
            (b"bad.f\0", bad_source),
            (b"good.f\0", good_source),
        )
    ):
        _write_entry(
            image,
            6 + slot,
            name=name,
            start=14 + slot,
            count=1,
            used=len(source),
            entry_type=3,
        )
        image[(14 + slot) * SECTOR_SIZE : (15 + slot) * SECTOR_SIZE] = (
            _sector_allocation(source, 1)
        )
    runtime = _load_mp64fs_load(image)
    _mount(runtime)

    runtime.evaluate(b"LOAD outer.f", source_name="caught-load-inside-a-load")

    # The caught code, with the rest of the catching line applied to it.
    assert runtime.main_context.data.snapshot() == ((-77 + 7) % 2**64,)
    runtime.main_context.data.clear()
    assert _variable(runtime, "EVAL-STATUS") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert runtime.find("BAD-FIRST") is None
    assert runtime.find("BAD-TAIL") is None
    assert _execute(runtime, "GOOD-WORD") == (4242,)
    assert _execute(runtime, "OUTER-DONE") == (99,)
    assert _variable(runtime, "_LD-SP") == 0
    assert runtime.main_context.returns.snapshot() == ()


def test_load_clean_guards_misses_empty_and_missing_path_restore_state() -> None:
    runtime = _load_mp64fs_load()
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)

    runtime.evaluate(b"LOAD 41 42", source_name="load-without-filesystem")
    assert runtime.main_context.data.snapshot() == (42,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
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
        b" Not found: missing-dir/ignored.f\r\n"
    )

    assert _loader_globals(runtime) == loader_before
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
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


def test_a_catch_across_plain_evaluate_leaves_the_evaluator_ready_to_load() -> None:
    bad_source = b": BEFORE-EVAL-THROW 17 ;\r\n-77 THROW\r\n"
    good_source = b": AFTER-EVAL-RECOVERY 55 ;\r\n"
    image = _formatted_image(16)
    _write_entry(image, 6, name=b"bad.f\0", start=14, count=1,
                 used=len(bad_source), entry_type=3)
    _write_entry(image, 7, name=b"good.f\0", start=15, count=1,
                 used=len(good_source), entry_type=3)
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = _sector_allocation(bad_source, 1)
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = _sector_allocation(good_source, 1)
    runtime = _load_mp64fs_load(image)
    _mount(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]

    # A word of its own that reaches the parsing word through EVALUATE, and
    # an ordinary CATCH around it: nothing here knows about the evaluator.
    runtime.evaluate(
        b': TRY-BAD S" LOAD bad.f" EVALUATE ; '
        b": CATCH-BAD ['] TRY-BAD CATCH ;",
        source_name="catch-across-evaluate",
    )
    assert _execute(runtime, "CATCH-BAD") == (u64(-77),)

    # KDOS's EVALUATE gave the evaluator its depth back as the exception
    # passed, so the BIOS does not take the next load for a nested string
    # after a failure and skip its lines.
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "_LD-SP") == 0
    assert runtime.find("BEFORE-EVAL-THROW") is None
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)

    runtime.evaluate(b"LOAD good.f", source_name="load-after-evaluate-catch")

    assert _execute(runtime, "AFTER-EVAL-RECOVERY") == (55,)
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert runtime.drain_uart_output() == b""


def test_evaluate_returns_each_nested_level_to_its_own_entry_depth() -> None:
    runtime = _load_mp64fs_load()
    runtime.evaluate(
        b"VARIABLE EV-SEEN  VARIABLE EV-RAN "
        b": EV-INNER -5 THROW ; "
        b': EV-MID S" EV-INNER 1 EV-RAN !" EVALUATE ; '
        b': EV-OUTER S" EV-MID 2 EV-RAN !" EVALUATE ; '
        b": EV-CATCH-MID ['] EV-MID CATCH EVAL-DEPTH @ EV-SEEN ! ; "
        b': EV-NESTED S" EV-CATCH-MID 7" EVALUATE ;',
        source_name="nested-evaluate",
    )

    # Caught outside both strings: both frames are gone, and neither string's
    # tail ran.
    assert _execute(runtime, "CATCH", runtime.find("EV-OUTER").xt) == (u64(-5),)
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "EV-RAN") == 0

    # Caught inside a string: the catcher finds the depth of the string it
    # runs in, and that string goes on.
    assert _execute(runtime, "EV-NESTED") == (u64(-5), 7)
    assert _variable(runtime, "EV-SEEN") == 1
    assert _variable(runtime, "EVAL-DEPTH") == 0


def test_evaluate_without_an_exception_is_the_bios_word() -> None:
    runtime = _load_mp64fs_load()
    runtime.evaluate(
        b': EV-SUM S" 1 2 + 40" EVALUATE ; '
        b': EV-UNKNOWN S" 5 EV-NO-SUCH-WORD 6" EVALUATE ;',
        source_name="plain-evaluate",
    )

    assert _execute(runtime, "EV-SUM") == (3, 40)
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _variable(runtime, "EVAL-STATUS") == 0

    # An undefined token is a status, not an exception, and stays one.
    assert _execute(runtime, "EV-UNKNOWN") == (5,)
    assert _variable(runtime, "EVAL-STATUS") == 1
    assert _eval_token(runtime) == b"EV-NO-SUCH-WORD"
    assert _variable(runtime, "EVAL-DEPTH") == 0


def test_the_bios_evaluate_itself_still_leaves_its_frame_under_a_throw() -> None:
    runtime = _load_mp64fs_load()
    guarded = runtime.find("_EVALUATE-GUARDED")
    assert guarded is not None
    bios_evaluate = runtime.dictionary.resolve(
        guarded.implementation.operations[0].value
    )
    shadow = runtime.find("EVALUATE")
    assert shadow is not None and shadow is not bios_evaluate
    runtime.evaluate(
        f': EV-RAW S" -9 THROW" {bios_evaluate.xt} EXECUTE ;'.encode("ascii"),
        source_name="raw-evaluate",
    )

    # A word that reaches the BIOS entry, as one compiled before the shadow
    # would, is outside the wrapper: its catcher must unwind for itself.
    assert _execute(runtime, "CATCH", runtime.find("EV-RAW").xt) == (u64(-9),)
    assert _variable(runtime, "EVAL-DEPTH") == 1
    assert _execute(runtime, "EVALUATOR-UNWIND", 0) == ()
    assert _variable(runtime, "EVAL-DEPTH") == 0


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
    # The THROW carries the failure; the loader leaves no status set.
    assert _variable(runtime, "EVAL-STATUS") == 0
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
    # The THROW carries the failure; the loader leaves no status set.
    assert _variable(runtime, "EVAL-STATUS") == 0
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


def test_load_reads_nested_relative_paths_through_the_selected_source() -> None:
    runtime = _load_mp64fs_load()
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        {
            b"lib/outer.f": (
                b": HOST-OUTER 11 ;\n"
                b"LOAD inner.f\n"
                b"LOAD ../top.f\n"
                b": HOST-AFTER 14 ;\n"
            ),
            b"lib/inner.f": b": HOST-INNER 12 ;\n",
            b"top.f": b": HOST-TOP 13 ;\n",
        },
        reserve_slack=24,
    )
    _select_source(runtime, source)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    completion_before = runtime.storage.completion

    runtime.evaluate(b"LOAD lib/outer.f", source_name="load-from-host-source")

    assert _execute(runtime, "HOST-OUTER") == (11,)
    assert _execute(runtime, "HOST-INNER") == (12,)
    assert _execute(runtime, "HOST-TOP") == (13,)
    assert _execute(runtime, "HOST-AFTER") == (14,)
    kinds = [call[0] for call in source.calls]
    assert kinds == [
        "dir@", "resolve", "read",
        "dir@", "resolve", "read", "dir!",
        "dir@", "resolve", "read", "dir!",
        "dir!",
    ]
    # Each nested load resolves from its caller's directory, and every
    # resolve starts from a cleared file record.
    assert [call[1:] for call in source.calls if call[0] == "resolve"] == [
        (b"lib/outer.f", bytes(MODULE_FILE_BYTES)),
        (b"inner.f", bytes(MODULE_FILE_BYTES)),
        (b"../top.f", bytes(MODULE_FILE_BYTES)),
    ]
    assert source.reads() == [b"lib/outer.f", b"lib/inner.f", b"top.f"]
    tokens = [call[1] for call in source.calls if call[0] == "dir@"]
    assert [call[1] for call in source.calls if call[0] == "dir!"] == [
        tokens[1],
        tokens[2],
        tokens[0],
    ]
    assert source.live_tokens == {}
    assert source.directory == ()
    assert _variable(runtime, "MODULE-SOURCE") == source.address
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 131313
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    # KDOS's own filesystem took no part.
    assert _variable(runtime, "CWD") == 0xFF
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == completion_before
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_load_reports_a_sources_misses_and_throws_its_other_failures() -> None:
    runtime = _load_mp64fs_load()
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        {b"lib/empty.f": b"", b"lib/real.f": b": HOST-REAL 5 ;\n"},
    )
    _select_source(runtime, source)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]

    def settled() -> None:
        assert source.live_tokens == {}
        assert source.directory == ()
        assert source.reads() == []
        assert _variable(runtime, "MODULE-SOURCE") == source.address
        assert _variable(runtime, "LOAD-HOOK-TRACE") == 0
        assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
        assert _variable(runtime, "_LD-SP") == 0
        assert _variable(runtime, "_LD-FROM-SP") == 0
        assert _loader_globals(runtime) == loader_before
        assert runtime.main_context.returns.snapshot() == ()

    # The source leaves lib current when it finds no file there.
    runtime.evaluate(b"LOAD lib/missing.f 7", source_name="host-miss")
    assert runtime.main_context.data.snapshot() == (7,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" Not found: lib/missing.f\r\n"
    settled()

    runtime.evaluate(b"LOAD lib/empty.f", source_name="host-empty")
    assert runtime.drain_uart_output() == b" Empty file\r\n"
    settled()

    source.resolve_ior = MS_E_NO_FILESYSTEM
    runtime.evaluate(b"LOAD lib/real.f", source_name="host-no-filesystem")
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    settled()

    source.resolve_ior = u64(-37)
    runtime.evaluate(b"' LOAD CATCH lib/real.f", source_name="host-other")
    assert runtime.main_context.data.snapshot() == (u64(-37),)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b""
    settled()
    assert runtime.find("HOST-REAL") is None
    assert source.resolves() == [b"lib/missing.f"] + [b"lib/empty.f"] + [
        b"lib/real.f"
    ] * 2

    source.resolve_ior = 0
    runtime.evaluate(b"LOAD lib/real.f", source_name="host-after-failures")
    assert _execute(runtime, "HOST-REAL") == (5,)
    assert source.live_tokens == {}
    assert source.directory == ()


def test_a_sources_read_failure_takes_the_guarded_cleanup() -> None:
    runtime = _load_mp64fs_load()
    source = _HostSource(
        runtime,
        "HOST-SOURCE",
        {b"lib/unread.f": b": HOST-UNREAD 5 ;\n"},
    )
    _select_source(runtime, source)
    _install_loader_trace(runtime)
    loader_before = _seed_loader_globals(runtime)
    heap_before = _execute(runtime, "HEAP-FREE-BYTES")[0]
    dictionary_before = (runtime.dictionary.here, runtime.dictionary.latest)
    source.read_ior = u64(-99)

    runtime.evaluate(b"' LOAD CATCH lib/unread.f", source_name="host-read")

    assert runtime.main_context.data.snapshot() == (u64(-99),)
    runtime.main_context.data.clear()
    assert source.reads() == [b"lib/unread.f"]
    assert runtime.find("HOST-UNREAD") is None
    assert (runtime.dictionary.here, runtime.dictionary.latest) == (
        dictionary_before
    )
    assert source.live_tokens == {}
    assert source.directory == ()
    assert _variable(runtime, "MODULE-SOURCE") == source.address
    assert _variable(runtime, "LOAD-HOOK-TRACE") == 23
    assert _execute(runtime, "HEAP-FREE-BYTES") == (heap_before,)
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert _variable(runtime, "EVAL-DEPTH") == 0
    assert _loader_globals(runtime) == loader_before
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.returns.snapshot() == ()


def test_a_load_keeps_the_source_it_selects_unless_it_fails() -> None:
    runtime = _load_mp64fs_load()
    first = _HostSource(
        runtime,
        "HOST-FIRST",
        {
            b"boot/switch.f": (
                b": BEFORE-SWITCH 1 ;\n"
                b"HOST-SECOND MODULE-SOURCE!\n"
                b"LOAD next.f\n"
            ),
            b"boot/fail.f": b"HOST-SECOND MODULE-SOURCE!\n-77 THROW\n",
        },
    )
    second = _HostSource(
        runtime,
        "HOST-SECOND",
        {b"next.f": b": FROM-SECOND 2 ;\n"},
    )
    _select_source(runtime, first)

    runtime.evaluate(b"' LOAD CATCH boot/fail.f", source_name="failed-switch")

    assert runtime.main_context.data.snapshot() == (u64(-77),)
    runtime.main_context.data.clear()
    assert _variable(runtime, "MODULE-SOURCE") == first.address
    assert first.live_tokens == {}
    assert first.directory == ()
    assert second.calls == []
    assert _variable(runtime, "_LD-FROM-SP") == 0

    runtime.evaluate(b"LOAD boot/switch.f", source_name="kept-switch")

    assert _execute(runtime, "BEFORE-SWITCH") == (1,)
    assert _execute(runtime, "FROM-SECOND") == (2,)
    assert _variable(runtime, "MODULE-SOURCE") == second.address
    # The first source has its directory back although it is not selected.
    assert first.live_tokens == {}
    assert first.directory == ()
    assert [call[0] for call in second.calls] == [
        "dir@",
        "resolve",
        "read",
        "dir!",
    ]
    assert second.live_tokens == {}
    assert _variable(runtime, "_LD-SP") == 0
    assert _variable(runtime, "_LD-FROM-SP") == 0
    assert runtime.drain_uart_output() == b""


def test_builtin_source_resolves_the_path_it_is_given() -> None:
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
    text = b"\\ p\n" * 125 + b": BUILTIN-READ 42 ;\n"
    assert len(text) > SECTOR_SIZE
    allocation = _sector_allocation(text, 2)
    offset = _write_entry(
        image,
        8,
        name=b"mod.f\0",
        start=14,
        count=1,
        used=len(text),
        entry_type=3,
        parent=7,
        secondary_start=17,
        secondary_count=1,
    )
    struct.pack_into("<II", image, offset + 36, 0x0102_0304, 0xA1B2_C3D4)
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = allocation[:SECTOR_SIZE]
    image[17 * SECTOR_SIZE : 18 * SECTOR_SIZE] = allocation[SECTOR_SIZE:]
    runtime = _load_mp64fs_load()
    builtin = _execute(runtime, "FS-MODULE-SOURCE")[0]
    record = runtime.define_created(
        "TEST-FILE-RECORD",
        initial_body=b"\xA5" * MODULE_FILE_BYTES,
    ).body_address
    path = b"pkg/mod.f"
    span = runtime.define_created("TEST-PATH", initial_body=path).body_address
    missing = runtime.define_created(
        "TEST-MISSING-PATH",
        initial_body=b"pkg/none.f",
    ).body_address

    assert _execute(
        runtime,
        "_FS-MS-RESOLVE",
        span,
        len(path),
        record,
        builtin,
    ) == (MS_E_NO_FILESYSTEM,)

    runtime.storage.attach(image)
    _mount(runtime)
    runtime.evaluate(b"PARSE-NAME something-else.f", source_name="stale-name")
    completion_before = runtime.storage.completion

    assert _execute(
        runtime,
        "_FS-MS-RESOLVE",
        span,
        len(path),
        record,
        builtin,
    ) == (0,)

    assert tuple(
        runtime.memory.read64(record + offset) for offset in range(0, 40, 8)
    ) == (
        (0xA1B2_C3D4 << 32) | len(text),
        (0x0102_0304 << 32) | (14 << 16) | 8,
        len(text),
        2 * SECTOR_SIZE,
        8,
    )
    assert _variable(runtime, "CWD") == 7
    assert _execute(runtime, "_FS-MS-DIR@", builtin) == (7,)
    assert runtime.memory.read_bytes(
        _execute(runtime, "PATHBUF")[0],
        len(path) + 1,
    ) == path + b"\0"
    assert runtime.storage.completion == completion_before

    buffer, status = _execute(runtime, "ALLOCATE", 2 * SECTOR_SIZE)
    assert status == 0
    loader_buffer = _variable(runtime, "LD-BUF")
    assert _execute(runtime, "_FS-MS-READ", buffer, record, builtin) == (0,)
    assert runtime.memory.read_bytes(buffer, len(text)) == text
    assert _variable(runtime, "LD-BUF") == loader_buffer != buffer
    assert runtime.storage.completion == completion_before + 2

    assert _execute(runtime, "_FS-MS-DIR!", 0xFF, builtin) == ()
    assert _variable(runtime, "CWD") == 0xFF
    assert _execute(
        runtime,
        "_FS-MS-RESOLVE",
        missing,
        len(b"pkg/none.f"),
        record,
        builtin,
    ) == (MS_E_NOT_FOUND,)
    # A path that names no file leaves the directory it was resolved from,
    # though the walk went into pkg: the loader may ask for another name.
    assert _variable(runtime, "CWD") == 0xFF
    assert runtime.drain_uart_output() == b""
    assert runtime.main_context.returns.snapshot() == ()
