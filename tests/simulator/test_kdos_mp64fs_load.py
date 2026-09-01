"""Unchanged-source harness for the MP64FS source loader."""

from __future__ import annotations

import hashlib
from pathlib import Path

from simulator.runtime import MegaForthRuntime
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_fd_pool import (
    _load_mp64fs_fd_pool,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
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
SLICE_BYTES = 11_337
SLICE_SHA256 = (
    "efad4e40860bc7cdc484b58ac652d9b7286541a7adfdb156d4ae66a3f73ba9fe"
)
SLICE_GIT_BLOB = "8fd4577b4ac2128934672eb123ca78bf88468d52"

# Exact defining-word/name order in kdos.f.  Keeping the defining word in
# the ledger makes source-layout changes visible even if a replacement happens
# to publish the same dictionary name.
SOURCE_LEDGER = (
    ("VARIABLE", b"LD-BUF"),
    ("VARIABLE", b"LD-SZ"),
    ("VARIABLE", b"LD-CUR"),
    ("VARIABLE", b"LD-LEN"),
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
    (":", b"_LD-WALK"),
    (":", b"_LD-RELEASE"),
    (":", b"_LD-WALK-GUARDED"),
    (":", b"LOAD"),
)

DEFINITIONS = tuple(name for _definer, name in SOURCE_LEDGER)

CONSTANTS = (
    ("_LD-FRAME", 56),
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


def test_load_slice_is_exact_and_publishes_complete_source_ledger() -> None:
    runtime = _load_mp64fs_load()

    assert len(SOURCE_LEDGER) == 50
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert tuple(_constant(runtime, name) for name, _value in CONSTANTS) == (
        tuple(value for _name, value in CONSTANTS)
    )
    assert all(_variable(runtime, name) == 0 for name in VARIABLES)

    sized_bodies = (
        ("_LD-STK", "_LD-SP", 56 * 16),
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
