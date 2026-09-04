"""Unchanged-source acceptance and discrepancy oracles for MP64FS CAT."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.storage import SECTOR_SIZE, STORAGE_RESULT_MEDIA_REMOVED
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _diagnostics,
    _mount_snapshot,
)
from tests.simulator.test_kdos_mp64fs_mutation import (
    _load_mp64fs_mutation,
    _mount,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-mp64fs-cat-5409-5436.f"
)

FIRST_LINE = 5409
LAST_LINE = 5436
SLICE_SHA256 = (
    "e645378a2f4a6a6f5e5e46716a9d12513397bdfa6ec441aba9af51d36ff86f23"
)
SLICE_GIT_BLOB = "2d20b05dc5ca8deaf1c8ca28f80d2d36a66634e5"

DEFINITIONS = (
    b"CAT-SLOT",
    b"CAT",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 838
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == (
        b'    ."  Renamed to: " NAMEBUF .ZSTR CR ;\n'
    )
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── FS-LARGEST-FREE — largest contiguous free run in bitmap "
        "──────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_cat(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_cat(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_cat(_load_mp64fs_mutation(image))


def test_cat_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_mutation(_formatted_image())
    before = _mount_snapshot(runtime)
    epoch_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    _evaluate_mp64fs_cat(runtime)

    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _variable(runtime, "CAT-SLOT") == 0
    assert _mount_snapshot(runtime) == before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == epoch_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_cat_no_filesystem_leaves_the_name_token_unparsed() -> None:
    runtime = _load_mp64fs_cat()

    runtime.evaluate(b"CAT 41", source_name="cat-no-filesystem")

    assert runtime.main_context.data.snapshot() == (41,)
    assert runtime.main_context.returns.snapshot() == ()
    runtime.main_context.data.clear()
    assert _variable(runtime, "CAT-SLOT") == 0
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b" No filesystem\r\n"


def test_cat_missing_and_empty_files_do_not_read_content() -> None:
    image = _formatted_image(17)
    _write_entry(
        image,
        7,
        name=b"empty\0",
        start=14,
        count=1,
        used=0,
        entry_type=2,
    )
    runtime = _load_mp64fs_cat(image)
    _mount(runtime)
    completion = runtime.storage.completion
    here = runtime.dictionary.here

    runtime.evaluate(b"CAT missing", source_name="cat-missing")
    assert runtime.drain_uart_output() == b" Not found: missing\r\n"
    assert _variable(runtime, "CAT-SLOT") == MASK64

    runtime.evaluate(b"CAT empty", source_name="cat-empty")
    assert runtime.drain_uart_output() == b" (empty file)\r\n"
    assert _variable(runtime, "CAT-SLOT") == 7

    assert runtime.storage.completion == completion
    assert runtime.dictionary.here == here
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_cat_reads_the_primary_allocation_and_publishes_exact_used_bytes() -> None:
    image = _formatted_image(17)
    payload = b"A\nB\r\n\x00\x1b\x80Z"
    _write_entry(
        image,
        5,
        name=b"bytes\0",
        start=14,
        count=2,
        used=len(payload),
        entry_type=2,
    )
    allocation = payload + bytes((0xCC,)) * (2 * SECTOR_SIZE - len(payload))
    image[14 * SECTOR_SIZE : 16 * SECTOR_SIZE] = allocation
    runtime = _load_mp64fs_cat(image)
    _mount(runtime)
    completion = runtime.storage.completion
    here = runtime.dictionary.here
    runtime.memory.fill(here, 2 * SECTOR_SIZE, 0xA5)

    runtime.evaluate(b"CAT bytes", source_name="cat-bytes")

    assert runtime.drain_uart_output() == b"A\r\nB\r\r\n\x00\x1b\x80Z"
    assert runtime.storage.completion == completion + 1
    assert _diagnostics(runtime) == (0, 2, 0)
    assert runtime.memory.read_bytes(here, 2 * SECTOR_SIZE) == allocation
    assert runtime.dictionary.here == here
    assert _variable(runtime, "CAT-SLOT") == 5
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_cat_stale_media_aborts_before_publishing_content() -> None:
    image = _formatted_image(16)
    _write_entry(
        image,
        6,
        name=b"live\0",
        start=14,
        count=1,
        used=4,
        entry_type=2,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = b"data" + bytes(508)
    runtime = _load_mp64fs_cat(image)
    _mount(runtime)
    completion = runtime.storage.completion
    here = runtime.dictionary.here
    runtime.storage.attach(image)
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"CAT live",
            source_name="cat-stale-media",
            context=context,
        )

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert runtime.storage.completion == completion
    assert _variable(runtime, "FS-OK") == 0
    assert _diagnostics(runtime)[:2] == (STORAGE_RESULT_MEDIA_REMOVED, 0)
    assert _execute(runtime, "IOR-STALE?", _diagnostics(runtime)[2]) == (TRUE,)
    assert runtime.dictionary.here == here
    assert _variable(runtime, "CAT-SLOT") == 6


def test_cat_secondary_extent_discrepancy_emits_unread_here_scratch() -> None:
    image = _formatted_image(17)
    _write_entry(
        image,
        3,
        name=b"split\0",
        start=14,
        count=1,
        used=SECTOR_SIZE + 1,
        entry_type=2,
        secondary_start=15,
        secondary_count=1,
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = b"P" * SECTOR_SIZE
    image[15 * SECTOR_SIZE : 16 * SECTOR_SIZE] = b"Q" * SECTOR_SIZE
    runtime = _load_mp64fs_cat(image)
    _mount(runtime)
    completion = runtime.storage.completion
    here = runtime.dictionary.here
    runtime.memory.fill(here, SECTOR_SIZE + 1, 0xA5)

    runtime.evaluate(b"CAT split", source_name="cat-secondary-discrepancy")

    assert runtime.drain_uart_output() == b"P" * SECTOR_SIZE + b"\xA5"
    assert runtime.storage.completion == completion + 1
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.memory.read8(here + SECTOR_SIZE) == 0xA5
    assert runtime.dictionary.here == here
    assert _variable(runtime, "CAT-SLOT") == 3
