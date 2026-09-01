"""Unchanged-source acceptance for MP64FS directory listing words."""

from __future__ import annotations

import hashlib
from pathlib import Path

from shared.storage import SECTOR_SIZE
from simulator.runtime import MegaForthRuntime
from tests.simulator.test_bios_mp64fs import _formatted_image, _write_entry
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _load_mp64fs_lifecycle,
    _mount_snapshot,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _execute,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-mp64fs-listing-5218-5285.f"
)

FIRST_LINE = 5218
LAST_LINE = 5285
SLICE_SHA256 = (
    "c3c831bc183ee999c8b5a0d1fb4edd169890be1e5fa44ad726d3025923fdb3b7"
)
SLICE_GIT_BLOB = "2f81cfae66c857b917a8930ecde7c43cac7695e0"

DEFINITIONS = (
    b".FTYPE",
    b"DIR",
    b"CATALOG",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2_167
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b'    ."  MP64FS formatted" CR ;\n'
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── FIND-BY-NAME — shared directory lookup "
        "───────────────────────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_listing(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_listing(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_listing(_load_mp64fs_lifecycle(image))


def test_mp64fs_listing_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_lifecycle(_formatted_image())
    before = _mount_snapshot(runtime)

    _evaluate_mp64fs_listing(runtime)

    assert len(DEFINITIONS) == 3
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _mount_snapshot(runtime) == before
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_ftype_publishes_every_named_type_and_numeric_unknowns() -> None:
    runtime = _load_mp64fs_listing()
    expected = (
        b" free",
        b" raw",
        b" text",
        b" forth",
        b" doc",
        b" data",
        b" tut",
        b" bdl",
        b" dir",
        b" stream",
        b" link",
    )

    for entry_type, label in enumerate(expected):
        assert _execute(runtime, ".FTYPE", entry_type) == ()
        assert runtime.drain_uart_output() == label

    assert _execute(runtime, ".FTYPE", 255) == ()
    assert runtime.drain_uart_output() == b" ?255 "
    assert _execute(runtime, "HEX") == ()
    assert _execute(runtime, ".FTYPE", 255) == ()
    assert runtime.drain_uart_output() == b" ?FF "
    assert runtime.storage.completion == 0


def test_directory_publishers_report_absent_filesystem_without_io() -> None:
    runtime = _load_mp64fs_listing()

    assert _execute(runtime, "DIR") == ()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _execute(runtime, "CATALOG") == ()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0


def test_directory_publishers_mount_filter_and_count_exact_cached_state() -> None:
    image = _formatted_image(20)
    _write_entry(
        image,
        0,
        name=b"alpha.txt\0",
        start=14,
        count=1,
        used=5,
        entry_type=2,
    )
    _write_entry(
        image,
        1,
        name=b"docs\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    target = _write_entry(
        image,
        127,
        name=b"target\0",
        start=15,
        count=1,
        used=700,
        entry_type=10,
        secondary_start=16,
        secondary_count=1,
    )
    image[target + 33] = 0xA5
    _write_entry(
        image,
        2,
        name=b"child.f\0",
        start=17,
        count=1,
        used=12,
        entry_type=3,
        parent=1,
    )
    runtime = _load_mp64fs_listing(image)

    assert _execute(runtime, "DIR") == ()
    assert runtime.drain_uart_output() == (
        b" MP64FS loaded\r\n"
        b" --- Directory ---\r\n"
        b"  alpha.txt   5  B    text\r\n"
        b"  docs /   0  B    dir\r\n"
        b"  target   700  B    link\r\n"
        b"3  file(s), 2  free sectors (1024  bytes free)\r\n"
    )
    assert runtime.storage.completion == 6

    runtime.storage.attach(_formatted_image())
    assert _execute(runtime, "CATALOG") == ()
    assert runtime.drain_uart_output() == (
        b" Name                     Bytes     Secs  Type  Flg\r\n"
        b"  alpha.txt  5   1   2   0 \r\n"
        b"  docs  0   0   8   0 \r\n"
        b"  target  700   1   10   165 \r\n"
        b" (3  files, 2  free sectors)\r\n"
    )
    assert _variable(runtime, "FS-OK") != 0
    assert runtime.storage.completion == 6

    cwd = _execute(runtime, "CWD")[0]
    runtime.memory.write64(cwd, 1)
    assert _execute(runtime, "DIR") == ()
    assert runtime.drain_uart_output() == (
        b" --- Directory ---\r\n"
        b"  child.f   12  B    forth\r\n"
        b"1  file(s), 2  free sectors (1024  bytes free)\r\n"
    )
    assert runtime.storage.completion == 6


def test_dir_discrepancy_oracle_preserves_unterminated_name_spill() -> None:
    image = _formatted_image()
    _write_entry(
        image,
        0,
        name=b"A" * 24,
        start=14,
        count=1,
        used=1,
        entry_type=2,
    )
    runtime = _load_mp64fs_listing(image)

    assert _execute(runtime, "DIR") == ()

    assert runtime.drain_uart_output() == (
        b" MP64FS loaded\r\n"
        b" --- Directory ---\r\n"
        + b"  "
        + b"A" * 24
        + b"\x0e   1  B    text\r\n"
        + b"1  file(s), 0  free sectors (0  bytes free)\r\n"
    )
    assert _variable(runtime, "FS-OK") != 0
    assert runtime.storage.completion == 6
