"""Unchanged-source acceptance for KDOS subdirectory navigation."""

from __future__ import annotations

import hashlib
import struct
from pathlib import Path

import pytest

from shared.cells import MASK64, TRUE
from shared.mp64fs import MP64FS_ENTRY_SIZE
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_FLUSH,
    STORAGE_RESULT_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import (
    _entry_offset,
    _formatted_image,
    _write_entry,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_filesystem_encryption import (
    _load_filesystem_encryption,
    _load_filesystem_encryption_service,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _cache_addresses,
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


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-subdirectory-navigation-6201-6296.f"
)

FIRST_LINE = 6201
LAST_LINE = 6296
SLICE_BYTES = 3_082
SLICE_SHA256 = (
    "dc7f065cfac1fc3eb6efd1de7f4b0f472ff40e66fa14666e1087c18047e1d6c8"
)
SLICE_GIT_BLOB = "b964ca87a1af44e54b22abd25116edd2a7e2a853"

SOURCE_LEDGER = (
    ("CREATE", b"_PWD-STK"),
    (":", b"PWD"),
    (":", b"CD"),
    (":", b"MKDIR"),
    (":", b"RMDIR"),
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
    assert lines[FIRST_LINE - 2] == b"\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\\ =====================================================================\n"
    return source


def _evaluate_subdirectory_navigation(
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


def _load_subdirectory_navigation(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_subdirectory_navigation(
        _load_filesystem_encryption(image)
    )


def _load_subdirectory_navigation_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_subdirectory_navigation(
        _load_filesystem_encryption_service(storage)
    )


def _write_directory(
    image: bytearray,
    slot: int,
    name: bytes,
    *,
    parent: int = 0xFF,
) -> int:
    return _write_entry(
        image,
        slot,
        name=name + b"\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
        parent=parent,
    )


def _expected_directory_entry(
    name: bytes,
    *,
    parent: int,
    mtime: int,
) -> bytes:
    entry = bytearray(MP64FS_ENTRY_SIZE)
    entry[: len(name) + 1] = name + b"\0"
    entry[32] = 8
    entry[34] = parent
    struct.pack_into("<I", entry, 36, mtime & 0xFFFF_FFFF)
    return bytes(entry)


def test_subdirectory_slice_is_exact_and_load_time_pure() -> None:
    runtime = _load_filesystem_encryption(_formatted_image())
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
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    rtc_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    runtime = _evaluate_subdirectory_navigation(runtime)

    assert len(SOURCE_LEDGER) == 5
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    stack_body = runtime.find("_PWD-STK")
    following = runtime.find("PWD")
    assert stack_body is not None
    assert following is not None
    assert following.header_address - stack_body.body_address == 64
    assert _mount_snapshot(runtime) == mount_before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert (
        runtime.memory.read_bytes(namebuf, 24),
        runtime.memory.read_bytes(pathbuf, 128),
        _variable(runtime, "PN-LEN"),
    ) == parser_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == rtc_before
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_pwd_orders_acyclic_paths_caps_display_at_eight_and_cleans_stack() -> None:
    image = _formatted_image()
    for slot in range(9):
        _write_directory(
            image,
            slot,
            f"d{slot + 1}".encode("ascii"),
            parent=0xFF if slot == 0 else slot - 1,
        )
    runtime = _load_subdirectory_navigation(image)
    _mount(runtime)
    completion_before = runtime.storage.completion
    media_before = runtime.storage.image_bytes

    for cwd, expected in (
        (0xFF, b" /\r\n"),
        (0, b" /d1/\r\n"),
        (2, b" /d1/d2/d3/\r\n"),
        (7, b" /d1/d2/d3/d4/d5/d6/d7/d8/\r\n"),
        (8, b" /d2/d3/d4/d5/d6/d7/d8/d9/\r\n"),
    ):
        _store(runtime, "CWD", cwd)
        assert _execute(runtime, "PWD") == ()
        assert runtime.drain_uart_output() == expected
        assert runtime.main_context.data.snapshot() == ()
        assert runtime.main_context.returns.snapshot() == ()

    assert runtime.storage.completion == completion_before
    assert runtime.storage.image_bytes == media_before

    absent = _load_subdirectory_navigation()
    assert _execute(absent, "PWD") == ()
    assert absent.drain_uart_output() == b" /\r\n"
    assert absent.storage.completion == 0


def test_cd_handles_direct_children_parents_root_and_clean_failures() -> None:
    image = _formatted_image()
    _write_directory(image, 0, b"child")
    _write_directory(image, 1, b"grand", parent=0)
    _write_entry(
        image,
        2,
        name=b"file\0",
        start=14,
        count=1,
        used=1,
        entry_type=2,
    )
    runtime = _load_subdirectory_navigation(image)
    _mount(runtime)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"CD child", source_name="cd-child")
    assert _variable(runtime, "CWD") == 0
    assert runtime.drain_uart_output() == b""
    runtime.evaluate(b"CD grand", source_name="cd-grandchild")
    assert _variable(runtime, "CWD") == 1
    assert runtime.drain_uart_output() == b""
    runtime.evaluate(b"CD ..", source_name="cd-parent")
    assert _variable(runtime, "CWD") == 0
    assert runtime.drain_uart_output() == b""
    runtime.evaluate(b"CD /", source_name="cd-root")
    assert _variable(runtime, "CWD") == 0xFF
    assert runtime.drain_uart_output() == b""
    runtime.evaluate(b"CD ..", source_name="cd-parent-at-root")
    assert _variable(runtime, "CWD") == 0xFF
    assert runtime.drain_uart_output() == b""

    runtime.evaluate(b"CD missing", source_name="cd-missing")
    assert runtime.drain_uart_output() == b" Not found: missing\r\n"
    runtime.evaluate(b"CD file", source_name="cd-nondirectory")
    assert runtime.drain_uart_output() == b" Not a directory: file\r\n"
    runtime.evaluate(b"CD child/grand", source_name="cd-direct-component")
    assert runtime.drain_uart_output() == (
        b" Not found: child/grand\r\n"
    )

    assert _variable(runtime, "CWD") == 0xFF
    mount_after = _mount_snapshot(runtime)
    assert mount_after[:2] == mount_before[:2]
    assert mount_after[3:] == mount_before[3:]
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion_before
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    absent = _load_subdirectory_navigation()
    absent.evaluate(b"CD 41", source_name="cd-no-filesystem")
    assert absent.main_context.data.snapshot() == (41,)
    absent.main_context.data.clear()
    assert absent.main_context.returns.snapshot() == ()
    assert absent.drain_uart_output() == b" No filesystem\r\n"
    assert _variable(absent, "CWD") == 0xFF
    assert absent.storage.completion == 0


def test_mkdir_uses_lowest_slot_and_publishes_exact_metadata_only() -> None:
    image = _formatted_image()
    _write_directory(image, 0, b"parent")
    _write_entry(
        image,
        1,
        name=b"occupied\0",
        start=14,
        count=1,
        used=17,
        entry_type=5,
    )
    payload = bytes((index * 29 + 7) & 0xFF for index in range(SECTOR_SIZE))
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = payload
    runtime = _load_subdirectory_navigation(image)
    runtime.rtc.set_epoch_ms(1_700_000_123_456)
    _mount(runtime)
    _store(runtime, "CWD", 0)
    assert _execute(runtime, "FIND-FREE-SLOT") == (2,)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion
    expected_mtime = 1_700_000_123
    assert _execute(runtime, "TICKS@") == (expected_mtime,)

    runtime.evaluate(b"MKDIR newdir", source_name="mkdir-success")

    expected_entry = _expected_directory_entry(
        b"newdir",
        parent=0,
        mtime=expected_mtime,
    )
    _superblock, bitmap, directory = _cache_addresses(runtime)
    assert runtime.memory.read_bytes(
        directory + 2 * MP64FS_ENTRY_SIZE,
        MP64FS_ENTRY_SIZE,
    ) == expected_entry
    assert runtime.drain_uart_output() == b" Created dir: newdir\r\n"
    assert runtime.storage.completion == completion_before + 3
    assert _diagnostics(runtime) == (0, 12, 0)
    assert _execute(runtime, "FIND-FREE-SLOT") == (3,)

    mount_after = _mount_snapshot(runtime)
    expected_directory = bytearray(mount_before[5])
    expected_directory[
        2 * MP64FS_ENTRY_SIZE : 3 * MP64FS_ENTRY_SIZE
    ] = expected_entry
    assert mount_after[:5] == mount_before[:5]
    assert mount_after[5] == bytes(expected_directory)
    assert runtime.memory.read_bytes(bitmap, SECTOR_SIZE) == mount_before[4][
        :SECTOR_SIZE
    ]
    expected_media = bytearray(media_before)
    entry_offset = _entry_offset(image, 2)
    expected_media[entry_offset : entry_offset + MP64FS_ENTRY_SIZE] = (
        expected_entry
    )
    assert runtime.storage.image_bytes == bytes(expected_media)
    assert runtime.storage.image_bytes[14 * SECTOR_SIZE :] == payload
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()

    cache_after_success = _mount_snapshot(runtime)
    media_after_success = runtime.storage.image_bytes
    completion_after_success = runtime.storage.completion
    runtime.evaluate(b"MKDIR newdir", source_name="mkdir-duplicate")
    assert runtime.drain_uart_output() == b" Already exists: newdir\r\n"
    assert _mount_snapshot(runtime) == cache_after_success
    assert runtime.storage.image_bytes == media_after_success
    assert runtime.storage.completion == completion_after_success


def test_mkdir_full_and_empty_name_preserve_the_source_edge_behaviors() -> None:
    full_image = _formatted_image()
    for slot in range(128):
        _write_directory(full_image, slot, f"d{slot:03d}".encode("ascii"))
    full = _load_subdirectory_navigation(full_image)
    _mount(full)
    full_cache = _mount_snapshot(full)
    full_media = full.storage.image_bytes
    full_completion = full.storage.completion

    full.evaluate(b"MKDIR overflow", source_name="mkdir-full")

    assert full.drain_uart_output() == b" Directory full\r\n"
    assert _execute(full, "FIND-FREE-SLOT") == (MASK64,)
    assert _mount_snapshot(full) == full_cache
    assert full.storage.image_bytes == full_media
    assert full.storage.completion == full_completion

    empty_image = _formatted_image()
    empty = _load_subdirectory_navigation(empty_image)
    empty.rtc.set_epoch_ms(9_876_543_210)
    _mount(empty)
    cache_before = _mount_snapshot(empty)
    media_before = empty.storage.image_bytes
    completion_before = empty.storage.completion
    expected_entry = _expected_directory_entry(
        b"",
        parent=0xFF,
        mtime=9_876_543,
    )

    empty.evaluate(b"MKDIR", source_name="mkdir-empty-name")

    _superblock, _bitmap, directory = _cache_addresses(empty)
    assert empty.memory.read_bytes(directory, MP64FS_ENTRY_SIZE) == (
        expected_entry
    )
    assert empty.drain_uart_output() == b" Created dir: \r\n"
    assert empty.storage.completion == completion_before + 3
    assert _execute(empty, "FIND-FREE-SLOT") == (0,)
    empty.evaluate(b"PARSE-NAME", source_name="parse-empty-name")
    assert _execute(empty, "FIND-BY-NAME") == (MASK64,)

    expected_directory = bytearray(cache_before[5])
    expected_directory[:MP64FS_ENTRY_SIZE] = expected_entry
    cache_after = _mount_snapshot(empty)
    assert cache_after[:5] == cache_before[:5]
    assert cache_after[5] == bytes(expected_directory)
    expected_media = bytearray(media_before)
    offset = _entry_offset(empty_image, 0)
    expected_media[offset : offset + MP64FS_ENTRY_SIZE] = expected_entry
    assert empty.storage.image_bytes == bytes(expected_media)
    assert empty.storage.image_bytes[14 * SECTOR_SIZE :] == media_before[
        14 * SECTOR_SIZE :
    ]


def test_rmdir_clears_empty_and_rejects_miss_file_and_nonempty_with_slot_leak() -> None:
    image = _formatted_image()
    empty_offset = _write_directory(image, 0, b"empty")
    _write_directory(image, 1, b"parent")
    _write_directory(image, 127, b"child", parent=1)
    _write_entry(
        image,
        3,
        name=b"file\0",
        start=14,
        count=1,
        used=1,
        entry_type=2,
    )
    payload = bytes((index * 13 + 5) & 0xFF for index in range(SECTOR_SIZE))
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = payload
    runtime = _load_subdirectory_navigation(image)
    _mount(runtime)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion_before = runtime.storage.completion

    runtime.evaluate(b"RMDIR empty", source_name="rmdir-empty")

    _superblock, bitmap, directory = _cache_addresses(runtime)
    assert runtime.memory.read_bytes(directory, MP64FS_ENTRY_SIZE) == bytes(
        MP64FS_ENTRY_SIZE
    )
    assert runtime.drain_uart_output() == b" Removed dir: empty\r\n"
    assert runtime.storage.completion == completion_before + 3
    assert _diagnostics(runtime) == (0, 12, 0)
    mount_after_remove = _mount_snapshot(runtime)
    expected_directory = bytearray(mount_before[5])
    expected_directory[:MP64FS_ENTRY_SIZE] = bytes(MP64FS_ENTRY_SIZE)
    assert mount_after_remove[:5] == mount_before[:5]
    assert mount_after_remove[5] == bytes(expected_directory)
    assert runtime.memory.read_bytes(bitmap, SECTOR_SIZE) == mount_before[4][
        :SECTOR_SIZE
    ]
    expected_media = bytearray(media_before)
    expected_media[empty_offset : empty_offset + MP64FS_ENTRY_SIZE] = bytes(
        MP64FS_ENTRY_SIZE
    )
    assert runtime.storage.image_bytes == bytes(expected_media)
    assert runtime.storage.image_bytes[14 * SECTOR_SIZE :] == payload

    stable_cache = _mount_snapshot(runtime)
    stable_media = runtime.storage.image_bytes
    stable_completion = runtime.storage.completion
    runtime.evaluate(b"RMDIR missing", source_name="rmdir-missing")
    assert runtime.drain_uart_output() == b" Not found: missing\r\n"
    assert runtime.main_context.data.snapshot() == ()
    runtime.evaluate(b"RMDIR file", source_name="rmdir-file")
    assert runtime.drain_uart_output() == b" Not a directory\r\n"
    assert runtime.main_context.data.snapshot() == ()

    runtime.evaluate(b"RMDIR parent", source_name="rmdir-nonempty")
    assert runtime.drain_uart_output() == b" Directory not empty\r\n"
    assert runtime.main_context.data.snapshot() == (1,)
    runtime.main_context.data.clear()
    assert runtime.main_context.returns.snapshot() == ()
    assert _mount_snapshot(runtime) == stable_cache
    assert runtime.storage.image_bytes == stable_media
    assert runtime.storage.completion == stable_completion
    assert runtime.spinlocks.owner(2) is None


def test_mkdir_late_flush_failure_retains_published_cache_and_media_entry() -> None:
    image = _formatted_image()
    storage = HostedStorageService(
        image,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_subdirectory_navigation_service(storage)
    runtime.rtc.set_epoch_ms(1_234_567_890)
    _mount(runtime)
    cache_before = _mount_snapshot(runtime)
    media_before = storage.image_bytes
    completion_before = storage.completion
    expected_entry = _expected_directory_entry(
        b"late",
        parent=0xFF,
        mtime=1_234_567,
    )
    context = runtime.main_context
    assert context.data.snapshot() == ()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"MKDIR late",
            source_name="mkdir-late-flush-failure",
            context=context,
        )

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert storage.completion == completion_before + 2
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert _variable(runtime, "FS-OK") == TRUE

    expected_directory = bytearray(cache_before[5])
    expected_directory[:MP64FS_ENTRY_SIZE] = expected_entry
    cache_after = _mount_snapshot(runtime)
    assert cache_after[:5] == cache_before[:5]
    assert cache_after[5] == bytes(expected_directory)
    expected_media = bytearray(media_before)
    offset = _entry_offset(image, 0)
    expected_media[offset : offset + MP64FS_ENTRY_SIZE] = expected_entry
    assert storage.image_bytes == bytes(expected_media)
    assert storage.image_bytes[14 * SECTOR_SIZE :] == media_before[
        14 * SECTOR_SIZE :
    ]
    assert _execute(runtime, "FIND-FREE-SLOT") == (1,)
    assert runtime.spinlocks.owner(2) is None
