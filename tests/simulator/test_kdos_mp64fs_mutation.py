"""Unchanged-source acceptance for MP64FS lookup and metadata mutation."""

from __future__ import annotations

import hashlib
import struct
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
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _cache_addresses,
    _diagnostics,
    _load_mp64fs_lifecycle_service,
    _mount_snapshot,
)
from tests.simulator.test_kdos_mp64fs_listing import (
    _evaluate_mp64fs_listing,
    _load_mp64fs_listing,
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
    / "kdos-mp64fs-mutation-5286-5408.f"
)

FIRST_LINE = 5286
LAST_LINE = 5408
SLICE_SHA256 = (
    "a890bfaabc682f1c6d9b71ccbbcc5767d4184da1184ea363b87754496ae9c028"
)
SLICE_GIT_BLOB = "6020009b45d56172fb97289a6a441dea51d19590"

DEFINITIONS = (
    b"FIND-BY-NAME",
    b"TICKS@",
    b"MK-NSEC",
    b"MK-TYPE",
    b"MK-SLOT",
    b"MK-START",
    b"MKFILE",
    b"RM-SLOT",
    b"RMFILE",
    b"RN-SLOT",
    b"RENAME",
)

SCRATCH_VARIABLES = (
    "MK-NSEC",
    "MK-TYPE",
    "MK-SLOT",
    "MK-START",
    "RM-SLOT",
    "RN-SLOT",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 4_020
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b'    . ."  free sectors)" CR ;\n'
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── CAT — print file contents to terminal "
        "────────────────────────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_mutation(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_mutation(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_mutation(_load_mp64fs_listing(image))


def _load_mp64fs_mutation_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_mutation(
        _evaluate_mp64fs_listing(
            _load_mp64fs_lifecycle_service(storage)
        )
    )


def _mount(runtime: MegaForthRuntime) -> None:
    assert _execute(runtime, "FS-LOAD") == ()
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.drain_uart_output() == b" MP64FS loaded\r\n"


def _pattern_data(image: bytearray, data_start: int = 14) -> bytes:
    payload = bytes(
        ((index * 17 + 3) & 0xFF)
        for index in range(len(image) - data_start * SECTOR_SIZE)
    )
    image[data_start * SECTOR_SIZE :] = payload
    return payload


def test_mp64fs_mutation_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_listing(_formatted_image())
    runtime.rtc.set_epoch_ms(0x0102_0304_0506_0708)
    before = _mount_snapshot(runtime)
    epoch_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)

    _evaluate_mp64fs_mutation(runtime)

    assert len(DEFINITIONS) == 11
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert all(_variable(runtime, name) == 0 for name in SCRATCH_VARIABLES)
    assert _mount_snapshot(runtime) == before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == epoch_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""


def test_ticks_and_mkfile_publish_first_fit_metadata_without_wiping_data() -> None:
    image = _formatted_image(24)
    expected_data = _pattern_data(image)
    _write_entry(
        image,
        7,
        name=b"prefix\0",
        start=14,
        count=3,
        used=1,
        entry_type=1,
    )
    runtime = _load_mp64fs_mutation(image)
    epoch_ms = 1_700_000_123_456
    epoch_seconds = epoch_ms // 1000
    runtime.rtc.set_epoch_ms(epoch_ms)

    assert _execute(runtime, "TICKS@") == (epoch_seconds,)
    runtime.evaluate(b"2 2 MKFILE foo", source_name="mkfile-success")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == (
        b" MP64FS loaded\r\n"
        b" Created: foo (2  sectors at 17  )\r\n"
    )
    assert runtime.storage.completion == 9
    assert _diagnostics(runtime) == (0, 12, 0)
    assert _variable(runtime, "MK-NSEC") == 2
    assert _variable(runtime, "MK-TYPE") == 2
    assert _variable(runtime, "MK-SLOT") == 0
    assert _variable(runtime, "MK-START") == 17
    assert runtime.rtc.epoch_ms == epoch_ms

    _superblock, bitmap, directory = _cache_addresses(runtime)
    entry = runtime.memory.read_bytes(directory, 48)
    expected_entry = bytearray(48)
    expected_entry[:4] = b"foo\0"
    struct.pack_into("<HHI", expected_entry, 24, 17, 2, 0)
    expected_entry[32] = 2
    expected_entry[34] = 0xFF
    struct.pack_into("<I", expected_entry, 36, epoch_seconds & 0xFFFF_FFFF)
    assert entry == bytes(expected_entry)
    assert runtime.memory.read8(bitmap + 17 // 8) & (1 << (17 % 8))
    assert runtime.memory.read8(bitmap + 18 // 8) & (1 << (18 % 8))

    media = runtime.storage.image_bytes
    assert media[_entry_offset(image, 0) : _entry_offset(image, 0) + 48] == (
        bytes(expected_entry)
    )
    assert media[SECTOR_SIZE + 17 // 8] & (1 << (17 % 8))
    assert media[SECTOR_SIZE + 18 // 8] & (1 << (18 % 8))
    assert media[14 * SECTOR_SIZE :] == expected_data


def test_find_by_name_uses_cwd_first_slot_and_all_24_name_bytes() -> None:
    image = _formatted_image(20)
    stale_tail = _write_entry(
        image,
        1,
        name=b"alpha\0",
        start=14,
        used=1,
        entry_type=1,
    )
    image[stale_tail + 6] = 0xA5
    _write_entry(
        image,
        4,
        name=b"alpha\0",
        start=15,
        used=1,
        entry_type=1,
    )
    _write_entry(
        image,
        6,
        name=b"alpha\0",
        start=16,
        used=1,
        entry_type=1,
    )
    _write_entry(
        image,
        7,
        name=b"parent\0",
        start=0,
        count=0,
        used=0,
        entry_type=8,
    )
    _write_entry(
        image,
        2,
        name=b"alpha\0",
        start=17,
        used=1,
        entry_type=1,
        parent=7,
    )
    runtime = _load_mp64fs_mutation(image)
    _mount(runtime)
    completion = runtime.storage.completion

    runtime.evaluate(b"PARSE-NAME alpha")
    assert _execute(runtime, "FIND-BY-NAME") == (4,)
    cwd = _execute(runtime, "CWD")[0]
    runtime.memory.write64(cwd, 7)
    assert _execute(runtime, "FIND-BY-NAME") == (2,)

    runtime.evaluate(b"PARSE-NAME missing")
    assert _execute(runtime, "FIND-BY-NAME") == (MASK64,)
    runtime.memory.write64(cwd, 0xFF)
    runtime.memory.write64(_execute(runtime, "FS-OK")[0], 0)
    runtime.evaluate(b"PARSE-NAME alpha")
    assert _execute(runtime, "FIND-BY-NAME") == (4,)
    assert runtime.storage.completion == completion
    assert runtime.drain_uart_output() == b""


def test_mutation_guards_leave_unparsed_numeric_name_tokens() -> None:
    runtime = _load_mp64fs_mutation()

    runtime.evaluate(b"1 1 MKFILE 41")
    assert runtime.main_context.data.snapshot() == (41,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"

    runtime.evaluate(b"RMFILE 42")
    assert runtime.main_context.data.snapshot() == (42,)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"

    runtime.evaluate(b"RENAME 43 44")
    assert runtime.main_context.data.snapshot() == (43, 44)
    runtime.main_context.data.clear()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    assert runtime.storage.completion == 0


def test_mkfile_duplicate_directory_full_and_no_space_do_not_sync() -> None:
    duplicate_image = _formatted_image(20)
    _write_entry(
        duplicate_image,
        0,
        name=b"dupe\0",
        start=14,
        used=1,
        entry_type=1,
    )
    duplicate = _load_mp64fs_mutation(duplicate_image)
    _mount(duplicate)
    before = _mount_snapshot(duplicate)
    media_before = duplicate.storage.image_bytes
    completion = duplicate.storage.completion
    duplicate.evaluate(b"2 2 MKFILE dupe")
    assert duplicate.drain_uart_output() == b" File exists: dupe\r\n"
    assert _mount_snapshot(duplicate) == before
    assert duplicate.storage.image_bytes == media_before
    assert duplicate.storage.completion == completion
    assert _variable(duplicate, "MK-NSEC") == 2
    assert _variable(duplicate, "MK-TYPE") == 2

    full_image = _formatted_image(142)
    for slot in range(128):
        _write_entry(
            full_image,
            slot,
            name=f"f{slot:03d}\0".encode("ascii"),
            start=14 + slot,
            used=1,
            entry_type=1,
        )
    full = _load_mp64fs_mutation(full_image)
    _mount(full)
    before = _mount_snapshot(full)
    media_before = full.storage.image_bytes
    completion = full.storage.completion
    full.evaluate(b"1 1 MKFILE overflow")
    assert full.drain_uart_output() == b" Directory full\r\n"
    assert _mount_snapshot(full) == before
    assert full.storage.image_bytes == media_before
    assert full.storage.completion == completion
    assert _variable(full, "MK-SLOT") == MASK64

    tight_image = _formatted_image(20)
    for slot, sector in enumerate((14, 16, 18)):
        _write_entry(
            tight_image,
            slot,
            name=f"used{slot}\0".encode("ascii"),
            start=sector,
            used=1,
            entry_type=1,
        )
    tight = _load_mp64fs_mutation(tight_image)
    _mount(tight)
    before = _mount_snapshot(tight)
    media_before = tight.storage.image_bytes
    completion = tight.storage.completion
    tight.evaluate(b"2 1 MKFILE tight")
    assert tight.drain_uart_output() == (
        b" No space: need 2  sectors, 3  free\r\n"
    )
    assert _mount_snapshot(tight) == before
    assert tight.storage.image_bytes == media_before
    assert tight.storage.completion == completion
    assert _variable(tight, "MK-SLOT") == 3
    assert _variable(tight, "MK-START") == MASK64


def test_rmfile_clears_both_extents_and_entry_without_wiping_payload() -> None:
    image = _formatted_image(24)
    expected_data = _pattern_data(image)
    victim = _write_entry(
        image,
        5,
        name=b"victim\0",
        start=14,
        count=2,
        used=1_500,
        entry_type=5,
        secondary_start=18,
        secondary_count=2,
    )
    _write_entry(
        image,
        1,
        name=b"other\0",
        start=16,
        used=1,
        entry_type=1,
    )
    runtime = _load_mp64fs_mutation(image)
    _mount(runtime)
    completion = runtime.storage.completion

    runtime.evaluate(b"RMFILE victim")

    assert runtime.main_context.data.snapshot() == ()
    assert runtime.drain_uart_output() == b" Deleted: victim\r\n"
    assert runtime.storage.completion == completion + 3
    assert _variable(runtime, "RM-SLOT") == 5
    _superblock, bitmap, directory = _cache_addresses(runtime)
    for sector in (14, 15, 18, 19):
        assert _execute(runtime, "BIT-FREE?", sector) == (TRUE,)
    assert _execute(runtime, "BIT-FREE?", 16) == (0,)
    assert runtime.memory.read_bytes(directory + 5 * 48, 48) == bytes(48)
    media = runtime.storage.image_bytes
    assert media[victim : victim + 48] == bytes(48)
    for sector in (14, 15, 18, 19):
        assert not media[SECTOR_SIZE + sector // 8] & (1 << (sector % 8))
    assert media[SECTOR_SIZE + 16 // 8] & (1 << (16 % 8))
    assert media[14 * SECTOR_SIZE :] == expected_data
    assert runtime.memory.read8(bitmap + 16 // 8) & (1 << (16 % 8))

    after_delete = _mount_snapshot(runtime)
    media_after_delete = runtime.storage.image_bytes
    completion = runtime.storage.completion
    runtime.evaluate(b"RMFILE victim")
    assert runtime.drain_uart_output() == b" Not found: victim\r\n"
    assert _variable(runtime, "RM-SLOT") == MASK64
    assert _mount_snapshot(runtime) == after_delete
    assert runtime.storage.image_bytes == media_after_delete
    assert runtime.storage.completion == completion


def test_rename_changes_only_name_and_short_circuits_collisions() -> None:
    image = _formatted_image(20)
    old = _write_entry(
        image,
        5,
        name=b"old\0",
        start=14,
        used=100,
        entry_type=2,
    )
    struct.pack_into("<I", image, old + 36, 0x1234_5678)
    _write_entry(
        image,
        1,
        name=b"taken\0",
        start=15,
        used=1,
        entry_type=1,
    )
    runtime = _load_mp64fs_mutation(image)
    _mount(runtime)
    _superblock, _bitmap, directory = _cache_addresses(runtime)
    mount_before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    before_entry = runtime.memory.read_bytes(directory + 5 * 48, 48)
    completion = runtime.storage.completion

    runtime.evaluate(b"RENAME old new")

    assert runtime.drain_uart_output() == b" Renamed to: new\r\n"
    assert runtime.storage.completion == completion + 3
    after_entry = runtime.memory.read_bytes(directory + 5 * 48, 48)
    assert after_entry[:24] == b"new\0" + bytes(20)
    assert after_entry[24:] == before_entry[24:]
    mount_after = _mount_snapshot(runtime)
    expected_directory = bytearray(mount_before[5])
    expected_directory[5 * 48 : 6 * 48] = after_entry
    assert mount_after[:5] == mount_before[:5]
    assert mount_after[5] == bytes(expected_directory)
    assert mount_after[6] == mount_before[6]
    expected_media = bytearray(media_before)
    expected_media[old : old + 48] = after_entry
    assert runtime.storage.image_bytes == bytes(expected_media)
    assert _variable(runtime, "RN-SLOT") == 5

    completion = runtime.storage.completion
    cache_after_success = _mount_snapshot(runtime)
    media_after_success = runtime.storage.image_bytes
    runtime.evaluate(b"RENAME new new")
    assert runtime.drain_uart_output() == b" Name taken: new\r\n"
    runtime.evaluate(b"RENAME new taken")
    assert runtime.drain_uart_output() == b" Name taken: taken\r\n"
    assert runtime.storage.completion == completion
    assert _mount_snapshot(runtime) == cache_after_success
    assert runtime.storage.image_bytes == media_after_success

    runtime.evaluate(b"RENAME ghost 41")
    assert runtime.drain_uart_output() == b" Not found: ghost\r\n"
    assert runtime.main_context.data.snapshot() == (41,)
    runtime.main_context.data.clear()
    assert runtime.storage.completion == completion
    assert _mount_snapshot(runtime) == cache_after_success
    assert runtime.storage.image_bytes == media_after_success


def test_mkfile_flush_failure_retains_cache_and_staged_metadata() -> None:
    original = _formatted_image(20)
    expected_data = _pattern_data(original)
    storage = HostedStorageService(
        original,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_mp64fs_mutation_service(storage)
    runtime.rtc.set_epoch_ms(1_700_000_123_456)
    _mount(runtime)
    completion = storage.completion
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"1 1 MKFILE late",
            source_name="mkfile-flush-failure",
            context=context,
        )

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert storage.completion == completion + 2
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert _variable(runtime, "FS-OK") == TRUE
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    _superblock, bitmap, directory = _cache_addresses(runtime)
    expected_entry = runtime.memory.read_bytes(directory, 48)
    assert expected_entry[:5] == b"late\0"
    assert _execute(runtime, "BIT-FREE?", 14) == (0,)
    media = storage.image_bytes
    assert media[_entry_offset(original, 0) : _entry_offset(original, 0) + 48] == (
        expected_entry
    )
    assert media[SECTOR_SIZE + 14 // 8] & (1 << (14 % 8))
    assert runtime.memory.read8(bitmap + 14 // 8) & (1 << (14 % 8))
    assert media[14 * SECTOR_SIZE :] == expected_data
    assert runtime.spinlocks.owner(2) is None
