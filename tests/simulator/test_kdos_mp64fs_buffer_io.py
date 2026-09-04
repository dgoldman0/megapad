"""Acceptance and discrepancy oracles for MP64FS named Buffer transfers."""

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
from tests.simulator.test_kdos_mp64fs_cat import (
    _evaluate_mp64fs_cat,
)
from tests.simulator.test_kdos_mp64fs_free import (
    _evaluate_mp64fs_free,
    _load_mp64fs_free,
)
from tests.simulator.test_kdos_mp64fs_lifecycle import (
    _cache_addresses,
    _diagnostics,
    _mount_snapshot,
    _store,
)
from tests.simulator.test_kdos_mp64fs_mutation import (
    _load_mp64fs_mutation_service,
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
    / "kdos-mp64fs-buffer-io-5472-5514.f"
)

FIRST_LINE = 5472
LAST_LINE = 5514
SLICE_SHA256 = (
    "7b4511333822c8f4aca8e3fd0768fa520d72e398a14529240bf6e66792627104"
)
SLICE_GIT_BLOB = "8b4645f16c7ac2f21036282a896b7ede6bad16b0"

DEFINITIONS = (
    b"SB-SLOT",
    b"SB-DESC",
    b"SAVE-BUFFER",
    b"LB-SLOT",
    b"LB-DESC",
    b"LOAD-BUFFER",
)

SCRATCH_VARIABLES = (
    "SB-SLOT",
    "SB-DESC",
    "LB-SLOT",
    "LB-DESC",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 1_317
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == (
        b'    . ."  files, " FS-MAX-FILES . ."  max" CR ;\n'
    )
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── FD Pool — fixed pool of reusable file descriptors "
        "────────────────\n".encode("utf-8")
    )
    return source


def _evaluate_mp64fs_buffer_io(
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


def _load_mp64fs_buffer_io(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_buffer_io(_load_mp64fs_free(image))


def _load_mp64fs_buffer_io_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    runtime = _load_mp64fs_mutation_service(storage)
    _evaluate_mp64fs_cat(runtime)
    _evaluate_mp64fs_free(runtime)
    return _evaluate_mp64fs_buffer_io(runtime)


def _define_buffer(
    runtime: MegaForthRuntime,
    name: str,
    *,
    width: int = 1,
    length: int = SECTOR_SIZE,
) -> tuple[int, int]:
    runtime.evaluate(
        f"0 {width} {length} BUFFER {name}".encode("ascii"),
        source_name=f"define-{name.lower()}",
    )
    descriptor = _execute(runtime, name)[0]
    return descriptor, _execute(runtime, "B.DATA", descriptor)[0]


def _descriptor_snapshot(
    runtime: MegaForthRuntime,
    descriptor: int,
) -> tuple[int, ...]:
    return tuple(
        runtime.memory.read64(descriptor + offset)
        for offset in range(0, 32, 8)
    )


def test_buffer_io_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_free(_formatted_image())
    descriptor, data = _define_buffer(runtime, "LOAD-SENTINEL")
    runtime.memory.fill(data, SECTOR_SIZE, 0xA5)
    buffer_before = (
        _descriptor_snapshot(runtime, descriptor),
        runtime.memory.read_bytes(data, SECTOR_SIZE),
    )
    runtime.rtc.set_epoch_ms(0x0102_0304_0506_0708)
    epoch_before = (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch)
    mount_before = _mount_snapshot(runtime)

    _evaluate_mp64fs_buffer_io(runtime)

    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert all(_variable(runtime, name) == 0 for name in SCRATCH_VARIABLES)
    assert (
        _descriptor_snapshot(runtime, descriptor),
        runtime.memory.read_bytes(data, SECTOR_SIZE),
    ) == buffer_before
    assert _mount_snapshot(runtime) == mount_before
    assert (runtime.rtc.epoch_ms, runtime.rtc.epoch_latch) == epoch_before
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


def test_buffer_io_no_filesystem_drops_descriptor_but_not_name_token() -> None:
    runtime = _load_mp64fs_buffer_io()
    for index, name in enumerate(SCRATCH_VARIABLES, start=1):
        _store(runtime, name, index * 0x111)

    runtime.evaluate(b"123 SAVE-BUFFER 41", source_name="save-no-filesystem")
    assert runtime.main_context.data.snapshot() == (41,)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    runtime.main_context.data.clear()

    runtime.evaluate(b"456 LOAD-BUFFER 42", source_name="load-no-filesystem")
    assert runtime.main_context.data.snapshot() == (42,)
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b" No filesystem\r\n"
    runtime.main_context.data.clear()

    assert tuple(
        _variable(runtime, name) for name in SCRATCH_VARIABLES
    ) == (0x111, 0x222, 0x333, 0x444)
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0


def test_buffer_io_misses_store_scratch_without_dereferencing_descriptor() -> None:
    runtime = _load_mp64fs_buffer_io(_formatted_image())
    _mount(runtime)
    before = _mount_snapshot(runtime)
    media_before = runtime.storage.image_bytes
    completion = runtime.storage.completion

    runtime.evaluate(b"123 SAVE-BUFFER missing", source_name="save-missing")
    assert runtime.drain_uart_output() == (
        b" Not found: missing  (create with MKFILE first)\r\n"
    )
    assert _variable(runtime, "SB-DESC") == 123
    assert _variable(runtime, "SB-SLOT") == MASK64

    runtime.evaluate(b"456 LOAD-BUFFER absent", source_name="load-missing")
    assert runtime.drain_uart_output() == b" Not found: absent\r\n"
    assert _variable(runtime, "LB-DESC") == 456
    assert _variable(runtime, "LB-SLOT") == MASK64

    assert _mount_snapshot(runtime) == before
    assert runtime.storage.image_bytes == media_before
    assert runtime.storage.completion == completion
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()


def test_buffer_io_loads_full_allocation_then_saves_metadata_in_order() -> None:
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        7,
        name=b"target\0",
        start=14,
        count=1,
        used=3,
        entry_type=5,
    )
    image[entry_offset + 33] = 0xA5
    struct.pack_into("<I", image, entry_offset + 36, 0x1020_3040)
    struct.pack_into("<I", image, entry_offset + 40, 0x5060_7080)
    initial = b"abc" + bytes(
        ((index * 19 + 7) & 0xFF) for index in range(SECTOR_SIZE - 3)
    )
    image[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = initial
    runtime = _load_mp64fs_buffer_io(image)
    _mount(runtime)
    descriptor, data = _define_buffer(runtime, "IO-BUF")
    descriptor_before = _descriptor_snapshot(runtime, descriptor)
    runtime.memory.fill(data, SECTOR_SIZE, 0)
    cache_before_load = _mount_snapshot(runtime)
    media_before_load = runtime.storage.image_bytes
    completion = runtime.storage.completion

    runtime.evaluate(b"IO-BUF LOAD-BUFFER target", source_name="load-target")

    assert runtime.drain_uart_output() == b" Loaded 3  bytes from target\r\n"
    assert runtime.storage.completion == completion + 1
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.memory.read_bytes(data, SECTOR_SIZE) == initial
    assert _descriptor_snapshot(runtime, descriptor) == descriptor_before
    assert _mount_snapshot(runtime)[:6] == cache_before_load[:6]
    assert runtime.storage.image_bytes == media_before_load
    assert _variable(runtime, "LB-DESC") == descriptor
    assert _variable(runtime, "LB-SLOT") == 7

    pattern = bytes(
        ((index * 37 + 11) & 0xFF) for index in range(SECTOR_SIZE)
    )
    runtime.memory.write_bytes(data, pattern)
    _superblock, bitmap, directory = _cache_addresses(runtime)
    bitmap_before = runtime.memory.read_bytes(bitmap, SECTOR_SIZE)
    directory_before = runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE)
    entry_before = directory_before[7 * 48 : 8 * 48]
    media_before_save = runtime.storage.image_bytes
    completion = runtime.storage.completion

    runtime.evaluate(b"IO-BUF SAVE-BUFFER target", source_name="save-target")

    assert runtime.drain_uart_output() == b" Saved 512  bytes to target\r\n"
    assert runtime.storage.completion == completion + 4
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.memory.read_bytes(bitmap, SECTOR_SIZE) == bitmap_before
    expected_entry = bytearray(entry_before)
    struct.pack_into("<I", expected_entry, 28, SECTOR_SIZE)
    expected_directory = bytearray(directory_before)
    expected_directory[7 * 48 : 8 * 48] = expected_entry
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == (
        bytes(expected_directory)
    )

    expected_media = bytearray(media_before_save)
    expected_media[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = pattern
    expected_media[entry_offset : entry_offset + 48] = expected_entry
    assert runtime.storage.image_bytes == bytes(expected_media)
    assert _descriptor_snapshot(runtime, descriptor) == descriptor_before
    assert _variable(runtime, "SB-DESC") == descriptor
    assert _variable(runtime, "SB-SLOT") == 7
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    assert runtime.spinlocks.owner(2) is None


def test_buffer_width_discrepancy_stores_length_but_transfers_full_sector() -> None:
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        2,
        name=b"wide\0",
        start=14,
        count=1,
        used=5,
        entry_type=5,
    )
    runtime = _load_mp64fs_buffer_io(image)
    _mount(runtime)
    descriptor, data = _define_buffer(
        runtime,
        "WIDE-BUF",
        width=2,
        length=256,
    )
    descriptor_before = _descriptor_snapshot(runtime, descriptor)
    pattern = bytes(
        ((index * 13 + 0x31) & 0xFF) for index in range(SECTOR_SIZE)
    )
    runtime.memory.write_bytes(data, pattern)
    completion = runtime.storage.completion

    runtime.evaluate(b"WIDE-BUF SAVE-BUFFER wide", source_name="save-wide")

    assert runtime.drain_uart_output() == b" Saved 256  bytes to wide\r\n"
    assert runtime.storage.completion == completion + 4
    assert runtime.storage.image_bytes[
        14 * SECTOR_SIZE : 15 * SECTOR_SIZE
    ] == pattern
    assert struct.unpack_from(
        "<I", runtime.storage.image_bytes, entry_offset + 28
    )[0] == 256
    assert _execute(runtime, "B.BYTES", descriptor) == (SECTOR_SIZE,)
    assert _execute(runtime, "B.LEN", descriptor) == (256,)
    assert _descriptor_snapshot(runtime, descriptor) == descriptor_before

    runtime.memory.fill(data, SECTOR_SIZE, 0)
    completion = runtime.storage.completion
    media_before_load = runtime.storage.image_bytes
    runtime.evaluate(b"WIDE-BUF LOAD-BUFFER wide", source_name="load-wide")

    assert runtime.drain_uart_output() == b" Loaded 256  bytes from wide\r\n"
    assert runtime.storage.completion == completion + 1
    assert _diagnostics(runtime) == (0, 1, 0)
    assert runtime.memory.read_bytes(data, SECTOR_SIZE) == pattern
    assert runtime.storage.image_bytes == media_before_load
    assert _execute(runtime, "B.LEN", descriptor) == (256,)
    assert _descriptor_snapshot(runtime, descriptor) == descriptor_before


def test_save_buffer_flush_failure_retains_payload_and_used_metadata() -> None:
    image = _formatted_image(16)
    entry_offset = _write_entry(
        image,
        4,
        name=b"late\0",
        start=14,
        count=1,
        used=3,
        entry_type=5,
    )
    struct.pack_into("<II", image, entry_offset + 36, 0x1122_3344, 0x5566_7788)
    storage = HostedStorageService(
        image,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_mp64fs_buffer_io_service(storage)
    _mount(runtime)
    descriptor, data = _define_buffer(runtime, "LATE-BUF")
    pattern = bytes(
        ((index * 29 + 0x17) & 0xFF) for index in range(SECTOR_SIZE)
    )
    runtime.memory.write_bytes(data, pattern)
    _superblock, _bitmap, directory = _cache_addresses(runtime)
    entry_before = runtime.memory.read_bytes(directory + 4 * 48, 48)
    media_before = storage.image_bytes
    completion = storage.completion
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.evaluate(
            b"LATE-BUF SAVE-BUFFER late",
            source_name="save-buffer-flush-failure",
            context=context,
        )

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert storage.completion == completion + 3
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "SB-DESC") == descriptor
    assert _variable(runtime, "SB-SLOT") == 4

    expected_entry = bytearray(entry_before)
    struct.pack_into("<I", expected_entry, 28, SECTOR_SIZE)
    assert runtime.memory.read_bytes(directory + 4 * 48, 48) == (
        bytes(expected_entry)
    )
    expected_media = bytearray(media_before)
    expected_media[14 * SECTOR_SIZE : 15 * SECTOR_SIZE] = pattern
    expected_media[entry_offset : entry_offset + 48] = expected_entry
    assert storage.image_bytes == bytes(expected_media)
    assert runtime.spinlocks.owner(2) is None
