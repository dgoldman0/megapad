"""Unchanged-source acceptance for the MP64FS mount and format lifecycle."""

from __future__ import annotations

import hashlib
import struct
from pathlib import Path

import pytest

from shared.cells import TRUE
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_FLUSH,
    STORAGE_CMD_READ,
    STORAGE_RESULT_MEDIA_REMOVED,
    STORAGE_RESULT_UNSUPPORTED,
)
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_bios_mp64fs import (
    _formatted_image,
    _superblock,
    _write_entry,
)
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_file_abstraction import (
    _evaluate_file_abstraction,
)
from tests.simulator.test_kdos_mp64fs_cache import (
    _evaluate_mp64fs_cache,
    _load_mp64fs_cache,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _execute,
    _variable,
)
from tests.simulator.test_kdos_storage_compat import (
    _load_storage_compat_service,
    _patterned_image,
    _system_objects,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-mp64fs-lifecycle-5135-5217.f"
)

FIRST_LINE = 5135
LAST_LINE = 5217
SLICE_SHA256 = (
    "829268e2d06f11c19bda4a5fa0606e883fdf3ab4a3690a741f0cd2616ada4137"
)
SLICE_GIT_BLOB = "dc762b3e8a5fc77134cc61d685bc5e4d9f46a34f"

DEFINITIONS = (
    b"FS-LOAD",
    b"FS-SYNC",
    b"FS-ENSURE",
    b"FORMAT",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 2_999
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    LOOP ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    assert lines[LAST_LINE + 1] == (
        "\\ ── .FTYPE — print file type name ───────────────────────────────────\n".encode(
            "utf-8"
        )
    )
    return source


def _evaluate_mp64fs_lifecycle(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_mp64fs_lifecycle(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_lifecycle(_load_mp64fs_cache(image))


def _load_mp64fs_lifecycle_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    return _evaluate_mp64fs_lifecycle(
        _evaluate_mp64fs_cache(
            _evaluate_file_abstraction(
                _load_storage_compat_service(storage)
            )
        )
    )


def _store(runtime: MegaForthRuntime, name: str, value: int) -> None:
    address = _execute(runtime, name)[0]
    runtime.memory.write64(address, value)


def _cache_addresses(runtime: MegaForthRuntime) -> tuple[int, int, int]:
    return (
        _execute(runtime, "FS-SUPER")[0],
        _execute(runtime, "FS-BMAP")[0],
        _execute(runtime, "FS-DIR")[0],
    )


def _mount_snapshot(runtime: MegaForthRuntime) -> tuple[object, ...]:
    superblock, bitmap, directory = _cache_addresses(runtime)
    return (
        _variable(runtime, "FS-TOTAL"),
        _variable(runtime, "FS-BMAP-N"),
        _variable(runtime, "CWD"),
        runtime.memory.read_bytes(superblock, SECTOR_SIZE),
        runtime.memory.read_bytes(bitmap, 16 * SECTOR_SIZE),
        runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE),
        _diagnostics(runtime),
    )


def _diagnostics(runtime: MegaForthRuntime) -> tuple[int, int, int]:
    return (
        _variable(runtime, "DISK-IO-STATUS"),
        _variable(runtime, "DISK-IO-COMPLETED"),
        _variable(runtime, "DISK-IO-IOR"),
    )


def _metadata_bitmap(total_sectors: int) -> bytes:
    bitmap_sectors = (total_sectors + 4095) // 4096
    data_start = 1 + bitmap_sectors + 12
    bitmap = bytearray(bitmap_sectors * SECTOR_SIZE)
    for sector in range(data_start):
        bitmap[sector // 8] |= 1 << (sector % 8)
    return bytes(bitmap)


def test_mp64fs_lifecycle_slice_is_exact_and_has_no_load_time_effects() -> None:
    runtime = _load_mp64fs_lifecycle(_formatted_image())

    assert len(DEFINITIONS) == 4
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b""
    assert runtime.spinlocks.owner(2) is None


@pytest.mark.parametrize(
    ("case", "expected_uart", "expected_completion"),
    (
        ("absent", b" No disk attached\r\n", 0),
        ("bad-magic", b" Invalid MP64FS\r\n", 1),
    ),
)
def test_fs_load_failure_precedence_preserves_unpublished_cache_state(
    case: str,
    expected_uart: bytes,
    expected_completion: int,
) -> None:
    image: bytearray | None
    if case == "absent":
        image = None
    else:
        image = _formatted_image()
        image[0:4] = b"BAD!"
    runtime = _load_mp64fs_lifecycle(image)
    if case == "absent":
        assert _execute(runtime, "FS-ENSURE") == ()
        assert _variable(runtime, "FS-OK") == 0
        assert runtime.storage.completion == 0
        assert runtime.drain_uart_output() == b""
    superblock, bitmap, directory = _cache_addresses(runtime)
    runtime.memory.fill(superblock, SECTOR_SIZE, 0xA1)
    runtime.memory.fill(bitmap, 16 * SECTOR_SIZE, 0xB2)
    runtime.memory.fill(directory, 12 * SECTOR_SIZE, 0xC3)
    _store(runtime, "FS-TOTAL", 0x111)
    _store(runtime, "FS-BMAP-N", 7)
    _store(runtime, "CWD", 37)
    _store(runtime, "DISK-IO-STATUS", 0x44)
    _store(runtime, "DISK-IO-COMPLETED", 0x55)
    _store(runtime, "DISK-IO-IOR", 0x66)
    _store(runtime, "FS-OK", TRUE)
    before = _mount_snapshot(runtime)

    assert _execute(runtime, "FS-LOAD") == ()

    assert _variable(runtime, "FS-OK") == 0
    assert _mount_snapshot(runtime) == before
    assert runtime.storage.completion == expected_completion
    assert runtime.drain_uart_output() == expected_uart


def test_fs_ensure_loads_dynamic_geometry_then_trusts_the_true_marker() -> None:
    image = _formatted_image(4_097)
    _write_entry(
        image,
        37,
        name=b"payload.bin\0",
        start=15,
        used=SECTOR_SIZE,
    )
    original = bytes(image)
    runtime = _load_mp64fs_lifecycle(image)
    superblock, bitmap, directory = _cache_addresses(runtime)
    runtime.memory.fill(bitmap, 16 * SECTOR_SIZE, 0xA5)
    _store(runtime, "CWD", 73)

    assert _execute(runtime, "FS-ENSURE") == ()

    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "FS-TOTAL") == 4_097
    assert _variable(runtime, "FS-BMAP-N") == 2
    assert _execute(runtime, "FS-DIR-START") == (3,)
    assert _execute(runtime, "FS-DSTART") == (15,)
    assert _variable(runtime, "CWD") == 73
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == original[
        :SECTOR_SIZE
    ]
    assert runtime.memory.read_bytes(bitmap, 2 * SECTOR_SIZE) == original[
        SECTOR_SIZE : 3 * SECTOR_SIZE
    ]
    assert runtime.memory.read_bytes(
        bitmap + 2 * SECTOR_SIZE,
        14 * SECTOR_SIZE,
    ) == bytes((0xA5,)) * (14 * SECTOR_SIZE)
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == original[
        3 * SECTOR_SIZE : 15 * SECTOR_SIZE
    ]
    block, volume = _system_objects(runtime)
    assert _variable(runtime, "FS-VOLUME") == volume
    assert _execute(runtime, "BD-VALID?", block) == (TRUE,)
    assert _execute(runtime, "VOL-VALID?", volume) == (TRUE,)
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.storage.completion == 6
    assert runtime.drain_uart_output() == b" MP64FS loaded\r\n"

    runtime.storage.attach(bytes((0xE7,)) * len(original))
    completion = runtime.storage.completion
    assert _execute(runtime, "FS-ENSURE") == ()
    assert runtime.storage.completion == completion
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "CWD") == 73
    assert runtime.drain_uart_output() == b""


def test_fs_load_late_read_failure_retains_progress_without_mounting() -> None:
    original = _formatted_image()
    replacement = _formatted_image()
    _write_entry(replacement, 0, name=b"replacement.bin\0", used=SECTOR_SIZE)

    class SwapAtSixthReadStorage(HostedStorageService):
        def __init__(self) -> None:
            super().__init__(original)
            self.read_acceptances = 0

        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            if command != STORAGE_CMD_READ:
                return
            assert expected_generation == self.media_generation
            self.read_acceptances += 1
            if self.read_acceptances == 6:
                self.attach(replacement)

    storage = SwapAtSixthReadStorage()
    runtime = _load_mp64fs_lifecycle_service(storage)
    superblock, bitmap, directory = _cache_addresses(runtime)
    runtime.memory.fill(directory, 12 * SECTOR_SIZE, 0xA5)
    _store(runtime, "CWD", 41)
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("FS-LOAD", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert storage.read_acceptances == 6
    assert storage.completion == 6
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "FS-TOTAL") == 15
    assert _variable(runtime, "FS-BMAP-N") == 1
    assert _variable(runtime, "CWD") == 41
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == original[
        :SECTOR_SIZE
    ]
    assert runtime.memory.read_bytes(bitmap, SECTOR_SIZE) == original[
        SECTOR_SIZE : 2 * SECTOR_SIZE
    ]
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == bytes(
        (0xA5,)
    ) * (12 * SECTOR_SIZE)
    expected_ior = _execute(
        runtime,
        "IOR-FROM-BLOCK-RESULT",
        STORAGE_RESULT_MEDIA_REMOVED,
    )[0]
    assert _diagnostics(runtime) == (STORAGE_RESULT_MEDIA_REMOVED, 0, expected_ior)
    assert _execute(runtime, "IOR-STALE?", expected_ior) == (TRUE,)
    _block, volume = _system_objects(runtime)
    assert _execute(runtime, "VOL-STALE?", volume) == (TRUE,)
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert runtime.spinlocks.owner(2) is None


def test_fs_sync_guard_then_publishes_only_bitmap_and_directory() -> None:
    image = _formatted_image(16)
    _write_entry(image, 0, name=b"live.bin\0", used=SECTOR_SIZE)
    runtime = _load_mp64fs_lifecycle(image)
    before_guard = runtime.storage.image_bytes
    _store(runtime, "DISK-IO-STATUS", 0x31)
    _store(runtime, "DISK-IO-COMPLETED", 0x32)
    _store(runtime, "DISK-IO-IOR", 0x33)

    assert _execute(runtime, "FS-SYNC") == ()
    assert runtime.storage.image_bytes == before_guard
    assert runtime.storage.completion == 0
    assert _diagnostics(runtime) == (0x31, 0x32, 0x33)
    assert runtime.drain_uart_output() == b" FS not loaded\r\n"

    assert _execute(runtime, "FS-LOAD") == ()
    assert runtime.drain_uart_output() == b" MP64FS loaded\r\n"
    superblock, bitmap, directory = _cache_addresses(runtime)
    before_sync = runtime.storage.image_bytes
    assert _execute(runtime, "BIT-SET", 15) == ()
    runtime.memory.write8(directory + 33, 0x5A)
    runtime.memory.write_bytes(directory + 36, struct.pack("<I", 0x12345678))
    expected_bitmap = runtime.memory.read_bytes(bitmap, SECTOR_SIZE)
    expected_directory = runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE)
    completion = runtime.storage.completion

    assert _execute(runtime, "FS-SYNC") == ()

    media = runtime.storage.image_bytes
    assert runtime.storage.completion == completion + 3
    assert media[:SECTOR_SIZE] == before_sync[:SECTOR_SIZE]
    assert media[SECTOR_SIZE : 2 * SECTOR_SIZE] == expected_bitmap
    assert media[2 * SECTOR_SIZE : 14 * SECTOR_SIZE] == expected_directory
    assert media[14 * SECTOR_SIZE :] == before_sync[14 * SECTOR_SIZE :]
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == before_sync[
        :SECTOR_SIZE
    ]
    assert _variable(runtime, "FS-OK") == TRUE
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.drain_uart_output() == b""


def test_fs_sync_flush_failure_retains_writes_and_loaded_marker() -> None:
    original = _formatted_image(16)
    storage = HostedStorageService(
        original,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_mp64fs_lifecycle_service(storage)

    assert _execute(runtime, "FS-LOAD") == ()
    assert runtime.drain_uart_output() == b" MP64FS loaded\r\n"
    superblock, bitmap, directory = _cache_addresses(runtime)
    before_sync = storage.image_bytes
    assert _execute(runtime, "BIT-SET", 15) == ()
    runtime.memory.write8(directory + 33, 0x5A)
    runtime.memory.write_bytes(directory + 36, struct.pack("<I", 0x12345678))
    expected_bitmap = runtime.memory.read_bytes(bitmap, SECTOR_SIZE)
    expected_directory = runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE)
    completion = storage.completion
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("FS-SYNC", context=context)

    media = storage.image_bytes
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert storage.completion == completion + 2
    assert media[:SECTOR_SIZE] == before_sync[:SECTOR_SIZE]
    assert media[SECTOR_SIZE : 2 * SECTOR_SIZE] == expected_bitmap
    assert media[2 * SECTOR_SIZE : 14 * SECTOR_SIZE] == expected_directory
    assert media[14 * SECTOR_SIZE :] == before_sync[14 * SECTOR_SIZE :]
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == before_sync[
        :SECTOR_SIZE
    ]
    assert _variable(runtime, "FS-OK") == TRUE
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert runtime.spinlocks.owner(2) is None


def test_format_rejects_small_media_before_cache_or_media_mutation() -> None:
    original = _patterned_image(14)
    runtime = _load_mp64fs_lifecycle(original)
    superblock, bitmap, directory = _cache_addresses(runtime)
    runtime.memory.fill(superblock, SECTOR_SIZE, 0xA1)
    runtime.memory.fill(bitmap, 16 * SECTOR_SIZE, 0xB2)
    runtime.memory.fill(directory, 12 * SECTOR_SIZE, 0xC3)
    _store(runtime, "FS-TOTAL", 0x222)
    _store(runtime, "FS-BMAP-N", 9)
    _store(runtime, "CWD", 27)
    _store(runtime, "FS-OK", TRUE)
    before = _mount_snapshot(runtime)

    assert _execute(runtime, "FORMAT") == ()

    assert _variable(runtime, "FS-OK") == 0
    assert _mount_snapshot(runtime) == before
    assert runtime.storage.image_bytes == original
    assert runtime.storage.completion == 0
    assert runtime.drain_uart_output() == b" Unsupported disk size\r\n"


def test_format_builds_dynamic_metadata_without_touching_data_sectors() -> None:
    original = _patterned_image(4_097)
    runtime = _load_mp64fs_lifecycle(original)
    superblock, bitmap, directory = _cache_addresses(runtime)
    runtime.memory.fill(bitmap, 16 * SECTOR_SIZE, 0xA5)
    _store(runtime, "CWD", 44)

    assert _execute(runtime, "FORMAT") == ()

    expected_superblock = bytes(_superblock(4_097))
    expected_bitmap = _metadata_bitmap(4_097)
    media = runtime.storage.image_bytes
    assert media[:SECTOR_SIZE] == expected_superblock
    assert media[SECTOR_SIZE : 3 * SECTOR_SIZE] == expected_bitmap
    assert media[3 * SECTOR_SIZE : 15 * SECTOR_SIZE] == bytes(
        12 * SECTOR_SIZE
    )
    assert media[15 * SECTOR_SIZE :] == original[15 * SECTOR_SIZE :]
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == expected_superblock
    assert runtime.memory.read_bytes(bitmap, 2 * SECTOR_SIZE) == expected_bitmap
    assert runtime.memory.read_bytes(
        bitmap + 2 * SECTOR_SIZE,
        14 * SECTOR_SIZE,
    ) == bytes((0xA5,)) * (14 * SECTOR_SIZE)
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == bytes(
        12 * SECTOR_SIZE
    )
    assert _variable(runtime, "FS-TOTAL") == 4_097
    assert _variable(runtime, "FS-BMAP-N") == 2
    assert _variable(runtime, "FS-OK") == TRUE
    assert _variable(runtime, "CWD") == 255
    assert _diagnostics(runtime) == (0, 12, 0)
    assert runtime.storage.completion == 4
    assert runtime.drain_uart_output() == b" MP64FS formatted\r\n"


def test_format_flush_failure_retains_written_metadata_but_not_mount_state() -> None:
    original = _patterned_image(15)
    storage = HostedStorageService(
        original,
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_FLUSH,
    )
    runtime = _load_mp64fs_lifecycle_service(storage)
    superblock, bitmap, directory = _cache_addresses(runtime)
    _store(runtime, "CWD", 63)
    context = runtime.new_context()

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("FORMAT", context=context)

    expected_superblock = bytes(_superblock(15))
    expected_bitmap = _metadata_bitmap(15)
    media = storage.image_bytes
    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert media[:SECTOR_SIZE] == expected_superblock
    assert media[SECTOR_SIZE : 2 * SECTOR_SIZE] == expected_bitmap
    assert media[2 * SECTOR_SIZE : 14 * SECTOR_SIZE] == bytes(
        12 * SECTOR_SIZE
    )
    assert media[14 * SECTOR_SIZE :] == original[14 * SECTOR_SIZE :]
    assert runtime.memory.read_bytes(superblock, SECTOR_SIZE) == expected_superblock
    assert runtime.memory.read_bytes(bitmap, SECTOR_SIZE) == expected_bitmap
    assert runtime.memory.read_bytes(directory, 12 * SECTOR_SIZE) == bytes(
        12 * SECTOR_SIZE
    )
    assert _variable(runtime, "FS-TOTAL") == 15
    assert _variable(runtime, "FS-BMAP-N") == 1
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "CWD") == 63
    assert _diagnostics(runtime) == (
        STORAGE_RESULT_UNSUPPORTED,
        12,
        _constant(runtime, "BD-E-UNSUPPORTED"),
    )
    assert storage.completion == 3
    assert runtime.drain_uart_output() == b"Disk flush failed"
    assert runtime.spinlocks.owner(2) is None
