"""Unchanged-source acceptance for KDOS storage compatibility wrappers."""

from __future__ import annotations

import hashlib
from pathlib import Path

import pytest

from shared.cells import TRUE
from shared.storage import SECTOR_SIZE, STORAGE_CMD_FLUSH
from simulator.errors import ForthAbort
from simulator.runtime import MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_partition_discovery import (
    _evaluate_partition,
    _load_partition,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _define_extent,
    _execute,
    _load_storage,
    _variable,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-storage-compat-4670-4803.f"
)

FIRST_LINE = 4670
LAST_LINE = 4803
SLICE_SHA256 = (
    "7ba6cb19989623363d2e78ac45ae81b1b7e4bb2ad51864005bfbb35b1f768199"
)
SLICE_GIT_BLOB = "70046e73a1d53a562671ac7b196466354fbdd5ab"

DEFINITIONS = (
    b"SYSTEM-BD",
    b"SYSTEM-RAW-VOLUME",
    b"FS-VOLUME",
    b"FS-OK",
    b"STORAGE-OPEN",
    b"FS-VOLUME!",
    b"STORAGE-ENSURE",
    b"DISK-IO-STATUS",
    b"DISK-IO-COMPLETED",
    b"DISK-IO-IOR",
    b"_DISK-XFER-OK?",
    b"_RAW-DISK-READ?",
    b"_RAW-DISK-WRITE?",
    b"_RAW-DISK-FLUSH?",
    b"_DISK-READ?",
    b"_DISK-WRITE?",
    b"_DISK-FLUSH?",
    b"_DISK-READ",
    b"_DISK-WRITE",
    b"_DISK-FLUSH",
    b"B.SECTORS",
    b"B.SAVE",
    b"B.LOAD",
    b"DISK-INFO",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 4_127
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == (
        b"    _PART-LOCK ['] _PART-SCAN CATCH _PART-UNLOCK "
        b"?DUP IF THROW THEN ;\n"
    )
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_storage_compat(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_storage_compat(
    image: bytes | bytearray | None = None,
) -> MegaForthRuntime:
    return _evaluate_storage_compat(_load_partition(image))


def _load_storage_compat_service(
    storage: HostedStorageService,
) -> MegaForthRuntime:
    runtime = MegaForthRuntime(storage=storage)
    return _evaluate_storage_compat(_evaluate_partition(_load_storage(runtime)))


def _patterned_image(sectors: int) -> bytes:
    return b"".join(
        bytes((lba & 0xFF,)) * SECTOR_SIZE for lba in range(sectors)
    )


def _system_objects(runtime: MegaForthRuntime) -> tuple[int, int]:
    return (
        _execute(runtime, "SYSTEM-BD")[0],
        _execute(runtime, "SYSTEM-RAW-VOLUME")[0],
    )


def test_storage_compat_slice_is_exact_and_loads_without_touching_media() -> None:
    runtime = _load_storage_compat()
    block, volume = _system_objects(runtime)

    assert len(DEFINITIONS) == 24
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    # CREATE/ALLOT does not clear these bodies; virgin hosted memory supplies
    # the zero-filled first-construction state required by the object contract.
    assert runtime.memory.read_bytes(block, 128) == bytes(128)
    assert runtime.memory.read_bytes(volume, 144) == bytes(144)
    assert _variable(runtime, "FS-VOLUME") == volume
    for name in (
        "FS-OK",
        "DISK-IO-STATUS",
        "DISK-IO-COMPLETED",
        "DISK-IO-IOR",
    ):
        assert _variable(runtime, name) == 0
    assert runtime.storage.completion == 0
    assert runtime.spinlocks.owner(2) is None
    assert runtime.drain_uart_output() == b""


def test_storage_open_replaces_the_singleton_objects_and_ensure_reuses_them() -> None:
    runtime = _load_storage_compat(_patterned_image(8))
    block, volume = _system_objects(runtime)

    assert _execute(runtime, "STORAGE-OPEN") == (0,)
    assert _execute(runtime, "BD-VALID?", block) == (TRUE,)
    assert _execute(runtime, "VOL-VALID?", volume) == (TRUE,)
    assert _variable(runtime, "FS-VOLUME") == volume
    assert runtime.memory.read64(block + 88) == 1
    first_block_cookie = _execute(runtime, "BD.COOKIE", block)[0]
    first_volume_cookie = _execute(runtime, "VOL.COOKIE", volume)[0]

    assert _execute(runtime, "STORAGE-ENSURE") == (0,)
    assert _execute(runtime, "BD.COOKIE", block) == (first_block_cookie,)
    assert _execute(runtime, "VOL.COOKIE", volume) == (first_volume_cookie,)
    fs_ok = _execute(runtime, "FS-OK")[0]
    runtime.memory.write64(fs_ok, TRUE)
    assert _execute(runtime, "STORAGE-OPEN") == (0,)
    assert _execute(runtime, "BD.COOKIE", block)[0] != first_block_cookie
    assert _execute(runtime, "VOL.COOKIE", volume)[0] != first_volume_cookie
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.memory.read64(block + 88) == 1

    assert _execute(runtime, "VOL-CLOSE", volume) == (0,)
    block_cookie = _execute(runtime, "BD.COOKIE", block)[0]
    assert _execute(runtime, "STORAGE-ENSURE") == (
        _constant(runtime, "VOL-E-STALE"),
    )
    assert _variable(runtime, "FS-OK") == 0
    assert _execute(runtime, "BD.COOKIE", block) == (block_cookie,)
    assert _execute(runtime, "VOL-VALID?", volume) == (0,)
    assert _execute(runtime, "STORAGE-ENSURE") == (0,)
    assert _execute(runtime, "VOL-VALID?", volume) == (TRUE,)

    runtime.storage.attach(bytes((0xE1,)) * (8 * SECTOR_SIZE))
    assert _execute(runtime, "STORAGE-ENSURE") == (
        _constant(runtime, "VOL-E-STALE"),
    )
    assert _variable(runtime, "FS-OK") == 0
    stale_cookie = _execute(runtime, "VOL.COOKIE", volume)[0]
    assert _execute(runtime, "STORAGE-ENSURE") == (
        _constant(runtime, "VOL-E-STALE"),
    )
    assert _execute(runtime, "VOL.COOKIE", volume) == (stale_cookie,)
    assert _execute(runtime, "STORAGE-OPEN") == (0,)
    assert _execute(runtime, "VOL-STALE?", volume) == (0,)
    assert runtime.memory.read64(block + 88) == 1


def test_storage_open_failure_does_not_restore_the_old_raw_binding() -> None:
    runtime = _load_storage_compat(_patterned_image(8))
    block, system_volume = _system_objects(runtime)
    other_volume = _define_extent(runtime, "OTHER-LIVE-VOLUME", 144)
    assert _execute(runtime, "STORAGE-OPEN") == (0,)
    assert _execute(
        runtime,
        "VOL-SLICE",
        1,
        2,
        1,
        0,
        block,
        other_volume,
    ) == (0,)
    block_cookie = _execute(runtime, "BD.COOKIE", block)[0]
    fs_ok = _execute(runtime, "FS-OK")[0]
    runtime.memory.write64(fs_ok, TRUE)

    assert _execute(runtime, "STORAGE-OPEN") == (
        _constant(runtime, "BD-E-BUSY"),
    )
    assert _execute(runtime, "VOL-VALID?", system_volume) == (0,)
    assert _execute(runtime, "VOL-VALID?", other_volume) == (TRUE,)
    assert _execute(runtime, "BD.COOKIE", block) == (block_cookie,)
    assert runtime.memory.read64(block + 88) == 1
    assert _variable(runtime, "FS-VOLUME") == system_volume
    assert _variable(runtime, "FS-OK") == TRUE


def test_fs_volume_selection_validates_without_taking_an_extra_reference() -> None:
    runtime = _load_storage_compat(_patterned_image(8))
    block, system_volume = _system_objects(runtime)
    custom_volume = _define_extent(runtime, "CUSTOM-VOLUME", 144)
    invalid_volume = _define_extent(runtime, "INVALID-VOLUME", 144)
    dma = _define_extent(runtime, "CUSTOM-VOLUME-DMA", SECTOR_SIZE)
    assert _execute(runtime, "STORAGE-OPEN") == (0,)
    assert _execute(
        runtime,
        "VOL-SLICE",
        2,
        3,
        1,
        7,
        block,
        custom_volume,
    ) == (0,)
    assert runtime.memory.read64(block + 88) == 2
    fs_ok = _execute(runtime, "FS-OK")[0]
    runtime.memory.write64(fs_ok, TRUE)

    assert _execute(runtime, "FS-VOLUME!", custom_volume) == (0,)
    assert _variable(runtime, "FS-VOLUME") == custom_volume
    assert _variable(runtime, "FS-OK") == 0
    assert runtime.memory.read64(block + 88) == 2
    assert _execute(runtime, "_DISK-READ?", dma, 0, 1) == (TRUE,)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == (
        bytes((2,)) * SECTOR_SIZE
    )
    runtime.memory.write64(fs_ok, TRUE)
    assert _execute(runtime, "FS-VOLUME!", invalid_volume) == (
        _constant(runtime, "VOL-E-BAD-DESCRIPTOR"),
    )
    assert _variable(runtime, "FS-VOLUME") == custom_volume
    assert _variable(runtime, "FS-OK") == TRUE

    runtime.storage.attach(bytes((0xD2,)) * (8 * SECTOR_SIZE))
    assert _execute(runtime, "FS-VOLUME!", custom_volume) == (
        _constant(runtime, "VOL-E-STALE"),
    )
    assert _variable(runtime, "FS-VOLUME") == custom_volume
    assert _variable(runtime, "FS-OK") == TRUE
    assert runtime.memory.read64(block + 88) == 2
    assert _execute(runtime, "VOL-VALID?", system_volume) == (TRUE,)


def test_raw_checked_wrappers_transfer_and_retain_exact_diagnostics() -> None:
    runtime = _load_storage_compat(_patterned_image(4))
    dma = _define_extent(runtime, "RAW-COMPAT-DMA", SECTOR_SIZE)

    assert _execute(runtime, "_RAW-DISK-READ?", dma, 1, 1) == (TRUE,)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == bytes((1,)) * SECTOR_SIZE
    assert _variable(runtime, "DISK-IO-STATUS") == 0
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") == 0

    runtime.memory.fill(dma, SECTOR_SIZE, 0xA6)
    assert _execute(runtime, "_RAW-DISK-WRITE?", dma, 2, 1) == (TRUE,)
    assert runtime.storage.image_bytes[
        2 * SECTOR_SIZE : 3 * SECTOR_SIZE
    ] == bytes((0xA6,)) * SECTOR_SIZE
    assert _execute(runtime, "_RAW-DISK-FLUSH?") == (TRUE,)
    assert _variable(runtime, "DISK-IO-STATUS") == 0
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") == 0

    fs_ok = _execute(runtime, "FS-OK")[0]
    runtime.memory.write64(fs_ok, TRUE)
    runtime.storage.detach()
    assert _execute(runtime, "_RAW-DISK-FLUSH?") == (0,)
    assert _variable(runtime, "DISK-IO-STATUS") == 1
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") != 0
    assert _variable(runtime, "FS-OK") == TRUE


def test_transfer_helper_converts_a_short_success_to_an_internal_error() -> None:
    runtime = _load_storage_compat()

    assert _execute(runtime, "_DISK-XFER-OK?", 2, 1, 0) == (0,)
    assert _variable(runtime, "DISK-IO-STATUS") == 14
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "BD-E-INTERNAL",
    )


def test_stale_flush_clears_cache_only_for_the_selected_volume_path() -> None:
    replacement = bytes((0xE8,)) * (2 * SECTOR_SIZE)

    class SwapAtFlushAcceptanceStorage(HostedStorageService):
        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            if command != STORAGE_CMD_FLUSH:
                return
            assert expected_generation == self.media_generation
            self.attach(replacement)

    raw_storage = SwapAtFlushAcceptanceStorage(bytes(2 * SECTOR_SIZE))
    raw = _load_storage_compat_service(raw_storage)
    raw_dma = _define_extent(raw, "RAW-STALE-FLUSH-DMA", SECTOR_SIZE)
    assert _execute(raw, "_RAW-DISK-READ?", raw_dma, 0, 1) == (TRUE,)
    raw_fs_ok = _execute(raw, "FS-OK")[0]
    raw.memory.write64(raw_fs_ok, TRUE)

    assert _execute(raw, "_RAW-DISK-FLUSH?") == (0,)
    assert _variable(raw, "DISK-IO-STATUS") == 11
    assert _variable(raw, "DISK-IO-COMPLETED") == 1
    assert _execute(raw, "IOR-STALE?", _variable(raw, "DISK-IO-IOR")) == (
        TRUE,
    )
    assert _variable(raw, "FS-OK") == TRUE
    assert raw_storage.completion == 2

    selected_storage = SwapAtFlushAcceptanceStorage(bytes(2 * SECTOR_SIZE))
    selected = _load_storage_compat_service(selected_storage)
    selected_dma = _define_extent(
        selected,
        "SELECTED-STALE-FLUSH-DMA",
        SECTOR_SIZE,
    )
    assert _execute(selected, "_DISK-READ?", selected_dma, 0, 1) == (TRUE,)
    selected_fs_ok = _execute(selected, "FS-OK")[0]
    selected.memory.write64(selected_fs_ok, TRUE)

    assert _execute(selected, "_DISK-FLUSH?") == (0,)
    assert _variable(selected, "DISK-IO-STATUS") == 11
    assert _variable(selected, "DISK-IO-COMPLETED") == 1
    assert _execute(
        selected,
        "IOR-STALE?",
        _variable(selected, "DISK-IO-IOR"),
    ) == (TRUE,)
    assert _variable(selected, "FS-OK") == 0
    assert selected_storage.completion == 2


def test_volume_checked_wrapper_autobinds_then_reports_a_stale_binding() -> None:
    runtime = _load_storage_compat(_patterned_image(4))
    dma = _define_extent(runtime, "VOLUME-COMPAT-DMA", SECTOR_SIZE)
    block, volume = _system_objects(runtime)

    assert _execute(runtime, "_DISK-READ?", dma, 3, 1) == (TRUE,)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == bytes((3,)) * SECTOR_SIZE
    assert _execute(runtime, "BD-VALID?", block) == (TRUE,)
    assert _execute(runtime, "VOL-VALID?", volume) == (TRUE,)
    assert _execute(runtime, "_DISK-FLUSH?") == (TRUE,)
    assert _variable(runtime, "DISK-IO-STATUS") == 0
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") == 0
    fs_ok = _execute(runtime, "FS-OK")[0]
    runtime.memory.write64(fs_ok, TRUE)

    runtime.storage.attach(bytes((0xC7,)) * (4 * SECTOR_SIZE))
    assert _execute(runtime, "_DISK-FLUSH?") == (0,)
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "DISK-IO-COMPLETED") == 1
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-STALE",
    )
    assert _variable(runtime, "DISK-IO-STATUS") == 11
    runtime.memory.fill(dma, SECTOR_SIZE, 0x5A)
    assert _execute(runtime, "_DISK-READ?", dma, 0, 1) == (0,)
    assert runtime.memory.read_bytes(dma, SECTOR_SIZE) == bytes((0x5A,)) * SECTOR_SIZE
    assert _variable(runtime, "FS-OK") == 0
    assert _variable(runtime, "DISK-IO-COMPLETED") == 0
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-STALE",
    )
    assert _variable(runtime, "DISK-IO-STATUS") == 11
    assert runtime.memory.read64(block + 88) == 1


def test_aborting_compatibility_wrapper_keeps_failure_diagnostics() -> None:
    runtime = _load_storage_compat(_patterned_image(2))
    dma = _define_extent(runtime, "ABORT-COMPAT-DMA", SECTOR_SIZE)
    context = runtime.new_context()
    for value in (dma, 2, 1):
        context.data.push(value)

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        runtime.execute("_DISK-READ", context=context)

    assert context.data.snapshot() == ()
    assert context.returns.snapshot() == ()
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert _variable(runtime, "DISK-IO-COMPLETED") == 0
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-RANGE",
    )


def test_buffer_save_and_load_use_safe_full_sector_payloads_without_flushing() -> None:
    runtime = _load_storage_compat(_patterned_image(8))
    runtime.evaluate(
        b"0 1 0 BUFFER ZERO-BUFFER "
        b"0 1 1 BUFFER ONE-BUFFER "
        b"0 1 511 BUFFER TAIL-BUFFER "
        b"0 1 512 BUFFER SECTOR-BUFFER "
        b"0 1 513 BUFFER ROUNDED-BUFFER",
        source_name="storage-compat-buffers",
    )
    zero = _execute(runtime, "ZERO-BUFFER")[0]
    one = _execute(runtime, "ONE-BUFFER")[0]
    tail = _execute(runtime, "TAIL-BUFFER")[0]
    descriptor = _execute(runtime, "SECTOR-BUFFER")[0]
    rounded = _execute(runtime, "ROUNDED-BUFFER")[0]
    data = _execute(runtime, "B.DATA", descriptor)[0]
    assert _execute(runtime, "B.SECTORS", zero) == (0,)
    assert _execute(runtime, "B.SECTORS", one) == (1,)
    assert _execute(runtime, "B.SECTORS", tail) == (1,)
    assert _execute(runtime, "B.SECTORS", descriptor) == (1,)
    assert _execute(runtime, "B.SECTORS", rounded) == (2,)
    runtime.memory.fill(data, SECTOR_SIZE, 0xB4)

    before_save = runtime.storage.completion
    assert _execute(runtime, "B.SAVE", descriptor, 4) == ()
    assert runtime.storage.completion == before_save + 1
    assert runtime.storage.image_bytes[
        4 * SECTOR_SIZE : 5 * SECTOR_SIZE
    ] == bytes((0xB4,)) * SECTOR_SIZE
    assert _execute(runtime, "_DISK-FLUSH") == ()
    assert runtime.storage.completion == before_save + 2

    runtime.memory.fill(data, SECTOR_SIZE, 0)
    assert _execute(runtime, "B.LOAD", descriptor, 4) == ()
    assert runtime.memory.read_bytes(data, SECTOR_SIZE) == bytes((0xB4,)) * SECTOR_SIZE
    assert runtime.storage.completion == before_save + 3
    assert _variable(runtime, "DISK-IO-IOR") == 0


def test_zero_sized_buffer_save_and_load_abort_before_disk_io() -> None:
    runtime = _load_storage_compat(_patterned_image(2))
    runtime.evaluate(
        b"0 1 0 BUFFER EMPTY-DISK-BUFFER",
        source_name="empty-storage-compat-buffer",
    )
    descriptor = _execute(runtime, "EMPTY-DISK-BUFFER")[0]

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        _execute(runtime, "B.SAVE", descriptor, 0)
    assert runtime.drain_uart_output() == b"Disk write failed"
    assert runtime.storage.completion == 0
    assert _variable(runtime, "DISK-IO-STATUS") == 4
    assert _variable(runtime, "DISK-IO-COMPLETED") == 0
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-RANGE",
    )

    with pytest.raises(ForthAbort, match='Forth ABORT"'):
        _execute(runtime, "B.LOAD", descriptor, 0)
    assert runtime.drain_uart_output() == b"Disk read failed"
    assert runtime.storage.completion == 0
    assert _variable(runtime, "DISK-IO-STATUS") == 4
    assert _variable(runtime, "DISK-IO-COMPLETED") == 0
    assert _variable(runtime, "DISK-IO-IOR") == _constant(
        runtime,
        "VOL-E-RANGE",
    )


def test_disk_info_reports_the_current_attachment_without_opening_it() -> None:
    absent = _load_storage_compat()
    absent_block, absent_volume = _system_objects(absent)
    assert _execute(absent, "DISK-INFO") == ()
    assert absent.drain_uart_output() == b" Storage:  not attached\r\n"
    assert absent.memory.read_bytes(absent_block, 128) == bytes(128)
    assert absent.memory.read_bytes(absent_volume, 144) == bytes(144)

    present = _load_storage_compat(_patterned_image(1))
    present_block, present_volume = _system_objects(present)
    assert _execute(present, "DISK-INFO") == ()
    assert present.drain_uart_output() == b" Storage:  present\r\n"
    assert present.memory.read_bytes(present_block, 128) == bytes(128)
    assert present.memory.read_bytes(present_volume, 144) == bytes(144)
