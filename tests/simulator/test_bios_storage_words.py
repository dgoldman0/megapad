"""Focused pseudo-BIOS acceptance for hosted checked block storage."""

from __future__ import annotations

from pathlib import Path

import pytest

from shared.cells import MASK64
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_CAPS,
    STORAGE_CAP_GEN_GUARD,
    STORAGE_CMD_FLUSH,
    STORAGE_CMD_READ,
    STORAGE_RESULT_ADDRESS_OVERFLOW,
    STORAGE_RESULT_DMA_INVALID,
    STORAGE_RESULT_FLUSH_FAILURE,
    STORAGE_RESULT_INVALID_COUNT,
    STORAGE_RESULT_LBA_RANGE,
    STORAGE_RESULT_MEDIA_REMOVED,
    STORAGE_RESULT_NO_MEDIA,
    STORAGE_RESULT_PARTIAL,
    STORAGE_RESULT_TIMEOUT,
    STORAGE_RESULT_UNSUPPORTED,
    STORAGE_RESULT_WRITE_PROTECTED,
    STORAGE_STATUS_ERROR,
    STORAGE_STATUS_MEDIA_CHANGED,
    STORAGE_STATUS_PRESENT,
    STORAGE_STATUS_RESULT_VALID,
    STORAGE_STATUS_WRITE_PROTECTED,
)
from simulator.memory import (
    EXTERNAL_BASE,
    HBW_BASE,
    MMIO_BASE,
    VRAM_BASE,
)
from simulator.platform import create_one_core_address_space
from simulator.runtime import MegaForthRuntime
from simulator.spinlocks import HostedSpinlockBank, SPINLOCK_ACQUIRED
from simulator.stacks import StackUnderflow
from simulator.storage import HostedStorageService


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *inputs: int,
) -> tuple[int, ...]:
    context = runtime.main_context
    assert context.data.snapshot() == ()
    for value in inputs:
        context.data.push(value)
    runtime.execute(name)
    result = context.data.snapshot()
    context.data.clear()
    assert context.returns.snapshot() == ()
    return result


def test_default_runtime_exposes_only_the_checked_absent_storage_profile() -> None:
    runtime = MegaForthRuntime()

    assert _execute(runtime, "DISK@") == (0,)
    assert _execute(runtime, "DISK-SECTORS") == (0,)
    assert _execute(runtime, "DISK-MEDIA-GEN") == (0,)
    assert _execute(runtime, "DISK-CAPS") == (STORAGE_CAPS,)
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 0, 1) == (
        0,
        STORAGE_RESULT_NO_MEDIA,
    )
    assert _execute(runtime, "DISK-FLUSH-CHECKED") == (
        STORAGE_RESULT_NO_MEDIA,
    )
    assert runtime.storage.completion == 0


def test_storage_construction_validates_media_and_has_exclusive_runtime_ownership(
    tmp_path: Path,
) -> None:
    with pytest.raises(ValueError, match="multiple"):
        HostedStorageService(b"short")
    with pytest.raises(ValueError, match="mutually exclusive"):
        HostedStorageService(bytes(SECTOR_SIZE), image_path=tmp_path / "disk.img")
    with pytest.raises(ValueError, match="unsupported bits"):
        HostedStorageService(capabilities=0x80)
    with pytest.raises(ValueError, match="absent storage"):
        HostedStorageService(write_protected=True)
    with pytest.raises(TypeError, match="HostedStorageService"):
        MegaForthRuntime(storage=object())  # type: ignore[arg-type]

    storage = HostedStorageService(bytes(SECTOR_SIZE))
    runtime = MegaForthRuntime(storage=storage)
    assert runtime.storage is storage
    with pytest.raises(ValueError, match="already owned"):
        MegaForthRuntime(storage=storage)

    before = (
        storage.image_bytes,
        storage.image_path,
        storage.status,
        storage.media_generation,
    )
    with pytest.raises(TypeError):
        storage.attach(
            bytes((0xA7,)) * SECTOR_SIZE,
            image_path=object(),  # type: ignore[arg-type]
        )
    assert (
        storage.image_bytes,
        storage.image_path,
        storage.status,
        storage.media_generation,
    ) == before

    reusable_after_failure = HostedStorageService(bytes(SECTOR_SIZE))
    with pytest.raises(ValueError, match="dictionary start address"):
        MegaForthRuntime(
            dictionary_start=0,
            storage=reusable_after_failure,
        )
    reused_runtime = MegaForthRuntime(storage=reusable_after_failure)
    assert reused_runtime.storage is reusable_after_failure


def test_attachment_identity_status_and_terminal_publication_are_persistent() -> None:
    storage = HostedStorageService(bytes((0x31,)) * SECTOR_SIZE)
    runtime = MegaForthRuntime(storage=storage)

    assert storage.media_generation == 1
    assert storage.total_sectors == 1
    assert _execute(runtime, "DISK@") == (
        STORAGE_STATUS_PRESENT | STORAGE_STATUS_MEDIA_CHANGED,
    )
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 0, 1) == (1, 0)
    assert storage.completion == 1
    assert storage.result == 0
    assert storage.transferred == 1

    storage.attach(bytes((0x52,)) * SECTOR_SIZE)
    assert storage.media_generation == 2
    assert storage.completion == 1
    assert storage.transferred == 1
    assert storage.status == (
        STORAGE_STATUS_PRESENT
        | STORAGE_STATUS_MEDIA_CHANGED
        | STORAGE_STATUS_RESULT_VALID
    )

    storage.detach()
    assert storage.media_generation == 3
    assert storage.total_sectors == 0
    assert storage.status == (
        STORAGE_STATUS_MEDIA_CHANGED | STORAGE_STATUS_RESULT_VALID
    )


def test_generation_checked_stack_underflow_leaves_native_inputs_untouched() -> None:
    runtime = MegaForthRuntime(
        storage=HostedStorageService(bytes(SECTOR_SIZE)),
    )
    context = runtime.main_context
    context.data.push(99)

    with pytest.raises(StackUnderflow, match="data stack underflow"):
        runtime.execute("DISK-READ-GEN-CHECKED")

    assert context.data.snapshot() == (99,)
    assert context.returns.snapshot() == ()


@pytest.mark.parametrize(
    "dma",
    (1, EXTERNAL_BASE + 7, VRAM_BASE + 7, HBW_BASE + 7),
)
def test_checked_io_admits_unaligned_spans_in_every_physical_memory_class(
    dma: int,
) -> None:
    memory = create_one_core_address_space(
        external_size=SECTOR_SIZE + 16,
        vram_size=SECTOR_SIZE + 16,
        hbw_size=SECTOR_SIZE + 16,
    )
    original = bytes(index & 0xFF for index in range(SECTOR_SIZE))
    storage = HostedStorageService(original)
    runtime = MegaForthRuntime(memory=memory, storage=storage)

    assert _execute(runtime, "DISK-READ-CHECKED", dma, 0, 1) == (1, 0)
    assert memory.read_bytes(dma, SECTOR_SIZE) == original

    memory.fill(dma, SECTOR_SIZE, 0xA5)
    assert _execute(runtime, "DISK-WRITE-CHECKED", dma, 0, 1) == (1, 0)
    assert storage.image_bytes == bytes((0xA5,)) * SECTOR_SIZE


def test_checked_transfer_splits_at_the_255_sector_controller_boundary() -> None:
    sector_count = 256
    memory = create_one_core_address_space(
        external_size=sector_count * SECTOR_SIZE,
    )
    image = b"".join(
        bytes((sector & 0xFF,)) * SECTOR_SIZE
        for sector in range(sector_count)
    )
    storage = HostedStorageService(image)
    runtime = MegaForthRuntime(memory=memory, storage=storage)

    assert _execute(
        runtime,
        "DISK-READ-GEN-CHECKED",
        EXTERNAL_BASE,
        0,
        sector_count,
        storage.media_generation,
    ) == (sector_count, 0)
    assert memory.read8(EXTERNAL_BASE) == 0
    assert memory.read8(EXTERNAL_BASE + 254 * SECTOR_SIZE) == 254
    assert memory.read8(EXTERNAL_BASE + 255 * SECTOR_SIZE) == 255
    assert storage.completion == 2
    assert storage.transferred == 1


def test_later_chunk_guard_reports_only_confirmed_pre_swap_progress() -> None:
    sector_count = 256
    old_image = (
        bytes((0x31,)) * (255 * SECTOR_SIZE)
        + bytes((0x42,)) * SECTOR_SIZE
    )
    replacement = bytes((0xE5,)) * (sector_count * SECTOR_SIZE)

    class SwapAtSecondReadAcceptanceStorage(HostedStorageService):
        def __init__(self) -> None:
            super().__init__(old_image)
            self.acceptances = 0

        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            assert command == STORAGE_CMD_READ
            assert expected_generation == self.media_generation
            self.acceptances += 1
            if self.acceptances == 2:
                self.attach(replacement)

    memory = create_one_core_address_space(
        external_size=sector_count * SECTOR_SIZE,
    )
    storage = SwapAtSecondReadAcceptanceStorage()
    runtime = MegaForthRuntime(memory=memory, storage=storage)

    assert _execute(
        runtime,
        "DISK-READ-CHECKED",
        EXTERNAL_BASE,
        0,
        sector_count,
    ) == (
        255,
        STORAGE_RESULT_MEDIA_REMOVED | STORAGE_RESULT_PARTIAL,
    )
    assert memory.read8(EXTERNAL_BASE) == 0x31
    assert memory.read8(EXTERNAL_BASE + 254 * SECTOR_SIZE) == 0x31
    assert memory.read8(EXTERNAL_BASE + 255 * SECTOR_SIZE) == 0
    assert storage.image_bytes == replacement
    assert storage.completion == 2
    assert storage.result == STORAGE_RESULT_MEDIA_REMOVED
    assert storage.transferred == 0


def test_checked_validation_priority_and_physical_span_results_are_exact() -> None:
    original = bytes((0x44,)) * (4 * SECTOR_SIZE)
    storage = HostedStorageService(original)
    runtime = MegaForthRuntime(storage=storage)
    generation = storage.media_generation
    stale = generation + 1
    bank0 = runtime.memory.regions[0]
    assert _execute(runtime, "DISK@") == (
        STORAGE_STATUS_PRESENT | STORAGE_STATUS_MEDIA_CHANGED,
    )
    resident_before = runtime.memory.resident_page_count

    assert _execute(
        runtime,
        "DISK-READ-GEN-CHECKED",
        MASK64,
        99,
        0,
        stale,
    ) == (0, STORAGE_RESULT_MEDIA_REMOVED)
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 0, 0) == (
        0,
        STORAGE_RESULT_INVALID_COUNT,
    )
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 4, 1) == (
        0,
        STORAGE_RESULT_LBA_RANGE,
    )
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 3, 2) == (
        0,
        STORAGE_RESULT_LBA_RANGE,
    )
    assert _execute(
        runtime,
        "DISK-READ-CHECKED",
        MASK64 - 255,
        0,
        1,
    ) == (0, STORAGE_RESULT_ADDRESS_OVERFLOW)
    assert _execute(
        runtime,
        "DISK-READ-CHECKED",
        MASK64 - 511,
        0,
        1,
    ) == (0, STORAGE_RESULT_DMA_INVALID)
    assert _execute(runtime, "DISK-READ-CHECKED", MMIO_BASE, 0, 1) == (
        0,
        STORAGE_RESULT_DMA_INVALID,
    )
    assert _execute(
        runtime,
        "DISK-READ-CHECKED",
        bank0.limit - SECTOR_SIZE // 2,
        0,
        1,
    ) == (0, STORAGE_RESULT_DMA_INVALID)

    assert runtime.memory.resident_page_count == resident_before
    assert storage.image_bytes == original
    assert storage.completion == 0
    assert storage.status == (
        STORAGE_STATUS_PRESENT | STORAGE_STATUS_MEDIA_CHANGED
    )

    storage.set_write_protected(True)
    assert _execute(runtime, "DISK-WRITE-CHECKED", MMIO_BASE, 0, 1) == (
        0,
        STORAGE_RESULT_DMA_INVALID,
    )
    assert _execute(runtime, "DISK-WRITE-CHECKED", 0, 0, 1) == (
        0,
        STORAGE_RESULT_WRITE_PROTECTED,
    )
    assert storage.image_bytes == original
    assert storage.completion == 1
    assert storage.result == STORAGE_RESULT_WRITE_PROTECTED
    assert storage.status & (
        STORAGE_STATUS_PRESENT
        | STORAGE_STATUS_WRITE_PROTECTED
        | STORAGE_STATUS_RESULT_VALID
        | STORAGE_STATUS_ERROR
    ) == (
        STORAGE_STATUS_PRESENT
        | STORAGE_STATUS_WRITE_PROTECTED
        | STORAGE_STATUS_RESULT_VALID
        | STORAGE_STATUS_ERROR
    )


def test_no_media_and_missing_capabilities_precede_later_validation() -> None:
    absent = MegaForthRuntime(storage=HostedStorageService(capabilities=0))
    assert _execute(
        absent,
        "DISK-READ-GEN-CHECKED",
        MASK64,
        MASK64,
        0,
        MASK64,
    ) == (0, STORAGE_RESULT_NO_MEDIA)

    storage = HostedStorageService(
        bytes(SECTOR_SIZE),
        capabilities=STORAGE_CAPS & ~STORAGE_CAP_GEN_GUARD,
    )
    unsupported = MegaForthRuntime(storage=storage)
    assert _execute(
        unsupported,
        "DISK-READ-GEN-CHECKED",
        MASK64,
        MASK64,
        0,
        MASK64,
    ) == (0, STORAGE_RESULT_UNSUPPORTED)
    assert _execute(unsupported, "DISK-FLUSH-CHECKED") == (
        STORAGE_RESULT_UNSUPPORTED,
    )
    assert storage.completion == 0


def test_checked_words_use_depthless_filesystem_lock_two() -> None:
    storage = HostedStorageService(bytes((0x73,)) * SECTOR_SIZE)
    runtime = MegaForthRuntime(storage=storage)
    runtime.spinlocks = HostedSpinlockBank(core_count=2)

    assert runtime.spinlocks.acquire(2, 1) == SPINLOCK_ACQUIRED
    context = runtime.main_context
    for value in (77, 0, 0, 1):
        context.data.push(value)
    runtime.execute("DISK-READ-CHECKED")
    assert context.data.snapshot() == (77, 0, STORAGE_RESULT_TIMEOUT)
    assert runtime.spinlocks.owner(2) == 1
    runtime.spinlocks.release(2, 1)
    context.data.clear()

    assert runtime.spinlocks.acquire(2, 0) == SPINLOCK_ACQUIRED
    assert _execute(runtime, "DISK-READ-CHECKED", 0, 0, 1) == (1, 0)
    assert runtime.spinlocks.owner(2) is None


def test_pathless_checked_flush_completes_as_a_semantic_barrier() -> None:
    storage = HostedStorageService(bytes(2 * SECTOR_SIZE))
    runtime = MegaForthRuntime(storage=storage)
    runtime.memory.fill(0, SECTOR_SIZE, 0xBC)

    assert _execute(runtime, "DISK-WRITE-CHECKED", 0, 1, 1) == (1, 0)
    assert storage.image_bytes[SECTOR_SIZE] == 0xBC
    assert _execute(runtime, "DISK-FLUSH-CHECKED") == (0,)
    assert storage.completion == 2
    assert storage.transferred == 0


def test_generation_guard_rejects_a_swap_at_flush_acceptance(
    tmp_path: Path,
) -> None:
    replacement = bytes((0xD7,)) * SECTOR_SIZE
    replacement_path = tmp_path / "replacement.img"

    class SwapAtFlushAcceptanceStorage(HostedStorageService):
        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            assert command == STORAGE_CMD_FLUSH
            assert expected_generation == self.media_generation
            self.attach(replacement, image_path=replacement_path)

    storage = SwapAtFlushAcceptanceStorage(bytes(SECTOR_SIZE))
    runtime = MegaForthRuntime(storage=storage)
    generation = storage.media_generation

    assert _execute(
        runtime,
        "DISK-FLUSH-GEN-CHECKED",
        generation,
    ) == (STORAGE_RESULT_MEDIA_REMOVED,)
    assert storage.image_bytes == replacement
    assert not replacement_path.exists()
    assert storage.completion == 1
    assert storage.result == STORAGE_RESULT_MEDIA_REMOVED
    assert storage.transferred == 0


def test_flush_failure_reports_a_conservative_partial_result(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    storage = HostedStorageService(bytes(SECTOR_SIZE))
    storage.attach(bytes(SECTOR_SIZE), image_path=tmp_path / "unwritten.img")
    runtime = MegaForthRuntime(storage=storage)

    def fail_open(_path: Path, *_args: object, **_kwargs: object) -> None:
        raise OSError("deterministic flush failure")

    monkeypatch.setattr(Path, "open", fail_open)

    expected = STORAGE_RESULT_FLUSH_FAILURE | STORAGE_RESULT_PARTIAL
    assert _execute(
        runtime,
        "DISK-FLUSH-GEN-CHECKED",
        storage.media_generation,
    ) == (expected,)
    assert storage.result == expected
    assert storage.completion == 1
    assert storage.transferred == 0
