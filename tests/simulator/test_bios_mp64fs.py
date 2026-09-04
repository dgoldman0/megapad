"""Focused acceptance for the hosted BIOS MP64FS validator."""

from __future__ import annotations

import struct

import pytest

from shared.mp64fs import (
    MP64FS_DIRECTORY_BYTES,
    MP64FS_ENTRY_SIZE,
    MP64FSGeometry,
    bios_mp64fs_metadata_valid,
    decode_bios_mp64fs_geometry,
)
from shared.storage import SECTOR_SIZE, STORAGE_RESULT_OK
from simulator.memory import SparseAddressSpace
from simulator.runtime import ExecutionContext, MegaForthRuntime
from simulator.spinlocks import HostedSpinlockBank, SPINLOCK_ACQUIRED
from simulator.stacks import ReturnStack
from simulator.storage import HostedStorageService


def _execute(
    runtime: MegaForthRuntime,
    name: str,
    *,
    context: ExecutionContext | None = None,
) -> tuple[int, ...]:
    active_context = runtime.main_context if context is None else context
    assert active_context.data.snapshot() == ()
    runtime.execute(name, context=active_context)
    result = active_context.data.snapshot()
    active_context.data.clear()
    assert active_context.returns.snapshot() == ()
    return result


def _superblock(total_sectors: int) -> bytearray:
    bitmap_sectors = (total_sectors + 4095) // 4096
    directory_start = 1 + bitmap_sectors
    data_start = directory_start + 12
    block = bytearray(SECTOR_SIZE)
    block[0:4] = b"MP64"
    struct.pack_into("<H", block, 4, 1)
    struct.pack_into("<I", block, 6, total_sectors)
    struct.pack_into(
        "<HHHHH",
        block,
        10,
        1,
        bitmap_sectors,
        directory_start,
        12,
        data_start,
    )
    block[20] = 128
    block[21] = 48
    return block


def _formatted_image(total_sectors: int = 15) -> bytearray:
    block = _superblock(total_sectors)
    image = bytearray(total_sectors * SECTOR_SIZE)
    image[:SECTOR_SIZE] = block
    data_start = struct.unpack_from("<H", block, 18)[0]
    for sector in range(data_start):
        _set_allocated(image, sector)
    return image


def _directory_offset(image: bytes | bytearray) -> int:
    return struct.unpack_from("<H", image, 14)[0] * SECTOR_SIZE


def _entry_offset(image: bytes | bytearray, slot: int) -> int:
    return _directory_offset(image) + slot * MP64FS_ENTRY_SIZE


def _set_allocated(image: bytearray, sector: int) -> None:
    image[SECTOR_SIZE + sector // 8] |= 1 << (sector % 8)


def _write_entry(
    image: bytearray,
    slot: int,
    *,
    name: bytes = b"file\0",
    start: int = 14,
    count: int = 1,
    used: int = 0,
    entry_type: int = 1,
    parent: int = 0xFF,
    secondary_start: int = 0,
    secondary_count: int = 0,
) -> int:
    if len(name) > 24:
        raise ValueError("test entry name exceeds its packed field")
    offset = _entry_offset(image, slot)
    entry = bytearray(MP64FS_ENTRY_SIZE)
    entry[: len(name)] = name
    struct.pack_into("<HHI", entry, 24, start, count, used)
    entry[32] = entry_type
    entry[34] = parent
    struct.pack_into("<HH", entry, 44, secondary_start, secondary_count)
    image[offset : offset + MP64FS_ENTRY_SIZE] = entry
    if entry_type != 8:
        for sector in range(start, start + count):
            _set_allocated(image, sector)
        for sector in range(secondary_start, secondary_start + secondary_count):
            _set_allocated(image, sector)
    return offset


def test_geometry_decoder_covers_the_marker1_limits_without_allocating_media() -> None:
    assert decode_bios_mp64fs_geometry(_superblock(15), 15) == MP64FSGeometry(
        total_sectors=15,
        bitmap_sectors=1,
        directory_start=2,
        data_start=14,
    )
    assert decode_bios_mp64fs_geometry(
        _superblock(65_536),
        65_536,
    ) == MP64FSGeometry(
        total_sectors=65_536,
        bitmap_sectors=16,
        directory_start=17,
        data_start=29,
    )
    assert decode_bios_mp64fs_geometry(_superblock(15), 16) is None
    assert decode_bios_mp64fs_geometry(_superblock(65_537), 65_537) is None
    with pytest.raises(ValueError, match="canonical"):
        MP64FSGeometry(15, 0, 1, 13)


def test_metadata_validator_accepts_the_last_sector_and_secondary_extent() -> None:
    geometry = MP64FSGeometry(
        total_sectors=65_536,
        bitmap_sectors=16,
        directory_start=17,
        data_start=29,
    )
    bitmap = bytearray(16 * SECTOR_SIZE)
    for sector in range(geometry.data_start):
        bitmap[sector // 8] |= 1 << (sector % 8)
    bitmap[65_535 // 8] |= 1 << (65_535 % 8)
    directory = bytearray(MP64FS_DIRECTORY_BYTES)
    directory[0:5] = b"last\0"
    struct.pack_into("<HHI", directory, 24, 65_535, 1, 2 * SECTOR_SIZE)
    directory[32] = 1
    directory[34] = 0xFF
    struct.pack_into("<HH", directory, 44, 65_535, 1)

    assert bios_mp64fs_metadata_valid(bitmap, directory, geometry)
    bitmap[65_535 // 8] &= ~(1 << (65_535 % 8)) & 0xFF
    assert not bios_mp64fs_metadata_valid(bitmap, directory, geometry)


def test_validator_absence_headroom_and_lock_fail_before_dma() -> None:
    absent = MegaForthRuntime()
    assert _execute(absent, "MP64FS-VALID?") == (0,)
    assert absent.storage.completion == 0

    image = _formatted_image()
    headroom = MegaForthRuntime(storage=HostedStorageService(image))
    scratch = headroom.main_context.data.empty_pointer
    sentinel = bytes((0xA5,)) * SECTOR_SIZE
    headroom.memory.write_bytes(scratch, sentinel)
    short_stack_context = ExecutionContext(
        returns=ReturnStack(
            memory=headroom.memory,
            floor=scratch,
            empty_pointer=scratch + 4096,
        )
    )
    assert _execute(
        headroom,
        "MP64FS-VALID?",
        context=short_stack_context,
    ) == (0,)
    assert headroom.storage.completion == 0
    assert headroom.memory.read_bytes(scratch, SECTOR_SIZE) == sentinel

    unsupported = MegaForthRuntime(
        storage=HostedStorageService(image, capabilities=0),
    )
    unsupported_scratch = unsupported.main_context.data.empty_pointer
    unsupported.memory.write_bytes(unsupported_scratch, sentinel)
    assert _execute(unsupported, "MP64FS-VALID?") == (0,)
    assert unsupported.storage.completion == 0
    assert unsupported.memory.read_bytes(unsupported_scratch, SECTOR_SIZE) == sentinel

    locked = MegaForthRuntime(storage=HostedStorageService(image))
    locked.spinlocks = HostedSpinlockBank(core_count=2)
    assert locked.spinlocks.acquire(2, 1) == SPINLOCK_ACQUIRED
    assert _execute(locked, "MP64FS-VALID?") == (0,)
    assert locked.storage.completion == 0
    assert locked.spinlocks.owner(2) == 1


def test_validator_scratch_headroom_accepts_only_the_exact_bios_reserve() -> None:
    image = _formatted_image()
    scratch_bytes = MP64FS_DIRECTORY_BYTES + 16 * SECTOR_SIZE
    checked_read_reserve = 128

    accepted = MegaForthRuntime(storage=HostedStorageService(image))
    scratch = accepted.main_context.data.empty_pointer
    exact_context = ExecutionContext(
        returns=ReturnStack(
            memory=accepted.memory,
            floor=scratch,
            empty_pointer=(
                scratch
                + scratch_bytes
                + checked_read_reserve
            ),
        )
    )
    assert _execute(
        accepted,
        "MP64FS-VALID?",
        context=exact_context,
    ) == (1,)
    assert accepted.storage.completion == 3

    rejected = MegaForthRuntime(storage=HostedStorageService(image))
    scratch = rejected.main_context.data.empty_pointer
    one_cell_short = ExecutionContext(
        returns=ReturnStack(
            memory=rejected.memory,
            floor=scratch,
            empty_pointer=(
                scratch
                + scratch_bytes
                + checked_read_reserve
                - 8
            ),
        )
    )
    assert _execute(
        rejected,
        "MP64FS-VALID?",
        context=one_cell_short,
    ) == (0,)
    assert rejected.storage.completion == 0


def test_validator_uses_the_foreground_stack_geometry_for_host_contexts() -> None:
    runtime = MegaForthRuntime(storage=HostedStorageService(_formatted_image()))
    context = runtime.new_context()

    assert _execute(runtime, "MP64FS-VALID?", context=context) == (1,)
    assert runtime.storage.completion == 3


def test_validator_preserves_existing_caller_cells_beneath_its_literal_flag() -> None:
    runtime = MegaForthRuntime(storage=HostedStorageService(_formatted_image()))
    context = runtime.main_context
    context.data.push(0xA55A)

    runtime.execute("MP64FS-VALID?")

    assert context.data.snapshot() == (0xA55A, 1)
    context.data.clear()
    assert context.returns.snapshot() == ()


def test_validator_reads_dynamic_metadata_into_the_native_scratch_layout() -> None:
    image = _formatted_image(4097)
    data_start = struct.unpack_from("<H", image, 18)[0]
    _write_entry(
        image,
        37,
        name=b"payload.bin\0",
        start=data_start,
        used=SECTOR_SIZE,
    )
    before = bytes(image)
    storage = HostedStorageService(image)
    runtime = MegaForthRuntime(storage=storage)
    scratch = runtime.main_context.data.empty_pointer
    runtime.memory.fill(scratch, MP64FS_DIRECTORY_BYTES + 2 * SECTOR_SIZE, 0xA5)

    assert _execute(runtime, "MP64FS-VALID?") == (1,)
    assert storage.completion == 3
    assert storage.result == STORAGE_RESULT_OK
    assert storage.transferred == 12
    assert storage.image_bytes == before
    assert runtime.spinlocks.owner(2) is None
    assert runtime.memory.read_bytes(scratch, MP64FS_DIRECTORY_BYTES) == before[
        3 * SECTOR_SIZE : 15 * SECTOR_SIZE
    ]
    assert runtime.memory.read_bytes(scratch + MP64FS_DIRECTORY_BYTES, 1024) == (
        before[SECTOR_SIZE : 3 * SECTOR_SIZE]
    )


def test_validator_rejects_every_noncanonical_superblock_field_after_one_read() -> None:
    mutations = (
        (0, b"BAD!"),
        (4, struct.pack("<H", 2)),
        (6, struct.pack("<I", 14)),
        (10, struct.pack("<H", 0)),
        (12, struct.pack("<H", 2)),
        (14, struct.pack("<H", 3)),
        (16, struct.pack("<H", 11)),
        (18, struct.pack("<H", 15)),
        (20, bytes((127,))),
        (21, bytes((47,))),
    )
    storage = HostedStorageService(_formatted_image())
    runtime = MegaForthRuntime(storage=storage)

    for offset, payload in mutations:
        image = _formatted_image()
        image[offset : offset + len(payload)] = payload
        storage.attach(image)
        before_completion = storage.completion
        assert _execute(runtime, "MP64FS-VALID?") == (0,)
        assert storage.completion == before_completion + 1
        assert runtime.spinlocks.owner(2) is None


def test_validator_rejects_reserved_bitmap_and_directory_violations() -> None:
    images: list[bytearray] = []

    free_metadata = _formatted_image()
    free_metadata[SECTOR_SIZE] &= 0xFE
    images.append(free_metadata)

    for mutation in (
        "type-zero",
        "type-high",
        "parent-high",
        "parent-empty",
        "primary-zero",
        "primary-metadata",
        "primary-range",
        "primary-free",
        "secondary-start-only",
        "secondary-count-only",
        "used-over-capacity",
        "directory-with-data",
    ):
        image = _formatted_image()
        entry = _write_entry(image, 0)
        if mutation == "type-zero":
            image[entry + 32] = 0
        elif mutation == "type-high":
            image[entry + 32] = 11
        elif mutation == "parent-high":
            image[entry + 34] = 128
        elif mutation == "parent-empty":
            image[entry + 34] = 1
        elif mutation == "primary-zero":
            struct.pack_into("<H", image, entry + 26, 0)
        elif mutation == "primary-metadata":
            struct.pack_into("<H", image, entry + 24, 13)
        elif mutation == "primary-range":
            struct.pack_into("<H", image, entry + 26, 2)
        elif mutation == "primary-free":
            image[SECTOR_SIZE + 14 // 8] &= ~(1 << (14 % 8)) & 0xFF
        elif mutation == "secondary-start-only":
            struct.pack_into("<H", image, entry + 44, 14)
        elif mutation == "secondary-count-only":
            struct.pack_into("<H", image, entry + 46, 1)
        elif mutation == "used-over-capacity":
            struct.pack_into("<I", image, entry + 28, SECTOR_SIZE + 1)
        elif mutation == "directory-with-data":
            image[entry + 32] = 8
        images.append(image)

    non_directory_parent = _formatted_image()
    _write_entry(non_directory_parent, 0, name=b"parent\0")
    _write_entry(non_directory_parent, 1, name=b"child\0", parent=0)
    images.append(non_directory_parent)

    storage = HostedStorageService(_formatted_image())
    runtime = MegaForthRuntime(storage=storage)
    for image in images:
        storage.attach(image)
        before_completion = storage.completion
        assert _execute(runtime, "MP64FS-VALID?") == (0,)
        assert storage.completion == before_completion + 3
        assert runtime.spinlocks.owner(2) is None


def test_validator_preserves_the_bios_omissions_instead_of_strengthening_policy() -> None:
    image = _formatted_image(16)
    image[22:SECTOR_SIZE] = bytes((0xA5,)) * (SECTOR_SIZE - 22)
    _set_allocated(image, 15)
    image[2 * SECTOR_SIZE - 1] = 0xFF
    _write_entry(
        image,
        0,
        name=b"A" * 24,
        start=0,
        count=0,
        entry_type=8,
        parent=1,
    )
    _write_entry(
        image,
        1,
        name=b"B" * 24,
        start=0,
        count=0,
        entry_type=8,
        parent=0,
    )
    live = _write_entry(image, 2, name=b"C" * 24, used=SECTOR_SIZE, parent=0)
    image[live + 33] = 0xFF
    image[live + 35] = 0xA5
    struct.pack_into("<II", image, live + 36, 0xDEADBEEF, 0xCAFEBABE)
    _write_entry(image, 3, name=b"C" * 24, used=SECTOR_SIZE, parent=1)
    ignored = _entry_offset(image, 4)
    image[ignored : ignored + MP64FS_ENTRY_SIZE] = bytes((0,)) + bytes((0xFF,)) * 47

    runtime = MegaForthRuntime(storage=HostedStorageService(image))
    assert _execute(runtime, "MP64FS-VALID?") == (1,)
    assert runtime.storage.completion == 3


def test_validator_rejects_an_inter_read_media_swap_even_when_both_images_validate() -> None:
    original = _formatted_image()
    replacement = _formatted_image()
    _write_entry(replacement, 0, name=b"new.bin\0", used=SECTOR_SIZE)

    class SwapBeforeBitmapReadStorage(HostedStorageService):
        def __init__(self) -> None:
            super().__init__(original)
            self.read_count = 0

        def read_checked(
            self,
            memory: SparseAddressSpace,
            dma: int,
            lba: int,
            count: int,
            *,
            generation: int | None = None,
        ) -> tuple[int, int]:
            self.read_count += 1
            if self.read_count == 2:
                self.attach(replacement)
            return super().read_checked(
                memory,
                dma,
                lba,
                count,
                generation=generation,
            )

    storage = SwapBeforeBitmapReadStorage()
    runtime = MegaForthRuntime(storage=storage)
    assert _execute(runtime, "MP64FS-VALID?") == (0,)
    assert storage.read_count == 3
    assert storage.completion == 3
    assert storage.media_generation == 2
    assert runtime.spinlocks.owner(2) is None
