"""Unchanged-source acceptance for KDOS raw, MBR, and GPT discovery."""

from __future__ import annotations

import binascii
import hashlib
import struct
from collections.abc import Sequence
from pathlib import Path

import pytest

from shared.cells import TRUE
from shared.storage import SECTOR_SIZE, STORAGE_CMD_READ
from simulator.memory import SparseAddressSpace
from simulator.platform import create_one_core_address_space
from simulator.runtime import CreatedDefinition, MegaForthRuntime
from simulator.storage import HostedStorageService
from tests.simulator.test_kdos_aes import (
    KDOS_GIT_BLOB,
    MEGAPAD_REVISION,
    _git_blob_id,
)
from tests.simulator.test_kdos_storage_block_volume import (
    _constant,
    _define_extent,
    _execute,
    _load_storage,
    _runtime_with_media,
)


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
KDOS_SOURCE = REPOSITORY_ROOT / "kdos.f"
FIXTURE = (
    Path(__file__).with_name("fixtures")
    / "kdos-partition-discovery-4100-4669.f"
)

FIRST_LINE = 4100
LAST_LINE = 4669
SLICE_SHA256 = (
    "bf46ad3acc9deaf380ac4229fe9196219fc0111df8d8f5a6650ffa95fb766112"
)
SLICE_GIT_BLOB = "b04840a4e3c9c660a4949d56722154eedaee7f76"

DEFINITIONS = (
    b"PART-WORKSPACE-MIN",
    b"GPT-MAX-ENTRIES",
    b"GPT-MAX-ENTRY-SIZE",
    b"PART-E-CORRUPT",
    b"PART-E-CAPACITY",
    b"PART-E-WORKSPACE",
    b"PART-E-UNSUPPORTED",
    b"PART-E-CRC-UNSUPPORTED",
    b"PART-E-CRC-BUSY",
    b"PART-E-BAD-DESCRIPTOR",
    b"_GPT-CRC-STATUS>IOR",
    b"_PART-BD",
    b"_PART-OUT",
    b"_PART-MAX",
    b"_PART-WS",
    b"_PART-BYTES",
    b"_PART-COUNT",
    b"PART-VOLUME",
    b"_PART-CLEAR",
    b"_PART-SETUP",
    b"_PART-FAIL",
    b"_PART-READ",
    b"_PART-FINALIZE",
    b"_OV-A",
    b"_OV-ALEN",
    b"_OV-B",
    b"_OV-BLEN",
    b"_RANGES-OVERLAP?",
    b"_MBR-ENTRY",
    b"_MBR-TYPE",
    b"_MBR-BASE",
    b"_MBR-LEN",
    b"_MBR-EXTENDED?",
    b"_MBR-E",
    b"_MBR-TYPE-V",
    b"_MBR-BASE-V",
    b"_MBR-LEN-V",
    b"_MBR-BOOT-V",
    b"_MBR-INDEX-V",
    b"_MBR-CANDIDATE-OVERLAP?",
    b"_MBR-STAGE",
    b"_MBR-PROTECTIVE-VALID?",
    b"_MBR-SCAN",
    b"_CRC32-IEEE-RAW?",
    b"_CRC32-IEEE-CHECKED",
    b"CRC32-IEEE-BUF",
    b"_GUID-ZERO?",
    b"_GUID-SAME?",
    b"_BYTES-ZERO?",
    b"_GH-BUF",
    b"_GH-CUR",
    b"_GH-BACK",
    b"_GH-SAVED-CRC",
    b"_GPT-CRC-IOR",
    b"_GPT-HEADER-CRC?",
    b"_GPT-HEADER-VALID?",
    b"_GPT-FIRST",
    b"_GPT-LAST",
    b"_GPT-PARRAY",
    b"_GPT-BARRAY",
    b"_GPT-NENT",
    b"_GPT-ESIZE",
    b"_GPT-ACRC",
    b"_GPT-GUID0",
    b"_GPT-GUID1",
    b"_GPT-HSIZE",
    b"_GPT-ARRAY-BYTES",
    b"_GPT-ARRAY-SECTORS",
    b"_GPT-SAVE-PRIMARY",
    b"_GPT-HEADERS-AGREE?",
    b"_GPT-METADATA-VALID?",
    b"_GP-USED",
    b"_GP-OK",
    b"_GPT-PROTECTIVE-MBR?",
    b"_GA-LBA",
    b"_GA-REM",
    b"_GA-CHUNK",
    b"_GA-EXPECTED",
    b"_GA-RAW",
    b"_GPT-ARRAY-CRC?",
    b"_GPT-ARRAYS-AGREE?",
    b"_GE-INDEX",
    b"_GE-ARRAY",
    b"_GE-SECTOR",
    b"_GE-INTRA",
    b"_GPT-READ-ENTRY",
    b"_GPE-TYPE0",
    b"_GPE-TYPE1",
    b"_GPE-UNIQ0",
    b"_GPE-UNIQ1",
    b"_GPE-FIRST",
    b"_GPE-LAST",
    b"_GPE-LEN",
    b"_GPE-ATTR",
    b"_GPE-INDEX",
    b"_GPC-CONFLICT",
    b"_GPC-IOR",
    b"_GPC-P",
    b"_GPT-PRIOR-CONFLICT?",
    b"_GPT-STAGE-ENTRY",
    b"_GPT-SCAN",
    b"_PS-USED",
    b"_PS-PROTECTIVE",
    b"_PART-RAW-RESULT",
    b"_PART-SCAN",
    b"_PART-LOCK",
    b"_PART-UNLOCK",
    b"MBR-SCAN",
    b"GPT-SCAN",
    b"PART-SCAN",
)


def _verified_slice() -> bytes:
    source = FIXTURE.read_bytes()
    assert len(source) == 18_979
    assert source.count(b"\n") == LAST_LINE - FIRST_LINE + 1
    assert hashlib.sha256(source).hexdigest() == SLICE_SHA256
    assert _git_blob_id(source) == SLICE_GIT_BLOB

    complete_kdos = KDOS_SOURCE.read_bytes()
    assert _git_blob_id(complete_kdos) == KDOS_GIT_BLOB
    lines = complete_kdos.splitlines(keepends=True)
    assert lines[FIRST_LINE - 2] == b"    VOL.BD BD-FLUSH ;\n"
    assert source == b"".join(lines[FIRST_LINE - 1 : LAST_LINE])
    assert lines[LAST_LINE] == b"\n"
    return source


def _evaluate_partition(runtime: MegaForthRuntime) -> MegaForthRuntime:
    result = runtime.evaluate(
        _verified_slice(),
        source_name=f"kdos.f@{MEGAPAD_REVISION}:{FIRST_LINE}-{LAST_LINE}",
    )
    assert tuple(word.name for word in result.definitions) == DEFINITIONS
    assert runtime.main_context.data.snapshot() == ()
    assert runtime.main_context.returns.snapshot() == ()
    return runtime


def _load_partition(
    image: bytes | bytearray | None = None,
    *,
    memory: SparseAddressSpace | None = None,
) -> MegaForthRuntime:
    if image is None:
        return _evaluate_partition(_load_storage())
    if memory is None:
        return _evaluate_partition(_runtime_with_media(bytes(image)))
    runtime = MegaForthRuntime(
        memory=memory,
        storage=HostedStorageService(bytes(image)),
    )
    return _evaluate_partition(_load_storage(runtime))


def _patterned_image(sectors: int) -> bytearray:
    image = bytearray(sectors * SECTOR_SIZE)
    for lba in range(sectors):
        start = lba * SECTOR_SIZE
        image[start : start + SECTOR_SIZE] = bytes((lba & 0xFF,)) * SECTOR_SIZE
    return image


def _put_mbr_entry(
    sector: bytearray,
    index: int,
    part_type: int,
    first_lba: int,
    sectors: int,
    *,
    bootable: bool = False,
) -> None:
    offset = 446 + index * 16
    sector[offset] = 0x80 if bootable else 0
    sector[offset + 1 : offset + 4] = b"\x00\x02\x00"
    sector[offset + 4] = part_type
    sector[offset + 5 : offset + 8] = b"\xFE\xFF\xFF"
    struct.pack_into("<II", sector, offset + 8, first_lba, sectors)


def _mbr_image(
    total_sectors: int = 256,
    entries: Sequence[tuple[int, int, int] | None] = (
        None,
        (0x83, 8, 16),
        None,
        (0x0C, 40, 24),
    ),
) -> bytearray:
    image = _patterned_image(total_sectors)
    mbr = bytearray(SECTOR_SIZE)
    for index, entry in enumerate(entries):
        if entry is None:
            continue
        part_type, first_lba, sectors = entry
        _put_mbr_entry(
            mbr,
            index,
            part_type,
            first_lba,
            sectors,
            bootable=index == 0,
        )
    mbr[510:512] = b"\x55\xAA"
    image[:SECTOR_SIZE] = mbr
    return image


def _protective_mbr(total_sectors: int) -> bytes:
    mbr = bytearray(SECTOR_SIZE)
    _put_mbr_entry(
        mbr,
        0,
        0xEE,
        1,
        min(total_sectors - 1, 0xFFFF_FFFF),
    )
    mbr[510:512] = b"\x55\xAA"
    return bytes(mbr)


def _gpt_header(
    *,
    current_lba: int,
    backup_lba: int,
    first_usable: int,
    last_usable: int,
    disk_guid: bytes,
    entries_lba: int,
    entry_count: int,
    entry_size: int,
    entries_crc: int,
) -> bytes:
    header = bytearray(SECTOR_SIZE)
    struct.pack_into(
        "<8sIIIIQQQQ16sQIII",
        header,
        0,
        b"EFI PART",
        0x0001_0000,
        92,
        0,
        0,
        current_lba,
        backup_lba,
        first_usable,
        last_usable,
        disk_guid,
        entries_lba,
        entry_count,
        entry_size,
        entries_crc,
    )
    crc = binascii.crc32(header[:92]) & 0xFFFF_FFFF
    struct.pack_into("<I", header, 16, crc)
    return bytes(header)


def _gpt_image(
    total_sectors: int = 256,
    partitions: Sequence[tuple[int, int]] = ((40, 55), (80, 111)),
    *,
    entry_count: int = 32,
    entry_size: int = 128,
    partition_indices: Sequence[int] | None = None,
) -> bytearray:
    if partition_indices is None:
        partition_indices = tuple(range(len(partitions)))
    if len(partition_indices) != len(partitions):
        raise ValueError("partition indices must match the partition list")
    table_bytes = entry_count * entry_size
    table_sectors = (table_bytes + SECTOR_SIZE - 1) // SECTOR_SIZE
    primary_entries_lba = 2
    backup_header_lba = total_sectors - 1
    backup_entries_lba = backup_header_lba - table_sectors
    first_usable = max(34, primary_entries_lba + table_sectors)
    last_usable = min(total_sectors - 34, backup_entries_lba - 1)

    image = _patterned_image(total_sectors)
    image[:SECTOR_SIZE] = _protective_mbr(total_sectors)
    entries = bytearray(table_bytes)
    type_guids = (
        bytes.fromhex("A2A0D0EBE5B9334487C068B6B72699C7"),
        bytes.fromhex("AF3DC60F838472478E793D69D8477DE4"),
    )
    unique_guids = (
        bytes.fromhex("00112233445566778899AABBCCDDEEFF"),
        bytes.fromhex("102132435465768798A9BACBDCEDFE0F"),
    )
    for ordinal, (index, partition) in enumerate(
        zip(partition_indices, partitions, strict=True)
    ):
        first_lba, last_lba = partition
        offset = index * entry_size
        entries[offset : offset + 16] = type_guids[
            ordinal % len(type_guids)
        ]
        entries[offset + 16 : offset + 32] = unique_guids[
            ordinal % len(unique_guids)
        ]
        struct.pack_into("<QQQ", entries, offset + 32, first_lba, last_lba, 0)

    entries_crc = binascii.crc32(entries) & 0xFFFF_FFFF
    disk_guid = bytes.fromhex("78563412BC9AF0DE1122334455667788")
    primary_header = _gpt_header(
        current_lba=1,
        backup_lba=backup_header_lba,
        first_usable=first_usable,
        last_usable=last_usable,
        disk_guid=disk_guid,
        entries_lba=primary_entries_lba,
        entry_count=entry_count,
        entry_size=entry_size,
        entries_crc=entries_crc,
    )
    backup_header = _gpt_header(
        current_lba=backup_header_lba,
        backup_lba=1,
        first_usable=first_usable,
        last_usable=last_usable,
        disk_guid=disk_guid,
        entries_lba=backup_entries_lba,
        entry_count=entry_count,
        entry_size=entry_size,
        entries_crc=entries_crc,
    )
    primary_table = primary_entries_lba * SECTOR_SIZE
    backup_table = backup_entries_lba * SECTOR_SIZE
    image[primary_table : primary_table + table_bytes] = entries
    image[backup_table : backup_table + table_bytes] = entries
    image[SECTOR_SIZE : 2 * SECTOR_SIZE] = primary_header
    backup_header_offset = backup_header_lba * SECTOR_SIZE
    image[backup_header_offset : backup_header_offset + SECTOR_SIZE] = (
        backup_header
    )
    return image


def _scan_extents(
    runtime: MegaForthRuntime,
    *,
    volume_slots: int = 4,
) -> tuple[int, int, int]:
    block = _define_extent(runtime, "PART-BD", 128)
    volumes = _define_extent(runtime, "PART-VOLUMES", volume_slots * 144)
    workspace = _define_extent(runtime, "PART-WORK", 5120)
    assert _execute(runtime, "BD-OPEN", block) == (0,)
    return block, volumes, workspace


def _volume_fields(
    runtime: MegaForthRuntime,
    volume: int,
) -> tuple[int, int, int, int, int]:
    return (
        _execute(runtime, "VOL.BASE", volume)[0],
        _execute(runtime, "VOL.SECTORS", volume)[0],
        _execute(runtime, "VOL.SCHEME", volume)[0],
        _execute(runtime, "VOL.INDEX", volume)[0],
        _execute(runtime, "VOL-VALID?", volume)[0],
    )


def test_partition_slice_is_exact_and_loads_without_runtime_side_effects() -> None:
    runtime = _load_partition()

    assert len(DEFINITIONS) == 110
    assert all(runtime.find(name) is not None for name in DEFINITIONS)
    variables = tuple(
        word
        for name in DEFINITIONS
        if (word := runtime.find(name)) is not None
        and isinstance(word.implementation, CreatedDefinition)
    )
    assert len(variables) == 58
    assert all(
        runtime.memory.read64(word.body_address) == 0 for word in variables
    )
    assert _constant(runtime, "PART-WORKSPACE-MIN") == 5120
    assert _constant(runtime, "GPT-MAX-ENTRIES") == 4096
    assert _constant(runtime, "GPT-MAX-ENTRY-SIZE") == 4096
    assert _constant(runtime, "PART-E-CORRUPT") == 0x0804_1409
    assert _constant(runtime, "PART-E-CAPACITY") == 0x0004_1504
    assert _constant(runtime, "PART-E-WORKSPACE") == 0x0004_160E
    assert runtime.storage.completion == 0
    assert runtime.crc.owner is None
    assert runtime.spinlocks.owner(0) is None
    assert runtime.drain_uart_output() == b""


def test_part_scan_raw_fallback_publishes_one_identity_volume() -> None:
    runtime = _load_partition(_patterned_image(20))
    block, volumes, workspace = _scan_extents(runtime)

    assert _execute(
        runtime,
        "PART-SCAN",
        block,
        volumes,
        4,
        workspace,
        5120,
    ) == (1, 0)
    assert _volume_fields(runtime, volumes) == (0, 20, 0, 0, TRUE)
    assert runtime.memory.read64(block + 88) == 1
    assert runtime.spinlocks.owner(0) is None


def test_mbr_and_dispatch_scans_publish_bounded_source_volumes() -> None:
    runtime = _load_partition(_mbr_image())
    block, volumes, workspace = _scan_extents(runtime)

    for scanner in ("MBR-SCAN", "PART-SCAN"):
        assert _execute(
            runtime,
            scanner,
            block,
            volumes,
            4,
            workspace,
            5120,
        ) == (2, 0)
        assert _volume_fields(runtime, volumes) == (8, 16, 1, 1, TRUE)
        assert _volume_fields(runtime, volumes + 144) == (40, 24, 1, 3, TRUE)
        assert runtime.memory.read64(block + 88) == 2
        assert runtime.spinlocks.owner(0) is None


def test_mbr_failures_clear_outputs_and_report_capacity_then_workspace() -> None:
    invalid = _load_partition(
        _mbr_image(entries=((0x83, 8, 16), (0x0C, 250, 16)))
    )
    block, volumes, workspace = _scan_extents(invalid, volume_slots=2)
    assert _execute(
        invalid,
        "MBR-SCAN",
        block,
        volumes,
        2,
        workspace,
        5120,
    ) == (0, _constant(invalid, "PART-E-CORRUPT"))
    assert invalid.memory.read_bytes(volumes, 2 * 144) == bytes(2 * 144)
    assert invalid.memory.read64(block + 88) == 0
    assert invalid.spinlocks.owner(0) is None

    runtime = _load_partition(_mbr_image())
    block, volumes, workspace = _scan_extents(runtime, volume_slots=3)
    canary = volumes + 2 * 144
    runtime.memory.fill(canary, 144, 0xA5)
    assert _execute(
        runtime,
        "MBR-SCAN",
        block,
        volumes,
        1,
        workspace,
        5120,
    ) == (0, _constant(runtime, "PART-E-CAPACITY"))
    assert runtime.memory.read64(block + 88) == 0
    assert _execute(
        runtime,
        "MBR-SCAN",
        block,
        volumes,
        2,
        workspace,
        5120,
    ) == (2, 0)
    assert runtime.memory.read64(block + 88) == 2
    assert _execute(
        runtime,
        "MBR-SCAN",
        block,
        volumes,
        2,
        workspace,
        5119,
    ) == (0, _constant(runtime, "PART-E-WORKSPACE"))
    assert runtime.memory.read_bytes(volumes, 2 * 144) == bytes(2 * 144)
    assert runtime.memory.read_bytes(canary, 144) == bytes((0xA5,)) * 144
    assert runtime.memory.read64(block + 88) == 0
    assert runtime.spinlocks.owner(0) is None


def test_gpt_scan_validates_both_headers_arrays_and_partition_entries() -> None:
    runtime = _load_partition(
        _gpt_image(
            entry_count=3,
            entry_size=248,
            partition_indices=(0, 2),
        )
    )
    block, volumes, workspace = _scan_extents(runtime)

    for scanner in ("GPT-SCAN", "PART-SCAN"):
        assert _execute(
            runtime,
            scanner,
            block,
            volumes,
            4,
            workspace,
            5120,
        ) == (2, 0)
        assert _volume_fields(runtime, volumes) == (40, 16, 2, 0, TRUE)
        assert _volume_fields(runtime, volumes + 144) == (80, 32, 2, 2, TRUE)
        assert runtime.memory.read64(block + 88) == 2
        assert runtime.crc.mode == 4
        assert runtime.crc.owner is None
        assert runtime.spinlocks.owner(0) is None


@pytest.mark.parametrize(
    "region",
    (
        "primary-header",
        "backup-header",
        "primary-array",
        "backup-array",
    ),
)
def test_gpt_copy_crc_corruption_fails_transactionally(region: str) -> None:
    image = _gpt_image()
    total_sectors = len(image) // SECTOR_SIZE
    backup_header = (total_sectors - 1) * SECTOR_SIZE
    backup_entries_lba = struct.unpack_from("<Q", image, backup_header + 72)[0]
    offsets = {
        "primary-header": SECTOR_SIZE + 16,
        "backup-header": backup_header + 16,
        "primary-array": 2 * SECTOR_SIZE + 127,
        "backup-array": backup_entries_lba * SECTOR_SIZE + 127,
    }
    image[offsets[region]] ^= 0x80
    corrupt = _load_partition(image)
    block, volumes, workspace = _scan_extents(corrupt)

    assert _execute(
        corrupt,
        "GPT-SCAN",
        block,
        volumes,
        4,
        workspace,
        5120,
    ) == (0, _constant(corrupt, "PART-E-CORRUPT"))
    assert corrupt.memory.read_bytes(volumes, 4 * 144) == bytes(4 * 144)
    assert corrupt.memory.read64(block + 88) == 0
    assert corrupt.crc.owner is None
    assert corrupt.spinlocks.owner(0) is None


def test_gpt_crc_capability_absence_fails_transactionally() -> None:
    unsupported = _load_partition(
        _gpt_image(),
        memory=create_one_core_address_space(crypto_capabilities=0),
    )
    block, volumes, workspace = _scan_extents(unsupported)
    assert _execute(
        unsupported,
        "GPT-SCAN",
        block,
        volumes,
        4,
        workspace,
        5120,
    ) == (0, _constant(unsupported, "PART-E-CRC-UNSUPPORTED"))
    assert unsupported.memory.read64(block + 88) == 0
    assert unsupported.crc.owner is None
    assert unsupported.spinlocks.owner(0) is None


def test_gpt_busy_preserves_the_callers_crc_owner_and_releases_scan_lock() -> None:
    runtime = _load_partition(_gpt_image())
    block, volumes, workspace = _scan_extents(runtime)
    assert _execute(runtime, "CRC-MODE!", 0) == (0,)
    assert _execute(runtime, "CRC-RESET") == (0,)
    before = runtime.crc.accumulator

    assert _execute(
        runtime,
        "GPT-SCAN",
        block,
        volumes,
        4,
        workspace,
        5120,
    ) == (0, _constant(runtime, "PART-E-CRC-BUSY"))
    assert runtime.crc.owner == (0, 0)
    assert runtime.crc.accumulator == before
    assert runtime.memory.read64(block + 88) == 0
    assert runtime.spinlocks.owner(0) is None
    assert _execute(runtime, "CRC-FINAL@") == (0,)


def test_gpt_late_generation_swap_clears_staging_and_releases_resources() -> None:
    replacement = bytes((0xE7,)) * (256 * SECTOR_SIZE)

    class SwapDuringArrayReadStorage(HostedStorageService):
        __slots__ = ("read_acceptances",)

        def __init__(self, image: bytes | bytearray) -> None:
            super().__init__(image)
            self.read_acceptances = 0

        def _before_guarded_accept(
            self,
            command: int,
            expected_generation: int,
        ) -> None:
            assert expected_generation == self.media_generation
            if command != STORAGE_CMD_READ:
                return
            self.read_acceptances += 1
            if self.read_acceptances == 5:
                self.attach(replacement)

    storage = SwapDuringArrayReadStorage(_gpt_image())
    runtime = _evaluate_partition(
        _load_storage(MegaForthRuntime(storage=storage))
    )
    block, volumes, workspace = _scan_extents(runtime)
    stale_ior = _execute(runtime, "IOR-FROM-BLOCK-RESULT", 11)[0]

    assert _execute(
        runtime,
        "GPT-SCAN",
        block,
        volumes,
        4,
        workspace,
        5120,
    ) == (0, stale_ior)
    assert storage.read_acceptances == 5
    assert storage.image_bytes == replacement
    assert runtime.memory.read_bytes(volumes, 4 * 144) == bytes(4 * 144)
    assert runtime.memory.read64(block + 88) == 0
    assert runtime.crc.owner is None
    assert runtime.spinlocks.owner(0) is None
