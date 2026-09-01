"""Backend-neutral MP64FS geometry and BIOS validation semantics."""

from __future__ import annotations

from dataclasses import dataclass

from shared.storage import SECTOR_SIZE


MP64FS_MAGIC = b"MP64"
MP64FS_MARKER = 1
MP64FS_MIN_SECTORS = 15
MP64FS_MAX_SECTORS = 65_536
MP64FS_BITMAP_START = 1
MP64FS_MAX_BITMAP_SECTORS = 16
MP64FS_DIRECTORY_SECTORS = 12
MP64FS_DIRECTORY_BYTES = MP64FS_DIRECTORY_SECTORS * SECTOR_SIZE
MP64FS_ENTRY_SIZE = 48
MP64FS_MAX_FILES = 128


@dataclass(frozen=True, slots=True)
class MP64FSGeometry:
    """Canonical marker-1 geometry retained after superblock validation."""

    total_sectors: int
    bitmap_sectors: int
    directory_start: int
    data_start: int

    def __post_init__(self) -> None:
        values = (
            self.total_sectors,
            self.bitmap_sectors,
            self.directory_start,
            self.data_start,
        )
        if any(
            isinstance(value, bool) or not isinstance(value, int)
            for value in values
        ):
            raise TypeError("MP64FS geometry fields must be integers")
        expected_bitmap = (self.total_sectors + 4095) // 4096
        expected_directory = MP64FS_BITMAP_START + expected_bitmap
        expected_data = expected_directory + MP64FS_DIRECTORY_SECTORS
        if (
            not MP64FS_MIN_SECTORS
            <= self.total_sectors
            <= MP64FS_MAX_SECTORS
            or self.bitmap_sectors != expected_bitmap
            or self.directory_start != expected_directory
            or self.data_start != expected_data
            or self.data_start >= self.total_sectors
        ):
            raise ValueError("MP64FS geometry is not canonical marker-1 geometry")


def decode_bios_mp64fs_geometry(
    superblock: bytes | bytearray | memoryview,
    media_sectors: int,
) -> MP64FSGeometry | None:
    """Return canonical geometry when the executable BIOS would accept it."""

    block = bytes(superblock)
    if len(block) != SECTOR_SIZE:
        raise ValueError("MP64FS superblock must contain exactly one sector")
    if isinstance(media_sectors, bool) or not isinstance(media_sectors, int):
        raise TypeError("MP64FS media sector count must be an integer")
    if media_sectors < 0:
        raise ValueError("MP64FS media sector count must not be negative")

    if block[0:4] != MP64FS_MAGIC:
        return None
    if _u16(block, 4) != MP64FS_MARKER:
        return None

    total_sectors = _u32(block, 6)
    if not MP64FS_MIN_SECTORS <= total_sectors <= MP64FS_MAX_SECTORS:
        return None
    if total_sectors != media_sectors:
        return None

    bitmap_sectors = (total_sectors + 4095) // 4096
    directory_start = MP64FS_BITMAP_START + bitmap_sectors
    data_start = directory_start + MP64FS_DIRECTORY_SECTORS
    if _u16(block, 10) != MP64FS_BITMAP_START:
        return None
    if _u16(block, 12) != bitmap_sectors:
        return None
    if _u16(block, 14) != directory_start:
        return None
    if _u16(block, 16) != MP64FS_DIRECTORY_SECTORS:
        return None
    if _u16(block, 18) != data_start:
        return None
    if data_start >= total_sectors:
        return None
    if block[20] != MP64FS_MAX_FILES:
        return None
    if block[21] != MP64FS_ENTRY_SIZE:
        return None
    return MP64FSGeometry(
        total_sectors=total_sectors,
        bitmap_sectors=bitmap_sectors,
        directory_start=directory_start,
        data_start=data_start,
    )


def bios_mp64fs_metadata_valid(
    bitmap: bytes | bytearray | memoryview,
    directory: bytes | bytearray | memoryview,
    geometry: MP64FSGeometry,
) -> bool:
    """Validate allocation and directory metadata with exact BIOS rules."""

    if not isinstance(geometry, MP64FSGeometry):
        raise TypeError("MP64FS metadata validation requires geometry")
    bitmap_bytes = bytes(bitmap)
    directory_bytes = bytes(directory)
    expected_bitmap_bytes = geometry.bitmap_sectors * SECTOR_SIZE
    if len(bitmap_bytes) != expected_bitmap_bytes:
        raise ValueError("MP64FS bitmap length does not match geometry")
    if len(directory_bytes) != MP64FS_DIRECTORY_BYTES:
        raise ValueError("MP64FS directory must contain exactly 128 entries")

    for sector in range(geometry.data_start):
        if not _allocated(bitmap_bytes, sector):
            return False

    for slot in range(MP64FS_MAX_FILES):
        offset = slot * MP64FS_ENTRY_SIZE
        entry = directory_bytes[offset : offset + MP64FS_ENTRY_SIZE]
        # Native BIOS treats name[0] == 0 as empty and ignores its tail.
        if entry[0] == 0:
            continue

        parent = entry[34]
        if parent != 0xFF:
            if parent > 127:
                return False
            parent_offset = parent * MP64FS_ENTRY_SIZE
            parent_entry = directory_bytes[
                parent_offset : parent_offset + MP64FS_ENTRY_SIZE
            ]
            if parent_entry[0] == 0 or parent_entry[32] != 8:
                return False

        entry_type = entry[32]
        if not 1 <= entry_type <= 10:
            return False
        if entry_type == 8:
            if (
                _u16(entry, 24)
                or _u16(entry, 26)
                or _u32(entry, 28)
                or _u16(entry, 44)
                or _u16(entry, 46)
            ):
                return False
            continue

        primary_start = _u16(entry, 24)
        primary_count = _u16(entry, 26)
        if not _allocated_run_valid(
            bitmap_bytes,
            geometry,
            primary_start,
            primary_count,
        ):
            return False

        secondary_start = _u16(entry, 44)
        secondary_count = _u16(entry, 46)
        if secondary_count == 0:
            if secondary_start != 0:
                return False
        elif secondary_start == 0 or not _allocated_run_valid(
            bitmap_bytes,
            geometry,
            secondary_start,
            secondary_count,
        ):
            return False

        capacity = (primary_count + secondary_count) * SECTOR_SIZE
        if _u32(entry, 28) > capacity:
            return False
    return True


def _allocated_run_valid(
    bitmap: bytes,
    geometry: MP64FSGeometry,
    start: int,
    count: int,
) -> bool:
    if count == 0 or start < geometry.data_start:
        return False
    if start >= geometry.total_sectors:
        return False
    if count > geometry.total_sectors - start:
        return False
    return all(_allocated(bitmap, sector) for sector in range(start, start + count))


def _allocated(bitmap: bytes, sector: int) -> bool:
    return bool(bitmap[sector // 8] & (1 << (sector % 8)))


def _u16(payload: bytes, offset: int) -> int:
    return int.from_bytes(payload[offset : offset + 2], "little")


def _u32(payload: bytes, offset: int) -> int:
    return int.from_bytes(payload[offset : offset + 4], "little")


__all__ = [
    "MP64FS_BITMAP_START",
    "MP64FS_DIRECTORY_BYTES",
    "MP64FS_DIRECTORY_SECTORS",
    "MP64FS_ENTRY_SIZE",
    "MP64FS_MAGIC",
    "MP64FS_MARKER",
    "MP64FS_MAX_BITMAP_SECTORS",
    "MP64FS_MAX_FILES",
    "MP64FS_MAX_SECTORS",
    "MP64FS_MIN_SECTORS",
    "MP64FSGeometry",
    "bios_mp64fs_metadata_valid",
    "decode_bios_mp64fs_geometry",
]
