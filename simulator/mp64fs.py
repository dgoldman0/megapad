"""Semantic implementation of the BIOS MP64FS mount validator.

The validator intentionally mirrors the executable BIOS policy rather than
``diskutil``'s host-side interpretation of the format.  In particular, it
performs the same three checked reads into the BIOS scratch layout and retains
the BIOS's deliberately narrow occupied-entry and allocation checks.
"""

from __future__ import annotations

from collections.abc import Callable
from typing import TYPE_CHECKING

from shared.mp64fs import (
    MP64FS_BITMAP_START,
    MP64FS_DIRECTORY_BYTES,
    MP64FS_DIRECTORY_SECTORS,
    MP64FS_MAX_BITMAP_SECTORS,
    bios_mp64fs_metadata_valid,
    decode_bios_mp64fs_geometry,
)
from shared.storage import (
    SECTOR_SIZE,
    STORAGE_RESULT_OK,
    STORAGE_STATUS_PRESENT,
)

if TYPE_CHECKING:
    from simulator.runtime import ExecutionContext, MegaForthRuntime


MP64FS_METADATA_BUFFER_BYTES = (
    MP64FS_DIRECTORY_BYTES + MP64FS_MAX_BITMAP_SECTORS * SECTOR_SIZE
)
MP64FS_CHECKED_READ_STACK_BYTES = 128


def validate_attached_mp64fs(
    runtime: MegaForthRuntime,
    context: ExecutionContext,
    read_checked: Callable[[int, int, int], tuple[int, int]],
) -> int:
    """Execute ``MP64FS-VALID?`` and return its literal ``1`` or ``0``."""

    if not runtime.storage.status & STORAGE_STATUS_PRESENT:
        return 0

    # Executable core-0 BIOS uses R2/2, its initial data-stack pointer.
    scratch = runtime.main_context.data.empty_pointer
    media_generation = runtime.storage.media_generation
    return_stack = (
        context.returns
        if context.returns.backed
        else runtime.main_context.returns
    )
    return_pointer = return_stack.pointer
    if return_pointer < MP64FS_CHECKED_READ_STACK_BYTES:
        return 0
    protected_limit = return_pointer - MP64FS_CHECKED_READ_STACK_BYTES
    if (
        scratch > protected_limit
        or MP64FS_METADATA_BUFFER_BYTES > protected_limit - scratch
    ):
        return 0

    if not _read_succeeded(read_checked(scratch, 0, 1), expected=1):
        return 0
    superblock = runtime.memory.read_bytes(scratch, SECTOR_SIZE)
    geometry = decode_bios_mp64fs_geometry(
        superblock,
        runtime.storage.total_sectors,
    )
    if geometry is None:
        return 0

    bitmap_address = scratch + MP64FS_DIRECTORY_BYTES
    if not _read_succeeded(
        read_checked(
            bitmap_address,
            MP64FS_BITMAP_START,
            geometry.bitmap_sectors,
        ),
        expected=geometry.bitmap_sectors,
    ):
        return 0
    if not _read_succeeded(
        read_checked(
            scratch,
            geometry.directory_start,
            MP64FS_DIRECTORY_SECTORS,
        ),
        expected=MP64FS_DIRECTORY_SECTORS,
    ):
        return 0

    bitmap = runtime.memory.read_bytes(
        bitmap_address,
        geometry.bitmap_sectors * SECTOR_SIZE,
    )
    directory = runtime.memory.read_bytes(scratch, MP64FS_DIRECTORY_BYTES)
    if not bios_mp64fs_metadata_valid(bitmap, directory, geometry):
        return 0
    if runtime.storage.media_generation != media_generation:
        return 0
    return 1


def _read_succeeded(result: tuple[int, int], *, expected: int) -> bool:
    completed, status = result
    return status == STORAGE_RESULT_OK and completed == expected


__all__ = [
    "validate_attached_mp64fs",
]
