"""Machine-independent constants for the MegaPad block-storage ABI, and the
host file that holds a storage device's media."""

from __future__ import annotations

import os
from pathlib import Path


SECTOR_SIZE = 512

# Storage controllers on every machine: unit 0 holds the system disk, unit 1
# the settings disk, whose medium is write-protected.
STORAGE_UNITS = 2

STORAGE_CMD_READ = 0x01
STORAGE_CMD_WRITE = 0x02
STORAGE_CMD_STATUS = 0x03
STORAGE_CMD_RESET = 0x04
STORAGE_CMD_FLUSH = 0xFF

STORAGE_STATUS_BUSY = 0x01
STORAGE_STATUS_ERROR = 0x02
STORAGE_STATUS_REJECTED = 0x04
STORAGE_STATUS_RESULT_VALID = 0x08
STORAGE_STATUS_MEDIA_CHANGED = 0x10
STORAGE_STATUS_WRITE_PROTECTED = 0x20
STORAGE_STATUS_PRESENT = 0x80

STORAGE_RESULT_PARTIAL = 0x80
STORAGE_RESULT_OK = 0x00
STORAGE_RESULT_NO_MEDIA = 0x01
STORAGE_RESULT_UNSUPPORTED = 0x02
STORAGE_RESULT_INVALID_COUNT = 0x03
STORAGE_RESULT_LBA_RANGE = 0x04
STORAGE_RESULT_ADDRESS_OVERFLOW = 0x05
STORAGE_RESULT_DMA_INVALID = 0x06
STORAGE_RESULT_DMA_FAILURE = 0x07
STORAGE_RESULT_WRITE_PROTECTED = 0x08
STORAGE_RESULT_MEDIA_FAILURE = 0x09
STORAGE_RESULT_TIMEOUT = 0x0A
STORAGE_RESULT_MEDIA_REMOVED = 0x0B
STORAGE_RESULT_RESET_ABORTED = 0x0C
STORAGE_RESULT_FLUSH_FAILURE = 0x0D
STORAGE_RESULT_INTERNAL = 0x0E

STORAGE_CAP_READ = 0x01
STORAGE_CAP_WRITE = 0x02
STORAGE_CAP_FLUSH = 0x04
STORAGE_CAP_PRECISE_RESULT = 0x08
STORAGE_CAP_COMPLETION = 0x10
STORAGE_CAP_MEDIA_GEN = 0x20
STORAGE_CAP_GEN_GUARD = 0x40
STORAGE_CAPS = (
    STORAGE_CAP_READ
    | STORAGE_CAP_WRITE
    | STORAGE_CAP_FLUSH
    | STORAGE_CAP_PRECISE_RESULT
    | STORAGE_CAP_COMPLETION
    | STORAGE_CAP_MEDIA_GEN
    | STORAGE_CAP_GEN_GUARD
)


class HostImageFile:
    """The host file that holds a storage device's media.

    The device marks each sector it changes, and a flush writes only the
    marked sectors, in place, then syncs the file.  The whole image is
    written instead when the file is not known to hold the media: after
    media is attached from anything but that file, or when the file is
    missing or of another size.  A flush with nothing marked leaves the
    file alone, since the last flush already made it durable.  A failed
    flush keeps every mark, so the next one writes them all again.
    """

    __slots__ = ("_dirty", "_whole")

    def __init__(self) -> None:
        self._dirty: set[int] = set()
        self._whole = True

    def media_attached(self, *, file_holds_media: bool) -> None:
        """New media: the file already holds it, or must receive all of it."""

        self._dirty.clear()
        self._whole = not file_holds_media

    def media_detached(self) -> None:
        self._dirty.clear()
        self._whole = False

    def mark(self, first_sector: int, count: int = 1) -> None:
        """Sectors the device has changed, or may have, since the last flush."""

        self._dirty.update(range(first_sector, first_sector + count))

    def flush(self, path: str | os.PathLike[str], image: bytearray) -> None:
        """Make PATH hold IMAGE durably.  Host failures raise ``OSError``."""

        if not self._whole and not self._dirty:
            return
        path = Path(path)
        try:
            stream = path.open("r+b")
        except FileNotFoundError:
            stream = path.open("wb")
            whole = True
        else:
            whole = (
                self._whole
                or os.fstat(stream.fileno()).st_size != len(image)
            )
        with stream, memoryview(image) as view:
            if whole:
                _write_at(stream, 0, view)
                stream.truncate(len(image))
            else:
                for first, count in _runs(sorted(self._dirty)):
                    start = first * SECTOR_SIZE
                    _write_at(
                        stream, start, view[start:start + count * SECTOR_SIZE]
                    )
            stream.flush()
            os.fsync(stream.fileno())
        self._dirty.clear()
        self._whole = False


def _runs(sectors: list[int]):
    """Consecutive sectors of a sorted list, as (first, count) runs."""

    first = previous = None
    for sector in sectors:
        if previous is not None and sector == previous + 1:
            previous = sector
            continue
        if first is not None:
            yield first, previous - first + 1
        first = previous = sector
    if first is not None:
        yield first, previous - first + 1


def _write_at(stream, offset: int, data: memoryview) -> None:
    stream.seek(offset)
    written = stream.write(data)
    if written != len(data):
        raise OSError(
            f"short storage image write: {written} of {len(data)} bytes "
            f"at offset {offset}"
        )


__all__ = [
    "HostImageFile",
    "SECTOR_SIZE",
    "STORAGE_CAPS",
    "STORAGE_CAP_COMPLETION",
    "STORAGE_CAP_FLUSH",
    "STORAGE_CAP_GEN_GUARD",
    "STORAGE_CAP_MEDIA_GEN",
    "STORAGE_CAP_PRECISE_RESULT",
    "STORAGE_CAP_READ",
    "STORAGE_CAP_WRITE",
    "STORAGE_CMD_FLUSH",
    "STORAGE_CMD_READ",
    "STORAGE_CMD_RESET",
    "STORAGE_CMD_STATUS",
    "STORAGE_CMD_WRITE",
    "STORAGE_RESULT_ADDRESS_OVERFLOW",
    "STORAGE_RESULT_DMA_FAILURE",
    "STORAGE_RESULT_DMA_INVALID",
    "STORAGE_RESULT_FLUSH_FAILURE",
    "STORAGE_RESULT_INTERNAL",
    "STORAGE_RESULT_INVALID_COUNT",
    "STORAGE_RESULT_LBA_RANGE",
    "STORAGE_RESULT_MEDIA_FAILURE",
    "STORAGE_RESULT_MEDIA_REMOVED",
    "STORAGE_RESULT_NO_MEDIA",
    "STORAGE_RESULT_OK",
    "STORAGE_RESULT_PARTIAL",
    "STORAGE_RESULT_RESET_ABORTED",
    "STORAGE_RESULT_TIMEOUT",
    "STORAGE_RESULT_UNSUPPORTED",
    "STORAGE_RESULT_WRITE_PROTECTED",
    "STORAGE_STATUS_BUSY",
    "STORAGE_STATUS_ERROR",
    "STORAGE_STATUS_MEDIA_CHANGED",
    "STORAGE_STATUS_PRESENT",
    "STORAGE_STATUS_REJECTED",
    "STORAGE_STATUS_RESULT_VALID",
    "STORAGE_STATUS_WRITE_PROTECTED",
]
