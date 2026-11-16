"""Synchronous block-storage service for the hosted MegaPad profile.

The hardware controller is byte-register driven and may expose DMA cadence,
contention, partial completion, and timeout.  The hosted simulator keeps the
same public BIOS results and media identity rules, but completes each admitted
request semantically against one bounded image and one guest-memory span.

Test faults and the request observer are the emulator controller's own,
defined once in native code (``shared/accel/storage_faults.h``).  The
runtime binds them from the native extension; a service without them refuses
to plant a fault.  A fault acts where the byte-serial controller would meet
it, and the service lands exactly what that controller would have moved.
"""

from __future__ import annotations

import mmap
import os
from pathlib import Path
from typing import Callable

from shared.cells import MASK64
from shared.storage import (
    SECTOR_SIZE,
    HostImageFile,
    STORAGE_CAP_COMPLETION,
    STORAGE_CAP_FLUSH,
    STORAGE_CAP_GEN_GUARD,
    STORAGE_CAP_MEDIA_GEN,
    STORAGE_CAP_PRECISE_RESULT,
    STORAGE_CAP_READ,
    STORAGE_CAP_WRITE,
    STORAGE_CAPS,
    STORAGE_CMD_FLUSH,
    STORAGE_CMD_READ,
    STORAGE_CMD_WRITE,
    STORAGE_RESULT_ADDRESS_OVERFLOW,
    STORAGE_RESULT_DMA_FAILURE,
    STORAGE_RESULT_DMA_INVALID,
    STORAGE_RESULT_FLUSH_FAILURE,
    STORAGE_RESULT_INTERNAL,
    STORAGE_RESULT_INVALID_COUNT,
    STORAGE_RESULT_LBA_RANGE,
    STORAGE_RESULT_MEDIA_FAILURE,
    STORAGE_RESULT_MEDIA_REMOVED,
    STORAGE_RESULT_NO_MEDIA,
    STORAGE_RESULT_OK,
    STORAGE_RESULT_PARTIAL,
    STORAGE_RESULT_UNSUPPORTED,
    STORAGE_RESULT_WRITE_PROTECTED,
    STORAGE_STATUS_ERROR,
    STORAGE_STATUS_MEDIA_CHANGED,
    STORAGE_STATUS_PRESENT,
    STORAGE_STATUS_RESULT_VALID,
    STORAGE_STATUS_WRITE_PROTECTED,
)
from simulator.memory import MemoryAccessError, SparseAddressSpace


DEFAULT_IMAGE_SECTORS = 2048
MAX_MEDIA_SECTORS = 0xFFFF_FFFF
CONTROLLER_MAX_SECTORS = 0xFF
READ_REQUIRED_CAPS = (
    STORAGE_CAP_READ
    | STORAGE_CAP_PRECISE_RESULT
    | STORAGE_CAP_COMPLETION
    | STORAGE_CAP_MEDIA_GEN
    | STORAGE_CAP_GEN_GUARD
)
WRITE_REQUIRED_CAPS = (
    STORAGE_CAP_WRITE
    | STORAGE_CAP_PRECISE_RESULT
    | STORAGE_CAP_COMPLETION
    | STORAGE_CAP_MEDIA_GEN
    | STORAGE_CAP_GEN_GUARD
)
FLUSH_REQUIRED_CAPS = (
    STORAGE_CAP_FLUSH
    | STORAGE_CAP_PRECISE_RESULT
    | STORAGE_CAP_COMPLETION
    | STORAGE_CAP_MEDIA_GEN
    | STORAGE_CAP_GEN_GUARD
)


class HostedStorageService:
    """Own one simulator-local sector image and BIOS-visible controller state."""

    __slots__ = (
        "_admission_hook",
        "_capabilities",
        "_claimed",
        "_completion",
        "_fault_core",
        "_faults",
        "_host_file",
        "_image",
        "_image_path",
        "_media_changed",
        "_media_generation",
        "_observer",
        "_present",
        "_result",
        "_result_valid",
        "_transferred",
        "_write_protected",
    )

    def __init__(
        self,
        image: bytes | bytearray | memoryview | None = None,
        *,
        image_path: str | os.PathLike[str] | None = None,
        write_protected: bool = False,
        capabilities: int = STORAGE_CAPS,
    ) -> None:
        if image is not None and image_path is not None:
            raise ValueError(
                "storage image bytes and image path are mutually exclusive"
            )
        if not isinstance(write_protected, bool):
            raise TypeError("storage write_protected flag must be bool")
        if isinstance(capabilities, bool) or not isinstance(capabilities, int):
            raise TypeError("storage capabilities must be an integer")
        if not 0 <= capabilities <= 0xFF or capabilities & ~STORAGE_CAPS:
            raise ValueError("storage capabilities contain unsupported bits")

        self._capabilities = capabilities
        self._claimed = False
        self._image = bytearray()
        self._image_path: Path | None = None
        self._host_file = HostImageFile()
        self._present = False
        self._write_protected = False
        self._media_generation = 0
        self._media_changed = False
        self._completion = 0
        self._result = STORAGE_RESULT_OK
        self._result_valid = False
        self._transferred = 0
        self._fault_core = None
        self._faults = None
        self._observer = None
        self._admission_hook = None

        if image_path is not None:
            path = Path(image_path)
            try:
                payload = path.read_bytes()
                found = True
            except FileNotFoundError:
                # A write-protected medium is never written, so a file that
                # holds no image can never come to hold one.
                if write_protected:
                    raise
                payload = bytes(SECTOR_SIZE * DEFAULT_IMAGE_SECTORS)
                found = False
            self.attach(
                payload,
                image_path=path,
                write_protected=write_protected,
            )
            # Media read from its own file needs only its changes written.
            self._host_file.media_attached(file_holds_media=found)
        elif image is not None:
            self.attach(image, write_protected=write_protected)
        elif write_protected:
            raise ValueError("absent storage cannot be write protected")

    @staticmethod
    def _validated_image(
        image: bytes | bytearray | memoryview,
    ) -> bytearray:
        if not isinstance(image, (bytes, bytearray, memoryview)):
            raise TypeError("storage image must be bytes-like")
        payload = bytearray(image)
        if len(payload) % SECTOR_SIZE:
            raise ValueError(
                f"storage image size {len(payload)} is not a multiple of "
                f"the {SECTOR_SIZE}-byte sector size"
            )
        if len(payload) // SECTOR_SIZE > MAX_MEDIA_SECTORS:
            raise ValueError("storage image exceeds the u32 sector capacity")
        return payload

    def claim(self) -> None:
        """Give one runtime exclusive ownership of this mutable service."""

        if self._claimed:
            raise ValueError("storage service is already owned by a runtime")
        self._claimed = True

    @property
    def present(self) -> bool:
        return self._present

    @property
    def write_protected(self) -> bool:
        return self._write_protected

    @property
    def status(self) -> int:
        status = 0
        if self._present:
            status |= STORAGE_STATUS_PRESENT
        if self._write_protected:
            status |= STORAGE_STATUS_WRITE_PROTECTED
        if self._media_changed:
            status |= STORAGE_STATUS_MEDIA_CHANGED
        if self._result_valid:
            status |= STORAGE_STATUS_RESULT_VALID
            if self._result & 0x7F:
                status |= STORAGE_STATUS_ERROR
        return status

    @property
    def capabilities(self) -> int:
        return self._capabilities

    @property
    def total_sectors(self) -> int:
        return len(self._image) // SECTOR_SIZE if self._present else 0

    @property
    def media_generation(self) -> int:
        return self._media_generation

    @property
    def completion(self) -> int:
        return self._completion

    @property
    def result(self) -> int:
        return self._result

    @property
    def transferred(self) -> int:
        return self._transferred

    @property
    def image_bytes(self) -> bytes:
        return bytes(self._image)

    @property
    def image_path(self) -> Path | None:
        return self._image_path

    def attach(
        self,
        image: bytes | bytearray | memoryview | mmap.mmap,
        *,
        image_path: str | os.PathLike[str] | Path | None = None,
        write_protected: bool = False,
        file_holds_media: bool = False,
        adopt: bool = False,
    ) -> None:
        """Replace media without flushing and advance its wrapping identity.

        The service keeps a copy of IMAGE, or with ADOPT the bytearray or
        writable mapping itself, so that the caller sees the guest's writes
        land in it.  The first flush writes the whole image to IMAGE_PATH,
        unless FILE_HOLDS_MEDIA says that the file already holds it, when
        only the sectors changed since are written.
        """

        if not isinstance(write_protected, bool):
            raise TypeError("storage write_protected flag must be bool")
        if file_holds_media and image_path is None:
            raise ValueError("a medium held in its file needs the file's path")
        payload = (
            self._adopted_image(image) if adopt
            else self._validated_image(image)
        )
        next_path = None if image_path is None else Path(image_path)
        self._image = payload
        self._image_path = next_path
        self._host_file.media_attached(file_holds_media=file_holds_media)
        self._present = True
        self._write_protected = write_protected
        self._advance_media_generation()

    @staticmethod
    def _adopted_image(image: bytearray | mmap.mmap) -> bytearray | mmap.mmap:
        if not isinstance(image, (bytearray, mmap.mmap)):
            raise TypeError(
                "an adopted storage image must be a bytearray or a mapping"
            )
        with memoryview(image) as view:
            if view.readonly:
                raise TypeError("an adopted storage image must be writable")
        if len(image) % SECTOR_SIZE:
            raise ValueError(
                f"storage image size {len(image)} is not a multiple of "
                f"the {SECTOR_SIZE}-byte sector size"
            )
        if len(image) // SECTOR_SIZE > MAX_MEDIA_SECTORS:
            raise ValueError("storage image exceeds the u32 sector capacity")
        return image

    def detach(self) -> None:
        """Detach media without implicitly flushing guest changes."""

        self._image = bytearray()
        self._image_path = None
        self._host_file.media_detached()
        self._present = False
        self._write_protected = False
        self._advance_media_generation()

    def set_write_protected(self, enabled: bool) -> None:
        if not isinstance(enabled, bool):
            raise TypeError("storage write_protected flag must be bool")
        if not self._present and enabled:
            raise ValueError("absent storage cannot be write protected")
        self._write_protected = enabled

    def acknowledge_media_change(self) -> None:
        self._media_changed = False

    def bind_fault_core(self, core) -> None:
        """Take the test faults and request observer from CORE, the native
        extension, which defines them once for both machines' controllers.
        Binding the same extension again changes nothing."""

        if self._fault_core is core:
            return
        if self._fault_core is not None:
            raise ValueError("storage service already has its fault core")
        self._fault_core = core
        self._faults = core.StorageFaultQueue()

    def _require_fault_core(self):
        core = self._fault_core
        if core is None:
            raise RuntimeError(
                "storage faults and the request observer need MegaPad's "
                "native simulator extension (make simulator-accel), which "
                "the runtime binds to its storage"
            )
        return core

    def inject_fault(
        self,
        stage: str,
        result: int = STORAGE_RESULT_MEDIA_FAILURE,
        *,
        command: int | None = None,
        sector_index: int | None = None,
        byte_index: int | None = None,
        action: str = "fail",
    ):
        """Queue one one-shot fault, as the emulator's controller does.

        A stall cannot be planted: this service completes every request
        when it is made, so nothing can hold one at its start.
        """

        fault = self._make_fault(
            stage,
            result,
            command=command,
            sector_index=sector_index,
            byte_index=byte_index,
            action=action,
        )
        self._faults.push(fault)
        return fault

    def _make_fault(self, *args, **kwargs):
        fault = self._require_fault_core().storage_fault(*args, **kwargs)
        if fault.action == "stall":
            raise ValueError(
                "the hosted storage service completes each request when it "
                "is made and cannot hold one stalled"
            )
        return fault

    def clear_faults(self) -> None:
        if self._faults is not None:
            self._faults.clear()

    def observe(
        self, hook: Callable[[int, int, int], None] | None = None
    ):
        """Record every request this service admits from now on.

        The emulator's controller admits a request once it has accepted
        and checked it, just before the transfer or flush; this service
        admits each controller-sized chunk of a transfer, and each flush,
        at that same point.  The observer records the request and plants
        the faults planned for it, then HOOK, if given, is called with the
        command, LBA and count; a flush names no sectors.
        """

        core = self._require_fault_core()
        observer = core.StorageRequestObserver()
        self._observer = observer
        self._admission_hook = hook
        return observer

    def stop_observing(self) -> None:
        self._observer = None
        self._admission_hook = None

    def plan_fault(
        self,
        request_command: int,
        ordinal: int,
        *,
        after_writes: int | None = None,
        **fault,
    ):
        """Plant a fault, described as ``inject_fault`` takes one, when the
        ORDINAL-th REQUEST_COMMAND is admitted.  With AFTER_WRITES, ORDINAL
        counts only the requests admitted after exactly that many writes.
        The request observer plants it, so one must be attached."""

        observer = self._observer
        if observer is None:
            raise RuntimeError("plan a storage fault after observe()")
        made = self._make_fault(**fault)
        observer.plan(made, request_command, ordinal, after_writes)
        return made

    def _admit(self, command: int, lba: int, count: int) -> None:
        self._observer.admit(command, lba, count, self._faults)
        hook = self._admission_hook
        if hook is not None:
            hook(command, lba, count)

    def _take_early_fault(self, command: int):
        """The fault that acts as the controller accepts or starts COMMAND."""

        faults = self._faults
        if not faults:
            return None
        fault = faults.take("accept", command)
        if fault is None:
            fault = faults.take("start", command)
        return fault

    def _fault_result(self, fault, effect_applied: bool) -> int:
        """Act on FAULT and return the controller's result for it."""

        if fault.action == "detach":
            self.detach()
        return self._fault_core.storage_fault_result(fault, effect_applied)

    def read_checked(
        self,
        memory: SparseAddressSpace,
        dma: int,
        lba: int,
        count: int,
        *,
        generation: int | None = None,
    ) -> tuple[int, int]:
        return self._transfer(
            memory,
            dma,
            lba,
            count,
            write=False,
            generation=generation,
        )

    def write_checked(
        self,
        memory: SparseAddressSpace,
        dma: int,
        lba: int,
        count: int,
        *,
        generation: int | None = None,
    ) -> tuple[int, int]:
        return self._transfer(
            memory,
            dma,
            lba,
            count,
            write=True,
            generation=generation,
        )

    def flush_checked(self, *, generation: int | None = None) -> int:
        return self._flush(generation=generation)

    def _advance_media_generation(self) -> None:
        self._media_generation = (self._media_generation + 1) & 0xFFFF_FFFF
        self._media_changed = True

    def _publish_result(self, result: int, transferred: int) -> None:
        self._result = result & 0xFF
        self._transferred = transferred & 0xFF
        self._result_valid = True
        self._completion = (self._completion + 1) & 0xFFFF_FFFF

    def _validate_request(
        self,
        memory: SparseAddressSpace,
        dma: int,
        lba: int,
        count: int,
        *,
        write: bool,
        generation: int | None,
    ) -> int:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("storage DMA memory must be a SparseAddressSpace")
        dma = self._cell(dma, label="storage DMA address")
        lba = self._cell(lba, label="storage LBA")
        count = self._cell(count, label="storage sector count")
        if generation is not None:
            generation = self._cell(
                generation,
                label="storage media generation",
            )
        if not self._present:
            return STORAGE_RESULT_NO_MEDIA
        required = WRITE_REQUIRED_CAPS if write else READ_REQUIRED_CAPS
        if self._capabilities & required != required:
            return STORAGE_RESULT_UNSUPPORTED
        if generation is not None and generation != self._media_generation:
            return STORAGE_RESULT_MEDIA_REMOVED
        if count == 0:
            return STORAGE_RESULT_INVALID_COUNT
        capacity = self.total_sectors
        if lba >= capacity or count > capacity - lba:
            return STORAGE_RESULT_LBA_RANGE

        byte_count = count * SECTOR_SIZE
        if byte_count > MASK64 + 1 - dma:
            return STORAGE_RESULT_ADDRESS_OVERFLOW
        limit = dma + byte_count
        if not any(
            region.base <= dma and limit <= region.limit
            for region in memory.regions
        ):
            return STORAGE_RESULT_DMA_INVALID
        return STORAGE_RESULT_OK

    def _transfer(
        self,
        memory: SparseAddressSpace,
        dma: int,
        lba: int,
        count: int,
        *,
        write: bool,
        generation: int | None,
    ) -> tuple[int, int]:
        expected_generation = (
            self._media_generation if generation is None else generation
        )
        status = self._validate_request(
            memory,
            dma,
            lba,
            count,
            write=write,
            generation=generation,
        )
        if status != STORAGE_RESULT_OK:
            return 0, status

        command = STORAGE_CMD_WRITE if write else STORAGE_CMD_READ
        completed = 0
        while completed < count:
            if self._media_generation != expected_generation:
                result = STORAGE_RESULT_MEDIA_REMOVED
                if completed:
                    result |= STORAGE_RESULT_PARTIAL
                return completed, result

            chunk_count = min(CONTROLLER_MAX_SECTORS, count - completed)
            chunk_bytes = chunk_count * SECTOR_SIZE
            chunk_dma = dma + completed * SECTOR_SIZE
            media_start = (lba + completed) * SECTOR_SIZE
            media_limit = media_start + chunk_bytes
            self._before_guarded_accept(command, expected_generation)
            if self._media_generation != expected_generation:
                self._publish_result(STORAGE_RESULT_MEDIA_REMOVED, 0)
                result = STORAGE_RESULT_MEDIA_REMOVED
                if completed:
                    result |= STORAGE_RESULT_PARTIAL
                return completed, result

            fault = self._take_early_fault(command)
            if fault is not None:
                result = self._fault_result(fault, False)
                self._publish_result(result, 0)
                return _chunk_status(result, 0, chunk_count, completed)

            if write and self._write_protected:
                self._publish_result(STORAGE_RESULT_WRITE_PROTECTED, 0)
                result = STORAGE_RESULT_WRITE_PROTECTED
                if completed:
                    result |= STORAGE_RESULT_PARTIAL
                return completed, result

            if self._observer is not None:
                self._admit(command, lba + completed, chunk_count)
            faults = self._faults
            hit = (
                faults.take_in_transfer(command, chunk_count)
                if faults else None
            )
            if hit is not None:
                # Land what the byte-serial controller would have moved
                # when the fault met it, then complete as it would.
                fault, _stage, _sector, _byte, moved, confirmed = hit
                media_limit = media_start + moved
                chunk_bytes = moved

            image = self._image
            try:
                if not chunk_bytes:
                    pass
                elif write:
                    payload = memory.read_bytes(chunk_dma, chunk_bytes)
                    self._host_file.mark(
                        lba + completed,
                        -(-chunk_bytes // SECTOR_SIZE),
                    )
                    image[media_start:media_limit] = payload
                else:
                    memory.write_bytes(
                        chunk_dma,
                        image[media_start:media_limit],
                    )
            except MemoryAccessError:
                self._publish_result(STORAGE_RESULT_DMA_FAILURE, 0)
                result = STORAGE_RESULT_DMA_FAILURE
                if completed:
                    result |= STORAGE_RESULT_PARTIAL
                return completed, result

            if hit is not None:
                result = self._fault_result(fault, moved > 0)
                self._publish_result(result, confirmed)
                completed, status = _chunk_status(
                    result, confirmed, chunk_count, completed
                )
                if status is not None:
                    return completed, status
                continue

            completed += chunk_count
            if self._media_generation != expected_generation:
                return completed, (
                    STORAGE_RESULT_MEDIA_REMOVED | STORAGE_RESULT_PARTIAL
                )
            self._publish_result(STORAGE_RESULT_OK, chunk_count)
        return completed, STORAGE_RESULT_OK

    def _flush(self, *, generation: int | None) -> int:
        expected_generation = (
            self._media_generation if generation is None else generation
        )
        if generation is not None:
            generation = self._cell(
                generation,
                label="storage media generation",
            )
        if not self._present:
            status = STORAGE_RESULT_NO_MEDIA
        elif self._capabilities & FLUSH_REQUIRED_CAPS != FLUSH_REQUIRED_CAPS:
            status = STORAGE_RESULT_UNSUPPORTED
        elif generation is not None and generation != self._media_generation:
            status = STORAGE_RESULT_MEDIA_REMOVED
        else:
            status = STORAGE_RESULT_OK

        if status != STORAGE_RESULT_OK:
            return status

        self._before_guarded_accept(
            STORAGE_CMD_FLUSH,
            expected_generation,
        )
        if self._media_generation != expected_generation:
            self._publish_result(STORAGE_RESULT_MEDIA_REMOVED, 0)
            return STORAGE_RESULT_MEDIA_REMOVED

        fault = self._take_early_fault(STORAGE_CMD_FLUSH)
        if fault is None:
            if self._observer is not None:
                self._admit(STORAGE_CMD_FLUSH, 0, 0)
            faults = self._faults
            if faults:
                fault = faults.take("flush", STORAGE_CMD_FLUSH)
        if fault is not None:
            # A flush the fault stops leaves the host file as it was.
            result = self._fault_result(fault, False)
            self._publish_result(result, 0)
            return result

        image_path = self._image_path
        try:
            if image_path is not None:
                self._host_file.flush(image_path, self._image)
        except OSError:
            result = STORAGE_RESULT_FLUSH_FAILURE | STORAGE_RESULT_PARTIAL
            self._publish_result(result, 0)
            return result

        if self._media_generation != expected_generation:
            return STORAGE_RESULT_MEDIA_REMOVED | STORAGE_RESULT_PARTIAL

        self._publish_result(STORAGE_RESULT_OK, 0)
        return STORAGE_RESULT_OK

    def _before_guarded_accept(
        self,
        command: int,
        expected_generation: int,
    ) -> None:
        """Deterministic test seam immediately before guarded acceptance."""

        del command, expected_generation

    @staticmethod
    def _cell(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        if not 0 <= value <= MASK64:
            raise ValueError(f"{label} must be a uint64 cell")
        return value


def _chunk_status(
    result: int,
    transferred: int,
    chunk_count: int,
    completed: int,
) -> tuple[int, int | None]:
    """The BIOS's checked transfer after one controller command.

    The sectors the command confirmed are added to those already completed.
    A nonzero result, or an OK that confirmed fewer sectors than the command
    asked for (which the BIOS reports as INTERNAL), ends the transfer, marked
    PARTIAL when any sector of it completed; otherwise the status is None
    and the next command follows.
    """

    completed += transferred
    if result != STORAGE_RESULT_OK:
        status = result
    elif transferred != chunk_count:
        status = STORAGE_RESULT_INTERNAL
    else:
        return completed, None
    if completed:
        status |= STORAGE_RESULT_PARTIAL
    return completed, status


__all__ = [
    "CONTROLLER_MAX_SECTORS",
    "DEFAULT_IMAGE_SECTORS",
    "FLUSH_REQUIRED_CAPS",
    "HostedStorageService",
    "MAX_MEDIA_SECTORS",
    "READ_REQUIRED_CAPS",
    "WRITE_REQUIRED_CAPS",
]
