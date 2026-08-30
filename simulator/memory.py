"""Sparse guest memory and caller-bounded allocation for the hosted simulator.

The hosted backend exposes machine-shaped guest addresses without allocating
the gaps between physical memory classes.  Ordinary memory is zero-initialized
and materialized in fixed-size pages on first write.  MMIO is a reserved routed
aperture: it never falls through to sparse RAM when no service is installed.

This module owns address geometry and allocation lifetime only.  It does not
install KDOS ``ALLOCATE``/``FREE`` words; those remain ordinary target source.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum, auto
from typing import Protocol

from shared.cells import MASK64
from simulator.errors import ExecutionError, SimulatorError


ADDRESS_SPACE_SIZE = MASK64 + 1

BANK0_BASE = 0x0000_0000
BANK0_DEFAULT_SIZE = 1 << 20
EXTERNAL_BASE = 0x0010_0000
VRAM_BASE = 0xFF00_0000
HBW_BASE = 0xFFD0_0000
MMIO_BASE = 0xFFFF_FF00_0000_0000
MMIO_LIMIT = 0xFFFF_FF80_0000_0000

DEFAULT_PAGE_SIZE = 4096
_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))


class AddressClass(Enum):
    """Source-visible physical address classes."""

    BANK0 = auto()
    EXTERNAL = auto()
    VRAM = auto()
    HBW = auto()
    MMIO = auto()


@dataclass(frozen=True, slots=True)
class RegionSpec:
    """One bounded ordinary-memory region in the guest address space."""

    kind: AddressClass
    base: int
    size: int

    @property
    def limit(self) -> int:
        """Return the host-representable exclusive end of the region."""

        return self.base + self.size


class MMIOPort(Protocol):
    """Backend-local service boundary for the reserved MMIO aperture."""

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Validate one complete access before any byte callback occurs."""

    def read8(self, offset: int) -> int:
        """Read one byte at an offset from :data:`MMIO_BASE`."""

    def write8(self, offset: int, value: int) -> None:
        """Write one byte at an offset from :data:`MMIO_BASE`."""


class MemoryAccessError(ExecutionError):
    """Base class for one rejected guest-memory operation."""

    def __init__(
        self,
        message: str,
        *,
        operation: str,
        address: int,
        length: int,
    ) -> None:
        self.operation = operation
        self.address = address
        self.length = length
        super().__init__(message)


class AddressOverflowError(MemoryAccessError):
    """An address or complete span lies outside unsigned 64-bit geometry."""


class UnmappedAddressError(MemoryAccessError):
    """The first byte of a nonempty access has no mapped address class."""


class CrossRegionAccessError(MemoryAccessError):
    """A nonempty access begins in one class but escapes its mapped region."""


class MMIOAccessError(MemoryAccessError):
    """The MMIO service boundary rejected or could not serve an access."""

    def __init__(
        self,
        message: str,
        *,
        operation: str,
        address: int,
        length: int,
    ) -> None:
        self.offset = address - MMIO_BASE
        self.write = operation in ("write", "fill")
        super().__init__(
            message,
            operation=operation,
            address=address,
            length=length,
        )


class InvalidAllocationError(SimulatorError):
    """A region-allocation request does not name a valid allocator action."""

    def __init__(
        self,
        message: str,
        *,
        operation: str,
        address: int | None = None,
        size: int | None = None,
    ) -> None:
        self.operation = operation
        self.address = address
        self.size = size
        super().__init__(message)


def _require_integer(value: int, *, label: str) -> int:
    if not isinstance(value, int):
        raise TypeError(f"{label} must be an integer")
    return value


def _require_nonnegative(value: int, *, label: str) -> int:
    value = _require_integer(value, label=label)
    if value < 0:
        raise ValueError(f"{label} must be non-negative")
    return value


def _checked_span(
    address: int,
    length: int,
    *,
    operation: str,
) -> int:
    """Return an exclusive end without ever accepting uint64 wrap."""

    address = _require_integer(address, label="address")
    length = _require_integer(length, label="length")
    if not 0 <= address <= MASK64:
        raise AddressOverflowError(
            f"{operation} address is outside the uint64 address space",
            operation=operation,
            address=address,
            length=length,
        )
    if length < 0 or length > ADDRESS_SPACE_SIZE - address:
        raise AddressOverflowError(
            f"{operation} span would wrap the uint64 address space",
            operation=operation,
            address=address,
            length=length,
        )
    return address + length


class _SparseRegion:
    """Lazy fixed-page backing for one ordinary physical region."""

    __slots__ = ("spec", "page_size", "pages")

    def __init__(self, spec: RegionSpec, page_size: int) -> None:
        self.spec = spec
        self.page_size = page_size
        self.pages: dict[int, bytearray] = {}

    def read(self, offset: int, length: int) -> bytes:
        result = bytearray(length)
        result_offset = 0
        while result_offset < length:
            absolute_offset = offset + result_offset
            page_index, page_offset = divmod(absolute_offset, self.page_size)
            chunk_size = min(self.page_size - page_offset, length - result_offset)
            page = self.pages.get(page_index)
            if page is not None:
                result[result_offset : result_offset + chunk_size] = page[
                    page_offset : page_offset + chunk_size
                ]
            result_offset += chunk_size
        return bytes(result)

    def write(self, offset: int, payload: bytes) -> None:
        payload_offset = 0
        length = len(payload)
        while payload_offset < length:
            absolute_offset = offset + payload_offset
            page_index, page_offset = divmod(absolute_offset, self.page_size)
            chunk_size = min(self.page_size - page_offset, length - payload_offset)
            page = self.pages.get(page_index)
            if page is None:
                page = bytearray(self.page_size)
                self.pages[page_index] = page
            page[page_offset : page_offset + chunk_size] = payload[
                payload_offset : payload_offset + chunk_size
            ]
            payload_offset += chunk_size

    def fill(self, offset: int, length: int, value: int) -> None:
        consumed = 0
        while consumed < length:
            absolute_offset = offset + consumed
            page_index, page_offset = divmod(absolute_offset, self.page_size)
            chunk_size = min(self.page_size - page_offset, length - consumed)
            page = self.pages.get(page_index)
            if page is None:
                if value == 0:
                    consumed += chunk_size
                    continue
                page = bytearray(self.page_size)
                self.pages[page_index] = page
            page[page_offset : page_offset + chunk_size] = bytes((value,)) * chunk_size
            consumed += chunk_size


@dataclass(frozen=True, slots=True)
class _ResolvedSpan:
    kind: AddressClass
    address: int
    length: int
    region: _SparseRegion | None
    offset: int


class SparseAddressSpace:
    """Sparse 64-bit byte address space with an explicit MMIO boundary."""

    def __init__(
        self,
        *,
        bank0_size: int = BANK0_DEFAULT_SIZE,
        external_size: int = 0,
        vram_size: int = 0,
        hbw_size: int = 0,
        mmio: MMIOPort | None = None,
        page_size: int = DEFAULT_PAGE_SIZE,
    ) -> None:
        page_size = _require_nonnegative(page_size, label="page size")
        if page_size == 0 or page_size & (page_size - 1):
            raise ValueError("page size must be a positive power of two")

        sizes = (
            (AddressClass.BANK0, BANK0_BASE, bank0_size, "Bank 0 size"),
            (AddressClass.EXTERNAL, EXTERNAL_BASE, external_size, "external size"),
            (AddressClass.VRAM, VRAM_BASE, vram_size, "VRAM size"),
            (AddressClass.HBW, HBW_BASE, hbw_size, "HBW size"),
        )
        specs: list[RegionSpec] = []
        for kind, base, raw_size, label in sizes:
            size = _require_nonnegative(raw_size, label=label)
            if size == 0:
                continue
            if size > ADDRESS_SPACE_SIZE - base:
                raise ValueError(f"{label} wraps the uint64 address space")
            spec = RegionSpec(kind, base, size)
            if spec.base < MMIO_LIMIT and MMIO_BASE < spec.limit:
                raise ValueError(f"{label} intersects the MMIO aperture")
            specs.append(spec)

        specs.sort(key=lambda spec: spec.base)
        for previous, current in zip(specs, specs[1:]):
            if previous.limit > current.base:
                raise ValueError(
                    f"{previous.kind.name} and {current.kind.name} regions overlap"
                )

        self._page_size = page_size
        self._regions = tuple(_SparseRegion(spec, page_size) for spec in specs)
        self._specs = tuple(region.spec for region in self._regions)
        self._mmio = mmio

    @property
    def regions(self) -> tuple[RegionSpec, ...]:
        return self._specs

    @property
    def page_size(self) -> int:
        return self._page_size

    @property
    def resident_page_count(self) -> int:
        """Number of ordinary-memory pages materialized by writes."""

        return sum(len(region.pages) for region in self._regions)

    def classify(self, address: int) -> AddressClass | None:
        """Return the mapped class of one byte address, if any."""

        _checked_span(address, 0, operation="classify")
        if MMIO_BASE <= address < MMIO_LIMIT:
            return AddressClass.MMIO
        region = self._region_at(address)
        return None if region is None else region.spec.kind

    def read8(self, address: int) -> int:
        return self._read_integer(address, 1)

    def write8(self, address: int, value: int) -> None:
        self._write_integer(address, value, 1)

    def read16(self, address: int) -> int:
        return self._read_integer(address, 2)

    def write16(self, address: int, value: int) -> None:
        self._write_integer(address, value, 2)

    def read32(self, address: int) -> int:
        return self._read_integer(address, 4)

    def write32(self, address: int, value: int) -> None:
        self._write_integer(address, value, 4)

    def read64(self, address: int) -> int:
        return self._read_integer(address, 8)

    def write64(self, address: int, value: int) -> None:
        self._write_integer(address, value, 8)

    def read_bytes(self, address: int, length: int) -> bytes:
        """Read one complete single-region byte span."""

        resolved = self._resolve(address, length, operation="read")
        if resolved is None:
            return b""
        if resolved.kind is AddressClass.MMIO:
            raise MMIOAccessError(
                "block reads cannot target MMIO",
                operation="read",
                address=resolved.address,
                length=resolved.length,
            )
        assert resolved.region is not None
        return resolved.region.read(resolved.offset, resolved.length)

    def write_bytes(
        self,
        address: int,
        payload: bytes | bytearray | memoryview,
    ) -> None:
        """Write one complete single-region byte span after preflight."""

        if not isinstance(payload, (bytes, bytearray, memoryview)):
            raise TypeError("payload must be bytes-like")
        raw = bytes(payload)
        resolved = self._resolve(address, len(raw), operation="write")
        if resolved is None:
            return
        if resolved.kind is AddressClass.MMIO:
            raise MMIOAccessError(
                "block writes cannot target MMIO",
                operation="write",
                address=resolved.address,
                length=resolved.length,
            )
        assert resolved.region is not None
        resolved.region.write(resolved.offset, raw)

    def fill(self, address: int, length: int, value: int) -> None:
        """Fill a complete span with the low byte of *value*."""

        value = _require_integer(value, label="fill value") & 0xFF
        resolved = self._resolve(address, length, operation="fill")
        if resolved is None:
            return
        if resolved.kind is AddressClass.MMIO:
            raise MMIOAccessError(
                "fills cannot target MMIO",
                operation="fill",
                address=resolved.address,
                length=resolved.length,
            )
        assert resolved.region is not None
        resolved.region.fill(resolved.offset, resolved.length, value)

    def copy_forward(self, source: int, destination: int, length: int) -> None:
        """Copy bytes low-to-high with BIOS ``CMOVE`` overlap semantics."""

        length = _require_nonnegative(length, label="CMOVE length")
        if length == 0:
            return

        try:
            source_span = self._resolve(
                source,
                length,
                operation="CMOVE source",
            )
            destination_span = self._resolve(
                destination,
                length,
                operation="CMOVE destination",
            )
        except MemoryAccessError:
            self._copy_forward_bytes(source, destination, length)
            return

        assert source_span is not None
        assert destination_span is not None
        if (
            source_span.kind is AddressClass.MMIO
            or destination_span.kind is AddressClass.MMIO
        ):
            self._copy_forward_bytes(source, destination, length)
            return
        assert source_span.region is not None
        assert destination_span.region is not None

        destructive_overlap = (
            source_span.region is destination_span.region
            and source_span.offset < destination_span.offset
            and destination_span.offset < source_span.offset + length
        )
        if destructive_overlap:
            stride = destination_span.offset - source_span.offset
            seed = source_span.region.read(source_span.offset, stride)
            payload = (seed * ((length + stride - 1) // stride))[:length]
        else:
            payload = source_span.region.read(source_span.offset, length)
        destination_span.region.write(destination_span.offset, payload)

    def _copy_forward_bytes(
        self,
        source: int,
        destination: int,
        length: int,
    ) -> None:
        for offset in range(length):
            self.write8(destination + offset, self.read8(source + offset))

    def _read_integer(self, address: int, width: int) -> int:
        if width not in _INTEGER_WIDTHS:
            raise ValueError("integer width must be 1, 2, 4, or 8 bytes")
        resolved = self._resolve(address, width, operation="read")
        assert resolved is not None
        if resolved.kind is AddressClass.MMIO:
            payload = self._mmio_read(resolved)
        else:
            assert resolved.region is not None
            payload = resolved.region.read(resolved.offset, resolved.length)
        return int.from_bytes(payload, "little")

    def _write_integer(self, address: int, value: int, width: int) -> None:
        if width not in _INTEGER_WIDTHS:
            raise ValueError("integer width must be 1, 2, 4, or 8 bytes")
        value = _require_integer(value, label="stored value")
        mask = (1 << (width * 8)) - 1
        payload = (value & mask).to_bytes(width, "little")
        resolved = self._resolve(address, width, operation="write")
        assert resolved is not None
        if resolved.kind is AddressClass.MMIO:
            self._mmio_write(resolved, payload)
            return
        assert resolved.region is not None
        resolved.region.write(resolved.offset, payload)

    def _region_at(self, address: int) -> _SparseRegion | None:
        for region in self._regions:
            if region.spec.base <= address < region.spec.limit:
                return region
        return None

    def _resolve(
        self,
        address: int,
        length: int,
        *,
        operation: str,
    ) -> _ResolvedSpan | None:
        end = _checked_span(address, length, operation=operation)
        if length == 0:
            return None

        if MMIO_BASE <= address < MMIO_LIMIT:
            if end > MMIO_LIMIT:
                raise CrossRegionAccessError(
                    f"{operation} span escapes the MMIO aperture",
                    operation=operation,
                    address=address,
                    length=length,
                )
            return _ResolvedSpan(
                AddressClass.MMIO,
                address,
                length,
                None,
                address - MMIO_BASE,
            )

        region = self._region_at(address)
        if region is None:
            raise UnmappedAddressError(
                f"{operation} begins at an unmapped guest address",
                operation=operation,
                address=address,
                length=length,
            )
        if end > region.spec.limit:
            raise CrossRegionAccessError(
                f"{operation} span escapes the {region.spec.kind.name} region",
                operation=operation,
                address=address,
                length=length,
            )
        return _ResolvedSpan(
            region.spec.kind,
            address,
            length,
            region,
            address - region.spec.base,
        )

    def _mmio_preflight(self, resolved: _ResolvedSpan, *, write: bool) -> MMIOPort:
        operation = "write" if write else "read"
        port = self._mmio
        if port is None:
            raise MMIOAccessError(
                f"no MMIO service is installed for {operation}",
                operation=operation,
                address=resolved.address,
                length=resolved.length,
            )
        try:
            port.preflight(resolved.offset, resolved.length, write=write)
        except Exception as exc:
            raise MMIOAccessError(
                f"MMIO service rejected {operation} preflight",
                operation=operation,
                address=resolved.address,
                length=resolved.length,
            ) from exc
        return port

    def _mmio_read(self, resolved: _ResolvedSpan) -> bytearray:
        port = self._mmio_preflight(resolved, write=False)
        result = bytearray(resolved.length)
        for index in range(resolved.length):
            try:
                value = port.read8(resolved.offset + index)
                if not isinstance(value, int) or not 0 <= value <= 0xFF:
                    raise ValueError("MMIO read8 did not return a byte")
                result[index] = value
            except Exception as exc:
                raise MMIOAccessError(
                    "MMIO service failed during read",
                    operation="read",
                    address=resolved.address,
                    length=resolved.length,
                ) from exc
        return result

    def _mmio_write(self, resolved: _ResolvedSpan, payload: bytes) -> None:
        port = self._mmio_preflight(resolved, write=True)
        for index, value in enumerate(payload):
            try:
                port.write8(resolved.offset + index, value)
            except Exception as exc:
                raise MMIOAccessError(
                    "MMIO service failed during write",
                    operation="write",
                    address=resolved.address,
                    length=resolved.length,
                ) from exc


@dataclass(frozen=True, slots=True)
class _Allocation:
    size: int
    alignment: int


class RegionAllocator:
    """Deterministic first-fit allocator over one caller-selected RAM interval."""

    def __init__(
        self,
        memory: SparseAddressSpace,
        base: int,
        limit: int,
        *,
        default_alignment: int = 8,
    ) -> None:
        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("memory must be a SparseAddressSpace")
        base = _require_integer(base, label="allocator base")
        limit = _require_integer(limit, label="allocator limit")
        if not 0 <= base <= MASK64:
            raise ValueError("allocator base is outside the uint64 address space")
        if not base < limit <= ADDRESS_SPACE_SIZE:
            raise ValueError(
                "allocator limit must be above base within uint64 geometry"
            )
        try:
            alignment = self._validate_alignment(
                default_alignment,
                operation="configure",
            )
        except InvalidAllocationError as exc:
            raise ValueError(str(exc)) from exc

        try:
            resolved = memory._resolve(base, limit - base, operation="allocate")
        except MemoryAccessError as exc:
            raise ValueError(
                "allocator interval must fit one mapped ordinary-memory region"
            ) from exc
        assert resolved is not None
        if resolved.kind is AddressClass.MMIO:
            raise ValueError("allocator interval must use ordinary memory")

        self.memory = memory
        self.base = base
        self.limit = limit
        self.default_alignment = alignment
        self._free: list[tuple[int, int]] = [(base, limit)]
        self._allocations: dict[int, _Allocation] = {}

    @property
    def free_bytes(self) -> int:
        return sum(limit - base for base, limit in self._free)

    @property
    def live_allocations(self) -> tuple[tuple[int, int], ...]:
        return tuple(
            (address, allocation.size)
            for address, allocation in sorted(self._allocations.items())
        )

    def allocate(self, size: int, *, alignment: int | None = None) -> int | None:
        size = self._validate_size(size, operation="allocate")
        requested_alignment = self._alignment_or_default(
            alignment,
            operation="allocate",
        )

        for index, (free_base, free_limit) in enumerate(self._free):
            aligned_base = self._align_up(free_base, requested_alignment)
            allocation_limit = aligned_base + size
            if allocation_limit > free_limit:
                continue

            replacement: list[tuple[int, int]] = []
            if free_base < aligned_base:
                replacement.append((free_base, aligned_base))
            if allocation_limit < free_limit:
                replacement.append((allocation_limit, free_limit))
            self._free[index : index + 1] = replacement
            self._allocations[aligned_base] = _Allocation(size, requested_alignment)
            return aligned_base
        return None

    def allocation_size(self, address: int) -> int:
        return self._require_live(address, operation="inspect").size

    def free(self, address: int) -> None:
        allocation = self._require_live(address, operation="free")
        del self._allocations[address]
        self._insert_free(address, address + allocation.size)

    def resize(
        self,
        address: int,
        size: int,
        *,
        alignment: int | None = None,
    ) -> int | None:
        allocation = self._require_live(address, operation="resize")
        size = self._validate_size(size, operation="resize")
        requested_alignment = (
            allocation.alignment
            if alignment is None
            else self._validate_alignment(alignment, operation="resize")
        )

        if address % requested_alignment == 0:
            if size == allocation.size:
                self._allocations[address] = _Allocation(size, requested_alignment)
                return address
            if size < allocation.size:
                old_limit = address + allocation.size
                new_limit = address + size
                self._allocations[address] = _Allocation(size, requested_alignment)
                self._insert_free(new_limit, old_limit)
                return address

            additional = size - allocation.size
            old_limit = address + allocation.size
            if self._consume_adjacent(old_limit, additional):
                self._allocations[address] = _Allocation(size, requested_alignment)
                return address

        replacement = self.allocate(size, alignment=requested_alignment)
        if replacement is None:
            return None
        try:
            preserved = self.memory.read_bytes(address, min(allocation.size, size))
            self.memory.write_bytes(replacement, preserved)
        except BaseException:
            self.free(replacement)
            raise
        self.free(address)
        return replacement

    @staticmethod
    def _align_up(value: int, alignment: int) -> int:
        return (value + alignment - 1) & -alignment

    @staticmethod
    def _validate_alignment(alignment: int, *, operation: str) -> int:
        if not isinstance(alignment, int):
            raise TypeError("allocation alignment must be an integer")
        if alignment <= 0 or alignment & (alignment - 1):
            raise InvalidAllocationError(
                "allocation alignment must be a positive power of two",
                operation=operation,
            )
        return alignment

    def _alignment_or_default(self, alignment: int | None, *, operation: str) -> int:
        if alignment is None:
            return self.default_alignment
        return self._validate_alignment(alignment, operation=operation)

    @staticmethod
    def _validate_size(size: int, *, operation: str) -> int:
        if not isinstance(size, int):
            raise TypeError("allocation size must be an integer")
        if size <= 0 or size > ADDRESS_SPACE_SIZE:
            raise InvalidAllocationError(
                "allocation size must be positive and fit uint64 geometry",
                operation=operation,
                size=size,
            )
        return size

    def _require_live(self, address: int, *, operation: str) -> _Allocation:
        if not isinstance(address, int):
            raise TypeError("allocation address must be an integer")
        allocation = self._allocations.get(address)
        if allocation is None:
            raise InvalidAllocationError(
                "allocation address is not the base of a live allocation",
                operation=operation,
                address=address,
            )
        return allocation

    def _consume_adjacent(self, address: int, size: int) -> bool:
        for index, (free_base, free_limit) in enumerate(self._free):
            if free_base != address:
                continue
            if size > free_limit - free_base:
                return False
            consumed_limit = free_base + size
            if consumed_limit == free_limit:
                del self._free[index]
            else:
                self._free[index] = (consumed_limit, free_limit)
            return True
        return False

    def _insert_free(self, base: int, limit: int) -> None:
        if base >= limit:
            return
        index = 0
        while index < len(self._free) and self._free[index][0] < base:
            index += 1

        if index > 0 and self._free[index - 1][1] > base:
            raise RuntimeError("allocator free spans overlap")
        if index < len(self._free) and limit > self._free[index][0]:
            raise RuntimeError("allocator free spans overlap")

        if index > 0 and self._free[index - 1][1] == base:
            index -= 1
            base = self._free[index][0]
            del self._free[index]
        if index < len(self._free) and limit == self._free[index][0]:
            limit = self._free[index][1]
            del self._free[index]
        self._free.insert(index, (base, limit))


__all__ = [
    "ADDRESS_SPACE_SIZE",
    "BANK0_BASE",
    "BANK0_DEFAULT_SIZE",
    "DEFAULT_PAGE_SIZE",
    "EXTERNAL_BASE",
    "HBW_BASE",
    "MMIO_BASE",
    "MMIO_LIMIT",
    "VRAM_BASE",
    "AddressClass",
    "AddressOverflowError",
    "CrossRegionAccessError",
    "InvalidAllocationError",
    "MMIOAccessError",
    "MMIOPort",
    "MemoryAccessError",
    "RegionAllocator",
    "RegionSpec",
    "SparseAddressSpace",
    "UnmappedAddressError",
]
