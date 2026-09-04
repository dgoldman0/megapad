"""Focused sparse-memory and bounded-allocation tests."""

from __future__ import annotations

from dataclasses import dataclass, field

import pytest

from shared.cells import MASK64
from simulator.memory import (
    ADDRESS_SPACE_SIZE,
    EXTERNAL_BASE,
    HBW_BASE,
    MMIO_BASE,
    MMIO_LIMIT,
    VRAM_BASE,
    AddressClass,
    AddressOverflowError,
    CrossRegionAccessError,
    InvalidAllocationError,
    MMIOAccessError,
    RegionAllocator,
    SparseAddressSpace,
    UnmappedAddressError,
)


@dataclass
class RecordingMMIO:
    reject: bool = False
    values: dict[int, int] = field(default_factory=dict)
    events: list[tuple[object, ...]] = field(default_factory=list)

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        self.events.append(("preflight", offset, width, write))
        if self.reject:
            raise RuntimeError("rejected")

    def read8(self, offset: int) -> int:
        self.events.append(("read", offset))
        return self.values.get(offset, 0)

    def write8(self, offset: int, value: int) -> None:
        self.events.append(("write", offset, value))
        self.values[offset] = value


def test_canonical_regions_are_distinct_zero_initialized_address_classes() -> None:
    memory = SparseAddressSpace(
        bank0_size=0x100,
        external_size=0x100,
        vram_size=0x100,
        hbw_size=0x100,
        page_size=16,
    )
    addresses = (
        (0, AddressClass.BANK0, 0x11),
        (EXTERNAL_BASE, AddressClass.EXTERNAL, 0x22),
        (VRAM_BASE, AddressClass.VRAM, 0x33),
        (HBW_BASE, AddressClass.HBW, 0x44),
    )

    assert memory.resident_page_count == 0
    for address, kind, _value in addresses:
        assert memory.classify(address) is kind
        assert memory.read8(address) == 0
    assert memory.resident_page_count == 0

    for address, _kind, value in addresses:
        memory.write8(address, value)
    assert [memory.read8(address) for address, _kind, _value in addresses] == [
        0x11,
        0x22,
        0x33,
        0x44,
    ]


def test_optional_physical_regions_default_absent_without_bank0_aliasing() -> None:
    memory = SparseAddressSpace()

    for address in (EXTERNAL_BASE, VRAM_BASE, HBW_BASE):
        assert memory.classify(address) is None
        with pytest.raises(UnmappedAddressError):
            memory.read8(address)


def test_large_advertised_region_remains_sparse_until_written() -> None:
    memory = SparseAddressSpace(
        bank0_size=0,
        external_size=1 << 30,
        page_size=4096,
    )

    assert memory.read8(EXTERNAL_BASE + (1 << 30) - 1) == 0
    assert memory.resident_page_count == 0
    memory.fill(EXTERNAL_BASE + 0x1234, 32, 0)
    assert memory.resident_page_count == 0


def test_sparse_pages_materialize_only_on_write_across_fixed_boundaries() -> None:
    memory = SparseAddressSpace(bank0_size=0x1000, page_size=8)

    assert memory.read64(7) == 0
    assert memory.resident_page_count == 0

    memory.write64(7, 0x8877_6655_4433_2211)

    assert memory.resident_page_count == 2
    assert memory.read64(7) == 0x8877_6655_4433_2211


def test_unaligned_integer_access_is_exact_little_endian_and_truncating() -> None:
    memory = SparseAddressSpace(bank0_size=0x40, page_size=16)

    memory.write64(3, 0x8877_6655_4433_2211)
    assert memory.read_bytes(3, 8) == bytes.fromhex("11 22 33 44 55 66 77 88")
    assert memory.read64(3) == 0x8877_6655_4433_2211

    memory.write16(13, 0x12345)
    memory.write32(17, -1)
    assert memory.read16(13) == 0x2345
    assert memory.read32(17) == 0xFFFF_FFFF


def test_span_boundaries_reject_wrap_and_crossing_before_mutation() -> None:
    memory = SparseAddressSpace(bank0_size=16, page_size=8)
    memory.fill(0, 16, 0xA5)
    before = memory.read_bytes(0, 16)

    with pytest.raises(CrossRegionAccessError) as crossing:
        memory.write32(14, 0x1122_3344)
    assert crossing.value.operation == "write"
    assert crossing.value.address == 14
    assert crossing.value.length == 4
    assert memory.read_bytes(0, 16) == before

    with pytest.raises(UnmappedAddressError):
        memory.read8(16)

    assert memory.read_bytes(MASK64, 0) == b""
    memory.write_bytes(MASK64, b"")
    memory.fill(MASK64, 0, 0xFF)

    with pytest.raises(AddressOverflowError) as wrapped:
        memory.read_bytes(MASK64, 2)
    assert wrapped.value.address == MASK64
    assert wrapped.value.length == 2
    assert wrapped.value.operation == "read"


def test_region_configuration_rejects_invalid_geometry_without_materialization(
) -> None:
    with pytest.raises(ValueError, match="non-negative"):
        SparseAddressSpace(external_size=-1)
    with pytest.raises(ValueError, match="overlap"):
        SparseAddressSpace(
            bank0_size=EXTERNAL_BASE + 1,
            external_size=1,
        )
    with pytest.raises(ValueError, match="MMIO"):
        SparseAddressSpace(bank0_size=MMIO_BASE + 1)
    with pytest.raises(ValueError, match="wraps"):
        SparseAddressSpace(hbw_size=ADDRESS_SPACE_SIZE)


def test_mmio_is_reserved_and_wide_access_uses_one_preflight_then_le_bytes() -> None:
    port = RecordingMMIO()
    memory = SparseAddressSpace(bank0_size=0x100, mmio=port)
    address = MMIO_BASE + 0x20

    assert memory.classify(address) is AddressClass.MMIO
    memory.write32(address, 0x7856_3412)
    assert port.events == [
        ("preflight", 0x20, 4, True),
        ("write", 0x20, 0x12),
        ("write", 0x21, 0x34),
        ("write", 0x22, 0x56),
        ("write", 0x23, 0x78),
    ]

    port.events.clear()
    assert memory.read32(address) == 0x7856_3412
    assert port.events == [
        ("preflight", 0x20, 4, False),
        ("read", 0x20),
        ("read", 0x21),
        ("read", 0x22),
        ("read", 0x23),
    ]
    assert memory.resident_page_count == 0


def test_missing_and_rejected_mmio_fault_without_byte_callbacks() -> None:
    missing = SparseAddressSpace(bank0_size=0x100)
    with pytest.raises(MMIOAccessError) as absent:
        missing.write8(MMIO_BASE + 7, 0x42)
    assert absent.value.offset == 7
    assert absent.value.write is True

    port = RecordingMMIO(reject=True)
    memory = SparseAddressSpace(bank0_size=0x100, mmio=port)
    with pytest.raises(MMIOAccessError) as rejected:
        memory.write64(MMIO_BASE + 8, 0x1122_3344_5566_7788)
    assert rejected.value.address == MMIO_BASE + 8
    assert rejected.value.length == 8
    assert port.events == [("preflight", 8, 8, True)]
    assert port.values == {}

    port.events.clear()
    with pytest.raises(CrossRegionAccessError):
        memory.write16(MMIO_LIMIT - 1, 0x1234)
    assert port.events == []


@pytest.mark.parametrize(
    "operation",
    (
        lambda memory, address: memory.read_bytes(address, 1),
        lambda memory, address: memory.write_bytes(address, b"\x01"),
        lambda memory, address: memory.fill(address, 1, 0x01),
    ),
)
def test_block_memory_operations_never_expand_into_mmio(operation) -> None:
    port = RecordingMMIO()
    memory = SparseAddressSpace(bank0_size=0x100, mmio=port)

    with pytest.raises(MMIOAccessError):
        operation(memory, MMIO_BASE)

    assert port.events == []


def test_forward_copy_preserves_low_to_high_overlap_semantics() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)
    memory.write_bytes(0x20, b"abcdef")

    memory.copy_forward(0x20, 0x21, 5)

    assert memory.read_bytes(0x20, 6) == b"aaaaaa"

    memory.write_bytes(0x20, b"abcdef")
    memory.copy_forward(0x21, 0x20, 5)
    assert memory.read_bytes(0x20, 6) == b"bcdeff"


def test_forward_copy_zero_length_and_mmio_use_byte_transaction_semantics() -> None:
    port = RecordingMMIO(values={0: 0x31, 1: 0x32})
    memory = SparseAddressSpace(bank0_size=0x100, mmio=port)

    memory.copy_forward(MASK64, MASK64, 0)
    assert port.events == []

    memory.copy_forward(MMIO_BASE, 0x20, 2)
    assert memory.read_bytes(0x20, 2) == b"12"
    assert port.events == [
        ("preflight", 0, 1, False),
        ("read", 0),
        ("preflight", 1, 1, False),
        ("read", 1),
    ]


def test_forward_copy_fault_retains_the_completed_low_byte_prefix() -> None:
    memory = SparseAddressSpace(bank0_size=0x24)
    memory.write_bytes(0x10, b"ABCDEFGH")

    with pytest.raises(UnmappedAddressError):
        memory.copy_forward(0x10, 0x20, 8)

    assert memory.read_bytes(0x20, 4) == b"ABCD"


def test_allocator_is_aligned_deterministic_first_fit_and_fully_coalescing() -> None:
    memory = SparseAddressSpace(bank0_size=0x400, page_size=16)
    allocator = RegionAllocator(memory, 0x100, 0x200)

    first = allocator.allocate(5)
    second = allocator.allocate(8)
    third = allocator.allocate(9, alignment=16)
    assert (first, second, third) == (0x100, 0x108, 0x110)

    allocator.free(first)
    assert allocator.allocate(4) == 0x100

    for address, _size in allocator.live_allocations:
        allocator.free(address)
    assert allocator.free_bytes == 0x100
    assert allocator.allocate(0x100, alignment=1) == 0x100


def test_allocator_configuration_requires_one_ordinary_mapped_region() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)

    with pytest.raises(ValueError, match="power of two"):
        RegionAllocator(memory, 0, 0x80, default_alignment=3)
    with pytest.raises(ValueError, match="one mapped ordinary-memory region"):
        RegionAllocator(memory, 0x80, 0x101)
    with pytest.raises(ValueError, match="ordinary memory"):
        RegionAllocator(memory, MMIO_BASE, MMIO_BASE + 0x10)


def test_allocator_reports_exhaustion_and_rejects_invalid_lifetimes() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)
    allocator = RegionAllocator(memory, 0x20, 0x40, default_alignment=1)

    with pytest.raises(InvalidAllocationError) as invalid_size:
        allocator.allocate(0)
    assert invalid_size.value.operation == "allocate"
    assert invalid_size.value.size == 0

    with pytest.raises(InvalidAllocationError, match="power of two"):
        allocator.allocate(1, alignment=3)

    address = allocator.allocate(0x20)
    assert address == 0x20
    assert allocator.allocate(1) is None

    with pytest.raises(InvalidAllocationError) as interior:
        allocator.free(address + 1)
    assert interior.value.operation == "free"
    assert interior.value.address == address + 1

    allocator.free(address)
    with pytest.raises(InvalidAllocationError, match="live allocation"):
        allocator.free(address)


def test_allocator_reuse_does_not_implicitly_clear_guest_bytes() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)
    allocator = RegionAllocator(memory, 0x20, 0x40, default_alignment=1)

    address = allocator.allocate(8)
    assert address is not None
    memory.write_bytes(address, b"retained")
    allocator.free(address)

    assert allocator.allocate(8) == address
    assert memory.read_bytes(address, 8) == b"retained"


def test_resize_moves_or_grows_in_place_and_preserves_payload() -> None:
    memory = SparseAddressSpace(bank0_size=0x100)
    allocator = RegionAllocator(memory, 0x20, 0x80, default_alignment=8)
    original = allocator.allocate(8)
    blocker = allocator.allocate(8)
    assert original == 0x20
    assert blocker == 0x28
    memory.write_bytes(original, b"ABCDEFGH")

    moved = allocator.resize(original, 16)
    assert moved == 0x30
    assert memory.read_bytes(moved, 8) == b"ABCDEFGH"
    with pytest.raises(InvalidAllocationError):
        allocator.allocation_size(original)

    allocator.free(blocker)
    shrunk = allocator.resize(moved, 4)
    assert shrunk == moved
    assert allocator.allocation_size(moved) == 4
    assert memory.read_bytes(moved, 4) == b"ABCD"

    grown = allocator.resize(moved, 12)
    assert grown == moved
    assert memory.read_bytes(grown, 4) == b"ABCD"


def test_failed_resize_is_atomic_for_allocation_and_bytes() -> None:
    memory = SparseAddressSpace(bank0_size=0x40)
    allocator = RegionAllocator(memory, 0x10, 0x20, default_alignment=1)
    original = allocator.allocate(8)
    blocker = allocator.allocate(8)
    assert original == 0x10
    assert blocker == 0x18
    memory.write_bytes(original, b"12345678")

    assert allocator.resize(original, 9) is None
    assert allocator.allocation_size(original) == 8
    assert memory.read_bytes(original, 8) == b"12345678"
    assert allocator.live_allocations == ((0x10, 8), (0x18, 8))
