"""Fixed shared backing keeps the existing guest-memory contract."""

from __future__ import annotations

import gc

import pytest

from shared.cells import MASK64
from shared.memory_backing import DenseMemoryBacking
from simulator.memory import (
    EXTERNAL_BASE, HBW_BASE, MMIO_BASE, VRAM_BASE,
    AddressClass, AddressOverflowError, CrossRegionAccessError,
    MMIOAccessError, SparseAddressSpace, UnmappedAddressError,
)
from simulator.platform import SYSINFO_BANK0_SIZE, create_one_core_address_space
from tests.simulator.test_memory import RecordingMMIO


def _memory(dense, **kwargs):
    sparse = SparseAddressSpace(**kwargs)
    if not dense:
        return sparse
    backing = DenseMemoryBacking((region.base, region.size)
                                 for region in sparse.regions)
    return SparseAddressSpace(**kwargs, dense_backing=backing)


def test_backing_exports_distinct_zeroed_regions_and_independent_pinned_views():
    backing = DenseMemoryBacking(((0, 32), (EXTERNAL_BASE, 17)))
    assert backing.regions == ((0, 32), (EXTERNAL_BASE, 17))
    first = backing.buffer_at(0)
    sibling = backing.buffer_at(0)
    external = backing.buffer_at(EXTERNAL_BASE)
    assert first is not sibling
    assert first.obj is sibling.obj
    assert first.obj is not external.obj
    assert first.ndim == first.itemsize == 1
    assert first.c_contiguous and not first.readonly
    assert bytes(first) == bytes(32)
    assert bytes(external) == bytes(17)
    first[3] = 0xA5
    assert sibling[3] == 0xA5
    assert external[3] == 0
    exporter = first.obj
    first.release()
    assert sibling[3] == 0xA5
    sibling.release()
    # No client view remains on this exporter; the owner's private pin alone
    # must still make its capacity immutable.
    with pytest.raises(BufferError):
        exporter.append(0)
    replacement = backing.buffer_at(0)
    assert replacement[3] == 0xA5
    replacement.release()
    external.release()


def test_memory_keeps_backing_alive_and_shares_host_and_guest_writes():
    owner = DenseMemoryBacking(((0, 64),))
    first = SparseAddressSpace(bank0_size=64, dense_backing=owner)
    second = SparseAddressSpace(bank0_size=64, dense_backing=owner)
    assert type(first) is SparseAddressSpace
    assert first.dense_backing is second.dense_backing is owner
    view = owner.buffer_at(0)
    exporter = view.obj
    view[7:15] = bytes.fromhex("1122334455667788")
    assert first.read64(7) == second.read64(7) == 0x8877665544332211
    first.write16(9, 0xAABB)
    assert bytes(view[7:15]) == bytes.fromhex("1122bbaa55667788")
    assert second.read16(9) == 0xAABB
    view.release()
    del view, owner, first
    gc.collect()
    second.write8(63, 0x7E)
    assert exporter[63] == second.read8(63) == 0x7E
    with pytest.raises(BufferError):
        exporter.extend(b"x")
    del second
    gc.collect()
    exporter.extend(b"x")
    assert len(exporter) == 65


@pytest.mark.parametrize("dense", [False, True], ids=["sparse", "dense"])
def test_zero_reads_unaligned_scalars_and_partial_final_regions(dense):
    memory = _memory(dense, bank0_size=65, external_size=33,
                     vram_size=17, hbw_size=8, page_size=16)
    assert (memory.dense_backing is not None) is dense
    expected_pages = 5 + 3 + 2 + 1
    assert memory.resident_page_count == (expected_pages if dense else 0)
    for base, kind, size in ((0, AddressClass.BANK0, 65),
                             (EXTERNAL_BASE, AddressClass.EXTERNAL, 33),
                             (VRAM_BASE, AddressClass.VRAM, 17),
                             (HBW_BASE, AddressClass.HBW, 8)):
        assert memory.classify(base) is kind
        assert memory.read_bytes(base, size) == bytes(size)
        memory.write8(base + size - 1, size)
        assert memory.read8(base + size - 1) == size
    memory.write64(15, 0x8877665544332211)
    memory.write16(63, 0x12345)
    memory.write32(EXTERNAL_BASE + 15, -1)
    memory.fill(VRAM_BASE + 3, 7, 0x1AB)
    assert memory.read_bytes(15, 8) == bytes.fromhex("1122334455667788")
    assert memory.read64(15) == 0x8877665544332211
    assert memory.read16(63) == 0x2345
    assert memory.read32(EXTERNAL_BASE + 15) == 0xFFFFFFFF
    assert memory.read_bytes(VRAM_BASE + 3, 7) == b"\xAB" * 7
    if dense:
        assert memory.resident_page_count == expected_pages


@pytest.mark.parametrize("action,error", [
    (lambda memory: memory.write64(60, 0x1122334455667788), CrossRegionAccessError),
    (lambda memory: memory.write_bytes(EXTERNAL_BASE + 31, b"abc"), CrossRegionAccessError),
    (lambda memory: memory.read64(MASK64 - 3), AddressOverflowError),
    (lambda memory: memory.write8(VRAM_BASE, 1), UnmappedAddressError),
])
def test_dense_and_sparse_reject_the_same_complete_span_before_mutation(action, error):
    observations = []
    for dense in (False, True):
        memory = _memory(dense, bank0_size=65, external_size=33, page_size=16)
        memory.fill(0, 65, 0xA5)
        memory.fill(EXTERNAL_BASE, 33, 0x5A)
        with pytest.raises(error) as caught:
            action(memory)
        failure = caught.value
        observations.append((type(failure), str(failure), failure.operation,
                             failure.address, failure.length,
                             memory.read_bytes(0, 65),
                             memory.read_bytes(EXTERNAL_BASE, 33)))
    assert observations[0] == observations[1]
    assert observations[0][-2:] == (b"\xA5" * 65, b"\x5A" * 33)


@pytest.mark.parametrize("dense", [False, True], ids=["sparse", "dense"])
@pytest.mark.parametrize("method,source,destination,length,expected", [
    ("copy_forward", 0x20, 0x21, 5, b"aaaaaa"),
    ("copy_backward", 0x22, 0x20, 4, b"efefef"),
    ("move", 0x20, 0x21, 5, b"aabcde"),
    ("move", 0x21, 0x20, 5, b"bcdeff"),
])
def test_overlap_keeps_each_copy_direction(dense, method, source, destination,
                                         length, expected):
    memory = _memory(dense, bank0_size=64, page_size=4)
    memory.write_bytes(0x20, b"abcdef")
    getattr(memory, method)(source, destination, length)
    assert memory.read_bytes(0x20, 6) == expected


@pytest.mark.parametrize("dense", [False, True], ids=["sparse", "dense"])
def test_copy_fault_keeps_completed_prefix_and_empty_spans_touch_nothing(dense):
    port = RecordingMMIO(reject=True)
    memory = _memory(dense, bank0_size=0x24, mmio=port)
    memory.write_bytes(0x10, b"ABCDEFGH")
    with pytest.raises(UnmappedAddressError) as caught:
        memory.copy_forward(0x10, 0x20, 8)
    assert (caught.value.address, caught.value.length) == (0x24, 1)
    assert memory.read_bytes(0x20, 4) == b"ABCD"
    assert memory.read_bytes(MASK64, 0) == b""
    memory.write_bytes(MASK64, b"")
    memory.fill(MASK64, 0, 0xFF)
    memory.copy_forward(MASK64, MMIO_BASE, 0)
    memory.move(MMIO_BASE, MMIO_BASE, 8)
    assert port.events == []


@pytest.mark.parametrize("dense", [False, True], ids=["sparse", "dense"])
def test_mmio_keeps_one_wide_preflight_and_never_becomes_backing(dense):
    port = RecordingMMIO()
    memory = _memory(dense, bank0_size=64, mmio=port)
    assert memory.classify(MMIO_BASE + 0x20) is AddressClass.MMIO
    memory.write32(MMIO_BASE + 0x20, 0x78563412)
    assert port.events == [
        ("preflight", 0x20, 4, True),
        ("write", 0x20, 0x12), ("write", 0x21, 0x34),
        ("write", 0x22, 0x56), ("write", 0x23, 0x78),
    ]
    port.events.clear()
    assert memory.read32(MMIO_BASE + 0x20) == 0x78563412
    assert port.events == [("preflight", 0x20, 4, False),
                           ("read", 0x20), ("read", 0x21),
                           ("read", 0x22), ("read", 0x23)]
    port.events.clear()
    with pytest.raises(MMIOAccessError):
        memory.write_bytes(MMIO_BASE + 0x20, b"x")
    assert port.events == []
    port.reject = True
    with pytest.raises(MMIOAccessError):
        memory.write64(MMIO_BASE + 8, 1)
    assert port.events == [("preflight", 8, 8, True)]
    assert memory.read_bytes(0, 64) == bytes(64)


@pytest.mark.parametrize("geometry,error", [
    (((True, 8),), TypeError),
    (((0, True),), TypeError),
    (((0, 0),), ValueError),
    (((MASK64, 2),), ValueError),
    (((0, 16), (8, 16)), ValueError),
    (((0, 16), (0, 16)), ValueError),
])
def test_backing_rejects_invalid_or_aliasing_geometry(geometry, error):
    with pytest.raises(error):
        DenseMemoryBacking(geometry)


@pytest.mark.parametrize("geometry", [(), ((0, 31),), ((1, 32),),
                                      ((0, 32), (EXTERNAL_BASE, 8))])
def test_memory_requires_exact_configured_geometry(geometry):
    backing = DenseMemoryBacking(geometry)
    with pytest.raises(ValueError):
        SparseAddressSpace(bank0_size=32, dense_backing=backing)


def test_platform_forwards_the_same_owner_and_reports_unchanged_geometry():
    backing = DenseMemoryBacking(((0, 65536), (EXTERNAL_BASE, 31)))
    memory = create_one_core_address_space(bank0_size=65536, external_size=31,
                                           dense_backing=backing)
    assert type(memory) is SparseAddressSpace
    assert memory.dense_backing is backing
    assert memory.read64(MMIO_BASE + SYSINFO_BANK0_SIZE) == 65536
    view = backing.buffer_at(EXTERNAL_BASE)
    view[30] = 0xCC
    assert memory.read8(EXTERNAL_BASE + 30) == 0xCC
    view.release()


def test_dense_convenience_allocates_only_the_configured_topology():
    memory = create_one_core_address_space(bank0_size=65536, external_size=33,
                                           vram_size=17, hbw_size=8,
                                           dense_backing=True)
    owner = memory.dense_backing
    assert type(owner) is DenseMemoryBacking
    assert owner.regions == tuple((region.base, region.size)
                                  for region in memory.regions)
    assert owner.regions == ((0, 65536), (EXTERNAL_BASE, 33),
                             (VRAM_BASE, 17), (HBW_BASE, 8))
    assert memory.classify(EXTERNAL_BASE + 33) is None
    assert memory.read_bytes(EXTERNAL_BASE, 33) == bytes(33)
