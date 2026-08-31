"""Focused one-core hosted-platform MMIO tests."""

from __future__ import annotations

import pytest

from simulator.memory import (
    MMIO_BASE,
    AddressClass,
    MMIOAccessError,
    SparseAddressSpace,
)
from simulator.platform import (
    BOARD_ID_VERSION,
    HOSTED_CRYPTO_CAPABILITIES,
    SYSINFO_LIMIT,
    SYSINFO_OFFSET,
    OneCoreSysInfo,
    PlatformMMIOAccessError,
    SysInfoAccessError,
    create_one_core_address_space,
)


SYSINFO_ADDRESS = MMIO_BASE + SYSINFO_OFFSET


def _register(memory, offset: int) -> int:
    return memory.read64(SYSINFO_ADDRESS + offset)


def test_sysinfo_reports_the_returned_address_spaces_actual_geometry() -> None:
    memory = create_one_core_address_space(
        bank0_size=0x2_3000,
        external_size=0x4_5000,
        vram_size=0x6_7000,
        hbw_size=0x8_9000,
        page_size=256,
    )
    regions = {spec.kind: spec for spec in memory.regions}

    assert _register(memory, 0x00) == BOARD_ID_VERSION
    assert _register(memory, 0x08) == regions[AddressClass.BANK0].size
    assert _register(memory, 0x10) == 1
    assert _register(memory, 0x18) == 0
    assert _register(memory, 0x20) == regions[AddressClass.HBW].base
    assert _register(memory, 0x28) == regions[AddressClass.HBW].size
    assert _register(memory, 0x30) == (
        regions[AddressClass.BANK0].size + regions[AddressClass.HBW].size
    )
    assert _register(memory, 0x38) == regions[AddressClass.EXTERNAL].base
    assert _register(memory, 0x40) == regions[AddressClass.EXTERNAL].size
    assert _register(memory, 0x48) == 1
    assert _register(memory, 0x50) == regions[AddressClass.VRAM].base
    assert _register(memory, 0x58) == regions[AddressClass.VRAM].size
    assert _register(memory, 0x60) == HOSTED_CRYPTO_CAPABILITIES
    assert _register(memory, 0x68) == 1


def test_absent_optional_regions_are_not_advertised_from_default_constants() -> None:
    memory = create_one_core_address_space(bank0_size=0x1_2340)

    assert _register(memory, 0x08) == 0x1_2340
    assert _register(memory, 0x20) == 0
    assert _register(memory, 0x28) == 0
    assert _register(memory, 0x30) == 0x1_2340
    assert _register(memory, 0x38) == 0
    assert _register(memory, 0x40) == 0
    assert _register(memory, 0x50) == 0
    assert _register(memory, 0x58) == 0


def test_crypto_capability_profile_accepts_only_admitted_hosted_bits() -> None:
    memory = create_one_core_address_space(crypto_capabilities=0)

    assert _register(memory, 0x60) == 0
    for capabilities in (1, 2, 4, HOSTED_CRYPTO_CAPABILITIES):
        memory = create_one_core_address_space(
            crypto_capabilities=capabilities
        )
        assert _register(memory, 0x60) == capabilities

    with pytest.raises(ValueError, match="unimplemented crypto bits"):
        create_one_core_address_space(crypto_capabilities=8)


def test_exact_window_supports_little_endian_naturally_aligned_reads() -> None:
    memory = create_one_core_address_space(bank0_size=0x12_3456)

    assert memory.read8(SYSINFO_ADDRESS) == 0x01
    assert memory.read16(SYSINFO_ADDRESS) == 0x0001
    assert memory.read32(SYSINFO_ADDRESS) == 0x0002_0001
    assert memory.read32(SYSINFO_ADDRESS + 4) == 0x4D50_3634
    assert memory.read64(SYSINFO_ADDRESS) == BOARD_ID_VERSION
    assert memory.read64(MMIO_BASE + SYSINFO_LIMIT - 8) == 1

    assert len(
        bytes(
            memory.read8(MMIO_BASE + offset)
            for offset in range(SYSINFO_OFFSET, SYSINFO_LIMIT)
        )
    ) == 0x70


@pytest.mark.parametrize(
    ("offset", "width", "error_type"),
    (
        (SYSINFO_OFFSET - 1, 1, PlatformMMIOAccessError),
        (SYSINFO_LIMIT, 1, PlatformMMIOAccessError),
        (SYSINFO_OFFSET + 1, 2, SysInfoAccessError),
        (SYSINFO_LIMIT - 1, 2, SysInfoAccessError),
    ),
)
def test_unmapped_misaligned_and_crossing_reads_fail_in_whole_access_preflight(
    offset: int,
    width: int,
    error_type: type[ValueError],
) -> None:
    memory = create_one_core_address_space(bank0_size=0x1000)
    read = {1: memory.read8, 2: memory.read16}[width]

    with pytest.raises(MMIOAccessError, match="preflight") as exc_info:
        read(MMIO_BASE + offset)

    cause = exc_info.value.__cause__
    assert isinstance(cause, error_type)
    assert cause.offset == offset
    assert cause.width == width
    assert cause.write is False


def test_every_sysinfo_write_is_rejected_in_preflight_without_mutation() -> None:
    memory = create_one_core_address_space(bank0_size=0x2_0000)
    before = _register(memory, 0x18)

    with pytest.raises(MMIOAccessError, match="preflight") as exc_info:
        memory.write64(SYSINFO_ADDRESS + 0x18, 0xFFFF_FFFF_FFFF_FFFF)

    cause = exc_info.value.__cause__
    assert isinstance(cause, SysInfoAccessError)
    assert cause.offset == SYSINFO_OFFSET + 0x18
    assert cause.width == 8
    assert cause.write is True
    assert _register(memory, 0x18) == before


def test_direct_service_callbacks_reject_bypassed_writes_and_outside_bytes() -> None:
    service = OneCoreSysInfo()
    memory = SparseAddressSpace(bank0_size=0x1000, mmio=service)
    service.bind(memory)

    with pytest.raises(SysInfoAccessError) as write_error:
        service.write8(SYSINFO_OFFSET, 0xFF)
    assert write_error.value.write is True

    with pytest.raises(SysInfoAccessError) as read_error:
        service.read8(SYSINFO_LIMIT)
    assert read_error.value.write is False


def test_sysinfo_binding_is_single_assignment() -> None:
    service = OneCoreSysInfo()
    memory = SparseAddressSpace(bank0_size=0x1000, mmio=service)
    service.bind(memory)

    with pytest.raises(RuntimeError, match="already bound"):
        service.bind(memory)
