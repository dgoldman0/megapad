import pytest

from asm import assemble
from devices import (
    BusError,
    CRYPTO_CAP_CRC_REFLECT_RAW,
    CRYPTO_CAP_KECCAK_F1600,
    CRYPTO_CAP_SHA3_STREAM,
    DeviceBus,
    SYSINFO_BASE,
    SystemInfo,
)
from megapad64 import IVEC_BUS_FAULT, TrapError
from system import MMIO_START, MegapadSystem


def _read_le(bus: DeviceBus, offset: int, width: int) -> int:
    bus.preflight_access(offset, width)
    return sum(bus.read8(offset + i) << (8 * i) for i in range(width))


def test_crypto_capability_and_requester_count_extend_exact_window():
    bus = DeviceBus()
    sysinfo = SystemInfo(
        crypto_caps=CRYPTO_CAP_CRC_REFLECT_RAW,
        num_bus_ports=9,
    )
    bus.register(sysinfo)

    assert sysinfo.size == 0x70
    assert _read_le(bus, SYSINFO_BASE + 0x60, 8) == CRYPTO_CAP_CRC_REFLECT_RAW
    assert _read_le(bus, SYSINFO_BASE + 0x68, 8) == 9
    with pytest.raises(BusError):
        bus.read8(SYSINFO_BASE + 0x70)


@pytest.mark.parametrize(
    ("offset", "width"),
    [
        (0x61, 2),
        (0x62, 4),
        (0x64, 8),
        (0x6F, 2),
        (0x70, 1),
    ],
)
def test_sysinfo_preflight_rejects_misaligned_crossing_and_outside_spans(
    offset: int,
    width: int,
):
    bus = DeviceBus()
    bus.register(SystemInfo())

    with pytest.raises(BusError) as exc_info:
        bus.preflight_access(SYSINFO_BASE + offset, width)

    assert exc_info.value.offset == SYSINFO_BASE + offset


def test_capability_and_topology_register_writes_are_ignored():
    sysinfo = SystemInfo(
        crypto_caps=CRYPTO_CAP_CRC_REFLECT_RAW,
        num_bus_ports=7,
    )

    for offset in range(0x60, 0x70):
        sysinfo.write8(offset, 0xFF)

    assert sysinfo.crypto_caps == CRYPTO_CAP_CRC_REFLECT_RAW
    assert sysinfo.num_bus_ports == 7
    assert sysinfo.read8(0x60) == CRYPTO_CAP_CRC_REFLECT_RAW
    assert sysinfo.read8(0x68) == 7


def test_sysinfo_rejects_reserved_capability_bits_and_zero_requesters():
    with pytest.raises(ValueError, match="reserved bits"):
        SystemInfo(crypto_caps=1 << 4)
    with pytest.raises(ValueError, match="must be positive"):
        SystemInfo(num_bus_ports=0)


def _system() -> MegapadSystem:
    return MegapadSystem(
        ram_size=4096,
        num_cores=2,
        num_clusters=1,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )


def test_system_reports_qualified_crypto_and_actual_bus_requesters():
    system = _system()
    caps_addr = MMIO_START + SYSINFO_BASE + 0x60
    ports_addr = MMIO_START + SYSINFO_BASE + 0x68

    assert system.cpu.mem_read64(caps_addr) == (
        CRYPTO_CAP_CRC_REFLECT_RAW
        | CRYPTO_CAP_SHA3_STREAM
        | CRYPTO_CAP_KECCAK_F1600
    )
    assert system.cpu.mem_read64(ports_addr) == 6  # 2 cores + cluster + NIC + disk + WOTS


def test_python_wide_mmio_preflight_prevents_partial_sysinfo_write():
    system = _system()
    address = MMIO_START + SYSINFO_BASE + 0x19
    cluster_en_before = system.sysinfo.cluster_en

    with pytest.raises(TrapError) as exc_info:
        system.cpu.mem_write16(address, 0xFFFF)

    assert exc_info.value.ivec_id == IVEC_BUS_FAULT
    assert system.sysinfo.cluster_en == cluster_en_before
    assert system.cpu.trap_addr == address


def test_native_fallback_preflights_exact_sysinfo_span_before_callbacks():
    system = _system()
    cpu = system.cpu
    address = MMIO_START + SYSINFO_BASE + 0x19
    cluster_en_before = system.sysinfo.cluster_en
    code = assemble(
        "ldi r1, 0xFF\n"
        f"ldi64 r2, {address}\n"
        "st.h r2, r1\n"
        "halt\n"
    )
    cpu.load_bytes(0, code)
    cpu.pc = 0

    cpu.step()
    cpu.step()
    with pytest.raises(TrapError) as exc_info:
        cpu.step()

    assert exc_info.value.ivec_id == IVEC_BUS_FAULT
    assert system.sysinfo.cluster_en == cluster_en_before
    assert cpu.trap_addr == address
