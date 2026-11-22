"""The hosted platform's attachment table and its devices.

Every MMIO device attaches to one native table, which refuses overlapping
windows and hands each access, with its requester, to the device whose
window holds its first byte. These cases cover the table itself, the Python
adapter, the requester, the lines a device raises, the mailbox that every
profile now has, and System Info built for other topologies.
"""

from __future__ import annotations

import gc
import weakref

import pytest

import _megaforth_native as native
from simulator.chiplet import MAILBOX_ACK, MAILBOX_OFFSET, MAILBOX_SEND, MAILBOX_STATUS
from simulator.memory import MMIO_BASE, MMIOAccessError
from simulator.platform import (
    BOARD_ID_VERSION,
    HostedPlatform,
    PlatformMMIOAccessError,
    Topology,
    create_one_core_address_space,
)
from simulator.aes import HostedAESService
from simulator.entropy import HostedTRNGService
from simulator.rtc import RTC_UPTIME, HostedRTCService
from simulator.runtime import MegaForthRuntime
from simulator.sha3 import HostedSHA3Service


class Recorder:
    """A Python device that records what reaches it."""

    def __init__(self, value: int = 0x5A) -> None:
        self.value = value
        self.calls: list[tuple] = []

    def preflight(self, offset, width, *, write):
        self.calls.append(("preflight", offset, width, write))

    def read8(self, offset):
        self.calls.append(("read8", offset))
        return self.value

    def write8(self, offset, value):
        self.calls.append(("write8", offset, value))


def _table():
    table = native.PlatformTable()
    table.set_refusal(_raise)
    return table


def _raise(name, message, offset, width, write):
    raise PlatformMMIOAccessError(
        f"{name}: {message}", offset=offset, width=width, write=write)


# ---------------------------------------------------------------------------
#  The table
# ---------------------------------------------------------------------------

def test_a_python_device_sees_each_whole_access_then_its_bytes():
    table = _table()
    device = Recorder()
    table.attach("probe", device, [(0x100, 0x110)])

    table.preflight(0x104, 4, True)
    for index in range(4):
        table.write8(0x104 + index, index)
    table.preflight(0x10F, 1, False)
    assert table.read8(0x10F) == 0x5A
    assert device.calls == [
        ("preflight", 0x104, 4, True),
        ("write8", 0x104, 0), ("write8", 0x105, 1),
        ("write8", 0x106, 2), ("write8", 0x107, 3),
        ("preflight", 0x10F, 1, False),
        ("read8", 0x10F),
    ]
    assert table.name_at(0x10F) == "probe"
    assert table.name_at(0x110) is None
    assert table.python_target_at(0x100) is device
    assert table.device("probe") is device


@pytest.mark.parametrize(
    "windows",
    ([(0x108, 0x118)], [(0x0F0, 0x101)], [(0x0F0, 0x200)], [(0x10F, 0x110)]),
)
def test_a_window_that_overlaps_another_is_refused(windows):
    table = _table()
    table.attach("first", Recorder(), [(0x100, 0x110)])
    with pytest.raises(ValueError, match="overlaps"):
        table.attach("second", Recorder(), windows)
    assert table.windows == [(0x100, 0x110, "first")]
    # Touching windows are separate.
    table.attach("second", Recorder(), [(0x110, 0x120), (0x0F0, 0x100)])
    assert [name for _start, _limit, name in table.windows] == [
        "second", "first", "second"]


@pytest.mark.parametrize(
    "windows",
    ([], [(0x100, 0x100)], [(0x200, 0x100)], [(0, (1 << 39) + 1)],
     [(0x100, 0x110), (0x108, 0x120)]),
)
def test_impossible_windows_and_names_are_refused(windows):
    table = _table()
    with pytest.raises(ValueError):
        table.attach("device", Recorder(), windows)
    table.attach("device", Recorder(), [(0x100, 0x110)])
    with pytest.raises(ValueError, match="already attached"):
        table.attach("device", Recorder(), [(0x200, 0x210)])


def test_an_access_outside_every_window_is_refused_through_the_handler():
    table = _table()
    table.attach("device", Recorder(), [(0x100, 0x110)])
    with pytest.raises(PlatformMMIOAccessError) as refused:
        table.preflight(0x0FF, 2, False)
    assert (refused.value.offset, refused.value.width, refused.value.write) == (
        0x0FF, 2, False)
    with pytest.raises(PlatformMMIOAccessError):
        table.write8(0x110, 1)

    bare = native.PlatformTable()
    with pytest.raises(ValueError, match="outside every admitted"):
        bare.preflight(0, 1, False)


def test_a_python_device_returns_a_byte_or_is_refused():
    table = _table()
    table.attach("device", Recorder(value=0x100), [(0x100, 0x110)])
    with pytest.raises(ValueError, match="did not return a byte"):
        table.read8(0x100)
    table.replace("device", Recorder(value=True))
    with pytest.raises(ValueError, match="did not return a byte"):
        table.read8(0x100)


def test_a_device_exception_passes_through_unchanged():
    class Failing(Recorder):
        def preflight(self, offset, width, *, write):
            raise KeyError("failing device")

    table = _table()
    table.attach("device", Failing(), [(0x100, 0x110)])
    with pytest.raises(KeyError, match="failing device"):
        table.preflight(0x100, 1, False)


def test_replacing_a_device_keeps_its_windows():
    table = _table()
    first, second = Recorder(1), Recorder(2)
    table.attach("device", first, [(0x100, 0x110)])
    table.replace("device", second)
    assert table.read8(0x100) == 2
    assert table.device("device") is second
    with pytest.raises(KeyError):
        table.replace("missing", first)


def test_the_requester_reaches_native_devices():
    table = _table()
    mailbox = native.Mailbox(2)
    table.attach("mailbox", mailbox, [(MAILBOX_OFFSET, MAILBOX_OFFSET + 0x10)])
    assert table.requester == 0

    table.write8(MAILBOX_OFFSET, 0x11)           # core 0's data word
    table.requester = 1
    table.write8(MAILBOX_OFFSET, 0x22)           # core 1's
    assert mailbox.data(0) == 0x11 and mailbox.data(1) == 0x22
    assert table.read8(MAILBOX_OFFSET) == 0x22
    table.write8(MAILBOX_SEND, 0)                # core 1 signals core 0
    assert table.lines(0) == native.LINE_IPI
    assert mailbox.data(0) == 0x22               # the target takes the word

    table.requester = 0
    assert table.read8(MAILBOX_STATUS) == 0b10
    table.write8(MAILBOX_ACK, 1)
    assert table.lines(0) == 0

    # A requester that names no core reads 0 and changes nothing.
    table.requester = 7
    assert table.read8(MAILBOX_OFFSET) == 0
    table.write8(MAILBOX_SEND, 0)
    assert mailbox.table.pending_mask(0) == 0
    with pytest.raises(ValueError):
        table.requester = 256


def test_the_table_and_its_devices_are_collected():
    memory = create_one_core_address_space(bank0_size=0x1000)
    probe = weakref.ref(memory.mmio)
    table = weakref.ref(memory.mmio.table)
    del memory
    gc.collect()
    assert probe() is None and table() is None


# ---------------------------------------------------------------------------
#  The platform
# ---------------------------------------------------------------------------

def test_the_platform_attaches_every_window_once():
    memory = create_one_core_address_space(external_size=0x4000, chiplet_ports=1)
    names = [name for _start, _limit, name in memory.mmio.table.windows]
    assert names == [
        "sysinfo", "mailbox", "aes", "sha3", "entropy", "rtc", "rtc", "audio",
        "chiplet",
    ]
    # The mailbox works over the table the ports ring through.
    platform = memory.mmio
    assert platform.mailbox.table.port_count == 1
    platform.chiplet.mailbox.send(1, 0)
    assert memory.read8(MMIO_BASE + MAILBOX_STATUS) == 0b10
    assert platform.table.lines(0) == native.LINE_IPI | native.LINE_EXTERNAL_FLAG


def test_assigning_a_service_reroutes_its_windows():
    memory = create_one_core_address_space()
    platform = memory.mmio
    replacement = HostedRTCService(initial_uptime_ms=0x1234)
    platform.rtc = replacement
    assert platform.rtc is replacement
    assert platform.table.python_target_at(RTC_UPTIME) is replacement
    assert memory.read16(MMIO_BASE + RTC_UPTIME) == 0x1234
    with pytest.raises(TypeError):
        platform.rtc = HostedAESService()


@pytest.mark.parametrize(
    ("full_cores", "clusters", "cores", "enabled", "ports"),
    ((2, 0, 2, 0, 5), (4, 0, 4, 0, 7), (4, 3, 16, 0b111, 10), (1, 1, 5, 0b1, 5)),
)
def test_system_info_reports_a_topology(full_cores, clusters, cores, enabled, ports):
    platform = HostedPlatform(
        aes=HostedAESService(), sha3=HostedSHA3Service(6),
        entropy=HostedTRNGService(), rtc=HostedRTCService(None),
        topology=Topology(full_cores, clusters),
    )
    from simulator.memory import SparseAddressSpace

    memory = SparseAddressSpace(bank0_size=0x1000, mmio=platform)
    platform.bind(memory)
    registers = platform.sysinfo.registers
    assert registers[0] == BOARD_ID_VERSION
    assert (registers[2], registers[3], registers[9], registers[13]) == (
        cores, enabled, full_cores, ports)
    assert platform.mailbox.core_count == cores
    # Runtime construction still admits one full core alone.
    with pytest.raises(ValueError, match="exactly one advertised full core"):
        MegaForthRuntime(memory=memory)


def test_the_mailbox_window_keeps_its_shape():
    memory = create_one_core_address_space()
    with pytest.raises(MMIOAccessError):
        memory.read16(MMIO_BASE + MAILBOX_OFFSET + 1)
    with pytest.raises(MMIOAccessError):
        memory.read16(MMIO_BASE + MAILBOX_OFFSET + 0x0F)
    memory.write32(MMIO_BASE + MAILBOX_OFFSET + 4, 0xA1B2C3D4)
    assert memory.read64(MMIO_BASE + MAILBOX_OFFSET) == 0xA1B2C3D4 << 32
    assert memory.read8(MMIO_BASE + MAILBOX_SEND) == 0
