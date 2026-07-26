"""Phase 4 Element 5 host-only device-clocking oracles."""

from __future__ import annotations

import pytest

from devices import Device, DeviceBus
from system import MegapadSystem


class _PassiveDevice(Device):
    def __init__(self, name: str, base: int):
        super().__init__(name, base, 1)


class _ClockedDevice(Device):
    def __init__(
        self,
        name: str,
        base: int,
        events: list[tuple[str, int]],
        *,
        fail: bool = False,
    ):
        super().__init__(name, base, 1)
        self._events = events
        self._fail = fail

    def tick(self, cycles: int):
        self._events.append((self.name, cycles))
        if self._fail:
            raise RuntimeError(f"{self.name} tick failed")


def test_bus_ticks_only_concrete_clock_hooks_in_registration_order() -> None:
    events: list[tuple[str, int]] = []
    bus = DeviceBus()
    bus.register(_PassiveDevice("passive-a", 0))
    bus.register(_ClockedDevice("clocked-a", 1, events))
    bus.register(_PassiveDevice("passive-b", 2))
    bus.register(_ClockedDevice("clocked-b", 3, events))

    bus.tick(7)

    assert events == [("clocked-a", 7), ("clocked-b", 7)]
    assert [device.name for device in bus._clocked_devices] == [
        "clocked-a",
        "clocked-b",
    ]


def test_external_clock_ownership_and_failure_prefix_are_exact() -> None:
    events: list[tuple[str, int]] = []
    bus = DeviceBus()
    bus.register(
        _ClockedDevice("external", 0, events),
        externally_clocked=True,
    )
    bus.register(_ClockedDevice("before", 1, events))
    bus.register(_ClockedDevice("failure", 2, events, fail=True))
    bus.register(_ClockedDevice("after", 3, events))

    with pytest.raises(RuntimeError, match="failure tick failed"):
        bus.tick(11)

    assert events == [("before", 11), ("failure", 11)]


def test_system_native_clock_avoids_proxy_double_ticks_and_keeps_extensions():
    system = MegapadSystem(
        ram_size=4096,
        num_cores=1,
        num_clusters=0,
        hbw_size=0,
        ext_mem_size=0,
        vram_size=0,
    )
    events: list[tuple[str, int]] = []
    extension = _ClockedDevice("extension", 0x1000, events)
    system.bus.register(extension)
    system.timer.control = 1

    system.bus.tick(17)

    assert system._native_system.system_cycles == 17
    assert system.timer.counter == 17
    assert events == [("extension", 17)]
    assert system.bus._clocked_devices == [extension]
