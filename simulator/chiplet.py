"""Chiplet ports of the hosted profiles.

The ports themselves are the C++ model the emulator uses
(``shared/accel/chiplet_port.h``, ``docs/chiplet-link.md``), reached through
the native extension. This module is the hosted profile's glue around it:
the ports' native link, the chiplets attached to them, and the points at
which those chiplets step. The register window at MMIO offset ``0xE00``,
which takes aligned 64-bit accesses alone, and the mailbox at ``0x500``,
whose table the ports ring through, are native devices on the platform's
table (``simulator/platform.py``).

A chiplet steps where the guest can observe it: before each read of a
window register or of the mailbox's ``STATUS``, and when a host call to the
runtime returns, which on a machine with several cores is the end of a
round. A guest that polls ``STATUS`` for ``READY`` or for a port's signal
therefore makes progress, and the outcome depends on the guest's accesses
alone. A port's signal wakes a core that sleeps in IDL, and the core takes
it as the BIOS does (``simulator/cores.py``); the hosted profiles have no
flag branch.
"""

from __future__ import annotations

import os

from simulator.memory import AddressClass, SparseAddressSpace


CHIPLET_OFFSET = 0xE00
CHIPLET_LIMIT = 0xF00
CHIPLET_RANGES = 8
# W: each port's window into memory its chiplet lends is 2^W bytes, from
# 0x0100_0000_0000_0000. The tile accumulator's TACC-STORE writes there
# (simulator/tacc.py), and PORT_COUNT reports W as the model's register does.
CHIPLET_WINDOW_BITS = 48

MAILBOX_OFFSET = 0x500
MAILBOX_LIMIT = 0x510
MAILBOX_SEND = MAILBOX_OFFSET + 0x08
MAILBOX_STATUS = MAILBOX_OFFSET + 0x09
MAILBOX_ACK = MAILBOX_OFFSET + 0x0A



class ChipletAccessError(ValueError):
    """One access does not belong to the chiplet window."""

    def __init__(
        self,
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        self.offset = offset
        self.width = width
        self.write = write
        super().__init__(message)


def _native_extension():
    try:
        import _megaforth_native as extension
    except ImportError:
        extension = None
    if extension is None or not hasattr(extension, "ChipletLink"):
        raise RuntimeError(
            "chiplet ports require a current _megaforth_native build; "
            "run make build"
        )
    return extension


class HostedChipletPorts:
    """The hosted profile's chiplet ports and their chiplets.

    The ports' IDs follow the profile's cores: with ``core_count`` cores the
    first port's ID is ``core_count``.
    """

    __slots__ = ("_port_count", "_core_count", "_link")

    def __init__(self, port_count: int = 1, *, core_count: int = 1) -> None:
        for name, value in (("port_count", port_count), ("core_count", core_count)):
            if isinstance(value, bool) or not isinstance(value, int):
                raise TypeError(f"{name} must be an integer")
        if core_count < 1:
            raise ValueError("chiplet ports need at least one core")
        if not 1 <= port_count <= 256 - core_count:
            raise ValueError("port IDs must follow the cores' in 8 bits")
        # Fail here, where the caller asked for ports, when the model is
        # absent from the build.
        _native_extension()
        self._port_count = port_count
        self._core_count = core_count
        self._link = None

    @property
    def port_count(self) -> int:
        return self._port_count

    @property
    def core_count(self) -> int:
        return self._core_count

    @property
    def first_port_id(self) -> int:
        return self._core_count

    @property
    def port_count_word(self) -> int:
        """``PORT_COUNT``, which System Info repeats at ``+0x70``."""

        return (
            self._port_count
            | (self.first_port_id << 8)
            | (CHIPLET_RANGES << 16)
            | (CHIPLET_WINDOW_BITS << 24)
        )

    def bind(self, memory: SparseAddressSpace) -> None:
        """Bind once to the address space whose external memory is lent."""

        if not isinstance(memory, SparseAddressSpace):
            raise TypeError("chiplet memory must be a SparseAddressSpace")
        if self._link is not None:
            raise RuntimeError("the chiplet ports are already bound")
        extension = _native_extension()
        base, size = 0, 0
        for spec in memory.regions:
            if spec.kind is AddressClass.EXTERNAL:
                base, size = spec.base, spec.size
        dense = memory.dense_backing
        if size and dense is not None and (base, size) in dense.regions:
            self._link = extension.ChipletLink(
                dense.buffer_at(base),
                base,
                self._core_count,
                self._port_count,
                CHIPLET_RANGES,
            )
        else:
            # Sparse memory has no single buffer. A machine without external
            # memory gets an empty range, which lends nothing.
            self._link = extension.ChipletLink.over_callbacks(
                memory.read_bytes,
                memory.write_bytes,
                base,
                size,
                self._core_count,
                self._port_count,
                CHIPLET_RANGES,
            )

    def _require_bound(self):
        link = self._link
        if link is None:
            raise RuntimeError(
                "the chiplet ports are not bound to an address space"
            )
        return link

    @property
    def link(self):
        """The native model of the ports, whose register window the
        platform attaches and through whose windows the tile accumulator's
        stores reach memory the chiplets lend."""

        return self._require_bound()

    # -- chiplets ---------------------------------------------------------

    def attach_test_chiplet(self, port: int = 0) -> None:
        """Attach the test chiplet built into the native extension."""

        self._require_bound().attach_test_chiplet(port)

    def attach_module(
        self,
        port: int,
        module_path: str | os.PathLike,
        config: str | None = None,
    ) -> None:
        """Attach a chiplet's model loaded from a module library."""

        self._require_bound().load_chiplet_module(
            port, os.fspath(module_path), config
        )

    def detach(self, port: int) -> None:
        self._require_bound().detach(port)

    def set_budget(self, port: int, budget: int) -> None:
        """Set the work a port's chiplet may do in one step."""

        self._require_bound().set_budget(port, budget)

    def step(self) -> bool:
        """Step every port's chiplet once; whether more work waits."""

        return bool(self._require_bound().step())

    @property
    def work_waits(self) -> bool:
        return bool(self._require_bound().work_waits())

    @property
    def mailbox(self):
        """The participant table: the cores first, then port ``p`` at
        ``first_port_id + p``. The platform's mailbox works over this same
        table."""

        return self._require_bound().mailbox

    def save(self) -> bytes:
        return self._require_bound().save()

    def restore(self, state: bytes) -> bool:
        return bool(self._require_bound().restore(state))


__all__ = [
    "CHIPLET_LIMIT",
    "CHIPLET_OFFSET",
    "CHIPLET_RANGES",
    "ChipletAccessError",
    "HostedChipletPorts",
    "MAILBOX_ACK",
    "MAILBOX_LIMIT",
    "MAILBOX_OFFSET",
    "MAILBOX_SEND",
    "MAILBOX_STATUS",
]
