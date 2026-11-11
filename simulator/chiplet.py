"""Chiplet ports of the hosted one-core profile.

The ports themselves are the C++ model the emulator uses
(``shared/accel/chiplet_port.h``, ``docs/chiplet-link.md``), reached through
the native extension. This module is the hosted profile's glue around it:

* the register window at MMIO offset ``0xE00``, which takes aligned 64-bit
  accesses alone;
* a mailbox at ``0x500`` for the profile's one core and its ports, over the
  table the ports use: the core rings a port through ``SEND``, sees a port's
  signal in ``STATUS`` and clears it through ``ACK``;
* the points at which attached chiplets step.

The hosted profile has no scheduler rounds, interrupts or flag branch, so a
chiplet steps where the guest can observe it: before each read of a window
register or of the mailbox's ``STATUS``, and when a dispatch returns to its
host. A guest that polls ``STATUS`` for ``READY`` or for a port's signal
therefore makes progress, and the outcome depends on the guest's accesses
alone. A port's signal reaching the guest as an interrupt waits for the
simulator's multicore work.
"""

from __future__ import annotations

import os

from simulator.memory import AddressClass, SparseAddressSpace


CHIPLET_OFFSET = 0xE00
CHIPLET_LIMIT = 0xF00
CHIPLET_RANGES = 8

MAILBOX_OFFSET = 0x500
MAILBOX_LIMIT = 0x510
MAILBOX_SEND = MAILBOX_OFFSET + 0x08
MAILBOX_STATUS = MAILBOX_OFFSET + 0x09
MAILBOX_ACK = MAILBOX_OFFSET + 0x0A

# The hosted profile has one core, so its ports' IDs start at 1.
HOSTED_CORE_COUNT = 1
HOSTED_CORE_ID = 0

_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))


class ChipletAccessError(ValueError):
    """One access does not belong to the chiplet window or the mailbox."""

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
    """The hosted profile's chiplet ports, their window and their chiplets."""

    __slots__ = ("_port_count", "_link", "_latch", "_latch_base")

    def __init__(self, port_count: int = 1) -> None:
        if isinstance(port_count, bool) or not isinstance(port_count, int):
            raise TypeError("port_count must be an integer")
        if not 1 <= port_count <= 256 - HOSTED_CORE_COUNT:
            raise ValueError("port IDs must follow the core's in 8 bits")
        # Fail here, where the caller asked for ports, when the model is
        # absent from the build.
        _native_extension()
        self._port_count = port_count
        self._link = None
        self._latch = bytearray(8)
        self._latch_base: int | None = None

    @property
    def port_count(self) -> int:
        return self._port_count

    @property
    def first_port_id(self) -> int:
        return HOSTED_CORE_COUNT

    @property
    def port_count_word(self) -> int:
        """``PORT_COUNT``, which System Info repeats at ``+0x70``."""

        return (
            self._port_count
            | (self.first_port_id << 8)
            | (CHIPLET_RANGES << 16)
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
                HOSTED_CORE_COUNT,
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
                HOSTED_CORE_COUNT,
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
        """The participant table: the core is ID 0, port ``p`` is ``1 + p``."""

        return self._require_bound().mailbox

    def save(self) -> bytes:
        return self._require_bound().save()

    def restore(self, state: bytes) -> bool:
        return bool(self._require_bound().restore(state))

    # -- the register window ---------------------------------------------

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Admit one aligned 64-bit access inside the window."""

        self._require_bound()
        if not isinstance(offset, int) or not isinstance(width, int):
            raise TypeError("chiplet window offset and width must be integers")
        if (
            width != 8
            or offset % 8
            or not CHIPLET_OFFSET <= offset
            or offset + width > CHIPLET_LIMIT
        ):
            raise ChipletAccessError(
                "the chiplet window takes aligned 64-bit accesses",
                offset=offset,
                width=width,
                write=write,
            )

    def read8(self, offset: int) -> int:
        link = self._require_bound()
        self._require_window_byte(offset, write=False)
        index = offset & 7
        if index == 0:
            # The chiplets step before the guest looks at a register.
            link.step()
        value = link.read64((offset & ~7) - CHIPLET_OFFSET)
        return (value >> (8 * index)) & 0xFF

    def write8(self, offset: int, value: int) -> None:
        link = self._require_bound()
        self._require_window_byte(offset, write=True)
        index = offset & 7
        base = offset & ~7
        # A register is written whole: its eight bytes arrive in order and
        # take effect with the last.
        if index == 0:
            self._latch_base = base
        elif self._latch_base != base:
            raise ChipletAccessError(
                "a chiplet register is written as one 64-bit access",
                offset=offset,
                width=1,
                write=True,
            )
        self._latch[index] = value & 0xFF
        if index == 7:
            self._latch_base = None
            link.write64(
                base - CHIPLET_OFFSET,
                int.from_bytes(self._latch, "little"),
            )

    @staticmethod
    def _require_window_byte(offset: int, *, write: bool) -> None:
        if not isinstance(offset, int):
            raise TypeError("chiplet window offset must be an integer")
        if not CHIPLET_OFFSET <= offset < CHIPLET_LIMIT:
            raise ChipletAccessError(
                "byte access is outside the chiplet window",
                offset=offset,
                width=1,
                write=write,
            )


class HostedChipletMailbox:
    """The mailbox as the hosted profile's one core reaches it.

    Bytes 0 to 7 are the core's data slot, which no port reads: a chiplet's
    data crosses in lent memory. A write of an ID to ``SEND`` signals that
    participant, ``STATUS`` shows the pending bits of IDs 0 to 7, and a write
    of an ID to ``ACK`` clears that participant's signal.
    """

    __slots__ = ("_ports", "_data")

    def __init__(self, ports: HostedChipletPorts) -> None:
        if not isinstance(ports, HostedChipletPorts):
            raise TypeError("mailbox ports must be HostedChipletPorts")
        self._ports = ports
        self._data = bytearray(8)

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        self._ports.mailbox
        if not isinstance(offset, int) or not isinstance(width, int):
            raise TypeError("mailbox offset and width must be integers")
        if (
            width not in _INTEGER_WIDTHS
            or offset % width
            or not MAILBOX_OFFSET <= offset
            or offset + width > MAILBOX_LIMIT
        ):
            raise ChipletAccessError(
                "access is outside the mailbox or not naturally aligned",
                offset=offset,
                width=width,
                write=write,
            )

    def read8(self, offset: int) -> int:
        self._require_byte(offset, write=False)
        register = offset - MAILBOX_OFFSET
        if register < 8:
            return self._data[register]
        if offset == MAILBOX_STATUS:
            # The chiplets step before the guest looks for their signal.
            self._ports.step()
            return self._ports.mailbox.pending_mask(HOSTED_CORE_ID) & 0xFF
        return 0

    def write8(self, offset: int, value: int) -> None:
        self._require_byte(offset, write=True)
        register = offset - MAILBOX_OFFSET
        value &= 0xFF
        if register < 8:
            self._data[register] = value
        elif offset == MAILBOX_SEND:
            self._ports.mailbox.send(HOSTED_CORE_ID, value)
        elif offset == MAILBOX_ACK:
            self._ports.mailbox.acknowledge(HOSTED_CORE_ID, value)

    @staticmethod
    def _require_byte(offset: int, *, write: bool) -> None:
        if not isinstance(offset, int):
            raise TypeError("mailbox offset must be an integer")
        if not MAILBOX_OFFSET <= offset < MAILBOX_LIMIT:
            raise ChipletAccessError(
                "byte access is outside the mailbox",
                offset=offset,
                width=1,
                write=write,
            )


__all__ = [
    "CHIPLET_LIMIT",
    "CHIPLET_OFFSET",
    "CHIPLET_RANGES",
    "ChipletAccessError",
    "HostedChipletMailbox",
    "HostedChipletPorts",
    "MAILBOX_ACK",
    "MAILBOX_LIMIT",
    "MAILBOX_OFFSET",
    "MAILBOX_SEND",
    "MAILBOX_STATUS",
]
