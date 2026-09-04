"""Hosted state for the public terminal-geometry BIOS vocabulary.

The semantic simulator exposes terminal dimensions through the same public
words as the executable BIOS, without installing the UART geometry MMIO
device.  One state object may be lent to a session owner; its own lock keeps a
diagnostic snapshot coherent with the clear-on-read resize flag.
"""

from __future__ import annotations

import operator
import threading
from dataclasses import dataclass


def terminal_dimension(name: str, value: int) -> int:
    """Return one validated public uint16 terminal dimension."""

    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not 1 <= result <= (1 << 16) - 1:
        raise ValueError(f"{name} must be between 1 and 65535")
    return int(result)


@dataclass(frozen=True, slots=True)
class HostedTerminalGeometry:
    """Read-only snapshot of hosted terminal dimensions and resize state."""

    cols: int
    rows: int
    resized: bool


class HostedTerminalGeometryState:
    """Mutable geometry shared by one runtime and its current session owner."""

    __slots__ = ("_cols", "_lock", "_resized", "_rows")

    def __init__(self, cols: int = 80, rows: int = 24) -> None:
        self._cols = terminal_dimension("terminal columns", cols)
        self._rows = terminal_dimension("terminal rows", rows)
        self._resized = False
        self._lock = threading.RLock()

    def apply(self, cols: int, rows: int) -> None:
        """Publish a host resize and set the sticky guest notification."""

        normalized_cols = terminal_dimension("terminal columns", cols)
        normalized_rows = terminal_dimension("terminal rows", rows)
        with self._lock:
            self._cols = normalized_cols
            self._rows = normalized_rows
            # Match UART_GEOM host_set_size: every accepted host update raises
            # the flag, even when its dimensions equal the current pair.
            self._resized = True

    def consume_resized(self) -> bool:
        """Return and clear the sticky resize notification atomically."""

        with self._lock:
            resized = self._resized
            self._resized = False
            return resized

    def snapshot(self) -> HostedTerminalGeometry:
        with self._lock:
            return HostedTerminalGeometry(
                self._cols,
                self._rows,
                self._resized,
            )

    def restore(self, snapshot: HostedTerminalGeometry) -> None:
        """Adopt an exact detached-session snapshot as fixed legacy state."""

        if not isinstance(snapshot, HostedTerminalGeometry):
            raise TypeError("snapshot must be HostedTerminalGeometry")
        normalized_cols = terminal_dimension("terminal columns", snapshot.cols)
        normalized_rows = terminal_dimension("terminal rows", snapshot.rows)
        if not isinstance(snapshot.resized, bool):
            raise TypeError("terminal resized state must be bool")
        with self._lock:
            self._cols = normalized_cols
            self._rows = normalized_rows
            self._resized = snapshot.resized


__all__ = [
    "HostedTerminalGeometry",
    "HostedTerminalGeometryState",
    "terminal_dimension",
]
