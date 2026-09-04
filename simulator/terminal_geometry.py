"""Hosted state for the public terminal-geometry BIOS vocabulary.

The semantic simulator exposes terminal dimensions through the same public
words as the executable BIOS, without installing the UART geometry MMIO
device.  One state object may be lent to a session owner; its own lock keeps
the dimensions, clear-on-read status flags, and asynchronous guest resize
request coherent.
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
    resize_denied: bool = False


class HostedTerminalGeometryState:
    """Mutable geometry shared by one runtime and its current session owner."""

    __slots__ = (
        "_cols",
        "_lock",
        "_request_cols",
        "_request_generation",
        "_request_pending",
        "_request_rows",
        "_resize_denied",
        "_resized",
        "_rows",
    )

    def __init__(self, cols: int = 80, rows: int = 24) -> None:
        self._cols = terminal_dimension("terminal columns", cols)
        self._rows = terminal_dimension("terminal rows", rows)
        self._resized = False
        self._resize_denied = False
        self._request_pending = False
        self._request_cols = 0
        self._request_rows = 0
        self._request_generation = 0
        self._lock = threading.RLock()

    def _advance_request_generation_locked(self) -> None:
        self._request_generation = (self._request_generation + 1) & (
            (1 << 64) - 1
        )

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

    def consume_resize_denied(self) -> bool:
        """Return and clear only the sticky guest-request denial flag."""

        with self._lock:
            denied = self._resize_denied
            self._resize_denied = False
            return denied

    def request_resize(self, cols: int, rows: int) -> None:
        """Publish one complete low-16-bit guest request for later service.

        The executable BIOS writes only two little-endian bytes for each
        operand and performs no validation.  Masking here retains that public
        behavior while making the semantic word one atomic publication.
        Existing RESIZED and REQ_DENIED notifications deliberately survive a
        replacement request, just as the BIOS leaves STATUS untouched.
        """

        with self._lock:
            self._request_cols = operator.index(cols) & 0xFFFF
            self._request_rows = operator.index(rows) & 0xFFFF
            self._request_pending = True
            self._advance_request_generation_locked()

    def snapshot_resize_request(self) -> tuple[int, int, int] | None:
        """Return an immutable generation-qualified pending request."""

        with self._lock:
            if not self._request_pending:
                return None
            return (
                self._request_generation,
                self._request_cols,
                self._request_rows,
            )

    def host_accept_resize_if_pending(
        self,
        generation: int,
        cols: int,
        rows: int,
    ) -> bool:
        """Accept only the request represented by ``generation``."""

        normalized_cols = terminal_dimension("terminal columns", cols)
        normalized_rows = terminal_dimension("terminal rows", rows)
        with self._lock:
            if (
                not self._request_pending
                or operator.index(generation) != self._request_generation
            ):
                return False
            self._cols = normalized_cols
            self._rows = normalized_rows
            self._request_pending = False
            self._resized = True
            self._advance_request_generation_locked()
            return True

    def host_deny_resize_if_pending(self, generation: int) -> bool:
        """Deny only the request represented by ``generation``."""

        with self._lock:
            if (
                not self._request_pending
                or operator.index(generation) != self._request_generation
            ):
                return False
            self._request_pending = False
            self._resize_denied = True
            self._advance_request_generation_locked()
            return True

    def snapshot(self) -> HostedTerminalGeometry:
        with self._lock:
            return HostedTerminalGeometry(
                self._cols,
                self._rows,
                self._resized,
                self._resize_denied,
            )

    def restore(self, snapshot: HostedTerminalGeometry) -> None:
        """Adopt an exact detached-session snapshot as fixed legacy state."""

        if not isinstance(snapshot, HostedTerminalGeometry):
            raise TypeError("snapshot must be HostedTerminalGeometry")
        normalized_cols = terminal_dimension("terminal columns", snapshot.cols)
        normalized_rows = terminal_dimension("terminal rows", snapshot.rows)
        if not isinstance(snapshot.resized, bool):
            raise TypeError("terminal resized state must be bool")
        if not isinstance(snapshot.resize_denied, bool):
            raise TypeError("terminal resize-denied state must be bool")
        with self._lock:
            self._cols = normalized_cols
            self._rows = normalized_rows
            self._resized = snapshot.resized
            self._resize_denied = snapshot.resize_denied
            # A detached host cannot complete an old session's request.  Keep
            # the guest-visible status but retire the host-facing operation.
            self._request_pending = False
            self._request_cols = 0
            self._request_rows = 0
            self._advance_request_generation_locked()


__all__ = [
    "HostedTerminalGeometry",
    "HostedTerminalGeometryState",
    "terminal_dimension",
]
