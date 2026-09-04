"""MegaPad hooks for the shared optional rich-terminal host port."""

from __future__ import annotations

import weakref
from contextlib import contextmanager
from typing import TYPE_CHECKING, Iterator

from shared.rich_terminal_host import (
    RichTerminalHostHooks,
    SharedRichTerminalHost,
)

if TYPE_CHECKING:
    from .system import MegapadSystem


class _MegapadRichTerminalHooks(RichTerminalHostHooks):
    """Apply shared host-port transitions at MegaPad scheduler boundaries."""

    def __init__(self, system: MegapadSystem):
        self._system_ref = weakref.ref(system)

    def _system(self) -> MegapadSystem:
        system = self._system_ref()
        if system is None:
            raise RuntimeError("the owning MegaPad system no longer exists")
        return system

    @contextmanager
    def scheduler_boundary(self) -> Iterator[None]:
        system = self._system()
        with system._scheduler_lock:
            yield

    def reject_batch_reentry_locked(self) -> None:
        self._system()._reject_native_batch_reentry()

    def drain_completed_egress_locked(self) -> None:
        self._system()._drain_native_uart_output()

    def require_attach_ready_locked(self) -> None:
        self._system()._require_rich_terminal_attach_ready_locked()

    def bind_machine_sink_locked(
        self,
        sink: SharedRichTerminalHost | None,
    ) -> None:
        self._system().uart._set_rich_terminal_host(sink)

    def discard_machine_egress_locked(self) -> None:
        self._system().uart._discard_native_output()

    def apply_uart_ingress_locked(self, epoch: int, payload: bytes) -> None:
        self._system()._schedule_rich_terminal_uart_input_locked(epoch, payload)

    def apply_geometry_locked(self, epoch: int, cols: int, rows: int) -> None:
        self._system()._schedule_rich_terminal_resize_locked(epoch, cols, rows)

    def pending_uart_ingress_bytes_locked(self) -> int:
        return self._system().uart.rx_pending

    def discard_uart_ingress_tail_locked(self, count: int) -> None:
        self._system().uart._discard_rx_tail(count)


class MegapadRichTerminalHost(SharedRichTerminalHost):
    """One explicit, exclusive rich-terminal attachment to MegaPad."""

    def __init__(self, system: MegapadSystem):
        hooks = _MegapadRichTerminalHooks(system)
        self._megapad_hooks = hooks
        super().__init__(hooks)

    def _system(self) -> MegapadSystem:
        """Return the owner, preserving the adapter's existing private seam."""
        return self._megapad_hooks._system()


__all__ = ["MegapadRichTerminalHost"]
