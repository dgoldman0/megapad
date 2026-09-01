"""Deterministic runtime-local model of the pseudo-BIOS Timer service.

The hosted simulator advances this timer from admitted semantic dispatcher
steps.  It deliberately has no wall-clock source, MMIO aperture, interrupt
vectoring, or idle-wake behavior; those belong to machine emulation rather
than the source-compatible pseudo-BIOS boundary.
"""

from __future__ import annotations

from shared.cells import MASK64


UINT32_MASK = 0xFFFF_FFFF
UINT8_MASK = 0xFF

TIMER_ENABLED = 0x01
TIMER_IRQ_ENABLED = 0x02
TIMER_AUTO_RELOAD = 0x04
TIMER_MATCHED = 0x01


class HostedTimerService:
    """Own one deterministic 32-bit Timer instance for a hosted runtime."""

    __slots__ = (
        "_compare",
        "_control",
        "_counter",
        "_irq_pending",
        "_status",
    )

    def __init__(
        self,
        *,
        counter: int = 0,
        compare: int = UINT32_MASK,
        control: int = TIMER_ENABLED,
        status: int = 0,
        irq_pending: bool = False,
    ) -> None:
        self._counter = self._require_unsigned(
            counter,
            maximum=UINT32_MASK,
            label="timer counter",
        )
        self._compare = self._require_unsigned(
            compare,
            maximum=UINT32_MASK,
            label="timer compare",
        )
        self._control = self._require_unsigned(
            control,
            maximum=UINT8_MASK,
            label="timer control",
        )
        self._status = self._require_unsigned(
            status,
            maximum=UINT8_MASK,
            label="timer status",
        )
        if not isinstance(irq_pending, bool):
            raise TypeError("timer IRQ pending state must be a boolean")
        self._irq_pending = irq_pending

    @property
    def counter(self) -> int:
        """Return the current wrapping 32-bit count."""

        return self._counter

    @property
    def compare(self) -> int:
        """Return the retained 32-bit compare value."""

        return self._compare

    @property
    def control(self) -> int:
        """Return all eight retained control bits."""

        return self._control

    @property
    def status(self) -> int:
        """Return all eight retained status bits, including sticky match."""

        return self._status

    @property
    def irq_pending(self) -> bool:
        """Return whether an IRQ-enabled match latched a pending request."""

        return self._irq_pending

    def advance(self) -> None:
        """Advance by exactly one admitted hosted timer tick."""

        if not self._control & TIMER_ENABLED:
            return

        self._counter = (self._counter + 1) & UINT32_MASK
        if self._counter != self._compare:
            return

        self._status |= TIMER_MATCHED
        if self._control & TIMER_IRQ_ENABLED:
            self._irq_pending = True
        if self._control & TIMER_AUTO_RELOAD:
            self._counter = 0

    def write_compare(self, value: int) -> None:
        """Atomically retain the low 32 bits of one guest cell."""

        cell = self._require_unsigned(
            value,
            maximum=MASK64,
            label="timer compare cell",
        )
        self._compare = cell & UINT32_MASK

    def write_control(self, value: int) -> None:
        """Atomically retain the low eight bits of one guest cell."""

        cell = self._require_unsigned(
            value,
            maximum=MASK64,
            label="timer control cell",
        )
        self._control = cell & UINT8_MASK

    def acknowledge(self) -> None:
        """Clear the sticky compare-match bit and the latched IRQ request."""

        self._status &= ~TIMER_MATCHED
        self._irq_pending = False

    def clone(self) -> HostedTimerService:
        """Return an independent copy suitable for one runtime instance."""

        return HostedTimerService(
            counter=self._counter,
            compare=self._compare,
            control=self._control,
            status=self._status,
            irq_pending=self._irq_pending,
        )

    @staticmethod
    def _require_unsigned(value: int, *, maximum: int, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an unsigned integer")
        if not 0 <= value <= maximum:
            raise ValueError(f"{label} must be in range 0..{maximum}")
        return value


__all__ = [
    "HostedTimerService",
    "TIMER_AUTO_RELOAD",
    "TIMER_ENABLED",
    "TIMER_IRQ_ENABLED",
    "TIMER_MATCHED",
    "UINT32_MASK",
]
