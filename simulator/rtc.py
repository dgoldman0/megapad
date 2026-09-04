"""Deterministic hosted model of the admitted RTC epoch subwindow.

Only the writable, latched 64-bit epoch-millisecond register is admitted here.
The service deliberately does not consult host wall time or model uptime,
calendar, alarm, or control registers.  Host callers advance or replace the
current value explicitly so source execution remains reproducible.
"""

from __future__ import annotations

from shared.cells import MASK64


RTC_EPOCH = 0xB08
RTC_EPOCH_SIZE = 8
RTC_EPOCH_LIMIT = RTC_EPOCH + RTC_EPOCH_SIZE

_INTEGER_WIDTHS = frozenset((1, 2, 4, 8))


class RTCAccessError(ValueError):
    """One direct access does not belong to the admitted RTC subwindow."""

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


class HostedRTCService:
    """One runtime-local deterministic epoch register with read latching."""

    __slots__ = ("_epoch_latch", "_epoch_ms")

    def __init__(self, initial_epoch_ms: int = 0) -> None:
        self._epoch_ms = self._require_u64(
            initial_epoch_ms,
            label="initial epoch milliseconds",
        )
        self._epoch_latch = 0

    @property
    def epoch_ms(self) -> int:
        """Return the current host-controlled epoch-millisecond value."""

        return self._epoch_ms

    @property
    def epoch_latch(self) -> int:
        """Return the value retained by the most recent low-byte read."""

        return self._epoch_latch

    def set_epoch_ms(self, value: int) -> None:
        """Replace the current deterministic epoch without changing the latch."""

        self._epoch_ms = self._require_u64(
            value,
            label="epoch milliseconds",
        )

    def advance_ms(self, delta: int) -> None:
        """Advance the current epoch modulo the 64-bit register width."""

        if isinstance(delta, bool) or not isinstance(delta, int):
            raise TypeError("epoch advance must be a non-negative integer")
        if delta < 0:
            raise ValueError("epoch advance must be a non-negative integer")
        self._epoch_ms = (self._epoch_ms + delta) & MASK64

    def preflight(self, offset: int, width: int, *, write: bool) -> None:
        """Admit supported-width spans wholly inside the epoch register."""

        if isinstance(offset, bool) or not isinstance(offset, int):
            raise TypeError("RTC offset must be an integer")
        if isinstance(width, bool) or not isinstance(width, int):
            raise TypeError("RTC width must be an integer")
        if width not in _INTEGER_WIDTHS:
            self._reject(
                "RTC width must be 1, 2, 4, or 8 bytes",
                offset=offset,
                width=width,
                write=write,
            )
        if offset < RTC_EPOCH or offset + width > RTC_EPOCH_LIMIT:
            self._reject(
                "access is outside the admitted RTC epoch subwindow",
                offset=offset,
                width=width,
                write=write,
            )

    def read8(self, offset: int) -> int:
        """Read one byte, latching the complete value at the low byte."""

        self._require_byte_offset(offset, write=False)
        if offset == RTC_EPOCH:
            self._epoch_latch = self._epoch_ms
        shift = (offset - RTC_EPOCH) * 8
        return (self._epoch_latch >> shift) & 0xFF

    def write8(self, offset: int, value: int) -> None:
        """Replace one little-endian byte of the current epoch register."""

        self._require_byte_offset(offset, write=True)
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError("RTC byte value must be an integer")
        if not 0 <= value <= 0xFF:
            raise ValueError("RTC byte value must be in range 0..255")
        shift = (offset - RTC_EPOCH) * 8
        mask = 0xFF << shift
        self._epoch_ms = (self._epoch_ms & ~mask) | (value << shift)

    def _require_byte_offset(self, offset: int, *, write: bool) -> None:
        if isinstance(offset, bool) or not isinstance(offset, int):
            raise TypeError("RTC offset must be an integer")
        if not RTC_EPOCH <= offset < RTC_EPOCH_LIMIT:
            self._reject(
                "byte access is outside the admitted RTC epoch subwindow",
                offset=offset,
                width=1,
                write=write,
            )

    @staticmethod
    def _require_u64(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be a uint64 integer")
        if not 0 <= value <= MASK64:
            raise ValueError(f"{label} must be a uint64 integer")
        return value

    @staticmethod
    def _reject(
        message: str,
        *,
        offset: int,
        width: int,
        write: bool,
    ) -> None:
        raise RTCAccessError(
            message,
            offset=offset,
            width=width,
            write=write,
        )


__all__ = [
    "RTC_EPOCH",
    "RTC_EPOCH_LIMIT",
    "RTC_EPOCH_SIZE",
    "HostedRTCService",
    "RTCAccessError",
]
