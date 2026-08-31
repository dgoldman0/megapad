"""Requester-aware hosted hardware-spinlock semantics.

The hosted simulator does not model bus arbitration or MMIO timing.  It does
preserve the public BIOS value contract needed by ordinary KDOS: sixteen
independent owner-tracked locks, an atomic nonblocking acquire result, same-
core reacquisition, and owner-only release.  Reacquisition is deliberately
depthless; one release makes the lock free, matching the executable device.
"""

from __future__ import annotations


SPINLOCK_COUNT = 16
SPINLOCK_ACQUIRED = 0
SPINLOCK_BUSY = 1


class HostedSpinlockBank:
    """Runtime-local semantic spinlocks keyed by architectural core ID."""

    def __init__(
        self,
        *,
        core_count: int,
    ) -> None:
        self._core_count = self._positive_integer(
            core_count,
            label="spinlock core count",
        )
        self._owners: list[int | None] = [None] * SPINLOCK_COUNT

    @property
    def core_count(self) -> int:
        return self._core_count

    @property
    def lock_count(self) -> int:
        return SPINLOCK_COUNT

    @property
    def owners(self) -> tuple[int | None, ...]:
        """Return an immutable diagnostic snapshot of every owner."""

        return tuple(self._owners)

    def owner(self, lock_id: int) -> int | None:
        """Return one current owner, or ``None`` while the lock is free."""

        return self._owners[self._lock_index(lock_id)]

    def acquire(self, lock_id: int, requester_core: int) -> int:
        """Try once, returning 0 for acquired/reentrant and 1 for busy."""

        index = self._lock_index(lock_id)
        requester = self._requester(requester_core)
        owner = self._owners[index]
        if owner is None:
            self._owners[index] = requester
            return SPINLOCK_ACQUIRED
        if owner == requester:
            return SPINLOCK_ACQUIRED
        return SPINLOCK_BUSY

    def release(self, lock_id: int, requester_core: int) -> None:
        """Release only when the requester owns the selected lock."""

        index = self._lock_index(lock_id)
        requester = self._requester(requester_core)
        if self._owners[index] == requester:
            self._owners[index] = None

    def reset(self) -> None:
        """Return the bank to its architectural reset state."""

        self._owners[:] = [None] * SPINLOCK_COUNT

    def _lock_index(self, lock_id: int) -> int:
        if isinstance(lock_id, bool) or not isinstance(lock_id, int):
            raise TypeError("spinlock ID must be an integer")
        if not 0 <= lock_id < SPINLOCK_COUNT:
            raise ValueError("spinlock ID is outside the hosted bank")
        return lock_id

    def _requester(self, requester_core: int) -> int:
        if isinstance(requester_core, bool) or not isinstance(
            requester_core,
            int,
        ):
            raise TypeError("spinlock requester core must be an integer")
        if not 0 <= requester_core < self._core_count:
            raise ValueError("spinlock requester is outside the hosted profile")
        return requester_core

    @staticmethod
    def _positive_integer(value: int, *, label: str) -> int:
        if isinstance(value, bool) or not isinstance(value, int):
            raise TypeError(f"{label} must be an integer")
        if value <= 0:
            raise ValueError(f"{label} must be positive")
        return value


__all__ = [
    "HostedSpinlockBank",
    "SPINLOCK_ACQUIRED",
    "SPINLOCK_BUSY",
    "SPINLOCK_COUNT",
]
