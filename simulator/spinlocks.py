"""The hosted machine's hardware spinlocks.

The bank is native code, the model the emulator is to share
(``shared/accel/spinlock_bank.h``): sixteen locks keyed by architectural
core ID, an atomic acquire that answers 0 when the lock was free or is
already the caller's and 1 while another core holds it, depthless
reacquisition, so that one release frees the lock, and release by the owner
alone. The BIOS words ``SPIN@`` and ``SPIN!``, the checked disk words and the
native engine's disk words all take their locks from the one bank. The class
loads with the simulator's extension on first use, so importing the
simulator needs no build.
"""

from __future__ import annotations


SPINLOCK_COUNT = 16
SPINLOCK_ACQUIRED = 0
SPINLOCK_BUSY = 1


def __getattr__(name: str):
    if name == "HostedSpinlockBank":
        from simulator.native_execution import native_service

        bank = native_service("SpinlockBank", "spinlock bank")
        globals()[name] = bank
        return bank
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    "HostedSpinlockBank",
    "SPINLOCK_ACQUIRED",
    "SPINLOCK_BUSY",
    "SPINLOCK_COUNT",
]
