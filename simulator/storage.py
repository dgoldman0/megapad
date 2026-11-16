"""Synchronous block-storage service for the hosted MegaPad profile.

The hardware controller is byte-register driven and may expose DMA cadence,
contention, partial completion, and timeout.  The hosted simulator keeps the
same public BIOS results and media identity rules, but completes each admitted
request semantically against one bounded image and one guest-memory span.

The service is native code, simulator/accel/storage_service.h: the one
definition of the simulator's disk words, which the Python words call through
its binding and the native engine calls directly.  Its test faults and
request observer are the emulator controller's own
(shared/accel/storage_faults.h), and the host file that holds a medium is
shared.storage's HostImageFile, as the emulator's controller keeps it.  The
class loads with the simulator's extension on first use.
"""

from __future__ import annotations


def __getattr__(name: str):
    if name == "HostedStorageService":
        from simulator.native_execution import native_service

        service = native_service("StorageService", "storage service")
        globals()[name] = service
        return service
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = ["HostedStorageService"]
