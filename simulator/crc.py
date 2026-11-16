"""The hosted profile's CRC service.

The service is native code, simulator/accel/crc_service.h: the BIOS checked
owner record over the recurrence the emulator's CRC unit uses
(shared/accel/crc.h).  It is the one definition of the CRC words: the Python
words call it through this binding and the native engine calls it directly.
The class loads with the simulator's extension on first use.  The initial
simulator advertises one full guest core and one foreground task, so every
caller identity is ``(COREID, TASK-ID) = (0, 0)``.
"""

from __future__ import annotations


CRC_STATUS_OK = 0
CRC_STATUS_UNSUPPORTED = 1
CRC_STATUS_STATE = 2
CRC_STATUS_RANGE = 3

GuestIdentity = tuple[int, int]


def __getattr__(name: str):
    if name == "HostedCRCService":
        from simulator.native_execution import native_service

        service = native_service("CRCService", "CRC service")
        globals()[name] = service
        return service
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")


__all__ = [
    "CRC_STATUS_OK",
    "CRC_STATUS_RANGE",
    "CRC_STATUS_STATE",
    "CRC_STATUS_UNSUPPORTED",
    "GuestIdentity",
    "HostedCRCService",
]
