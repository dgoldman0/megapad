"""The hosted profile's CRC service.

The service is native code, simulator/accel/crc_service.h: the BIOS checked
owner record over the recurrence the emulator's CRC unit uses
(shared/accel/crc.h).  It is the one definition of the CRC words: the Python
words call it through this binding and the native engine calls it directly.
The initial simulator advertises one full guest core and one foreground
task, so every caller identity is ``(COREID, TASK-ID) = (0, 0)``.
"""

from __future__ import annotations

try:
    from _megaforth_native import CRCService as HostedCRCService
except ImportError as error:  # pragma: no cover - a build problem
    raise ImportError(
        "the simulator's CRC service is native code: build MegaPad's "
        "simulator extension with make simulator-accel"
    ) from error


CRC_STATUS_OK = 0
CRC_STATUS_UNSUPPORTED = 1
CRC_STATUS_STATE = 2
CRC_STATUS_RANGE = 3

GuestIdentity = tuple[int, int]


__all__ = [
    "CRC_STATUS_OK",
    "CRC_STATUS_RANGE",
    "CRC_STATUS_STATE",
    "CRC_STATUS_UNSUPPORTED",
    "GuestIdentity",
    "HostedCRCService",
]
