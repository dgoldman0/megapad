"""Stable public surface for MegaPad's additive presentation-terminal port."""

from .testing import FakeTerminalHost
from .transport import (
    AdmissionStatus,
    BoundedEgressQueue,
    EgressBatch,
    EgressDelivery,
    EgressPoll,
    EgressWatermarks,
    GeometryRecord,
    HostPortLimits,
    IngressRecord,
    ScheduledEventPoll,
    ScheduledHostEvent,
    TerminalHost,
    TerminalHostLease,
)


__all__ = [
    "AdmissionStatus",
    "BoundedEgressQueue",
    "EgressBatch",
    "EgressDelivery",
    "EgressPoll",
    "EgressWatermarks",
    "FakeTerminalHost",
    "GeometryRecord",
    "HostPortLimits",
    "IngressRecord",
    "ScheduledEventPoll",
    "ScheduledHostEvent",
    "TerminalHost",
    "TerminalHostLease",
]
