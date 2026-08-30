"""Stable public surface for MegaPad's additive rich-terminal port."""

from .cell_model import Cell, Cursor, TerminalView
from .driver import (
    DriverLimits,
    DriverServiceResult,
    DriverStatus,
    RichTerminalDriver,
)
from .server import TerminalConfig, TerminalSessionError, TerminalState
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
    ResizeRecord,
    ScheduledEventPoll,
    ScheduledHostEvent,
    TerminalHost,
    TerminalHostLease,
)


__all__ = [
    "AdmissionStatus",
    "BoundedEgressQueue",
    "Cell",
    "Cursor",
    "DriverLimits",
    "DriverServiceResult",
    "DriverStatus",
    "EgressBatch",
    "EgressDelivery",
    "EgressPoll",
    "EgressWatermarks",
    "FakeTerminalHost",
    "GeometryRecord",
    "HostPortLimits",
    "IngressRecord",
    "RichTerminalDriver",
    "ResizeRecord",
    "ScheduledEventPoll",
    "ScheduledHostEvent",
    "TerminalHost",
    "TerminalHostLease",
    "TerminalConfig",
    "TerminalSessionError",
    "TerminalState",
    "TerminalView",
]
