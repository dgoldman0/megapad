"""Stable public surface for MegaPad's additive presentation-terminal port."""

from .cell_model import Cell, Cursor, TerminalView
from .driver import (
    DriverLimits,
    DriverServiceResult,
    DriverStatus,
    PresentationTerminalDriver,
)
from .megapad import MegapadTerminalHost
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
    "MegapadTerminalHost",
    "PresentationTerminalDriver",
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
