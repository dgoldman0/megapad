"""Explicit frontend driver joining a MegaPad lease to the APT-1 core."""

from __future__ import annotations

import operator
from collections import deque
from dataclasses import dataclass
from enum import Enum
from typing import Callable, Protocol

from .apt1 import CONTROL_RESERVE_BYTES, HEADER_BYTES, UINT64_MAX
from .retained_model import RetainedPolicy
from .retained_wire import ControlEventKind
from .server import (
    OutboundBytes,
    TerminalOutputView,
    RichTerminalCore,
    TerminalConfig,
    TerminalSessionError,
    TerminalState,
)
from .transport import (
    AdmissionStatus,
    HostPortLimits,
    TerminalHostLease,
)


_MAX_FATAL_CONTROL_FRAME_BYTES = 296
_MIN_VALID_RESULT_EVENTS = 3
_KEY_FRAME_BYTES = HEADER_BYTES + 16
_TEXT_FRAME_OVERHEAD = HEADER_BYTES + 12
_POINTER_FRAME_BYTES = HEADER_BYTES + 28
_CONTROL_EVENT_FRAME_BYTES = HEADER_BYTES + 40
_FOCUS_FRAME_BYTES = HEADER_BYTES + 16
_RESIZE_FRAME_BYTES = HEADER_BYTES + 16
_RETAINED_DISCOVERY_FRAME_BYTES = HEADER_BYTES + 64
_RETAINED_DISCOVERY_BATCH_BYTES = (
    2 * _RETAINED_DISCOVERY_FRAME_BYTES + HEADER_BYTES + 8
)
_RETAINED_DISCOVERY_BATCH_EVENTS = 3
_MAX_FIXED_INPUT_FRAME_BYTES = max(
    _KEY_FRAME_BYTES,
    _POINTER_FRAME_BYTES,
    _FOCUS_FRAME_BYTES,
    _RESIZE_FRAME_BYTES,
)


def _integer(name: str, value, *, minimum: int, maximum: int) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not minimum <= result <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return int(result)


def _validate_retained_driver_capacity(
    host_limits: HostPortLimits,
    limits: DriverLimits,
    *,
    retained_configured: bool,
) -> None:
    if not retained_configured:
        return
    if host_limits.ordinary_ingress_bytes < _RETAINED_DISCOVERY_FRAME_BYTES:
        raise ValueError(
            "ordinary ingress capacity cannot admit one RETAINED-1 "
            "discovery reply"
        )
    if (
        limits.pending_outbound_bytes < _RETAINED_DISCOVERY_BATCH_BYTES
        or limits.pending_outbound_events < _RETAINED_DISCOVERY_BATCH_EVENTS
    ):
        raise ValueError(
            "driver retention cannot admit the complete RETAINED-1 "
            "discovery reply tuple"
        )


@dataclass(frozen=True, slots=True)
class DriverLimits:
    """Caller-owned capacity for responses retained outside the host port."""

    pending_outbound_bytes: int
    pending_outbound_events: int

    def __post_init__(self) -> None:
        bytes_limit = _integer(
            "pending_outbound_bytes",
            self.pending_outbound_bytes,
            minimum=CONTROL_RESERVE_BYTES,
            maximum=UINT64_MAX,
        )
        event_limit = _integer(
            "pending_outbound_events",
            self.pending_outbound_events,
            minimum=_MIN_VALID_RESULT_EVENTS,
            maximum=UINT64_MAX,
        )
        object.__setattr__(self, "pending_outbound_bytes", bytes_limit)
        object.__setattr__(self, "pending_outbound_events", event_limit)


class DriverStatus(str, Enum):
    IDLE = "idle"
    PROGRESS = "progress"
    BACKPRESSURED = "backpressured"
    INVALID = "invalid"
    STALE = "stale"
    FAILED = "failed"


@dataclass(frozen=True, slots=True)
class DriverServiceResult:
    status: DriverStatus
    machine_batches: int = 0
    outbound_records: int = 0
    ansi_bytes: int = 0
    views: int = 0


@dataclass(frozen=True, slots=True)
class _PendingOutbound:
    record: OutboundBytes
    resize_geometry: tuple[int, int] | None = None


class _AttachSystem(Protocol):
    def attach_rich_terminal(
        self,
        limits: HostPortLimits,
    ) -> TerminalHostLease: ...


class RichTerminalDriver:
    """Own one explicit lease and service it outside guest execution.

    The driver never executes the machine.  Its owner alternates bounded
    :meth:`service` calls with normal runner calls, allowing admitted ingress
    to cross at the next deterministic scheduler boundary.
    """

    def __init__(
        self,
        lease: TerminalHostLease,
        core: RichTerminalCore,
        host_limits: HostPortLimits,
        limits: DriverLimits,
        *,
        ansi_sink: Callable[[bytes], None] | None = None,
        view_sink: Callable[[TerminalOutputView], None] | None = None,
    ):
        if not isinstance(lease, TerminalHostLease):
            raise TypeError("lease must be TerminalHostLease")
        if not isinstance(core, RichTerminalCore):
            raise TypeError("core must be RichTerminalCore")
        if not isinstance(host_limits, HostPortLimits):
            raise TypeError("host_limits must be HostPortLimits")
        if not isinstance(limits, DriverLimits):
            raise TypeError("limits must be DriverLimits")
        if ansi_sink is not None and not callable(ansi_sink):
            raise TypeError("ansi_sink must be callable or None")
        if view_sink is not None and not callable(view_sink):
            raise TypeError("view_sink must be callable or None")
        _validate_retained_driver_capacity(
            host_limits,
            limits,
            retained_configured=core.retained_configured,
        )
        self._lease = lease
        self._core = core
        self._host_limits = host_limits
        self._limits = limits
        self._ansi_sink = ansi_sink
        self._view_sink = view_sink
        self._pending: deque[_PendingOutbound] = deque()
        self._pending_bytes = 0
        self._resize_intent: tuple[int, int] | None = None
        self._failure_reason: str | None = None
        self._closed = False

    @classmethod
    def attach(
        cls,
        system: _AttachSystem,
        host_limits: HostPortLimits,
        terminal_config: TerminalConfig,
        driver_limits: DriverLimits,
        *,
        ansi_sink: Callable[[bytes], None] | None = None,
        view_sink: Callable[[TerminalOutputView], None] | None = None,
        retained_policy: RetainedPolicy | None = None,
        session_id_factory: Callable[[], int] | None = None,
    ) -> RichTerminalDriver:
        """Validate the complete vertical's capacities before acquisition."""

        if not isinstance(host_limits, HostPortLimits):
            raise TypeError("host_limits must be HostPortLimits")
        if not isinstance(terminal_config, TerminalConfig):
            raise TypeError("terminal_config must be TerminalConfig")
        if not isinstance(driver_limits, DriverLimits):
            raise TypeError("driver_limits must be DriverLimits")
        if retained_policy is not None and not isinstance(
            retained_policy, RetainedPolicy
        ):
            raise TypeError("retained_policy must be RetainedPolicy or None")
        _validate_retained_driver_capacity(
            host_limits,
            driver_limits,
            retained_configured=retained_policy is not None,
        )
        required_publication = (
            terminal_config.max_transaction_bytes + CONTROL_RESERVE_BYTES
        )
        if required_publication > UINT64_MAX:
            raise OverflowError("maximum machine publication exceeds uint64")
        if host_limits.retained_publication_bytes < required_publication:
            raise ValueError(
                "retained_publication_bytes must admit one maximum transaction "
                "plus the APT-1 control reserve"
            )
        if terminal_config.max_feed_bytes < required_publication:
            raise ValueError(
                "max_feed_bytes must admit one maximum transaction plus the "
                "APT-1 control reserve"
            )
        if host_limits.ingress_control_bytes < _MAX_FATAL_CONTROL_FRAME_BYTES:
            raise ValueError(
                "ingress control capacity cannot admit one maximum APT-1 ERROR"
            )
        if host_limits.ingress_control_events < 1:
            raise ValueError("ingress control capacity needs one event slot")
        if (
            host_limits.ordinary_ingress_bytes < _MAX_FIXED_INPUT_FRAME_BYTES
            or host_limits.ordinary_ingress_events < 1
        ):
            raise ValueError(
                "ordinary ingress capacity cannot admit every fixed-size input frame"
            )

        lease = system.attach_rich_terminal(host_limits)
        try:
            core = RichTerminalCore(
                terminal_config,
                attachment_epoch=lease.attachment_epoch,
                retained_policy=retained_policy,
                session_id_factory=session_id_factory,
            )
            geometry = lease.submit_geometry(
                terminal_config.cols,
                terminal_config.rows,
            )
            if geometry is not AdmissionStatus.ACCEPTED:
                raise RuntimeError("initial terminal geometry was not admitted")
            return cls(
                lease,
                core,
                host_limits,
                driver_limits,
                ansi_sink=ansi_sink,
                view_sink=view_sink,
            )
        except BaseException:
            lease.close()
            raise

    @property
    def core(self) -> RichTerminalCore:
        return self._core

    @property
    def attachment_epoch(self) -> int:
        return self._lease.attachment_epoch

    @property
    def pending_outbound_bytes(self) -> int:
        return self._pending_bytes

    @property
    def pending_outbound_events(self) -> int:
        return len(self._pending)

    @property
    def pending_resize(self) -> tuple[int, int] | None:
        return self._resize_intent

    @property
    def max_text_bytes(self) -> int:
        """Effective one-event TEXT bound across peer and local storage."""

        return max(
            0,
            min(
                self._core.max_text_bytes,
                self._host_limits.ordinary_ingress_bytes - _TEXT_FRAME_OVERHEAD,
                self._limits.pending_outbound_bytes - _TEXT_FRAME_OVERHEAD,
            ),
        )

    @property
    def max_legacy_input_bytes(self) -> int:
        """Effective one-record bound while the guest still owns ANSI."""

        return min(
            self._host_limits.ordinary_ingress_bytes,
            self._limits.pending_outbound_bytes,
        )

    @property
    def failure_reason(self) -> str | None:
        return self._failure_reason

    @property
    def closed(self) -> bool:
        return self._closed

    def service(self, *, max_batches: int = 1) -> DriverServiceResult:
        """Service replies and at most ``max_batches`` machine publications."""

        batch_limit = _integer(
            "max_batches", max_batches, minimum=1, maximum=UINT64_MAX
        )
        if self._closed:
            return DriverServiceResult(DriverStatus.STALE)
        if self._failure_reason is not None:
            return DriverServiceResult(DriverStatus.FAILED)
        if self._core.state not in {
            TerminalState.ACTIVE,
            TerminalState.RESYNCING,
        }:
            self._resize_intent = None

        batches = 0
        admitted = 0
        ansi_bytes = 0
        views = 0

        while batches < batch_limit:
            flush_status, count = self._flush_pending()
            admitted += count
            if flush_status is not DriverStatus.PROGRESS:
                return DriverServiceResult(
                    flush_status,
                    batches,
                    admitted,
                    ansi_bytes,
                    views,
                )

            polled = self._lease.poll_egress()
            if polled.status is AdmissionStatus.STALE:
                self._closed = True
                return DriverServiceResult(
                    DriverStatus.STALE,
                    batches,
                    admitted,
                    ansi_bytes,
                    views,
                )
            delivery = polled.delivery
            if delivery is None:
                resize_status, materialized = self._materialize_resize()
                if resize_status in {DriverStatus.FAILED, DriverStatus.STALE}:
                    return DriverServiceResult(
                        resize_status,
                        batches,
                        admitted,
                        ansi_bytes,
                        views,
                    )
                if materialized:
                    flush_status, count = self._flush_pending()
                    admitted += count
                    if flush_status is DriverStatus.BACKPRESSURED:
                        self._fail(
                            "atomic resize admission changed after preflight"
                        )
                        return DriverServiceResult(
                            DriverStatus.FAILED,
                            batches,
                            admitted,
                            ansi_bytes,
                            views,
                        )
                    if flush_status is not DriverStatus.PROGRESS:
                        return DriverServiceResult(
                            flush_status,
                            batches,
                            admitted,
                            ansi_bytes,
                            views,
                        )
                status = (
                    DriverStatus.PROGRESS
                    if batches or admitted or ansi_bytes or views or materialized
                    else DriverStatus.IDLE
                )
                return DriverServiceResult(
                    status,
                    batches,
                    admitted,
                    ansi_bytes,
                    views,
                )

            try:
                result = self._core.feed_machine(delivery.batch.payload)
                self._retain_outbound(result.outbound)
                if self._core.state not in {
                    TerminalState.ACTIVE,
                    TerminalState.RESYNCING,
                }:
                    self._resize_intent = None
            except (TerminalSessionError, TypeError, ValueError) as exc:
                delivery.release()
                self._fail(str(exc))
                return DriverServiceResult(
                    DriverStatus.FAILED,
                    batches + 1,
                    admitted,
                    ansi_bytes,
                    views,
                )

            release = delivery.release()
            if release is AdmissionStatus.STALE:
                self._closed = True
                return DriverServiceResult(
                    DriverStatus.STALE,
                    batches,
                    admitted,
                    ansi_bytes,
                    views,
                )
            batches += 1

            try:
                if result.ansi_bytes and self._ansi_sink is not None:
                    self._ansi_sink(result.ansi_bytes)
                ansi_bytes += len(result.ansi_bytes)
                if self._view_sink is not None:
                    for view in result.views:
                        self._view_sink(view)
                views += len(result.views)
            except Exception as exc:
                self._fail(f"terminal consumer failed: {exc}")
                return DriverServiceResult(
                    DriverStatus.FAILED,
                    batches,
                    admitted,
                    ansi_bytes,
                    views,
                )

        flush_status, count = self._flush_pending()
        admitted += count
        status = (
            flush_status
            if flush_status in {DriverStatus.BACKPRESSURED, DriverStatus.STALE}
            else DriverStatus.PROGRESS
        )
        return DriverServiceResult(status, batches, admitted, ansi_bytes, views)

    def send_key(
        self,
        key_symbol: int,
        *,
        action: int = 1,
        location: int = 0,
        modifiers: int = 0,
    ) -> DriverStatus:
        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if not self._can_retain(_KEY_FRAME_BYTES, 1):
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self._core.send_key(
                key_symbol,
                action=action,
                location=location,
                modifiers=modifiers,
            )
            if outbound is None:
                return DriverStatus.BACKPRESSURED
            self._retain_outbound((outbound,))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def send_legacy_input(self, data) -> DriverStatus:
        """Queue raw input only before the enhanced OPEN boundary.

        This keeps boot-time and unsupported-peer interaction inside the
        exclusive lease instead of falling through the suppressed legacy
        UART facade.  Once OPEN begins, all input must use framed messages.
        """

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if self._core.state not in {TerminalState.ANSI, TerminalState.PROBING}:
            return DriverStatus.INVALID
        if isinstance(data, str):
            return DriverStatus.INVALID
        try:
            raw = memoryview(data).tobytes()
        except (TypeError, ValueError):
            return DriverStatus.INVALID
        if not raw or len(raw) > self.max_legacy_input_bytes:
            return DriverStatus.INVALID
        if not self._can_retain(len(raw), 1):
            return DriverStatus.BACKPRESSURED
        try:
            self._retain_outbound((OutboundBytes(raw, False),))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def send_text(self, data, *, paste: bool = False) -> DriverStatus:
        """Queue one nonempty normalized UTF-8 TEXT event."""

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if isinstance(data, str):
            return DriverStatus.INVALID
        try:
            raw = memoryview(data).tobytes()
        except (TypeError, ValueError):
            return DriverStatus.INVALID
        frame_bytes = _TEXT_FRAME_OVERHEAD + len(raw)
        if not raw or len(raw) > self.max_text_bytes:
            return DriverStatus.INVALID
        if not self._can_retain(frame_bytes, 1):
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self._core.send_text(raw, paste=paste)
            if outbound is None:
                return DriverStatus.BACKPRESSURED
            self._retain_outbound((outbound,))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def send_pointer(
        self,
        x: int,
        y: int,
        *,
        buttons: int = 0,
        modifiers: int = 0,
        kind: int = 1,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> DriverStatus:
        """Queue one normalized cell-coordinate pointer event."""

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if not self._can_retain(_POINTER_FRAME_BYTES, 1):
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self._core.send_pointer(
                x,
                y,
                buttons=buttons,
                modifiers=modifiers,
                kind=kind,
                wheel_x=wheel_x,
                wheel_y=wheel_y,
            )
            if outbound is None:
                return DriverStatus.BACKPRESSURED
            self._retain_outbound((outbound,))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def send_control_event(
        self,
        owner_id: int,
        owner_generation: int,
        control_id: int,
        *,
        model_revision: int,
        event_kind: ControlEventKind = ControlEventKind.ACTIVATE,
        modifiers: int = 0,
    ) -> DriverStatus:
        """Queue one revision-bound semantic-control activation."""

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if not self._can_retain(_CONTROL_EVENT_FRAME_BYTES, 1):
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self._core.send_control_event(
                owner_id,
                owner_generation,
                control_id,
                model_revision=model_revision,
                event_kind=event_kind,
                modifiers=modifiers,
            )
            if outbound is None:
                return DriverStatus.BACKPRESSURED
            self._retain_outbound((outbound,))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def send_focus(self, focused: bool) -> DriverStatus:
        """Queue one normalized focus transition."""

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        if not self._can_retain(_FOCUS_FRAME_BYTES, 1):
            return DriverStatus.BACKPRESSURED
        try:
            outbound = self._core.send_focus(focused)
            if outbound is None:
                return DriverStatus.BACKPRESSURED
            self._retain_outbound((outbound,))
        except (TerminalSessionError, TypeError, ValueError):
            return DriverStatus.INVALID
        return DriverStatus.PROGRESS

    def request_resize(self, cols: int, rows: int) -> DriverStatus:
        """Record the latest desired geometry for safe service-time emission."""

        if self._closed:
            return DriverStatus.STALE
        if self._failure_reason is not None:
            return DriverStatus.FAILED
        try:
            geometry = self._core.validate_resize(cols, rows)
        except (TypeError, ValueError):
            return DriverStatus.INVALID
        state = self._core.state
        if state is TerminalState.ANSI:
            if geometry == self._core.selected_geometry:
                return DriverStatus.PROGRESS
            admission = self._lease.submit_geometry(*geometry)
            if admission is AdmissionStatus.BACKPRESSURED:
                return DriverStatus.BACKPRESSURED
            if admission is AdmissionStatus.STALE:
                self._closed = True
                return DriverStatus.STALE
            try:
                self._core.select_ansi_geometry(*geometry)
            except (TerminalSessionError, TypeError, ValueError) as exc:
                self._fail(f"cannot commit admitted ANSI geometry: {exc}")
                return DriverStatus.FAILED
            return DriverStatus.PROGRESS
        if state not in {TerminalState.ACTIVE, TerminalState.RESYNCING}:
            return DriverStatus.BACKPRESSURED
        if geometry == self._core.selected_geometry:
            self._resize_intent = None
            return DriverStatus.PROGRESS
        self._resize_intent = geometry
        return DriverStatus.PROGRESS

    def close(self) -> AdmissionStatus:
        """Hard-retire this outer attachment and restore legacy ownership."""

        if self._closed:
            return AdmissionStatus.ACCEPTED
        result = self._lease.close()
        if result not in {AdmissionStatus.ACCEPTED, AdmissionStatus.STALE}:
            return result
        self._closed = True
        self._pending.clear()
        self._pending_bytes = 0
        self._resize_intent = None
        return result

    def _retain_outbound(self, records: tuple[OutboundBytes, ...]) -> None:
        additional_events = len(records)
        additional_bytes = sum(len(record.payload) for record in records)
        if not self._can_retain(additional_bytes, additional_events):
            raise TerminalSessionError(
                "one machine publication exceeded the caller-owned outbound "
                "retention capacity"
            )
        self._pending.extend(_PendingOutbound(record) for record in records)
        self._pending_bytes += additional_bytes

    def _retain_resize(
        self,
        record: OutboundBytes,
        cols: int,
        rows: int,
    ) -> None:
        size = len(record.payload)
        if record.control or not self._can_retain(size, 1):
            raise TerminalSessionError(
                "resize exceeded the caller-owned outbound retention capacity"
            )
        self._pending.append(_PendingOutbound(record, (cols, rows)))
        self._pending_bytes += size

    def _can_retain(self, additional_bytes: int, additional_events: int) -> bool:
        return (
            additional_events
            <= self._limits.pending_outbound_events - len(self._pending)
            and additional_bytes
            <= self._limits.pending_outbound_bytes - self._pending_bytes
        )

    def _materialize_resize(self) -> tuple[DriverStatus, bool]:
        geometry = self._resize_intent
        if geometry is None or not self._core.resize_ready:
            return DriverStatus.IDLE, False
        quiescent = self._lease.machine_egress_quiescent()
        if quiescent is AdmissionStatus.STALE:
            self._closed = True
            return DriverStatus.STALE, False
        if quiescent is AdmissionStatus.BACKPRESSURED:
            return DriverStatus.IDLE, False
        admission = self._lease.resize_admission_ready(_RESIZE_FRAME_BYTES)
        if admission is AdmissionStatus.STALE:
            self._closed = True
            return DriverStatus.STALE, False
        if admission is AdmissionStatus.BACKPRESSURED:
            return DriverStatus.IDLE, False
        if not self._can_retain(_RESIZE_FRAME_BYTES, 1):
            return DriverStatus.BACKPRESSURED, False
        try:
            outbound = self._core.send_resize(*geometry)
            if outbound is None:
                return DriverStatus.BACKPRESSURED, False
            self._retain_resize(outbound, *geometry)
        except (TerminalSessionError, TypeError, ValueError) as exc:
            self._fail(f"cannot materialize terminal resize: {exc}")
            return DriverStatus.FAILED, False
        self._resize_intent = None
        return DriverStatus.PROGRESS, True

    def _flush_pending(self) -> tuple[DriverStatus, int]:
        admitted = 0
        while self._pending:
            pending = self._pending[0]
            record = pending.record
            geometry = pending.resize_geometry
            if geometry is None:
                status = self._lease.submit_ingress(
                    record.payload,
                    control=record.control,
                )
            else:
                status = self._lease.submit_resize(
                    record.payload,
                    cols=geometry[0],
                    rows=geometry[1],
                )
            if status is AdmissionStatus.BACKPRESSURED:
                return DriverStatus.BACKPRESSURED, admitted
            if status is AdmissionStatus.STALE:
                self._closed = True
                return DriverStatus.STALE, admitted
            if record.result_transaction_id is not None:
                self._core.settle_result_delivery(
                    record.result_transaction_id
                )
            if record.lifecycle_result is not None:
                self._core.settle_lifecycle_result_delivery(
                    record.lifecycle_result
                )
            self._pending.popleft()
            self._pending_bytes -= len(record.payload)
            admitted += 1
        return DriverStatus.PROGRESS, admitted

    def _fail(self, reason: str) -> None:
        self._failure_reason = reason


__all__ = [
    "DriverLimits",
    "DriverServiceResult",
    "DriverStatus",
    "RichTerminalDriver",
]
