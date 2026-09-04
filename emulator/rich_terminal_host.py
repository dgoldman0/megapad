"""MegaPad scheduler adapter for the optional rich-terminal port.

The adapter owns only transport state.  It never parses terminal bytes and
never calls terminal-owned code from a machine settlement boundary.
"""

from __future__ import annotations

import threading
import weakref
from collections import deque
from dataclasses import dataclass
from typing import TYPE_CHECKING

from rich_terminal.transport import (
    UINT64_MAX,
    AdmissionStatus,
    BoundedEgressQueue,
    EgressBatch,
    EgressPoll,
    GeometryRecord,
    HostPortLimits,
    IngressRecord,
    ResizeRecord,
    ScheduledHostEvent,
    TerminalHostLease,
    _integer,
    _payload_bytes,
)

if TYPE_CHECKING:
    from .system import MegapadSystem


@dataclass(frozen=True, slots=True)
class _RunnerAdmission:
    can_execute: bool
    external_events_applied: int = 0


class MegapadRichTerminalHost:
    """One explicit, exclusive rich-terminal attachment.

    ``MegapadSystem`` constructs this adapter but does not acquire it.  A
    frontend must call :meth:`attach` with its own storage limits.  Lease
    transitions serialize on the machine scheduler lock; polling and bounded
    host admission remain independent of guest execution.
    """

    def __init__(self, system: MegapadSystem):
        self._system_ref = weakref.ref(system)
        self._lock = threading.RLock()
        self._epoch_clock = 0
        self._active_epoch: int | None = None
        self._active_token: object | None = None
        self._limits: HostPortLimits | None = None
        self._egress: BoundedEgressQueue | None = None
        self._retained_publication: EgressBatch | None = None
        self._next_publication_sequence = 0
        self._next_schedule_sequence = 0
        self._scheduled: deque[ScheduledHostEvent] = deque()
        self._pending_ingress_bytes = 0
        self._pending_ingress_events = 0
        self._pending_ordinary_ingress_bytes = 0
        self._pending_ordinary_ingress_events = 0
        self._pending_geometry_events = 0
        self._applied_ingress_bytes = 0
        self._failure_reason: str | None = None

    def _system(self) -> MegapadSystem:
        system = self._system_ref()
        if system is None:
            raise RuntimeError("the owning MegaPad system no longer exists")
        return system

    @property
    def epoch(self) -> int:
        with self._lock:
            return self._epoch_clock

    @property
    def active_attachment_epoch(self) -> int | None:
        with self._lock:
            return self._active_epoch

    @property
    def enhanced_attached(self) -> bool:
        with self._lock:
            return self._active_token is not None

    @property
    def retained_publication(self) -> EgressBatch | None:
        with self._lock:
            return self._retained_publication

    @property
    def failure_reason(self) -> str | None:
        """Latched machine-boundary failure for the current attachment."""
        with self._lock:
            return self._failure_reason

    @property
    def accepted_egress_bytes(self) -> int:
        with self._lock:
            return 0 if self._egress is None else self._egress.accepted_bytes

    @property
    def accepted_egress_batches(self) -> int:
        with self._lock:
            return 0 if self._egress is None else self._egress.accepted_batches

    @property
    def pending_ingress_bytes(self) -> int:
        with self._lock:
            return self._pending_ingress_bytes

    @property
    def pending_ingress_events(self) -> int:
        with self._lock:
            return self._pending_ingress_events

    @property
    def pending_geometry_events(self) -> int:
        with self._lock:
            return self._pending_geometry_events

    @property
    def runner_backpressured(self) -> bool:
        with self._lock:
            return self._runner_backpressured_locked()

    @property
    def can_start_guest_batch(self) -> bool:
        return not self.runner_backpressured

    def _next_epoch_locked(self) -> int:
        if self._epoch_clock == UINT64_MAX:
            raise OverflowError("attachment epoch exhausted")
        return self._epoch_clock + 1

    def attach(self, limits: HostPortLimits) -> TerminalHostLease:
        """Acquire the enhanced primary without replacing legacy callbacks."""
        if not isinstance(limits, HostPortLimits):
            raise TypeError("limits must be HostPortLimits")
        system = self._system()
        with system._scheduler_lock:
            system._reject_native_batch_reentry()
            with self._lock:
                if self._active_token is not None:
                    raise RuntimeError(
                        "an enhanced primary attachment is already active"
                    )

            # Legacy bytes completed before the ownership transition remain
            # legacy bytes.  Drain them before publishing the new epoch.
            system._drain_native_uart_output()
            system._require_rich_terminal_attach_ready_locked()

            with self._lock:
                if self._active_token is not None:
                    raise RuntimeError(
                        "an enhanced primary attachment is already active"
                    )
                epoch = self._next_epoch_locked()
                token = object()
                queue = BoundedEgressQueue(epoch, limits.egress)
                lease = TerminalHostLease(self, token, epoch)

                self._epoch_clock = epoch
                self._active_epoch = epoch
                self._active_token = token
                self._limits = limits
                self._egress = queue
                self._retained_publication = None
                self._next_publication_sequence = 0
                self._next_schedule_sequence = 0
                self._applied_ingress_bytes = 0
                self._failure_reason = None
                self._clear_scheduled_locked()
                system.uart._set_rich_terminal_host(self)
                return lease

    def _current_locked(self, token: object, epoch: int) -> bool:
        return (
            self._active_token is token
            and self._active_epoch == epoch
            and self._egress is not None
        )

    def _epoch_current_locked(self, epoch: int) -> bool:
        return self._active_token is not None and self._active_epoch == epoch

    def _epoch_is_current(self, epoch: int) -> bool:
        with self._lock:
            return self._epoch_current_locked(epoch)

    def _runner_backpressured_locked(self) -> bool:
        if self._egress is None:
            return False
        return (
            self._failure_reason is not None
            or self._retained_publication is not None
            or self._egress.backpressured
        )

    def _runner_stop_reason(self) -> str:
        with self._lock:
            return (
                "terminal_failure"
                if self._failure_reason is not None
                else "host_backpressure"
            )

    def _machine_drain_admitted(self) -> bool:
        """Whether a destructive native UART drain can occupy the port."""
        with self._lock:
            if self._active_token is None:
                return True
            return not self._runner_backpressured_locked()

    def _publish_machine_egress(self, payload) -> bool:
        """Publish one completed machine batch, returning whether it was owned."""
        with self._lock:
            if self._active_token is None:
                return False
            immutable = _payload_bytes(payload, allow_empty=True)
            if not immutable:
                return True

            limits = self._limits
            queue = self._egress
            epoch = self._active_epoch
            assert limits is not None and queue is not None and epoch is not None
            if len(immutable) > limits.retained_publication_bytes:
                # The native drain already crossed the execution boundary, so
                # it cannot be put back.  Keep ownership here so binary never
                # leaks to legacy listeners and stop all later guest work
                # until the attachment is explicitly retired.
                self._failure_reason = (
                    f"machine UART publication of {len(immutable)} bytes exceeds "
                    f"the retained limit of {limits.retained_publication_bytes}"
                )
                return True
            if self._retained_publication is not None:
                raise RuntimeError(
                    "machine UART publication crossed a retained egress boundary"
                )
            if self._next_publication_sequence > UINT64_MAX:
                raise OverflowError("publication sequence exhausted")

            batch = EgressBatch(
                epoch,
                self._next_publication_sequence,
                immutable,
            )
            status = queue.accept(batch)
            if status is AdmissionStatus.STALE:
                raise RuntimeError("active rich-terminal egress queue became stale")
            self._next_publication_sequence += 1
            if status is AdmissionStatus.BACKPRESSURED:
                self._retained_publication = batch
            return True

    def _lease_poll(self, token: object, epoch: int) -> EgressPoll:
        with self._lock:
            if not self._current_locked(token, epoch):
                return EgressPoll(AdmissionStatus.STALE)
            queue = self._egress
            assert queue is not None
            return queue.poll()

    def _lease_machine_egress_quiescent(
        self,
        token: object,
        epoch: int,
    ) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            queue = self._egress
            assert queue is not None
            if self._retained_publication is not None or queue.accepted_batches:
                return AdmissionStatus.BACKPRESSURED
            return AdmissionStatus.ACCEPTED

    def _lease_submit_ingress(
        self,
        token: object,
        epoch: int,
        payload,
        *,
        control: bool,
    ) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            if not isinstance(control, bool):
                raise TypeError("control must be bool")
            immutable = _payload_bytes(payload, allow_empty=True)
            if not immutable:
                return AdmissionStatus.ACCEPTED
            limits = self._limits
            assert limits is not None
            size = len(immutable)
            if (
                size > limits.ingress_bytes - self._pending_ingress_bytes
                or self._pending_ingress_events >= limits.ingress_events
            ):
                return AdmissionStatus.BACKPRESSURED
            if not control and (
                size
                > limits.ordinary_ingress_bytes
                - self._pending_ordinary_ingress_bytes
                or self._pending_ordinary_ingress_events
                >= limits.ordinary_ingress_events
            ):
                return AdmissionStatus.BACKPRESSURED

            sequence = self._allocate_schedule_sequence_locked()
            self._scheduled.append(
                IngressRecord(epoch, sequence, immutable, control)
            )
            self._pending_ingress_bytes += size
            self._pending_ingress_events += 1
            if not control:
                self._pending_ordinary_ingress_bytes += size
                self._pending_ordinary_ingress_events += 1
            return AdmissionStatus.ACCEPTED

    def _lease_submit_geometry(
        self,
        token: object,
        epoch: int,
        cols: int,
        rows: int,
    ) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            limits = self._limits
            assert limits is not None
            if self._pending_geometry_events >= limits.geometry_events:
                return AdmissionStatus.BACKPRESSURED
            normalized_cols = _integer(
                "cols", cols, minimum=1, maximum=(1 << 16) - 1
            )
            normalized_rows = _integer(
                "rows", rows, minimum=1, maximum=(1 << 16) - 1
            )
            sequence = self._allocate_schedule_sequence_locked()
            self._scheduled.append(
                GeometryRecord(
                    epoch,
                    sequence,
                    normalized_cols,
                    normalized_rows,
                )
            )
            self._pending_geometry_events += 1
            return AdmissionStatus.ACCEPTED

    def _lease_submit_resize(
        self,
        token: object,
        epoch: int,
        payload,
        *,
        cols: int,
        rows: int,
    ) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            immutable = _payload_bytes(payload, allow_empty=False)
            normalized_cols = _integer(
                "cols", cols, minimum=1, maximum=(1 << 16) - 1
            )
            normalized_rows = _integer(
                "rows", rows, minimum=1, maximum=(1 << 16) - 1
            )
            limits = self._limits
            assert limits is not None
            size = len(immutable)
            if not self._resize_fits_locked(size):
                return AdmissionStatus.BACKPRESSURED

            sequence = self._allocate_schedule_sequence_locked()
            self._scheduled.append(
                ResizeRecord(
                    epoch,
                    sequence,
                    immutable,
                    normalized_cols,
                    normalized_rows,
                )
            )
            self._pending_ingress_bytes += size
            self._pending_ingress_events += 1
            self._pending_ordinary_ingress_bytes += size
            self._pending_ordinary_ingress_events += 1
            self._pending_geometry_events += 1
            return AdmissionStatus.ACCEPTED

    def _lease_resize_admission_ready(
        self,
        token: object,
        epoch: int,
        payload_bytes: int,
    ) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            size = _integer("payload_bytes", payload_bytes, minimum=1)
            return (
                AdmissionStatus.ACCEPTED
                if self._resize_fits_locked(size)
                else AdmissionStatus.BACKPRESSURED
            )

    def _resize_fits_locked(self, size: int) -> bool:
        limits = self._limits
        assert limits is not None
        return not (
            size > limits.ingress_bytes - self._pending_ingress_bytes
            or self._pending_ingress_events >= limits.ingress_events
            or size
            > limits.ordinary_ingress_bytes
            - self._pending_ordinary_ingress_bytes
            or self._pending_ordinary_ingress_events
            >= limits.ordinary_ingress_events
            or self._pending_geometry_events >= limits.geometry_events
        )

    def _allocate_schedule_sequence_locked(self) -> int:
        if self._next_schedule_sequence > UINT64_MAX:
            raise OverflowError("schedule sequence exhausted")
        sequence = self._next_schedule_sequence
        self._next_schedule_sequence += 1
        return sequence

    def _service_before_guest_locked(self) -> _RunnerAdmission:
        """Cross the host-to-machine boundary in the contracted order."""
        system = self._system()
        with self._lock:
            if self._active_token is None:
                return _RunnerAdmission(True)
            queue = self._egress
            epoch = self._active_epoch
            assert queue is not None and epoch is not None
            if self._failure_reason is not None:
                return _RunnerAdmission(False)

            retained = self._retained_publication
            if retained is not None:
                status = queue.accept(retained)
                if status is AdmissionStatus.BACKPRESSURED:
                    return _RunnerAdmission(False)
                if status is AdmissionStatus.STALE:
                    raise RuntimeError(
                        "active retained terminal output publication became stale"
                    )
                self._retained_publication = None

            applied = 0
            while self._scheduled:
                event = self._scheduled[0]
                if event.attachment_epoch != epoch:
                    raise RuntimeError(
                        "rich-terminal scheduler queue crossed an attachment epoch"
                    )
                if isinstance(event, IngressRecord):
                    system._schedule_rich_terminal_uart_input_locked(
                        epoch,
                        event.payload,
                    )
                    size = len(event.payload)
                    self._pending_ingress_bytes -= size
                    self._pending_ingress_events -= 1
                    if not event.control:
                        self._pending_ordinary_ingress_bytes -= size
                        self._pending_ordinary_ingress_events -= 1
                    self._applied_ingress_bytes += size
                    applied += 1
                elif isinstance(event, GeometryRecord):
                    system._schedule_rich_terminal_resize_locked(
                        epoch,
                        event.cols,
                        event.rows,
                    )
                    self._pending_geometry_events -= 1
                    applied += 1
                else:
                    assert isinstance(event, ResizeRecord)
                    try:
                        system._schedule_rich_terminal_resize_locked(
                            epoch,
                            event.cols,
                            event.rows,
                        )
                        system._schedule_rich_terminal_uart_input_locked(
                            epoch,
                            event.payload,
                        )
                    except Exception as exc:
                        self._failure_reason = (
                            "atomic terminal resize scheduling failed: "
                            f"{exc}"
                        )
                        return _RunnerAdmission(False, applied)
                    size = len(event.payload)
                    self._pending_ingress_bytes -= size
                    self._pending_ingress_events -= 1
                    self._pending_ordinary_ingress_bytes -= size
                    self._pending_ordinary_ingress_events -= 1
                    self._pending_geometry_events -= 1
                    self._applied_ingress_bytes += size
                    applied += 1
                self._scheduled.popleft()

            return _RunnerAdmission(
                not self._runner_backpressured_locked(),
                applied,
            )

    def _lease_close(self, token: object, epoch: int) -> AdmissionStatus:
        system = self._system()
        with system._scheduler_lock:
            system._reject_native_batch_reentry()
            with self._lock:
                if not self._current_locked(token, epoch):
                    return AdmissionStatus.STALE
                next_epoch = self._next_epoch_locked()

                # A direct facade write may have left native output in place
                # after the queue became backpressured.  It belongs to this
                # epoch and must never fall through to ANSI after release.
                system.uart._discard_native_output()
                self._discard_applied_ingress_locked(system)
                self._retire_attachment_locked()
                self._epoch_clock = next_epoch
                system.uart._set_rich_terminal_host(None)
                return AdmissionStatus.ACCEPTED

    def _retire_for_machine_reset_locked(self) -> None:
        """Invalidate the active epoch before a running machine is rebooted."""
        system = self._system()
        with self._lock:
            if self._active_token is None:
                return
            next_epoch = self._next_epoch_locked()
            system.uart._discard_native_output()
            self._discard_applied_ingress_locked(system)
            self._retire_attachment_locked()
            self._epoch_clock = next_epoch
            system.uart._set_rich_terminal_host(None)

    def _discard_applied_ingress_locked(self, system: MegapadSystem) -> None:
        # Legacy/future UART ingress is barred for the duration of the lease,
        # so any unconsumed attachment bytes form a suffix behind RX bytes
        # that predated acquisition.  The guest may already have consumed any
        # prefix; min() is therefore the exact remaining attachment count.
        remaining = min(
            self._applied_ingress_bytes,
            system.uart.rx_pending,
        )
        if remaining:
            system.uart._discard_rx_tail(remaining)

    def _retire_attachment_locked(self) -> None:
        if self._egress is not None:
            self._egress.retire()
        self._active_epoch = None
        self._active_token = None
        self._limits = None
        self._egress = None
        self._retained_publication = None
        self._next_publication_sequence = 0
        self._next_schedule_sequence = 0
        self._applied_ingress_bytes = 0
        self._failure_reason = None
        self._clear_scheduled_locked()

    def _clear_scheduled_locked(self) -> None:
        self._scheduled.clear()
        self._pending_ingress_bytes = 0
        self._pending_ingress_events = 0
        self._pending_ordinary_ingress_bytes = 0
        self._pending_ordinary_ingress_events = 0
        self._pending_geometry_events = 0


__all__ = ["MegapadRichTerminalHost"]
