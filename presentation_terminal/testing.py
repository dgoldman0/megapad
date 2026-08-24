"""Deterministic, capacity-faithful fake for the terminal host port."""

from __future__ import annotations

import threading
from collections import deque

from .transport import (
    UINT64_MAX,
    AdmissionStatus,
    BoundedEgressQueue,
    EgressBatch,
    EgressPoll,
    GeometryRecord,
    HostPortLimits,
    IngressRecord,
    ScheduledEventPoll,
    ScheduledHostEvent,
    TerminalHostLease,
    _integer,
    _payload_bytes,
)


class FakeTerminalHost:
    """Exact-boundary fake used by headless terminal and adapter tests.

    Construction does not attach an enhanced terminal.  ``attach()`` is an
    explicit ownership transition.  Accepted ingress and geometry remain in
    a bounded pending queue until ``take_scheduled_event()`` simulates a later
    legal scheduler boundary.
    """

    def __init__(self):
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

    @property
    def epoch(self) -> int:
        """Most recently allocated attachment/reset/detach epoch."""
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
    def accepted_egress_bytes(self) -> int:
        with self._lock:
            return 0 if self._egress is None else self._egress.accepted_bytes

    @property
    def accepted_egress_batches(self) -> int:
        with self._lock:
            return 0 if self._egress is None else self._egress.accepted_batches

    @property
    def runner_backpressured(self) -> bool:
        with self._lock:
            if self._egress is None:
                return False
            return (
                self._retained_publication is not None
                or self._egress.backpressured
            )

    @property
    def can_start_guest_batch(self) -> bool:
        """Whether this fake would admit another guest execution batch."""
        return not self.runner_backpressured

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
    def pending_ingress(self) -> tuple[IngressRecord, ...]:
        with self._lock:
            return tuple(
                event
                for event in self._scheduled
                if isinstance(event, IngressRecord)
            )

    @property
    def pending_geometry(self) -> tuple[GeometryRecord, ...]:
        with self._lock:
            return tuple(
                event
                for event in self._scheduled
                if isinstance(event, GeometryRecord)
            )

    def _next_epoch_locked(self) -> int:
        if self._epoch_clock == UINT64_MAX:
            raise OverflowError("attachment epoch exhausted")
        return self._epoch_clock + 1

    def attach(self, limits: HostPortLimits) -> TerminalHostLease:
        """Acquire the sole enhanced attachment with caller-owned limits."""
        if not isinstance(limits, HostPortLimits):
            raise TypeError("limits must be HostPortLimits")
        with self._lock:
            if self._active_token is not None:
                raise RuntimeError("an enhanced primary attachment is already active")
            epoch = self._next_epoch_locked()
            token = object()
            queue = BoundedEgressQueue(epoch, limits.egress)
            self._epoch_clock = epoch
            self._active_epoch = epoch
            self._active_token = token
            self._limits = limits
            self._egress = queue
            self._retained_publication = None
            self._next_publication_sequence = 0
            self._next_schedule_sequence = 0
            self._clear_scheduled_locked()
            return TerminalHostLease(self, token, epoch)

    def _current_locked(self, token: object, epoch: int) -> bool:
        return (
            self._active_token is token
            and self._active_epoch == epoch
            and self._egress is not None
        )

    def _lease_identity(
        self,
        lease: TerminalHostLease,
    ) -> tuple[object, int] | None:
        if not isinstance(lease, TerminalHostLease):
            raise TypeError("lease must be TerminalHostLease")
        if not lease._belongs_to(self):
            return None
        return lease._identity()

    def publish_egress(
        self,
        lease: TerminalHostLease,
        payload,
    ) -> AdmissionStatus:
        """Simulate one completed machine publication without callbacks.

        A publication that cannot enter the accepted queue occupies the sole
        adapter-retained slot.  A caller must service that slot rather than
        calling this method again as a retry.
        """
        identity = self._lease_identity(lease)
        if identity is None:
            return AdmissionStatus.STALE
        token, epoch = identity
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            immutable = _payload_bytes(payload, allow_empty=True)
            if not immutable:
                return AdmissionStatus.ACCEPTED
            limits = self._limits
            queue = self._egress
            assert limits is not None and queue is not None
            if len(immutable) > limits.retained_publication_bytes:
                raise ValueError(
                    "publication exceeds the configured retained-publication limit"
                )
            if self._retained_publication is not None:
                return AdmissionStatus.BACKPRESSURED
            if self._next_publication_sequence > UINT64_MAX:
                raise OverflowError("publication sequence exhausted")

            batch = EgressBatch(
                attachment_epoch=epoch,
                publication_sequence=self._next_publication_sequence,
                payload=immutable,
            )
            status = queue.accept(batch)
            if status is AdmissionStatus.ACCEPTED:
                self._next_publication_sequence += 1
                return status
            if status is AdmissionStatus.BACKPRESSURED:
                self._retained_publication = batch
                self._next_publication_sequence += 1
            return status

    def service_retained(self, lease: TerminalHostLease) -> AdmissionStatus:
        """Perform runner step 2 without starting guest execution."""
        identity = self._lease_identity(lease)
        if identity is None:
            return AdmissionStatus.STALE
        token, epoch = identity
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            queue = self._egress
            assert queue is not None
            retained = self._retained_publication
            if retained is None:
                return (
                    AdmissionStatus.BACKPRESSURED
                    if queue.backpressured
                    else AdmissionStatus.ACCEPTED
                )
            status = queue.accept(retained)
            if status is AdmissionStatus.ACCEPTED:
                self._retained_publication = None
            return status

    def _lease_poll(self, token: object, epoch: int) -> EgressPoll:
        with self._lock:
            if not self._current_locked(token, epoch):
                return EgressPoll(AdmissionStatus.STALE)
            queue = self._egress
            assert queue is not None
            return queue.poll()

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
            record = IngressRecord(epoch, sequence, immutable, control)
            self._scheduled.append(record)
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
            normalized_cols = _integer("cols", cols, minimum=1)
            normalized_rows = _integer("rows", rows, minimum=1)
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

    def _allocate_schedule_sequence_locked(self) -> int:
        if self._next_schedule_sequence > UINT64_MAX:
            raise OverflowError("schedule sequence exhausted")
        sequence = self._next_schedule_sequence
        self._next_schedule_sequence += 1
        return sequence

    def take_scheduled_event(
        self,
        lease: TerminalHostLease,
    ) -> ScheduledEventPoll:
        """Apply no event; transfer one to a simulated scheduler boundary."""
        identity = self._lease_identity(lease)
        if identity is None:
            return ScheduledEventPoll(AdmissionStatus.STALE)
        token, epoch = identity
        with self._lock:
            if not self._current_locked(token, epoch):
                return ScheduledEventPoll(AdmissionStatus.STALE)
            if not self._scheduled:
                return ScheduledEventPoll(AdmissionStatus.ACCEPTED)
            event = self._scheduled.popleft()
            if isinstance(event, IngressRecord):
                size = len(event.payload)
                self._pending_ingress_bytes -= size
                self._pending_ingress_events -= 1
                if not event.control:
                    self._pending_ordinary_ingress_bytes -= size
                    self._pending_ordinary_ingress_events -= 1
            else:
                self._pending_geometry_events -= 1
            return ScheduledEventPoll(AdmissionStatus.ACCEPTED, event)

    def _lease_close(self, token: object, epoch: int) -> AdmissionStatus:
        with self._lock:
            if not self._current_locked(token, epoch):
                return AdmissionStatus.STALE
            next_epoch = self._next_epoch_locked()
            self._retire_attachment_locked()
            self._epoch_clock = next_epoch
            return AdmissionStatus.ACCEPTED

    def reset(self) -> int:
        """Retire the active attachment before simulated execution resumes."""
        with self._lock:
            next_epoch = self._next_epoch_locked()
            self._retire_attachment_locked()
            self._epoch_clock = next_epoch
            return next_epoch

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
        self._clear_scheduled_locked()

    def _clear_scheduled_locked(self) -> None:
        self._scheduled.clear()
        self._pending_ingress_bytes = 0
        self._pending_ingress_events = 0
        self._pending_ordinary_ingress_bytes = 0
        self._pending_ordinary_ingress_events = 0
        self._pending_geometry_events = 0


__all__ = ["FakeTerminalHost"]
