"""Bounded, epoch-qualified transport primitives for rich terminals.

This module is deliberately independent of MegaPad's UART, scheduler, ANSI
terminal, and rendering code.  A later machine adapter can use these types at
the scheduler boundary without importing or invoking terminal-owned code.
"""

from __future__ import annotations

import operator
import threading
from collections import deque
from dataclasses import dataclass
from enum import Enum
from typing import Protocol, TypeAlias, runtime_checkable


UINT64_MAX = (1 << 64) - 1


class AdmissionStatus(str, Enum):
    """Result of an epoch-qualified ownership or admission operation."""

    ACCEPTED = "accepted"
    BACKPRESSURED = "backpressured"
    STALE = "stale"


def _integer(name: str, value, *, minimum: int, maximum: int = UINT64_MAX) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not minimum <= result <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return int(result)


def _checked_sum(name: str, *values: int) -> int:
    total = 0
    for value in values:
        if value > UINT64_MAX - total:
            raise ValueError(f"{name} exceeds uint64 capacity")
        total += value
    return total


def _payload_bytes(payload, *, allow_empty: bool) -> bytes:
    if isinstance(payload, str):
        raise TypeError("payload must be bytes-like, not str")
    try:
        immutable = bytes(payload)
    except (TypeError, ValueError) as exc:
        raise TypeError("payload must be bytes-like") from exc
    if not allow_empty and not immutable:
        raise ValueError("payload must not be empty")
    return immutable


@dataclass(frozen=True, slots=True)
class EgressBatch:
    """One immutable, non-empty machine publication."""

    attachment_epoch: int
    publication_sequence: int
    payload: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "attachment_epoch",
            _integer("attachment_epoch", self.attachment_epoch, minimum=1),
        )
        object.__setattr__(
            self,
            "publication_sequence",
            _integer(
                "publication_sequence",
                self.publication_sequence,
                minimum=0,
            ),
        )
        object.__setattr__(
            self,
            "payload",
            _payload_bytes(self.payload, allow_empty=False),
        )


@dataclass(frozen=True, slots=True)
class IngressRecord:
    """One admitted, not-yet-applied terminal-to-machine payload."""

    attachment_epoch: int
    schedule_sequence: int
    payload: bytes
    control: bool = False

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "attachment_epoch",
            _integer("attachment_epoch", self.attachment_epoch, minimum=1),
        )
        object.__setattr__(
            self,
            "schedule_sequence",
            _integer("schedule_sequence", self.schedule_sequence, minimum=0),
        )
        object.__setattr__(
            self,
            "payload",
            _payload_bytes(self.payload, allow_empty=False),
        )
        if not isinstance(self.control, bool):
            raise TypeError("control must be bool")


@dataclass(frozen=True, slots=True)
class GeometryRecord:
    """One admitted, not-yet-applied terminal geometry change."""

    attachment_epoch: int
    schedule_sequence: int
    cols: int
    rows: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "attachment_epoch",
            _integer("attachment_epoch", self.attachment_epoch, minimum=1),
        )
        object.__setattr__(
            self,
            "schedule_sequence",
            _integer("schedule_sequence", self.schedule_sequence, minimum=0),
        )
        object.__setattr__(
            self,
            "cols",
            _integer("cols", self.cols, minimum=1, maximum=(1 << 16) - 1),
        )
        object.__setattr__(
            self,
            "rows",
            _integer("rows", self.rows, minimum=1, maximum=(1 << 16) - 1),
        )


@dataclass(frozen=True, slots=True)
class ResizeRecord:
    """One atomic APT RESIZE ingress plus matching MMIO geometry change."""

    attachment_epoch: int
    schedule_sequence: int
    payload: bytes
    cols: int
    rows: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "attachment_epoch",
            _integer("attachment_epoch", self.attachment_epoch, minimum=1),
        )
        object.__setattr__(
            self,
            "schedule_sequence",
            _integer("schedule_sequence", self.schedule_sequence, minimum=0),
        )
        object.__setattr__(
            self,
            "payload",
            _payload_bytes(self.payload, allow_empty=False),
        )
        object.__setattr__(
            self,
            "cols",
            _integer("cols", self.cols, minimum=1, maximum=(1 << 16) - 1),
        )
        object.__setattr__(
            self,
            "rows",
            _integer("rows", self.rows, minimum=1, maximum=(1 << 16) - 1),
        )


ScheduledHostEvent: TypeAlias = IngressRecord | GeometryRecord | ResizeRecord


@dataclass(frozen=True, slots=True)
class EgressWatermarks:
    """Caller-provided hard acceptance limits and resume thresholds."""

    high_bytes: int
    low_bytes: int
    high_batches: int
    low_batches: int

    def __post_init__(self) -> None:
        high_bytes = _integer("high_bytes", self.high_bytes, minimum=1)
        low_bytes = _integer("low_bytes", self.low_bytes, minimum=0)
        high_batches = _integer("high_batches", self.high_batches, minimum=1)
        low_batches = _integer("low_batches", self.low_batches, minimum=0)
        if low_bytes >= high_bytes:
            raise ValueError("low_bytes must be less than high_bytes")
        if low_batches >= high_batches:
            raise ValueError("low_batches must be less than high_batches")
        object.__setattr__(self, "high_bytes", high_bytes)
        object.__setattr__(self, "low_bytes", low_bytes)
        object.__setattr__(self, "high_batches", high_batches)
        object.__setattr__(self, "low_batches", low_batches)


@dataclass(frozen=True, slots=True)
class HostPortLimits:
    """All caller-supplied storage limits for one enhanced attachment.

    The control reserve is protected from ordinary ingress.  It includes both
    bytes and event slots so at least one negotiated control response can be
    admitted when ordinary input has reached its own allowance.
    """

    egress: EgressWatermarks
    retained_publication_bytes: int
    ingress_bytes: int
    ingress_events: int
    ingress_control_bytes: int
    ingress_control_events: int
    geometry_events: int

    def __post_init__(self) -> None:
        if not isinstance(self.egress, EgressWatermarks):
            raise TypeError("egress must be EgressWatermarks")
        retained = _integer(
            "retained_publication_bytes",
            self.retained_publication_bytes,
            minimum=1,
        )
        ingress_bytes = _integer("ingress_bytes", self.ingress_bytes, minimum=1)
        ingress_events = _integer(
            "ingress_events", self.ingress_events, minimum=1
        )
        control_bytes = _integer(
            "ingress_control_bytes",
            self.ingress_control_bytes,
            minimum=1,
        )
        control_events = _integer(
            "ingress_control_events",
            self.ingress_control_events,
            minimum=1,
        )
        geometry_events = _integer(
            "geometry_events", self.geometry_events, minimum=1
        )
        if retained > self.egress.high_bytes:
            raise ValueError(
                "retained_publication_bytes must fit the accepted egress byte limit"
            )
        if control_bytes > ingress_bytes:
            raise ValueError("ingress_control_bytes cannot exceed ingress_bytes")
        if control_events > ingress_events:
            raise ValueError("ingress_control_events cannot exceed ingress_events")

        _checked_sum(
            "combined byte storage",
            self.egress.high_bytes,
            retained,
            ingress_bytes,
        )
        _checked_sum(
            "combined record storage",
            self.egress.high_batches,
            1,
            ingress_events,
            geometry_events,
        )
        object.__setattr__(self, "retained_publication_bytes", retained)
        object.__setattr__(self, "ingress_bytes", ingress_bytes)
        object.__setattr__(self, "ingress_events", ingress_events)
        object.__setattr__(self, "ingress_control_bytes", control_bytes)
        object.__setattr__(self, "ingress_control_events", control_events)
        object.__setattr__(self, "geometry_events", geometry_events)

    @property
    def ordinary_ingress_bytes(self) -> int:
        return self.ingress_bytes - self.ingress_control_bytes

    @property
    def ordinary_ingress_events(self) -> int:
        return self.ingress_events - self.ingress_control_events


@dataclass(frozen=True, slots=True)
class EgressPoll:
    """Result of an epoch-qualified terminal poll."""

    status: AdmissionStatus
    delivery: EgressDelivery | None = None


@dataclass(frozen=True, slots=True)
class ScheduledEventPoll:
    """Result of explicitly crossing the fake scheduler boundary."""

    status: AdmissionStatus
    event: ScheduledHostEvent | None = None


class BoundedEgressQueue:
    """Ordered acceptance queue with explicit poll/release ownership.

    Accepted byte and batch capacity remains reserved after ``poll()`` until
    the returned delivery is explicitly released.  This mirrors a terminal
    parser owning the immutable batch outside scheduler settlement.
    """

    def __init__(self, attachment_epoch: int, watermarks: EgressWatermarks):
        if not isinstance(watermarks, EgressWatermarks):
            raise TypeError("watermarks must be EgressWatermarks")
        self._attachment_epoch = _integer(
            "attachment_epoch", attachment_epoch, minimum=1
        )
        self._watermarks = watermarks
        self._queued: deque[EgressBatch] = deque()
        self._in_flight: dict[object, EgressBatch] = {}
        self._accepted_bytes = 0
        self._accepted_batches = 0
        self._next_sequence = 0
        self._backpressured = False
        self._retired = False
        self._lock = threading.RLock()

    @property
    def attachment_epoch(self) -> int:
        return self._attachment_epoch

    @property
    def accepted_bytes(self) -> int:
        with self._lock:
            return self._accepted_bytes

    @property
    def accepted_batches(self) -> int:
        with self._lock:
            return self._accepted_batches

    @property
    def queued_batches(self) -> int:
        with self._lock:
            return len(self._queued)

    @property
    def in_flight_batches(self) -> int:
        with self._lock:
            return len(self._in_flight)

    @property
    def backpressured(self) -> bool:
        with self._lock:
            return self._backpressured and not self._retired

    @property
    def retired(self) -> bool:
        with self._lock:
            return self._retired

    def accept(self, batch: EgressBatch) -> AdmissionStatus:
        """Accept one whole publication or leave ownership with its caller."""
        if not isinstance(batch, EgressBatch):
            raise TypeError("batch must be EgressBatch")
        with self._lock:
            if self._retired or batch.attachment_epoch != self._attachment_epoch:
                return AdmissionStatus.STALE
            if batch.publication_sequence != self._next_sequence:
                raise ValueError(
                    "publication_sequence is not the next ordered publication"
                )
            if self._backpressured:
                return AdmissionStatus.BACKPRESSURED

            payload_bytes = len(batch.payload)
            fits_bytes = payload_bytes <= (
                self._watermarks.high_bytes - self._accepted_bytes
            )
            fits_batches = self._accepted_batches < self._watermarks.high_batches
            if not fits_bytes or not fits_batches:
                self._backpressured = True
                return AdmissionStatus.BACKPRESSURED

            self._queued.append(batch)
            self._accepted_bytes += payload_bytes
            self._accepted_batches += 1
            self._next_sequence += 1
            if (
                self._accepted_bytes >= self._watermarks.high_bytes
                or self._accepted_batches >= self._watermarks.high_batches
            ):
                self._backpressured = True
            return AdmissionStatus.ACCEPTED

    def poll(self) -> EgressPoll:
        """Transfer the oldest queued batch without releasing its capacity."""
        with self._lock:
            if self._retired:
                return EgressPoll(AdmissionStatus.STALE)
            if not self._queued:
                return EgressPoll(AdmissionStatus.ACCEPTED)
            batch = self._queued.popleft()
            delivery_token = object()
            self._in_flight[delivery_token] = batch
            return EgressPoll(
                AdmissionStatus.ACCEPTED,
                EgressDelivery(self, delivery_token, batch),
            )

    def _release(self, delivery_token: object) -> AdmissionStatus:
        with self._lock:
            if self._retired:
                return AdmissionStatus.STALE
            batch = self._in_flight.pop(delivery_token, None)
            if batch is None:
                return AdmissionStatus.STALE
            self._accepted_bytes -= len(batch.payload)
            self._accepted_batches -= 1
            if (
                self._backpressured
                and self._accepted_bytes <= self._watermarks.low_bytes
                and self._accepted_batches <= self._watermarks.low_batches
            ):
                self._backpressured = False
            return AdmissionStatus.ACCEPTED

    def retire(self) -> None:
        """Invalidate all queue and delivery handles without invoking callers."""
        with self._lock:
            if self._retired:
                return
            self._retired = True
            self._queued.clear()
            self._in_flight.clear()
            self._accepted_bytes = 0
            self._accepted_batches = 0
            self._backpressured = False


class EgressDelivery:
    """Explicit ownership handle for one polled immutable batch."""

    __slots__ = (
        "_batch",
        "_delivery_token",
        "_queue",
        "_release_lock",
        "_release_result",
    )

    def __init__(
        self,
        queue: BoundedEgressQueue,
        delivery_token: object,
        batch: EgressBatch,
    ):
        self._batch = batch
        self._queue = queue
        self._delivery_token = delivery_token
        self._release_lock = threading.Lock()
        self._release_result: AdmissionStatus | None = None

    @property
    def batch(self) -> EgressBatch:
        return self._batch

    def release(self) -> AdmissionStatus:
        """Release exactly this batch's byte and batch reservation once."""
        with self._release_lock:
            if self._release_result is None:
                self._release_result = self._queue._release(self._delivery_token)
            return self._release_result

    def __enter__(self) -> EgressBatch:
        return self._batch

    def __exit__(self, exc_type, exc, traceback) -> None:
        self.release()


class _LeaseOwner(Protocol):
    def _lease_poll(self, token: object, epoch: int) -> EgressPoll: ...

    def _lease_machine_egress_quiescent(
        self,
        token: object,
        epoch: int,
    ) -> AdmissionStatus: ...

    def _lease_submit_ingress(
        self,
        token: object,
        epoch: int,
        payload,
        *,
        control: bool,
    ) -> AdmissionStatus: ...

    def _lease_submit_geometry(
        self,
        token: object,
        epoch: int,
        cols: int,
        rows: int,
    ) -> AdmissionStatus: ...

    def _lease_submit_resize(
        self,
        token: object,
        epoch: int,
        payload,
        *,
        cols: int,
        rows: int,
    ) -> AdmissionStatus: ...

    def _lease_resize_admission_ready(
        self,
        token: object,
        epoch: int,
        payload_bytes: int,
    ) -> AdmissionStatus: ...

    def _lease_close(self, token: object, epoch: int) -> AdmissionStatus: ...


class TerminalHostLease:
    """Opaque handle for one exact enhanced-terminal attachment."""

    __slots__ = (
        "_attachment_epoch",
        "_close_lock",
        "_closed_current",
        "_owner",
        "_token",
    )

    def __init__(
        self,
        owner: _LeaseOwner,
        token: object,
        attachment_epoch: int,
    ):
        self._owner = owner
        self._token = token
        self._attachment_epoch = _integer(
            "attachment_epoch", attachment_epoch, minimum=1
        )
        self._close_lock = threading.Lock()
        self._closed_current = False

    @property
    def attachment_epoch(self) -> int:
        return self._attachment_epoch

    def poll_egress(self) -> EgressPoll:
        return self._owner._lease_poll(self._token, self._attachment_epoch)

    def machine_egress_quiescent(self) -> AdmissionStatus:
        """Report whether no accepted or adapter-retained batch remains."""

        return self._owner._lease_machine_egress_quiescent(
            self._token,
            self._attachment_epoch,
        )

    def submit_ingress(self, payload, *, control: bool = False) -> AdmissionStatus:
        return self._owner._lease_submit_ingress(
            self._token,
            self._attachment_epoch,
            payload,
            control=control,
        )

    def submit_geometry(self, cols: int, rows: int) -> AdmissionStatus:
        return self._owner._lease_submit_geometry(
            self._token,
            self._attachment_epoch,
            cols,
            rows,
        )

    def submit_resize(self, payload, *, cols: int, rows: int) -> AdmissionStatus:
        """Atomically admit framed RESIZE ingress and matching geometry."""

        return self._owner._lease_submit_resize(
            self._token,
            self._attachment_epoch,
            payload,
            cols=cols,
            rows=rows,
        )

    def resize_admission_ready(self, payload_bytes: int) -> AdmissionStatus:
        """Preflight one composite resize without reserving capacity."""

        return self._owner._lease_resize_admission_ready(
            self._token,
            self._attachment_epoch,
            payload_bytes,
        )

    def close(self) -> AdmissionStatus:
        with self._close_lock:
            if self._closed_current:
                return AdmissionStatus.ACCEPTED
            result = self._owner._lease_close(
                self._token,
                self._attachment_epoch,
            )
            if result is AdmissionStatus.ACCEPTED:
                self._closed_current = True
            return result

    def _belongs_to(self, owner: object) -> bool:
        return self._owner is owner

    def _identity(self) -> tuple[object, int]:
        return self._token, self._attachment_epoch

    def __enter__(self) -> TerminalHostLease:
        return self

    def __exit__(self, exc_type, exc, traceback) -> None:
        self.close()


@runtime_checkable
class TerminalHost(Protocol):
    """Attachment surface implemented by a machine adapter or exact fake."""

    def attach(self, limits: HostPortLimits) -> TerminalHostLease: ...


__all__ = [
    "AdmissionStatus",
    "BoundedEgressQueue",
    "EgressBatch",
    "EgressDelivery",
    "EgressPoll",
    "EgressWatermarks",
    "GeometryRecord",
    "HostPortLimits",
    "IngressRecord",
    "ResizeRecord",
    "ScheduledEventPoll",
    "ScheduledHostEvent",
    "TerminalHost",
    "TerminalHostLease",
]
