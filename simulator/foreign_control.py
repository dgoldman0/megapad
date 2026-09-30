"""Stack-local authority for task foreign return cells.

This module owns no dispatcher, adapter, native frame or callback. Exact issued
entry identities remain in a bounded live table. Retained stack metadata keeps
only a small tombstone after retirement, so restoring raw cells or snapshots
cannot recreate authority.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum

from shared.cells import CELL_BYTES, MASK64


MAX_FOREIGN_RETURNS = 8
_CONTROL_ISSUANCE = object()


class ForeignControlError(RuntimeError):
    """Foreign return authority is missing, stale, or inconsistent."""


class ForeignRetirementReason(str, Enum):
    RETURNED = "returned"
    FRONTIER = "frontier"
    COOKIE = "cookie"
    METADATA = "metadata"
    RESTORED = "restored"
    CLEARED = "cleared"
    CLOSED = "closed"


def _uint64(value: int, label: str, *, positive: bool = False) -> None:
    if type(value) is not int:
        raise TypeError(f"{label} must be an exact uint64 integer")
    if not (1 if positive else 0) <= value <= MASK64:
        raise ValueError(f"{label} is outside the uint64 range")


@dataclass(frozen=True, slots=True, eq=False, kw_only=True)
class ForeignContinuation:
    """Copyable diagnostics; only the control's exact issued object is live.

    Retirement is permanent for an issued identity. Its marker contains no
    frame, code, stack, issuer or native owner reference. Constructing/copying
    this class, including a visually identical live-looking value, grants no
    return authority.
    """

    root_id: int
    frame_id: int
    request_id: int
    slot_address: int
    raw_cookie: int
    _retirement: ForeignRetirementReason | None = field(default=None, init=False, repr=False)

    def __post_init__(self) -> None:
        for label in ("root_id", "frame_id", "request_id"):
            _uint64(getattr(self, label), label, positive=True)
        _uint64(self.slot_address, "slot address")
        if self.slot_address % CELL_BYTES:
            raise ValueError("foreign return slot must be cell aligned")
        _uint64(self.raw_cookie, "raw cookie")

    @property
    def retired(self) -> bool:
        return self._retirement is not None

    @property
    def retirement_reason(self) -> ForeignRetirementReason | None:
        return self._retirement


@dataclass(frozen=True, slots=True, eq=False)
class ForeignRetirement:
    entry: ForeignContinuation = field(repr=False)
    reason: ForeignRetirementReason
    generation: int
    root_id: int
    frame_id: int
    request_id: int
    slot_address: int
    raw_cookie: int

    def __post_init__(self) -> None:
        if type(self.entry) is not ForeignContinuation:
            raise TypeError("retirement requires an exact ForeignContinuation")
        if type(self.reason) is not ForeignRetirementReason:
            raise TypeError("retirement reason must be an exact ForeignRetirementReason")
        _uint64(self.generation, "retirement generation", positive=True)
        for label in ("root_id", "frame_id", "request_id"):
            _uint64(getattr(self, label), label, positive=True)
        _uint64(self.slot_address, "slot address")
        _uint64(self.raw_cookie, "raw cookie")
        if self.slot_address % CELL_BYTES:
            raise ValueError("retired foreign slot must be cell aligned")


@dataclass(frozen=True, slots=True)
class _LiveReturn:
    entry: ForeignContinuation
    root_id: int
    frame_id: int
    request_id: int
    slot_address: int
    raw_cookie: int


class ForeignReturnControl:
    """One stack-issued owner; all public transitions require exact issuer identity.

    Retirement only records evidence. A dispatcher must drain and settle it
    after the complete semantic operation, before publishing another foreign
    return. The live table plus pending queue never exceeds eight entries.
    """

    __slots__ = (
        "_stack", "_issuer", "_memory", "_view", "_floor", "_empty",
        "_live", "_pending", "_generation", "_closed",
    )

    def __init__(self, stack, issuer: object, *, _issuance=None) -> None:
        if _issuance is not _CONTROL_ISSUANCE:
            raise TypeError("foreign return control must be issued by ReturnStack")
        self._stack = stack
        self._issuer = issuer
        self._memory = stack._memory
        self._view = stack._memory_view
        self._floor = stack._floor
        self._empty = stack._empty_pointer
        self._live: list[_LiveReturn] = []
        self._pending: list[ForeignRetirement] = []
        self._generation = 0
        self._closed = False

    @classmethod
    def _issue(cls, stack, issuer: object) -> ForeignReturnControl:
        from simulator.memory import SparseAddressSpace, _QualifiedOrdinarySpan
        from simulator.stacks import ReturnStack

        if (type(stack) is not ReturnStack or type(stack._memory) is not SparseAddressSpace
                or type(stack._memory_view) is not _QualifiedOrdinarySpan):
            raise TypeError("foreign returns require an exact backed ReturnStack")
        if issuer is None:
            raise TypeError("foreign issuer must be a non-None opaque identity")
        return cls(stack, issuer, _issuance=_CONTROL_ISSUANCE)

    @property
    def live_count(self) -> int:
        self._require_control_values()
        return len(self._live)

    @property
    def pending_count(self) -> int:
        self._require_control_values()
        return len(self._pending)

    @property
    def closed(self) -> bool:
        if type(self._closed) is not bool:
            raise ForeignControlError("foreign closed state is not an exact boolean")
        return self._closed

    def _require_issuer(self, issuer: object) -> None:
        if issuer is not self._issuer:
            raise ForeignControlError("foreign return issuer does not own this control")
        if type(self._closed) is not bool:
            raise ForeignControlError("foreign closed state is not an exact boolean")
        if self._closed:
            raise ForeignControlError("foreign return control is closed")
        self._require_stack()

    def _require_control_values(self) -> None:
        if type(self._closed) is not bool:
            raise ForeignControlError("foreign closed state is not an exact boolean")
        if type(self._generation) is not int or not 0 <= self._generation <= MASK64:
            raise ForeignControlError("foreign retirement generation is not an exact uint64")
        if (type(self._live) is not list or type(self._pending) is not list
                or len(self._live) + len(self._pending) > MAX_FOREIGN_RETURNS):
            raise ForeignControlError("foreign live and pending tables must be bounded exact lists")
        # Validate every scalar before ordering, comparison, hashing or access.
        for record in self._live:
            if type(record) is not _LiveReturn or type(record.entry) is not ForeignContinuation:
                raise ForeignControlError("foreign live record identity type changed")
            for label in ("root_id", "frame_id", "request_id", "slot_address", "raw_cookie"):
                value = getattr(record, label)
                minimum = 1 if label.endswith("_id") else 0
                if type(value) is not int or not minimum <= value <= MASK64:
                    raise ForeignControlError("foreign live record must contain exact uint64 values")
        previous_generation = 0
        for record in self._pending:
            if type(record) is not ForeignRetirement:
                raise ForeignControlError("foreign pending record identity type changed")
            try:
                ForeignRetirement.__post_init__(record)
            except (TypeError, ValueError) as exc:
                raise ForeignControlError("foreign pending record values changed") from exc
            if not previous_generation < record.generation <= self._generation:
                raise ForeignControlError("foreign pending generations are inconsistent")
            previous_generation = record.generation

    def _require_stack(self) -> None:
        from simulator.memory import SparseAddressSpace, _QualifiedOrdinarySpan
        from simulator.stacks import ReturnStack

        self._require_control_values()
        stack = self._stack
        if self._closed or type(stack) is not ReturnStack or stack._foreign_control is not self:
            raise ForeignControlError("foreign return control is no longer bound to its stack")
        if (type(self._floor) is not int or type(self._empty) is not int
                or not 0 <= self._floor < self._empty <= MASK64 + 1
                or self._floor % CELL_BYTES or self._empty % CELL_BYTES
                or type(self._memory) is not SparseAddressSpace
                or type(self._view) is not _QualifiedOrdinarySpan
                or stack._memory is not self._memory or stack._memory_view is not self._view
                or type(stack._floor) is not int or stack._floor != self._floor
                or type(stack._empty_pointer) is not int or stack._empty_pointer != self._empty
                or type(stack._pointer) is not int
                or not self._floor <= stack._pointer <= self._empty
                or stack._pointer % CELL_BYTES):
            raise ForeignControlError("foreign return stack geometry changed")
        metadata = stack._continuations
        if type(metadata) is not dict or any(type(key) is not int for key in metadata):
            raise ForeignControlError("foreign return metadata is not a canonical dictionary")
        previous_slot = self._empty
        root_id = self._live[0].root_id if self._live else None
        frames = set()
        identities = set()
        for record in self._live:
            if (not self._floor <= record.slot_address < previous_slot
                    or record.slot_address % CELL_BYTES or record.root_id != root_id
                    or record.frame_id in frames or id(record.entry) in identities):
                raise ForeignControlError("foreign live slot ownership is inconsistent")
            frames.add(record.frame_id)
            identities.add(id(record.entry))
            previous_slot = record.slot_address

    def _invalid_reason(self, record: _LiveReturn) -> ForeignRetirementReason | None:
        stack = self._stack
        if not stack._pointer <= record.slot_address < self._empty:
            return ForeignRetirementReason.FRONTIER
        entry = record.entry
        for label in ("root_id", "frame_id", "request_id", "slot_address", "raw_cookie"):
            value = getattr(entry, label)
            if type(value) is not int or value != getattr(record, label):
                return ForeignRetirementReason.METADATA
        if entry._retirement is not None:
            return ForeignRetirementReason.METADATA
        metadata = dict.get(stack._continuations, record.slot_address)
        if (type(metadata) is not tuple or len(metadata) != 2 or metadata[0] is not entry
                or type(metadata[1]) is not int or metadata[1] != record.raw_cookie):
            return ForeignRetirementReason.METADATA
        raw = self._view.read64(record.slot_address)
        if type(raw) is not int or raw != record.raw_cookie:
            return ForeignRetirementReason.COOKIE
        return None

    def _retire_suffix(self, index: int, reason: ForeignRetirementReason) -> None:
        suffix = self._live[index:]
        if not suffix:
            return
        if self._generation > MASK64 - len(suffix):
            raise ForeignControlError("foreign retirement generation space exhausted")
        records = [
            ForeignRetirement(item.entry, reason, self._generation + offset,
                              item.root_id, item.frame_id, item.request_id,
                              item.slot_address, item.raw_cookie)
            for offset, item in enumerate(reversed(suffix), 1)
        ]
        # Allocate the bounded result before changing its issued identities.
        pending = self._pending + records
        if len(pending) > MAX_FOREIGN_RETURNS:
            raise AssertionError("foreign retirement queue exceeded its live bound")
        for record in records:
            object.__setattr__(record.entry, "_retirement", reason)
        del self._live[index:]
        self._generation += len(records)
        self._pending = pending

    def _reconcile(self) -> None:
        self._require_stack()
        for index, record in enumerate(self._live):
            reason = self._invalid_reason(record)
            if reason is not None:
                self._retire_suffix(index, reason)
                return

    def reconcile(self, issuer: object) -> None:
        self._require_issuer(issuer)
        self._reconcile()

    def _frontier_changed(self) -> None:
        self._require_stack()
        for index, record in enumerate(self._live):
            if not self._stack._pointer <= record.slot_address < self._empty:
                self._retire_suffix(index, ForeignRetirementReason.FRONTIER)
                return

    def _raw_mismatch(self, address: int) -> None:
        self._require_stack()
        for index, record in enumerate(self._live):
            if record.slot_address == address:
                self._retire_suffix(index, ForeignRetirementReason.COOKIE)
                return

    def _retire_all(self, reason: ForeignRetirementReason) -> None:
        self._require_stack()
        self._retire_suffix(0, reason)

    def push(
        self, issuer: object, *, root_id: int, frame_id: int, request_id: int,
    ) -> ForeignContinuation:
        self._require_issuer(issuer)
        for label, value in (("root ID", root_id), ("frame ID", frame_id),
                             ("request ID", request_id)):
            _uint64(value, label, positive=True)
        self._reconcile()
        if self._pending:
            raise ForeignControlError("drain foreign retirement records before another push")
        if len(self._live) >= MAX_FOREIGN_RETURNS:
            raise ForeignControlError("at most eight foreign return slots may be live")
        if self._live and root_id != self._live[0].root_id:
            raise ForeignControlError("live foreign return slots must share their root")
        if any(record.frame_id == frame_id for record in self._live):
            raise ForeignControlError("a frame already owns a pending foreign return")
        stack = self._stack
        stack.require_push_capacity(1)
        slot = stack._push_address()
        if (type(stack._continuation_cookie) is not int
                or not 0 <= stack._continuation_cookie < MASK64):
            raise ForeignControlError("foreign cookie counter must be an unexhausted exact uint64")
        raw = stack._next_continuation_cookie(0)
        entry = ForeignContinuation(root_id=root_id, frame_id=frame_id, request_id=request_id,
                                    slot_address=slot, raw_cookie=raw)
        record = _LiveReturn(entry, root_id, frame_id, request_id, slot, raw)
        live = self._live + [record]
        self._view.write64(slot, raw)
        stack._continuations[slot] = (entry, raw)
        stack._pointer = slot
        self._live = live
        return entry

    def consume(self, issuer: object, expected: ForeignContinuation) -> ForeignContinuation:
        self._require_issuer(issuer)
        if type(expected) is not ForeignContinuation:
            raise TypeError("foreign return requires an exact ForeignContinuation")
        self._reconcile()
        if self._pending:
            raise ForeignControlError("pending foreign retirements must be settled before return")
        if (not self._live or self._live[-1].entry is not expected
                or self._stack._pointer != self._live[-1].slot_address):
            raise ForeignControlError("foreign return is not the exact live top entry")
        self._retire_suffix(len(self._live) - 1, ForeignRetirementReason.RETURNED)
        # Keep raw bytes and typed metadata as a non-resumable tombstone.
        self._stack._pointer += CELL_BYTES
        return expected

    def drain_retired(self, issuer: object) -> tuple[ForeignRetirement, ...]:
        self._require_issuer(issuer)
        self._reconcile()
        result = tuple(self._pending)
        self._pending.clear()
        return result

    def close(self, issuer: object) -> tuple[ForeignRetirement, ...]:
        if issuer is not self._issuer:
            raise ForeignControlError("foreign return issuer does not own this control")
        if type(self._closed) is not bool:
            raise ForeignControlError("foreign closed state is not an exact boolean")
        if self._closed:
            return ()
        self._require_stack()
        self._retire_all(ForeignRetirementReason.CLOSED)
        result = tuple(self._pending)
        self._pending.clear()
        self._stack._foreign_control = None
        self._stack = self._memory = self._view = None
        self._closed = True
        return result


__all__ = [
    "MAX_FOREIGN_RETURNS", "ForeignControlError", "ForeignRetirementReason",
    "ForeignContinuation", "ForeignRetirement", "ForeignReturnControl",
]
