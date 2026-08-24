"""Shared transaction, revision, and geometry authority for APT presentation.

CELL-1 and RETAINED-1 deliberately share one transaction slot, transaction-ID
high-water mark, and presentation revision.  This module contains that small
renderer-neutral authority.  It has no wire parser, transport, or renderer
dependency and therefore can be shared by the legacy CELL path and the
additive retained path without making either one own the other.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass
from enum import Enum

from .apt1 import UINT32_MAX, UINT64_MAX


class PresentationStateError(ValueError):
    """A transaction, result, revision, reset, or geometry invariant failed."""


class TransactionFamily(str, Enum):
    # Initial and ordinary legacy CELL transactions are one family.  Snapshot
    # is a mode of that family, not a second transaction namespace.
    CELL = "CELL"
    PRESENT = "PRESENT"
    OWNER_DROP = "OWNER_DROP"


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


@dataclass(frozen=True, slots=True)
class PresentationGeometry:
    cols: int
    rows: int
    generation: int = 0

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "cols", _integer("cols", self.cols, minimum=1, maximum=UINT32_MAX)
        )
        object.__setattr__(
            self, "rows", _integer("rows", self.rows, minimum=1, maximum=UINT32_MAX)
        )
        object.__setattr__(
            self,
            "generation",
            _integer(
                "generation", self.generation, minimum=0, maximum=UINT64_MAX
            ),
        )


@dataclass(frozen=True, slots=True)
class TransactionLease:
    """Exact authority for the one open transaction-like operation."""

    family: TransactionFamily
    transaction_id: int
    base_revision: int
    presentation_epoch: int


@dataclass(frozen=True, slots=True)
class ResultLease:
    """The post-commit result gate that must settle before another BEGIN."""

    family: TransactionFamily
    transaction_id: int
    revision: int
    succeeded: bool


class PresentationClock:
    """Own one epoch's global transaction/revision state.

    ``reserve`` consumes a transaction ID immediately, including when later
    semantic validation fails.  ``complete_*`` closes the transaction and
    opens a result gate.  ``settle_result`` is intentionally separate so
    returned ordinary credit cannot accidentally authorize another BEGIN
    before the ordered result has been consumed.
    """

    def __init__(
        self,
        *,
        presentation_epoch: int,
        revision: int = 0,
        transaction_high_water: int = 0,
    ) -> None:
        self._presentation_epoch = _integer(
            "presentation_epoch",
            presentation_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        self._revision = _integer(
            "revision", revision, minimum=0, maximum=UINT64_MAX
        )
        self._transaction_high_water = _integer(
            "transaction_high_water",
            transaction_high_water,
            minimum=0,
            maximum=UINT64_MAX,
        )
        self._open: TransactionLease | None = None
        self._result: ResultLease | None = None

    @property
    def presentation_epoch(self) -> int:
        return self._presentation_epoch

    @property
    def revision(self) -> int:
        return self._revision

    @property
    def transaction_high_water(self) -> int:
        return self._transaction_high_water

    @property
    def open_transaction(self) -> TransactionLease | None:
        return self._open

    @property
    def outstanding_result(self) -> ResultLease | None:
        return self._result

    def reserve(
        self,
        family: TransactionFamily,
        transaction_id: int,
        base_revision: int,
    ) -> TransactionLease:
        if not isinstance(family, TransactionFamily):
            raise TypeError("family must be TransactionFamily")
        normalized_id = _integer(
            "transaction_id", transaction_id, minimum=1, maximum=UINT64_MAX
        )
        normalized_base = _integer(
            "base_revision", base_revision, minimum=0, maximum=UINT64_MAX
        )
        if self._open is not None:
            raise PresentationStateError("a presentation transaction is already open")
        if self._result is not None:
            raise PresentationStateError("the preceding transaction result is outstanding")
        if normalized_id <= self._transaction_high_water:
            self._result = ResultLease(
                family, normalized_id, self._revision, False
            )
            raise PresentationStateError(
                "transaction_id is not monotonically increasing"
            )

        # Receipt consumes the ID even if base-revision or later semantic
        # validation rejects the request.
        self._transaction_high_water = normalized_id
        if normalized_base != self._revision:
            self._result = ResultLease(
                family, normalized_id, self._revision, False
            )
            raise PresentationStateError(
                f"base revision {normalized_base} does not match {self._revision}"
            )
        lease = TransactionLease(
            family,
            normalized_id,
            normalized_base,
            self._presentation_epoch,
        )
        self._open = lease
        return lease

    def next_revision(self, lease: TransactionLease) -> int:
        self._require_open(lease)
        if self._revision == UINT64_MAX:
            raise PresentationStateError("presentation revision is exhausted")
        return self._revision + 1

    def complete_success(self, lease: TransactionLease) -> ResultLease:
        next_revision = self.next_revision(lease)
        self._revision = next_revision
        self._open = None
        result = ResultLease(
            lease.family, lease.transaction_id, next_revision, True
        )
        self._result = result
        return result

    def complete_rejected(self, lease: TransactionLease) -> ResultLease:
        self._require_open(lease)
        self._open = None
        result = ResultLease(
            lease.family, lease.transaction_id, self._revision, False
        )
        self._result = result
        return result

    def abort(self, lease: TransactionLease) -> None:
        self._require_open(lease)
        self._open = None

    def settle_result(self, transaction_id: int) -> ResultLease:
        normalized_id = _integer(
            "transaction_id", transaction_id, minimum=1, maximum=UINT64_MAX
        )
        result = self._result
        if result is None:
            raise PresentationStateError("no presentation result is outstanding")
        if result.transaction_id != normalized_id:
            raise PresentationStateError("result transaction_id mismatch")
        self._result = None
        return result

    def soft_reset(self, requested_epoch: int) -> None:
        epoch = _integer(
            "requested_epoch", requested_epoch, minimum=0, maximum=UINT32_MAX
        )
        if self._presentation_epoch == UINT32_MAX:
            raise PresentationStateError("presentation epoch is exhausted")
        if epoch != self._presentation_epoch + 1:
            raise PresentationStateError(
                "requested epoch is not current presentation epoch plus one"
            )
        if self._open is not None or self._result is not None:
            raise PresentationStateError(
                "presentation transaction/result must settle before reset"
            )
        self._presentation_epoch = epoch
        self._revision = 0
        self._transaction_high_water = 0

    def _require_open(self, lease: TransactionLease) -> None:
        if not isinstance(lease, TransactionLease):
            raise TypeError("lease must be TransactionLease")
        if self._open is not lease:
            raise PresentationStateError("transaction lease is not the open authority")


__all__ = [
    "PresentationClock",
    "PresentationGeometry",
    "PresentationStateError",
    "ResultLease",
    "TransactionFamily",
    "TransactionLease",
]
