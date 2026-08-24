"""Focused tests for the shared APT presentation authority."""

from __future__ import annotations

import pytest

from presentation_terminal.apt1 import UINT32_MAX, UINT64_MAX
from presentation_terminal.presentation_model import (
    PresentationClock,
    PresentationGeometry,
    PresentationStateError,
    TransactionFamily,
)


def test_cell_and_present_share_transaction_id_and_revision_domains():
    clock = PresentationClock(presentation_epoch=7)

    # SNAPSHOT is deliberately a mode of the CELL family.  Its first success
    # advances the same clock that subsequent retained transactions use.
    snapshot = clock.reserve(TransactionFamily.CELL, 1, 0)
    assert clock.next_revision(snapshot) == 1
    result = clock.complete_success(snapshot)
    assert (result.transaction_id, result.revision, result.succeeded) == (1, 1, True)
    assert clock.open_transaction is None

    with pytest.raises(PresentationStateError, match="result is outstanding"):
        clock.reserve(TransactionFamily.PRESENT, 2, 1)

    assert clock.settle_result(1) is result
    retained = clock.reserve(TransactionFamily.PRESENT, 2, 1)
    assert clock.complete_success(retained).revision == 2
    clock.settle_result(2)

    cell_delta = clock.reserve(TransactionFamily.CELL, 3, 2)
    assert clock.complete_success(cell_delta).revision == 3
    clock.settle_result(3)
    assert clock.transaction_high_water == 3


def test_rejected_begin_consumes_id_and_holds_result_gate_until_settled():
    clock = PresentationClock(presentation_epoch=0, revision=4)

    with pytest.raises(PresentationStateError, match="base revision"):
        clock.reserve(TransactionFamily.PRESENT, 9, 3)

    rejected = clock.outstanding_result
    assert rejected is not None
    assert rejected.family is TransactionFamily.PRESENT
    assert (rejected.transaction_id, rejected.revision, rejected.succeeded) == (
        9,
        4,
        False,
    )
    assert clock.transaction_high_water == 9
    assert clock.open_transaction is None

    with pytest.raises(PresentationStateError, match="result is outstanding"):
        clock.reserve(TransactionFamily.CELL, 10, 4)

    clock.settle_result(9)
    lease = clock.reserve(TransactionFamily.CELL, 10, 4)
    rejected_commit = clock.complete_rejected(lease)
    assert rejected_commit.revision == 4
    clock.settle_result(10)

    # A stale ID is also an ordered semantic rejection.  It does not move the
    # high-water mark but it does occupy the same result gate.
    with pytest.raises(PresentationStateError, match="monotonically"):
        clock.reserve(TransactionFamily.PRESENT, 9, 4)
    assert clock.transaction_high_water == 10
    assert clock.outstanding_result.transaction_id == 9


def test_abort_releases_transaction_without_revision_or_result():
    clock = PresentationClock(presentation_epoch=3, revision=8)
    lease = clock.reserve(TransactionFamily.PRESENT, 17, 8)

    clock.abort(lease)

    assert clock.revision == 8
    assert clock.transaction_high_water == 17
    assert clock.open_transaction is None
    assert clock.outstanding_result is None
    next_lease = clock.reserve(TransactionFamily.CELL, 18, 8)
    assert next_lease.presentation_epoch == 3


def test_soft_reset_requires_settlement_and_restarts_only_epoch_domains():
    clock = PresentationClock(
        presentation_epoch=4, revision=12, transaction_high_water=20
    )
    lease = clock.reserve(TransactionFamily.OWNER_DROP, 21, 12)
    clock.complete_success(lease)

    with pytest.raises(PresentationStateError, match="must settle"):
        clock.soft_reset(5)

    clock.settle_result(21)
    clock.soft_reset(5)
    assert clock.presentation_epoch == 5
    assert clock.revision == 0
    assert clock.transaction_high_water == 0

    with pytest.raises(PresentationStateError, match="plus one"):
        clock.soft_reset(7)


def test_geometry_has_unsigned_wire_bounds_without_policy_caps():
    geometry = PresentationGeometry(UINT32_MAX, UINT32_MAX, UINT64_MAX)
    assert geometry.cols == UINT32_MAX
    assert geometry.rows == UINT32_MAX
    assert geometry.generation == UINT64_MAX

    with pytest.raises(ValueError, match="between 1"):
        PresentationGeometry(0, 1)
    with pytest.raises(TypeError, match="not bool"):
        PresentationGeometry(True, 1)


def test_wrong_lease_cannot_mutate_the_open_authority():
    first = PresentationClock(presentation_epoch=0)
    second = PresentationClock(presentation_epoch=0)
    lease = first.reserve(TransactionFamily.CELL, 1, 0)
    foreign = second.reserve(TransactionFamily.CELL, 1, 0)

    with pytest.raises(PresentationStateError, match="not the open authority"):
        first.complete_success(foreign)

    assert first.open_transaction is lease
    assert first.revision == 0
