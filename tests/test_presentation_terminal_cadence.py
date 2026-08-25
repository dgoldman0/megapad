"""Focused tests for latest-view physical presentation cadence."""

from __future__ import annotations

from dataclasses import replace

import pytest

from presentation_terminal.cell_model import Cell, Cursor, TerminalView
from presentation_terminal.presentation_cadence import PresentationCadenceScheduler
from presentation_terminal.presentation_coordinator import CompositePresentationView
from presentation_terminal.presentation_model import (
    PresentationGeometry,
    PresentationStateError,
)
from presentation_terminal.retained_model import (
    RetainedFeature,
    RetainedPolicy,
)


def _policy(interval_us: int) -> RetainedPolicy:
    return RetainedPolicy(
        features=(
            RetainedFeature.CORE
            if interval_us == 0
            else RetainedFeature.CORE | RetainedFeature.CADENCE
        ),
        max_owner_records=1,
        max_live_owners=1,
        max_regions=1,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=1,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=248,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_label_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=interval_us,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=64,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=248,
    )


def _view(
    epoch: int,
    revision: int,
    *,
    attachment_epoch: int = 1,
    session_id: int = 2,
) -> CompositePresentationView:
    geometry = PresentationGeometry(1, 1)
    return CompositePresentationView(
        presentation_epoch=epoch,
        revision=revision,
        geometry=geometry,
        cell=TerminalView(
            attachment_epoch=attachment_epoch,
            session_id=session_id,
            presentation_epoch=epoch,
            revision=revision,
            cols=geometry.cols,
            rows=geometry.rows,
            cells=((Cell(ord(" "), 7, 0),),),
            dirty_spans=(),
            cursor=Cursor(0, 0, True),
        ),
        retained=None,
    )


def test_zero_interval_and_first_view_are_immediately_eligible() -> None:
    clock = [10]
    cadence = PresentationCadenceScheduler(
        policy=_policy(0), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 0)
    cadence.replace_session(1, 2, first)

    assert cadence.pending_revision == 0
    assert cadence.service() is first
    assert cadence.presented_revision == 0
    assert cadence.pending_revision is None

    next_view = _view(0, 1)
    cadence.submit(next_view)
    assert cadence.service() is next_view


def test_default_monotonic_clock_is_available_to_production_callers() -> None:
    cadence = PresentationCadenceScheduler(policy=_policy(0))
    first = _view(0, 0)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first


def test_one_pending_slot_coalesces_to_latest_until_interval_expires() -> None:
    clock = [1_000]
    cadence = PresentationCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    cadence.replace_session(1, 2)
    first = _view(0, 1)
    cadence.submit(first)
    assert cadence.service() is first

    skipped = _view(0, 2)
    latest = _view(0, 3)
    cadence.submit(skipped)
    cadence.submit(latest)
    clock[0] = 1_099
    assert cadence.service() is None
    assert cadence.pending_revision == 3
    clock[0] = 1_100
    assert cadence.service() is latest


def test_exact_view_retry_is_idempotent_but_foreign_revision_is_rejected() -> None:
    cadence = PresentationCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: 0
    )
    first = _view(0, 4)
    cadence.replace_session(1, 2, first)
    cadence.submit(replace(first))
    assert cadence.pending_revision == 4

    with pytest.raises(PresentationStateError, match="same or lower revision"):
        cadence.submit(
            replace(first, cell=replace(first.cell, cursor=Cursor(0, 0, False)))
        )
    with pytest.raises(PresentationStateError, match="same or lower revision"):
        cadence.submit(_view(0, 3))


def test_clock_rollback_cannot_make_a_pending_view_eligible_early() -> None:
    clock = [1_000]
    cadence = PresentationCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    cadence.replace_session(1, 2, _view(0, 1))
    assert cadence.service() is not None
    cadence.submit(_view(0, 2))

    clock[0] = 1_050
    assert cadence.service() is None
    clock[0] = 900
    assert cadence.service() is None
    clock[0] = 1_099
    assert cadence.service() is None
    clock[0] = 1_100
    assert cadence.service() is not None


def test_session_and_epoch_replacement_discard_stale_views_and_reset_eligibility() -> None:
    clock = [1_000]
    cadence = PresentationCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    cadence.replace_session(1, 2, _view(0, 1))
    assert cadence.service() is not None
    cadence.submit(_view(0, 2))

    reset_view = _view(1, 0)
    cadence.reset_presentation_epoch(1, reset_view)
    assert cadence.presented_revision is None
    assert cadence.service() is reset_view
    with pytest.raises(PresentationStateError, match="foreign presentation epoch"):
        cadence.submit(_view(0, 3))

    replacement = _view(0, 0, attachment_epoch=2, session_id=3)
    cadence.replace_session(2, 3, replacement)
    assert cadence.service() is replacement
    with pytest.raises(PresentationStateError, match="foreign session"):
        cadence.submit(_view(0, 1))
