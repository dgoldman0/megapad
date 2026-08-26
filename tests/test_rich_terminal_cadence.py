"""Focused tests for latest-view physical display cadence."""

from __future__ import annotations

from dataclasses import replace

import pytest

from rich_terminal.cell_model import Cell, Cursor, TerminalView
from rich_terminal.display_cadence import DisplayCadenceScheduler
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.update_authority import (
    TerminalGeometry,
    TerminalUpdateError,
)
from rich_terminal.retained_model import (
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
) -> CompositeTerminalView:
    geometry = TerminalGeometry(1, 1)
    return CompositeTerminalView(
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


def test_service_offers_without_early_display_and_ack_starts_cadence() -> None:
    clock = [1_000]
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 0)
    cadence.replace_session(1, 2, first)

    assert cadence.service() is first
    assert cadence.offered_revision == 0
    assert cadence.presented_revision is None
    assert cadence.displayed_revision is None

    latest = _view(0, 1)
    cadence.submit(latest)
    clock[0] = 1_100
    assert cadence.service() is None
    assert cadence.pending_revision == 1

    cadence.acknowledge(first)
    assert cadence.presented_revision == 0
    assert cadence.offered_revision is None
    clock[0] = 1_199
    assert cadence.service() is None
    clock[0] = 1_200
    assert cadence.service() is latest


def test_zero_interval_and_first_view_are_immediately_offerable() -> None:
    clock = [10]
    cadence = DisplayCadenceScheduler(
        policy=_policy(0), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 0)
    cadence.replace_session(1, 2, first)

    assert cadence.pending_revision == 0
    assert cadence.service() is first
    assert cadence.offered_revision == 0
    assert cadence.displayed_revision is None
    assert cadence.pending_revision is None

    next_view = _view(0, 1)
    cadence.submit(next_view)
    assert cadence.service() is None
    cadence.acknowledge(first)
    assert cadence.service() is next_view


def test_default_monotonic_clock_is_available_to_production_callers() -> None:
    cadence = DisplayCadenceScheduler(policy=_policy(0))
    first = _view(0, 0)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.acknowledge(first)
    assert cadence.presented_revision == 0


def test_newer_submissions_coalesce_while_one_offer_awaits_ack() -> None:
    clock = [1_000]
    cadence = DisplayCadenceScheduler(
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
    assert cadence.service() is None
    assert cadence.offered_revision == 1
    assert cadence.pending_revision == 3
    cadence.acknowledge(first)
    clock[0] = 1_099
    assert cadence.service() is None
    clock[0] = 1_100
    assert cadence.service() is latest


def test_exact_view_retry_is_idempotent_but_foreign_revision_is_rejected() -> None:
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: 0
    )
    first = _view(0, 4)
    cadence.replace_session(1, 2, first)
    cadence.submit(replace(first))
    assert cadence.pending_revision == 4

    with pytest.raises(TerminalUpdateError, match="same or lower revision"):
        cadence.submit(
            replace(first, cell=replace(first.cell, cursor=Cursor(0, 0, False)))
        )
    with pytest.raises(TerminalUpdateError, match="same or lower revision"):
        cadence.submit(_view(0, 3))


def test_ack_requires_exact_offer_and_rejects_stale_or_cross_scope_views() -> None:
    cadence = DisplayCadenceScheduler(policy=_policy(0), monotonic_us=lambda: 10)
    offered = _view(0, 1)
    cadence.replace_session(1, 2, offered)
    assert cadence.service() is offered

    with pytest.raises(TerminalUpdateError, match="exact outstanding"):
        cadence.acknowledge(replace(offered))
    with pytest.raises(TerminalUpdateError, match="exact outstanding"):
        cadence.acknowledge(_view(0, 2))
    with pytest.raises(TerminalUpdateError, match="foreign presentation_epoch"):
        cadence.acknowledge(_view(1, 0))
    with pytest.raises(TerminalUpdateError, match="foreign session"):
        cadence.acknowledge(_view(0, 1, attachment_epoch=2, session_id=3))

    cadence.acknowledge(offered)
    with pytest.raises(TerminalUpdateError, match="exact outstanding"):
        cadence.acknowledge(offered)


def test_revoke_requeues_offer_or_coalesces_it_behind_newer_pending() -> None:
    cadence = DisplayCadenceScheduler(policy=_policy(0), monotonic_us=lambda: 0)
    first = _view(0, 1)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.revoke_offer(first)
    assert cadence.offered_revision is None
    assert cadence.pending_revision == 1
    assert cadence.presented_revision is None
    assert cadence.service() is first

    cadence.submit(_view(0, 2))
    latest = _view(0, 3)
    cadence.submit(latest)
    cadence.revoke_offer(first)
    assert cadence.offered_revision is None
    assert cadence.pending_revision == 3
    assert cadence.service() is latest


def test_revoke_presented_requires_exact_identity_and_requeues_immediately() -> None:
    clock = [1_000]
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 1)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.acknowledge(first)

    with pytest.raises(TerminalUpdateError, match="exact acknowledged"):
        cadence.revoke_presented(replace(first))
    cadence.revoke_presented(first)
    assert cadence.presented_revision is None
    assert cadence.offered_revision is None
    assert cadence.pending_revision == 1
    assert cadence.service() is first

    with pytest.raises(TerminalUpdateError, match="exact acknowledged"):
        cadence.revoke_presented(first)


def test_revoke_presented_preserves_a_newer_pending_view() -> None:
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: 1_000
    )
    first = _view(0, 1)
    latest = _view(0, 2)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.acknowledge(first)
    cadence.submit(latest)

    cadence.revoke_presented(first)
    assert cadence.presented_revision is None
    assert cadence.pending_revision == 2
    assert cadence.service() is latest


def test_clock_rollback_cannot_make_a_pending_view_eligible_early() -> None:
    clock = [1_000]
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 1)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.acknowledge(first)
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
    cadence = DisplayCadenceScheduler(
        policy=_policy(100), monotonic_us=lambda: clock[0]
    )
    first = _view(0, 1)
    cadence.replace_session(1, 2, first)
    assert cadence.service() is first
    cadence.acknowledge(first)
    cadence.submit(_view(0, 2))

    reset_view = _view(1, 0)
    cadence.reset_presentation_epoch(1, reset_view)
    assert cadence.displayed_revision is None
    assert cadence.offered_revision is None
    assert cadence.pending_revision == 0
    assert cadence.service() is reset_view
    with pytest.raises(TerminalUpdateError, match="foreign presentation_epoch"):
        cadence.acknowledge(first)
    with pytest.raises(TerminalUpdateError, match="foreign presentation_epoch"):
        cadence.submit(_view(0, 3))

    replacement = _view(0, 0, attachment_epoch=2, session_id=3)
    cadence.replace_session(2, 3, replacement)
    assert cadence.offered_revision is None
    assert cadence.pending_revision == 0
    assert cadence.service() is replacement
    with pytest.raises(TerminalUpdateError, match="foreign session"):
        cadence.submit(_view(0, 1))
