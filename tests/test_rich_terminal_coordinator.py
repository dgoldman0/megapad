"""Focused atomic publication tests for the composite terminal output view."""

from __future__ import annotations

import pytest

from rich_terminal.cell_model import (
    Cell,
    CellModel,
    CellSpan,
    Cursor,
    TransactionBegin,
)
from rich_terminal.output_coordinator import TerminalOutputCoordinator
from rich_terminal.update_authority import (
    TerminalUpdateAuthority,
    TerminalGeometry,
    TransactionFamily,
)
from rich_terminal.retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerQuotas,
    RetainedFeature,
    RetainedPolicy,
)
from rich_terminal.retained_scene import (
    CommitDisposition,
    RegionDefinition,
    RetainedMode,
    RetainedSceneModel,
)
from rich_terminal.retained_resources import RetainedResourceStore


SESSION_ID = 0x0123456789ABCDEF
PRESENTATION_EPOCH = 3
GEOMETRY = TerminalGeometry(2, 2, 0)


def _policy() -> RetainedPolicy:
    return RetainedPolicy(
        features=RetainedFeature.CORE,
        max_owner_records=1,
        max_live_owners=1,
        max_regions=2,
        max_resources=0,
        max_objects=0,
        max_series=0,
        max_operations_per_transaction=4,
        max_resource_chunk_bytes=0,
        max_retained_transaction_bytes=1024,
        total_resource_bytes=0,
        image_format=0,
        max_image_width=0,
        max_image_height=0,
        max_path_points=0,
        max_glyph_run_bytes=0,
        max_samples_per_append=0,
        max_history_per_series=0,
        minimum_presentation_interval_us=0,
        total_sample_slots=0,
        total_utf8_bytes=0,
        client_to_terminal_max_payload=64,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=1024,
    )


def _domain():
    clock = TerminalUpdateAuthority(presentation_epoch=PRESENTATION_EPOCH)
    cell = CellModel(
        attachment_epoch=7,
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        cols=GEOMETRY.cols,
        rows=GEOMETRY.rows,
        max_transaction_bytes=1024,
        max_cells=4,
    )
    owners = OwnerLedger(
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        policy=_policy(),
    )
    owner = OwnerIdentity(SESSION_ID, PRESENTATION_EPOCH, 1, 1)
    owners.open(
        owner,
        OwnerQuotas(
            regions=2,
            resources=0,
            objects=0,
            series=0,
            resource_bytes=0,
            utf8_bytes=0,
            sample_slots=0,
        ),
    )
    retained = RetainedSceneModel(
        clock=clock,
        owners=owners,
        resources=RetainedResourceStore(owners),
        geometry=GEOMETRY,
    )
    coordinator = TerminalOutputCoordinator(
        clock=clock,
        cell_model=cell,
        retained_model=retained,
        geometry=GEOMETRY,
    )
    return clock, cell, owners, owner, retained, coordinator


def _prepare_initial_cell(clock: TerminalUpdateAuthority, cell: CellModel):
    lease = clock.reserve(TransactionFamily.CELL, 1, 0)
    cell.begin_with_lease(
        TransactionBegin(1, 0, 2, 2, 2, 4),
        snapshot=True,
        lease=lease,
    )
    cell.stage_span(
        CellSpan(0, 0, (Cell(ord("A"), 7, 0), Cell(ord("B"), 7, 0)))
    )
    cell.stage_span(
        CellSpan(1, 0, (Cell(ord("C"), 7, 0), Cell(ord("D"), 7, 0)))
    )
    cell.stage_cursor(Cursor(0, 0, True))
    return lease, cell.prepare_publication(
        lease,
        global_revision=clock.next_revision(lease),
    )


def _install_initial_cell(clock, cell, coordinator):
    lease, cell_prepared = _prepare_initial_cell(clock, cell)
    prepared = coordinator.prepare_commit(lease, cell=cell_prepared)
    result = coordinator.install_prepared(prepared)
    clock.settle_result(result.transaction_id)
    return prepared.view


def test_cell_only_prepare_is_nonmutating_and_rejects_foreign_or_stale_installs():
    clock, cell, _, _, retained, coordinator = _domain()
    source = coordinator.view
    retained_source = retained.state
    lease, cell_prepared = _prepare_initial_cell(clock, cell)

    prepared = coordinator.prepare_commit(lease, cell=cell_prepared)
    foreign_coordinator = TerminalOutputCoordinator(
        clock=clock,
        cell_model=cell,
        retained_model=retained,
        geometry=GEOMETRY,
    )
    foreign = foreign_coordinator.prepare_commit(lease, cell=cell_prepared)

    assert coordinator.view is source
    assert cell.view is None
    assert retained.state is retained_source
    assert clock.revision == 0
    assert clock.open_transaction is lease
    assert prepared.view.revision == 1
    assert prepared.view.retained is retained_source

    with pytest.raises(RuntimeError, match="stale or foreign"):
        coordinator.install_prepared(foreign)
    assert clock.revision == 0
    assert clock.open_transaction is lease
    assert cell.view is None

    result = coordinator.install_prepared(prepared)
    assert result.revision == 1
    assert coordinator.view is prepared.view
    assert coordinator.view.cell is cell_prepared.view
    assert coordinator.view.retained is retained_source
    assert cell.view is cell_prepared.view
    assert retained.state is retained_source

    with pytest.raises(RuntimeError, match="stale or foreign"):
        coordinator.install_prepared(prepared)
    assert clock.revision == 1
    assert clock.outstanding_result is result


def test_retained_only_and_mixed_commits_share_or_swap_planes_as_one_view():
    clock, cell, owners, owner, retained, coordinator = _domain()
    initial = _install_initial_cell(clock, cell, coordinator)

    cell_source = initial.cell
    retained_source = retained.state
    ledger_source = owners.state
    retained_lease = clock.reserve(TransactionFamily.PRESENT, 2, 1)
    retained.begin(retained_lease, RetainedMode.REPLACE_START, GEOMETRY)
    retained.define_region(
        RegionDefinition(owner, 1, 0, 0, 2, 2, 0, True, True, 0)
    )
    retained_prepared = retained.prepare_commit(CommitDisposition.COMMIT)
    retained_composite = coordinator.prepare_commit(
        retained_lease,
        retained=retained_prepared,
    )

    assert coordinator.view is initial
    assert retained.state is retained_source
    assert owners.state is ledger_source
    assert clock.revision == 1
    assert retained_composite.view.cell is cell_source
    assert retained_composite.view.retained is retained_prepared.state

    retained_result = coordinator.install_prepared(retained_composite)
    assert retained_result.revision == 2
    assert coordinator.view is retained_composite.view
    assert coordinator.view.cell is cell_source
    assert retained.state is retained_prepared.state
    assert owners.state is retained_prepared.ledger.state
    clock.settle_result(retained_result.transaction_id)

    mixed_source = coordinator.view
    cell_source = cell.view
    retained_source = retained.state
    mixed_lease = clock.reserve(TransactionFamily.PRESENT, 3, 2)
    cell.begin_with_lease(
        TransactionBegin(3, 2, 2, 2, 1, 1),
        snapshot=False,
        lease=mixed_lease,
    )
    cell.stage_span(CellSpan(0, 0, (Cell(ord("Z"), 2, 0),)))
    cell.stage_cursor(Cursor(1, 1, True))
    cell_prepared = cell.prepare_publication(
        mixed_lease,
        global_revision=clock.next_revision(mixed_lease),
    )
    retained.begin(mixed_lease, RetainedMode.REPLACE_CONTINUE, GEOMETRY)
    retained_prepared = retained.prepare_commit(
        CommitDisposition.COMMIT_AND_REVEAL
    )
    mixed = coordinator.prepare_commit(
        mixed_lease,
        cell=cell_prepared,
        retained=retained_prepared,
    )

    assert coordinator.view is mixed_source
    assert cell.view is cell_source
    assert retained.state is retained_source
    assert clock.revision == 2

    result = coordinator.install_prepared(mixed)
    assert result.revision == 3
    assert coordinator.view is mixed.view
    assert coordinator.view.revision == 3
    assert coordinator.view.cell is cell_prepared.view
    assert coordinator.view.retained is retained_prepared.state
    assert cell.view is cell_prepared.view
    assert retained.state is retained_prepared.state
    assert coordinator.view.cell.cells[1] is cell_source.cells[1]
    assert coordinator.view.retained.retained_visible
    assert clock.outstanding_result is result


def test_owner_retirement_publishes_ledger_scene_and_composite_as_one_revision():
    clock, cell, owners, owner, retained, coordinator = _domain()
    _install_initial_cell(clock, cell, coordinator)

    retained_lease = clock.reserve(TransactionFamily.PRESENT, 2, 1)
    retained.begin(retained_lease, RetainedMode.REPLACE_START, GEOMETRY)
    retained.define_region(
        RegionDefinition(owner, 1, 0, 0, 2, 2, 0, True, True, 0)
    )
    retained_prepared = retained.prepare_commit(CommitDisposition.COMMIT)
    retained_publication = coordinator.prepare_commit(
        retained_lease,
        retained=retained_prepared,
    )
    retained_result = coordinator.install_prepared(retained_publication)
    clock.settle_result(retained_result.transaction_id)

    source_view = coordinator.view
    source_cell = cell.view
    source_scene = retained.state
    source_ledger = owners.state
    assert source_scene.hidden is not None
    assert owner.owner_id in source_scene.hidden.owners

    drop_lease = clock.reserve(TransactionFamily.OWNER_DROP, 3, 2)
    retirement = retained.prepare_owner_retirement(drop_lease, owner)
    publication = coordinator.prepare_owner_retirement(drop_lease, retirement)

    assert coordinator.view is source_view
    assert retained.state is source_scene
    assert owners.state is source_ledger
    assert publication.view.revision == 3
    assert publication.view.cell is source_cell
    assert publication.view.retained is retirement.state

    result = coordinator.install_owner_retirement(publication)
    assert result.revision == 3
    assert coordinator.view is publication.view
    assert coordinator.view.cell is source_cell
    assert coordinator.view.retained is retained.state
    assert retained.state.hidden is not None
    assert owner.owner_id not in retained.state.hidden.owners
    assert not owners.state.records[owner.owner_id].live
    assert source_scene.hidden.owners[owner.owner_id].owner == owner


def test_stale_retained_ledger_fails_before_clock_or_composite_publication():
    clock, cell, owners, owner, retained, coordinator = _domain()
    source = _install_initial_cell(clock, cell, coordinator)

    lease = clock.reserve(TransactionFamily.PRESENT, 2, 1)
    retained.begin(lease, RetainedMode.REPLACE_START, GEOMETRY)
    retained.define_region(
        RegionDefinition(owner, 1, 0, 0, 2, 2, 0, True, True, 0)
    )
    retained_prepared = retained.prepare_commit(CommitDisposition.COMMIT)
    prepared = coordinator.prepare_commit(lease, retained=retained_prepared)
    scene_source = retained.state
    cell_source = cell.view

    # Simulate an out-of-band owner lifecycle mutation after composite
    # preparation.  The retained candidate's exact ledger provenance is now
    # stale, and that must be detected before completing the shared clock.
    owners.install_prepared(owners.prepare_drop(owner))
    with pytest.raises(RuntimeError, match="stale or foreign"):
        coordinator.install_prepared(prepared)

    assert clock.revision == 1
    assert clock.open_transaction is lease
    assert clock.outstanding_result is None
    assert coordinator.view is source
    assert cell.view is cell_source
    assert retained.state is scene_source
    assert retained.transaction_open
