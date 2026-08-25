"""Atomic renderer-neutral publication of CELL and retained terminal output.

The individual models build immutable candidates and retain their own staging
invariants.  :class:`TerminalOutputCoordinator` supplies the publication boundary:
all participating candidates are validated first, one shared update authority
lease completes, and only then are the prevalidated plane states installed.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import cast

from .cell_model import CellModel, PreparedCellPublication, TerminalView
from .update_authority import (
    TerminalUpdateAuthority,
    TerminalGeometry,
    TerminalUpdateError,
    ResultLease,
    TransactionFamily,
    TransactionLease,
)
from .retained_scene import (
    PreparedOwnerRetirement,
    PreparedSceneInstall,
    RetainedSceneModel,
    SceneModelState,
)


@dataclass(frozen=True, slots=True)
class CompositeTerminalView:
    """One immutable logical terminal view with independently shared planes.

    ``revision`` is the authoritative global revision.  A plane object can be
    shared from an older composite when that plane did not participate in the
    transaction, so a plane's own revision can legitimately lag this value.
    """

    presentation_epoch: int
    revision: int
    geometry: TerminalGeometry
    cell: TerminalView | None
    retained: SceneModelState | None


@dataclass(frozen=True, slots=True)
class PreparedOutputInstall:
    """A composite candidate bound to one coordinator and exact source view."""

    view: CompositeTerminalView
    lease: TransactionLease
    cell: PreparedCellPublication | None
    retained: PreparedSceneInstall | None
    _coordinator_token: object
    _source_view: CompositeTerminalView


@dataclass(frozen=True, slots=True)
class PreparedOwnerRetirementPublication:
    """A scene-aware OWNER_DROP candidate bound to one composite source."""

    view: CompositeTerminalView
    lease: TransactionLease
    retirement: PreparedOwnerRetirement
    _coordinator_token: object
    _source_view: CompositeTerminalView


class TerminalOutputCoordinator:
    """Publish CELL-only, retained-only, or mixed state under one clock result."""

    def __init__(
        self,
        *,
        clock: TerminalUpdateAuthority,
        cell_model: CellModel,
        geometry: TerminalGeometry,
        retained_model: RetainedSceneModel | None = None,
    ) -> None:
        if not isinstance(clock, TerminalUpdateAuthority):
            raise TypeError("clock must be TerminalUpdateAuthority")
        if not isinstance(cell_model, CellModel):
            raise TypeError("cell_model must be CellModel")
        if not isinstance(geometry, TerminalGeometry):
            raise TypeError("geometry must be TerminalGeometry")
        if retained_model is not None and not isinstance(
            retained_model, RetainedSceneModel
        ):
            raise TypeError("retained_model must be RetainedSceneModel or None")
        if cell_model.presentation_epoch != clock.presentation_epoch:
            raise TerminalUpdateError(
                "CELL model and update-authority epochs do not match"
            )
        if cell_model.geometry != (geometry.cols, geometry.rows):
            raise TerminalUpdateError(
                "CELL model and composite geometry do not match"
            )
        cell = cell_model.view
        if cell is not None:
            self._validate_cell_view(cell, clock, geometry)
            if cell.revision > clock.revision:
                raise TerminalUpdateError(
                    "CELL view revision is ahead of the update authority"
                )

        retained = None
        if retained_model is not None:
            if retained_model.clock is not clock:
                raise TerminalUpdateError(
                    "retained model does not use the composite update authority"
                )
            retained = retained_model.state
            self._validate_retained_state(retained, clock, geometry)
            if retained.revision > clock.revision:
                raise TerminalUpdateError(
                    "retained state revision is ahead of the update authority"
                )

        self._clock = clock
        self._cell_model = cell_model
        self._retained_model = retained_model
        self._token = object()
        self._selected_geometry = geometry
        self._source_cell = cell
        self._source_retained = retained
        self._view = CompositeTerminalView(
            presentation_epoch=clock.presentation_epoch,
            revision=clock.revision,
            geometry=geometry,
            cell=cell,
            retained=retained,
        )

    @property
    def clock(self) -> TerminalUpdateAuthority:
        return self._clock

    @property
    def view(self) -> CompositeTerminalView:
        return self._view

    def prepare_commit(
        self,
        lease: TransactionLease,
        *,
        cell: PreparedCellPublication | None = None,
        retained: PreparedSceneInstall | None = None,
        geometry: TerminalGeometry | None = None,
    ) -> PreparedOutputInstall:
        """Build and fully validate one composite candidate without publication."""

        if cell is None and retained is None:
            raise TerminalUpdateError(
                "an output commit must contain at least one plane"
            )
        if not isinstance(lease, TransactionLease):
            raise TypeError("lease must be TransactionLease")
        if cell is not None and not isinstance(cell, PreparedCellPublication):
            raise TypeError("cell must be PreparedCellPublication or None")
        if retained is not None and not isinstance(retained, PreparedSceneInstall):
            raise TypeError("retained must be PreparedSceneInstall or None")
        if geometry is None:
            target_geometry = self._selected_geometry
        elif isinstance(geometry, TerminalGeometry):
            target_geometry = geometry
        else:
            raise TypeError("geometry must be TerminalGeometry or None")

        target_revision = self._clock.next_revision(lease)
        prepared = PreparedOutputInstall(
            view=CompositeTerminalView(
                presentation_epoch=self._clock.presentation_epoch,
                revision=target_revision,
                geometry=target_geometry,
                cell=self._source_cell if cell is None else cell.view,
                retained=(
                    self._source_retained if retained is None else retained.state
                ),
            ),
            lease=lease,
            cell=cell,
            retained=retained,
            _coordinator_token=self._token,
            _source_view=self._view,
        )
        self.validate_prepared(prepared)
        return prepared

    def validate_prepared(self, prepared: PreparedOutputInstall) -> None:
        """Validate every source and candidate without mutating any authority."""

        if not isinstance(prepared, PreparedOutputInstall):
            raise TypeError("prepared must be PreparedOutputInstall")
        if (
            prepared._coordinator_token is not self._token
            or prepared._source_view is not self._view
        ):
            raise RuntimeError("prepared output is stale or foreign")
        if self._cell_model.view is not self._source_cell:
            raise RuntimeError("composite CELL source view changed outside coordinator")
        if self._retained_model is None:
            if prepared._source_view.retained is not None or prepared.retained is not None:
                raise RuntimeError("composite has no retained model")
        elif self._retained_model.state is not self._source_retained:
            raise RuntimeError("composite retained source changed outside coordinator")

        lease = prepared.lease
        if self._clock.open_transaction is not lease:
            raise RuntimeError("prepared output lost its transaction lease")
        if not lease.admitted:
            raise TerminalUpdateError(
                "a rejected output transaction cannot be installed"
            )
        if lease.family not in (TransactionFamily.CELL, TransactionFamily.PRESENT):
            raise TerminalUpdateError(
                "lease family cannot publish composite terminal output"
            )
        if lease.family is TransactionFamily.CELL and prepared.retained is not None:
            raise TerminalUpdateError(
                "a CELL transaction cannot contain a retained plane"
            )
        if prepared.cell is None and prepared.retained is None:
            raise TerminalUpdateError(
                "an output commit must contain at least one plane"
            )

        revision = self._clock.next_revision(lease)
        view = prepared.view
        if (
            view.presentation_epoch != self._clock.presentation_epoch
            or view.revision != revision
        ):
            raise RuntimeError("prepared composite revision or epoch is stale")

        if prepared.cell is None:
            if view.cell is not self._source_cell:
                raise RuntimeError("unchanged CELL plane was not structurally shared")
        else:
            self._cell_model.validate_prepared(prepared.cell, lease=lease)
            if prepared.cell.transaction_id != lease.transaction_id:
                raise RuntimeError("prepared CELL transaction ID does not match lease")
            if view.cell is not prepared.cell.view:
                raise RuntimeError("prepared composite has the wrong CELL candidate")
            self._validate_cell_view(view.cell, self._clock, view.geometry)
            if view.cell.revision != revision:
                raise RuntimeError("prepared CELL revision is not the global revision")

        if prepared.retained is None:
            if view.retained is not self._source_retained:
                raise RuntimeError("unchanged retained plane was not structurally shared")
        else:
            retained_model = self._retained_model
            if retained_model is None:
                raise RuntimeError("composite has no retained model")
            if prepared.retained.lease is not lease:
                raise RuntimeError("prepared retained lease does not match composite lease")
            retained_model.validate_prepared(prepared.retained)
            if view.retained is not prepared.retained.state:
                raise RuntimeError("prepared composite has the wrong retained candidate")
            self._validate_retained_state(view.retained, self._clock, view.geometry)
            if view.retained.revision != revision:
                raise RuntimeError("prepared retained revision is not the global revision")

        if view.cell is not None and (
            view.cell.cols != view.geometry.cols or view.cell.rows != view.geometry.rows
        ):
            raise TerminalUpdateError(
                "composite CELL plane does not match terminal geometry"
            )
        if view.retained is not None and view.retained.geometry != view.geometry:
            raise TerminalUpdateError(
                "composite retained plane does not match terminal geometry"
            )

    def install_prepared(self, prepared: PreparedOutputInstall) -> ResultLease:
        """Complete one clock result, then publish all prevalidated state."""

        self.validate_prepared(prepared)

        # No validation, allocation, or policy work may follow this point.
        retained_model = cast(RetainedSceneModel, self._retained_model)
        result = self._clock.complete_success(prepared.lease)
        if prepared.cell is not None:
            self._cell_model._install_prevalidated(prepared.cell)
        if prepared.retained is not None:
            retained_model._install_prevalidated(prepared.retained)
        self._source_cell = prepared.view.cell
        self._source_retained = prepared.view.retained
        self._selected_geometry = prepared.view.geometry
        self._view = prepared.view
        return result

    def admit_resize(self, geometry: TerminalGeometry) -> None:
        """Bind already-selected model state without publishing a half resize.

        The last immutable composite remains the physical view until the peer
        commits the mandatory PRESENT CELL_REPLACE.  Subsequent preparation
        nevertheless uses the newly selected CELL/retained sources.
        """

        if not isinstance(geometry, TerminalGeometry):
            raise TypeError("geometry must be TerminalGeometry")
        if (
            self._clock.open_transaction is not None
            or self._clock.outstanding_result is not None
        ):
            raise TerminalUpdateError("resize admission requires a settled clock")
        if geometry.generation <= self._selected_geometry.generation:
            raise TerminalUpdateError("resize geometry generation is not newer")
        if self._cell_model.geometry != (geometry.cols, geometry.rows):
            raise TerminalUpdateError("CELL model did not select resize geometry")
        if self._cell_model.view is not None or not self._cell_model.awaiting_snapshot:
            raise TerminalUpdateError("CELL model does not require replacement")
        retained_model = self._retained_model
        if retained_model is None:
            raise TerminalUpdateError("retained resize has no retained model")
        retained = retained_model.state
        if retained.geometry != geometry:
            raise TerminalUpdateError("retained model did not select resize geometry")
        if retained.retained_visible or retained.hidden is not None:
            raise TerminalUpdateError("retained resize did not hide stale layout")
        self._selected_geometry = geometry
        self._source_cell = None
        self._source_retained = retained

    def prepare_owner_retirement(
        self,
        lease: TransactionLease,
        retirement: PreparedOwnerRetirement,
    ) -> PreparedOwnerRetirementPublication:
        """Prepare OWNER_DROP publication across ledger, scenes, and composite."""

        if not isinstance(lease, TransactionLease):
            raise TypeError("lease must be TransactionLease")
        if not isinstance(retirement, PreparedOwnerRetirement):
            raise TypeError("retirement must be PreparedOwnerRetirement")
        if retirement.lease is not lease:
            raise TerminalUpdateError(
                "owner retirement and coordinator leases do not match"
            )
        target_revision = self._clock.next_revision(lease)
        prepared = PreparedOwnerRetirementPublication(
            view=CompositeTerminalView(
                presentation_epoch=self._clock.presentation_epoch,
                revision=target_revision,
                geometry=self._view.geometry,
                cell=self._view.cell,
                retained=retirement.state,
            ),
            lease=lease,
            retirement=retirement,
            _coordinator_token=self._token,
            _source_view=self._view,
        )
        self.validate_owner_retirement(prepared)
        return prepared

    def validate_owner_retirement(
        self,
        prepared: PreparedOwnerRetirementPublication,
    ) -> None:
        """Validate all OWNER_DROP sources before the shared clock advances."""

        if not isinstance(prepared, PreparedOwnerRetirementPublication):
            raise TypeError("prepared must be PreparedOwnerRetirementPublication")
        if (
            prepared._coordinator_token is not self._token
            or prepared._source_view is not self._view
        ):
            raise RuntimeError("prepared owner retirement is stale or foreign")
        if self._cell_model.view is not prepared._source_view.cell:
            raise RuntimeError("composite CELL source view changed outside coordinator")
        retained_model = self._retained_model
        if retained_model is None:
            raise RuntimeError("composite has no retained model")
        if retained_model.state is not prepared._source_view.retained:
            raise RuntimeError("composite retained source changed outside coordinator")

        lease = prepared.lease
        if self._clock.open_transaction is not lease:
            raise RuntimeError("prepared owner retirement lost its transaction lease")
        if lease.family is not TransactionFamily.OWNER_DROP or not lease.admitted:
            raise TerminalUpdateError(
                "owner retirement requires an admitted OWNER_DROP transaction"
            )
        if prepared.retirement.lease is not lease:
            raise RuntimeError("prepared retained retirement has the wrong lease")
        retained_model.validate_owner_retirement(prepared.retirement)

        revision = self._clock.next_revision(lease)
        view = prepared.view
        if (
            view.presentation_epoch != self._clock.presentation_epoch
            or view.revision != revision
        ):
            raise RuntimeError("prepared owner retirement revision or epoch is stale")
        if view.geometry != prepared._source_view.geometry:
            raise RuntimeError("owner retirement changed composite geometry")
        if view.cell is not prepared._source_view.cell:
            raise RuntimeError("owner retirement did not share the CELL plane")
        if view.retained is not prepared.retirement.state:
            raise RuntimeError("owner retirement has the wrong retained candidate")
        self._validate_retained_state(
            prepared.retirement.state,
            self._clock,
            view.geometry,
        )

    def install_owner_retirement(
        self,
        prepared: PreparedOwnerRetirementPublication,
    ) -> ResultLease:
        """Complete one OWNER_DROP result and publish all prepared authority."""

        self.validate_owner_retirement(prepared)

        # No validation, allocation, or policy work may follow this point.
        retained_model = cast(RetainedSceneModel, self._retained_model)
        result = self._clock.complete_success(prepared.lease)
        retained_model._install_owner_retirement_prevalidated(
            prepared.retirement
        )
        self._view = prepared.view
        return result

    @staticmethod
    def _validate_cell_view(
        view: TerminalView,
        clock: TerminalUpdateAuthority,
        geometry: TerminalGeometry,
    ) -> None:
        if view.presentation_epoch != clock.presentation_epoch:
            raise TerminalUpdateError(
                "CELL view is outside the update-authority epoch"
            )
        if view.revision > clock.revision + 1:
            raise TerminalUpdateError(
                "CELL view revision is ahead of the update authority"
            )
        if (view.cols, view.rows) != (geometry.cols, geometry.rows):
            raise TerminalUpdateError(
                "CELL view does not match terminal geometry"
            )

    @staticmethod
    def _validate_retained_state(
        state: SceneModelState,
        clock: TerminalUpdateAuthority,
        geometry: TerminalGeometry,
    ) -> None:
        if state.geometry != geometry:
            raise TerminalUpdateError(
                "retained state does not match terminal geometry"
            )
        if state.revision > clock.revision + 1:
            raise TerminalUpdateError(
                "retained state revision is ahead of the update authority"
            )


__all__ = [
    "CompositeTerminalView",
    "PreparedOwnerRetirementPublication",
    "PreparedOutputInstall",
    "TerminalOutputCoordinator",
]
