"""Atomic renderer-neutral publication of CELL and retained presentation planes.

The individual models build immutable candidates and retain their own staging
invariants.  :class:`PresentationCoordinator` supplies the publication boundary:
all participating candidates are validated first, one shared presentation clock
lease completes, and only then are the prevalidated plane states installed.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import cast

from .cell_model import CellModel, PreparedCellPublication, TerminalView
from .presentation_model import (
    PresentationClock,
    PresentationGeometry,
    PresentationStateError,
    ResultLease,
    TransactionFamily,
    TransactionLease,
)
from .retained_scene import PreparedSceneInstall, RetainedSceneModel, SceneModelState


@dataclass(frozen=True, slots=True)
class CompositePresentationView:
    """One immutable logical presentation with independently shared planes.

    ``revision`` is the authoritative global revision.  A plane object can be
    shared from an older composite when that plane did not participate in the
    transaction, so a plane's own revision can legitimately lag this value.
    """

    presentation_epoch: int
    revision: int
    geometry: PresentationGeometry
    cell: TerminalView | None
    retained: SceneModelState | None


@dataclass(frozen=True, slots=True)
class PreparedPresentationInstall:
    """A composite candidate bound to one coordinator and exact source view."""

    view: CompositePresentationView
    lease: TransactionLease
    cell: PreparedCellPublication | None
    retained: PreparedSceneInstall | None
    _coordinator_token: object
    _source_view: CompositePresentationView


class PresentationCoordinator:
    """Publish CELL-only, retained-only, or mixed state under one clock result."""

    def __init__(
        self,
        *,
        clock: PresentationClock,
        cell_model: CellModel,
        geometry: PresentationGeometry,
        retained_model: RetainedSceneModel | None = None,
    ) -> None:
        if not isinstance(clock, PresentationClock):
            raise TypeError("clock must be PresentationClock")
        if not isinstance(cell_model, CellModel):
            raise TypeError("cell_model must be CellModel")
        if not isinstance(geometry, PresentationGeometry):
            raise TypeError("geometry must be PresentationGeometry")
        if retained_model is not None and not isinstance(
            retained_model, RetainedSceneModel
        ):
            raise TypeError("retained_model must be RetainedSceneModel or None")
        if cell_model.presentation_epoch != clock.presentation_epoch:
            raise PresentationStateError(
                "CELL model and presentation clock epochs do not match"
            )
        if cell_model.geometry != (geometry.cols, geometry.rows):
            raise PresentationStateError(
                "CELL model and composite geometry do not match"
            )
        cell = cell_model.view
        if cell is not None:
            self._validate_cell_view(cell, clock, geometry)
            if cell.revision > clock.revision:
                raise PresentationStateError(
                    "CELL view revision is ahead of the presentation clock"
                )

        retained = None
        if retained_model is not None:
            if retained_model.clock is not clock:
                raise PresentationStateError(
                    "retained model does not use the composite presentation clock"
                )
            retained = retained_model.state
            self._validate_retained_state(retained, clock, geometry)
            if retained.revision > clock.revision:
                raise PresentationStateError(
                    "retained state revision is ahead of the presentation clock"
                )

        self._clock = clock
        self._cell_model = cell_model
        self._retained_model = retained_model
        self._token = object()
        self._view = CompositePresentationView(
            presentation_epoch=clock.presentation_epoch,
            revision=clock.revision,
            geometry=geometry,
            cell=cell,
            retained=retained,
        )

    @property
    def clock(self) -> PresentationClock:
        return self._clock

    @property
    def view(self) -> CompositePresentationView:
        return self._view

    def prepare_commit(
        self,
        lease: TransactionLease,
        *,
        cell: PreparedCellPublication | None = None,
        retained: PreparedSceneInstall | None = None,
        geometry: PresentationGeometry | None = None,
    ) -> PreparedPresentationInstall:
        """Build and fully validate one composite candidate without publication."""

        if cell is None and retained is None:
            raise PresentationStateError(
                "a presentation commit must contain at least one plane"
            )
        if not isinstance(lease, TransactionLease):
            raise TypeError("lease must be TransactionLease")
        if cell is not None and not isinstance(cell, PreparedCellPublication):
            raise TypeError("cell must be PreparedCellPublication or None")
        if retained is not None and not isinstance(retained, PreparedSceneInstall):
            raise TypeError("retained must be PreparedSceneInstall or None")
        if geometry is None:
            target_geometry = self._view.geometry
        elif isinstance(geometry, PresentationGeometry):
            target_geometry = geometry
        else:
            raise TypeError("geometry must be PresentationGeometry or None")

        target_revision = self._clock.next_revision(lease)
        prepared = PreparedPresentationInstall(
            view=CompositePresentationView(
                presentation_epoch=self._clock.presentation_epoch,
                revision=target_revision,
                geometry=target_geometry,
                cell=self._view.cell if cell is None else cell.view,
                retained=(
                    self._view.retained if retained is None else retained.state
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

    def validate_prepared(self, prepared: PreparedPresentationInstall) -> None:
        """Validate every source and candidate without mutating any authority."""

        if not isinstance(prepared, PreparedPresentationInstall):
            raise TypeError("prepared must be PreparedPresentationInstall")
        if (
            prepared._coordinator_token is not self._token
            or prepared._source_view is not self._view
        ):
            raise RuntimeError("prepared presentation is stale or foreign")
        if self._cell_model.view is not prepared._source_view.cell:
            raise RuntimeError("composite CELL source view changed outside coordinator")
        if self._retained_model is None:
            if prepared._source_view.retained is not None or prepared.retained is not None:
                raise RuntimeError("composite has no retained model")
        elif self._retained_model.state is not prepared._source_view.retained:
            raise RuntimeError("composite retained source changed outside coordinator")

        lease = prepared.lease
        if self._clock.open_transaction is not lease:
            raise RuntimeError("prepared presentation lost its transaction lease")
        if not lease.admitted:
            raise PresentationStateError(
                "a rejected presentation transaction cannot be installed"
            )
        if lease.family not in (TransactionFamily.CELL, TransactionFamily.PRESENT):
            raise PresentationStateError(
                "lease family cannot publish a composite presentation"
            )
        if lease.family is TransactionFamily.CELL and prepared.retained is not None:
            raise PresentationStateError(
                "a CELL transaction cannot contain a retained plane"
            )
        if prepared.cell is None and prepared.retained is None:
            raise PresentationStateError(
                "a presentation commit must contain at least one plane"
            )

        revision = self._clock.next_revision(lease)
        view = prepared.view
        if (
            view.presentation_epoch != self._clock.presentation_epoch
            or view.revision != revision
        ):
            raise RuntimeError("prepared composite revision or epoch is stale")

        if prepared.cell is None:
            if view.cell is not prepared._source_view.cell:
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
            if view.retained is not prepared._source_view.retained:
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
            raise PresentationStateError(
                "composite CELL plane does not match presentation geometry"
            )
        if view.retained is not None and view.retained.geometry != view.geometry:
            raise PresentationStateError(
                "composite retained plane does not match presentation geometry"
            )

    def install_prepared(self, prepared: PreparedPresentationInstall) -> ResultLease:
        """Complete one clock result, then publish all prevalidated state."""

        self.validate_prepared(prepared)

        # No validation, allocation, or policy work may follow this point.
        retained_model = cast(RetainedSceneModel, self._retained_model)
        result = self._clock.complete_success(prepared.lease)
        if prepared.cell is not None:
            self._cell_model._install_prevalidated(prepared.cell)
        if prepared.retained is not None:
            retained_model._install_prevalidated(prepared.retained)
        self._view = prepared.view
        return result

    @staticmethod
    def _validate_cell_view(
        view: TerminalView,
        clock: PresentationClock,
        geometry: PresentationGeometry,
    ) -> None:
        if view.presentation_epoch != clock.presentation_epoch:
            raise PresentationStateError(
                "CELL view is outside the presentation clock epoch"
            )
        if view.revision > clock.revision + 1:
            raise PresentationStateError(
                "CELL view revision is ahead of the presentation clock"
            )
        if (view.cols, view.rows) != (geometry.cols, geometry.rows):
            raise PresentationStateError(
                "CELL view does not match presentation geometry"
            )

    @staticmethod
    def _validate_retained_state(
        state: SceneModelState,
        clock: PresentationClock,
        geometry: PresentationGeometry,
    ) -> None:
        if state.geometry != geometry:
            raise PresentationStateError(
                "retained state does not match presentation geometry"
            )
        if state.revision > clock.revision + 1:
            raise PresentationStateError(
                "retained state revision is ahead of the presentation clock"
            )


__all__ = [
    "CompositePresentationView",
    "PreparedPresentationInstall",
    "PresentationCoordinator",
]
