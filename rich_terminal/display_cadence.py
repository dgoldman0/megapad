"""Renderer-neutral physical display cadence and latest-view coalescing."""

from __future__ import annotations

import operator
import time
from collections.abc import Callable

from .apt1 import UINT32_MAX, UINT64_MAX
from .cell_model import TerminalView
from .output_coordinator import CompositeTerminalView
from .update_authority import TerminalUpdateError
from .retained_model import RetainedPolicy


def _system_monotonic_us() -> int:
    return time.monotonic_ns() // 1_000


def _integer(name: str, value, *, minimum: int, maximum: int | None = None) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if result < minimum or (maximum is not None and result > maximum):
        upper = "unbounded" if maximum is None else str(maximum)
        raise ValueError(f"{name} must be between {minimum} and {upper}")
    return int(result)


class DisplayCadenceScheduler:
    """Coalesce committed views without delaying logical protocol service.

    The scheduler retains at most the physically displayed view and one latest
    pending view.  Session and presentation_epoch transitions are explicit so
    a stale immutable view cannot re-enter a replacement scope.
    """

    def __init__(
        self,
        *,
        policy: RetainedPolicy,
        monotonic_us: Callable[[], int] = _system_monotonic_us,
    ) -> None:
        if not isinstance(policy, RetainedPolicy):
            raise TypeError("policy must be RetainedPolicy")
        if not callable(monotonic_us):
            raise TypeError("monotonic_us must be callable")
        self._minimum_interval_us = policy.minimum_presentation_interval_us
        self._monotonic_us = monotonic_us
        self._attachment_epoch: int | None = None
        self._session_id: int | None = None
        self._presentation_epoch: int | None = None
        self._displayed: CompositeTerminalView | None = None
        self._pending: CompositeTerminalView | None = None
        self._last_observed_us: int | None = None
        self._last_displayed_us: int | None = None

    @property
    def displayed_revision(self) -> int | None:
        return None if self._displayed is None else self._displayed.revision

    @property
    def pending_revision(self) -> int | None:
        return None if self._pending is None else self._pending.revision

    def replace_session(
        self,
        attachment_epoch: int,
        session_id: int,
        initial_view: CompositeTerminalView | None = None,
    ) -> None:
        """Replace all renderer state with one new protocol session at epoch zero."""

        attachment = _integer(
            "attachment_epoch", attachment_epoch, minimum=1, maximum=UINT64_MAX
        )
        session = _integer("session_id", session_id, minimum=1, maximum=UINT64_MAX)
        self._validate_initial_view(
            initial_view,
            attachment_epoch=attachment,
            session_id=session,
            presentation_epoch=0,
        )

        self._attachment_epoch = attachment
        self._session_id = session
        self._presentation_epoch = 0
        self._displayed = None
        self._pending = initial_view
        self._last_displayed_us = None

    def reset_presentation_epoch(
        self,
        new_epoch: int,
        initial_view: CompositeTerminalView | None = None,
    ) -> None:
        """Discard the prior epoch and make the first replacement immediately due."""

        current = self._require_session()
        epoch = _integer(
            "new_epoch", new_epoch, minimum=0, maximum=UINT32_MAX
        )
        if current == UINT32_MAX or epoch != current + 1:
            raise TerminalUpdateError(
                "new_epoch is not current presentation_epoch plus one"
            )
        assert self._attachment_epoch is not None
        assert self._session_id is not None
        self._validate_initial_view(
            initial_view,
            attachment_epoch=self._attachment_epoch,
            session_id=self._session_id,
            presentation_epoch=epoch,
        )

        self._presentation_epoch = epoch
        self._displayed = None
        self._pending = initial_view
        self._last_displayed_us = None

    def submit(self, view: CompositeTerminalView) -> None:
        """Retain the newest current-scope logical view for physical display."""

        if not isinstance(view, CompositeTerminalView):
            raise TypeError("view must be CompositeTerminalView")
        current_epoch = self._require_session()
        assert self._attachment_epoch is not None
        assert self._session_id is not None
        self._validate_view(
            view,
            attachment_epoch=self._attachment_epoch,
            session_id=self._session_id,
            presentation_epoch=current_epoch,
        )

        if view == self._pending or view == self._displayed:
            return
        newest = self._pending if self._pending is not None else self._displayed
        if newest is not None and view.revision <= newest.revision:
            raise TerminalUpdateError(
                "same or lower revision cannot replace a different view"
            )
        self._pending = view

    def service(self) -> CompositeTerminalView | None:
        """Return the newest pending view at its first eligible opportunity."""

        pending = self._pending
        if pending is None:
            return None
        if self._minimum_interval_us == 0:
            return self._display(pending, displayed_at_us=None)

        now = self._read_monotonic_us()
        last = self._last_displayed_us
        if last is not None and now - last < self._minimum_interval_us:
            return None
        return self._display(pending, displayed_at_us=now)

    def _display(
        self,
        view: CompositeTerminalView,
        *,
        displayed_at_us: int | None,
    ) -> CompositeTerminalView:
        self._pending = None
        self._displayed = view
        self._last_displayed_us = displayed_at_us
        return view

    def _read_monotonic_us(self) -> int:
        observed = _integer("monotonic_us result", self._monotonic_us(), minimum=0)
        previous = self._last_observed_us
        if previous is not None and observed < previous:
            observed = previous
        self._last_observed_us = observed
        return observed

    def _require_session(self) -> int:
        epoch = self._presentation_epoch
        if self._attachment_epoch is None or self._session_id is None or epoch is None:
            raise TerminalUpdateError("no rich-terminal session is active")
        return epoch

    @classmethod
    def _validate_initial_view(
        cls,
        view: CompositeTerminalView | None,
        *,
        attachment_epoch: int,
        session_id: int,
        presentation_epoch: int,
    ) -> None:
        if view is None:
            return
        if not isinstance(view, CompositeTerminalView):
            raise TypeError("initial_view must be CompositeTerminalView or None")
        cls._validate_view(
            view,
            attachment_epoch=attachment_epoch,
            session_id=session_id,
            presentation_epoch=presentation_epoch,
        )

    @staticmethod
    def _validate_view(
        view: CompositeTerminalView,
        *,
        attachment_epoch: int,
        session_id: int,
        presentation_epoch: int,
    ) -> None:
        cell = view.cell
        if cell is None:
            raise TerminalUpdateError("composite view has no mandatory CELL plane")
        if not isinstance(cell, TerminalView):
            raise TypeError("composite CELL plane must be TerminalView")
        if view.presentation_epoch != presentation_epoch or (
            cell.presentation_epoch != presentation_epoch
        ):
            raise TerminalUpdateError(
                "view belongs to a foreign presentation_epoch"
            )
        if (
            cell.attachment_epoch != attachment_epoch
            or cell.session_id != session_id
        ):
            raise TerminalUpdateError("view belongs to a foreign session")
        if cell.revision > view.revision:
            raise TerminalUpdateError(
                "CELL plane revision is ahead of the composite revision"
            )
        if (cell.cols, cell.rows) != (view.geometry.cols, view.geometry.rows):
            raise TerminalUpdateError(
                "CELL plane does not match composite terminal geometry"
            )


__all__ = ["DisplayCadenceScheduler"]
