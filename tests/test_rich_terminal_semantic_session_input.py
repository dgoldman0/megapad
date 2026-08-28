"""Focused physical-proof tests for semantic control input at MachineSession."""

from __future__ import annotations

from dataclasses import replace
from types import MappingProxyType, SimpleNamespace

import pytest

from rich_terminal import DriverStatus
from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_scene import RetainedScene, SceneModelState
from rich_terminal.retained_view import DisplayScope
from rich_terminal.update_authority import TerminalGeometry
from session import MachineSession


ATTACHMENT_EPOCH = 7
SESSION_ID = 0x1020304050607080
PRESENTATION_EPOCH = 3
MODEL_REVISION = 23
GEOMETRY = TerminalGeometry(3, 2, 9)


class _RecordingDriver:
    def __init__(
        self,
        view: CompositeTerminalView,
        *,
        status: DriverStatus = DriverStatus.PROGRESS,
    ) -> None:
        self.core = SimpleNamespace(
            retained_configured=True,
            retained_enabled=True,
            output_view=view,
            model_revision=view.revision,
        )
        self.status = status
        self.control_events: list[tuple[int, int, int, int, int]] = []

    def send_control_event(
        self,
        owner_id: int,
        owner_generation: int,
        control_id: int,
        *,
        modifiers: int,
        model_revision: int,
    ) -> DriverStatus:
        self.control_events.append(
            (
                owner_id,
                owner_generation,
                control_id,
                modifiers,
                model_revision,
            )
        )
        return self.status


class _Cadence:
    def __init__(self, view: CompositeTerminalView) -> None:
        self.pending_revision: int | None = None
        self.offered_revision: int | None = None
        self.displayed_revision: int | None = view.revision
        self.presented = view

    def revoke_presented(self, view: CompositeTerminalView) -> None:
        assert view is self.presented
        self.presented = None
        self.displayed_revision = None
        self.pending_revision = view.revision


def _composite() -> CompositeTerminalView:
    row = (BLANK_CELL,) * GEOMETRY.cols
    cell = TerminalView(
        attachment_epoch=ATTACHMENT_EPOCH,
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        revision=19,
        cols=GEOMETRY.cols,
        rows=GEOMETRY.rows,
        cells=(row,) * GEOMETRY.rows,
        dirty_spans=(),
        cursor=Cursor(0, 0, True),
    )
    retained = SceneModelState(
        revision=21,
        geometry=GEOMETRY,
        active=RetainedScene(MappingProxyType({})),
        hidden=None,
        hidden_kind=None,
        requirement=None,
        retained_visible=True,
        retained_initialized=True,
    )
    return CompositeTerminalView(
        presentation_epoch=PRESENTATION_EPOCH,
        revision=MODEL_REVISION,
        geometry=GEOMETRY,
        cell=cell,
        retained=retained,
    )


def _scope(view: CompositeTerminalView) -> DisplayScope:
    assert view.cell is not None and view.retained is not None
    return DisplayScope(
        attachment_epoch=view.cell.attachment_epoch,
        session_id=view.cell.session_id,
        presentation_epoch=view.presentation_epoch,
        model_revision=view.revision,
        geometry_generation=view.geometry.generation,
        cell_revision=view.cell.revision,
        retained_revision=view.retained.revision,
    )


def _ready_session(
    *,
    driver_status: DriverStatus = DriverStatus.PROGRESS,
) -> tuple[MachineSession, _RecordingDriver, CompositeTerminalView, DisplayScope]:
    view = _composite()
    scope = _scope(view)
    driver = _RecordingDriver(view, status=driver_status)
    session = object.__new__(MachineSession)
    session._rich_terminal_config = SimpleNamespace(retained_policy=object())
    session._rich_terminal_driver = driver
    session._display_cadence = _Cadence(view)
    session._display_offer = None
    session._display_offer_composite = None
    session._displayed_composite_output = view
    session._logical_composite_output = view
    session._last_acknowledged_display_offer = (41, scope)
    session._display_cadence_scope = (
        scope.attachment_epoch,
        scope.session_id,
        scope.presentation_epoch,
    )
    session._rich_terminal_mutation_blocked = lambda: False
    session.revision = MODEL_REVISION + 10_000
    return session, driver, view, scope


def test_control_event_uses_only_the_exact_acknowledged_display_scope():
    session, driver, _, scope = _ready_session()

    assert session._acknowledged_output_scope() == scope
    assert session._output_revision_ready()
    assert (
        session.send_control_event(7, 2, 11, modifiers=0x15)
        is DriverStatus.PROGRESS
    )
    assert driver.control_events == [(7, 2, 11, 0x15, scope.model_revision)]
    assert scope.model_revision != session.revision
    with pytest.raises(TypeError, match="model_revision"):
        session.send_control_event(7, 2, 11, model_revision=session.revision)


def test_control_event_does_not_delegate_before_physical_ack():
    session, driver, view, _ = _ready_session()
    session._displayed_composite_output = None
    session._last_acknowledged_display_offer = None
    session._display_cadence.offered_revision = view.revision
    session._display_offer = object()
    session._display_offer_composite = view

    assert (
        session.send_control_event(7, 2, 11)
        is DriverStatus.BACKPRESSURED
    )
    assert driver.control_events == []


def test_control_event_does_not_delegate_while_newer_output_is_pending():
    session, driver, view, _ = _ready_session()
    newer = replace(view, revision=view.revision + 1)
    session._logical_composite_output = newer
    driver.core.output_view = newer
    driver.core.model_revision = newer.revision
    session._display_cadence.pending_revision = newer.revision

    assert (
        session.send_control_event(7, 2, 11)
        is DriverStatus.BACKPRESSURED
    )
    assert driver.control_events == []


def test_control_event_does_not_delegate_after_physical_display_revocation():
    session, driver, _, _ = _ready_session()

    assert session.revoke_physical_display()
    assert (
        session.send_control_event(7, 2, 11)
        is DriverStatus.BACKPRESSURED
    )
    assert driver.control_events == []


def test_control_event_preserves_mutation_driver_and_delegate_statuses():
    blocked, blocked_driver, _, _ = _ready_session()
    blocked._rich_terminal_mutation_blocked = lambda: True
    assert blocked.send_control_event(7, 2, 11) is DriverStatus.FAILED
    assert blocked_driver.control_events == []

    absent, _, _, _ = _ready_session()
    absent._rich_terminal_driver = None
    assert absent.send_control_event(7, 2, 11) is DriverStatus.INVALID

    blocked_absent, _, _, _ = _ready_session()
    blocked_absent._rich_terminal_driver = None
    blocked_absent._rich_terminal_mutation_blocked = lambda: True
    assert blocked_absent.send_control_event(7, 2, 11) is DriverStatus.FAILED

    delegated, delegated_driver, _, scope = _ready_session(
        driver_status=DriverStatus.STALE
    )
    assert delegated.send_control_event(7, 2, 11) is DriverStatus.STALE
    assert delegated_driver.control_events == [(7, 2, 11, 0, scope.model_revision)]


def test_output_ready_wrapper_preserves_nonretained_compatibility():
    session = object.__new__(MachineSession)
    session._rich_terminal_config = None

    assert session._acknowledged_output_scope() is None
    assert session._output_revision_ready()

    session._rich_terminal_config = SimpleNamespace(retained_policy=None)
    assert session._acknowledged_output_scope() is None
    assert session._output_revision_ready()
