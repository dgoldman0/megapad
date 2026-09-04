"""Focused semantic-session coverage for the shared terminal frontend."""

from __future__ import annotations

from rich_terminal.driver import DriverLimits
from rich_terminal.server import TerminalConfig, TerminalState
from rich_terminal.transport import EgressWatermarks, HostPortLimits
from session import RichTerminalSessionConfig
from simulator.rich_terminal_host import SemanticBatchStop
from simulator.session import SimulatorMachineSession
from tests.simulator.test_kdos_exceptions import _load_exceptions
from tests.test_rich_terminal_dual_backend import (
    LIVE_HANDSHAKE_SCENARIO_SOURCE,
    ONE_CORE_UART_LOCK_SHIMS,
    RICH_TERMINAL_SOURCE,
    _stored_cell,
)


SESSION_ROOT_SOURCE = b"""
: IDLE  [ 0 C, ] ;
VARIABLE DBS-PUBLISHED
: DBS-RUN
  DBL-BOOT
  BEGIN
    DBL-SERVICE
    DBL-ACTIVE @ DBS-PUBLISHED @ 0= AND IF
      DBL-SNAPSHOT
      TRUE DBS-PUBLISHED !
    THEN
    IDLE
  AGAIN ;
"""


def _rich_terminal_config() -> RichTerminalSessionConfig:
    return RichTerminalSessionConfig(
        host_limits=HostPortLimits(
            egress=EgressWatermarks(
                high_bytes=8_192,
                low_bytes=1_024,
                high_batches=16,
                low_batches=2,
            ),
            retained_publication_bytes=4_608,
            ingress_bytes=8_192,
            ingress_events=16,
            ingress_control_bytes=4_096,
            ingress_control_events=8,
            geometry_events=2,
        ),
        terminal_config=TerminalConfig(
            max_payload=256,
            max_transaction_bytes=512,
            terminal_receive_credit=1_024,
            max_cells=4,
            max_feed_bytes=4_608,
            max_cols=4,
            max_rows=2,
            cols=2,
            rows=2,
        ),
        driver_limits=DriverLimits(4_096, 8),
        ansi_history_bytes=1_024,
        service_batches=1,
    )


def test_semantic_session_reuses_normal_frontend_through_first_cell_view() -> None:
    runtime = _load_exceptions()
    runtime.evaluate(
        ONE_CORE_UART_LOCK_SHIMS + RICH_TERMINAL_SOURCE.read_bytes(),
        source_name="one-core-uart-lock-shims+rich-terminal.f",
        step_budget=1_000_000,
    )
    runtime.evaluate(
        LIVE_HANDSHAKE_SCENARIO_SOURCE + SESSION_ROOT_SOURCE,
        source_name="simulator-session-cell-root.f",
    )

    with SimulatorMachineSession(
        runtime,
        "DBS-RUN",
        cols=2,
        rows=2,
        rich_terminal=_rich_terminal_config(),
    ) as session:
        session.boot()
        result = session.run_until_idle()

        assert result.stop_reason is SemanticBatchStop.IDLE
        assert result.semantic_steps > 0
        assert session.semantic_steps_total > 0
        assert session.idle
        assert not session.halted
        assert session.rich_terminal_state is TerminalState.ACTIVE
        assert session.rich_terminal_failure is None
        assert session.rich_terminal_driver is not None
        session_id = session.rich_terminal_driver.core.session_id
        assert session_id is not None and session_id != 0
        assert _stored_cell(runtime, "DBL-SESSION-ID") == session_id
        assert session.rich_terminal_driver.core.model_revision == 1
        assert not session.rich_terminal_work_pending

        snapshot = session.snapshot()
        assert session.revision == 1
        assert session.visible_geometry == (2, 2)
        assert snapshot.lines() == ["AB", "C "]
        assert (snapshot.cursor_row, snapshot.cursor_col) == (1, 1)
        assert snapshot.cursor_visible
        assert tuple(
            (cell.fg, cell.bg, cell.attrs)
            for row in snapshot.cells
            for cell in row
        ) == (
            ((170, 170, 170), (0, 0, 0), 1),
            ((0, 170, 0), (0, 0, 0), 8),
            ((0, 0, 170), (0, 0, 0), 0),
            ((170, 170, 170), (170, 0, 0), 32),
        )
        assert session.raw_text() == ""
