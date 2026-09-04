"""Semantic-simulator composition for the shared terminal session frontend."""

from __future__ import annotations

import operator
import time
from dataclasses import dataclass

from rich_terminal import DriverServiceResult, DriverStatus
from session import MachineSession, RichTerminalSessionConfig
from simulator.rich_terminal_host import (
    SemanticBatchResult,
    SemanticBatchStop,
    SimulatorSessionBackend,
)
from simulator.runtime import MegaForthRuntime


@dataclass(frozen=True, slots=True)
class SimulatorSessionRun:
    """One host-visible run-to-completion or run-to-IDL boundary."""

    semantic_steps: int
    external_events_applied: int
    stop_reason: SemanticBatchStop
    terminal_progress: bool


class SimulatorMachineSession(MachineSession):
    """Bind one hosted Forth runtime to the normal terminal session authority.

    The semantic runtime has no instruction/cycle batch. One owner call runs a
    fresh root entry to completion or its next ``IDL`` suspension, and later
    calls resume that same suspension only after admitted UART input. Terminal
    driver service surrounds that exact boundary, matching the architectural
    session without fabricating hardware statistics or a ``MegapadSystem``.
    """

    def __init__(
        self,
        runtime: MegaForthRuntime,
        entry: bytes | str | int,
        *,
        cols: int = 80,
        rows: int = 30,
        semantic_step_budget: int | None = None,
        rich_terminal: RichTerminalSessionConfig | None = None,
    ) -> None:
        if not isinstance(runtime, MegaForthRuntime):
            raise TypeError("runtime must be a MegaForthRuntime")
        if isinstance(entry, bool) or not isinstance(entry, (bytes, str, int)):
            raise TypeError("entry must be a word name or execution token")
        if semantic_step_budget is not None:
            if isinstance(semantic_step_budget, bool):
                raise TypeError("semantic_step_budget must be an integer or None")
            try:
                semantic_step_budget = operator.index(semantic_step_budget)
            except TypeError as exc:
                raise TypeError(
                    "semantic_step_budget must be an integer or None"
                ) from exc
            if semantic_step_budget <= 0:
                raise ValueError("semantic_step_budget must be positive")

        self.runtime = runtime
        self.entry = entry
        self.semantic_step_budget = semantic_step_budget
        self._backend: SimulatorSessionBackend | None = None
        self._booted = False
        self._dispatch_started = False
        self._halted = False
        self._semantic_steps_total = 0
        self._initialize_terminal_frontend(cols, rows, rich_terminal)

        backend = SimulatorSessionBackend(
            runtime,
            legacy_output_sink=self._receive_batch,
            terminal_cols=cols,
            terminal_rows=rows,
        )
        self._backend = backend
        try:
            if rich_terminal is None:
                self.resize(cols, rows)
            else:
                self._attach_rich_terminal()
        except BaseException:
            driver = self._rich_terminal_driver
            if driver is not None:
                driver.close()
                self._rich_terminal_driver = None
            backend.close()
            self._backend = None
            raise

    @property
    def backend(self) -> SimulatorSessionBackend:
        backend = self._backend
        if backend is None:
            raise RuntimeError("the simulator session is closed")
        return backend

    @property
    def booted(self) -> bool:
        return self._booted

    @property
    def halted(self) -> bool:
        return self._halted

    @property
    def idle(self) -> bool:
        backend = self.backend
        return bool(
            not self._halted
            and backend.suspended
            and not self.runtime.uart_input_available
            and not self.rich_terminal_work_pending
        )

    @property
    def semantic_steps_total(self) -> int:
        return self._semantic_steps_total

    def _terminal_attachment_target(self) -> SimulatorSessionBackend:
        return self.backend

    def _terminal_host_state(self):
        return self.backend.rich_terminal_host

    def _inject_legacy_terminal_input(self, data: bytes) -> None:
        self.backend.inject_legacy_uart_input(data)

    def _set_legacy_terminal_geometry(self, cols: int, rows: int) -> None:
        self.backend.set_legacy_geometry(cols, rows)

    def boot(self, entry: bytes | str | int | None = None) -> None:
        """Arm the already-prepared semantic runtime for its root dispatch."""

        if self._closed:
            raise RuntimeError("the simulator session is closed")
        if self._booted:
            if entry is not None and entry != self.entry:
                raise RuntimeError("the simulator root entry is already armed")
            return
        if entry is not None:
            if isinstance(entry, bool) or not isinstance(entry, (bytes, str, int)):
                raise TypeError("entry must be a word name or execution token")
            self.entry = entry
        self._booted = True

    def run_boundary(self) -> SimulatorSessionRun:
        """Service driver/guest/driver across one semantic owner boundary."""

        if self._closed:
            raise RuntimeError("the simulator session is closed")
        if not self._booted:
            raise RuntimeError("boot the simulator session before running it")

        before = self.service_rich_terminal()
        cadence_before = self._last_cadence_service_progress

        if self._halted:
            semantic = SemanticBatchResult(0, SemanticBatchStop.COMPLETED, 0)
        else:
            backend = self.backend
            if backend.suspended:
                semantic = backend.run_semantic_batch()
            elif not self._dispatch_started:
                semantic = backend.run_semantic_batch(
                    entry=self.entry,
                    step_budget=self.semantic_step_budget,
                )
                if semantic.stop_reason in {
                    SemanticBatchStop.COMPLETED,
                    SemanticBatchStop.IDLE,
                }:
                    self._dispatch_started = True
            else:
                raise RuntimeError(
                    "the semantic root dispatch is neither suspended nor halted"
                )

        self._semantic_steps_total += semantic.semantic_steps
        if semantic.stop_reason is SemanticBatchStop.COMPLETED:
            self._halted = True
        elif semantic.stop_reason is SemanticBatchStop.TERMINAL_FAILURE:
            reason = self._terminal_host_state().failure_reason
            self._latch_rich_terminal_failure(
                reason or "rich-terminal simulator host failed"
            )

        after = self.service_rich_terminal()
        cadence_after = self._last_cadence_service_progress
        terminal_progress = bool(
            semantic.external_events_applied
            or cadence_before
            or cadence_after
            or self._service_progress(before)
            or self._service_progress(after)
        )
        self._last_batch_rich_terminal_progress = bool(
            semantic.semantic_steps or terminal_progress
        )
        self._refresh_output_display_boundary()
        return SimulatorSessionRun(
            semantic_steps=semantic.semantic_steps,
            external_events_applied=semantic.external_events_applied,
            stop_reason=semantic.stop_reason,
            terminal_progress=terminal_progress,
        )

    @staticmethod
    def _service_progress(result: DriverServiceResult | None) -> bool:
        return result is not None and result.status is DriverStatus.PROGRESS

    def run_until_idle(
        self,
        *,
        wall_timeout_s: float = 10.0,
        max_boundaries: int = 100_000,
    ) -> SimulatorSessionRun:
        """Advance until the semantic root halts or waits without host work."""

        if wall_timeout_s <= 0:
            raise ValueError("wall_timeout_s must be positive")
        if max_boundaries <= 0:
            raise ValueError("max_boundaries must be positive")
        deadline = time.perf_counter() + wall_timeout_s
        last: SimulatorSessionRun | None = None
        for _ in range(max_boundaries):
            if time.perf_counter() >= deadline:
                raise TimeoutError("simulator session did not become idle")
            last = self.run_boundary()
            if self._halted or self.idle:
                return last
            if not self.last_batch_made_progress:
                return last
        raise RuntimeError("simulator session exceeded its boundary limit")

    def reset(self, *args, **kwargs):
        raise RuntimeError(
            "semantic reset requires rebuilding the prepared runtime and session"
        )

    def step(self) -> int:
        """Advance one semantic boundary and return semantic, not cycle, work."""

        return self.run_boundary().semantic_steps

    def close(self) -> None:
        if self._closed:
            return
        backend = self._backend
        try:
            driver = self._rich_terminal_driver
            if driver is not None:
                driver.close()
                self._rich_terminal_driver = None
            self._logical_composite_output = None
            self._displayed_composite_output = None
            self._clear_display_offer_tokens()
            self._display_cadence_scope = None
            self._display_cadence = None
        finally:
            try:
                if backend is not None:
                    backend.close()
            finally:
                self._closed = True


__all__ = ["SimulatorMachineSession", "SimulatorSessionRun"]
