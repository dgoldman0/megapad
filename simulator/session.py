"""Semantic-simulator composition for shared terminal session ownership."""

from __future__ import annotations

import operator
import threading
import time
from dataclasses import dataclass

from rich_terminal import DriverServiceResult, DriverStatus
from session import MachineSession, RichTerminalSessionConfig
from shared_session import SharedMachine
from simulator.rich_terminal_host import (
    SemanticBatchResult,
    SemanticBatchStop,
    SimulatorSessionBackend,
)
from simulator.runtime import CreatedDefinition, MegaForthRuntime


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


class SimulatorSharedMachine(SharedMachine):
    """Expose one semantic session through the shared JSON-session authority.

    Presentation, display acknowledgement, input authorization, and terminal
    mutation are inherited unchanged. The runner and diagnostic surfaces stay
    explicit about semantic boundaries: they never manufacture instructions,
    cycles, cores, clocks, or devices for a runtime that has none.
    """

    def __init__(
        self,
        session: SimulatorMachineSession,
        *,
        idle_sleep_s: float = 0.002,
        host_profile: bool = False,
    ) -> None:
        if not isinstance(session, SimulatorMachineSession):
            raise TypeError("session must be a SimulatorMachineSession")
        if not isinstance(host_profile, bool):
            raise TypeError("host_profile must be a boolean")
        if host_profile:
            raise ValueError("host profiling is unavailable for semantic sessions")
        super().__init__(
            session,
            idle_tick_cycles=0,
            idle_sleep_s=idle_sleep_s,
            host_profile=False,
        )
        self.total_external_events = 0

    @property
    def semantic_session(self) -> SimulatorMachineSession:
        return self.session

    def start(self) -> None:
        """Boot the prepared root and start its semantic owner thread."""

        with self.lock:
            if self._thread is not None:
                return
            self.semantic_session.boot()
            self._reset_generation += 1
            self._thread = threading.Thread(
                target=self._run_loop,
                name="megapad-simulator-shared-machine",
                daemon=True,
            )
            self._thread.start()

    def _record_boundary_locked(self, result: SimulatorSessionRun) -> None:
        self.total_steps += result.semantic_steps
        self.total_batches += 1
        self.total_external_events += result.external_events_applied
        self.last_stop_reason = result.stop_reason.value

    def _terminal_failure_locked(self) -> str | None:
        failure = self.semantic_session.rich_terminal_failure
        if failure is not None:
            return failure
        if self.semantic_session.rich_terminal_lost:
            return "rich-terminal attachment lost"
        return None

    def _run_loop(self) -> None:
        while True:
            should_wait = False
            with self.condition:
                if self._stopping:
                    return
                if self.paused:
                    self.condition.wait(timeout=0.1)
                    continue

                failure = self._terminal_failure_locked()
                if failure is not None:
                    self.last_error = f"TerminalSessionError: {failure}"
                    self.paused = True
                    continue

                session = self.semantic_session
                if (session.halted or session.idle) and not (
                    session.rich_terminal_work_pending
                ):
                    should_wait = True
                else:
                    try:
                        result = session.run_boundary()
                        self._record_boundary_locked(result)
                        failure = self._terminal_failure_locked()
                        if failure is not None:
                            self.last_error = f"TerminalSessionError: {failure}"
                            self.paused = True
                        elif not session.last_batch_made_progress:
                            should_wait = True
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True

            if should_wait:
                with self.condition:
                    self.condition.wait(timeout=self.idle_sleep_s)
            else:
                time.sleep(0)

    def status(self, *, detailed: bool = True) -> dict:
        """Return backend-neutral terminal state plus semantic accounting."""

        with self.lock:
            session = self.semantic_session
            rich_terminal_failure = session.rich_terminal_failure
            rich_terminal_pending = session.rich_terminal_work_pending
            rich_terminal_driver = session.rich_terminal_driver
            rich_terminal_core = (
                None if rich_terminal_driver is None else rich_terminal_driver.core
            )
            operational = rich_terminal_failure is None
            halted = session.halted
            idle = session.idle and operational
            visible_cols, visible_rows = session.visible_geometry
            if session.rich_terminal_lost:
                state = "lost"
            elif rich_terminal_failure is not None:
                state = "terminal_failed"
            elif self.last_error:
                state = "error"
            elif self.paused:
                state = "paused"
            elif halted and not rich_terminal_pending:
                state = "halted"
            elif idle:
                state = "idle"
            elif self.last_stop_reason == SemanticBatchStop.HOST_BACKPRESSURE.value:
                state = "backpressured"
            else:
                state = "running"

            result = {
                "backend": "simulator",
                "generation": self._reset_generation,
                "state": state,
                "paused": self.paused,
                "halted": halted,
                "idle": idle,
                "stop_reason": self.last_stop_reason,
                "step_unit": "semantic_step",
                "steps": self.total_steps,
                "batch_unit": "semantic_boundary",
                "batches": self.total_batches,
                "external_events_applied": self.total_external_events,
                "revision": session.revision,
                "raw_bytes": session.raw_output_end,
                "raw_start": session.raw_output_start,
                "raw_offset": session.raw_output_end,
                "raw_retained_bytes": len(session.raw_output),
                "output_batches": session.output_batches,
                "byte_callbacks": session.output_byte_callbacks,
                "terminal": [visible_cols, visible_rows],
                "uptime_s": time.time() - self.started_at,
                "error": self.last_error,
                "rich_terminal": {
                    "enabled": session.rich_terminal_enabled,
                    "display_required": session.retained_display_required,
                    "state": (
                        None
                        if session.rich_terminal_state is None
                        else session.rich_terminal_state.value
                    ),
                    "pending": rich_terminal_pending,
                    "lost": session.rich_terminal_lost,
                    "failure": rich_terminal_failure,
                    "machine_publications": (
                        0
                        if rich_terminal_core is None
                        else rich_terminal_core.machine_publications_received
                    ),
                    "machine_publication_bytes": (
                        0
                        if rich_terminal_core is None
                        else rich_terminal_core.machine_publication_bytes_received
                    ),
                    "frames": (
                        0
                        if rich_terminal_core is None
                        else rich_terminal_core.frames_received
                    ),
                    "frame_bytes": (
                        0
                        if rich_terminal_core is None
                        else rich_terminal_core.frame_bytes_received
                    ),
                    "frames_by_type": (
                        {}
                        if rich_terminal_core is None
                        else {
                            f"0x{frame_type:04X}": count
                            for frame_type, count in sorted(
                                rich_terminal_core.frames_received_by_type.items()
                            )
                        }
                    ),
                    "frame_bytes_by_type": (
                        {}
                        if rich_terminal_core is None
                        else {
                            f"0x{frame_type:04X}": byte_count
                            for frame_type, byte_count in sorted(
                                rich_terminal_core.frames_received_by_type.items()
                            )
                        }
                    ),
                    "decoder_buffered_bytes": (
                        0
                        if rich_terminal_core is None
                        else rich_terminal_core.decoder_buffered_bytes
                    ),
                },
            }
            if detailed:
                result["simulator"] = {
                    "booted": session.booted,
                    "suspended": session.backend.suspended,
                    "semantic_steps": session.semantic_steps_total,
                    "semantic_boundaries": self.total_batches,
                    "external_events_applied": self.total_external_events,
                }
            return result

    def step(self, count: int = 1) -> dict:
        """Advance at most ``count`` semantic owner boundaries while paused."""

        count = int(count)
        if count <= 0 or count > 1_000_000:
            raise ValueError("step count must be between 1 and 1000000")
        with self.condition:
            if not self.paused:
                raise RuntimeError("machine must be paused before stepping")
            failure = self._terminal_failure_locked()
            if failure is not None:
                self.last_error = f"TerminalSessionError: {failure}"
                raise RuntimeError(
                    "rich terminal failure requires a machine reset: " + failure
                )

            boundaries = 0
            semantic_steps = 0
            external_events = 0
            stop_reason = self.last_stop_reason
            session = self.semantic_session
            for _ in range(count):
                if (session.halted or session.idle) and not (
                    session.rich_terminal_work_pending
                ):
                    stop_reason = (
                        SemanticBatchStop.COMPLETED.value
                        if session.halted
                        else SemanticBatchStop.IDLE.value
                    )
                    break
                try:
                    result = session.run_boundary()
                except Exception as exc:
                    self.last_error = f"{type(exc).__name__}: {exc}"
                    self.paused = True
                    raise
                self._record_boundary_locked(result)
                boundaries += 1
                semantic_steps += result.semantic_steps
                external_events += result.external_events_applied
                stop_reason = result.stop_reason.value
                failure = self._terminal_failure_locked()
                if failure is not None:
                    self.last_error = f"TerminalSessionError: {failure}"
                    self.paused = True
                    break
                if not session.last_batch_made_progress:
                    break

            self.last_stop_reason = stop_reason
            return {
                "boundaries": boundaries,
                "semantic_steps": semantic_steps,
                "external_events_applied": external_events,
                "stop_reason": stop_reason,
                "status": self.status(),
            }

    def reset(self, *, paused: bool | None = None) -> dict:
        """Reject reset until callers can supply a newly prepared runtime."""

        with self.condition:
            if paused is not None and not isinstance(paused, bool):
                raise TypeError("reset paused must be a boolean or null")
            message = (
                "semantic reset requires rebuilding the prepared runtime and session"
            )
            self.last_error = f"RuntimeError: {message}"
            self.paused = True
            self.condition.notify_all()
            raise RuntimeError(message)

    @staticmethod
    def _unsupported_diagnostic(name: str) -> RuntimeError:
        return RuntimeError(
            f"{name} is unavailable without emulator hardware state"
        )

    def network(self) -> dict:
        raise self._unsupported_diagnostic("network diagnostics")

    def forth(self, names: list[str]) -> dict:
        """Resolve newest hosted dictionary bindings without a CPU walk."""

        with self.lock:
            runtime = self.semantic_session.runtime
            found: dict[str, dict] = {}
            for requested in names:
                key = str(requested).upper()
                if key in found:
                    continue
                word = runtime.find(key)
                if word is None:
                    continue
                record = {
                    "name": word.name.decode("ascii", errors="replace"),
                    "header": word.header_address,
                    "code": word.xt,
                }
                if isinstance(word.implementation, CreatedDefinition):
                    record["data_address"] = word.body_address
                    record["value"] = runtime.memory.read64(word.body_address)
                found[key] = record
            return {"here": runtime.dictionary.here, "words": found}

    def peek(self, address: int, count: int = 1) -> dict:
        """Read ordinary semantic memory with the shared diagnostic shape."""

        address = int(address)
        count = int(count)
        if address < 0 or not (1 <= count <= 256):
            raise ValueError("peek requires a non-negative address and 1..256 cells")
        with self.lock:
            memory = self.semantic_session.runtime.memory
            return {
                "address": address,
                "cell_size": 8,
                "values": [
                    int(memory.read64(address + index * 8))
                    for index in range(count)
                ],
            }

    def start_phase_profile(
        self,
        address: int,
        max_events: int,
        *,
        generation: int,
    ) -> dict:
        raise self._unsupported_diagnostic("instruction phase profiling")

    def phase_profile(self) -> dict:
        raise self._unsupported_diagnostic("instruction phase profiling")

    def stop_phase_profile(self) -> dict:
        raise self._unsupported_diagnostic("instruction phase profiling")


__all__ = [
    "SimulatorMachineSession",
    "SimulatorSessionRun",
    "SimulatorSharedMachine",
]
