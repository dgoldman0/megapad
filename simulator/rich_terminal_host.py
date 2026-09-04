"""Hosted semantic-batch adapter for the shared rich-terminal host port.

The adapter owns the simulator effects that cannot live in shared policy:
scheduler exclusion, UART settlement, terminal geometry, and resumable semantic
dispatch.  ``TX-FLUSH`` remains a source-visible no-op; one outer semantic call
contributes at most one publication when it returns or blocks. Bytes completed
before backend ownership remain a distinct earlier boundary. Public runtime
execution and UART mutation are rejected outside the backend's guest call.
"""

from __future__ import annotations

import operator
import threading
import weakref
from contextlib import contextmanager
from dataclasses import dataclass
from enum import Enum
from typing import Callable, Iterator

from rich_terminal.transport import HostPortLimits, TerminalHostLease
from shared.rich_terminal_host import RichTerminalHostHooks, SharedRichTerminalHost
from simulator.errors import ExecutionError
from simulator.runtime import (
    BlockedExecution,
    ExecutionContext,
    IdleWake,
    MegaForthRuntime,
)


def _dimension(name: str, value: int) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not 1 <= result <= (1 << 16) - 1:
        raise ValueError(f"{name} must be between 1 and 65535")
    return int(result)


@dataclass(frozen=True, slots=True)
class HostedTerminalGeometry:
    """Read-only snapshot of adapter-local terminal geometry."""

    cols: int
    rows: int
    resized: bool


class _HostedTerminalGeometryState:
    """Boundary-owned dimensions and sticky resize observation."""

    __slots__ = ("cols", "resized", "rows")

    def __init__(self, cols: int = 80, rows: int = 24) -> None:
        self.cols = _dimension("terminal columns", cols)
        self.rows = _dimension("terminal rows", rows)
        self.resized = False

    def apply(self, cols: int, rows: int) -> None:
        normalized_cols = _dimension("terminal columns", cols)
        normalized_rows = _dimension("terminal rows", rows)
        self.cols = normalized_cols
        self.rows = normalized_rows
        self.resized = True

    def snapshot(self) -> HostedTerminalGeometry:
        return HostedTerminalGeometry(self.cols, self.rows, self.resized)


class SemanticBatchStop(str, Enum):
    """Reason one simulator-owned semantic batch returned to its caller."""

    COMPLETED = "completed"
    IDLE = "idle"
    HOST_BACKPRESSURE = "host_backpressure"
    TERMINAL_FAILURE = "terminal_failure"


@dataclass(frozen=True, slots=True)
class SemanticBatchResult:
    """Per-boundary semantic work and host-event accounting."""

    semantic_steps: int
    stop_reason: SemanticBatchStop
    external_events_applied: int = 0


class _SimulatorRichTerminalHooks(RichTerminalHostHooks):
    """Keep shared-policy capabilities private to one simulator backend."""

    def __init__(self, backend: SimulatorSessionBackend) -> None:
        self._backend_ref = weakref.ref(backend)

    def _backend(self) -> SimulatorSessionBackend:
        backend = self._backend_ref()
        if backend is None:
            raise RuntimeError("the owning simulator session no longer exists")
        return backend

    @contextmanager
    def scheduler_boundary(self) -> Iterator[None]:
        with self._backend()._scheduler_boundary():
            yield

    def reject_batch_reentry_locked(self) -> None:
        self._backend()._reject_batch_reentry_locked()

    def drain_completed_egress_locked(self) -> None:
        self._backend()._drain_completed_egress_locked()

    def require_attach_ready_locked(self) -> None:
        self._backend()._require_attach_ready_locked()

    def bind_machine_sink_locked(
        self,
        sink: SharedRichTerminalHost | None,
    ) -> None:
        self._backend()._bind_machine_sink_locked(sink)

    def discard_machine_egress_locked(self) -> None:
        self._backend()._discard_machine_egress_locked()

    def apply_uart_ingress_locked(self, epoch: int, payload: bytes) -> None:
        self._backend()._apply_uart_ingress_locked(epoch, payload)

    def apply_geometry_locked(self, epoch: int, cols: int, rows: int) -> None:
        self._backend()._apply_geometry_locked(epoch, cols, rows)

    def pending_uart_ingress_bytes_locked(self) -> int:
        return self._backend()._pending_uart_ingress_bytes_locked()

    def discard_uart_ingress_tail_locked(self, count: int) -> None:
        self._backend()._discard_uart_ingress_tail_locked(count)


class SimulatorSessionBackend:
    """Own one hosted runtime's deterministic terminal/session boundary."""

    def __init__(
        self,
        runtime: MegaForthRuntime,
        *,
        legacy_output_sink: Callable[[bytes], None],
        terminal_cols: int = 80,
        terminal_rows: int = 24,
    ) -> None:
        if not isinstance(runtime, MegaForthRuntime):
            raise TypeError("runtime must be a MegaForthRuntime")
        if not callable(legacy_output_sink):
            raise TypeError("legacy_output_sink must be callable")
        self._runtime = runtime
        self._legacy_output_sink = legacy_output_sink
        self._geometry = _HostedTerminalGeometryState(
            terminal_cols,
            terminal_rows,
        )
        self._boundary_lock = threading.RLock()
        self._guest_call_active = False
        self._legacy_callback_active = False
        self._owner_token = object()
        self._closed = False
        self._machine_sink: SharedRichTerminalHost | None = None
        self._suspension: BlockedExecution | None = None
        self._reported_suspension_steps = 0
        hooks = _SimulatorRichTerminalHooks(self)
        self._rich_terminal_hooks = hooks
        self._rich_terminal_host = SharedRichTerminalHost(hooks)
        self._runtime._claim_session_owner(self._owner_token)

    @property
    def runtime(self) -> MegaForthRuntime:
        return self._runtime

    @property
    def geometry(self) -> HostedTerminalGeometry:
        with self._boundary_lock:
            return self._geometry.snapshot()

    @property
    def rich_terminal_host(self) -> SharedRichTerminalHost:
        return self._rich_terminal_host

    @property
    def suspended(self) -> bool:
        with self._boundary_lock:
            return self._suspension is not None

    @property
    def closed(self) -> bool:
        with self._boundary_lock:
            return self._closed

    def attach_rich_terminal(
        self,
        limits: HostPortLimits,
    ) -> TerminalHostLease:
        """Acquire the shared enhanced-primary port for this runtime."""

        with self._boundary_lock:
            self._require_open()
            return self._rich_terminal_host.attach(limits)

    def close(self) -> None:
        """Release exclusive runtime ownership after its terminal lease."""

        with self._boundary_lock:
            if self._closed:
                return
            self._reject_batch_reentry_locked()
            if self._rich_terminal_host.enhanced_attached:
                raise RuntimeError(
                    "close the enhanced terminal lease before its backend"
                )
            if self._suspension is not None:
                self.cancel_suspension()
            self._runtime._release_session_owner(self._owner_token)
            self._closed = True

    def cancel_suspension(self) -> bool:
        """Cancel the backend-owned IDL continuation, if one exists."""

        with self._boundary_lock:
            self._require_open()
            self._reject_batch_reentry_locked()
            suspended = self._suspension
            if suspended is None:
                return False
            try:
                with self._runtime._session_owner_scope(self._owner_token):
                    self._runtime.cancel_suspension(suspended.suspension)
            finally:
                self._suspension = None
                self._reported_suspension_steps = 0
            return True

    def inject_legacy_uart_input(self, payload: bytes) -> None:
        """Publish legacy input only while no enhanced primary owns the UART."""

        with self._boundary_lock:
            self._require_open()
            self._reject_batch_reentry_locked()
            if self._rich_terminal_host.enhanced_attached:
                raise RuntimeError(
                    "the enhanced terminal lease owns terminal ingress"
                )
            self._runtime._session_inject_uart_input(self._owner_token, payload)

    def set_legacy_geometry(self, cols: int, rows: int) -> None:
        """Apply legacy host geometry only while the enhanced port is idle."""

        with self._boundary_lock:
            self._require_open()
            self._reject_batch_reentry_locked()
            if self._rich_terminal_host.enhanced_attached:
                raise RuntimeError(
                    "the enhanced terminal lease owns terminal geometry"
                )
            self._geometry.apply(cols, rows)

    def run_semantic_batch(
        self,
        entry: bytes | str | int | None = None,
        *,
        context: ExecutionContext | None = None,
        step_budget: int | None = None,
    ) -> SemanticBatchResult:
        """Run or resume one outer semantic dispatch and settle its UART bytes.

        A fresh dispatch requires ``entry``. Once that dispatch blocks at IDL,
        the backend owns its opaque suspension and later calls omit all entry
        arguments. UART availability after event admission supplies the exact
        interrupt wake; geometry alone leaves the dispatch idle.
        """

        with self._boundary_lock:
            self._require_open()
            self._reject_batch_reentry_locked()
            suspended = self._suspension
            if suspended is None:
                if entry is None:
                    raise ValueError("entry is required for a fresh semantic batch")
            elif entry is not None or context is not None or step_budget is not None:
                raise ValueError(
                    "entry, context, and step_budget must be omitted while suspended"
                )

            return self._run_semantic_batch_owned(
                entry,
                context=context,
                step_budget=step_budget,
                suspended=suspended,
            )

    def _run_semantic_batch_owned(
        self,
        entry: bytes | str | int | None,
        *,
        context: ExecutionContext | None,
        step_budget: int | None,
        suspended: BlockedExecution | None,
    ) -> SemanticBatchResult:
        """Run below the scheduler and runtime-ownership boundaries."""

        self._guest_call_active = True
        guest_started = False
        result: SemanticBatchResult
        try:
            admission = self._rich_terminal_host._service_before_guest_locked()
            if not admission.can_execute:
                result = self._host_stop_result(
                    admission.external_events_applied
                )
            else:
                # Output completed before this call belongs to an earlier
                # boundary and cannot be merged into the guest publication.
                self._drain_completed_egress_locked()
                if self._rich_terminal_host.runner_backpressured:
                    result = self._host_stop_result(
                        admission.external_events_applied
                    )
                elif (
                    suspended is not None
                    and not self._runtime.uart_input_available
                ):
                    result = SemanticBatchResult(
                        0,
                        SemanticBatchStop.IDLE,
                        admission.external_events_applied,
                    )
                else:
                    guest_started = True
                    if suspended is None:
                        with self._runtime._session_owner_scope(
                            self._owner_token
                        ):
                            execution = self._runtime.run_until_blocked(
                                entry,
                                context=context,
                                step_budget=step_budget,
                            )
                        prior_steps = 0
                    else:
                        try:
                            with self._runtime._session_owner_scope(
                                self._owner_token
                            ):
                                wake = self._runtime.deliver_idle_wake(
                                    suspended.suspension,
                                    IdleWake.INTERRUPT,
                                )
                                execution = self._runtime.resume(
                                    suspended.suspension,
                                    wake,
                                )
                        except BaseException as error:
                            self._cancel_failed_resume(suspended, error)
                            raise
                        prior_steps = self._reported_suspension_steps

                    if execution.semantic_steps < prior_steps:
                        raise AssertionError(
                            "resumed semantic step count moved backwards"
                        )
                    semantic_steps = execution.semantic_steps - prior_steps
                    if isinstance(execution, BlockedExecution):
                        self._suspension = execution
                        self._reported_suspension_steps = execution.semantic_steps
                        stop_reason = SemanticBatchStop.IDLE
                    else:
                        self._suspension = None
                        self._reported_suspension_steps = 0
                        stop_reason = SemanticBatchStop.COMPLETED
                    result = SemanticBatchResult(
                        semantic_steps,
                        stop_reason,
                        admission.external_events_applied,
                    )
        finally:
            try:
                if guest_started:
                    self._drain_completed_egress_locked()
            finally:
                self._guest_call_active = False

        if self._rich_terminal_host.failure_reason is not None:
            return SemanticBatchResult(
                result.semantic_steps,
                SemanticBatchStop.TERMINAL_FAILURE,
                result.external_events_applied,
            )
        return result

    def _cancel_failed_resume(
        self,
        suspended: BlockedExecution,
        original_error: BaseException,
    ) -> None:
        """Release a continuation still owned after a failed resume."""

        try:
            with self._runtime._session_owner_scope(self._owner_token):
                self._runtime.cancel_suspension(suspended.suspension)
        except ExecutionError:
            # Resume clears the old handle before re-entering guest code. A
            # guest fault after that point therefore makes cancellation stale.
            pass
        except BaseException as cancel_error:
            original_error.add_note(
                "failed to cancel simulator suspension after resume error: "
                f"{type(cancel_error).__name__}: {cancel_error}"
            )
        finally:
            self._suspension = None
            self._reported_suspension_steps = 0

    def _host_stop_result(self, external_events_applied: int) -> SemanticBatchResult:
        reason = self._rich_terminal_host._runner_stop_reason()
        stop = (
            SemanticBatchStop.TERMINAL_FAILURE
            if reason == SemanticBatchStop.TERMINAL_FAILURE.value
            else SemanticBatchStop.HOST_BACKPRESSURE
        )
        return SemanticBatchResult(0, stop, external_events_applied)

    def _require_open(self) -> None:
        if self._closed:
            raise RuntimeError("the simulator session backend is closed")

    @contextmanager
    def _scheduler_boundary(self) -> Iterator[None]:
        with self._boundary_lock:
            yield

    def _reject_batch_reentry_locked(self) -> None:
        if self._guest_call_active:
            raise RuntimeError("a hosted semantic batch is already active")
        if self._legacy_callback_active:
            raise RuntimeError(
                "a legacy output callback cannot reenter the session backend"
            )

    def _drain_completed_egress_locked(self) -> None:
        payload = self._runtime._session_drain_uart_output(self._owner_token)
        if not payload:
            return
        sink = self._machine_sink
        if sink is not None:
            if not sink._publish_machine_egress(payload):
                raise RuntimeError(
                    "the bound rich-terminal sink rejected owned output"
                )
            return
        self._legacy_callback_active = True
        try:
            self._legacy_output_sink(payload)
        finally:
            self._legacy_callback_active = False

    def _require_attach_ready_locked(self) -> None:
        if self._machine_sink is not None:
            raise RuntimeError("the simulator already has an enhanced primary sink")

    def _bind_machine_sink_locked(
        self,
        sink: SharedRichTerminalHost | None,
    ) -> None:
        if sink is not None and self._machine_sink is not None:
            raise RuntimeError("the simulator already has an enhanced primary sink")
        self._machine_sink = sink

    def _discard_machine_egress_locked(self) -> None:
        self._runtime._session_drain_uart_output(self._owner_token)

    def _apply_uart_ingress_locked(self, epoch: int, payload: bytes) -> None:
        if not self._rich_terminal_host._epoch_is_current(epoch):
            raise RuntimeError("rich-terminal UART ingress has a stale epoch")
        self._runtime._session_inject_uart_input(self._owner_token, payload)

    def _apply_geometry_locked(self, epoch: int, cols: int, rows: int) -> None:
        if not self._rich_terminal_host._epoch_is_current(epoch):
            raise RuntimeError("terminal geometry has a stale epoch")
        self._geometry.apply(cols, rows)

    def _pending_uart_ingress_bytes_locked(self) -> int:
        return self._runtime.uart_input_pending

    def _discard_uart_ingress_tail_locked(self, count: int) -> None:
        self._runtime._session_discard_uart_input_tail(
            self._owner_token,
            count,
        )


__all__ = [
    "HostedTerminalGeometry",
    "SemanticBatchResult",
    "SemanticBatchStop",
    "SimulatorSessionBackend",
]
