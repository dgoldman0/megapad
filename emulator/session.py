"""Architectural MP64 construction and synchronous session execution."""

from __future__ import annotations

import math
import operator
import os
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Literal

from asm import assemble
from display import VirtualTerminal
from emulator.devices import RTC
from rich_terminal import DriverStatus, TerminalSessionError
from rich_terminal.display_cadence import DisplayCadenceScheduler
# Keep the temporary flat session alias compatible while benchmark callers
# migrate. Frontend consumers import these values from shared.session directly.
from shared.session import (
    OutputSnapshotRows,
    RichTerminalSessionConfig,
    RichTerminalSessionPolicy,
    TerminalCell,
    TerminalDisplayOffer,
    TerminalSession,
    TerminalSnapshot,
)

if TYPE_CHECKING:
    from nic_backends import NICBackend
    from emulator.system import MegapadSystem, SystemRunStats


_BIOS_CACHE: dict[tuple[str, int, int], tuple[bytes, dict[str, int]]] = {}
_ACCEL_HOOKS = (
    ("w_rect_fill", 1, 53),
    ("w_blit_glyph", 2, 79),
    ("w_vram_copy", 3, 131),
    ("w_blit_string", 4, 175),
)
_IDLE_OWNER_YIELD_SECONDS = 0.001


@dataclass(frozen=True)
class RunReport:
    reason: str
    steps: int
    batches: int
    elapsed_s: float
    output_bytes: int
    matched: bool = False

    def to_dict(self) -> dict:
        return asdict(self)


class MachineSession(TerminalSession):
    """Own one architectural machine and its common terminal frontend."""

    def __init__(
        self,
        system: MegapadSystem,
        *,
        cols: int = 80,
        rows: int = 30,
        batch_steps: int = 100_000,
        rich_terminal: RichTerminalSessionConfig | None = None,
    ):
        if batch_steps <= 0:
            raise ValueError("batch_steps must be positive")
        self.system = system
        self.batch_steps = int(batch_steps)
        super().__init__(cols, rows, rich_terminal)
        self.bios_labels: dict[str, int] = {}
        self._old_on_tx = self.system.uart.on_tx
        self._old_on_tx_batch = self.system.uart.on_tx_batch
        self.system.uart.on_tx = self._receive_byte
        self.system.uart.on_tx_batch = self._receive_batch
        try:
            if rich_terminal is None:
                self.resize(cols, rows)
            else:
                self._attach_rich_terminal()
        except BaseException:
            if self._rich_terminal_driver is not None:
                self._rich_terminal_driver.close()
                self._rich_terminal_driver = None
            self.system.uart.on_tx = self._old_on_tx
            self.system.uart.on_tx_batch = self._old_on_tx_batch
            raise


    @classmethod
    def from_bios(
        cls,
        bios_path: str | os.PathLike,
        *,
        storage_image: str | os.PathLike | None = None,
        ram_size: int = 1 << 20,
        ext_mem_size: int = 128 << 20,
        vram_size: int = 4 << 20,
        num_cores: int = 1,
        num_clusters: int = 0,
        lanes: int | None = None,
        cols: int = 80,
        rows: int = 30,
        batch_steps: int = 100_000,
        rich_terminal: RichTerminalSessionConfig | None = None,
        nic_backend: NICBackend | None = None,
        realtime_clock: bool = False,
        rtc_epoch_ms: int | None = None,
    ) -> "MachineSession":
        """A session on a new machine running the BIOS at BIOS_PATH.

        The machine's clock starts at RTC_EPOCH_MS, milliseconds since the
        Unix epoch, when one is given; otherwise at the host's time for a
        real-time clock, or at zero.
        """

        from emulator.system import MegapadSystem

        code, labels = _load_bios(Path(bios_path))
        system = MegapadSystem(
            ram_size=ram_size,
            storage_image=str(storage_image) if storage_image else None,
            ext_mem_size=ext_mem_size,
            vram_size=vram_size,
            num_cores=num_cores,
            num_clusters=num_clusters,
            worker_count=lanes,
            nic_backend=nic_backend,
            realtime_clock=realtime_clock,
            rtc_epoch_ms=rtc_epoch_ms,
        )
        system.load_binary(0, code)
        for name, hook_id, code_size in _ACCEL_HOOKS:
            if name in labels:
                system.cpu.register_accel_hook(
                    labels[name],
                    hook_id,
                    code_size,
                )
        session = cls(
            system,
            cols=cols,
            rows=rows,
            batch_steps=batch_steps,
            rich_terminal=rich_terminal,
        )
        session.bios_labels = dict(labels)
        return session


    def _terminal_attachment_target(self):
        """Return the backend exposing the shared rich-terminal host port."""

        return self.system


    def _terminal_host_state(self):
        """Return the backend-neutral host-port state used for liveness."""

        return self.system.rich_terminal_host


    def _inject_legacy_terminal_input(self, data: bytes) -> None:
        """Inject bytes while no enhanced terminal owns the stream."""

        self.system.uart.inject_input(data)


    def _set_legacy_terminal_geometry(self, cols: int, rows: int) -> None:
        """Commit geometry while the ANSI frontend owns the stream."""

        self.system.uart_geom.host_set_size(cols, rows)


    def close(self):
        if self._closed:
            return
        try:
            self._close_terminal_frontend()
            self.system.storage.save_image()
        finally:
            self.system.uart.on_tx = self._old_on_tx
            self.system.uart.on_tx_batch = self._old_on_tx_batch
            try:
                self.system.audio.release_host_sink()
            finally:
                self.system.nic.stop()
                self._closed = True


    def boot(self, entry: int = 0):
        reattach = self.rich_terminal_enabled and self.system._booted
        try:
            if reattach:
                self._close_rich_terminal()
                self._output_view = None
                self._logical_composite_output = None
                self._displayed_composite_output = None
                self._clear_display_offer_tokens()
                self._display_cadence_scope = None
                config = self._rich_terminal_config
                self._display_cadence = (
                    None
                    if config is None or config.retained_policy is None
                    else DisplayCadenceScheduler(
                        policy=config.retained_policy
                    )
                )
                if self._output_view_selected:
                    self.revision += 1
                self._output_view_selected = False
            self.system.boot(entry)
            if reattach:
                self._attach_rich_terminal()
        except BaseException as exc:
            if self.rich_terminal_enabled:
                self._record_rich_terminal_failure(
                    f"rich-terminal boot failed: {type(exc).__name__}: {exc}",
                    lost=self._rich_terminal_driver is None,
                )
            raise


    def reset(self, entry: int = 0, *, clear_terminal: bool = True):
        """Reset the owned machine and optionally clear captured terminal state."""
        try:
            self._close_rich_terminal()
            self.raw_output.clear()
            self._raw_output_start = self._raw_output_total
            self.output_batches = 0
            self.output_byte_callbacks = 0
            self._output_view = None
            self._output_view_selected = False
            self._logical_composite_output = None
            self._displayed_composite_output = None
            self._clear_display_offer_tokens()
            self._display_cadence_scope = None
            self._display_cadence = (
                None
                if self._rich_terminal_config is None
                or self._rich_terminal_config.retained_policy is None
                else DisplayCadenceScheduler(
                    policy=self._rich_terminal_config.retained_policy
                )
            )
            self._last_cadence_service_progress = False
            self._rich_terminal_failure_reason = None
            self._rich_terminal_lost = False
            self._last_batch_rich_terminal_progress = False
            if clear_terminal:
                cols, rows = self.terminal.cols, self.terminal.rows
                self.terminal = VirtualTerminal(
                    cols=cols,
                    rows=rows,
                    uart_inject=self._inject_terminal_response,
                )
                if not self.rich_terminal_enabled:
                    self.system.uart_geom.host_set_size(cols, rows)
            self.revision += 1
            self.system.boot(entry, discard_uart_output=True)
            if self.rich_terminal_enabled:
                self._attach_rich_terminal()
        except BaseException as exc:
            if self.rich_terminal_enabled:
                self._record_rich_terminal_failure(
                    f"rich-terminal reset failed: {type(exc).__name__}: {exc}",
                    lost=self._rich_terminal_driver is None,
                )
            raise


    def run_batch_stats(self, steps: int | None = None) -> SystemRunStats:
        """Run one session-owned driver/machine/driver alternation."""

        count = self.batch_steps if steps is None else operator.index(steps)
        if count <= 0:
            raise ValueError("steps must be positive")
        before = self.service_rich_terminal()
        cadence_before = self._last_cadence_service_progress
        stats = self.system.run_batch_stats(count)
        after = self.service_rich_terminal()
        cadence_after = self._last_cadence_service_progress
        self._last_batch_rich_terminal_progress = bool(
            stats.external_events_applied
            or cadence_before
            or cadence_after
            or (
                before is not None
                and before.status is DriverStatus.PROGRESS
            )
            or (
                after is not None
                and after.status is DriverStatus.PROGRESS
            )
        )
        if stats.system_stop_reason == "terminal_failure":
            reason = self._terminal_host_state().failure_reason
            self._latch_rich_terminal_failure(reason or "rich-terminal host failed")
        self._refresh_output_display_boundary()
        return stats


    def run(
        self,
        *,
        max_steps: int = 10_000_000,
        wall_timeout_s: float = 10.0,
        until_text: str | None = None,
        text_scope: Literal["raw", "screen"] = "raw",
        advance_idle: bool = False,
        idle_tick_cycles: int = 10_000,
    ) -> RunReport:
        if max_steps < 0:
            raise ValueError("max_steps cannot be negative")
        if wall_timeout_s <= 0:
            raise ValueError("wall_timeout_s must be positive")
        if text_scope not in ("raw", "screen"):
            raise ValueError("text_scope must be 'raw' or 'screen'")
        if idle_tick_cycles <= 0:
            raise ValueError("idle_tick_cycles must be positive")
        start = time.perf_counter()
        deadline = start + wall_timeout_s
        output_start = self._raw_output_total
        steps = 0
        batches = 0
        matched = False
        reason = "step_budget"

        def has_match() -> bool:
            if until_text is None:
                return False
            haystack = self.raw_text() if text_scope == "raw" else self.screen_text()
            return until_text in haystack

        def advance_idle_devices() -> None:
            # Jump straight to a sleeping core's next timed wake, if any.
            timed_wake = self.system.idle_wake_delay_s()
            cycles = idle_tick_cycles
            if timed_wake is not None:
                cycles = max(cycles, math.ceil(timed_wake * RTC.CLOCK_HZ))
            self.system.bus.tick(cycles)
            self.system.wake_idle_cores()

        while steps < max_steps:
            if has_match():
                matched = True
                reason = "matched"
                break
            if self.rich_terminal_failure is not None:
                reason = "terminal_failure"
                break
            transport_pending = self._rich_terminal_transport_has_pending_work()
            cadence_pending = self._display_cadence_has_pending_work()
            if self.system.all_halted and not transport_pending and not cadence_pending:
                reason = "halted"
                break
            if time.perf_counter() >= deadline:
                reason = "wall_timeout"
                break
            owner_quiescent = self.system.all_halted or (
                self.system.all_idle_or_halted
                and not self.system.uart.has_rx_data
            )
            if owner_quiescent and not transport_pending and cadence_pending:
                if self._service_display_cadence():
                    continue
                if advance_idle and not self.system.all_halted:
                    advance_idle_devices()
                    if not self.system.all_idle_or_halted:
                        continue
                remaining = deadline - time.perf_counter()
                if remaining > 0:
                    time.sleep(min(_IDLE_OWNER_YIELD_SECONDS, remaining))
                continue
            if (
                self.system.all_idle_or_halted
                and not self.system.uart.has_rx_data
                and not transport_pending
            ):
                if not advance_idle:
                    reason = "idle"
                    break
                advance_idle_devices()
                if self.system.all_idle_or_halted:
                    time.sleep(_IDLE_OWNER_YIELD_SECONDS)
                continue
            count = min(self.batch_steps, max_steps - steps)
            if self._rich_terminal_driver is None:
                executed = self.system.run_batch(count)
                rich_terminal_progress = False
                stop_reason = ""
            else:
                try:
                    stats = self.run_batch_stats(count)
                except TerminalSessionError:
                    reason = "terminal_failure"
                    break
                executed = stats.instructions_executed
                rich_terminal_progress = self._last_batch_rich_terminal_progress
                stop_reason = stats.system_stop_reason
            batches += 1
            cadence_wait_boundary = (
                self._display_cadence_has_pending_work()
                and (
                    self.system.all_halted
                    or (
                        self.system.all_idle_or_halted
                        and not self.system.uart.has_rx_data
                    )
                )
            )
            if executed <= 0 and not (
                rich_terminal_progress
                or self._rich_terminal_transport_has_pending_work()
                or cadence_wait_boundary
                or stop_reason == "all_idle"
            ):
                reason = "stalled"
                break
            steps += executed

        if not matched and has_match():
            matched = True
            reason = "matched"
        elapsed = time.perf_counter() - start
        return RunReport(
            reason=reason,
            steps=steps,
            batches=batches,
            elapsed_s=elapsed,
            output_bytes=self._raw_output_total - output_start,
            matched=matched,
        )


    def wait_for_idle(
        self,
        *,
        max_steps: int = 10_000_000,
        wall_timeout_s: float = 10.0,
    ) -> RunReport:
        return self.run(max_steps=max_steps, wall_timeout_s=wall_timeout_s)


    def wait_for_text(
        self,
        text: str,
        *,
        scope: Literal["raw", "screen"] = "raw",
        max_steps: int = 10_000_000,
        wall_timeout_s: float = 10.0,
    ) -> RunReport:
        return self.run(
            max_steps=max_steps,
            wall_timeout_s=wall_timeout_s,
            until_text=text,
            text_scope=scope,
            advance_idle=True,
        )


    def step(self) -> int:
        if not self.rich_terminal_enabled:
            return self.system.step()
        self.service_rich_terminal()
        cycles = self.system.step()
        self.service_rich_terminal()
        self._refresh_output_display_boundary()
        return cycles



def _load_bios(path: Path) -> tuple[bytes, dict[str, int]]:
    path = path.expanduser().resolve()
    stat = path.stat()
    key = (str(path), stat.st_mtime_ns, stat.st_size)
    cached = _BIOS_CACHE.get(key)
    if cached is not None:
        code, labels = cached
        return code, dict(labels)

    labels: dict[str, int] = {}
    if path.suffix.lower() == ".asm":
        code = bytes(assemble(path.read_text(encoding="utf-8"), labels_out=labels))
    else:
        code = path.read_bytes()
    _BIOS_CACHE.clear()
    _BIOS_CACHE[key] = (code, dict(labels))
    return code, labels
