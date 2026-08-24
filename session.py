"""Synchronous machine control and headless terminal capture for MegaPad."""

from __future__ import annotations

import json
import operator
import os
import time
from dataclasses import asdict, dataclass, replace
from pathlib import Path
from typing import TYPE_CHECKING, Literal

from asm import assemble
from display import VirtualTerminal
from presentation_terminal import (
    DriverLimits,
    DriverServiceResult,
    DriverStatus,
    EgressWatermarks,
    HostPortLimits,
    PresentationTerminalDriver,
    TerminalConfig,
    TerminalSessionError,
    TerminalState,
    TerminalView,
)
from presentation_terminal.apt1 import CONTROL_RESERVE_BYTES, snapshot_wire_bytes
from system import MegapadSystem, SystemRunStats

if TYPE_CHECKING:
    from nic_backends import NICBackend


_BIOS_CACHE: dict[tuple[str, int, int], tuple[bytes, dict[str, int]]] = {}
_ACCEL_HOOKS = (
    ("w_rect_fill", 1, 53),
    ("w_blit_glyph", 2, 79),
    ("w_vram_copy", 3, 131),
    ("w_blit_string", 4, 175),
)


@dataclass(frozen=True)
class TerminalCell:
    char: str
    fg: tuple[int, int, int]
    bg: tuple[int, int, int]
    attrs: int


@dataclass(frozen=True)
class TerminalSnapshot:
    cols: int
    rows: int
    cells: tuple[tuple[TerminalCell, ...], ...]
    cursor_col: int
    cursor_row: int
    cursor_visible: bool
    alternate_screen: bool

    def lines(self, trim_right: bool = False) -> list[str]:
        result = ["".join(cell.char for cell in row) for row in self.cells]
        if trim_right:
            result = [line.rstrip() for line in result]
        return result

    def text(self, trim_right: bool = False) -> str:
        return "\n".join(self.lines(trim_right=trim_right))

    def find(self, needle: str) -> list[tuple[int, int]]:
        hits: list[tuple[int, int]] = []
        for row, line in enumerate(self.lines()):
            start = 0
            while True:
                col = line.find(needle, start)
                if col < 0:
                    break
                hits.append((row, col))
                start = col + 1
        return hits

    def to_dict(self) -> dict:
        return {
            "cols": self.cols,
            "rows": self.rows,
            "cursor": {
                "col": self.cursor_col,
                "row": self.cursor_row,
                "visible": self.cursor_visible,
            },
            "alternate_screen": self.alternate_screen,
            "lines": self.lines(),
            "cells": [
                [
                    {
                        "char": cell.char,
                        "fg": list(cell.fg),
                        "bg": list(cell.bg),
                        "attrs": cell.attrs,
                    }
                    for cell in row
                ]
                for row in self.cells
            ],
        }

    def write_text(self, path: str | os.PathLike, trim_right: bool = True):
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(self.text(trim_right=trim_right) + "\n", encoding="utf-8")

    def write_json(self, path: str | os.PathLike):
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(
            json.dumps(self.to_dict(), indent=2, ensure_ascii=False) + "\n",
            encoding="utf-8",
        )

    def write_png(
        self,
        path: str | os.PathLike,
        *,
        font_path: str | os.PathLike | None = None,
        font_size: int = 16,
        padding: int = 6,
    ):
        """Render this immutable terminal state to a PNG using Pillow."""
        try:
            from PIL import Image, ImageDraw, ImageFont
        except ImportError as exc:
            raise RuntimeError("PNG capture requires Pillow") from exc

        selected_font = _resolve_font(font_path)
        if selected_font is not None:
            font = ImageFont.truetype(str(selected_font), font_size)
        else:
            font = ImageFont.load_default()

        bbox = font.getbbox("M")
        cell_w = max(1, int(round(font.getlength("M"))))
        cell_h = max(1, bbox[3] - bbox[1] + 4)
        image = Image.new(
            "RGB",
            (self.cols * cell_w + padding * 2,
             self.rows * cell_h + padding * 2),
            (0, 0, 0),
        )
        draw = ImageDraw.Draw(image)

        for row_index, row in enumerate(self.cells):
            for col_index, cell in enumerate(row):
                x = padding + col_index * cell_w
                y = padding + row_index * cell_h
                fg = cell.fg
                bg = cell.bg
                if cell.attrs & 32:
                    fg, bg = bg, fg
                if cell.attrs & 1:
                    fg = tuple(min(255, int(channel * 1.4)) for channel in fg)
                if cell.attrs & 2:
                    fg = tuple(channel // 2 for channel in fg)
                if bg != (0, 0, 0):
                    draw.rectangle((x, y, x + cell_w - 1, y + cell_h - 1), fill=bg)
                if cell.char and cell.char != " " and not (cell.attrs & 64):
                    draw.text((x, y - bbox[1] + 1), cell.char, font=font, fill=fg)
                if cell.attrs & 8:
                    draw.line((x, y + cell_h - 2, x + cell_w - 1, y + cell_h - 2), fill=fg)
                if cell.attrs & 128:
                    mid = y + cell_h // 2
                    draw.line((x, mid, x + cell_w - 1, mid), fill=fg)

        if self.cursor_visible:
            x = padding + self.cursor_col * cell_w
            y = padding + self.cursor_row * cell_h
            draw.rectangle((x, y + cell_h - 2, x + cell_w - 1, y + cell_h - 1), fill=(255, 255, 255))

        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        image.save(target, format="PNG")


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


@dataclass(frozen=True, slots=True)
class PresentationSessionConfig:
    """Caller-owned bounds for one optional presentation attachment."""

    host_limits: HostPortLimits
    terminal_config: TerminalConfig
    driver_limits: DriverLimits
    ansi_history_bytes: int
    service_batches: int = 1

    def __post_init__(self) -> None:
        if not isinstance(self.host_limits, HostPortLimits):
            raise TypeError("host_limits must be HostPortLimits")
        if not isinstance(self.terminal_config, TerminalConfig):
            raise TypeError("terminal_config must be TerminalConfig")
        if not isinstance(self.driver_limits, DriverLimits):
            raise TypeError("driver_limits must be DriverLimits")
        for name, value, minimum in (
            ("ansi_history_bytes", self.ansi_history_bytes, 0),
            ("service_batches", self.service_batches, 1),
        ):
            if isinstance(value, bool):
                raise TypeError(f"{name} must be an integer, not bool")
            try:
                normalized = operator.index(value)
            except TypeError as exc:
                raise TypeError(f"{name} must be an integer") from exc
            if normalized < minimum:
                raise ValueError(f"{name} must be at least {minimum}")
            if normalized > (1 << 64) - 1:
                raise ValueError(f"{name} must fit uint64")
            object.__setattr__(self, name, int(normalized))


@dataclass(frozen=True, slots=True)
class PresentationSessionPolicy:
    """Reusable product bounds for explicitly enabled presentation sessions."""

    max_cols: int
    max_rows: int
    egress_high_publications: int
    egress_high_batches: int
    egress_low_batches: int
    ingress_bytes: int
    ingress_events: int
    ingress_control_bytes: int
    ingress_control_events: int
    geometry_events: int
    pending_outbound_bytes: int
    pending_outbound_events: int
    ansi_history_bytes: int
    service_batches: int = 1

    def __post_init__(self) -> None:
        minima = {
            "max_cols": 1,
            "max_rows": 1,
            "egress_high_publications": 2,
            "egress_high_batches": 1,
            "egress_low_batches": 0,
            "ingress_bytes": 1,
            "ingress_events": 1,
            "ingress_control_bytes": 1,
            "ingress_control_events": 1,
            "geometry_events": 1,
            "pending_outbound_bytes": 1,
            "pending_outbound_events": 1,
            "ansi_history_bytes": 0,
            "service_batches": 1,
        }
        for name, minimum in minima.items():
            value = getattr(self, name)
            if isinstance(value, bool):
                raise TypeError(f"{name} must be an integer, not bool")
            try:
                normalized = operator.index(value)
            except TypeError as exc:
                raise TypeError(f"{name} must be an integer") from exc
            if normalized < minimum:
                raise ValueError(f"{name} must be at least {minimum}")
            if normalized > (1 << 64) - 1:
                raise ValueError(f"{name} must fit uint64")
            object.__setattr__(self, name, int(normalized))
        if self.max_cols > 0xFFFF or self.max_rows > 0xFFFF:
            raise ValueError("maximum geometry must fit APT-1 uint16 fields")
        if self.egress_low_batches >= self.egress_high_batches:
            raise ValueError("egress batch low watermark must be below high")
        if self.ingress_control_bytes >= self.ingress_bytes:
            raise ValueError("ordinary ingress needs a nonempty byte allowance")
        if self.ingress_control_events >= self.ingress_events:
            raise ValueError("ordinary ingress needs a nonempty event allowance")
        if self.ingress_control_bytes < CONTROL_RESERVE_BYTES:
            raise ValueError(
                "ingress control reserve must admit the APT-1 reserve"
            )
        if self.ingress_bytes - self.ingress_control_bytes < 68:
            raise ValueError("ordinary ingress cannot admit every fixed input")
        if self.pending_outbound_bytes < CONTROL_RESERVE_BYTES:
            raise ValueError("pending outbound bytes must admit control reserve")
        if self.pending_outbound_events < 3:
            raise ValueError("pending outbound needs three result records")
        # Constructing the maximum geometry proves the complete cross-object
        # capacity contract once, rather than discovering a mismatch at attach.
        self.configuration(self.max_cols, self.max_rows)

    @property
    def maximum_transaction_bytes(self) -> int:
        return snapshot_wire_bytes(self.max_cols, self.max_rows)

    @property
    def retained_publication_bytes(self) -> int:
        return self.maximum_transaction_bytes + CONTROL_RESERVE_BYTES

    def configuration(self, cols: int, rows: int) -> PresentationSessionConfig:
        """Bind selected geometry without weakening the declared maxima."""
        selected: dict[str, int] = {}
        for name, value, maximum in (
            ("cols", cols, self.max_cols),
            ("rows", rows, self.max_rows),
        ):
            if isinstance(value, bool):
                raise TypeError(f"{name} must be an integer, not bool")
            try:
                normalized = operator.index(value)
            except TypeError as exc:
                raise TypeError(f"{name} must be an integer") from exc
            if not 1 <= normalized <= maximum:
                raise ValueError(f"{name} exceeds presentation policy")
            selected[name] = int(normalized)
        transaction_bytes = self.maximum_transaction_bytes
        publication_bytes = self.retained_publication_bytes
        return PresentationSessionConfig(
            host_limits=HostPortLimits(
                egress=EgressWatermarks(
                    high_bytes=(
                        publication_bytes * self.egress_high_publications
                    ),
                    low_bytes=publication_bytes,
                    high_batches=self.egress_high_batches,
                    low_batches=self.egress_low_batches,
                ),
                retained_publication_bytes=publication_bytes,
                ingress_bytes=self.ingress_bytes,
                ingress_events=self.ingress_events,
                ingress_control_bytes=self.ingress_control_bytes,
                ingress_control_events=self.ingress_control_events,
                geometry_events=self.geometry_events,
            ),
            terminal_config=TerminalConfig(
                max_payload=max(32, 12 + 8 * self.max_cols),
                max_transaction_bytes=transaction_bytes,
                terminal_receive_credit=transaction_bytes,
                max_cells=self.max_cols * self.max_rows,
                max_feed_bytes=publication_bytes,
                max_cols=self.max_cols,
                max_rows=self.max_rows,
                cols=selected["cols"],
                rows=selected["rows"],
            ),
            driver_limits=DriverLimits(
                self.pending_outbound_bytes,
                self.pending_outbound_events,
            ),
            ansi_history_bytes=self.ansi_history_bytes,
            service_batches=self.service_batches,
        )

    def to_dict(self) -> dict[str, int]:
        return asdict(self)


class MachineSession:
    """One synchronous owner for a MegaPad machine and terminal model."""

    KEY_SEQUENCES = {
        "enter": b"\r",
        "return": b"\r",
        "escape": b"\x1b",
        "esc": b"\x1b",
        "tab": b"\t",
        "backspace": b"\x08",
        "delete": b"\x1b[3~",
        "up": b"\x1b[A",
        "down": b"\x1b[B",
        "right": b"\x1b[C",
        "left": b"\x1b[D",
        "home": b"\x1b[H",
        "end": b"\x1b[F",
        "pageup": b"\x1b[5~",
        "pagedown": b"\x1b[6~",
        "insert": b"\x1b[2~",
        "f1": b"\x1bOP",
        "f2": b"\x1bOQ",
        "f3": b"\x1bOR",
        "f4": b"\x1bOS",
        "f5": b"\x1b[15~",
        "f6": b"\x1b[17~",
        "f7": b"\x1b[18~",
        "f8": b"\x1b[19~",
        "f9": b"\x1b[20~",
        "f10": b"\x1b[21~",
        "f11": b"\x1b[23~",
        "f12": b"\x1b[24~",
    }
    NAMED_CHARACTERS = {
        "space": " ",
    }
    PRESENTATION_KEY_SYMBOLS = {
        "backspace": 0x00110001,
        "tab": 0x00110002,
        "enter": 0x00110003,
        "return": 0x00110003,
        "escape": 0x00110004,
        "esc": 0x00110004,
        "insert": 0x00110005,
        "delete": 0x00110006,
        "home": 0x00110007,
        "end": 0x00110008,
        "pageup": 0x00110009,
        "pagedown": 0x0011000A,
        "left": 0x0011000B,
        "right": 0x0011000C,
        "up": 0x0011000D,
        "down": 0x0011000E,
        **{f"f{index}": 0x0011001F + index for index in range(1, 13)},
    }
    PRESENTATION_MODIFIERS = {
        "shift": 1 << 0,
        "ctrl": 1 << 1,
        "alt": 1 << 2,
        "super": 1 << 3,
    }
    MODIFIED_CSI_KEYS = {
        "up": ("1", "A"),
        "down": ("1", "B"),
        "right": ("1", "C"),
        "left": ("1", "D"),
        "home": ("1", "H"),
        "end": ("1", "F"),
        "insert": ("2", "~"),
        "delete": ("3", "~"),
        "pageup": ("5", "~"),
        "pagedown": ("6", "~"),
        "f5": ("15", "~"),
        "f6": ("17", "~"),
        "f7": ("18", "~"),
        "f8": ("19", "~"),
        "f9": ("20", "~"),
        "f10": ("21", "~"),
        "f11": ("23", "~"),
        "f12": ("24", "~"),
    }

    def __init__(
        self,
        system: MegapadSystem,
        *,
        cols: int = 80,
        rows: int = 30,
        batch_steps: int = 100_000,
        presentation: PresentationSessionConfig | None = None,
    ):
        if batch_steps <= 0:
            raise ValueError("batch_steps must be positive")
        if presentation is not None and not isinstance(
            presentation, PresentationSessionConfig
        ):
            raise TypeError("presentation must be PresentationSessionConfig or None")
        if presentation is not None and (
            cols != presentation.terminal_config.cols
            or rows != presentation.terminal_config.rows
        ):
            raise ValueError(
                "session geometry must match the presentation terminal config"
            )
        self.system = system
        self.batch_steps = int(batch_steps)
        self._presentation_config = presentation
        self._presentation_driver: PresentationTerminalDriver | None = None
        self._presentation_view: TerminalView | None = None
        self._presentation_view_selected = False
        self._presentation_failure_reason: str | None = None
        self._presentation_lost = False
        self._last_batch_presentation_progress = False
        self.terminal = VirtualTerminal(
            cols=cols,
            rows=rows,
            uart_inject=self._inject_terminal_response,
        )
        self.raw_output = bytearray()
        self._raw_output_total = 0
        self._raw_output_start = 0
        self.output_batches = 0
        self.output_byte_callbacks = 0
        self.revision = 0
        self.bios_labels: dict[str, int] = {}
        self._closed = False
        self._old_on_tx = self.system.uart.on_tx
        self._old_on_tx_batch = self.system.uart.on_tx_batch
        self.system.uart.on_tx = self._receive_byte
        self.system.uart.on_tx_batch = self._receive_batch
        try:
            if presentation is None:
                self.resize(cols, rows)
            else:
                self._attach_presentation_terminal()
        except BaseException:
            if self._presentation_driver is not None:
                self._presentation_driver.close()
                self._presentation_driver = None
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
        presentation: PresentationSessionConfig | None = None,
        nic_backend: NICBackend | None = None,
        realtime_clock: bool = False,
    ) -> "MachineSession":
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
            presentation=presentation,
        )
        session.bios_labels = dict(labels)
        return session

    def __enter__(self) -> "MachineSession":
        return self

    def __exit__(self, exc_type, exc, traceback):
        self.close()

    @property
    def presentation_enabled(self) -> bool:
        return self._presentation_config is not None

    @property
    def presentation_driver(self) -> PresentationTerminalDriver | None:
        return self._presentation_driver

    @property
    def presentation_state(self) -> TerminalState | None:
        if self._presentation_failure_reason is not None:
            return TerminalState.FAILED
        driver = self._presentation_driver
        return None if driver is None else driver.core.state

    @property
    def presentation_failure(self) -> str | None:
        if self._presentation_failure_reason is not None:
            return self._presentation_failure_reason
        driver = self._presentation_driver
        if driver is not None:
            if driver.failure_reason is not None:
                self._record_presentation_failure(driver.failure_reason)
                return self._presentation_failure_reason
            host = self.system.presentation_terminal_host
            if (
                driver.closed
                or host.active_attachment_epoch != driver.attachment_epoch
            ):
                self._record_presentation_failure(
                    "presentation attachment became stale",
                    lost=True,
                )
                return self._presentation_failure_reason
        if self._presentation_config is not None:
            host_failure = self.system.presentation_terminal_host.failure_reason
            if host_failure is not None:
                self._record_presentation_failure(host_failure)
                return self._presentation_failure_reason
            if driver is None and not self._closed:
                self._record_presentation_failure(
                    "presentation driver is unavailable",
                    lost=True,
                )
                return self._presentation_failure_reason
        return None

    @property
    def presentation_lost(self) -> bool:
        """Whether the exact attachment disappeared outside controlled reset."""

        return self._presentation_lost

    @property
    def raw_output_start(self) -> int:
        """Absolute offset of the first retained diagnostic ANSI byte."""

        return self._raw_output_start

    @property
    def raw_output_end(self) -> int:
        """Absolute offset immediately after all observed ANSI bytes."""

        return self._raw_output_total

    @property
    def visible_geometry(self) -> tuple[int, int]:
        """Geometry of the immutable view currently exposed to observers."""

        view = self._presentation_view if self._presentation_view_selected else None
        if view is not None:
            return view.cols, view.rows
        with self.terminal._lock:
            return self.terminal.cols, self.terminal.rows

    @property
    def presentation_work_pending(self) -> bool:
        """Whether a runner boundary can advance owned terminal work."""

        return self._presentation_has_pending_work()

    @property
    def last_batch_made_progress(self) -> bool:
        return self._last_batch_presentation_progress

    def close(self):
        if self._closed:
            return
        try:
            driver = self._presentation_driver
            if driver is not None:
                driver.close()
                self._presentation_driver = None
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
        reattach = self.presentation_enabled and self.system._booted
        try:
            if reattach:
                self._close_presentation_terminal()
                self._presentation_view = None
                if self._presentation_view_selected:
                    self.revision += 1
                self._presentation_view_selected = False
            self.system.boot(entry)
            if reattach:
                self._attach_presentation_terminal()
        except BaseException as exc:
            if self.presentation_enabled:
                self._record_presentation_failure(
                    f"presentation boot failed: {type(exc).__name__}: {exc}",
                    lost=self._presentation_driver is None,
                )
            raise

    def reset(self, entry: int = 0, *, clear_terminal: bool = True):
        """Reset the owned machine and optionally clear captured terminal state."""
        try:
            self._close_presentation_terminal()
            self.raw_output.clear()
            self._raw_output_start = self._raw_output_total
            self.output_batches = 0
            self.output_byte_callbacks = 0
            self._presentation_view = None
            self._presentation_view_selected = False
            self._presentation_failure_reason = None
            self._presentation_lost = False
            self._last_batch_presentation_progress = False
            if clear_terminal:
                cols, rows = self.terminal.cols, self.terminal.rows
                self.terminal = VirtualTerminal(
                    cols=cols,
                    rows=rows,
                    uart_inject=self._inject_terminal_response,
                )
                if not self.presentation_enabled:
                    self.system.uart_geom.host_set_size(cols, rows)
            self.revision += 1
            self.system.boot(entry, discard_uart_output=True)
            if self.presentation_enabled:
                self._attach_presentation_terminal()
        except BaseException as exc:
            if self.presentation_enabled:
                self._record_presentation_failure(
                    f"presentation reset failed: {type(exc).__name__}: {exc}",
                    lost=self._presentation_driver is None,
                )
            raise

    def _attach_presentation_terminal(self) -> None:
        config = self._presentation_config
        if config is None:
            return
        if self._presentation_driver is not None:
            raise RuntimeError("presentation terminal is already attached")
        terminal_config = replace(
            config.terminal_config,
            cols=self.terminal.cols,
            rows=self.terminal.rows,
        )
        self._presentation_driver = PresentationTerminalDriver.attach(
            self.system,
            config.host_limits,
            terminal_config,
            config.driver_limits,
            ansi_sink=self._receive_presentation_ansi,
            view_sink=self._receive_presentation_view,
        )
        self._presentation_failure_reason = None
        self._presentation_lost = False

    def _close_presentation_terminal(self) -> None:
        driver = self._presentation_driver
        if driver is None:
            return
        driver.close()
        self._presentation_driver = None

    def _inject_terminal_response(self, data: bytes) -> None:
        if self._presentation_mutation_blocked():
            raise RuntimeError(self._presentation_failure_reason)
        driver = self._presentation_driver
        if driver is None:
            self.system.uart.inject_input(data)
            return
        status = driver.send_legacy_input(data)
        if status is not DriverStatus.PROGRESS:
            raise RuntimeError(
                f"cannot enqueue ANSI terminal response: {status.value}"
            )

    def _append_raw_output(self, data: bytes) -> None:
        payload = bytes(data)
        if not payload:
            return
        self._raw_output_total += len(payload)
        config = self._presentation_config
        if config is None:
            self.raw_output.extend(payload)
            return
        limit = config.ansi_history_bytes
        if limit == 0:
            self.raw_output.clear()
            self._raw_output_start = self._raw_output_total
            return
        if len(payload) >= limit:
            self.raw_output[:] = payload[-limit:]
        else:
            overflow = len(self.raw_output) + len(payload) - limit
            if overflow > 0:
                del self.raw_output[:overflow]
            self.raw_output.extend(payload)
        self._raw_output_start = self._raw_output_total - len(self.raw_output)

    def _presentation_mutation_blocked(self) -> bool:
        reason = self.presentation_failure
        if reason is None:
            return False
        if self._presentation_failure_reason is None:
            self._presentation_failure_reason = reason
        return True

    def _receive_byte(self, value: int):
        self._append_raw_output(bytes((value,)))
        self.output_byte_callbacks += 1
        self.terminal.write(value)
        self.revision += 1

    def _receive_batch(self, data: bytes):
        self._append_raw_output(data)
        self.output_batches += 1
        self.terminal.write(data)
        self.revision += 1

    def _receive_presentation_ansi(self, data: bytes) -> None:
        self._receive_batch(data)

    def _receive_presentation_view(self, view: TerminalView) -> None:
        if (self.terminal.cols, self.terminal.rows) != (view.cols, view.rows):
            self.terminal.resize(view.cols, view.rows)
        self._presentation_view = view
        self._presentation_view_selected = True
        self.revision += 1

    def clear_output(self):
        self.raw_output.clear()
        self._raw_output_start = self._raw_output_total

    def raw_text(self) -> str:
        return bytes(self.raw_output).decode("utf-8", errors="replace")

    def screen_text(self, trim_right: bool = False) -> str:
        return self.snapshot().text(trim_right=trim_right)

    def service_presentation_terminal(self) -> DriverServiceResult | None:
        """Service the optional driver without executing guest instructions."""

        driver = self._presentation_driver
        config = self._presentation_config
        if config is None:
            return None
        if driver is None:
            reason = self.presentation_failure or "presentation driver is unavailable"
            self._latch_presentation_failure(reason, lost=True)
        if self._presentation_failure_reason is not None:
            raise TerminalSessionError(self._presentation_failure_reason)
        result = driver.service(max_batches=config.service_batches)
        self._raise_presentation_failure(result)
        self._sync_presentation_geometry()
        self._refresh_presentation_display_boundary()
        return result

    def run_batch_stats(self, steps: int | None = None) -> SystemRunStats:
        """Run one session-owned driver/machine/driver alternation."""

        count = self.batch_steps if steps is None else operator.index(steps)
        if count <= 0:
            raise ValueError("steps must be positive")
        before = self.service_presentation_terminal()
        stats = self.system.run_batch_stats(count)
        after = self.service_presentation_terminal()
        self._last_batch_presentation_progress = bool(
            stats.external_events_applied
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
            reason = self.system.presentation_terminal_host.failure_reason
            self._latch_presentation_failure(reason or "presentation host failed")
        self._refresh_presentation_display_boundary()
        return stats

    def _raise_presentation_failure(
        self,
        result: DriverServiceResult,
    ) -> None:
        if result.status is DriverStatus.FAILED:
            driver = self._presentation_driver
            reason = None if driver is None else driver.failure_reason
            self._latch_presentation_failure(reason or "presentation driver failed")
        if result.status is DriverStatus.STALE:
            self._latch_presentation_failure(
                "presentation attachment became stale",
                lost=True,
            )
        host_failure = self.system.presentation_terminal_host.failure_reason
        if host_failure is not None:
            self._latch_presentation_failure(host_failure)

    def _latch_presentation_failure(
        self,
        reason: str,
        *,
        lost: bool = False,
    ) -> None:
        self._record_presentation_failure(reason, lost=lost)
        raise TerminalSessionError(self._presentation_failure_reason)

    def _record_presentation_failure(
        self,
        reason: str,
        *,
        lost: bool = False,
    ) -> None:
        if self._presentation_failure_reason is None:
            self._presentation_failure_reason = str(reason)
        self._presentation_lost = self._presentation_lost or lost

    def _presentation_has_pending_work(self) -> bool:
        driver = self._presentation_driver
        if driver is None:
            return False
        host = self.system.presentation_terminal_host
        return bool(
            driver.pending_outbound_events
            or (
                driver.pending_resize is not None
                and driver.core.resize_ready
            )
            or host.accepted_egress_batches
            or host.retained_publication is not None
            or host.pending_ingress_events
            or host.pending_geometry_events
        )

    def _refresh_presentation_display_boundary(self) -> None:
        driver = self._presentation_driver
        if driver is None or not self._presentation_view_selected:
            return
        host = self.system.presentation_terminal_host
        if (
            driver.core.state is TerminalState.ANSI
            and driver.pending_outbound_events == 0
            and host.pending_ingress_events == 0
            and host.pending_geometry_events == 0
        ):
            self._presentation_view_selected = False
            self.revision += 1

    def _sync_presentation_geometry(self) -> None:
        """Mirror only geometry already committed by the protocol core."""

        driver = self._presentation_driver
        if driver is None:
            return
        cols, rows = driver.core.selected_geometry
        if (self.terminal.cols, self.terminal.rows) == (cols, rows):
            return
        self.terminal.resize(cols, rows)
        if not self._presentation_view_selected:
            self.revision += 1

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

        while steps < max_steps:
            if has_match():
                matched = True
                reason = "matched"
                break
            if self.presentation_failure is not None:
                reason = "terminal_failure"
                break
            if (
                self.system.all_halted
                and not self._presentation_has_pending_work()
            ):
                reason = "halted"
                break
            if time.perf_counter() - start >= wall_timeout_s:
                reason = "wall_timeout"
                break
            if (
                self.system.all_idle_or_halted
                and not self.system.uart.has_rx_data
                and not self._presentation_has_pending_work()
            ):
                if not advance_idle:
                    reason = "idle"
                    break
                self.system.bus.tick(idle_tick_cycles)
                if self.system.timer.irq_pending:
                    for cpu in self.system.cores:
                        if cpu.idle and cpu.flag_i:
                            cpu.idle = False
                            break
                for cpu in self.system.cores:
                    if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                        cpu.idle = False
                core0 = self.system.cores[0]
                if core0.idle and self.system._any_nic_rx():
                    core0.idle = False
                if self.system.all_idle_or_halted:
                    time.sleep(0.001)
                continue
            count = min(self.batch_steps, max_steps - steps)
            if self._presentation_driver is None:
                executed = self.system.run_batch(count)
                presentation_progress = False
                stop_reason = ""
            else:
                try:
                    stats = self.run_batch_stats(count)
                except TerminalSessionError:
                    reason = "terminal_failure"
                    break
                executed = stats.instructions_executed
                presentation_progress = self._last_batch_presentation_progress
                stop_reason = stats.system_stop_reason
            batches += 1
            if executed <= 0 and not (
                presentation_progress
                or self._presentation_has_pending_work()
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

    def send_text(self, text: str | bytes) -> DriverStatus | None:
        if isinstance(text, str):
            payload = text.encode("utf-8")
        else:
            try:
                payload = bytes(text)
            except (TypeError, ValueError) as exc:
                raise TypeError("text must be str or bytes-like") from exc
        if self._presentation_mutation_blocked():
            return DriverStatus.FAILED
        driver = self._presentation_driver
        if driver is None:
            self.system.uart.inject_input(payload)
            return None
        if driver.core.state in {TerminalState.ANSI, TerminalState.PROBING}:
            return driver.send_legacy_input(payload)
        return driver.send_text(payload)

    @staticmethod
    def _key_parts(key: str) -> tuple[str, set[str]]:
        if not isinstance(key, str):
            raise TypeError("key must be str")
        normalized = key.strip().lower().replace("_", "")
        parts = normalized.split("+")
        if not parts or not parts[-1]:
            raise ValueError(f"unknown key: {key}")
        modifiers = set(parts[:-1])
        return parts[-1], modifiers

    def _legacy_key_bytes(self, key: str) -> bytes:
        normalized = key.strip().lower().replace("_", "")
        if normalized in self.KEY_SEQUENCES:
            return self.KEY_SEQUENCES[normalized]
        base, modifiers = self._key_parts(key)
        if (
            modifiers
            and modifiers <= {"ctrl", "alt", "shift"}
            and base in self.MODIFIED_CSI_KEYS
        ):
            modifier = 1
            modifier += 1 if "shift" in modifiers else 0
            modifier += 2 if "alt" in modifiers else 0
            modifier += 4 if "ctrl" in modifiers else 0
            parameter, final = self.MODIFIED_CSI_KEYS[base]
            return f"\x1b[{parameter};{modifier}{final}".encode("ascii")
        char = self.NAMED_CHARACTERS.get(base, base)
        if len(char) == 1 and modifiers == {"ctrl"}:
            if "a" <= char <= "z":
                return bytes([ord(char) & 0x1F])
        if len(char) == 1 and modifiers == {"alt"}:
            return b"\x1b" + char.encode("utf-8")
        if len(char) == 1 and modifiers and modifiers <= {"ctrl", "alt", "shift"}:
            modifier = 1
            modifier += 1 if "shift" in modifiers else 0
            modifier += 2 if "alt" in modifiers else 0
            modifier += 4 if "ctrl" in modifiers else 0
            return f"\x1b[{ord(char)};{modifier}u".encode("ascii")
        if len(char) == 1 and not modifiers:
            return char.encode("utf-8")
        raise ValueError(f"unknown key: {key}")

    def _presentation_key(self, key: str) -> tuple[int, int]:
        base, modifiers = self._key_parts(key)
        if not modifiers <= self.PRESENTATION_MODIFIERS.keys():
            raise ValueError(f"unknown key modifier in: {key}")
        symbol = self.PRESENTATION_KEY_SYMBOLS.get(base)
        if symbol is None:
            char = self.NAMED_CHARACTERS.get(base, base)
            if len(char) != 1:
                raise ValueError(f"unknown key: {key}")
            symbol = ord(char)
        modifier_bits = 0
        for modifier in modifiers:
            modifier_bits |= self.PRESENTATION_MODIFIERS[modifier]
        return symbol, modifier_bits

    def send_key(self, key: str) -> DriverStatus | None:
        if self._presentation_mutation_blocked():
            return DriverStatus.FAILED
        driver = self._presentation_driver
        if driver is None or driver.core.state in {
            TerminalState.ANSI,
            TerminalState.PROBING,
        }:
            payload = self._legacy_key_bytes(key)
            if driver is None:
                self.system.uart.inject_input(payload)
                return None
            return driver.send_legacy_input(payload)
        symbol, modifiers = self._presentation_key(key)
        return driver.send_key(symbol, modifiers=modifiers)

    def send_pointer(
        self,
        x: int,
        y: int,
        *,
        buttons: int = 0,
        modifiers: int = 0,
        kind: int = 1,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> DriverStatus:
        if self._presentation_mutation_blocked():
            return DriverStatus.FAILED
        driver = self._presentation_driver
        if driver is None:
            return DriverStatus.INVALID
        return driver.send_pointer(
            x,
            y,
            buttons=buttons,
            modifiers=modifiers,
            kind=kind,
            wheel_x=wheel_x,
            wheel_y=wheel_y,
        )

    def send_focus(self, focused: bool) -> DriverStatus:
        if self._presentation_mutation_blocked():
            return DriverStatus.FAILED
        driver = self._presentation_driver
        if driver is None:
            return DriverStatus.INVALID
        return driver.send_focus(focused)

    def resize(self, cols: int, rows: int) -> DriverStatus | None:
        if self._presentation_mutation_blocked():
            return DriverStatus.FAILED
        driver = self._presentation_driver
        if driver is not None:
            state = driver.core.state
            status = driver.request_resize(cols, rows)
            if (
                status is DriverStatus.PROGRESS
                and state is TerminalState.ANSI
            ):
                changed = (
                    cols != self.terminal.cols or rows != self.terminal.rows
                )
                self.terminal.resize(cols, rows)
                if changed and not self._presentation_view_selected:
                    self.revision += 1
            return status
        changed = cols != self.terminal.cols or rows != self.terminal.rows
        self.terminal.resize(cols, rows)
        self.system.uart_geom.host_set_size(cols, rows)
        if changed:
            self.revision += 1
        return None

    def step(self) -> int:
        if not self.presentation_enabled:
            return self.system.step()
        self.service_presentation_terminal()
        cycles = self.system.step()
        self.service_presentation_terminal()
        self._refresh_presentation_display_boundary()
        return cycles

    def snapshot(self) -> TerminalSnapshot:
        view = (
            self._presentation_view
            if self._presentation_view_selected
            else None
        )
        if view is not None:
            return self._snapshot_presentation_view(view)
        terminal = self.terminal
        with terminal._lock:
            cells = tuple(
                tuple(
                    TerminalCell(
                        char=cell[0],
                        fg=tuple(cell[1]),
                        bg=tuple(cell[2]),
                        attrs=cell[3] if len(cell) > 3 else 0,
                    )
                    for cell in row
                )
                for row in terminal.grid
            )
            return TerminalSnapshot(
                cols=terminal.cols,
                rows=terminal.rows,
                cells=cells,
                cursor_col=terminal.cx,
                cursor_row=terminal.cy,
                cursor_visible=terminal.cursor_visible,
                alternate_screen=terminal._in_alt_screen,
            )

    @staticmethod
    def _snapshot_presentation_view(view: TerminalView) -> TerminalSnapshot:
        palette = VirtualTerminal.COLORS
        cells = tuple(
            tuple(
                TerminalCell(
                    char=chr(cell.codepoint),
                    fg=palette[cell.foreground],
                    bg=palette[cell.background],
                    attrs=(cell.attributes & 0x3F)
                    | ((cell.attributes & 0x40) << 1),
                )
                for cell in row
            )
            for row in view.cells
        )
        return TerminalSnapshot(
            cols=view.cols,
            rows=view.rows,
            cells=cells,
            cursor_col=view.cursor.column,
            cursor_row=view.cursor.row,
            cursor_visible=view.cursor.visible,
            alternate_screen=False,
        )


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


def _resolve_font(path: str | os.PathLike | None) -> Path | None:
    candidates = []
    if path:
        candidates.append(Path(path).expanduser())
    if os.environ.get("MP64_TERMINAL_FONT"):
        candidates.append(Path(os.environ["MP64_TERMINAL_FONT"]).expanduser())
    candidates.extend([
        Path("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"),
        Path("/usr/share/fonts/dejavu/DejaVuSansMono.ttf"),
    ])
    for candidate in candidates:
        if candidate.is_file():
            return candidate.resolve()
    return None
