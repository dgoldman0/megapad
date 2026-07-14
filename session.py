"""Synchronous machine control and headless terminal capture for MegaPad."""

from __future__ import annotations

import json
import os
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Literal

from asm import assemble
from display import VirtualTerminal
from system import MegapadSystem

if TYPE_CHECKING:
    from nic_backends import NICBackend


_BIOS_CACHE: dict[tuple[str, int, int], tuple[bytes, dict[str, int]]] = {}
_ACCEL_HOOKS = (
    ("w_rect_fill", 1),
    ("w_blit_glyph", 2),
    ("w_vram_copy", 3),
    ("w_blit_string", 4),
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
    ):
        if batch_steps <= 0:
            raise ValueError("batch_steps must be positive")
        self.system = system
        self.batch_steps = int(batch_steps)
        self.terminal = VirtualTerminal(
            cols=cols,
            rows=rows,
            uart_inject=self.system.uart.inject_input,
        )
        self.raw_output = bytearray()
        self.output_batches = 0
        self.output_byte_callbacks = 0
        self.revision = 0
        self.bios_labels: dict[str, int] = {}
        self._closed = False
        self._old_on_tx = self.system.uart.on_tx
        self._old_on_tx_batch = self.system.uart.on_tx_batch
        self.system.uart.on_tx = self._receive_byte
        self.system.uart.on_tx_batch = self._receive_batch
        self.resize(cols, rows)

    @classmethod
    def from_bios(
        cls,
        bios_path: str | os.PathLike,
        *,
        storage_image: str | os.PathLike | None = None,
        ram_size: int = 1 << 20,
        ext_mem_size: int = 16 << 20,
        vram_size: int = 4 << 20,
        num_cores: int = 1,
        num_clusters: int = 0,
        cols: int = 80,
        rows: int = 30,
        batch_steps: int = 100_000,
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
            nic_backend=nic_backend,
            realtime_clock=realtime_clock,
        )
        system.load_binary(0, code)
        for name, hook_id in _ACCEL_HOOKS:
            if name in labels:
                system.cpu.register_accel_hook(labels[name], hook_id)
        session = cls(system, cols=cols, rows=rows, batch_steps=batch_steps)
        session.bios_labels = dict(labels)
        return session

    def __enter__(self) -> "MachineSession":
        return self

    def __exit__(self, exc_type, exc, traceback):
        self.close()

    def close(self):
        if self._closed:
            return
        try:
            self.system.storage.save_image()
        finally:
            self.system.uart.on_tx = self._old_on_tx
            self.system.uart.on_tx_batch = self._old_on_tx_batch
            self.system.nic.stop()
            self._closed = True

    def boot(self, entry: int = 0):
        self.system.boot(entry)

    def reset(self, entry: int = 0, *, clear_terminal: bool = True):
        """Reset the owned machine and optionally clear captured terminal state."""
        if hasattr(self.system.cpu, "_cs"):
            self.system.cpu._cs.uart_init()
        self.system.uart._tx_ring_base = 0
        self.system.uart.tx_buffer.clear()
        self.system.uart.rx_buffer.clear()
        self.raw_output.clear()
        self.output_batches = 0
        self.output_byte_callbacks = 0
        if clear_terminal:
            cols, rows = self.terminal.cols, self.terminal.rows
            self.terminal = VirtualTerminal(
                cols=cols,
                rows=rows,
                uart_inject=self.system.uart.inject_input,
            )
            self.system.uart_geom.host_set_size(cols, rows)
        self.revision += 1
        self.system.boot(entry)

    def _receive_byte(self, value: int):
        self.raw_output.append(value)
        self.output_byte_callbacks += 1
        self.terminal.write(value)
        self.revision += 1

    def _receive_batch(self, data: bytes):
        self.raw_output.extend(data)
        self.output_batches += 1
        self.terminal.write(data)
        self.revision += 1

    def clear_output(self):
        self.raw_output.clear()

    def raw_text(self) -> str:
        return bytes(self.raw_output).decode("utf-8", errors="replace")

    def screen_text(self, trim_right: bool = False) -> str:
        return self.snapshot().text(trim_right=trim_right)

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
        output_start = len(self.raw_output)
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
            if self.system.all_halted:
                reason = "halted"
                break
            if time.perf_counter() - start >= wall_timeout_s:
                reason = "wall_timeout"
                break
            if self.system.all_idle_or_halted and not self.system.uart.has_rx_data:
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
            executed = self.system.run_batch(count)
            batches += 1
            if executed <= 0:
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
            output_bytes=len(self.raw_output) - output_start,
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

    def send_text(self, text: str | bytes):
        self.system.uart.inject_input(text)

    def send_key(self, key: str):
        normalized = key.strip().lower().replace("_", "")
        if normalized in self.KEY_SEQUENCES:
            self.send_text(self.KEY_SEQUENCES[normalized])
            return
        parts = normalized.split("+")
        modifiers = set(parts[:-1])
        base = parts[-1]
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
            self.send_text(f"\x1b[{parameter};{modifier}{final}".encode("ascii"))
            return
        char = self.NAMED_CHARACTERS.get(base, base)
        if len(char) == 1 and modifiers == {"ctrl"}:
            if "a" <= char <= "z":
                self.send_text(bytes([ord(char) & 0x1F]))
                return
        if len(char) == 1 and modifiers == {"alt"}:
            self.send_text(b"\x1b" + char.encode("utf-8"))
            return
        if len(char) == 1 and modifiers and modifiers <= {"ctrl", "alt", "shift"}:
            modifier = 1
            modifier += 1 if "shift" in modifiers else 0
            modifier += 2 if "alt" in modifiers else 0
            modifier += 4 if "ctrl" in modifiers else 0
            self.send_text(f"\x1b[{ord(char)};{modifier}u".encode("ascii"))
            return
        if len(char) == 1 and not modifiers:
            self.send_text(char)
            return
        raise ValueError(f"unknown key: {key}")

    def resize(self, cols: int, rows: int):
        changed = cols != self.terminal.cols or rows != self.terminal.rows
        self.terminal.resize(cols, rows)
        self.system.uart_geom.host_set_size(cols, rows)
        if changed:
            self.revision += 1

    def step(self) -> int:
        return self.system.step()

    def snapshot(self) -> TerminalSnapshot:
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
