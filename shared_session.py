"""Shared single-owner MegaPad runtime and local JSON control protocol."""

from __future__ import annotations

import base64
import json
import operator
import os
import socket
import stat
import threading
import time
from collections.abc import Mapping
from pathlib import Path
from typing import Any

from rich_terminal import DriverStatus
from rich_terminal.apt1 import UINT32_MAX, UINT64_MAX
from rich_terminal.retained_view import (
    INT32_MAX,
    INT32_MIN,
    DisplayScope,
    GlyphRunDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
)
from rich_terminal.retained_scene import ObjectBounds, RGBA
from rich_terminal.update_authority import TerminalUpdateError
from runtime_paths import RuntimeOwnershipLock, shared_session_socket
from session import (
    MachineSession,
    TerminalCell,
    TerminalDisplayOffer,
    TerminalSnapshot,
)


DEFAULT_SOCKET = shared_session_socket()
MAX_REQUEST_BYTES = 1 << 20


def _wire_object(data, name: str, fields: tuple[str, ...]) -> Mapping[str, Any]:
    if not isinstance(data, Mapping):
        raise TypeError(f"{name} must be an object")
    keys = set(data)
    expected = set(fields)
    if keys != expected:
        missing = sorted(expected - keys)
        unknown = sorted(keys - expected)
        raise ValueError(
            f"{name} fields are not exact; missing={missing}, unknown={unknown}"
        )
    return data


def _wire_integer(
    value,
    name: str,
    *,
    minimum: int,
    maximum: int | None = None,
) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        normalized = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if normalized < minimum or (maximum is not None and normalized > maximum):
        upper = "unbounded" if maximum is None else str(maximum)
        raise ValueError(f"{name} must be between {minimum} and {upper}")
    return int(normalized)


def _wire_boolean(value, name: str) -> bool:
    if not isinstance(value, bool):
        raise TypeError(f"{name} must be bool")
    return value


def _wire_text(value, name: str) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    try:
        value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} must contain only Unicode scalar values") from exc
    return value


def _wire_integer_array(
    value,
    name: str,
    length: int,
    *,
    maximum: int = UINT32_MAX,
) -> tuple[int, ...]:
    if not isinstance(value, (list, tuple)) or len(value) != length:
        raise TypeError(f"{name} must be an array of {length} integers")
    return tuple(
        _wire_integer(item, f"{name}[{index}]", minimum=0, maximum=maximum)
        for index, item in enumerate(value)
    )


def _rgb_pack(color: tuple[int, int, int]) -> int:
    return (color[0] << 16) | (color[1] << 8) | color[2]


def _rgb_unpack(value: int) -> tuple[int, int, int]:
    return ((value >> 16) & 0xFF, (value >> 8) & 0xFF, value & 0xFF)


def snapshot_to_wire(snapshot: TerminalSnapshot) -> dict:
    """Run-length encode a terminal snapshot for the local viewer protocol."""
    runs: list[list[Any]] = []
    current = None
    count = 0
    for row in snapshot.cells:
        for cell in row:
            value = (
                cell.char,
                _rgb_pack(cell.fg),
                _rgb_pack(cell.bg),
                cell.attrs,
            )
            if value == current:
                count += 1
                continue
            if current is not None:
                runs.append([count, *current])
            current = value
            count = 1
    if current is not None:
        runs.append([count, *current])
    return {
        "cols": snapshot.cols,
        "rows": snapshot.rows,
        "cursor": [
            snapshot.cursor_row,
            snapshot.cursor_col,
            snapshot.cursor_visible,
        ],
        "alternate_screen": snapshot.alternate_screen,
        "runs": runs,
    }


def snapshot_from_wire(data: dict) -> TerminalSnapshot:
    """Decode a strict wire snapshot into the immutable public snapshot type."""

    wire = _wire_object(
        data,
        "snapshot",
        ("cols", "rows", "cursor", "alternate_screen", "runs"),
    )
    cols = _wire_integer(wire["cols"], "snapshot cols", minimum=1)
    rows = _wire_integer(wire["rows"], "snapshot rows", minimum=1)
    expected = cols * rows

    cursor = wire["cursor"]
    if not isinstance(cursor, (list, tuple)) or len(cursor) != 3:
        raise TypeError("snapshot cursor must be a three-item array")
    cursor_row = _wire_integer(
        cursor[0], "snapshot cursor row", minimum=0, maximum=UINT32_MAX
    )
    cursor_col = _wire_integer(
        cursor[1], "snapshot cursor col", minimum=0, maximum=UINT32_MAX
    )
    cursor_visible = _wire_boolean(cursor[2], "snapshot cursor visible")
    if cursor_visible and (cursor_row >= rows or cursor_col >= cols):
        raise ValueError("visible snapshot cursor must be inside the geometry")
    alternate_screen = _wire_boolean(
        wire["alternate_screen"], "snapshot alternate_screen"
    )

    runs = wire["runs"]
    if not isinstance(runs, (list, tuple)):
        raise TypeError("snapshot runs must be an array")
    flat: list[TerminalCell] = []
    for index, run in enumerate(runs):
        if not isinstance(run, (list, tuple)) or len(run) != 5:
            raise TypeError(f"snapshot run {index} must be a five-item array")
        count = _wire_integer(run[0], f"snapshot run {index} count", minimum=1)
        char = _wire_text(run[1], f"snapshot run {index} char")
        if len(char) != 1:
            raise ValueError(f"snapshot run {index} char must be one character")
        fg = _wire_integer(
            run[2], f"snapshot run {index} foreground", minimum=0, maximum=0xFFFFFF
        )
        bg = _wire_integer(
            run[3], f"snapshot run {index} background", minimum=0, maximum=0xFFFFFF
        )
        attrs = _wire_integer(
            run[4], f"snapshot run {index} attrs", minimum=0, maximum=0xFF
        )
        if len(flat) + count > expected:
            raise ValueError("snapshot runs exceed the declared geometry")
        cell = TerminalCell(
            char=char,
            fg=_rgb_unpack(fg),
            bg=_rgb_unpack(bg),
            attrs=attrs,
        )
        flat.extend([cell] * count)
    if len(flat) != expected:
        raise ValueError(f"snapshot has {len(flat)} cells, expected {expected}")
    cells = tuple(
        tuple(flat[row * cols:(row + 1) * cols])
        for row in range(rows)
    )
    return TerminalSnapshot(
        cols=cols,
        rows=rows,
        cells=cells,
        cursor_col=cursor_col,
        cursor_row=cursor_row,
        cursor_visible=cursor_visible,
        alternate_screen=alternate_screen,
    )


def display_scope_to_wire(scope: DisplayScope) -> dict:
    """Encode one exact retained-display scope without hidden model state."""

    if not isinstance(scope, DisplayScope):
        raise TypeError("scope must be DisplayScope")
    return {
        "attachment_epoch": scope.attachment_epoch,
        "session_id": scope.session_id,
        "presentation_epoch": scope.presentation_epoch,
        "model_revision": scope.model_revision,
        "geometry_generation": scope.geometry_generation,
        "cell_revision": scope.cell_revision,
        "retained_revision": scope.retained_revision,
    }


def display_scope_from_wire(data: dict) -> DisplayScope:
    """Decode an exact retained-display scope and re-run all DTO invariants."""

    wire = _wire_object(
        data,
        "display scope",
        (
            "attachment_epoch",
            "session_id",
            "presentation_epoch",
            "model_revision",
            "geometry_generation",
            "cell_revision",
            "retained_revision",
        ),
    )
    retained_revision = wire["retained_revision"]
    if retained_revision is not None:
        retained_revision = _wire_integer(
            retained_revision,
            "display scope retained_revision",
            minimum=0,
            maximum=UINT64_MAX,
        )
    return DisplayScope(
        attachment_epoch=_wire_integer(
            wire["attachment_epoch"],
            "display scope attachment_epoch",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        session_id=_wire_integer(
            wire["session_id"],
            "display scope session_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        presentation_epoch=_wire_integer(
            wire["presentation_epoch"],
            "display scope presentation_epoch",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        model_revision=_wire_integer(
            wire["model_revision"],
            "display scope model_revision",
            minimum=0,
            maximum=UINT64_MAX,
        ),
        geometry_generation=_wire_integer(
            wire["geometry_generation"],
            "display scope geometry_generation",
            minimum=0,
            maximum=UINT64_MAX,
        ),
        cell_revision=_wire_integer(
            wire["cell_revision"],
            "display scope cell_revision",
            minimum=0,
            maximum=UINT64_MAX,
        ),
        retained_revision=retained_revision,
    )


_DRAW_WIRE_FIELDS = (
    "object_id",
    "z_order",
    "bounds",
    "foreground",
    "background",
    "attributes",
    "text",
)
_REGION_WIRE_FIELDS = (
    "owner_id",
    "owner_generation",
    "region_id",
    "cell_x",
    "cell_y",
    "cell_cols",
    "cell_rows",
    "z_order",
    "clipped",
    "draws",
)


def retained_draw_plane_to_wire(plane: RetainedDrawPlane) -> dict:
    """Encode only the immutable renderer-facing draw plane."""

    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    return {
        "retained_initialized": plane.retained_initialized,
        "retained_visible": plane.retained_visible,
        "regions": [
            {
                "owner_id": region.owner_id,
                "owner_generation": region.owner_generation,
                "region_id": region.region_id,
                "cell_x": region.cell_x,
                "cell_y": region.cell_y,
                "cell_cols": region.cell_cols,
                "cell_rows": region.cell_rows,
                "z_order": region.z_order,
                "clipped": region.clipped,
                "draws": [
                    {
                        "object_id": draw.object_id,
                        "z_order": draw.z_order,
                        "bounds": [
                            draw.bounds.left,
                            draw.bounds.top,
                            draw.bounds.right,
                            draw.bounds.bottom,
                        ],
                        "foreground": [
                            draw.foreground.red,
                            draw.foreground.green,
                            draw.foreground.blue,
                            draw.foreground.alpha,
                        ],
                        "background": [
                            draw.background.red,
                            draw.background.green,
                            draw.background.blue,
                            draw.background.alpha,
                        ],
                        "attributes": draw.attributes,
                        "text": draw.text,
                    }
                    for draw in region.draws
                ],
            }
            for region in plane.regions
        ],
    }


def retained_draw_plane_from_wire(data: dict) -> RetainedDrawPlane:
    """Decode the complete draw plane with strict scalar types."""

    wire = _wire_object(
        data,
        "retained draw plane",
        ("retained_initialized", "retained_visible", "regions"),
    )
    regions_wire = wire["regions"]
    if not isinstance(regions_wire, (list, tuple)):
        raise TypeError("retained draw regions must be an array")
    regions: list[RetainedRegionDraw] = []
    for region_index, raw_region in enumerate(regions_wire):
        region = _wire_object(
            raw_region,
            f"retained region {region_index}",
            _REGION_WIRE_FIELDS,
        )
        draws_wire = region["draws"]
        if not isinstance(draws_wire, (list, tuple)):
            raise TypeError(f"retained region {region_index} draws must be an array")
        draws: list[GlyphRunDraw] = []
        for draw_index, raw_draw in enumerate(draws_wire):
            draw = _wire_object(
                raw_draw,
                f"retained region {region_index} draw {draw_index}",
                _DRAW_WIRE_FIELDS,
            )
            prefix = f"retained region {region_index} draw {draw_index}"
            bounds = _wire_integer_array(draw["bounds"], f"{prefix} bounds", 4)
            foreground = _wire_integer_array(
                draw["foreground"], f"{prefix} foreground", 4, maximum=0xFF
            )
            background = _wire_integer_array(
                draw["background"], f"{prefix} background", 4, maximum=0xFF
            )
            draws.append(
                GlyphRunDraw(
                    object_id=_wire_integer(
                        draw["object_id"],
                        f"{prefix} object_id",
                        minimum=1,
                        maximum=UINT64_MAX,
                    ),
                    z_order=_wire_integer(
                        draw["z_order"],
                        f"{prefix} z_order",
                        minimum=INT32_MIN,
                        maximum=INT32_MAX,
                    ),
                    bounds=ObjectBounds(*bounds),
                    foreground=RGBA(*foreground),
                    background=RGBA(*background),
                    attributes=_wire_integer(
                        draw["attributes"], f"{prefix} attributes", minimum=0,
                        maximum=0x7F,
                    ),
                    text=_wire_text(draw["text"], f"{prefix} text"),
                )
            )
        prefix = f"retained region {region_index}"
        regions.append(
            RetainedRegionDraw(
                owner_id=_wire_integer(
                    region["owner_id"],
                    f"{prefix} owner_id",
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
                owner_generation=_wire_integer(
                    region["owner_generation"],
                    f"{prefix} owner_generation",
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
                region_id=_wire_integer(
                    region["region_id"],
                    f"{prefix} region_id",
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
                cell_x=_wire_integer(
                    region["cell_x"], f"{prefix} cell_x", minimum=0, maximum=UINT32_MAX
                ),
                cell_y=_wire_integer(
                    region["cell_y"], f"{prefix} cell_y", minimum=0, maximum=UINT32_MAX
                ),
                cell_cols=_wire_integer(
                    region["cell_cols"],
                    f"{prefix} cell_cols",
                    minimum=1,
                    maximum=UINT32_MAX,
                ),
                cell_rows=_wire_integer(
                    region["cell_rows"],
                    f"{prefix} cell_rows",
                    minimum=1,
                    maximum=UINT32_MAX,
                ),
                z_order=_wire_integer(
                    region["z_order"],
                    f"{prefix} z_order",
                    minimum=INT32_MIN,
                    maximum=INT32_MAX,
                ),
                clipped=_wire_boolean(region["clipped"], f"{prefix} clipped"),
                draws=tuple(draws),
            )
        )
    return RetainedDrawPlane(
        retained_initialized=_wire_boolean(
            wire["retained_initialized"], "retained draw initialized"
        ),
        retained_visible=_wire_boolean(
            wire["retained_visible"], "retained draw visible"
        ),
        regions=tuple(regions),
    )


def display_offer_to_wire(offer: TerminalDisplayOffer) -> dict:
    """Encode one immutable physical offer without model authority objects."""

    if not isinstance(offer, TerminalDisplayOffer):
        raise TypeError("offer must be TerminalDisplayOffer")
    return {
        "offer_id": offer.offer_id,
        "scope": display_scope_to_wire(offer.scope),
        "cell": snapshot_to_wire(offer.cell),
        "retained": retained_draw_plane_to_wire(offer.retained),
    }


def display_offer_from_wire(data: dict) -> TerminalDisplayOffer:
    """Decode an exact immutable physical offer from the display wire."""

    wire = _wire_object(
        data,
        "display offer",
        ("offer_id", "scope", "cell", "retained"),
    )
    return TerminalDisplayOffer(
        offer_id=_wire_integer(wire["offer_id"], "display offer id", minimum=1),
        scope=display_scope_from_wire(wire["scope"]),
        cell=snapshot_from_wire(wire["cell"]),
        retained=retained_draw_plane_from_wire(wire["retained"]),
    )


class SharedMachine:
    """Continuously runs one MachineSession and serializes all mutations."""

    def __init__(
        self,
        session: MachineSession,
        *,
        idle_tick_cycles: int = 200_000,
        idle_sleep_s: float = 0.002,
    ):
        self.session = session
        self.idle_tick_cycles = int(idle_tick_cycles)
        self.idle_sleep_s = float(idle_sleep_s)
        self.lock = threading.RLock()
        self.condition = threading.Condition(self.lock)
        self.paused = False
        self.total_steps = 0
        self.total_batches = 0
        self.last_error: str | None = None
        self.last_stop_reason: str | None = None
        self._reset_generation = 0
        self.started_at = time.time()
        self._stopping = False
        self._thread: threading.Thread | None = None

    def start(self):
        with self.lock:
            if self._thread is not None:
                return
            self.session.boot()
            self._reset_generation += 1
            self._thread = threading.Thread(
                target=self._run_loop,
                name="megapad-shared-machine",
                daemon=True,
            )
            self._thread.start()

    def stop(self):
        with self.condition:
            self._stopping = True
            self.condition.notify_all()
        if self._thread is not None and self._thread is not threading.current_thread():
            self._thread.join(timeout=3.0)
        self.session.close()

    def _run_loop(self):
        while True:
            idle_wait = False
            progress_wait = False
            with self.condition:
                if self._stopping:
                    return
                if self.paused:
                    self.condition.wait(timeout=0.1)
                    continue
                system = self.session.system
                terminal_failure = self.session.rich_terminal_failure
                if terminal_failure is not None:
                    self.last_error = f"TerminalSessionError: {terminal_failure}"
                    self.paused = True
                    continue
                terminal_pending = self.session.rich_terminal_work_pending
                if system.all_halted and not terminal_pending:
                    self.condition.wait(timeout=0.05)
                    continue
                if (
                    system.all_idle_or_halted
                    and not system.uart.has_rx_data
                    and not terminal_pending
                ):
                    idle_wait = True
                else:
                    try:
                        stats = self.session.run_batch_stats(
                            self.session.batch_steps
                        )
                        self.last_stop_reason = stats.system_stop_reason
                        executed = stats.instructions_executed
                        if executed > 0:
                            self.total_steps += executed
                            self.total_batches += 1
                        elif not self.session.last_batch_made_progress:
                            # A bounded host queue can remain legitimately
                            # blocked until a client supplies input or another
                            # runner boundary becomes admissible.  Preserve the
                            # exact stop reason and wait instead of fake-charging
                            # a guest instruction or hot-spinning.
                            progress_wait = True
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True

            if progress_wait:
                with self.condition:
                    self.condition.wait(timeout=self.idle_sleep_s)
            elif idle_wait:
                with self.condition:
                    self.condition.wait(timeout=self.idle_sleep_s)
                    if self._stopping or self.paused:
                        continue
                    system = self.session.system
                    try:
                        system.bus.tick(self.idle_tick_cycles)

                        # Settle wake lines after the shared owner's larger
                        # idle tick. Without this handoff an interrupt can
                        # become pending while every core stays asleep.
                        if system.timer.irq_pending:
                            for cpu in system.cores:
                                if cpu.idle and cpu.flag_i:
                                    cpu.idle = False
                                    break
                        for cpu in system.cores:
                            if cpu.idle and cpu.irq_ipi and cpu.flag_i:
                                cpu.idle = False
                        core0 = system.cores[0]
                        if core0.idle and system._any_nic_rx():
                            core0.idle = False
                    except Exception as exc:
                        self.last_error = f"{type(exc).__name__}: {exc}"
                        self.paused = True
            else:
                time.sleep(0)

    @staticmethod
    def _nearest_label(labels: dict[str, int], address: int) -> dict | None:
        matches = (
            (value, name) for name, value in labels.items() if value <= address
        )
        try:
            value, name = max(matches)
        except ValueError:
            return None
        return {"name": name, "address": value, "offset": address - value}

    def _forth_dictionary(self, cpu) -> tuple[list[dict], int]:
        labels = self.session.bios_labels
        latest_variable = labels.get("var_latest")
        here_variable = labels.get("var_here")
        if latest_variable is None or here_variable is None:
            return [], 0

        words = []
        seen = set()
        try:
            entry = int(cpu.mem_read64(latest_variable))
            here = int(cpu.mem_read64(here_variable))
            while entry and entry not in seen and len(words) < 16_384:
                seen.add(entry)
                flags_len = int(cpu.mem_read8(entry + 8))
                name_len = flags_len & 0x7F
                name = bytes(
                    int(cpu.mem_read8(entry + 9 + index))
                    for index in range(name_len)
                ).decode("ascii", errors="replace")
                code = entry + 9 + name_len
                word = {"name": name, "header": entry, "code": code}
                prefix = bytes(int(cpu.mem_read8(code + index)) for index in range(3))
                suffix = bytes(
                    int(cpu.mem_read8(code + 11 + index)) for index in range(6)
                )
                if prefix == b"\xf0\x60\x10" and suffix == b"\x67\xe0\x08\x54\xe1\x0e":
                    data_address = sum(
                        int(cpu.mem_read8(code + 3 + index)) << (index * 8)
                        for index in range(8)
                    )
                    word["data_address"] = data_address
                    word["value"] = int(cpu.mem_read64(data_address))
                words.append(word)
                entry = int(cpu.mem_read64(entry))
        except (IndexError, RuntimeError, ValueError):
            return words, 0
        return words, here

    @staticmethod
    def _forth_word_at(words: list[dict], here: int, address: int) -> dict | None:
        upper = here
        for word in sorted(words, key=lambda item: item["code"], reverse=True):
            code = word["code"]
            if code <= address < upper:
                return {
                    "name": word["name"],
                    "header": word["header"],
                    "code": code,
                    "offset": address - code,
                }
            upper = code
        return None

    def _forth_diagnostics(self, cpu) -> dict:
        registers = [int(value) for value in cpu.regs]

        def cells(address: int, count: int = 8) -> list[int]:
            values = []
            for index in range(count):
                try:
                    values.append(int(cpu.mem_read64(address + index * 8)))
                except (IndexError, RuntimeError, ValueError):
                    break
            return values

        ip = registers[3]
        labels = self.session.bios_labels
        words, here = self._forth_dictionary(cpu)
        return_stack = cells(registers[15])
        result = {
            "instruction_pointer": ip,
            "data_stack_pointer": registers[14],
            "return_stack_pointer": registers[15],
            "data_stack": cells(registers[14]),
            "return_stack": return_stack,
            "return_words": [
                self._forth_word_at(words, here, address)
                or self._nearest_label(labels, address)
                for address in return_stack
            ],
            "bios_primitive": self._nearest_label(labels, int(cpu.pc)),
            "word": self._forth_word_at(words, here, ip),
        }
        return result

    def forth(self, names: list[str]) -> dict:
        with self.lock:
            words, here = self._forth_dictionary(self.session.system.cpu)
            wanted = {str(name).upper() for name in names}
            found = {}
            for word in words:
                key = word["name"].upper()
                if key in wanted and key not in found:
                    found[key] = word
            return {"here": here, "words": found}

    def peek(self, address: int, count: int = 1) -> dict:
        address = int(address)
        count = int(count)
        if address < 0 or not (1 <= count <= 256):
            raise ValueError("peek requires a non-negative address and 1..256 cells")
        with self.lock:
            cpu = self.session.system.cpu
            return {
                "address": address,
                "cell_size": 8,
                "values": [
                    int(cpu.mem_read64(address + index * 8))
                    for index in range(count)
                ],
            }

    def status(self, *, detailed: bool = True) -> dict:
        """Return machine status.

        Detailed status remains the default for control and diagnostic
        clients.  High-frequency observers such as the session viewer can
        opt out of CPU/Forth/network diagnostics, most notably avoiding a
        complete Forth dictionary walk while holding the machine lock.
        """
        with self.lock:
            system = self.session.system
            cpu = system.cpu
            rich_terminal_failure = self.session.rich_terminal_failure
            rich_terminal_pending = self.session.rich_terminal_work_pending
            quiescent = not system.uart.has_rx_data and not rich_terminal_pending
            operational = rich_terminal_failure is None
            halted = system.all_halted
            idle = system.all_idle_or_halted and quiescent and operational
            visible_cols, visible_rows = self.session.visible_geometry
            if self.session.rich_terminal_lost:
                state = "lost"
            elif rich_terminal_failure is not None:
                state = "terminal_failed"
            elif self.last_error:
                state = "error"
            elif self.paused:
                state = "paused"
            elif halted and not rich_terminal_pending and operational:
                state = "halted"
            elif idle:
                state = "idle"
            elif self.last_stop_reason == "host_backpressure":
                state = "backpressured"
            else:
                state = "running"
            result = {
                "generation": self._reset_generation,
                "state": state,
                "paused": self.paused,
                "halted": halted,
                "idle": idle,
                "stop_reason": self.last_stop_reason,
                "steps": self.total_steps,
                "batches": self.total_batches,
                "revision": self.session.revision,
                "raw_bytes": self.session.raw_output_end,
                "raw_start": self.session.raw_output_start,
                "raw_offset": self.session.raw_output_end,
                "raw_retained_bytes": len(self.session.raw_output),
                "output_batches": self.session.output_batches,
                "byte_callbacks": self.session.output_byte_callbacks,
                "terminal": [visible_cols, visible_rows],
                "uptime_s": time.time() - self.started_at,
                "error": self.last_error,
                "rich_terminal": {
                    "enabled": self.session.rich_terminal_enabled,
                    "display_required": self.session.retained_display_required,
                    "state": (
                        None
                        if self.session.rich_terminal_state is None
                        else self.session.rich_terminal_state.value
                    ),
                    "pending": rich_terminal_pending,
                    "lost": self.session.rich_terminal_lost,
                    "failure": rich_terminal_failure,
                },
            }
            if not detailed:
                return result

            backend = system.nic.backend
            result.update(
                {
                    "cpu": {
                        "pc": cpu.pc,
                        "cycles": cpu.cycle_count,
                        "registers": [int(value) for value in cpu.regs],
                        "psel": cpu.psel,
                        "xsel": cpu.xsel,
                        "spsel": cpu.spsel,
                    },
                    "forth": self._forth_diagnostics(cpu),
                    "clock": {
                        "mode": system.rtc.clock_mode,
                        "uptime_ms": system.rtc.uptime_ms,
                        "epoch_ms": system.rtc.epoch_ms,
                    },
                    "nic": {
                        "backend": system.nic.backend_name,
                        "link_up": system.nic.link_up,
                        "tx_frames": getattr(
                            backend, "tx_frames", system.nic.tx_count
                        ),
                        "rx_frames": getattr(backend, "rx_frames", 0),
                        "rx_queued": cpu._cs.nic_rx_queue_size(),
                    },
                }
            )
            return result

    def network(self) -> dict:
        with self.lock:
            system = self.session.system
            backend = system.nic.backend
            result = {
                "backend": system.nic.backend_name,
                "link_up": system.nic.link_up,
                "guest_tx_frames": system.nic.tx_count,
                "guest_rx_frames": system.cpu._cs.nic_get_rx_count(),
                "guest_rx_queued": system.cpu._cs.nic_rx_queue_size(),
            }
            if backend is not None and hasattr(backend, "stats"):
                result["transport"] = backend.stats()
            return result

    def pause(self) -> dict:
        with self.condition:
            self.paused = True
            self.condition.notify_all()
            return self.status()

    def resume(self) -> dict:
        with self.condition:
            terminal_failure = self.session.rich_terminal_failure
            if terminal_failure is not None or self.session.rich_terminal_lost:
                raise RuntimeError(
                    "rich terminal failure requires a machine reset: "
                    f"{terminal_failure or 'attachment lost'}"
                )
            self.paused = False
            self.last_error = None
            self.condition.notify_all()
            return self.status()

    def step(self, count: int = 1) -> dict:
        count = int(count)
        if count <= 0 or count > 1_000_000:
            raise ValueError("step count must be between 1 and 1000000")
        with self.condition:
            if not self.paused:
                raise RuntimeError("machine must be paused before stepping")
            terminal_failure = self.session.rich_terminal_failure
            if terminal_failure is not None or self.session.rich_terminal_lost:
                self.last_error = (
                    "TerminalSessionError: "
                    f"{terminal_failure or 'rich-terminal attachment lost'}"
                )
                raise RuntimeError(
                    "rich terminal failure requires a machine reset: "
                    f"{terminal_failure or 'attachment lost'}"
                )
            executed = 0
            cycles = 0
            stop_reason = "instruction_limit"
            for _ in range(count):
                if (
                    self.session.system.all_halted
                    and not self.session.rich_terminal_work_pending
                ):
                    stop_reason = "all_halted"
                    break
                try:
                    stats = self.session.run_batch_stats(1)
                except Exception as exc:
                    self.last_error = f"{type(exc).__name__}: {exc}"
                    self.paused = True
                    raise
                stop_reason = stats.system_stop_reason
                cycles += stats.system_cycles_advanced
                executed += stats.instructions_executed
                if stats.instructions_executed == 0:
                    break
            self.last_stop_reason = stop_reason
            self.total_steps += executed
            return {
                "executed": executed,
                "cycles": cycles,
                "stop_reason": stop_reason,
                "status": self.status(),
            }

    def reset(self, *, paused: bool | None = None) -> dict:
        with self.condition:
            if paused is not None and not isinstance(paused, bool):
                raise TypeError("reset paused must be a boolean or null")
            try:
                self.session.reset()
            except Exception as exc:
                self.last_error = f"{type(exc).__name__}: {exc}"
                self.paused = True
                self.condition.notify_all()
                raise
            self.total_steps = 0
            self.total_batches = 0
            self.last_error = None
            self.last_stop_reason = "reset"
            self._reset_generation += 1
            if paused is not None:
                self.paused = paused
            self.condition.notify_all()
            return self.status()

    def send_text(
        self,
        text: str,
        *,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        with self.condition:
            byte_count = len(text.encode("utf-8"))
            if not self._generation_current(generation):
                return {"status": "stale_generation", "accepted_bytes": 0}
            refusal = self._display_input_refusal(
                display_authorized=display_authorized,
                display_lease_ack=display_lease_ack,
                display_request_ack=display_request_ack,
            )
            if refusal is not None:
                return {"status": refusal, "accepted_bytes": 0}
            status = self._terminal_mutation_status(self.session.send_text(text))
            self.condition.notify_all()
            return {
                "status": status.value,
                "accepted_bytes": (
                    byte_count if status is DriverStatus.PROGRESS else 0
                ),
            }

    def send_key(
        self,
        key: str,
        *,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        with self.condition:
            if not self._generation_current(generation):
                return {"status": "stale_generation", "accepted_events": 0}
            refusal = self._display_input_refusal(
                display_authorized=display_authorized,
                display_lease_ack=display_lease_ack,
                display_request_ack=display_request_ack,
            )
            if refusal is not None:
                return {"status": refusal, "accepted_events": 0}
            status = self._terminal_mutation_status(self.session.send_key(key))
            self.condition.notify_all()
            return {
                "status": status.value,
                "accepted_events": 1 if status is DriverStatus.PROGRESS else 0,
            }

    def resize(
        self,
        cols: int,
        rows: int,
        *,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        cols = _wire_integer(cols, "terminal cols", minimum=1)
        rows = _wire_integer(rows, "terminal rows", minimum=1)
        if not self.session.rich_terminal_enabled and not (
            1 <= cols <= 400 and 1 <= rows <= 200
        ):
            raise ValueError("ANSI terminal size must be within 1x1 and 400x200")
        with self.condition:
            current_generation = self._generation_current(generation)
            visible_cols, visible_rows = self.session.visible_geometry
            if not current_generation:
                return {
                    "status": "stale_generation",
                    "accepted": False,
                    "requested": [cols, rows],
                    "cols": visible_cols,
                    "rows": visible_rows,
                    "revision": self.session.revision,
                }
            refusal = self._display_input_refusal(
                display_authorized=display_authorized,
                display_lease_ack=display_lease_ack,
                display_request_ack=display_request_ack,
            )
            if refusal is not None:
                return {
                    "status": refusal,
                    "accepted": False,
                    "requested": [cols, rows],
                    "cols": visible_cols,
                    "rows": visible_rows,
                    "revision": self.session.revision,
                }
            status = self._terminal_mutation_status(self.session.resize(cols, rows))
            visible_cols, visible_rows = self.session.visible_geometry
            self.condition.notify_all()
            return {
                "status": status.value,
                "accepted": status is DriverStatus.PROGRESS,
                "requested": [cols, rows],
                "cols": visible_cols,
                "rows": visible_rows,
                "revision": self.session.revision,
            }

    def _display_input_refusal(
        self,
        *,
        display_authorized: bool,
        display_lease_ack: tuple[int, DisplayScope] | None,
        display_request_ack: tuple[int, DisplayScope] | None,
    ) -> str | None:
        """Gate retained input on the exact physical view this lease ACKed."""

        if not isinstance(display_authorized, bool):
            raise TypeError("display_authorized must be bool")
        if not self.session.retained_display_required:
            return None
        if not display_authorized:
            return "stale_display"
        current_ack = self.session.last_acknowledged_display_offer
        if current_ack is None or display_lease_ack is None:
            return DriverStatus.BACKPRESSURED.value
        if display_request_ack != display_lease_ack or display_lease_ack != current_ack:
            return "stale_display"
        return None

    def _generation_current(self, generation: int | None) -> bool:
        if generation is None:
            return True
        if isinstance(generation, bool):
            raise TypeError("generation must be an integer, not bool")
        try:
            normalized = operator.index(generation)
        except TypeError as exc:
            raise TypeError("generation must be an integer") from exc
        if normalized < 0:
            raise ValueError("generation cannot be negative")
        return normalized == self._reset_generation

    def _terminal_mutation_status(
        self,
        status: DriverStatus | None,
    ) -> DriverStatus:
        normalized = DriverStatus.PROGRESS if status is None else status
        if normalized in {DriverStatus.STALE, DriverStatus.FAILED}:
            reason = self.session.rich_terminal_failure or (
                "rich-terminal attachment became stale"
                if normalized is DriverStatus.STALE
                else "rich terminal failed"
            )
            self.last_error = f"TerminalSessionError: {reason}"
            self.paused = True
        return normalized

    def screen(
        self,
        since: int = -1,
        *,
        since_offer: int = 0,
        display_authorized: bool = False,
    ) -> dict:
        since = _wire_integer(since, "screen since", minimum=-1)
        since_offer = _wire_integer(
            since_offer, "screen since_offer", minimum=0
        )
        if not isinstance(display_authorized, bool):
            raise TypeError("display_authorized must be bool")
        with self.lock:
            revision = self.session.revision
            snapshot = None if since == revision else self.session.snapshot()
            generation = self._reset_generation
            offer = self.session.display_offer if display_authorized else None
            if offer is not None and offer.offer_id == since_offer:
                offer = None

        # Both renderer DTOs are immutable.  Keep the machine lock only for a
        # coherent capture; RLE and rich-plane conversion proceed while the
        # emulator continues running.
        result = {
            "changed": snapshot is not None or offer is not None,
            "revision": revision,
        }
        if snapshot is not None:
            result["snapshot"] = snapshot_to_wire(snapshot)
        if display_authorized:
            result["generation"] = generation
            if offer is not None:
                result["display_offer"] = display_offer_to_wire(offer)
        return result

    def present(
        self,
        offer_id: int,
        scope: DisplayScope,
        *,
        generation: int,
    ) -> dict:
        """Atomically ACK one exact retained-display offer at the machine."""

        offer_id = _wire_integer(offer_id, "display offer id", minimum=1)
        if not isinstance(scope, DisplayScope):
            raise TypeError("scope must be DisplayScope")
        with self.condition:
            if not self._generation_current(generation):
                return {"status": "stale_generation", "presented": False}
            try:
                changed = self.session.acknowledge_display_offer(offer_id, scope)
            except TerminalUpdateError:
                return {"status": "stale_display", "presented": False}
            self.condition.notify_all()
            return {
                "status": "presented" if changed else "duplicate",
                "presented": True,
                "revision": self.session.revision,
            }

    def revoke_physical_display(self) -> bool:
        """Revoke the exact retained sink and wake cadence for a successor."""

        with self.condition:
            changed = self.session.revoke_physical_display()
            self.condition.notify_all()
            return changed

    def text(self, trim_right: bool = True) -> dict:
        with self.lock:
            return {
                "revision": self.session.revision,
                "text": self.session.screen_text(trim_right=trim_right),
            }

    def raw(self, since: int = 0) -> dict:
        with self.lock:
            requested = int(since)
            available_from = self.session.raw_output_start
            offset = self.session.raw_output_end
            start = max(available_from, min(requested, offset))
            data = bytes(self.session.raw_output[start - available_from:])
            return {
                "start": start,
                "available_from": available_from,
                "offset": offset,
                "truncated": requested < available_from,
                "text": data.decode("utf-8", errors="replace"),
                "data_base64": base64.b64encode(data).decode("ascii"),
            }

    def capture(self, params: dict) -> dict:
        with self.lock:
            snapshot = self.session.snapshot()
            outputs = {}
            if params.get("text"):
                snapshot.write_text(params["text"])
                outputs["text"] = str(Path(params["text"]).resolve())
            if params.get("json"):
                snapshot.write_json(params["json"])
                outputs["json"] = str(Path(params["json"]).resolve())
            if params.get("png"):
                snapshot.write_png(
                    params["png"],
                    font_path=params.get("font"),
                    font_size=int(params.get("font_size", 16)),
                )
                outputs["png"] = str(Path(params["png"]).resolve())
            return {"revision": self.session.revision, "outputs": outputs}


class SessionServer:
    """Unix-domain JSON request server for one SharedMachine."""

    def __init__(self, machine: SharedMachine, socket_path: str = DEFAULT_SOCKET):
        self.machine = machine
        self.socket_path = str(Path(socket_path).expanduser())
        self._socket: socket.socket | None = None
        self._stopping = threading.Event()
        self._clients: dict[socket.socket, int] = {}
        self._clients_lock = threading.Lock()
        self._next_connection_id = 1
        self._display_lock = threading.RLock()
        self._display_holder: int | None = None
        self._display_delivered: tuple[int, DisplayScope] | None = None
        self._display_ack: tuple[int, DisplayScope] | None = None
        self._serve_thread: threading.Thread | None = None
        self._socket_owner: RuntimeOwnershipLock | None = None
        self._socket_identity: tuple[int, int] | None = None

    def start(self):
        self._bind()
        try:
            self.machine.start()
        except Exception:
            self._close_owned_listener()
            raise

    def serve_in_thread(self):
        self.start()
        self._serve_thread = threading.Thread(
            target=self.serve_forever,
            name="megapad-session-server",
            daemon=True,
        )
        self._serve_thread.start()

    def _bind(self):
        path = Path(self.socket_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        ownership = RuntimeOwnershipLock.acquire(self.socket_path)
        self._socket_owner = ownership
        server = None
        bound_info = None
        try:
            try:
                existing = os.lstat(path)
            except FileNotFoundError:
                pass
            else:
                self._validate_socket_path(path, existing)
                probe = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                try:
                    probe.connect(self.socket_path)
                except ConnectionRefusedError:
                    if not self._unlink_socket_if_matching(path, existing):
                        raise RuntimeError(
                            f"shared session socket changed during stale "
                            f"recovery: {path}"
                        )
                else:
                    raise RuntimeError(
                        f"shared session already listening at {path}"
                    )
                finally:
                    probe.close()

            server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            server.bind(self.socket_path)
            bound_info = os.lstat(path)
            self._validate_socket_path(path, bound_info)
            os.chmod(self.socket_path, 0o600)
            server.listen(8)
            server.settimeout(0.25)
            info = os.lstat(path)
            self._validate_socket_path(path, info)
            identity = (info.st_dev, info.st_ino)
            bound_identity = (bound_info.st_dev, bound_info.st_ino)
            if identity != bound_identity:
                raise RuntimeError(
                    f"shared session socket changed during bind: {path}"
                )
            self._socket_identity = bound_identity
            self._socket = server
        except Exception:
            if server is not None:
                try:
                    server.close()
                except OSError:
                    pass
            if bound_info is not None:
                self._unlink_socket_if_matching(path, bound_info)
            self._socket_owner = None
            self._socket_identity = None
            ownership.release()
            raise

    @staticmethod
    def _validate_socket_path(path: Path, info: os.stat_result) -> None:
        if not stat.S_ISSOCK(info.st_mode):
            raise RuntimeError(
                f"unsafe shared session path is not a socket: {path}"
            )
        if info.st_uid != os.getuid():
            raise RuntimeError(
                f"unsafe shared session socket is owned by uid {info.st_uid}, "
                f"expected {os.getuid()}: {path}"
            )

    @staticmethod
    def _unlink_socket_if_matching(
        path: Path,
        expected: os.stat_result,
    ) -> bool:
        try:
            current = os.lstat(path)
        except FileNotFoundError:
            return False
        if (current.st_dev, current.st_ino) != (
            expected.st_dev,
            expected.st_ino,
        ):
            return False
        path.unlink()
        return True

    def _close_owned_listener(self) -> bool:
        ownership = self._socket_owner
        if ownership is None:
            return False
        self._socket_owner = None
        try:
            if self._socket is not None:
                try:
                    self._socket.close()
                except OSError:
                    pass
                self._socket = None
            identity = self._socket_identity
            self._socket_identity = None
            if identity is None:
                return False
            path = Path(self.socket_path)
            try:
                current = os.lstat(path)
            except FileNotFoundError:
                return False
            if (current.st_dev, current.st_ino) != identity:
                return False
            path.unlink()
            return True
        finally:
            ownership.release()

    def serve_forever(self):
        if self._socket is None:
            self.start()
        try:
            while not self._stopping.is_set():
                try:
                    client, _ = self._socket.accept()
                except socket.timeout:
                    continue
                except OSError:
                    break
                with self._clients_lock:
                    connection_id = self._next_connection_id
                    self._next_connection_id += 1
                    self._clients[client] = connection_id
                threading.Thread(
                    target=self._handle_client,
                    args=(client, connection_id),
                    daemon=True,
                    name="megapad-session-client",
                ).start()
        finally:
            self.stop()

    def _handle_client(self, client: socket.socket, connection_id: int):
        try:
            reader = client.makefile("rb")
            while not self._stopping.is_set():
                line = reader.readline(MAX_REQUEST_BYTES + 1)
                if not line:
                    break
                if len(line) > MAX_REQUEST_BYTES:
                    self._send(client, {"id": None, "ok": False, "error": "request too large"})
                    break
                request = None
                try:
                    request = json.loads(line)
                    result = self.dispatch(
                        request.get("method"),
                        request.get("params") or {},
                        connection_id=connection_id,
                    )
                    response = {"id": request.get("id"), "ok": True, "result": result}
                except Exception as exc:
                    response = {
                        "id": request.get("id") if isinstance(request, dict) else None,
                        "ok": False,
                        "error": f"{type(exc).__name__}: {exc}",
                    }
                self._send(client, response)
        finally:
            try:
                self._release_display_holder(connection_id)
            finally:
                with self._clients_lock:
                    self._clients.pop(client, None)
                try:
                    client.close()
                except OSError:
                    pass

    @staticmethod
    def _send(client: socket.socket, response: dict):
        payload = json.dumps(response, ensure_ascii=False, separators=(",", ":"))
        client.sendall(payload.encode("utf-8") + b"\n")

    @staticmethod
    def _required_generation(params: dict) -> int:
        if "generation" not in params:
            raise ValueError("mutating input request requires generation")
        value = params["generation"]
        if isinstance(value, bool):
            raise TypeError("generation must be an integer, not bool")
        try:
            generation = operator.index(value)
        except TypeError as exc:
            raise TypeError("generation must be an integer") from exc
        if generation < 0:
            raise ValueError("generation cannot be negative")
        return int(generation)

    @staticmethod
    def _required_display_pair(params: Mapping[str, Any]) -> tuple[int, DisplayScope]:
        if "display_offer_id" not in params or "display_scope" not in params:
            raise ValueError(
                "display request requires display_offer_id and display_scope"
            )
        return (
            _wire_integer(
                params["display_offer_id"], "display_offer_id", minimum=1
            ),
            display_scope_from_wire(params["display_scope"]),
        )

    @classmethod
    def _optional_display_pair(
        cls,
        params: Mapping[str, Any],
    ) -> tuple[int, DisplayScope] | None:
        has_id = "display_offer_id" in params
        has_scope = "display_scope" in params
        if not has_id and not has_scope:
            return None
        if has_id != has_scope:
            raise ValueError(
                "display proof requires both display_offer_id and display_scope"
            )
        return cls._required_display_pair(params)

    def _claim_display(self, connection_id: int | None) -> dict:
        if connection_id is None:
            raise ValueError("claim_display requires a live client connection")
        normalized = _wire_integer(
            connection_id, "connection identity", minimum=1
        )
        with self._display_lock:
            if self._stopping.is_set():
                return {"status": "stopping", "claimed": False}
            holder = self._display_holder
            if holder is None:
                self._display_holder = normalized
                self._display_delivered = None
                self._display_ack = None
                return {"status": "claimed", "claimed": True}
            if holder == normalized:
                return {"status": "claimed", "claimed": True}
            return {"status": "display_busy", "claimed": False}

    def _release_display_holder(self, connection_id: int) -> bool:
        """Drop one exact lease and requeue all of its physical sink state."""

        normalized = _wire_integer(
            connection_id, "connection identity", minimum=1
        )
        with self._display_lock:
            if self._display_holder != normalized:
                return False
            try:
                return self.machine.revoke_physical_display()
            finally:
                self._display_holder = None
                self._display_delivered = None
                self._display_ack = None

    def _screen_for_connection(
        self,
        params: Mapping[str, Any],
        connection_id: int | None,
    ) -> dict:
        with self._display_lock:
            authorized = (
                connection_id is not None
                and self._display_holder == connection_id
            )
            result = self.machine.screen(
                params.get("since", -1),
                since_offer=params.get("since_offer", 0),
                display_authorized=authorized,
            )
            offer = result.get("display_offer")
            if authorized and offer is not None:
                self._display_delivered = (
                    _wire_integer(
                        offer["offer_id"], "display offer id", minimum=1
                    ),
                    display_scope_from_wire(offer["scope"]),
                )
            return result

    def _present_for_connection(
        self,
        params: Mapping[str, Any],
        connection_id: int | None,
    ) -> dict:
        generation = self._required_generation(params)
        pair = self._required_display_pair(params)
        with self._display_lock:
            if connection_id is None or self._display_holder != connection_id:
                return {"status": "stale_display", "presented": False}
            if pair != self._display_delivered:
                return {"status": "stale_display", "presented": False}
            result = self.machine.present(
                pair[0],
                pair[1],
                generation=generation,
            )
            if result["status"] in {"presented", "duplicate"}:
                self._display_ack = pair
            return result

    def _dispatch_terminal_input(
        self,
        method: str,
        params: Mapping[str, Any],
        connection_id: int | None,
    ) -> dict:
        generation = self._required_generation(params)
        request_ack = self._optional_display_pair(params)
        with self._display_lock:
            authorized = (
                connection_id is not None
                and self._display_holder == connection_id
            )
            common = {
                "generation": generation,
                "display_authorized": authorized,
                "display_lease_ack": self._display_ack if authorized else None,
                "display_request_ack": request_ack,
            }
            if method == "send_text":
                return self.machine.send_text(str(params.get("text", "")), **common)
            if method == "send_key":
                return self.machine.send_key(str(params["key"]), **common)
            assert method == "resize"
            return self.machine.resize(params["cols"], params["rows"], **common)

    def dispatch(
        self,
        method: str,
        params: dict,
        *,
        connection_id: int | None = None,
    ) -> Any:
        if method == "ping":
            return {"time": time.time()}
        if method == "status":
            detailed = params.get("detailed", True)
            if not isinstance(detailed, bool):
                raise ValueError("status detailed must be a boolean")
            result = self.machine.status(detailed=detailed)
            with self._clients_lock:
                result["clients"] = len(self._clients)
            return result
        if method == "network":
            return self.machine.network()
        if method == "forth":
            names = params.get("names") or []
            if not isinstance(names, list) or len(names) > 64:
                raise ValueError("forth names must be a list of at most 64 items")
            return self.machine.forth(names)
        if method == "peek":
            return self.machine.peek(params["address"], params.get("count", 1))
        if method == "pause":
            return self.machine.pause()
        if method == "resume":
            return self.machine.resume()
        if method == "step":
            return self.machine.step(params.get("count", 1))
        if method == "reset":
            with self._display_lock:
                result = self.machine.reset(paused=params.get("paused"))
                self._display_delivered = None
                self._display_ack = None
                return result
        if method == "claim_display":
            return self._claim_display(connection_id)
        if method == "present":
            return self._present_for_connection(params, connection_id)
        if method in {"send_text", "send_key", "resize"}:
            return self._dispatch_terminal_input(method, params, connection_id)
        if method == "screen":
            return self._screen_for_connection(params, connection_id)
        if method == "text":
            return self.machine.text(bool(params.get("trim_right", True)))
        if method == "raw":
            return self.machine.raw(params.get("since", 0))
        if method == "capture":
            return self.machine.capture(params)
        if method == "shutdown":
            timer = threading.Timer(0.05, self.stop)
            timer.daemon = True
            timer.start()
            return {"stopping": True}
        raise ValueError(f"unknown method: {method!r}")

    def stop(self):
        if self._stopping.is_set():
            return
        self._stopping.set()
        self._close_owned_listener()
        with self._clients_lock:
            clients = list(self._clients)
            self._clients.clear()
        try:
            with self._display_lock:
                try:
                    if self._display_holder is not None:
                        self.machine.revoke_physical_display()
                finally:
                    self._display_holder = None
                    self._display_delivered = None
                    self._display_ack = None
        finally:
            for client in clients:
                try:
                    client.close()
                except OSError:
                    pass
            self.machine.stop()


class SessionClient:
    """Thread-safe request client for the local shared-session socket."""

    def __init__(self, socket_path: str = DEFAULT_SOCKET, timeout: float = 5.0):
        self.socket_path = str(Path(socket_path).expanduser())
        self.timeout = float(timeout)
        self._socket: socket.socket | None = None
        self._reader = None
        self._lock = threading.Lock()
        self._next_id = 1

    def connect(self):
        if self._socket is not None:
            return
        client = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        client.settimeout(self.timeout)
        client.connect(self.socket_path)
        self._socket = client
        self._reader = client.makefile("rb")

    def close(self):
        if self._reader is not None:
            self._reader.close()
            self._reader = None
        if self._socket is not None:
            self._socket.close()
            self._socket = None

    def __enter__(self) -> "SessionClient":
        self.connect()
        return self

    def __exit__(self, exc_type, exc, traceback):
        self.close()

    def request(self, method: str, **params):
        with self._lock:
            self.connect()
            request_id = self._next_id
            self._next_id += 1
            request = {"id": request_id, "method": method, "params": params}
            payload = json.dumps(request, ensure_ascii=False, separators=(",", ":"))
            self._socket.sendall(payload.encode("utf-8") + b"\n")
            line = self._reader.readline()
            if not line:
                self.close()
                raise ConnectionError("shared session closed the connection")
            response = json.loads(line)
            if response.get("id") != request_id:
                raise RuntimeError("shared session response id mismatch")
            if not response.get("ok"):
                raise RuntimeError(response.get("error", "shared session request failed"))
            return response.get("result")
