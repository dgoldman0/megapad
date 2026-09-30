"""Shared single-owner MegaPad runtime and local JSON control protocol."""

from __future__ import annotations

from abc import ABC, abstractmethod

import base64
import binascii
import json
import operator
import os
import socket
import stat
import threading
import time
from collections.abc import Mapping
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from rich_terminal import DriverStatus
from rich_terminal.apt1 import UINT32_MAX, UINT64_MAX
from rich_terminal.retained_view import (
    INT32_MAX,
    INT32_MIN,
    INT64_MAX,
    INT64_MIN,
    DisplayScope,
    GlyphRunDraw,
    ImageDraw,
    ItemViewDraw,
    ImageResourceManifest,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    MeterDraw,
    PlotDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    SeriesHistoryDraw,
    StatusDraw,
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
    WaveformDraw,
    retained_draw_key,
    retained_draw_order,
)
from rich_terminal.retained_scene import (
    ControlKind,
    ControlState,
    ImageFit,
    ObjectBounds,
    Point,
    RGBA,
    Sample,
    validate_control_shape,
)
from rich_terminal.retained_resources import RGBAResource
from rich_terminal.semantic_content import (
    SemanticTextContent,
    decode_semantic_text_content,
    encode_semantic_text_content,
)
from rich_terminal.semantic_items import (
    ItemViewContent,
    decode_item_view_content,
    encode_item_view_content,
)
from rich_terminal.update_authority import TerminalUpdateError
from rich_terminal.retained_wire import ControlEventKind
from display import ATTR_CONTINUATION, ATTR_WIDE
from runtime_paths import RuntimeOwnershipLock, shared_session_socket
from shared.session import (
    TerminalSession,
    TerminalCell,
    TerminalDisplayOffer,
    TerminalSnapshot,
)


DEFAULT_SOCKET = shared_session_socket()
MAX_REQUEST_BYTES = 1 << 20

_PHASE_EVENT_PHASE_MASK = 0xFF
_PHASE_EVENT_SEQUENCE_SHIFT = 8
_PHASE_PROFILE_SCHEMA = "megapad.guest-phase-events"
_PHASE_PROFILE_MAX_EVENTS = 65_536


@dataclass
class _PhaseEventProfile:
    address: int
    max_events: int
    machine_generation: int
    batch_step_bound: int | None
    started_steps: int
    started_batches: int
    initial_event: int
    last_event: int
    last_sample_steps: int
    last_sample_batches: int
    status: str = "active"
    sample_attempts: int = 1
    successful_samples: int = 1
    observed_transitions: int = 0
    coalesced_transitions: int = 0
    dropped_records: int = 0
    dropped_transitions: int = 0
    stopped_steps: int | None = None
    stopped_batches: int | None = None
    error: dict[str, str] | None = None
    transitions: list[dict[str, Any]] = field(default_factory=list)


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



_DISPLAY_INPUT_FIELDS = ("generation", "display_offer_id", "display_scope")
_CONTROL_TARGET_FIELDS = ("owner_id", "owner_generation", "control_id", "modifiers")
_CONTROL_INPUT_FIELDS = _DISPLAY_INPUT_FIELDS + _CONTROL_TARGET_FIELDS
# One exact field set per positioned CONTROL_EVENT kind, mirroring its tail.
_TEXT_EVENT_FIELDS = {
    int(ControlEventKind.PLACE): _CONTROL_INPUT_FIELDS
    + ("event_kind", "content_revision", "item_key", "scalar_offset"),
    int(ControlEventKind.EXTEND): _CONTROL_INPUT_FIELDS
    + ("event_kind", "content_revision", "item_key", "scalar_offset"),
    int(ControlEventKind.FOLLOW): _CONTROL_INPUT_FIELDS
    + ("event_kind", "content_revision", "item_key", "scalar_offset"),
    int(ControlEventKind.SCROLL): _CONTROL_INPUT_FIELDS
    + ("event_kind", "wheel_x", "wheel_y"),
}
# Item events name one item and carry no offset.
_TEXT_EVENT_FIELDS.update(
    {
        int(kind): _CONTROL_INPUT_FIELDS + ("event_kind", "content_revision", "item_key")
        for kind in (
            ControlEventKind.SELECT,
            ControlEventKind.OPEN,
            ControlEventKind.EXPAND,
            ControlEventKind.COLLAPSE,
            ControlEventKind.CHECK,
        )
    }
)
_POINTER_INPUT_FIELDS = _DISPLAY_INPUT_FIELDS + (
    "x",
    "y",
    "buttons",
    "modifiers",
    "kind",
    "wheel_x",
    "wheel_y",
)
# Pointer and control input name positions in one acknowledged frame, so the
# request must carry that frame's display proof; keys and text need not.
_DISPLAY_BOUND_INPUT_METHODS = frozenset(
    ("send_control_event", "send_text_event", "send_pointer")
)


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


def _wire_sha3_256(value, name: str) -> bytes:
    """Decode one canonical lowercase SHA3-256 hex identity."""

    encoded = _wire_text(value, name)
    if len(encoded) != 64 or encoded != encoded.lower():
        raise ValueError(f"{name} must be 64 lowercase hexadecimal characters")
    try:
        digest = bytes.fromhex(encoded)
    except ValueError as exc:
        raise ValueError(
            f"{name} must be 64 lowercase hexadecimal characters"
        ) from exc
    if len(digest) != 32 or digest.hex() != encoded:
        raise ValueError(f"{name} must be canonical lowercase SHA3-256 hex")
    return digest


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


def _row_runs(row) -> tuple[tuple[int, tuple], ...]:
    runs: list[tuple[int, tuple]] = []
    current = None
    count = 0
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
            runs.append((count, current))
        current = value
        count = 1
    if current is not None:
        runs.append((count, current))
    return tuple(runs)


class WireRowRuns:
    """The runs of each row of the last snapshot encoded with this memo.

    Renderer snapshots share every unchanged row, an immutable tuple of
    immutable cells, between offers.  A row object met in the previous
    snapshot therefore has exactly the runs recorded for it then, and only
    new rows are encoded.  Entries are keyed by row identity and hold their
    rows, so no other object can share a key while its entry exists, and a
    matching key is always the same row.  Each call replaces the whole memo,
    so concurrent callers can only lose entries, never share a wrong one.
    """

    __slots__ = ("_rows",)

    def __init__(self) -> None:
        self._rows: dict[int, tuple[tuple, tuple[tuple[int, tuple], ...]]] = {}

    def runs(self, rows) -> list[tuple[tuple[int, tuple], ...]]:
        previous = self._rows
        current: dict[int, tuple[tuple, tuple[tuple[int, tuple], ...]]] = {}
        result = []
        for row in rows:
            key = id(row)
            entry = current.get(key) or previous.get(key)
            if entry is None:
                entry = (row, _row_runs(row))
            current[key] = entry
            result.append(entry[1])
        self._rows = current
        return result


def snapshot_to_wire(
    snapshot: TerminalSnapshot,
    rows: WireRowRuns | None = None,
) -> dict:
    """Run-length encode a terminal snapshot for the local viewer protocol.

    Runs continue across row ends.  ``rows`` keeps each row's runs between
    snapshots that share rows.
    """
    runs: list[list[Any]] = []
    last = None
    for row_runs in (rows if rows is not None else WireRowRuns()).runs(
        snapshot.cells
    ):
        for count, value in row_runs:
            if value == last:
                runs[-1][0] += count
            else:
                runs.append([count, *value])
                last = value
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


def _snapshot_cursor_from_wire(
    wire: Mapping[str, Any],
    cols: int,
    rows: int,
) -> tuple[int, int, bool, bool]:
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
    return cursor_row, cursor_col, cursor_visible, alternate_screen


def _snapshot_run_from_wire(run, name: str) -> tuple[int, TerminalCell]:
    if not isinstance(run, (list, tuple)) or len(run) != 5:
        raise TypeError(f"{name} must be a five-item array")
    count = _wire_integer(run[0], f"{name} count", minimum=1)
    char = _wire_text(run[1], f"{name} char")
    fg = _wire_integer(run[2], f"{name} foreground", minimum=0, maximum=0xFFFFFF)
    bg = _wire_integer(run[3], f"{name} background", minimum=0, maximum=0xFFFFFF)
    attrs = _wire_integer(run[4], f"{name} attrs", minimum=0, maximum=0x3FF)
    # A lead cell shows one whole character, which may hold several
    # scalars; the continuation of a wide character shows none.
    if attrs & ATTR_CONTINUATION:
        if char or attrs & ATTR_WIDE:
            raise ValueError(f"{name} continuation must be empty and not wide")
    elif not char:
        raise ValueError(f"{name} char must not be empty")
    return count, TerminalCell(
        char=char,
        fg=_rgb_unpack(fg),
        bg=_rgb_unpack(bg),
        attrs=attrs,
    )


def _snapshot_row_pairs_valid(row: tuple[TerminalCell, ...], row_index: int) -> None:
    cols = len(row)
    for column, cell in enumerate(row):
        wide = cell.attrs & ATTR_WIDE
        if wide and (
            column + 1 == cols or not row[column + 1].attrs & ATTR_CONTINUATION
        ) or cell.attrs & ATTR_CONTINUATION and (
            column == 0 or not row[column - 1].attrs & ATTR_WIDE
        ):
            raise ValueError(
                f"snapshot row {row_index} column {column} breaks a wide pair"
            )


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
    cursor_row, cursor_col, cursor_visible, alternate_screen = (
        _snapshot_cursor_from_wire(wire, cols, rows)
    )

    runs = wire["runs"]
    if not isinstance(runs, (list, tuple)):
        raise TypeError("snapshot runs must be an array")
    flat: list[TerminalCell] = []
    for index, run in enumerate(runs):
        count, cell = _snapshot_run_from_wire(run, f"snapshot run {index}")
        if len(flat) + count > expected:
            raise ValueError("snapshot runs exceed the declared geometry")
        flat.extend([cell] * count)
    if len(flat) != expected:
        raise ValueError(f"snapshot has {len(flat)} cells, expected {expected}")
    cells = tuple(
        tuple(flat[row * cols:(row + 1) * cols])
        for row in range(rows)
    )
    for row_index, row in enumerate(cells):
        _snapshot_row_pairs_valid(row, row_index)
    return TerminalSnapshot(
        cols=cols,
        rows=rows,
        cells=cells,
        cursor_col=cursor_col,
        cursor_row=cursor_row,
        cursor_visible=cursor_visible,
        alternate_screen=alternate_screen,
    )


def _snapshot_changes_to_wire(
    snapshot: TerminalSnapshot,
    base: TerminalSnapshot,
    rows: WireRowRuns | None,
) -> dict:
    """The rows of ``snapshot`` that differ from ``base``, each run-length
    encoded on its own."""

    by_row = None if rows is None else rows.runs(snapshot.cells)
    changed = []
    for index, (row, previous) in enumerate(zip(snapshot.cells, base.cells)):
        if row is previous or row == previous:
            continue
        row_runs = _row_runs(row) if by_row is None else by_row[index]
        changed.append([index, [[count, *value] for count, value in row_runs]])
    return {
        "cols": snapshot.cols,
        "rows": snapshot.rows,
        "cursor": [
            snapshot.cursor_row,
            snapshot.cursor_col,
            snapshot.cursor_visible,
        ],
        "alternate_screen": snapshot.alternate_screen,
        "changed_rows": changed,
    }


def _snapshot_changes_from_wire(data, base: TerminalSnapshot) -> TerminalSnapshot:
    """Rebuild a snapshot from its changed rows and the base's other rows."""

    wire = _wire_object(
        data,
        "snapshot changes",
        ("cols", "rows", "cursor", "alternate_screen", "changed_rows"),
    )
    cols = _wire_integer(wire["cols"], "snapshot cols", minimum=1)
    rows = _wire_integer(wire["rows"], "snapshot rows", minimum=1)
    if (cols, rows) != (base.cols, base.rows):
        raise ValueError("snapshot changes do not have their base's geometry")
    cursor_row, cursor_col, cursor_visible, alternate_screen = (
        _snapshot_cursor_from_wire(wire, cols, rows)
    )
    changed = wire["changed_rows"]
    if not isinstance(changed, (list, tuple)):
        raise TypeError("snapshot changed rows must be an array")
    cells = list(base.cells)
    previous = -1
    for position, item in enumerate(changed):
        if not isinstance(item, (list, tuple)) or len(item) != 2:
            raise TypeError(f"snapshot changed row {position} must be a two-item array")
        row_index = _wire_integer(
            item[0],
            f"snapshot changed row {position} index",
            minimum=0,
            maximum=rows - 1,
        )
        if row_index <= previous:
            raise ValueError("snapshot changed rows must be in increasing order")
        previous = row_index
        runs = item[1]
        if not isinstance(runs, (list, tuple)):
            raise TypeError(f"snapshot row {row_index} runs must be an array")
        row: list[TerminalCell] = []
        for index, run in enumerate(runs):
            count, cell = _snapshot_run_from_wire(
                run, f"snapshot row {row_index} run {index}"
            )
            if len(row) + count > cols:
                raise ValueError(f"snapshot row {row_index} runs exceed its columns")
            row.extend([cell] * count)
        if len(row) != cols:
            raise ValueError(
                f"snapshot row {row_index} has {len(row)} cells, expected {cols}"
            )
        cells[row_index] = tuple(row)
        _snapshot_row_pairs_valid(cells[row_index], row_index)
    return TerminalSnapshot(
        cols=cols,
        rows=rows,
        cells=tuple(cells),
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


_GLYPH_RUN_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "foreground",
    "background",
    "attributes",
    "text",
)
_POLYLINE_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "points",
    "stroke_width",
    "color",
    "closed",
)
_IMAGE_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "resource_id",
    "fit",
    "opacity",
)
_READOUT_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "foreground",
    "background",
    "text",
)
_METER_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "foreground",
    "background",
    "vertical",
    "show_value",
    "minimum",
    "maximum",
    "value",
)
_STATUS_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "inactive",
    "active",
    "value",
    "shape",
)
_PLOT_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "series_id",
    "minimum",
    "maximum",
    "line",
    "fill",
    "fill_to_minimum",
    "draw_points",
)
_WAVEFORM_WIRE_FIELDS = (
    "kind",
    "object_id",
    "z_order",
    "bounds",
    "parent_bounds",
    "series_id",
    "minimum",
    "maximum",
    "trace",
    "zero_line",
    "zero_value",
    "draw_zero_line",
)
_SERIES_HISTORY_WIRE_FIELDS = (
    "owner_id",
    "owner_generation",
    "series_id",
    "samples",
)
_IMAGE_RESOURCE_WIRE_FIELDS = (
    "owner_id",
    "owner_generation",
    "resource_id",
    "format",
    "width",
    "height",
    "byte_length",
    "sha3_256",
)
_MENU_BAR_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "z_order",
    "bounds",
    "menus",
)
_MENU_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "label",
    "entries",
)
_MENU_ITEM_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "label",
    "shortcut",
)
_MENU_SEPARATOR_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
)
_TEXT_COLLECTION_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "z_order",
    "bounds",
    "content_stx1_base64",
)
_ITEM_VIEW_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "z_order",
    "bounds",
    "content_itm1_base64",
)
_TABSET_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "z_order",
    "bounds",
    "tabs",
)
_TAB_WIRE_FIELDS = (
    "kind",
    "control_id",
    "state",
    "order",
    "label",
    "shortcut",
)
_REGION_HEADER_FIELDS = (
    "owner_id",
    "owner_generation",
    "region_id",
    "logical_x",
    "logical_y",
    "logical_cols",
    "logical_rows",
    "clip_x",
    "clip_y",
    "clip_cols",
    "clip_rows",
    "z_order",
    "clipped",
)
_REGION_WIRE_FIELDS = _REGION_HEADER_FIELDS + ("draws",)
# A region carried as changes against the base region with its identity.
_REGION_CHANGE_FIELDS = _REGION_HEADER_FIELDS + ("removed", "changed")


def _semantic_content_to_wire(content: SemanticTextContent) -> str:
    """Carry the one canonical STX1 schema through JSON without restating it."""

    payload = encode_semantic_text_content(content)
    return base64.b64encode(payload).decode("ascii")


def _item_content_to_wire(content: ItemViewContent) -> str:
    """Carry the one canonical ITM1 schema through JSON without restating it."""

    return base64.b64encode(encode_item_view_content(content)).decode("ascii")


def _bounds_to_wire(bounds: ObjectBounds) -> list[int]:
    if not isinstance(bounds, ObjectBounds):
        raise TypeError("bounds must be ObjectBounds")
    return [
        bounds.cell_x,
        bounds.cell_y,
        bounds.cell_cols,
        bounds.cell_rows,
    ]


def _bounds_path_to_wire(bounds_path: tuple[ObjectBounds, ...]) -> list[list[int]]:
    return [_bounds_to_wire(bounds) for bounds in bounds_path]


def _bounds_path_from_wire(value, name: str) -> tuple[ObjectBounds, ...]:
    if not isinstance(value, (list, tuple)):
        raise TypeError(f"{name} must be an array")
    return tuple(
        ObjectBounds(
            *_wire_integer_array(item, f"{name}[{index}]", 4)
        )
        for index, item in enumerate(value)
    )


def _series_history_to_wire(history: SeriesHistoryDraw) -> dict:
    if not isinstance(history, SeriesHistoryDraw):
        raise TypeError("history must be SeriesHistoryDraw")
    return {
        "owner_id": history.owner_id,
        "owner_generation": history.owner_generation,
        "series_id": history.series_id,
        "samples": [
            [sample.timestamp_us, sample.value] for sample in history.samples
        ],
    }


def _sample_from_wire(value, name: str) -> Sample:
    if not isinstance(value, (list, tuple)) or len(value) != 2:
        raise TypeError(f"{name} must be a two-integer array")
    return Sample(
        _wire_integer(
            value[0], f"{name} timestamp_us", minimum=0, maximum=UINT64_MAX
        ),
        _wire_integer(
            value[1], f"{name} value", minimum=INT64_MIN, maximum=INT64_MAX
        ),
    )


def _series_history_from_wire(data, name: str) -> SeriesHistoryDraw:
    wire = _wire_object(data, name, _SERIES_HISTORY_WIRE_FIELDS)
    samples_wire = wire["samples"]
    if not isinstance(samples_wire, (list, tuple)):
        raise TypeError(f"{name} samples must be an array")
    return SeriesHistoryDraw(
        owner_id=_wire_integer(
            wire["owner_id"], f"{name} owner_id", minimum=1, maximum=UINT64_MAX
        ),
        owner_generation=_wire_integer(
            wire["owner_generation"],
            f"{name} owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        series_id=_wire_integer(
            wire["series_id"], f"{name} series_id", minimum=1, maximum=UINT64_MAX
        ),
        samples=tuple(
            _sample_from_wire(sample, f"{name} sample {index}")
            for index, sample in enumerate(samples_wire)
        ),
    )


def _image_resource_to_wire(resource: ImageResourceManifest) -> dict:
    if not isinstance(resource, ImageResourceManifest):
        raise TypeError("resource must be ImageResourceManifest")
    return {
        "owner_id": resource.owner_id,
        "owner_generation": resource.owner_generation,
        "resource_id": resource.resource_id,
        "format": int(resource.format),
        "width": resource.width,
        "height": resource.height,
        "byte_length": resource.byte_length,
        "sha3_256": resource.sha3_256.hex(),
    }


def _image_resource_from_wire(data, name: str) -> ImageResourceManifest:
    wire = _wire_object(data, name, _IMAGE_RESOURCE_WIRE_FIELDS)
    return ImageResourceManifest(
        owner_id=_wire_integer(
            wire["owner_id"], f"{name} owner_id", minimum=1, maximum=UINT64_MAX
        ),
        owner_generation=_wire_integer(
            wire["owner_generation"],
            f"{name} owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        resource_id=_wire_integer(
            wire["resource_id"],
            f"{name} resource_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        format=_wire_integer(
            wire["format"], f"{name} format", minimum=1, maximum=0xFF
        ),
        width=_wire_integer(
            wire["width"], f"{name} width", minimum=1, maximum=UINT32_MAX
        ),
        height=_wire_integer(
            wire["height"], f"{name} height", minimum=1, maximum=UINT32_MAX
        ),
        byte_length=_wire_integer(
            wire["byte_length"],
            f"{name} byte_length",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        sha3_256=_wire_sha3_256(wire["sha3_256"], f"{name} sha3_256"),
    )


def _canonical_base64(value, name: str) -> bytes:
    encoded = _wire_text(value, name)
    try:
        ascii_payload = encoded.encode("ascii", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} must be canonical base64 ASCII") from exc
    try:
        payload = base64.b64decode(ascii_payload, validate=True)
    except (binascii.Error, ValueError) as exc:
        raise ValueError(f"{name} must be canonical base64") from exc
    if base64.b64encode(payload).decode("ascii") != encoded:
        raise ValueError(f"{name} must use canonical base64 padding")
    return payload


def _item_content_from_wire(value, name: str) -> ItemViewContent:
    payload = _canonical_base64(value, name)
    try:
        return decode_item_view_content(payload)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} is not canonical ITM1: {exc}") from exc


def _semantic_content_from_wire(value, name: str) -> SemanticTextContent:
    encoded = _wire_text(value, name)
    try:
        ascii_payload = encoded.encode("ascii", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} must be canonical base64 ASCII") from exc
    try:
        payload = base64.b64decode(ascii_payload, validate=True)
    except (binascii.Error, ValueError) as exc:
        raise ValueError(f"{name} must be canonical base64") from exc
    if base64.b64encode(payload).decode("ascii") != encoded:
        raise ValueError(f"{name} must use canonical base64 padding")
    try:
        return decode_semantic_text_content(payload)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} is not canonical STX1: {exc}") from exc


def _validate_collection_draw_shape(
    kind: ControlKind,
    state: ControlState,
    order: int,
    z_order: int,
    bounds: ObjectBounds,
    content: SemanticTextContent | ItemViewContent,
) -> None:
    """Reassert family rules from immutable O(1) content summaries."""

    validate_control_shape(
        kind=kind,
        state=state,
        z_order=z_order,
        parent_control_id=0,
        order=order,
        bounds=bounds,
        label="",
        shortcut="",
        content=content,
    )


def _tab_to_wire(tab: TabDraw) -> dict:
    if not isinstance(tab, TabDraw):
        raise TypeError("tab must be TabDraw")
    return {
        "kind": "tab",
        "control_id": tab.control_id,
        "state": int(tab.state),
        "order": tab.order,
        "label": tab.label,
        "shortcut": tab.shortcut,
    }


def _menu_entry_to_wire(entry: MenuItemDraw | MenuSeparatorDraw) -> dict:
    if isinstance(entry, MenuItemDraw):
        return {
            "kind": "menu_item",
            "control_id": entry.control_id,
            "state": int(entry.state),
            "order": entry.order,
            "label": entry.label,
            "shortcut": entry.shortcut,
        }
    if isinstance(entry, MenuSeparatorDraw):
        return {
            "kind": "menu_separator",
            "control_id": entry.control_id,
            "state": int(entry.state),
            "order": entry.order,
        }
    raise TypeError("menu entries must be MenuItemDraw or MenuSeparatorDraw")


def _menu_to_wire(menu: MenuDraw) -> dict:
    if not isinstance(menu, MenuDraw):
        raise TypeError("menu must be MenuDraw")
    return {
        "kind": "menu",
        "control_id": menu.control_id,
        "state": int(menu.state),
        "order": menu.order,
        "label": menu.label,
        "entries": [_menu_entry_to_wire(entry) for entry in menu.entries],
    }


def _retained_draw_to_wire(
    draw: (
        GlyphRunDraw
        | PolylineDraw
        | ImageDraw
        | ReadoutDraw
        | MeterDraw
        | StatusDraw
        | PlotDraw
        | WaveformDraw
        | MenuBarDraw
        | TextAreaDraw
        | TextGridDraw
        | TabSetDraw
    ),
) -> dict:
    if isinstance(draw, GlyphRunDraw):
        return {
            "kind": "glyph_run",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
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
    if isinstance(draw, PolylineDraw):
        return {
            "kind": "polyline",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
            "points": [[point.x, point.y] for point in draw.points],
            "stroke_width": draw.stroke_width,
            "color": [
                draw.color.red,
                draw.color.green,
                draw.color.blue,
                draw.color.alpha,
            ],
            "closed": draw.closed,
        }
    if isinstance(draw, ImageDraw):
        return {
            "kind": "image",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
            "resource_id": draw.resource_id,
            "fit": int(draw.fit),
            "opacity": draw.opacity,
        }
    if isinstance(draw, ReadoutDraw):
        return {
            "kind": "readout",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
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
            "text": draw.text,
        }
    if isinstance(draw, MeterDraw):
        return {
            "kind": "meter",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
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
            "vertical": draw.vertical,
            "show_value": draw.show_value,
            "minimum": draw.minimum,
            "maximum": draw.maximum,
            "value": draw.value,
        }
    if isinstance(draw, StatusDraw):
        return {
            "kind": "status",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
            "inactive": [
                draw.inactive.red,
                draw.inactive.green,
                draw.inactive.blue,
                draw.inactive.alpha,
            ],
            "active": [
                draw.active.red,
                draw.active.green,
                draw.active.blue,
                draw.active.alpha,
            ],
            "value": draw.value,
            "shape": draw.shape,
        }
    if isinstance(draw, PlotDraw):
        return {
            "kind": "plot",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
            "series_id": draw.series_id,
            "minimum": draw.minimum,
            "maximum": draw.maximum,
            "line": [draw.line.red, draw.line.green, draw.line.blue, draw.line.alpha],
            "fill": [draw.fill.red, draw.fill.green, draw.fill.blue, draw.fill.alpha],
            "fill_to_minimum": draw.fill_to_minimum,
            "draw_points": draw.draw_points,
        }
    if isinstance(draw, WaveformDraw):
        return {
            "kind": "waveform",
            "object_id": draw.object_id,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "parent_bounds": _bounds_path_to_wire(draw.parent_bounds),
            "series_id": draw.series_id,
            "minimum": draw.minimum,
            "maximum": draw.maximum,
            "trace": [
                draw.trace.red,
                draw.trace.green,
                draw.trace.blue,
                draw.trace.alpha,
            ],
            "zero_line": [
                draw.zero_line.red,
                draw.zero_line.green,
                draw.zero_line.blue,
                draw.zero_line.alpha,
            ],
            "zero_value": draw.zero_value,
            "draw_zero_line": draw.draw_zero_line,
        }
    if isinstance(draw, MenuBarDraw):
        return {
            "kind": "menu_bar",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "menus": [_menu_to_wire(menu) for menu in draw.menus],
        }
    if isinstance(draw, (TextAreaDraw, TextGridDraw)):
        kind = (
            ControlKind.TEXT_AREA
            if isinstance(draw, TextAreaDraw)
            else ControlKind.TEXT_GRID
        )
        _validate_collection_draw_shape(
            kind,
            draw.state,
            draw.order,
            draw.z_order,
            draw.bounds,
            draw.content,
        )
        return {
            "kind": "text_area" if kind is ControlKind.TEXT_AREA else "text_grid",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "content_stx1_base64": _semantic_content_to_wire(draw.content),
        }
    if isinstance(draw, ItemViewDraw):
        _validate_collection_draw_shape(
            ControlKind.ITEM_VIEW,
            draw.state,
            draw.order,
            draw.z_order,
            draw.bounds,
            draw.content,
        )
        return {
            "kind": "item_view",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "content_itm1_base64": _item_content_to_wire(draw.content),
        }
    if isinstance(draw, TabSetDraw):
        return {
            "kind": "tabset",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": _bounds_to_wire(draw.bounds),
            "tabs": [_tab_to_wire(tab) for tab in draw.tabs],
        }
    raise TypeError("retained draw is outside the shared-viewer vocabulary")


def _region_header_to_wire(region: RetainedRegionDraw) -> dict:
    return {
        "owner_id": region.owner_id,
        "owner_generation": region.owner_generation,
        "region_id": region.region_id,
        "logical_x": region.logical_x,
        "logical_y": region.logical_y,
        "logical_cols": region.logical_cols,
        "logical_rows": region.logical_rows,
        "clip_x": region.clip_x,
        "clip_y": region.clip_y,
        "clip_cols": region.clip_cols,
        "clip_rows": region.clip_rows,
        "z_order": region.z_order,
        "clipped": region.clipped,
    }


def retained_draw_plane_to_wire(plane: RetainedDrawPlane) -> dict:
    """Encode only the immutable renderer-facing draw plane."""

    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    return {
        "retained_initialized": plane.retained_initialized,
        "retained_visible": plane.retained_visible,
        "series": [_series_history_to_wire(history) for history in plane.series],
        "resources": [
            _image_resource_to_wire(resource) for resource in plane.resources
        ],
        "regions": [
            {
                **_region_header_to_wire(region),
                "draws": [_retained_draw_to_wire(draw) for draw in region.draws],
            }
            for region in plane.regions
        ],
    }


def _draws_by_key(draws) -> dict | None:
    """Each draw by its identity, or None when one identity names two draws."""

    by_key = {}
    for draw in draws:
        key = retained_draw_key(draw)
        if key in by_key:
            return None
        by_key[key] = draw
    return by_key


def _retained_plane_changes_to_wire(
    plane: RetainedDrawPlane,
    base: RetainedDrawPlane,
) -> dict:
    """``plane`` with each region that has a base carried as its changes."""

    base_regions = {
        (region.owner_id, region.owner_generation, region.region_id): region
        for region in base.regions
    }
    regions = []
    for region in plane.regions:
        entry = _region_header_to_wire(region)
        previous = base_regions.get(
            (region.owner_id, region.owner_generation, region.region_id)
        )
        before = None if previous is None else _draws_by_key(previous.draws)
        after = None if before is None else _draws_by_key(region.draws)
        if after is None:
            entry["draws"] = [_retained_draw_to_wire(draw) for draw in region.draws]
        else:
            entry["removed"] = [list(key) for key in before if key not in after]
            entry["changed"] = [
                _retained_draw_to_wire(draw)
                for key, draw in after.items()
                if before.get(key) != draw
            ]
        regions.append(entry)
    return {
        "retained_initialized": plane.retained_initialized,
        "retained_visible": plane.retained_visible,
        "series": [_series_history_to_wire(history) for history in plane.series],
        "resources": [
            _image_resource_to_wire(resource) for resource in plane.resources
        ],
        "regions": regions,
    }


def _control_state_from_wire(value, name: str) -> ControlState:
    return ControlState(
        _wire_integer(value, name, minimum=0, maximum=0xFFFF)
    )


def _menu_entry_from_wire(data, name: str) -> MenuItemDraw | MenuSeparatorDraw:
    if not isinstance(data, Mapping):
        raise TypeError(f"{name} must be an object")
    kind = data.get("kind")
    if kind == "menu_item":
        wire = _wire_object(data, name, _MENU_ITEM_WIRE_FIELDS)
        return MenuItemDraw(
            control_id=_wire_integer(
                wire["control_id"],
                f"{name} control_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            state=_control_state_from_wire(wire["state"], f"{name} state"),
            order=_wire_integer(
                wire["order"],
                f"{name} order",
                minimum=0,
                maximum=UINT32_MAX,
            ),
            label=_wire_text(wire["label"], f"{name} label"),
            shortcut=_wire_text(wire["shortcut"], f"{name} shortcut"),
        )
    if kind == "menu_separator":
        wire = _wire_object(data, name, _MENU_SEPARATOR_WIRE_FIELDS)
        return MenuSeparatorDraw(
            control_id=_wire_integer(
                wire["control_id"],
                f"{name} control_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            state=_control_state_from_wire(wire["state"], f"{name} state"),
            order=_wire_integer(
                wire["order"],
                f"{name} order",
                minimum=0,
                maximum=UINT32_MAX,
            ),
        )
    raise ValueError(f"{name} kind is not a semantic menu entry")


def _menu_from_wire(data, name: str) -> MenuDraw:
    wire = _wire_object(data, name, _MENU_WIRE_FIELDS)
    if wire["kind"] != "menu":
        raise ValueError(f"{name} kind must be menu")
    entries_wire = wire["entries"]
    if not isinstance(entries_wire, (list, tuple)):
        raise TypeError(f"{name} entries must be an array")
    return MenuDraw(
        control_id=_wire_integer(
            wire["control_id"],
            f"{name} control_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        state=_control_state_from_wire(wire["state"], f"{name} state"),
        order=_wire_integer(
            wire["order"],
            f"{name} order",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        label=_wire_text(wire["label"], f"{name} label"),
        entries=tuple(
            _menu_entry_from_wire(entry, f"{name} entry {entry_index}")
            for entry_index, entry in enumerate(entries_wire)
        ),
    )


def _tab_from_wire(data, name: str) -> TabDraw:
    wire = _wire_object(data, name, _TAB_WIRE_FIELDS)
    if wire["kind"] != "tab":
        raise ValueError(f"{name} kind must be tab")
    return TabDraw(
        control_id=_wire_integer(
            wire["control_id"],
            f"{name} control_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        state=_control_state_from_wire(wire["state"], f"{name} state"),
        order=_wire_integer(
            wire["order"],
            f"{name} order",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        label=_wire_text(wire["label"], f"{name} label"),
        shortcut=_wire_text(wire["shortcut"], f"{name} shortcut"),
    )


def _text_collection_from_wire(
    data,
    name: str,
    kind: ControlKind,
) -> TextAreaDraw | TextGridDraw:
    wire = _wire_object(data, name, _TEXT_COLLECTION_WIRE_FIELDS)
    expected_tag = (
        "text_area" if kind is ControlKind.TEXT_AREA else "text_grid"
    )
    if wire["kind"] != expected_tag:
        raise ValueError(f"{name} kind must be {expected_tag}")
    state = _control_state_from_wire(wire["state"], f"{name} state")
    order = _wire_integer(
        wire["order"], f"{name} order", minimum=0, maximum=UINT32_MAX
    )
    z_order = _wire_integer(
        wire["z_order"],
        f"{name} z_order",
        minimum=INT32_MIN,
        maximum=INT32_MAX,
    )
    bounds = ObjectBounds(
        *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
    )
    content = _semantic_content_from_wire(
        wire["content_stx1_base64"],
        f"{name} content_stx1_base64",
    )
    _validate_collection_draw_shape(
        kind,
        state,
        order,
        z_order,
        bounds,
        content,
    )
    draw_type = TextAreaDraw if kind is ControlKind.TEXT_AREA else TextGridDraw
    return draw_type(
        control_id=_wire_integer(
            wire["control_id"],
            f"{name} control_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        state=state,
        order=order,
        z_order=z_order,
        bounds=bounds,
        content=content,
    )


def _item_view_from_wire(data, name: str) -> ItemViewDraw:
    wire = _wire_object(data, name, _ITEM_VIEW_WIRE_FIELDS)
    if wire["kind"] != "item_view":
        raise ValueError(f"{name} kind must be item_view")
    state = _control_state_from_wire(wire["state"], f"{name} state")
    order = _wire_integer(
        wire["order"], f"{name} order", minimum=0, maximum=UINT32_MAX
    )
    z_order = _wire_integer(
        wire["z_order"],
        f"{name} z_order",
        minimum=INT32_MIN,
        maximum=INT32_MAX,
    )
    bounds = ObjectBounds(
        *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
    )
    content = _item_content_from_wire(
        wire["content_itm1_base64"],
        f"{name} content_itm1_base64",
    )
    _validate_collection_draw_shape(
        ControlKind.ITEM_VIEW, state, order, z_order, bounds, content
    )
    return ItemViewDraw(
        control_id=_wire_integer(
            wire["control_id"],
            f"{name} control_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        state=state,
        order=order,
        z_order=z_order,
        bounds=bounds,
        content=content,
    )


def _tabset_from_wire(data, name: str) -> TabSetDraw:
    wire = _wire_object(data, name, _TABSET_WIRE_FIELDS)
    if wire["kind"] != "tabset":
        raise ValueError(f"{name} kind must be tabset")
    tabs_wire = wire["tabs"]
    if not isinstance(tabs_wire, (list, tuple)):
        raise TypeError(f"{name} tabs must be an array")
    return TabSetDraw(
        control_id=_wire_integer(
            wire["control_id"],
            f"{name} control_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        state=_control_state_from_wire(wire["state"], f"{name} state"),
        order=_wire_integer(
            wire["order"],
            f"{name} order",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        z_order=_wire_integer(
            wire["z_order"],
            f"{name} z_order",
            minimum=INT32_MIN,
            maximum=INT32_MAX,
        ),
        bounds=ObjectBounds(
            *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
        ),
        tabs=tuple(
            _tab_from_wire(tab, f"{name} tab {tab_index}")
            for tab_index, tab in enumerate(tabs_wire)
        ),
    )


def _retained_draw_from_wire(
    data,
    name: str,
) -> (
    GlyphRunDraw
    | PolylineDraw
    | ImageDraw
    | ReadoutDraw
    | MeterDraw
    | StatusDraw
    | PlotDraw
    | WaveformDraw
    | MenuBarDraw
    | TextAreaDraw
    | TextGridDraw
    | TabSetDraw
    | ItemViewDraw
):
    if not isinstance(data, Mapping):
        raise TypeError(f"{name} must be an object")
    kind = data.get("kind")
    if kind == "glyph_run":
        wire = _wire_object(data, name, _GLYPH_RUN_WIRE_FIELDS)
        bounds = _wire_integer_array(wire["bounds"], f"{name} bounds", 4)
        foreground = _wire_integer_array(
            wire["foreground"], f"{name} foreground", 4, maximum=0xFF
        )
        background = _wire_integer_array(
            wire["background"], f"{name} background", 4, maximum=0xFF
        )
        return GlyphRunDraw(
            object_id=_wire_integer(
                wire["object_id"],
                f"{name} object_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(*bounds),
            foreground=RGBA(*foreground),
            background=RGBA(*background),
            attributes=_wire_integer(
                wire["attributes"],
                f"{name} attributes",
                minimum=0,
                maximum=0x7F,
            ),
            text=_wire_text(wire["text"], f"{name} text"),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
        )
    if kind == "polyline":
        wire = _wire_object(data, name, _POLYLINE_WIRE_FIELDS)
        points_wire = wire["points"]
        if not isinstance(points_wire, (list, tuple)):
            raise TypeError(f"{name} points must be an array")
        return PolylineDraw(
            object_id=_wire_integer(
                wire["object_id"],
                f"{name} object_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            points=tuple(
                Point(
                    *_wire_integer_array(point, f"{name} point {index}", 2)
                )
                for index, point in enumerate(points_wire)
            ),
            stroke_width=_wire_integer(
                wire["stroke_width"],
                f"{name} stroke_width",
                minimum=1,
                maximum=UINT32_MAX,
            ),
            color=RGBA(
                *_wire_integer_array(
                    wire["color"], f"{name} color", 4, maximum=0xFF
                )
            ),
            closed=_wire_boolean(wire["closed"], f"{name} closed"),
        )
    if kind == "image":
        wire = _wire_object(data, name, _IMAGE_WIRE_FIELDS)
        return ImageDraw(
            object_id=_wire_integer(
                wire["object_id"],
                f"{name} object_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            resource_id=_wire_integer(
                wire["resource_id"],
                f"{name} resource_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            fit=ImageFit(
                _wire_integer(
                    wire["fit"], f"{name} fit", minimum=0, maximum=0xFF
                )
            ),
            opacity=_wire_integer(
                wire["opacity"], f"{name} opacity", minimum=0, maximum=0xFF
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
        )
    if kind == "readout":
        wire = _wire_object(data, name, _READOUT_WIRE_FIELDS)
        return ReadoutDraw(
            object_id=_wire_integer(
                wire["object_id"], f"{name} object_id", minimum=1, maximum=UINT64_MAX
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            foreground=RGBA(
                *_wire_integer_array(
                    wire["foreground"], f"{name} foreground", 4, maximum=0xFF
                )
            ),
            background=RGBA(
                *_wire_integer_array(
                    wire["background"], f"{name} background", 4, maximum=0xFF
                )
            ),
            text=_wire_text(wire["text"], f"{name} text"),
        )
    if kind == "meter":
        wire = _wire_object(data, name, _METER_WIRE_FIELDS)
        return MeterDraw(
            object_id=_wire_integer(
                wire["object_id"], f"{name} object_id", minimum=1, maximum=UINT64_MAX
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            foreground=RGBA(
                *_wire_integer_array(
                    wire["foreground"], f"{name} foreground", 4, maximum=0xFF
                )
            ),
            background=RGBA(
                *_wire_integer_array(
                    wire["background"], f"{name} background", 4, maximum=0xFF
                )
            ),
            vertical=_wire_boolean(wire["vertical"], f"{name} vertical"),
            show_value=_wire_boolean(wire["show_value"], f"{name} show_value"),
            minimum=_wire_integer(
                wire["minimum"],
                f"{name} minimum",
                minimum=INT64_MIN,
                maximum=INT64_MAX,
            ),
            maximum=_wire_integer(
                wire["maximum"],
                f"{name} maximum",
                minimum=INT64_MIN,
                maximum=INT64_MAX,
            ),
            value=_wire_integer(
                wire["value"],
                f"{name} value",
                minimum=INT64_MIN,
                maximum=INT64_MAX,
            ),
        )
    if kind == "status":
        wire = _wire_object(data, name, _STATUS_WIRE_FIELDS)
        return StatusDraw(
            object_id=_wire_integer(
                wire["object_id"], f"{name} object_id", minimum=1, maximum=UINT64_MAX
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            inactive=RGBA(
                *_wire_integer_array(
                    wire["inactive"], f"{name} inactive", 4, maximum=0xFF
                )
            ),
            active=RGBA(
                *_wire_integer_array(
                    wire["active"], f"{name} active", 4, maximum=0xFF
                )
            ),
            value=_wire_integer(
                wire["value"],
                f"{name} value",
                minimum=INT64_MIN,
                maximum=INT64_MAX,
            ),
            shape=_wire_integer(
                wire["shape"], f"{name} shape", minimum=0, maximum=2
            ),
        )
    if kind == "plot":
        wire = _wire_object(data, name, _PLOT_WIRE_FIELDS)
        return PlotDraw(
            object_id=_wire_integer(
                wire["object_id"], f"{name} object_id", minimum=1, maximum=UINT64_MAX
            ),
            z_order=_wire_integer(
                wire["z_order"], f"{name} z_order", minimum=INT32_MIN, maximum=INT32_MAX
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            series_id=_wire_integer(
                wire["series_id"], f"{name} series_id", minimum=1, maximum=UINT64_MAX
            ),
            minimum=_wire_integer(
                wire["minimum"], f"{name} minimum", minimum=INT64_MIN, maximum=INT64_MAX
            ),
            maximum=_wire_integer(
                wire["maximum"], f"{name} maximum", minimum=INT64_MIN, maximum=INT64_MAX
            ),
            line=RGBA(
                *_wire_integer_array(wire["line"], f"{name} line", 4, maximum=0xFF)
            ),
            fill=RGBA(
                *_wire_integer_array(wire["fill"], f"{name} fill", 4, maximum=0xFF)
            ),
            fill_to_minimum=_wire_boolean(
                wire["fill_to_minimum"], f"{name} fill_to_minimum"
            ),
            draw_points=_wire_boolean(wire["draw_points"], f"{name} draw_points"),
        )
    if kind == "waveform":
        wire = _wire_object(data, name, _WAVEFORM_WIRE_FIELDS)
        return WaveformDraw(
            object_id=_wire_integer(
                wire["object_id"], f"{name} object_id", minimum=1, maximum=UINT64_MAX
            ),
            z_order=_wire_integer(
                wire["z_order"], f"{name} z_order", minimum=INT32_MIN, maximum=INT32_MAX
            ),
            bounds=ObjectBounds(
                *_wire_integer_array(wire["bounds"], f"{name} bounds", 4)
            ),
            parent_bounds=_bounds_path_from_wire(
                wire["parent_bounds"], f"{name} parent_bounds"
            ),
            series_id=_wire_integer(
                wire["series_id"], f"{name} series_id", minimum=1, maximum=UINT64_MAX
            ),
            minimum=_wire_integer(
                wire["minimum"], f"{name} minimum", minimum=INT64_MIN, maximum=INT64_MAX
            ),
            maximum=_wire_integer(
                wire["maximum"], f"{name} maximum", minimum=INT64_MIN, maximum=INT64_MAX
            ),
            trace=RGBA(
                *_wire_integer_array(wire["trace"], f"{name} trace", 4, maximum=0xFF)
            ),
            zero_line=RGBA(
                *_wire_integer_array(
                    wire["zero_line"], f"{name} zero_line", 4, maximum=0xFF
                )
            ),
            zero_value=_wire_integer(
                wire["zero_value"],
                f"{name} zero_value",
                minimum=INT64_MIN,
                maximum=INT64_MAX,
            ),
            draw_zero_line=_wire_boolean(
                wire["draw_zero_line"], f"{name} draw_zero_line"
            ),
        )
    if kind == "menu_bar":
        wire = _wire_object(data, name, _MENU_BAR_WIRE_FIELDS)
        bounds = _wire_integer_array(wire["bounds"], f"{name} bounds", 4)
        menus_wire = wire["menus"]
        if not isinstance(menus_wire, (list, tuple)):
            raise TypeError(f"{name} menus must be an array")
        return MenuBarDraw(
            control_id=_wire_integer(
                wire["control_id"],
                f"{name} control_id",
                minimum=1,
                maximum=UINT64_MAX,
            ),
            state=_control_state_from_wire(wire["state"], f"{name} state"),
            order=_wire_integer(
                wire["order"],
                f"{name} order",
                minimum=0,
                maximum=UINT32_MAX,
            ),
            z_order=_wire_integer(
                wire["z_order"],
                f"{name} z_order",
                minimum=INT32_MIN,
                maximum=INT32_MAX,
            ),
            bounds=ObjectBounds(*bounds),
            menus=tuple(
                _menu_from_wire(menu, f"{name} menu {menu_index}")
                for menu_index, menu in enumerate(menus_wire)
            ),
        )
    if kind == "text_area":
        return _text_collection_from_wire(data, name, ControlKind.TEXT_AREA)
    if kind == "text_grid":
        return _text_collection_from_wire(data, name, ControlKind.TEXT_GRID)
    if kind == "tabset":
        return _tabset_from_wire(data, name)
    if kind == "item_view":
        return _item_view_from_wire(data, name)
    raise ValueError(f"{name} kind is not a retained draw kind")


def _plane_parts_from_wire(
    data,
) -> tuple[bool, bool, tuple[SeriesHistoryDraw, ...], tuple[ImageResourceManifest, ...], Any]:
    wire = _wire_object(
        data,
        "retained draw plane",
        (
            "retained_initialized",
            "retained_visible",
            "series",
            "resources",
            "regions",
        ),
    )
    series_wire = wire["series"]
    if not isinstance(series_wire, (list, tuple)):
        raise TypeError("retained draw series must be an array")
    series = tuple(
        _series_history_from_wire(history, f"retained series {history_index}")
        for history_index, history in enumerate(series_wire)
    )
    resources_wire = wire["resources"]
    if not isinstance(resources_wire, (list, tuple)):
        raise TypeError("retained draw resources must be an array")
    resources = tuple(
        _image_resource_from_wire(
            resource,
            f"retained resource {resource_index}",
        )
        for resource_index, resource in enumerate(resources_wire)
    )
    regions_wire = wire["regions"]
    if not isinstance(regions_wire, (list, tuple)):
        raise TypeError("retained draw regions must be an array")
    return (
        _wire_boolean(wire["retained_initialized"], "retained draw initialized"),
        _wire_boolean(wire["retained_visible"], "retained draw visible"),
        series,
        resources,
        regions_wire,
    )


def _region_header_from_wire(region: Mapping[str, Any], prefix: str) -> dict:
    return {
        "owner_id": _wire_integer(
            region["owner_id"],
            f"{prefix} owner_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        "owner_generation": _wire_integer(
            region["owner_generation"],
            f"{prefix} owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        "region_id": _wire_integer(
            region["region_id"],
            f"{prefix} region_id",
            minimum=1,
            maximum=UINT64_MAX,
        ),
        "logical_x": _wire_integer(
            region["logical_x"],
            f"{prefix} logical_x",
            minimum=INT32_MIN,
            maximum=INT32_MAX,
        ),
        "logical_y": _wire_integer(
            region["logical_y"],
            f"{prefix} logical_y",
            minimum=INT32_MIN,
            maximum=INT32_MAX,
        ),
        "logical_cols": _wire_integer(
            region["logical_cols"],
            f"{prefix} logical_cols",
            minimum=1,
            maximum=UINT32_MAX,
        ),
        "logical_rows": _wire_integer(
            region["logical_rows"],
            f"{prefix} logical_rows",
            minimum=1,
            maximum=UINT32_MAX,
        ),
        "clip_x": _wire_integer(
            region["clip_x"],
            f"{prefix} clip_x",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        "clip_y": _wire_integer(
            region["clip_y"],
            f"{prefix} clip_y",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        "clip_cols": _wire_integer(
            region["clip_cols"],
            f"{prefix} clip_cols",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        "clip_rows": _wire_integer(
            region["clip_rows"],
            f"{prefix} clip_rows",
            minimum=0,
            maximum=UINT32_MAX,
        ),
        "z_order": _wire_integer(
            region["z_order"],
            f"{prefix} z_order",
            minimum=INT32_MIN,
            maximum=INT32_MAX,
        ),
        "clipped": _wire_boolean(region["clipped"], f"{prefix} clipped"),
    }


def _region_draws_from_wire(region: Mapping[str, Any], prefix: str) -> tuple:
    draws_wire = region["draws"]
    if not isinstance(draws_wire, (list, tuple)):
        raise TypeError(f"{prefix} draws must be an array")
    return tuple(
        _retained_draw_from_wire(raw_draw, f"{prefix} draw {draw_index}")
        for draw_index, raw_draw in enumerate(draws_wire)
    )


def retained_draw_plane_from_wire(data: dict) -> RetainedDrawPlane:
    """Decode the complete draw plane with strict scalar types."""

    initialized, visible, series, resources, regions_wire = _plane_parts_from_wire(
        data
    )
    regions: list[RetainedRegionDraw] = []
    for region_index, raw_region in enumerate(regions_wire):
        prefix = f"retained region {region_index}"
        region = _wire_object(raw_region, prefix, _REGION_WIRE_FIELDS)
        regions.append(
            RetainedRegionDraw(
                **_region_header_from_wire(region, prefix),
                draws=_region_draws_from_wire(region, prefix),
            )
        )
    return RetainedDrawPlane(
        retained_initialized=initialized,
        retained_visible=visible,
        regions=tuple(regions),
        series=series,
        resources=resources,
    )


def _draw_key_from_wire(value, name: str) -> tuple[str, int]:
    if not isinstance(value, (list, tuple)) or len(value) != 2:
        raise TypeError(f"{name} must be a two-item array")
    kind = value[0]
    if kind not in ("object", "control"):
        raise ValueError(f"{name} must name an object or a control")
    return kind, _wire_integer(value[1], f"{name} id", minimum=1, maximum=UINT64_MAX)


def _retained_plane_changes_from_wire(
    data,
    base: RetainedDrawPlane,
) -> RetainedDrawPlane:
    """Rebuild a plane whose regions may be carried as changes to ``base``."""

    initialized, visible, series, resources, regions_wire = _plane_parts_from_wire(
        data
    )
    base_regions = {
        (region.owner_id, region.owner_generation, region.region_id): region
        for region in base.regions
    }
    regions: list[RetainedRegionDraw] = []
    for region_index, raw_region in enumerate(regions_wire):
        prefix = f"retained region {region_index}"
        if not isinstance(raw_region, Mapping) or "draws" in raw_region:
            region = _wire_object(raw_region, prefix, _REGION_WIRE_FIELDS)
            regions.append(
                RetainedRegionDraw(
                    **_region_header_from_wire(region, prefix),
                    draws=_region_draws_from_wire(region, prefix),
                )
            )
            continue
        region = _wire_object(raw_region, prefix, _REGION_CHANGE_FIELDS)
        header = _region_header_from_wire(region, prefix)
        previous = base_regions.get(
            (header["owner_id"], header["owner_generation"], header["region_id"])
        )
        draws = None if previous is None else _draws_by_key(previous.draws)
        if draws is None:
            raise ValueError(f"{prefix} changes name no base region with keyed draws")
        removed_wire = region["removed"]
        changed_wire = region["changed"]
        if not isinstance(removed_wire, (list, tuple)):
            raise TypeError(f"{prefix} removed draws must be an array")
        if not isinstance(changed_wire, (list, tuple)):
            raise TypeError(f"{prefix} changed draws must be an array")
        removed = set()
        for index, raw_key in enumerate(removed_wire):
            key = _draw_key_from_wire(raw_key, f"{prefix} removed draw {index}")
            if key not in draws:
                raise ValueError(f"{prefix} removes a draw its base does not have")
            del draws[key]
            removed.add(key)
        changed = set()
        for index, raw_draw in enumerate(changed_wire):
            draw = _retained_draw_from_wire(raw_draw, f"{prefix} changed draw {index}")
            key = retained_draw_key(draw)
            if key in changed or key in removed:
                raise ValueError(f"{prefix} changes one draw twice")
            changed.add(key)
            draws[key] = draw
        regions.append(
            RetainedRegionDraw(**header, draws=retained_draw_order(draws.values()))
        )
    return RetainedDrawPlane(
        retained_initialized=initialized,
        retained_visible=visible,
        regions=tuple(regions),
        series=series,
        resources=resources,
    )


def display_offer_to_wire(
    offer: TerminalDisplayOffer,
    rows: WireRowRuns | None = None,
    base: TerminalDisplayOffer | None = None,
) -> dict:
    """Encode one immutable physical offer without model authority objects.

    With ``base``, the viewer's presented offer, an offer with the same CELL
    geometry is encoded as its changes against that base.
    """

    if not isinstance(offer, TerminalDisplayOffer):
        raise TypeError("offer must be TerminalDisplayOffer")
    if base is not None and not isinstance(base, TerminalDisplayOffer):
        raise TypeError("base must be TerminalDisplayOffer")
    if base is None or (base.cell.cols, base.cell.rows) != (
        offer.cell.cols,
        offer.cell.rows,
    ):
        return {
            "offer_id": offer.offer_id,
            "scope": display_scope_to_wire(offer.scope),
            "cell": snapshot_to_wire(offer.cell, rows),
            "retained": retained_draw_plane_to_wire(offer.retained),
        }
    return {
        "offer_id": offer.offer_id,
        "base_offer_id": base.offer_id,
        "scope": display_scope_to_wire(offer.scope),
        "cell": _snapshot_changes_to_wire(offer.cell, base.cell, rows),
        "retained": _retained_plane_changes_to_wire(offer.retained, base.retained),
    }


def display_offer_from_wire(
    data: dict,
    base: TerminalDisplayOffer | None = None,
) -> TerminalDisplayOffer:
    """Decode an exact immutable physical offer from the display wire.

    An offer carried as changes is rebuilt from ``base``, which must be the
    offer it names.
    """

    if isinstance(data, Mapping) and "base_offer_id" in data:
        wire = _wire_object(
            data,
            "display offer",
            ("offer_id", "base_offer_id", "scope", "cell", "retained"),
        )
        base_offer_id = _wire_integer(
            wire["base_offer_id"], "display offer base id", minimum=1
        )
        if not isinstance(base, TerminalDisplayOffer) or base.offer_id != base_offer_id:
            raise ValueError("display offer changes name a base this viewer does not hold")
        return TerminalDisplayOffer(
            offer_id=_wire_integer(wire["offer_id"], "display offer id", minimum=1),
            scope=display_scope_from_wire(wire["scope"]),
            cell=_snapshot_changes_from_wire(wire["cell"], base.cell),
            retained=_retained_plane_changes_from_wire(wire["retained"], base.retained),
        )
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


class SharedSessionOwner(ABC):
    """Shared presentation, input, and lifecycle authority for one session."""

    def __init__(
        self,
        session: TerminalSession,
        *,
        idle_sleep_s: float = 0.002,
        idle_wait_cap_s: float = 0.02,
        host_profile: bool = False,
    ):
        if not isinstance(host_profile, bool):
            raise TypeError("host_profile must be a boolean")
        self.session = session
        # idle_sleep_s paces a boundary that made no progress.  When every
        # core sleeps, the owner instead waits for input (which notifies the
        # condition) or the next timed wake, rechecking at least every
        # idle_wait_cap_s for sources that do not notify, such as NIC frames.
        self.idle_sleep_s = float(idle_sleep_s)
        self.idle_wait_cap_s = float(idle_wait_cap_s)
        self._host_profile_enabled = host_profile
        # Screen encodings reuse the runs of rows unchanged since the last.
        self._wire_rows = WireRowRuns()
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
        self._phase_profile: _PhaseEventProfile | None = None

    @staticmethod
    def _phase_event_fields(event: int) -> tuple[int, int]:
        return (
            event >> _PHASE_EVENT_SEQUENCE_SHIFT,
            event & _PHASE_EVENT_PHASE_MASK,
        )

    @abstractmethod
    def _phase_profile_address_valid(self, address: int) -> bool:
        """Whether a complete phase cell is readable in admitted guest memory."""
        raise NotImplementedError

    @abstractmethod
    def _phase_profile_read(self, address: int) -> int:
        """Read a phase cell without changing guest state."""
        raise NotImplementedError

    @abstractmethod
    def _phase_profile_batch_step_bound(self) -> int | None:
        """Return a fixed sampling work bound, or None for variable intervals."""
        raise NotImplementedError

    def _phase_profile_snapshot_locked(self) -> dict:
        profile = self._phase_profile
        if profile is None:
            return {
                "schema": _PHASE_PROFILE_SCHEMA,
                "schema_version": 1,
                "status": "disabled",
                "machine_generation": self._reset_generation,
                "current_steps": self.total_steps,
                "current_batches": self.total_batches,
            }

        initial_sequence, initial_phase = self._phase_event_fields(
            profile.initial_event
        )
        last_sequence, last_phase = self._phase_event_fields(profile.last_event)
        return {
            "schema": _PHASE_PROFILE_SCHEMA,
            "schema_version": 1,
            "status": profile.status,
            "machine_generation": profile.machine_generation,
            "address": profile.address,
            "encoding": "u64-sequence-high56-phase-low8",
            "batch_step_bound": profile.batch_step_bound,
            "max_events": profile.max_events,
            "started_steps": profile.started_steps,
            "started_batches": profile.started_batches,
            "current_steps": self.total_steps,
            "current_batches": self.total_batches,
            "last_sample_steps": profile.last_sample_steps,
            "last_sample_batches": profile.last_sample_batches,
            "stopped_steps": profile.stopped_steps,
            "stopped_batches": profile.stopped_batches,
            "initial": {
                "event": profile.initial_event,
                "sequence": initial_sequence,
                "phase": initial_phase,
            },
            "last": {
                "event": profile.last_event,
                "sequence": last_sequence,
                "phase": last_phase,
            },
            "sample_attempts": profile.sample_attempts,
            "successful_samples": profile.successful_samples,
            "observed_transitions": profile.observed_transitions,
            "coalesced_transitions": profile.coalesced_transitions,
            "dropped_records": profile.dropped_records,
            "dropped_transitions": profile.dropped_transitions,
            "error": None if profile.error is None else dict(profile.error),
            "transitions": [dict(item) for item in profile.transitions],
        }

    def start_phase_profile(
        self,
        address: int,
        max_events: int,
        *,
        generation: int,
    ) -> dict:
        """Observe one packed guest phase cell without changing guest state."""

        normalized_generation = _wire_integer(
            generation,
            "phase profile generation",
            minimum=0,
        )
        normalized_address = _wire_integer(
            address,
            "phase profile address",
            minimum=0,
            maximum=UINT64_MAX,
        )
        normalized_capacity = _wire_integer(
            max_events,
            "phase profile max_events",
            minimum=1,
            maximum=_PHASE_PROFILE_MAX_EVENTS,
        )
        with self.condition:
            thread = self._thread
            if thread is None or self._stopping or not thread.is_alive():
                raise RuntimeError("phase profile requires a running machine")
            if normalized_generation != self._reset_generation:
                raise RuntimeError(
                    "stale phase profile generation "
                    f"{normalized_generation}; current generation is "
                    f"{self._reset_generation}"
                )
            if self._phase_profile is not None:
                raise RuntimeError("phase profile is already configured")
            if not self._phase_profile_address_valid(normalized_address):
                raise ValueError(
                    "phase profile address must name a complete RAM or "
                    "external-memory cell"
                )
            event = _wire_integer(
                self._phase_profile_read(normalized_address),
                "phase profile event",
                minimum=0,
                maximum=UINT64_MAX,
            )
            self._phase_profile = _PhaseEventProfile(
                address=normalized_address,
                max_events=normalized_capacity,
                machine_generation=self._reset_generation,
                batch_step_bound=self._phase_profile_batch_step_bound(),
                started_steps=self.total_steps,
                started_batches=self.total_batches,
                initial_event=event,
                last_event=event,
                last_sample_steps=self.total_steps,
                last_sample_batches=self.total_batches,
            )
            return self._phase_profile_snapshot_locked()

    def phase_profile(self) -> dict:
        """Return a bounded copy without performing another guest read."""

        with self.lock:
            return self._phase_profile_snapshot_locked()

    def stop_phase_profile(self) -> dict:
        """Freeze and remove the observer, returning its final snapshot."""

        with self.condition:
            profile = self._phase_profile
            if profile is None:
                return self._phase_profile_snapshot_locked()
            if profile.status == "active":
                profile.status = "stopped"
                profile.stopped_steps = self.total_steps
                profile.stopped_batches = self.total_batches
            result = self._phase_profile_snapshot_locked()
            self._phase_profile = None
            return result

    def _sample_phase_profile(
        self,
        step_lower_bound: int,
        step_upper_bound: int,
        *,
        source: str,
        batch_index: int | None,
    ) -> None:
        """Sample after one exact guest retirement interval under the lock."""

        profile = self._phase_profile
        if profile is None or profile.status != "active":
            return

        profile.sample_attempts += 1
        try:
            event = _wire_integer(
                self._phase_profile_read(profile.address),
                "phase profile event",
                minimum=0,
                maximum=UINT64_MAX,
            )
        except Exception as exc:
            # Diagnostics must never pause or fail the running guest.  Freeze
            # this profile so a bad address cannot cause repeated reads.
            profile.status = "read_error"
            profile.stopped_steps = step_upper_bound
            profile.stopped_batches = self.total_batches
            profile.error = {
                "kind": type(exc).__name__,
                "message": str(exc),
            }
            return

        profile.successful_samples += 1
        profile.last_sample_steps = step_upper_bound
        profile.last_sample_batches = self.total_batches
        if event == profile.last_event:
            return

        previous_sequence, previous_phase = self._phase_event_fields(
            profile.last_event
        )
        sequence, phase = self._phase_event_fields(event)
        if sequence <= previous_sequence:
            profile.status = "invalid_event"
            profile.stopped_steps = step_upper_bound
            profile.stopped_batches = self.total_batches
            profile.error = {
                "kind": "sequence_regression",
                "message": (
                    f"phase sequence {sequence} did not advance beyond "
                    f"{previous_sequence}"
                ),
            }
            return

        sequence_delta = sequence - previous_sequence
        coalesced = sequence_delta - 1
        profile.observed_transitions += sequence_delta
        profile.coalesced_transitions += coalesced
        transition = {
            "machine_generation": profile.machine_generation,
            "sample_index": profile.successful_samples - 1,
            "source": source,
            "batch_index": batch_index,
            "step_lower_bound": step_lower_bound,
            "step_upper_bound": step_upper_bound,
            "previous_event": profile.last_event,
            "previous_sequence": previous_sequence,
            "previous_phase": previous_phase,
            "event": event,
            "sequence": sequence,
            "phase": phase,
            "coalesced_transitions": coalesced,
        }
        if len(profile.transitions) < profile.max_events:
            profile.transitions.append(transition)
        else:
            profile.dropped_records += 1
            profile.dropped_transitions += sequence_delta
        profile.last_event = event

    @abstractmethod
    def start(self):
        """Boot the selected session and start its execution owner thread."""
        raise NotImplementedError

    def stop(self):
        with self.condition:
            self._stopping = True
            self._phase_profile = None
            self.condition.notify_all()
        if self._thread is not None and self._thread is not threading.current_thread():
            self._thread.join(timeout=3.0)
        self.session.close()

    @abstractmethod
    def _run_loop(self):
        """Run the selected backend's work, wait, and event-admission policy."""
        raise NotImplementedError

    @abstractmethod
    def forth(self, names: list[str]) -> dict:
        """Resolve dictionary diagnostics using the selected word representation."""
        raise NotImplementedError

    @abstractmethod
    def peek(self, address: int, count: int = 1) -> dict:
        """Read diagnostic cells through the selected guest memory model."""
        raise NotImplementedError

    @abstractmethod
    def status(self, *, detailed: bool = True) -> dict:
        """Report terminal state and the backend's actual execution accounting."""
        raise NotImplementedError

    @abstractmethod
    def network(self) -> dict:
        """Report supported network diagnostics or reject unsupported access."""
        raise NotImplementedError

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

    @abstractmethod
    def step(self, count: int = 1) -> dict:
        """Advance paused execution in the selected backend's work units."""
        raise NotImplementedError

    @abstractmethod
    def reset(self, *, paused: bool | None = None) -> dict:
        """Reset supported execution state or reject an unavailable reset."""
        raise NotImplementedError

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

    def send_control_event(
        self,
        owner_id: int,
        owner_generation: int,
        control_id: int,
        *,
        event_kind: int = int(ControlEventKind.ACTIVATE),
        modifiers: int = 0,
        content_revision: int = 0,
        item_key: int = 0,
        scalar_offset: int = 0,
        wheel_x: int = 0,
        wheel_y: int = 0,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        """Forward one owner-qualified control intent under the display lease.

        ACTIVATE names a control; PLACE and EXTEND also name one STX1 position
        and SCROLL carries wheel detents.  The terminal core checks that the
        fields match the kind and that the position is still carried.
        """

        normalized_owner = _wire_integer(
            owner_id,
            "semantic control owner_id",
            minimum=1,
            maximum=UINT64_MAX,
        )
        normalized_owner_generation = _wire_integer(
            owner_generation,
            "semantic control owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        )
        normalized_control = _wire_integer(
            control_id,
            "semantic control control_id",
            minimum=1,
            maximum=UINT64_MAX,
        )
        normalized_modifiers = _wire_integer(
            modifiers,
            "semantic control modifiers",
            minimum=0,
            maximum=0x3F,
        )
        normalized_kind = ControlEventKind(
            _wire_integer(
                event_kind,
                "semantic control event_kind",
                minimum=1,
                maximum=max(ControlEventKind),
            )
        )
        tail = {
            "content_revision": _wire_integer(
                content_revision,
                "semantic control content_revision",
                minimum=0,
                maximum=UINT64_MAX,
            ),
            "item_key": _wire_integer(
                item_key,
                "semantic control item_key",
                minimum=0,
                maximum=UINT64_MAX,
            ),
            "scalar_offset": _wire_integer(
                scalar_offset,
                "semantic control scalar_offset",
                minimum=0,
                maximum=UINT32_MAX,
            ),
            "wheel_x": _wire_integer(
                wheel_x,
                "semantic control wheel_x",
                minimum=-(1 << 15),
                maximum=(1 << 15) - 1,
            ),
            "wheel_y": _wire_integer(
                wheel_y,
                "semantic control wheel_y",
                minimum=-(1 << 15),
                maximum=(1 << 15) - 1,
            ),
        }
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
            status = self._terminal_mutation_status(
                self.session.send_control_event(
                    normalized_owner,
                    normalized_owner_generation,
                    normalized_control,
                    event_kind=normalized_kind,
                    modifiers=normalized_modifiers,
                    **tail,
                )
            )
            self.condition.notify_all()
            return {
                "status": status.value,
                "accepted_events": 1 if status is DriverStatus.PROGRESS else 0,
            }

    def send_pointer(
        self,
        x: int,
        y: int,
        *,
        buttons: int,
        modifiers: int,
        kind: int,
        wheel_x: int = 0,
        wheel_y: int = 0,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        """Forward one cell-position pointer event on residual content.

        The viewer decides from the exact acknowledged hit map that the cell
        shows CELL or residual content; this host only proves the request
        still names that acknowledged display.
        """

        values = {
            "x": _wire_integer(x, "pointer x", minimum=-(1 << 31), maximum=(1 << 31) - 1),
            "y": _wire_integer(y, "pointer y", minimum=-(1 << 31), maximum=(1 << 31) - 1),
            "buttons": _wire_integer(buttons, "pointer buttons", minimum=0, maximum=0x1F),
            "modifiers": _wire_integer(
                modifiers, "pointer modifiers", minimum=0, maximum=0x3F
            ),
            "kind": _wire_integer(kind, "pointer kind", minimum=1, maximum=4),
            "wheel_x": _wire_integer(
                wheel_x, "pointer wheel_x", minimum=-(1 << 15), maximum=(1 << 15) - 1
            ),
            "wheel_y": _wire_integer(
                wheel_y, "pointer wheel_y", minimum=-(1 << 15), maximum=(1 << 15) - 1
            ),
        }
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
            status = self._terminal_mutation_status(
                self.session.send_pointer(
                    values["x"],
                    values["y"],
                    buttons=values["buttons"],
                    modifiers=values["modifiers"],
                    kind=values["kind"],
                    wheel_x=values["wheel_x"],
                    wheel_y=values["wheel_y"],
                )
            )
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
        base_offer: int = 0,
    ) -> dict:
        since = _wire_integer(since, "screen since", minimum=-1)
        since_offer = _wire_integer(
            since_offer, "screen since_offer", minimum=0
        )
        base_offer = _wire_integer(base_offer, "screen base_offer", minimum=0)
        if not isinstance(display_authorized, bool):
            raise TypeError("display_authorized must be bool")
        with self.lock:
            revision = self.session.revision
            snapshot = None if since == revision else self.session.snapshot()
            generation = self._reset_generation
            offer = self.session.display_offer if display_authorized else None
            if offer is not None and offer.offer_id == since_offer:
                offer = None
            # The holder's presented offer is the base it names only while the
            # session still holds that same presentation.
            base = None
            if offer is not None and base_offer:
                presented = self.session.acknowledged_display_offer
                if presented is not None and presented.offer_id == base_offer:
                    base = presented

        # Both renderer DTOs are immutable.  Keep the machine lock only for a
        # coherent capture; RLE and rich-plane conversion proceed while the
        # session continues running.
        result = {
            "changed": snapshot is not None or offer is not None,
            "revision": revision,
        }
        if snapshot is not None:
            result["snapshot"] = snapshot_to_wire(snapshot, self._wire_rows)
        if display_authorized:
            result["generation"] = generation
            if offer is not None:
                result["display_offer"] = display_offer_to_wire(
                    offer, self._wire_rows, base
                )
        return result

    @staticmethod
    def _resource_refusal(status: str) -> dict:
        if status not in {
            "stale_generation",
            "stale_display",
            "invalid_resource",
        }:
            raise ValueError("resource refusal has an unknown status")
        return {"status": status, "available": False}

    def display_resource_chunk(
        self,
        offer_id: int,
        scope: DisplayScope,
        owner_id: int,
        owner_generation: int,
        resource_id: int,
        sha3_256: bytes,
        offset: int,
        max_bytes: int,
        *,
        generation: int,
    ) -> dict:
        """Copy one exact current-offer resource range for its physical sink.

        Pixel bytes remain solely in the private composite pin.  The machine
        lock covers generation/display authorization and the bounded immutable
        copy; base64 expansion happens after that lock is released.
        """

        normalized_offer = _wire_integer(
            offer_id, "display offer id", minimum=1, maximum=UINT64_MAX
        )
        if not isinstance(scope, DisplayScope):
            raise TypeError("scope must be DisplayScope")
        normalized_owner = _wire_integer(
            owner_id, "resource owner_id", minimum=1, maximum=UINT64_MAX
        )
        normalized_owner_generation = _wire_integer(
            owner_generation,
            "resource owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        )
        normalized_resource = _wire_integer(
            resource_id, "resource_id", minimum=1, maximum=UINT64_MAX
        )
        if not isinstance(sha3_256, (bytes, bytearray, memoryview)):
            raise TypeError("sha3_256 must be bytes-like")
        normalized_digest = bytes(sha3_256)
        if len(normalized_digest) != 32:
            raise ValueError("sha3_256 must be exactly 32 bytes")
        normalized_offset = _wire_integer(
            offset, "resource offset", minimum=0, maximum=UINT64_MAX
        )
        normalized_max = _wire_integer(
            max_bytes, "resource max_bytes", minimum=1, maximum=UINT64_MAX
        )

        with self.lock:
            if not self._generation_current(generation):
                return self._resource_refusal("stale_generation")

            offer = self.session.display_offer
            composite = self.session._display_offer_composite
            if (
                offer is None
                or composite is None
                or offer.offer_id != normalized_offer
                or offer.scope != scope
                or composite.presentation_epoch != scope.presentation_epoch
                or composite.revision != scope.model_revision
            ):
                return self._resource_refusal("stale_display")
            cell = composite.cell
            if (
                cell is None
                or cell.attachment_epoch != scope.attachment_epoch
                or cell.session_id != scope.session_id
                or cell.presentation_epoch != scope.presentation_epoch
                or cell.revision != scope.cell_revision
                or composite.geometry.generation != scope.geometry_generation
            ):
                return self._resource_refusal("stale_display")

            driver = self.session.rich_terminal_driver
            policy = None if driver is None else driver.core.retained_policy
            if policy is None or policy.max_resource_chunk_bytes <= 0:
                return self._resource_refusal("invalid_resource")

            resource_key = (
                normalized_owner,
                normalized_owner_generation,
                normalized_resource,
            )
            manifest = next(
                (
                    candidate
                    for candidate in offer.retained.resources
                    if candidate.resource_key == resource_key
                ),
                None,
            )
            matching_resources = tuple(
                candidate
                for candidate in composite.resources
                if isinstance(candidate, RGBAResource)
                and (
                    candidate.owner.owner_id,
                    candidate.owner.owner_generation,
                    candidate.resource_id,
                )
                == resource_key
            )
            resource = (
                matching_resources[0]
                if len(matching_resources) == 1
                else None
            )
            if (
                manifest is None
                or resource is None
                or manifest.sha3_256 != normalized_digest
                or resource.digest != normalized_digest
                or resource.owner.session_id != scope.session_id
                or resource.owner.presentation_epoch != scope.presentation_epoch
                or resource.format != manifest.format
                or resource.width != manifest.width
                or resource.height != manifest.height
                or resource.byte_length != manifest.byte_length
                or normalized_offset > resource.byte_length
            ):
                return self._resource_refusal("invalid_resource")

            bounded_max = min(
                normalized_max,
                policy.max_resource_chunk_bytes,
            )
            data = resource.read(normalized_offset, bounded_max)
            next_offset = normalized_offset + len(data)
            response = {
                "status": "chunk",
                "available": True,
                "owner_id": normalized_owner,
                "owner_generation": normalized_owner_generation,
                "resource_id": normalized_resource,
                "sha3_256": normalized_digest.hex(),
                "offset": normalized_offset,
                "next_offset": next_offset,
                "byte_length": resource.byte_length,
                "eof": next_offset == resource.byte_length,
            }

        response["data_base64"] = base64.b64encode(data).decode("ascii")
        return response

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
    """Unix-domain JSON request server for one shared session owner."""

    def __init__(self, machine: SharedSessionOwner, socket_path: str = DEFAULT_SOCKET):
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
                base_offer=params.get("base_offer", 0),
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

    def _display_resource_chunk_for_connection(
        self,
        params: Mapping[str, Any],
        connection_id: int | None,
    ) -> dict:
        params = _wire_object(
            params,
            "display resource chunk",
            (
                "generation",
                "display_offer_id",
                "display_scope",
                "owner_id",
                "owner_generation",
                "resource_id",
                "sha3_256",
                "offset",
                "max_bytes",
            ),
        )
        generation = self._required_generation(params)
        pair = self._required_display_pair(params)
        owner_id = _wire_integer(
            params["owner_id"],
            "resource owner_id",
            minimum=1,
            maximum=UINT64_MAX,
        )
        owner_generation = _wire_integer(
            params["owner_generation"],
            "resource owner_generation",
            minimum=1,
            maximum=UINT64_MAX,
        )
        resource_id = _wire_integer(
            params["resource_id"],
            "resource_id",
            minimum=1,
            maximum=UINT64_MAX,
        )
        digest = _wire_sha3_256(params["sha3_256"], "resource sha3_256")
        offset = _wire_integer(
            params["offset"],
            "resource offset",
            minimum=0,
            maximum=UINT64_MAX,
        )
        max_bytes = _wire_integer(
            params["max_bytes"],
            "resource max_bytes",
            minimum=1,
            maximum=UINT64_MAX,
        )

        with self._display_lock:
            if connection_id is None or self._display_holder != connection_id:
                return {"status": "stale_display", "available": False}
            if pair != self._display_delivered:
                return {"status": "stale_display", "available": False}
            return self.machine.display_resource_chunk(
                pair[0],
                pair[1],
                owner_id,
                owner_generation,
                resource_id,
                digest,
                offset,
                max_bytes,
                generation=generation,
            )

    def _dispatch_terminal_input(
        self,
        method: str,
        params: Mapping[str, Any],
        connection_id: int | None,
    ) -> dict:
        if method == "send_control_event":
            params = _wire_object(
                params,
                "semantic control input",
                _CONTROL_INPUT_FIELDS,
            )
        elif method == "send_text_event":
            kind = params.get("event_kind") if isinstance(params, Mapping) else None
            fields = _TEXT_EVENT_FIELDS.get(kind)
            if fields is None:
                raise ValueError(
                    "text event_kind must be 2 PLACE, 3 EXTEND, 4 SCROLL, 5 FOLLOW, "
                    "6 SELECT, 7 OPEN, 8 EXPAND, 9 COLLAPSE, or 10 CHECK"
                )
            params = _wire_object(params, "text control input", fields)
        elif method == "send_pointer":
            params = _wire_object(params, "pointer input", _POINTER_INPUT_FIELDS)
        generation = self._required_generation(params)
        request_ack = (
            self._required_display_pair(params)
            if method in _DISPLAY_BOUND_INPUT_METHODS
            else self._optional_display_pair(params)
        )
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
            if method == "send_control_event":
                return self.machine.send_control_event(
                    params["owner_id"],
                    params["owner_generation"],
                    params["control_id"],
                    modifiers=params["modifiers"],
                    **common,
                )
            if method == "send_text_event":
                tail = {
                    name: params[name]
                    for name in (
                        "content_revision",
                        "item_key",
                        "scalar_offset",
                        "wheel_x",
                        "wheel_y",
                    )
                    if name in params
                }
                return self.machine.send_control_event(
                    params["owner_id"],
                    params["owner_generation"],
                    params["control_id"],
                    event_kind=params["event_kind"],
                    modifiers=params["modifiers"],
                    **tail,
                    **common,
                )
            if method == "send_pointer":
                return self.machine.send_pointer(
                    params["x"],
                    params["y"],
                    buttons=params["buttons"],
                    modifiers=params["modifiers"],
                    kind=params["kind"],
                    wheel_x=params["wheel_x"],
                    wheel_y=params["wheel_y"],
                    **common,
                )
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
        if method == "start_phase_profile":
            params = _wire_object(
                params,
                "phase profile start",
                ("generation", "address", "max_events"),
            )
            return self.machine.start_phase_profile(
                params["address"],
                params["max_events"],
                generation=params["generation"],
            )
        if method == "phase_profile":
            _wire_object(params, "phase profile snapshot", ())
            return self.machine.phase_profile()
        if method == "stop_phase_profile":
            _wire_object(params, "phase profile stop", ())
            return self.machine.stop_phase_profile()
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
        if method == "display_resource_chunk":
            return self._display_resource_chunk_for_connection(
                params,
                connection_id,
            )
        if method in {
            "send_text",
            "send_key",
            "send_control_event",
            "send_text_event",
            "send_pointer",
            "resize",
        }:
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
