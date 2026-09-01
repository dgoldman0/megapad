"""Shared single-owner MegaPad runtime and local JSON control protocol."""

from __future__ import annotations

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

from megapad64 import Megapad64Error
from rich_terminal import DriverStatus
from rich_terminal.apt1 import UINT32_MAX, UINT64_MAX
from rich_terminal.retained_view import (
    INT32_MAX,
    INT32_MIN,
    INT64_MAX,
    INT64_MIN,
    DisplayScope,
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    MeterDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    StatusDraw,
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
)
from rich_terminal.retained_scene import (
    ControlKind,
    ControlState,
    ObjectBounds,
    Point,
    RGBA,
    validate_control_shape,
)
from rich_terminal.semantic_content import (
    SemanticTextContent,
    decode_semantic_text_content,
    encode_semantic_text_content,
)
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

_PHASE_EVENT_PHASE_MASK = 0xFF
_PHASE_EVENT_SEQUENCE_SHIFT = 8
_PHASE_PROFILE_SCHEMA = "megapad.guest-phase-events"
_PHASE_PROFILE_MAX_EVENTS = 65_536


@dataclass
class _PhaseEventProfile:
    address: int
    max_events: int
    machine_generation: int
    batch_step_bound: int
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


def _semantic_content_to_wire(content: SemanticTextContent) -> str:
    """Carry the one canonical STX1 schema through JSON without restating it."""

    payload = encode_semantic_text_content(content)
    return base64.b64encode(payload).decode("ascii")


def _bounds_to_wire(bounds: ObjectBounds) -> list[int]:
    if not isinstance(bounds, ObjectBounds):
        raise TypeError("bounds must be ObjectBounds")
    return [bounds.left, bounds.top, bounds.right, bounds.bottom]


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
    content: SemanticTextContent,
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
        | ReadoutDraw
        | MeterDraw
        | StatusDraw
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
    if isinstance(draw, MenuBarDraw):
        return {
            "kind": "menu_bar",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": [
                draw.bounds.left,
                draw.bounds.top,
                draw.bounds.right,
                draw.bounds.bottom,
            ],
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
            "bounds": [
                draw.bounds.left,
                draw.bounds.top,
                draw.bounds.right,
                draw.bounds.bottom,
            ],
            "content_stx1_base64": _semantic_content_to_wire(draw.content),
        }
    if isinstance(draw, TabSetDraw):
        return {
            "kind": "tabset",
            "control_id": draw.control_id,
            "state": int(draw.state),
            "order": draw.order,
            "z_order": draw.z_order,
            "bounds": [
                draw.bounds.left,
                draw.bounds.top,
                draw.bounds.right,
                draw.bounds.bottom,
            ],
            "tabs": [_tab_to_wire(tab) for tab in draw.tabs],
        }
    raise TypeError("retained draw is outside the shared-viewer vocabulary")


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
                "draws": [_retained_draw_to_wire(draw) for draw in region.draws],
            }
            for region in plane.regions
        ],
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
    | ReadoutDraw
    | MeterDraw
    | StatusDraw
    | MenuBarDraw
    | TextAreaDraw
    | TextGridDraw
    | TabSetDraw
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
    raise ValueError(f"{name} kind is not a retained draw kind")


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
        draws = [
            _retained_draw_from_wire(
                raw_draw,
                f"retained region {region_index} draw {draw_index}",
            )
            for draw_index, raw_draw in enumerate(draws_wire)
        ]
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
        host_profile: bool = False,
    ):
        if not isinstance(host_profile, bool):
            raise TypeError("host_profile must be a boolean")
        self.session = session
        self.idle_tick_cycles = int(idle_tick_cycles)
        self.idle_sleep_s = float(idle_sleep_s)
        self._host_profile_enabled = host_profile
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

    def _phase_profile_address_valid(self, address: int) -> bool:
        """Restrict diagnostics to regions that may hold Forth variables."""

        system = self.session.system
        ram_size = int(system.ram_size)
        if 0 <= address and address + 8 <= ram_size:
            return True
        if not int(system.ext_mem_size):
            return False
        return int(system.ext_mem_base) <= address and address + 8 <= int(
            system.ext_mem_end
        )

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
                self.session.system.cpu.mem_read64(normalized_address),
                "phase profile event",
                minimum=0,
                maximum=UINT64_MAX,
            )
            self._phase_profile = _PhaseEventProfile(
                address=normalized_address,
                max_events=normalized_capacity,
                machine_generation=self._reset_generation,
                batch_step_bound=int(self.session.batch_steps),
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
                self.session.system.cpu.mem_read64(profile.address),
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

    def start(self):
        with self.lock:
            if self._thread is not None:
                return
            self.session.boot()
            if self._host_profile_enabled:
                self.session.system.start_host_profile()
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
            self._phase_profile = None
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
                            step_lower_bound = self.total_steps
                            self.total_steps += executed
                            self.total_batches += 1
                            self._sample_phase_profile(
                                step_lower_bound,
                                self.total_steps,
                                source="run_batch",
                                batch_index=self.total_batches,
                            )
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

        # Scalar CPU reads alias unmapped addresses into Bank 0, so validate
        # headers against the only two regions where Forth may build words.
        # ENTER/LEAVE-USERLAND can make the link chain alternate between them.
        system = self.session.system
        regions = [("ram", 0, int(system.ram_size))]
        if system.ext_mem_size:
            regions.append(
                ("ext", int(system.ext_mem_base), int(system.ext_mem_end))
            )

        def containing_region(address: int, count: int):
            if address < 0 or count < 0:
                return None
            end = address + count
            if end < address or end > 1 << 64:
                return None
            matches = [
                region
                for region in regions
                if region[1] <= address and end <= region[2]
            ]
            return matches[0] if len(matches) == 1 else None

        words = []
        seen = set()
        try:
            entry = int(cpu.mem_read64(latest_variable))
            here = int(cpu.mem_read64(here_variable))
            active_regions = [
                region
                for region in regions
                if region[1] <= here < region[2]
            ]
            if not active_regions:
                active_regions = [
                    region for region in regions if here == region[2]
                ]
            if len(active_regions) != 1:
                return [], 0
            ceilings = {name: limit for name, _base, limit in regions}
            ceilings[active_regions[0][0]] = here
            while entry:
                if entry in seen:
                    return words, 0
                region = containing_region(entry, 9)
                if region is None:
                    return words, 0
                region_name, _region_base, _region_limit = region
                upper = ceilings[region_name]
                if entry + 9 > upper:
                    return words, 0
                seen.add(entry)
                flags_len = int(cpu.mem_read8(entry + 8))
                name_len = flags_len & 0x7F
                code = entry + 9 + name_len
                if (
                    code > upper
                    or containing_region(entry, 9 + name_len) != region
                ):
                    return words, 0
                name = bytes(
                    int(cpu.mem_read8(entry + 9 + index))
                    for index in range(name_len)
                ).decode("ascii", errors="replace")
                word = {
                    "name": name,
                    "header": entry,
                    "code": code,
                    "_region": region_name,
                    "_upper": upper,
                }
                if code + 17 <= upper:
                    prefix = bytes(
                        int(cpu.mem_read8(code + index)) for index in range(3)
                    )
                    suffix = bytes(
                        int(cpu.mem_read8(code + 11 + index))
                        for index in range(6)
                    )
                    if (
                        prefix == b"\xf0\x60\x10"
                        and suffix == b"\x67\xe0\x08\x54\xe1\x0e"
                    ):
                        data_address = sum(
                            int(cpu.mem_read8(code + 3 + index)) << (index * 8)
                            for index in range(8)
                        )
                        if containing_region(data_address, 8) is not None:
                            word["data_address"] = data_address
                            word["value"] = int(cpu.mem_read64(data_address))
                words.append(word)
                ceilings[region_name] = entry
                next_entry = int(cpu.mem_read64(entry))
                entry = next_entry
        except (IndexError, Megapad64Error, RuntimeError, ValueError):
            return words, 0

        # The physical end safely bounds an inactive region during traversal,
        # but it is too broad for instruction-address lookup.  KDOS records
        # each inactive dictionary's exact saved HERE before switching banks.
        saved_here_words = {
            "ram": "SYS-HERE-SAVE",
            "ext": "U-DICT-HERE",
        }
        active_region = active_regions[0][0]
        for region_name, base, limit in regions:
            if region_name == active_region:
                continue
            newest = next(
                (word for word in words if word["_region"] == region_name),
                None,
            )
            saved_candidates = [
                word
                for word in words
                # KDOS owns the oldest Bank-0 definition; later shadows are
                # ordinary Forth words, not dictionary-switch state.
                if word["_region"] == "ram"
                and word["name"].upper() == saved_here_words[region_name]
                and "value" in word
            ]
            saved = int(saved_candidates[-1]["value"]) if saved_candidates else 0
            if newest is None or saved == 0:
                continue
            if not (base <= saved <= limit and newest["code"] <= saved):
                return words, 0
            newest["_upper"] = saved
        return words, here

    @staticmethod
    def _forth_word_at(words: list[dict], here: int, address: int) -> dict | None:
        for word in words:
            code = word["code"]
            upper = word.get("_upper", here)
            if code <= address < upper:
                return {
                    "name": word["name"],
                    "header": word["header"],
                    "code": code,
                    "offset": address - code,
                }
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
                    found[key] = {
                        field: value
                        for field, value in word.items()
                        if not field.startswith("_")
                    }
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
            rich_terminal_driver = self.session.rich_terminal_driver
            rich_terminal_core = (
                None
                if rich_terminal_driver is None
                else rich_terminal_driver.core
            )
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
                                rich_terminal_core.frame_bytes_received_by_type.items()
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
            if self._host_profile_enabled:
                result["host_profile"] = system.host_profile_snapshot()
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
                batch_executed = stats.instructions_executed
                executed += batch_executed
                if batch_executed == 0:
                    break
                step_lower_bound = self.total_steps
                self.total_steps += batch_executed
                self._sample_phase_profile(
                    step_lower_bound,
                    self.total_steps,
                    source="step",
                    batch_index=None,
                )
            self.last_stop_reason = stop_reason
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
            self._phase_profile = None
            try:
                self.session.reset()
                if self._host_profile_enabled:
                    self.session.system.start_host_profile()
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

    def send_control_event(
        self,
        owner_id: int,
        owner_generation: int,
        control_id: int,
        *,
        modifiers: int = 0,
        generation: int | None = None,
        display_authorized: bool = False,
        display_lease_ack: tuple[int, DisplayScope] | None = None,
        display_request_ack: tuple[int, DisplayScope] | None = None,
    ) -> dict:
        """Forward one owner-qualified activation under the display lease."""

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
                    modifiers=normalized_modifiers,
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
        if method == "send_control_event":
            params = _wire_object(
                params,
                "semantic control input",
                (
                    "generation",
                    "display_offer_id",
                    "display_scope",
                    "owner_id",
                    "owner_generation",
                    "control_id",
                    "modifiers",
                ),
            )
        generation = self._required_generation(params)
        request_ack = (
            self._required_display_pair(params)
            if method == "send_control_event"
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
        if method in {"send_text", "send_key", "send_control_event", "resize"}:
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
