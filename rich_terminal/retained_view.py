"""Renderer-facing generic draw projection of one immutable terminal composite.

The retained scene model deliberately remains richer than any one renderer
slice. This module is the fail-closed boundary for the first visible slice: it
copies only the active, physically visible root draw values needed by a view
sink and preserves their exact composite scope and deterministic draw order.
Hidden rebuild targets never cross this boundary.
"""

from __future__ import annotations

import operator
from collections.abc import Mapping
from dataclasses import dataclass

from .apt1 import UINT32_MAX, UINT64_MAX
from .cell_model import TerminalView
from .output_coordinator import CompositeTerminalView
from .retained_model import OwnerIdentity
from .retained_scene import (
    CONTROL_STATE_MASK,
    GLYPH_RUN_ATTRIBUTE_MASK,
    ControlDefinition,
    ControlKind,
    ControlState,
    GroupBody,
    GlyphRunBody,
    MeterBody,
    ObjectBounds,
    ObjectDefinition,
    OwnerScene,
    Point,
    PolylineBody,
    ReadoutBody,
    RGBA,
    RegionDefinition,
    RetainedScene,
    SceneModelState,
    StatusBody,
    validate_control_shape,
)
from .semantic_content import SemanticTextContent
from .update_authority import TerminalGeometry


INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
INT64_MIN = -(1 << 63)
INT64_MAX = (1 << 63) - 1


class RetainedViewError(ValueError):
    """The immutable composite cannot be consumed by the draw-plane slice."""


def _integer(name: str, value, *, minimum: int, maximum: int) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not minimum <= result <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return int(result)


def _boolean(name: str, value) -> bool:
    if not isinstance(value, bool):
        raise TypeError(f"{name} must be bool")
    return value


_CONTROL_ALLOWED_STATES = {
    ControlKind.MENU_BAR: ControlState.VISIBLE | ControlState.ENABLED,
    ControlKind.MENU: (
        ControlState.VISIBLE
        | ControlState.ENABLED
        | ControlState.OPEN
        | ControlState.SELECTED
    ),
    ControlKind.MENU_ITEM: (
        ControlState.VISIBLE
        | ControlState.ENABLED
        | ControlState.SELECTED
        | ControlState.CHECKED
    ),
    ControlKind.MENU_SEPARATOR: ControlState.VISIBLE,
    ControlKind.TEXT_AREA: (
        ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
    ),
    ControlKind.TEXT_GRID: (
        ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
    ),
    ControlKind.TABSET: ControlState.VISIBLE | ControlState.ENABLED,
    ControlKind.TAB: (
        ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
    ),
}


def _control_state(
    name: str,
    value,
    kind: ControlKind,
    *,
    visible_draw: bool,
) -> ControlState:
    if isinstance(value, bool):
        raise TypeError(f"{name} must not be bool")
    try:
        state_bits = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be ControlState-compatible") from exc
    if state_bits < 0 or state_bits > 0xFFFF:
        raise ValueError(f"{name} must fit u16")
    state = ControlState(state_bits)
    if int(state) & ~int(CONTROL_STATE_MASK):
        raise ValueError(f"{name} contains reserved CONTROL-1 bits")
    if int(state) & ~int(_CONTROL_ALLOWED_STATES[kind]):
        raise ValueError(f"{name} contains bits not defined for {kind.name}")
    if state & (ControlState.OPEN | ControlState.SELECTED) and not (
        state & ControlState.VISIBLE and state & ControlState.ENABLED
    ):
        raise ValueError("open or selected controls must be visible and enabled")
    if visible_draw and not state & ControlState.VISIBLE:
        raise ValueError(f"{kind.name} draw must be visible")
    return state


def _control_text(name: str, value, *, nonempty: bool) -> str:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    if nonempty and not value:
        raise ValueError(f"{name} must be nonempty")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in value):
        raise ValueError(f"{name} contains a control character")
    try:
        value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc
    return value


@dataclass(frozen=True, slots=True)
class DisplayScope:
    """Exact immutable model scope carried to a physical view sink."""

    attachment_epoch: int
    session_id: int
    presentation_epoch: int
    model_revision: int
    geometry_generation: int
    cell_revision: int
    retained_revision: int | None

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("attachment_epoch", 1, UINT64_MAX),
            ("session_id", 1, UINT64_MAX),
            ("presentation_epoch", 0, UINT32_MAX),
            ("model_revision", 0, UINT64_MAX),
            ("geometry_generation", 0, UINT64_MAX),
            ("cell_revision", 0, UINT64_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        retained_revision = self.retained_revision
        if retained_revision is not None:
            object.__setattr__(
                self,
                "retained_revision",
                _integer(
                    "retained_revision",
                    retained_revision,
                    minimum=0,
                    maximum=UINT64_MAX,
                ),
            )
        if self.cell_revision > self.model_revision:
            raise ValueError("CELL revision cannot exceed the composite revision")
        if (
            self.retained_revision is not None
            and self.retained_revision > self.model_revision
        ):
            raise ValueError("retained revision cannot exceed the composite revision")


@dataclass(frozen=True, slots=True)
class GlyphRunDraw:
    """One visible styled glyph run with an exact GROUP bounds path."""

    object_id: int
    z_order: int
    bounds: ObjectBounds
    foreground: RGBA
    background: RGBA
    attributes: int
    text: str
    parent_bounds: tuple[ObjectBounds, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        if not isinstance(self.foreground, RGBA) or not isinstance(
            self.background, RGBA
        ):
            raise TypeError("glyph-run colors must be RGBA")
        attributes = _integer(
            "attributes", self.attributes, minimum=0, maximum=0xFFFF
        )
        if attributes & ~GLYPH_RUN_ATTRIBUTE_MASK:
            raise ValueError("attributes contain unsupported GLYPH_RUN bits")
        object.__setattr__(self, "attributes", attributes)
        if not isinstance(self.text, str):
            raise TypeError("text must be str")
        if "\0" in self.text or "\r" in self.text or "\n" in self.text:
            raise ValueError("glyph-run text contains NUL, CR, or LF")
        try:
            self.text.encode("utf-8", "strict")
        except UnicodeEncodeError as exc:
            raise ValueError("glyph-run text contains a non-scalar surrogate") from exc
        object.__setattr__(
            self,
            "parent_bounds",
            _object_bounds_path("parent_bounds", self.parent_bounds),
        )


def _object_bounds_path(name: str, value) -> tuple[ObjectBounds, ...]:
    bounds = tuple(value)
    if any(not isinstance(item, ObjectBounds) for item in bounds):
        raise TypeError(f"{name} must contain only ObjectBounds values")
    return tuple(
        ObjectBounds(item.left, item.top, item.right, item.bottom)
        for item in bounds
    )


@dataclass(frozen=True, slots=True)
class PolylineDraw:
    """One visible straight-segment path with renderer-owned rasterization."""

    object_id: int
    z_order: int
    bounds: ObjectBounds
    points: tuple[Point, ...]
    stroke_width: int
    color: RGBA
    closed: bool
    parent_bounds: tuple[ObjectBounds, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        object.__setattr__(
            self,
            "bounds",
            ObjectBounds(
                self.bounds.left,
                self.bounds.top,
                self.bounds.right,
                self.bounds.bottom,
            ),
        )
        points = tuple(self.points)
        if len(points) < 2 or any(not isinstance(point, Point) for point in points):
            raise ValueError("polyline draw requires at least two Point values")
        object.__setattr__(
            self,
            "points",
            tuple(Point(point.x, point.y) for point in points),
        )
        object.__setattr__(
            self,
            "stroke_width",
            _integer(
                "stroke_width",
                self.stroke_width,
                minimum=1,
                maximum=UINT32_MAX,
            ),
        )
        if not isinstance(self.color, RGBA):
            raise TypeError("color must be RGBA")
        if not isinstance(self.closed, bool):
            raise TypeError("closed must be bool")
        object.__setattr__(
            self,
            "parent_bounds",
            _object_bounds_path("parent_bounds", self.parent_bounds),
        )


@dataclass(frozen=True, slots=True)
class ReadoutDraw:
    """One canonical formatted instrument value."""

    object_id: int
    z_order: int
    bounds: ObjectBounds
    foreground: RGBA
    background: RGBA
    text: str
    parent_bounds: tuple[ObjectBounds, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        if not isinstance(self.foreground, RGBA) or not isinstance(self.background, RGBA):
            raise TypeError("readout colors must be RGBA")
        if not isinstance(self.text, str):
            raise TypeError("readout text must be str")
        if (
            not self.text
            or "\0" in self.text
            or "\r" in self.text
            or "\n" in self.text
        ):
            raise ValueError(
                "readout text must be nonempty and contain no NUL, CR, or LF"
            )
        try:
            self.text.encode("utf-8", "strict")
        except UnicodeEncodeError as exc:
            raise ValueError("readout text contains a non-scalar surrogate") from exc
        object.__setattr__(
            self,
            "parent_bounds",
            _object_bounds_path("parent_bounds", self.parent_bounds),
        )


@dataclass(frozen=True, slots=True)
class MeterDraw:
    """One bounded scalar meter with renderer-owned orientation and label."""

    object_id: int
    z_order: int
    bounds: ObjectBounds
    foreground: RGBA
    background: RGBA
    vertical: bool
    show_value: bool
    minimum: int
    maximum: int
    value: int
    parent_bounds: tuple[ObjectBounds, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        if not isinstance(self.foreground, RGBA) or not isinstance(
            self.background, RGBA
        ):
            raise TypeError("meter colors must be RGBA")
        object.__setattr__(self, "vertical", _boolean("vertical", self.vertical))
        object.__setattr__(
            self,
            "show_value",
            _boolean("show_value", self.show_value),
        )
        for name in ("minimum", "maximum", "value"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=INT64_MIN,
                    maximum=INT64_MAX,
                ),
            )
        if (
            self.minimum >= self.maximum
            or not self.minimum <= self.value <= self.maximum
        ):
            raise ValueError("meter range/value is invalid")
        object.__setattr__(
            self,
            "parent_bounds",
            _object_bounds_path("parent_bounds", self.parent_bounds),
        )


@dataclass(frozen=True, slots=True)
class StatusDraw:
    """One binary numeric indicator with a renderer-neutral shape."""

    object_id: int
    z_order: int
    bounds: ObjectBounds
    inactive: RGBA
    active: RGBA
    value: int
    shape: int
    parent_bounds: tuple[ObjectBounds, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "object_id",
            _integer("object_id", self.object_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        if not isinstance(self.inactive, RGBA) or not isinstance(self.active, RGBA):
            raise TypeError("status colors must be RGBA")
        object.__setattr__(
            self,
            "value",
            _integer("value", self.value, minimum=INT64_MIN, maximum=INT64_MAX),
        )
        object.__setattr__(
            self,
            "shape",
            _integer("shape", self.shape, minimum=0, maximum=2),
        )
        object.__setattr__(
            self,
            "parent_bounds",
            _object_bounds_path("parent_bounds", self.parent_bounds),
        )


@dataclass(frozen=True, slots=True)
class MenuItemDraw:
    """One visible semantic menu item with renderer-owned geometry."""

    control_id: int
    state: ControlState
    order: int
    label: str
    shortcut: str

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "control_id",
            _integer("control_id", self.control_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "state",
            _control_state(
                "state",
                self.state,
                ControlKind.MENU_ITEM,
                visible_draw=True,
            ),
        )
        object.__setattr__(
            self,
            "order",
            _integer("order", self.order, minimum=0, maximum=UINT32_MAX),
        )
        object.__setattr__(
            self,
            "label",
            _control_text("label", self.label, nonempty=True),
        )
        object.__setattr__(
            self,
            "shortcut",
            _control_text("shortcut", self.shortcut, nonempty=False),
        )


@dataclass(frozen=True, slots=True)
class MenuSeparatorDraw:
    """One visible semantic separator with renderer-owned geometry."""

    control_id: int
    state: ControlState
    order: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "control_id",
            _integer("control_id", self.control_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "state",
            _control_state(
                "state",
                self.state,
                ControlKind.MENU_SEPARATOR,
                visible_draw=True,
            ),
        )
        object.__setattr__(
            self,
            "order",
            _integer("order", self.order, minimum=0, maximum=UINT32_MAX),
        )


MenuEntryDraw = MenuItemDraw | MenuSeparatorDraw


def _semantic_order_key(
    draw: MenuEntryDraw | MenuDraw | TabDraw,
) -> tuple[int, int]:
    return draw.order, draw.control_id


@dataclass(frozen=True, slots=True)
class MenuDraw:
    """One visible menu; entries exist only while it is authoritatively open."""

    control_id: int
    state: ControlState
    order: int
    label: str
    entries: tuple[MenuEntryDraw, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "control_id",
            _integer("control_id", self.control_id, minimum=1, maximum=UINT64_MAX),
        )
        state = _control_state(
            "state",
            self.state,
            ControlKind.MENU,
            visible_draw=True,
        )
        object.__setattr__(self, "state", state)
        object.__setattr__(
            self,
            "order",
            _integer("order", self.order, minimum=0, maximum=UINT32_MAX),
        )
        object.__setattr__(
            self,
            "label",
            _control_text("label", self.label, nonempty=True),
        )
        entries = tuple(self.entries)
        if any(not isinstance(entry, (MenuItemDraw, MenuSeparatorDraw)) for entry in entries):
            raise TypeError("entries must contain only semantic menu-entry draws")
        if not state & ControlState.OPEN and entries:
            raise ValueError("a closed menu cannot contain visible entries")
        if len({entry.order for entry in entries}) != len(entries):
            raise ValueError("menu entry order is duplicated")
        if tuple(sorted(entries, key=_semantic_order_key)) != entries:
            raise ValueError("menu entries are not in semantic order")
        if (
            sum(
                isinstance(entry, MenuItemDraw)
                and bool(entry.state & ControlState.SELECTED)
                for entry in entries
            )
            > 1
        ):
            raise ValueError("MENU has multiple selected items")
        object.__setattr__(self, "entries", entries)


@dataclass(frozen=True, slots=True)
class MenuBarDraw:
    """One visible semantic menu root anchored in UNORM32 region geometry."""

    control_id: int
    state: ControlState
    order: int
    z_order: int
    bounds: ObjectBounds
    menus: tuple[MenuDraw, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "control_id",
            _integer("control_id", self.control_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "state",
            _control_state(
                "state",
                self.state,
                ControlKind.MENU_BAR,
                visible_draw=True,
            ),
        )
        order = _integer("order", self.order, minimum=0, maximum=UINT32_MAX)
        if order != 0:
            raise ValueError("MENU_BAR draw order must be zero")
        object.__setattr__(self, "order", order)
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        menus = tuple(self.menus)
        if any(not isinstance(menu, MenuDraw) for menu in menus):
            raise TypeError("menus must contain only MenuDraw values")
        if len({menu.order for menu in menus}) != len(menus):
            raise ValueError("menu order is duplicated")
        if tuple(sorted(menus, key=_semantic_order_key)) != menus:
            raise ValueError("menus are not in semantic order")
        if sum(bool(menu.state & ControlState.OPEN) for menu in menus) > 1:
            raise ValueError("MENU_BAR has multiple open menus")
        if sum(bool(menu.state & ControlState.SELECTED) for menu in menus) > 1:
            raise ValueError("MENU_BAR has multiple selected menus")
        control_ids = {self.control_id}
        for menu in menus:
            descendants = (menu.control_id,) + tuple(
                entry.control_id for entry in menu.entries
            )
            descendant_ids = set(descendants)
            if (
                len(descendant_ids) != len(descendants)
                or control_ids & descendant_ids
            ):
                raise ValueError("MENU_BAR control IDs are duplicated")
            control_ids.update(descendant_ids)
        object.__setattr__(self, "menus", menus)


def _root_draw_fields(
    kind: ControlKind,
    control_id,
    state,
    order,
    z_order,
    bounds,
    content: SemanticTextContent | None,
) -> tuple[int, ControlState, int, int, ObjectBounds, SemanticTextContent | None]:
    control_id = _integer(
        "control_id", control_id, minimum=1, maximum=UINT64_MAX
    )
    state = _control_state("state", state, kind, visible_draw=True)
    order = _integer("order", order, minimum=0, maximum=UINT32_MAX)
    z_order = _integer(
        "z_order", z_order, minimum=INT32_MIN, maximum=INT32_MAX
    )
    if not isinstance(bounds, ObjectBounds):
        raise TypeError("bounds must be ObjectBounds")
    bounds = ObjectBounds(bounds.left, bounds.top, bounds.right, bounds.bottom)
    if order:
        raise ValueError(f"{kind.name} draw order must be zero")
    if kind in (ControlKind.TEXT_AREA, ControlKind.TEXT_GRID):
        if not isinstance(content, SemanticTextContent):
            raise TypeError(f"{kind.name} draw requires SemanticTextContent")
    elif content is not None:
        raise ValueError(f"{kind.name} draw carries no semantic text content")
    return control_id, state, order, z_order, bounds, content


@dataclass(frozen=True, slots=True)
class TextAreaDraw:
    """One visible logical text area anchored in region-relative geometry."""

    control_id: int
    state: ControlState
    order: int
    z_order: int
    bounds: ObjectBounds
    content: SemanticTextContent

    def __post_init__(self) -> None:
        control_id, state, order, z_order, bounds, content = _root_draw_fields(
            ControlKind.TEXT_AREA,
            self.control_id,
            self.state,
            self.order,
            self.z_order,
            self.bounds,
            self.content,
        )
        assert content is not None
        object.__setattr__(self, "control_id", control_id)
        object.__setattr__(self, "state", state)
        object.__setattr__(self, "order", order)
        object.__setattr__(self, "z_order", z_order)
        object.__setattr__(self, "bounds", bounds)
        object.__setattr__(self, "content", content)


@dataclass(frozen=True, slots=True)
class TextGridDraw:
    """One visible logical text grid anchored in region-relative geometry."""

    control_id: int
    state: ControlState
    order: int
    z_order: int
    bounds: ObjectBounds
    content: SemanticTextContent

    def __post_init__(self) -> None:
        control_id, state, order, z_order, bounds, content = _root_draw_fields(
            ControlKind.TEXT_GRID,
            self.control_id,
            self.state,
            self.order,
            self.z_order,
            self.bounds,
            self.content,
        )
        assert content is not None
        object.__setattr__(self, "control_id", control_id)
        object.__setattr__(self, "state", state)
        object.__setattr__(self, "order", order)
        object.__setattr__(self, "z_order", z_order)
        object.__setattr__(self, "bounds", bounds)
        object.__setattr__(self, "content", content)


@dataclass(frozen=True, slots=True)
class TabDraw:
    """One visible semantic tab with renderer-owned child geometry."""

    control_id: int
    state: ControlState
    order: int
    label: str
    shortcut: str

    def __post_init__(self) -> None:
        control_id = _integer(
            "control_id", self.control_id, minimum=1, maximum=UINT64_MAX
        )
        state = _control_state(
            "state", self.state, ControlKind.TAB, visible_draw=True
        )
        order = _integer("order", self.order, minimum=0, maximum=UINT32_MAX)
        label = _control_text("label", self.label, nonempty=True)
        shortcut = _control_text("shortcut", self.shortcut, nonempty=False)
        object.__setattr__(self, "control_id", control_id)
        object.__setattr__(self, "state", state)
        object.__setattr__(self, "order", order)
        object.__setattr__(self, "label", label)
        object.__setattr__(self, "shortcut", shortcut)


@dataclass(frozen=True, slots=True)
class TabSetDraw:
    """One visible tabset root and its visible tabs in semantic order."""

    control_id: int
    state: ControlState
    order: int
    z_order: int
    bounds: ObjectBounds
    tabs: tuple[TabDraw, ...]

    def __post_init__(self) -> None:
        control_id, state, order, z_order, bounds, content = _root_draw_fields(
            ControlKind.TABSET,
            self.control_id,
            self.state,
            self.order,
            self.z_order,
            self.bounds,
            None,
        )
        assert content is None
        object.__setattr__(self, "control_id", control_id)
        object.__setattr__(self, "state", state)
        object.__setattr__(self, "order", order)
        object.__setattr__(self, "z_order", z_order)
        object.__setattr__(self, "bounds", bounds)
        tabs = tuple(self.tabs)
        if any(not isinstance(tab, TabDraw) for tab in tabs):
            raise TypeError("tabs must contain only TabDraw values")
        if len({tab.order for tab in tabs}) != len(tabs):
            raise ValueError("tab order is duplicated")
        if tuple(sorted(tabs, key=_semantic_order_key)) != tabs:
            raise ValueError("tabs are not in semantic order")
        if sum(bool(tab.state & ControlState.SELECTED) for tab in tabs) > 1:
            raise ValueError("TABSET has multiple selected tabs")
        tab_ids = tuple(tab.control_id for tab in tabs)
        if len(set(tab_ids)) != len(tab_ids) or self.control_id in tab_ids:
            raise ValueError("TABSET control IDs are duplicated")
        object.__setattr__(self, "tabs", tabs)


ObjectDraw = GlyphRunDraw | PolylineDraw | ReadoutDraw | MeterDraw | StatusDraw
SemanticRootDraw = MenuBarDraw | TextAreaDraw | TextGridDraw | TabSetDraw
RetainedDraw = ObjectDraw | SemanticRootDraw

_OBJECT_DRAW_TYPES = (
    GlyphRunDraw,
    PolylineDraw,
    ReadoutDraw,
    MeterDraw,
    StatusDraw,
)


def _semantic_draw_control_ids(draw: SemanticRootDraw) -> set[int]:
    control_ids = {draw.control_id}
    if isinstance(draw, MenuBarDraw):
        for menu in draw.menus:
            control_ids.add(menu.control_id)
            control_ids.update(entry.control_id for entry in menu.entries)
    elif isinstance(draw, TabSetDraw):
        control_ids.update(tab.control_id for tab in draw.tabs)
    return control_ids


def _draw_order_key(draw: RetainedDraw) -> tuple[int, int, int]:
    if isinstance(draw, _OBJECT_DRAW_TYPES):
        return draw.z_order, 0, draw.object_id
    return draw.z_order, 1, draw.control_id


@dataclass(frozen=True, slots=True)
class RetainedRegionDraw:
    """One visible region and its ordered generic draw values."""

    owner_id: int
    owner_generation: int
    region_id: int
    cell_x: int
    cell_y: int
    cell_cols: int
    cell_rows: int
    z_order: int
    clipped: bool
    draws: tuple[RetainedDraw, ...]

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "region_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        for name, minimum in (
            ("cell_x", 0),
            ("cell_y", 0),
            ("cell_cols", 1),
            ("cell_rows", 1),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT32_MAX),
            )
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX),
        )
        object.__setattr__(self, "clipped", _boolean("clipped", self.clipped))
        draws = tuple(self.draws)
        if any(
            not isinstance(
                draw,
                (
                    GlyphRunDraw,
                    PolylineDraw,
                    ReadoutDraw,
                    MeterDraw,
                    StatusDraw,
                    MenuBarDraw,
                    TextAreaDraw,
                    TextGridDraw,
                    TabSetDraw,
                ),
            )
            for draw in draws
        ):
            raise TypeError("draws contain a value outside the retained draw vocabulary")
        if tuple(sorted(draws, key=_draw_order_key)) != draws:
            raise ValueError("region draw values are not in back-to-front order")
        object.__setattr__(self, "draws", draws)


@dataclass(frozen=True, slots=True)
class RetainedDrawPlane:
    """The active generic draw plane for one composite revision."""

    retained_initialized: bool
    retained_visible: bool
    regions: tuple[RetainedRegionDraw, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "retained_initialized",
            _boolean("retained_initialized", self.retained_initialized),
        )
        object.__setattr__(
            self,
            "retained_visible",
            _boolean("retained_visible", self.retained_visible),
        )
        regions = tuple(self.regions)
        if any(not isinstance(region, RetainedRegionDraw) for region in regions):
            raise TypeError("regions must contain only RetainedRegionDraw values")
        if tuple(
            sorted(
                regions,
                key=lambda region: (region.z_order, region.owner_id, region.region_id),
            )
        ) != regions:
            raise ValueError("regions are not in back-to-front order")
        if not self.retained_visible and regions:
            raise ValueError("a hidden retained plane cannot contain draw regions")
        if self.retained_visible and not self.retained_initialized:
            raise ValueError("an uninitialized retained plane cannot be visible")
        control_ids_by_owner: dict[tuple[int, int], set[int]] = {}
        for region in regions:
            owner = region.owner_id, region.owner_generation
            owner_control_ids = control_ids_by_owner.setdefault(owner, set())
            for draw in region.draws:
                if isinstance(draw, _OBJECT_DRAW_TYPES):
                    continue
                draw_control_ids = _semantic_draw_control_ids(draw)
                if owner_control_ids & draw_control_ids:
                    raise ValueError("owner semantic control IDs are duplicated")
                owner_control_ids.update(draw_control_ids)
        object.__setattr__(self, "regions", regions)


def _effectively_visible(
    definition: ObjectDefinition,
    objects,
) -> bool:
    """Resolve object/group visibility iteratively without native recursion."""

    current = definition
    visited: set[int] = set()
    while True:
        if not current.visible:
            return False
        parent_id = current.parent_object_id
        if parent_id == 0:
            return True
        if parent_id in visited:
            raise RetainedViewError("retained object graph contains a cycle")
        visited.add(parent_id)
        parent = objects.get(parent_id)
        if parent is None:
            raise RetainedViewError("retained object refers to a missing parent")
        if parent.owner != definition.owner or parent.region_id != definition.region_id:
            raise RetainedViewError("retained object parent crosses owner or region scope")
        if not isinstance(parent.body, GroupBody):
            raise RetainedViewError("retained object parent is not a GROUP")
        current = parent


def _object_parent_bounds(
    definition: ObjectDefinition,
    objects,
) -> tuple[ObjectBounds, ...]:
    """Copy the root-to-leaf GROUP bounds path for one visible object."""

    current = definition
    reverse_path: list[ObjectBounds] = []
    visited: set[int] = set()
    while current.parent_object_id:
        parent_id = current.parent_object_id
        if parent_id in visited:
            raise RetainedViewError("retained object graph contains a cycle")
        visited.add(parent_id)
        parent = objects.get(parent_id)
        if parent is None:
            raise RetainedViewError("retained object refers to a missing parent")
        if parent.owner != definition.owner or parent.region_id != definition.region_id:
            raise RetainedViewError("retained object parent crosses owner or region scope")
        if not isinstance(parent.body, GroupBody):
            raise RetainedViewError("retained object parent is not a GROUP")
        reverse_path.append(parent.bounds)
        current = parent
    return tuple(
        ObjectBounds(bounds.left, bounds.top, bounds.right, bounds.bottom)
        for bounds in reversed(reverse_path)
    )


def _validate_owner_scope(owner_scene: OwnerScene, owner_key: int, view) -> None:
    owner = owner_scene.owner
    cell = view.cell
    assert cell is not None
    if not isinstance(owner, OwnerIdentity):
        raise RetainedViewError("retained owner scene has an invalid authority")
    try:
        normalized_owner_key = _integer(
            "owner map key",
            owner_key,
            minimum=1,
            maximum=UINT64_MAX,
        )
    except (TypeError, ValueError) as exc:
        raise RetainedViewError(f"retained owner map key is invalid: {exc}") from exc
    if normalized_owner_key != owner.owner_id:
        raise RetainedViewError("retained owner map key does not match owner identity")
    if (
        owner.session_id != cell.session_id
        or owner.presentation_epoch != view.presentation_epoch
    ):
        raise RetainedViewError("retained owner is outside the composite scope")


_ValidatedControl = tuple[
    ControlDefinition,
    ControlKind,
    ControlState,
    SemanticTextContent | None,
]
_ValidatedControlMap = dict[int, _ValidatedControl]


def _validate_control_value(
    definition: ControlDefinition,
) -> tuple[ControlKind, ControlState, SemanticTextContent | None]:
    if isinstance(definition.kind, bool):
        raise TypeError("control kind must not be bool")
    try:
        kind = ControlKind(definition.kind)
    except (TypeError, ValueError) as exc:
        raise ValueError("control kind is not a CONTROL-1 kind") from exc
    state = _control_state(
        "control state",
        definition.state,
        kind,
        visible_draw=False,
    )
    for name, minimum, maximum in (
        ("control_id", 1, UINT64_MAX),
        ("region_id", 1, UINT64_MAX),
        ("parent_control_id", 0, UINT64_MAX),
        ("order", 0, UINT32_MAX),
        ("z_order", INT32_MIN, INT32_MAX),
    ):
        _integer(
            name,
            getattr(definition, name),
            minimum=minimum,
            maximum=maximum,
        )
    label = _control_text("control label", definition.label, nonempty=False)
    shortcut = _control_text(
        "control shortcut",
        definition.shortcut,
        nonempty=False,
    )
    bounds = definition.bounds
    if bounds is not None:
        if not isinstance(bounds, ObjectBounds):
            raise TypeError("control bounds must be ObjectBounds or None")
        bounds = ObjectBounds(bounds.left, bounds.top, bounds.right, bounds.bottom)
    content = definition.content
    validate_control_shape(
        kind=kind,
        state=state,
        z_order=definition.z_order,
        parent_control_id=definition.parent_control_id,
        order=definition.order,
        bounds=bounds,
        label=label,
        shortcut=shortcut,
        content=content,
    )
    return kind, state, content


def _validate_control_graph(
    owner_scene: OwnerScene,
) -> tuple[
    _ValidatedControlMap,
    dict[int, tuple[int, ...]],
    dict[int, tuple[int, ...]],
]:
    controls = owner_scene.controls
    if not isinstance(controls, Mapping):
        raise RetainedViewError("retained control collection is not a map")

    validated: _ValidatedControlMap = {}
    for control_key, definition in controls.items():
        if not isinstance(definition, ControlDefinition):
            raise RetainedViewError("retained control map contains an invalid value")
        try:
            normalized_key = _integer(
                "control map key",
                control_key,
                minimum=1,
                maximum=UINT64_MAX,
            )
            kind, state, content = _validate_control_value(definition)
        except (AttributeError, TypeError, ValueError) as exc:
            raise RetainedViewError(f"retained control value is invalid: {exc}") from exc
        if normalized_key != definition.control_id:
            raise RetainedViewError(
                "retained control map key does not match control identity"
            )
        if definition.owner != owner_scene.owner:
            raise RetainedViewError("retained control owner identity is invalid")
        if definition.region_id not in owner_scene.regions:
            raise RetainedViewError("retained control refers to a missing region")
        validated[normalized_key] = definition, kind, state, content

    children: dict[int, list[int]] = {}
    sibling_orders: set[tuple[int, int]] = set()
    open_menu_by_bar: set[int] = set()
    selected_menu_by_bar: set[int] = set()
    selected_item_by_menu: set[int] = set()
    selected_tab_by_tabset: set[int] = set()
    roots_by_region: dict[int, list[int]] = {}
    root_kinds = {
        ControlKind.MENU_BAR,
        ControlKind.TEXT_AREA,
        ControlKind.TEXT_GRID,
        ControlKind.TABSET,
    }
    expected_parent = {
        ControlKind.MENU: ControlKind.MENU_BAR,
        ControlKind.MENU_ITEM: ControlKind.MENU,
        ControlKind.MENU_SEPARATOR: ControlKind.MENU,
        ControlKind.TAB: ControlKind.TABSET,
    }
    for control_id, (definition, kind, state, _) in validated.items():
        if kind in root_kinds:
            roots_by_region.setdefault(definition.region_id, []).append(control_id)
            continue
        parent = validated.get(definition.parent_control_id)
        expected_kind = expected_parent[kind]
        if parent is None or parent[1] is not expected_kind:
            raise RetainedViewError(
                f"retained {kind.name} parent is not a live {expected_kind.name}"
            )
        parent_definition = parent[0]
        if parent_definition.region_id != definition.region_id:
            raise RetainedViewError("retained control parent crosses region scope")
        order_key = definition.parent_control_id, definition.order
        if order_key in sibling_orders:
            raise RetainedViewError("retained control sibling order is duplicated")
        sibling_orders.add(order_key)
        children.setdefault(definition.parent_control_id, []).append(control_id)

        if kind is ControlKind.MENU:
            if state & ControlState.OPEN:
                if definition.parent_control_id in open_menu_by_bar:
                    raise RetainedViewError("retained MENU_BAR has multiple open menus")
                open_menu_by_bar.add(definition.parent_control_id)
            if state & ControlState.SELECTED:
                if definition.parent_control_id in selected_menu_by_bar:
                    raise RetainedViewError(
                        "retained MENU_BAR has multiple selected menus"
                    )
                selected_menu_by_bar.add(definition.parent_control_id)
        elif kind is ControlKind.MENU_ITEM and state & ControlState.SELECTED:
            if definition.parent_control_id in selected_item_by_menu:
                raise RetainedViewError("retained MENU has multiple selected items")
            selected_item_by_menu.add(definition.parent_control_id)
        elif kind is ControlKind.TAB and state & ControlState.SELECTED:
            if definition.parent_control_id in selected_tab_by_tabset:
                raise RetainedViewError("retained TABSET has multiple selected tabs")
            selected_tab_by_tabset.add(definition.parent_control_id)

    ordered_children = {
        parent_id: tuple(
            sorted(
                control_ids,
                key=lambda control_id: (
                    validated[control_id][0].order,
                    control_id,
                ),
            )
        )
        for parent_id, control_ids in children.items()
    }
    ordered_roots = {
        region_id: tuple(
            sorted(
                control_ids,
                key=lambda control_id: (
                    validated[control_id][0].z_order,
                    control_id,
                ),
            )
        )
        for region_id, control_ids in roots_by_region.items()
    }
    return validated, ordered_children, ordered_roots


def _project_menu_bar(
    root_id: int,
    controls: _ValidatedControlMap,
    children: dict[int, tuple[int, ...]],
) -> MenuBarDraw:
    root, root_kind, root_state, _ = controls[root_id]
    if root_kind is not ControlKind.MENU_BAR or root.bounds is None:
        raise RetainedViewError("semantic root is not a bounded MENU_BAR")

    menus: list[MenuDraw] = []
    for menu_id in children.get(root_id, ()):
        menu, menu_kind, menu_state, _ = controls[menu_id]
        if menu_kind is not ControlKind.MENU:
            raise RetainedViewError("semantic MENU_BAR child is not a MENU")
        if not menu_state & ControlState.VISIBLE:
            continue

        entries: list[MenuEntryDraw] = []
        if menu_state & ControlState.OPEN:
            for entry_id in children.get(menu_id, ()):
                entry, entry_kind, entry_state, _ = controls[entry_id]
                if not entry_state & ControlState.VISIBLE:
                    continue
                if entry_kind is ControlKind.MENU_ITEM:
                    entries.append(
                        MenuItemDraw(
                            control_id=entry.control_id,
                            state=entry_state,
                            order=entry.order,
                            label=entry.label,
                            shortcut=entry.shortcut,
                        )
                    )
                elif entry_kind is ControlKind.MENU_SEPARATOR:
                    entries.append(
                        MenuSeparatorDraw(
                            control_id=entry.control_id,
                            state=entry_state,
                            order=entry.order,
                        )
                    )
                else:
                    raise RetainedViewError("semantic MENU child is not a menu entry")
        menus.append(
            MenuDraw(
                control_id=menu.control_id,
                state=menu_state,
                order=menu.order,
                label=menu.label,
                entries=tuple(entries),
            )
        )

    return MenuBarDraw(
        control_id=root.control_id,
        state=root_state,
        order=root.order,
        z_order=root.z_order,
        bounds=root.bounds,
        menus=tuple(menus),
    )


def _project_text_root(
    root_id: int,
    controls: _ValidatedControlMap,
) -> TextAreaDraw | TextGridDraw:
    root, kind, state, content = controls[root_id]
    if root.bounds is None or content is None:
        raise RetainedViewError(f"semantic {kind.name} root is incomplete")
    draw_type = {
        ControlKind.TEXT_AREA: TextAreaDraw,
        ControlKind.TEXT_GRID: TextGridDraw,
    }.get(kind)
    if draw_type is None:
        raise RetainedViewError("semantic text root has the wrong control kind")
    return draw_type(
        control_id=root.control_id,
        state=state,
        order=root.order,
        z_order=root.z_order,
        bounds=root.bounds,
        content=content,
    )


def _project_tabset(
    root_id: int,
    controls: _ValidatedControlMap,
    children: dict[int, tuple[int, ...]],
) -> TabSetDraw:
    root, kind, state, _ = controls[root_id]
    if kind is not ControlKind.TABSET or root.bounds is None:
        raise RetainedViewError("semantic root is not a bounded TABSET")
    tabs: list[TabDraw] = []
    for tab_id in children.get(root_id, ()):
        tab, tab_kind, tab_state, _ = controls[tab_id]
        if tab_kind is not ControlKind.TAB:
            raise RetainedViewError("semantic TABSET child is not a TAB")
        if not tab_state & ControlState.VISIBLE:
            continue
        tabs.append(
            TabDraw(
                control_id=tab.control_id,
                state=tab_state,
                order=tab.order,
                label=tab.label,
                shortcut=tab.shortcut,
            )
        )
    return TabSetDraw(
        control_id=root.control_id,
        state=state,
        order=root.order,
        z_order=root.z_order,
        bounds=root.bounds,
        tabs=tuple(tabs),
    )


def _project_semantic_root(
    root_id: int,
    controls: _ValidatedControlMap,
    children: dict[int, tuple[int, ...]],
) -> SemanticRootDraw:
    kind = controls[root_id][1]
    if kind is ControlKind.MENU_BAR:
        return _project_menu_bar(root_id, controls, children)
    if kind in (ControlKind.TEXT_AREA, ControlKind.TEXT_GRID):
        return _project_text_root(root_id, controls)
    if kind is ControlKind.TABSET:
        return _project_tabset(root_id, controls, children)
    raise RetainedViewError(f"semantic root has unsupported kind {kind.name}")


def project_composite_draw_plane(
    view: CompositeTerminalView,
) -> tuple[DisplayScope, RetainedDrawPlane]:
    """Project one exact composite to generic renderer draw values.

    A physically visible unsupported drawing object or a visible nested glyph
    run is rejected. Invisible regions, objects, and group cascades do not become
    draw commands.  The hidden rebuild target is intentionally never visited.
    """

    if not isinstance(view, CompositeTerminalView):
        raise TypeError("view must be CompositeTerminalView")
    if not isinstance(view.geometry, TerminalGeometry):
        raise TypeError("composite geometry must be TerminalGeometry")
    cell = view.cell
    if cell is None:
        raise RetainedViewError("a display composite requires the CELL plane")
    if not isinstance(cell, TerminalView):
        raise TypeError("composite CELL plane must be TerminalView")
    if (
        cell.presentation_epoch != view.presentation_epoch
        or cell.cols != view.geometry.cols
        or cell.rows != view.geometry.rows
    ):
        raise RetainedViewError("CELL plane does not match composite scope or geometry")
    if cell.revision > view.revision:
        raise RetainedViewError("CELL plane revision exceeds composite revision")

    retained = view.retained
    if retained is not None:
        if not isinstance(retained, SceneModelState):
            raise TypeError("composite retained plane must be SceneModelState or None")
        if retained.geometry != view.geometry:
            raise RetainedViewError("retained plane does not match composite geometry")
        if retained.revision > view.revision:
            raise RetainedViewError("retained plane revision exceeds composite revision")
        if not isinstance(retained.active, RetainedScene) or not isinstance(
            retained.active.owners,
            Mapping,
        ):
            raise RetainedViewError("retained active target is not a scene map")

    scope = DisplayScope(
        attachment_epoch=cell.attachment_epoch,
        session_id=cell.session_id,
        presentation_epoch=view.presentation_epoch,
        model_revision=view.revision,
        geometry_generation=view.geometry.generation,
        cell_revision=cell.revision,
        retained_revision=None if retained is None else retained.revision,
    )
    if retained is None:
        return scope, RetainedDrawPlane(False, False, ())
    if retained.retained_visible and not retained.retained_initialized:
        raise RetainedViewError("retained state is visible before initialization")
    if not retained.retained_visible:
        return scope, RetainedDrawPlane(
            retained.retained_initialized,
            False,
            (),
        )

    projected_regions: list[RetainedRegionDraw] = []
    for owner_key, owner_scene in retained.active.owners.items():
        if not isinstance(owner_scene, OwnerScene):
            raise RetainedViewError("retained scene contains an invalid owner value")
        _validate_owner_scope(owner_scene, owner_key, view)
        owner = owner_scene.owner
        if not isinstance(owner_scene.regions, Mapping):
            raise RetainedViewError("retained region collection is not a map")
        if not isinstance(owner_scene.objects, Mapping):
            raise RetainedViewError("retained object collection is not a map")

        for region_key, region in owner_scene.regions.items():
            if not isinstance(region, RegionDefinition):
                raise RetainedViewError("retained region map contains an invalid value")
            try:
                normalized_region_key = _integer(
                    "region map key",
                    region_key,
                    minimum=1,
                    maximum=UINT64_MAX,
                )
            except (TypeError, ValueError) as exc:
                raise RetainedViewError(
                    f"retained region map key is invalid: {exc}"
                ) from exc
            if normalized_region_key != region.region_id or region.owner != owner:
                raise RetainedViewError("retained region map or owner identity is invalid")
            if region.geometry_generation != view.geometry.generation:
                raise RetainedViewError("retained region geometry stamp is stale")
            try:
                region.validate_geometry(view.geometry)
            except (TypeError, ValueError) as exc:
                raise RetainedViewError(str(exc)) from exc

        for object_key, definition in owner_scene.objects.items():
            if not isinstance(definition, ObjectDefinition):
                raise RetainedViewError("retained object map contains an invalid value")
            try:
                normalized_object_key = _integer(
                    "object map key",
                    object_key,
                    minimum=1,
                    maximum=UINT64_MAX,
                )
            except (TypeError, ValueError) as exc:
                raise RetainedViewError(
                    f"retained object map key is invalid: {exc}"
                ) from exc
            if normalized_object_key != definition.object_id or definition.owner != owner:
                raise RetainedViewError("retained object map or owner identity is invalid")
            if definition.region_id not in owner_scene.regions:
                raise RetainedViewError("retained object refers to a missing region")

        controls, control_children, control_roots = _validate_control_graph(owner_scene)
        for region in owner_scene.regions.values():
            if not region.visible:
                continue
            draws: list[RetainedDraw] = []
            for definition in owner_scene.objects.values():
                if definition.region_id != region.region_id:
                    continue
                if not _effectively_visible(definition, owner_scene.objects):
                    continue
                if isinstance(definition.body, GroupBody):
                    continue
                parent_bounds = _object_parent_bounds(
                    definition,
                    owner_scene.objects,
                )
                bounds = definition.bounds
                body = definition.body
                if isinstance(body, GlyphRunBody):
                    draws.append(
                        GlyphRunDraw(
                            object_id=definition.object_id,
                            z_order=definition.z_order,
                            bounds=bounds,
                            foreground=body.foreground,
                            background=body.background,
                            attributes=body.attributes,
                            text=body.text,
                            parent_bounds=parent_bounds,
                        )
                    )
                    continue
                if isinstance(body, PolylineBody):
                    draws.append(
                        PolylineDraw(
                            object_id=definition.object_id,
                            z_order=definition.z_order,
                            bounds=bounds,
                            points=body.points,
                            stroke_width=body.stroke_width,
                            color=body.color,
                            closed=body.closed,
                            parent_bounds=parent_bounds,
                        )
                    )
                    continue
                if isinstance(body, ReadoutBody):
                    try:
                        utf8_bound = _integer(
                            "owner UTF-8 usage",
                            owner_scene.usage.utf8_bytes,
                            minimum=0,
                            maximum=UINT64_MAX,
                        )
                        text = body.formatted_bytes(
                            min(utf8_bound, UINT32_MAX)
                        ).decode("utf-8", "strict")
                    except (AttributeError, TypeError, ValueError) as exc:
                        raise RetainedViewError(
                            f"READOUT object {definition.object_id} cannot be projected: {exc}"
                        ) from exc
                    draws.append(
                        ReadoutDraw(
                            object_id=definition.object_id,
                            z_order=definition.z_order,
                            bounds=bounds,
                            foreground=body.foreground,
                            background=body.background,
                            text=text,
                            parent_bounds=parent_bounds,
                        )
                    )
                    continue
                if isinstance(body, MeterBody):
                    draws.append(
                        MeterDraw(
                            object_id=definition.object_id,
                            z_order=definition.z_order,
                            bounds=bounds,
                            foreground=body.foreground,
                            background=body.background,
                            vertical=body.vertical,
                            show_value=body.show_value,
                            minimum=body.minimum,
                            maximum=body.maximum,
                            value=body.value,
                            parent_bounds=parent_bounds,
                        )
                    )
                    continue
                if isinstance(body, StatusBody):
                    draws.append(
                        StatusDraw(
                            object_id=definition.object_id,
                            z_order=definition.z_order,
                            bounds=bounds,
                            inactive=body.inactive,
                            active=body.active,
                            value=body.value,
                            shape=body.shape,
                            parent_bounds=parent_bounds,
                        )
                    )
                    continue
                raise RetainedViewError(
                    f"visible {definition.kind.name} object "
                    f"{definition.object_id} is unsupported by draw-plane rendering"
                )
            for control_id in control_roots.get(region.region_id, ()):
                if controls[control_id][2] & ControlState.VISIBLE:
                    draws.append(
                        _project_semantic_root(
                            control_id,
                            controls,
                            control_children,
                        )
                    )
            draws.sort(key=_draw_order_key)
            projected_regions.append(
                RetainedRegionDraw(
                    owner_id=owner.owner_id,
                    owner_generation=owner.owner_generation,
                    region_id=region.region_id,
                    cell_x=region.cell_x,
                    cell_y=region.cell_y,
                    cell_cols=region.cell_cols,
                    cell_rows=region.cell_rows,
                    z_order=region.z_order,
                    clipped=region.clipped,
                    draws=tuple(draws),
                )
            )

    projected_regions.sort(
        key=lambda region: (region.z_order, region.owner_id, region.region_id)
    )
    return scope, RetainedDrawPlane(
        retained_initialized=retained.retained_initialized,
        retained_visible=True,
        regions=tuple(projected_regions),
    )


__all__ = [
    "DisplayScope",
    "GlyphRunDraw",
    "MenuBarDraw",
    "MenuDraw",
    "MenuEntryDraw",
    "MenuItemDraw",
    "MenuSeparatorDraw",
    "MeterDraw",
    "ObjectDraw",
    "PolylineDraw",
    "ReadoutDraw",
    "RetainedDraw",
    "RetainedRegionDraw",
    "RetainedDrawPlane",
    "RetainedViewError",
    "SemanticRootDraw",
    "StatusDraw",
    "TabDraw",
    "TabSetDraw",
    "TextAreaDraw",
    "TextGridDraw",
    "project_composite_draw_plane",
]
