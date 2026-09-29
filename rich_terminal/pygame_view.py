"""Pygame compositor for the retained generic draw plane.

The caller owns CELL rendering and paints the cursor after this compositor.
"""

from __future__ import annotations

import operator
from collections.abc import Mapping
from dataclasses import dataclass, replace
from itertools import repeat

from . import text_rules
from .apt1 import UINT32_MAX, UINT64_MAX
from .retained_model import ResourceFormat
from .retained_scene import ControlKind, ControlState, ImageFit
from .retained_view import (
    GlyphRunDraw,
    ImageDraw,
    ImageResourceManifest,
    ItemViewDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    MeterDraw,
    PlotDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    StatusDraw,
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
    WaveformDraw,
    retained_draw_key,
)
from .semantic_content import (
    SemanticContentFlag,
    SemanticTextRole,
    SemanticTextState,
    TextStyle,
)
from .semantic_items import (
    ItemColumnKind,
    ItemRole,
    ItemState,
    ItemViewRole,
    card_field_width,
    card_row_count,
)

ATTR_BOLD = 0x01
ATTR_DIM = 0x02
ATTR_ITALIC = 0x04
ATTR_UNDERLINE = 0x08
ATTR_REVERSE = 0x20
ATTR_STRIKE = 0x40


@dataclass(frozen=True, slots=True)
class _WideRect:
    """Python-integer logical geometry; never passed directly into pygame."""

    left: int
    top: int
    width: int
    height: int

    @property
    def right(self) -> int:
        return self.left + self.width

    @property
    def bottom(self) -> int:
        return self.top + self.height

    @property
    def centerx(self) -> int:
        return self.left + self.width // 2

    @property
    def centery(self) -> int:
        return self.top + self.height // 2

    @property
    def size(self) -> tuple[int, int]:
        return self.width, self.height

    def move(self, x: int, y: int) -> _WideRect:
        return _WideRect(self.left + x, self.top + y, self.width, self.height)


def _wide_intersection(rect, clip) -> _WideRect:
    left = max(rect.left, clip.left)
    top = max(rect.top, clip.top)
    right = min(rect.right, clip.right)
    bottom = min(rect.bottom, clip.bottom)
    if left >= right or top >= bottom:
        return _WideRect(0, 0, 0, 0)
    return _WideRect(left, top, right - left, bottom - top)


def _bounded_pygame_rect(pygame_module, rect, clip):
    """Intersect in Python integers before constructing one SDL-backed Rect."""

    visible = _wide_intersection(rect, clip)
    if visible.width <= 0 or visible.height <= 0:
        return pygame_module.Rect(0, 0, 0, 0)
    return pygame_module.Rect(
        visible.left,
        visible.top,
        visible.width,
        visible.height,
    )


# The control palette is deliberately renderer-owned.  These values describe
# one restrained dark surface system rather than protocol state or application
# annotations; callers can continue to use the independent CELL palette.
_BAR_SURFACE = (22, 27, 35, 246)
_POPUP_SURFACE = (27, 33, 43, 255)
_BORDER = (94, 107, 126, 128)
_SHADOW = (0, 0, 0, 76)
_TITLE_IDLE = (38, 45, 57, 150)
_ROW_IDLE = (35, 42, 54, 90)
_ROW_HOVER = (53, 68, 91, 236)
_ROW_SELECTED = (37, 72, 126, 238)
_ROW_PRESSED = (42, 98, 190, 248)
_ACCENT = (78, 139, 246, 255)
_TEXT = (239, 243, 249)
_MUTED_TEXT = (157, 168, 184)
_DISABLED_TEXT = (103, 113, 128)
_SEPARATOR = (106, 118, 136, 112)
_COLLECTION_SURFACE = (18, 23, 31)
_COLLECTION_BORDER = (72, 84, 102)
_TEXT_SELECTION = (42, 75, 122)
_GRID_CELL = (26, 32, 42)
_GRID_HEADER = (34, 43, 57)
_GRID_UNAVAILABLE = (23, 28, 36)
_GRID_PRIMARY = (39, 69, 112)


@dataclass(frozen=True, slots=True)
class TextLook:
    """How a theme shows one meaning of text: a colour (None keeps the
    text's own), the bold and italic faces, and an underline colour."""

    color: tuple[int, int, int] | None = None
    bold: bool = False
    italic: bool = False
    underline: tuple[int, int, int] | None = None


# The reference theme (SEMANTIC-CONTENT-1): a colour for every meaning, the
# bold face for keywords, headings, and strong text, the italic face for
# comments and emphasis, an underline for links, and a red one for errors.
# A look never moves a character out of its slots.
REFERENCE_TEXT_THEME: dict[TextStyle, TextLook] = {
    TextStyle.KEYWORD: TextLook((110, 180, 250), bold=True),
    TextStyle.COMMENT: TextLook((135, 146, 160), italic=True),
    TextStyle.STRING: TextLook((222, 196, 132)),
    TextStyle.NUMBER: TextLook((196, 160, 250)),
    TextStyle.HEADING: TextLook((250, 170, 95), bold=True),
    TextStyle.EMPHASIS: TextLook((244, 214, 186), italic=True),
    TextStyle.STRONG: TextLook((255, 255, 255), bold=True),
    TextStyle.CODE: TextLook((140, 210, 140)),
    TextStyle.LINK: TextLook((100, 170, 250), underline=(100, 170, 250)),
    TextStyle.ERROR: TextLook((250, 125, 125), underline=(235, 75, 75)),
}


# Public renderer-cache handoff key.  It is exactly
# ``ImageResourceManifest.key`` and deliberately includes immutable content
# metadata, not only the owner-local authority tuple.  Session/presentation
# scope remains an outer cache concern because this mapping belongs to one
# exact display offer.
ImageSurfaceKey = tuple[
    int,
    int,
    int,
    ResourceFormat,
    int,
    int,
    int,
    bytes,
]


@dataclass(frozen=True, slots=True)
class ControlIdentity:
    """Exact semantic identity used only for renderer-local interaction state."""

    owner_id: int
    owner_generation: int
    control_id: int

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "control_id"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
            )


@dataclass(frozen=True, slots=True)
class PixelRect:
    """Immutable half-open integer geometry with no pygame object ownership."""

    left: int
    top: int
    right: int
    bottom: int

    def __post_init__(self) -> None:
        for name in ("left", "top", "right", "bottom"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0),
            )
        if self.left >= self.right or self.top >= self.bottom:
            raise ValueError("pixel rectangle must have positive width and height")

    @property
    def width(self) -> int:
        return self.right - self.left

    @property
    def height(self) -> int:
        return self.bottom - self.top

    def contains(self, x: int, y: int) -> bool:
        horizontal = _integer("x", x, minimum=-(1 << 63), maximum=(1 << 63) - 1)
        vertical = _integer("y", y, minimum=-(1 << 63), maximum=(1 << 63) - 1)
        return (
            self.left <= horizontal < self.right
            and self.top <= vertical < self.bottom
        )


@dataclass(frozen=True, slots=True)
class ControlHitTarget:
    """One effectively enabled control in deterministic painter order."""

    identity: ControlIdentity
    kind: ControlKind
    rect: PixelRect

    def __post_init__(self) -> None:
        if not isinstance(self.identity, ControlIdentity):
            raise TypeError("identity must be ControlIdentity")
        if isinstance(self.kind, bool):
            raise TypeError("kind must not be bool")
        try:
            kind = ControlKind(self.kind)
        except (TypeError, ValueError) as exc:
            raise ValueError("kind is not a semantic control kind") from exc
        if kind not in (
            ControlKind.MENU,
            ControlKind.MENU_ITEM,
            ControlKind.TAB,
        ):
            raise ValueError("only MENU, MENU_ITEM, and TAB can be hit targets")
        object.__setattr__(self, "kind", kind)
        if not isinstance(self.rect, PixelRect):
            raise TypeError("rect must be PixelRect")


@dataclass(frozen=True, slots=True)
class RegionOcclusion:
    """Region or popup coverage that blocks controls painted below it."""

    owner_id: int
    owner_generation: int
    region_id: int
    rect: PixelRect

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "region_id"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
            )
        if not isinstance(self.rect, PixelRect):
            raise TypeError("rect must be PixelRect")


@dataclass(frozen=True, slots=True)
class ControlSurface:
    """Renderer-laid-out control area that is not itself a target.

    A menu bar, tabset, disabled text root, or open popup blocks lower
    controls and never starts a raw pointer gesture: its pixels were laid out
    by the renderer, so the cell under them is not what the client drew.
    """

    owner_id: int
    owner_generation: int
    control_id: int
    rect: PixelRect

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "control_id"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
            )
        if not isinstance(self.rect, PixelRect):
            raise TypeError("rect must be PixelRect")


@dataclass(frozen=True, slots=True)
class TextPosition:
    """One STX1 position: a carried item key and a scalar boundary."""

    item_key: int
    scalar_offset: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "item_key",
            _integer("item_key", self.item_key, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "scalar_offset",
            _integer(
                "scalar_offset",
                self.scalar_offset,
                minimum=0,
                maximum=UINT32_MAX,
            ),
        )


@dataclass(frozen=True, slots=True)
class TextHitTarget:
    """One enabled TEXT_AREA or TEXT_GRID root and the layout it was painted with.

    ``rect`` is the visible root.  The anchor fields keep the unclipped root
    geometry because rows and columns are partitioned from it exactly as the
    paint pass partitioned them.  TEXT_AREA ``rows`` holds ``(row, item_key,
    text)`` for every carried row, laid out in ``direction`` exactly as the
    paint pass laid it out; TEXT_GRID ``cells`` holds ``(row, column,
    row_span, column_span, item_key, selectable)`` for every carried item
    that intersects the viewport.  ``links`` holds ``(item_key, start, end)``
    for each TEXT_AREA style run whose meaning is LINK, and ``read_only``
    whether the content is READ_ONLY, which decide when a press follows a
    link (SEMANTIC-CONTENT-1).
    """

    identity: ControlIdentity
    kind: ControlKind
    rect: PixelRect
    anchor_left: int
    anchor_top: int
    anchor_width: int
    anchor_height: int
    content_revision: int
    viewport_row: int
    viewport_column: int
    viewport_rows: int
    viewport_columns: int
    rows: tuple[tuple[int, int, str], ...] = ()
    cells: tuple[tuple[int, int, int, int, int, bool], ...] = ()
    direction: int = text_rules.DIRECTION_AUTO
    links: tuple[tuple[int, int, int], ...] = ()
    read_only: bool = False

    def __post_init__(self) -> None:
        if not isinstance(self.identity, ControlIdentity):
            raise TypeError("identity must be ControlIdentity")
        if isinstance(self.kind, bool):
            raise TypeError("kind must not be bool")
        kind = ControlKind(self.kind)
        if kind not in (ControlKind.TEXT_AREA, ControlKind.TEXT_GRID):
            raise ValueError("only TEXT_AREA and TEXT_GRID are text targets")
        object.__setattr__(self, "kind", kind)
        if not isinstance(self.rect, PixelRect):
            raise TypeError("rect must be PixelRect")
        for name in ("anchor_width", "anchor_height", "viewport_rows", "viewport_columns"):
            _integer(name, getattr(self, name), minimum=1)
        for name in ("content_revision",):
            _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX)
        _integer("direction", self.direction, minimum=0, maximum=2)
        object.__setattr__(self, "rows", tuple(sorted(self.rows)))
        object.__setattr__(self, "cells", tuple(self.cells))
        object.__setattr__(self, "links", tuple(self.links))
        object.__setattr__(self, "read_only", bool(self.read_only))

    def _index(self, origin: int, extent: int, count: int, value: int) -> int:
        """Invert ``_partition_edge``: the logical index whose span holds value."""

        index = ((value - origin) * count) // extent
        index = min(max(index, 0), count - 1)
        while index > 0 and _partition_edge(origin, extent, index, count) > value:
            index -= 1
        while (
            index + 1 < count
            and _partition_edge(origin, extent, index + 1, count) <= value
        ):
            index += 1
        return index

    def position_at(self, x: int, y: int, *, clamp: bool = False) -> TextPosition | None:
        """Map one physical point to the position this root painted there.

        With ``clamp`` a point outside the visible root is first moved to its
        nearest edge, as a drag that leaves the root does.
        """

        rect = self.rect
        if clamp:
            x = min(max(x, rect.left), rect.right - 1)
            y = min(max(y, rect.top), rect.bottom - 1)
        elif not rect.contains(x, y):
            return None
        row = self.viewport_row + self._index(
            self.anchor_top, self.anchor_height, self.viewport_rows, y
        )
        slot = self._index(self.anchor_left, self.anchor_width, self.viewport_columns, x)
        if self.kind is ControlKind.TEXT_GRID:
            column = self.viewport_column + slot
            for item_row, item_column, row_span, column_span, key, selectable in self.cells:
                if (
                    item_row <= row < item_row + row_span
                    and item_column <= column < item_column + column_span
                ):
                    return TextPosition(key, 0) if selectable else None
            return None
        # APT-1-TEXT Section 9.1: a point on a character names its start, and
        # past the content the end side names the row's end.
        above = None
        below = None
        for item_row, key, text in self.rows:
            if item_row == row:
                layout = text_rules.cached_row(text, self.direction, True)
                shift = _text_area_shift(layout, self.viewport_column, self.viewport_columns)
                return TextPosition(key, layout.position_at_column(slot - shift))
            if item_row < row:
                above = (key, len(text))
            elif below is None:
                below = key
        if above is not None:
            return TextPosition(above[0], above[1])
        if below is not None:
            return TextPosition(below, 0)
        return None

    def link_at(self, x: int, y: int) -> TextPosition | None:
        """The start of the TEXT_AREA character painted at one physical
        point, when a LINK run covers that character's first scalar."""

        if self.kind is not ControlKind.TEXT_AREA or not self.links:
            return None
        if not self.rect.contains(x, y):
            return None
        row = self.viewport_row + self._index(
            self.anchor_top, self.anchor_height, self.viewport_rows, y
        )
        slot = self._index(self.anchor_left, self.anchor_width, self.viewport_columns, x)
        for item_row, key, text in self.rows:
            if item_row != row:
                continue
            layout = text_rules.cached_row(text, self.direction, True)
            shift = _text_area_shift(layout, self.viewport_column, self.viewport_columns)
            placed = layout.character_at_column(slot - shift)
            if placed is None:
                return None
            for link_key, start, end in self.links:
                if link_key == key and start <= placed.start < end:
                    return TextPosition(key, placed.start)
            return None
        return None


@dataclass(frozen=True, slots=True)
class ItemPart:
    """One shown item as its view painted it: the item's area, and the
    disclosure mark and check box inside it when it has them."""

    item_key: int
    area: PixelRect
    selectable: bool
    disclosure: PixelRect | None = None
    expanded: bool = False
    check_box: PixelRect | None = None

    def __post_init__(self) -> None:
        _integer("item_key", self.item_key, minimum=1, maximum=UINT64_MAX)
        for name in ("area", "disclosure", "check_box"):
            value = getattr(self, name)
            if value is not None and not isinstance(value, PixelRect):
                raise TypeError(f"{name} must be PixelRect")
        if self.area is None:
            raise TypeError("area must be PixelRect")
        object.__setattr__(self, "selectable", bool(self.selectable))
        object.__setattr__(self, "expanded", bool(self.expanded))


@dataclass(frozen=True, slots=True)
class ItemHitTarget:
    """One enabled ITEM_VIEW root and the item areas its paint pass drew.

    ``item_at`` names what a press at one point asks for
    (SEMANTIC-CONTENT-1): the disclosure mark expands or collapses, the check
    box checks, and anywhere else on a selectable item selects it.
    """

    identity: ControlIdentity
    rect: PixelRect
    content_revision: int
    items: tuple[ItemPart, ...] = ()

    def __post_init__(self) -> None:
        if not isinstance(self.identity, ControlIdentity):
            raise TypeError("identity must be ControlIdentity")
        if not isinstance(self.rect, PixelRect):
            raise TypeError("rect must be PixelRect")
        _integer("content_revision", self.content_revision, minimum=1, maximum=UINT64_MAX)
        items = tuple(self.items)
        if any(not isinstance(item, ItemPart) for item in items):
            raise TypeError("items must contain only ItemPart values")
        object.__setattr__(self, "items", items)

    def item_at(self, x: int, y: int) -> tuple[str, int] | None:
        """``(action, item_key)`` for a press at one point, where action is
        "select", "expand", "collapse", or "check"; None for nothing."""

        if not self.rect.contains(x, y):
            return None
        for part in self.items:
            if not part.area.contains(x, y):
                continue
            if part.disclosure is not None and part.disclosure.contains(x, y):
                return ("collapse" if part.expanded else "expand", part.item_key)
            if part.check_box is not None and part.check_box.contains(x, y):
                return ("check", part.item_key)
            if part.selectable:
                return ("select", part.item_key)
            return None
        return None


@dataclass(frozen=True, slots=True)
class ResidualPoint:
    """A point showing CELL or residual content, named by its cell."""

    column: int
    row: int


HitMapEntry = (
    ControlHitTarget | RegionOcclusion | ControlSurface | TextHitTarget | ItemHitTarget
)
PointerTarget = ControlHitTarget | TextHitTarget | ItemHitTarget | ResidualPoint
HIT_MAP_ENTRY_TYPES = (
    ControlHitTarget,
    RegionOcclusion,
    ControlSurface,
    TextHitTarget,
    ItemHitTarget,
)


def _validated_hit_entries(hit_entries) -> tuple[HitMapEntry, ...]:
    entries = tuple(hit_entries)
    if any(not isinstance(entry, HIT_MAP_ENTRY_TYPES) for entry in entries):
        raise TypeError(
            "hit_entries must contain only ControlHitTarget, RegionOcclusion, "
            "ControlSurface, TextHitTarget, or ItemHitTarget values"
        )
    return entries


def hit_test_hit_map(
    hit_entries: tuple[HitMapEntry, ...],
    x: int,
    y: int,
) -> ControlHitTarget | None:
    """Resolve one painter-ordered immutable map without region click-through."""

    for entry in reversed(hit_entries):
        if not entry.rect.contains(x, y):
            continue
        if isinstance(entry, ControlHitTarget):
            return entry
        return None
    return None


def resolve_pointer(
    hit_entries: tuple[HitMapEntry, ...],
    x: int,
    y: int,
    *,
    cell_width: int,
    cell_height: int,
) -> PointerTarget | None:
    """Resolve where one point on the composed terminal surface may go.

    An activatable control or a text root wins in reverse painter order.  A
    control surface (menu bar, tabset, popup, disabled text root) swallows the
    point.  A region barrier, or no entry at all, means the point shows CELL
    or residual content, so a raw pointer gesture may name its cell.
    """

    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
    for entry in reversed(hit_entries):
        if not entry.rect.contains(x, y):
            continue
        if isinstance(entry, (ControlHitTarget, TextHitTarget, ItemHitTarget)):
            return entry
        if isinstance(entry, ControlSurface):
            return None
        break
    if x < 0 or y < 0:
        return None
    return ResidualPoint(x // cell_w, y // cell_h)


@dataclass(frozen=True, slots=True)
class PaintedDraw:
    """One draw of a composed frame, as a later partial repaint needs it.

    ``key`` is the draw's identity in its region.  ``extent`` holds every
    pixel the draw may change, or is None when it changes none.  ``entries``
    are its hit entries and ``popup_entries`` those of the popups it opened,
    which the region's popup pass adds after all of its draws.
    """

    key: tuple[str, int]
    extent: PixelRect | None
    entries: tuple[HitMapEntry, ...] = ()
    popup_entries: tuple[HitMapEntry, ...] = ()


@dataclass(frozen=True, slots=True)
class PaintedRegion:
    """One region of a composed frame: its full viewport, its occlusion
    entry, and its draws in painter order."""

    key: tuple[int, int, int]
    viewport: PixelRect | None
    occlusion: tuple[HitMapEntry, ...]
    draws: tuple[PaintedDraw, ...]

    def entries(self) -> tuple[HitMapEntry, ...]:
        """The region's hit entries in the composition's painter order."""

        return (
            *self.occlusion,
            *(entry for draw in self.draws for entry in draw.entries),
            *(entry for draw in self.draws for entry in draw.popup_entries),
        )


@dataclass(frozen=True, slots=True)
class CompositeDrawResult:
    """One completed paint pass and its immutable semantic hit map.

    ``hit_entries`` is stored in back-to-front painter order.  A region's
    barrier precedes its own controls, and each menu bar, tabset, or popup
    surface precedes its targets; an enabled text root is its own target.
    Reverse testing lets enabled controls win while blocking input through
    covered padding or disabled controls.  ``hit_targets`` remains a filtered
    inspection view; barriers are never represented as fake controls.
    """

    surface: object
    hit_entries: tuple[HitMapEntry, ...]
    # How the retained plane was painted, region by region, for a partial
    # repaint of the next frame; empty when no plane was composed.
    regions: tuple[PaintedRegion, ...] = ()

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "hit_entries",
            _validated_hit_entries(self.hit_entries),
        )
        regions = tuple(self.regions)
        if any(not isinstance(region, PaintedRegion) for region in regions):
            raise TypeError("regions must contain only PaintedRegion values")
        object.__setattr__(self, "regions", regions)

    @property
    def hit_targets(self) -> tuple[ControlHitTarget, ...]:
        """Control-only compatibility/inspection view of the exact hit map."""

        return tuple(
            entry
            for entry in self.hit_entries
            if isinstance(entry, ControlHitTarget)
        )

    def hit_test(self, x: int, y: int) -> ControlHitTarget | None:
        return hit_test_hit_map(self.hit_entries, x, y)


@dataclass(frozen=True, slots=True)
class _MenuMetrics:
    font_height: int
    horizontal_padding: int
    vertical_padding: int
    gap: int
    popup_padding: int
    check_column: int
    shortcut_gap: int
    title_height: int
    row_height: int
    separator_height: int
    corner_radius: int
    shadow_offset: int


@dataclass(frozen=True, slots=True)
class _MenuPopup:
    """One open popup deferred until its region's ordinary paint is complete."""

    anchor: _WideRect
    viewport: object
    menu: MenuDraw
    title: _WideRect
    metrics: _MenuMetrics
    root_enabled: bool


def _integer(name: str, value, *, minimum: int, maximum: int | None = None) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if result < minimum or (maximum is not None and result > maximum):
        upper = "unbounded" if maximum is None else str(maximum)
        raise ValueError(f"{name} must be between {minimum} and {upper}")
    return int(result)


def unorm_low_edge(value: int, extent: int) -> int:
    normalized = _integer("value", value, minimum=0, maximum=UINT32_MAX)
    pixels = _integer("extent", extent, minimum=0)
    return (normalized * pixels) // UINT32_MAX


def unorm_high_edge(value: int, extent: int) -> int:
    normalized = _integer("value", value, minimum=0, maximum=UINT32_MAX)
    pixels = _integer("extent", extent, minimum=0)
    numerator = normalized * pixels
    return (numerator + UINT32_MAX - 1) // UINT32_MAX


def _rgb(color):
    return color.red, color.green, color.blue


def _bounds_rect(pygame_module, parent_rect, bounds, cell_width, cell_height):
    """Resolve CELL_RECT32 without rewriting it to the current viewport."""

    return _WideRect(
        parent_rect.left + bounds.cell_x * cell_width,
        parent_rect.top + bounds.cell_y * cell_height,
        bounds.cell_cols * cell_width,
        bounds.cell_rows * cell_height,
    )


def _object_rect(pygame_module, region, region_rect, draw):
    cell_width = region_rect.width // region.logical_cols
    cell_height = region_rect.height // region.logical_rows
    parent = region_rect
    for bounds in draw.parent_bounds:
        parent = _bounds_rect(
            pygame_module,
            parent,
            bounds,
            cell_width,
            cell_height,
        )
    return _bounds_rect(
        pygame_module,
        parent,
        draw.bounds,
        cell_width,
        cell_height,
    )


def _region_viewport(pygame_module, surface, region, region_rect):
    """Resolve the independent physical clip in selected-surface cells."""

    viewport = surface.get_rect().clip(surface.get_clip())
    if not region.clipped:
        return viewport
    if region.clip_cols == 0:
        return pygame_module.Rect(0, 0, 0, 0)
    cell_width = region_rect.width // region.logical_cols
    cell_height = region_rect.height // region.logical_rows
    physical_clip = _WideRect(
        region.clip_x * cell_width,
        region.clip_y * cell_height,
        region.clip_cols * cell_width,
        region.clip_rows * cell_height,
    )
    return _bounded_pygame_rect(pygame_module, physical_clip, viewport)


_OBJECT_DRAWS = (
    GlyphRunDraw,
    PolylineDraw,
    ImageDraw,
    ReadoutDraw,
    MeterDraw,
    StatusDraw,
    PlotDraw,
    WaveformDraw,
)
_ROOT_CONTROLS = (TextAreaDraw, TextGridDraw, ItemViewDraw, TabSetDraw)


def _draw_extent(
    pygame_module,
    region,
    region_rect,
    viewport,
    draw,
    cell_width: int,
    cell_height: int,
    control_font,
) -> PixelRect | None:
    """Every pixel DRAW may change, from the rules its painter clips by.

    VIEWPORT is the region's viewport on the full frame.  An object paints
    within its object rectangle and a root control within its anchor.  A
    menu bar paints its anchor and the shadow below it, and while one of its
    menus is open, popups anywhere in the viewport.
    """

    if isinstance(draw, _OBJECT_DRAWS):
        painted = _object_rect(pygame_module, region, region_rect, draw)
    elif isinstance(draw, _ROOT_CONTROLS):
        painted = _bounds_rect(
            pygame_module,
            region_rect,
            draw.bounds,
            region_rect.width // region.logical_cols,
            region_rect.height // region.logical_rows,
        )
    elif isinstance(draw, MenuBarDraw):
        if any(menu.state & ControlState.OPEN for menu in draw.menus):
            painted = viewport
        else:
            anchor = _bounds_rect(
                pygame_module, region_rect, draw.bounds, cell_width, cell_height
            )
            shadow = _menu_metrics(control_font, cell_width, cell_height).shadow_offset
            painted = _WideRect(
                anchor.left, anchor.top, anchor.width, anchor.height + shadow
            )
    else:  # Legitimate newer kinds remain fail-closed until implemented.
        raise TypeError("unsupported retained draw value")
    visible = _bounded_pygame_rect(pygame_module, painted, viewport)
    if visible.width <= 0 or visible.height <= 0:
        return None
    return _pixel_rect(visible)


def _font_height(font, fallback: int) -> int:
    for accessor_name in ("get_linesize", "get_height"):
        accessor = getattr(font, accessor_name, None)
        if callable(accessor):
            try:
                value = operator.index(accessor())
            except (TypeError, ValueError):
                continue
            if value > 0:
                return int(value)
    size = getattr(font, "size", None)
    if callable(size):
        measured = size("Mg")
        if (
            isinstance(measured, (tuple, list))
            and len(measured) == 2
            and not isinstance(measured[1], bool)
        ):
            try:
                value = operator.index(measured[1])
            except TypeError:
                pass
            else:
                if value > 0:
                    return int(value)
    return fallback


def _text_width(font, text: str) -> int:
    size = getattr(font, "size", None)
    if callable(size):
        measured = size(text)
        if (
            isinstance(measured, (tuple, list))
            and len(measured) == 2
            and not isinstance(measured[0], bool)
        ):
            try:
                value = operator.index(measured[0])
            except TypeError:
                pass
            else:
                if value >= 0:
                    return int(value)
    raise TypeError(
        "control font must expose non-rendering size() text measurement"
    )


def _menu_metrics(font, cell_width: int, cell_height: int) -> _MenuMetrics:
    font_height = _font_height(font, cell_height)
    horizontal_padding = max(5, font_height // 2, cell_width // 3)
    vertical_padding = max(3, font_height // 4)
    gap = max(2, font_height // 5)
    popup_padding = max(4, font_height // 3)
    check_column = max(font_height, cell_width)
    shortcut_gap = max(8, font_height // 2)
    return _MenuMetrics(
        font_height=font_height,
        horizontal_padding=horizontal_padding,
        vertical_padding=vertical_padding,
        gap=gap,
        popup_padding=popup_padding,
        check_column=check_column,
        shortcut_gap=shortcut_gap,
        title_height=font_height + 2 * vertical_padding,
        row_height=font_height + 2 * vertical_padding,
        separator_height=max(5, font_height // 2),
        corner_radius=max(3, font_height // 3),
        shadow_offset=max(2, font_height // 5),
    )


def _rounded_rect(
    pygame_module,
    surface,
    rect,
    color,
    *,
    radius: int,
    width: int = 0,
) -> None:
    if rect.width <= 0 or rect.height <= 0:
        return
    radius = min(max(0, radius), rect.width // 2, rect.height // 2)
    rgba = tuple(color)
    if len(rgba) == 4 and rgba[3] == 0:
        return
    viewport = surface.get_rect().clip(surface.get_clip())
    visible = _bounded_pygame_rect(pygame_module, rect, viewport)
    if visible.width <= 0 or visible.height <= 0:
        return
    layer = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    # Only true edges within one corner/border influence radius of the visible
    # pixels.  Replace farther edges outside that influence band before the
    # geometry enters pygame's signed native Rect storage.
    maximum_margin = 2 * max(viewport.width, viewport.height, 1) + 2
    margin = min(max(radius, width) + 2, maximum_margin)
    safe_left = max(rect.left, visible.left - margin)
    safe_top = max(rect.top, visible.top - margin)
    safe_right = min(rect.right, visible.right + margin)
    safe_bottom = min(rect.bottom, visible.bottom + margin)
    shifted = pygame_module.Rect(
        safe_left - visible.left,
        safe_top - visible.top,
        safe_right - safe_left,
        safe_bottom - safe_top,
    )
    pygame_module.draw.rect(
        layer,
        rgba if len(rgba) == 4 else (*rgba[:3], 0xFF),
        shifted,
        width=width,
        border_radius=min(radius, maximum_margin),
    )
    surface.blit(layer, visible)


def _clip_line_segment(start, end, clip, padding: int):
    """Cohen-Sutherland clip in Python integers before calling pygame."""

    left = clip.left - padding
    top = clip.top - padding
    right = clip.right - 1 + padding
    bottom = clip.bottom - 1 + padding
    x0, y0 = start
    x1, y1 = end

    def code(x, y):
        result = 0
        if x < left:
            result |= 1
        elif x > right:
            result |= 2
        if y < top:
            result |= 4
        elif y > bottom:
            result |= 8
        return result

    for _ in range(16):
        code0 = code(x0, y0)
        code1 = code(x1, y1)
        if not (code0 | code1):
            return (x0, y0), (x1, y1)
        if code0 & code1:
            return None
        outside = code0 or code1
        if outside & 8:
            if y1 == y0:
                return None
            x = x0 + (x1 - x0) * (bottom - y0) // (y1 - y0)
            y = bottom
        elif outside & 4:
            if y1 == y0:
                return None
            x = x0 + (x1 - x0) * (top - y0) // (y1 - y0)
            y = top
        elif outside & 2:
            if x1 == x0:
                return None
            y = y0 + (y1 - y0) * (right - x0) // (x1 - x0)
            x = right
        else:
            if x1 == x0:
                return None
            y = y0 + (y1 - y0) * (left - x0) // (x1 - x0)
            x = left
        if outside == code0:
            if (x, y) == (x0, y0):
                return None
            x0, y0 = x, y
        else:
            if (x, y) == (x1, y1):
                return None
            x1, y1 = x, y
    return None


def _alpha_line(pygame_module, surface, color, start, end, *, width: int = 1) -> None:
    rgba = tuple(color)
    if len(rgba) == 4 and rgba[3] == 0:
        return
    visible = surface.get_rect().clip(surface.get_clip())
    if visible.width <= 0 or visible.height <= 0:
        return
    native_width = min(max(1, width), 2 * max(visible.width, visible.height) + 1)
    clipped = _clip_line_segment(start, end, visible, native_width)
    if clipped is None:
        return
    safe_start, safe_end = clipped
    if len(rgba) != 4 or rgba[3] == 0xFF:
        pygame_module.draw.line(
            surface,
            rgba[:3],
            safe_start,
            safe_end,
            native_width,
        )
        return
    layer = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    pygame_module.draw.line(
        layer,
        rgba,
        (safe_start[0] - visible.left, safe_start[1] - visible.top),
        (safe_end[0] - visible.left, safe_end[1] - visible.top),
        native_width,
    )
    surface.blit(layer, visible)


def _paint_text(
    pygame_module,
    surface,
    font,
    text: str,
    color,
    *,
    left: int | None = None,
    right: int | None = None,
    center_y: int,
) -> None:
    if not text:
        return
    viewport = surface.get_rect().clip(surface.get_clip())
    if viewport.width <= 0 or viewport.height <= 0:
        return
    tab_advance = max(1, _text_width(font, " ") * 4)
    text_width = sum(
        _scalar_advance(font, character, tab_advance)
        for character in _label_characters(text)
    )
    if right is not None:
        logical_left = right - text_width
        logical_right = right
    elif left is not None:
        logical_left = left
        logical_right = left + text_width
    else:
        raise ValueError("control text needs a left or right edge")
    font_height = _font_height(font, viewport.height)
    _paint_bounded_text(
        pygame_module,
        surface,
        font,
        text,
        color,
        viewport,
        left=logical_left,
        right=logical_right,
        top=center_y - font_height // 2,
        bottom=center_y - font_height // 2 + font_height,
        tab_advance=tab_advance,
    )


def _partition_edge(origin: int, extent: int, index: int, count: int) -> int:
    """Map one logical edge with exact integer arithmetic and no float drift."""

    return origin + (index * extent) // count


def _semantic_root_rects(
    pygame_module,
    surface,
    region,
    region_rect,
    bounds,
):
    """Return stable root geometry and its physical clip without reflowing it."""

    cell_width = region_rect.width // region.logical_cols
    cell_height = region_rect.height // region.logical_rows
    anchor = _bounds_rect(
        pygame_module,
        region_rect,
        bounds,
        cell_width,
        cell_height,
    )
    viewport = _region_viewport(
        pygame_module,
        surface,
        region,
        region_rect,
    )
    return anchor, _bounded_pygame_rect(pygame_module, anchor, viewport)


def _scalar_advance(font, character: str, tab_advance: int) -> int:
    if character == "\t":
        return tab_advance
    return max(1, _text_width(font, character))


def _label_characters(text: str, direction: int = text_rules.DIRECTION_AUTO):
    """Renderer-laid-out text in visual order, one display text per
    character (APT-1-TEXT Section 10): one paragraph in DIRECTION,
    reordered, mirrored, and joined.  Plain ASCII in an LTR or AUTO
    paragraph is already in visual order, one scalar per character."""

    if text.isascii() and direction != text_rules.DIRECTION_RTL:
        return text
    return [placed.text for placed in text_rules.cached_row(text, direction, True).characters]


def _bounded_text_width(
    font,
    text: str,
    maximum: int,
    tab_advance: int,
    direction: int = text_rules.DIRECTION_AUTO,
) -> int:
    """Measure only until a renderer-owned pixel bound has been exceeded."""

    width = 0
    for character in _label_characters(text, direction):
        advance = _scalar_advance(font, character, tab_advance)
        if width > maximum - advance:
            return maximum + 1
        width += advance
    return width


def _paint_bounded_text(
    pygame_module,
    surface,
    font,
    text: str,
    color,
    visible_rect,
    *,
    left: int,
    right: int,
    top: int,
    bottom: int,
    tab_advance: int,
    direction: int = text_rules.DIRECTION_AUTO,
) -> None:
    """Paint laid-out text a character at a time, in visual order, so
    clipping never creates a huge text surface."""

    if (
        right <= left
        or bottom <= top
        or visible_rect.width <= 0
        or visible_rect.height <= 0
    ):
        return
    prior_clip = surface.get_clip()
    clip = visible_rect.clip(prior_clip)
    if clip.width <= 0 or clip.height <= 0:
        return
    cursor = left
    center_y = top + (bottom - top) // 2
    try:
        for character in _label_characters(text, direction):
            if cursor >= right:
                break
            advance = _scalar_advance(font, character, tab_advance)
            slot_right = min(cursor + advance, right)
            if (
                character != "\t"
                and cursor < clip.right
                and slot_right > clip.left
            ):
                rgba = tuple(color)
                if len(rgba) == 4 and rgba[3] == 0:
                    cursor += advance
                    continue
                glyph = font.render(character, True, rgba[:3])
                if len(rgba) == 4 and rgba[3] != 0xFF:
                    glyph = glyph.copy()
                    glyph.fill(
                        (255, 255, 255, rgba[3]),
                        special_flags=pygame_module.BLEND_RGBA_MULT,
                    )
                glyph_rect = glyph.get_rect()
                glyph_left = cursor
                glyph_top = center_y - glyph_rect.height // 2
                paint_left = max(glyph_left, cursor, clip.left)
                paint_top = max(glyph_top, clip.top)
                paint_right = min(
                    glyph_left + glyph_rect.width,
                    slot_right,
                    clip.right,
                )
                paint_bottom = min(glyph_top + glyph_rect.height, clip.bottom)
                if paint_left < paint_right and paint_top < paint_bottom:
                    source = pygame_module.Rect(
                        paint_left - glyph_left,
                        paint_top - glyph_top,
                        paint_right - paint_left,
                        paint_bottom - paint_top,
                    )
                    surface.blit(glyph, (paint_left, paint_top), source)
            cursor += advance
    finally:
        surface.set_clip(prior_clip)


def _blit_bounded_surface(pygame_module, destination, source, left: int, top: int, clip):
    """Crop one source before any destination coordinate enters pygame."""

    source_width, source_height = source.get_size()
    paint_left = max(left, clip.left)
    paint_top = max(top, clip.top)
    paint_right = min(left + source_width, clip.right)
    paint_bottom = min(top + source_height, clip.bottom)
    if paint_left >= paint_right or paint_top >= paint_bottom:
        return
    source_rect = pygame_module.Rect(
        paint_left - left,
        paint_top - top,
        paint_right - paint_left,
        paint_bottom - paint_top,
    )
    destination.blit(source, (paint_left, paint_top), source_rect)


def _clipped_python_rect(
    pygame_module,
    left: int,
    top: int,
    right: int,
    bottom: int,
    clip,
):
    """Clip unbounded Python edges before constructing an SDL-backed Rect."""

    clipped_left = max(left, clip.left)
    clipped_top = max(top, clip.top)
    clipped_right = min(right, clip.right)
    clipped_bottom = min(bottom, clip.bottom)
    if clipped_left >= clipped_right or clipped_top >= clipped_bottom:
        return None
    return pygame_module.Rect(
        clipped_left,
        clipped_top,
        clipped_right - clipped_left,
        clipped_bottom - clipped_top,
    )


def _paint_clipped_border(
    pygame_module,
    surface,
    color,
    *,
    left: int,
    top: int,
    right: int,
    bottom: int,
    width: int,
    clip,
) -> None:
    """Paint only true logical border strips that intersect the physical clip."""

    thickness = min(width, max(0, right - left), max(0, bottom - top))
    if thickness <= 0:
        return
    strips = (
        (left, top, right, min(bottom, top + thickness)),
        (left, max(top, bottom - thickness), right, bottom),
        (left, top, min(right, left + thickness), bottom),
        (max(left, right - thickness), top, right, bottom),
    )
    for strip in strips:
        rectangle = _clipped_python_rect(
            pygame_module,
            *strip,
            clip,
        )
        if rectangle is not None:
            surface.fill(color, rectangle)


def _pixel_rect(rect) -> PixelRect:
    return PixelRect(rect.left, rect.top, rect.right, rect.bottom)


def _identity(region, control_id: int) -> ControlIdentity:
    return ControlIdentity(region.owner_id, region.owner_generation, control_id)


def _matches(identity: ControlIdentity, candidate: ControlIdentity | None) -> bool:
    return candidate is not None and identity == candidate


def _control_surface(
    identity: ControlIdentity,
    state: ControlState,
    *,
    effectively_enabled: bool,
    hovered: ControlIdentity | None,
    pressed: ControlIdentity | None,
):
    if not effectively_enabled:
        return None
    if _matches(identity, pressed):
        return _ROW_PRESSED
    if _matches(identity, hovered):
        return _ROW_HOVER
    if state & (ControlState.OPEN | ControlState.SELECTED):
        return _ROW_SELECTED
    return None


def _draw_checkmark(
    pygame_module,
    surface,
    rect,
    metrics: _MenuMetrics,
    color,
) -> None:
    size = max(5, min(metrics.font_height, rect.height) * 2 // 3)
    left = rect.left + metrics.popup_padding + max(0, (metrics.check_column - size) // 2)
    top = rect.centery - size // 2
    thickness = max(1, metrics.font_height // 9)
    points = (
        (left, top + size // 2),
        (left + size // 3, top + size - 1),
        (left + size, top),
    )
    _alpha_line(
        pygame_module,
        surface,
        (*tuple(color)[:3], 0xFF),
        points[0],
        points[1],
        width=thickness,
    )
    _alpha_line(
        pygame_module,
        surface,
        (*tuple(color)[:3], 0xFF),
        points[1],
        points[2],
        width=thickness,
    )


def _popup_dimensions(font, menu: MenuDraw, metrics: _MenuMetrics) -> tuple[int, int]:
    label_width = _text_width(font, menu.label)
    shortcut_width = 0
    height = 2 * metrics.popup_padding
    for entry in menu.entries:
        if isinstance(entry, MenuSeparatorDraw):
            height += metrics.separator_height
            continue
        label_width = max(label_width, _text_width(font, entry.label))
        shortcut_width = max(shortcut_width, _text_width(font, entry.shortcut))
        height += metrics.row_height
    width = (
        2 * metrics.popup_padding
        + metrics.check_column
        + metrics.gap
        + label_width
    )
    if shortcut_width:
        width += metrics.shortcut_gap + shortcut_width
    return width, height


def _popup_rect(pygame_module, anchor, title_rect, viewport, width: int, height: int, metrics):
    below_top = anchor.bottom + metrics.gap
    space_below = max(0, viewport.bottom - below_top)
    space_above = max(0, anchor.top - metrics.gap - viewport.top)
    if height <= space_below or space_below >= space_above:
        top = below_top
    else:
        top = anchor.top - metrics.gap - height

    left = title_rect.left
    if width <= viewport.width:
        left = min(max(left, viewport.left), viewport.right - width)
    else:
        left = viewport.left
    return _WideRect(left, top, width, height)


def _paint_popup(
    pygame_module,
    surface,
    font,
    region,
    anchor,
    viewport,
    menu: MenuDraw,
    title_rect,
    metrics: _MenuMetrics,
    *,
    root_enabled: bool,
    hovered: ControlIdentity | None,
    pressed: ControlIdentity | None,
) -> list[HitMapEntry]:
    width, height = _popup_dimensions(font, menu, metrics)
    popup = _popup_rect(
        pygame_module,
        anchor,
        title_rect,
        viewport,
        width,
        height,
        metrics,
    )
    visible_popup = _bounded_pygame_rect(pygame_module, popup, viewport)
    if visible_popup.width <= 0 or visible_popup.height <= 0:
        return []

    shadow = popup.move(metrics.shadow_offset, metrics.shadow_offset)
    _rounded_rect(
        pygame_module,
        surface,
        shadow,
        _SHADOW,
        radius=metrics.corner_radius + 1,
    )
    _rounded_rect(
        pygame_module,
        surface,
        popup,
        _POPUP_SURFACE,
        radius=metrics.corner_radius,
    )
    _rounded_rect(
        pygame_module,
        surface,
        popup,
        _BORDER,
        radius=metrics.corner_radius,
        width=1,
    )

    menu_enabled = root_enabled and bool(menu.state & ControlState.ENABLED)
    entries: list[HitMapEntry] = [
        ControlSurface(
            region.owner_id,
            region.owner_generation,
            menu.control_id,
            _pixel_rect(visible_popup),
        )
    ]
    row_top = popup.top + metrics.popup_padding
    for entry in menu.entries:
        if isinstance(entry, MenuSeparatorDraw):
            separator = _WideRect(
                popup.left + metrics.popup_padding + metrics.check_column,
                row_top,
                max(
                    0,
                    popup.width
                    - 2 * metrics.popup_padding
                    - metrics.check_column,
                ),
                metrics.separator_height,
            )
            if separator.width:
                _alpha_line(
                    pygame_module,
                    surface,
                    _SEPARATOR,
                    (separator.left, separator.centery),
                    (separator.right - 1, separator.centery),
                )
            row_top += metrics.separator_height
            continue

        row = _WideRect(
            popup.left + metrics.popup_padding,
            row_top,
            popup.width - 2 * metrics.popup_padding,
            metrics.row_height,
        )
        visible_row = _bounded_pygame_rect(
            pygame_module,
            row,
            visible_popup,
        )
        identity = _identity(region, entry.control_id)
        effectively_enabled = menu_enabled and bool(
            entry.state & ControlState.ENABLED
        )
        fill = _control_surface(
            identity,
            entry.state,
            effectively_enabled=effectively_enabled,
            hovered=hovered,
            pressed=pressed,
        )
        if fill is None and effectively_enabled:
            fill = _ROW_IDLE
        if fill is not None:
            _rounded_rect(
                pygame_module,
                surface,
                row,
                fill,
                radius=max(2, metrics.corner_radius - 1),
            )
        text_color = _TEXT if effectively_enabled else _DISABLED_TEXT
        prior_clip = surface.get_clip()
        try:
            surface.set_clip(visible_row)
            if entry.state & ControlState.CHECKED:
                _draw_checkmark(
                    pygame_module,
                    surface,
                    row,
                    metrics,
                    _ACCENT if effectively_enabled else _DISABLED_TEXT,
                )
            _paint_text(
                pygame_module,
                surface,
                font,
                entry.label,
                text_color,
                left=(
                    row.left
                    + metrics.popup_padding
                    + metrics.check_column
                    + metrics.gap
                ),
                center_y=row.centery,
            )
            _paint_text(
                pygame_module,
                surface,
                font,
                entry.shortcut,
                _MUTED_TEXT if effectively_enabled else _DISABLED_TEXT,
                right=row.right - metrics.popup_padding,
                center_y=row.centery,
            )
        finally:
            surface.set_clip(prior_clip)
        if effectively_enabled and visible_row.width > 0 and visible_row.height > 0:
            entries.append(
                ControlHitTarget(
                    identity,
                    ControlKind.MENU_ITEM,
                    _pixel_rect(visible_row),
                )
            )
        row_top += metrics.row_height
    return entries


def _paint_menu_bar(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: MenuBarDraw,
    cell_width: int,
    cell_height: int,
    *,
    hovered: ControlIdentity | None,
    pressed: ControlIdentity | None,
) -> tuple[list[HitMapEntry], list[_MenuPopup]]:
    anchor = _bounds_rect(
        pygame_module,
        region_rect,
        draw.bounds,
        cell_width,
        cell_height,
    )
    viewport = _region_viewport(
        pygame_module,
        surface,
        region,
        region_rect,
    )
    visible_anchor = _bounded_pygame_rect(pygame_module, anchor, viewport)
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return [], []

    metrics = _menu_metrics(font, cell_width, cell_height)
    root_enabled = bool(draw.state & ControlState.ENABLED)
    targets: list[HitMapEntry] = [
        ControlSurface(
            region.owner_id,
            region.owner_generation,
            draw.control_id,
            _pixel_rect(visible_anchor),
        )
    ]
    popups: list[_MenuPopup] = []
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(viewport)
        shadow = anchor.move(0, metrics.shadow_offset)
        _rounded_rect(
            pygame_module,
            surface,
            shadow,
            _SHADOW,
            radius=metrics.corner_radius + 1,
        )
        _rounded_rect(
            pygame_module,
            surface,
            anchor,
            _BAR_SURFACE,
            radius=metrics.corner_radius,
        )
        _rounded_rect(
            pygame_module,
            surface,
            anchor,
            _BORDER,
            radius=metrics.corner_radius,
            width=1,
        )

        title_height = min(anchor.height, metrics.title_height)
        title_top = anchor.top + (anchor.height - title_height) // 2
        title_left = anchor.left + metrics.gap
        surface.set_clip(visible_anchor)
        for menu in draw.menus:
            title_width = _text_width(font, menu.label) + 2 * metrics.horizontal_padding
            title = _WideRect(
                title_left,
                title_top,
                title_width,
                title_height,
            )
            visible_title = _bounded_pygame_rect(
                pygame_module,
                title,
                visible_anchor,
            )
            identity = _identity(region, menu.control_id)
            effectively_enabled = root_enabled and bool(
                menu.state & ControlState.ENABLED
            )
            fill = _control_surface(
                identity,
                menu.state,
                effectively_enabled=effectively_enabled,
                hovered=hovered,
                pressed=pressed,
            )
            if fill is None and effectively_enabled:
                fill = _TITLE_IDLE
            if fill is not None:
                _rounded_rect(
                    pygame_module,
                    surface,
                    title,
                    fill,
                    radius=max(2, metrics.corner_radius - 1),
                )
            if (
                menu.state & ControlState.OPEN
                and visible_title.width > 0
                and visible_title.height > 0
            ):
                accent_y = title.bottom - 1
                _alpha_line(
                    pygame_module,
                    surface,
                    _ACCENT,
                    (title.left + metrics.horizontal_padding, accent_y),
                    (title.right - metrics.horizontal_padding - 1, accent_y),
                    width=max(1, metrics.font_height // 10),
                )
                popups.append(
                    _MenuPopup(anchor, viewport, menu, title, metrics, root_enabled)
                )
            text_color = _TEXT if effectively_enabled else _DISABLED_TEXT
            if visible_title.width > 0 and visible_title.height > 0:
                text_clip = surface.get_clip()
                try:
                    surface.set_clip(visible_title)
                    _paint_text(
                        pygame_module,
                        surface,
                        font,
                        menu.label,
                        text_color,
                        left=title.left + metrics.horizontal_padding,
                        center_y=title.centery,
                    )
                finally:
                    surface.set_clip(text_clip)
                if effectively_enabled:
                    targets.append(
                        ControlHitTarget(
                            identity,
                            ControlKind.MENU,
                            _pixel_rect(visible_title),
                        )
                    )
            title_left = title.right + metrics.gap

    finally:
        surface.set_clip(prior_clip)
    return targets, popups


def _text_root_entries(region, draw, anchor, visible_anchor) -> list[HitMapEntry]:
    """Return one painted text root's hit entry from its exact paint geometry.

    An enabled root becomes a text target carrying the layout it was painted
    with; a disabled one still blocks lower controls and raw pointer input.
    """

    rect = _pixel_rect(visible_anchor)
    if not draw.state & ControlState.ENABLED:
        return [
            ControlSurface(
                region.owner_id,
                region.owner_generation,
                draw.control_id,
                rect,
            )
        ]
    content = draw.content
    layout = {
        "anchor_left": anchor.left,
        "anchor_top": anchor.top,
        "anchor_width": anchor.width,
        "anchor_height": anchor.height,
        "content_revision": content.content_revision,
        "viewport_row": content.viewport_row,
        "viewport_column": content.viewport_column,
        "viewport_rows": content.viewport_rows,
        "viewport_columns": content.viewport_columns,
    }
    identity = _identity(region, draw.control_id)
    if isinstance(draw, TextAreaDraw):
        return [
            TextHitTarget(
                identity,
                ControlKind.TEXT_AREA,
                rect,
                rows=tuple(
                    (item.row, item.item_key, item.text)
                    for item in content.items
                ),
                direction=content.direction,
                links=tuple(
                    (item.item_key, run.start, run.end)
                    for item in content.items
                    for run in item.runs
                    if run.meaning is TextStyle.LINK
                ),
                read_only=bool(content.flags & SemanticContentFlag.READ_ONLY),
                **layout,
            )
        ]
    row_end = content.viewport_row + content.viewport_rows
    column_end = content.viewport_column + content.viewport_columns
    return [
        TextHitTarget(
            identity,
            ControlKind.TEXT_GRID,
            rect,
            cells=tuple(
                (
                    item.row,
                    item.column,
                    item.row_span,
                    item.column_span,
                    item.item_key,
                    item.role is SemanticTextRole.CONTENT
                    and not item.state & SemanticTextState.UNAVAILABLE,
                )
                for item in content.items
                if item.row + item.row_span > content.viewport_row
                and item.row < row_end
                and item.column + item.column_span > content.viewport_column
                and item.column < column_end
            ),
            **layout,
        )
    ]


def _text_area_shift(layout, column_start: int, columns: int) -> int:
    """The viewport slot of a laid-out row's visual column 0.

    SEMANTIC-CONTENT-1: an LTR row starts at the viewport's left edge, with
    its column origin counted in cells from the left; an RTL row is
    mirrored, starting at the right edge with the origin counted from the
    right.
    """

    if layout.rtl:
        return columns - layout.width + column_start
    return -column_start


def _paint_cluster(
    pygame_module,
    surface,
    font,
    text: str,
    color,
    clip,
    *,
    left: int,
    top: int,
    bottom: int,
    bold: bool = False,
    italic: bool = False,
) -> None:
    """Paint one character's display scalars as one glyph cluster from
    LEFT, centred between TOP and BOTTOM and clipped to CLIP, in the bold
    or italic face when asked."""

    styled = (
        (bold or italic)
        and callable(getattr(font, "set_bold", None))
        and callable(getattr(font, "set_italic", None))
    )
    if styled:
        prior = font.get_bold(), font.get_italic()
        font.set_bold(bold)
        font.set_italic(italic)
    try:
        glyph, has_ink, _width, height = _glyph_raster(
            pygame_module, font, text, color, 0xFF
        )
    finally:
        if styled:
            font.set_bold(prior[0])
            font.set_italic(prior[1])
    if has_ink:
        glyph_top = top + (bottom - top) // 2 - height // 2
        _blit_bounded_surface(pygame_module, surface, glyph, left, glyph_top, clip)


def _paint_text_area(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: TextAreaDraw,
) -> list[HitMapEntry]:
    """Paint one exact logical text viewport with persistent selection state.

    Each row is one paragraph in the content's direction, laid out by the
    shared text rules (APT-1-TEXT Sections 3 to 8): its characters take
    their cells in visual order, a right-to-left row is mirrored, and a
    selection covers exactly the characters between its endpoints, whose
    cells need not be contiguous.  A character takes the reference theme's
    look for the meaning of the style run over its first scalar.
    """

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module,
        surface,
        region,
        region_rect,
        draw.bounds,
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return []
    content = draw.content
    row_start = content.viewport_row
    row_end = row_start + content.viewport_rows
    column_start = content.viewport_column
    columns = content.viewport_columns
    visible_items = []
    primary_item = None
    anchor_item = None
    for item in content.items:
        if item.item_key == content.primary_key:
            primary_item = item
        if item.item_key == content.anchor_key:
            anchor_item = item
        if row_start <= item.row < row_end:
            visible_items.append(item)

    selection = None
    if anchor_item is not None and primary_item is not None:
        endpoint_a = (anchor_item.row, content.anchor_offset)
        endpoint_b = (primary_item.row, content.primary_offset)
        if endpoint_a != endpoint_b:
            selection = tuple(sorted((endpoint_a, endpoint_b)))

    def row_edges(row: int) -> tuple[int, int]:
        relative_row = row - row_start
        return (
            _partition_edge(anchor.top, anchor.height, relative_row, content.viewport_rows),
            _partition_edge(anchor.top, anchor.height, relative_row + 1, content.viewport_rows),
        )

    def slot_edge(slot: int) -> int:
        return _partition_edge(anchor.left, anchor.width, slot, columns)

    enabled = bool(draw.state & ControlState.ENABLED)
    text_color = _TEXT if enabled else _DISABLED_TEXT
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, visible_anchor)
        for item in visible_items:
            row_top, row_bottom = row_edges(item.row)
            if _clipped_python_rect(
                pygame_module, anchor.left, row_top, anchor.right, row_bottom, visible_anchor
            ) is None:
                continue
            layout = text_rules.cached_row(item.text, content.direction, True)
            shift = _text_area_shift(layout, column_start, columns)
            selected = None
            if selection is not None:
                (first_row, first_offset), (last_row, last_offset) = selection
                if first_row <= item.row <= last_row:
                    low = first_offset if item.row == first_row else 0
                    high = last_offset if item.row == last_row else layout.length
                    if low < high:
                        selected = (low, high)
            for placed in layout.characters:
                first = placed.column + shift
                last = first + placed.width
                if last <= 0 or first >= columns:
                    continue
                left = slot_edge(max(first, 0))
                right = slot_edge(min(last, columns))
                if selected is not None and selected[0] <= placed.start < selected[1]:
                    selected_rect = _clipped_python_rect(
                        pygame_module, left, row_top, right, row_bottom, visible_anchor
                    )
                    if selected_rect is not None:
                        surface.fill(_TEXT_SELECTION, selected_rect)
                # A character the viewport edge cuts is not drawn, and a tab
                # is a renderer-owned blank (APT-1-TEXT Section 6).
                if first < 0 or last > columns or placed.text == "\t":
                    continue
                glyph_clip = _clipped_python_rect(
                    pygame_module, left, row_top, right, row_bottom, visible_anchor
                )
                if glyph_clip is not None:
                    look = None
                    if item.runs:
                        meaning = item.meaning_at(placed.start)
                        if meaning is not None:
                            look = REFERENCE_TEXT_THEME.get(meaning)
                    color = text_color
                    if look is not None and look.color is not None and enabled:
                        color = look.color
                    _paint_cluster(
                        pygame_module,
                        surface,
                        font,
                        placed.text,
                        color,
                        glyph_clip,
                        left=left,
                        top=row_top,
                        bottom=row_bottom,
                        bold=look is not None and look.bold,
                        italic=look is not None and look.italic,
                    )
                    if look is not None and look.underline is not None:
                        underline = _clipped_python_rect(
                            pygame_module,
                            left,
                            row_bottom - 2,
                            right,
                            row_bottom - 1,
                            glyph_clip,
                        )
                        if underline is not None:
                            surface.fill(
                                look.underline if enabled else _DISABLED_TEXT,
                                underline,
                            )

        surface.set_clip(visible_anchor)
        _paint_clipped_border(
            pygame_module,
            surface,
            _ACCENT[:3]
            if draw.state & ControlState.SELECTED
            else _COLLECTION_BORDER,
            left=anchor.left,
            top=anchor.top,
            right=anchor.right,
            bottom=anchor.bottom,
            width=1,
            clip=visible_anchor,
        )
        if primary_item is not None and row_start <= primary_item.row < row_end:
            # APT-1-TEXT Section 9.2: the caret stands at its character's
            # leading edge, the left at an even level and the right at an odd
            # one; at the row's end, just past the content on its end side.
            layout = text_rules.cached_row(primary_item.text, content.direction, True)
            shift = _text_area_shift(layout, column_start, columns)
            placed = layout.caret_character(content.primary_offset)
            if placed is None:
                edge = (0 if layout.rtl else layout.width) + shift
            elif placed.level & 1:
                edge = placed.column + placed.width + shift
            else:
                edge = placed.column + shift
            if 0 <= edge <= columns:
                row_top, row_bottom = row_edges(primary_item.row)
                caret_x = min(slot_edge(edge), anchor.right - 1)
                caret = _clipped_python_rect(
                    pygame_module,
                    caret_x,
                    row_top + (1 if row_bottom - row_top > 2 else 0),
                    caret_x + 1,
                    row_bottom - (1 if row_bottom - row_top > 2 else 0),
                    visible_anchor,
                )
                if caret is not None:
                    surface.fill(_ACCENT[:3] if enabled else _DISABLED_TEXT, caret)
    finally:
        surface.set_clip(prior_clip)
    return _text_root_entries(region, draw, anchor, visible_anchor)


def _paint_text_grid(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: TextGridDraw,
    cell_width: int,
) -> list[HitMapEntry]:
    """Paint logical grid spans directly, without materializing a cell matrix."""

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module,
        surface,
        region,
        region_rect,
        draw.bounds,
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return []
    content = draw.content
    row_start = content.viewport_row
    row_end = row_start + content.viewport_rows
    column_start = content.viewport_column
    column_end = column_start + content.viewport_columns
    enabled = bool(draw.state & ControlState.ENABLED)
    padding = max(2, cell_width // 4)
    tab_advance = max(1, cell_width * 4)
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, visible_anchor)
        for item in content.items:
            item_bottom = item.row + item.row_span
            item_right = item.column + item.column_span
            if (
                item_bottom <= row_start
                or item.row >= row_end
                or item_right <= column_start
                or item.column >= column_end
            ):
                continue
            logical_left = _partition_edge(
                anchor.left,
                anchor.width,
                item.column - column_start,
                content.viewport_columns,
            )
            logical_top = _partition_edge(
                anchor.top,
                anchor.height,
                item.row - row_start,
                content.viewport_rows,
            )
            logical_right = _partition_edge(
                anchor.left,
                anchor.width,
                item_right - column_start,
                content.viewport_columns,
            )
            logical_bottom = _partition_edge(
                anchor.top,
                anchor.height,
                item_bottom - row_start,
                content.viewport_rows,
            )
            visible_item = _clipped_python_rect(
                pygame_module,
                logical_left,
                logical_top,
                logical_right,
                logical_bottom,
                visible_anchor,
            )
            if visible_item is None:
                continue

            if item.state & SemanticTextState.UNAVAILABLE:
                fill = _GRID_UNAVAILABLE
            elif item.item_key == content.primary_key:
                fill = _GRID_PRIMARY
            elif item.role in (
                SemanticTextRole.ROW_HEADER,
                SemanticTextRole.COLUMN_HEADER,
            ):
                fill = _GRID_HEADER
            else:
                fill = _GRID_CELL
            surface.fill(fill, visible_item)
            _paint_clipped_border(
                pygame_module,
                surface,
                _COLLECTION_BORDER,
                left=logical_left,
                top=logical_top,
                right=logical_right,
                bottom=logical_bottom,
                width=1,
                clip=visible_anchor,
            )
            if (
                item.item_key == content.primary_key
                and item.state & SemanticTextState.UNAVAILABLE
            ):
                _paint_clipped_border(
                    pygame_module,
                    surface,
                    _MUTED_TEXT,
                    left=logical_left,
                    top=logical_top,
                    right=logical_right,
                    bottom=logical_bottom,
                    width=2,
                    clip=visible_anchor,
                )
            if item.state & SemanticTextState.CURRENT:
                _paint_clipped_border(
                    pygame_module,
                    surface,
                    _ACCENT[:3],
                    left=logical_left,
                    top=logical_top,
                    right=logical_right,
                    bottom=logical_bottom,
                    width=2,
                    clip=visible_anchor,
                )

            text_color = (
                _DISABLED_TEXT
                if not enabled or item.state & SemanticTextState.UNAVAILABLE
                else _TEXT
            )
            text_left = logical_left + padding
            text_right = logical_right - padding
            # Each item is one paragraph in the content's direction; a
            # right-to-left one is set against the item's right edge.
            if (
                not item.text.isascii()
                or content.direction == text_rules.DIRECTION_RTL
            ) and text_rules.cached_row(item.text, content.direction, True).rtl:
                text_left = max(
                    text_left,
                    text_right
                    - _bounded_text_width(
                        font,
                        item.text,
                        text_right - text_left,
                        tab_advance,
                        content.direction,
                    ),
                )
            _paint_bounded_text(
                pygame_module,
                surface,
                font,
                item.text,
                text_color,
                visible_item,
                left=text_left,
                right=text_right,
                top=logical_top,
                bottom=logical_bottom,
                tab_advance=tab_advance,
                direction=content.direction,
            )

        surface.set_clip(visible_anchor)
        _paint_clipped_border(
            pygame_module,
            surface,
            _ACCENT[:3]
            if draw.state & ControlState.SELECTED
            else _COLLECTION_BORDER,
            left=anchor.left,
            top=anchor.top,
            right=anchor.right,
            bottom=anchor.bottom,
            width=1,
            clip=visible_anchor,
        )
    finally:
        surface.set_clip(prior_clip)
    return _text_root_entries(region, draw, anchor, visible_anchor)


# Reference item view layout, in cells: a tree indents two cells a level and
# gives the disclosure mark two; a check box takes two and a gap; fields in
# a row are two cells apart.
_ITEM_INDENT = 2
_ITEM_MARK = 2
_ITEM_CHECK = 3
_ITEM_GAP = 2


def _paint_item_view(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: ItemViewDraw,
) -> list[HitMapEntry]:
    """Paint one item view's viewport, one item per row (a card per item),
    in the monospace font on the root's cell slots (SEMANTIC-CONTENT-1).

    The layout is this renderer's: a tree indents by depth and marks
    expandable items, a table heads its columns and aligns NUMBER fields at
    the end, a list or tree puts later fields at the row's end, and sections
    set their headings in the heading look.  Cards take the exact rows the
    contract gives, the first starting ``viewport_row`` rows above the root:
    field 0 one cell in and later fields three, a WRAP field's lines on
    rows of their own.
    """

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module, surface, region, region_rect, draw.bounds
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return []
    content = draw.content
    slots = draw.bounds.cell_cols
    rows = draw.bounds.cell_rows
    direction = content.direction
    enabled = bool(draw.state & ControlState.ENABLED)
    role = content.role
    shown = content.shown_items()
    header = role is ItemViewRole.TABLE and any(
        column.label for column in content.columns
    )
    cards = role is ItemViewRole.CARDS

    def slot_edge(slot: int) -> int:
        return _partition_edge(anchor.left, anchor.width, min(max(slot, 0), slots), slots)

    def row_edge(row: int) -> int:
        return _partition_edge(anchor.top, anchor.height, min(max(row, 0), rows), rows)

    def width_of(text: str) -> int:
        return text_rules.cached_row(text, direction, True).width

    # A table's columns take their natural widths; the first gives way when
    # they do not fit.
    column_starts: list[int] = []
    column_widths: list[int] = []
    if role is ItemViewRole.TABLE:
        widths = []
        for index, column in enumerate(content.columns):
            natural = width_of(column.label) if header else 0
            for item in shown:
                if index < len(item.fields):
                    natural = max(natural, width_of(item.fields[index].text))
            widths.append(max(1, natural))
        gaps = _ITEM_GAP * (len(widths) - 1)
        if sum(widths) + gaps > slots:
            widths[0] = max(1, slots - gaps - sum(widths[1:]))
        start = 0
        for width in widths:
            column_starts.append(start)
            column_widths.append(width)
            start += width + _ITEM_GAP

    def paint_text(item_field, text, first, last, top, bottom, *, color, end=False, look=None):
        """Paint one field or label between slots FIRST and LAST as one
        paragraph, at LAST's side when END or right-to-left; a character
        the bounds cut is not drawn."""

        paint_layout(item_field, text_rules.cached_row(text, direction, True),
                     first, last, top, bottom, color=color, end=end, look=look)

    def paint_layout(item_field, layout, first, last, top, bottom, *, color, end=False,
                     look=None):
        """Paint one laid-out row or line between slots FIRST and LAST."""

        if last <= first:
            return
        start = last - layout.width if (end or layout.rtl) else first
        start = max(start, first)
        for placed in layout.characters:
            left_slot = start + placed.column
            right_slot = left_slot + placed.width
            if right_slot > last:
                continue
            left, right = slot_edge(left_slot), slot_edge(right_slot)
            clip = _clipped_python_rect(pygame_module, left, top, right, bottom, visible_anchor)
            if clip is None:
                continue
            char_look = look
            if item_field is not None and item_field.runs:
                meaning = item_field.meaning_at(placed.start)
                if meaning is not None:
                    char_look = REFERENCE_TEXT_THEME.get(meaning)
            char_color = color
            if char_look is not None and char_look.color is not None and enabled:
                char_color = char_look.color
            _paint_cluster(
                pygame_module, surface, font, placed.text, char_color, clip,
                left=left, top=top, bottom=bottom,
                bold=char_look is not None and char_look.bold,
                italic=char_look is not None and char_look.italic,
            )
            if char_look is not None and char_look.underline is not None:
                underline = _clipped_python_rect(
                    pygame_module, left, bottom - 2, right, bottom - 1, clip
                )
                if underline is not None:
                    surface.fill(char_look.underline if enabled else _DISABLED_TEXT, underline)

    parts: list[ItemPart] = []
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, visible_anchor)
        row = 0
        if header:
            top, bottom = row_edge(0), row_edge(1)
            for index, column in enumerate(content.columns):
                first = column_starts[index]
                paint_text(
                    None, column.label, first, first + column_widths[index], top, bottom,
                    color=_MUTED_TEXT, end=column.kind is ItemColumnKind.NUMBER,
                )
            line = _clipped_python_rect(
                pygame_module, anchor.left, bottom - 1, anchor.right, bottom, visible_anchor
            )
            if line is not None:
                surface.fill(_COLLECTION_BORDER, line)
            row = 1
        heading = REFERENCE_TEXT_THEME[TextStyle.HEADING]
        if cards:
            row = -content.viewport_row
        for item in shown:
            item_rows = card_row_count(content, item, slots) if cards else 1
            item_row = row
            top, bottom = row_edge(row), row_edge(row + item_rows)
            row += item_rows
            area = _clipped_python_rect(
                pygame_module, anchor.left, top, anchor.right, bottom, visible_anchor
            )
            if area is None:
                continue
            state = item.state
            unavailable = bool(state & ItemState.UNAVAILABLE)
            color = _DISABLED_TEXT if (not enabled or unavailable) else _TEXT
            if state & ItemState.SELECTED:
                surface.fill(_TEXT_SELECTION, area)
            if state & ItemState.CURRENT:
                bar = _clipped_python_rect(
                    pygame_module, anchor.left, top, anchor.left + 2, bottom, visible_anchor
                )
                if bar is not None:
                    surface.fill(_ACCENT[:3], bar)
            if cards:
                _paint_clipped_border(
                    pygame_module, surface, _COLLECTION_BORDER,
                    left=anchor.left, top=top, right=anchor.right, bottom=bottom,
                    width=1, clip=visible_anchor,
                )
            slot = 0
            if role in (ItemViewRole.TREE, ItemViewRole.SECTIONS):
                slot = _ITEM_INDENT * item.depth
            line_top, line_bottom = top, row_edge(item_row + 1)
            disclosure = None
            if role is ItemViewRole.TREE:
                if state & ItemState.EXPANDABLE:
                    mark = "\u25be" if state & ItemState.EXPANDED else "\u25b8"
                    paint_text(None, mark, slot, slot + _ITEM_MARK, line_top, line_bottom,
                               color=_MUTED_TEXT)
                    disclosure = _clipped_python_rect(
                        pygame_module, slot_edge(slot), line_top,
                        slot_edge(slot + _ITEM_MARK), line_bottom, visible_anchor,
                    )
                slot += _ITEM_MARK
            check_box = None
            if state & ItemState.CHECKABLE:
                box_left, box_right = slot_edge(slot), slot_edge(slot + 2)
                size = max(3, min(box_right - box_left, line_bottom - line_top) - 4)
                box_top = line_top + (line_bottom - line_top - size) // 2
                box_left += (box_right - box_left - size) // 2
                _paint_clipped_border(
                    pygame_module, surface, color,
                    left=box_left, top=box_top, right=box_left + size, bottom=box_top + size,
                    width=1, clip=visible_anchor,
                )
                if state & ItemState.CHECKED:
                    inner = _clipped_python_rect(
                        pygame_module, box_left + 2, box_top + 2,
                        box_left + size - 2, box_top + size - 2, visible_anchor,
                    )
                    if inner is not None:
                        surface.fill(_ACCENT[:3] if enabled else _DISABLED_TEXT, inner)
                if not unavailable:
                    check_box = _clipped_python_rect(
                        pygame_module, slot_edge(slot), line_top,
                        slot_edge(slot + 2), line_bottom, visible_anchor,
                    )
                slot += _ITEM_CHECK
            fields = item.fields
            if item.role is ItemRole.SECTION:
                paint_text(fields[0], fields[0].text, 0, slots, line_top, line_bottom,
                           color=color, look=heading)
            elif role is ItemViewRole.TABLE:
                for index, item_field in enumerate(fields):
                    first = max(column_starts[index], slot if index == 0 else 0)
                    paint_text(
                        item_field, item_field.text, first,
                        column_starts[index] + column_widths[index], line_top, line_bottom,
                        color=color,
                        end=content.columns[index].kind is ItemColumnKind.NUMBER,
                    )
            elif cards:
                line_row = item_row
                for index, column in enumerate(content.columns):
                    first = 1 if index == 0 else 3
                    last = first + card_field_width(slots, index)
                    if index >= len(fields):
                        line_row += 1
                        continue
                    item_field = fields[index]
                    if column.wrap:
                        lines = text_rules.cached_lines(
                            item_field.text, direction, card_field_width(slots, index)
                        )
                    else:
                        lines = (text_rules.cached_row(item_field.text, direction, True),)
                    for line in lines:
                        if 0 <= line_row < rows:
                            paint_layout(item_field, line, first, last, row_edge(line_row),
                                         row_edge(line_row + 1), color=color)
                        line_row += 1
            else:
                # Later fields sit at the row's end, the last one last.
                right = slots
                for index in range(len(fields) - 1, 0, -1):
                    item_field = fields[index]
                    width = width_of(item_field.text)
                    paint_text(item_field, item_field.text, max(slot, right - width), right,
                               line_top, line_bottom, color=_MUTED_TEXT if not unavailable else color,
                               end=True)
                    right = max(slot, right - width - _ITEM_GAP)
                paint_text(fields[0], fields[0].text, slot, right, line_top, line_bottom,
                           color=color)
            parts.append(
                ItemPart(
                    item.item_key,
                    _pixel_rect(area),
                    item.role is ItemRole.ITEM and not unavailable,
                    None if disclosure is None else _pixel_rect(disclosure),
                    bool(state & ItemState.EXPANDED),
                    None if check_box is None else _pixel_rect(check_box),
                )
            )
        surface.set_clip(visible_anchor)
        _paint_clipped_border(
            pygame_module, surface,
            _ACCENT[:3] if draw.state & ControlState.SELECTED else _COLLECTION_BORDER,
            left=anchor.left, top=anchor.top, right=anchor.right, bottom=anchor.bottom,
            width=1, clip=visible_anchor,
        )
    finally:
        surface.set_clip(prior_clip)
    rect = _pixel_rect(visible_anchor)
    if not enabled:
        return [ControlSurface(region.owner_id, region.owner_generation, draw.control_id, rect)]
    return [
        ItemHitTarget(
            _identity(region, draw.control_id), rect, content.content_revision, tuple(parts)
        )
    ]


def _tab_width(
    font,
    tab: TabDraw,
    metrics: _MenuMetrics,
    maximum: int,
    tab_advance: int,
) -> int:
    label_width = _bounded_text_width(font, tab.label, maximum, tab_advance)
    shortcut_width = _bounded_text_width(
        font,
        tab.shortcut,
        maximum,
        tab_advance,
    )
    width = 2 * metrics.horizontal_padding + label_width
    if tab.shortcut:
        width += metrics.shortcut_gap + shortcut_width
    return max(metrics.font_height + 2 * metrics.horizontal_padding, width)


def _paint_tabset(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: TabSetDraw,
    cell_width: int,
    cell_height: int,
    *,
    hovered: ControlIdentity | None,
    pressed: ControlIdentity | None,
) -> list[HitMapEntry]:
    """Lay out generic tabs: the root surface, then enabled TAB targets."""

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module,
        surface,
        region,
        region_rect,
        draw.bounds,
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return []
    metrics = _menu_metrics(font, cell_width, cell_height)
    tab_advance = max(1, _text_width(font, " ") * 4)
    widths = [
        _tab_width(font, tab, metrics, anchor.width, tab_advance)
        for tab in draw.tabs
    ]
    total_width = sum(widths)
    if widths:
        total_width += metrics.gap * (len(widths) - 1)
    natural_layout = total_width <= anchor.width
    root_enabled = bool(draw.state & ControlState.ENABLED)
    targets: list[HitMapEntry] = [
        ControlSurface(
            region.owner_id,
            region.owner_generation,
            draw.control_id,
            _pixel_rect(visible_anchor),
        )
    ]
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, visible_anchor)
        _paint_clipped_border(
            pygame_module,
            surface,
            _COLLECTION_BORDER,
            left=anchor.left,
            top=anchor.top,
            right=anchor.right,
            bottom=anchor.bottom,
            width=1,
            clip=visible_anchor,
        )
        natural_left = anchor.left
        for index, tab in enumerate(draw.tabs):
            if natural_layout:
                tab_rect = _WideRect(
                    natural_left,
                    anchor.top,
                    widths[index],
                    anchor.height,
                )
                natural_left = tab_rect.right + metrics.gap
            else:
                tab_left = _partition_edge(
                    anchor.left,
                    anchor.width,
                    index,
                    len(draw.tabs),
                )
                tab_right = _partition_edge(
                    anchor.left,
                    anchor.width,
                    index + 1,
                    len(draw.tabs),
                )
                tab_rect = _WideRect(
                    tab_left,
                    anchor.top,
                    tab_right - tab_left,
                    anchor.height,
                )
            visible_tab = _bounded_pygame_rect(
                pygame_module,
                tab_rect,
                visible_anchor,
            )
            if visible_tab.width <= 0 or visible_tab.height <= 0:
                continue

            identity = _identity(region, tab.control_id)
            effectively_enabled = root_enabled and bool(
                tab.state & ControlState.ENABLED
            )
            fill = _control_surface(
                identity,
                tab.state,
                effectively_enabled=effectively_enabled,
                hovered=hovered,
                pressed=pressed,
            )
            if fill is None:
                fill = _TITLE_IDLE if effectively_enabled else _GRID_UNAVAILABLE
            _rounded_rect(
                pygame_module,
                surface,
                tab_rect,
                fill,
                radius=max(2, metrics.corner_radius - 1),
            )
            if tab.state & ControlState.SELECTED:
                accent_y = tab_rect.bottom - 1
                _alpha_line(
                    pygame_module,
                    surface,
                    _ACCENT,
                    (tab_rect.left, accent_y),
                    (tab_rect.right - 1, accent_y),
                    width=max(1, metrics.font_height // 10),
                )

            inner_left = tab_rect.left + metrics.horizontal_padding
            inner_right = tab_rect.right - metrics.horizontal_padding
            shortcut_width = _bounded_text_width(
                font,
                tab.shortcut,
                max(0, inner_right - inner_left),
                tab_advance,
            )
            shortcut_left = max(inner_left, inner_right - shortcut_width)
            label_right = (
                max(inner_left, shortcut_left - metrics.shortcut_gap)
                if tab.shortcut
                else inner_right
            )
            text_color = _TEXT if effectively_enabled else _DISABLED_TEXT
            _paint_bounded_text(
                pygame_module,
                surface,
                font,
                tab.label,
                text_color,
                visible_tab,
                left=inner_left,
                right=label_right,
                top=tab_rect.top,
                bottom=tab_rect.bottom,
                tab_advance=tab_advance,
            )
            if tab.shortcut:
                _paint_bounded_text(
                    pygame_module,
                    surface,
                    font,
                    tab.shortcut,
                    _MUTED_TEXT if effectively_enabled else _DISABLED_TEXT,
                    visible_tab,
                    left=shortcut_left,
                    right=inner_right,
                    top=tab_rect.top,
                    bottom=tab_rect.bottom,
                    tab_advance=tab_advance,
                )
            if effectively_enabled:
                targets.append(
                    ControlHitTarget(
                        identity,
                        ControlKind.TAB,
                        _pixel_rect(visible_tab),
                    )
                )

    finally:
        surface.set_clip(prior_clip)
    return targets


def _draw_decoration(pygame_module, surface, slot, color, alpha, y):
    """Draw one clipped source-over decoration without discarding alpha."""
    _alpha_line(
        pygame_module,
        surface,
        (*color, alpha),
        (slot.left, y),
        (slot.right - 1, y),
    )


def _polyline_point(rect, point) -> tuple[int, int]:
    """Map one UNORM32 point to an inclusive pixel center inside rect."""

    width = max(0, rect.width - 1)
    height = max(0, rect.height - 1)
    return (
        rect.left + (point.x * width) // UINT32_MAX,
        rect.top + (point.y * height) // UINT32_MAX,
    )


def _alpha_circle(pygame_module, surface, color, center, radius: int) -> None:
    rgba = tuple(color)
    radius = max(1, radius)
    if len(rgba) == 4 and rgba[3] == 0:
        return
    visible = surface.get_rect().clip(surface.get_clip())
    if visible.width <= 0 or visible.height <= 0:
        return
    layer = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    paint = rgba if len(rgba) == 4 else (*rgba[:3], 0xFF)
    radius_squared = radius * radius
    for local_y in range(visible.height):
        dy = visible.top + local_y - center[1]
        remaining = radius_squared - dy * dy
        if remaining < 0:
            continue
        for local_x in range(visible.width):
            dx = visible.left + local_x - center[0]
            if dx * dx <= remaining:
                layer.set_at((local_x, local_y), paint)
    surface.blit(layer, visible)


def _paint_polyline(pygame_module, surface, region, region_rect, draw) -> None:
    object_rect = _object_rect(pygame_module, region, region_rect, draw)
    clip = _bounded_pygame_rect(
        pygame_module,
        object_rect,
        _region_viewport(pygame_module, surface, region, region_rect),
    )
    prior_clip = surface.get_clip()
    clip = clip.clip(prior_clip)
    if clip.width <= 0 or clip.height <= 0 or draw.color.alpha == 0:
        return
    minimum_extent = min(object_rect.width, object_rect.height)
    if minimum_extent <= 0:
        return
    thickness = max(
        1,
        (draw.stroke_width * minimum_extent + UINT32_MAX - 1) // UINT32_MAX,
    )
    points = tuple(_polyline_point(object_rect, point) for point in draw.points)
    segments = list(zip(points, points[1:]))
    if draw.closed:
        segments.append((points[-1], points[0]))
    color = (*_rgb(draw.color), draw.color.alpha)
    try:
        surface.set_clip(clip)
        for start, end in segments:
            _alpha_line(
                pygame_module,
                surface,
                color,
                start,
                end,
                width=thickness,
            )
        # pygame's one-pixel line already supplies its endpoint pixels.  Wider
        # strokes receive explicit round caps and joins so their appearance is
        # independent of the platform line primitive's endpoint convention.
        if thickness > 1:
            radius = thickness // 2
            for point in points:
                _alpha_circle(pygame_module, surface, color, point, radius)
    finally:
        surface.set_clip(prior_clip)


def _object_clip(pygame_module, surface, region, region_rect, draw):
    object_rect = _object_rect(pygame_module, region, region_rect, draw)
    clip = _bounded_pygame_rect(
        pygame_module,
        object_rect,
        _region_viewport(pygame_module, surface, region, region_rect),
    )
    return object_rect, clip


def _paint_readout(pygame_module, surface, font, region, region_rect, draw) -> None:
    object_rect, clip = _object_clip(
        pygame_module, surface, region, region_rect, draw
    )
    if clip.width <= 0 or clip.height <= 0:
        return
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        _rounded_rect(
            pygame_module,
            surface,
            object_rect,
            (*_rgb(draw.background), draw.background.alpha),
            radius=0,
        )
        if not draw.text or draw.foreground.alpha == 0:
            return
        padding = min(
            max(1, min(object_rect.width, object_rect.height) // 10),
            object_rect.width // 2,
        )
        left = object_rect.left + padding
        right = object_rect.right - padding
        available = max(0, right - left)
        tab_advance = max(1, _font_height(font, object_rect.height) * 2)
        text_width = _bounded_text_width(font, draw.text, available, tab_advance)
        if text_width <= available:
            left = right - text_width
        _paint_bounded_text(
            pygame_module,
            surface,
            font,
            draw.text,
            (*_rgb(draw.foreground), draw.foreground.alpha),
            clip,
            left=left,
            right=right,
            top=object_rect.top,
            bottom=object_rect.bottom,
            tab_advance=tab_advance,
        )
    finally:
        surface.set_clip(prior_clip)


def _contrast_rgba(color) -> tuple[int, int, int, int]:
    luminance = 299 * color.red + 587 * color.green + 114 * color.blue
    channel = 0 if luminance >= 128_000 else 255
    return channel, channel, channel, 255


def _paint_meter(pygame_module, surface, font, region, region_rect, draw) -> None:
    object_rect, clip = _object_clip(
        pygame_module, surface, region, region_rect, draw
    )
    if clip.width <= 0 or clip.height <= 0:
        return
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        background = (*_rgb(draw.background), draw.background.alpha)
        foreground = (*_rgb(draw.foreground), draw.foreground.alpha)
        _rounded_rect(
            pygame_module,
            surface,
            object_rect,
            background,
            radius=0,
        )
        span = draw.maximum - draw.minimum
        progress = draw.value - draw.minimum
        extent = object_rect.height if draw.vertical else object_rect.width
        filled = (progress * extent) // span
        if draw.value == draw.maximum:
            filled = extent
        if filled:
            if draw.vertical:
                fill_rect = _WideRect(
                    object_rect.left,
                    object_rect.bottom - filled,
                    object_rect.width,
                    filled,
                )
            else:
                fill_rect = _WideRect(
                    object_rect.left,
                    object_rect.top,
                    filled,
                    object_rect.height,
                )
            _rounded_rect(
                pygame_module,
                surface,
                fill_rect,
                foreground,
                radius=0,
            )
        if draw.show_value:
            text = str(draw.value)
            center_covered = (
                filled * 2 >= extent if not draw.vertical else filled * 2 > extent
            )
            base = draw.foreground if center_covered else draw.background
            text_color = _contrast_rgba(base)
            padding = min(
                max(1, min(object_rect.width, object_rect.height) // 10),
                object_rect.width // 2,
            )
            left = object_rect.left + padding
            right = object_rect.right - padding
            available = max(0, right - left)
            tab_advance = max(1, _font_height(font, object_rect.height) * 2)
            text_width = _bounded_text_width(font, text, available, tab_advance)
            if text_width <= available:
                left += (available - text_width) // 2
                right = left + text_width
            _paint_bounded_text(
                pygame_module,
                surface,
                font,
                text,
                text_color,
                clip,
                left=left,
                right=right,
                top=object_rect.top,
                bottom=object_rect.bottom,
                tab_advance=tab_advance,
            )
    finally:
        surface.set_clip(prior_clip)


def _paint_status(pygame_module, surface, region, region_rect, draw) -> None:
    object_rect, clip = _object_clip(
        pygame_module, surface, region, region_rect, draw
    )
    if clip.width <= 0 or clip.height <= 0:
        return
    color = draw.active if draw.value else draw.inactive
    if color.alpha == 0:
        return
    side = min(object_rect.width, object_rect.height)
    if side <= 0:
        return
    shape = _WideRect(
        object_rect.left + (object_rect.width - side) // 2,
        object_rect.top + (object_rect.height - side) // 2,
        side,
        side,
    )
    visible = _bounded_pygame_rect(pygame_module, shape, clip)
    if visible.width <= 0 or visible.height <= 0:
        return
    layer = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    rgba = (*_rgb(color), color.alpha)
    center_x2 = shape.left * 2 + shape.width
    center_y2 = shape.top * 2 + shape.height
    radius2 = side
    for local_y in range(visible.height):
        pixel_y2 = (visible.top + local_y) * 2 + 1
        dy = abs(pixel_y2 - center_y2)
        for local_x in range(visible.width):
            pixel_x2 = (visible.left + local_x) * 2 + 1
            dx = abs(pixel_x2 - center_x2)
            if draw.shape == 0:
                painted = dx * dx + dy * dy <= radius2 * radius2
            elif draw.shape == 1:
                painted = True
            else:
                painted = dx + dy <= radius2
            if painted:
                layer.set_at((local_x, local_y), rgba)
    surface.blit(layer, visible)


def _series_value_y(rect, value: int, minimum: int, maximum: int) -> int:
    clamped = min(max(value, minimum), maximum)
    extent = max(0, rect.height - 1)
    return rect.bottom - 1 - ((clamped - minimum) * extent) // (maximum - minimum)


def _series_points(rect, samples, minimum: int, maximum: int) -> tuple[tuple[int, int], ...]:
    if not samples:
        return ()
    if len(samples) == 1:
        x_positions = (rect.left + max(0, rect.width - 1) // 2,)
    else:
        first = samples[0].timestamp_us
        timestamp_span = samples[-1].timestamp_us - first
        extent = max(0, rect.width - 1)
        x_positions = tuple(
            rect.left + ((sample.timestamp_us - first) * extent) // timestamp_span
            for sample in samples
        )
    return tuple(
        (x, _series_value_y(rect, sample.value, minimum, maximum))
        for x, sample in zip(x_positions, samples)
    )


def _alpha_polygon(pygame_module, surface, color, points) -> None:
    rgba = tuple(color)
    if not points or (len(rgba) == 4 and rgba[3] == 0):
        return
    visible = surface.get_rect().clip(surface.get_clip())
    if visible.width <= 0 or visible.height <= 0:
        return

    polygon = list(points)

    def intersect(start, end, axis, edge):
        delta = end[axis] - start[axis]
        if delta == 0:
            return start
        other = 1 - axis
        other_value = start[other] + (
            (end[other] - start[other]) * (edge - start[axis]) // delta
        )
        return (edge, other_value) if axis == 0 else (other_value, edge)

    for axis, edge, keep_greater in (
        (0, visible.left, True),
        (0, visible.right - 1, False),
        (1, visible.top, True),
        (1, visible.bottom - 1, False),
    ):
        if not polygon:
            return
        output = []
        prior = polygon[-1]
        prior_inside = prior[axis] >= edge if keep_greater else prior[axis] <= edge
        for current in polygon:
            current_inside = (
                current[axis] >= edge if keep_greater else current[axis] <= edge
            )
            if current_inside != prior_inside:
                output.append(intersect(prior, current, axis, edge))
            if current_inside:
                output.append(current)
            prior, prior_inside = current, current_inside
        polygon = output
    if len(polygon) < 3:
        return
    if len(rgba) != 4 or rgba[3] == 0xFF:
        pygame_module.draw.polygon(surface, rgba[:3], polygon)
        return
    layer = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    pygame_module.draw.polygon(
        layer,
        rgba,
        tuple(
            (point[0] - visible.left, point[1] - visible.top)
            for point in polygon
        ),
    )
    surface.blit(layer, visible)


def _paint_plot(pygame_module, surface, region, region_rect, draw, samples) -> None:
    object_rect, clip = _object_clip(
        pygame_module, surface, region, region_rect, draw
    )
    if clip.width <= 0 or clip.height <= 0 or not samples:
        return
    points = _series_points(object_rect, samples, draw.minimum, draw.maximum)
    line = (*_rgb(draw.line), draw.line.alpha)
    fill = (*_rgb(draw.fill), draw.fill.alpha)
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        if draw.fill_to_minimum and draw.fill.alpha:
            baseline = object_rect.bottom - 1
            if len(points) == 1:
                _alpha_line(
                    pygame_module,
                    surface,
                    fill,
                    points[0],
                    (points[0][0], baseline),
                )
            else:
                _alpha_polygon(
                    pygame_module,
                    surface,
                    fill,
                    ((points[0][0], baseline), *points, (points[-1][0], baseline)),
                )
        for start, end in zip(points, points[1:]):
            _alpha_line(pygame_module, surface, line, start, end)
        if draw.draw_points or len(points) == 1:
            for point in points:
                _alpha_circle(pygame_module, surface, line, point, 1)
    finally:
        surface.set_clip(prior_clip)


def _paint_waveform(
    pygame_module,
    surface,
    region,
    region_rect,
    draw,
    samples,
) -> None:
    object_rect, clip = _object_clip(
        pygame_module, surface, region, region_rect, draw
    )
    if clip.width <= 0 or clip.height <= 0:
        return
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        if draw.draw_zero_line and draw.zero_line.alpha:
            y = _series_value_y(
                object_rect,
                draw.zero_value,
                draw.minimum,
                draw.maximum,
            )
            _alpha_line(
                pygame_module,
                surface,
                (*_rgb(draw.zero_line), draw.zero_line.alpha),
                (object_rect.left, y),
                (object_rect.right - 1, y),
            )
        points = _series_points(object_rect, samples, draw.minimum, draw.maximum)
        trace = (*_rgb(draw.trace), draw.trace.alpha)
        for start, end in zip(points, points[1:]):
            _alpha_line(pygame_module, surface, trace, start, end)
        if len(points) == 1:
            _alpha_circle(pygame_module, surface, trace, points[0], 1)
    finally:
        surface.set_clip(prior_clip)


def _glyph_raster(pygame_module, font, codepoint: str, color, alpha: int):
    """Rasterize one glyph: (surface, has_ink, width, height)."""

    glyph = font.render(codepoint, True, color)
    if alpha != 0xFF:
        glyph = glyph.copy()
        glyph.fill(
            (255, 255, 255, alpha),
            special_flags=pygame_module.BLEND_RGBA_MULT,
        )
    ink = glyph.get_bounding_rect()
    return (glyph, ink.width > 0 and ink.height > 0, *glyph.get_size())


_GLYPH_SLOTS: dict[str, tuple[tuple[tuple[str, int, int], ...], int]] = {}
_GLYPH_SLOTS_LIMIT = 4096


def _glyph_slots(text: str):
    """A GLYPH_RUN's characters as (character, first slot, slots), and the
    slot count (APT-1-TEXT Section 10).  A character takes ``W(c)`` equal
    slots, so a wide one spans two and one of width 0 draws nothing."""

    if text.isascii():
        return zip(text, range(len(text)), repeat(1)), len(text)
    cached = _GLYPH_SLOTS.get(text)
    if cached is None:
        slots = []
        total = 0
        for character in text_rules.characters(text):
            width = text_rules.char_width(character)
            slots.append((character, total, width))
            total += width
        cached = (tuple(slots), total)
        if len(_GLYPH_SLOTS) >= _GLYPH_SLOTS_LIMIT:
            _GLYPH_SLOTS.clear()
        _GLYPH_SLOTS[text] = cached
    return cached


def _batched_glyph_blits(
    pygame_module, font, text: str, rasters: dict, color, alpha: int,
    object_rect, clip, bold: bool,
):
    """Blit entries for one undecorated run, each cropped to its own slot.

    Every entry is the source crop and position that _blit_bounded_surface
    uses for that glyph under its slot's clip, so blitting them together
    paints the same pixels as the per-slot path.  Slots partition the run,
    so their order cannot matter.
    """
    slots, count = _glyph_slots(text)
    run_left = object_rect.left
    run_width = object_rect.width
    top = object_rect.top
    clip_left = clip.left
    clip_right = clip.right
    crop_top = max(top, clip.top)
    crop_bottom = min(top + object_rect.height, clip.bottom)
    blits = []
    if crop_top >= crop_bottom:
        return blits
    offsets = (0, 1) if bold else (0,)
    # A raster without ink paints nothing here.  Once this run's space proves
    # inkless, later spaces are skipped before any slot arithmetic.  Glyphs
    # are still rasterized in text order, exactly as the per-slot path does.
    inkless_space = False
    for codepoint, index, width in slots:
        if inkless_space and codepoint == " " or not width:
            continue
        left = run_left + (index * run_width) // count
        right = run_left + ((index + width) * run_width) // count
        if left >= right or right <= clip_left or left >= clip_right:
            continue
        raster = rasters.get(codepoint)
        if raster is None:
            raster = _glyph_raster(pygame_module, font, codepoint, color, alpha)
            rasters[codepoint] = raster
        glyph, has_ink, width, height = raster
        if not has_ink:
            if codepoint == " ":
                inkless_space = True
            continue
        paint_bottom = min(top + height, crop_bottom)
        if crop_top >= paint_bottom:
            continue
        crop_left = max(left, clip_left)
        crop_right = min(right, clip_right)
        for offset in offsets:
            x = left + offset
            paint_left = max(x, crop_left)
            paint_right = min(x + width, crop_right)
            if paint_left < paint_right:
                blits.append((
                    glyph,
                    (paint_left, crop_top),
                    (paint_left - x, crop_top - top,
                     paint_right - paint_left, paint_bottom - crop_top),
                ))
    return blits


def _paint_glyph_run(pygame_module, surface, font, region, region_rect, draw, glyphs):
    object_rect = _object_rect(pygame_module, region, region_rect, draw)
    clip = _bounded_pygame_rect(
        pygame_module,
        object_rect,
        _region_viewport(pygame_module, surface, region, region_rect),
    )
    prior_clip = surface.get_clip()
    clip = clip.clip(prior_clip)
    if clip.width <= 0 or clip.height <= 0:
        return
    foreground, background = draw.foreground, draw.background
    if draw.attributes & ATTR_REVERSE:
        foreground, background = background, foreground
    try:
        surface.set_clip(clip)
        if background.alpha == 0xFF:
            surface.fill(_rgb(background), clip)
        elif background.alpha:
            _rounded_rect(
                pygame_module,
                surface,
                object_rect,
                (*_rgb(background), background.alpha),
                radius=0,
            )
        slots, count = _glyph_slots(draw.text)
        if not count or foreground.alpha == 0:
            return
        color = _rgb(foreground)
        if draw.attributes & ATTR_DIM:
            color = tuple(channel // 2 for channel in color)
        italic = bool(draw.attributes & ATTR_ITALIC)
        prior_italic = None
        set_italic = None
        if italic:
            get_italic = getattr(font, "get_italic", None)
            set_italic = getattr(font, "set_italic", None)
            if not callable(get_italic) or not callable(set_italic):
                raise TypeError("font must support italic GLYPH_RUN rendering")
            prior_italic = bool(get_italic())
            set_italic(True)
        # The cache belongs to this composition and its one glyph font.
        # Italic is the only font state this painter changes, and each run
        # restores it. Opacity belongs in the key because cached surfaces must
        # remain immutable after rasterization.
        rasters = glyphs.setdefault((color, foreground.alpha, italic), {})
        decorated = draw.attributes & (ATTR_UNDERLINE | ATTR_STRIKE)
        try:
            batched = None
            if not decorated:
                batched = _batched_glyph_blits(
                    pygame_module,
                    font,
                    draw.text,
                    rasters,
                    color,
                    foreground.alpha,
                    object_rect,
                    clip,
                    bool(draw.attributes & ATTR_BOLD),
                )
            if batched is not None:
                if batched:
                    surface.blits(batched, doreturn=False)
                return
            for codepoint, index, width in slots:
                if not width:
                    continue
                left = object_rect.left + (index * object_rect.width) // count
                right = object_rect.left + ((index + width) * object_rect.width) // count
                if left >= right or right <= clip.left or left >= clip.right:
                    continue
                raster = rasters.get(codepoint)
                if raster is None:
                    raster = _glyph_raster(
                        pygame_module, font, codepoint, color, foreground.alpha
                    )
                    rasters[codepoint] = raster
                glyph, has_ink, _width, _height = raster
                # Empty ink still occupies its slot and can have decorations.
                # Inspect the actual raster: a custom font may paint spaces.
                if not has_ink and not decorated:
                    continue
                slot = _WideRect(
                    left,
                    object_rect.top,
                    right - left,
                    object_rect.height,
                )
                slot_clip = _bounded_pygame_rect(
                    pygame_module,
                    slot,
                    clip,
                )
                if slot_clip.width <= 0 or slot_clip.height <= 0:
                    continue
                surface.set_clip(slot_clip)
                # A glyph run uses the same cell origin as the mandatory CELL
                # renderer.  Every glyph and decoration is clipped to its own
                # equal slot, so font overhang cannot alter an adjacent cell.
                if has_ink:
                    _blit_bounded_surface(
                        pygame_module,
                        surface,
                        glyph,
                        slot.left,
                        slot.top,
                        slot_clip,
                    )
                    if draw.attributes & ATTR_BOLD:
                        _blit_bounded_surface(
                            pygame_module,
                            surface,
                            glyph,
                            slot.left + 1,
                            slot.top,
                            slot_clip,
                        )
                if draw.attributes & ATTR_UNDERLINE:
                    _draw_decoration(
                        pygame_module,
                        surface,
                        slot,
                        color,
                        foreground.alpha,
                        slot.bottom - 1,
                    )
                if draw.attributes & ATTR_STRIKE:
                    _draw_decoration(
                        pygame_module,
                        surface,
                        slot,
                        color,
                        foreground.alpha,
                        slot.top + slot.height // 2,
                    )
        finally:
            if set_italic is not None:
                set_italic(prior_italic)
    finally:
        surface.set_clip(prior_clip)


def _optional_identity(name: str, value) -> ControlIdentity | None:
    if value is not None and not isinstance(value, ControlIdentity):
        raise TypeError(f"{name} must be ControlIdentity or None")
    return value


def _preflight_image_surfaces(
    pygame_module,
    plane: RetainedDrawPlane,
    resource_surfaces: Mapping[ImageSurfaceKey, object] | None,
) -> dict[tuple[int, int, int], object]:
    """Resolve every visible IMAGE before the destination can be mutated.

    The public mapping is offer-scoped and keyed by the full immutable
    :attr:`ImageResourceManifest.key`.  The returned authority-keyed dictionary
    is a private convenience for region-local draw lookup; callers cannot use
    that shorter key to bypass manifest metadata or digest authorization.
    """

    if resource_surfaces is None:
        surfaces: Mapping[ImageSurfaceKey, object] = {}
    elif isinstance(resource_surfaces, Mapping):
        surfaces = resource_surfaces
    else:
        raise TypeError("resource_surfaces must be a mapping or None")

    manifests = {manifest.resource_key: manifest for manifest in plane.resources}
    resolved: dict[tuple[int, int, int], object] = {}
    for resource_key, manifest in manifests.items():
        if not isinstance(manifest, ImageResourceManifest):
            raise TypeError("draw plane contains an invalid IMAGE resource manifest")
        try:
            source = surfaces[manifest.key]
        except KeyError as exc:
            raise ValueError(
                "visible IMAGE has no exact resource surface"
            ) from exc
        get_size = getattr(source, "get_size", None)
        if not callable(get_size):
            raise TypeError("IMAGE resource surface has no get_size operation")
        try:
            size = tuple(get_size())
        except (TypeError, ValueError) as exc:
            raise TypeError("IMAGE resource surface size is invalid") from exc
        if len(size) != 2:
            raise TypeError("IMAGE resource surface size must have two axes")
        width = _integer("IMAGE surface width", size[0], minimum=1)
        height = _integer("IMAGE surface height", size[1], minimum=1)
        if (width, height) != (manifest.width, manifest.height):
            raise ValueError(
                "IMAGE resource surface dimensions do not match its manifest"
            )
        if not callable(getattr(source, "get_at", None)):
            raise TypeError(
                "IMAGE resource surface lacks immutable bounded-sampling operations"
            )
        resolved[resource_key] = source

    for region in plane.regions:
        for draw in region.draws:
            if not isinstance(draw, ImageDraw):
                continue
            resource_key = (
                region.owner_id,
                region.owner_generation,
                draw.resource_id,
            )
            if resource_key not in resolved:
                raise ValueError("visible IMAGE has no exact resource manifest")

    return resolved


def _image_fit_geometry(fit: ImageFit, source_size, object_rect):
    """Return a bounded source crop, target size, and centered destination."""

    destination_width = object_rect.width
    destination_height = object_rect.height
    if destination_width <= 0 or destination_height <= 0:
        return None
    source_width, source_height = source_size
    source_crop = None

    if fit is ImageFit.STRETCH:
        target_width = destination_width
        target_height = destination_height
    elif fit is ImageFit.CONTAIN:
        if destination_width * source_height <= destination_height * source_width:
            target_width = destination_width
            target_height = max(
                1,
                (source_height * destination_width) // source_width,
            )
        else:
            target_height = destination_height
            target_width = max(
                1,
                (source_width * destination_height) // source_height,
            )
    elif fit is ImageFit.COVER:
        # Crop the immutable source before scaling.  Scaling the entire source
        # to its cover extent can make the off-object axis arbitrarily large
        # for legal extreme aspect ratios.  The cropped input and scaled output
        # are both bounded by the source resource and destination object.
        if source_width * destination_height > source_height * destination_width:
            crop_width = max(
                1,
                min(
                    source_width,
                    (
                        source_height * destination_width
                        + destination_height
                        - 1
                    )
                    // destination_height,
                ),
            )
            crop_height = source_height
        else:
            crop_width = source_width
            crop_height = max(
                1,
                min(
                    source_height,
                    (
                        source_width * destination_height
                        + destination_width
                        - 1
                    )
                    // destination_width,
                ),
            )
        crop_x = (source_width - crop_width) // 2
        crop_y = (source_height - crop_height) // 2
        if (crop_width, crop_height) != (source_width, source_height):
            source_crop = (crop_x, crop_y, crop_width, crop_height)
        target_width = destination_width
        target_height = destination_height
    else:  # ImageDraw already validates this; retain a local fail-closed guard.
        raise TypeError("IMAGE draw has an unsupported fit")

    if target_width <= destination_width:
        target_x = object_rect.left + (destination_width - target_width) // 2
    else:
        target_x = object_rect.left - (target_width - destination_width) // 2
    if target_height <= destination_height:
        target_y = object_rect.top + (destination_height - target_height) // 2
    else:
        target_y = object_rect.top - (target_height - destination_height) // 2
    return source_crop, (target_width, target_height), (target_x, target_y)


def _paint_image(
    pygame_module,
    surface,
    region,
    region_rect,
    draw: ImageDraw,
    source,
) -> None:
    """Scale and composite one immutable cached resource without mutating it."""

    object_rect, clip = _object_clip(
        pygame_module,
        surface,
        region,
        region_rect,
        draw,
    )
    if clip.width <= 0 or clip.height <= 0 or draw.opacity == 0:
        return
    geometry = _image_fit_geometry(draw.fit, source.get_size(), object_rect)
    if geometry is None:
        return
    source_crop, target_size, target_position = geometry
    target = _WideRect(*target_position, *target_size)
    visible = _bounded_pygame_rect(pygame_module, target, clip)
    if visible.width <= 0 or visible.height <= 0:
        return
    source_width, source_height = source.get_size()
    if source_crop is None:
        source_x = source_y = 0
        sampled_width, sampled_height = source_width, source_height
    else:
        source_x, source_y, sampled_width, sampled_height = source_crop

    # Sample only physically visible pixels.  A legal offscreen CELL_RECT32
    # may have a multi-billion-cell extent; scaling an intermediate to that
    # logical size would turn clipping into an allocation attack.
    image = pygame_module.Surface(visible.size, flags=pygame_module.SRCALPHA)
    for target_y in range(visible.height):
        logical_y = visible.top + target_y - target.top
        sample_y = source_y + min(
            sampled_height - 1,
            (logical_y * sampled_height) // target.height,
        )
        for target_x in range(visible.width):
            logical_x = visible.left + target_x - target.left
            sample_x = source_x + min(
                sampled_width - 1,
                (logical_x * sampled_width) // target.width,
            )
            pixel = tuple(source.get_at((sample_x, sample_y)))
            if len(pixel) == 3:
                pixel = (*pixel, 0xFF)
            if draw.opacity != 0xFF:
                pixel = (*pixel[:3], (pixel[3] * draw.opacity) // 0xFF)
            image.set_at((target_x, target_y), pixel)
    surface.blit(image, visible)


class _FullSurfaceBounds:
    """The full-surface rect and clip a fresh composition starts from."""

    def __init__(self, pygame_module, width: int, height: int) -> None:
        self._rect = pygame_module.Rect(0, 0, width, height)

    def get_rect(self):
        return self._rect.copy()

    def get_clip(self):
        return self._rect.copy()


def opaque_cell_coverage(
    pygame_module,
    plane: RetainedDrawPlane,
    cols: int,
    rows: int,
    cell_width: int,
    cell_height: int,
) -> bytearray:
    """Mark each CELL cell whose pixels a GLYPH_RUN fill will overwrite.

    Returns one byte per cell, row-major, set when the complete cell box lies
    inside the opaque background fill of a GLYPH_RUN in PLANE.  That painter
    fills its whole clipped rectangle before drawing any glyph and restores
    the surface clip afterwards, so every earlier pixel inside the fill,
    including all CELL pixels, is replaced.  The rectangle is computed with
    the painter's own object, viewport and clip rules on the full-surface
    clip that composition starts with.  Translucent, partial and other draws
    never count, so CELL still paints everything they may reveal.
    """
    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    cols = _integer("cols", cols, minimum=0)
    rows = _integer("rows", rows, minimum=0)
    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
    covered = bytearray(cols * rows)
    bounds = None
    for region in plane.regions:
        region_rect = _WideRect(
            region.logical_x * cell_w,
            region.logical_y * cell_h,
            region.logical_cols * cell_w,
            region.logical_rows * cell_h,
        )
        viewport = None
        for draw in region.draws:
            if not isinstance(draw, GlyphRunDraw):
                continue
            background = (
                draw.foreground if draw.attributes & ATTR_REVERSE else draw.background
            )
            if background.alpha != 0xFF:
                continue
            if viewport is None:
                if bounds is None:
                    bounds = _FullSurfaceBounds(
                        pygame_module, cols * cell_w, rows * cell_h
                    )
                viewport = _region_viewport(pygame_module, bounds, region, region_rect)
            fill = _bounded_pygame_rect(
                pygame_module,
                _object_rect(pygame_module, region, region_rect, draw),
                viewport,
            )
            first_col = -(-fill.left // cell_w)
            end_col = min(cols, fill.right // cell_w)
            first_row = -(-fill.top // cell_h)
            end_row = min(rows, fill.bottom // cell_h)
            if first_col >= end_col or first_row >= end_row:
                continue
            span = b"\x01" * (end_col - first_col)
            for row in range(first_row, end_row):
                start = row * cols + first_col
                covered[start : start + len(span)] = span
    return covered


def _paint_draw(
    pygame_module,
    surface,
    font,
    control_font,
    region,
    region_rect,
    draw,
    cell_w: int,
    cell_h: int,
    glyphs: dict,
    image_surfaces,
    series_by_key,
    hovered,
    pressed,
    hit_entries: list,
    region_popups: list,
) -> None:
    """Paint one draw, adding its hit entries and the popups it opens."""

    if isinstance(draw, GlyphRunDraw):
        _paint_glyph_run(
            pygame_module,
            surface,
            font,
            region,
            region_rect,
            draw,
            glyphs,
        )
    elif isinstance(draw, PolylineDraw):
        _paint_polyline(
            pygame_module,
            surface,
            region,
            region_rect,
            draw,
        )
    elif isinstance(draw, ImageDraw):
        _paint_image(
            pygame_module,
            surface,
            region,
            region_rect,
            draw,
            image_surfaces[
                (
                    region.owner_id,
                    region.owner_generation,
                    draw.resource_id,
                )
            ],
        )
    elif isinstance(draw, ReadoutDraw):
        _paint_readout(
            pygame_module,
            surface,
            font,
            region,
            region_rect,
            draw,
        )
    elif isinstance(draw, MeterDraw):
        _paint_meter(
            pygame_module,
            surface,
            font,
            region,
            region_rect,
            draw,
        )
    elif isinstance(draw, StatusDraw):
        _paint_status(
            pygame_module,
            surface,
            region,
            region_rect,
            draw,
        )
    elif isinstance(draw, PlotDraw):
        _paint_plot(
            pygame_module,
            surface,
            region,
            region_rect,
            draw,
            series_by_key[
                (region.owner_id, region.owner_generation, draw.series_id)
            ],
        )
    elif isinstance(draw, WaveformDraw):
        _paint_waveform(
            pygame_module,
            surface,
            region,
            region_rect,
            draw,
            series_by_key[
                (region.owner_id, region.owner_generation, draw.series_id)
            ],
        )
    elif isinstance(draw, MenuBarDraw):
        targets, popups = _paint_menu_bar(
            pygame_module,
            surface,
            control_font,
            region,
            region_rect,
            draw,
            cell_w,
            cell_h,
            hovered=hovered,
            pressed=pressed,
        )
        hit_entries.extend(targets)
        region_popups.extend(popups)
    elif isinstance(draw, TextAreaDraw):
        hit_entries.extend(
            _paint_text_area(
                pygame_module,
                surface,
                font,
                region,
                region_rect,
                draw,
            )
        )
    elif isinstance(draw, TextGridDraw):
        hit_entries.extend(
            _paint_text_grid(
                pygame_module,
                surface,
                control_font,
                region,
                region_rect,
                draw,
                cell_w,
            )
        )
    elif isinstance(draw, ItemViewDraw):
        hit_entries.extend(
            _paint_item_view(
                pygame_module,
                surface,
                font,
                region,
                region_rect,
                draw,
            )
        )
    elif isinstance(draw, TabSetDraw):
        hit_entries.extend(
            _paint_tabset(
                pygame_module,
                surface,
                control_font,
                region,
                region_rect,
                draw,
                cell_w,
                cell_h,
                hovered=hovered,
                pressed=pressed,
            )
        )
    else:  # Legitimate newer kinds remain fail-closed until implemented.
        raise TypeError("unsupported retained draw value")


def _region_rect(region, cell_width: int, cell_height: int) -> _WideRect:
    return _WideRect(
        region.logical_x * cell_width,
        region.logical_y * cell_height,
        region.logical_cols * cell_width,
        region.logical_rows * cell_height,
    )


def _region_occlusion(pygame_module, region, region_rect, viewport) -> tuple:
    coverage = _bounded_pygame_rect(pygame_module, region_rect, viewport)
    if coverage.width <= 0 or coverage.height <= 0:
        return ()
    return (
        RegionOcclusion(
            region.owner_id,
            region.owner_generation,
            region.region_id,
            PixelRect(coverage.left, coverage.top, coverage.right, coverage.bottom),
        ),
    )


def _meets(extent: PixelRect | None, area: PixelRect) -> bool:
    return (
        extent is not None
        and extent.left < area.right
        and area.left < extent.right
        and extent.top < area.bottom
        and area.top < extent.bottom
    )


def retained_plane_layout(
    pygame_module,
    plane: RetainedDrawPlane,
    width: int,
    height: int,
    cell_width: int,
    cell_height: int,
    *,
    control_font,
) -> tuple[PaintedRegion, ...]:
    """Lay PLANE out on a WIDTH by HEIGHT frame without painting it.

    Each region gets its full-frame viewport and occlusion entry, and each
    draw its identity and extent; no draw has hit entries yet.
    """

    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
    frame = _FullSurfaceBounds(pygame_module, width, height)
    regions = []
    for region in plane.regions:
        region_rect = _region_rect(region, cell_w, cell_h)
        viewport = _region_viewport(pygame_module, frame, region, region_rect)
        regions.append(
            PaintedRegion(
                (region.owner_id, region.owner_generation, region.region_id),
                None
                if viewport.width <= 0 or viewport.height <= 0
                else _pixel_rect(viewport),
                _region_occlusion(pygame_module, region, region_rect, viewport),
                tuple(
                    PaintedDraw(
                        retained_draw_key(draw),
                        _draw_extent(
                            pygame_module,
                            region,
                            region_rect,
                            viewport,
                            draw,
                            cell_w,
                            cell_h,
                            control_font,
                        ),
                    )
                    for draw in region.draws
                ),
            )
        )
    return tuple(regions)


def _paint_region(
    pygame_module,
    surface,
    font,
    control_font,
    region,
    planned: PaintedRegion,
    cell_w: int,
    cell_h: int,
    glyphs: dict,
    image_surfaces,
    series_by_key,
    hovered,
    pressed,
    area: PixelRect | None = None,
) -> list[list]:
    """Paint REGION's draws in order, then the popups they opened.

    With AREA, only the draws whose extent meets it are painted.  Returns
    ``[draw index, entries, popup entries]`` for each painted draw.
    """

    region_rect = _region_rect(region, cell_w, cell_h)
    popups: list[_MenuPopup] = []
    owners: list[int] = []
    painted: list[list] = []
    for index, (draw, planned_draw) in enumerate(zip(region.draws, planned.draws)):
        if area is not None and not _meets(planned_draw.extent, area):
            continue
        entries: list[HitMapEntry] = []
        popups_start = len(popups)
        _paint_draw(
            pygame_module,
            surface,
            font,
            control_font,
            region,
            region_rect,
            draw,
            cell_w,
            cell_h,
            glyphs,
            image_surfaces,
            series_by_key,
            hovered,
            pressed,
            entries,
            popups,
        )
        owners.extend([len(painted)] * (len(popups) - popups_start))
        painted.append([index, tuple(entries), []])
    # A popup is a renderer-owned foreground surface above this region's
    # ordinary controls and objects.  Keep both its pixels and item targets
    # here so a later collection cannot cover the popup while its old hits
    # remain active.  Higher regions still paint and occlude afterward.
    for popup, owner in zip(popups, owners):
        prior_clip = surface.get_clip()
        try:
            surface.set_clip(popup.viewport)
            painted[owner][2].extend(
                _paint_popup(
                    pygame_module,
                    surface,
                    control_font,
                    region,
                    popup.anchor,
                    popup.viewport,
                    popup.menu,
                    popup.title,
                    popup.metrics,
                    root_enabled=popup.root_enabled,
                    hovered=hovered,
                    pressed=pressed,
                )
            )
        finally:
            surface.set_clip(prior_clip)
    return painted


def composite_draw_plane_result(
    pygame_module,
    surface,
    plane,
    font,
    cell_width: int,
    cell_height: int,
    *,
    resource_surfaces: Mapping[ImageSurfaceKey, object] | None = None,
    control_font=None,
    hovered: ControlIdentity | None = None,
    pressed: ControlIdentity | None = None,
) -> CompositeDrawResult:
    """Paint one plane and return its deterministic semantic hit map.

    The caller retains the pygame surface.  All hit geometry is copied into
    immutable, renderer-owned integer values and therefore remains stable when
    pygame reuses or mutates Rect instances after this pass.
    """
    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
    control_font = font if control_font is None else control_font
    hovered = _optional_identity("hovered", hovered)
    pressed = _optional_identity("pressed", pressed)
    image_surfaces = _preflight_image_surfaces(
        pygame_module,
        plane,
        resource_surfaces,
    )
    series_by_key = {history.key: history.samples for history in plane.series}
    layout = retained_plane_layout(
        pygame_module,
        plane,
        *surface.get_size(),
        cell_w,
        cell_h,
        control_font=control_font,
    )
    hit_entries: list[HitMapEntry] = []
    glyphs = {}
    painted_regions: list[PaintedRegion] = []
    for region, planned in zip(plane.regions, layout):
        region_rect = _region_rect(region, cell_w, cell_h)
        occlusion = _region_occlusion(
            pygame_module,
            region,
            region_rect,
            _region_viewport(pygame_module, surface, region, region_rect),
        )
        painted = _paint_region(
            pygame_module,
            surface,
            font,
            control_font,
            region,
            planned,
            cell_w,
            cell_h,
            glyphs,
            image_surfaces,
            series_by_key,
            hovered,
            pressed,
        )
        record = PaintedRegion(
            planned.key,
            planned.viewport,
            occlusion,
            tuple(
                replace(planned.draws[index], entries=entries,
                        popup_entries=tuple(popup_entries))
                for index, entries, popup_entries in painted
            ),
        )
        painted_regions.append(record)
        hit_entries.extend(record.entries())
    return CompositeDrawResult(surface, tuple(hit_entries), tuple(painted_regions))


def repaint_draw_plane_area(
    pygame_module,
    surface,
    plane,
    font,
    cell_width: int,
    cell_height: int,
    area: PixelRect,
    *,
    layout: tuple[PaintedRegion, ...],
    resource_surfaces: Mapping[ImageSurfaceKey, object] | None = None,
    control_font=None,
    hovered: ControlIdentity | None = None,
    pressed: ControlIdentity | None = None,
) -> dict[tuple, tuple[tuple[HitMapEntry, ...], tuple[HitMapEntry, ...]]]:
    """Repaint, under the clip AREA, every draw of PLANE whose extent meets it.

    LAYOUT is PLANE's ``retained_plane_layout`` for SURFACE.  The caller has
    already painted everything below the plane inside AREA.  Draws are
    painted in painter order, each region's popups after its draws, so inside
    AREA this gives the pixels a full composition gives.  Returns each
    painted draw's entries and popup entries by region and draw identity;
    only a draw whose whole extent lies in AREA painted its exact entries.
    """

    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    if not isinstance(area, PixelRect):
        raise TypeError("area must be PixelRect")
    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
    control_font = font if control_font is None else control_font
    hovered = _optional_identity("hovered", hovered)
    pressed = _optional_identity("pressed", pressed)
    image_surfaces = _preflight_image_surfaces(pygame_module, plane, resource_surfaces)
    series_by_key = {history.key: history.samples for history in plane.series}
    glyphs = {}
    painted_draws = {}
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(
            pygame_module.Rect(area.left, area.top, area.width, area.height)
        )
        for region, planned in zip(plane.regions, layout):
            if not _meets(planned.viewport, area):
                continue
            for index, entries, popup_entries in _paint_region(
                pygame_module,
                surface,
                font,
                control_font,
                region,
                planned,
                cell_w,
                cell_h,
                glyphs,
                image_surfaces,
                series_by_key,
                hovered,
                pressed,
                area,
            ):
                painted_draws[(planned.key, planned.draws[index].key)] = (
                    entries,
                    tuple(popup_entries),
                )
    finally:
        surface.set_clip(prior_clip)
    return painted_draws


def composite_draw_plane(
    pygame_module,
    surface,
    plane,
    font,
    cell_width: int,
    cell_height: int,
    *,
    resource_surfaces: Mapping[ImageSurfaceKey, object] | None = None,
    control_font=None,
    hovered: ControlIdentity | None = None,
    pressed: ControlIdentity | None = None,
):
    """Composite the draw plane between caller-owned CELL and cursor layers."""
    return composite_draw_plane_result(
        pygame_module,
        surface,
        plane,
        font,
        cell_width,
        cell_height,
        resource_surfaces=resource_surfaces,
        control_font=control_font,
        hovered=hovered,
        pressed=pressed,
    ).surface


__all__ = [
    "CompositeDrawResult",
    "PaintedDraw",
    "PaintedRegion",
    "ControlHitTarget",
    "ControlIdentity",
    "ControlSurface",
    "HitMapEntry",
    "ImageSurfaceKey",
    "PixelRect",
    "PointerTarget",
    "RegionOcclusion",
    "HIT_MAP_ENTRY_TYPES",
    "ItemHitTarget",
    "ItemPart",
    "ResidualPoint",
    "TextHitTarget",
    "TextPosition",
    "composite_draw_plane",
    "composite_draw_plane_result",
    "repaint_draw_plane_area",
    "retained_plane_layout",
    "hit_test_hit_map",
    "resolve_pointer",
    "opaque_cell_coverage",
    "unorm_high_edge",
    "unorm_low_edge",
]
