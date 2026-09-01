"""Pygame compositor for the retained generic draw plane.

The caller owns CELL rendering and paints the cursor after this compositor.
"""

from __future__ import annotations

import operator
from collections.abc import Mapping
from dataclasses import dataclass

from .apt1 import UINT32_MAX, UINT64_MAX
from .retained_model import ResourceFormat
from .retained_scene import ControlKind, ControlState, ImageFit
from .retained_view import (
    GlyphRunDraw,
    ImageDraw,
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
    StatusDraw,
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
    WaveformDraw,
)
from .semantic_content import SemanticTextRole, SemanticTextState

ATTR_BOLD = 0x01
ATTR_DIM = 0x02
ATTR_ITALIC = 0x04
ATTR_UNDERLINE = 0x08
ATTR_REVERSE = 0x20
ATTR_STRIKE = 0x40


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
class CompositeDrawResult:
    """One completed paint pass and its immutable semantic hit map.

    ``hit_targets`` is stored in back-to-front painter order.  ``hit_test``
    searches it in reverse, so overlapping later-painted controls win without
    leaking pygame.Rect or mutable renderer state into the retained model.
    """

    surface: object
    hit_targets: tuple[ControlHitTarget, ...]

    def __post_init__(self) -> None:
        targets = tuple(self.hit_targets)
        if any(not isinstance(target, ControlHitTarget) for target in targets):
            raise TypeError("hit_targets must contain only ControlHitTarget values")
        object.__setattr__(self, "hit_targets", targets)

    def hit_test(self, x: int, y: int) -> ControlHitTarget | None:
        for target in reversed(self.hit_targets):
            if target.rect.contains(x, y):
                return target
        return None


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


def _bounds_rect(pygame_module, region_rect, bounds):
    left = unorm_low_edge(bounds.left, region_rect.width)
    top = unorm_low_edge(bounds.top, region_rect.height)
    right = unorm_high_edge(bounds.right, region_rect.width)
    bottom = unorm_high_edge(bounds.bottom, region_rect.height)
    return pygame_module.Rect(
        region_rect.left + left,
        region_rect.top + top,
        right - left,
        bottom - top,
    )


def _object_rect(pygame_module, region_rect, draw):
    parent = region_rect
    for bounds in draw.parent_bounds:
        parent = _bounds_rect(pygame_module, parent, bounds)
    return _bounds_rect(pygame_module, parent, draw.bounds)


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
    glyph = font.render(text, True, _TEXT[:3])
    width = getattr(glyph, "get_width", None)
    if not callable(width):
        raise TypeError("control font must measure or render text surfaces")
    return _integer("rendered text width", width(), minimum=0)


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
    if len(rgba) == 4 and rgba[3] != 0xFF:
        if rgba[3] == 0:
            return
        layer = pygame_module.Surface(rect.size, flags=pygame_module.SRCALPHA)
        pygame_module.draw.rect(
            layer,
            rgba,
            layer.get_rect(),
            width=width,
            border_radius=radius,
        )
        surface.blit(layer, rect)
        return
    pygame_module.draw.rect(
        surface,
        rgba[:3],
        rect,
        width=width,
        border_radius=radius,
    )


def _alpha_line(pygame_module, surface, color, start, end, *, width: int = 1) -> None:
    rgba = tuple(color)
    if len(rgba) != 4 or rgba[3] == 0xFF:
        pygame_module.draw.line(surface, rgba[:3], start, end, width)
        return
    if rgba[3] == 0:
        return
    left = min(start[0], end[0])
    top = min(start[1], end[1])
    right = max(start[0], end[0]) + width
    bottom = max(start[1], end[1]) + width
    bounds = pygame_module.Rect(left, top, right - left, bottom - top)
    layer = pygame_module.Surface(bounds.size, flags=pygame_module.SRCALPHA)
    pygame_module.draw.line(
        layer,
        rgba,
        (start[0] - left, start[1] - top),
        (end[0] - left, end[1] - top),
        width,
    )
    surface.blit(layer, bounds)


def _paint_text(
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
    glyph = font.render(text, True, tuple(color)[:3])
    glyph_rect = glyph.get_rect()
    glyph_rect.centery = center_y
    if right is not None:
        glyph_rect.right = right
    elif left is not None:
        glyph_rect.left = left
    else:
        raise ValueError("control text needs a left or right edge")
    surface.blit(glyph, glyph_rect)


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

    anchor = _bounds_rect(pygame_module, region_rect, bounds)
    viewport = surface.get_rect().clip(surface.get_clip())
    if region.clipped:
        viewport = viewport.clip(region_rect)
    return anchor, anchor.clip(viewport)


def _scalar_advance(font, character: str, tab_advance: int) -> int:
    if character == "\t":
        return tab_advance
    return max(1, _text_width(font, character))


def _bounded_text_width(font, text: str, maximum: int, tab_advance: int) -> int:
    """Measure only until a renderer-owned pixel bound has been exceeded."""

    width = 0
    for character in text:
        advance = _scalar_advance(font, character, tab_advance)
        if width > maximum - advance:
            return maximum + 1
        width += advance
    return width


def _paint_bounded_scalar_text(
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
) -> None:
    """Paint scalar-at-a-time so clipping never creates a huge text surface."""

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
        for character in text:
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
    pygame_module.draw.lines(surface, tuple(color)[:3], False, points, thickness)


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
    return pygame_module.Rect(left, top, width, height)


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
) -> list[ControlHitTarget]:
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
    visible_popup = popup.clip(viewport)
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
    targets: list[ControlHitTarget] = []
    row_top = popup.top + metrics.popup_padding
    for entry in menu.entries:
        if isinstance(entry, MenuSeparatorDraw):
            separator = pygame_module.Rect(
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

        row = pygame_module.Rect(
            popup.left + metrics.popup_padding,
            row_top,
            popup.width - 2 * metrics.popup_padding,
            metrics.row_height,
        )
        visible_row = row.clip(visible_popup)
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
            targets.append(
                ControlHitTarget(
                    identity,
                    ControlKind.MENU_ITEM,
                    _pixel_rect(visible_row),
                )
            )
        row_top += metrics.row_height
    return targets


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
) -> list[ControlHitTarget]:
    anchor = _bounds_rect(pygame_module, region_rect, draw.bounds)
    viewport = surface.get_rect().clip(surface.get_clip())
    if region.clipped:
        viewport = viewport.clip(region_rect)
    visible_anchor = anchor.clip(viewport)
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return []

    metrics = _menu_metrics(font, cell_width, cell_height)
    root_enabled = bool(draw.state & ControlState.ENABLED)
    targets: list[ControlHitTarget] = []
    open_menus: list[tuple[MenuDraw, object]] = []
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
            title = pygame_module.Rect(
                title_left,
                title_top,
                title_width,
                title_height,
            )
            visible_title = title.clip(visible_anchor)
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
                open_menus.append((menu, title))
            text_color = _TEXT if effectively_enabled else _DISABLED_TEXT
            if visible_title.width > 0 and visible_title.height > 0:
                text_clip = surface.get_clip()
                try:
                    surface.set_clip(visible_title)
                    _paint_text(
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

        # Popups are deliberately painted after every title.  Their item hit
        # targets therefore follow title targets in the same painter order.
        surface.set_clip(viewport)
        for menu, title in open_menus:
            targets.extend(
                _paint_popup(
                    pygame_module,
                    surface,
                    font,
                    region,
                    anchor,
                    viewport,
                    menu,
                    title,
                    metrics,
                    root_enabled=root_enabled,
                    hovered=hovered,
                    pressed=pressed,
                )
            )
    finally:
        surface.set_clip(prior_clip)
    return targets


def _paint_text_area(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: TextAreaDraw,
) -> None:
    """Paint one exact logical text viewport with persistent selection state."""

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module,
        surface,
        region,
        region_rect,
        draw.bounds,
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return
    content = draw.content
    row_start = content.viewport_row
    row_end = row_start + content.viewport_rows
    column_start = content.viewport_column
    column_end = column_start + content.viewport_columns
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

    enabled = bool(draw.state & ControlState.ENABLED)
    text_color = _TEXT if enabled else _DISABLED_TEXT
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, anchor)
        for item in visible_items:
            surface.set_clip(visible_anchor)
            relative_row = item.row - row_start
            row_top = _partition_edge(
                anchor.top,
                anchor.height,
                relative_row,
                content.viewport_rows,
            )
            row_bottom = _partition_edge(
                anchor.top,
                anchor.height,
                relative_row + 1,
                content.viewport_rows,
            )
            row_rect = pygame_module.Rect(
                anchor.left,
                row_top,
                anchor.width,
                row_bottom - row_top,
            )
            visible_row = row_rect.clip(visible_anchor)
            if visible_row.width <= 0 or visible_row.height <= 0:
                continue

            if selection is not None:
                selection_start, selection_end = selection
                if selection_start[0] <= item.row <= selection_end[0]:
                    selected_start = (
                        selection_start[1]
                        if item.row == selection_start[0]
                        else 0
                    )
                    selected_end = (
                        selection_end[1]
                        if item.row == selection_end[0]
                        else len(item.text)
                    )
                    selected_start = max(selected_start, column_start)
                    selected_end = min(
                        selected_end,
                        column_end,
                        len(item.text),
                    )
                    if selected_start < selected_end:
                        selection_left = _partition_edge(
                            anchor.left,
                            anchor.width,
                            selected_start - column_start,
                            content.viewport_columns,
                        )
                        selection_right = _partition_edge(
                            anchor.left,
                            anchor.width,
                            selected_end - column_start,
                            content.viewport_columns,
                        )
                        selected_rect = pygame_module.Rect(
                            selection_left,
                            row_top,
                            selection_right - selection_left,
                            row_bottom - row_top,
                        ).clip(visible_anchor)
                        if selected_rect.width > 0 and selected_rect.height > 0:
                            surface.fill(_TEXT_SELECTION, selected_rect)

            first_scalar = max(column_start, 0)
            last_scalar = min(len(item.text), column_end)
            for scalar_offset in range(first_scalar, last_scalar):
                character = item.text[scalar_offset]
                if character == "\t":
                    continue
                relative_column = scalar_offset - column_start
                slot_left = _partition_edge(
                    anchor.left,
                    anchor.width,
                    relative_column,
                    content.viewport_columns,
                )
                slot_right = _partition_edge(
                    anchor.left,
                    anchor.width,
                    relative_column + 1,
                    content.viewport_columns,
                )
                slot = pygame_module.Rect(
                    slot_left,
                    row_top,
                    slot_right - slot_left,
                    row_bottom - row_top,
                )
                slot_clip = slot.clip(visible_anchor)
                if slot_clip.width <= 0 or slot_clip.height <= 0:
                    continue
                surface.set_clip(slot_clip)
                glyph = font.render(character, True, text_color)
                glyph_rect = glyph.get_rect()
                glyph_rect.left = slot.left
                glyph_rect.centery = slot.centery
                surface.blit(glyph, glyph_rect)

        surface.set_clip(visible_anchor)
        pygame_module.draw.rect(
            surface,
            _ACCENT[:3]
            if draw.state & ControlState.SELECTED
            else _COLLECTION_BORDER,
            anchor,
            width=1,
        )
        if (
            primary_item is not None
            and row_start <= primary_item.row < row_end
            and column_start <= content.primary_offset <= column_end
        ):
            relative_row = primary_item.row - row_start
            row_top = _partition_edge(
                anchor.top,
                anchor.height,
                relative_row,
                content.viewport_rows,
            )
            row_bottom = _partition_edge(
                anchor.top,
                anchor.height,
                relative_row + 1,
                content.viewport_rows,
            )
            caret_x = _partition_edge(
                anchor.left,
                anchor.width,
                content.primary_offset - column_start,
                content.viewport_columns,
            )
            if caret_x >= anchor.right:
                caret_x = anchor.right - 1
            caret = pygame_module.Rect(
                caret_x,
                row_top + (1 if row_bottom - row_top > 2 else 0),
                1,
                max(1, row_bottom - row_top - 2),
            ).clip(visible_anchor)
            if caret.width > 0 and caret.height > 0:
                surface.fill(_ACCENT[:3] if enabled else _DISABLED_TEXT, caret)
    finally:
        surface.set_clip(prior_clip)


def _paint_text_grid(
    pygame_module,
    surface,
    font,
    region,
    region_rect,
    draw: TextGridDraw,
    cell_width: int,
) -> None:
    """Paint logical grid spans directly, without materializing a cell matrix."""

    anchor, visible_anchor = _semantic_root_rects(
        pygame_module,
        surface,
        region,
        region_rect,
        draw.bounds,
    )
    if visible_anchor.width <= 0 or visible_anchor.height <= 0:
        return
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
        surface.fill(_COLLECTION_SURFACE, anchor)
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
            _paint_bounded_scalar_text(
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
            )

        surface.set_clip(visible_anchor)
        pygame_module.draw.rect(
            surface,
            _ACCENT[:3]
            if draw.state & ControlState.SELECTED
            else _COLLECTION_BORDER,
            anchor,
            width=1,
        )
    finally:
        surface.set_clip(prior_clip)


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
) -> list[ControlHitTarget]:
    """Lay out generic tabs and return only enabled TAB activation targets."""

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
    targets: list[ControlHitTarget] = []
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(visible_anchor)
        surface.fill(_COLLECTION_SURFACE, anchor)
        pygame_module.draw.rect(
            surface,
            _COLLECTION_BORDER,
            anchor,
            width=1,
        )
        natural_left = anchor.left
        for index, tab in enumerate(draw.tabs):
            if natural_layout:
                tab_rect = pygame_module.Rect(
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
                tab_rect = pygame_module.Rect(
                    tab_left,
                    anchor.top,
                    tab_right - tab_left,
                    anchor.height,
                )
            visible_tab = tab_rect.clip(visible_anchor)
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
            _paint_bounded_scalar_text(
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
                _paint_bounded_scalar_text(
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
    if alpha == 0xFF:
        pygame_module.draw.line(
            surface,
            color,
            (slot.left, y),
            (slot.right - 1, y),
        )
        return
    layer = pygame_module.Surface(slot.size, flags=pygame_module.SRCALPHA)
    pygame_module.draw.line(
        layer,
        (*color, alpha),
        (0, y - slot.top),
        (slot.width - 1, y - slot.top),
    )
    surface.blit(layer, slot)


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
    if len(rgba) != 4 or rgba[3] == 0xFF:
        pygame_module.draw.circle(surface, rgba[:3], center, radius)
        return
    if rgba[3] == 0:
        return
    diameter = 2 * radius + 1
    bounds = pygame_module.Rect(
        center[0] - radius,
        center[1] - radius,
        diameter,
        diameter,
    )
    layer = pygame_module.Surface(bounds.size, flags=pygame_module.SRCALPHA)
    pygame_module.draw.circle(layer, rgba, (radius, radius), radius)
    surface.blit(layer, bounds)


def _paint_polyline(pygame_module, surface, region, region_rect, draw) -> None:
    object_rect = _object_rect(pygame_module, region_rect, draw)
    clip = object_rect.clip(surface.get_rect())
    if region.clipped:
        clip = clip.clip(region_rect)
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
    object_rect = _object_rect(pygame_module, region_rect, draw)
    clip = object_rect.clip(surface.get_rect()).clip(surface.get_clip())
    if region.clipped:
        clip = clip.clip(region_rect)
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
        _paint_bounded_scalar_text(
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
                fill_rect = pygame_module.Rect(
                    object_rect.left,
                    object_rect.bottom - filled,
                    object_rect.width,
                    filled,
                )
            else:
                fill_rect = pygame_module.Rect(
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
            _paint_bounded_scalar_text(
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
    shape_rect = pygame_module.Rect(
        (object_rect.width - side) // 2,
        (object_rect.height - side) // 2,
        side,
        side,
    )
    layer = pygame_module.Surface(object_rect.size, flags=pygame_module.SRCALPHA)
    rgba = (*_rgb(color), color.alpha)
    if draw.shape == 0:
        pygame_module.draw.ellipse(layer, rgba, shape_rect)
    elif draw.shape == 1:
        pygame_module.draw.rect(layer, rgba, shape_rect)
    else:
        pygame_module.draw.polygon(
            layer,
            rgba,
            (
                (shape_rect.centerx, shape_rect.top),
                (shape_rect.right - 1, shape_rect.centery),
                (shape_rect.centerx, shape_rect.bottom - 1),
                (shape_rect.left, shape_rect.centery),
            ),
        )
    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        surface.blit(layer, object_rect)
    finally:
        surface.set_clip(prior_clip)


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
    if len(rgba) != 4 or rgba[3] == 0xFF:
        pygame_module.draw.polygon(surface, rgba[:3], points)
        return
    left = min(point[0] for point in points)
    top = min(point[1] for point in points)
    right = max(point[0] for point in points) + 1
    bottom = max(point[1] for point in points) + 1
    layer = pygame_module.Surface((right - left, bottom - top), flags=pygame_module.SRCALPHA)
    pygame_module.draw.polygon(
        layer,
        rgba,
        tuple((point[0] - left, point[1] - top) for point in points),
    )
    surface.blit(layer, (left, top))


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


def _paint_glyph_run(pygame_module, surface, font, region, region_rect, draw):
    object_rect = _object_rect(pygame_module, region_rect, draw)
    clip = object_rect.clip(surface.get_rect())
    if region.clipped:
        clip = clip.clip(region_rect)
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
            surface.fill(_rgb(background), object_rect)
        elif background.alpha:
            layer = pygame_module.Surface(
                object_rect.size,
                flags=pygame_module.SRCALPHA,
            )
            layer.fill((*_rgb(background), background.alpha))
            surface.blit(layer, object_rect)
        count = len(draw.text)
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
        try:
            for index, codepoint in enumerate(draw.text):
                left = object_rect.left + (index * object_rect.width) // count
                right = object_rect.left + ((index + 1) * object_rect.width) // count
                slot = pygame_module.Rect(
                    left,
                    object_rect.top,
                    right - left,
                    object_rect.height,
                )
                slot_clip = slot.clip(clip)
                if slot_clip.width <= 0 or slot_clip.height <= 0:
                    continue
                surface.set_clip(slot_clip)
                glyph = font.render(codepoint, True, color)
                if foreground.alpha != 0xFF:
                    glyph = glyph.copy()
                    glyph.fill(
                        (255, 255, 255, foreground.alpha),
                        special_flags=pygame_module.BLEND_RGBA_MULT,
                    )
                # A glyph run uses the same cell origin as the mandatory CELL
                # renderer.  Every glyph and decoration is clipped to its own
                # equal slot, so font overhang cannot alter an adjacent cell.
                origin = (slot.left, slot.top)
                surface.blit(glyph, origin)
                if draw.attributes & ATTR_BOLD:
                    surface.blit(glyph, (origin[0] + 1, origin[1]))
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
        if (
            not callable(getattr(source, "copy", None))
            or not callable(getattr(source, "set_alpha", None))
            or not callable(getattr(source, "subsurface", None))
        ):
            raise TypeError(
                "IMAGE resource surface lacks immutable crop/opacity operations"
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

    if resolved:
        transform = getattr(pygame_module, "transform", None)
        if transform is None or not callable(getattr(transform, "scale", None)):
            raise TypeError("pygame module has no IMAGE scaling operation")
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
    image = source
    if source_crop is not None:
        image = source.subsurface(pygame_module.Rect(*source_crop))
    if tuple(image.get_size()) != target_size:
        image = pygame_module.transform.scale(image, target_size)
    if draw.opacity != 0xFF:
        # Even a freshly scaled surface is copied here so opacity application
        # has one simple ownership rule and can never alter a viewer cache.
        image = image.copy()
        image.set_alpha(draw.opacity)

    prior_clip = surface.get_clip()
    try:
        surface.set_clip(clip)
        surface.blit(image, target_position)
    finally:
        surface.set_clip(prior_clip)


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
    targets: list[ControlHitTarget] = []
    for region in plane.regions:
        region_rect = pygame_module.Rect(
            region.cell_x * cell_w,
            region.cell_y * cell_h,
            region.cell_cols * cell_w,
            region.cell_rows * cell_h,
        )
        if not surface.get_rect().contains(region_rect):
            raise ValueError("retained region exceeds the CELL surface")
        for draw in region.draws:
            if isinstance(draw, GlyphRunDraw):
                _paint_glyph_run(
                    pygame_module,
                    surface,
                    font,
                    region,
                    region_rect,
                    draw,
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
                targets.extend(
                    _paint_menu_bar(
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
            elif isinstance(draw, TextAreaDraw):
                _paint_text_area(
                    pygame_module,
                    surface,
                    font,
                    region,
                    region_rect,
                    draw,
                )
            elif isinstance(draw, TextGridDraw):
                _paint_text_grid(
                    pygame_module,
                    surface,
                    control_font,
                    region,
                    region_rect,
                    draw,
                    cell_w,
                )
            elif isinstance(draw, TabSetDraw):
                targets.extend(
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
    return CompositeDrawResult(surface, tuple(targets))


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
    "ControlHitTarget",
    "ControlIdentity",
    "ImageSurfaceKey",
    "PixelRect",
    "composite_draw_plane",
    "composite_draw_plane_result",
    "unorm_high_edge",
    "unorm_low_edge",
]
