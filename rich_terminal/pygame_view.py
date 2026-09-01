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
    """Visible region coverage that blocks controls painted below the region."""

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


HitMapEntry = ControlHitTarget | RegionOcclusion


def _validated_hit_entries(hit_entries) -> tuple[HitMapEntry, ...]:
    entries = tuple(hit_entries)
    if any(
        not isinstance(entry, (ControlHitTarget, RegionOcclusion))
        for entry in entries
    ):
        raise TypeError(
            "hit_entries must contain only ControlHitTarget or "
            "RegionOcclusion values"
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


@dataclass(frozen=True, slots=True)
class CompositeDrawResult:
    """One completed paint pass and its immutable semantic hit map.

    ``hit_entries`` is stored in back-to-front painter order.  A region's
    occlusion precedes its own controls, so reverse testing lets those controls
    win and then stops before any lower region.  ``hit_targets`` remains a
    filtered inspection view; barriers are never represented as fake controls.
    """

    surface: object
    hit_entries: tuple[HitMapEntry, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "hit_entries",
            _validated_hit_entries(self.hit_entries),
        )

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
    text_width = sum(_scalar_advance(font, character, tab_advance) for character in text)
    if right is not None:
        logical_left = right - text_width
        logical_right = right
    elif left is not None:
        logical_left = left
        logical_right = left + text_width
    else:
        raise ValueError("control text needs a left or right edge")
    font_height = _font_height(font, viewport.height)
    _paint_bounded_scalar_text(
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
    targets: list[ControlHitTarget] = []
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
                open_menus.append((menu, title))
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
        surface.fill(_COLLECTION_SURFACE, visible_anchor)
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
            row_rect = _WideRect(
                anchor.left,
                row_top,
                anchor.width,
                row_bottom - row_top,
            )
            visible_row = _bounded_pygame_rect(
                pygame_module,
                row_rect,
                visible_anchor,
            )
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
                        selected_rect = _clipped_python_rect(
                            pygame_module,
                            selection_left,
                            row_top,
                            selection_right,
                            row_bottom,
                            visible_anchor,
                        )
                        if selected_rect is not None:
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
                slot = _WideRect(
                    slot_left,
                    row_top,
                    slot_right - slot_left,
                    row_bottom - row_top,
                )
                slot_clip = _bounded_pygame_rect(
                    pygame_module,
                    slot,
                    visible_anchor,
                )
                if slot_clip.width <= 0 or slot_clip.height <= 0:
                    continue
                _paint_bounded_scalar_text(
                    pygame_module,
                    surface,
                    font,
                    character,
                    text_color,
                    slot_clip,
                    left=slot.left,
                    right=slot.right,
                    top=slot.top,
                    bottom=slot.bottom,
                    tab_advance=max(1, slot.width),
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


def _paint_glyph_run(pygame_module, surface, font, region, region_rect, draw):
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
    hit_entries: list[HitMapEntry] = []
    for region in plane.regions:
        region_rect = _WideRect(
            region.logical_x * cell_w,
            region.logical_y * cell_h,
            region.logical_cols * cell_w,
            region.logical_rows * cell_h,
        )
        region_coverage = _bounded_pygame_rect(
            pygame_module,
            region_rect,
            _region_viewport(pygame_module, surface, region, region_rect),
        )
        if region_coverage.width > 0 and region_coverage.height > 0:
            hit_entries.append(
                RegionOcclusion(
                    region.owner_id,
                    region.owner_generation,
                    region.region_id,
                    PixelRect(
                        region_coverage.left,
                        region_coverage.top,
                        region_coverage.right,
                        region_coverage.bottom,
                    ),
                )
            )
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
                hit_entries.extend(
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
    return CompositeDrawResult(surface, tuple(hit_entries))


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
    "HitMapEntry",
    "ImageSurfaceKey",
    "PixelRect",
    "RegionOcclusion",
    "composite_draw_plane",
    "composite_draw_plane_result",
    "hit_test_hit_map",
    "unorm_high_edge",
    "unorm_low_edge",
]
