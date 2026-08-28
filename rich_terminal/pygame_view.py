"""Pygame compositor for the retained generic draw plane.

The caller owns CELL rendering and paints the cursor after this compositor.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass

from .apt1 import UINT32_MAX, UINT64_MAX
from .retained_scene import ControlKind, ControlState
from .retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    RetainedDrawPlane,
)

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
        if kind not in (ControlKind.MENU, ControlKind.MENU_ITEM):
            raise ValueError("only MENU and MENU_ITEM can be hit targets")
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


def _object_rect(pygame_module, region_rect, draw: GlyphRunDraw):
    return _bounds_rect(pygame_module, region_rect, draw.bounds)


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


def composite_draw_plane_result(
    pygame_module,
    surface,
    plane,
    font,
    cell_width: int,
    cell_height: int,
    *,
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
            else:  # RetainedDrawPlane validates this, but fail closed if forged.
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
        control_font=control_font,
        hovered=hovered,
        pressed=pressed,
    ).surface


__all__ = [
    "CompositeDrawResult",
    "ControlHitTarget",
    "ControlIdentity",
    "PixelRect",
    "composite_draw_plane",
    "composite_draw_plane_result",
    "unorm_high_edge",
    "unorm_low_edge",
]
