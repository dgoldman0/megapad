"""Pygame compositor for the retained generic draw plane.

The caller owns CELL rendering and paints the cursor after this compositor.
"""

from __future__ import annotations

import operator

from .apt1 import UINT32_MAX
from .retained_view import GlyphRunDraw, RetainedDrawPlane

ATTR_BOLD = 0x01
ATTR_DIM = 0x02
ATTR_ITALIC = 0x04
ATTR_UNDERLINE = 0x08
ATTR_REVERSE = 0x20
ATTR_STRIKE = 0x40


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


def _object_rect(pygame_module, region_rect, draw: GlyphRunDraw):
    left = unorm_low_edge(draw.bounds.left, region_rect.width)
    top = unorm_low_edge(draw.bounds.top, region_rect.height)
    right = unorm_high_edge(draw.bounds.right, region_rect.width)
    bottom = unorm_high_edge(draw.bounds.bottom, region_rect.height)
    return pygame_module.Rect(
        region_rect.left + left,
        region_rect.top + top,
        right - left,
        bottom - top,
    )


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


def composite_draw_plane(
    pygame_module,
    surface,
    plane,
    font,
    cell_width: int,
    cell_height: int,
):
    """Composite the draw plane between caller-owned CELL and cursor layers."""
    if not isinstance(plane, RetainedDrawPlane):
        raise TypeError("plane must be RetainedDrawPlane")
    cell_w = _integer("cell_width", cell_width, minimum=1)
    cell_h = _integer("cell_height", cell_height, minimum=1)
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
            _paint_glyph_run(pygame_module, surface, font, region, region_rect, draw)
    return surface


__all__ = ["composite_draw_plane", "unorm_high_edge", "unorm_low_edge"]
