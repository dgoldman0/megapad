"""Pygame compositor for the first retained root-LABEL rendering slice.

The caller owns CELL rendering and the cursor overlay.  This module mutates the
already-rendered CELL surface only by source-over compositing the retained plane
between those two layers; it imports no pygame implementation at module load.
"""

from __future__ import annotations

import operator

from .apt1 import UINT32_MAX
from .retained_view import (
    RetainedLabelDraw,
    RetainedRegionDraw,
    RetainedRootLabelPlane,
)


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
    """Map a UNORM32 low edge by exact integer floor."""

    normalized = _integer("value", value, minimum=0, maximum=UINT32_MAX)
    pixels = _integer("extent", extent, minimum=0)
    return (normalized * pixels) // UINT32_MAX


def unorm_high_edge(value: int, extent: int) -> int:
    """Map a UNORM32 high edge by exact integer ceiling."""

    normalized = _integer("value", value, minimum=0, maximum=UINT32_MAX)
    pixels = _integer("extent", extent, minimum=0)
    numerator = normalized * pixels
    return (numerator + UINT32_MAX - 1) // UINT32_MAX


def _ellipsized_text(font, text: str, available_width: int) -> str:
    if not text or font.size(text)[0] <= available_width:
        return text
    ellipsis = "\u2026"
    if font.size(ellipsis)[0] > available_width:
        return ""
    # Font shaping and kerning do not guarantee monotone prefix widths, so use
    # a bounded descending search rather than assuming a binary-search order.
    for end in range(len(text) - 1, -1, -1):
        candidate = text[:end] + ellipsis
        if font.size(candidate)[0] <= available_width:
            return candidate
    return ""


def _label_text(label: RetainedLabelDraw, font, available_width: int) -> str:
    if not label.ellipsize:
        return label.text
    return _ellipsized_text(font, label.text, available_width)


def _aligned_origin(label, object_rect, glyph_width: int, glyph_height: int):
    if label.horizontal_align == 0:
        x = object_rect.left
    elif label.horizontal_align == 1:
        x = object_rect.left + (object_rect.width - glyph_width) // 2
    else:
        x = object_rect.right - glyph_width

    if label.vertical_align == 0:
        y = object_rect.top
    elif label.vertical_align == 1:
        y = object_rect.top + (object_rect.height - glyph_height) // 2
    else:
        y = object_rect.bottom - glyph_height
    return x, y


def _object_rect(pygame_module, region_rect, label: RetainedLabelDraw):
    left = unorm_low_edge(label.left, region_rect.width)
    top = unorm_low_edge(label.top, region_rect.height)
    right = unorm_high_edge(label.right, region_rect.width)
    bottom = unorm_high_edge(label.bottom, region_rect.height)
    return pygame_module.Rect(
        region_rect.left + left,
        region_rect.top + top,
        right - left,
        bottom - top,
    )


def _render_label(
    pygame_module,
    surface,
    font,
    region: RetainedRegionDraw,
    region_rect,
    label: RetainedLabelDraw,
) -> None:
    object_rect = _object_rect(pygame_module, region_rect, label)
    clip = object_rect.clip(surface.get_rect())
    if region.clipped:
        clip = clip.clip(region_rect)
    prior_clip = surface.get_clip()
    clip = clip.clip(prior_clip)
    if clip.width <= 0 or clip.height <= 0 or label.alpha == 0:
        return

    text = _label_text(label, font, object_rect.width)
    if not text:
        return
    glyph = font.render(text, True, (label.red, label.green, label.blue))
    if label.alpha != 0xFF:
        glyph = glyph.copy()
        glyph.fill(
            (0xFF, 0xFF, 0xFF, label.alpha),
            special_flags=pygame_module.BLEND_RGBA_MULT,
        )
    origin = _aligned_origin(label, object_rect, glyph.get_width(), glyph.get_height())
    try:
        surface.set_clip(clip)
        surface.blit(glyph, origin)
    finally:
        surface.set_clip(prior_clip)


def composite_root_labels(
    pygame_module,
    surface,
    plane: RetainedRootLabelPlane,
    font,
    cell_width: int,
    cell_height: int,
):
    """Composite ``plane`` onto an existing CELL surface and return it.

    Regions and LABEL values are already immutable and deterministically sorted
    by :mod:`rich_terminal.retained_view`.  The cursor is deliberately not
    drawn here so the caller can paint it last.
    """

    if not isinstance(plane, RetainedRootLabelPlane):
        raise TypeError("plane must be RetainedRootLabelPlane")
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
        for label in region.labels:
            _render_label(
                pygame_module,
                surface,
                font,
                region,
                region_rect,
                label,
            )
    return surface


__all__ = [
    "composite_root_labels",
    "unorm_high_edge",
    "unorm_low_edge",
]
