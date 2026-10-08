"""Host-owned appearances and bounded, antialiased channel materials.

An appearance never enters the retained protocol.  The small nine-patch
atlases below cache curved edges, not window-sized surfaces: their allocation
is independent of guest coordinates, pane size, and the number of frames.
"""

from __future__ import annotations

from collections import OrderedDict
from dataclasses import dataclass


@dataclass(frozen=True, slots=True)
class Appearance:
    name: str
    flowing: bool = False
    surface: tuple[int, int, int] = (18, 23, 31)
    channel: tuple[int, int, int] = (23, 32, 38)
    border: tuple[int, int, int] = (74, 94, 103)
    highlight: tuple[int, int, int] = (114, 140, 148)
    accent: tuple[int, int, int] = (132, 246, 218)
    selection: tuple[int, int, int] = (35, 62, 65)

    def __post_init__(self):
        if not isinstance(self.name, str) or not self.name:
            raise ValueError("appearance name must be nonempty text")
        if not isinstance(self.flowing, bool):
            raise TypeError("flowing must be bool")
        for name in ("surface", "channel", "border", "highlight", "accent", "selection"):
            value = getattr(self, name)
            if not isinstance(value, tuple) or len(value) != 3 or any(
                type(channel) is not int or not 0 <= channel <= 255 for channel in value
            ):
                raise ValueError(f"{name} must be an immutable RGB tuple")


REFERENCE_APPEARANCE = Appearance("reference")
FLOWING_APPEARANCE = Appearance("flowing", flowing=True, surface=(12, 20, 25))


def get_appearance(name: str) -> Appearance:
    """Resolve a viewer option without mutating a process-wide theme."""
    if name == "reference":
        return REFERENCE_APPEARANCE
    if name == "flowing":
        return FLOWING_APPEARANCE
    raise ValueError(f"unknown appearance {name!r}; choose reference or flowing")


_ATLAS_SCALE = 4
_MAX_RADIUS = 32
_CACHE_MAX_BYTES = 1024 * 1024
_CACHE_MAX_ENTRIES = 128
_CHANNEL_CACHE: OrderedDict[tuple, object] = OrderedDict()
_channel_cache_bytes = 0


def _rgba(color):
    return tuple(color) if len(color) == 4 else (*color, 255)


def _channel_atlas(pygame, radius, fill, border, highlight):
    """Return one bounded square nine-patch; its centre is one pixel wide."""
    global _channel_cache_bytes
    key = (id(pygame), radius, fill, border, highlight)
    cached = _CHANNEL_CACHE.get(key)
    if cached is not None:
        _CHANNEL_CACHE.move_to_end(key)
        return cached
    side = 2 * (radius + 1) + 1
    scale = _ATLAS_SCALE
    large = pygame.Surface((side * scale, side * scale), flags=pygame.SRCALPHA)
    bounds = large.get_rect()
    pygame.draw.rect(large, fill, bounds, border_radius=radius * scale)
    if border is not None:
        pygame.draw.rect(large, border, bounds, width=scale, border_radius=radius * scale)
    if highlight is not None and side > 5:
        # The top inner edge supplies shallow material depth.  It is part of
        # the atlas, so clipped and complete paints use exactly the same edge.
        large.set_clip(pygame.Rect(0, 0, side * scale, 2 * scale))
        pygame.draw.rect(
            large, highlight, bounds.inflate(-2 * scale, -2 * scale),
            width=scale, border_radius=max(0, (radius - 1) * scale),
        )
        large.set_clip(None)
    atlas = pygame.transform.smoothscale(large, (side, side))
    cost = atlas.get_pitch() * atlas.get_height()
    while _CHANNEL_CACHE and (
        len(_CHANNEL_CACHE) >= _CACHE_MAX_ENTRIES
        or _channel_cache_bytes + cost > _CACHE_MAX_BYTES
    ):
        _, old = _CHANNEL_CACHE.popitem(last=False)
        _channel_cache_bytes -= old.get_pitch() * old.get_height()
    _CHANNEL_CACHE[key] = atlas
    _channel_cache_bytes += cost
    return atlas


def paint_channel(pygame, surface, rect, fill, *, radius=12, border=None, highlight=None):
    """Paint a rounded material within RECT and the caller's existing clip.

    Corners are cached at four-times resolution and filtered once.  Straight
    edges are nine-patch strips.  Each destination is clipped in Python before
    constructing a native Rect or allocating a scaled strip, even when RECT
    comes from an extreme wire coordinate.  Nothing paints outside RECT.
    """
    width, height = rect.width, rect.height
    if width <= 0 or height <= 0:
        return
    clip = surface.get_rect().clip(surface.get_clip())
    if rect.right <= clip.left or rect.left >= clip.right or rect.bottom <= clip.top or rect.top >= clip.bottom:
        return
    radius = min(max(0, radius), _MAX_RADIUS, max(0, (min(width, height) - 3) // 2))
    fill = _rgba(fill)
    border = None if border is None else _rgba(border)
    highlight = None if highlight is None else _rgba(highlight)
    if min(width, height) < 3:
        visible = pygame.Rect(
            max(rect.left, clip.left), max(rect.top, clip.top),
            min(rect.right, clip.right) - max(rect.left, clip.left),
            min(rect.bottom, clip.bottom) - max(rect.top, clip.top),
        )
        layer = pygame.Surface(visible.size, flags=pygame.SRCALPHA)
        layer.fill(fill)
        surface.blit(layer, visible)
        return
    atlas = _channel_atlas(pygame, radius, fill, border, highlight)
    corner = radius + 1
    side = atlas.get_width()
    xs = (rect.left, rect.left + corner, rect.right - corner, rect.right)
    ys = (rect.top, rect.top + corner, rect.bottom - corner, rect.bottom)
    sources = (0, corner, corner + 1, side)
    for row in range(3):
        for column in range(3):
            left, top = max(xs[column], clip.left), max(ys[row], clip.top)
            right, bottom = min(xs[column + 1], clip.right), min(ys[row + 1], clip.bottom)
            if left >= right or top >= bottom:
                continue
            sx, sy = sources[column], sources[row]
            sw, sh = sources[column + 1] - sx, sources[row + 1] - sy
            # Only the centre row/column stretches.  Crop the unscaled axes
            # before scaling so a damaged corner matches a complete paint.
            if column != 1:
                sx += left - xs[column]
                sw = right - left
            if row != 1:
                sy += top - ys[row]
                sh = bottom - top
            source = atlas.subsurface(pygame.Rect(sx, sy, sw, sh))
            size = (right - left, bottom - top)
            if sw == sh == 1:
                color = source.get_at((0, 0))
                if color.a == 0:
                    continue
                if color.a == 255:
                    surface.fill(color, pygame.Rect(left, top, *size))
                    continue
            if source.get_size() != size:
                source = pygame.transform.scale(source, size)
            surface.blit(source, (left, top))


__all__ = ["Appearance", "REFERENCE_APPEARANCE", "FLOWING_APPEARANCE", "get_appearance"]
