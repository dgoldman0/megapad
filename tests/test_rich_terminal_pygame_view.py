"""Deterministic off-screen pixel tests for generic draw-plane composition."""

from __future__ import annotations

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.pygame_view import composite_draw_plane, unorm_high_edge, unorm_low_edge
from rich_terminal.retained_scene import ObjectBounds, RGBA
from rich_terminal.retained_view import GlyphRunDraw, RetainedDrawPlane, RetainedRegionDraw


def _run(object_id, z_order, foreground, background, text, *, attributes=0):
    return GlyphRunDraw(object_id, z_order, ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX), RGBA(*foreground), RGBA(*background), attributes, text)


def _region(*draws, clipped=True):
    return RetainedRegionDraw(1, 1, 1, 0, 0, 2, 1, 0, clipped, draws)


def _plane(region):
    return RetainedDrawPlane(True, True, (region,))


class _PixelFont:
    def __init__(self, pygame_module):
        self.pygame = pygame_module
        self.rendered = []
        self.italic = False
        self.italic_changes = []

    def get_italic(self):
        return self.italic

    def set_italic(self, value):
        self.italic = bool(value)
        self.italic_changes.append(self.italic)

    def render(self, text, antialias, color):
        assert antialias and len(text) == 1
        self.rendered.append((text, color))
        glyph = self.pygame.Surface((1, 1), flags=self.pygame.SRCALPHA)
        glyph.fill((*color, 255))
        return glyph


class _OverhangFont(_PixelFont):
    def render(self, text, antialias, color):
        assert antialias and len(text) == 1
        self.rendered.append((text, color))
        size = (5, 2) if text == "A" else (1, 1)
        glyph = self.pygame.Surface(size, flags=self.pygame.SRCALPHA)
        glyph.fill((220, 30, 40, 255))
        return glyph


def test_unorm_edges_use_exact_floor_and_ceiling_without_float_rounding():
    half_low = UINT32_MAX // 2
    assert unorm_low_edge(0, 10) == 0
    assert unorm_high_edge(UINT32_MAX, 10) == 10
    assert unorm_low_edge(half_low, 10) == 4
    assert unorm_high_edge(half_low, 10) == 5
    with pytest.raises(TypeError, match="not bool"):
        unorm_low_edge(True, 10)


def test_compositor_fills_opaque_background_rasterizes_equal_slots_and_orders_draws():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    surface.fill((1, 2, 3))
    font = _PixelFont(pygame)
    region = _region(
        _run(1, 0, (220, 30, 40, 255), (10, 20, 30, 255), "AB"),
        _run(2, 1, (20, 210, 70, 255), (40, 50, 60, 255), "CD"),
    )

    result = composite_draw_plane(pygame, surface, _plane(region), font, 4, 5)

    assert result is surface
    assert tuple(surface.get_at((0, 0)))[:3] == (20, 210, 70)
    assert tuple(surface.get_at((4, 0)))[:3] == (20, 210, 70)
    assert tuple(surface.get_at((2, 2)))[:3] == (40, 50, 60)
    assert [item[0] for item in font.rendered] == ["A", "B", "C", "D"]
    pygame.draw.rect(surface, (255, 255, 255), (0, 4, 1, 1))
    assert tuple(surface.get_at((0, 4)))[:3] == (255, 255, 255)


def test_compositor_renders_attributes_deterministically():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    font = _PixelFont(pygame)
    # reverse selects background as glyph color and foreground as the fill;
    # underline and strike provide deterministic full-width pixels.
    draw = _run(1, 0, (200, 100, 40, 255), (20, 60, 180, 255), "X", attributes=0x20 | 0x08 | 0x40)

    composite_draw_plane(pygame, surface, _plane(_region(draw)), font, 4, 5)

    assert tuple(surface.get_at((7, 2)))[:3] == (20, 60, 180)  # strike
    assert tuple(surface.get_at((7, 4)))[:3] == (20, 60, 180)  # underline
    assert tuple(surface.get_at((3, 1)))[:3] == (200, 100, 40)  # reversed fill


def test_compositor_renders_italic_and_restores_the_caller_font_state():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    font = _PixelFont(pygame)
    draw = _run(1, 0, (200, 100, 40, 255), (20, 60, 180, 255), "X", attributes=0x04)

    composite_draw_plane(pygame, surface, _plane(_region(draw)), font, 4, 5)

    assert font.italic_changes == [True, False]
    assert font.italic is False


def test_compositor_clips_font_overhang_to_each_equal_scalar_slot():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    surface.fill((10, 20, 30))
    font = _OverhangFont(pygame)
    draw = _run(1, 0, (220, 30, 40, 255), (10, 20, 30, 255), "AB")

    composite_draw_plane(pygame, surface, _plane(_region(draw)), font, 4, 5)

    assert tuple(surface.get_at((3, 1)))[:3] == (220, 30, 40)
    assert tuple(surface.get_at((4, 1)))[:3] == (10, 20, 30)


def test_translucent_decoration_uses_source_over_instead_of_opaque_rgb():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    font = _PixelFont(pygame)
    draw = _run(
        1,
        0,
        (200, 100, 0, 128),
        (20, 40, 60, 255),
        "X",
        attributes=0x08,
    )

    composite_draw_plane(pygame, surface, _plane(_region(draw)), font, 4, 5)

    assert tuple(surface.get_at((7, 4)))[:3] == pytest.approx((110, 70, 30), abs=1)


def test_transparent_foreground_suppresses_glyph_but_preserves_background_and_clip():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((8, 5))
    surface.fill((2, 4, 6))
    surface.set_clip(pygame.Rect(4, 0, 4, 5))
    font = _PixelFont(pygame)
    draw = _run(1, 0, (255, 255, 255, 0), (80, 90, 100, 255), "X")

    composite_draw_plane(pygame, surface, _plane(_region(draw)), font, 4, 5)

    assert tuple(surface.get_at((0, 0)))[:3] == (2, 4, 6)
    assert tuple(surface.get_at((4, 0)))[:3] == (80, 90, 100)
    assert font.rendered == []
    assert surface.get_clip() == pygame.Rect(4, 0, 4, 5)
