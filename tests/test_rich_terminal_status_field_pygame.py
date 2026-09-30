"""Structured status paints explicit slots without changing input geometry."""

from dataclasses import replace

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal
from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.pygame_view import composite_draw_plane_result
from rich_terminal.retained_scene import ObjectBounds, StatusSeverity
from rich_terminal.retained_view import (
    RetainedDrawPlane, RetainedRegionDraw, StatusFieldDraw,
)
from session_viewer import compose_terminal_frame_changes, compose_terminal_frame_result


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _draw(**changes):
    values = dict(object_id=1, z_order=0, bounds=ObjectBounds(1, 2, 20, 1),
                  label="Mode", value="Ready", label_cols=8,
                  severity=StatusSeverity.SUCCESS, emphasized=False)
    values.update(changes)
    return StatusFieldDraw(**values)


def _plane(draw):
    region = RetainedRegionDraw(1, 1, 1, 0, 0, 30, 10,
                                0, 0, 30, 10, 0, True, () if draw is None else (draw,))
    return RetainedDrawPlane(True, True, (region,))


def _paint(draw, font, appearance, surface=None):
    if surface is None:
        surface = pygame.Surface((240, 120))
        surface.fill((63, 22, 97))
    return composite_draw_plane_result(pygame, surface, _plane(draw), font, 8, 12,
                                       appearance=appearance)


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_status_field_clips_to_exact_footprint_and_restores_clip(font, appearance):
    surface = pygame.Surface((240, 120))
    surface.fill((63, 22, 97))
    before = surface.copy()
    clip = pygame.Rect(13, 20, 127, 13)
    surface.set_clip(clip)
    result = _paint(_draw(emphasized=True), font, appearance, surface)
    footprint = pygame.Rect(8, 24, 160, 12)
    changed = 0
    for y in range(120):
        for x in range(240):
            if surface.get_at((x, y)) != before.get_at((x, y)):
                changed += 1
                assert footprint.collidepoint(x, y) and clip.collidepoint(x, y)
    assert changed
    assert surface.get_clip() == clip
    assert not result.hit_targets


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_long_label_and_value_cannot_cross_the_guest_cell_split(font, appearance):
    ordinary = _paint(_draw(), font, appearance).surface
    long_label = _paint(_draw(label="Label " * 100), font, appearance).surface
    long_value = _paint(_draw(value="Value " * 100), font, appearance).surface
    label = pygame.Rect(8, 24, 64, 12)
    value = pygame.Rect(72, 24, 96, 12)
    assert pygame.image.tobytes(ordinary.subsurface(value), "RGBA") == pygame.image.tobytes(long_label.subsurface(value), "RGBA")
    assert pygame.image.tobytes(ordinary.subsurface(label), "RGBA") == pygame.image.tobytes(long_value.subsurface(label), "RGBA")


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_status_changes_moves_and_drop_match_complete_composition(font, appearance):
    terminal = VirtualTerminal(cols=30, rows=10)
    terminal.write(b"Underlying text\r\nSecond row\r\nStatus fallback")
    previous = None
    glyph_cache = {}
    initial = _draw()
    for draw in (initial, replace(initial, value="Dirty", severity=StatusSeverity.WARNING),
                 replace(initial, value="Failed", severity=StatusSeverity.ERROR, emphasized=True),
                 replace(initial, bounds=ObjectBounds(4, 4, 20, 1)), None):
        incremental = compose_terminal_frame_changes(
            pygame, terminal, font, 8, 12, retained_plane=_plane(draw), show_cursor=False,
            previous=previous, appearance=appearance, glyph_cache=glyph_cache,
        )
        full = compose_terminal_frame_result(
            pygame, terminal, font, 8, 12, retained_plane=_plane(draw),
            show_cursor=False, appearance=appearance,
        )
        assert pygame.image.tobytes(incremental.surface, "RGBA") == pygame.image.tobytes(full.surface, "RGBA")
        assert incremental.hit_entries == full.hit_entries
        previous = incremental


def test_wide_status_bounds_preserve_group_translation_and_region_clip(font):
    draw = _draw(bounds=ObjectBounds(-(1 << 31), 0, (1 << 32) - 1, 1),
                 label="", label_cols=0, value="", emphasized=True,
                 parent_bounds=(ObjectBounds(3, 2, 2, 1),))
    # GROUP supplies coordinates and visibility, while the region supplies
    # clipping. Allocation stays bounded to the visible surface.
    surface = pygame.Surface((240, 120))
    surface.fill((63, 22, 97))
    _paint(draw, font, FLOWING_APPEARANCE, surface)
    assert surface.get_at((0, 0))[:3] == (63, 22, 97)
    assert surface.get_at((30, 29))[:3] != (63, 22, 97)
    assert surface.get_at((80, 25))[:3] != (63, 22, 97)
    assert surface.get_at((80, 40))[:3] == (63, 22, 97)
