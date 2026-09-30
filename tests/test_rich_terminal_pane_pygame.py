"""Pane chrome must preserve occupied cells, hit geometry, and partial paint."""

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal
from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.pygame_view import composite_draw_plane_result
from rich_terminal.retained_scene import ControlState, ObjectBounds
from rich_terminal.retained_view import (
    PaneDraw, RetainedDrawPlane, RetainedRegionDraw, TabDraw, TabSetDraw,
)
from session_viewer import compose_terminal_frame_changes, compose_terminal_frame_result


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 16)


def _plane(*, focused=False, title="Pane", origin=(1, 1), controls=False,
           header=True, chrome_clip=None):
    x, y = origin
    top = 1 if header else 0
    content = ObjectBounds(1, top, 18, 9 - top)
    pane = PaneDraw(1, 0, ObjectBounds(0, 0, 20, 10), 2, content, title, focused)
    cx, cy, cc, cr = chrome_clip or (x, y, 20, 10)
    chrome = RetainedRegionDraw(1, 1, 1, x, y, 20, 10,
                                cx, cy, cc, cr, 0, True, (pane,))
    state = ControlState.VISIBLE | ControlState.ENABLED
    draws = () if not controls else (TabSetDraw(
        100, state, 0, 0, ObjectBounds(0, 0, 18, 1),
        (TabDraw(101, state | ControlState.SELECTED, 0, "Keep", ""),)),)
    inner = RetainedRegionDraw(1, 1, 2, x + 1, y + top, 18, 9 - top,
                               x + 1, y + top, 18, 9 - top, 1, True, draws)
    return RetainedDrawPlane(True, True, (chrome, inner))


def _paint(plane, font, appearance, surface=None):
    surface = pygame.Surface((240, 180)) if surface is None else surface
    return composite_draw_plane_result(pygame, surface, plane, font, 8, 12,
                                       appearance=appearance)


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_pane_replaces_only_declared_chrome_and_restores_caller_clip(font, appearance):
    surface = pygame.Surface((240, 180))
    # Occupied CELL content is deliberately noisy, making even one changed
    # content pixel observable. No independent content repaint can hide it.
    for y in range(180):
        for x in range(240):
            surface.set_at((x, y), ((x * 13) % 256, (y * 17) % 256, 123))
    prior = surface.copy()
    clip = pygame.Rect(9, 14, 154, 115)
    surface.set_clip(clip)
    result = _paint(_plane(focused=True), font, appearance, surface)
    outer, hole = pygame.Rect(8, 12, 160, 120), pygame.Rect(16, 24, 144, 96)
    changed = 0
    for y in range(180):
        for x in range(240):
            difference = surface.get_at((x, y)) != prior.get_at((x, y))
            if difference:
                changed += 1
                assert clip.collidepoint(x, y) and outer.collidepoint(x, y)
                assert not hole.collidepoint(x, y)
    assert changed
    assert not result.hit_targets
    assert surface.get_clip() == clip


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_pane_focus_and_title_do_not_change_content_hit_targets(font, appearance):
    unfocused = _paint(_plane(controls=True), font, appearance)
    focused = _paint(_plane(controls=True, focused=True, title="Other"), font, appearance)
    plain_content = _paint(RetainedDrawPlane(True, True, (_plane(controls=True).regions[1],)),
                           font, appearance)
    assert focused.hit_targets == unfocused.hit_targets == plain_content.hit_targets
    assert focused.hit_targets
    for target in focused.hit_targets:
        x, y = target.rect.left, target.rect.top
        assert focused.hit_test(x, y) == plain_content.hit_test(x, y)


def test_title_without_reserved_header_is_metadata_only(font):
    empty = _paint(_plane(header=False, title=""), font, FLOWING_APPEARANCE)
    named = _paint(_plane(header=False, title="A title must not cover the first menu row"),
                   font, FLOWING_APPEARANCE)
    assert pygame.image.tobytes(empty.surface, "RGBA") == pygame.image.tobytes(named.surface, "RGBA")


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_pane_focus_title_move_and_drop_partial_repaint_matches_full(font, appearance):
    terminal = VirtualTerminal(cols=30, rows=15)
    terminal.write(b"Underlying CELL text stays authoritative\r\nSecond row")
    previous = None
    glyph_cache = {}
    for plane in (
        _plane(controls=True),
        _plane(controls=True, focused=True, title="Focused"),
        _plane(controls=True, focused=True, title="A long clipped pane title"),
        _plane(controls=True, origin=(3, 2)),
        RetainedDrawPlane(True, True, ()),
    ):
        incremental = compose_terminal_frame_changes(
            pygame, terminal, font, 8, 12, retained_plane=plane, show_cursor=False,
            previous=previous, appearance=appearance, glyph_cache=glyph_cache,
        )
        complete = compose_terminal_frame_result(
            pygame, terminal, font, 8, 12, retained_plane=plane, show_cursor=False,
            appearance=appearance,
        )
        assert pygame.image.tobytes(incremental.surface, "RGBA") == pygame.image.tobytes(complete.surface, "RGBA")
        assert incremental.hit_entries == complete.hit_entries
        previous = incremental


def test_extreme_pane_coordinates_stay_bounded_and_leave_content_hole_untouched(font):
    pane = PaneDraw(1, 0, ObjectBounds(-(1 << 31), 0, (1 << 32) - 1, 3), 2,
                    ObjectBounds(0, 0, (1 << 32) - 1, 2), "Metadata only")
    chrome = RetainedRegionDraw(1, 1, 1, 0, 0, 30, 15, 0, 0, 0, 0, 0, False, (pane,))
    inner = RetainedRegionDraw(1, 1, 2, 0, 0, 30, 2, 0, 0, 30, 2, 1, True, ())
    surface = pygame.Surface((240, 180))
    surface.fill((191, 29, 53))
    _paint(RetainedDrawPlane(True, True, (chrome, inner)), font, FLOWING_APPEARANCE, surface)
    assert surface.get_at((100, 12))[:3] == (191, 29, 53)
    assert surface.get_at((100, 30))[:3] != (191, 29, 53)
    assert surface.get_at((100, 50))[:3] == (191, 29, 53)
