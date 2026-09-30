"""Typed grid styling changes paint inside existing cell and hit rectangles."""

from dataclasses import replace

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal
from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.pygame_view import TextHitTarget, TextPosition, composite_draw_plane_result
from rich_terminal.retained_scene import ControlState, ObjectBounds
from rich_terminal.retained_view import RetainedDrawPlane, RetainedRegionDraw, TextGridDraw
from rich_terminal.semantic_content import (
    SemanticContentFlag, SemanticTextContent, SemanticTextItem, SemanticTextRole,
    SemanticTextState,
)
from session_viewer import compose_terminal_frame_changes, compose_terminal_frame_result

ACTIVE = ControlState.VISIBLE | ControlState.ENABLED
APPEARANCES = (REFERENCE_APPEARANCE, FLOWING_APPEARANCE)
ROLES = (SemanticTextRole.CONTENT, SemanticTextRole.NUMBER,
         SemanticTextRole.FORMULA, SemanticTextRole.ERROR)
COLORS = ((239, 243, 249), (105, 201, 220), (116, 214, 159), (244, 139, 139))
DISABLED = (103, 113, 128)
SENTINEL = (63, 22, 97)


class RectangleFont:
    """Four-pixel glyph slots make alignment and foreground exact pixel facts."""

    def __init__(self):
        self.rendered = []

    def size(self, text):
        return len(text) * 4, 4

    def get_linesize(self):
        return 4

    def render(self, text, antialias, color):
        assert antialias
        self.rendered.append((text, tuple(color)))
        glyph = pygame.Surface((max(1, len(text) * 4), 4), flags=pygame.SRCALPHA)
        glyph.fill((*color, 255))
        return glyph


@pytest.fixture
def font():
    return RectangleFont()


def _grid(*, roles=ROLES, text="12", primary_key=0, unavailable=(), enabled=True,
          revision=1, bounds=ObjectBounds(1, 1, 16, 2), direction=SemanticContentFlag(0)):
    items = tuple(SemanticTextItem(
        index + 1, 0, index, 1, 1, role,
        SemanticTextState.UNAVAILABLE if index + 1 in unavailable else SemanticTextState(0),
        text,
    ) for index, role in enumerate(roles))
    content = SemanticTextContent(revision, 2, len(roles), 0, 0, 2, len(roles),
                                  direction, primary_key, 0, 0, 0, items)
    return TextGridDraw(11, ACTIVE if enabled else ControlState.VISIBLE,
                        0, 0, bounds, content)


def _plane(grid):
    region = RetainedRegionDraw(1, 1, 1, 0, 0, 20, 6, 0, 0, 20, 6,
                                0, True, () if grid is None else (grid,))
    return RetainedDrawPlane(True, True, (region,))


def _paint(grid, font, appearance, surface=None):
    if surface is None:
        surface = pygame.Surface((200, 60))
        surface.fill(SENTINEL)
    return composite_draw_plane_result(pygame, surface, _plane(grid), font, 10, 10,
                                       control_font=font, appearance=appearance)


def _target(result):
    targets = [entry for entry in result.hit_entries if isinstance(entry, TextHitTarget)]
    assert len(targets) == 1
    return targets[0]


def _pixels(surface, rect, color):
    return {(x, y) for y in range(rect.top, rect.bottom)
            for x in range(rect.left, rect.right) if surface.get_at((x, y))[:3] == color}


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_identical_text_uses_only_published_role_for_alignment_and_color(font, appearance):
    result = _paint(_grid(), font, appearance)
    target = _target(result)
    assert (target.rect.left, target.rect.top, target.rect.width, target.rect.height) == (10, 10, 160, 20)
    for index, (role, color) in enumerate(zip(ROLES, COLORS)):
        left = 10 + index * 40
        cell = pygame.Rect(left, 10, 40, 10)
        ink = _pixels(result.surface, cell, color)
        # Identical "12" remains plain left-aligned text for CONTENT and
        # ERROR, while only NUMBER and FORMULA choose the right-hand edge.
        expected_left = left + (30 if role in (SemanticTextRole.NUMBER, SemanticTextRole.FORMULA) else 2)
        assert ink == {(x, y) for x in range(expected_left, expected_left + 8) for y in range(13, 17)}
        assert target.position_at(left + 1, 15) == TextPosition(index + 1, 0)
        assert target.position_at(left + 39, 15) == TextPosition(index + 1, 0)
    assert target.position_at(30, 25) is None
    assert result.surface.get_at((5, 15))[:3] == SENTINEL


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_non_numeric_published_result_text_is_not_parsed_or_reclassified(font, appearance):
    # A numeric role is metadata supplied by the guest; its value string
    # need not parse as a host float or formula expression.
    result = _paint(_grid(text="zz"), font, appearance)
    for index, color in enumerate(COLORS):
        left = 10 + index * 40
        expected_x = left + (30 if index in (1, 2) else 2)
        assert result.surface.get_at((expected_x, 15))[:3] == color


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_headers_and_unavailable_values_keep_same_slots_without_selectable_items(font, appearance):
    grid = _grid(roles=(SemanticTextRole.ROW_HEADER, SemanticTextRole.COLUMN_HEADER,
                        SemanticTextRole.NUMBER, SemanticTextRole.ERROR), unavailable=(3,))
    result = _paint(grid, font, appearance)
    target = _target(result)
    assert [target.position_at(30 + 40 * index, 15) for index in range(4)] == [
        None, None, None, TextPosition(4, 0),
    ]
    assert result.surface.get_at((120, 15))[:3] == DISABLED
    assert result.surface.get_at((132, 15))[:3] == COLORS[3]


@pytest.mark.parametrize("appearance", APPEARANCES)
@pytest.mark.parametrize("role", ROLES[1:])
def test_disabled_and_unavailable_foregrounds_override_typed_roles(font, appearance, role):
    roles = (role,)
    for grid in (_grid(roles=roles, enabled=False), _grid(roles=roles, unavailable=(1,))):
        result = _paint(grid, font, appearance)
        x = 160 if role in (SemanticTextRole.NUMBER, SemanticTextRole.FORMULA) else 12
        assert result.surface.get_at((x, 15))[:3] == DISABLED
        if grid.state & ControlState.ENABLED:
            assert _target(result).position_at(50, 15) is None
        else:
            assert not any(isinstance(entry, TextHitTarget) for entry in result.hit_entries)


@pytest.mark.parametrize("role,color", tuple(zip(ROLES[1:], COLORS[1:])))
def test_flowing_selected_cell_uses_contrasting_foreground_without_moving_hit_geometry(font, role, color):
    ordinary = _paint(_grid(roles=(role,)), font, FLOWING_APPEARANCE)
    selected = _paint(_grid(roles=(role,), primary_key=1), font, FLOWING_APPEARANCE)
    x = 160 if role in (SemanticTextRole.NUMBER, SemanticTextRole.FORMULA) else 12
    assert ordinary.surface.get_at((x, 15))[:3] == color
    assert selected.surface.get_at((x, 15))[:3] == FLOWING_APPEARANCE.surface
    assert _target(selected) == _target(ordinary)


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_content_keeps_bidi_alignment_while_error_has_explicit_left_alignment(font, appearance):
    result = _paint(_grid(roles=(SemanticTextRole.CONTENT, SemanticTextRole.ERROR),
                          text="אב", direction=SemanticContentFlag.DIRECTION_RTL), font, appearance)
    left = _pixels(result.surface, pygame.Rect(10, 10, 80, 10), COLORS[0])
    right = _pixels(result.surface, pygame.Rect(90, 10, 80, 10), COLORS[3])
    assert min(x for x, _ in left) == 80
    assert min(x for x, _ in right) == 92
    # Grid input names whole cells, so paragraph direction must not mirror
    # the guest's published column positions.
    assert _target(result).position_at(15, 15) == TextPosition(1, 0)
    assert _target(result).position_at(95, 15) == TextPosition(2, 0)


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_long_text_and_caller_clip_cannot_modify_adjacent_cells_or_expand_clip(font, appearance):
    grid = _grid()
    baseline = _paint(grid, font, appearance)
    for index in range(4):
        items = tuple(replace(item, text="9" * 10000) if i == index else item
                      for i, item in enumerate(grid.content.items))
        changed = replace(grid, content=replace(grid.content, items=items))
        font.rendered.clear()
        painted = _paint(changed, font, appearance)
        # At most ten scalar glyphs fit in the changed cell. The other
        # three values contribute six glyphs; none is rendered as one
        # unbounded string surface.
        assert len(font.rendered) <= 16
        assert all(len(text) == 1 for text, _ in font.rendered)
        allowed = pygame.Rect(10 + index * 40, 10, 40, 10)
        for y in range(60):
            for x in range(200):
                if painted.surface.get_at((x, y)) != baseline.surface.get_at((x, y)):
                    assert allowed.collidepoint(x, y)
        assert _target(painted) == _target(baseline)
    surface = pygame.Surface((200, 60))
    surface.fill(SENTINEL)
    clip = pygame.Rect(53, 12, 8, 5)
    surface.set_clip(clip)
    _paint(changed, font, appearance, surface)
    assert surface.get_clip() == clip
    for y in range(60):
        for x in range(200):
            if not clip.collidepoint(x, y):
                assert surface.get_at((x, y))[:3] == SENTINEL


@pytest.mark.parametrize("appearance", APPEARANCES)
def test_role_selection_value_and_geometry_changes_match_full_repaint(font, appearance):
    terminal = VirtualTerminal(cols=20, rows=6)
    terminal.write(b"Underlying CELL grid fallback")
    previous = None
    glyph_cache = {}
    frames = (
        _grid(),
        _grid(roles=(SemanticTextRole.NUMBER, SemanticTextRole.CONTENT,
                     SemanticTextRole.ERROR, SemanticTextRole.FORMULA), revision=2),
        _grid(primary_key=2, revision=3),
        _grid(text="999999999", revision=4),
        _grid(unavailable=(2,), revision=5),
        _grid(enabled=False, revision=6),
        _grid(bounds=ObjectBounds(-2, 3, 16, 2), revision=7),
        None,
    )
    for grid in frames:
        incremental = compose_terminal_frame_changes(
            pygame, terminal, font, 10, 10, retained_plane=_plane(grid), show_cursor=False,
            previous=previous, appearance=appearance, glyph_cache=glyph_cache, control_font=font,
        )
        full = compose_terminal_frame_result(
            pygame, terminal, font, 10, 10, retained_plane=_plane(grid),
            show_cursor=False, appearance=appearance, control_font=font,
        )
        assert pygame.image.tobytes(incremental.surface, "RGBA") == pygame.image.tobytes(full.surface, "RGBA")
        assert incremental.hit_entries == full.hit_entries
        previous = incremental
