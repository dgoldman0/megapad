"""Seconds-scale off-screen coverage for the semantic pygame menu path."""

from __future__ import annotations

from dataclasses import FrozenInstanceError

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.pygame_view import (
    ControlIdentity,
    PixelRect,
    composite_draw_plane,
    composite_draw_plane_result,
)
from rich_terminal.retained_scene import (
    ControlKind,
    ControlState,
    ObjectBounds,
    RGBA,
)
from rich_terminal.retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    TextAreaDraw,
)
from rich_terminal.semantic_content import (
    SemanticContentFlag,
    SemanticTextContent,
    SemanticTextItem,
    SemanticTextRole,
    SemanticTextState,
)


VISIBLE = ControlState.VISIBLE
ENABLED = ControlState.ENABLED


class _ControlFont:
    """A deterministic sans-like rectangle font for layout and pixel oracles."""

    def __init__(self, pygame_module, *, character_width=6, height=12):
        self.pygame = pygame_module
        self.character_width = character_width
        self.height = height

    def get_linesize(self):
        return self.height

    def size(self, text):
        return len(text) * self.character_width, self.height

    def render(self, text, antialias, color):
        assert antialias
        glyph = self.pygame.Surface(
            (max(1, len(text) * self.character_width), self.height),
            flags=self.pygame.SRCALPHA,
        )
        glyph.fill((*color, 255))
        return glyph


class _GlyphFont:
    """Legacy glyph-only font that proves semantic text uses control_font."""

    def __init__(self, pygame_module):
        self.pygame = pygame_module

    def render(self, text, antialias, color):
        assert antialias and len(text) == 1
        glyph = self.pygame.Surface((1, 1), flags=self.pygame.SRCALPHA)
        glyph.fill((*color, 255))
        return glyph


def _region(*draws, cols=30, rows=20, clipped=True):
    return RetainedRegionDraw(11, 7, 1, 0, 0, cols, rows, 0, clipped, draws)


def _plane(*regions):
    return RetainedDrawPlane(True, True, regions)


def _item(control_id, order, label, shortcut="", *, state=VISIBLE | ENABLED):
    return MenuItemDraw(control_id, state, order, label, shortcut)


def _menu_bar(*, z_order=0, root_id=1, menu_id=2, bounds=None):
    if bounds is None:
        bounds = ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX // 5)
    file_menu = MenuDraw(
        menu_id,
        VISIBLE | ENABLED | ControlState.OPEN | ControlState.SELECTED,
        0,
        "File",
        (
            _item(
                menu_id + 1,
                0,
                "New document",
                "Ctrl+N",
                state=(
                    VISIBLE
                    | ENABLED
                    | ControlState.SELECTED
                    | ControlState.CHECKED
                ),
            ),
            _item(menu_id + 2, 1, "Unavailable", state=VISIBLE),
            MenuSeparatorDraw(menu_id + 3, VISIBLE, 2),
            _item(menu_id + 4, 3, "Close", "Ctrl+W"),
        ),
    )
    return MenuBarDraw(
        root_id,
        VISIBLE | ENABLED,
        0,
        z_order,
        bounds,
        (
            file_menu,
            MenuDraw(menu_id + 5, VISIBLE | ENABLED, 1, "Edit", ()),
            MenuDraw(menu_id + 6, VISIBLE, 2, "Help", ()),
        ),
    )


def _render(pygame, plane, *, hovered=None, pressed=None, size=(300, 200)):
    surface = pygame.Surface(size)
    surface.fill((184, 190, 201))
    result = composite_draw_plane_result(
        pygame,
        surface,
        plane,
        _GlyphFont(pygame),
        10,
        10,
        control_font=_ControlFont(pygame),
        hovered=hovered,
        pressed=pressed,
    )
    return surface, result


def _target(result, control_id):
    return next(
        target
        for target in result.hit_targets
        if target.identity.control_id == control_id
    )


def test_open_menu_uses_modern_layered_pixels_and_enabled_hit_targets_only():
    pygame = pytest.importorskip("pygame")
    bar = _menu_bar()
    plane = _plane(_region(bar))
    edit_identity = ControlIdentity(11, 7, 7)

    idle_surface, idle = _render(pygame, plane)
    hover_surface, hovered = _render(pygame, plane, hovered=edit_identity)
    press_surface, pressed = _render(pygame, plane, pressed=edit_identity)

    # Root bars, disabled menus/items, and separators are deliberately absent.
    assert [target.identity.control_id for target in idle.hit_targets] == [
        2,
        7,
        3,
        6,
    ]
    assert [target.kind for target in idle.hit_targets] == [
        ControlKind.MENU,
        ControlKind.MENU,
        ControlKind.MENU_ITEM,
        ControlKind.MENU_ITEM,
    ]
    assert idle.hit_targets == hovered.hit_targets == pressed.hit_targets
    assert isinstance(idle.hit_targets, tuple)
    assert all(isinstance(target.rect, PixelRect) for target in idle.hit_targets)
    assert all(not isinstance(target.rect, pygame.Rect) for target in idle.hit_targets)
    with pytest.raises(FrozenInstanceError):
        idle.hit_targets[0].rect.left = 99

    # The open bar and popup form dark layers over the caller's CELL surface.
    assert tuple(idle_surface.get_at((150, 20)))[:3] != (184, 190, 201)
    assert tuple(idle_surface.get_at((10, 50)))[:3] != (184, 190, 201)

    # Transient state is renderer-local and visibly differentiates each phase.
    edit = _target(idle, 7).rect
    state_pixel = (edit.left + 2, (edit.top + edit.bottom) // 2)
    assert len(
        {
            tuple(idle_surface.get_at(state_pixel))[:3],
            tuple(hover_surface.get_at(state_pixel))[:3],
            tuple(press_surface.get_at(state_pixel))[:3],
        }
    ) == 3

    # A checked selected row contains an opaque, code-drawn blue checkmark.
    selected = _target(idle, 3).rect
    check_pixels = {
        tuple(idle_surface.get_at((x, y)))[:3]
        for x in range(selected.left, min(selected.right, selected.left + 22))
        for y in range(selected.top, selected.bottom)
    }
    assert (78, 139, 246) in check_pixels


def test_hit_geometry_is_deterministic_and_reverse_painter_order_wins():
    pygame = pytest.importorskip("pygame")
    lower = _menu_bar(z_order=0, root_id=1, menu_id=2)
    upper = _menu_bar(z_order=1, root_id=20, menu_id=21)
    plane = _plane(_region(lower, upper))

    _, first = _render(pygame, plane)
    _, second = _render(pygame, plane)

    assert first.hit_targets == second.hit_targets
    lower_file = _target(first, 2)
    point = (
        (lower_file.rect.left + lower_file.rect.right) // 2,
        (lower_file.rect.top + lower_file.rect.bottom) // 2,
    )
    assert first.hit_test(*point).identity.control_id == 21


def test_popup_flips_above_and_clips_to_the_renderer_viewport():
    pygame = pytest.importorskip("pygame")
    bounds = ObjectBounds(
        0,
        (UINT32_MAX * 3) // 4,
        UINT32_MAX,
        UINT32_MAX,
    )
    long_menu = MenuDraw(
        2,
        VISIBLE | ENABLED | ControlState.OPEN,
        0,
        "File",
        (
            _item(3, 0, "A very long renderer-owned menu label", "Ctrl+Shift+N"),
            _item(4, 1, "Second row"),
            _item(5, 2, "Third row"),
        ),
    )
    bar = MenuBarDraw(
        1,
        VISIBLE | ENABLED,
        0,
        0,
        bounds,
        (long_menu,),
    )
    plane = _plane(_region(bar, cols=12, rows=9, clipped=True))

    _, result = _render(pygame, plane, size=(120, 90))

    menu = _target(result, 2).rect
    items = [_target(result, control_id).rect for control_id in (3, 4, 5)]
    assert max(item.bottom for item in items) < menu.top
    assert any(item.right == 120 for item in items)
    assert all(
        0 <= edge <= limit
        for item in items
        for edge, limit in (
            (item.left, 120),
            (item.right, 120),
            (item.top, 90),
            (item.bottom, 90),
        )
    )


def test_legacy_glyph_composition_keeps_surface_return_and_empty_hit_map():
    pygame = pytest.importorskip("pygame")
    glyph = GlyphRunDraw(
        1,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        RGBA(230, 240, 250, 255),
        RGBA(10, 20, 30, 255),
        0,
        "AB",
    )
    plane = _plane(_region(glyph, cols=2, rows=1))
    font = _GlyphFont(pygame)
    legacy_surface = pygame.Surface((20, 10))
    result_surface = composite_draw_plane(
        pygame,
        legacy_surface,
        plane,
        font,
        10,
        10,
    )
    result_surface_2 = pygame.Surface((20, 10))
    result = composite_draw_plane_result(
        pygame,
        result_surface_2,
        plane,
        font,
        10,
        10,
    )

    assert result_surface is legacy_surface
    assert result.surface is result_surface_2
    assert result.hit_targets == ()
    assert tuple(result_surface_2.get_at((5, 5)))[:3] == (10, 20, 30)


def test_collection_draw_fails_before_the_current_pygame_sink_can_present_it():
    pygame = pytest.importorskip("pygame")
    content = SemanticTextContent(
        content_revision=1,
        rows=1,
        columns=8,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=1,
        viewport_columns=8,
        flags=SemanticContentFlag(0),
        primary_key=1,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                1,
                8,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "Pad",
            ),
        ),
    )
    area = TextAreaDraw(
        30,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        content,
    )
    plane = _plane(_region(area, cols=8, rows=1))
    surface = pygame.Surface((80, 10))
    surface.fill((23, 29, 37))

    with pytest.raises(TypeError, match="unsupported retained draw value"):
        composite_draw_plane_result(
            pygame,
            surface,
            plane,
            _GlyphFont(pygame),
            10,
            10,
        )
    assert tuple(surface.get_at((5, 5)))[:3] == (23, 29, 37)
