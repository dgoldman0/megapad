"""Seconds-scale off-screen coverage for semantic Pygame controls."""

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
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
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
    """Deterministic terminal monospace font with scalar-only glyph surfaces."""

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


def test_text_area_paints_exact_viewport_selection_and_persistent_caret():
    pygame = pytest.importorskip("pygame")
    content = SemanticTextContent(
        content_revision=1,
        rows=3,
        columns=5,
        viewport_row=1,
        viewport_column=1,
        viewport_rows=2,
        viewport_columns=4,
        flags=SemanticContentFlag(0),
        primary_key=2,
        primary_offset=4,
        anchor_key=2,
        anchor_offset=2,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                1,
                5,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "OFF",
            ),
            SemanticTextItem(
                2,
                1,
                0,
                1,
                5,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "ABCDE",
            ),
        ),
    )
    area = TextAreaDraw(
        30,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, (UINT32_MAX * 3) // 4, UINT32_MAX),
        content,
    )
    plane = _plane(_region(area, cols=8, rows=4))

    surface, result = _render(pygame, plane, size=(80, 40))

    assert result.hit_targets == ()
    # The semantic root is an opaque semantic representation over the CELL
    # fallback; pixels outside that root retain the caller-owned CELL raster.
    assert tuple(surface.get_at((70, 20)))[:3] == (184, 190, 201)
    assert tuple(surface.get_at((10, 30)))[:3] != (184, 190, 201)
    # Offsets 2..4 select two exact scalar slots in the first visible row.
    assert tuple(surface.get_at((20, 3)))[:3] != tuple(
        surface.get_at((5, 3))
    )[:3]
    # The primary position is rendered persistently at viewport-relative edge 3.
    assert tuple(surface.get_at((45, 5)))[:3] == (78, 139, 246)


def test_text_area_clip_does_not_reflow_or_relocate_an_offscreen_endpoint():
    pygame = pytest.importorskip("pygame")

    class RecordingGlyphFont(_GlyphFont):
        def __init__(self, pygame_module):
            super().__init__(pygame_module)
            self.rendered = []

        def render(self, text, antialias, color):
            self.rendered.append(text)
            return super().render(text, antialias, color)

    content = SemanticTextContent(
        content_revision=2,
        rows=2,
        columns=4,
        viewport_row=1,
        viewport_column=0,
        viewport_rows=1,
        viewport_columns=4,
        flags=SemanticContentFlag(0),
        primary_key=2,
        primary_offset=2,
        anchor_key=1,
        anchor_offset=1,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                1,
                4,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "ZZZZ",
            ),
            SemanticTextItem(
                2,
                1,
                0,
                1,
                4,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "ABCD",
            ),
        ),
    )
    area = TextAreaDraw(
        31,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        content,
    )
    surface = pygame.Surface((40, 10))
    surface.fill((184, 190, 201))
    surface.set_clip(pygame.Rect(10, 0, 20, 10))
    font = RecordingGlyphFont(pygame)

    result = composite_draw_plane_result(
        pygame,
        surface,
        _plane(_region(area, cols=4, rows=1)),
        font,
        10,
        10,
    )

    assert result.hit_targets == ()
    assert font.rendered == ["B", "C"]
    assert tuple(surface.get_at((5, 5)))[:3] == (184, 190, 201)
    assert tuple(surface.get_at((20, 5)))[:3] == (78, 139, 246)


def test_text_grid_maps_spans_and_states_without_inventing_item_hits():
    pygame = pytest.importorskip("pygame")
    content = SemanticTextContent(
        content_revision=4,
        rows=4,
        columns=4,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=4,
        viewport_columns=4,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=4,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                1,
                4,
                SemanticTextRole.COLUMN_HEADER,
                SemanticTextState(0),
                "Week",
            ),
            SemanticTextItem(
                2,
                1,
                0,
                1,
                2,
                SemanticTextRole.CONTENT,
                SemanticTextState.CURRENT,
                "Today",
            ),
            SemanticTextItem(
                3,
                1,
                2,
                1,
                2,
                SemanticTextRole.CONTENT,
                SemanticTextState.UNAVAILABLE,
                "Busy",
            ),
            SemanticTextItem(
                4,
                2,
                1,
                1,
                2,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "Selected",
            ),
        ),
    )
    grid = TextGridDraw(
        40,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        content,
    )

    surface, result = _render(
        pygame,
        _plane(_region(grid, cols=12, rows=8)),
        size=(120, 80),
    )

    assert result.hit_targets == ()
    header = tuple(surface.get_at((5, 3)))[:3]
    unavailable = tuple(surface.get_at((65, 23)))[:3]
    primary = tuple(surface.get_at((35, 43)))[:3]
    blank = tuple(surface.get_at((5, 65)))[:3]
    assert len({header, unavailable, primary, blank}) == 4
    assert tuple(surface.get_at((1, 21)))[:3] == (78, 139, 246)


def test_tabset_owns_layout_and_emits_only_enabled_tab_targets():
    pygame = pytest.importorskip("pygame")
    tabset = TabSetDraw(
        50,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        (
            TabDraw(51, VISIBLE | ENABLED | ControlState.SELECTED, 0, "Pad", ""),
            TabDraw(52, VISIBLE | ENABLED, 1, "Daybook", "D"),
            TabDraw(53, VISIBLE, 2, "Disabled", ""),
        ),
    )
    # The labels do not fit this root naturally, so the renderer's generic
    # overflow policy gives every semantic tab one deterministic partition.
    plane = _plane(_region(tabset, cols=12, rows=4))
    hover_identity = ControlIdentity(11, 7, 52)

    idle_surface, idle = _render(pygame, plane, size=(120, 40))
    hover_surface, hovered = _render(
        pygame,
        plane,
        hovered=hover_identity,
        size=(120, 40),
    )

    assert [target.identity.control_id for target in idle.hit_targets] == [51, 52]
    assert [target.kind for target in idle.hit_targets] == [
        ControlKind.TAB,
        ControlKind.TAB,
    ]
    assert idle.hit_targets == hovered.hit_targets
    daybook = _target(idle, 52).rect
    point = (daybook.left + 2, (daybook.top + daybook.bottom) // 2)
    assert idle.hit_test(*point).identity == hover_identity
    assert tuple(idle_surface.get_at(point))[:3] != tuple(
        hover_surface.get_at(point)
    )[:3]


def test_later_tabset_wins_hit_testing_in_painter_order():
    pygame = pytest.importorskip("pygame")
    bounds = ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX)
    lower = TabSetDraw(
        70,
        VISIBLE | ENABLED,
        0,
        0,
        bounds,
        (TabDraw(71, VISIBLE | ENABLED, 0, "Lower", ""),),
    )
    upper = TabSetDraw(
        80,
        VISIBLE | ENABLED,
        0,
        1,
        bounds,
        (TabDraw(81, VISIBLE | ENABLED, 0, "Upper", ""),),
    )

    _, result = _render(
        pygame,
        _plane(_region(lower, upper, cols=12, rows=4)),
        size=(120, 40),
    )

    lower_target = _target(result, 71)
    point = (
        (lower_target.rect.left + lower_target.rect.right) // 2,
        (lower_target.rect.top + lower_target.rect.bottom) // 2,
    )
    assert result.hit_test(*point).identity.control_id == 81


def test_grid_rasterizes_only_visible_scalars_without_a_logical_cell_matrix():
    pygame = pytest.importorskip("pygame")

    class CountingFont(_ControlFont):
        def __init__(self, pygame_module):
            super().__init__(pygame_module)
            self.render_calls = []

        def render(self, text, antialias, color):
            self.render_calls.append(text)
            return super().render(text, antialias, color)

    text = "X" * 10_000
    content = SemanticTextContent(
        content_revision=1,
        rows=1,
        columns=10_000,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=1,
        viewport_columns=10_000,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=0,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                1,
                10_000,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                text,
            ),
        ),
    )
    grid = TextGridDraw(
        60,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        content,
    )
    font = CountingFont(pygame)
    surface = pygame.Surface((40, 10))

    result = composite_draw_plane_result(
        pygame,
        surface,
        _plane(_region(grid, cols=4, rows=1)),
        _GlyphFont(pygame),
        10,
        10,
        control_font=font,
    )

    assert result.hit_targets == ()
    assert 0 < len(font.render_calls) < 10
    assert all(len(call) == 1 for call in font.render_calls)


def test_grid_clips_extreme_u32_spans_before_constructing_pygame_rects():
    pygame = pytest.importorskip("pygame")
    content = SemanticTextContent(
        content_revision=6,
        rows=UINT32_MAX,
        columns=UINT32_MAX,
        viewport_row=UINT32_MAX - 1,
        viewport_column=UINT32_MAX - 1,
        viewport_rows=1,
        viewport_columns=1,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=0,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                1,
                0,
                0,
                UINT32_MAX,
                UINT32_MAX,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "X",
            ),
        ),
    )
    grid = TextGridDraw(
        61,
        VISIBLE | ENABLED,
        0,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        content,
    )
    surface = pygame.Surface((40, 10))
    surface.fill((184, 190, 201))
    # Neither the root edge nor any true item edge crosses this interior clip.
    # Its left edge must therefore remain ordinary item fill, not a border
    # invented by saturating the item's extreme logical origin to the clip.
    surface.set_clip(pygame.Rect(10, 1, 20, 8))

    result = composite_draw_plane_result(
        pygame,
        surface,
        _plane(_region(grid, cols=4, rows=1)),
        _GlyphFont(pygame),
        10,
        10,
        control_font=_ControlFont(pygame),
    )

    assert result.hit_targets == ()
    assert tuple(surface.get_at((10, 5)))[:3] == tuple(
        surface.get_at((20, 5))
    )[:3]
    assert tuple(surface.get_at((10, 5)))[:3] != (184, 190, 201)
