"""Host material geometry, clipping and bounded allocation contracts."""

from dataclasses import FrozenInstanceError
from types import SimpleNamespace

import pytest

from rich_terminal import appearance
from rich_terminal.appearance import (
    Appearance, FLOWING_APPEARANCE, REFERENCE_APPEARANCE, get_appearance,
    paint_channel,
)


def _rect(left, top, width, height):
    return SimpleNamespace(left=left, top=top, width=width, height=height,
                           right=left + width, bottom=top + height)


def test_appearance_is_immutable_and_names_are_explicit():
    assert get_appearance("reference") is REFERENCE_APPEARANCE
    assert get_appearance("flowing") is FLOWING_APPEARANCE
    assert hash(FLOWING_APPEARANCE) == hash(get_appearance("flowing"))
    with pytest.raises(FrozenInstanceError):
        FLOWING_APPEARANCE.name = "other"
    with pytest.raises(ValueError, match="unknown appearance"):
        get_appearance("unknown")
    with pytest.raises(ValueError, match="immutable RGB tuple"):
        Appearance("custom", accent=[1, 2, 3])


@pytest.mark.parametrize("radius", [0, 3, 12, 32])
@pytest.mark.parametrize("size", [(2, 2), (3, 4), (23, 15), (80, 56)])
def test_channel_partial_paints_equal_one_complete_paint(radius, size):
    pygame = pytest.importorskip("pygame")
    complete = pygame.Surface((100, 70))
    partial = pygame.Surface((100, 70))
    complete.fill((3, 5, 7))
    partial.fill((3, 5, 7))
    rect = _rect(7, 5, *size)
    kwargs = dict(radius=radius, border=(73, 94, 103), highlight=(114, 140, 148))
    paint_channel(pygame, complete, rect, (12, 20, 25), **kwargs)
    # Irregular damage strips cut through both curved corners and straight
    # edges.  Each must sample the same atlas coordinates as the full paint.
    for left, right in zip((0, 9, 21, 47, 100), (9, 21, 47, 100)):
        partial.set_clip(pygame.Rect(left, 0, right - left, 70))
        paint_channel(pygame, partial, rect, (12, 20, 25), **kwargs)
    assert pygame.image.tobytes(partial, "RGBA") == pygame.image.tobytes(complete, "RGBA")


def test_channel_has_antialiased_edges_and_stays_inside_its_bounds():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((50, 40), flags=pygame.SRCALPHA)
    paint_channel(pygame, surface, _rect(5, 5, 40, 30), (80, 180, 160), radius=14)
    assert any(0 < surface.get_at((x, y)).a < 255 for y in range(5, 15) for x in range(5, 15))
    assert all(surface.get_at((x, y)).a == 0 for y in range(40) for x in range(50)
               if x < 5 or x >= 45 or y < 5 or y >= 35)


@pytest.mark.parametrize("fill", [(12, 20, 25, 255), (80, 180, 160, 160)])
def test_atlas_flat_centre_and_edge_strips_preserve_exact_rgba(fill):
    pygame = pytest.importorskip("pygame")
    radius = 12
    atlas = appearance._channel_atlas(pygame, radius, fill, None, None)
    centre = radius + 1
    # These patches are stretched independently.  Attenuation in a strip but
    # not its neighbouring centre produces a visible rectangular seam even
    # though both should have the same flat material colour and opacity.
    points = ((centre, centre), (centre, 4), (4, centre),
              (centre, atlas.get_height() - 5), (atlas.get_width() - 5, centre))
    assert all(tuple(atlas.get_at(point)) == fill for point in points)


def test_extreme_logical_geometry_only_allocates_visible_strips():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((40, 24))
    surface.fill((1, 2, 3))
    surface.set_clip(pygame.Rect(3, 4, 17, 13))
    paint_channel(pygame, surface, _rect(-(1 << 60), -(1 << 60), 1 << 62, 1 << 62),
                  (12, 20, 25), radius=1 << 50, border=(70, 90, 100))
    assert tuple(surface.get_at((10, 10)))[:3] == (12, 20, 25)
    assert tuple(surface.get_at((0, 0)))[:3] == (1, 2, 3)
    assert surface.get_clip() == pygame.Rect(3, 4, 17, 13)


def test_atlas_cache_has_byte_and_entry_bounds_and_reuses_geometry():
    pygame = pytest.importorskip("pygame")
    appearance._CHANNEL_CACHE.clear()
    appearance._channel_cache_bytes = 0
    surface = pygame.Surface((120, 80))
    for index in range(160):
        paint_channel(pygame, surface, _rect(0, 0, 100, 70), (index, 20, 25),
                      radius=32, border=(70, 90, 100))
    assert len(appearance._CHANNEL_CACHE) <= appearance._CACHE_MAX_ENTRIES
    assert appearance._channel_cache_bytes <= appearance._CACHE_MAX_BYTES
    before = len(appearance._CHANNEL_CACHE), appearance._channel_cache_bytes
    # A different window width reuses the exact same small curved-edge atlas.
    paint_channel(pygame, surface, _rect(0, 0, 115, 70), (159, 20, 25),
                  radius=32, border=(70, 90, 100))
    assert (len(appearance._CHANNEL_CACHE), appearance._channel_cache_bytes) == before


class _RecordingFont:
    def __init__(self, pygame):
        self.pygame = pygame
        self.rendered = []

    def size(self, text):
        return len(text) * 3, 5

    def get_linesize(self):
        return 5

    def render(self, text, antialias, color):
        self.rendered.append((text, color))
        return self.pygame.Surface((max(1, len(text) * 3), 5), flags=self.pygame.SRCALPHA)


@pytest.mark.parametrize("vertical", [False, True])
@pytest.mark.parametrize("value", [0, 50, 100])
def test_flowing_meter_keeps_value_extent_and_original_colour(vertical, value):
    pygame = pytest.importorskip("pygame")
    from rich_terminal.pygame_view import composite_draw_plane_result
    from rich_terminal.retained_scene import ObjectBounds, RGBA
    from rich_terminal.retained_view import MeterDraw, RetainedDrawPlane, RetainedRegionDraw

    foreground, background = (220, 65, 45), (12, 20, 25)
    meter = MeterDraw(1, 0, ObjectBounds(0, 0, 2, 2), RGBA(*foreground, 255),
                      RGBA(*background, 255), vertical, True, 0, 100, value)
    region = RetainedRegionDraw(1, 1, 1, 0, 0, 2, 2, 0, 0, 0, 0, 0, False, (meter,))
    surface = pygame.Surface((100, 60))
    font = _RecordingFont(pygame)
    composite_draw_plane_result(pygame, surface, RetainedDrawPlane(True, True, (region,)),
                                font, 50, 30, appearance=FLOWING_APPEARANCE)
    assert "".join(text for text, _ in font.rendered) == str(value)
    point = (50, 30) if value != 50 else (50, 45) if vertical else (25, 30)
    assert tuple(surface.get_at(point))[:3] == (background if value == 0 else foreground)


@pytest.mark.parametrize("enabled", [False, True])
def test_flowing_selected_tab_respects_disabled_parent_and_shortcut_contrast(enabled):
    pygame = pytest.importorskip("pygame")
    from rich_terminal.pygame_view import _DISABLED_TEXT, composite_draw_plane_result
    from rich_terminal.retained_scene import ControlState, ObjectBounds
    from rich_terminal.retained_view import RetainedDrawPlane, RetainedRegionDraw, TabDraw, TabSetDraw

    visible = ControlState.VISIBLE
    active = visible | ControlState.ENABLED
    tab = TabDraw(2, active | ControlState.SELECTED, 0, "One", "F1")
    tabs = TabSetDraw(1, active if enabled else visible, 0, 0,
                     ObjectBounds(0, 0, 10, 2), (tab,))
    region = RetainedRegionDraw(1, 1, 1, 0, 0, 10, 2, 0, 0, 0, 0, 0, False, (tabs,))
    surface = pygame.Surface((100, 40))
    font = _RecordingFont(pygame)
    result = composite_draw_plane_result(
        pygame, surface, RetainedDrawPlane(True, True, (region,)), font, 10, 20,
        appearance=FLOWING_APPEARANCE,
    )
    assert "".join(text for text, _ in font.rendered) == "OneF1"
    assert {tuple(color) for _, color in font.rendered} == {
        FLOWING_APPEARANCE.surface if enabled else _DISABLED_TEXT
    }
    assert bool(result.hit_targets) is enabled


@pytest.mark.parametrize("kind", ("tab", "menu"))
def test_button_selection_leaves_container_dividers_and_hit_geometry_intact(kind):
    pygame = pytest.importorskip("pygame")
    from rich_terminal.pygame_view import composite_draw_plane_result
    from rich_terminal.retained_scene import ControlState, ObjectBounds
    from rich_terminal.retained_view import (
        MenuBarDraw, MenuDraw, RetainedDrawPlane, RetainedRegionDraw, TabDraw, TabSetDraw,
    )

    active = ControlState.VISIBLE | ControlState.ENABLED
    height = 40 if kind == "tab" else 7

    def render(selected):
        state = active | (ControlState.SELECTED if selected else ControlState(0))
        bounds = ObjectBounds(0, 0, 10, 1)
        if kind == "tab":
            draw = TabSetDraw(1, active, 0, 0, bounds, (TabDraw(2, state, 0, "One", ""),))
        else:
            if selected:
                state |= ControlState.OPEN
            draw = MenuBarDraw(1, active, 0, 0, bounds, (MenuDraw(2, state, 0, "One", ()),))
        region = RetainedRegionDraw(1, 1, 1, 0, 0, 10, 1, 0, 0, 0, 0, 0, False, (draw,))
        surface = pygame.Surface((100, height))
        font = _RecordingFont(pygame)
        result = composite_draw_plane_result(
            pygame, surface, RetainedDrawPlane(True, True, (region,)), font, 10, height,
            appearance=FLOWING_APPEARANCE,
        )
        return surface, result

    idle, idle_result = render(False)
    selected, selected_result = render(True)
    assert selected_result.hit_entries == idle_result.hit_entries
    assert pygame.image.tobytes(selected, "RGBA") != pygame.image.tobytes(idle, "RGBA")
    edge_rows = (0, 1, height - 2, height - 1) if kind == "tab" else (0, height - 1)
    assert all(selected.get_at((x, y)) == idle.get_at((x, y))
               for y in edge_rows for x in range(100))
    assert all(selected.get_at((x, y)) == idle.get_at((x, y))
               for x in (0, 1, 98, 99) for y in range(height))


class _EdgeFont:
    """Full-height glyphs expose material gaps otherwise hidden by font margins."""

    marker = (231, 7, 219)

    def __init__(self, pygame, *, ink):
        self.pygame = pygame
        self.ink = ink
        self.colors = []

    def size(self, text):
        return len(text) * 4, 20

    def get_linesize(self):
        return 20

    def render(self, text, antialias, color):
        self.colors.append(tuple(color)[:3])
        glyph = self.pygame.Surface((max(1, len(text) * 4), 20),
                                    flags=self.pygame.SRCALPHA)
        if self.ink:
            glyph.fill((*self.marker, 255))
        return glyph


def _edge_collection(kind, *, text=""):
    from rich_terminal.retained_scene import ControlState, ObjectBounds
    from rich_terminal.retained_view import ItemViewDraw, TextAreaDraw, TextGridDraw
    from rich_terminal.semantic_content import (
        SemanticContentFlag, SemanticTextContent, SemanticTextItem,
        SemanticTextRole, SemanticTextState,
    )
    from rich_terminal.semantic_items import ItemColumn, ItemColumnKind, ItemViewContent, ItemViewRole

    bounds = ObjectBounds(0, 0, 10, 1)
    state = ControlState.VISIBLE | ControlState.ENABLED
    if kind == "items":
        content = ItemViewContent(1, ItemViewRole.LIST, 0,
                                  (ItemColumn(ItemColumnKind.TEXT),), 0, 0, 0, ())
        return ItemViewDraw(1, state, 0, 0, bounds, content)
    items = (() if not text else
             (SemanticTextItem(1, 0, 0, 1, 1, SemanticTextRole.CONTENT,
                               SemanticTextState(0), text),))
    content = SemanticTextContent(1, 1, 1, 0, 0, 1, 1,
                                  SemanticContentFlag.READ_ONLY,
                                  1 if items else 0, 0, 0, 0, items)
    draw_type = TextAreaDraw if kind == "area" else TextGridDraw
    return draw_type(1, state, 0, 0, bounds, content)


def _render_edge_draw(pygame, draw, font):
    from rich_terminal.pygame_view import composite_draw_plane_result
    from rich_terminal.retained_view import RetainedDrawPlane, RetainedRegionDraw

    surface = pygame.Surface((100, 20))
    surface.fill(FLOWING_APPEARANCE.surface)
    region = RetainedRegionDraw(1, 1, 1, 0, 0, 10, 1, 0, 0, 0, 0, 0, False, (draw,))
    composite_draw_plane_result(pygame, surface, RetainedDrawPlane(True, True, (region,)),
                                font, 10, 20, appearance=FLOWING_APPEARANCE)
    return surface


@pytest.mark.parametrize("kind", ("area", "grid", "items"))
def test_zero_inset_collection_roots_keep_decoration_out_of_the_text_interior(kind):
    pygame = pytest.importorskip("pygame")
    surface = _render_edge_draw(pygame, _edge_collection(kind), _EdgeFont(pygame, ink=False))
    # One perimeter pixel and the shallow top highlight may remain.  A curved
    # outline must not occupy the interior of these unpadded text viewports.
    assert all(tuple(surface.get_at((x, y)))[:3] == FLOWING_APPEARANCE.surface
               for y in range(2, 19) for x in range(1, 99))


@pytest.mark.parametrize("kind", ("selected_grid", "readout"))
def test_edge_text_retains_contrasting_material_without_moving_its_slots(kind):
    pygame = pytest.importorskip("pygame")
    from rich_terminal.retained_scene import ObjectBounds, RGBA
    from rich_terminal.retained_view import ReadoutDraw

    text = "W" * 24
    foreground = FLOWING_APPEARANCE.surface
    if kind == "selected_grid":
        draw = _edge_collection("grid", text=text)
    else:
        draw = ReadoutDraw(1, 0, ObjectBounds(0, 0, 10, 1),
                           RGBA(*foreground, 255), RGBA(*FLOWING_APPEARANCE.accent, 255), text)
    underlay = _render_edge_draw(pygame, draw, _EdgeFont(pygame, ink=False))
    font = _EdgeFont(pygame, ink=True)
    ink = _render_edge_draw(pygame, draw, font)
    occupied = {(x, y) for y in range(20) for x in range(100)
                if tuple(ink.get_at((x, y)))[:3] == font.marker}
    assert occupied == {(x, y) for y in range(20) for x in range(2, 98)}
    assert set(font.colors) == {foreground}
    # Check the fill beneath every painted glyph pixel, including the top and
    # bottom corners of both edge characters.  Transparent capsule corners
    # previously exposed the same dark surface used for selected text.
    assert all(sum(abs(channel - foreground[index])
                   for index, channel in enumerate(tuple(underlay.get_at(point))[:3])) > 200
               for point in occupied)
