"""Deterministic off-screen pixel tests for generic draw-plane composition."""

from __future__ import annotations

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.pygame_view import composite_draw_plane, unorm_high_edge, unorm_low_edge
from rich_terminal.retained_model import ResourceFormat
from rich_terminal.retained_scene import ImageFit, ObjectBounds, Point, RGBA, Sample
from rich_terminal.retained_view import (
    GlyphRunDraw,
    ImageDraw,
    ImageResourceManifest,
    MeterDraw,
    PlotDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    SeriesHistoryDraw,
    StatusDraw,
    WaveformDraw,
)


def _run(object_id, z_order, foreground, background, text, *, attributes=0):
    return GlyphRunDraw(object_id, z_order, ObjectBounds(0, 0, 2, 1), RGBA(*foreground), RGBA(*background), attributes, text)


def _region(*draws, clipped=True):
    clip = (0, 0, 2, 1) if clipped else (0, 0, 0, 0)
    return RetainedRegionDraw(
        1, 1, 1, 0, 0, 2, 1, *clip, 0, clipped, draws
    )


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


def test_polyline_uses_iterative_group_geometry_without_crossing_parent_bounds():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((10, 10))
    surface.fill((2, 4, 6))
    font = _PixelFont(pygame)
    line = PolylineDraw(
        object_id=1,
        z_order=0,
        bounds=ObjectBounds(0, 0, 1, 1),
        points=(Point(UINT32_MAX, 0), Point(UINT32_MAX, UINT32_MAX)),
        stroke_width=1,
        color=RGBA(20, 210, 70, 255),
        closed=False,
        parent_bounds=(ObjectBounds(0, 0, 1, 1),),
    )
    region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, True, (line,)
    )

    composite_draw_plane(pygame, surface, _plane(region), font, 10, 10)

    assert tuple(surface.get_at((4, 5)))[:3] == (20, 210, 70)
    assert tuple(surface.get_at((5, 5)))[:3] == (2, 4, 6)


def test_closed_polyline_adds_only_the_canonical_final_segment():
    pygame = pytest.importorskip("pygame")
    font = _PixelFont(pygame)

    def paint(closed):
        surface = pygame.Surface((9, 9))
        surface.fill((2, 4, 6))
        line = PolylineDraw(
            1,
            0,
            ObjectBounds(0, 0, 1, 1),
            (
                Point(0, 0),
                Point(UINT32_MAX, 0),
                Point(UINT32_MAX, UINT32_MAX),
            ),
            1,
            RGBA(220, 30, 40, 255),
            closed,
        )
        region = RetainedRegionDraw(
            1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (line,)
        )
        composite_draw_plane(pygame, surface, _plane(region), font, 9, 9)
        return surface

    open_surface = paint(False)
    closed_surface = paint(True)

    assert tuple(open_surface.get_at((4, 4)))[:3] == (2, 4, 6)
    assert tuple(closed_surface.get_at((4, 4)))[:3] == (220, 30, 40)


def test_translucent_polyline_obeys_the_caller_clip_and_source_over_blending():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((10, 5))
    surface.fill((20, 40, 60))
    surface.set_clip(pygame.Rect(5, 0, 5, 5))
    font = _PixelFont(pygame)
    line = PolylineDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        (Point(0, 0), Point(UINT32_MAX, 0)),
        1,
        RGBA(200, 100, 0, 128),
        False,
    )
    region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (line,)
    )

    composite_draw_plane(pygame, surface, _plane(region), font, 10, 5)

    assert tuple(surface.get_at((4, 0)))[:3] == (20, 40, 60)
    assert tuple(surface.get_at((7, 0)))[:3] == pytest.approx((110, 70, 30), abs=1)
    assert surface.get_clip() == pygame.Rect(5, 0, 5, 5)


def test_readout_right_aligns_canonical_text_and_preserves_color_alpha():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((10, 5))
    surface.fill((2, 4, 6))
    font = _PixelFont(pygame)
    readout = ReadoutDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        RGBA(200, 100, 0, 128),
        RGBA(20, 40, 60, 255),
        "12",
    )
    region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (readout,)
    )

    composite_draw_plane(pygame, surface, _plane(region), font, 10, 5)

    assert tuple(surface.get_at((0, 0)))[:3] == (20, 40, 60)
    assert tuple(surface.get_at((6, 2)))[:3] == (20, 40, 60)
    assert tuple(surface.get_at((7, 2)))[:3] == pytest.approx((110, 70, 30), abs=1)
    assert tuple(surface.get_at((8, 2)))[:3] == pytest.approx((110, 70, 30), abs=1)
    assert {rendered[0] for rendered in font.rendered} == {"1", "2"}


def test_meter_maps_horizontal_and_vertical_ranges_with_exact_integer_edges():
    pygame = pytest.importorskip("pygame")
    font = _PixelFont(pygame)

    horizontal_surface = pygame.Surface((8, 4))
    horizontal = MeterDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        RGBA(20, 210, 70, 255),
        RGBA(10, 20, 30, 255),
        False,
        False,
        0,
        100,
        25,
    )
    horizontal_region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (horizontal,)
    )
    composite_draw_plane(
        pygame, horizontal_surface, _plane(horizontal_region), font, 8, 4
    )

    vertical_surface = pygame.Surface((4, 8))
    vertical = MeterDraw(
        2,
        0,
        ObjectBounds(0, 0, 1, 1),
        RGBA(220, 30, 40, 255),
        RGBA(10, 20, 30, 255),
        True,
        False,
        -10,
        10,
        0,
    )
    vertical_region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (vertical,)
    )
    composite_draw_plane(
        pygame, vertical_surface, _plane(vertical_region), font, 4, 8
    )

    assert tuple(horizontal_surface.get_at((1, 2)))[:3] == (20, 210, 70)
    assert tuple(horizontal_surface.get_at((2, 2)))[:3] == (10, 20, 30)
    assert tuple(vertical_surface.get_at((2, 3)))[:3] == (10, 20, 30)
    assert tuple(vertical_surface.get_at((2, 4)))[:3] == (220, 30, 40)


@pytest.mark.parametrize(
    ("shape", "value", "center", "corner"),
    (
        (0, 1, (20, 210, 70), (2, 4, 6)),
        (1, 1, (20, 210, 70), (20, 210, 70)),
        (2, 1, (20, 210, 70), (2, 4, 6)),
        (1, 0, (90, 90, 90), (90, 90, 90)),
    ),
)
def test_status_rasterizes_canonical_shape_and_active_state(
    shape, value, center, corner
):
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((7, 7))
    surface.fill((2, 4, 6))
    font = _PixelFont(pygame)
    status = StatusDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        RGBA(90, 90, 90, 255),
        RGBA(20, 210, 70, 255),
        value,
        shape,
    )
    region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (status,)
    )

    composite_draw_plane(pygame, surface, _plane(region), font, 7, 7)

    assert tuple(surface.get_at((3, 3)))[:3] == center
    assert tuple(surface.get_at((0, 0)))[:3] == corner


def _series_plane(draw, samples):
    history = SeriesHistoryDraw(1, 1, draw.series_id, tuple(samples))
    region = RetainedRegionDraw(
        1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, True, (draw,)
    )
    return RetainedDrawPlane(True, True, (region,), (history,))


def test_plot_maps_timestamps_and_clips_values_to_exact_object_edges():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((11, 11))
    surface.fill((2, 4, 6))
    font = _PixelFont(pygame)
    plot = PlotDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        7,
        -10,
        10,
        RGBA(20, 210, 70, 255),
        RGBA(0, 0, 0, 0),
        False,
        False,
    )
    samples = (Sample(10, -20), Sample(20, 20), Sample(40, 0))

    composite_draw_plane(
        pygame, surface, _series_plane(plot, samples), font, 11, 11
    )

    assert tuple(surface.get_at((0, 10)))[:3] == (20, 210, 70)
    assert tuple(surface.get_at((3, 0)))[:3] == (20, 210, 70)
    assert tuple(surface.get_at((10, 5)))[:3] == (20, 210, 70)


def test_plot_fill_uses_source_over_and_a_single_sample_is_centered():
    pygame = pytest.importorskip("pygame")
    font = _PixelFont(pygame)
    filled_surface = pygame.Surface((10, 10))
    filled_surface.fill((20, 40, 60))
    filled = PlotDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        7,
        0,
        10,
        RGBA(0, 0, 0, 0),
        RGBA(200, 100, 0, 128),
        True,
        False,
    )
    composite_draw_plane(
        pygame,
        filled_surface,
        _series_plane(filled, (Sample(10, 10), Sample(20, 10))),
        font,
        10,
        10,
    )

    single_surface = pygame.Surface((9, 9))
    single_surface.fill((2, 4, 6))
    single = PlotDraw(
        2,
        0,
        ObjectBounds(0, 0, 1, 1),
        8,
        0,
        10,
        RGBA(220, 30, 40, 255),
        RGBA(0, 0, 0, 0),
        False,
        False,
    )
    composite_draw_plane(
        pygame,
        single_surface,
        _series_plane(single, (Sample(99, 5),)),
        font,
        9,
        9,
    )

    assert tuple(filled_surface.get_at((5, 5)))[:3] == pytest.approx(
        (110, 70, 30), abs=1
    )
    assert tuple(single_surface.get_at((4, 4)))[:3] == (220, 30, 40)


def test_waveform_draws_zero_line_even_when_committed_history_is_empty():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((9, 9))
    surface.fill((2, 4, 6))
    font = _PixelFont(pygame)
    waveform = WaveformDraw(
        1,
        0,
        ObjectBounds(0, 0, 1, 1),
        7,
        -10,
        10,
        RGBA(220, 30, 40, 255),
        RGBA(90, 90, 90, 255),
        0,
        True,
    )

    composite_draw_plane(
        pygame, surface, _series_plane(waveform, ()), font, 9, 9
    )

    assert tuple(surface.get_at((4, 4)))[:3] == (90, 90, 90)
    assert tuple(surface.get_at((4, 3)))[:3] == (2, 4, 6)


def _image_manifest(resource_id, width, height):
    return ImageResourceManifest(
        owner_id=1,
        owner_generation=1,
        resource_id=resource_id,
        format=ResourceFormat.RGBA8,
        width=width,
        height=height,
        byte_length=width * height * 4,
        sha3_256=bytes((resource_id,)) * 32,
    )


def _image_draw(resource_id, fit, *, opacity=255, bounds=None, z_order=0):
    return ImageDraw(
        object_id=resource_id,
        z_order=z_order,
        bounds=(
            ObjectBounds(0, 0, 1, 1)
            if bounds is None
            else bounds
        ),
        resource_id=resource_id,
        fit=fit,
        opacity=opacity,
    )


def _image_plane(draws, manifests, *, cell_cols=1, cell_rows=1):
    region = RetainedRegionDraw(
        1,
        1,
        1,
        0,
        0,
        cell_cols,
        cell_rows,
        0,
        0,
        cell_cols,
        cell_rows,
        0,
        True,
        tuple(draws),
    )
    return RetainedDrawPlane(
        True,
        True,
        (region,),
        (),
        tuple(manifests),
    )


def _surface_pixels(surface):
    return tuple(
        tuple(tuple(surface.get_at((x, y))) for x in range(surface.get_width()))
        for y in range(surface.get_height())
    )


def test_image_dependencies_are_preflighted_before_any_destination_mutation():
    pygame = pytest.importorskip("pygame")
    destination = pygame.Surface((8, 4))
    destination.fill((2, 4, 6))
    before = _surface_pixels(destination)
    first = _image_manifest(1, 1, 1)
    second = _image_manifest(2, 1, 1)
    first_surface = pygame.Surface((1, 1), flags=pygame.SRCALPHA)
    first_surface.fill((220, 30, 40, 255))
    plane = _image_plane(
        (
            _image_draw(
                1,
                ImageFit.STRETCH,
                bounds=ObjectBounds(0, 0, 1, 1),
            ),
            _image_draw(
                2,
                ImageFit.STRETCH,
                bounds=ObjectBounds(1, 0, 1, 1),
                z_order=1,
            ),
        ),
        (first, second),
        cell_cols=2,
    )

    with pytest.raises(ValueError, match="no exact resource surface"):
        composite_draw_plane(
            pygame,
            destination,
            plane,
            _PixelFont(pygame),
            8,
            4,
            resource_surfaces={first.key: first_surface},
        )

    assert _surface_pixels(destination) == before


@pytest.mark.parametrize(
    ("fit", "top_pixel", "center_pixel", "bottom_pixel"),
    (
        (ImageFit.STRETCH, (220, 30, 40), (220, 30, 40), (220, 30, 40)),
        (ImageFit.CONTAIN, (2, 4, 6), (220, 30, 40), (2, 4, 6)),
        (ImageFit.COVER, (220, 30, 40), (220, 30, 40), (220, 30, 40)),
    ),
)
def test_image_fit_modes_use_deterministic_integer_target_geometry(
    fit,
    top_pixel,
    center_pixel,
    bottom_pixel,
):
    pygame = pytest.importorskip("pygame")
    destination = pygame.Surface((6, 6))
    destination.fill((2, 4, 6))
    source = pygame.Surface((2, 1), flags=pygame.SRCALPHA)
    source.fill((220, 30, 40, 255))
    manifest = _image_manifest(1, 2, 1)
    plane = _image_plane((_image_draw(1, fit),), (manifest,))

    composite_draw_plane(
        pygame,
        destination,
        plane,
        _PixelFont(pygame),
        6,
        6,
        resource_surfaces={manifest.key: source},
    )

    assert tuple(destination.get_at((3, 0)))[:3] == top_pixel
    assert tuple(destination.get_at((3, 2)))[:3] == center_pixel
    assert tuple(destination.get_at((3, 5)))[:3] == bottom_pixel


def test_cover_centers_oversized_raster_and_crops_at_the_object_bounds():
    pygame = pytest.importorskip("pygame")
    destination = pygame.Surface((4, 4))
    destination.fill((2, 4, 6))
    source = pygame.Surface((4, 1), flags=pygame.SRCALPHA)
    source.set_at((0, 0), (220, 30, 40, 255))
    source.set_at((1, 0), (20, 210, 70, 255))
    source.set_at((2, 0), (30, 80, 220, 255))
    source.set_at((3, 0), (240, 210, 20, 255))
    manifest = _image_manifest(1, 4, 1)
    plane = _image_plane(
        (_image_draw(1, ImageFit.COVER),),
        (manifest,),
    )

    composite_draw_plane(
        pygame,
        destination,
        plane,
        _PixelFont(pygame),
        4,
        4,
        resource_surfaces={manifest.key: source},
    )

    assert tuple(destination.get_at((0, 2)))[:3] == (20, 210, 70)
    assert tuple(destination.get_at((1, 2)))[:3] == (20, 210, 70)
    # The exact one-pixel centered crop has an odd left/right remainder;
    # deterministic top/left bias selects source pixel one.
    assert tuple(destination.get_at((2, 2)))[:3] == (20, 210, 70)
    assert tuple(destination.get_at((3, 2)))[:3] == (20, 210, 70)


def test_cover_crops_extreme_aspect_source_before_bounded_scale(monkeypatch):
    pygame = pytest.importorskip("pygame")
    destination = pygame.Surface((20, 10))
    source = pygame.Surface((256, 1), flags=pygame.SRCALPHA)
    source.fill((80, 120, 200, 255))
    manifest = _image_manifest(1, 256, 1)
    plane = _image_plane(
        (_image_draw(1, ImageFit.COVER),),
        (manifest,),
    )
    original_scale = pygame.transform.scale
    scales = []

    def recording_scale(cropped, size):
        scales.append((tuple(cropped.get_size()), tuple(size)))
        return original_scale(cropped, size)

    monkeypatch.setattr(pygame.transform, "scale", recording_scale)
    composite_draw_plane(
        pygame,
        destination,
        plane,
        _PixelFont(pygame),
        20,
        10,
        resource_surfaces={manifest.key: source},
    )

    assert scales == [((2, 1), (20, 10))]


def test_image_opacity_uses_a_temporary_and_preserves_cache_surface_and_clip():
    pygame = pytest.importorskip("pygame")
    destination = pygame.Surface((4, 2))
    destination.fill((0, 0, 200))
    destination.set_clip(pygame.Rect(2, 0, 2, 2))
    source = pygame.Surface((1, 1), flags=pygame.SRCALPHA)
    source.fill((200, 0, 0, 255))
    source_pixel = tuple(source.get_at((0, 0)))
    source_alpha = source.get_alpha()
    manifest = _image_manifest(1, 1, 1)
    plane = _image_plane(
        (_image_draw(1, ImageFit.STRETCH, opacity=128),),
        (manifest,),
    )

    composite_draw_plane(
        pygame,
        destination,
        plane,
        _PixelFont(pygame),
        4,
        2,
        resource_surfaces={manifest.key: source},
    )

    assert tuple(destination.get_at((1, 1)))[:3] == (0, 0, 200)
    assert tuple(destination.get_at((3, 1)))[:3] == pytest.approx(
        (100, 0, 100),
        abs=1,
    )
    assert destination.get_clip() == pygame.Rect(2, 0, 2, 2)
    assert tuple(source.get_at((0, 0))) == source_pixel
    assert source.get_alpha() == source_alpha
