"""Deterministic off-screen pixel tests for root-LABEL composition."""

from __future__ import annotations

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.pygame_view import (
    composite_root_labels,
    unorm_high_edge,
    unorm_low_edge,
)
from rich_terminal.retained_view import (
    RetainedLabelDraw,
    RetainedRegionDraw,
    RetainedRootLabelPlane,
)


def _label(
    object_id: int,
    z_order: int,
    color,
    text: str,
    *,
    horizontal_align: int = 0,
    vertical_align: int = 0,
    ellipsize: bool = False,
) -> RetainedLabelDraw:
    return RetainedLabelDraw(
        object_id=object_id,
        z_order=z_order,
        left=0,
        top=0,
        right=UINT32_MAX,
        bottom=UINT32_MAX,
        red=color[0],
        green=color[1],
        blue=color[2],
        alpha=color[3],
        horizontal_align=horizontal_align,
        vertical_align=vertical_align,
        ellipsize=ellipsize,
        text=text,
    )


def _plane(region: RetainedRegionDraw) -> RetainedRootLabelPlane:
    return RetainedRootLabelPlane(True, True, (region,))


class _BlockFont:
    """A fixed two-by-three-pixel glyph oracle backed by pygame surfaces."""

    def __init__(self, pygame_module):
        self.pygame = pygame_module
        self.rendered: list[str] = []

    @staticmethod
    def size(text: str):
        return len(text) * 2, 3

    def render(self, text: str, antialias: bool, color):
        assert antialias
        self.rendered.append(text)
        glyph = self.pygame.Surface(
            (len(text) * 2, 3),
            flags=self.pygame.SRCALPHA,
        )
        glyph.fill((*color, 255))
        return glyph


def test_unorm_edges_use_exact_floor_and_ceiling_without_float_rounding():
    half_low = UINT32_MAX // 2
    half_high = half_low + 1

    assert unorm_low_edge(0, 10) == 0
    assert unorm_high_edge(0, 10) == 0
    assert unorm_low_edge(UINT32_MAX, 10) == 10
    assert unorm_high_edge(UINT32_MAX, 10) == 10
    assert unorm_low_edge(half_low, 10) == 4
    assert unorm_high_edge(half_low, 10) == 5
    assert unorm_low_edge(half_high, 10) == 5
    assert unorm_high_edge(half_high, 10) == 6
    with pytest.raises(TypeError, match="not bool"):
        unorm_low_edge(True, 10)


def test_compositor_draws_labels_over_cell_in_order_and_leaves_cursor_last():
    pygame = pytest.importorskip("pygame")
    cell_color = (8, 12, 20)
    red = (220, 30, 40, 255)
    green = (20, 210, 70, 255)
    surface = pygame.Surface((16, 10))
    surface.fill(cell_color)
    font = _BlockFont(pygame)
    region = RetainedRegionDraw(
        owner_id=1,
        owner_generation=1,
        region_id=1,
        cell_x=0,
        cell_y=0,
        cell_cols=4,
        cell_rows=2,
        z_order=0,
        clipped=True,
        labels=(
            _label(1, 0, red, "X", horizontal_align=1, vertical_align=1),
            _label(2, 1, green, "X", horizontal_align=1, vertical_align=1),
        ),
    )

    result = composite_root_labels(pygame, surface, _plane(region), font, 4, 5)

    assert result is surface
    assert tuple(surface.get_at((0, 0)))[:3] == cell_color
    assert tuple(surface.get_at((7, 3)))[:3] == green[:3]
    assert font.rendered == ["X", "X"]

    # Cursor presentation remains the caller's final overlay, not retained
    # compositor state.
    pygame.draw.rect(surface, (255, 255, 255), (7, 4, 2, 1))
    assert tuple(surface.get_at((7, 4)))[:3] == (255, 255, 255)


def test_compositor_clips_nonwrapping_text_and_applies_requested_ellipsis():
    pygame = pytest.importorskip("pygame")
    cell_color = (3, 5, 7)
    surface = pygame.Surface((12, 5))
    surface.fill(cell_color)
    font = _BlockFont(pygame)
    region = RetainedRegionDraw(
        owner_id=1,
        owner_generation=1,
        region_id=1,
        cell_x=1,
        cell_y=0,
        cell_cols=1,
        cell_rows=1,
        z_order=0,
        clipped=False,
        labels=(
            _label(
                1,
                0,
                (180, 90, 30, 255),
                "XXXX",
                ellipsize=True,
            ),
        ),
    )

    composite_root_labels(pygame, surface, _plane(region), font, 4, 5)

    assert font.rendered == ["X…"]
    assert tuple(surface.get_at((4, 0)))[:3] == (180, 90, 30)
    assert tuple(surface.get_at((7, 0)))[:3] == (180, 90, 30)
    assert tuple(surface.get_at((8, 0)))[:3] == cell_color


def test_compositor_source_over_blends_label_alpha_onto_cell_pixels():
    pygame = pytest.importorskip("pygame")
    surface = pygame.Surface((4, 5))
    surface.fill((10, 20, 30))
    font = _BlockFont(pygame)
    region = RetainedRegionDraw(
        owner_id=1,
        owner_generation=1,
        region_id=1,
        cell_x=0,
        cell_y=0,
        cell_cols=1,
        cell_rows=1,
        z_order=0,
        clipped=True,
        labels=(_label(1, 0, (210, 120, 50, 128), "X"),),
    )

    composite_root_labels(pygame, surface, _plane(region), font, 4, 5)

    # SDL's integer source-over path rounds each channel to its nearest
    # representable byte.  Assert the exact deterministic off-screen pixel.
    assert tuple(surface.get_at((0, 0)))[:3] == (110, 70, 40)


def test_compositor_intersects_a_caller_owned_surface_clip():
    pygame = pytest.importorskip("pygame")
    cell_color = (2, 4, 6)
    surface = pygame.Surface((8, 5))
    surface.fill(cell_color)
    surface.set_clip(pygame.Rect(4, 0, 4, 5))
    font = _BlockFont(pygame)
    region = RetainedRegionDraw(
        owner_id=1,
        owner_generation=1,
        region_id=1,
        cell_x=0,
        cell_y=0,
        cell_cols=2,
        cell_rows=1,
        z_order=0,
        clipped=True,
        labels=(_label(1, 0, (180, 90, 30, 255), "XXXX"),),
    )

    composite_root_labels(pygame, surface, _plane(region), font, 4, 5)

    assert tuple(surface.get_at((0, 0)))[:3] == cell_color
    assert tuple(surface.get_at((4, 0)))[:3] == (180, 90, 30)
    assert surface.get_clip() == pygame.Rect(4, 0, 4, 5)
