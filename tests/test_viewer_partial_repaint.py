"""Partial repaint of the shared-session viewer (docs/viewer-partial-repaint.md).

A composition records, region by region and draw by draw, the pixels each
draw may change and the hit entries it produced.  A later frame repaints
only what changed from that record, and must equal a full composition.
"""

from __future__ import annotations

import gzip
import json
from dataclasses import replace
from pathlib import Path

import pytest

pygame = pytest.importorskip("pygame")

from rich_terminal.pygame_view import (  # noqa: E402
    ControlIdentity,
    PlotDraw,
    WaveformDraw,
    composite_draw_plane_result,
)
from rich_terminal.retained_scene import ControlState  # noqa: E402
from rich_terminal.retained_view import (  # noqa: E402
    ImageDraw,
    MenuBarDraw,
    RetainedDrawPlane,
    TabSetDraw,
)
from shared_session import display_offer_from_wire  # noqa: E402
from tests.test_rich_terminal_semantic_shared_wire import (  # noqa: E402
    _collection_plane,
    _instrument_plane,
    _plane,
    _series_plane,
)

FIXTURES = Path(__file__).parent / "fixtures" / "compositor"
SENTINEL = (1, 2, 3)


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 18)


def _cell_size(font):
    return font.size("M")[0], font.get_linesize()


def typing_offers():
    """Twelve consecutive offers of Akashic fa068df2's typing run on MegaPad
    2f42352, the first complete and each later one as changes to the one
    before it."""

    wires = json.loads(gzip.decompress((FIXTURES / "desktop-typing-sequence.json.gz").read_bytes()))
    offers = [display_offer_from_wire(wires[0])]
    for wire in wires[1:]:
        offers.append(display_offer_from_wire(wire, offers[-1]))
    return offers


def _planes():
    """Real Desktop planes and synthetic ones with every draw family, open
    menus and tabsets."""

    planes = {"typing": typing_offers()[0].retained}
    for name in ("desktop-ready.json.gz", "desktop-typed.json.gz"):
        wire = json.loads(gzip.decompress((FIXTURES / name).read_bytes()))
        planes[name] = display_offer_from_wire(wire).retained
    planes.update(
        menus=_plane(),
        collections=_collection_plane(),
        instruments=_instrument_plane(),
        series=_series_plane(),
    )
    return planes


def _surface(plane, font):
    cell_w, cell_h = _cell_size(font)
    cols = max((r.logical_x + r.logical_cols for r in plane.regions), default=1)
    rows = max((r.logical_y + r.logical_rows for r in plane.regions), default=1)
    return pygame.Surface((min(cols, 280) * cell_w, min(rows, 84) * cell_h))


def _compose(surface, plane, font, **kwargs):
    return composite_draw_plane_result(
        pygame, surface, plane, font, *_cell_size(font), control_font=font, **kwargs
    )


def _alone(plane, region, draw):
    """A plane holding only DRAW, with the series it reads."""

    series = ()
    if isinstance(draw, (PlotDraw, WaveformDraw)):
        key = (region.owner_id, region.owner_generation, draw.series_id)
        series = tuple(history for history in plane.series if history.key == key)
    return RetainedDrawPlane(True, True, (replace(region, draws=(draw,)),), series=series)


def _changed(surface):
    """Bounding rectangles of every pixel that is no longer the sentinel."""

    mask = pygame.mask.from_threshold(surface, SENTINEL, (1, 1, 1, 255))
    mask.invert()
    return mask.get_bounding_rects()


@pytest.mark.parametrize("name", sorted(_planes()))
def test_the_painted_record_gives_the_hit_map_in_painter_order(font, name):
    plane = _planes()[name]
    result = _compose(_surface(plane, font), plane, font)
    assert len(result.regions) == len(plane.regions)
    assert tuple(
        entry for region in result.regions for entry in region.entries()
    ) == result.hit_entries
    for region, painted in zip(plane.regions, result.regions):
        assert painted.key == (region.owner_id, region.owner_generation, region.region_id)
        assert [draw.key for draw in painted.draws] == [
            ("object", draw.object_id) if hasattr(draw, "object_id")
            else ("control", draw.control_id)
            for draw in region.draws
        ]


@pytest.mark.parametrize("name", sorted(_planes()))
def test_every_draw_paints_only_inside_its_recorded_extent(font, name):
    plane = _planes()[name]
    surface = _surface(plane, font)
    result = _compose(surface, plane, font)
    checked = 0
    for region, painted in zip(plane.regions, result.regions):
        for index, (draw, record) in enumerate(zip(region.draws, painted.draws)):
            if isinstance(draw, ImageDraw):
                continue
            # Every control, and a spread of the many glyph runs.
            if name != "menus" and not hasattr(draw, "control_id") and index % 9:
                continue
            identities = [None]
            if isinstance(draw, (MenuBarDraw, TabSetDraw)):
                children = draw.menus if isinstance(draw, MenuBarDraw) else draw.tabs
                identities += [
                    ControlIdentity(region.owner_id, region.owner_generation, child.control_id)
                    for child in children
                ]
            for identity in identities:
                surface.fill(SENTINEL)
                _compose(surface, _alone(plane, region, draw), font,
                         hovered=identity, pressed=identity)
                changed = _changed(surface)
                if record.extent is None:
                    assert not changed, draw
                    continue
                extent = pygame.Rect(record.extent.left, record.extent.top,
                                     record.extent.width, record.extent.height)
                assert all(extent.contains(rect) for rect in changed), (draw, changed, extent)
                checked += 1
    assert checked


def test_an_open_menu_bar_may_paint_its_whole_region(font):
    plane = _planes()["menus"]
    result = _compose(_surface(plane, font), plane, font)
    for region, painted in zip(plane.regions, result.regions):
        for draw, record in zip(region.draws, painted.draws):
            if isinstance(draw, MenuBarDraw):
                opened = any(menu.state & ControlState.OPEN for menu in draw.menus)
                assert (record.extent == painted.viewport) is opened
