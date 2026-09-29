"""Partial repaint of the shared-session viewer (docs/viewer-partial-repaint.md).

A composition records, region by region and draw by draw, the pixels each
draw may change and the hit entries it produced.  A later frame repaints
only what changed from that record, and must equal a full composition.
"""

from __future__ import annotations

import gzip
import json
import random
from dataclasses import replace
from pathlib import Path

import pytest

pygame = pytest.importorskip("pygame")

from rich_terminal.pygame_view import (  # noqa: E402
    ATTR_BOLD,
    ATTR_UNDERLINE,
    ControlIdentity,
    PlotDraw,
    WaveformDraw,
    composite_draw_plane_result,
)
from rich_terminal.retained_scene import (  # noqa: E402
    RGBA,
    ControlState,
    ObjectBounds,
    Sample,
)
from rich_terminal.retained_view import (  # noqa: E402
    GlyphRunDraw,
    ImageDraw,
    MenuBarDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    TabSetDraw,
    retained_draw_order,
)
from display import VirtualTerminal  # noqa: E402
from session_viewer import (  # noqa: E402
    apply_terminal_snapshot,
    compose_terminal_frame_changes,
    compose_terminal_frame_result,
)
from shared_session import display_offer_from_wire  # noqa: E402
from tests.test_rich_terminal_semantic_shared_wire import (  # noqa: E402
    _collection_plane,
    _instrument_plane,
    _menu_bar,
    _plane,
    _polyline,
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


# ---------------------------------------------------------------------------
# Repainting only what a frame changes
# ---------------------------------------------------------------------------


def _full(offer_cell, plane, font, *, show_cursor=True, hovered=None, pressed=None):
    """The reference: a full composition on a fresh terminal and glyph cache."""

    terminal = VirtualTerminal(cols=offer_cell.cols, rows=offer_cell.rows)
    apply_terminal_snapshot(terminal, offer_cell)
    return compose_terminal_frame_result(
        pygame, terminal, font, *_cell_size(font), retained_plane=plane,
        show_cursor=show_cursor, glyph_cache={}, control_font=font,
        hovered=hovered, pressed=pressed,
    )


def _assert_frame(frame, reference):
    assert frame.surface.get_size() == reference.surface.get_size()
    assert pygame.image.tobytes(frame.surface, "RGBA") == pygame.image.tobytes(
        reference.surface, "RGBA"
    )
    assert frame.hit_entries == reference.hit_entries


def test_typing_frames_repaint_to_their_full_composition(font):
    offers = typing_offers()
    terminal = VirtualTerminal(cols=offers[0].cell.cols, rows=offers[0].cell.rows)
    cache = {}
    frame = None
    partial = 0
    for offer in offers:
        apply_terminal_snapshot(terminal, offer.cell)
        frame = compose_terminal_frame_changes(
            pygame, terminal, font, *_cell_size(font), retained_plane=offer.retained,
            show_cursor=True, glyph_cache=cache, control_font=font, previous=frame,
        )
        _assert_frame(frame, _full(offer.cell, offer.retained, font))
        if frame.damage is not None:
            partial += 1
            width, height = frame.surface.get_size()
            assert sum(rect.width * rect.height for rect in frame.damage) < width * height // 4
    assert partial == len(offers) - 1


# ---------------------------------------------------------------------------
# Random edits of a scene with every draw family
# ---------------------------------------------------------------------------

SCENE_COLS, SCENE_ROWS = 80, 25
_TEXT = "AMW@gj|_. ~#ďĲŁ€Ωй"
_OPEN = ControlState.OPEN | ControlState.SELECTED


def _scene_plane(draws_a, draws_b, header_b, history):
    region_a = RetainedRegionDraw(1, 2, 3, 0, 0, SCENE_COLS, SCENE_ROWS,
                                  0, 0, 0, 0, 0, False, retained_draw_order(draws_a))
    region_b = RetainedRegionDraw(1, 2, 4, *header_b, 1, True, retained_draw_order(draws_b))
    return RetainedDrawPlane(True, True, (region_a, region_b), (history,))


def _initial_scene():
    collection = _collection_plane().regions[0].draws
    tabset, area, grid = collection
    draws_a = {
        "menu": replace(_menu_bar(z_order=6), bounds=ObjectBounds(0, 0, 60, 1)),
        "tabset": replace(tabset, bounds=ObjectBounds(0, 1, 40, 2)),
        "area": replace(area, bounds=ObjectBounds(0, 3, 40, 10)),
        "grid": replace(grid, bounds=ObjectBounds(42, 3, 30, 8)),
        "polyline": replace(_polyline(object_id=12, z_order=5),
                            bounds=ObjectBounds(50, 14, 20, 8), parent_bounds=()),
        "readout": replace(_instrument_plane().regions[0].draws[0],
                           object_id=60, bounds=ObjectBounds(72, 3, 8, 1), parent_bounds=()),
        "meter": replace(_instrument_plane().regions[0].draws[1],
                         object_id=61, bounds=ObjectBounds(72, 5, 8, 2), parent_bounds=()),
        "status": replace(_instrument_plane().regions[0].draws[2],
                          object_id=62, bounds=ObjectBounds(72, 8, 2, 1), parent_bounds=()),
        "plot": replace(_series_plane().regions[0].draws[0],
                        object_id=70, bounds=ObjectBounds(0, 15, 30, 8)),
        "waveform": replace(_series_plane().regions[0].draws[1],
                            object_id=71, bounds=ObjectBounds(32, 15, 16, 8)),
    }
    for index in range(6):
        draws_a[f"run{index}"] = GlyphRunDraw(
            100 + index, index % 4, ObjectBounds(4 * index, 13 + index % 2, 12, 1),
            RGBA(230, 235, 244, 255), RGBA(17, 20, 28, 255 if index % 3 else 160),
            0, "run %d" % index,
        )
    draws_b = {
        f"b{index}": GlyphRunDraw(
            200 + index, 0, ObjectBounds(index * 6, index, 10, 2),
            RGBA(250, 200, 40, 255), RGBA(20, 60, 160, 255), index % 2, "over %d" % index,
        )
        for index in range(4)
    }
    return draws_a, draws_b, [44, 12, 30, 10, 2, 1, 20, 6], _series_plane().series[0]


def _scene_terminal(randomizer) -> VirtualTerminal:
    terminal = VirtualTerminal(cols=SCENE_COLS, rows=SCENE_ROWS)
    palette = [(0, 0, 0), (200, 40, 40), (40, 200, 90), (230, 230, 230), (20, 60, 160)]
    with terminal._lock:
        terminal.grid = [
            [(randomizer.choice(_TEXT), randomizer.choice(palette),
              randomizer.choice(palette), randomizer.choice((0, 1, 8, 32, 128)))
             for _ in range(SCENE_COLS)]
            for _ in range(SCENE_ROWS)
        ]
    return terminal


def _edit(randomizer, draws_a, draws_b, header_b, history, terminal, state):
    """Apply one random edit to the scene or the viewer state."""

    choice = randomizer.randrange(11)
    runs = [key for key in draws_a if key.startswith("run")]
    if choice == 0 and runs:
        key = randomizer.choice(runs)
        draws_a[key] = replace(draws_a[key], text="".join(
            randomizer.choice(_TEXT) for _ in range(randomizer.randrange(1, 9))))
    elif choice == 1 and runs:
        key = randomizer.choice(runs)
        draws_a[key] = replace(draws_a[key], bounds=ObjectBounds(
            randomizer.randrange(70), randomizer.randrange(24), randomizer.randrange(1, 12), 1),
            z_order=randomizer.randrange(7))
    elif choice == 2 and runs:
        del draws_a[randomizer.choice(runs)]
    elif choice == 3:
        object_id = 100 + randomizer.randrange(20)
        draws_a[f"run{object_id - 100}"] = GlyphRunDraw(
            object_id, randomizer.randrange(7),
            ObjectBounds(randomizer.randrange(70), randomizer.randrange(24), 10, 1),
            RGBA(250, 250, 250, 255), RGBA(40, 200, 90, randomizer.choice((255, 128))),
            randomizer.choice((0, ATTR_UNDERLINE, ATTR_BOLD)), "new")
    elif choice == 4:
        # A closed menu shows no entries; reopening restores them.
        menu = draws_a["menu"]
        first = menu.menus[0]
        if first.state & ControlState.OPEN:
            first = replace(first, state=first.state & ~_OPEN, entries=())
        else:
            first = replace(first, state=first.state | _OPEN,
                            entries=_menu_bar().menus[0].entries)
        draws_a["menu"] = replace(menu, menus=(first, *menu.menus[1:]))
    elif choice == 5:
        area = draws_a["area"]
        content = area.content
        draws_a["area"] = replace(area, content=replace(
            content, content_revision=content.content_revision + 1,
            primary_offset=randomizer.choice((content.primary_offset, 0, 1))))
    elif choice == 6:
        grid = draws_a["grid"]
        draws_a["grid"] = replace(grid, content=replace(
            grid.content, content_revision=grid.content.content_revision + 1))
    elif choice == 7:
        header_b[:2] = [randomizer.randrange(40, 60), randomizer.randrange(5, 15)]
    elif choice == 8:
        history = replace(history, samples=tuple(
            Sample(timestamp, randomizer.randrange(-30, 30)) for timestamp in (10, 20, 40)))
    elif choice == 9:
        with terminal._lock:
            for _ in range(randomizer.randrange(1, 6)):
                row = terminal.grid[randomizer.randrange(SCENE_ROWS)]
                column = randomizer.randrange(SCENE_COLS)
                row[column] = (randomizer.choice(_TEXT), (250, 250, 250), (0, 0, 0), 0)
            terminal.cx = randomizer.randrange(SCENE_COLS)
            terminal.cy = randomizer.randrange(SCENE_ROWS)
            terminal.cursor_visible = randomizer.random() < 0.7
        state["show_cursor"] = randomizer.random() < 0.7
    else:
        identities = [None] + [ControlIdentity(1, 2, control_id)
                               for control_id in (21, 22, 24, 25, 31, 32)]
        state["hovered"] = randomizer.choice(identities)
        state["pressed"] = randomizer.choice(identities)
    return history


@pytest.mark.parametrize("seed", range(4))
def test_random_scene_edits_repaint_to_their_full_composition(font, seed):
    randomizer = random.Random(seed)
    draws_a, draws_b, header_b, history = _initial_scene()
    terminal = _scene_terminal(randomizer)
    state = {"show_cursor": True, "hovered": None, "pressed": None}
    cache = {}
    frame = None
    partial = 0
    for _step in range(40):
        for _ in range(randomizer.randrange(1, 4)):
            history = _edit(randomizer, draws_a, draws_b, header_b, history, terminal, state)
        plane = _scene_plane(draws_a.values(), draws_b.values(), header_b, history)
        frame = compose_terminal_frame_changes(
            pygame, terminal, font, *_cell_size(font), retained_plane=plane,
            show_cursor=state["show_cursor"], glyph_cache=cache, control_font=font,
            hovered=state["hovered"], pressed=state["pressed"], previous=frame,
        )
        reference_terminal = VirtualTerminal(cols=SCENE_COLS, rows=SCENE_ROWS)
        with terminal._lock:
            reference_terminal.grid = [list(row) for row in terminal.grid]
            reference_terminal.cx, reference_terminal.cy = terminal.cx, terminal.cy
            reference_terminal.cursor_visible = terminal.cursor_visible
        reference = compose_terminal_frame_result(
            pygame, reference_terminal, font, *_cell_size(font), retained_plane=plane,
            show_cursor=state["show_cursor"], glyph_cache={}, control_font=font,
            hovered=state["hovered"], pressed=state["pressed"],
        )
        _assert_frame(frame, reference)
        partial += frame.damage is not None
    assert partial >= 10


def test_a_changed_control_keeps_the_entries_of_its_whole_repaint(font):
    """A text area and a later glyph run that sticks out past its edge both
    change.  The run's rectangle repaints part of the text area after the
    text area's own rectangle repainted all of it; only the whole repaint's
    entries are exact."""

    area = replace(_collection_plane().regions[0].draws[1], bounds=ObjectBounds(0, 3, 40, 10))

    def plane(area_draw, text):
        run = GlyphRunDraw(100, 5, ObjectBounds(30, 5, 20, 1), RGBA(250, 250, 250, 255),
                           RGBA(40, 60, 160, 255), 0, text)
        region = RetainedRegionDraw(1, 2, 3, 0, 0, SCENE_COLS, SCENE_ROWS, 0, 0, 0, 0,
                                    0, False, retained_draw_order((area_draw, run)))
        return RetainedDrawPlane(True, True, (region,))

    terminal = _scene_terminal(random.Random(5))
    cache = {}
    first = compose_terminal_frame_changes(
        pygame, terminal, font, *_cell_size(font), retained_plane=plane(area, "one"),
        show_cursor=False, glyph_cache=cache, control_font=font,
    )
    moved = replace(area, content=replace(
        area.content, content_revision=area.content.content_revision + 1, primary_offset=0))
    second_plane = plane(moved, "two")
    second = compose_terminal_frame_changes(
        pygame, terminal, font, *_cell_size(font), retained_plane=second_plane,
        show_cursor=False, glyph_cache=cache, control_font=font, previous=first,
    )
    assert second.damage is not None and len(second.damage) >= 2
    reference_terminal = VirtualTerminal(cols=SCENE_COLS, rows=SCENE_ROWS)
    reference_terminal.grid = [list(row) for row in terminal.grid]
    _assert_frame(second, compose_terminal_frame_result(
        pygame, reference_terminal, font, *_cell_size(font), retained_plane=second_plane,
        show_cursor=False, glyph_cache={}, control_font=font,
    ))
