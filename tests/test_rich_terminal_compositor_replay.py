"""Pixel-exact replay of the compositor's CELL skip and glyph-run fast path.

Each frame is composed twice: once as the viewer composes it, and once with
the CELL coverage skip and the batched glyph path both disabled, which is the
complete per-cell and per-slot reference.  Every RGBA pixel and the whole
hit map must match.  Frames are real Desktop offers recorded by Akashic's
typing run plus synthetic planes aimed at partial and translucent coverage,
glyph overhang, clipping and every glyph attribute.
"""

from __future__ import annotations

import gzip
import json
import random
from pathlib import Path

import pytest

pygame = pytest.importorskip("pygame")

import session_viewer  # noqa: E402
from display import ATTR_CONTINUATION, ATTR_WIDE, VirtualTerminal  # noqa: E402
from rich_terminal import pygame_view  # noqa: E402
from rich_terminal.pygame_view import (  # noqa: E402
    ATTR_BOLD,
    ATTR_DIM,
    ATTR_ITALIC,
    ATTR_REVERSE,
    ATTR_STRIKE,
    ATTR_UNDERLINE,
    opaque_cell_coverage,
)
from rich_terminal.retained_scene import ObjectBounds, RGBA  # noqa: E402
from rich_terminal.retained_view import (  # noqa: E402
    GlyphRunDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
)
from shared_session import display_offer_from_wire  # noqa: E402

FIXTURES = Path(__file__).parent / "fixtures" / "compositor"
# Real 280x84 Desktop offers from Akashic 49fddfd's typing run: the ready
# frame and the frame showing all nineteen typed characters.
DESKTOP_OFFERS = ("desktop-ready.json.gz", "desktop-typed.json.gz")


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    # pygame's bundled proportional font: many glyphs overhang one cell,
    # which exercises the overhang rules harder than a monospace font.
    return pygame.font.Font(None, 18)


def _cell_size(font):
    return font.size("M")[0], font.get_linesize()


def _compose(monkeypatch, terminal, plane, font, *, fast: bool):
    cell_w, cell_h = _cell_size(font)
    with monkeypatch.context() as patch:
        if not fast:
            patch.setattr(session_viewer, "_cell_coverage", lambda *args: None)
            patch.setattr(pygame_view, "_batched_glyph_blits", lambda *args: None)
        result = session_viewer.compose_terminal_frame_result(
            pygame,
            terminal,
            font,
            cell_w,
            cell_h,
            retained_plane=plane,
            show_cursor=True,
            glyph_cache={},
            control_font=font,
        )
    return pygame.image.tobytes(result.surface, "RGBA"), result.hit_entries


def _assert_exact(monkeypatch, terminal, plane, font):
    fast = _compose(monkeypatch, terminal, plane, font, fast=True)
    reference = _compose(monkeypatch, terminal, plane, font, fast=False)
    assert fast[0] == reference[0], "fast composition changed pixels"
    assert fast[1] == reference[1], "fast composition changed the hit map"


def _desktop_offer(name):
    with gzip.open(FIXTURES / name, "rt", encoding="utf-8") as stream:
        return display_offer_from_wire(json.load(stream))


@pytest.mark.parametrize("name", DESKTOP_OFFERS)
def test_real_desktop_frames_compose_identically(monkeypatch, font, name):
    offer = _desktop_offer(name)
    terminal = VirtualTerminal(cols=offer.cell.cols, rows=offer.cell.rows)
    session_viewer.apply_terminal_snapshot(terminal, offer.cell)
    covered = opaque_cell_coverage(
        pygame, offer.retained, terminal.cols, terminal.rows, *_cell_size(font)
    )
    # The recorded Desktop repaints most cells with opaque glyph runs, so the
    # skip matters here, but menus and collections keep CELL visible too.
    assert 0.5 < sum(covered) / len(covered) < 1.0
    _assert_exact(monkeypatch, terminal, offer.retained, font)


# ---------------------------------------------------------------------------
# Synthetic partial-coverage planes
# ---------------------------------------------------------------------------

COLS, ROWS = 24, 10
_CHARACTERS = "AMW@gj|_. ~#ďĲŁ€Ωй中"


def _terminal(seed: int) -> VirtualTerminal:
    randomizer = random.Random(seed)
    terminal = VirtualTerminal(cols=COLS, rows=ROWS)
    palette = [(0, 0, 0), (200, 40, 40), (40, 200, 90), (230, 230, 230), (20, 60, 160)]
    with terminal._lock:
        terminal.grid = [
            [
                (
                    randomizer.choice(_CHARACTERS),
                    randomizer.choice(palette),
                    randomizer.choice(palette),
                    randomizer.choice((0, 1, 2, 8, 32, 64, 128, 1 | 8, 32 | 128)),
                )
                for _ in range(COLS)
            ]
            for _ in range(ROWS)
        ]
        terminal.cx, terminal.cy, terminal.cursor_visible = 3, 4, True
    return terminal


def _run(object_id, bounds, foreground, background, text, attributes=0):
    return GlyphRunDraw(
        object_id,
        object_id,
        ObjectBounds(*bounds),
        RGBA(*foreground),
        RGBA(*background),
        attributes,
        text,
    )


def _region(draws, *, logical=(0, 0, COLS, ROWS), clip=None, region_id=1):
    clipped = clip is not None
    return RetainedRegionDraw(
        1,
        1,
        region_id,
        *logical,
        *(clip if clipped else (0, 0, 0, 0)),
        region_id,
        clipped,
        tuple(draws),
    )


def _plane(*regions):
    return RetainedDrawPlane(True, True, tuple(regions))


OPAQUE = (240, 240, 240, 255)
INK = (250, 200, 40, 255)
GLASS = (30, 30, 60, 128)

SYNTHETIC_PLANES = {
    # Full rows, half rows and uncovered rows beside one another.
    "partial-rows": _plane(_region([
        _run(1, (0, 0, COLS, 2), INK, OPAQUE, "full rows"),
        _run(2, (0, 3, COLS // 2, 1), INK, OPAQUE, "left half"),
        _run(3, (COLS // 2 + 1, 5, 7, 2), INK, OPAQUE, "island"),
    ])),
    # A translucent fill reveals CELL and never covers; faint ink over an
    # opaque fill still does.
    "translucent": _plane(_region([
        _run(1, (0, 0, COLS, ROWS // 2), INK, GLASS, "glass over cell"),
        _run(2, (2, 6, 10, 2), (250, 250, 250, 90), OPAQUE, "faint ink"),
    ])),
    # REVERSE swaps the painted background: an opaque foreground covers, a
    # translucent one does not.
    "reverse": _plane(_region([
        _run(1, (0, 1, 12, 2), OPAQUE, GLASS, "reversed opaque", ATTR_REVERSE),
        _run(2, (12, 1, 12, 2), GLASS, OPAQUE, "reversed glass", ATTR_REVERSE),
    ])),
    # The region clip bounds the fill; cells outside it keep CELL.
    "clipped-region": _plane(_region(
        [_run(1, (0, 0, COLS, ROWS), INK, OPAQUE, "clipped everywhere")],
        clip=(3, 2, 9, 5),
    )),
    # A nested region placed off the origin and partly outside the screen.
    "offset-region": _plane(
        _region([_run(1, (0, 0, 6, 3), INK, OPAQUE, "edge")],
                logical=(COLS - 5, ROWS - 2, 6, 3), region_id=2),
        _region([_run(2, (1, 1, 4, 2), INK, OPAQUE, "mid")],
                logical=(4, 3, 8, 4), region_id=3),
    ),
    # Text shorter and longer than the run's cells makes uneven slots, and
    # every attribute takes either the batched or the per-slot path.
    "attributes": _plane(_region([
        _run(1, (0, 0, 20, 1), INK, OPAQUE, "short", ATTR_BOLD),
        _run(2, (0, 1, 5, 1), INK, OPAQUE, "much longer text", ATTR_BOLD),
        _run(3, (0, 2, COLS, 1), INK, OPAQUE, "under lined", ATTR_UNDERLINE),
        _run(4, (0, 3, COLS, 1), INK, OPAQUE, "struck out", ATTR_STRIKE),
        _run(5, (0, 4, COLS, 1), INK, OPAQUE, "italic WMW", ATTR_ITALIC),
        _run(6, (0, 5, COLS, 1), INK, OPAQUE, "dim text", ATTR_DIM),
        _run(7, (0, 6, COLS, 1), (250, 200, 40, 160), OPAQUE, "alpha ink"),
        _run(8, (0, 7, COLS, 1), INK, OPAQUE, "WWWWWWWWWWWWWWWWWWWWWWWW", ATTR_BOLD),
        _run(9, (0, 8, 12, 2), INK, OPAQUE, "tall slots"),
    ])),
}


@pytest.mark.parametrize("seed", range(3))
@pytest.mark.parametrize("name", sorted(SYNTHETIC_PLANES))
def test_synthetic_partial_coverage_composes_identically(monkeypatch, font, name, seed):
    _assert_exact(monkeypatch, _terminal(seed), SYNTHETIC_PLANES[name], font)


def test_coverage_counts_only_whole_cells_under_opaque_glyph_fills(font):
    cell_w, cell_h = _cell_size(font)
    covered = opaque_cell_coverage(
        pygame, SYNTHETIC_PLANES["partial-rows"], COLS, ROWS, cell_w, cell_h
    )
    rows = [covered[row * COLS:(row + 1) * COLS] for row in range(ROWS)]
    assert rows[0] == rows[1] == b"\x01" * COLS
    assert rows[2] == b"\x00" * COLS
    assert rows[3] == b"\x01" * (COLS // 2) + b"\x00" * (COLS - COLS // 2)
    assert rows[5] == rows[6] == (
        b"\x00" * (COLS // 2 + 1) + b"\x01" * 7 + b"\x00" * (COLS - COLS // 2 - 8)
    )
    # Glass never covers; faint ink over an opaque fill still does.
    translucent = opaque_cell_coverage(
        pygame, SYNTHETIC_PLANES["translucent"], COLS, ROWS, cell_w, cell_h)
    assert not any(translucent[:6 * COLS])
    assert translucent[6 * COLS:8 * COLS] == 2 * (
        b"\x00" * 2 + b"\x01" * 10 + b"\x00" * (COLS - 12))
    reverse = opaque_cell_coverage(
        pygame, SYNTHETIC_PLANES["reverse"], COLS, ROWS, cell_w, cell_h)
    assert reverse[COLS:COLS + 12] == b"\x01" * 12
    assert reverse[COLS + 12:2 * COLS] == b"\x00" * 12
    clipped = opaque_cell_coverage(
        pygame, SYNTHETIC_PLANES["clipped-region"], COLS, ROWS, cell_w, cell_h)
    assert sum(clipped) == 9 * 5
    assert clipped[2 * COLS + 3] and not clipped[2 * COLS + 2]


def test_skipped_cells_are_exactly_the_covered_cells_whose_glyphs_fit(font):
    cell_w, cell_h = _cell_size(font)
    terminal = _terminal(7)
    plane = SYNTHETIC_PLANES["partial-rows"]
    covered = opaque_cell_coverage(pygame, plane, COLS, ROWS, cell_w, cell_h)
    rendered = []

    class CountingFont:
        def render(self, text, antialias, color):
            rendered.append(text)
            return font.render(text, antialias, color)

    cache = {}
    terminal.render(pygame, CountingFont(), cell_w, cell_h, show_cursor=False,
                    _cache=cache, covered=covered)
    fits = next(value for key, value in cache.items()
                if isinstance(key, tuple) and len(key) == 3
                and key[1:] == (cell_w, cell_h))
    # Every covered glyph was sized exactly once; an overhanging one is drawn.
    for row in range(ROWS):
        for col in range(COLS):
            char, _fg, _bg, attrs = terminal.grid[row][col]
            if covered[row * COLS + col] and char not in ("", " "):
                assert (char, 2 if attrs & 0x100 else 1) in fits
    assert any(not fit for fit in fits.values())
    assert any(fit for fit in fits.values())


# ---------------------------------------------------------------------------
# Repainting one area of the CELL pass
# ---------------------------------------------------------------------------


def _terminal_with_wide_pairs(seed: int) -> VirtualTerminal:
    """A random grid whose wide characters take their lead and continuation."""

    terminal = _terminal(seed)
    randomizer = random.Random(seed + 100)
    with terminal._lock:
        for row in terminal.grid:
            for column in range(0, COLS - 1, 5):
                if randomizer.random() < 0.6:
                    _char, fg, bg, attrs = row[column]
                    row[column] = ("中", fg, bg, attrs | ATTR_WIDE)
                    row[column + 1] = ("", fg, bg, attrs | ATTR_CONTINUATION)
    return terminal


def _areas(randomizer, bounds, count: int):
    """Pixel rectangles that cut cells, wide pairs and the frame's edges."""

    for _ in range(count):
        left = randomizer.randrange(-5, bounds.width)
        top = randomizer.randrange(-5, bounds.height)
        area = pygame.Rect(left, top, randomizer.randrange(1, bounds.width // 2),
                           randomizer.randrange(1, bounds.height // 2))
        yield area.clip(bounds)


def _repaint(terminal, full, font, cell_size, area, cache, covered=None):
    frame = full.copy()
    frame.set_clip(area)
    frame.fill(VirtualTerminal._DEFAULT_BG)
    terminal.paint_area(pygame, frame, font, *cell_size, area,
                        _cache=cache, covered=covered)
    frame.set_clip(None)
    return pygame.image.tobytes(frame, "RGBA")


@pytest.fixture(scope="module")
def big_font():
    pygame.font.init()
    # Drawn into the ordinary font's cells, its glyphs reach more than two
    # cells right and more than one cell down.
    return pygame.font.Font(None, 48)


@pytest.mark.parametrize("big", (False, True))
@pytest.mark.parametrize("seed", range(4))
def test_repainting_any_cell_area_gives_the_full_render(font, big_font, big, seed):
    cell_size = _cell_size(font)
    paint_font = big_font if big else font
    terminal = _terminal_with_wide_pairs(seed)
    covered = opaque_cell_coverage(
        pygame, SYNTHETIC_PLANES["partial-rows"], COLS, ROWS, *cell_size
    )
    randomizer = random.Random(seed)
    for coverage in (None, covered):
        cache = {}
        full = terminal.render(pygame, paint_font, *cell_size, show_cursor=False,
                               _cache=cache, covered=coverage)
        expected = pygame.image.tobytes(full, "RGBA")
        for area in _areas(randomizer, full.get_rect(), 40):
            assert _repaint(
                terminal, full, paint_font, cell_size, area, cache, coverage
            ) == expected, area


def test_an_area_repaint_needs_the_reach_of_glyphs_beside_it(font, big_font):
    """Glyphs that reach past two cells right and below their row are only
    repainted into an area through the recorded glyph extent."""

    cell_size = _cell_size(font)
    terminal = _terminal_with_wide_pairs(0)
    cache = {}
    full = terminal.render(pygame, big_font, *cell_size, show_cursor=False, _cache=cache)
    expected = pygame.image.tobytes(full, "RGBA")
    widest, tallest = next(value for key, value in cache.items() if not isinstance(key, tuple))
    assert widest > 2 * cell_size[0] and tallest > cell_size[1]
    forgetful = {key: value for key, value in cache.items() if isinstance(key, tuple)}
    areas = list(_areas(random.Random(1), full.get_rect(), 80))
    assert all(
        _repaint(terminal, full, big_font, cell_size, area, cache) == expected
        for area in areas
    )
    assert any(
        _repaint(terminal, full, big_font, cell_size, area, forgetful) != expected
        for area in areas
    )
