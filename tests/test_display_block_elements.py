"""Block elements are painted as exact rectangles, not font glyphs.

A picture drawn in CELL from half blocks and quadrants must meet at cell
edges with no gap a glyph's side bearings would leave.
"""

from __future__ import annotations

import pytest

pygame = pytest.importorskip("pygame")

from display import VirtualTerminal  # noqa: E402

CELL_W, CELL_H = 8, 16
FG = (250, 200, 100)
BG = (10, 20, 30)


@pytest.fixture(scope="module")
def font():
    pygame.font.init()
    return pygame.font.Font(None, 18)


def _render(font, text: str):
    terminal = VirtualTerminal(cols=len(text), rows=1)
    terminal.write(f"\x1b[38;2;{';'.join(map(str, FG))}m"
                   f"\x1b[48;2;{';'.join(map(str, BG))}m{text}".encode())
    return terminal.render(pygame, font, CELL_W, CELL_H, show_cursor=False)


def _cell(surface, column: int) -> list[list[tuple[int, int, int]]]:
    return [
        [tuple(surface.get_at((column * CELL_W + x, y)))[:3] for x in range(CELL_W)]
        for y in range(CELL_H)
    ]


def _filled(left: int, top: int, right: int, bottom: int):
    """The expected cell: FG inside the box given in eighths, BG elsewhere."""

    return [
        [
            FG if left * CELL_W // 8 <= x < right * CELL_W // 8
            and top * CELL_H // 8 <= y < bottom * CELL_H // 8 else BG
            for x in range(CELL_W)
        ]
        for y in range(CELL_H)
    ]


@pytest.mark.parametrize(
    ("character", "box"),
    (
        ("▀", (0, 0, 8, 4)),  # upper half
        ("▄", (0, 4, 8, 8)),  # lower half
        ("█", (0, 0, 8, 8)),  # full block
        ("▌", (0, 0, 4, 8)),  # left half
        ("▐", (4, 0, 8, 8)),  # right half
        ("▁", (0, 7, 8, 8)),  # lower one eighth
        ("▕", (7, 0, 8, 8)),  # right one eighth
        ("▘", (0, 0, 4, 4)),  # upper left quadrant
        ("▗", (4, 4, 8, 8)),  # lower right quadrant
    ),
)
def test_a_block_element_fills_exactly_its_part_of_the_cell(font, character, box):
    surface = _render(font, character)
    assert _cell(surface, 0) == _filled(*box)


def test_quadrant_pairs_and_shades_are_exact(font):
    surface = _render(font, "▚▒")
    upper_left = _filled(0, 0, 4, 4)
    lower_right = _filled(4, 4, 8, 8)
    assert _cell(surface, 0) == [
        [FG if FG in (a, b) else BG for a, b in zip(row_a, row_b)]
        for row_a, row_b in zip(upper_left, lower_right)
    ]
    medium = tuple((f * 128 + b * 127) // 255 for f, b in zip(FG, BG))
    assert _cell(surface, 1) == [[medium] * CELL_W for _ in range(CELL_H)]


def test_adjacent_full_blocks_meet_with_no_gap(font):
    surface = _render(font, "█" * 3)
    assert all(
        tuple(surface.get_at((x, y)))[:3] == FG
        for x in range(3 * CELL_W)
        for y in range(CELL_H)
    )
