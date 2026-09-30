"""Wide characters and characters of several scalars in terminal cells.

APT-1-TEXT Sections 3 to 6 put one character in one cell, or in a lead and a
continuation cell when it is wide.  These tests cover the simulator's ANSI
terminal, the CELL-1 view it shows, the viewer's snapshot wire, and the
GLYPH_RUN slots of the rich viewer.
"""

from __future__ import annotations

import pytest

from display import ATTR_CONTINUATION, ATTR_WIDE, VirtualTerminal
from rich_terminal.cell_model import (
    ATTRIBUTE_CLUSTER,
    ATTRIBUTE_CONTINUATION,
    ATTRIBUTE_WIDE,
    Cell,
    Cursor,
    TerminalView,
)
from rich_terminal.pygame_view import _glyph_slots
from rich_terminal.retained_view import DisplayScope, RetainedDrawPlane
from shared.session import (
    OutputSnapshotRows,
    TerminalCell,
    TerminalDisplayOffer,
    TerminalSnapshot,
)
from shared_session import (
    WireRowRuns,
    display_offer_from_wire,
    display_offer_to_wire,
    snapshot_from_wire,
    snapshot_to_wire,
)


def _row(terminal: VirtualTerminal, row: int = 0) -> list[tuple[str, int]]:
    return [(cell[0], cell[3] & (ATTR_WIDE | ATTR_CONTINUATION)) for cell in terminal.grid[row]]


def _feed(text: str | bytes, cols: int = 8, rows: int = 2) -> VirtualTerminal:
    terminal = VirtualTerminal(cols=cols, rows=rows)
    terminal.write(text.encode("utf-8") if isinstance(text, str) else text)
    return terminal


W, C = ATTR_WIDE, ATTR_CONTINUATION


def test_scalars_join_their_character_and_wide_ones_take_two_cells() -> None:
    terminal = _feed("e\u0301\u4e2dx")
    assert _row(terminal)[:5] == [("e\u0301", 0), ("\u4e2d", W), ("", C), ("x", 0), (" ", 0)]
    assert terminal.cx == 4


def test_emoji_sequences_and_flags_are_one_wide_character() -> None:
    family = "\U0001F468\u200d\U0001F469\u200d\U0001F467"
    terminal = _feed(family + "\U0001F1EF\U0001F1F5!")
    assert _row(terminal)[:5] == [(family, W), ("", C), ("\U0001F1EF\U0001F1F5", W), ("", C), ("!", 0)]


def test_emoji_presentation_widens_a_character_in_place() -> None:
    terminal = _feed("\u2764")
    assert _row(terminal)[:2] == [("\u2764", 0), (" ", 0)]
    assert terminal.cx == 1
    terminal.write("\ufe0fz".encode())
    assert _row(terminal)[:3] == [("\u2764\ufe0f", W), ("", C), ("z", 0)]


def test_a_wide_character_never_spans_rows() -> None:
    terminal = _feed("abc\u4e2d", cols=4)
    assert _row(terminal, 0) == [("a", 0), ("b", 0), ("c", 0), (" ", 0)]
    assert _row(terminal, 1)[:2] == [("\u4e2d", W), ("", C)]


def test_zero_width_characters_take_no_cell_and_lone_marks_take_one() -> None:
    terminal = _feed("\u200ba\u0301")
    assert _row(terminal)[:2] == [("a\u0301", 0), (" ", 0)]
    # An escape sequence ends the character, so the mark stands alone.
    terminal = _feed(b"e\x1b[m\xcc\x81")
    assert _row(terminal)[:3] == [("e", 0), ("\u0301", 0), (" ", 0)]


def test_ill_formed_utf8_shows_one_replacement_per_maximal_subpart() -> None:
    terminal = _feed(b"\xe0\x80A\xf0\x9f\x98B\xc2\x85")
    assert [cell[0] for cell in terminal.grid[0][:6]] == [
        "\ufffd", "\ufffd", "A", "\ufffd", "B", "\ufffd",
    ]


def test_overwriting_or_erasing_half_a_pair_blanks_the_other_half() -> None:
    terminal = _feed("\u4e2d\u6587")
    terminal.write(b"\x1b[1;2Hx")
    assert _row(terminal)[:4] == [(" ", 0), ("x", 0), ("\u6587", W), ("", C)]
    terminal.write(b"\x1b[1;4H\x1b[K")
    assert _row(terminal)[:4] == [(" ", 0), ("x", 0), (" ", 0), (" ", 0)]


def test_resize_cuts_no_pair() -> None:
    terminal = _feed("ab\u4e2d", cols=4)
    terminal.resize(3, 2)
    assert _row(terminal) == [("a", 0), ("b", 0), (" ", 0)]


def _view(cells) -> TerminalView:
    return TerminalView(
        attachment_epoch=1,
        session_id=1,
        presentation_epoch=1,
        revision=1,
        cols=len(cells[0]),
        rows=len(cells),
        cells=tuple(tuple(row) for row in cells),
        dirty_spans=(),
        cursor=Cursor(0, 0, False),
    )


def test_cell_view_snapshots_carry_whole_characters_and_true_columns() -> None:
    view = _view([[
        Cell(0x4E2D, 7, 0, ATTRIBUTE_WIDE | 0x40),
        Cell(0, 7, 0, ATTRIBUTE_CONTINUATION | 0x40),
        Cell(ord("e"), 7, 0, ATTRIBUTE_CLUSTER | 1, (0x301,)),
        Cell(ord("o"), 7, 0),
        Cell(ord("k"), 7, 0),
    ]])
    snapshot = OutputSnapshotRows().snapshot(view)
    assert [(cell.char, cell.attrs) for cell in snapshot.cells[0]] == [
        ("\u4e2d", 0x80 | ATTR_WIDE),
        ("", 0x80 | ATTR_CONTINUATION),
        ("e\u0301", 1),
        ("o", 0),
        ("k", 0),
    ]
    assert snapshot.lines() == ["\u4e2de\u0301ok"]
    assert snapshot.find("ok") == [(0, 3)]
    assert snapshot.find("\u0301") == [(0, 2)]
    assert snapshot.row_text(0, 2, 4) == "e\u0301o"


def _view_of_rows(rows) -> TerminalView:
    """A view holding exactly these row objects, as a publication shares them."""

    return TerminalView(
        attachment_epoch=1,
        session_id=1,
        presentation_epoch=1,
        revision=1,
        cols=len(rows[0]),
        rows=len(rows),
        cells=tuple(rows),
        dirty_spans=(),
        cursor=Cursor(0, 0, False),
    )


def test_cell_view_snapshots_convert_only_rows_the_model_replaced() -> None:
    top = (
        Cell(0x4E2D, 7, 0, ATTRIBUTE_WIDE),
        Cell(0, 7, 0, ATTRIBUTE_CONTINUATION),
        Cell(ord("e"), 2, 4, ATTRIBUTE_CLUSTER | 1, (0x301,)),
    )
    blank = tuple(Cell(ord(" "), 7, 0) for _ in range(3))
    typed = (Cell(ord("x"), 1, 0, 0x40), Cell(ord(" "), 7, 0), Cell(ord(" "), 7, 0))
    rows = OutputSnapshotRows()

    first = rows.snapshot(_view_of_rows((top, blank, blank)))
    assert first.cells[1] is first.cells[2]
    second = rows.snapshot(_view_of_rows((top, typed, blank)))
    assert second == OutputSnapshotRows().snapshot(_view_of_rows((top, typed, blank)))
    assert second.cells[0] is first.cells[0]
    assert second.cells[2] is first.cells[2]
    assert second.cells[1] != first.cells[1]

    # Only the same row object is reused; an equal row is converted anew.
    equal = tuple(
        Cell(cell.codepoint, cell.foreground, cell.background, cell.attributes, cell.extras)
        for cell in top
    )
    third = rows.snapshot(_view_of_rows((equal, typed, blank)))
    assert third == second
    assert third.cells[0] is not second.cells[0]
    assert third.cells[1] is second.cells[1]


def test_snapshot_wire_reuses_row_runs_and_joins_runs_across_rows(monkeypatch) -> None:
    import shared_session

    def row(text: str):
        return tuple(TerminalCell(char, (1, 2, 3), (4, 5, 6), 0) for char in text)

    def snapshot(*rows) -> TerminalSnapshot:
        return TerminalSnapshot(3, len(rows), rows, cursor_col=0, cursor_row=0,
                                cursor_visible=False, alternate_screen=False)

    encoded = []
    original = shared_session._row_runs

    def counting(cells):
        encoded.append(cells)
        return original(cells)

    monkeypatch.setattr(shared_session, "_row_runs", counting)
    ends, spaces, begins = row("a  "), row("   "), row("  b")
    memo = WireRowRuns()

    first = snapshot(ends, spaces, begins)
    wire = snapshot_to_wire(first, memo)
    blue, red = 0x010203, 0x040506
    assert wire["runs"] == [[1, "a", blue, red, 0], [7, " ", blue, red, 0],
                            [1, "b", blue, red, 0]]
    assert snapshot_from_wire(wire) == first

    # Only the replaced row is encoded; the others keep their runs, and runs
    # still join across row ends exactly as a fresh encoding joins them.
    typed = row("x  ")
    second = snapshot(ends, typed, begins, spaces)
    encoded.clear()
    wire = snapshot_to_wire(second, memo)
    assert [id(cells) for cells in encoded] == [id(typed)]
    assert wire == snapshot_to_wire(second)
    assert wire["runs"][2:4] == [[1, "x", blue, red, 0], [4, " ", blue, red, 0]]

    # Only the same row object is reused; an equal row is encoded anew.
    equal = row("a  ")
    third = snapshot(equal, typed, begins, spaces)
    encoded.clear()
    wire = snapshot_to_wire(third, memo)
    assert [id(cells) for cells in encoded] == [id(equal)]
    assert wire == snapshot_to_wire(third)


def _snapshot(chars_and_attrs) -> TerminalSnapshot:
    row = tuple(
        TerminalCell(char, (1, 2, 3), (4, 5, 6), attrs)
        for char, attrs in chars_and_attrs
    )
    return TerminalSnapshot(
        len(row), 1, (row,), cursor_col=0, cursor_row=0,
        cursor_visible=False, alternate_screen=False,
    )


def test_snapshot_wire_round_trips_clusters_and_pairs() -> None:
    snapshot = _snapshot([
        ("\U0001F468\u200d\U0001F469", ATTR_WIDE), ("", ATTR_CONTINUATION),
        ("a\u0301", 8), (" ", 0),
    ])
    assert snapshot_from_wire(snapshot_to_wire(snapshot)) == snapshot


@pytest.mark.parametrize(
    "cells",
    [
        [("\u4e2d", ATTR_WIDE), ("x", 0)],
        [("x", 0), ("", ATTR_CONTINUATION)],
        [("x", 0), ("\u4e2d", ATTR_WIDE)],
        [("", 0), ("x", 0)],
        [("\u4e2d", ATTR_WIDE), ("y", ATTR_CONTINUATION)],
    ],
)
def test_snapshot_wire_rejects_broken_pairs_and_empty_leads(cells) -> None:
    with pytest.raises(ValueError):
        snapshot_from_wire(snapshot_to_wire(_snapshot(cells)))


def test_glyph_run_characters_take_their_width_in_slots() -> None:
    slots, total = _glyph_slots("a\u4e2de\u0301\u200b\U0001F1EF\U0001F1F5")
    assert list(slots) == [
        ("a", 0, 1),
        ("\u4e2d", 1, 2),
        ("e\u0301", 3, 1),
        ("\u200b", 4, 0),
        ("\U0001F1EF\U0001F1F5", 4, 2),
    ]
    assert total == 6
    slots, total = _glyph_slots("ab")
    assert (list(slots), total) == ([("a", 0, 1), ("b", 1, 1)], 2)


def _cells_offer(offer_id: int, *rows, cursor=(0, 0, False)) -> TerminalDisplayOffer:
    return TerminalDisplayOffer(
        offer_id,
        DisplayScope(1, 2, 3, offer_id, 0, offer_id, offer_id),
        TerminalSnapshot(len(rows[0]), len(rows), rows, cursor_col=cursor[1],
                         cursor_row=cursor[0], cursor_visible=cursor[2],
                         alternate_screen=False),
        RetainedDrawPlane(False, False, ()),
    )


def _text_row(*cells) -> tuple[TerminalCell, ...]:
    return tuple(TerminalCell(char, (1, 2, 3), (4, 5, 6), attrs) for char, attrs in cells)


def test_offer_changes_carry_only_the_cell_rows_that_differ() -> None:
    top = _text_row(("a", 0), ("b", 0), (" ", 0), (" ", 0))
    wide = _text_row(("\u4e2d", W), ("", C), ("x", 0), (" ", 0))
    blank = _text_row(*[(" ", 0)] * 4)
    base = _cells_offer(4, top, blank, blank)
    moved = _text_row((" ", 0), ("\u4e2d", W), ("", C), ("e\u0301", 0))
    offer = _cells_offer(5, top, moved, _text_row(*[(" ", 0)] * 4),
                         cursor=(1, 3, True))

    for memo in (None, WireRowRuns()):
        wire = display_offer_to_wire(offer, memo, base)
        blue, red = 0x010203, 0x040506
        # The equal third row is a different object but is not sent.
        assert wire["cell"]["changed_rows"] == [[1, [
            [1, " ", blue, red, 0], [1, "\u4e2d", blue, red, W],
            [1, "", blue, red, C], [1, "e\u0301", blue, red, 0],
        ]]]
        assert wire["cell"]["cursor"] == [1, 3, True]
        rebuilt = display_offer_from_wire(wire, base)
        assert rebuilt == offer
        assert rebuilt.cell.cells[0] is top and rebuilt.cell.cells[2] is blank

    wire = display_offer_to_wire(_cells_offer(6, top, wide, blank), base=base)
    assert display_offer_from_wire(wire, base).cell.cells[1] == wide


@pytest.mark.parametrize(
    ("mutate", "match"),
    [
        (lambda cell: cell["changed_rows"].append([0, cell["changed_rows"][0][1]]),
         "increasing order"),
        (lambda cell: cell["changed_rows"][0].__setitem__(0, 3), "snapshot changed row 0 index"),
        (lambda cell: cell["changed_rows"][0][1].pop(), "has 3 cells"),
        (lambda cell: cell["changed_rows"][0][1][1].__setitem__(4, 0), "breaks a wide pair"),
        (lambda cell: cell.update(cols=5), "base's geometry"),
    ],
)
def test_offer_changes_reject_rows_that_cannot_rebuild_the_screen(mutate, match) -> None:
    top = _text_row(("a", 0), ("b", 0), (" ", 0), (" ", 0))
    blank = _text_row(*[(" ", 0)] * 4)
    base = _cells_offer(4, top, blank, blank)
    moved = _text_row((" ", 0), ("\u4e2d", W), ("", C), ("y", 0))
    wire = display_offer_to_wire(_cells_offer(5, top, moved, blank), base=base)
    mutate(wire["cell"])
    with pytest.raises((ValueError, TypeError), match=match):
        display_offer_from_wire(wire, base)

