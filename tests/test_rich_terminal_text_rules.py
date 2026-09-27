"""The terminal's implementation of the shared text contract, APT-1-TEXT.

Characters, widths, and bidi are checked against Unicode's own conformance
files for the pinned Unicode 15.1.0 data; joining and row layout against
the contract's rules.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

import generate_text_data  # noqa: E402
from rich_terminal import text_rules as tr  # noqa: E402


def _ucd(name: str) -> Path:
    try:
        return generate_text_data.ucd_file(generate_text_data.DEFAULT_UCD, name)
    except generate_text_data.UcdError as exc:  # pragma: no cover - host data
        pytest.skip(f"pinned Unicode 15.1.0 data unavailable: {exc}")


def _data_lines(path: Path):
    for line in path.read_text(encoding="utf-8").splitlines():
        body = line.split("#", 1)[0].strip()
        if body:
            yield body


def test_generated_text_data_is_current() -> None:
    _ucd("UnicodeData.txt")
    assert generate_text_data.main(["--check"]) == 0


def test_every_grapheme_break_test_case() -> None:
    failures = []
    for body in _data_lines(_ucd("auxiliary/GraphemeBreakTest.txt")):
        scalars, breaks = [], []
        for token in body.split():
            if token == "\u00f7":
                breaks.append(len(scalars))
            elif token != "\u00d7":
                scalars.append(int(token, 16))
        spans = tr.segment(scalars)
        got = [start for start, _ in spans] + [len(scalars)]
        if got != breaks:
            failures.append(body)
    assert failures == []


def test_every_bidi_test_case() -> None:
    names = {name: index for index, name in enumerate(tr.BIDI_NAMES)}
    levels_line = order_line = None
    failures = []
    total = 0
    for body in _data_lines(_ucd("BidiTest.txt")):
        if body.startswith("@Levels:"):
            levels_line = body.split(":", 1)[1].split()
            continue
        if body.startswith("@Reorder:"):
            order_line = [int(v) for v in body.split(":", 1)[1].split()]
            continue
        text, bits = body.split(";")
        classes = [names[name] for name in text.split()]
        bits = int(bits, 16)
        for bit, direction in (
            (1, tr.DIRECTION_AUTO), (2, tr.DIRECTION_LTR), (4, tr.DIRECTION_RTL),
        ):
            if bits & bit:
                total += 1
                levels, _ = tr.bidi_levels(classes, direction)
                shown = ["x" if level is None else str(level) for level in levels]
                if shown != levels_line or tr.visual_order(levels) != order_line:
                    failures.append((text, direction))
    assert total == 770_241
    assert failures == []


def test_every_bidi_character_test_case() -> None:
    directions = {0: tr.DIRECTION_LTR, 1: tr.DIRECTION_RTL, 2: tr.DIRECTION_AUTO}
    failures = []
    total = 0
    for body in _data_lines(_ucd("BidiCharacterTest.txt")):
        fields = body.split(";")
        scalars = [int(v, 16) for v in fields[0].split()]
        classes = [tr.bidi_class(tr.props(cp)) for cp in scalars]
        levels, paragraph = tr.bidi_levels(classes, directions[int(fields[1])], scalars)
        shown = ["x" if level is None else str(level) for level in levels]
        order = [int(v) for v in fields[4].split()]
        total += 1
        if (
            paragraph != int(fields[2])
            or shown != fields[3].split()
            or tr.visual_order(levels) != order
        ):
            failures.append(fields[0])
    assert total == 91_707
    assert failures == []


@pytest.mark.parametrize(
    ("text", "widths"),
    [
        ("abc", [1, 1, 1]),
        ("\u4e2d\u6587", [2, 2]),
        ("e\u0301", [1]),                                # combining accent
        ("\U0001F1EF\U0001F1F5\U0001F1FA", [2, 1]),      # a flag, then a lone indicator
        ("\u2764\ufe0f\u2764", [2, 1]),                  # emoji and text presentation
        ("\U0001F44D\U0001F3FD\u261d\U0001F3FD", [2, 2]),  # modifiers
        ("\U0001F468\u200d\U0001F469\u200d\U0001F467", [2]),  # one family
        ("#\ufe0f\u20e3", [2]),                          # keycap
        ("\u0301", [1]),                                 # mark with no base
        ("\u200b\u200f\u202e", [0, 0, 0]),              # default ignorables
        ("\u1100\u1161\u11a8", [2]),                     # Hangul jamo syllable
        ("\uff8a\uff21", [1, 2]),                        # halfwidth and fullwidth
        ("\u3099", [1]),                                 # combining kana alone
    ],
)
def test_character_widths(text: str, widths: list[int]) -> None:
    assert [tr.char_width(ch) for ch in tr.characters(text)] == widths


def test_invalid_input_becomes_replacement_characters() -> None:
    assert tr.display_scalars(b"a\xe2\x82b\xff") == [0x61, 0xFFFD, 0x62, 0xFFFD]
    assert tr.display_scalars("a\tb\x01\u2028") == [0x61, 0xFFFD, 0x62, 0xFFFD, 0xFFFD]
    assert tr.display_scalars("a\tb", keep_tab=True) == [0x61, 9, 0x62]


def test_arabic_letters_join_by_joining_type() -> None:
    word = [ord(ch) for ch in "\u0645\u0631\u062d\u0628\u0627"]
    forms = tr.joining_forms(word)
    assert forms == [
        tr.FORM_INITIAL, tr.FORM_FINAL, tr.FORM_INITIAL, tr.FORM_MEDIAL, tr.FORM_FINAL,
    ]
    shown = [tr.arabic_form(cp, form) for cp, form in zip(word, forms)]
    assert shown == [0xFEE3, 0xFEAE, 0xFEA3, 0xFE92, 0xFE8E]
    # A zero-width non-joiner stops joining; a transparent mark does not.
    assert tr.joining_forms([0x628, 0x200C, 0x628]) == [
        tr.FORM_ISOLATED, tr.FORM_ISOLATED, tr.FORM_ISOLATED,
    ]
    assert tr.joining_forms([0x628, 0x64E, 0x628]) == [
        tr.FORM_INITIAL, tr.FORM_ISOLATED, tr.FORM_FINAL,
    ]


def _visual(layout: tr.RowLayout) -> str:
    return "".join(placed.text for placed in layout.characters)


def test_row_layout_orders_whole_characters_visually() -> None:
    mixed = tr.layout_row("abc \u05d0\u05d1\u05d2!")
    assert mixed.paragraph_level == 0
    assert _visual(mixed) == "abc \u05d2\u05d1\u05d0!"
    rtl = tr.layout_row("\u05e9\u05dc\u05d5\u05dd, world!")
    assert rtl.paragraph_level == 1
    assert _visual(rtl) == "!world ,\u05dd\u05d5\u05dc\u05e9"
    forced = tr.layout_row("abc \u05d0", tr.DIRECTION_RTL)
    assert _visual(forced) == "\u05d0 abc"
    # Clusters keep their scalars in logical order when reversed.
    marks = tr.layout_row("\u05d0\u05b8\u05d1")
    assert [placed.text for placed in marks.characters] == ["\u05d1", "\u05d0\u05b8"]


def test_row_layout_mirrors_and_joins() -> None:
    mirrored = tr.layout_row("\u05d0(\u05d1)")
    assert _visual(mirrored) == "(\u05d1)\u05d0"
    assert [placed.text for placed in mirrored.characters] == ["(", "\u05d1", ")", "\u05d0"]
    arabic = tr.layout_row("\u0645\u0631\u062d\u0628\u0627")
    assert _visual(arabic) == "\ufe8e\ufe92\ufea3\ufeae\ufee3"


def test_row_layout_widths_and_columns() -> None:
    layout = tr.layout_row("a\u4e2d\U0001F1EF\U0001F1F5e\u0301\u200bz")
    assert [(p.text, p.width, p.column) for p in layout.characters] == [
        ("a", 1, 0),
        ("\u4e2d", 2, 1),
        ("\U0001F1EF\U0001F1F5", 2, 3),
        ("e\u0301", 1, 5),
        ("z", 1, 6),
    ]
    assert layout.width == 7
    assert layout.length == 8
    # Section 9.1: a point on a character names its start; past the end,
    # the row's end.
    assert [layout.position_at_column(c) for c in range(9)] == [0, 1, 1, 2, 2, 4, 7, 8, 8]


def test_row_layout_keeps_tabs_as_one_cell_on_request() -> None:
    layout = tr.layout_row("a\tb", keep_tab=True)
    assert [(p.text, p.width) for p in layout.characters] == [("a", 1), ("\t", 1), ("b", 1)]
    assert tr.layout_row("a\tb").characters[1].text == "\ufffd"


def test_caret_belongs_to_the_character_that_starts_at_it() -> None:
    layout = tr.layout_row("ab\u05d0\u05d1")
    # Visual: a b [bet] [alef]; the caret before alef (offset 2) belongs to
    # alef, which sits at column 3.
    assert layout.caret_character(2).column == 3
    assert layout.caret_character(3).column == 2
    assert layout.caret_character(4) is None
