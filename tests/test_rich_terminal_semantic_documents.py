"""Byte, value and row oracles for DOC1 documents (SEMANTIC-CONTENT-1)."""

from __future__ import annotations

import struct
from dataclasses import replace

import pytest

from rich_terminal import text_rules
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEvent,
    ControlEventKind,
    ControlWireDefinition,
    decode_control_definition,
    encode_control_definition,
    encode_control_event,
)
from rich_terminal.semantic_content import (
    SemanticContentError,
    SemanticContentErrorCode,
    StyleRun,
    TextStyle,
)
from rich_terminal.semantic_documents import (
    DOCUMENT_TAG,
    BlockContainer,
    BlockKind,
    ContainerKind,
    DocumentBlock,
    DocumentContent,
    DocumentFlag,
    block_lines,
    block_row_count,
    block_text_width,
    decode_document_content,
    document_viewport_problem,
    encode_document_content,
    placed_blocks,
    shown_link,
)

QUOTE = BlockContainer(ContainerKind.QUOTE, 2)
FIRST_BULLET = BlockContainer(ContainerKind.BULLET_ITEM, 2, item_start=True)
BULLET = BlockContainer(ContainerKind.BULLET_ITEM, 2)


def _numbered(number: int, start: bool = True) -> BlockContainer:
    return BlockContainer(ContainerKind.NUMBERED_ITEM, 4, number, start)


def _guide(viewport_row: int = 0) -> DocumentContent:
    """A short document with every block kind, read at width 20."""

    return DocumentContent(
        content_revision=5,
        flags=DocumentFlag(0),
        blocks=(
            DocumentBlock(1, BlockKind.HEADING, "Getting started", level=1),
            DocumentBlock(
                2,
                BlockKind.PARAGRAPH,
                "Read the notes first, then the pictures.",
                runs=(StyleRun(9, 5, TextStyle.LINK), StyleRun(31, 8, TextStyle.EMPHASIS)),
                rows_before=1,
            ),
            DocumentBlock(3, BlockKind.PARAGRAPH, "A quoted line", rows_before=1,
                          containers=(QUOTE,)),
            DocumentBlock(4, BlockKind.PARAGRAPH, "and its second paragraph",
                          rows_before=1, rows_before_inside=1, containers=(QUOTE,)),
            DocumentBlock(5, BlockKind.PARAGRAPH, "one", rows_before=1,
                          containers=(FIRST_BULLET,)),
            DocumentBlock(6, BlockKind.PARAGRAPH, "two, nested", containers=(BULLET, FIRST_BULLET)),
            DocumentBlock(7, BlockKind.PARAGRAPH, "tenth", containers=(_numbered(10),)),
            DocumentBlock(8, BlockKind.CODE, "x = 1\n\nprint(x)   # shown", rows_before=1),
            DocumentBlock(9, BlockKind.RULE, rows_before=1),
            DocumentBlock(10, BlockKind.FIGURE, "A sunset", rows_before=1, picture_rows=5),
            DocumentBlock(11, BlockKind.PAGE, "iv", rows_before=1),
        ),
        viewport_row=viewport_row,
    )


def test_doc1_has_exact_headers_and_round_trips() -> None:
    content = _guide()
    raw = encode_document_content(content)
    assert len(raw) == content.wire_bytes
    assert struct.unpack_from("<IHHQHHIII", raw) == (DOCUMENT_TAG, 1, 0, 5, 0, 0, 11, 0, 0)
    # The first block: key, kind, level, rows before, inside, picture rows,
    # container count, reserved, text bytes, run count.
    assert struct.unpack_from("<QHHHHIHHII", raw, 32) == (1, 2, 1, 0, 0, 0, 0, 0, 15, 0)
    assert decode_document_content(raw) == content
    assert content.object_slots == 12
    assert content.utf8_bytes == sum(len(block.text.encode()) for block in content.blocks)
    assert content.style_run_count == 2
    assert content.block(7).containers[0].number == 10
    assert content.block(99) is None


def test_containers_and_runs_have_exact_records() -> None:
    block = DocumentBlock(
        3, BlockKind.PARAGRAPH, "q", runs=(StyleRun(0, 1, TextStyle.STRONG),),
        rows_before=2, rows_before_inside=1, containers=(QUOTE, _numbered(12)),
    )
    raw = encode_document_content(DocumentContent(1, DocumentFlag(0), (block,)))
    assert struct.unpack_from("<QHHHHIHHII", raw, 32) == (3, 1, 0, 2, 1, 0, 2, 0, 1, 1)
    assert struct.unpack_from("<BBHI", raw, 64) == (1, 0, 2, 0)
    assert struct.unpack_from("<BBHI", raw, 72) == (3, 1, 4, 12)
    assert raw[80:81] == b"q"
    assert struct.unpack_from("<IIHH", raw, 81) == (0, 1, 7, 0)
    assert len(raw) == 93


@pytest.mark.parametrize(
    "build, message",
    [
        (lambda: DocumentBlock(1, BlockKind.HEADING, "h"), "level is 1 to 6"),
        (lambda: DocumentBlock(1, BlockKind.HEADING, "h", level=7), "level is 1 to 6"),
        (lambda: DocumentBlock(1, BlockKind.PARAGRAPH, "p", level=1), "only a heading"),
        (lambda: DocumentBlock(1, BlockKind.FIGURE, "f"), "picture rows"),
        (lambda: DocumentBlock(1, BlockKind.CODE, "c", picture_rows=2), "picture rows"),
        (lambda: DocumentBlock(1, BlockKind.RULE, "r"), "no text"),
        (lambda: DocumentBlock(1, BlockKind.PAGE, "3", containers=(QUOTE,)), "no container"),
        (lambda: DocumentBlock(1, BlockKind.PAGE, "3", level=1), "only a heading"),
        (lambda: DocumentBlock(1, BlockKind.PARAGRAPH, "tab\there"), "control character"),
        (lambda: DocumentBlock(1, BlockKind.PARAGRAPH, "p", rows_before_inside=1),
         "more containers"),
        (lambda: DocumentBlock(0, BlockKind.PARAGRAPH, "p"), "block_key"),
        (lambda: DocumentBlock(1, BlockKind.PARAGRAPH, "ab",
                               runs=(StyleRun(1, 2, TextStyle.CODE),)), "past the block"),
        (lambda: DocumentBlock(1, BlockKind.PARAGRAPH, "abcd",
                               runs=(StyleRun(0, 2, TextStyle.CODE),
                                     StyleRun(2, 2, TextStyle.CODE))), "touch"),
        (lambda: BlockContainer(ContainerKind.QUOTE, 0), "width"),
        (lambda: BlockContainer(ContainerKind.QUOTE, 2, item_start=True), "only a list item"),
        (lambda: BlockContainer(ContainerKind.BULLET_ITEM, 2, 3), "only a numbered item"),
        (lambda: DocumentContent(1, DocumentFlag(0), (), viewport_row=1), "viewport row zero"),
        (lambda: DocumentContent(1, DocumentFlag(3), ()), "both LTR and RTL"),
        (lambda: DocumentContent(
            1, DocumentFlag(0),
            (DocumentBlock(1, BlockKind.RULE), DocumentBlock(1, BlockKind.RULE))),
         "duplicated"),
    ],
)
def test_value_rules(build, message) -> None:
    with pytest.raises(ValueError, match=message):
        build()


def test_decoder_rejects_every_noncanonical_byte() -> None:
    raw = encode_document_content(_guide())

    def code_of(payload) -> SemanticContentErrorCode:
        with pytest.raises(SemanticContentError) as caught:
            decode_document_content(bytes(payload))
        return caught.value.code

    assert code_of(raw[:31]) is SemanticContentErrorCode.PAYLOAD
    assert code_of(raw[:-1]) is SemanticContentErrorCode.PAYLOAD
    assert code_of(raw + b"\0") is SemanticContentErrorCode.PAYLOAD
    for offset, value, code in (
        (0, 0x31585453, SemanticContentErrorCode.CONSISTENCY),   # STX1's tag
        (4, 2, SemanticContentErrorCode.ENUM),                   # version
        (6, 1, SemanticContentErrorCode.RESERVED),
        (18, 1, SemanticContentErrorCode.RESERVED),
        (28, 1, SemanticContentErrorCode.RESERVED),
    ):
        bad = bytearray(raw)
        if offset in (4, 6, 18):
            struct.pack_into("<H", bad, offset, value)
        else:
            struct.pack_into("<I", bad, offset, value)
        assert code_of(bad) is code
    bad = bytearray(raw)
    struct.pack_into("<H", bad, 16, 4)
    assert code_of(bad) is SemanticContentErrorCode.RESERVED
    struct.pack_into("<H", bad, 16, 3)
    assert code_of(bad) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw)
    struct.pack_into("<I", bad, 20, 1000)
    assert code_of(bad) is SemanticContentErrorCode.PAYLOAD
    # The first block's kind, reserved field and heading level.
    for offset, size, value, code in (
        (40, "<H", 7, SemanticContentErrorCode.ENUM),
        (40, "<H", 9, SemanticContentErrorCode.ENUM),
        (54, "<H", 1, SemanticContentErrorCode.RESERVED),
        (42, "<H", 0, SemanticContentErrorCode.CONSISTENCY),
    ):
        bad = bytearray(raw)
        struct.pack_into(size, bad, offset, value)
        assert code_of(bad) is code
    # A container's kind and flags, in the third block.
    third = 32 + sum(block.wire_bytes for block in _guide().blocks[:2])
    container = third + 32
    bad = bytearray(raw)
    bad[container] = 7
    assert code_of(bad) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw)
    bad[container + 1] = 2
    assert code_of(bad) is SemanticContentErrorCode.RESERVED
    # Text that is not UTF-8 or holds a control character.
    text = container + 8
    bad = bytearray(raw)
    bad[text] = 0xFF
    assert code_of(bad) is SemanticContentErrorCode.SCALAR
    bad[text] = 0x09
    assert code_of(bad) is SemanticContentErrorCode.SCALAR
    # A style run's reserved field, meaning and length, in the second block.
    second = 32 + _guide().blocks[0].wire_bytes
    run = second + 32 + len(_guide().blocks[1].text)
    for offset, size, value, code in (
        (run + 10, "<H", 1, SemanticContentErrorCode.RESERVED),
        (run + 8, "<H", 99, SemanticContentErrorCode.ENUM),
        (run + 4, "<I", 0, SemanticContentErrorCode.CONSISTENCY),
    ):
        bad = bytearray(raw)
        struct.pack_into(size, bad, offset, value)
        assert code_of(bad) is code


def test_rows_follow_the_shared_line_rules() -> None:
    content = _guide()
    width = 20

    def lines(key):
        return [
            (line.start, line.end, line.width)
            for line in block_lines(content, content.block(key), width)
        ]

    def rows(key):
        return block_row_count(content, content.block(key), width)

    assert rows(1) == 1
    # "Read the notes " / "first, then the " / "pictures." at 20 cells; the
    # spaces at a line's end take no cell.
    assert lines(2) == [(0, 15, 14), (15, 31, 15), (31, 40, 9)]
    assert lines(2) == [
        (line.start, line.end, line.width)
        for line in text_rules.layout_lines(content.block(2).text, 0, 20)
    ]
    assert rows(2) == 4
    # A quote takes two cells, so its text has 18.
    assert block_text_width(content.block(3), width) == 18
    assert lines(4) == [(0, 15, 14), (15, 24, 9)]
    assert rows(3) == 2 and rows(4) == 3
    # Nested items take both their containers' cells.
    assert block_text_width(content.block(6), width) == 16
    assert block_text_width(content.block(7), width) == 16
    # Code breaks into hard lines: "print(x)   # shown" is 18 cells, "x = 1"
    # and an empty paragraph are one line each.
    assert lines(8) == [(0, 5, 5), (6, 6, 0), (7, 25, 18)]
    narrow = [
        (line.start, line.end, line.width)
        for line in block_lines(content, content.block(8), 10)
    ]
    assert narrow == [(0, 5, 5), (6, 6, 0), (7, 17, 10), (17, 25, 8)]
    assert rows(9) == 2
    # A figure's picture rows, then its caption's one line.
    assert rows(10) == 1 + 5 + 1
    # A page's label is laid out as a paragraph is: one line here, and one
    # empty line when the label is empty.
    assert lines(11) == [(0, 2, 2)] and rows(11) == 2
    empty = DocumentBlock(12, BlockKind.PAGE)
    assert block_row_count(DocumentContent(1, DocumentFlag(0), (empty,)), empty, 20) == 1
    long = DocumentBlock(13, BlockKind.PAGE, "Appendix the second")
    assert [(line.start, line.end) for line in block_lines(
        DocumentContent(1, DocumentFlag(0), (long,)), long, 10)] == [(0, 9), (9, 19)]
    # A container wider than the root leaves one cell for text.
    wide = DocumentBlock(1, BlockKind.PARAGRAPH, "ab", containers=(BlockContainer(
        ContainerKind.QUOTE, 30),))
    assert block_text_width(wide, 20) == 1
    # A figure with no caption has only its picture rows.
    bare = DocumentBlock(2, BlockKind.FIGURE, picture_rows=3)
    assert block_row_count(DocumentContent(1, DocumentFlag(0), (bare,)), bare, 20) == 3


def test_blocks_are_placed_one_after_another_from_the_viewport_row() -> None:
    content = _guide()
    placed = placed_blocks(content, 20)
    assert [item.block.block_key for item in placed] == list(range(1, 12))
    assert placed[0].top == 0 and placed[0].content_top == 0
    assert placed[1].top == 1 and placed[1].content_top == 2
    for before, after in zip(placed, placed[1:]):
        assert after.top == before.top + before.rows
    figure = placed[-2]
    assert figure.text_top == figure.content_top + 5
    assert document_viewport_problem(content, 20, 60) is None
    # Scrolled two rows into the paragraph, which starts the carried blocks.
    later = replace(content, blocks=content.blocks[1:], viewport_row=2)
    placed = placed_blocks(later, 20)
    assert placed[0].top == -2 and placed[0].content_top == -1
    assert placed[0].text_top == -1 and placed[1].top == 2
    assert document_viewport_problem(later, 20, 60) is None
    assert document_viewport_problem(replace(later, viewport_row=4), 20, 60) == (
        "the viewport row lies past the first block"
    )


def test_a_terminal_checks_the_viewport_against_the_root() -> None:
    assert document_viewport_problem(_guide(1), 20, 60) == (
        "the viewport row lies past the first block"
    )
    with_two = DocumentContent(1, DocumentFlag(0), (
        DocumentBlock(1, BlockKind.PARAGRAPH, "one"),
        DocumentBlock(2, BlockKind.PARAGRAPH, "two", rows_before=1),
    ), viewport_row=1)
    assert document_viewport_problem(with_two, 20, 2) == (
        "the viewport row lies past the first block"
    )
    # The first block shows its one row; the second starts on root row 1.
    shown = replace(with_two, viewport_row=0)
    assert document_viewport_problem(shown, 20, 2) is None
    # In a root one row tall, the second block starts below it.
    assert document_viewport_problem(shown, 20, 1) == "a carried block starts below the root"
    assert document_viewport_problem(DocumentContent(1, DocumentFlag(0), ()), 20, 1) is None


def test_a_link_is_shown_only_where_the_root_shows_its_character() -> None:
    content = _guide()
    # "notes" is a link at offsets 9 to 13 of block 2, on its first line.
    assert shown_link(content, 20, 60, 2, 9)
    assert shown_link(content, 20, 60, 2, 13)
    # Plain text, emphasis, an unknown block, and a root too short to show it.
    assert not shown_link(content, 20, 60, 2, 0)
    assert not shown_link(content, 20, 60, 2, 31)
    assert not shown_link(content, 20, 60, 42, 9)
    assert not shown_link(content, 20, 2, 2, 9)


def _document_control(content=None, bounds=None) -> ControlWireDefinition:
    return ControlWireDefinition(
        owner_id=7,
        owner_generation=3,
        control_id=41,
        kind=ControlKind.DOCUMENT,
        state=ControlState.VISIBLE | ControlState.ENABLED,
        z_order=0,
        region_id=1,
        parent_control_id=0,
        order=0,
        # The sample's 27 rows at width 20 fit in 30.
        bounds=ObjectBounds(0, 2, 20, 30) if bounds is None else bounds,
        label="",
        shortcut="",
        content=_guide() if content is None else content,
    )


def test_a_document_control_carries_one_doc1_body() -> None:
    definition = _document_control()
    raw = encode_control_definition(definition)
    body = encode_document_content(definition.content)
    assert struct.unpack_from("<H", raw, 24)[0] == 14
    assert struct.unpack_from("<I", raw, 76)[0] == len(body)
    assert raw[80:] == body
    assert decode_control_definition(raw) == definition
    with pytest.raises(ValueError, match="past the first block"):
        _document_control(_guide(viewport_row=1))
    # Too short a root for every carried block: the third starts on row 5.
    with pytest.raises(ValueError, match="starts below the root"):
        _document_control(bounds=ObjectBounds(0, 2, 20, 2))
    with pytest.raises(ValueError):
        replace(_document_control(), kind=ControlKind.ITEM_VIEW)


def test_scroll_and_follow_carry_their_usual_tails_on_documents() -> None:
    follow = ControlEvent(7, 3, 41, ControlEventKind.FOLLOW, 0, 11,
                          content_revision=5, item_key=2, scalar_offset=9)
    assert struct.unpack_from("<QQII", encode_control_event(follow), 40) == (5, 2, 9, 0)
    scroll = ControlEvent(7, 3, 41, ControlEventKind.SCROLL, 0, 11, wheel_y=2)
    assert len(encode_control_event(scroll)) == 48


def test_the_document_feature_is_bit_sixteen() -> None:
    assert RetainedFeature.CONTROL_DOCUMENTS == 1 << 16
