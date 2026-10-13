"""Renderer-neutral documents for retained DOCUMENT controls.

A document is text read from top to bottom, with its structure
(SEMANTIC-CONTENT-1, DOC1 body): paragraphs, headings, code, rules, and
figures, inside quotes and list items, and the starts of its pages.  The client carries the blocks the
root shows and says how far the first is scrolled; the renderer breaks their
lines and draws them, and both find the same rows.

The value and its wire codec are immutable and self-validating.  Like STX1
and ITM1, DOC1 has no block or text maximum of its own: the enclosing APT-1
payload, transaction, owner UTF-8 reservation, and caller-provided terminal
limits are the bounds.

Rows are exact: ``block_row_count`` gives a block's rows at a root width and
``placed_blocks`` places the carried blocks in a root, so that the terminal's
viewport check, its input check, and the renderer use one rule.
"""

from __future__ import annotations

import bisect
import struct
from dataclasses import dataclass, field
from enum import IntEnum, IntFlag

from . import text_rules
from .apt1 import UINT16_MAX, UINT32_MAX, UINT64_MAX
from .semantic_content import (
    SemanticContentError,
    SemanticContentErrorCode,
    StyleRun,
    TextStyle,
    _integer,
)


DOCUMENT_TAG = 0x31434F44  # little-endian ``DOC1``
DOCUMENT_VERSION = 1

_HEADER = struct.Struct("<IHHQHHIII")
_BLOCK = struct.Struct("<QHHHHIHHII")
_CONTAINER = struct.Struct("<BBHI")
_STYLE_RUN = struct.Struct("<IIHH")

# A heading's levels, 1 the highest.
HEADING_LEVELS = range(1, 7)


class DocumentFlag(IntFlag):
    """Bits 0 and 1 hold the paragraph direction of the document's prose
    (APT-1-TEXT Section 7.1): neither for AUTO, one for LTR or RTL.  Both
    together are invalid."""

    DIRECTION_LTR = 1 << 0
    DIRECTION_RTL = 1 << 1


DOCUMENT_FLAG_MASK = DocumentFlag.DIRECTION_LTR | DocumentFlag.DIRECTION_RTL


class BlockKind(IntEnum):
    PARAGRAPH = 1
    HEADING = 2
    CODE = 3
    RULE = 4
    FIGURE = 5
    PAGE = 6


class ContainerKind(IntEnum):
    QUOTE = 1
    BULLET_ITEM = 2
    NUMBERED_ITEM = 3


class ContainerFlag(IntFlag):
    """Bit 0, ``ITEM_START``: the block starts this list item, so the item's
    mark is shown on the block's first content row."""

    ITEM_START = 1 << 0


CONTAINER_FLAG_MASK = ContainerFlag.ITEM_START
_LIST_ITEMS = frozenset((ContainerKind.BULLET_ITEM, ContainerKind.NUMBERED_ITEM))


def _has_control(value: str) -> bool:
    return any(
        (ord(character) < 0x20 and character != "\n") or character == "\x7f"
        for character in value
    )


def _enum(name: str, kind, value):
    if isinstance(value, bool):
        raise TypeError(f"{name} must not be bool")
    try:
        return kind(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} is not a valid {kind.__name__}") from exc


@dataclass(frozen=True, slots=True)
class BlockContainer:
    """One quote or list item a block sits in, and the cells it takes at the
    left of each of the block's rows."""

    kind: ContainerKind
    width: int
    number: int = 0
    item_start: bool = False

    def __post_init__(self) -> None:
        object.__setattr__(self, "kind", _enum("container kind", ContainerKind, self.kind))
        object.__setattr__(
            self, "width", _integer("width", self.width, minimum=1, maximum=UINT16_MAX)
        )
        object.__setattr__(
            self, "number", _integer("number", self.number, minimum=0, maximum=UINT32_MAX)
        )
        if self.number and self.kind is not ContainerKind.NUMBERED_ITEM:
            raise ValueError("only a numbered item has a number")
        if not isinstance(self.item_start, bool):
            raise TypeError("item_start must be bool")
        if self.item_start and self.kind not in _LIST_ITEMS:
            raise ValueError("only a list item can start")

    @property
    def flags(self) -> ContainerFlag:
        return ContainerFlag.ITEM_START if self.item_start else ContainerFlag(0)

    @property
    def list_item(self) -> bool:
        return self.kind in _LIST_ITEMS


@dataclass(frozen=True, slots=True)
class DocumentBlock:
    """One stable-keyed block: its kind, the quotes and list items around it,
    the blank rows before it, and its text with the style runs that say what
    parts of it mean, under the rules of STX1's style runs."""

    block_key: int
    kind: BlockKind
    text: str = ""
    runs: tuple[StyleRun, ...] = ()
    level: int = 0
    rows_before: int = 0
    rows_before_inside: int = 0
    picture_rows: int = 0
    containers: tuple[BlockContainer, ...] = ()
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _run_starts: tuple[int, ...] = field(init=False, repr=False, compare=False)
    _indent: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "block_key",
            _integer("block_key", self.block_key, minimum=1, maximum=UINT64_MAX),
        )
        kind = _enum("block kind", BlockKind, self.kind)
        object.__setattr__(self, "kind", kind)
        for name, maximum in (
            ("level", UINT16_MAX),
            ("rows_before", UINT16_MAX),
            ("rows_before_inside", UINT16_MAX),
            ("picture_rows", UINT32_MAX),
        ):
            object.__setattr__(
                self, name, _integer(name, getattr(self, name), minimum=0, maximum=maximum)
            )
        if kind is BlockKind.HEADING:
            if self.level not in HEADING_LEVELS:
                raise ValueError("a heading's level is 1 to 6")
        elif self.level:
            raise ValueError("only a heading has a level")
        if (kind is BlockKind.FIGURE) != bool(self.picture_rows):
            raise ValueError("a figure, and only a figure, has picture rows")
        containers = tuple(self.containers)
        if any(not isinstance(container, BlockContainer) for container in containers):
            raise TypeError("containers must contain only BlockContainer values")
        if len(containers) > UINT16_MAX:
            raise ValueError("container count exceeds u16")
        if self.rows_before_inside > len(containers):
            raise ValueError("the rows before lie inside more containers than the block has")
        if kind is BlockKind.PAGE and containers:
            raise ValueError("a page's start sits in no container")
        object.__setattr__(self, "containers", containers)
        object.__setattr__(self, "_indent", sum(container.width for container in containers))
        if not isinstance(self.text, str):
            raise TypeError("text must be str")
        if _has_control(self.text):
            raise ValueError("text contains a control character")
        try:
            utf8 = self.text.encode("utf-8", "strict")
        except UnicodeEncodeError as exc:
            raise ValueError("text contains a non-scalar surrogate") from exc
        object.__setattr__(self, "_utf8_bytes", len(utf8))
        runs = tuple(self.runs)
        if any(not isinstance(run, StyleRun) for run in runs):
            raise TypeError("runs must contain only StyleRun values")
        if len(runs) > UINT32_MAX:
            raise ValueError("style run count exceeds u32")
        if kind is BlockKind.RULE and (self.text or runs):
            raise ValueError("a rule has no text and no style runs")
        prior: StyleRun | None = None
        for run in runs:
            if run.end > len(self.text):
                raise ValueError("a style run reaches past the block's text")
            if prior is not None:
                if run.start < prior.end:
                    raise ValueError("style runs are out of order or overlap")
                if run.start == prior.end and run.meaning is prior.meaning:
                    raise ValueError("two style runs with the same meaning touch")
            prior = run
        object.__setattr__(self, "runs", runs)
        object.__setattr__(self, "_run_starts", tuple(run.start for run in runs))

    @property
    def indent(self) -> int:
        """The cells its containers take at the left of each row."""

        return self._indent

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return (
            _BLOCK.size
            + _CONTAINER.size * len(self.containers)
            + self._utf8_bytes
            + _STYLE_RUN.size * len(self.runs)
        )

    def meaning_at(self, offset: int) -> TextStyle | None:
        """The meaning of the scalar at OFFSET, or None when it is plain."""

        index = bisect.bisect_right(self._run_starts, offset) - 1
        if index < 0:
            return None
        run = self.runs[index]
        return run.meaning if offset < run.end else None


@dataclass(frozen=True, slots=True)
class DocumentContent:
    """One canonical document view: the blocks a root shows, in the
    document's order, the first starting ``viewport_row`` rows above the
    root.  A terminal checks the viewport against the control's bounds."""

    content_revision: int
    flags: DocumentFlag
    blocks: tuple[DocumentBlock, ...]
    viewport_row: int = 0
    style_run_count: int = field(init=False, repr=False, compare=False)
    _by_key: dict = field(init=False, repr=False, compare=False)
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _wire_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "content_revision",
            _integer("content_revision", self.content_revision, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "viewport_row",
            _integer("viewport_row", self.viewport_row, minimum=0, maximum=UINT32_MAX),
        )
        flag_bits = _integer("flags", self.flags, minimum=0, maximum=UINT16_MAX)
        if flag_bits & ~int(DOCUMENT_FLAG_MASK):
            raise ValueError("flags contain reserved document bits")
        if flag_bits == int(DOCUMENT_FLAG_MASK):
            raise ValueError("flags name both LTR and RTL")
        object.__setattr__(self, "flags", DocumentFlag(flag_bits))
        blocks = tuple(self.blocks)
        if any(not isinstance(block, DocumentBlock) for block in blocks):
            raise TypeError("blocks must contain only DocumentBlock values")
        if len(blocks) > UINT32_MAX:
            raise ValueError("block count exceeds u32")
        if not blocks and self.viewport_row:
            raise ValueError("a document with no blocks has viewport row zero")
        by_key: dict[int, DocumentBlock] = {}
        wire_bytes = _HEADER.size
        utf8_bytes = 0
        style_run_count = 0
        for block in blocks:
            if block.block_key in by_key:
                raise ValueError("block keys are duplicated")
            by_key[block.block_key] = block
            if block.wire_bytes > UINT32_MAX - wire_bytes:
                raise ValueError("document content exceeds u32 wire bytes")
            wire_bytes += block.wire_bytes
            utf8_bytes += block.utf8_bytes
            style_run_count += len(block.runs)
        object.__setattr__(self, "blocks", blocks)
        object.__setattr__(self, "style_run_count", style_run_count)
        object.__setattr__(self, "_by_key", by_key)
        object.__setattr__(self, "_utf8_bytes", utf8_bytes)
        object.__setattr__(self, "_wire_bytes", wire_bytes)

    @property
    def direction(self) -> int:
        """The prose's paragraph direction: 0 AUTO, 1 LTR, or 2 RTL,
        numbered as text_rules numbers them."""

        return int(self.flags) & 3

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return self._wire_bytes

    @property
    def object_slots(self) -> int:
        """The object-quota slots a DOCUMENT with this content takes: one
        for the control and one for each carried block."""

        return 1 + len(self.blocks)

    def block(self, block_key: int) -> DocumentBlock | None:
        """The carried block with this key, or None."""

        return self._by_key.get(block_key)


# ---------------------------------------------------------------------------
# Rows
# ---------------------------------------------------------------------------


def block_text_width(block: DocumentBlock, root_cols: int) -> int:
    """``T = max(W - I, 1)``: the cells left for a block's text in a root
    ``root_cols`` wide, after its containers."""

    return max(root_cols - block.indent, 1)


def block_lines(
    content: DocumentContent, block: DocumentBlock, root_cols: int
) -> tuple[text_rules.LineLayout, ...]:
    """A block's text broken into its lines at a root width: prose by
    APT-1-TEXT Section 12 in the content's direction, code into hard lines
    left to right.  A rule has none, and so has a figure with no caption."""

    if block.kind is BlockKind.RULE or (block.kind is BlockKind.FIGURE and not block.text):
        return ()
    width = block_text_width(block, root_cols)
    if block.kind is BlockKind.CODE:
        return text_rules.cached_lines(block.text, text_rules.DIRECTION_LTR, width, True)
    return text_rules.cached_lines(block.text, content.direction, width)


def block_content_rows(content: DocumentContent, block: DocumentBlock, root_cols: int) -> int:
    """The rows a block's content takes after its rows before."""

    if block.kind is BlockKind.RULE:
        return 1
    return block.picture_rows + len(block_lines(content, block, root_cols))


def block_row_count(content: DocumentContent, block: DocumentBlock, root_cols: int) -> int:
    """How many rows one block takes at a root width, its rows before
    included (SEMANTIC-CONTENT-1)."""

    return block.rows_before + block_content_rows(content, block, root_cols)


@dataclass(frozen=True, slots=True)
class PlacedBlock:
    """One carried block where a root of some width shows it.  ``top`` is
    the root row of its first row, its rows before included, and may lie
    above the root; ``text_top`` is the root row of its first line of text,
    after any picture rows."""

    block: DocumentBlock
    top: int
    content_top: int
    text_top: int
    rows: int
    lines: tuple[text_rules.LineLayout, ...]


def placed_blocks(content: DocumentContent, root_cols: int) -> tuple[PlacedBlock, ...]:
    """Every carried block placed in a root ``root_cols`` wide: the first
    starts ``viewport_row`` rows above the root's top edge, and each later
    one on the row after the one before it ends."""

    placed: list[PlacedBlock] = []
    row = -content.viewport_row
    for block in content.blocks:
        lines = block_lines(content, block, root_cols)
        content_top = row + block.rows_before
        rows = block.rows_before + (
            1 if block.kind is BlockKind.RULE else block.picture_rows + len(lines)
        )
        placed.append(
            PlacedBlock(block, row, content_top, content_top + block.picture_rows, rows, lines)
        )
        row += rows
    return tuple(placed)


def document_viewport_problem(
    content: DocumentContent, root_cols: int, root_rows: int
) -> str | None:
    """Why the carried blocks do not fit a root of this size, or None: the
    first block shows a row, and every later block starts above the root's
    bottom edge, so it shows one too."""

    if not content.blocks:
        return None
    first = content.blocks[0]
    if content.viewport_row >= block_row_count(content, first, root_cols):
        return "the viewport row lies past the first block"
    top = block_row_count(content, first, root_cols) - content.viewport_row
    for block in content.blocks[1:]:
        if top >= root_rows:
            return "a carried block starts below the root"
        top += block_row_count(content, block, root_cols)
    return None


def shown_link(
    content: DocumentContent,
    root_cols: int,
    root_rows: int,
    block_key: int,
    scalar_offset: int,
) -> bool:
    """Whether a position names the start of a character the root shows in
    a carried block, whose meaning is ``LINK`` (SEMANTIC-CONTENT-1)."""

    block = content.block(block_key)
    if block is None or block.meaning_at(scalar_offset) is not TextStyle.LINK:
        return False
    for placed in placed_blocks(content, root_cols):
        if placed.block is not block:
            continue
        for index, line in enumerate(placed.lines):
            row = placed.text_top + index
            if not 0 <= row < root_rows:
                continue
            if any(character.start == scalar_offset for character in line.characters):
                return True
        return False
    return False


# ---------------------------------------------------------------------------
# Codec
# ---------------------------------------------------------------------------


def encode_document_content(content: DocumentContent) -> bytes:
    """Encode one exact DOC1 value without imposing an extra capacity."""

    if not isinstance(content, DocumentContent):
        raise TypeError("content must be DocumentContent")
    result = bytearray(content.wire_bytes)
    _HEADER.pack_into(
        result,
        0,
        DOCUMENT_TAG,
        DOCUMENT_VERSION,
        0,
        content.content_revision,
        int(content.flags),
        0,
        len(content.blocks),
        content.viewport_row,
        0,
    )
    offset = _HEADER.size
    for block in content.blocks:
        text = block.text.encode("utf-8", "strict")
        _BLOCK.pack_into(
            result,
            offset,
            block.block_key,
            int(block.kind),
            block.level,
            block.rows_before,
            block.rows_before_inside,
            block.picture_rows,
            len(block.containers),
            0,
            len(text),
            len(block.runs),
        )
        offset += _BLOCK.size
        for container in block.containers:
            _CONTAINER.pack_into(
                result,
                offset,
                int(container.kind),
                int(container.flags),
                container.width,
                container.number,
            )
            offset += _CONTAINER.size
        result[offset : offset + len(text)] = text
        offset += len(text)
        for run in block.runs:
            _STYLE_RUN.pack_into(result, offset, run.start, run.length, int(run.meaning), 0)
            offset += _STYLE_RUN.size
    return bytes(result)


def _fail(code: SemanticContentErrorCode, detail: str) -> SemanticContentError:
    return SemanticContentError(code, detail)


def decode_document_content(payload) -> DocumentContent:
    """Decode one exact DOC1 value and reject all non-canonical bytes."""

    if isinstance(payload, str):
        raise TypeError("document content must be bytes-like, not str")
    try:
        raw = bytes(payload)
    except (TypeError, ValueError) as exc:
        raise TypeError("document content must be bytes-like") from exc
    if len(raw) < _HEADER.size:
        raise _fail(
            SemanticContentErrorCode.PAYLOAD,
            "document content is shorter than its fixed header",
        )
    (
        tag,
        version,
        reserved0,
        revision,
        flags,
        reserved1,
        block_count,
        viewport_row,
        reserved2,
    ) = _HEADER.unpack_from(raw)
    if tag != DOCUMENT_TAG:
        raise _fail(SemanticContentErrorCode.CONSISTENCY, "document content tag is not DOC1")
    if version != DOCUMENT_VERSION:
        raise _fail(
            SemanticContentErrorCode.ENUM,
            f"document content version {version} is not canonical",
        )
    if reserved0 or reserved1 or reserved2:
        raise _fail(
            SemanticContentErrorCode.RESERVED, "document content reserved field is nonzero"
        )
    if flags & ~int(DOCUMENT_FLAG_MASK):
        raise _fail(SemanticContentErrorCode.RESERVED, "document flags contain reserved bits")
    if flags == int(DOCUMENT_FLAG_MASK):
        raise _fail(SemanticContentErrorCode.ENUM, "document direction 3 is invalid")
    offset = _HEADER.size
    if block_count > (len(raw) - offset) // _BLOCK.size:
        raise _fail(
            SemanticContentErrorCode.PAYLOAD,
            "block count cannot fit the content payload",
        )
    blocks: list[DocumentBlock] = []
    for _ in range(block_count):
        if _BLOCK.size > len(raw) - offset:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a block header is truncated")
        (
            block_key,
            kind_value,
            level,
            rows_before,
            rows_before_inside,
            picture_rows,
            container_count,
            reserved,
            text_bytes,
            run_count,
        ) = _BLOCK.unpack_from(raw, offset)
        offset += _BLOCK.size
        if reserved:
            raise _fail(SemanticContentErrorCode.RESERVED, "a block's reserved field is nonzero")
        try:
            kind = BlockKind(kind_value)
        except ValueError as exc:
            raise _fail(
                SemanticContentErrorCode.ENUM, f"block kind {kind_value} is not canonical"
            ) from exc
        if container_count > (len(raw) - offset) // _CONTAINER.size:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a block's containers are truncated")
        containers: list[BlockContainer] = []
        for _ in range(container_count):
            container_kind, container_flags, width, number = _CONTAINER.unpack_from(raw, offset)
            offset += _CONTAINER.size
            if container_flags & ~int(CONTAINER_FLAG_MASK):
                raise _fail(
                    SemanticContentErrorCode.RESERVED,
                    "a container's flags contain reserved bits",
                )
            try:
                normalized = ContainerKind(container_kind)
            except ValueError as exc:
                raise _fail(
                    SemanticContentErrorCode.ENUM,
                    f"container kind {container_kind} is not canonical",
                ) from exc
            try:
                containers.append(
                    BlockContainer(
                        normalized,
                        width,
                        number,
                        bool(container_flags & ContainerFlag.ITEM_START),
                    )
                )
            except (TypeError, ValueError) as exc:
                raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc
        if text_bytes > len(raw) - offset:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a block's text is truncated")
        try:
            text = raw[offset : offset + text_bytes].decode("utf-8", "strict")
        except UnicodeDecodeError as exc:
            raise _fail(
                SemanticContentErrorCode.SCALAR, "a block's text is not well-formed UTF-8"
            ) from exc
        if _has_control(text):
            raise _fail(
                SemanticContentErrorCode.SCALAR, "a block's text contains a control character"
            )
        offset += text_bytes
        if run_count > (len(raw) - offset) // _STYLE_RUN.size:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a block's style runs are truncated")
        runs = []
        for _ in range(run_count):
            start, length, meaning, run_reserved = _STYLE_RUN.unpack_from(raw, offset)
            offset += _STYLE_RUN.size
            if run_reserved:
                raise _fail(
                    SemanticContentErrorCode.RESERVED,
                    "a style run's reserved field is nonzero",
                )
            try:
                style = TextStyle(meaning)
            except ValueError as exc:
                raise _fail(
                    SemanticContentErrorCode.ENUM,
                    f"style meaning {meaning} is not canonical",
                ) from exc
            if not length:
                raise _fail(SemanticContentErrorCode.CONSISTENCY, "a style run is empty")
            runs.append(StyleRun(start, length, style))
        try:
            blocks.append(
                DocumentBlock(
                    block_key=block_key,
                    kind=kind,
                    text=text,
                    runs=tuple(runs),
                    level=level,
                    rows_before=rows_before,
                    rows_before_inside=rows_before_inside,
                    picture_rows=picture_rows,
                    containers=tuple(containers),
                )
            )
        except (TypeError, ValueError) as exc:
            raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc
    if offset != len(raw):
        raise _fail(SemanticContentErrorCode.PAYLOAD, "document content has trailing bytes")
    try:
        return DocumentContent(
            content_revision=revision,
            flags=DocumentFlag(flags),
            blocks=tuple(blocks),
            viewport_row=viewport_row,
        )
    except (TypeError, ValueError) as exc:
        raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc


__all__ = [
    "CONTAINER_FLAG_MASK",
    "DOCUMENT_FLAG_MASK",
    "DOCUMENT_TAG",
    "DOCUMENT_VERSION",
    "HEADING_LEVELS",
    "BlockContainer",
    "BlockKind",
    "ContainerFlag",
    "ContainerKind",
    "DocumentBlock",
    "DocumentContent",
    "DocumentFlag",
    "PlacedBlock",
    "block_content_rows",
    "block_lines",
    "block_row_count",
    "block_text_width",
    "decode_document_content",
    "document_viewport_problem",
    "encode_document_content",
    "placed_blocks",
    "shown_link",
]
