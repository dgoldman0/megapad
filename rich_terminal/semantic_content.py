"""Renderer-neutral text collections for retained semantic controls.

The retained CONTROL namespace carries identity, hierarchy, geometry, and
interaction state.  This module supplies one bounded content value for the
semantic controls that need more than a label: text areas and logical text
grids.  It deliberately describes text and logical placement, not terminal
buffer reservations, fonts, pixels, padding, or panel refresh policy.

The value and its wire codec are immutable and self-validating.  There is no
independent item or text maximum: the enclosing APT-1 payload, transaction,
owner UTF-8 reservation, and caller-provided terminal limits are the bounds.
"""

from __future__ import annotations

import operator
import struct
from dataclasses import dataclass, field
from enum import Enum, IntEnum, IntFlag

from .apt1 import UINT16_MAX, UINT32_MAX, UINT64_MAX


SEMANTIC_TEXT_TAG = 0x31585453  # little-endian ``STX1``
SEMANTIC_TEXT_VERSION = 1

_CONTENT_HEADER = struct.Struct("<IHHQIIIIIIIIQQII")
_ITEM_HEADER = struct.Struct("<QIIIIHHI")


class SemanticContentErrorCode(str, Enum):
    PAYLOAD = "PAYLOAD"
    RESERVED = "RESERVED"
    SCALAR = "SCALAR"
    ENUM = "ENUM"
    CONSISTENCY = "CONSISTENCY"


class SemanticContentError(ValueError):
    """An exact semantic-content byte or value rejection."""

    def __init__(self, code: SemanticContentErrorCode, detail: str):
        self.code = code
        self.detail = detail
        super().__init__(f"{code.value}: {detail}")


class SemanticContentFlag(IntFlag):
    """Renderer-neutral properties of one complete text collection."""

    READ_ONLY = 1 << 0


SEMANTIC_CONTENT_FLAG_MASK = SemanticContentFlag.READ_ONLY


class SemanticTextRole(IntEnum):
    CONTENT = 1
    ROW_HEADER = 2
    COLUMN_HEADER = 3


class SemanticTextState(IntFlag):
    """Authoritative per-item state; hover and press stay renderer-owned."""

    CURRENT = 1 << 0
    UNAVAILABLE = 1 << 1


SEMANTIC_TEXT_STATE_MASK = SemanticTextState.CURRENT | SemanticTextState.UNAVAILABLE


def _integer(name: str, value, *, minimum: int, maximum: int) -> int:
    if isinstance(value, bool):
        raise TypeError(f"{name} must be an integer, not bool")
    try:
        result = operator.index(value)
    except TypeError as exc:
        raise TypeError(f"{name} must be an integer") from exc
    if not minimum <= result <= maximum:
        raise ValueError(f"{name} must be between {minimum} and {maximum}")
    return int(result)


def _has_disallowed_control(value: str) -> bool:
    for character in value:
        scalar = ord(character)
        if (scalar < 0x20 and character != "\t") or scalar == 0x7F:
            return True
    return False


def _clean_text(name: str, value: str) -> bytes:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    if _has_disallowed_control(value):
        raise ValueError(f"{name} contains a disallowed control character")
    try:
        return value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc


def _rectangles_overlap(items: tuple[SemanticTextItem, ...]) -> bool:
    """Return whether canonical item rectangles overlap in two dimensions.

    The common unit-row-span case stays linear. Coordinate compression plus a
    row-event range-maximum sweep handles genuine row spans without the old
    row-band restriction and keeps adversarial validation bounded to
    O(n log n) time and O(n) caller-accounted working storage.
    """

    if len(items) < 2:
        return False
    if all(item.row_span == 1 for item in items):
        prior_row = -1
        prior_right = 0
        for item in items:
            if item.row != prior_row:
                prior_row = item.row
                prior_right = 0
            if item.column < prior_right:
                return True
            prior_right = item.column + item.column_span
        return False
    columns: set[int] = set()
    events: list[tuple[int, int, int, int]] = []
    for item in items:
        right = item.column + item.column_span
        bottom = item.row + item.row_span
        columns.add(item.column)
        columns.add(right)
        # End events sort before start events at a shared half-open boundary.
        events.append((bottom, 0, item.column, right))
        events.append((item.row, 1, item.column, right))

    ordered_columns = sorted(columns)
    column_index = {
        column: index for index, column in enumerate(ordered_columns)
    }
    segment_count = len(ordered_columns) - 1
    maximum = [0] * (4 * segment_count)
    lazy = [0] * (4 * segment_count)

    def push(node: int) -> None:
        delta = lazy[node]
        if not delta:
            return
        left_child = node * 2
        right_child = left_child + 1
        maximum[left_child] += delta
        maximum[right_child] += delta
        lazy[left_child] += delta
        lazy[right_child] += delta
        lazy[node] = 0

    def add(
        node: int,
        left: int,
        right: int,
        query_left: int,
        query_right: int,
        delta: int,
    ) -> None:
        if query_left <= left and right <= query_right:
            maximum[node] += delta
            lazy[node] += delta
            return
        push(node)
        middle = (left + right) // 2
        if query_left < middle:
            add(node * 2, left, middle, query_left, query_right, delta)
        if middle < query_right:
            add(node * 2 + 1, middle, right, query_left, query_right, delta)
        maximum[node] = max(maximum[node * 2], maximum[node * 2 + 1])

    def query(
        node: int,
        left: int,
        right: int,
        query_left: int,
        query_right: int,
    ) -> int:
        if query_left <= left and right <= query_right:
            return maximum[node]
        push(node)
        middle = (left + right) // 2
        result = 0
        if query_left < middle:
            result = query(
                node * 2,
                left,
                middle,
                query_left,
                query_right,
            )
        if middle < query_right:
            result = max(
                result,
                query(
                    node * 2 + 1,
                    middle,
                    right,
                    query_left,
                    query_right,
                ),
            )
        return result

    events.sort()
    for _, event_kind, column, right in events:
        left_index = column_index[column]
        right_index = column_index[right]
        if event_kind == 0:
            add(1, 0, segment_count, left_index, right_index, -1)
        else:
            if query(1, 0, segment_count, left_index, right_index):
                return True
            add(1, 0, segment_count, left_index, right_index, 1)
    return False


@dataclass(frozen=True, slots=True)
class SemanticTextItem:
    """One stable-keyed text item in logical row/column geometry."""

    item_key: int
    row: int
    column: int
    row_span: int
    column_span: int
    role: SemanticTextRole
    state: SemanticTextState
    text: str
    _utf8_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("item_key", 1, UINT64_MAX),
            ("row", 0, UINT32_MAX),
            ("column", 0, UINT32_MAX),
            ("row_span", 1, UINT32_MAX),
            ("column_span", 1, UINT32_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        if isinstance(self.role, bool):
            raise TypeError("role must not be bool")
        try:
            role = SemanticTextRole(self.role)
        except (TypeError, ValueError) as exc:
            raise ValueError("role is not a semantic text role") from exc
        object.__setattr__(self, "role", role)
        if isinstance(self.state, bool):
            raise TypeError("state must not be bool")
        try:
            state_bits = operator.index(self.state)
        except TypeError as exc:
            raise TypeError("state must be SemanticTextState-compatible") from exc
        if not 0 <= state_bits <= UINT16_MAX:
            raise ValueError("state must fit u16")
        state = SemanticTextState(state_bits)
        if int(state) & ~int(SEMANTIC_TEXT_STATE_MASK):
            raise ValueError("state contains reserved semantic text bits")
        if state & SemanticTextState.CURRENT and state & SemanticTextState.UNAVAILABLE:
            raise ValueError("an unavailable semantic text item cannot be current")
        object.__setattr__(self, "state", state)
        text = _clean_text("text", self.text)
        object.__setattr__(self, "_utf8_bytes", len(text))

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes


@dataclass(frozen=True, slots=True)
class SemanticTextContent:
    """One canonical, complete renderer-neutral text collection.

    ``primary`` is the caret item for a text area or selected item for a text
    grid.  ``anchor`` is an optional text-selection anchor.  Offsets count
    Unicode scalar values in the named item's text, not bytes or graphemes.
    A zero key means the corresponding position is absent and requires offset
    zero.  Item coordinates are absolute within ``rows``/``columns``;
    ``viewport_row``/``viewport_column`` and
    ``viewport_rows``/``viewport_columns`` select one exact logical clip
    without preventing an offscreen primary or anchor item from being carried.
    """

    content_revision: int
    rows: int
    columns: int
    viewport_row: int
    viewport_column: int
    viewport_rows: int
    viewport_columns: int
    flags: SemanticContentFlag
    primary_key: int
    primary_offset: int
    anchor_key: int
    anchor_offset: int
    items: tuple[SemanticTextItem, ...]
    text_area_compatible: bool = field(init=False, repr=False, compare=False)
    current_item_count: int = field(init=False, repr=False, compare=False)
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _wire_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("content_revision", 1, UINT64_MAX),
            ("rows", 1, UINT32_MAX),
            ("columns", 1, UINT32_MAX),
            ("viewport_row", 0, UINT32_MAX),
            ("viewport_column", 0, UINT32_MAX),
            ("viewport_rows", 1, UINT32_MAX),
            ("viewport_columns", 1, UINT32_MAX),
            ("primary_key", 0, UINT64_MAX),
            ("primary_offset", 0, UINT32_MAX),
            ("anchor_key", 0, UINT64_MAX),
            ("anchor_offset", 0, UINT32_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        if (
            self.viewport_rows > self.rows - self.viewport_row
            or self.viewport_columns > self.columns - self.viewport_column
        ):
            raise ValueError("semantic text viewport exceeds content bounds")
        if isinstance(self.flags, bool):
            raise TypeError("flags must not be bool")
        try:
            flag_bits = operator.index(self.flags)
        except TypeError as exc:
            raise TypeError("flags must be SemanticContentFlag-compatible") from exc
        if not 0 <= flag_bits <= UINT32_MAX:
            raise ValueError("flags must fit u32")
        flags = SemanticContentFlag(flag_bits)
        if int(flags) & ~int(SEMANTIC_CONTENT_FLAG_MASK):
            raise ValueError("flags contain reserved semantic content bits")
        object.__setattr__(self, "flags", flags)

        items = tuple(self.items)
        if any(not isinstance(item, SemanticTextItem) for item in items):
            raise TypeError("items must contain only SemanticTextItem values")
        if len(items) > UINT32_MAX:
            raise ValueError("item count exceeds u32")
        by_key: dict[int, SemanticTextItem] = {}
        wire_bytes = _CONTENT_HEADER.size
        utf8_bytes = 0
        prior_order: tuple[int, int, int] | None = None
        text_area_compatible = True
        current_item_count = 0
        for item in items:
            item_bytes = _ITEM_HEADER.size + item.utf8_bytes
            if item_bytes > UINT32_MAX - wire_bytes:
                raise ValueError("semantic text content exceeds u32 wire bytes")
            wire_bytes += item_bytes
            utf8_bytes += item.utf8_bytes
            if (
                item.role is not SemanticTextRole.CONTENT
                or item.row_span != 1
                or item.column != 0
                or item.column_span != self.columns
                or item.state
                or len(item.text) > self.columns
            ):
                text_area_compatible = False
            if item.state & SemanticTextState.CURRENT:
                current_item_count += 1
            item_order = item.row, item.column, item.item_key
            if prior_order is not None and item_order < prior_order:
                raise ValueError(
                    "semantic text items are not in canonical row/column/key order"
                )
            prior_order = item_order
            if item.item_key in by_key:
                raise ValueError("semantic text item keys are duplicated")
            by_key[item.item_key] = item
            if item.row_span > self.rows - item.row:
                raise ValueError("semantic text item row span exceeds content bounds")
            if item.column_span > self.columns - item.column:
                raise ValueError("semantic text item column span exceeds content bounds")
        if _rectangles_overlap(items):
            raise ValueError("semantic text item rectangles overlap")

        self._validate_position(
            "primary", self.primary_key, self.primary_offset, by_key
        )
        self._validate_position("anchor", self.anchor_key, self.anchor_offset, by_key)
        if self.anchor_key and not self.primary_key:
            raise ValueError("semantic text anchor requires a primary position")
        object.__setattr__(self, "items", items)
        object.__setattr__(self, "text_area_compatible", text_area_compatible)
        object.__setattr__(self, "current_item_count", current_item_count)
        object.__setattr__(self, "_utf8_bytes", utf8_bytes)
        object.__setattr__(self, "_wire_bytes", wire_bytes)

    @staticmethod
    def _validate_position(
        name: str,
        item_key: int,
        offset: int,
        by_key: dict[int, SemanticTextItem],
    ) -> None:
        if item_key == 0:
            if offset:
                raise ValueError(f"absent {name} key requires offset zero")
            return
        item = by_key.get(item_key)
        if item is None:
            raise ValueError(f"{name} key does not name a semantic text item")
        if offset > len(item.text):
            raise ValueError(f"{name} offset exceeds the item's Unicode scalar length")

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return self._wire_bytes


def encode_semantic_text_content(content: SemanticTextContent) -> bytes:
    """Encode one exact STX1 value without imposing an extra capacity."""

    if not isinstance(content, SemanticTextContent):
        raise TypeError("content must be SemanticTextContent")
    result = bytearray(content.wire_bytes)
    _CONTENT_HEADER.pack_into(
        result,
        0,
        SEMANTIC_TEXT_TAG,
        SEMANTIC_TEXT_VERSION,
        0,
        content.content_revision,
        content.rows,
        content.columns,
        content.viewport_row,
        content.viewport_column,
        content.viewport_rows,
        content.viewport_columns,
        len(content.items),
        int(content.flags),
        content.primary_key,
        content.anchor_key,
        content.primary_offset,
        content.anchor_offset,
    )
    offset = _CONTENT_HEADER.size
    for item in content.items:
        # The immutable item already validated the scalar/control contract.
        text = item.text.encode("utf-8", "strict")
        text_bytes = item.utf8_bytes
        _ITEM_HEADER.pack_into(
            result,
            offset,
            item.item_key,
            item.row,
            item.column,
            item.row_span,
            item.column_span,
            int(item.role),
            int(item.state),
            text_bytes,
        )
        offset += _ITEM_HEADER.size
        result[offset : offset + text_bytes] = text
        offset += text_bytes
    return bytes(result)


def decode_semantic_text_content(payload) -> SemanticTextContent:
    """Decode one exact STX1 value and reject all non-canonical bytes."""

    if isinstance(payload, str):
        raise TypeError("semantic text content must be bytes-like, not str")
    try:
        raw = bytes(payload)
    except (TypeError, ValueError) as exc:
        raise TypeError("semantic text content must be bytes-like") from exc
    if len(raw) < _CONTENT_HEADER.size:
        raise SemanticContentError(
            SemanticContentErrorCode.PAYLOAD,
            "semantic text content is shorter than its fixed header",
        )
    (
        tag,
        version,
        reserved0,
        revision,
        rows,
        columns,
        viewport_row,
        viewport_column,
        viewport_rows,
        viewport_columns,
        item_count,
        flags,
        primary_key,
        anchor_key,
        primary_offset,
        anchor_offset,
    ) = _CONTENT_HEADER.unpack_from(raw)
    if tag != SEMANTIC_TEXT_TAG:
        raise SemanticContentError(
            SemanticContentErrorCode.CONSISTENCY,
            "semantic text content tag is not STX1",
        )
    if version != SEMANTIC_TEXT_VERSION:
        raise SemanticContentError(
            SemanticContentErrorCode.ENUM,
            f"semantic text content version {version} is not canonical",
        )
    if reserved0:
        raise SemanticContentError(
            SemanticContentErrorCode.RESERVED,
            "semantic text content reserved field is nonzero",
        )
    if flags & ~int(SEMANTIC_CONTENT_FLAG_MASK):
        raise SemanticContentError(
            SemanticContentErrorCode.RESERVED,
            "semantic text content flags contain reserved bits",
        )
    if item_count > (len(raw) - _CONTENT_HEADER.size) // _ITEM_HEADER.size:
        raise SemanticContentError(
            SemanticContentErrorCode.PAYLOAD,
            "semantic text item count cannot fit the content payload",
        )

    offset = _CONTENT_HEADER.size
    items: list[SemanticTextItem] = []
    for _ in range(item_count):
        if _ITEM_HEADER.size > len(raw) - offset:
            raise SemanticContentError(
                SemanticContentErrorCode.PAYLOAD,
                "semantic text item header is truncated",
            )
        values = _ITEM_HEADER.unpack_from(raw, offset)
        offset += _ITEM_HEADER.size
        text_bytes = values[7]
        if text_bytes > len(raw) - offset:
            raise SemanticContentError(
                SemanticContentErrorCode.PAYLOAD,
                "semantic text item bytes are truncated",
            )
        text_raw = raw[offset : offset + text_bytes]
        offset += text_bytes
        try:
            text = text_raw.decode("utf-8", "strict")
        except UnicodeDecodeError as exc:
            raise SemanticContentError(
                SemanticContentErrorCode.SCALAR,
                "semantic text item is not well-formed UTF-8",
            ) from exc
        if _has_disallowed_control(text):
            raise SemanticContentError(
                SemanticContentErrorCode.SCALAR,
                "semantic text item contains a disallowed control character",
            )
        try:
            role = SemanticTextRole(values[5])
        except ValueError as exc:
            raise SemanticContentError(
                SemanticContentErrorCode.ENUM,
                f"semantic text role {values[5]} is not canonical",
            ) from exc
        if values[6] & ~int(SEMANTIC_TEXT_STATE_MASK):
            raise SemanticContentError(
                SemanticContentErrorCode.RESERVED,
                "semantic text item state contains reserved bits",
            )
        try:
            items.append(
                SemanticTextItem(
                    item_key=values[0],
                    row=values[1],
                    column=values[2],
                    row_span=values[3],
                    column_span=values[4],
                    role=role,
                    state=SemanticTextState(values[6]),
                    text=text,
                )
            )
        except (TypeError, ValueError) as exc:
            raise SemanticContentError(
                SemanticContentErrorCode.CONSISTENCY,
                str(exc),
            ) from exc
    if offset != len(raw):
        raise SemanticContentError(
            SemanticContentErrorCode.PAYLOAD,
            "semantic text content has trailing bytes",
        )
    try:
        return SemanticTextContent(
            content_revision=revision,
            rows=rows,
            columns=columns,
            viewport_row=viewport_row,
            viewport_column=viewport_column,
            viewport_rows=viewport_rows,
            viewport_columns=viewport_columns,
            flags=SemanticContentFlag(flags),
            primary_key=primary_key,
            primary_offset=primary_offset,
            anchor_key=anchor_key,
            anchor_offset=anchor_offset,
            items=tuple(items),
        )
    except (TypeError, ValueError) as exc:
        raise SemanticContentError(
            SemanticContentErrorCode.CONSISTENCY,
            str(exc),
        ) from exc


__all__ = [
    "SEMANTIC_CONTENT_FLAG_MASK",
    "SEMANTIC_TEXT_STATE_MASK",
    "SEMANTIC_TEXT_TAG",
    "SEMANTIC_TEXT_VERSION",
    "SemanticContentError",
    "SemanticContentErrorCode",
    "SemanticContentFlag",
    "SemanticTextContent",
    "SemanticTextItem",
    "SemanticTextRole",
    "SemanticTextState",
    "decode_semantic_text_content",
    "encode_semantic_text_content",
]
