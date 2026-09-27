"""Renderer-neutral item collections for retained ITEM_VIEW controls.

An item view shows items as a list, a tree, a table, sections, or cards
(SEMANTIC-CONTENT-1, ITM1 body).  The client gives each item a stable key,
its parent and depth, its fields, and its state, and a viewport over the
items' order; the renderer lays them out and never infers structure from
their text.

The value and its wire codec are immutable and self-validating.  Like STX1,
ITM1 has no item or text maximum of its own: the enclosing APT-1 payload,
transaction, owner UTF-8 reservation, and caller-provided terminal limits
are the bounds.
"""

from __future__ import annotations

import bisect
import struct
from dataclasses import dataclass, field
from enum import IntEnum, IntFlag

from .apt1 import UINT16_MAX, UINT32_MAX, UINT64_MAX
from .semantic_content import (
    SemanticContentError,
    SemanticContentErrorCode,
    StyleRun,
    TextStyle,
    _integer,
)


ITEM_VIEW_TAG = 0x314D5449  # little-endian ``ITM1``
ITEM_VIEW_VERSION = 1

_HEADER = struct.Struct("<IHHQHHIIIII")
_COLUMN = struct.Struct("<HHI")
_ITEM = struct.Struct("<QQIHHHHI")
_FIELD = struct.Struct("<II")
_STYLE_RUN = struct.Struct("<IIHH")


class ItemViewRole(IntEnum):
    LIST = 1
    TREE = 2
    TABLE = 3
    SECTIONS = 4
    CARDS = 5


class ItemViewFlag(IntFlag):
    """Bits 0 and 1 hold the paragraph direction of every field and label
    (APT-1-TEXT Section 7.1): neither for AUTO, one for LTR or RTL.  Both
    together are invalid."""

    DIRECTION_LTR = 1 << 0
    DIRECTION_RTL = 1 << 1


ITEM_VIEW_FLAG_MASK = ItemViewFlag.DIRECTION_LTR | ItemViewFlag.DIRECTION_RTL


class ItemColumnKind(IntEnum):
    TEXT = 1
    NUMBER = 2


class ItemRole(IntEnum):
    ITEM = 1
    SECTION = 2


class ItemState(IntFlag):
    """Authoritative per-item state; hover and press stay renderer-owned."""

    SELECTED = 1 << 0
    CURRENT = 1 << 1
    EXPANDABLE = 1 << 2
    EXPANDED = 1 << 3
    CHECKABLE = 1 << 4
    CHECKED = 1 << 5
    UNAVAILABLE = 1 << 6


ITEM_STATE_MASK = (
    ItemState.SELECTED
    | ItemState.CURRENT
    | ItemState.EXPANDABLE
    | ItemState.EXPANDED
    | ItemState.CHECKABLE
    | ItemState.CHECKED
    | ItemState.UNAVAILABLE
)


def _has_control(value: str) -> bool:
    return any(ord(character) < 0x20 or character == "\x7f" for character in value)


def _clean_text(name: str, value: str) -> bytes:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    if _has_control(value):
        raise ValueError(f"{name} contains a control character")
    try:
        return value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc


def _enum(name: str, kind, value):
    if isinstance(value, bool):
        raise TypeError(f"{name} must not be bool")
    try:
        return kind(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} is not a valid {kind.__name__}") from exc


@dataclass(frozen=True, slots=True)
class ItemColumn:
    """One column: what kind of values its fields hold, and a label that may
    be empty."""

    kind: ItemColumnKind
    label: str = ""
    _utf8_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "kind", _enum("column kind", ItemColumnKind, self.kind))
        object.__setattr__(self, "_utf8_bytes", len(_clean_text("label", self.label)))

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return _COLUMN.size + self._utf8_bytes


@dataclass(frozen=True, slots=True)
class ItemField:
    """One field's text and the style runs that say what parts of it mean,
    under the rules of STX1's style runs."""

    text: str
    runs: tuple[StyleRun, ...] = ()
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _run_starts: tuple[int, ...] = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "_utf8_bytes", len(_clean_text("field text", self.text)))
        runs = tuple(self.runs)
        if any(not isinstance(run, StyleRun) for run in runs):
            raise TypeError("runs must contain only StyleRun values")
        if len(runs) > UINT32_MAX:
            raise ValueError("style run count exceeds u32")
        prior: StyleRun | None = None
        for run in runs:
            if run.end > len(self.text):
                raise ValueError("a style run reaches past the field's text")
            if prior is not None:
                if run.start < prior.end:
                    raise ValueError("style runs are out of order or overlap")
                if run.start == prior.end and run.meaning is prior.meaning:
                    raise ValueError("two style runs with the same meaning touch")
            prior = run
        object.__setattr__(self, "runs", runs)
        object.__setattr__(self, "_run_starts", tuple(run.start for run in runs))

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return _FIELD.size + self._utf8_bytes + _STYLE_RUN.size * len(self.runs)

    def meaning_at(self, offset: int) -> TextStyle | None:
        """The meaning of the scalar at OFFSET, or None when it is plain."""

        index = bisect.bisect_right(self._run_starts, offset) - 1
        if index < 0:
            return None
        run = self.runs[index]
        return run.meaning if offset < run.end else None


@dataclass(frozen=True, slots=True)
class ViewItem:
    """One stable-keyed item: its place in the view's order and structure,
    its state, and one field per column from the first."""

    item_key: int
    parent_key: int
    ordinal: int
    depth: int
    state: ItemState
    role: ItemRole
    fields: tuple[ItemField, ...]
    _wire_bytes: int = field(init=False, repr=False, compare=False)
    _utf8_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("item_key", 1, UINT64_MAX),
            ("parent_key", 0, UINT64_MAX),
            ("ordinal", 0, UINT32_MAX),
            ("depth", 0, UINT16_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        if self.parent_key == self.item_key:
            raise ValueError("an item cannot be its own parent")
        object.__setattr__(self, "role", _enum("item role", ItemRole, self.role))
        state_bits = _integer("state", self.state, minimum=0, maximum=UINT16_MAX)
        if state_bits & ~int(ITEM_STATE_MASK):
            raise ValueError("state contains reserved item bits")
        state = ItemState(state_bits)
        if state & ItemState.EXPANDED and not state & ItemState.EXPANDABLE:
            raise ValueError("an expanded item must be expandable")
        if state & ItemState.CHECKED and not state & ItemState.CHECKABLE:
            raise ValueError("a checked item must be checkable")
        if state & ItemState.UNAVAILABLE and state & ItemState.SELECTED:
            raise ValueError("an unavailable item cannot be selected")
        object.__setattr__(self, "state", state)
        fields_ = tuple(self.fields)
        if any(not isinstance(item_field, ItemField) for item_field in fields_):
            raise TypeError("fields must contain only ItemField values")
        if not 1 <= len(fields_) <= UINT16_MAX:
            raise ValueError("an item has between one and 65535 fields")
        if self.role is ItemRole.SECTION and (state or len(fields_) != 1):
            raise ValueError("a section has state zero and exactly one field")
        object.__setattr__(self, "fields", fields_)
        object.__setattr__(
            self,
            "_wire_bytes",
            _ITEM.size + sum(item_field.wire_bytes for item_field in fields_),
        )
        object.__setattr__(
            self, "_utf8_bytes", sum(item_field.utf8_bytes for item_field in fields_)
        )

    @property
    def wire_bytes(self) -> int:
        return self._wire_bytes

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes


@dataclass(frozen=True, slots=True)
class ItemViewContent:
    """One canonical, complete renderer-neutral item collection.

    Items are in the order the application shows them, numbered by
    ``ordinal`` from zero to ``item_total`` minus one.  The viewport is the
    ordinals from ``viewport_first`` for ``viewport_count`` items; every one
    of them is carried, and so is a selected item.
    """

    content_revision: int
    role: ItemViewRole
    flags: ItemViewFlag
    columns: tuple[ItemColumn, ...]
    item_total: int
    viewport_first: int
    viewport_count: int
    items: tuple[ViewItem, ...]
    style_run_count: int = field(init=False, repr=False, compare=False)
    _by_key: dict = field(init=False, repr=False, compare=False)
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _wire_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("content_revision", 1, UINT64_MAX),
            ("item_total", 0, UINT32_MAX),
            ("viewport_first", 0, UINT32_MAX),
            ("viewport_count", 0, UINT32_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        object.__setattr__(self, "role", _enum("item view role", ItemViewRole, self.role))
        flag_bits = _integer("flags", self.flags, minimum=0, maximum=UINT16_MAX)
        if flag_bits & ~int(ITEM_VIEW_FLAG_MASK):
            raise ValueError("flags contain reserved item view bits")
        if flag_bits == int(ITEM_VIEW_FLAG_MASK):
            raise ValueError("flags name both LTR and RTL")
        object.__setattr__(self, "flags", ItemViewFlag(flag_bits))
        if self.item_total == 0:
            if self.viewport_first or self.viewport_count:
                raise ValueError("an empty item view has an empty viewport at zero")
        elif not (
            self.viewport_first < self.item_total
            and 1 <= self.viewport_count <= self.item_total - self.viewport_first
        ):
            raise ValueError("the viewport does not lie within the items' order")

        columns = tuple(self.columns)
        if any(not isinstance(column, ItemColumn) for column in columns):
            raise TypeError("columns must contain only ItemColumn values")
        if not 1 <= len(columns) <= UINT32_MAX:
            raise ValueError("an item view has at least one column")
        items = tuple(self.items)
        if any(not isinstance(item, ViewItem) for item in items):
            raise TypeError("items must contain only ViewItem values")
        if len(items) > UINT32_MAX:
            raise ValueError("item count exceeds u32")

        wire_bytes = _HEADER.size + sum(column.wire_bytes for column in columns)
        utf8_bytes = sum(column.utf8_bytes for column in columns)
        by_key: dict[int, ViewItem] = {}
        style_run_count = 0
        in_viewport = 0
        selected = current = 0
        prior: ViewItem | None = None
        viewport_end = self.viewport_first + self.viewport_count
        for item in items:
            if len(item.fields) > len(columns):
                raise ValueError("an item has more fields than the view has columns")
            if item.ordinal >= self.item_total:
                raise ValueError("an item's ordinal lies past the item total")
            if prior is not None and item.ordinal <= prior.ordinal:
                raise ValueError("items are not in increasing ordinal order")
            if item.item_key in by_key:
                raise ValueError("item keys are duplicated")
            by_key[item.item_key] = item
            if self.viewport_first <= item.ordinal < viewport_end:
                in_viewport += 1
            if item.state & ItemState.SELECTED:
                selected += 1
            if item.state & ItemState.CURRENT:
                current += 1
            style_run_count += sum(len(item_field.runs) for item_field in item.fields)
            if item.wire_bytes > UINT32_MAX - wire_bytes:
                raise ValueError("item view content exceeds u32 wire bytes")
            wire_bytes += item.wire_bytes
            utf8_bytes += item.utf8_bytes
            prior = item
        if in_viewport != self.viewport_count:
            raise ValueError("an ordinal in the viewport is not carried")
        if selected > 1:
            raise ValueError("more than one item is selected")
        if current > 1:
            raise ValueError("more than one item is current")
        # Structure, now that every carried parent can be looked up.  Each
        # item is compared only with its parent and the item before it.
        prior = None
        for item in items:
            self._validate_structure(item, by_key.get(item.parent_key))
            if item.ordinal == 0 and item.depth:
                raise ValueError("the first item in the order has depth zero")
            if prior is not None and item.ordinal == prior.ordinal + 1:
                if item.depth > prior.depth + 1:
                    raise ValueError("depth rises by more than one between neighbours")
                if item.depth == prior.depth + 1 and item.parent_key != prior.item_key:
                    raise ValueError("an item deeper than the one before is not its child")
            prior = item
        object.__setattr__(self, "columns", columns)
        object.__setattr__(self, "items", items)
        object.__setattr__(self, "style_run_count", style_run_count)
        object.__setattr__(self, "_by_key", by_key)
        object.__setattr__(self, "_utf8_bytes", utf8_bytes)
        object.__setattr__(self, "_wire_bytes", wire_bytes)

    def _validate_structure(self, item: ViewItem, parent: ViewItem | None) -> None:
        if parent is not None and parent.ordinal >= item.ordinal:
            raise ValueError("an item's parent comes after it in the order")
        if self.role in (ItemViewRole.LIST, ItemViewRole.TABLE, ItemViewRole.CARDS):
            if (
                item.role is not ItemRole.ITEM
                or item.parent_key
                or item.depth
                or item.state & ItemState.EXPANDABLE
            ):
                raise ValueError(
                    "a list, table, or cards item is a top-level item that cannot expand"
                )
            return
        if self.role is ItemViewRole.TREE:
            if item.role is not ItemRole.ITEM:
                raise ValueError("every tree item has the item role")
            if not item.parent_key:
                if item.depth:
                    raise ValueError("a top-level tree item has depth zero")
                return
            if not item.depth:
                raise ValueError("a tree item with a parent has depth one or more")
            if parent is not None and (
                item.depth != parent.depth + 1
                or not parent.state & ItemState.EXPANDABLE
                or not parent.state & ItemState.EXPANDED
            ):
                raise ValueError(
                    "a tree item is one deeper than its parent, which is expanded"
                )
            return
        # SECTIONS
        if item.state & ItemState.EXPANDABLE:
            raise ValueError("items in sections cannot expand")
        if item.role is ItemRole.SECTION:
            if item.parent_key or item.depth:
                raise ValueError("a section has parent zero and depth zero")
            return
        if not item.parent_key or item.depth != 1:
            raise ValueError("an item in sections has depth one and names its section")
        if parent is not None and parent.role is not ItemRole.SECTION:
            raise ValueError("an item's parent in sections is a section")

    @property
    def direction(self) -> int:
        """Every field's and label's paragraph direction: 0 AUTO, 1 LTR, or
        2 RTL, numbered as text_rules numbers them."""

        return int(self.flags) & 3

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return self._wire_bytes

    def item(self, item_key: int) -> ViewItem | None:
        """The carried item with this key, or None."""

        return self._by_key.get(item_key)

    def shown_items(self) -> tuple[ViewItem, ...]:
        """The carried items in the viewport, in order."""

        end = self.viewport_first + self.viewport_count
        return tuple(
            item for item in self.items if self.viewport_first <= item.ordinal < end
        )


def encode_item_view_content(content: ItemViewContent) -> bytes:
    """Encode one exact ITM1 value without imposing an extra capacity."""

    if not isinstance(content, ItemViewContent):
        raise TypeError("content must be ItemViewContent")
    result = bytearray(content.wire_bytes)
    _HEADER.pack_into(
        result,
        0,
        ITEM_VIEW_TAG,
        ITEM_VIEW_VERSION,
        0,
        content.content_revision,
        int(content.role),
        int(content.flags),
        len(content.columns),
        content.item_total,
        content.viewport_first,
        content.viewport_count,
        len(content.items),
    )
    offset = _HEADER.size
    for column in content.columns:
        label = column.label.encode("utf-8", "strict")
        _COLUMN.pack_into(result, offset, int(column.kind), 0, len(label))
        offset += _COLUMN.size
        result[offset : offset + len(label)] = label
        offset += len(label)
    for item in content.items:
        _ITEM.pack_into(
            result,
            offset,
            item.item_key,
            item.parent_key,
            item.ordinal,
            item.depth,
            int(item.state),
            int(item.role),
            len(item.fields),
            0,
        )
        offset += _ITEM.size
        for item_field in item.fields:
            text = item_field.text.encode("utf-8", "strict")
            _FIELD.pack_into(result, offset, len(text), len(item_field.runs))
            offset += _FIELD.size
            result[offset : offset + len(text)] = text
            offset += len(text)
            for run in item_field.runs:
                _STYLE_RUN.pack_into(
                    result, offset, run.start, run.length, int(run.meaning), 0
                )
                offset += _STYLE_RUN.size
    return bytes(result)


def _fail(code: SemanticContentErrorCode, detail: str) -> SemanticContentError:
    return SemanticContentError(code, detail)


def _decode_text(raw: bytes, what: str) -> str:
    try:
        text = raw.decode("utf-8", "strict")
    except UnicodeDecodeError as exc:
        raise _fail(
            SemanticContentErrorCode.SCALAR, f"{what} is not well-formed UTF-8"
        ) from exc
    if _has_control(text):
        raise _fail(SemanticContentErrorCode.SCALAR, f"{what} contains a control character")
    return text


def decode_item_view_content(payload) -> ItemViewContent:
    """Decode one exact ITM1 value and reject all non-canonical bytes."""

    if isinstance(payload, str):
        raise TypeError("item view content must be bytes-like, not str")
    try:
        raw = bytes(payload)
    except (TypeError, ValueError) as exc:
        raise TypeError("item view content must be bytes-like") from exc
    if len(raw) < _HEADER.size:
        raise _fail(
            SemanticContentErrorCode.PAYLOAD,
            "item view content is shorter than its fixed header",
        )
    (
        tag,
        version,
        reserved0,
        revision,
        role_value,
        flags,
        column_count,
        item_total,
        viewport_first,
        viewport_count,
        item_count,
    ) = _HEADER.unpack_from(raw)
    if tag != ITEM_VIEW_TAG:
        raise _fail(SemanticContentErrorCode.CONSISTENCY, "item view content tag is not ITM1")
    if version != ITEM_VIEW_VERSION:
        raise _fail(
            SemanticContentErrorCode.ENUM,
            f"item view content version {version} is not canonical",
        )
    if reserved0:
        raise _fail(
            SemanticContentErrorCode.RESERVED, "item view content reserved field is nonzero"
        )
    try:
        role = ItemViewRole(role_value)
    except ValueError as exc:
        raise _fail(
            SemanticContentErrorCode.ENUM, f"item view role {role_value} is not canonical"
        ) from exc
    if flags & ~int(ITEM_VIEW_FLAG_MASK):
        raise _fail(
            SemanticContentErrorCode.RESERVED, "item view flags contain reserved bits"
        )
    if flags == int(ITEM_VIEW_FLAG_MASK):
        raise _fail(SemanticContentErrorCode.ENUM, "item view direction 3 is invalid")
    remaining = len(raw) - _HEADER.size
    if column_count > remaining // _COLUMN.size:
        raise _fail(
            SemanticContentErrorCode.PAYLOAD,
            "item view column count cannot fit the content payload",
        )

    offset = _HEADER.size
    columns: list[ItemColumn] = []
    for _ in range(column_count):
        if _COLUMN.size > len(raw) - offset:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a column record is truncated")
        kind_value, reserved, label_bytes = _COLUMN.unpack_from(raw, offset)
        offset += _COLUMN.size
        if reserved:
            raise _fail(
                SemanticContentErrorCode.RESERVED, "a column's reserved field is nonzero"
            )
        try:
            kind = ItemColumnKind(kind_value)
        except ValueError as exc:
            raise _fail(
                SemanticContentErrorCode.ENUM, f"column kind {kind_value} is not canonical"
            ) from exc
        if label_bytes > len(raw) - offset:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "a column label is truncated")
        label = _decode_text(raw[offset : offset + label_bytes], "a column label")
        offset += label_bytes
        columns.append(ItemColumn(kind, label))

    if item_count > (len(raw) - offset) // _ITEM.size:
        raise _fail(
            SemanticContentErrorCode.PAYLOAD,
            "item count cannot fit the content payload",
        )
    items: list[ViewItem] = []
    for _ in range(item_count):
        if _ITEM.size > len(raw) - offset:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "an item header is truncated")
        (
            item_key,
            parent_key,
            ordinal,
            depth,
            state,
            item_role_value,
            field_count,
            reserved,
        ) = _ITEM.unpack_from(raw, offset)
        offset += _ITEM.size
        if reserved:
            raise _fail(
                SemanticContentErrorCode.RESERVED, "an item's reserved field is nonzero"
            )
        if state & ~int(ITEM_STATE_MASK):
            raise _fail(
                SemanticContentErrorCode.RESERVED, "an item's state contains reserved bits"
            )
        try:
            item_role = ItemRole(item_role_value)
        except ValueError as exc:
            raise _fail(
                SemanticContentErrorCode.ENUM,
                f"item role {item_role_value} is not canonical",
            ) from exc
        if field_count > (len(raw) - offset) // _FIELD.size:
            raise _fail(SemanticContentErrorCode.PAYLOAD, "an item's fields are truncated")
        item_fields: list[ItemField] = []
        for _ in range(field_count):
            if _FIELD.size > len(raw) - offset:
                raise _fail(SemanticContentErrorCode.PAYLOAD, "a field record is truncated")
            text_bytes, run_count = _FIELD.unpack_from(raw, offset)
            offset += _FIELD.size
            if text_bytes > len(raw) - offset:
                raise _fail(SemanticContentErrorCode.PAYLOAD, "a field's text is truncated")
            text = _decode_text(raw[offset : offset + text_bytes], "a field")
            offset += text_bytes
            if run_count > (len(raw) - offset) // _STYLE_RUN.size:
                raise _fail(
                    SemanticContentErrorCode.PAYLOAD, "a field's style runs are truncated"
                )
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
                item_fields.append(ItemField(text, tuple(runs)))
            except (TypeError, ValueError) as exc:
                raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc
        try:
            items.append(
                ViewItem(
                    item_key=item_key,
                    parent_key=parent_key,
                    ordinal=ordinal,
                    depth=depth,
                    state=ItemState(state),
                    role=item_role,
                    fields=tuple(item_fields),
                )
            )
        except (TypeError, ValueError) as exc:
            raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc
    if offset != len(raw):
        raise _fail(SemanticContentErrorCode.PAYLOAD, "item view content has trailing bytes")
    try:
        return ItemViewContent(
            content_revision=revision,
            role=role,
            flags=ItemViewFlag(flags),
            columns=tuple(columns),
            item_total=item_total,
            viewport_first=viewport_first,
            viewport_count=viewport_count,
            items=tuple(items),
        )
    except (TypeError, ValueError) as exc:
        raise _fail(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc


__all__ = [
    "ITEM_STATE_MASK",
    "ITEM_VIEW_FLAG_MASK",
    "ITEM_VIEW_TAG",
    "ITEM_VIEW_VERSION",
    "ItemColumn",
    "ItemColumnKind",
    "ItemField",
    "ItemRole",
    "ItemState",
    "ItemViewContent",
    "ItemViewFlag",
    "ItemViewRole",
    "ViewItem",
    "decode_item_view_content",
    "encode_item_view_content",
]
