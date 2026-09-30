"""Immutable typed values and explicit cell slots for semantic FIELD controls.

FDC1 carries committed guest state. Host-side editing, input acknowledgement,
and the CONTROL identity/envelope belong to their existing outer protocols.
There is no family-local item or text limit: wire widths and enclosing frame,
transaction, owner object, and aggregate UTF-8 limits bound this content.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass, field
from enum import IntEnum, IntFlag

from .apt1 import UINT32_MAX, UINT64_MAX
from .semantic_content import SemanticContentError, SemanticContentErrorCode, _integer


FIELD_CONTENT_TAG = 0x31434446  # little-endian FDC1
FIELD_CONTENT_VERSION = 1
FIELD_FLAG_MASK = 1
INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
INT64_MIN = -(1 << 63)
INT64_MAX = (1 << 63) - 1
_HEADER = struct.Struct("<IHHQIIiiIIiiIIqqqqII")
_CHOICE = struct.Struct("<qII")


class FieldKind(IntEnum):
    INTEGER = 1
    CHOICE = 2
    TEXT = 3


class FieldFlag(IntFlag):
    READ_ONLY = 1


def _clean_text(name: str, value: str) -> bytes:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    if any(ord(character) < 0x20 or 0x7F <= ord(character) <= 0x9F
           or character in "\u2028\u2029" for character in value):
        raise ValueError(f"{name} contains a control or line-separator character")
    try:
        encoded = value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc
    if len(encoded) > UINT32_MAX:
        raise ValueError(f"{name} byte count exceeds u32")
    return encoded


@dataclass(frozen=True, slots=True)
class FieldRect:
    """A root-relative signed origin and positive extents, or canonical zero."""

    x: int
    y: int
    cols: int
    rows: int

    def __post_init__(self) -> None:
        for name in ("x", "y"):
            object.__setattr__(self, name, _integer(
                name, getattr(self, name), minimum=INT32_MIN, maximum=INT32_MAX))
        for name in ("cols", "rows"):
            object.__setattr__(self, name, _integer(
                name, getattr(self, name), minimum=0, maximum=UINT32_MAX))
        if (self.cols == 0 or self.rows == 0) and any(self.as_tuple()):
            raise ValueError("an empty field rectangle must be canonical all-zero")

    def as_tuple(self) -> tuple[int, int, int, int]:
        return self.x, self.y, self.cols, self.rows

    @property
    def empty(self) -> bool:
        return self.cols == 0

    @property
    def right(self) -> int:
        return self.x + self.cols

    @property
    def bottom(self) -> int:
        return self.y + self.rows


@dataclass(frozen=True, slots=True)
class FieldChoice:
    value: int
    label: str
    _utf8_bytes: int = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "value", _integer(
            "choice value", self.value, minimum=INT64_MIN, maximum=INT64_MAX))
        encoded = _clean_text("choice label", self.label)
        if not encoded:
            raise ValueError("a field choice label must be nonempty")
        object.__setattr__(self, "_utf8_bytes", len(encoded))

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def wire_bytes(self) -> int:
        return _CHOICE.size + self._utf8_bytes


@dataclass(frozen=True, slots=True)
class FieldContent:
    content_revision: int
    kind: FieldKind
    flags: FieldFlag
    label_bounds: FieldRect
    value_bounds: FieldRect
    value: int = 0
    minimum: int = 0
    maximum: int = 0
    step: int = 0
    choices: tuple[FieldChoice, ...] = ()
    text: str = ""
    _utf8_bytes: int = field(init=False, repr=False, compare=False)
    _display_text: str = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "content_revision", _integer(
            "content_revision", self.content_revision, minimum=1, maximum=UINT64_MAX))
        kind = FieldKind(_integer("kind", self.kind, minimum=1, maximum=3))
        flags = _integer("flags", self.flags, minimum=0, maximum=UINT32_MAX)
        if flags & ~FIELD_FLAG_MASK:
            raise ValueError("field flags contain reserved bits")
        object.__setattr__(self, "kind", kind)
        object.__setattr__(self, "flags", FieldFlag(flags))
        for name in ("label_bounds", "value_bounds"):
            if not isinstance(getattr(self, name), FieldRect):
                raise TypeError(f"{name} must be FieldRect")
        if self.value_bounds.empty:
            raise ValueError("a FIELD requires a positive value rectangle")
        for name in ("value", "minimum", "maximum", "step"):
            object.__setattr__(self, name, _integer(
                name, getattr(self, name), minimum=INT64_MIN, maximum=INT64_MAX))
        choices = tuple(self.choices)
        if any(not isinstance(choice, FieldChoice) for choice in choices):
            raise TypeError("choices must contain only FieldChoice values")
        if len(choices) > UINT32_MAX:
            raise ValueError("choice count exceeds u32")
        object.__setattr__(self, "choices", choices)
        text_bytes = len(_clean_text("field text", self.text))
        if kind is FieldKind.INTEGER:
            if not self.minimum <= self.value <= self.maximum:
                raise ValueError("integer field value must lie in its inclusive range")
            if self.step <= 0:
                raise ValueError("integer field step must be positive")
            if choices or self.text:
                raise ValueError("integer fields carry neither choices nor text")
        elif kind is FieldKind.CHOICE:
            if self.minimum or self.maximum or self.step or self.text:
                raise ValueError("choice fields require zero range, step, and text")
            if not choices:
                raise ValueError("choice fields require at least one choice")
            values = {choice.value for choice in choices}
            if len(values) != len(choices):
                raise ValueError("field choice values must be unique")
            if self.value not in values:
                raise ValueError("the current field value must be a declared choice")
        else:
            if self.value or self.minimum or self.maximum or self.step or choices:
                raise ValueError("text fields require zero numeric values and no choices")
        aggregate = text_bytes + sum(choice.utf8_bytes for choice in choices)
        if aggregate > UINT64_MAX:
            raise ValueError("aggregate field UTF-8 bytes exceed u64")
        object.__setattr__(self, "_utf8_bytes", aggregate)
        display_text = (str(self.value) if kind is FieldKind.INTEGER else
                        next(choice.label for choice in choices if choice.value == self.value)
                        if kind is FieldKind.CHOICE else self.text)
        object.__setattr__(self, "_display_text", display_text)

    @property
    def display_text(self) -> str:
        return self._display_text

    @property
    def read_only(self) -> bool:
        return bool(self.flags & FieldFlag.READ_ONLY)

    @property
    def is_adjustable(self) -> bool:
        return not self.read_only and self.kind in (FieldKind.INTEGER, FieldKind.CHOICE)

    @property
    def utf8_bytes(self) -> int:
        return self._utf8_bytes

    @property
    def text_bytes(self) -> int:
        """Total charged text bytes, including every choice label."""
        return self._utf8_bytes

    @property
    def object_slots(self) -> int:
        return 1 + len(self.choices)

    @property
    def wire_bytes(self) -> int:
        return _HEADER.size + _CHOICE.size * len(self.choices) + self._utf8_bytes

    def validate_geometry(self, *, cols: int, rows: int, label: str) -> None:
        """Check slots against the enclosing CONTROL bounds and label."""
        cols = _integer("cols", cols, minimum=1, maximum=UINT32_MAX)
        rows = _integer("rows", rows, minimum=1, maximum=UINT32_MAX)
        _clean_text("field label", label)
        if bool(label) == self.label_bounds.empty:
            raise ValueError("label bounds must be positive exactly when the CONTROL label is nonempty")
        for name in ("label_bounds", "value_bounds"):
            bounds = getattr(self, name)
            if bounds.empty:
                continue
            if bounds.x < 0 or bounds.y < 0 or bounds.right > cols or bounds.bottom > rows:
                raise ValueError(f"{name} must lie inside the FIELD root")
        left, right = self.label_bounds, self.value_bounds
        if (not left.empty and left.x < right.right and right.x < left.right
                and left.y < right.bottom and right.y < left.bottom):
            raise ValueError("FIELD label and value rectangles must not overlap")


def encode_field_content(content: FieldContent) -> bytes:
    if not isinstance(content, FieldContent):
        raise TypeError("content must be FieldContent")
    text = content.text.encode("utf-8", "strict")
    encoded = bytearray(_HEADER.pack(
        FIELD_CONTENT_TAG, FIELD_CONTENT_VERSION, int(content.kind),
        content.content_revision, int(content.flags), 0,
        *content.label_bounds.as_tuple(), *content.value_bounds.as_tuple(),
        content.value, content.minimum, content.maximum, content.step,
        len(content.choices), len(text),
    ))
    for choice in content.choices:
        label = choice.label.encode("utf-8", "strict")
        encoded += _CHOICE.pack(choice.value, len(label), 0)
        encoded += label
    encoded += text
    return bytes(encoded)


def _decode_text(raw: bytes, name: str) -> str:
    try:
        result = raw.decode("utf-8", "strict")
        _clean_text(name, result)
        return result
    except (UnicodeDecodeError, ValueError) as exc:
        raise SemanticContentError(SemanticContentErrorCode.SCALAR, str(exc)) from exc


def decode_field_content(payload) -> FieldContent:
    try:
        raw = memoryview(payload).tobytes()
    except TypeError as exc:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "FIELD content must be bytes-like") from exc
    if len(raw) < _HEADER.size:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "FIELD content header is truncated")
    fields = _HEADER.unpack_from(raw)
    tag, version, kind, revision, flags, reserved = fields[:6]
    if tag != FIELD_CONTENT_TAG or version != FIELD_CONTENT_VERSION or kind not in (1, 2, 3):
        raise SemanticContentError(SemanticContentErrorCode.ENUM,
                                   "FIELD content tag, version, or kind is unsupported")
    if flags & ~FIELD_FLAG_MASK or reserved:
        raise SemanticContentError(SemanticContentErrorCode.RESERVED,
                                   "FIELD content flags or reserved field is noncanonical")
    choice_count, text_bytes = fields[-2:]
    offset = _HEADER.size
    if choice_count > (len(raw) - offset) // _CHOICE.size:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "FIELD choice records are truncated")
    choices = []
    try:
        for _ in range(choice_count):
            if len(raw) - offset < _CHOICE.size:
                raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                           "FIELD choice record is truncated")
            value, label_bytes, reserved = _CHOICE.unpack_from(raw, offset)
            offset += _CHOICE.size
            if reserved:
                raise SemanticContentError(SemanticContentErrorCode.RESERVED,
                                           "FIELD choice reserved field is nonzero")
            if label_bytes > len(raw) - offset:
                raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                           "FIELD choice label is truncated")
            label = _decode_text(raw[offset:offset + label_bytes], "choice label")
            offset += label_bytes
            choices.append(FieldChoice(value, label))
        if text_bytes != len(raw) - offset:
            raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                       "FIELD text length or trailing bytes are noncanonical")
        return FieldContent(revision, kind, flags,
                            FieldRect(*fields[6:10]), FieldRect(*fields[10:14]),
                            *fields[14:18], tuple(choices),
                            _decode_text(raw[offset:], "field text"))
    except SemanticContentError:
        raise
    except (TypeError, ValueError) as exc:
        raise SemanticContentError(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc


__all__ = [
    "FIELD_CONTENT_TAG", "FIELD_CONTENT_VERSION", "FIELD_FLAG_MASK",
    "FieldKind", "FieldFlag", "FieldRect", "FieldChoice", "FieldContent",
    "encode_field_content", "decode_field_content",
]
