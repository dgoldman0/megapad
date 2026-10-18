"""Immutable DIALOG and BUTTON content (RETAINED-1 sections 9.3 and 9.4).

A dialog carries its severity, a NOTICE flag, and its message lines in a
guest-declared rectangle; its title is the CONTROL label, shown in a title
rectangle. The client breaks the message into lines and the renderer never
rewraps them. A button carries only its role flags. Neither adds a
family-local limit: wire widths and the enclosing frame, transaction, owner
object, and aggregate UTF-8 limits bound this content.
"""

from __future__ import annotations

import struct
from dataclasses import dataclass, field
from enum import IntFlag

from .apt1 import UINT32_MAX, UINT64_MAX
from .semantic_content import SemanticContentError, SemanticContentErrorCode, _integer
from .semantic_fields import FieldRect


DIALOG_CONTENT_TAG = 0x31474C44  # little-endian DLG1
DIALOG_CONTENT_VERSION = 1
DIALOG_FLAG_MASK = 1
BUTTON_CONTENT_TAG = 0x314E5442  # little-endian BTN1
BUTTON_CONTENT_VERSION = 1
BUTTON_FLAG_MASK = 7
# Severities are STATUS_FIELD's: NEUTRAL 0, INFO 1, SUCCESS 2, WARNING 3, ERROR 4.
DIALOG_SEVERITY_MAX = 4
_DIALOG_HEADER = struct.Struct("<IHHQIIiiIIiiII")
_BUTTON = struct.Struct("<IHH")


class DialogFlag(IntFlag):
    NOTICE = 1


class ButtonFlag(IntFlag):
    DEFAULT = 1
    CANCEL = 2
    DESTRUCTIVE = 4


def _forbidden(character: str, *, line_feed: bool) -> bool:
    code = ord(character)
    if code == 0x0A:
        return not line_feed
    return code < 0x20 or 0x7F <= code <= 0x9F or character in "  "


def _clean(name: str, value: str, *, line_feed: bool = False) -> bytes:
    if not isinstance(value, str):
        raise TypeError(f"{name} must be str")
    if any(_forbidden(character, line_feed=line_feed) for character in value):
        raise ValueError(f"{name} contains a control or line-separator character")
    try:
        encoded = value.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc
    if len(encoded) > UINT32_MAX:
        raise ValueError(f"{name} byte count exceeds u32")
    return encoded


@dataclass(frozen=True, slots=True)
class DialogContent:
    content_revision: int
    flags: DialogFlag
    severity: int
    title_bounds: FieldRect
    message_bounds: FieldRect
    message: str = ""
    _message_bytes: int = field(init=False, repr=False, compare=False)
    _lines: tuple[str, ...] = field(init=False, repr=False, compare=False)

    def __post_init__(self) -> None:
        object.__setattr__(self, "content_revision", _integer(
            "content_revision", self.content_revision, minimum=1, maximum=UINT64_MAX))
        flags = _integer("flags", self.flags, minimum=0, maximum=0xFFFF)
        if flags & ~DIALOG_FLAG_MASK:
            raise ValueError("dialog flags contain reserved bits")
        object.__setattr__(self, "flags", DialogFlag(flags))
        object.__setattr__(self, "severity", _integer(
            "severity", self.severity, minimum=0, maximum=DIALOG_SEVERITY_MAX))
        for name in ("title_bounds", "message_bounds"):
            if not isinstance(getattr(self, name), FieldRect):
                raise TypeError(f"{name} must be FieldRect")
        encoded = _clean("dialog message", self.message, line_feed=True)
        lines = tuple(self.message.split("\n")) if self.message else ()
        if bool(self.message) == self.message_bounds.empty:
            raise ValueError(
                "message bounds must be positive exactly when the message is nonempty")
        if len(lines) > self.message_bounds.rows:
            raise ValueError("a dialog message has more lines than its rectangle has rows")
        object.__setattr__(self, "_message_bytes", len(encoded))
        object.__setattr__(self, "_lines", lines)

    @property
    def notice(self) -> bool:
        return bool(self.flags & DialogFlag.NOTICE)

    @property
    def lines(self) -> tuple[str, ...]:
        return self._lines

    @property
    def utf8_bytes(self) -> int:
        return self._message_bytes

    @property
    def text_bytes(self) -> int:
        return self._message_bytes

    @property
    def object_slots(self) -> int:
        return 1

    @property
    def wire_bytes(self) -> int:
        return _DIALOG_HEADER.size + self._message_bytes

    def validate_geometry(self, *, cols: int, rows: int, title: str) -> None:
        """Check the rectangles against the enclosing DIALOG bounds and title."""
        cols = _integer("cols", cols, minimum=1, maximum=UINT32_MAX)
        rows = _integer("rows", rows, minimum=1, maximum=UINT32_MAX)
        _clean("dialog title", title)
        if bool(title) == self.title_bounds.empty:
            raise ValueError(
                "title bounds must be positive exactly when the CONTROL label is nonempty")
        for name in ("title_bounds", "message_bounds"):
            bounds = getattr(self, name)
            if bounds.empty:
                continue
            if bounds.x < 0 or bounds.y < 0 or bounds.right > cols or bounds.bottom > rows:
                raise ValueError(f"{name} must lie inside the DIALOG root")
        top, body = self.title_bounds, self.message_bounds
        if (not top.empty and not body.empty and top.x < body.right and body.x < top.right
                and top.y < body.bottom and body.y < top.bottom):
            raise ValueError("DIALOG title and message rectangles must not overlap")


@dataclass(frozen=True, slots=True)
class ButtonContent:
    flags: ButtonFlag = ButtonFlag(0)

    def __post_init__(self) -> None:
        flags = _integer("flags", self.flags, minimum=0, maximum=0xFFFF)
        if flags & ~BUTTON_FLAG_MASK:
            raise ValueError("button flags contain reserved bits")
        if flags & ButtonFlag.DESTRUCTIVE and flags & ButtonFlag.DEFAULT:
            raise ValueError("a destructive button is never the default")
        object.__setattr__(self, "flags", ButtonFlag(flags))

    @property
    def default(self) -> bool:
        return bool(self.flags & ButtonFlag.DEFAULT)

    @property
    def cancel(self) -> bool:
        return bool(self.flags & ButtonFlag.CANCEL)

    @property
    def destructive(self) -> bool:
        return bool(self.flags & ButtonFlag.DESTRUCTIVE)

    @property
    def utf8_bytes(self) -> int:
        return 0

    @property
    def text_bytes(self) -> int:
        return 0

    @property
    def object_slots(self) -> int:
        return 1

    @property
    def wire_bytes(self) -> int:
        return _BUTTON.size


def encode_dialog_content(content: DialogContent) -> bytes:
    if not isinstance(content, DialogContent):
        raise TypeError("content must be DialogContent")
    message = content.message.encode("utf-8", "strict")
    return _DIALOG_HEADER.pack(
        DIALOG_CONTENT_TAG, DIALOG_CONTENT_VERSION, int(content.flags),
        content.content_revision, int(content.severity), len(message),
        *content.title_bounds.as_tuple(), *content.message_bounds.as_tuple(),
    ) + message


def encode_button_content(content: ButtonContent) -> bytes:
    if not isinstance(content, ButtonContent):
        raise TypeError("content must be ButtonContent")
    return _BUTTON.pack(BUTTON_CONTENT_TAG, BUTTON_CONTENT_VERSION, int(content.flags))


def _raw(payload, name: str) -> bytes:
    try:
        return memoryview(payload).tobytes()
    except TypeError as exc:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   f"{name} content must be bytes-like") from exc


def decode_dialog_content(payload) -> DialogContent:
    raw = _raw(payload, "DIALOG")
    if len(raw) < _DIALOG_HEADER.size:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "DIALOG content header is truncated")
    (tag, version, flags, revision, severity, message_bytes,
     *rects) = _DIALOG_HEADER.unpack_from(raw)
    if tag != DIALOG_CONTENT_TAG or version != DIALOG_CONTENT_VERSION:
        raise SemanticContentError(SemanticContentErrorCode.ENUM,
                                   "DIALOG content tag or version is unsupported")
    if flags & ~DIALOG_FLAG_MASK:
        raise SemanticContentError(SemanticContentErrorCode.RESERVED,
                                   "DIALOG flags are noncanonical")
    if severity > DIALOG_SEVERITY_MAX:
        raise SemanticContentError(SemanticContentErrorCode.ENUM,
                                   "DIALOG severity is reserved")
    if message_bytes != len(raw) - _DIALOG_HEADER.size:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "DIALOG message length or trailing bytes are noncanonical")
    try:
        message = raw[_DIALOG_HEADER.size:].decode("utf-8", "strict")
        _clean("dialog message", message, line_feed=True)
    except (UnicodeDecodeError, ValueError) as exc:
        raise SemanticContentError(SemanticContentErrorCode.SCALAR, str(exc)) from exc
    try:
        return DialogContent(revision, flags, severity, FieldRect(*rects[:4]),
                             FieldRect(*rects[4:]), message)
    except (TypeError, ValueError) as exc:
        raise SemanticContentError(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc


def decode_button_content(payload) -> ButtonContent:
    raw = _raw(payload, "BUTTON")
    if len(raw) != _BUTTON.size:
        raise SemanticContentError(SemanticContentErrorCode.PAYLOAD,
                                   "BUTTON content must be exactly 8 bytes")
    tag, version, flags = _BUTTON.unpack(raw)
    if tag != BUTTON_CONTENT_TAG or version != BUTTON_CONTENT_VERSION:
        raise SemanticContentError(SemanticContentErrorCode.ENUM,
                                   "BUTTON content tag or version is unsupported")
    if flags & ~BUTTON_FLAG_MASK:
        raise SemanticContentError(SemanticContentErrorCode.RESERVED,
                                   "BUTTON flags are noncanonical")
    try:
        return ButtonContent(ButtonFlag(flags))
    except (TypeError, ValueError) as exc:
        raise SemanticContentError(SemanticContentErrorCode.CONSISTENCY, str(exc)) from exc


__all__ = [
    "BUTTON_CONTENT_TAG", "BUTTON_CONTENT_VERSION", "BUTTON_FLAG_MASK",
    "ButtonContent", "ButtonFlag",
    "DIALOG_CONTENT_TAG", "DIALOG_CONTENT_VERSION", "DIALOG_FLAG_MASK",
    "DialogContent", "DialogFlag",
    "decode_button_content", "decode_dialog_content",
    "encode_button_content", "encode_dialog_content",
]
