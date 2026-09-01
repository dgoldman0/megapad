"""Immutable RETAINED-1 scene targets and atomic definition transactions.

The scene model is renderer-neutral.  It owns active and hidden immutable
targets, validates exact owner authority and final references, accounts each
target independently against OWNER_OPEN reservations, and publishes a prepared
scene together with its one atomic owner-ledger high-water candidate.

Resource bytes and uploads are intentionally not in this layer.  Renderer-
neutral IMAGE definitions carry exact-owner resource IDs which are validated
through a read-only resource-store dependency.  Object definitions,
scalar/visibility mutations, and bounded series histories are complete semantic
values; every update preserves the definition-time checks below and publishes
only through the same immutable transaction seam.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass, field, replace
from enum import Enum, IntEnum, IntFlag
from types import MappingProxyType
from typing import Mapping

from .apt1 import UINT32_MAX, UINT64_MAX
from .update_authority import (
    TerminalUpdateAuthority,
    TerminalGeometry,
    TerminalUpdateError,
    ResultLease,
    TransactionFamily,
    TransactionLease,
)
from .retained_model import (
    ItemNamespace,
    OwnerIdentity,
    OwnerLedger,
    OwnerLedgerError,
    OwnerQuotas,
    PreparedOwnerLedgerInstall,
    RetainedFeature,
)
from .retained_resources import ResourceStoreState, RetainedResourceStore
from .semantic_content import SemanticTextContent


INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
INT64_MIN = -(1 << 63)
INT64_MAX = (1 << 63) - 1

# GLYPH_RUN is the physically exact styled-cell primitive used by the current
# rich view.  Blink is deliberately not admitted: it requires a presentation
# phase/cadence contract that this primitive does not carry.  Rejecting it at
# every boundary prevents a view from acknowledging pixels that silently omit
# a requested CELL style.
GLYPH_RUN_ATTRIBUTE_MASK = 0x006F


class SceneErrorCode(str, Enum):
    STATE = "STATE"
    AUTHORITY = "AUTHORITY"
    DUPLICATE_ID = "DUPLICATE_ID"
    MISSING_ID = "MISSING_ID"
    BOUNDS = "BOUNDS"
    FEATURE = "FEATURE"
    QUOTA = "QUOTA"
    GRAPH = "GRAPH"


class SceneModelError(ValueError):
    def __init__(self, code: SceneErrorCode, detail: str):
        self.code = code
        self.detail = detail
        super().__init__(f"{code.value}: {detail}")


class RetainedMode(IntEnum):
    DELTA = 1
    REPLACE_START = 2
    REPLACE_CONTINUE = 3
    LAYOUT_START = 4
    LAYOUT_CONTINUE = 5


class CommitDisposition(IntEnum):
    COMMIT = 0
    COMMIT_AND_REVEAL = 1


class HiddenTargetKind(str, Enum):
    REPLACE = "REPLACE"
    LAYOUT = "LAYOUT"


class RebuildRequirement(str, Enum):
    REPLACE = "REPLACE"
    LAYOUT = "LAYOUT"


class ObjectKind(IntEnum):
    GROUP = 1
    POLYLINE = 2
    IMAGE = 3
    GLYPH_RUN = 4
    READOUT = 5
    METER = 6
    STATUS = 7
    PLOT = 8
    WAVEFORM = 9


class ImageFit(IntEnum):
    STRETCH = 0
    CONTAIN = 1
    COVER = 2


class ControlKind(IntEnum):
    """Renderer-neutral semantic controls in the CONTROL namespace."""

    MENU_BAR = 1
    MENU = 2
    MENU_ITEM = 3
    MENU_SEPARATOR = 4
    TEXT_AREA = 5
    TEXT_GRID = 6
    TABSET = 7
    TAB = 8


class ControlState(IntFlag):
    """Authoritative guest state; transient hover/press state stays terminal-owned."""

    VISIBLE = 1 << 0
    ENABLED = 1 << 1
    OPEN = 1 << 2
    SELECTED = 1 << 3
    CHECKED = 1 << 4


CONTROL_STATE_MASK = (
    ControlState.VISIBLE
    | ControlState.ENABLED
    | ControlState.OPEN
    | ControlState.SELECTED
    | ControlState.CHECKED
)


class ReadoutFormat(IntEnum):
    INTEGER = 0
    FIXED = 1
    PERCENT = 2


class TimestampMode(IntEnum):
    EXPLICIT = 0
    UNIFORM = 1


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


def _boolean(name: str, value) -> bool:
    if not isinstance(value, bool):
        raise TypeError(f"{name} must be bool")
    return value


def _text_bytes(name: str, text: str) -> bytes:
    if not isinstance(text, str):
        raise TypeError(f"{name} must be str")
    if "\r" in text or "\n" in text or "\0" in text:
        raise ValueError(f"{name} contains CR, LF, or NUL")
    try:
        return text.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc


def _control_text_bytes(name: str, text: str) -> bytes:
    """Return clean single-line semantic text for renderer-owned controls."""

    encoded = _text_bytes(name, text)
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in text):
        raise ValueError(f"{name} contains a control character")
    return encoded


def _add_usage(name: str, left: int, right: int) -> int:
    if right > UINT64_MAX - left:
        raise SceneModelError(SceneErrorCode.QUOTA, f"{name} overflows uint64")
    return left + right


@dataclass(frozen=True, slots=True)
class RGBA:
    red: int
    green: int
    blue: int
    alpha: int

    def __post_init__(self) -> None:
        for name in ("red", "green", "blue", "alpha"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=0xFF),
            )


@dataclass(frozen=True, slots=True)
class Point:
    x: int
    y: int

    def __post_init__(self) -> None:
        object.__setattr__(self, "x", _integer("x", self.x, minimum=0, maximum=UINT32_MAX))
        object.__setattr__(self, "y", _integer("y", self.y, minimum=0, maximum=UINT32_MAX))


@dataclass(frozen=True, slots=True)
class ObjectBounds:
    left: int
    top: int
    right: int
    bottom: int

    def __post_init__(self) -> None:
        for name in ("left", "top", "right", "bottom"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        if self.left >= self.right or self.top >= self.bottom:
            raise ValueError("object bounds must have positive width and height")


@dataclass(frozen=True, slots=True)
class RegionDefinition:
    owner: OwnerIdentity
    region_id: int
    cell_x: int
    cell_y: int
    cell_cols: int
    cell_rows: int
    z_order: int
    visible: bool
    clipped: bool
    geometry_generation: int

    def __post_init__(self) -> None:
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        for name, minimum, maximum in (
            ("region_id", 1, UINT64_MAX),
            ("cell_x", 0, UINT32_MAX),
            ("cell_y", 0, UINT32_MAX),
            ("cell_cols", 1, UINT32_MAX),
            ("cell_rows", 1, UINT32_MAX),
            ("z_order", INT32_MIN, INT32_MAX),
            ("geometry_generation", 0, UINT64_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        object.__setattr__(self, "visible", _boolean("visible", self.visible))
        object.__setattr__(self, "clipped", _boolean("clipped", self.clipped))

    def validate_geometry(self, geometry: TerminalGeometry) -> None:
        if self.geometry_generation != geometry.generation:
            raise SceneModelError(SceneErrorCode.BOUNDS, "region geometry stamp is stale")
        if (
            self.cell_x > geometry.cols - self.cell_cols
            if self.cell_cols <= geometry.cols
            else True
        ):
            raise SceneModelError(SceneErrorCode.BOUNDS, "region exceeds cell columns")
        if (
            self.cell_y > geometry.rows - self.cell_rows
            if self.cell_rows <= geometry.rows
            else True
        ):
            raise SceneModelError(SceneErrorCode.BOUNDS, "region exceeds cell rows")


@dataclass(frozen=True, slots=True)
class GroupBody:
    pass


@dataclass(frozen=True, slots=True)
class PolylineBody:
    points: tuple[Point, ...]
    stroke_width: int
    color: RGBA
    closed: bool = False

    def __post_init__(self) -> None:
        points = tuple(self.points)
        if len(points) < 2 or any(not isinstance(point, Point) for point in points):
            raise ValueError("polyline requires at least two Point values")
        object.__setattr__(self, "points", points)
        object.__setattr__(
            self,
            "stroke_width",
            _integer("stroke_width", self.stroke_width, minimum=1, maximum=UINT32_MAX),
        )
        if not isinstance(self.color, RGBA):
            raise TypeError("color must be RGBA")
        object.__setattr__(self, "closed", _boolean("closed", self.closed))


@dataclass(frozen=True, slots=True)
class ImageBody:
    """One renderer-neutral reference to an exact-owner immutable RGBA resource."""

    resource_id: int
    fit: ImageFit
    opacity: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "resource_id",
            _integer(
                "resource_id", self.resource_id, minimum=1, maximum=UINT64_MAX
            ),
        )
        if isinstance(self.fit, bool):
            raise TypeError("fit must not be bool")
        try:
            fit = ImageFit(self.fit)
        except (TypeError, ValueError) as exc:
            raise ValueError("fit is not a RETAINED-1 IMAGE fit") from exc
        object.__setattr__(self, "fit", fit)
        object.__setattr__(
            self,
            "opacity",
            _integer("opacity", self.opacity, minimum=0, maximum=0xFF),
        )


@dataclass(frozen=True, slots=True)
class GlyphRunBody:
    foreground: RGBA
    background: RGBA
    attributes: int
    text: str

    def __post_init__(self) -> None:
        if not isinstance(self.foreground, RGBA):
            raise TypeError("foreground must be RGBA")
        if not isinstance(self.background, RGBA):
            raise TypeError("background must be RGBA")
        attributes = _integer(
            "attributes", self.attributes, minimum=0, maximum=0xFFFF
        )
        if attributes & ~GLYPH_RUN_ATTRIBUTE_MASK:
            raise ValueError("attributes contain unsupported GLYPH_RUN bits")
        object.__setattr__(
            self,
            "attributes",
            attributes,
        )
        _text_bytes("text", self.text)


@dataclass(frozen=True, slots=True)
class ReadoutBody:
    foreground: RGBA
    background: RGBA
    format: ReadoutFormat
    decimal_places: int
    value: int
    scale: int
    unit: str

    def __post_init__(self) -> None:
        if not isinstance(self.foreground, RGBA) or not isinstance(self.background, RGBA):
            raise TypeError("readout colors must be RGBA")
        if isinstance(self.format, bool):
            raise TypeError("format must not be bool")
        try:
            normalized_format = ReadoutFormat(self.format)
        except (TypeError, ValueError) as exc:
            raise ValueError("format is not a RETAINED-1 readout format") from exc
        object.__setattr__(self, "format", normalized_format)
        object.__setattr__(
            self,
            "decimal_places",
            _integer("decimal_places", self.decimal_places, minimum=0, maximum=UINT32_MAX),
        )
        object.__setattr__(
            self, "value", _integer("value", self.value, minimum=INT64_MIN, maximum=INT64_MAX)
        )
        object.__setattr__(
            self, "scale", _integer("scale", self.scale, minimum=INT64_MIN, maximum=INT64_MAX)
        )
        _text_bytes("unit", self.unit)
        if normalized_format is ReadoutFormat.INTEGER:
            if self.decimal_places != 0 or self.scale != 1:
                raise ValueError("integer readout requires decimal_places zero and scale one")
        elif self.scale <= 0:
            raise ValueError("fixed and percent readouts require positive scale")

    def formatted_bytes(self, maximum: int) -> bytes:
        maximum = _integer("maximum", maximum, minimum=0, maximum=UINT32_MAX)
        unit = _text_bytes("unit", self.unit)
        negative = self.value < 0
        percent = self.format is ReadoutFormat.PERCENT
        multiplier = 100 if percent else 1
        if self.format is ReadoutFormat.INTEGER:
            integer_part = abs(self.value)
            remainder = 0
            denominator = 1
        else:
            integer_part, remainder = divmod(abs(self.value) * multiplier, self.scale)
            denominator = self.scale
        integer_ascii = str(integer_part).encode("ascii")
        minimum = len(unit) + int(negative) + int(percent) + len(integer_ascii)
        if self.decimal_places:
            minimum += 1 + self.decimal_places
        if minimum > maximum:
            raise SceneModelError(SceneErrorCode.QUOTA, "readout exceeds UTF-8 byte bound")

        if self.format is ReadoutFormat.INTEGER:
            fraction = bytearray()
        else:
            # Generate a caller-bounded number of digits by long division.
            # This avoids both a giant 10**N temporary and Python's decimal
            # conversion digit ceiling while retaining exact rational
            # rounding.  Allocation is bounded by the checked output maximum.
            fraction = bytearray(self.decimal_places)
            for index in range(self.decimal_places):
                digit, remainder = divmod(remainder * 10, denominator)
                fraction[index] = 0x30 + digit
            if remainder * 2 >= denominator:
                position = len(fraction) - 1
                while position >= 0 and fraction[position] == 0x39:
                    fraction[position] = 0x30
                    position -= 1
                if position >= 0:
                    fraction[position] += 1
                else:
                    integer_part += 1
                    integer_ascii = str(integer_part).encode("ascii")
        result = (
            (b"-" if negative else b"")
            + integer_ascii
            + ((b"." + bytes(fraction)) if self.decimal_places else b"")
            + (b"%" if percent else b"")
            + unit
        )
        if len(result) > maximum:
            raise SceneModelError(SceneErrorCode.QUOTA, "readout exceeds UTF-8 byte bound")
        return result


@dataclass(frozen=True, slots=True)
class MeterBody:
    foreground: RGBA
    background: RGBA
    vertical: bool
    show_value: bool
    minimum: int
    maximum: int
    value: int

    def __post_init__(self) -> None:
        if not isinstance(self.foreground, RGBA) or not isinstance(self.background, RGBA):
            raise TypeError("meter colors must be RGBA")
        object.__setattr__(self, "vertical", _boolean("vertical", self.vertical))
        object.__setattr__(self, "show_value", _boolean("show_value", self.show_value))
        for name in ("minimum", "maximum", "value"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=INT64_MIN, maximum=INT64_MAX),
            )
        if self.minimum >= self.maximum or not self.minimum <= self.value <= self.maximum:
            raise ValueError("meter range/value is invalid")


@dataclass(frozen=True, slots=True)
class StatusBody:
    inactive: RGBA
    active: RGBA
    value: int
    shape: int

    def __post_init__(self) -> None:
        if not isinstance(self.inactive, RGBA) or not isinstance(self.active, RGBA):
            raise TypeError("status colors must be RGBA")
        object.__setattr__(
            self, "value", _integer("value", self.value, minimum=INT64_MIN, maximum=INT64_MAX)
        )
        object.__setattr__(self, "shape", _integer("shape", self.shape, minimum=0, maximum=2))


@dataclass(frozen=True, slots=True)
class PlotBody:
    series_id: int
    minimum: int
    maximum: int
    line: RGBA
    fill: RGBA
    fill_to_minimum: bool = False
    draw_points: bool = False

    def __post_init__(self) -> None:
        _validate_series_consumer(self, include_zero_line=False)
        object.__setattr__(self, "fill_to_minimum", _boolean("fill_to_minimum", self.fill_to_minimum))
        object.__setattr__(self, "draw_points", _boolean("draw_points", self.draw_points))


@dataclass(frozen=True, slots=True)
class WaveformBody:
    series_id: int
    minimum: int
    maximum: int
    trace: RGBA
    zero_line: RGBA
    zero_value: int
    draw_zero_line: bool = False

    def __post_init__(self) -> None:
        _validate_series_consumer(self, include_zero_line=True)
        object.__setattr__(
            self, "draw_zero_line", _boolean("draw_zero_line", self.draw_zero_line)
        )


def _validate_series_consumer(body, *, include_zero_line: bool) -> None:
    object.__setattr__(
        body, "series_id", _integer("series_id", body.series_id, minimum=1, maximum=UINT64_MAX)
    )
    for name in ("minimum", "maximum"):
        object.__setattr__(
            body,
            name,
            _integer(name, getattr(body, name), minimum=INT64_MIN, maximum=INT64_MAX),
        )
    if body.minimum >= body.maximum:
        raise ValueError("series consumer minimum must be less than maximum")
    colors = (body.trace, body.zero_line) if include_zero_line else (body.line, body.fill)
    if any(not isinstance(color, RGBA) for color in colors):
        raise TypeError("series consumer colors must be RGBA")
    if include_zero_line:
        object.__setattr__(
            body,
            "zero_value",
            _integer("zero_value", body.zero_value, minimum=INT64_MIN, maximum=INT64_MAX),
        )
        if not body.minimum <= body.zero_value <= body.maximum:
            raise ValueError("waveform zero line is outside its range")


ObjectBody = (
    GroupBody
    | PolylineBody
    | ImageBody
    | GlyphRunBody
    | ReadoutBody
    | MeterBody
    | StatusBody
    | PlotBody
    | WaveformBody
)


_BODY_KIND = {
    GroupBody: ObjectKind.GROUP,
    PolylineBody: ObjectKind.POLYLINE,
    ImageBody: ObjectKind.IMAGE,
    GlyphRunBody: ObjectKind.GLYPH_RUN,
    ReadoutBody: ObjectKind.READOUT,
    MeterBody: ObjectKind.METER,
    StatusBody: ObjectKind.STATUS,
    PlotBody: ObjectKind.PLOT,
    WaveformBody: ObjectKind.WAVEFORM,
}


@dataclass(frozen=True, slots=True)
class ObjectDefinition:
    owner: OwnerIdentity
    object_id: int
    region_id: int
    parent_object_id: int
    bounds: ObjectBounds
    z_order: int
    visible: bool
    body: ObjectBody

    def __post_init__(self) -> None:
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        for name, minimum in (("object_id", 1), ("region_id", 1), ("parent_object_id", 0)):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        object.__setattr__(
            self, "z_order", _integer("z_order", self.z_order, minimum=INT32_MIN, maximum=INT32_MAX)
        )
        object.__setattr__(self, "visible", _boolean("visible", self.visible))
        if type(self.body) not in _BODY_KIND:
            raise TypeError("body is not a supported retained object body")

    @property
    def kind(self) -> ObjectKind:
        return _BODY_KIND[type(self.body)]


def validate_control_shape(
    *,
    kind,
    state,
    z_order: int,
    parent_control_id: int,
    order: int,
    bounds: ObjectBounds | None,
    label: str,
    shortcut: str,
    content: SemanticTextContent | None,
) -> tuple[ControlKind, ControlState]:
    """Validate the common scene/wire shape of one semantic control.

    Authority, IDs, region existence, graph siblings, policy, and quotas belong
    to their later boundaries.  Keeping kind/state/content shape here prevents
    the wire value and immutable scene value from drifting into two schemas.
    """

    if isinstance(kind, bool):
        raise TypeError("kind must not be bool")
    try:
        normalized_kind = ControlKind(kind)
    except (TypeError, ValueError) as exc:
        raise ValueError("kind is not a CONTROL-1 control kind") from exc
    if isinstance(state, bool):
        raise TypeError("state must not be bool")
    try:
        state_bits = operator.index(state)
    except TypeError as exc:
        raise TypeError("state must be ControlState-compatible") from exc
    if state_bits < 0 or state_bits > 0xFFFF:
        raise ValueError("state must fit u16")
    normalized_state = ControlState(state_bits)
    if int(normalized_state) & ~int(CONTROL_STATE_MASK):
        raise ValueError("state contains reserved CONTROL-1 bits")
    if bounds is not None and not isinstance(bounds, ObjectBounds):
        raise TypeError("bounds must be ObjectBounds or None")
    label_bytes = _control_text_bytes("label", label)
    shortcut_bytes = _control_text_bytes("shortcut", shortcut)
    if content is not None and not isinstance(content, SemanticTextContent):
        raise TypeError("content must be SemanticTextContent or None")

    allowed = {
        ControlKind.MENU_BAR: ControlState.VISIBLE | ControlState.ENABLED,
        ControlKind.MENU: (
            ControlState.VISIBLE
            | ControlState.ENABLED
            | ControlState.OPEN
            | ControlState.SELECTED
        ),
        ControlKind.MENU_ITEM: (
            ControlState.VISIBLE
            | ControlState.ENABLED
            | ControlState.SELECTED
            | ControlState.CHECKED
        ),
        ControlKind.MENU_SEPARATOR: ControlState.VISIBLE,
        ControlKind.TEXT_AREA: (
            ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
        ),
        ControlKind.TEXT_GRID: (
            ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
        ),
        ControlKind.TABSET: ControlState.VISIBLE | ControlState.ENABLED,
        ControlKind.TAB: (
            ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED
        ),
    }[normalized_kind]
    if int(normalized_state) & ~int(allowed):
        raise ValueError(
            f"state contains bits not defined for {normalized_kind.name}"
        )
    if normalized_state & (ControlState.OPEN | ControlState.SELECTED) and not (
        normalized_state & ControlState.VISIBLE
        and normalized_state & ControlState.ENABLED
    ):
        raise ValueError("open or selected controls must be visible and enabled")

    root_kinds = {
        ControlKind.MENU_BAR,
        ControlKind.TEXT_AREA,
        ControlKind.TEXT_GRID,
        ControlKind.TABSET,
    }
    if normalized_kind in root_kinds:
        if parent_control_id or order or bounds is None:
            raise ValueError(
                f"{normalized_kind.name} requires root order zero and positive bounds"
            )
        if label_bytes or shortcut_bytes:
            raise ValueError(
                f"{normalized_kind.name} carries no label or shortcut"
            )
        if normalized_kind in (ControlKind.MENU_BAR, ControlKind.TABSET):
            if content is not None:
                raise ValueError(
                    f"{normalized_kind.name} carries no semantic text content"
                )
        elif content is None:
            raise ValueError(
                f"{normalized_kind.name} requires semantic text content"
            )
        elif normalized_kind is ControlKind.TEXT_AREA:
            if not content.text_area_compatible:
                raise ValueError(
                    "TEXT_AREA items must be bounded full-row state-zero CONTENT values"
                )
        else:
            if content.anchor_key or content.anchor_offset or content.primary_offset:
                raise ValueError(
                    "TEXT_GRID positions name whole items and require zero offsets"
                )
            if content.current_item_count > 1:
                raise ValueError("TEXT_GRID has more than one current item")
    else:
        if parent_control_id == 0 or bounds is not None or z_order != 0:
            raise ValueError(
                f"{normalized_kind.name} requires a parent and renderer-owned geometry"
            )
        if content is not None:
            raise ValueError(
                f"{normalized_kind.name} carries no semantic text content"
            )
        if normalized_kind in (
            ControlKind.MENU,
            ControlKind.MENU_ITEM,
            ControlKind.TAB,
        ):
            if not label_bytes:
                raise ValueError(
                    f"{normalized_kind.name} requires a nonempty label"
                )
        elif label_bytes or shortcut_bytes:
            raise ValueError("MENU_SEPARATOR carries no label or shortcut")
        if normalized_kind is ControlKind.MENU and shortcut_bytes:
            raise ValueError("MENU carries no shortcut")
    return normalized_kind, normalized_state


@dataclass(frozen=True, slots=True)
class ControlDefinition:
    """One semantic control node whose visual representation belongs to the view.

    Root controls carry an anchor rectangle and z order.  Descendants carry
    semantic ordering and state, leaving typography, padding, clipping,
    rasterization, and hit targets to the selected renderer.  TEXT_AREA and
    TEXT_GRID roots use one immutable logical text collection; menu and tab
    controls carry no renderer-specific payload.
    """

    owner: OwnerIdentity
    control_id: int
    kind: ControlKind
    state: ControlState
    z_order: int
    region_id: int
    parent_control_id: int
    order: int
    bounds: ObjectBounds | None
    label: str
    shortcut: str
    content: SemanticTextContent | None = None

    def __post_init__(self) -> None:
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        for name, minimum, maximum in (
            ("control_id", 1, UINT64_MAX),
            ("region_id", 1, UINT64_MAX),
            ("parent_control_id", 0, UINT64_MAX),
            ("order", 0, UINT32_MAX),
            ("z_order", INT32_MIN, INT32_MAX),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=maximum),
            )
        kind, state = validate_control_shape(
            kind=self.kind,
            state=self.state,
            z_order=self.z_order,
            parent_control_id=self.parent_control_id,
            order=self.order,
            bounds=self.bounds,
            label=self.label,
            shortcut=self.shortcut,
            content=self.content,
        )
        object.__setattr__(self, "kind", kind)
        object.__setattr__(self, "state", state)

    @property
    def visible(self) -> bool:
        return bool(self.state & ControlState.VISIBLE)

    @property
    def enabled(self) -> bool:
        return bool(self.state & ControlState.ENABLED)


@dataclass(frozen=True, slots=True)
class Sample:
    timestamp_us: int
    value: int

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "timestamp_us",
            _integer(
                "timestamp_us", self.timestamp_us, minimum=0, maximum=UINT64_MAX
            ),
        )
        object.__setattr__(
            self,
            "value",
            _integer("value", self.value, minimum=INT64_MIN, maximum=INT64_MAX),
        )


@dataclass(frozen=True, slots=True)
class ExplicitSamples:
    samples: tuple[Sample, ...]

    def __post_init__(self) -> None:
        samples = tuple(self.samples)
        if not samples or any(not isinstance(sample, Sample) for sample in samples):
            raise ValueError("explicit batch requires at least one Sample")
        if any(
            current.timestamp_us >= following.timestamp_us
            for current, following in zip(samples, samples[1:])
        ):
            raise ValueError("explicit sample timestamps are not strictly increasing")
        object.__setattr__(self, "samples", samples)


@dataclass(frozen=True, slots=True)
class UniformSamples:
    first_timestamp_us: int
    values: tuple[int, ...]

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "first_timestamp_us",
            _integer(
                "first_timestamp_us",
                self.first_timestamp_us,
                minimum=0,
                maximum=UINT64_MAX,
            ),
        )
        values = tuple(self.values)
        if not values:
            raise ValueError("uniform batch requires at least one value")
        normalized = tuple(
            _integer("value", value, minimum=INT64_MIN, maximum=INT64_MAX)
            for value in values
        )
        object.__setattr__(self, "values", normalized)


SeriesBatch = ExplicitSamples | UniformSamples


@dataclass(frozen=True, slots=True)
class SeriesDefinition:
    owner: OwnerIdentity
    series_id: int
    history_capacity: int
    timestamp_mode: TimestampMode
    uniform_interval_us: int
    samples: tuple[Sample, ...] = ()

    def __post_init__(self) -> None:
        if not isinstance(self.owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        object.__setattr__(
            self,
            "series_id",
            _integer("series_id", self.series_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "history_capacity",
            _integer("history_capacity", self.history_capacity, minimum=1, maximum=UINT32_MAX),
        )
        if isinstance(self.timestamp_mode, bool):
            raise TypeError("timestamp_mode must not be bool")
        try:
            mode = TimestampMode(self.timestamp_mode)
        except (TypeError, ValueError) as exc:
            raise ValueError("timestamp_mode is not RETAINED-1 explicit or uniform") from exc
        object.__setattr__(self, "timestamp_mode", mode)
        object.__setattr__(
            self,
            "uniform_interval_us",
            _integer(
                "uniform_interval_us", self.uniform_interval_us, minimum=0, maximum=UINT64_MAX
            ),
        )
        if (mode is TimestampMode.EXPLICIT) != (self.uniform_interval_us == 0):
            raise ValueError("series timestamp mode and uniform interval disagree")
        samples = tuple(self.samples)
        if any(not isinstance(sample, Sample) for sample in samples):
            raise TypeError("series history must contain only Sample values")
        if len(samples) > self.history_capacity:
            raise ValueError("series history exceeds its declared capacity")
        if any(
            current.timestamp_us >= following.timestamp_us
            for current, following in zip(samples, samples[1:])
        ):
            raise ValueError("series history timestamps are not strictly increasing")
        object.__setattr__(self, "samples", samples)


@dataclass(frozen=True, slots=True)
class SceneUsage:
    regions: int = 0
    objects: int = 0
    series: int = 0
    utf8_bytes: int = 0
    sample_slots: int = 0


@dataclass(frozen=True, slots=True)
class OwnerScene:
    owner: OwnerIdentity
    regions: Mapping[int, RegionDefinition]
    objects: Mapping[int, ObjectDefinition]
    series: Mapping[int, SeriesDefinition]
    usage: SceneUsage
    controls: Mapping[int, ControlDefinition] = field(
        default_factory=lambda: MappingProxyType({})
    )


@dataclass(frozen=True, slots=True)
class RetainedScene:
    owners: Mapping[int, OwnerScene]


@dataclass(frozen=True, slots=True)
class SceneModelState:
    revision: int
    geometry: TerminalGeometry
    active: RetainedScene
    hidden: RetainedScene | None
    hidden_kind: HiddenTargetKind | None
    requirement: RebuildRequirement | None
    retained_visible: bool
    retained_initialized: bool


@dataclass(slots=True)
class _MutableOwnerScene:
    """One transaction-private copy-on-write owner target.

    Published ``OwnerScene`` maps remain immutable.  The first operation for an
    owner copies those maps once into this private builder; later operations
    mutate the same dictionaries and update exact aggregate usage in constant
    time.  ``prepare_commit`` audits and freezes the builder into fresh mapping
    proxies before a prepared value can escape the model.
    """

    owner: OwnerIdentity
    regions: dict[int, RegionDefinition]
    objects: dict[int, ObjectDefinition]
    series: dict[int, SeriesDefinition]
    controls: dict[int, ControlDefinition]
    usage: SceneUsage


@dataclass(slots=True)
class _SceneStaging:
    lease: TransactionLease
    mode: RetainedMode
    geometry: TerminalGeometry
    source: RetainedScene
    owners: dict[int, OwnerScene | _MutableOwnerScene]
    item_advances: list[tuple[OwnerIdentity, ItemNamespace, int]]
    staged_high_water: dict[tuple[int, ItemNamespace], int]
    operation_count: int
    frozen: RetainedScene | None = None
    rejected: bool = False
    prepared: bool = False


@dataclass(frozen=True, slots=True)
class PreparedSceneInstall:
    state: SceneModelState
    ledger: PreparedOwnerLedgerInstall
    lease: TransactionLease
    _model_token: object
    _source_state: SceneModelState
    _resource_state: ResourceStoreState
    _staging: _SceneStaging


@dataclass(frozen=True, slots=True)
class PreparedOwnerRetirement:
    """One exact owner removal prepared across every retained target.

    The owner ledger tombstone and both scene-plane removals share the same
    source capabilities.  A coordinator can therefore validate all of them
    before completing the OWNER_DROP clock lease, after which publication is
    a fixed sequence of non-failing reference assignments.
    """

    owner: OwnerIdentity
    state: SceneModelState
    ledger: PreparedOwnerLedgerInstall
    lease: TransactionLease
    _model_token: object
    _source_state: SceneModelState
    _resource_state: ResourceStoreState


class RetainedSceneModel:
    """Active/hidden retained targets sharing one update authority."""

    def __init__(
        self,
        *,
        clock: TerminalUpdateAuthority,
        owners: OwnerLedger,
        resources: RetainedResourceStore,
        geometry: TerminalGeometry,
    ) -> None:
        if not isinstance(clock, TerminalUpdateAuthority):
            raise TypeError("clock must be TerminalUpdateAuthority")
        if not isinstance(owners, OwnerLedger):
            raise TypeError("owners must be OwnerLedger")
        if not isinstance(resources, RetainedResourceStore):
            raise TypeError("resources must be RetainedResourceStore")
        if resources.owner_ledger is not owners:
            raise ValueError("resources and scene must share one exact owner ledger")
        owners.policy.validate_geometry(geometry)
        self._clock = clock
        self._owners = owners
        self._resources = resources
        self._token = object()
        empty = RetainedScene(MappingProxyType({}))
        self._state = SceneModelState(
            clock.revision,
            geometry,
            empty,
            None,
            None,
            RebuildRequirement.REPLACE,
            False,
            False,
        )
        self._staging: _SceneStaging | None = None

    @property
    def state(self) -> SceneModelState:
        return self._state

    @property
    def clock(self) -> TerminalUpdateAuthority:
        return self._clock

    @property
    def resource_store(self) -> RetainedResourceStore:
        """The exact read-only resource authority dependency for this scene."""

        return self._resources

    @property
    def transaction_open(self) -> bool:
        return self._staging is not None

    def resource_referenced(self, owner: OwnerIdentity, resource_id: int) -> bool:
        """Whether either committed scene plane references one exact resource.

        Transaction-private staging is intentionally excluded: RESOURCE_DROP is
        serialized outside transactions, while active and committed hidden
        targets are the two authoritative reference planes.
        """

        if not isinstance(owner, OwnerIdentity):
            raise TypeError("owner must be OwnerIdentity")
        normalized_id = _integer(
            "resource_id", resource_id, minimum=1, maximum=UINT64_MAX
        )
        state = self._state
        for scene in (state.active, state.hidden):
            if scene is None:
                continue
            owner_scene = scene.owners.get(owner.owner_id)
            if owner_scene is None or owner_scene.owner != owner:
                continue
            if any(
                isinstance(definition.body, ImageBody)
                and definition.body.resource_id == normalized_id
                for definition in owner_scene.objects.values()
            ):
                return True
        return False

    def require_interactable_control(
        self,
        owner: OwnerIdentity,
        control_id: int,
    ) -> ControlDefinition:
        """Resolve one exact active semantic target without mutating guest state."""

        try:
            self._owners.require_live(owner)
            normalized_id = _integer(
                "control_id", control_id, minimum=1, maximum=UINT64_MAX
            )
        except OwnerLedgerError as exc:
            raise SceneModelError(SceneErrorCode.AUTHORITY, str(exc)) from exc
        except (TypeError, ValueError) as exc:
            raise SceneModelError(SceneErrorCode.STATE, str(exc)) from exc
        state = self._state
        if not state.retained_initialized or not state.retained_visible:
            raise SceneModelError(SceneErrorCode.STATE, "semantic controls are not visible")
        owner_scene = state.active.owners.get(owner.owner_id)
        if owner_scene is None or owner_scene.owner != owner:
            raise SceneModelError(SceneErrorCode.AUTHORITY, "control owner is not active")
        definition = owner_scene.controls.get(normalized_id)
        if definition is None:
            raise SceneModelError(SceneErrorCode.MISSING_ID, "control ID is not active")
        region = owner_scene.regions.get(definition.region_id)
        if region is None or not region.visible:
            raise SceneModelError(SceneErrorCode.BOUNDS, "control region is not visible")
        if definition.kind is ControlKind.TAB:
            parent = owner_scene.controls.get(definition.parent_control_id)
            if (
                parent is None
                or parent.kind is not ControlKind.TABSET
                or not parent.visible
                or not parent.enabled
            ):
                raise SceneModelError(
                    SceneErrorCode.GRAPH,
                    "TAB parent is not an interactive TABSET",
                )
            if not definition.visible or not definition.enabled:
                raise SceneModelError(
                    SceneErrorCode.STATE,
                    "control is hidden or disabled",
                )
            return definition
        if definition.kind not in (ControlKind.MENU, ControlKind.MENU_ITEM):
            raise SceneModelError(SceneErrorCode.STATE, "control kind is not activatable")
        if not definition.visible or not definition.enabled:
            raise SceneModelError(SceneErrorCode.STATE, "control is hidden or disabled")

        parent = owner_scene.controls.get(definition.parent_control_id)
        expected_parent = (
            ControlKind.MENU_BAR
            if definition.kind is ControlKind.MENU
            else ControlKind.MENU
        )
        if (
            parent is None
            or parent.kind is not expected_parent
            or not parent.visible
            or not parent.enabled
        ):
            raise SceneModelError(SceneErrorCode.GRAPH, "control ancestry is not interactive")
        if definition.kind is ControlKind.MENU:
            return definition
        if not parent.state & ControlState.OPEN:
            raise SceneModelError(SceneErrorCode.STATE, "menu item belongs to a closed menu")
        root = owner_scene.controls.get(parent.parent_control_id)
        if (
            root is None
            or root.kind is not ControlKind.MENU_BAR
            or not root.visible
            or not root.enabled
        ):
            raise SceneModelError(SceneErrorCode.GRAPH, "menu item root is not interactive")
        return definition

    def begin(
        self,
        lease: TransactionLease,
        mode: RetainedMode,
        geometry: TerminalGeometry,
    ) -> None:
        if self._staging is not None:
            raise SceneModelError(SceneErrorCode.STATE, "a retained transaction is already open")
        if self._clock.open_transaction is not lease:
            raise SceneModelError(SceneErrorCode.STATE, "lease is not the global open transaction")
        if lease.family is not TransactionFamily.PRESENT or not lease.admitted:
            raise SceneModelError(SceneErrorCode.STATE, "lease is not an admitted PRESENT transaction")
        if geometry != self._state.geometry:
            raise SceneModelError(SceneErrorCode.BOUNDS, "transaction geometry is stale")
        if isinstance(mode, bool):
            raise SceneModelError(SceneErrorCode.STATE, "retained mode must not be bool")
        try:
            selected_mode = RetainedMode(mode)
        except (TypeError, ValueError) as exc:
            raise SceneModelError(SceneErrorCode.STATE, "invalid retained mode") from exc

        state = self._state
        if selected_mode is RetainedMode.DELTA:
            if state.requirement is not None or state.hidden is not None:
                raise SceneModelError(SceneErrorCode.STATE, "DELTA cannot target a rebuilding model")
            candidate = state.active
        elif selected_mode is RetainedMode.REPLACE_START:
            candidate = RetainedScene(MappingProxyType({}))
        elif selected_mode is RetainedMode.REPLACE_CONTINUE:
            if state.hidden is None or state.hidden_kind is not HiddenTargetKind.REPLACE:
                raise SceneModelError(SceneErrorCode.STATE, "no hidden replacement target exists")
            candidate = state.hidden
        elif selected_mode is RetainedMode.LAYOUT_START:
            if state.requirement is not RebuildRequirement.LAYOUT:
                raise SceneModelError(SceneErrorCode.STATE, "layout rebuild is not required")
            candidate = state.active
        else:
            if state.hidden is None or state.hidden_kind is not HiddenTargetKind.LAYOUT:
                raise SceneModelError(SceneErrorCode.STATE, "no hidden layout target exists")
            candidate = state.hidden
        self._staging = _SceneStaging(
            lease=lease,
            mode=selected_mode,
            geometry=geometry,
            source=candidate,
            owners=dict(candidate.owners),
            item_advances=[],
            staged_high_water={},
            operation_count=0,
        )

    def define_region(self, region: RegionDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(region, RegionDefinition):
            self._fail(SceneErrorCode.STATE, "region must be RegionDefinition")
        self._require_owner(region.owner)
        try:
            region.validate_geometry(staging.geometry)
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        owner_scene = self._owner_builder(staging, region.owner)
        if region.region_id in owner_scene.regions:
            self._fail(SceneErrorCode.DUPLICATE_ID, "region ID already exists in target")
        self._stage_new_id(staging, region.owner, ItemNamespace.REGION, region.region_id)
        usage = self._usage_after(owner_scene, region_delta=1)
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.regions[region.region_id] = region
        self._commit_operation(staging, owner_scene, usage)

    def replace_region(self, region: RegionDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(region, RegionDefinition):
            self._fail(SceneErrorCode.STATE, "region must be RegionDefinition")
        self._require_owner(region.owner)
        try:
            region.validate_geometry(staging.geometry)
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        owner_scene = self._owner_builder(staging, region.owner)
        if region.region_id not in owner_scene.regions:
            self._fail(SceneErrorCode.MISSING_ID, "region replacement ID is absent")
        usage = owner_scene.usage
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.regions[region.region_id] = region
        self._commit_operation(staging, owner_scene, usage)

    def define_object(self, definition: ObjectDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, ObjectDefinition):
            self._fail(SceneErrorCode.STATE, "object must be ObjectDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_builder(staging, definition.owner)
        if definition.object_id in owner_scene.objects:
            self._fail(SceneErrorCode.DUPLICATE_ID, "object ID already exists in target")
        self._validate_object_policy(definition)
        self._validate_object_dependencies(owner_scene, definition)
        self._stage_new_id(staging, definition.owner, ItemNamespace.OBJECT, definition.object_id)
        usage = self._usage_after(
            owner_scene,
            object_delta=1,
            utf8_add=self._object_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.objects[definition.object_id] = definition
        self._commit_operation(staging, owner_scene, usage)

    def replace_object(self, definition: ObjectDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, ObjectDefinition):
            self._fail(SceneErrorCode.STATE, "object must be ObjectDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_builder(staging, definition.owner)
        current = owner_scene.objects.get(definition.object_id)
        if current is None:
            self._fail(SceneErrorCode.MISSING_ID, "object replacement ID is absent")
        if definition.kind is not current.kind:
            self._fail(
                SceneErrorCode.STATE,
                "object replacement cannot change the object type",
            )
        self._validate_object_policy(definition)
        self._validate_object_dependencies(owner_scene, definition)
        usage = self._usage_after(
            owner_scene,
            utf8_remove=self._object_utf8_bytes(current),
            utf8_add=self._object_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.objects[definition.object_id] = definition
        self._commit_operation(staging, owner_scene, usage)

    def define_control(self, definition: ControlDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, ControlDefinition):
            self._fail(SceneErrorCode.STATE, "control must be ControlDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_builder(staging, definition.owner)
        if definition.control_id in owner_scene.controls:
            self._fail(SceneErrorCode.DUPLICATE_ID, "control ID already exists in target")
        self._validate_control_policy(definition)
        self._validate_control_dependencies(owner_scene, definition)
        self._stage_new_id(
            staging,
            definition.owner,
            ItemNamespace.CONTROL,
            definition.control_id,
        )
        usage = self._usage_after(
            owner_scene,
            object_delta=self._control_object_slots(definition),
            utf8_add=self._control_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.controls[definition.control_id] = definition
        self._commit_operation(staging, owner_scene, usage)

    def replace_control(self, definition: ControlDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, ControlDefinition):
            self._fail(SceneErrorCode.STATE, "control must be ControlDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_builder(staging, definition.owner)
        current = owner_scene.controls.get(definition.control_id)
        if current is None:
            self._fail(SceneErrorCode.MISSING_ID, "control replacement ID is absent")
        if definition.kind is not current.kind:
            self._fail(
                SceneErrorCode.STATE,
                "control replacement cannot change the control kind",
            )
        if definition.kind in {
            ControlKind.MENU_BAR,
            ControlKind.MENU,
            ControlKind.MENU_ITEM,
            ControlKind.MENU_SEPARATOR,
            ControlKind.TABSET,
        }:
            compatible = replace(definition, state=current.state) == current
            failure = "control replacement may change only the control state"
        elif definition.kind in {ControlKind.TEXT_AREA, ControlKind.TEXT_GRID}:
            compatible = (
                replace(
                    definition,
                    state=current.state,
                    content=current.content,
                )
                == current
            )
            failure = (
                "text control replacement may change only state and semantic content"
            )
            if (
                compatible
                and definition.content != current.content
                and definition.content is not None
                and current.content is not None
                and definition.content.content_revision
                <= current.content.content_revision
            ):
                self._fail(
                    SceneErrorCode.STATE,
                    "changed semantic content requires a newer content revision",
                )
        else:  # TAB
            compatible = (
                replace(
                    definition,
                    state=current.state,
                    label=current.label,
                    shortcut=current.shortcut,
                )
                == current
            )
            failure = "TAB replacement may change only state, label, and shortcut"
        if not compatible:
            self._fail(SceneErrorCode.STATE, failure)
        self._validate_control_policy(definition)
        self._validate_control_dependencies(owner_scene, definition)
        usage = self._usage_after(
            owner_scene,
            object_delta=(
                self._control_object_slots(definition)
                - self._control_object_slots(current)
            ),
            utf8_remove=self._control_utf8_bytes(current),
            utf8_add=self._control_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.controls[definition.control_id] = definition
        self._commit_operation(staging, owner_scene, usage)

    def define_series(self, definition: SeriesDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, SeriesDefinition):
            self._fail(SceneErrorCode.STATE, "series must be SeriesDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_builder(staging, definition.owner)
        if definition.series_id in owner_scene.series:
            self._fail(SceneErrorCode.DUPLICATE_ID, "series ID already exists in target")
        policy = self._owners.policy
        if not policy.features & RetainedFeature.SERIES:
            self._fail(SceneErrorCode.FEATURE, "SERIES was not advertised")
        if definition.history_capacity > policy.max_history_per_series:
            self._fail(SceneErrorCode.QUOTA, "series history exceeds advertised maximum")
        if definition.samples:
            self._fail(SceneErrorCode.STATE, "SERIES_DEFINE history must begin empty")
        self._stage_new_id(staging, definition.owner, ItemNamespace.SERIES, definition.series_id)
        usage = self._usage_after(
            owner_scene,
            series_delta=1,
            sample_slots_add=definition.history_capacity,
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.series[definition.series_id] = definition
        self._commit_operation(staging, owner_scene, usage)

    def drop_region(self, owner: OwnerIdentity, region_id: int) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "region_id", region_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        if normalized_id not in owner_scene.regions:
            self._fail(SceneErrorCode.MISSING_ID, "region drop ID is absent")
        usage = self._usage_after(owner_scene, region_delta=-1)
        self._admit_operation(staging, owner_scene, usage)
        del owner_scene.regions[normalized_id]
        self._commit_operation(staging, owner_scene, usage)

    def drop_object(self, owner: OwnerIdentity, object_id: int) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "object_id", object_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.objects.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "object drop ID is absent")
        usage = self._usage_after(
            owner_scene,
            object_delta=-1,
            utf8_remove=self._object_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        del owner_scene.objects[normalized_id]
        self._commit_operation(staging, owner_scene, usage)

    def drop_control(self, owner: OwnerIdentity, control_id: int) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "control_id", control_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.controls.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "control drop ID is absent")
        usage = self._usage_after(
            owner_scene,
            object_delta=-self._control_object_slots(definition),
            utf8_remove=self._control_utf8_bytes(definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        del owner_scene.controls[normalized_id]
        self._commit_operation(staging, owner_scene, usage)

    def drop_series(self, owner: OwnerIdentity, series_id: int) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "series_id", series_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.series.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "series drop ID is absent")
        usage = self._usage_after(
            owner_scene,
            series_delta=-1,
            sample_slots_remove=definition.history_capacity,
        )
        self._admit_operation(staging, owner_scene, usage)
        del owner_scene.series[normalized_id]
        self._commit_operation(staging, owner_scene, usage)

    def set_object_value(
        self, owner: OwnerIdentity, object_id: int, value: int
    ) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "object_id", object_id, minimum=1, maximum=UINT64_MAX
            )
            normalized_value = _integer(
                "value", value, minimum=INT64_MIN, maximum=INT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.objects.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "object value target is absent")
        body = definition.body
        if not isinstance(body, (ReadoutBody, MeterBody, StatusBody)):
            self._fail(
                SceneErrorCode.STATE,
                "OBJECT_SET_VALUE requires READOUT, METER, or STATUS",
            )
        if isinstance(body, MeterBody) and not body.minimum <= normalized_value <= body.maximum:
            self._fail(SceneErrorCode.BOUNDS, "meter value is outside its declared range")
        try:
            replacement_body = replace(body, value=normalized_value)
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.BOUNDS, str(exc))
        replacement_definition = replace(definition, body=replacement_body)
        usage = self._usage_after(
            owner_scene,
            utf8_remove=self._object_utf8_bytes(definition),
            utf8_add=self._object_utf8_bytes(replacement_definition),
        )
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.objects[normalized_id] = replacement_definition
        self._commit_operation(staging, owner_scene, usage)

    def set_object_visibility(
        self, owner: OwnerIdentity, object_id: int, visible: bool
    ) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "object_id", object_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        if not isinstance(visible, bool):
            self._fail(SceneErrorCode.STATE, "visibility must be bool")
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.objects.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "object visibility target is absent")
        usage = owner_scene.usage
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.objects[normalized_id] = replace(definition, visible=visible)
        self._commit_operation(staging, owner_scene, usage)

    def append_series(
        self, owner: OwnerIdentity, series_id: int, batch: SeriesBatch
    ) -> None:
        self._mutate_series(owner, series_id, batch, replace_history=False)

    def replace_series(
        self, owner: OwnerIdentity, series_id: int, batch: SeriesBatch
    ) -> None:
        self._mutate_series(owner, series_id, batch, replace_history=True)

    def _mutate_series(
        self,
        owner: OwnerIdentity,
        series_id: int,
        batch: SeriesBatch,
        *,
        replace_history: bool,
    ) -> None:
        staging = self._require_mutable_staging()
        self._require_owner(owner)
        try:
            normalized_id = _integer(
                "series_id", series_id, minimum=1, maximum=UINT64_MAX
            )
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        owner_scene = self._owner_builder(staging, owner)
        definition = owner_scene.series.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "series mutation target is absent")
        samples = self._normalize_series_batch(definition, batch)
        if (
            not replace_history
            and definition.samples
            and samples[0].timestamp_us <= definition.samples[-1].timestamp_us
        ):
            self._fail(
                SceneErrorCode.BOUNDS,
                "SERIES_APPEND first timestamp is not newer than committed history",
            )
        if replace_history:
            history = samples
        else:
            combined = definition.samples + samples
            history = combined[-definition.history_capacity :]
        replacement_series = replace(definition, samples=history)
        usage = owner_scene.usage
        self._admit_operation(staging, owner_scene, usage)
        owner_scene.series[normalized_id] = replacement_series
        self._commit_operation(staging, owner_scene, usage)

    def _normalize_series_batch(
        self, definition: SeriesDefinition, batch: SeriesBatch
    ) -> tuple[Sample, ...]:
        policy = self._owners.policy
        if isinstance(batch, ExplicitSamples):
            if definition.timestamp_mode is not TimestampMode.EXPLICIT:
                self._fail(SceneErrorCode.STATE, "explicit batch targets a uniform series")
            samples = batch.samples
        elif isinstance(batch, UniformSamples):
            if definition.timestamp_mode is not TimestampMode.UNIFORM:
                self._fail(SceneErrorCode.STATE, "uniform batch targets an explicit series")
            count = len(batch.values)
            interval = definition.uniform_interval_us
            if count > 1 and count - 1 > (UINT64_MAX - batch.first_timestamp_us) // interval:
                self._fail(SceneErrorCode.BOUNDS, "uniform sample timestamp overflows uint64")
            samples = tuple(
                Sample(batch.first_timestamp_us + index * interval, value)
                for index, value in enumerate(batch.values)
            )
        else:
            self._fail(
                SceneErrorCode.STATE,
                "series batch must be ExplicitSamples or UniformSamples",
            )
        count = len(samples)
        if count > policy.max_samples_per_append:
            self._fail(SceneErrorCode.QUOTA, "sample batch exceeds advertised maximum")
        if count > definition.history_capacity:
            self._fail(SceneErrorCode.QUOTA, "sample batch exceeds series capacity")
        return samples

    def prepare_commit(self, disposition: CommitDisposition) -> PreparedSceneInstall:
        staging = self._require_staging()
        if staging.rejected:
            raise SceneModelError(SceneErrorCode.STATE, "retained transaction was rejected")
        if isinstance(disposition, bool):
            self._fail(SceneErrorCode.STATE, "commit disposition must not be bool")
        try:
            selected = CommitDisposition(disposition)
        except (TypeError, ValueError) as exc:
            self._fail(SceneErrorCode.STATE, "invalid commit disposition")
        if staging.mode is RetainedMode.DELTA and selected is not CommitDisposition.COMMIT:
            self._fail(SceneErrorCode.STATE, "DELTA cannot reveal a hidden target")
        if selected is CommitDisposition.COMMIT_AND_REVEAL and staging.mode not in (
            RetainedMode.REPLACE_CONTINUE,
            RetainedMode.LAYOUT_CONTINUE,
        ):
            self._fail(
                SceneErrorCode.STATE,
                "START is intermediate; only CONTINUE may reveal",
            )
        if staging.mode is RetainedMode.DELTA and staging.operation_count == 0:
            self._fail(SceneErrorCode.STATE, "DELTA requires at least one operation")

        candidate = self._freeze_staging(staging)
        if (
            selected is CommitDisposition.COMMIT_AND_REVEAL
            and staging.mode in (RetainedMode.LAYOUT_START, RetainedMode.LAYOUT_CONTINUE)
        ):
            for owner_scene in candidate.owners.values():
                if any(
                    region.geometry_generation != staging.geometry.generation
                    for region in owner_scene.regions.values()
                ):
                    self._fail(SceneErrorCode.BOUNDS, "layout reveal has stale regions")

        try:
            ledger = self._owners.prepare_item_ids(staging.item_advances)
        except OwnerLedgerError as exc:
            self._fail(SceneErrorCode.AUTHORITY, str(exc))
        try:
            revision = self._clock.next_revision(staging.lease)
        except TerminalUpdateError as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        old = self._state
        if staging.mode is RetainedMode.DELTA:
            state = replace(old, revision=revision, active=candidate)
        elif selected is CommitDisposition.COMMIT_AND_REVEAL:
            state = SceneModelState(
                revision,
                staging.geometry,
                candidate,
                None,
                None,
                None,
                True,
                True,
            )
        else:
            kind = (
                HiddenTargetKind.REPLACE
                if staging.mode in (RetainedMode.REPLACE_START, RetainedMode.REPLACE_CONTINUE)
                else HiddenTargetKind.LAYOUT
            )
            state = replace(
                old,
                revision=revision,
                hidden=candidate,
                hidden_kind=kind,
                requirement=(
                    RebuildRequirement.REPLACE
                    if kind is HiddenTargetKind.REPLACE
                    else RebuildRequirement.LAYOUT
                ),
            )
        resource_state = self._resources.state
        self._validate_scene(state.active, resource_state)
        if state.hidden is not None and state.hidden is not state.active:
            self._validate_scene(state.hidden, resource_state)
        prepared = PreparedSceneInstall(
            state=state,
            ledger=ledger,
            lease=staging.lease,
            _model_token=self._token,
            _source_state=old,
            _resource_state=resource_state,
            _staging=staging,
        )
        staging.prepared = True
        return prepared

    def install_prepared(self, prepared: PreparedSceneInstall) -> ResultLease:
        self.validate_prepared(prepared)
        result = self._clock.complete_success(prepared.lease)
        self._install_prevalidated(prepared)
        return result

    def prepare_owner_retirement(
        self,
        lease: TransactionLease,
        owner: OwnerIdentity,
    ) -> PreparedOwnerRetirement:
        """Prepare one exact OWNER_DROP across active, hidden, and ledger state."""

        if self._staging is not None:
            raise SceneModelError(
                SceneErrorCode.STATE,
                "owner retirement waits for the retained transaction to settle",
            )
        if self._clock.open_transaction is not lease:
            raise SceneModelError(
                SceneErrorCode.STATE,
                "owner retirement lease is not the global open transaction",
            )
        if lease.family is not TransactionFamily.OWNER_DROP or not lease.admitted:
            raise SceneModelError(
                SceneErrorCode.STATE,
                "owner retirement requires an admitted OWNER_DROP transaction",
            )

        # prepare_drop validates the exact session/epoch/ID/generation and
        # reserves the tombstone candidate without mutating live authority.
        ledger = self._owners.prepare_drop(owner)
        source = self._state
        active = self._scene_without_owner(source.active, owner)
        hidden = (
            None
            if source.hidden is None
            else self._scene_without_owner(source.hidden, owner)
        )
        resource_state = self._resources.state
        self._validate_scene(active, resource_state)
        if hidden is not None and hidden is not active:
            self._validate_scene(hidden, resource_state)
        try:
            revision = self._clock.next_revision(lease)
        except TerminalUpdateError as exc:
            raise SceneModelError(SceneErrorCode.STATE, str(exc)) from exc
        return PreparedOwnerRetirement(
            owner=owner,
            state=replace(
                source,
                revision=revision,
                active=active,
                hidden=hidden,
            ),
            ledger=ledger,
            lease=lease,
            _model_token=self._token,
            _source_state=source,
            _resource_state=resource_state,
        )

    def install_owner_retirement(
        self,
        prepared: PreparedOwnerRetirement,
    ) -> ResultLease:
        """Complete and install a prevalidated owner retirement directly."""

        self.validate_owner_retirement(prepared)
        result = self._clock.complete_success(prepared.lease)
        self._install_owner_retirement_prevalidated(prepared)
        return result

    def validate_owner_retirement(
        self,
        prepared: PreparedOwnerRetirement,
    ) -> None:
        """Validate scene, ledger, lease, and revision before OWNER_DROP."""

        if not isinstance(prepared, PreparedOwnerRetirement):
            raise TypeError("prepared must be PreparedOwnerRetirement")
        if (
            prepared._model_token is not self._token
            or prepared._source_state is not self._state
            or prepared._resource_state is not self._resources.state
            or self._staging is not None
        ):
            raise RuntimeError("prepared owner retirement is stale or foreign")
        self._owners.validate_prepared(prepared.ledger)
        if self._clock.open_transaction is not prepared.lease:
            raise RuntimeError("prepared owner retirement lost its transaction lease")
        if (
            prepared.lease.family is not TransactionFamily.OWNER_DROP
            or not prepared.lease.admitted
        ):
            raise RuntimeError("prepared owner retirement has an invalid clock lease")
        if self._clock.next_revision(prepared.lease) != prepared.state.revision:
            raise RuntimeError("prepared owner retirement revision is stale")

    def validate_prepared(self, prepared: PreparedSceneInstall) -> None:
        """Validate exact scene, ledger, lease, and revision provenance."""

        if not isinstance(prepared, PreparedSceneInstall):
            raise TypeError("prepared must be PreparedSceneInstall")
        if (
            prepared._model_token is not self._token
            or prepared._source_state is not self._state
            or prepared._resource_state is not self._resources.state
            or prepared._staging is not self._staging
            or prepared._staging.rejected
            or not prepared._staging.prepared
        ):
            raise RuntimeError("prepared retained scene is stale or foreign")
        self._owners.validate_prepared(prepared.ledger)
        if self._clock.open_transaction is not prepared.lease:
            raise RuntimeError("prepared retained scene lost its transaction lease")
        if self._clock.next_revision(prepared.lease) != prepared.state.revision:
            raise RuntimeError("prepared retained scene revision is stale")

    def _install_prevalidated(self, prepared: PreparedSceneInstall) -> None:
        """Install after a coordinator has completed every fallible check."""

        self._owners._install_prevalidated(prepared.ledger)
        self._state = prepared.state
        self._staging = None

    def _install_owner_retirement_prevalidated(
        self,
        prepared: PreparedOwnerRetirement,
    ) -> None:
        """Install an owner retirement after every fallible check completed."""

        self._owners._install_prevalidated(prepared.ledger)
        self._state = prepared.state

    def reject(self) -> ResultLease:
        staging = self._require_staging()
        result = self._clock.complete_rejected(staging.lease)
        self._staging = None
        return result

    def abort(self) -> None:
        staging = self._require_staging()
        self._clock.abort(staging.lease)
        self._staging = None

    def require_layout(self, geometry: TerminalGeometry) -> None:
        if self._staging is not None or self._clock.open_transaction is not None or self._clock.outstanding_result is not None:
            raise SceneModelError(SceneErrorCode.STATE, "layout waits for transaction/result settlement")
        self._owners.policy.validate_geometry(geometry)
        if geometry.generation <= self._state.geometry.generation:
            raise SceneModelError(SceneErrorCode.BOUNDS, "layout generation is not newer")
        requirement = (
            RebuildRequirement.LAYOUT
            if self._state.retained_initialized
            else RebuildRequirement.REPLACE
        )
        self._state = replace(
            self._state,
            geometry=geometry,
            hidden=None,
            hidden_kind=None,
            requirement=requirement,
            retained_visible=False,
        )

    @staticmethod
    def _scene_without_owner(
        scene: RetainedScene,
        owner: OwnerIdentity,
    ) -> RetainedScene:
        present = scene.owners.get(owner.owner_id)
        if present is None:
            return scene
        if present.owner != owner:
            raise RuntimeError(
                "retained scene owner identity disagrees with exact ledger authority"
            )
        owners = dict(scene.owners)
        del owners[owner.owner_id]
        return RetainedScene(MappingProxyType(owners))

    def _stage_new_id(
        self,
        staging: _SceneStaging,
        owner: OwnerIdentity,
        namespace: ItemNamespace,
        item_id: int,
    ) -> None:
        record = self._require_owner(owner)
        key = (owner.owner_id, namespace)
        current = staging.staged_high_water.get(key, record.high_water.value(namespace))
        if item_id <= current:
            self._fail(
                SceneErrorCode.DUPLICATE_ID,
                f"{namespace.value} ID does not exceed high-water {current}",
            )
        staging.staged_high_water[key] = item_id
        staging.item_advances.append((owner, namespace, item_id))

    def _owner_builder(
        self,
        staging: _SceneStaging,
        owner: OwnerIdentity,
    ) -> _MutableOwnerScene:
        current = staging.owners.get(owner.owner_id)
        if current is None:
            mutable = _MutableOwnerScene(owner, {}, {}, {}, {}, SceneUsage())
            staging.owners[owner.owner_id] = mutable
            return mutable
        if current.owner != owner:
            self._fail(SceneErrorCode.AUTHORITY, "scene owner generation mismatch")
        if isinstance(current, _MutableOwnerScene):
            return current
        mutable = _MutableOwnerScene(
            current.owner,
            dict(current.regions),
            dict(current.objects),
            dict(current.series),
            dict(current.controls),
            current.usage,
        )
        staging.owners[owner.owner_id] = mutable
        return mutable

    def _object_utf8_bytes(self, definition: ObjectDefinition) -> int:
        body = definition.body
        if isinstance(body, GlyphRunBody):
            return len(_text_bytes("text", body.text))
        if isinstance(body, ReadoutBody):
            try:
                return len(
                    body.formatted_bytes(self._owners.policy.max_glyph_run_bytes)
                )
            except SceneModelError as exc:
                self._fail(exc.code, exc.detail)
        return 0

    @staticmethod
    def _control_object_slots(definition: ControlDefinition) -> int:
        return 1 + (0 if definition.content is None else len(definition.content.items))

    @staticmethod
    def _control_utf8_bytes(definition: ControlDefinition) -> int:
        content_bytes = (
            0 if definition.content is None else definition.content.utf8_bytes
        )
        return (
            len(_control_text_bytes("label", definition.label))
            + len(_control_text_bytes("shortcut", definition.shortcut))
            + content_bytes
        )

    def _usage_after(
        self,
        owner_scene: _MutableOwnerScene,
        *,
        region_delta: int = 0,
        object_delta: int = 0,
        series_delta: int = 0,
        utf8_remove: int = 0,
        utf8_add: int = 0,
        sample_slots_remove: int = 0,
        sample_slots_add: int = 0,
    ) -> SceneUsage:
        prior = owner_scene.usage
        regions = prior.regions + region_delta
        objects = prior.objects + object_delta
        series = prior.series + series_delta
        if (
            regions < 0
            or objects < 0
            or series < 0
            or utf8_remove > prior.utf8_bytes
            or sample_slots_remove > prior.sample_slots
        ):
            self._fail(SceneErrorCode.STATE, "retained staging usage is inconsistent")
        try:
            utf8_bytes = _add_usage(
                "UTF-8 usage",
                prior.utf8_bytes - utf8_remove,
                utf8_add,
            )
            sample_slots = _add_usage(
                "sample-slot usage",
                prior.sample_slots - sample_slots_remove,
                sample_slots_add,
            )
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        return SceneUsage(regions, objects, series, utf8_bytes, sample_slots)

    def _admit_operation(
        self,
        staging: _SceneStaging,
        owner_scene: _MutableOwnerScene,
        usage: SceneUsage,
    ) -> None:
        self._validate_usage(owner_scene.owner, usage)
        if staging.operation_count >= self._owners.policy.max_operations_per_transaction:
            self._fail(SceneErrorCode.QUOTA, "operation count exceeds caller policy")

    @staticmethod
    def _commit_operation(
        staging: _SceneStaging,
        owner_scene: _MutableOwnerScene,
        usage: SceneUsage,
    ) -> None:
        owner_scene.usage = usage
        staging.operation_count += 1

    def _freeze_staging(self, staging: _SceneStaging) -> RetainedScene:
        if staging.frozen is not None:
            return staging.frozen
        if staging.operation_count == 0:
            staging.frozen = staging.source
            return staging.source

        owners: dict[int, OwnerScene] = {}
        for owner_id, owner_scene in staging.owners.items():
            if isinstance(owner_scene, _MutableOwnerScene):
                try:
                    frozen_owner = self._make_owner_scene(
                        owner_scene.owner,
                        owner_scene.regions,
                        owner_scene.objects,
                        owner_scene.series,
                        owner_scene.controls,
                    )
                except SceneModelError as exc:
                    self._fail(exc.code, exc.detail)
                if frozen_owner.usage != owner_scene.usage:
                    self._fail(
                        SceneErrorCode.STATE,
                        "retained staging usage does not match its frozen target",
                    )
                owners[owner_id] = frozen_owner
            else:
                owners[owner_id] = owner_scene
        staging.frozen = RetainedScene(MappingProxyType(dict(owners)))
        return staging.frozen

    def _make_owner_scene(
        self,
        owner: OwnerIdentity,
        regions: Mapping[int, RegionDefinition],
        objects: Mapping[int, ObjectDefinition],
        series: Mapping[int, SeriesDefinition],
        controls: Mapping[int, ControlDefinition],
    ) -> OwnerScene:
        utf8_bytes = 0
        for definition in objects.values():
            utf8_bytes = _add_usage(
                "UTF-8 usage",
                utf8_bytes,
                self._object_utf8_bytes(definition),
            )
        for definition in controls.values():
            utf8_bytes = _add_usage(
                "UTF-8 usage",
                utf8_bytes,
                self._control_utf8_bytes(definition),
            )
        sample_slots = 0
        for definition in series.values():
            sample_slots = _add_usage("sample-slot usage", sample_slots, definition.history_capacity)
        usage = SceneUsage(
            len(regions),
            len(objects)
            + sum(
                self._control_object_slots(definition)
                for definition in controls.values()
            ),
            len(series),
            utf8_bytes,
            sample_slots,
        )
        return OwnerScene(
            owner,
            MappingProxyType(dict(regions)),
            MappingProxyType(dict(objects)),
            MappingProxyType(dict(series)),
            usage,
            MappingProxyType(dict(controls)),
        )

    def _validate_usage(self, owner: OwnerIdentity, usage: SceneUsage) -> None:
        record = self._require_owner(owner)
        assert isinstance(record.quotas, OwnerQuotas)
        quota = record.quotas
        checks = (
            (usage.regions, quota.regions, "region"),
            (usage.objects, quota.objects, "object"),
            (usage.series, quota.series, "series"),
            (usage.utf8_bytes, quota.utf8_bytes, "UTF-8-byte"),
            (usage.sample_slots, quota.sample_slots, "sample-slot"),
        )
        for used, reserved, name in checks:
            if used > reserved:
                self._fail(SceneErrorCode.QUOTA, f"owner {name} usage exceeds reservation")

    def _validate_object_policy(self, definition: ObjectDefinition) -> None:
        policy = self._owners.policy
        kind = definition.kind
        if kind is ObjectKind.GLYPH_RUN:
            required = RetainedFeature.CORE
        elif kind in (ObjectKind.GROUP, ObjectKind.POLYLINE):
            required = RetainedFeature.VECTOR
        elif kind is ObjectKind.IMAGE:
            required = RetainedFeature.RGBA_IMAGE
        elif kind in (ObjectKind.PLOT, ObjectKind.WAVEFORM):
            required = RetainedFeature.SERIES
        else:
            required = RetainedFeature.INSTRUMENT
        if not policy.features & required:
            self._fail(SceneErrorCode.FEATURE, f"{kind.name} feature was not advertised")
        if isinstance(definition.body, PolylineBody) and len(definition.body.points) > policy.max_path_points:
            self._fail(SceneErrorCode.QUOTA, "polyline point count exceeds advertised maximum")
        if isinstance(definition.body, GlyphRunBody):
            if policy.max_glyph_run_bytes == 0:
                self._fail(SceneErrorCode.FEATURE, "glyph runs were not advertised")
            if len(_text_bytes("text", definition.body.text)) > policy.max_glyph_run_bytes:
                self._fail(SceneErrorCode.QUOTA, "glyph run exceeds advertised byte maximum")
        if isinstance(definition.body, ReadoutBody):
            try:
                definition.body.formatted_bytes(policy.max_glyph_run_bytes)
            except SceneModelError as exc:
                self._fail(exc.code, exc.detail)

    def _validate_control_policy(self, definition: ControlDefinition) -> None:
        features = self._owners.policy.features
        if not features & RetainedFeature.CONTROLS:
            self._fail(SceneErrorCode.FEATURE, "semantic controls were not advertised")
        if definition.kind in {
            ControlKind.TEXT_AREA,
            ControlKind.TEXT_GRID,
            ControlKind.TABSET,
            ControlKind.TAB,
        } and not features & RetainedFeature.CONTROL_COLLECTIONS:
            self._fail(
                SceneErrorCode.FEATURE,
                "CONTROL_COLLECTIONS was not advertised",
            )

    def _validate_control_dependencies(
        self,
        owner_scene: OwnerScene | _MutableOwnerScene,
        definition: ControlDefinition,
    ) -> None:
        if definition.region_id not in owner_scene.regions:
            self._fail(
                SceneErrorCode.GRAPH,
                "control region must be defined before the dependent control",
            )
        if definition.kind in {
            ControlKind.MENU_BAR,
            ControlKind.TEXT_AREA,
            ControlKind.TEXT_GRID,
            ControlKind.TABSET,
        }:
            return
        parent = owner_scene.controls.get(definition.parent_control_id)
        expected = {
            ControlKind.MENU: ControlKind.MENU_BAR,
            ControlKind.MENU_ITEM: ControlKind.MENU,
            ControlKind.MENU_SEPARATOR: ControlKind.MENU,
            ControlKind.TAB: ControlKind.TABSET,
        }[definition.kind]
        if parent is None or parent.kind is not expected:
            self._fail(
                SceneErrorCode.GRAPH,
                f"{definition.kind.name} parent must be a live {expected.name}",
            )
        if parent.region_id != definition.region_id:
            self._fail(SceneErrorCode.GRAPH, "control parent belongs to another region")

    def _validate_object_dependencies(
        self,
        owner_scene: OwnerScene | _MutableOwnerScene,
        definition: ObjectDefinition,
    ) -> None:
        if definition.region_id not in owner_scene.regions:
            self._fail(
                SceneErrorCode.GRAPH,
                "object region must be defined before the dependent object",
            )
        if definition.parent_object_id:
            parent = owner_scene.objects.get(definition.parent_object_id)
            if parent is None or parent.kind is not ObjectKind.GROUP:
                self._fail(
                    SceneErrorCode.GRAPH,
                    "object GROUP parent must be defined before its child",
                )
            if parent.region_id != definition.region_id:
                self._fail(
                    SceneErrorCode.GRAPH,
                    "object parent belongs to another region",
                )
        if isinstance(definition.body, (PlotBody, WaveformBody)):
            if definition.body.series_id not in owner_scene.series:
                self._fail(
                    SceneErrorCode.GRAPH,
                    "object series must be defined before its consumer",
                )
        if isinstance(definition.body, ImageBody):
            self._validate_image_resource(
                definition.owner,
                definition.body.resource_id,
                self._resources.state,
            )

    def _validate_image_resource(
        self,
        owner: OwnerIdentity,
        resource_id: int,
        resource_state: ResourceStoreState,
    ) -> None:
        resource = resource_state.resources.get(
            (owner.owner_id, owner.owner_generation, resource_id)
        )
        if (
            resource is None
            or resource.owner != owner
            or resource.resource_id != resource_id
        ):
            self._fail(
                SceneErrorCode.GRAPH,
                "IMAGE refers to an absent exact-owner resource",
            )

    def _require_owner(self, owner: OwnerIdentity):
        try:
            return self._owners.require_live(owner)
        except OwnerLedgerError as exc:
            self._fail(SceneErrorCode.AUTHORITY, str(exc))

    def _validate_scene(
        self,
        scene: RetainedScene,
        resource_state: ResourceStoreState,
    ) -> None:
        for owner_scene in scene.owners.values():
            self._validate_usage(owner_scene.owner, owner_scene.usage)
            for object_key, definition in owner_scene.objects.items():
                if object_key != definition.object_id or definition.owner != owner_scene.owner:
                    self._fail(SceneErrorCode.GRAPH, "object map key or owner is invalid")
                if definition.region_id not in owner_scene.regions:
                    self._fail(SceneErrorCode.GRAPH, "object refers to an absent region")
                parent_id = definition.parent_object_id
                if parent_id:
                    parent = owner_scene.objects.get(parent_id)
                    if parent is None or parent.kind is not ObjectKind.GROUP:
                        self._fail(SceneErrorCode.GRAPH, "object parent is not a live GROUP")
                    if parent.region_id != definition.region_id:
                        self._fail(SceneErrorCode.GRAPH, "object parent belongs to another region")
                if isinstance(definition.body, (PlotBody, WaveformBody)) and definition.body.series_id not in owner_scene.series:
                    self._fail(SceneErrorCode.GRAPH, "object refers to an absent series")
                if isinstance(definition.body, ImageBody):
                    self._validate_image_resource(
                        definition.owner,
                        definition.body.resource_id,
                        resource_state,
                    )

            # Resolve every parent chain once.  The prior per-object walk was
            # worst-case quadratic for a valid deep GROUP tree.
            completed: set[int] = set()
            for object_id in owner_scene.objects:
                if object_id in completed:
                    continue
                path: set[int] = set()
                current = object_id
                while current and current not in completed:
                    if current in path:
                        self._fail(SceneErrorCode.GRAPH, "object parent graph contains a cycle")
                    path.add(current)
                    current = owner_scene.objects[current].parent_object_id
                completed.update(path)

            sibling_orders: set[tuple[int, int]] = set()
            open_menu_by_bar: set[int] = set()
            selected_menu_by_bar: set[int] = set()
            selected_item_by_menu: set[int] = set()
            selected_tab_by_tabset: set[int] = set()
            for control_key, definition in owner_scene.controls.items():
                if (
                    control_key != definition.control_id
                    or definition.owner != owner_scene.owner
                ):
                    self._fail(SceneErrorCode.GRAPH, "control map key or owner is invalid")
                if definition.region_id not in owner_scene.regions:
                    self._fail(SceneErrorCode.GRAPH, "control refers to an absent region")
                self._validate_control_policy(definition)
                self._validate_control_dependencies(owner_scene, definition)
                if definition.kind in {
                    ControlKind.MENU_BAR,
                    ControlKind.TEXT_AREA,
                    ControlKind.TEXT_GRID,
                    ControlKind.TABSET,
                }:
                    continue
                order_key = (definition.parent_control_id, definition.order)
                if order_key in sibling_orders:
                    self._fail(SceneErrorCode.GRAPH, "control sibling order is duplicated")
                sibling_orders.add(order_key)
                if definition.kind is ControlKind.MENU:
                    if definition.state & ControlState.OPEN:
                        if definition.parent_control_id in open_menu_by_bar:
                            self._fail(SceneErrorCode.GRAPH, "MENU_BAR has multiple open menus")
                        open_menu_by_bar.add(definition.parent_control_id)
                    if definition.state & ControlState.SELECTED:
                        if definition.parent_control_id in selected_menu_by_bar:
                            self._fail(
                                SceneErrorCode.GRAPH,
                                "MENU_BAR has multiple selected menus",
                            )
                        selected_menu_by_bar.add(definition.parent_control_id)
                elif (
                    definition.kind is ControlKind.MENU_ITEM
                    and definition.state & ControlState.SELECTED
                ):
                    if definition.parent_control_id in selected_item_by_menu:
                        self._fail(SceneErrorCode.GRAPH, "MENU has multiple selected items")
                    selected_item_by_menu.add(definition.parent_control_id)
                elif (
                    definition.kind is ControlKind.TAB
                    and definition.state & ControlState.SELECTED
                ):
                    if definition.parent_control_id in selected_tab_by_tabset:
                        self._fail(
                            SceneErrorCode.GRAPH,
                            "TABSET has multiple selected tabs",
                        )
                    selected_tab_by_tabset.add(definition.parent_control_id)

    def _require_staging(self) -> _SceneStaging:
        if self._staging is None:
            raise SceneModelError(SceneErrorCode.STATE, "no retained transaction is open")
        return self._staging

    def _require_mutable_staging(self) -> _SceneStaging:
        staging = self._require_staging()
        if staging.prepared:
            staging.rejected = True
            raise SceneModelError(
                SceneErrorCode.STATE,
                "retained transaction is already prepared and frozen",
            )
        if staging.rejected:
            raise SceneModelError(SceneErrorCode.STATE, "retained transaction was rejected")
        return staging

    def _fail(self, code: SceneErrorCode, detail: str):
        if self._staging is not None:
            self._staging.rejected = True
        raise SceneModelError(code, detail)


__all__ = [
    "CommitDisposition",
    "CONTROL_STATE_MASK",
    "ControlDefinition",
    "ControlKind",
    "ControlState",
    "ExplicitSamples",
    "GroupBody",
    "GLYPH_RUN_ATTRIBUTE_MASK",
    "HiddenTargetKind",
    "GlyphRunBody",
    "ImageBody",
    "ImageFit",
    "MeterBody",
    "ObjectBounds",
    "ObjectDefinition",
    "ObjectKind",
    "OwnerScene",
    "PlotBody",
    "Point",
    "PolylineBody",
    "PreparedOwnerRetirement",
    "PreparedSceneInstall",
    "RGBA",
    "ReadoutBody",
    "ReadoutFormat",
    "RebuildRequirement",
    "RegionDefinition",
    "RetainedMode",
    "RetainedScene",
    "RetainedSceneModel",
    "Sample",
    "SceneErrorCode",
    "SceneModelError",
    "SceneModelState",
    "SceneUsage",
    "SeriesDefinition",
    "StatusBody",
    "TimestampMode",
    "UniformSamples",
    "validate_control_shape",
    "WaveformBody",
]
