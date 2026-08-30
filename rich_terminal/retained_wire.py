"""Pure RETAINED-1 discovery and transaction-envelope payload codec.

This module stops at the payload boundary.  It neither owns frame ordering nor
mutates a terminal output model; callers first use the base APT-1 frame codec and
then decode the exact payload type selected by the frame message ID.
"""

from __future__ import annotations

import operator
import struct
from dataclasses import dataclass
from enum import Enum, IntEnum

from .apt1 import UINT16_MAX, UINT32_MAX, UINT64_MAX
from .retained_model import OwnerQuotas, RetainedFeature, RetainedPolicy
from .retained_scene import (
    CONTROL_STATE_MASK,
    ControlKind,
    ControlState,
    ExplicitSamples,
    GLYPH_RUN_ATTRIBUTE_MASK,
    GroupBody,
    GlyphRunBody,
    MeterBody,
    ObjectBounds,
    ObjectKind,
    PlotBody,
    Point,
    PolylineBody,
    ReadoutBody,
    ReadoutFormat,
    RGBA,
    Sample,
    StatusBody,
    TimestampMode,
    UniformSamples,
    validate_control_shape,
    WaveformBody,
)
from .semantic_content import (
    SemanticContentError,
    SemanticTextContent,
    decode_semantic_text_content,
    encode_semantic_text_content,
)


RET1_TAG = 0x31544552
_RETAINED_FEATURE_MASK = 0x33F

_RET_QUERY = struct.Struct("<II")
_RET_CAPS = struct.Struct("<IHHQIIIIIIIIQQ")
_RET_FORMATS = struct.Struct("<IIIIIIIIIIQQQ")
_OWNER_OPEN = struct.Struct("<QQIIIIQQQQ")
_RET_RESULT = struct.Struct("<HHIQQQQQ")
_OWNER_DROP = struct.Struct("<QQQQ")
_PRESENT_BEGIN = struct.Struct("<QQQQIIIIIIII")
_PRESENT_COMMIT = struct.Struct("<QII")
_REGION_DEFINITION = struct.Struct("<QQQIIIIiI")
_OWNER_ITEM = struct.Struct("<QQQ")
_OBJECT_PREFIX = struct.Struct("<QQQHHiQQIIII")
_POLYLINE_BODY = struct.Struct("<II4BI")
_POINT = struct.Struct("<II")
_GLYPH_RUN_BODY = struct.Struct("<4B4BHHI")
_READOUT_BODY = struct.Struct("<8BIIqqII")
_METER_BODY = struct.Struct("<8BIIqqqQ")
_STATUS_BODY = struct.Struct("<8BqIIQ")
_PLOT_BODY = struct.Struct("<Qqq8BII")
_WAVEFORM_BODY = struct.Struct("<Qqq8BqII")
_OBJECT_SET_VALUE = struct.Struct("<QQQq")
_OBJECT_SET_VISIBILITY = struct.Struct("<QQQB7s")
_SERIES_DEFINITION = struct.Struct("<QQQIIQ")
_SERIES_SAMPLES = struct.Struct("<QQQIIQ")
_EXPLICIT_SAMPLE = struct.Struct("<Qq")
_UNIFORM_SAMPLE = struct.Struct("<q")
_CONTROL_PREFIX = struct.Struct("<QQQHHiQQIIIIIIII")
_CONTROL_EVENT = struct.Struct("<QQQHHIQ")


class RetainedMessageType(IntEnum):
    RET_RESULT = 0x000A
    OWNER_DROP = 0x000B
    RESOURCE_ABORT = 0x000C
    RESOURCE_BEGIN = 0x1000
    RESOURCE_CHUNK = 0x1001
    RESOURCE_COMMIT = 0x1002
    RESOURCE_DROP = 0x1003
    PRESENT_BEGIN = 0x2000
    PRESENT_COMMIT = 0x2001
    OWNER_OPEN = 0x2002
    REGION_DEFINE = 0x2010
    REGION_REPLACE = 0x2011
    REGION_DROP = 0x2012
    OBJECT_DEFINE = 0x2020
    OBJECT_REPLACE = 0x2021
    OBJECT_SET_VALUE = 0x2022
    OBJECT_SET_VISIBILITY = 0x2023
    OBJECT_DROP = 0x2024
    SERIES_DEFINE = 0x3000
    SERIES_APPEND = 0x3001
    SERIES_REPLACE = 0x3002
    SERIES_DROP = 0x3003
    CONTROL_DEFINE = 0x4000
    CONTROL_REPLACE = 0x4001
    CONTROL_DROP = 0x4002
    RET_QUERY = 0x8000
    RET_CAPS = 0x8001
    RET_FORMATS = 0x8002


class RetStatus(IntEnum):
    OK = 0
    INVALID = 1
    STALE_OWNER = 2
    NO_CAPACITY = 3
    DUPLICATE_ID = 4
    IN_USE = 5
    BAD_CONTENT = 6
    ABORTED = 7


class CellMode(IntEnum):
    NONE = 0
    DELTA = 1
    REPLACE = 2


class PresentRetainedMode(IntEnum):
    NONE = 0
    DELTA = 1
    REPLACE_START = 2
    REPLACE_CONTINUE = 3
    LAYOUT_START = 4
    LAYOUT_CONTINUE = 5


class PresentDisposition(IntEnum):
    COMMIT = 0
    COMMIT_AND_REVEAL = 1


class ControlEventKind(IntEnum):
    ACTIVATE = 1


class RetainedWireErrorCode(str, Enum):
    PAYLOAD = "PAYLOAD"
    RESERVED = "RESERVED"
    SCALAR = "SCALAR"
    ENUM = "ENUM"
    CONSISTENCY = "CONSISTENCY"


class RetainedWireError(ValueError):
    def __init__(self, code: RetainedWireErrorCode, detail: str):
        self.code = code
        self.detail = detail
        super().__init__(f"{code.value}: {detail}")


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


def _payload(value, size: int, name: str) -> bytes:
    if isinstance(value, str):
        raise TypeError(f"{name} payload must be bytes-like, not str")
    try:
        raw = bytes(value)
    except (TypeError, ValueError) as exc:
        raise TypeError(f"{name} payload must be bytes-like") from exc
    if len(raw) != size:
        raise RetainedWireError(
            RetainedWireErrorCode.PAYLOAD,
            f"{name} payload is {len(raw)} bytes, expected {size}",
        )
    return raw


def _variable_payload(value, minimum_size: int, name: str) -> bytes:
    """Copy one already frame-bounded payload and enforce its fixed prefix."""

    if isinstance(value, str):
        raise TypeError(f"{name} payload must be bytes-like, not str")
    try:
        raw = bytes(value)
    except (TypeError, ValueError) as exc:
        raise TypeError(f"{name} payload must be bytes-like") from exc
    if len(raw) < minimum_size:
        raise RetainedWireError(
            RetainedWireErrorCode.PAYLOAD,
            f"{name} payload is {len(raw)} bytes, expected at least {minimum_size}",
        )
    return raw


def _boolean(name: str, value) -> bool:
    if not isinstance(value, bool):
        raise TypeError(f"{name} must be bool")
    return value


def _enum(name: str, enum_type, value):
    if isinstance(value, bool):
        raise TypeError(f"{name} must not be bool")
    try:
        return enum_type(value)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{name} is not a valid {enum_type.__name__}") from exc


def _checked_add(name: str, *values: int) -> int:
    total = 0
    for value in values:
        if value > UINT64_MAX - total:
            raise ValueError(f"{name} overflows uint64")
        total += value
    return total


def _checked_multiply(name: str, left: int, right: int) -> int:
    if left and right > UINT64_MAX // left:
        raise ValueError(f"{name} overflows uint64")
    return left * right


def _control_text_bytes(name: str, text: str) -> bytes:
    if not isinstance(text, str):
        raise TypeError(f"{name} must be str")
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in text):
        raise ValueError(f"{name} contains a control character")
    try:
        return text.encode("utf-8", "strict")
    except UnicodeEncodeError as exc:
        raise ValueError(f"{name} contains a non-scalar surrogate") from exc


@dataclass(frozen=True, slots=True)
class RetainedQuery:
    pass


@dataclass(frozen=True, slots=True)
class RetainedCaps:
    features: RetainedFeature
    max_owner_records: int
    max_live_owners: int
    max_regions: int
    max_resources: int
    max_objects: int
    max_series: int
    max_operations_per_transaction: int
    max_resource_chunk_bytes: int
    max_retained_transaction_bytes: int
    total_resource_bytes: int

    def __post_init__(self) -> None:
        if isinstance(self.features, bool):
            raise TypeError("features must not be bool")
        try:
            features = RetainedFeature(operator.index(self.features))
        except (TypeError, ValueError) as exc:
            raise TypeError("features must be RetainedFeature-compatible") from exc
        if int(features) & ~_RETAINED_FEATURE_MASK:
            raise ValueError("features contain reserved RETAINED-1 bits")
        if not features & RetainedFeature.CORE:
            raise ValueError("RETAINED-1 requires CORE")
        if features & RetainedFeature.SERIES and not features & RetainedFeature.INSTRUMENT:
            raise ValueError("SERIES requires INSTRUMENT")
        if (
            features & RetainedFeature.CONTROL_COLLECTIONS
            and not features & RetainedFeature.CONTROLS
        ):
            raise ValueError("CONTROL_COLLECTIONS requires CONTROLS")
        object.__setattr__(self, "features", features)
        for name in (
            "max_owner_records",
            "max_live_owners",
            "max_regions",
            "max_resources",
            "max_objects",
            "max_series",
            "max_operations_per_transaction",
            "max_resource_chunk_bytes",
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        for name in ("max_retained_transaction_bytes", "total_resource_bytes"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT64_MAX),
            )
        if self.max_owner_records == 0 or self.max_live_owners == 0:
            raise ValueError("owner maxima must be positive")
        if self.max_live_owners > self.max_owner_records:
            raise ValueError("max_live_owners exceeds max_owner_records")
        if (
            self.max_regions == 0
            or self.max_operations_per_transaction == 0
            or self.max_retained_transaction_bytes == 0
        ):
            raise ValueError("CORE maxima must be positive")
        if features & RetainedFeature.CONTROLS and self.max_objects == 0:
            raise ValueError("CONTROLS requires object capacity")

    def policy(
        self,
        formats: RetainedFormats,
        *,
        client_to_terminal_max_payload: int,
        terminal_to_client_max_payload: int,
        base_max_transaction_bytes: int,
    ) -> RetainedPolicy:
        if not isinstance(formats, RetainedFormats):
            raise TypeError("formats must be RetainedFormats")
        return RetainedPolicy(
            features=self.features,
            max_owner_records=self.max_owner_records,
            max_live_owners=self.max_live_owners,
            max_regions=self.max_regions,
            max_resources=self.max_resources,
            max_objects=self.max_objects,
            max_series=self.max_series,
            max_operations_per_transaction=self.max_operations_per_transaction,
            max_resource_chunk_bytes=self.max_resource_chunk_bytes,
            max_retained_transaction_bytes=self.max_retained_transaction_bytes,
            total_resource_bytes=self.total_resource_bytes,
            image_format=formats.image_format,
            max_image_width=formats.max_image_width,
            max_image_height=formats.max_image_height,
            max_path_points=formats.max_path_points,
            max_glyph_run_bytes=formats.max_glyph_run_bytes,
            max_samples_per_append=formats.max_samples_per_append,
            max_history_per_series=formats.max_history_per_series,
            minimum_presentation_interval_us=formats.minimum_presentation_interval_us,
            total_sample_slots=formats.total_sample_slots,
            total_utf8_bytes=formats.total_utf8_bytes,
            client_to_terminal_max_payload=client_to_terminal_max_payload,
            terminal_to_client_max_payload=terminal_to_client_max_payload,
            base_max_transaction_bytes=base_max_transaction_bytes,
        )


@dataclass(frozen=True, slots=True)
class RetainedFormats:
    coordinate_format: int
    color_format: int
    image_format: int
    max_image_width: int
    max_image_height: int
    max_path_points: int
    max_glyph_run_bytes: int
    max_samples_per_append: int
    max_history_per_series: int
    minimum_presentation_interval_us: int
    total_sample_slots: int
    total_utf8_bytes: int

    def __post_init__(self) -> None:
        for name in (
            "coordinate_format",
            "color_format",
            "image_format",
            "max_image_width",
            "max_image_height",
            "max_path_points",
            "max_glyph_run_bytes",
            "max_samples_per_append",
            "max_history_per_series",
            "minimum_presentation_interval_us",
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT32_MAX),
            )
        for name in ("total_sample_slots", "total_utf8_bytes"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=0, maximum=UINT64_MAX),
            )
        if self.coordinate_format != 1 or self.color_format != 1:
            raise ValueError("RETAINED-1 requires UNORM32 coordinates and RGBA8 colors")
        if self.image_format not in (0, 1):
            raise ValueError("image_format must be zero or raw RGBA8")


@dataclass(frozen=True, slots=True)
class OwnerOpen:
    owner_id: int
    owner_generation: int
    quotas: OwnerQuotas

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "owner_id", _integer("owner_id", self.owner_id, minimum=1, maximum=UINT64_MAX)
        )
        object.__setattr__(
            self,
            "owner_generation",
            _integer(
                "owner_generation", self.owner_generation, minimum=1, maximum=UINT64_MAX
            ),
        )
        if not isinstance(self.quotas, OwnerQuotas):
            raise TypeError("quotas must be OwnerQuotas")


_LIFECYCLE_REQUESTS = {
    RetainedMessageType.OWNER_OPEN,
    RetainedMessageType.RESOURCE_BEGIN,
    RetainedMessageType.RESOURCE_CHUNK,
    RetainedMessageType.RESOURCE_COMMIT,
    RetainedMessageType.RESOURCE_DROP,
    RetainedMessageType.RESOURCE_ABORT,
}

_RESULT_STATUSES = {
    RetainedMessageType.OWNER_OPEN: {
        RetStatus.OK,
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
        RetStatus.NO_CAPACITY,
    },
    RetainedMessageType.RESOURCE_BEGIN: {
        RetStatus.OK,
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
        RetStatus.NO_CAPACITY,
        RetStatus.DUPLICATE_ID,
    },
    # Accepted chunks are acknowledged only by covering CREDIT.
    RetainedMessageType.RESOURCE_CHUNK: {
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
    },
    RetainedMessageType.RESOURCE_COMMIT: {
        RetStatus.OK,
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
        RetStatus.BAD_CONTENT,
    },
    RetainedMessageType.RESOURCE_DROP: {
        RetStatus.OK,
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
        RetStatus.IN_USE,
    },
    RetainedMessageType.RESOURCE_ABORT: {
        RetStatus.INVALID,
        RetStatus.STALE_OWNER,
        RetStatus.ABORTED,
    },
}


@dataclass(frozen=True, slots=True)
class RetainedResult:
    request_type: RetainedMessageType
    status: RetStatus
    owner_id: int
    owner_generation: int
    item_id: int
    current_revision: int
    accepted_bytes: int = 0

    def __post_init__(self) -> None:
        request = _enum("request_type", RetainedMessageType, self.request_type)
        status = _enum("status", RetStatus, self.status)
        if request not in _LIFECYCLE_REQUESTS:
            raise ValueError("RET_RESULT request_type is not a lifecycle request")
        if status not in _RESULT_STATUSES[request]:
            raise ValueError(
                f"{status.name} is not valid for {request.name} RET_RESULT"
            )
        object.__setattr__(self, "request_type", request)
        object.__setattr__(self, "status", status)
        owner_minimum = 0 if status is RetStatus.INVALID else 1
        for name, minimum in (
            ("owner_id", owner_minimum),
            ("owner_generation", owner_minimum),
            ("item_id", 0),
            ("current_revision", 0),
            ("accepted_bytes", 0),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )
        if request is RetainedMessageType.OWNER_OPEN:
            if self.item_id != 0:
                raise ValueError("OWNER_OPEN result item_id must be zero")
        elif self.item_id == 0 and status is not RetStatus.INVALID:
            raise ValueError("resource result item_id must be nonzero")
        successful_commit = (
            request is RetainedMessageType.RESOURCE_COMMIT
            and status is RetStatus.OK
        )
        if successful_commit and self.accepted_bytes == 0:
            raise ValueError("successful RESOURCE_COMMIT accepted_bytes must be positive")
        if not successful_commit and self.accepted_bytes != 0:
            raise ValueError("accepted_bytes is nonzero outside successful RESOURCE_COMMIT")


@dataclass(frozen=True, slots=True)
class OwnerDrop:
    transaction_id: int
    base_revision: int
    owner_id: int
    owner_generation: int

    def __post_init__(self) -> None:
        for name, minimum in (
            ("transaction_id", 1),
            ("base_revision", 0),
            ("owner_id", 1),
            ("owner_generation", 1),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )


@dataclass(frozen=True, slots=True)
class PresentBegin:
    transaction_id: int
    base_revision: int
    geometry_generation: int
    declared_transaction_bytes: int
    cols: int
    rows: int
    cell_span_count: int
    cell_count: int
    retained_operation_count: int
    cell_mode: CellMode
    retained_mode: PresentRetainedMode

    def __post_init__(self) -> None:
        for name, minimum in (
            ("transaction_id", 1),
            ("base_revision", 0),
            ("geometry_generation", 0),
            ("declared_transaction_bytes", 1),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )
        for name, minimum in (
            ("cols", 1),
            ("rows", 1),
            ("cell_span_count", 0),
            ("cell_count", 0),
            ("retained_operation_count", 0),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT32_MAX),
            )
        cell_mode = _enum("cell_mode", CellMode, self.cell_mode)
        retained_mode = _enum(
            "retained_mode", PresentRetainedMode, self.retained_mode
        )
        object.__setattr__(self, "cell_mode", cell_mode)
        object.__setattr__(self, "retained_mode", retained_mode)
        if cell_mode is CellMode.NONE:
            if self.cell_span_count or self.cell_count:
                raise ValueError("CELL_NONE requires zero CELL counts")
        elif cell_mode is CellMode.DELTA:
            if (self.cell_span_count == 0) != (self.cell_count == 0):
                raise ValueError("CELL_DELTA zero span and cell counts must agree")
        else:
            cells = _checked_multiply("CELL_REPLACE geometry", self.cols, self.rows)
            if cells > UINT32_MAX:
                raise ValueError("CELL_REPLACE cell count exceeds its u32 field")
            if self.cell_span_count != self.rows or self.cell_count != cells:
                raise ValueError("CELL_REPLACE counts are not canonical full rows")
        if retained_mode is PresentRetainedMode.NONE:
            if self.retained_operation_count:
                raise ValueError("RET_NONE requires zero retained operations")
        elif retained_mode is PresentRetainedMode.DELTA:
            if self.retained_operation_count == 0:
                raise ValueError("RET_DELTA requires at least one operation")
        if cell_mode is CellMode.NONE and retained_mode is PresentRetainedMode.NONE:
            raise ValueError("PRESENT_BEGIN cannot have both modes NONE")

        minimum = 160  # complete BEGIN plus complete COMMIT
        if cell_mode is not CellMode.NONE:
            span_bytes = _checked_add(
                "CELL span bytes",
                _checked_multiply("CELL span frame prefixes", self.cell_span_count, 52),
                _checked_multiply("CELL values", self.cell_count, 8),
            )
            minimum = _checked_add("PRESENT transaction minimum", minimum, 56, span_bytes)
        minimum = _checked_add(
            "PRESENT transaction minimum",
            minimum,
            _checked_multiply(
                "retained operation minimum frames", self.retained_operation_count, 64
            ),
        )
        if self.declared_transaction_bytes < minimum:
            raise ValueError("declared transaction bytes are below the canonical minimum")
        if self.retained_operation_count == 0 and self.declared_transaction_bytes != minimum:
            raise ValueError("operation-free declared transaction bytes are not exact")


@dataclass(frozen=True, slots=True)
class PresentCommit:
    transaction_id: int
    disposition: PresentDisposition

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "transaction_id",
            _integer(
                "transaction_id", self.transaction_id, minimum=1, maximum=UINT64_MAX
            ),
        )
        object.__setattr__(
            self,
            "disposition",
            _enum("disposition", PresentDisposition, self.disposition),
        )


@dataclass(frozen=True, slots=True)
class RegionWireDefinition:
    """Exact REGION_DEFINE/REGION_REPLACE payload before session binding."""

    owner_id: int
    owner_generation: int
    region_id: int
    cell_x: int
    cell_y: int
    cell_cols: int
    cell_rows: int
    z_order: int
    flags: int

    def __post_init__(self) -> None:
        for name, minimum, maximum in (
            ("owner_id", 1, UINT64_MAX),
            ("owner_generation", 1, UINT64_MAX),
            ("region_id", 1, UINT64_MAX),
            ("cell_x", 0, UINT32_MAX),
            ("cell_y", 0, UINT32_MAX),
            ("cell_cols", 1, UINT32_MAX),
            ("cell_rows", 1, UINT32_MAX),
            ("z_order", -(1 << 31), (1 << 31) - 1),
            ("flags", 0, 0x3),
        ):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=minimum,
                    maximum=maximum,
                ),
            )

    @property
    def visible(self) -> bool:
        return bool(self.flags & 0x1)

    @property
    def clipped(self) -> bool:
        return bool(self.flags & 0x2)


ObjectWireBody = (
    GroupBody
    | PolylineBody
    | GlyphRunBody
    | ReadoutBody
    | MeterBody
    | StatusBody
    | PlotBody
    | WaveformBody
)


_WIRE_BODY_KIND = {
    GroupBody: ObjectKind.GROUP,
    PolylineBody: ObjectKind.POLYLINE,
    GlyphRunBody: ObjectKind.GLYPH_RUN,
    ReadoutBody: ObjectKind.READOUT,
    MeterBody: ObjectKind.METER,
    StatusBody: ObjectKind.STATUS,
    PlotBody: ObjectKind.PLOT,
    WaveformBody: ObjectKind.WAVEFORM,
}


@dataclass(frozen=True, slots=True)
class RetainedItemReference:
    """Exact owner-scoped REGION/OBJECT/SERIES/CONTROL drop payload."""

    owner_id: int
    owner_generation: int
    item_id: int

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "item_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )


@dataclass(frozen=True, slots=True)
class ControlWireDefinition:
    """Complete semantic control definition before session binding."""

    owner_id: int
    owner_generation: int
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
        for name, minimum in (
            ("owner_id", 1),
            ("owner_generation", 1),
            ("control_id", 1),
            ("region_id", 1),
            ("parent_control_id", 0),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )
        object.__setattr__(
            self,
            "z_order",
            _integer(
                "z_order",
                self.z_order,
                minimum=-(1 << 31),
                maximum=(1 << 31) - 1,
            ),
        )
        object.__setattr__(
            self,
            "order",
            _integer("order", self.order, minimum=0, maximum=UINT32_MAX),
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
class ControlEvent:
    """Revision-bound semantic activation emitted by the terminal."""

    owner_id: int
    owner_generation: int
    control_id: int
    event_kind: ControlEventKind
    modifiers: int
    model_revision: int

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "control_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        object.__setattr__(
            self,
            "event_kind",
            _enum("event_kind", ControlEventKind, self.event_kind),
        )
        object.__setattr__(
            self,
            "modifiers",
            _integer("modifiers", self.modifiers, minimum=0, maximum=0x3F),
        )
        object.__setattr__(
            self,
            "model_revision",
            _integer(
                "model_revision",
                self.model_revision,
                minimum=0,
                maximum=UINT64_MAX,
            ),
        )


@dataclass(frozen=True, slots=True)
class ObjectWireDefinition:
    """Complete non-image object definition before session binding."""

    owner_id: int
    owner_generation: int
    object_id: int
    region_id: int
    parent_object_id: int
    bounds: ObjectBounds
    z_order: int
    visible: bool
    body: ObjectWireBody

    def __post_init__(self) -> None:
        for name, minimum in (
            ("owner_id", 1),
            ("owner_generation", 1),
            ("object_id", 1),
            ("region_id", 1),
            ("parent_object_id", 0),
        ):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=minimum, maximum=UINT64_MAX),
            )
        if not isinstance(self.bounds, ObjectBounds):
            raise TypeError("bounds must be ObjectBounds")
        object.__setattr__(
            self,
            "z_order",
            _integer("z_order", self.z_order, minimum=-(1 << 31), maximum=(1 << 31) - 1),
        )
        object.__setattr__(self, "visible", _boolean("visible", self.visible))
        if type(self.body) not in _WIRE_BODY_KIND:
            raise TypeError("body is not a supported non-image RETAINED-1 body")

    @property
    def kind(self) -> ObjectKind:
        return _WIRE_BODY_KIND[type(self.body)]


@dataclass(frozen=True, slots=True)
class ObjectSetValue:
    owner_id: int
    owner_generation: int
    object_id: int
    value: int

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "object_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        object.__setattr__(
            self,
            "value",
            _integer("value", self.value, minimum=-(1 << 63), maximum=(1 << 63) - 1),
        )


@dataclass(frozen=True, slots=True)
class ObjectSetVisibility:
    owner_id: int
    owner_generation: int
    object_id: int
    visible: bool

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "object_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        object.__setattr__(self, "visible", _boolean("visible", self.visible))


@dataclass(frozen=True, slots=True)
class SeriesWireDefinition:
    owner_id: int
    owner_generation: int
    series_id: int
    history_capacity: int
    timestamp_mode: TimestampMode
    uniform_interval_us: int

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "series_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        object.__setattr__(
            self,
            "history_capacity",
            _integer(
                "history_capacity",
                self.history_capacity,
                minimum=1,
                maximum=UINT32_MAX,
            ),
        )
        mode = _enum("timestamp_mode", TimestampMode, self.timestamp_mode)
        object.__setattr__(self, "timestamp_mode", mode)
        object.__setattr__(
            self,
            "uniform_interval_us",
            _integer(
                "uniform_interval_us",
                self.uniform_interval_us,
                minimum=0,
                maximum=UINT64_MAX,
            ),
        )
        if mode is TimestampMode.EXPLICIT:
            if self.uniform_interval_us != 0:
                raise ValueError("explicit series interval must be zero")
        elif self.uniform_interval_us == 0:
            raise ValueError("uniform series interval must be positive")


SeriesWireBatch = ExplicitSamples | UniformSamples


@dataclass(frozen=True, slots=True)
class SeriesWireSamples:
    owner_id: int
    owner_generation: int
    series_id: int
    batch: SeriesWireBatch

    def __post_init__(self) -> None:
        for name in ("owner_id", "owner_generation", "series_id"):
            object.__setattr__(
                self,
                name,
                _integer(name, getattr(self, name), minimum=1, maximum=UINT64_MAX),
            )
        if not isinstance(self.batch, (ExplicitSamples, UniformSamples)):
            raise TypeError("batch must be ExplicitSamples or UniformSamples")
        count = (
            len(self.batch.samples)
            if isinstance(self.batch, ExplicitSamples)
            else len(self.batch.values)
        )
        _integer("sample_count", count, minimum=1, maximum=UINT32_MAX)

    @property
    def timestamp_mode(self) -> TimestampMode:
        if isinstance(self.batch, ExplicitSamples):
            return TimestampMode.EXPLICIT
        return TimestampMode.UNIFORM

    @property
    def sample_count(self) -> int:
        if isinstance(self.batch, ExplicitSamples):
            return len(self.batch.samples)
        return len(self.batch.values)


def encode_ret_query(_query: RetainedQuery = RetainedQuery()) -> bytes:
    if not isinstance(_query, RetainedQuery):
        raise TypeError("query must be RetainedQuery")
    return _RET_QUERY.pack(RET1_TAG, 0)


def decode_ret_query(payload) -> RetainedQuery:
    raw = _payload(payload, _RET_QUERY.size, "RET_QUERY")
    tag, reserved = _RET_QUERY.unpack(raw)
    if tag != RET1_TAG:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, "RET_QUERY tag is not RET1")
    if reserved:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "RET_QUERY reserved is nonzero")
    return RetainedQuery()


def encode_ret_caps(caps: RetainedCaps) -> bytes:
    if not isinstance(caps, RetainedCaps):
        raise TypeError("caps must be RetainedCaps")
    return _RET_CAPS.pack(
        RET1_TAG, 0, 0, int(caps.features), caps.max_owner_records,
        caps.max_live_owners, caps.max_regions, caps.max_resources,
        caps.max_objects, caps.max_series, caps.max_operations_per_transaction,
        caps.max_resource_chunk_bytes, caps.max_retained_transaction_bytes,
        caps.total_resource_bytes,
    )


def decode_ret_caps(payload) -> RetainedCaps:
    raw = _payload(payload, _RET_CAPS.size, "RET_CAPS")
    tag, reserved0, reserved1, *values = _RET_CAPS.unpack(raw)
    if tag != RET1_TAG:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, "RET_CAPS tag is invalid")
    if reserved0 or reserved1:
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "RET_CAPS reserved fields are nonzero",
        )
    try:
        return RetainedCaps(*values)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_ret_formats(formats: RetainedFormats) -> bytes:
    if not isinstance(formats, RetainedFormats):
        raise TypeError("formats must be RetainedFormats")
    return _RET_FORMATS.pack(
        formats.coordinate_format, formats.color_format, formats.image_format,
        formats.max_image_width, formats.max_image_height, formats.max_path_points,
        formats.max_glyph_run_bytes, formats.max_samples_per_append,
        formats.max_history_per_series, formats.minimum_presentation_interval_us,
        formats.total_sample_slots, formats.total_utf8_bytes, 0,
    )


def decode_ret_formats(payload) -> RetainedFormats:
    raw = _payload(payload, _RET_FORMATS.size, "RET_FORMATS")
    *values, reserved = _RET_FORMATS.unpack(raw)
    if reserved:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "RET_FORMATS reserved is nonzero")
    try:
        return RetainedFormats(*values)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_owner_open(request: OwnerOpen) -> bytes:
    if not isinstance(request, OwnerOpen):
        raise TypeError("request must be OwnerOpen")
    quota = request.quotas
    return _OWNER_OPEN.pack(
        request.owner_id, request.owner_generation, quota.regions, quota.resources,
        quota.objects, quota.series, quota.resource_bytes, quota.utf8_bytes,
        quota.sample_slots, 0,
    )


def decode_owner_open(payload) -> OwnerOpen:
    raw = _payload(payload, _OWNER_OPEN.size, "OWNER_OPEN")
    owner_id, generation, regions, resources, objects, series, resource_bytes, utf8_bytes, sample_slots, reserved = _OWNER_OPEN.unpack(raw)
    if reserved:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "OWNER_OPEN reserved is nonzero")
    try:
        return OwnerOpen(
            owner_id, generation,
            OwnerQuotas(regions, resources, objects, series, resource_bytes, utf8_bytes, sample_slots),
        )
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_ret_result(result: RetainedResult) -> bytes:
    if not isinstance(result, RetainedResult):
        raise TypeError("result must be RetainedResult")
    return _RET_RESULT.pack(
        int(result.request_type), int(result.status), 0, result.owner_id,
        result.owner_generation, result.item_id, result.current_revision,
        result.accepted_bytes,
    )


def decode_ret_result(payload) -> RetainedResult:
    raw = _payload(payload, _RET_RESULT.size, "RET_RESULT")
    request, status, detail, owner_id, generation, item_id, revision, accepted = _RET_RESULT.unpack(raw)
    if detail:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "RET_RESULT detail is nonzero")
    try:
        return RetainedResult(request, status, owner_id, generation, item_id, revision, accepted)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_owner_drop(request: OwnerDrop) -> bytes:
    if not isinstance(request, OwnerDrop):
        raise TypeError("request must be OwnerDrop")
    return _OWNER_DROP.pack(
        request.transaction_id, request.base_revision,
        request.owner_id, request.owner_generation,
    )


def decode_owner_drop(payload) -> OwnerDrop:
    raw = _payload(payload, _OWNER_DROP.size, "OWNER_DROP")
    try:
        return OwnerDrop(*_OWNER_DROP.unpack(raw))
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_present_begin(begin: PresentBegin) -> bytes:
    if not isinstance(begin, PresentBegin):
        raise TypeError("begin must be PresentBegin")
    return _PRESENT_BEGIN.pack(
        begin.transaction_id, begin.base_revision, begin.geometry_generation,
        begin.declared_transaction_bytes, begin.cols, begin.rows,
        begin.cell_span_count, begin.cell_count, begin.retained_operation_count,
        int(begin.cell_mode), int(begin.retained_mode), 0,
    )


def decode_present_begin(payload) -> PresentBegin:
    raw = _payload(payload, _PRESENT_BEGIN.size, "PRESENT_BEGIN")
    *values, reserved = _PRESENT_BEGIN.unpack(raw)
    if reserved:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "PRESENT_BEGIN reserved is nonzero")
    try:
        return PresentBegin(*values)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_present_commit(commit: PresentCommit) -> bytes:
    if not isinstance(commit, PresentCommit):
        raise TypeError("commit must be PresentCommit")
    return _PRESENT_COMMIT.pack(commit.transaction_id, int(commit.disposition), 0)


def decode_present_commit(payload) -> PresentCommit:
    raw = _payload(payload, _PRESENT_COMMIT.size, "PRESENT_COMMIT")
    transaction_id, disposition, reserved = _PRESENT_COMMIT.unpack(raw)
    if reserved:
        raise RetainedWireError(RetainedWireErrorCode.RESERVED, "PRESENT_COMMIT reserved is nonzero")
    try:
        return PresentCommit(transaction_id, disposition)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_region_definition(definition: RegionWireDefinition) -> bytes:
    if not isinstance(definition, RegionWireDefinition):
        raise TypeError("definition must be RegionWireDefinition")
    return _REGION_DEFINITION.pack(
        definition.owner_id,
        definition.owner_generation,
        definition.region_id,
        definition.cell_x,
        definition.cell_y,
        definition.cell_cols,
        definition.cell_rows,
        definition.z_order,
        definition.flags,
    )


def decode_region_definition(payload) -> RegionWireDefinition:
    raw = _payload(payload, _REGION_DEFINITION.size, "REGION definition")
    values = _REGION_DEFINITION.unpack(raw)
    if values[-1] & ~0x3:
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "REGION definition flags contain reserved bits",
        )
    try:
        return RegionWireDefinition(*values)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_region_replace(definition: RegionWireDefinition) -> bytes:
    return encode_region_definition(definition)


def decode_region_replace(payload) -> RegionWireDefinition:
    return decode_region_definition(payload)


def _encode_item_reference(reference: RetainedItemReference, name: str) -> bytes:
    if not isinstance(reference, RetainedItemReference):
        raise TypeError(f"reference must be RetainedItemReference for {name}")
    return _OWNER_ITEM.pack(
        reference.owner_id,
        reference.owner_generation,
        reference.item_id,
    )


def _decode_item_reference(payload, name: str) -> RetainedItemReference:
    raw = _payload(payload, _OWNER_ITEM.size, name)
    try:
        return RetainedItemReference(*_OWNER_ITEM.unpack(raw))
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_region_drop(reference: RetainedItemReference) -> bytes:
    return _encode_item_reference(reference, "REGION_DROP")


def decode_region_drop(payload) -> RetainedItemReference:
    return _decode_item_reference(payload, "REGION_DROP")


def _rgba_values(color: RGBA) -> tuple[int, int, int, int]:
    return color.red, color.green, color.blue, color.alpha


def _wire_text(raw: bytes, name: str) -> str:
    try:
        text = raw.decode("utf-8", "strict")
    except UnicodeDecodeError as exc:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            f"{name} is not well-formed UTF-8 scalar text",
        ) from exc
    if "\r" in text or "\n" in text or "\0" in text:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            f"{name} contains CR, LF, or NUL",
        )
    return text


def _wire_control_text(raw: bytes, name: str) -> str:
    text = _wire_text(raw, name)
    if any(ord(character) < 0x20 or ord(character) == 0x7F for character in text):
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            f"{name} contains a control character",
        )
    return text


def _body_size(raw: bytes, expected: int, name: str) -> None:
    if len(raw) != expected:
        raise RetainedWireError(
            RetainedWireErrorCode.PAYLOAD,
            f"{name} body is {len(raw)} bytes, expected {expected}",
        )


def _encode_object_body(body: ObjectWireBody) -> bytes:
    if isinstance(body, GroupBody):
        return b""
    if isinstance(body, PolylineBody):
        point_count = _integer(
            "point_count", len(body.points), minimum=2, maximum=UINT32_MAX
        )
        result = bytearray(_POLYLINE_BODY.size + point_count * _POINT.size)
        _POLYLINE_BODY.pack_into(
            result,
            0,
            point_count,
            body.stroke_width,
            *_rgba_values(body.color),
            int(body.closed),
        )
        offset = _POLYLINE_BODY.size
        for point in body.points:
            _POINT.pack_into(result, offset, point.x, point.y)
            offset += _POINT.size
        return bytes(result)
    if isinstance(body, GlyphRunBody):
        text = body.text.encode("utf-8", "strict")
        text_bytes = _integer(
            "text_bytes", len(text), minimum=0, maximum=UINT32_MAX
        )
        return _GLYPH_RUN_BODY.pack(
            *_rgba_values(body.foreground),
            *_rgba_values(body.background),
            body.attributes,
            0,
            text_bytes,
        ) + text
    if isinstance(body, ReadoutBody):
        unit = body.unit.encode("utf-8", "strict")
        unit_bytes = _integer(
            "unit_bytes", len(unit), minimum=0, maximum=UINT32_MAX
        )
        return _READOUT_BODY.pack(
            *_rgba_values(body.foreground),
            *_rgba_values(body.background),
            int(body.format),
            body.decimal_places,
            body.value,
            body.scale,
            unit_bytes,
            0,
        ) + unit
    if isinstance(body, MeterBody):
        return _METER_BODY.pack(
            *_rgba_values(body.foreground),
            *_rgba_values(body.background),
            int(body.vertical),
            int(body.show_value),
            body.minimum,
            body.maximum,
            body.value,
            0,
        )
    if isinstance(body, StatusBody):
        return _STATUS_BODY.pack(
            *_rgba_values(body.inactive),
            *_rgba_values(body.active),
            body.value,
            body.shape,
            0,
            0,
        )
    if isinstance(body, PlotBody):
        flags = int(body.fill_to_minimum) | (int(body.draw_points) << 1)
        return _PLOT_BODY.pack(
            body.series_id,
            body.minimum,
            body.maximum,
            *_rgba_values(body.line),
            *_rgba_values(body.fill),
            flags,
            0,
        )
    if isinstance(body, WaveformBody):
        return _WAVEFORM_BODY.pack(
            body.series_id,
            body.minimum,
            body.maximum,
            *_rgba_values(body.trace),
            *_rgba_values(body.zero_line),
            body.zero_value,
            int(body.draw_zero_line),
            0,
        )
    raise TypeError("body is not a supported non-image RETAINED-1 body")


def encode_object_definition(definition: ObjectWireDefinition) -> bytes:
    if not isinstance(definition, ObjectWireDefinition):
        raise TypeError("definition must be ObjectWireDefinition")
    return _OBJECT_PREFIX.pack(
        definition.owner_id,
        definition.owner_generation,
        definition.object_id,
        int(definition.kind),
        int(definition.visible),
        definition.z_order,
        definition.region_id,
        definition.parent_object_id,
        definition.bounds.left,
        definition.bounds.top,
        definition.bounds.right,
        definition.bounds.bottom,
    ) + _encode_object_body(definition.body)


def _decode_object_body(kind: ObjectKind, raw: bytes) -> ObjectWireBody:
    try:
        if kind is ObjectKind.GROUP:
            _body_size(raw, 0, "GROUP")
            return GroupBody()
        if kind is ObjectKind.POLYLINE:
            if len(raw) < _POLYLINE_BODY.size:
                _body_size(raw, _POLYLINE_BODY.size, "POLYLINE prefix")
            values = _POLYLINE_BODY.unpack_from(raw)
            point_count, stroke_width = values[:2]
            path_flags = values[-1]
            if path_flags & ~0x1:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "POLYLINE path flags contain reserved bits",
                )
            expected = _POLYLINE_BODY.size + point_count * _POINT.size
            _body_size(raw, expected, "POLYLINE")
            points = tuple(
                Point(*_POINT.unpack_from(raw, _POLYLINE_BODY.size + index * _POINT.size))
                for index in range(point_count)
            )
            return PolylineBody(
                points,
                stroke_width,
                RGBA(*values[2:6]),
                bool(path_flags),
            )
        if kind is ObjectKind.GLYPH_RUN:
            if len(raw) < _GLYPH_RUN_BODY.size:
                _body_size(raw, _GLYPH_RUN_BODY.size, "GLYPH_RUN prefix")
            values = _GLYPH_RUN_BODY.unpack_from(raw)
            attributes, reserved, text_bytes = values[8:11]
            if attributes & ~GLYPH_RUN_ATTRIBUTE_MASK:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "GLYPH_RUN attributes contain unsupported bits",
                )
            if reserved:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "GLYPH_RUN reserved field is nonzero",
                )
            _body_size(raw, _GLYPH_RUN_BODY.size + text_bytes, "GLYPH_RUN")
            return GlyphRunBody(
                RGBA(*values[:4]),
                RGBA(*values[4:8]),
                attributes,
                _wire_text(raw[_GLYPH_RUN_BODY.size :], "GLYPH_RUN text"),
            )
        if kind is ObjectKind.READOUT:
            if len(raw) < _READOUT_BODY.size:
                _body_size(raw, _READOUT_BODY.size, "READOUT prefix")
            values = _READOUT_BODY.unpack_from(raw)
            unit_bytes, reserved = values[12:14]
            if reserved:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "READOUT reserved is nonzero",
                )
            if values[8] not in tuple(int(member) for member in ReadoutFormat):
                raise RetainedWireError(
                    RetainedWireErrorCode.ENUM,
                    "READOUT format is not canonical",
                )
            _body_size(raw, _READOUT_BODY.size + unit_bytes, "READOUT")
            return ReadoutBody(
                RGBA(*values[:4]),
                RGBA(*values[4:8]),
                ReadoutFormat(values[8]),
                values[9],
                values[10],
                values[11],
                _wire_text(raw[_READOUT_BODY.size :], "READOUT unit"),
            )
        if kind is ObjectKind.METER:
            _body_size(raw, _METER_BODY.size, "METER")
            values = _METER_BODY.unpack(raw)
            orientation, meter_flags = values[8:10]
            if orientation not in (0, 1):
                raise RetainedWireError(
                    RetainedWireErrorCode.ENUM,
                    "METER orientation is not canonical",
                )
            if meter_flags & ~0x1 or values[13]:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "METER flags or reserved field are nonzero",
                )
            return MeterBody(
                RGBA(*values[:4]),
                RGBA(*values[4:8]),
                bool(orientation),
                bool(meter_flags),
                values[10],
                values[11],
                values[12],
            )
        if kind is ObjectKind.STATUS:
            _body_size(raw, _STATUS_BODY.size, "STATUS")
            values = _STATUS_BODY.unpack(raw)
            if values[9] not in (0, 1, 2):
                raise RetainedWireError(
                    RetainedWireErrorCode.ENUM,
                    "STATUS shape is not canonical",
                )
            if values[10] or values[11]:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "STATUS flags or reserved field are nonzero",
                )
            return StatusBody(
                RGBA(*values[:4]),
                RGBA(*values[4:8]),
                values[8],
                values[9],
            )
        if kind is ObjectKind.PLOT:
            _body_size(raw, _PLOT_BODY.size, "PLOT")
            values = _PLOT_BODY.unpack(raw)
            flags, reserved = values[11:13]
            if flags & ~0x3 or reserved:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "PLOT flags or reserved field are nonzero",
                )
            return PlotBody(
                values[0],
                values[1],
                values[2],
                RGBA(*values[3:7]),
                RGBA(*values[7:11]),
                bool(flags & 0x1),
                bool(flags & 0x2),
            )
        if kind is ObjectKind.WAVEFORM:
            _body_size(raw, _WAVEFORM_BODY.size, "WAVEFORM")
            values = _WAVEFORM_BODY.unpack(raw)
            flags, reserved = values[12:14]
            if flags & ~0x1 or reserved:
                raise RetainedWireError(
                    RetainedWireErrorCode.RESERVED,
                    "WAVEFORM flags or reserved field are nonzero",
                )
            return WaveformBody(
                values[0],
                values[1],
                values[2],
                RGBA(*values[3:7]),
                RGBA(*values[7:11]),
                values[11],
                bool(flags),
            )
    except RetainedWireError:
        raise
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc
    raise RetainedWireError(
        RetainedWireErrorCode.ENUM,
        f"object type {int(kind)} has no non-image codec",
    )


def decode_object_definition(payload) -> ObjectWireDefinition:
    raw = _variable_payload(payload, _OBJECT_PREFIX.size, "OBJECT definition")
    values = _OBJECT_PREFIX.unpack_from(raw)
    if not values[0] or not values[1] or not values[2] or not values[6]:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            "OBJECT owner, generation, object, and region IDs must be nonzero",
        )
    try:
        kind = ObjectKind(values[3])
    except ValueError as exc:
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            f"object type {values[3]} has no non-image codec",
        ) from exc
    if values[4] & ~0x1:
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "OBJECT flags contain reserved bits",
        )
    body = _decode_object_body(kind, raw[_OBJECT_PREFIX.size :])
    try:
        return ObjectWireDefinition(
            owner_id=values[0],
            owner_generation=values[1],
            object_id=values[2],
            region_id=values[6],
            parent_object_id=values[7],
            bounds=ObjectBounds(*values[8:12]),
            z_order=values[5],
            visible=bool(values[4]),
            body=body,
        )
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_object_replace(definition: ObjectWireDefinition) -> bytes:
    return encode_object_definition(definition)


def decode_object_replace(payload) -> ObjectWireDefinition:
    return decode_object_definition(payload)


def encode_object_set_value(update: ObjectSetValue) -> bytes:
    if not isinstance(update, ObjectSetValue):
        raise TypeError("update must be ObjectSetValue")
    return _OBJECT_SET_VALUE.pack(
        update.owner_id,
        update.owner_generation,
        update.object_id,
        update.value,
    )


def decode_object_set_value(payload) -> ObjectSetValue:
    raw = _payload(payload, _OBJECT_SET_VALUE.size, "OBJECT_SET_VALUE")
    try:
        return ObjectSetValue(*_OBJECT_SET_VALUE.unpack(raw))
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_object_set_visibility(update: ObjectSetVisibility) -> bytes:
    if not isinstance(update, ObjectSetVisibility):
        raise TypeError("update must be ObjectSetVisibility")
    return _OBJECT_SET_VISIBILITY.pack(
        update.owner_id,
        update.owner_generation,
        update.object_id,
        int(update.visible),
        bytes(7),
    )


def decode_object_set_visibility(payload) -> ObjectSetVisibility:
    raw = _payload(payload, _OBJECT_SET_VISIBILITY.size, "OBJECT_SET_VISIBILITY")
    owner_id, generation, object_id, visible, reserved = _OBJECT_SET_VISIBILITY.unpack(raw)
    if reserved != bytes(7):
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "OBJECT_SET_VISIBILITY padding is nonzero",
        )
    if visible not in (0, 1):
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            "OBJECT_SET_VISIBILITY boolean is not canonical",
        )
    try:
        return ObjectSetVisibility(owner_id, generation, object_id, bool(visible))
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_object_drop(reference: RetainedItemReference) -> bytes:
    return _encode_item_reference(reference, "OBJECT_DROP")


def decode_object_drop(payload) -> RetainedItemReference:
    return _decode_item_reference(payload, "OBJECT_DROP")


def encode_control_definition(definition: ControlWireDefinition) -> bytes:
    if not isinstance(definition, ControlWireDefinition):
        raise TypeError("definition must be ControlWireDefinition")
    label = _control_text_bytes("label", definition.label)
    shortcut = _control_text_bytes("shortcut", definition.shortcut)
    label_bytes = _integer(
        "label_bytes", len(label), minimum=0, maximum=UINT32_MAX
    )
    shortcut_bytes = _integer(
        "shortcut_bytes", len(shortcut), minimum=0, maximum=UINT32_MAX
    )
    content = (
        b""
        if definition.content is None
        else encode_semantic_text_content(definition.content)
    )
    content_bytes = _integer(
        "content_bytes", len(content), minimum=0, maximum=UINT32_MAX
    )
    if definition.bounds is None:
        bounds = (0, 0, 0, 0)
    else:
        bounds = (
            definition.bounds.left,
            definition.bounds.top,
            definition.bounds.right,
            definition.bounds.bottom,
        )
    return _CONTROL_PREFIX.pack(
        definition.owner_id,
        definition.owner_generation,
        definition.control_id,
        int(definition.kind),
        int(definition.state),
        definition.z_order,
        definition.region_id,
        definition.parent_control_id,
        definition.order,
        *bounds,
        label_bytes,
        shortcut_bytes,
        content_bytes,
    ) + label + shortcut + content


def decode_control_definition(payload) -> ControlWireDefinition:
    raw = _variable_payload(payload, _CONTROL_PREFIX.size, "CONTROL definition")
    values = _CONTROL_PREFIX.unpack_from(raw)
    if not values[0] or not values[1] or not values[2] or not values[6]:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            "CONTROL owner, generation, control, and region IDs must be nonzero",
        )
    try:
        kind = ControlKind(values[3])
    except ValueError as exc:
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            f"control kind {values[3]} is not canonical",
        ) from exc
    if values[4] & ~int(CONTROL_STATE_MASK):
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "CONTROL state contains reserved bits",
        )
    label_bytes, shortcut_bytes, content_bytes = values[13:16]
    expected = _checked_add(
        "CONTROL payload bytes",
        _CONTROL_PREFIX.size,
        label_bytes,
        shortcut_bytes,
        content_bytes,
    )
    _body_size(raw, expected, "CONTROL definition")
    raw_bounds = values[9:13]
    try:
        bounds = None if raw_bounds == (0, 0, 0, 0) else ObjectBounds(*raw_bounds)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc
    text_offset = _CONTROL_PREFIX.size
    label = _wire_control_text(
        raw[text_offset : text_offset + label_bytes], "CONTROL label"
    )
    text_offset += label_bytes
    shortcut = _wire_control_text(
        raw[text_offset : text_offset + shortcut_bytes], "CONTROL shortcut"
    )
    text_offset += shortcut_bytes
    try:
        content = (
            None
            if content_bytes == 0
            else decode_semantic_text_content(
                raw[text_offset : text_offset + content_bytes]
            )
        )
    except SemanticContentError as exc:
        raise RetainedWireError(
            RetainedWireErrorCode(exc.code.value),
            exc.detail,
        ) from exc
    try:
        return ControlWireDefinition(
            owner_id=values[0],
            owner_generation=values[1],
            control_id=values[2],
            kind=kind,
            state=ControlState(values[4]),
            z_order=values[5],
            region_id=values[6],
            parent_control_id=values[7],
            order=values[8],
            bounds=bounds,
            label=label,
            shortcut=shortcut,
            content=content,
        )
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_control_replace(definition: ControlWireDefinition) -> bytes:
    return encode_control_definition(definition)


def decode_control_replace(payload) -> ControlWireDefinition:
    return decode_control_definition(payload)


def encode_control_drop(reference: RetainedItemReference) -> bytes:
    return _encode_item_reference(reference, "CONTROL_DROP")


def decode_control_drop(payload) -> RetainedItemReference:
    return _decode_item_reference(payload, "CONTROL_DROP")


def encode_control_event(event: ControlEvent) -> bytes:
    if not isinstance(event, ControlEvent):
        raise TypeError("event must be ControlEvent")
    return _CONTROL_EVENT.pack(
        event.owner_id,
        event.owner_generation,
        event.control_id,
        int(event.event_kind),
        event.modifiers,
        0,
        event.model_revision,
    )


def decode_control_event(payload) -> ControlEvent:
    raw = _payload(payload, _CONTROL_EVENT.size, "CONTROL_EVENT")
    owner_id, generation, control_id, event_kind, modifiers, reserved, revision = (
        _CONTROL_EVENT.unpack(raw)
    )
    if reserved:
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "CONTROL_EVENT reserved field is nonzero",
        )
    if modifiers & ~0x3F:
        raise RetainedWireError(
            RetainedWireErrorCode.RESERVED,
            "CONTROL_EVENT modifiers contain reserved bits",
        )
    try:
        kind = ControlEventKind(event_kind)
    except ValueError as exc:
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            f"CONTROL_EVENT kind {event_kind} is not canonical",
        ) from exc
    try:
        return ControlEvent(
            owner_id,
            generation,
            control_id,
            kind,
            modifiers,
            revision,
        )
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, str(exc)) from exc


def encode_series_definition(definition: SeriesWireDefinition) -> bytes:
    if not isinstance(definition, SeriesWireDefinition):
        raise TypeError("definition must be SeriesWireDefinition")
    return _SERIES_DEFINITION.pack(
        definition.owner_id,
        definition.owner_generation,
        definition.series_id,
        definition.history_capacity,
        int(definition.timestamp_mode),
        definition.uniform_interval_us,
    )


def decode_series_definition(payload) -> SeriesWireDefinition:
    raw = _payload(payload, _SERIES_DEFINITION.size, "SERIES_DEFINE")
    values = _SERIES_DEFINITION.unpack(raw)
    if not values[0] or not values[1] or not values[2] or not values[3]:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            "SERIES_DEFINE authority, ID, and capacity must be nonzero",
        )
    if values[4] not in (int(TimestampMode.EXPLICIT), int(TimestampMode.UNIFORM)):
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            "SERIES_DEFINE timestamp mode is not canonical",
        )
    try:
        return SeriesWireDefinition(*values)
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_series_samples(update: SeriesWireSamples) -> bytes:
    if not isinstance(update, SeriesWireSamples):
        raise TypeError("update must be SeriesWireSamples")
    count = update.sample_count
    if isinstance(update.batch, ExplicitSamples):
        result = bytearray(_SERIES_SAMPLES.size + count * _EXPLICIT_SAMPLE.size)
        _SERIES_SAMPLES.pack_into(
            result,
            0,
            update.owner_id,
            update.owner_generation,
            update.series_id,
            count,
            int(TimestampMode.EXPLICIT),
            0,
        )
        offset = _SERIES_SAMPLES.size
        for sample in update.batch.samples:
            _EXPLICIT_SAMPLE.pack_into(result, offset, sample.timestamp_us, sample.value)
            offset += _EXPLICIT_SAMPLE.size
        return bytes(result)
    result = bytearray(_SERIES_SAMPLES.size + count * _UNIFORM_SAMPLE.size)
    _SERIES_SAMPLES.pack_into(
        result,
        0,
        update.owner_id,
        update.owner_generation,
        update.series_id,
        count,
        int(TimestampMode.UNIFORM),
        update.batch.first_timestamp_us,
    )
    offset = _SERIES_SAMPLES.size
    for value in update.batch.values:
        _UNIFORM_SAMPLE.pack_into(result, offset, value)
        offset += _UNIFORM_SAMPLE.size
    return bytes(result)


def decode_series_samples(payload) -> SeriesWireSamples:
    raw = _variable_payload(payload, _SERIES_SAMPLES.size, "SERIES samples")
    owner_id, generation, series_id, count, mode, first_timestamp_us = (
        _SERIES_SAMPLES.unpack_from(raw)
    )
    if not owner_id or not generation or not series_id:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            "SERIES sample authority and ID must be nonzero",
        )
    if mode not in (int(TimestampMode.EXPLICIT), int(TimestampMode.UNIFORM)):
        raise RetainedWireError(
            RetainedWireErrorCode.ENUM,
            "SERIES sample timestamp mode is not canonical",
        )
    if count == 0:
        raise RetainedWireError(
            RetainedWireErrorCode.SCALAR,
            "SERIES sample count must be positive",
        )
    sample_size = (
        _EXPLICIT_SAMPLE.size
        if mode == int(TimestampMode.EXPLICIT)
        else _UNIFORM_SAMPLE.size
    )
    _body_size(raw, _SERIES_SAMPLES.size + count * sample_size, "SERIES samples")
    try:
        if mode == int(TimestampMode.EXPLICIT):
            if first_timestamp_us != 0:
                raise RetainedWireError(
                    RetainedWireErrorCode.CONSISTENCY,
                    "explicit SERIES first_timestamp_us must be zero",
                )
            samples = tuple(
                Sample(
                    *_EXPLICIT_SAMPLE.unpack_from(
                        raw, _SERIES_SAMPLES.size + index * sample_size
                    )
                )
                for index in range(count)
            )
            batch: SeriesWireBatch = ExplicitSamples(samples)
        else:
            values = tuple(
                _UNIFORM_SAMPLE.unpack_from(raw, _SERIES_SAMPLES.size + index * sample_size)[0]
                for index in range(count)
            )
            batch = UniformSamples(first_timestamp_us, values)
        return SeriesWireSamples(owner_id, generation, series_id, batch)
    except RetainedWireError:
        raise
    except (TypeError, ValueError) as exc:
        raise RetainedWireError(RetainedWireErrorCode.CONSISTENCY, str(exc)) from exc


def encode_series_append(update: SeriesWireSamples) -> bytes:
    return encode_series_samples(update)


def decode_series_append(payload) -> SeriesWireSamples:
    return decode_series_samples(payload)


def encode_series_replace(update: SeriesWireSamples) -> bytes:
    return encode_series_samples(update)


def decode_series_replace(payload) -> SeriesWireSamples:
    return decode_series_samples(payload)


def encode_series_drop(reference: RetainedItemReference) -> bytes:
    return _encode_item_reference(reference, "SERIES_DROP")


def decode_series_drop(payload) -> RetainedItemReference:
    return _decode_item_reference(payload, "SERIES_DROP")


__all__ = [
    "CellMode", "ControlEvent", "ControlEventKind", "ControlKind", "ControlState",
    "ControlWireDefinition", "ObjectSetValue", "ObjectSetVisibility", "ObjectWireBody",
    "ObjectWireDefinition", "OwnerDrop", "OwnerOpen", "PresentBegin", "PresentDisposition",
    "PresentRetainedMode", "PresentCommit", "RegionWireDefinition", "RetainedItemReference",
    "RET1_TAG", "RetStatus", "SeriesWireBatch", "SeriesWireDefinition", "SeriesWireSamples",
    "RetainedCaps", "RetainedFormats", "RetainedMessageType", "RetainedQuery",
    "RetainedResult", "RetainedWireError", "RetainedWireErrorCode",
    "decode_control_definition", "decode_control_drop", "decode_control_event",
    "decode_control_replace",
    "decode_object_definition", "decode_object_drop", "decode_object_replace",
    "decode_object_set_value", "decode_object_set_visibility", "decode_owner_drop",
    "decode_owner_open", "decode_present_begin", "decode_present_commit",
    "decode_region_definition", "decode_region_drop", "decode_region_replace",
    "decode_ret_caps", "decode_ret_formats", "decode_ret_query", "decode_ret_result",
    "decode_series_append", "decode_series_definition", "decode_series_drop",
    "decode_series_replace", "decode_series_samples", "encode_control_definition",
    "encode_control_drop", "encode_control_event", "encode_control_replace",
    "encode_object_definition",
    "encode_object_drop", "encode_object_replace", "encode_object_set_value",
    "encode_object_set_visibility", "encode_owner_drop", "encode_owner_open",
    "encode_present_begin", "encode_present_commit", "encode_region_definition",
    "encode_region_drop", "encode_region_replace", "encode_ret_caps", "encode_ret_formats",
    "encode_ret_query", "encode_ret_result", "encode_series_append",
    "encode_series_definition", "encode_series_drop", "encode_series_replace",
    "encode_series_samples",
]
