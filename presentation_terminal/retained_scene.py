"""Immutable RETAINED-1 scene targets and atomic definition transactions.

The scene model is renderer-neutral.  It owns active and hidden immutable
targets, validates exact owner authority and final references, accounts each
target independently against OWNER_OPEN reservations, and publishes a prepared
scene together with its one atomic owner-ledger high-water candidate.

Resource objects and uploads are intentionally not in this layer.  SoundLab
object definitions, scalar/visibility mutations, and bounded series histories
are complete semantic values; every update preserves the definition-time
checks below and publishes only through the same immutable transaction seam.
"""

from __future__ import annotations

import operator
from dataclasses import dataclass, replace
from enum import Enum, IntEnum
from types import MappingProxyType
from typing import Mapping

from .apt1 import UINT32_MAX, UINT64_MAX
from .presentation_model import (
    PresentationClock,
    PresentationGeometry,
    PresentationStateError,
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


INT32_MIN = -(1 << 31)
INT32_MAX = (1 << 31) - 1
INT64_MIN = -(1 << 63)
INT64_MAX = (1 << 63) - 1


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
    LABEL = 4
    READOUT = 5
    METER = 6
    STATUS = 7
    PLOT = 8
    WAVEFORM = 9


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

    def validate_geometry(self, geometry: PresentationGeometry) -> None:
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
class LabelBody:
    color: RGBA
    horizontal_align: int
    vertical_align: int
    text: str
    ellipsize: bool = False

    def __post_init__(self) -> None:
        if not isinstance(self.color, RGBA):
            raise TypeError("color must be RGBA")
        object.__setattr__(
            self,
            "horizontal_align",
            _integer("horizontal_align", self.horizontal_align, minimum=0, maximum=2),
        )
        object.__setattr__(
            self,
            "vertical_align",
            _integer("vertical_align", self.vertical_align, minimum=0, maximum=2),
        )
        _text_bytes("text", self.text)
        object.__setattr__(self, "ellipsize", _boolean("ellipsize", self.ellipsize))


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
            raise SceneModelError(SceneErrorCode.QUOTA, "readout exceeds label-byte bound")

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
            raise SceneModelError(SceneErrorCode.QUOTA, "readout exceeds label-byte bound")
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
    | LabelBody
    | ReadoutBody
    | MeterBody
    | StatusBody
    | PlotBody
    | WaveformBody
)


_BODY_KIND = {
    GroupBody: ObjectKind.GROUP,
    PolylineBody: ObjectKind.POLYLINE,
    LabelBody: ObjectKind.LABEL,
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
            raise TypeError("body is not a supported SoundLab object body")

    @property
    def kind(self) -> ObjectKind:
        return _BODY_KIND[type(self.body)]


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


@dataclass(frozen=True, slots=True)
class RetainedScene:
    owners: Mapping[int, OwnerScene]


@dataclass(frozen=True, slots=True)
class SceneModelState:
    revision: int
    geometry: PresentationGeometry
    active: RetainedScene
    hidden: RetainedScene | None
    hidden_kind: HiddenTargetKind | None
    requirement: RebuildRequirement | None
    retained_visible: bool
    retained_initialized: bool


@dataclass(slots=True)
class _SceneStaging:
    lease: TransactionLease
    mode: RetainedMode
    geometry: PresentationGeometry
    candidate: RetainedScene
    item_advances: list[tuple[OwnerIdentity, ItemNamespace, int]]
    staged_high_water: dict[tuple[int, ItemNamespace], int]
    operation_count: int
    rejected: bool = False
    prepared: bool = False


@dataclass(frozen=True, slots=True)
class PreparedSceneInstall:
    state: SceneModelState
    ledger: PreparedOwnerLedgerInstall
    lease: TransactionLease
    _model_token: object
    _source_state: SceneModelState
    _staging: _SceneStaging


class RetainedSceneModel:
    """Active/hidden retained targets sharing one presentation clock."""

    def __init__(
        self,
        *,
        clock: PresentationClock,
        owners: OwnerLedger,
        geometry: PresentationGeometry,
    ) -> None:
        if not isinstance(clock, PresentationClock):
            raise TypeError("clock must be PresentationClock")
        if not isinstance(owners, OwnerLedger):
            raise TypeError("owners must be OwnerLedger")
        owners.policy.validate_geometry(geometry)
        self._clock = clock
        self._owners = owners
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
    def clock(self) -> PresentationClock:
        return self._clock

    @property
    def transaction_open(self) -> bool:
        return self._staging is not None

    def begin(
        self,
        lease: TransactionLease,
        mode: RetainedMode,
        geometry: PresentationGeometry,
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
            if state.requirement is not RebuildRequirement.REPLACE:
                raise SceneModelError(SceneErrorCode.STATE, "replacement rebuild is not required")
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
        self._staging = _SceneStaging(lease, selected_mode, geometry, candidate, [], {}, 0)

    def define_region(self, region: RegionDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(region, RegionDefinition):
            self._fail(SceneErrorCode.STATE, "region must be RegionDefinition")
        self._require_owner(region.owner)
        try:
            region.validate_geometry(staging.geometry)
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        owner_scene = self._owner_scene(staging.candidate, region.owner)
        if region.region_id in owner_scene.regions:
            self._fail(SceneErrorCode.DUPLICATE_ID, "region ID already exists in target")
        self._stage_new_id(staging, region.owner, ItemNamespace.REGION, region.region_id)
        regions = dict(owner_scene.regions)
        regions[region.region_id] = region
        self._install_owner_candidate(staging, owner_scene, regions=regions)

    def replace_region(self, region: RegionDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(region, RegionDefinition):
            self._fail(SceneErrorCode.STATE, "region must be RegionDefinition")
        self._require_owner(region.owner)
        try:
            region.validate_geometry(staging.geometry)
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        owner_scene = self._owner_scene(staging.candidate, region.owner)
        if region.region_id not in owner_scene.regions:
            self._fail(SceneErrorCode.MISSING_ID, "region replacement ID is absent")
        regions = dict(owner_scene.regions)
        regions[region.region_id] = region
        self._install_owner_candidate(staging, owner_scene, regions=regions)

    def define_object(self, definition: ObjectDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, ObjectDefinition):
            self._fail(SceneErrorCode.STATE, "object must be ObjectDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_scene(staging.candidate, definition.owner)
        if definition.object_id in owner_scene.objects:
            self._fail(SceneErrorCode.DUPLICATE_ID, "object ID already exists in target")
        self._validate_object_policy(definition)
        self._validate_object_dependencies(owner_scene, definition)
        self._stage_new_id(staging, definition.owner, ItemNamespace.OBJECT, definition.object_id)
        objects = dict(owner_scene.objects)
        objects[definition.object_id] = definition
        self._install_owner_candidate(staging, owner_scene, objects=objects)

    def define_series(self, definition: SeriesDefinition) -> None:
        staging = self._require_mutable_staging()
        if not isinstance(definition, SeriesDefinition):
            self._fail(SceneErrorCode.STATE, "series must be SeriesDefinition")
        self._require_owner(definition.owner)
        owner_scene = self._owner_scene(staging.candidate, definition.owner)
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
        series = dict(owner_scene.series)
        series[definition.series_id] = definition
        self._install_owner_candidate(staging, owner_scene, series=series)

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
        owner_scene = self._owner_scene(staging.candidate, owner)
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
        objects = dict(owner_scene.objects)
        objects[normalized_id] = replace(definition, body=replacement_body)
        self._install_owner_candidate(staging, owner_scene, objects=objects)

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
        owner_scene = self._owner_scene(staging.candidate, owner)
        definition = owner_scene.objects.get(normalized_id)
        if definition is None:
            self._fail(SceneErrorCode.MISSING_ID, "object visibility target is absent")
        objects = dict(owner_scene.objects)
        objects[normalized_id] = replace(definition, visible=visible)
        self._install_owner_candidate(staging, owner_scene, objects=objects)

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
        owner_scene = self._owner_scene(staging.candidate, owner)
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
        series = dict(owner_scene.series)
        series[normalized_id] = replacement_series
        self._install_owner_candidate(staging, owner_scene, series=series)

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

        self._validate_scene(staging.candidate)
        if (
            selected is CommitDisposition.COMMIT_AND_REVEAL
            and staging.mode in (RetainedMode.LAYOUT_START, RetainedMode.LAYOUT_CONTINUE)
        ):
            for owner_scene in staging.candidate.owners.values():
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
        except PresentationStateError as exc:
            self._fail(SceneErrorCode.STATE, str(exc))
        old = self._state
        if staging.mode is RetainedMode.DELTA:
            state = replace(old, revision=revision, active=staging.candidate)
        elif selected is CommitDisposition.COMMIT_AND_REVEAL:
            state = SceneModelState(
                revision,
                staging.geometry,
                staging.candidate,
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
                hidden=staging.candidate,
                hidden_kind=kind,
            )
        prepared = PreparedSceneInstall(
            state, ledger, staging.lease, self._token, old, staging
        )
        staging.prepared = True
        return prepared

    def install_prepared(self, prepared: PreparedSceneInstall) -> ResultLease:
        self.validate_prepared(prepared)
        result = self._clock.complete_success(prepared.lease)
        self._install_prevalidated(prepared)
        return result

    def validate_prepared(self, prepared: PreparedSceneInstall) -> None:
        """Validate exact scene, ledger, lease, and revision provenance."""

        if not isinstance(prepared, PreparedSceneInstall):
            raise TypeError("prepared must be PreparedSceneInstall")
        if (
            prepared._model_token is not self._token
            or prepared._source_state is not self._state
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

    def reject(self) -> ResultLease:
        staging = self._require_staging()
        result = self._clock.complete_rejected(staging.lease)
        self._staging = None
        return result

    def abort(self) -> None:
        staging = self._require_staging()
        self._clock.abort(staging.lease)
        self._staging = None

    def require_layout(self, geometry: PresentationGeometry) -> None:
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

    def _install_owner_candidate(
        self,
        staging: _SceneStaging,
        prior: OwnerScene,
        *,
        regions: Mapping[int, RegionDefinition] | None = None,
        objects: Mapping[int, ObjectDefinition] | None = None,
        series: Mapping[int, SeriesDefinition] | None = None,
    ) -> None:
        try:
            candidate = self._make_owner_scene(
                prior.owner,
                prior.regions if regions is None else regions,
                prior.objects if objects is None else objects,
                prior.series if series is None else series,
            )
        except SceneModelError as exc:
            self._fail(exc.code, exc.detail)
        self._validate_usage(candidate)
        owners = dict(staging.candidate.owners)
        owners[prior.owner.owner_id] = candidate
        staging.candidate = RetainedScene(MappingProxyType(owners))
        staging.operation_count += 1
        if staging.operation_count > self._owners.policy.max_operations_per_transaction:
            self._fail(SceneErrorCode.QUOTA, "operation count exceeds caller policy")

    def _owner_scene(self, scene: RetainedScene, owner: OwnerIdentity) -> OwnerScene:
        current = scene.owners.get(owner.owner_id)
        if current is not None:
            if current.owner != owner:
                self._fail(SceneErrorCode.AUTHORITY, "scene owner generation mismatch")
            return current
        return self._make_owner_scene(owner, {}, {}, {})

    def _make_owner_scene(
        self,
        owner: OwnerIdentity,
        regions: Mapping[int, RegionDefinition],
        objects: Mapping[int, ObjectDefinition],
        series: Mapping[int, SeriesDefinition],
    ) -> OwnerScene:
        utf8_bytes = 0
        for definition in objects.values():
            if isinstance(definition.body, LabelBody):
                utf8_bytes = _add_usage("UTF-8 usage", utf8_bytes, len(_text_bytes("text", definition.body.text)))
            elif isinstance(definition.body, ReadoutBody):
                formatted = definition.body.formatted_bytes(self._owners.policy.max_label_bytes)
                utf8_bytes = _add_usage("UTF-8 usage", utf8_bytes, len(formatted))
        sample_slots = 0
        for definition in series.values():
            sample_slots = _add_usage("sample-slot usage", sample_slots, definition.history_capacity)
        usage = SceneUsage(len(regions), len(objects), len(series), utf8_bytes, sample_slots)
        return OwnerScene(
            owner,
            MappingProxyType(dict(regions)),
            MappingProxyType(dict(objects)),
            MappingProxyType(dict(series)),
            usage,
        )

    def _validate_usage(self, scene: OwnerScene) -> None:
        record = self._require_owner(scene.owner)
        assert isinstance(record.quotas, OwnerQuotas)
        quota = record.quotas
        usage = scene.usage
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
        if kind in (ObjectKind.GROUP, ObjectKind.POLYLINE):
            required = RetainedFeature.VECTOR
        elif kind in (ObjectKind.PLOT, ObjectKind.WAVEFORM):
            required = RetainedFeature.SERIES
        else:
            required = RetainedFeature.INSTRUMENT
        if not policy.features & required:
            self._fail(SceneErrorCode.FEATURE, f"{kind.name} feature was not advertised")
        if isinstance(definition.body, PolylineBody) and len(definition.body.points) > policy.max_path_points:
            self._fail(SceneErrorCode.QUOTA, "polyline point count exceeds advertised maximum")
        if isinstance(definition.body, LabelBody) and len(_text_bytes("text", definition.body.text)) > policy.max_label_bytes:
            self._fail(SceneErrorCode.QUOTA, "label exceeds advertised byte maximum")
        if isinstance(definition.body, ReadoutBody):
            try:
                definition.body.formatted_bytes(policy.max_label_bytes)
            except SceneModelError as exc:
                self._fail(exc.code, exc.detail)

    def _validate_object_dependencies(
        self, owner_scene: OwnerScene, definition: ObjectDefinition
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

    def _require_owner(self, owner: OwnerIdentity):
        try:
            return self._owners.require_live(owner)
        except OwnerLedgerError as exc:
            self._fail(SceneErrorCode.AUTHORITY, str(exc))

    def _validate_scene(self, scene: RetainedScene) -> None:
        for owner_scene in scene.owners.values():
            self._validate_usage(owner_scene)
            for definition in owner_scene.objects.values():
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

            # Iterative traversal avoids coupling valid nesting depth to the
            # Python call stack.  Each chain is bounded by object quota.
            for object_id in owner_scene.objects:
                seen: set[int] = set()
                current = object_id
                while current:
                    if current in seen:
                        self._fail(SceneErrorCode.GRAPH, "object parent graph contains a cycle")
                    seen.add(current)
                    current = owner_scene.objects[current].parent_object_id

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
    "ExplicitSamples",
    "GroupBody",
    "HiddenTargetKind",
    "LabelBody",
    "MeterBody",
    "ObjectBounds",
    "ObjectDefinition",
    "ObjectKind",
    "OwnerScene",
    "PlotBody",
    "Point",
    "PolylineBody",
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
    "WaveformBody",
]
