"""Pure RETAINED-1 discovery and transaction-envelope payload codec.

This module stops at the payload boundary.  It neither owns frame ordering nor
mutates a presentation model; callers first use the base APT-1 frame codec and
then decode the exact payload type selected by the frame message ID.
"""

from __future__ import annotations

import operator
import struct
from dataclasses import dataclass
from enum import Enum, IntEnum

from .apt1 import UINT16_MAX, UINT32_MAX, UINT64_MAX
from .retained_model import OwnerQuotas, RetainedFeature, RetainedPolicy


RET1_TAG = 0x31544552

_RET_QUERY = struct.Struct("<II")
_RET_CAPS = struct.Struct("<IHHQIIIIIIIIQQ")
_RET_FORMATS = struct.Struct("<IIIIIIIIIIQQQ")
_OWNER_OPEN = struct.Struct("<QQIIIIQQQQ")
_RET_RESULT = struct.Struct("<HHIQQQQQ")
_OWNER_DROP = struct.Struct("<QQQQ")
_PRESENT_BEGIN = struct.Struct("<QQQQIIIIIIII")
_PRESENT_COMMIT = struct.Struct("<QII")


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
        if int(features) & ~0x3F:
            raise ValueError("features contain reserved RETAINED-1 bits")
        if not features & RetainedFeature.CORE:
            raise ValueError("RETAINED-1 requires CORE")
        if features & RetainedFeature.SERIES and not features & RetainedFeature.INSTRUMENT:
            raise ValueError("SERIES requires INSTRUMENT")
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
            max_label_bytes=formats.max_label_bytes,
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
    max_label_bytes: int
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
            "max_label_bytes",
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
        RET1_TAG, 1, 0, int(caps.features), caps.max_owner_records,
        caps.max_live_owners, caps.max_regions, caps.max_resources,
        caps.max_objects, caps.max_series, caps.max_operations_per_transaction,
        caps.max_resource_chunk_bytes, caps.max_retained_transaction_bytes,
        caps.total_resource_bytes,
    )


def decode_ret_caps(payload) -> RetainedCaps:
    raw = _payload(payload, _RET_CAPS.size, "RET_CAPS")
    tag, major, minor, *values = _RET_CAPS.unpack(raw)
    if tag != RET1_TAG or (major, minor) != (1, 0):
        raise RetainedWireError(RetainedWireErrorCode.SCALAR, "RET_CAPS tag/version is invalid")
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
        formats.max_label_bytes, formats.max_samples_per_append,
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


__all__ = [
    "CellMode", "OwnerDrop", "OwnerOpen", "PresentBegin", "PresentDisposition",
    "PresentRetainedMode", "PresentCommit", "RET1_TAG", "RetStatus",
    "RetainedCaps", "RetainedFormats", "RetainedMessageType", "RetainedQuery",
    "RetainedResult", "RetainedWireError", "RetainedWireErrorCode",
    "decode_owner_drop", "decode_owner_open", "decode_present_begin",
    "decode_present_commit", "decode_ret_caps", "decode_ret_formats",
    "decode_ret_query", "decode_ret_result", "encode_owner_drop",
    "encode_owner_open", "encode_present_begin", "encode_present_commit",
    "encode_ret_caps", "encode_ret_formats", "encode_ret_query",
    "encode_ret_result",
]
