"""Headless APT-1 terminal session core.

This module composes the production wire codec and CELL-1 model without
coupling either to MegaPad execution or a renderer.  Callers feed one bounded
machine publication at a time, retain the returned immutable outbound bytes
until the host ingress port accepts them, and publish returned views at their
own UI boundary.
"""

from __future__ import annotations

import operator
import secrets
import struct
from dataclasses import dataclass, replace
from enum import Enum
from typing import Callable

from .apt1 import (
    HEADER_BYTES,
    MANDATORY_CAPABILITIES,
    STRUCTURAL_MAX_PAYLOAD,
    UINT16_MAX,
    UINT32_MAX,
    UINT64_MAX,
    Frame,
    FrameEncoder,
    IncrementalFrameDecoder,
    MessageType,
    NegotiationError,
    Offer,
    OpenRequest,
    Probe,
    SessionFramingError,
    encode_offer,
    parse_negotiation,
    snapshot_wire_bytes,
)
from .cell_model import (
    CellModel,
    CellModelError,
    CellModelErrorCode,
    TerminalView,
    TransactionBegin,
    decode_abort,
    decode_cell_span,
    decode_commit,
    decode_cursor,
    decode_transaction_begin,
)
from .presentation_coordinator import (
    CompositePresentationView,
    PresentationCoordinator,
)
from .presentation_model import (
    PresentationClock,
    PresentationGeometry,
    PresentationStateError,
    ResultLease,
    TransactionFamily,
    TransactionLease,
)
from .retained_model import (
    OwnerIdentity,
    OwnerLedger,
    OwnerLedgerError,
    OwnerLedgerErrorCode,
    OwnerLedgerState,
    OwnerQuotas,
    RetainedPolicy,
)
from .retained_scene import (
    CommitDisposition,
    ObjectDefinition,
    RegionDefinition,
    RetainedMode,
    RetainedSceneModel,
    SceneModelError,
    SceneModelState,
    SeriesDefinition,
)
from .retained_wire import (
    CellMode,
    PresentBegin,
    PresentDisposition,
    PresentRetainedMode,
    RetStatus,
    RetainedCaps,
    RetainedFormats,
    RetainedMessageType,
    RetainedResult,
    RetainedWireError,
    decode_object_definition,
    decode_object_drop,
    decode_object_replace,
    decode_object_set_value,
    decode_object_set_visibility,
    decode_ret_query,
    decode_present_begin,
    decode_present_commit,
    decode_region_definition,
    decode_region_drop,
    decode_region_replace,
    decode_series_append,
    decode_series_definition,
    decode_series_drop,
    decode_series_replace,
    encode_ret_caps,
    encode_ret_formats,
    encode_ret_result,
)


_READY = struct.Struct("<IIIIIIQ")
_CREDIT = struct.Struct("<Q")
_TX_RESULT = struct.Struct("<QHHQ")
_KEY = struct.Struct("<IBBHQ")
_TEXT_PREFIX = struct.Struct("<HHQ")
_POINTER = struct.Struct("<iiHHHHhhQ")
_RESIZE = struct.Struct("<IIQ")
_FOCUS = struct.Struct("<B7sQ")
_CLOSE = struct.Struct("<H6sQ")
_CLOSE_ACK = struct.Struct("<H6s")
_SOFT_RESET_REQUEST = struct.Struct("<I4xQ")
_SOFT_RESET_ACK = struct.Struct("<IHH")
_OWNER_OPEN = struct.Struct("<QQIIIIQQQQ")
_OWNER_DROP = struct.Struct("<QQQQ")
_PRESENT_BEGIN = struct.Struct("<QQQQIIIIIIII")

_RETAINED_DISCOVERY_REPLY_BYTES = 2 * (HEADER_BYTES + 64)

_NAMED_KEY_SYMBOLS = frozenset(range(0x00110001, 0x0011000F)) | frozenset(
    range(0x00110020, 0x0011002C)
)

_NEGOTIATION_PREFIX = b"\x1b]9999;APT1;"
_NEGOTIATION_MAX_BYTES = 160
_ESC = 0x1B
_BEL = 0x07

_CONTROL_TYPES = frozenset(
    {
        MessageType.SERVER_READY,
        MessageType.CLIENT_READY,
        MessageType.CREDIT,
        MessageType.ERROR,
        MessageType.CLOSE,
        MessageType.CLOSE_ACK,
        MessageType.SOFT_RESET_REQUEST,
        MessageType.SOFT_RESET_ACK,
        MessageType.TX_RESULT,
        MessageType.TX_ABORT,
    }
)

_RETAINED_PRESENT_OPERATION_TYPES = frozenset(
    {
        RetainedMessageType.REGION_DEFINE,
        RetainedMessageType.REGION_REPLACE,
        RetainedMessageType.REGION_DROP,
        RetainedMessageType.OBJECT_DEFINE,
        RetainedMessageType.OBJECT_REPLACE,
        RetainedMessageType.OBJECT_SET_VALUE,
        RetainedMessageType.OBJECT_SET_VISIBILITY,
        RetainedMessageType.OBJECT_DROP,
        RetainedMessageType.SERIES_DEFINE,
        RetainedMessageType.SERIES_APPEND,
        RetainedMessageType.SERIES_REPLACE,
        RetainedMessageType.SERIES_DROP,
    }
)


class TerminalState(str, Enum):
    ANSI = "ANSI"
    PROBING = "PROBING"
    OPENING = "OPENING"
    ACTIVE = "ACTIVE"
    RESYNCING = "RESYNCING"
    CLOSING = "CLOSING"
    FAILED = "FAILED"


class TerminalSessionError(RuntimeError):
    """Fatal enhanced-session failure requiring close or outer epoch reset."""


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


def _bytes(name: str, value) -> bytes:
    if isinstance(value, str):
        raise TypeError(f"{name} must be bytes-like, not str")
    try:
        return bytes(value)
    except (TypeError, ValueError) as exc:
        raise TypeError(f"{name} must be bytes-like") from exc


@dataclass(frozen=True, slots=True)
class TerminalConfig:
    """Caller-owned bounds and selected geometry for one attachment."""

    max_payload: int
    max_transaction_bytes: int
    terminal_receive_credit: int
    max_cells: int
    max_feed_bytes: int
    max_cols: int
    max_rows: int
    cols: int
    rows: int

    def __post_init__(self) -> None:
        max_payload = _integer(
            "max_payload",
            self.max_payload,
            minimum=1,
            maximum=STRUCTURAL_MAX_PAYLOAD,
        )
        max_transaction = _integer(
            "max_transaction_bytes",
            self.max_transaction_bytes,
            minimum=1,
            maximum=UINT32_MAX,
        )
        receive_credit = _integer(
            "terminal_receive_credit",
            self.terminal_receive_credit,
            minimum=1,
            maximum=UINT32_MAX,
        )
        max_cells = _integer(
            "max_cells", self.max_cells, minimum=1, maximum=UINT64_MAX
        )
        max_feed = _integer(
            "max_feed_bytes",
            self.max_feed_bytes,
            minimum=1,
            maximum=UINT64_MAX,
        )
        max_cols = _integer(
            "max_cols", self.max_cols, minimum=1, maximum=UINT16_MAX
        )
        max_rows = _integer(
            "max_rows", self.max_rows, minimum=1, maximum=UINT16_MAX
        )
        cols = _integer("cols", self.cols, minimum=1, maximum=UINT16_MAX)
        rows = _integer("rows", self.rows, minimum=1, maximum=UINT16_MAX)
        if cols > max_cols or rows > max_rows:
            raise ValueError("selected geometry exceeds caller-owned axis bounds")
        required_payload = max(
            _READY.size, 12 + 8 * min(max_cols, max_cells)
        )
        if max_payload < required_payload:
            raise ValueError(
                "max_payload cannot admit READY and one maximum-width CELL_SPAN"
            )
        if cols * rows > max_cells:
            raise ValueError("selected geometry exceeds caller-owned model capacity")
        snapshot_bytes = snapshot_wire_bytes(cols, rows)
        if max_transaction < snapshot_bytes:
            raise ValueError("max_transaction_bytes cannot admit the selected snapshot")
        if receive_credit < max_transaction:
            raise ValueError("terminal_receive_credit must admit one maximum transaction")
        object.__setattr__(self, "max_payload", max_payload)
        object.__setattr__(self, "max_transaction_bytes", max_transaction)
        object.__setattr__(self, "terminal_receive_credit", receive_credit)
        object.__setattr__(self, "max_cells", max_cells)
        object.__setattr__(self, "max_feed_bytes", max_feed)
        object.__setattr__(self, "max_cols", max_cols)
        object.__setattr__(self, "max_rows", max_rows)
        object.__setattr__(self, "cols", cols)
        object.__setattr__(self, "rows", rows)


@dataclass(frozen=True, slots=True)
class LifecycleResultLease:
    """Exact object-identity gate for one emitted lifecycle result.

    The token is deliberately independent of the validated RET_RESULT domain
    object.  A well-framed rejected request must echo even a semantically
    invalid zero owner/item scalar, while delivery still needs an exact object
    marker at the host admission boundary.
    """

    request_type: RetainedMessageType
    owner_id: int
    owner_generation: int
    item_id: int = 0

    def __post_init__(self) -> None:
        if isinstance(self.request_type, bool):
            raise TypeError("request_type must not be bool")
        try:
            request_type = RetainedMessageType(self.request_type)
        except (TypeError, ValueError) as exc:
            raise ValueError(
                "request_type must be a retained lifecycle type"
            ) from exc
        if request_type not in {
            RetainedMessageType.OWNER_OPEN,
            RetainedMessageType.RESOURCE_BEGIN,
            RetainedMessageType.RESOURCE_CHUNK,
            RetainedMessageType.RESOURCE_COMMIT,
            RetainedMessageType.RESOURCE_DROP,
            RetainedMessageType.RESOURCE_ABORT,
        }:
            raise ValueError("request_type must be a retained lifecycle type")
        object.__setattr__(self, "request_type", request_type)
        for name in ("owner_id", "owner_generation", "item_id"):
            object.__setattr__(
                self,
                name,
                _integer(
                    name,
                    getattr(self, name),
                    minimum=0,
                    maximum=UINT64_MAX,
                ),
            )


@dataclass(frozen=True, slots=True)
class OutboundBytes:
    payload: bytes
    control: bool
    result_transaction_id: int | None = None
    lifecycle_result: LifecycleResultLease | None = None

    def __post_init__(self) -> None:
        payload = _bytes("payload", self.payload)
        if not payload:
            raise ValueError("outbound payload must not be empty")
        object.__setattr__(self, "payload", payload)
        if not isinstance(self.control, bool):
            raise TypeError("control must be bool")
        if self.result_transaction_id is not None:
            object.__setattr__(
                self,
                "result_transaction_id",
                _integer(
                    "result_transaction_id",
                    self.result_transaction_id,
                    minimum=1,
                    maximum=UINT64_MAX,
                ),
            )
            if not self.control:
                raise ValueError("a TX_RESULT delivery marker must be control")
        if self.lifecycle_result is not None:
            if not isinstance(self.lifecycle_result, LifecycleResultLease):
                raise TypeError(
                    "lifecycle_result must be LifecycleResultLease or None"
                )
            if not self.control:
                raise ValueError("a RET_RESULT delivery marker must be control")
        if (
            self.result_transaction_id is not None
            and self.lifecycle_result is not None
        ):
            raise ValueError("an outbound record cannot settle two result gates")


PresentationView = TerminalView | CompositePresentationView


@dataclass(slots=True)
class _PresentWireState:
    transaction_id: int
    declared_transaction_bytes: int
    cell_span_count: int
    cell_count: int
    retained_operation_count: int
    cell_mode: int
    retained_mode: int
    begin: PresentBegin | None
    cell_spans_seen: int = 0
    cells_seen: int = 0
    cursor_seen: bool = False
    retained_operations_seen: int = 0


@dataclass(frozen=True, slots=True)
class CoreResult:
    ansi_bytes: bytes = b""
    outbound: tuple[OutboundBytes, ...] = ()
    views: tuple[PresentationView, ...] = ()


class _NegotiationScanner:
    """Recognize only APT's fixed private OSC prefix while ANSI owns bytes."""

    def __init__(self) -> None:
        self._candidate = bytearray()

    @property
    def pending(self) -> bool:
        return bool(self._candidate)

    def drain(self) -> bytes:
        result = bytes(self._candidate)
        self._candidate.clear()
        return result

    def push(self, byte: int) -> tuple[bytes, Probe | Offer | OpenRequest | None, bytes | None]:
        """Return ANSI bytes, a parsed record, and its exact raw bytes."""

        if not self._candidate:
            if byte != _ESC:
                return bytes((byte,)), None, None
            self._candidate.append(byte)
            return b"", None, None

        self._candidate.append(byte)
        size = len(self._candidate)
        if size <= len(_NEGOTIATION_PREFIX):
            if _NEGOTIATION_PREFIX.startswith(self._candidate):
                return b"", None, None
            return self._flush_mismatch()

        if self._candidate[-2:] == b"\x1b\\":
            raw = bytes(self._candidate)
            self._candidate.clear()
            try:
                return b"", parse_negotiation(raw), raw
            except NegotiationError:
                return raw, None, None

        if byte == _BEL or size >= _NEGOTIATION_MAX_BYTES:
            return self._flush_mismatch()
        return b"", None, None

    def _flush_mismatch(
        self,
    ) -> tuple[bytes, Probe | Offer | OpenRequest | None, bytes | None]:
        raw = bytes(self._candidate)
        self._candidate.clear()
        suffix = raw.rfind(b"\x1b", 1)
        if suffix >= 0 and _NEGOTIATION_PREFIX.startswith(raw[suffix:]):
            self._candidate.extend(raw[suffix:])
            raw = raw[:suffix]
        return raw, None, None


class PresentationTerminalCore:
    """Server side of one optional APT-1 CELL-1 terminal attachment."""

    def __init__(
        self,
        config: TerminalConfig,
        *,
        attachment_epoch: int,
        retained_policy: RetainedPolicy | None = None,
        session_id_factory: Callable[[], int] | None = None,
    ):
        """Construct one attachment core.

        ``retained_policy=None`` is the production CELL-only default.  Passing
        a policy is an internal backend-composition assertion: the composer is
        responsible for installing the mandatory retained dispatch/model
        layers before exposing that opt-in profile to a guest.
        """

        if not isinstance(config, TerminalConfig):
            raise TypeError("config must be TerminalConfig")
        if retained_policy is not None and not isinstance(
            retained_policy, RetainedPolicy
        ):
            raise TypeError("retained_policy must be RetainedPolicy or None")
        self._config = config
        self._attachment_epoch = _integer(
            "attachment_epoch",
            attachment_epoch,
            minimum=1,
            maximum=UINT64_MAX,
        )
        self._session_id_factory = session_id_factory or self._random_session_id
        self._state = TerminalState.ANSI
        self._scanner = _NegotiationScanner()
        self._nonce: int | None = None
        self._session_id: int | None = None
        self._offer: Offer | None = None
        self._open: OpenRequest | None = None
        self._decoder: IncrementalFrameDecoder | None = None
        self._encoder: FrameEncoder | None = None
        self._model: CellModel | None = None
        self._clock: PresentationClock | None = None
        self._retained_model: RetainedSceneModel | None = None
        self._coordinator: PresentationCoordinator | None = None
        self._configured_retained_policy = retained_policy
        self._retained_caps = (
            None
            if retained_policy is None
            else self._caps_from_policy(retained_policy)
        )
        self._retained_formats = (
            None
            if retained_policy is None
            else self._formats_from_policy(retained_policy)
        )
        self._session_retained_policy: RetainedPolicy | None = None
        self._owner_ledger: OwnerLedger | None = None
        self._outstanding_lifecycle_result: LifecycleResultLease | None = None
        self._reset_crossed_lifecycle_consumed = False
        self._retained_query_seen = False
        self._retained_enabled = False
        self._reset_requested_epoch: int | None = None
        self._client_data_grant = 0
        self._client_data_received = 0
        self._client_data_released = 0
        self._server_data_grant = 0
        self._server_data_sent = 0
        self._client_max_text = 0
        self._pointer_buttons = 0
        self._geometry_generation = 0
        self._wire_transaction_id: int | None = None
        self._wire_transaction_snapshot = False
        self._wire_transaction_bytes = 0
        self._discard_transaction_status: int | None = None
        self._present_wire_state: _PresentWireState | None = None
        self._most_recent_wire_aborted_id = 0

    @staticmethod
    def _random_session_id() -> int:
        value = 0
        while value == 0:
            value = secrets.randbits(64)
        return value

    @staticmethod
    def _caps_from_policy(policy: RetainedPolicy) -> RetainedCaps:
        return RetainedCaps(
            features=policy.features,
            max_owner_records=policy.max_owner_records,
            max_live_owners=policy.max_live_owners,
            max_regions=policy.max_regions,
            max_resources=policy.max_resources,
            max_objects=policy.max_objects,
            max_series=policy.max_series,
            max_operations_per_transaction=(
                policy.max_operations_per_transaction
            ),
            max_resource_chunk_bytes=policy.max_resource_chunk_bytes,
            max_retained_transaction_bytes=(
                policy.max_retained_transaction_bytes
            ),
            total_resource_bytes=policy.total_resource_bytes,
        )

    @staticmethod
    def _formats_from_policy(policy: RetainedPolicy) -> RetainedFormats:
        return RetainedFormats(
            coordinate_format=1,
            color_format=1,
            image_format=policy.image_format,
            max_image_width=policy.max_image_width,
            max_image_height=policy.max_image_height,
            max_path_points=policy.max_path_points,
            max_label_bytes=policy.max_label_bytes,
            max_samples_per_append=policy.max_samples_per_append,
            max_history_per_series=policy.max_history_per_series,
            minimum_presentation_interval_us=(
                policy.minimum_presentation_interval_us
            ),
            total_sample_slots=policy.total_sample_slots,
            total_utf8_bytes=policy.total_utf8_bytes,
        )

    def _bind_retained_policy(
        self,
        *,
        terminal_to_client_max_payload: int,
    ) -> RetainedPolicy | None:
        """Bind advertised maxima to this session's negotiated base limits.

        A caller policy is validated when it is constructed, but the peer's
        terminal-to-client maximum is not known until OPEN.  Reconstructing
        the policy from the exact advertised records makes an incompatible
        negotiation the contract's deterministic CELL-only outcome instead
        of weakening any advertised maximum.
        """

        caps = self._retained_caps
        formats = self._retained_formats
        if caps is None or formats is None:
            return None
        try:
            policy = caps.policy(
                formats,
                client_to_terminal_max_payload=self._config.max_payload,
                terminal_to_client_max_payload=(
                    terminal_to_client_max_payload
                ),
                base_max_transaction_bytes=(
                    self._config.max_transaction_bytes
                ),
            )
            policy.validate_geometry(
                PresentationGeometry(
                    self._config.cols,
                    self._config.rows,
                    self._geometry_generation,
                )
            )
        except (TypeError, ValueError):
            return None
        return policy

    @property
    def state(self) -> TerminalState:
        return self._state

    @property
    def active(self) -> bool:
        return self._state is TerminalState.ACTIVE

    @property
    def retained_configured(self) -> bool:
        """Whether the attachment caller supplied a RETAINED-1 policy."""

        return self._configured_retained_policy is not None

    @property
    def retained_enabled(self) -> bool:
        """Whether deterministic discovery succeeded in the current epoch."""

        return self._retained_enabled

    @property
    def retained_policy(self) -> RetainedPolicy | None:
        """The current session policy, exposed only after valid discovery."""

        if not self._retained_enabled:
            return None
        return self._session_retained_policy

    @property
    def owner_state(self) -> OwnerLedgerState | None:
        """Immutable exact-generation owner authority for the current epoch."""

        if not self._retained_enabled or self._owner_ledger is None:
            return None
        return self._owner_ledger.state

    @property
    def retained_state(self) -> SceneModelState | None:
        """Immutable retained scene state after successful discovery."""

        if not self._retained_enabled or self._retained_model is None:
            return None
        return self._retained_model.state

    @property
    def presentation_view(self) -> PresentationView | None:
        """The latest immutable CELL-only or composite presentation view."""

        if self._coordinator is not None:
            return self._coordinator.view
        return None if self._model is None else self._model.view

    @property
    def outstanding_lifecycle_result(self) -> LifecycleResultLease | None:
        """The exact RET_RESULT whose delivery gates later lifecycle work."""

        return self._outstanding_lifecycle_result

    @property
    def session_id(self) -> int | None:
        return self._session_id

    @property
    def view(self) -> TerminalView | None:
        if self._model is None or self._reset_requested_epoch is not None:
            return None
        return self._model.view

    @property
    def presentation_revision(self) -> int:
        """The authoritative shared CELL/retained presentation revision."""

        return 0 if self._clock is None else self._clock.revision

    @property
    def outstanding_result_transaction_id(self) -> int | None:
        clock = self._clock
        if clock is None or clock.outstanding_result is None:
            return None
        return clock.outstanding_result.transaction_id

    @property
    def max_text_bytes(self) -> int:
        """Maximum UTF-8 bytes accepted by one negotiated TEXT event."""

        return self._client_max_text

    @property
    def geometry_generation(self) -> int:
        return self._geometry_generation

    @property
    def selected_geometry(self) -> tuple[int, int]:
        return self._config.cols, self._config.rows

    @property
    def resize_ready(self) -> bool:
        model = self._model
        decoder = self._decoder
        clock = self._clock
        quiescent = (
            self._state in {TerminalState.ACTIVE, TerminalState.RESYNCING}
            and model is not None
            and decoder is not None
            and clock is not None
            and decoder.buffered_bytes == 0
            and self._wire_transaction_id is None
            and clock.open_transaction is None
            and clock.outstanding_result is None
            and self._outstanding_lifecycle_result is None
            and self._reset_requested_epoch is None
            and not model.transaction_open
        )
        if not quiescent:
            return False
        if self._retained_enabled:
            retained = self._retained_model
            return retained is not None and not retained.transaction_open
        return self._state is TerminalState.ACTIVE and not model.awaiting_snapshot

    def validate_resize(self, cols: int, rows: int) -> tuple[int, int]:
        """Validate geometry against this attachment's declared bounds."""

        normalized_cols = _integer(
            "cols", cols, minimum=1, maximum=UINT16_MAX
        )
        normalized_rows = _integer(
            "rows", rows, minimum=1, maximum=UINT16_MAX
        )
        if (
            normalized_cols > self._config.max_cols
            or normalized_rows > self._config.max_rows
        ):
            raise ValueError("new geometry exceeds caller-owned axis bounds")
        if normalized_cols * normalized_rows > self._config.max_cells:
            raise ValueError("new geometry exceeds caller-owned model capacity")
        if (
            snapshot_wire_bytes(normalized_cols, normalized_rows)
            > self._config.max_transaction_bytes
        ):
            raise ValueError("new geometry cannot fit a mandatory snapshot")
        if 12 + 8 * normalized_cols > self._config.max_payload:
            raise ValueError(
                "new geometry cannot fit one maximum-width CELL_SPAN"
            )
        if self._retained_enabled:
            policy = self._session_retained_policy
            if policy is None:
                raise TerminalSessionError("retained resize lost its bound policy")
            policy.validate_geometry(
                PresentationGeometry(
                    normalized_cols,
                    normalized_rows,
                    self._geometry_generation + 1,
                )
            )
        return normalized_cols, normalized_rows

    def select_ansi_geometry(self, cols: int, rows: int) -> None:
        """Commit an already-admitted legacy geometry before negotiation."""

        normalized_cols, normalized_rows = self.validate_resize(cols, rows)
        if self._state is not TerminalState.ANSI:
            raise TerminalSessionError(
                "legacy geometry can change only before negotiation"
            )
        self._config = replace(
            self._config,
            cols=normalized_cols,
            rows=normalized_rows,
        )

    def feed_machine(self, data) -> CoreResult:
        """Consume one bounded machine publication outside scheduler settlement."""

        raw = _bytes("data", data)
        if len(raw) > self._config.max_feed_bytes:
            raise ValueError("machine publication exceeds configured feed capacity")
        if self._state is TerminalState.FAILED:
            raise TerminalSessionError("enhanced session has already failed")
        if not raw:
            return CoreResult()

        if self._state in {
            TerminalState.OPENING,
            TerminalState.ACTIVE,
            TerminalState.RESYNCING,
            TerminalState.CLOSING,
        }:
            return self._feed_framed(raw)
        return self._feed_ansi_owned(raw)

    def cancel_probe(self) -> CoreResult:
        """Return to ANSI before OPEN; no binary session has begun."""

        if self._state not in {TerminalState.ANSI, TerminalState.PROBING}:
            raise TerminalSessionError("cannot locally cancel after the OPEN boundary")
        held = self._scanner.drain()
        self._state = TerminalState.ANSI
        self._nonce = None
        self._session_id = None
        self._offer = None
        return CoreResult(ansi_bytes=held)

    def send_key(
        self,
        key_symbol: int,
        *,
        action: int = 1,
        location: int = 0,
        modifiers: int = 0,
    ) -> OutboundBytes | None:
        """Encode one normalized key event, or return None for data backpressure."""

        self._require_active_model()
        revision = self._require_clock().revision
        symbol = _integer(
            "key_symbol", key_symbol, minimum=0, maximum=UINT32_MAX
        )
        printable_scalar = symbol <= 0x10FFFF and not 0xD800 <= symbol <= 0xDFFF
        if not printable_scalar and symbol not in _NAMED_KEY_SYMBOLS:
            raise ValueError("key_symbol is neither a Unicode scalar nor a named CELL-1 key")
        normalized_action = _integer("action", action, minimum=1, maximum=3)
        normalized_location = _integer("location", location, minimum=0, maximum=3)
        normalized_modifiers = _integer(
            "modifiers", modifiers, minimum=0, maximum=0x3F
        )
        payload = _KEY.pack(
            symbol,
            normalized_action,
            normalized_location,
            normalized_modifiers,
            revision,
        )
        return self._encode_data(MessageType.KEY, payload)

    def send_text(self, data, *, paste: bool = False) -> OutboundBytes | None:
        """Encode one bounded, well-formed UTF-8 TEXT event."""

        self._require_active_model()
        revision = self._require_clock().revision
        if isinstance(data, str):
            raise TypeError("data must be bytes-like, not str")
        try:
            raw = memoryview(data).tobytes()
        except (TypeError, ValueError) as exc:
            raise TypeError("data must be bytes-like") from exc
        if not raw:
            raise ValueError("text data must not be empty")
        try:
            raw.decode("utf-8", errors="strict")
        except UnicodeDecodeError as exc:
            raise ValueError("text data must be well-formed UTF-8") from exc
        if len(raw) > self._client_max_text:
            raise ValueError("text data exceeds the negotiated client limit")
        if not isinstance(paste, bool):
            raise TypeError("paste must be bool")
        payload = _TEXT_PREFIX.pack(int(paste), 0, revision) + raw
        return self._encode_data(MessageType.TEXT, payload)

    def send_pointer(
        self,
        x: int,
        y: int,
        *,
        buttons: int = 0,
        modifiers: int = 0,
        kind: int = 1,
        wheel_x: int = 0,
        wheel_y: int = 0,
    ) -> OutboundBytes | None:
        """Encode one cell-coordinate pointer transition."""

        self._require_active_model()
        revision = self._require_clock().revision
        normalized_x = _integer(
            "x", x, minimum=-(1 << 31), maximum=(1 << 31) - 1
        )
        normalized_y = _integer(
            "y", y, minimum=-(1 << 31), maximum=(1 << 31) - 1
        )
        normalized_buttons = _integer(
            "buttons", buttons, minimum=0, maximum=0x1F
        )
        normalized_modifiers = _integer(
            "modifiers", modifiers, minimum=0, maximum=0x3F
        )
        normalized_kind = _integer("kind", kind, minimum=1, maximum=4)
        normalized_wheel_x = _integer(
            "wheel_x", wheel_x, minimum=-(1 << 15), maximum=(1 << 15) - 1
        )
        normalized_wheel_y = _integer(
            "wheel_y", wheel_y, minimum=-(1 << 15), maximum=(1 << 15) - 1
        )
        if normalized_kind != 4 and (normalized_wheel_x or normalized_wheel_y):
            raise ValueError("wheel deltas require pointer kind 4")
        changed = self._pointer_buttons ^ normalized_buttons
        payload = _POINTER.pack(
            normalized_x,
            normalized_y,
            normalized_buttons,
            changed,
            normalized_modifiers,
            normalized_kind,
            normalized_wheel_x,
            normalized_wheel_y,
            revision,
        )
        encoded = self._encode_data(MessageType.POINTER, payload)
        if encoded is not None:
            self._pointer_buttons = normalized_buttons
        return encoded

    def send_focus(self, focused: bool) -> OutboundBytes | None:
        """Encode one normalized focus transition."""

        self._require_active_model()
        revision = self._require_clock().revision
        if not isinstance(focused, bool):
            raise TypeError("focused must be bool")
        return self._encode_data(
            MessageType.FOCUS,
            _FOCUS.pack(int(focused), bytes(7), revision),
        )

    def send_resize(self, cols: int, rows: int) -> OutboundBytes | None:
        """Encode one geometry change and require its replacement snapshot."""
        normalized_cols, normalized_rows = self.validate_resize(cols, rows)
        if (normalized_cols, normalized_rows) == self.selected_geometry:
            raise ValueError("resize geometry is unchanged")
        if not self.resize_ready:
            raise TerminalSessionError(
                "terminal resize waits for a settled transaction boundary "
                "and presentation state"
            )
        if self._wire_transaction_id is not None:
            raise TerminalSessionError(
                "terminal resize waits for the client transaction boundary"
            )
        model = self._require_active_model()
        decoder = self._decoder
        if decoder is None or decoder.buffered_bytes:
            raise TerminalSessionError(
                "terminal resize waits for a complete client frame"
            )
        model.validate_geometry(normalized_cols, normalized_rows)
        if self._geometry_generation == UINT64_MAX:
            raise TerminalSessionError("terminal geometry generation is exhausted")

        generation = self._geometry_generation + 1
        geometry = PresentationGeometry(normalized_cols, normalized_rows, generation)
        retained = self._retained_model if self._retained_enabled else None
        coordinator = self._coordinator if self._retained_enabled else None
        if retained is not None:
            policy = self._session_retained_policy
            if policy is None or coordinator is None:
                raise TerminalSessionError("retained resize lost its model authority")
            required = policy.validate_geometry(geometry)
            if required > self._client_data_grant - self._client_data_received:
                return None
        encoded = self._encode_data(
            MessageType.RESIZE,
            _RESIZE.pack(normalized_cols, normalized_rows, generation),
        )
        if encoded is None:
            return None
        if retained is None:
            self._rebase_legacy_cell_replacement_clock()
        model.select_geometry(normalized_cols, normalized_rows)
        if retained is not None:
            try:
                retained.require_layout(geometry)
                coordinator.admit_resize(geometry)
            except (PresentationStateError, SceneModelError) as exc:
                self._fatal(f"cannot install retained resize boundary: {exc}", cause=exc)
        self._config = replace(
            self._config,
            cols=normalized_cols,
            rows=normalized_rows,
        )
        self._geometry_generation = generation
        request = self._open
        assert request is not None
        if retained is None:
            self._session_retained_policy = self._bind_retained_policy(
                terminal_to_client_max_payload=request.client_max_payload,
            )
        self._state = TerminalState.RESYNCING
        return encoded

    def request_soft_reset(self) -> OutboundBytes:
        """Begin one ordered presentation-epoch reset from an ACTIVE session."""

        if self._state is not TerminalState.ACTIVE:
            raise TerminalSessionError("soft reset requires an ACTIVE session")
        if self._reset_requested_epoch is not None:
            raise TerminalSessionError("a soft reset is already pending")
        if self._outstanding_lifecycle_result is not None:
            raise TerminalSessionError("soft reset waits for RET_RESULT delivery")
        decoder = self._decoder
        if decoder is None or decoder.buffered_bytes:
            raise TerminalSessionError("soft reset waits for a complete client frame")
        clock = self._require_clock()
        if clock.outstanding_result is not None:
            raise TerminalSessionError("soft reset waits for TX_RESULT delivery")
        if clock.presentation_epoch == UINT32_MAX:
            raise TerminalSessionError("presentation epoch is exhausted")

        requested_epoch = clock.presentation_epoch + 1
        encoded = self._encode_control(
            MessageType.SOFT_RESET_REQUEST,
            _SOFT_RESET_REQUEST.pack(requested_epoch, clock.revision),
        )
        try:
            decoder.expect_epoch_transition(
                MessageType.SOFT_RESET_ACK,
                requested_epoch,
            )
        except (RuntimeError, TypeError, ValueError) as exc:
            self._fatal(f"cannot arm soft-reset epoch transition: {exc}", cause=exc)
        self._reset_requested_epoch = requested_epoch
        self._reset_crossed_lifecycle_consumed = False
        self._state = TerminalState.RESYNCING
        return encoded

    def settle_result_delivery(self, transaction_id: int) -> ResultLease:
        """Release the BEGIN gate after its exact TX_RESULT is admitted."""

        try:
            return self._require_clock().settle_result(transaction_id)
        except (PresentationStateError, TypeError, ValueError) as exc:
            self._fatal(f"cannot settle TX_RESULT delivery: {exc}", cause=exc)

    def settle_lifecycle_result_delivery(
        self,
        result: LifecycleResultLease,
    ) -> LifecycleResultLease:
        """Release the lifecycle gate after its exact RET_RESULT is admitted."""

        outstanding = self._outstanding_lifecycle_result
        if not isinstance(result, LifecycleResultLease):
            self._fatal("RET_RESULT delivery marker has the wrong type")
        if outstanding is None:
            self._fatal("no RET_RESULT delivery is outstanding")
        if result is not outstanding:
            self._fatal("RET_RESULT delivery marker is stale or foreign")
        self._outstanding_lifecycle_result = None
        return result

    def _feed_ansi_owned(self, raw: bytes) -> CoreResult:
        ansi = bytearray()
        outbound: list[OutboundBytes] = []
        views: list[PresentationView] = []

        for position, byte in enumerate(raw):
            emitted, record, record_bytes = self._scanner.push(byte)
            ansi.extend(emitted)
            if record is None:
                continue
            assert record_bytes is not None
            consumed, switched, generated = self._handle_negotiation(record)
            if consumed:
                outbound.extend(generated)
            else:
                ansi.extend(record_bytes)
            if switched:
                remainder = raw[position + 1 :]
                if remainder:
                    framed = self._feed_framed(remainder)
                    ansi.extend(framed.ansi_bytes)
                    outbound.extend(framed.outbound)
                    views.extend(framed.views)
                break

        return CoreResult(bytes(ansi), tuple(outbound), tuple(views))

    def _handle_negotiation(
        self,
        record: Probe | Offer | OpenRequest,
    ) -> tuple[bool, bool, tuple[OutboundBytes, ...]]:
        if isinstance(record, Probe):
            if self._state not in {TerminalState.ANSI, TerminalState.PROBING}:
                return False, False, ()
            if self._state is TerminalState.PROBING and record.nonce == self._nonce:
                assert self._offer is not None
                return True, False, (OutboundBytes(encode_offer(self._offer), True),)
            session_id = _integer(
                "generated session_id",
                self._session_id_factory(),
                minimum=1,
                maximum=UINT64_MAX,
            )
            offer = Offer(
                nonce=record.nonce,
                session_id=session_id,
                max_payload=self._config.max_payload,
                max_transaction=self._config.max_transaction_bytes,
                terminal_receive_credit=self._config.terminal_receive_credit,
                cols=self._config.cols,
                rows=self._config.rows,
            )
            self._nonce = record.nonce
            self._session_id = session_id
            self._offer = offer
            self._state = TerminalState.PROBING
            return True, False, (OutboundBytes(encode_offer(offer), True),)

        if not isinstance(record, OpenRequest) or self._state is not TerminalState.PROBING:
            return False, False, ()
        if (
            record.nonce != self._nonce
            or record.session_id != self._session_id
            or record.client_max_payload < _READY.size
        ):
            return False, False, ()

        assert self._session_id is not None
        self._open = record
        self._decoder = IncrementalFrameDecoder(
            self._session_id,
            max_payload=self._config.max_payload,
        )
        self._encoder = FrameEncoder(
            self._session_id,
            max_payload=record.client_max_payload,
        )
        self._model = CellModel(
            attachment_epoch=self._attachment_epoch,
            session_id=self._session_id,
            presentation_epoch=0,
            cols=self._config.cols,
            rows=self._config.rows,
            max_transaction_bytes=self._config.max_transaction_bytes,
            max_cells=self._config.max_cells,
        )
        self._clock = PresentationClock(presentation_epoch=0)
        self._session_retained_policy = self._bind_retained_policy(
            terminal_to_client_max_payload=record.client_max_payload,
        )
        self._owner_ledger = None
        self._retained_model = None
        self._coordinator = None
        self._outstanding_lifecycle_result = None
        self._reset_crossed_lifecycle_consumed = False
        self._retained_query_seen = False
        self._retained_enabled = False
        self._reset_requested_epoch = None
        self._client_data_grant = self._config.terminal_receive_credit
        self._server_data_grant = record.client_receive_credit
        self._state = TerminalState.OPENING
        ready = _READY.pack(
            1,
            self._config.max_payload,
            self._config.max_transaction_bytes,
            self._config.terminal_receive_credit,
            self._config.cols,
            self._config.rows,
            MANDATORY_CAPABILITIES,
        )
        return True, True, (self._encode_control(MessageType.SERVER_READY, ready),)

    def _feed_framed(self, raw: bytes) -> CoreResult:
        decoder = self._decoder
        if decoder is None:
            self._fatal("framed state has no decoder")
        try:
            frames = decoder.feed(raw)
        except SessionFramingError as exc:
            self._fatal(str(exc), cause=exc)
        outbound: list[OutboundBytes] = []
        views: list[PresentationView] = []
        for index, frame in enumerate(frames):
            if frame.message_type == MessageType.CLOSE:
                if index != len(frames) - 1 or decoder.buffered_bytes:
                    self._fatal("client sent bytes after CLOSE before CLOSE_ACK")
            generated, view = self._process_frame(frame)
            outbound.extend(generated)
            if view is not None:
                views.append(view)
            if self._state is TerminalState.CLOSING:
                self._complete_peer_close()
        return CoreResult(outbound=tuple(outbound), views=tuple(views))

    def _process_frame(
        self,
        frame: Frame,
    ) -> tuple[tuple[OutboundBytes, ...], PresentationView | None]:
        if self._present_wire_state is not None and frame.message_type not in {
            MessageType.TX_ABORT,
            MessageType.CELL_SPAN,
            MessageType.CURSOR,
            RetainedMessageType.PRESENT_COMMIT,
            *_RETAINED_PRESENT_OPERATION_TYPES,
        }:
            self._fatal("frame intervened inside a PRESENT transaction")

        if (
            frame.message_type == RetainedMessageType.RET_QUERY
            and self._configured_retained_policy is not None
        ):
            if self._state is TerminalState.OPENING:
                self._fatal("CLIENT_READY or CLOSE was not the first client frame")
            request = self._open
            if request is None:
                self._fatal("RET_QUERY has no negotiated OPEN bounds")
            self._session_retained_policy = self._bind_retained_policy(
                terminal_to_client_max_payload=request.client_max_payload,
            )
            self._charge_data(frame, include_in_transaction=False)
            if self._session_retained_policy is None:
                if self._retained_query_seen:
                    self._fatal("RET_QUERY was already consumed in this epoch")
                self._retained_query_seen = True
                return self._release_data(frame.complete_bytes), None
            return self._accept_retained_query(frame), None

        if frame.message_type == RetainedMessageType.PRESENT_BEGIN:
            if not self._retained_enabled:
                self._fatal("PRESENT_BEGIN arrived before retained discovery")
            self._charge_data(frame)
            return self._accept_present_begin(frame), None

        if frame.message_type in _RETAINED_PRESENT_OPERATION_TYPES:
            if not self._retained_enabled:
                self._fatal("retained mutation arrived before retained discovery")
            self._charge_data(frame)
            self._accept_retained_operation(frame)
            return (), None

        if frame.message_type == RetainedMessageType.PRESENT_COMMIT:
            if not self._retained_enabled:
                self._fatal("PRESENT_COMMIT arrived before retained discovery")
            self._charge_data(frame)
            return self._accept_present_commit(frame)

        if frame.message_type == RetainedMessageType.OWNER_OPEN:
            if not self._retained_enabled:
                self._fatal("OWNER_OPEN arrived before retained discovery")
            self._charge_data(frame, include_in_transaction=False)
            return self._accept_owner_open(frame), None

        if frame.message_type == RetainedMessageType.OWNER_DROP:
            if not self._retained_enabled:
                self._fatal("OWNER_DROP arrived before retained discovery")
            return self._accept_owner_drop(frame)

        try:
            message_type = MessageType(frame.message_type)
        except ValueError:
            if frame.optional:
                self._charge_data(frame, include_in_transaction=False)
                return self._release_data(frame.complete_bytes), None
            self._fatal(f"unsupported mandatory message type 0x{frame.message_type:04x}")

        if message_type is MessageType.CLOSE:
            return self._accept_close(frame.payload), None

        if self._state is TerminalState.OPENING:
            if message_type is not MessageType.CLIENT_READY:
                self._fatal("CLIENT_READY or CLOSE was not the first client frame")
            self._accept_client_ready(frame.payload)
            self._state = TerminalState.ACTIVE
            return (), None

        if message_type is MessageType.CREDIT:
            self._accept_credit(frame.payload)
            return (), None
        if message_type is MessageType.TX_ABORT:
            return self._accept_abort(frame.payload), None
        if message_type is MessageType.SOFT_RESET_ACK:
            return self._accept_soft_reset_ack(frame), None
        if message_type in _CONTROL_TYPES:
            self._fatal(f"unexpected client control frame {message_type.name}")

        if self._retained_enabled and message_type is MessageType.SNAPSHOT_BEGIN:
            self._fatal(
                "legacy SNAPSHOT_BEGIN is forbidden after retained discovery"
            )

        self._charge_data(frame)
        if message_type in {MessageType.TX_BEGIN, MessageType.SNAPSHOT_BEGIN}:
            return self._accept_begin(frame, message_type), None
        if message_type is MessageType.CELL_SPAN:
            if self._present_wire_state is None:
                self._accept_span(frame.payload)
            else:
                self._accept_present_span(frame.payload)
            return (), None
        if message_type is MessageType.CURSOR:
            if self._present_wire_state is None:
                self._accept_cursor(frame.payload)
            else:
                self._accept_present_cursor(frame.payload)
            return (), None
        if message_type in {MessageType.TX_COMMIT, MessageType.SNAPSHOT_COMMIT}:
            return self._accept_commit(frame, message_type)
        self._fatal(f"message {message_type.name} is not legal client presentation data")

    def _accept_close(self, payload: bytes) -> tuple[OutboundBytes, ...]:
        if self._state not in {
            TerminalState.OPENING,
            TerminalState.ACTIVE,
            TerminalState.RESYNCING,
        }:
            self._fatal("CLOSE is outside an open enhanced session")
        if len(payload) != _CLOSE.size:
            self._fatal("CLOSE payload length is not 16")
        reason, reserved, _last_revision = _CLOSE.unpack(payload)
        if reserved != bytes(6):
            self._fatal("CLOSE reserved bytes are nonzero")
        clock = self._clock
        if self._outstanding_lifecycle_result is not None or (
            clock is not None and clock.outstanding_result is not None
        ):
            self._fatal("CLOSE crossed an unsettled emitted result")

        # CLOSE is itself the synchronized retirement boundary, so a BEGIN
        # which has not emitted a result is discarded instead of preventing
        # closure.  Check exact model/clock provenance before aborting either
        # side of the shared transaction seam.
        transaction_id = self._wire_transaction_id
        model = self._model
        retained = self._retained_model
        if transaction_id is None:
            if (clock is not None and clock.open_transaction is not None) or (
                model is not None and model.transaction_open
            ) or (
                retained is not None and retained.transaction_open
            ):
                self._fatal("CLOSE found transaction authority without wire state")
        else:
            if clock is None or model is None:
                self._fatal("CLOSE cannot retire an incomplete session transaction")
            lease = clock.open_transaction
            if lease is None or lease.transaction_id != transaction_id:
                self._fatal("CLOSE wire transaction has no matching clock lease")
            if model.transaction_open:
                try:
                    model.abort(transaction_id)
                except CellModelError as exc:
                    self._fatal(
                        f"cannot discard CELL staging for CLOSE: {exc}",
                        cause=exc,
                    )
            if retained is not None and retained.transaction_open:
                try:
                    retained.abort()
                except (PresentationStateError, SceneModelError) as exc:
                    self._fatal(
                        f"cannot discard retained transaction for CLOSE: {exc}",
                        cause=exc,
                    )
            else:
                try:
                    clock.abort(lease)
                except PresentationStateError as exc:
                    self._fatal(
                        f"cannot discard transaction for CLOSE: {exc}",
                        cause=exc,
                    )
            self._clear_wire_transaction()

        acknowledgement = self._encode_control(
            MessageType.CLOSE_ACK,
            _CLOSE_ACK.pack(reason, bytes(6)),
        )
        self._state = TerminalState.CLOSING
        return (acknowledgement,)

    def _complete_peer_close(self) -> None:
        """Retire state after the complete ACK has been encoded for delivery."""

        if self._state is not TerminalState.CLOSING:
            self._fatal("close completion is outside CLOSING")
        self._state = TerminalState.ANSI
        self._scanner = _NegotiationScanner()
        self._nonce = None
        self._session_id = None
        self._offer = None
        self._open = None
        self._decoder = None
        self._encoder = None
        self._model = None
        self._clock = None
        self._session_retained_policy = None
        self._owner_ledger = None
        self._retained_model = None
        self._coordinator = None
        self._outstanding_lifecycle_result = None
        self._reset_crossed_lifecycle_consumed = False
        self._retained_query_seen = False
        self._retained_enabled = False
        self._reset_requested_epoch = None
        self._client_data_grant = 0
        self._client_data_received = 0
        self._client_data_released = 0
        self._server_data_grant = 0
        self._server_data_sent = 0
        self._client_max_text = 0
        self._pointer_buttons = 0
        self._geometry_generation = 0
        self._most_recent_wire_aborted_id = 0
        self._clear_wire_transaction()

    def _accept_client_ready(self, payload: bytes) -> None:
        if len(payload) != _READY.size:
            self._fatal("CLIENT_READY payload length is not 32")
        (
            profile,
            client_max_payload,
            reserved_a,
            client_credit,
            max_text,
            reserved_b,
            capabilities,
        ) = _READY.unpack(payload)
        request = self._open
        assert request is not None
        if (
            profile != 1
            or client_max_payload != request.client_max_payload
            or reserved_a != 0
            or client_credit != request.client_receive_credit
            or max_text == 0
            or client_max_payload < 12
            or max_text > client_max_payload - 12
            or reserved_b != 0
            or capabilities != MANDATORY_CAPABILITIES
        ):
            self._fatal("CLIENT_READY does not match OPEN or mandatory CELL-1 limits")
        self._client_max_text = max_text

    def _accept_credit(self, payload: bytes) -> None:
        if len(payload) != _CREDIT.size:
            self._fatal("CREDIT payload length is not eight")
        grant = _CREDIT.unpack(payload)[0]
        if grant < self._server_data_grant:
            self._fatal("client data-credit grant decreased")
        self._server_data_grant = grant

    def _accept_retained_query(
        self,
        frame: Frame,
    ) -> tuple[OutboundBytes, ...]:
        """Answer one valid epoch-local RETAINED-1 discovery query."""

        if self._state is not TerminalState.ACTIVE:
            self._fatal("RET_QUERY is outside ACTIVE")
        if self._retained_query_seen:
            self._fatal("RET_QUERY was already consumed in this epoch")

        model = self._require_model()
        clock = self._require_clock()
        if (
            self._reset_requested_epoch is not None
            or self._wire_transaction_id is not None
            or model.view is None
            or model.awaiting_snapshot
            or model.transaction_open
            or clock.open_transaction is not None
            or clock.outstanding_result is not None
            or clock.transaction_high_water == 0
        ):
            self._fatal(
                "RET_QUERY requires the settled initial snapshot result"
            )
        try:
            decode_ret_query(frame.payload)
        except (RetainedWireError, TypeError, ValueError) as exc:
            self._fatal(f"invalid RET_QUERY: {exc}", cause=exc)

        available = self._server_data_grant - self._server_data_sent
        if available < _RETAINED_DISCOVERY_REPLY_BYTES:
            self._fatal(
                "RET_QUERY lacks the required 208-byte reply allowance"
            )

        caps = self._retained_caps
        formats = self._retained_formats
        if caps is None or formats is None:
            self._fatal("RET_QUERY has no bound retained policy")

        session_id = self._session_id
        if session_id is None:
            self._fatal("RET_QUERY has no live session identity")
        policy = self._session_retained_policy
        if policy is None:
            self._fatal("RET_QUERY lost its bound retained policy")
        try:
            owner_ledger = OwnerLedger(
                session_id=session_id,
                presentation_epoch=clock.presentation_epoch,
                policy=policy,
            )
            geometry = PresentationGeometry(
                self._config.cols,
                self._config.rows,
                self._geometry_generation,
            )
            retained_model = RetainedSceneModel(
                clock=clock,
                owners=owner_ledger,
                geometry=geometry,
            )
            coordinator = PresentationCoordinator(
                clock=clock,
                cell_model=model,
                retained_model=retained_model,
                geometry=geometry,
            )
        except (PresentationStateError, SceneModelError, TypeError, ValueError) as exc:
            self._fatal(f"cannot initialize retained presentation: {exc}", cause=exc)

        self._retained_query_seen = True
        caps_reply = self._encode_data(
            RetainedMessageType.RET_CAPS,
            encode_ret_caps(caps),
        )
        formats_reply = self._encode_data(
            RetainedMessageType.RET_FORMATS,
            encode_ret_formats(formats),
        )
        if caps_reply is None or formats_reply is None:
            self._fatal("RET_QUERY reply allowance changed after preflight")
        covering_credit = self._release_data(frame.complete_bytes)
        if len(covering_credit) != 1:
            self._fatal("RET_QUERY did not produce one covering CREDIT")

        # All fixed payloads and both directional sequence advances have now
        # succeeded.  Publish capability only after the complete ordered
        # CAPS, FORMATS, covering-CREDIT tuple exists.
        self._owner_ledger = owner_ledger
        self._retained_model = retained_model
        self._coordinator = coordinator
        self._retained_enabled = True
        return (caps_reply, formats_reply, covering_credit[0])

    def _accept_owner_open(
        self,
        frame: Frame,
    ) -> tuple[OutboundBytes, ...]:
        """Reserve one exact owner quota set and order its RET_RESULT."""

        ledger, clock = self._require_owner_lifecycle_ready(
            "OWNER_OPEN",
            allow_crossed_reset=True,
        )
        if len(frame.payload) != _OWNER_OPEN.size:
            self._fatal(
                f"OWNER_OPEN payload length is {len(frame.payload)}, "
                f"expected {_OWNER_OPEN.size}"
            )
        (
            owner_id,
            owner_generation,
            regions,
            resources,
            objects,
            series,
            resource_bytes,
            utf8_bytes,
            sample_slots,
            reserved,
        ) = _OWNER_OPEN.unpack(frame.payload)

        prepared = None
        if owner_id == 0 or owner_generation == 0 or reserved != 0:
            status = RetStatus.INVALID
        else:
            try:
                quotas = OwnerQuotas(
                    regions,
                    resources,
                    objects,
                    series,
                    resource_bytes,
                    utf8_bytes,
                    sample_slots,
                )
                identity = self._owner_identity(owner_id, owner_generation)
                prepared = ledger.prepare_open(identity, quotas)
            except OwnerLedgerError as exc:
                status = self._owner_open_status(exc)
            except (TypeError, ValueError) as exc:
                self._fatal(f"cannot normalize OWNER_OPEN fields: {exc}", cause=exc)
            else:
                status = RetStatus.OK
                try:
                    ledger.validate_prepared(prepared)
                except (RuntimeError, TypeError) as exc:
                    self._fatal(
                        f"cannot validate OWNER_OPEN publication: {exc}",
                        cause=exc,
                    )

        result_lease = LifecycleResultLease(
            RetainedMessageType.OWNER_OPEN,
            owner_id,
            owner_generation,
        )
        result = RetainedResult(
            RetainedMessageType.OWNER_OPEN,
            status,
            owner_id,
            owner_generation,
            0,
            clock.revision,
        )
        result_record = self._encode_control(
            RetainedMessageType.RET_RESULT,
            encode_ret_result(result),
            lifecycle_result=result_lease,
        )
        covering_credit = self._release_data(frame.complete_bytes)
        if len(covering_credit) != 1:
            self._fatal("OWNER_OPEN did not produce one covering CREDIT")

        if prepared is not None:
            ledger._install_prevalidated(prepared)
        self._outstanding_lifecycle_result = result_lease
        return (result_record, covering_credit[0])

    def _accept_owner_drop(
        self,
        frame: Frame,
    ) -> tuple[tuple[OutboundBytes, ...], PresentationView | None]:
        """Apply one revisioned exact-owner drop through the shared clock."""

        _ledger, clock = self._require_owner_lifecycle_ready(
            "OWNER_DROP",
            allow_crossed_reset=True,
        )
        if len(frame.payload) != _OWNER_DROP.size:
            self._fatal(
                f"OWNER_DROP payload length is {len(frame.payload)}, "
                f"expected {_OWNER_DROP.size}"
            )
        transaction_id, base_revision, owner_id, owner_generation = (
            _OWNER_DROP.unpack(frame.payload)
        )
        identity = None
        if owner_id != 0 and owner_generation != 0:
            identity = self._owner_identity(owner_id, owner_generation)

        try:
            lease = clock.reserve(
                TransactionFamily.OWNER_DROP,
                transaction_id,
                base_revision,
            )
        except (PresentationStateError, TypeError, ValueError) as exc:
            lease = clock.open_transaction
            if (
                lease is None
                or lease.family is not TransactionFamily.OWNER_DROP
                or lease.transaction_id != transaction_id
            ):
                self._fatal(str(exc), cause=exc)
            status = (
                3
                if lease.rejection is not None
                and "base revision" in lease.rejection
                else 2
            )
            return self._complete_owner_drop_rejection(lease, status), None

        if owner_id == 0 or owner_generation == 0:
            return self._complete_owner_drop_rejection(lease, 2), None
        assert identity is not None
        try:
            retirement = self._require_retained_model().prepare_owner_retirement(
                lease,
                identity,
            )
            prepared = self._require_coordinator().prepare_owner_retirement(
                lease,
                retirement,
            )
        except OwnerLedgerError:
            return self._complete_owner_drop_rejection(lease, 2), None
        except (PresentationStateError, SceneModelError, RuntimeError, TypeError) as exc:
            self._fatal(f"cannot validate OWNER_DROP publication: {exc}", cause=exc)

        if self._reset_requested_epoch is not None:
            return self._complete_owner_drop_rejection(lease, 1), None

        revision = prepared.view.revision
        result_record = self._encode_control(
            MessageType.TX_RESULT,
            _TX_RESULT.pack(transaction_id, 0, 0, revision),
            result_transaction_id=transaction_id,
        )
        try:
            result_lease = self._require_coordinator().install_owner_retirement(
                prepared
            )
        except (PresentationStateError, RuntimeError) as exc:
            self._fatal(f"cannot complete OWNER_DROP: {exc}", cause=exc)
        if result_lease.revision != revision:
            self._fatal("OWNER_DROP revision changed after preparation")
        return (result_record,), prepared.view

    def _complete_owner_drop_rejection(
        self,
        lease: TransactionLease,
        status: int,
    ) -> tuple[OutboundBytes, ...]:
        clock = self._require_clock()
        try:
            result_lease = clock.complete_rejected(lease)
        except PresentationStateError as exc:
            self._fatal(f"cannot reject OWNER_DROP: {exc}", cause=exc)
        result_record = self._encode_control(
            MessageType.TX_RESULT,
            _TX_RESULT.pack(
                lease.transaction_id,
                status,
                0,
                result_lease.revision,
            ),
            result_transaction_id=lease.transaction_id,
        )
        return (result_record,)

    def _require_owner_lifecycle_ready(
        self,
        request_name: str,
        *,
        allow_crossed_reset: bool,
    ) -> tuple[OwnerLedger, PresentationClock]:
        pending_reset = self._reset_requested_epoch is not None
        if pending_reset:
            if (
                not allow_crossed_reset
                or self._state is not TerminalState.RESYNCING
                or self._reset_crossed_lifecycle_consumed
            ):
                self._fatal(f"{request_name} crossed an unavailable soft reset boundary")
            self._reset_crossed_lifecycle_consumed = True
        elif self._state is not TerminalState.ACTIVE:
            self._fatal(f"{request_name} is outside ACTIVE")
        if self._outstanding_lifecycle_result is not None:
            self._fatal(f"{request_name} crossed an outstanding RET_RESULT")
        model = self._require_model()
        clock = self._require_clock()
        if (
            self._wire_transaction_id is not None
            or model.transaction_open
            or clock.open_transaction is not None
            or clock.outstanding_result is not None
        ):
            self._fatal(f"{request_name} crossed a transaction/result boundary")
        ledger = self._owner_ledger
        if ledger is None:
            self._fatal(f"{request_name} has no retained owner authority")
        return ledger, clock

    def _owner_identity(
        self,
        owner_id: int,
        owner_generation: int,
    ) -> OwnerIdentity:
        session_id = self._session_id
        if session_id is None:
            self._fatal("owner lifecycle has no live session identity")
        return OwnerIdentity(
            session_id=session_id,
            presentation_epoch=self._require_clock().presentation_epoch,
            owner_id=owner_id,
            owner_generation=owner_generation,
        )

    @staticmethod
    def _owner_open_status(error: OwnerLedgerError) -> RetStatus:
        statuses = {
            OwnerLedgerErrorCode.INVALID: RetStatus.INVALID,
            OwnerLedgerErrorCode.STALE_OWNER: RetStatus.STALE_OWNER,
            OwnerLedgerErrorCode.NO_CAPACITY: RetStatus.NO_CAPACITY,
            OwnerLedgerErrorCode.DUPLICATE_ID: RetStatus.INVALID,
        }
        return statuses[error.code]

    def _accept_present_begin(
        self,
        frame: Frame,
    ) -> tuple[OutboundBytes, ...]:
        """Open one shared CELL/retained PRESENT transaction."""

        if self._state not in {TerminalState.ACTIVE, TerminalState.RESYNCING}:
            self._fatal("PRESENT_BEGIN is outside ACTIVE/RESYNCING")
        if self._wire_transaction_id is not None:
            self._fatal("nested transaction begin")
        if self._reset_requested_epoch is not None:
            self._fatal("new PRESENT_BEGIN crossed a pending soft reset")
        if self._outstanding_lifecycle_result is not None:
            self._fatal("PRESENT_BEGIN crossed an outstanding RET_RESULT")
        if len(frame.payload) != _PRESENT_BEGIN.size:
            self._fatal(
                f"PRESENT_BEGIN payload length is {len(frame.payload)}, "
                f"expected {_PRESENT_BEGIN.size}"
            )

        raw = _PRESENT_BEGIN.unpack(frame.payload)
        (
            transaction_id,
            base_revision,
            _geometry_generation,
            declared_transaction_bytes,
            _cols,
            _rows,
            cell_span_count,
            cell_count,
            retained_operation_count,
            cell_mode,
            retained_mode,
            _reserved,
        ) = raw
        wire = _PresentWireState(
            transaction_id=transaction_id,
            declared_transaction_bytes=declared_transaction_bytes,
            cell_span_count=cell_span_count,
            cell_count=cell_count,
            retained_operation_count=retained_operation_count,
            cell_mode=cell_mode,
            retained_mode=retained_mode,
            begin=None,
        )
        self._wire_transaction_id = transaction_id
        self._wire_transaction_snapshot = False
        self._wire_transaction_bytes = frame.complete_bytes
        self._discard_transaction_status = None
        self._present_wire_state = wire

        clock = self._require_clock()
        try:
            lease = clock.reserve(
                TransactionFamily.PRESENT,
                transaction_id,
                base_revision,
            )
        except (PresentationStateError, TypeError, ValueError) as exc:
            lease = clock.open_transaction
            if (
                lease is None
                or lease.family is not TransactionFamily.PRESENT
                or lease.transaction_id != transaction_id
            ):
                self._fatal(str(exc), cause=exc)
            self._discard_transaction_status = (
                3
                if lease.rejection is not None
                and "base revision" in lease.rejection
                else 2
            )
            return ()

        try:
            begin = decode_present_begin(frame.payload)
        except RetainedWireError:
            self._discard_transaction_status = 2
            return ()
        wire.begin = begin

        if (
            self._state is TerminalState.RESYNCING
            and self._reset_requested_epoch is None
            and begin.cell_mode is not CellMode.REPLACE
        ):
            self._discard_transaction_status = 2
            return ()

        retained_state = self._require_retained_model().state
        retained_policy = self._session_retained_policy
        if retained_policy is None:
            self._fatal("PRESENT_BEGIN lost its retained policy")
        available_before_begin = self._client_data_grant - (
            self._client_data_received - frame.complete_bytes
        )
        geometry = PresentationGeometry(
            self._config.cols,
            self._config.rows,
            self._geometry_generation,
        )
        if (
            begin.declared_transaction_bytes
            > retained_policy.max_retained_transaction_bytes
            or begin.declared_transaction_bytes > self._config.max_transaction_bytes
            or begin.declared_transaction_bytes > available_before_begin
            or begin.retained_operation_count
            > retained_policy.max_operations_per_transaction
            or (begin.cols, begin.rows, begin.geometry_generation)
            != (geometry.cols, geometry.rows, geometry.generation)
            or retained_state.geometry != geometry
        ):
            self._discard_transaction_status = 2
            return ()

        model = self._require_model()
        if begin.cell_mode is not CellMode.NONE:
            try:
                model.begin_with_lease(
                    TransactionBegin(
                        begin.transaction_id,
                        begin.base_revision,
                        begin.cols,
                        begin.rows,
                        begin.cell_span_count,
                        begin.cell_count,
                    ),
                    snapshot=begin.cell_mode is CellMode.REPLACE,
                    lease=lease,
                )
            except CellModelError as exc:
                self._discard_transaction_status = self._result_status(exc)
                return ()

        if begin.retained_mode is not PresentRetainedMode.NONE:
            try:
                self._require_retained_model().begin(
                    lease,
                    RetainedMode(begin.retained_mode),
                    geometry,
                )
            except SceneModelError:
                self._discard_transaction_status = 2
        return ()

    def _accept_present_span(self, payload: bytes) -> None:
        wire = self._require_present_wire_state()
        wire.cell_spans_seen += 1
        if (
            wire.cell_mode == int(CellMode.NONE)
            or wire.cursor_seen
            or wire.retained_operations_seen
            or wire.cell_spans_seen > wire.cell_span_count
        ):
            self._discard_transaction_status = 2
        try:
            span = decode_cell_span(payload)
        except CellModelError:
            self._discard_transaction_status = 2
            return
        wire.cells_seen += span.count
        if wire.cells_seen > wire.cell_count:
            self._discard_transaction_status = 2
        begin = wire.begin
        if begin is not None and begin.cell_mode is CellMode.REPLACE:
            expected_row = wire.cell_spans_seen - 1
            if (
                span.row != expected_row
                or span.column != 0
                or span.count != begin.cols
            ):
                self._discard_transaction_status = 2
        if self._discard_transaction_status is not None:
            return
        try:
            self._require_model().stage_span(span)
        except CellModelError as exc:
            self._discard_transaction_status = self._result_status(exc)

    def _accept_present_cursor(self, payload: bytes) -> None:
        wire = self._require_present_wire_state()
        if (
            wire.cell_mode == int(CellMode.NONE)
            or wire.cursor_seen
            or wire.retained_operations_seen
            or wire.cell_spans_seen != wire.cell_span_count
        ):
            self._discard_transaction_status = 2
        wire.cursor_seen = True
        try:
            cursor = decode_cursor(payload)
        except CellModelError:
            self._discard_transaction_status = 2
            return
        if self._discard_transaction_status is not None:
            return
        try:
            self._require_model().stage_cursor(cursor)
        except CellModelError as exc:
            self._discard_transaction_status = self._result_status(exc)

    def _accept_retained_operation(self, frame: Frame) -> None:
        """Decode and stage one owner-bound non-image retained mutation."""

        wire = self._require_present_wire_state()
        if (
            wire.cell_spans_seen != wire.cell_span_count
            or (
                wire.cell_mode != int(CellMode.NONE)
                and not wire.cursor_seen
            )
        ):
            self._discard_transaction_status = 2
        wire.retained_operations_seen += 1
        if wire.retained_operations_seen > wire.retained_operation_count:
            self._discard_transaction_status = 2

        message_type = RetainedMessageType(frame.message_type)
        try:
            if message_type is RetainedMessageType.REGION_DEFINE:
                operation = decode_region_definition(frame.payload)
            elif message_type is RetainedMessageType.REGION_REPLACE:
                operation = decode_region_replace(frame.payload)
            elif message_type is RetainedMessageType.REGION_DROP:
                operation = decode_region_drop(frame.payload)
            elif message_type is RetainedMessageType.OBJECT_DEFINE:
                operation = decode_object_definition(frame.payload)
            elif message_type is RetainedMessageType.OBJECT_REPLACE:
                operation = decode_object_replace(frame.payload)
            elif message_type is RetainedMessageType.OBJECT_SET_VALUE:
                operation = decode_object_set_value(frame.payload)
            elif message_type is RetainedMessageType.OBJECT_SET_VISIBILITY:
                operation = decode_object_set_visibility(frame.payload)
            elif message_type is RetainedMessageType.OBJECT_DROP:
                operation = decode_object_drop(frame.payload)
            elif message_type is RetainedMessageType.SERIES_DEFINE:
                operation = decode_series_definition(frame.payload)
            elif message_type is RetainedMessageType.SERIES_APPEND:
                operation = decode_series_append(frame.payload)
            elif message_type is RetainedMessageType.SERIES_REPLACE:
                operation = decode_series_replace(frame.payload)
            elif message_type is RetainedMessageType.SERIES_DROP:
                operation = decode_series_drop(frame.payload)
            else:  # The caller admits only _RETAINED_PRESENT_OPERATION_TYPES.
                self._fatal("retained mutation dispatch table is incomplete")
        except RetainedWireError:
            self._discard_transaction_status = 2
            return

        begin = wire.begin
        if begin is None or begin.retained_mode is PresentRetainedMode.NONE:
            self._discard_transaction_status = 2
            return
        if self._discard_transaction_status is not None:
            return

        try:
            owner = self._owner_identity(
                operation.owner_id,
                operation.owner_generation,
            )
            model = self._require_retained_model()
            if message_type in {
                RetainedMessageType.REGION_DEFINE,
                RetainedMessageType.REGION_REPLACE,
            }:
                region = RegionDefinition(
                    owner=owner,
                    region_id=operation.region_id,
                    cell_x=operation.cell_x,
                    cell_y=operation.cell_y,
                    cell_cols=operation.cell_cols,
                    cell_rows=operation.cell_rows,
                    z_order=operation.z_order,
                    visible=operation.visible,
                    clipped=operation.clipped,
                    geometry_generation=begin.geometry_generation,
                )
                if message_type is RetainedMessageType.REGION_DEFINE:
                    model.define_region(region)
                else:
                    model.replace_region(region)
            elif message_type is RetainedMessageType.REGION_DROP:
                model.drop_region(owner, operation.item_id)
            elif message_type in {
                RetainedMessageType.OBJECT_DEFINE,
                RetainedMessageType.OBJECT_REPLACE,
            }:
                definition = ObjectDefinition(
                    owner=owner,
                    object_id=operation.object_id,
                    region_id=operation.region_id,
                    parent_object_id=operation.parent_object_id,
                    bounds=operation.bounds,
                    z_order=operation.z_order,
                    visible=operation.visible,
                    body=operation.body,
                )
                if message_type is RetainedMessageType.OBJECT_DEFINE:
                    model.define_object(definition)
                else:
                    model.replace_object(definition)
            elif message_type is RetainedMessageType.OBJECT_SET_VALUE:
                model.set_object_value(owner, operation.object_id, operation.value)
            elif message_type is RetainedMessageType.OBJECT_SET_VISIBILITY:
                model.set_object_visibility(
                    owner,
                    operation.object_id,
                    operation.visible,
                )
            elif message_type is RetainedMessageType.OBJECT_DROP:
                model.drop_object(owner, operation.item_id)
            elif message_type is RetainedMessageType.SERIES_DEFINE:
                model.define_series(
                    SeriesDefinition(
                        owner=owner,
                        series_id=operation.series_id,
                        history_capacity=operation.history_capacity,
                        timestamp_mode=operation.timestamp_mode,
                        uniform_interval_us=operation.uniform_interval_us,
                    )
                )
            elif message_type is RetainedMessageType.SERIES_APPEND:
                model.append_series(owner, operation.series_id, operation.batch)
            elif message_type is RetainedMessageType.SERIES_REPLACE:
                model.replace_series(owner, operation.series_id, operation.batch)
            elif message_type is RetainedMessageType.SERIES_DROP:
                model.drop_series(owner, operation.item_id)
        except (SceneModelError, TypeError, ValueError):
            self._discard_transaction_status = 2

    def _accept_present_commit(
        self,
        frame: Frame,
    ) -> tuple[tuple[OutboundBytes, ...], PresentationView | None]:
        wire = self._require_present_wire_state()
        clock = self._require_clock()
        lease = clock.open_transaction
        if (
            lease is None
            or lease.family is not TransactionFamily.PRESENT
            or lease.transaction_id != wire.transaction_id
        ):
            self._fatal("PRESENT wire transaction has no matching clock lease")

        status = self._discard_transaction_status
        commit = None
        try:
            commit = decode_present_commit(frame.payload)
        except RetainedWireError:
            status = 2
        if commit is not None and commit.transaction_id != wire.transaction_id:
            status = 2
        if (
            wire.begin is None
            or self._wire_transaction_bytes != wire.declared_transaction_bytes
            or wire.cell_spans_seen != wire.cell_span_count
            or wire.cells_seen != wire.cell_count
            or wire.retained_operations_seen != wire.retained_operation_count
            or (
                wire.cell_mode == int(CellMode.NONE)
                and wire.cursor_seen
            )
            or (
                wire.cell_mode != int(CellMode.NONE)
                and not wire.cursor_seen
            )
        ):
            status = 2
        if (
            commit is not None
            and wire.retained_mode == int(PresentRetainedMode.NONE)
            and commit.disposition is PresentDisposition.COMMIT_AND_REVEAL
        ):
            status = 2

        view: PresentationView | None = None
        result_lease: ResultLease | None = None
        if status is None:
            assert commit is not None
            assert wire.begin is not None
            try:
                cell_prepared = (
                    None
                    if wire.begin.cell_mode is CellMode.NONE
                    else self._require_model().prepare_publication(
                        lease,
                        global_revision=clock.next_revision(lease),
                    )
                )
                retained_prepared = (
                    None
                    if wire.begin.retained_mode is PresentRetainedMode.NONE
                    else self._require_retained_model().prepare_commit(
                        CommitDisposition(commit.disposition)
                    )
                )
                prepared = self._require_coordinator().prepare_commit(
                    lease,
                    cell=cell_prepared,
                    retained=retained_prepared,
                )
            except (CellModelError, SceneModelError):
                status = 2
            except (PresentationStateError, RuntimeError, TypeError, ValueError) as exc:
                self._fatal(f"cannot prepare PRESENT publication: {exc}", cause=exc)
            else:
                if self._reset_requested_epoch is None:
                    try:
                        result_lease = self._require_coordinator().install_prepared(
                            prepared
                        )
                    except (PresentationStateError, RuntimeError) as exc:
                        self._fatal(f"cannot install PRESENT publication: {exc}", cause=exc)
                    status = 0
                    view = prepared.view
                else:
                    result_lease = self._reject_present_transaction(lease)
                    status = 1

        if status != 0 and result_lease is None:
            result_lease = self._reject_present_transaction(lease)
        if result_lease is None:
            self._fatal("PRESENT_COMMIT did not settle its clock lease")
        result = self._encode_control(
            MessageType.TX_RESULT,
            _TX_RESULT.pack(
                wire.transaction_id,
                status,
                0,
                result_lease.revision,
            ),
            result_transaction_id=wire.transaction_id,
        )
        released = self._wire_transaction_bytes
        self._clear_wire_transaction()
        if (
            status == 0
            and self._state is TerminalState.RESYNCING
            and self._reset_requested_epoch is None
            and wire.begin is not None
            and wire.begin.cell_mode is CellMode.REPLACE
        ):
            self._state = TerminalState.ACTIVE
        return (result,) + self._release_data(released), view

    def _reject_present_transaction(self, lease: TransactionLease) -> ResultLease:
        model = self._require_model()
        if model.transaction_open:
            try:
                model.abort(lease.transaction_id)
            except CellModelError as exc:
                self._fatal(f"cannot discard rejected PRESENT CELL state: {exc}", cause=exc)
        retained = self._retained_model
        if retained is not None and retained.transaction_open:
            try:
                return retained.reject()
            except (PresentationStateError, SceneModelError) as exc:
                self._fatal(f"cannot reject PRESENT retained state: {exc}", cause=exc)
        try:
            return self._require_clock().complete_rejected(lease)
        except PresentationStateError as exc:
            self._fatal(f"cannot reject PRESENT transaction: {exc}", cause=exc)

    def _charge_data(self, frame: Frame, *, include_in_transaction: bool = True) -> None:
        complete = frame.complete_bytes
        if complete > self._client_data_grant - self._client_data_received:
            self._fatal("client exceeded cumulative terminal receive credit")
        if complete > UINT64_MAX - self._client_data_received:
            self._fatal("client cumulative sent-byte count overflowed")
        self._client_data_received += complete
        if include_in_transaction and self._wire_transaction_id is not None:
            self._wire_transaction_bytes += complete

    def _accept_begin(
        self,
        frame: Frame,
        message_type: MessageType,
    ) -> tuple[OutboundBytes, ...]:
        if self._state not in {TerminalState.ACTIVE, TerminalState.RESYNCING}:
            self._fatal("transaction begin is outside ACTIVE/RESYNCING")
        if self._wire_transaction_id is not None:
            self._fatal("nested transaction begin")
        if self._reset_requested_epoch is not None:
            self._fatal("new transaction begin crossed a pending soft reset")
        if self._outstanding_lifecycle_result is not None:
            self._fatal("transaction begin crossed an outstanding RET_RESULT")
        try:
            begin = decode_transaction_begin(frame.payload)
        except CellModelError as exc:
            self._fatal(str(exc), cause=exc)
        snapshot = message_type is MessageType.SNAPSHOT_BEGIN
        self._wire_transaction_id = begin.transaction_id
        self._wire_transaction_snapshot = snapshot
        self._wire_transaction_bytes = frame.complete_bytes
        self._discard_transaction_status = None
        clock = self._require_clock()
        try:
            lease = clock.reserve(
                TransactionFamily.CELL,
                begin.transaction_id,
                begin.base_revision,
            )
        except PresentationStateError as exc:
            lease = clock.open_transaction
            if (
                lease is None
                or lease.family is not TransactionFamily.CELL
                or lease.transaction_id != begin.transaction_id
            ):
                self._fatal(str(exc), cause=exc)
            self._discard_transaction_status = (
                2
                if begin.transaction_id <= clock.transaction_high_water
                else 3
            )
            # ``reserve`` updates the high-water before a base check.  A lease
            # rejected for its base therefore has the new ID equal to the
            # high-water, unlike an old/duplicate ID.
            if lease.rejection is not None and "base revision" in lease.rejection:
                self._discard_transaction_status = 3
            return ()
        model = self._require_model()
        try:
            model.begin_with_lease(begin, snapshot=snapshot, lease=lease)
        except CellModelError as exc:
            self._discard_transaction_status = self._result_status(exc)
        return ()

    def _accept_span(self, payload: bytes) -> None:
        if self._wire_transaction_id is None:
            self._fatal("CELL_SPAN is outside a transaction")
        if self._discard_transaction_status is not None:
            return
        try:
            self._require_model().stage_span(decode_cell_span(payload))
        except CellModelError as exc:
            self._discard_transaction_status = self._result_status(exc)

    def _accept_cursor(self, payload: bytes) -> None:
        if self._wire_transaction_id is None:
            self._fatal("CURSOR is outside a transaction")
        if self._discard_transaction_status is not None:
            return
        try:
            self._require_model().stage_cursor(decode_cursor(payload))
        except CellModelError as exc:
            self._discard_transaction_status = self._result_status(exc)

    def _accept_commit(
        self,
        frame: Frame,
        message_type: MessageType,
    ) -> tuple[tuple[OutboundBytes, ...], PresentationView | None]:
        if self._present_wire_state is not None:
            self._fatal("legacy CELL commit crossed a PRESENT transaction")
        transaction_id = self._wire_transaction_id
        if transaction_id is None:
            self._fatal("commit is outside a transaction")
        clock = self._require_clock()
        lease = clock.open_transaction
        if lease is None or lease.transaction_id != transaction_id:
            self._fatal("wire transaction has no matching clock lease")
        status = self._discard_transaction_status
        view: PresentationView | None = None
        result_lease: ResultLease | None = None
        try:
            commit_id = decode_commit(frame.payload)
        except CellModelError:
            # The known commit type is still an unambiguous transaction
            # boundary, so reject this transaction without sacrificing the
            # framed session merely because its inner payload was malformed.
            commit_id = transaction_id
            status = 2
        expected_type = (
            MessageType.SNAPSHOT_COMMIT
            if self._wire_transaction_snapshot
            else MessageType.TX_COMMIT
        )
        if commit_id != transaction_id or message_type is not expected_type:
            status = 2
        model = self._require_model()
        if status is None:
            try:
                prepared = model.prepare_publication(
                    lease,
                    global_revision=clock.next_revision(lease),
                )
            except CellModelError as exc:
                status = self._result_status(exc)
            else:
                if self._reset_requested_epoch is None:
                    coordinator = self._coordinator
                    if coordinator is None:
                        try:
                            result_lease = clock.complete_success(lease)
                            view = model.install_prepared(prepared)
                        except (PresentationStateError, RuntimeError) as exc:
                            self._fatal(
                                f"cannot install committed CELL publication: {exc}",
                                cause=exc,
                            )
                    else:
                        try:
                            composite = coordinator.prepare_commit(
                                lease,
                                cell=prepared,
                            )
                            result_lease = coordinator.install_prepared(composite)
                            view = composite.view
                        except (PresentationStateError, RuntimeError) as exc:
                            self._fatal(
                                f"cannot install composite CELL publication: {exc}",
                                cause=exc,
                            )
                    status = 0
                else:
                    # A structurally and semantically valid COMMIT that
                    # crossed an already-emitted reset is ordered but does not
                    # become visible or advance the old-epoch revision.
                    try:
                        model.abort(transaction_id)
                    except CellModelError as exc:
                        self._fatal(str(exc), cause=exc)
                    try:
                        result_lease = clock.complete_rejected(lease)
                    except PresentationStateError as exc:
                        self._fatal(str(exc), cause=exc)
                    status = 1

        if status != 0 and clock.open_transaction is lease:
            if model.transaction_open:
                try:
                    model.abort(transaction_id)
                except CellModelError as exc:
                    self._fatal(str(exc), cause=exc)
            try:
                result_lease = clock.complete_rejected(lease)
            except PresentationStateError as exc:
                self._fatal(str(exc), cause=exc)

        if status is None:
            self._fatal("transaction commit did not settle a result status")
        if result_lease is None:
            self._fatal("transaction commit did not settle its clock lease")
        result = self._encode_control(
            MessageType.TX_RESULT,
            _TX_RESULT.pack(
                transaction_id,
                status,
                0,
                result_lease.revision,
            ),
            result_transaction_id=transaction_id,
        )
        released = self._wire_transaction_bytes
        self._clear_wire_transaction()
        outputs = (result,) + self._release_data(released)
        if (
            status == 0
            and self._state is TerminalState.RESYNCING
            and self._reset_requested_epoch is None
        ):
            self._state = TerminalState.ACTIVE
        return outputs, view

    def _accept_abort(self, payload: bytes) -> tuple[OutboundBytes, ...]:
        try:
            transaction_id, _reason = decode_abort(payload)
        except CellModelError as exc:
            self._fatal(str(exc), cause=exc)
        current = self._wire_transaction_id
        if current is None:
            if transaction_id == self._most_recent_wire_aborted_id:
                return ()
            try:
                self._require_model().abort(transaction_id)
            except CellModelError as exc:
                self._fatal(str(exc), cause=exc)
            return ()
        if transaction_id != current:
            self._fatal("TX_ABORT transaction_id mismatch")
        model = self._require_model()
        if model.transaction_open:
            try:
                model.abort(transaction_id)
            except CellModelError as exc:
                self._fatal(str(exc), cause=exc)
        clock = self._require_clock()
        lease = clock.open_transaction
        if lease is None or lease.transaction_id != transaction_id:
            self._fatal("wire abort has no matching clock lease")
        retained = self._retained_model
        if retained is not None and retained.transaction_open:
            try:
                retained.abort()
            except (PresentationStateError, SceneModelError) as exc:
                self._fatal(str(exc), cause=exc)
        else:
            try:
                clock.abort(lease)
            except PresentationStateError as exc:
                self._fatal(str(exc), cause=exc)
        released = self._wire_transaction_bytes
        self._most_recent_wire_aborted_id = transaction_id
        self._clear_wire_transaction()
        return self._release_data(released)

    def _accept_soft_reset_ack(
        self,
        frame: Frame,
    ) -> tuple[OutboundBytes, ...]:
        requested_epoch = self._reset_requested_epoch
        if requested_epoch is None or self._state is not TerminalState.RESYNCING:
            self._fatal("SOFT_RESET_ACK arrived without a pending reset")
        if len(frame.payload) != _SOFT_RESET_ACK.size:
            self._fatal("SOFT_RESET_ACK payload length is not eight")
        echoed_epoch, status, reserved = _SOFT_RESET_ACK.unpack(frame.payload)
        if (
            echoed_epoch != requested_epoch
            or frame.presentation_epoch != requested_epoch
            or status != 0
            or reserved != 0
        ):
            self._fatal("SOFT_RESET_ACK does not accept the requested epoch")

        clock = self._require_clock()
        if clock.outstanding_result is not None:
            self._fatal("SOFT_RESET_ACK crossed an undelivered TX_RESULT")
        if self._outstanding_lifecycle_result is not None:
            self._fatal("SOFT_RESET_ACK crossed an undelivered RET_RESULT")
        released = 0
        if self._wire_transaction_id is not None:
            transaction_id = self._wire_transaction_id
            lease = clock.open_transaction
            if lease is None or lease.transaction_id != transaction_id:
                self._fatal("reset-crossed transaction has no matching clock lease")
            model = self._require_model()
            if model.transaction_open:
                try:
                    model.abort(transaction_id)
                except CellModelError as exc:
                    self._fatal(str(exc), cause=exc)
            retained = self._retained_model
            if retained is not None and retained.transaction_open:
                try:
                    retained.abort()
                except (PresentationStateError, SceneModelError) as exc:
                    self._fatal(str(exc), cause=exc)
            else:
                try:
                    clock.abort(lease)
                except PresentationStateError as exc:
                    self._fatal(str(exc), cause=exc)
            released = self._wire_transaction_bytes
            self._clear_wire_transaction()
        elif clock.open_transaction is not None:
            self._fatal("clock transaction exists without a wire transaction")

        model = self._require_model()
        try:
            model.soft_reset(requested_epoch)
            clock.soft_reset(requested_epoch)
        except (CellModelError, PresentationStateError) as exc:
            self._fatal(f"cannot install soft-reset epoch: {exc}", cause=exc)
        encoder = self._encoder
        if encoder is None:
            self._fatal("soft-reset acknowledgement has no response encoder")
        encoder.set_presentation_epoch(requested_epoch)
        self._reset_requested_epoch = None
        self._owner_ledger = None
        self._retained_model = None
        self._coordinator = None
        self._outstanding_lifecycle_result = None
        self._reset_crossed_lifecycle_consumed = False
        self._retained_query_seen = False
        self._retained_enabled = False
        self._pointer_buttons = 0
        self._most_recent_wire_aborted_id = 0
        return self._release_data(released)

    def _rebase_legacy_cell_replacement_clock(self) -> None:
        """Select the pre-RETAINED resize snapshot's revision-zero baseline.

        CELL-1 fallback defines a later legacy replacement snapshot as
        revision one.  The transaction-ID high-water remains epoch-wide.
        RETAINED-1 discovery disables this bridge and uses revision-preserving
        PRESENT CELL_REPLACE instead.
        """

        if self._retained_enabled:
            self._fatal(
                "legacy resize snapshot rebase is unavailable after RETAINED-1"
            )
        clock = self._require_clock()
        if clock.open_transaction is not None or clock.outstanding_result is not None:
            self._fatal("legacy resize rebase requires a settled transaction clock")
        self._clock = PresentationClock(
            presentation_epoch=clock.presentation_epoch,
            revision=0,
            transaction_high_water=clock.transaction_high_water,
        )

    def _clear_wire_transaction(self) -> None:
        self._wire_transaction_id = None
        self._wire_transaction_snapshot = False
        self._wire_transaction_bytes = 0
        self._discard_transaction_status = None
        self._present_wire_state = None

    @staticmethod
    def _result_status(error: CellModelError) -> int:
        return 3 if error.code is CellModelErrorCode.STALE_REVISION else 2

    def _release_data(self, count: int) -> tuple[OutboundBytes, ...]:
        if count == 0:
            return ()
        if count > self._client_data_received - self._client_data_released:
            self._fatal("terminal attempted to release unreceived client bytes")
        self._client_data_released += count
        grant = self._config.terminal_receive_credit + self._client_data_released
        if grant > UINT64_MAX:
            self._fatal("terminal cumulative receive-credit grant overflowed")
        self._client_data_grant = grant
        return (self._encode_control(MessageType.CREDIT, _CREDIT.pack(grant)),)

    def _encode_control(
        self,
        message_type: int,
        payload: bytes,
        *,
        result_transaction_id: int | None = None,
        lifecycle_result: LifecycleResultLease | None = None,
    ) -> OutboundBytes:
        encoder = self._encoder
        if encoder is None:
            self._fatal("framed response requested before OPEN")
        try:
            encoded = encoder.encode(message_type, payload)
        except (OverflowError, TypeError, ValueError) as exc:
            self._fatal(f"cannot encode control frame: {exc}", cause=exc)
        return OutboundBytes(
            encoded,
            True,
            result_transaction_id,
            lifecycle_result,
        )

    def _encode_data(self, message_type: MessageType, payload: bytes) -> OutboundBytes | None:
        encoder = self._encoder
        if encoder is None:
            self._fatal("data response requested before OPEN")
        complete = HEADER_BYTES + len(payload)
        if complete > self._server_data_grant - self._server_data_sent:
            return None
        if complete > UINT64_MAX - self._server_data_sent:
            self._fatal("server cumulative sent-byte count overflowed")
        try:
            encoded = encoder.encode(message_type, payload)
        except (OverflowError, TypeError, ValueError) as exc:
            self._fatal(f"cannot encode data frame: {exc}", cause=exc)
        self._server_data_sent += complete
        return OutboundBytes(encoded, False)

    def _require_model(self) -> CellModel:
        if self._model is None:
            self._fatal("enhanced session has no CELL-1 model")
        return self._model

    def _require_clock(self) -> PresentationClock:
        if self._clock is None:
            self._fatal("enhanced session has no presentation clock")
        return self._clock

    def _require_retained_model(self) -> RetainedSceneModel:
        if self._retained_model is None:
            self._fatal("retained dispatch has no scene model")
        return self._retained_model

    def _require_coordinator(self) -> PresentationCoordinator:
        if self._coordinator is None:
            self._fatal("retained dispatch has no presentation coordinator")
        return self._coordinator

    def _require_present_wire_state(self) -> _PresentWireState:
        wire = self._present_wire_state
        if wire is None or self._wire_transaction_id is None:
            self._fatal("retained mutation is outside a PRESENT transaction")
        return wire

    def _require_active_model(self) -> CellModel:
        if self._state is not TerminalState.ACTIVE:
            raise TerminalSessionError("normalized input requires an ACTIVE session")
        model = self._require_model()
        clock = self._require_clock()
        if (
            model.awaiting_snapshot
            or model.transaction_open
            or clock.open_transaction is not None
            or clock.outstanding_result is not None
        ):
            raise TerminalSessionError(
                "normalized input waits for committed model/result boundaries"
            )
        retained = self._retained_model
        if retained is not None and retained.state.hidden is not None:
            raise TerminalSessionError(
                "normalized input waits for retained hidden-target reveal"
            )
        return model

    def _fatal(self, detail: str, *, cause: BaseException | None = None):
        self._state = TerminalState.FAILED
        error = TerminalSessionError(detail)
        if cause is None:
            raise error
        raise error from cause


__all__ = [
    "CoreResult",
    "LifecycleResultLease",
    "OutboundBytes",
    "PresentationView",
    "PresentationTerminalCore",
    "TerminalConfig",
    "TerminalSessionError",
    "TerminalState",
]
