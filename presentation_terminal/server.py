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
    decode_abort,
    decode_cell_span,
    decode_commit,
    decode_cursor,
    decode_transaction_begin,
)
from .presentation_model import (
    PresentationClock,
    PresentationStateError,
    ResultLease,
    TransactionFamily,
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
class OutboundBytes:
    payload: bytes
    control: bool
    result_transaction_id: int | None = None

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


@dataclass(frozen=True, slots=True)
class CoreResult:
    ansi_bytes: bytes = b""
    outbound: tuple[OutboundBytes, ...] = ()
    views: tuple[TerminalView, ...] = ()


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
        session_id_factory: Callable[[], int] | None = None,
    ):
        if not isinstance(config, TerminalConfig):
            raise TypeError("config must be TerminalConfig")
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
        self._most_recent_wire_aborted_id = 0

    @staticmethod
    def _random_session_id() -> int:
        value = 0
        while value == 0:
            value = secrets.randbits(64)
        return value

    @property
    def state(self) -> TerminalState:
        return self._state

    @property
    def active(self) -> bool:
        return self._state is TerminalState.ACTIVE

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
        return (
            self._state is TerminalState.ACTIVE
            and model is not None
            and decoder is not None
            and clock is not None
            and decoder.buffered_bytes == 0
            and self._wire_transaction_id is None
            and clock.open_transaction is None
            and clock.outstanding_result is None
            and not model.awaiting_snapshot
            and not model.transaction_open
        )

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
        encoded = self._encode_data(
            MessageType.RESIZE,
            _RESIZE.pack(normalized_cols, normalized_rows, generation),
        )
        if encoded is None:
            return None
        self._rebase_legacy_cell_replacement_clock()
        model.select_geometry(normalized_cols, normalized_rows)
        self._config = replace(
            self._config,
            cols=normalized_cols,
            rows=normalized_rows,
        )
        self._geometry_generation = generation
        self._state = TerminalState.RESYNCING
        return encoded

    def request_soft_reset(self) -> OutboundBytes:
        """Begin one ordered presentation-epoch reset from an ACTIVE session."""

        if self._state is not TerminalState.ACTIVE:
            raise TerminalSessionError("soft reset requires an ACTIVE session")
        if self._reset_requested_epoch is not None:
            raise TerminalSessionError("a soft reset is already pending")
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
        self._state = TerminalState.RESYNCING
        return encoded

    def settle_result_delivery(self, transaction_id: int) -> ResultLease:
        """Release the BEGIN gate after its exact TX_RESULT is admitted."""

        try:
            return self._require_clock().settle_result(transaction_id)
        except (PresentationStateError, TypeError, ValueError) as exc:
            self._fatal(f"cannot settle TX_RESULT delivery: {exc}", cause=exc)

    def _feed_ansi_owned(self, raw: bytes) -> CoreResult:
        ansi = bytearray()
        outbound: list[OutboundBytes] = []
        views: list[TerminalView] = []

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
        views: list[TerminalView] = []
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
    ) -> tuple[tuple[OutboundBytes, ...], TerminalView | None]:
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

        self._charge_data(frame)
        if message_type in {MessageType.TX_BEGIN, MessageType.SNAPSHOT_BEGIN}:
            return self._accept_begin(frame, message_type), None
        if message_type is MessageType.CELL_SPAN:
            self._accept_span(frame.payload)
            return (), None
        if message_type is MessageType.CURSOR:
            self._accept_cursor(frame.payload)
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
    ) -> tuple[tuple[OutboundBytes, ...], TerminalView | None]:
        transaction_id = self._wire_transaction_id
        if transaction_id is None:
            self._fatal("commit is outside a transaction")
        clock = self._require_clock()
        lease = clock.open_transaction
        if lease is None or lease.transaction_id != transaction_id:
            self._fatal("wire transaction has no matching clock lease")
        status = self._discard_transaction_status
        view: TerminalView | None = None
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
                    try:
                        result_lease = clock.complete_success(lease)
                        view = model.install_prepared(prepared)
                    except (PresentationStateError, RuntimeError) as exc:
                        self._fatal(
                            f"cannot install committed CELL publication: {exc}",
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
        message_type: MessageType,
        payload: bytes,
        *,
        result_transaction_id: int | None = None,
    ) -> OutboundBytes:
        encoder = self._encoder
        if encoder is None:
            self._fatal("framed response requested before OPEN")
        try:
            encoded = encoder.encode(message_type, payload)
        except (OverflowError, TypeError, ValueError) as exc:
            self._fatal(f"cannot encode control frame: {exc}", cause=exc)
        return OutboundBytes(encoded, True, result_transaction_id)

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
        return model

    def _fatal(self, detail: str, *, cause: BaseException | None = None):
        self._state = TerminalState.FAILED
        error = TerminalSessionError(detail)
        if cause is None:
            raise error
        raise error from cause


__all__ = [
    "CoreResult",
    "OutboundBytes",
    "PresentationTerminalCore",
    "TerminalConfig",
    "TerminalSessionError",
    "TerminalState",
]
