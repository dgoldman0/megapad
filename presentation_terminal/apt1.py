"""Production APT-1 negotiation and incremental frame codec.

The codec deliberately stops at the wire boundary.  It validates the exact
contract, owns only one bounded partial-frame buffer, and latches every
framing failure because binary from a damaged enhanced session must never be
reinterpreted as ANSI.
"""

from __future__ import annotations

import operator
import re
import struct
from dataclasses import dataclass
from enum import Enum, IntEnum


CONTRACT_ID = "APT-1-CELL-1-2026-08-24"
MAGIC = b"\xa5PT1"
VERSION = 1
HEADER_BYTES = 40
STRUCTURAL_MAX_PAYLOAD = 1_048_576
MANDATORY_CAPABILITIES = 0x3F
CONTROL_RESERVE_BYTES = 4_096

UINT16_MAX = (1 << 16) - 1
UINT32_MAX = (1 << 32) - 1
UINT64_MAX = (1 << 64) - 1

_HEADER_PREFIX = struct.Struct("<4sBBHHHIQQI")
_HEADER = struct.Struct("<4sBBHHHIQQII")
_ST = b"\x1b\\"
_NEGOTIATION_PREFIX = b"\x1b]9999;APT1;"
_UPPER_HEX = re.compile(br"[0-9A-F]+\Z")


class MessageType(IntEnum):
    SERVER_READY = 0x0001
    CLIENT_READY = 0x0002
    CREDIT = 0x0003
    ERROR = 0x0004
    CLOSE = 0x0005
    CLOSE_ACK = 0x0006
    SOFT_RESET_REQUEST = 0x0007
    SOFT_RESET_ACK = 0x0008
    TX_RESULT = 0x0009

    TX_BEGIN = 0x0100
    CELL_SPAN = 0x0101
    CURSOR = 0x0102
    TX_COMMIT = 0x0103
    TX_ABORT = 0x0104
    SNAPSHOT_BEGIN = 0x0110
    SNAPSHOT_COMMIT = 0x0111

    KEY = 0x0200
    TEXT = 0x0201
    POINTER = 0x0202
    RESIZE = 0x0203
    FOCUS = 0x0204


class FramingErrorCode(str, Enum):
    BAD_MAGIC = "BAD_MAGIC"
    BAD_VERSION = "BAD_VERSION"
    BAD_HEADER_SIZE = "BAD_HEADER_SIZE"
    BAD_FLAGS = "BAD_FLAGS"
    BAD_RESERVED = "BAD_RESERVED"
    PAYLOAD_TOO_LARGE = "PAYLOAD_TOO_LARGE"
    WRONG_SESSION = "WRONG_SESSION"
    SEQUENCE_GAP = "SEQUENCE_GAP"
    STALE_PRESENTATION_EPOCH = "STALE_PRESENTATION_EPOCH"
    BAD_CRC32C = "BAD_CRC32C"
    SESSION_ALREADY_FAILED = "SESSION_ALREADY_FAILED"


class SessionFramingError(ValueError):
    """Fatal APT-1 framing error for one negotiated session."""

    def __init__(self, code: FramingErrorCode, detail: str):
        self.code = code
        self.detail = detail
        super().__init__(f"{code.value}: {detail}")


class NegotiationError(ValueError):
    """A private OSC record is syntactically APT-1 but not acceptable."""


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


def _make_crc32c_table() -> tuple[int, ...]:
    table: list[int] = []
    for index in range(256):
        value = index
        for _ in range(8):
            value = (value >> 1) ^ (0x82F63B78 if value & 1 else 0)
        table.append(value)
    return tuple(table)


_CRC32C_TABLE = _make_crc32c_table()


def crc32c(data) -> int:
    """Return reflected Castagnoli CRC-32C with the APT-1 parameters."""

    payload = _bytes("data", data)
    crc = UINT32_MAX
    for byte in payload:
        crc = _CRC32C_TABLE[(crc ^ byte) & 0xFF] ^ (crc >> 8)
    return crc ^ UINT32_MAX


@dataclass(frozen=True, slots=True)
class Frame:
    """One fully validated, immutable APT-1 frame."""

    message_type: int
    session_id: int
    sequence: int
    presentation_epoch: int
    payload: bytes

    def __post_init__(self) -> None:
        object.__setattr__(
            self,
            "message_type",
            _integer("message_type", self.message_type, minimum=0, maximum=UINT16_MAX),
        )
        object.__setattr__(
            self,
            "session_id",
            _integer("session_id", self.session_id, minimum=1, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "sequence",
            _integer("sequence", self.sequence, minimum=0, maximum=UINT64_MAX),
        )
        object.__setattr__(
            self,
            "presentation_epoch",
            _integer(
                "presentation_epoch",
                self.presentation_epoch,
                minimum=0,
                maximum=UINT32_MAX,
            ),
        )
        payload = _bytes("payload", self.payload)
        if len(payload) > STRUCTURAL_MAX_PAYLOAD:
            raise ValueError("payload exceeds the APT-1 structural maximum")
        object.__setattr__(self, "payload", payload)

    @property
    def optional(self) -> bool:
        return bool(self.message_type & 0x8000)

    @property
    def complete_bytes(self) -> int:
        return HEADER_BYTES + len(self.payload)


def encode_frame(frame: Frame, *, max_payload: int = STRUCTURAL_MAX_PAYLOAD) -> bytes:
    """Encode one validated frame without maintaining sequence state."""

    if not isinstance(frame, Frame):
        raise TypeError("frame must be Frame")
    negotiated_max = _integer(
        "max_payload",
        max_payload,
        minimum=1,
        maximum=STRUCTURAL_MAX_PAYLOAD,
    )
    if len(frame.payload) > negotiated_max:
        raise ValueError("payload exceeds the negotiated maximum")
    prefix = _HEADER_PREFIX.pack(
        MAGIC,
        VERSION,
        HEADER_BYTES,
        frame.message_type,
        0,
        0,
        len(frame.payload),
        frame.session_id,
        frame.sequence,
        frame.presentation_epoch,
    )
    return prefix + struct.pack("<I", crc32c(prefix + frame.payload)) + frame.payload


class FrameEncoder:
    """Directional encoder that advances sequence only after successful encode."""

    def __init__(
        self,
        session_id: int,
        *,
        max_payload: int,
        initial_sequence: int = 0,
        presentation_epoch: int = 0,
    ):
        self._session_id = _integer(
            "session_id", session_id, minimum=1, maximum=UINT64_MAX
        )
        self._max_payload = _integer(
            "max_payload",
            max_payload,
            minimum=1,
            maximum=STRUCTURAL_MAX_PAYLOAD,
        )
        self._next_sequence: int | None = _integer(
            "initial_sequence",
            initial_sequence,
            minimum=0,
            maximum=UINT64_MAX,
        )
        self._presentation_epoch = _integer(
            "presentation_epoch",
            presentation_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )

    @property
    def next_sequence(self) -> int | None:
        return self._next_sequence

    @property
    def presentation_epoch(self) -> int:
        return self._presentation_epoch

    def set_presentation_epoch(self, epoch: int) -> None:
        self._presentation_epoch = _integer(
            "epoch", epoch, minimum=0, maximum=UINT32_MAX
        )

    def encode(self, message_type: int, payload=b"") -> bytes:
        sequence = self._next_sequence
        if sequence is None:
            raise OverflowError("directional sequence is exhausted")
        frame = Frame(
            message_type=message_type,
            session_id=self._session_id,
            sequence=sequence,
            presentation_epoch=self._presentation_epoch,
            payload=payload,
        )
        encoded = encode_frame(frame, max_payload=self._max_payload)
        self._next_sequence = None if sequence == UINT64_MAX else sequence + 1
        return encoded


class IncrementalFrameDecoder:
    """Incremental, fail-closed decoder with one bounded partial-frame buffer."""

    def __init__(
        self,
        session_id: int,
        *,
        max_payload: int,
        expected_sequence: int = 0,
        presentation_epoch: int = 0,
    ):
        self._session_id = _integer(
            "session_id", session_id, minimum=1, maximum=UINT64_MAX
        )
        self._max_payload = _integer(
            "max_payload",
            max_payload,
            minimum=1,
            maximum=STRUCTURAL_MAX_PAYLOAD,
        )
        self._expected_sequence: int | None = _integer(
            "expected_sequence",
            expected_sequence,
            minimum=0,
            maximum=UINT64_MAX,
        )
        self._presentation_epoch = _integer(
            "presentation_epoch",
            presentation_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        self._pending_epoch_transition: tuple[int, int] | None = None
        self._buffer = bytearray()
        self._frame_bytes: int | None = None
        self._failed: SessionFramingError | None = None

    @property
    def expected_sequence(self) -> int | None:
        return self._expected_sequence

    @property
    def presentation_epoch(self) -> int:
        return self._presentation_epoch

    @property
    def buffered_bytes(self) -> int:
        return len(self._buffer)

    @property
    def failed(self) -> bool:
        return self._failed is not None

    def expect_epoch_transition(self, message_type: int, requested_epoch: int) -> None:
        """Permit exactly one next frame to cross the soft-reset epoch boundary."""

        if self._failed is not None:
            self._raise_already_failed()
        type_id = _integer(
            "message_type", message_type, minimum=0, maximum=UINT16_MAX
        )
        epoch = _integer(
            "requested_epoch",
            requested_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        if self._buffer:
            raise RuntimeError("cannot alter epoch policy with a partial frame buffered")
        if self._pending_epoch_transition is not None:
            raise RuntimeError("an epoch transition is already pending")
        if self._presentation_epoch == UINT32_MAX or epoch != self._presentation_epoch + 1:
            raise ValueError("requested epoch must be current presentation epoch plus one")
        self._pending_epoch_transition = (type_id, epoch)

    def advance_presentation_epoch(self, requested_epoch: int) -> None:
        """Advance after locally accepting a peer soft-reset request.

        This is intentionally explicit: framing alone cannot decide whether a
        semantically valid reset request was accepted by the session layer.
        """

        if self._failed is not None:
            self._raise_already_failed()
        epoch = _integer(
            "requested_epoch",
            requested_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        if self._buffer:
            raise RuntimeError("cannot advance epoch with a partial frame buffered")
        if self._pending_epoch_transition is not None:
            raise RuntimeError("cannot advance epoch while a peer transition is pending")
        if self._presentation_epoch == UINT32_MAX or epoch != self._presentation_epoch + 1:
            raise ValueError("requested epoch must be current presentation epoch plus one")
        self._presentation_epoch = epoch

    def feed(self, data) -> tuple[Frame, ...]:
        if self._failed is not None:
            self._raise_already_failed()
        incoming = memoryview(_bytes("data", data))
        frames: list[Frame] = []
        position = 0

        while position < len(incoming):
            target = HEADER_BYTES if self._frame_bytes is None else self._frame_bytes
            needed = target - len(self._buffer)
            take = min(needed, len(incoming) - position)
            self._buffer.extend(incoming[position : position + take])
            position += take

            if self._frame_bytes is None and len(self._buffer) == HEADER_BYTES:
                self._frame_bytes = HEADER_BYTES + self._validate_header()
            if self._frame_bytes is not None and len(self._buffer) == self._frame_bytes:
                frames.append(self._finish_frame())

        return tuple(frames)

    def _fatal(self, code: FramingErrorCode, detail: str) -> None:
        error = SessionFramingError(code, detail)
        self._failed = error
        self._buffer.clear()
        self._frame_bytes = None
        raise error

    def _raise_already_failed(self) -> None:
        assert self._failed is not None
        raise SessionFramingError(
            FramingErrorCode.SESSION_ALREADY_FAILED,
            f"prior fatal error was {self._failed.code.value}",
        ) from self._failed

    def _validate_header(self) -> int:
        (
            magic,
            version,
            header_bytes,
            message_type,
            flags,
            reserved,
            payload_length,
            session_id,
            sequence,
            epoch,
            _checksum,
        ) = _HEADER.unpack(self._buffer)

        if magic != MAGIC:
            self._fatal(FramingErrorCode.BAD_MAGIC, f"received {magic.hex()}")
        if version != VERSION:
            self._fatal(FramingErrorCode.BAD_VERSION, f"received {version}")
        if header_bytes != HEADER_BYTES:
            self._fatal(
                FramingErrorCode.BAD_HEADER_SIZE,
                f"received {header_bytes}",
            )
        if flags:
            self._fatal(FramingErrorCode.BAD_FLAGS, f"received 0x{flags:04x}")
        if reserved:
            self._fatal(
                FramingErrorCode.BAD_RESERVED,
                f"received 0x{reserved:04x}",
            )
        if payload_length > self._max_payload:
            self._fatal(
                FramingErrorCode.PAYLOAD_TOO_LARGE,
                f"declared {payload_length}, maximum {self._max_payload}",
            )
        if session_id != self._session_id:
            self._fatal(
                FramingErrorCode.WRONG_SESSION,
                f"received 0x{session_id:016x}",
            )
        if self._expected_sequence is None or sequence != self._expected_sequence:
            self._fatal(
                FramingErrorCode.SEQUENCE_GAP,
                f"received {sequence}, expected {self._expected_sequence}",
            )

        transition = self._pending_epoch_transition
        epoch_is_current = epoch == self._presentation_epoch
        epoch_is_transition = transition == (message_type, epoch)
        if not epoch_is_current and not epoch_is_transition:
            self._fatal(
                FramingErrorCode.STALE_PRESENTATION_EPOCH,
                f"received {epoch}, expected {self._presentation_epoch}",
            )
        return payload_length

    def _finish_frame(self) -> Frame:
        raw = bytes(self._buffer)
        (
            _magic,
            _version,
            _header_bytes,
            message_type,
            _flags,
            _reserved,
            _payload_length,
            session_id,
            sequence,
            epoch,
            checksum,
        ) = _HEADER.unpack_from(raw)
        payload = raw[HEADER_BYTES:]
        expected_crc = crc32c(raw[:36] + payload)
        if checksum != expected_crc:
            self._fatal(
                FramingErrorCode.BAD_CRC32C,
                f"received 0x{checksum:08x}, expected 0x{expected_crc:08x}",
            )

        frame = Frame(message_type, session_id, sequence, epoch, payload)
        self._buffer.clear()
        self._frame_bytes = None
        self._expected_sequence = None if sequence == UINT64_MAX else sequence + 1
        if self._pending_epoch_transition == (message_type, epoch):
            self._presentation_epoch = epoch
            self._pending_epoch_transition = None
        return frame


@dataclass(frozen=True, slots=True)
class Probe:
    nonce: int


@dataclass(frozen=True, slots=True)
class Offer:
    nonce: int
    session_id: int
    max_payload: int
    max_transaction: int
    terminal_receive_credit: int
    cols: int
    rows: int


@dataclass(frozen=True, slots=True)
class OpenRequest:
    nonce: int
    session_id: int
    client_max_payload: int
    client_receive_credit: int


NegotiationRecord = Probe | Offer | OpenRequest


def _hex_field(name: str, field: bytes, width: int, *, nonzero: bool = False) -> int:
    if len(field) != width or _UPPER_HEX.fullmatch(field) is None:
        raise NegotiationError(f"{name} must be {width} uppercase hexadecimal digits")
    value = int(field, 16)
    if nonzero and value == 0:
        raise NegotiationError(f"{name} must be nonzero")
    return value


def _hex(value: int, width: int, name: str, *, nonzero: bool = False) -> bytes:
    maximum = (1 << (width * 4)) - 1
    normalized = _integer(name, value, minimum=1 if nonzero else 0, maximum=maximum)
    return f"{normalized:0{width}X}".encode("ascii")


def snapshot_wire_bytes(cols: int, rows: int) -> int:
    """Return the exact mandatory full-snapshot transaction byte budget."""

    columns = _integer("cols", cols, minimum=1, maximum=UINT32_MAX)
    lines = _integer("rows", rows, minimum=1, maximum=UINT32_MAX)
    total = 176 + lines * (52 + 8 * columns)
    if total > UINT64_MAX:
        raise OverflowError("snapshot transaction byte count exceeds uint64")
    return total


def encode_probe(nonce: int) -> bytes:
    return _NEGOTIATION_PREFIX + b"P;" + _hex(nonce, 16, "nonce", nonzero=True) + b";CELL1" + _ST


def encode_offer(offer: Offer) -> bytes:
    if not isinstance(offer, Offer):
        raise TypeError("offer must be Offer")
    _validate_offer(offer)
    fields = (
        b"O",
        _hex(offer.nonce, 16, "nonce", nonzero=True),
        _hex(offer.session_id, 16, "session_id", nonzero=True),
        _hex(offer.max_payload, 8, "max_payload"),
        _hex(offer.max_transaction, 8, "max_transaction"),
        _hex(offer.terminal_receive_credit, 8, "terminal_receive_credit"),
        _hex(offer.cols, 4, "cols"),
        _hex(offer.rows, 4, "rows"),
        b"CELL1",
    )
    return _NEGOTIATION_PREFIX + b";".join(fields) + _ST


def encode_open(request: OpenRequest) -> bytes:
    if not isinstance(request, OpenRequest):
        raise TypeError("request must be OpenRequest")
    _validate_open(request)
    fields = (
        b"A",
        _hex(request.nonce, 16, "nonce", nonzero=True),
        _hex(request.session_id, 16, "session_id", nonzero=True),
        _hex(request.client_max_payload, 8, "client_max_payload"),
        _hex(request.client_receive_credit, 8, "client_receive_credit"),
        b"CELL1",
    )
    return _NEGOTIATION_PREFIX + b";".join(fields) + _ST


def parse_negotiation(record) -> NegotiationRecord:
    """Parse one complete APT-1 private OSC record with exact field widths."""

    raw = _bytes("record", record)
    if not raw.startswith(_NEGOTIATION_PREFIX) or not raw.endswith(_ST):
        raise NegotiationError("record is not a complete APT-1 OSC 9999 string")
    body = raw[len(_NEGOTIATION_PREFIX) : -len(_ST)]
    if b"\x1b" in body or b"\x07" in body:
        raise NegotiationError("negotiation body contains an early terminator")
    fields = body.split(b";")
    if not fields:
        raise NegotiationError("negotiation record has no verb")

    if fields[0] == b"P" and len(fields) == 3 and fields[2] == b"CELL1":
        return Probe(_hex_field("nonce", fields[1], 16, nonzero=True))

    if fields[0] == b"O" and len(fields) == 9 and fields[8] == b"CELL1":
        offer = Offer(
            nonce=_hex_field("nonce", fields[1], 16, nonzero=True),
            session_id=_hex_field("session_id", fields[2], 16, nonzero=True),
            max_payload=_hex_field("max_payload", fields[3], 8),
            max_transaction=_hex_field("max_transaction", fields[4], 8),
            terminal_receive_credit=_hex_field(
                "terminal_receive_credit", fields[5], 8
            ),
            cols=_hex_field("cols", fields[6], 4),
            rows=_hex_field("rows", fields[7], 4),
        )
        _validate_offer(offer)
        return offer

    if fields[0] == b"A" and len(fields) == 6 and fields[5] == b"CELL1":
        request = OpenRequest(
            nonce=_hex_field("nonce", fields[1], 16, nonzero=True),
            session_id=_hex_field("session_id", fields[2], 16, nonzero=True),
            client_max_payload=_hex_field("client_max_payload", fields[3], 8),
            client_receive_credit=_hex_field("client_receive_credit", fields[4], 8),
        )
        _validate_open(request)
        return request

    raise NegotiationError("unknown verb, profile, or field count")


def _validate_offer(offer: Offer) -> None:
    _integer("nonce", offer.nonce, minimum=1, maximum=UINT64_MAX)
    _integer("session_id", offer.session_id, minimum=1, maximum=UINT64_MAX)
    max_payload = _integer(
        "max_payload",
        offer.max_payload,
        minimum=1,
        maximum=STRUCTURAL_MAX_PAYLOAD,
    )
    max_transaction = _integer(
        "max_transaction", offer.max_transaction, minimum=1, maximum=UINT32_MAX
    )
    credit = _integer(
        "terminal_receive_credit",
        offer.terminal_receive_credit,
        minimum=1,
        maximum=UINT32_MAX,
    )
    cols = _integer("cols", offer.cols, minimum=1, maximum=UINT16_MAX)
    rows = _integer("rows", offer.rows, minimum=1, maximum=UINT16_MAX)
    if max_payload < 12 + 8 * cols:
        raise NegotiationError("max_payload cannot admit one maximum-width CELL_SPAN")
    snapshot_bytes = snapshot_wire_bytes(cols, rows)
    if max_transaction < snapshot_bytes:
        raise NegotiationError("max_transaction cannot admit the mandatory snapshot")
    if credit < max_transaction:
        raise NegotiationError("terminal_receive_credit is smaller than max_transaction")


def _validate_open(request: OpenRequest) -> None:
    _integer("nonce", request.nonce, minimum=1, maximum=UINT64_MAX)
    _integer("session_id", request.session_id, minimum=1, maximum=UINT64_MAX)
    _integer(
        "client_max_payload",
        request.client_max_payload,
        minimum=1,
        maximum=STRUCTURAL_MAX_PAYLOAD,
    )
    _integer(
        "client_receive_credit",
        request.client_receive_credit,
        minimum=1,
        maximum=UINT32_MAX,
    )


__all__ = [
    "CONTRACT_ID",
    "CONTROL_RESERVE_BYTES",
    "Frame",
    "FrameEncoder",
    "FramingErrorCode",
    "HEADER_BYTES",
    "IncrementalFrameDecoder",
    "MAGIC",
    "MANDATORY_CAPABILITIES",
    "MessageType",
    "NegotiationError",
    "NegotiationRecord",
    "Offer",
    "OpenRequest",
    "Probe",
    "STRUCTURAL_MAX_PAYLOAD",
    "SessionFramingError",
    "VERSION",
    "crc32c",
    "encode_frame",
    "encode_offer",
    "encode_open",
    "encode_probe",
    "parse_negotiation",
    "snapshot_wire_bytes",
]
