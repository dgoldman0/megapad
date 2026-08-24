"""Atomic renderer-neutral CELL-1 presentation model.

The model stages one bounded transaction, validates the declared wire shape,
and publishes immutable views only after a complete commit.  It contains no
UART, parser, renderer, or Akashic-native-cell assumptions.
"""

from __future__ import annotations

import operator
import struct
from dataclasses import dataclass
from enum import Enum

from .apt1 import UINT32_MAX, UINT64_MAX, snapshot_wire_bytes


WIRE_ATTRIBUTE_MASK = 0x007F
_BEGIN = struct.Struct("<QQIIII")
_SPAN_PREFIX = struct.Struct("<III")
_CELL = struct.Struct("<IBBH")
_CURSOR = struct.Struct("<IIB7x")
_COMMIT = struct.Struct("<Q")
_ABORT = struct.Struct("<QH6x")


class CellModelErrorCode(str, Enum):
    STATE = "STATE"
    TRANSACTION = "TRANSACTION"
    STALE_REVISION = "STALE_REVISION"
    BOUNDS = "BOUNDS"
    SCALAR = "SCALAR"
    PAYLOAD = "PAYLOAD"


class CellModelError(ValueError):
    """Semantic failure that invalidates the currently staged transaction."""

    def __init__(self, code: CellModelErrorCode, detail: str):
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


def _payload(value) -> bytes:
    if isinstance(value, str):
        raise TypeError("payload must be bytes-like, not str")
    try:
        return bytes(value)
    except (TypeError, ValueError) as exc:
        raise TypeError("payload must be bytes-like") from exc


def _is_unicode_scalar(codepoint: int) -> bool:
    return codepoint <= 0x10FFFF and not 0xD800 <= codepoint <= 0xDFFF


@dataclass(frozen=True, slots=True)
class Cell:
    codepoint: int
    foreground: int
    background: int
    attributes: int = 0

    def __post_init__(self) -> None:
        codepoint = _integer(
            "codepoint", self.codepoint, minimum=0, maximum=0x10FFFF
        )
        if not _is_unicode_scalar(codepoint):
            raise ValueError("codepoint must be a Unicode scalar value")
        attributes = _integer(
            "attributes", self.attributes, minimum=0, maximum=0xFFFF
        )
        if attributes & ~WIRE_ATTRIBUTE_MASK:
            raise ValueError("attributes contain undefined CELL-1 bits")
        object.__setattr__(self, "codepoint", codepoint)
        object.__setattr__(
            self,
            "foreground",
            _integer("foreground", self.foreground, minimum=0, maximum=0xFF),
        )
        object.__setattr__(
            self,
            "background",
            _integer("background", self.background, minimum=0, maximum=0xFF),
        )
        object.__setattr__(self, "attributes", attributes)


BLANK_CELL = Cell(0x20, 7, 0, 0)


@dataclass(frozen=True, slots=True)
class Cursor:
    row: int
    column: int
    visible: bool

    def __post_init__(self) -> None:
        object.__setattr__(
            self, "row", _integer("row", self.row, minimum=0, maximum=UINT32_MAX)
        )
        object.__setattr__(
            self,
            "column",
            _integer("column", self.column, minimum=0, maximum=UINT32_MAX),
        )
        if not isinstance(self.visible, bool):
            raise TypeError("visible must be bool")


@dataclass(frozen=True, slots=True)
class DirtySpan:
    row: int
    column: int
    count: int


@dataclass(frozen=True, slots=True)
class TerminalView:
    """Immutable model revision suitable for renderer publication."""

    attachment_epoch: int
    session_id: int
    presentation_epoch: int
    revision: int
    cols: int
    rows: int
    cells: tuple[tuple[Cell, ...], ...]
    dirty_spans: tuple[DirtySpan, ...]
    cursor: Cursor


@dataclass(frozen=True, slots=True)
class TransactionBegin:
    transaction_id: int
    base_revision: int
    cols: int
    rows: int
    span_count: int
    cell_count: int


@dataclass(frozen=True, slots=True)
class CellSpan:
    row: int
    column: int
    cells: tuple[Cell, ...]

    @property
    def count(self) -> int:
        return len(self.cells)


@dataclass(slots=True)
class _Staging:
    begin: TransactionBegin
    snapshot: bool
    spans_seen: int
    cells_seen: int
    last_row: int
    last_end_column: int
    next_snapshot_cell: int
    cursor: Cursor | None
    dirty_spans: list[DirtySpan]
    changed_rows: dict[int, list[Cell]]
    snapshot_rows: list[list[Cell | None]] | None


def decode_transaction_begin(payload) -> TransactionBegin:
    raw = _payload(payload)
    if len(raw) != _BEGIN.size:
        raise CellModelError(
            CellModelErrorCode.PAYLOAD,
            f"transaction begin payload is {len(raw)} bytes, expected {_BEGIN.size}",
        )
    return TransactionBegin(*_BEGIN.unpack(raw))


def decode_cell_span(payload) -> CellSpan:
    raw = _payload(payload)
    if len(raw) < _SPAN_PREFIX.size:
        raise CellModelError(CellModelErrorCode.PAYLOAD, "CELL_SPAN prefix is truncated")
    row, column, count = _SPAN_PREFIX.unpack_from(raw)
    if count == 0:
        raise CellModelError(CellModelErrorCode.PAYLOAD, "CELL_SPAN count is zero")
    expected = _SPAN_PREFIX.size + count * _CELL.size
    if len(raw) != expected:
        raise CellModelError(
            CellModelErrorCode.PAYLOAD,
            f"CELL_SPAN payload is {len(raw)} bytes, expected {expected}",
        )
    cells: list[Cell] = []
    position = _SPAN_PREFIX.size
    for _ in range(count):
        codepoint, foreground, background, attributes = _CELL.unpack_from(raw, position)
        try:
            cells.append(Cell(codepoint, foreground, background, attributes))
        except ValueError as exc:
            code = (
                CellModelErrorCode.SCALAR
                if "scalar" in str(exc)
                else CellModelErrorCode.PAYLOAD
            )
            raise CellModelError(code, str(exc)) from exc
        position += _CELL.size
    return CellSpan(row, column, tuple(cells))


def decode_cursor(payload) -> Cursor:
    raw = _payload(payload)
    if len(raw) != _CURSOR.size:
        raise CellModelError(
            CellModelErrorCode.PAYLOAD,
            f"CURSOR payload is {len(raw)} bytes, expected {_CURSOR.size}",
        )
    row, column, visible = _CURSOR.unpack(raw)
    if any(raw[9:]):
        raise CellModelError(CellModelErrorCode.PAYLOAD, "CURSOR reserved bytes are nonzero")
    if visible not in (0, 1):
        raise CellModelError(CellModelErrorCode.PAYLOAD, "CURSOR visible is not zero or one")
    return Cursor(row, column, bool(visible))


def decode_commit(payload) -> int:
    raw = _payload(payload)
    if len(raw) != _COMMIT.size:
        raise CellModelError(
            CellModelErrorCode.PAYLOAD,
            f"commit payload is {len(raw)} bytes, expected {_COMMIT.size}",
        )
    return _COMMIT.unpack(raw)[0]


def decode_abort(payload) -> tuple[int, int]:
    raw = _payload(payload)
    if len(raw) != _ABORT.size:
        raise CellModelError(
            CellModelErrorCode.PAYLOAD,
            f"abort payload is {len(raw)} bytes, expected {_ABORT.size}",
        )
    if any(raw[10:]):
        raise CellModelError(CellModelErrorCode.PAYLOAD, "abort reserved bytes are nonzero")
    return _ABORT.unpack(raw)


class CellModel:
    """One CELL-1 session model with atomic transaction publication."""

    def __init__(
        self,
        *,
        attachment_epoch: int,
        session_id: int,
        presentation_epoch: int,
        cols: int,
        rows: int,
        max_transaction_bytes: int,
        max_cells: int,
    ):
        self._attachment_epoch = _integer(
            "attachment_epoch",
            attachment_epoch,
            minimum=1,
            maximum=UINT64_MAX,
        )
        self._session_id = _integer(
            "session_id", session_id, minimum=1, maximum=UINT64_MAX
        )
        self._presentation_epoch = _integer(
            "presentation_epoch",
            presentation_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        self._cols = _integer("cols", cols, minimum=1, maximum=UINT32_MAX)
        self._rows = _integer("rows", rows, minimum=1, maximum=UINT32_MAX)
        self._max_transaction_bytes = _integer(
            "max_transaction_bytes",
            max_transaction_bytes,
            minimum=1,
            maximum=UINT64_MAX,
        )
        self._max_cells = _integer(
            "max_cells", max_cells, minimum=1, maximum=UINT64_MAX
        )
        if self._cols * self._rows > self._max_cells:
            raise ValueError("initial geometry exceeds caller-owned model capacity")
        if snapshot_wire_bytes(self._cols, self._rows) > self._max_transaction_bytes:
            raise ValueError("max_transaction_bytes cannot admit the initial snapshot")
        self._view: TerminalView | None = None
        self._staging: _Staging | None = None
        self._last_transaction_id = 0
        self._most_recent_aborted_id = 0
        self._awaiting_snapshot = True

    @property
    def view(self) -> TerminalView | None:
        return self._view

    @property
    def revision(self) -> int:
        return 0 if self._view is None else self._view.revision

    @property
    def presentation_epoch(self) -> int:
        return self._presentation_epoch

    @property
    def awaiting_snapshot(self) -> bool:
        return self._awaiting_snapshot

    @property
    def transaction_open(self) -> bool:
        return self._staging is not None

    @property
    def geometry(self) -> tuple[int, int]:
        return self._cols, self._rows

    def begin(self, begin: TransactionBegin, *, snapshot: bool) -> None:
        if not isinstance(begin, TransactionBegin):
            raise TypeError("begin must be TransactionBegin")
        if not isinstance(snapshot, bool):
            raise TypeError("snapshot must be bool")
        if self._staging is not None:
            self._fail(CellModelErrorCode.STATE, "a transaction is already open")

        try:
            transaction_id = _integer(
                "transaction_id",
                begin.transaction_id,
                minimum=1,
                maximum=UINT64_MAX,
            )
            base_revision = _integer(
                "base_revision",
                begin.base_revision,
                minimum=0,
                maximum=UINT64_MAX,
            )
            cols = _integer("cols", begin.cols, minimum=1, maximum=UINT32_MAX)
            rows = _integer("rows", begin.rows, minimum=1, maximum=UINT32_MAX)
            span_count = _integer(
                "span_count", begin.span_count, minimum=0, maximum=UINT32_MAX
            )
            cell_count = _integer(
                "cell_count", begin.cell_count, minimum=0, maximum=UINT32_MAX
            )
        except (TypeError, ValueError) as exc:
            raise CellModelError(CellModelErrorCode.TRANSACTION, str(exc)) from exc

        if transaction_id <= self._last_transaction_id:
            raise CellModelError(
                CellModelErrorCode.TRANSACTION,
                "transaction_id is not monotonically increasing",
            )
        # Receipt consumes the epoch-scoped ID even when a later semantic
        # check rejects the declaration.  Reusing a failed ID would make the
        # peer's ordered transaction history ambiguous.
        self._last_transaction_id = transaction_id
        if (cols, rows) != (self._cols, self._rows):
            raise CellModelError(CellModelErrorCode.BOUNDS, "transaction geometry is stale")
        if snapshot:
            if base_revision != 0:
                raise CellModelError(
                    CellModelErrorCode.STALE_REVISION,
                    "snapshot base_revision must be zero",
                )
            if span_count == 0 or cell_count != cols * rows:
                raise CellModelError(
                    CellModelErrorCode.TRANSACTION,
                    "snapshot must declare every cell and at least one span",
                )
        else:
            if self._awaiting_snapshot or self._view is None:
                raise CellModelError(
                    CellModelErrorCode.STATE,
                    "a replacement snapshot is required",
                )
            if base_revision != self._view.revision:
                raise CellModelError(
                    CellModelErrorCode.STALE_REVISION,
                    f"base revision {base_revision} does not match {self._view.revision}",
                )
            if (span_count == 0) != (cell_count == 0):
                raise CellModelError(
                    CellModelErrorCode.TRANSACTION,
                    "zero span and cell counts must agree",
                )

        transaction_bytes = 176 + 52 * span_count + 8 * cell_count
        if transaction_bytes > UINT64_MAX:
            raise CellModelError(
                CellModelErrorCode.TRANSACTION,
                "declared transaction byte count exceeds uint64",
            )
        if transaction_bytes > self._max_transaction_bytes:
            raise CellModelError(
                CellModelErrorCode.TRANSACTION,
                "declared transaction exceeds negotiated maximum",
            )

        normalized = TransactionBegin(
            transaction_id,
            base_revision,
            cols,
            rows,
            span_count,
            cell_count,
        )
        snapshot_rows = (
            [[None for _ in range(cols)] for _ in range(rows)] if snapshot else None
        )
        self._staging = _Staging(
            begin=normalized,
            snapshot=snapshot,
            spans_seen=0,
            cells_seen=0,
            last_row=-1,
            last_end_column=0,
            next_snapshot_cell=0,
            cursor=None,
            dirty_spans=[],
            changed_rows={},
            snapshot_rows=snapshot_rows,
        )

    def stage_span(self, span: CellSpan) -> None:
        staging = self._require_staging()
        if not isinstance(span, CellSpan):
            self._fail(CellModelErrorCode.TRANSACTION, "span must be CellSpan")
        count = span.count
        if count == 0:
            self._fail(CellModelErrorCode.TRANSACTION, "span count is zero")
        if span.row >= self._rows or span.column >= self._cols:
            self._fail(CellModelErrorCode.BOUNDS, "span origin is outside geometry")
        if count > self._cols - span.column:
            self._fail(CellModelErrorCode.BOUNDS, "span extends past row boundary")
        if staging.spans_seen >= staging.begin.span_count:
            self._fail(CellModelErrorCode.TRANSACTION, "more spans than declared")
        if count > staging.begin.cell_count - staging.cells_seen:
            self._fail(CellModelErrorCode.TRANSACTION, "more cells than declared")
        if span.row < staging.last_row or (
            span.row == staging.last_row and span.column < staging.last_end_column
        ):
            self._fail(
                CellModelErrorCode.TRANSACTION,
                "spans are overlapping or not row-major",
            )

        if staging.snapshot:
            expected_flat = staging.next_snapshot_cell
            actual_flat = span.row * self._cols + span.column
            if actual_flat != expected_flat:
                self._fail(
                    CellModelErrorCode.TRANSACTION,
                    "snapshot spans contain a gap or overlap",
                )
            assert staging.snapshot_rows is not None
            target = staging.snapshot_rows[span.row]
        else:
            assert self._view is not None
            target = staging.changed_rows.get(span.row)
            if target is None:
                target = list(self._view.cells[span.row])
                staging.changed_rows[span.row] = target

        for offset, cell in enumerate(span.cells):
            if not isinstance(cell, Cell):
                self._fail(CellModelErrorCode.TRANSACTION, "span contains a non-Cell value")
            target[span.column + offset] = cell

        staging.spans_seen += 1
        staging.cells_seen += count
        staging.last_row = span.row
        staging.last_end_column = span.column + count
        staging.next_snapshot_cell += count
        staging.dirty_spans.append(DirtySpan(span.row, span.column, count))

    def stage_cursor(self, cursor: Cursor) -> None:
        staging = self._require_staging()
        if not isinstance(cursor, Cursor):
            self._fail(CellModelErrorCode.TRANSACTION, "cursor must be Cursor")
        if staging.cursor is not None:
            self._fail(CellModelErrorCode.TRANSACTION, "cursor was already staged")
        if cursor.visible and (cursor.row >= self._rows or cursor.column >= self._cols):
            self._fail(CellModelErrorCode.BOUNDS, "visible cursor is outside geometry")
        staging.cursor = cursor

    def commit(self, transaction_id: int) -> TerminalView:
        staging = self._require_staging()
        try:
            normalized_id = _integer(
                "transaction_id",
                transaction_id,
                minimum=1,
                maximum=UINT64_MAX,
            )
        except (TypeError, ValueError) as exc:
            self._fail(CellModelErrorCode.TRANSACTION, str(exc))
        if normalized_id != staging.begin.transaction_id:
            self._fail(CellModelErrorCode.TRANSACTION, "commit transaction_id mismatch")
        if staging.spans_seen != staging.begin.span_count:
            self._fail(CellModelErrorCode.TRANSACTION, "span count does not match declaration")
        if staging.cells_seen != staging.begin.cell_count:
            self._fail(CellModelErrorCode.TRANSACTION, "cell count does not match declaration")
        if staging.cursor is None:
            self._fail(CellModelErrorCode.TRANSACTION, "transaction has no cursor")

        if staging.snapshot:
            if staging.next_snapshot_cell != self._cols * self._rows:
                self._fail(CellModelErrorCode.TRANSACTION, "snapshot is not complete")
            assert staging.snapshot_rows is not None
            rows = tuple(
                tuple(cell for cell in row if cell is not None)
                for row in staging.snapshot_rows
            )
            if any(len(row) != self._cols for row in rows):
                self._fail(CellModelErrorCode.TRANSACTION, "snapshot contains an empty cell")
            revision = 1
        else:
            assert self._view is not None
            if self._view.revision == UINT64_MAX:
                self._fail(CellModelErrorCode.STATE, "model revision is exhausted")
            if staging.changed_rows:
                mutable_rows = list(self._view.cells)
                for row_index, row in staging.changed_rows.items():
                    mutable_rows[row_index] = tuple(row)
                rows = tuple(mutable_rows)
            else:
                rows = self._view.cells
            revision = self._view.revision + 1

        view = TerminalView(
            attachment_epoch=self._attachment_epoch,
            session_id=self._session_id,
            presentation_epoch=self._presentation_epoch,
            revision=revision,
            cols=self._cols,
            rows=self._rows,
            cells=rows,
            dirty_spans=tuple(staging.dirty_spans),
            cursor=staging.cursor,
        )
        self._staging = None
        self._view = view
        self._awaiting_snapshot = False
        return view

    def abort(self, transaction_id: int) -> None:
        normalized_id = _integer(
            "transaction_id", transaction_id, minimum=1, maximum=UINT64_MAX
        )
        staging = self._staging
        if staging is None:
            if normalized_id == self._most_recent_aborted_id:
                return
            raise CellModelError(CellModelErrorCode.STATE, "no transaction is open")
        if normalized_id != staging.begin.transaction_id:
            self._fail(CellModelErrorCode.TRANSACTION, "abort transaction_id mismatch")
        self._staging = None
        self._most_recent_aborted_id = normalized_id

    def soft_reset(self, requested_epoch: int) -> None:
        epoch = _integer(
            "requested_epoch",
            requested_epoch,
            minimum=0,
            maximum=UINT32_MAX,
        )
        if self._presentation_epoch == UINT32_MAX or epoch != self._presentation_epoch + 1:
            raise CellModelError(
                CellModelErrorCode.STATE,
                "requested epoch is not current presentation epoch plus one",
            )
        self._presentation_epoch = epoch
        self._view = None
        self._staging = None
        self._last_transaction_id = 0
        self._most_recent_aborted_id = 0
        self._awaiting_snapshot = True

    def validate_geometry(self, cols: int, rows: int) -> tuple[int, int]:
        """Validate a replacement geometry without mutating model state."""

        new_cols = _integer("cols", cols, minimum=1, maximum=UINT32_MAX)
        new_rows = _integer("rows", rows, minimum=1, maximum=UINT32_MAX)
        if self._staging is not None:
            raise CellModelError(
                CellModelErrorCode.STATE,
                "geometry cannot change during a transaction",
            )
        if snapshot_wire_bytes(new_cols, new_rows) > self._max_transaction_bytes:
            raise CellModelError(
                CellModelErrorCode.BOUNDS,
                "new geometry cannot fit a mandatory snapshot",
            )
        if new_cols * new_rows > self._max_cells:
            raise CellModelError(
                CellModelErrorCode.BOUNDS,
                "new geometry exceeds caller-owned model capacity",
            )
        return new_cols, new_rows

    def select_geometry(self, cols: int, rows: int) -> None:
        new_cols, new_rows = self.validate_geometry(cols, rows)
        self._cols = new_cols
        self._rows = new_rows
        self._view = None
        self._most_recent_aborted_id = 0
        self._awaiting_snapshot = True

    def _require_staging(self) -> _Staging:
        if self._staging is None:
            raise CellModelError(CellModelErrorCode.STATE, "no transaction is open")
        return self._staging

    def _raise(self, code: CellModelErrorCode, detail: str) -> None:
        raise CellModelError(code, detail)

    def _fail(self, code: CellModelErrorCode, detail: str) -> None:
        staging = self._staging
        if staging is not None:
            self._most_recent_aborted_id = staging.begin.transaction_id
            self._staging = None
        raise CellModelError(code, detail)


__all__ = [
    "BLANK_CELL",
    "Cell",
    "CellModel",
    "CellModelError",
    "CellModelErrorCode",
    "CellSpan",
    "Cursor",
    "DirtySpan",
    "TerminalView",
    "TransactionBegin",
    "WIRE_ATTRIBUTE_MASK",
    "decode_abort",
    "decode_cell_span",
    "decode_commit",
    "decode_cursor",
    "decode_transaction_begin",
]
