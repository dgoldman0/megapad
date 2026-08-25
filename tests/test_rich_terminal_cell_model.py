"""Focused atomicity tests for the renderer-neutral CELL-1 model."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from rich_terminal.cell_model import (
    Cell,
    CellModel,
    CellModelError,
    CellModelErrorCode,
    CellSpan,
    Cursor,
    DirtySpan,
    TransactionBegin,
    decode_abort,
    decode_cell_span,
    decode_commit,
    decode_cursor,
    decode_transaction_begin,
)
from rich_terminal.update_authority import (
    TerminalUpdateAuthority,
    TransactionFamily,
)


ROOT = Path(__file__).resolve().parents[1]


def _happy_payloads() -> dict[str, list[bytes]]:
    manifest = json.loads(
        (ROOT / "conformance" / "apt1" / "manifest.json").read_text(
            encoding="utf-8"
        )
    )
    transcript = next(
        item for item in manifest["transcripts"] if item["name"] == "happy_session"
    )
    result: dict[str, list[bytes]] = {}
    for item in transcript["frames"]:
        result.setdefault(item["message"], []).append(bytes.fromhex(item["full_hex"])[40:])
    return result


def _model() -> CellModel:
    return CellModel(
        attachment_epoch=7,
        session_id=0x0123456789ABCDEF,
        presentation_epoch=0,
        cols=2,
        rows=2,
        max_transaction_bytes=1_048_576,
        max_cells=4,
    )


def _commit_vector_snapshot(model: CellModel):
    payloads = _happy_payloads()
    model.begin(decode_transaction_begin(payloads["SNAPSHOT_BEGIN"][0]), snapshot=True)
    _stage_vector_replacement_body(model, payloads)
    return model.commit(decode_commit(payloads["SNAPSHOT_COMMIT"][0]))


def _stage_vector_replacement_body(
    model: CellModel,
    payloads: dict[str, list[bytes]] | None = None,
) -> None:
    if payloads is None:
        payloads = _happy_payloads()
    model.stage_span(decode_cell_span(payloads["CELL_SPAN"][0]))
    model.stage_span(decode_cell_span(payloads["CELL_SPAN"][1]))
    model.stage_cursor(decode_cursor(payloads["CURSOR"][0]))


def _prepare_clocked_initial_snapshot(
    model: CellModel,
    clock: TerminalUpdateAuthority,
):
    payloads = _happy_payloads()
    begin = decode_transaction_begin(payloads["SNAPSHOT_BEGIN"][0])
    lease = clock.reserve(TransactionFamily.CELL, begin.transaction_id, 0)
    model.begin_with_lease(begin, snapshot=True, lease=lease)
    _stage_vector_replacement_body(model, payloads)
    return lease, model.prepare_publication(
        lease,
        global_revision=clock.next_revision(lease),
    )


def test_normative_snapshot_publishes_one_atomic_immutable_view():
    model = _model()
    assert model.awaiting_snapshot
    assert model.view is None

    view = _commit_vector_snapshot(model)

    assert view.attachment_epoch == 7
    assert view.presentation_epoch == 0
    assert view.revision == 1
    assert (view.cols, view.rows) == (2, 2)
    assert tuple(cell.codepoint for row in view.cells for cell in row) == (
        ord("A"),
        ord("B"),
        ord("C"),
        ord(" "),
    )
    assert view.cells[0][0] == Cell(ord("A"), 7, 0, 1)
    assert view.cursor == Cursor(1, 1, True)
    assert view.dirty_spans == (DirtySpan(0, 0, 2), DirtySpan(1, 0, 2))
    assert model.view is view
    assert not model.awaiting_snapshot
    assert not model.transaction_open


def test_shared_clock_prepare_is_nonmutating_and_install_is_atomic():
    model = _model()
    clock = TerminalUpdateAuthority(presentation_epoch=0)
    lease, prepared = _prepare_clocked_initial_snapshot(model, clock)

    # Preparing allocates the complete immutable candidate, but neither the
    # shared clock nor the CELL model has published it yet.
    assert prepared.transaction_id == 1
    assert prepared.view.revision == 1
    assert clock.revision == 0
    assert model.view is None
    assert model.awaiting_snapshot
    assert model.transaction_open

    result = clock.complete_success(lease)
    assert result.revision == prepared.view.revision
    installed = model.install_prepared(prepared)

    assert installed is prepared.view
    assert model.view is installed
    assert model.revision == 1
    assert not model.awaiting_snapshot
    assert not model.transaction_open


def test_present_lease_can_prepare_replace_all_at_nonzero_global_revision():
    model = _model()
    _commit_vector_snapshot(model)
    model.select_geometry(2, 2)
    clock = TerminalUpdateAuthority(
        presentation_epoch=0,
        revision=7,
        transaction_high_water=10,
    )
    lease = clock.reserve(TransactionFamily.PRESENT, 11, 7)
    begin = TransactionBegin(11, 7, 2, 2, 2, 4)

    model.begin_with_lease(begin, snapshot=True, lease=lease)
    _stage_vector_replacement_body(model)
    prepared = model.prepare_publication(
        lease,
        global_revision=clock.next_revision(lease),
    )

    assert model.view is None
    assert prepared.view.revision == 8
    clock.complete_success(lease)
    assert model.install_prepared(prepared).revision == 8


def test_clocked_delta_uses_global_revision_when_cell_view_lags():
    model = _model()
    old = _commit_vector_snapshot(model)
    clock = TerminalUpdateAuthority(
        presentation_epoch=0,
        revision=4,
        transaction_high_water=8,
    )
    lease = clock.reserve(TransactionFamily.CELL, 9, 4)
    replacement = Cell(ord("G"), 2, 3)

    # Retained-only commits may have advanced the shared clock beyond the last
    # CELL publication. The exact clock lease, not old.revision, owns the base.
    model.begin_with_lease(
        TransactionBegin(9, 4, 2, 2, 1, 1),
        snapshot=False,
        lease=lease,
    )
    model.stage_span(CellSpan(1, 0, (replacement,)))
    model.stage_cursor(Cursor(0, 0, False))
    prepared = model.prepare_publication(
        lease,
        global_revision=clock.next_revision(lease),
    )

    assert model.view is old
    assert prepared.view.revision == 5
    clock.complete_success(lease)
    installed = model.install_prepared(prepared)
    assert installed.revision == 5
    assert installed.cells[1][0] is replacement


def test_prepared_publication_rejects_stale_and_foreign_model_state():
    model = _model()
    clock = TerminalUpdateAuthority(presentation_epoch=0)
    lease, first = _prepare_clocked_initial_snapshot(model, clock)
    delayed = model.prepare_publication(
        lease,
        global_revision=clock.next_revision(lease),
    )

    foreign_model = _model()
    foreign_clock = TerminalUpdateAuthority(presentation_epoch=0)
    foreign_lease, foreign = _prepare_clocked_initial_snapshot(
        foreign_model,
        foreign_clock,
    )

    clock.complete_success(lease)
    installed = model.install_prepared(first)

    with pytest.raises(RuntimeError, match="stale or foreign"):
        model.install_prepared(delayed)
    assert model.view is installed

    with pytest.raises(RuntimeError, match="stale or foreign"):
        model.install_prepared(foreign)
    assert model.view is installed
    assert foreign_model.view is None
    assert foreign_clock.open_transaction is foreign_lease


def test_delta_preserves_old_view_and_shares_unchanged_rows():
    model = _model()
    old = _commit_vector_snapshot(model)
    replacement = Cell(ord("Z"), 10, 20, 0x08)

    model.begin(TransactionBegin(2, 1, 2, 2, 1, 1), snapshot=False)
    model.stage_span(CellSpan(0, 1, (replacement,)))
    model.stage_cursor(Cursor(0, 0, False))

    # Nothing under construction is visible to a renderer.
    assert model.view is old
    assert old.cells[0][1].codepoint == ord("B")

    new = model.commit(2)
    assert new.revision == 2
    assert new.cells[0][1] is replacement
    assert new.cells[1] is old.cells[1]
    assert new.cells[0] is not old.cells[0]
    assert new.dirty_spans == (DirtySpan(0, 1, 1),)
    assert old.cells[0][1].codepoint == ord("B")


def test_invalid_staging_aborts_without_partial_visibility_or_id_reuse():
    model = _model()
    old = _commit_vector_snapshot(model)
    model.begin(TransactionBegin(2, 1, 2, 2, 1, 1), snapshot=False)

    with pytest.raises(CellModelError) as caught:
        model.stage_span(CellSpan(0, 2, (Cell(ord("X"), 7, 0),)))
    assert caught.value.code is CellModelErrorCode.BOUNDS
    assert not model.transaction_open
    assert model.view is old
    assert model.revision == 1

    with pytest.raises(CellModelError, match="monotonically"):
        model.begin(TransactionBegin(2, 1, 2, 2, 0, 0), snapshot=False)

    model.begin(TransactionBegin(3, 1, 2, 2, 0, 0), snapshot=False)
    model.stage_cursor(Cursor(0, 0, False))
    view = model.commit(3)
    assert view.revision == 2
    assert view.cells is old.cells


def test_snapshot_gap_and_soft_reset_require_a_fresh_complete_replacement():
    model = _model()
    _commit_vector_snapshot(model)
    model.soft_reset(1)
    assert model.presentation_epoch == 1
    assert model.revision == 0
    assert model.view is None
    assert model.awaiting_snapshot

    with pytest.raises(CellModelError) as normal:
        model.begin(TransactionBegin(1, 0, 2, 2, 0, 0), snapshot=False)
    assert normal.value.code is CellModelErrorCode.STATE

    model.begin(TransactionBegin(2, 0, 2, 2, 2, 4), snapshot=True)
    with pytest.raises(CellModelError, match="gap"):
        model.stage_span(CellSpan(0, 1, (Cell(ord("A"), 7, 0),)))
    assert not model.transaction_open


def test_payload_decoders_reject_reserved_bits_scalars_and_lengths():
    payloads = _happy_payloads()
    cursor = bytearray(payloads["CURSOR"][0])
    cursor[-1] = 1
    with pytest.raises(CellModelError, match="reserved"):
        decode_cursor(cursor)

    abort = bytearray(16)
    abort[0] = 1
    abort[-1] = 1
    with pytest.raises(CellModelError, match="reserved"):
        decode_abort(abort)

    span = bytearray(payloads["CELL_SPAN"][0])
    span[12:16] = (0xD800).to_bytes(4, "little")
    with pytest.raises(CellModelError) as scalar:
        decode_cell_span(span)
    assert scalar.value.code is CellModelErrorCode.SCALAR

    with pytest.raises(CellModelError, match="truncated"):
        decode_cell_span(b"\0" * 11)


def test_geometry_is_caller_bounded_and_cannot_interleave_a_transaction():
    with pytest.raises(ValueError, match="model capacity"):
        CellModel(
            attachment_epoch=1,
            session_id=1,
            presentation_epoch=0,
            cols=3,
            rows=2,
            max_transaction_bytes=1_048_576,
            max_cells=4,
        )

    model = _model()
    model.begin(TransactionBegin(1, 0, 2, 2, 2, 4), snapshot=True)
    with pytest.raises(CellModelError, match="during a transaction"):
        model.select_geometry(1, 4)
    model.abort(1)
    model.select_geometry(1, 4)
    assert model.geometry == (1, 4)
    assert model.awaiting_snapshot
