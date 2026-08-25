"""Lightweight structural locks for guest retained discovery composition."""

from __future__ import annotations

import re
from pathlib import Path


SOURCE = Path(__file__).resolve().parents[1] / "rich-terminal.f"


def _definition(source: str, word: str) -> str:
    match = re.search(
        rf"^:\s+{re.escape(word)}(?:\s|$).*?;\s*$",
        source,
        re.MULTILINE | re.DOTALL,
    )
    assert match is not None, word
    return match.group(0)


def test_retained_discovery_is_explicit_and_scheduled_without_input_starvation() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    discover = _definition(source, "PT-RETAINED-DISCOVER")
    service = _definition(source, "PT-SERVICE")

    assert "_PT.S.RET-ENABLED? !" in discover
    assert "_PT-FRAME" not in discover
    assert service.count("_PT-SERVICE-RET-QUERY") == 2
    assert service.index("_PT-SERVICE-RET-QUERY") < service.rindex(
        "_PT-SERVICE-BINARY"
    ) < service.rindex("_PT-SERVICE-RET-QUERY")


def test_service_yields_at_completion_before_any_close_boundary() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    binary = _definition(source, "_PT-SERVICE-BINARY")
    service = _definition(source, "PT-SERVICE")

    # Entry and post-dispatch guards prevent a later CLOSE/CLOSE_ACK in the
    # same buffered input from erasing a completion before its exact consumer
    # can poll it.
    assert binary.count("_PT.S.COMPLETE? @ IF PT-S-OK EXIT THEN") == 2
    assert binary.index("_PT.S.COMPLETE? @ IF PT-S-OK EXIT THEN") < (
        binary.index("BEGIN")
    )
    post_dispatch = binary.index(
        "_PT.S.COMPLETE? @ IF PT-S-OK EXIT THEN",
        binary.index("_PT-TRY-FRAME"),
    )
    assert post_dispatch < binary.index("_PT.S.STATE @ PT-ST-ANSI")

    # Sequence exhaustion reserves CLOSE until no transaction/result authority
    # remains.  The held path only services input and bypasses every ordinary
    # outbound scheduler.
    boundary = service.index("0xFFFFFFFFFFFFFFFE _PT-U>=")
    held_binary = service.index("_PT-SERVICE-BINARY EXIT", boundary)
    assert service.index("_PT-RESULT-BUSY? 0= AND IF", boundary) < held_binary
    assert held_binary < service.index("_PT-SERVICE-CREDIT", boundary)


def test_retained_activation_waits_for_legacy_transaction_settlement() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    activate = _definition(source, "_PT-RET-ACTIVATE-READY")
    credit = _definition(source, "_PT-DISPATCH-CREDIT")
    service = _definition(source, "PT-SERVICE")

    assert "_PT-RD-WAIT-CREDIT" in activate
    assert "_PT.S.PEER-GRANT @" in activate
    assert "_PT.S.RET-WATERMARK @" in activate
    assert "_PT.S.TX-OPEN? @" in activate
    assert "_PT-RESULT-BUSY?" in activate
    assert "_PT-RD-AVAILABLE" in activate

    # CREDIT records the covering grant, but availability remains gated until
    # any pre-discovery CELL transaction and its completion have settled.
    assert "_PT-RET-ACTIVATE-READY" in credit
    assert "_PT-RD-AVAILABLE" not in credit

    # PT-SERVICE retries once before ordinary schedulers and again after its
    # main binary-input pass can have consumed the legacy TX_RESULT.
    assert service.count("_PT-RET-ACTIVATE-READY") == 2
    main_binary = service.rindex("_PT-SERVICE-BINARY")
    assert service.index("_PT-RET-ACTIVATE-READY") < main_binary
    assert main_binary < service.rindex("_PT-RET-ACTIVATE-READY")


def test_retained_records_and_legacy_snapshot_are_lifecycle_gated() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    caps = _definition(source, "PT-RETAINED-CAPS@")
    formats = _definition(source, "PT-RETAINED-FORMATS@")
    begin = _definition(source, "_PT-BEGIN-TX")

    assert "PT-RETAINED-AVAILABLE?" in caps
    assert "PT-RETAINED-AVAILABLE?" in formats
    assert "_PT.S.RET-STATE @ _PT-RD-AVAILABLE =" in begin
    assert "PT-S-UNSUPPORTED EXIT" in begin


def test_present_begin_keeps_wire_authority_inside_pt() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    begin = _definition(source, "PT-PRESENT-BEGIN")
    emit = _definition(source, "_PT-PB-EMIT")
    preflight = _definition(source, "_PT-PB-PREFLIGHT?")

    assert "_PT.S.NEXT-TXID @" in begin
    assert "_PT.S.NEXT-TXID !" in begin
    assert "_PT.S.REVISION @ 0xFFFFFFFFFFFFFFFF =" in begin
    assert "_PT.S.REVISION @" in emit
    assert "_PT.S.GEOMETRY-GEN @" in emit
    assert "_PT-PB-BYTES @" in emit
    assert "_PT.S.PEER-GRANT @" in preflight
    assert "_PT.S.PEER-SENT @" in preflight
    assert "_PT.S.RET-CAPS 48 + _PT-U64@" in preflight


def test_present_body_is_exact_and_currently_region_bounded() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    operation = _definition(source, "PT-PRESENT-OP")
    region = _definition(source, "_PT-PO-REGION?")
    commit = _definition(source, "PT-PRESENT-COMMIT")

    assert "_PT.S.TX-RET-OPS-DONE" in operation
    assert "_PT.S.TX-RET-BYTES-DONE" in operation
    assert "_PT-M-REGION-DEFINE" in region
    assert "_PT-M-REGION-REPLACE" in region
    assert "_PT-M-REGION-DROP" in region
    assert "_PT.S.TX-RET-OPS @ <>" in commit
    assert "_PT.S.TX-RET-BYTES @ <>" in commit
    assert "PT-RET-REPLACE-START =" in commit
    assert "PT-RET-LAYOUT-START =" in commit


def test_retained_completion_is_bounded_without_weakening_legacy_cell() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    result = _definition(source, "_PT-DISPATCH-TX-RESULT")
    record = _definition(source, "_PT-RECORD-TX-COMPLETION")
    completion = _definition(source, "PT-COMPLETION-POLL")
    legacy_begin = _definition(source, "_PT-BEGIN-TX")

    assert "80  CONSTANT /PT-COMPLETION" in source
    assert "_PT-AWAIT-PRESENT" in result
    assert "PT-CELL-NONE = AND" in result
    assert "_PT-AWAIT-OWNER-DROP" in result
    assert "_PT-AWAIT-CELL" in record
    assert "_PT-RD-AVAILABLE" in record
    assert "PT-REQUEST-TX-COMMIT" in record
    assert "_PT-COMPLETION-DEST?" in completion
    assert "_PT-COMPLETION-CLEAR" in completion
    assert "_PT-RESULT-BUSY?" in legacy_begin
    assert "_PT-RD-AVAILABLE" not in _definition(source, "PT-TX-BEGIN")


def test_composed_storage_can_be_proven_disjoint_from_every_pt_borrow() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    predicate = _definition(source, "PT-STORAGE-DISJOINT?")

    assert "_PT-VALID-S?" in predicate
    assert "_PT-RANGE-VALID?" in predicate
    assert "_PT.S.RX-A @" in predicate
    assert "_PT.S.RX-U @" in predicate
    assert "_PT.S.TX-A @" in predicate
    assert "_PT.S.TX-U @" in predicate
    assert "_PT.S.EVENT-A @" in predicate
    assert "_PT.S.EVENT-U @" in predicate
    assert predicate.count("_PT-RANGES-OVERLAP?") == 4
    assert "!" not in predicate.replace("_PT-SD-S !", "").replace(
        "_PT-SD-U !", ""
    ).replace("_PT-SD-A !", "")


def test_owner_lifecycle_uses_ret_result_and_shared_drop_tx_result() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    owner_open = _definition(source, "PT-OWNER-OPEN")
    owner_drop = _definition(source, "PT-OWNER-DROP")
    ret_result = _definition(source, "_PT-DISPATCH-RET-RESULT")

    assert "_PT-M-OWNER-OPEN 64" in owner_open
    assert "TRUE _PT-OO-S @ _PT-FRAME-SEND" in owner_open
    assert "_PT-M-OWNER-DROP 32" in owner_drop
    assert "FALSE _PT-OD-S @ _PT-FRAME-SEND" in owner_drop
    assert "_PT-AWAIT-OWNER-DROP" in owner_drop
    assert "_PT.S.LIFE-TYPE @" in ret_result
    assert "_PT-COMPLETE-RET!" in ret_result


def test_retained_resize_and_reset_barriers_preserve_the_wire_profile() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    resize = _definition(source, "_PT-DISPATCH-RESIZE")
    resize_state = _definition(source, "_PT-RESIZE-STATE?")
    owner_open = _definition(source, "PT-OWNER-OPEN")
    owner_drop = _definition(source, "PT-OWNER-DROP")
    service_credit = _definition(source, "_PT-SERVICE-CREDIT")
    close = _definition(source, "PT-CLOSE")

    assert "PT-ST-RESYNCING" in resize_state
    assert "_PT-RSZ-GEN @ 0=" in resize
    assert resize.count("_PT-RD-WAIT-CREDIT") >= 3
    assert "_PT.S.RET-CAPS 48 + _PT-U64@" in resize
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in owner_open
    assert "PT-S-WOULD-BLOCK EXIT" in owner_open
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in owner_drop
    assert "PT-S-WOULD-BLOCK EXIT" in owner_drop
    assert "_PT.S.RESET-PENDING?" in service_credit
    assert "_PT.S.RESET-PENDING?" in close
