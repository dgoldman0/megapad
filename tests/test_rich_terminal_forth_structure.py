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


def test_parenthetical_comments_close_on_their_physical_source_line() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    unterminated = [
        (line_number, line)
        for line_number, line in enumerate(source.splitlines(), start=1)
        if re.search(r"\([^)]*$", line)
    ]
    assert unterminated == []


def test_header_ready_and_caps_reserved_fields_are_zero() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    frame_begin = _definition(source, "_PT-FRAME-BEGIN")
    receive_header = _definition(source, "_PT-RX-HEADER?")
    send_ready = _definition(source, "_PT-SEND-CLIENT-READY")
    receive_ready = _definition(source, "_PT-READY-PAYLOAD?")
    receive_caps = _definition(source, "_PT-RET-CAPS-VALID?")

    assert "0 _PT-F-A @ 4 + C!" in frame_begin
    assert "_PT-RX-A @ 4 + C@ 0<> OR" in receive_header
    assert "0 _PT-FRAME-PAYLOAD L!" in send_ready
    assert "_PT-RX-P @ L@ 0<>" in receive_ready
    assert "_PT-RV-P @ 4 + W@ 0<>" in receive_caps
    assert "_PT-RV-P @ 6 + W@ 0<>" in receive_caps


def test_frame_crc_prefers_checked_hardware_crc32c_without_stealing_owner() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    feed = _definition(source, "_PT-CRC-FEED?")
    hardware = _definition(source, "_PT-FRAME-CRC-HARDWARE?")
    frame_crc = _definition(source, "_PT-FRAME-CRC")

    assert "OVER @ CRC-FEED ?DUP IF" in feed
    assert "OVER C@ CRC-FEED-BYTE ?DUP IF" in feed
    assert "5 CRC-MODE! ?DUP IF 0 SWAP EXIT THEN" in hardware
    assert "0xFFFFFFFF CRC-INIT! ?DUP IF" in hardware
    assert "_PT-CRC-A @ 36 _PT-CRC-FEED?" in hardware
    assert "_PT-CRC-A @ _PT-HDR + _PT-CRC-U @ _PT-CRC-FEED?" in hardware
    assert hardware.count("CRC-FINAL@ DROP 0 R> EXIT") == 3
    assert "CRC-FINAL@ 0" in hardware

    assert "_PT-FRAME-CRC-HARDWARE? ?DUP IF" in frame_crc
    assert "0xFFFFFFFF _PT-CRC-A @ 36 _PT-CRC-RANGE" in frame_crc
    assert "_PT-CRC-A @ _PT-HDR + _PT-CRC-U @ _PT-CRC-RANGE" in frame_crc
    assert "0xFFFFFFFF XOR" in frame_crc


def test_glyph_run_discovery_capacity_is_core_owned() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    caps = _definition(source, "_PT-RET-CAPS-VALID?")
    formats = _definition(source, "_PT-RET-FORMATS-VALID?")

    optional_objects = caps.index("_PT-RV-FEATURES @ 0x11E AND IF")
    assert caps.index("_PT-RV-P @ 32 + L@ 0=", optional_objects) > optional_objects
    assert "0x1E AND 0<> _PT-POSITIVE-EXACT?" not in caps
    glyph_capacity = formats.index("_PT-RF-FORMATS @ 24 + L@ ?DUP IF")
    assert (
        formats.index(
            "_PT-RF-CAPS @ 32 + L@ 0= IF DROP FALSE EXIT THEN",
            glyph_capacity,
        )
        > glyph_capacity
    )
    assert formats.index("_PT-RF-FORMATS @ 48 + _PT-U64@ U>", glyph_capacity) > glyph_capacity
    assert formats.index("280 + _PT-RV-RETMAX @ U>", glyph_capacity) > glyph_capacity
    instrument = formats.index("_PT-RV-FEATURES @ 0x08 AND IF", glyph_capacity)
    assert glyph_capacity < instrument


def test_control_discovery_uses_shared_object_and_utf8_capacity() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    caps = _definition(source, "_PT-RET-CAPS-VALID?")
    formats = _definition(source, "_PT-RET-FORMATS-VALID?")

    assert "0x100    CONSTANT _PT-RET-CONTROLS" in source
    assert "0x13F    CONSTANT _PT-RET-FEATURE-MASK" in source
    assert "_PT-RET-FEATURE-MASK INVERT AND" in caps
    assert "_PT-RV-FEATURES @ 0x11E AND IF" in caps
    controls = caps.index("_PT-RV-FEATURES @ _PT-RET-CONTROLS AND IF")
    assert caps.index("_PT.S.PEER-MAX-PAY @ 80 U<", controls) > controls
    assert caps.index("_PT.S.CLIENT-MAX-PAY @ 40 U<", controls) > controls
    assert caps.index("_PT.S.TX-U @ 120 U<", controls) > controls
    assert caps.index("_PT-RV-RETMAX @ 280 U<", controls) > controls

    no_glyph = formats.index("_PT-RF-FORMATS @ 24 + L@ ?DUP IF")
    assert formats.index("_PT-RV-FEATURES @ _PT-RET-CONTROLS AND 0<>", no_glyph) > (
        no_glyph
    )
    assert formats.index("_PT-POSITIVE-EXACT?", no_glyph) > no_glyph


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

    # Sequence exhaustion latches the same bounded close intent before any
    # ordinary outbound scheduler.  An open local transaction is discarded;
    # emitted result authority is handled by the pending-close service path.
    boundary = service.index("0xFFFFFFFFFFFFFFFE _PT-U>=")
    latch = service.index("TRUE _PT-SVC-S @ _PT.S.CLOSE-PENDING? !", boundary)
    deadline = service.index("_PT.S.DEADLINE !", latch)
    discard = service.index("_PT-TX-CLEAR", deadline)
    publish = service.index("_PT-PUBLISH-PENDING-CLOSE EXIT", discard)
    assert latch < deadline < discard < publish
    assert publish < service.index("_PT-SERVICE-CREDIT", boundary)


def test_close_intent_is_one_bounded_writer_barrier() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    settlement = _definition(source, "_PT-SETTLEMENT-BUSY?")
    writer_busy = _definition(source, "_PT-RESULT-BUSY?")
    begin_close = _definition(source, "_PT-BEGIN-CLOSE")
    publish = _definition(source, "_PT-PUBLISH-PENDING-CLOSE")
    pending_service = _definition(source, "_PT-SERVICE-CLOSE-PENDING")
    soft_reset = _definition(source, "_PT-DISPATCH-SOFT-RESET")
    apply_reset = _definition(source, "_PT-APPLY-PENDING-RESET")
    to_ansi = _definition(source, "_PT-TO-ANSI")
    fail_common = _definition(source, "_PT-FAIL-COMMON")
    close = _definition(source, "PT-CLOSE")
    service = _definition(source, "PT-SERVICE")

    assert "880 CONSTANT /PT-SESSION" in source
    assert ": _PT.S.CLOSE-PENDING?  ( s -- a ) 872 + ;" in source
    for field in (
        "_PT.S.AWAIT?",
        "_PT.S.LIFE-AWAIT?",
        "_PT.S.COMPLETE?",
        "_PT.S.RESET-PENDING?",
    ):
        assert field in settlement
    assert "_PT.S.CLOSE-PENDING?" not in settlement
    assert "_PT.S.CLOSE-PENDING? @ OR" in writer_busy
    assert "_PT-SETTLEMENT-BUSY?" in writer_busy

    # The wire transition consumes the already-latched reason and deadline;
    # neither can be refreshed by publication backpressure or CLOSE_ACK wait.
    assert "_PT.S.CLOSE-REASON !" not in begin_close
    assert "_PT.S.DEADLINE !" not in begin_close
    assert "_PT.S.CLOSE-REASON !" not in publish
    assert "_PT.S.DEADLINE !" not in publish
    assert "_PT-SETTLEMENT-BUSY?" in publish
    assert "_PT-RESULT-BUSY?" not in publish
    assert publish.index("_PT-TX-CLEAR") < publish.index("_PT-BEGIN-CLOSE")
    assert publish.index("_PT-BEGIN-CLOSE") < publish.index(
        "FALSE _PT-CLOSE-S @ _PT.S.CLOSE-PENDING? !"
    )

    repeated = close.index("_PT.S.CLOSE-PENDING? @ IF")
    repeated_exit = close.index("PT-S-WOULD-BLOCK EXIT", repeated)
    reason = close.index("_PT.S.CLOSE-REASON !", repeated)
    latch = close.index("TRUE _PT-PC-S @ _PT.S.CLOSE-PENDING? !", reason)
    deadline = close.index("_PT.S.DEADLINE !", latch)
    discard = close.index("_PT-TX-CLEAR", deadline)
    publish_call = close.index("_PT-PUBLISH-PENDING-CLOSE", discard)
    assert repeated < repeated_exit < reason < latch < deadline < discard
    assert discard < publish_call
    assert close.count("_PT.S.CLOSE-REASON !") == 1
    assert close.count("_PT.S.DEADLINE !") == 1

    # Pending service has its own early scheduler.  It retires event
    # backpressure on both sides of binary parsing, exposes at most one result,
    # checks the original bound before RESET_ACK, and cannot emit ordinary
    # output or buy time.
    pre_discard = pending_service.index("_PT-DISCARD-PENDING-EVENT")
    binary = pending_service.index("_PT-SERVICE-BINARY", pre_discard)
    post_discard = pending_service.index("_PT-DISCARD-PENDING-EVENT", binary)
    first_timeout = pending_service.index("_PT.S.DEADLINE @ _PT-U>=", post_discard)
    would_return = pending_service.index(
        "_PT-CLOSE-STATUS @ PT-S-WOULD-BLOCK = IF", first_timeout
    )
    completion_gate = pending_service.index("_PT.S.COMPLETE? @ IF", would_return)
    reset = pending_service.index("_PT-APPLY-PENDING-RESET", completion_gate)
    second_timeout = pending_service.index("_PT.S.DEADLINE @ _PT-U>=", reset)
    publish_retry = pending_service.index("_PT-PUBLISH-PENDING-CLOSE")
    assert pre_discard < binary < post_discard < first_timeout
    assert first_timeout < would_return < completion_gate < reset
    assert reset < second_timeout < publish_retry
    assert pending_service.count("_PT-DISCARD-PENDING-EVENT") == 2
    assert pending_service.count("_PT.S.DEADLINE @ _PT-U>=") == 2
    assert pending_service.count("_PT.S.STATE @ PT-ST-CLOSING = IF") == 2
    assert pending_service.count(
        "FALSE _PT-SVC-S @ _PT.S.CLOSE-PENDING? !"
    ) >= 4
    for forbidden in (
        "_PT-SERVICE-CREDIT",
        "_PT-SERVICE-RET-QUERY",
        "_PT-RET-ACTIVATE-READY",
        "PT-RETAINED-DISCOVER",
    ):
        assert forbidden not in pending_service

    # A valid crossed reset cannot take the legacy sequence-headroom close
    # while a first close intent is pending.  RESET_ACK uses max-1 when it
    # fits; at max the reset is subsumed so the original CLOSE owns that slot.
    headroom = soft_reset.index("THEN U> IF")
    reset_close = soft_reset.index("_PT-RESET-CLOSE EXIT", headroom)
    assert soft_reset.index("_PT.S.CLOSE-PENDING? @ 0= IF", headroom) < reset_close
    latch = soft_reset.index("TRUE OVER _PT.S.RESET-PENDING? !")
    deferred_apply = soft_reset.index("_PT-APPLY-PENDING-RESET", latch)
    assert soft_reset.index("_PT.S.CLOSE-PENDING? @ IF", latch) < deferred_apply
    final_slot = apply_reset.index("_PT.S.TX-SEQ @ 0xFFFFFFFFFFFFFFFF =")
    close_qualified = apply_reset.index("_PT.S.CLOSE-PENDING? @")
    epoch_advance = apply_reset.index("_PT.S.RESET-EPOCH @ OVER _PT.S.EPOCH !")
    assert close_qualified < final_slot < epoch_advance
    assert "AND IF" in apply_reset[final_slot:epoch_advance]
    assert "_PT.S.RESET-PENDING? OFF" in apply_reset[final_slot:epoch_advance]
    assert "_PT.S.RESET-EPOCH OFF" in apply_reset[final_slot:epoch_advance]
    assert "_PT-RESET-CLOSE" not in pending_service

    # Every terminal boundary clears both parts of the pending-close latch;
    # neither ANSI reuse nor a hard failure may inherit stale close authority.
    for field in ("_PT.S.CLOSE-PENDING? OFF", "_PT.S.CLOSE-OPENING? OFF"):
        assert field in to_ansi
        assert field in fail_common
    pending_branch = service.index("_PT-SERVICE-CLOSE-PENDING")
    assert pending_branch < service.index("0xFFFFFFFFFFFFFFFE _PT-U>=")
    assert pending_branch < service.index("_PT-SERVICE-CREDIT")
    assert service.index("_PT.S.EVENT-PENDING !") < service.index(
        "_PT-PUBLISH-PENDING-CLOSE EXIT"
    )

    # The underlying ACTIVE/RESYNCING wire state remains intact for results,
    # while every public admission surface sees the irrevocable close barrier.
    for word in (
        "PT-ACTIVE?",
        "PT-RETAINED-STATE@",
        "PT-RETAINED-DISCOVER",
        "_PT-OP-LOST?",
    ):
        assert "_PT.S.CLOSE-PENDING?" in _definition(source, word)


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
    outbound = _definition(source, "PT-OUTBOUND-MAX-PAYLOAD@")
    begin = _definition(source, "_PT-BEGIN-TX")

    assert "PT-RETAINED-AVAILABLE?" in caps
    assert "PT-RETAINED-AVAILABLE?" in formats
    assert "PT-ACTIVE? 0= IF DROP 0 EXIT THEN" in outbound
    assert "_PT.S.PEER-MAX-PAY @" in outbound
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


def test_replace_start_can_refresh_live_or_pending_retained_state() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    retained_args = _definition(source, "_PT-PB-RET-ARGS?")

    replace_start = retained_args.index("PT-RET-REPLACE-START = IF")
    replace_continue = retained_args.index("PT-RET-REPLACE-CONTINUE = IF")
    replace_branch = retained_args[replace_start:replace_continue]

    assert "TRUE EXIT" in replace_branch
    assert "_PT-RB-REPLACE-REQUIRED" not in replace_branch
    assert "_PT-RB-NONE = AND EXIT" in retained_args[:replace_start]
    assert "_PT-RB-REPLACE-PENDING = EXIT" in retained_args[replace_continue:]
    assert "_PT-RB-LAYOUT-REQUIRED = EXIT" in retained_args[replace_continue:]
    assert "_PT-RB-LAYOUT-PENDING =" in retained_args[replace_continue:]


def test_present_body_keeps_raw_fixed_operations_private() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    operation = _definition(source, "_PT-PRESENT-FIXED-OP")
    region = _definition(source, "_PT-PO-REGION?")
    region_write = _definition(source, "_PT-REGION-WRITE")
    admit = _definition(source, "_PT-PO-ADMIT")
    send = _definition(source, "_PT-PO-SEND")
    commit = _definition(source, "PT-PRESENT-COMMIT")

    assert re.search(r"^:\s+PT-PRESENT-OP(?:\s|$)", source, re.MULTILINE) is None
    assert "_PT-PO-ADMIT" in operation
    assert "_PT-PO-SEND" in operation
    assert operation.index("_PT-VALID-S?") < operation.index("_PT-PO-SOURCE?")
    assert operation.index("_PT-OP-LOST?") < operation.index("_PT-PO-SOURCE?")
    assert "_PT.S.TX-RET-OPS-DONE" in admit
    assert "_PT.S.TX-RET-BYTES-DONE" in admit
    assert "_PT.S.TX-RET-OPS-DONE !" in send
    assert "_PT.S.TX-RET-BYTES-DONE !" in send
    assert "_PT-M-REGION-DEFINE" in region
    assert "_PT-M-REGION-REPLACE" in region
    assert "_PT-M-REGION-DROP" in region
    assert "_PT-PRESENT-FIXED-OP" in region_write
    assert "_PT.S.TX-RET-OPS @ <>" in commit
    assert "_PT.S.TX-RET-BYTES @ <>" in commit
    assert "PT-RET-REPLACE-START =" in commit
    assert "PT-RET-LAYOUT-START =" in commit


def test_typed_glyph_run_writers_own_exact_object_wire_assembly() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    define = _definition(source, "PT-GLYPH-RUN-DEFINE")
    replace = _definition(source, "PT-GLYPH-RUN-REPLACE")
    glyph_run = _definition(source, "_PT-GLYPH-RUN-WRITE")
    body = _definition(source, "_PT-GR-DEFINE-BODY")
    fields = _definition(source, "_PT-GR-FIELDS?")
    text_source = _definition(source, "_PT-GR-TEXT-SOURCE?")
    text = _definition(source, "_PT-GR-TEXT?")
    payload = _definition(source, "_PT-GR-PAYLOAD!")
    retained_args = _definition(source, "_PT-PB-RET-ARGS?")
    admit = _definition(source, "_PT-PO-ADMIT")
    send = _definition(source, "_PT-PO-SEND")
    scrub = _definition(source, "_PT-GR-SCRUB")

    assert "0x2020 CONSTANT _PT-M-OBJECT-DEFINE" in source
    assert "0x2021 CONSTANT _PT-M-OBJECT-REPLACE" in source
    assert "_PT-M-OBJECT-DEFINE _PT-GLYPH-RUN-WRITE" in define
    assert "_PT-M-OBJECT-REPLACE _PT-GLYPH-RUN-WRITE" in replace
    assert "_PT-GR-TYPE @ _PT-PO-TYPE !" in body
    assert "_PT-M-OBJECT-DEFINE =" in body
    assert "_PT-M-OBJECT-REPLACE =" in body
    assert body.index("_PT-PO-ADMIT") < body.index("_PT-GR-PAYLOAD!")
    assert body.index("_PT-GR-PAYLOAD!") < body.index("_PT-PO-SEND")
    assert "_PT-FRAME-BEGIN" in admit
    assert "_PT.S.TX-RET-OPS-DONE !" not in admit
    assert "_PT.S.TX-RET-BYTES-DONE !" not in admit
    assert "_PT.S.TX-RET-OPS-DONE !" in send
    assert "_PT.S.TX-RET-BYTES-DONE !" in send

    # The complete 64-byte common prefix and 16-byte GLYPH_RUN prefix are
    # packed by PT; Akashic supplies typed styling and the borrowed text span.
    expected_stores = (
        "_PT-GR-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-GR-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-GR-OBJECT @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "4 _PT-FRAME-PAYLOAD 24 + W!",
        "_PT-GR-VISIBLE @ _PT-FRAME-PAYLOAD 26 + W!",
        "_PT-GR-Z @ _PT-FRAME-PAYLOAD 28 + L!",
        "_PT-GR-REGION @ _PT-FRAME-PAYLOAD 32 + _PT-U64!",
        "_PT-GR-PARENT @ _PT-FRAME-PAYLOAD 40 + _PT-U64!",
        "_PT-GR-LEFT @ _PT-FRAME-PAYLOAD 48 + L!",
        "_PT-GR-TOP @ _PT-FRAME-PAYLOAD 52 + L!",
        "_PT-GR-RIGHT @ _PT-FRAME-PAYLOAD 56 + L!",
        "_PT-GR-BOTTOM @ _PT-FRAME-PAYLOAD 60 + L!",
        "_PT-GR-FG-RED @ _PT-FRAME-PAYLOAD 64 + C!",
        "_PT-GR-FG-GREEN @ _PT-FRAME-PAYLOAD 65 + C!",
        "_PT-GR-FG-BLUE @ _PT-FRAME-PAYLOAD 66 + C!",
        "_PT-GR-FG-ALPHA @ _PT-FRAME-PAYLOAD 67 + C!",
        "_PT-GR-BG-RED @ _PT-FRAME-PAYLOAD 68 + C!",
        "_PT-GR-BG-GREEN @ _PT-FRAME-PAYLOAD 69 + C!",
        "_PT-GR-BG-BLUE @ _PT-FRAME-PAYLOAD 70 + C!",
        "_PT-GR-BG-ALPHA @ _PT-FRAME-PAYLOAD 71 + C!",
        "_PT-GR-ATTRS @ _PT-FRAME-PAYLOAD 72 + W!",
        "0 _PT-FRAME-PAYLOAD 74 + W!",
        "_PT-GR-TEXT-U @ _PT-FRAME-PAYLOAD 76 + L!",
        "_PT-FRAME-PAYLOAD 80 + SWAP MOVE",
    )
    for store in expected_stores:
        assert store in payload

    assert "_PT-GR-TEXT-U @ 80 _PT-UADD?" in fields
    assert "_PT.S.RET-FORMATS 24 + L@ U>" in fields
    assert "_PT.S.RET-FORMATS 24 + L@ 0= IF FALSE EXIT THEN" in fields
    assert "_PT.S.RET-CAPS 8 + _PT-U64@ 0x08 AND" not in fields
    assert "0x006F CONSTANT _PT-GR-ATTR-MASK" in source
    assert "_PT-GR-ATTRS @ _PT-GR-ATTR-MASK INVERT AND" in fields
    assert body.index("_PT-GR-FIELDS?") < body.index("_PT-PO-ADMIT")
    assert "DUP 0<> SWAP 1 <> AND" in fields
    assert fields.count("DUP 0<> SWAP 1 <> AND") == 1
    assert "_PT-PB-RET-OPS @ 64 _PT-UMUL?" in retained_args
    assert "_PT-PB-RET-BYTES @ U>" in retained_args
    assert re.search(r"\bMOD\b", retained_args) is None

    assert "_PT-GR-TEXT-U @ 0= IF" in text_source
    assert "_PT-GR-TEXT-A @ 0= EXIT" in text_source
    assert "_PT-RANGE-VALID?" in text_source
    assert text_source.count("_PT-RANGES-OVERLAP?") == 2
    assert text.index("_PT-GR-TEXT-U @ 0= IF TRUE EXIT THEN") < (
        text.index("_PT-UTF8?")
    )
    for control in ("DUP 0=", "OVER 10 =", "SWAP 13 ="):
        assert control in text

    # No payload-sized staging allocation or borrowed text pointer survives
    # the guarded call.
    assert "CREATE" not in glyph_run + body + payload
    assert "ALLOT" not in glyph_run + body + payload
    assert "CATCH" in glyph_run
    assert "_PT-GR-SCRUB" in glyph_run
    assert "0 _PT-GR-TYPE !" in scrub
    assert "0 _PT-GR-TEXT-A !" in scrub
    assert "0 _PT-U8-A !" in scrub


def test_typed_control_writers_own_exact_wire_and_declared_accounting() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    define = _definition(source, "PT-CONTROL-DEFINE")
    replace = _definition(source, "PT-CONTROL-REPLACE")
    drop = _definition(source, "PT-CONTROL-DROP")
    write = _definition(source, "_PT-CONTROL-WRITE")
    body = _definition(source, "_PT-CT-DEFINE-BODY")
    fields = _definition(source, "_PT-CT-FIELDS?")
    kinds = _definition(source, "_PT-CT-KIND?")
    text = _definition(source, "_PT-CT-TEXT?")
    payload = _definition(source, "_PT-CT-PAYLOAD!")
    scrub = _definition(source, "_PT-CT-SCRUB")

    assert "0x4000 CONSTANT _PT-M-CONTROL-DEFINE" in source
    assert "0x4001 CONSTANT _PT-M-CONTROL-REPLACE" in source
    assert "0x4002 CONSTANT _PT-M-CONTROL-DROP" in source
    assert "_PT-M-CONTROL-DEFINE _PT-CONTROL-WRITE" in define
    assert "_PT-M-CONTROL-REPLACE _PT-CONTROL-WRITE" in replace
    assert "_PT-RET-CONTROLS? 0= IF PT-S-UNSUPPORTED EXIT THEN" in body
    assert body.index("_PT-PO-ADMIT") < body.index("_PT-CT-PAYLOAD!")
    assert body.index("_PT-CT-PAYLOAD!") < body.index("_PT-PO-SEND")

    expected_stores = (
        "_PT-CT-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-CT-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-CT-ID @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-CT-KIND @ _PT-FRAME-PAYLOAD 24 + W!",
        "_PT-CT-STATE @ _PT-FRAME-PAYLOAD 26 + W!",
        "_PT-CT-Z @ _PT-FRAME-PAYLOAD 28 + L!",
        "_PT-CT-REGION @ _PT-FRAME-PAYLOAD 32 + _PT-U64!",
        "_PT-CT-PARENT @ _PT-FRAME-PAYLOAD 40 + _PT-U64!",
        "_PT-CT-ORDER @ _PT-FRAME-PAYLOAD 48 + L!",
        "_PT-CT-LEFT @ _PT-FRAME-PAYLOAD 52 + L!",
        "_PT-CT-TOP @ _PT-FRAME-PAYLOAD 56 + L!",
        "_PT-CT-RIGHT @ _PT-FRAME-PAYLOAD 60 + L!",
        "_PT-CT-BOTTOM @ _PT-FRAME-PAYLOAD 64 + L!",
        "_PT-CT-LABEL-U @ _PT-FRAME-PAYLOAD 68 + L!",
        "_PT-CT-SHORTCUT-U @ _PT-FRAME-PAYLOAD 72 + L!",
        "0 _PT-FRAME-PAYLOAD 76 + L!",
        "_PT-FRAME-PAYLOAD 80 + SWAP MOVE",
        "_PT-FRAME-PAYLOAD 80 + _PT-CT-LABEL-U @ + SWAP MOVE",
    )
    for store in expected_stores:
        assert store in payload

    assert "_PT-CT-LABEL-U @ 80 _PT-UADD?" in fields
    assert "_PT-CT-SHORTCUT-U @ _PT-UADD?" in fields
    assert "_PT.S.PEER-MAX-PAY @ U>" in fields
    assert "_PT-CT-STATE @ 0x1F INVERT AND" in fields
    assert "PT-CONTROL-OPEN PT-CONTROL-SELECTED OR AND" in fields
    for kind in (
        "PT-CONTROL-MENU-BAR",
        "PT-CONTROL-MENU",
        "PT-CONTROL-MENU-ITEM",
        "PT-CONTROL-MENU-SEPARATOR",
    ):
        assert kind in kinds
    assert "_PT-CT-ROOT-BOUNDS?" in kinds
    assert kinds.count("_PT-CT-DESCENDANT?") == 3

    assert "_PT-UTF8?" in text
    assert "DUP 32 U< SWAP 127 = OR" in text
    assert text.count("_PT-RANGES-OVERLAP?") == 2
    assert "CATCH" in write
    assert "_PT-CT-SCRUB" in write
    assert "CREATE" not in write + body + payload
    assert "ALLOT" not in write + body + payload
    assert "0 _PT-CT-LABEL-A !" in scrub
    assert "0 _PT-CT-SHORTCUT-A !" in scrub

    assert "_PT-M-CONTROL-DROP _PT-PO-TYPE !" in drop
    assert "24 _PT-PO-U !" in drop
    assert drop.index("_PT-PO-ADMIT") < drop.index("_PT-FRAME-PAYLOAD _PT-U64!")
    assert drop.index("_PT-FRAME-PAYLOAD 16 + _PT-U64!") < drop.index(
        "_PT-PO-SEND"
    )


def test_control_event_is_feature_revision_and_type_checked() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    input_types = _definition(source, "_PT-INPUT-TYPE?")
    dispatch = _definition(source, "_PT-DISPATCH-CONTROL-EVENT")
    route = _definition(source, "_PT-DISPATCH")
    describe = _definition(source, "_PT-EVENT-DESCRIBE")

    assert "0x0205 CONSTANT PT-EVENT-CONTROL" in source
    assert "_PT-M-CONTROL-EVENT =" in input_types
    assert "_PT-M-CONTROL-EVENT" in route
    assert "_PT-RET-CONTROLS? 0=" in dispatch
    assert "_PT-RX-LEN @ 40 <>" in dispatch
    assert dispatch.count("_PT-U64@ 0=") == 3
    assert "W@ PT-CONTROL-ACTIVATE <>" in dispatch
    assert "W@ 0x3F INVERT AND 0<>" in dispatch
    assert "_PT-RX-P @ 28 + L@ 0<>" in dispatch
    assert "_PT.S.REVISION @ <>" in dispatch
    assert "_PT-ACCEPT-EVENT" in dispatch

    for mapping in (
        "_PT-EP-P @ 32 + _PT-U64@ _PT-EP-DST @ 8 + !",
        "_PT-EP-P @ _PT-U64@ _PT-EP-DST @ 16 + !",
        "_PT-EP-P @ 8 + _PT-U64@ _PT-EP-DST @ 24 + !",
        "_PT-EP-P @ 16 + _PT-U64@ _PT-EP-DST @ 32 + !",
        "_PT-EP-P @ 26 + W@ 16 LSHIFT OR _PT-EP-DST @ 40 + !",
    ):
        assert mapping in describe

    for accessor in (
        "PT-CONTROL-EVENT-OWNER@",
        "PT-CONTROL-EVENT-GENERATION@",
        "PT-CONTROL-EVENT-ID@",
        "PT-CONTROL-EVENT-KIND@",
        "PT-CONTROL-EVENT-MODIFIERS@",
    ):
        assert _definition(source, accessor)


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
    overlap = _definition(source, "_PT-RANGES-OVERLAP-READONLY?")

    assert "_PT-VALID-S?" in predicate
    assert "_PT-RANGE-VALID?" in predicate
    assert "_PT.S.RX-A @" in predicate
    assert "_PT.S.RX-U @" in predicate
    assert "_PT.S.TX-A @" in predicate
    assert "_PT.S.TX-U @" in predicate
    assert "_PT.S.EVENT-A @" in predicate
    assert "_PT.S.EVENT-U @" in predicate
    assert predicate.count("_PT-RANGES-OVERLAP-READONLY?") == 4
    assert "_PT-RANGES-OVERLAP?" not in predicate
    assert "!" not in predicate
    assert "VARIABLE _PT-SD-" not in source
    assert "!" not in overlap
    assert "VARIABLE" not in overlap
    assert "2OVER + 2 PICK U> >R" in overlap
    assert "+ SWAP DROP SWAP U>" in overlap
    assert "R> AND" in overlap


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
    settlement = _definition(source, "_PT-SETTLEMENT-BUSY?")

    assert "PT-ST-RESYNCING" in resize_state
    assert "_PT-RSZ-GEN @ 0=" in resize
    assert resize.count("_PT-RD-WAIT-CREDIT") >= 3
    assert "_PT.S.RET-CAPS 48 + _PT-U64@" in resize
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in owner_open
    assert "PT-S-WOULD-BLOCK EXIT" in owner_open
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in owner_drop
    assert "PT-S-WOULD-BLOCK EXIT" in owner_drop
    assert "_PT.S.RESET-PENDING?" in service_credit
    assert "_PT.S.RESET-PENDING?" in settlement
    assert "_PT-PUBLISH-PENDING-CLOSE" in close
