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


def test_transaction_frames_batch_until_an_atomic_protocol_boundary() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    publish = _definition(source, "_PT-FRAME-PUBLISH")
    send = _definition(source, "_PT-FRAME-SEND")
    queue = _definition(source, "_PT-FRAME-QUEUE")

    write = publish.index("_PT-F-A @ _PT-F-TOTAL @ TYPE")
    flush = publish.index("_PT-F-FLUSH @ IF TX-FLUSH THEN")
    release = publish.index("UART-RELEASE")
    assert publish.index("UART-ACQUIRE") < write < flush < release
    assert "TRUE _PT-F-FLUSH !" in send
    assert "FALSE _PT-F-FLUSH !" in queue

    buffered = (
        "_PT-EMIT-BEGIN",
        "_PT-PB-EMIT",
        "PT-CELL",
        "PT-CURSOR",
        "_PT-PO-SEND",
    )
    for word in buffered:
        definition = _definition(source, word)
        assert "_PT-FRAME-QUEUE" in definition
        assert "_PT-FRAME-SEND" not in definition
    assert source.count("_PT-FRAME-QUEUE") == len(buffered) + 1

    for word in (
        "_PT-SEND-CLIENT-READY",
        "_PT-SEND-CREDIT",
        "_PT-SEND-CLOSE",
        "_PT-SEND-CLOSE-ACK",
        "_PT-SEND-ABORT",
        "_PT-SEND-RESET-ACK",
        "_PT-SEND-RET-QUERY",
        "_PT-SEND-FATAL-ERROR",
        "PT-OWNER-OPEN",
        "PT-OWNER-DROP",
        "PT-PRESENT-COMMIT",
        "_PT-COMMIT",
    ):
        definition = _definition(source, word)
        assert "_PT-FRAME-SEND" in definition
        assert "_PT-FRAME-QUEUE" not in definition

    negotiation = _definition(source, "_PT-W-PUBLISH")
    assert "TYPE TX-FLUSH" in negotiation


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
    assert "0x200    CONSTANT _PT-RET-CONTROL-COLLECTIONS" in source
    assert "0x33F    CONSTANT _PT-RET-FEATURE-MASK" in source
    assert "_PT-RET-FEATURE-MASK INVERT AND" in caps
    assert (
        "_PT-RV-FEATURES @ _PT-RET-CONTROL-COLLECTIONS AND\n"
        "    _PT-RV-FEATURES @ _PT-RET-CONTROLS AND 0= AND"
    ) in caps
    assert "_PT-RV-FEATURES @ 0x11E AND IF" in caps
    controls = caps.index("_PT-RV-FEATURES @ _PT-RET-CONTROLS AND IF")
    assert caps.index("_PT.S.PEER-MAX-PAY @ 80 U<", controls) > controls
    assert caps.index("_PT.S.CLIENT-MAX-PAY @ 40 U<", controls) > controls
    assert caps.index("_PT.S.TX-U @ 120 U<", controls) > controls
    assert caps.index("_PT-RV-RETMAX @ 280 U<", controls) > controls

    collections = caps.index(
        "_PT-RV-FEATURES @ _PT-RET-CONTROL-COLLECTIONS AND IF"
    )
    assert caps.index("_PT.S.PEER-MAX-PAY @ 152 U<", collections) > collections
    assert caps.index("_PT.S.TX-U @ 192 U<", collections) > collections
    assert caps.index("_PT-RV-RETMAX @ 352 U<", collections) > collections

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

    assert "952 CONSTANT /PT-SESSION" in source
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
    source_span = _definition(source, "_PT-CT-SOURCE?")
    text = _definition(source, "_PT-CT-TEXT?")
    spans = _definition(source, "_PT-CT-SPANS-DISJOINT?")
    payload = _definition(source, "_PT-CT-PAYLOAD!")
    scrub = _definition(source, "_PT-CT-SCRUB")

    assert "0x4000 CONSTANT _PT-M-CONTROL-DEFINE" in source
    assert "0x4001 CONSTANT _PT-M-CONTROL-REPLACE" in source
    assert "0x4002 CONSTANT _PT-M-CONTROL-DROP" in source
    assert "_PT-M-CONTROL-DEFINE _PT-CONTROL-WRITE" in define
    assert "_PT-M-CONTROL-REPLACE _PT-CONTROL-WRITE" in replace
    assert "_PT-RET-CONTROLS? 0= IF PT-S-UNSUPPORTED EXIT THEN" in body
    assert "_PT-CT-KIND @ _PT-CT-COLLECTION-KIND? IF" in body
    assert "_PT-RET-CONTROL-COLLECTIONS? 0= IF" in body
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
        "_PT-CT-CONTENT-U @ _PT-FRAME-PAYLOAD 76 + L!",
        "_PT-FRAME-PAYLOAD 80 + SWAP MOVE",
        "_PT-FRAME-PAYLOAD 80 + _PT-CT-LABEL-U @ + SWAP MOVE",
        "_PT-FRAME-PAYLOAD 80 + _PT-CT-LABEL-U @ +",
        "_PT-CT-SHORTCUT-U @ + SWAP MOVE",
    )
    for store in expected_stores:
        assert store in payload

    assert "_PT-CT-LABEL-U @ 80 _PT-UADD?" in fields
    assert "_PT-CT-SHORTCUT-U @ _PT-UADD?" in fields
    assert "_PT-CT-CONTENT-U @ _PT-UADD?" in fields
    assert "_PT.S.PEER-MAX-PAY @ U>" in fields
    assert "_PT-CT-STATE @ 0x1F INVERT AND" in fields
    assert "PT-CONTROL-OPEN PT-CONTROL-SELECTED OR AND" in fields
    for kind in (
        "PT-CONTROL-MENU-BAR",
        "PT-CONTROL-MENU",
        "PT-CONTROL-MENU-ITEM",
        "PT-CONTROL-MENU-SEPARATOR",
        "PT-CONTROL-TEXT-AREA",
        "PT-CONTROL-TEXT-GRID",
        "PT-CONTROL-TABSET",
        "PT-CONTROL-TAB",
    ):
        assert kind in kinds
    for value, kind in enumerate(
        (
            "PT-CONTROL-TEXT-AREA",
            "PT-CONTROL-TEXT-GRID",
            "PT-CONTROL-TABSET",
            "PT-CONTROL-TAB",
        ),
        start=5,
    ):
        assert f"{value} CONSTANT {kind}" in source
    assert "_PT-CT-CONTENT-U @ 72 U<" in kinds
    assert "_PT-CT-ROOT-BOUNDS?" in kinds
    assert kinds.count("_PT-CT-DESCENDANT?") == 4

    assert "_PT-UTF8?" in text
    assert "DUP 32 U< SWAP 127 = OR" in text
    assert source_span.count("_PT-RANGES-OVERLAP?") == 2
    assert spans.count("_PT-RANGES-OVERLAP?") == 3
    assert "_PT-CT-CONTENT-A @ _PT-CT-CONTENT-U @ _PT-CT-SOURCE?" in fields
    assert "_PT-CT-SPANS-DISJOINT?" in fields
    assert "CATCH" in write
    assert "_PT-CT-SCRUB" in write
    assert "CREATE" not in write + body + payload
    assert "ALLOT" not in write + body + payload
    assert "0 _PT-CT-LABEL-A !" in scrub
    assert "0 _PT-CT-SHORTCUT-A !" in scrub
    assert "0 _PT-CT-CONTENT-A !" in scrub
    assert "_PT-CT-CONTENT-U ! _PT-CT-CONTENT-A !" in write

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


def test_resource_lifecycle_exposes_one_generic_typed_abi() -> None:
    source = SOURCE.read_text(encoding="utf-8")

    for declaration in (
        "1 CONSTANT PT-RESOURCE-RGBA8",
        "0 CONSTANT PT-RESOURCE-ABORT-CALLER-CANCEL",
        "1 CONSTANT PT-RESOURCE-ABORT-RESET-REBUILD",
        "2 CONSTANT PT-RESOURCE-ABORT-LOCAL-SHUTDOWN",
        "0x000C CONSTANT _PT-M-RESOURCE-ABORT",
        "0x1000 CONSTANT _PT-M-RESOURCE-BEGIN",
        "0x1001 CONSTANT _PT-M-RESOURCE-CHUNK",
        "0x1002 CONSTANT _PT-M-RESOURCE-COMMIT",
        "0x1003 CONSTANT _PT-M-RESOURCE-DROP",
        "_PT-M-RESOURCE-BEGIN  CONSTANT PT-REQUEST-RESOURCE-BEGIN",
        "_PT-M-RESOURCE-CHUNK  CONSTANT PT-REQUEST-RESOURCE-CHUNK",
        "_PT-M-RESOURCE-COMMIT CONSTANT PT-REQUEST-RESOURCE-COMMIT",
        "_PT-M-RESOURCE-DROP   CONSTANT PT-REQUEST-RESOURCE-DROP",
        "_PT-M-RESOURCE-ABORT  CONSTANT PT-REQUEST-RESOURCE-ABORT",
    ):
        assert declaration in source

    assert "952 CONSTANT /PT-SESSION" in source
    for accessor in (
        ": _PT.S.LIFE-ITEM       ( s -- a ) 880 + ;",
        ": _PT.S.LIFE-WATERMARK  ( s -- a ) 888 + ;",
        ": _PT.S.LIFE-BYTES      ( s -- a ) 896 + ;",
        ": _PT.S.UPLOAD?         ( s -- a ) 904 + ;",
        ": _PT.S.UPLOAD-OWNER    ( s -- a ) 912 + ;",
        ": _PT.S.UPLOAD-GENERATION ( s -- a ) 920 + ;",
        ": _PT.S.UPLOAD-ITEM     ( s -- a ) 928 + ;",
        ": _PT.S.UPLOAD-LENGTH   ( s -- a ) 936 + ;",
        ": _PT.S.UPLOAD-OFFSET   ( s -- a ) 944 + ;",
    ):
        assert accessor in source

    assert (
        "\\ Stack: owner generation resource format width height flags byte-length\n"
        "\\        sha3-a sha3-u session -- status\n"
        ": PT-RESOURCE-BEGIN"
    ) in source
    assert (
        "\\ Stack: owner generation resource offset data-a data-u session -- status\n"
        ": PT-RESOURCE-CHUNK"
    ) in source
    assert (
        ": PT-RESOURCE-COMMIT  "
        "( owner generation resource session -- status )"
    ) in source
    assert (
        ": PT-RESOURCE-DROP  "
        "( owner generation resource session -- status )"
    ) in source
    assert (
        ": PT-RESOURCE-ABORT\n"
        "    ( owner generation resource reason session -- status )"
    ) in source


def test_resource_begin_and_chunk_are_bounded_exact_wire_writers() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    frame_begin = _definition(source, "_PT-FRAME-BEGIN")
    source_safe = _definition(source, "_PT-RESOURCE-SOURCE?")
    ordinary = _definition(source, "_PT-RESOURCE-ORDINARY-ADMIT")
    begin_fields = _definition(source, "_PT-RESOURCE-BEGIN-FIELDS?")
    begin_body = _definition(source, "_PT-RESOURCE-BEGIN-BODY")
    begin = _definition(source, "PT-RESOURCE-BEGIN")
    begin_scrub = _definition(source, "_PT-RESOURCE-BEGIN-SCRUB")
    chunk_fields = _definition(source, "_PT-RESOURCE-CHUNK-FIELDS?")
    chunk_body = _definition(source, "_PT-RESOURCE-CHUNK-BODY")
    chunk = _definition(source, "PT-RESOURCE-CHUNK")
    chunk_scrub = _definition(source, "_PT-RESOURCE-CHUNK-SCRUB")

    # Borrowed digest and chunk bytes must be valid and disjoint from both
    # storage regions that PT can overwrite while constructing the frame.
    assert "_PT-RANGE-VALID?" in source_safe
    assert source_safe.count("_PT-RANGES-OVERLAP?") == 2
    assert "/PT-SESSION" in source_safe
    assert "_PT.S.TX-A @" in source_safe
    assert "_PT.S.TX-U @" in source_safe
    assert "_PT-F-TOTAL @ 0 FILL" in frame_begin

    # RESOURCE_BEGIN has exact RGBA8 dimensions/length/hash semantics and
    # consumes ordinary cumulative credit only after every bound is proven.
    for invariant in (
        "_PT-RBG-FORMAT @ PT-RESOURCE-RGBA8 <>",
        "_PT-RBG-FLAGS @ 0<>",
        "_PT-RBG-WIDTH @ DUP 0= SWAP _PT-U32? 0= OR",
        "_PT-RBG-HEIGHT @ DUP 0= SWAP _PT-U32? 0= OR",
        "_PT.S.RET-FORMATS 12 + L@ U>",
        "_PT.S.RET-FORMATS 16 + L@ U>",
        "_PT-RBG-WIDTH @ _PT-RBG-HEIGHT @ _PT-UMUL?",
        "4 _PT-UMUL?",
        "_PT-RBG-LENGTH @ <>",
        "_PT.S.RET-CAPS 56 + _PT-U64@ U>",
        "_PT-RBG-DIGEST-U @ 32 <>",
        "_PT-RESOURCE-SOURCE?",
    ):
        assert invariant in begin_fields
    assert "_PT.S.RET-CAPS 8 + _PT-U64@ 0x04 AND 0=" in begin_body
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in begin_body
    assert "_PT-RESULT-BUSY?" in begin_body
    assert "_PT.S.UPLOAD? @ IF PT-S-WOULD-BLOCK EXIT THEN" in begin_body
    assert "120 _PT-RBG-S @ _PT-RESOURCE-ORDINARY-ADMIT" in begin_body
    assert "_PT-M-RESOURCE-BEGIN 80" in begin_body
    for store in (
        "_PT-RBG-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-RBG-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-RBG-ITEM @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-RBG-FORMAT @ _PT-FRAME-PAYLOAD 24 + L!",
        "_PT-RBG-WIDTH @ _PT-FRAME-PAYLOAD 28 + L!",
        "_PT-RBG-HEIGHT @ _PT-FRAME-PAYLOAD 32 + L!",
        "_PT-RBG-FLAGS @ _PT-FRAME-PAYLOAD 36 + L!",
        "_PT-RBG-LENGTH @ _PT-FRAME-PAYLOAD 40 + _PT-U64!",
        "_PT-RBG-DIGEST-A @ 32 _PT-FRAME-PAYLOAD 48 + SWAP MOVE",
    ):
        assert store in begin_body
    sent = begin_body.index("TRUE _PT-RBG-S @ _PT-FRAME-SEND")
    assert sent < begin_body.index("TRUE _PT-RBG-S @ _PT.S.UPLOAD? !")
    assert sent < begin_body.index("_PT-RESOURCE-LIFE!")
    assert "CATCH" in begin
    assert "_PT-RESOURCE-BEGIN-SCRUB" in begin
    for pointer in ("_PT-RBG-DIGEST-A", "_PT-RBG-DIGEST-U", "_PT-RSA"):
        assert f"0 {pointer} !" in begin_scrub

    # RESOURCE_CHUNK is sequential and non-empty.  Its covering watermark is
    # initial credit plus the post-send cumulative byte count, not a per-frame
    # acknowledgement surrogate.
    for invariant in (
        "_PT.S.UPLOAD-OWNER @ <>",
        "_PT.S.UPLOAD-GENERATION @ <> OR",
        "_PT.S.UPLOAD-ITEM @ <> OR",
        "_PT.S.UPLOAD-OFFSET @ <>",
        "_PT-RCH-DATA-U @ DUP 0= SWAP _PT-U32? 0= OR",
        "_PT.S.RET-CAPS 44 + L@ U>",
        "_PT-RCH-OFFSET @ _PT-RCH-DATA-U @ _PT-UADD?",
        "_PT.S.UPLOAD-LENGTH @ U>",
        "32 _PT-RCH-DATA-U @ _PT-UADD?",
        "40 _PT-UADD?",
        "_PT-RESOURCE-SOURCE?",
    ):
        assert invariant in chunk_fields
    post_send = chunk_fields.index("_PT.S.PEER-SENT @ SWAP _PT-UADD?")
    initial = chunk_fields.index("_PT.S.PEER-INITIAL @ _PT-UADD?", post_send)
    watermark = chunk_fields.index("_PT-RCH-WATERMARK !", initial)
    assert post_send < initial < watermark
    assert "_PT-RCH-FRAME-U @ _PT-RCH-S @" in chunk_body
    assert "_PT-RESOURCE-ORDINARY-ADMIT" in chunk_body
    assert "_PT-M-RESOURCE-CHUNK _PT-RCH-PAYLOAD-U @" in chunk_body
    for store in (
        "_PT-RCH-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-RCH-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-RCH-ITEM @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-RCH-OFFSET @ _PT-FRAME-PAYLOAD 24 + _PT-U64!",
        "_PT-FRAME-PAYLOAD 32 + SWAP MOVE",
    ):
        assert store in chunk_body
    sent = chunk_body.index("TRUE _PT-RCH-S @ _PT-FRAME-SEND")
    assert sent < chunk_body.index("_PT-RESOURCE-LIFE!")
    assert "CATCH" in chunk
    assert "_PT-RESOURCE-CHUNK-SCRUB" in chunk
    for pointer in ("_PT-RCH-DATA-A", "_PT-RCH-DATA-U", "_PT-RSA"):
        assert f"0 {pointer} !" in chunk_scrub

    assert "_PT.S.PEER-SENT @" in ordinary
    assert "_PT.S.PEER-GRANT @ U>" in ordinary
    assert "PT-S-WOULD-BLOCK EXIT" in ordinary


def test_resource_commit_drop_and_abort_preserve_lifecycle_authority() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    item = _definition(source, "_PT-RESOURCE-ITEM-WRITE")
    exact = _definition(source, "_PT-RESOURCE-ITEM-EXACT?")
    commit = _definition(source, "PT-RESOURCE-COMMIT")
    drop = _definition(source, "PT-RESOURCE-DROP")
    abort = _definition(source, "PT-RESOURCE-ABORT")
    tracked_abort = _definition(source, "_PT-RESOURCE-ABORT-TRACKED")

    for field in (
        "_PT.S.UPLOAD-OWNER @ =",
        "_PT.S.UPLOAD-GENERATION @ = AND",
        "_PT.S.UPLOAD-ITEM @ = AND",
    ):
        assert field in exact
    commit_branch = item.index("_PT-RRI-TYPE @ _PT-M-RESOURCE-COMMIT = IF")
    drop_branch = item.index("ELSE", commit_branch)
    admit = item.index("64 _PT-RRI-S @ _PT-RESOURCE-ORDINARY-ADMIT", drop_branch)
    assert "_PT.S.UPLOAD? @ 0=" in item[commit_branch:drop_branch]
    assert "_PT-RESOURCE-ITEM-EXACT?" in item[commit_branch:drop_branch]
    assert "_PT.S.UPLOAD-OFFSET @" in item[commit_branch:drop_branch]
    assert "_PT.S.UPLOAD-LENGTH @ <>" in item[commit_branch:drop_branch]
    assert "_PT-M-RESOURCE-DROP <>" in item[drop_branch:admit]
    assert "_PT-RETAINED-LIFECYCLE-STATE?" in item[drop_branch:admit]
    assert "_PT.S.UPLOAD? @ IF PT-S-WOULD-BLOCK EXIT THEN" in item[drop_branch:admit]
    assert "_PT-RRI-TYPE @ 24" in item
    for store in (
        "_PT-RRI-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-RRI-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-RRI-ITEM @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
    ):
        assert store in item
    assert "TRUE _PT-RRI-S @ _PT-FRAME-SEND" in item
    assert item.index("TRUE _PT-RRI-S @ _PT-FRAME-SEND") < item.index(
        "_PT-RESOURCE-LIFE!"
    )
    assert "_PT-M-RESOURCE-COMMIT _PT-RESOURCE-ITEM-WRITE" in commit
    assert "_PT-M-RESOURCE-DROP _PT-RESOURCE-ITEM-WRITE" in drop

    assert "_PT.S.UPLOAD? @ 0=" in abort
    for field in (
        "_PT.S.UPLOAD-OWNER @ <>",
        "_PT.S.UPLOAD-GENERATION @ <> OR",
        "_PT.S.UPLOAD-ITEM @ <> OR",
    ):
        assert field in abort
    assert "PT-RESOURCE-ABORT-LOCAL-SHUTDOWN U>" in abort
    assert "_PT-RESOURCE-ABORT-TRACKED" in abort
    assert "_PT-M-RESOURCE-ABORT 32" in tracked_abort
    for store in (
        "_PT.S.UPLOAD-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT.S.UPLOAD-ITEM @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-URA-REASON @ _PT-FRAME-PAYLOAD 24 + W!",
    ):
        assert store in tracked_abort
    assert "FALSE _PT-URA-S @ _PT-FRAME-SEND" in tracked_abort
    assert tracked_abort.index("FALSE _PT-URA-S @ _PT-FRAME-SEND") < (
        tracked_abort.index("TRUE _PT-URA-S @ _PT.S.LIFE-AWAIT? !")
    )
    for field in (
        "_PT.S.LIFE-TYPE !",
        "_PT.S.LIFE-OWNER !",
        "_PT.S.LIFE-GENERATION !",
        "_PT.S.LIFE-ITEM !",
        "_PT.S.LIFE-WATERMARK !",
        "_PT.S.LIFE-BYTES !",
    ):
        assert field in tracked_abort


def test_resource_results_are_request_aware_and_chunks_settle_on_credit() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    statuses = _definition(source, "_PT-RET-RESULT-STATUS?")
    result = _definition(source, "_PT-DISPATCH-RET-RESULT")
    credit = _definition(source, "_PT-DISPATCH-CREDIT")
    complete_chunk = _definition(source, "_PT-COMPLETE-CHUNK-CREDIT")

    begin_branch = statuses.index("_PT-M-RESOURCE-BEGIN = IF")
    chunk_branch = statuses.index("_PT-M-RESOURCE-CHUNK = IF", begin_branch)
    commit_branch = statuses.index("_PT-M-RESOURCE-COMMIT = IF", chunk_branch)
    drop_branch = statuses.index("_PT-M-RESOURCE-DROP = IF", commit_branch)
    abort_branch = statuses.index("_PT-M-RESOURCE-ABORT = IF", drop_branch)
    chunk_statuses = statuses[chunk_branch:commit_branch]
    assert "PT-RET-INVALID" in chunk_statuses
    assert "PT-RET-STALE-OWNER" in chunk_statuses
    assert "PT-RET-OK" not in chunk_statuses
    assert "PT-RET-DUPLICATE-ID _PT-U<=" in statuses[begin_branch:chunk_branch]
    assert "PT-RET-BAD-CONTENT" in statuses[commit_branch:drop_branch]
    assert "PT-RET-IN-USE" in statuses[drop_branch:abort_branch]
    assert "PT-RET-ABORTED" in statuses[abort_branch:]

    assert "_PT-RX-LEN @ 48 <>" in result
    assert "_PT.S.LIFE-AWAIT? @ 0=" in result
    assert "_PT.S.LIFE-TYPE @ <>" in result
    assert "_PT-RET-RESULT-STATUS?" in result
    for field in (
        "_PT.S.LIFE-OWNER @ <>",
        "_PT.S.LIFE-GENERATION @ <>",
        "_PT.S.LIFE-ITEM @ <>",
        "_PT.S.REVISION @ <>",
    ):
        assert field in result
    accepted = result.index("_PT-RX-P @ 40 + _PT-U64@ _PT-CMP-ACCEPTED !")
    completion = result.index("_PT-COMPLETE-RET!", accepted)
    assert "_PT-M-RESOURCE-COMMIT =" in result[accepted:completion]
    assert "PT-RET-OK = AND" in result[accepted:completion]
    assert "_PT.S.UPLOAD-LENGTH @ <>" in result[accepted:completion]
    assert "_PT-CMP-ACCEPTED @ 0<>" in result[accepted:completion]
    assert result.index("_PT-COMPLETE-RET!", accepted) < result.index(
        "_PT-LIFE-CLEAR", completion
    )

    # Success for CHUNK is inferred only from a covering cumulative CREDIT.
    # It becomes an ordinary pollable RET completion carrying exactly the
    # bytes from the tracked chunk before the lifecycle wait is released.
    life = credit.index("_PT.S.LIFE-AWAIT? @")
    request = credit.index("_PT-M-RESOURCE-CHUNK = AND", life)
    covering = credit.index("_PT.S.LIFE-WATERMARK @ _PT-U>=", request)
    synthesize = credit.index("_PT-COMPLETE-CHUNK-CREDIT", covering)
    assert life < request < covering < synthesize
    advance = complete_chunk.index("_PT.S.UPLOAD-OFFSET @")
    accepted_store = complete_chunk.index(
        "_PT.S.LIFE-BYTES @ R@ _PT.S.COMP-ACCEPTED !", advance
    )
    visible = complete_chunk.index("TRUE R@ _PT.S.COMPLETE? !", accepted_store)
    clear = complete_chunk.index("_PT-LIFE-CLEAR", visible)
    assert "PT-COMPLETE-RET R@ _PT.S.COMP-KIND !" in complete_chunk
    assert "PT-REQUEST-RESOURCE-CHUNK R@ _PT.S.COMP-REQUEST !" in complete_chunk
    assert "PT-RET-OK R@ _PT.S.COMP-STATUS !" in complete_chunk
    assert advance < accepted_store < visible < clear


def test_series_lifecycle_exposes_one_typed_native_cell_abi() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    mode = _definition(source, "_PT-SERIES-MODE?")
    feature = _definition(source, "_PT-RET-SERIES?")

    for declaration in (
        "0 CONSTANT PT-SERIES-TIMESTAMP-EXPLICIT",
        "1 CONSTANT PT-SERIES-TIMESTAMP-UNIFORM",
        "0x3000 CONSTANT _PT-M-SERIES-DEFINE",
        "0x3001 CONSTANT _PT-M-SERIES-APPEND",
        "0x3002 CONSTANT _PT-M-SERIES-REPLACE",
        "0x3003 CONSTANT _PT-M-SERIES-DROP",
    ):
        assert declaration in source
    assert "PT-SERIES-TIMESTAMP-EXPLICIT =" in mode
    assert "PT-SERIES-TIMESTAMP-UNIFORM = OR" in mode
    assert "PT-RETAINED-AVAILABLE? 0=" in feature
    assert "_PT-RET-SERIES AND 0<>" in feature

    assert (
        "\\ Stack: owner generation series capacity timestamp-mode interval-us\n"
        "\\        session -- status\n"
        ": PT-SERIES-DEFINE"
    ) in source
    for word in ("PT-SERIES-APPEND", "PT-SERIES-REPLACE"):
        assert (
            "\\ Stack: owner generation series timestamp-mode "
            "first-timestamp-us\n"
            "\\        samples-a samples-u session -- status\n"
            f": {word}"
        ) in source
    assert (
        ": PT-SERIES-DROP  "
        "( owner generation series session -- status )"
    ) in source

    # The guest surface borrows exactly the caller's native-cell span.  It
    # allocates no retained sample cache and introduces no local policy cap.
    series = source[
        source.index("\\ Bounded SERIES is a renderer-neutral") :
        source.index("\\ GLYPH_RUN is the complete styled-cell draw primitive")
    ]
    assert "CREATE " not in series
    assert "ALLOT" not in series
    assert " CONSTANT " not in series
    assert " MOVE" not in series
    assert "samples-a samples-u" in series
    assert "_PT.S.RET-FORMATS 28 + L@" in series
    assert "_PT.S.RET-FORMATS 32 + L@" in series


def test_series_define_is_feature_gated_and_exactly_encoded() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    fields = _definition(source, "_PT-SERIES-DEFINE-FIELDS?")
    write = _definition(source, "_PT-SERIES-DEFINE-WRITE")
    public = _definition(source, "PT-SERIES-DEFINE")

    assert "_PT-SERDEF-OWNER @ 0=" in fields
    assert "_PT-SERDEF-GENERATION @ 0= OR" in fields
    assert "_PT-SERDEF-ID @ 0= OR" in fields
    assert "_PT-SERDEF-CAPACITY @ DUP 0= SWAP _PT-U32? 0= OR" in fields
    assert "_PT.S.RET-FORMATS 32 + L@ U>" in fields
    assert "_PT-SERIES-MODE? 0=" in fields
    explicit = fields.index("PT-SERIES-TIMESTAMP-EXPLICIT = IF")
    uniform = fields.index("ELSE", explicit)
    assert "_PT-SERDEF-INTERVAL @ 0=" in fields[explicit:uniform]
    assert "_PT-SERDEF-INTERVAL @ 0<>" in fields[uniform:]

    assert "_PT-VALID-S? 0=" in write
    assert "_PT-OP-LOST?" in write
    assert "_PT-RET-SERIES? 0=" in write
    assert "_PT-SERDEF-TYPE @ _PT-M-SERIES-DEFINE <>" in write
    assert "_PT-SERIES-DEFINE-FIELDS? 0=" in write
    assert "_PT-SERDEF-TYPE @ _PT-PO-TYPE !" in write
    assert "40 _PT-PO-U !" in write
    assert "_PT-SERDEF-S @ _PT-PO-S !" in write
    admit = write.index("_PT-PO-ADMIT")
    for store in (
        "_PT-SERDEF-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-SERDEF-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-SERDEF-ID @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-SERDEF-CAPACITY @ _PT-FRAME-PAYLOAD 24 + L!",
        "_PT-SERDEF-MODE @ _PT-FRAME-PAYLOAD 28 + L!",
        "_PT-SERDEF-INTERVAL @ _PT-FRAME-PAYLOAD 32 + _PT-U64!",
    ):
        assert store in write
        assert admit < write.index(store)
    assert write.index("_PT-SERDEF-INTERVAL @", admit) < write.index("_PT-PO-SEND")
    assert "_PT-M-SERIES-DEFINE _PT-SERIES-DEFINE-WRITE" in public


def test_series_samples_are_checked_ordered_and_encoded_cell_by_cell() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    source_span = _definition(source, "_PT-SERIES-SOURCE?")
    ordered = _definition(source, "_PT-SERIES-EXPLICIT-ORDERED?")
    fields = _definition(source, "_PT-SERIES-SAMPLES-FIELDS?")
    payload = _definition(source, "_PT-SERIES-SAMPLES-PAYLOAD!")
    body = _definition(source, "_PT-SERIES-SAMPLES-BODY")
    write = _definition(source, "_PT-SERIES-SAMPLES-WRITE")
    scrub = _definition(source, "_PT-SERIES-SCRUB")
    append = _definition(source, "PT-SERIES-APPEND")
    replace = _definition(source, "PT-SERIES-REPLACE")

    # samples-u is an exact nonempty native-cell byte span.  Checked end
    # arithmetic and disjointness keep frame construction from overwriting
    # either the caller source or PT's own session state.
    assert "_PT-SS-A @ 0= _PT-SS-U @ 0= OR" in source_span
    assert "_PT-SS-A @ 1 CELLS 1- AND" in source_span
    assert "_PT-SS-A @ _PT-SS-U @ _PT-UADD?" in source_span
    assert "_PT-SS-END !" in source_span
    assert source_span.count("_PT-RANGES-OVERLAP?") == 2
    assert "/PT-SESSION" in source_span
    assert "_PT.S.TX-A @" in source_span
    assert "_PT.S.TX-U @" in source_span

    explicit = fields.index("PT-SERIES-TIMESTAMP-EXPLICIT = IF")
    uniform = fields.index("ELSE", explicit)
    maxima = fields.index("_PT.S.RET-FORMATS 28 + L@", uniform)
    assert "_PT-SS-FIRST @ IF FALSE EXIT THEN" in fields[explicit:uniform]
    assert "16 _PT-SS-STRIDE !" in fields[explicit:uniform]
    assert "8 _PT-SS-STRIDE !" in fields[uniform:maxima]
    assert "_PT-SS-STRIDE @ _PT-UMUL?" in fields
    assert "_PT-SS-MAX-BYTES !" in fields
    assert "_PT-SS-U @ U<" in fields
    assert "_PT-SS-U @ _PT-SS-STRIDE @ MOD" in fields
    assert "_PT-SS-U @ _PT-SS-STRIDE @ / DUP _PT-SS-COUNT !" in fields
    assert "DUP 0= SWAP _PT-U32? 0= OR" in fields
    assert "_PT.S.RET-FORMATS 32 + L@ U>" in fields
    assert "_PT-SS-U @ 40 _PT-UADD?" in fields
    assert "_PT-SS-PAYLOAD-U !" in fields
    assert "_PT-SERIES-SOURCE? 0=" in fields
    assert "_PT-SERIES-EXPLICIT-ORDERED?" in fields

    assert "_PT-SS-A @ @ _PT-SS-LAST-TIMESTAMP !" in ordered
    assert "_PT-SS-A @ I 2* CELLS + @" in ordered
    assert "_PT-SS-LAST-TIMESTAMP @ U> 0=" in ordered
    assert "FALSE UNLOOP EXIT" in ordered

    # Both sample representations are serialized from native cells.  A raw
    # MOVE would accidentally make caller memory layout part of the wire ABI.
    for store in (
        "_PT-SS-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-SS-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-SS-ID @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
        "_PT-SS-COUNT @ _PT-FRAME-PAYLOAD 24 + L!",
        "_PT-SS-MODE @ _PT-FRAME-PAYLOAD 28 + L!",
        "_PT-SS-FIRST @ _PT-FRAME-PAYLOAD 32 + _PT-U64!",
    ):
        assert store in payload
    assert "_PT-SS-A @ I 2* CELLS + DUP @" in payload
    assert "_PT-FRAME-PAYLOAD 40 + I 16 * + _PT-U64!" in payload
    assert "1 CELLS + @" in payload
    assert "_PT-FRAME-PAYLOAD 48 + I 16 * + _PT-U64!" in payload
    assert "_PT-SS-A @ I CELLS + @" in payload
    assert "_PT-FRAME-PAYLOAD 40 + I 8 * + _PT-U64!" in payload
    assert "MOVE" not in payload

    assert "_PT-RET-SERIES? 0=" in body
    assert "_PT-M-SERIES-APPEND =" in body
    assert "_PT-M-SERIES-REPLACE = OR" in body
    assert "_PT-SERIES-SAMPLES-FIELDS? 0=" in body
    assert "_PT-SS-PAYLOAD-U @ _PT-PO-U !" in body
    assert body.index("_PT-PO-ADMIT") < body.index(
        "_PT-SERIES-SAMPLES-PAYLOAD!"
    ) < body.index("_PT-PO-SEND")
    assert "CATCH" in write
    assert "_PT-SERIES-SCRUB" in write
    for pointer in (
        "_PT-SS-A",
        "_PT-SS-U",
        "_PT-SS-END",
        "_PT-SS-LAST-TIMESTAMP",
        "_PT-SS-MAX-BYTES",
    ):
        assert f"0 {pointer} !" in scrub
    assert "_PT-M-SERIES-APPEND _PT-SERIES-SAMPLES-WRITE" in append
    assert "_PT-M-SERIES-REPLACE _PT-SERIES-SAMPLES-WRITE" in replace


def test_series_drop_and_shared_present_accounting_are_exact() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    define = _definition(source, "_PT-SERIES-DEFINE-WRITE")
    samples = _definition(source, "_PT-SERIES-SAMPLES-BODY")
    drop = _definition(source, "PT-SERIES-DROP")
    admit = _definition(source, "_PT-PO-ADMIT")
    send = _definition(source, "_PT-PO-SEND")

    assert "_PT-VALID-S? 0=" in drop
    assert "_PT-OP-LOST?" in drop
    assert "_PT-RET-SERIES? 0=" in drop
    assert "_PT-SDROP-OWNER @ 0=" in drop
    assert "_PT-SDROP-GENERATION @ 0= OR" in drop
    assert "_PT-SDROP-ID @ 0= OR" in drop
    assert "_PT-M-SERIES-DROP _PT-PO-TYPE !" in drop
    assert "24 _PT-PO-U !" in drop
    admitted = drop.index("_PT-PO-ADMIT")
    for store in (
        "_PT-SDROP-OWNER @ _PT-FRAME-PAYLOAD _PT-U64!",
        "_PT-SDROP-GENERATION @ _PT-FRAME-PAYLOAD 8 + _PT-U64!",
        "_PT-SDROP-ID @ _PT-FRAME-PAYLOAD 16 + _PT-U64!",
    ):
        assert store in drop
        assert admitted < drop.index(store)
    assert drop.index("_PT-SDROP-ID @", admitted) < drop.index("_PT-PO-SEND")

    for writer in (define, samples, drop):
        assert "_PT-PO-ADMIT" in writer
        assert "_PT-PO-SEND" in writer
    for gate in (
        "_PT.S.TX-OPEN? @ 0=",
        "_PT.S.TX-KIND @ _PT-TX-PRESENT <>",
        "_PT.S.TX-RET-MODE @ PT-RET-NONE =",
        "_PT.S.SPAN-REMAIN @",
        "_PT.S.TX-SPANS-DONE @",
        "_PT.S.TX-CELLS-DONE @",
        "_PT.S.CURSOR-DONE? @",
    ):
        assert gate in admit
    assert "_PT.S.TX-RET-OPS-DONE @ 1 _PT-UADD?" in admit
    assert "_PT.S.TX-RET-OPS @ U>" in admit
    assert "_PT-PO-U @ 40 _PT-UADD?" in admit
    assert "_PT.S.TX-RET-BYTES-DONE @ SWAP _PT-UADD?" in admit
    assert "_PT.S.TX-RET-BYTES @ U>" in admit
    assert "_PT-PO-TYPE @ _PT-PO-U @ _PT-PO-S @ _PT-FRAME-BEGIN" in admit
    queued = send.index("TRUE _PT-PO-S @ _PT-FRAME-QUEUE")
    ops = send.index("_PT.S.TX-RET-OPS-DONE !", queued)
    byte_count = send.index("_PT.S.TX-RET-BYTES-DONE !", ops)
    assert queued < ops < byte_count


def test_soft_reset_aborts_the_old_epoch_upload_before_reset_ack() -> None:
    source = SOURCE.read_text(encoding="utf-8")
    apply_reset = _definition(source, "_PT-APPLY-PENDING-RESET")
    soft_reset = _definition(source, "_PT-DISPATCH-SOFT-RESET")
    tracked_abort = _definition(source, "_PT-RESOURCE-ABORT-TRACKED")
    ret_result = _definition(source, "_PT-DISPATCH-RET-RESULT")
    ret_reset = _definition(source, "_PT-RET-RESET")

    blockers = apply_reset.index("_PT.S.LIFE-AWAIT? @ OR")
    completion = apply_reset.index("_PT.S.COMPLETE? @ OR", blockers)
    # A previously latched CLOSE owns the only legal frame at the final
    # sequence number.  That narrow path subsumes reset (and CLOSE teardown
    # destroys the upload), because a RESOURCE_ABORT cannot be encoded there.
    close = apply_reset.index("_PT.S.CLOSE-PENDING? @", completion)
    final_slot = apply_reset.index(
        "_PT.S.TX-SEQ @ 0xFFFFFFFFFFFFFFFF =", close
    )
    subsume_pending = apply_reset.index("_PT.S.RESET-PENDING? OFF", final_slot)
    subsume_epoch = apply_reset.index("_PT.S.RESET-EPOCH OFF", subsume_pending)
    subsume_exit = apply_reset.index("PT-S-OK EXIT", subsume_epoch)
    upload = apply_reset.index("_PT.S.UPLOAD? @ IF", subsume_exit)
    reason = apply_reset.index("PT-RESOURCE-ABORT-RESET-REBUILD", upload)
    abort = apply_reset.index("_PT-RESOURCE-ABORT-TRACKED", reason)
    epoch = apply_reset.index("_PT.S.RESET-EPOCH @ OVER _PT.S.EPOCH !", abort)
    reset_state = apply_reset.index("_PT-RET-RESET", epoch)
    ack = apply_reset.index("_PT-SEND-RESET-ACK", reset_state)
    assert blockers < completion < close < final_slot
    assert final_slot < subsume_pending < subsume_epoch < subsume_exit < upload
    assert upload < reason < abort < epoch < reset_state < ack
    assert "DROP PT-S-OK EXIT" in apply_reset[abort:epoch]

    # The valid request is latched while the old epoch is still authoritative;
    # applying it either waits on an existing lifecycle result/completion or
    # creates the exact tracked abort result wait above.
    latch_epoch = soft_reset.index("_PT.S.RESET-EPOCH !")
    latch_pending = soft_reset.index("TRUE OVER _PT.S.RESET-PENDING? !", latch_epoch)
    apply = soft_reset.index("_PT-APPLY-PENDING-RESET", latch_pending)
    assert latch_epoch < latch_pending < apply
    assert "FALSE _PT-URA-S @ _PT-FRAME-SEND" in tracked_abort
    assert "_PT-M-RESOURCE-ABORT _PT-URA-S @ _PT.S.LIFE-TYPE !" in tracked_abort
    abort_result = ret_result.index("_PT-M-RESOURCE-ABORT = IF")
    assert ret_result.index("_PT-UPLOAD-CLEAR", abort_result) < ret_result.index(
        "_PT-COMPLETE-RET!", abort_result
    )
    assert "DUP _PT-UPLOAD-CLEAR" in ret_reset


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
