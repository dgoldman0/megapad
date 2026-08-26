"""Lightweight conformance tests for the production APT-1 wire codec."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from rich_terminal.apt1 import (
    Frame,
    FrameEncoder,
    FramingErrorCode,
    IncrementalFrameDecoder,
    MessageType,
    NegotiationError,
    Offer,
    OpenRequest,
    Probe,
    SessionFramingError,
    crc32c,
    encode_frame,
    encode_offer,
    encode_open,
    encode_probe,
    parse_negotiation,
    snapshot_wire_bytes,
)


ROOT = Path(__file__).resolve().parents[1]
VECTORS = ROOT / "conformance" / "apt1"


def _manifest() -> dict:
    return json.loads((VECTORS / "manifest.json").read_text(encoding="utf-8"))


def _malformed(name: str) -> bytes:
    entry = next(item for item in _manifest()["transcripts"] if item["name"] == name)
    return bytes.fromhex(entry["full_hex"])


def test_crc_and_negotiation_are_contract_exact():
    assert crc32c(b"123456789") == 0xE3069283
    assert snapshot_wire_bytes(2, 2) == 312

    probe = encode_probe(0x0123456789ABCDEF)
    assert probe == b"\x1b]9999;APT1;P;0123456789ABCDEF;CELL1\x1b\\"
    assert parse_negotiation(probe) == Probe(0x0123456789ABCDEF)

    offer = Offer(
        nonce=0x0123456789ABCDEF,
        session_id=0xFEDCBA9876543210,
        max_payload=0x1000,
        max_transaction=0x10000,
        terminal_receive_credit=0x20000,
        cols=80,
        rows=24,
    )
    assert parse_negotiation(encode_offer(offer)) == offer

    request = OpenRequest(
        nonce=offer.nonce,
        session_id=offer.session_id,
        client_max_payload=0x1000,
        client_receive_credit=0x2000,
    )
    assert parse_negotiation(encode_open(request)) == request

    with pytest.raises(NegotiationError, match="uppercase"):
        parse_negotiation(probe.replace(b"ABCDEF", b"abcdef"))
    with pytest.raises(NegotiationError, match="maximum-width"):
        encode_offer(
            Offer(
                nonce=1,
                session_id=2,
                max_payload=64,
                max_transaction=100_000,
                terminal_receive_credit=100_000,
                cols=80,
                rows=24,
            )
        )


def test_encoder_matches_the_normative_first_frame():
    manifest = _manifest()
    first = manifest["transcripts"][0]["frames"][0]
    expected = bytes.fromhex(first["full_hex"])
    header = first["expected"]["header"]

    frame = Frame(
        message_type=int(header["type"], 16),
        session_id=int(header["session"], 16),
        sequence=header["sequence"],
        presentation_epoch=header["presentation_epoch"],
        payload=expected[40:],
    )
    assert encode_frame(frame) == expected

    encoder = FrameEncoder(
        frame.session_id,
        max_payload=1_048_576,
        presentation_epoch=frame.presentation_epoch,
    )
    assert encoder.encode(frame.message_type, frame.payload) == expected
    assert encoder.next_sequence == 1


def test_reserved_header_byte_is_zero_and_nonzero_is_fatal():
    encoded = encode_frame(
        Frame(
            message_type=MessageType.SERVER_READY,
            session_id=0x0123456789ABCDEF,
            sequence=0,
            presentation_epoch=0,
            payload=b"",
        )
    )
    assert encoded[4] == 0

    malformed = bytearray(encoded)
    malformed[4] = 1
    decoder = IncrementalFrameDecoder(
        0x0123456789ABCDEF,
        max_payload=1_048_576,
    )
    with pytest.raises(SessionFramingError) as caught:
        decoder.feed(malformed)
    assert caught.value.code is FramingErrorCode.BAD_RESERVED
    assert decoder.failed
    assert decoder.buffered_bytes == 0


def test_incremental_decoders_accept_the_full_bidirectional_happy_transcript():
    manifest = _manifest()
    transcript = next(
        item for item in manifest["transcripts"] if item["name"] == "happy_session"
    )
    session = int(transcript["frames"][0]["expected"]["header"]["session"], 16)
    decoders = {
        "server_to_client": IncrementalFrameDecoder(
            session, max_payload=manifest["max_payload"]
        ),
        "client_to_server": IncrementalFrameDecoder(
            session, max_payload=manifest["max_payload"]
        ),
    }

    decoded: list[Frame] = []
    for item in transcript["frames"]:
        direction = item["direction"]
        message = item["message"]
        decoder = decoders[direction]
        if message == "SOFT_RESET_ACK":
            decoder.expect_epoch_transition(MessageType.SOFT_RESET_ACK, 1)

        raw = bytes.fromhex(item["full_hex"])
        # Exercise arbitrary UART publication boundaries, including headers.
        produced = ()
        for position in range(0, len(raw), 7):
            part = decoder.feed(raw[position : position + 7])
            assert not produced or not part
            if part:
                produced = part
        assert len(produced) == 1
        frame = produced[0]
        decoded.append(frame)
        expected_header = item["expected"]["header"]
        assert frame.message_type == int(expected_header["type"], 16)
        assert frame.sequence == expected_header["sequence"]
        assert frame.presentation_epoch == expected_header["presentation_epoch"]
        assert frame.payload == raw[40:]

        if message == "SOFT_RESET_REQUEST":
            decoder.advance_presentation_epoch(1)

    assert len(decoded) == 17
    assert decoders["server_to_client"].presentation_epoch == 1
    assert decoders["client_to_server"].presentation_epoch == 1


@pytest.mark.parametrize(
    ("name", "kwargs", "code"),
    [
        ("bad_crc", {}, FramingErrorCode.BAD_CRC32C),
        ("oversized_length", {}, FramingErrorCode.PAYLOAD_TOO_LARGE),
        ("sequence_gap", {"expected_sequence": 1}, FramingErrorCode.SEQUENCE_GAP),
        (
            "stale_epoch",
            {"expected_sequence": 12, "presentation_epoch": 1},
            FramingErrorCode.STALE_PRESENTATION_EPOCH,
        ),
    ],
)
def test_malformed_vectors_fail_closed(name, kwargs, code):
    decoder = IncrementalFrameDecoder(
        0x0123456789ABCDEF,
        max_payload=1_048_576,
        **kwargs,
    )
    with pytest.raises(SessionFramingError) as caught:
        decoder.feed(_malformed(name))
    assert caught.value.code is code
    assert decoder.failed
    assert decoder.buffered_bytes == 0

    with pytest.raises(SessionFramingError) as repeated:
        decoder.feed(b"")
    assert repeated.value.code is FramingErrorCode.SESSION_ALREADY_FAILED
