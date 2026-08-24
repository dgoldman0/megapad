#!/usr/bin/env python3
"""Validate the normative APT-1 CELL-1 byte transcripts.

The encoder and transcript descriptions here deliberately do not consume the
manifest's ``full_hex`` values when constructing frames.  That makes the
manifest and textual transcripts independently checkable without third-party
packages.
"""

from __future__ import annotations

import json
import struct
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


CONTRACT_ID = "APT-1-CELL-1-2026-08-24"
MAGIC = b"\xa5PT1"
VERSION = 1
HEADER_BYTES = 40
MAX_PAYLOAD = 1_048_576
SESSION = 0x0123456789ABCDEF
CAPABILITIES = 0x3F

MESSAGE_TYPES = {
    "SERVER_READY": 0x0001,
    "CLIENT_READY": 0x0002,
    "CREDIT": 0x0003,
    "ERROR": 0x0004,
    "CLOSE": 0x0005,
    "CLOSE_ACK": 0x0006,
    "SOFT_RESET_REQUEST": 0x0007,
    "SOFT_RESET_ACK": 0x0008,
    "TX_RESULT": 0x0009,
    "TX_BEGIN": 0x0100,
    "CELL_SPAN": 0x0101,
    "CURSOR": 0x0102,
    "TX_COMMIT": 0x0103,
    "TX_ABORT": 0x0104,
    "SNAPSHOT_BEGIN": 0x0110,
    "SNAPSHOT_COMMIT": 0x0111,
    "KEY": 0x0200,
    "TEXT": 0x0201,
    "POINTER": 0x0202,
    "RESIZE": 0x0203,
    "FOCUS": 0x0204,
}

HEADER_PREFIX = struct.Struct("<4sBBHHHIQQI")
HEADER = struct.Struct("<4sBBHHHIQQII")
READY = struct.Struct("<IIIIIIQ")
SNAPSHOT_BEGIN = struct.Struct("<QQIIII")
CELL_SPAN_PREFIX = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
COMMIT = struct.Struct("<Q")
KEY = struct.Struct("<IBBHQ")
SOFT_RESET_REQUEST = struct.Struct("<I4xQ")
SOFT_RESET_ACK = struct.Struct("<IHH")
TX_RESULT = struct.Struct("<QHHQ")


def crc32c(data: bytes) -> int:
    """Return CRC-32C (Castagnoli), reflected, init/final xor all ones."""

    crc = 0xFFFFFFFF
    for byte in data:
        crc ^= byte
        for _ in range(8):
            crc = (crc >> 1) ^ (0x82F63B78 if crc & 1 else 0)
    return crc ^ 0xFFFFFFFF


def encode_frame(
    message: str,
    payload: bytes,
    sequence: int,
    presentation_epoch: int,
    *,
    session: int = SESSION,
) -> bytes:
    if len(payload) > MAX_PAYLOAD:
        raise ValueError("canonical payload exceeds the contract maximum")
    prefix = HEADER_PREFIX.pack(
        MAGIC,
        VERSION,
        HEADER_BYTES,
        MESSAGE_TYPES[message],
        0,
        0,
        len(payload),
        session,
        sequence,
        presentation_epoch,
    )
    checksum = crc32c(prefix + payload)
    return prefix + struct.pack("<I", checksum) + payload


def server_ready_payload() -> bytes:
    return READY.pack(
        1,
        MAX_PAYLOAD,
        MAX_PAYLOAD,
        MAX_PAYLOAD,
        2,
        2,
        CAPABILITIES,
    )


def client_ready_payload() -> bytes:
    return READY.pack(
        1,
        MAX_PAYLOAD,
        0,
        MAX_PAYLOAD,
        4_096,
        0,
        CAPABILITIES,
    )


CELLS = (
    {"codepoint": 0x41, "fg": 7, "bg": 0, "attributes": 0x0001},
    {"codepoint": 0x42, "fg": 2, "bg": 0, "attributes": 0x0008},
    {"codepoint": 0x43, "fg": 4, "bg": 0, "attributes": 0x0000},
    {"codepoint": 0x20, "fg": 7, "bg": 1, "attributes": 0x0020},
)


def snapshot_begin_payload(transaction_id: int, base_revision: int) -> bytes:
    return SNAPSHOT_BEGIN.pack(transaction_id, base_revision, 2, 2, 2, 4)


def span_payload(row: int, cells: Iterable[dict[str, int]]) -> bytes:
    cell_list = tuple(cells)
    prefix = CELL_SPAN_PREFIX.pack(row, 0, len(cell_list))
    encoded = b"".join(
        CELL.pack(cell["codepoint"], cell["fg"], cell["bg"], cell["attributes"])
        for cell in cell_list
    )
    return prefix + encoded


def cursor_payload() -> bytes:
    return CURSOR.pack(1, 1, 1)


@dataclass(frozen=True)
class FrameSpec:
    direction: str
    message: str
    sequence: int
    epoch: int
    payload: bytes
    decoded_payload: dict[str, Any]
    state_from: str
    state_to: str
    effect: str


def frame_specs() -> tuple[FrameSpec, ...]:
    server_ready = {
        "profile": 1,
        "terminal_receive_max_payload": MAX_PAYLOAD,
        "max_transaction_bytes": MAX_PAYLOAD,
        "terminal_receive_credit": MAX_PAYLOAD,
        "current_cols": 2,
        "current_rows": 2,
        "capabilities": f"0x{CAPABILITIES:016x}",
    }
    client_ready = {
        "profile": 1,
        "client_receive_max_payload": MAX_PAYLOAD,
        "reserved_after_max_payload": 0,
        "client_receive_credit": MAX_PAYLOAD,
        "max_text_event_bytes": 4_096,
        "reserved_after_max_text_event_bytes": 0,
        "capabilities": f"0x{CAPABILITIES:016x}",
    }
    first_begin = {
        "transaction_id": 1,
        "base_revision": 0,
        "columns": 2,
        "rows": 2,
        "span_count": 2,
        "cell_count": 4,
    }
    row_zero = {"row": 0, "column": 0, "count": 2, "cells": list(CELLS[:2])}
    row_one = {"row": 1, "column": 0, "count": 2, "cells": list(CELLS[2:])}
    cursor = {"row": 1, "column": 1, "visible": True}

    return (
        FrameSpec("server_to_client", "SERVER_READY", 0, 0,
                  server_ready_payload(), server_ready,
                  "OPENING", "ACTIVE", "client accepts server limits and becomes active"),
        FrameSpec("client_to_server", "CLIENT_READY", 0, 0,
                  client_ready_payload(), client_ready,
                  "OPENING", "ACTIVE", "terminal accepts client limits and becomes active"),
        FrameSpec("client_to_server", "SNAPSHOT_BEGIN", 1, 0,
                  snapshot_begin_payload(1, 0), first_begin,
                  "ACTIVE", "STAGING_SNAPSHOT", "stage a bounded replace-all snapshot"),
        FrameSpec("client_to_server", "CELL_SPAN", 2, 0,
                  span_payload(0, CELLS[:2]), row_zero,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "stage row zero"),
        FrameSpec("client_to_server", "CELL_SPAN", 3, 0,
                  span_payload(1, CELLS[2:]), row_one,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "stage row one"),
        FrameSpec("client_to_server", "CURSOR", 4, 0, cursor_payload(), cursor,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "stage cursor state"),
        FrameSpec("client_to_server", "SNAPSHOT_COMMIT", 5, 0, COMMIT.pack(1),
                  {"transaction_id": 1},
                  "STAGING_SNAPSHOT", "ACTIVE", "atomically publish presentation revision 1"),
        FrameSpec("server_to_client", "TX_RESULT", 1, 0,
                  TX_RESULT.pack(1, 0, 0, 1),
                  {"transaction_id": 1, "status": 0, "reserved": 0,
                   "model_revision": 1},
                  "ACTIVE", "ACTIVE", "confirm presentation revision 1 to the client"),
        FrameSpec("server_to_client", "KEY", 2, 0, KEY.pack(0x41, 1, 0, 1, 1),
                  {"key_symbol": 65, "action": 1, "location": 0, "modifiers": 1,
                   "model_revision": 1},
                  "ACTIVE", "ACTIVE", "deliver one normalized key press"),
        FrameSpec("server_to_client", "SOFT_RESET_REQUEST", 3, 0,
                  SOFT_RESET_REQUEST.pack(1, 1),
                  {"requested_epoch": 1, "last_revision": 1},
                  "ACTIVE", "RESYNCING", "discard presentation cache and request replace-all"),
        FrameSpec("client_to_server", "SOFT_RESET_ACK", 6, 1,
                  SOFT_RESET_ACK.pack(1, 0, 0),
                  {"requested_epoch": 1, "status": 0, "reserved": 0},
                  "RESYNCING", "RESYNCING", "accept epoch 1 and prepare rebuild"),
        FrameSpec("client_to_server", "SNAPSHOT_BEGIN", 7, 1,
                  snapshot_begin_payload(1, 0), first_begin,
                  "RESYNCING", "STAGING_SNAPSHOT", "stage epoch 1 replace-all snapshot"),
        FrameSpec("client_to_server", "CELL_SPAN", 8, 1,
                  span_payload(0, CELLS[:2]), row_zero,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "restage row zero"),
        FrameSpec("client_to_server", "CELL_SPAN", 9, 1,
                  span_payload(1, CELLS[2:]), row_one,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "restage row one"),
        FrameSpec("client_to_server", "CURSOR", 10, 1, cursor_payload(), cursor,
                  "STAGING_SNAPSHOT", "STAGING_SNAPSHOT", "restage cursor state"),
        FrameSpec("client_to_server", "SNAPSHOT_COMMIT", 11, 1, COMMIT.pack(1),
                  {"transaction_id": 1},
                  "STAGING_SNAPSHOT", "ACTIVE", "atomically publish presentation revision 1"),
        FrameSpec("server_to_client", "TX_RESULT", 4, 1,
                  TX_RESULT.pack(1, 0, 0, 1),
                  {"transaction_id": 1, "status": 0, "reserved": 0,
                   "model_revision": 1},
                  "ACTIVE", "ACTIVE", "confirm rebuilt presentation revision 1"),
    )


def make_oversized_header() -> bytes:
    return HEADER.pack(
        MAGIC,
        VERSION,
        HEADER_BYTES,
        MESSAGE_TYPES["CELL_SPAN"],
        0,
        0,
        MAX_PAYLOAD + 1,
        SESSION,
        1,
        0,
        0,
    )


def canonical_transcripts() -> dict[str, dict[str, Any]]:
    happy_specs = frame_specs()
    happy_frames = [encode_frame(s.message, s.payload, s.sequence, s.epoch) for s in happy_specs]

    good_client_ready = encode_frame("CLIENT_READY", client_ready_payload(), 0, 0)
    # CRC is in the header at byte 36, not at the end of a frame with payload.
    bad_crc = bytearray(good_client_ready)
    bad_crc[36] ^= 1

    sequence_gap_payload = snapshot_begin_payload(1, 0)
    sequence_gap = encode_frame("SNAPSHOT_BEGIN", sequence_gap_payload, 2, 0)
    stale_epoch_payload = snapshot_begin_payload(1, 0)
    stale_epoch = encode_frame("SNAPSHOT_BEGIN", stale_epoch_payload, 12, 0)

    return {
        "happy_session": {
            "file": "happy_session.hex",
            "kind": "happy_path",
            "bytes": b"".join(happy_frames),
            "frames": tuple(zip(happy_specs, happy_frames)),
            "initial_state": "OPENING",
            "final_state": "ACTIVE",
            "expected_revision": 1,
            "expected_epoch": 1,
        },
        "bad_crc": {
            "file": "bad_crc.hex",
            "kind": "malformed",
            "bytes": bytes(bad_crc),
            "expected_error": "BAD_CRC32C",
            "expected_disposition": "terminate session; restore ANSI only after acknowledged close or hard attachment reset",
            "context": {"state": "OPENING", "direction": "client_to_server"},
        },
        "sequence_gap": {
            "file": "sequence_gap.hex",
            "kind": "malformed",
            "bytes": sequence_gap,
            "expected_error": "SEQUENCE_GAP",
            "expected_disposition": "terminate session; restore ANSI only after acknowledged close or hard attachment reset",
            "context": {
                "state": "ACTIVE",
                "direction": "client_to_server",
                "expected_sequence": 1,
                "received_sequence": 2,
            },
        },
        "oversized_length": {
            "file": "oversized_length.hex",
            "kind": "malformed",
            "bytes": make_oversized_header(),
            "expected_error": "PAYLOAD_TOO_LARGE",
            "expected_disposition": "reject before payload allocation and terminate session; restore ANSI only after acknowledged close or hard attachment reset",
            "context": {"declared_payload_length": MAX_PAYLOAD + 1, "available_payload_bytes": 0},
        },
        "stale_epoch": {
            "file": "stale_epoch.hex",
            "kind": "malformed",
            "bytes": stale_epoch,
            "expected_error": "STALE_PRESENTATION_EPOCH",
            "expected_disposition": "terminate session; restore ANSI only after acknowledged close or hard attachment reset",
            "context": {
                "state": "ACTIVE",
                "direction": "client_to_server",
                "current_epoch": 1,
                "received_epoch": 0,
            },
        },
    }


def compact_hex(data: bytes) -> str:
    return data.hex()


def read_hex_lines(path: Path) -> tuple[bytes, ...]:
    text_lines = tuple(line.strip() for line in path.read_text(encoding="ascii").splitlines())
    if not text_lines or any(not line for line in text_lines):
        raise ValueError(f"{path.name}: empty transcript")
    decoded = []
    for line_number, line in enumerate(text_lines, start=1):
        if len(line) % 2:
            raise ValueError(
                f"{path.name}:{line_number}: odd number of hexadecimal digits"
            )
        try:
            decoded.append(bytes.fromhex(line))
        except ValueError as exc:
            raise ValueError(
                f"{path.name}:{line_number}: non-hexadecimal transcript content"
            ) from exc
    return tuple(decoded)


def decode_header(frame: bytes) -> dict[str, Any]:
    if len(frame) < HEADER_BYTES:
        raise ValueError("frame shorter than the fixed header")
    magic, version, header_bytes, type_id, flags, reserved, payload_len, session, sequence, epoch, checksum = HEADER.unpack_from(frame)
    return {
        "magic": magic.hex(),
        "version": version,
        "header_bytes": header_bytes,
        "type": f"0x{type_id:04x}",
        "flags": flags,
        "reserved": reserved,
        "payload_length": payload_len,
        "session": f"0x{session:016x}",
        "sequence": sequence,
        "presentation_epoch": epoch,
        "crc32c": f"0x{checksum:08x}",
    }


def expected_frame_entry(spec: FrameSpec, frame: bytes) -> dict[str, Any]:
    return {
        "direction": spec.direction,
        "message": spec.message,
        "full_hex": compact_hex(frame),
        "expected": {
            "header": decode_header(frame),
            "payload": spec.decoded_payload,
            "state_transition": {
                "from": spec.state_from,
                "to": spec.state_to,
                "effect": spec.effect,
            },
        },
    }


def expected_manifest_transcript(name: str, canonical: dict[str, Any]) -> dict[str, Any]:
    base: dict[str, Any] = {
        "name": name,
        "file": canonical["file"],
        "kind": canonical["kind"],
        "full_hex": compact_hex(canonical["bytes"]),
    }
    if canonical["kind"] == "happy_path":
        base.update({
            "initial_state": canonical["initial_state"],
            "frames": [expected_frame_entry(spec, frame) for spec, frame in canonical["frames"]],
            "expected_final": {
                "state": canonical["final_state"],
                "presentation_revision": canonical["expected_revision"],
                "presentation_epoch": canonical["expected_epoch"],
            },
        })
    else:
        base.update({
            "context": canonical["context"],
            "expected_error": canonical["expected_error"],
            "expected_disposition": canonical["expected_disposition"],
        })
        if len(canonical["bytes"]) >= HEADER_BYTES:
            base["expected_header"] = decode_header(canonical["bytes"])
    return base


def fail(message: str) -> None:
    raise AssertionError(message)


def check_equal(label: str, actual: Any, expected: Any) -> None:
    if actual != expected:
        fail(f"{label} differs\nexpected: {expected!r}\nactual:   {actual!r}")


def validate_frame_bytes(label: str, frame: bytes, *, allow_invalid_crc: bool = False) -> None:
    header = decode_header(frame)
    check_equal(f"{label} magic", header["magic"], MAGIC.hex())
    check_equal(f"{label} version", header["version"], VERSION)
    check_equal(f"{label} header size", header["header_bytes"], HEADER_BYTES)
    check_equal(f"{label} flags", header["flags"], 0)
    check_equal(f"{label} reserved", header["reserved"], 0)
    payload_len = header["payload_length"]
    check_equal(f"{label} total size", len(frame), HEADER_BYTES + payload_len)
    if payload_len > MAX_PAYLOAD:
        fail(f"{label}: well-formed canonical frame exceeds max payload")
    actual_crc = struct.unpack_from("<I", frame, 36)[0]
    expected_crc = crc32c(frame[:36] + frame[40:])
    if allow_invalid_crc:
        if actual_crc == expected_crc:
            fail(f"{label}: bad-CRC sentinel unexpectedly has a valid checksum")
    else:
        check_equal(f"{label} CRC-32C", actual_crc, expected_crc)


def validate_manifest_metadata(manifest: dict[str, Any]) -> None:
    check_equal("contract_id", manifest.get("contract_id"), CONTRACT_ID)
    check_equal("byte_order", manifest.get("byte_order"), "little-endian")
    check_equal("max_payload", manifest.get("max_payload"), MAX_PAYLOAD)
    check_equal("message_types", manifest.get("message_types"), {
        name: f"0x{type_id:04x}" for name, type_id in MESSAGE_TYPES.items()
    })
    expected_header = {
        "size": HEADER_BYTES,
        "magic_hex": MAGIC.hex(),
        "version": VERSION,
        "layout": [
            "magic[4]", "version:u8", "header_bytes:u8", "type:u16",
            "flags:u16", "reserved:u16", "payload_len:u32", "session:u64",
            "directional_sequence:u64", "presentation_epoch:u32", "crc32c:u32",
        ],
        "crc_coverage": "header bytes 0..35 followed by payload",
        "crc_algorithm": "CRC-32C (Castagnoli), reflected 0x82f63b78, init/final xor 0xffffffff",
    }
    check_equal("header metadata", manifest.get("header"), expected_header)
    expected_negotiation = {
        "notation": "ESC denotes byte 0x1b; ST denotes ESC followed by backslash; bracketed fields are uppercase hexadecimal at the stated width",
        "probe": "ESC ] 9999;APT1;P;<nonce:16>;CELL1 ST",
        "offer": "ESC ] 9999;APT1;O;<nonce:16>;<session:16>;<max-payload:8>;<max-transaction:8>;<terminal-rx-credit:8>;<cols:4>;<rows:4>;CELL1 ST",
        "open": "ESC ] 9999;APT1;A;<nonce:16>;<session:16>;<client-max-payload:8>;<client-rx-credit:8>;CELL1 ST",
    }
    check_equal("negotiation text", manifest.get("negotiation"), expected_negotiation)
    if crc32c(b"123456789") != 0xE3069283:
        fail("validator's CRC-32C implementation failed its standard check value")


def validate() -> None:
    root = Path(__file__).resolve().parent
    manifest = json.loads((root / "manifest.json").read_text(encoding="utf-8"))
    validate_manifest_metadata(manifest)

    canonical = canonical_transcripts()
    expected_entries = [expected_manifest_transcript(name, data) for name, data in canonical.items()]
    check_equal("manifest transcript descriptions", manifest.get("transcripts"), expected_entries)

    for name, data in canonical.items():
        on_disk_lines = read_hex_lines(root / data["file"])
        expected_lines = (
            tuple(frame for _spec, frame in data["frames"])
            if data["kind"] == "happy_path"
            else (data["bytes"],)
        )
        check_equal(f"{name} frame-per-line layout", on_disk_lines, expected_lines)
        on_disk = b"".join(on_disk_lines)
        check_equal(f"{name} textual hex", on_disk, data["bytes"])

    next_sequence = {"server_to_client": 0, "client_to_server": 0}
    legal_states = {"ANSI", "PROBING", "OPENING", "ACTIVE", "RESYNCING", "CLOSING", "STAGING_SNAPSHOT"}
    for index, (spec, frame) in enumerate(canonical["happy_session"]["frames"]):
        label = f"happy_session frame {index} ({spec.message})"
        validate_frame_bytes(label, frame)
        decoded = decode_header(frame)
        check_equal(f"{label} type", decoded["type"], f"0x{MESSAGE_TYPES[spec.message]:04x}")
        check_equal(f"{label} session", decoded["session"], f"0x{SESSION:016x}")
        check_equal(f"{label} sequence", decoded["sequence"], next_sequence[spec.direction])
        check_equal(f"{label} presentation epoch", decoded["presentation_epoch"], spec.epoch)
        check_equal(f"{label} payload", frame[HEADER_BYTES:], spec.payload)
        if spec.state_from not in legal_states or spec.state_to not in legal_states:
            fail(f"{label}: state transition uses an unknown state")
        if not spec.effect:
            fail(f"{label}: state transition has no specified effect")
        next_sequence[spec.direction] += 1

    bad_crc = canonical["bad_crc"]["bytes"]
    validate_frame_bytes("bad_crc", bad_crc, allow_invalid_crc=True)

    gap = canonical["sequence_gap"]["bytes"]
    validate_frame_bytes("sequence_gap", gap)
    check_equal("sequence_gap sequence", decode_header(gap)["sequence"], 2)

    oversized = canonical["oversized_length"]["bytes"]
    check_equal("oversized sentinel header size", len(oversized), HEADER_BYTES)
    check_equal("oversized declared length", decode_header(oversized)["payload_length"], MAX_PAYLOAD + 1)

    stale = canonical["stale_epoch"]["bytes"]
    validate_frame_bytes("stale_epoch", stale)
    check_equal("stale epoch", decode_header(stale)["presentation_epoch"], 0)

    print(
        f"validated {len(canonical)} APT-1 transcript files, "
        f"{len(canonical['happy_session']['frames'])} happy-path frames, "
        f"contract {CONTRACT_ID}"
    )


if __name__ == "__main__":
    try:
        validate()
    except (AssertionError, KeyError, OSError, ValueError, json.JSONDecodeError) as exc:
        print(f"vector validation failed: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
