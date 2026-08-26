#!/usr/bin/env python3
"""Generate and independently validate APT-1 RETAINED-1 transcripts.

This module deliberately imports no MegaPad production encoder, decoder, or
model.  The layouts below are direct translations of the normative wire
documents, so the checked-in bytes remain an independent interoperability
oracle rather than a round-trip test of one implementation.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import struct
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Sequence

from oracle_vectors import check_oracles
from semantic_reducer import reduce_transcript


CONTRACT_ID = "APT-1-RETAINED-1-2026-08-24"
BASE_CONTRACT_ID = "APT-1-CELL-1-2026-08-24"
MAGIC = b"\xa5PT1"
HEADER_BYTES = 40
MAX_PAYLOAD = 1_048_576
SESSION = 0x0123456789ABCDEF

CLIENT = "client_to_terminal"
TERMINAL = "terminal_to_client"

MESSAGE_TYPES = {
    # Base frames used by these additive-profile journeys.
    "CREDIT": 0x0003,
    "SOFT_RESET_REQUEST": 0x0007,
    "SOFT_RESET_ACK": 0x0008,
    "TX_RESULT": 0x0009,
    "TX_BEGIN": 0x0100,
    "CELL_SPAN": 0x0101,
    "CURSOR": 0x0102,
    "TX_COMMIT": 0x0103,
    "SNAPSHOT_BEGIN": 0x0110,
    "SNAPSHOT_COMMIT": 0x0111,
    # RETAINED-1.
    "RET_RESULT": 0x000A,
    "OWNER_DROP": 0x000B,
    "RESOURCE_ABORT": 0x000C,
    "RESOURCE_BEGIN": 0x1000,
    "RESOURCE_CHUNK": 0x1001,
    "RESOURCE_COMMIT": 0x1002,
    "RESOURCE_DROP": 0x1003,
    "PRESENT_BEGIN": 0x2000,
    "PRESENT_COMMIT": 0x2001,
    "OWNER_OPEN": 0x2002,
    "REGION_DEFINE": 0x2010,
    "REGION_REPLACE": 0x2011,
    "REGION_DROP": 0x2012,
    "OBJECT_DEFINE": 0x2020,
    "OBJECT_REPLACE": 0x2021,
    "OBJECT_SET_VALUE": 0x2022,
    "OBJECT_SET_VISIBILITY": 0x2023,
    "OBJECT_DROP": 0x2024,
    "SERIES_DEFINE": 0x3000,
    "SERIES_APPEND": 0x3001,
    "SERIES_REPLACE": 0x3002,
    "SERIES_DROP": 0x3003,
    "RET_QUERY": 0x8000,
    "RET_CAPS": 0x8001,
    "RET_FORMATS": 0x8002,
}

RETAINED_MESSAGE_TYPES = {
    name: value
    for name, value in MESSAGE_TYPES.items()
    if name
    not in {
        "CREDIT",
        "SOFT_RESET_REQUEST",
        "SOFT_RESET_ACK",
        "TX_RESULT",
        "TX_BEGIN",
        "CELL_SPAN",
        "CURSOR",
        "TX_COMMIT",
        "SNAPSHOT_BEGIN",
        "SNAPSHOT_COMMIT",
    }
}

RESERVE_MESSAGES = {
    "CREDIT",
    "SOFT_RESET_REQUEST",
    "SOFT_RESET_ACK",
    "TX_RESULT",
    "RET_RESULT",
    "OWNER_DROP",
    "RESOURCE_ABORT",
}

STATUS = {
    "RET_OK": 0,
    "RET_INVALID": 1,
    "RET_STALE_OWNER": 2,
    "RET_NO_CAPACITY": 3,
    "RET_DUPLICATE_ID": 4,
    "RET_IN_USE": 5,
    "RET_BAD_CONTENT": 6,
    "RET_ABORTED": 7,
}

FEATURE_BITS = {
    "RET_CORE": 0,
    "RET_VECTOR": 1,
    "RET_RGBA_IMAGE": 2,
    "RET_INSTRUMENT": 3,
    "RET_SERIES": 4,
    "RET_CADENCE": 5,
}
FEATURES = sum(1 << bit for bit in FEATURE_BITS.values())

OBJECT_TYPES = {
    "GROUP": 1,
    "POLYLINE": 2,
    "IMAGE": 3,
    "GLYPH_RUN": 4,
    "READOUT": 5,
    "METER": 6,
    "STATUS": 7,
    "PLOT": 8,
    "WAVEFORM": 9,
}

TAG = 0x31544552
OWNER_ID = 0x534F554E444C4142
OWNER_GENERATION = 7
REGION_ID = 1
RESOURCE_ID = 1
SERIES_ID = 1
EXPLICIT_SERIES_ID = 2
GROUP_ID = 1
POLYLINE_ID = 2
GLYPH_RUN_ID = 3
READOUT_ID = 4
METER_ID = 5
STATUS_ID = 6
PLOT_ID = 7
WAVEFORM_ID = 8
IMAGE_ID = 1

BASE_MAX_TRANSACTION = 65_536
CLIENT_RECEIVE_CREDIT = 4_096
INITIAL_CLIENT_TO_TERMINAL_GRANT = 65_536
PRIOR_SNAPSHOT_COMPLETE_BYTES = 17_476
QUERY_CREDIT_BEFORE = INITIAL_CLIENT_TO_TERMINAL_GRANT + PRIOR_SNAPSHOT_COMPLETE_BYTES

CAPS_VALUES = {
    "max_owner_records": 8,
    "max_live_owners": 4,
    "max_regions": 16,
    "max_resources": 8,
    "max_objects": 64,
    "max_series": 16,
    "max_operations_per_transaction": 128,
    "max_resource_chunk_bytes": 4_096,
    "max_retained_transaction_bytes": 32_768,
    "total_resource_bytes": 1_048_576,
}

FORMAT_VALUES = {
    "coordinate_format": 1,
    "color_format": 1,
    "image_format": 1,
    "max_image_width": 256,
    "max_image_height": 256,
    "max_path_points": 256,
    "max_glyph_run_bytes": 256,
    "max_samples_per_append": 64,
    "max_history_per_series": 512,
    "minimum_presentation_interval_us": 500_000,
    "total_sample_slots": 4_096,
    "total_utf8_bytes": 16_384,
}

HEADER_PREFIX = struct.Struct("<4sBBHHHIQQI")
HEADER = struct.Struct("<4sBBHHHIQQII")
CREDIT = struct.Struct("<Q")
TX_RESULT = struct.Struct("<QHHQ")
SOFT_RESET_REQUEST = struct.Struct("<I4xQ")
SOFT_RESET_ACK = struct.Struct("<IHH")
SNAPSHOT_BEGIN = struct.Struct("<QQIIII")
TX_BEGIN = struct.Struct("<QQIIII")
CELL_SPAN_PREFIX = struct.Struct("<III")
CELL = struct.Struct("<IBBH")
CURSOR = struct.Struct("<IIB7x")
COMMIT_ID = struct.Struct("<Q")
TX_COMMIT = struct.Struct("<Q")

RET_QUERY = struct.Struct("<II")
RET_CAPS = struct.Struct("<IHHQIIIIIIIIQQ")
RET_FORMATS = struct.Struct("<IIIIIIIIIIQQQ")
RET_RESULT = struct.Struct("<HHIQQQQQ")
OWNER_OPEN = struct.Struct("<QQIIIIQQQQ")
PRESENT_BEGIN = struct.Struct("<QQQQIIIIIIII")
PRESENT_COMMIT = struct.Struct("<QII")
REGION = struct.Struct("<QQQIIIIiI")
OBJECT_PREFIX = struct.Struct("<QQQHHiQQIIII")
POLYLINE_BODY = struct.Struct("<II4BI")
POINT = struct.Struct("<II")
GLYPH_RUN_BODY = struct.Struct("<4B4BHHI")
READOUT_BODY = struct.Struct("<8BIIqqII")
METER_BODY = struct.Struct("<8BIIqqqQ")
STATUS_BODY = struct.Struct("<8BqIIQ")
PLOT_BODY = struct.Struct("<Qqq8BII")
WAVEFORM_BODY = struct.Struct("<Qqq8BqII")
IMAGE_BODY = struct.Struct("<QIB3x")
OBJECT_SET_VALUE = struct.Struct("<QQQq")
OBJECT_SET_VISIBILITY = struct.Struct("<QQQB7x")
SERIES_DEFINE = struct.Struct("<QQQIIQ")
SERIES_SAMPLES = struct.Struct("<QQQIIQ")
RESOURCE_BEGIN = struct.Struct("<QQQIIIIQ32s")
RESOURCE_CHUNK = struct.Struct("<QQQQ")
RESOURCE_COMMIT = struct.Struct("<QQQ")
RESOURCE_ABORT = struct.Struct("<QQQH6x")
OWNER_DROP = struct.Struct("<QQQQ")
OWNER_ITEM = struct.Struct("<QQQ")


def crc32c(data: bytes) -> int:
    """Return reflected CRC-32C with the APT-1 initialization convention."""

    crc = 0xFFFFFFFF
    for byte in data:
        crc ^= byte
        for _ in range(8):
            crc = (crc >> 1) ^ (0x82F63B78 if crc & 1 else 0)
    return crc ^ 0xFFFFFFFF


def encode_frame(message: str, payload: bytes, sequence: int, epoch: int) -> bytes:
    if message not in MESSAGE_TYPES:
        raise KeyError(f"unknown canonical message {message}")
    if len(payload) > MAX_PAYLOAD:
        raise ValueError("canonical payload exceeds APT-1's structural maximum")
    prefix = HEADER_PREFIX.pack(
        MAGIC,
        0,
        HEADER_BYTES,
        MESSAGE_TYPES[message],
        0,
        0,
        len(payload),
        SESSION,
        sequence,
        epoch,
    )
    checksum = crc32c(prefix + payload)
    return prefix + struct.pack("<I", checksum) + payload


@dataclass(frozen=True, slots=True)
class FrameSpec:
    direction: str
    message: str
    sequence: int
    epoch: int
    payload: bytes
    effect: str

    @property
    def encoded(self) -> bytes:
        return encode_frame(self.message, self.payload, self.sequence, self.epoch)

    @property
    def ordinary(self) -> bool:
        return self.message not in RESERVE_MESSAGES


@dataclass(frozen=True, slots=True)
class Scenario:
    name: str
    file: str
    kind: str
    precondition: dict[str, Any]
    frames: tuple[FrameSpec, ...]
    expected: dict[str, Any]


def fs(
    direction: str,
    message: str,
    sequence: int,
    epoch: int,
    payload: bytes,
    effect: str,
) -> FrameSpec:
    return FrameSpec(direction, message, sequence, epoch, payload, effect)


def frame_bytes(message: str, payload: bytes) -> int:
    return HEADER_BYTES + len(payload)


def credit_payload(value: int) -> bytes:
    return CREDIT.pack(value)


def caps_payload() -> bytes:
    return RET_CAPS.pack(
        TAG,
        0,
        0,
        FEATURES,
        CAPS_VALUES["max_owner_records"],
        CAPS_VALUES["max_live_owners"],
        CAPS_VALUES["max_regions"],
        CAPS_VALUES["max_resources"],
        CAPS_VALUES["max_objects"],
        CAPS_VALUES["max_series"],
        CAPS_VALUES["max_operations_per_transaction"],
        CAPS_VALUES["max_resource_chunk_bytes"],
        CAPS_VALUES["max_retained_transaction_bytes"],
        CAPS_VALUES["total_resource_bytes"],
    )


def formats_payload() -> bytes:
    return RET_FORMATS.pack(
        FORMAT_VALUES["coordinate_format"],
        FORMAT_VALUES["color_format"],
        FORMAT_VALUES["image_format"],
        FORMAT_VALUES["max_image_width"],
        FORMAT_VALUES["max_image_height"],
        FORMAT_VALUES["max_path_points"],
        FORMAT_VALUES["max_glyph_run_bytes"],
        FORMAT_VALUES["max_samples_per_append"],
        FORMAT_VALUES["max_history_per_series"],
        FORMAT_VALUES["minimum_presentation_interval_us"],
        FORMAT_VALUES["total_sample_slots"],
        FORMAT_VALUES["total_utf8_bytes"],
        0,
    )


def owner_open_payload(
    *,
    generation: int = OWNER_GENERATION,
    region_quota: int = 2,
    resource_quota: int = 0,
    object_quota: int = 4,
    series_quota: int = 1,
    resource_bytes: int = 0,
    utf8_bytes: int = 64,
    sample_slots: int = 8,
) -> bytes:
    return OWNER_OPEN.pack(
        OWNER_ID,
        generation,
        region_quota,
        resource_quota,
        object_quota,
        series_quota,
        resource_bytes,
        utf8_bytes,
        sample_slots,
        0,
    )


def ret_result_payload(
    request: str,
    status: int,
    revision: int,
    *,
    generation: int = OWNER_GENERATION,
    item_id: int = 0,
    accepted_bytes: int = 0,
) -> bytes:
    return RET_RESULT.pack(
        MESSAGE_TYPES[request],
        status,
        0,
        OWNER_ID,
        generation,
        item_id,
        revision,
        accepted_bytes,
    )


def region_payload(message: str, *, cols: int, rows: int) -> tuple[str, bytes]:
    if message not in {"REGION_DEFINE", "REGION_REPLACE"}:
        raise ValueError("region helper only accepts DEFINE or REPLACE")
    return message, REGION.pack(
        OWNER_ID,
        OWNER_GENERATION,
        REGION_ID,
        0,
        0,
        cols,
        rows,
        0,
        0x3,
    )


def object_payload(
    object_id: int,
    object_type: int,
    body: bytes,
    *,
    left: int,
    top: int,
    right: int,
    bottom: int,
) -> bytes:
    return OBJECT_PREFIX.pack(
        OWNER_ID,
        OWNER_GENERATION,
        object_id,
        object_type,
        0x1,
        0,
        REGION_ID,
        0,
        left,
        top,
        right,
        bottom,
    ) + body


def readout_payload(initial_value: int = -1_200) -> bytes:
    unit = b"dB"
    body = READOUT_BODY.pack(
        0xF0,
        0xF4,
        0xFF,
        0xFF,
        0x10,
        0x18,
        0x28,
        0xFF,
        1,
        2,
        initial_value,
        100,
        len(unit),
        0,
    ) + unit
    return object_payload(
        READOUT_ID,
        OBJECT_TYPES["READOUT"],
        body,
        left=0,
        top=0,
        right=0xFFFFFFFF,
        bottom=0x2FFFFFFF,
    )


def group_payload() -> bytes:
    return object_payload(
        GROUP_ID,
        OBJECT_TYPES["GROUP"],
        b"",
        left=0,
        top=0,
        right=0xFFFFFFFF,
        bottom=0x2FFFFFFF,
    )


def polyline_payload() -> bytes:
    points = (
        (0, 0xFFFFFFFF),
        (0x7FFFFFFF, 0),
        (0xFFFFFFFF, 0xBFFFFFFF),
    )
    body = POLYLINE_BODY.pack(
        len(points),
        0x02000000,
        0x53,
        0xD8,
        0xFB,
        0xFF,
        0,
    ) + b"".join(POINT.pack(x, y) for x, y in points)
    payload = OBJECT_PREFIX.pack(
        OWNER_ID,
        OWNER_GENERATION,
        POLYLINE_ID,
        OBJECT_TYPES["POLYLINE"],
        0x1,
        0,
        REGION_ID,
        GROUP_ID,
        0,
        0,
        0xFFFFFFFF,
        0xFFFFFFFF,
    )
    return payload + body


def glyph_run_payload(text: bytes = b"SoundLab") -> bytes:
    body = GLYPH_RUN_BODY.pack(
        0xF0,
        0xF4,
        0xFF,
        0xFF,
        0x10,
        0x18,
        0x28,
        0xFF,
        0,
        0,
        len(text),
    ) + text
    payload = OBJECT_PREFIX.pack(
        OWNER_ID,
        OWNER_GENERATION,
        GLYPH_RUN_ID,
        OBJECT_TYPES["GLYPH_RUN"],
        0x1,
        1,
        REGION_ID,
        GROUP_ID,
        0,
        0,
        0xFFFFFFFF,
        0xFFFFFFFF,
    )
    return payload + body


def meter_payload(initial_value: int = -1_200) -> bytes:
    body = METER_BODY.pack(
        0x40,
        0xE0,
        0x80,
        0xFF,
        0x10,
        0x18,
        0x28,
        0xFF,
        0,
        1,
        -6_000,
        0,
        initial_value,
        0,
    )
    return object_payload(
        METER_ID,
        OBJECT_TYPES["METER"],
        body,
        left=0,
        top=0x30000000,
        right=0xFFFFFFFF,
        bottom=0x47FFFFFF,
    )


def status_payload(initial_value: int = 0) -> bytes:
    body = STATUS_BODY.pack(
        0x30,
        0x38,
        0x48,
        0xFF,
        0x50,
        0xE0,
        0x90,
        0xFF,
        initial_value,
        0,
        0,
        0,
    )
    return object_payload(
        STATUS_ID,
        OBJECT_TYPES["STATUS"],
        body,
        left=0,
        top=0x48000000,
        right=0x1FFFFFFF,
        bottom=0x5FFFFFFF,
    )


def plot_payload(series_id: int = SERIES_ID) -> bytes:
    body = PLOT_BODY.pack(
        series_id,
        -32_768,
        32_767,
        0x53,
        0xD8,
        0xFB,
        0xFF,
        0x20,
        0x70,
        0x90,
        0x60,
        0x2,
        0,
    )
    return object_payload(
        PLOT_ID,
        OBJECT_TYPES["PLOT"],
        body,
        left=0,
        top=0x30000000,
        right=0xFFFFFFFF,
        bottom=0xFFFFFFFF,
    )


def waveform_payload() -> bytes:
    body = WAVEFORM_BODY.pack(
        EXPLICIT_SERIES_ID,
        -32_768,
        32_767,
        0xA8,
        0x78,
        0xFF,
        0xFF,
        0x50,
        0x58,
        0x68,
        0xFF,
        0,
        1,
        0,
    )
    return object_payload(
        WAVEFORM_ID,
        OBJECT_TYPES["WAVEFORM"],
        body,
        left=0,
        top=0xB0000000,
        right=0xFFFFFFFF,
        bottom=0xFFFFFFFF,
    )


def image_payload() -> bytes:
    return object_payload(
        IMAGE_ID,
        OBJECT_TYPES["IMAGE"],
        IMAGE_BODY.pack(RESOURCE_ID, 1, 255),
        left=0,
        top=0,
        right=0xFFFFFFFF,
        bottom=0xFFFFFFFF,
    )


def uniform_series_payload(message: str, timestamps_us: int, values: Sequence[int]) -> tuple[str, bytes]:
    if message not in {"SERIES_APPEND", "SERIES_REPLACE"}:
        raise ValueError("series helper only accepts APPEND or REPLACE")
    if not values:
        raise ValueError("canonical series payload must be nonempty")
    prefix = SERIES_SAMPLES.pack(
        OWNER_ID,
        OWNER_GENERATION,
        SERIES_ID,
        len(values),
        1,
        timestamps_us,
    )
    return message, prefix + struct.pack(f"<{len(values)}q", *values)


def explicit_series_payload(
    message: str,
    samples: Sequence[tuple[int, int]],
) -> tuple[str, bytes]:
    if message not in {"SERIES_APPEND", "SERIES_REPLACE"}:
        raise ValueError("series helper only accepts APPEND or REPLACE")
    if not samples:
        raise ValueError("canonical explicit series payload must be nonempty")
    prefix = SERIES_SAMPLES.pack(
        OWNER_ID,
        OWNER_GENERATION,
        EXPLICIT_SERIES_ID,
        len(samples),
        0,
        0,
    )
    return message, prefix + b"".join(struct.pack("<Qq", timestamp, value) for timestamp, value in samples)


def present_transaction(
    *,
    start_sequence: int,
    epoch: int,
    transaction_id: int,
    base_revision: int,
    geometry_generation: int,
    cols: int,
    rows: int,
    retained_mode: int,
    disposition: int,
    operations: Sequence[tuple[str, bytes]],
    cell_mode: int = 0,
    cell_spans: Sequence[bytes] = (),
    cursor_payload: bytes | None = None,
    declared_adjustment: int = 0,
    retained_operation_count_override: int | None = None,
) -> tuple[tuple[FrameSpec, ...], int]:
    if cell_mode == 0 and (cell_spans or cursor_payload is not None):
        raise ValueError("CELL_NONE cannot carry CELL spans or a cursor")
    if cell_mode != 0 and (not cell_spans or cursor_payload is None):
        raise ValueError("a CELL mutation requires spans and exactly one cursor")
    cell_count = sum(CELL_SPAN_PREFIX.unpack_from(payload)[2] for payload in cell_spans)
    commit = PRESENT_COMMIT.pack(transaction_id, disposition, 0)
    actual_bytes = (
        frame_bytes("PRESENT_BEGIN", b"\0" * PRESENT_BEGIN.size)
        + sum(frame_bytes("CELL_SPAN", payload) for payload in cell_spans)
        + (frame_bytes("CURSOR", cursor_payload) if cursor_payload is not None else 0)
        + sum(frame_bytes(message, payload) for message, payload in operations)
        + frame_bytes("PRESENT_COMMIT", commit)
    )
    declared_bytes = actual_bytes + declared_adjustment
    begin = PRESENT_BEGIN.pack(
        transaction_id,
        base_revision,
        geometry_generation,
        declared_bytes,
        cols,
        rows,
        len(cell_spans),
        cell_count,
        len(operations) if retained_operation_count_override is None else retained_operation_count_override,
        cell_mode,
        retained_mode,
        0,
    )
    frames: list[FrameSpec] = [
        fs(
            CLIENT,
            "PRESENT_BEGIN",
            start_sequence,
            epoch,
            begin,
            f"open retained transaction {transaction_id}",
        )
    ]
    next_sequence = start_sequence + 1
    for row_index, payload in enumerate(cell_spans):
        frames.append(
            fs(
                CLIENT,
                "CELL_SPAN",
                next_sequence,
                epoch,
                payload,
                f"stage CELL span {row_index + 1} of {len(cell_spans)}",
            )
        )
        next_sequence += 1
    if cursor_payload is not None:
        frames.append(fs(CLIENT, "CURSOR", next_sequence, epoch, cursor_payload, "stage the exact CELL cursor"))
        next_sequence += 1
    for offset, (message, payload) in enumerate(operations, start=1):
        frames.append(
            fs(
                CLIENT,
                message,
                next_sequence,
                epoch,
                payload,
                f"stage retained operation {offset} of {len(operations)}",
            )
        )
        next_sequence += 1
    frames.append(
        fs(
            CLIENT,
            "PRESENT_COMMIT",
            next_sequence,
            epoch,
            commit,
            "validate and atomically commit the retained transaction",
        )
    )
    return tuple(frames), actual_bytes


def legacy_cell_transaction(
    *,
    start_sequence: int,
    epoch: int,
    transaction_id: int,
    base_revision: int,
    cols: int,
    rows: int,
    cell_spans: Sequence[bytes],
    cursor_payload: bytes,
) -> tuple[tuple[FrameSpec, ...], int]:
    """Encode one unchanged CELL-1 transaction inside a retained-enabled epoch."""

    cell_count = sum(CELL_SPAN_PREFIX.unpack_from(payload)[2] for payload in cell_spans)
    begin = TX_BEGIN.pack(transaction_id, base_revision, cols, rows, len(cell_spans), cell_count)
    frames: list[FrameSpec] = [
        fs(CLIENT, "TX_BEGIN", start_sequence, epoch, begin, "open an unchanged legacy CELL transaction")
    ]
    next_sequence = start_sequence + 1
    for row_index, payload in enumerate(cell_spans):
        frames.append(
            fs(
                CLIENT,
                "CELL_SPAN",
                next_sequence,
                epoch,
                payload,
                f"stage legacy CELL span {row_index + 1} of {len(cell_spans)}",
            )
        )
        next_sequence += 1
    frames.append(fs(CLIENT, "CURSOR", next_sequence, epoch, cursor_payload, "stage the legacy CELL cursor"))
    next_sequence += 1
    frames.append(
        fs(
            CLIENT,
            "TX_COMMIT",
            next_sequence,
            epoch,
            TX_COMMIT.pack(transaction_id),
            "commit through the unchanged CELL-1 transaction family",
        )
    )
    return tuple(frames), sum(len(frame.encoded) for frame in frames)


def query_supported() -> Scenario:
    query = fs(CLIENT, "RET_QUERY", 29, 0, RET_QUERY.pack(TAG, 0), "issue the single optional retained query after the mandatory 80x25 snapshot")
    grant = QUERY_CREDIT_BEFORE + len(query.encoded)
    frames = (
        query,
        fs(TERMINAL, "RET_CAPS", 2, 0, caps_payload(), "advertise retained feature and capacity policy after the snapshot result"),
        fs(TERMINAL, "RET_FORMATS", 3, 0, formats_payload(), "advertise exact scalar and format bounds"),
        fs(TERMINAL, "CREDIT", 4, 0, credit_payload(grant), "release the prior snapshot and query only after both discovery replies"),
    )
    return Scenario(
        "ret_query_supported",
        "ret_query_supported.hex",
        "discovery_supported",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 1,
            "last_transaction_id": 1,
            "mandatory_cell_snapshot_complete": True,
            "next_client_sequence": 29,
            "next_terminal_sequence": 2,
            "client_to_terminal_credit_before": QUERY_CREDIT_BEFORE,
            "client_to_terminal_initial_grant": INITIAL_CLIENT_TO_TERMINAL_GRANT,
            "prior_snapshot_complete_bytes": PRIOR_SNAPSHOT_COMPLETE_BYTES,
            "client_to_terminal_sent_data_bytes_before": PRIOR_SNAPSHOT_COMPLETE_BYTES,
            "terminal_to_client_available_credit": CLIENT_RECEIVE_CREDIT,
        },
        frames,
        {
            "retained_enabled": True,
            "initial_replacement_required": True,
            "covering_credit": grant,
            "reply_order": ["RET_CAPS", "RET_FORMATS", "CREDIT"],
        },
    )


def query_unsupported() -> Scenario:
    query = fs(CLIENT, "RET_QUERY", 29, 0, RET_QUERY.pack(TAG, 0), "issue the optional query to a CELL-only terminal after the mandatory 80x25 snapshot")
    grant = QUERY_CREDIT_BEFORE + len(query.encoded)
    frames = (
        query,
        fs(TERMINAL, "CREDIT", 2, 0, credit_payload(grant), "negative answer at the exact snapshot-plus-query covering boundary"),
    )
    return Scenario(
        "ret_query_unsupported",
        "ret_query_unsupported.hex",
        "discovery_unsupported",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 1,
            "last_transaction_id": 1,
            "mandatory_cell_snapshot_complete": True,
            "next_client_sequence": 29,
            "next_terminal_sequence": 2,
            "client_to_terminal_credit_before": QUERY_CREDIT_BEFORE,
            "client_to_terminal_initial_grant": INITIAL_CLIENT_TO_TERMINAL_GRANT,
            "prior_snapshot_complete_bytes": PRIOR_SNAPSHOT_COMPLETE_BYTES,
            "client_to_terminal_sent_data_bytes_before": PRIOR_SNAPSHOT_COMPLETE_BYTES,
        },
        frames,
        {
            "retained_enabled": False,
            "continue_profile": "CELL-1",
            "covering_credit": grant,
            "retained_reply_count": 0,
        },
    )


def soundlab_initial_replace() -> Scenario:
    credit_before = QUERY_CREDIT_BEFORE + frame_bytes("RET_QUERY", RET_QUERY.pack(TAG, 0))
    owner = fs(
        CLIENT,
        "OWNER_OPEN",
        30,
        0,
        owner_open_payload(object_quota=12, series_quota=2, utf8_bytes=128, sample_slots=7),
        "atomically reserve the SoundLab owner quotas",
    )
    owner_credit = credit_before + len(owner.encoded)

    start_operations = (
        region_payload("REGION_DEFINE", cols=80, rows=25),
        (
            "SERIES_DEFINE",
            SERIES_DEFINE.pack(OWNER_ID, OWNER_GENERATION, SERIES_ID, 4, 1, 500_000),
        ),
        uniform_series_payload("SERIES_REPLACE", 1_000_000, (-1_200, -900, -600)),
        (
            "SERIES_DEFINE",
            SERIES_DEFINE.pack(OWNER_ID, OWNER_GENERATION, EXPLICIT_SERIES_ID, 3, 0, 0),
        ),
        explicit_series_payload("SERIES_REPLACE", ((1_000_000, -2_000), (1_750_000, 2_000))),
        ("OBJECT_DEFINE", group_payload()),
        ("OBJECT_DEFINE", polyline_payload()),
        ("OBJECT_DEFINE", glyph_run_payload()),
    )
    continue_operations = (
        ("OBJECT_DEFINE", readout_payload()),
        ("OBJECT_DEFINE", meter_payload()),
        ("OBJECT_DEFINE", status_payload()),
        ("OBJECT_DEFINE", plot_payload()),
        ("OBJECT_DEFINE", waveform_payload()),
    )
    start_transaction, start_declared = present_transaction(
        start_sequence=31,
        epoch=0,
        transaction_id=2,
        base_revision=1,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=2,
        disposition=0,
        operations=start_operations,
    )
    start_credit = owner_credit + start_declared
    continue_transaction, continue_declared = present_transaction(
        start_sequence=41,
        epoch=0,
        transaction_id=3,
        base_revision=2,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=3,
        disposition=1,
        operations=continue_operations,
    )
    continue_credit = start_credit + continue_declared
    legacy_span = CELL_SPAN_PREFIX.pack(0, 0, 1) + CELL.pack(ord("L"), 7, 0, 1)
    legacy_transaction, legacy_declared = legacy_cell_transaction(
        start_sequence=48,
        epoch=0,
        transaction_id=4,
        base_revision=3,
        cols=80,
        rows=25,
        cell_spans=(legacy_span,),
        cursor_payload=CURSOR.pack(0, 1, 1),
    )
    final_credit = continue_credit + legacy_declared
    frames = (
        owner,
        fs(TERMINAL, "RET_RESULT", 5, 0, ret_result_payload("OWNER_OPEN", STATUS["RET_OK"], 1), "confirm owner quota reservation"),
        fs(TERMINAL, "CREDIT", 6, 0, credit_payload(owner_credit), "release the consumed OWNER_OPEN bytes"),
        *start_transaction,
        fs(TERMINAL, "TX_RESULT", 7, 0, TX_RESULT.pack(2, 0, 0, 2), "commit the graph-valid hidden SoundLab start at revision 2"),
        fs(TERMINAL, "CREDIT", 8, 0, credit_payload(start_credit), "release REPLACE_START only after its TX_RESULT"),
        *continue_transaction,
        fs(TERMINAL, "TX_RESULT", 9, 0, TX_RESULT.pack(3, 0, 0, 3), "reveal the complete SoundLab scene at revision 3"),
        fs(TERMINAL, "CREDIT", 10, 0, credit_payload(continue_credit), "release REPLACE_CONTINUE only after its TX_RESULT"),
        *legacy_transaction,
        fs(TERMINAL, "TX_RESULT", 11, 0, TX_RESULT.pack(4, 0, 0, 4), "confirm unchanged legacy CELL interleaving at revision 4"),
        fs(TERMINAL, "CREDIT", 12, 0, credit_payload(final_credit), "release legacy CELL bytes after TX_RESULT"),
    )
    return Scenario(
        "soundlab_initial_replace",
        "soundlab_initial_replace.hex",
        "retained_replace_commit",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 1,
            "last_transaction_id": 1,
            "geometry": {"cols": 80, "rows": 25, "generation": 0},
            "next_client_sequence": 30,
            "next_terminal_sequence": 5,
            "client_to_terminal_credit_before": credit_before,
            "initial_replacement_required": True,
        },
        frames,
        {
            "global_revision": 4,
            "replace_start_transaction_bytes": start_declared,
            "replace_continue_transaction_bytes": continue_declared,
            "legacy_transaction_bytes": legacy_declared,
            "covering_credits": [owner_credit, start_credit, continue_credit, final_credit],
            "visible_retained": True,
            "owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
            "identities": {
                "regions": [REGION_ID],
                "objects": [
                    GROUP_ID,
                    POLYLINE_ID,
                    GLYPH_RUN_ID,
                    READOUT_ID,
                    METER_ID,
                    STATUS_ID,
                    PLOT_ID,
                    WAVEFORM_ID,
                ],
                "series": [SERIES_ID, EXPLICIT_SERIES_ID],
            },
            "series_samples": [
                {"timestamp_us": 1_000_000, "value": -1_200},
                {"timestamp_us": 1_500_000, "value": -900},
                {"timestamp_us": 2_000_000, "value": -600},
            ],
            "explicit_series_samples": [
                {"timestamp_us": 1_000_000, "value": -2_000},
                {"timestamp_us": 1_750_000, "value": 2_000},
            ],
        },
    )


def soundlab_dynamic_append() -> Scenario:
    credit_before = 85_290
    operations = (
        (
            "OBJECT_SET_VALUE",
            OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, READOUT_ID, -300),
        ),
        (
            "OBJECT_SET_VALUE",
            OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, METER_ID, -300),
        ),
        (
            "OBJECT_SET_VALUE",
            OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, STATUS_ID, 1),
        ),
        (
            "OBJECT_SET_VISIBILITY",
            OBJECT_SET_VISIBILITY.pack(OWNER_ID, OWNER_GENERATION, GLYPH_RUN_ID, 0),
        ),
        uniform_series_payload("SERIES_APPEND", 2_500_000, (-300, 100)),
        explicit_series_payload("SERIES_APPEND", ((2_500_000, -1_000), (3_250_000, 1_000))),
    )
    transaction, declared = present_transaction(
        start_sequence=52,
        epoch=0,
        transaction_id=5,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=operations,
    )
    credit_after = credit_before + declared
    frames = (
        *transaction,
        fs(TERMINAL, "TX_RESULT", 13, 0, TX_RESULT.pack(5, 0, 0, 5), "confirm dynamic update at global revision 5"),
        fs(TERMINAL, "CREDIT", 14, 0, credit_payload(credit_after), "release dynamic transaction bytes after TX_RESULT"),
    )
    return Scenario(
        "soundlab_dynamic_append",
        "soundlab_dynamic_append.hex",
        "dynamic_only_commit",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 4,
            "last_transaction_id": 4,
            "geometry": {"cols": 80, "rows": 25, "generation": 0},
            "next_client_sequence": 52,
            "next_terminal_sequence": 13,
            "client_to_terminal_credit_before": credit_before,
            "newest_series_timestamp_us": 2_000_000,
        },
        frames,
        {
            "global_revision": 5,
            "declared_transaction_bytes": declared,
            "covering_credit": credit_after,
            "readout_value": -300,
            "meter_value": -300,
            "status_value": 1,
            "glyph_run_visible": False,
            "appended_samples": [
                {"timestamp_us": 2_500_000, "value": -300},
                {"timestamp_us": 3_000_000, "value": 100},
            ],
            "uniform_ring_after_eviction": [
                {"timestamp_us": 1_500_000, "value": -900},
                {"timestamp_us": 2_000_000, "value": -600},
                {"timestamp_us": 2_500_000, "value": -300},
                {"timestamp_us": 3_000_000, "value": 100},
            ],
            "explicit_appended_samples": [
                {"timestamp_us": 2_500_000, "value": -1_000},
                {"timestamp_us": 3_250_000, "value": 1_000},
            ],
            "explicit_ring_after_eviction": [
                {"timestamp_us": 1_750_000, "value": 2_000},
                {"timestamp_us": 2_500_000, "value": -1_000},
                {"timestamp_us": 3_250_000, "value": 1_000},
            ],
            "static_definition_messages": 0,
        },
    )


def mutation_and_drop_lifecycle() -> Scenario:
    credit_before = 85_078
    replace_ops = (("OBJECT_REPLACE", glyph_run_payload(b"SoundLab armed")),)
    replace_tx, replace_bytes = present_transaction(
        start_sequence=48,
        epoch=0,
        transaction_id=4,
        base_revision=3,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=replace_ops,
    )
    replace_credit = credit_before + replace_bytes

    drop_ops: tuple[tuple[str, bytes], ...] = tuple(
        ("OBJECT_DROP", OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, object_id))
        for object_id in (
            POLYLINE_ID,
            GLYPH_RUN_ID,
            GROUP_ID,
            READOUT_ID,
            METER_ID,
            STATUS_ID,
            PLOT_ID,
            WAVEFORM_ID,
        )
    ) + (
        ("SERIES_DROP", OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, SERIES_ID)),
        ("SERIES_DROP", OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, EXPLICIT_SERIES_ID)),
        ("REGION_DROP", OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, REGION_ID)),
    )
    drop_tx, drop_bytes = present_transaction(
        start_sequence=51,
        epoch=0,
        transaction_id=5,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=drop_ops,
    )
    drop_credit = replace_credit + drop_bytes
    frames = (
        *replace_tx,
        fs(TERMINAL, "TX_RESULT", 11, 0, TX_RESULT.pack(4, 0, 0, 4), "confirm complete GLYPH_RUN replacement"),
        fs(TERMINAL, "CREDIT", 12, 0, credit_payload(replace_credit), "release replacement transaction after TX_RESULT"),
        *drop_tx,
        fs(TERMINAL, "TX_RESULT", 13, 0, TX_RESULT.pack(5, 0, 0, 5), "confirm graph-safe object, series, and region drops"),
        fs(TERMINAL, "CREDIT", 14, 0, credit_payload(drop_credit), "release drop transaction after TX_RESULT"),
    )
    return Scenario(
        "mutation_and_drop_lifecycle",
        "mutation_and_drop_lifecycle.hex",
        "mutation_and_drop_commit",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "last_transaction_id": 3,
            "geometry": {"cols": 80, "rows": 25, "generation": 0},
            "next_client_sequence": 48,
            "next_terminal_sequence": 11,
            "client_to_terminal_credit_before": credit_before,
            "prior_history_complete_bytes": 19_542,
            "live_objects": list(range(GROUP_ID, WAVEFORM_ID + 1)),
            "live_series": [SERIES_ID, EXPLICIT_SERIES_ID],
            "live_regions": [REGION_ID],
        },
        frames,
        {
            "global_revision": 5,
            "replacement_transaction_bytes": replace_bytes,
            "drop_transaction_bytes": drop_bytes,
            "covering_credits": [replace_credit, drop_credit],
            "owner_remains_live": True,
            "live_objects": [],
            "live_series": [],
            "live_regions": [],
        },
    )


def owner_drop_tombstone() -> Scenario:
    reopen = fs(
        CLIENT,
        "OWNER_OPEN",
        104,
        0,
        owner_open_payload(generation=OWNER_GENERATION + 1),
        "reopen the tombstoned numeric owner ID only with a newer generation",
    )
    reopen_credit = 140_000 + len(reopen.encoded)
    frames = (
        fs(
            CLIENT,
            "OWNER_DROP",
            100,
            0,
            OWNER_DROP.pack(4, 3, OWNER_ID, OWNER_GENERATION),
            "drop the exact live owner generation and all of its retained state",
        ),
        fs(TERMINAL, "TX_RESULT", 100, 0, TX_RESULT.pack(4, 0, 0, 4), "confirm exact drop and tombstone creation"),
        fs(
            CLIENT,
            "OWNER_DROP",
            101,
            0,
            OWNER_DROP.pack(5, 4, OWNER_ID, OWNER_GENERATION),
            "repeat the exact tombstone drop idempotently as a new ordered request",
        ),
        fs(TERMINAL, "TX_RESULT", 101, 0, TX_RESULT.pack(5, 0, 0, 5), "confirm allocation-idempotent revisioned drop"),
        fs(
            CLIENT,
            "OWNER_DROP",
            102,
            0,
            OWNER_DROP.pack(6, 5, OWNER_ID, OWNER_GENERATION - 1),
            "attempt owner drop with stale generation authority",
        ),
        fs(TERMINAL, "TX_RESULT", 102, 0, TX_RESULT.pack(6, 2, 0, 5), "reject stale generation without changing tombstone or revision"),
        fs(
            CLIENT,
            "OWNER_DROP",
            103,
            0,
            OWNER_DROP.pack(7, 4, OWNER_ID, OWNER_GENERATION),
            "attempt exact tombstone drop from a stale base revision",
        ),
        fs(TERMINAL, "TX_RESULT", 103, 0, TX_RESULT.pack(7, 3, 0, 5), "reject stale base without changing tombstone or revision"),
        reopen,
        fs(TERMINAL, "RET_RESULT", 104, 0, ret_result_payload("OWNER_OPEN", STATUS["RET_OK"], 5, generation=OWNER_GENERATION + 1), "reserve quotas for the newer live generation"),
        fs(TERMINAL, "CREDIT", 105, 0, credit_payload(reopen_credit), "release newer-generation OWNER_OPEN bytes"),
    )
    return Scenario(
        "owner_drop_tombstone",
        "owner_drop_tombstone.hex",
        "owner_tombstone_lifecycle",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "last_transaction_id": 3,
            "next_client_sequence": 100,
            "next_terminal_sequence": 100,
            "client_to_terminal_credit_before": 140_000,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
        },
        frames,
        {
            "global_revision": 5,
            "owner_live": True,
            "tombstone": {"id": OWNER_ID, "generation": OWNER_GENERATION},
            "reopened_generation": OWNER_GENERATION + 1,
            "idempotent_drop_status": 0,
            "stale_drop_status": 2,
            "stale_base_drop_status": 3,
            "covering_credit": reopen_credit,
        },
    )


def control_reserve_boundary() -> Scenario:
    grant_before = 150_000
    reopen = fs(
        CLIENT,
        "OWNER_OPEN",
        141,
        0,
        owner_open_payload(generation=OWNER_GENERATION + 1),
        "use newly granted ordinary credit to reopen with a newer generation",
    )
    reopen_grant = grant_before + len(reopen.encoded)
    frames = (
        fs(
            CLIENT,
            "OWNER_DROP",
            140,
            0,
            OWNER_DROP.pack(4, 3, OWNER_ID, OWNER_GENERATION),
            "retire the exact owner through control reserve while ordinary credit is zero",
        ),
        fs(TERMINAL, "TX_RESULT", 140, 0, TX_RESULT.pack(4, 0, 0, 4), "confirm reserve-backed owner retirement"),
        fs(TERMINAL, "CREDIT", 141, 0, credit_payload(reopen_grant), "grant exactly one OWNER_OPEN of new ordinary allowance"),
        reopen,
        fs(TERMINAL, "RET_RESULT", 142, 0, ret_result_payload("OWNER_OPEN", STATUS["RET_OK"], 4, generation=OWNER_GENERATION + 1), "confirm newer-generation reopen"),
        fs(TERMINAL, "CREDIT", 143, 0, credit_payload(reopen_grant + len(reopen.encoded)), "release consumed OWNER_OPEN bytes"),
    )
    return Scenario(
        "control_reserve_boundary",
        "control_reserve_boundary.hex",
        "ordinary_exhaustion_control_progress",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "last_transaction_id": 3,
            "next_client_sequence": 140,
            "next_terminal_sequence": 140,
            "client_to_terminal_cumulative_grant": grant_before,
            "client_to_terminal_sent_data_bytes": grant_before,
            "ordinary_credit_available": 0,
            "control_reserve_available": 4096,
            "pending_resource_begin_complete_bytes": 120,
        },
        frames,
        {
            "resource_begin_emitted": False,
            "owner_drop_used_ordinary_credit": False,
            "owner_drop_revision": 4,
            "reopened_generation": OWNER_GENERATION + 1,
            "covering_credits": [reopen_grant, reopen_grant + len(reopen.encoded)],
        },
    )


def resource_lifecycle() -> Scenario:
    credit_before = 125_000
    drop_in_use = fs(
        CLIENT,
        "RESOURCE_DROP",
        90,
        0,
        OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID),
        "attempt to drop a resource referenced by the active IMAGE",
    )
    drop_in_use_credit = credit_before + len(drop_in_use.encoded)

    rgba = b"\x44\x88\xcc\xff"
    begin = fs(
        CLIENT,
        "RESOURCE_BEGIN",
        91,
        0,
        RESOURCE_BEGIN.pack(
            OWNER_ID,
            OWNER_GENERATION,
            2,
            1,
            1,
            1,
            0,
            len(rgba),
            hashlib.sha3_256(rgba).digest(),
        ),
        "open a second bounded upload for abort qualification",
    )
    begin_credit = drop_in_use_credit + len(begin.encoded)
    wrong_commit = fs(
        CLIENT,
        "RESOURCE_COMMIT",
        92,
        0,
        RESOURCE_COMMIT.pack(OWNER_ID, OWNER_GENERATION, 3),
        "reject a same-authority wrong-resource commit while preserving resource 2 upload",
    )
    wrong_commit_credit = begin_credit + len(wrong_commit.encoded)
    bad_abort = fs(
        CLIENT,
        "RESOURCE_ABORT",
        93,
        0,
        RESOURCE_ABORT.pack(OWNER_ID, OWNER_GENERATION, 2, 9),
        "reject an unknown abort reason while preserving the exact upload",
    )
    good_abort = fs(
        CLIENT,
        "RESOURCE_ABORT",
        94,
        0,
        RESOURCE_ABORT.pack(OWNER_ID, OWNER_GENERATION, 2, 0),
        "cancel the still-open upload and release its reservation",
    )

    drop_object_tx, drop_object_bytes = present_transaction(
        start_sequence=95,
        epoch=0,
        transaction_id=3,
        base_revision=2,
        geometry_generation=0,
        cols=1,
        rows=1,
        retained_mode=1,
        disposition=0,
        operations=(("OBJECT_DROP", OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, IMAGE_ID)),),
    )
    transaction_credit = wrong_commit_credit + drop_object_bytes
    drop_success = fs(
        CLIENT,
        "RESOURCE_DROP",
        98,
        0,
        OWNER_ITEM.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID),
        "drop the now-unreferenced published resource",
    )
    final_credit = transaction_credit + len(drop_success.encoded)
    frames = (
        drop_in_use,
        fs(TERMINAL, "RET_RESULT", 90, 0, ret_result_payload("RESOURCE_DROP", STATUS["RET_IN_USE"], 2, item_id=RESOURCE_ID), "reject in-use resource drop without mutation"),
        fs(TERMINAL, "CREDIT", 91, 0, credit_payload(drop_in_use_credit), "release rejected RESOURCE_DROP bytes"),
        begin,
        fs(TERMINAL, "RET_RESULT", 92, 0, ret_result_payload("RESOURCE_BEGIN", STATUS["RET_OK"], 2, item_id=2), "open resource 2 upload"),
        fs(TERMINAL, "CREDIT", 93, 0, credit_payload(begin_credit), "release RESOURCE_BEGIN bytes"),
        wrong_commit,
        fs(TERMINAL, "RET_RESULT", 94, 0, ret_result_payload("RESOURCE_COMMIT", STATUS["RET_INVALID"], 2, item_id=3), "reject wrong resource ID and preserve resource 2 upload"),
        fs(TERMINAL, "CREDIT", 95, 0, credit_payload(wrong_commit_credit), "release wrong-resource RESOURCE_COMMIT bytes"),
        bad_abort,
        fs(TERMINAL, "RET_RESULT", 96, 0, ret_result_payload("RESOURCE_ABORT", STATUS["RET_INVALID"], 2, item_id=2), "reject bad abort reason and retain upload"),
        good_abort,
        fs(TERMINAL, "RET_RESULT", 97, 0, ret_result_payload("RESOURCE_ABORT", STATUS["RET_ABORTED"], 2, item_id=2), "destroy the exact upload and release reservation"),
        *drop_object_tx,
        fs(TERMINAL, "TX_RESULT", 98, 0, TX_RESULT.pack(3, 0, 0, 3), "commit IMAGE removal at revision 3"),
        fs(TERMINAL, "CREDIT", 99, 0, credit_payload(transaction_credit), "release object-drop transaction after TX_RESULT"),
        drop_success,
        fs(TERMINAL, "RET_RESULT", 100, 0, ret_result_payload("RESOURCE_DROP", STATUS["RET_OK"], 3, item_id=RESOURCE_ID), "release unreferenced resource storage"),
        fs(TERMINAL, "CREDIT", 101, 0, credit_payload(final_credit), "release successful RESOURCE_DROP bytes"),
    )
    return Scenario(
        "resource_lifecycle",
        "resource_lifecycle.hex",
        "resource_abort_drop_lifecycle",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 2,
            "last_transaction_id": 2,
            "geometry": {"cols": 1, "rows": 1, "generation": 0},
            "next_client_sequence": 90,
            "next_terminal_sequence": 90,
            "client_to_terminal_credit_before": credit_before,
            "resource_1_referenced_by_image_1": True,
            "second_live_owner_reuses_region_object_series_id_1": True,
            "no_upload_open": True,
        },
        frames,
        {
            "global_revision": 3,
            "covering_credits": [
                drop_in_use_credit,
                begin_credit,
                wrong_commit_credit,
                transaction_credit,
                final_credit,
            ],
            "wrong_resource_commit_preserved_upload": True,
            "bad_abort_preserved_upload": True,
            "good_abort_status": "RET_ABORTED",
            "resource_1_published": False,
            "resource_2_published": False,
            "upload_open": False,
        },
    )


def mixed_commit_and_rejections() -> Scenario:
    credit = 130_000
    transaction_id = 4
    cell_span = CELL_SPAN_PREFIX.pack(0, 0, 1) + CELL.pack(ord("M"), 2, 0, 1)
    cursor = CURSOR.pack(0, 1, 1)
    value = OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, READOUT_ID, -250)
    commit = PRESENT_COMMIT.pack(transaction_id, 0, 0)
    mixed_bytes = (
        frame_bytes("PRESENT_BEGIN", b"\0" * PRESENT_BEGIN.size)
        + frame_bytes("CELL_SPAN", cell_span)
        + frame_bytes("CURSOR", cursor)
        + frame_bytes("OBJECT_SET_VALUE", value)
        + frame_bytes("PRESENT_COMMIT", commit)
    )
    begin = PRESENT_BEGIN.pack(
        transaction_id,
        3,
        0,
        mixed_bytes,
        80,
        25,
        1,
        1,
        1,
        1,
        1,
        0,
    )
    mixed_frames = (
        fs(CLIENT, "PRESENT_BEGIN", 110, 0, begin, "open a mixed CELL/retained delta"),
        fs(CLIENT, "CELL_SPAN", 111, 0, cell_span, "stage one CELL mutation"),
        fs(CLIENT, "CURSOR", 112, 0, cursor, "stage the mandatory mixed CELL cursor"),
        fs(CLIENT, "OBJECT_SET_VALUE", 113, 0, value, "stage one retained value mutation"),
        fs(CLIENT, "PRESENT_COMMIT", 114, 0, commit, "atomically commit both terminal output planes"),
    )
    credit += mixed_bytes

    bad_bytes_tx, bad_bytes_actual = present_transaction(
        start_sequence=115,
        epoch=0,
        transaction_id=5,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=(("OBJECT_SET_VALUE", OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, READOUT_ID, -200)),),
        declared_adjustment=1,
    )
    credit_after_bad_bytes = credit + bad_bytes_actual

    bad_count_tx, bad_count_actual = present_transaction(
        start_sequence=118,
        epoch=0,
        transaction_id=6,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=(("OBJECT_SET_VALUE", OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, READOUT_ID, -150)),),
        retained_operation_count_override=2,
    )
    credit_after_bad_count = credit_after_bad_bytes + bad_count_actual

    timestamp_tx, timestamp_actual = present_transaction(
        start_sequence=121,
        epoch=0,
        transaction_id=7,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=(explicit_series_payload("SERIES_APPEND", ((3_250_000, 5), (4_000_000, 10))),),
    )
    timestamp_credit = credit_after_bad_count + timestamp_actual

    missing_reference_tx, missing_reference_actual = present_transaction(
        start_sequence=124,
        epoch=0,
        transaction_id=8,
        base_revision=4,
        geometry_generation=0,
        cols=80,
        rows=25,
        retained_mode=1,
        disposition=0,
        operations=(("OBJECT_REPLACE", plot_payload(series_id=99)),),
    )
    missing_reference_credit = timestamp_credit + missing_reference_actual

    lost_span = CELL_SPAN_PREFIX.pack(0, 1, 1) + CELL.pack(ord("!"), 1, 0, 0)
    lost_cursor = CURSOR.pack(0, 2, 1)
    lost_object = plot_payload(series_id=99)
    lost_commit = PRESENT_COMMIT.pack(9, 0, 0)
    lost_bytes = (
        frame_bytes("PRESENT_BEGIN", b"\0" * PRESENT_BEGIN.size)
        + frame_bytes("CELL_SPAN", lost_span)
        + frame_bytes("CURSOR", lost_cursor)
        + frame_bytes("OBJECT_REPLACE", lost_object)
        + frame_bytes("PRESENT_COMMIT", lost_commit)
    )
    lost_begin = PRESENT_BEGIN.pack(9, 4, 0, lost_bytes, 80, 25, 1, 1, 1, 1, 1, 0)
    lost_tx = (
        fs(CLIENT, "PRESENT_BEGIN", 127, 0, lost_begin, "open a mixed transaction whose retained graph is invalid"),
        fs(CLIENT, "CELL_SPAN", 128, 0, lost_span, "stage a valid CELL mutation in the mixed transaction"),
        fs(CLIENT, "CURSOR", 129, 0, lost_cursor, "stage the mandatory CELL cursor"),
        fs(CLIENT, "OBJECT_REPLACE", 130, 0, lost_object, "stage a PLOT replacement with missing series 99"),
        fs(CLIENT, "PRESENT_COMMIT", 131, 0, lost_commit, "reject the mixed transaction without partial model mutation"),
    )
    final_credit = missing_reference_credit + lost_bytes

    frames = (
        *mixed_frames,
        fs(TERMINAL, "TX_RESULT", 110, 0, TX_RESULT.pack(4, 0, 0, 4), "confirm one global mixed-plane revision"),
        fs(TERMINAL, "CREDIT", 111, 0, credit_payload(credit), "release mixed transaction only after TX_RESULT"),
        *bad_bytes_tx,
        fs(TERMINAL, "TX_RESULT", 112, 0, TX_RESULT.pack(5, 2, 0, 4), "reject wrong declared byte count with active model unchanged"),
        fs(TERMINAL, "CREDIT", 113, 0, credit_payload(credit_after_bad_bytes), "release drained bad-byte transaction"),
        *bad_count_tx,
        fs(TERMINAL, "TX_RESULT", 114, 0, TX_RESULT.pack(6, 2, 0, 4), "reject retained operation-count mismatch"),
        fs(TERMINAL, "CREDIT", 115, 0, credit_payload(credit_after_bad_count), "release drained bad-count transaction"),
        *timestamp_tx,
        fs(TERMINAL, "TX_RESULT", 116, 0, TX_RESULT.pack(7, 2, 0, 4), "reject explicit timestamp that does not advance newest history"),
        fs(TERMINAL, "CREDIT", 117, 0, credit_payload(timestamp_credit), "release rejected timestamp transaction"),
        *missing_reference_tx,
        fs(TERMINAL, "TX_RESULT", 118, 0, TX_RESULT.pack(8, 2, 0, 4), "reject missing series reference while retained-only session remains recoverable"),
        fs(TERMINAL, "CREDIT", 119, 0, credit_payload(missing_reference_credit), "release rejected missing-reference transaction"),
        *lost_tx,
        fs(TERMINAL, "TX_RESULT", 120, 0, TX_RESULT.pack(9, 2, 0, 4), "reject mixed transaction and force client SESSION_LOST"),
        fs(TERMINAL, "CREDIT", 121, 0, credit_payload(final_credit), "release rejected mixed transaction after its result"),
    )
    return Scenario(
        "mixed_commit_and_rejections",
        "mixed_commit_and_rejections.hex",
        "mixed_commit_and_semantic_rejections",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "last_transaction_id": 3,
            "geometry": {"cols": 80, "rows": 25, "generation": 0},
            "next_client_sequence": 110,
            "next_terminal_sequence": 110,
            "client_to_terminal_credit_before": 130_000,
            "explicit_series_newest_timestamp_us": 3_250_000,
        },
        frames,
        {
            "global_revision": 4,
            "mixed_transaction_bytes": mixed_bytes,
            "bad_declared_bytes_actual": bad_bytes_actual,
            "bad_declared_bytes_field": bad_bytes_actual + 1,
            "bad_retained_operation_count": 2,
            "recoverable_retained_rejections": 4,
            "missing_reference_series_id": 99,
            "mixed_rejection_client_state": "SESSION_LOST",
            "covering_credits": [
                credit,
                credit_after_bad_bytes,
                credit_after_bad_count,
                timestamp_credit,
                missing_reference_credit,
                final_credit,
            ],
        },
    )


def legacy_cell_and_replace_continue() -> Scenario:
    """Interleave unchanged CELL-1 with a two-commit hidden replacement."""

    credit_before = 72_000
    owner = fs(
        CLIENT,
        "OWNER_OPEN",
        150,
        0,
        owner_open_payload(
            region_quota=1,
            resource_quota=0,
            object_quota=2,
            series_quota=1,
            resource_bytes=0,
            utf8_bytes=8,
            sample_slots=4,
        ),
        "reserve the bounded owner ledger before building the hidden replacement",
    )
    owner_credit = credit_before + len(owner.encoded)

    legacy_span = CELL_SPAN_PREFIX.pack(0, 0, 1) + CELL.pack(ord("L"), 7, 0, 1)
    legacy, legacy_declared = legacy_cell_transaction(
        start_sequence=151,
        epoch=0,
        transaction_id=2,
        base_revision=1,
        cols=2,
        rows=1,
        cell_spans=(legacy_span,),
        cursor_payload=CURSOR.pack(0, 1, 1),
    )
    legacy_credit = owner_credit + legacy_declared

    replace_start, start_declared = present_transaction(
        start_sequence=155,
        epoch=0,
        transaction_id=3,
        base_revision=2,
        geometry_generation=0,
        cols=2,
        rows=1,
        retained_mode=2,
        disposition=0,
        operations=(
            region_payload("REGION_DEFINE", cols=2, rows=1),
            ("SERIES_DEFINE", SERIES_DEFINE.pack(OWNER_ID, OWNER_GENERATION, SERIES_ID, 4, 1, 500_000)),
        ),
    )
    start_credit = legacy_credit + start_declared

    replace_continue, continue_declared = present_transaction(
        start_sequence=159,
        epoch=0,
        transaction_id=4,
        base_revision=3,
        geometry_generation=0,
        cols=2,
        rows=1,
        retained_mode=3,
        disposition=1,
        operations=(
            uniform_series_payload("SERIES_REPLACE", 1_000_000, (10, 20)),
            ("OBJECT_DEFINE", readout_payload(initial_value=10)),
            ("OBJECT_DEFINE", plot_payload()),
        ),
    )
    final_credit = start_credit + continue_declared
    frames = (
        owner,
        fs(TERMINAL, "RET_RESULT", 150, 0, ret_result_payload("OWNER_OPEN", STATUS["RET_OK"], 1), "commit the exact owner quota reservation"),
        fs(TERMINAL, "CREDIT", 151, 0, credit_payload(owner_credit), "release OWNER_OPEN bytes after RET_RESULT"),
        *legacy,
        fs(TERMINAL, "TX_RESULT", 152, 0, TX_RESULT.pack(2, 0, 0, 2), "advance the shared revision through legacy TX_COMMIT"),
        fs(TERMINAL, "CREDIT", 153, 0, credit_payload(legacy_credit), "release the legacy CELL transaction after TX_RESULT"),
        *replace_start,
        fs(TERMINAL, "TX_RESULT", 154, 0, TX_RESULT.pack(3, 0, 0, 3), "commit a graph-valid hidden replacement without reveal"),
        fs(TERMINAL, "CREDIT", 155, 0, credit_payload(start_credit), "release REPLACE_START bytes after TX_RESULT"),
        *replace_continue,
        fs(TERMINAL, "TX_RESULT", 156, 0, TX_RESULT.pack(4, 0, 0, 4), "atomically reveal the completed hidden replacement"),
        fs(TERMINAL, "CREDIT", 157, 0, credit_payload(final_credit), "release REPLACE_CONTINUE bytes after TX_RESULT"),
    )
    return Scenario(
        "legacy_cell_and_replace_continue",
        "legacy_cell_and_replace_continue.hex",
        "legacy_cell_interleave_hidden_replace",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 1,
            "last_transaction_id": 1,
            "geometry": {"cols": 2, "rows": 1, "generation": 0},
            "next_client_sequence": 150,
            "next_terminal_sequence": 150,
            "client_to_terminal_credit_before": credit_before,
            "retained_enabled": True,
            "initial_replacement_required": True,
        },
        frames,
        {
            "global_revision": 4,
            "legacy_transaction_bytes": legacy_declared,
            "replace_start_transaction_bytes": start_declared,
            "replace_continue_transaction_bytes": continue_declared,
            "covering_credits": [owner_credit, legacy_credit, start_credit, final_credit],
        },
    )


def resize_layout_sync() -> Scenario:
    credit_before = 75_000
    cols = 3
    rows = 2
    cell_spans = (
        CELL_SPAN_PREFIX.pack(0, 0, cols)
        + b"".join(CELL.pack(ord(char), 7, 0, 1) for char in "RES"),
        CELL_SPAN_PREFIX.pack(1, 0, cols)
        + b"".join(CELL.pack(ord(char), 7, 0, 1) for char in "IZE"),
    )
    cell_transaction, cell_declared = present_transaction(
        start_sequence=30,
        epoch=0,
        transaction_id=5,
        base_revision=4,
        geometry_generation=1,
        cols=cols,
        rows=rows,
        cell_mode=2,
        cell_spans=cell_spans,
        cursor_payload=CURSOR.pack(1, 2, 1),
        retained_mode=0,
        disposition=0,
        operations=(),
    )
    cell_credit = credit_before + cell_declared
    layout_start, start_declared = present_transaction(
        start_sequence=35,
        epoch=0,
        transaction_id=6,
        base_revision=5,
        geometry_generation=1,
        cols=cols,
        rows=rows,
        retained_mode=4,
        disposition=0,
        operations=(region_payload("REGION_REPLACE", cols=cols, rows=rows),),
    )
    start_credit = cell_credit + start_declared
    layout_continue, continue_declared = present_transaction(
        start_sequence=38,
        epoch=0,
        transaction_id=7,
        base_revision=6,
        geometry_generation=1,
        cols=cols,
        rows=rows,
        retained_mode=5,
        disposition=1,
        operations=(),
    )
    final_credit = start_credit + continue_declared
    frames = (
        *cell_transaction,
        fs(TERMINAL, "TX_RESULT", 30, 0, TX_RESULT.pack(5, 0, 0, 5), "confirm the canonical resize CELL replacement at revision 5"),
        fs(TERMINAL, "CREDIT", 31, 0, credit_payload(cell_credit), "release resize CELL replacement bytes after TX_RESULT"),
        *layout_start,
        fs(TERMINAL, "TX_RESULT", 32, 0, TX_RESULT.pack(6, 0, 0, 6), "commit the hidden copy-on-write layout target at revision 6"),
        fs(TERMINAL, "CREDIT", 33, 0, credit_payload(start_credit), "release LAYOUT_START bytes after its result"),
        *layout_continue,
        fs(TERMINAL, "TX_RESULT", 34, 0, TX_RESULT.pack(7, 0, 0, 7), "reveal the committed hidden layout target at revision 7"),
        fs(TERMINAL, "CREDIT", 35, 0, credit_payload(final_credit), "release LAYOUT_CONTINUE bytes after its result"),
    )
    return Scenario(
        "resize_layout_sync",
        "resize_layout_sync.hex",
        "layout_reveal",
        {
            "state": "ACTIVE_LAYOUT_REQUIRED",
            "presentation_epoch": 0,
            "global_revision": 4,
            "last_transaction_id": 4,
            "selected_geometry": {"cols": cols, "rows": rows, "generation": 1},
            "cell_model_geometry": {"cols": 2, "rows": 1},
            "next_client_sequence": 30,
            "next_terminal_sequence": 30,
            "client_to_terminal_credit_before": credit_before,
            "identities": {
                "regions": [REGION_ID],
                "objects": list(range(GROUP_ID, WAVEFORM_ID + 1)),
                "series": [SERIES_ID, EXPLICIT_SERIES_ID],
            },
        },
        frames,
        {
            "global_revision": 7,
            "cell_replace_transaction_bytes": cell_declared,
            "layout_start_transaction_bytes": start_declared,
            "layout_continue_transaction_bytes": continue_declared,
            "covering_credits": [cell_credit, start_credit, final_credit],
            "retained_visible": True,
            "region_identity_preserved": REGION_ID,
            "object_identities_preserved": list(range(GROUP_ID, WAVEFORM_ID + 1)),
            "series_identities_preserved": [SERIES_ID, EXPLICIT_SERIES_ID],
            "static_definition_messages": 0,
        },
    )


def reset_crossed_present_commit() -> Scenario:
    transaction, declared = present_transaction(
        start_sequence=170,
        epoch=0,
        transaction_id=5,
        base_revision=4,
        geometry_generation=0,
        cols=2,
        rows=1,
        retained_mode=1,
        disposition=0,
        operations=(("OBJECT_SET_VALUE", OBJECT_SET_VALUE.pack(OWNER_ID, OWNER_GENERATION, READOUT_ID, 99)),),
    )
    reset_request = fs(
        TERMINAL,
        "SOFT_RESET_REQUEST",
        170,
        0,
        SOFT_RESET_REQUEST.pack(1, 4),
        "request epoch 1 after the PRESENT body but before accepting its COMMIT",
    )
    frames = (
        transaction[0],
        transaction[1],
        reset_request,
        transaction[2],
        fs(TERMINAL, "TX_RESULT", 171, 0, TX_RESULT.pack(5, 1, 0, 4), "settle the crossed valid PRESENT_COMMIT as reset-aborted without mutation"),
        fs(CLIENT, "SOFT_RESET_ACK", 173, 1, SOFT_RESET_ACK.pack(1, 0, 0), "acknowledge only after the old-epoch result settles"),
    )
    return Scenario(
        "reset_crossed_present_commit",
        "reset_crossed_present_commit.hex",
        "reset_crossed_present_settlement",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 4,
            "last_transaction_id": 4,
            "geometry": {"cols": 2, "rows": 1, "generation": 0},
            "next_client_sequence": 170,
            "next_terminal_sequence": 170,
            "client_to_terminal_credit_before": 85_000,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
        },
        frames,
        {
            "presentation_epoch": 1,
            "global_revision": 0,
            "crossed_transaction_bytes": declared,
            "crossed_status": "ABORTED",
            "old_epoch_revision_after_result": 4,
            "tx_abort_emitted": False,
        },
    )


def reset_crossed_owner_drop() -> Scenario:
    reset_request = fs(
        TERMINAL,
        "SOFT_RESET_REQUEST",
        180,
        0,
        SOFT_RESET_REQUEST.pack(1, 4),
        "request epoch 1 before the terminal accepts the already-emitted owner drop",
    )
    drop = fs(
        CLIENT,
        "OWNER_DROP",
        180,
        0,
        OWNER_DROP.pack(5, 4, OWNER_ID, OWNER_GENERATION),
        "cross the reset request with an otherwise-valid exact owner drop",
    )
    frames = (
        reset_request,
        drop,
        fs(TERMINAL, "TX_RESULT", 181, 0, TX_RESULT.pack(5, 1, 0, 4), "settle the crossed owner drop without tombstone, quota, or model mutation"),
        fs(CLIENT, "SOFT_RESET_ACK", 181, 1, SOFT_RESET_ACK.pack(1, 0, 0), "retire epoch 0 only after consuming the crossed-drop result"),
    )
    return Scenario(
        "reset_crossed_owner_drop",
        "reset_crossed_owner_drop.hex",
        "reset_crossed_owner_drop_settlement",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 4,
            "last_transaction_id": 4,
            "geometry": {"cols": 2, "rows": 1, "generation": 0},
            "next_client_sequence": 180,
            "next_terminal_sequence": 180,
            "client_to_terminal_credit_before": 86_000,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
        },
        frames,
        {
            "presentation_epoch": 1,
            "global_revision": 0,
            "crossed_status": "ABORTED",
            "old_epoch_revision_after_result": 4,
            "owner_state_unchanged_before_ack": True,
            "tx_abort_emitted": False,
        },
    )


def reset_wrong_tuple_upload_abort() -> Scenario:
    rgba = b"\x11\x22\x33\xff"
    resource_id = 2
    credit_before = 92_000
    begin = fs(
        CLIENT,
        "RESOURCE_BEGIN",
        190,
        0,
        RESOURCE_BEGIN.pack(
            OWNER_ID,
            OWNER_GENERATION,
            resource_id,
            1,
            1,
            1,
            0,
            len(rgba),
            hashlib.sha3_256(rgba).digest(),
        ),
        "open the real session-wide upload before reset",
    )
    begin_credit = credit_before + len(begin.encoded)
    wrong_chunk = fs(
        CLIENT,
        "RESOURCE_CHUNK",
        191,
        0,
        RESOURCE_CHUNK.pack(OWNER_ID, OWNER_GENERATION - 1, resource_id, 0) + rgba,
        "cross reset with a wrong-generation chunk that must not affect the real upload",
    )
    chunk_credit = begin_credit + len(wrong_chunk.encoded)
    abort = fs(
        CLIENT,
        "RESOURCE_ABORT",
        192,
        0,
        RESOURCE_ABORT.pack(OWNER_ID, OWNER_GENERATION, resource_id, 1),
        "abort the preserved exact upload before acknowledging reset",
    )
    frames = (
        begin,
        fs(TERMINAL, "RET_RESULT", 190, 0, ret_result_payload("RESOURCE_BEGIN", STATUS["RET_OK"], 3, item_id=resource_id), "open the exact four-byte upload"),
        fs(TERMINAL, "CREDIT", 191, 0, credit_payload(begin_credit), "release RESOURCE_BEGIN bytes"),
        fs(TERMINAL, "SOFT_RESET_REQUEST", 192, 0, SOFT_RESET_REQUEST.pack(1, 3), "request epoch 1 while the real upload remains open"),
        wrong_chunk,
        fs(
            TERMINAL,
            "RET_RESULT",
            193,
            0,
            RET_RESULT.pack(
                MESSAGE_TYPES["RESOURCE_CHUNK"],
                STATUS["RET_STALE_OWNER"],
                0,
                OWNER_ID,
                OWNER_GENERATION - 1,
                resource_id,
                3,
                0,
            ),
            "echo the offending tuple and preserve the unrelated real upload",
        ),
        fs(TERMINAL, "CREDIT", 194, 0, credit_payload(chunk_credit), "release the consumed wrong-tuple chunk"),
        abort,
        fs(TERMINAL, "RET_RESULT", 195, 0, ret_result_payload("RESOURCE_ABORT", STATUS["RET_ABORTED"], 3, item_id=resource_id), "confirm exact old-epoch upload destruction"),
        fs(CLIENT, "SOFT_RESET_ACK", 193, 1, SOFT_RESET_ACK.pack(1, 0, 0), "acknowledge reset only after exact upload abort settles"),
    )
    return Scenario(
        "reset_wrong_tuple_upload_abort",
        "reset_wrong_tuple_upload_abort.hex",
        "reset_upload_wrong_tuple_noninterference",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "last_transaction_id": 3,
            "next_client_sequence": 190,
            "next_terminal_sequence": 190,
            "client_to_terminal_credit_before": credit_before,
            "no_upload_open": True,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
        },
        frames,
        {
            "presentation_epoch": 1,
            "global_revision": 0,
            "wrong_tuple_status": "RET_STALE_OWNER",
            "upload_preserved_until_exact_abort": True,
            "covering_credits": [begin_credit, chunk_credit],
        },
    )


def soft_reset_replay() -> Scenario:
    epoch = 1
    credit = 85_518
    rgba = b"\x18\x70\xd8\xff\xa8\x78\xff\xff"
    digest = hashlib.sha3_256(rgba).digest()

    reset_request = fs(
        TERMINAL,
        "SOFT_RESET_REQUEST",
        15,
        0,
        SOFT_RESET_REQUEST.pack(epoch, 5),
        "request epoch 1 and discard the old retained plane",
    )
    reset_ack = fs(
        CLIENT,
        "SOFT_RESET_ACK",
        54,
        epoch,
        SOFT_RESET_ACK.pack(epoch, 0, 0),
        "acknowledge reset without restarting directional sequence or credit",
    )

    snapshot_cols = 80
    snapshot_rows = 25
    blank = CELL.pack(ord(" "), 7, 0, 0)
    snapshot_spans = tuple(
        (
            "CELL_SPAN",
            CELL_SPAN_PREFIX.pack(row, 0, snapshot_cols)
            + (CELL.pack(ord("S"), 7, 0, 1) + blank * (snapshot_cols - 1) if row == 0 else blank * snapshot_cols),
        )
        for row in range(snapshot_rows)
    )
    snapshot_payloads = (
        ("SNAPSHOT_BEGIN", SNAPSHOT_BEGIN.pack(1, 0, snapshot_cols, snapshot_rows, snapshot_rows, snapshot_cols * snapshot_rows)),
        *snapshot_spans,
        ("CURSOR", CURSOR.pack(0, 0, 0)),
        ("SNAPSHOT_COMMIT", COMMIT_ID.pack(1)),
    )
    snapshot_frames = tuple(
        fs(CLIENT, name, 55 + index, epoch, payload, "rebuild mandatory CELL state first")
        for index, (name, payload) in enumerate(snapshot_payloads)
    )
    snapshot_bytes = sum(len(frame.encoded) for frame in snapshot_frames)
    credit += snapshot_bytes

    next_client_sequence = 55 + len(snapshot_frames)
    query = fs(CLIENT, "RET_QUERY", next_client_sequence, epoch, RET_QUERY.pack(TAG, 0), "rediscover RETAINED-1 in the new epoch")
    query_credit = credit + len(query.encoded)

    owner_payload = owner_open_payload(
        region_quota=1,
        resource_quota=1,
        object_quota=1,
        series_quota=0,
        resource_bytes=len(rgba),
        utf8_bytes=0,
        sample_slots=0,
    )
    owner = fs(CLIENT, "OWNER_OPEN", next_client_sequence + 1, epoch, owner_payload, "reopen bounded owner authority in epoch 1")
    owner_credit = query_credit + len(owner.encoded)

    resource_begin_payload = RESOURCE_BEGIN.pack(
        OWNER_ID,
        OWNER_GENERATION,
        RESOURCE_ID,
        1,
        2,
        1,
        0,
        len(rgba),
        digest,
    )
    resource_begin = fs(CLIENT, "RESOURCE_BEGIN", next_client_sequence + 2, epoch, resource_begin_payload, "reserve and begin replay of one RGBA resource")
    begin_credit = owner_credit + len(resource_begin.encoded)
    resource_chunk = fs(
        CLIENT,
        "RESOURCE_CHUNK",
        next_client_sequence + 3,
        epoch,
        RESOURCE_CHUNK.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 0) + rgba[:4],
        "replay the first ordered RGBA pixel",
    )
    chunk_credit = begin_credit + len(resource_chunk.encoded)
    resource_chunk_2 = fs(
        CLIENT,
        "RESOURCE_CHUNK",
        next_client_sequence + 4,
        epoch,
        RESOURCE_CHUNK.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 4) + rgba[4:],
        "send the second chunk only after the first covering CREDIT",
    )
    chunk_credit_2 = chunk_credit + len(resource_chunk_2.encoded)
    resource_commit = fs(
        CLIENT,
        "RESOURCE_COMMIT",
        next_client_sequence + 5,
        epoch,
        RESOURCE_COMMIT.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID),
        "validate SHA3-256 and publish the immutable resource",
    )
    commit_credit = chunk_credit_2 + len(resource_commit.encoded)

    start_transaction, start_declared = present_transaction(
        start_sequence=next_client_sequence + 6,
        epoch=epoch,
        transaction_id=2,
        base_revision=1,
        geometry_generation=0,
        cols=snapshot_cols,
        rows=snapshot_rows,
        retained_mode=2,
        disposition=0,
        operations=(region_payload("REGION_DEFINE", cols=snapshot_cols, rows=snapshot_rows),),
    )
    start_credit = commit_credit + start_declared
    continue_transaction, continue_declared = present_transaction(
        start_sequence=next_client_sequence + 9,
        epoch=epoch,
        transaction_id=3,
        base_revision=2,
        geometry_generation=0,
        cols=snapshot_cols,
        rows=snapshot_rows,
        retained_mode=3,
        disposition=1,
        operations=(("OBJECT_DEFINE", image_payload()),),
    )
    reveal_credit = start_credit + continue_declared

    frames = (
        reset_request,
        reset_ack,
        *snapshot_frames,
        fs(TERMINAL, "TX_RESULT", 16, epoch, TX_RESULT.pack(1, 0, 0, 1), "confirm CELL-first revision 1"),
        fs(TERMINAL, "CREDIT", 17, epoch, credit_payload(credit), "release snapshot bytes after TX_RESULT"),
        query,
        fs(TERMINAL, "RET_CAPS", 18, epoch, caps_payload(), "readvertise retained capacities for epoch 1"),
        fs(TERMINAL, "RET_FORMATS", 19, epoch, formats_payload(), "readvertise retained formats for epoch 1"),
        fs(TERMINAL, "CREDIT", 20, epoch, credit_payload(query_credit), "cover query after both replies"),
        owner,
        fs(TERMINAL, "RET_RESULT", 21, epoch, ret_result_payload("OWNER_OPEN", STATUS["RET_OK"], 1), "confirm replay owner reservation"),
        fs(TERMINAL, "CREDIT", 22, epoch, credit_payload(owner_credit), "release OWNER_OPEN bytes"),
        resource_begin,
        fs(TERMINAL, "RET_RESULT", 23, epoch, ret_result_payload("RESOURCE_BEGIN", STATUS["RET_OK"], 1, item_id=RESOURCE_ID), "open bounded resource upload"),
        fs(TERMINAL, "CREDIT", 24, epoch, credit_payload(begin_credit), "release RESOURCE_BEGIN bytes"),
        resource_chunk,
        fs(TERMINAL, "CREDIT", 25, epoch, credit_payload(chunk_credit), "acknowledge the valid chunk only through credit"),
        resource_chunk_2,
        fs(TERMINAL, "CREDIT", 26, epoch, credit_payload(chunk_credit_2), "cover the second ordered chunk before commit"),
        resource_commit,
        fs(TERMINAL, "RET_RESULT", 27, epoch, ret_result_payload("RESOURCE_COMMIT", STATUS["RET_OK"], 1, item_id=RESOURCE_ID, accepted_bytes=len(rgba)), "publish digest-verified resource"),
        fs(TERMINAL, "CREDIT", 28, epoch, credit_payload(commit_credit), "release RESOURCE_COMMIT bytes"),
        *start_transaction,
        fs(TERMINAL, "TX_RESULT", 29, epoch, TX_RESULT.pack(2, 0, 0, 2), "commit the graph-valid hidden reset replacement at revision 2"),
        fs(TERMINAL, "CREDIT", 30, epoch, credit_payload(start_credit), "release reset REPLACE_START after TX_RESULT"),
        *continue_transaction,
        fs(TERMINAL, "TX_RESULT", 31, epoch, TX_RESULT.pack(3, 0, 0, 3), "reveal the completed reset replacement at revision 3"),
        fs(TERMINAL, "CREDIT", 32, epoch, credit_payload(reveal_credit), "release reset REPLACE_CONTINUE after TX_RESULT"),
    )
    return Scenario(
        "soft_reset_replay",
        "soft_reset_replay.hex",
        "reset_resource_scene_replay",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 5,
            "next_client_sequence": 54,
            "next_terminal_sequence": 15,
            "client_to_terminal_credit_before": 85_518,
            "prior_history_complete_bytes": 19_982,
            "old_retained_visible": True,
        },
        frames,
        {
            "presentation_epoch": 1,
            "global_revision": 3,
            "directional_sequences_restarted": False,
            "credit_restarted": False,
            "cell_snapshot_before_discovery": True,
            "resource": {
                "id": RESOURCE_ID,
                "byte_length": len(rgba),
                "sha3_256": digest.hex(),
            },
            "visible_scene": {"regions": [REGION_ID], "objects": [IMAGE_ID]},
            "replace_start_transaction_bytes": start_declared,
            "replace_continue_transaction_bytes": continue_declared,
            "covering_credits": [
                credit,
                query_credit,
                owner_credit,
                begin_credit,
                chunk_credit,
                chunk_credit_2,
                commit_credit,
                start_credit,
                reveal_credit,
            ],
        },
    )


def stale_generation() -> Scenario:
    credit_before = 100_000
    stale = fs(
        CLIENT,
        "OWNER_OPEN",
        60,
        0,
        owner_open_payload(generation=OWNER_GENERATION - 1),
        "attempt to reopen a live owner ID with stale authority",
    )
    credit_after = credit_before + len(stale.encoded)
    over_quota = fs(
        CLIENT,
        "OWNER_OPEN",
        61,
        0,
        OWNER_OPEN.pack(OWNER_ID + 1, 1, CAPS_VALUES["max_regions"] + 1, 0, 0, 0, 0, 0, 0, 0),
        "request a region quota above the advertised global maximum",
    )
    final_credit = credit_after + len(over_quota.encoded)
    frames = (
        stale,
        fs(TERMINAL, "RET_RESULT", 60, 0, ret_result_payload("OWNER_OPEN", STATUS["RET_STALE_OWNER"], 3, generation=OWNER_GENERATION - 1), "reject stale generation without model or quota mutation"),
        fs(TERMINAL, "CREDIT", 61, 0, credit_payload(credit_after), "release rejected OWNER_OPEN bytes"),
        over_quota,
        fs(
            TERMINAL,
            "RET_RESULT",
            62,
            0,
            RET_RESULT.pack(MESSAGE_TYPES["OWNER_OPEN"], STATUS["RET_INVALID"], 0, OWNER_ID + 1, 1, 0, 3, 0),
            "reject quota-above-advertised owner as invalid without ledger or revision mutation",
        ),
        fs(TERMINAL, "CREDIT", 63, 0, credit_payload(final_credit), "release over-quota OWNER_OPEN bytes"),
    )
    return Scenario(
        "stale_generation",
        "stale_generation.hex",
        "lifecycle_rejection",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "next_client_sequence": 60,
            "next_terminal_sequence": 60,
            "client_to_terminal_credit_before": credit_before,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
        },
        frames,
        {
            "ret_status": "RET_STALE_OWNER",
            "over_quota_status": "RET_INVALID",
            "global_revision": 3,
            "quota_ledger_changed": False,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
            "covering_credits": [credit_after, final_credit],
        },
    )


def aggregate_quota_exhaustion() -> Scenario:
    """Reject an individually legal second owner that overcommits totals."""

    credit_before = 101_000
    requested_owner = OWNER_ID + 1
    requested_generation = 1
    requested_quotas = (8, 4, 30, 8, 400_000, 7_000, 1_500)
    request = fs(
        CLIENT,
        "OWNER_OPEN",
        164,
        0,
        OWNER_OPEN.pack(
            requested_owner,
            requested_generation,
            *requested_quotas,
            0,
        ),
        "request individually legal quotas that exceed already-reserved aggregate totals",
    )
    final_credit = credit_before + len(request.encoded)
    frames = (
        request,
        fs(
            TERMINAL,
            "RET_RESULT",
            164,
            0,
            RET_RESULT.pack(
                MESSAGE_TYPES["OWNER_OPEN"],
                STATUS["RET_NO_CAPACITY"],
                0,
                requested_owner,
                requested_generation,
                0,
                4,
                0,
            ),
            "reject aggregate overcommit without allocating an owner record or changing any quota ledger",
        ),
        fs(TERMINAL, "CREDIT", 165, 0, credit_payload(final_credit), "release the rejected but fully consumed OWNER_OPEN frame"),
    )
    return Scenario(
        "aggregate_quota_exhaustion",
        "aggregate_quota_exhaustion.hex",
        "aggregate_owner_quota_rejection",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 4,
            "last_transaction_id": 4,
            "next_client_sequence": 164,
            "next_terminal_sequence": 164,
            "client_to_terminal_credit_before": credit_before,
            "existing_owner": {
                "id": OWNER_ID,
                "generation": OWNER_GENERATION,
                "region_quota": 10,
                "resource_quota": 5,
                "object_quota": 40,
                "series_quota": 10,
                "resource_bytes": 700_000,
                "utf8_bytes": 10_000,
                "sample_slots": 3_000,
            },
        },
        frames,
        {
            "ret_status": "RET_NO_CAPACITY",
            "global_revision": 4,
            "covering_credit": final_credit,
        },
    )


def resource_chunk_overrun() -> Scenario:
    credit_before = 110_000
    rgba = b"\x10\x20\x30\xff"
    digest = hashlib.sha3_256(rgba).digest()
    begin = fs(
        CLIENT,
        "RESOURCE_BEGIN",
        70,
        0,
        RESOURCE_BEGIN.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 1, 1, 1, 0, len(rgba), digest),
        "reserve a four-byte RGBA resource upload",
    )
    begin_credit = credit_before + len(begin.encoded)
    overrun_data = rgba + b"\x00"
    chunk = fs(
        CLIENT,
        "RESOURCE_CHUNK",
        71,
        0,
        RESOURCE_CHUNK.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 0) + overrun_data,
        "send one well-framed chunk whose checked end exceeds declared length",
    )
    chunk_credit = begin_credit + len(chunk.encoded)
    frames = (
        begin,
        fs(TERMINAL, "RET_RESULT", 70, 0, ret_result_payload("RESOURCE_BEGIN", STATUS["RET_OK"], 3, item_id=RESOURCE_ID), "open the bounded upload"),
        fs(TERMINAL, "CREDIT", 71, 0, credit_payload(begin_credit), "release RESOURCE_BEGIN bytes"),
        chunk,
        fs(TERMINAL, "RET_RESULT", 72, 0, ret_result_payload("RESOURCE_CHUNK", STATUS["RET_INVALID"], 3, item_id=RESOURCE_ID), "reject the whole overrun chunk and destroy the upload"),
        fs(TERMINAL, "CREDIT", 73, 0, credit_payload(chunk_credit), "release the consumed invalid chunk bytes"),
    )
    return Scenario(
        "resource_chunk_overrun",
        "resource_chunk_overrun.hex",
        "resource_upload_rejection",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "next_client_sequence": 70,
            "next_terminal_sequence": 70,
            "client_to_terminal_credit_before": credit_before,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
            "no_upload_open": True,
        },
        frames,
        {
            "ret_status": "RET_INVALID",
            "declared_resource_bytes": len(rgba),
            "chunk_bytes": len(overrun_data),
            "upload_open": False,
            "resource_published": False,
            "reservation_released": True,
            "global_revision": 3,
            "covering_credits": [begin_credit, chunk_credit],
        },
    )


def resource_digest_failure() -> Scenario:
    credit_before = 120_000
    rgba = b"\x10\x20\x30\xff"
    correct_digest = hashlib.sha3_256(rgba).digest()
    wrong_digest = bytes([correct_digest[0] ^ 0x01]) + correct_digest[1:]
    begin = fs(
        CLIENT,
        "RESOURCE_BEGIN",
        80,
        0,
        RESOURCE_BEGIN.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 1, 1, 1, 0, len(rgba), wrong_digest),
        "begin an upload with a structurally valid but incorrect declared digest",
    )
    begin_credit = credit_before + len(begin.encoded)
    chunk = fs(
        CLIENT,
        "RESOURCE_CHUNK",
        81,
        0,
        RESOURCE_CHUNK.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID, 0) + rgba,
        "accept exactly the declared four data bytes",
    )
    chunk_credit = begin_credit + len(chunk.encoded)
    commit = fs(
        CLIENT,
        "RESOURCE_COMMIT",
        82,
        0,
        RESOURCE_COMMIT.pack(OWNER_ID, OWNER_GENERATION, RESOURCE_ID),
        "request digest validation and immutable publication",
    )
    commit_credit = chunk_credit + len(commit.encoded)
    frames = (
        begin,
        fs(TERMINAL, "RET_RESULT", 80, 0, ret_result_payload("RESOURCE_BEGIN", STATUS["RET_OK"], 3, item_id=RESOURCE_ID), "open the upload before content validation"),
        fs(TERMINAL, "CREDIT", 81, 0, credit_payload(begin_credit), "release RESOURCE_BEGIN bytes"),
        chunk,
        fs(TERMINAL, "CREDIT", 82, 0, credit_payload(chunk_credit), "acknowledge valid chunk only through covering credit"),
        commit,
        fs(TERMINAL, "RET_RESULT", 83, 0, ret_result_payload("RESOURCE_COMMIT", STATUS["RET_BAD_CONTENT"], 3, item_id=RESOURCE_ID), "reject digest mismatch and destroy upload"),
        fs(TERMINAL, "CREDIT", 84, 0, credit_payload(commit_credit), "release failed RESOURCE_COMMIT bytes"),
    )
    return Scenario(
        "resource_digest_failure",
        "resource_digest_failure.hex",
        "resource_upload_rejection",
        {
            "state": "ACTIVE",
            "presentation_epoch": 0,
            "global_revision": 3,
            "next_client_sequence": 80,
            "next_terminal_sequence": 80,
            "client_to_terminal_credit_before": credit_before,
            "live_owner": {"id": OWNER_ID, "generation": OWNER_GENERATION},
            "no_upload_open": True,
        },
        frames,
        {
            "ret_status": "RET_BAD_CONTENT",
            "declared_sha3_256": wrong_digest.hex(),
            "actual_sha3_256": correct_digest.hex(),
            "accepted_bytes": 0,
            "upload_open": False,
            "resource_published": False,
            "reservation_released": True,
            "global_revision": 3,
            "covering_credits": [begin_credit, chunk_credit, commit_credit],
        },
    )


QUOTA_FIELDS = (
    "region_quota",
    "resource_quota",
    "object_quota",
    "series_quota",
    "resource_bytes",
    "utf8_bytes",
    "sample_slots",
)

QUOTA_LIMITS = {
    "region_quota": CAPS_VALUES["max_regions"],
    "resource_quota": CAPS_VALUES["max_resources"],
    "object_quota": CAPS_VALUES["max_objects"],
    "series_quota": CAPS_VALUES["max_series"],
    "resource_bytes": CAPS_VALUES["total_resource_bytes"],
    "utf8_bytes": FORMAT_VALUES["total_utf8_bytes"],
    "sample_slots": FORMAT_VALUES["total_sample_slots"],
}

def scenarios() -> tuple[Scenario, ...]:
    return (
        query_supported(),
        query_unsupported(),
        soundlab_initial_replace(),
        soundlab_dynamic_append(),
        mutation_and_drop_lifecycle(),
        mixed_commit_and_rejections(),
        legacy_cell_and_replace_continue(),
        resize_layout_sync(),
        reset_crossed_present_commit(),
        reset_crossed_owner_drop(),
        reset_wrong_tuple_upload_abort(),
        soft_reset_replay(),
        owner_drop_tombstone(),
        control_reserve_boundary(),
        stale_generation(),
        aggregate_quota_exhaustion(),
        resource_lifecycle(),
        resource_chunk_overrun(),
        resource_digest_failure(),
    )


def expected_manifest() -> dict[str, Any]:
    invariant = {
        "ret_query_supported": "CAPS then FORMATS precede the exact covering CREDIT and require an initial hidden replacement",
        "ret_query_unsupported": "the first covering CREDIT is the deterministic negative answer with no retained reply",
        "soundlab_initial_replace": "full SoundLab scene commits hidden through REPLACE_START and REPLACE_CONTINUE, reveals at revision 3, then accepts an unchanged legacy CELL transaction at revision 4",
        "soundlab_dynamic_append": "value and series history change without retransmitting any static definition",
        "mutation_and_drop_lifecycle": "complete GLYPH_RUN replacement then graph-safe object, series, and region drops leave exact owner authority live",
        "mixed_commit_and_rejections": "CELL and retained state commit atomically; retained-only rejections recover, while the final rejected mixed transaction requires SESSION_LOST",
        "legacy_cell_and_replace_continue": "unchanged CELL TX interleaves in the global revision domain before hidden REPLACE_START and REPLACE_CONTINUE reveal",
        "resize_layout_sync": "canonical full-width PRESENT CELL_REPLACE precedes hidden LAYOUT_START and zero-operation LAYOUT_CONTINUE reveal",
        "reset_crossed_present_commit": "an already-requested reset settles a crossed valid PRESENT_COMMIT as old-epoch status 1 before ACK without mutation",
        "reset_crossed_owner_drop": "an already-requested reset settles a crossed valid OWNER_DROP as old-epoch status 1 before ACK without dropping authority",
        "reset_wrong_tuple_upload_abort": "one crossed wrong-generation CHUNK preserves the real upload, then exact old-epoch RESOURCE_ABORT completes before ACK",
        "soft_reset_replay": "sequence and credit continue while CELL-first rebuild, rediscovery, resource replay, REPLACE_START, and REPLACE_CONTINUE restart epoch state",
        "owner_drop_tombstone": "exact and repeated tombstone drops advance revisions, stale authority and stale base are recoverable, and only a newer generation reopens",
        "control_reserve_boundary": "zero ordinary credit blocks bulk RESOURCE_BEGIN while reserve-backed OWNER_DROP/TX_RESULT retires authority before CREDIT-enabled reopen",
        "stale_generation": "stale OWNER_OPEN returns RET_STALE_OWNER without quota or revision mutation",
        "aggregate_quota_exhaustion": "an individually valid second OWNER_OPEN returns RET_NO_CAPACITY because immutable aggregate reservations would overflow",
        "resource_lifecycle": "wrong-resource COMMIT noninterference, in-use drop, bad and good abort, reference removal, and successful resource drop retain exact lifecycle boundaries",
        "resource_chunk_overrun": "a whole overrun chunk returns RET_INVALID, destroys upload state, and releases its reservation",
        "resource_digest_failure": "digest mismatch returns RET_BAD_CONTENT with zero accepted bytes and no resource publication",
    }

    def summary(scenario: Scenario) -> dict[str, Any]:
        credits = scenario.expected.get("covering_credits")
        if credits is None and "covering_credit" in scenario.expected:
            credits = [scenario.expected["covering_credit"]]
        entry: dict[str, Any] = {
            "name": scenario.name,
            "file": scenario.file,
            "meta_file": f"{Path(scenario.file).stem}.meta.json",
            "state_file": f"{Path(scenario.file).stem}.state.json",
            "kind": scenario.kind,
            "frame_count": len(scenario.frames),
            "invariant": invariant[scenario.name],
        }
        if credits is not None:
            entry["expected_credit_watermarks"] = credits
        if "global_revision" in scenario.expected:
            entry["expected_global_revision"] = scenario.expected["global_revision"]
        if "ret_status" in scenario.expected:
            entry["expected_status"] = scenario.expected["ret_status"]
        if "over_quota_status" in scenario.expected:
            entry["expected_over_quota_status"] = scenario.expected["over_quota_status"]
        return entry

    return {
        "contract_id": CONTRACT_ID,
        "base_contract_id": BASE_CONTRACT_ID,
        "byte_order": "little-endian",
        "header": {
            "size": HEADER_BYTES,
            "magic_hex": MAGIC.hex(),
            "reserved0": 0,
            "crc": "CRC-32C over header bytes 0..35 followed by payload",
        },
        "canonical_session": f"0x{SESSION:016x}",
        "message_types": {
            name: f"0x{value:04x}" for name, value in sorted(RETAINED_MESSAGE_TYPES.items(), key=lambda item: item[1])
        },
        "status_values": STATUS,
        "feature_bits": FEATURE_BITS,
        "object_types": OBJECT_TYPES,
        "canonical_terminal_policy": {
            "base_max_payload": MAX_PAYLOAD,
            "base_max_transaction_bytes": BASE_MAX_TRANSACTION,
            "client_receive_credit": CLIENT_RECEIVE_CREDIT,
            "features": f"0x{FEATURES:016x}",
            **CAPS_VALUES,
            **FORMAT_VALUES,
        },
        "fixture_format": "one complete frame per nonempty ASCII-hex line",
        "semantic_meta_format": "independent directions plus exact initial state and two cumulative credit ledgers",
        "state_sidecar_format": "independent deduplicated exact snapshots with one ordered state reference after every consumed frame",
        "transcripts": [summary(scenario) for scenario in scenarios()],
    }


def fail(message: str) -> None:
    raise AssertionError(message)


def check_equal(label: str, actual: Any, expected: Any) -> None:
    if actual != expected:
        fail(f"{label} differs\nexpected: {expected!r}\nactual:   {actual!r}")


def decode_header(frame: bytes) -> dict[str, int | bytes]:
    if len(frame) < HEADER_BYTES:
        raise ValueError("frame shorter than APT-1 header")
    magic, reserved0, header_bytes, type_id, flags, reserved, payload_length, session, sequence, epoch, checksum = HEADER.unpack_from(frame)
    return {
        "magic": magic,
        "reserved0": reserved0,
        "header_bytes": header_bytes,
        "type": type_id,
        "flags": flags,
        "reserved": reserved,
        "payload_length": payload_length,
        "session": session,
        "sequence": sequence,
        "epoch": epoch,
        "checksum": checksum,
    }


def read_hex_lines(path: Path) -> tuple[bytes, ...]:
    lines = tuple(line.strip() for line in path.read_text(encoding="ascii").splitlines())
    if not lines or any(not line for line in lines):
        raise ValueError(f"{path.name}: expected one or more nonempty frame lines")
    decoded: list[bytes] = []
    for line_number, line in enumerate(lines, start=1):
        if len(line) % 2:
            raise ValueError(f"{path.name}:{line_number}: odd hexadecimal length")
        try:
            decoded.append(bytes.fromhex(line))
        except ValueError as exc:
            raise ValueError(f"{path.name}:{line_number}: invalid hexadecimal") from exc
    return tuple(decoded)


def validate_frame(label: str, spec: FrameSpec, actual: bytes) -> None:
    expected = spec.encoded
    check_equal(f"{label} exact bytes", actual, expected)
    header = decode_header(actual)
    check_equal(f"{label} magic", header["magic"], MAGIC)
    check_equal(f"{label} reserved0", header["reserved0"], 0)
    check_equal(f"{label} header size", header["header_bytes"], HEADER_BYTES)
    check_equal(f"{label} type", header["type"], MESSAGE_TYPES[spec.message])
    check_equal(f"{label} flags", header["flags"], 0)
    check_equal(f"{label} reserved", header["reserved"], 0)
    check_equal(f"{label} session", header["session"], SESSION)
    check_equal(f"{label} sequence", header["sequence"], spec.sequence)
    check_equal(f"{label} epoch", header["epoch"], spec.epoch)
    check_equal(f"{label} payload length", header["payload_length"], len(spec.payload))
    check_equal(f"{label} complete length", len(actual), HEADER_BYTES + len(spec.payload))
    check_equal(f"{label} payload", actual[HEADER_BYTES:], spec.payload)
    check_equal(f"{label} CRC-32C", header["checksum"], crc32c(actual[:36] + actual[40:]))


def validate_sequence_space(scenario: Scenario) -> None:
    next_sequence = {
        CLIENT: scenario.precondition["next_client_sequence"],
        TERMINAL: scenario.precondition["next_terminal_sequence"],
    }
    for frame in scenario.frames:
        check_equal(
            f"{scenario.name} {frame.direction} sequence for {frame.message}",
            frame.sequence,
            next_sequence[frame.direction],
        )
        next_sequence[frame.direction] += 1


def decoded_credit(frame: FrameSpec) -> int:
    if frame.message != "CREDIT":
        raise ValueError("credit decoder used for a non-credit frame")
    return CREDIT.unpack(frame.payload)[0]


def find_frames(scenario: Scenario, message: str) -> tuple[FrameSpec, ...]:
    return tuple(frame for frame in scenario.frames if frame.message == message)


def validate_discovery(supported: Scenario, unsupported: Scenario) -> None:
    supported_messages = [frame.message for frame in supported.frames]
    check_equal(
        "supported discovery order",
        supported_messages,
        ["RET_QUERY", "RET_CAPS", "RET_FORMATS", "CREDIT"],
    )
    unsupported_messages = [frame.message for frame in unsupported.frames]
    check_equal("unsupported discovery order", unsupported_messages, ["RET_QUERY", "CREDIT"])
    expected_credit = QUERY_CREDIT_BEFORE + frame_bytes("RET_QUERY", RET_QUERY.pack(TAG, 0))
    check_equal("supported query covering credit", decoded_credit(supported.frames[-1]), expected_credit)
    check_equal("unsupported query covering credit", decoded_credit(unsupported.frames[-1]), expected_credit)
    check_equal("CAPS exact payload bytes", len(supported.frames[1].payload), 64)
    check_equal("FORMATS exact payload bytes", len(supported.frames[2].payload), 64)
    check_equal("query exact payload bytes", len(supported.frames[0].payload), 8)
    if decoded_credit(supported.frames[-1]) <= QUERY_CREDIT_BEFORE:
        fail("supported discovery did not restore the complete query frame")
    caps = RET_CAPS.unpack(supported.frames[1].payload)
    check_equal(
        "RET_CAPS fields",
        caps,
        (
            TAG,
            0,
            0,
            FEATURES,
            CAPS_VALUES["max_owner_records"],
            CAPS_VALUES["max_live_owners"],
            CAPS_VALUES["max_regions"],
            CAPS_VALUES["max_resources"],
            CAPS_VALUES["max_objects"],
            CAPS_VALUES["max_series"],
            CAPS_VALUES["max_operations_per_transaction"],
            CAPS_VALUES["max_resource_chunk_bytes"],
            CAPS_VALUES["max_retained_transaction_bytes"],
            CAPS_VALUES["total_resource_bytes"],
        ),
    )
    formats = RET_FORMATS.unpack(supported.frames[2].payload)
    check_equal(
        "RET_FORMATS fields",
        formats,
        (
            FORMAT_VALUES["coordinate_format"],
            FORMAT_VALUES["color_format"],
            FORMAT_VALUES["image_format"],
            FORMAT_VALUES["max_image_width"],
            FORMAT_VALUES["max_image_height"],
            FORMAT_VALUES["max_path_points"],
            FORMAT_VALUES["max_glyph_run_bytes"],
            FORMAT_VALUES["max_samples_per_append"],
            FORMAT_VALUES["max_history_per_series"],
            FORMAT_VALUES["minimum_presentation_interval_us"],
            FORMAT_VALUES["total_sample_slots"],
            FORMAT_VALUES["total_utf8_bytes"],
            0,
        ),
    )
    if supported.precondition["terminal_to_client_available_credit"] < 2 * (HEADER_BYTES + 64):
        fail("supported discovery lacks the required 208 terminal outbound ordinary bytes")
    if caps[12] > BASE_MAX_TRANSACTION or caps[11] + RESOURCE_CHUNK.size > MAX_PAYLOAD:
        fail("canonical discovery maxima are internally inconsistent with base negotiation")


def declared_present_bytes(scenario: Scenario) -> tuple[int, int]:
    begin_index = next(index for index, frame in enumerate(scenario.frames) if frame.message == "PRESENT_BEGIN")
    commit_index = next(index for index in range(begin_index, len(scenario.frames)) if scenario.frames[index].message == "PRESENT_COMMIT")
    transaction = scenario.frames[begin_index : commit_index + 1]
    begin_values = PRESENT_BEGIN.unpack(transaction[0].payload)
    declared = begin_values[3]
    actual = sum(len(frame.encoded) for frame in transaction)
    check_equal(f"{scenario.name} declared transaction bytes", declared, actual)
    check_equal(f"{scenario.name} retained operation count", begin_values[8], len(transaction) - 2)
    return declared, begin_values[10]


def validate_initial_and_dynamic(initial: Scenario, dynamic: Scenario) -> None:
    replacement_start, replacement_continue = output_update_segments(initial)
    start = PRESENT_BEGIN.unpack(replacement_start[0].payload)
    continuation = PRESENT_BEGIN.unpack(replacement_continue[0].payload)
    check_equal("initial replacement START mode/base", (start[0],start[1],start[10]), (2,1,2))
    check_equal("initial replacement CONTINUE mode/base", (continuation[0],continuation[1],continuation[10]), (3,2,3))
    check_equal("initial replacement START bytes", start[3], initial.expected["replace_start_transaction_bytes"])
    check_equal("initial replacement CONTINUE bytes", continuation[3], initial.expected["replace_continue_transaction_bytes"])
    check_equal("initial hidden START disposition", PRESENT_COMMIT.unpack(replacement_start[-1].payload)[1], 0)
    check_equal("initial reveal CONTINUE disposition", PRESENT_COMMIT.unpack(replacement_continue[-1].payload)[1], 1)
    initial_client_messages = [frame.message for frame in initial.frames if frame.direction == CLIENT]
    check_equal("initial full object-family count", initial_client_messages.count("OBJECT_DEFINE"), 8)
    check_equal("initial replacement transaction count", initial_client_messages.count("PRESENT_BEGIN"), 2)
    check_equal("initial legacy CELL interleave", initial_client_messages[-4:], ["TX_BEGIN","CELL_SPAN","CURSOR","TX_COMMIT"])
    check_equal(
        "initial transaction results",
        [TX_RESULT.unpack(frame.payload) for frame in find_frames(initial,"TX_RESULT")],
        [(2,0,0,2),(3,0,0,3),(4,0,0,4)],
    )

    dynamic_declared, dynamic_mode = declared_present_bytes(dynamic)
    check_equal("dynamic retained mode", dynamic_mode, 1)
    check_equal("dynamic manifest transaction bytes", dynamic.expected["declared_transaction_bytes"], dynamic_declared)
    dynamic_client_messages = [frame.message for frame in dynamic.frames if frame.direction == CLIENT]
    check_equal(
        "dynamic frames contain mutations only",
        dynamic_client_messages,
        [
            "PRESENT_BEGIN",
            "OBJECT_SET_VALUE",
            "OBJECT_SET_VALUE",
            "OBJECT_SET_VALUE",
            "OBJECT_SET_VISIBILITY",
            "SERIES_APPEND",
            "SERIES_APPEND",
            "PRESENT_COMMIT",
        ],
    )
    forbidden = {"REGION_DEFINE", "REGION_REPLACE", "OBJECT_DEFINE", "OBJECT_REPLACE", "SERIES_DEFINE", "SERIES_REPLACE"}
    if forbidden.intersection(dynamic_client_messages):
        fail("dynamic fixture retransmits a static definition")
    set_values = [OBJECT_SET_VALUE.unpack(frame.payload) for frame in find_frames(dynamic, "OBJECT_SET_VALUE")]
    check_equal(
        "dynamic semantic values",
        set_values,
        [
            (OWNER_ID, OWNER_GENERATION, READOUT_ID, -300),
            (OWNER_ID, OWNER_GENERATION, METER_ID, -300),
            (OWNER_ID, OWNER_GENERATION, STATUS_ID, 1),
        ],
    )
    check_equal(
        "dynamic GLYPH_RUN visibility",
        OBJECT_SET_VISIBILITY.unpack(find_frames(dynamic, "OBJECT_SET_VISIBILITY")[0].payload),
        (OWNER_ID, OWNER_GENERATION, GLYPH_RUN_ID, 0),
    )
    uniform_append, explicit_append = find_frames(dynamic, "SERIES_APPEND")
    owner, generation, series, count, mode, first_timestamp = SERIES_SAMPLES.unpack_from(uniform_append.payload)
    check_equal("dynamic append identity", (owner, generation, series), (OWNER_ID, OWNER_GENERATION, SERIES_ID))
    check_equal("dynamic append header", (count, mode, first_timestamp), (2, 1, 2_500_000))
    check_equal("dynamic append values", struct.unpack_from("<2q", uniform_append.payload, SERIES_SAMPLES.size), (-300, 100))
    owner, generation, series, count, mode, first_timestamp = SERIES_SAMPLES.unpack_from(explicit_append.payload)
    check_equal("explicit append identity", (owner, generation, series), (OWNER_ID, OWNER_GENERATION, EXPLICIT_SERIES_ID))
    check_equal("explicit append header", (count, mode, first_timestamp), (2, 0, 0))
    check_equal(
        "explicit append samples",
        (
            struct.unpack_from("<Qq", explicit_append.payload, SERIES_SAMPLES.size),
            struct.unpack_from("<Qq", explicit_append.payload, SERIES_SAMPLES.size + 16),
        ),
        ((2_500_000, -1_000), (3_250_000, 1_000)),
    )
    check_equal(
        "uniform bounded-ring eviction",
        dynamic.expected["uniform_ring_after_eviction"],
        [
            {"timestamp_us": 1_500_000, "value": -900},
            {"timestamp_us": 2_000_000, "value": -600},
            {"timestamp_us": 2_500_000, "value": -300},
            {"timestamp_us": 3_000_000, "value": 100},
        ],
    )
    check_equal(
        "explicit bounded-ring eviction",
        dynamic.expected["explicit_ring_after_eviction"],
        [
            {"timestamp_us": 1_750_000, "value": 2_000},
            {"timestamp_us": 2_500_000, "value": -1_000},
            {"timestamp_us": 3_250_000, "value": 1_000},
        ],
    )


def validate_object_bodies(initial: Scenario) -> None:
    check_equal(
        "initial REGION_DEFINE fields",
        REGION.unpack(find_frames(initial, "REGION_DEFINE")[0].payload),
        (OWNER_ID, OWNER_GENERATION, REGION_ID, 0, 0, 80, 25, 0, 3),
    )
    owner_values = OWNER_OPEN.unpack(find_frames(initial, "OWNER_OPEN")[0].payload)
    check_equal("SoundLab owner object/series quotas", owner_values[4:6], (12, 2))
    check_equal("SoundLab owner text/sample quotas", (owner_values[7], owner_values[8]), (128, 7))

    series_defines = find_frames(initial, "SERIES_DEFINE")
    check_equal("series definition count", len(series_defines), 2)
    check_equal(
        "uniform series definition",
        SERIES_DEFINE.unpack(series_defines[0].payload),
        (OWNER_ID, OWNER_GENERATION, SERIES_ID, 4, 1, 500_000),
    )
    check_equal(
        "explicit series definition",
        SERIES_DEFINE.unpack(series_defines[1].payload),
        (OWNER_ID, OWNER_GENERATION, EXPLICIT_SERIES_ID, 3, 0, 0),
    )
    uniform_replace, explicit_replace = find_frames(initial, "SERIES_REPLACE")
    uniform_header = SERIES_SAMPLES.unpack_from(uniform_replace.payload)
    check_equal("uniform SERIES_REPLACE header", uniform_header, (OWNER_ID, OWNER_GENERATION, SERIES_ID, 3, 1, 1_000_000))
    check_equal("uniform SERIES_REPLACE values", struct.unpack_from("<3q", uniform_replace.payload, SERIES_SAMPLES.size), (-1_200, -900, -600))
    explicit_header = SERIES_SAMPLES.unpack_from(explicit_replace.payload)
    check_equal("explicit SERIES_REPLACE header", explicit_header, (OWNER_ID, OWNER_GENERATION, EXPLICIT_SERIES_ID, 2, 0, 0))
    check_equal(
        "explicit SERIES_REPLACE samples",
        (
            struct.unpack_from("<Qq", explicit_replace.payload, SERIES_SAMPLES.size),
            struct.unpack_from("<Qq", explicit_replace.payload, SERIES_SAMPLES.size + 16),
        ),
        ((1_000_000, -2_000), (1_750_000, 2_000)),
    )

    definitions = find_frames(initial, "OBJECT_DEFINE")
    check_equal("non-image SoundLab object family count", len(definitions), 8)
    expected_ids_types = (
        (GROUP_ID, OBJECT_TYPES["GROUP"]),
        (POLYLINE_ID, OBJECT_TYPES["POLYLINE"]),
        (GLYPH_RUN_ID, OBJECT_TYPES["GLYPH_RUN"]),
        (READOUT_ID, OBJECT_TYPES["READOUT"]),
        (METER_ID, OBJECT_TYPES["METER"]),
        (STATUS_ID, OBJECT_TYPES["STATUS"]),
        (PLOT_ID, OBJECT_TYPES["PLOT"]),
        (WAVEFORM_ID, OBJECT_TYPES["WAVEFORM"]),
    )
    for frame, (object_id, object_type) in zip(definitions, expected_ids_types, strict=True):
        prefix = OBJECT_PREFIX.unpack_from(frame.payload)
        check_equal(f"object {object_id} authority", prefix[:3], (OWNER_ID, OWNER_GENERATION, object_id))
        check_equal(f"object {object_id} type", prefix[3], object_type)
        check_equal(f"object {object_id} visibility flags", prefix[4], 1)
        check_equal(f"object {object_id} region", prefix[6], REGION_ID)
        if not prefix[8] < prefix[10] or not prefix[9] < prefix[11]:
            fail(f"object {object_id} has noncanonical UNORM bounds")

    group_body = definitions[0].payload[OBJECT_PREFIX.size :]
    check_equal("GROUP exact empty body", group_body, b"")

    polyline_body = definitions[1].payload[OBJECT_PREFIX.size :]
    point_count, stroke_width, red, green, blue, alpha, path_flags = POLYLINE_BODY.unpack_from(polyline_body)
    check_equal("POLYLINE scalar fields", (point_count, stroke_width, red, green, blue, alpha, path_flags), (3, 0x02000000, 0x53, 0xD8, 0xFB, 0xFF, 0))
    check_equal("POLYLINE exact body length", len(polyline_body), POLYLINE_BODY.size + point_count * POINT.size)
    check_equal(
        "POLYLINE points",
        tuple(POINT.unpack_from(polyline_body, POLYLINE_BODY.size + index * POINT.size) for index in range(point_count)),
        ((0, 0xFFFFFFFF), (0x7FFFFFFF, 0), (0xFFFFFFFF, 0xBFFFFFFF)),
    )

    glyph_run_body = definitions[2].payload[OBJECT_PREFIX.size :]
    glyph_run_values = GLYPH_RUN_BODY.unpack_from(glyph_run_body)
    check_equal(
        "GLYPH_RUN background/attributes/reserved/length",
        glyph_run_values[4:],
        (0x10, 0x18, 0x28, 0xFF, 0, 0, 8),
    )
    check_equal(
        "GLYPH_RUN UTF-8",
        glyph_run_body[GLYPH_RUN_BODY.size :],
        b"SoundLab",
    )
    check_equal(
        "GLYPH_RUN exact body length",
        len(glyph_run_body),
        GLYPH_RUN_BODY.size + glyph_run_values[10],
    )

    readout_body = definitions[3].payload[OBJECT_PREFIX.size :]
    readout_values = READOUT_BODY.unpack_from(readout_body)
    check_equal("READOUT format/value/scale", readout_values[8:12], (1, 2, -1_200, 100))
    check_equal("READOUT unit metadata", readout_values[12:14], (2, 0))
    check_equal("READOUT unit", readout_body[READOUT_BODY.size :], b"dB")

    meter_body = definitions[4].payload[OBJECT_PREFIX.size :]
    meter_values = METER_BODY.unpack(meter_body)
    check_equal("METER orientation/flags/range/value/reserved", meter_values[8:], (0, 1, -6_000, 0, -1_200, 0))

    status_body = definitions[5].payload[OBJECT_PREFIX.size :]
    status_values = STATUS_BODY.unpack(status_body)
    check_equal("STATUS value/shape/flags/reserved", status_values[8:], (0, 0, 0, 0))

    plot_body = definitions[6].payload[OBJECT_PREFIX.size :]
    plot_values = PLOT_BODY.unpack(plot_body)
    check_equal("PLOT series/range", plot_values[:3], (SERIES_ID, -32_768, 32_767))
    check_equal("PLOT flags/reserved", plot_values[-2:], (2, 0))

    waveform_body = definitions[7].payload[OBJECT_PREFIX.size :]
    waveform_values = WAVEFORM_BODY.unpack(waveform_body)
    check_equal("WAVEFORM series/range", waveform_values[:3], (EXPLICIT_SERIES_ID, -32_768, 32_767))
    check_equal("WAVEFORM zero/flags/reserved", waveform_values[-3:], (0, 1, 0))


def output_update_segments(scenario: Scenario) -> tuple[tuple[FrameSpec, ...], ...]:
    segments: list[tuple[FrameSpec, ...]] = []
    index = 0
    while index < len(scenario.frames):
        if scenario.frames[index].message != "PRESENT_BEGIN":
            index += 1
            continue
        end = index + 1
        while end < len(scenario.frames) and scenario.frames[end].message != "PRESENT_COMMIT":
            end += 1
        if end == len(scenario.frames):
            fail(f"{scenario.name} contains unterminated PRESENT transaction")
        segments.append(scenario.frames[index : end + 1])
        index = end + 1
    return tuple(segments)


def validate_mutation_and_drops(scenario: Scenario) -> None:
    replace, drops = output_update_segments(scenario)
    check_equal("mutation/drop transaction count", len((replace, drops)), 2)
    check_equal("replacement body messages", [frame.message for frame in replace], ["PRESENT_BEGIN", "OBJECT_REPLACE", "PRESENT_COMMIT"])
    replacement_prefix = OBJECT_PREFIX.unpack_from(replace[1].payload)
    check_equal(
        "OBJECT_REPLACE identity/type",
        (replacement_prefix[2], replacement_prefix[3]),
        (GLYPH_RUN_ID, OBJECT_TYPES["GLYPH_RUN"]),
    )
    replacement_run = replace[1].payload[OBJECT_PREFIX.size :]
    replacement_meta = GLYPH_RUN_BODY.unpack_from(replacement_run)
    check_equal(
        "OBJECT_REPLACE glyph-run bytes",
        replacement_run[GLYPH_RUN_BODY.size :],
        b"SoundLab armed",
    )
    check_equal(
        "OBJECT_REPLACE exact length",
        len(replacement_run),
        GLYPH_RUN_BODY.size + replacement_meta[10],
    )

    drop_messages = [frame.message for frame in drops]
    check_equal(
        "drop family messages",
        drop_messages,
        ["PRESENT_BEGIN"] + ["OBJECT_DROP"] * 8 + ["SERIES_DROP", "SERIES_DROP", "REGION_DROP", "PRESENT_COMMIT"],
    )
    object_ids = [OWNER_ITEM.unpack(frame.payload)[2] for frame in drops if frame.message == "OBJECT_DROP"]
    check_equal(
        "dropped object identities",
        object_ids,
        [
            POLYLINE_ID,
            GLYPH_RUN_ID,
            GROUP_ID,
            READOUT_ID,
            METER_ID,
            STATUS_ID,
            PLOT_ID,
            WAVEFORM_ID,
        ],
    )
    series_ids = [OWNER_ITEM.unpack(frame.payload)[2] for frame in drops if frame.message == "SERIES_DROP"]
    check_equal("dropped series identities", series_ids, [SERIES_ID, EXPLICIT_SERIES_ID])
    check_equal("dropped region identity", OWNER_ITEM.unpack(next(frame.payload for frame in drops if frame.message == "REGION_DROP"))[2], REGION_ID)
    results = [TX_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "TX_RESULT")]
    check_equal("mutation/drop results", results, [(4, 0, 0, 4), (5, 0, 0, 5)])


def validate_owner_tombstone(scenario: Scenario) -> None:
    drops = find_frames(scenario, "OWNER_DROP")
    check_equal("owner drop request count", len(drops), 4)
    check_equal("exact live drop", OWNER_DROP.unpack(drops[0].payload), (4, 3, OWNER_ID, OWNER_GENERATION))
    check_equal("exact tombstone repeat", OWNER_DROP.unpack(drops[1].payload), (5, 4, OWNER_ID, OWNER_GENERATION))
    check_equal("stale tombstone drop", OWNER_DROP.unpack(drops[2].payload), (6, 5, OWNER_ID, OWNER_GENERATION - 1))
    check_equal("stale-base tombstone drop", OWNER_DROP.unpack(drops[3].payload), (7, 4, OWNER_ID, OWNER_GENERATION))
    results = [TX_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "TX_RESULT")]
    check_equal("owner drop results", results, [(4, 0, 0, 4), (5, 0, 0, 5), (6, 2, 0, 5), (7, 3, 0, 5)])
    reopen = OWNER_OPEN.unpack(find_frames(scenario, "OWNER_OPEN")[0].payload)
    check_equal("newer generation tombstone reopen", reopen[:2], (OWNER_ID, OWNER_GENERATION + 1))
    result = RET_RESULT.unpack(find_frames(scenario, "RET_RESULT")[0].payload)
    check_equal("newer generation reopen result", (result[0], result[1], result[4], result[6]), (MESSAGE_TYPES["OWNER_OPEN"], STATUS["RET_OK"], OWNER_GENERATION + 1, 5))
    check_equal("OWNER_DROP reserve does not affect ordinary credit", decoded_credit(find_frames(scenario, "CREDIT")[0]), scenario.precondition["client_to_terminal_credit_before"] + len(find_frames(scenario, "OWNER_OPEN")[0].encoded))


def validate_control_reserve_boundary(scenario: Scenario) -> None:
    check_equal("reserve boundary starts at zero ordinary credit", scenario.precondition["ordinary_credit_available"], 0)
    check_equal("reserve boundary pending bulk frame", scenario.precondition["pending_resource_begin_complete_bytes"], 120)
    if find_frames(scenario, "RESOURCE_BEGIN"):
        fail("reserve boundary emitted bulk RESOURCE_BEGIN with zero ordinary credit")
    owner_drop = find_frames(scenario, "OWNER_DROP")[0]
    check_equal("reserve boundary exact OWNER_DROP", OWNER_DROP.unpack(owner_drop.payload), (4, 3, OWNER_ID, OWNER_GENERATION))
    check_equal("reserve boundary OWNER_DROP class", owner_drop.ordinary, False)
    result = find_frames(scenario, "TX_RESULT")[0]
    check_equal("reserve boundary exact TX_RESULT", TX_RESULT.unpack(result.payload), (4, 0, 0, 4))
    check_equal("reserve boundary TX_RESULT class", result.ordinary, False)
    first_credit, second_credit = find_frames(scenario, "CREDIT")
    reopen = find_frames(scenario, "OWNER_OPEN")[0]
    grant_before = scenario.precondition["client_to_terminal_cumulative_grant"]
    check_equal("reserve boundary first grant", decoded_credit(first_credit), grant_before + len(reopen.encoded))
    check_equal("reserve boundary reopen exact admission", len(reopen.encoded), 104)
    check_equal("reserve boundary released grant", decoded_credit(second_credit), grant_before + 2 * len(reopen.encoded))
    check_equal("reserve boundary newer generation", OWNER_OPEN.unpack(reopen.payload)[:2], (OWNER_ID, OWNER_GENERATION + 1))


def validate_resource_lifecycle(scenario: Scenario) -> None:
    check_equal(
        "resource lifecycle owner-scoped local IDs",
        scenario.precondition["second_live_owner_reuses_region_object_series_id_1"],
        True,
    )
    resource_drops = find_frames(scenario, "RESOURCE_DROP")
    check_equal("resource drop identities", [OWNER_ITEM.unpack(frame.payload) for frame in resource_drops], [(OWNER_ID, OWNER_GENERATION, RESOURCE_ID)] * 2)
    drop_results = [
        RET_RESULT.unpack(frame.payload)
        for frame in find_frames(scenario, "RET_RESULT")
        if RET_RESULT.unpack(frame.payload)[0] == MESSAGE_TYPES["RESOURCE_DROP"]
    ]
    check_equal("resource in-use/success statuses", [result[1] for result in drop_results], [STATUS["RET_IN_USE"], STATUS["RET_OK"]])

    aborts = find_frames(scenario, "RESOURCE_ABORT")
    check_equal("RESOURCE_ABORT exact payloads", [RESOURCE_ABORT.unpack(frame.payload) for frame in aborts], [(OWNER_ID, OWNER_GENERATION, 2, 9), (OWNER_ID, OWNER_GENERATION, 2, 0)])
    abort_results = [
        RET_RESULT.unpack(frame.payload)
        for frame in find_frames(scenario, "RET_RESULT")
        if RET_RESULT.unpack(frame.payload)[0] == MESSAGE_TYPES["RESOURCE_ABORT"]
    ]
    check_equal("bad/good abort statuses", [result[1] for result in abort_results], [STATUS["RET_INVALID"], STATUS["RET_ABORTED"]])
    commits = find_frames(scenario, "RESOURCE_COMMIT")
    check_equal(
        "wrong-resource COMMIT tuple",
        [RESOURCE_COMMIT.unpack(frame.payload) for frame in commits],
        [(OWNER_ID, OWNER_GENERATION, 3)],
    )
    commit_results = [
        RET_RESULT.unpack(frame.payload)
        for frame in find_frames(scenario, "RET_RESULT")
        if RET_RESULT.unpack(frame.payload)[0] == MESSAGE_TYPES["RESOURCE_COMMIT"]
    ]
    check_equal(
        "wrong-resource COMMIT result preserves exact upload",
        [(result[1], result[3], result[4], result[5], result[6]) for result in commit_results],
        [(STATUS["RET_INVALID"], OWNER_ID, OWNER_GENERATION, 3, 2)],
    )
    object_drop = find_frames(scenario, "OBJECT_DROP")
    check_equal("resource lifecycle IMAGE drop", [OWNER_ITEM.unpack(frame.payload) for frame in object_drop], [(OWNER_ID, OWNER_GENERATION, IMAGE_ID)])


def validate_mixed_and_rejections(scenario: Scenario) -> None:
    mixed, bad_bytes, bad_count, bad_timestamp, missing_reference, lost_mixed = output_update_segments(scenario)
    mixed_begin = PRESENT_BEGIN.unpack(mixed[0].payload)
    check_equal("mixed CELL/retained counts", mixed_begin[6:11], (1, 1, 1, 1, 1))
    check_equal("mixed frame order", [frame.message for frame in mixed], ["PRESENT_BEGIN", "CELL_SPAN", "CURSOR", "OBJECT_SET_VALUE", "PRESENT_COMMIT"])
    check_equal("mixed declared bytes", mixed_begin[3], sum(len(frame.encoded) for frame in mixed))

    bad_bytes_begin = PRESENT_BEGIN.unpack(bad_bytes[0].payload)
    check_equal("bad declared byte sentinel", bad_bytes_begin[3], sum(len(frame.encoded) for frame in bad_bytes) + 1)
    bad_count_begin = PRESENT_BEGIN.unpack(bad_count[0].payload)
    check_equal("bad operation-count sentinel", (bad_count_begin[8], len(bad_count) - 2), (2, 1))
    timestamp = find_frames(scenario, "SERIES_APPEND")[-1]
    timestamp_header = SERIES_SAMPLES.unpack_from(timestamp.payload)
    check_equal("bad timestamp series identity/mode", (timestamp_header[2], timestamp_header[4]), (EXPLICIT_SERIES_ID, 0))
    first_sample = struct.unpack_from("<Qq", timestamp.payload, SERIES_SAMPLES.size)
    check_equal("bad append starts at existing newest timestamp", first_sample[0], scenario.precondition["explicit_series_newest_timestamp_us"])

    missing_plot = next(frame for frame in missing_reference if frame.message == "OBJECT_REPLACE")
    missing_plot_body = PLOT_BODY.unpack(missing_plot.payload[OBJECT_PREFIX.size :])
    check_equal("retained-only missing series reference", missing_plot_body[0], 99)
    lost_begin = PRESENT_BEGIN.unpack(lost_mixed[0].payload)
    check_equal("rejected mixed transaction modes", (lost_begin[9], lost_begin[10]), (1, 1))
    check_equal(
        "rejected mixed frame order",
        [frame.message for frame in lost_mixed],
        ["PRESENT_BEGIN", "CELL_SPAN", "CURSOR", "OBJECT_REPLACE", "PRESENT_COMMIT"],
    )
    lost_plot = next(frame for frame in lost_mixed if frame.message == "OBJECT_REPLACE")
    check_equal("mixed missing series reference", PLOT_BODY.unpack(lost_plot.payload[OBJECT_PREFIX.size :])[0], 99)

    results = [TX_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "TX_RESULT")]
    check_equal(
        "mixed and recoverable rejection results",
        results,
        [
            (4, 0, 0, 4),
            (5, 2, 0, 4),
            (6, 2, 0, 4),
            (7, 2, 0, 4),
            (8, 2, 0, 4),
            (9, 2, 0, 4),
        ],
    )
    check_equal("mixed rejection terminal state", scenario.expected["mixed_rejection_client_state"], "SESSION_LOST")


def validate_resize(scenario: Scenario) -> None:
    cell_replace, layout_start, layout_continue = output_update_segments(scenario)
    check_equal(
        "resize canonical CELL_REPLACE frames",
        [frame.message for frame in cell_replace],
        ["PRESENT_BEGIN", "CELL_SPAN", "CELL_SPAN", "CURSOR", "PRESENT_COMMIT"],
    )
    begin = PRESENT_BEGIN.unpack(cell_replace[0].payload)
    check_equal("resize CELL_REPLACE begin", begin[:3], (5, 4, 1))
    check_equal("resize geometry", begin[4:6], (3, 2))
    check_equal("resize CELL counts and modes", begin[6:11], (2, 6, 0, 2, 0))
    canonical_bytes = 216 + 2 * (52 + 8 * 3)
    check_equal("resize canonical checked byte formula", begin[3], canonical_bytes)
    check_equal("resize CELL_REPLACE actual bytes", sum(len(frame.encoded) for frame in cell_replace), canonical_bytes)
    for row, span_frame in enumerate(cell_replace[1:3]):
        row_value, column, count = CELL_SPAN_PREFIX.unpack_from(span_frame.payload)
        check_equal(f"resize row {row} canonical span", (row_value, column, count), (row, 0, 3))
    check_equal("resize CELL fallback commit disposition", PRESENT_COMMIT.unpack(cell_replace[-1].payload)[1], 0)

    start_begin = PRESENT_BEGIN.unpack(layout_start[0].payload)
    continue_begin = PRESENT_BEGIN.unpack(layout_continue[0].payload)
    check_equal("resize LAYOUT_START mode/base", (start_begin[0], start_begin[1], start_begin[10]), (6, 5, 4))
    check_equal("resize LAYOUT_CONTINUE mode/base", (continue_begin[0], continue_begin[1], continue_begin[10]), (7, 6, 5))
    check_equal("resize LAYOUT_START frames", [frame.message for frame in layout_start], ["PRESENT_BEGIN", "REGION_REPLACE", "PRESENT_COMMIT"])
    check_equal("resize LAYOUT_CONTINUE zero-op frames", [frame.message for frame in layout_continue], ["PRESENT_BEGIN", "PRESENT_COMMIT"])
    check_equal("resize hidden intermediate disposition", PRESENT_COMMIT.unpack(layout_start[-1].payload)[1], 0)
    check_equal("resize reveal disposition", PRESENT_COMMIT.unpack(layout_continue[-1].payload)[1], 1)
    region = REGION.unpack(find_frames(scenario, "REGION_REPLACE")[0].payload)
    check_equal("resize exact owner/region identity", region[:3], (OWNER_ID, OWNER_GENERATION, REGION_ID))
    check_equal("resize region extent", region[3:7], (0, 0, 3, 2))
    check_equal(
        "resize transaction results",
        [TX_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "TX_RESULT")],
        [(5, 0, 0, 5), (6, 0, 0, 6), (7, 0, 0, 7)],
    )


def validate_legacy_interleave(scenario: Scenario) -> None:
    check_equal("legacy interleave snapshot count", len(find_frames(scenario, "SNAPSHOT_BEGIN")), 0)
    begin = TX_BEGIN.unpack(find_frames(scenario, "TX_BEGIN")[0].payload)
    check_equal("legacy interleave TX_BEGIN", begin, (2, 1, 2, 1, 1, 1))
    check_equal("legacy interleave TX_COMMIT", TX_COMMIT.unpack(find_frames(scenario, "TX_COMMIT")[0].payload), (2,))
    replacement_start, replacement_continue = output_update_segments(scenario)
    start = PRESENT_BEGIN.unpack(replacement_start[0].payload)
    continuation = PRESENT_BEGIN.unpack(replacement_continue[0].payload)
    check_equal("replacement START mode/base", (start[0], start[1], start[10]), (3, 2, 2))
    check_equal("replacement CONTINUE mode/base", (continuation[0], continuation[1], continuation[10]), (4, 3, 3))
    check_equal("replacement START hidden disposition", PRESENT_COMMIT.unpack(replacement_start[-1].payload)[1], 0)
    check_equal("replacement CONTINUE reveal disposition", PRESENT_COMMIT.unpack(replacement_continue[-1].payload)[1], 1)
    check_equal(
        "legacy/replacement shared results",
        [TX_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "TX_RESULT")],
        [(2, 0, 0, 2), (3, 0, 0, 3), (4, 0, 0, 4)],
    )


def validate_aggregate_quota(scenario: Scenario) -> None:
    values = OWNER_OPEN.unpack(find_frames(scenario, "OWNER_OPEN")[0].payload)
    request = {"owner_id":values[0],"generation":values[1],**dict(zip(QUOTA_FIELDS,values[2:9],strict=True))}
    if not all(request[field] <= QUOTA_LIMITS[field] for field in QUOTA_FIELDS):
        fail("aggregate quota fixture request is not individually valid")
    existing = dict(scenario.precondition["existing_owner"])
    existing_record = {
        "owner_id": existing.pop("id"),
        "generation": existing.pop("generation"),
        **existing,
    }
    if all(existing_record[field] + request[field] <= QUOTA_LIMITS[field] for field in QUOTA_FIELDS):
        fail("aggregate quota fixture does not exceed any aggregate total")
    result = RET_RESULT.unpack(find_frames(scenario, "RET_RESULT")[0].payload)
    check_equal(
        "aggregate quota rejection result",
        (result[0], result[1], result[3], result[4], result[6], result[7]),
        (MESSAGE_TYPES["OWNER_OPEN"], STATUS["RET_NO_CAPACITY"], OWNER_ID + 1, 1, 4, 0),
    )


def validate_reset_crossings(present: Scenario, owner_drop: Scenario) -> None:
    present_messages = [frame.message for frame in present.frames]
    check_equal(
        "crossed PRESENT settlement order",
        present_messages,
        ["PRESENT_BEGIN", "OBJECT_SET_VALUE", "SOFT_RESET_REQUEST", "PRESENT_COMMIT", "TX_RESULT", "SOFT_RESET_ACK"],
    )
    check_equal("crossed PRESENT reset request", SOFT_RESET_REQUEST.unpack(find_frames(present, "SOFT_RESET_REQUEST")[0].payload), (1, 4))
    check_equal("crossed PRESENT status", TX_RESULT.unpack(find_frames(present, "TX_RESULT")[0].payload), (5, 1, 0, 4))
    check_equal("crossed PRESENT ACK", SOFT_RESET_ACK.unpack(find_frames(present, "SOFT_RESET_ACK")[0].payload), (1, 0, 0))
    check_equal("crossed PRESENT TX_ABORT count", len([frame for frame in present.frames if frame.message == "TX_ABORT"]), 0)

    check_equal(
        "crossed OWNER_DROP settlement order",
        [frame.message for frame in owner_drop.frames],
        ["SOFT_RESET_REQUEST", "OWNER_DROP", "TX_RESULT", "SOFT_RESET_ACK"],
    )
    check_equal("crossed OWNER_DROP reset request", SOFT_RESET_REQUEST.unpack(find_frames(owner_drop, "SOFT_RESET_REQUEST")[0].payload), (1, 4))
    check_equal("crossed OWNER_DROP request", OWNER_DROP.unpack(find_frames(owner_drop, "OWNER_DROP")[0].payload), (5, 4, OWNER_ID, OWNER_GENERATION))
    check_equal("crossed OWNER_DROP status", TX_RESULT.unpack(find_frames(owner_drop, "TX_RESULT")[0].payload), (5, 1, 0, 4))
    request_index = present_messages.index("SOFT_RESET_REQUEST")
    result_index = present_messages.index("TX_RESULT")
    ack_index = present_messages.index("SOFT_RESET_ACK")
    if not request_index < result_index < ack_index:
        fail("crossed PRESENT result does not settle before reset ACK")
    drop_messages = [frame.message for frame in owner_drop.frames]
    if not drop_messages.index("SOFT_RESET_REQUEST") < drop_messages.index("TX_RESULT") < drop_messages.index("SOFT_RESET_ACK"):
        fail("crossed OWNER_DROP result does not settle before reset ACK")


def validate_reset_upload_noninterference(scenario: Scenario) -> None:
    messages = [frame.message for frame in scenario.frames]
    check_equal(
        "reset upload settlement order",
        messages,
        [
            "RESOURCE_BEGIN","RET_RESULT","CREDIT","SOFT_RESET_REQUEST",
            "RESOURCE_CHUNK","RET_RESULT","CREDIT","RESOURCE_ABORT",
            "RET_RESULT","SOFT_RESET_ACK",
        ],
    )
    chunk = RESOURCE_CHUNK.unpack_from(find_frames(scenario,"RESOURCE_CHUNK")[0].payload)
    check_equal("wrong-generation CHUNK tuple", chunk, (OWNER_ID,OWNER_GENERATION-1,2,0))
    abort = RESOURCE_ABORT.unpack(find_frames(scenario,"RESOURCE_ABORT")[0].payload)
    check_equal("reset cleanup abort tuple/reason", abort, (OWNER_ID,OWNER_GENERATION,2,1))
    results = [RET_RESULT.unpack(frame.payload) for frame in find_frames(scenario,"RET_RESULT")]
    check_equal(
        "reset upload exact results",
        [(value[0],value[1],value[3],value[4],value[5],value[6],value[7]) for value in results],
        [
            (MESSAGE_TYPES["RESOURCE_BEGIN"],STATUS["RET_OK"],OWNER_ID,OWNER_GENERATION,2,3,0),
            (MESSAGE_TYPES["RESOURCE_CHUNK"],STATUS["RET_STALE_OWNER"],OWNER_ID,OWNER_GENERATION-1,2,3,0),
            (MESSAGE_TYPES["RESOURCE_ABORT"],STATUS["RET_ABORTED"],OWNER_ID,OWNER_GENERATION,2,3,0),
        ],
    )


def validate_reset_replay(scenario: Scenario) -> None:
    frames = scenario.frames
    request = find_frames(scenario, "SOFT_RESET_REQUEST")[0]
    ack = find_frames(scenario, "SOFT_RESET_ACK")[0]
    check_equal("reset request payload", SOFT_RESET_REQUEST.unpack(request.payload), (1, 5))
    check_equal("reset ACK payload", SOFT_RESET_ACK.unpack(ack.payload), (1, 0, 0))
    check_equal("reset request old epoch", request.epoch, 0)
    check_equal("reset ACK new epoch", ack.epoch, 1)
    check_equal("reset directional sequence continuity", (request.sequence, ack.sequence), (15, 54))
    ack_index = frames.index(ack)
    next_client_data = next(frame for frame in frames[ack_index + 1 :] if frame.direction == CLIENT and frame.ordinary)
    check_equal("first post-reset client data", next_client_data.message, "SNAPSHOT_BEGIN")
    snapshot_begin = SNAPSHOT_BEGIN.unpack(next_client_data.payload)
    check_equal("reset snapshot selected geometry/counts", snapshot_begin, (1, 0, 80, 25, 25, 2_000))
    snapshot_spans = find_frames(scenario, "CELL_SPAN")
    check_equal("reset snapshot full-width row count", len(snapshot_spans), 25)
    for row, span in enumerate(snapshot_spans):
        check_equal(
            f"reset snapshot canonical row {row}",
            CELL_SPAN_PREFIX.unpack_from(span.payload),
            (row, 0, 80),
        )
        check_equal(f"reset snapshot row {row} payload bytes", len(span.payload), CELL_SPAN_PREFIX.size + 80 * CELL.size)
    snapshot_result_index = next(index for index, frame in enumerate(frames) if frame.message == "TX_RESULT" and TX_RESULT.unpack(frame.payload)[0] == 1)
    query_index = next(index for index, frame in enumerate(frames) if frame.message == "RET_QUERY")
    if query_index <= snapshot_result_index:
        fail("reset replay discovers retained support before CELL snapshot succeeds")
    caps_index = next(index for index, frame in enumerate(frames) if frame.message == "RET_CAPS")
    formats_index = next(index for index, frame in enumerate(frames) if frame.message == "RET_FORMATS")
    query_credit_index = next(
        index
        for index in range(formats_index + 1, len(frames))
        if frames[index].message == "CREDIT"
    )
    if not query_index < caps_index < formats_index < query_credit_index:
        fail("reset rediscovery violates CAPS/FORMATS/covering-CREDIT order")
    resource_begin = find_frames(scenario, "RESOURCE_BEGIN")[0]
    resource_values = RESOURCE_BEGIN.unpack(resource_begin.payload)
    declared_digest = resource_values[-1]
    chunks = find_frames(scenario, "RESOURCE_CHUNK")
    check_equal("reset replay ordered chunk count", len(chunks), 2)
    check_equal("reset replay chunk offsets", [RESOURCE_CHUNK.unpack_from(chunk.payload)[3] for chunk in chunks], [0, 4])
    data = b"".join(chunk.payload[RESOURCE_CHUNK.size :] for chunk in chunks)
    check_equal("reset replay resource byte length", resource_values[7], len(data))
    check_equal("reset replay SHA3-256", declared_digest, hashlib.sha3_256(data).digest())
    first_chunk_index = frames.index(chunks[0])
    second_chunk_index = frames.index(chunks[1])
    if not any(frame.message == "CREDIT" for frame in frames[first_chunk_index + 1 : second_chunk_index]):
        fail("reset replay sends second RESOURCE_CHUNK before covering first-chunk CREDIT")
    image = find_frames(scenario, "OBJECT_DEFINE")[0]
    image_prefix = OBJECT_PREFIX.unpack_from(image.payload)
    check_equal("reset IMAGE identity/type", (image_prefix[2], image_prefix[3]), (IMAGE_ID, OBJECT_TYPES["IMAGE"]))
    check_equal("reset IMAGE exact body", IMAGE_BODY.unpack(image.payload[OBJECT_PREFIX.size :]), (RESOURCE_ID, 1, 255))
    replacement_start, replacement_continue = output_update_segments(scenario)
    start = PRESENT_BEGIN.unpack(replacement_start[0].payload)
    continuation = PRESENT_BEGIN.unpack(replacement_continue[0].payload)
    check_equal("reset retained START mode/base", (start[0],start[1],start[10]), (2,1,2))
    check_equal("reset retained CONTINUE mode/base", (continuation[0],continuation[1],continuation[10]), (3,2,3))
    check_equal("reset START transaction bytes", start[3], scenario.expected["replace_start_transaction_bytes"])
    check_equal("reset CONTINUE transaction bytes", continuation[3], scenario.expected["replace_continue_transaction_bytes"])
    check_equal("reset hidden START", PRESENT_COMMIT.unpack(replacement_start[-1].payload)[1], 0)
    check_equal("reset CONTINUE reveal", PRESENT_COMMIT.unpack(replacement_continue[-1].payload)[1], 1)


def validate_stale_generation(scenario: Scenario) -> None:
    stale_frame, over_quota_frame = find_frames(scenario, "OWNER_OPEN")
    owner = OWNER_OPEN.unpack(stale_frame.payload)
    check_equal("stale generation request", owner[1], OWNER_GENERATION - 1)
    stale_result, over_quota_result = [RET_RESULT.unpack(frame.payload) for frame in find_frames(scenario, "RET_RESULT")]
    result = stale_result
    check_equal("stale result request type", result[0], MESSAGE_TYPES["OWNER_OPEN"])
    check_equal("stale result status", result[1], STATUS["RET_STALE_OWNER"])
    check_equal("stale result revision", result[6], 3)
    check_equal("stale result allocations", result[7], 0)
    over_quota = OWNER_OPEN.unpack(over_quota_frame.payload)
    check_equal("over-quota owner identity", over_quota[:2], (OWNER_ID + 1, 1))
    check_equal("over-quota region reservation", over_quota[2], CAPS_VALUES["max_regions"] + 1)
    check_equal(
        "over-quota result",
        (over_quota_result[0], over_quota_result[1], over_quota_result[3], over_quota_result[4], over_quota_result[6], over_quota_result[7]),
        (MESSAGE_TYPES["OWNER_OPEN"], STATUS["RET_INVALID"], OWNER_ID + 1, 1, 3, 0),
    )


def validate_resource_failures(overrun: Scenario, digest_failure: Scenario) -> None:
    overrun_begin = RESOURCE_BEGIN.unpack(find_frames(overrun, "RESOURCE_BEGIN")[0].payload)
    overrun_chunk = find_frames(overrun, "RESOURCE_CHUNK")[0]
    offset = RESOURCE_CHUNK.unpack_from(overrun_chunk.payload)[3]
    data = overrun_chunk.payload[RESOURCE_CHUNK.size :]
    check_equal("overrun chunk starts at accepted offset", offset, 0)
    if offset + len(data) <= overrun_begin[7]:
        fail("overrun sentinel does not exceed its declared resource byte length")
    overrun_result = RET_RESULT.unpack(find_frames(overrun, "RET_RESULT")[-1].payload)
    check_equal("overrun request type", overrun_result[0], MESSAGE_TYPES["RESOURCE_CHUNK"])
    check_equal("overrun status", overrun_result[1], STATUS["RET_INVALID"])
    check_equal("overrun accepted bytes", overrun_result[7], 0)

    digest_begin = RESOURCE_BEGIN.unpack(find_frames(digest_failure, "RESOURCE_BEGIN")[0].payload)
    digest_chunk = find_frames(digest_failure, "RESOURCE_CHUNK")[0]
    digest_data = digest_chunk.payload[RESOURCE_CHUNK.size :]
    actual_digest = hashlib.sha3_256(digest_data).digest()
    if digest_begin[-1] == actual_digest:
        fail("digest-failure sentinel unexpectedly declares the correct digest")
    digest_result = RET_RESULT.unpack(find_frames(digest_failure, "RET_RESULT")[-1].payload)
    check_equal("digest failure request type", digest_result[0], MESSAGE_TYPES["RESOURCE_COMMIT"])
    check_equal("digest failure status", digest_result[1], STATUS["RET_BAD_CONTENT"])
    check_equal("digest failure accepted bytes", digest_result[7], 0)
    valid_chunk_credit_index = next(
        index
        for index, frame in enumerate(digest_failure.frames)
        if frame.message == "CREDIT" and index > digest_failure.frames.index(digest_chunk)
    )
    commit_index = digest_failure.frames.index(find_frames(digest_failure, "RESOURCE_COMMIT")[0])
    if valid_chunk_credit_index >= commit_index:
        fail("digest fixture sends RESOURCE_COMMIT before covering valid chunk credit")


def validate_struct_sizes() -> None:
    expected = {
        "HEADER": (HEADER.size, 40),
        "TX_BEGIN": (TX_BEGIN.size, 32),
        "TX_COMMIT": (TX_COMMIT.size, 8),
        "RET_QUERY": (RET_QUERY.size, 8),
        "RET_CAPS": (RET_CAPS.size, 64),
        "RET_FORMATS": (RET_FORMATS.size, 64),
        "RET_RESULT": (RET_RESULT.size, 48),
        "OWNER_OPEN": (OWNER_OPEN.size, 64),
        "PRESENT_BEGIN": (PRESENT_BEGIN.size, 64),
        "PRESENT_COMMIT": (PRESENT_COMMIT.size, 16),
        "REGION": (REGION.size, 48),
        "OBJECT_PREFIX": (OBJECT_PREFIX.size, 64),
        "POLYLINE_BODY": (POLYLINE_BODY.size, 16),
        "POINT": (POINT.size, 8),
        "GLYPH_RUN_BODY": (GLYPH_RUN_BODY.size, 16),
        "READOUT_BODY": (READOUT_BODY.size, 40),
        "METER_BODY": (METER_BODY.size, 48),
        "STATUS_BODY": (STATUS_BODY.size, 32),
        "PLOT_BODY": (PLOT_BODY.size, 40),
        "WAVEFORM_BODY": (WAVEFORM_BODY.size, 48),
        "IMAGE_BODY": (IMAGE_BODY.size, 16),
        "OBJECT_SET_VALUE": (OBJECT_SET_VALUE.size, 32),
        "OBJECT_SET_VISIBILITY": (OBJECT_SET_VISIBILITY.size, 32),
        "SERIES_DEFINE": (SERIES_DEFINE.size, 40),
        "SERIES_SAMPLES": (SERIES_SAMPLES.size, 40),
        "RESOURCE_BEGIN": (RESOURCE_BEGIN.size, 80),
        "RESOURCE_CHUNK": (RESOURCE_CHUNK.size, 32),
        "RESOURCE_COMMIT": (RESOURCE_COMMIT.size, 24),
        "RESOURCE_ABORT": (RESOURCE_ABORT.size, 32),
        "OWNER_DROP": (OWNER_DROP.size, 32),
        "OWNER_ITEM": (OWNER_ITEM.size, 24),
    }
    for name, (actual, required) in expected.items():
        check_equal(f"{name} struct size", actual, required)


def validate_registry_coverage(canonical: Sequence[Scenario]) -> None:
    used = {frame.message for scenario in canonical for frame in scenario.frames}
    missing = set(RETAINED_MESSAGE_TYPES) - used
    if missing:
        fail(f"retained payload families lack a complete framed oracle: {sorted(missing)}")
    for scenario in canonical:
        for frame in scenario.frames:
            if frame.message in {"OWNER_DROP", "RESOURCE_ABORT", "RET_RESULT"} and frame.ordinary:
                fail(f"{scenario.name}: {frame.message} lost its control-reserve classification")


def write_fixtures(root: Path) -> None:
    for scenario in scenarios():
        text = "".join(f"{frame.encoded.hex()}\n" for frame in scenario.frames)
        (root / scenario.file).write_text(text, encoding="ascii")


def validate(root: Path) -> None:
    if crc32c(b"123456789") != 0xE3069283:
        fail("independent CRC-32C implementation failed its standard check value")
    validate_struct_sizes()

    manifest = json.loads((root / "manifest.json").read_text(encoding="utf-8"))
    check_equal("manifest", manifest, expected_manifest())
    check_oracles(root)

    canonical = scenarios()
    validate_registry_coverage(canonical)
    for scenario in canonical:
        actual_frames = read_hex_lines(root / scenario.file)
        check_equal(f"{scenario.name} frame count", len(actual_frames), len(scenario.frames))
        validate_sequence_space(scenario)
        for index, (spec, actual) in enumerate(zip(scenario.frames, actual_frames, strict=True)):
            validate_frame(f"{scenario.name} frame {index} ({spec.message})", spec, actual)
        meta_path = root / f"{Path(scenario.file).stem}.meta.json"
        state_path = root / f"{Path(scenario.file).stem}.state.json"
        meta = json.loads(meta_path.read_text(encoding="utf-8"))
        checked_in_sidecar = json.loads(state_path.read_text(encoding="utf-8"))
        check_equal(
            f"{scenario.name} independently reduced state sidecar",
            reduce_transcript(meta, actual_frames),
            checked_in_sidecar,
        )

    by_name = {scenario.name: scenario for scenario in canonical}
    validate_discovery(by_name["ret_query_supported"], by_name["ret_query_unsupported"])
    validate_initial_and_dynamic(by_name["soundlab_initial_replace"], by_name["soundlab_dynamic_append"])
    validate_object_bodies(by_name["soundlab_initial_replace"])
    validate_mutation_and_drops(by_name["mutation_and_drop_lifecycle"])
    validate_mixed_and_rejections(by_name["mixed_commit_and_rejections"])
    validate_legacy_interleave(by_name["legacy_cell_and_replace_continue"])
    validate_resize(by_name["resize_layout_sync"])
    validate_reset_crossings(
        by_name["reset_crossed_present_commit"],
        by_name["reset_crossed_owner_drop"],
    )
    validate_reset_upload_noninterference(by_name["reset_wrong_tuple_upload_abort"])
    validate_reset_replay(by_name["soft_reset_replay"])
    validate_owner_tombstone(by_name["owner_drop_tombstone"])
    validate_control_reserve_boundary(by_name["control_reserve_boundary"])
    validate_stale_generation(by_name["stale_generation"])
    validate_aggregate_quota(by_name["aggregate_quota_exhaustion"])
    validate_resource_lifecycle(by_name["resource_lifecycle"])
    validate_resource_failures(by_name["resource_chunk_overrun"], by_name["resource_digest_failure"])

    print(
        f"validated {len(canonical)} RETAINED-1 transcript files, "
        f"{sum(len(scenario.frames) for scenario in canonical)} complete frames, "
        f"contract {CONTRACT_ID}"
    )


def parse_args(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--write-fixtures",
        action="store_true",
        help="mechanically rewrite the canonical .hex fixtures",
    )
    parser.add_argument(
        "--print-manifest",
        action="store_true",
        help="print the canonical manifest JSON without reading checked-in assets",
    )
    parser.add_argument(
        "--write-manifest",
        action="store_true",
        help="mechanically rewrite only manifest.json",
    )
    return parser.parse_args(argv)


def main(argv: Sequence[str] | None = None) -> int:
    args = parse_args(sys.argv[1:] if argv is None else argv)
    root = Path(__file__).resolve().parent
    if args.print_manifest:
        print(json.dumps(expected_manifest(), indent=2))
        return 0
    if args.write_manifest:
        (root / "manifest.json").write_text(
            json.dumps(expected_manifest(), indent=2) + "\n",
            encoding="utf-8",
        )
        return 0
    if args.write_fixtures:
        write_fixtures(root)
        return 0
    validate(root)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (AssertionError, KeyError, OSError, ValueError, json.JSONDecodeError) as exc:
        print(f"vector validation failed: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc
