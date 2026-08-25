"""Byte-oracle tests for the pure RETAINED-1 envelope codec."""

from __future__ import annotations

from dataclasses import replace
from functools import lru_cache
from pathlib import Path

import pytest

from presentation_terminal.apt1 import UINT32_MAX, UINT64_MAX
from presentation_terminal.retained_model import OwnerQuotas, RetainedFeature
from presentation_terminal.retained_wire import (
    CellMode,
    OwnerDrop,
    OwnerOpen,
    PresentBegin,
    PresentCommit,
    PresentDisposition,
    PresentRetainedMode,
    RetStatus,
    RetainedCaps,
    RetainedFormats,
    RetainedMessageType,
    RetainedResult,
    RetainedWireError,
    RetainedWireErrorCode,
    decode_owner_drop,
    decode_owner_open,
    decode_present_begin,
    decode_present_commit,
    decode_ret_caps,
    decode_ret_formats,
    decode_ret_query,
    decode_ret_result,
    encode_owner_drop,
    encode_owner_open,
    encode_present_begin,
    encode_present_commit,
    encode_ret_caps,
    encode_ret_formats,
    encode_ret_query,
    encode_ret_result,
)


ROOT = Path(__file__).resolve().parents[1]
VECTOR_DIR = ROOT / "conformance" / "apt1-retained1"


@lru_cache(maxsize=None)
def _oracle_payloads(message_type: RetainedMessageType) -> tuple[bytes, ...]:
    found: list[bytes] = []
    for path in sorted(VECTOR_DIR.glob("*.hex")):
        for line in path.read_text(encoding="ascii").splitlines():
            raw = bytes.fromhex(line)
            if int.from_bytes(raw[6:8], "little") == int(message_type):
                found.append(raw[40:])
    assert found, f"no oracle payload for {message_type.name}"
    return tuple(found)


@pytest.mark.parametrize(
    ("message_type", "decode", "encode"),
    (
        (RetainedMessageType.RET_QUERY, decode_ret_query, encode_ret_query),
        (RetainedMessageType.RET_CAPS, decode_ret_caps, encode_ret_caps),
        (RetainedMessageType.RET_FORMATS, decode_ret_formats, encode_ret_formats),
        (RetainedMessageType.OWNER_OPEN, decode_owner_open, encode_owner_open),
        (RetainedMessageType.RET_RESULT, decode_ret_result, encode_ret_result),
        (RetainedMessageType.OWNER_DROP, decode_owner_drop, encode_owner_drop),
        (RetainedMessageType.PRESENT_BEGIN, decode_present_begin, encode_present_begin),
        (RetainedMessageType.PRESENT_COMMIT, decode_present_commit, encode_present_commit),
    ),
)
def test_committed_full_frame_oracles_round_trip_exact_payloads(
    message_type, decode, encode
):
    payload = _oracle_payloads(message_type)[0]

    decoded = decode(payload)

    assert encode(decoded) == payload


def test_discovery_pair_builds_the_canonical_caller_bounded_policy():
    caps_payload = _oracle_payloads(RetainedMessageType.RET_CAPS)[0]
    formats_payload = _oracle_payloads(RetainedMessageType.RET_FORMATS)[0]
    caps = decode_ret_caps(caps_payload)
    formats = decode_ret_formats(formats_payload)

    policy = caps.policy(
        formats,
        client_to_terminal_max_payload=1_048_576,
        terminal_to_client_max_payload=1_048_576,
        base_max_transaction_bytes=65_536,
    )

    assert policy.features == RetainedFeature(0x3F)
    assert policy.max_owner_records == 8
    assert policy.max_live_owners == 4
    assert policy.max_retained_transaction_bytes == 32_768
    assert policy.max_path_points == 256
    assert policy.max_samples_per_append == 64
    assert policy.total_sample_slots == 4096

    with pytest.raises(ValueError, match="VECTOR object"):
        caps.policy(
            formats,
            client_to_terminal_max_payload=1024,
            terminal_to_client_max_payload=64,
            base_max_transaction_bytes=65_536,
        )


@pytest.mark.parametrize(
    ("message_type", "decode"),
    (
        (RetainedMessageType.RET_QUERY, decode_ret_query),
        (RetainedMessageType.RET_CAPS, decode_ret_caps),
        (RetainedMessageType.RET_FORMATS, decode_ret_formats),
        (RetainedMessageType.OWNER_OPEN, decode_owner_open),
        (RetainedMessageType.RET_RESULT, decode_ret_result),
        (RetainedMessageType.OWNER_DROP, decode_owner_drop),
        (RetainedMessageType.PRESENT_BEGIN, decode_present_begin),
        (RetainedMessageType.PRESENT_COMMIT, decode_present_commit),
    ),
)
def test_every_payload_decoder_rejects_non_exact_length(message_type, decode):
    payload = _oracle_payloads(message_type)[0]

    with pytest.raises(RetainedWireError) as truncated:
        decode(payload[:-1])
    assert truncated.value.code is RetainedWireErrorCode.PAYLOAD

    with pytest.raises(RetainedWireError) as trailing:
        decode(payload + b"\0")
    assert trailing.value.code is RetainedWireErrorCode.PAYLOAD


@pytest.mark.parametrize(
    ("message_type", "decode", "offset"),
    (
        (RetainedMessageType.RET_QUERY, decode_ret_query, 4),
        (RetainedMessageType.RET_FORMATS, decode_ret_formats, 56),
        (RetainedMessageType.OWNER_OPEN, decode_owner_open, 56),
        (RetainedMessageType.RET_RESULT, decode_ret_result, 4),
        (RetainedMessageType.PRESENT_BEGIN, decode_present_begin, 60),
        (RetainedMessageType.PRESENT_COMMIT, decode_present_commit, 12),
    ),
)
def test_reserved_fields_are_strictly_zero(message_type, decode, offset):
    payload = bytearray(_oracle_payloads(message_type)[0])
    payload[offset] = 1

    with pytest.raises(RetainedWireError) as caught:
        decode(payload)
    assert caught.value.code is RetainedWireErrorCode.RESERVED


def test_query_caps_and_formats_reject_tag_version_feature_and_format_aliases():
    query = bytearray(encode_ret_query())
    query[0] ^= 1
    with pytest.raises(RetainedWireError, match="tag"):
        decode_ret_query(query)

    caps = bytearray(_oracle_payloads(RetainedMessageType.RET_CAPS)[0])
    caps[4:6] = (2).to_bytes(2, "little")
    with pytest.raises(RetainedWireError, match="version"):
        decode_ret_caps(caps)

    caps = bytearray(_oracle_payloads(RetainedMessageType.RET_CAPS)[0])
    caps[8:16] = (1 << 6).to_bytes(8, "little")
    with pytest.raises(RetainedWireError, match="reserved"):
        decode_ret_caps(caps)

    formats = bytearray(_oracle_payloads(RetainedMessageType.RET_FORMATS)[0])
    formats[0:4] = (0).to_bytes(4, "little")
    with pytest.raises(RetainedWireError, match="UNORM32"):
        decode_ret_formats(formats)


def test_owner_lifecycle_scalars_and_result_semantics_are_exact():
    quotas = OwnerQuotas(1, 0, 2, 1, 0, 32, 8)
    request = OwnerOpen(UINT64_MAX, UINT64_MAX, quotas)
    assert decode_owner_open(encode_owner_open(request)) == request

    with pytest.raises(ValueError, match="between 1"):
        OwnerOpen(0, 1, quotas)
    with pytest.raises(ValueError, match="item_id must be zero"):
        RetainedResult(
            RetainedMessageType.OWNER_OPEN,
            RetStatus.OK,
            1,
            1,
            9,
            0,
        )
    with pytest.raises(ValueError, match="accepted_bytes"):
        RetainedResult(
            RetainedMessageType.RESOURCE_BEGIN,
            RetStatus.OK,
            1,
            1,
            1,
            0,
            4,
        )
    with pytest.raises(ValueError, match="must be positive"):
        RetainedResult(
            RetainedMessageType.RESOURCE_COMMIT,
            RetStatus.OK,
            1,
            1,
            1,
            0,
            0,
        )
    with pytest.raises(ValueError, match="not valid for RESOURCE_CHUNK"):
        RetainedResult(
            RetainedMessageType.RESOURCE_CHUNK,
            RetStatus.OK,
            1,
            1,
            1,
            0,
        )
    with pytest.raises(ValueError, match="not valid for RESOURCE_ABORT"):
        RetainedResult(
            RetainedMessageType.RESOURCE_ABORT,
            RetStatus.OK,
            1,
            1,
            1,
            0,
        )
    with pytest.raises(ValueError, match="lifecycle"):
        RetainedResult(
            RetainedMessageType.PRESENT_BEGIN,
            RetStatus.INVALID,
            1,
            1,
            0,
            0,
        )

    drop = OwnerDrop(UINT64_MAX, UINT64_MAX, UINT64_MAX, UINT64_MAX)
    assert decode_owner_drop(encode_owner_drop(drop)) == drop
    with pytest.raises(ValueError, match="between 1"):
        OwnerDrop(0, 0, 1, 1)


@pytest.mark.parametrize(
    ("owner_id", "owner_generation"),
    ((0, 0), (0, 7), (7, 0)),
)
def test_ret_invalid_round_trips_exact_invalid_owner_scalars(
    owner_id: int,
    owner_generation: int,
):
    result = RetainedResult(
        RetainedMessageType.OWNER_OPEN,
        RetStatus.INVALID,
        owner_id,
        owner_generation,
        0,
        11,
    )

    assert decode_ret_result(encode_ret_result(result)) == result


@pytest.mark.parametrize(
    "status",
    (RetStatus.OK, RetStatus.STALE_OWNER, RetStatus.NO_CAPACITY),
)
def test_non_invalid_owner_results_still_require_nonzero_authority(status: RetStatus):
    with pytest.raises(ValueError, match="between 1"):
        RetainedResult(
            RetainedMessageType.OWNER_OPEN,
            status,
            0,
            1,
            0,
            11,
        )


def test_ret_invalid_resource_result_round_trips_a_zero_item_id():
    result = RetainedResult(
        RetainedMessageType.RESOURCE_BEGIN,
        RetStatus.INVALID,
        7,
        1,
        0,
        11,
    )

    assert decode_ret_result(encode_ret_result(result)) == result


def test_non_invalid_resource_result_still_requires_a_nonzero_item_id():
    with pytest.raises(ValueError, match="item_id must be nonzero"):
        RetainedResult(
            RetainedMessageType.RESOURCE_BEGIN,
            RetStatus.OK,
            7,
            1,
            0,
            11,
        )


def test_present_begin_enforces_modes_counts_and_exact_operation_free_bytes():
    retained_start = PresentBegin(
        1,
        0,
        0,
        160,
        80,
        25,
        0,
        0,
        0,
        CellMode.NONE,
        PresentRetainedMode.REPLACE_START,
    )
    assert decode_present_begin(encode_present_begin(retained_start)) == retained_start

    cell_replace_bytes = 216 + 2 * (52 + 8 * 3)
    cell_replace = PresentBegin(
        2,
        1,
        1,
        cell_replace_bytes,
        3,
        2,
        2,
        6,
        0,
        CellMode.REPLACE,
        PresentRetainedMode.NONE,
    )
    assert decode_present_begin(encode_present_begin(cell_replace)) == cell_replace

    with pytest.raises(ValueError, match="both modes NONE"):
        replace(retained_start, retained_mode=PresentRetainedMode.NONE)
    with pytest.raises(ValueError, match="at least one operation"):
        replace(retained_start, retained_mode=PresentRetainedMode.DELTA)
    with pytest.raises(ValueError, match="zero CELL counts"):
        replace(retained_start, cell_span_count=1)
    with pytest.raises(ValueError, match="canonical full rows"):
        replace(cell_replace, cell_count=5)
    with pytest.raises(ValueError, match="not exact"):
        replace(cell_replace, declared_transaction_bytes=cell_replace_bytes + 1)
    with pytest.raises(ValueError, match="below the canonical minimum"):
        replace(retained_start, declared_transaction_bytes=159)


def test_present_enums_and_uint_bounds_reject_boolean_or_reserved_aliases():
    with pytest.raises(TypeError, match="must not be bool"):
        PresentBegin(1, 0, 0, 160, 1, 1, 0, 0, 0, True, PresentRetainedMode.REPLACE_START)
    with pytest.raises(TypeError, match="must not be bool"):
        PresentCommit(1, True)
    with pytest.raises(ValueError, match=f"between 1 and {UINT64_MAX}"):
        PresentCommit(UINT64_MAX + 1, PresentDisposition.COMMIT)

    commit = PresentCommit(UINT64_MAX, PresentDisposition.COMMIT_AND_REVEAL)
    assert decode_present_commit(encode_present_commit(commit)) == commit


def test_present_replace_product_must_fit_the_wire_cell_count():
    with pytest.raises(ValueError, match="exceeds its u32 field"):
        PresentBegin(
            1,
            0,
            0,
            UINT64_MAX,
            UINT32_MAX,
            2,
            2,
            UINT32_MAX,
            0,
            CellMode.REPLACE,
            PresentRetainedMode.NONE,
        )
