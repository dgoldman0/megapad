"""Byte-oracle tests for the pure RETAINED-1 envelope codec."""

from __future__ import annotations

import struct
from dataclasses import replace
from functools import lru_cache
from pathlib import Path

import pytest

from rich_terminal.apt1 import UINT32_MAX, UINT64_MAX
from rich_terminal.retained_model import OwnerQuotas, RetainedFeature
from rich_terminal.retained_scene import (
    ExplicitSamples,
    GroupBody,
    GlyphRunBody,
    ObjectBounds,
    ObjectKind,
    Point,
    PolylineBody,
    RGBA,
    Sample,
    TimestampMode,
    UniformSamples,
)
from rich_terminal.retained_wire import (
    CellMode,
    ObjectSetValue,
    ObjectSetVisibility,
    ObjectWireDefinition,
    OwnerDrop,
    OwnerOpen,
    PresentBegin,
    PresentCommit,
    PresentDisposition,
    PresentRetainedMode,
    RegionWireDefinition,
    RetainedItemReference,
    RetStatus,
    RetainedCaps,
    RetainedFormats,
    RetainedMessageType,
    RetainedResult,
    RetainedWireError,
    RetainedWireErrorCode,
    SeriesWireDefinition,
    SeriesWireSamples,
    decode_object_definition,
    decode_object_drop,
    decode_object_set_value,
    decode_object_set_visibility,
    decode_owner_drop,
    decode_owner_open,
    decode_present_begin,
    decode_present_commit,
    decode_region_definition,
    decode_region_drop,
    decode_ret_caps,
    decode_ret_formats,
    decode_ret_query,
    decode_ret_result,
    decode_series_definition,
    decode_series_drop,
    decode_series_samples,
    encode_object_definition,
    encode_object_drop,
    encode_object_set_value,
    encode_object_set_visibility,
    encode_owner_drop,
    encode_owner_open,
    encode_present_begin,
    encode_present_commit,
    encode_region_definition,
    encode_region_drop,
    encode_ret_caps,
    encode_ret_formats,
    encode_ret_query,
    encode_ret_result,
    encode_series_definition,
    encode_series_drop,
    encode_series_samples,
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
        (
            RetainedMessageType.REGION_DEFINE,
            decode_region_definition,
            encode_region_definition,
        ),
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
        (RetainedMessageType.REGION_DEFINE, decode_region_definition),
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


def test_query_caps_and_formats_reject_tag_reserved_feature_and_format_aliases():
    query = bytearray(encode_ret_query())
    query[0] ^= 1
    with pytest.raises(RetainedWireError, match="tag"):
        decode_ret_query(query)

    caps = bytearray(_oracle_payloads(RetainedMessageType.RET_CAPS)[0])
    caps[4:6] = (1).to_bytes(2, "little")
    with pytest.raises(RetainedWireError, match="reserved") as caught:
        decode_ret_caps(caps)
    assert caught.value.code is RetainedWireErrorCode.RESERVED

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


def test_region_definition_codec_enforces_exact_scalar_and_flag_contract():
    definition = RegionWireDefinition(
        UINT64_MAX,
        UINT64_MAX,
        UINT64_MAX,
        UINT32_MAX - 1,
        UINT32_MAX - 1,
        1,
        1,
        -(1 << 31),
        0x3,
    )
    assert decode_region_definition(encode_region_definition(definition)) == definition
    assert definition.visible
    assert definition.clipped

    payload = bytearray(encode_region_definition(definition))
    payload[-4:] = (0x4).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as reserved:
        decode_region_definition(payload)
    assert reserved.value.code is RetainedWireErrorCode.RESERVED

    payload = bytearray(encode_region_definition(definition))
    payload[32:36] = bytes(4)
    with pytest.raises(RetainedWireError) as scalar:
        decode_region_definition(payload)
    assert scalar.value.code is RetainedWireErrorCode.SCALAR


def test_glyph_run_object_define_empty_text_has_exact_eighty_byte_payload():
    definition = ObjectWireDefinition(
        0x0102030405060708,
        0x1112131415161718,
        0x2122232425262728,
        0x3132333435363738,
        0,
        ObjectBounds(0, 1, UINT32_MAX - 1, UINT32_MAX),
        -7,
        True,
        GlyphRunBody(
            RGBA(0x41, 0x42, 0x43, 0x44),
            RGBA(0x45, 0x46, 0x47, 0x48),
            0x006F,
            "",
        ),
    )

    payload = encode_object_definition(definition)
    expected = struct.pack(
        "<QQQHHiQQIIII4B4BHHI",
        definition.owner_id,
        definition.owner_generation,
        definition.object_id,
        int(ObjectKind.GLYPH_RUN),
        1,
        definition.z_order,
        definition.region_id,
        definition.parent_object_id,
        definition.bounds.left,
        definition.bounds.top,
        definition.bounds.right,
        definition.bounds.bottom,
        0x41,
        0x42,
        0x43,
        0x44,
        0x45,
        0x46,
        0x47,
        0x48,
        0x006F,
        0,
        0,
    )

    assert len(payload) == 80
    assert payload == expected
    assert decode_object_definition(payload) == definition

    # PRESENT_BEGIN declares the complete BEGIN + one 120-byte operation
    # frame + COMMIT transaction.  PT receives 120 as retained-frame-bytes
    # and derives this exact 280-byte declaration itself.
    begin = PresentBegin(
        1,
        0,
        1,
        280,
        80,
        25,
        0,
        0,
        1,
        CellMode.NONE,
        PresentRetainedMode.REPLACE_START,
    )
    begin_payload = encode_present_begin(begin)
    assert begin_payload[24:32] == (280).to_bytes(8, "little")
    assert decode_present_begin(begin_payload) == begin


def test_glyph_run_object_define_appends_exact_multibyte_scalar_text():
    text = "λ🙂"
    definition = ObjectWireDefinition(
        1,
        2,
        3,
        4,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        5,
        False,
        GlyphRunBody(RGBA(1, 2, 3, 255), RGBA(4, 5, 6, 7), 0x41, text),
    )

    payload = encode_object_definition(definition)
    encoded = text.encode("utf-8")
    assert len(payload) == 80 + len(encoded)
    assert payload[72:74] == (0x41).to_bytes(2, "little")
    assert payload[74:76] == bytes(2)
    assert payload[76:80] == len(encoded).to_bytes(4, "little")
    assert payload[80:] == encoded
    assert decode_object_definition(payload) == definition


@pytest.mark.parametrize("bad_text", (b"\0", b"\r", b"\n", b"\xC0\x80"))
def test_glyph_run_object_define_rejects_controls_and_invalid_utf8(bad_text):
    valid = ObjectWireDefinition(
        1,
        1,
        1,
        1,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        0,
        True,
        GlyphRunBody(RGBA(1, 2, 3, 4), RGBA(5, 6, 7, 8), 0, ""),
    )
    payload = bytearray(encode_object_definition(valid))
    payload[76:80] = len(bad_text).to_bytes(4, "little")
    payload.extend(bad_text)

    with pytest.raises(RetainedWireError) as error:
        decode_object_definition(payload)
    assert error.value.code is RetainedWireErrorCode.SCALAR


@pytest.mark.parametrize(
    ("message_type", "decode", "encode"),
    (
        (RetainedMessageType.REGION_REPLACE, decode_region_definition, encode_region_definition),
        (RetainedMessageType.REGION_DROP, decode_region_drop, encode_region_drop),
        (RetainedMessageType.OBJECT_SET_VALUE, decode_object_set_value, encode_object_set_value),
        (
            RetainedMessageType.OBJECT_SET_VISIBILITY,
            decode_object_set_visibility,
            encode_object_set_visibility,
        ),
        (RetainedMessageType.OBJECT_DROP, decode_object_drop, encode_object_drop),
        (
            RetainedMessageType.SERIES_DEFINE,
            decode_series_definition,
            encode_series_definition,
        ),
        (RetainedMessageType.SERIES_DROP, decode_series_drop, encode_series_drop),
    ),
)
def test_fixed_payload_oracles_round_trip_exactly(message_type, decode, encode):
    for payload in _oracle_payloads(message_type):
        assert encode(decode(payload)) == payload


def test_every_non_image_object_oracle_round_trips_through_typed_bodies():
    kinds = set()
    for message_type in (
        RetainedMessageType.OBJECT_DEFINE,
        RetainedMessageType.OBJECT_REPLACE,
    ):
        for payload in _oracle_payloads(message_type):
            object_type = int.from_bytes(payload[24:26], "little")
            if object_type == 3:  # IMAGE is deliberately a separate resource slice.
                continue
            definition = decode_object_definition(payload)
            kinds.add(definition.kind)
            assert encode_object_definition(definition) == payload

    glyph_run = ObjectWireDefinition(
        1,
        1,
        1,
        1,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        0,
        True,
        GlyphRunBody(RGBA(1, 2, 3, 4), RGBA(5, 6, 7, 8), 0x6F, "draw"),
    )
    assert decode_object_definition(encode_object_definition(glyph_run)) == glyph_run
    kinds.add(glyph_run.kind)

    assert kinds == set(ObjectKind)


@pytest.mark.parametrize(
    "message_type",
    (RetainedMessageType.SERIES_APPEND, RetainedMessageType.SERIES_REPLACE),
)
def test_every_series_sample_oracle_round_trips_without_policy_caps(message_type):
    for payload in _oracle_payloads(message_type):
        update = decode_series_samples(payload)
        assert encode_series_samples(update) == payload


def test_variable_payload_codecs_are_structural_not_policy_capped():
    points = tuple(Point(index, UINT32_MAX - index) for index in range(257))
    polyline = ObjectWireDefinition(
        1,
        1,
        1,
        1,
        0,
        ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
        0,
        True,
        PolylineBody(points, 1, RGBA(1, 2, 3, 4), False),
    )
    assert decode_object_definition(encode_object_definition(polyline)) == polyline

    glyph_run = replace(
        polyline,
        object_id=2,
        body=GlyphRunBody(RGBA(4, 3, 2, 1), RGBA(1, 2, 3, 4), 0, "x" * 257),
    )
    assert decode_object_definition(encode_object_definition(glyph_run)) == glyph_run

    uniform = SeriesWireSamples(1, 1, 1, UniformSamples(7, tuple(range(65))))
    assert decode_series_samples(encode_series_samples(uniform)) == uniform

    explicit = SeriesWireSamples(
        1,
        1,
        2,
        ExplicitSamples(tuple(Sample(index + 1, -index) for index in range(65))),
    )
    assert decode_series_samples(encode_series_samples(explicit)) == explicit


def test_typed_values_enforce_authority_and_scalar_contracts():
    with pytest.raises(ValueError, match="between 1"):
        RetainedItemReference(0, 1, 1)
    with pytest.raises(TypeError, match="must be bool"):
        ObjectSetVisibility(1, 1, 1, 1)
    with pytest.raises(ValueError, match="interval must be zero"):
        SeriesWireDefinition(1, 1, 1, 4, TimestampMode.EXPLICIT, 1)
    with pytest.raises(ValueError, match="interval must be positive"):
        SeriesWireDefinition(1, 1, 1, 4, TimestampMode.UNIFORM, 0)

    value = ObjectSetValue(UINT64_MAX, UINT64_MAX, UINT64_MAX, -(1 << 63))
    assert decode_object_set_value(encode_object_set_value(value)) == value


def test_object_decoders_reject_reserved_bits_enums_text_and_non_exact_bodies():
    group = bytearray(
        next(
            payload
            for payload in _oracle_payloads(RetainedMessageType.OBJECT_DEFINE)
            if int.from_bytes(payload[24:26], "little") == int(ObjectKind.GROUP)
        )
    )
    group[26:28] = (2).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as flags:
        decode_object_definition(group)
    assert flags.value.code is RetainedWireErrorCode.RESERVED

    group[26:28] = (1).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as trailing:
        decode_object_definition(group + b"\0")
    assert trailing.value.code is RetainedWireErrorCode.PAYLOAD

    glyph_run = bytearray(
        encode_object_definition(
            ObjectWireDefinition(
                1,
                1,
                1,
                1,
                0,
                ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
                0,
                True,
                GlyphRunBody(RGBA(1, 2, 3, 4), RGBA(5, 6, 7, 8), 0, "x"),
            )
        )
    )
    glyph_run[-1] = 0xFF
    with pytest.raises(RetainedWireError) as utf8:
        decode_object_definition(glyph_run)
    assert utf8.value.code is RetainedWireErrorCode.SCALAR

    glyph_run = bytearray(
        encode_object_definition(
            ObjectWireDefinition(
                1,
                1,
                1,
                1,
                0,
                ObjectBounds(0, 0, UINT32_MAX, UINT32_MAX),
                0,
                True,
                GlyphRunBody(RGBA(1, 2, 3, 4), RGBA(5, 6, 7, 8), 0, "x"),
            )
        )
    )
    glyph_run[72:74] = (0x10).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as attributes:
        decode_object_definition(glyph_run)
    assert attributes.value.code is RetainedWireErrorCode.RESERVED

    glyph_run[72:74] = bytes(2)
    glyph_run[74:76] = (1).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as reserved:
        decode_object_definition(glyph_run)
    assert reserved.value.code is RetainedWireErrorCode.RESERVED

    glyph_run[74:76] = bytes(2)
    glyph_run[24:26] = (3).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as image:
        decode_object_definition(glyph_run)
    assert image.value.code is RetainedWireErrorCode.ENUM


def test_mutation_and_series_decoders_reject_padding_modes_and_count_aliases():
    visibility = bytearray(
        _oracle_payloads(RetainedMessageType.OBJECT_SET_VISIBILITY)[0]
    )
    visibility[-1] = 1
    with pytest.raises(RetainedWireError) as padding:
        decode_object_set_visibility(visibility)
    assert padding.value.code is RetainedWireErrorCode.RESERVED

    visibility[-1] = 0
    visibility[24] = 2
    with pytest.raises(RetainedWireError) as boolean:
        decode_object_set_visibility(visibility)
    assert boolean.value.code is RetainedWireErrorCode.ENUM

    definition = bytearray(_oracle_payloads(RetainedMessageType.SERIES_DEFINE)[0])
    definition[28:32] = (2).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as mode:
        decode_series_definition(definition)
    assert mode.value.code is RetainedWireErrorCode.ENUM

    samples = bytearray(_oracle_payloads(RetainedMessageType.SERIES_APPEND)[0])
    samples[24:28] = UINT32_MAX.to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as count:
        decode_series_samples(samples)
    assert count.value.code is RetainedWireErrorCode.PAYLOAD

    explicit = SeriesWireSamples(1, 1, 1, ExplicitSamples((Sample(1, 2),)))
    payload = bytearray(encode_series_samples(explicit))
    payload[32:40] = (1).to_bytes(8, "little")
    with pytest.raises(RetainedWireError) as first:
        decode_series_samples(payload)
    assert first.value.code is RetainedWireErrorCode.CONSISTENCY
