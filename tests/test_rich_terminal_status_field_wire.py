"""Independent STF1 byte oracles and strict malformed-frame rejection."""

from dataclasses import replace
import struct

import pytest

from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ObjectBounds, StatusFieldBody, StatusSeverity
from rich_terminal.retained_wire import (
    ObjectWireDefinition, RetainedCaps, RetainedWireError,
    decode_object_definition, decode_ret_caps,
    encode_object_definition, encode_object_replace, encode_ret_caps,
)


def definition():
    return ObjectWireDefinition(7, 2, 11, 3, 9, ObjectBounds(-2, 4, 20, 1),
                                -9, True, StatusFieldBody("茶 Lab", "Ready ✓", 8,
                                                        StatusSeverity.SUCCESS, True))


def test_status_field_wire_matches_independent_header_and_text_byte_oracle():
    value = definition()
    label, status = "茶 Lab".encode(), "Ready ✓".encode()
    expected = struct.pack("<QQQHHiQQiiII", 7, 2, 11, 11, 1, -9,
                           3, 9, -2, 4, 20, 1)
    expected += struct.pack("<IHHIIIIQ", 0x31465453, 1, 1, 2, 8,
                            len(label), len(status), 0) + label + status
    assert encode_object_definition(value) == expected
    assert encode_object_replace(value) == expected
    assert decode_object_definition(expected) == value
    assert len(expected) == 96 + len(label) + len(status)


@pytest.mark.parametrize("offset,fmt,value", [
    (64, "<I", 0), (68, "<H", 2), (70, "<H", 2),
    (72, "<I", 5), (76, "<I", 0), (76, "<I", 20),
    (76, "<I", 21), (80, "<I", 0xFFFFFFFF),
    (84, "<I", 0xFFFFFFFF), (88, "<Q", 1), (60, "<I", 2),
])
def test_noncanonical_header_geometry_lengths_or_severity_are_rejected(offset, fmt, value):
    raw = bytearray(encode_object_definition(definition()))
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(RetainedWireError):
        decode_object_definition(raw)


@pytest.mark.parametrize("bad", [b"\xff", b"\xed\xa0\x80", b"\n", b"\t", b"\x00", b"\xc2\x85", b"\xe2\x80\xa8", b"\xe2\x80\xa9", b"\x7f"])
@pytest.mark.parametrize("slot", ["label", "value"])
def test_both_wire_slots_reject_invalid_utf8_and_forbidden_line_scalars(bad, slot):
    raw = bytearray(encode_object_definition(definition())[:96])
    label, value = (bad, b"valid") if slot == "label" else (b"valid", bad)
    struct.pack_into("<II", raw, 80, len(label), len(value))
    with pytest.raises(RetainedWireError):
        decode_object_definition(raw + label + value)


def test_declared_split_must_itself_be_valid_utf8_even_if_joined_text_is_valid():
    raw = bytearray(encode_object_definition(definition())[:96])
    struct.pack_into("<II", raw, 80, 1, 2)
    with pytest.raises(RetainedWireError):
        decode_object_definition(raw + "茶".encode())


def test_truncation_and_trailing_bytes_are_rejected():
    raw = encode_object_definition(definition())
    for malformed in (raw[:95], raw[:-1], raw + b"\0"):
        with pytest.raises(RetainedWireError):
            decode_object_definition(malformed)


def test_empty_status_has_exact_minimum_size_without_padding():
    value = replace(definition(), body=StatusFieldBody("", "", 0))
    assert len(encode_object_definition(value)) == 96
    assert decode_object_definition(encode_object_definition(value)) == value


@pytest.mark.parametrize("severity", list(StatusSeverity))
def test_all_descriptive_severities_and_both_emphasis_values_round_trip(severity):
    for emphasized in (False, True):
        value = replace(definition(), visible=False,
                        body=StatusFieldBody("State", "Ready", 8, severity, emphasized))
        assert decode_object_definition(encode_object_definition(value)) == value


def test_discovery_accepts_status_only_profile_and_preserves_legacy_profile():
    legacy = RetainedCaps(RetainedFeature.CORE, 1, 1, 1, 0, 8, 0, 8, 0, 4096, 0)
    fields = replace(legacy, features=RetainedFeature.CORE | RetainedFeature.STATUS_FIELDS)
    assert decode_ret_caps(encode_ret_caps(fields)) == fields
    assert decode_ret_caps(encode_ret_caps(legacy)) == legacy
    with pytest.raises(ValueError, match="object capacity"):
        replace(fields, max_objects=0)
    with pytest.raises(ValueError, match="reserved"):
        replace(fields, features=RetainedFeature.CORE | (1 << 16))
