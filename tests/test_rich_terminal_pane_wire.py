"""Independent byte checks for the capability-gated PNE1 object body."""

from dataclasses import replace
import struct

import pytest

from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ObjectBounds, PaneBody
from rich_terminal.retained_wire import (
    ObjectWireDefinition, RetainedCaps, RetainedWireError,
    decode_object_definition, decode_ret_caps,
    encode_object_definition, encode_object_replace, encode_ret_caps,
)


def definition():
    return ObjectWireDefinition(7, 2, 11, 3, 0, ObjectBounds(-2, 4, 20, 10),
                                -9, True,
                                PaneBody(8, ObjectBounds(1, 1, 18, 8), "茶 Lab", True))


def test_pane_wire_matches_independently_packed_bytes():
    value = definition()
    title = "茶 Lab".encode("utf-8")
    expected = struct.pack("<QQQHHiQQiiII", 7, 2, 11, 10, 1, -9,
                           3, 0, -2, 4, 20, 10)
    expected += struct.pack("<IHHQiiIIII", 0x31454E50, 1, 1, 8,
                            1, 1, 18, 8, len(title), 0) + title
    assert encode_object_definition(value) == expected
    assert encode_object_replace(value) == expected
    assert decode_object_definition(expected) == value
    assert len(expected) == 104 + len(title)


@pytest.mark.parametrize("offset,fmt,value", [
    (64, "<I", 0),             # tag
    (68, "<H", 2),             # unknown version
    (70, "<H", 2),             # reserved state
    (72, "<Q", 0),             # absent content region
    (72, "<Q", 3),             # own chrome region
    (80, "<i", -1),            # content outside outer bounds
    (88, "<I", 0),             # zero extent
    (92, "<I", 10),            # bottom outside outer bounds
    (96, "<I", 0xFFFFFFFF),    # declared title length
    (100, "<I", 1),            # reserved tail
    (40, "<Q", 1),             # parent object
    (26, "<H", 0),             # focused but hidden
])
def test_noncanonical_pane_payload_rejected(offset, fmt, value):
    raw = bytearray(encode_object_definition(definition()))
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(RetainedWireError):
        decode_object_definition(raw)


@pytest.mark.parametrize("tail", [b"\xff", b"\xed\xa0\x80", b"\n", b"\xc2\x85", b"\xe2\x80\xa8", b"\x7f"])
def test_wire_titles_reject_invalid_scalars_and_non_single_line_text(tail):
    raw = bytearray(encode_object_definition(definition())[:104])
    struct.pack_into("<I", raw, 96, len(tail))
    with pytest.raises(RetainedWireError):
        decode_object_definition(raw + tail)


def test_exact_payload_size_rejects_truncation_and_trailing_bytes():
    raw = encode_object_definition(definition())
    for altered in (raw[:103], raw[:-1], raw + b"\0"):
        with pytest.raises(RetainedWireError):
            decode_object_definition(altered)


def test_empty_title_is_canonical_and_has_no_extra_padding():
    value = replace(definition(), body=PaneBody(8, ObjectBounds(0, 0, 20, 10), ""))
    raw = encode_object_definition(value)
    assert len(raw) == 104
    assert decode_object_definition(raw) == value


def test_caps_accept_panes_without_controls_and_preserve_legacy_caps():
    legacy = RetainedCaps(RetainedFeature.CORE, 1, 1, 3, 0, 8, 0, 8, 0, 4096, 0)
    pane_caps = replace(legacy, features=RetainedFeature.CORE | RetainedFeature.PANES)
    assert decode_ret_caps(encode_ret_caps(pane_caps)) == pane_caps
    assert decode_ret_caps(encode_ret_caps(legacy)) == legacy
    with pytest.raises(ValueError, match="object capacity"):
        replace(pane_caps, max_objects=0)
    with pytest.raises(ValueError, match="at least two regions"):
        replace(pane_caps, max_regions=1)
    assert replace(pane_caps, max_regions=2).max_regions == 2
