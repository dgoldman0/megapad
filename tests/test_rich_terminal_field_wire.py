"""Independent FIELD CONTROL and revision-bound ADJUST wire oracles."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_wire as helpers
from tests.test_rich_terminal_field_model import content, choices, text_content
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    CONTROL_EVENT_MAX_PAYLOAD, ControlEvent, ControlEventKind, ControlWireDefinition,
    RetainedWireError, control_event_payload_size, decode_control_definition,
    decode_control_event, decode_control_replace, decode_ret_caps,
    encode_control_definition, encode_control_event, encode_control_replace, encode_ret_caps,
)
from rich_terminal.semantic_fields import FieldRect, encode_field_content


LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.FIELDS


def definition(**changes):
    return ControlWireDefinition(**(dict(owner_id=7, owner_generation=2, control_id=11,
                                         kind=ControlKind.FIELD, state=LIVE, z_order=-9,
                                         region_id=3, parent_control_id=0, order=0,
                                         bounds=ObjectBounds(-2, 4, 20, 1),
                                         label="Gain", shortcut="", content=content()) | changes))


def event(**changes):
    return ControlEvent(**(dict(owner_id=7, owner_generation=2, control_id=11,
                                event_kind=ControlEventKind.ADJUST, modifiers=3,
                                model_revision=42, content_revision=17, adjustment=-2) | changes))


def test_fields_require_controls_without_enabling_collection_families():
    caps = helpers._control_caps(features=FEATURES)
    assert decode_ret_caps(encode_ret_caps(caps)) == caps
    assert struct.unpack_from("<Q", encode_ret_caps(caps), 8)[0] == 0x4101
    policy = replace(caps, max_retained_transaction_bytes=376).policy(
        helpers._control_formats(), client_to_terminal_max_payload=176,
        terminal_to_client_max_payload=64, base_max_transaction_bytes=376)
    assert not policy.features & RetainedFeature.CONTROL_COLLECTIONS
    with pytest.raises(ValueError, match="FIELDS requires CONTROLS"):
        helpers._control_caps(features=RetainedFeature.CORE | RetainedFeature.FIELDS)
    with pytest.raises(ValueError, match="FIELDS requires CONTROLS"):
        replace(policy, features=RetainedFeature.CORE | RetainedFeature.FIELDS)
    with pytest.raises(ValueError, match="176-byte"):
        replace(policy, client_to_terminal_max_payload=175)
    with pytest.raises(ValueError, match="transaction maximum"):
        replace(policy, max_retained_transaction_bytes=375)
    with pytest.raises(ValueError, match="reserved"):
        replace(caps, features=FEATURES | (1 << 16))


@pytest.mark.parametrize("field_content", [content(), choices(), text_content()])
def test_field_prefix_contains_fixed_geometry_and_exact_typed_content(field_content):
    value = definition(content=field_content)
    body = encode_field_content(field_content)  # FDC1 has independent byte oracles in its own suite.
    expected = struct.pack("<QQQHHiQQIiiIIIII", 7, 2, 11, 13, 3, -9,
                           3, 0, 0, -2, 4, 20, 1, 4, 0, len(body)) + b"Gain" + body
    assert encode_control_definition(value) == expected
    assert encode_control_replace(value) == expected
    assert decode_control_definition(expected) == value
    assert decode_control_replace(expected) == value


def test_empty_label_and_text_have_exact_176_byte_payload():
    value = definition(label="", content=text_content(text="", label_bounds=FieldRect(0, 0, 0, 0),
                                                       value_bounds=FieldRect(0, 0, 20, 1)))
    assert len(encode_control_definition(value)) == 176
    assert decode_control_definition(encode_control_definition(value)) == value


@pytest.mark.parametrize("offset,fmt,value", [
    (24, "<H", 14), (26, "<H", 7), (40, "<Q", 1), (48, "<I", 1),
    (60, "<I", 19), (64, "<I", 0), (72, "<I", 1), (76, "<I", 0),
    (84, "<I", 0), (88, "<H", 2), (104, "<I", 1),
])
def test_malformed_field_envelope_and_nested_content_reject(offset, fmt, value):
    raw = bytearray(encode_control_definition(definition()))
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(RetainedWireError):
        decode_control_definition(raw)


def test_field_root_label_uses_stricter_single_line_scalar_contract():
    raw = bytearray(encode_control_definition(definition()))
    raw[80:84] = b"Ga\xc2\x85"
    with pytest.raises(RetainedWireError):
        decode_control_definition(raw)


def test_adjust_is_exact_56_byte_frame_with_signed_amount_and_revision():
    expected = struct.pack("<QQQHHIQ", 7, 2, 11, 11, 3, 0, 42)
    expected += struct.pack("<Qq", 17, -2)
    assert encode_control_event(event()) == expected
    assert decode_control_event(expected) == event()
    assert control_event_payload_size(ControlEventKind.ADJUST) == 56
    assert CONTROL_EVENT_MAX_PAYLOAD == 64
    assert not event().keyed and not event().positioned and not event().names_item


@pytest.mark.parametrize("adjustment", [-(1 << 63), -1, 1, (1 << 63) - 1])
def test_adjust_preserves_full_nonzero_signed64_range(adjustment):
    value = event(adjustment=adjustment)
    assert decode_control_event(encode_control_event(value)) == value


@pytest.mark.parametrize("changes", [
    {"adjustment": 0}, {"adjustment": True}, {"adjustment": 1 << 63},
    {"adjustment": -(1 << 63) - 1}, {"adjustment": 1.0},
    {"content_revision": 0}, {"content_revision": True},
    {"item_key": 1}, {"scalar_offset": 1}, {"wheel_x": 1}, {"wheel_y": -1},
])
def test_adjust_rejects_noncanonical_values_and_unrelated_event_fields(changes):
    with pytest.raises((TypeError, ValueError)):
        event(**changes)


def test_unrelated_event_kinds_require_zero_adjustment():
    for value in (ControlEvent(7, 2, 11, ControlEventKind.ACTIVATE, 0, 42),
                  ControlEvent(7, 2, 11, ControlEventKind.SCROLL, 0, 42, wheel_y=1),
                  ControlEvent(7, 2, 11, ControlEventKind.PLACE, 0, 42,
                               content_revision=1, item_key=1)):
        assert decode_control_event(encode_control_event(value)) == value
        with pytest.raises(ValueError):
            replace(value, adjustment=1)


@pytest.mark.parametrize("offset,fmt,value", [(24, "<H", 12), (28, "<I", 1),
                                             (40, "<Q", 0), (48, "<q", 0)])
def test_adjust_rejects_unknown_kind_reserved_prefix_and_zero_tail(offset, fmt, value):
    raw = bytearray(encode_control_event(event()))
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(RetainedWireError):
        decode_control_event(raw)


def test_adjust_decoder_requires_exact_tail_and_never_accepts_keyed_event_tail():
    raw = encode_control_event(event())
    for bad in (raw[:40], raw[:-1], raw + b"\0", raw + bytes(8)):
        with pytest.raises(RetainedWireError):
            decode_control_event(bad)
