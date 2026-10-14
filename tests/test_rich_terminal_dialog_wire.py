"""Independent DLG1, BTN1, modal-region, and TEXT field position wire oracles."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_wire as helpers
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEvent, ControlEventKind, ControlWireDefinition, RegionWireDefinition,
    RetainedWireError, decode_control_definition, decode_control_event,
    decode_region_definition, decode_ret_caps, encode_control_definition,
    encode_control_event, encode_region_definition, encode_ret_caps,
)
from rich_terminal.semantic_content import SemanticContentError, SemanticContentErrorCode
from rich_terminal.semantic_dialogs import (
    ButtonContent, ButtonFlag, DialogContent, DialogFlag, decode_button_content,
    decode_dialog_content, encode_button_content, encode_dialog_content,
)
from rich_terminal.semantic_fields import FieldRect


LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.DIALOGS
MESSAGE = "Delete notes.md?\nIt cannot be undone."


def dialog_content(**changes):
    return DialogContent(**(dict(content_revision=5, flags=DialogFlag(0), severity=3,
                                 title_bounds=FieldRect(1, 0, 18, 1),
                                 message_bounds=FieldRect(1, 1, 18, 2), message=MESSAGE)
                            | changes))


def dlg1(flags=0, revision=5, severity=3, message=MESSAGE.encode(),
         title=(1, 0, 18, 1), body=(1, 1, 18, 2)):
    return struct.pack("<IHHQIIiiIIiiII", 0x31474C44, 1, flags, revision, severity,
                       len(message), *title, *body) + message


def definition(**changes):
    return ControlWireDefinition(**(dict(owner_id=7, owner_generation=2, control_id=11,
                                         kind=ControlKind.DIALOG, state=ControlState.VISIBLE,
                                         z_order=4, region_id=3, parent_control_id=0, order=0,
                                         bounds=ObjectBounds(2, 2, 20, 6), label="Delete",
                                         shortcut="", content=dialog_content()) | changes))


def test_dlg1_has_an_exact_56_byte_header_then_its_message():
    assert len(dlg1(message=b"")) == 56
    assert encode_dialog_content(dialog_content()) == dlg1()
    assert decode_dialog_content(dlg1()) == dialog_content()
    assert dialog_content().wire_bytes == len(dlg1())
    assert dialog_content().lines == ("Delete notes.md?", "It cannot be undone.")


@pytest.mark.parametrize("offset,fmt,value,code", [
    (0, "<I", 0, SemanticContentErrorCode.ENUM),
    (4, "<H", 2, SemanticContentErrorCode.ENUM),
    (6, "<H", 2, SemanticContentErrorCode.RESERVED),
    (8, "<Q", 0, SemanticContentErrorCode.CONSISTENCY),
    (16, "<I", 5, SemanticContentErrorCode.ENUM),
    (20, "<I", 0xFFFF, SemanticContentErrorCode.PAYLOAD),
    (52, "<I", 1, SemanticContentErrorCode.CONSISTENCY),  # two lines, one row
    (36, "<I", 0, SemanticContentErrorCode.CONSISTENCY),  # half-empty title slot
])
def test_noncanonical_dlg1_fields_have_deterministic_rejection(offset, fmt, value, code):
    raw = bytearray(dlg1())
    struct.pack_into(fmt, raw, offset, value)
    with pytest.raises(SemanticContentError) as error:
        decode_dialog_content(raw)
    assert error.value.code is code


@pytest.mark.parametrize("message", [b"a\tb", b"\xff", b"a\xc2\x85", b"a\xe2\x80\xa8b"])
def test_dlg1_message_allows_only_line_feeds_among_controls(message):
    with pytest.raises(SemanticContentError) as error:
        decode_dialog_content(dlg1(message=message, body=(1, 1, 18, 2)))
    assert error.value.code is SemanticContentErrorCode.SCALAR


def test_btn1_is_exactly_eight_bytes_of_role_flags():
    for flags in (0, 1, 2, 4, 6):
        raw = struct.pack("<IHH", 0x314E5442, 1, flags)
        assert decode_button_content(raw) == ButtonContent(ButtonFlag(flags))
        assert encode_button_content(ButtonContent(ButtonFlag(flags))) == raw
    for raw in (struct.pack("<IHH", 0x314E5442, 1, 5),   # destructive default
                struct.pack("<IHH", 0x314E5442, 1, 8),   # reserved flag
                struct.pack("<IHH", 0x314E5442, 2, 0),   # version
                struct.pack("<IHH", 0x314E5442, 1, 0) + b"\0"):
        with pytest.raises(SemanticContentError):
            decode_button_content(raw)


def test_dialog_and_button_controls_round_trip_with_their_bodies():
    value = definition()
    raw = encode_control_definition(value)
    prefix = struct.unpack_from("<QQQHHiQQIiiIIIII", raw)
    assert prefix[3] == 15 and prefix[-3:] == (6, 0, len(dlg1()))
    assert raw[80:86] == b"Delete" and raw[86:] == dlg1()
    assert decode_control_definition(raw) == value
    press = definition(control_id=12, kind=ControlKind.BUTTON, state=LIVE, z_order=5,
                       bounds=ObjectBounds(4, 6, 8, 1), label="Delete", shortcut="D",
                       content=ButtonContent(ButtonFlag.DESTRUCTIVE | ButtonFlag.CANCEL))
    raw = encode_control_definition(press)
    assert struct.unpack_from("<H", raw, 24)[0] == 16
    assert raw[80:87] == b"DeleteD" and raw[87:] == struct.pack("<IHH", 0x314E5442, 1, 6)
    assert decode_control_definition(raw) == press


def test_the_dialogs_feature_is_bit_17_and_needs_controls():
    caps = helpers._control_caps(features=FEATURES)
    assert struct.unpack_from("<Q", encode_ret_caps(caps), 8)[0] == 0x20101
    assert decode_ret_caps(encode_ret_caps(caps)) == caps
    with pytest.raises(ValueError, match="DIALOGS requires CONTROLS"):
        helpers._control_caps(features=RetainedFeature.CORE | RetainedFeature.DIALOGS)
    policy = replace(caps, max_retained_transaction_bytes=336).policy(
        helpers._control_formats(), client_to_terminal_max_payload=136,
        terminal_to_client_max_payload=64, base_max_transaction_bytes=336)
    with pytest.raises(ValueError, match="136-byte"):
        replace(policy, client_to_terminal_max_payload=135)
    with pytest.raises(ValueError, match="transaction maximum"):
        replace(policy, max_retained_transaction_bytes=335)


def test_region_flag_bit_two_is_modal():
    raw = struct.pack("<QQQiiIIIIIIiI", 7, 2, 3, 0, 0, 24, 12, 0, 0, 0, 0, 9, 0x5)
    region = decode_region_definition(raw)
    assert region.modal and region.visible and not region.clipped
    assert encode_region_definition(region) == raw
    with pytest.raises(RetainedWireError):
        decode_region_definition(raw[:-4] + struct.pack("<I", 0x9))
    with pytest.raises(ValueError):
        RegionWireDefinition(7, 2, 3, 0, 0, 24, 12, 0, 0, 0, 0, 9, 0x8)


@pytest.mark.parametrize("kind", [ControlEventKind.PLACE, ControlEventKind.EXTEND])
def test_a_text_field_position_names_item_key_zero(kind):
    value = ControlEvent(owner_id=7, owner_generation=2, control_id=11, event_kind=kind,
                         modifiers=0, model_revision=42, content_revision=3, item_key=0,
                         scalar_offset=4)
    raw = encode_control_event(value)
    assert raw[40:] == struct.pack("<QQII", 3, 0, 4, 0)
    assert decode_control_event(raw) == value


def test_item_events_still_require_a_nonzero_key():
    with pytest.raises(ValueError):
        ControlEvent(owner_id=7, owner_generation=2, control_id=11,
                     event_kind=ControlEventKind.SELECT, modifiers=0, model_revision=42,
                     content_revision=3, item_key=0)
