"""Independent taskbar CONTROL byte oracles and negotiated capability rules."""

from dataclasses import replace
import struct

import pytest

from tests import test_rich_terminal_semantic_wire as helpers
from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlWireDefinition, RetainedWireError, RetainedWireErrorCode,
    decode_control_definition, decode_control_replace, decode_ret_caps,
    encode_control_definition, encode_control_replace, encode_ret_caps,
)


LIVE = ControlState.VISIBLE | ControlState.ENABLED
FEATURES = RetainedFeature.CORE | RetainedFeature.CONTROLS | RetainedFeature.TASKBARS


def _root(**changes):
    return replace(ControlWireDefinition(
        7, 2, 1, ControlKind.TASKBAR, LIVE, -4, 4, 0, 0,
        ObjectBounds(-2, 9, 80, 1), "", "",
    ), **changes)


def _task(**changes):
    return replace(ControlWireDefinition(
        7, 2, 2, ControlKind.TASK, LIVE | ControlState.MINIMIZED, 0,
        4, 1, 5, ObjectBounds(4, 0, 12, 1), "Pad λ", "A-P",
    ), **changes)


def test_taskbar_feature_is_additive_and_uses_existing_control_minima():
    caps = helpers._control_caps(features=FEATURES)
    encoded = encode_ret_caps(caps)
    assert struct.unpack_from("<Q", encoded, 8)[0] == 0x2101
    assert decode_ret_caps(encoded) == caps
    policy = replace(caps, max_retained_transaction_bytes=280).policy(
        helpers._control_formats(), client_to_terminal_max_payload=80,
        terminal_to_client_max_payload=64, base_max_transaction_bytes=280,
    )
    assert policy.features == FEATURES
    assert policy.max_glyph_run_bytes == 0
    assert not policy.features & RetainedFeature.CONTROL_COLLECTIONS
    for features in (RetainedFeature.CORE | RetainedFeature.TASKBARS,
                     RetainedFeature.CORE | RetainedFeature.TASKBARS | RetainedFeature.PANES):
        with pytest.raises(ValueError, match="TASKBARS requires CONTROLS"):
            helpers._control_caps(features=features)
        with pytest.raises(ValueError, match="TASKBARS requires CONTROLS"):
            replace(policy, features=features)


@pytest.mark.parametrize("definition,kind,state", [
    (_root(), 10, 3), (_task(), 11, 35),
    (_task(kind=ControlKind.LAUNCHER, state=LIVE), 12, 3),
])
def test_taskbar_control_prefix_is_exact_and_roundtrips_define_and_replace(definition, kind, state):
    label = definition.label.encode("utf-8")
    shortcut = definition.shortcut.encode("utf-8")
    bounds = definition.bounds
    expected = struct.pack(
        "<QQQHHiQQIiiIIIII", 7, 2, definition.control_id, kind, state,
        definition.z_order, 4, definition.parent_control_id, definition.order,
        bounds.cell_x, bounds.cell_y, bounds.cell_cols, bounds.cell_rows,
        len(label), len(shortcut), 0,
    ) + label + shortcut
    assert len(expected) == 80 + len(label) + len(shortcut)
    assert encode_control_definition(definition) == expected
    assert encode_control_replace(definition) == expected
    assert decode_control_definition(expected) == definition
    assert decode_control_replace(expected) == definition


@pytest.mark.parametrize("changes", [
    {"parent_control_id": 1}, {"order": 1}, {"bounds": None},
    {"bounds": ObjectBounds(0, 0, 4, 2)}, {"label": "title"},
    {"shortcut": "M"}, {"state": LIVE | ControlState.SELECTED},
    {"state": LIVE | ControlState.MINIMIZED},
])
def test_taskbar_root_rejects_noncanonical_shape(changes):
    with pytest.raises((TypeError, ValueError)):
        _root(**changes)


@pytest.mark.parametrize("changes", [
    {"parent_control_id": 0}, {"z_order": 1}, {"bounds": None},
    {"bounds": ObjectBounds(-1, 0, 4, 1)},
    {"bounds": ObjectBounds(0, 1, 4, 1)},
    {"bounds": ObjectBounds(0, 0, 4, 2)}, {"label": ""},
    {"state": LIVE | ControlState.OPEN}, {"state": LIVE | ControlState.CHECKED},
    {"state": LIVE | ControlState.SELECTED | ControlState.MINIMIZED},
    {"state": ControlState.SELECTED},
    {"kind": ControlKind.LAUNCHER, "state": LIVE | ControlState.SELECTED},
    {"kind": ControlKind.LAUNCHER, "state": LIVE | ControlState.MINIMIZED},
])
def test_taskbar_entries_reject_noncanonical_state_or_geometry(changes):
    with pytest.raises((TypeError, ValueError)):
        _task(**changes)


@pytest.mark.parametrize("text", ["bad\t", "bad\x7f", "bad\x85", "bad\u2028", "bad\u2029", "bad\ud800"])
@pytest.mark.parametrize("field", ["label", "shortcut"])
def test_taskbar_entry_text_is_strict_single_line(field, text):
    with pytest.raises(ValueError):
        _task(**{field: text})


@pytest.mark.parametrize("offset,fmt,value,code", [
    (24, "<H", 0xFFFF, RetainedWireErrorCode.ENUM),
    (26, "<H", 1 << 6, RetainedWireErrorCode.RESERVED),
    (26, "<H", 3 | 8 | 32, RetainedWireErrorCode.CONSISTENCY),
    (28, "<i", 1, RetainedWireErrorCode.CONSISTENCY),
    (40, "<Q", 0, RetainedWireErrorCode.CONSISTENCY),
    (52, "<i", -1, RetainedWireErrorCode.CONSISTENCY),
    (56, "<i", 1, RetainedWireErrorCode.CONSISTENCY),
    (60, "<I", 0, RetainedWireErrorCode.CONSISTENCY),
    (64, "<I", 2, RetainedWireErrorCode.CONSISTENCY),
    (68, "<I", 500, RetainedWireErrorCode.PAYLOAD),
    (76, "<I", 1, RetainedWireErrorCode.PAYLOAD),
])
def test_taskbar_decoder_rejects_malformed_prefix(offset, fmt, value, code):
    payload = bytearray(encode_control_definition(_task()))
    struct.pack_into(fmt, payload, offset, value)
    with pytest.raises(RetainedWireError) as error:
        decode_control_definition(payload)
    assert error.value.code is code


@pytest.mark.parametrize("fault", ["truncated", "trailing", "utf8", "separator"])
def test_taskbar_decoder_rejects_bad_text_or_payload_length(fault):
    payload = bytearray(encode_control_definition(_task()))
    if fault == "truncated":
        payload.pop()
    elif fault == "trailing":
        payload += b"\0"
    elif fault == "utf8":
        payload[80] = 0xFF
    else:
        # Replace the five-byte ASCII prefix with a three-byte separator and
        # two ASCII bytes, preserving the declared UTF-8 byte count exactly.
        payload[80:85] = "\u2028xx".encode("utf-8")
    with pytest.raises(RetainedWireError):
        decode_control_definition(payload)


def test_new_minimized_bit_stays_invalid_for_existing_controls():
    menu = helpers._menu_item()
    with pytest.raises(ValueError, match="not defined for MENU_ITEM"):
        replace(menu, state=LIVE | ControlState.MINIMIZED)
    payload = bytearray(encode_control_definition(menu))
    struct.pack_into("<H", payload, 26, int(LIVE | ControlState.MINIMIZED))
    with pytest.raises(RetainedWireError) as error:
        decode_control_definition(payload)
    assert error.value.code is RetainedWireErrorCode.CONSISTENCY
