"""Focused capability and byte-oracle tests for semantic CONTROL-1 values."""

from __future__ import annotations

import struct
from dataclasses import replace

import pytest

from rich_terminal.apt1 import MessageType, UINT32_MAX, UINT64_MAX
from rich_terminal.retained_model import (
    ItemHighWater,
    ItemNamespace,
    OwnerLedgerError,
    OwnerLedgerErrorCode,
    RetainedFeature,
)
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlEvent,
    ControlEventKind,
    ControlWireDefinition,
    RetainedCaps,
    RetainedFormats,
    RetainedItemReference,
    RetainedMessageType,
    RetainedWireError,
    RetainedWireErrorCode,
    decode_control_definition,
    decode_control_drop,
    decode_control_event,
    decode_control_replace,
    decode_ret_caps,
    encode_control_definition,
    encode_control_drop,
    encode_control_event,
    encode_control_replace,
    encode_ret_caps,
)


def _control_caps(*, features=None, max_objects: int = 8) -> RetainedCaps:
    return RetainedCaps(
        RetainedFeature.CORE | RetainedFeature.CONTROLS
        if features is None
        else features,
        4,
        2,
        4,
        0,
        max_objects,
        0,
        16,
        0,
        4096,
        0,
    )


def _control_formats(*, total_utf8_bytes: int = 1024) -> RetainedFormats:
    return RetainedFormats(
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        total_utf8_bytes,
    )


def _control_policy(*, inbound: int = 256, total_utf8_bytes: int = 1024):
    return _control_caps().policy(
        _control_formats(total_utf8_bytes=total_utf8_bytes),
        client_to_terminal_max_payload=inbound,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=4096,
    )


def _menu_item(**changes) -> ControlWireDefinition:
    values = {
        "owner_id": 1,
        "owner_generation": 2,
        "control_id": 3,
        "kind": ControlKind.MENU_ITEM,
        "state": ControlState.VISIBLE | ControlState.ENABLED,
        "z_order": 0,
        "region_id": 4,
        "parent_control_id": 5,
        "order": 6,
        "bounds": None,
        "label": "Open λ",
        "shortcut": "Ctrl+O",
    }
    values.update(changes)
    return ControlWireDefinition(**values)


def test_controls_feature_uses_object_and_aggregate_utf8_capacity() -> None:
    caps = _control_caps()
    assert decode_ret_caps(encode_ret_caps(caps)) == caps
    policy = _control_policy()
    assert policy.features == RetainedFeature.CORE | RetainedFeature.CONTROLS
    assert policy.max_objects == 8
    assert policy.max_glyph_run_bytes == 0
    assert policy.total_utf8_bytes == 1024

    with pytest.raises(ValueError, match="object capacity"):
        _control_caps(max_objects=0)
    with pytest.raises(ValueError, match="aggregate UTF-8 capacity"):
        _control_policy(total_utf8_bytes=0)
    with pytest.raises(ValueError, match="80-byte inbound payload"):
        _control_policy(inbound=79)

    for reserved_bit in (1 << 6, 1 << 7):
        with pytest.raises(ValueError, match="reserved RETAINED-1 bits"):
            _control_caps(features=RetainedFeature.CORE | reserved_bit)


def test_control_ids_have_an_independent_high_water_namespace() -> None:
    initial = ItemHighWater(region=1, resource=2, object=9, series=4, control=5)
    advanced = initial.advanced(ItemNamespace.CONTROL, 6)

    assert advanced.control == 6
    assert advanced.object == 9
    assert advanced.value(ItemNamespace.CONTROL) == 6
    assert initial.control == 5

    with pytest.raises(OwnerLedgerError) as duplicate:
        advanced.advanced(ItemNamespace.CONTROL, 6)
    assert duplicate.value.code is OwnerLedgerErrorCode.DUPLICATE_ID


def test_control_message_ids_are_exact_and_event_is_base_input_only() -> None:
    assert MessageType.CONTROL_EVENT == 0x0205
    assert RetainedMessageType.CONTROL_DEFINE == 0x4000
    assert RetainedMessageType.CONTROL_REPLACE == 0x4001
    assert RetainedMessageType.CONTROL_DROP == 0x4002
    assert not hasattr(RetainedMessageType, "CONTROL_EVENT")
    assert ControlEventKind.ACTIVATE == 1


def test_menu_bar_definition_has_the_exact_eighty_byte_prefix() -> None:
    definition = ControlWireDefinition(
        owner_id=0x0102030405060708,
        owner_generation=0x1112131415161718,
        control_id=0x2122232425262728,
        kind=ControlKind.MENU_BAR,
        state=ControlState.VISIBLE | ControlState.ENABLED,
        z_order=-7,
        region_id=0x3132333435363738,
        parent_control_id=0,
        order=0,
        bounds=ObjectBounds(1, 2, UINT32_MAX - 1, UINT32_MAX),
        label="",
        shortcut="",
    )

    payload = encode_control_definition(definition)
    expected = struct.pack(
        "<QQQHHiQQIIIIIIII",
        definition.owner_id,
        definition.owner_generation,
        definition.control_id,
        int(ControlKind.MENU_BAR),
        int(ControlState.VISIBLE | ControlState.ENABLED),
        -7,
        definition.region_id,
        0,
        0,
        1,
        2,
        UINT32_MAX - 1,
        UINT32_MAX,
        0,
        0,
        0,
    )

    assert len(payload) == 80
    assert payload == expected
    assert decode_control_definition(payload) == definition
    assert decode_control_replace(encode_control_replace(definition)) == definition


def test_child_definition_encodes_none_bounds_then_exact_label_and_shortcut() -> None:
    definition = _menu_item(
        owner_id=UINT64_MAX,
        owner_generation=UINT64_MAX,
        control_id=UINT64_MAX,
        order=UINT32_MAX,
        state=(
            ControlState.VISIBLE
            | ControlState.ENABLED
            | ControlState.SELECTED
            | ControlState.CHECKED
        ),
    )
    payload = encode_control_definition(definition)
    label = definition.label.encode("utf-8")
    shortcut = definition.shortcut.encode("utf-8")

    assert payload[52:68] == bytes(16)
    assert payload[68:72] == len(label).to_bytes(4, "little")
    assert payload[72:76] == len(shortcut).to_bytes(4, "little")
    assert payload[76:80] == bytes(4)
    assert payload[80:] == label + shortcut
    assert decode_control_definition(payload) == definition

    # The codec has no independent label cap. Frame admission and the owner's
    # aggregate UTF-8 reservation enforce the negotiated bound later.
    long_label = replace(definition, control_id=1, label="λ" * 257, shortcut="")
    assert decode_control_definition(encode_control_definition(long_label)) == long_label


@pytest.mark.parametrize(
    "changes",
    (
        {"kind": ControlKind.MENU_ITEM, "state": ControlState.OPEN},
        {
            "kind": ControlKind.MENU_ITEM,
            "state": ControlState.VISIBLE | ControlState.ENABLED | ControlState.OPEN,
        },
        {"kind": ControlKind.MENU_SEPARATOR, "state": ControlState.ENABLED},
        {"kind": ControlKind.MENU_SEPARATOR, "label": "not empty"},
        {"kind": ControlKind.MENU, "shortcut": "Ctrl+M"},
        {"kind": ControlKind.MENU, "parent_control_id": 0},
        {"kind": ControlKind.MENU, "bounds": ObjectBounds(0, 0, 1, 1)},
        {"kind": ControlKind.MENU, "z_order": 1},
    ),
)
def test_control_values_enforce_kind_canonical_state_and_geometry(changes) -> None:
    with pytest.raises(ValueError):
        _menu_item(**changes)

    with pytest.raises(ValueError, match="root order zero"):
        ControlWireDefinition(
            1,
            1,
            1,
            ControlKind.MENU_BAR,
            ControlState.VISIBLE,
            0,
            1,
            0,
            1,
            ObjectBounds(0, 0, 1, 1),
            "",
            "",
        )
    with pytest.raises(ValueError, match="reserved CONTROL-1 bits"):
        _menu_item(state=1 << 5)
    with pytest.raises(ValueError, match="control character"):
        _menu_item(label="bad\tlabel")


def test_control_decoder_rejects_reserved_lengths_bounds_and_text() -> None:
    valid = bytearray(encode_control_definition(_menu_item()))

    reserved_state = bytearray(valid)
    reserved_state[26:28] = (1 << 5).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as state:
        decode_control_definition(reserved_state)
    assert state.value.code is RetainedWireErrorCode.RESERVED

    reserved_field = bytearray(valid)
    reserved_field[76:80] = (1).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as reserved:
        decode_control_definition(reserved_field)
    assert reserved.value.code is RetainedWireErrorCode.RESERVED

    bad_length = bytearray(valid)
    bad_length[68:72] = (UINT32_MAX).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as length:
        decode_control_definition(bad_length)
    assert length.value.code is RetainedWireErrorCode.PAYLOAD

    bad_text = bytearray(valid)
    bad_text[80] = 0xFF
    with pytest.raises(RetainedWireError) as text:
        decode_control_definition(bad_text)
    assert text.value.code is RetainedWireErrorCode.SCALAR

    bar = bytearray(
        encode_control_definition(
            ControlWireDefinition(
                1,
                1,
                1,
                ControlKind.MENU_BAR,
                ControlState.VISIBLE,
                0,
                1,
                0,
                0,
                ObjectBounds(1, 1, 2, 2),
                "",
                "",
            )
        )
    )
    bar[60:64] = (1).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as bounds:
        decode_control_definition(bar)
    assert bounds.value.code is RetainedWireErrorCode.CONSISTENCY


def test_control_drop_and_revision_bound_activation_have_exact_payloads() -> None:
    reference = RetainedItemReference(UINT64_MAX, UINT64_MAX, UINT64_MAX)
    assert encode_control_drop(reference) == struct.pack(
        "<QQQ", UINT64_MAX, UINT64_MAX, UINT64_MAX
    )
    assert decode_control_drop(encode_control_drop(reference)) == reference

    event = ControlEvent(
        0x0102030405060708,
        0x1112131415161718,
        0x2122232425262728,
        ControlEventKind.ACTIVATE,
        0x3F,
        0x3132333435363738,
    )
    expected = struct.pack(
        "<QQQHHIQ",
        event.owner_id,
        event.owner_generation,
        event.control_id,
        1,
        0x3F,
        0,
        event.model_revision,
    )
    assert len(expected) == 40
    assert encode_control_event(event) == expected
    assert decode_control_event(expected) == event

    reserved = bytearray(expected)
    reserved[28:32] = (1).to_bytes(4, "little")
    with pytest.raises(RetainedWireError) as padding:
        decode_control_event(reserved)
    assert padding.value.code is RetainedWireErrorCode.RESERVED

    modifiers = bytearray(expected)
    modifiers[26:28] = (0x40).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as modifier_bits:
        decode_control_event(modifiers)
    assert modifier_bits.value.code is RetainedWireErrorCode.RESERVED

    kind = bytearray(expected)
    kind[24:26] = (2).to_bytes(2, "little")
    with pytest.raises(RetainedWireError) as event_kind:
        decode_control_event(kind)
    assert event_kind.value.code is RetainedWireErrorCode.ENUM
