"""Focused byte and value oracles for retained semantic content."""

from __future__ import annotations

from dataclasses import replace
import struct

import pytest

from rich_terminal.retained_model import RetainedFeature
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_wire import (
    ControlWireDefinition,
    RetainedCaps,
    RetainedFormats,
    RetainedWireError,
    RetainedWireErrorCode,
    decode_control_definition,
    decode_ret_caps,
    encode_control_definition,
    encode_ret_caps,
)
from rich_terminal.semantic_content import (
    SemanticContentError,
    SemanticContentErrorCode,
    SemanticContentFlag,
    SemanticTextContent,
    SemanticTextItem,
    SemanticTextRole,
    SemanticTextState,
    decode_semantic_text_content,
    encode_semantic_text_content,
)


BOUNDS = ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF)


def _line(item_key: int, row: int, text: str) -> SemanticTextItem:
    return SemanticTextItem(
        item_key=item_key,
        row=row,
        column=0,
        row_span=1,
        column_span=23,
        role=SemanticTextRole.CONTENT,
        state=SemanticTextState(0),
        text=text,
    )


def _text_area_content(revision: int = 7) -> SemanticTextContent:
    return SemanticTextContent(
        content_revision=revision,
        rows=11,
        columns=23,
        viewport_row=2,
        viewport_column=4,
        viewport_rows=5,
        viewport_columns=8,
        flags=SemanticContentFlag(0),
        primary_key=102,
        primary_offset=2,
        anchor_key=101,
        anchor_offset=1,
        items=(
            _line(101, 1, "first"),
            _line(102, 3, "λine"),
            _line(103, 6, ""),
        ),
    )


def _root(kind: ControlKind, content=None, **changes) -> ControlWireDefinition:
    values = {
        "owner_id": 1,
        "owner_generation": 2,
        "control_id": 3,
        "kind": kind,
        "state": ControlState.VISIBLE | ControlState.ENABLED,
        "z_order": 4,
        "region_id": 5,
        "parent_control_id": 0,
        "order": 0,
        "bounds": BOUNDS,
        "label": "",
        "shortcut": "",
        "content": content,
    }
    values.update(changes)
    return ControlWireDefinition(**values)


def test_stx1_collection_has_exact_headers_and_round_trips() -> None:
    content = _text_area_content()
    payload = encode_semantic_text_content(content)

    expected = struct.pack(
        "<IHHQIIIIIIIIQQII",
        0x31585453,
        1,
        0,
        7,
        11,
        23,
        2,
        4,
        5,
        8,
        3,
        0,
        102,
        101,
        2,
        1,
    )
    expected += struct.pack("<QIIIIHHI", 101, 1, 0, 1, 23, 1, 0, 5) + b"first"
    expected += (
        struct.pack("<QIIIIHHI", 102, 3, 0, 1, 23, 1, 0, 5)
        + b"\xce\xbbine"
    )
    expected += struct.pack("<QIIIIHHI", 103, 6, 0, 1, 23, 1, 0, 0)
    assert payload == expected
    assert content.wire_bytes == len(expected)
    assert decode_semantic_text_content(payload) == content

    reserved_header = bytearray(payload)
    reserved_header[6] = 1
    with pytest.raises(SemanticContentError) as header:
        decode_semantic_text_content(reserved_header)
    assert header.value.code is SemanticContentErrorCode.RESERVED

    reserved_state = bytearray(payload)
    reserved_state[72 + 26] = 4
    with pytest.raises(SemanticContentError) as item:
        decode_semantic_text_content(reserved_state)
    assert item.value.code is SemanticContentErrorCode.RESERVED

    disallowed_text = bytearray(payload)
    disallowed_text[72 + 32] = ord("\n")
    with pytest.raises(SemanticContentError) as scalar:
        decode_semantic_text_content(disallowed_text)
    assert scalar.value.code is SemanticContentErrorCode.SCALAR

    impossible_count = bytearray(payload)
    impossible_count[40:44] = (0xFFFFFFFF).to_bytes(4, "little")
    with pytest.raises(SemanticContentError) as count:
        decode_semantic_text_content(impossible_count)
    assert count.value.code is SemanticContentErrorCode.PAYLOAD

    with pytest.raises(SemanticContentError) as trailing:
        decode_semantic_text_content(payload + b"\0")
    assert trailing.value.code is SemanticContentErrorCode.PAYLOAD


def test_collection_positions_geometry_and_text_are_canonical() -> None:
    content = _text_area_content()
    with pytest.raises(ValueError, match="between 1"):
        replace(content, content_revision=0)
    with pytest.raises(ValueError, match="canonical"):
        replace(content, items=tuple(reversed(content.items)))
    with pytest.raises(ValueError, match="overlap"):
        replace(
            content,
            items=(
                _line(1, 0, "a"),
                replace(_line(2, 0, "b"), column=10, column_span=10),
            ),
        )
    with pytest.raises(ValueError, match="primary offset"):
        replace(content, primary_offset=100)
    with pytest.raises(ValueError, match="control character"):
        replace(content.items[0], text="line\nfeed")
    tabbed = replace(content.items[0], text="one\ttwo")
    tabbed_content = replace(
        content,
        items=(tabbed,) + content.items[1:],
    )
    assert decode_semantic_text_content(
        encode_semantic_text_content(tabbed_content)
    ).items[0].text == "one\ttwo"


def test_grid_spans_use_true_rectangle_overlap() -> None:
    spanning = SemanticTextItem(
        1,
        0,
        0,
        2,
        1,
        SemanticTextRole.CONTENT,
        SemanticTextState(0),
        "left",
    )
    lower_right = SemanticTextItem(
        2,
        1,
        1,
        1,
        1,
        SemanticTextRole.CONTENT,
        SemanticTextState(0),
        "right",
    )
    content = SemanticTextContent(
        1,
        2,
        2,
        0,
        0,
        2,
        2,
        SemanticContentFlag.READ_ONLY,
        0,
        0,
        0,
        0,
        (spanning, lower_right),
    )
    assert decode_semantic_text_content(
        encode_semantic_text_content(content)
    ) == content
    with pytest.raises(ValueError, match="rectangles overlap"):
        replace(content, items=(spanning, replace(lower_right, column=0)))


def test_viewport_uses_absolute_items_and_can_carry_an_offscreen_anchor() -> None:
    content = SemanticTextContent(
        content_revision=11,
        rows=100,
        columns=80,
        viewport_row=50,
        viewport_column=4,
        viewport_rows=10,
        viewport_columns=40,
        flags=SemanticContentFlag(0),
        primary_key=2,
        primary_offset=2,
        anchor_key=1,
        anchor_offset=1,
        items=(
            SemanticTextItem(
                1,
                2,
                0,
                1,
                80,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "offscreen anchor",
            ),
            SemanticTextItem(
                2,
                50,
                0,
                1,
                80,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "visible line",
            ),
        ),
    )
    definition = _root(ControlKind.TEXT_AREA, content)
    assert decode_control_definition(
        encode_control_definition(definition)
    ).content == content
    with pytest.raises(ValueError, match="viewport"):
        replace(content, viewport_row=100)


def test_control_record_carries_one_generic_text_content_body() -> None:
    definition = _root(ControlKind.TEXT_AREA, _text_area_content())
    content = encode_semantic_text_content(definition.content)
    payload = encode_control_definition(definition)

    assert payload[68:72] == bytes(4)
    assert payload[72:76] == bytes(4)
    assert payload[76:80] == len(content).to_bytes(4, "little")
    assert payload[80:] == content
    assert decode_control_definition(payload) == definition

    malformed = bytearray(payload)
    malformed[80 + 6] = 1
    with pytest.raises(RetainedWireError) as reserved:
        decode_control_definition(malformed)
    assert reserved.value.code is RetainedWireErrorCode.RESERVED

    with pytest.raises(ValueError, match="requires semantic text content"):
        _root(ControlKind.TEXT_AREA)
    with pytest.raises(ValueError, match="full-row"):
        _root(
            ControlKind.TEXT_AREA,
            replace(
                _text_area_content(),
                items=(replace(_line(1, 0, "bad"), column_span=19),),
                primary_key=0,
                primary_offset=0,
                anchor_key=0,
                anchor_offset=0,
            ),
        )


def test_text_grid_and_tabs_share_control_identity_without_private_layout() -> None:
    content = SemanticTextContent(
        content_revision=9,
        rows=2,
        columns=2,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=2,
        viewport_columns=2,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=13,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                10,
                0,
                0,
                1,
                1,
                SemanticTextRole.COLUMN_HEADER,
                SemanticTextState(0),
                "Mon",
            ),
            SemanticTextItem(
                11,
                0,
                1,
                1,
                1,
                SemanticTextRole.COLUMN_HEADER,
                SemanticTextState(0),
                "Tue",
            ),
            SemanticTextItem(
                12,
                1,
                0,
                1,
                1,
                SemanticTextRole.CONTENT,
                SemanticTextState.CURRENT,
                "30",
            ),
            SemanticTextItem(
                13,
                1,
                1,
                1,
                1,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "31",
            ),
        ),
    )
    grid = _root(ControlKind.TEXT_GRID, content)
    assert decode_control_definition(encode_control_definition(grid)) == grid

    tabset = _root(ControlKind.TABSET, control_id=20)
    tab = ControlWireDefinition(
        owner_id=1,
        owner_generation=2,
        control_id=21,
        kind=ControlKind.TAB,
        state=ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED,
        z_order=0,
        region_id=5,
        parent_control_id=20,
        order=0,
        bounds=None,
        label="Notes",
        shortcut="Alt+1",
    )
    assert decode_control_definition(encode_control_definition(tabset)) == tabset
    assert decode_control_definition(encode_control_definition(tab)) == tab


def test_semantic_content_capability_is_additive_and_caller_bounded() -> None:
    caps = RetainedCaps(
        RetainedFeature.CORE
        | RetainedFeature.CONTROLS
        | RetainedFeature.CONTROL_COLLECTIONS,
        4,
        2,
        4,
        0,
        8,
        0,
        16,
        0,
        4096,
        0,
    )
    formats = RetainedFormats(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1024)
    policy = caps.policy(
        formats,
        client_to_terminal_max_payload=512,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=4096,
    )
    assert policy.features & RetainedFeature.CONTROL_COLLECTIONS
    assert not hasattr(policy, "max_semantic_items")
    encoded_caps = encode_ret_caps(caps)
    assert int.from_bytes(encoded_caps[8:16], "little") & (1 << 9)
    assert decode_ret_caps(encoded_caps) == caps

    exact_minimum = replace(caps, max_retained_transaction_bytes=352).policy(
        formats,
        client_to_terminal_max_payload=152,
        terminal_to_client_max_payload=64,
        base_max_transaction_bytes=352,
    )
    assert exact_minimum.client_to_terminal_max_payload == 152
    assert exact_minimum.max_retained_transaction_bytes == 352

    with pytest.raises(ValueError, match="requires CONTROLS"):
        replace(
            caps,
            features=(
                RetainedFeature.CORE | RetainedFeature.CONTROL_COLLECTIONS
            ),
        )
    with pytest.raises(ValueError, match="152-byte inbound payload"):
        caps.policy(
            formats,
            client_to_terminal_max_payload=151,
            terminal_to_client_max_payload=64,
            base_max_transaction_bytes=4096,
        )
    with pytest.raises(ValueError, match="advertised operation"):
        replace(caps, max_retained_transaction_bytes=351).policy(
            formats,
            client_to_terminal_max_payload=512,
            terminal_to_client_max_payload=64,
            base_max_transaction_bytes=4096,
        )
