"""Byte and value oracles for ITM1 item collections (SEMANTIC-CONTENT-1)."""

from __future__ import annotations

import struct
from dataclasses import replace

import pytest

from rich_terminal.semantic_content import (
    SemanticContentError,
    SemanticContentErrorCode,
    StyleRun,
    TextStyle,
)
from rich_terminal.semantic_items import (
    ITEM_VIEW_TAG,
    ItemColumn,
    ItemColumnKind,
    ItemField,
    ItemRole,
    ItemState,
    ItemViewContent,
    ItemViewFlag,
    ItemViewRole,
    ViewItem,
    decode_item_view_content,
    encode_item_view_content,
)

TEXT = ItemColumnKind.TEXT
NUMBER = ItemColumnKind.NUMBER
ITEM = ItemRole.ITEM
SECTION = ItemRole.SECTION
S = ItemState


def _item(key, ordinal, *texts, parent=0, depth=0, state=0, role=ITEM):
    return ViewItem(
        item_key=key,
        parent_key=parent,
        ordinal=ordinal,
        depth=depth,
        state=ItemState(state),
        role=role,
        fields=tuple(t if isinstance(t, ItemField) else ItemField(t) for t in texts),
    )


def _tree() -> ItemViewContent:
    # /            (expanded)
    #   docs       (collapsed)
    #   notes.md   (selected)
    #   example.f
    return ItemViewContent(
        content_revision=9,
        role=ItemViewRole.TREE,
        flags=ItemViewFlag(0),
        columns=(ItemColumn(TEXT),),
        item_total=4,
        viewport_first=0,
        viewport_count=4,
        items=(
            _item(1, 0, "/", state=S.EXPANDABLE | S.EXPANDED),
            _item(2, 1, "docs", parent=1, depth=1, state=S.EXPANDABLE),
            _item(3, 2, "notes.md", parent=1, depth=1, state=S.SELECTED),
            _item(4, 3, "example.f", parent=1, depth=1),
        ),
    )


def _table() -> ItemViewContent:
    return ItemViewContent(
        content_revision=4,
        role=ItemViewRole.TABLE,
        flags=ItemViewFlag(0),
        columns=(
            ItemColumn(TEXT, "Name"),
            ItemColumn(NUMBER, "Size"),
            ItemColumn(TEXT, "Type"),
        ),
        item_total=51,
        viewport_first=10,
        viewport_count=2,
        items=(
            _item(700, 10, "large.txt", "2K", "file"),
            _item(701, 11, "notes.md", "54", "file"),
            _item(730, 40, "zeta", "<DIR>", "dir", state=S.SELECTED),
        ),
    )


def test_itm1_has_exact_headers_and_round_trips() -> None:
    content = _table()
    raw = encode_item_view_content(content)
    assert len(raw) == content.wire_bytes
    header = struct.unpack_from("<IHHQHHIIIII", raw)
    assert header == (ITEM_VIEW_TAG, 1, 0, 4, 3, 0, 3, 51, 10, 2, 3)
    assert raw[:4] == b"ITM1"
    offset = 40
    labels = []
    for _ in range(3):
        kind, reserved, label_bytes = struct.unpack_from("<HHI", raw, offset)
        offset += 8
        labels.append((kind, reserved, raw[offset : offset + label_bytes].decode()))
        offset += label_bytes
    assert labels == [(1, 0, "Name"), (2, 0, "Size"), (1, 0, "Type")]
    key, parent, ordinal, depth, state, role, fields, reserved = struct.unpack_from(
        "<QQIHHHHI", raw, offset
    )
    assert (key, parent, ordinal, depth, state, role, fields, reserved) == (
        700, 0, 10, 0, 0, 1, 3, 0
    )
    offset += 32
    text_bytes, runs = struct.unpack_from("<II", raw, offset)
    assert (text_bytes, runs) == (9, 0)
    assert raw[offset + 8 : offset + 17] == b"large.txt"
    assert decode_item_view_content(raw) == content
    assert content.utf8_bytes == len("NameSizeType") + sum(
        len(f.text) for item in content.items for f in item.fields
    )


def test_every_role_round_trips_and_shows_its_viewport() -> None:
    tree = _tree()
    assert decode_item_view_content(encode_item_view_content(tree)) == tree
    assert [item.item_key for item in tree.shown_items()] == [1, 2, 3, 4]
    table = _table()
    # The selected item outside the viewport is carried but not shown.
    assert [item.item_key for item in table.shown_items()] == [700, 701]
    assert table.item(730).state & S.SELECTED
    sections = ItemViewContent(
        content_revision=2,
        role=ItemViewRole.SECTIONS,
        flags=ItemViewFlag.DIRECTION_LTR,
        columns=(ItemColumn(TEXT), ItemColumn(TEXT)),
        item_total=4,
        viewport_first=0,
        viewport_count=4,
        items=(
            _item(1, 0, "SCHEDULE", role=SECTION),
            _item(2, 1, "09:30", "Project review", parent=1, depth=1),
            _item(3, 2, "TASKS", role=SECTION),
            _item(4, 3, "Plan the release", parent=3, depth=1,
                  state=S.CHECKABLE | S.CHECKED),
        ),
    )
    assert decode_item_view_content(encode_item_view_content(sections)) == sections
    assert sections.direction == 1
    for role in (ItemViewRole.LIST, ItemViewRole.CARDS):
        flat = ItemViewContent(
            1, role, ItemViewFlag(0), (ItemColumn(TEXT), ItemColumn(TEXT)), 2, 0, 2,
            (_item(5, 0, "Akashic Pad", "running", state=S.CURRENT),
             _item(6, 1, "Daybook", "ready", state=S.SELECTED)),
        )
        assert decode_item_view_content(encode_item_view_content(flat)) == flat


def test_an_empty_view_has_an_empty_viewport() -> None:
    empty = ItemViewContent(1, ItemViewRole.LIST, 0, (ItemColumn(TEXT),), 0, 0, 0, ())
    raw = encode_item_view_content(empty)
    assert len(raw) == 48
    assert decode_item_view_content(raw) == empty
    with pytest.raises(ValueError, match="empty viewport"):
        ItemViewContent(1, ItemViewRole.LIST, 0, (ItemColumn(TEXT),), 0, 0, 1, ())


@pytest.mark.parametrize(
    "change, message",
    [
        (dict(viewport_count=5), "viewport does not lie"),
        (dict(items=_tree().items[:3]), "not carried"),
        (dict(role=ItemViewRole.LIST), "top-level item"),
    ],
)
def test_viewport_and_role_rules(change, message) -> None:
    with pytest.raises(ValueError, match=message):
        replace(_tree(), **change)


@pytest.mark.parametrize(
    "items, message",
    [
        # A child of a collapsed parent.
        ((_item(1, 0, "/", state=S.EXPANDABLE),
          _item(2, 1, "a", parent=1, depth=1)), "which is expanded"),
        # Depth jumps by two.
        ((_item(1, 0, "/", state=S.EXPANDABLE | S.EXPANDED),
          _item(2, 1, "a", parent=1, depth=2)), "one deeper"),
        # Deeper than the item before but not its child.
        ((_item(1, 0, "/", state=S.EXPANDABLE | S.EXPANDED),
          _item(2, 1, "a", parent=9, depth=1)), "not its child"),
        # A parent later in the order.
        ((_item(2, 0, "a", parent=1, depth=1),
          _item(1, 1, "/", state=S.EXPANDABLE | S.EXPANDED)), "comes after it"),
        # Two selected items.
        ((_item(1, 0, "a", state=S.SELECTED), _item(2, 1, "b", state=S.SELECTED)),
         "more than one item is selected"),
        # Duplicate keys.
        ((_item(1, 0, "a"), _item(1, 1, "b")), "duplicated"),
        # Out of order.
        ((_item(1, 1, "a"), _item(2, 0, "b")), "increasing ordinal"),
    ],
)
def test_tree_structure_is_explicit_and_checked(items, message) -> None:
    with pytest.raises(ValueError, match=message):
        ItemViewContent(1, ItemViewRole.TREE, 0, (ItemColumn(TEXT),), 2, 0, 2, items)


def test_item_state_and_section_rules() -> None:
    with pytest.raises(ValueError, match="must be expandable"):
        _item(1, 0, "a", state=S.EXPANDED)
    with pytest.raises(ValueError, match="must be checkable"):
        _item(1, 0, "a", state=S.CHECKED)
    with pytest.raises(ValueError, match="cannot be selected"):
        _item(1, 0, "a", state=S.UNAVAILABLE | S.SELECTED)
    with pytest.raises(ValueError, match="section has state zero"):
        _item(1, 0, "A", "B", role=SECTION)
    with pytest.raises(ValueError, match="control character"):
        ItemField("tab\there")
    with pytest.raises(ValueError, match="more fields than"):
        ItemViewContent(1, ItemViewRole.LIST, 0, (ItemColumn(TEXT),), 1, 0, 1,
                        (_item(1, 0, "a", "b"),))
    with pytest.raises(ValueError, match="names its section"):
        ItemViewContent(1, ItemViewRole.SECTIONS, 0, (ItemColumn(TEXT),), 1, 0, 1,
                        (_item(1, 0, "loose"),))


def test_fields_carry_style_runs() -> None:
    linked = ItemField("see notes.md", (StyleRun(4, 8, TextStyle.LINK),))
    content = ItemViewContent(
        1, ItemViewRole.CARDS, 0, (ItemColumn(TEXT),), 1, 0, 1,
        (_item(1, 0, linked),),
    )
    decoded = decode_item_view_content(encode_item_view_content(content))
    assert decoded.items[0].fields[0].runs == (StyleRun(4, 8, TextStyle.LINK),)
    assert decoded.items[0].fields[0].meaning_at(5) is TextStyle.LINK
    assert decoded.style_run_count == 1
    with pytest.raises(ValueError, match="reaches past"):
        ItemField("ab", (StyleRun(1, 2, TextStyle.LINK),))


def _code(raw: bytes) -> SemanticContentErrorCode:
    with pytest.raises(SemanticContentError) as caught:
        decode_item_view_content(raw)
    return caught.value.code


def test_decoder_rejects_every_noncanonical_byte() -> None:
    raw = bytearray(encode_item_view_content(_tree()))
    assert _code(bytes(raw) + b"\0") is SemanticContentErrorCode.PAYLOAD
    assert _code(bytes(raw[:-1])) is SemanticContentErrorCode.PAYLOAD
    assert _code(bytes(raw[:39])) is SemanticContentErrorCode.PAYLOAD
    bad = bytearray(raw); bad[0] = 0
    assert _code(bytes(bad)) is SemanticContentErrorCode.CONSISTENCY
    bad = bytearray(raw); bad[4] = 2
    assert _code(bytes(bad)) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw); bad[6] = 1
    assert _code(bytes(bad)) is SemanticContentErrorCode.RESERVED
    bad = bytearray(raw); bad[16] = 6
    assert _code(bytes(bad)) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw); bad[18] = 3
    assert _code(bytes(bad)) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw); bad[18] = 4
    assert _code(bytes(bad)) is SemanticContentErrorCode.RESERVED
    # The first column's kind and reserved field.
    bad = bytearray(raw); bad[40] = 3
    assert _code(bytes(bad)) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw); bad[42] = 1
    assert _code(bytes(bad)) is SemanticContentErrorCode.RESERVED
    item = 48  # the first item, after the one empty-label column
    bad = bytearray(raw); bad[item + 22] = 0x80
    assert _code(bytes(bad)) is SemanticContentErrorCode.RESERVED
    bad = bytearray(raw); bad[item + 24] = 3
    assert _code(bytes(bad)) is SemanticContentErrorCode.ENUM
    bad = bytearray(raw); bad[item + 28] = 1
    assert _code(bytes(bad)) is SemanticContentErrorCode.RESERVED
    # The first field's text "/" becomes a control character.
    bad = bytearray(raw); bad[item + 40] = 0x09
    assert _code(bytes(bad)) is SemanticContentErrorCode.SCALAR
    # A structural rule broken in otherwise canonical bytes.
    bad = bytearray(raw); bad[item + 22] = int(S.EXPANDABLE)  # "/" no longer expanded
    assert _code(bytes(bad)) is SemanticContentErrorCode.CONSISTENCY


# --- The CONTROL record and CONTROL_EVENT on the wire -------------------------

from rich_terminal.retained_model import RetainedFeature  # noqa: E402
from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds  # noqa: E402
from rich_terminal.retained_wire import (  # noqa: E402
    ControlEvent,
    ControlEventKind,
    ControlWireDefinition,
    RetainedWireError,
    RetainedWireErrorCode,
    control_event_payload_size,
    decode_control_definition,
    decode_control_event,
    encode_control_definition,
    encode_control_event,
)
from rich_terminal.semantic_content import (  # noqa: E402
    SemanticContentFlag,
    SemanticTextContent,
)


def _item_view_control(content=None, kind=ControlKind.ITEM_VIEW):
    return ControlWireDefinition(
        owner_id=7,
        owner_generation=3,
        control_id=40,
        kind=kind,
        state=ControlState.VISIBLE | ControlState.ENABLED | ControlState.SELECTED,
        z_order=0,
        region_id=1,
        parent_control_id=0,
        order=0,
        bounds=ObjectBounds(0, 2, 30, 8),
        label="",
        shortcut="",
        content=_tree() if content is None else content,
    )


def test_an_item_view_control_carries_one_itm1_body() -> None:
    definition = _item_view_control()
    raw = encode_control_definition(definition)
    body = encode_item_view_content(definition.content)
    assert struct.unpack_from("<I", raw, 76)[0] == len(body)
    assert raw[80:] == body
    assert decode_control_definition(raw) == definition
    # Each kind takes only its own body.
    with pytest.raises(ValueError, match="item collection"):
        _item_view_control(
            SemanticTextContent(1, 1, 1, 0, 0, 1, 1, SemanticContentFlag(0),
                                0, 0, 0, 0, ())
        )
    with pytest.raises(ValueError, match="semantic text content"):
        _item_view_control(kind=ControlKind.TEXT_AREA)


def test_item_events_have_an_item_tail_with_reserved_fields() -> None:
    for kind in (
        ControlEventKind.SELECT,
        ControlEventKind.OPEN,
        ControlEventKind.EXPAND,
        ControlEventKind.COLLAPSE,
        ControlEventKind.CHECK,
    ):
        event = ControlEvent(7, 3, 40, kind, 2, 11, content_revision=9, item_key=3)
        raw = encode_control_event(event)
        assert len(raw) == control_event_payload_size(kind) == 64
        assert struct.unpack_from("<QQII", raw, 40) == (9, 3, 0, 0)
        assert decode_control_event(raw) == event
        assert event.names_item and event.keyed and not event.positioned
        bad = bytearray(raw)
        bad[56] = 1
        with pytest.raises(RetainedWireError) as caught:
            decode_control_event(bytes(bad))
        assert caught.value.code is RetainedWireErrorCode.RESERVED
    with pytest.raises(ValueError):
        ControlEvent(7, 3, 40, ControlEventKind.SELECT, 0, 11,
                     content_revision=9, item_key=3, scalar_offset=1)
    with pytest.raises(ValueError):
        ControlEvent(7, 3, 40, ControlEventKind.OPEN, 0, 11)


def test_the_item_feature_needs_the_collections_feature() -> None:
    assert RetainedFeature.CONTROL_ITEMS == 1 << 10
