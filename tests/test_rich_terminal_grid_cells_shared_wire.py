"""Typed grid roles roundtrip in existing STX1 full and delta display offers."""

import base64
import json
import struct
from dataclasses import replace

import pytest

from rich_terminal.retained_scene import ControlState, ObjectBounds
from rich_terminal.retained_view import (
    DisplayScope, RetainedDrawPlane, RetainedRegionDraw, TextGridDraw,
)
from rich_terminal.semantic_content import (
    SemanticContentFlag, SemanticTextContent, SemanticTextItem,
    SemanticTextRole, SemanticTextState, decode_semantic_text_content,
)
from shared.session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from shared_session import (
    display_offer_from_wire, display_offer_to_wire,
    retained_draw_plane_from_wire, retained_draw_plane_to_wire,
)

VISIBLE = ControlState.VISIBLE | ControlState.ENABLED
TYPED_ROLES = (SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR)


def _content():
    roles = (SemanticTextRole.CONTENT, *TYPED_ROLES)
    items = tuple(SemanticTextItem(100 + i, 1, 4 * i, 1, 4, role,
                                   SemanticTextState(0), "12") for i, role in enumerate(roles))
    return SemanticTextContent(19, 5, 20, 1, 0, 2, 16,
                               SemanticContentFlag.READ_ONLY, 101, 0, 0, 0, items)


def _plane():
    grid = TextGridDraw(11, VISIBLE, 0, -2, ObjectBounds(2, 3, 16, 2), _content())
    region = RetainedRegionDraw(1, 2, 1, 0, 0, 24, 12, 0, 0, 24, 12, 0, True, (grid,))
    return RetainedDrawPlane(True, True, (region,))


def _offer(offer_id, plane):
    cell = TerminalCell(" ", (255, 255, 255), (0, 0, 0), 0)
    return TerminalDisplayOffer(
        offer_id, DisplayScope(1, 2, 3, offer_id, 0, offer_id, offer_id),
        TerminalSnapshot(24, 12, ((cell,) * 24,) * 12, 0, 0, False, False), plane,
    )


def _grid_wire():
    wire = retained_draw_plane_to_wire(_plane())
    return wire, wire["regions"][0]["draws"][0]


def _change_payload(draw_wire, mutate):
    payload = bytearray(base64.b64decode(draw_wire["content_stx1_base64"]))
    mutate(payload)
    draw_wire["content_stx1_base64"] = base64.b64encode(payload).decode("ascii")


def test_full_wire_preserves_roles_in_existing_stx1_envelope_and_exact_geometry():
    plane = _plane()
    wire, grid = _grid_wire()
    assert set(grid) == {"kind", "control_id", "state", "order", "z_order", "bounds", "content_stx1_base64"}
    assert grid["kind"] == "text_grid"
    assert grid["bounds"] == [2, 3, 16, 2]
    payload = base64.b64decode(grid["content_stx1_base64"])
    assert struct.unpack_from("<IHH", payload) == (0x31585453, 1, 0)
    decoded = decode_semantic_text_content(payload)
    assert decoded == plane.regions[0].draws[0].content
    assert [item.role for item in decoded.items] == [SemanticTextRole.CONTENT, *TYPED_ROLES]
    assert decoded.content_revision == 19
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane


@pytest.mark.parametrize("invalid_role", [0, 7, 0xFFFF])
def test_full_wire_rejects_unknown_item_role_values(invalid_role):
    wire, grid = _grid_wire()
    _change_payload(grid, lambda payload: struct.pack_into("<H", payload, 72 + 24, invalid_role))
    with pytest.raises(ValueError, match="STX1"):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("role", TYPED_ROLES)
def test_shared_wire_rejects_typed_role_under_text_area_kind(role):
    content = SemanticTextContent(
        1, 1, 10, 0, 0, 1, 10, SemanticContentFlag(0), 0, 0, 0, 0,
        (SemanticTextItem(1, 0, 0, 1, 10, role, SemanticTextState(0), "12"),),
    )
    plane = _plane()
    region = plane.regions[0]
    grid = replace(region.draws[0], content=content)
    wire = retained_draw_plane_to_wire(replace(plane, regions=(replace(region, draws=(grid,)),)))
    wire["regions"][0]["draws"][0]["kind"] = "text_area"
    with pytest.raises(ValueError, match="TEXT_AREA"):
        retained_draw_plane_from_wire(wire)


def test_delta_changes_roles_and_content_revision_without_mutating_old_offer():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    grid = region.draws[0]
    items = tuple(replace(item, role=SemanticTextRole.CONTENT) for item in grid.content.items)
    content = replace(grid.content, content_revision=20, items=items)
    updated = replace(grid, content=content)
    offer = _offer(2, replace(base.retained, regions=(replace(region, draws=(updated,)),)))
    wire = display_offer_to_wire(offer, base=base)
    changed = wire["retained"]["regions"][0]["changed"][0]
    assert changed["kind"] == "text_grid"
    assert "content_stx1_base64" in changed
    assert display_offer_from_wire(json.loads(json.dumps(wire)), base) == offer
    assert grid.content.items[1].role is SemanticTextRole.NUMBER
    assert grid.content.content_revision == 19
    assert updated.bounds == grid.bounds
    assert [(item.item_key, item.row, item.column, item.column_span) for item in updated.content.items] == [
        (item.item_key, item.row, item.column, item.column_span) for item in grid.content.items
    ]


def test_delta_revalidates_embedded_role_values():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    grid = replace(region.draws[0], content=replace(region.draws[0].content, content_revision=20))
    offer = _offer(2, replace(base.retained, regions=(replace(region, draws=(grid,)),)))
    wire = display_offer_to_wire(offer, base=base)
    changed = wire["retained"]["regions"][0]["changed"][0]
    _change_payload(changed, lambda payload: struct.pack_into("<H", payload, 72 + 24, 7))
    with pytest.raises(ValueError, match="STX1"):
        display_offer_from_wire(wire, base)


def test_unchanged_typed_grid_delta_reuses_base_and_drop_removes_root_identity():
    base = _offer(1, _plane())
    unchanged = _offer(2, base.retained)
    wire = display_offer_to_wire(unchanged, base=base)
    assert wire["retained"]["regions"][0]["changed"] == []
    assert display_offer_from_wire(wire, base) == unchanged
    region = base.retained.regions[0]
    dropped = _offer(3, replace(base.retained, regions=(replace(region, draws=()),)))
    wire = display_offer_to_wire(dropped, base=base)
    assert wire["retained"]["regions"][0]["removed"] == [["control", 11]]
    assert display_offer_from_wire(wire, base) == dropped
