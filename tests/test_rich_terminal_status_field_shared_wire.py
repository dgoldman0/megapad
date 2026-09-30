"""Strict full/delta JSON transport for read-only structured status fields."""

import json
from dataclasses import replace

import pytest

from rich_terminal.retained_scene import ObjectBounds, StatusSeverity
from rich_terminal.retained_view import (
    DisplayScope, RetainedDrawPlane, RetainedRegionDraw, StatusFieldDraw,
)
from shared.session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from shared_session import (
    display_offer_from_wire, display_offer_to_wire,
    retained_draw_plane_from_wire, retained_draw_plane_to_wire,
)


def _plane():
    field = StatusFieldDraw(
        41, -2, ObjectBounds(-1, 2, 12, 1), "Mode", "茶 — ready", 5,
        StatusSeverity.SUCCESS, True,
        (ObjectBounds(2, 1, 16, 8), ObjectBounds(-2, -1, 14, 5)),
    )
    region = RetainedRegionDraw(
        1, 2, 1, 0, 0, 20, 12, 0, 0, 20, 12, 0, True, (field,),
    )
    return RetainedDrawPlane(True, True, (region,))


def _offer(offer_id, plane):
    cell = TerminalCell(" ", (255, 255, 255), (0, 0, 0), 0)
    return TerminalDisplayOffer(
        offer_id, DisplayScope(1, 2, 3, offer_id, 0, offer_id, offer_id),
        TerminalSnapshot(20, 12, ((cell,) * 20,) * 12, 0, 0, False, False), plane,
    )


def test_full_wire_has_exact_fields_and_signed_group_paths():
    plane = _plane()
    wire = retained_draw_plane_to_wire(plane)
    assert wire["regions"][0]["draws"][0] == {
        "kind": "status_field", "object_id": 41, "z_order": -2,
        "bounds": [-1, 2, 12, 1],
        "parent_bounds": [[2, 1, 16, 8], [-2, -1, 14, 5]],
        "label": "Mode", "value": "茶 — ready", "label_cols": 5,
        "severity": 2, "emphasized": True,
    }
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane


@pytest.mark.parametrize("field,value", [
    ("object_id", True), ("object_id", 0), ("object_id", 1 << 64),
    ("z_order", False), ("z_order", 1 << 31),
    ("bounds", [True, 2, 12, 1]), ("bounds", [-(1 << 31)-1, 2, 12, 1]),
    ("bounds", [0, 2, 0, 1]), ("bounds", [0, 2, 12, 2]),
    ("bounds", [0, 2, 1 << 32, 1]), ("bounds", [0, 2, 12.0, 1]),
    ("label_cols", True), ("label_cols", -1), ("label_cols", 13),
    ("label_cols", 0), ("label_cols", 12), ("label_cols", "5"),
    ("severity", True), ("severity", -1), ("severity", 5),
    ("severity", 2.0), ("severity", "2"),
    ("emphasized", 1), ("emphasized", "true"),
    ("label", 7), ("value", None),
    ("label", "bad\x00"), ("value", "bad\x1f"),
    ("label", "bad\x7f"), ("value", "bad\x85"),
    ("label", "bad\u2028"), ("value", "bad\u2029"),
    ("label", "bad\ud800"), ("value", "bad\udfff"),
    ("parent_bounds", "path"), ("parent_bounds", [[0, 0, 12]]),
    ("parent_bounds", [[0, 0, False, 1]]),
    ("parent_bounds", [[0, 1 << 31, 12, 1]]),
    ("parent_bounds", [[0, 0, 12, 0]]),
])
def test_wire_rejects_invalid_field_scalars_shape_and_text(field, value):
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0][field] = value
    with pytest.raises((TypeError, ValueError)):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("field", [
    "label", "value", "label_cols", "severity", "emphasized", "parent_bounds",
])
def test_wire_requires_every_status_field_property(field):
    wire = retained_draw_plane_to_wire(_plane())
    del wire["regions"][0]["draws"][0][field]
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


def test_wire_rejects_inferred_semantics_or_other_unknown_properties():
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0]["infer_severity_from_value"] = True
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("severity", list(StatusSeverity))
def test_all_severity_values_roundtrip(severity):
    plane = _plane()
    region = plane.regions[0]
    field = replace(region.draws[0], severity=severity)
    plane = replace(plane, regions=(replace(region, draws=(field,)),))
    assert retained_draw_plane_from_wire(retained_draw_plane_to_wire(plane)) == plane


def test_delta_updates_exact_field_without_changing_geometry_or_identity():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    field = replace(region.draws[0], value="Paused", severity=StatusSeverity.WARNING,
                    emphasized=False)
    changed = replace(base.retained, regions=(replace(region, draws=(field,)),))
    offer = _offer(2, changed)
    wire = display_offer_to_wire(offer, base=base)
    assert wire["retained"]["regions"][0]["changed"][0]["kind"] == "status_field"
    assert wire["retained"]["regions"][0]["removed"] == []
    assert display_offer_from_wire(json.loads(json.dumps(wire)), base) == offer
    assert base.retained.regions[0].draws[0].value == "茶 — ready"


def test_unchanged_field_reuses_base_and_drop_removes_only_its_object_key():
    base = _offer(1, _plane())
    unchanged = _offer(2, base.retained)
    wire = display_offer_to_wire(unchanged, base=base)
    assert wire["retained"]["regions"][0]["changed"] == []
    assert display_offer_from_wire(wire, base) == unchanged
    region = base.retained.regions[0]
    dropped = _offer(3, replace(base.retained, regions=(replace(region, draws=()),)))
    wire = display_offer_to_wire(dropped, base=base)
    assert wire["retained"]["regions"][0]["removed"] == [["object", 41]]
    assert display_offer_from_wire(wire, base) == dropped


def test_delta_revalidates_field_shape_in_changed_objects():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    field = replace(region.draws[0], value="Paused")
    offer = _offer(2, replace(base.retained, regions=(replace(region, draws=(field,)),)))
    wire = display_offer_to_wire(offer, base=base)
    wire["retained"]["regions"][0]["changed"][0]["label_cols"] = 12
    with pytest.raises(ValueError):
        display_offer_from_wire(wire, base)
