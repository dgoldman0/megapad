"""Strict immutable FIELD transport carries canonical FDC1 through JSON."""

import base64
from dataclasses import replace
import json
import struct

import pytest

from tests.test_rich_terminal_pane_shared_wire import _offer
from rich_terminal.retained_scene import ControlState, ObjectBounds
from rich_terminal.retained_view import FieldDraw, RetainedDrawPlane, RetainedRegionDraw
from rich_terminal.semantic_fields import (
    FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect, encode_field_content,
)
from shared_session import (
    display_offer_from_wire, display_offer_to_wire,
    retained_draw_plane_from_wire, retained_draw_plane_to_wire,
)


def _content(kind=FieldKind.INTEGER):
    values = dict(content_revision=7, kind=kind, flags=FieldFlag(0),
                  label_bounds=FieldRect(0, 0, 6, 1),
                  value_bounds=FieldRect(8, 0, 12, 3))
    if kind is FieldKind.INTEGER:
        values.update(value=-3, minimum=-10, maximum=10, step=1)
    elif kind is FieldKind.CHOICE:
        values.update(value=-3, choices=(FieldChoice(-3, "茶"), FieldChoice(4, "Café")))
    else:
        values.update(text="音 — Café")
    return FieldContent(**values)


def _plane(kind=FieldKind.INTEGER):
    field = FieldDraw(41, ControlState.VISIBLE | ControlState.ENABLED, 0, -2,
                      ObjectBounds(-2, 1, 20, 3), "Gain λ", _content(kind))
    region = RetainedRegionDraw(1, 2, 1, 0, 0, 20, 12, 0, 0, 20, 12, 0, True, (field,))
    return RetainedDrawPlane(True, True, (region,))


@pytest.mark.parametrize("kind", tuple(FieldKind))
def test_field_full_json_has_one_exact_schema_and_signed_geometry(kind):
    plane = _plane(kind)
    wire = retained_draw_plane_to_wire(plane)
    assert wire["regions"][0]["draws"][0] == {
        "kind": "field", "control_id": 41, "state": 3, "order": 0,
        "z_order": -2, "bounds": [-2, 1, 20, 3], "label": "Gain λ",
        "content_fdc1_base64": base64.b64encode(encode_field_content(_content(kind))).decode("ascii"),
    }
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane
    offer = _offer(1, plane)
    assert display_offer_from_wire(json.loads(json.dumps(display_offer_to_wire(offer)))) == offer


@pytest.mark.parametrize("field,value", [
    ("control_id", True), ("control_id", 0), ("control_id", 1 << 64),
    ("state", True), ("state", 3 | 32), ("order", 1),
    ("z_order", 1 << 31), ("bounds", [True, 1, 20, 3]),
    ("bounds", [-2, 1, 0, 3]), ("bounds", [-2, 1, 10, 3]),
    ("label", 12), ("label", "bad\u2028"), ("label", ""),
    ("content_fdc1_base64", 12), ("content_fdc1_base64", "!!!"),
    ("content_fdc1_base64", "音"), ("content_fdc1_base64", ""),
])
def test_field_shared_wire_rejects_noncanonical_scalars_slots_and_base64(field, value):
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0][field] = value
    with pytest.raises((TypeError, ValueError)):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("fault", ["version", "reserved", "kind", "trailing", "value_slot"])
def test_field_json_revalidates_canonical_fdc1_instead_of_trusting_encoded_bytes(fault):
    wire = retained_draw_plane_to_wire(_plane())
    draw = wire["regions"][0]["draws"][0]
    payload = bytearray(base64.b64decode(draw["content_fdc1_base64"]))
    if fault == "version":
        struct.pack_into("<H", payload, 4, 2)
    elif fault == "reserved":
        struct.pack_into("<I", payload, 20, 1)
    elif fault == "kind":
        struct.pack_into("<H", payload, 6, 4)
    elif fault == "value_slot":
        struct.pack_into("<i", payload, 40, 19)
    else:
        payload += b"\0"
    draw["content_fdc1_base64"] = base64.b64encode(payload).decode("ascii")
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("field", ["label", "content_fdc1_base64", "order", "state", "bounds"])
def test_field_wire_requires_every_field_and_rejects_unknown_fields(field):
    wire = retained_draw_plane_to_wire(_plane())
    draw = wire["regions"][0]["draws"][0]
    del draw[field]
    with pytest.raises(ValueError, match="fields are not exact"):
        retained_draw_plane_from_wire(wire)
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0]["infer_from_cell"] = True
    with pytest.raises(ValueError, match="fields are not exact"):
        retained_draw_plane_from_wire(wire)


def test_field_delta_replaces_one_exact_control_and_preserves_acknowledged_content():
    plane = _plane()
    region = plane.regions[0]
    original = region.draws[0]
    unchanged = replace(original, control_id=42, z_order=0,
                        bounds=ObjectBounds(0, 6, 20, 3))
    plane = replace(plane, regions=(replace(region, draws=(original, unchanged)),))
    base = _offer(1, plane)
    replacement = replace(original, content=replace(original.content, content_revision=8, value=4))
    current = _offer(2, replace(plane, regions=(replace(region, draws=(replacement, unchanged)),)))
    wire = display_offer_to_wire(current, base=base)
    assert wire["base_offer_id"] == 1
    assert [(draw["kind"], draw["control_id"]) for draw in
            wire["retained"]["regions"][0]["changed"]] == [("field", 41)]
    rebuilt = display_offer_from_wire(json.loads(json.dumps(wire)), base)
    assert rebuilt == current
    assert rebuilt.retained.regions[0].draws[1] is unchanged
    assert base.retained.regions[0].draws[0].content.value == -3
    assert rebuilt.retained.regions[0].draws[0].content.value == 4
    with pytest.raises(ValueError, match="does not hold"):
        display_offer_from_wire(wire)
