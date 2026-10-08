"""Strict full and incremental shared-viewer transport for pane chrome."""

import json
from dataclasses import replace

import pytest

from rich_terminal.retained_scene import ObjectBounds, RGBA
from rich_terminal.retained_view import (
    DisplayScope, GlyphRunDraw, PaneDraw, RetainedDrawPlane, RetainedRegionDraw,
)
from shared.session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from shared_session import (
    display_offer_from_wire, display_offer_to_wire,
    retained_draw_plane_from_wire, retained_draw_plane_to_wire,
)


def _plane():
    pane = PaneDraw(
        41, -2, ObjectBounds(2, 1, 12, 8), 2,
        ObjectBounds(1, 2, 10, 5), "Sound Lab — 茶", True,
    )
    chrome = RetainedRegionDraw(
        1, 2, 1, 0, 0, 20, 12, 0, 0, 20, 12, 0, True, (pane,)
    )
    content = RetainedRegionDraw(
        1, 2, 2, 0, 0, 20, 12, 3, 3, 10, 5, 1, True, ()
    )
    return RetainedDrawPlane(True, True, (chrome, content))


def _offer(offer_id, plane):
    cell = TerminalCell(" ", (255, 255, 255), (0, 0, 0), 0)
    return TerminalDisplayOffer(
        offer_id, DisplayScope(1, 2, 3, offer_id, 0, offer_id, offer_id),
        TerminalSnapshot(20, 12, ((cell,) * 20,) * 12, 0, 0, False, False), plane,
    )


def test_pane_full_wire_has_explicit_canonical_fields_and_unicode():
    plane = _plane()
    wire = retained_draw_plane_to_wire(plane)
    assert wire["regions"][0]["draws"][0] == {
        "kind": "pane", "object_id": 41, "z_order": -2,
        "bounds": [2, 1, 12, 8], "parent_bounds": [],
        "content_region_id": 2, "content_bounds": [1, 2, 10, 5],
        "title": "Sound Lab — 茶", "focused": True,
    }
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane


def test_signed_pane_offsets_survive_transport_with_clipped_content():
    plane = _plane()
    chrome, content = plane.regions
    pane = replace(chrome.draws[0], bounds=ObjectBounds(-2, -1, 12, 8))
    chrome = replace(chrome, draws=(pane,))
    content = replace(content, clip_x=0, clip_y=1, clip_cols=9, clip_rows=5)
    plane = replace(plane, regions=(chrome, content))
    assert retained_draw_plane_from_wire(retained_draw_plane_to_wire(plane)) == plane


@pytest.mark.parametrize("field,value", [
    ("object_id", True), ("object_id", 0), ("object_id", 1 << 64),
    ("z_order", False), ("z_order", 1 << 31),
    ("content_region_id", True), ("content_region_id", 0),
    ("content_region_id", 1 << 64), ("content_region_id", "2"),
    ("focused", 1), ("focused", "true"),
    ("bounds", [True, 1, 12, 8]), ("bounds", [1 << 31, 1, 12, 8]),
    ("bounds", [2, 1, 0, 8]), ("bounds", [2, 1, 12, 1 << 32]),
    ("content_bounds", [1, False, 10, 5]),
    ("content_bounds", [-1, 2, 10, 5]),
    ("content_bounds", [1, 2, 12, 5]),
    ("content_bounds", [1, 2, 10.0, 5]),
    ("content_bounds", [1, 2, 10]),
    ("parent_bounds", [[0, 0, 20, 12]]),
    ("title", 7), ("title", "bad\x00"), ("title", "bad\x7f"),
    ("title", "bad\x85"), ("title", "bad\u2028"),
    ("title", "bad\u2029"), ("title", "bad\ud800"),
])
def test_pane_wire_rejects_noncanonical_scalars_and_shape(field, value):
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0][field] = value
    with pytest.raises((TypeError, ValueError)):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("field", ["title", "focused", "content_region_id", "content_bounds", "parent_bounds"])
def test_pane_wire_requires_every_declared_field(field):
    wire = retained_draw_plane_to_wire(_plane())
    del wire["regions"][0]["draws"][0][field]
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


def test_pane_wire_rejects_unknown_fields():
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0]["infer_title_from_cells"] = True
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("change", [
    {"clipped": False, "clip_x": 0, "clip_y": 0, "clip_cols": 0, "clip_rows": 0},
    {"clip_x": 2}, {"clip_y": 2}, {"clip_cols": 11}, {"clip_rows": 6},
    {"z_order": -1},
])
def test_wire_rechecks_cross_region_clip_and_paint_order(change):
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][1].update(change)
    # Restore wire ordering so the relationship itself must reject bad z.
    wire["regions"].sort(key=lambda item: (item["z_order"], item["owner_id"], item["region_id"]))
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


def test_content_region_cannot_be_the_chrome_region():
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"][0]["draws"][0]["content_region_id"] = 1
    with pytest.raises(ValueError, match="differ"):
        retained_draw_plane_from_wire(wire)


def test_hidden_content_region_and_other_owner_same_id_do_not_grant_authority():
    plane = _plane()
    chrome, content = plane.regions
    # Hidden regions have no wire entry. An unrelated owner's region 2 remains
    # independently rendered and must never satisfy this pane's binding.
    unrelated = replace(content, owner_id=9, clipped=False,
                        clip_x=0, clip_y=0, clip_cols=0, clip_rows=0)
    plane = replace(plane, regions=(chrome, unrelated))
    assert retained_draw_plane_from_wire(retained_draw_plane_to_wire(plane)) == plane


def test_wire_rejects_two_panes_bound_to_one_content_region():
    wire = retained_draw_plane_to_wire(_plane())
    pane = dict(wire["regions"][0]["draws"][0], object_id=42)
    wire["regions"][0]["draws"].append(pane)
    with pytest.raises(ValueError, match="multiple PANE"):
        retained_draw_plane_from_wire(wire)


def test_wire_rejects_duplicate_region_and_cross_region_object_identities():
    wire = retained_draw_plane_to_wire(_plane())
    wire["regions"].append(dict(wire["regions"][1]))
    with pytest.raises(ValueError, match="region identities"):
        retained_draw_plane_from_wire(wire)
    plane = _plane()
    glyph = GlyphRunDraw(41, 0, ObjectBounds(3, 3, 2, 1),
                         RGBA(255, 255, 255, 255), RGBA(0, 0, 0, 255), 0, "x")
    with pytest.raises(ValueError, match="object IDs"):
        replace(plane, regions=(plane.regions[0], replace(plane.regions[1], draws=(glyph,))))


def test_pane_delta_updates_chrome_and_rebuilds_exact_plane():
    base = _offer(1, _plane())
    chrome, content = base.retained.regions
    pane = replace(chrome.draws[0], title="Sound Lab *", focused=False)
    changed = replace(base.retained, regions=(replace(chrome, draws=(pane,)), content))
    offer = _offer(2, changed)
    wire = display_offer_to_wire(offer, base=base)
    assert wire["retained"]["regions"][0]["changed"][0]["kind"] == "pane"
    assert wire["retained"]["regions"][1]["changed"] == []
    assert display_offer_from_wire(json.loads(json.dumps(wire)), base) == offer


def test_pane_delta_rechecks_content_region_changes_against_unchanged_chrome():
    base = _offer(1, _plane())
    wire = display_offer_to_wire(_offer(2, base.retained), base=base)
    wire["retained"]["regions"][1]["clip_x"] = 2
    with pytest.raises(ValueError, match="content bounds"):
        display_offer_from_wire(wire, base)


def test_pane_delta_removal_leaves_content_region_unchanged():
    base = _offer(1, _plane())
    chrome, content = base.retained.regions
    plane = replace(base.retained, regions=(replace(chrome, draws=()), content))
    offer = _offer(2, plane)
    wire = display_offer_to_wire(offer, base=base)
    assert wire["retained"]["regions"][0]["removed"] == [["object", 41]]
    assert display_offer_from_wire(wire, base) == offer
