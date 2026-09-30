"""Status fields preserve the guest's slots through immutable projection."""

from dataclasses import FrozenInstanceError, replace
from types import MappingProxyType

import pytest

from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    GroupBody, ObjectBounds, ObjectDefinition, OwnerScene, RegionDefinition,
    RetainedScene, SceneModelState, SceneUsage, StatusFieldBody, StatusSeverity,
)
from rich_terminal.retained_view import (
    StatusFieldDraw, project_composite_draw_plane,
    retained_draw_control_ids, retained_draw_key, retained_draw_order,
)
from rich_terminal.update_authority import TerminalGeometry


OWNER = OwnerIdentity(7, 3, 1, 2)
GEOMETRY = TerminalGeometry(20, 12, 9)
BOUNDS = ObjectBounds(-1, 2, 12, 1)
OUTER = ObjectBounds(2, 1, 16, 8)
INNER = ObjectBounds(-2, 1, 14, 5)


def _scene(*, field_visible=True, group_visible=True, region_visible=True):
    region = RegionDefinition(
        OWNER, 1, 0, 0, 20, 12, 0, 0, 20, 12, 0, region_visible, True, 9,
    )
    outer = ObjectDefinition(OWNER, 10, 1, 0, OUTER, 0, True, GroupBody())
    inner = ObjectDefinition(OWNER, 11, 1, 10, INNER, 0, group_visible, GroupBody())
    field = ObjectDefinition(
        OWNER, 12, 1, 11, BOUNDS, -2, field_visible,
        StatusFieldBody("Mode", "茶 — ready", 5, StatusSeverity.SUCCESS, True),
    )
    return OwnerScene(
        owner=OWNER,
        regions=MappingProxyType({1: region}),
        objects=MappingProxyType({10: outer, 11: inner, 12: field}),
        series=MappingProxyType({}),
        usage=SceneUsage(regions=1, objects=3, utf8_bytes=100),
    )


def _composite(scene, *, visible=True):
    row = (BLANK_CELL,) * 20
    cell = TerminalView(8, 7, 3, 4, 20, 12, (row,) * 12, (), Cursor(0, 0, False))
    retained = SceneModelState(
        revision=6, geometry=GEOMETRY,
        active=RetainedScene(MappingProxyType({1: scene})),
        hidden=None, hidden_kind=None, requirement=None,
        retained_visible=visible, retained_initialized=True,
    )
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def test_projection_keeps_exact_status_slots_and_nested_group_geometry():
    scene = _scene()
    scope, plane = project_composite_draw_plane(_composite(scene))
    assert (scope.session_id, scope.presentation_epoch, scope.model_revision) == (7, 3, 6)
    draw, = plane.regions[0].draws
    assert draw == StatusFieldDraw(
        12, -2, BOUNDS, "Mode", "茶 — ready", 5,
        StatusSeverity.SUCCESS, True, (OUTER, INNER),
    )
    assert draw.parent_bounds[0] is not scene.objects[10].bounds
    assert draw.parent_bounds[1] is not scene.objects[11].bounds
    assert retained_draw_key(draw) == ("object", 12)
    assert retained_draw_control_ids(draw) == frozenset()
    with pytest.raises(FrozenInstanceError):
        draw.value = "mutated"


@pytest.mark.parametrize("changes", [
    {"field_visible": False}, {"group_visible": False}, {"region_visible": False},
])
def test_hidden_field_or_ancestor_produces_no_draw(changes):
    _, plane = project_composite_draw_plane(_composite(_scene(**changes)))
    assert not any(region.draws for region in plane.regions)


def test_hidden_retained_plane_does_not_project_status_fields():
    _, plane = project_composite_draw_plane(_composite(_scene(), visible=False))
    assert not plane.retained_visible
    assert plane.regions == ()


def test_replacement_keeps_previous_plane_immutable_and_object_identity_stable():
    scene = _scene()
    _, old = project_composite_draw_plane(_composite(scene))
    field = replace(scene.objects[12], body=StatusFieldBody(
        "Mode", "Paused", 5, StatusSeverity.WARNING,
    ))
    changed = replace(scene, objects=MappingProxyType({**scene.objects, 12: field}))
    _, new = project_composite_draw_plane(_composite(changed))
    prior, current = old.regions[0].draws[0], new.regions[0].draws[0]
    assert prior.value == "茶 — ready"
    assert current.value == "Paused"
    assert current.severity is StatusSeverity.WARNING
    assert retained_draw_key(prior) == retained_draw_key(current)
    assert current.bounds == prior.bounds
    assert current.parent_bounds == prior.parent_bounds
    assert not current.emphasized


@pytest.mark.parametrize("label,value,label_cols", [
    ("", "ready", 0), ("Mode", "", 12), ("", "", 0),
    ("An intentionally long label", "untruncated metadata", 5),
])
def test_projection_preserves_metadata_for_explicit_empty_or_narrow_slots(label, value, label_cols):
    draw = StatusFieldDraw(1, 0, BOUNDS, label, value, label_cols)
    assert (draw.label, draw.value, draw.label_cols) == (label, value, label_cols)


@pytest.mark.parametrize("changes", [
    {"object_id": True}, {"object_id": 0}, {"object_id": 1 << 64},
    {"z_order": False}, {"z_order": 1 << 31},
    {"bounds": ObjectBounds(0, 0, 12, 2)},
    {"label_cols": True}, {"label_cols": -1}, {"label_cols": 13},
    {"label_cols": 0}, {"label_cols": 12},
    {"severity": True}, {"severity": 5}, {"severity": "1"},
    {"emphasized": 1}, {"emphasized": "true"},
    {"label": None}, {"value": 123},
    {"label": "bad\x00"}, {"value": "bad\x1f"},
    {"label": "bad\x7f"}, {"value": "bad\x85"},
    {"label": "bad\u2028"}, {"value": "bad\u2029"},
    {"label": "bad\ud800"}, {"value": "bad\udfff"},
    {"parent_bounds": ((0, 0, 12, 1),)},
])
def test_draw_rejects_invalid_shape_and_noncanonical_values(changes):
    draw = StatusFieldDraw(12, -2, BOUNDS, "Mode", "Ready", 5)
    with pytest.raises((TypeError, ValueError)):
        replace(draw, **changes)


def test_field_sorting_uses_object_identity_after_z_order():
    field = StatusFieldDraw(12, -2, BOUNDS, "Mode", "Ready", 5)
    earlier = replace(field, object_id=11)
    later = replace(field, object_id=10, z_order=0)
    assert retained_draw_order((later, field, earlier)) == (earlier, field, later)
