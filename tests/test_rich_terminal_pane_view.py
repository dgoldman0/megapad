"""Pane projection preserves explicit geometry, visibility, and owner scope."""

from dataclasses import FrozenInstanceError, replace
from types import MappingProxyType

import pytest

from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    GlyphRunBody, ObjectBounds, ObjectDefinition, OwnerScene, PaneBody,
    RegionDefinition, RetainedScene, RGBA, SceneModelState, SceneUsage,
)
from rich_terminal.retained_view import (
    PaneDraw, RetainedViewError, project_composite_draw_plane,
    retained_draw_control_ids, retained_draw_key, retained_draw_order,
)
from rich_terminal.update_authority import TerminalGeometry


GEOMETRY = TerminalGeometry(20, 12, 9)
OWNER = OwnerIdentity(7, 3, 1, 2)
BOUNDS = ObjectBounds(2, 1, 12, 8)
CONTENT = ObjectBounds(1, 2, 10, 5)


def _scene(*, pane_visible=True, content_visible=True):
    chrome = RegionDefinition(
        OWNER, 1, 1, 1, 18, 10, 1, 1, 18, 10, 0, True, True, 9
    )
    # Content logical coordinates need not coincide with the pane rectangle.
    content = RegionDefinition(
        OWNER, 2, 0, 0, 20, 12, 4, 4, 10, 5, 1, content_visible, True, 9
    )
    pane = ObjectDefinition(
        OWNER, 11, 1, 0, BOUNDS, -2, pane_visible,
        PaneBody(2, CONTENT, "Sound Lab — 茶", pane_visible),
    )
    text = ObjectDefinition(
        OWNER, 12, 2, 0, ObjectBounds(4, 4, 10, 1), 0, True,
        GlyphRunBody(RGBA(255, 255, 255, 255), RGBA(0, 0, 0, 255), 0, "content"),
    )
    return OwnerScene(
        owner=OWNER,
        regions=MappingProxyType({1: chrome, 2: content}),
        objects=MappingProxyType({11: pane, 12: text}),
        series=MappingProxyType({}),
        usage=SceneUsage(regions=2, objects=2, utf8_bytes=100),
    )


def _composite(scene, *, visible=True):
    row = (BLANK_CELL,) * GEOMETRY.cols
    cell = TerminalView(
        8, 7, 3, 4, 20, 12, (row,) * 12, (), Cursor(0, 0, False)
    )
    retained = SceneModelState(
        revision=6, geometry=GEOMETRY,
        active=RetainedScene(MappingProxyType({1: scene})),
        hidden=None, hidden_kind=None, requirement=None,
        retained_visible=visible, retained_initialized=True,
    )
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def test_pane_projection_copies_exact_scope_and_independent_content_geometry():
    source = _scene()
    scope, plane = project_composite_draw_plane(_composite(source))
    assert (scope.session_id, scope.presentation_epoch, scope.model_revision) == (7, 3, 6)
    assert scope.geometry_generation == 9
    pane = plane.regions[0].draws[0]
    assert pane == PaneDraw(11, -2, BOUNDS, 2, CONTENT, "Sound Lab — 茶", True)
    assert retained_draw_key(pane) == ("object", 11)
    assert retained_draw_control_ids(pane) == frozenset()
    assert (plane.regions[1].logical_x, plane.regions[1].logical_y) == (0, 0)
    assert (plane.regions[1].clip_x, plane.regions[1].clip_y) == (4, 4)
    with pytest.raises(FrozenInstanceError):
        pane.title = "mutated"


@pytest.mark.parametrize("pane_visible,content_visible", [(False, True), (True, False), (False, False)])
def test_pane_visibility_never_changes_content_visibility(pane_visible, content_visible):
    _, plane = project_composite_draw_plane(
        _composite(_scene(pane_visible=pane_visible, content_visible=content_visible))
    )
    by_id = {region.region_id: region for region in plane.regions}
    assert bool(by_id[1].draws) is pane_visible
    assert (2 in by_id) is content_visible
    if content_visible:
        assert by_id[2].draws[0].object_id == 12


def test_hidden_plane_projects_no_pane_or_content():
    _, plane = project_composite_draw_plane(_composite(_scene(), visible=False))
    assert not plane.retained_visible
    assert plane.regions == ()


@pytest.mark.parametrize("change", [
    {"clipped": False, "clip_x": 0, "clip_y": 0, "clip_cols": 0, "clip_rows": 0},
    {"clip_x": 3},
    {"clip_y": 3},
    {"clip_cols": 11},
    {"clip_rows": 6},
    {"z_order": -1},
])
def test_projection_rejects_invalid_content_binding_even_when_hidden(change):
    scene = _scene(content_visible=False)
    content = replace(scene.regions[2], **change)
    scene = replace(scene, regions=MappingProxyType({1: scene.regions[1], 2: content}))
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_composite(scene))


def test_projection_rejects_missing_and_duplicate_content_bindings():
    scene = _scene()
    # Remove the content object as well so only the pane has a dangling edge.
    missing = replace(
        scene, regions=MappingProxyType({1: scene.regions[1]}),
        objects=MappingProxyType({11: scene.objects[11]}),
    )
    with pytest.raises(RetainedViewError, match="exact-owner content region"):
        project_composite_draw_plane(_composite(missing))
    duplicate = replace(scene.objects[11], object_id=13)
    scene = replace(scene, objects=MappingProxyType({**scene.objects, 13: duplicate}))
    with pytest.raises(RetainedViewError, match="multiple panes"):
        project_composite_draw_plane(_composite(scene))


def test_empty_explicit_content_clip_is_valid():
    scene = _scene()
    content = replace(scene.regions[2], clip_x=0, clip_y=0, clip_cols=0, clip_rows=0)
    scene = replace(scene, regions=MappingProxyType({1: scene.regions[1], 2: content}))
    _, plane = project_composite_draw_plane(_composite(scene))
    assert plane.regions[1].clip_cols == plane.regions[1].clip_rows == 0


def test_pane_draw_keeps_title_metadata_without_top_chrome_space():
    pane = PaneDraw(1, 0, ObjectBounds(0, 0, 2, 3), 2,
                    ObjectBounds(0, 0, 2, 3), "Metadata title")
    assert pane.title == "Metadata title"
    assert pane.parent_bounds == ()


@pytest.mark.parametrize("changes", [
    {"object_id": True}, {"content_region_id": False}, {"z_order": True},
    {"focused": 1}, {"parent_bounds": (ObjectBounds(0, 0, 20, 12),)},
    {"content_bounds": ObjectBounds(-1, 1, 2, 2)},
    {"content_bounds": ObjectBounds(2, 2, 11, 5)},
    {"title": "bad\x85title"}, {"title": "bad\u2028title"},
    {"title": "bad\ud800title"},
])
def test_pane_draw_rejects_noncanonical_values(changes):
    pane = PaneDraw(11, -2, BOUNDS, 2, CONTENT, "Desk", True)
    with pytest.raises((TypeError, ValueError)):
        replace(pane, **changes)


def test_pane_uses_object_identity_and_stable_z_order():
    pane = PaneDraw(11, 0, BOUNDS, 2, CONTENT, "Desk")
    earlier = replace(pane, object_id=10)
    later = replace(pane, object_id=9, z_order=1)
    assert retained_draw_order((later, pane, earlier)) == (earlier, pane, later)
