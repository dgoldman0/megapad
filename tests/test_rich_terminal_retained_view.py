"""Focused immutable renderer draw-plane projection tests."""

from __future__ import annotations

from dataclasses import replace
from types import MappingProxyType

import pytest

from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    GroupBody,
    HiddenTargetKind,
    GlyphRunBody,
    ObjectBounds,
    ObjectDefinition,
    OwnerScene,
    Point,
    PolylineBody,
    PlotBody,
    ReadoutBody,
    ReadoutFormat,
    RegionDefinition,
    RetainedScene,
    RGBA,
    SceneModelState,
    SceneUsage,
    MeterBody,
    StatusBody,
)
from rich_terminal.retained_view import (
    DisplayScope,
    MeterDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedViewError,
    StatusDraw,
    project_composite_draw_plane,
)
from rich_terminal.update_authority import TerminalGeometry


SESSION_ID = 0x0123456789ABCDEF
PRESENTATION_EPOCH = 3
GEOMETRY = TerminalGeometry(8, 4, 9)
WHITE = RGBA(255, 255, 255, 255)
GREEN = RGBA(20, 220, 80, 255)


def _cell_view(*, revision: int = 5) -> TerminalView:
    row = (BLANK_CELL,) * GEOMETRY.cols
    return TerminalView(
        attachment_epoch=7,
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        revision=revision,
        cols=GEOMETRY.cols,
        rows=GEOMETRY.rows,
        cells=(row,) * GEOMETRY.rows,
        dirty_spans=(),
        cursor=Cursor(0, 0, True),
    )


def _owner(owner_id: int, generation: int = 1) -> OwnerIdentity:
    return OwnerIdentity(
        SESSION_ID,
        PRESENTATION_EPOCH,
        owner_id,
        generation,
    )


def _region(
    owner: OwnerIdentity,
    region_id: int,
    *,
    z_order: int = 0,
    visible: bool = True,
) -> RegionDefinition:
    return RegionDefinition(
        owner=owner,
        region_id=region_id,
        cell_x=0,
        cell_y=0,
        cell_cols=GEOMETRY.cols,
        cell_rows=GEOMETRY.rows,
        z_order=z_order,
        visible=visible,
        clipped=True,
        geometry_generation=GEOMETRY.generation,
    )


def _object(
    owner: OwnerIdentity,
    object_id: int,
    region_id: int,
    body,
    *,
    z_order: int = 0,
    visible: bool = True,
    parent: int = 0,
) -> ObjectDefinition:
    return ObjectDefinition(
        owner=owner,
        object_id=object_id,
        region_id=region_id,
        parent_object_id=parent,
        bounds=ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF),
        z_order=z_order,
        visible=visible,
        body=body,
    )


def _owner_scene(owner, regions, objects) -> OwnerScene:
    utf8 = sum(
        (
            len(definition.body.text.encode("utf-8"))
            if isinstance(definition.body, GlyphRunBody)
            else len(definition.body.formatted_bytes(0xFFFFFFFF))
        )
        for definition in objects
        if isinstance(definition.body, (GlyphRunBody, ReadoutBody))
    )
    return OwnerScene(
        owner=owner,
        regions=MappingProxyType(
            {definition.region_id: definition for definition in regions}
        ),
        objects=MappingProxyType(
            {definition.object_id: definition for definition in objects}
        ),
        series=MappingProxyType({}),
        usage=SceneUsage(
            regions=len(regions),
            objects=len(objects),
            utf8_bytes=utf8,
        ),
    )


def _composite(
    owner_scenes,
    *,
    revision: int = 8,
    retained_revision: int = 8,
    initialized: bool = True,
    visible: bool = True,
    hidden: RetainedScene | None = None,
) -> CompositeTerminalView:
    active = RetainedScene(
        MappingProxyType(
            {owner_scene.owner.owner_id: owner_scene for owner_scene in owner_scenes}
        )
    )
    state = SceneModelState(
        revision=retained_revision,
        geometry=GEOMETRY,
        active=active,
        hidden=hidden,
        hidden_kind=None if hidden is None else HiddenTargetKind.REPLACE,
        requirement=None,
        retained_visible=visible,
        retained_initialized=initialized,
    )
    return CompositeTerminalView(
        presentation_epoch=PRESENTATION_EPOCH,
        revision=revision,
        geometry=GEOMETRY,
        cell=_cell_view(),
        retained=state,
    )


def test_projection_preserves_global_scope_and_deterministic_draw_order():
    later_owner = _owner(9, 4)
    later_region = _region(later_owner, 3, z_order=4)
    later_scene = _owner_scene(
        later_owner,
        [later_region],
        [
            _object(
                later_owner,
                20,
                3,
                GlyphRunBody(WHITE, GREEN, 0, "later"),
                z_order=2,
            ),
            _object(
                later_owner,
                10,
                3,
                GlyphRunBody(GREEN, WHITE, 1, "first"),
                z_order=-2,
            ),
        ],
    )
    first_owner = _owner(2, 6)
    first_region = _region(first_owner, 7, z_order=-1)
    first_scene = _owner_scene(
        first_owner,
        [first_region],
        [_object(first_owner, 4, 7, GlyphRunBody(WHITE, GREEN, 0, "front"))],
    )

    scope, plane = project_composite_draw_plane(
        _composite([later_scene, first_scene])
    )

    assert scope == DisplayScope(
        attachment_epoch=7,
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        model_revision=8,
        geometry_generation=GEOMETRY.generation,
        cell_revision=5,
        retained_revision=8,
    )
    assert [region.owner_id for region in plane.regions] == [2, 9]
    assert [draw.object_id for draw in plane.regions[1].draws] == [10, 20]
    first_draw = plane.regions[1].draws[0]
    assert (first_draw.text, first_draw.attributes) == ("first", 1)
    assert first_draw.foreground == GREEN
    assert first_draw.background == WHITE


def test_hidden_planes_and_effectively_hidden_children_emit_no_draw_values():
    owner = _owner(5)
    region = _region(owner, 1)
    unsupported = _object(
        owner,
        1,
        1,
        PlotBody(1, 0, 10, WHITE, GREEN),
    )
    hidden_scope, hidden_plane = project_composite_draw_plane(
        _composite([_owner_scene(owner, [region], [unsupported])], visible=False)
    )

    assert hidden_scope.model_revision == 8
    assert hidden_plane.retained_initialized
    assert not hidden_plane.retained_visible
    assert hidden_plane.regions == ()

    hidden_group = _object(owner, 2, 1, GroupBody(), visible=False)
    nested = _object(
        owner,
        3,
        1,
        GlyphRunBody(WHITE, GREEN, 0, "not physical"),
        parent=2,
    )
    _, visible_plane = project_composite_draw_plane(
        _composite([_owner_scene(owner, [region], [nested, hidden_group])])
    )
    assert len(visible_plane.regions) == 1
    assert visible_plane.regions[0].draws == ()


def test_projection_fails_closed_on_visible_unsupported_objects_and_projects_groups():
    owner = _owner(5)
    region = _region(owner, 1)
    plot = _object(owner, 1, 1, PlotBody(1, 0, 10, WHITE, GREEN))
    with pytest.raises(RetainedViewError, match="visible PLOT"):
        project_composite_draw_plane(
            _composite([_owner_scene(owner, [region], [plot])])
        )

    group = replace(
        _object(owner, 2, 1, GroupBody()),
        bounds=ObjectBounds(0x10000000, 0x20000000, 0xEFFFFFFF, 0xDFFFFFFF),
    )
    nested = _object(
        owner,
        3,
        1,
        GlyphRunBody(WHITE, GREEN, 0, "nested"),
        parent=2,
    )
    _, plane = project_composite_draw_plane(
        _composite([_owner_scene(owner, [region], [nested, group])])
    )

    assert len(plane.regions[0].draws) == 1
    draw = plane.regions[0].draws[0]
    assert draw.text == "nested"
    assert draw.parent_bounds == (group.bounds,)


def test_projection_copies_polyline_geometry_and_nested_group_path():
    owner = _owner(5)
    region = _region(owner, 1)
    outer = replace(
        _object(owner, 2, 1, GroupBody()),
        bounds=ObjectBounds(0, 0, 0xCFFFFFFF, 0xEFFFFFFF),
    )
    inner = replace(
        _object(owner, 3, 1, GroupBody(), parent=2),
        bounds=ObjectBounds(0x10000000, 0x20000000, 0xFFFFFFFF, 0xFFFFFFFF),
    )
    body = PolylineBody(
        (Point(0, 0), Point(0x7FFFFFFF, 0xFFFFFFFF), Point(0xFFFFFFFF, 0)),
        0x08000000,
        GREEN,
        True,
    )
    line = replace(
        _object(owner, 4, 1, body, z_order=7, parent=3),
        bounds=ObjectBounds(0x20000000, 0, 0xDFFFFFFF, 0xFFFFFFFF),
    )

    _, plane = project_composite_draw_plane(
        _composite([_owner_scene(owner, [region], [line, inner, outer])])
    )

    assert len(plane.regions[0].draws) == 1
    draw = plane.regions[0].draws[0]
    assert isinstance(draw, PolylineDraw)
    assert draw.object_id == 4
    assert draw.z_order == 7
    assert draw.bounds == line.bounds
    assert draw.parent_bounds == (outer.bounds, inner.bounds)
    assert draw.points == body.points
    assert draw.stroke_width == body.stroke_width
    assert draw.color == GREEN
    assert draw.closed


def test_projection_formats_and_copies_instruments_without_renderer_hints():
    owner = _owner(5)
    region = _region(owner, 1)
    group = replace(
        _object(owner, 1, 1, GroupBody()),
        bounds=ObjectBounds(0x10000000, 0, 0xFFFFFFFF, 0xFFFFFFFF),
    )
    readout = _object(
        owner,
        2,
        1,
        ReadoutBody(
            WHITE,
            GREEN,
            ReadoutFormat.FIXED,
            1,
            -125,
            10,
            " dB",
        ),
        z_order=1,
        parent=1,
    )
    meter = _object(
        owner,
        3,
        1,
        MeterBody(GREEN, WHITE, False, True, -50, 50, 25),
        z_order=2,
        parent=1,
    )
    status = _object(
        owner,
        4,
        1,
        StatusBody(WHITE, GREEN, -1, 2),
        z_order=3,
        parent=1,
    )

    _, plane = project_composite_draw_plane(
        _composite([_owner_scene(owner, [region], [status, meter, readout, group])])
    )

    projected_readout, projected_meter, projected_status = plane.regions[0].draws
    assert isinstance(projected_readout, ReadoutDraw)
    assert projected_readout.text == "-12.5 dB"
    assert projected_readout.parent_bounds == (group.bounds,)
    assert isinstance(projected_meter, MeterDraw)
    assert (
        projected_meter.minimum,
        projected_meter.maximum,
        projected_meter.value,
        projected_meter.vertical,
        projected_meter.show_value,
    ) == (-50, 50, 25, False, True)
    assert isinstance(projected_status, StatusDraw)
    assert (projected_status.value, projected_status.shape) == (-1, 2)


def test_projection_rejects_a_readout_inconsistent_with_owner_utf8_usage():
    owner = _owner(5)
    region = _region(owner, 1)
    readout = _object(
        owner,
        1,
        1,
        ReadoutBody(WHITE, GREEN, ReadoutFormat.INTEGER, 0, 1234, 1, " V"),
    )
    forged = replace(
        _owner_scene(owner, [region], [readout]),
        usage=SceneUsage(regions=1, objects=1, utf8_bytes=2),
    )

    with pytest.raises(RetainedViewError, match="READOUT object 1 cannot be projected"):
        project_composite_draw_plane(_composite([forged]))


def test_projection_never_traverses_the_hidden_rebuild_target():
    owner = _owner(5)
    region = _region(owner, 1)
    active = _owner_scene(
        owner,
        [region],
        [_object(owner, 1, 1, GlyphRunBody(WHITE, GREEN, 0, "active"))],
    )
    hidden_owner = _owner(6)
    hidden_region = _region(hidden_owner, 1)
    hidden_scene = RetainedScene(
        MappingProxyType(
            {
                hidden_owner.owner_id: _owner_scene(
                    hidden_owner,
                    [hidden_region],
                    [
                        _object(
                            hidden_owner,
                            1,
                            1,
                            PlotBody(1, 0, 10, WHITE, GREEN),
                        )
                    ],
                )
            }
        )
    )

    _, plane = project_composite_draw_plane(
        _composite([active], hidden=hidden_scene)
    )

    assert [draw.text for draw in plane.regions[0].draws] == ["active"]


def test_projection_rejects_forged_plane_types_at_the_sink_boundary():
    owner = _owner(5)
    region = _region(owner, 1)
    composite = _composite([_owner_scene(owner, [region], [])])

    with pytest.raises(TypeError, match="CELL plane"):
        project_composite_draw_plane(replace(composite, cell=object()))
    with pytest.raises(TypeError, match="retained plane"):
        project_composite_draw_plane(replace(composite, retained=object()))
    with pytest.raises(TypeError, match="geometry"):
        project_composite_draw_plane(replace(composite, geometry=object()))
