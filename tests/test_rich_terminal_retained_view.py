"""Focused immutable renderer draw-plane projection tests."""

from __future__ import annotations

import hashlib
from dataclasses import replace
from types import MappingProxyType

import pytest

from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity, ResourceFormat
from rich_terminal.retained_resources import ResourceDeclaration, RGBAResource
from rich_terminal.retained_scene import (
    GroupBody,
    HiddenTargetKind,
    GlyphRunBody,
    ImageBody,
    ImageFit,
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
    Sample,
    SceneModelState,
    SceneUsage,
    SeriesDefinition,
    MeterBody,
    StatusBody,
    TimestampMode,
    WaveformBody,
)
from rich_terminal.retained_view import (
    DisplayScope,
    ImageDraw,
    ImageResourceManifest,
    MeterDraw,
    PlotDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    RetainedViewError,
    SeriesHistoryDraw,
    StatusDraw,
    WaveformDraw,
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


def _resource(
    owner: OwnerIdentity,
    resource_id: int,
    pixels: bytes,
    *,
    width: int = 1,
    height: int = 1,
) -> RGBAResource:
    return RGBAResource(
        owner,
        ResourceDeclaration(
            resource_id=resource_id,
            format=ResourceFormat.RGBA8,
            width=width,
            height=height,
            byte_length=len(pixels),
            digest=hashlib.sha3_256(pixels).digest(),
        ),
        pixels,
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


def _owner_scene(owner, regions, objects, series=()) -> OwnerScene:
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
        series=MappingProxyType(
            {definition.series_id: definition for definition in series}
        ),
        usage=SceneUsage(
            regions=len(regions),
            objects=len(objects),
            series=len(series),
            utf8_bytes=utf8,
            sample_slots=sum(definition.history_capacity for definition in series),
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
    resources: tuple[RGBAResource, ...] = (),
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
        resources=resources,
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


def test_projection_fails_closed_on_a_missing_series_and_projects_groups():
    owner = _owner(5)
    region = _region(owner, 1)
    plot = _object(owner, 1, 1, PlotBody(1, 0, 10, WHITE, GREEN))
    with pytest.raises(RetainedViewError, match="refers to a missing series"):
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


def test_projection_copies_images_and_deduplicates_sorted_resource_manifests():
    owner = _owner(5, 7)
    region = _region(owner, 1)
    group = replace(
        _object(owner, 1, 1, GroupBody()),
        bounds=ObjectBounds(0x10000000, 0, 0xEFFFFFFF, 0xFFFFFFFF),
    )
    later = _object(
        owner,
        2,
        1,
        ImageBody(9, ImageFit.COVER, 128),
        z_order=2,
        parent=1,
    )
    first = _object(
        owner,
        3,
        1,
        ImageBody(3, ImageFit.STRETCH, 255),
        z_order=-1,
    )
    repeated = _object(
        owner,
        4,
        1,
        ImageBody(9, ImageFit.CONTAIN, 64),
        z_order=3,
        parent=1,
    )
    invisible = _object(
        owner,
        5,
        1,
        ImageBody(11, ImageFit.CONTAIN, 255),
        visible=False,
    )
    resource_3 = _resource(owner, 3, bytes((3, 4, 5, 255)))
    resource_9 = _resource(
        owner,
        9,
        bytes((9, 8, 7, 255, 6, 5, 4, 255)),
        width=2,
    )
    resource_11 = _resource(owner, 11, bytes((1, 2, 3, 255)))

    _, plane = project_composite_draw_plane(
        _composite(
            [
                _owner_scene(
                    owner,
                    [region],
                    [repeated, invisible, later, group, first],
                )
            ],
            resources=(resource_11, resource_9, resource_3, resource_9),
        )
    )

    assert isinstance(plane, RetainedDrawPlane)
    first_draw, later_draw, repeated_draw = plane.regions[0].draws
    assert all(
        isinstance(draw, ImageDraw)
        for draw in (first_draw, later_draw, repeated_draw)
    )
    assert (
        first_draw.object_id,
        first_draw.resource_id,
        first_draw.fit,
        first_draw.opacity,
    ) == (3, 3, ImageFit.STRETCH, 255)
    assert (
        later_draw.object_id,
        later_draw.resource_id,
        later_draw.fit,
        later_draw.opacity,
        later_draw.parent_bounds,
    ) == (2, 9, ImageFit.COVER, 128, (group.bounds,))
    assert repeated_draw.parent_bounds == (group.bounds,)

    assert [manifest.resource_id for manifest in plane.resources] == [3, 9]
    manifest_3, manifest_9 = plane.resources
    assert isinstance(manifest_3, ImageResourceManifest)
    assert manifest_3.key == (
        owner.owner_id,
        owner.owner_generation,
        3,
        ResourceFormat.RGBA8,
        1,
        1,
        4,
        hashlib.sha3_256(bytes((3, 4, 5, 255))).digest(),
    )
    assert (
        manifest_9.width,
        manifest_9.height,
        manifest_9.byte_length,
        manifest_9.sha3_256,
    ) == (2, 1, 8, resource_9.digest)
    assert not hasattr(manifest_9, "data")

    with pytest.raises(ValueError, match="no exact resource manifest"):
        replace(plane, resources=())
    with pytest.raises(ValueError, match="identities are duplicated"):
        replace(
            plane,
            resources=(manifest_3, manifest_3, manifest_9),
        )
    empty_region = replace(plane.regions[0], draws=())
    with pytest.raises(ValueError, match="unreferenced IMAGE resource manifest"):
        replace(plane, regions=(empty_region,))


def test_projection_rejects_missing_and_wrong_exact_owner_image_pins():
    owner = _owner(5, 7)
    region = _region(owner, 1)
    image = _object(
        owner,
        1,
        1,
        ImageBody(4, ImageFit.CONTAIN, 255),
    )
    scene = _owner_scene(owner, [region], [image])

    with pytest.raises(RetainedViewError, match="no exact pinned resource"):
        project_composite_draw_plane(_composite([scene]))

    wrong_owner = _owner(6, 7)
    wrong_pin = _resource(wrong_owner, 4, bytes((1, 2, 3, 255)))
    with pytest.raises(RetainedViewError, match="wrong exact owner"):
        project_composite_draw_plane(
            _composite([scene], resources=(wrong_pin,))
        )


def test_hidden_retained_plane_exposes_no_image_draws_or_manifests():
    owner = _owner(5)
    region = _region(owner, 1)
    image = _object(
        owner,
        1,
        1,
        ImageBody(1, ImageFit.COVER, 200),
    )
    resource = _resource(owner, 1, bytes((10, 20, 30, 255)))

    _, plane = project_composite_draw_plane(
        _composite(
            [_owner_scene(owner, [region], [image])],
            visible=False,
            resources=(resource,),
        )
    )

    assert not plane.retained_visible
    assert plane.regions == ()
    assert plane.resources == ()
    manifest = ImageResourceManifest(
        owner.owner_id,
        owner.owner_generation,
        resource.resource_id,
        resource.format,
        resource.width,
        resource.height,
        resource.byte_length,
        resource.digest,
    )
    with pytest.raises(ValueError, match="hidden retained plane"):
        RetainedDrawPlane(True, False, (), (), (manifest,))


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


def test_projection_copies_one_history_for_multiple_series_consumers():
    owner = _owner(5)
    region = _region(owner, 1)
    history = SeriesDefinition(
        owner,
        7,
        8,
        TimestampMode.EXPLICIT,
        0,
        (
            Sample(10, -5),
            Sample(20, 15),
            Sample(40, 0),
        ),
    )
    group = _object(owner, 1, 1, GroupBody())
    plot = _object(
        owner,
        2,
        1,
        PlotBody(7, -10, 20, GREEN, RGBA(20, 220, 80, 96), True, True),
        z_order=1,
        parent=1,
    )
    waveform = _object(
        owner,
        3,
        1,
        WaveformBody(7, -20, 20, WHITE, GREEN, 0, True),
        z_order=2,
        parent=1,
    )

    _, plane = project_composite_draw_plane(
        _composite(
            [_owner_scene(owner, [region], [waveform, plot, group], [history])]
        )
    )

    assert plane.series == (
        SeriesHistoryDraw(owner.owner_id, owner.owner_generation, 7, history.samples),
    )
    assert plane.series[0].samples is history.samples
    projected_plot, projected_waveform = plane.regions[0].draws
    assert isinstance(projected_plot, PlotDraw)
    assert (
        projected_plot.series_id,
        projected_plot.minimum,
        projected_plot.maximum,
        projected_plot.fill_to_minimum,
        projected_plot.draw_points,
    ) == (7, -10, 20, True, True)
    assert isinstance(projected_waveform, WaveformDraw)
    assert (
        projected_waveform.series_id,
        projected_waveform.zero_value,
        projected_waveform.draw_zero_line,
    ) == (7, 0, True)


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
