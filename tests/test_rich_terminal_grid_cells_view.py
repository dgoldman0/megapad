"""Typed spreadsheet roles preserve the existing immutable TEXT_GRID geometry."""

from copy import copy
from dataclasses import FrozenInstanceError, replace
from types import MappingProxyType

import pytest

from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    ControlDefinition, ControlKind, ControlState, ObjectBounds, OwnerScene,
    RegionDefinition, RetainedScene, SceneModelState, SceneUsage,
)
from rich_terminal.retained_view import (
    RetainedViewError, TextGridDraw, project_composite_draw_plane,
    retained_draw_control_ids, retained_draw_key,
)
from rich_terminal.semantic_content import (
    GRID_DATA_ROLES, SemanticContentFlag, SemanticTextContent,
    SemanticTextItem, SemanticTextRole, SemanticTextState,
)
from rich_terminal.update_authority import TerminalGeometry

OWNER = OwnerIdentity(7, 3, 1, 2)
GEOMETRY = TerminalGeometry(40, 12, 9)
VISIBLE = ControlState.VISIBLE | ControlState.ENABLED
BOUNDS = ObjectBounds(2, 3, 32, 3)
TYPED_ROLES = (SemanticTextRole.NUMBER, SemanticTextRole.FORMULA, SemanticTextRole.ERROR)


def _content():
    return SemanticTextContent(
        content_revision=19, rows=50, columns=100,
        viewport_row=10, viewport_column=20, viewport_rows=3, viewport_columns=32,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=103, primary_offset=0, anchor_key=0, anchor_offset=0,
        items=(
            SemanticTextItem(101, 10, 24, 1, 10, SemanticTextRole.COLUMN_HEADER,
                             SemanticTextState(0), "A"),
            SemanticTextItem(102, 11, 20, 1, 3, SemanticTextRole.ROW_HEADER,
                             SemanticTextState(0), "11"),
            SemanticTextItem(103, 11, 24, 1, 10, SemanticTextRole.NUMBER,
                             SemanticTextState.CURRENT, "-42.50"),
            SemanticTextItem(104, 11, 34, 1, 10, SemanticTextRole.FORMULA,
                             SemanticTextState(0), "123"),
            SemanticTextItem(105, 11, 44, 1, 8, SemanticTextRole.ERROR,
                             SemanticTextState(0), "#ERR"),
            SemanticTextItem(106, 12, 24, 1, 10, SemanticTextRole.CONTENT,
                             SemanticTextState(0), "123"),
            SemanticTextItem(107, 12, 34, 1, 10, SemanticTextRole.FORMULA,
                             SemanticTextState.UNAVAILABLE, "stale"),
        ),
    )


def _control(content=None, *, state=VISIBLE):
    return ControlDefinition(OWNER, 11, ControlKind.TEXT_GRID, state, -2,
                             1, 0, 0, BOUNDS, "", "", _content() if content is None else content)


def _view(control, *, region_visible=True, retained_visible=True):
    region = RegionDefinition(OWNER, 1, 0, 0, 40, 12, 0, 0, 40, 12,
                              0, region_visible, True, 9)
    scene = OwnerScene(
        OWNER, MappingProxyType({1: region}), MappingProxyType({}), MappingProxyType({}),
        SceneUsage(regions=1, objects=1 + len(control.content.items)),
        controls=MappingProxyType({11: control}),
    )
    row = (BLANK_CELL,) * 40
    cell = TerminalView(8, 7, 3, 4, 40, 12, (row,) * 12, (), Cursor(0, 0, False))
    retained = SceneModelState(6, GEOMETRY, RetainedScene(MappingProxyType({1: scene})),
                               None, None, None, retained_visible, True)
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def test_projection_preserves_typed_roles_value_text_keys_and_unequal_spans():
    content = _content()
    scope, plane = project_composite_draw_plane(_view(_control(content)))
    draw, = plane.regions[0].draws
    assert draw == TextGridDraw(11, VISIBLE, 0, -2, BOUNDS, content)
    assert scope.model_revision == 6
    assert draw.content.content_revision == 19
    assert (draw.content.viewport_row, draw.content.viewport_column) == (10, 20)
    assert (draw.content.viewport_rows, draw.content.viewport_columns) == (3, 32)
    assert [(item.item_key, item.role, item.column_span, item.text) for item in draw.content.items] == [
        (101, SemanticTextRole.COLUMN_HEADER, 10, "A"),
        (102, SemanticTextRole.ROW_HEADER, 3, "11"),
        (103, SemanticTextRole.NUMBER, 10, "-42.50"),
        (104, SemanticTextRole.FORMULA, 10, "123"),
        (105, SemanticTextRole.ERROR, 8, "#ERR"),
        (106, SemanticTextRole.CONTENT, 10, "123"),
        (107, SemanticTextRole.FORMULA, 10, "stale"),
    ]
    assert draw.content.requires_grid_cells
    assert retained_draw_key(draw) == ("control", 11)
    assert retained_draw_control_ids(draw) == frozenset({11})
    with pytest.raises(FrozenInstanceError):
        draw.content.items[2].role = SemanticTextRole.CONTENT


def test_role_changes_keep_old_plane_immutable_and_geometry_unchanged():
    content = _content()
    _, old = project_composite_draw_plane(_view(_control(content)))
    items = tuple(replace(item, role=SemanticTextRole.CONTENT) if item.role in TYPED_ROLES else item
                  for item in content.items)
    updated = replace(content, content_revision=20, items=items)
    _, new = project_composite_draw_plane(_view(_control(updated)))
    prior, current = old.regions[0].draws[0], new.regions[0].draws[0]
    assert prior.content.items[2].role is SemanticTextRole.NUMBER
    assert current.content.items[2].role is SemanticTextRole.CONTENT
    assert prior.content.requires_grid_cells
    assert not current.content.requires_grid_cells
    assert (prior.content.content_revision, current.content.content_revision) == (19, 20)
    assert current.bounds == prior.bounds
    assert [(i.item_key, i.row, i.column, i.row_span, i.column_span, i.text) for i in current.content.items] == [
        (i.item_key, i.row, i.column, i.row_span, i.column_span, i.text) for i in prior.content.items
    ]


@pytest.mark.parametrize("role", TYPED_ROLES)
def test_text_area_rejects_typed_grid_roles_even_for_otherwise_valid_full_row_content(role):
    item = SemanticTextItem(1, 0, 0, 1, 10, role, SemanticTextState(0), "12")
    content = SemanticTextContent(1, 1, 10, 0, 0, 1, 10,
                                  SemanticContentFlag(0), 0, 0, 0, 0, (item,))
    assert content.requires_grid_cells
    assert not content.text_area_compatible
    with pytest.raises(ValueError, match="TEXT_AREA"):
        ControlDefinition(OWNER, 11, ControlKind.TEXT_AREA, VISIBLE, -2,
                          1, 0, 0, BOUNDS, "", "", content)
    forged = copy(_control(content))
    object.__setattr__(forged, "kind", ControlKind.TEXT_AREA)
    with pytest.raises(RetainedViewError, match="TEXT_AREA"):
        project_composite_draw_plane(_view(forged))


@pytest.mark.parametrize("hidden", ["control", "region", "retained"])
def test_hidden_typed_grid_omits_draw(hidden):
    control = _control(state=ControlState.ENABLED if hidden == "control" else VISIBLE)
    _, plane = project_composite_draw_plane(_view(
        control, region_visible=hidden != "region", retained_visible=hidden != "retained",
    ))
    assert not any(region.draws for region in plane.regions)


def test_data_role_vocabulary_excludes_headers_and_text_is_not_reinterpreted():
    assert GRID_DATA_ROLES == frozenset({SemanticTextRole.CONTENT, *TYPED_ROLES})
    assert SemanticTextRole.ROW_HEADER not in GRID_DATA_ROLES
    assert SemanticTextRole.COLUMN_HEADER not in GRID_DATA_ROLES
    content = _content()
    assert content.items[5].text == content.items[3].text == "123"
    assert content.items[5].role is SemanticTextRole.CONTENT
    assert content.items[3].role is SemanticTextRole.FORMULA
