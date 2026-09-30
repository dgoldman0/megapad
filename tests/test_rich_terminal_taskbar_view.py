"""Taskbar projection preserves authored slots and rejects forged scene graphs."""

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
    RetainedViewError, TaskBarDraw, TaskDraw, project_composite_draw_plane,
    retained_draw_control_ids, retained_draw_key,
)
from rich_terminal.update_authority import TerminalGeometry

OWNER = OwnerIdentity(7, 3, 1, 2)
GEOMETRY = TerminalGeometry(30, 12, 9)
VISIBLE = ControlState.VISIBLE | ControlState.ENABLED
ROOT_BOUNDS = ObjectBounds(-1, 10, 30, 1)


def _controls():
    return {
        1: ControlDefinition(OWNER, 1, ControlKind.TASKBAR, VISIBLE, 4, 1, 0, 0,
                             ROOT_BOUNDS, "", ""),
        2: ControlDefinition(OWNER, 2, ControlKind.LAUNCHER, VISIBLE, 0, 1, 1, 0,
                             ObjectBounds(0, 0, 4, 1), "Run", "F2"),
        3: ControlDefinition(OWNER, 3, ControlKind.TASK, VISIBLE | ControlState.SELECTED,
                             0, 1, 1, 1, ObjectBounds(5, 0, 10, 1), "茶 Editor", "F3"),
        4: ControlDefinition(OWNER, 4, ControlKind.TASK, VISIBLE | ControlState.MINIMIZED,
                             0, 1, 1, 2, ObjectBounds(16, 0, 12, 1), "Sound Lab", ""),
    }


def _view(controls=None, *, region_visible=True):
    controls = _controls() if controls is None else controls
    region = RegionDefinition(OWNER, 1, 0, 0, 30, 12, 0, 0, 30, 12,
                              0, region_visible, True, 9)
    scene = OwnerScene(OWNER, MappingProxyType({1: region}), MappingProxyType({}),
                       MappingProxyType({}), SceneUsage(regions=1, objects=len(controls)),
                       controls=MappingProxyType(controls))
    row = (BLANK_CELL,) * 30
    cell = TerminalView(8, 7, 3, 4, 30, 12, (row,) * 12, (), Cursor(0, 0, False))
    retained = SceneModelState(6, GEOMETRY, RetainedScene(MappingProxyType({1: scene})),
                               None, None, None, True, True)
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def _forge(control, **changes):
    result = copy(control)
    for name, value in changes.items():
        object.__setattr__(result, name, value)
    return result


def test_projection_preserves_task_identity_slots_kind_and_authoritative_state():
    scope, plane = project_composite_draw_plane(_view())
    bar, = plane.regions[0].draws
    assert isinstance(bar, TaskBarDraw)
    assert bar.bounds == ROOT_BOUNDS
    assert scope.model_revision == 6
    assert [(t.control_id, t.kind, t.bounds.cell_x, t.bounds.cell_cols) for t in bar.tasks] == [
        (2, ControlKind.LAUNCHER, 0, 4), (3, ControlKind.TASK, 5, 10),
        (4, ControlKind.TASK, 16, 12),
    ]
    assert bar.tasks[1].state & ControlState.SELECTED
    assert bar.tasks[2].state & ControlState.MINIMIZED
    assert bar.tasks[1].label == "茶 Editor"
    assert retained_draw_key(bar) == ("control", 1)
    assert retained_draw_control_ids(bar) == frozenset({1, 2, 3, 4})
    with pytest.raises(FrozenInstanceError):
        bar.tasks[1].label = "changed"


def test_hiding_a_task_preserves_other_slots_and_control_identity():
    controls = _controls()
    controls[3] = replace(controls[3], state=ControlState.ENABLED)
    _, plane = project_composite_draw_plane(_view(controls))
    bar = plane.regions[0].draws[0]
    assert [t.control_id for t in bar.tasks] == [2, 4]
    assert bar.tasks[1].bounds == ObjectBounds(16, 0, 12, 1)
    assert retained_draw_control_ids(bar) == frozenset({1, 2, 4})


@pytest.mark.parametrize("region_hidden", [False, True])
def test_hidden_root_or_region_omits_taskbar(region_hidden):
    controls = _controls()
    if not region_hidden:
        controls[1] = replace(controls[1], state=ControlState.ENABLED)
    _, plane = project_composite_draw_plane(_view(controls, region_visible=not region_hidden))
    assert not any(region.draws for region in plane.regions)


@pytest.mark.parametrize("change", [
    {"bounds": ObjectBounds(14, 0, 12, 1)},
    {"bounds": ObjectBounds(20, 0, 12, 1)},
    {"order": 1}, {"parent_control_id": 99},
])
def test_projection_rechecks_hidden_children_against_complete_graph(change):
    controls = _controls()
    controls[1] = replace(controls[1], state=ControlState.ENABLED)
    controls[4] = replace(controls[4], state=ControlState.ENABLED, **change)
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_view(controls))


def test_projection_rejects_multiple_selected_tasks_even_with_hidden_root():
    controls = _controls()
    controls[1] = replace(controls[1], state=ControlState.ENABLED)
    controls[4] = replace(controls[4], state=VISIBLE | ControlState.SELECTED)
    with pytest.raises(RetainedViewError, match="multiple selected"):
        project_composite_draw_plane(_view(controls))


@pytest.mark.parametrize("control_id,change", [
    (1, {"bounds": ObjectBounds(-1, 10, 30, 2)}),
    (1, {"label": "invalid root label"}),
    (3, {"state": VISIBLE | ControlState.SELECTED | ControlState.MINIMIZED}),
    (3, {"bounds": ObjectBounds(-1, 0, 10, 1)}),
    (3, {"bounds": ObjectBounds(5, 1, 10, 1)}),
    (3, {"bounds": None}), (3, {"z_order": 1}),
    (3, {"label": "bad\x85"}), (3, {"shortcut": "bad\u2028"}),
    (2, {"state": VISIBLE | ControlState.MINIMIZED}),
    (2, {"kind": True}),
])
def test_projection_revalidates_forged_definitions(control_id, change):
    controls = _controls()
    controls[control_id] = _forge(controls[control_id], **change)
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_view(controls))


def _draws():
    return (
        TaskDraw(2, ControlKind.LAUNCHER, VISIBLE, 0, ObjectBounds(0, 0, 4, 1), "Run"),
        TaskDraw(3, ControlKind.TASK, VISIBLE, 1, ObjectBounds(5, 0, 10, 1), "Editor"),
    )


@pytest.mark.parametrize("changes", [
    {"control_id": True}, {"kind": True}, {"kind": ControlKind.TAB},
    {"state": ControlState.ENABLED}, {"state": VISIBLE | ControlState.CHECKED},
    {"state": VISIBLE | ControlState.SELECTED | ControlState.MINIMIZED},
    {"order": -1}, {"bounds": ObjectBounds(0, 1, 4, 1)},
    {"bounds": ObjectBounds(0, 0, 4, 2)}, {"bounds": ObjectBounds(-1, 0, 4, 1)},
    {"label": ""}, {"label": "bad\u2029"}, {"shortcut": "bad\x9f"},
])
def test_task_draw_rejects_invalid_immutable_input(changes):
    with pytest.raises((TypeError, ValueError)):
        replace(_draws()[1], **changes)


def test_taskbar_draw_validates_sorting_overlap_identity_and_selection():
    first, second = _draws()
    invalid = (
        (second, first),
        (first, replace(second, order=first.order)),
        (first, replace(second, control_id=first.control_id)),
        (first, replace(second, control_id=1)),
        (first, replace(second, bounds=ObjectBounds(3, 0, 10, 1))),
        (first, replace(second, bounds=ObjectBounds(25, 0, 10, 1))),
        (replace(first, kind=ControlKind.TASK, state=VISIBLE | ControlState.SELECTED),
         replace(second, state=VISIBLE | ControlState.SELECTED)),
    )
    for tasks in invalid:
        with pytest.raises(ValueError):
            TaskBarDraw(1, VISIBLE, 0, 0, ROOT_BOUNDS, tasks)


def test_empty_and_adjacent_task_slots_are_valid_without_reflow():
    first, second = _draws()
    empty = TaskBarDraw(1, VISIBLE, 0, 0, ROOT_BOUNDS, ())
    assert empty.tasks == ()
    second = replace(second, bounds=ObjectBounds(4, 0, 10, 1))
    bar = replace(empty, tasks=(first, second))
    assert bar.tasks[1].bounds.cell_x == 4
