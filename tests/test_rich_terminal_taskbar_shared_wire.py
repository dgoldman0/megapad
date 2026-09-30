"""Taskbar full/delta JSON retains each child's explicit cell rectangle."""

import json
from dataclasses import replace

import pytest

from rich_terminal.retained_scene import ControlKind, ControlState, ObjectBounds
from rich_terminal.retained_view import (
    DisplayScope, RetainedDrawPlane, RetainedRegionDraw, TaskBarDraw, TaskDraw,
)
from shared.session import TerminalCell, TerminalDisplayOffer, TerminalSnapshot
from shared_session import (
    display_offer_from_wire, display_offer_to_wire,
    retained_draw_plane_from_wire, retained_draw_plane_to_wire,
)

VISIBLE = ControlState.VISIBLE | ControlState.ENABLED


def _plane():
    tasks = (
        TaskDraw(2, ControlKind.LAUNCHER, VISIBLE, 0, ObjectBounds(0, 0, 4, 1), "Run", "F2"),
        TaskDraw(3, ControlKind.TASK, VISIBLE | ControlState.SELECTED, 1,
                 ObjectBounds(5, 0, 7, 1), "茶 Editor"),
        TaskDraw(4, ControlKind.TASK, VISIBLE | ControlState.MINIMIZED, 2,
                 ObjectBounds(13, 0, 7, 1), "Sound"),
    )
    bar = TaskBarDraw(1, VISIBLE, 0, 4, ObjectBounds(-1, 10, 22, 1), tasks)
    region = RetainedRegionDraw(1, 2, 1, 0, 0, 24, 12, 0, 0, 24, 12, 0, True, (bar,))
    return RetainedDrawPlane(True, True, (region,))


def _offer(offer_id, plane):
    cell = TerminalCell(" ", (255, 255, 255), (0, 0, 0), 0)
    return TerminalDisplayOffer(
        offer_id, DisplayScope(1, 2, 3, offer_id, 0, offer_id, offer_id),
        TerminalSnapshot(24, 12, ((cell,) * 24,) * 12, 0, 0, False, False), plane,
    )


def _bar_wire():
    wire = retained_draw_plane_to_wire(_plane())
    return wire, wire["regions"][0]["draws"][0]


def test_full_wire_has_exact_root_and_task_fields():
    plane = _plane()
    wire, bar = _bar_wire()
    assert bar["kind"] == "taskbar"
    assert bar["bounds"] == [-1, 10, 22, 1]
    assert set(bar) == {"kind", "control_id", "state", "order", "z_order", "bounds", "tasks"}
    assert bar["tasks"][0] == {
        "kind": "launcher", "control_id": 2, "state": 3, "order": 0,
        "bounds": [0, 0, 4, 1], "label": "Run", "shortcut": "F2",
    }
    assert bar["tasks"][1]["kind"] == "task"
    assert bar["tasks"][2]["state"] == int(VISIBLE | ControlState.MINIMIZED)
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane


@pytest.mark.parametrize("field,value", [
    ("control_id", True), ("control_id", 0), ("control_id", 1 << 64),
    ("state", True), ("state", int(VISIBLE | ControlState.MINIMIZED)),
    ("order", 1), ("z_order", 1 << 31),
    ("bounds", [-1, 10, 22, 2]), ("bounds", [True, 10, 22, 1]),
    ("bounds", [-(1 << 31)-1, 10, 22, 1]), ("tasks", "tasks"),
])
def test_wire_rejects_invalid_root_fields(field, value):
    wire, bar = _bar_wire()
    bar[field] = value
    with pytest.raises((TypeError, ValueError)):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("field,value", [
    ("kind", "tab"), ("kind", True), ("control_id", True), ("control_id", 0),
    ("state", True), ("state", int(ControlState.ENABLED)),
    ("state", int(VISIBLE | ControlState.SELECTED | ControlState.MINIMIZED)),
    ("order", -1), ("order", 0), ("bounds", [5, 1, 7, 1]),
    ("bounds", [-1, 0, 7, 1]), ("bounds", [5, 0, 7, 2]),
    ("bounds", [3, 0, 7, 1]), ("bounds", [20, 0, 7, 1]),
    ("bounds", [5, 0, False, 1]), ("label", ""), ("label", 7),
    ("label", "bad\x85"), ("label", "bad\u2028"),
    ("shortcut", "bad\u2029"), ("shortcut", "bad\ud800"),
])
def test_wire_rejects_invalid_child_fields(field, value):
    wire, bar = _bar_wire()
    bar["tasks"][1][field] = value
    with pytest.raises((TypeError, ValueError)):
        retained_draw_plane_from_wire(wire)


@pytest.mark.parametrize("field", ["bounds", "label", "shortcut", "kind", "order"])
def test_wire_requires_all_child_properties(field):
    wire, bar = _bar_wire()
    del bar["tasks"][1][field]
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)


def test_wire_rejects_unknown_properties_duplicate_ids_and_multiple_selected_tasks():
    wire, bar = _bar_wire()
    bar["tasks"][1]["autofit"] = True
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(wire)
    for control_id in (1, 2):
        wire, bar = _bar_wire()
        bar["tasks"][1]["control_id"] = control_id
        with pytest.raises(ValueError, match="duplicated"):
            retained_draw_plane_from_wire(wire)
    wire, bar = _bar_wire()
    bar["tasks"][2]["state"] = int(VISIBLE | ControlState.SELECTED)
    with pytest.raises(ValueError, match="multiple selected"):
        retained_draw_plane_from_wire(wire)


def test_delta_updates_selected_and_minimized_state_with_stable_geometry():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    bar = region.draws[0]
    first, editor, sound = bar.tasks
    tasks = (first, replace(editor, state=VISIBLE | ControlState.MINIMIZED),
             replace(sound, state=VISIBLE | ControlState.SELECTED))
    updated = replace(bar, tasks=tasks)
    offer = _offer(2, replace(base.retained, regions=(replace(region, draws=(updated,)),)))
    wire = display_offer_to_wire(offer, base=base)
    assert wire["retained"]["regions"][0]["changed"][0]["kind"] == "taskbar"
    assert display_offer_from_wire(json.loads(json.dumps(wire)), base) == offer
    assert [task.bounds for task in updated.tasks] == [task.bounds for task in bar.tasks]
    assert bar.tasks[1].state & ControlState.SELECTED


def test_delta_removing_middle_task_preserves_other_slots_and_removing_root_uses_control_key():
    base = _offer(1, _plane())
    region = base.retained.regions[0]
    bar = region.draws[0]
    fewer = replace(bar, tasks=(bar.tasks[0], bar.tasks[2]))
    offer = _offer(2, replace(base.retained, regions=(replace(region, draws=(fewer,)),)))
    assert display_offer_from_wire(display_offer_to_wire(offer, base=base), base) == offer
    dropped = _offer(3, replace(base.retained, regions=(replace(region, draws=()),)))
    wire = display_offer_to_wire(dropped, base=base)
    assert wire["retained"]["regions"][0]["removed"] == [["control", 1]]
    assert display_offer_from_wire(wire, base) == dropped
