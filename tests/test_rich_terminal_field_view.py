"""Typed field projection preserves committed values, slot geometry and identity."""

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
    FieldDraw, RetainedViewError, project_composite_draw_plane,
    retained_draw_control_ids, retained_draw_key, retained_draw_order,
)
from rich_terminal.semantic_fields import (
    FieldChoice, FieldContent, FieldFlag, FieldKind, FieldRect,
)
from rich_terminal.update_authority import TerminalGeometry

OWNER = OwnerIdentity(7, 3, 1, 2)
GEOMETRY = TerminalGeometry(30, 12, 9)
VISIBLE = ControlState.VISIBLE | ControlState.ENABLED
BOUNDS = ObjectBounds(-1, 2, 16, 2)
LABEL = FieldRect(0, 0, 8, 1)
VALUE = FieldRect(9, 0, 7, 2)


def _content(kind=FieldKind.INTEGER, *, read_only=False, revision=13):
    flags = FieldFlag.READ_ONLY if read_only else FieldFlag(0)
    if kind is FieldKind.INTEGER:
        return FieldContent(revision, kind, flags, LABEL, VALUE,
                            value=-20, minimum=-100, maximum=200, step=3)
    if kind is FieldKind.CHOICE:
        return FieldContent(revision, kind, flags, LABEL, VALUE, value=100,
                            choices=(FieldChoice(-5, "Sine"), FieldChoice(100, "茶 — Saw")))
    return FieldContent(revision, kind, flags, LABEL, VALUE, text="αβ / Café")


def _control(content=None, **changes):
    values = dict(
        owner=OWNER, control_id=11, kind=ControlKind.FIELD,
        state=VISIBLE | ControlState.SELECTED, z_order=-2,
        region_id=1, parent_control_id=0, order=0, bounds=BOUNDS,
        label="Amplitude", shortcut="", content=_content() if content is None else content,
    )
    values.update(changes)
    return ControlDefinition(**values)


def _view(controls=None, *, region_visible=True, retained_visible=True):
    controls = {11: _control()} if controls is None else controls
    region = RegionDefinition(OWNER, 1, 0, 0, 30, 12, 0, 0, 30, 12,
                              0, region_visible, True, 9)
    scene = OwnerScene(
        OWNER, MappingProxyType({1: region}), MappingProxyType({}), MappingProxyType({}),
        SceneUsage(regions=1, objects=sum(
            control.content.object_slots if isinstance(control.content, FieldContent) else 1
            for control in controls.values()
        )), controls=MappingProxyType(controls),
    )
    row = (BLANK_CELL,) * 30
    cell = TerminalView(8, 7, 3, 4, 30, 12, (row,) * 12, (), Cursor(0, 0, False))
    retained = SceneModelState(6, GEOMETRY, RetainedScene(MappingProxyType({1: scene})),
                               None, None, None, retained_visible, True)
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def _forge(control, **changes):
    result = copy(control)
    for name, value in changes.items():
        object.__setattr__(result, name, value)
    return result


@pytest.mark.parametrize("kind", list(FieldKind))
@pytest.mark.parametrize("read_only", [False, True])
def test_projection_preserves_exact_field_type_value_revision_and_slots(kind, read_only):
    content = _content(kind, read_only=read_only)
    scope, plane = project_composite_draw_plane(_view({11: _control(content)}))
    draw, = plane.regions[0].draws
    assert draw == FieldDraw(11, VISIBLE | ControlState.SELECTED, 0, -2,
                             BOUNDS, "Amplitude", content)
    assert scope.model_revision == 6
    assert draw.content.content_revision == 13
    assert draw.content.read_only is read_only
    assert draw.content.kind is kind
    assert draw.content.label_bounds == LABEL
    assert draw.content.value_bounds == VALUE
    assert retained_draw_key(draw) == ("control", 11)
    assert retained_draw_control_ids(draw) == frozenset({11})
    if kind is FieldKind.CHOICE:
        assert [choice.value for choice in draw.content.choices] == [-5, 100]
        assert draw.content.choices[1].label == "茶 — Saw"
    with pytest.raises(FrozenInstanceError):
        draw.label = "Changed"
    with pytest.raises(FrozenInstanceError):
        draw.content.content_revision = 99


def test_field_replacement_keeps_prior_committed_content_immutable():
    old_content = _content()
    _, old = project_composite_draw_plane(_view({11: _control(old_content)}))
    new_content = replace(old_content, content_revision=14, value=100)
    _, new = project_composite_draw_plane(_view({11: _control(new_content)}))
    prior, current = old.regions[0].draws[0], new.regions[0].draws[0]
    assert (prior.content.content_revision, prior.content.value) == (13, -20)
    assert (current.content.content_revision, current.content.value) == (14, 100)
    assert current.bounds == prior.bounds
    assert current.content.label_bounds == prior.content.label_bounds
    assert current.content.value_bounds == prior.content.value_bounds


@pytest.mark.parametrize("hidden", ["field", "region", "retained"])
def test_hidden_field_or_plane_has_no_draw(hidden):
    controls = {11: _control(state=ControlState.ENABLED)} if hidden == "field" else None
    _, plane = project_composite_draw_plane(_view(
        controls, region_visible=hidden != "region", retained_visible=hidden != "retained",
    ))
    assert not any(region.draws for region in plane.regions)


def test_empty_label_requires_and_preserves_canonical_empty_label_slot():
    content = replace(_content(), label_bounds=FieldRect(0, 0, 0, 0))
    control = _control(content, label="")
    _, plane = project_composite_draw_plane(_view({11: control}))
    draw = plane.regions[0].draws[0]
    assert draw.label == ""
    assert draw.content.label_bounds.empty
    assert draw.content.value_bounds == VALUE


@pytest.mark.parametrize("change", [
    {"control_id": True}, {"kind": True},
    {"state": VISIBLE | ControlState.MINIMIZED},
    {"state": ControlState.SELECTED},
    {"order": 1}, {"parent_control_id": 7},
    {"bounds": None}, {"bounds": ObjectBounds(-1, 2, 15, 2)},
    {"label": "bad\x85"}, {"label": "bad\u2028"}, {"label": "bad\ud800"},
    {"shortcut": "F2"}, {"content": None}, {"content": "FDC1"},
])
def test_projection_revalidates_forged_control_definitions(change):
    controls = {11: _forge(_control(), **change)}
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_view(controls))


@pytest.mark.parametrize("change", [
    {"label_bounds": FieldRect(0, 0, 0, 0)},
    {"label_bounds": FieldRect(8, 0, 8, 1)},
    {"value_bounds": FieldRect(-1, 0, 7, 1)},
    {"value_bounds": FieldRect(9, 1, 7, 2)},
    {"value_bounds": FieldRect(10, 0, 7, 1)},
])
def test_projection_rejects_forged_definition_with_misaligned_content_slots(change):
    content = replace(_content(), **change)
    control = _forge(_control(), content=content)
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_view({11: control}))


def test_hidden_field_is_still_validated_for_bad_geometry():
    control = _forge(_control(state=ControlState.ENABLED),
                     bounds=ObjectBounds(-1, 2, 3, 1))
    with pytest.raises(RetainedViewError):
        project_composite_draw_plane(_view({11: control}, region_visible=False))


def test_field_cannot_host_control_children():
    field = _control()
    child = ControlDefinition(
        OWNER, 12, ControlKind.TAB, VISIBLE, 0, 1, field.control_id, 0,
        None, "Unexpected", "",
    )
    with pytest.raises(RetainedViewError, match="parent"):
        project_composite_draw_plane(_view({11: field, 12: child}))


@pytest.mark.parametrize("change", [
    {"control_id": True}, {"control_id": 0}, {"control_id": 1 << 64},
    {"state": True}, {"state": ControlState.ENABLED},
    {"state": VISIBLE | ControlState.OPEN}, {"order": 1},
    {"z_order": True}, {"z_order": 1 << 31},
    {"label": ""}, {"label": "bad\x00"}, {"label": "bad\x7f"},
    {"label": "bad\x85"}, {"label": "bad\u2029"},
    {"bounds": ObjectBounds(0, 0, 16, 1)}, {"content": None},
])
def test_field_draw_rejects_invalid_direct_inputs(change):
    draw = FieldDraw(11, VISIBLE, 0, -2, BOUNDS, "Amplitude", _content())
    with pytest.raises((TypeError, ValueError)):
        replace(draw, **change)


def test_field_order_uses_z_then_root_control_id():
    draw = FieldDraw(11, VISIBLE, 0, -2, BOUNDS, "Amplitude", _content())
    earlier = replace(draw, control_id=10)
    later = replace(draw, control_id=9, z_order=0)
    assert retained_draw_order((later, draw, earlier)) == (earlier, draw, later)
