"""Dialogs, buttons, and modal regions from the scene to the reference
renderer, its hit map, and the shared-viewer transport (RETAINED-1
sections 9 to 9.4)."""

from dataclasses import replace
import json
from types import MappingProxyType

import pytest

pygame = pytest.importorskip("pygame")

from rich_terminal.appearance import FLOWING_APPEARANCE, REFERENCE_APPEARANCE
from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal import pygame_view
from rich_terminal.pygame_view import (
    ControlHitTarget, ControlSurface, TextHitTarget, composite_draw_plane_result,
    resolve_pointer,
)
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    ControlDefinition, ControlKind, ControlState, ObjectBounds, OwnerScene,
    RegionDefinition, RetainedScene, SceneModelState, SceneUsage,
)
from rich_terminal.retained_view import (
    ButtonDraw, DialogDraw, FieldDraw, project_composite_draw_plane,
)
from rich_terminal.semantic_dialogs import ButtonContent, ButtonFlag, DialogContent, DialogFlag
from rich_terminal.semantic_fields import FieldContent, FieldFlag, FieldKind, FieldRect
from rich_terminal.update_authority import TerminalGeometry
from shared_session import retained_draw_plane_from_wire, retained_draw_plane_to_wire

OWNER = OwnerIdentity(7, 3, 1, 2)
GEOMETRY = TerminalGeometry(30, 12, 9)
LIVE = ControlState.VISIBLE | ControlState.ENABLED
NONE = FieldRect(0, 0, 0, 0)
CELL_W, CELL_H = 8, 12


def _field(**changes):
    values = dict(
        owner=OWNER, control_id=11, kind=ControlKind.FIELD, state=LIVE | ControlState.SELECTED,
        z_order=0, region_id=1, parent_control_id=0, order=0,
        bounds=ObjectBounds(1, 10, 20, 1), label="Name", shortcut="",
        content=FieldContent(5, FieldKind.TEXT, FieldFlag(0), FieldRect(0, 0, 5, 1),
                             FieldRect(6, 0, 14, 1), text="notes.md", caret=5, anchor=8),
    )
    values.update(changes)
    return ControlDefinition(**values)


def _dialog(**changes):
    values = dict(
        owner=OWNER, control_id=21, kind=ControlKind.DIALOG, state=ControlState.VISIBLE,
        z_order=0, region_id=2, parent_control_id=0, order=0,
        bounds=ObjectBounds(0, 0, 20, 5), label="Delete", shortcut="",
        content=DialogContent(3, DialogFlag(0), 3, FieldRect(1, 0, 18, 1),
                              FieldRect(1, 1, 18, 2), "Delete notes.md?\nIt cannot be undone."),
    )
    values.update(changes)
    return ControlDefinition(**values)


def _button(control_id=22, *, x=2, label="Delete", flags=ButtonFlag.DESTRUCTIVE, **changes):
    values = dict(
        owner=OWNER, control_id=control_id, kind=ControlKind.BUTTON, state=LIVE,
        z_order=1, region_id=2, parent_control_id=0, order=0,
        bounds=ObjectBounds(x, 3, 8, 1), label=label, shortcut="",
        content=ButtonContent(flags),
    )
    values.update(changes)
    return ControlDefinition(**values)


def _view(controls, *, modal=True, modal_visible=True):
    base = RegionDefinition(OWNER, 1, 0, 0, 30, 12, 0, 0, 30, 12, 0, True, True, 9)
    # The modal region covers rows 2 to 9 and columns 4 to 27.
    overlay = RegionDefinition(OWNER, 2, 4, 2, 24, 8, 0, 0, 0, 0, 5, modal_visible,
                               False, 9, modal=modal)
    scene = OwnerScene(
        OWNER, MappingProxyType({1: base, 2: overlay}), MappingProxyType({}),
        MappingProxyType({}), SceneUsage(regions=2, objects=len(controls)),
        controls=MappingProxyType({control.control_id: control for control in controls}),
    )
    row = (BLANK_CELL,) * 30
    cell = TerminalView(8, 7, 3, 4, 30, 12, (row,) * 12, (), Cursor(0, 0, False))
    retained = SceneModelState(6, GEOMETRY, RetainedScene(MappingProxyType({1: scene})),
                               None, None, None, True, True)
    return CompositeTerminalView(3, 6, GEOMETRY, cell, retained)


def _plane(controls, **kwargs):
    return project_composite_draw_plane(_view(controls, **kwargs))[1]


def _paint(plane, appearance=REFERENCE_APPEARANCE):
    pygame.font.init()
    font = pygame.font.Font(None, 16)
    surface = pygame.Surface((30 * CELL_W, 12 * CELL_H))
    return composite_draw_plane_result(pygame, surface, plane, font, CELL_W, CELL_H,
                                       appearance=appearance)


def _controls():
    return [_field(bounds=ObjectBounds(5, 4, 20, 1)), _dialog(), _button(),
            _button(23, x=11, label="Keep", flags=ButtonFlag.CANCEL | ButtonFlag.DEFAULT)]


def test_projection_carries_dialogs_buttons_and_inactive_controls_beneath():
    plane = _plane(_controls())
    base, overlay = plane.regions
    assert base.inactive_controls == frozenset({11}) and not base.modal
    assert overlay.modal and not overlay.inactive_controls
    dialog, delete, keep = overlay.draws
    assert isinstance(dialog, DialogDraw) and dialog.title == "Delete"
    assert dialog.content.lines == ("Delete notes.md?", "It cannot be undone.")
    assert isinstance(delete, ButtonDraw) and delete.content.destructive
    assert keep.content.default and keep.content.cancel


@pytest.mark.parametrize("changes", [
    dict(modal=False), dict(modal_visible=False),
])
def test_only_a_visible_modal_region_makes_controls_inactive(changes):
    base = _plane(_controls(), **changes).regions[0]
    assert not base.inactive_controls


def test_a_control_outside_the_modal_barrier_stays_active():
    # Row 11 lies below the modal rectangle's rows 2 to 9.
    plane = _plane([_field(bounds=ObjectBounds(1, 11, 20, 1)), _dialog(), _button()])
    assert not plane.regions[0].inactive_controls


@pytest.mark.parametrize("appearance", [REFERENCE_APPEARANCE, FLOWING_APPEARANCE])
def test_buttons_take_points_the_dialog_swallows_the_rest(appearance):
    result = _paint(_plane(_controls()), appearance)
    surface = lambda x, y: resolve_pointer(  # noqa: E731
        result.hit_entries, x, y, cell_width=CELL_W, cell_height=CELL_H)
    # The Delete button: region (4, 2) + button (2, 3) = cells 6..13 of row 5.
    delete = surface(7 * CELL_W, 5 * CELL_H + 3)
    assert isinstance(delete, ControlHitTarget) and delete.kind is ControlKind.BUTTON
    assert delete.identity.control_id == 22
    keep = surface(16 * CELL_W, 5 * CELL_H + 3)
    assert isinstance(keep, ControlHitTarget) and keep.identity.control_id == 23
    # The message is the dialog's surface: a press there goes nowhere.
    assert surface(7 * CELL_W, 3 * CELL_H + 3) is None


def test_an_inactive_field_beneath_the_modal_region_takes_no_input_and_dims():
    active = _paint(_plane(_controls(), modal=False))
    inactive = _paint(_plane(_controls()))
    point = (12 * CELL_W, 4 * CELL_H + 6)
    assert any(isinstance(entry, TextHitTarget) for entry in active.hit_entries)
    assert not any(isinstance(entry, TextHitTarget) for entry in inactive.hit_entries)
    assert any(isinstance(entry, ControlSurface) and entry.control_id == 11
               for entry in inactive.hit_entries)
    # The field spans cells 5 to 24 of row 4 and the dialog cells 4 to 23,
    # so the field's last cell shows beside the dialog, dimmed.
    outside = (24 * CELL_W + 4, 4 * CELL_H + 6)
    assert inactive.surface.get_at(outside) != active.surface.get_at(outside)
    assert resolve_pointer(inactive.hit_entries, *point, cell_width=CELL_W,
                           cell_height=CELL_H) is None


def test_a_disabled_button_swallows_points_without_activating():
    controls = _controls()
    controls[2] = _button(state=ControlState.VISIBLE)
    result = _paint(_plane(controls))
    assert resolve_pointer(result.hit_entries, 7 * CELL_W, 5 * CELL_H + 3,
                           cell_width=CELL_W, cell_height=CELL_H) is None


def test_a_text_field_shows_its_caret_and_selection_only_while_selected():
    field = _field()
    selected = _paint(_plane([field]))
    plain = _paint(_plane([_field(state=LIVE)]))
    # The value slot starts at cell 7 of row 10; "notes.md" selects ".md".
    selection = (7 * CELL_W + 6 * CELL_W + 2, 10 * CELL_H + 2)
    assert selected.surface.get_at(selection) != plain.surface.get_at(selection)


def test_an_empty_text_field_shows_its_placeholder():
    content = FieldContent(5, FieldKind.TEXT, FieldFlag(0), FieldRect(0, 0, 5, 1),
                           FieldRect(6, 0, 14, 1), placeholder="New name")
    empty = _paint(_plane([_field(content=content, state=LIVE)]))
    blank = _paint(_plane([_field(content=replace(content, placeholder=""), state=LIVE)]))
    slot = pygame.Rect(7 * CELL_W, 10 * CELL_H, 8 * CELL_W, CELL_H)
    assert (pygame.image.tobytes(empty.surface.subsurface(slot), "RGBA")
            != pygame.image.tobytes(blank.surface.subsurface(slot), "RGBA"))


class _EightPixelFont:
    """Every character eight pixels wide."""

    def size(self, text):
        return (8 * len(text), 16)


@pytest.mark.parametrize("text, left, right, center, expected", [
    # Text that fits starts at its start edge: the left for left to right,
    # the right for right to left.
    ("Delete", 10, 100, False, 10),
    ("\u05e9\u05dc\u05d5\u05dd", 10, 100, False, 100 - 32),
    # Text too wide is clipped at its end side, so its start stays in view.
    ("Delete notes.md", 10, 50, False, 10),
    ("\u05e9\u05dc\u05d5\u05dd \u05e2\u05d5\u05dc\u05dd", 10, 50, False, 50 - 72),
    # A centred label is centred while it fits, and else keeps its start.
    ("Keep", 10, 100, True, 10 + (90 - 32) // 2),
    ("\u05e9\u05dc\u05d5\u05dd \u05e2\u05d5\u05dc\u05dd", 10, 50, True, 50 - 72),
])
def test_a_laid_out_label_starts_at_its_start_edge_and_clips_its_end(
    text, left, right, center, expected,
):
    assert pygame_view._label_left(_EightPixelFont(), text, left, right, 32,
                                   center=center) == expected


def _painted_text(monkeypatch, controls):
    """The text, left, and right of every bounded text paint."""

    calls = []
    paint = pygame_view._paint_bounded_text

    def record(pygame_module, surface, font, text, color, visible_rect, **kwargs):
        calls.append((text, kwargs["left"], kwargs["right"], font))
        return paint(pygame_module, surface, font, text, color, visible_rect, **kwargs)

    monkeypatch.setattr(pygame_view, "_paint_bounded_text", record)
    _paint(_plane(controls))
    return calls


def _width(font, text):
    advance = max(1, CELL_W * 4)
    return sum(pygame_view._scalar_advance(font, character, advance)
               for character in pygame_view._label_characters(text))


def test_dialog_lines_start_at_their_start_edge(monkeypatch):
    hebrew = "\u05dc\u05de\u05d7\u05d5\u05e7?"
    dialog = _dialog(content=DialogContent(
        3, DialogFlag(0), 3, FieldRect(1, 0, 18, 1), FieldRect(1, 1, 18, 2),
        hebrew + "\nIt cannot be undone."))
    calls = {text: (left, right, font)
             for text, left, right, font in _painted_text(monkeypatch, [dialog])}
    left, right, font = calls[hebrew]
    assert right - left == _width(font, hebrew) < 18 * CELL_W
    left, right, _font = calls["It cannot be undone."]
    assert right - left == 18 * CELL_W


def test_a_button_label_too_wide_keeps_its_start(monkeypatch):
    hebrew = " ".join(["\u05de\u05d7\u05d9\u05e7\u05d4 \u05e1\u05d5\u05e4\u05d9\u05ea"] * 3)
    calls = {text: (left, right, font) for text, left, right, font
             in _painted_text(monkeypatch, [_dialog(), _button(label=hebrew)])}
    left, right, font = calls[hebrew]
    assert right - left == _width(font, hebrew) > 8 * CELL_W


def test_the_shared_viewer_transport_carries_dialogs_buttons_and_modality():
    plane = _plane(_controls())
    wire = retained_draw_plane_to_wire(plane)
    base, overlay = wire["regions"]
    assert (base["modal"], base["inactive_controls"]) == (False, [11])
    assert (overlay["modal"], overlay["inactive_controls"]) == (True, [])
    assert [draw["kind"] for draw in overlay["draws"]] == ["dialog", "button", "button"]
    assert retained_draw_plane_from_wire(json.loads(json.dumps(wire))) == plane
    broken = json.loads(json.dumps(wire))
    broken["regions"][0]["inactive_controls"] = [11, 11]
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(broken)
    broken["regions"][0]["inactive_controls"] = [99]
    with pytest.raises(ValueError):
        retained_draw_plane_from_wire(broken)
