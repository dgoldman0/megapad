"""Focused projection tests for renderer-neutral semantic menu draws."""

from __future__ import annotations

from dataclasses import FrozenInstanceError
from types import MappingProxyType

import pytest

from rich_terminal.apt1 import UINT32_MAX
from rich_terminal.cell_model import BLANK_CELL, Cursor, TerminalView
from rich_terminal.output_coordinator import CompositeTerminalView
from rich_terminal.retained_model import OwnerIdentity
from rich_terminal.retained_scene import (
    ControlDefinition,
    ControlKind,
    ControlState,
    GlyphRunBody,
    HiddenTargetKind,
    ObjectBounds,
    ObjectDefinition,
    OwnerScene,
    RegionDefinition,
    RetainedScene,
    RGBA,
    SceneModelState,
    SceneUsage,
)
from rich_terminal.retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    RetainedViewError,
    project_composite_draw_plane,
)
from rich_terminal.update_authority import TerminalGeometry


SESSION_ID = 0x1020304050607080
PRESENTATION_EPOCH = 3
GEOMETRY = TerminalGeometry(24, 12, 7)
FULL_BOUNDS = ObjectBounds(0, 0, UINT32_MAX, 0x18000000)
WHITE = RGBA(255, 255, 255, 255)
BLUE = RGBA(10, 40, 120, 255)


def _owner(owner_id: int, generation: int = 1) -> OwnerIdentity:
    return OwnerIdentity(
        SESSION_ID,
        PRESENTATION_EPOCH,
        owner_id,
        generation,
    )


def _region(
    owner: OwnerIdentity,
    region_id: int = 1,
    *,
    visible: bool = True,
) -> RegionDefinition:
    return RegionDefinition(
        owner,
        region_id,
        0,
        0,
        GEOMETRY.cols,
        GEOMETRY.rows,
        0,
        visible,
        True,
        GEOMETRY.generation,
    )


def _glyph(
    owner: OwnerIdentity,
    object_id: int,
    *,
    z_order: int,
    text: str,
) -> ObjectDefinition:
    return ObjectDefinition(
        owner,
        object_id,
        1,
        0,
        FULL_BOUNDS,
        z_order,
        True,
        GlyphRunBody(WHITE, BLUE, 0, text),
    )


def _control(
    owner: OwnerIdentity,
    control_id: int,
    kind: ControlKind,
    *,
    region_id: int = 1,
    parent: int = 0,
    order: int = 0,
    state: ControlState | None = None,
    z_order: int = 10,
    label: str = "",
    shortcut: str = "",
) -> ControlDefinition:
    if state is None:
        state = (
            ControlState.VISIBLE
            if kind is ControlKind.MENU_SEPARATOR
            else ControlState.VISIBLE | ControlState.ENABLED
        )
    return ControlDefinition(
        owner=owner,
        control_id=control_id,
        kind=kind,
        state=state,
        z_order=z_order if kind is ControlKind.MENU_BAR else 0,
        region_id=region_id,
        parent_control_id=parent,
        order=order,
        bounds=FULL_BOUNDS if kind is ControlKind.MENU_BAR else None,
        label=label,
        shortcut=shortcut,
    )


def _owner_scene(
    owner: OwnerIdentity,
    regions,
    *,
    objects=(),
    controls=(),
    control_map=None,
) -> OwnerScene:
    object_map = {definition.object_id: definition for definition in objects}
    if control_map is None:
        control_map = {
            definition.control_id: definition for definition in controls
        }
    return OwnerScene(
        owner=owner,
        regions=MappingProxyType(
            {definition.region_id: definition for definition in regions}
        ),
        objects=MappingProxyType(object_map),
        series=MappingProxyType({}),
        usage=SceneUsage(
            regions=len(regions),
            objects=len(object_map) + len(control_map),
        ),
        controls=MappingProxyType(dict(control_map)),
    )


def _cell_view() -> TerminalView:
    row = (BLANK_CELL,) * GEOMETRY.cols
    return TerminalView(
        attachment_epoch=5,
        session_id=SESSION_ID,
        presentation_epoch=PRESENTATION_EPOCH,
        revision=8,
        cols=GEOMETRY.cols,
        rows=GEOMETRY.rows,
        cells=(row,) * GEOMETRY.rows,
        dirty_spans=(),
        cursor=Cursor(0, 0, True),
    )


def _composite(
    owner_scenes,
    *,
    hidden: RetainedScene | None = None,
) -> CompositeTerminalView:
    state = SceneModelState(
        revision=9,
        geometry=GEOMETRY,
        active=RetainedScene(
            MappingProxyType(
                {
                    owner_scene.owner.owner_id: owner_scene
                    for owner_scene in owner_scenes
                }
            )
        ),
        hidden=hidden,
        hidden_kind=None if hidden is None else HiddenTargetKind.REPLACE,
        requirement=None,
        retained_visible=True,
        retained_initialized=True,
    )
    return CompositeTerminalView(
        presentation_epoch=PRESENTATION_EPOCH,
        revision=9,
        geometry=GEOMETRY,
        cell=_cell_view(),
        retained=state,
    )


def test_projection_preserves_semantics_and_orders_draw_families_deterministically():
    owner = _owner(7, 2)
    open_selected = (
        ControlState.VISIBLE
        | ControlState.ENABLED
        | ControlState.OPEN
        | ControlState.SELECTED
    )
    selected_checked = (
        ControlState.VISIBLE
        | ControlState.ENABLED
        | ControlState.SELECTED
        | ControlState.CHECKED
    )
    controls = (
        _control(owner, 40, ControlKind.MENU_BAR, z_order=5),
        _control(owner, 41, ControlKind.MENU_BAR, z_order=-3),
        _control(
            owner,
            43,
            ControlKind.MENU,
            parent=40,
            order=1,
            label="Edit",
        ),
        _control(
            owner,
            42,
            ControlKind.MENU,
            parent=40,
            state=open_selected,
            label="File",
        ),
        _control(
            owner,
            46,
            ControlKind.MENU_ITEM,
            parent=42,
            order=2,
            state=selected_checked,
            label="Save",
            shortcut="Ctrl+S",
        ),
        _control(
            owner,
            44,
            ControlKind.MENU_SEPARATOR,
            parent=42,
        ),
        _control(
            owner,
            45,
            ControlKind.MENU_ITEM,
            parent=42,
            order=1,
            label="Open",
            shortcut="Ctrl+O",
        ),
    )
    scene = _owner_scene(
        owner,
        (_region(owner),),
        objects=(
            _glyph(owner, 40, z_order=5, text="same namespace number"),
            _glyph(owner, 41, z_order=-3, text="behind root"),
            _glyph(owner, 8, z_order=6, text="front"),
        ),
        controls=controls,
    )

    _, plane = project_composite_draw_plane(_composite((scene,)))

    draws = plane.regions[0].draws
    assert [type(draw) for draw in draws] == [
        GlyphRunDraw,
        MenuBarDraw,
        GlyphRunDraw,
        MenuBarDraw,
        GlyphRunDraw,
    ]
    assert [
        draw.object_id if isinstance(draw, GlyphRunDraw) else draw.control_id
        for draw in draws
    ] == [41, 41, 40, 40, 8]

    menu_bar = draws[3]
    assert isinstance(menu_bar, MenuBarDraw)
    assert (
        menu_bar.control_id,
        menu_bar.state,
        menu_bar.order,
        menu_bar.z_order,
        menu_bar.bounds,
    ) == (40, ControlState.VISIBLE | ControlState.ENABLED, 0, 5, FULL_BOUNDS)
    assert [menu.control_id for menu in menu_bar.menus] == [42, 43]
    file_menu, edit_menu = menu_bar.menus
    assert (file_menu.state, file_menu.order, file_menu.label) == (
        open_selected,
        0,
        "File",
    )
    assert [type(entry) for entry in file_menu.entries] == [
        MenuSeparatorDraw,
        MenuItemDraw,
        MenuItemDraw,
    ]
    assert [entry.control_id for entry in file_menu.entries] == [44, 45, 46]
    assert (
        file_menu.entries[1].label,
        file_menu.entries[1].shortcut,
        file_menu.entries[2].state,
    ) == ("Open", "Ctrl+O", selected_checked)
    assert (edit_menu.control_id, edit_menu.label, edit_menu.entries) == (
        43,
        "Edit",
        (),
    )
    with pytest.raises(FrozenInstanceError):
        file_menu.label = "forged"


def _menu_item_draw(
    control_id: int,
    *,
    order: int,
    selected: bool = False,
) -> MenuItemDraw:
    state = ControlState.VISIBLE | ControlState.ENABLED
    if selected:
        state |= ControlState.SELECTED
    return MenuItemDraw(control_id, state, order, f"Item {control_id}", "")


def _menu_draw(
    control_id: int,
    *,
    order: int,
    state: ControlState | None = None,
    entries=(),
) -> MenuDraw:
    if state is None:
        state = ControlState.VISIBLE | ControlState.ENABLED
    return MenuDraw(control_id, state, order, f"Menu {control_id}", tuple(entries))


def _menu_bar_draw(control_id: int, *, z_order: int = 0, menus=()) -> MenuBarDraw:
    return MenuBarDraw(
        control_id,
        ControlState.VISIBLE | ControlState.ENABLED,
        0,
        z_order,
        FULL_BOUNDS,
        tuple(menus),
    )


def test_semantic_draw_dtos_reject_multiple_selected_items():
    open_state = ControlState.VISIBLE | ControlState.ENABLED | ControlState.OPEN

    with pytest.raises(ValueError, match="multiple selected items"):
        _menu_draw(
            10,
            order=0,
            state=open_state,
            entries=(
                _menu_item_draw(11, order=0, selected=True),
                _menu_item_draw(12, order=1, selected=True),
            ),
        )


@pytest.mark.parametrize(
    ("exclusive_state", "message"),
    (
        (ControlState.OPEN, "multiple open menus"),
        (ControlState.SELECTED, "multiple selected menus"),
    ),
)
def test_semantic_draw_dtos_reject_ambiguous_menu_state(
    exclusive_state: ControlState,
    message: str,
):
    menu_state = ControlState.VISIBLE | ControlState.ENABLED | exclusive_state

    with pytest.raises(ValueError, match=message):
        _menu_bar_draw(
            1,
            menus=(
                _menu_draw(2, order=0, state=menu_state),
                _menu_draw(3, order=1, state=menu_state),
            ),
        )


def test_semantic_draw_dtos_reject_duplicate_ids_within_a_menu_tree():
    open_state = ControlState.VISIBLE | ControlState.ENABLED | ControlState.OPEN

    with pytest.raises(ValueError, match="control IDs are duplicated"):
        _menu_bar_draw(
            1,
            menus=(
                _menu_draw(
                    2,
                    order=0,
                    state=open_state,
                    entries=(_menu_item_draw(1, order=0),),
                ),
            ),
        )

    with pytest.raises(ValueError, match="control IDs are duplicated"):
        _menu_bar_draw(
            1,
            menus=(
                _menu_draw(
                    2,
                    order=0,
                    state=open_state,
                    entries=(
                        _menu_item_draw(3, order=0),
                        _menu_item_draw(3, order=1),
                    ),
                ),
            ),
        )


def test_semantic_draw_plane_rejects_duplicate_ids_across_owner_regions():
    first = RetainedRegionDraw(7, 2, 1, 0, 0, 1, 1, 0, False, (_menu_bar_draw(1),))
    second = RetainedRegionDraw(7, 2, 2, 1, 0, 1, 1, 1, False, (_menu_bar_draw(1),))

    with pytest.raises(ValueError, match="owner semantic control IDs are duplicated"):
        RetainedDrawPlane(True, True, (first, second))


def test_projection_applies_closed_open_and_visible_control_cascades():
    owner = _owner(7)
    open_state = ControlState.VISIBLE | ControlState.ENABLED | ControlState.OPEN
    controls = (
        _control(owner, 1, ControlKind.MENU_BAR),
        _control(owner, 2, ControlKind.MENU, parent=1, label="Closed"),
        _control(
            owner,
            3,
            ControlKind.MENU_ITEM,
            parent=2,
            label="Not effective",
        ),
        _control(
            owner,
            4,
            ControlKind.MENU,
            parent=1,
            order=1,
            state=open_state,
            label="Open",
        ),
        _control(
            owner,
            5,
            ControlKind.MENU_ITEM,
            parent=4,
            label="Visible item",
        ),
        _control(
            owner,
            6,
            ControlKind.MENU_ITEM,
            parent=4,
            order=1,
            state=ControlState.ENABLED,
            label="Invisible item",
        ),
        _control(
            owner,
            7,
            ControlKind.MENU_SEPARATOR,
            parent=4,
            order=2,
        ),
        _control(
            owner,
            8,
            ControlKind.MENU,
            parent=1,
            order=2,
            state=ControlState.ENABLED,
            label="Invisible menu",
        ),
        _control(
            owner,
            9,
            ControlKind.MENU_ITEM,
            parent=8,
            label="Invisible cascade",
        ),
        _control(
            owner,
            10,
            ControlKind.MENU_BAR,
            state=ControlState.ENABLED,
            z_order=20,
        ),
        _control(
            owner,
            11,
            ControlKind.MENU,
            parent=10,
            state=open_state,
            label="Hidden root child",
        ),
        _control(
            owner,
            12,
            ControlKind.MENU_ITEM,
            parent=11,
            label="Hidden root entry",
        ),
    )
    scene = _owner_scene(owner, (_region(owner),), controls=controls)

    _, plane = project_composite_draw_plane(_composite((scene,)))

    assert len(plane.regions[0].draws) == 1
    root = plane.regions[0].draws[0]
    assert isinstance(root, MenuBarDraw)
    assert [menu.control_id for menu in root.menus] == [2, 4]
    assert root.menus[0].entries == ()
    assert [entry.control_id for entry in root.menus[1].entries] == [5, 7]


def test_projection_never_traverses_hidden_semantic_target():
    owner = _owner(7)
    active = _owner_scene(
        owner,
        (_region(owner),),
        controls=(_control(owner, 1, ControlKind.MENU_BAR),),
    )
    hidden_owner = _owner(99)
    hidden = RetainedScene(
        MappingProxyType(
            {
                hidden_owner.owner_id: _owner_scene(
                    hidden_owner,
                    (_region(hidden_owner),),
                    control_map={1: object()},
                )
            }
        )
    )

    _, plane = project_composite_draw_plane(_composite((active,), hidden=hidden))

    root = plane.regions[0].draws[0]
    assert isinstance(root, MenuBarDraw)
    assert root.control_id == 1


@pytest.mark.parametrize(
    ("case", "message"),
    (
        ("map_key", "map key"),
        ("owner", "owner identity"),
        ("region", "missing region"),
        ("missing_parent", "parent"),
        ("wrong_parent", "parent"),
        ("cross_region", "crosses region"),
    ),
)
def test_projection_fails_closed_on_forged_control_maps(case: str, message: str):
    owner = _owner(7)
    regions = (_region(owner),)
    if case == "map_key":
        controls = {99: _control(owner, 1, ControlKind.MENU_BAR)}
    elif case == "owner":
        controls = {1: _control(_owner(8), 1, ControlKind.MENU_BAR)}
    elif case == "region":
        controls = {
            1: _control(owner, 1, ControlKind.MENU_BAR, region_id=2),
        }
    elif case == "missing_parent":
        controls = {
            2: _control(
                owner,
                2,
                ControlKind.MENU,
                parent=99,
                label="Orphan",
            ),
        }
    elif case == "wrong_parent":
        controls = {
            1: _control(owner, 1, ControlKind.MENU_BAR),
            2: _control(
                owner,
                2,
                ControlKind.MENU_ITEM,
                parent=1,
                label="Wrong level",
            ),
        }
    else:
        regions = (_region(owner), _region(owner, 2))
        controls = {
            1: _control(owner, 1, ControlKind.MENU_BAR),
            2: _control(
                owner,
                2,
                ControlKind.MENU,
                region_id=2,
                parent=1,
                label="Crossed",
            ),
        }
    scene = _owner_scene(
        owner,
        regions,
        control_map=controls,
    )

    with pytest.raises(RetainedViewError, match=message):
        project_composite_draw_plane(_composite((scene,)))
