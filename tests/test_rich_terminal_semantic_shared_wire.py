"""Focused shared-viewer wire checks for semantic retained controls."""

from __future__ import annotations

from copy import deepcopy

import pytest

from rich_terminal.retained_scene import ControlState, ObjectBounds, RGBA
from rich_terminal.retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
)
from shared_session import retained_draw_plane_from_wire, retained_draw_plane_to_wire


VISIBLE = ControlState.VISIBLE
ENABLED = ControlState.ENABLED
FULL_BOUNDS = ObjectBounds(0, 0, 0xFFFFFFFF, 0xFFFFFFFF)


def _glyph(*, object_id: int = 11, z_order: int = 4) -> GlyphRunDraw:
    return GlyphRunDraw(
        object_id=object_id,
        z_order=z_order,
        bounds=FULL_BOUNDS,
        foreground=RGBA(230, 235, 244, 255),
        background=RGBA(17, 20, 28, 255),
        attributes=0,
        text="Desk",
    )


def _menu_bar(*, z_order: int = 4) -> MenuBarDraw:
    return MenuBarDraw(
        control_id=20,
        state=VISIBLE | ENABLED,
        order=0,
        z_order=z_order,
        bounds=ObjectBounds(0, 0, 0xFFFFFFFF, 0x0FFFFFFF),
        menus=(
            MenuDraw(
                control_id=21,
                state=VISIBLE | ENABLED | ControlState.OPEN | ControlState.SELECTED,
                order=0,
                label="File",
                entries=(
                    MenuItemDraw(
                        control_id=22,
                        state=(
                            VISIBLE
                            | ENABLED
                            | ControlState.SELECTED
                            | ControlState.CHECKED
                        ),
                        order=0,
                        label="Save…",
                        shortcut="Ctrl+S",
                    ),
                    MenuSeparatorDraw(
                        control_id=23,
                        state=VISIBLE,
                        order=1,
                    ),
                    MenuItemDraw(
                        control_id=24,
                        state=VISIBLE,
                        order=2,
                        label="Close",
                        shortcut="",
                    ),
                ),
            ),
            MenuDraw(
                control_id=25,
                state=VISIBLE | ENABLED,
                order=1,
                label="Edit",
                entries=(),
            ),
        ),
    )


def _plane() -> RetainedDrawPlane:
    return RetainedDrawPlane(
        retained_initialized=True,
        retained_visible=True,
        regions=(
            RetainedRegionDraw(
                owner_id=1,
                owner_generation=2,
                region_id=3,
                cell_x=0,
                cell_y=0,
                cell_cols=80,
                cell_rows=25,
                z_order=0,
                clipped=False,
                # At equal z, the renderer-neutral painter contract places
                # glyph runs behind semantic controls.
                draws=(_glyph(), _menu_bar()),
            ),
        ),
    )


def test_semantic_menu_tree_round_trips_with_explicit_draw_tags():
    plane = _plane()

    wire = retained_draw_plane_to_wire(plane)

    assert retained_draw_plane_from_wire(wire) == plane
    glyph, bar = wire["regions"][0]["draws"]
    assert glyph["kind"] == "glyph_run"
    assert bar["kind"] == "menu_bar"
    assert bar["menus"][0]["kind"] == "menu"
    assert [entry["kind"] for entry in bar["menus"][0]["entries"]] == [
        "menu_item",
        "menu_separator",
        "menu_item",
    ]
    assert bar["menus"][0]["entries"][0]["label"] == "Save…"
    assert bar["menus"][0]["entries"][0]["shortcut"] == "Ctrl+S"


@pytest.mark.parametrize(
    ("mutate", "error", "match"),
    (
        (
            lambda wire: wire["regions"][0]["draws"][1].update(
                {"kind": "canvas"}
            ),
            ValueError,
            "not a retained draw kind",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0][
                "entries"
            ][0].update({"pixel_width": 120}),
            ValueError,
            "fields are not exact",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1].update(
                {"state": True}
            ),
            TypeError,
            "not bool",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0].update(
                {"state": int(VISIBLE | ENABLED) | (1 << 15)}
            ),
            ValueError,
            "reserved CONTROL-1 bits",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0].update(
                {"label": "File\nMenu"}
            ),
            ValueError,
            "control character",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0][
                "entries"
            ][2].update({"control_id": 22}),
            ValueError,
            "control IDs are duplicated",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][1].update(
                {"state": int(VISIBLE | ENABLED | ControlState.OPEN)}
            ),
            ValueError,
            "multiple open menus",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0][
                "entries"
            ][2].update(
                {"state": int(VISIBLE | ENABLED | ControlState.SELECTED)}
            ),
            ValueError,
            "multiple selected items",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0].update(
                {"state": int(VISIBLE | ENABLED)}
            ),
            ValueError,
            "closed menu",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0][
                "entries"
            ].reverse(),
            ValueError,
            "semantic order",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1]["menus"][0][
                "entries"
            ][0].update({"kind": "menu"}),
            ValueError,
            "not a semantic menu entry",
        ),
    ),
)
def test_semantic_menu_wire_rejects_unknown_or_invalid_values(mutate, error, match):
    wire = deepcopy(retained_draw_plane_to_wire(_plane()))
    mutate(wire)

    with pytest.raises(error, match=match):
        retained_draw_plane_from_wire(wire)


def test_decoder_reasserts_cross_family_back_to_front_order():
    wire = deepcopy(retained_draw_plane_to_wire(_plane()))
    wire["regions"][0]["draws"].reverse()

    with pytest.raises(ValueError, match="back-to-front order"):
        retained_draw_plane_from_wire(wire)
