"""Focused shared-viewer wire checks for semantic retained controls."""

from __future__ import annotations

import base64
from copy import deepcopy

import pytest

from rich_terminal.retained_scene import ControlState, ObjectBounds, Point, RGBA, Sample
from rich_terminal.retained_view import (
    GlyphRunDraw,
    MenuBarDraw,
    MenuDraw,
    MenuItemDraw,
    MenuSeparatorDraw,
    MeterDraw,
    PlotDraw,
    PolylineDraw,
    ReadoutDraw,
    RetainedDrawPlane,
    RetainedRegionDraw,
    SeriesHistoryDraw,
    StatusDraw,
    TabDraw,
    TabSetDraw,
    TextAreaDraw,
    TextGridDraw,
    WaveformDraw,
)
from rich_terminal.semantic_content import (
    SemanticContentFlag,
    SemanticTextContent,
    SemanticTextItem,
    SemanticTextRole,
    SemanticTextState,
    encode_semantic_text_content,
)
from shared_session import retained_draw_plane_from_wire, retained_draw_plane_to_wire


VISIBLE = ControlState.VISIBLE
ENABLED = ControlState.ENABLED
FULL_BOUNDS = ObjectBounds(0, 0, 80, 25)


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


def _polyline(*, object_id: int = 12, z_order: int = 5) -> PolylineDraw:
    return PolylineDraw(
        object_id=object_id,
        z_order=z_order,
        bounds=ObjectBounds(5, 0, 70, 25),
        points=(Point(0, 0), Point(0x7FFFFFFF, 0xFFFFFFFF), Point(0xFFFFFFFF, 0)),
        stroke_width=0x08000000,
        color=RGBA(20, 210, 70, 192),
        closed=True,
        parent_bounds=(
            ObjectBounds(0, 2, 80, 21),
            ObjectBounds(10, 0, 70, 25),
        ),
    )


def _menu_bar(*, z_order: int = 4) -> MenuBarDraw:
    return MenuBarDraw(
        control_id=20,
        state=VISIBLE | ENABLED,
        order=0,
        z_order=z_order,
        bounds=ObjectBounds(0, 0, 80, 2),
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


def _text_area_content() -> SemanticTextContent:
    return SemanticTextContent(
        content_revision=3,
        rows=2,
        columns=8,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=2,
        viewport_columns=8,
        flags=SemanticContentFlag(0),
        primary_key=32,
        primary_offset=2,
        anchor_key=31,
        anchor_offset=1,
        items=(
            SemanticTextItem(
                31,
                0,
                0,
                1,
                8,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "Pad",
            ),
            SemanticTextItem(
                32,
                1,
                0,
                1,
                8,
                SemanticTextRole.CONTENT,
                SemanticTextState(0),
                "draft",
            ),
        ),
    )


def _text_grid_content() -> SemanticTextContent:
    return SemanticTextContent(
        content_revision=5,
        rows=3,
        columns=4,
        viewport_row=0,
        viewport_column=0,
        viewport_rows=3,
        viewport_columns=4,
        flags=SemanticContentFlag.READ_ONLY,
        primary_key=52,
        primary_offset=0,
        anchor_key=0,
        anchor_offset=0,
        items=(
            SemanticTextItem(
                51,
                0,
                0,
                1,
                2,
                SemanticTextRole.COLUMN_HEADER,
                SemanticTextState(0),
                "Mo",
            ),
            SemanticTextItem(
                52,
                1,
                2,
                1,
                1,
                SemanticTextRole.CONTENT,
                SemanticTextState.CURRENT,
                "8",
            ),
        ),
    )


def _collection_plane() -> RetainedDrawPlane:
    tabset = TabSetDraw(
        control_id=30,
        state=VISIBLE | ENABLED,
        order=0,
        z_order=1,
        bounds=ObjectBounds(0, 0, 80, 2),
        tabs=(
            TabDraw(
                31,
                VISIBLE | ENABLED | ControlState.SELECTED,
                0,
                "one.txt",
                "",
            ),
            TabDraw(32, VISIBLE | ENABLED, 1, "two.txt", "Alt+2"),
        ),
    )
    area = TextAreaDraw(
        40,
        VISIBLE | ENABLED,
        0,
        2,
        FULL_BOUNDS,
        _text_area_content(),
    )
    grid = TextGridDraw(
        50,
        VISIBLE | ENABLED | ControlState.SELECTED,
        0,
        3,
        FULL_BOUNDS,
        _text_grid_content(),
    )
    return RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1,
                2,
                3,
                0,
                0,
                80,
                25,
                0,
                0,
                0,
                0,
                0,
                False,
                (tabset, area, grid),
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
                logical_x=0,
                logical_y=0,
                logical_cols=80,
                logical_rows=25,
                clip_x=0,
                clip_y=0,
                clip_cols=0,
                clip_rows=0,
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


def test_object_draws_round_trip_with_exact_group_paths_and_vector_geometry():
    glyph = GlyphRunDraw(
        object_id=11,
        z_order=4,
        bounds=FULL_BOUNDS,
        foreground=RGBA(230, 235, 244, 255),
        background=RGBA(17, 20, 28, 255),
        attributes=0,
        text="Desk",
        parent_bounds=(ObjectBounds(0, 0, 70, 25),),
    )
    line = _polyline()
    plane = RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1, 2, 3, 0, 0, 80, 25, 0, 0, 0, 0, 0, False, (glyph, line)
            ),
        ),
    )

    wire = retained_draw_plane_to_wire(plane)

    assert retained_draw_plane_from_wire(wire) == plane
    glyph_wire, line_wire = wire["regions"][0]["draws"]
    assert glyph_wire["parent_bounds"] == [[0, 0, 70, 25]]
    assert set(line_wire) == {
        "kind",
        "object_id",
        "z_order",
        "bounds",
        "parent_bounds",
        "points",
        "stroke_width",
        "color",
        "closed",
    }
    assert line_wire["kind"] == "polyline"
    assert line_wire["points"] == [
        [0, 0],
        [0x7FFFFFFF, 0xFFFFFFFF],
        [0xFFFFFFFF, 0],
    ]
    assert line_wire["closed"] is True


@pytest.mark.parametrize(
    ("mutate", "error", "match"),
    (
        (
            lambda draw: draw["points"][0].__setitem__(0, True),
            TypeError,
            "not bool",
        ),
        (
            lambda draw: draw["parent_bounds"].append([0, 0, 1]),
            TypeError,
            "array of 4 integers",
        ),
        (
            lambda draw: draw.update({"closed": 1}),
            TypeError,
            "must be bool",
        ),
        (
            lambda draw: draw.update({"renderer_hint": "smooth"}),
            ValueError,
            "fields are not exact",
        ),
    ),
)
def test_polyline_wire_rejects_noncanonical_geometry(mutate, error, match):
    plane = RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1, 2, 3, 0, 0, 80, 25, 0, 0, 0, 0, 0, False, (_polyline(),)
            ),
        ),
    )
    wire = deepcopy(retained_draw_plane_to_wire(plane))
    mutate(wire["regions"][0]["draws"][0])

    with pytest.raises(error, match=match):
        retained_draw_plane_from_wire(wire)


def _instrument_plane() -> RetainedDrawPlane:
    parent_bounds = (ObjectBounds(0, 0, 75, 25),)
    draws = (
        ReadoutDraw(
            20,
            1,
            FULL_BOUNDS,
            RGBA(230, 235, 244, 192),
            RGBA(17, 20, 28, 255),
            "-12.5 dB",
            parent_bounds,
        ),
        MeterDraw(
            21,
            2,
            FULL_BOUNDS,
            RGBA(20, 210, 70, 255),
            RGBA(17, 20, 28, 255),
            True,
            True,
            -50,
            50,
            25,
            parent_bounds,
        ),
        StatusDraw(
            22,
            3,
            FULL_BOUNDS,
            RGBA(90, 90, 90, 255),
            RGBA(250, 190, 40, 160),
            -1,
            2,
            parent_bounds,
        ),
    )
    return RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1, 2, 3, 0, 0, 80, 25, 0, 0, 0, 0, 0, False, draws
            ),
        ),
    )


def test_instrument_draws_round_trip_as_distinct_typed_values():
    plane = _instrument_plane()

    wire = retained_draw_plane_to_wire(plane)

    assert retained_draw_plane_from_wire(wire) == plane
    readout, meter, status = wire["regions"][0]["draws"]
    assert readout["kind"] == "readout"
    assert readout["text"] == "-12.5 dB"
    assert meter["kind"] == "meter"
    assert (meter["minimum"], meter["maximum"], meter["value"]) == (-50, 50, 25)
    assert meter["vertical"] is True
    assert status["kind"] == "status"
    assert (status["value"], status["shape"]) == (-1, 2)


@pytest.mark.parametrize(
    ("draw_index", "field", "value", "error", "match"),
    (
        (0, "text", 12, TypeError, "must be str"),
        (0, "text", "", ValueError, "must be nonempty"),
        (1, "vertical", 1, TypeError, "must be bool"),
        (1, "value", 51, ValueError, "meter range/value is invalid"),
        (2, "shape", 3, ValueError, "between 0 and 2"),
        (2, "value", True, TypeError, "not bool"),
    ),
)
def test_instrument_wire_rejects_invalid_typed_values(
    draw_index, field, value, error, match
):
    wire = deepcopy(retained_draw_plane_to_wire(_instrument_plane()))
    wire["regions"][0]["draws"][draw_index][field] = value

    with pytest.raises(error, match=match):
        retained_draw_plane_from_wire(wire)


def _series_plane() -> RetainedDrawPlane:
    history = SeriesHistoryDraw(
        1,
        2,
        7,
        (Sample(10, -5), Sample(20, 15), Sample(40, 0)),
    )
    plot = PlotDraw(
        30,
        1,
        FULL_BOUNDS,
        7,
        -10,
        20,
        RGBA(20, 210, 70, 255),
        RGBA(20, 210, 70, 96),
        True,
        True,
    )
    waveform = WaveformDraw(
        31,
        2,
        FULL_BOUNDS,
        7,
        -20,
        20,
        RGBA(230, 235, 244, 192),
        RGBA(90, 90, 90, 255),
        0,
        True,
    )
    return RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1,
                2,
                3,
                0,
                0,
                80,
                25,
                0,
                0,
                0,
                0,
                0,
                False,
                (plot, waveform),
            ),
        ),
        (history,),
    )


def test_series_history_is_copied_once_and_draws_carry_only_its_identity():
    plane = _series_plane()

    wire = retained_draw_plane_to_wire(plane)

    assert retained_draw_plane_from_wire(wire) == plane
    assert wire["series"] == [
        {
            "owner_id": 1,
            "owner_generation": 2,
            "series_id": 7,
            "samples": [[10, -5], [20, 15], [40, 0]],
        }
    ]
    plot, waveform = wire["regions"][0]["draws"]
    assert plot["kind"] == "plot"
    assert waveform["kind"] == "waveform"
    assert plot["series_id"] == waveform["series_id"] == 7
    assert "samples" not in plot and "samples" not in waveform


@pytest.mark.parametrize(
    ("mutate", "error", "match"),
    (
        (
            lambda wire: wire["series"][0]["samples"][1].__setitem__(0, 10),
            ValueError,
            "not strictly increasing",
        ),
        (
            lambda wire: wire["series"][0]["samples"][0].__setitem__(1, True),
            TypeError,
            "not bool",
        ),
        (
            lambda wire: wire["series"][0].update({"history_capacity": 8}),
            ValueError,
            "fields are not exact",
        ),
        (
            lambda wire: wire.update({"series": []}),
            ValueError,
            "has no copied history",
        ),
        (
            lambda wire: wire["regions"][0]["draws"][1].update(
                {"zero_value": 21}
            ),
            ValueError,
            "outside its range",
        ),
    ),
)
def test_series_wire_rejects_invalid_history_or_consumer_values(
    mutate, error, match
):
    wire = deepcopy(retained_draw_plane_to_wire(_series_plane()))
    mutate(wire)

    with pytest.raises(error, match=match):
        retained_draw_plane_from_wire(wire)


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


def test_collection_draws_round_trip_with_exact_tags_and_canonical_stx1() -> None:
    plane = _collection_plane()

    wire = retained_draw_plane_to_wire(plane)

    assert retained_draw_plane_from_wire(wire) == plane
    tabset, area, grid = wire["regions"][0]["draws"]
    assert set(tabset) == {
        "kind",
        "control_id",
        "state",
        "order",
        "z_order",
        "bounds",
        "tabs",
    }
    assert tabset["kind"] == "tabset"
    assert [tab["kind"] for tab in tabset["tabs"]] == ["tab", "tab"]
    assert set(tabset["tabs"][0]) == {
        "kind",
        "control_id",
        "state",
        "order",
        "label",
        "shortcut",
    }
    for draw, tag, content in (
        (area, "text_area", plane.regions[0].draws[1].content),
        (grid, "text_grid", plane.regions[0].draws[2].content),
    ):
        assert set(draw) == {
            "kind",
            "control_id",
            "state",
            "order",
            "z_order",
            "bounds",
            "content_stx1_base64",
        }
        assert draw["kind"] == tag
        assert draw["content_stx1_base64"] == base64.b64encode(
            encode_semantic_text_content(content)
        ).decode("ascii")


def test_collection_decoder_rejects_noncanonical_or_invalid_stx1_text() -> None:
    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    area = wire["regions"][0]["draws"][1]
    area["content_stx1_base64"] += "="
    with pytest.raises(ValueError, match="canonical base64"):
        retained_draw_plane_from_wire(wire)

    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    area = wire["regions"][0]["draws"][1]
    area["content_stx1_base64"] = "not*base64"
    with pytest.raises(ValueError, match="canonical base64"):
        retained_draw_plane_from_wire(wire)

    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    area = wire["regions"][0]["draws"][1]
    area["content_stx1_base64"] = base64.b64encode(b"STX1").decode("ascii")
    with pytest.raises(ValueError, match="canonical STX1"):
        retained_draw_plane_from_wire(wire)


def test_collection_decoder_reasserts_exact_fields_family_and_tab_graph() -> None:
    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    wire["regions"][0]["draws"][1]["items"] = []
    with pytest.raises(ValueError, match="fields are not exact"):
        retained_draw_plane_from_wire(wire)

    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    wire["regions"][0]["draws"][2]["kind"] = "text_area"
    with pytest.raises(ValueError, match="TEXT_AREA"):
        retained_draw_plane_from_wire(wire)

    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    tabs = wire["regions"][0]["draws"][0]["tabs"]
    tabs[1]["control_id"] = tabs[0]["control_id"]
    with pytest.raises(ValueError, match="control IDs are duplicated"):
        retained_draw_plane_from_wire(wire)

    wire = deepcopy(retained_draw_plane_to_wire(_collection_plane()))
    wire["regions"][0]["draws"][0]["tabs"][0]["pixel_left"] = 2
    with pytest.raises(ValueError, match="fields are not exact"):
        retained_draw_plane_from_wire(wire)


def test_collection_encoder_rejects_a_mislabeled_content_family() -> None:
    area = TextAreaDraw(
        40,
        VISIBLE | ENABLED,
        0,
        2,
        FULL_BOUNDS,
        _text_grid_content(),
    )
    plane = RetainedDrawPlane(
        True,
        True,
        (
            RetainedRegionDraw(
                1, 2, 3, 0, 0, 80, 25, 0, 0, 0, 0, 0, False, (area,)
            ),
        ),
    )

    with pytest.raises(ValueError, match="TEXT_AREA"):
        retained_draw_plane_to_wire(plane)
